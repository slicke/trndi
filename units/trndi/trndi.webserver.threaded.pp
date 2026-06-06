(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)
unit trndi.webserver.threaded;

{$mode objfpc}{$H+}

{
  Minimal HTTP server for exposing current glucose readings and predictions.

  Threading model:
    - TWebServerThread owns the listening socket and runs the accept loop.
    - Each accepted connection is handled on its own TClientHandlerThread.
    - The thread-safe-callback contract below MUST hold; otherwise concurrent
      requests (or even one request racing with the UI) can corrupt state.

  Thread-safety contract for callbacks:
    TGetCurrentReadingFunc / TGetPredictionsFunc are invoked from
    TClientHandlerThread (i.e. NOT the main/UI thread, and NOT serialized
    with each other). Implementations must either:
      a) protect any shared state with a critical section, or
      b) marshal to the main thread via Synchronize/Queue.
    Returning a managed dynamic array of value-type records (BGResults) is
    the easiest safe contract: the array is copied to the caller, so the
    server thread never touches the producer's storage after the call
    returns.
}

interface

uses
Classes, SysUtils, Sockets, fpjson, jsonparser, trndi.types, DateUtils
{$IFNDEF Windows}, BaseUnix{$ELSE}, WinSock2{$IFEND};

type
  { Callback function types for thread-safe data access }
TGetCurrentReadingFunc = function: BGResults of object;
TGetPredictionsFunc = function: BGResults of object;

  { TClientHandlerThread - handles a single accepted connection }
TClientHandlerThread = class(TThread)
private
  FClientSocket: TSocket;
  FAuthToken: string;
  FGetCurrentReading: TGetCurrentReadingFunc;
  FGetPredictions: TGetPredictionsFunc;
  FStartedAtUtc: TDateTime;
  FPort: word;
  FActiveCounter: PLongInt;
  function ReadingToJSON(const Reading: BGReading; IncludeDelta: boolean = true): TJSONObject;
  function HandleRequest(const Request: string): string;
  function CheckAuth(const Headers: string): boolean;
  function ReadRequest(out Request: string; out TooLarge: boolean): boolean;
  procedure SendAll(const Data: string);
protected
  procedure Execute; override;
public
  constructor Create(AClientSocket: TSocket; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    const AStartedAtUtc: TDateTime; APort: word;
    AActiveCounter: PLongInt);
end;

  { TWebServerThread - listens and dispatches connections }
TWebServerThread = class(TThread)
private
  FPort: word;
  FAuthToken: string;
  FServerSocket: TSocket;
  FStartedAtUtc: TDateTime;
  FLoopbackOnly: boolean;
  FGetCurrentReading: TGetCurrentReadingFunc;
  FGetPredictions: TGetPredictionsFunc;
  FActiveCounter: PLongInt;
protected
  procedure Execute; override;
public
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    ALoopbackOnly: boolean;
    AActiveCounter: PLongInt);
  destructor Destroy; override;
  procedure CloseServerSocket;
end;

  { TTrndiWebServer }
TTrndiWebServer = class
private
  FThread: TWebServerThread;
  FPort: word;
  FEnabled: boolean;
  FActiveClients: LongInt;
public
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    ALoopbackOnly: boolean = false);
  destructor Destroy; override;
  procedure Start;
  procedure Stop;
  function Active: boolean;
  property Port: word read FPort;
  property Enabled: boolean read FEnabled;
end;

implementation

const
INVALID_SOCKET = TSocket(-1);
MAX_REQUEST_SIZE = 16 * 1024;          // hard cap on inbound request bytes
REQUEST_READ_TIMEOUT_MS = 5000;        // total budget to receive headers
LOOPBACK_ADDR_HOST_ORDER = $7F000001;  // 127.0.0.1
{$IFDEF WINDOWS}
SHUT_RDWR = SD_BOTH;
{$ELSE}
SHUT_RDWR = 2;
{$ENDIF}

{$IFDEF WINDOWS}
// Windows-specific socket wrapper functions
function SocketShutdown(s: TSocket; how: integer): integer;
begin
  Result := WinSock2.shutdown(s, how);
end;

function SocketSetOpt(s: TSocket; level, optname: integer; optval: pchar; optlen: integer): integer;
begin
  Result := WinSock2.setsockopt(s, level, optname, optval, optlen);
end;

function SocketSelect(nfds: integer; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): integer;
begin
  Result := WinSock2.select(nfds, readfds, writefds, exceptfds, timeout);
end;

procedure FD_ZERO_Helper(var fdset: TFDSet);
begin
  WinSock2.FD_ZERO(fdset);
end;

procedure FD_SET_Helper(s: TSocket; var fdset: TFDSet);
begin
  WinSock2.FD_SET(s, fdset);
end;

function HostToNetLong(v: longword): longword;
begin
  Result := WinSock2.htonl(v);
end;

{$ELSE}
// Unix-specific socket wrapper functions
function SocketShutdown(s: TSocket; how: integer): integer;
begin
  Result := fpShutdown(s, how);
end;

function SocketSetOpt(s: TSocket; level, optname: integer; optval: pchar; optlen: integer): integer;
begin
  Result := fpSetSockOpt(s, level, optname, optval, optlen);
end;

function SocketSelect(nfds: integer; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): integer;
begin
  Result := fpSelect(nfds, readfds, writefds, exceptfds, timeout);
end;

procedure FD_ZERO_Helper(var fdset: TFDSet);
begin
  fpFD_ZERO(fdset);
end;

procedure FD_SET_Helper(s: TSocket; var fdset: TFDSet);
begin
  fpFD_SET(s, fdset);
end;

function HostToNetLong(v: longword): longword;
begin
  Result := htonl(v);
end;
{$ENDIF}

// Length-leaking-resistant string compare. Always walks min(LenA, LenB)
// bytes and folds the length mismatch into the diff, so timing does not
// reveal where the inputs first differ.
function ConstantTimeEquals(const A, B: string): boolean;
var
  i, diff, lenA, lenB, lenMin: integer;
begin
  lenA := Length(A);
  lenB := Length(B);
  diff := lenA xor lenB;
  if lenA < lenB then
    lenMin := lenA
  else
    lenMin := lenB;
  for i := 1 to lenMin do
    diff := diff or (Ord(A[i]) xor Ord(B[i]));
  Result := diff = 0;
end;

function FormatUtcIso(const ALocalTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
    LocalTimeToUniversal(ALocalTime));
end;

{ TClientHandlerThread }

constructor TClientHandlerThread.Create(AClientSocket: TSocket; const AAuthToken: string;
AGetCurrentReading: TGetCurrentReadingFunc;
AGetPredictions: TGetPredictionsFunc;
const AStartedAtUtc: TDateTime; APort: word;
AActiveCounter: PLongInt);
begin
  inherited Create(true); // suspended; caller calls Start after setup is complete
  FreeOnTerminate := true;
  FClientSocket := AClientSocket;
  FAuthToken := AAuthToken;
  FGetCurrentReading := AGetCurrentReading;
  FGetPredictions := AGetPredictions;
  FStartedAtUtc := AStartedAtUtc;
  FPort := APort;
  FActiveCounter := AActiveCounter;
end;

function TClientHandlerThread.CheckAuth(const Headers: string): boolean;
const
  AUTH_NAME = 'authorization';
var
  Lines: TStringList;
  i, ColonPos, SpacePos: integer;
  Line, HeaderName, HeaderValue, Scheme, Token: string;
begin
  if FAuthToken = '' then
  begin
    Result := true;
    Exit;
  end;

  Result := false;
  Lines := TStringList.Create;
  try
    Lines.Text := Headers;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      ColonPos := Pos(':', Line);
      if ColonPos <= 0 then
        Continue;
      HeaderName := LowerCase(Trim(Copy(Line, 1, ColonPos - 1)));
      if HeaderName <> AUTH_NAME then
        Continue;

      HeaderValue := Trim(Copy(Line, ColonPos + 1, MaxInt));
      SpacePos := Pos(' ', HeaderValue);
      if SpacePos <= 0 then
        Exit;
      Scheme := LowerCase(Copy(HeaderValue, 1, SpacePos - 1));
      if Scheme <> 'bearer' then
        Exit;
      Token := Trim(Copy(HeaderValue, SpacePos + 1, MaxInt));
      Result := ConstantTimeEquals(Token, FAuthToken);
      Exit;
    end;
  finally
    Lines.Free;
  end;
end;

function TClientHandlerThread.ReadingToJSON(const Reading: BGReading; IncludeDelta: boolean): TJSONObject;
var
  fs: TFormatSettings;
begin
  fs.decimalSeparator := '.';
  Result := TJSONObject.Create;
  try
    Result.Add('mgdl', round(Reading.val));
    Result.Add('mmol', FormatFloat('0.0', Reading.convert(mmol, BGPrimary), fs));

    if IncludeDelta then
    begin
      Result.Add('mgdl_delta', round(Reading.delta));
      Result.Add('mmol_delta', FormatFloat('0.0', Reading.convert(mmol, BGDelta), fs));
    end;

    Result.Add('trend', integer(Reading.trend));
    // `timestamp` is kept as local-time "YYYY-MM-DD HH:MM:SS" for backwards
    // compatibility; new consumers should prefer `timestamp_utc` (ISO 8601).
    Result.Add('timestamp', DateTimeToStr(Reading.date));
    Result.Add('timestamp_utc', FormatUtcIso(Reading.date));
  except
    Result.Free;
    raise;
  end;
end;

function TClientHandlerThread.HandleRequest(const Request: string): string;
var
  Lines: TStringList;
  Method, URI, URIPath, Headers: string;
  ResponseObj: TJSONObject;
  CurrentReadings: BGResults;
  Predictions: BGResults;
  PredArray, Endpoints: TJSONArray;
  i: integer;
  QueryPos: integer;
  NowUtc: TDateTime;
  UptimeSeconds: integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Request;
    if Lines.Count = 0 then
    begin
      Result := 'HTTP/1.1 400 Bad Request'#13#10#13#10;
      Exit;
    end;

    // Parse first line: METHOD URI HTTP/1.x
    Method := Copy(Lines[0], 1, Pos(' ', Lines[0]) - 1);
    URI := Copy(Lines[0], Pos(' ', Lines[0]) + 1, 1000);
    URI := Copy(URI, 1, Pos(' ', URI) - 1);
    URIPath := URI;
    QueryPos := Pos('?', URIPath);
    if QueryPos > 0 then
      URIPath := Copy(URIPath, 1, QueryPos - 1);
    QueryPos := Pos('#', URIPath);
    if QueryPos > 0 then
      URIPath := Copy(URIPath, 1, QueryPos - 1);
    Headers := Lines.Text;

    // CORS preflight
    if Method = 'OPTIONS' then
    begin
      Result := 'HTTP/1.1 204 No Content'#13#10 +
        'Access-Control-Allow-Origin: *'#13#10 +
        'Access-Control-Allow-Methods: GET, POST, OPTIONS'#13#10 +
        'Access-Control-Allow-Headers: Content-Type, Authorization'#13#10#13#10;
      Exit;
    end;

    // Check auth
    if not CheckAuth(Headers) then
    begin
      Result := 'HTTP/1.1 401 Unauthorized'#13#10 +
        'Content-Type: application/json'#13#10 +
        'Access-Control-Allow-Origin: *'#13#10 +
        'Connection: close'#13#10#13#10 +
        '{"error":"Unauthorized"}';
      Exit;
    end;

    // Route requests
    ResponseObj := TJSONObject.Create;
    try
      if URIPath = '/glucose' then
      begin
        if Assigned(FGetCurrentReading) then
        begin
          CurrentReadings := FGetCurrentReading();
          if Length(CurrentReadings) > 0 then
          begin
            for i := Low(CurrentReadings) to High(CurrentReadings) do
              ResponseObj.Add(i.ToString, ReadingToJSON(CurrentReadings[i], true));
            Result := 'HTTP/1.1 200 OK'#13#10;
          end
          else
          begin
            ResponseObj.Add('error', 'No data available');
            Result := 'HTTP/1.1 503 Service Unavailable'#13#10;
          end;
        end
        else
        begin
          ResponseObj.Add('error', 'Service not configured');
          Result := 'HTTP/1.1 500 Internal Server Error'#13#10;
        end;
      end
      else
      if URIPath = '/predict' then
      begin
        PredArray := TJSONArray.Create;
        if Assigned(FGetPredictions) then
        begin
          Predictions := FGetPredictions();
          for i := 0 to High(Predictions) do
            PredArray.Add(ReadingToJSON(Predictions[i], true));
        end;
        ResponseObj.Add('predictions', PredArray);
        Result := 'HTTP/1.1 200 OK'#13#10;
      end
      else
      if URIPath = '/status' then
      begin
        ResponseObj.Add('status', 'ok');
        ResponseObj.Add('data_available', Assigned(FGetCurrentReading));
        Result := 'HTTP/1.1 200 OK'#13#10;
      end
      else
      if URIPath = '/health' then
      begin
        NowUtc := LocalTimeToUniversal(Now);
        UptimeSeconds := SecondsBetween(NowUtc, FStartedAtUtc);
        if UptimeSeconds < 0 then
          UptimeSeconds := 0;

        ResponseObj.Add('status', 'ok');
        ResponseObj.Add('service', 'trndi-webapi');
        ResponseObj.Add('timestamp_utc', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', NowUtc));
        ResponseObj.Add('uptime_seconds', UptimeSeconds);
        ResponseObj.Add('port', FPort);
        ResponseObj.Add('auth_required', FAuthToken <> '');
        ResponseObj.Add('data_available', Assigned(FGetCurrentReading));

        Endpoints := TJSONArray.Create;
        Endpoints.Add('/glucose');
        Endpoints.Add('/predict');
        Endpoints.Add('/status');
        Endpoints.Add('/health');
        ResponseObj.Add('endpoints', Endpoints);

        Result := 'HTTP/1.1 200 OK'#13#10;
      end
      else
      begin
        ResponseObj.Add('error', 'Not found');
        Result := 'HTTP/1.1 404 Not Found'#13#10;
      end;

      Result := Result +
        'Content-Type: application/json'#13#10 +
        'Access-Control-Allow-Origin: *'#13#10 +
        'Connection: close'#13#10#13#10 +
        ResponseObj.AsJSON;
    finally
      ResponseObj.Free;
    end;
  finally
    Lines.Free;
  end;
end;

// Reads bytes from the client up to MAX_REQUEST_SIZE or until CRLFCRLF
// (end of headers). Bounded by REQUEST_READ_TIMEOUT_MS total wall time
// across all recv calls so a slow/silent peer cannot tie up the handler.
function TClientHandlerThread.ReadRequest(out Request: string; out TooLarge: boolean): boolean;
var
  Buffer: array[0..2047] of byte;
  BytesRead: integer;
  ReqStream: TMemoryStream;
  StartScan, j: NativeInt;
  PBuf: PByte;
  Found: boolean;
  Deadline, NowMs: QWord;
  RemainMs: QWord;
  ReadFDs: TFDSet;
  TimeVal: TTimeVal;
  SelN: integer;
begin
  Result := false;
  TooLarge := false;
  Request := '';
  Deadline := GetTickCount64 + REQUEST_READ_TIMEOUT_MS;
  ReqStream := TMemoryStream.Create;
  try
    Found := false;
    while not Terminated do
    begin
      NowMs := GetTickCount64;
      if NowMs >= Deadline then
        Break;
      RemainMs := Deadline - NowMs;

      FD_ZERO_Helper(ReadFDs);
      FD_SET_Helper(FClientSocket, ReadFDs);
      TimeVal.tv_sec := RemainMs div 1000;
      TimeVal.tv_usec := (RemainMs mod 1000) * 1000;
      SelN := SocketSelect(FClientSocket + 1, @ReadFDs, nil, nil, @TimeVal);
      if SelN <= 0 then
        Break; // timeout or error
      if Terminated then
        Break;

      {$IFDEF WINDOWS}
      BytesRead := WinSock2.recv(FClientSocket, Buffer, SizeOf(Buffer), 0);
      {$ELSE}
      BytesRead := fpRecv(FClientSocket, @Buffer, SizeOf(Buffer), 0);
      {$ENDIF}
      if BytesRead <= 0 then
        Break; // peer closed or error

      ReqStream.Write(Buffer, BytesRead);

      if ReqStream.Size > MAX_REQUEST_SIZE then
      begin
        TooLarge := true;
        Exit;
      end;

      // Scan only the newly appended region (+3 bytes overlap) for CRLFCRLF
      StartScan := ReqStream.Size - BytesRead;
      if StartScan > 3 then
        Dec(StartScan, 3)
      else
        StartScan := 0;

      if ReqStream.Size >= 4 then
      begin
        PBuf := PByte(ReqStream.Memory);
        for j := StartScan to ReqStream.Size - 4 do
          if (PBuf[j] = 13) and (PBuf[j+1] = 10) and (PBuf[j+2] = 13) and (PBuf[j+3] = 10) then
          begin
            Found := true;
            Break;
          end;
      end;
      if Found then
        Break;
    end;

    if ReqStream.Size > 0 then
    begin
      SetLength(Request, ReqStream.Size);
      Move(ReqStream.Memory^, Request[1], ReqStream.Size);
      Result := Found; // only "good" if we actually saw end-of-headers
    end;
  finally
    ReqStream.Free;
  end;
end;

procedure TClientHandlerThread.SendAll(const Data: string);
var
  Total, Sent: SizeInt;
  N: integer;
  P: PChar;
begin
  if Data = '' then
    Exit;
  Total := Length(Data);
  Sent := 0;
  P := PChar(Data);
  while Sent < Total do
  begin
    if Terminated then
      Exit;
    {$IFDEF WINDOWS}
    N := WinSock2.send(FClientSocket, P[Sent], Total - Sent, 0);
    {$ELSE}
    N := fpSend(FClientSocket, @P[Sent], Total - Sent, 0);
    {$ENDIF}
    if N <= 0 then
      Exit; // peer closed or error; nothing useful to do
    Inc(Sent, N);
  end;
end;

procedure TClientHandlerThread.Execute;
var
  Request, Response: string;
  TooLarge: boolean;
begin
  try
    try
      if FClientSocket = INVALID_SOCKET then
        Exit;

      if not ReadRequest(Request, TooLarge) then
      begin
        if TooLarge then
          SendAll('HTTP/1.1 413 Payload Too Large'#13#10 +
                  'Content-Type: application/json'#13#10 +
                  'Connection: close'#13#10#13#10 +
                  '{"error":"Request too large"}');
        // Otherwise: timeout / peer closed / malformed -- just drop.
        Exit;
      end;

      Response := HandleRequest(Request);
      SendAll(Response);
    except
      // Never let an exception escape -- it would terminate the worker
      // without cleanup and could take the process down.
    end;
  finally
    if FClientSocket <> INVALID_SOCKET then
    begin
      CloseSocket(FClientSocket);
      FClientSocket := INVALID_SOCKET;
    end;
    if FActiveCounter <> nil then
      InterlockedDecrement(FActiveCounter^);
  end;
end;

{ TWebServerThread }

constructor TWebServerThread.Create(APort: word; const AAuthToken: string;
AGetCurrentReading: TGetCurrentReadingFunc;
AGetPredictions: TGetPredictionsFunc;
ALoopbackOnly: boolean;
AActiveCounter: PLongInt);
begin
  inherited Create(true); // Create suspended
  FreeOnTerminate := false; // Owner stops + frees thread (needed for safe shutdown)
  FPort := APort;
  FAuthToken := AAuthToken;
  FGetCurrentReading := AGetCurrentReading;
  FGetPredictions := AGetPredictions;
  FLoopbackOnly := ALoopbackOnly;
  FActiveCounter := AActiveCounter;
  FServerSocket := INVALID_SOCKET;
  FStartedAtUtc := LocalTimeToUniversal(Now);
end;

destructor TWebServerThread.Destroy;
begin
  if FServerSocket <> INVALID_SOCKET then
    CloseSocket(FServerSocket);
  inherited Destroy;
end;

procedure TWebServerThread.CloseServerSocket;
begin
  if FServerSocket <> INVALID_SOCKET then
  begin
    // Shutdown the socket to interrupt any blocking accept/recv calls
    SocketShutdown(FServerSocket, SHUT_RDWR);
    CloseSocket(FServerSocket);
    FServerSocket := INVALID_SOCKET;
  end;
end;

procedure TWebServerThread.Execute;
var
  ClientSocket: TSocket;
  Client: TClientHandlerThread;
  {$IFDEF WINDOWS}
  SockAddr: WinSock2.TSockAddr;
  {$ELSE}
  SockAddr: TInetSockAddr;
  {$ENDIF}
  SockLen: TSockLen;
  OptVal: integer;
  ReadFDs: TFDSet;
  TimeVal: TTimeVal;
  SelectResult: integer;
  BindAddr: longword;
  {$IFDEF WINDOWS}
  WSAData: TWSAData;
  InetAddr: TInetSockAddr;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Initialize Winsock on Windows
  if WSAStartup($0202, WSAData) <> 0 then  // Version 2.2
    Exit;
  {$ENDIF}

  try
    // Create socket
    {$IFDEF WINDOWS}
    FServerSocket := WinSock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    {$ELSE}
    FServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
    {$ENDIF}
    if FServerSocket = INVALID_SOCKET then
      Exit;

    // Set socket options
    OptVal := 1;
    SocketSetOpt(FServerSocket, SOL_SOCKET, SO_REUSEADDR, pchar(@OptVal), SizeOf(OptVal));

    if FLoopbackOnly then
      BindAddr := HostToNetLong(LOOPBACK_ADDR_HOST_ORDER)
    else
      BindAddr := INADDR_ANY;

    // Bind to port
    {$IFDEF WINDOWS}
    FillChar(InetAddr, SizeOf(InetAddr), 0);
    InetAddr.sin_family := AF_INET;
    InetAddr.sin_port := WinSock2.htons(FPort);
    InetAddr.sin_addr.s_addr := BindAddr;
    Move(InetAddr, SockAddr, SizeOf(InetAddr));
    if WinSock2.bind(FServerSocket, SockAddr, SizeOf(SockAddr)) <> 0 then
    begin
      CloseSocket(FServerSocket);
      FServerSocket := INVALID_SOCKET;
      Exit;
    end;
    {$ELSE}
    FillChar(SockAddr, SizeOf(SockAddr), 0);
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_port := htons(FPort);
    SockAddr.sin_addr.s_addr := BindAddr;
    if fpBind(FServerSocket, @SockAddr, SizeOf(SockAddr)) <> 0 then
    begin
      CloseSocket(FServerSocket);
      FServerSocket := INVALID_SOCKET;
      Exit;
    end;
    {$ENDIF}

    // Listen
    {$IFDEF WINDOWS}
    if WinSock2.listen(FServerSocket, 16) <> 0 then
      {$ELSE}
      if fpListen(FServerSocket, 16) <> 0 then
        {$ENDIF}
      begin
        CloseSocket(FServerSocket);
        FServerSocket := INVALID_SOCKET;
        Exit;
      end;

    // Accept loop with select timeout
    while not Terminated do
    begin
      if FServerSocket = INVALID_SOCKET then
        Break;

      FD_ZERO_Helper(ReadFDs);
      FD_SET_Helper(FServerSocket, ReadFDs);
      TimeVal.tv_sec := 0;
      TimeVal.tv_usec := 500000;  // 500ms timeout

      SelectResult := SocketSelect(FServerSocket + 1, @ReadFDs, nil, nil, @TimeVal);

      if Terminated then
        Break;
      if SelectResult <= 0 then
        Continue;

      SockLen := SizeOf(SockAddr);
      {$IFDEF WINDOWS}
      ClientSocket := WinSock2.accept(FServerSocket, @SockAddr, @SockLen);
      {$ELSE}
      ClientSocket := fpAccept(FServerSocket, @SockAddr, @SockLen);
      {$ENDIF}

      if Terminated then
      begin
        if ClientSocket <> INVALID_SOCKET then
          CloseSocket(ClientSocket);
        Break;
      end;

      if ClientSocket = INVALID_SOCKET then
        Continue;

      // Hand off to a per-connection worker so concurrent clients do not
      // block each other. Increment BEFORE Start so the owner's Stop()
      // sees the in-flight worker even if scheduling delays it.
      if FActiveCounter <> nil then
        InterlockedIncrement(FActiveCounter^);
      try
        Client := TClientHandlerThread.Create(ClientSocket, FAuthToken,
          FGetCurrentReading, FGetPredictions,
          FStartedAtUtc, FPort, FActiveCounter);
        // Worker owns ClientSocket from here on.
        Client.Start;
      except
        // Could not spawn worker -- undo bookkeeping and clean up the socket.
        CloseSocket(ClientSocket);
        if FActiveCounter <> nil then
          InterlockedDecrement(FActiveCounter^);
      end;
    end;
  except
    // Silently handle exceptions during shutdown
  end;

  if FServerSocket <> INVALID_SOCKET then
  begin
    CloseSocket(FServerSocket);
    FServerSocket := INVALID_SOCKET;
  end;

  {$IFDEF WINDOWS}
  WSACleanup;
  {$ENDIF}
end;

{ TTrndiWebServer }

constructor TTrndiWebServer.Create(APort: word; const AAuthToken: string;
AGetCurrentReading: TGetCurrentReadingFunc;
AGetPredictions: TGetPredictionsFunc;
ALoopbackOnly: boolean);
begin
  inherited Create;
  FPort := APort;
  FEnabled := false;
  FActiveClients := 0;
  FThread := TWebServerThread.Create(APort, AAuthToken,
    AGetCurrentReading, AGetPredictions,
    ALoopbackOnly, @FActiveClients);
end;

destructor TTrndiWebServer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TTrndiWebServer.Start;
begin
  if not FEnabled and Assigned(FThread) then
  begin
    FThread.Start;
    FEnabled := true;
  end;
end;

procedure TTrndiWebServer.Stop;
const
  CLIENT_DRAIN_POLL_MS = 50;
begin
  if FEnabled and Assigned(FThread) then
  begin
    // 1. Close the listen socket so accept() unblocks immediately.
    FThread.CloseServerSocket;

    // 2. Tell the accept loop to stop and wait for it. After WaitFor the
    //    listener will no longer spawn new client workers.
    FThread.Terminate;
    FThread.WaitFor;

    // 3. Wait for any in-flight client workers to finish before we let the
    //    parent free us -- the workers hold method pointers into the owner
    //    and a pointer to FActiveClients. Each worker is bounded by
    //    REQUEST_READ_TIMEOUT_MS plus handler time, so this is finite.
    while InterlockedExchangeAdd(FActiveClients, 0) > 0 do
      Sleep(CLIENT_DRAIN_POLL_MS);

    FreeAndNil(FThread);
    FEnabled := false;
  end;
end;

function TTrndiWebServer.Active: boolean;
begin
  Result := FEnabled and Assigned(FThread) and not FThread.Finished;
end;

end.
