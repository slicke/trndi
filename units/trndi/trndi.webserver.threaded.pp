unit trndi.webserver.threaded;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, StrUtils, Sockets, fpjson, jsonparser, trndi.funcs, trndi.types
{$IFNDEF Windows}, BaseUnix{$ELSE}, WinSock2{$IFEND};

type
  { Callback function types for thread-safe data access }
TGetCurrentReadingFunc = function: BGResults of object;
TGetPredictionsFunc = function: BGResults of object;

  { TWebServerThread }
TWebServerThread = class(TThread)
private
  FPort: word;
  FAuthToken: string;
  FServerSocket: TSocket;
  FGetCurrentReading: TGetCurrentReadingFunc;
  FGetPredictions: TGetPredictionsFunc;
  function ReadingToJSON(const Reading: BGReading; IncludeDelta: boolean = true): TJSONObject;
  function HandleRequest(const Request: string): string;
  function CheckAuth(const Headers: string): boolean;
protected
  procedure Execute; override;
public
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc);
  destructor Destroy; override;
  procedure CloseServerSocket;
end;

  { TTrndiWebServer }
TTrndiWebServer = class
private
  FThread: TWebServerThread;
  FPort: word;
  FEnabled: boolean;
public
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc);
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
{$ENDIF}

{ TWebServerThread }

constructor TWebServerThread.Create(APort: word; const AAuthToken: string;
AGetCurrentReading: TGetCurrentReadingFunc;
AGetPredictions: TGetPredictionsFunc);
begin
  inherited Create(true); // Create suspended
  FreeOnTerminate := false; // Owner stops + frees thread (needed for safe shutdown)
  FPort := APort;
  FAuthToken := AAuthToken;
  FGetCurrentReading := AGetCurrentReading;
  FGetPredictions := AGetPredictions;
  FServerSocket := INVALID_SOCKET;
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

function TWebServerThread.CheckAuth(const Headers: string): boolean;
var
  Lines: TStringList;
  i: integer;
  Line: string;
begin
  Result := true; // Allow if no token configured
  if FAuthToken = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := Headers;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      if (Length(Line) >= Length('Authorization:')) and
         SameText(Copy(Line, 1, Length('Authorization:')), 'Authorization:') then
      begin
        Result := Pos('Bearer ' + FAuthToken, Line) > 0;
        Exit;
      end;
    end;
  finally
    Lines.Free;
  end;
  Result := false;
end;

function TWebServerThread.ReadingToJSON(const Reading: BGReading; IncludeDelta: boolean): TJSONObject;
var
  fs: TFormatSettings;
begin
  fs.decimalSeparator := '.';
  Result := TJSONObject.Create;
  try
    // Add both mg/dL and mmol/L values
    Result.Add('mgdl', round(Reading.val));
    Result.Add('mmol', FormatFloat('0.0', Reading.convert(mmol, BGPrimary), fs));
    
    if IncludeDelta then
    begin
      Result.Add('mgdl_delta', round(Reading.delta));
      Result.Add('mmol_delta', FormatFloat('0.0', Reading.convert(mmol, BGDelta), fs));
    end;
    
    Result.Add('trend', integer(Reading.trend));
    Result.Add('timestamp', DateTimeToStr(Reading.date));
  except
    Result.Free;
    raise;
  end;
end;

function TWebServerThread.HandleRequest(const Request: string): string;
var
  Lines: TStringList;
  Method, URI, Headers: string;
  ResponseObj: TJSONObject;
  CurrentReadings: BGResults;
  Predictions: BGResults;
  PredArray: TJSONArray;
  i: integer;
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
        'Access-Control-Allow-Origin: *'#13#10#13#10 +
        '{"error":"Unauthorized"}';
      Exit;
    end;

    // Route requests
    ResponseObj := TJSONObject.Create;
    try
      if URI = '/glucose' then
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
      if URI = '/predict' then
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
      if URI = '/status' then
      begin
        ResponseObj.Add('status', 'ok');
        ResponseObj.Add('data_available', Assigned(FGetCurrentReading));
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

procedure TWebServerThread.Execute;
var
  ClientSocket: TSocket;
  Request, Response: string;
  Buffer: array[0..4095] of char;
  BytesRead: integer;
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
  ReqStream: TMemoryStream;
  StartScan: NativeInt;
  j: NativeInt;
  Found: boolean;
  PBuf: PByte;
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

    // Bind to port
    {$IFDEF WINDOWS}
    FillChar(InetAddr, SizeOf(InetAddr), 0);
    InetAddr.sin_family := AF_INET;
    InetAddr.sin_port := WinSock2.htons(FPort);
    InetAddr.sin_addr.s_addr := INADDR_ANY;
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
    SockAddr.sin_addr.s_addr := INADDR_ANY;
    if fpBind(FServerSocket, @SockAddr, SizeOf(SockAddr)) <> 0 then
    begin
      CloseSocket(FServerSocket);
      FServerSocket := INVALID_SOCKET;
      Exit;
    end;
    {$ENDIF}

    // Listen
    {$IFDEF WINDOWS}
    if WinSock2.listen(FServerSocket, 5) <> 0 then
      {$ELSE}
      if fpListen(FServerSocket, 5) <> 0 then
        {$ENDIF}
      begin
        CloseSocket(FServerSocket);
        FServerSocket := INVALID_SOCKET;
        Exit;
      end;

    // Accept loop with select timeout
    while not Terminated do
    begin
      // Check if socket was closed (during shutdown)
      if FServerSocket = INVALID_SOCKET then
        Break;
      
      // Use select with timeout to check for incoming connections
      FD_ZERO_Helper(ReadFDs);
      FD_SET_Helper(FServerSocket, ReadFDs);
      TimeVal.tv_sec := 0;
      TimeVal.tv_usec := 500000;  // 500ms timeout
      
      SelectResult := SocketSelect(FServerSocket + 1, @ReadFDs, nil, nil, @TimeVal);
      
      // Check terminated after select
      if Terminated then
        Break;
      
      // If select returned error or timeout, continue loop
      if SelectResult <= 0 then
        Continue;
        
      // Socket is ready for accept
      SockLen := SizeOf(SockAddr);
      {$IFDEF WINDOWS}
      ClientSocket := WinSock2.accept(FServerSocket, @SockAddr, @SockLen);
      {$ELSE}
      ClientSocket := fpAccept(FServerSocket, @SockAddr, @SockLen);
      {$ENDIF}
      
      // Check terminated again after accept
      if Terminated then
      begin
        if ClientSocket <> INVALID_SOCKET then
          CloseSocket(ClientSocket);
        Break;
      end;
      
      if ClientSocket <> INVALID_SOCKET then
      try
        // Read request efficiently into a memory stream to avoid repeated string reallocations
        ReqStream := TMemoryStream.Create;
        try
          Found := false;
          repeat
            FillChar(Buffer, SizeOf(Buffer), 0);
            {$IFDEF WINDOWS}
            BytesRead := WinSock2.recv(ClientSocket, Buffer, SizeOf(Buffer), 0);
            {$ELSE}
            BytesRead := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer), 0);
            {$ENDIF}
            if BytesRead > 0 then
            begin
              ReqStream.Write(Buffer, BytesRead);

              // Scan only the newly appended region (+3 bytes overlap) for CRLFCRLF to detect end of headers
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
            end;
          until (BytesRead <= 0) or Found;

          if ReqStream.Size > 0 then
          begin
            SetLength(Request, ReqStream.Size);
            if ReqStream.Size > 0 then
              Move(ReqStream.Memory^, Request[1], ReqStream.Size);

            // Handle request and send response
            Response := HandleRequest(Request);
            {$IFDEF WINDOWS}
            WinSock2.send(ClientSocket, Response[1], Length(Response), 0);
            {$ELSE}
            fpSend(ClientSocket, @Response[1], Length(Response), 0);
            {$ENDIF}
          end;
        finally
          ReqStream.Free;
        end;
      finally
        CloseSocket(ClientSocket);
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
AGetPredictions: TGetPredictionsFunc);
begin
  inherited Create;
  FPort := APort;
  FEnabled := false;
  FThread := TWebServerThread.Create(APort, AAuthToken, AGetCurrentReading, AGetPredictions);
end;

destructor TTrndiWebServer.Destroy;
begin
  Stop;
  // Thread will free itself (FreeOnTerminate = True)
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
begin
  if FEnabled and Assigned(FThread) then
  begin
    // Close the server socket first to unblock fpAccept
    FThread.CloseServerSocket;

    // Signal termination and wait for completion so callbacks can't hit freed objects
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
    FEnabled := false;
  end;
end;

function TTrndiWebServer.Active: boolean;
begin
  Result := FEnabled and Assigned(FThread) and not FThread.Finished;
end;

end.
