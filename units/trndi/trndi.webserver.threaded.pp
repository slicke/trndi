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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-09-15: Capped concurrent /events streams at MAX_EVENT_STREAMS with
 *   a dedicated counter; a subscriber past the cap is answered 503.
 *)
unit trndi.webserver.threaded;

{$mode objfpc}{$H+}

{
  Minimal HTTP server for exposing current glucose readings and predictions,
  plus a server-sent-events stream (/events) that pushes state changes to
  subscribers instead of making them poll.

  Threading model:
    - TWebServerThread owns the listening socket and runs the accept loop.
    - Each accepted connection is handled on its own TClientHandlerThread.
      A /events subscriber keeps its handler alive until the peer closes or
      the server shuts down; the handler polls TWebEventHub for new events.
      At most MAX_EVENT_STREAMS such handlers exist at once; a subscriber
      past that gets 503, since each one is a thread held for as long as
      the peer likes.
    - TWebEventHub is the fan-out point: the owner publishes from any thread
      (the UI thread in practice), handlers copy out under the hub's lock.
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
Classes, SysUtils, Sockets, fpjson, jsonparser, syncobjs, trndi.types, DateUtils
{$IFNDEF Windows}, BaseUnix{$ELSE}, WinSock2{$IFEND};

const
  {** Upper bound on concurrent /events subscribers. Every stream holds a
      handler thread until the peer hangs up, so without a cap a misbehaving
      client could grow the process by one thread per reconnect. A request
      past the cap is answered 503 with Retry-After and closed; plain
      request/response endpoints are not counted against it. }
  MAX_EVENT_STREAMS = 16;

type
  { Callback function types for thread-safe data access }
TGetCurrentReadingFunc = function: BGResults of object;
TGetPredictionsFunc = function: BGResults of object;

  {** One event on the /events stream. }
TWebEvent = record
  Seq: int64;    //< Monotonic id; sent as the SSE "id:" line
  Name: string;  //< SSE event name (reading, predict, alert, status, snooze)
  Data: string;  //< Single-line JSON payload
end;
TWebEventArray = array of TWebEvent;

  {** Thread-safe fan-out point for the /events stream.

      The owner publishes from any thread; every handler serving /events polls
      the hub for events newer than the last one it sent. Sticky events keep
      their latest payload per name so a new subscriber can be handed the
      current state as a snapshot, and a recent-events ring lets a client that
      reconnects with Last-Event-ID replay what it missed. }
TWebEventHub = class
private
  FLock: TCriticalSection;
  FRing: TWebEventArray;   // recent events, oldest at FRingStart
  FRingStart: integer;
  FRingCount: integer;
  FSeq: int64;
  FSticky: TStringList;    // Name=Data, the latest payload of each sticky event
  FShutdown: boolean;
public
  constructor Create;
  destructor Destroy; override;
  {** Queue an event. A sticky event replaces its predecessor of the same
      name in the snapshot handed to new subscribers; with AOnlyIfChanged a
      sticky event whose data equals that predecessor is dropped, so callers
      can publish unconditionally and let the hub suppress the noise.
      @returns(True when the event was queued.) }
  function Publish(const AName, AData: string; ASticky: boolean = true;
    AOnlyIfChanged: boolean = true): boolean;
  {** Id of the most recently queued event (0 before the first). }
  function LatestSeq: int64;
  {** Copy the events queued after AfterSeq, oldest first.
      @returns(False when AfterSeq has already fallen out of the ring or is
      an id this hub has not issued (a client resuming across a server
      restart), in which case the caller should resynchronise from a
      snapshot.) }
  function CopySince(const AfterSeq: int64; out Items: TWebEventArray): boolean;
  {** Copy the latest payload of every sticky event, each stamped with the
      current LatestSeq so a client resuming from that id sees only what
      comes after the snapshot. }
  procedure CopySticky(out Items: TWebEventArray; out Seq: int64);
  {** Tell every subscriber's handler to end its stream. }
  procedure Shutdown;
  function IsShutdown: boolean;
end;

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
  FStreamCounter: PLongInt;   // open /events streams, capped at MAX_EVENT_STREAMS
  FHub: TWebEventHub;
  function HandleRequest(const Request: string): string;
  function CheckAuth(const Headers, QueryToken: string): boolean;
  function ReadRequest(out Request: string; out TooLarge: boolean): boolean;
  function SendAll(const Data: string): boolean;
  function SendEvent(const Event: TWebEvent): boolean;
  procedure ServeEventStream(const Headers: string);
  function ReserveStreamSlot: boolean;
  procedure ReleaseStreamSlot;
protected
  procedure Execute; override;
public
  constructor Create(AClientSocket: TSocket; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    const AStartedAtUtc: TDateTime; APort: word;
    AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub);
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
  FStreamCounter: PLongInt;
  FHub: TWebEventHub;
protected
  procedure Execute; override;
public
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    ALoopbackOnly: boolean;
    AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub);
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
  FActiveStreams: LongInt;   // subset of FActiveClients that serve /events
  FHub: TWebEventHub;
public
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    ALoopbackOnly: boolean = false);
  destructor Destroy; override;
  procedure Start;
  procedure Stop;
  function Active: boolean;

  {** Push a raw event to every /events subscriber. See TWebEventHub.Publish
      for the sticky and only-if-changed semantics. Safe from any thread. }
  function Publish(const AName, AData: string; ASticky: boolean = true;
    AOnlyIfChanged: boolean = true): boolean;
  {** Push the newest reading as a "reading" event (sticky, deduplicated on
      the payload, so re-applying the same reading publishes nothing). }
  procedure PublishReading(const Reading: BGReading);
  {** Push the current forecast as a "predict" event. An empty array clears
      the forecast for subscribers. }
  procedure PublishPredictions(const Preds: BGResults);
  {** Push an "alert" event. Alerts are not sticky: they mark a moment, not a
      state, and repeat whenever the alert engine re-fires. }
  procedure PublishAlert(const Kinds: array of string; const Reading: BGReading);
  {** Push the data/connection state as a "status" event. }
  procedure PublishStatus(const Fresh, Connected: boolean; const Detail: string);
  {** Push the alert-snooze state as a "snooze" event. AUntilLocal is ignored
      when AActive is false. }
  procedure PublishSnooze(const AActive: boolean; const AUntilLocal: TDateTime);

  property Port: word read FPort;
  property Enabled: boolean read FEnabled;
  property Hub: TWebEventHub read FHub;
end;

{** Serialise one reading the way every endpoint and event does it. }
function ReadingToJSON(const Reading: BGReading; IncludeDelta: boolean = true): TJSONObject;

{** Name of a reading's classification as exposed on the wire. }
function LevelName(const Level: BGValLevel): string;

implementation

const
INVALID_SOCKET = TSocket(-1);
MAX_REQUEST_SIZE = 16 * 1024;          // hard cap on inbound request bytes
REQUEST_READ_TIMEOUT_MS = 5000;        // total budget to receive headers
EVENT_RING_SIZE = 128;                 // events kept for Last-Event-ID replay
EVENT_POLL_MS = 250;                   // how often a stream handler looks for news
EVENT_KEEPALIVE_MS = 15000;            // idle comment so proxies/NATs keep the stream
LOOPBACK_ADDR_HOST_ORDER = $7F000001;  // 127.0.0.1
{$IFDEF WINDOWS}
SHUT_RDWR = SD_BOTH;
{$ELSE}
SHUT_RDWR = 2;
{$ENDIF}
// A peer that hangs up mid-send must not raise SIGPIPE and take the process
// down; an /events subscriber leaving is the normal case, not an error.
// Every Unix target but macOS has the per-call flag; macOS gets the socket
// option instead (set in TClientHandlerThread.Execute).
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
SEND_FLAGS = 0;
{$ELSE}
SEND_FLAGS = MSG_NOSIGNAL;
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

function LevelName(const Level: BGValLevel): string;
begin
  case Level of
  BGHigh: Result := 'high';
  BGLOW: Result := 'low';
  BGRangeHI: Result := 'range_high';
  BGRangeLO: Result := 'range_low';
  else
    Result := 'normal';
  end;
end;

function ReadingToJSON(const Reading: BGReading; IncludeDelta: boolean): TJSONObject;
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
    Result.Add('level', LevelName(Reading.level));
    // `timestamp` is kept as local-time "YYYY-MM-DD HH:MM:SS" for backwards
    // compatibility; new consumers should prefer `timestamp_utc` (ISO 8601).
    Result.Add('timestamp', DateTimeToStr(Reading.date));
    Result.Add('timestamp_utc', FormatUtcIso(Reading.date));
  except
    Result.Free;
    raise;
  end;
end;

// Value of the first header called AName (case-insensitive), '' when absent.
function HeaderValue(const Headers, AName: string): string;
var
  Lines: TStringList;
  i, ColonPos: integer;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.Text := Headers;
    for i := 0 to Lines.Count - 1 do
    begin
      ColonPos := Pos(':', Lines[i]);
      if ColonPos <= 0 then
        Continue;
      if LowerCase(Trim(Copy(Lines[i], 1, ColonPos - 1))) = LowerCase(AName) then
        Exit(Trim(Copy(Lines[i], ColonPos + 1, MaxInt)));
    end;
  finally
    Lines.Free;
  end;
end;

// Minimal percent-decoding for query values (also maps '+' to space).
function UrlDecode(const S: string): string;
var
  i, Code: integer;
begin
  Result := '';
  i := 1;
  while i <= Length(S) do
  begin
    if (S[i] = '%') and (i + 2 <= Length(S)) and
       TryStrToInt('$' + Copy(S, i + 1, 2), Code) then
    begin
      Result := Result + Chr(Code);
      Inc(i, 3);
    end
    else
    begin
      if S[i] = '+' then
        Result := Result + ' '
      else
        Result := Result + S[i];
      Inc(i);
    end;
  end;
end;

// Value of AName in a query string ("a=1&b=2"), '' when absent.
function QueryValue(const Query, AName: string): string;
var
  Parts: TStringArray;
  Part: string;
  EqPos: integer;
begin
  Result := '';
  Parts := Query.Split(['&']);
  for Part in Parts do
  begin
    EqPos := Pos('=', Part);
    if (EqPos > 0) and (Copy(Part, 1, EqPos - 1) = AName) then
      Exit(UrlDecode(Copy(Part, EqPos + 1, MaxInt)));
  end;
end;

// Splits "METHOD /path?query HTTP/1.x" (the first line of ARequest).
procedure ParseRequestLine(const ARequest: string; out Method, URIPath, Query: string);
var
  Line, URI: string;
  P: integer;
begin
  Method := '';
  URIPath := '';
  Query := '';
  P := Pos(#10, ARequest);
  if P > 0 then
    Line := Copy(ARequest, 1, P - 1)
  else
    Line := ARequest;
  Line := TrimRight(Line);

  P := Pos(' ', Line);
  if P <= 0 then
    Exit;
  Method := Copy(Line, 1, P - 1);
  URI := Copy(Line, P + 1, MaxInt);
  P := Pos(' ', URI);
  if P > 0 then
    URI := Copy(URI, 1, P - 1);

  P := Pos('#', URI);
  if P > 0 then
    URI := Copy(URI, 1, P - 1);
  P := Pos('?', URI);
  if P > 0 then
  begin
    URIPath := Copy(URI, 1, P - 1);
    Query := Copy(URI, P + 1, MaxInt);
  end
  else
    URIPath := URI;
end;

{ TWebEventHub }

constructor TWebEventHub.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FSticky := TStringList.Create;
  SetLength(FRing, EVENT_RING_SIZE);
  FRingStart := 0;
  FRingCount := 0;
  FSeq := 0;
  FShutdown := false;
end;

destructor TWebEventHub.Destroy;
begin
  FSticky.Free;
  FLock.Free;
  inherited Destroy;
end;

function TWebEventHub.Publish(const AName, AData: string; ASticky: boolean;
AOnlyIfChanged: boolean): boolean;
var
  Idx, Slot: integer;
begin
  Result := false;
  FLock.Acquire;
  try
    if ASticky then
    begin
      Idx := FSticky.IndexOfName(AName);
      if AOnlyIfChanged and (Idx >= 0) and (FSticky.ValueFromIndex[Idx] = AData) then
        Exit;
      if Idx >= 0 then
        FSticky.ValueFromIndex[Idx] := AData
      else
        FSticky.Add(AName + FSticky.NameValueSeparator + AData);
    end;

    Inc(FSeq);
    if FRingCount < Length(FRing) then
    begin
      Slot := (FRingStart + FRingCount) mod Length(FRing);
      Inc(FRingCount);
    end
    else
    begin
      // Full: overwrite the oldest and advance the start
      Slot := FRingStart;
      FRingStart := (FRingStart + 1) mod Length(FRing);
    end;
    FRing[Slot].Seq := FSeq;
    FRing[Slot].Name := AName;
    FRing[Slot].Data := AData;
    Result := true;
  finally
    FLock.Release;
  end;
end;

function TWebEventHub.LatestSeq: int64;
begin
  FLock.Acquire;
  try
    Result := FSeq;
  finally
    FLock.Release;
  end;
end;

function TWebEventHub.CopySince(const AfterSeq: int64; out Items: TWebEventArray): boolean;
var
  Oldest: int64;
  i, n, Slot: integer;
begin
  Items := nil;
  FLock.Acquire;
  try
    if AfterSeq = FSeq then
      Exit(true); // nothing new
    // An id this hub never issued: a Last-Event-ID carried over from before
    // a server restart. Treating it as "nothing new" left the subscriber
    // silent until the sequence had climbed past it; resynchronise instead.
    if AfterSeq > FSeq then
      Exit(false);

    if FRingCount = 0 then
      Exit(false);
    Oldest := FSeq - FRingCount + 1;
    if AfterSeq < Oldest - 1 then
      Exit(false); // the client missed events that are gone from the ring

    n := FSeq - AfterSeq;
    SetLength(Items, n);
    for i := 0 to n - 1 do
    begin
      Slot := (FRingStart + (FRingCount - n) + i) mod Length(FRing);
      Items[i] := FRing[Slot];
    end;
    Result := true;
  finally
    FLock.Release;
  end;
end;

procedure TWebEventHub.CopySticky(out Items: TWebEventArray; out Seq: int64);
var
  i: integer;
begin
  Items := nil;
  FLock.Acquire;
  try
    Seq := FSeq;
    SetLength(Items, FSticky.Count);
    for i := 0 to FSticky.Count - 1 do
    begin
      Items[i].Seq := FSeq;
      Items[i].Name := FSticky.Names[i];
      Items[i].Data := FSticky.ValueFromIndex[i];
    end;
  finally
    FLock.Release;
  end;
end;

procedure TWebEventHub.Shutdown;
begin
  FLock.Acquire;
  try
    FShutdown := true;
  finally
    FLock.Release;
  end;
end;

function TWebEventHub.IsShutdown: boolean;
begin
  FLock.Acquire;
  try
    Result := FShutdown;
  finally
    FLock.Release;
  end;
end;

{ TClientHandlerThread }

constructor TClientHandlerThread.Create(AClientSocket: TSocket; const AAuthToken: string;
AGetCurrentReading: TGetCurrentReadingFunc;
AGetPredictions: TGetPredictionsFunc;
const AStartedAtUtc: TDateTime; APort: word;
AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub);
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
  FStreamCounter := AStreamCounter;
  FHub := AHub;
end;

// Take one of the MAX_EVENT_STREAMS slots. Increment first and test after:
// a read-then-increment check would let two handlers racing through it both
// get in. FActiveCounter is deliberately untouched here; it counts every
// connection, streams included, and Stop drains on it.
function TClientHandlerThread.ReserveStreamSlot: boolean;
begin
  if FStreamCounter = nil then
    Exit(true);
  if InterlockedIncrement(FStreamCounter^) <= MAX_EVENT_STREAMS then
    Exit(true);
  InterlockedDecrement(FStreamCounter^);
  Result := false;
end;

procedure TClientHandlerThread.ReleaseStreamSlot;
begin
  if FStreamCounter <> nil then
    InterlockedDecrement(FStreamCounter^);
end;

// The token is normally carried as "Authorization: Bearer <token>". A browser
// EventSource cannot set headers, so a "?token=" query value is accepted too.
function TClientHandlerThread.CheckAuth(const Headers, QueryToken: string): boolean;
var
  HeaderVal, Scheme, Token: string;
  SpacePos: integer;
begin
  if FAuthToken = '' then
    Exit(true);

  Result := false;
  if QueryToken <> '' then
    Exit(ConstantTimeEquals(QueryToken, FAuthToken));

  HeaderVal := HeaderValue(Headers, 'authorization');
  if HeaderVal = '' then
    Exit;
  SpacePos := Pos(' ', HeaderVal);
  if SpacePos <= 0 then
    Exit;
  Scheme := LowerCase(Copy(HeaderVal, 1, SpacePos - 1));
  if Scheme <> 'bearer' then
    Exit;
  Token := Trim(Copy(HeaderVal, SpacePos + 1, MaxInt));
  Result := ConstantTimeEquals(Token, FAuthToken);
end;

function TClientHandlerThread.HandleRequest(const Request: string): string;
var
  Lines: TStringList;
  Method, URIPath, Query, Headers: string;
  ResponseObj: TJSONObject;
  CurrentReadings: BGResults;
  Predictions: BGResults;
  PredArray, Endpoints: TJSONArray;
  i: integer;
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

    ParseRequestLine(Request, Method, URIPath, Query);
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
    if not CheckAuth(Headers, QueryValue(Query, 'token')) then
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
        Endpoints.Add('/events');
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

function TClientHandlerThread.SendAll(const Data: string): boolean;
var
  Total, Sent: SizeInt;
  N: integer;
  P: PChar;
begin
  Result := true;
  if Data = '' then
    Exit;
  Total := Length(Data);
  Sent := 0;
  P := PChar(Data);
  while Sent < Total do
  begin
    if Terminated then
      Exit(false);
    {$IFDEF WINDOWS}
    N := WinSock2.send(FClientSocket, P[Sent], Total - Sent, 0);
    {$ELSE}
    N := fpSend(FClientSocket, @P[Sent], Total - Sent, SEND_FLAGS);
    {$ENDIF}
    if N <= 0 then
      Exit(false); // peer closed or error; nothing useful to do
    Inc(Sent, N);
  end;
end;

// One SSE frame: "id:", "event:", one "data:" line per payload line, blank line.
function TClientHandlerThread.SendEvent(const Event: TWebEvent): boolean;
var
  Frame, Line: string;
  DataLines: TStringArray;
begin
  Frame := 'id: ' + IntToStr(Event.Seq) + #10 +
    'event: ' + Event.Name + #10;
  DataLines := StringReplace(Event.Data, #13, '', [rfReplaceAll]).Split([#10]);
  if Length(DataLines) = 0 then
    Frame := Frame + 'data: '#10
  else
    for Line in DataLines do
      Frame := Frame + 'data: ' + Line + #10;
  Frame := Frame + #10;
  Result := SendAll(Frame);
end;

// GET /events: hold the connection open and stream hub events until the peer
// hangs up or the server shuts down. A fresh subscriber first receives a
// snapshot (every sticky event's latest payload); a client resuming with
// Last-Event-ID instead gets the events it missed, if the ring still has them.
procedure TClientHandlerThread.ServeEventStream(const Headers: string);
var
  LastSeq, SnapshotSeq: int64;
  Items: TWebEventArray;
  Ev: TWebEvent;
  i: integer;
  HaveReading: boolean;
  Readings: BGResults;
  Obj: TJSONObject;
  ReadFDs: TFDSet;
  TimeVal: TTimeVal;
  SelN, N: integer;
  Probe: array[0..255] of byte;
  LastSentMs: QWord;
begin
  if not SendAll('HTTP/1.1 200 OK'#13#10 +
    'Content-Type: text/event-stream'#13#10 +
    'Cache-Control: no-cache'#13#10 +
    'Connection: keep-alive'#13#10 +
    'Access-Control-Allow-Origin: *'#13#10 +
    'X-Accel-Buffering: no'#13#10#13#10 +
    'retry: 5000'#10#10) then
    Exit;

  if TryStrToInt64(HeaderValue(Headers, 'last-event-id'), LastSeq) and
     FHub.CopySince(LastSeq, Items) then
  begin
    // Resuming: replay what the client missed, nothing else.
  end
  else
  begin
    FHub.CopySticky(Items, SnapshotSeq);
    LastSeq := SnapshotSeq;

    // The hub only knows what was published since the server started; the
    // reading cache can predate it, so fall back to the reading callback.
    HaveReading := false;
    for i := 0 to High(Items) do
      if Items[i].Name = 'reading' then
        HaveReading := true;
    if (not HaveReading) and Assigned(FGetCurrentReading) then
    begin
      Readings := FGetCurrentReading();
      if Length(Readings) > 0 then
      begin
        Obj := ReadingToJSON(Readings[0], true);
        try
          Ev.Seq := SnapshotSeq;
          Ev.Name := 'reading';
          Ev.Data := Obj.AsJSON;
        finally
          Obj.Free;
        end;
        SetLength(Items, Length(Items) + 1);
        Items[High(Items)] := Ev;
      end;
    end;
  end;

  for i := 0 to High(Items) do
  begin
    if not SendEvent(Items[i]) then
      Exit;
    if Items[i].Seq > LastSeq then
      LastSeq := Items[i].Seq;
  end;
  LastSentMs := GetTickCount64;

  while (not Terminated) and (not FHub.IsShutdown) do
  begin
    // Wait a poll interval, and notice the peer hanging up meanwhile: a
    // readable socket that yields zero bytes is EOF. Anything the client
    // does send is ignored.
    FD_ZERO_Helper(ReadFDs);
    FD_SET_Helper(FClientSocket, ReadFDs);
    TimeVal.tv_sec := 0;
    TimeVal.tv_usec := EVENT_POLL_MS * 1000;
    SelN := SocketSelect(FClientSocket + 1, @ReadFDs, nil, nil, @TimeVal);
    if SelN < 0 then
      Exit;
    if SelN > 0 then
    begin
      {$IFDEF WINDOWS}
      N := WinSock2.recv(FClientSocket, Probe, SizeOf(Probe), 0);
      {$ELSE}
      N := fpRecv(FClientSocket, @Probe, SizeOf(Probe), 0);
      {$ENDIF}
      if N <= 0 then
        Exit;
    end;

    if FHub.LatestSeq > LastSeq then
    begin
      if not FHub.CopySince(LastSeq, Items) then
      begin
        // Fell too far behind for a replay: resynchronise from a snapshot.
        FHub.CopySticky(Items, SnapshotSeq);
        LastSeq := SnapshotSeq;
      end;
      for i := 0 to High(Items) do
      begin
        if not SendEvent(Items[i]) then
          Exit;
        if Items[i].Seq > LastSeq then
          LastSeq := Items[i].Seq;
      end;
      LastSentMs := GetTickCount64;
    end
    else
    if GetTickCount64 - LastSentMs >= EVENT_KEEPALIVE_MS then
    begin
      if not SendAll(': keepalive'#10#10) then
        Exit;
      LastSentMs := GetTickCount64;
    end;
  end;
end;

procedure TClientHandlerThread.Execute;
var
  Request, Response: string;
  Method, URIPath, Query: string;
  TooLarge: boolean;
  {$IFDEF DARWIN}
  OptVal: integer;
  {$ENDIF}
begin
  try
    try
      if FClientSocket = INVALID_SOCKET then
        Exit;

      {$IFDEF DARWIN}
      OptVal := 1;
      SocketSetOpt(FClientSocket, SOL_SOCKET, SO_NOSIGPIPE, pchar(@OptVal), SizeOf(OptVal));
      {$ENDIF}

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

      ParseRequestLine(Request, Method, URIPath, Query);
      if (Method = 'GET') and (URIPath = '/events') and Assigned(FHub) then
      begin
        if not CheckAuth(Request, QueryValue(Query, 'token')) then
          SendAll('HTTP/1.1 401 Unauthorized'#13#10 +
            'Content-Type: application/json'#13#10 +
            'Access-Control-Allow-Origin: *'#13#10 +
            'Connection: close'#13#10#13#10 +
            '{"error":"Unauthorized"}')
        else if not ReserveStreamSlot then
          // Authenticated, but every stream slot is taken. Retry-After
          // matches the "retry:" hint an open stream hands its subscriber.
          SendAll('HTTP/1.1 503 Service Unavailable'#13#10 +
            'Content-Type: application/json'#13#10 +
            'Access-Control-Allow-Origin: *'#13#10 +
            'Retry-After: 5'#13#10 +
            'Connection: close'#13#10#13#10 +
            '{"error":"Too many event streams"}')
        else
          try
            ServeEventStream(Request);
          finally
            // The one release for this slot, however the stream ended.
            ReleaseStreamSlot;
          end;
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
AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub);
begin
  inherited Create(true); // Create suspended
  FreeOnTerminate := false; // Owner stops + frees thread (needed for safe shutdown)
  FPort := APort;
  FAuthToken := AAuthToken;
  FGetCurrentReading := AGetCurrentReading;
  FGetPredictions := AGetPredictions;
  FLoopbackOnly := ALoopbackOnly;
  FActiveCounter := AActiveCounter;
  FStreamCounter := AStreamCounter;
  FHub := AHub;
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
          FStartedAtUtc, FPort, FActiveCounter, FStreamCounter, FHub);
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
  FActiveStreams := 0;
  FHub := TWebEventHub.Create;
  FThread := TWebServerThread.Create(APort, AAuthToken,
    AGetCurrentReading, AGetPredictions,
    ALoopbackOnly, @FActiveClients, @FActiveStreams, FHub);
end;

destructor TTrndiWebServer.Destroy;
begin
  Stop;
  // Stop drained every client handler, so nothing references the hub now.
  FreeAndNil(FHub);
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
    {$IFDEF HAIKU}
    // TThread.WaitFor pthread_joins a thread that FPC 3.2.2's cthreads has
    // already pthread_detach()ed in its exit path; Haiku frees the pthread
    // struct on detached exit, so the join GPFs. Poll Finished instead
    // (see SafeThreadJoin in trndi.native.threading).
    while not FThread.Finished do
      Sleep(1);
    {$ELSE}
    FThread.WaitFor;
    {$ENDIF}

    // 3. End every /events stream: their handlers otherwise live as long as
    //    the subscriber does, and the drain below would never finish.
    FHub.Shutdown;

    // 4. Wait for any in-flight client workers to finish before we let the
    //    parent free us -- the workers hold method pointers into the owner
    //    and a pointer to FActiveClients. A plain request is bounded by
    //    REQUEST_READ_TIMEOUT_MS plus handler time and a stream notices the
    //    shutdown within EVENT_POLL_MS, so this is finite.
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

function TTrndiWebServer.Publish(const AName, AData: string; ASticky: boolean;
AOnlyIfChanged: boolean): boolean;
begin
  Result := Assigned(FHub) and FHub.Publish(AName, AData, ASticky, AOnlyIfChanged);
end;

procedure TTrndiWebServer.PublishReading(const Reading: BGReading);
var
  Obj: TJSONObject;
begin
  Obj := ReadingToJSON(Reading, true);
  try
    Publish('reading', Obj.AsJSON, true, true);
  finally
    Obj.Free;
  end;
end;

procedure TTrndiWebServer.PublishPredictions(const Preds: BGResults);
var
  Obj: TJSONObject;
  Arr: TJSONArray;
  i: integer;
begin
  Obj := TJSONObject.Create;
  try
    Arr := TJSONArray.Create;
    for i := 0 to High(Preds) do
      Arr.Add(ReadingToJSON(Preds[i], true));
    Obj.Add('predictions', Arr);
    Publish('predict', Obj.AsJSON, true, true);
  finally
    Obj.Free;
  end;
end;

procedure TTrndiWebServer.PublishAlert(const Kinds: array of string; const Reading: BGReading);
var
  Obj: TJSONObject;
  Arr: TJSONArray;
  i: integer;
begin
  Obj := TJSONObject.Create;
  try
    Arr := TJSONArray.Create;
    for i := 0 to High(Kinds) do
      Arr.Add(Kinds[i]);
    Obj.Add('kinds', Arr);
    Obj.Add('reading', ReadingToJSON(Reading, true));
    Obj.Add('time_utc', FormatUtcIso(Now));
    Publish('alert', Obj.AsJSON, false, false);
  finally
    Obj.Free;
  end;
end;

procedure TTrndiWebServer.PublishStatus(const Fresh, Connected: boolean; const Detail: string);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('fresh', Fresh);
    Obj.Add('connected', Connected);
    Obj.Add('detail', Detail);
    Publish('status', Obj.AsJSON, true, true);
  finally
    Obj.Free;
  end;
end;

procedure TTrndiWebServer.PublishSnooze(const AActive: boolean; const AUntilLocal: TDateTime);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.Add('active', AActive);
    if AActive then
      Obj.Add('until_utc', FormatUtcIso(AUntilLocal))
    else
      Obj.Add('until_utc', '');
    Publish('snooze', Obj.AsJSON, true, true);
  finally
    Obj.Free;
  end;
end;

end.
