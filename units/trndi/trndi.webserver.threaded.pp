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
 * - 2026-09-15: SEND_FLAGS uses Haiku's own MSG_NOSIGNAL value ($0800) on
 *   Haiku, since FPC 3.2.2's sockets unit carries the BSD value there.
 * - 2026-09-15: Capped concurrent /events streams at MAX_EVENT_STREAMS with
 *   a dedicated counter; a subscriber past the cap is answered 503.
 * - 2026-09-29: Added the embedded dashboard (GET /), the /settings and
 *   /snooze endpoints backed by a command callback run on the main thread,
 *   Content-Length request bodies, and the loopback-or-token write policy.
 * - 2026-10-06: Added the Nightscout-compatible read endpoints
 *   (/api/v1/entries.json and aliases, /api/v1/status.json, /pebble,
 *   /sgv.json), authenticated the Nightscout way (api-secret or ?token=).
 *)
unit trndi.webserver.threaded;

{$mode objfpc}{$H+}

{
  Minimal HTTP server for exposing current glucose readings and predictions,
  plus a server-sent-events stream (/events) that pushes state changes to
  subscribers instead of making them poll, and a small built-in dashboard
  page (GET /) that consumes those endpoints from a browser.

  The dashboard's write side (/settings, /snooze) goes through one command
  callback (TWebCommandFunc). Unlike the reading callbacks it is run on the
  MAIN thread via TThread.Synchronize, so the owner can apply settings the
  way its settings dialog does. Writes are accepted only from a loopback peer
  or, when a token is configured, from any authenticated peer; the server is
  otherwise reachable by anyone on the network, and reading glucose is one
  thing, changing limits another.

  The same readings are also served in Nightscout's shape (/api/v1/entries.json
  and its aliases, /api/v1/status.json, /pebble, and xDrip's /sgv.json), so a
  client written for Nightscout can be pointed at Trndi. Those endpoints are
  read-only and take the token the way Nightscout clients send a secret; see
  ServeNightscout.

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
Classes, SysUtils, Sockets, fpjson, jsonparser, syncobjs, trndi.types, DateUtils, sha1
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

  {** Serve one dashboard command. Invoked ON THE MAIN THREAD (the handler
      marshals through TThread.Synchronize), so the implementation may touch
      UI state freely, but it must return promptly: the connection and the
      main thread both wait on it. ACommand is one of
        @unorderedList(
          @item(@code(settings.get): AParams is nil; describe the settings in AReply)
          @item(@code(settings.set): AParams holds the changes; apply them, then describe the result in AReply)
          @item(@code(snooze): AParams.minutes, 0 to resume; describe the snooze state in AReply))
      @returns(False to refuse the command; the client then gets 400 with AError.) }
TWebCommandFunc = function(const ACommand: string; const AParams: TJSONObject;
  AReply: TJSONObject; out AError: string): boolean of object;

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
  FCommand: TWebCommandFunc;
  FPeerIsLoopback: boolean;   // the connection came from 127.0.0.0/8
  // Parameters of the command in flight, handed across Synchronize.
  FCmdName: string;
  FCmdParams: TJSONObject;
  FCmdReply: TJSONObject;
  FCmdError: string;
  FCmdOk: boolean;
  function HandleRequest(const Request: string): string;
  function CheckAuth(const Headers, QueryToken: string): boolean;
  function CheckNightscoutAuth(const Headers, Query: string): boolean;
  function ServeNightscout(const Method, URIPath, Query, Headers: string;
    out Response: string): boolean;
  function WriteAllowed: boolean;
  function ServeCommand(const ACommand: string; AParams, AReply: TJSONObject): string;
  procedure RunCommandSync;
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
    AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub;
    ACommand: TWebCommandFunc; APeerIsLoopback: boolean);
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
  FCommand: TWebCommandFunc;
protected
  procedure Execute; override;
public
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    ALoopbackOnly: boolean;
    AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub;
    ACommand: TWebCommandFunc);
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
  {** ACommand backs the dashboard's /settings and /snooze; without it those
      endpoints answer 501 and the page hides its settings card. }
  constructor Create(APort: word; const AAuthToken: string;
    AGetCurrentReading: TGetCurrentReadingFunc;
    AGetPredictions: TGetPredictionsFunc;
    ALoopbackOnly: boolean = false;
    ACommand: TWebCommandFunc = nil);
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
{$I ../../inc/web_dashboard.inc}

const
INVALID_SOCKET = TSocket(-1);
MAX_REQUEST_SIZE = 16 * 1024;          // hard cap on inbound request bytes
REQUEST_READ_TIMEOUT_MS = 5000;        // total budget to receive headers
EVENT_RING_SIZE = 128;                 // events kept for Last-Event-ID replay
EVENT_POLL_MS = 250;                   // how often a stream handler looks for news
EVENT_KEEPALIVE_MS = 15000;            // idle comment so proxies/NATs keep the stream
NS_DEFAULT_COUNT = 10;                 // entries Nightscout returns when "count" is absent
// Reported in the Nightscout status document. Deliberately lower than any
// real Nightscout release: a client that gates features on the version then
// settles for the plain v1 read API, which is all that is served here.
NS_COMPAT_VERSION = '0.0.0-trndi';
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
// Haiku's value is spelled out: FPC 3.2.2's Haiku sockets unit copies the
// BSD constant ($20000), but Haiku's sys/socket.h defines MSG_NOSIGNAL as
// 0x0800. The kernel ignores the BSD bit, so SIGPIPE fired regardless.
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
SEND_FLAGS = 0;
{$ELSEIF DEFINED(HAIKU)}
SEND_FLAGS = $0800;
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

function NetToHostLong(v: longword): longword;
begin
  Result := WinSock2.ntohl(v);
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

function NetToHostLong(v: longword): longword;
begin
  Result := ntohl(v);
end;
{$ENDIF}

// True for any 127.0.0.0/8 address (network byte order in).
function IsLoopbackAddr(const NetAddr: longword): boolean;
begin
  Result := (NetToHostLong(NetAddr) shr 24) = 127;
end;

// Parses a JSON object body. An empty body counts as an empty object; a
// non-object or malformed body fails. The caller owns Obj on success.
function ParseJSONBody(const Body: string; out Obj: TJSONObject): boolean;
var
  Data: TJSONData;
begin
  Obj := nil;
  if Trim(Body) = '' then
  begin
    Obj := TJSONObject.Create;
    Exit(true);
  end;
  try
    Data := GetJSON(Body);
  except
    Exit(false);
  end;
  if Data is TJSONObject then
  begin
    Obj := TJSONObject(Data);
    Result := true;
  end
  else
  begin
    Data.Free;
    Result := false;
  end;
end;

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

{ Nightscout-compatible documents }

type
  // The Nightscout (and xDrip web service) resources this server answers.
TNightscoutRoute = (nrNone, nrEntries, nrCurrent, nrStatus, nrPebble, nrEmptyList);

// Maps a request path to the Nightscout resource it names. Nightscout serves
// its v1 resources with and without ".json". xDrip's two spellings (/sgv.json,
// /status.json) exist only with the suffix, and /status without it is this
// server's own endpoint. Repeated slashes count as one: clients that join a
// base URL ending in "/" to a path (Trndi's own Nightscout driver among them)
// ask for "/api/v1//entries.json", and the proxies in front of a real
// Nightscout merge that for them.
function NightscoutRouteOf(const URIPath: string): TNightscoutRoute;
var
  P: string;
begin
  P := URIPath;
  while Pos('//', P) > 0 do
    P := StringReplace(P, '//', '/', [rfReplaceAll]);

  if P = '/sgv.json' then
    Exit(nrEntries);
  if P = '/status.json' then
    Exit(nrStatus);
  if P = '/pebble' then
    Exit(nrPebble);

  if P.EndsWith('.json') then
    SetLength(P, Length(P) - 5);
  case P of
  '/api/v1/entries', '/api/v1/entries/sgv':
    Result := nrEntries;
  '/api/v1/entries/current':
    Result := nrCurrent;
  '/api/v1/status':
    Result := nrStatus;
  // Trndi relays readings only, so these lists are served empty: a client
  // that asks for them gets "nothing there" rather than a 404 to trip over.
  '/api/v1/devicestatus', '/api/v1/treatments':
    Result := nrEmptyList;
  else
    Result := nrNone;
  end;
end;

// Milliseconds since the Unix epoch, the form Nightscout carries in "date".
function EpochMs(const ALocalTime: TDateTime): int64;
begin
  Result := Round((LocalTimeToUniversal(ALocalTime) - UnixDateDelta) * MSecsPerDay);
end;

function FormatUtcIsoMs(const ALocalTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"',
    LocalTimeToUniversal(ALocalTime));
end;

// Nightscout's name for a trend. BG_TRENDS_STRING already holds those names;
// only the placeholder has none of its own.
function NightscoutDirection(const Trend: BGTrend): string;
begin
  if Trend = TdPlaceholder then
    Result := 'NONE'
  else
    Result := BG_TRENDS_STRING[Trend];
end;

// Nightscout's number for a trend, as /pebble reports it: 0 is NONE, then
// DoubleUp (1) through NOT COMPUTABLE (8) in BGTrend's own order.
function NightscoutTrendNumber(const Trend: BGTrend): integer;
begin
  if Trend = TdPlaceholder then
    Result := 0
  else
    Result := Ord(Trend) + 1;
end;

// One reading as a Nightscout "sgv" entry. Values are whole mg/dL, which is
// what Nightscout stores whatever unit it displays.
function ReadingToNightscoutEntry(const Reading: BGReading): TJSONObject;
var
  Ms: int64;
  Stamp: string;
begin
  Ms := EpochMs(Reading.date);
  Stamp := FormatUtcIsoMs(Reading.date);
  Result := TJSONObject.Create;
  try
    // Nightscout ids are 24 hex digits; derive a stable one from the time.
    Result.Add('_id', LowerCase(IntToHex(Ms, 24)));
    Result.Add('device', 'Trndi');
    Result.Add('date', Ms);
    Result.Add('dateString', Stamp);
    Result.Add('sysTime', Stamp);
    Result.Add('sgv', Round(Reading.convert(mgdl, BGPrimary)));
    if not Reading.deltaEmpty then
      Result.Add('delta', Round(Reading.convert(mgdl, BGDelta)));
    Result.Add('direction', NightscoutDirection(Reading.trend));
    Result.Add('type', 'sgv');
    Result.Add('utcOffset', -GetLocalTimeOffset);
  except
    Result.Free;
    raise;
  end;
end;

// The newest MaxCount readings as a Nightscout entries array. Readings
// without a value are left out, as Nightscout has no entry for them either.
function NightscoutEntries(const Readings: BGResults; MaxCount: integer): TJSONArray;
var
  i: integer;
begin
  Result := TJSONArray.Create;
  try
    for i := 0 to High(Readings) do
    begin
      if Result.Count >= MaxCount then
        Break;
      if Readings[i].empty then
        Continue;
      Result.Add(ReadingToNightscoutEntry(Readings[i]));
    end;
  except
    Result.Free;
    raise;
  end;
end;

// The /pebble document: server time plus the newest readings, with values as
// strings in the display unit (the one resource where Nightscout converts).
function NightscoutPebble(const Readings: BGResults; MaxCount: integer;
  AsMmol: boolean): TJSONObject;
var
  fs: TFormatSettings;
  Status, Bgs: TJSONArray;
  NowObj, Bg: TJSONObject;
  u: BGUnit;
  Fmt: string;
  i: integer;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  if AsMmol then
  begin
    u := mmol;
    Fmt := '0.0';
  end
  else
  begin
    u := mgdl;
    Fmt := '0';
  end;

  Result := TJSONObject.Create;
  try
    NowObj := TJSONObject.Create;
    NowObj.Add('now', EpochMs(Now));
    Status := TJSONArray.Create;
    Status.Add(NowObj);
    Result.Add('status', Status);

    Bgs := TJSONArray.Create;
    Result.Add('bgs', Bgs);
    for i := 0 to High(Readings) do
    begin
      if Bgs.Count >= MaxCount then
        Break;
      if Readings[i].empty then
        Continue;
      Bg := TJSONObject.Create;
      Bgs.Add(Bg);
      Bg.Add('sgv', FormatFloat(Fmt, Readings[i].convert(u, BGPrimary), fs));
      Bg.Add('trend', NightscoutTrendNumber(Readings[i].trend));
      Bg.Add('direction', NightscoutDirection(Readings[i].trend));
      Bg.Add('datetime', EpochMs(Readings[i].date));
      if not Readings[i].deltaEmpty then
        Bg.Add('bgdelta', FormatFloat(Fmt, Readings[i].convert(u, BGDelta), fs));
    end;

    Result.Add('cals', TJSONArray.Create);
  except
    Result.Free;
    raise;
  end;
end;

// The Nightscout status document. ASettings is the reply of the settings.get
// command, or nil when the owner serves none; the unit then reads mg/dl and
// the thresholds are left out. Thresholds are mg/dL, as in Nightscout. A
// disabled in-range band is reported as the limits themselves, since
// Nightscout always carries all four values and clients expect them.
function NightscoutStatus(const ASettings: TJSONObject): TJSONObject;
var
  Settings, Thresholds, Src: TJSONObject;
  Data: TJSONData;
  NowLocal: TDateTime;
  Lo, Hi: integer;
begin
  NowLocal := Now;
  Result := TJSONObject.Create;
  try
    Result.Add('status', 'ok');
    Result.Add('name', 'Trndi');
    Result.Add('version', NS_COMPAT_VERSION);
    Result.Add('serverTime', FormatUtcIsoMs(NowLocal));
    Result.Add('serverTimeEpoch', EpochMs(NowLocal));
    Result.Add('apiEnabled', true);
    Result.Add('careportalEnabled', false);

    Settings := TJSONObject.Create;
    Result.Add('settings', Settings);
    if Assigned(ASettings) and (ASettings.Get('unit', '') = 'mmol') then
      Settings.Add('units', 'mmol')
    else
      Settings.Add('units', 'mg/dl');

    Src := nil;
    if Assigned(ASettings) then
    begin
      Data := ASettings.Find('thresholds');
      if Data is TJSONObject then
        Src := TJSONObject(Data);
    end;
    if Assigned(Src) then
    begin
      Lo := Src.Get('lo', 0);
      Hi := Src.Get('hi', 0);
      Thresholds := TJSONObject.Create;
      Settings.Add('thresholds', Thresholds);
      Thresholds.Add('bgHigh', Hi);
      // Get falls back to its default for the null of a disabled band.
      Thresholds.Add('bgTargetTop', Src.Get('range_hi', Hi));
      Thresholds.Add('bgTargetBottom', Src.Get('range_lo', Lo));
      Thresholds.Add('bgLow', Lo);
    end;
  except
    Result.Free;
    raise;
  end;
end;

// The "count" query value, or ADefault when it is absent or not a positive
// number.
function CountParam(const Query: string; ADefault: integer): integer;
begin
  Result := StrToIntDef(QueryValue(Query, 'count'), ADefault);
  if Result < 1 then
    Result := ADefault;
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
AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub;
ACommand: TWebCommandFunc; APeerIsLoopback: boolean);
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
  FCommand := ACommand;
  FPeerIsLoopback := APeerIsLoopback;
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
// EventSource cannot set headers, so the /events caller passes the "?token="
// query value too; every other caller passes an empty QueryToken.
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

// Auth for the Nightscout-compatible endpoints. A Nightscout client has no
// way to send a bearer token: it carries the secret as an "api-secret" header
// holding the secret's SHA-1 in hex, or as a "?token=" query value. Both are
// accepted here, with the configured token standing in for the secret; the
// bearer header keeps working too. Unlike the plain endpoints this lets the
// token into a URL, which is the price of serving clients (watch faces,
// widgets) that can be given nothing but one.
function TClientHandlerThread.CheckNightscoutAuth(const Headers, Query: string): boolean;
var
  Secret, Token: string;
begin
  if FAuthToken = '' then
    Exit(true);

  Secret := HeaderValue(Headers, 'api-secret');
  if Secret <> '' then
    Exit(ConstantTimeEquals(LowerCase(Secret), SHA1Print(SHA1String(FAuthToken))));

  Token := QueryValue(Query, 'token');
  if Token <> '' then
    Exit(ConstantTimeEquals(Token, FAuthToken));

  Result := CheckAuth(Headers, '');
end;

// Answers the Nightscout-compatible read endpoints (see NightscoutRouteOf).
// Units and limits are the owner's, so the status document (and /pebble,
// when the request names no unit) asks for them through the settings.get
// command, which runs on the main thread like any other command.
// @returns(False when URIPath is not one of them; Response is then empty.)
function TClientHandlerThread.ServeNightscout(const Method, URIPath, Query,
  Headers: string; out Response: string): boolean;
var
  Route: TNightscoutRoute;
  StatusLine, Body, Units: string;
  Readings: BGResults;
  Settings: TJSONObject;
  Data: TJSONData;
begin
  Response := '';
  Route := NightscoutRouteOf(URIPath);
  if Route = nrNone then
    Exit(false);
  Result := true;

  if Method <> 'GET' then
  begin
    StatusLine := 'HTTP/1.1 405 Method Not Allowed';
    Body := '{"error":"Method not allowed"}';
  end
  else
  if not CheckNightscoutAuth(Headers, Query) then
  begin
    // The body Nightscout itself sends, for clients that look at it.
    StatusLine := 'HTTP/1.1 401 Unauthorized';
    Body := '{"status":401,"message":"Unauthorized","description":"Invalid/Missing"}';
  end
  else
  begin
    Readings := nil;
    if (Route in [nrEntries, nrCurrent, nrPebble]) and Assigned(FGetCurrentReading) then
      Readings := FGetCurrentReading();

    Units := QueryValue(Query, 'units');
    Settings := nil;
    Data := nil;
    try
      if (Route = nrStatus) or ((Route = nrPebble) and (Units = '')) then
      begin
        Settings := TJSONObject.Create;
        if not ServeCommand('settings.get', nil, Settings).StartsWith('HTTP/1.1 200') then
          FreeAndNil(Settings);
      end;

      case Route of
      nrEntries:
        Data := NightscoutEntries(Readings, CountParam(Query, NS_DEFAULT_COUNT));
      nrCurrent:
        Data := NightscoutEntries(Readings, 1);
      nrStatus:
        Data := NightscoutStatus(Settings);
      nrPebble:
      begin
        if Assigned(Settings) then
          Units := Settings.Get('unit', '');
        Data := NightscoutPebble(Readings, CountParam(Query, 1), Units = 'mmol');
      end;
      else
        Data := TJSONArray.Create;
      end;
      StatusLine := 'HTTP/1.1 200 OK';
      Body := Data.AsJSON;
    finally
      Data.Free;
      Settings.Free;
    end;
  end;

  // Content-Length is spelled out for these: the clients are often small
  // embedded HTTP stacks that do not read until the connection closes.
  Response := StatusLine + #13#10 +
    'Content-Type: application/json; charset=utf-8'#13#10 +
    'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
    'Access-Control-Allow-Origin: *'#13#10 +
    'Cache-Control: no-cache'#13#10 +
    'Connection: close'#13#10#13#10 +
    Body;
end;

// The write policy for /settings and /snooze. Reading glucose off the LAN is
// what the server is for; changing the app's limits is not something an
// unauthenticated peer elsewhere on the network gets to do. A loopback peer
// is the same user as the one at the keyboard; anyone else needs the token
// (which CheckAuth has verified by the time this is asked).
function TClientHandlerThread.WriteAllowed: boolean;
begin
  Result := FPeerIsLoopback or (FAuthToken <> '');
end;

// Body of the callback, on the main thread. The try/except is here rather
// than around Synchronize because an exception raised inside a synchronized
// method is re-raised in the calling thread, but we also want the message.
procedure TClientHandlerThread.RunCommandSync;
begin
  try
    FCmdOk := FCommand(FCmdName, FCmdParams, FCmdReply, FCmdError);
  except
    on E: Exception do
    begin
      FCmdOk := false;
      FCmdError := E.Message;
    end;
  end;
end;

// Runs ACommand on the main thread and leaves its answer in AReply.
// @returns(The HTTP status line for the response.)
function TClientHandlerThread.ServeCommand(const ACommand: string;
  AParams, AReply: TJSONObject): string;
begin
  if not Assigned(FCommand) then
  begin
    AReply.Add('error', 'Not supported');
    Exit('HTTP/1.1 501 Not Implemented'#13#10);
  end;
  // The owner is tearing down: its Stop drains handlers while servicing
  // Synchronize, but a command started now would land on a form that is
  // going away.
  if Assigned(FHub) and FHub.IsShutdown then
  begin
    AReply.Add('error', 'Shutting down');
    Exit('HTTP/1.1 503 Service Unavailable'#13#10);
  end;

  FCmdName := ACommand;
  FCmdParams := AParams;
  FCmdReply := AReply;
  FCmdError := '';
  FCmdOk := false;
  Synchronize(@RunCommandSync);
  FCmdParams := nil;
  FCmdReply := nil;

  if FCmdOk then
    Exit('HTTP/1.1 200 OK'#13#10);
  AReply.Clear;
  if FCmdError = '' then
    FCmdError := 'Rejected';
  AReply.Add('error', FCmdError);
  Result := 'HTTP/1.1 400 Bad Request'#13#10;
end;

function TClientHandlerThread.HandleRequest(const Request: string): string;
var
  Lines: TStringList;
  Method, URIPath, Query, Headers, Body, NsResponse: string;
  ResponseObj, Params: TJSONObject;
  CurrentReadings: BGResults;
  Predictions: BGResults;
  PredArray, Endpoints: TJSONArray;
  i, P: integer;
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
    // Headers end at the first blank line; whatever follows is the body
    // (ReadRequest has already waited for Content-Length bytes of it).
    P := Pos(#13#10#13#10, Request);
    if P > 0 then
    begin
      Headers := Copy(Request, 1, P + 1);
      Body := Copy(Request, P + 4, MaxInt);
    end
    else
    begin
      Headers := Request;
      Body := '';
    end;

    // The dashboard page. Served without auth: a browser navigating here
    // cannot send a header, and the page holds nothing but markup - every
    // value on it comes from the authenticated endpoints it calls.
    if (Method = 'GET') and ((URIPath = '/') or (URIPath = '/dashboard')) then
    begin
      Result := 'HTTP/1.1 200 OK'#13#10 +
        'Content-Type: text/html; charset=utf-8'#13#10 +
        'Content-Length: ' + IntToStr(Length(WEB_DASHBOARD_HTML)) + #13#10 +
        'Cache-Control: no-cache'#13#10 +
        'Connection: close'#13#10#13#10 +
        WEB_DASHBOARD_HTML;
      Exit;
    end;

    // CORS preflight
    if Method = 'OPTIONS' then
    begin
      Result := 'HTTP/1.1 204 No Content'#13#10 +
        'Access-Control-Allow-Origin: *'#13#10 +
        'Access-Control-Allow-Methods: GET, POST, OPTIONS'#13#10 +
        'Access-Control-Allow-Headers: Content-Type, Authorization, api-secret'#13#10#13#10;
      Exit;
    end;

    // The Nightscout-compatible endpoints authenticate the Nightscout way,
    // so they are answered ahead of the bearer check below.
    if ServeNightscout(Method, URIPath, Query, Headers, NsResponse) then
    begin
      Result := NsResponse;
      Exit;
    end;

    // Check auth. Only the /events stream (handled before this routine is
    // reached) accepts a "?token=" query value; plain endpoints require the
    // Authorization header so the token stays out of URLs and logs.
    if not CheckAuth(Headers, '') then
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
      if (URIPath = '/settings') and (Method = 'GET') then
      begin
        Result := ServeCommand('settings.get', nil, ResponseObj);
        if Result.StartsWith('HTTP/1.1 200') then
          ResponseObj.Add('writable', WriteAllowed);
      end
      else
      if ((URIPath = '/settings') or (URIPath = '/snooze')) and (Method = 'POST') then
      begin
        if not WriteAllowed then
        begin
          ResponseObj.Add('error', 'Changes need a localhost connection or a configured token');
          Result := 'HTTP/1.1 403 Forbidden'#13#10;
        end
        else
        if not ParseJSONBody(Body, Params) then
        begin
          ResponseObj.Add('error', 'Body must be a JSON object');
          Result := 'HTTP/1.1 400 Bad Request'#13#10;
        end
        else
          try
            if URIPath = '/snooze' then
              Result := ServeCommand('snooze', Params, ResponseObj)
            else
            begin
              Result := ServeCommand('settings.set', Params, ResponseObj);
              if Result.StartsWith('HTTP/1.1 200') then
                ResponseObj.Add('writable', true);
            end;
          finally
            Params.Free;
          end;
      end
      else
      if (URIPath = '/settings') or (URIPath = '/snooze') then
      begin
        ResponseObj.Add('error', 'Method not allowed');
        Result := 'HTTP/1.1 405 Method Not Allowed'#13#10;
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
        Endpoints.Add('/');
        Endpoints.Add('/glucose');
        Endpoints.Add('/predict');
        Endpoints.Add('/status');
        Endpoints.Add('/health');
        Endpoints.Add('/events');
        Endpoints.Add('/settings');
        Endpoints.Add('/snooze');
        Endpoints.Add('/api/v1/entries.json');
        Endpoints.Add('/api/v1/status.json');
        Endpoints.Add('/pebble');
        Endpoints.Add('/sgv.json');
        ResponseObj.Add('endpoints', Endpoints);
        ResponseObj.Add('command_support', Assigned(FCommand));

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

// Reads bytes from the client up to MAX_REQUEST_SIZE: the headers (until
// CRLFCRLF) plus, when they announce one, a Content-Length body. Bounded by
// REQUEST_READ_TIMEOUT_MS total wall time across all recv calls so a
// slow/silent peer cannot tie up the handler.
function TClientHandlerThread.ReadRequest(out Request: string; out TooLarge: boolean): boolean;
var
  Buffer: array[0..2047] of byte;
  BytesRead: integer;
  ReqStream: TMemoryStream;
  StartScan, j: NativeInt;
  HeaderEnd, Needed: NativeInt;   // byte counts; Needed < 0 until the headers are in
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
    HeaderEnd := 0;
    Needed := -1;
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

      if (not Found) and (ReqStream.Size >= 4) then
      begin
        PBuf := PByte(ReqStream.Memory);
        for j := StartScan to ReqStream.Size - 4 do
          if (PBuf[j] = 13) and (PBuf[j+1] = 10) and (PBuf[j+2] = 13) and (PBuf[j+3] = 10) then
          begin
            Found := true;
            HeaderEnd := j + 4;
            Break;
          end;
      end;
      if Found then
      begin
        if Needed < 0 then
        begin
          // End of headers. A POST announces its body with Content-Length;
          // keep reading until that many bytes follow the blank line.
          SetLength(Request, HeaderEnd);
          Move(ReqStream.Memory^, Request[1], HeaderEnd);
          Needed := HeaderEnd +
            StrToIntDef(HeaderValue(Request, 'content-length'), 0);
          if Needed < HeaderEnd then
            Needed := HeaderEnd;
          if Needed > MAX_REQUEST_SIZE then
          begin
            TooLarge := true;
            Exit;
          end;
        end;
        if ReqStream.Size >= Needed then
          Break;
      end;
    end;

    if ReqStream.Size > 0 then
    begin
      SetLength(Request, ReqStream.Size);
      Move(ReqStream.Memory^, Request[1], ReqStream.Size);
      // Only "good" if we saw end-of-headers and the announced body arrived.
      Result := Found and (ReqStream.Size >= Needed);
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
AActiveCounter, AStreamCounter: PLongInt; AHub: TWebEventHub;
ACommand: TWebCommandFunc);
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
  FCommand := ACommand;
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
  PeerLoopback: boolean;
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

      // Where the peer is decides whether it may change settings; see
      // TClientHandlerThread.WriteAllowed.
      PeerLoopback := (SockLen >= SizeOf(SockAddr)) and
        IsLoopbackAddr(SockAddr.sin_addr.s_addr);

      // Hand off to a per-connection worker so concurrent clients do not
      // block each other. Increment BEFORE Start so the owner's Stop()
      // sees the in-flight worker even if scheduling delays it.
      if FActiveCounter <> nil then
        InterlockedIncrement(FActiveCounter^);
      try
        Client := TClientHandlerThread.Create(ClientSocket, FAuthToken,
          FGetCurrentReading, FGetPredictions,
          FStartedAtUtc, FPort, FActiveCounter, FStreamCounter, FHub,
          FCommand, PeerLoopback);
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
ALoopbackOnly: boolean; ACommand: TWebCommandFunc);
begin
  inherited Create;
  FPort := APort;
  FEnabled := false;
  FActiveClients := 0;
  FActiveStreams := 0;
  FHub := TWebEventHub.Create;
  FThread := TWebServerThread.Create(APort, AAuthToken,
    AGetCurrentReading, AGetPredictions,
    ALoopbackOnly, @FActiveClients, @FActiveStreams, FHub, ACommand);
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
    //    shutdown within EVENT_POLL_MS, so this is finite. Stop runs on the
    //    main thread, and a handler may be parked in Synchronize waiting
    //    for that very thread: CheckSynchronize services it (and sleeps
    //    the poll interval otherwise) so the drain cannot deadlock on it.
    while InterlockedExchangeAdd(FActiveClients, 0) > 0 do
      CheckSynchronize(CLIENT_DRAIN_POLL_MS);

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
