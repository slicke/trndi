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
unit webserver_events_tests;

{$mode objfpc}{$H+}

{
  Tests for the web API's /events server-sent-events stream. A real
  TTrndiWebServer is started on a loopback port and driven with raw sockets,
  so the tests stay offline and need no LCL.
}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Sockets, syncobjs,
  trndi.types, trndi.webserver.threaded;

type
  { Reads everything the server sends into a buffer until EOF. }
  TSseReader = class(TThread)
  private
    FSocket: TSocket;
    FLock: TCriticalSection;
    FBuffer: string;
    FEof: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
    function Snapshot: string;
    function Eof: boolean;
    {** Wait until the buffer contains ASubstr or ATimeoutMs pass. }
    function WaitForText(const ASubstr: string; ATimeoutMs: integer): boolean;
    function WaitForEof(ATimeoutMs: integer): boolean;
  end;

  { One raw HTTP connection to the test server. }
  TSseClient = class
  private
    FSocket: TSocket;
    FReader: TSseReader;
  public
    constructor Create(APort: word; const ARequest: string);
    destructor Destroy; override;
    property Reader: TSseReader read FReader;
  end;

  TWebServerEventsTests = class(TTestCase)
  private
    FServer: TTrndiWebServer;
    FReadings: BGResults;
    function GetReadings: BGResults;
    function GetPredictions: BGResults;
    procedure StartServer(const AToken: string = '');
    function Port: word;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHealthListsEvents;
    procedure TestSnapshotOnConnect;
    procedure TestPublishedEventArrives;
    procedure TestStickyDedupe;
    procedure TestReplayWithLastEventId;
    procedure TestQueryTokenAuth;
    procedure TestStopClosesStream;
    procedure TestEventStreamLimit;
    procedure TestHubReplayAndRingOverflow;
  end;

implementation

const
  TEST_PORT_BASE = 18470;
  WAIT_MS = 4000;

var
  PortCounter: integer = 0;

function CountOccurrences(const Haystack, Needle: string): integer;
var
  P: integer;
begin
  Result := 0;
  P := Pos(Needle, Haystack);
  while P > 0 do
  begin
    Inc(Result);
    P := Pos(Needle, Haystack, P + Length(Needle));
  end;
end;

{ TSseReader }

constructor TSseReader.Create(ASocket: TSocket);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FSocket := ASocket;
  FLock := TCriticalSection.Create;
  FBuffer := '';
  FEof := false;
  Start;
end;

destructor TSseReader.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TSseReader.Execute;
var
  Buf: array[0..4095] of byte;
  N: integer;
  Chunk: string;
begin
  repeat
    N := fpRecv(FSocket, @Buf, SizeOf(Buf), 0);
    if N > 0 then
    begin
      SetLength(Chunk, N);
      Move(Buf, Chunk[1], N);
      FLock.Acquire;
      try
        FBuffer := FBuffer + Chunk;
      finally
        FLock.Release;
      end;
    end;
  until N <= 0;
  FLock.Acquire;
  try
    FEof := true;
  finally
    FLock.Release;
  end;
end;

function TSseReader.Snapshot: string;
begin
  FLock.Acquire;
  try
    Result := FBuffer;
  finally
    FLock.Release;
  end;
end;

function TSseReader.Eof: boolean;
begin
  FLock.Acquire;
  try
    Result := FEof;
  finally
    FLock.Release;
  end;
end;

function TSseReader.WaitForText(const ASubstr: string; ATimeoutMs: integer): boolean;
var
  Deadline: QWord;
begin
  Deadline := GetTickCount64 + ATimeoutMs;
  repeat
    if Pos(ASubstr, Snapshot) > 0 then
      Exit(true);
    if Eof then
      Exit(Pos(ASubstr, Snapshot) > 0);
    Sleep(20);
  until GetTickCount64 >= Deadline;
  Result := false;
end;

function TSseReader.WaitForEof(ATimeoutMs: integer): boolean;
var
  Deadline: QWord;
begin
  Deadline := GetTickCount64 + ATimeoutMs;
  repeat
    if Eof then
      Exit(true);
    Sleep(20);
  until GetTickCount64 >= Deadline;
  Result := false;
end;

{ TSseClient }

constructor TSseClient.Create(APort: word; const ARequest: string);
var
  Addr: TInetSockAddr;
  Attempt: integer;
  Connected: boolean;
begin
  inherited Create;
  FReader := nil;
  Connected := false;
  // The listener thread needs a moment to bind after Start.
  for Attempt := 1 to 100 do
  begin
    FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
    if FSocket < 0 then
      raise Exception.Create('fpSocket failed');
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(APort);
    Addr.sin_addr.s_addr := htonl($7F000001);
    if fpConnect(FSocket, @Addr, SizeOf(Addr)) = 0 then
    begin
      Connected := true;
      Break;
    end;
    CloseSocket(FSocket);
    FSocket := -1;
    Sleep(20);
  end;
  if not Connected then
    raise Exception.CreateFmt('could not connect to test web server on port %d', [APort]);
  if fpSend(FSocket, @ARequest[1], Length(ARequest), 0) <> Length(ARequest) then
    raise Exception.Create('fpSend failed');
  FReader := TSseReader.Create(FSocket);
end;

destructor TSseClient.Destroy;
begin
  if FSocket >= 0 then
  begin
    fpShutdown(FSocket, 2);
    CloseSocket(FSocket);
  end;
  if Assigned(FReader) then
  begin
    FReader.WaitFor;
    FReader.Free;
  end;
  inherited Destroy;
end;

{ TWebServerEventsTests }

function TWebServerEventsTests.GetReadings: BGResults;
begin
  Result := Copy(FReadings);
end;

function TWebServerEventsTests.GetPredictions: BGResults;
begin
  Result := nil;
end;

function TWebServerEventsTests.Port: word;
begin
  Result := TEST_PORT_BASE + PortCounter;
end;

procedure TWebServerEventsTests.StartServer(const AToken: string);
begin
  Inc(PortCounter);
  FServer := TTrndiWebServer.Create(Port, AToken, @GetReadings, @GetPredictions, true);
  FServer.Start;
end;

procedure TWebServerEventsTests.SetUp;
begin
  inherited SetUp;
  FServer := nil;
  SetLength(FReadings, 1);
  FReadings[0].Init(mgdl);
  FReadings[0].update(120, 4, mgdl);
  FReadings[0].date := EncodeDate(2026, 9, 12) + EncodeTime(10, 0, 0, 0);
end;

procedure TWebServerEventsTests.TearDown;
begin
  FreeAndNil(FServer);
  inherited TearDown;
end;

function Get(const Path: string; const Extra: string = ''): string;
begin
  Result := 'GET ' + Path + ' HTTP/1.1'#13#10 +
    'Host: localhost'#13#10 + Extra + #13#10;
end;

procedure TWebServerEventsTests.TestHealthListsEvents;
var
  C: TSseClient;
begin
  StartServer;
  C := TSseClient.Create(Port, Get('/health'));
  try
    AssertTrue('health answers', C.Reader.WaitForEof(WAIT_MS));
    AssertTrue('health lists /events', Pos('"/events"', C.Reader.Snapshot) > 0);
  finally
    C.Free;
  end;
end;

procedure TWebServerEventsTests.TestSnapshotOnConnect;
var
  C: TSseClient;
  S: string;
begin
  StartServer;
  FServer.PublishStatus(true, true, '');
  C := TSseClient.Create(Port, Get('/events'));
  try
    AssertTrue('reading snapshot arrives', C.Reader.WaitForText('event: reading', WAIT_MS));
    AssertTrue('status snapshot arrives', C.Reader.WaitForText('event: status', WAIT_MS));
    S := C.Reader.Snapshot;
    AssertTrue('SSE content type', Pos('Content-Type: text/event-stream', S) > 0);
    AssertTrue('reading value from the cache callback', Pos('"mgdl" : 120', S) > 0);
    AssertTrue('level is on the wire', Pos('"level" : "', S) > 0);
    AssertTrue('status payload', Pos('"fresh" : true', S) > 0);
    AssertEquals('exactly one reading in the snapshot', 1, CountOccurrences(S, 'event: reading'));
  finally
    C.Free;
  end;
end;

procedure TWebServerEventsTests.TestPublishedEventArrives;
var
  C: TSseClient;
  S: string;
begin
  StartServer;
  C := TSseClient.Create(Port, Get('/events'));
  try
    AssertTrue('snapshot arrives', C.Reader.WaitForText('event: reading', WAIT_MS));
    FServer.PublishAlert(['low', 'urgent_low'], FReadings[0]);
    AssertTrue('alert arrives', C.Reader.WaitForText('event: alert', WAIT_MS));
    AssertTrue('alert kinds', C.Reader.WaitForText('"urgent_low"', WAIT_MS));
    FReadings[0].update(130, 10, mgdl);
    FServer.PublishReading(FReadings[0]);
    AssertTrue('new reading arrives', C.Reader.WaitForText('"mgdl" : 130', WAIT_MS));
    S := C.Reader.Snapshot;
    AssertTrue('frames carry ids', Pos(#10'id: ', S) > 0);
  finally
    C.Free;
  end;
end;

procedure TWebServerEventsTests.TestStickyDedupe;
var
  C: TSseClient;
begin
  StartServer;
  FServer.PublishStatus(true, true, '');
  FServer.PublishStatus(true, true, '');
  C := TSseClient.Create(Port, Get('/events'));
  try
    AssertTrue('status snapshot arrives', C.Reader.WaitForText('event: status', WAIT_MS));
    FServer.PublishStatus(true, true, '');   // unchanged: dropped
    FServer.PublishStatus(false, true, 'x'); // changed: sent
    AssertTrue('changed status arrives', C.Reader.WaitForText('"fresh" : false', WAIT_MS));
    AssertEquals('unchanged status was not resent', 2,
      CountOccurrences(C.Reader.Snapshot, 'event: status'));
  finally
    C.Free;
  end;
end;

procedure TWebServerEventsTests.TestReplayWithLastEventId;
var
  A, B: TSseClient;
  S: string;
  P, E: integer;
  LastId: string;
begin
  StartServer;
  A := TSseClient.Create(Port, Get('/events'));
  try
    AssertTrue('snapshot arrives', A.Reader.WaitForText('event: reading', WAIT_MS));
    FServer.PublishAlert(['high'], FReadings[0]);
    AssertTrue('first alert arrives', A.Reader.WaitForText('"high"', WAIT_MS));
    S := A.Reader.Snapshot;
    // The id of the last frame A received
    P := 0;
    repeat
      E := Pos('id: ', S, P + 1);
      if E > 0 then
        P := E;
    until E = 0;
    AssertTrue('an id line was received', P > 0);
    E := Pos(#10, S, P);
    LastId := Copy(S, P + 4, E - P - 4);
  finally
    A.Free;
  end;

  FServer.PublishAlert(['low'], FReadings[0]);

  B := TSseClient.Create(Port, Get('/events', 'Last-Event-ID: ' + LastId + #13#10));
  try
    AssertTrue('missed alert is replayed', B.Reader.WaitForText('"low"', WAIT_MS));
    S := B.Reader.Snapshot;
    AssertEquals('no snapshot on resume', 0, CountOccurrences(S, 'event: reading'));
    AssertEquals('the already-seen alert is not replayed', 0, CountOccurrences(S, '"high"'));
  finally
    B.Free;
  end;

  // An id from before a server restart is unknown to this hub: the client
  // must get a snapshot rather than wait for the sequence to catch up.
  B := TSseClient.Create(Port, Get('/events', 'Last-Event-ID: 999999'#13#10));
  try
    AssertTrue('unknown id falls back to a snapshot', B.Reader.WaitForText('event: reading', WAIT_MS));
  finally
    B.Free;
  end;
end;

procedure TWebServerEventsTests.TestQueryTokenAuth;
var
  C: TSseClient;
begin
  StartServer('s3cret');
  C := TSseClient.Create(Port, Get('/events'));
  try
    AssertTrue('no token is refused', C.Reader.WaitForText('401', WAIT_MS));
  finally
    C.Free;
  end;
  C := TSseClient.Create(Port, Get('/events?token=wrong'));
  try
    AssertTrue('wrong token is refused', C.Reader.WaitForText('401', WAIT_MS));
  finally
    C.Free;
  end;
  C := TSseClient.Create(Port, Get('/events?token=s3cret'));
  try
    AssertTrue('query token opens the stream', C.Reader.WaitForText('text/event-stream', WAIT_MS));
  finally
    C.Free;
  end;
  C := TSseClient.Create(Port, Get('/glucose?token=s3cret'));
  try
    AssertTrue('query token works on plain endpoints too', C.Reader.WaitForText('200 OK', WAIT_MS));
  finally
    C.Free;
  end;
end;

procedure TWebServerEventsTests.TestStopClosesStream;
var
  C: TSseClient;
  T0: QWord;
begin
  StartServer;
  C := TSseClient.Create(Port, Get('/events'));
  try
    AssertTrue('stream is open', C.Reader.WaitForText('event: reading', WAIT_MS));
    T0 := GetTickCount64;
    FServer.Stop;
    AssertTrue('Stop drains the open stream promptly', GetTickCount64 - T0 < 3000);
    AssertTrue('subscriber sees EOF', C.Reader.WaitForEof(WAIT_MS));
  finally
    C.Free;
  end;
end;

procedure TWebServerEventsTests.TestEventStreamLimit;
var
  Clients: array of TSseClient;
  Extra: TSseClient;
  i, Attempt: integer;
  Reopened: boolean;
begin
  StartServer;
  SetLength(Clients, MAX_EVENT_STREAMS);
  for i := 0 to High(Clients) do
    Clients[i] := nil;
  Extra := nil;
  try
    for i := 0 to High(Clients) do
    begin
      Clients[i] := TSseClient.Create(Port, Get('/events'));
      AssertTrue('stream ' + IntToStr(i + 1) + ' opens',
        Clients[i].Reader.WaitForText('text/event-stream', WAIT_MS));
    end;

    Extra := TSseClient.Create(Port, Get('/events'));
    AssertTrue('one past the cap is refused', Extra.Reader.WaitForText('503', WAIT_MS));
    AssertTrue('refusal says why', Extra.Reader.WaitForText('Too many event streams', WAIT_MS));
    AssertTrue('refusal carries Retry-After', Pos('Retry-After', Extra.Reader.Snapshot) > 0);
    AssertTrue('refused connection is closed', Extra.Reader.WaitForEof(WAIT_MS));
    FreeAndNil(Extra);

    // Plain endpoints are not counted against the cap.
    Extra := TSseClient.Create(Port, Get('/health'));
    AssertTrue('plain request still served', Extra.Reader.WaitForText('200 OK', WAIT_MS));
    FreeAndNil(Extra);

    // A subscriber hanging up frees its slot. The handler notices the EOF on
    // its next poll, so allow a few refusals before the slot is back.
    FreeAndNil(Clients[0]);
    Reopened := false;
    for Attempt := 1 to 40 do
    begin
      Extra := TSseClient.Create(Port, Get('/events'));
      if Extra.Reader.WaitForText('text/event-stream', WAIT_MS) then
      begin
        Reopened := true;
        Break;
      end;
      AssertTrue('refused again while the slot is still held', Extra.Reader.WaitForText('503', WAIT_MS));
      FreeAndNil(Extra);
      Sleep(100);
    end;
    AssertTrue('slot is released when the subscriber hangs up', Reopened);
  finally
    Extra.Free;
    for i := 0 to High(Clients) do
      Clients[i].Free;
  end;
end;

procedure TWebServerEventsTests.TestHubReplayAndRingOverflow;
var
  Hub: TWebEventHub;
  Items: TWebEventArray;
  Seq: int64;
  i: integer;
begin
  Hub := TWebEventHub.Create;
  try
    AssertEquals('empty hub', int64(0), Hub.LatestSeq);
    AssertTrue('nothing to replay on an empty hub', Hub.CopySince(0, Items));
    AssertEquals(0, Length(Items));
    AssertFalse('an id the hub never issued needs a resync', Hub.CopySince(1, Items));

    AssertTrue(Hub.Publish('status', 'a'));
    AssertFalse('unchanged sticky is dropped', Hub.Publish('status', 'a'));
    AssertTrue('changed sticky goes through', Hub.Publish('status', 'b'));
    AssertTrue(Hub.Publish('alert', 'x', false, false));
    AssertTrue('non-sticky repeats', Hub.Publish('alert', 'x', false, false));
    AssertEquals(int64(4), Hub.LatestSeq);

    AssertTrue('the latest id has nothing new', Hub.CopySince(4, Items));
    AssertEquals(0, Length(Items));
    AssertFalse('an id past the latest needs a resync', Hub.CopySince(5, Items));

    AssertTrue(Hub.CopySince(2, Items));
    AssertEquals('two events after seq 2', 2, Length(Items));
    AssertEquals(int64(3), Items[0].Seq);
    AssertEquals('alert', Items[1].Name);

    Hub.CopySticky(Items, Seq);
    AssertEquals(int64(4), Seq);
    AssertEquals('only the sticky event is in the snapshot', 1, Length(Items));
    AssertEquals('b', Items[0].Data);
    AssertEquals('snapshot is stamped with the latest seq', int64(4), Items[0].Seq);

    for i := 1 to 300 do
      Hub.Publish('alert', IntToStr(i), false, false);
    AssertFalse('an id that fell out of the ring cannot be replayed', Hub.CopySince(2, Items));
    AssertTrue('a recent id can', Hub.CopySince(Hub.LatestSeq - 5, Items));
    AssertEquals(5, Length(Items));
    AssertEquals(IntToStr(300), Items[4].Data);
  finally
    Hub.Free;
  end;
end;

initialization
  RegisterTest(TWebServerEventsTests);

end.
