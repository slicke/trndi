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
unit webserver_dashboard_tests;

{$mode objfpc}{$H+}

{
  Tests for the web API's dashboard page (GET /) and the command-backed
  /settings and /snooze endpoints. A real TTrndiWebServer is started on a
  loopback port and driven with raw sockets (the client helpers come from
  webserver_events_tests), so the tests stay offline and need no LCL.

  The command callback is run on the main thread through TThread.Synchronize;
  the client helpers' wait loops call CheckSynchronize, which is what lets it
  run while a test blocks waiting for the response.
}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Sockets, fpjson,
  trndi.types, trndi.webserver.threaded, webserver_events_tests;

type
  TWebServerDashboardTests = class(TTestCase)
  private
    FServer: TTrndiWebServer;
    FReadings: BGResults;
    FCommands: TStringList;   // one "name|params-json" per command served
    FRefuseWith: string;      // when set, the callback refuses with this text
    function GetReadings: BGResults;
    function GetPredictions: BGResults;
    function Command(const ACommand: string; const AParams: TJSONObject;
      AReply: TJSONObject; out AError: string): boolean;
    function Port: word;
    procedure StartServer(const AToken: string = ''; AWithCommand: boolean = true);
    {** Send one request and return the whole response once the server closes. }
    function Exchange(const ARequest: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDashboardServed;
    procedure TestDashboardNeedsNoToken;
    procedure TestHealthListsNewEndpoints;
    procedure TestSettingsWithoutCommand;
    procedure TestSettingsGet;
    procedure TestSettingsPost;
    procedure TestBodySplitAcrossPackets;
    procedure TestSnoozePost;
    procedure TestCommandRefused;
    procedure TestBadJsonBody;
    procedure TestSettingsNeedsToken;
    procedure TestMethodNotAllowed;
  end;

implementation

const
  TEST_PORT_BASE = 18570;
  WAIT_MS = 4000;

var
  PortCounter: integer = 0;

function Get(const Path: string; const Extra: string = ''): string;
begin
  Result := 'GET ' + Path + ' HTTP/1.1'#13#10 +
    'Host: localhost'#13#10 + Extra + #13#10;
end;

function Post(const Path, Body: string; const Extra: string = ''): string;
begin
  Result := 'POST ' + Path + ' HTTP/1.1'#13#10 +
    'Host: localhost'#13#10 +
    'Content-Type: application/json'#13#10 +
    'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
    Extra + #13#10 + Body;
end;

function StatusLine(const Response: string): string;
var
  P: integer;
begin
  P := Pos(#13#10, Response);
  if P > 0 then
    Result := Copy(Response, 1, P - 1)
  else
    Result := Response;
end;

{ TWebServerDashboardTests }

function TWebServerDashboardTests.GetReadings: BGResults;
begin
  Result := Copy(FReadings);
end;

function TWebServerDashboardTests.GetPredictions: BGResults;
begin
  Result := nil;
end;

function TWebServerDashboardTests.Command(const ACommand: string;
  const AParams: TJSONObject; AReply: TJSONObject; out AError: string): boolean;
begin
  if AParams = nil then
    FCommands.Add(ACommand + '|')
  else
    FCommands.Add(ACommand + '|' + AParams.AsJSON);
  AError := FRefuseWith;
  Result := FRefuseWith = '';
  if Result then
  begin
    AReply.Add('served', ACommand);
    AReply.Add('unit', 'mmol');
  end;
end;

function TWebServerDashboardTests.Port: word;
begin
  Result := TEST_PORT_BASE + PortCounter;
end;

procedure TWebServerDashboardTests.StartServer(const AToken: string; AWithCommand: boolean);
begin
  Inc(PortCounter);
  if AWithCommand then
    FServer := TTrndiWebServer.Create(Port, AToken, @GetReadings, @GetPredictions, true, @Command)
  else
    FServer := TTrndiWebServer.Create(Port, AToken, @GetReadings, @GetPredictions, true);
  FServer.Start;
end;

function TWebServerDashboardTests.Exchange(const ARequest: string): string;
var
  C: TSseClient;
begin
  C := TSseClient.Create(Port, ARequest);
  try
    AssertTrue('server answers and closes', C.Reader.WaitForEof(WAIT_MS));
    Result := C.Reader.Snapshot;
  finally
    C.Free;
  end;
end;

procedure TWebServerDashboardTests.SetUp;
begin
  inherited SetUp;
  FServer := nil;
  FCommands := TStringList.Create;
  FRefuseWith := '';
  SetLength(FReadings, 1);
  FReadings[0].Init(mgdl);
  FReadings[0].update(120, 4, mgdl);
  FReadings[0].date := EncodeDate(2026, 9, 29) + EncodeTime(10, 0, 0, 0);
end;

procedure TWebServerDashboardTests.TearDown;
begin
  FreeAndNil(FServer);
  FreeAndNil(FCommands);
  inherited TearDown;
end;

procedure TWebServerDashboardTests.TestDashboardServed;
var
  S: string;
begin
  StartServer;
  S := Exchange(Get('/'));
  AssertEquals('status', 'HTTP/1.1 200 OK', StatusLine(S));
  AssertTrue('html content type', Pos('Content-Type: text/html', S) > 0);
  AssertTrue('the page', Pos('<title>Trndi</title>', S) > 0);
  AssertTrue('the page talks to /events', Pos('new EventSource("/events"', S) > 0);
  AssertTrue('content length announced', Pos('Content-Length: ', S) > 0);
  S := Exchange(Get('/dashboard'));
  AssertEquals('alias status', 'HTTP/1.1 200 OK', StatusLine(S));
end;

procedure TWebServerDashboardTests.TestDashboardNeedsNoToken;
var
  S: string;
begin
  StartServer('secret');
  S := Exchange(Get('/'));
  AssertEquals('page is public', 'HTTP/1.1 200 OK', StatusLine(S));
  S := Exchange(Get('/glucose'));
  AssertEquals('data is not', 'HTTP/1.1 401 Unauthorized', StatusLine(S));
end;

procedure TWebServerDashboardTests.TestHealthListsNewEndpoints;
var
  S: string;
begin
  StartServer;
  S := Exchange(Get('/health'));
  AssertTrue('lists /settings', Pos('"/settings"', S) > 0);
  AssertTrue('lists /snooze', Pos('"/snooze"', S) > 0);
  AssertTrue('lists the page', Pos('"/"', S) > 0);
  AssertTrue('reports command support', Pos('"command_support" : true', S) > 0);
end;

procedure TWebServerDashboardTests.TestSettingsWithoutCommand;
var
  S: string;
begin
  StartServer('', false);
  S := Exchange(Get('/settings'));
  AssertEquals('status', 'HTTP/1.1 501 Not Implemented', StatusLine(S));
  S := Exchange(Post('/snooze', '{"minutes":30}'));
  AssertEquals('snooze status', 'HTTP/1.1 501 Not Implemented', StatusLine(S));
  S := Exchange(Get('/health'));
  AssertTrue('health says so', Pos('"command_support" : false', S) > 0);
end;

procedure TWebServerDashboardTests.TestSettingsGet;
var
  S: string;
begin
  StartServer;
  S := Exchange(Get('/settings'));
  AssertEquals('status', 'HTTP/1.1 200 OK', StatusLine(S));
  AssertEquals('one command served', 1, FCommands.Count);
  AssertEquals('settings.get with no params', 'settings.get|', FCommands[0]);
  AssertTrue('the callback reply is the body', Pos('"served" : "settings.get"', S) > 0);
  AssertTrue('loopback peer may write', Pos('"writable" : true', S) > 0);
end;

procedure TWebServerDashboardTests.TestSettingsPost;
var
  S: string;
begin
  StartServer;
  S := Exchange(Post('/settings', '{"unit":"mgdl","override":{"enabled":true,"lo":70}}'));
  AssertEquals('status', 'HTTP/1.1 200 OK', StatusLine(S));
  AssertEquals('one command served', 1, FCommands.Count);
  AssertTrue('settings.set', Pos('settings.set|', FCommands[0]) = 1);
  AssertTrue('params reached the callback', Pos('"lo" : 70', FCommands[0]) > 0);
  AssertTrue('reply', Pos('"served" : "settings.set"', S) > 0);
end;

procedure TWebServerDashboardTests.TestBodySplitAcrossPackets;
var
  Req: string;
  Sock: TSocket;
  Addr: TInetSockAddr;
  Buf: array[0..4095] of byte;
  N, Cut: integer;
  Response: string;
begin
  StartServer;
  Req := Post('/snooze', '{"minutes":45}');
  // Send the headers plus the first byte of the body, then the rest a
  // moment later: the server must wait for Content-Length bytes.
  Cut := Pos(#13#10#13#10, Req) + 4;
  Sock := fpSocket(AF_INET, SOCK_STREAM, 0);
  AssertTrue('socket', Sock >= 0);
  try
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(Port);
    Addr.sin_addr.s_addr := htonl($7F000001);
    AssertEquals('connect', 0, fpConnect(Sock, @Addr, SizeOf(Addr)));
    AssertEquals('send headers', Cut, fpSend(Sock, @Req[1], Cut, 0));
    Sleep(150);
    AssertEquals('send body', Length(Req) - Cut, fpSend(Sock, @Req[Cut + 1], Length(Req) - Cut, 0));
    Response := '';
    repeat
      // Service Synchronize while the handler waits on the main thread.
      CheckSynchronize(20);
      N := fpRecv(Sock, @Buf, SizeOf(Buf), 0);
      if N > 0 then
        Response := Response + Copy(PChar(@Buf), 1, N);
    until N <= 0;
  finally
    CloseSocket(Sock);
  end;
  AssertEquals('status', 'HTTP/1.1 200 OK', StatusLine(Response));
  AssertEquals('one command served', 1, FCommands.Count);
  AssertTrue('whole body parsed', Pos('"minutes" : 45', FCommands[0]) > 0);
end;

procedure TWebServerDashboardTests.TestSnoozePost;
var
  S: string;
begin
  StartServer;
  S := Exchange(Post('/snooze', '{"minutes":30}'));
  AssertEquals('status', 'HTTP/1.1 200 OK', StatusLine(S));
  AssertEquals('one command served', 1, FCommands.Count);
  AssertTrue('snooze with minutes', Pos('snooze|{ "minutes" : 30 }', FCommands[0]) = 1);
end;

procedure TWebServerDashboardTests.TestCommandRefused;
var
  S: string;
begin
  StartServer;
  FRefuseWith := 'no thanks';
  S := Exchange(Post('/settings', '{"unit":"lbs"}'));
  AssertEquals('status', 'HTTP/1.1 400 Bad Request', StatusLine(S));
  AssertTrue('the refusal is the error', Pos('"error" : "no thanks"', S) > 0);
  AssertTrue('nothing else leaks into the body', Pos('"served"', S) = 0);
end;

procedure TWebServerDashboardTests.TestBadJsonBody;
var
  S: string;
begin
  StartServer;
  S := Exchange(Post('/settings', '{"unit":'));
  AssertEquals('malformed', 'HTTP/1.1 400 Bad Request', StatusLine(S));
  S := Exchange(Post('/settings', '[1,2]'));
  AssertEquals('not an object', 'HTTP/1.1 400 Bad Request', StatusLine(S));
  AssertEquals('the callback never ran', 0, FCommands.Count);
end;

procedure TWebServerDashboardTests.TestSettingsNeedsToken;
var
  S: string;
begin
  StartServer('secret');
  S := Exchange(Get('/settings'));
  AssertEquals('no header', 'HTTP/1.1 401 Unauthorized', StatusLine(S));
  S := Exchange(Get('/settings', 'Authorization: Bearer secret'#13#10));
  AssertEquals('with header', 'HTTP/1.1 200 OK', StatusLine(S));
  S := Exchange(Post('/snooze', '{"minutes":0}', 'Authorization: Bearer secret'#13#10));
  AssertEquals('write with header', 'HTTP/1.1 200 OK', StatusLine(S));
  AssertEquals('two commands served', 2, FCommands.Count);
end;

procedure TWebServerDashboardTests.TestMethodNotAllowed;
var
  S: string;
begin
  StartServer;
  S := Exchange(Get('/snooze'));
  AssertEquals('GET /snooze', 'HTTP/1.1 405 Method Not Allowed', StatusLine(S));
  AssertEquals('nothing served', 0, FCommands.Count);
end;

initialization
  RegisterTest(TWebServerDashboardTests);

end.
