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
unit webserver_nightscout_tests;

{$mode objfpc}{$H+}

{
  Tests for the web API's Nightscout-compatible read endpoints
  (/api/v1/entries.json and aliases, /api/v1/status.json, /pebble, /sgv.json).
  A real TTrndiWebServer is started on a loopback port and driven with raw
  sockets (the client helpers come from webserver_events_tests), so the tests
  stay offline and need no LCL.

  One test goes the whole way round instead: it points Trndi's own Nightscout
  driver at the server, which is the claim the endpoints make. It does real
  HTTP through the native layer, so it is skipped with TRNDI_NO_TESTSERVER=1
  like the other driver tests.
}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry, fpjson, jsonparser, sha1,
  trndi.types, trndi.api, trndi.api.nightscout, trndi.webserver.threaded,
  webserver_events_tests;

type
  TWebServerNightscoutTests = class(TTestCase)
  private
    FServer: TTrndiWebServer;
    FReadings: BGResults;
    FUnit: string;            // what the settings.get callback reports
    FRangeDisabled: boolean;  // report the in-range band as null
    function GetReadings: BGResults;
    function GetPredictions: BGResults;
    function Command(const ACommand: string; const AParams: TJSONObject;
      AReply: TJSONObject; out AError: string): boolean;
    function Port: word;
    procedure StartServer(const AToken: string = ''; AWithCommand: boolean = true);
    {** Send one request and return the whole response once the server closes. }
    function Exchange(const ARequest: string): string;
    {** GET APath and parse the body; fails unless the answer is 200. The
        caller owns the result. }
    function GetJSONFrom(const APath: string; const AExtra: string = ''): TJSONData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEntries;
    procedure TestEntriesCount;
    procedure TestEntryAliases;
    procedure TestEmptyReadingsLeftOut;
    procedure TestNoReadingsIsEmptyArray;
    procedure TestStatus;
    procedure TestStatusDisabledRange;
    procedure TestStatusWithoutCommand;
    procedure TestPebble;
    procedure TestEmptyLists;
    procedure TestAuth;
    procedure TestOwnStatusEndpointKept;
    procedure TestMethodNotAllowed;
    procedure TestHealthListsEndpoints;
    procedure TestNightscoutDriverRoundTrip;
  end;

implementation

const
  TEST_PORT_BASE = 18670;
  WAIT_MS = 4000;

var
  PortCounter: integer = 0;

function Get(const Path: string; const Extra: string = ''): string;
begin
  Result := 'GET ' + Path + ' HTTP/1.1'#13#10 +
    'Host: localhost'#13#10 + Extra + #13#10;
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

function BodyOf(const Response: string): string;
var
  P: integer;
begin
  P := Pos(#13#10#13#10, Response);
  if P > 0 then
    Result := Copy(Response, P + 4, MaxInt)
  else
    Result := '';
end;

function EpochMsOf(const ALocalTime: TDateTime): int64;
begin
  Result := int64(DateTimeToUnix(LocalTimeToUniversal(ALocalTime))) * 1000;
end;

{ TWebServerNightscoutTests }

function TWebServerNightscoutTests.GetReadings: BGResults;
begin
  Result := Copy(FReadings);
end;

function TWebServerNightscoutTests.GetPredictions: BGResults;
begin
  Result := nil;
end;

function TWebServerNightscoutTests.Command(const ACommand: string;
  const AParams: TJSONObject; AReply: TJSONObject; out AError: string): boolean;
var
  th: TJSONObject;
begin
  AError := '';
  Result := ACommand = 'settings.get';
  if not Result then
    Exit;
  // The shape TfBG.WebSettingsToJSON replies with, limits in mg/dL.
  AReply.Add('unit', FUnit);
  th := TJSONObject.Create;
  th.Add('lo', 70);
  th.Add('hi', 180);
  if FRangeDisabled then
  begin
    th.Add('range_lo', TJSONNull.Create);
    th.Add('range_hi', TJSONNull.Create);
  end
  else
  begin
    th.Add('range_lo', 80);
    th.Add('range_hi', 140);
  end;
  AReply.Add('thresholds', th);
end;

function TWebServerNightscoutTests.Port: word;
begin
  Result := TEST_PORT_BASE + PortCounter;
end;

procedure TWebServerNightscoutTests.StartServer(const AToken: string; AWithCommand: boolean);
begin
  Inc(PortCounter);
  if AWithCommand then
    FServer := TTrndiWebServer.Create(Port, AToken, @GetReadings, @GetPredictions, true, @Command)
  else
    FServer := TTrndiWebServer.Create(Port, AToken, @GetReadings, @GetPredictions, true);
  FServer.Start;
end;

function TWebServerNightscoutTests.Exchange(const ARequest: string): string;
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

function TWebServerNightscoutTests.GetJSONFrom(const APath, AExtra: string): TJSONData;
var
  S, Body: string;
begin
  S := Exchange(Get(APath, AExtra));
  AssertEquals(APath + ' status', 'HTTP/1.1 200 OK', StatusLine(S));
  AssertTrue(APath + ' is json', Pos('Content-Type: application/json', S) > 0);
  Body := BodyOf(S);
  AssertTrue(APath + ' announces its length',
    Pos('Content-Length: ' + IntToStr(Length(Body)) + #13#10, S) > 0);
  Result := GetJSON(Body);
end;

procedure TWebServerNightscoutTests.SetUp;
var
  Base: TDateTime;
begin
  inherited SetUp;
  FServer := nil;
  FUnit := 'mmol';
  FRangeDisabled := false;
  // Newest first, five minutes apart, the order the app caches them in.
  Base := EncodeDate(2026, 9, 29) + EncodeTime(10, 10, 0, 0);
  SetLength(FReadings, 3);
  FReadings[0].Init(mgdl);
  FReadings[0].update(120, 4, mgdl);
  FReadings[0].trend := TdFortyFiveUp;
  FReadings[0].date := Base;
  FReadings[1].Init(mgdl);
  FReadings[1].update(116, -2, mgdl);
  FReadings[1].trend := TdFlat;
  FReadings[1].date := IncMinute(Base, -5);
  FReadings[2].Init(mgdl);
  FReadings[2].update(118, 0, mgdl);
  FReadings[2].trend := TdSingleDown;
  FReadings[2].date := IncMinute(Base, -10);
end;

procedure TWebServerNightscoutTests.TearDown;
begin
  FreeAndNil(FServer);
  inherited TearDown;
end;

procedure TWebServerNightscoutTests.TestEntries;
var
  D: TJSONData;
  E: TJSONObject;
begin
  StartServer;
  D := GetJSONFrom('/api/v1/entries.json');
  try
    AssertTrue('an array', D is TJSONArray);
    AssertEquals('all three readings', 3, D.Count);

    E := TJSONArray(D).Objects[0];
    AssertEquals('sgv in mg/dL', 120, E.Get('sgv', 0));
    AssertEquals('delta in mg/dL', 4, E.Get('delta', 0));
    AssertEquals('direction', 'FortyFiveUp', E.Get('direction', ''));
    AssertEquals('type', 'sgv', E.Get('type', ''));
    AssertEquals('date is epoch ms', EpochMsOf(FReadings[0].date), E.Get('date', int64(0)));
    AssertEquals('dateString is the same instant, in UTC',
      FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss".000Z"',
      LocalTimeToUniversal(FReadings[0].date)), E.Get('dateString', ''));
    AssertEquals('an id of 24 hex digits', 24, Length(E.Get('_id', '')));

    E := TJSONArray(D).Objects[1];
    AssertEquals('second sgv', 116, E.Get('sgv', 0));
    AssertEquals('negative delta', -2, E.Get('delta', 0));
    AssertEquals('second direction', 'Flat', E.Get('direction', ''));
    AssertTrue('ids differ between entries',
      E.Get('_id', '') <> TJSONArray(D).Objects[0].Get('_id', ''));

    AssertEquals('third direction', 'SingleDown',
      TJSONArray(D).Objects[2].Get('direction', ''));
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestEntriesCount;
var
  D: TJSONData;
  i: integer;
begin
  StartServer;
  D := GetJSONFrom('/api/v1/entries.json?count=2');
  try
    AssertEquals('count limits', 2, D.Count);
    AssertEquals('newest kept', 120, TJSONArray(D).Objects[0].Get('sgv', 0));
  finally
    D.Free;
  end;

  // A bad count falls back to Nightscout's default of ten.
  SetLength(FReadings, 12);
  for i := 3 to 11 do
  begin
    FReadings[i].Init(mgdl);
    FReadings[i].update(100 + i, 0, mgdl);
    FReadings[i].date := IncMinute(FReadings[0].date, -5 * i);
  end;
  D := GetJSONFrom('/api/v1/entries.json?count=abc');
  try
    AssertEquals('default count', 10, D.Count);
  finally
    D.Free;
  end;
  D := GetJSONFrom('/api/v1/entries.json?count=50');
  try
    AssertEquals('no more than there is', 12, D.Count);
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestEntryAliases;
const
  // The doubled slash is what a client joining "…/api/v1/" and a path sends.
  PATHS: array[0..4] of string = ('/api/v1/entries', '/api/v1/entries/sgv.json',
    '/api/v1/entries/sgv', '/sgv.json', '/api/v1//entries/sgv.json');
var
  D: TJSONData;
  i: integer;
begin
  StartServer;
  for i := Low(PATHS) to High(PATHS) do
  begin
    D := GetJSONFrom(PATHS[i] + '?count=3');
    try
      AssertEquals(PATHS[i] + ' serves the entries', 3, D.Count);
    finally
      D.Free;
    end;
  end;

  D := GetJSONFrom('/api/v1/entries/current.json');
  try
    AssertEquals('current is one entry', 1, D.Count);
    AssertEquals('the newest', 120, TJSONArray(D).Objects[0].Get('sgv', 0));
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestEmptyReadingsLeftOut;
var
  D: TJSONData;
begin
  FReadings[0].Clear;
  StartServer;
  D := GetJSONFrom('/api/v1/entries.json');
  try
    AssertEquals('the valueless reading is gone', 2, D.Count);
    AssertEquals('the next one leads', 116, TJSONArray(D).Objects[0].Get('sgv', 0));
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestNoReadingsIsEmptyArray;
var
  D: TJSONData;
begin
  FReadings := nil;
  StartServer;
  D := GetJSONFrom('/api/v1/entries.json');
  try
    AssertTrue('still an array', D is TJSONArray);
    AssertEquals('with nothing in it', 0, D.Count);
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestStatus;
var
  D: TJSONData;
  O, Th: TJSONObject;
  Epoch: int64;
begin
  StartServer;
  D := GetJSONFrom('/api/v1/status.json');
  try
    O := D as TJSONObject;
    AssertEquals('status', 'ok', O.Get('status', ''));
    Epoch := O.Get('serverTimeEpoch', int64(0));
    AssertTrue('serverTimeEpoch is now, in ms',
      Abs(Epoch - EpochMsOf(Now)) < 10000);
    AssertEquals('units', 'mmol', O.Objects['settings'].Get('units', ''));
    Th := O.Objects['settings'].Objects['thresholds'];
    AssertEquals('bgHigh', 180, Th.Get('bgHigh', 0));
    AssertEquals('bgTargetTop', 140, Th.Get('bgTargetTop', 0));
    AssertEquals('bgTargetBottom', 80, Th.Get('bgTargetBottom', 0));
    AssertEquals('bgLow', 70, Th.Get('bgLow', 0));
  finally
    D.Free;
  end;

  FUnit := 'mgdl';
  D := GetJSONFrom('/status.json');
  try
    AssertEquals('xDrip spelling, Nightscout unit name', 'mg/dl',
      TJSONObject(D).Objects['settings'].Get('units', ''));
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestStatusDisabledRange;
var
  D: TJSONData;
  Th: TJSONObject;
begin
  FRangeDisabled := true;
  StartServer;
  D := GetJSONFrom('/api/v1/status.json');
  try
    Th := TJSONObject(D).Objects['settings'].Objects['thresholds'];
    AssertEquals('target top falls back to the high limit', 180, Th.Get('bgTargetTop', 0));
    AssertEquals('target bottom falls back to the low limit', 70, Th.Get('bgTargetBottom', 0));
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestStatusWithoutCommand;
var
  D: TJSONData;
  O: TJSONObject;
begin
  StartServer('', false);
  D := GetJSONFrom('/api/v1/status.json');
  try
    O := D as TJSONObject;
    AssertTrue('time is still served', O.Get('serverTimeEpoch', int64(0)) > 0);
    AssertEquals('unit defaults to mg/dl', 'mg/dl', O.Objects['settings'].Get('units', ''));
    AssertTrue('no thresholds to report', O.Objects['settings'].Find('thresholds') = nil);
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestPebble;
var
  D: TJSONData;
  O, Bg: TJSONObject;
begin
  StartServer;
  // No unit named: the app's own (mmol here).
  D := GetJSONFrom('/pebble');
  try
    O := D as TJSONObject;
    AssertTrue('now is served',
      Abs(O.Arrays['status'].Objects[0].Get('now', int64(0)) - EpochMsOf(Now)) < 10000);
    AssertEquals('one reading by default', 1, O.Arrays['bgs'].Count);
    Bg := O.Arrays['bgs'].Objects[0];
    AssertEquals('sgv as an mmol string', '6.7', Bg.Get('sgv', ''));
    AssertEquals('delta as an mmol string', '0.2', Bg.Get('bgdelta', ''));
    AssertEquals('Nightscout trend number', 3, Bg.Get('trend', 0));
    AssertEquals('direction', 'FortyFiveUp', Bg.Get('direction', ''));
    AssertEquals('datetime', EpochMsOf(FReadings[0].date), Bg.Get('datetime', int64(0)));
  finally
    D.Free;
  end;

  D := GetJSONFrom('/pebble?units=mgdl&count=2');
  try
    O := D as TJSONObject;
    AssertEquals('count', 2, O.Arrays['bgs'].Count);
    AssertEquals('sgv as an mg/dL string', '120', O.Arrays['bgs'].Objects[0].Get('sgv', ''));
    AssertEquals('negative delta', '-2', O.Arrays['bgs'].Objects[1].Get('bgdelta', ''));
    AssertEquals('flat is 4', 4, O.Arrays['bgs'].Objects[1].Get('trend', 0));
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestEmptyLists;
var
  D: TJSONData;
begin
  StartServer;
  D := GetJSONFrom('/api/v1/devicestatus.json?count=1');
  try
    AssertTrue('devicestatus is an array', D is TJSONArray);
    AssertEquals('an empty one', 0, D.Count);
  finally
    D.Free;
  end;
  D := GetJSONFrom('/api/v1/treatments.json');
  try
    AssertEquals('treatments too', 0, D.Count);
  finally
    D.Free;
  end;
end;

procedure TWebServerNightscoutTests.TestAuth;
var
  S, Hash: string;
begin
  StartServer('secret');
  Hash := SHA1Print(SHA1String('secret'));

  S := Exchange(Get('/api/v1/entries.json'));
  AssertEquals('no credentials', 'HTTP/1.1 401 Unauthorized', StatusLine(S));
  AssertTrue('no data in the refusal', Pos('"sgv"', S) = 0);

  S := Exchange(Get('/api/v1/entries.json', 'api-secret: ' + Hash + #13#10));
  AssertEquals('api-secret hash', 'HTTP/1.1 200 OK', StatusLine(S));
  S := Exchange(Get('/api/v1/entries.json', 'API-SECRET: ' + UpperCase(Hash) + #13#10));
  AssertEquals('header and hash in any case', 'HTTP/1.1 200 OK', StatusLine(S));
  S := Exchange(Get('/api/v1/entries.json', 'api-secret: secret'#13#10));
  AssertEquals('the plain secret is not a hash', 'HTTP/1.1 401 Unauthorized', StatusLine(S));
  S := Exchange(Get('/api/v1/entries.json',
    'api-secret: ' + SHA1Print(SHA1String('other')) + #13#10));
  AssertEquals('wrong hash', 'HTTP/1.1 401 Unauthorized', StatusLine(S));

  S := Exchange(Get('/api/v1/entries.json?count=1&token=secret'));
  AssertEquals('query token', 'HTTP/1.1 200 OK', StatusLine(S));
  S := Exchange(Get('/api/v1/entries.json?token=nope'));
  AssertEquals('wrong query token', 'HTTP/1.1 401 Unauthorized', StatusLine(S));

  S := Exchange(Get('/pebble', 'Authorization: Bearer secret'#13#10));
  AssertEquals('bearer still works', 'HTTP/1.1 200 OK', StatusLine(S));
  S := Exchange(Get('/api/v1/status.json'));
  AssertEquals('status is protected too', 'HTTP/1.1 401 Unauthorized', StatusLine(S));

  // The Nightscout forms are for the Nightscout endpoints only.
  S := Exchange(Get('/glucose?token=secret'));
  AssertEquals('/glucose ignores a query token', 'HTTP/1.1 401 Unauthorized', StatusLine(S));
  S := Exchange(Get('/glucose', 'api-secret: ' + Hash + #13#10));
  AssertEquals('/glucose ignores api-secret', 'HTTP/1.1 401 Unauthorized', StatusLine(S));
end;

procedure TWebServerNightscoutTests.TestOwnStatusEndpointKept;
var
  S: string;
begin
  StartServer;
  S := Exchange(Get('/status'));
  AssertEquals('status', 'HTTP/1.1 200 OK', StatusLine(S));
  AssertTrue('the server''s own document', Pos('"data_available"', S) > 0);
  AssertTrue('not the Nightscout one', Pos('serverTimeEpoch', S) = 0);
end;

procedure TWebServerNightscoutTests.TestMethodNotAllowed;
var
  S: string;
begin
  StartServer;
  S := Exchange('POST /api/v1/entries.json HTTP/1.1'#13#10 +
    'Host: localhost'#13#10 +
    'Content-Type: application/json'#13#10 +
    'Content-Length: 2'#13#10#13#10 + '[]');
  AssertEquals('read-only', 'HTTP/1.1 405 Method Not Allowed', StatusLine(S));
end;

procedure TWebServerNightscoutTests.TestHealthListsEndpoints;
var
  S: string;
begin
  StartServer;
  S := Exchange(Get('/health'));
  AssertTrue('lists entries', Pos('entries.json"', S) > 0);
  AssertTrue('lists pebble', Pos('pebble"', S) > 0);
end;

procedure TWebServerNightscoutTests.TestNightscoutDriverRoundTrip;
var
  api: NightScout;
  Got: BGResults;
  res: string;
  i: integer;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestNightscoutDriverRoundTrip: HTTP driver tests disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  // Recent readings, as a live server would hold. No command callback: the
  // driver blocks this (the main) thread while it waits, so nothing could
  // serve a synchronized settings.get.
  for i := 0 to High(FReadings) do
    FReadings[i].date := IncMinute(Now, -5 * i);
  StartServer('secret', false);

  api := NightScout.create('http://127.0.0.1:' + IntToStr(Port), 'nope');
  try
    AssertFalse('a wrong secret does not connect', api.connect);
  finally
    api.Free;
  end;

  api := NightScout.create('http://127.0.0.1:' + IntToStr(Port), 'secret');
  try
    AssertTrue('driver connects: ' + api.errormsg, api.connect);
    Got := api.getReadings(0, 3, '', res, false);
    AssertEquals('all readings came through', 3, Length(Got));
    for i := 0 to 2 do
    begin
      AssertEquals('value ' + IntToStr(i), FReadings[i].val, Got[i].val, 0.01);
      AssertEquals('delta ' + IntToStr(i), FReadings[i].delta, Got[i].delta, 0.01);
      AssertTrue('trend ' + IntToStr(i), FReadings[i].trend = Got[i].trend);
      AssertTrue('time ' + IntToStr(i),
        Abs(SecondsBetween(FReadings[i].date, Got[i].date)) <= 2);
    end;
  finally
    api.Free;
  end;
end;

initialization
  RegisterTest(TWebServerNightscoutTests);

end.
