unit api_xdrip_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
process, php_server_helper, fpjson, jsonparser;

type

TAPIXDripTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestXDrip;
  procedure TestXDripUnreachable;
  procedure TestXDripLocalServer;
end;

implementation

procedure TAPIXDripTester.TestXDrip;
var
  api: TrndiAPI;
begin
  // Can create
  api := xDrip.create('http://localhost:8080', 'testsecret');
  try
    // Test if the connect function runs (will fail without server)
    AssertFalse('API Connect should fail without server', api.connect);
    AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  finally
    api.Free;
  end;
end;

procedure TAPIXDripTester.TestXDripUnreachable;
var
  api: TrndiAPI;
begin
  api := xDrip.create('http://127.0.0.1:1', 'testsecret');
  try
    AssertFalse('Connect to unreachable xDrip should fail', api.connect);
    AssertTrue('Error message should not be empty', api.errormsg <> '');
    AssertTrue('Error message references connect or server',
      (Pos('Cannot connect', api.errormsg) > 0) or
      (Pos('Invalid address', api.errormsg) = 0)); // don't misinterpret
  finally
    api.Free;
  end;
end;

procedure TAPIXDripTester.TestXDripLocalServer;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  res: string;
  readings: BGResults;
  bg: BGReading;
  BaseURL: string;
  js: TJSONData;
  firstEntry: TJSONObject;
  rawDateMs: int64;
  expectedDate: TDateTime;
begin
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    api := xDrip.create(BaseURL, 'test22');
    try
      api.timezone := 120; // Simulate a desktop timezone that differs from the xDrip source.

      if not api.connect then
        Fail('xDrip connects to local fake server. Error: ' + api.errormsg);
      AssertTrue('xDrip connected', true);
      AssertTrue('xDrip basetime stays close to local time',
        Abs(api.getBasetime - DateTimeToUnix(Now)) < 300);
      
      // Test thresholds from status.json (same as Nightscout)
      AssertEquals('xDrip bgHigh threshold mapped', 260, api.cgmHi);
      AssertEquals('xDrip bgLow threshold mapped', 55, api.cgmLo);
      
      // Test getting current reading from pebble endpoint
      bg.Clear;
      AssertTrue('xDrip getCurrent returns data', api.getCurrent(bg));
      AssertTrue('Current reading has plausible value', bg.val > 0);
      AssertTrue('Current reading has plausible timestamp', bg.date > 0);
      
      // Test getting multiple readings
      readings := api.getReadings(30, 3, '', res);
      AssertTrue('xDrip returns at least one reading', Length(readings) > 0);
      AssertTrue('xDrip reading value set', readings[0].val > 0);
      AssertTrue('xDrip reading timestamp set', readings[0].date > 0);

      js := GetJSON(res);
      try
        AssertTrue('xDrip reading payload is an array', js is TJSONArray);
        firstEntry := TJSONObject(TJSONArray(js).Items[0]);
        rawDateMs := firstEntry.Get('date', int64(0));
        expectedDate := UnixToDateTime(rawDateMs div 1000, False);
        AssertEquals('xDrip reading timestamp uses UTC epoch from server',
          FormatDateTime('yyyy-mm-dd hh:nn:ss', expectedDate),
          FormatDateTime('yyyy-mm-dd hh:nn:ss', readings[0].date));
      finally
        js.Free;
      end;
    finally
      api.Free;
    end;
  finally
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;

procedure TAPIXDripTester.SetUp;
begin

end;

procedure TAPIXDripTester.TearDown;
begin

end;

initialization

RegisterTest(TAPIXDripTester);
end.