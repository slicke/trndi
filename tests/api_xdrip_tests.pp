unit api_xdrip_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
process, php_server_helper;

type

TAPIXDripTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestXDrip;
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

procedure TAPIXDripTester.TestXDripLocalServer;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  res: string;
  readings: BGResults;
  bg: BGReading;
  BaseURL: string;
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
      if not api.connect then
        Fail('xDrip connects to local fake server. Error: ' + api.errormsg);
      AssertTrue('xDrip connected', true);
      
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