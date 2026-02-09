unit api_nightscout_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
process, php_server_helper;

type

TAPINightscoutTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestNightscoutInvalidUrl;
  procedure TestNightscoutUnauthorized;
  procedure TestNightscoutGetReadingsRespectsMax;
  procedure TestNightScout;
end;

implementation

procedure TAPINightscoutTester.TestNightscoutInvalidUrl;
var
  api: NightScout;
begin
  api := NightScout.create('not-a-url', 'x');
  try
    AssertFalse('Invalid URL should not connect', api.connect);
  finally
    api.Free;
  end;
end;

procedure TAPINightscoutTester.TestNightscoutUnauthorized;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
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
    // Wrong secret should yield Unauthorized from the fake server
    api := NightScout.create(BaseURL, 'wrongsecret');
    AssertFalse('Connect should fail for unauthorized secret', api.connect);
    api.Free;
  finally
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;

procedure TAPINightscoutTester.TestNightscoutGetReadingsRespectsMax;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  res: string;
  readings: BGResults;
  BaseURL: string;
begin
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    api := NightScout.create(BaseURL, 'test22');
    try
      AssertTrue('Connect to local NS', api.connect);

      readings := api.getReadings(0, 3, '', res);
      AssertTrue('Should not exceed requested max count', Length(readings) <= 3);
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

procedure TAPINightscoutTester.TestNightscout;
var
  api: TrndiAPI;
  bg: BGreading;
  PHPProcess: TProcess;
  BaseURL: string;
begin
  bg.Clear;
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    // Your existing test code
    api := NightScout.create('test','test');
    try
      AssertFalse('API Connect Fail', api.connect);
      AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
    finally
      api.Free;
    end;

    api := NightScout.create(BaseURL,'test22');
    try
      asserttrue('Connect to local NS', api.connect);
      // Thresholds are provided by the fake server in status.json
      AssertEquals('NS bgHigh threshold mapped', 260, api.cgmHi);
      AssertEquals('NS bgLow threshold mapped', 55, api.cgmLo);
      AssertEquals('NS bgTargetTop threshold mapped', 180, api.cgmRangeHi);
      AssertEquals('NS bgTargetBottom threshold mapped', 80, api.cgmRangeLo);
      asserttrue('Get readings', api.getCurrent(bg));
      AssertTrue('Current reading has plausible value', (bg.val > 0));
      AssertTrue('Current reading has plausible timestamp', (bg.date > 0));
    finally
      api.free;
    end;

  finally
    // Terminate & free PHP server
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;

procedure TAPINightscoutTester.SetUp;
begin

end;

procedure TAPINightscoutTester.TearDown;
begin

end;

initialization

RegisterTest(TAPINightscoutTester);
end.