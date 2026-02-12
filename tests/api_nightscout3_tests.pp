unit api_nightscout3_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout3, trndi.types, dialogs, dateutils,
process, php_server_helper;

type

TAPINightscout3Tester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestNightscout3InvalidUrl;
  procedure TestNightscout3Unauthorized;
  procedure TestNightscout3GetReadingsRespectsMax;
  procedure TestNightscout3;
end;

implementation

procedure TAPINightscout3Tester.TestNightscout3InvalidUrl;
var
  api: NightScout3;
begin
  api := NightScout3.create('not-a-url', 'x');
  try
    AssertFalse('Invalid URL should not connect', api.connect);
  finally
    api.Free;
  end;
end;

procedure TAPINightscout3Tester.TestNightscout3Unauthorized;
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
    // Use invalid token
    api := NightScout3.create(BaseURL, 'invalid-token');
    try
      AssertFalse('Invalid token should not connect', api.connect);
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

procedure TAPINightscout3Tester.TestNightscout3GetReadingsRespectsMax;
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

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    api := NightScout3.create(BaseURL, 'test22');
    try
      AssertTrue('Nightscout3 connects to local fake server', api.connect);
      readings := api.getReadings(30, 5, '', res); // Request max 5 readings
      AssertTrue('Nightscout3 returns at most 5 readings', Length(readings) <= 5);
      AssertTrue('Nightscout3 returns at least 1 reading', Length(readings) >= 1);
      AssertTrue('Nightscout3 reading value set', readings[0].val > 0);
      AssertTrue('Nightscout3 reading timestamp set', readings[0].date > 0);
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

procedure TAPINightscout3Tester.TestNightscout3;
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

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    api := NightScout3.create(BaseURL, 'test22');
    try
      AssertTrue('Nightscout3 connects to local fake server', api.connect);
      readings := api.getReadings(30, 3, '', res);
      AssertTrue('Nightscout3 returns at least one reading', Length(readings) > 0);
      AssertTrue('Nightscout3 reading value set', readings[0].val > 0);
      AssertTrue('Nightscout3 reading timestamp set', readings[0].date > 0);
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

procedure TAPINightscout3Tester.SetUp;
begin

end;

procedure TAPINightscout3Tester.TearDown;
begin

end;

initialization

RegisterTest(TAPINightscout3Tester);
end.