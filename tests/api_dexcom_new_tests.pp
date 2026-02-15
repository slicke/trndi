unit api_dexcom_new_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.dexcomNew, trndi.types, dialogs, dateutils,
process, php_server_helper;

type

TAPIDexcomNewTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestDexcomNew;
  procedure TestDexcomNewLocalServer;
end;

implementation

procedure TAPIDexcomNewTester.TestDexcomNew;
var
  api: TrndiAPI;
begin
  // Can create
  api := DexcomUSANew.Create('test', 'test');
  // Test if the connect function runs (should fail with dummy credentials)
  AssertFalse('API Connect Fail', api.connect);
  AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  api.Free;
end;

procedure TAPIDexcomNewTester.TestDexcomNewLocalServer;
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
    // Use DexcomCustomNew with full URL override for testing
    api := DexcomCustomNew.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('DexcomNew connects to local fake server', api.connect);
      readings := api.getReadings(30, 3, '', res);
      AssertTrue('DexcomNew returns at least one reading', Length(readings) > 0);
      AssertTrue('DexcomNew reading value set', readings[0].val > 0);
      AssertTrue('DexcomNew reading timestamp set', readings[0].date > 0);
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

procedure TAPIDexcomNewTester.SetUp;
begin

end;

procedure TAPIDexcomNewTester.TearDown;
begin

end;

initialization

RegisterTest(TAPIDexcomNewTester);
end.