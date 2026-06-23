unit api_nightscout3_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout3, trndi.types, dialogs, dateutils,
test_server_helper;

type

TAPINightscout3Tester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestNightscout3InvalidUrl;
  procedure TestNightscout3Unauthorized;
  procedure TestNightscout3GetReadingsRespectsMax;
  procedure TestNightscout3RefreshesExpiredTokenDuringPolling;
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
  BaseURL: string;
begin

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // Use invalid token
    api := NightScout3.create(BaseURL, 'invalid-token');
    try
      AssertFalse('Invalid token should not connect', api.connect);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPINightscout3Tester.TestNightscout3GetReadingsRespectsMax;
var
  api: TrndiAPI;
  res: string;
  readings: BGResults;
  BaseURL: string;
begin

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    api := NightScout3.create(BaseURL, 'test22');
    try
      AssertTrue('Nightscout3 connects to local fake server', api.connect);
      readings := api.getReadings(30, 5, '', res, false); // Request max 5 readings
      AssertTrue('Nightscout3 returns at most 5 readings', Length(readings) <= 5);
      AssertTrue('Nightscout3 returns at least 1 reading', Length(readings) >= 1);
      AssertTrue('Nightscout3 reading value set', readings[0].val > 0);
      AssertTrue('Nightscout3 reading timestamp set', readings[0].date > 0);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPINightscout3Tester.TestNightscout3RefreshesExpiredTokenDuringPolling;
var
  api: TrndiAPI;
  res: string;
  firstReadings: BGResults;
  secondReadings: BGResults;
  BaseURL: string;
begin

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // Special token mode in fake server: first entries call succeeds,
    // second call requires re-auth and should still succeed transparently.
    api := NightScout3.create(BaseURL, 'rotate');
    try
      AssertTrue('Nightscout3 connects with rotating token mode', api.connect);

      firstReadings := api.getReadings(30, 5, '', res, false);
      AssertTrue('First fetch returns readings', Length(firstReadings) > 0);

      secondReadings := api.getReadings(30, 5, '', res, false);
      AssertTrue('Second fetch should recover from token expiry and return readings',
        Length(secondReadings) > 0);
      AssertTrue('Second fetch should still include a valid reading value',
        secondReadings[0].val > 0);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPINightscout3Tester.TestNightscout3;
var
  api: TrndiAPI;
  res: string;
  readings: BGResults;
  BaseURL: string;
begin

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    api := NightScout3.create(BaseURL, 'test22');
    try
      AssertTrue('Nightscout3 connects to local fake server', api.connect);
      readings := api.getReadings(30, 3, '', res, false);
      AssertTrue('Nightscout3 returns at least one reading', Length(readings) > 0);
      AssertTrue('Nightscout3 reading value set', readings[0].val > 0);
      AssertTrue('Nightscout3 reading timestamp set', readings[0].date > 0);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
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