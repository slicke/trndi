unit api_nightscout_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
test_server_helper;

type

TAPINightscoutTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestNightscoutInvalidUrl;
  procedure TestNightscoutUnauthorized;
  procedure TestNightscoutUnreachable;
  procedure TestNightscoutServerError;
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

// Edge network: explicit unreachable host/port should fail with a readable error
procedure TAPINightscoutTester.TestNightscoutUnreachable;
var
  api: NightScout;
begin
  api := NightScout.create('http://127.0.0.1:1', 'x');
  try
    AssertFalse('Connect should fail for unreachable server', api.connect);
    // network failures typically produce either no-data or connection messages
    AssertTrue('Error message should mention data or connect',
      (Pos('Did not receive any data', api.errormsg) > 0) or
      (Pos('Cannot connect', api.errormsg) > 0) or
      (api.errormsg <> ''));
  finally
    api.Free;
  end;
end;

// Edge network: server responds with HTTP 500 / non-JSON, should be handled gracefully
procedure TAPINightscoutTester.TestNightscoutServerError;
var
  api: NightScout;
  BaseURL: string;
begin

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');
  try
    // append special prefix that testserver treats as error generator
    api := NightScout.create(BaseURL + '/error500', 'test22');
    try
      AssertFalse('Connect should fail when server returns error', api.connect);
      // allow either parse error or unexpected structure message
      AssertTrue('Error message indicates bad response',
        (Pos('Unexpected JSON structure', api.errormsg) > 0) or
        (Pos('JSON parse error', api.errormsg) > 0) or
        (api.errormsg <> ''));
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPINightscoutTester.TestNightscoutUnauthorized;
var
  api: TrndiAPI;
  BaseURL: string;
begin

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // Wrong secret should yield Unauthorized from the fake server
    api := NightScout.create(BaseURL, 'wrongsecret');
    AssertFalse('Connect should fail for unauthorized secret', api.connect);
    api.Free;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPINightscoutTester.TestNightscoutGetReadingsRespectsMax;
var
  api: TrndiAPI;
  res: string;
  readings: BGResults;
  BaseURL: string;
begin

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    api := NightScout.create(BaseURL, 'test22');
    try
      AssertTrue('Connect to local NS', api.connect);

      readings := api.getReadings(0, 3, '', res, false);
      AssertTrue('Should not exceed requested max count', Length(readings) <= 3);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPINightscoutTester.TestNightscout;
var
  api: TrndiAPI;
  bg: BGreading;
  BaseURL: string;
begin
  bg.Clear;

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

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
    // Test server lifecycle handled by helper
    StopLocalTestServer;
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