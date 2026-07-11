unit api_dexcom_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.dexcom_helpers,
trndi.api.xdrip, trndi.types, dialogs, dateutils,
test_server_helper;

type

TAPIDexcomTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestDexcom;
  procedure TestDexcomLocalServer;
  procedure TestDexcomSessionFailureMatcher;
  procedure TestDexcomExpiredSession;
end;

implementation

procedure TAPIDexcomTester.TestDexcom;
var
  api: TrndiAPI;
begin
  // Can create
  api := DexcomUSA.Create('test', 'test');
  // Test if the connect function runs
  AssertFalse('API Connect Fail', api.connect);
  asserttrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  api.Free;
end;

procedure TAPIDexcomTester.TestDexcomLocalServer;
var
  api: TrndiAPI;
  readings: BGResults;
  BaseURL: string;
begin
  // Allow skipping integration tests via TRNDI_NO_TESTSERVER=1
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestDexcomLocalServer: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // Use the new "full URL" override for Dexcom baseUrl.
    api := DexcomCustom.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('Dexcom connects to local fake server', api.connect);
      readings := api.getReadings(30, 3, '');
      AssertTrue('Dexcom returns at least one reading', Length(readings) > 0);
      AssertTrue('Dexcom reading value set', readings[0].val > 0);
      AssertTrue('Dexcom reading timestamp set', readings[0].date > 0);
      // Regression: testserver emits canonical "/Date(N)/" timestamps near now.
      // The previous slicer-based parser threw on the leading "(" and the
      // per-item except cleared the reading, leaving .date = 0. Anything more
      // than an hour off "now" means the parser is back to garbage output.
      // readings[0].date comes back as local time (UnixToDateTime AReturnUTC=False),
      // so compare against local Now, not UTC.
      AssertTrue('Dexcom timestamp parses near current time',
        Abs(SecondsBetween(readings[0].date, Now)) < 3600);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomTester.TestDexcomSessionFailureMatcher;
begin
  // Dexcom sends CamelCase error codes without spaces; the matcher must catch
  // them, not just the prose variants (regression: an expired session went
  // undetected once a day, so the automatic re-login never fired).
  AssertTrue('CamelCase SessionIdNotFound',
    DexcomLooksLikeSessionFailure('{"Code":"SessionIdNotFound"}'));
  AssertTrue('CamelCase SessionNotValid',
    DexcomLooksLikeSessionFailure('{"Code":"SessionNotValid"}'));
  AssertTrue('Prose session not found',
    DexcomLooksLikeSessionFailure('Session ID not found'));
  AssertTrue('Prose session is invalid',
    DexcomLooksLikeSessionFailure('The session is invalid'));
  AssertTrue('Account password error',
    DexcomLooksLikeSessionFailure('{"Code":"AccountPasswordInvalid"}'));
  AssertFalse('Glucose payload is not a session failure',
    DexcomLooksLikeSessionFailure(
    '[{"WT":"/Date(1610464324000)/","ST":"/Date(1610464324000)/","Value":120,"Trend":"Flat"}]'));
  AssertFalse('Empty response is not a session failure',
    DexcomLooksLikeSessionFailure(''));
end;

procedure TAPIDexcomTester.TestDexcomExpiredSession;
var
  api: TrndiAPI;
  readings: BGResults;
  BaseURL: string;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestDexcomExpiredSession: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // The fake server issues session "EXPIRED-SESSION" to account "expired"
    // and rejects it on the glucose endpoint with the real-world error object.
    api := DexcomCustom.Create('expired', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('Dexcom connects (session issued)', api.connect);
      readings := api.getReadings(30, 3, '');
      // Regression: {"Code":"SessionIdNotFound",...} used to be iterated as
      // readings (a JSON object's Count is its member count), fabricating
      // cleared placeholders with date=0 that the UI rendered as a value of
      // -50.2 mmol/L from 46211 days ago.
      AssertEquals('Error object must not become readings', 0, Length(readings));
      AssertTrue('errormsg carries the Dexcom error code',
        Pos('SessionIdNotFound', api.errormsg) > 0);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomTester.SetUp;
begin

end;

procedure TAPIDexcomTester.TearDown;
begin

end;

initialization

RegisterTest(TAPIDexcomTester);
end.