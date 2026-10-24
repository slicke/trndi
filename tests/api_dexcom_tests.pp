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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-08-16: The session-failure matcher test now expects a credential
 *   failure to be terminal rather than a recoverable session error, so a wrong
 *   password is no longer retried into a lockout.
 * - 2026-10-16: Covers the two-step login, the {"accountId"} authenticate
 *   reply, the by-name fallback, the null-GUID account, duplicate readings and
 *   the clamped request window.
 *)
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
  procedure TestDexcomTwoStepLogin;
  procedure TestDexcomAccountIdObject;
  procedure TestDexcomByNameFallback;
  procedure TestDexcomUnknownAccount;
  procedure TestDexcomDuplicateReadings;
  procedure TestDexcomWindowClamp;
private
    {** Start (or reuse) the fake server; False when integration tests are
        disabled through TRNDI_NO_TESTSERVER=1. }
  function ServerUp(const AName: string; out BaseURL: string): boolean;
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
  // A wrong password is terminal, not a recoverable session failure. Matching
  // it here made the caller re-authenticate and resubmit the same bad
  // credentials, so every poll spent two failed sign-ins instead of one and
  // drove the account toward Dexcom's SSO_AuthenticateMaxAttemptsExceeded
  // lockout. It is reported through DexcomAuthFailureMessage instead.
  AssertFalse('Account password error is not a session failure',
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

function TAPIDexcomTester.ServerUp(const AName: string; out BaseURL: string): boolean;
begin
  Result := false;
  BaseURL := '';
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping ' + AName + ': embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');
  Result := true;
end;

procedure TAPIDexcomTester.TestDexcomTwoStepLogin;
var
  api: Dexcom;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomTwoStepLogin', BaseURL) then
    Exit;
  try
    // A plain username used to go straight to LoginPublisherAccountByName.
    // Every account type now resolves to an account id first, which the fake
    // server marks by issuing a different session for that route.
    api := DexcomCustom.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('Dexcom connects', api.connect);
      AssertEquals('Session comes from LoginPublisherAccountById', 'BYID-SESSION', api.Session);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomTester.TestDexcomAccountIdObject;
var
  api: Dexcom;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomAccountIdObject', BaseURL) then
    Exit;
  try
    // Newer accounts get {"accountId":"..."} from the authenticate call. The
    // old quote-stripping sent {accountId:...} on as the id and the login failed.
    api := DexcomCustom.Create('g7', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('Dexcom connects with an object-shaped account id', api.connect);
      AssertEquals('Session issued for the extracted id', 'BYID-SESSION', api.Session);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomTester.TestDexcomByNameFallback;
var
  api: Dexcom;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomByNameFallback', BaseURL) then
    Exit;
  try
    // A server that does not answer the authenticate call leaves the one-step
    // login as the way in.
    api := DexcomCustom.Create('noauth', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('Dexcom connects through the by-name fallback', api.connect);
      AssertEquals('Session comes from LoginPublisherAccountByName', 'BYNAME-SESSION', api.Session);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomTester.TestDexcomUnknownAccount;
var
  api: Dexcom;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomUnknownAccount', BaseURL) then
    Exit;
  try
    // The null GUID means the lookup ran and matched nothing; it must not be
    // sent on to the login call, nor fall back to the by-name route.
    api := DexcomCustom.Create('nobody', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertFalse('Null-GUID account id is rejected', api.connect);
      AssertTrue('Error names the missing account: ' + api.errormsg,
        Pos('No Dexcom account', api.errormsg) > 0);
      AssertTrue('The error is the user''s to fix', api.errorNeedsUser);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomTester.TestDexcomDuplicateReadings;
var
  api: Dexcom;
  readings: BGResults;
  BaseURL: string;
  raw: string;
begin
  if not ServerUp('TestDexcomDuplicateReadings', BaseURL) then
    Exit;
  try
    // G7 Direct-to-Watch: each sample arrives twice, a second apart. The
    // duplicate is dropped and the delta is taken against the previous
    // sample, not its duplicate (which gave 0).
    api := DexcomCustom.Create('dupes', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('Dexcom connects', api.connect);
      readings := api.getReadings(30, 10, '', raw, false);
      AssertEquals('Doubled uploads collapse to one reading each', 2, Length(readings));
      AssertEquals('Newest reading kept', 120.0, readings[0].val, 0.01);
      AssertEquals('Delta spans the two distinct samples', 5.0, readings[0].delta, 0.01);
      AssertTrue('Oldest reading has no older neighbour, so no delta', readings[1].deltaEmpty);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomTester.TestDexcomWindowClamp;
var
  api: Dexcom;
  readings: BGResults;
  BaseURL: string;
  raw: string;
begin
  if not ServerUp('TestDexcomWindowClamp', BaseURL) then
    Exit;
  try
    // The window comes from user-editable settings. An oversized one used to
    // raise out of getReadings; it is clamped to Share's limits instead.
    api := DexcomCustom.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('Dexcom connects', api.connect);
      readings := api.getReadings(5000, 1000, '', raw, false);
      AssertTrue('Oversized window still returns readings', Length(readings) > 0);
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