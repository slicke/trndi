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
 * - 2026-10-16: Covers the two-step login, the {"accountId"} authenticate
 *   reply, the by-name fallback, the null-GUID account, duplicate readings and
 *   the clamped request window.
 *)
unit api_dexcom_new_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.dexcomNew, trndi.types, dialogs, dateutils,
test_server_helper;

type

TAPIDexcomNewTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestDexcomNew;
  procedure TestDexcomNewUnreachable;
  procedure TestDexcomNewServerError;
  procedure TestDexcomNewLocalServer;
  procedure TestDexcomNewTwoStepLogin;
  procedure TestDexcomNewAccountIdObject;
  procedure TestDexcomNewByNameFallback;
  procedure TestDexcomNewUnknownAccount;
  procedure TestDexcomNewDuplicateReadings;
  procedure TestDexcomNewWindowClamp;
private
    {** Start (or reuse) the fake server; False when integration tests are
        disabled through TRNDI_NO_TESTSERVER=1. }
  function ServerUp(const AName: string; out BaseURL: string): boolean;
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

procedure TAPIDexcomNewTester.TestDexcomNewUnreachable;
var
  api: TrndiAPI;
begin
  // use the "custom" subclass so we can override the URL directly;
  // passing a bogus host/port will make the connection attempt fail.
  api := DexcomCustomNew.Create('user', 'pass',
    'http://127.0.0.1:1/ShareWebServices/Services/');
  try
    AssertFalse('Unreachable DexcomNew should not connect', api.connect);
    AssertTrue('Error message should not be empty', api.errormsg <> '');
  finally
    api.Free;
  end;
end;

procedure TAPIDexcomNewTester.TestDexcomNewServerError;
var
  api: TrndiAPI;
  BaseURL: string;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestDexcomNewServerError: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');
  try
    api := DexcomCustomNew.Create('anyuser','anypass', BaseURL + '/error500/ShareWebServices/Services/');
    try
      AssertFalse('Server error path should prevent connect', api.connect);
      AssertTrue('Error message indicates bad response', api.errormsg <> '');
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomNewTester.TestDexcomNewLocalServer;
var
  api: TrndiAPI;
  readings: BGResults;
  BaseURL: string;
begin
  // Allow skipping integration tests via TRNDI_NO_TESTSERVER=1
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestDexcomNewLocalServer: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // Use DexcomCustomNew with full URL override for testing
    api := DexcomCustomNew.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('DexcomNew connects to local fake server', api.connect);
      readings := api.getReadings(30, 3, '');
      AssertTrue('DexcomNew returns at least one reading', Length(readings) > 0);
      AssertTrue('DexcomNew reading value set', readings[0].val > 0);
      AssertTrue('DexcomNew reading timestamp set', readings[0].date > 0);
      // Regression: testserver emits canonical "/Date(N)/" timestamps near now.
      // The previous slicer-based parser threw on the leading "(" and the
      // per-item except cleared the reading, leaving .date = 0. Anything more
      // than an hour off "now" means the parser is back to garbage output.
      // readings[0].date comes back as local time (UnixToDateTime AReturnUTC=False),
      // so compare against local Now, not UTC.
      AssertTrue('DexcomNew timestamp parses near current time',
        Abs(SecondsBetween(readings[0].date, Now)) < 3600);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

function TAPIDexcomNewTester.ServerUp(const AName: string; out BaseURL: string): boolean;
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

procedure TAPIDexcomNewTester.TestDexcomNewTwoStepLogin;
var
  api: DexcomNew;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomNewTwoStepLogin', BaseURL) then
    Exit;
  try
    // A plain username used to go straight to LoginPublisherAccountByName.
    // Every account type now resolves to an account id first, which the fake
    // server marks by issuing a different session for that route.
    api := DexcomCustomNew.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('DexcomNew connects', api.connect);
      AssertEquals('Session comes from LoginPublisherAccountById', 'BYID-SESSION', api.Session);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomNewTester.TestDexcomNewAccountIdObject;
var
  api: DexcomNew;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomNewAccountIdObject', BaseURL) then
    Exit;
  try
    // Newer accounts get {"accountId":"..."} from the authenticate call.
    api := DexcomCustomNew.Create('g7', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('DexcomNew connects with an object-shaped account id', api.connect);
      AssertEquals('Session issued for the extracted id', 'BYID-SESSION', api.Session);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomNewTester.TestDexcomNewByNameFallback;
var
  api: DexcomNew;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomNewByNameFallback', BaseURL) then
    Exit;
  try
    // A server that does not answer the authenticate call leaves the one-step
    // login as the way in.
    api := DexcomCustomNew.Create('noauth', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('DexcomNew connects through the by-name fallback', api.connect);
      AssertEquals('Session comes from LoginPublisherAccountByName', 'BYNAME-SESSION', api.Session);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPIDexcomNewTester.TestDexcomNewUnknownAccount;
var
  api: DexcomNew;
  BaseURL: string;
begin
  if not ServerUp('TestDexcomNewUnknownAccount', BaseURL) then
    Exit;
  try
    // The null GUID means the lookup ran and matched nothing; it must not be
    // sent on to the login call, nor fall back to the by-name route.
    api := DexcomCustomNew.Create('nobody', 'anypass', BaseURL + '/ShareWebServices/Services/');
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

procedure TAPIDexcomNewTester.TestDexcomNewDuplicateReadings;
var
  api: DexcomNew;
  readings: BGResults;
  BaseURL: string;
  raw: string;
begin
  if not ServerUp('TestDexcomNewDuplicateReadings', BaseURL) then
    Exit;
  try
    // G7 Direct-to-Watch: each sample arrives twice, a second apart. The
    // duplicate is dropped and the delta is taken against the previous
    // sample, not its duplicate (which gave 0).
    api := DexcomCustomNew.Create('dupes', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('DexcomNew connects', api.connect);
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

procedure TAPIDexcomNewTester.TestDexcomNewWindowClamp;
var
  api: DexcomNew;
  readings: BGResults;
  BaseURL: string;
  raw: string;
begin
  if not ServerUp('TestDexcomNewWindowClamp', BaseURL) then
    Exit;
  try
    // The window comes from user-editable settings. An oversized one used to
    // raise out of getReadings; it is clamped to Share's limits instead.
    api := DexcomCustomNew.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services/');
    try
      AssertTrue('DexcomNew connects', api.connect);
      readings := api.getReadings(5000, 1000, '', raw, false);
      AssertTrue('Oversized window still returns readings', Length(readings) > 0);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
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