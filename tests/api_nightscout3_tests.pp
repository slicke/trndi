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
 *)
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
  procedure TestNightscout3ServesCachedWindowWhenUnchanged;
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
  // Allow skipping integration tests via TRNDI_NO_TESTSERVER=1
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestNightscout3Unauthorized: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

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
  // Allow skipping integration tests via TRNDI_NO_TESTSERVER=1
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestNightscout3GetReadingsRespectsMax: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

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
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestNightscout3RefreshesExpiredTokenDuringPolling: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

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

procedure TAPINightscout3Tester.TestNightscout3ServesCachedWindowWhenUnchanged;
var
  api: TrndiAPI;
  res: string;
  first, second, third: BGResults;
  BaseURL: string;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestNightscout3ServesCachedWindowWhenUnchanged: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    api := NightScout3.create(BaseURL, 'test22');
    try
      AssertTrue('Nightscout3 connects to local fake server', api.connect);

      // First fetch fills the window cache; the second learns the server's
      // (fixed) entries lastModified stamp. The fake server generates fresh
      // entry timestamps on every real fetch, so if the third call still
      // matches the second exactly it must have been served from the cache
      // via the unchanged /lastModified short-circuit.
      first := api.getReadings(30, 5, '', res, false);
      AssertTrue('First fetch returns readings', Length(first) > 0);
      Sleep(15);
      second := api.getReadings(30, 5, '', res, false);
      AssertTrue('Second fetch returns readings', Length(second) > 0);
      Sleep(15);
      third := api.getReadings(30, 5, '', res, false);

      AssertEquals('Cached fetch returns the same window size',
        Length(second), Length(third));
      AssertTrue('Cached fetch returns the same newest timestamp',
        second[0].date = third[0].date);
      AssertTrue('Cached fetch returns the same newest value',
        second[0].val = third[0].val);
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
  // Allow skipping integration tests via TRNDI_NO_TESTSERVER=1
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestNightscout3: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

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