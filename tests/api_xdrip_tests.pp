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
unit api_xdrip_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
test_server_helper, fpjson, jsonparser, sha1;

type

TAPIXDripTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestXDrip;
  procedure TestXDripUnreachable;
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

procedure TAPIXDripTester.TestXDripUnreachable;
var
  api: TrndiAPI;
begin
  api := xDrip.create('http://127.0.0.1:1', 'testsecret');
  try
    AssertFalse('Connect to unreachable xDrip should fail', api.connect);
    AssertTrue('Error message should not be empty', api.errormsg <> '');
    AssertTrue('Error message references connect or server',
      (Pos('Cannot connect', api.errormsg) > 0) or
      (Pos('Invalid address', api.errormsg) = 0)); // don't misinterpret
  finally
    api.Free;
  end;
end;

procedure TAPIXDripTester.TestXDripLocalServer;
var
  api: TrndiAPI;
  readings: BGResults;
  bg: BGReading;
  BaseURL: string;
  rawJson: string;
  native: TrndiNative;
  js: TJSONData;
  firstEntry: TJSONObject;
  rawDateMs: int64;
  expectedDate: TDateTime;
begin
  // Allow skipping integration tests via TRNDI_NO_TESTSERVER=1
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestXDripLocalServer: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  // Start or reuse embedded test server (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    api := xDrip.create(BaseURL, 'test22');
    try
      api.timezone := 120; // Simulate a desktop timezone that differs from the xDrip source.

      if not api.connect then
        Fail('xDrip connects to local fake server. Error: ' + api.errormsg);
      AssertTrue('xDrip connected', true);
      AssertTrue('xDrip basetime stays close to local time',
        Abs(api.getBasetime - DateTimeToUnix(Now)) < 300);
      
      // Test thresholds from status.json (same as Nightscout)
      AssertEquals('xDrip bgHigh threshold mapped', 260, api.cgmHi);
      AssertEquals('xDrip bgLow threshold mapped', 55, api.cgmLo);
      
      // Test getting current reading from pebble endpoint
      bg.Clear;
      AssertTrue('xDrip getCurrent returns data', api.getCurrent(bg));
      AssertTrue('Current reading has plausible value', bg.val > 0);
      AssertTrue('Current reading has plausible timestamp', bg.date > 0);
      
      // Test getting multiple readings
      readings := api.getReadings(30, 3, '');
      AssertTrue('xDrip returns at least one reading', Length(readings) > 0);
      AssertTrue('xDrip reading value set', readings[0].val > 0);
      AssertTrue('xDrip reading timestamp set', readings[0].date > 0);

      native := TrndiNative.Create('TrndiTest', BaseURL);
      try
        rawJson := native.request(false, 'sgv.json', ['count=3'], '',
          'api-secret=' + SHA1Print(SHA1String('test22')));
      finally
        native.Free;
      end;
      js := GetJSON(rawJson);
      try
        AssertTrue('xDrip reading payload is an array', js is TJSONArray);
        firstEntry := TJSONObject(TJSONArray(js).Items[0]);
        rawDateMs := firstEntry.Get('date', int64(0));
        expectedDate := UnixToDateTime(rawDateMs div 1000, False);
        // The fake server stamps each entry with the time of the request, so this
        // raw fetch and the getReadings fetch above can straddle a second boundary.
        // A timezone/epoch mis-mapping would be off by whole hours, so allow a few
        // seconds of drift rather than demanding an exact match.
        AssertTrue(Format('xDrip reading timestamp uses UTC epoch from server ' +
          '(expected near %s but was %s)',
          [FormatDateTime('yyyy-mm-dd hh:nn:ss', expectedDate),
           FormatDateTime('yyyy-mm-dd hh:nn:ss', readings[0].date)]),
          Abs(SecondsBetween(expectedDate, readings[0].date)) <= 5);
      finally
        js.Free;
      end;
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
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