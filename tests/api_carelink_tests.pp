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
unit api_carelink_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.api, trndi.api.carelink, trndi.types, dateutils,
test_server_helper;

type

TAPICareLinkTester = class(TTestCase)
private
  FCredsChangedCount: integer;
  FLastCreds: string;
  procedure HandleCredsChanged(const newCreds: string);
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  // Offline: credential-blob validation
  procedure TestEmptyCredentials;
  procedure TestNonJsonCredentials;
  procedure TestMissingRefreshToken;
  procedure TestGetReadingsWithoutConnect;
  procedure TestDeviceStatusDefaultsAreUnknown;
  // Offline: class-level metadata
  procedure TestSupportsWebLogin;
  procedure TestParamLabels;
  // Integration against the embedded test server
  procedure TestCareLinkLocalServer;
  procedure TestCareLinkPayloadMetadata;
  procedure TestCareLinkBolusExtraction;
  procedure TestCareLinkCarbExtraction;
  procedure TestTransientServerErrorFailsConnect;
end;

implementation

procedure TAPICareLinkTester.SetUp;
begin
  FCredsChangedCount := 0;
  FLastCreds := '';
end;

procedure TAPICareLinkTester.TearDown;
begin

end;

procedure TAPICareLinkTester.HandleCredsChanged(const newCreds: string);
begin
  Inc(FCredsChangedCount);
  FLastCreds := newCreds;
end;

procedure TAPICareLinkTester.TestEmptyCredentials;
var
  api: TrndiAPI;
begin
  // ParseCredentials rejects an empty blob before any network access
  api := CareLinkEU.Create('someuser', '');
  try
    AssertFalse('Connect must fail without token data', api.connect);
    AssertTrue('Error message mentions CareLink', Pos('CareLink', api.errormsg) > 0);
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestNonJsonCredentials;
var
  api: TrndiAPI;
begin
  api := CareLinkEU.Create('', 'this is not a json blob');
  try
    AssertFalse('Connect must fail on unparsable token data', api.connect);
    AssertTrue('Error message set', api.errormsg <> '');
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestMissingRefreshToken;
var
  api: TrndiAPI;
begin
  // A blob without refresh_token cannot keep the session alive
  api := CareLinkEU.Create('', '{"access_token":"abc","client_id":"x"}');
  try
    AssertFalse('Connect must fail without a refresh token', api.connect);
    AssertTrue('Error message set', api.errormsg <> '');
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestGetReadingsWithoutConnect;
var
  api: CareLink;
  readings: BGResults;
  res: string;
begin
  api := CareLinkEU.Create('', '{"refresh_token":"r","client_id":"c"}');
  try
    readings := api.GetReadings(1440, 10, '', res, false);
    AssertEquals('No readings before Connect', 0, Length(readings));
    AssertTrue('Status message says not authenticated', Pos('Not authenticated', res) > 0);
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestSupportsWebLogin;
begin
  // CareLink credentials come from the assisted browser login helper
  AssertTrue('CareLink uses the web login helper', CareLink.supportsWebLogin);
end;

procedure TAPICareLinkTester.TestParamLabels;
begin
  AssertTrue('User label set', CareLink.ParamLabel(APLUser) <> '');
  AssertTrue('Pass label set', CareLink.ParamLabel(APLPass) <> '');
  AssertTrue('Description mentions the browser login',
    Pos('browser', LowerCase(CareLink.ParamLabel(APLDesc))) > 0);
end;

procedure TAPICareLinkTester.TestCareLinkLocalServer;
var
  api: CareLink;
  readings: BGResults;
  BaseURL, creds, res: string;
  refreshesAfterConnect: integer;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestCareLinkLocalServer: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // token_url + data_url overrides skip live endpoint discovery entirely;
    // no access_token in the blob forces a refresh during Connect.
    creds := Format(
      '{"refresh_token":"initial-refresh","client_id":"test-client",' +
      '"token_url":"%s/carelink/token","data_url":"%s/carelink/display/message",' +
      '"patientId":"PAT1"}', [BaseURL, BaseURL]);

    api := CareLinkEU.Create('', creds);
    try
      api.OnCredentialsChanged := @HandleCredsChanged;

      if not api.Connect then
        Fail('CareLink connects to local fake server. Error: ' + api.errormsg);

      // Username comes from the access token's preferred_username claim
      AssertEquals('Username read from JWT claims', 'carelinkuser', api.Username);

      // The refresh rotated the token; the updated blob must be emitted for
      // persistence and keep the untouched keys as-is
      AssertEquals('One refresh during Connect', 1, FCredsChangedCount);
      AssertTrue('Rotated refresh token in emitted blob',
        Pos('rotated-refresh-', FLastCreds) > 0);
      AssertTrue('client_id preserved in emitted blob',
        Pos('"client_id" : "test-client"', FLastCreds) > 0);
      refreshesAfterConnect := FCredsChangedCount;

      readings := api.GetReadings(1440, 100, '', res, false);

      // 6 sgs entries, one of which is a gap (sg=0) that must be skipped
      AssertEquals('Gap slot skipped', 5, Length(readings));

      // Newest first after sorting
      AssertEquals('Newest reading value', 120.0, readings[0].convert(mgdl), 0.01);
      AssertTrue('Newest is most recent', readings[0].date > readings[1].date);

      // The device arrow (lastSGTrend=UP) overrides the computed trend on the
      // newest reading; older ones get the computed 5-min-normalized trend
      AssertEquals('Server arrow on newest', Ord(TdSingleUp), Ord(readings[0].trend));
      AssertEquals('Computed trend +10 mg/dL per 5 min', Ord(TdSingleUp), Ord(readings[1].trend));

      // Delta between consecutive readings (120 - 110)
      AssertEquals('Newest delta', 10.0, readings[0].convert(mgdl, BGDelta), 0.01);

      // Out-of-range values keep Trndi's sentinels: 450 -> 401, 30 -> 39
      AssertEquals('High clamp', 401.0, readings[3].convert(mgdl), 0.01);
      AssertEquals('Low clamp', 39.0, readings[4].convert(mgdl), 0.01);

      // Active insulin (IOB) parsed from the payload
      AssertEquals('Active insulin', 2.5, api.ActiveInsulin, 0.01);
      AssertTrue('Active insulin timestamp set', api.ActiveInsulinTime > 0);

      // The fresh token is valid for an hour: a second fetch must not refresh
      readings := api.GetReadings(1440, 100, '', res, false);
      AssertEquals('No extra refresh while token is fresh',
        refreshesAfterConnect, FCredsChangedCount);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

// The display-message payload carries more than readings: the account's own
// high/low limits, the server clock, auto-basal delivery and the sensor/pump
// housekeeping. Connect applies all of it from its probe fetch.
procedure TAPICareLinkTester.TestCareLinkPayloadMetadata;
var
  api: CareLink;
  status: TCGMDeviceStatus;
  BaseURL, creds: string;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestCareLinkPayloadMetadata: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    creds := Format(
      '{"refresh_token":"initial-refresh","client_id":"test-client",' +
      '"token_url":"%s/carelink/token","data_url":"%s/carelink/display/message",' +
      '"patientId":"PAT1"}', [BaseURL, BaseURL]);

    api := CareLinkEU.Create('', creds);
    try
      if not api.Connect then
        Fail('CareLink connects to local fake server. Error: ' + api.errormsg);

      // The limits schedule holds an older block (250/70), the block in force
      // (234/65) and a future one (999/1). Only the current block may apply,
      // replacing the base class's 401/40 placeholders.
      AssertEquals('High limit from the block in force', 234, api.cgmHi);
      AssertEquals('Low limit from the block in force', 65, api.cgmLo);

      // currentServerTime matches the test machine's clock, so the computed
      // offset must be ~0 rather than a timezone-sized number.
      AssertTrue('Server time offset is near zero, not a tz offset',
        Abs(api.offset) < 60);

      // Auto-basal is the sum of AUTO_BASAL_DELIVERY markers within the last
      // hour: 0.075 + 0.1. The MEAL marker and the 3h-old basal marker in the
      // fixture must not be counted.
      AssertEquals('Auto-basal rate over the last hour', 0.175, api.getBasalRate, 0.0001);

      AssertTrue('Device status reported', api.getDeviceStatus(status));
      AssertEquals('Sensor duration hours', 126, status.sensorDurationHours);
      AssertEquals('Reservoir percent', 5, status.reservoirPercent);
      AssertEquals('Reservoir units', 15.4, status.reservoirUnits, 0.01);
      AssertEquals('Pump battery percent', 50, status.pumpBatteryPercent);
      AssertTrue('Sensor reports no fault', status.sensorOK);
      AssertFalse('Pump not suspended', status.pumpSuspended);
      AssertEquals('No status message when the payload says NO_ERROR_MESSAGE',
        '', status.statusMessage);

      // gstBatteryLevel 255 is the device's "no reading" marker, not a
      // 255% battery, so it must come back as unknown.
      AssertEquals('Transmitter battery unknown, not 255',
        DEVICE_STATUS_UNKNOWN, status.transmitterBatteryPercent);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

// The markers list feeds the history graph's insulin overlay. Both delivery
// kinds must survive the trip, carbohydrate-only markers must not become
// phantom doses, and a zero delivery must not become a stem on the graph.
procedure TAPICareLinkTester.TestCareLinkBolusExtraction;
var
  api: CareLink;
  boluses: TBolusList;
  BaseURL, creds: string;
  i, autoCount, manualCount: integer;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestCareLinkBolusExtraction: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    creds := Format(
      '{"refresh_token":"initial-refresh","client_id":"test-client",' +
      '"token_url":"%s/carelink/token","data_url":"%s/carelink/display/message",' +
      '"patientId":"PAT1"}', [BaseURL, BaseURL]);

    api := CareLinkEU.Create('', creds);
    try
      AssertTrue('CareLink advertises bolus support', api.supportsBoluses);

      if not api.Connect then
        Fail('CareLink connects to local fake server. Error: ' + api.errormsg);

      AssertTrue('Boluses reported after a fetch', api.getBoluses(boluses));

      // Three auto-basal markers (0.075, 0.1, 5.5) plus the two usable INSULIN
      // markers. The MEAL marker is carbohydrates, and the 0 U INSULIN marker
      // is a record of nothing delivered — neither may appear.
      AssertEquals('Deliveries extracted', 5, Length(boluses));

      autoCount := 0;
      manualCount := 0;
      for i := 0 to High(boluses) do
      begin
        AssertTrue('No zero-unit delivery survives', boluses[i].units > 0);
        if boluses[i].automatic then
          Inc(autoCount)
        else
          Inc(manualCount);
      end;
      // 3 auto-basal + the AUTOCORRECTION insulin marker.
      AssertEquals('Automatic deliveries', 4, autoCount);
      AssertEquals('User-requested deliveries', 1, manualCount);

      // Oldest first: the graph draws in array order.
      for i := 1 to High(boluses) do
        AssertTrue('Deliveries are in chronological order',
          boluses[i].time >= boluses[i - 1].time);

      // The 3h-old auto-basal marker is outside the basal-rate window but is
      // still a delivery that happened, so the overlay must keep it.
      AssertEquals('Oldest delivery is the 3h-old auto-basal',
        5.5, boluses[0].units, 0.0001);

      // deliveredFastAmount, not the programmed figure, and the carbs entered
      // with it come along.
      AssertEquals('User bolus amount', 6.2, boluses[1].units, 0.0001);
      AssertFalse('User bolus is not automatic', boluses[1].automatic);
      AssertEquals('Carbs recorded with the bolus', 45.0, boluses[1].carbs, 0.0001);

      // An AUTOCORRECTION INSULIN marker is the pump acting on its own, even
      // though it arrives in the same marker type as a user bolus.
      AssertEquals('Auto-correction amount', 1.25, boluses[2].units, 0.0001);
      AssertTrue('Auto-correction is automatic', boluses[2].automatic);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

// Carbs arrive twice for a meal that was bolused for — once as a meal marker
// and once on the bolus. The overlay must show that meal once.
procedure TAPICareLinkTester.TestCareLinkCarbExtraction;
var
  api: CareLink;
  carbs: TCarbList;
  boluses: TBolusList;
  BaseURL, creds: string;
  i: integer;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestCareLinkCarbExtraction: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    creds := Format(
      '{"refresh_token":"initial-refresh","client_id":"test-client",' +
      '"token_url":"%s/carelink/token","data_url":"%s/carelink/display/message",' +
      '"patientId":"PAT1"}', [BaseURL, BaseURL]);

    api := CareLinkEU.Create('', creds);
    try
      AssertTrue('CareLink advertises carb support', api.supportsCarbs);

      if not api.Connect then
        Fail('CareLink connects to local fake server. Error: ' + api.errormsg);

      AssertTrue('Carbs reported after a fetch', api.getCarbs(carbs));

      // The fixture holds a standalone 30 g meal marker and a 45 g meal marker
      // that shares its moment with a bolus carrying carbInput 45. The second
      // pair is one meal, so three sources must yield two entries.
      AssertEquals('Carb entries after reconciling', 2, Length(carbs));

      for i := 0 to High(carbs) do
        AssertTrue('No zero-gram entry survives', carbs[i].grams > 0);

      for i := 1 to High(carbs) do
        AssertTrue('Carb entries are in chronological order',
          carbs[i].time >= carbs[i - 1].time);

      AssertEquals('Bolused meal counted once, at its meal-marker amount',
        45.0, carbs[0].grams, 0.0001);
      AssertEquals('Standalone meal marker', 30.0, carbs[1].grams, 0.0001);

      // The bolus keeps its own carb figure; the reconciling happens in the
      // carb list, not by stripping the bolus record.
      AssertTrue('Boluses still available', api.getBoluses(boluses));
      AssertEquals('Bolus retains its carb figure', 45.0, boluses[1].carbs, 0.0001);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

// A backend that reports nothing must not look like a faulting sensor.
procedure TAPICareLinkTester.TestDeviceStatusDefaultsAreUnknown;
var
  api: CareLink;
  status: TCGMDeviceStatus;
  boluses: TBolusList;
  carbs: TCarbList;
begin
  api := CareLinkEU.Create('', '{"refresh_token":"r","client_id":"c"}');
  try
    AssertFalse('No status before a fetch', api.getDeviceStatus(status));
    AssertEquals('Sensor duration unknown',
      DEVICE_STATUS_UNKNOWN, status.sensorDurationHours);
    AssertEquals('Reservoir unknown',
      DEVICE_STATUS_UNKNOWN, status.reservoirPercent);
    AssertTrue('Absent fault data does not mean a fault', status.sensorOK);
    AssertEquals('No basal rate before a fetch', 0.0, api.getBasalRate, 0.0001);
    AssertFalse('No boluses before a fetch', api.getBoluses(boluses));
    AssertEquals('Bolus list is empty, not stale', 0, Length(boluses));
    AssertFalse('No carbs before a fetch', api.getCarbs(carbs));
    AssertEquals('Carb list is empty, not stale', 0, Length(carbs));
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestTransientServerErrorFailsConnect;
var
  api: TrndiAPI;
  BaseURL, creds: string;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestTransientServerErrorFailsConnect: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // Token refresh works, but the data endpoint 500s persistently: the
    // retry loop must give up and surface the HTTP status
    creds := Format(
      '{"refresh_token":"initial-refresh","client_id":"test-client",' +
      '"token_url":"%s/carelink/token","data_url":"%s/error500"}',
      [BaseURL, BaseURL]);

    api := CareLinkEU.Create('', creds);
    try
      AssertFalse('Connect fails when the data endpoint keeps erroring', api.connect);
      AssertTrue('Error message carries the HTTP status',
        Pos('HTTP 500', api.errormsg) > 0);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

initialization

RegisterTest(TAPICareLinkTester);
end.
