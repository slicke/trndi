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
unit api_nightscout3_treatment_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, dateutils,
  trndi.api, trndi.api.nightscout3;

type
  (*****************************************************************************
    Nightscout v3 devicestatus / treatments extraction.

    Offline: the fixtures are handed straight to the extractors, so nothing
    here needs a server. What is worth testing is the reading of two
    collections that have no schema — Nightscout stores whatever an uploader
    writes — and the rules that keep an unreported field from being mistaken
    for a reported zero.
   ****************************************************************************)
  TNightscout3TreatmentTests = class(TTestCase)
  private
    FAPI: NightScout3;
    {** Load a fixture, substituting the sensor-expiry placeholder with a
        timestamp the given number of hours from now. }
    function LoadFixture(const AName: string; const AExpiryHours: double): string;
    procedure FeedDeviceStatus(const AExpiryHours: double);
    procedure FeedTreatments;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBolusExtraction;
    procedure TestCarbExtraction;
    procedure TestDeviceStatus;
    procedure TestExpiredSensorReportsNoLifeLeft;
    procedure TestUploaderBatteryIsNotThePumpBattery;
    procedure TestUnreadablePayloadReportsNothing;
    procedure TestNothingReportedBeforeAFetch;
  end;

implementation

type
  {** Exposes the protected extractors so a fixture payload can be pushed
      straight in, without the auth flow a real fetch would need. }
  TNightscout3Probe = class(NightScout3)
  public
    procedure FeedStatus(const AResponse: string);
    procedure FeedTreatmentList(const AResponse: string);
  end;

procedure TNightscout3Probe.FeedStatus(const AResponse: string);
begin
  ExtractDeviceStatus(AResponse);
end;

procedure TNightscout3Probe.FeedTreatmentList(const AResponse: string);
begin
  ExtractTreatments(AResponse);
end;

{------------------------------------------------------------------------------
  A UTC wall-clock time from the fixture, expressed in local time — the same
  conversion the driver applies, so every assertion below holds in any timezone
  rather than only in the one the fixture was written in.
 ------------------------------------------------------------------------------}
function FixtureTime(const y, mo, d, h, mi: word): TDateTime;
begin
  Result := UniversalTimeToLocal(EncodeDateTime(y, mo, d, h, mi, 0, 0));
end;

procedure TNightscout3TreatmentTests.SetUp;
begin
  FAPI := TNightscout3Probe.Create('http://localhost:1', '');
end;

procedure TNightscout3TreatmentTests.TearDown;
begin
  FreeAndNil(FAPI);
end;

function TNightscout3TreatmentTests.LoadFixture(const AName: string;
  const AExpiryHours: double): string;
var
  raw: TStringList;
  expiresAt: TDateTime;
begin
  raw := TStringList.Create;
  try
    raw.LoadFromFile('tests/fixtures/' + AName);
    Result := raw.Text;
  finally
    raw.Free;
  end;

  // Sensor life is a countdown, so a fixed timestamp in the file would decay
  // into "expired" the day after it was written. The placeholder is filled in
  // as UTC, which is what Nightscout stores and what the driver expects.
  expiresAt := LocalTimeToUniversal(Now) + (AExpiryHours / 24);
  Result := StringReplace(Result, '@SENSOR_EXPIRES@',
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', expiresAt), [rfReplaceAll]);
end;

procedure TNightscout3TreatmentTests.FeedDeviceStatus(const AExpiryHours: double);
begin
  TNightscout3Probe(FAPI).FeedStatus(
    LoadFixture('nightscout3_devicestatus.json', AExpiryHours));
end;

procedure TNightscout3TreatmentTests.FeedTreatments;
begin
  TNightscout3Probe(FAPI).FeedTreatmentList(
    LoadFixture('nightscout3_treatments.json', 0));
end;

{------------------------------------------------------------------------------
  Only records that carry insulin become deliveries, and a loop's own doses are
  marked so they can be kept off the graph separately from the user's.
 ------------------------------------------------------------------------------}
procedure TNightscout3TreatmentTests.TestBolusExtraction;
var
  boluses: TBolusList;
  i: integer;
begin
  AssertTrue('Nightscout advertises bolus support', FAPI.supportsBoluses);

  FeedTreatments;
  AssertTrue('Boluses reported after a fetch', FAPI.getBoluses(boluses));

  // The meal bolus, the loop bolus and the SMB. The temp basal carries no
  // insulin, the null and zero records report none, and the last record has no
  // timestamp to plot it against.
  AssertEquals('Deliveries in the payload', 3, Length(boluses));

  for i := 1 to High(boluses) do
    AssertTrue('Boluses are ordered oldest first',
      boluses[i].time >= boluses[i - 1].time);

  AssertEquals('Meal bolus units', 6.2, boluses[0].units, 0.0001);
  AssertEquals('Meal bolus carries its carbs', 45.0, boluses[0].carbs, 0.0001);
  AssertEquals('Meal bolus kind', 'MEAL BOLUS', boluses[0].kind);
  AssertEquals('Meal bolus time', FixtureTime(2026, 8, 11, 8, 30),
    boluses[0].time, 1 / 86400);
  AssertFalse('A bolus the user gave is not automatic', boluses[0].automatic);

  // Timed by `mills` rather than created_at, and flagged with `automatic`
  AssertEquals('Loop bolus units', 2.5, boluses[1].units, 0.0001);
  AssertEquals('Loop bolus timed from mills', FixtureTime(2026, 8, 11, 9, 0),
    boluses[1].time, 1 / 86400);
  AssertTrue('A dose flagged automatic is automatic', boluses[1].automatic);

  // isSMB is the flag AAPS uses; the event type says only "Correction Bolus"
  AssertEquals('SMB units', 0.15, boluses[2].units, 0.0001);
  AssertTrue('An SMB is automatic', boluses[2].automatic);
  AssertEquals('SMB carries no carbs', 0.0, boluses[2].carbs, 0.0001);
end;

{------------------------------------------------------------------------------
  A Nightscout record carries insulin and carbs together, so a meal bolus feeds
  both lists once — there is nothing to double-count and nothing to reconcile.
 ------------------------------------------------------------------------------}
procedure TNightscout3TreatmentTests.TestCarbExtraction;
var
  carbs: TCarbList;
begin
  AssertTrue('Nightscout advertises carb support', FAPI.supportsCarbs);

  FeedTreatments;
  AssertTrue('Carbs reported after a fetch', FAPI.getCarbs(carbs));

  // The standalone carb correction and the meal bolus's 45 g. The zero record
  // is not a meal and the untimed one cannot be placed.
  AssertEquals('Carb entries in the payload', 2, Length(carbs));

  AssertEquals('Standalone carbs', 15.0, carbs[0].grams, 0.0001);
  AssertEquals('Standalone carb kind', 'CARB CORRECTION', carbs[0].kind);
  AssertEquals('Standalone carb time', FixtureTime(2026, 8, 11, 7, 15),
    carbs[0].time, 1 / 86400);

  AssertEquals('Meal carbs', 45.0, carbs[1].grams, 0.0001);
  AssertEquals('Meal carb kind', 'MEAL BOLUS', carbs[1].kind);
  AssertEquals('Meal carbs share the bolus time', FixtureTime(2026, 8, 11, 8, 30),
    carbs[1].time, 1 / 86400);
end;

{------------------------------------------------------------------------------
  Each field comes from the newest record that carries it, not from the newest
  record: the fixture's newest entry is a phone with nothing but its own
  battery on it, and the pump figures sit two records further down.
 ------------------------------------------------------------------------------}
procedure TNightscout3TreatmentTests.TestDeviceStatus;
var
  status: TCGMDeviceStatus;
begin
  FeedDeviceStatus(6.5);
  AssertTrue('Device status reported', FAPI.getDeviceStatus(status));

  // 42.5 U from the 09:45 rig record, not the 120 U of the 08:00 one
  AssertEquals('Reservoir from the newest record that has one',
    42.5, status.reservoirUnits, 0.0001);
  AssertEquals('Pump battery from the newest record that has one',
    78, status.pumpBatteryPercent);

  // The older record was suspended; the newer one is not, and the newer wins
  AssertFalse('Suspend state from the newest record', status.pumpSuspended);
  AssertEquals('Pump status text', 'normal', status.statusMessage);

  // 6.5 hours out truncates to 6 whole hours of sensor life left
  AssertEquals('Sensor life counts down in whole hours',
    6, status.sensorDurationHours);
  AssertEquals('Sensor state text', 'OK', status.sensorState);
  AssertTrue('A healthy state is not a fault', status.sensorOK);

  // Nightscout has no percentage for the cartridge, and the transmitter figure
  // it carries is a voltage — neither may be invented from what is there.
  AssertEquals('Reservoir percent not reported by Nightscout',
    DEVICE_STATUS_UNKNOWN, status.reservoirPercent);
  AssertEquals('Transmitter battery not derived from a voltage',
    DEVICE_STATUS_UNKNOWN, status.transmitterBatteryPercent);
end;

{------------------------------------------------------------------------------
  A session that ended an hour ago has no life left, not a negative amount —
  a negative would collide with the "not reported" sentinel and silence the
  expiry warning exactly when it is due.
 ------------------------------------------------------------------------------}
procedure TNightscout3TreatmentTests.TestExpiredSensorReportsNoLifeLeft;
var
  status: TCGMDeviceStatus;
begin
  FeedDeviceStatus(-1);
  AssertTrue('Device status reported', FAPI.getDeviceStatus(status));
  AssertEquals('An expired sensor has no hours left',
    0, status.sensorDurationHours);
end;

{------------------------------------------------------------------------------
  uploader.battery is the phone doing the uploading. A flat phone is not a flat
  pump, and reporting it as one would fire the pump-battery warning on a site
  that has no pump at all.
 ------------------------------------------------------------------------------}
procedure TNightscout3TreatmentTests.TestUploaderBatteryIsNotThePumpBattery;
var
  status: TCGMDeviceStatus;
begin
  TNightscout3Probe(FAPI).FeedStatus(
    '[{"device":"phone","created_at":"2026-08-11T09:55:00.000Z",' +
    '"uploader":{"battery":7}}]');

  AssertFalse('A phone battery alone is not a device status',
    FAPI.getDeviceStatus(status));
  AssertEquals('Pump battery stays unreported',
    DEVICE_STATUS_UNKNOWN, status.pumpBatteryPercent);
end;

{------------------------------------------------------------------------------
  An error page, an empty body or a payload of the wrong shape must all leave
  the caches saying "nothing was reported" rather than "nothing is there".
 ------------------------------------------------------------------------------}
procedure TNightscout3TreatmentTests.TestUnreadablePayloadReportsNothing;
var
  boluses: TBolusList;
  carbs: TCarbList;
  status: TCGMDeviceStatus;
begin
  FeedTreatments;
  AssertTrue('Boluses reported after a good payload', FAPI.getBoluses(boluses));

  // A later fetch that fails must not leave the previous window on the graph
  TNightscout3Probe(FAPI).FeedTreatmentList('<html>401 Unauthorized</html>');
  AssertFalse('A broken payload reports nothing', FAPI.getBoluses(boluses));
  AssertEquals('Bolus list is empty', 0, Length(boluses));
  AssertFalse('A broken payload reports no carbs', FAPI.getCarbs(carbs));
  AssertEquals('Carb list is empty', 0, Length(carbs));

  FeedDeviceStatus(6.5);
  AssertTrue('Device status reported after a good payload',
    FAPI.getDeviceStatus(status));
  TNightscout3Probe(FAPI).FeedStatus('');
  AssertFalse('An empty body reports no device status',
    FAPI.getDeviceStatus(status));
  AssertEquals('Reservoir is unknown again',
    DEVICE_STATUS_UNKNOWN, status.reservoirUnits, 0.0001);
end;

{------------------------------------------------------------------------------
  Before any payload has been walked, "nothing reported" must be
  distinguishable from "no insulin given".
 ------------------------------------------------------------------------------}
procedure TNightscout3TreatmentTests.TestNothingReportedBeforeAFetch;
var
  boluses: TBolusList;
  carbs: TCarbList;
  status: TCGMDeviceStatus;
begin
  AssertFalse('No boluses before a fetch', FAPI.getBoluses(boluses));
  AssertEquals('Bolus list is empty', 0, Length(boluses));
  AssertFalse('No carbs before a fetch', FAPI.getCarbs(carbs));
  AssertEquals('Carb list is empty', 0, Length(carbs));
  AssertFalse('No device status before a fetch', FAPI.getDeviceStatus(status));
  AssertEquals('Sensor life is unknown',
    DEVICE_STATUS_UNKNOWN, status.sensorDurationHours);
end;

initialization
  RegisterTest(TNightscout3TreatmentTests);

end.
