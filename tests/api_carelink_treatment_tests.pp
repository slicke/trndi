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
unit api_carelink_treatment_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser, dateutils,
  trndi.api, trndi.api.carelink;

type
  (*****************************************************************************
    CareLink display-message payload extraction.

    The fixture is a synthetic display/message payload built to the shape of a
    real EU account's response (780G in SmartGuard). It runs entirely offline:
    reaching the real endpoint would mean standing up endpoint discovery and
    the token-refresh flow against a fake server, and the part worth testing is
    what the payload is turned into, not how it was fetched.

    The fixture's timestamps are fixed, so anything the driver computes
    relative to now cannot be asserted here — the auto-basal rate sums a
    one-hour window, and timeDiff is measured against the local clock. Those
    live in api_carelink_tests, against the embedded server's Now-relative
    payload. What is asserted here is everything the payload states outright.
   ****************************************************************************)
  TCareLinkTreatmentTests = class(TTestCase)
  private
    FAPI: CareLink;
    procedure FeedFixture;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLimitsFromSchedule;
    procedure TestBolusExtraction;
    procedure TestCarbExtraction;
    procedure TestDeviceStatus;
    procedure TestStalePayloadReportsNoBasalRate;
    procedure TestNothingReportedBeforeAFetch;
  end;

implementation

type
  {** Exposes the protected payload entry point so a fixture can be pushed
      straight in. }
  TCareLinkProbe = class(CareLinkEU)
  public
    procedure Feed(AData: TJSONData);
  end;

procedure TCareLinkProbe.Feed(AData: TJSONData);
begin
  // Goes through PayloadRoot as the real fetch does, so the "patientData"
  // unwrapping is exercised rather than bypassed.
  ApplyPayloadMetadata(PayloadRoot(AData));
end;

procedure TCareLinkTreatmentTests.SetUp;
begin
  FAPI := TCareLinkProbe.Create('', '{"refresh_token":"r","client_id":"c"}');
end;

procedure TCareLinkTreatmentTests.TearDown;
begin
  FreeAndNil(FAPI);
end;

{------------------------------------------------------------------------------
  Parse the fixture and hand it to the payload extractor.
 ------------------------------------------------------------------------------}
procedure TCareLinkTreatmentTests.FeedFixture;
var
  raw: TStringList;
  data: TJSONData;
begin
  raw := TStringList.Create;
  try
    raw.LoadFromFile('tests/fixtures/carelink_display_message.json');
    data := GetJSON(raw.Text);
    try
      if not (data is TJSONObject) then
        Fail('Fixture is not a JSON object');
      TCareLinkProbe(FAPI).Feed(data);
    finally
      data.Free;
    end;
  finally
    raw.Free;
  end;
end;

{------------------------------------------------------------------------------
  The limits are a schedule, not a single pair. The block in force is the
  newest one that has already started.
 ------------------------------------------------------------------------------}
procedure TCareLinkTreatmentTests.TestLimitsFromSchedule;
begin
  // Before the fetch the base class's placeholders are in place, so the test
  // cannot pass by accident on a payload that was never read.
  AssertEquals('High placeholder before a fetch', 401, FAPI.cgmHi);
  AssertEquals('Low placeholder before a fetch', 40, FAPI.cgmLo);

  FeedFixture;

  // 00:00 sets 250/70 and 07:00 replaces it with 234/65; the later block wins.
  AssertEquals('High limit from the block in force', 234, FAPI.cgmHi);
  AssertEquals('Low limit from the block in force', 65, FAPI.cgmLo);
end;

{------------------------------------------------------------------------------
  Insulin reaches the markers list as two different types, and only one of them
  distinguishes a dose the user asked for from one the pump decided on.
 ------------------------------------------------------------------------------}
procedure TCareLinkTreatmentTests.TestBolusExtraction;
var
  boluses: TBolusList;
  i, autoCount, manualCount: integer;
begin
  AssertTrue('CareLink advertises bolus support', FAPI.supportsBoluses);

  FeedFixture;
  AssertTrue('Boluses reported after a fetch', FAPI.getBoluses(boluses));

  // Three auto-basal markers and two usable INSULIN markers. The 0 U INSULIN
  // marker records a delivery that did not happen, and the MEAL, CALIBRATION
  // and AUTO_MODE_STATUS markers are not insulin at all.
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
  // Three auto-basal deliveries plus the AUTOCORRECTION bolus.
  AssertEquals('Automatic deliveries', 4, autoCount);
  AssertEquals('User-requested deliveries', 1, manualCount);

  // Oldest first, even though the fixture lists the 19:00 marker last.
  for i := 1 to High(boluses) do
    AssertTrue('Boluses are ordered oldest first',
      boluses[i].time >= boluses[i - 1].time);
  AssertEquals('Oldest delivery is the 19:00 auto-basal',
    EncodeDateTime(2026, 8, 10, 19, 0, 0, 0), boluses[0].time, 1 / 86400);

  // The meal bolus: deliveredFastAmount, its carbs carried along, and not
  // automatic despite arriving in the same marker type as one that is.
  AssertEquals('Meal bolus units', 6.2, boluses[3].units, 0.0001);
  AssertEquals('Meal bolus carbs', 45.0, boluses[3].carbs, 0.0001);
  AssertFalse('Meal bolus is not automatic', boluses[3].automatic);
  AssertEquals('Meal bolus timed from the marker',
    EncodeDateTime(2026, 8, 10, 20, 30, 0, 0), boluses[3].time, 1 / 86400);

  // An AUTOCORRECTION is the pump acting on its own; only activationType
  // separates it from the bolus above.
  AssertEquals('Auto-correction amount', 1.25, boluses[4].units, 0.0001);
  AssertTrue('Auto-correction is automatic', boluses[4].automatic);
  AssertEquals('Auto-correction carries no carbs', 0.0, boluses[4].carbs, 0.0001);
end;

{------------------------------------------------------------------------------
  A meal that was bolused for is reported twice — once as a meal marker and
  once as the carb figure on the bolus. It is one meal and must be drawn once.
 ------------------------------------------------------------------------------}
procedure TCareLinkTreatmentTests.TestCarbExtraction;
var
  carbs: TCarbList;
  boluses: TBolusList;
  i: integer;
begin
  AssertTrue('CareLink advertises carb support', FAPI.supportsCarbs);

  FeedFixture;
  AssertTrue('Carbs reported after a fetch', FAPI.getCarbs(carbs));

  // A 45 g meal marker at 20:29 and the 20:30 bolus's carbInput of 45 are one
  // meal; the 30 g marker at 21:40 stands alone. Three sources, two entries.
  AssertEquals('Carb entries after reconciling', 2, Length(carbs));

  for i := 0 to High(carbs) do
    AssertTrue('No zero-gram entry survives', carbs[i].grams > 0);

  for i := 1 to High(carbs) do
    AssertTrue('Carb entries are ordered oldest first',
      carbs[i].time >= carbs[i - 1].time);

  // The merged meal keeps the meal marker's own time and amount, not the
  // bolus's — the marker is the record of what was eaten.
  AssertEquals('Bolused meal counted once', 45.0, carbs[0].grams, 0.0001);
  AssertEquals('Merged meal keeps the marker time',
    EncodeDateTime(2026, 8, 10, 20, 29, 0, 0), carbs[0].time, 1 / 86400);
  AssertEquals('Standalone meal marker', 30.0, carbs[1].grams, 0.0001);

  // Reconciling happens in the carb list; the bolus record is left intact.
  AssertTrue('Boluses still available', FAPI.getBoluses(boluses));
  AssertEquals('Bolus keeps its own carb figure',
    45.0, boluses[3].carbs, 0.0001);
end;

{------------------------------------------------------------------------------
  Housekeeping the payload states outright, plus the one field that looks like
  a reading but is not.
 ------------------------------------------------------------------------------}
procedure TCareLinkTreatmentTests.TestDeviceStatus;
var
  status: TCGMDeviceStatus;
begin
  FeedFixture;
  AssertTrue('Device status reported', FAPI.getDeviceStatus(status));

  AssertEquals('Sensor duration hours', 126, status.sensorDurationHours);
  AssertEquals('Reservoir percent', 5, status.reservoirPercent);
  AssertEquals('Reservoir units', 15.4, status.reservoirUnits, 0.01);
  AssertEquals('Pump battery percent', 50, status.pumpBatteryPercent);
  AssertFalse('Pump not suspended', status.pumpSuspended);
  AssertTrue('Sensor reports no fault', status.sensorOK);
  AssertEquals('Sensor state carried through',
    'NO_ERROR_MESSAGE', status.sensorState);
  AssertEquals('No status message when the payload says NO_ERROR_MESSAGE',
    '', status.statusMessage);

  // gstBatteryLevel 255 is the device's "no reading" marker, not a 255%
  // battery, and must come back as unknown rather than as a full one.
  AssertEquals('Transmitter battery unknown, not 255',
    DEVICE_STATUS_UNKNOWN, status.transmitterBatteryPercent);

  // IOB is stated by the payload rather than derived, so it is assertable here.
  AssertEquals('Active insulin', 2.5, FAPI.ActiveInsulin, 0.0001);
end;

{------------------------------------------------------------------------------
  The auto-basal figure is the last hour's deliveries, so a payload whose
  markers are all older than that has nothing to report. Answering 0 rather
  than summing whatever is in the list keeps a stale payload from presenting
  yesterday's delivery as a current rate.
 ------------------------------------------------------------------------------}
procedure TCareLinkTreatmentTests.TestStalePayloadReportsNoBasalRate;
begin
  FeedFixture;
  AssertEquals('No basal rate from markers older than the window',
    0.0, FAPI.getBasalRate, 0.0001);

  // The deliveries themselves are still there: they happened, even though they
  // no longer say anything about the current rate.
  AssertTrue('Old deliveries still reported as deliveries',
    FAPI.supportsBoluses);
end;

{------------------------------------------------------------------------------
  Before any payload has been walked, "nothing reported" must be
  distinguishable from "no insulin given" — the accessors return False rather
  than an empty list a caller could read as a quiet night.
 ------------------------------------------------------------------------------}
procedure TCareLinkTreatmentTests.TestNothingReportedBeforeAFetch;
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
  AssertEquals('No basal rate before a fetch', 0.0, FAPI.getBasalRate, 0.0001);
end;

initialization
  RegisterTest(TCareLinkTreatmentTests);

end.
