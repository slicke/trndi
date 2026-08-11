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
unit api_tandem_treatment_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser, dateutils,
  trndi.api, trndi.api.tandem;

type
  (*****************************************************************************
    Tandem pump-logs treatment extraction.

    The fixture is a synthetic pump-logs payload built to the shape of a real
    EU account's response (t:slim X2, Dexcom G7, Control-IQ). It runs entirely
    offline: reaching the real endpoint would mean standing up the whole OIDC
    flow against a fake server, and the part worth testing is what the events
    are turned into, not how they were fetched.
   ****************************************************************************)
  TTandemTreatmentTests = class(TTestCase)
  private
    FAPI: Tandem;
    procedure FeedFixture;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBolusExtraction;
    procedure TestCarbExtraction;
    procedure TestDeviceStatus;
    procedure TestBasalRate;
    procedure TestNothingReportedBeforeAFetch;
  end;

implementation

type
  {** Exposes the protected extractor so a fixture payload can be pushed
      straight in. }
  TTandemProbe = class(TandemEU)
  public
    procedure Feed(AEvents: TJSONArray);
  end;

procedure TTandemProbe.Feed(AEvents: TJSONArray);
begin
  ExtractTreatments(AEvents);
end;

procedure TTandemTreatmentTests.SetUp;
begin
  FAPI := TTandemProbe.Create('test@example.com', 'unused');
end;

procedure TTandemTreatmentTests.TearDown;
begin
  FreeAndNil(FAPI);
end;

{------------------------------------------------------------------------------
  Parse the fixture and hand its events array to the extractor.
 ------------------------------------------------------------------------------}
procedure TTandemTreatmentTests.FeedFixture;
var
  raw: TStringList;
  data: TJSONData;
begin
  raw := TStringList.Create;
  try
    raw.LoadFromFile('tests/fixtures/tandem_pumplogs.json');
    data := GetJSON(raw.Text);
    try
      if not (data is TJSONObject) then
        Fail('Fixture is not a JSON object');
      if not (TJSONObject(data).Find('events') is TJSONArray) then
        Fail('Fixture has no events array');
      TTandemProbe(FAPI).Feed(TJSONArray(TJSONObject(data).Find('events')));
    finally
      data.Free;
    end;
  finally
    raw.Free;
  end;
end;

{------------------------------------------------------------------------------
  A bolus is spread over several events and only the completion says what
  actually went in. The figures below all come from a different event of the
  same bolusId, which is the point of the assembly step.
 ------------------------------------------------------------------------------}
procedure TTandemTreatmentTests.TestBolusExtraction;
var
  boluses: TBolusList;
  i: integer;
begin
  AssertTrue('Tandem advertises bolus support', FAPI.supportsBoluses);

  FeedFixture;
  AssertTrue('Boluses reported after a fetch', FAPI.getBoluses(boluses));

  // 1001 (meal), 1002 (interrupted correction) and 1005 (no completion event).
  // 1003 delivered 0 U and 1004 never got past the request, so neither is a
  // delivery that happened.
  AssertEquals('Deliveries that actually happened', 3, Length(boluses));

  // Oldest first, even though the fixture lists 1005's events first
  for i := 1 to High(boluses) do
    AssertTrue('Boluses are ordered oldest first',
      boluses[i].time >= boluses[i - 1].time);

  // 1001: delivered in full, timed from the activation rather than the
  // completion two and a half minutes later
  AssertEquals('Meal bolus units', 6.625, boluses[0].units, 0.0001);
  AssertEquals('Meal bolus carbs', 45.0, boluses[0].carbs, 0.0001);
  AssertEquals('Meal bolus kind', 'MEAL', boluses[0].kind);
  AssertEquals('Meal bolus timed from activation',
    EncodeDateTime(2026, 8, 10, 8, 0, 15, 0), boluses[0].time, 1 / 86400);

  // 1002: asked for 2.0 U, delivered 0.75. What went in is what the glucose
  // curve reflects, so the delivered figure must win over the requested one.
  AssertEquals('Interrupted bolus reports what was delivered',
    0.75, boluses[1].units, 0.0001);
  AssertEquals('Correction carries no carbs', 0.0, boluses[1].carbs, 0.0001);
  AssertEquals('Correction bolus kind', 'CORRECTION', boluses[1].kind);

  // 1005 has no completion event; the requested total stands in for it
  AssertEquals('Bolus without a completion falls back to the total',
    3.0, boluses[2].units, 0.0001);
  AssertEquals('Food bolus kind', 'FOOD', boluses[2].kind);

  // Control-IQ's automatic corrections are not yet distinguishable in the
  // payload, so nothing may be silently filed as automatic and hidden behind
  // the off-by-default overlay.
  for i := 0 to High(boluses) do
    AssertFalse('No Tandem bolus is reported automatic yet',
      boluses[i].automatic);
end;

{------------------------------------------------------------------------------
  Carbohydrates come only from the bolus calculator, so a bolus without carbs
  must not produce a phantom meal, and carbs entered for a bolus that never
  happened are not a meal we can vouch for either.
 ------------------------------------------------------------------------------}
procedure TTandemTreatmentTests.TestCarbExtraction;
var
  carbs: TCarbList;
begin
  AssertTrue('Tandem advertises carb support', FAPI.supportsCarbs);

  FeedFixture;
  AssertTrue('Carbs reported after a fetch', FAPI.getCarbs(carbs));

  // Only 1001. 1002 and 1005 carry no carbs; 1004's 30 g belong to a bolus
  // that never reached delivery.
  AssertEquals('Carb entries', 1, Length(carbs));
  AssertEquals('Meal size in grams', 45.0, carbs[0].grams, 0.0001);
  AssertEquals('Carb entry kind', 'MEAL', carbs[0].kind);
  AssertEquals('Carb entry shares the bolus time',
    EncodeDateTime(2026, 8, 10, 8, 0, 15, 0), carbs[0].time, 1 / 86400);
end;

{------------------------------------------------------------------------------
  Reservoir, battery and suspend state each come from the most recent event
  that carries them, and a field the pump could not read is absent rather than
  a reading of 255.
 ------------------------------------------------------------------------------}
procedure TTandemTreatmentTests.TestDeviceStatus;
var
  status: TCGMDeviceStatus;
begin
  FeedFixture;
  AssertTrue('Device status reported', FAPI.getDeviceStatus(status));

  // Latest code 9 is 21:00 (36 U), not the 06:00 one (120 U)
  AssertEquals('Reservoir from the last pump-state event',
    36.0, status.reservoirUnits, 0.0001);

  // The 22:00 battery-detail event is later than the 21:00 pump state, so its
  // ibc wins; the 05:00 event's 255 is a no-reading marker and must lose to
  // both rather than becoming a 255% battery.
  AssertEquals('Battery from the latest event that carries one',
    35, status.pumpBatteryPercent);

  AssertTrue('Suspend state from the last control-mode change',
    status.pumpSuspended);

  // Nothing in the payload reports these, so they must stay unknown rather
  // than defaulting to zero.
  AssertEquals('Sensor life not reported by Tandem',
    DEVICE_STATUS_UNKNOWN, status.sensorDurationHours);
  AssertEquals('Transmitter battery not reported by Tandem',
    DEVICE_STATUS_UNKNOWN, status.transmitterBatteryPercent);
  AssertEquals('Reservoir percent not reported by Tandem',
    DEVICE_STATUS_UNKNOWN, status.reservoirPercent);
  AssertTrue('No fault information means no fault', status.sensorOK);
end;

{------------------------------------------------------------------------------
  The Control-IQ rate command is in milliunits per hour, unlike the float U/hr
  of the basal-change event, and the later of the two must win.
 ------------------------------------------------------------------------------}
procedure TTandemTreatmentTests.TestBasalRate;
begin
  FeedFixture;

  // 23:30 commandedRate 1250 mU/hr beats the 00:00 commandedBasalRate of 3.653
  AssertEquals('Commanded rate scaled from milliunits',
    1.25, FAPI.getBasalRate, 0.0001);

  // The overlay wants a repeating daily schedule; one fetch window is not one
  AssertFalse('Tandem has no basal profile to draw', FAPI.supportsBasal);
end;

{------------------------------------------------------------------------------
  Before any payload has been walked, "nothing reported" must be distinguishable
  from "no insulin given" -- the accessors return False rather than an empty
  list that a caller could read as a quiet night.
 ------------------------------------------------------------------------------}
procedure TTandemTreatmentTests.TestNothingReportedBeforeAFetch;
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
  RegisterTest(TTandemTreatmentTests);

end.
