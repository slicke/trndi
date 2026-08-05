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
unit alert_engine_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  fpcunit, testregistry,
  trndi.alert.engine;

type
  {** Tests for the rule-based alert engine.

    The engine reads the wall clock internally, so time-dependent paths
    (re-alert intervals, minimum duration, snooze expiry) are exercised by
    seeding past timestamps through DeserializeState rather than by sleeping —
    the suite stays deterministic and instant. }
  TAlertEngineTests = class(TTestCase)
  private
    FEngine: TAlertEngine;
    FChangeCount: integer;
    procedure HandleStateChanged(Sender: TObject);
    {** Build one DeserializeState entry with explicit timestamps. }
    function StateEntry(const AKind: TAlertKind;
      const ASnoozedUntil, ALastFired, AViolationStartedAt: TDateTime;
      const AViolating: boolean): string;
    {** Minutes from now until a single rule's snooze deadline, read back out of
      SerializeState. ActiveSnoozeUntil only exposes the maximum across all
      rules, so per-rule deadlines have to come from the serialized form.
      Returns -1 when the rule has no entry. }
    function SnoozeMinutesFor(const AKind: TAlertKind): integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Hysteresis
    procedure HighHysteresisHoldsBandUntilExit;
    procedure LowHysteresisHoldsBandUntilExit;
    procedure ZeroHysteresisClearsAtThreshold;
    // Level rules
    procedure UrgentLowSubsumesLow;
    procedure ReturnToRangeResetsLevelState;
    procedure DisabledRuleNeverFires;
    // Re-alerting
    procedure OneShotDoesNotRefireWithinExcursion;
    procedure ReAlertHoldsBeforeInterval;
    procedure ReAlertRefiresAfterInterval;
    // Minimum duration
    procedure MinDurationSuppressesFirstFire;
    procedure MinDurationFiresOncePersisted;
    // Snooze
    procedure SnoozeActiveCapsUrgentLow;
    procedure SnoozeActiveHonoursUncappedRule;
    procedure SnoozeAllCannotBypassCap;
    procedure SnoozeActiveOnlyTouchesViolatingRules;
    procedure SnoozeClearsLastFiredSoAlertReturns;
    procedure ResumeAllClearsSnoozes;
    // Delta, missing, sensor fault
    procedure RapidFallFiresAndClears;
    procedure RapidRiseFiresAndClears;
    procedure MissingFiresOnceUntilReset;
    procedure SensorFaultFiresOnceUntilReset;
    // State persistence
    procedure SerializeSkipsCleanRules;
    procedure StateRoundTripPreservesSnoozeAndViolation;
    procedure StaleViolationIsDropped;
    procedure ExpiredSnoozeIsDroppedOnLoad;
    procedure MalformedStateIsIgnored;
    procedure EmptyStateIsNoOp;
    // Notification
    procedure OnStateChangedFiresOnViolation;
  end;

implementation

// ---------------------------------------------------------------------------
// Fixture
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.SetUp;
begin
  inherited SetUp;
  FEngine := TAlertEngine.Create;
  FChangeCount := 0;
end;

procedure TAlertEngineTests.TearDown;
begin
  FreeAndNil(FEngine);
  inherited TearDown;
end;

procedure TAlertEngineTests.HandleStateChanged(Sender: TObject);
begin
  Inc(FChangeCount);
end;

// Mirrors SerializeState's field layout so tests can inject arbitrary history.
function TAlertEngineTests.StateEntry(const AKind: TAlertKind;
  const ASnoozedUntil, ALastFired, AViolationStartedAt: TDateTime;
  const AViolating: boolean): string;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  Result := Format('%d|%s|%s|%s|%d',
    [Ord(AKind),
     FloatToStr(ASnoozedUntil, fs),
     FloatToStr(ALastFired, fs),
     FloatToStr(AViolationStartedAt, fs),
     Ord(AViolating)]);
end;

function TAlertEngineTests.SnoozeMinutesFor(const AKind: TAlertKind): integer;
var
  entries, fields: TStringList;
  fs: TFormatSettings;
  i, kindOrd: integer;
begin
  Result := -1;
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;

  entries := TStringList.Create;
  fields := TStringList.Create;
  try
    entries.StrictDelimiter := true;
    entries.Delimiter := ',';
    entries.DelimitedText := FEngine.SerializeState;
    for i := 0 to entries.Count - 1 do
    begin
      fields.Clear;
      fields.StrictDelimiter := true;
      fields.Delimiter := '|';
      fields.DelimitedText := entries[i];
      if fields.Count < 5 then Continue;
      if not TryStrToInt(fields[0], kindOrd) then Continue;
      if kindOrd <> Ord(AKind) then Continue;
      Exit(MinutesBetween(StrToFloatDef(fields[1], 0, fs), Now));
    end;
  finally
    fields.Free;
    entries.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Hysteresis
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.HighHysteresisHoldsBandUntilExit;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0, 0, 0, 1.0);

  res := FEngine.EvaluateLevel(10.5);
  AssertTrue('high should fire on entry', akHigh in res);

  // Below the threshold but inside the hysteresis band: stays in violation.
  res := FEngine.EvaluateLevel(9.5);
  AssertTrue('must not re-fire inside band', not (akHigh in res));
  AssertTrue('must stay violating inside band', FEngine.IsViolating(akHigh));

  // Past the band: excursion ends.
  FEngine.EvaluateLevel(8.5);
  AssertTrue('must clear below the band', not FEngine.IsViolating(akHigh));
end;

procedure TAlertEngineTests.LowHysteresisHoldsBandUntilExit;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akLow, true, 4.0, 0, 0, 0, 0.5);

  res := FEngine.EvaluateLevel(3.5);
  AssertTrue('low should fire on entry', akLow in res);

  res := FEngine.EvaluateLevel(4.3);
  AssertTrue('must not re-fire inside band', not (akLow in res));
  AssertTrue('must stay violating inside band', FEngine.IsViolating(akLow));

  FEngine.EvaluateLevel(4.6);
  AssertTrue('must clear above the band', not FEngine.IsViolating(akLow));
end;

procedure TAlertEngineTests.ZeroHysteresisClearsAtThreshold;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0, 0, 0, 0);

  FEngine.EvaluateLevel(12.0);
  AssertTrue('high should be violating', FEngine.IsViolating(akHigh));

  FEngine.EvaluateLevel(9.9);
  AssertTrue('zero hysteresis clears at the threshold',
    not FEngine.IsViolating(akHigh));
end;

// ---------------------------------------------------------------------------
// Level rules
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.UrgentLowSubsumesLow;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akLow, true, 4.0, 0);
  FEngine.SetupRule(akUrgentLow, true, 3.0, 0);

  res := FEngine.EvaluateLevel(2.5);
  AssertTrue('urgent low must fire', akUrgentLow in res);
  AssertTrue('regular low must not fire alongside urgent',
    not (akLow in res));
  AssertTrue('regular low must not be left violating',
    not FEngine.IsViolating(akLow));

  // Climbing back into the regular-low band starts a fresh low excursion.
  res := FEngine.EvaluateLevel(3.5);
  AssertTrue('low fires as a fresh excursion', akLow in res);
  AssertTrue('urgent low must clear', not FEngine.IsViolating(akUrgentLow));
end;

procedure TAlertEngineTests.ReturnToRangeResetsLevelState;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('first excursion fires', akHigh in res);

  FEngine.EvaluateLevel(6.0);
  AssertTrue('in range clears violation', not FEngine.IsViolating(akHigh));

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('second excursion fires again', akHigh in res);
end;

procedure TAlertEngineTests.DisabledRuleNeverFires;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, false, 10.0, 0);

  res := FEngine.EvaluateLevel(999.0);
  AssertTrue('disabled rule must not fire', not (akHigh in res));
  AssertTrue('disabled rule must not violate', not FEngine.IsViolating(akHigh));
end;

// ---------------------------------------------------------------------------
// Re-alerting
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.OneShotDoesNotRefireWithinExcursion;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('first evaluation fires', akHigh in res);

  res := FEngine.EvaluateLevel(13.0);
  AssertTrue('one-shot rule must not re-fire', not (akHigh in res));
end;

procedure TAlertEngineTests.ReAlertHoldsBeforeInterval;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 30);
  // Fired 5 minutes ago — well inside the 30 minute re-alert interval.
  FEngine.DeserializeState(StateEntry(akHigh, 0,
    IncMinute(Now, -5), IncMinute(Now, -20), true), 12);

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('must not re-fire before the interval elapses',
    not (akHigh in res));
  AssertTrue('must remain violating', FEngine.IsViolating(akHigh));
end;

procedure TAlertEngineTests.ReAlertRefiresAfterInterval;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 30);
  // Fired 45 minutes ago — past the 30 minute re-alert interval.
  FEngine.DeserializeState(StateEntry(akHigh, 0,
    IncMinute(Now, -45), IncMinute(Now, -60), true), 12);

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('must re-fire once the interval elapses', akHigh in res);
end;

// ---------------------------------------------------------------------------
// Minimum duration
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.MinDurationSuppressesFirstFire;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0, 0, 15, 0);

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('must not fire before the violation persists',
    not (akHigh in res));
  AssertTrue('violation is tracked while waiting',
    FEngine.IsViolating(akHigh));
end;

procedure TAlertEngineTests.MinDurationFiresOncePersisted;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0, 0, 15, 0);
  // Violating for 30 minutes, never fired.
  FEngine.DeserializeState(StateEntry(akHigh, 0, 0,
    IncMinute(Now, -30), true), 12);

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('must fire once the violation has persisted', akHigh in res);
end;

// ---------------------------------------------------------------------------
// Snooze
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.SnoozeActiveCapsUrgentLow;
var
  mins: integer;
begin
  FEngine.SetupRule(akUrgentLow, true, 3.0, 0, 15);

  FEngine.EvaluateLevel(2.5);
  FEngine.SnoozeActive(120);

  AssertTrue('urgent low is snoozed', FEngine.IsSnoozed(akUrgentLow));
  mins := SnoozeMinutesFor(akUrgentLow);
  AssertTrue('snooze must be capped at MaxSnoozeMinutes, got ' + IntToStr(mins),
    (mins >= 13) and (mins <= 15));
end;

procedure TAlertEngineTests.SnoozeActiveHonoursUncappedRule;
var
  mins: integer;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0, 0);

  FEngine.EvaluateLevel(12.0);
  FEngine.SnoozeActive(120);

  AssertTrue('high is snoozed', FEngine.IsSnoozed(akHigh));
  mins := MinutesBetween(FEngine.ActiveSnoozeUntil, Now);
  AssertTrue('uncapped rule gets the full snooze, got ' + IntToStr(mins),
    (mins >= 118) and (mins <= 120));
end;

procedure TAlertEngineTests.SnoozeAllCannotBypassCap;
var
  capped, uncapped: integer;
begin
  FEngine.SetupRule(akUrgentLow, true, 3.0, 0, 15);
  FEngine.SetupRule(akHigh, true, 10.0, 0, 0);

  // SnoozeAll ignores violation state, so it is the path most likely to let a
  // long snooze slip past the urgent-low cap.
  FEngine.SnoozeAll(120);

  capped := SnoozeMinutesFor(akUrgentLow);
  uncapped := SnoozeMinutesFor(akHigh);

  AssertTrue('urgent low must be snoozed', FEngine.IsSnoozed(akUrgentLow));
  AssertTrue('SnoozeAll must respect the cap, got ' + IntToStr(capped),
    (capped >= 0) and (capped <= 15));
  AssertTrue('uncapped rule keeps the full snooze, got ' + IntToStr(uncapped),
    (uncapped >= 118) and (uncapped <= 120));
end;

procedure TAlertEngineTests.SnoozeActiveOnlyTouchesViolatingRules;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);
  FEngine.SetupRule(akLow, true, 4.0, 0);

  FEngine.EvaluateLevel(12.0);
  FEngine.SnoozeActive(60);

  AssertTrue('violating rule is snoozed', FEngine.IsSnoozed(akHigh));
  AssertTrue('non-violating rule is untouched', not FEngine.IsSnoozed(akLow));
end;

procedure TAlertEngineTests.SnoozeClearsLastFiredSoAlertReturns;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('first evaluation fires', akHigh in res);
  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('one-shot holds', not (akHigh in res));

  // A zero-minute snooze lapses immediately but still clears LastFired, so the
  // alert returns rather than staying silent for the rest of the excursion.
  FEngine.SnoozeActive(0);
  AssertTrue('zero snooze is already lapsed', not FEngine.IsSnoozed(akHigh));

  res := FEngine.EvaluateLevel(12.0);
  AssertTrue('alert returns after the snooze lapses', akHigh in res);
end;

procedure TAlertEngineTests.ResumeAllClearsSnoozes;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);

  FEngine.EvaluateLevel(12.0);
  FEngine.SnoozeActive(60);
  AssertTrue('snoozed before resume', FEngine.AnySnoozed);

  FEngine.ResumeAll;
  AssertTrue('ResumeAll clears every snooze', not FEngine.AnySnoozed);
  AssertEquals('no active snooze remains', 0.0, FEngine.ActiveSnoozeUntil, 0.0);
end;

// ---------------------------------------------------------------------------
// Delta, missing, sensor fault
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.RapidFallFiresAndClears;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akRapidFall, true, -0.15, 0);

  res := FEngine.EvaluateDelta(-0.20);
  AssertTrue('rapid fall fires', akRapidFall in res);

  res := FEngine.EvaluateDelta(-0.20);
  AssertTrue('one-shot within the same excursion', not (akRapidFall in res));

  FEngine.EvaluateDelta(0.0);
  AssertTrue('clears when the delta recovers',
    not FEngine.IsViolating(akRapidFall));

  res := FEngine.EvaluateDelta(-0.20);
  AssertTrue('fires again on a new excursion', akRapidFall in res);
end;

procedure TAlertEngineTests.RapidRiseFiresAndClears;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akRapidRise, true, 0.15, 0);

  res := FEngine.EvaluateDelta(0.20);
  AssertTrue('rapid rise fires', akRapidRise in res);

  FEngine.EvaluateDelta(0.0);
  AssertTrue('clears when the delta recovers',
    not FEngine.IsViolating(akRapidRise));

  res := FEngine.EvaluateDelta(0.20);
  AssertTrue('fires again on a new excursion', akRapidRise in res);
end;

procedure TAlertEngineTests.MissingFiresOnceUntilReset;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akMissing, true, 0, 0);

  res := FEngine.EvaluateMissing;
  AssertTrue('missing fires', akMissing in res);

  res := FEngine.EvaluateMissing;
  AssertTrue('missing does not repeat while unresolved',
    not (akMissing in res));

  FEngine.ResetMissing;
  res := FEngine.EvaluateMissing;
  AssertTrue('missing fires again after readings resume then stop',
    akMissing in res);
end;

procedure TAlertEngineTests.SensorFaultFiresOnceUntilReset;
var
  res: TAlertKindSet;
begin
  FEngine.SetupRule(akSensorFault, true, 0, 0);

  res := FEngine.EvaluateSensorFault;
  AssertTrue('sensor fault fires', akSensorFault in res);

  res := FEngine.EvaluateSensorFault;
  AssertTrue('sensor fault does not repeat while unresolved',
    not (akSensorFault in res));

  FEngine.ResetSensorFault;
  res := FEngine.EvaluateSensorFault;
  AssertTrue('sensor fault fires again after a reset', akSensorFault in res);
end;

// ---------------------------------------------------------------------------
// State persistence
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.SerializeSkipsCleanRules;
var
  s: string;
begin
  AssertEquals('a clean engine serializes to nothing', '',
    FEngine.SerializeState);

  FEngine.SetupRule(akHigh, true, 10.0, 0);
  FEngine.EvaluateLevel(12.0);

  s := FEngine.SerializeState;
  AssertTrue('a fired rule is serialized', s <> '');
  AssertTrue('only the dirty rule is written', Pos(',', s) = 0);
end;

procedure TAlertEngineTests.StateRoundTripPreservesSnoozeAndViolation;
var
  s: string;
  restored: TAlertEngine;
  mins: integer;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0, 0);
  FEngine.EvaluateLevel(12.0);
  FEngine.SnoozeActive(60);
  s := FEngine.SerializeState;

  restored := TAlertEngine.Create;
  try
    restored.SetupRule(akHigh, true, 10.0, 0, 0);
    restored.DeserializeState(s, 12);

    AssertTrue('violation survives the round trip',
      restored.IsViolating(akHigh));
    AssertTrue('snooze survives the round trip', restored.IsSnoozed(akHigh));
    mins := MinutesBetween(restored.ActiveSnoozeUntil, Now);
    AssertTrue('snooze deadline survives intact, got ' + IntToStr(mins),
      (mins >= 58) and (mins <= 60));
  finally
    restored.Free;
  end;
end;

procedure TAlertEngineTests.StaleViolationIsDropped;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);
  // A violation that started a day ago is stale against a 12 hour cap.
  FEngine.DeserializeState(StateEntry(akHigh, 0,
    IncHour(Now, -24), IncHour(Now, -24), true), 12);

  AssertTrue('stale violation is dropped', not FEngine.IsViolating(akHigh));
  AssertEquals('stale state is not re-serialized', '', FEngine.SerializeState);
end;

procedure TAlertEngineTests.ExpiredSnoozeIsDroppedOnLoad;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);
  FEngine.DeserializeState(StateEntry(akHigh,
    IncMinute(Now, -5), 0, 0, false), 12);

  AssertTrue('an expired snooze does not survive the load',
    not FEngine.IsSnoozed(akHigh));
  AssertTrue('no rule reports as snoozed', not FEngine.AnySnoozed);
end;

procedure TAlertEngineTests.MalformedStateIsIgnored;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);

  FEngine.DeserializeState('garbage', 12);
  FEngine.DeserializeState('99|0|0|0|1', 12);
  FEngine.DeserializeState('|||', 12);
  FEngine.DeserializeState('notanumber|0|0|0|1', 12);

  AssertTrue('malformed input leaves no snooze', not FEngine.AnySnoozed);
  AssertTrue('malformed input leaves no violation',
    FEngine.ViolatingKinds = []);
end;

procedure TAlertEngineTests.EmptyStateIsNoOp;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);
  FEngine.EvaluateLevel(12.0);

  FEngine.DeserializeState('', 12);
  AssertTrue('empty payload must not wipe live state',
    FEngine.IsViolating(akHigh));
end;

// ---------------------------------------------------------------------------
// Notification
// ---------------------------------------------------------------------------

procedure TAlertEngineTests.OnStateChangedFiresOnViolation;
begin
  FEngine.SetupRule(akHigh, true, 10.0, 0);
  FEngine.OnStateChanged := @HandleStateChanged;

  FEngine.EvaluateLevel(12.0);
  AssertTrue('entering a violation notifies the caller', FChangeCount > 0);

  FChangeCount := 0;
  FEngine.ResumeAll;
  AssertTrue('ResumeAll notifies the caller', FChangeCount > 0);
end;

initialization
  RegisterTest(TAlertEngineTests);

end.
