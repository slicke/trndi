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

unit trndi.alert.engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math;

type
  {** Each distinct alert condition the engine can evaluate. }
  TAlertKind = (
    akHigh,        // BG at or above cgmHi
    akLow,         // BG at or below cgmLo
    akUrgentLow,   // BG at or below limitLO — max-snooze enforced
    akMissing,     // No fresh reading for configured interval
    akSensorFault, // Sensor-fault pattern detected by caller
    akRapidFall,   // BG delta below fall threshold
    akRapidRise    // BG delta above rise threshold
  );
  TAlertKindSet = set of TAlertKind;

const
  ALERT_KIND_LEVEL = [akHigh, akLow, akUrgentLow];

type
  TAlertRule = record
    Enabled: boolean;
    Threshold: double;            // trigger value in API units; 0 = not applicable
    ReAlertMinutes: integer;      // 0 = one-shot per excursion; >0 = re-alert after N min
    MaxSnoozeMinutes: integer;    // 0 = unlimited; positive caps snooze duration
    MinDurationMinutes: integer;  // 0 = fire immediately; >0 = require violation to persist
    HysteresisDelta: double;      // 0 = exact threshold; >0 widens the exit band (API units)
    SnoozedUntil: TDateTime;
    LastFired: TDateTime;         // 0 = never fired this excursion
    ViolationStartedAt: TDateTime;// 0 = not currently violating
  end;

  {** Rule-based alert engine.
    Replaces the old highAlerted/lowAlerted/missingAlerted boolean flags and
    global alertSnoozeUntil with per-rule state, re-alerting, and capped snooze
    for urgent conditions. The engine is a pure decision oracle — callers handle
    actual side-effects (notifications, Chroma, media) based on what fires. }
  TAlertEngine = class
  private
    FRules: array[TAlertKind] of TAlertRule;
    FViolating: TAlertKindSet;
    FOnStateChanged: TNotifyEvent;

    function ShouldFire(const AKind: TAlertKind): boolean;
    procedure MarkFired(const AKind: TAlertKind);
    procedure EnterViolation(const AKind: TAlertKind);
    procedure LeaveViolation(const AKind: TAlertKind);
    procedure NotifyChanged;
  public
    constructor Create;

    {** Configure a rule. Call once after API initializes thresholds.
      @param(AMinDurationMinutes) requires the violation to persist this long
      before firing — 0 fires on the first qualifying reading.
      @param(AHysteresisDelta) widens the band the value must exit before the
      violation clears — 0 clears at the exact threshold. Units match Threshold. }
    procedure SetupRule(const AKind: TAlertKind;
      const AEnabled: boolean;
      const AThreshold: double;
      const AReAlertMinutes: integer;
      const AMaxSnoozeMinutes: integer = 0;
      const AMinDurationMinutes: integer = 0;
      const AHysteresisDelta: double = 0);

    {** Evaluate absolute glucose level. Returns set of rules that fire. }
    function EvaluateLevel(const Val: double): TAlertKindSet;
    {** Evaluate glucose delta. Returns set of rules that fire. }
    function EvaluateDelta(const Delta: double): TAlertKindSet;
    {** Evaluate missing-reading condition. Returns [akMissing] if it fires. }
    function EvaluateMissing: TAlertKindSet;
    {** Evaluate sensor-fault condition. Returns [akSensorFault] if it fires. }
    function EvaluateSensorFault: TAlertKindSet;

    {** Call when BG returns to the in-range zone. Resets all level-rule state. }
    procedure ResetLevelAlerts;
    {** Call when sensor fault clears. }
    procedure ResetSensorFault;
    {** Call when fresh readings resume. }
    procedure ResetMissing;

    {** Snooze only the rules currently in violation. }
    procedure SnoozeActive(const AMinutes: integer);
    {** Snooze every rule regardless of current state. }
    procedure SnoozeAll(const AMinutes: integer);
    {** Cancel all active snoozes. }
    procedure ResumeAll;

    function IsSnoozed(const AKind: TAlertKind): boolean;
    function AnySnoozed: boolean;
    function IsViolating(const AKind: TAlertKind): boolean;
    function ViolatingKinds: TAlertKindSet;
    {** Latest snooze-until among all currently snoozed rules. }
    function ActiveSnoozeUntil: TDateTime;

    {** Serialize per-rule runtime state (snooze, last-fired, violation start) to
      an opaque string the caller can persist. Configuration is not included. }
    function SerializeState: string;
    {** Restore state previously produced by @link(SerializeState). Stale entries
      (expired snoozes, violation start older than @param(AMaxAgeHours)) are dropped. }
    procedure DeserializeState(const AData: string; const AMaxAgeHours: integer = 12);

    {** Fired after any mutation that should be persisted. }
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

implementation

constructor TAlertEngine.Create;
var
  k: TAlertKind;
begin
  inherited Create;
  FViolating := [];
  FOnStateChanged := nil;
  for k := Low(TAlertKind) to High(TAlertKind) do
  begin
    FRules[k].Enabled            := false;
    FRules[k].Threshold          := 0;
    FRules[k].ReAlertMinutes     := 0;
    FRules[k].MaxSnoozeMinutes   := 0;
    FRules[k].MinDurationMinutes := 0;
    FRules[k].HysteresisDelta    := 0;
    FRules[k].SnoozedUntil       := 0;
    FRules[k].LastFired          := 0;
    FRules[k].ViolationStartedAt := 0;
  end;
end;

procedure TAlertEngine.SetupRule(const AKind: TAlertKind;
  const AEnabled: boolean; const AThreshold: double;
  const AReAlertMinutes: integer; const AMaxSnoozeMinutes: integer;
  const AMinDurationMinutes: integer; const AHysteresisDelta: double);
begin
  FRules[AKind].Enabled            := AEnabled;
  FRules[AKind].Threshold          := AThreshold;
  FRules[AKind].ReAlertMinutes     := AReAlertMinutes;
  FRules[AKind].MaxSnoozeMinutes   := AMaxSnoozeMinutes;
  FRules[AKind].MinDurationMinutes := AMinDurationMinutes;
  FRules[AKind].HysteresisDelta    := Max(0, AHysteresisDelta);
end;

procedure TAlertEngine.NotifyChanged;
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

function TAlertEngine.ShouldFire(const AKind: TAlertKind): boolean;
begin
  Result := false;
  if not FRules[AKind].Enabled then Exit;
  if IsSnoozed(AKind) then Exit;
  // Persistence-before-fire: require violation to have lasted MinDurationMinutes.
  if FRules[AKind].MinDurationMinutes > 0 then
    if (FRules[AKind].ViolationStartedAt = 0) or
       (MinutesBetween(Now, FRules[AKind].ViolationStartedAt) < FRules[AKind].MinDurationMinutes) then
      Exit;
  if FRules[AKind].LastFired = 0 then
  begin
    Result := true;
    Exit;
  end;
  if FRules[AKind].ReAlertMinutes = 0 then Exit;
  Result := MinutesBetween(Now, FRules[AKind].LastFired) >= FRules[AKind].ReAlertMinutes;
end;

procedure TAlertEngine.MarkFired(const AKind: TAlertKind);
begin
  FRules[AKind].LastFired := Now;
  NotifyChanged;
end;

procedure TAlertEngine.EnterViolation(const AKind: TAlertKind);
begin
  if not (AKind in FViolating) then
  begin
    FRules[AKind].ViolationStartedAt := Now;
    Include(FViolating, AKind);
    NotifyChanged;
  end;
end;

procedure TAlertEngine.LeaveViolation(const AKind: TAlertKind);
var
  changed: boolean;
begin
  changed := (AKind in FViolating) or (FRules[AKind].LastFired <> 0)
    or (FRules[AKind].ViolationStartedAt <> 0);
  Exclude(FViolating, AKind);
  FRules[AKind].LastFired := 0;
  FRules[AKind].ViolationStartedAt := 0;
  if changed then
    NotifyChanged;
end;

function TAlertEngine.EvaluateLevel(const Val: double): TAlertKindSet;
var
  inUrgent, inLow, inHigh: boolean;
  enterUrgent, enterLow, enterHigh: boolean;
begin
  Result := [];

  inUrgent := akUrgentLow in FViolating;
  inLow    := akLow in FViolating;
  inHigh   := akHigh in FViolating;

  // Hysteresis: once in a band, stay in it until the value exits by HysteresisDelta.
  enterUrgent := FRules[akUrgentLow].Enabled and
    ((Val <= FRules[akUrgentLow].Threshold) or
     (inUrgent and (Val <= FRules[akUrgentLow].Threshold + FRules[akUrgentLow].HysteresisDelta)));

  enterLow := FRules[akLow].Enabled and
    ((Val <= FRules[akLow].Threshold) or
     (inLow and (Val <= FRules[akLow].Threshold + FRules[akLow].HysteresisDelta)));

  enterHigh := FRules[akHigh].Enabled and
    ((Val >= FRules[akHigh].Threshold) or
     (inHigh and (Val >= FRules[akHigh].Threshold - FRules[akHigh].HysteresisDelta)));

  // Urgent low subsumes regular low.
  if enterUrgent then
  begin
    EnterViolation(akUrgentLow);
    LeaveViolation(akHigh);
    // Reset akLow so it starts a fresh excursion if BG climbs back into the regular-low band.
    LeaveViolation(akLow);
    if ShouldFire(akUrgentLow) then
    begin
      MarkFired(akUrgentLow);
      Include(Result, akUrgentLow);
    end;
  end
  else if enterLow then
  begin
    EnterViolation(akLow);
    LeaveViolation(akHigh);
    LeaveViolation(akUrgentLow);
    if ShouldFire(akLow) then
    begin
      MarkFired(akLow);
      Include(Result, akLow);
    end;
  end
  else if enterHigh then
  begin
    EnterViolation(akHigh);
    LeaveViolation(akLow);
    LeaveViolation(akUrgentLow);
    if ShouldFire(akHigh) then
    begin
      MarkFired(akHigh);
      Include(Result, akHigh);
    end;
  end
  else
    ResetLevelAlerts;
end;

function TAlertEngine.EvaluateDelta(const Delta: double): TAlertKindSet;
begin
  Result := [];

  if FRules[akRapidFall].Enabled and (Delta <= FRules[akRapidFall].Threshold) then
  begin
    EnterViolation(akRapidFall);
    if ShouldFire(akRapidFall) then
    begin
      MarkFired(akRapidFall);
      Include(Result, akRapidFall);
    end;
  end
  else
    LeaveViolation(akRapidFall);

  if FRules[akRapidRise].Enabled and (Delta >= FRules[akRapidRise].Threshold) then
  begin
    EnterViolation(akRapidRise);
    if ShouldFire(akRapidRise) then
    begin
      MarkFired(akRapidRise);
      Include(Result, akRapidRise);
    end;
  end
  else
    LeaveViolation(akRapidRise);
end;

function TAlertEngine.EvaluateMissing: TAlertKindSet;
begin
  Result := [];
  if not FRules[akMissing].Enabled then Exit;
  EnterViolation(akMissing);
  if ShouldFire(akMissing) then
  begin
    MarkFired(akMissing);
    Include(Result, akMissing);
  end;
end;

function TAlertEngine.EvaluateSensorFault: TAlertKindSet;
begin
  Result := [];
  if not FRules[akSensorFault].Enabled then Exit;
  EnterViolation(akSensorFault);
  if ShouldFire(akSensorFault) then
  begin
    MarkFired(akSensorFault);
    Include(Result, akSensorFault);
  end;
end;

procedure TAlertEngine.ResetLevelAlerts;
var
  k: TAlertKind;
begin
  for k in ALERT_KIND_LEVEL do
    LeaveViolation(k);
end;

procedure TAlertEngine.ResetSensorFault;
begin
  LeaveViolation(akSensorFault);
end;

procedure TAlertEngine.ResetMissing;
begin
  LeaveViolation(akMissing);
end;

procedure TAlertEngine.SnoozeActive(const AMinutes: integer);
var
  k: TAlertKind;
  snoozeMin: integer;
begin
  for k in FViolating do
  begin
    snoozeMin := AMinutes;
    if (FRules[k].MaxSnoozeMinutes > 0) and (snoozeMin > FRules[k].MaxSnoozeMinutes) then
      snoozeMin := FRules[k].MaxSnoozeMinutes;
    FRules[k].SnoozedUntil := IncMinute(Now, snoozeMin);
    FRules[k].LastFired    := 0;
  end;
  NotifyChanged;
end;

procedure TAlertEngine.SnoozeAll(const AMinutes: integer);
var
  k: TAlertKind;
  snoozeMin: integer;
begin
  for k := Low(TAlertKind) to High(TAlertKind) do
  begin
    snoozeMin := AMinutes;
    if (FRules[k].MaxSnoozeMinutes > 0) and (snoozeMin > FRules[k].MaxSnoozeMinutes) then
      snoozeMin := FRules[k].MaxSnoozeMinutes;
    FRules[k].SnoozedUntil := IncMinute(Now, snoozeMin);
    FRules[k].LastFired    := 0;
  end;
  NotifyChanged;
end;

procedure TAlertEngine.ResumeAll;
var
  k: TAlertKind;
begin
  for k := Low(TAlertKind) to High(TAlertKind) do
    FRules[k].SnoozedUntil := 0;
  NotifyChanged;
end;

function TAlertEngine.IsSnoozed(const AKind: TAlertKind): boolean;
begin
  Result := (FRules[AKind].SnoozedUntil > 0) and (Now < FRules[AKind].SnoozedUntil);
  if not Result then
    FRules[AKind].SnoozedUntil := 0;
end;

function TAlertEngine.AnySnoozed: boolean;
var
  k: TAlertKind;
begin
  for k := Low(TAlertKind) to High(TAlertKind) do
    if IsSnoozed(k) then Exit(true);
  Result := false;
end;

function TAlertEngine.IsViolating(const AKind: TAlertKind): boolean;
begin
  Result := AKind in FViolating;
end;

function TAlertEngine.ViolatingKinds: TAlertKindSet;
begin
  Result := FViolating;
end;

function TAlertEngine.ActiveSnoozeUntil: TDateTime;
var
  k: TAlertKind;
begin
  Result := 0;
  for k := Low(TAlertKind) to High(TAlertKind) do
    if IsSnoozed(k) then
      if (Result = 0) or (FRules[k].SnoozedUntil > Result) then
        Result := FRules[k].SnoozedUntil;
end;

function TAlertEngine.SerializeState: string;
var
  k: TAlertKind;
  fs: TFormatSettings;
  parts: TStringList;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  parts := TStringList.Create;
  try
    for k := Low(TAlertKind) to High(TAlertKind) do
    begin
      if (FRules[k].SnoozedUntil = 0)
        and (FRules[k].LastFired = 0)
        and (FRules[k].ViolationStartedAt = 0) then
        Continue;
      parts.Add(Format('%d|%s|%s|%s|%d',
        [Ord(k),
         FloatToStr(FRules[k].SnoozedUntil, fs),
         FloatToStr(FRules[k].LastFired, fs),
         FloatToStr(FRules[k].ViolationStartedAt, fs),
         Ord(k in FViolating)]));
    end;
    Result := parts.DelimitedText;
  finally
    parts.Free;
  end;
end;

procedure TAlertEngine.DeserializeState(const AData: string;
  const AMaxAgeHours: integer);
var
  entries: TStringList;
  fields: TStringList;
  fs: TFormatSettings;
  i, kindOrd: integer;
  k: TAlertKind;
  snooze, lastFired, vStart: double;
  violating: boolean;
  maxAge: TDateTime;
begin
  if Trim(AData) = '' then Exit;
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  maxAge := IncHour(Now, -Abs(AMaxAgeHours));

  entries := TStringList.Create;
  fields := TStringList.Create;
  try
    entries.StrictDelimiter := true;
    entries.Delimiter := ',';
    entries.DelimitedText := AData;

    for i := 0 to entries.Count - 1 do
    begin
      fields.Clear;
      fields.StrictDelimiter := true;
      fields.Delimiter := '|';
      fields.DelimitedText := entries[i];
      if fields.Count < 5 then Continue;
      if not TryStrToInt(fields[0], kindOrd) then Continue;
      if (kindOrd < Ord(Low(TAlertKind))) or (kindOrd > Ord(High(TAlertKind))) then Continue;
      k := TAlertKind(kindOrd);

      snooze    := StrToFloatDef(fields[1], 0, fs);
      lastFired := StrToFloatDef(fields[2], 0, fs);
      vStart    := StrToFloatDef(fields[3], 0, fs);
      violating := fields[4] = '1';

      // Drop stale entries — expired snoozes, violations older than cap.
      if (snooze > 0) and (snooze <= Now) then snooze := 0;
      if (vStart > 0) and (vStart < maxAge) then
      begin
        vStart := 0;
        lastFired := 0;
        violating := false;
      end;
      if (lastFired > 0) and (lastFired < maxAge) then lastFired := 0;

      FRules[k].SnoozedUntil       := snooze;
      FRules[k].LastFired          := lastFired;
      FRules[k].ViolationStartedAt := vStart;
      if violating then
        Include(FViolating, k)
      else
        Exclude(FViolating, k);
    end;
  finally
    fields.Free;
    entries.Free;
  end;
end;

end.
