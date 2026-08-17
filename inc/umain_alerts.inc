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
(* MODIFICATION NOTICE (2026-08-05): Replaced floating-point wrapped-row
   rounding with integer ceiling division for FPC/macOS compatibility. *)
(* MODIFICATION NOTICE (2026-08-17): Added SeedLevelAlertRules, which sets the
   high/low/urgent-low rules from the API's thresholds. The boot path used to
   inline this before Connect, pinning every alert to initCGMCore's 401/40
   placeholders. *)
(* Warning overlay panel, connection badge, and alert snooze for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

// Shrink-to-fit: the largest size <= AStart (never below AMin) at which AText
// fits AMaxW x AMaxH. Shrink-only on purpose — the stale card derives its row
// heights from the form, not from its text, so the type never grows to fill
// and the layout can't twitch as the age counts up.
function FitFontSize(const AFont: TFont; const AText: string;
  AMaxW, AMaxH, AStart, AMin: integer): integer;
var
  bmp: TBitmap;
begin
  Result := AStart;
  if (AText = '') or (AMaxW <= 0) or (AMaxH <= 0) then
    Exit;

  // Measure on a temp TBitmap — TLabel.Canvas outside paint is unsafe on
  // Cocoa, and a 0x0 bitmap yields zero metrics on GTK3.
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(AFont);
    while Result > AMin do
    begin
      bmp.Canvas.Font.Size := Result;
      if (bmp.Canvas.TextWidth(AText) <= AMaxW) and
         (bmp.Canvas.TextHeight(AText) <= AMaxH) then
        Break;
      Dec(Result);
    end;
  finally
    bmp.Free;
  end;
end;

// Height a word-wrapped AText needs at AFontSize inside AMaxW. Counts each hard
// line's wrapped runs rather than assuming one row per line, so the card's
// detail area is sized from real metrics instead of a guessed multiplier.
// ASlack is appended to the last line for measurement only — the caller uses it
// to reserve room for text that grows in place (a retry countdown gaining a
// digit) without paying for a whole spare row.
function MeasureWrappedHeight(const AFont: TFont; const AText: string;
  AMaxW, AFontSize: integer; const ASlack: string = ''): integer;
var
  bmp: TBitmap;
  lines: TStringList;
  i, rows, lineW, lineH: integer;
  measured: string;
begin
  Result := 0;
  if (AText = '') or (AMaxW <= 0) then
    Exit;

  bmp := TBitmap.Create;
  lines := TStringList.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(AFont);
    bmp.Canvas.Font.Size := AFontSize;
    lineH := bmp.Canvas.TextHeight('Hg');
    if lineH <= 0 then
      lineH := AFontSize * 2;

    lines.Text := AText;
    rows := 0;
    for i := 0 to lines.Count - 1 do
    begin
      measured := lines[i];
      if i = lines.Count - 1 then
        measured := measured + ASlack;
      lineW := bmp.Canvas.TextWidth(measured);
      rows := rows + Max(1, (lineW + AMaxW - 1) div AMaxW);
    end;
    Result := Max(1, rows) * lineH;
  finally
    lines.Free;
    bmp.Free;
  end;
end;

{------------------------------------------------------------------------------
  Seed the level alert rules from the API's current thresholds.

  Persistence-before-fire (minutes) and hysteresis (mg/dL) are user-tunable;
  defaults filter out single-sample spikes and threshold bounce without
  delaying urgent-low alerts.

  Called after boot has settled the thresholds and again whenever settings
  change them, so the alerts track the limits the UI paints with. SetupRule
  rewrites only the rule's configuration - snooze, last-fired and violation
  timestamps survive, so re-seeding never resets an alert in progress.
------------------------------------------------------------------------------}
procedure TfBG.SeedLevelAlertRules;
begin
  if (FAlertEngine = nil) or (api = nil) then
    Exit;

  FAlertEngine.SetupRule(akHigh,       true,  api.cgmHi,  30, 0,
    native.GetIntSetting('alerts.duration.high', 10),
    native.GetFloatSetting('alerts.hysteresis.high', 5));
  FAlertEngine.SetupRule(akLow,        true,  api.cgmLo,  15, 0,
    native.GetIntSetting('alerts.duration.low', 5),
    native.GetFloatSetting('alerts.hysteresis.low', 5));
  FAlertEngine.SetupRule(akUrgentLow,  true,  api.limitLO, 5, 5,
    native.GetIntSetting('alerts.duration.urgent', 0),
    native.GetFloatSetting('alerts.hysteresis.urgent', 5));
end;

function TfBG.AlertsSnoozed: boolean;
begin
  Result := FAlertEngine.AnySnoozed;
end;

procedure TfBG.UpdateAlertSnoozeMenu;
var
  isSnoozed: boolean;
  snoozeUntil: TDateTime;
begin
  if miAlertSnooze = nil then
    Exit;

  isSnoozed  := FAlertEngine.AnySnoozed;
  snoozeUntil := FAlertEngine.ActiveSnoozeUntil;

  if isSnoozed then
    miAlertSnooze.Caption := Format(RS_ALERT_SNOOZE_ACTIVE, [FormatDateTime('hh:nn', snoozeUntil)])
  else
    miAlertSnooze.Caption := RS_ALERT_SNOOZE;

  if miAlertSnoozeOff <> nil then
  begin
    miAlertSnoozeOff.Enabled := isSnoozed;
    if isSnoozed then
      miAlertSnoozeOff.Caption := RS_ALERT_SNOOZE_RESUME
    else
      miAlertSnoozeOff.Caption := RS_ALERT_SNOOZE_NOT_ACTIVE;
  end;
end;

function TfBG.ClassifyConnectionStatus(const ErrorText: string): string;
var
  s: string;
begin
  s := LowerCase(Trim(ErrorText));
  if s = '' then
    Exit(RS_CONN_RETRYING);

  if (Pos('429', s) > 0) or (Pos('rate', s) > 0) or (Pos('too many', s) > 0) then
    Exit(RS_CONN_RATE_LIMITED);

  if (Pos('401', s) > 0) or (Pos('403', s) > 0) or (Pos('unauthor', s) > 0) or
    (Pos('forbidden', s) > 0) or (Pos('not authenticated', s) > 0) or
    (Pos('credential', s) > 0) then
    Exit(RS_CONN_AUTH_EXPIRED);

  Result := RS_CONN_RETRYING;
end;

function TfBG.GetSensorBadgeText: string;
var
  i, pOpen, pClose: integer;
  sensorText, lowerText, innerText, payloadText: string;
begin
  Result := '';
  if not native.GetBoolSetting('display.sensor_expiry', false) then
    Exit;

  if Length(bgs) = 0 then
    Exit;

  sensorText := '';
  for i := High(bgs) downto Low(bgs) do
    if not bgs[i].empty then
    begin
      sensorText := bgs[i].sensor;
      Break;
    end;

  if sensorText = '' then
    Exit;

  lowerText := LowerCase(sensorText);
  pOpen := Pos('(sensor ', lowerText);
  if pOpen = 0 then
    Exit;

  pClose := PosEx(')', sensorText, pOpen);
  if pClose > pOpen then
    innerText := Copy(sensorText, pOpen + 1, pClose - pOpen - 1)
  else
    innerText := Copy(sensorText, pOpen + 1, Length(sensorText) - pOpen);

  innerText := Trim(innerText);
  if innerText = '' then
    Exit;

  if Pos('sensor ', LowerCase(innerText)) = 1 then
  begin
    payloadText := Trim(Copy(innerText, Length('sensor ') + 1, MaxInt));
    if payloadText = '' then
      Exit('');
    Result := 'Sensor: ' + payloadText;
  end
  else
    Result := innerText;
end;

procedure TfBG.SetConnectionBadge(const StatusText: string; const BadgeColor: TColor);
var
  isOk: boolean;
  isCentered: boolean;
  showOkBadge: boolean;
  displayStatus: string;
  displayColor: TColor;
  forceShowBadge: boolean;
  sensorBadgeText: string;
begin
  FLastConnectionStatus := StatusText;
  displayStatus := StatusText;
  displayColor := BadgeColor;
  forceShowBadge := false;

  {$ifdef DEBUG}
  if Assigned(api) and (api is DebugAPI) then
  begin
    displayStatus := 'DEBUG';
    displayColor := RGBToColor(100, 150, 255);
    forceShowBadge := true;
  end;
  {$endif}

  isOk := displayStatus = RS_CONN_OK;
  isCentered := isOk or (displayStatus = RS_CONN_RETRYING) or forceShowBadge;
  sensorBadgeText := GetSensorBadgeText;
  showOkBadge := native.GetBoolSetting('ux.connectivity.button', false) or
    (sensorBadgeText <> '');

  if sensorBadgeText <> '' then
    displayStatus := displayStatus + ' | ' + sensorBadgeText;

  if isOk and (not showOkBadge) and (not forceShowBadge) then
  begin
    HideConnectionBadge;
    Exit;
  end;

  EnsureConnectionBadgeControls;
  ApplyConnectionBadgeLayout(displayStatus, displayColor, isCentered);
end;

procedure TfBG.HideConnectionBadge;
begin
  lInternet.Visible := false;
  if Assigned(FInternetBadgeBg) then
    FInternetBadgeBg.Visible := false;
  if Assigned(FInternetBadgeShadow) then
    FInternetBadgeShadow.Visible := false;
end;

procedure TfBG.EnsureConnectionBadgeControls;
begin
  if not Assigned(FInternetBadgeShadow) then
  begin
    FInternetBadgeShadow := TShape.Create(Self);
    FInternetBadgeShadow.Parent := Self;
    FInternetBadgeShadow.Shape := stRoundRect;
    FInternetBadgeShadow.Pen.Style := psClear;
  end;

  if not Assigned(FInternetBadgeBg) then
  begin
    FInternetBadgeBg := TShape.Create(Self);
    FInternetBadgeBg.Parent := Self;
    FInternetBadgeBg.Shape := stRoundRect;
    FInternetBadgeBg.Pen.Style := psSolid;
    FInternetBadgeBg.Pen.Width := 1;
  end;
end;

procedure TfBG.ApplyConnectionBadgeLayout(const DisplayStatus: string;
  const DisplayColor: TColor; const IsCentered: boolean);
var
  contentWidth: integer;
  maxBadgeWidth: integer;
  badgeTextPadding: integer;
begin
  lInternet.Caption := DisplayStatus;
  lInternet.Font.Style := [fsBold];
  lInternet.Font.Quality := fqCleartype;
  lInternet.Font.Color := GetTextColorForBackground(DisplayColor, 0, 0.65);
  lInternet.Transparent := true;
  lInternet.AutoSize := false;
  lInternet.WordWrap := false;
  badgeTextPadding := Max(18, round(ClientWidth * 0.02));
  // Measure via temp TBitmap — TLabel.Canvas outside paint SIGABRTs on Cocoa.
  with TBitmap.Create do
  try
    SetSize(1, 1);
    Canvas.Font.Assign(lInternet.Font);
    contentWidth := Canvas.TextWidth(DisplayStatus) + badgeTextPadding;
  finally
    Free;
  end;
  maxBadgeWidth := Max(96, round(ClientWidth * 0.42));
  lInternet.Width := Min(maxBadgeWidth, Max(96, contentWidth));
  lInternet.Height := Max(28, round(ClientHeight * 0.058));
  ScaleLbl(lInternet, taCenter, tlCenter, true);

  if IsCentered then
  begin
    lInternet.Anchors := [];
    lInternet.Left := Max(8, (ClientWidth - lInternet.Width) div 2);
    lInternet.Top := 8 + TitleBarOffset;
  end
  else
  begin
    lInternet.Anchors := [akTop, akRight];
    lInternet.Top := 8 + TitleBarOffset;
    lInternet.Left := Max(8, ClientWidth - lInternet.Width - 8);
  end;

  FInternetBadgeShadow.Brush.Color := BlendColors(DarkenColor(DisplayColor, 0.65), fBG.Color, 0.55);
  FInternetBadgeBg.Brush.Color := DisplayColor;
  FInternetBadgeBg.Pen.Color := BlendColors(DisplayColor, clBlack, 0.35);
  UpdateConnectionBadgeBackgroundBounds;
  FInternetBadgeShadow.Visible := true;
  FInternetBadgeShadow.BringToFront;
  FInternetBadgeBg.Visible := true;
  FInternetBadgeBg.BringToFront;

  lInternet.Cursor := crHandPoint;
  if Trim(FLastConnectionDetail) <> '' then
  begin
    lInternet.ShowHint := true;
    lInternet.Hint := RS_CONN_CLICK_FOR_DETAILS;
  end
  else
  begin
    lInternet.ShowHint := false;
    lInternet.Hint := '';
  end;
  lInternet.Visible := true;
  lInternet.BringToFront;
end;

procedure TfBG.UpdateConnectionBadgeBackgroundBounds;
var
  badgePadX: integer;
  badgePadY: integer;
  shadowOffset: integer;
begin
  if not Assigned(FInternetBadgeBg) then
    Exit;

  badgePadX := Max(6, round(lInternet.Width * 0.08));
  badgePadY := Max(3, round(lInternet.Height * 0.12));
  FInternetBadgeBg.SetBounds(lInternet.Left - badgePadX, lInternet.Top - badgePadY,
    lInternet.Width + (badgePadX * 2), lInternet.Height + (badgePadY * 2));

  if Assigned(FInternetBadgeShadow) then
  begin
    shadowOffset := Max(1, round(lInternet.Height * 0.1));
    FInternetBadgeShadow.SetBounds(FInternetBadgeBg.Left + shadowOffset,
      FInternetBadgeBg.Top + shadowOffset,
      FInternetBadgeBg.Width,
      FInternetBadgeBg.Height);
  end;
end;

procedure TfBG.lInternetClick(Sender: TObject);
var
  details: string;
begin
  details := Trim(FLastConnectionDetail);
  if details = '' then
  begin
    if FLastConnectionStatus = RS_CONN_OK then
      ShowMessage(RS_CONN_OK_INFO)
    else
      ShowMessage(Format(RS_CONN_DETAILS, [lInternet.Caption, RS_CONN_NO_DETAILS]));
    Exit;
  end;

  ShowMessage(Format(RS_CONN_DETAILS, [lInternet.Caption, details]));
end;

procedure TfBG.PersistAlertState(Sender: TObject);
begin
  if not Assigned(native) or not Assigned(FAlertEngine) then Exit;
  native.SetSetting('alerts.state', FAlertEngine.SerializeState);
end;

{------------------------------------------------------------------------------
  CheckPumpReservoir
  ------------------
  Raise one toast per reservoir step (30/25/20/15/10/5 U) as a pump backend's
  cartridge empties. Only the pump-aware backends (Tandem, CareLink) report a
  reservoir at all; everything else leaves the field at DEVICE_STATUS_UNKNOWN
  and is skipped here, since a missing figure must never read as an empty one.

  getDeviceStatus answers from the last fetch's cached payload, so this is
  cheap enough to call after every successful fetch. The step latch is
  persisted, so restarting Trndi on a low cartridge does not re-notify.
 ------------------------------------------------------------------------------}
procedure TfBG.CheckPumpReservoir;
var
  status: TCGMDeviceStatus;
  before: integer;
begin
  if not Assigned(api) or not Assigned(native) then
    Exit;
  if not native.GetBoolSetting('alerts.notice.reservoir', true) then
    Exit;
  if not api.getDeviceStatus(status) then
    Exit;

  before := FReservoirStep;
  if ReservoirShouldNotify(status.reservoirUnits, FReservoirStep) then
  begin
    TrndiDLog(Format('CheckPumpReservoir: %.1fU left, notifying at step %dU',
      [status.reservoirUnits, FReservoirStep]));
    native.attention(RS_ATTENTION_RESERVOIR,
      Format(RS_ATTENTION_RESERVOIR_DESC,
      [FormatFloat('0.#', status.reservoirUnits)]));
  end;

  if FReservoirStep <> before then
    native.SetSetting('alerts.reservoir.step', FReservoirStep);
end;

{------------------------------------------------------------------------------
  CheckSensorExpiry
  -----------------
  Raise one toast per sensor-expiry step (24/8/4/2/1 h) as a session runs out.
  Only CareLink reports remaining sensor life; the Tandem payload carries none
  (its CGM events have no session age), so it and every plain CGM backend leave
  the field at DEVICE_STATUS_UNKNOWN and are skipped here — the same rule the
  reservoir follows, that a missing figure never reads as an empty one.

  sensorDurationHours counts down, confirmed against the CareLink app. Reading
  it the other way would announce a fresh sensor as an expiring one, so a
  backend that ever reports elapsed hours instead must convert before filling
  the field rather than being handled here.
 ------------------------------------------------------------------------------}
procedure TfBG.CheckSensorExpiry;
var
  status: TCGMDeviceStatus;
  before: integer;
begin
  if not Assigned(api) or not Assigned(native) then
    Exit;
  if not native.GetBoolSetting('alerts.notice.sensor', true) then
    Exit;
  if not api.getDeviceStatus(status) then
    Exit;

  before := FSensorExpiryStep;
  if SensorExpiryShouldNotify(status.sensorDurationHours, FSensorExpiryStep) then
  begin
    TrndiDLog(Format('CheckSensorExpiry: %dh left, notifying at step %dh',
      [status.sensorDurationHours, FSensorExpiryStep]));
    if status.sensorDurationHours <= 1 then
      native.attention(RS_ATTENTION_SENSOR_EXPIRY,
        RS_ATTENTION_SENSOR_EXPIRY_DESC_ONE)
    else
      native.attention(RS_ATTENTION_SENSOR_EXPIRY,
        Format(RS_ATTENTION_SENSOR_EXPIRY_DESC, [status.sensorDurationHours]));
  end;

  if FSensorExpiryStep <> before then
    native.SetSetting('alerts.sensor.step', FSensorExpiryStep);
end;

{------------------------------------------------------------------------------
  CheckPumpBattery
  ----------------
  Raise one toast per pump-battery step (20/15/10/5/2 %) as the battery runs
  down. Both pump backends fill pumpBatteryPercent — Tandem from the `ibc`
  property its status and battery events carry, CareLink from
  pumpBatteryLevelPercent — and every other backend leaves it at
  DEVICE_STATUS_UNKNOWN and is skipped here, on the rule the reservoir and
  sensor ladders follow: a missing figure is not a flat battery.

  Unlike a cartridge, a battery goes back up as a matter of routine, which is
  what the wider PUMP_BATTERY_REARM_MARGIN is for — a pump on the charger over
  a coffee must not re-arm a step it has already announced.
 ------------------------------------------------------------------------------}
procedure TfBG.CheckPumpBattery;
var
  status: TCGMDeviceStatus;
  before: integer;
begin
  if not Assigned(api) or not Assigned(native) then
    Exit;
  if not native.GetBoolSetting('alerts.notice.battery', true) then
    Exit;
  if not api.getDeviceStatus(status) then
    Exit;

  before := FPumpBatteryStep;
  if PumpBatteryShouldNotify(status.pumpBatteryPercent, FPumpBatteryStep) then
  begin
    TrndiDLog(Format('CheckPumpBattery: %d%% left, notifying at step %d%%',
      [status.pumpBatteryPercent, FPumpBatteryStep]));
    native.attention(RS_ATTENTION_PUMP_BATTERY,
      Format(RS_ATTENTION_PUMP_BATTERY_DESC, [status.pumpBatteryPercent]));
  end;

  if FPumpBatteryStep <> before then
    native.SetSetting('alerts.battery.step', FPumpBatteryStep);
end;

procedure TfBG.SetAlertSnoozeMinutes(const Minutes: integer);
begin
  FAlertEngine.SnoozeActive(Minutes);
  UpdateAlertSnoozeMenu;
  ShowMessage(Format(RS_ALERT_SNOOZE_FOR, [Minutes]));
end;

procedure TfBG.miAlertSnooze15Click(Sender: TObject);
begin
  SetAlertSnoozeMinutes(15);
end;

procedure TfBG.miAlertSnooze30Click(Sender: TObject);
begin
  SetAlertSnoozeMinutes(30);
end;

procedure TfBG.miAlertSnooze60Click(Sender: TObject);
begin
  SetAlertSnoozeMinutes(60);
end;

procedure TfBG.miAlertSnoozeOffClick(Sender: TObject);
begin
  FAlertEngine.ResumeAll;
  UpdateAlertSnoozeMenu;
  ShowMessage(RS_ALERT_SNOOZE_OFF);
end;

procedure TfBG.ApplyChromaAlertAction(const ActionSettingKey: string;
const DefaultAction: string; const AColor: TRGBColor);
var
  chromaAction: string;
begin
  chromaAction := LowerCase(Trim(native.GetSetting(ActionSettingKey, DefaultAction)));

  if chromaAction = 'ignore' then
    Exit;

  if (chromaAction = 'off') or (chromaAction = 'none') then
    Chroma.SetNoneAll
  else
  if chromaAction = 'static' then
    Chroma.SetStaticAll(AColor)
  else
    Chroma.SetBreathDualAll(AColor, clRazerBlack);
end;

// Map a severity level to its visual properties. Single source of truth so
// paint, layout, and the resize handler stay consistent.
//
// wsCritical delegates its tint to the escalation stage: an outage a few
// minutes past the freshness threshold is routine and gets a neutral slate,
// while red is held back for the point where the sensor link is likely gone.
// Reserving red also stops "stale" from being mistaken for a hypo alarm.
procedure TfBG.GetWarnSeverityVisual(severity: TWarnSeverity;
  out tintColor: TColor; out opacity: byte; out layout: TWarnLayout);
begin
  case severity of
    wsInfo:
      begin
        tintColor := RGBToColor(230, 178, 50);   // amber
        opacity   := 240;
        layout    := wlBanner;
      end;
    wsSoon:
      begin
        tintColor := RGBToColor(230, 126, 34);   // pumpkin/orange
        opacity   := 240;
        layout    := wlBanner;
      end;
  else
    // wsCritical — stale/no data
    case FStaleStage of
      stLate:
        tintColor := RGBToColor(198, 146, 42);   // amber
      stLost:
        tintColor := RGBToColor(184, 62, 50);    // pomegranate
    else
      // stDelayed
      tintColor := RGBToColor(64, 74, 92);       // slate
    end;
    opacity := 245;
    layout  := wlStatusCard;
  end;
end;

function TfBG.CurrentStaleStage: TStaleStage;
var
  age: int64;
  bg: BGReading;
begin
  // No reading has ever landed (cold boot, backend never answered): there is no
  // outage to escalate, only an absence. Stay neutral.
  if not tryLastReading(bg) then
    Exit(stDelayed);

  age := MinutesBetween(Now, lastDataReading.date);
  if age >= STALE_LOST_MINUTES then
    Result := stLost
  else
  if age >= STALE_LATE_MINUTES then
    Result := stLate
  else
    Result := stDelayed;
end;

function TfBG.FormatStaleAge(const Minutes: int64): string;
var
  m: int64;
begin
  m := Minutes;
  if m < 0 then
    m := 0;

  if m >= MinsPerDay then
    Result := Format(RS_STALE_AGE_DAY, [m div MinsPerDay, (m mod MinsPerDay) div 60])
  else
  if m >= 60 then
  begin
    if (m mod 60) = 0 then
      Result := Format(RS_STALE_AGE_HOUR_ONLY, [m div 60])
    else
      Result := Format(RS_STALE_AGE_HOUR, [m div 60, m mod 60]);
  end
  else
    Result := Format(RS_STALE_AGE_MIN, [m]);
end;

procedure TfBG.BuildStaleCardText(out Headline, Detail, Extra: string);
var
  bg: BGReading;
  age, retrySecs: int64;
  val, last, ageText: string;
  haveReading: boolean;
begin
  val := '';
  haveReading := tryLastReading(bg);
  // tryLastReading can hand back a placeholder whose formatted value is a dash;
  // fall back to the last row that actually carried a number, which is also
  // what the age is measured against.
  if haveReading then
  begin
    val := bg.format(un, BG_MSG_SHORT);
    if (val <> '') and (val[1] = '-') then
    begin
      bg := lastDataReading;
      val := bg.format(un, BG_MSG_SHORT);
    end;
  end;

  if not haveReading then
  begin
    Headline := RS_STALE_WAITING;
    Detail   := RS_LAST_RECIEVE_NO;
  end
  else
  begin
    age := MinutesBetween(Now, lastDataReading.date);
    ageText := FormatStaleAge(age);
    if FStaleStage = stLost then
      Headline := Format(RS_STALE_LOST, [ageText])
    else
      Headline := Format(RS_STALE_NO_DATA, [ageText]);

    last := FormatDateTime('hh:nn', bg.date);
    if DateOf(bg.date) <> DateOf(Now) then
      Detail := Format(RS_STALE_LAST_AT_DAY, [val, last,
        Format(RS_DAYS_AGO, [DaysBetween(Now, bg.date)])])
    else
      Detail := Format(RS_STALE_LAST_AT, [val, last]);
  end;

  // Retry countdown rides along on the detail row. FNextFetchAt is set by
  // SetNextUpdateTimer; while a worker is in flight there is nothing to count
  // down to, so say so instead of showing a frozen number.
  if FApiCallInFlight then
    Detail := Detail + ' · ' + RS_STALE_RETRY_NOW
  else
  if FNextFetchAt > 0 then
  begin
    retrySecs := SecondsBetween(FNextFetchAt, Now);
    if FNextFetchAt <= Now then
      Detail := Detail + ' · ' + RS_STALE_RETRY_NOW
    else
      Detail := Detail + ' · ' + Format(RS_STALE_RETRY_IN, [retrySecs]);
  end;

  // Expanded row: the backend's own message plus the threshold that decided
  // this reading is stale in the first place. The message is composed for a
  // full-height panel (RS_NO_BACKEND carries blank lines between its
  // sentences); flatten it to single-spaced lines so it fits the card's two
  // detail rows.
  Extra := Trim(FWarnMessage);
  while Pos(LineEnding + LineEnding, Extra) > 0 do
    Extra := StringReplace(Extra, LineEnding + LineEnding, LineEnding, [rfReplaceAll]);
  if Extra <> '' then
    Extra := Extra + LineEnding;
  Extra := Extra + Format(RS_LAST_RECIEVE_AGE,
    [native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES)]);
end;

procedure TfBG.RefreshStaleCard;
var
  newStage: TStaleStage;
  head, detail, extra: string;
begin
  if not (Assigned(pnWarning) and pnWarning.Visible) then
    Exit;
  if FWarnSeverity <> wsCritical then
    Exit;

  newStage := CurrentStaleStage;
  if newStage <> FStaleStage then
  begin
    // Crossing an escalation boundary changes the tint and the headline
    // wording, so re-run the full layout once. Everything in between is a
    // caption swap — the geometry and font sizes are derived from the card
    // size, not from the text, so they can't drift as the age counts up.
    FStaleStage := newStage;
    fixWarningPanel;
  end
  else
  begin
    BuildStaleCardText(head, detail, extra);
    lMissing.Caption := head;
    if FWarnExpanded and (extra <> '') then
      pnWarnLast.Caption := detail + LineEnding + extra
    else
      pnWarnLast.Caption := detail;
  end;

  pnWarning.Repaint;
end;

procedure TfBG.fixWarningPanel;
const
  GLYPH_AREA_W = 56;   // Reserved width for the banner glyph on the left
  CHEVRON_AREA_W = 28; // Reserved width for expand chevron on the right
var
  padding: integer;
  calculatedFontSize: integer;
  bg: BGReading;
  last, val: string;
  tintColor: TColor;
  opacityTmp: byte;
  layout: TWarnLayout;
  baseBannerH, expandedExtraH: integer;
  detailCaption: string;
  headText, detailText, extraText: string;
  cardW, cardH, headH, detailH, extraH, glyphW, cardPad, detailW: integer;
  headFont, detailFont: integer;
begin
  calculatedFontSize := 12;
  pnWarning.AutoSize := false;
  GetWarnSeverityVisual(FWarnSeverity, tintColor, opacityTmp, layout);

  // Pick a high-contrast caption color against the tinted panel. Use pure
  // white/black (not a tint-derived shade) so the title doesn't share hue with
  // the panel background or with other on-screen values.
  if IsLightColor(tintColor) then
    lMissing.Font.Color := clBlack
  else
    lMissing.Font.Color := clWhite;
  // Detail line stays the same hue but blended ~35% toward the panel tint, so
  // the title clearly reads as primary and the detail as secondary.
  pnWarnLast.Font.Color := BlendColors(lMissing.Font.Color, tintColor, 0.35);

  if layout = wlStatusCard then
  begin
    // ----- Critical: compact status card pinned to the bottom -----
    // Type sizes come from the form, not from the text, so the per-second age
    // update in RefreshStaleCard is a pure caption swap: nothing reflows and
    // the card doesn't twitch as "9 min" becomes "10 min".
    BuildStaleCardText(headText, detailText, extraText);

    // Margins track the smaller dimension: a wide-but-short window shouldn't
    // spend a quarter of its height on the gap under the card.
    padding := Max(8, Min(ClientWidth div 28, ClientHeight div 22));
    cardPad := Max(8, ClientHeight div 40);
    cardW := Min(ClientWidth - (padding * 2), Max(220, round(ClientWidth * 0.86)));
    if cardW < 120 then
      cardW := Max(60, ClientWidth - 8);

    // Measured below at the detail size, so the bold banner face left on the
    // label by a previous wsInfo/wsSoon show can't skew the row count.
    pnWarnLast.Font.Style := [];
    headFont := Max(11, Min(30, ClientHeight div 16));
    detailFont := Max(8, round(headFont * 0.62));
    headH := round(headFont * 1.9);
    glyphW := Max(28, headH);
    // Text column: everything left of the chevron, right of the glyph gutter.
    detailW := Max(20, cardW - (cardPad * 2) - glyphW);

    // Two digits of slack: the retry countdown ticks up in place and must not
    // wrap onto a row the card wasn't sized for.
    detailH := MeasureWrappedHeight(pnWarnLast.Font, detailText, detailW,
      detailFont, '00');

    // The expanded row carries the backend message and the threshold, both of
    // which wrap — measure rather than guess a row count.
    extraH := 0;
    if FWarnExpanded and WarnShowDetails and (extraText <> '') then
      extraH := MeasureWrappedHeight(pnWarnLast.Font, extraText, detailW, detailFont);

    cardH := (cardPad * 2) + headH + detailH + extraH;
    // A small window (or a tall expanded row) must not push the card over the
    // value it is meant to annotate: cap it at 48% of the form and let the
    // expanded text clip instead.
    if cardH > round(ClientHeight * 0.48) then
    begin
      cardH := Max(48, round(ClientHeight * 0.48));
      extraH := Max(0, cardH - (cardPad * 2) - headH - detailH);
    end;

    pnWarning.Width  := cardW;
    pnWarning.Left   := (ClientWidth - cardW) div 2;
    pnWarning.Height := cardH;
    pnWarning.Top    := Max(0, ClientHeight - cardH - padding);

    // glyphW is the gutter reserved for the clock glyph pnWarningPaint draws;
    // it centers the glyph in 0..lMissing.Left, so the two stay in step.
    lMissing.Caption := headText;
    lMissing.AutoSize := false;
    lMissing.WordWrap := false;
    lMissing.OptimalFill := false;
    lMissing.Font.Style := [fsBold];
    lMissing.Alignment := taLeftJustify;
    lMissing.Layout := tlCenter;
    lMissing.Left   := cardPad + glyphW;
    lMissing.Top    := cardPad;
    lMissing.Width  := Max(20, cardW - cardPad - glyphW - CHEVRON_AREA_W);
    lMissing.Height := headH;
    // Shrink the headline if it can't fit the row — measured with two extra
    // digits of slack so the per-second age update (which only swaps captions)
    // still fits as "9 min" grows into "10 min".
    lMissing.Font.Size := FitFontSize(lMissing.Font, headText + '00',
      lMissing.Width, headH, headFont, 9);
    lMissing.Visible := true;

    if WarnShowDetails then
    begin
      if (extraH > 0) and (extraText <> '') then
        pnWarnLast.Caption := detailText + LineEnding + extraText
      else
        pnWarnLast.Caption := detailText;
      pnWarnLast.AutoSize := false;
      pnWarnLast.WordWrap := true;
      pnWarnLast.OptimalFill := false;
      pnWarnLast.Font.Size := detailFont;
      pnWarnLast.Font.Style := [];
      pnWarnLast.Alignment := taLeftJustify;
      pnWarnLast.Layout := tlTop;
      pnWarnLast.Left   := cardPad + glyphW;
      pnWarnLast.Top    := cardPad + headH;
      pnWarnLast.Width  := detailW;
      pnWarnLast.Height := detailH + extraH;
      pnWarnLast.Visible := true;
    end
    else
      pnWarnLast.Visible := false;

    // Keep the dimmed hero clear of the card. lVal is alClient and vertically
    // centered, so it can only be as tall as the band left above the card —
    // counted on both sides of center. ResizeUIElements' ScaleLbl takes the
    // font back over once fresh data restores the normal layout.
    lVal.Font.Size := FitFontSize(lVal.Font, lVal.Caption,
      Max(20, ClientWidth - 16),
      Max(24, ClientHeight - (2 * (cardH + padding))), 150, 8);
    lVal.Refresh;
  end
  else
  begin
    // ----- Info/Soon: slim filled banner anchored at the top -----
    // Compose the "last reading" detail caption for the banner's expanded row.
    detailCaption := '';
    if WarnShowDetails then
    begin
      if tryLastReading(bg) then
      begin
        val := bg.format(un, BG_MSG_SHORT);
        if (val <> '') and (val[1] = '-') then
        begin
          bg := lastDataReading;
          val := bg.format(un, BG_MSG_SHORT);
        end;
        last := FormatDateTime('hh:nn', bg.date);
        if DateOf(bg.date) <> Dateof(now) then
          last := last + ' ' + Format(RS_DAYS_AGO, [DaysBetween(now, bg.date)]);
        detailCaption := Format(RS_LAST_RECIEVE, [val, last]);
      end
      else
        detailCaption := RS_LAST_RECIEVE_NO;
      detailCaption := detailCaption + LineEnding +
        Format(RS_LAST_RECIEVE_AGE, [native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES)]);
    end;

    padding := Max(8, ClientWidth div 40);
    baseBannerH := Max(56, ClientHeight div 9);
    expandedExtraH := 0;
    if FWarnExpanded and WarnShowDetails and (detailCaption <> '') then
      expandedExtraH := Max(36, baseBannerH div 2);

    pnWarning.Width  := ClientWidth - (padding * 2);
    pnWarning.Left   := padding;
    pnWarning.Top    := padding + TitleBarOffset;
    pnWarning.Height := baseBannerH + expandedExtraH;
    FWarnBannerBaseH := baseBannerH;

    lMissing.Caption := FWarnMessage;
    lMissing.AutoSize := false;
    lMissing.WordWrap := true;
    lMissing.OptimalFill := true;
    lMissing.Font.Style := [fsBold];
    lMissing.Left   := GLYPH_AREA_W;
    lMissing.Top    := 6;
    lMissing.Width  := Max(40, pnWarning.Width - GLYPH_AREA_W - CHEVRON_AREA_W);
    lMissing.Height := baseBannerH - 12;
    ScaleLbl(lMissing, taLeftJustify, tlCenter);
    calculatedFontSize := lMissing.Font.Size;

    if WarnShowDetails and (detailCaption <> '') then
    begin
      pnWarnLast.Caption := detailCaption;
      pnWarnLast.Font.Size := Max(8, Min(14, calculatedFontSize div 2));
      pnWarnLast.Alignment := taLeftJustify;
      pnWarnLast.Layout := tlCenter;
      pnWarnLast.AutoSize := false;
      pnWarnLast.Left   := GLYPH_AREA_W;
      pnWarnLast.Top    := baseBannerH;
      pnWarnLast.Width  := pnWarning.Width - GLYPH_AREA_W - 8;
      pnWarnLast.Height := Max(0, expandedExtraH - 4);
      pnWarnLast.Visible := FWarnExpanded;
    end
    else
      pnWarnLast.Visible := false;
  end;
end;

procedure TfBG.showWarningPanel(const message: string;
  severity: TWarnSeverity = wsCritical;
  clearDisplayValues: boolean = false; showDetails: boolean = true);
var
  tintColor: TColor;
  opacityTmp: byte;
  layout: TWarnLayout;
begin
  FWarnSeverity := severity;
  FWarnExpanded := false;
  WarnShowDetails := showDetails;
  FWarnMessage := message;
  // The critical tint is stage-derived, so the stage has to be current before
  // GetWarnSeverityVisual is asked for a color.
  if severity = wsCritical then
    FStaleStage := CurrentStaleStage;

  GetWarnSeverityVisual(severity, tintColor, opacityTmp, layout);
  pnWarning.Color := tintColor;

  // The caption labels cover most of the panel and TGraphicControl doesn't
  // pass clicks up to its parent, so without this the expand chevron only
  // responds on the slivers of bare panel around the text.
  lMissing.OnClick := @pnWarningClick;
  pnWarnLast.OnClick := @pnWarningClick;
  if WarnShowDetails then
  begin
    pnWarning.Cursor := crHandPoint;
    lMissing.Cursor := crHandPoint;
    pnWarnLast.Cursor := crHandPoint;
  end
  else
  begin
    pnWarning.Cursor := crDefault;
    lMissing.Cursor := crDefault;
    pnWarnLast.Cursor := crDefault;
  end;

  pnWarning.Visible := true;
  tPing.Enabled := true;

  // Pulse on wsSoon: piggyback on tProgress (1 Hz) instead of allocating
  // another timer. tProgressTimer handles both pnNextProgress and pnWarning.
  // When severity is not wsSoon, the pulse is no longer needed — disable the
  // timer unless pnNextProgress is still relying on it (mirrors hideWarningPanel).
  if severity = wsSoon then
    tProgress.Enabled := true
  else if Assigned(tProgress) and tProgress.Enabled and
       (not (Assigned(pnNextProgress) and pnNextProgress.Visible)) then
    tProgress.Enabled := false;

  // On the stale card ApplyStaleDisplayState already owns lVal — it writes the
  // dimmed hero from lastDataReading, including the RS_HIGH / RS_LOW captions,
  // which this digit test would otherwise throw away for a dash.
  if clearDisplayValues and (layout <> wlStatusCard) then
    if (not CaptionStartsWithDigit(lVal.Caption)) or (lArrow.Caption = 'lArrow') then
    begin
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;

  pnWarning.Caption := '';
  fixWarningPanel;
  ApplyAlphaControl(pnWarning, opacityTmp);

  pnWarning.BringToFront;
  pnWarning.Refresh;
  lMissing.Refresh;
end;

procedure TfBG.hideWarningPanel;
begin
  pnWarning.Visible := false;
  FWarnExpanded := false;
  // Stop tProgress only if pnNextProgress doesn't still need it. The pulse was
  // the only reason it might be on while pnNextProgress was hidden.
  if Assigned(tProgress) and tProgress.Enabled and
     (not (Assigned(pnNextProgress) and pnNextProgress.Visible)) then
    tProgress.Enabled := false;
end;
