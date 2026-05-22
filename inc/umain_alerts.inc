(* Warning overlay panel, connection badge, and alert snooze for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

function TfBG.GetControlAbsolutePos(const Ctrl: TControl): TPoint;
var
  cur: TControl;
begin
  Result.X := 0;
  Result.Y := 0;
  cur := Ctrl;
  while Assigned(cur) do
  begin
    Inc(Result.X, cur.Left);
    Inc(Result.Y, cur.Top);
    cur := cur.Parent;
  end;
end;

function TfBG.GetPanelClientPos(const Ctrl: TControl; const Panel: TPanel): TPoint;
var
  ctrlPos: TPoint;
  panelPos: TPoint;
begin
  ctrlPos := GetControlAbsolutePos(Ctrl);
  panelPos := GetControlAbsolutePos(Panel);
  Result.X := ctrlPos.X - panelPos.X;
  Result.Y := ctrlPos.Y - panelPos.Y;
end;

function TfBG.PointInsideRoundedRect(const X, Y, W, H, R: integer): boolean;
var
  dx, dy: integer;
  rr: integer;
begin
  if (X < 0) or (Y < 0) or (X >= W) or (Y >= H) then
    Exit(False);

  if ((X >= R) and (X < W - R)) or ((Y >= R) and (Y < H - R)) then
    Exit(True);

  rr := R * R;

  if (X < R) and (Y < R) then
  begin
    dx := X - R;
    dy := Y - R;
    Exit((dx * dx + dy * dy) <= rr);
  end;

  if (X >= W - R) and (Y < R) then
  begin
    dx := X - (W - R - 1);
    dy := Y - R;
    Exit((dx * dx + dy * dy) <= rr);
  end;

  if (X < R) and (Y >= H - R) then
  begin
    dx := X - R;
    dy := Y - (H - R - 1);
    Exit((dx * dx + dy * dy) <= rr);
  end;

  if (X >= W - R) and (Y >= H - R) then
  begin
    dx := X - (W - R - 1);
    dy := Y - (H - R - 1);
    Exit((dx * dx + dy * dy) <= rr);
  end;

  Result := True;
end;

procedure TfBG.PaintWarningOverlay(const P: TPanel; const Radius: integer;
  const PanelFillColor: TColor; const AlphaRatio: double);
begin
  // Warning labels are now rendered as overlay controls via SyncWarningOverlayLabels
end;

procedure TfBG.EnsureWarningDots;
var
  i: integer;
begin
  if Length(FWarningDots) < ActiveDots + 1 then
    SetLength(FWarningDots, ActiveDots + 1);
  for i := 1 to ActiveDots do
    if not Assigned(FWarningDots[i]) then
    begin
      {$ifdef DARWIN}
      FWarningDots[i] := TLabel.Create(Self);
      with TLabel(FWarningDots[i]) do
      begin
        Parent := pnWarning;
        AutoSize := true;
        Alignment := taCenter;
        Layout := tlCenter;
        Transparent := true;
      end;
      {$else}
      FWarningDots[i] := TPaintBox.Create(Self);
      with TPaintBox(FWarningDots[i]) do
      begin
        Parent := pnWarning;
        AutoSize := false;
        OnPaint := @DotPaint;
      end;
      {$endif}

      with FWarningDots[i] do
      begin
        Visible := false;
        Caption := DOT_GRAPH;
        PopupMenu := pmSettings;
        Cursor := crHandPoint;
        OnClick := @onTrendClick;
      end;
    end;
end;

procedure TfBG.EnsureWarningLabels;
var
  labels: array[1..4] of TLabel;
  i: integer;
begin
  labels[1] := lArrow;
  labels[2] := lVal;
  labels[3] := lDiff;
  labels[4] := lAgo;

  for i := 1 to 4 do
    if not Assigned(FWarningLabels[i]) then
    begin
      FWarningLabels[i] := TLabel.Create(Self);
      with FWarningLabels[i] do
      begin
        Parent := pnWarning;
        AutoSize := false;
        Transparent := true;
        Visible := false;
      end;
    end;
end;

procedure TfBG.SyncWarningOverlayDots(const P: TPanel);
var
  i: integer;
  srcDot, dstDot: TDotControl;
  dotPos: TPoint;
  centerX, centerY: integer;
begin
  EnsureWarningDots;

  if Length(TrendDots) <= ActiveDots then Exit;
  for i := 1 to ActiveDots do
  begin
    srcDot := TrendDots[i];
    dstDot := FWarningDots[i];

    if (srcDot = nil) or (dstDot = nil) then
      Continue;

    if srcDot.Visible then
    begin
      dotPos := GetPanelClientPos(srcDot, P);
      dstDot.Parent := P;
      dstDot.Caption := srcDot.Caption;
      dstDot.Hint := srcDot.Hint;
      dstDot.ShowHint := srcDot.ShowHint or (srcDot.Hint <> '');
      dstDot.Tag := srcDot.Tag;
      dstDot.Font.Assign(srcDot.Font);
      dstDot.Width := srcDot.Width;
      dstDot.Height := srcDot.Height;
      dstDot.Left := dotPos.X;
      dstDot.Top := dotPos.Y;

      centerX := dstDot.Left + (dstDot.Width div 2);
      centerY := dstDot.Top + (dstDot.Height div 2);
      if PointInsideRoundedRect(centerX, centerY, P.Width, P.Height, 20) then
      begin
        dstDot.Visible := true;
        dstDot.BringToFront;
      end
      else
        dstDot.Visible := false;
    end
    else
      dstDot.Visible := false;
  end;
end;

procedure TfBG.SyncWarningOverlayLabels(const P: TPanel);
var
  srcLabels: array[1..4] of TLabel;
  i: integer;
  srcLbl, dstLbl: TLabel;
  lblPos: TPoint;
  centerX, centerY: integer;
begin
  EnsureWarningLabels;

  srcLabels[1] := lArrow;
  srcLabels[2] := lVal;
  srcLabels[3] := lDiff;
  srcLabels[4] := lAgo;

  for i := 1 to 4 do
  begin
    srcLbl := srcLabels[i];
    dstLbl := FWarningLabels[i];

    if (srcLbl = nil) or (dstLbl = nil) then
      Continue;

    if srcLbl.Visible and (srcLbl.Caption <> '') then
    begin
      lblPos := GetPanelClientPos(srcLbl, P);
      dstLbl.Parent := P;
      dstLbl.Caption := srcLbl.Caption;
      dstLbl.Font.Assign(srcLbl.Font);
      if i <= 2 then
        dstLbl.Font.Color := BlendColors(srcLbl.Font.Color, P.Color, 0.25)
      else
        dstLbl.Font.Color := BlendColors(srcLbl.Font.Color, P.Color, 0.6);
      dstLbl.Alignment := srcLbl.Alignment;
      dstLbl.Layout := srcLbl.Layout;
      dstLbl.Width := srcLbl.Width;
      dstLbl.Height := srcLbl.Height;
      dstLbl.Left := lblPos.X;
      dstLbl.Top := lblPos.Y;

      centerX := dstLbl.Left + (dstLbl.Width div 2);
      centerY := dstLbl.Top + (dstLbl.Height div 2);
      if PointInsideRoundedRect(centerX, centerY, P.Width, P.Height, 20) then
      begin
        dstLbl.Visible := true;
        if i <= 2 then
          dstLbl.SendToBack
        else
          dstLbl.BringToFront;
      end
      else
        dstLbl.Visible := false;
    end
    else
      dstLbl.Visible := false;
  end;
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
    sensorText := bgs[High(bgs)].sensor;

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
  lInternet.Canvas.Font.Assign(lInternet.Font);
  contentWidth := lInternet.Canvas.TextWidth(DisplayStatus) + badgeTextPadding;
  maxBadgeWidth := Max(96, round(ClientWidth * 0.42));
  lInternet.Width := Min(maxBadgeWidth, Max(96, contentWidth));
  lInternet.Height := Max(28, round(ClientHeight * 0.058));
  ScaleLbl(lInternet, taCenter, tlCenter, true);

  if IsCentered then
  begin
    lInternet.Anchors := [];
    lInternet.Left := Max(8, (ClientWidth - lInternet.Width) div 2);
    lInternet.Top := 8;
  end
  else
  begin
    lInternet.Anchors := [akTop, akRight];
    lInternet.Top := 8;
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

procedure TfBG.fixWarningPanel;
var
  padding: integer;
  calculatedFontSize: integer;
  bg: BGReading;
  last, val: string;
begin
  calculatedFontSize := 12;
  padding := (ClientWidth div 25);
  if not native.HasTouchScreen then
    padding := padding * 2;

  pnwarning.AutoSize := false;
  pnwarning.Width := ClientWidth - (padding * 2);
  pnWarning.left := padding;
  pnwarning.top := padding;
  pnWarning.Height := ClientHeight - (padding * 2);

  lMissing.Caption := '⚠️ ' + FWarnMessage + sLineBreak;

  lMissing.AutoSize := false;
  lMissing.left := 5;
  lMissing.top := 5;
  lMissing.Width := pnWarning.Width - 10;
  lMissing.Height := pnWarning.Height - 60;
  lMissing.wordwrap := true;
  lMissing.OptimalFill := true;

  ScaleLbl(lMissing, taCenter, tlCenter);
  calculatedFontSize := lMissing.Font.Size;

  if native.HasTouchScreen then
    pnWarning.Font.Color := pnWarning.color;

  if WarnShowDetails then
  begin
    if tryLastReading(bg) then
    begin
      val := bg.format(un, BG_MSG_SHORT);
      if val[1] = '-' then
      begin
        bg := lastDataReading;
        val := bg.format(un, BG_MSG_SHORT);
      end;

      last := FormatDateTime('hh:nn', bg.date);
      if DateOf(bg.date) <> Dateof(now) then
        last := last + ' ' + Format(RS_DAYS_AGO, [DaysBetween(now, bg.date)]);
      pnWarnLast.Caption := Format(RS_LAST_RECIEVE, [val, last]);
    end
    else
      pnWarnLast.Caption := RS_LAST_RECIEVE_NO;

    pnWarnLast.Caption := pnWarnLast.Caption + LineEnding +
      Format(RS_LAST_RECIEVE_AGE, [native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES)]);
  end
  else
    pnWarnLast.Caption := '';

  pnWarnLast.font.size := Max(8, Min(20, calculatedFontSize div 3));
  pnWarning.Canvas.Font.size := pnWarnLast.font.size;

  pnWarnLast.Height := pnWarning.Canvas.TextHeight(pnWarnLast.Caption) * 2;
  pnWarnLast.Width := Min(pnWarning.Width - 10,
    pnWarning.Canvas.TextWidth(pnWarnLast.Caption) + 10);
  pnWarnLast.left := 5;
  pnWarnLast.top := pnWarning.Height - pnWarnLast.Height - 5;

  SyncWarningOverlayDots(pnWarning);
  SyncWarningOverlayLabels(pnWarning);
end;

procedure TfBG.showWarningPanel(const message: string;
clearDisplayValues: boolean = false; opacity: integer = 235; showDetails: boolean = true);
begin
  pnWarning.Visible := true;
  tPing.Enabled := true;

  WarnShowDetails := showDetails and (opacity >= 150);

  FWarnMessage := message;
  lMissing.Caption := '⚠️ ' + message + sLineBreak;

  if clearDisplayValues then
    if (not CaptionStartsWithDigit(lVal.Caption)) or (lArrow.Caption = 'lArrow') then
    begin
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;

  pnWarning.Caption := '';
  fixWarningPanel;
  LAST_WARN_OPACITY := opacity;
  ApplyAlphaControl(pnWarning, opacity);

  pnWarning.BringToFront;

  pnWarning.Refresh;
  lMissing.Refresh;
end;

procedure TfBG.hideWarningPanel;
begin
  pnWarning.Visible := false;
end;
