(* Timer handlers and layout helpers for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

procedure TfBG.tAgoTimer(Sender: TObject);
var
  d: TDateTime;
  min: int64;
  showTimestamp: boolean;
  timeStr: string;
  ix: integer;

  // For prediction time updates
procedure UpdatePredictionTimes;
  var
    i, closest5, closest10, closest15, closestTarget: integer;
    diff5, diff10, diff15, diffTarget, currentDiff: integer;
    minutes, targetMinutes: integer;
    delta: glucose;
    trend: BGTrend;
    pred1, pred2, pred3: string;
    lastReadingValue: single;
    validCount: integer;
  begin
    if not (PredictGlucoseReading and lPredict.Visible and (Length(PredictionCache) > 0)) then
      Exit;

    lastReadingValue := lastReading.convert(mgdl);

    // Find predictions closest to 5, 10, 15 and the configured short horizon from NOW
    closest5 := -1;
    closest10 := -1;
    closest15 := -1;
    closestTarget := -1;
    diff5 := MaxInt;
    diff10 := MaxInt;
    diff15 := MaxInt;
    diffTarget := MaxInt;
    targetMinutes := PredictShortMinutes;

    for i := 0 to High(PredictionCache) do
    begin
      // Calculate how many minutes from NOW this prediction is
      currentDiff := Round(MinutesBetween(PredictionCache[i].date, Now));

      if (currentDiff >= 0) and (Abs(currentDiff - 5) < diff5) then
      begin
        diff5 := Abs(currentDiff - 5);
        closest5 := i;
      end;

      if (currentDiff >= 0) and (i <> closest5) and (Abs(currentDiff - 10) < diff10) then
      begin
        diff10 := Abs(currentDiff - 10);
        closest10 := i;
      end;

      if (currentDiff >= 0) and (i <> closest5) and (i <> closest10) and (Abs(currentDiff - 15) < diff15) then
      begin
        diff15 := Abs(currentDiff - 15);
        closest15 := i;
      end;

      if (currentDiff >= 0) and (Abs(currentDiff - targetMinutes) < diffTarget) then
      begin
        diffTarget := Abs(currentDiff - targetMinutes);
        closestTarget := i;
      end;
    end;

    // Check if we have at least one valid prediction
    validCount := 0;
    if closest5 >= 0 then
      Inc(validCount);
    if closest10 >= 0 then
      Inc(validCount);
    if closest15 >= 0 then
      Inc(validCount);

    if validCount = 0 then
      Exit;

    // Update prediction display with current times
    if PredictShortMode then
    begin
      // Short mode: show only configured-horizon prediction with arrow
      if closestTarget >= 0 then
      begin
        delta := PredictionCache[closestTarget].convert(mgdl) - lastReadingValue;
        trend := CalculateTrendFromDelta(delta);
        minutes := Round(MinutesBetween(PredictionCache[closestTarget].date, Now));

        if PredictShortShowValue then
          if PredictShortFullArrows then
            lPredict.Caption := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], PredictionCache[closestTarget].convert(un)])
          else
            case trend of
            TdDoubleUp, TdSingleUp, TdFortyFiveUp:
              lPredict.Caption := Format('⏱%d'' ↗ %.1f', [minutes, PredictionCache[closestTarget].convert(un)]);
            TdFlat:
              lPredict.Caption := Format('⏱%d'' → %.1f', [minutes, PredictionCache[closestTarget].convert(un)]);
            TdFortyFiveDown, TdSingleDown, TdDoubleDown:
              lPredict.Caption := Format('⏱%d'' ↘ %.1f', [minutes, PredictionCache[closestTarget].convert(un)]);
            else
              lPredict.Caption := '?';
            end;
        // Arrow-only mode doesn't show time, so no update needed
      end;
    end
    else
    begin
      // Full mode: show time, trend, and value for all three
      if closest5 >= 0 then
      begin
        minutes := Round(MinutesBetween(PredictionCache[closest5].date, Now));
        delta := PredictionCache[closest5].convert(mgdl) - lastReadingValue;
        trend := CalculateTrendFromDelta(delta);
        pred1 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], PredictionCache[closest5].convert(un)]);
      end
      else
        pred1 := '?';

      if closest10 >= 0 then
      begin
        minutes := Round(MinutesBetween(PredictionCache[closest10].date, Now));
        delta := PredictionCache[closest10].convert(mgdl) - lastReadingValue;
        trend := CalculateTrendFromDelta(delta);
        pred2 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], PredictionCache[closest10].convert(un)]);
      end
      else
        pred2 := '?';

      if closest15 >= 0 then
      begin
        minutes := Round(MinutesBetween(PredictionCache[closest15].date, Now));
        delta := PredictionCache[closest15].convert(mgdl) - lastReadingValue;
        trend := CalculateTrendFromDelta(delta);
        pred3 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], PredictionCache[closest15].convert(un)]);
      end
      else
        pred3 := '?';

      lPredict.Caption := pred1 + ' | ' + pred2 + ' | ' + pred3;
    end;

    // Keep prediction label geometry in sync with dynamic caption updates.
    if lPredict.Visible then
    begin
      if DIFF_ALIGN = taRightJustify then
        ScaleLbl(lPredict, taCenter, tlBottom, false)
      else
        ScaleLbl(lPredict, taRightJustify, tlBottom, false);
      // Keep horizontal anchoring stable during timer updates; Top is handled
      // by the main resize/layout path to avoid small vertical jumps.
      lPredict.Left := ClientWidth - lPredict.Width - 5;
    end;
  end;

procedure predictFuture(future: integer = 7);
  var
    bgr: BGResults;
    b: BGReading;
    time, i: integer;
    isLow: TTrndiBool;
    minTime: integer;
    warnLo, warnHi: glucose;
  begin
    if not PredictGlucoseReading then
      Exit;
    if not native.GetBoolSetting('predictions.warn') then
      Exit;

    warnLo := math.Max(api.cgmLo, api.limitLO);
    warnHi := math.Min(api.cgmHi, api.limitHI);

    if lastReading.val <= warnLo then
      Exit
    else
    if lastReading.val >= warnHi then
      Exit;

    if Length(PredictionCache) > 0 then
      bgr := PredictionCache
    else
      api.predictReadings(future, bgr);

    isLow := TTrndiBool.tbUnset;
    minTime := MaxInt;
    for i := Low(bgr) to High(bgr) do
    begin
      b := bgr[i];
      if b.empty then
        Continue;

      if (b.val <= warnLo) or (b.val >= warnHi) then
      begin
        time := Round(MinutesBetween(Now, b.date));
        if time < minTime then
        begin
          minTime := time;
          if b.val <= warnLo then
            isLow := TTrndiBool.tbTrue
          else
            isLow := TTrndiBool.tbFalse;
        end;
      end;
    end;

    if (isLow <> TTrndiBool.tbUnset) and (minTime < MaxInt) then
    begin
      if minTime > 3 then
      begin
        if isLow = TTrndiBool.tbTrue then
          showWarningPanel(Format(RS_LO_PREDICT, [minTime]), false, 50, false)
        else
          showWarningPanel(Format(RS_HI_PREDICT, [minTime]), false, 50, false);
      end
      else
      if isLow = TTrndiBool.tbTrue then
        showWarningPanel(RS_LO_PREDICT_SOON, false, 50, false)
      else
        showWarningPanel(RS_HI_PREDICT_SOON, false, 50, false);
    end;
  end;

begin
  if firstboot then
    exit;

  if Length(bgs) < 1 then
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO
  else
  try
    d := lastReading.date;
    showTimestamp := native.GetBoolSetting('display.timestamp');

    if showTimestamp then
    begin
      timeStr := FormatDateTime('hh:nn', d);
      {$ifndef lclgtk2}
      lAgo.Caption := '🕑 ' + timeStr;
      {$else}
      lAgo.Caption := '⌚ ' + timeStr;
      {$endif}
    end
    else
    begin
      min := MilliSecondsBetween(Now, d) div MILLIS_PER_MINUTE;
      if min < 0 then
        min := 0;
      {$ifndef lclgtk2}
      lAgo.Caption := '🕑 ' + Format(RS_LAST_UPDATE, [min]);
      {$else}
      lAgo.Caption := '⌚ ' + Format(RS_LAST_UPDATE, [min]);
      {$endif}
    end;

    UpdatePredictionTimes;

    ix := native.GetIntSetting('predictions.future_limit', DEFAULT_PREDICTION_FUTURE_LIMIT);
    predictFuture(ix);
  except
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO;
  end;

end;

procedure TfBG.tProgressTimer(Sender: TObject);
begin
  if Assigned(pnNextProgress) and pnNextProgress.Visible then
    pnNextProgress.Repaint;
end;

procedure TfBG.tClockTimer(Sender: TObject);
var
  clockText: string;
  {$ifdef TrndiExt}
  ex: boolean;
  {$endif}
begin
  tClock.Enabled := false;
  if tClock.Interval <> clockDisplay then
  begin
    {$ifdef TrndiExt}
    clockText := '';
    clockText := callFuncWithBGReadings('clockView', [DateTimeToStr(Now)], ex);
    if ex = false then
      lval.caption := FormatDateTime(FormatSettings.ShortTimeFormat, Now)
    else
      lval.caption := clockText;
    {$else}
    lval.Caption := FormatDateTime(FormatSettings.ShortTimeFormat, Now);
    {$endif}
    tClock.Interval := clockDisplay;
    lArrow.Visible := false;
  end
  else
  begin
    lval.Caption := lval.hint;
    tClock.Interval := clockInterval;
    lArrow.Visible := true;
  end;
  tClock.Enabled := true;
end;

procedure TfBG.tEdgesTimer(Sender: TObject);
begin

end;

procedure TfBG.tInitTimer(Sender: TObject);
begin
  tInit.Enabled := false;
  self.Width := self.Width + 1;
end;

procedure TfBG.tPingTimer(Sender: TObject);
begin
  if FShuttingDown then
    Exit;
  tPing.Enabled := false;
  if Sender = miDNS then
  begin
    if Assigned(FConnectivityThread) then
    begin
      if not FConnectivityThread.Finished then
        Exit;
      FConnectivityThread.WaitFor;
      FreeAndNil(FConnectivityThread);
    end;
    FConnectivityThread := TConnectivityCheckThread.Create(Self, true)
  end
  else
  begin
    if Assigned(FPingThread) then
    begin
      if not FPingThread.Finished then
        Exit;
      FPingThread.WaitFor;
      FreeAndNil(FPingThread);
    end;
    FPingThread := TConnectivityCheckThread.Create(Self, false);
  end;
end;

procedure TfBG.tResizeTimer(Sender: TObject);
begin
  tResize.Enabled := false;

  if DraggingWin then
  begin
    if pnWarning.Visible then
    begin
      fixWarningPanel;
      ApplyAlphaControl(pnWarning, LAST_WARN_OPACITY);
    end;
    Exit;
  end;

  actOnTrend(@HideDot);

  UpdateTrendElements;

  if not Assigned(api) then
    Exit;

  UpdateApiInformation;

  if not api.active then
    Exit;

  ResizeUIElements;

  UpdateTrendDots;
  actOnTrend(@showDot);

  AdjustGraph;

  fixWarningPanel;

  if (Length(TrendDots) > 1) and (TrendDots[1].Caption <> DOT_GRAPH) then
    actOnTrend(@ExpandDot);
end;

procedure TfBG.UpdateTrendElements;
begin
  actOnTrend(@ResizeDot);
  actOnTrend(@SetDotWidth);
end;

procedure TfBG.UpdateApiInformation;
begin
  miHi.Caption := Format(RS_HI_LEVEL, [api.cgmHi * BG_CONVERTIONS[un][mgdl]]);
  miLo.Caption := Format(RS_LO_LEVEL, [api.cgmLo * BG_CONVERTIONS[un][mgdl]]);

  if api.cgmRangeHi <> 500 then
    miRangeHi.Caption := Format(RS_RANGE_HI, [api.cgmRangeHi * BG_CONVERTIONS[un][mgdl]])
  else
    miRangeHi.Caption := RS_RANGE_HI_UNSUPPORTED;

  if api.cgmRangeLo <> 0 then
    miRangeLo.Caption := Format(RS_RANGE_LO, [api.cgmRangeLo * BG_CONVERTIONS[un][mgdl]])
  else
  begin
    miRangeLo.Caption := RS_RANGE_LO_UNSUPPORTED;
    UseRangeColor := false;
  end;
end;

procedure TfBG.nextProgressChange;
begin
  if not Assigned(pnNextProgress) then
    Exit;

  lTir.left := fBG.Width - lTir.Width - pnNextProgress.Width - 5;
end;

procedure TfBG.ResizeUIElements;
const
  PRED_WIDTH_NUM: array[1..3] of integer = (6, 8, 9);
  PRED_HEIGHT_NUM: array[1..3] of integer = (2, 3, 4);
var
  wnum, hnum: integer;
begin
  pnOffReading.Height := ClientHeight;
  pnOffReading.ClientWidth := ClientWidth div 35;

  pnOffRange.Height := ClientHeight div 10;

  pnOffRange.Font.Size := 7 + pnOffRange.Height div 5;

  CenterPanelToCaption(pnOffRange);
  pnOffRangeBar.Height := pnOffRange.Height div 4;
  pnOffRangeBar.Width := ClientWidth + 10;

  ScaleLbl(lVal);

  lDiff.Width := ClientWidth;
  lDiff.Height := (ClientHeight div 9) - 10;
  lDiff.Left := 0;
  lDiff.Top := ClientHeight - lDiff.Height + 1;
  ScaleLbl(lDiff, DIFF_ALIGN, tlCenter, true, 10);

  lAgo.Width := ClientWidth div 2;
  lAgo.Height := ClientHeight div 9;
  lAgo.Left := 0;
  lAgo.Top := 1 + IfThen(pnOffRange.Visible, pnOffRange.Height, 3);
  lAgo.Transparent := true;
  ScaleLbl(lAgo, taLeftJustify, tlCenter, true, IfThen(pnNextProgress.Visible, 10, 0));
  lAgo.BringToFront;

  lArrow.Height := ClientHeight;
  lArrow.Width := ClientWidth;
  lArrow.left := 0;
  lArrow.top := 0;
  ScaleLbl(lArrow);

  ScaleLbl(lTir);
  lTir.BringToFront;
  lTir.AutoSize := false;
  lTir.font := lAgo.Font;
  // Measure on a temp TBitmap — TLabel.Canvas outside paint SIGABRTs on Cocoa.
  with TBitmap.Create do
  try
    SetSize(1, 1);
    Canvas.Font.Assign(lTir.Font);
    lTir.Height := Canvas.TextHeight(lTir.Caption);
    lTir.Width := Canvas.TextWidth(lTir.Caption);
  finally
    Free;
  end;

  {$if DEFINED(LCLQt6) AND DEFINED(X_LINUXBSD)}
  if isWSL then
    lTir.Width := 50;
  {$endif}
  lTir.top := 0;

  lArrow.OptimalFill := true;

  pnMultiUser.Width := min(max(clientwidth div 30, 5), 50);
  pnMultiUser.Height := clientheight;
  pnMultiUser.top := 0;
  pnMultiUser.left := 0;

  if Assigned(lPredict) and lPredict.Visible then
  begin
    if PredictShortMode then
    begin
      if PredictShortShowValue then
      begin
        wnum := PRED_WIDTH_NUM[Max(Min(PredictShortSize, 3), 1)];
        hnum := PRED_HEIGHT_NUM[Max(Min(PredictShortSize, 3), 1)];
        lPredict.Width := ((ClientWidth * wnum) div 12) * 2;
        lPredict.Height := ((ClientHeight * hnum) div 12) div 2;
      end
      else
      begin
        wnum := PRED_WIDTH_NUM[Max(Min(PredictShortSize, 3), 1)];
        hnum := PRED_HEIGHT_NUM[Max(Min(PredictShortSize, 3), 1)];
        lPredict.Width := (ClientWidth * wnum) div 12;
        lPredict.Height := (ClientHeight * hnum) div 12;
      end;
    end
    else
    begin
      lPredict.Width := (ClientWidth * 2) div 5;
      lPredict.Height := ClientHeight div 12;
    end;

    lPredict.Left := ClientWidth - lPredict.Width - 5;
    lPredict.Top := ClientHeight - lPredict.Height - 5;

    if DIFF_ALIGN = taRightJustify then
      ScaleLbl(lPredict, taCenter, tlBottom, true)
    else
      ScaleLbl(lPredict, taRightJustify, tlBottom, true);
    lPredict.Left := ClientWidth - lPredict.Width - 5;
  end;
  nextProgressChange;
end;

procedure TfBG.UpdateTrendDots;
var
  Dot: TDotControl;
  Value: glucose;
  ok: boolean;
  wasVisible: boolean;
begin
  for Dot in TrendDots do
  begin
    if not Assigned(Dot) then Continue;
    wasVisible := Dot.Visible;

    ok := TryStrToFloat(Dot.Hint, Value, native.locale);

    if ok then
    begin
      if un = mgdl then
        Value := Value * BG_CONVERTIONS[mmol][mgdl];

      if Assigned(Dot.Parent) then
        SetPointHeight(Dot, Value, Dot.Parent.ClientHeight)
      else
        SetPointHeight(Dot, Value, fBG.ClientHeight);

      Dot.Visible := true;
    end
    else
    if Dot.Hint <> '' then
    begin
      TrndiDLog(Format('Warning: Could not parse Hint "%s" for dot. Keeping visibility=%s',
        [Dot.Hint, BoolToStr(wasVisible, true)]));
      Dot.Visible := wasVisible;
    end
    else
      Dot.Visible := false;
  end;

  if pnWarning.Visible then
    pnWarning.Refresh;
end;

procedure TfBG.ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
customTl: TTextLayout = tlCenter; allowCustom: boolean = true; padding: integer = 0);
var
  Low, High, Mid, i, pW, pH: integer;
  MaxWidth, MaxHeight: integer;
  TextWidth, TextHeight: integer;
  OptimalSize: integer;
  scaleFactor: single;
  oldW, oldH: integer;
  bmp: TBitmap;
begin
  if not ALabel.Visible then
    ALabel.Visible := true;

  if ALabel.Caption = '' then
    Exit;

  if (ALabel.Width <= 0) or (ALabel.Height <= 0) then
  begin
    ALabel.Width := 250;
    ALabel.Height := 250;
  end;

  if allowCustom then
  begin
    scaleFactor := native.GetFloatSetting('ux.labels.' + LowerCase(ALabel.Name) + '.scale', 1);
    if scaleFactor <= 0 then
      scaleFactor := 1;
    if scaleFactor <> 1 then
    begin
      oldW := ALabel.Width;
      oldH := ALabel.Height;
      ALabel.Width  := round(oldW * scaleFactor);
      ALabel.Height := round(oldH * scaleFactor);
      ALabel.Left := ALabel.Left - (ALabel.Width - oldW) div 2;
      ALabel.Top  := ALabel.Top  - (ALabel.Height - oldH) div 2;
      if Assigned(ALabel.Parent) then
      begin
        pW := ALabel.Parent.ClientWidth;
        pH := ALabel.Parent.ClientHeight;

        if ALabel.Width > pW then
          ALabel.Width := pW;
        if ALabel.Height > pH then
          ALabel.Height := pH;

        if ALabel.Left < 0 then
          ALabel.Left := 0;
        if ALabel.Left + ALabel.Width > pW then
          ALabel.Left := Max(0, (pW - ALabel.Width) div 2);

        if ALabel.Top < 0 then
          ALabel.Top := 0;
        if ALabel.Top + ALabel.Height > pH then
          ALabel.Top := pH - ALabel.Height;
      end;
    end;
  end;

  ALabel.AutoSize := false;
  ALabel.WordWrap := false;
  ALabel.Alignment := customAl;
  ALabel.Layout := customTl;

  if ALabel.Font.Color = ALabel.Color then
    ALabel.Font.Color := clBlack;

  MaxWidth := ALabel.Width - 4;
  MaxHeight := ALabel.Height - 4;

  Low := 1;
  High := 150;
  OptimalSize := 1;

  // Measure on a temp TBitmap. TLabel.Canvas outside paint is unsafe on the
  // Cocoa widgetset (TCocoaContext.SetAntialiasing SIGABRTs on a nil ctx).
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(ALabel.Font);

    while Low <= High do
    begin
      Mid := (Low + High) div 2;
      bmp.Canvas.Font.Size := Mid;

      TextWidth := bmp.Canvas.TextWidth(ALabel.Caption);
      TextHeight := bmp.Canvas.TextHeight(ALabel.Caption);

      if (TextWidth <= MaxWidth) and (TextHeight <= MaxHeight) then
      begin
        OptimalSize := Mid;
        Low := Mid + 1;
      end
      else
        High := Mid - 1;
    end;

    ALabel.Font.Size := OptimalSize;
    bmp.Canvas.Font.Size := OptimalSize;
    TextHeight := bmp.Canvas.TextHeight(ALabel.Caption);
    while ((TextWidth > MaxWidth) or (TextHeight > MaxHeight)) and (ALabel.Font.Size > 1) do
    begin
      i := ALabel.Font.Size;
      Dec(i);
      ALabel.Font.Size := i;
      bmp.Canvas.Font.Size := i;
      TextWidth := bmp.Canvas.TextWidth(ALabel.Caption);
      TextHeight := bmp.Canvas.TextHeight(ALabel.Caption);
    end;
  finally
    bmp.Free;
  end;

  oldW := ALabel.Width;
  ALabel.width := ALabel.Width - padding*2;
  if customAl <> taCenter then
    ALabel.left := ALabel.left + padding
  else
    ALabel.left := ALabel.left + ((oldW - ALabel.Width) div 2);

  ALabel.Refresh;
end;

// Update remote on timer
procedure TfBG.tMainTimer(Sender: TObject);
var
  nowTick: TDateTime;
  gapSeconds: int64;
  expectedSeconds: int64;
begin
  nowTick := Now;
  if FLastTimerTick > 0 then
  begin
    gapSeconds := SecondsBetween(nowTick, FLastTimerTick);
    expectedSeconds := Max(1, tMain.Interval div 1000);
    if gapSeconds > (expectedSeconds * 2) then
    begin
      FForceRefresh := true;
      TrndiDLog(Format('Wake detected: timer gap %d sec (expected ~%d sec). Forcing refresh.',
        [gapSeconds, expectedSeconds]));
    end;
  end;
  FLastTimerTick := nowTick;

  updateReading;
  {$ifdef TrndiExt}
  callFuncWithBGReadings('updateCallback', [DateTimeToStr(nowTick)]);
  {$endif}
end;

procedure TfBG.tMissedTimer(Sender: TObject);
var
  nowTick: TDateTime;
  d: TDateTime;
  min, sec: int64;
begin
  if firstboot then
    exit;

  nowTick := Now;
  d := lastReading.date;

  min := MilliSecondsBetween(nowTick, d) div MILLIS_PER_MINUTE;
  if min < 0 then
    min := 0;
  if min > 60000000 then
  begin
    d := lastDataReading.date;
    min := MilliSecondsBetween(nowTick, d) div MILLIS_PER_MINUTE;
    if min < 0 then
      min := 0;
  end;
  sec := (MilliSecondsBetween(nowTick, d) mod MILLIS_PER_MINUTE) div 1000;
  if sec < 0 then
    sec := 0;

  lDiff.Caption := Format(RS_OUTDATED_TIME, [FormatDateTime('hh:nn', d), min, sec]);
end;

procedure TfBG.tSetupTimer(Sender: TObject);
begin
  tSetup.Enabled := false;
  UpdateUIColors;
end;

procedure TfBG.tSwapTimer(Sender: TObject);
var
  c: TColor;
  currentCaption: string;
begin
  if fsStrikeOut in Lval.Font.Style then
  begin
    lArrow.Visible := true;
    Exit;
  end;

  tSwap.Enabled := false;
  currentCaption := lval.Caption;

  if currentCaption.IndexOf(':') > 0 then
    UpdateUIColors
  else
  if lVal.font.color <> lArrow.font.color then
  begin
    lArrow.BringToFront;
    c := lArrow.Font.Color;
    lArrow.Font.color := lVal.Font.color;
    lVal.Font.color := c;
  end
  else
  begin
    UpdateUIColors;
    lArrow.SendToBack;
    lVal.BringToFront;
    lArrow.SendToBack;
  end;
  tSwap.Enabled := true;
end;

// Handle a touch screen's long touch
procedure TfBG.tTouchTimer(Sender: TObject);
var
  p: TPoint;
  touchDuration: integer;
begin
  tTouch.Enabled := false;
  if IsTouched and HasTouch and (StartTouch > 0) then
  begin
    touchDuration := MilliSecondsBetween(Now, StartTouch);
    if touchDuration >= 450 then
    begin
      p := Mouse.CursorPos;
      if SecondsBetween(Now, last_popup) > 2 then
      begin
        last_popup := now;
        if ((not native.HasTouchScreen) or semiTouchMode) then
        begin
          sleep(100);
          Application.ProcessMessages;
          pmSettings.PopUp(p.X, p.Y);
        end
        else
        begin
          pnTouchMenu.Width := Width;
          pnTouchMenu.Height := Height;
          pnTouchMenu.top := 0;
          pnTouchMenu.left := 0;
          pnTouchMenu.Font.Color := clBlack;
          bTouchFull.width := width div 3;
          bTouchSettings.height := height div 4;
          bMenuPanelClose.height := height div 4;
          bTouchFull.font.size := lDiff.Font.size;
          bTouchSettings.font.size := lDiff.Font.size;
          bMenuPanelClose.font.size := lDiff.Font.size;
          bTouchMenu.font.size := lDiff.Font.size;
          pnTouchMenu.Show;
          pnTouchMenu.BringToFront;
          bMenuPanelClose.SetFocus;
        end;
      end;
    end;
  end;
end;
