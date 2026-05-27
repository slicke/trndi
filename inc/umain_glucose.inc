(* Glucose update pipeline, data fetching, UI state and color logic for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

// Request data from the backend and update GUI
procedure TfBG.ProcessCurrentReading;
var
  reading: BGReading;
  havePrev: boolean;
  gapMin: integer;
  deltaVal: double;

function FormatSigned(const Value: double): string;
  var
    signStr: string;
    fmtStr: string;
  begin
    if Value = 0 then
      signStr := '±'
    else
    if Value > 0 then
      signStr := '+'
    else
      signStr := '';

    fmtStr := StringReplace(BG_MSG_SIG_SHORT[un], '%+', signStr, [rfReplaceAll]);
    Result := SysUtils.Format(fmtStr, [Value]);
  end;
begin
  if firstboot then
    exit;
  reading := lastReading;

  if not privacyMode then
  begin
    if reading.val > api.limitHI then
      lVal.Caption := RS_HIGH
    else
    if reading.val < api.limitLO then
      lVal.Caption := RS_LOW
    else
      lVal.Caption := reading.format(un, BG_MSG_SHORT, BGPrimary);
  end
  else
    lVal.Caption := '';

  lval.hint := lval.Caption;

  havePrev := (Length(bgs) >= 2) and (not bgs[0].empty) and (not bgs[1].empty);
  if havePrev then
    gapMin := MinutesBetween(bgs[0].date, bgs[1].date)
  else
    gapMin := MaxInt;

  if (not havePrev) or (gapMin >= (INTERVAL_MINUTES * DELTA_MAX)) then
    lDiff.Caption := '--'
  else
  if reading.deltaEmpty then
  begin
    deltaVal := bgs[0].convert(un) - bgs[1].convert(un);
    lDiff.Caption := FormatSigned(deltaVal);
  end
  else
    lDiff.Caption := reading.format(un, BG_MSG_SIG_SHORT, BGDelta);
  lArrow.Caption := reading.trend.Img;
  lVal.Font.Style := [];

  TrndiDLog(Format(RS_LATEST_READING, [reading.val, DateTimeToStr(reading.date)]));

  if native.GetBoolSetting('main.announce', false) then
    speakReading;

  SetNextUpdateTimer(reading.date);
end;

function TfBG.FinalizeReadingUpdate(const Boot: boolean): boolean;
begin
  Result := IsDataFresh;
  if (not Result) and (not Boot) then
    Exit;

  FinalizeUIUpdate;

  if pnWarning.Visible then
    fixWarningPanel;

  {$ifdef TrndiExt}
  callFunc('fetchCallback',[
    bgs[0].format(mgdl, BG_MSG_SHORT),
    bgs[0].format(mmol, BG_MSG_SHORT),
    bgs[0].format(mgdl, BG_MSG_SIG_SHORT, BGDelta),
    bgs[0].format(mmol, BG_MSG_SHORT, BGDelta),
    not bgs[0].empty
    ]);
  {$endif}
end;

function TfBG.IsDataFresh: boolean;
var
  reading: BGReading;
begin
  Result := false;
  if firstboot then
    Exit(true);

  reading := lastReading;

  Result := MinutesBetween(Now, reading.date) <= native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);

  if not Result then
  begin
    tMissed.OnTimer(tMissed);
    lVal.Font.Style := [fsStrikeOut];
    if (not CaptionStartsWithDigit(lVal.Caption)) or (lArrow.Caption = 'lArrow') then
    begin
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;
    setColorMode(clBlack);
    lVal.Font.Color := clWhite;
    tMissed.Enabled := true;
    lArrow.Caption := '';
    native.setBadge('--', clBlack, badge_width, badge_font);
  end
  else
  begin
    tMissed.Enabled := false;
    if lVal.Font.Color = clWhite then
      lVal.Font.Color := GetTextColorForBackground(fBG.color);
  end;
end;

procedure TfBG.SetNextUpdateTimer(const LastReadingTime: TDateTime);
var
  secondsSinceLast: int64;
  intervalMs: int64;
begin
  if firstboot then
    exit;
  tMain.Enabled := false;
  secondsSinceLast := SecondsBetween(LastReadingTime, now);
  intervalMs := min(BG_REFRESH, BG_REFRESH - (secondsSinceLast * 1000));
  intervalMs := max(MIN_REFRESH_INTERVAL_MS, intervalMs);

  tMain.Interval := intervalMs + REFRESH_RESYNC_BUFFER_MS;
  tMain.Enabled := true;

  miRefresh.Caption := Format(RS_REFRESH_PLAIN, [TimeToStr(LastReadingTime),
    TimeToStr(IncMilliSecond(Now, tMain.Interval))]);
  {$if DEFINED(X_MAC) OR DEFINED(X_LINUXBSD)}
  if assigned(upmenu) then
    upMenu.Caption:= miRefresh.Caption;
  {$endif}
end;

// Calculate time in range
procedure tfBG.CalcRangeTime;
var
  reading: BGReading;
  range,
  ok,
  no,
  rangeMinutes: integer;
  cutoffTime: TDateTime;
  ranges: set of trndi.types.BGValLevel;
  meanStr: string;
begin
  ok := 0;
  no := 0;
  if native.GetBoolSetting('range.custom', false) then
    ranges := [BGRange, BGRangeHI, BGRangeLO]
  else
    ranges := [BGRange];

  rangeMinutes := native.GetIntSetting('range.time', 9999);
  cutoffTime := IncMinute(Now, -rangeMinutes);

  for reading in bgs do
    if reading.date >= cutoffTime then
      if reading.level in ranges then
        Inc(ok)
      else
        Inc(no);

  if (ok + no) > 0 then
  begin
    range := round((ok / (ok + no)) * 100);
    if tir_icon then
      case range of
      100..MaxInt:
        lTir.Caption := '👌';
      80..99:
        lTir.Caption := '😃';
      50..79:
        lTir.Caption := '😶';
      30..49:
        lTir.Caption := '😥';
      10..29:
        lTir.Caption := '😞';
      0..9:
        lTir.Caption := '😓';
      end
    else
      lTir.Caption := range.toString + '%';
  end
  else
  begin
    range := 0;
    lTir.Caption := range.toString + '-%';
  end;

  if native.GetBoolSetting('tir.displaymean', false) then
  begin
    meanStr := BGMean(un, -1);
    if meanStr <> '' then
      lTir.Caption := meanStr + ' | ' + lTir.Caption;
  end;

  lTir.Hint := range.toString;
  lTir.Visible := true;
end;

function TfBG.updateReading(boot: boolean = false): boolean;
begin
  {$ifdef DEBUG}
  if debug_load_text then
  begin
    lVal.Caption := '<Updating>';
    Application.ProcessMessages;
  end;
  {$endif}
  Result := false;
  lAgo.Caption := '⟳' + lAgo.Caption;

  native.start;
  lastup := 0;

  if not tAgo.Enabled then
    lAgo.Caption := '🕑 ' + RS_UNKNOWN_TIME;

  {$ifdef DEBUG}
  if debug_load_text then
  begin
    lVal.Caption := '<Fetching>';
    Application.ProcessMessages;
  end;
  {$endif}
  if FForceRefresh then
  begin
    if not FetchAndValidateReadingsForced then
      Exit;
    FForceRefresh := false;
  end
  else
  if not FetchAndValidateReadings then
    Exit;

  {$ifdef DEBUG}
  if debug_load_text then
  begin
    lVal.Caption := '<Processing>';
    Application.ProcessMessages;
  end;
  {$endif}
  ProcessCurrentReading;

  Result := FinalizeReadingUpdate(boot);
end;

procedure TfBG.FinalizeUpdate;
begin
  lastup := Now;

  if privacyMode then
  begin
    lval.Caption := privacyIcon(lastReading.level);
    lVal.hint := lval.Caption;
  end;

  tAgo.Enabled := true;
  tAgo.OnTimer(self);
  if Assigned(pnNextProgress) and pnNextProgress.Visible then
    tProgress.Enabled := true;
  Self.OnResize(lVal);

  UpdateFloatingWindow;

  UpdatePredictionLabel;

  if ShouldUpdateUI(fBG.Color, lVal.Caption, lTir.Caption, lTir.Color) then
  begin
    UpdateUIColors;
    CacheUIState(fBG.Color, lVal.Caption, lTir.Caption, lTir.color);
  end;

  native.setBadge(lVal.Caption, fBG.Color
    {$ifdef lclwin32}
    ,badge_width,badge_font
    {$endif}
    );
  native.WriteCurrentIndicatorCache(lVal.Caption, lastReading.date,
    native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES));
  native.done;

  if privacyMode then
    DOT_ADJUST := randomrange(-3, 3) / 10;
end;

procedure TfBG.UpdatePredictionLabel;
var
  bgr: BGResults;
  pred1, pred2, pred3: string;
  lastReadingTime: TDateTime;
  lastReadingValue: glucose;
  i, closest5, closest10, closest15, closestTarget: integer;
  diff5, diff10, diff15, diffTarget, currentDiff: integer;
  delta: glucose;
  trend: BGTrend;
  minutes, targetMinutes: integer;
  validCount: integer;
  predictionHorizon: integer;
begin
  if FShuttingDown then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  if not Assigned(api) then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  if not PredictGlucoseReading then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  lPredict.Visible := true;

  if (Length(PredictionCache) > 0) and (PredictionLastReadingTime = lastReading.date) then
  begin
    RenderPredictionCache(PredictionCache);
    Exit;
  end;

  if Assigned(FPredictionThread) then
  begin
    if not FPredictionThread.Finished then
      Exit;
    FPredictionThread.WaitFor;
    FreeAndNil(FPredictionThread);
  end;
  predictionHorizon := native.GetIntSetting('predictions.future_limit', DEFAULT_PREDICTION_FUTURE_LIMIT);
  FPredictionThread := TPredictionThread.Create(Self, predictionHorizon);
  lPredict.Caption := RS_PREDICTIONS_UNAVAILABLE;
end;

procedure TfBG.RenderPredictionCache(const bgr: BGResults);
var
  pred1, pred2, pred3: string;
  lastReadingTime: TDateTime;
  lastReadingValue: glucose;
  i, closest5, closest10, closest15, closestTarget: integer;
  diff5, diff10, diff15, diffTarget, currentDiff: integer;
  delta: glucose;
  trend: BGTrend;
  minutes, targetMinutes: integer;
  validCount: integer;
begin
  if not Assigned(api) then
    Exit;

  if not PredictGlucoseReading then
    Exit;

  lastReadingTime := lastReading.date;
  lastReadingValue := lastReading.convert(mgdl);

  closest5 := -1; closest10 := -1; closest15 := -1; closestTarget := -1;
  diff5 := MaxInt; diff10 := MaxInt; diff15 := MaxInt; diffTarget := MaxInt;
  targetMinutes := PredictShortMinutes;

  for i := 0 to High(bgr) do
  begin
    currentDiff := Round(MinutesBetween(bgr[i].date, lastReadingTime));
    if currentDiff < 2 then
      Continue;

    if Abs(currentDiff - 5) < diff5 then
    begin
      diff5 := Abs(currentDiff - 5);
      closest5 := i;
    end;

    if (Abs(currentDiff - 10) < diff10) and (i <> closest5) then
    begin
      diff10 := Abs(currentDiff - 10);
      closest10 := i;
    end;

    if (Abs(currentDiff - 15) < diff15) and (i <> closest5) and (i <> closest10) then
    begin
      diff15 := Abs(currentDiff - 15);
      closest15 := i;
    end;

    if Abs(currentDiff - targetMinutes) < diffTarget then
    begin
      diffTarget := Abs(currentDiff - targetMinutes);
      closestTarget := i;
    end;
  end;

  validCount := 0;
  if closest5 >= 0 then
    Inc(validCount);
  if closest10 >= 0 then
    Inc(validCount);
  if closest15 >= 0 then
    Inc(validCount);
  if validCount = 0 then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  if api.stablePrediction and not PredictShortMode then
  begin
    lPredict.Caption := RS_PREDICTIONS_STABLE;
    if DIFF_ALIGN = taRightJustify then
      ScaleLbl(lPredict, taCenter, tlBottom, false)
    else
      ScaleLbl(lPredict, taRightJustify, tlBottom, false);
    lPredict.Left := ClientWidth - lPredict.Width - 5;
    Exit;
  end;

  if PredictShortMode then
  begin
    if closestTarget >= 0 then
    begin
      delta := bgr[closestTarget].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      minutes := Round(MinutesBetween(bgr[closestTarget].date, lastReadingTime));

      if PredictShortShowValue then
      begin
        if PredictShortFullArrows then
          lPredict.Caption := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closestTarget].convert(un)])
        else
        begin
          case trend of
          TdDoubleUp, TdSingleUp, TdFortyFiveUp:
            lPredict.Caption := Format('⏱%d'' ↗ %.1f', [minutes, bgr[closestTarget].convert(un)]);
          TdFlat:
            lPredict.Caption := Format('⏱%d'' → %.1f', [minutes, bgr[closestTarget].convert(un)]);
          TdFortyFiveDown, TdSingleDown, TdDoubleDown:
            lPredict.Caption := Format('⏱%d'' ↘ %.1f', [minutes, bgr[closestTarget].convert(un)]);
          else
            lPredict.Caption := '?';
          end;
        end;
      end
      else
      begin
        if PredictShortFullArrows then
          lPredict.Caption := BG_TREND_ARROWS_UTF[trend]
        else
          case trend of
          TdDoubleUp, TdSingleUp, TdFortyFiveUp:
            lPredict.Caption := '↗';
          TdFlat:
            lPredict.Caption := '→';
          TdFortyFiveDown, TdSingleDown, TdDoubleDown:
            lPredict.Caption := '↘';
          else
            lPredict.Caption := '?';
          end;
      end;
    end
    else
      lPredict.Caption := '?';
  end
  else
  begin
    if closest5 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest5].date, lastReadingTime));
      delta := bgr[closest5].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred1 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest5].convert(un)]);
    end
    else
      pred1 := '?';

    if closest10 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest10].date, lastReadingTime));
      delta := bgr[closest10].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred2 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest10].convert(un)]);
    end
    else
      pred2 := '?';

    if closest15 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest15].date, lastReadingTime));
      delta := bgr[closest15].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred3 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest15].convert(un)]);
    end
    else
      pred3 := '?';

    lPredict.Caption := Format('%s | %s | %s', [pred1, pred2, pred3]);
  end;

  if lPredict.Visible then
  begin
    if DIFF_ALIGN = taRightJustify then
      ScaleLbl(lPredict, taCenter, tlBottom, false)
    else
      ScaleLbl(lPredict, taRightJustify, tlBottom, false);
    lPredict.Left := ClientWidth - lPredict.Width - 5;
  end;
end;

procedure TfBG.CompleteUIUpdate;
begin
  ProcessCurrentReading;
  UpdateUIBasedOnGlucose;
  FinalizeUpdate;
  CalcRangeTime;
end;

procedure TfBG.FinalizeUIUpdate;
begin
  UpdateUIBasedOnGlucose;
  FinalizeUpdate;
  CalcRangeTime;
end;

procedure TfBG.UpdateFloatingWindow;
begin
  if Assigned(fFloat) then
  begin
    fFloat.Color := fBg.Color;
    fFloat.lVal.Caption := lval.Caption;
    fFloat.lVal.Visible := true;
    fFloat.lArrow.Caption := lArrow.Caption;
    fFloat.lArrow.Visible := fFloat.lArrow.Caption <> '';
    if fFloat.miFontMain.Checked then
    begin
      fFloat.lVal.font.color := lval.font.color;
      fFloat.larrow.font.color := fFloat.lVal.font.color;
    end;
    fFloat.pnMultiUser.Visible := pnMultiUser.Visible;
    if fFloat.pnMultiUser.Visible then
      fFloat.pnMultiUser.Color := pnMultiUser.Color;
    fFloat.FormResize(fFloat);
    fFloat.lArrow.Caption := lArrow.Caption;
    fFloat.lArrow.Visible := fFloat.lArrow.Caption <> '';
    fFloat.lVal.Visible := true;
    if fFloat.miFontMain.Checked then
    begin
      fFloat.lVal.Font.Color := lVal.Font.Color;
      fFloat.lArrow.Font.Color := fFloat.lVal.Font.Color;
    end;
    fFloat.pnMultiUser.Visible := pnMultiUser.Visible;
    if fFloat.pnMultiUser.Visible then
      fFloat.pnMultiUser.Color := pnMultiUser.Color;
  end;
end;

function TfBG.IsSensorFaultSuspected(const Readings: array of BGReading): boolean;
const
  REQUIRED_CHANGES = 4;
  SIGNIFICANT_DELTA_MGDL = 54;
var
  i: integer;
  samplesChecked: integer;
  deltaAbs: double;
  currVal, prevVal: double;
  gapMin: integer;
begin
  Result := false;

  if Length(Readings) < (REQUIRED_CHANGES + 1) then
    Exit;

  samplesChecked := 0;
  for i := 0 to REQUIRED_CHANGES - 1 do
  begin
    if Readings[i].empty or Readings[i + 1].empty then
      Exit;

    gapMin := Abs(MinutesBetween(Readings[i].date, Readings[i + 1].date));
    if gapMin > (INTERVAL_MINUTES * DELTA_MAX) then
      Exit;

    currVal := Readings[i].convert(mgdl);
    prevVal := Readings[i + 1].convert(mgdl);
    deltaAbs := Abs(currVal - prevVal);

    if deltaAbs < SIGNIFICANT_DELTA_MGDL then
      Exit;

    Inc(samplesChecked);
  end;

  Result := samplesChecked = REQUIRED_CHANGES;
end;

procedure TfBG.UpdateUIBasedOnGlucose;
var
  reading: BGReading;
  col: TColor;
  txt: string;
  sensorFault: boolean;
begin
  txt := '';

  if api = nil then
    Exit;

  reading := lastReading;

  if reading.val >= api.cgmHi then
    HandleHighGlucose(reading)
  else
  if reading.val <= api.cgmLo then
    HandleLowGlucose(reading)
  else
    HandleNormalGlucose(reading);

  sensorFault := IsSensorFaultSuspected(bgs);
  if sensorFault then
  begin
    if akSensorFault in FAlertEngine.EvaluateSensorFault then
      native.attention(RS_ATTENTION_SENSOR_FAULT, RS_ATTENTION_SENSOR_FAULT_DESC);
  end
  else
    FAlertEngine.ResetSensorFault;

  if pnWarning.Visible then
    Exit;

  pnOffReading.Visible := native.GetBoolSetting('ux.off_bar', false);
  case reading.level of
  trndi.types.BGHigh:
    txt := RS_HIGH;
  trndi.types.BGLOW:
    txt := RS_LOW;
  trndi.types.BGRange:
  begin
    txt := '';
    pnOffReading.Visible := false;
  end;
  trndi.types.BGRangeHI:
    txt := RS_OFF_HI;
  trndi.types.BGRangeLO:
    txt := RS_OFF_LO;
  end;

  pnOffReading.Caption := txt;

  pnOffReading.color := GetAdjustedColorForBackground(fBG.Color, fBG.Color);
  pnOffReading.font.color := GetTextColorForBackground(pnOffReading.Color, 0, 0.7);
end;

procedure TfBG.HandleHighGlucose(const reading: BGReading);
var
  url: string;
  defaultAction: string;
  fired: TAlertKindSet;
begin
  setColorMode(bg_color_hi);

  fired := FAlertEngine.EvaluateLevel(reading.val);
  if not (akHigh in fired) then
    Exit;

  if native.getBoolSetting('alerts.notice.hilo', true) then
    native.attention(ifthen(multi, multinick, RS_WARN_BG_HI_TITLE),
      Format(RS_WARN_BG_HI, [lVal.Caption]));

  if native.GetBoolSetting('media.pause') then
    actOnMediaController(mcaPause);

  if native.TryGetSetting('media.url_high', url) then
    actOnMediaController(mcaURL, url);

  if native.TryGetSetting('url_remote.url_high', url) then
    native.getURL(url, url);

  if assigned(native) and assigned(chroma) then
    if native.GetBoolSetting('razer.enabled', false) and chroma.Initialized then
    begin
      {$ifdef Windows}
      defaultAction := 'static';
      {$else}
      defaultAction := 'breath';
      {$endif}
      ApplyChromaAlertAction('razer.high.action', defaultAction, clRazerRed);
    end;
end;

procedure TfBG.HandleLowGlucose(const reading: BGReading);
var
  url: string;
  defaultAction: string;
  fired: TAlertKindSet;
begin
  SetColorMode(bg_color_lo);

  fired := FAlertEngine.EvaluateLevel(reading.val);
  if (fired * [akLow, akUrgentLow]) = [] then
    Exit;

  if native.getBoolSetting('alerts.notice.hilo', true) then
    native.attention(ifthen(multi, multinick, RS_WARN_BG_LO_TITLE),
      Format(RS_WARN_BG_LO, [lVal.Caption]));

  if native.GetBoolSetting('media.pause') then
    actOnMediaController(mcaPause);

  if native.TryGetSetting('media.url_low', url) then
    actOnMediaController(mcaURL, url);

  if native.TryGetSetting('url_remote.url_low', url) then
    native.getURL(url, url);

  if assigned(native) and assigned(chroma) then
    if native.GetBoolSetting('razer.enabled', false) and chroma.Initialized then
    begin
      {$ifdef Windows}
      defaultAction := 'static';
      {$else}
      defaultAction := 'breath';
      {$endif}
      ApplyChromaAlertAction('razer.low.action', defaultAction, clRazerBlue);
    end;
end;

procedure TfBG.HandleNormalGlucose(const reading: BGReading);
var
  formattedReading, url: string;
  parsedInt: integer;
  parsedFloat: single;
  go: boolean = false;
begin
  SetColorMode(bg_color_ok);
  FAlertEngine.ResetLevelAlerts;

  if un = mmol then
  begin
    formattedReading := reading.format(mmol, BG_MSG_SHORT, BGPrimary);
    if (TryStrToFloat(formattedReading, parsedFloat, native.locale)) and (parsedFloat = 5.5) then
      go := true
    else
      perfectTriggered := false;
  end
  else
  begin
    formattedReading := reading.format(mgdl, BG_MSG_SHORT, BGPrimary);
    if (TryStrToInt(formattedReading, parsedInt)) and (parsedInt = 100) then
      go := true
    else
      perfectTriggered := false;
  end;

  if go and (not perfectTriggered) then
  begin
    perfectTriggered := true;

    if native.TryGetSetting('media.url_perfect', url) then
      actOnMediaController(mcaURL, url);

    if native.TryGetSetting('url_remote.url_high', url) then
      native.getURL(url, url);
  end;

  if assigned(native) and assigned(chroma) then
    if chroma.Initialized and native.GetBoolSetting('razer.enabled', false) and native.GetBoolSetting('razer.normal', false) then
      Chroma.SetStaticAll(clRazerGreen);

  UpdateOffRangePanel(reading.val);
end;

function TfBG.DoFetchAndValidateReadings(const ForceRefresh: boolean): boolean;
var
  {$ifdef DEBUG}
  res: string;
  {$endif}
  msg: string;
const
  API_CACHE_SECONDS = 10;
begin
  Result := false;

  if api = nil then
    Exit;

  if (not ForceRefresh) and (SecondsBetween(Now, FLastAPICall) < API_CACHE_SECONDS) and
    FCachedReadingsValid then
  begin
    EnterCriticalSection(FReadingsLock);
    try
      if FCachedReadingsValid and (Length(FCachedReadings) > 0) then
      begin
        SetLength(bgs, Length(FCachedReadings));
        Move(FCachedReadings[0], bgs[0], Length(FCachedReadings) * SizeOf(BGReading));
        Result := true;
        Exit;
      end;
    finally
      LeaveCriticalSection(FReadingsLock);
    end;
  end;

  {$ifdef DEBUG}
  bgs := api.getReadings(MAX_MIN, MAX_RESULT, '', res);
  if miDebugBackend.Checked then
    if Showing then
      if res.IsEmpty then
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'),
          '[empty!]', res)
      else
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'),
          '', res, uxmtCustom, 10);
  {$ELSE}
  bgs := api.getReadings(MAX_MIN, MAX_RESULT);
  {$endif}

  FLastAPICall := Now;

  if (Length(bgs) < 1) and FCachedReadingsValid then
  begin
    FLastFetchHadData := false;
    EnterCriticalSection(FReadingsLock);
    try
      if FCachedReadingsValid and (Length(FCachedReadings) > 0) then
      begin
        SetLength(bgs, Length(FCachedReadings));
        Move(FCachedReadings[0], bgs[0], Length(FCachedReadings) * SizeOf(BGReading));
      end;
    finally
      LeaveCriticalSection(FReadingsLock);
    end;
    TrndiDLog('DoFetchAndValidateReadings: API returned no data, using cached readings');
    FLastConnectionDetail := Trim(api.errormsg);
    if FLastConnectionDetail = '' then
      FLastConnectionDetail := RS_NO_BACKEND;
    SetConnectionBadge(ClassifyConnectionStatus(api.errormsg), RGBToColor(185, 60, 45));
    tPing.Enabled := true;
    tPingTimer(nil);
  end
  else
  if Length(bgs) > 0 then
  begin
    FLastFetchHadData := true;
    EnterCriticalSection(FReadingsLock);
    try
      SetLength(FCachedReadings, Length(bgs));
      Move(bgs[0], FCachedReadings[0], Length(bgs) * SizeOf(BGReading));
      FCachedReadingsValid := true;
    finally
      LeaveCriticalSection(FReadingsLock);
    end;
    tPing.Enabled := false;
    FLastConnectionDetail := '';
    SetConnectionBadge(RS_CONN_OK, RGBToColor(60, 150, 90));
  end;

  if native.GetBoolSetting('override.enabled') then
  begin
    api.cgmLo := native.GetIntSetting('override.lo', api.cgmLo);
    api.cgmHi := native.GetIntSetting('override.hi', api.cgmHi);
  end;
  if native.GetBoolSetting('override.range') then
  begin
    api.cgmRangeLo := native.GetIntSetting('override.rangelo', api.cgmRangeLo);
    api.cgmRangeHi := native.GetIntSetting('override.rangehi', api.cgmRangeHi);
  end;

  hideWarningPanel;

  if (Length(bgs) < 1) or (not IsDataFresh) then
  begin
    msg := RS_NO_BACKEND;
    if Assigned(api) and (Trim(api.errormsg) <> '') then
    begin
      msg := msg + sLineBreak + api.errormsg;
      TrndiDLog('Backend error: ' + api.errormsg);
    end;

    FLastConnectionDetail := msg;
    SetConnectionBadge(ClassifyConnectionStatus(api.errormsg), RGBToColor(185, 60, 45));

    if (Length(bgs) > 0) then
    try
    except
    end;

    showWarningPanel(msg, true);

    if akMissing in FAlertEngine.EvaluateMissing then
      native.attention(RS_ATTENTION_MISSING, RS_ATTENTION_MISSING_DESC);
    Exit;
  end;

  FAlertEngine.ResetMissing;

  TrndiDLog(Format('DoFetchAndValidateReadings: Got %d readings from API', [Length(bgs)]));

  PlaceTrendDots(bgs);
  Result := true;
end;

function TfBG.FetchAndValidateReadings: boolean;
begin
  TrndiDLog('umain: About to call native.updateBegin');
  native.updateBegin;
  try
    Result := DoFetchAndValidateReadings(false);
  finally
    native.updateDone;
  end;
end;

function TfBG.FetchAndValidateReadingsForced: boolean;
begin
  TrndiDLog('umain: About to call native.updateBegin (force)');
  native.updateBegin;
  try
    Result := DoFetchAndValidateReadings(true);
  finally
    native.updateDone;
  end;
end;

procedure TfBG.UpdateOffRangePanel(const Value: double);
var
  on: boolean = true;
begin
  if (Value >= api.cgmHi) or (Value <= api.cgmLo) then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
    if Assigned(fFloat) then
    begin
      ffloat.lRangeDown.Visible := false;
      ffloat.lRangeUp.Visible := false;
    end;
  end
  else
  if Value <= api.cgmRangeLo then
    DisplayLowRange
  else
  if Value >= api.cgmRangeHi then
    DisplayHighRange
  else
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
    on := false;
  end;

  if on and UseRangeColor then
    SetColorMode(pnOffRange.Color);
end;

procedure TfBG.UpdateUIColors;
const
  clGoodGreen = $00005900;
var
  r: integer;
begin
  lVal.Font.Color := GetTextColorForBackground(fBG.color);
  lArrow.Font.Color := LightenColor(fBG.color, 0.5);
  lArrow.SendToBack;
  lDiff.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lAgo.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lTir.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lPredict.Font.Color := GetTextColorForBackground(fBG.color, 0.7, 0.4);

  if TryStrToInt(lTir.hint, r) then
    if r < bad_tir then
      lTir.Font.color := GetAdjustedColorForBackground(clMaroon,
        fBG.Color, 0.6, 0.4, true)
    else
    if r >= good_tir then
      lTir.Font.color := GetAdjustedColorForBackground(clGoodGreen,
        fBG.Color, 0.6, 0.4, true);
end;

function TfBG.GetTextColorForBackground(const BgColor: TColor;
const DarkenFactor: double = 0.5; const LightenFactor: double = 0.3): TColor;
begin
  if native.GetBoolSetting('main.high_contrast', false) then
  begin
    if IsLightColor(BgColor) then
      Result := clBlack
    else
      Result := clWhite;
    exit;
  end;
  if IsLightColor(BgColor) then
    Result := DarkenColor(BgColor, DarkenFactor)
  else
    Result := LightenColor(BgColor, LightenFactor);
end;

function TfBG.GetAdjustedColorForBackground(const BaseColor: TColor;
const BgColor: TColor; const DarkenFactor: double = 0.6;
const LightenFactor: double = 0.4; const PreferLighter: boolean = false): TColor;
begin
  if PreferLighter then
    Result := LightenColor(BaseColor, LightenFactor)
  else
  if IsLightColor(BgColor) then
    Result := DarkenColor(BaseColor, DarkenFactor)
  else
    Result := LightenColor(BaseColor, LightenFactor);
end;

function TfBG.BlendFontColorWithBackground(const ForeColor: TColor;
const BgColor: TColor; const AlphaRatio: double): TColor;
begin
  Result := lclintf.RGB(
    Round(GetRValue(ForeColor) * (1 - AlphaRatio) + GetRValue(BgColor) * AlphaRatio),
    Round(GetGValue(ForeColor) * (1 - AlphaRatio) + GetGValue(BgColor) * AlphaRatio),
    Round(GetBValue(ForeColor) * (1 - AlphaRatio) + GetBValue(BgColor) * AlphaRatio));
end;

procedure TfBG.RenderWarningLabel(const LabelControl: TLabel; const P: TPanel;
const BgColor: TColor; const AlphaRatio: double);
var
  relX, relY, textW: integer;
begin
  if not (LabelControl.Visible and (LabelControl.Caption <> '')) then
    Exit;

  relX := LabelControl.Left - P.Left;
  relY := LabelControl.Top - P.Top;

  with P.Canvas do
  begin
    Font.Assign(LabelControl.Font);
    Font.Color := BlendFontColorWithBackground(LabelControl.Font.Color, BgColor, AlphaRatio);
    Brush.Style := bsClear;

    textW := TextWidth(LabelControl.Caption);

    case LabelControl.Alignment of
    taLeftJustify:
      TextOut(relX, relY, LabelControl.Caption);
    taRightJustify:
      TextOut(relX + LabelControl.Width - textW, relY, LabelControl.Caption);
    taCenter:
      TextOut(relX + (LabelControl.Width - textW) div 2, relY, LabelControl.Caption);
    end;

    Brush.Style := bsSolid;
  end;
end;

procedure TfBG.DisplayLowRange;
begin
  pnOffRange.Color := bg_rel_color_lo;
  pnOffRangeBar.Color := bg_rel_color_lo;
  pnOffRange.Font.Color := bg_rel_color_lo_txt;
  if UseRangeColor then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else
  begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
    setColorMode;
  end;
  pnOffRangeBar.Visible := true;
  pnOffRange.Caption := Format('↧ %s ↧', [RS_OFF_LO]);

  if Assigned(fFloat) then
  begin
    if not UseRangeColor then
      ffloat.lRangeDown.Visible := true;
    ffloat.Font.color := bg_rel_color_lo_txt;
  end;
  CenterPanelToCaption(pnOffRange);
end;

procedure TfBG.DisplayHighRange;
begin
  pnOffRange.Color := bg_rel_color_hi;
  pnOffRangeBar.Color := bg_rel_color_hi;
  pnOffRange.Font.Color := bg_rel_color_hi_txt;
  if UseRangeColor then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else
  begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
    setColorMode;
  end;
  pnOffRange.Caption := Format('↥ %s ↥', [RS_OFF_HI]);

  if Assigned(fFloat) then
  begin
    if not UseRangeColor then
      ffloat.lRangeUp.Visible := true;
    ffloat.Font.color := bg_rel_color_hi_txt;
  end;
  CenterPanelToCaption(pnOffRange);
end;

// PlaceTrendDots method to map readings to TrendDots
procedure TfBG.PlaceTrendDots(const Readings: array of BGReading);
var
  SortedReadings: array of BGReading;
  currentTime: TDateTime;
  currentHash: cardinal;
begin
  if Length(Readings) = 0 then
    Exit;

  currentHash := CalculateReadingsHash(Readings);
  if currentHash = FLastReadingsHash then
    Exit;

  FLastReadingsHash := currentHash;

  currentTime := Now;
  SetLength({%H-}SortedReadings, Length(Readings));
  Move(Readings[0], SortedReadings[0], Length(Readings) * SizeOf(BGReading));

  SortReadingsDescending(SortedReadings);

  HandleLatestReadingFreshness(SortedReadings[0], currentTime);

  ProcessTimeIntervals(SortedReadings, currentTime);
end;

procedure TfBG.HandleLatestReadingFreshness(const LatestReading: BGReading;
CurrentTime: TDateTime);
var
  isFresh: boolean;
begin
  isFresh := MinutesBetween(CurrentTime, LatestReading.date) <=
    native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);

  if (Length(TrendDots) > ActiveDots) and Assigned(TrendDots[ActiveDots]) then
  begin
    TrendDots[ActiveDots].Visible := isFresh;

    if isFresh then
      TrndiDLog(Format('TrendDots[%d] shown as latest reading is fresh.', [ActiveDots]))
    else
      TrndiDLog(Format('TrendDots[%d] hidden due to outdated reading.', [ActiveDots]));
  end;
end;

procedure TfBG.ProcessTimeIntervals(const SortedReadings: array of BGReading;
CurrentTime: TDateTime);
var
  slotIndex, i, labelNumber, searchStart: integer;
  slotStart, slotEnd, anchorTime: TDateTime;
  slotEpsilon: double;
  found: boolean;
  reading: BGReading;
  l: TDotControl;
const
  TIME_EPSILON_DAYS      = 10 / 86400;
  RECENT_SLOT_GRACE_DAYS = 60 / 86400;
begin
  if Length(SortedReadings) = 0 then
    Exit;

  searchStart := 0;

  anchorTime := RecodeMinute(CurrentTime,
    (MinuteOf(CurrentTime) div INTERVAL_MINUTES) * INTERVAL_MINUTES);
  anchorTime := RecodeSecond(anchorTime, 0);
  anchorTime := RecodeMilliSecond(anchorTime, 0);

  TrndiDLog(Format('PlaceTrendDots: Processing %d readings, anchor=%s (latest=%s)',
    [Length(SortedReadings), DateTimeToStr(anchorTime), DateTimeToStr(SortedReadings[0].date)]));

  for slotIndex := 0 to ActiveDots - 1 do
  begin
    slotEnd := IncMinute(anchorTime, -INTERVAL_MINUTES * slotIndex);
    slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

    if slotIndex = 0 then
      slotEpsilon := RECENT_SLOT_GRACE_DAYS
    else
      slotEpsilon := TIME_EPSILON_DAYS;

    found := false;

    TrndiDLog(Format('Searching slot %d (TrendDots[%d]): %s to %s',
      [slotIndex, ActiveDots - slotIndex, DateTimeToStr(slotStart), DateTimeToStr(slotEnd)]));

    for i := searchStart to High(SortedReadings) do
    begin
      reading := SortedReadings[i];

      if reading.empty then
      begin
        TrndiDLog(Format('  Reading at %s is empty (curr=BG_NO_VAL), skipping',
          [DateTimeToStr(reading.date)]));
        Continue;
      end;

      if reading.date > slotEnd + slotEpsilon then
      begin
        TrndiDLog(Format('  Reading at %s is too new (>%.3f sec after slot end), skipping',
          [DateTimeToStr(reading.date), (reading.date - slotEnd) * 86400]));
        Continue;
      end;

      if (reading.date > slotStart) and (reading.date <= slotEnd + slotEpsilon) then
      begin
        TrndiDLog(Format('  Found match at %s (value: %.1f, diff from slotEnd: %.1f sec)',
          [DateTimeToStr(reading.date), reading.val, (slotEnd - reading.date) * 86400]));
        found := UpdateLabelForReading(slotIndex, reading);
        if found then
        begin
          searchStart := i + 1;
          Break;
        end;
        if PredictShortSize >= 3 then
          lPredict.Font.Style := [fsBold]
        else
          lPredict.Font.Style := [];
      end
      else
      if reading.date < slotStart - (10 / 86400) then
      begin
        TrndiDLog(Format('  Reading at %s is too old (%.3f sec before slot start), stopping',
          [DateTimeToStr(reading.date), (slotStart - reading.date) * 86400]));
        Break;
      end;
    end;

    if not found then
    begin
      labelNumber := ActiveDots - slotIndex;
      if Length(TrendDots) <= labelNumber then Continue;
      l := TrendDots[labelNumber];

      if Assigned(l) then
      begin
        l.Visible := false;
        l.Hint := '';
        TrndiDLog(Format('TrendDots[%d] hidden as no reading found in interval.',
          [labelNumber]));
      end;
    end;
  end;

  TrndiDLog(Format('PlaceTrendDots complete: anchor=%s', [DateTimeToStr(anchorTime)]));
end;

function TfBG.BGMean(const UnitPref: BGUnit = BGUnit.mmol; const NumReadings: integer = NUM_DOTS): string;
var
  SortedReadings: array of BGReading;
  slotIndex, i, searchStart: integer;
  slotStart, slotEnd, anchorTime: TDateTime;
  reading: BGReading;
  sum: double;
  count: integer;
  v: double;
  slots: integer;
const
  TIME_EPSILON_DAYS = 10 / 86400;
begin
  if Length(bgs) = 0 then
  begin
    Result := '';
    Exit;
  end;

  sum := 0.0;
  count := 0;

  if NumReadings = -1 then
  begin
    SortedReadings := bgs;
    for i := 0 to High(SortedReadings) do
    begin
      reading := SortedReadings[i];
      if reading.empty then
        Continue;
      sum := sum + reading.convert(UnitPref);
      count := count + 1;
    end;
  end
  else
  begin
    SetLength(SortedReadings, Length(bgs));
    Move(bgs[0], SortedReadings[0], Length(bgs) * SizeOf(BGReading));
    SortReadingsDescending(SortedReadings);

    slots := NumReadings;
    if slots > ActiveDots then
      slots := ActiveDots;
    if slots <= 0 then
      slots := ActiveDots;

    anchorTime := RecodeMinute(Now, (MinuteOf(Now) div INTERVAL_MINUTES) * INTERVAL_MINUTES);
    anchorTime := RecodeSecond(anchorTime, 0);
    anchorTime := RecodeMilliSecond(anchorTime, 0);
    searchStart := 0;

    for slotIndex := 0 to slots - 1 do
    begin
      slotEnd := IncMinute(anchorTime, -INTERVAL_MINUTES * slotIndex);
      slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

      for i := searchStart to High(SortedReadings) do
      begin
        reading := SortedReadings[i];
        if reading.empty then
          Continue;
        if reading.date > slotEnd + TIME_EPSILON_DAYS then
          Continue;
        if (reading.date > slotStart) and (reading.date <= slotEnd + TIME_EPSILON_DAYS) then
        begin
          sum := sum + reading.convert(UnitPref);
          count := count + 1;
          searchStart := i + 1;
          Break;
        end
        else
        if reading.date < slotStart - TIME_EPSILON_DAYS then
        begin
          searchStart := i;
          Break;
        end;
      end;
    end;
  end;

  if count > 0 then
    v := sum / count
  else
    v := 0.0;

  Result := Format(BG_MSG_SHORT[UnitPref], [v]);
end;

function TfBG.UpdateLabelForReading(SlotIndex: integer;
const Reading: BGReading): boolean;
var
  labelNumber: integer;
  l: TDotControl;
  H, M, S, MS: word;
begin
  Result := false;
  if firstboot then
    Exit;

  labelNumber := ActiveDots - SlotIndex;
  if Length(TrendDots) <= labelNumber then Exit;
  l := TrendDots[labelNumber];

  if Assigned(l) then
  begin
    l.Visible := true;
    l.Hint := Reading.format(un, BG_MSG_SHORT, BGPrimary);

    DecodeTime(reading.date, H, M, S, MS);
    l.Tag := H * 100 + M;

    l.Caption := DOT_GRAPH;

    {$ifdef DARWIN}
    l.AdjustSize;
    {$endif}

    setPointHeight(l, Reading.convert(mmol), l.Parent.ClientHeight);

    l.Font.Color := DetermineColorForReading(Reading);

    TrndiDLog(Format('TrendDots[%d] updated with reading at %s (Value: %.2f).',
      [labelNumber, DateTimeToStr(Reading.date), Reading.val]));
    l.BringToFront;
    Result := true;
  end;
end;

function TfBG.DetermineColorForReading(const Reading: BGReading): TColor;
begin
  if native.GetBoolSetting('main.high_contrast', false) then
  begin
    result := GetTextColorForBackground(fbg.color);
    if result = clBlack then
      result := $00C8C8C8
    else
      result := $00626262;
    Exit;
  end;

  if Reading.val >= api.cgmHi then
    Result := bg_color_hi
  else
  if Reading.val <= api.cgmLo then
    Result := bg_color_lo
  else
  begin
    Result := bg_color_ok_txt;

    if Reading.val <= api.cgmRangeLo then
      Result := bg_rel_color_lo
    else
    if Reading.val >= api.cgmRangeHi then
      Result := bg_rel_color_hi;
  end;

  Result := LightenColor(Result, -0.8);
end;

function TfBG.setColorMode: boolean;
begin
  Result := setColorMode(clFuchsia, true);
end;

function TfBG.setColorMode(bg: tColor; const nocolor: boolean = false): boolean;
begin
  if not nocolor then
    fBG.Color := bg;

  if not multi
    and titlecolor
    and UseRangeColor then
  begin
    Result := SetSingleColorMode;
    Exit;
  end
  else
  if not UseRangeColor
    and pnOffRange.Visible then
    if native.SetTitleColor(handle, pnOffRange.Color,
      IfThen(IsLightColor(pnOffRange.Color), clBlack, clWhite)) then
      Exit(true);

  if customTitleBar and (pnMultiUser.Color <> clBlack) then
    if native.SetTitleColor(handle, pnMultiUser.Color,
      IfThen(IsLightColor(pnMultiUser.Color), clBlack, clWhite)) then
      Exit(true);

  if native.isDarkMode then
    native.setDarkMode
  {$ifdef windows}
    (self.Handle)
  {$endif}
  ;

  Result := false;
end;

function TfBG.setSingleColorMode: boolean;
begin
  Result := native.SetTitleColor(handle, fBG.color,
    IfThen(IsLightColor(fBG.color), clBlack, clWhite));
end;

function TfBG.CalculateReadingsHash(const Readings: array of BGReading): cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(Readings) do
    Result := Result xor cardinal(Trunc(Readings[i].val * 100)) xor
      cardinal(Trunc(Readings[i].date * 86400));
end;

function TfBG.ShouldUpdateUI(const NewColor: TColor; const NewCaption: string;
const NewTir: string; const NewTirColor: TColor): boolean;
begin
  Result := (NewColor <> FLastUIColor) or (NewCaption <> FLastUICaption) or
    (NewTir <> FLastTir) or (NewTirColor <> FLastTirColor);
end;

procedure TfBG.CacheUIState(const UIColor: TColor; const UICaption: string;
const UITir: string; const UITirColor: TColor);
begin
  FLastUIColor := UIColor;
  FLastUICaption := UICaption;
  FLastTir := UITir;
  FLastTirColor := UITirColor;
end;

procedure TfBG.CheckForUpdates(ShowUpToDateMessage: boolean = false);
var
  res, rn, r, newVersionMessage: string;
  rok, parsed: boolean;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  latestRelease: string;
  lastIgnoreDate: string;
  ignoreDate: TDateTime;
  daysSinceIgnore: integer;
  fmt: TFormatSettings;
begin
  try
    if not ShowUpToDateMessage then
    begin
      lastIgnoreDate := native.GetSetting('update.ignore.date', '');
      if lastIgnoreDate <> '' then
      begin
        parsed := false;
        if TryStrToDateTime(lastIgnoreDate, ignoreDate) then
          parsed := true
        else
        if Length(Trim(lastIgnoreDate)) >= 19 then
        try
          ignoreDate := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', Copy(Trim(lastIgnoreDate), 1, 19));
          parsed := true;
        except
          parsed := false;
        end;

        if parsed then
        begin
          daysSinceIgnore := DaysBetween(Now, ignoreDate);
          if daysSinceIgnore < 14 then
            Exit;
        end
        else
          TrndiDLog(Format('CheckForUpdates: failed to parse update.ignore.date: %s', [lastIgnoreDate]));
      end;
    end;

    TrndiNative.getURL('https://api.github.com/repos/slicke/trndi/releases/latest', res);

    latestRelease := 'Unknown';
    JsonData := nil;
    try
      JsonData := GetJSON(res);
      if Assigned(JsonData) and (JsonData is TJSONObject) then
      begin
        JsonObj := TJSONObject(JsonData);
        latestRelease := JsonObj.Get('name', JsonObj.Get('tag_name', 'Unknown'));
      end;
    except
      latestRelease := 'Parse error';
    end;

    if Assigned(JsonData) then
      FreeAndNil(JsonData);

    rok := HasNewerRelease(res, rn, false);

    if rok then
    begin
      r := GetNewerVersionURL(res);
      if r = '' then
        r := 'https://github.com/slicke/trndi/releases/latest';
      newVersionMessage := Format(RS_NEWVER, [rn]);
      case UXDialog(uxdAuto, RS_NEWVER_CAPTION, newVersionMessage, [mbYes, mbNo, mbIgnore], mtInformation) of
      mrYes:
        OpenURL(r);
      mrIgnore:
      begin
        native.SetSetting('update.ignore.date', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
        ShowMessage(Format(RS_UPDATE_SNOOZE, [DateTimeToStr(IncDay(Now, 14))]));
      end;
      end;
    end
    else
    if ShowUpToDateMessage then
      if BUILD_NUMBER = 'dev' then
        ShowMessage('Up to date (Dev Build)' + LineEnding +
          'Build: ' + BUILD_NUMBER + LineEnding +
          'Branch: ' + GIT_BRANCH + LineEnding +
          'Build Date: ' + {$I %DATE%} + ' ' + {$I %TIME%} + LineEnding +
          'Latest release: ' + latestRelease + LineEnding + LineEnding +
          'Note: Dev builds are always considered "newer" than releases')
      else
        ShowMessage(RS_UPTODATE);

  except
    on E: Exception do
      if ShowUpToDateMessage then
        ShowMessage('Update check failed: ' + E.Message);
  end;
end;

procedure TfBG.actOnMediaController(act: TMediaControllerAct; arg: string = '');
begin
  if native.GetBoolSetting('media.disabled', false) then
    Exit;
  if not assigned(mediacontroller) then
    Exit;
  if not mediacontroller.IsInitialized then
    Exit;

  case act of
  mcaPlay:
    MediaController.Play;
  mcaPause:
    MediaController.Pause;
  mcaURL:
    MediaController.PlayTrackFromURL(arg);
  end;
end;
