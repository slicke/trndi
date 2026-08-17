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

  // For prediction time updates
procedure UpdatePredictionTimes;
  var
    i, closest5, closest10, closest15, closestTarget: integer;
    diff5, diff10, diff15, diffTarget, currentDiffSigned: integer;
    minutes, targetMinutes: integer;
    delta: glucose;
    trend: BGTrend;
    pred1, pred2, pred3: string;
    lastReadingValue: single;
    validCount: integer;
    prevCaption: string;
  begin
    if not (PredictGlucoseReading and lPredict.Visible and (Length(PredictionCache) > 0)) then
      Exit;

    prevCaption := lPredict.Caption;
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
      // Signed minutes from NOW: negative = past, positive = future.
      // MinutesBetween is Abs()-based, so a stale past prediction would
      // otherwise win "closest to N" against a real future one.
      currentDiffSigned := Round((PredictionCache[i].date - Now) * MinsPerDay);

      if (currentDiffSigned >= 0) and (Abs(currentDiffSigned - 5) < diff5) then
      begin
        diff5 := Abs(currentDiffSigned - 5);
        closest5 := i;
      end;

      if (currentDiffSigned >= 0) and (i <> closest5) and (Abs(currentDiffSigned - 10) < diff10) then
      begin
        diff10 := Abs(currentDiffSigned - 10);
        closest10 := i;
      end;

      if (currentDiffSigned >= 0) and (i <> closest5) and (i <> closest10) and (Abs(currentDiffSigned - 15) < diff15) then
      begin
        diff15 := Abs(currentDiffSigned - 15);
        closest15 := i;
      end;

      if (currentDiffSigned >= 0) and (Abs(currentDiffSigned - targetMinutes) < diffTarget) then
      begin
        diffTarget := Abs(currentDiffSigned - targetMinutes);
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
    // Only when the caption actually changed — ScaleLbl runs a binary-search
    // font measurement plus a repaint, which is wasted work on the (common)
    // ticks where the displayed minutes haven't moved yet.
    if lPredict.Visible and (lPredict.Caption <> prevCaption) then
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

procedure predictFuture;
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

    // Skip threshold compare until the first fetch has landed; lastReading
    // returns a zero-initialized record when bgs is empty and val=0 would
    // spuriously cross the low threshold.
    if Length(bgs) = 0 then
      Exit;

    warnLo := math.Max(api.cgmLo, api.limitLO);
    warnHi := math.Min(api.cgmHi, api.limitHI);

    if lastReading.val <= warnLo then
      Exit
    else
    if lastReading.val >= warnHi then
      Exit;

    // Use only the cache filled asynchronously by TPredictionThread (via
    // UpdatePredictionLabel after each applied reading). Falling back to
    // api.predictReadings here would run a synchronous HTTP fetch on the
    // main thread from a 15 s timer — same class of bug as the resize-tick
    // fetch removed from tResizeTimer. An empty cache just skips this tick.
    if Length(PredictionCache) = 0 then
      Exit;
    bgr := PredictionCache;

    isLow := TTrndiBool.tbUnset;
    minTime := MaxInt;
    for i := Low(bgr) to High(bgr) do
    begin
      b := bgr[i];
      if b.empty then
        Continue;

      if (b.val <= warnLo) or (b.val >= warnHi) then
      begin
        // Signed minutes: ignore past entries (MinutesBetween is Abs()-based,
        // so a 1-minute-ago warning would otherwise trigger PREDICT_SOON).
        time := Round((b.date - Now) * MinsPerDay);
        if (time > 0) and (time < minTime) then
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
          showWarningPanel(Format(RS_LO_PREDICT, [minTime]), wsInfo, false, false)
        else
          showWarningPanel(Format(RS_HI_PREDICT, [minTime]), wsInfo, false, false);
      end
      else
      if isLow = TTrndiBool.tbTrue then
        showWarningPanel(RS_LO_PREDICT_SOON, wsSoon, false, false)
      else
        showWarningPanel(RS_HI_PREDICT_SOON, wsSoon, false, false);
    end;
  end;

begin
  if firstboot then
    exit;

  if Length(bgs) < 1 then
    lAgo.Caption := CLOCK_CAPTION_PREFIX + RS_COMPUTE_FAILED_AGO
  else
  try
    d := lastReading.date;
    showTimestamp := native.GetBoolSetting('display.timestamp');

    if showTimestamp then
    begin
      timeStr := FormatDateTime('hh:nn', d);
      lAgo.Caption := CLOCK_CAPTION_PREFIX + timeStr;
    end
    else
    begin
      min := MilliSecondsBetween(Now, d) div MILLIS_PER_MINUTE;
      if min < 0 then
        min := 0;
      lAgo.Caption := CLOCK_CAPTION_PREFIX + Format(RS_LAST_UPDATE, [min]);
    end;

    UpdatePredictionTimes;

    predictFuture;
  except
    lAgo.Caption := CLOCK_CAPTION_PREFIX + RS_COMPUTE_FAILED_AGO;
  end;

  // Catch-all activation for the fullscreen clock: covers fullscreen entered
  // behind DoFullScreen's back (e.g. the macOS green button) and the setting
  // being toggled elsewhere. Nearly free — the overlay's own minute gate
  // swallows the call when nothing changed.
  UpdateClockOverlay;
end;

procedure TfBG.tProgressTimer(Sender: TObject);
const
  PROGRESS_TICK_MS = 1000;   // enough to keep a 5-minute bar moving smoothly
  PROGRESS_PULSE_MS = 125;   // frame rate for the overdue breathing pulse
var
  wantMs, nowSec: integer;
begin
  wantMs := PROGRESS_TICK_MS;
  if Assigned(pnNextProgress) and pnNextProgress.Visible then
  begin
    pnNextProgress.Repaint;
    // pnNextProgressPaint sets FProgressPulsing while the reading is overdue;
    // the pulse needs real frames, the rest of the cycle does not.
    if FProgressPulsing then
      wantMs := PROGRESS_PULSE_MS;
  end;
  if tProgress.Interval <> wantMs then
    tProgress.Interval := wantMs;

  // Drive the wsSoon pulse: 1-second tick is enough for the discrete two-tone
  // alternation in pnWarningPaint to feel like a deliberate alert beat — and
  // repainting that whole card at the pulse rate above would only flicker.
  if Assigned(pnWarning) and pnWarning.Visible and (FWarnSeverity = wsSoon) then
  begin
    nowSec := SecondOf(Now);
    if nowSec <> FWarnPulseSecond then
    begin
      FWarnPulseSecond := nowSec;
      pnWarning.Repaint;
    end;
  end;
end;

{------------------------------------------------------------------------------
  UpdateClockOverlay
  ------------------
  The persistent fullscreen clock: time and reading visible at once, which is
  what a wall/bedside display wants — the windowed alternation makes the
  viewer wait out a 20 s cycle to see the time. Shown while main.clock is on
  AND the form is fullscreen (however it got there: menu toggle, --kiosk,
  double-click); windowed mode keeps the classic alternation.

  Label and timer are created on first need and the timer only runs while the
  overlay shows. A minute gate keeps the 1 s tick nearly free: the clockView
  extension call and the ScaleLbl font search only run on a minute flip, on
  first show, or when the caller says geometry changed (ForceLayout).
------------------------------------------------------------------------------}
procedure TfBG.UpdateClockOverlay(const ForceLayout: boolean);
var
  clockText: string;
  {$ifdef TrndiExt}
  ex: boolean;
  {$endif}
begin
  if firstboot or FShuttingDown or (not Assigned(native)) then
    Exit;

  if not (native.GetBoolSetting('main.clock', false) and IsFullscreenNow) then
  begin
    if Assigned(lClock) then
      lClock.Visible := false;
    if Assigned(tClockOverlay) then
      tClockOverlay.Enabled := false;
    Exit;
  end;

  if not Assigned(lClock) then
  begin
    lClock := TLabel.Create(Self);
    lClock.Name := 'lClock'; // gives it a ux.labels.lclock.scale hook
    lClock.Parent := Self;
    lClock.Transparent := true;
    lClock.PopupMenu := pmSettings; // right-click keeps working on the clock
  end;
  if not Assigned(tClockOverlay) then
  begin
    tClockOverlay := TTimer.Create(Self);
    tClockOverlay.Interval := 1000;
    tClockOverlay.OnTimer := @tClockOverlayTimer;
  end;
  tClockOverlay.Enabled := true;

  if lClock.Visible and (FClockOverlayMinute = MinuteOf(Now)) and
    (not ForceLayout) then
    Exit;
  FClockOverlayMinute := MinuteOf(Now);

  // Same text pipeline as the alternating clock below, so a clockView
  // extension styles the overlay and the flash alike.
  {$ifdef TrndiExt}
  // Also fall back when clockView exists but returned no text (undefined/'')
  clockText := callFuncWithBGReadings('clockView', [DateTimeToStr(Now)], ex);
  if (not ex) or (clockText = '') then
    clockText := FormatDateTime(FormatSettings.ShortTimeFormat, Now);
  {$else}
  clockText := FormatDateTime(FormatSettings.ShortTimeFormat, Now);
  {$endif}
  lClock.Caption := clockText;

  // Top-center band, same strip lAgo (left) and lTir (right) occupy; the
  // font mirrors lAgo so UpdateUIColors' color choices carry over.
  lClock.Width := ClientWidth div 4;
  lClock.Height := ClientHeight div 9;
  lClock.Top := TitleBarOffset + 1 + IfThen(pnOffRange.Visible, pnOffRange.Height, 3);
  lClock.Font.Assign(lAgo.Font);
  lClock.Visible := true;
  ScaleLbl(lClock, taCenter, tlCenter, true);
  lClock.Left := (ClientWidth - lClock.Width) div 2;
  lClock.BringToFront;
end;

procedure TfBG.tClockOverlayTimer(Sender: TObject);
begin
  UpdateClockOverlay;
end;

procedure TfBG.tClockTimer(Sender: TObject);
var
  clockText: string;
  {$ifdef TrndiExt}
  ex: boolean;
  {$endif}
begin
  tClock.Enabled := false;
  // While the fullscreen overlay shows the time permanently, flashing the
  // hero over to the clock would just hide the glucose for 5 s per cycle.
  // Normalize back to "reading shown" if the flash was mid-cycle when the
  // overlay appeared, and idle until the overlay goes away again.
  if Assigned(lClock) and lClock.Visible then
  begin
    if tClock.Interval = clockDisplay then
    begin
      lVal.Caption := lVal.Hint;
      tClock.Interval := clockInterval;
      lArrow.Visible := true;
    end;
    tClock.Enabled := true;
    Exit;
  end;
  if tClock.Interval <> clockDisplay then
  begin
    {$ifdef TrndiExt}
    clockText := '';
    clockText := callFuncWithBGReadings('clockView', [DateTimeToStr(Now)], ex);
    // Fall back when no extension handles clockView, and also when a handler
    // exists but returned no text (undefined/'') — otherwise the label went
    // blank (or showed 'undefined' before callFuncWithBGReadings filtered it).
    if (ex = false) or (clockText = '') then
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
      begin
        tPing.Enabled := true;
        Exit;
      end;
      SafeThreadJoin(FConnectivityThread);
      FreeAndNil(FConnectivityThread);
    end;
    FConnectivityThread := TConnectivityCheckThread.Create(Self, true)
  end
  else
  begin
    if Assigned(FPingThread) then
    begin
      if not FPingThread.Finished then
      begin
        tPing.Enabled := true;
        Exit;
      end;
      SafeThreadJoin(FPingThread);
      FreeAndNil(FPingThread);
    end;
    FPingThread := TConnectivityCheckThread.Create(Self, false);
  end;
end;

procedure TfBG.tResizeTimer(Sender: TObject);
var
  resizeWarnTint: TColor;
  resizeWarnAlpha: byte;
  resizeWarnLayout: TWarnLayout;
begin
  tResize.Enabled := false;

  if DraggingWin then
  begin
    if pnWarning.Visible then
    begin
      fixWarningPanel;
      GetWarnSeverityVisual(FWarnSeverity, resizeWarnTint, resizeWarnAlpha, resizeWarnLayout);
      ApplyAlphaControl(pnWarning, resizeWarnAlpha);
    end;
    Exit;
  end;

  actOnTrend(@HideDot);

  UpdateTrendElements;

  if not Assigned(api) then
    Exit;

  UpdateApiInformation;

  // Skip the dot/graph/TIR relayout when there's nothing to draw. Previously
  // this gate read `api.active`, which fired a synchronous HTTP fetch on every
  // resize tick — wrong place to do I/O. `bgs` already reflects the latest
  // applied readings; if it's empty we have nothing to lay out.
  if Length(bgs) = 0 then
    Exit;

  ResizeUIElements;

  UpdateTrendDots;
  actOnTrend(@showDot);

  AdjustGraph;

  fixWarningPanel;

  // Collapse expanded (value-text) dots back to markers after a relayout.
  // DOT_FRESH counts as a marker too — with a 1-dot window TrendDots[1] is the
  // latest dot and may carry the fresh ring rather than the plain dot.
  if (Length(TrendDots) > 1) and (TrendDots[1].Caption <> DOT_GRAPH)
    and (TrendDots[1].Caption <> DOT_FRESH) then
    actOnTrend(@ExpandDot);
end;

procedure TfBG.UpdateTrendElements;
begin
  actOnTrend(@ResizeDot);
  actOnTrend(@SetDotWidth);
  LayoutPredictionDots;
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
    // cgmRangeLo is still the disabled sentinel before the first fetch has
    // connected and pulled thresholds — the boot-time resize tick lands here
    // and must not clobber the user's ux.range_color preference. Only force
    // the feature off once the backend has actually delivered data and the
    // sentinel means "unsupported" rather than "not loaded yet".
    if api.active then
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

  // Everything anchored to the top edge sits below the drawn title bar when
  // one is active (TitleBarOffset is 0 otherwise).
  pnOffRange.Top := TitleBarOffset;
  pnOffRange.Height := ClientHeight div 10;

  pnOffRange.Font.Size := 7 + pnOffRange.Height div 5;

  CenterPanelToCaption(pnOffRange);
  pnOffRangeBar.Top := TitleBarOffset;
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
  lAgo.Top := TitleBarOffset + 1 + IfThen(pnOffRange.Visible, pnOffRange.Height, 3);
  lAgo.Transparent := true;
  // Leave the clock clear of the enabled left-side progress bar at every form width.
  ScaleLbl(lAgo, taLeftJustify, tlCenter, true,
    IfThen(pnNextProgress.Visible, pnNextProgress.Width + 2, 0));
  lAgo.BringToFront;

  lArrow.Height := ClientHeight;
  lArrow.Width := ClientWidth;
  lArrow.left := 0;
  lArrow.top := 0;
  ScaleLbl(lArrow);
  // Keep the rotating arrow overlay tracking lArrow's bounds. ScaleLbl force-
  // shows lArrow, so re-hide the glyph while the vector arrow is active —
  // otherwise both the glyph and the rotating arrow draw ("two shafts").
  if Assigned(FTrendArrow) then
  begin
    FTrendArrow.BoundsRect := lArrow.BoundsRect;
    // Stale data has no current direction to point in. ApplyStaleDisplayState
    // hides the overlay when the outage starts; without this gate the next
    // resize would hand it straight back, since RotatingArrow alone says
    // nothing about whether the reading it points at is still current.
    if FTrendDataStale then
      FTrendArrow.Visible := false
    else
    if RotatingArrow then
    begin
      lArrow.Visible := false;
      FTrendArrow.Visible := true;
    end;
  end;

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
  lTir.top := TitleBarOffset;

  lArrow.OptimalFill := true;

  pnMultiUser.Width := min(max(clientwidth div 30, 5), 50);
  pnMultiUser.Height := clientheight - TitleBarOffset;
  pnMultiUser.top := TitleBarOffset;
  pnMultiUser.left := 0;

  if Assigned(lPredict) and lPredict.Visible then
  begin
    if PredictShortMode then
    begin
      // The largest short-prediction size gets a bold face so the glyph keeps
      // its weight at the bigger footprint.
      if PredictShortSize >= 3 then
        lPredict.Font.Style := [fsBold]
      else
        lPredict.Font.Style := [];
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
      lPredict.Font.Style := [];
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

  // The drawn title bar and its resize grips must stay above the controls the
  // BringToFront calls in this pass may have raised over them.
  if Assigned(FTitleBar) then
    FTitleBar.BringToFront;
  if Assigned(FWindowGrips) then
    FWindowGrips.EnsureOnTop;

  // Keep the fullscreen clock's band and font in step with the labels that
  // were just re-laid out (it mirrors lAgo's font).
  UpdateClockOverlay(true);
end;

procedure TfBG.UpdateTrendDots;
var
  Dot: TDotControl;
  Value: glucose;
  ok: boolean;
  wasVisible: boolean;
  ix: integer;
begin
  // Indexed rather than a for-in: showing a dot needs its slot number so the
  // narrow-window clamp can decide whether this slot is one of the rendered
  // ones. TrendDots is 1-based with index 0 left nil.
  for ix := Low(TrendDots) to High(TrendDots) do
  begin
    Dot := TrendDots[ix];
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

      // Position the dot regardless — only its visibility is clamped, so
      // widening the window brings it back already in the right place.
      Dot.Visible := DotSlotVisible(ix);
    end
    else
    if Dot.Hint <> '' then
    begin
      TrndiDLog(Format('Warning: Could not parse Hint "%s" for dot. Keeping visibility=%s',
        [Dot.Hint, BoolToStr(wasVisible, true)]));
      Dot.Visible := wasVisible and DotSlotVisible(ix);
    end
    else
      Dot.Visible := false;
  end;

  // Keep prediction dots positioned alongside the history dots, or hidden when
  // the dot view is off so they never linger after the mode is disabled.
  if PredictionDotsActive then
    UpdatePredictionDotHeights
  else
    HidePredictionDots;

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
    TextWidth := bmp.Canvas.TextWidth(ALabel.Caption);
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
  wantMs: int64;
begin
  // Re-tune the repeating cadence from the newest reading's age on every tick.
  // SetNextUpdateTimer only runs on the fresh path (ApplyFetchedReadings bails
  // before it when data is stale), so without this the rapid-poll cadence
  // could neither engage during an outage nor stand down once the outage has
  // outgrown RAPID_POLL_MAX_OVERDUE_MS — the timer would just repeat whatever
  // interval was programmed last. Assigning Interval mid-tick restarts the
  // countdown from now, which is exactly the schedule FNextFetchAt records.
  if Length(bgs) > 0 then
  begin
    wantMs := NextFetchIntervalMs(lastReading.date);
    if tMain.Interval <> wantMs then
      tMain.Interval := wantMs;
  end;

  // tMain repeats, so the next attempt is one interval out from here. Recorded
  // for the stale card's countdown (a TTimer exposes no "time remaining");
  // SetNextUpdateTimer overwrites it with the resynced time if data lands.
  FNextFetchAt := IncMilliSecond(Now, tMain.Interval);
  updateReading;
  {$ifdef TrndiExt}
  callFuncWithBGReadings('updateCallback', [DateTimeToStr(Now)]);
  {$endif}
end;

// Fired by trndi.native when the OS reports a resume-from-sleep event.
// Always invoked on the main thread (platform units marshal via
// Application.QueueAsyncCall). Forces the next fetch to bypass the cache
// and kicks updateReading immediately. The network may not be ready yet
// (Wi-Fi reassociation takes a few seconds on resume); the fetch worker
// handles failures, and the existing tMain/tMissed cadence retries
// shortly after.
procedure TfBG.OnSystemWake;
begin
  if FShuttingDown then
    Exit;
  TrndiDLog('OnSystemWake: OS reported resume-from-sleep — forcing refresh');
  FForceRefresh := true;
  if firstboot then
    Exit;
  updateReading;
end;

// Fired by trndi.native when the user clicks one of Trndi's notifications:
// bring the main window back. Invoked on the main thread. On Wayland the
// platform layer has already exported the server's activation token (when
// one was provided), so the focus request can actually take effect.
procedure TfBG.OnNoticeClick;
begin
  if FShuttingDown then
    Exit;
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Show;
  BringToFront;
end;

// Ticks once a second for as long as data is stale. The age and the retry
// countdown live on the status card, so this just asks the card to recompute
// its captions (and to re-lay out if the outage crossed an escalation
// boundary) — see RefreshStaleCard.
procedure TfBG.tMissedTimer(Sender: TObject);
begin
  if firstboot then
    exit;

  RefreshStaleCard;
  // The graph has to age with the card: without this the last dot stays parked
  // in the newest slot for the whole outage. Self-limiting — the call is a
  // no-op until the slot grid crosses an INTERVAL_MINUTES boundary.
  RefreshStaleTrendDots;
end;

procedure TfBG.tSetupTimer(Sender: TObject);
begin
  tSetup.Enabled := false;
  UpdateUIColors;
end;

// Fires once shortly after the main form is shown. Performs the synchronous
// GitHub releases lookup off the FormShow critical path so the form's first
// paint isn't blocked by network latency.
procedure TfBG.tUpdateCheckTimer(Sender: TObject);
begin
  if Assigned(tUpdateCheck) then
  begin
    tUpdateCheck.Enabled := false;
    FreeAndNil(tUpdateCheck);
  end;
  // Skip the blocking HTTPS call if the form is tearing down — the
  // ProcessMessages pumps inside ShutdownBackgroundThreads can otherwise
  // dispatch this WM_TIMER and stall shutdown on a synchronous network
  // call.
  if FShuttingDown then
    Exit;
  CheckForUpdates;
end;

// One-shot kiosk activation (--kiosk): enter fullscreen and hold the
// platform's keep-awake inhibition. Runs shortly after FormShow so the WM has
// finished mapping the window before DoFullScreen flips BorderStyle and
// WindowState. DoFullScreen is a toggle, so the fullscreen check keeps a
// double fire (or a window the WM already made fullscreen) from undoing it.
procedure TfBG.tKioskApplyTimer(Sender: TObject);
begin
  if Assigned(tKioskApply) then
  begin
    tKioskApply.Enabled := false;
    FreeAndNil(tKioskApply);
  end;
  if FShuttingDown then
    Exit;
  if not IsFullscreenNow then
    DoFullScreen;
  if Assigned(native) then
    native.SetKeepAwake(true);
  TrndiDLog('Kiosk mode active: fullscreen + keep-awake');
end;

// Fires once after Application.Run has started pumping the message loop;
// kicks off the very first async glucose fetch. Spawning the TThread from
// inside FormCreate can leave the form's WM_PAINT queued behind a worker
// that starts its work (Sleep / network I/O) before the main thread has had
// a chance to enter Application.Run — symptom: form in taskbar, never drawn.
procedure TfBG.tBootFetchTimer(Sender: TObject);
begin
  if Assigned(tBootFetch) then
  begin
    tBootFetch.Enabled := false;
    FreeAndNil(tBootFetch);
  end;
  RequestAsyncFetch(true, false);
end;

// Animates the braille-spinner glyph in lVal while the very first fetch is
// in flight. Self-stops on the first tick after FBootFetchPending clears,
// leaving whatever caption ApplyFetchedReadings wrote in place.
procedure TfBG.tBootSpinnerTimer(Sender: TObject);
const
  Frames: array[0..9] of string = (
    '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'
  );
begin
  if FShuttingDown or (not FBootFetchPending) then
  begin
    StopBootSpinner;
    Exit;
  end;
  FBootSpinnerFrame := (FBootSpinnerFrame + 1) mod Length(Frames);
  lVal.Caption := Frames[FBootSpinnerFrame];
end;

// Halts the spinner timer and lands lVal on a "loaded" glyph so the brief
// post-fetch UI work in ApplyResult doesn't leave the spinner stranded
// mid-cycle. Called by ApplyResult right before the heavy main-thread work
// (PlaceTrendDots / FinalizeUIUpdate) starts; ProcessCurrentReading
// overwrites the caption with the real value moments later.
procedure TfBG.StopBootSpinner;
begin
  if Assigned(tBootSpinner) then
  begin
    tBootSpinner.Enabled := false;
    FreeAndNil(tBootSpinner);
  end;
  // Length() returns BYTES — a single braille glyph is 3 UTF-8 bytes, so
  // <= 3 matches the spinner cycle state without matching real readings.
  if (lVal.Caption = '') or (Length(lVal.Caption) <= 3) then
  begin
    lVal.Caption := '⠿';
    // Force the "loaded" glyph onto the screen before ApplyResult continues
    // into the heavy main-thread work below — otherwise ProcessCurrentReading
    // overwrites the caption with the real value before any paint runs and
    // the spinner is left visibly stranded mid-cycle.
    lVal.Refresh;
  end;
end;

// QueueAsyncCall target — invokes the resize handler on the next message
// loop iteration. Lets ApplyResult return so lVal/lArrow/lDiff paint the
// new value before the trend-dot + graph relayout runs.
procedure TfBG.DeferredPostFetchResize({%H-}Data: PtrInt);
begin
  if FShuttingDown then
    Exit;
  tResizeTimer(Self);
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

// Size the touch menu to cover the window. The panel aligns itself with
// alTop/alClient/alBottom, so only the fixed bands need explicit sizes: the two
// full-width buttons take a fifth of the height each, and the remainder splits
// into two equal rows of finger-sized buttons.
procedure TfBG.LayoutTouchMenu;
var
  bandHeight, colWidth: integer;
begin
  pnTouchMenu.SetBounds(0, 0, Width, Height);
  // Set before the per-button Font.Size assignments below: those flip
  // ParentFont off, freezing whatever colour was inherited at that point.
  pnTouchMenu.Font.Color := clBlack;

  bandHeight := Height div 5;
  bTouchSettings.Height := bandHeight;
  bMenuPanelClose.Height := bandHeight;

  // pnTouchContents (alClient) gets what is left; row 1 takes half of it and
  // row 2, being alClient itself, absorbs the rest.
  pnTouchRow1.Height := (Height - (2 * bandHeight)) div 2;

  // The last button on each row is alClient and takes the leftover width.
  colWidth := Width div 3;
  bTouchFull.Width := colWidth;
  bTouchRefresh.Width := colWidth;
  bTouchExit.Width := Width div 2;

  bTouchSettings.Font.Size := lDiff.Font.Size;
  bTouchFull.Font.Size := lDiff.Font.Size;
  bTouchRefresh.Font.Size := lDiff.Font.Size;
  bTouchHistory.Font.Size := lDiff.Font.Size;
  bTouchExit.Font.Size := lDiff.Font.Size;
  bTouchMenu.Font.Size := lDiff.Font.Size;
  bMenuPanelClose.Font.Size := lDiff.Font.Size;
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
        // The gesture became a menu, not a window move: drop the pending drag
        // so the finger sliding over the menu doesn't drag the window along,
        // and so the release doesn't trigger a relayout behind it.
        DraggingWin := false;
        DragStarted := false;
        if ((not native.HasTouchScreen) or semiTouchMode) then
        begin
          sleep(100);
          Application.ProcessMessages;
          pmSettings.PopUp(p.X, p.Y);
        end
        else
        begin
          LayoutTouchMenu;
          pnTouchMenu.Show;
          pnTouchMenu.BringToFront;
          bMenuPanelClose.SetFocus;
        end;
      end;
    end;
  end;
end;
