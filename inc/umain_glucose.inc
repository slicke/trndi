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
(* MODIFICATION NOTICE (2026-08-11): The alert engine is now evaluated once
   per update, before the threshold routing, so its hysteresis band and
   min-duration persistence survive readings that dip just inside range;
   routing an in-band reading to the "normal" handler used to reset the
   engine and defeat both settings. *)
(* Glucose update pipeline, data fetching, UI state and color logic for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

// Show or hide the rotating trend arrow overlay and set its angle/colour.
procedure TfBG.UpdateTrendArrow(const reading: BGReading; havePrev: boolean;
  gapMin: integer);
var
  deltaPer5: double;
  angle: single;
  haveDelta: boolean;
begin
  // Prefer an angle derived from the actual rate of change; fall back to the
  // discrete trend when no usable delta is available (first/stale reading).
  haveDelta := havePrev and (gapMin > 0)
    and (gapMin < (INTERVAL_MINUTES * DELTA_MAX))
    and (Length(bgs) >= 2) and (not bgs[0].empty) and (not bgs[1].empty);
  if haveDelta then
  begin
    // Normalise to a 5-minute rate. Clamp the divisor to >= 5 min so readings
    // closer together than the standard interval don't amplify a small drift
    // into a steep tilt; longer gaps still attenuate (a slower true rate).
    deltaPer5 := (bgs[0].convert(mgdl) - bgs[1].convert(mgdl)) * 5.0
      / Max(gapMin, INTERVAL_MINUTES);
    angle := CalculateTrendAngle(deltaPer5);
  end
  else
    angle := TrendToAngle(reading.trend);
  FLastArrowAngle := angle; // Cached so the floating window can reuse it.

  if not RotatingArrow then
  begin
    if Assigned(FTrendArrow) then
      FTrendArrow.Visible := false;
    lArrow.Visible := true;
    Exit;
  end;

  // Lazily create the overlay, sharing lArrow's parent so it sits over the
  // same client area.
  if not Assigned(FTrendArrow) then
  begin
    FTrendArrow := TTrendArrow.Create(Self);
    FTrendArrow.Parent := lArrow.Parent;
  end;

  FTrendArrow.ArrowColor := lArrow.Font.Color;
  FTrendArrow.BoundsRect := lArrow.BoundsRect;
  FTrendArrow.Angle := angle;
  lArrow.Visible := false;
  FTrendArrow.Visible := true;
  FTrendArrow.SendToBack;
end;

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
  begin
    // Show the change over one reporting interval, not the raw difference
    // between the last two samples. When a reading is dropped, bgs[1] sits two
    // intervals back and the raw difference silently covers twice the window it
    // appears to — a gap just under the DELTA_MAX cutoff still passes the guard
    // above. Normalising keeps this number agreeing with the trend arrow, which
    // already scales the same way in UpdateTrendArrow, and with what xDrip and
    // Nightscout report for the same gap.
    if reading.deltaEmpty then
      deltaVal := bgs[0].convert(un) - bgs[1].convert(un)
    else
      // Backend-supplied deltas (Tandem, CareLink) store the raw observed
      // difference too, so they need the same scaling.
      deltaVal := reading.convert(un, BGDelta);

    // Clamp the divisor to a full interval so samples arriving closer together
    // than usual can't amplify a small drift into a large-looking delta.
    deltaVal := deltaVal * INTERVAL_MINUTES / Max(gapMin, INTERVAL_MINUTES);
    lDiff.Caption := FormatSigned(deltaVal);
  end;
  lArrow.Caption := reading.trend.Img;
  UpdateTrendArrow(reading, havePrev, gapMin);
  lVal.Font.Style := [];

  // Paint the value labels synchronously so they hit the screen before the
  // deferred resize / dot relayout runs on the next message-loop tick —
  // queued async calls outrank WM_PAINT, so without this the value would
  // only appear after the heavy layout work completes.
  lVal.Refresh;
  lArrow.Refresh;
  lDiff.Refresh;

  TrndiDLog(Format(RS_LATEST_READING, [reading.val, DateTimeToStr(reading.date)]));

  if native.GetBoolSetting('main.announce', false) then
    speakReading;

  SetNextUpdateTimer(reading.date);
end;

{------------------------------------------------------------------------------
  NotifyExtensionsLevel
  ---------------------
  Broadcast the current glucose classification to extensions as
  levelCallback(level, previous). Fired on every finalized reading update, not
  only on transitions — extensions that only care about changes compare the
  two arguments, while ambient outputs (smart lamps, RGB devices) can use the
  repeat calls to re-assert their state. Runs for stale updates too, so lights
  driven by an extension don't keep signalling an outdated level.
 ------------------------------------------------------------------------------}
procedure TfBG.NotifyExtensionsLevel(const Fresh: boolean);
{$ifdef TrndiExt}
var
  lvl: string;
begin
  if Fresh then
  begin
    // IsDataFresh reports true on first boot before anything was fetched;
    // there is no level to broadcast until a reading exists.
    if (Length(bgs) < 1) or bgs[0].empty then
      Exit;
    case bgs[0].level of
    trndi.types.BGHigh:
      lvl := 'high';
    trndi.types.BGLOW:
      lvl := 'low';
    trndi.types.BGRangeHI:
      lvl := 'range-high';
    trndi.types.BGRangeLO:
      lvl := 'range-low';
    else
      lvl := 'normal';
    end;
  end
  else
    lvl := 'stale';

  callFunc('levelCallback', [lvl, FExtLastLevel]);
  FExtLastLevel := lvl;
end;
{$else}
begin
end;
{$endif}

function TfBG.FinalizeReadingUpdate(const Boot: boolean): boolean;
begin
  Result := IsDataFresh;
  if Result then
    ApplyFreshDisplayState
  else
    ApplyStaleDisplayState;

  NotifyExtensionsLevel(Result);

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
begin
  Result := false;
  if firstboot then
    Exit(true);

  if Length(bgs) < 1 then
    Exit(false);

  Result := MinutesBetween(Now, lastReading.date) <= native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);
end;

//------------------------------------------------------------------------------
// Display state that accompanies IsDataFresh. Split out of the predicate so
// both fetch paths (empty result and stale result) apply the same state — as
// side effects of a short-circuited `or` they used to be skipped whenever the
// result set was empty, which is exactly the boot-with-no-data case.
//------------------------------------------------------------------------------

procedure TfBG.ApplyFreshDisplayState;
begin
  if firstboot then
    Exit;

  // lVal.Font.Color is deliberately not restored here: fBG.color is still the
  // stale black at this point (ProcessCurrentReading picks the new background
  // afterwards), so any color computed now would be wrong. UpdateUIColors owns
  // it and runs from FinalizeUIUpdate once the background is final.
  tMissed.Enabled := false;
  // Hand the value's size back to the LCL's fill-the-label logic, which
  // ApplyStaleDisplayState turns off so the card can shrink the dimmed hero.
  lVal.OptimalFill := true;
end;

procedure TfBG.ApplyStaleDisplayState;
var
  haveReading: boolean;
  stale: BGReading;
begin
  if firstboot then
    Exit;

  haveReading := Length(bgs) > 0;

  if haveReading then
  begin
    // Let tMissed drive the stale card's live age; it needs a real timestamp,
    // since with bgs empty lastReading.date is 0.
    tMissed.OnTimer(tMissed);
    tMissed.Enabled := true;
  end
  else
    tMissed.Enabled := false;

  // The age and the last-reading context belong to the stale card now, so the
  // delta and "last update" slots are cleared rather than repeating the same
  // facts around it. lAgo in particular is only re-armed by FinalizeUpdate on
  // the fresh path, so without this it would sit on the boot caption for the
  // whole outage.
  lDiff.Caption := '';
  lAgo.Caption := '';
  // Same reasoning for time-in-range, which CalcRangeTime only recomputes on
  // the fresh path: it is a percentage over a window that has stopped
  // collecting, so leaving it up reads as a live statistic. Cleared rather than
  // hidden because ScaleLbl force-shows every label it touches on resize, but
  // returns immediately on an empty caption — the same way lDiff/lAgo stay
  // gone. CalcRangeTime fills it back in on recovery.
  lTir.Caption := '';
  // Struck out as well as dimmed: the dimming alone reads as "de-emphasised",
  // which isn't the same claim as "do not act on this number". Only when a
  // value is actually there — a bare '--' with a line through it is noise.
  if haveReading then
    lVal.Font.Style := [fsStrikeOut]
  else
    lVal.Font.Style := [];
  // lVal is alClient with OptimalFill, so it normally scales to the whole form
  // and would run under the card. Turn the fill off for the duration of the
  // outage; fixWarningPanel then sizes the hero to the band above the card,
  // and ApplyFreshDisplayState hands the fill back on recovery.
  lVal.OptimalFill := false;

  // Put the last known value back up as a dimmed hero rather than blanking it:
  // it is still the most recent thing we know, and the card underneath states
  // plainly how old it is. Written from lastDataReading rather than left to
  // whatever happens to be on screen, so a cold boot that lands straight on
  // stale data shows the value too instead of the boot spinner's glyph.
  lVal.Caption := '--';
  if haveReading and (not privacyMode) then
  begin
    stale := lastDataReading;
    if not stale.empty then
    begin
      // Same out-of-scale mapping ProcessCurrentReading applies, so a stale
      // reading past the sensor's limits doesn't come back as a bare number.
      if Assigned(api) and (stale.val > api.limitHI) then
        lVal.Caption := RS_HIGH
      else
      if Assigned(api) and (stale.val < api.limitLO) then
        lVal.Caption := RS_LOW
      else
        lVal.Caption := stale.format(un, BG_MSG_SHORT, BGPrimary);
      lVal.Hint := lVal.Caption;
    end;
  end;

  lArrow.Caption := '';
  // Clearing the caption only silences the glyph. With RotatingArrow on the
  // direction is drawn by a separate vector overlay that no caption change can
  // reach, so it would keep asserting a trend over a struck-out value for the
  // whole outage. ResizeUIElements re-shows it on every resize, and is gated on
  // the same freshness flag so it can't come back behind this.
  if Assigned(FTrendArrow) then
    FTrendArrow.Visible := false;
  // A dark slate rather than pure black: the stale card has to sit on top of
  // it and still read as a raised surface.
  setColorMode(STALE_BG_COLOR);
  // Dimmed hero — white at ~58% against the stale background (BlendColors'
  // alpha weights the *first* argument). Computed rather than a literal grey
  // so it tracks STALE_BG_COLOR.
  lVal.Font.Color := BlendColors(clWhite, STALE_BG_COLOR, 0.58);

  // setColorMode changed fBG.Color behind the UI-state cache's back. Without
  // invalidating it, a recovery reading that happens to carry the same value,
  // TIR and range color as the one before the outage compares equal on all
  // four fields, ShouldUpdateUI returns false, and UpdateUIColors never runs —
  // leaving this white caption on the restored (non-black) background.
  FLastUIColor := TColor(-1);

  native.badgeTrend := '';
  native.setBadge('--', clBlack, badge_width, badge_font);

  // The float mirrors the main window's value, colours and arrow, but it only
  // ever got them from FinalizeUpdate — which sits on the fresh path, so an
  // always-on-top window kept showing the last reading in its live range colour
  // with a trend arrow beside it for the whole outage, with nothing to say the
  // data had stopped. Everything it mirrors is final by this point: the
  // background, the value caption, the strike-through and the dimmed colour are
  // all set above, so this last call is enough to carry the outage across.
  UpdateFloatingWindow;
end;

function TfBG.NextFetchIntervalMs(const LastReadingTime: TDateTime): int64;
var
  ageMs: int64;
begin
  // Signed on purpose: SecondsBetween is Abs()-based, which turns a future-dated
  // reading (uploader clock ahead of ours) into a large positive "age" and can
  // trip the rapid-poll branch below on a reading that is not overdue at all.
  // Subtracting directly keeps the sign, so a future timestamp falls through to
  // the full-cadence path instead.
  ageMs := Round((Now - LastReadingTime) * MSecsPerDay);

  // A reading is overdue once it is older than the CGM cadence. Backends that
  // can answer "anything new?" cheaply (NS3's /lastModified probe) get a tight
  // retry loop so a late upload lands within seconds instead of waiting out
  // the MIN_REFRESH_INTERVAL_MS clamp below. Past RAPID_POLL_MAX_OVERDUE_MS
  // the outage is a gap (sensor warm-up, phone out of range), not a late
  // upload, and the normal cadence takes back over.
  if (ageMs >= BG_REFRESH) and (ageMs < RAPID_POLL_MAX_OVERDUE_MS) and
    Assigned(api) and api.supportsRapidPolling then
    Exit(RAPID_POLL_INTERVAL_MS);

  Result := min(BG_REFRESH, BG_REFRESH - ageMs);
  Result := max(MIN_REFRESH_INTERVAL_MS, Result);
  Result := Result + REFRESH_RESYNC_BUFFER_MS;
end;

procedure TfBG.SetNextUpdateTimer(const LastReadingTime: TDateTime);
begin
  if firstboot then
    exit;
  tMain.Enabled := false;
  tMain.Interval := NextFetchIntervalMs(LastReadingTime);
  tMain.Enabled := true;
  // TTimer has no "time remaining", so remember when this one is due; the stale
  // card counts down to it.
  FNextFetchAt := IncMilliSecond(Now, tMain.Interval);

  miRefresh.Caption := Format(RS_REFRESH_PLAIN, [TimeToStr(LastReadingTime),
    TimeToStr(FNextFetchAt)]);
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
    // No readings fell inside the TIR window: show a placeholder rather than a
    // number. -1 marks "nothing to measure" so the hint below stays empty —
    // the hint carries the percentage that UpdateUIColors (and the settings
    // dialog's Display preview) tint good/bad by, and a stored 0 made an empty
    // window indistinguishable from a genuine, alarming 0% in range.
    range := -1;
    lTir.Caption := '--%';
  end;

  if native.GetBoolSetting('tir.displaymean', false) then
  begin
    meanStr := BGMean(un, -1);
    if meanStr <> '' then
      lTir.Caption := meanStr + ' | ' + lTir.Caption;
  end;

  if range >= 0 then
    lTir.Hint := range.toString
  else
    lTir.Hint := '';
  lTir.Visible := true;
end;

function TfBG.updateReading(boot: boolean = false): boolean;
var
  force: boolean;
begin
  {$ifdef DEBUG}
  if debug_load_text then
    lVal.Caption := '<Updating>';
  {$endif}
  lAgo.Caption := '⟳' + lAgo.Caption;

  native.start;
  lastup := 0;

  if not tAgo.Enabled then
    lAgo.Caption := '🕑 ' + RS_UNKNOWN_TIME;

  // The fetch now runs on TGlucoseFetchThread. The post-fetch UI flow
  // (ApplyFetchedReadings → ProcessCurrentReading → FinalizeReadingUpdate)
  // is dispatched via Synchronize when the worker completes.
  //
  // Only consume FForceRefresh once a fresh worker is actually created — if
  // RequestAsyncFetch drops the request (api unavailable, fetch in flight),
  // the force intent must survive so the next attempt honours it.
  force := FForceRefresh;
  Result := RequestAsyncFetch(boot, force);
  if Result and force then
    FForceRefresh := false;
end;

procedure TfBG.FinalizeUpdate;
begin
  lastup := Now;

  // The drawn title bar renders the form caption itself; repaint it here so
  // the caption stays current even when the bar's colors did not change (the
  // color sink only fires from the SetTitleColor paths).
  if Assigned(FTitleBar) then
    FTitleBar.RefreshTitle;

  if privacyMode then
  begin
    lval.Caption := privacyIcon(lastReading.level);
    lVal.hint := lval.Caption;
  end;

  tAgo.Enabled := true;
  tAgo.OnTimer(self);
  if Assigned(pnNextProgress) and pnNextProgress.Visible then
    tProgress.Enabled := true;
  // Defer the relayout — the synchronous Self.OnResize(lVal) chain runs
  // tResizeTimer (hide/resize/show across every trend dot + AdjustGraph's
  // forced Update), which is hundreds of ms on boot and freezes the
  // message loop. Queuing it as an async call hands control back to the
  // loop so lVal/lArrow/lDiff paint the new value first; the dot/graph
  // layout follows on the next message-loop iteration.
  Application.QueueAsyncCall(@DeferredPostFetchResize, 0);

  UpdateFloatingWindow;

  UpdatePredictionLabel;

  if ShouldUpdateUI(fBG.Color, lVal.Caption, lTir.Caption, lTir.Color) then
  begin
    UpdateUIColors;
    CacheUIState(fBG.Color, lVal.Caption, lTir.Caption, lTir.color);
  end;

  // Optional trend arrow next to the reading (Windows taskbar icon /
  // macOS dock badge)
  if native.GetBoolSetting('badge.trend', true) then
    native.badgeTrend := lastReading.trend.Img
  else
    native.badgeTrend := '';
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
    HidePredictionDots;
    Exit;
  end;

  // In dot mode the prediction is drawn as hollow dots on the trend, so the
  // text label stays hidden; the prediction fetch still runs below to feed it.
  lPredict.Visible := not PredictionDotsActive;

  if (Length(PredictionCache) > 0) and (PredictionLastReadingTime = lastReading.date) then
  begin
    RenderPredictionCache(PredictionCache);
    Exit;
  end;

  if Assigned(FPredictionThread) then
  begin
    if not FPredictionThread.Finished then
      Exit;
    SafeThreadJoin(FPredictionThread);
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
    HidePredictionDots;
    Exit;
  end;

  // Dot view: render the 5/10/15-minute predictions as hollow dots continuing
  // the trend to the right, and keep the text label hidden.
  if PredictionDotsActive then
  begin
    RenderPredictionDots(bgr, closest5, closest10, closest15);
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

procedure TfBG.RenderPredictionDots(const bgr: BGResults;
  Closest5, Closest10, Closest15: integer);

  procedure ApplyDot(Slot, Idx: integer);
  var
    l: TDotControl;
    H, M, S, MS: word;
  begin
    if (Slot < 0) or (Slot > High(PredictionDots)) then
      Exit;
    l := PredictionDots[Slot];
    if not Assigned(l) then
      Exit;

    if (Idx >= 0) and (Idx <= High(bgr)) then
    begin
      l.Caption := DOT_PREDICT;
      // Store the value in the display unit so UpdatePredictionDotHeights can
      // parse it the same way the history dots are parsed.
      l.Hint := bgr[Idx].format(un, BG_MSG_SHORT, BGPrimary);
      // Encode the predicted time so the right-click menu can show it.
      DecodeTime(bgr[Idx].date, H, M, S, MS);
      l.Tag := H * 100 + M;
      l.Font.Color := DetermineColorForReading(bgr[Idx]);
      l.Visible := true;
    end
    else
    begin
      l.Hint := '';
      l.Visible := false;
    end;
  end;

begin
  lPredict.Visible := false;

  ApplyDot(0, Closest5);
  ApplyDot(1, Closest10);
  ApplyDot(2, Closest15);

  // Re-run the shared dot layout so the new prediction dots are sized,
  // positioned in their trailing slots, and aligned to the same value axis as
  // the history dots (UpdateTrendDots sets value-based tops, AdjustGraph then
  // applies the trend's vertical offsets to every dot).
  LayoutPredictionDots;
  UpdateTrendDots;
  AdjustGraph;
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
    // Carries the stale strike-through. The float has no room for a status
    // card, so the struck-out value is most of what says "do not act on this".
    // Mirrored rather than set from the freshness flag so the fresh path clears
    // it by the same route.
    fFloat.lVal.Font.Style := lVal.Font.Style;
    fFloat.lArrow.Caption := lArrow.Caption;
    fFloat.lArrow.Visible := fFloat.lArrow.Caption <> '';
    // The fixed black/white font choice is a preference about the *live* look,
    // made against a range colour. It does not survive the switch to
    // STALE_BG_COLOR — black on near-black leaves the value barely readable, and
    // an unreadable value is worse than an overridden preference. So while data
    // is stale take the main window's computed dimmed tone regardless, which is
    // derived from the stale background and therefore always legible on it.
    if fFloat.miFontMain.Checked or FTrendDataStale then
    begin
      fFloat.lVal.font.color := lval.font.color;
      fFloat.larrow.font.color := fFloat.lVal.font.color;
    end;
    fFloat.pnMultiUser.Visible := userColorBar;
    if fFloat.pnMultiUser.Visible then
      fFloat.pnMultiUser.Color := pnMultiUser.Color;
    fFloat.FormResize(fFloat);

    // Mirror the rotating trend arrow (uses the angle cached by UpdateTrendArrow).
    // Suppressed while data is stale for the same reason the main window's is:
    // the cached angle describes a reading that is no longer current, and the
    // float is small enough that an arrow is most of what you read off it.
    fFloat.SetTrendArrow(RotatingArrow and (not FTrendDataStale),
      FLastArrowAngle, fFloat.lArrow.Font.Color);
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
  txt: string;
  sensorFault: boolean;
  fired: TAlertKindSet;
begin
  txt := '';

  if api = nil then
    Exit;

  reading := lastReading;

  // One evaluation per update, made BEFORE the threshold routing below. The
  // engine tracks violations across its hysteresis band, so a reading that
  // slipped a hair back inside range must still reach it — routing such a
  // reading straight to the "normal" handler used to reset the engine and
  // defeat both the hysteresis and the min-duration settings (a BG bouncing
  // on the high threshold restarted its persistence clock on every dip and
  // could never fire). The exact thresholds still pick the display color.
  fired := FAlertEngine.EvaluateLevel(reading.val);

  if reading.val >= api.cgmHi then
    HandleHighGlucose(reading, fired)
  else
  if reading.val <= api.cgmLo then
    HandleLowGlucose(reading, fired)
  else
    HandleNormalGlucose(reading, fired);

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

procedure TfBG.RaiseHighLevelAlert;
var
  url: string;
  defaultAction: string;
begin
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

procedure TfBG.RaiseLowLevelAlert;
var
  url: string;
  defaultAction: string;
begin
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

procedure TfBG.HandleHighGlucose(const reading: BGReading; const fired: TAlertKindSet);
begin
  setColorMode(bg_color_hi);

  if akHigh in fired then
    RaiseHighLevelAlert;
end;

procedure TfBG.HandleLowGlucose(const reading: BGReading; const fired: TAlertKindSet);
begin
  SetColorMode(bg_color_lo);

  if (fired * [akLow, akUrgentLow]) <> [] then
    RaiseLowLevelAlert;
end;

procedure TfBG.HandleNormalGlucose(const reading: BGReading; const fired: TAlertKindSet);
var
  formattedReading, url: string;
  parsedInt: integer;
  parsedFloat: single;
  go: boolean = false;
begin
  SetColorMode(bg_color_ok);

  // The engine has already been evaluated for this reading (see
  // UpdateUIBasedOnGlucose): an in-range value outside every hysteresis band
  // cleared the level rules there, while one still inside a band kept its
  // violation alive — and may even have fired, when a min-duration ran out on
  // a reading hovering just under the threshold. Honor such a verdict rather
  // than swallowing it, or the alert would be lost to the re-alert gate once
  // the value crosses back over.
  if akHigh in fired then
    RaiseHighLevelAlert;
  if (fired * [akLow, akUrgentLow]) <> [] then
    RaiseLowLevelAlert;

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

    if native.TryGetSetting('url_remote.url_perfect', url) then
      native.getURL(url, url);
  end;

  // Skip the all-clear green when a level alert fired on this very reading —
  // it would immediately overwrite the alert's own Chroma effect.
  if (fired * ALERT_KIND_LEVEL) = [] then
    if assigned(native) and assigned(chroma) then
      if chroma.Initialized and native.GetBoolSetting('razer.enabled', false) and native.GetBoolSetting('razer.normal', false) then
        Chroma.SetStaticAll(clRazerGreen);

  UpdateOffRangePanel(reading.val);
end;

{------------------------------------------------------------------------------
  ApplyFetchedReadings
  --------------------
  Main-thread half of a fetch: takes the readings returned by the network call
  (whether the call happened inline or on a TGlucoseFetchThread worker) and
  runs all of the side-effecting bookkeeping — cache reconciliation, override
  settings, warning panel, alert engine, dot placement.

  Returning False here means "no usable data to render"; the caller (sync
  flow or worker-thread ApplyResult) should skip ProcessCurrentReading /
  FinalizeReadingUpdate and let the warning panel + periodic timer drive
  the next attempt.
 ------------------------------------------------------------------------------}
function TfBG.ApplyFetchedReadings(const Readings: BGResults;
const ErrorMsg: string; Boot: boolean): boolean;
var
  msg: string;
  i: integer;
  hasData: boolean;
  fresh: boolean;
begin
  Result := false;

  // A result set where every reading is a cleared placeholder (no value, no
  // timestamp) is a failed fetch in disguise — e.g. a backend that mapped an
  // error payload into readings. Treat it as empty so it can't masquerade as
  // fresh data or displace the cached last-good readings below.
  hasData := false;
  for i := 0 to High(Readings) do
    if not Readings[i].empty then
    begin
      hasData := true;
      Break;
    end;

  // Copy worker output into the main-thread bgs slot.
  if hasData then
  begin
    SetLength(bgs, Length(Readings));
    Move(Readings[0], bgs[0], Length(Readings) * SizeOf(BGReading));
  end
  else
    SetLength(bgs, 0);

  // Cache the freshness signal on the API so `api.active` answers without I/O.
  if Assigned(api) then
    api.noteReadings(Readings);

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
    TrndiDLog('ApplyFetchedReadings: API returned no data, using cached readings');
    FLastConnectionDetail := Trim(ErrorMsg);
    if FLastConnectionDetail = '' then
      FLastConnectionDetail := RS_NO_BACKEND;
    SetConnectionBadge(ClassifyConnectionStatus(ErrorMsg), RGBToColor(185, 60, 45));
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

  if Assigned(api) then
  begin
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
  end;

  hideWarningPanel;

  // An empty result set is never usable, even during firstboot (where
  // IsDataFresh reports fresh so the unconfigured Setup screen is left alone).
  fresh := (Length(bgs) > 0) and IsDataFresh;

  // Freshness tints the trend dots (DetermineColorForReading fades them once
  // this flag is up), so a flip has to force PlaceTrendDots to reprocess even
  // when neither the readings nor the slot grid moved — same FLastReadingsHash
  // reset ApplyTrendDotCount uses. Without it the trace keeps its live colors
  // until the next INTERVAL_MINUTES boundary happens to roll over.
  if FTrendDataStale <> (not fresh) then
  begin
    FTrendDataStale := not fresh;
    FLastReadingsHash := 0;
  end;

  if fresh then
    ApplyFreshDisplayState
  else
    ApplyStaleDisplayState;

  if not fresh then
  begin
    msg := RS_NO_BACKEND;
    if Trim(ErrorMsg) <> '' then
    begin
      msg := msg + sLineBreak + ErrorMsg;
      TrndiDLog('Backend error: ' + ErrorMsg);
    end;

    FLastConnectionDetail := msg;
    SetConnectionBadge(ClassifyConnectionStatus(ErrorMsg), RGBToColor(185, 60, 45));

    // On boot, prefer the dedicated boot-failure message so the user knows
    // it's the first fetch that failed (and not a mid-session outage).
    if Boot then
      showWarningPanel(RS_NO_BOOT_READING, wsCritical, true)
    else
      showWarningPanel(msg, wsCritical, true);

    // No fresh readings means any prior prediction overlay is stale —
    // hide it so the no-data warning isn't visually contradicted by X
    // marks that look like fresh predictions.
    HidePredictionDots;
    lPredict.Visible := false;

    // The history itself is still worth showing, but it has to keep moving:
    // re-place it here (and once a second from tMissed) so the dots slide out
    // of the newest slots as the outage grows, and pick up the faded stale
    // coloring the flag above just turned on.
    RefreshStaleTrendDots;

    if akMissing in FAlertEngine.EvaluateMissing then
      native.attention(RS_ATTENTION_MISSING, RS_ATTENTION_MISSING_DESC);
    Exit;
  end;

  FAlertEngine.ResetMissing;

  // Pump housekeeping rides along with the readings the fetch just cached.
  CheckPumpReservoir;
  CheckSensorExpiry;
  CheckPumpBattery;

  TrndiDLog(Format('ApplyFetchedReadings: Got %d readings', [Length(bgs)]));

  PlaceTrendDots(bgs);
  Result := true;
end;

{------------------------------------------------------------------------------
  RequestAsyncFetch
  -----------------
  Spawn a TGlucoseFetchThread to call api.getReadings off the main thread.
  Drops the request if a fetch is already in flight (caller can rely on the
  in-flight one to complete shortly). On success, the worker's ApplyResult
  Synchronizes back into ApplyFetchedReadings + ProcessCurrentReading +
  FinalizeReadingUpdate.

  Boot   — preserves the two-attempt fast retry that used to live in
           FormCreate's blocking path, and routes the failure to the
           boot-specific warning panel.
  Force  — passed to api.getReadings as `noCache`. HTTP-based backends
           (Nightscout v1/v3, xDrip) append a cache-busting `_=<unix>` query
           param so intermediaries and client-side HTTP caches don't serve a
           stale response; POST-based backends (Dexcom, Tandem) and synthetic
           debug backends ignore it. Also surfaces as "(Forced)" in the
           debug-info dialog title.
 ------------------------------------------------------------------------------}
function TfBG.RequestAsyncFetch(Boot: boolean; Force: boolean): boolean;
begin
  Result := false;

  if FShuttingDown then
    Exit;

  if api = nil then
    Exit;

  if FApiCallInFlight then
  begin
    TrndiDLog('RequestAsyncFetch: dropped — api call already in flight');
    // A request is already pending (could be glucose OR history); return
    // False so the caller knows no new worker was created and can preserve
    // one-shot state (e.g. FForceRefresh) for the next attempt.
    Exit;
  end;

  // Second guard: ApplyResult clears FApiCallInFlight from inside Synchronize,
  // which returns before the worker's Execute has finished unwinding. In that
  // brief window FGlucoseFetchThread.Finished is still false even though
  // FApiCallInFlight is false — without this check the assignment below would
  // overwrite a still-running thread reference and leak it. Drop the request;
  // the next periodic/menu trigger will pick it up once Finished is true.
  if Assigned(FGlucoseFetchThread) and (not FGlucoseFetchThread.Finished) then
  begin
    TrndiDLog('RequestAsyncFetch: dropped — previous worker still tearing down');
    Exit;
  end;

  // Lazily reap a previously-finished worker before starting a new one;
  // mirrors the pattern used by FPredictionThread.
  if Assigned(FGlucoseFetchThread) and FGlucoseFetchThread.Finished then
  begin
    SafeThreadJoin(FGlucoseFetchThread);
    FreeAndNil(FGlucoseFetchThread);
  end;

  FApiCallInFlight := true;
  if Boot then
    FBootFetchPending := true;

  native.updateBegin;
  TrndiDLog('RequestAsyncFetch: starting worker (boot=' +
    BoolToStr(Boot, true) + ' force=' + BoolToStr(Force, true) + ')');

  FGlucoseFetchThread := TGlucoseFetchThread.Create(Self, Boot, Force);
  Result := true;
end;

{------------------------------------------------------------------------------
  RequestHistoryFetch
  -------------------
  Spawn a THistoryFetchThread for a custom-window history-graph fetch. The
  network call runs off-main and ShowHistoryGraph is opened from the worker's
  ApplyResult via Synchronize. While in flight, both history menu items are
  disabled so a second click can't queue a duplicate.
 ------------------------------------------------------------------------------}
function TfBG.RequestHistoryFetch(Minutes, MaxReadings: integer;
Mode: THistoryFetchMode): boolean;
begin
  Result := false;

  if FShuttingDown then
    Exit;

  if api = nil then
  begin
    ShowMessage('API not initialized yet.');
    Exit;
  end;

  if FApiCallInFlight then
  begin
    TrndiDLog('RequestHistoryFetch: dropped — api call already in flight');
    Exit;
  end;

  // Mirror RequestAsyncFetch's "previous worker still tearing down" guard.
  if Assigned(FHistoryFetchThread) and (not FHistoryFetchThread.Finished) then
  begin
    TrndiDLog('RequestHistoryFetch: dropped — previous worker still tearing down');
    Exit;
  end;

  if Assigned(FHistoryFetchThread) and FHistoryFetchThread.Finished then
  begin
    SafeThreadJoin(FHistoryFetchThread);
    FreeAndNil(FHistoryFetchThread);
  end;

  FApiCallInFlight := true;
  if Assigned(mi24h) then
    mi24h.Enabled := false;
  if Assigned(miReadingsSince) then
    miReadingsSince.Enabled := false;

  TrndiDLog(Format('RequestHistoryFetch: starting worker (minutes=%d, max=%d, mode=%d)',
    [Minutes, MaxReadings, Ord(Mode)]));

  FHistoryFetchThread := THistoryFetchThread.Create(Self, Minutes, MaxReadings, Mode);
  Result := true;
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
  if Assigned(FTrendArrow) then
  begin
    FTrendArrow.ArrowColor := lArrow.Font.Color;
    if FTrendArrow.Visible then
      FTrendArrow.SendToBack;
  end;
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
function TfBG.PlaceTrendDots(const Readings: array of BGReading): boolean;
var
  SortedReadings: array of BGReading;
  currentTime: TDateTime;
  currentHash: cardinal;
  timeBucket: int64;
begin
  Result := false;
  if Length(Readings) = 0 then
    Exit;

  currentTime := Now;
  // Slots are anchored to wall-clock INTERVAL_MINUTES boundaries, so identical
  // readings must still be re-placed once Now crosses a boundary: the latest
  // dot has to shift left and vacate the newest slot when no new reading has
  // arrived in time.
  timeBucket := Trunc(currentTime * MinsPerDay) div INTERVAL_MINUTES;
  currentHash := CalculateReadingsHash(Readings);
  if (currentHash = FLastReadingsHash) and (timeBucket = FLastDotTimeBucket) then
    Exit;

  // Reported so callers that own their own relayout (RefreshStaleTrendDots)
  // can skip it in lockstep with this short-circuit, rather than inferring it
  // from the cache fields below — FLastReadingsHash is also used as a
  // force-reprocess sentinel, so its value alone can't answer "did we run?".
  Result := true;

  FLastReadingsHash := currentHash;
  FLastDotTimeBucket := timeBucket;
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
  // The ringed DOT_FRESH marker is optional (ux.dot_fresh); when disabled the
  // newest dot stays a plain DOT_GRAPH even while the reading is fresh.
  FLatestIsFresh := isFresh and native.GetBoolSetting('ux.dot_fresh', false);

  if (Length(TrendDots) > ACTIVE_DOTS) and Assigned(TrendDots[ACTIVE_DOTS]) then
  begin
    TrendDots[ACTIVE_DOTS].Visible := isFresh;

    if isFresh then
      TrndiDLog(Format('TrendDots[%d] shown as latest reading is fresh.', [ACTIVE_DOTS]))
    else
      TrndiDLog(Format('TrendDots[%d] hidden due to outdated reading.', [ACTIVE_DOTS]));
  end;
end;

procedure TfBG.ProcessTimeIntervals(const SortedReadings: array of BGReading;
CurrentTime: TDateTime);
var
  slotIndex, i, labelNumber, searchStart: integer;
  slotStart, slotEnd, anchorTime, anchorSource: TDateTime;
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

  // Anchor from whichever is later: Now or the latest reading's timestamp.
  // Rounding Now alone would put the anchor before a reading that arrived a
  // couple of minutes past the last 5-minute boundary (common with Dexcom's
  // sample cadence + fetch delay), which then skips it from slot 0 and shifts
  // every subsequent dot one slot to the right, leaving a visible gap.
  anchorSource := CurrentTime;
  if SortedReadings[0].date > anchorSource then
    anchorSource := SortedReadings[0].date;

  anchorTime := RecodeMinute(anchorSource,
    (MinuteOf(anchorSource) div INTERVAL_MINUTES) * INTERVAL_MINUTES);
  anchorTime := RecodeSecond(anchorTime, 0);
  anchorTime := RecodeMilliSecond(anchorTime, 0);

  // If the latest reading still sits past the anchor+grace (i.e. it's several
  // seconds beyond the just-rounded 5-min tick), push the anchor forward one
  // interval so the reading lands cleanly inside slot 0's window.
  if SortedReadings[0].date > anchorTime + RECENT_SLOT_GRACE_DAYS then
    anchorTime := IncMinute(anchorTime, INTERVAL_MINUTES);

  TrndiDLog(Format('PlaceTrendDots: Processing %d readings, anchor=%s (latest=%s)',
    [Length(SortedReadings), DateTimeToStr(anchorTime), DateTimeToStr(SortedReadings[0].date)]));

  for slotIndex := 0 to ACTIVE_DOTS - 1 do
  begin
    slotEnd := IncMinute(anchorTime, -INTERVAL_MINUTES * slotIndex);
    slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

    if slotIndex = 0 then
      slotEpsilon := RECENT_SLOT_GRACE_DAYS
    else
      slotEpsilon := TIME_EPSILON_DAYS;

    found := false;

    TrndiDLog(Format('Searching slot %d (TrendDots[%d]): %s to %s',
      [slotIndex, ACTIVE_DOTS - slotIndex, DateTimeToStr(slotStart), DateTimeToStr(slotEnd)]));

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
      labelNumber := ACTIVE_DOTS - slotIndex;
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
    if slots > ACTIVE_DOTS then
      slots := ACTIVE_DOTS;
    if slots <= 0 then
      slots := ACTIVE_DOTS;

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

  labelNumber := ACTIVE_DOTS - SlotIndex;
  if Length(TrendDots) <= labelNumber then Exit;
  l := TrendDots[labelNumber];

  if Assigned(l) then
  begin
    // The slot is filled either way (Result below drives PlaceTrendDots'
    // search cursor) — a narrow window only withholds the dot from view.
    l.Visible := DotSlotVisible(labelNumber);
    l.Hint := Reading.format(un, BG_MSG_SHORT, BGPrimary);

    DecodeTime(reading.date, H, M, S, MS);
    l.Tag := H * 100 + M;

    // The newest slot's dot gets the ringed "fresh" marker while the reading
    // is within the freshness threshold; history slots stay plain dots.
    if (labelNumber = ACTIVE_DOTS) and FLatestIsFresh then
      l.Caption := DOT_FRESH
    else
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
  // High contrast returns early on purpose: it is an accessibility mode whose
  // whole point is maximum separation from the background, and fading the dots
  // toward STALE_BG_COLOR would work directly against it. The stale card, the
  // struck-out hero value and the emptying slots still carry the message there.
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

  // No blanket darkening here any more: the result is the dot's *identity*
  // color, and DotPaint lifts it to DOT_MIN_CONTRAST against whatever
  // background it actually lands on. That has to happen at paint time — the
  // dots are colored from PlaceTrendDots, which runs before
  // UpdateUIBasedOnGlucose picks the new background, so fBG.Color is still the
  // previous reading's color at this point.

  // While data is stale the trace is a record, not a live plot. Fade it toward
  // the stale background so the dots read as history — at full saturation a red
  // one looks exactly like an active hypo/hyper, which is the one reading the
  // graph is least entitled to claim during an outage. BlendColors weights its
  // *first* argument, so STALE_DOT_BLEND is how much range color survives.
  if FTrendDataStale then
    Result := BlendColors(Result, STALE_BG_COLOR, STALE_DOT_BLEND);
end;

function TfBG.setColorMode: boolean;
begin
  Result := setColorMode(clFuchsia, true);
end;

function TfBG.setColorMode(bg: tColor; const nocolor: boolean = false): boolean;
var
  hasBadge: boolean;
begin
  if not nocolor then
    fBG.Color := bg;

  // On badge platforms (Windows) the active user's identity is shown by the
  // title-bar badge, so multi-user mode is normalised to the single-user rule:
  // the title bar follows the "colour with glucose colour" preference instead
  // of being forced to the user colour. The main-form colour bar is redundant
  // next to the badge, so hide it (the borderless float window keeps its bar,
  // mirrored from userColorBar).
  hasBadge := multi and native.SupportsUserBadge;
  if hasBadge then
    pnMultiUser.Visible := false;

  if (not multi or hasBadge)
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

  // Without a badge, tint the title bar with the user's colour (and drop the
  // now-redundant left colour bar). With a badge the colour is on the badge,
  // so the title bar is left to the glucose/off-range rules above.
  if (not hasBadge)
    and customTitleBar and (pnMultiUser.Color <> clBlack) then
    if native.SetTitleColor(handle, pnMultiUser.Color,
      IfThen(IsLightColor(pnMultiUser.Color), clBlack, clWhite)) then
    begin
      pnMultiUser.Visible := false;
      Exit(true);
    end;

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

procedure TfBG.UserBadgeClicked;
begin
  miSettingsClick(Self);
end;

procedure TfBG.RefreshUserBadge;
{$ifdef X_WIN}
var
  bg, txt: TColor;
{$endif}
begin
{$ifdef X_WIN}
  if not (multi and native.SupportsUserBadge) then
    Exit;

  // The pill wears the user's colour when set; otherwise a subtle theme-aware
  // chip so the name stays legible on the default caption.
  if pnMultiUser.Color <> clBlack then
    bg := pnMultiUser.Color
  else if native.isDarkMode then
    bg := RGBToColor(64, 64, 68)
  else
    bg := RGBToColor(228, 228, 231);
  // A small pill needs maximum legibility, so the text is straight black/white
  // contrast (same rule as the title-bar text). A same-hue tint washes out on
  // light account colours such as yellow.
  txt := IfThen(IsLightColor(bg), clBlack, clWhite);

  if not native.ShowUserBadge(multinick, bg, txt, @UserBadgeClicked) then
    // Badge could not be created — fall back to the caption-text prefix once.
    if Pos('[', Caption) = 0 then
      Caption := Format(RS_USER_CAPTION, [multinick, Caption]);
{$endif}
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
  lastIgnoreDate: string;
  ignoreDate: TDateTime;
  parsed: boolean;
begin
  if FShuttingDown then
    Exit;

  if FUpdateCheckInFlight then
    Exit;

  // Honour a user-requested 14-day snooze before spawning a worker. The
  // manual menu/keyboard trigger (ShowUpToDateMessage=true) bypasses the
  // snooze so the user always gets feedback.
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
        if DaysBetween(Now, ignoreDate) < 14 then
          Exit;
      end
      else
        TrndiDLog(Format('CheckForUpdates: failed to parse update.ignore.date: %s', [lastIgnoreDate]));
    end;
  end;

  // Reap a previous worker that already posted its result. Live workers are
  // gated by FUpdateCheckInFlight above, so we never overwrite an in-flight
  // thread here.
  if Assigned(FUpdateCheckThread) and FUpdateCheckThread.Finished then
  begin
    SafeThreadJoin(FUpdateCheckThread);
    FreeAndNil(FUpdateCheckThread);
  end;
  if Assigned(FUpdateCheckThread) and (not FUpdateCheckThread.Finished) then
  begin
    TrndiDLog('CheckForUpdates: dropped — previous worker still tearing down');
    Exit;
  end;

  FUpdateCheckInFlight := true;
  FUpdateCheckThread := TUpdateCheckThread.Create(Self, ShowUpToDateMessage);
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
