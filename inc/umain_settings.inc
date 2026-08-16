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
(* MODIFICATION NOTICE (2026-08-11): The alert-hysteresis spinners now load
   before the mmol/L unit conversion runs, matching the save path's mg/dL
   premise; loading after it showed the stored mg/dL figure as mmol and
   inflated it ~18-fold on the next save. *)
(* Settings dialog and instant-apply configuration for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

{------------------------------------------------------------------------------
  Apply configuration changes that can take effect immediately without restart.
  This includes fonts, colors, display options, predictions, and UI preferences.
  Called after saving settings to provide instant feedback to the user.
------------------------------------------------------------------------------}
procedure TfBG.ApplySettingsInstantly;
var
  fontName: string;
  minutesStr: string;
  minutesVal: integer;
  langCode: string;
  newDotCount: integer;
begin
  if native.CheckSetting('unit', 'mmol', 'mmol') then
    un := BGUnit.mmol
  else
    un := BGUnit.mgdl;

  if api <> nil then
  begin
    api.cgmLo := native.GetIntSetting('override.lo', api.cgmLo);
    api.cgmHi := native.GetIntSetting('override.hi', api.cgmHi);
    api.cgmRangeLo := native.GetIntSetting('override.rangelo', api.cgmRangeLo);
    api.cgmRangeHi := native.GetIntSetting('override.rangehi', api.cgmRangeHi);
  end;

  langCode := native.GetSetting('locale', 'en');
  if applocale <> langCode then
  begin
    applocale := langCode;
    SetDefaultLang(langCode, getLangPath);
  end;

  DIFF_ALIGN := native.GetAlignmentSetting('ux.labels.ldiff.alignment', taCenter);

  fontName := native.GetSetting('font.val', 'default');
  if fontName <> 'default' then
    lVal.Font.Name := fontName;

  fontName := native.GetSetting('font.arrow', 'default');
  if fontName <> 'default' then
    lArrow.Font.Name := fontName;

  fontName := native.GetSetting('font.ago', 'default');
  if fontName <> 'default' then
  begin
    lAgo.Font.Name := fontName;
    lTir.Font.Name := fontName;
  end;

  bg_color_ok := native.GetColorSetting('ux.bg_color_ok', bg_color_ok);
  bg_color_hi := native.GetColorSetting('ux.bg_color_hi', bg_color_hi);
  bg_color_lo := native.GetColorSetting('ux.bg_color_lo', bg_color_lo);
  bg_color_ok_txt := native.GetColorSetting('ux.bg_color_ok_txt', bg_color_ok_txt);
  bg_color_hi_txt := native.GetColorSetting('ux.bg_color_hi_txt', bg_color_hi_txt);
  bg_color_lo_txt := native.GetColorSetting('ux.bg_color_lo_txt', bg_color_lo_txt);
  bg_rel_color_hi := native.GetColorSetting('ux.bg_rel_color_hi', bg_rel_color_hi);
  bg_rel_color_lo := native.GetColorSetting('ux.bg_rel_color_lo', bg_rel_color_lo);
  bg_rel_color_hi_txt := native.GetColorSetting('ux.bg_rel_color_hi_txt', bg_rel_color_hi_txt);
  bg_rel_color_lo_txt := native.GetColorSetting('ux.bg_rel_color_lo_txt', bg_rel_color_lo_txt);

  if native.GetBoolSetting('main.high_contrast', false) then
  begin
    bg_color_ok := clBlack;
    bg_color_ok_txt := clWhite;
    bg_color_hi := clBlack;
    bg_color_hi_txt := clWhite;
    bg_color_lo := clWhite;
    bg_color_lo_txt := clBlack;
    bg_rel_color_hi := clBlack;
    bg_rel_color_lo := clWhite;
    bg_rel_color_hi_txt := clWHite;
    bg_rel_color_lo_txt := clBlack;
  end;

  // See the note in umain_init.inc: cached rather than read in DotPaint. The
  // dots are re-placed and repainted further down, so a changed mode shows
  // without waiting for the next reading.
  DotColorMode := DotColorModeFromSetting(
    native.GetIntSetting('ux.dot_color_mode', Ord(DOT_COLOR_MODE_DEFAULT)));

  tir_bg := native.GetColorSetting('ux.tir_color', tir_bg);
  tir_custom_bg := native.GetColorSetting('ux.tir_color_custom', tir_custom_bg);
  titlecolor := native.GetBoolSetting('ux.title_color', true);

  PredictGlucoseReading := native.GetBoolSetting('predictions.enable', false);
  PredictShortMode := native.GetBoolSetting('predictions.short', false);
  PredictDotMode := native.GetBoolSetting('predictions.dots', false);
  RotatingArrow := native.GetBoolSetting('display.rotating_arrow', false);
  ShowBolusOverlay := native.GetBoolSetting('display.bolus_overlay', false);
  ShowAutoBolusOverlay := native.GetBoolSetting('display.bolus_overlay.auto', false);
  ShowCarbOverlay := native.GetBoolSetting('display.carb_overlay', false);
  PredictShortFullArrows := native.GetBoolSetting('predictions.short.fullarrows', false);
  PredictShortShowValue := native.GetBoolSetting('predictions.short.showvalue', false);
  minutesStr := '';
  if not native.TryGetSetting('predictions.short.minutes', minutesStr) then
    minutesStr := '';
  minutesVal := StrToIntDef(minutesStr, 10);
  if minutesVal in [5, 10, 15] then
    PredictShortMinutes := minutesVal
  else
    PredictShortMinutes := 10;
  PredictShortSize := native.GetIntSetting('predictions.short.size', 1);
  if PredictShortSize < 1 then
    PredictShortSize := 1
  else
  if PredictShortSize > 3 then
    PredictShortSize := 3;
  // Dot mode draws the predictions on the trend, so the text label must stay
  // hidden — it still carries the placeholder/last text caption, which would
  // otherwise reappear in the lower right until the next forced refresh.
  lPredict.Visible := PredictGlucoseReading and not PredictDotMode;

  DATA_FRESHNESS_THRESHOLD_MINUTES :=
    native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);
  tir_icon := native.GetBoolSetting('range.tir_icon', false);

  if FLastConnectionStatus <> '' then
    if Assigned(FInternetBadgeBg) then
      SetConnectionBadge(FLastConnectionStatus, FInternetBadgeBg.Brush.Color)
    else
      SetConnectionBadge(FLastConnectionStatus, RGBToColor(60, 150, 90));

  UseRangeColor := native.GetBoolSetting('ux.range_color', true);

  tClock.Enabled := native.GetBoolSetting('main.clock', false);
  // Apply the clock toggle to the fullscreen overlay right away rather than
  // on the next timer tick (matters when Settings is used from a kiosk).
  UpdateClockOverlay(true);

  tSwap.Enabled := native.GetBoolSetting('main.alternate', false);

  if native.GetBoolSetting('main.ontop', false) then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;

  // The drawn title bar may need to appear/disappear when its setting or
  // main.noborders changed; it also keeps the window frameless, so it wins
  // over the default border restore below.
  ApplyOwnTitleBar;

  if native.GetBoolSetting('main.noborders', false) or OwnTitleBarActive then
    Self.BorderStyle := bsNone
  else
  {$ifdef DARWIN}
    Self.BorderStyle := bsSizeable;
  {$else}
    Self.BorderStyle := bsSizeToolWin;
  {$endif}

  // ResizeUIElements lays out and ScaleLbl:s all the labels (with the correct
  // alignment/padding for each) using the fonts applied above, and re-hides
  // lArrow when the rotating arrow overlay is active. Don't re-run ScaleLbl
  // on individual labels here — the default (centered, unpadded) parameters
  // shift lAgo right and desync lTir from the measured layout.
  ResizeUIElements;

  newDotCount := native.GetIntSetting('ux.dot_count', NUM_DOTS);
  if (newDotCount >= 1) and (newDotCount <= MAX_DOT_COUNT) and (newDotCount <> ACTIVE_DOTS) then
  begin
    ACTIVE_DOTS := newDotCount;
    if MAX_MIN < ACTIVE_DOTS * INTERVAL_MINUTES + 10 then
      MAX_MIN := ACTIVE_DOTS * INTERVAL_MINUTES + 10;
    FreeTrendDotControls;
    // MAX_RESULT is a reading count, not minutes. Mirror InitializeAPI's
    // reporting-interval scaling with ACTIVE_DOTS as the base.
    if Assigned(api) then
      MAX_RESULT := Max(MAX_RESULT,
        ACTIVE_DOTS * Max(1, (INTERVAL_MINUTES + api.getReportingInterval - 1)
                     div api.getReportingInterval))
    else if MAX_RESULT < ACTIVE_DOTS then
      MAX_RESULT := ACTIVE_DOTS;
    CreateTrendDots;
    BuildDotWindowMenu;
    Application.ProcessMessages;
    tResize.OnTimer(Self);
  end;

  // Re-place the dots from the cached readings regardless of whether the dot
  // count changed: per-slot captions depend on settings too (e.g. the
  // ux.dot_fresh marker), and PlaceTrendDots would otherwise dedupe on the
  // unchanged readings hash and skip the pass.
  FLastReadingsHash := 0;
  if Length(bgs) > 0 then
    PlaceTrendDots(bgs);

  // UpdateTrendDots sets value-based tops; AdjustGraph must always follow to
  // apply the trend row's vertical offsets (half-diameter centring + in-view
  // correction) — same pairing as tResizeTimer. Without it the whole row sits
  // visibly lower until the next full relayout. It also repaints the dots.
  UpdateTrendDots;
  AdjustGraph;

  UpdateUIColors;
  UpdateUIBasedOnGlucose;

  ProcessCurrentReading;

  // Refresh the taskbar/dock badge so a toggled trend-arrow option applies
  // without waiting for the next reading.
  if (not firstboot) and (not lastReading.empty) then
  begin
    if native.GetBoolSetting('badge.trend', true) then
      native.badgeTrend := lastReading.trend.Img
    else
      native.badgeTrend := '';
    native.setBadge(lVal.Caption, fBG.Color
      {$ifdef lclwin32}
      ,badge_width,badge_font
      {$endif}
      );
  end;

  if Assigned(fFloat) then
    UpdateFloatingWindow;

  // Repaint the title-bar user badge so a toggled dark/light theme updates the
  // fallback chip colour (no-op when unsupported or not in multi-user mode).
  RefreshUserBadge;

  Self.Invalidate;
end;

// Handle settings menu click
procedure TfBG.miSettingsClick(Sender: TObject);
var
  fConf: TfConf;

procedure LoadUserSettings(f: TfConf);
  var
    remoteType: string;
    userNames: TStringArray;
    posName: string;
    i: integer;
    usersStr: string;
    posValue: integer;
    po: TrndiPos;
    sizeVal: integer;
    minutesVal: integer;
    loLimit, hiLimit, rangeLo, rangeHi: integer;
  function DefaultChromaAlertAction: string;
    begin
      {$ifdef Windows}
      Result := 'static';
    {$else}
      Result := 'breath';
    {$endif}
    end;

  function ChromaActionToIndex(const Action: string): integer;
    var
      a: string;
    begin
      a := LowerCase(Trim(Action));
      if a = 'static' then
        Exit(1);
      if (a = 'off') or (a = 'none') then
        Exit(2);
      if a = 'ignore' then
        Exit(3);
      Result := 0;
    end;
  begin
    with f, native do
    begin
      // A stored backend that no longer resolves — removed driver, hand-edited
      // settings, or a debug backend picked in a test build and then run from a
      // release build — must not be presented as NightScout, which is what
      // BackendDisplayName falls back to. Leave the picker unselected instead;
      // cbSysChange explains why and the save below keeps the stored value.
      remoteType := GetSetting('remote.type');
      if (remoteType = '') or BackendExists(remoteType) then
      begin
        f.UnknownBackend := '';
        remoteType := BackendDisplayName(remoteType);
        cbSys.ItemIndex := 0;
        for i := 0 to cbSys.Items.Count - 1 do
          if cbSys.Items[i] = remoteType then
            cbSys.ItemIndex := i;
      end
      else
      begin
        f.UnknownBackend := remoteType;
        cbSys.ItemIndex := -1;
      end;
      f.cbSysChange(f);
      eAddr.Text := GetSetting('remote.target');
      ePass.Text := GetSetting('remote.creds');
      // Remember what was loaded so the save path can tell a user edit apart
      // from a token the backend rotated while this dialog was open.
      f.LoadedCreds := ePass.Text;
      rbUnit.ItemIndex := IfThen(GetSetting('unit', 'mmol') = 'mmol', 0, 1);
      case GetIntSetting('ux.dot_count', NUM_DOTS) of
      6:  f.rbTrendWindow.ItemIndex := 0;
      18: f.rbTrendWindow.ItemIndex := 2;
      24: f.rbTrendWindow.ItemIndex := 3;
      36: f.rbTrendWindow.ItemIndex := 4;
      50: f.rbTrendWindow.ItemIndex := 5;
      else f.rbTrendWindow.ItemIndex := 1;
      end;
      spTHRESHOLD.Value := native.GetIntSetting('system.fresh_threshold',
        DATA_FRESHNESS_THRESHOLD_MINUTES);
      spDeltaMax.Value := native.GetIntSetting('ux.delta_max', DELTA_MAX);

      // A stored override wins; otherwise seed from the backend, or from plain
      // defaults while no backend is up yet.
      if api = nil then
      begin
        loLimit := GetIntSetting('override.lo', 60);
        hiLimit := GetIntSetting('override.hi', 180);
        rangeLo := GetIntSetting('override.rangelo', TrndiAPI.CGM_RANGE_LO_DISABLED);
        rangeHi := GetIntSetting('override.rangehi', TrndiAPI.CGM_RANGE_HI_DISABLED);
      end
      else
      begin
        loLimit := GetIntSetting('override.lo', api.cgmLo);
        hiLimit := GetIntSetting('override.hi', api.cgmHi);
        rangeLo := GetIntSetting('override.rangelo', api.cgmRangeLo);
        rangeHi := GetIntSetting('override.rangehi', api.cgmRangeHi);
      end;

      // A backend without a personal range reports the disabled sentinels
      // (0 / 500 mg/dL), which showed up in the dialog as a custom range of
      // "0 - 27.8 mmol/L". Fall back to the limits, which is what an unset
      // range means in practice - the checkbox stays off, so nothing is
      // written back unless the user actually enables it.
      if rangeLo = TrndiAPI.CGM_RANGE_LO_DISABLED then
        rangeLo := loLimit;
      if rangeHi = TrndiAPI.CGM_RANGE_HI_DISABLED then
        rangeHi := hiLimit;

      fsLo.Value := loLimit;
      fsHi.Value := hiLimit;
      fsLoRange.Value := rangeLo;
      fsHiRange.Value := rangeHi;

      // Stored in mg/dL, like every other glucose figure. These must be
      // loaded BEFORE the rbUnitClick call below: that is what converts the
      // spinners to mmol/L for a mmol user, and the save path multiplies by
      // toMGDL on the same premise. Loading them after the conversion (as
      // this once did) showed the raw mg/dL figure as if it were mmol and
      // inflated the stored value ~18-fold on the next save.
      fsAlertHystHi.Value  := native.GetFloatSetting('alerts.hysteresis.high', 5);
      fsAlertHystLo.Value  := native.GetFloatSetting('alerts.hysteresis.low', 5);
      fsAlertHystUrg.Value := native.GetFloatSetting('alerts.hysteresis.urgent', 5);

      fsDiffScale.Value := GetFloatSetting('ux.labels.ldiff.scale', 1);
      fsPredictScale.Value := GetFloatSetting('ux.labels.lpredict.scale', 1);

      cbMoveDIffRight.Checked := GetAlignmentSetting('ux.labels.ldiff.alignment', taCenter) = taRightJustify;

      cbTIR.Checked := native.GetBoolSetting('range.custom', true);
      seTir.Value := native.GetIntSetting('range.time', 9999);

      cbTirIcon.checked := native.GetBoolSetting('range.tir_icon', false);
      cbShowMean.Checked := native.GetBoolSetting('tir.displaymean', false);

      cbOffBar.Checked := native.GetBoolSetting('ux.off_bar', false);
      cbPaintHiLo.Checked := native.GetBoolSetting('ux.paint_range', true);
      cbPaintLines.Checked := native.GetBoolSetting('ux.paint_range_lines', false);
      cbPaintHiLoRange.Checked :=
        native.GetBoolSetting('ux.paint_range_cgmrange', false);
      edCommaSep.Text := GetCharSetting('locale.separator', '.');
      cbProgress.Checked := GetBoolSetting('main.next_progress', false);
      cbConnectivityButton.Checked := GetBoolSetting('ux.connectivity.button', false);
      cbShowSensorExpiry.Checked := GetBoolSetting('display.sensor_expiry', false);

      if CheckSetting('unit', 'mmol', 'mmol') then
        rbUnitClick(Self);

      // rbUnitClick only runs for mmol/L, so bind the custom range to the
      // low/high limits here for the mg/dL case too.
      ApplyRangeBounds;

      cbCust.Checked := GetBoolSetting('override.enabled');
      cbCustRange.Checked := GetBoolSetting('override.range');
      cbPredictions.Checked := GetBoolSetting('predictions.enable');
      cbWebAPI.Checked := GetBoolSetting('webserver.enable');
      cbPredictShort.Checked := GetBoolSetting('predictions.short');
      cbPredictDots.Checked := GetBoolSetting('predictions.dots');
      cbRotatingArrow.Checked := GetBoolSetting('display.rotating_arrow');
      cbBolusOverlay.Checked := GetBoolSetting('display.bolus_overlay');
      cbBolusOverlayAuto.Checked := GetBoolSetting('display.bolus_overlay.auto');
      // The dependent box follows the parent from the moment the dialog opens,
      // not only when the user touches it.
      cbBolusOverlayAuto.Enabled := cbBolusOverlay.Checked;
      cbCarbOverlay.Checked := GetBoolSetting('display.carb_overlay');
      cbDotFresh.Checked := GetBoolSetting('ux.dot_fresh', false);
      // ItemIndex order is the TDotColorMode order — see the type's note.
      rgDots.ItemIndex := Ord(DotColorModeFromSetting(
        GetIntSetting('ux.dot_color_mode', Ord(DOT_COLOR_MODE_DEFAULT))));
      // Trend arrow in the taskbar icon (Windows) / dock badge (macOS) /
      // tray icon and panel indicators (Linux, BSD)
      {$if defined(X_WIN) or defined(X_MAC) or defined(X_PC)}
      // Keep this default in step with the two read sites in umain_settings /
      // umain_glucose, or the checkbox will disagree with what the badge shows.
      cbBadgeTrend.Checked := GetBoolSetting('badge.trend', true);
      {$else}
      cbBadgeTrend.Visible := false;
      {$endif}
      cbWarnLoHi.Checked := GetBoolSetting('predictions.warn');
      cbPredictShortFullArrows.Checked := GetBoolSetting('predictions.short.fullarrows');
      if GetBoolSetting('predictions.short.showvalue') then
        rbPredictShortShowValue.Checked := true
      else
        rbPredictShortArrowOnly.Checked := true;
      sizeVal := native.GetIntSetting('predictions.short.size', 1);
      if sizeVal < 1 then
        sizeVal := 1;
      if sizeVal > 3 then
        sizeVal := 3;
      cbPredictShortSize.ItemIndex := sizeVal - 1;

      minutesVal := native.GetIntSetting('predictions.short.minutes', 10);
      if not (minutesVal in [5, 10, 15]) then
        minutesVal := 10;
      case minutesVal of
      5:
        cbPredictShortMinutes.ItemIndex := 0;
      15:
        cbPredictShortMinutes.ItemIndex := 2;
      else
        cbPredictShortMinutes.ItemIndex := 1;
      end;
      UpdatePredictionStates;

      edMusicHigh.Text := GetSetting('media.url_high', '');
      edMusicLow.Text := GetSetting('media.url_low', '');
      edMusicPerfect.Text := GetSetting('media.url_perfect', '');

      edURLHigh.Text := GetSetting('url_remote.url_high', '');
      edURLLow.Text := GetSetting('url_remote.url_low', '');
      edURLPerfect.Text := GetSetting('url_remote.url_perfect', '');

      // The proxy fields are deliberately absent here: they live in the
      // non-user-scoped (root) settings, which is the only scope the HTTP
      // layer reads, and fConf loads/saves them itself in
      // LoadProxySettingsIntoUI / SaveProxySettingsFromUI. Reading them
      // user-scoped here handed the dialog a different (empty) value for
      // every named user and let the OnChange handler write that back over
      // the working proxy.

      cbChroma.Checked := GetBoolSetting('razer.enabled', false);
      cbChromaNormal.Checked := GetBoolSetting('razer.normal', false);
      cbChromaHigh.ItemIndex := ChromaActionToIndex(
        GetSetting('razer.high.action', DefaultChromaAlertAction));
      cbChromaLow.ItemIndex := ChromaActionToIndex(
        GetSetting('razer.low.action', DefaultChromaAlertAction));

      cbMediaDisable.Checked := native.GetBoolSetting('media.disabled', false);
      cbMusicPause.Checked := GetBoolSetting('media.pause');
      cbTTS.Checked := GetBoolSetting('main.announce', false);
      cbHContrast.Checked := GetBoolSetting('main.high_contrast', false);
      // Voice enumeration is deferred to the Accessibility tab's OnShow;
      // stash the saved selection so EnsureTTSVoices can apply it then.
      f.pendingTTSVoiceIndex := GetIntSetting('tts.voice', 0);
      f.pendingTTSVoiceName := GetSetting('tts.voice.name', '');
      seTTSRate.Value := GetIntSetting('tts.rate', 0);
      fsHi.Enabled := cbCust.Checked;
      fsLo.Enabled := cbCust.Checked;
      fsHiRange.Enabled := cbCustRange.Checked;
      fsLoRange.Enabled := cbCustRange.Checked;

      if TryGetCSVSetting('users.names', userNames, true) then
      begin
        lbUsers.Clear;
        lbUsers.Items.AddStrings(userNames);
        usersStr := '';
        for i := 0 to High(userNames) do
        begin
          if i > 0 then
            usersStr := usersStr + ',';
          usersStr := usersStr + userNames[i];
        end;
        TrndiDLog(Format('LoadUserSettings: loaded users.names: %s', [usersStr]));
      end
      else
      begin
        lbUsers.Enabled := false;
        TrndiDLog('LoadUserSettings: users.names not found or empty');
      end;

      lbUsers.Items.Add('- ' + RS_DEFAULT_ACCOUNT + ' -');
      cbUserColor.Checked := native.GetRootSetting('users.colorbox', 'true') = 'true';
      posValue := native.GetIntSetting('position.main', Ord(tpoCenter));

      cbPos.Items.Clear;
      for po in TrndiPos do
      begin
        posName := TrndiPosNames[po];
        cbPos.Items.Add(posName);

        if Ord(po) = posValue then
          cbPos.ItemIndex := Ord(po);
      end;

      if cbPos.ItemIndex = -1 then
        cbPos.ItemIndex := 0;

      cbSize.Checked := GetBoolSetting('size.main');
      cbHints.Checked := GetBoolSetting(HINTS_ENABLED_KEY, true);
      cbFlash.Visible := false;

      cbRangeColor.Checked := native.GetBoolSetting('ux.range_color', true);
      cbClock.Checked      := native.GetBoolSetting('main.clock', false);
      cbAlternate.Checked  := native.GetBoolSetting('main.alternate', false);
      cbOnTop.Checked      := native.GetBoolSetting('main.ontop', false);
      cbNoBorders.Checked  := native.GetBoolSetting('main.noborders', false);

      cbAlertHiLo.Checked := native.getBoolSetting('alerts.notice.hilo', true);
      cbAlertMissing.Checked := native.getBoolSetting('alerts.notice.missing', true);
      cbAlertReservoir.Checked := native.getBoolSetting('alerts.notice.reservoir', true);
      cbAlertSensor.Checked := native.getBoolSetting('alerts.notice.sensor', true);
      cbAlertBattery.Checked := native.getBoolSetting('alerts.notice.battery', true);

      // Minutes, not glucose: no unit conversion, so these can load here.
      // The hysteresis spinners load earlier, before the mmol conversion.
      spAlertDurHi.Value   := native.GetIntSetting('alerts.duration.high', 10);
      spAlertDurLo.Value   := native.GetIntSetting('alerts.duration.low', 5);
      spAlertDurUrg.Value  := native.GetIntSetting('alerts.duration.urgent', 0);
    end;
  end;

procedure LoadLanguageSettings(f: TfConf);
  var
    i: integer;
    langEntry: string;
  begin
    with f, native do
    begin
      ListLanguageFiles(cbLang.Items, GetLangPath);
      cbLang.Items.Add('Trndi.en');
      cbLang.Items.Add('Trndi.auto');
      for i := 0 to cbLang.Items.Count - 1 do
      begin
        langEntry := cbLang.Items[i];
        cbLang.Items[i] := ExtractDelimited(2, langEntry, ['.']);
        langEntry := cbLang.Items[i];
        cbLang.Items[i] := Format('%s (%s)', [GetLanguageName(langEntry), langEntry]);
        if CheckSetting('locale', '', langEntry) then
          cbLang.ItemIndex := i;
      end;
      if cbLang.ItemIndex = -1 then
        cbLang.ItemIndex := cbLang.Items.Count - 1;
    end;
  end;

procedure SetupUIElements(f: TfConf);
  begin
    with f do
    begin
      f.Invalidate;

      // Seed the Display miniature's fonts from the live labels; the captions
      // are read straight off those labels by RenderDisplayPreview. pnDisplay
      // keeps the default UI font so the dialog's font reset knows "stock".
      f.FontVal.Assign(Self.lVal.Font);
      f.FontArrow.Assign(Self.lArrow.Font);
      f.FontAgo.Assign(Self.lAgo.Font);
      pnDisplay.Color := Self.Color;
      pnDisplay.Font := fBG.Font;

      cl_ok_bg.ButtonColor := native.GetColorSetting('ux.bg_color_ok', bg_color_ok);
      cl_hi_bg.ButtonColor := native.GetColorSetting('ux.bg_color_hi', bg_color_hi);
      cl_lo_bg.ButtonColor := native.GetColorSetting('ux.bg_color_lo', bg_color_lo);

      cl_ok_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_ok_txt', bg_color_ok_txt);
      cl_hi_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_hi_txt', bg_color_hi_txt);
      cl_lo_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_lo_txt', bg_color_lo_txt);

      cl_hi_bg_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_hi', bg_rel_color_hi);
      cl_lo_bg_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_lo', bg_rel_color_lo);

      cl_hi_txt_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_hi_txt', bg_rel_color_hi_txt);
      cl_lo_txt_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_lo_txt', bg_rel_color_lo_txt);

      cbTitleColor.Checked := native.GetBoolSetting('ux.title_color', true);

      cbTirColor.Checked := native.GetBoolSetting('ux.tir_color_on');
      cbTirColorCustom.Checked := native.GetBoolSetting('ux.tir_color_custom_on');

      cbTirColorBg.Checked := not cbTirColor.Checked;
      cbTirColorBgCustom.Checked := not cbTirColorCustom.Checked;

      cbTirBar.ButtonColor := native.GetColorSetting('ux.tir_color');
      cbTirBarCustom.ButtonColor := native.GetColorSetting('ux.tir_color_custom');
    end;
  end;

procedure SetupExtensions(f: TfConf);
  begin
    with f, native do
    begin
      {$ifdef TrndiExt}
      eExt.Text := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
    {$else}
      eExt.Text := '- ' + RS_noPlugins + ' -';
      eExt.Enabled := false;
      bExtOpen.Enabled := false;
    {$endif}
      cbPrivacy.Checked := GetBoolSetting('ext.privacy');
      cbTimeStamp.Checked := GetBoolSetting('display.timestamp');
    end;
  end;

procedure SetupTouchAndNotifications(f: TfConf);
  begin
    with f, native do
    begin
      cbTouch.Checked := HasTouch;
      cbMultiTouch.Checked := HasMultiTouch;
    end;
  end;

procedure SaveUserSettings(f: TfConf);
  var
    langCode: string;
    i: integer;
    loVal, hiVal, rangeLoVal, rangeHiVal: double;
  function IndexToChromaAction(const Index: integer): string;
    begin
      case Index of
      1:
        Result := 'static';
      2:
        Result := 'off';
      3:
        Result := 'ignore';
      else
        Result := 'breath';
      end;
    end;
  begin
    with f, native do
    begin
      SetSetting('font.val', f.FontVal.Name);
      SetSetting('font.arrow', f.FontArrow.Name);
      SetSetting('font.ago', f.FontAgo.Name);

      if cbMoveDIffRight.Checked then
        SetAlignmentSetting('ux.labels.ldiff.alignment', taRightJustify)
      else
        SetAlignmentSetting('ux.labels.ldiff.alignment', taCenter);

      // Only overwrite the stored backend when one is actually selected. An
      // unresolvable value leaves cbSys unselected (see the load path), and
      // BackendCode would silently turn it into 'API_NS' — losing the user's
      // configuration on an unrelated save, e.g. after changing a font.
      if cbSys.ItemIndex >= 0 then
        SetSetting('remote.type', BackendCode(cbSys.Text), false);
      SetSetting('remote.target', eAddr.Text, false);
      // Only write credentials the user actually edited. tMain keeps firing
      // while this dialog is modal, so a backend that rotates tokens (CareLink)
      // can have written a fresh blob to 'remote.creds' since the load —
      // writing the field back unconditionally would restore the spent one and
      // leave the stored login dead.
      if ePass.Text <> f.LoadedCreds then
        SetSetting('remote.creds', ePass.Text, false);

      langCode := ExtractLangCode(cbLang.Items[cbLang.ItemIndex]);

      if (langCode = '') or (LowerCase(langCode) = 'auto') then
        langCode := TrndiNative.GetOSLanguage;

      SetSetting('locale', langCode);

      native.SetSetting('position.main', cbPos.ItemIndex);
      native.setSetting('size.main', cbSize.Checked);
      native.SetSetting(HINTS_ENABLED_KEY, cbHints.Checked);
      native.setSetting('alerts.notice.hilo', cbAlertHiLo.Checked);
      native.setSetting('alerts.notice.missing', cbAlertMissing.Checked);
      native.setSetting('alerts.notice.reservoir', cbAlertReservoir.Checked);
      native.setSetting('alerts.notice.sensor', cbAlertSensor.Checked);
      native.setSetting('alerts.notice.battery', cbAlertBattery.Checked);

      native.SetSetting('alerts.duration.high',     spAlertDurHi.Value);
      native.SetSetting('alerts.duration.low',      spAlertDurLo.Value);
      native.SetSetting('alerts.duration.urgent',   spAlertDurUrg.Value);
      if rbUnit.ItemIndex = 0 then
      begin
        native.SetFloatSetting('alerts.hysteresis.high',   fsAlertHystHi.Value * TrndiAPI.toMGDL);
        native.SetFloatSetting('alerts.hysteresis.low',    fsAlertHystLo.Value * TrndiAPI.toMGDL);
        native.SetFloatSetting('alerts.hysteresis.urgent', fsAlertHystUrg.Value * TrndiAPI.toMGDL);
      end
      else
      begin
        native.SetFloatSetting('alerts.hysteresis.high',   fsAlertHystHi.Value);
        native.SetFloatSetting('alerts.hysteresis.low',    fsAlertHystLo.Value);
        native.SetFloatSetting('alerts.hysteresis.urgent', fsAlertHystUrg.Value);
      end;

      native.SetSetting('ux.range_color', cbRangeColor.Checked);
      native.SetSetting('main.clock',     cbClock.Checked);
      native.SetSetting('main.alternate', cbAlternate.Checked);
      native.SetSetting('main.ontop',     cbOnTop.Checked);
      native.SetSetting('main.noborders', cbNoBorders.Checked);

      for i := lbUsers.Items.Count - 1 downto 0 do
        if lbUsers.items[i][1] = '-' then
          lbUsers.items.Delete(i);

      SetCSVSetting('users.names', lbUsers.Items.toStringArray, true);

      SetSetting('users.colorbox', cbUserColor.Checked, true);

      SetBoolSetting('unit', rbUnit.ItemIndex = 0, 'mmol', 'mgdl');
      SetSetting('ext.privacy', cbPrivacy.Checked);
      SetSetting('display.timestamp', cbTimeStamp.Checked);
      SetSetting('display.sensor_expiry', cbShowSensorExpiry.Checked);

      case f.rbTrendWindow.ItemIndex of
      0: SetSetting('ux.dot_count', 6);
      2: SetSetting('ux.dot_count', 18);
      3: SetSetting('ux.dot_count', 24);
      4: SetSetting('ux.dot_count', 36);
      5: SetSetting('ux.dot_count', 50);
      else SetSetting('ux.dot_count', 10);
      end;

      SetSetting('system.fresh_threshold', spTHRESHOLD.Value);
      SetSetting('ux.delta_max', spDeltaMax.Value);

      // Both pairs are ordered as the user leaves each field, but a dialog
      // closed while a field still holds focus never fires OnEditingDone -
      // so never persist a pair that reads "low above high".
      loVal := Min(fsLo.Value, fsHi.Value);
      hiVal := Max(fsLo.Value, fsHi.Value);
      rangeLoVal := Min(fsLoRange.Value, fsHiRange.Value);
      rangeHiVal := Max(fsLoRange.Value, fsHiRange.Value);

      if rbUnit.ItemIndex = 0 then
      begin
        SetSetting('override.lo', Round(loVal * TrndiAPI.toMGDL).ToString);
        SetSetting('override.hi', Round(hiVal * TrndiAPI.toMGDL).ToString);
      end
      else
      begin
        SetSetting('override.lo', Round(loVal).ToString);
        SetSetting('override.hi', Round(hiVal).ToString);
      end;

      // The range fields always hold a usable value now (they fall back to the
      // limits when the backend supplies no range), so writing them
      // unconditionally would hand everyone who opens Settings a personal
      // range they never asked for. Persist only while the box is ticked, and
      // clear the keys otherwise so the backend's own range keeps applying -
      // both override sites read these with the API value as the default.
      if cbCustRange.Checked then
      begin
        if rbUnit.ItemIndex = 0 then
        begin
          SetSetting('override.rangelo', Round(rangeLoVal * TrndiAPI.toMGDL).ToString);
          SetSetting('override.rangehi', Round(rangeHiVal * TrndiAPI.toMGDL).ToString);
        end
        else
        begin
          SetSetting('override.rangelo', Round(rangeLoVal).ToString);
          SetSetting('override.rangehi', Round(rangeHiVal).ToString);
        end;
      end
      else
      begin
        DeleteSetting('override.rangelo');
        DeleteSetting('override.rangehi');
      end;

      native.SetFloatSetting('ux.labels.ldiff.scale', fsDiffScale.Value);
      native.SetFloatSetting('ux.labels.lpredict.scale', fsPredictScale.Value);
      native.SetSetting('range.custom', cbTIR.Checked);
      native.SetSetting('range.tir_icon', cbTirIcon.checked);
      native.SetSetting('tir.displaymean', cbShowMean.Checked);

      native.SetSetting('range.time', seTir.Value);
      native.SetSetting('ux.off_bar', cbOffBar.Checked);
      native.SetSetting('ux.paint_range', cbPaintHiLo.Checked);
      native.SetSetting('ux.paint_range_lines', cbPaintLines.Checked);
      native.SetSetting('ux.paint_range_cgmrange', cbPaintHiLoRange.Checked);
      native.SetSetting('main.next_progress', cbProgress.Checked);
      native.SetSetting('ux.connectivity.button', cbConnectivityButton.Checked);
      native.SetSetting('locale.separator', edCommaSep.Text);


      SetSetting('override.enabled', cbCust.Checked);
      SetSetting('override.range', cbCustRange.Checked);
      SetSetting('predictions.enable', cbPredictions.Checked);
      SetSetting('webserver.enable', cbWebAPI.Checked);
      if TrndiNative.AutoStartAvailable then
        TrndiNative.SetAutoStart(cbAutoStart.Checked);
      SetSetting('predictions.warn', cbWarnLoHi.Checked);
      SetSetting('predictions.short', cbPredictShort.Checked);
      SetSetting('predictions.dots', cbPredictDots.Checked);
      SetSetting('display.rotating_arrow', cbRotatingArrow.Checked);
      SetSetting('display.bolus_overlay', cbBolusOverlay.Checked);
      SetSetting('display.bolus_overlay.auto', cbBolusOverlayAuto.Checked);
      SetSetting('display.carb_overlay', cbCarbOverlay.Checked);
      SetSetting('ux.dot_fresh', cbDotFresh.Checked);
      // A radio group with nothing selected would otherwise store -1, which
      // DotColorModeFromSetting reads back as the default anyway - but leaving
      // the stored value alone keeps the user's choice through a dialog that
      // somehow opened unselected.
      if f.rgDots.ItemIndex >= 0 then
        SetSetting('ux.dot_color_mode', f.rgDots.ItemIndex);
      // Must match the visibility gate above (X_PC included): the checkbox is
      // shown on Linux/BSD, where the setting drives the tray icon arrow and
      // the arrow published to the GNOME/Plasma panel indicators.
      {$if defined(X_WIN) or defined(X_MAC) or defined(X_PC)}
      SetSetting('badge.trend', cbBadgeTrend.Checked);
      {$endif}
      SetSetting('predictions.short.fullarrows', cbPredictShortFullArrows.Checked);
      SetSetting('predictions.short.showvalue', rbPredictShortShowValue.Checked);
      if cbPredictShortSize.ItemIndex >= 0 then
        SetSetting('predictions.short.size', cbPredictShortSize.ItemIndex + 1);
      case cbPredictShortMinutes.ItemIndex of
      0:
        SetSetting('predictions.short.minutes', 5);
      2:
        SetSetting('predictions.short.minutes', 15);
      else
        SetSetting('predictions.short.minutes', 10);
      end;
      SetSetting('media.url_high', edMusicHigh.Text);
      SetSetting('media.url_low', edMusicLow.Text);
      SetSetting('media.url_perfect', edMusicPerfect.Text);

      SetSetting('url_remote.url_high', edURLHigh.Text);
      SetSetting('url_remote.url_low', edURLLow.Text);
      SetSetting('url_remote.url_perfect', edURLPerfect.Text);

      // Proxy settings are root-scoped and owned by fConf.SaveProxySettingsFromUI
      // — see the matching note in LoadUserSettings.

      SetSetting('razer.enabled', cbChroma.Checked);
      SetSetting('razer.normal', cbChromaNormal.Checked);
      SetSetting('razer.high.action', IndexToChromaAction(cbChromaHigh.ItemIndex));
      SetSetting('razer.low.action', IndexToChromaAction(cbChromaLow.ItemIndex));

      SetSetting('media.disabled', cbMediaDisable.Checked);
      SetSetting('media.pause', cbMusicPause.Checked);
      SetSetting('main.high_contrast', cbHContrast.Checked);
      SetSetting('main.announce', cbTTS.Checked);
      // The voice list is filled lazily; if the Accessibility tab was never
      // shown there is no valid selection to persist — keep the old setting.
      if f.TTSVoicesLoaded then
      begin
        SetSetting('tts.voice', cbTTSVoice.ItemIndex);
        // Index 0 is 'Default'; every other entry names a voice the platform
        // TTS layer looks up by name, so the index alone is not enough.
        if cbTTSVoice.ItemIndex > 0 then
          SetSetting('tts.voice.name', cbTTSVoice.Text)
        else
          DeleteSetting('tts.voice.name');
      end;
      SetSetting('tts.rate', seTTSRate.Value);

      SetColorSetting('ux.bg_color_ok', cl_ok_bg.ButtonColor);
      SetColorSetting('ux.bg_color_hi', cl_hi_bg.ButtonColor);
      SetColorSetting('ux.bg_color_lo', cl_lo_bg.ButtonColor);

      SetColorSetting('ux.bg_color_ok_txt', cl_ok_txt.ButtonColor);
      SetColorSetting('ux.bg_color_hi_txt', cl_hi_txt.ButtonColor);
      SetColorSetting('ux.bg_color_lo_txt', cl_lo_txt.ButtonColor);

      SetColorSetting('ux.bg_rel_color_hi', cl_hi_bg_cust.ButtonColor);
      SetColorSetting('ux.bg_rel_color_lo', cl_lo_bg_cust.ButtonColor);

      SetColorSetting('ux.bg_rel_color_hi_txt', cl_hi_txt_cust.ButtonColor);
      SetColorSetting('ux.bg_rel_color_lo_txt', cl_lo_txt_cust.ButtonColor);

      native.SetSetting('ux.title_color', cbTitleColor.Checked);

      native.SetSetting('ux.tir_color_on', cbTirColor.Checked);
      native.SetSetting('ux.tir_color_custom_on', cbTirColorCustom.Checked);

      native.SetColorSetting('ux.tir_color', cbTirBar.ButtonColor);
      native.SetColorSetting('ux.tir_color_custom', cbTirBarCustom.ButtonColor);

      if applocale <> langCode then
        applocale := langCode;
    end;
  end;

var
  extensionsPath: string;
  {$if defined(X_PC)}
  distro: string;
  {$endif}
  needsRestart: boolean;
  origAPI, origTarget: string;
begin
  origAPI := native.GetSetting('remote.type');
  origTarget := native.GetSetting('remote.target');

  fConf := TfConf.Create(Self);
  try
    // The dialog hosts the preview surfaces but cannot render them — the
    // color pipeline lives on this side (see TDotPreviewEvent and
    // TDisplayPreviewEvent in uconf).
    fConf.OnDotPreview := @RenderDotModePreview;
    fConf.OnDisplayPreview := @RenderDisplayPreview;
    with native do
    begin
      fConf.cbSys.Items.Clear;
      // Debug backends are part of the registry in debug builds
      ListBackendNames(fConf.cbSys.Items);
    end;

    // Device enumeration is deferred to the Chroma tab's OnShow — initializing
    // the SDK can block for over a second while connecting to Synapse, which
    // made opening settings sluggish. Hand over the live instance (if any) so
    // the tab reuses it instead of re-initializing.
    if assigned(chroma) and chroma.Initialized then
      fConf.chroma := chroma;

    // Filling the controls fires their OnChange handlers, and a few of those
    // explain the setting in a popup. That explanation is meant for the user
    // flipping the switch - firing it here greeted anyone with a stored
    // override with a dialog every time they opened Settings.
    fConf.SettingsLoading := true;
    try
      LoadUserSettings(fConf);
      LoadLanguageSettings(fConf);
      SetupUIElements(fConf);
      SetupExtensions(fConf);
      SetupTouchAndNotifications(fConf);
    finally
      fConf.SettingsLoading := false;
    end;

    {$if defined(X_PC)}
    fConf.lOS.Caption := GetLinuxDistro(distro) + ' ' + distro;

    {$if defined(LCLQt6)}
    fConf.lWidgetset.Caption := 'QT6 ' + qtVersion;
    {$elseif defined(LCLGTK2)}
    fConf.lWidgetset.Caption := 'GTK2';
    {$elseif defined(LCLGTK3)}
    fConf.lWidgetset.Caption := 'GTK3';
    {$endif}
    {$elseif defined(X_HAIKU)}
    fConf.lOS.Caption := 'Haiku';
    fConf.lWidgetset.Caption := 'Haiku';
    {$elseif defined(X_MAC)}
    fConf.lOS.Caption := 'macOS';
    fConf.lWidgetset.Caption := 'Native Apple Coca';
    {$else}
    fConf.lOS.Caption := 'Windows'  + SysUtils.Win32MajorVersion.tostring + '.' + SysUtils.Win32MinorVersion.tostring + ' - Build ' +  Win32BuildNumber.ToString;
    fConf.lWidgetset.Caption := 'Native Windows';
    {$endif}
    fConf.lWM.Caption := GetWindowManagerName;
    fConf.lArch.Caption := {$I %FPCTargetOS%} + '/' + {$I %FPCTARGETCPU%};

    {$ifdef TrndiExt}
    extensionsPath := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
    ForceDirectoriesUTF8(extensionsPath);
    // Reading + parsing every extension file is deferred to the Extensions
    // tab's OnShow so it doesn't delay opening the dialog.
    fConf.DeferExtensionList(extensionsPath);
    fConf.OnReloadExtensions := @ReloadExtensions;
    {$else}
    fConf.tsExt.Enabled := false;
    fConf.tsExt.Visible := false;
    fConf.lbExtensions.items.add('- Extensions disabled -');
    {$endif}
    // Wayland: swap the compositor's forced decorations for the drawn bar
    // before the window maps, so Settings matches the rest of the app.
    DressModalForm(fConf);
    ShowFormModalSafe(fConf);

    if not firstboot then
    begin
      if IsProblematicWM then
        fBG.Hide;
      if SlickePrompt(sdsAuto, RS_SETTINGS_SAVE, RS_SETTINGS_SAVE_DESC,
        [[mbYes, mbNo], [mbNo, mbYes]]) <> mrYes then
      begin
        fBG.Show;
        Exit;
      end;
      fBG.Show;
    end;

    SaveUserSettings(fConf);

    // Credentials are asked of the dialog rather than diffed against the store:
    // a token rotated on the fetch thread while this was open changes the
    // stored blob without the user having touched anything, and that must not
    // read as "settings changed, restart to apply".
    needsRestart := (origAPI <> native.GetSetting('remote.type')) or
      (origTarget <> native.GetSetting('remote.target')) or
      (fConf.ePass.Text <> fConf.LoadedCreds);

    if firstboot then
      exit;

  finally
    fConf.Free;
  end;

  ApplySettingsInstantly;

  if needsRestart then
    ShowMessage(RS_RESTART_APPLY)
  else
    ShowMessage(RS_SETTINGS_SAVED);
end;

{$ifdef DARWIN}
procedure TfBG.ShowAboutDialog(Sender: TObject);
var
  html: string;
  versionInfo: string;
begin
  versionInfo := 'Version ' + BUILD_NUMBER;
  {$if defined(LCLCocoa)}
  versionInfo := versionInfo + ' (macOS)';
  {$endif}

  html := '<img src="https://trndi.app/doc/img/trndi-logo.png">' + sHTMLLineBreak +
    '<b>Trndi - CGM viewer</b>' + sHTMLLineBreak +
    '<i>Continuous Glucose Monitor data display</i>' + sHTMLLineBreak +
    'for desktop and embedded devices' + sHTMLLineBreak + sHTMLLineBreak +
    versionInfo + sHTMLLineBreak +
    'https://github.com/slicke/trndi';

  SlickeHTMLMsg(sdsAuto, 'About Trndi', html, [mbOK], uxmtInformation, 25);
end;

procedure TfBG.CheckForUpdatesMenuClick(Sender: TObject);
begin
  CheckForUpdates(true);
end;
{$endif}
