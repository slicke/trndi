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
  i: integer;
  dotChar: WChar;
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

  dotChar := native.GetWideCharSetting('font.dot', WChar($2B24));
  for i := 1 to High(TrendDots) do
    if TrendDots[i] <> nil then
      TrendDots[i].Caption := dotChar;

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

  tir_bg := native.GetColorSetting('ux.tir_color', tir_bg);
  tir_custom_bg := native.GetColorSetting('ux.tir_color_custom', tir_custom_bg);
  titlecolor := native.GetBoolSetting('ux.title_color', true);

  PredictGlucoseReading := native.GetBoolSetting('predictions.enable', false);
  PredictShortMode := native.GetBoolSetting('predictions.short', false);
  PredictDotMode := native.GetBoolSetting('predictions.dots', false);
  RotatingArrow := native.GetBoolSetting('display.rotating_arrow', false);
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
  lPredict.Visible := PredictGlucoseReading;

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

  tSwap.Enabled := native.GetBoolSetting('main.alternate', false);

  if native.GetBoolSetting('main.ontop', false) then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;

  if native.GetBoolSetting('main.noborders', false) then
    Self.BorderStyle := bsNone
  else
  {$ifdef DARWIN}
    Self.BorderStyle := bsSizeable;
  {$else}
    Self.BorderStyle := bsSizeToolWin;
  {$endif}

  ResizeUIElements;
  ScaleLbl(lVal);
  ScaleLbl(lArrow);
  // ScaleLbl re-shows lArrow; re-hide the glyph when the rotating arrow is on.
  if RotatingArrow and Assigned(FTrendArrow) then
  begin
    lArrow.Visible := false;
    FTrendArrow.Visible := true;
  end;
  ScaleLbl(lAgo);
  if Assigned(lPredict) and lPredict.Visible then
    ScaleLbl(lPredict, taRightJustify, tlBottom);

  newDotCount := native.GetIntSetting('ux.dot_count', NUM_DOTS);
  if (newDotCount >= 1) and (newDotCount <= MAX_DOT_COUNT) and (newDotCount <> ACTIVE_DOTS) then
  begin
    ACTIVE_DOTS := newDotCount;
    if MAX_MIN < ACTIVE_DOTS * INTERVAL_MINUTES + 10 then
      MAX_MIN := ACTIVE_DOTS * INTERVAL_MINUTES + 10;
    for i := 1 to High(TrendDots) do
      if Assigned(TrendDots[i]) then
      begin
        TrendDots[i].Free;
        TrendDots[i] := nil;
      end;
    SetLength(TrendDots, 0);
    for i := 1 to High(FWarningDots) do
      if Assigned(FWarningDots[i]) then
      begin
        FWarningDots[i].Free;
        FWarningDots[i] := nil;
      end;
    SetLength(FWarningDots, 0);
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
    FLastReadingsHash := 0;
    Application.ProcessMessages;
    tResize.OnTimer(Self);
    if Length(bgs) > 0 then
      PlaceTrendDots(bgs);
  end;

  UpdateTrendDots;
  for i := 1 to High(TrendDots) do
    if TrendDots[i] <> nil then
      TrendDots[i].Invalidate;

  UpdateUIColors;
  UpdateUIBasedOnGlucose;

  ProcessCurrentReading;

  if Assigned(fFloat) then
    UpdateFloatingWindow;

  Self.Invalidate;
end;

// Handle settings menu click
procedure TfBG.miSettingsClick(Sender: TObject);
var
  fConf: TfConf;
  lastUsers: integer;
  minutesStr: string;
  minutesVal: integer;

procedure LoadUserSettings(f: TfConf);
  function APIToName(const str: string): string;
    begin
      case str of
      'API_NS':
        result := API_NS;
      'API_NS3':
        result := API_NS3;
      'API_DEX_USA':
        result := API_DEX_USA;
      'API_DEX_EU':
        result := API_DEX_EU;
      'API_DEX_NEW_USA':
        result := API_DEX_NEW_USA;
      'API_DEX_NEW_EU':
        result := API_DEX_NEW_EU;
      'API_DEX_NEW_JP':
        result := API_DEX_NEW_JP;
      'API_TANDEM_USA':
        result := API_TANDEM_USA;
      'API_TANDEM_EU':
        result := API_TANDEM_EU;
      'API_XDRIP':
        result := API_XDRIP;
      {$ifdef DEBUG}
      'API_D_DEBUG':
        result := API_D_DEBUG;
      'API_D_MISSING':
        result := API_D_MISSING;
      'API_D_PERFECT':
        result := API_D_PERFECT;
      'API_D_CUSTOM':
        result := API_D_CUSTOM;
      'API_D_EDGE':
        result := API_D_EDGE;
      'API_D_FIRST':
        result := API_D_FIRST;
      'API_D_FIRSTX':
        result := API_D_FIRSTX;
      'API_D_INTERMITTENT':
        result := API_D_INTERMITTENT;
      'API_D_SLOW':
        result := API_D_SLOW;
      'API_D_FAULTY':
        result := API_D_FAULTY;
      'API_D_LOWSOON':
        result := API_D_LOWSOON;
      'API_D_SENSOR_EXPIRY':
        result := API_D_SENSOR_EXPIRY;
      'API_D_LATE_MISSING':
        result := API_D_LATE_MISSING;
      'API_D_SECOND':
        result := API_D_SECOND;
    {$endif}
      else
        Result := 'API_NS';
      end;
    end;
  var
    remoteType: string;
    userNames: TStringArray;
    hasUsers: boolean;
    posName: string;
    i: integer;
    usersStr: string;
    posValue: integer;
    po: TrndiPos;
    sizeVal: integer;
    minutesVal: integer;
    voiceName: string;
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
      remoteType := APIToName(GetSetting('remote.type'));
      cbSys.ItemIndex := 0;
      for i := 0 to cbSys.Items.Count - 1 do
        if cbSys.Items[i] = remoteType then
          cbSys.ItemIndex := i;
      f.cbSysChange(f);
      eAddr.Text := GetSetting('remote.target');
      ePass.Text := GetSetting('remote.creds');
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
      spDeltaMax.Value := native.GetIntSetting('ux.delta_max',
        DATA_FRESHNESS_THRESHOLD_MINUTES);

      if api = nil then
      begin
        fsLo.Value := GetIntSetting('override.lo', 60);
        fsHi.Value := GetIntSetting('override.hi', 180);
        fsLoRange.Value := GetIntSetting('override.rangelo', 80);
        fsHiRange.Value := GetIntSetting('override.rangehi', 160);
      end
      else
      begin
        fsLo.Value := GetIntSetting('override.lo', api.cgmLo);
        fsHi.Value := GetIntSetting('override.hi', api.cgmHi);
        fsLoRange.Value := GetIntSetting('override.rangelo', api.cgmRangeLo);
        fsHiRange.Value := GetIntSetting('override.rangehi', api.cgmRangeHi);
      end;

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

      cbCust.Checked := GetBoolSetting('override.enabled');
      cbCustRange.Checked := GetBoolSetting('override.range');
      cbPredictions.Checked := GetBoolSetting('predictions.enable');
      cbWebAPI.Checked := GetBoolSetting('webserver.enable');
      cbPredictShort.Checked := GetBoolSetting('predictions.short');
      cbPredictDots.Checked := GetBoolSetting('predictions.dots');
      cbRotatingArrow.Checked := GetBoolSetting('display.rotating_arrow');
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

      edProxyHost.Text := GetSetting('proxy.host', '');
      edProxyPort.Text := GetSetting('proxy.port', '8080');
      edProxyUser.Text := GetSetting('proxy.user', '');
      edProxyPass.Text := GetSetting('proxy.pass', '');

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
      cbTTSVoice.ItemIndex := GetIntSetting('tts.voice', 0);
      {$ifdef X_MAC}
      if cbTTSVoice.Items.Count > 1 then
      begin
        voiceName := GetSetting('tts.voice.name', '');
        if voiceName <> '' then
          cbTTSVoice.ItemIndex := cbTTSVoice.Items.IndexOf(voiceName);
      end;
    {$endif}
      {$ifdef X_PC}
      if cbTTSVoice.Items.Count > 1 then
      begin
        voiceName := GetSetting('tts.voice.name', '');
        if voiceName <> '' then
          cbTTSVoice.ItemIndex := cbTTSVoice.Items.IndexOf(voiceName);
      end;
    {$endif}
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
      cbFlash.Visible := false;

      cbRangeColor.Checked := native.GetBoolSetting('ux.range_color', true);
      cbClock.Checked      := native.GetBoolSetting('main.clock', false);
      cbAlternate.Checked  := native.GetBoolSetting('main.alternate', false);
      cbOnTop.Checked      := native.GetBoolSetting('main.ontop', false);
      cbNoBorders.Checked  := native.GetBoolSetting('main.noborders', false);

      cbAlertHiLo.Checked := native.getBoolSetting('alerts.notice.hilo', true);
      cbAlertMissing.Checked := native.getBoolSetting('alerts.notice.missing', true);

      spAlertDurHi.Value   := native.GetIntSetting('alerts.duration.high', 10);
      spAlertDurLo.Value   := native.GetIntSetting('alerts.duration.low', 5);
      spAlertDurUrg.Value  := native.GetIntSetting('alerts.duration.urgent', 0);
      fsAlertHystHi.Value  := native.GetFloatSetting('alerts.hysteresis.high', 5);
      fsAlertHystLo.Value  := native.GetFloatSetting('alerts.hysteresis.low', 5);
      fsAlertHystUrg.Value := native.GetFloatSetting('alerts.hysteresis.urgent', 5);
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

      lVal.Font.Name := Self.lVal.Font.Name;
      lArrow.Font.Name := Self.lArrow.Font.Name;
      lAgo.Font.Name := Self.lAgo.Font.Name;
      lVal.Font.Color := Self.lVal.Font.Color;
      lArrow.Font.Color := Self.lArrow.Font.Color;
      lAgo.Font.Color := Self.lAgo.Font.Color;
      lVal.Caption := Self.lVal.Caption;
      lArrow.Caption := Self.lArrow.Caption;
      lAgo.Caption := Self.lAgo.Caption;
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
  function APIToCode(const str: string): string;
    begin
      case str of
      API_NS:
        result := 'API_NS';
      API_NS3:
        result := 'API_NS3';
      API_DEX_USA:
        result := 'API_DEX_USA';
      API_DEX_EU:
        result := 'API_DEX_EU';
      API_DEX_NEW_USA:
        result := 'API_DEX_NEW_USA';
      API_DEX_NEW_EU:
        result := 'API_DEX_NEW_EU';
      API_DEX_NEW_JP:
        result := 'API_DEX_NEW_JP';
      API_TANDEM_USA:
        result := 'API_TANDEM_USA';
      API_TANDEM_EU:
        result := 'API_TANDEM_EU';
      API_XDRIP:
        result := 'API_XDRIP';
      {$ifdef DEBUG}
      API_D_DEBUG:
        result := 'API_D_DEBUG';
      API_D_MISSING:
        result := 'API_D_MISSING';
      API_D_PERFECT:
        result := 'API_D_PERFECT';
      API_D_CUSTOM:
        result := 'API_D_CUSTOM';
      API_D_EDGE:
        result := 'API_D_EDGE';
      API_D_FIRST:
        result := 'API_D_FIRST';
      API_D_FIRSTX:
        result := 'API_D_FIRSTX';
      API_D_INTERMITTENT:
        result := 'API_D_INTERMITTENT';
      API_D_SLOW:
        result := 'API_D_SLOW';
      API_D_FAULTY:
        result := 'API_D_FAULTY';
      API_D_LOWSOON:
        result := 'API_D_LOWSOON';
      API_D_SENSOR_EXPIRY:
        result := 'API_D_SENSOR_EXPIRY';
      API_D_LATE_MISSING:
        result := 'API_D_LATE_MISSING';
      API_D_SECOND:
        result := 'API_D_SECOND';
    {$endif}
      else
        Result := 'API_NS';
      end;
    end;

  var
    langCode: string;
    i: integer;
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
      SetSetting('font.val', lVal.Font.Name);
      SetSetting('font.arrow', lArrow.Font.Name);
      SetSetting('font.ago', lAgo.Font.Name);

      if cbMoveDIffRight.Checked then
        SetAlignmentSetting('ux.labels.ldiff.alignment', taRightJustify)
      else
        SetAlignmentSetting('ux.labels.ldiff.alignment', taCenter);

      SetSetting('remote.type', APIToCode(cbSys.Text), false);
      SetSetting('remote.target', eAddr.Text, false);
      SetSetting('remote.creds', ePass.Text, false);

      langCode := ExtractLangCode(cbLang.Items[cbLang.ItemIndex]);

      if (langCode = '') or (LowerCase(langCode) = 'auto') then
        langCode := TrndiNative.GetOSLanguage;

      SetSetting('locale', langCode);

      native.SetSetting('position.main', cbPos.ItemIndex);
      native.setSetting('size.main', cbSize.Checked);
      native.setSetting('alerts.notice.hilo', cbAlertHiLo.Checked);
      native.setSetting('alerts.notice.missing', cbAlertMissing.Checked);

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

      if rbUnit.ItemIndex = 0 then
      begin
        SetSetting('override.lo', Round(fsLo.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.hi', Round(fsHi.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.rangelo', Round(fsLoRange.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.rangehi', Round(fsHiRange.Value * TrndiAPI.toMGDL).ToString);
      end
      else
      begin
        SetSetting('override.lo', Round(fsLo.Value).ToString);
        SetSetting('override.hi', Round(fsHi.Value).ToString);
        SetSetting('override.rangelo', Round(fsLoRange.Value).ToString);
        SetSetting('override.rangehi', Round(fsHiRange.Value).ToString);
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

      SetSetting('proxy.host', edProxyHost.Text);
      SetSetting('proxy.port', edProxyPort.Text);
      SetSetting('proxy.user', edProxyUser.Text);
      SetSetting('proxy.pass', edProxyPass.Text);

      SetSetting('razer.enabled', cbChroma.Checked);
      SetSetting('razer.normal', cbChromaNormal.Checked);
      SetSetting('razer.high.action', IndexToChromaAction(cbChromaHigh.ItemIndex));
      SetSetting('razer.low.action', IndexToChromaAction(cbChromaLow.ItemIndex));

      SetSetting('media.disabled', cbMediaDisable.Checked);
      SetSetting('media.pause', cbMusicPause.Checked);
      SetSetting('main.high_contrast', cbHContrast.Checked);
      SetSetting('main.announce', cbTTS.Checked);
      SetSetting('tts.voice', cbTTSVoice.ItemIndex);
      {$ifdef X_MAC}
      if cbTTSVoice.ItemIndex > 0 then
        SetSetting('tts.voice.name', cbTTSVoice.Text)
      else
        DeleteSetting('tts.voice.name');
    {$endif}
      {$ifdef X_PC}
      if cbTTSVoice.ItemIndex > 0 then
        SetSetting('tts.voice.name', cbTTSVoice.Text)
      else
        DeleteSetting('tts.voice.name');
    {$endif}
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
  i: integer;
  distro: string;
  needsRestart: boolean;
  origAPI, origTarget, origCreds: string;
begin
  origAPI := native.GetSetting('remote.type');
  origTarget := native.GetSetting('remote.target');
  origCreds := native.GetSetting('remote.creds');

  fConf := TfConf.Create(Self);
  try
    with native do
    begin
      fConf.cbSys.Items.Clear;
      fConf.cbSys.Items.AddStrings([
        API_NS,
        API_NS3,
        API_DEX_USA,
        API_DEX_EU,
        API_DEX_NEW_USA,
        API_DEX_NEW_EU,
        API_DEX_NEW_JP,
        API_TANDEM_USA,
        API_TANDEM_EU,
        API_XDRIP
        ]);
      {$ifdef DEBUG}
      fConf.cbSys.Items.AddStrings(API_DEBUG);
    {$endif}
    end;

    if assigned(chroma) and chroma.Initialized then
      for i := 0 to Chroma.GetDeviceCount - 1 do
        fConf.lbChroma.Items.Add(Chroma.GetDevice(i).Name) else
    begin
      fConf.chroma := TRazerChromaFactory.CreateInstance;
      if not fconf.chroma.Initialize then
        fConf.lbChroma.Items.Add('No Razer driver detected')
      else for i := 0 to fConf.Chroma.GetDeviceCount - 1 do
          fConf.lbChroma.Items.Add(fConf.Chroma.GetDevice(i).Name);
      fConf.Chroma.Free;
    end;

    LoadUserSettings(fConf);
    LoadLanguageSettings(fConf);
    SetupUIElements(fConf);
    SetupExtensions(fConf);
    SetupTouchAndNotifications(fConf);

    lastUsers := fConf.lbUsers.Count;

    {$if defined(X_PC)}
    fConf.lOS.Caption := GetLinuxDistro(distro) + ' ' + distro;

    {$if defined(LCLQt6)}
    fConf.lWidgetset.Caption := 'QT6 ' + qtVersion;
    {$elseif defined(LCLGTK2)}
    fConf.lWidgetset.Caption := 'GTK2';
    {$elseif defined(LCLGTK3)}
    fConf.lWidgetset.Caption := 'GTK3';
    {$endif}
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
    fConf.LoadExtensionList(extensionsPath);
    fConf.OnReloadExtensions := @ReloadExtensions;
    {$else}
    fConf.tsExt.Enabled := false;
    fConf.tsExt.Visible := false;
    fConf.lbExtensions.items.add('- Extensions disabled -');
    {$endif}
    ShowFormModalSafe(fConf);

    if not firstboot then
    begin
      if IsProblematicWM then
        fBG.Hide;
      if ExtText(uxdAuto, RS_SETTINGS_SAVE,RS_SETTINGS_SAVE_DESC,[mbYes, mbNo]) <> mrYes then
      begin
        fBG.Show;
        Exit;
      end;
      fBG.Show;
    end;

    SaveUserSettings(fConf);

    needsRestart := (origAPI <> native.GetSetting('remote.type')) or
      (origTarget <> native.GetSetting('remote.target')) or
      (origCreds <> native.GetSetting('remote.creds'));

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

  ExtHTML(uxdAuto, 'About Trndi', html, [mbOK], uxmtInformation, 25);
end;

procedure TfBG.CheckForUpdatesMenuClick(Sender: TObject);
begin
  CheckForUpdates(true);
end;
{$endif}
