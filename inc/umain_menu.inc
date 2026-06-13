(* Menu event handlers and UI click handlers for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

procedure TfBG.lDiffClick({%H-}Sender: TObject);
begin
  ShowMessage(RS_DIFF);
end;

procedure TfBG.lPredictClick(Sender: TObject);
begin
  SHowMessage(RS_PREDICT);
end;

procedure TfBG.miBasalRateClick(Sender: TObject);
var
  res: single;
  err: string;
begin
  res := api.getBasalRate;
  err := api.errormsg;
  if res = 0 then
    ShowMessage(Format('%s: %s', [RS_NODATA_ERROR, IfThen(err <> '', err, RS_NODATA_NONE)]))
  else
    ShowMessage(res.toString);
end;

procedure TfBG.AutoEnableBasalOverlay;
var
  profile: TBasalProfile;
  ok: boolean;
begin
  if not api.supportsBasal then
    Exit;

  ok := api.getBasalProfile(profile);
  if not ok then
    Exit;

  if fHistoryGraph <> nil then
  begin
    fHistoryGraph.SetBasalProfile(profile);
    fHistoryGraph.SetBasalOverlayEnabled(true);
  end;
end;

procedure TfBG.AutoAddPredictionOverlay;
var
  predictions: BGResults;
begin
  if (api = nil) or (fHistoryGraph = nil) then
    Exit;
  if api.predictReadings(6, predictions) then
    fHistoryGraph.SetPredictions(predictions);
end;

procedure TfBG.miDNSClick(Sender: TObject);
begin
  tPingTimer(miDNS);
end;

procedure TfBG.miDotsInViewClick(Sender: TObject);
var
  i: integer;
begin
  i := dotsInView;
  ShowMessage(IfThen(i = 0, 'Yes', 'Offset: ' + i.toString));
end;

procedure TfBG.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfBG.miATouchAutoClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;

  native.touchOverride := tbUnknown;
end;

procedure TfBG.miADotAdjustClick(Sender: TObject);
var
  mr: TModalResult;
  da: single;
begin
  da := (ExtNumericInput(uxdAuto, RS_SERVICE_DOT_ADJUST, RS_SERVICE_DOT_ADJUST_ADD,
    RS_SERVICE_DOT_ADJUST_DESC, DOT_ADJUST *
    100, FLOAT_NONE, FLOAT_NONE, false, mr) / 100);

  if mr = mrOk then
  begin
    DOT_ADJUST := da;

    if ExtMsg(uxdAuto, RS_UX_CHANGES_SAVE, RS_UX_CHANGES_REMEMBER, RS_CHANGES_APPLY,
      FormatFloat('0.00', da), uxclWhite, uxclRed, [mbYes, mbNo]) = mrYes then
      native.SetFloatSetting('ux.dot_adjust', da);
  end;
end;

procedure TfBG.miADotScaleClick(Sender: TObject);
begin
end;

procedure TfBG.miADotsClick(Sender: TObject);
begin
  ShowMessage(Format(RS_SERVICE_ADJUST, [DOT_ADJUST * 100, dotscale]));
end;

procedure TfBG.miASystemInfoClick(Sender: TObject);
var
  sysver, platformInfo: string;
  {$ifdef Linux}
  ver: string;
  {$endif}
begin
  {$if defined(LCLWin32)}
  sysver := SysUtils.Win32MajorVersion.tostring + '.' + SysUtils.Win32MinorVersion.tostring + ' - Build ' +  Win32BuildNumber.ToString;
  {$elseif defined(Linux)}
  platformInfo := getlinuxdistro(ver);
  sysver := platformInfo + ' ' + ver;
  {$endif}

  {$if defined(LCLQt6)}
  platformInfo := 'QT6 - ' + qtVersion + ' - ' + sysver;
  {$elseif defined(LCLGTK2)}
  platformInfo := 'GTK2 - '  + sysver;
  {$elseif defined(LCLGTK3)}
  platformInfo := 'GTK3';
  {$elseif defined(LCLWIN32)}
  platformInfo := 'Windows Native - ' + sysver;
  {$elseif defined(LCLCocoa)}
  platformInfo := 'macOS Native';
  {$else}
  platformInfo := 'unsupportd widgetset';
  {$endif}

  ShowMessage(Format(RS_SERVICE_SYSINFO, [{$I %FPCTargetOS%}, {$I %FPCTargetCPU%}, platformInfo, DefaultFormatSettings.DecimalSeparator]));
end;

procedure TfBG.miATouchClick(Sender: TObject);
begin
end;

procedure TfBG.miATouchNoClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;
  native.touchOverride := tbFalse;
end;

procedure TfBG.miATouchYesClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;
  native.touchOverride := tbTrue;
end;

procedure TfBG.miDebugBackendClick(Sender: TObject);
begin
  miDebugBackend.Checked := not miDebugBackend.Checked;
end;

procedure TfBG.miExtLogClick(Sender: TObject);
begin
  {$ifdef TrndiExt}
  with TTrndiExtEngine.Instance do
  begin
    ShowMessage(Output);
    ClearOutput;
  end;
  {$endif}
end;

procedure TfBG.miGuidelinesClick(Sender: TObject);
begin
  miGuidelines.Checked := not miGuidelines.Checked;
  DOT_LINES := miGuidelines.Checked;
end;

procedure TfBG.miPredictClick(Sender: TObject);
var
  bgr: BGResults;
  i: integer;
  msg: string;
  mr: TModalResult;
begin
  i := ExtIntInput(uxdAuto, RS_PREDICT_AMOUNT_CAPTION, RS_PREDICT_AMOUNT_TITLE, RS_PREDICT_AMOUNT_DESC, 3, mr);
  if (mr <> mrOK) or (i < 1) then
    i := 3;
  if i > 20 then
    i := 20;

  if not api.predictReadings(i, bgr) then
  begin
    ShowMessage(Format(RS_SERVICE_PREDICT_UNABLE, [api.errormsg]));
    Exit;
  end;

  msg := RS_SERVICE_PREDICTIONS + LineEnding;

  if Length(bgr) > 0 then
    for i := 0 to High(bgr) do
      msg := msg + Format(RS_SERVICE_PREDICT_POINT, [
        i + 1,
        bgr[i].convert(un),
        BG_UNIT_NAMES[un],
        FormatDateTime('hh:nn', bgr[i].date)
        ]) + LineEnding;

  ShowMessage(msg);
end;

procedure TfBG.miReadingsSinceClick(Sender: TObject);
var
  minDate, selectedDate: TDateTime;
  mr: TModalResult;
  minutesSince, maxReadings: integer;
  maxAge, maxDays: integer;
  description: string;
begin
  maxAge := api.getMaxAge;

  if maxAge = -1 then
  begin
    minDate := EncodeDate(2000, 1, 1);
    description := Format(RS_DATE_PICKER_DESC_UNLIMITED, [api.getSystemName]);
  end
  else
  begin
    maxDays := maxAge div 1440;
    minDate := Trunc(Now) - maxDays;
    description := Format(RS_DATE_PICKER_DESC_LIMITED, [api.getSystemName, maxDays]);
  end;

  selectedDate := ExtDatePicker(
    uxdAuto,
    RS_DATE_PICKER_CAPTION,
    RS_DATE_PICKER_TITLE,
    description,
    minDate,
    minDate,
    Trunc(Now),
    mr,
    uxmtOK
    );

  if mr = mrOk then
  begin
    minutesSince := MinutesBetween(Now, selectedDate);
    maxReadings := minutesSince div 5;
    RequestHistoryFetch(minutesSince, maxReadings, hfmDatePicker);
  end;
end;

procedure TfBG.pmSettingsClose(Sender: TObject);
begin
  last_popup := now;
end;

procedure TfBG.pnWarningClick(Sender: TObject);
begin
  {$ifdef TrndiExt}
  if not funcBool('uxClick',
    ['no-reading'], true) then
    Exit;
  {$endif}
  // Inline expand: clicking the panel toggles a detail row inside the banner
  // (or the bottom area in critical mode). No modal — feels native and
  // respects the user's current focus.
  if not WarnShowDetails then
    Exit;
  FWarnExpanded := not FWarnExpanded;
  fixWarningPanel;
  pnWarning.Refresh;
end;

procedure TfBG.lAgoClick(Sender: TObject);
var
  displayMsg, dm, deviceStr, expiryStr, lowerSensor: string;
  s: system.utf8char;
  i, pOpen, pClose: integer;
begin
  dm := '';
  if firstboot then
    exit;

  displayMsg := miRefresh.Caption + sHTMLLineBreak + sHTMLLineBreak;
  for s in displayMsg do
    if s in ['0'..'9']  then
      dm += Format('<b>%s</b>', [s])
    else
      dm += s;
  displayMsg := dm;

  if lastReading.TryGetRSSI(i) then
    displayMsg += sHTMLLineBreak + Format(RS_UX_RSSI, [i]);
  if lastReading.TryGetNoise(i) then
    displayMsg += sHTMLLineBreak + Format(RS_UX_NOISE, [i]);

  deviceStr := lastReading.sensor;
  expiryStr := '';
  lowerSensor := LowerCase(deviceStr);
  pOpen := Pos('(sensor ', lowerSensor);
  if pOpen > 1 then
  begin
    pClose := PosEx(')', deviceStr, pOpen);
    if pClose > pOpen then
      expiryStr := Copy(deviceStr, pOpen + 1, pClose - pOpen - 1)
    else
      expiryStr := Copy(deviceStr, pOpen + 1, Length(deviceStr) - pOpen);
    expiryStr := Trim(expiryStr);
    deviceStr := Trim(Copy(deviceStr, 1, pOpen - 1));
    if Pos('sensor ', LowerCase(expiryStr)) = 1 then
      expiryStr := Trim(Copy(expiryStr, Length('sensor ') + 1, MaxInt));
  end;

  displayMsg += sHTMLLineBreak + Format(RS_UX_DEVICE, [deviceStr]);
  if expiryStr <> '' then
    displayMsg += sHTMLLineBreak + Format(RS_UX_SENSOR_EXPIRY, [expiryStr]);

  displayMsg += sHTMLLineBreak + Format(RS_UX_API, [api.systemName]);

  ExtHTML(uxdAuto, RS_UX_TRANSMITTER_INFO, displayMsg, [mbOK], uxmtInformation, 10);
end;

procedure TfBG.lArrowClick(Sender: TObject);
begin
end;

procedure TfBG.lDot7DblClick(Sender: TObject);
begin
end;

procedure TfBG.lgMainClick(Sender: TObject);
begin
end;

procedure TfBG.lTirClick(Sender: TObject);
var
  minTotal, hours, mins, rangeMinutes: integer;
  msg, row2, row3: string;
  hi, lo, rhi, rlo: double;
begin
  if Length(bgs) = 0 then
    Exit;
  minTotal := MinutesBetween(now, bgs[High(bgs)].date);
  rangeMinutes := native.GetIntSetting('range.time', 9999);
  if minTotal > rangeMinutes then
    minTotal := rangeMinutes;

  {$ifdef TrndiExt}
  if not funcBool('uxClick',[
    'tir',
    mintotal,
    lTir.Caption
    ], true) then
    Exit;
  {$endif}

  hi := api.cgmHi * BG_CONVERTIONS[un][mgdl];
  lo := api.cgmLo * BG_CONVERTIONS[un][mgdl];
  rhi := api.cgmRangeHi * BG_CONVERTIONS[un][mgdl];
  rlo := api.cgmRangeLo * BG_CONVERTIONS[un][mgdl];

  row2 := Format(RS_TIR_ROW2, [lo, hi]);
  row3 := Format(RS_TIR_ROW3, [rlo, rhi]);
  if minTotal < 60 then
    msg := Format(RS_TIR_M, ['<b>'+minTotal.toString+'</b>'])
  else
  begin
    hours := minTotal div 60;
    mins := minTotal mod 60;
    if hours < 2 then
      msg := Format(RS_TIR_H1, ['<b>'+mins.toString+'</b>'])
    else
      msg := Format(RS_TIR_H, ['<b>'+hours.toString+'</b>', '<b>'+mins.toString+'</b>']);
  end;

  ExtHTML(uxdAuto, 'Trndi', msg+'<br><br><i>'+row2+'<br>'+row3+'</i>');
end;

procedure TfBG.lValClick(Sender: TObject);
begin
  if (lVal.Caption = RS_SETUP) then
    miSettings.Click;
  if firstboot then
    exit;
  StopFlashing;
end;

procedure TfBG.lValDblClick(Sender: TObject);
begin
  if HasTouch then
    DoFullScreen;
end;

procedure TfBG.lValStartDrag(Sender: TObject; var {%H-}DragObject: TDragObject);
begin
end;

procedure TfBG.miAnnounceClick(Sender: TObject);
begin
end;

procedure TfBG.TfFloatOnHide(Sender: TObject);
begin
  miFloatOn.Checked := fFloat.Showing;
end;

procedure TfBG.miFloatOnClick(Sender: TObject);
begin
  {$ifdef LCLGTK3}
  Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
  Exit;
  {$endif}
  if fFloat.Showing then
    fFloat.Hide
  else
  begin
    fFloat.Show;
    if not assigned(fFloat.onhide) then
      ffloat.OnHide := @TfFloatOnHide;
    fFloat.Color := fBg.Color;
    fFloat.lVal.Caption := lval.Caption;
    fFloat.lVal.Visible := true;
    if fFloat.miFontMain.Checked then
    begin
      fFloat.lVal.font.color := lval.font.color;
      fFloat.larrow.font.color := fFloat.lVal.font.color;
    end;
    fFloat.lArrow.Caption := lArrow.Caption;
    fFloat.lArrow.Visible := fFloat.lArrow.Caption <> '';
    if pnMultiUser.Visible then
    begin
      fFloat.pnMultiUser.Visible := true;
      fFloat.pnMultiUser.Color := pnMultiUser.Color;
    end;
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
  miFloatOn.Checked := fFloat.Showing;
end;

procedure TfBG.mi24hClick(Sender: TObject);
begin
  RequestHistoryFetch(1440, 288, hfm24h);
end;

procedure TfBG.miHistoryClick(Sender: TObject);
begin
  ShowHistoryGraph(bgs, un, CurrentHistoryGraphPalette,
    api.cgmHi, api.cgmLo, api.cgmRangeHi, api.cgmRangeLo);
  AutoEnableBasalOverlay;
  AutoAddPredictionOverlay;
end;

// Force update on menu click
procedure TfBG.miForceClick(Sender: TObject);
const
  API_CACHE_SECONDS = 10;
var
  secondsSinceLastCall: integer;
  waitTime: integer;
  msg: string;
  Result: integer;
begin
  if firstboot then
    exit;

  if FLastAPICall = 0 then
  begin
    updateReading;
    Exit;
  end;

  secondsSinceLastCall := SecondsBetween(Now, FLastAPICall);
  if secondsSinceLastCall < API_CACHE_SECONDS then
  begin
    waitTime := API_CACHE_SECONDS - secondsSinceLastCall;
    msg := Format(RS_FORCE_REFRESH_CACHED, [secondsSinceLastCall, waitTime]);

    Result := slicke.UX.alert.ExtMsg(uxdAuto, RS_REFRESH_PROMPT, msg,
      RS_FORCE_REFRESH_DETAIL, '', uxclWhite, uxclRed, [mbRetry, mbCancel],
      uxmtInformation);

    if Result = mrRetry then
    begin
      FForceRefresh := true;
      updateReading;
    end;
  end
  else
    updateReading;
end;

procedure TfBG.miLimitExplainClick(Sender: TObject);
begin
  ShowMessage(RS_LIMIT_EXPLAIN_TEXT);
end;

procedure TfBG.pmSettingsMeasureItem(Sender: TObject; ACanvas: TCanvas;
var AWidth, AHeight: integer);
const
  PaddingHorizontal = 40;
  PaddingVertical = 8;
  MinItemWidth = 100;
  MinItemHeight = 25;
begin
  if not (Sender is TMenuItem) then
    Exit;

  with TMenuItem(Sender) do
  begin
    if Caption = '-' then
      Exit;

    with ACanvas.Font do
    begin
      Name := 'Arial';
      Size := 16;
      Style := [fsBold];
    end;

    AWidth := ACanvas.TextWidth(Caption) + PaddingHorizontal;
    AHeight := ACanvas.TextHeight(Caption) + PaddingVertical;

    AWidth := Max(AWidth, MinItemWidth);
    AHeight := Max(AHeight, MinItemHeight);
  end;
end;

procedure TfBG.pmSettingsPopup(Sender: TObject);
var
  shifted: boolean;
{$ifdef LCLQt6}
function SafeQtStyle(Handle: QWidgetH; const Style: string): boolean;
  var
    ws: widestring;
  begin
    Result := false;
    if Handle = nil then
      Exit;
    if PtrUInt(Handle) < $1000 then
      Exit;
    try
      if not QWidget_isEnabled(Handle) and QWidget_isEnabled(Handle) then
        Exit;
      ws := UTF8Decode(Style);
      QWidget_setStyleSheet(Handle, PWideString(@ws));
      Result := true;
    except
      Result := false;
    end;
  end;

function ShiftKeyPressed: boolean;
  var
    modifiers: QtKeyboardModifiers;
  begin
    modifiers := QGuiApplication_keyboardModifiers;
    Result := (modifiers and QtShiftModifier) <> 0;
  end;
{$else}
function ShiftKeyPressed: boolean;
  begin
    Result := ssShift in GetKeyShiftState;
  end;
{$endif}
var
  tpb: TDotControl;
  H, M: integer;
begin
  Application.ProcessMessages;
  try
    shifted := ShiftKeyPressed;
  except
    shifted := false;
  end;
  miAdvanced.Visible := shifted;
  miATouch.Checked := HasTouch;
  {$ifdef DEBUG}
  miDebugBackend.Visible := true;
  {$endif}

  miDotVal.Checked := false;

  BuildDotWindowMenu;

  if (Sender as TPopupMenu).PopupComponent is TDotControl then
  begin
    tpb := (Sender as TPopupMenu).PopupComponent as TDotControl;
    H := tpb.Tag div 100;
    M := tpb.Tag mod 100;

    miDotVal.Visible := true;
    if tpb.Caption = DOT_PREDICT then
      miDotVal.Caption := Format(RS_UX_PREDICTION_HERE, [tpb.hint, H, M])
    else
      miDotVal.Caption := Format(RS_UX_READING_HERE, [tpb.hint, H, M]);
  end
  else
    miDotVal.Visible := false;

  UpdateAlertSnoozeMenu;
  last_popup := now;
end;

procedure TfBG.pnMultiUserClick(Sender: TObject);
begin
  if username <> '' then
  begin
    if multinick = username then
      ShowMessage(Format(RS_MULTINAME, [username]))
    else
      ShowMessage(Format(RS_MULTINAME_NAMED, [multinick, username]));
  end
  else
  if multinick <> RS_DEFAULT_ACCOUNT then
    ShowMessage(Format(RS_MULTINAME_DEF_NAMED, [multinick]))
  else
    ShowMessage(RS_MULTINAME_DEF);
end;

// Handle off range panel click
procedure TfBG.pnOffRangeClick(Sender: TObject);
var
  ishigh: boolean;
begin
  ishigh := (Sender as TPanel).Color = bg_rel_color_hi;
  {$ifdef TrndiExt}
  if not funcBool('uxClick',[
    'range',
    ishigh
    ], true) then
    Exit;
  {$endif}

  ShowMessage(Format(RS_RANGE_EXPLANATION, [IfThen(ishigh, RS_OVER, RS_UNDER)]));
end;

