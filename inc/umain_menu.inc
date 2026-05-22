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

procedure TfBG.miDotNormalDrawItem(Sender: TObject; ACanvas: TCanvas;
ARect: TRect; AState: TOwnerDrawState);
begin
end;

procedure TfBG.miDotNormalMeasureItem(Sender: TObject; ACanvas: TCanvas;
var AWidth, AHeight: integer);
begin
end;

procedure TfBG.miDotsInViewClick(Sender: TObject);
var
  i: integer;
begin
  i := dotsInView;
  ShowMessage(IfThen(i = 0, 'Yes', 'Offset: ' + i.toString));
end;

procedure TfBG.miDotSmallClick(Sender: TObject);
begin
end;

procedure TfBG.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfBG.miCustomDotsClick(Sender: TObject);
var
  mr: TModalResult;
  dots: single;
begin
  dots := ExtNumericInput(uxdAuto,sDotSize,sCustomiseDotSize, sEnterDotSize,dotscale,FLOAT_NONE, FLOAT_NONE, true,mr);
  if mr = mrOk then
  begin
    native.SetFloatSetting('ux.dot_scale', dots);
    dotscale := dots;
  end;
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

    if ExtMsg(uxdAuto, sChangesSave, sChangesRemember, sChangesApply,
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
  readings: BGResults;
  res: string;
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
    readings := api.getReadings(minutesSince, maxReadings, '', res);

    if Length(readings) = 0 then
    begin
      if res <> '' then
        ShowMessage(Format(RS_DATE_PICKER_NO_READINGS_ERR, [res]))
      else
        ShowMessage(RS_DATE_PICKER_NO_READINGS);
      Exit;
    end;

    ShowHistoryGraph(readings, un, CurrentHistoryGraphPalette,
      api.cgmHi, api.cgmLo, api.cgmRangeHi, api.cgmRangeLo);
    AutoEnableBasalOverlay;
  end;
end;

procedure TfBG.pmSettingsClose(Sender: TObject);
begin
  last_popup := now;
end;

procedure TfBG.pnWarningClick(Sender: TObject);
var
  bg: BGReading;
  detail: string;
begin
  {$ifdef TrndiExt}
  if not funcBool('uxClick',
    ['no-reading'], true) then
    Exit;
  {$endif}
  detail := '';
  if FWarnMessage <> '' then
    detail := FWarnMessage + LineEnding + LineEnding;
  if tryLastReading(bg) then
    detail := detail + Format(RS_LAST_RECIEVE, [bg.format(un, BG_MSG_SHORT),
      FormatDateTime('hh:nn', bg.date)])
  else
    detail := detail + RS_LAST_RECIEVE_NO;
  ShowMessage(detail);
end;

procedure TfBG.lAgoClick(Sender: TObject);
var
  displayMsg, dm: string;
  s: system.utf8char;
  i: integer;
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
    displayMsg += sHTMLLineBreak + Format(sRSSI, [i]);
  if lastReading.TryGetNoise(i) then
    displayMsg += sHTMLLineBreak + Format(sNoise, [i]);

  displayMsg += sHTMLLineBreak + Format(sDevice, [lastReading.sensor]);

  displayMsg += sHTMLLineBreak + Format(sAPI, [api.systemName]);

  ExtHTML(uxdAuto, sTransmitterInfo, displayMsg, [mbOK], uxmtInformation, 10);
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

procedure TfBG.miAlternateClick(Sender: TObject);
begin
  miAlternate.Checked := not miAlternate.Checked;
  tSwap.Enabled := miAlternate.Checked;
end;

procedure TfBG.miClockClick(Sender: TObject);
begin
  miClock.Checked := not miClock.Checked;
  tClock.Enabled := miClock.Checked;
  native.SetSetting('main.clock', tClock.Enabled);
end;

procedure TfBG.miDotNormalClick(Sender: TObject);
var
  fmt: TFormatSettings;
begin
  fmt.DecimalSeparator := '.';
  dotscale := StrToFloat((Sender as TMenuItem).Hint, fmt);
  native.SetFloatSetting('ux.dot_scale', dotscale);
  FormResize(fBG);
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
var
  res: string;
  readings: BGResults;
begin
  if api = nil then
  begin
    ShowMessage('API not initialized yet.');
    Exit;
  end;

  readings := api.getReadings(1440, 288, '', res);

  if Length(readings) = 0 then
  begin
    if res <> '' then
      ShowMessage('No readings returned: ' + res)
    else
      ShowMessage('No readings returned for the last 24 hours.');
    Exit;
  end;

  ShowHistoryGraph(readings, un, CurrentHistoryGraphPalette,
    api.cgmHi, api.cgmLo, api.cgmRangeHi, api.cgmRangeLo);
  AutoEnableBasalOverlay;
  AutoAddPredictionOverlay;
end;

procedure TfBG.miHistoryClick(Sender: TObject);
begin
  ShowHistoryGraph(bgs, un, CurrentHistoryGraphPalette,
    api.cgmHi, api.cgmLo, api.cgmRangeHi, api.cgmRangeLo);
  AutoEnableBasalOverlay;
  AutoAddPredictionOverlay;
end;

procedure TfBG.miRangeColorClick(Sender: TObject);
begin
  miRangeColor.Checked := not miRangeColor.Checked;
  miForce.Click;
  native.SetSetting('ux.range_color', miRangeColor.Checked);
end;

procedure TfBG.miBordersClick(Sender: TObject);
begin
  {$ifdef LCLGTK3}
  Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
  Exit;
  {$endif}
  miBorders.Checked := not miBorders.Checked;
  if miBorders.Checked then
    self.BorderStyle := bsNone
  else
  {$ifdef DARWIN}
    BorderStyle := bsSizeable;
    {$else}
    BorderStyle := bsSizeToolWin;
  {$endif}

  setColorMode;
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
    msg := Format(sForceRefreshCached, [secondsSinceLastCall, waitTime]);

    Result := slicke.UX.alert.ExtMsg(uxdAuto, sRefrshQ, msg,
      sForceRefreshDetail, '', uxclWhite, uxclRed, [mbRetry, mbCancel],
      uxmtInformation);

    if Result = mrRetry then
      if FetchAndValidateReadingsForced then
        CompleteUIUpdate;
  end
  else
    updateReading;
end;

procedure TfBG.miLimitExplainClick(Sender: TObject);
begin
  ShowMessage(RS_LIMIT_EXPLAIN_TEXT);
end;

procedure TfBG.miOnTopClick(Sender: TObject);
{$ifdef LCLQt6}
var
  QtWidget: TQtWidget;
  flags: nativeuint;
  sessionType: string;
const
  Qt_WindowStaysOnTopHint = $00008000;
{$endif}
begin
  miOnTop.Checked := not miOnTop.Checked;
  if miOnTop.Checked then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;

  {$ifdef LCLQt6}
  sessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');
  if LowerCase(sessionType) = 'wayland' then
  begin
    TrndiDLog('OnTop requested but session is Wayland; compositor may ignore programmatic hints.');
    if miOnTop.Checked then
      ShowMessage(RS_WAYLAND);
  end;
  try
    if HandleAllocated then
    begin
      QtWidget := TQtWidget(Handle);
      if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
      begin
        flags := QtWidget.windowFlags;
        if miOnTop.Checked then
        begin
          QtWidget.setWindowFlags(flags or Qt_WindowStaysOnTopHint);
          Self.Visible := true;
          Self.Show;
          Self.BringToFront;
          Self.SetFocus;
          try
            Application.BringToFront;
          except end;
          if (Left < -1000) or (Top < -1000) or (Left > Screen.Width) or (Top > Screen.Height) then
          begin
            Width := Min(Width, Screen.Width - 40);
            Height := Min(Height, Screen.Height - 40);
            Left := (Screen.Width - Width) div 2;
            Top := (Screen.Height - Height) div 2;
            Application.ProcessMessages;
          end;
        end
        else
          QtWidget.setWindowFlags(flags and (not Qt_WindowStaysOnTopHint));
      end;
    end;
  except
  end;
  {$endif}

  setColorMode;
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
    miDotVal.Caption := Format(sReadingHere, [tpb.hint, H, M]);
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

