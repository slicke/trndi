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
// Main form helper functions for trend dots and UI initialization

// Apply a procedure to all trend points; also provides an index
procedure TfBG.actOnTrend(proc: TTrendProcLoop);
var
  ix: integer;
  ls: array[1..10] of TDotControl;
begin
  ls := TrendDots; // Directly use the TrendDots array
  for ix := 1 to NUM_DOTS do
  begin
    proc(ls[ix], NUM_DOTS, ix, ls);
    ls[ix].Repaint;
  end;
  // Run the procedure on the given label
end;

// Apply a procedure to all trend points
procedure TfBG.actOnTrend(proc: TTrendProc);
var
  ix: integer;
  ls: array[1..10] of TDotControl;
begin
  ls := TrendDots; // Directly use the TrendDots array
  for ix := 1 to NUM_DOTS do
  begin
    proc(ls[ix], NUM_DOTS, ix);
    ls[ix].Repaint;
  end;
end;

procedure tfBG.SetLang;
var
  lang: string;
  langPath: string;
  moPath: string;
  langLoaded: string;
begin
  lang := native.GetSetting('locale', '');
  if (lang = 'auto') or (lang = '') then
    lang := native.GetOSLanguage;
  applocale := lang;
  Application.ProcessMessages;

  langPath := getLangPath;
  moPath := langPath + 'Trndi.' + lang + '.mo';
  LogMessage('Lang select: setting=' + native.GetSetting('locale', '') + ' resolved=' + lang + ' path=' + langPath + ' mo=' + moPath);
  langLoaded := SetDefaultLang(lang, langPath, 'Trndi');
  LogMessage('Lang loaded=' + langLoaded + ' RS_INIT_UI=' + RS_INIT_UI);
  if not FileExists(moPath) then
    LogMessage('Lang MO missing: ' + lang + ' path=' + langPath + ' mo=' + moPath);
end;

{$if DEFINED(DARWIN) OR DEFINED(X_LINUXBSD)}
procedure TfBG.InitializePlatformMenus;
var
  MainMenu: TMainMenu;
  AppMenu,
  forceMenu,
  SettingsMenu,
  HelpMenu,
  GithubMenu: TMenuItem;
begin
  MainMenu := TMainMenu.Create(self);
  fBg.Menu := MainMenu;
  {$ifdef X_MAC}
  MacAppDelegate := TMyAppDelegate.alloc.init;
  NSApp.setDelegate(NSObject(MacAppDelegate));

  Application.Title := 'Trndi';
  {$ENDIF}
  AppMenu := TMenuItem.Create(Self); // Application menu

  {$ifdef X_MAC}
  AppMenu.Caption := #$EF#$A3#$BF;   // Unicode Apple logo char
  {$else}
  AppMenu.Caption := RS_MENU_NAME;   // Unicode Apple logo char
  {$endif}
  MainMenu.Items.Insert(0, AppMenu);

  SettingsMenu := TMenuitem.Create(self);
  settingsmenu.Caption := RS_MENU_SETTINGS;
  settingsmenu.OnClick := misettings.OnClick;
  AppMenu.Insert(0, SettingsMenu);

  forcemenu := TMenuItem.Create(self);
  forcemenu.Caption := RS_MENU_FORCE;
  forcemenu.onclick := miForce.OnClick;
  AppMenu.Insert(1, forceMenu);

  helpmenu := TMenuItem.Create(self);
  helpmenu.Caption := RS_MENU_HELP;
  MainMenu.Items.Insert(1, helpMenu);

  upmenu := TMenuItem.Create(self);
  upmenu.Caption := mirefresh.Caption;
  upmenu.Enabled := false;

  githubmenu := TMenuItem.Create(self);
  githubmenu.Caption := RS_TRNDI_GIHUB;
  githubmenu.onclick := @onGH;
  helpMenu.Insert(0, githubMenu);

  helpMenu.Insert(0, upMenu);
end;
{$endif}

procedure TfBG.InitializeUIComponents;
var
  i: integer;
  fontName: string;
begin
  // Set dots
  DOT_GRAPH := native.GetWideCharSetting('font.dot', WChar($2B24));
  DOT_FRESH := native.GetWideCharSetting('font.dot_fresh', WChar($2600));

  // Load fonts
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

  // Sensitive data
  DATA_FRESHNESS_THRESHOLD_MINUTES :=
    native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);

  tir_icon := native.GetBoolSetting('range.tir_icon', false);

  // Create trend dots dynamically (TLabel on Darwin, TPaintBox elsewhere)
  CreateTrendDots;

  // Check touch screen
  HasTouch := native.HasTouchScreen(HasMultiTouch);
  if HasMultiTouch then
    touchHelper := TTouchDetector.Create;

  miATouch.Checked := hastouch;

  actOnTrend(@initDot);

  //--- Colors
  bg_color_ok := native.GetColorSetting('ux.bg_color_ok', bg_color_ok);
  bg_color_hi := native.GetColorSetting('ux.bg_color_hi', bg_color_hi);
  bg_color_lo := native.GetColorSetting('ux.bg_color_lo', bg_color_lo);

  bg_color_ok_txt := native.GetColorSetting('ux.bg_color_ok_txt', bg_color_ok_txt);
  bg_color_hi_txt := native.GetColorSetting('ux.bg_color_hi_txt', bg_color_hi_txt);
  bg_color_lo_txt := native.GetColorSetting('ux.bg_color_lo_txt', bg_color_lo_txt);

  bg_rel_color_hi := native.GetColorSetting('ux.bg_rel_color_hi', bg_rel_color_hi);
  bg_rel_color_lo := native.GetColorSetting('ux.bg_rel_color_lo', bg_rel_color_lo);

  bg_rel_color_lo_txt := native.GetColorSetting('ux.bg_rel_color_lo_txt',
    bg_rel_color_lo_txt);
  bg_rel_color_hi_txt := native.GetColorSetting('ux.bg_rel_color_hi_txt',
    bg_rel_color_hi_txt);

  // Color title bar (on Windows?)
  titlecolor := native.GetBoolSetting('ux.title_color', true);

  if native.GetBoolSetting('ux.tir_color_on') then
    tir_bg := native.GetColorSetting('ux.tir_color');
  if native.GetBoolSetting('ux.tir_color_custom_on') then
    tir_custom_bg := native.GetColorSetting('ux.tir_color_custom');

  // Clear labels at startup (they'll be populated when readings are available)
  lRef.Caption := '';
  lDiff.Caption := '';
end;

procedure TfBG.InitializeSplashScreen;
begin
  fSplash := TfSplash.Create(nil);
  if IsProblematicWM then // It might hide dialogs behind the splash screen
    if not IsSemiProblematicWM then // Gnome etc kind of works
      with fsplash do
      begin
        fSplash.Image1.Hide;
        lSplashWarn.hide;
        fSplash.incProgress(1, 'Trndi is loading...');
        linfo.top := 0;
        linfo.left := 400;
        Height := linfo.canvas.TextHeight('Pq') + 5;

        lTrndi.AutoSize := true;
        fSplash.incProgress(5, 'Trndi | You need to accept the license agreement! ');
        lTrndi.top := 0;
        lTrndi.left := 0;
        lTrndi.font := linfo.font;
        lTrndi.font.color := clWhite;
        Application.ProcessMessages;
      end;
  FStoredWindowInfo.Initialized := false;
  fSplash.Image1.Picture.Icon := Application.Icon;
  fSplash.incProgress(7, '');
  fSplash.lInfo.Font.Color := fSplash.lSplashWarn.Font.color;
  {$ifdef X_WIN}
  fSplash.ShowInTaskBar := stAlways;
  {$endif}
  {$ifdef DARWIN}
  fSplash.lSplashWarn.Font.Size := 9;
  {$endif}
  fSplash.Show;
end;

procedure TfBG.LoadUserProfile;
var
  i: integer;
  userNick: string;
  userList: TStringArray;
begin
  if native.TryGetCSVSetting('users.names', userList, true) then
  begin // We have more than 1 user
    i := ExtList(uxdAuto, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE, // Show a prompt to choose user
      RS_MULTIUSER_BOX, userList, true);

    if i > -1 then
    begin // We chose a user
      username := userList[i]; // Set username
      native.configUser := username; // Set settings main key
      userNick := native.GetSettingEx('user.nick', username); // Load the user's nickname

      fbg.Caption := Format(RS_USER_CAPTION, [userNick, fBG.Caption]); // Set the main form title
      multinick := userNick; // Set the multi user nickname
    end
    else
    begin // Standard user
      username := ''; // No username
      userNick := native.GetSettingEx('user.nick', RS_DEFAULT_ACCOUNT); // Get the standard user's nick
      multinick := userNick; // Set the multi user nickname
      fbg.Caption := Format(RS_USER_CAPTION, [userNick, fBG.Caption]); // Set the main form title
    end;

    multi := true; // Enable multi user support
    pnMultiUser.Color := native.GetColorSetting('user.color', clBlack); // Set user's color
    if pnMultiUser.Color <> clBlack then // Act if the panel is not transparent (black)
    begin
      pnMultiUser.Visible := native.GetRootSetting('users.colorbox', 'true') = 'true';
      customTitlebar := setColorMode; // Set the custom title bar value depending if the panel is showing
    end;
  end
  else
    multi := false; // No multi user
end;

procedure TfBG.CheckAndAcceptLicense;
const
  license = '⚠️ IMPORTANT MEDICAL WARNING ⚠️'#10#13 + #10 +
    'This app is NOT a medical device.'#10 +
    '• Do NOT make medical decisions based on this data'#10 +
    '• Data may be WRONG, delayed, or unavailable'#10 +
    '• Always verify with your official CGM device'#10 +
    '• For emergencies, contact medical professionals'#10 + #10 +
    'By continuing, you acknowledge that:'#10 +
    '• You use this app at your own risk'#10 +
    '• The developers have NO LIABILITY'#10 +
    '• You have read and agree to the full terms';
var
  i: integer;
begin
  if native.GetBoolSetting('license.250608') <> true then
    while i <> mrYes do
    begin
      i := ExtMsg(uxdAuto, 'License', 'You must accept the full terms conditions',
        'Do you agree to the terms and full license?', license,
        uxclWhite, uxclRed, [mbYes, mbCancel, mbUxRead], uxmtCustom, uxscLarge);
      if i = mrYes then
        native.SetSetting('license.250608', true)
      else
      if i = mrCancel then
      begin
        Application.Terminate;
        try
          Halt(1);
        finally
          halt(1);
        end;
        Exit;
      end
      else
        OpenURL('https://github.com/slicke/trndi/blob/main/LICENSE.md');
    end;
end;

function TfBG.InitializeAPI: boolean;
var
  apiTarget, apiCreds, maxmin, maxresult: string;
  minmax, resultmax: integer;
begin
  Result := true;

  if native.TryGetSetting('remote.max_min', maxmin) then
    if native.TryGetSetting('remote.max_result', maxresult) then
      if TryStrToInt(maxmin, minmax) and TryStrToInt(maxresult, resultmax) then
      begin
        MAX_MIN := minmax;
        MAX_RESULT := resultmax;
      end;

  apiTarget := native.GetSetting('remote.target');
  apiCreds := native.GetSetting('remote.creds');

  case native.GetSetting('remote.type') of
  API_NS,
  'API_NS':
    api := NightScout.Create(apiTarget, apiCreds);
  API_NS3,
  'API_NS3':
    api := NightScout3.Create(apiTarget, apiCreds);
  API_DEX_USA,
  'API_DEX_USA':
    api := DexcomUSA.Create(apiTarget, apiCreds);
  API_DEX_EU,
  'API_DEX_EU':
    api := DexcomWorld.Create(apiTarget, apiCreds);
  API_DEX_NEW_USA,
  'API_DEX_NEW_USA':
    api := DexcomUSANew.Create(apiTarget, apiCreds);
  API_DEX_NEW_EU,
  'API_DEX_NEW_EU':
    api := DexcomWorldNew.Create(apiTarget, apiCreds);
  API_XDRIP,
  'API_XDRIP':
    api := xDrip.Create(apiTarget, apiCreds);
  {$ifdef DEBUG}
  API_D_DEBUG,
  'API_D_DEBUG':
    api := DebugAPI.Create(apiTarget, apiCreds);
  API_D_MISSING,
  'API_D_MISSING':
    api := DebugMissingAPI.Create(apiTarget, apiCreds);
  API_D_PERFECT,
  'API_D_PERFECT':
    api := DebugPerfectAPI.Create(apiTarget, apiCreds);
  API_D_CUSTOM,
  'API_D_CUSTOM':
    api := DebugCustomAPI.Create(apiTarget, apiCreds);
  API_D_EDGE,
  'API_D_EDGE':
    api := DebugEdgeAPI.Create(apiTarget, apiCreds);
  API_D_FIRST,
  'API_D_FIRST':
    api := DebugFirstMissingAPI.Create(apiTarget, apiCreds);
  API_D_FIRSTX,
  'API_D_FIRSTX':
    api := DebugFirstXMissingAPI.Create(apiTarget, apiCreds);
  API_D_SECOND,
  'API_D_SECOND':
    api := DebugSecondMissingAPI.Create(apiTarget, apiCreds);
    {$endif}
  else
    Result := false;
  end;
end;

{$ifdef X_LINUXBSD}
procedure TfBG.InitializeLinuxPlatform;
var
  distroId, fontName: string;
  hasDefaultFont: boolean;
begin
  hasDefaultFont := FontTXTInList(fontName);

  if not hasDefaultFont then
  begin
    distroId := scanLinuxDistro(['fedora','ubuntu','debian']);
    case distroId of
    'fedora':
      fontName := 'Poppins';
    'ubuntu':
      fontName := 'Sans';
    else
      fontName := 'Sans';
    end;
  end;

  IsRaspberry := false;
  if distroId = 'debian' then
    IsRaspberry := FileExists('/etc/rpi-issue');

  fBG.Font.Name := fontName;

  {$ifndef LCLQt6}
  case ExtText(uxdAuto, 'This version may not work as intended!',
      'This release of Trndi was compiled for a non-supported platform ("widgetset")'#10#10+
      'Performance might be bad, graphics might not work correctly and features might not work as intended!'#10+
      'Please download the official release (Qt6) from github.com/slicke/trndi'#10#10+
      'Continue Anyway?',
      [MbYes, mbNo, mbUXRead],uxmtWarning) of
  mrNo:
    Application.Terminate;
  mrYes:
    Application.ProcessMessages;
  else
    OpenURL('https://github.com/slicke/trndi/blob/main/doc/Widgetsets.md');
  end;
  {$endif}
  isWSL := TrndiNative.DetectWSL.IsWSL;
  if isWSL then
    Showmessage('Windows Linux Subsystem (WSL) detected. Due to limitations in WSL, graphic issues may occur. Commonly, windows will appear at random positions an not where expected!');
end;
{$endif}

{$ifdef DEBUG}
procedure TfBG.miDebugUXMsgClick(Sender: TObject);
var
  old: TTrndiBool;
begin
  old := TrndiNative.touchOverride;
  TrndiNative.touchOverride := TTrndiBool.tbTrue;
  UXMessage(uxdOnForm,'Test Message','This is a debug test',uxmtInformation,self);
  TrndiNative.touchOverride := old;
end;

procedure TfBG.miDebugLogClick(Sender: TObject);
begin
  TrndiDebugLogAlert := not TrndiDebugLogAlert;
end;

procedure TfBG.miDebugLoadTextClick(Sender: TObject);
begin
  debug_load_text := not debug_load_text;
  ShowMessage(Format('Now %s', [IfThen(debug_load_text, 'showing', 'not showing')]));
end;

{$endif}

// Initialize the TrendDots array in FormCreate
procedure TfBG.FormCreate(Sender: TObject);
procedure haltBoot;
  begin
    firstboot := true;
    fSplash.Hide;
    fSplash.Free;
    tMain.Enabled := false;
    lArrow.Caption := '';
    lVal.Caption := RS_SETUP;
    lVal.Cursor := crHandPoint;
    bSettings.Show;
  end;

var
  i: integer;
  guifontName, txtfontName, apiTarget: string;
  fil: boolean;
  userlocale: TFormatSettings;
  minutesStr: string;
  minutesVal: integer;
  {$ifdef DEBUG}
  miDebugUXMsg, miDebugLog, miDebugLoadText: TMenuItem;
  {$endif}
begin
  firstboot := false;

  // Initialize native interface
  native := TrndiNative.Create;
  if (ssMeta in GetKeyShiftState) and ExtMsgYesNo(uxdNormal,'Run Trndi in English?', 'Run Trndi in English this once? You pressed the Windows/meta key, and can now choose to run Trndi in English. You can change the language in Settings.') then
  else
    SetLang;

  // Initialize splash screen first
  InitializeSplashScreen;
  Application.ProcessMessages;
  Application.OnException := @AppExceptionHandler;

  // Start media backend early
  fSplash.incProgress(10, RS_INIT_MEDIA);
  MediaController := TSystemMediaController.Create(Self);
  MediaController.Initialize;
  Application.ProcessMessages;

  fSplash.incProgress(15, RS_INIT_FONTS);
  // Font validation
  fil := FontGUIInList(guifontName);
  if not fil then
    ShowMessage(Format(RS_FONT_ERROR, [guifontName]));

  fil := FontTxtInList(txtfontName);
  if not fil then
    ShowMessage(Format(RS_FONT_ERROR, [txtfontName]));

  // Platform-specific menu setup
  {$ifdef DARWIN}
  InitializePlatformMenus;
  {$endif}
  {$ifdef X_LINUXBSD}
  if TrndiNative.HasGlobalMenu then
    InitializePlatformMenus;
  {$endif}

  if ssShift in GetKeyShiftState then
    if ExtMsgYesNo('Override big screen?', 'Shift Pressed! Force Trndi to treat your system as ' + IfThen(native.DetectTouchScreen(fil), 'Small screen (non-touch)', 'Big screen (touch)')+'?', uxmtWarning) then
      if native.DetectTouchScreen(fil) then
        native.touchOverride := tbFalse
      else
        native.touchOverride := tbTrue;

  setColorMode;

  // Platform-specific initialization
  {$ifdef X_LINUXBSD}
  fSplash.incProgress(20, RS_INIT_LINUX);
  InitializeLinuxPlatform;
  {$endif}

  // Set border style
  {$ifdef DARWIN}
  BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}

  Application.ProcessMessages;

  // Set fonts for non-Darwin platforms
  {$ifndef DARWIN}
  fSplash.incProgress(25, RS_INIT_MAC);
  fBG.font.Name := txtFontName;
  lArrow.Font.Name := guifontName;
  {$endif}


  fSplash.incProgress(30, RS_INIT_UI);
  // Initialize UI components
  InitializeUIComponents;
  Application.ProcessMessages;

  // Force first UI update by initializing cached UI state to sentinel values
  // This ensures ShouldUpdateUI(...) returns true on first valid update
  FLastUIColor := TColor(-1);
  FLastTirColor := TColor(-1);
  FLastUICaption := '<uninitialized>';
  FLastTir := '<uninitialized>';

  // Configuration and user setup
  with native do
  begin
    fSplash.incProgress(35, RS_INIT_PROFILES);
    LoadUserProfile;

    fSplash.incProgress(40, RS_INIT_LOCALE);
    // Locale setup
    userlocale := DefaultFormatSettings;
    userlocale.DecimalSeparator := GetCharSetting('locale.separator', '.');
    badge_adjust := GetIntSetting('ux.badge_size', 0) / 10;
    native.locale := userlocale;
    semiTouchMode := GetBoolSetting('ux.semi_touch', false);

    fSplash.incProgress(45, RS_INIT_LICENSE);
    // License check
    CheckAndAcceptLicense;
    Application.ProcessMessages;

    fSplash.incProgress(50, RS_INIT_UNIT);
    // Privacy and unit settings
    privacyMode := GetBoolSetting('ext.privacy');
    if CheckSetting('unit', 'mmol', 'mmol') then
      un := BGUnit.mmol
    else
      un := BGUnit.mgdl;


    // API setup and validation
    apiTarget := GetSetting('remote.target');
    if apiTarget = '' then
    begin
      fSplash.incProgress(55, RS_INIT_INITIAL);
      tMain.Enabled := false;
      for i := 0 to fBG.ComponentCount - 1 do
        if (fbg.Components[i] is TLabel) and (fbg.Components[i] <> lval) then
          (fbg.Components[i] as TLabel).Caption := '';
      miSettings.Click;
      ShowMessage(RS_FORCE_QUIT_SETUP);
      Application.Terminate;
      Exit;
    end;

    fSplash.incProgress(60, RS_INIT_API);
    Application.ProcessMessages;
    if not InitializeAPI then
    begin
      haltBoot;
      Exit;
    end;

    fSplash.incProgress(65, RS_INIT_CONNECT);
    Application.ProcessMessages;
    if not api.Connect then
    begin
      haltBoot;
      ShowMessage(api.ErrorMsg);
      miSettings.Click;
      // Ensure the application exits so the user can restart with new settings
      Application.Terminate;
      Exit;
    end;

    fSplash.incProgress(70, RS_INIT_UX);
    // UI preferences
    {$ifdef DARWIN}
    dotscale := GetFloatSetting('ux.dot_scale', 0.7);
    {$else}
    dotscale := GetFloatSetting('ux.dot_scale', 1);
    {$endif}
    DOT_ADJUST := GetFloatSetting('ux.dot_adjust', 0);

    DOT_OFFSET_RANGE := GetIntSetting('ux.dot_offset_range', DOT_OFFSET_RANGE);
    DELTA_MAX := GetIntSetting('ux.delta_max', DELTA_MAX);
    // DOT_VISUAL_OFFSET := CalculateDotVisualOffset;  // No longer needed - centering uses half dot height
    miRangeColor.Checked := GetBoolSetting('ux.range_color', true);
    PaintRange := native.GetBoolSetting('ux.paint_range', true);
    PaintRangeLines := native.GetBoolSetting('ux.paint_range_lines', false);
    PaintRangeCGMRange := native.GetBoolSetting('ux.paint_range_cgmrange', false);
    // Extensions
    {$ifdef TrndiExt}
    fSplash.incProgress(80, RS_SPLASH_LOADING_INIT);
    LoadExtensions;
    {$endif}

    fSplash.incProgress(90, RS_INIT_CUSTOM_OVERRIDES);
    // Override settings - always apply if set, regardless of checkbox state
    // This allows extensions and manual config edits to work
    api.cgmLo := GetIntSetting('override.lo', api.cgmLo);
    api.cgmHi := GetIntSetting('override.hi', api.cgmHi);
    api.cgmRangeLo := GetIntSetting('override.rangelo', api.cgmRangeLo);
    api.cgmRangeHi := GetIntSetting('override.rangehi', api.cgmRangeHi);

    PredictGlucoseReading := GetBoolSetting('predictions.enable', false);
    PredictShortMode := GetBoolSetting('predictions.short', false);
    PredictShortFullArrows := GetBoolSetting('predictions.short.fullarrows', false);
    PredictShortShowValue := GetBoolSetting('predictions.short.showvalue', false);
    // Prefer user-scoped minutes only; do not fall back to global
    minutesStr := '';
    if not TryGetSetting('predictions.short.minutes', minutesStr) then
      minutesStr := '';
    minutesVal := StrToIntDef(minutesStr, 10);
    if minutesVal in [5, 10, 15] then
      PredictShortMinutes := minutesVal
    else
      PredictShortMinutes := 10;
    // Read short mode size setting (small or big). Sanitize to 'small' otherwise.
    PredictShortSize := native.GetIntSetting('predictions.short.size', 1);
    if PredictShortSize < 1 then
      PredictShortSize := 1
    else
    if PredictShortSize > 3 then
      PredictShortSize := 3;
    lPredict.Visible := PredictGlucoseReading;
  end;

  // Final initialization and first reading
  Application.ProcessMessages;
  if not updateReading(true) then
  begin
    fSplash.incProgress(95, RS_INIT_FIRST);
    updateReading; // Second attempt for setup
    showWarningPanel(RS_NO_BOOT_READING);
  end;

  Chroma := TRazerChromaFactory.CreateInstance;
  if native.GetBoolSetting('razer.enabled', false) then
  begin
    fSplash.incProgress(96, RS_INIT_CHROMA);
    if not Chroma.Initialize then
      Showmessage('❌ Failed to initialize Razer Chroma: ', Chroma.LastError);
  end;

  // Cleanup splash screen
  fSplash.Close;
  fSplash.Free;
  tmain.Enabled := true;
  tsetup.Enabled := true;
  
  // Initialize web server fields
  FWebServer := nil;
  tWebServerStart := nil;
  
  // Start web server with delayed timer (after all initialization is complete)
  if native.GetBoolSetting('webserver.enable', false) then
  begin
    tWebServerStart := TTimer.Create(Self);
    tWebServerStart.Interval := 2000; // 2 second delay
    tWebServerStart.OnTimer := @tWebServerStartTimer;
    tWebServerStart.Enabled := true;
  end;

  {$ifndef TrndiExt}
  miExtLog.Enabled := false;
  {$endif}
  {$ifdef DEBUG}
  TrndiDebugLogAlert := false;
  miDebugUXMSg := TMenuItem.Create(miAdvanced);
  miDebugUXMSg.Caption := '[debug] Trigger UX message';
  miAdvanced.Insert(miAdvanced.Count, miDebugUXMSg);
  miDebugUXMsg.OnClick := @miDebugUXMsgClick;

  miDebugLog := TMenuItem.Create(miAdvanced);
  miDebugLog.Caption := '[debug] Toggle alert messages';
  miAdvanced.Insert(miAdvanced.Count, miDebugLog);
  miDebugLog.OnClick := @miDebugLogClick;

  miDebugLoadText := TMenuItem.Create(miAdvanced);
  miDebugLoadText.Caption := '[debug] Show load indication';
  miAdvanced.Insert(miAdvanced.count, miDebugLoadText);
  miDebugLoadText.onClick := @miDebugLoadTextClick;
  {$endif}
end;

// Web Server Methods
procedure TfBG.tWebServerStartTimer(Sender: TObject);
begin
  // Disable the timer after first execution
  if Assigned(tWebServerStart) then
  begin
    tWebServerStart.Enabled := false;
    FreeAndNil(tWebServerStart);
  end;
  
  // Start the web server
  StartWebServer;
end;

procedure TfBG.StartWebServer;
var
  Port: integer;
  Token: string;
begin
  if Assigned(FWebServer) then
    Exit; // Already running
    
  Port := native.GetIntSetting('webserver.port', 8080);
  Token := native.GetSetting('webserver.token', '');
  
  FWebServer := TTrndiWebServer.Create(Port, Token, @GetCurrentReadingForWeb, @GetPredictionsForWeb);
  TTrndiWebServer(FWebServer).Start;
end;

procedure TfBG.StopWebServer;
begin
  if Assigned(FWebServer) then
  begin
    TTrndiWebServer(FWebServer).Stop;
    FreeAndNil(FWebServer);
  end;
end;

function TfBG.GetCurrentReadingForWeb: BGResults;
begin
  // Return the cached current reading if available
  if Length(FCachedReadings) > 0 then
    Result := FCachedReadings
  else
    result := []// Return an empty reading if no data is available - Init will set curr to BG_NO_VAL
  ;
end;

function TfBG.GetPredictionsForWeb: BGResults;
begin
  // Return empty array - predictions should be cached separately if needed
  // We cannot make API calls from the web server thread as they would block
  SetLength(Result, 0);
end;

function TfBG.WebServerActive: boolean;
begin
  Result := Assigned(FWebServer) and TTrndiWebServer(FWebServer).Active;
end;
