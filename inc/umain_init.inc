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

// Apply a procedure to all active trend dots; also provides count and index
procedure TfBG.actOnTrend(proc: TTrendProcLoop);
var
  ix: integer;
begin
  if Length(TrendDots) <= ACTIVE_DOTS then Exit;
  for ix := 1 to ACTIVE_DOTS do
  begin
    proc(TrendDots[ix], ACTIVE_DOTS, ix, TrendDots);
    // Skip Repaint when the dot isn't on screen — early-init paths (e.g.
    // applying initDot before the first reading) leave dots hidden, so the
    // paint would be discarded anyway.
    if TrendDots[ix].Visible then
      TrendDots[ix].Repaint;
  end;
end;

// Apply a procedure to all active trend dots
procedure TfBG.actOnTrend(proc: TTrendProc);
var
  ix: integer;
begin
  if Length(TrendDots) <= ACTIVE_DOTS then Exit;
  for ix := 1 to ACTIVE_DOTS do
  begin
    proc(TrendDots[ix], ACTIVE_DOTS, ix);
    if TrendDots[ix].Visible then
      TrendDots[ix].Repaint;
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
  TrndiDLog('Lang select: setting=' + native.GetSetting('locale', '') + ' resolved=' + lang + ' path=' + langPath + ' mo=' + moPath);
  langLoaded := SetDefaultLang(lang, langPath, 'Trndi');
  TrndiDLog('Lang loaded=' + langLoaded + ' RS_INIT_UI=' + RS_INIT_UI);
  if not FileExists(moPath) then
    TrndiDLog('Lang MO missing: ' + lang + ' path=' + langPath + ' mo=' + moPath);
end;

{$if DEFINED(DARWIN) OR DEFINED(X_LINUXBSD)}
procedure TfBG.InitializePlatformMenus;
var
  MainMenu: TMainMenu;
  AppMenu,
  forceMenu,
  SettingsMenu,
  HelpMenu,
  GithubMenu,
  AboutMenu,
  CheckUpdatesMenu: TMenuItem;
begin
  {$ifdef X_MAC}
  MacAppDelegate := TMyAppDelegate.alloc.init;
  NSApp.setDelegate(NSObject(MacAppDelegate));

  Application.Title := 'Trndi';
  {$ENDIF}

  // Only create and populate a `TMainMenu` when a desktop-global menu exists.
  // When no global menu is present, skip creating the main menu entirely
  // so no in-window menubar is shown.
  if native.HasGlobalMenu then
  begin
    MainMenu := TMainMenu.Create(self);
    fBg.Menu := MainMenu;

    AppMenu := TMenuItem.Create(Self); // Application menu

    {$ifdef X_MAC}
    AppMenu.Caption := #$EF#$A3#$BF;   // Unicode Apple logo char
    {$else}
    AppMenu.Caption := RS_MENU_NAME;   // Unicode Apple logo char
    {$endif}
    MainMenu.Items.Insert(0, AppMenu);


  {$ifdef DARWIN}
  // Add About menu item (standard on macOS)
  AboutMenu := TMenuItem.Create(self);
  AboutMenu.Caption := 'About Trndi';
  AboutMenu.OnClick := @ShowAboutDialog;
  AppMenu.Insert(0, AboutMenu);

  // Add separator
  AppMenu.Insert(1, TMenuItem.Create(self));
  AppMenu.Items[1].Caption := '-';
  {$endif}

  SettingsMenu := TMenuitem.Create(self);
  settingsmenu.Caption := RS_MENU_SETTINGS;
  settingsmenu.OnClick := misettings.OnClick;
  {$ifdef DARWIN}
  AppMenu.Insert(2, SettingsMenu);
  {$else}
  AppMenu.Insert(0, SettingsMenu);
  {$endif}

  forcemenu := TMenuItem.Create(self);
  forcemenu.Caption := RS_MENU_FORCE;
  forcemenu.onclick := miForce.OnClick;
  {$ifdef DARWIN}
  AppMenu.Insert(3, forceMenu);
  {$else}
  AppMenu.Insert(1, forceMenu);
  {$endif}

  {$ifdef DARWIN}
  // Add Check for Updates menu item
  CheckUpdatesMenu := TMenuItem.Create(self);
  CheckUpdatesMenu.Caption := 'Check for Updates...';
  CheckUpdatesMenu.OnClick := @CheckForUpdatesMenuClick;
  AppMenu.Insert(4, CheckUpdatesMenu);

  // On macOS, change "Exit Trndi" to "Quit Trndi" for consistency
  miExit.Caption := 'Quit Trndi';
  miExit.ShortCut := 16465; // Cmd+Q
  {$endif}

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
end;
{$endif}

procedure TfBG.InitializeUIComponents;
var
  i: integer;
  fontName: string;
begin
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

  // Read dot count before CreateTrendDots so the right number of slots is allocated
  ACTIVE_DOTS := native.GetIntSetting('ux.dot_count', NUM_DOTS);
  if ACTIVE_DOTS < 1 then ACTIVE_DOTS := 1;
  if ACTIVE_DOTS > MAX_DOT_COUNT then ACTIVE_DOTS := MAX_DOT_COUNT;

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

  if native.GetBoolSetting('main.high_contrast', false) then begin
    bg_color_ok := clBlack;
    bg_color_ok_txt := clWhite;
    // Hi
    bg_color_hi := clBlack;
    bg_color_hi_txt := clWhite;
    // Low
    bg_color_lo := clWhite;
    bg_color_lo_txt := clBlack;

    bg_rel_color_hi := clBlack;
    bg_rel_color_lo := clWhite;
    bg_rel_color_hi_txt := clWHite;
    bg_rel_color_lo_txt := clBlack;
  end;

  // Color title bar (on Windows?)
  titlecolor := native.GetBoolSetting('ux.title_color', true);

  if native.GetBoolSetting('ux.tir_color_on') then
    tir_bg := native.GetColorSetting('ux.tir_color');
  if native.GetBoolSetting('ux.tir_color_custom_on') then
    tir_custom_bg := native.GetColorSetting('ux.tir_color_custom');

  // Clear labels at startup (they'll be populated when readings are available)
  lRef.Caption := '';
  lDiff.Caption := '';

  // Make connection badge interactive for quick diagnostics.
  if not Assigned(FInternetBadgeBg) then
  begin
    FInternetBadgeBg := TShape.Create(Self);
    FInternetBadgeBg.Parent := Self;
    FInternetBadgeBg.Shape := stRoundRect;
    FInternetBadgeBg.Pen.Style := psClear;
    FInternetBadgeBg.Visible := false;
  end;

  lInternet.OnClick := @lInternetClick;
  lInternet.Cursor := crHandPoint;
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
//  fSplash.Image1.Picture.Icon := Application.Icon;
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
  sortHelper: TStringList;
begin
  if native.TryGetCSVSetting('users.names', userList, true) then
  begin // We have more than 1 user
    // Sort case-insensitively so the picker scans predictably regardless of
    // the insertion order stored in users.names.
    if Length(userList) > 1 then
    begin
      sortHelper := TStringList.Create;
      try
        sortHelper.CaseSensitive := false;
        sortHelper.AddStrings(userList);
        sortHelper.Sort;
        userList := sortHelper.ToStringArray;
      finally
        sortHelper.Free;
      end;
    end;
    SlickeDialogsInTaskbar := true;
    i := ExtList(uxdAuto, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE, // Show a prompt to choose user
      RS_MULTIUSER_BOX, userList, true);
    SlickeDialogsInTaskbar := false;

    if i > -1 then
    begin // We chose a user
      username := userList[i]; // Set username
      native.configUser := username; // Set settings main key
      native.SetRootSetting('users.active', username);
      userNick := native.GetSettingEx('user.nick', username); // Load the user's nickname

      multinick := userNick; // Set the multi user nickname
      // On platforms with a native title-bar badge (Windows) the name rides in
      // that badge — shown from FormShow — so skip the caption-text prefix.
      if not native.SupportsUserBadge then
        fbg.Caption := Format(RS_USER_CAPTION, [userNick, fBG.Caption]); // Set the main form title
    end
    else
    begin // Standard user
      username := ''; // No username
      native.SetRootSetting('users.active', username);
      userNick := native.GetSettingEx('user.nick', RS_DEFAULT_ACCOUNT); // Get the standard user's nick
      multinick := userNick; // Set the multi user nickname
      // See above: the badge platforms carry the name in the title-bar badge.
      if not native.SupportsUserBadge then
        fbg.Caption := Format(RS_USER_CAPTION, [userNick, fBG.Caption]); // Set the main form title
    end;

    multi := true; // Enable multi user support
    pnMultiUser.Color := native.GetColorSetting('user.color', clBlack); // Set user's color
    if pnMultiUser.Color <> clBlack then // Act if the panel is not transparent (black)
    begin
      pnMultiUser.Visible := native.GetRootSetting('users.colorbox', 'true') = 'true';
      userColorBar := pnMultiUser.Visible;
      // setColorMode hides pnMultiUser again when the title bar takes the
      // user's colour — the bar would be redundant next to a coloured title.
      customTitlebar := setColorMode; // Set the custom title bar value depending if the panel is showing
    end;
  end
  else
  begin
    native.SetRootSetting('users.active', '');
    multi := false; // No multi user
  end;
end;

procedure TfBG.CheckAndAcceptLicense;
const
  license = '⚠️ IMPORTANT MEDICAL WARNING ⚠️'#10 + #10 +
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
  begin
    i := 0;
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
        Halt(1);
      end
      else
        OpenURL('https://github.com/slicke/trndi/blob/main/LICENSE.md');
    end;
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

  // The registry resolves both stable codes ('API_NS') and the display names
  // legacy settings stored, and instantiates the matching backend class.
  api := CreateBackend(native.GetSetting('remote.type'), apiTarget, apiCreds);
  if api = nil then
    Result := false;

  // Scale MAX_RESULT to the API's reporting frequency so all dot slots can be
  // filled even for backends that upload more often than every 5 minutes.
  // Formula: 25 × ceil(INTERVAL_MINUTES / reportingInterval)
  //   5-min backend → 25 × 1 = 25
  //   1-min backend → 25 × 5 = 125
  if Assigned(api) then
    MAX_RESULT := Max(MAX_RESULT,
      25 * Max(1, (INTERVAL_MINUTES + api.getReportingInterval - 1)
                   div api.getReportingInterval));
end;

{$ifdef X_LINUXBSD}
procedure TfBG.InitializeLinuxPlatform;
var
  distroId, fontName: string;
  hasDefaultFont: boolean;
begin
  hasDefaultFont := FontTXTInList(fontName);
  distroId := scanLinuxDistro(['fedora','ubuntu','debian']);

  if not hasDefaultFont then
  begin
    case distroId of
    'fedora':
      fontName := 'Poppins';
    else
      fontName := 'Sans';
    end;
  end;

  IsRaspberry := (distroId = 'debian') and FileExists('/etc/rpi-issue');

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
procedure TfBG.miDebugUXMsgClick({%H-}Sender: TObject);
var
  old: TTrndiBool;
begin
  old := TrndiNative.touchOverride;
  TrndiNative.touchOverride := TTrndiBool.tbTrue;
  UXMessage(uxdOnForm,'Test Message','This is a debug test',uxmtInformation,self);
  TrndiNative.touchOverride := old;
end;

procedure TfBG.miDebugLogClick({%H-}Sender: TObject);
begin
  TRNDI_DEBUG_LOG_ALERT := not TRNDI_DEBUG_LOG_ALERT;
end;

procedure TfBG.miDebugLoadTextClick(Sender: TObject);
begin
  debug_load_text := not debug_load_text;
  ShowMessage(Format('Now %s', [IfThen(debug_load_text, 'showing', 'not showing')]));
end;

procedure TfBG.miDebugLogAPIClick(Sender: TObject);
begin
  DEBUG_LOG_ALERT := true;
  (sender as TMenuItem).Enabled := false;
end;

{$endif}

// Initialize the TrendDots array in FormCreate
procedure TfBG.FormCreate({%H-}Sender: TObject);
procedure haltBoot;
  begin
    firstboot := true;
    fSplash.Hide;
    FreeAndNil(fSplash);
    tMain.Enabled := false;
    lArrow.Caption := '';
    lVal.Caption := RS_SETUP;
    lVal.Cursor := crHandPoint;
    bSettings.Show;
  end;

var
  guifontName, txtfontName, apiTarget, prevSetup: string;
  fil: boolean;
  userlocale: TFormatSettings;
  minutesStr: string;
  minutesVal: integer;
  fWizard: TfWizard;
  {$ifdef DEBUG}
  miDebugUXMsg, miDebugLog, miDebugLoadText, miDebugLogAPI: TMenuItem;
  {$endif}
begin
  TrndiDLog(Format(#10'=== Trndi started at %s ===', [DateTimeToStr(Now)]));
  firstboot := false;

  InitCriticalSection(FReadingsLock);

  // Initialize native interface
  native := TrndiNative.Create;

  if (ssMeta in GetKeyShiftState) and ExtMsgYesNo(uxdNormal,'Run Trndi in English?', 'Run Trndi in English this once? You pressed the Windows/meta key, and can now choose to run Trndi in English. You can change the language in Settings.') then
  else
    SetLang;

    // Establish active user scope as early as possible so startup settings
  // (language/media/etc.) resolve for the selected user.
  LoadUserProfile;

  // Initialize splash screen first
  InitializeSplashScreen;
  fSplash.incProgress(10, RS_INIT_TRNDI);
  Application.ProcessMessages;
  Application.OnException := @AppExceptionHandler;

  // Start media backend early
  if not native.GetBoolSetting('media.disabled', false) then begin
    fSplash.incProgress(10, RS_INIT_MEDIA);
    MediaController := TSystemMediaController.Create(Self);
    MediaController.Initialize;
  end;
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
  FLastConnectionDetail := '';

  // Configuration and user setup
  with native do
  begin
    fSplash.incProgress(35, RS_INIT_PROFILES);
    // Already loaded before SetLang/media init to ensure proper user scope.

    fSplash.incProgress(40, RS_INIT_LOCALE);
    // Locale setup
    userlocale := DefaultFormatSettings;
    userlocale.DecimalSeparator := GetCharSetting('locale.separator', '.');

    locale := userlocale;
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
      fSplash.Hide;

      fWizard := TfWizard.Create(nil, native);
      try
        if fWizard.ShowModal = mrOK then
        begin
          // Wizard saved settings — re-read target and continue init
          apiTarget := GetSetting('remote.target');

          // Re-apply dot count: dots were created with default 10 above,
          // before the wizard ran, so recreate if the user picked a different count.
          ACTIVE_DOTS := GetIntSetting('ux.dot_count', ACTIVE_DOTS);
          if ACTIVE_DOTS < 1 then ACTIVE_DOTS := 1;
          if ACTIVE_DOTS > MAX_DOT_COUNT then ACTIVE_DOTS := MAX_DOT_COUNT;
          if MAX_MIN < ACTIVE_DOTS * INTERVAL_MINUTES + 10 then
            MAX_MIN := ACTIVE_DOTS * INTERVAL_MINUTES + 10;
          // MAX_RESULT is a reading count, not minutes. api isn't created yet here;
          // InitializeAPI will scale this up by the reporting interval below.
          if MAX_RESULT < ACTIVE_DOTS then
            MAX_RESULT := ACTIVE_DOTS;
          FreeTrendDotControls;
          CreateTrendDots;

          fSplash.Show;
        end
        else
        begin
          // User cancelled — close app without showing the main window
          fSplash.Free;
          fSplash := nil;
          firstboot := true;
          FCloseAfterFormCreate := true;
          Application.ShowMainForm := false;
          Application.Terminate;
          Exit;
        end;
      finally
        fWizard.Free;
      end;
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

    FAlertEngine := TAlertEngine.Create;
    // Persistence-before-fire (minutes) and hysteresis (mg/dL) are user-tunable;
    // defaults filter out single-sample spikes and threshold bounce without
    // delaying urgent-low alerts.
    FAlertEngine.SetupRule(akHigh,       true,  api.cgmHi,  30, 0,
      native.GetIntSetting('alerts.duration.high', 10),
      native.GetFloatSetting('alerts.hysteresis.high', 5));
    FAlertEngine.SetupRule(akLow,        true,  api.cgmLo,  15, 0,
      native.GetIntSetting('alerts.duration.low', 5),
      native.GetFloatSetting('alerts.hysteresis.low', 5));
    FAlertEngine.SetupRule(akUrgentLow,  true,  api.limitLO, 5, 5,
      native.GetIntSetting('alerts.duration.urgent', 0),
      native.GetFloatSetting('alerts.hysteresis.urgent', 5));
    FAlertEngine.SetupRule(akMissing,
      GetBoolSetting('alerts.notice.missing', true), 0, 15);
    FAlertEngine.SetupRule(akSensorFault, true, 0, 30);
    FAlertEngine.SetupRule(akRapidFall,  false, 0, 0);
    FAlertEngine.SetupRule(akRapidRise,  false, 0, 0);

    FAlertEngine.DeserializeState(native.GetSetting('alerts.state', ''));
    FAlertEngine.OnStateChanged := @PersistAlertState;

    api.APIEmitter := @APIReceiver;
    api.OnCredentialsChanged := @APICredentialsChanged;
    // A failed Connect at boot (e.g. an expired CareLink token) lands the
    // user directly in Settings; the main window is never shown. New backend
    // settings are retried in place, closing Settings unchanged quits.
    while not api.Connect do
    begin
      fSplash.Hide;
      ShowMessage(api.ErrorMsg);

      prevSetup := GetSetting('remote.type') + '|' + GetSetting('remote.target') +
        '|' + GetSetting('remote.creds');
      firstboot := true; // Settings saves silently and leaves fBG hidden
      miSettings.Click;
      firstboot := false;

      if GetSetting('remote.type') + '|' + GetSetting('remote.target') +
        '|' + GetSetting('remote.creds') = prevSetup then
      begin
        firstboot := true; // Ensure quit doesn't ask for confirmation
        FCloseAfterFormCreate := true;
        Application.ShowMainForm := false;
        Application.Terminate;
        FreeAndNil(fSplash);
        Exit;
      end;

      FreeAndNil(api);
      if not InitializeAPI then
      begin
        haltBoot;
        Exit;
      end;
      api.APIEmitter := @APIReceiver;
      api.OnCredentialsChanged := @APICredentialsChanged;
      fSplash.Show;
    end;

    fSplash.incProgress(70, RS_INIT_UX);
    // UI preferences
    DOT_ADJUST := GetFloatSetting('ux.dot_adjust', 0);

    // Ensure MAX_MIN and MAX_RESULT cover the full dot window.
    // ACTIVE_DOTS was read before CreateTrendDots; the API-based MAX_RESULT
    // scaling earlier used a default dot count — bump it for higher counts.
    // MAX_RESULT is a reading count; mirror InitializeAPI's reporting-interval
    // scaling but with ACTIVE_DOTS as the base.
    if MAX_MIN < ACTIVE_DOTS * INTERVAL_MINUTES + 10 then
      MAX_MIN := ACTIVE_DOTS * INTERVAL_MINUTES + 10;
    MAX_RESULT := Max(MAX_RESULT,
      ACTIVE_DOTS * Max(1, (INTERVAL_MINUTES + api.getReportingInterval - 1)
                   div api.getReportingInterval));

    DIFF_ALIGN := native.GetAlignmentSetting('ux.labels.ldiff.alignment', taCenter);

    DOT_OFFSET_RANGE := GetIntSetting('ux.dot_offset_range', DOT_OFFSET_RANGE);
    DELTA_MAX := GetIntSetting('ux.delta_max', DELTA_MAX);
    UseRangeColor := GetBoolSetting('ux.range_color', true);
    PaintRange := GetBoolSetting('ux.paint_range', true);
    PaintRangeLines := GetBoolSetting('ux.paint_range_lines', false);
    PaintRangeCGMRange := GetBoolSetting('ux.paint_range_cgmrange', false);
    // Extensions
    {$ifdef TrndiExt}
    fSplash.incProgress(80, RS_SPLASH_LOADING_INIT);
    LoadExtensions;
    {$endif}

    // Apply wizard-suggested thresholds for APIs that don't supply their own
    // (cgmHi stays at initCGMCore's sentinel 401 when the API never set it)
    if api.cgmHi = 401 then
    begin
      api.cgmHi := GetIntSetting('wizard.hi', api.cgmHi);
      api.cgmLo := GetIntSetting('wizard.lo', api.cgmLo);
    end;

    fSplash.incProgress(85, RS_INIT_CUSTOM_OVERRIDES);
    // Override settings - always apply if set, regardless of checkbox state
    // This allows extensions and manual config edits to work
    api.cgmLo := GetIntSetting('override.lo', api.cgmLo);
    api.cgmHi := GetIntSetting('override.hi', api.cgmHi);
    api.cgmRangeLo := GetIntSetting('override.rangelo', api.cgmRangeLo);
    api.cgmRangeHi := GetIntSetting('override.rangehi', api.cgmRangeHi);

    PredictGlucoseReading := GetBoolSetting('predictions.enable', false);
    PredictShortMode := GetBoolSetting('predictions.short', false);
    PredictDotMode := GetBoolSetting('predictions.dots', false);
    RotatingArrow := GetBoolSetting('display.rotating_arrow', false);
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
    PredictShortSize := GetIntSetting('predictions.short.size', 1);
    if PredictShortSize < 1 then
      PredictShortSize := 1
    else
    if PredictShortSize > 3 then
      PredictShortSize := 3;
    lPredict.Visible := PredictGlucoseReading;
  end;

  // Final initialization and first reading. The fetch is dispatched to a
  // background TGlucoseFetchThread so the splash isn't held by the network
  // round-trip; the worker's ApplyResult runs the post-fetch UI flow once
  // results land. The legacy "two quick attempts" semantics are now inside
  // the worker (Execute retries once when boot+empty).
  Application.ProcessMessages;
  fSplash.incProgress(90, RS_INIT_FETCH);

  // Neutralize the visual chrome that would otherwise leak through while we
  // wait. FormPaint also short-circuits range-band drawing while
  // FBootFetchPending is true so the threshold lines don't appear over a
  // value we don't have yet.
  // The form's resize handler autosizes lVal to fill most of the height;
  // a short single glyph reads as "loading" without being mistaken for a
  // numeric reading, and won't get rotated/clipped like a sentence would.
  // tBootSpinner animates the glyph through a braille-dot cycle so the
  // window looks alive while the first network round-trip is in flight.
  FBootSpinnerFrame := 0;
  lVal.Caption := '⠋';
  lVal.Font.Style := lVal.Font.Style - [fsStrikeOut];
  lArrow.Caption := '';
  lDiff.Caption := '';
  lRef.Caption := '';
  lAgo.Caption := RS_INIT_FETCH;
  lTir.Caption := '';
  if Assigned(pnOffReading) then
    pnOffReading.Visible := false;
  if Assigned(pnOffRange) then
    pnOffRange.Visible := false;
  if Assigned(pnOffRangeBar) then
    pnOffRangeBar.Visible := false;
  if Assigned(pnNextProgress) then
    pnNextProgress.Visible := false;
  fBG.Color := clBlack;
  if Assigned(lPredict) then
    lPredict.Visible := false;

  // The actual fetch is kicked off via a one-shot timer (tBootFetch) so the
  // worker thread is created only after Application.Run is pumping the
  // message loop. Spawning a TThread that immediately blocks on a long Sleep
  // from inside FormCreate can leave the form's WM_PAINT queued indefinitely
  // — symptoms include "icon in taskbar but window never draws". Letting the
  // message loop run first guarantees the form is painted and interactive
  // before any threading machinery starts.
  //
  // Note: Enabled stays false here. The explicit Application.ProcessMessages
  // further down (and any implicit pumping from fSplash.Close / Self.Show)
  // would otherwise dispatch this 50 ms WM_TIMER while FormCreate is still
  // mid-setup. Arming happens at the very end of FormCreate.
  FBootFetchPending := true;
  tBootFetch := TTimer.Create(Self);
  tBootFetch.Interval := 50;
  tBootFetch.OnTimer := @tBootFetchTimer;
  tBootFetch.Enabled := false;

  tBootSpinner := TTimer.Create(Self);
  tBootSpinner.Interval := 100;
  tBootSpinner.OnTimer := @tBootSpinnerTimer;
  tBootSpinner.Enabled := true;

  Chroma := TRazerChromaFactory.CreateInstance;
  if native.GetBoolSetting('razer.enabled', false) then
  begin
    fSplash.incProgress(96, RS_INIT_CHROMA);
    if not Chroma.Initialize then
      Showmessage('❌ Failed to initialize Razer Chroma: ', Chroma.LastError);
  end;

  // Cleanup splash screen. Nil after Free so later `if Assigned(fSplash)`
  // checks (e.g., LoadExtensions during a reload) don't dereference a dangling
  // pointer and crash inside TControl.GetText.
  fSplash.Close;
  FreeAndNil(fSplash);
  
  // Bring main window to front after splash closes
  Application.BringToFront;
  Self.Show;
  Self.BringToFront;
  Self.SetFocus;

  // Flush WM_SHOWWINDOW/WM_PAINT now so the form actually renders before we
  // hand control to whatever long-running init steps remain. Without this,
  // a slow worker that calls Synchronize before the message loop is properly
  // pumping can leave the window in the taskbar but unpainted.
  Application.ProcessMessages;

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

  // Add quick alert snooze controls to the popup menu.
  miAlertSnooze := TMenuItem.Create(pmSettings);
  miAlertSnooze.Caption := RS_ALERT_SNOOZE;

  miAlertSnooze15 := TMenuItem.Create(miAlertSnooze);
  miAlertSnooze15.Caption := '15 min';
  miAlertSnooze15.OnClick := @miAlertSnooze15Click;
  miAlertSnooze.Add(miAlertSnooze15);

  miAlertSnooze30 := TMenuItem.Create(miAlertSnooze);
  miAlertSnooze30.Caption := '30 min';
  miAlertSnooze30.OnClick := @miAlertSnooze30Click;
  miAlertSnooze.Add(miAlertSnooze30);

  miAlertSnooze60 := TMenuItem.Create(miAlertSnooze);
  miAlertSnooze60.Caption := '60 min';
  miAlertSnooze60.OnClick := @miAlertSnooze60Click;
  miAlertSnooze.Add(miAlertSnooze60);

  miAlertSnooze.Add(TMenuItem.Create(miAlertSnooze));
  miAlertSnooze.Items[miAlertSnooze.Count - 1].Caption := '-';

  miAlertSnoozeOff := TMenuItem.Create(miAlertSnooze);
  miAlertSnoozeOff.Caption := RS_ALERT_SNOOZE_OFF;
  miAlertSnoozeOff.OnClick := @miAlertSnoozeOffClick;
  miAlertSnooze.Add(miAlertSnoozeOff);

  if miExit <> nil then
    pmSettings.Items.Insert(miExit.MenuIndex, miAlertSnooze)
  else
    pmSettings.Items.Add(miAlertSnooze);

  UpdateAlertSnoozeMenu;

  {$ifdef Windows}
  // On Windows the native menu theme ignores the system dark mode; opt in
  // to our owner-draw handler so popup items render with dark colors that
  // match the rest of the UI when the user is in dark mode.
  if TrndiNative.isDarkMode then
  begin
    pmSettings.OnDrawItem := @pmSettingsDrawItem;
    // Opt the process into Windows' dark popup-menu / scrollbar theme so the
    // menu's window frame matches the items we owner-draw inside it.
    TrndiNative.SetPreferredDarkMode;
  end;
  {$endif}

  {$ifdef DEBUG}
  TRNDI_DEBUG_LOG_ALERT := false;
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

  miDebugLogAPI := TMenuItem.Create(miAdvanced);
  miDebugLogAPI.Caption := '[debug] Log API calls';
  miAdvanced.Insert(miAdvanced.count, miDebugLogAPI);
  miDebugLogAPI.onClick := @miDebugLogAPIClick;
  {$endif}

  // Arm the boot-fetch timer last. From this point there is no further
  // Application.ProcessMessages in FormCreate, so tBootFetchTimer cannot
  // fire until FormCreate returns and Application.Run starts pumping —
  // i.e. the form is fully constructed before any worker thread spawns.
  if Assigned(tBootFetch) then
    tBootFetch.Enabled := true;
end;

// Web Server Methods
procedure TfBG.tWebServerStartTimer({%H-}Sender: TObject);
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
    Result := nil; // ensure managed result is initialized
  // Return the cached current reading if available
    EnterCriticalSection(FReadingsLock);
  try
    if Length(FCachedReadings) > 0 then
    begin
      SetLength(Result, Length(FCachedReadings));
      Move(FCachedReadings[0], Result[0], Length(FCachedReadings) * SizeOf(BGReading));
    end
    else
      Result := [];// Return an empty reading if no data is available - Init will set curr to BG_NO_VAL
  finally
      LeaveCriticalSection(FReadingsLock);
  end;
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
