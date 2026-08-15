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
  // Startup calls this twice in multi-user mode: once before the account is
  // known (so the picker itself is localized) and once after, since 'locale'
  // is a per-user setting. Reloading the same catalog is wasted work, so the
  // second pass only costs anything when the account switches language.
  if (applocale <> '') and (lang = applocale) then
    Exit;
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
  {$ifdef DARWIN}
  { Modification notice (2026-08-05): use the native Command modifier for
    macOS menu shortcuts instead of the Ctrl-based shortcuts loaded from LFM. }
  miSettings.ShortCut := ShortCut(Ord('S'), [ssMeta]);
  miHistory.ShortCut := ShortCut(Ord('H'), [ssMeta]);
  miFullScreen.ShortCut := ShortCut(Ord('F'), [ssMeta]);
  miFloatOn.ShortCut := ShortCut(Ord('W'), [ssMeta]);
  miExit.ShortCut := ShortCut(Ord('Q'), [ssMeta]);
  {$endif}

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

  SettingsMenu := TMenuItem.Create(self);
  SettingsMenu.Caption := RS_MENU_SETTINGS;
  SettingsMenu.OnClick := miSettings.OnClick;
  SettingsMenu.ShortCut := miSettings.ShortCut;
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

  // How the trend dots derive their color from the range colors above. Read
  // once and cached: DotPaint runs per dot on every paint pass.
  DotColorMode := DotColorModeFromSetting(
    native.GetIntSetting('ux.dot_color_mode', Ord(DOT_COLOR_MODE_DEFAULT)));

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
  i, preselect: integer;
  userNick, lastUser: string;
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
    // Open on the account chosen last time rather than on whichever name sorts
    // first: users.active is written on every start, so the common case (same
    // person, same machine) is a single click. An account removed since then
    // simply isn't found and the picker opens on the first entry.
    lastUser := native.GetRootSetting('users.active', '');
    preselect := 0;
    if lastUser <> '' then
      for i := 0 to High(userList) do
        if SameText(userList[i], lastUser) then
        begin
          preselect := i;
          Break;
        end;

    SlickeDialogsInTaskbar := true;
    i := SlickeList(sdsAuto, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE, // Show a prompt to choose user
      RS_MULTIUSER_BOX, userList, true, uxmtCog, preselect);
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
      // The drawn Wayland bar does not exist yet at this point; its identity
      // chip takes over (and drops the prefix) from RefreshUserBadge.
      if not native.SupportsUserBadge then
        fbg.Caption := Format(RS_USER_CAPTION, [userNick, FBaseCaption]); // Set the main form title
    end
    else
    begin // Standard user
      username := ''; // No username
      native.SetRootSetting('users.active', username);
      userNick := native.GetSettingEx('user.nick', RS_DEFAULT_ACCOUNT); // Get the standard user's nick
      multinick := userNick; // Set the multi user nickname
      // See above: the badge platforms carry the name in the title-bar badge.
      if not native.SupportsUserBadge then
        fbg.Caption := Format(RS_USER_CAPTION, [userNick, FBaseCaption]); // Set the main form title
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
    '• Alerts may be delayed or never arrive — Trndi is NOT an alarm'#10 +
    '• Always verify with your official CGM device'#10 +
    '• For emergencies, contact medical professionals'#10 + #10 +
    'By continuing, you acknowledge that:'#10 +
    '• You use this app at your own risk'#10 +
    '• The developers have NO LIABILITY'#10 +
    '• You have read and agree to the full terms'#10 + #10 +
    // The dialog asks the user to accept "the full license", so name it: Trndi
    // is GPLv3, and the warnings above are its GPL section 7 additional terms.
    'Trndi is free software under the GNU General Public License v3,'#10 +
    'with the medical disclaimer above as an additional term.';
  // Every license key we have ever shipped, newest last. Add the outgoing key
  // here whenever the current one is bumped, so a returning user is told the
  // terms changed instead of being shown a first-run gate again.
  priorKeys: array[0..0] of string = ('license.250608');
var
  i, k: integer;
  termsChanged: boolean;
  title, question: string;
begin
  // The key is dated: bump it whenever the terms above change materially, so
  // everyone re-accepts what they are actually agreeing to. 260804 added the
  // warning that alerts are not an alarm system (see DISCLAIMER.md).
  if native.GetBoolSetting('license.260804') <> true then
  begin
    termsChanged := false;
    for k := low(priorKeys) to high(priorKeys) do
      if native.GetBoolSetting(priorKeys[k]) then
        termsChanged := true;

    if termsChanged then
    begin
      // The GPLv3 text itself never changes, so a re-prompt can only mean the
      // medical disclaimer (the GPL section 7 additional term) moved.
      title := 'Trndi''s medical disclaimer has been updated';
      question := 'You accepted an earlier version. Do you agree to the updated terms?';
    end
    else
    begin
      title := 'You must accept the full terms conditions';
      question := 'Do you agree to the terms and full license?';
    end;

    i := 0;
    while i <> mrYes do
    begin
      i := SlickeMsg(sdsAuto, 'License', title, question, license,
        uxclWhite, uxclRed,
        // Cancel quits the app, so it never sits between the two continue actions
        [[mbYes, mbSlickeRead, mbCancel], [mbSlickeRead, mbCancel, mbYes]],
        uxmtCustom, uxscLarge);
      if i = mrYes then
        native.SetSetting('license.260804', true)
      else
      if i = mrCancel then
      begin
        Application.Terminate;
        Halt(1);
      end
      else
        // "Read" on a medical-warning gate wants the disclaimer, which states
        // its GPL section 7 relationship and links the license text itself.
        OpenURL('https://github.com/slicke/trndi/blob/main/DISCLAIMER.md');
    end;
  end;
end;

function TfBG.InitializeAPI: boolean;
var
  apiTarget, apiCreds, maxmin, maxresult, remoteType: string;
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
  remoteType := native.GetSetting('remote.type');
  api := CreateBackend(remoteType, apiTarget, apiCreds);
  if api = nil then
  begin
    Result := false;
    // The caller drops to the "Setup" screen, which on its own is
    // indistinguishable from a fresh install — name the missing backend so the
    // user knows their configuration is intact and what to do about it.
    if remoteType <> '' then
    begin
      if Assigned(fSplash) then
        fSplash.Hide;
      {$ifdef DEBUG}
      // This build *is* a test build, so "only exists in test builds" would
      // contradict itself — Settings lists the debug backends. An unresolvable
      // API_D_* value here means that entry was renamed or removed, which is
      // exactly what the generic message says.
      ShowMessage(Format(RS_BACKEND_GONE_BOOT, [remoteType]));
      {$else}
      if IsDebugBackend(remoteType) then
        ShowMessage(Format(RS_BACKEND_DEBUG_GONE_BOOT, [remoteType]))
      else
        ShowMessage(Format(RS_BACKEND_GONE_BOOT, [remoteType]));
      {$endif}
    end;
  end;

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
  case SlickePrompt(sdsAuto, 'This version may not work as intended!',
      'This release of Trndi was compiled for a non-supported platform ("widgetset")'#10#10+
      'Performance might be bad, graphics might not work correctly and features might not work as intended!'#10+
      'Please download the official release (Qt6) from github.com/slicke/trndi'#10#10+
      'Continue Anyway?',
      [MbYes, mbNo, mbSlickeRead],uxmtWarning) of
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
  SlickeMessage(sdsOnForm,'Test Message','This is a debug test',uxmtInformation,self);
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
  fil, englishOnce: boolean;
  userlocale: TFormatSettings;
  minutesStr: string;
  minutesVal, paramIx: integer;
  fWizard: TfWizard;
  {$ifdef DEBUG}
  miDebugUXMsg, miDebugLog, miDebugLoadText, miDebugLogAPI: TMenuItem;
  {$endif}
begin
  TrndiDLog(Format(#10'=== Trndi started at %s ===', [DateTimeToStr(Now)]));
  firstboot := false;

  // Caption as designed. The multi-user name prefix is added to and removed
  // from *this* — never to an already-prefixed caption — as the title-bar
  // badge appears or disappears (see RefreshUserBadge).
  FBaseCaption := Caption;

  // --kiosk: dedicated-display mode (RPi wall display, bedside screen).
  // FormShow schedules the actual activation: fullscreen, keep-awake, and no
  // update popup. Everything else (setup wizard, menus) behaves as normal so
  // a kiosk can still be configured on first boot. Parsed by hand rather
  // than via Application.HasOption so the test build's Forms mock suffices.
  FKioskMode := false;
  for paramIx := 1 to ParamCount do
    if ParamStr(paramIx) = '--kiosk' then
      FKioskMode := true;
  if FKioskMode then
    TrndiDLog('Kiosk mode requested via --kiosk');

  InitCriticalSection(FReadingsLock);

  // Initialize native interface
  native := TrndiNative.Create;

  englishOnce := (ssMeta in GetKeyShiftState) and SlickeMsgYesNo(sdsNormal,'Run Trndi in English?', 'Run Trndi in English this once? You pressed the Windows/meta key, and can now choose to run Trndi in English. You can change the language in Settings.');
  if not englishOnce then
    SetLang;

    // Establish active user scope as early as possible so startup settings
  // (language/media/etc.) resolve for the selected user.
  LoadUserProfile;

  // 'locale' is stored per user, but the SetLang above has to run before the
  // account is known - it has to, or the picker itself would be untranslated.
  // So it resolved the *default* account's language. Re-apply now that
  // native.configUser is set, or a secondary account never gets the language
  // it saved. No-ops when both accounts use the same language, and is skipped
  // entirely when the meta-key override asked for English this once.
  if multi and (not englishOnce) then
    SetLang;

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
    if SlickeMsgYesNo('Override big screen?', 'Shift Pressed! Force Trndi to treat your system as ' + IfThen(native.DetectTouchScreen(fil), 'Small screen (non-touch)', 'Big screen (touch)')+'?', uxmtWarning) then
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
  {$if DEFINED(X_LINUXBSD)}
  // Not the emoji font here: the trend glyphs are plain arrows (U+2191..U+2198)
  // and the emoji fonts only cover the two diagonals, so ↗/↘ came out in the
  // emoji face while ↑/↓/→ fell through to whatever fontconfig substituted --
  // different weight per trend, and Font.Color only reaching half of them.
  // DejaVu covers the whole arrow block in one style and is on every desktop.
  if Screen.Fonts.IndexOf('DejaVu Sans') >= 0 then
    lArrow.Font.Name := 'DejaVu Sans'
  else
    lArrow.Font.Name := txtFontName;
  {$else}
  lArrow.Font.Name := guifontName;
  {$endif}
  {$endif}


  fSplash.incProgress(30, RS_INIT_UI);
  // Initialize UI components
  InitializeUIComponents;
  Application.ProcessMessages;

  // Application-drawn title bar (Wayland / forced via ux.own_titlebar). After
  // the font setup above so the bar sizes itself from the real UI font.
  ApplyOwnTitleBar;

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
        // Same drawn-title-bar treatment as Settings on Wayland sessions.
        DressModalForm(fWizard);
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

    // Reservoir ladder latch: restoring it keeps a restart on a low cartridge
    // from repeating a step that was already announced.
    FReservoirStep := native.GetIntSetting('alerts.reservoir.step', 0);
    // Same for the sensor-expiry ladder, so a restart four hours from the end
    // of a session does not repeat the step it already announced.
    FSensorExpiryStep := native.GetIntSetting('alerts.sensor.step', 0);
    // And for the battery ladder, so a restart on a nearly flat pump does not
    // repeat the step it already announced.
    FPumpBatteryStep := native.GetIntSetting('alerts.battery.step', 0);

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
    ShowBolusOverlay := GetBoolSetting('display.bolus_overlay', false);
    ShowAutoBolusOverlay := GetBoolSetting('display.bolus_overlay.auto', false);
    ShowCarbOverlay := GetBoolSetting('display.carb_overlay', false);
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
    // Hidden in dot mode — the predictions are drawn on the trend instead.
    lPredict.Visible := PredictGlucoseReading and not PredictDotMode;
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
