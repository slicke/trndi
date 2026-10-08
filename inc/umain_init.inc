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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-09-20: Added NotifyExtensionsAlert, the extension-side twin of
 *   WebPublishAlert.
 * - 2026-09-29: Added WebCommand (and WebSettingsToJSON/WebSnoozeJSON), the
 *   main-thread command handler behind the web dashboard's /settings and
 *   /snooze; StartWebServer hands it to the server.
 * - 2026-10-06: GetPredictionsForWeb returns the forecast WebPublishPredictions
 *   last cached, so /predict is no longer always empty.
 * - 2026-10-07: WebHistoryPNG hides the graph's interaction hint, which
 *   only makes sense in the live window.
 * - 2026-10-06: WebCommand serves report.get and history.png (WebReportToJSON,
 *   WebHistoryPNG) behind the web API's /report and /history.png.
 * - 2026-10-07: WebHistoryPNG takes a theme parameter (light, the default,
 *   or dark) and hands it to the graph as SetDarkTheme; the help-banner call
 *   went with the banner.
 * - 2026-10-07: WebHistoryPNG plots HeldReadings (the day-long cache merged
 *   with the live readings), and a backend change drops the cache.
 * - 2026-10-07: StartWebServer tries the ports after the configured one when
 *   it is taken (another Trndi, usually) and tells the user which port it
 *   got, or that it got none.
 * - 2026-10-07: The web server is handed the multi-user account name
 *   (UserLabel) and WebSettingsToJSON reports it as "user", so the
 *   dashboard and /health can say whose Trndi they show.
 *)
(* MODIFICATION NOTICE (2026-08-17): The high/low/urgent-low alert rules are now
   seeded after Connect and the wizard/override adjustments instead of before
   them, so they no longer inherit initCGMCore's placeholder thresholds. *)
(* MODIFICATION NOTICE (2026-08-18): The UI-state cache seeds FLastTirHint in
   place of the removed FLastTirColor, following ShouldUpdateUI's switch to the
   time-in-range hint. *)
(* MODIFICATION NOTICE (2026-08-27): actOnTrend and the per-dot init are gone
   with the per-dot controls; FormCreate now calls InitTrendModel, which
   allocates the slot arrays and the single trend paint surface. *)
(* MODIFICATION NOTICE (2026-09-17): Added ResumeBootAfterCancelledClose so a
   quit prompt declined during the boot Connect no longer strands the app with
   FBootConnectPending set and every fetch dropped. *)
(* MODIFICATION NOTICE (2026-09-10): LoadUserProfile shows the accounts in
   SlickeAccountPicker (colour dot, nickname, key) with the default account as
   the first row, in place of the plain SlickeList drop-down of stored keys. *)
// Main form helper functions for trend dots and UI initialization

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
  AboutMenu.Caption := RS_MENU_ABOUT;
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
  CheckUpdatesMenu.Caption := RS_MENU_CHECK_UPDATES;
  CheckUpdatesMenu.OnClick := @CheckForUpdatesMenuClick;
  AppMenu.Insert(4, CheckUpdatesMenu);

  // On macOS, change "Exit Trndi" to "Quit Trndi" for consistency
  miExit.Caption := RS_QUIT;
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
  // Equal-width digits for the reading (ux.tabular_digits), so the number
  // does not reflow as it ticks. Only when the streamed control really is
  // the utabularlabel interposer -- the field says so, but a reader that
  // resolved the .lfm's TLabel to the stock class would make the property
  // write land past the object.
  if lVal is TLabel then
    lVal.TabularDigits := native.GetBoolSetting('ux.tabular_digits', true);
  fontName := native.GetSetting('font.arrow', 'default');
  if fontName <> 'default' then
    lArrow.Font.Name := fontName;

  fontName := native.GetSetting('font.ago', 'default');
  if fontName <> 'default' then
  begin
    FAgoBadge.Font.Name := fontName;
    FTirBadge.Font.Name := fontName;
  end;

  // Sensitive data
  DATA_FRESHNESS_THRESHOLD_MINUTES :=
    native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);

  tir_icon := native.GetBoolSetting('range.tir_icon', false);

  // Read dot count before InitTrendModel so the right number of slots is allocated
  ACTIVE_DOTS := native.GetIntSetting('ux.dot_count', NUM_DOTS);
  if ACTIVE_DOTS < 1 then ACTIVE_DOTS := 1;
  if ACTIVE_DOTS > MAX_DOT_COUNT then ACTIVE_DOTS := MAX_DOT_COUNT;

  // Allocate the trend slot model and its paint surface
  InitTrendModel;

  // Check touch screen
  HasTouch := native.HasTouchScreen(HasMultiTouch);
  if HasMultiTouch then
    touchHelper := TTouchDetector.Create;

  miATouch.Checked := hastouch;

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
  // Optional connecting line between the dots, cached for the same reason.
  TrendLineEnabled := native.GetBoolSetting('ux.dot_line', false);
  TrendLineWidthStep :=
    EnsureRange(native.GetIntSetting('ux.dot_line_width', 2), 1, 3);
  TrendDotFade := native.GetBoolSetting('ux.dot_fade', true);

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
  {$ifdef X_WIN}
  fSplash.ShowInTaskBar := stAlways;
  {$endif}
  fSplash.Show;
end;

procedure TfBG.LoadUserProfile;
var
  i, preselect: integer;
  userNick, lastUser: string;
  userList: TStringArray;
  accounts: TSlickeAccounts;
  sortHelper: TStringList;

  // One picker row from the Accounts tab's stored nickname and colour, read
  // under that account's settings scope. The scope is reset afterwards so
  // nothing else in start-up resolves against a browsed account.
  function AccountRow(const key, fallbackNick: string): TSlickeAccount;
  begin
    native.configUser := key;
    Result.Key := key;
    Result.Nick := native.GetSettingEx('user.nick', fallbackNick);
    Result.Color := native.GetColorSetting('user.color', clBlack);
    native.configUser := '';
  end;

begin
  // --no-multi: the accounts are simply not consulted, so this is the
  // single-user start below -- default settings scope, no picker, no badge
  // or name in the title bar, plain alert titles.
  if (not FNoMultiMode) and native.TryGetCSVSetting('users.names', userList, true) then
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

    // The default account is a row like any other, first in the list, so the
    // picker has one way to answer. Its stored key is the empty scope, so the
    // row shows "Default" instead (dropped by the picker when it matches the nick).
    accounts := nil;
    SetLength(accounts, Length(userList) + 1);
    accounts[0] := AccountRow('', RS_DEFAULT_ACCOUNT);
    accounts[0].Key := RS_DEFAULT_ACCOUNT;
    for i := 0 to High(userList) do
      accounts[i + 1] := AccountRow(userList[i], userList[i]);

    // Open on the account chosen last time rather than on whichever name sorts
    // first: users.active is written on every start, so the common case (same
    // person, same machine) is a single Enter. An empty users.active is the
    // default account, i.e. the first row; an account removed since then
    // simply isn't found and the picker opens there as well.
    lastUser := native.GetRootSetting('users.active', '');
    preselect := 0;
    if lastUser <> '' then
      for i := 0 to High(userList) do
        if SameText(userList[i], lastUser) then
        begin
          preselect := i + 1;
          Break;
        end;

    SlickeDialogsInTaskbar := true;
    // The reject button keeps its "Default" caption: closing or escaping the
    // picker starts the default account, as it always has.
    i := SlickeAccountPicker(sdsAuto, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE,
      RS_MULTIUSER_BOX, accounts, preselect, uxmtCog, smbSlickeDefault);
    SlickeDialogsInTaskbar := false;

    if i > 0 then
    begin // We chose a named account
      username := userList[i - 1]; // Set username
      native.configUser := username; // Set settings main key
      native.SetRootSetting('users.active', username);
      userNick := accounts[i].Nick; // The account's nickname, read above

      multinick := userNick; // Set the multi user nickname
      // On platforms with a native title-bar badge (Windows) the name rides in
      // that badge — shown from FormShow — so skip the caption-text prefix.
      // The drawn Wayland bar does not exist yet at this point; its identity
      // chip takes over (and drops the prefix) from RefreshUserBadge.
      if not native.SupportsUserBadge then
        fbg.Caption := Format(RS_USER_CAPTION, [userNick, FBaseCaption]); // Set the main form title
    end
    else
    begin // Standard user: chosen as the first row, or by cancelling
      username := ''; // No username
      native.SetRootSetting('users.active', username);
      userNick := accounts[0].Nick; // The standard user's nick
      multinick := userNick; // Set the multi user nickname
      // See above: the badge platforms carry the name in the title-bar badge.
      if not native.SupportsUserBadge then
        fbg.Caption := Format(RS_USER_CAPTION, [userNick, FBaseCaption]); // Set the main form title
    end;

    multi := true; // Enable multi user support
    pnMultiUser.Hint := MultiUserDescription; // Hover names the account; click repeats it
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
    // A --no-multi start is not a choice: leave users.active alone so the
    // next interactive start still opens the picker on the last account.
    if not FNoMultiMode then
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
  // The stored key is 'license.' + a date; bump the date whenever the terms
  // above change materially, so everyone re-accepts what they are actually
  // agreeing to. 260804 added the warning that alerts are not an alarm
  // system (see DISCLAIMER.md).
  currentTerms = '260804';
  // Every terms date we have ever shipped, newest last. Add the outgoing date
  // here whenever the current one is bumped, so a returning user is told the
  // terms changed instead of being shown a first-run gate again.
  priorTerms: array[0..0] of string = ('250608');
var
  i: integer;
  title, question: string;

  // Every multi-user account accepts the disclaimer on its own: the key is
  // read in the account scope, so one account's acceptance never covers
  // another. (The default account is unprefixed, so its key is the root key.)
  function Accepted(const terms: string): boolean; overload;
  begin
    Result := native.GetBoolSetting('license.' + terms);
  end;

  // True when any of the given terms versions was accepted.
  function AcceptedAny(const terms: array of string): boolean;
  var
    t: string;
  begin
    Result := false;
    for t in terms do
      if Accepted(t) then
        Exit(true);
  end;

begin
  if not Accepted(currentTerms) then
  begin
    if AcceptedAny(priorTerms) then
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
        native.SetSetting('license.' + currentTerms, true)
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

  // Ensure MAX_MIN and MAX_RESULT cover the full dot window, then scale
  // MAX_RESULT to the API's reporting frequency so all dot slots can be
  // filled even for backends that upload more often than every 5 minutes.
  // Formula: max(25, ACTIVE_DOTS) × ceil(INTERVAL_MINUTES / reportingInterval)
  //   5-min backend → 25 × 1 = 25
  //   1-min backend → 25 × 5 = 125
  // getReportingInterval is a backend constant, so this needs no Connect.
  if Assigned(api) then
  begin
    if MAX_MIN < ACTIVE_DOTS * INTERVAL_MINUTES + 10 then
      MAX_MIN := ACTIVE_DOTS * INTERVAL_MINUTES + 10;
    MAX_RESULT := Max(MAX_RESULT,
      Max(25, ACTIVE_DOTS) *
      Max(1, (INTERVAL_MINUTES + api.getReportingInterval - 1)
        div api.getReportingInterval));
  end;
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

// The Setup screen replaces the reading display when no backend could be
// created at boot (haltBoot). The normal relayout path (tResizeTimer /
// ResizeUIElements) exits early without an API, so it never cleans up after
// the LFM: the off-reading strip stayed on as a letter-wrapped sliver, the
// prediction label lingered, "Setup" kept its design-time fill and the
// Settings button hid under the drawn title bar. Lay out the few controls
// the screen actually uses and hide the rest. Runs from haltBoot and again
// from tResizeTimer on every resize while the screen is up.
procedure TfBG.LayoutSetupScreen;
begin
  // Chrome that only means something with readings on screen.
  pnOffReading.Visible := false;
  pnOffRange.Visible := false;
  pnOffRangeBar.Visible := false;
  if Assigned(pbNextProgress) then
    pbNextProgress.Visible := false;
  if Assigned(FPredictStrip) then
    FPredictStrip.Visible := false;
  lArrow.Caption := '';
  lDiff.Caption := '';
  FAgoBadge.Clear;
  FTirBadge.Clear;
  lRef.Caption := '';

  // "Setup" fills the window like a reading would; lVal is client-aligned so
  // only the font needs refitting.
  ScaleLbl(lVal);

  // Keep the button clear of the application-drawn title bar (Wayland);
  // TitleBarOffset is 0 with native decorations.
  bSettings.Left := 8;
  bSettings.Top := TitleBarOffset + 8;
end;

// Initialize the TrendDots array in FormCreate
procedure TfBG.FormCreate({%H-}Sender: TObject);
var
  guifontName, txtfontName, apiTarget: string;
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
  // Extension safe mode is decided by the key state at launch, not at the
  // (now deferred) moment the extensions load.
  FSafeModeRequested := ssCtrl in GetKeyShiftState;

  // Assigned here rather than in the LFM: it pairs with the wsMinimized guard
  // in tResizeTimer, which is the only reason the event is wanted at all.
  OnWindowStateChange := @FormWindowStateChange;

  // Caption as designed. The multi-user name prefix is added to and removed
  // from *this* — never to an already-prefixed caption — as the title-bar
  // badge appears or disappears (see RefreshUserBadge).
  FBaseCaption := Caption;

  // The forecast text lives in a custom-painted strip rather than an LFM
  // label: it fits its own fonts and colours its cells by range, which a
  // TLabel can't. Hidden until settings say otherwise (below).
  FPredictStrip := TPredictionStrip.Create(Self);
  FPredictStrip.Name := 'lPredict'; // ux.labels.lpredict.scale keys off this
  FPredictStrip.Parent := Self;
  FPredictStrip.Visible := false;
  FPredictStrip.OnClick := @lPredictClick;

  // Same story for time-in-range: a caption, a mean part and a fill bar are
  // beyond a TLabel. Named after the label it replaced for the extension
  // hook's sake.
  FTirBadge := TStatBadge.Create(Self);
  FTirBadge.Name := 'lTir';
  FTirBadge.Parent := Self;
  FTirBadge.OnClick := @lTirClick;

  // And the reading age on the left, the mirror image: "3 min", or the clock
  // time over "last reading".
  // Its Font is the top band's face — the TIR badge and the fullscreen clock
  // copy it, as they copied lAgo's. No bar: an age has no share to fill.
  FAgoBadge := TStatBadge.Create(Self);
  FAgoBadge.Name := 'lAgo';
  FAgoBadge.Parent := Self;
  FAgoBadge.Alignment := taLeftJustify;
  FAgoBadge.ShowBar := false;
  FAgoBadge.PopupMenu := pmSettings;
  FAgoBadge.OnClick := @lAgoClick;
  FAgoBadge.OnDblClick := @lDiffDblClick;
  FAgoBadge.OnMouseMove := @FormMouseMove;

  // Everything that answers a click says so: a hand cursor and a hint that
  // names what the click gives. Before this only the connection badge did,
  // and the rest had to be found by trying.
  FAgoBadge.Cursor := crHandPoint;
  FAgoBadge.ShowHint := true;
  FAgoBadge.Hint := RS_HINT_AGO;
  FTirBadge.Cursor := crHandPoint;
  FTirBadge.ShowHint := true; // CalcRangeTime writes the hint
  FPredictStrip.Cursor := crHandPoint;
  FPredictStrip.ShowHint := true;
  FPredictStrip.Hint := RS_HINT_PREDICT;
  lDiff.Cursor := crHandPoint;
  lDiff.ShowHint := true;
  lDiff.Hint := RS_DIFF;
  pnOffRange.Cursor := crHandPoint;
  pnOffRange.ShowHint := true;
  pnOffRange.Hint := RS_HINT_OFF_RANGE;
  pnOffRangeBar.Cursor := crHandPoint;
  pnOffRangeBar.ShowHint := true;
  pnOffRangeBar.Hint := RS_HINT_OFF_RANGE;
  pnMultiUser.Cursor := crHandPoint;
  pnMultiUser.ShowHint := true;
  pnMultiUser.Hint := RS_HINT_MULTIUSER;

  // --kiosk: dedicated-display mode (RPi wall display, bedside screen).
  // FormShow schedules the actual activation: fullscreen, keep-awake, and no
  // update popup. Everything else (setup wizard, menus) behaves as normal so
  // a kiosk can still be configured on first boot. Parsed by hand rather
  // than via Application.HasOption so the test build's Forms mock suffices.
  // --no-multi: with several accounts configured, skip the start-up account
  // picker and run as a single-user Trndi on the default settings -- for
  // scripted launches, kiosks and development runs where a dialog nobody
  // answers would hold the start.
  FKioskMode := false;
  FNoMultiMode := false;
  for paramIx := 1 to ParamCount do
    if ParamStr(paramIx) = '--kiosk' then
      FKioskMode := true
    else
    if ParamStr(paramIx) = '--no-multi' then
      FNoMultiMode := true;
  if FKioskMode then
    TrndiDLog('Kiosk mode requested via --kiosk');
  if FNoMultiMode then
    TrndiDLog('Account picker skipped via --no-multi');

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

  // The media backend is created here but initialised on first use
  // (actOnMediaController): detection spawns one pgrep/tasklist per known
  // player, and that used to sit on the boot path for a feature that only
  // acts when an alert fires.
  if not native.GetBoolSetting('media.disabled', false) then begin
    fSplash.incProgress(10, RS_INIT_MEDIA);
    MediaController := TSystemMediaController.Create(Self);
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
  // InitializePlatformMenus gates on native.HasGlobalMenu itself.
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
  FLastTirHint := '<uninitialized>';
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
          InitTrendModel;

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
      EnterSetupScreen;
      Exit;
    end;

    fSplash.incProgress(65, RS_INIT_CONNECT);
    Application.ProcessMessages;

    FAlertEngine := TAlertEngine.Create;
    // The level rules (high/low/urgent low) are seeded further down, once
    // Connect and the wizard/override adjustments have settled the thresholds -
    // reading them here would pin every alert to initCGMCore's placeholders.
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
    // api.Connect runs on the boot worker (TGlucoseFetchThread with
    // ConnectFirst), so the window is up and painting while the backend
    // answers. ApplyConnectedApi finishes the threshold-dependent setup once
    // it succeeds; DeferredBootConnectFailure runs the "error, Settings,
    // retry or quit" flow when it does not.
    FBootConnectPending := true;

    fSplash.incProgress(70, RS_INIT_UX);
    // UI preferences
    DOT_ADJUST := GetFloatSetting('ux.dot_adjust', 0);

    DIFF_ALIGN := native.GetAlignmentSetting('ux.labels.ldiff.alignment', taCenter);

    DOT_OFFSET_RANGE := GetIntSetting('ux.dot_offset_range', DOT_OFFSET_RANGE);
    DELTA_MAX := GetIntSetting('ux.delta_max', DELTA_MAX);
    UseRangeColor := GetBoolSetting('ux.range_color', true);
    PaintRange := GetBoolSetting('ux.paint_range', true);
    PaintRangeLines := GetBoolSetting('ux.paint_range_lines', false);
    PaintRangeCGMRange := GetBoolSetting('ux.paint_range_cgmrange', false);
    // Extensions and the threshold fix-ups (wizard fallback, overrides, alert
    // seeding) wait for the backend: see ApplyConnectedApi.

    PredictGlucoseReading := GetBoolSetting('predictions.enable', false);
    PredictShortMode := GetBoolSetting('predictions.short', false);
    PredictDotMode := GetBoolSetting('predictions.dots', false);
    // Seed the boundary watcher with the current window state; the colors
    // themselves pick the dim up live in setColorMode on the first paint.
    FNightDimmed := NightDimActive;
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
    FPredictStrip.Visible := PredictGlucoseReading and not PredictDotMode;
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
  SetAgoText(RS_INIT_CONNECT, '');
  FTirBadge.Clear;
  if Assigned(pnOffReading) then
    pnOffReading.Visible := false;
  if Assigned(pnOffRange) then
    pnOffRange.Visible := false;
  if Assigned(pnOffRangeBar) then
    pnOffRangeBar.Visible := false;
  if Assigned(pbNextProgress) then
    pbNextProgress.Visible := false;
  fBG.Color := clBlack;
  if Assigned(FPredictStrip) then
    FPredictStrip.Visible := false;

  // The actual fetch is kicked off via a one-shot timer (tBootFetch) so the
  // worker thread is created only after Application.Run is pumping the
  // message loop. Spawning a TThread that immediately blocks on a long Sleep
  // from inside FormCreate can leave the form's WM_PAINT queued indefinitely
  // — symptoms include "icon in taskbar but window never draws". Letting the
  // message loop run first guarantees the form is painted and interactive
  // before any threading machinery starts.
  //
  // Note: the timer is not created here. The explicit
  // Application.ProcessMessages further down (and any implicit pumping from
  // fSplash.FadeOutAndClose / Self.Show) would otherwise dispatch its 50 ms
  // WM_TIMER while FormCreate is still mid-setup. ArmBootFetch runs at the
  // very end of FormCreate.
  FBootFetchPending := true;

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
  fSplash.FadeOutAndClose;
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

  // One alert type at a time: a sensor warm-up repeats "Outdated readings"
  // every quarter hour, and the only way to quiet it used to be muting high
  // and low along with it.
  miAlertSnooze.Add(TMenuItem.Create(miAlertSnooze));
  miAlertSnooze.Items[miAlertSnooze.Count - 1].Caption := '-';
  miSnoozeMissing := AddKindSnoozeMenu(akMissing, RS_ALERT_SNOOZE_ONLY_MISSING);
  miSnoozeHigh := AddKindSnoozeMenu(akHigh, RS_ALERT_SNOOZE_ONLY_HIGH);
  miSnoozeLow := AddKindSnoozeMenu(akLow, RS_ALERT_SNOOZE_ONLY_LOW);

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
  ArmBootFetch(50);
end;

// Main thread, from the boot worker's Synchronize, after api.Connect
// succeeded and before its getReadings. Everything here used to sit inline
// in FormCreate behind the synchronous Connect.
procedure TfBG.ApplyConnectedApi;
begin
  // Apply wizard-suggested thresholds for APIs that don't supply their own
  // (cgmHi stays at initCGMCore's sentinel 401 when the API never set it)
  if api.cgmHi = 401 then
  begin
    api.cgmHi := native.GetIntSetting('wizard.hi', api.cgmHi);
    api.cgmLo := native.GetIntSetting('wizard.lo', api.cgmLo);
  end;

  // Override settings - always apply if set, regardless of checkbox state
  // This allows extensions and manual config edits to work
  api.cgmLo := native.GetIntSetting('override.lo', api.cgmLo);
  api.cgmHi := native.GetIntSetting('override.hi', api.cgmHi);
  api.cgmRangeLo := native.GetIntSetting('override.rangelo', api.cgmRangeLo);
  api.cgmRangeHi := native.GetIntSetting('override.rangehi', api.cgmRangeHi);

  // A transient boot failure may have put the retry card up; Connect works
  // now, so stand it down before the first reading lands.
  ClearBootRetry;

  // Thresholds are final now - give the alert engine the same limits the UI
  // paints with, and the menu its real captions.
  SeedLevelAlertRules;
  UpdateApiInformation;

  SetAgoText(RS_INIT_FETCH, '');

  {$ifdef TrndiExt}
  // Extensions capture the api pointer (TJSfuncs), so they load only once the
  // backend that serves the session is settled. Queued rather than called:
  // this runs inside the worker's Synchronize and the permission prompts
  // LoadExtensions can raise are modal. The first fetchCallback comes from
  // the same worker's later ApplyResult, well after this queued call ran.
  Application.QueueAsyncCall(@DeferredLoadExtensions, 0);
  {$endif}
end;

procedure TfBG.DeferredLoadExtensions({%H-}Data: PtrInt);
begin
  if FShuttingDown then
    Exit;
  {$ifdef TrndiExt}
  LoadExtensions;
  {$endif}
end;

// Queued from ApplyConnectResult on the main thread. This is the old
// FormCreate connect loop, run once per failure with the window visible: a
// failed Connect at boot (e.g. an expired CareLink token) lands the user in
// Settings; new backend settings are retried in place, closing Settings
// unchanged quits.
procedure TfBG.DeferredBootConnectFailure({%H-}Data: PtrInt);
var
  prevSetup: string;
begin
  if FShuttingDown then
    Exit;

  // The worker has left Synchronize by now (ApplyConnectResult queued this
  // call and returned); reap it so the api it used can be replaced below.
  if Assigned(FGlucoseFetchThread) then
  begin
    SafeThreadJoin(FGlucoseFetchThread);
    FreeAndNil(FGlucoseFetchThread);
  end;

  // Wi-Fi that is not up yet at login, a DNS hiccup, a server mid-restart:
  // none of these are the user's to fix, and the dialog-Settings-quit flow
  // below closed Trndi over them. Keep the window up and try again instead.
  if IsRetryableBootError(FBootConnectError) then
  begin
    BeginBootRetry(FBootConnectError);
    Exit;
  end;

  ShowMessage(FBootConnectError);

  prevSetup := native.GetSetting('remote.type') + '|' + native.GetSetting('remote.target') +
    '|' + native.GetSetting('remote.creds');
  firstboot := true; // Settings saves silently and skips the save prompt
  miSettings.Click;
  firstboot := false;

  if native.GetSetting('remote.type') + '|' + native.GetSetting('remote.target') +
    '|' + native.GetSetting('remote.creds') = prevSetup then
  begin
    firstboot := true; // Ensure quit doesn't ask for confirmation
    Close;
    Exit;
  end;

  FreeAndNil(api);
  // The cache belongs to the backend that filled it.
  FHistoryCache := nil;
  FHistoryCacheAt := 0;
  if not InitializeAPI then
  begin
    EnterSetupScreen;
    Exit;
  end;
  api.APIEmitter := @APIReceiver;
  api.OnCredentialsChanged := @APICredentialsChanged;
  ArmBootFetch(100);
end;

// Main thread, from FormClose after the user declined the quit prompt.
// FormClose raises FShuttingDown before the (modal) prompt, and the prompt
// pumps messages: a boot worker whose Connect finished meanwhile hit the
// shutdown branch of ApplyConnectResult and gave up, and a tBootFetch tick
// retired its timer the same way. Neither clears FBootConnectPending, and
// nothing else ever re-arms the boot fetch, so every later fetch was dropped
// and lVal sat on the spinner glyph until a restart. Re-arm it here; the
// timer route waits out the aborted worker's teardown by itself.
procedure TfBG.ResumeBootAfterCancelledClose;
begin
  if firstboot or (api = nil) or (not FBootConnectPending) then
    Exit;
  // A retry countdown was already waiting: let it fire now rather than arm
  // a second boot fetch beside it.
  if FBootRetryPending then
  begin
    RetryBootNow;
    Exit;
  end;
  // The worker is still inside Connect: its ApplyConnectResult now runs with
  // FShuttingDown cleared and takes the normal path.
  if FApiCallInFlight then
    Exit;
  ArmBootFetch(50);
end;

procedure TfBG.EnterSetupScreen;
begin
  firstboot := true;
  if Assigned(fSplash) then
  begin
    fSplash.Hide;
    FreeAndNil(fSplash);
  end;
  // Reached after boot too (backend replaced with one that cannot be
  // created): the spinner would otherwise overwrite the Setup caption.
  FBootFetchPending := false;
  FBootConnectPending := false;
  if Assigned(tBootSpinner) then
    StopBootSpinner;
  tMain.Enabled := false;
  lArrow.Caption := '';
  lVal.Caption := RS_SETUP;
  lVal.Cursor := crHandPoint;
  bSettings.Show;
  LayoutSetupScreen;
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
const
  // Ports tried in all, starting at the configured one. Each Trndi instance
  // reads its own profile's webserver.port, and profiles are usually left on
  // the default, so a second instance finds the port held by the first.
  WEB_PORT_ATTEMPTS = 15;
var
  Port, LastPort: integer;
  Token: string;
  Server: TTrndiWebServer;
begin
  if Assigned(FWebServer) then
    Exit; // Already running
    
  Port := native.GetIntSetting('webserver.port', 8080);
  Token := native.GetSetting('webserver.token', '');
  LastPort := Min(Port + WEB_PORT_ATTEMPTS - 1, High(word));
  
  Server := TTrndiWebServer.Create(Port, Token, @GetCurrentReadingForWeb,
    @GetPredictionsForWeb, false, @WebCommand);
  // Several instances serve several accounts, often a port apart: name the
  // account so /health tells them apart. Single-user stays anonymous.
  if multi then
    Server.UserLabel := multinick;
  if not Server.Start(WEB_PORT_ATTEMPTS) then
  begin
    Server.Free;
    TrndiDLog(Format('Web API: no free port between %d and %d', [Port, LastPort]));
    native.attention(RS_WEBAPI_TITLE, Format(RS_WEBAPI_NO_PORT, [Port, LastPort]));
    Exit;
  end;
  FWebServer := Server;

  // The user set up their dashboard or script for the configured port, so
  // a move has to be said out loud.
  if Server.Port <> Port then
  begin
    TrndiDLog(Format('Web API: port %d is in use, listening on %d', [Port, Server.Port]));
    native.attention(RS_WEBAPI_TITLE, Format(RS_WEBAPI_PORT_MOVED, [Port, Server.Port]));
  end;

  // Seed the /events snapshot with what the UI already knows. The reading
  // itself is not needed here: a subscriber gets it from the cache callback
  // until the next fetch publishes one.
  WebPublishStatus;
  WebPublishSnooze;
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
  Result := nil;
  // Computing a forecast means API calls, which this (web server) thread must
  // not make; serve the one WebPublishPredictions cached. A forecast from
  // before predictions were switched off is not offered as current.
  if not PredictGlucoseReading then
    Exit;
  EnterCriticalSection(FReadingsLock);
  try
    Result := Copy(FCachedPredictions);
  finally
    LeaveCriticalSection(FReadingsLock);
  end;
end;

function TfBG.WebServerActive: boolean;
begin
  Result := Assigned(FWebServer) and TTrndiWebServer(FWebServer).Active;
end;

{------------------------------------------------------------------------------
  WebPublish*
  -----------
  Relay state changes to the web API's /events stream. Each is a no-op while
  no server is running, and the server drops sticky events whose payload did
  not change, so the callers publish unconditionally from the paths that
  apply readings, alerts, forecasts and snoozes. The forecast is also kept
  for /predict (GetPredictionsForWeb), so that endpoint and the predict event
  always carry the same thing.
 ------------------------------------------------------------------------------}
procedure TfBG.WebPublishReading(const Reading: BGReading);
begin
  if Assigned(FWebServer) then
    TTrndiWebServer(FWebServer).PublishReading(Reading);
end;

procedure TfBG.WebPublishStatus;
begin
  if Assigned(FWebServer) then
    TTrndiWebServer(FWebServer).PublishStatus(
      FCachedReadingsValid and (not FTrendDataStale),
      FLastFetchHadData, FLastConnectionDetail);
end;

const
  // Wire names of the alert kinds, in TAlertKind order.
  WEB_ALERT_KIND_NAMES: array[TAlertKind] of string = (
    'high', 'low', 'urgent_low', 'missing', 'sensor_fault', 'rapid_fall', 'rapid_rise');

procedure TfBG.WebPublishAlert(const Kinds: TAlertKindSet; const Reading: BGReading);
var
  Names: array of string;
  Kind: TAlertKind;
begin
  if (not Assigned(FWebServer)) or (Kinds = []) then
    Exit;
  Names := nil;
  for Kind in Kinds do
  begin
    SetLength(Names, Length(Names) + 1);
    Names[High(Names)] := WEB_ALERT_KIND_NAMES[Kind];
  end;
  TTrndiWebServer(FWebServer).PublishAlert(Names, Reading);
end;

const
  // Names the extension API uses for the alert kinds, in TAlertKind order.
  // Hyphenated like the level names extensions already see ('range-high').
  EXT_ALERT_KIND_NAMES: array[TAlertKind] of string = (
    'high', 'low', 'urgent-low', 'missing', 'sensor-fault', 'rapid-fall', 'rapid-rise');

procedure TfBG.NotifyExtensionsAlert(const Kinds: TAlertKindSet; const Reading: BGReading);
{$ifdef TrndiExt}
var
  Kind: TAlertKind;
begin
  if Kinds = [] then
    Exit;
  for Kind in Kinds do
    callFunc('alertCallback', [EXT_ALERT_KIND_NAMES[Kind],
      Reading.convert(mgdl), Reading.convert(mmol)]);
end;
{$else}
begin
end;
{$endif}

procedure TfBG.WebPublishSnooze;
begin
  if Assigned(FWebServer) and Assigned(FAlertEngine) then
    TTrndiWebServer(FWebServer).PublishSnooze(
      FAlertEngine.AnySnoozed, FAlertEngine.ActiveSnoozeUntil);
end;

procedure TfBG.WebPublishPredictions(const Preds: BGResults);
begin
  EnterCriticalSection(FReadingsLock);
  try
    FCachedPredictions := Copy(Preds);
  finally
    LeaveCriticalSection(FReadingsLock);
  end;

  if Assigned(FWebServer) then
    TTrndiWebServer(FWebServer).PublishPredictions(Preds);
end;

{------------------------------------------------------------------------------
  WebCommand and helpers
  ----------------------
  The web dashboard's settings card and snooze buttons end up here. The
  server runs WebCommand on the main thread (TWebCommandFunc), so it may read
  api/FAlertEngine and re-apply settings the way the Settings dialog does:
  write the same keys, then ApplySettingsInstantly. Values on the wire are
  mg/dL integers, the unit the readings already carry; the page converts for
  display. The server itself decides who may write (loopback or token), so
  nothing here needs to.
 ------------------------------------------------------------------------------}
function TfBG.WebSnoozeJSON: TJSONObject;
var
  untilLocal: TDateTime;
  kinds: TJSONArray;
  kind: TAlertKind;
begin
  Result := TJSONObject.Create;
  kinds := TJSONArray.Create;
  if Assigned(FAlertEngine) and FAlertEngine.AnySnoozed then
  begin
    untilLocal := FAlertEngine.ActiveSnoozeUntil;
    Result.Add('active', true);
    Result.Add('until_utc', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
      LocalTimeToUniversal(untilLocal)));
    // A per-kind snooze is "active" too; the list says which kinds.
    for kind in FAlertEngine.SnoozedKinds do
      kinds.Add(WEB_ALERT_KIND_NAMES[kind]);
  end
  else
  begin
    Result.Add('active', false);
    Result.Add('until_utc', '');
  end;
  Result.Add('kinds', kinds);
end;

procedure TfBG.WebSettingsToJSON(AReply: TJSONObject);
var
  ov, th: TJSONObject;
  lo, hi, rangeLo, rangeHi: integer;
begin
  // The effective limits: what the readings are classified against right now.
  // Before a backend is connected (setup screen) fall back to the defaults
  // the Settings dialog shows in the same state.
  if Assigned(api) then
  begin
    lo := api.cgmLo;
    hi := api.cgmHi;
    rangeLo := api.cgmRangeLo;
    rangeHi := api.cgmRangeHi;
  end
  else
  begin
    lo := 60;
    hi := 180;
    rangeLo := TrndiAPI.CGM_RANGE_LO_DISABLED;
    rangeHi := TrndiAPI.CGM_RANGE_HI_DISABLED;
  end;

  AReply.Add('unit', IfThen(native.GetSetting('unit', 'mmol') = 'mmol', 'mmol', 'mgdl'));
  AReply.Add('predictions', native.GetBoolSetting('predictions.enable', false));
  // The account shown, for the dashboard's heading; null in single-user mode.
  if multi then
    AReply.Add('user', multinick)
  else
    AReply.Add('user', TJSONNull.Create);

  // The stored overrides, with the effective limits as the values the page
  // shows while an override is off (mirrors the dialog's ApplyRangeBounds:
  // an unset band falls back to the limits rather than the sentinels).
  ov := TJSONObject.Create;
  ov.Add('enabled', native.GetBoolSetting('override.enabled', false));
  ov.Add('lo', native.GetIntSetting('override.lo', lo));
  ov.Add('hi', native.GetIntSetting('override.hi', hi));
  ov.Add('range', native.GetBoolSetting('override.range', false));
  if rangeLo = TrndiAPI.CGM_RANGE_LO_DISABLED then
    ov.Add('range_lo', native.GetIntSetting('override.rangelo', lo))
  else
    ov.Add('range_lo', native.GetIntSetting('override.rangelo', rangeLo));
  if rangeHi = TrndiAPI.CGM_RANGE_HI_DISABLED then
    ov.Add('range_hi', native.GetIntSetting('override.rangehi', hi))
  else
    ov.Add('range_hi', native.GetIntSetting('override.rangehi', rangeHi));
  AReply.Add('override', ov);

  th := TJSONObject.Create;
  th.Add('lo', lo);
  th.Add('hi', hi);
  if rangeLo = TrndiAPI.CGM_RANGE_LO_DISABLED then
    th.Add('range_lo', TJSONNull.Create)
  else
    th.Add('range_lo', rangeLo);
  if rangeHi = TrndiAPI.CGM_RANGE_HI_DISABLED then
    th.Add('range_hi', TJSONNull.Create)
  else
    th.Add('range_hi', rangeHi);
  AReply.Add('thresholds', th);

  AReply.Add('snooze', WebSnoozeJSON);
end;

procedure TfBG.WebReportToJSON(const st: TTrndiReportStats; AReply: TJSONObject);
const
  BAND_KEYS: array[TTrndiReportBand] of string =
    ('low', 'below_range', 'in_range', 'above_range', 'high');

  function UtcIso(const t: TDateTime): string;
  begin
    Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', LocalTimeToUniversal(t));
  end;

  // Two decimals is what the dialog prints; the raw doubles would only add
  // floating-point noise to the wire.
  function Num(const v: double): TJSONData;
  begin
    Result := WebDecimal(v, 2);
  end;

  function Mark(const value: double; const at: TDateTime): TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.Add('value', Num(value));
    Result.Add('at_utc', UtcIso(at));
  end;

var
  bands, bandObj, lim: TJSONObject;
  buckets: TJSONArray;
  bucket: TJSONObject;
  b: TTrndiReportBand;
  i: integer;
begin
  AReply.Add('valid', st.valid);
  AReply.Add('unit', IfThen(un = BGUnit.mmol, 'mmol', 'mgdl'));
  if not st.valid then
  begin
    AReply.Add('count', 0);
    Exit;
  end;

  AReply.Add('first_utc', UtcIso(st.first));
  AReply.Add('last_utc', UtcIso(st.last));
  AReply.Add('span_minutes', st.spanMinutes);
  AReply.Add('count', st.count);
  AReply.Add('cadence_minutes', Num(st.cadenceMinutes));
  AReply.Add('coverage_percent', Num(st.coverage));

  lim := TJSONObject.Create;
  lim.Add('lo', Num(st.limits.lo));
  lim.Add('hi', Num(st.limits.hi));
  if st.limits.hasRange then
  begin
    lim.Add('range_lo', Num(st.limits.rangeLo));
    lim.Add('range_hi', Num(st.limits.rangeHi));
  end
  else
  begin
    lim.Add('range_lo', TJSONNull.Create);
    lim.Add('range_hi', TJSONNull.Create);
  end;
  AReply.Add('limits', lim);

  bands := TJSONObject.Create;
  for b := Low(TTrndiReportBand) to High(TTrndiReportBand) do
  begin
    bandObj := TJSONObject.Create;
    bandObj.Add('count', st.bands[b]);
    bandObj.Add('percent', Num(TrndiReportBandPercent(st, b)));
    bands.Add(BAND_KEYS[b], bandObj);
  end;
  AReply.Add('bands', bands);
  // Between the clinical limits, whatever the personal band: the figure the
  // TIR badge shows when the band is off.
  AReply.Add('in_limits_percent', Num(TrndiReportBandPercent(st, rbBelowRange) +
    TrndiReportBandPercent(st, rbInRange) + TrndiReportBandPercent(st, rbAboveRange)));

  AReply.Add('mean', Num(st.mean));
  AReply.Add('median', Num(st.median));
  AReply.Add('sd', Num(st.sd));
  AReply.Add('cv_percent', Num(st.cv));
  AReply.Add('gmi_percent', Num(st.gmiPercent));
  AReply.Add('gmi_mmol_mol', Num(st.gmiMmolMol));
  AReply.Add('lowest', Mark(st.lowest, st.lowestAt));
  AReply.Add('highest', Mark(st.highest, st.highestAt));
  AReply.Add('low_excursions', st.lowExcursions);
  AReply.Add('high_excursions', st.highExcursions);
  AReply.Add('longest_gap_minutes', st.longestGap);
  AReply.Add('longest_gap_at_utc', UtcIso(st.longestGapAt));
  AReply.Add('sparkline', TrndiReportSparkline(st));

  buckets := TJSONArray.Create;
  for i := 0 to High(st.buckets) do
  begin
    bucket := TJSONObject.Create;
    bucket.Add('starts_utc', UtcIso(st.buckets[i].starts));
    bucket.Add('count', st.buckets[i].count);
    if st.buckets[i].count > 0 then
      bucket.Add('mean', Num(st.buckets[i].mean))
    else
      bucket.Add('mean', TJSONNull.Create);
    buckets.Add(bucket);
  end;
  AReply.Add('buckets', buckets);
end;

function TfBG.WebHistoryPNG(const AParams: TJSONObject; AReply: TJSONObject;
  out AError: string): boolean;
const
  DEFAULT_WIDTH = 760;   // The graph window's own 96 dpi design size
  DEFAULT_HEIGHT = 460;
  MIN_WIDTH = 240;
  MAX_WIDTH = 2400;
  MIN_HEIGHT = 160;
  MAX_HEIGHT = 1600;
  MAX_MINUTES = 7 * 24 * 60;
var
  w, h, minutes: integer;
  ms: TMemoryStream;
  png, theme: string;
begin
  Result := false;
  AError := '';
  if api = nil then
  begin
    AError := 'No backend connected';
    Exit;
  end;

  w := DEFAULT_WIDTH;
  h := DEFAULT_HEIGHT;
  minutes := 0;
  theme := 'light';
  if AParams <> nil then
  begin
    w := AParams.Get('width', DEFAULT_WIDTH);
    h := AParams.Get('height', DEFAULT_HEIGHT);
    minutes := AParams.Get('minutes', 0);
    theme := AParams.Get('theme', 'light');
  end;
  if (theme <> 'light') and (theme <> 'dark') then
  begin
    AError := 'theme must be light or dark';
    Exit;
  end;
  if (w < MIN_WIDTH) or (w > MAX_WIDTH) or (h < MIN_HEIGHT) or (h > MAX_HEIGHT) then
  begin
    AError := Format('width must be %d-%d and height %d-%d',
      [MIN_WIDTH, MAX_WIDTH, MIN_HEIGHT, MAX_HEIGHT]);
    Exit;
  end;
  if (minutes < 0) or (minutes > MAX_MINUTES) then
  begin
    AError := Format('minutes must be 0 (everything held) to %d', [MAX_MINUTES]);
    Exit;
  end;

  // A form of its own, never shown: drawing into the user's open graph would
  // resize it and change its range under them. It renders without a window
  // handle, and lives until the main form goes.
  if FWebGraph = nil then
    FWebGraph := TfHistoryGraph.Create(Self);
  FWebGraph.ClientWidth := w;
  FWebGraph.ClientHeight := h;
  FWebGraph.SetPalette(CurrentHistoryGraphPalette);
  FWebGraph.SetThresholds(api.cgmHi, api.cgmLo, api.cgmRangeHi, api.cgmRangeLo);
  // The browser's appearance, not this desktop's: the PNG sits in the
  // dashboard's card, whose colours follow the page.
  FWebGraph.SetDarkTheme(theme = 'dark');
  FWebGraph.SetRangeMinutes(minutes);
  FWebGraph.SetReadings(HeldReadings, un);
  AutoEnableBasalOverlay(FWebGraph);
  AutoEnableBolusOverlay(FWebGraph);
  AutoEnableCarbOverlay(FWebGraph);
  AutoAddPredictionOverlay(FWebGraph);

  ms := TMemoryStream.Create;
  try
    FWebGraph.RenderToStream(ms);
    SetLength(png, ms.Size);
    if ms.Size > 0 then
      Move(ms.Memory^, png[1], ms.Size);
  finally
    ms.Free;
  end;

  AReply.Add('png_base64', EncodeStringBase64(png));
  AReply.Add('width', w);
  AReply.Add('height', h);
  AReply.Add('minutes', minutes);
  Result := true;
end;

function TfBG.WebCommand(const ACommand: string; const AParams: TJSONObject;
  AReply: TJSONObject; out AError: string): boolean;

  function Refuse(const Msg: string): boolean;
  begin
    AError := Msg;
    Result := false;
  end;

var
  ov: TJSONObject;
  data: TJSONData;
  unitStr: string;
  ovEnabled, ovRange: boolean;
  lo, hi, rangeLo, rangeHi, minutes: integer;
begin
  AError := '';
  Result := true;

  if ACommand = 'settings.get' then
    WebSettingsToJSON(AReply)
  else
  if ACommand = 'settings.set' then
  begin
    if AParams = nil then
      Exit(Refuse('Missing body'));

    // Validate everything first, so a refused request changes nothing.
    ovEnabled := false;
    ovRange := false;
    lo := 0;
    hi := 0;
    rangeLo := 0;
    rangeHi := 0;
    unitStr := AParams.Get('unit', '');
    if (unitStr <> '') and (unitStr <> 'mmol') and (unitStr <> 'mgdl') then
      Exit(Refuse('unit must be mmol or mgdl'));

    ov := nil;
    data := AParams.Find('override');
    if data <> nil then
    begin
      if not (data is TJSONObject) then
        Exit(Refuse('override must be an object'));
      ov := TJSONObject(data);
      ovEnabled := ov.Get('enabled', native.GetBoolSetting('override.enabled', false));
      ovRange := ov.Get('range', native.GetBoolSetting('override.range', false));
      lo := ov.Get('lo', native.GetIntSetting('override.lo', 60));
      hi := ov.Get('hi', native.GetIntSetting('override.hi', 180));
      rangeLo := ov.Get('range_lo', native.GetIntSetting('override.rangelo', lo));
      rangeHi := ov.Get('range_hi', native.GetIntSetting('override.rangehi', hi));
      if ovEnabled and ((lo < 20) or (hi > 600) or (lo >= hi)) then
        Exit(Refuse('Low must be below high, within 20-600 mg/dL'));
      if ovRange and ((rangeLo >= rangeHi) or (rangeLo < 20) or (rangeHi > 600)) then
        Exit(Refuse('The in-range band must run from a lower to a higher value'));
      if ovRange and ovEnabled and ((rangeLo < lo) or (rangeHi > hi)) then
        Exit(Refuse('The in-range band must sit between the low and high limits'));
    end;

    if unitStr <> '' then
      native.SetSetting('unit', unitStr);
    if AParams.Find('predictions') <> nil then
      native.SetSetting('predictions.enable', AParams.Get('predictions', false));
    if ov <> nil then
    begin
      // Same keys, same on/off handling as the Settings dialog: an override
      // that is off has its values removed so the backend's own apply.
      native.SetSetting('override.enabled', ovEnabled);
      if ovEnabled then
      begin
        native.SetSetting('override.lo', lo);
        native.SetSetting('override.hi', hi);
      end
      else
      begin
        native.DeleteSetting('override.lo');
        native.DeleteSetting('override.hi');
      end;
      native.SetSetting('override.range', ovRange);
      if ovRange then
      begin
        native.SetSetting('override.rangelo', rangeLo);
        native.SetSetting('override.rangehi', rangeHi);
      end
      else
      begin
        native.DeleteSetting('override.rangelo');
        native.DeleteSetting('override.rangehi');
      end;
    end;

    ApplySettingsInstantly;
    WebSettingsToJSON(AReply);
  end
  else
  if ACommand = 'snooze' then
  begin
    if not Assigned(FAlertEngine) then
      Exit(Refuse('Alerts are not available'));
    minutes := -1;
    if AParams <> nil then
      minutes := AParams.Get('minutes', -1);
    if (minutes < 0) or (minutes > 1440) then
      Exit(Refuse('minutes must be 0 (resume) to 1440'));
    // Same as the tray menu: pause every rule, per-rule caps still apply.
    // The engine's state callback publishes the new state to /events.
    if minutes = 0 then
      FAlertEngine.ResumeAll
    else
      FAlertEngine.SnoozeAll(minutes);
    UpdateAlertSnoozeMenu;
    RefreshConnectionBadge;
    AReply.Add('snooze', WebSnoozeJSON);
  end
  else
  if ACommand = 'report.get' then
  begin
    minutes := 0;
    if AParams <> nil then
      minutes := AParams.Get('minutes', 0);
    if (minutes < 0) or (minutes > 7 * 24 * 60) then
      Exit(Refuse('minutes must be 0 (everything held) to 10080'));
    WebReportToJSON(BuildGlucoseReport(minutes), AReply);
  end
  else
  if ACommand = 'history.png' then
    Result := WebHistoryPNG(AParams, AReply, AError)
  else
    Result := Refuse('Unknown command');
end;
