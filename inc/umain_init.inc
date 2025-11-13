(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)
// Main form helper functions for trend dots and UI initialization

// Apply a procedure to all trend points; also provides an index
procedure TfBG.actOnTrend(proc: TTrendProcLoop);
var
  ix: integer;
  ls: array[1..10] of TPaintBox;
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
  ls: array[1..10] of TPaintBox;
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
begin
  lang := native.GetSetting('locale', '');
  if (lang = 'auto') or (lang = '') then
    lang := native.GetOSLanguage;
  applocale := lang;
  Application.ProcessMessages;

  SetDefaultLang(lang, getLangPath);
end;

{$ifdef X_LINUXBSD}
{$endif}

{$ifdef DARWIN}
procedure TfBG.InitializePlatformMenus;
var
  MainMenu: TMainMenu;
  AppMenu,
  forceMenu,
  SettingsMenu,
  HelpMenu,
  GithubMenu: TMenuItem;
begin
  MacAppDelegate := TMyAppDelegate.alloc.init;
  NSApp.setDelegate(NSObject(MacAppDelegate));

  Application.Title := 'Trndi';
  MainMenu := TMainMenu.Create(self);
  fBg.Menu := MainMenu;
  AppMenu := TMenuItem.Create(Self); // Application menu
  AppMenu.Caption := #$EF#$A3#$BF;   // Unicode Apple logo char
  MainMenu.Items.Insert(0, AppMenu);
  SettingsMenu := TMenuitem.Create(self);
  settingsmenu.Caption := miSettings.Caption;
  settingsmenu.OnClick := misettings.OnClick;
  AppMenu.Insert(0, SettingsMenu);

  forcemenu := TMenuItem.Create(self);
  forcemenu.Caption := miForce.caption;
  forcemenu.onclick := miForce.OnClick;
  AppMenu.Insert(1, forceMenu);

  helpmenu := TMenuItem.Create(self);
  helpmenu.Caption := 'Help';
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
  s: string;
begin
  // Set dots
  DOT_GRAPH := native.GetWideCharSetting('font.dot', System.widechar($2B24));
  DOT_FRESH := native.GetWideCharSetting('font.dot_fresh', System.widechar($2600));

  // Load fonts
  s := native.GetSetting('font.val', 'default');
  if s <> 'default' then
    lVal.Font.Name := s;
  s := native.GetSetting('font.arrow', 'default');
  if s <> 'default' then
    lArrow.Font.Name := s;

  s := native.GetSetting('font.ago', 'default');
  if s <> 'default' then
  begin
    lAgo.Font.Name := s;
    lTir.Font.Name := s;
  end;

  // Sensitive data
  DATA_FRESHNESS_THRESHOLD_MINUTES :=
    native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);

  // Check graph
  for i := 1 to NUM_DOTS do
  begin
    s := 'lDot' + IntToStr(i);
    TrendDots[i] := FindComponent(s) as TPaintBox;
    if not Assigned(TrendDots[i]) then
      ShowMessage(Format('Label %s is missing!', [s]))
    else
      LogMessage(Format('Label %s assigned to TrendDots[%d].', [s, i]));
  end;

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
        linfo.Caption := 'Trndi is loading...';
        linfo.top := 0;
        linfo.left := 400;
        Height := linfo.canvas.TextHeight('Pq') + 5;

        label1.AutoSize := true;
        label1.Caption := 'Trndi | You need to accept the license agreement! ';
        label1.top := 0;
        label1.left := 0;
        label1.font := linfo.font;
        label1.font.color := clWhite;
        Application.ProcessMessages;
      end;
  FStoredWindowInfo.Initialized := false;
  fSplash.Image1.Picture.Icon := Application.Icon;
  fSplash.lInfo.Caption := '';
  fSplash.lInfo.Font.Color := fSplash.lSplashWarn.Font.color;
  {$ifdef X_WIN}
  fSplash.ShowInTaskBar := stAlways;
  {$endif}
  fSplash.Show;
end;

procedure TfBG.LoadUserProfile;
var
  i: integer;
  s: string;
begin
  if username <> '' then
  begin
    with TStringList.Create do
    begin
      AddCommaText(username);
      i := ExtList(uxdAuto, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE,
        RS_MULTIUSER_BOX, ToStringArray, true);

      if (i > -1) and (strings[i] <> '') then
      begin
        username := strings[i];
        native.configUser := username;
        s := native.GetSettingEx('user.nick', username);

        fbg.Caption := Format(RS_USER_CAPTION, [s, fBG.Caption]);
        multinick := s;
      end
      else
      begin
        username := '';
        s := native.GetSettingEx('user.nick', RS_DEFAULT_ACCOUNT);

        multinick := s;
        fbg.Caption := Format(RS_USER_CAPTION, [s, fBG.Caption]);
      end;
      Free;
    end;// Load possible other users
    multi := true;
    pnMultiUser.Color := native.GetColorSetting('user.color', clBlack);
    if pnMultiUser.Color <> clBlack then
    begin
      pnMultiUser.Visible := native.GetRootSetting('users.colorbox', 'true') = 'true';
      customTitlebar := setColorMode;
      // Set the custom title bar value depending if the panel is showing
    end;
  end
  else
    multi := false;
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
        uxclWhite, uxclRed, [mbYes, mbCancel, mbUxRead], uxmtCustom, 5);
      if i = mrYes then
        native.SetBoolSetting('license.250608', true)
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
  apiTarget, apiCreds: string;
begin
  Result := true;

  apiTarget := native.GetSetting('remote.target');
  apiCreds := native.GetSetting('remote.creds');

  case native.GetSetting('remote.type') of
  'NightScout':
    api := NightScout.Create(apiTarget, apiCreds, '');
  'NightScout v3':
    api := NightScout3.Create(apiTarget, apiCreds, '');
  'Dexcom (USA)':
    api := Dexcom.Create(apiTarget, apiCreds, 'usa');
  'Dexcom (Outside USA)':
    api := Dexcom.Create(apiTarget, apiCreds, 'world');
  'xDrip':
    api := xDrip.Create(apiTarget, apiCreds, '');
  {$ifdef DEBUG}
  '* Debug Backend *':
    api := DebugAPI.Create(apiTarget, apiCreds, '');
  '* Debug Missing Backend *':
    api := DebugMissingAPI.Create(apiTarget, apiCreds, '');
  '* Debug Perfect Backend *':
    api := DebugPerfectAPI.Create(apiTarget, apiCreds, '');
  '* Debug Custom Backend *':
    api := DebugCustomAPI.Create(apiTarget, apiCreds, '');
  '* Debug Edge Backend *':
    api := DebugEdgeAPI.Create(apiTarget, apiCreds, '');
    {$endif}
  else
    Result := false;
  end;
end;

{$ifdef X_LINUXBSD}
procedure TfBG.InitializeLinuxPlatform;
var
  x, s: string;
begin
  x := scanLinuxDistro(['fedora','ubuntu','debian']);
  case x of
  'fedora':
    s := 'Poppins';
  'ubuntu':
    s := 'Sans';
  else
    s := 'default';
  end;

  IsRaspberry := false;
  if x = 'debian' then
    IsRaspberry := FileExists('/etc/rpi-issue');

  fBG.Font.Name := s;

  {$ifndef LCLQt6}
  Showmessage('This release of Trndi was compiled for a non-supported platform ("widgetset")'#10'Performance might be bad and features might not work as intended!'#10#10'Please download the official release (Qt6) from github.com/slicke/trndi');
  {$endif}
  isWSL := TrndiNative.DetectWSL.IsWSL;
  if isWSL then
    Showmessage('Windows Linux Subsystem (WSL) detected. Due to limitations in WSL, graphic issues may occur. Commonly, windows will appear at random positions an not where expected!');
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
  guifontName, txtfontName, apiTarget, apiCreds: string;
  fil: boolean;
  userlocale: TFormatSettings;
begin
  firstboot := false;
  // Initialize splash screen first
  InitializeSplashScreen;
  Application.ProcessMessages;
  Application.OnException := @AppExceptionHandler;

  // Start media backend early
  fSplash.lInfo.Caption := 'Starting Media Backend...';
  MediaController := TSystemMediaController.Create(Self);
  MediaController.Initialize;
  Application.ProcessMessages;

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

  // Initialize native interface
  native := TrndiNative.Create;
  if ssShift in GetKeyShiftState then
    if native.DetectTouchScreen(fil) then
      native.touchOverride := tbFalse
    else
      native.touchOverride := tbTrue;

  setColorMode;

  // Platform-specific initialization
  {$ifdef X_LINUXBSD}
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
  fBG.font.Name := txtFontName;
  lArrow.Font.Name := guifontName;
  {$endif}

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
    SetLang;
    username := GetRootSetting('users.names', '');
    LoadUserProfile;

    // Locale setup
    userlocale := DefaultFormatSettings;
    userlocale.DecimalSeparator := GetCharSetting('locale.separator', '.');
    badge_adjust := GetIntSetting('ux.badge_size', 0) / 10;
    native.locale := userlocale;

    // License check
    CheckAndAcceptLicense;
    Application.ProcessMessages;

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
      tMain.Enabled := false;
      for i := 0 to fBG.ComponentCount - 1 do
        if (fbg.Components[i] is TLabel) and (fbg.Components[i] <> lval) then
          (fbg.Components[i] as TLabel).Caption := '';
      miSettings.Click;
      ShowMessage(RS_FORCE_QUIT_SETUP);
      Application.Terminate;
      Exit;
    end;

    Application.ProcessMessages;
    if not InitializeAPI then
    begin
      haltBoot;
      Exit;
    end;

    Application.ProcessMessages;
    if not api.Connect then
    begin
      haltBoot;
      ShowMessage(api.ErrorMsg);
      miSettings.Click;
      self.Close;
      Exit;
    end;


    // UI preferences
    dotscale := GetIntSetting('ux.dot_scale', 1);
    DOT_ADJUST := GetFloatSetting('ux.dot_adjust', 0);

    DOT_OFFSET_RANGE := GetIntSetting('ux.dot_offset_range', DOT_OFFSET_RANGE);
    // DOT_VISUAL_OFFSET := CalculateDotVisualOffset;  // No longer needed - centering uses half dot height
    miRangeColor.Checked := GetBoolSetting('ux.range_color', true);
    PaintRange := native.GetBoolSetting('ux.paint_range', true);
    PaintRangeLines := native.GetBoolSetting('ux.paint_range_lines', false);
    PaintRangeCGMRange := native.GetBoolSetting('ux.paint_range_cgmrange', false);
    // Extensions
    {$ifdef TrndiExt}
    fSplash.lInfo.Caption := RS_SPLASH_LOADING_INIT;
    LoadExtensions;
    {$endif}

    // Override settings - always apply if set, regardless of checkbox state
    // This allows extensions and manual config edits to work
    api.cgmLo := GetIntSetting('override.lo', api.cgmLo);
    api.cgmHi := GetIntSetting('override.hi', api.cgmHi);
    api.cgmRangeLo := GetIntSetting('override.rangelo', api.cgmRangeLo);
    api.cgmRangeHi := GetIntSetting('override.rangehi', api.cgmRangeHi);

    PredictGlucoseReading := GetBoolSetting('predictions.enable', false);
    PredictShortMode := GetBoolSetting('predictions.short', false);
    lPredict.Visible := PredictGlucoseReading;
  end;

  // Final initialization and first reading
  Application.ProcessMessages;
  if not updateReading(true) then
  begin
    updateReading; // Second attempt for setup
    showWarningPanel(RS_NO_BOOT_READING);
  end;

  // Cleanup splash screen
  fSplash.Close;
  fSplash.Free;
  tmain.Enabled := true;
  tsetup.Enabled := true;
end;
