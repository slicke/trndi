unit trndi.native.linux;

{**
  @abstract(Linux/BSD-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeLinux) which derives from
  @link(TTrndiNativeBase) and implements behaviors using common Linux tools
  and LCL facilities.

  Responsibilities include:
  - Text-to-speech via @code(spd-say) (@link(TTrndiNativeLinux.Speak))
  - Drawing a badge on the system tray icon (@link(TTrndiNativeLinux.SetTray))
  - Synchronizing KDE taskbar badge (@link(TTrndiNativeLinux.SetBadge))
  - Placeholder for dark mode toggling (@link(TTrndiNativeLinux.setDarkMode))
  - HTTP GET via libCURL
  - Settings persisted in INI/CFG with backward compatibility

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Graphics, IniFiles, Dialogs,
ExtCtrls, Forms, Math, LCLIntf, KDEBadge, trndi.native.base, FileUtil, Menus,
libpascurl;

type
  {!
    @abstract(Linux implementation of @link(TTrndiNativeBase).)
    Uses spd-say for speech and draws badges on tray/KDE taskbar.
  }
TTrndiNativeLinux = class(TTrndiNativeBase)
protected
  Tray: TTrayIcon;
  TrayMenu: TPopupMenu;
  inistore: TIniFile; // Linux-specific settings store
    // Flashing support
  FFlashTimer: TTimer;
  FFlashEnd: TDateTime;
  FFlashPhase: integer;
  FFlashValue: string;
  FFlashBaseColor: TColor;
  FFlashCycleMS: integer;
  procedure FlashTimerTick(Sender: TObject);
    {** Resolve the INI/CFG file path with backward compatibility.
        Preference order: Lazarus app config, ~/.config/Trndi/trndi.ini, legacy ~/.config/Trndi.cfg }
  function ResolveIniPath: string; virtual;
    {** Ensure the INI store is created and directory exists. }
  procedure EnsureIni; inline;
public
  {** Prefer gdbus notifications under Qt6; fallback to base attention when
      gdbus is unavailable or fails. }
  procedure attention(topic, message: string); override;
    {** Free tray resources, shutdown KDE badge if needed, and close INI store. }
  destructor Destroy; override;
    {** Speaks @param(Text) using spd-say, if available.
        Shows a user-visible message when the tool is not present. }
    {** Speak text via spd-say if present; warn user if missing. }
  procedure Speak(const Text: string); override;
    {** Draw a badge with @param(Value) text on tray icon using @param(BadgeColor).
        @param(badge_size_ratio Determines badge diameter relative to icon size)
        @param(min_font_size Lower bound for font size while fitting text) }
    {** Draw a badge on the tray icon. }
  {** Draw a badge with @param(Value) text on tray icon using @param(BadgeColor).
      @param(badge_size_ratio Determines badge diameter relative to icon size)
      @param(min_font_size Lower bound for font size while fitting text) }
  procedure SetTray(const Value: string; BadgeColor: TColor;
    badge_size_ratio: double = 0.8; min_font_size: integer = 8);
    {** Convenience overload: redirects to base two-arg version. }
  procedure setBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
    {** Synchronize KDE badge with numeric value and update tray badge drawing. }
  procedure setBadge(const Value: string; BadgeColor: TColor;
    badge_size_ratio: double; min_font_size: integer); overload; override;
  procedure StartBadgeFlash(const Value: string; badgeColor: TColor;
    DurationMS: integer = 10000; CycleMS: integer = 400); override;
  procedure StopBadgeFlash; override;
    {** Placeholder for desktop-specific dark mode. Return False for now. }
  class function setDarkMode: boolean; // no-op placeholder

    // Settings API overrides
    {** Read setting from INI (multi-section + legacy key=value fallback). }
  function GetSetting(const keyname: string; def: string = '';
    global: boolean = false): string; override;
    {** Write setting to canonical [trndi] section and flush to disk. }
  procedure SetSetting(const keyname: string; const val: string;
    global: boolean = false); override;
    {** Delete setting across known sections for completeness. }
  procedure DeleteSetting(const keyname: string; global: boolean = false); override;
    {** Drop INI handle; re-created on demand. }
  procedure ReloadSettings; override;
  {** Simple HTTP GET using FPC HTTP client with default UA.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
  class function getURL(const url: string; out res: string): boolean; override;
  {** Desktop-aware dark mode detection.
    Order:
    1) KDE Plasma via kreadconfig5: General/ColorScheme contains "Dark".
    2) GNOME via gsettings: org.gnome.desktop.interface color-scheme (prefer-dark/default),
     then fallback to gtk-theme containing "-dark".
    3) GTK_THEME environment variable contains "dark" (e.g. Adwaita:dark).
    4) Fallback heuristic comparing clWindow vs clWindowText brightness.
  }
  class function isDarkMode: boolean; override;
    {** Returns True if notify-send is available on this system. }
  class function isNotificationSystemAvailable: boolean; override;
    {** Identify notification backend: 'gdbus' (Qt6 path) or 'notify-send' or 'none'. }
  class function getNotificationSystem: string; override;

    {** Triggers when the tray icon is clicked }
  procedure trayClick(Sender: TObject);
end;

implementation

uses
Process, Types, LCLType;

{------------------------------------------------------------------------------
  IsNotifySendAvailable
  ---------------------
  Check PATH for the 'notify-send' tool. Returns True if found.
 ------------------------------------------------------------------------------}
function IsNotifySendAvailable: boolean;
var
  AProcess: TProcess;
  OutputLines: TStringList;
begin
  Result := false;
  AProcess := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    AProcess.Executable := '/usr/bin/which';
    AProcess.Parameters.Add('notify-send');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    AProcess.Execute;
    OutputLines.LoadFromStream(AProcess.Output);
    if (OutputLines.Count > 0) and FileExists(Trim(OutputLines[0])) then
      Result := true;
  except
    on E: Exception do
      Result := false;
  end;
  OutputLines.Free;
  AProcess.Free;
end;

{------------------------------------------------------------------------------
  RunAndCaptureSimple
  -------------------
  Run an external command and capture stdout. Returns True when exit code is 0.
 ------------------------------------------------------------------------------}
function RunAndCaptureSimple(const Exec: string; const Params: array of string;
out StdoutS: string; out ExitCode: integer): boolean;
var
  P: TProcess;
  i: integer;
  OutStr: TStringStream;
  Buf: array[0..4095] of byte;
  n: SizeInt;
begin
  Result := false;
  StdoutS := '';
  ExitCode := -1;

  P := TProcess.Create(nil);
  OutStr := TStringStream.Create('');
  try
    P.Executable := Exec;
    for i := 0 to High(Params) do
      P.Parameters.Add(Params[i]);
    P.Options := [poUsePipes, poWaitOnExit];
    P.ShowWindow := swoHIDE;
    try
      P.Execute;
      while P.Running do
      begin
        while P.Output.NumBytesAvailable > 0 do
        begin
          n := P.Output.Read(Buf, SizeOf(Buf));
          if n > 0 then
            OutStr.WriteBuffer(Buf, n)
          else
            Break;
        end;
        Sleep(3);
      end;
      // Drain any remaining bytes after process exits
      while P.Output.NumBytesAvailable > 0 do
      begin
        n := P.Output.Read(Buf, SizeOf(Buf));
        if n > 0 then
          OutStr.WriteBuffer(Buf, n)
        else
          Break;
      end;
      ExitCode := P.ExitStatus;
      StdoutS := Trim(OutStr.DataString);
      Result := ExitCode = 0;
    except
      on E: Exception do
      begin
        StdoutS := '';
        ExitCode := -1;
        Result := false;
      end;
    end;
  finally
    OutStr.Free;
    P.Free;
  end;
end;

// C-compatible write callback for libcurl used in this unit
function CurlWriteCallback_Linux(buffer: pchar; size, nmemb: longword;
userdata: Pointer): longword; cdecl;
var
  Bytes: SizeInt;
  SS: TStringStream;
begin
  if (userdata = nil) or (buffer = nil) then
  begin
    Result := 0;
    Exit;
  end;
  SS := TStringStream(userdata);
  Bytes := SizeInt(size) * SizeInt(nmemb);
  if Bytes > 0 then
    SS.WriteBuffer(buffer^, Bytes);
  Result := Bytes;
end;

// Return value of environment variable Name (empty if unset)
function EnvValue(const Name: string): string; inline;
begin
  Result := GetEnvironmentVariable(Name);
end;

// Best-effort desktop environment hint (XDG_CURRENT_DESKTOP or DESKTOP_SESSION)
function DesktopHint: string;
begin
  Result := EnvValue('XDG_CURRENT_DESKTOP');
  if Result = '' then
    Result := EnvValue('DESKTOP_SESSION');
end;

// True if S contains the substring "dark" (case-insensitive)
function ContainsDark(const S: string): boolean; inline;
begin
  Result := Pos('dark', LowerCase(S)) > 0;
end;

// Forward declaration for helper declared later in this unit
function FindInPath(const FileName: string): string; forward;

// True when the desktop environment is KDE/Plasma or GNOME-family (gnome/ubuntu/unity)
function IsKdeOrGnomeLike: boolean; inline;
var
  d: string;
begin
  d := LowerCase(DesktopHint);
  Result := (Pos('kde', d) > 0) or (Pos('plasma', d) > 0) or
    (Pos('gnome', d) > 0) or (Pos('ubuntu', d) > 0) or (Pos('unity', d) > 0);
end;

// Decide whether we should use gdbus for notifications (Qt6 + gdbus + KDE/GNOME-like)
function UseGDBusForNotifications: boolean; inline;
begin
  {$IFDEF LCLQt6}
  Result := (FindInPath('gdbus') <> '') and IsKdeOrGnomeLike;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  DetectGnomeDark
  ---------------
  GNOME/Ubuntu/Unity: read color-scheme (prefer-dark/default) or gtk-theme via
  gsettings. Returns True if a decision was made and sets isDark accordingly.
 ------------------------------------------------------------------------------}
function DetectGnomeDark(out isDark: boolean): boolean;
var
  gsettingsPath, outS: string;
  exitCode: integer;
  dHint: string;
begin
  Result := false;
  isDark := false;
  dHint := LowerCase(DesktopHint);
  if (Pos('gnome', dHint) = 0) and (Pos('ubuntu', dHint) = 0) and
    (Pos('unity', dHint) = 0) then
  // Not obviously GNOME; still proceed if gsettings exists
  ;
  gsettingsPath := FindInPath('gsettings');
  if gsettingsPath = '' then
    Exit(false);

  // GNOME 42+: color-scheme prefer-dark/default
  if RunAndCaptureSimple(gsettingsPath,
    ['get', 'org.gnome.desktop.interface', 'color-scheme'], outS, exitCode) and
    (exitCode = 0) then
  begin
    outS := LowerCase(StringReplace(outS, '''', '', [rfReplaceAll]));
    if Pos('prefer-dark', outS) > 0 then
    begin
      isDark := true;
      Exit(true);
    end
    else
    if (Pos('default', outS) > 0) or (Pos('prefer-light', outS) > 0) then
    begin
      isDark := false;
      Exit(true);
    end;
    // fallthrough to gtk-theme
  end;

  // Fallback: inspect gtk-theme name for '*-dark'
  if RunAndCaptureSimple(gsettingsPath,
    ['get', 'org.gnome.desktop.interface', 'gtk-theme'], outS, exitCode) and
    (exitCode = 0) then
  begin
    outS := LowerCase(StringReplace(outS, '''', '', [rfReplaceAll]));
    if ContainsDark(outS) then
      isDark := true
    else
      isDark := false;
    Exit(true);
  end;

  Result := false; // unable to determine via GNOME
end;

{------------------------------------------------------------------------------
  DetectKDEDark
  -------------
  KDE Plasma: read General/ColorScheme via kreadconfig5. Returns True if a
  decision was made and sets isDark accordingly.
 ------------------------------------------------------------------------------}
function DetectKDEDark(out isDark: boolean): boolean;
var
  kreadPath, outS: string;
  exitCode: integer;
  dHint: string;
begin
  Result := false;
  isDark := false;
  dHint := LowerCase(DesktopHint);
  if (Pos('kde', dHint) = 0) and (Pos('plasma', dHint) = 0) then
  // Not obviously KDE; continue if tool exists
  ;
  kreadPath := FindInPath('kreadconfig5');
  if kreadPath = '' then
    Exit(false);

  // Read the active color scheme
  if RunAndCaptureSimple(kreadPath, ['--group', 'General', '--key', 'ColorScheme'],
    outS, exitCode) and (exitCode = 0) then
  begin
    if ContainsDark(outS) then
      isDark := true
    else
      isDark := false;
    Exit(true);
  end;

  Result := false;
end;

{------------------------------------------------------------------------------
  getURL
  ------
  Linux/PC implementation using cURL; returns response text or error.
 ------------------------------------------------------------------------------}
class function TTrndiNativeLinux.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  handle: CURL;
  headers: pcurl_slist;
  errCode: CURLcode;
  responseStream: TStringStream;
begin
  res := '';
  headers := nil;
  responseStream := TStringStream.Create('');
  try
    curl_global_init(CURL_GLOBAL_DEFAULT);
    handle := curl_easy_init();
    if handle = nil then
    begin
      res := 'curl: failed to init';
      Result := false;
      Exit;
    end;

    // Set URL and options
    curl_easy_setopt(handle, CURLOPT_URL, pchar(url));
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, longint(1));
    curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(DEFAULT_USER_AGENT));

    // Write callback
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallback_Linux));
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));

    errCode := curl_easy_perform(handle);
    if errCode <> CURLE_OK then
    begin
      res := string(curl_easy_strerror(errCode));
      Result := false;
    end
    else
    begin
      res := Trim(responseStream.DataString);
      Result := true;
    end;

    curl_easy_cleanup(handle);
  finally
    if headers <> nil then
      curl_slist_free_all(headers);
    responseStream.Free;
  end;
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable
  -----------------------------
  True when 'notify-send' exists on PATH.
 ------------------------------------------------------------------------------}
class function TTrndiNativeLinux.isNotificationSystemAvailable: boolean;
begin
  // Only treat gdbus as available when on KDE/GNOME-like desktops under Qt6
  if UseGDBusForNotifications then
    Exit(true);
  // Otherwise, rely on notify-send presence
  Result := IsNotifySendAvailable;
end;
{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Return 'gdbus' under Qt6 when gdbus is present; else 'notify-send' if found; else 'none'.
 ------------------------------------------------------------------------------}
class function TTrndiNativeLinux.getNotificationSystem: string;
begin
  if UseGDBusForNotifications then
    Exit('gdbus');
  if IsNotifySendAvailable then
    Exit('notify-send');
  Result := 'none';
end;


{------------------------------------------------------------------------------
  isDarkMode
  ----------
  Desktop-aware detection: KDE (kreadconfig5), GNOME (gsettings), GTK_THEME,
  or fallback luminance heuristic.
 ------------------------------------------------------------------------------}
class function TTrndiNativeLinux.isDarkMode: boolean;
var
  v: boolean;
  envGtkTheme: string;

function Brightness(C: TColor): double;
  begin
    Result := (Red(C) * 0.3) + (Green(C) * 0.59) + (Blue(C) * 0.11);
  end;

begin
  // 1) KDE Plasma: kreadconfig5 ColorScheme
  if DetectKDEDark(v) then
    Exit(v);

  // 2) GNOME: gsettings color-scheme/gtk-theme
  if DetectGnomeDark(v) then
    Exit(v);

  // 3) GTK_THEME environment variable (e.g., Adwaita:dark)
  envGtkTheme := EnvValue('GTK_THEME');
  if envGtkTheme <> '' then
    if ContainsDark(envGtkTheme) then
      Exit(true)
    else
      Exit(false);

  // 4) Last-resort heuristic using system colors
  Result := (Brightness(ColorToRGB(clWindow)) < Brightness(ColorToRGB(clWindowText)));
end;

{------------------------------------------------------------------------------
  ResolveIniPath
  --------------
  Always use Lazarus' app config file path (typically ~/.config/Trndi/trndi.ini).
 ------------------------------------------------------------------------------}
function TTrndiNativeLinux.ResolveIniPath: string;
begin
  Result := GetAppConfigFile(false);
end;

{------------------------------------------------------------------------------
  EnsureIni
  ---------
  Ensure the INI store is created and directory exists.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.EnsureIni;
var
  path: string;
begin
  if not Assigned(inistore) then
  begin
    path := ResolveIniPath;
    if ExtractFilePath(path) <> '' then
      ForceDirectories(ExtractFilePath(path));
    inistore := TIniFile.Create(path);
  end;
end;

{------------------------------------------------------------------------------
  attention
  ---------
  Send a desktop notification. Prefer gdbus path under Qt6; otherwise fallback.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.attention(topic, message: string);
{$IFDEF LCLQt6}
function RunAndCapture(const Exec: string; const Params: array of string;
  out StdoutS, StderrS: string; out ExitCode: integer): boolean;
  var
    P: TProcess;
    i: integer;
    OutStr, ErrStr: TStringStream;
    Buf: array[0..4095] of byte;
    n: SizeInt;
  begin
    Result := false;
    StdoutS := ''; StderrS := ''; ExitCode := -1;

    P := TProcess.Create(nil);
    OutStr := TStringStream.Create('');
    ErrStr := TStringStream.Create('');
    try
      P.Executable := Exec;
      for i := 0 to High(Params) do
        P.Parameters.Add(Params[i]);
      P.Options := [poUsePipes, poWaitOnExit];
      P.ShowWindow := swoHIDE;
      P.Execute;

      repeat
        while P.Output.NumBytesAvailable > 0 do
        begin
          n := P.Output.Read(Buf, SizeOf(Buf));
          if n > 0 then
            OutStr.WriteBuffer(Buf, n) else Break;
        end;
        while P.Stderr.NumBytesAvailable > 0 do
        begin
          n := P.Stderr.Read(Buf, SizeOf(Buf));
          if n > 0 then
            ErrStr.WriteBuffer(Buf, n) else Break;
        end;
        if not P.Running then
          Break;
        Sleep(5);
      until false;

      ExitCode := P.ExitStatus;
      StdoutS := OutStr.DataString;
      StderrS := ErrStr.DataString;
      Result := ExitCode = 0;
    finally
      ErrStr.Free; OutStr.Free; P.Free;
    end;
  end;
  {$ENDIF}
  {$IFDEF LCLQt6}
var
  Params: array of string;
  OutS, ErrS: string;
  ExitCode: integer;
  s: string;
  p, i: integer;
  NewId: cardinal;
  ReplaceId: cardinal;
  {$ENDIF}
begin
  {$IFDEF LCLQt6}
  if UseGDBusForNotifications then
  begin
    ReplaceId := 0;
    SetLength(Params, 0);
    Params :=
      ['call', '--session',
      '--dest', 'org.freedesktop.Notifications',
      '--object-path', '/org/freedesktop/Notifications',
      '--method', 'org.freedesktop.Notifications.Notify',
       '''Trndi''',
      IntToStr(ReplaceId),
      '''''',
      '''' + topic + '''',
      '''' + message + '''',
      '[]',
      '{}',
      IntToStr(noticeDuration)];
    // Above maps to: app_name, replace_id, app_icon, summary, body, actions, hints, timeout

    if RunAndCapture('gdbus', Params, OutS, ErrS, ExitCode) then
    begin
      // Parse the returned uint32 notification id (for potential replace/update)
      NewId := 0;
      s := OutS;
      p := Pos('uint32', s);
      if p > 0 then
      begin
        Inc(p, Length('uint32'));
        while (p <= Length(s)) and (s[p] = ' ') do
          Inc(p);
        i := p;
        while (i <= Length(s)) and (s[i] in ['0'..'9']) do
          Inc(i);
        if i > p then
          NewId := StrToIntDef(Copy(s, p, i - p), 0);
      end;
    end
    else
      inherited attention(topic, message);
  end
  else
    inherited attention(topic, message);
  {$ELSE}
  inherited attention(topic, message);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Destroy
  -------
  Clean up tray, badges, and INI store before inherited destructor.
 ------------------------------------------------------------------------------}
destructor TTrndiNativeLinux.Destroy;
begin
  if not noFree then
  begin
    ClearBadge;
    ShutdownBadge;
  end;
  if Assigned(TrayMenU) then
    TrayMenu.Free;
  if Assigned(Tray) then
    Tray.Free;
  if Assigned(FFlashTimer) then
    FreeAndNil(FFlashTimer);
  if Assigned(inistore) then
    inistore.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  FindInPath
  ----------
  Search PATH for a file name and return the first matching absolute path.
 ------------------------------------------------------------------------------}
function FindInPath(const FileName: string): string;
var
  PathVar, Dir: string;
  Paths: TStringList;
  i: integer;
begin
  Result := '';
  PathVar := GetEnvironmentVariable('PATH');
  Paths := TStringList.Create;
  try
    Paths.Delimiter := ':';
    Paths.StrictDelimiter := true;
    Paths.DelimitedText := PathVar;
    for i := 0 to Paths.Count - 1 do
    begin
      Dir := IncludeTrailingPathDelimiter(Paths[i]);
      if FileExists(Dir + FileName) then
        Exit(Dir + FileName);
    end;
  finally
    Paths.Free;
  end;
end;

{------------------------------------------------------------------------------
  GetSystemLangTag
  ----------------
  Return a BCP 47-like language tag from environment (e.g., sv-SE).
 ------------------------------------------------------------------------------}
function GetSystemLangTag: string;

function FirstSegment(const S, Sep: string): string;
  var
    P: SizeInt;
  begin
    Result := S;
    P := Pos(Sep, Result);
    if P > 0 then
      Result := Copy(Result, 1, P - 1);
  end;

var
  L: string;
  P: SizeInt;
begin
  L := GetEnvironmentVariable('LC_ALL');
  if L = '' then
    L := GetEnvironmentVariable('LANGUAGE');
  if L = '' then
    L := GetEnvironmentVariable('LANG');

  if L = '' then
    Exit('');

  L := FirstSegment(L, ':');

  P := Pos('.', L);
  if P > 0 then
    L := Copy(L, 1, P - 1);

  L := StringReplace(L, '_', '-', [rfReplaceAll]);

  L := LowerCase(L);
  P := Pos('-', L);
  if P > 0 then
    L := Copy(L, 1, P) + UpperCase(Copy(L, P + 1, MaxInt));

  Result := L;
end;

{------------------------------------------------------------------------------
  Speak
  -----
  Use spd-say to speak the provided text in the current system language.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.Speak(const Text: string);
var
  CmdPath, Lang: string;
  Proc: TProcess;
begin
  CmdPath := FindInPath('spd-say');
  if CmdPath = '' then
  begin
    ShowMessage('Error: spd-say is not installed.');
    Exit;
  end;

  Lang := GetSystemLangTag;

  Proc := TProcess.Create(nil);
  try
    Proc.Executable := CmdPath;
    if Lang <> '' then
      Proc.Parameters.AddStrings(['-l', Lang])
    else
    ;

    Proc.Parameters.Add('--');
    Proc.Parameters.Add(Text);

    Proc.Options := [];
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;


procedure TTrndiNativeLinux.trayClick(Sender: TObject);
begin
  Application.mainform.hide;
  Application.mainform.Show;
  Application.MainForm.BringToFront;
  Application.MainForm.SetFocus;
end;

{------------------------------------------------------------------------------
  SetTray
  -------
  Draw a badge on the system tray icon and synchronize KDE taskbar badge.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.SetTray(const Value: string; BadgeColor: TColor;
badge_size_ratio: double = 0.8; min_font_size: integer = 8);
const
  INITIAL_FONT_SIZE_RATIO = 0.5;
  TEXT_PADDING = 3;
  CORNER_RADIUS = 6;
var
  BaseIcon, OutIcon: TIcon;
  Bmp: TBitmap;
  W, H, BadgeSize, Radius: integer;
  BadgeRect: TRect;
  TextW, TextH: integer;
  FontSize: integer;
  TextColor: TColor;
  rgb: longint;
  r, g, b: byte;
  BadgeText: string;
  dval: double;
begin
  // Ensure we have a tray icon instance
  if not Assigned(Tray) then
  begin
    Tray := TTrayIcon.Create(Application.MainForm);
    tray.OnClick := @trayClick;
    TrayMenu := TPopupMenu.Create(tray);
    tray.PopUpMenu := traymenu;
    traymenu.Items.add(TMenuItem.Create(tray));
    with TrayMenu.Items[0] do
    begin
      Caption := 'Trndi';
      onclick := @trayClick;
    end;
  end;

  if Value = '' then
  begin
    // Reset to app icon and force refresh
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      Tray.Icon.Assign(Application.Icon);
    // Toggle visibility to force redraw in some tray implementations
    Tray.Visible := false;
    Tray.Visible := true;
    Exit;
  end;

  try
    if TryStrToFloat(Value, dval, fsettings) then
      BadgeText := FormatFloat('0.0', dval, fsettings)
    else
      BadgeText := Value;
  except
    BadgeText := Value;
  end;

  BaseIcon := TIcon.Create;
  OutIcon := TIcon.Create;
  Bmp := TBitmap.Create;
  try
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      BaseIcon.Assign(Application.Icon)
    else
      BaseIcon.SetSize(24, 24);

    W := BaseIcon.Width;
    H := BaseIcon.Height;
    if (W <= 0) or (H <= 0) then
    begin
      W := 24;
      H := 24;
    end;

    // Badge occupies a fraction of the icon's smallest side
    BadgeSize := Round(Min(W, H) * badge_size_ratio);
    if BadgeSize < 10 then
      BadgeSize := 10;

    Bmp.SetSize(W, H);
    Bmp.PixelFormat := pf32bit;

    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.Brush.Color := clNone;
    Bmp.Canvas.FillRect(Rect(0, 0, W, H));
    Bmp.Canvas.Draw(0, 0, BaseIcon);

    BadgeRect := Rect(W - BadgeSize, H - BadgeSize, W, H);

    rgb := ColorToRGB(BadgeColor);
    r := byte(rgb);
    g := byte(rgb shr 8);
    b := byte(rgb shr 16);
    if (0.299 * r + 0.587 * g + 0.114 * b) > 128 then
      TextColor := clBlack
    else
      TextColor := clWhite;

    Bmp.Canvas.Brush.Color := BadgeColor;
    Bmp.Canvas.Pen.Color := BadgeColor;

    if BadgeSize <= 12 then
      Bmp.Canvas.FillRect(BadgeRect)
    else
    begin
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then
        Radius := 2;

      Bmp.Canvas.RoundRect(
        BadgeRect.Left, BadgeRect.Top,
        BadgeRect.Right, BadgeRect.Bottom,
        Radius * 2, Radius * 2
        );

      Bmp.Canvas.FillRect(
        Rect(BadgeRect.Right - Radius, BadgeRect.Bottom - Radius,
        BadgeRect.Right, BadgeRect.Bottom)
        );
    end;

    Bmp.Canvas.Font.Name := 'DejaVu Sans';
    Bmp.Canvas.Font.Style := [fsBold];
    Bmp.Canvas.Font.Color := TextColor;

    FontSize := Round(BadgeSize * INITIAL_FONT_SIZE_RATIO);
    if FontSize < min_font_size then
      FontSize := min_font_size;
    Bmp.Canvas.Font.Size := FontSize;

    TextW := Bmp.Canvas.TextWidth(BadgeText);
    TextH := Bmp.Canvas.TextHeight(BadgeText);

    // Fit text without going smaller than minimum font size
    while (TextW > (BadgeSize - TEXT_PADDING)) and (FontSize > min_font_size) do
    begin
      Dec(FontSize);
      Bmp.Canvas.Font.Size := FontSize;
      TextW := Bmp.Canvas.TextWidth(BadgeText);
      TextH := Bmp.Canvas.TextHeight(BadgeText);
    end;

    Bmp.Canvas.Brush.Style := bsClear;
    Bmp.Canvas.TextOut(
      BadgeRect.Left + ((BadgeRect.Right - BadgeRect.Left) - TextW) div 2,
      BadgeRect.Top + ((BadgeRect.Bottom - BadgeRect.Top) - TextH) div 2,
      BadgeText
      );

    OutIcon.Assign(Bmp);
    Tray.Icon.Assign(OutIcon);
    Tray.Visible := false;
    Tray.Visible := true;
  finally
    Bmp.Free;
    OutIcon.Free;
    BaseIcon.Free;
  end;
end;

{------------------------------------------------------------------------------
  SetBadge (advanced)
  -------------------
  Update KDE taskbar badge and tray icon badge with size/font options.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.SetBadge(const Value: string; BadgeColor: TColor;
badge_size_ratio: double; min_font_size: integer);
var
  f: double;
begin
  if KDEBadge.GDesktopId = '' then
    InitializeBadge('com.slicke.trndi.desktop', 150, nil);
  ClearBadge;
  
  // Only set numeric badge if value can be parsed as a number
  // For placeholders like '--', clear the badge instead
  if TryStrToFloat(Value, f) then
    KDEBadge.SetBadge(f);
  // If TryStrToFloat fails, badge stays cleared (ClearBadge above)
  
  SetTray(Value, badgecolor, badge_size_ratio, min_font_size);
end;

// Overload: delegate to the full implementation with default parameters
procedure TTrndiNativeLinux.SetBadge(const Value: string; BadgeColor: TColor);
begin
  SetBadge(Value, BadgeColor, DEFAULT_BADGE_SIZE_RATIO, DEFAULT_MIN_FONT_SIZE);
end;

// Flash timer tick: pulse brightness (simple 4-phase like Windows)
procedure TTrndiNativeLinux.FlashTimerTick(Sender: TObject);

function ScaleColor(c: TColor; factor: double): TColor;
  var
    rc: longint;
    r, g, b: integer;
  begin
    rc := ColorToRGB(c);
    r := Round(GetRValue(rc) * factor);
    if r > 255 then
      r := 255;
    g := Round(GetGValue(rc) * factor);
    if g > 255 then
      g := 255;
    b := Round(GetBValue(rc) * factor);
    if b > 255 then
      b := 255;
    Result := RGB(r, g, b);
  end;

var
  factor: double;
  phaseColor: TColor;
begin
  if (Now > FFlashEnd) or (FFlashValue = '') then
  begin
    StopBadgeFlash;
    Exit;
  end;
  case FFlashPhase mod 4 of
  0:
    factor := 1.0;
  1:
    factor := 1.35;
  2:
    factor := 1.0;
  3:
    factor := 0.7;
  else
    factor := 1.0;
  end;
  phaseColor := ScaleColor(FFlashBaseColor, factor);
  // Only tray icon animates; KDE numeric badge stays stable for clarity
  SetTray(FFlashValue, phaseColor, DEFAULT_BADGE_SIZE_RATIO, DEFAULT_MIN_FONT_SIZE);
  Inc(FFlashPhase);
end;

procedure TTrndiNativeLinux.StartBadgeFlash(const Value: string;
badgeColor: TColor; DurationMS: integer; CycleMS: integer);
begin
  FFlashValue := Value;
  FFlashBaseColor := badgeColor;
  FFlashCycleMS := CycleMS;
  FFlashEnd := Now + (DurationMS / (24 * 60 * 60 * 1000));
  FFlashPhase := 0;
  if FFlashTimer = nil then
  begin
    FFlashTimer := TTimer.Create(nil);
    FFlashTimer.OnTimer := @FlashTimerTick;
  end;
  FFlashTimer.Interval := CycleMS;
  FFlashTimer.Enabled := true;
  FlashTimerTick(nil); // immediate first frame
end;

procedure TTrndiNativeLinux.StopBadgeFlash;
begin
  if Assigned(FFlashTimer) then
  begin
    FFlashTimer.Enabled := false;
    FreeAndNil(FFlashTimer);
  end;
  if FFlashValue <> '' then
    SetTray(FFlashValue, FFlashBaseColor, DEFAULT_BADGE_SIZE_RATIO,
      DEFAULT_MIN_FONT_SIZE);
  FFlashValue := '';
end;
{------------------------------------------------------------------------------
  setDarkMode
  -----------
  Placeholder; currently returns False on Linux.
 ------------------------------------------------------------------------------}
class function TTrndiNativeLinux.setDarkMode: boolean;
begin
{------------------------------------------------------------------------------
  setDarkMode
  -----------
  Placeholder; currently returns False on Linux.
 ------------------------------------------------------------------------------}
  // Placeholder: switching dark mode programmatically is DE-specific and not supported here.
  // Return False to indicate no change was made.
  Result := false;
end;

{------------------------------------------------------------------------------
  Settings
  --------
  Read/write settings using Lazarus app config file under a single [trndi] section.
 ------------------------------------------------------------------------------}
function TTrndiNativeLinux.GetSetting(const keyname: string; def: string;
global: boolean): string;
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  Result := inistore.ReadString('trndi', key, def);
end;

{------------------------------------------------------------------------------
  SetSetting
  ----------
  Write setting to canonical [trndi] section and flush to disk.
  NOTE: we use [trndi] as the format in the win registry and macOS are flat
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.SetSetting(const keyname: string;
const val: string; global: boolean);
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  // Write under a canonical section
  inistore.WriteString('trndi', key, val);
  inistore.UpdateFile;
end;

{------------------------------------------------------------------------------
  DeleteSetting
  -------------
  Delete setting across known sections for completeness.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.DeleteSetting(const keyname: string; global: boolean);
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  inistore.DeleteKey('trndi', key);
  inistore.UpdateFile;
end;

{------------------------------------------------------------------------------
  ReloadSettings
  --------------
  Drop INI handle; it will be lazily re-created on next access.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeLinux.ReloadSettings;
begin
  FreeAndNil(inistore);
  // will be recreated on next access
end;

end.
