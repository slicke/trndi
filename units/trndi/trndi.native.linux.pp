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

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Graphics, fphttpclient, openssl, opensslsockets, IniFiles, Dialogs,
  ExtCtrls, Forms, Math, LCLIntf, KDEBadge, trndi.native.base, FileUtil;

type
  {!
    @abstract(Linux implementation of @link(TTrndiNativeBase).)
    Uses spd-say for speech and draws badges on tray/KDE taskbar.
  }
  TTrndiNativeLinux = class(TTrndiNativeBase)
  protected
    Tray: TTrayIcon;
    inistore: TIniFile; // Linux-specific settings store
    function ResolveIniPath: string; virtual;
    procedure EnsureIni; inline;
  public
  {** Prefer gdbus notifications under Qt6; fallback to base attention. }
  procedure attention(topic, message: string); override;
    destructor Destroy; override;
    {** Speaks @param(Text) using spd-say, if available.
        Shows a user-visible message when the tool is not present. }
  {** Speak text via spd-say if present; warn user if missing. }
  procedure Speak(const Text: string); override;
    {** Draw a badge with @param(Value) text on tray icon using @param(BadgeColor).
        @param(badge_size_ratio Determines badge diameter relative to icon size)
        @param(min_font_size Lower bound for font size while fitting text) }
  {** Draw a badge on the tray icon. }
  procedure SetTray(const Value: string; BadgeColor: TColor; badge_size_ratio: double = 0.8; min_font_size: integer = 8);
    {** Convenience overload: delegates to base 2-arg SetBadge. }
  {** Convenience overload redirects to base two-arg version. }
  procedure setBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
    {** Synchronize KDE badge with numeric value and update tray badge drawing. }
    procedure setBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer); overload; override;
    {** Placeholder; desktop environments differ. Implement where feasible. }
  {** Placeholder for desktop-specific dark mode. }
  class function setDarkMode: boolean; // no-op placeholder

    // Settings API overrides
  {** Read setting from INI (multi-section + legacy key=value fallback). }
  function GetSetting(const keyname: string; def: string = ''; global: boolean = false): string; override;
  {** Write setting to canonical [trndi] section and flush to disk. }
  procedure SetSetting(const keyname: string; const val: string; global: boolean = false); override;
  {** Delete setting across known sections for completeness. }
  procedure DeleteSetting(const keyname: string; global: boolean = false); override;
  {** Drop INI handle; re-created on demand. }
  procedure ReloadSettings; override;
  {** Simple HTTP GET using FPC HTTP client with default UA. }
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
  {** True if notify-send is available on this system. }
  class function isNotificationSystemAvailable: boolean; override;
  end;

implementation

uses
  Process, Types, LCLType;

{** Check PATH for the 'notify-send' tool. }
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

{** Run an external command and capture stdout. Returns True on exit code 0. }
function RunAndCaptureSimple(const Exec: string; const Params: array of string;
                             out StdoutS: string; out ExitCode: Integer): Boolean;
var
  P: TProcess;
  i: Integer;
  OutStr: TStringStream;
  Buf: array[0..4095] of byte;
  n: SizeInt;
begin
  Result := False;
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
          if n > 0 then OutStr.WriteBuffer(Buf, n) else Break;
        end;
        Sleep(3);
      end;
      // Drain any remaining bytes after process exits
      while P.Output.NumBytesAvailable > 0 do
      begin
        n := P.Output.Read(Buf, SizeOf(Buf));
        if n > 0 then OutStr.WriteBuffer(Buf, n) else Break;
      end;
      ExitCode := P.ExitStatus;
      StdoutS := Trim(OutStr.DataString);
      Result := ExitCode = 0;
    except
      on E: Exception do
      begin
        StdoutS := '';
        ExitCode := -1;
        Result := False;
      end;
    end;
  finally
    OutStr.Free;
    P.Free;
  end;
end;

function EnvValue(const Name: string): string; inline;
begin
  Result := GetEnvironmentVariable(Name);
end;

function DesktopHint: string;
begin
  Result := EnvValue('XDG_CURRENT_DESKTOP');
  if Result = '' then
    Result := EnvValue('DESKTOP_SESSION');
end;

function ContainsDark(const S: string): boolean; inline;
begin
  Result := Pos('dark', LowerCase(S)) > 0;
end;

// Forward declaration for helper declared later in this unit
function FindInPath(const FileName: string): string; forward;

function DetectGnomeDark(out isDark: boolean): boolean;
var
  gsettingsPath, outS: string;
  exitCode: Integer;
  dHint: string;
begin
  Result := False;
  isDark := False;
  dHint := LowerCase(DesktopHint);
  if (Pos('gnome', dHint) = 0) and (Pos('ubuntu', dHint) = 0) and (Pos('unity', dHint) = 0) then
  begin
    // Not obviously GNOME; still proceed if gsettings exists
  end;
  gsettingsPath := FindInPath('gsettings');
  if gsettingsPath = '' then Exit(False);

  // GNOME 42+: color-scheme prefer-dark/default
  if RunAndCaptureSimple(gsettingsPath,
     ['get','org.gnome.desktop.interface','color-scheme'], outS, exitCode) and (exitCode = 0) then
  begin
    outS := LowerCase(StringReplace(outS, '''', '', [rfReplaceAll]));
    if Pos('prefer-dark', outS) > 0 then
    begin
      isDark := True; Exit(True);
    end
    else if (Pos('default', outS) > 0) or (Pos('prefer-light', outS) > 0) then
    begin
      isDark := False; Exit(True);
    end;
    // fallthrough to gtk-theme
  end;

  // Fallback: inspect gtk-theme name for '*-dark'
  if RunAndCaptureSimple(gsettingsPath,
     ['get','org.gnome.desktop.interface','gtk-theme'], outS, exitCode) and (exitCode = 0) then
  begin
    outS := LowerCase(StringReplace(outS, '''', '', [rfReplaceAll]));
    if ContainsDark(outS) then
      isDark := True
    else
      isDark := False;
    Exit(True);
  end;

  Result := False; // unable to determine via GNOME
end;

function DetectKDEDark(out isDark: boolean): boolean;
var
  kreadPath, outS: string;
  exitCode: Integer;
  dHint: string;
begin
  Result := False; isDark := False;
  dHint := LowerCase(DesktopHint);
  if (Pos('kde', dHint) = 0) and (Pos('plasma', dHint) = 0) then
  begin
    // Not obviously KDE; continue if tool exists
  end;
  kreadPath := FindInPath('kreadconfig5');
  if kreadPath = '' then Exit(False);

  // Read the active color scheme
  if RunAndCaptureSimple(kreadPath,
     ['--group','General','--key','ColorScheme'], outS, exitCode) and (exitCode = 0) then
  begin
    if ContainsDark(outS) then
      isDark := True
    else
      isDark := False;
    Exit(True);
  end;

  Result := False;
end;
{
  Linux/PC implementation of class function getURL
}
class function TTrndiNativeLinux.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  client: TFPHttpClient;
  responseStream: TStringStream;
begin
  res := '';
  client := TFPHttpClient.Create(nil);
  responseStream := TStringStream.Create('');
  try
    try
      client.AddHeader('User-Agent', DEFAULT_USER_AGENT);
      client.Get(url, responseStream);
      res := Trim(responseStream.DataString);
      Result := true;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
      end;
    end;
  finally
    client.Free;
    responseStream.Free;
  end;
end;

class function TTrndiNativeLinux.isNotificationSystemAvailable: boolean;
begin
  Result := IsNotifySendAvailable;
end;


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
  if DetectKDEDark(v) then Exit(v);

  // 2) GNOME: gsettings color-scheme/gtk-theme
  if DetectGnomeDark(v) then Exit(v);

  // 3) GTK_THEME environment variable (e.g. Adwaita:dark)
  envGtkTheme := EnvValue('GTK_THEME');
  if envGtkTheme <> '' then
  begin
    if ContainsDark(envGtkTheme) then Exit(True) else Exit(False);
  end;

  // 4) Last-resort heuristic using system colors
  Result := (Brightness(ColorToRGB(clWindow)) < Brightness(ColorToRGB(clWindowText)));
end;
function TTrndiNativeLinux.ResolveIniPath: string;
var
  home, pA, pB, pC: string;
begin
  // Prefer existing files in this order:
  // 1) Lazarus app config file (typically ~/.config/Trndi/trndi.ini)
  // 2) Explicit ~/.config/Trndi/trndi.ini
  // 3) Legacy ~/.config/Trndi.cfg
  pA := GetAppConfigFile(False);

  home := GetEnvironmentVariable('HOME');
  if home = '' then
    home := GetUserDir; // Fallback
  pC := IncludeTrailingPathDelimiter(home) + '.config' + DirectorySeparator + 'Trndi.cfg';
  pB := IncludeTrailingPathDelimiter(home) + '.config' + DirectorySeparator + 'Trndi' + DirectorySeparator + 'trndi.ini';

  if (pA <> '') and FileExists(pA) then Exit(pA);
  if FileExists(pB) then Exit(pB);
  if FileExists(pC) then Exit(pC);

  // Nothing exists; default to Lazarus app config file path
  Result := pA;
  if Result = '' then
    Result := pB; // reasonable default under ~/.config/Trndi/trndi.ini
end;

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


procedure TTrndiNativeLinux.attention(topic, message: string);
{$IFDEF LCLQt6}
  function RunAndCapture(const Exec: string; const Params: array of string;
                         out StdoutS, StderrS: string; out ExitCode: Integer): Boolean;
  var
    P: TProcess;
    i: Integer;
    OutStr, ErrStr: TStringStream;
    Buf: array[0..4095] of byte;
    n: SizeInt;
  begin
    Result := False;
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
          if n > 0 then OutStr.WriteBuffer(Buf, n) else Break;
        end;
        while P.Stderr.NumBytesAvailable > 0 do
        begin
          n := P.Stderr.Read(Buf, SizeOf(Buf));
          if n > 0 then ErrStr.WriteBuffer(Buf, n) else Break;
        end;
        if not P.Running then Break;
        Sleep(5);
      until False;

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
  ExitCode: Integer;
  s: string;
  p, i: Integer;
  NewId: Cardinal;
  ReplaceId: Cardinal;
{$ENDIF}
begin
{$IFDEF LCLQt6}
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
      while (p <= Length(s)) and (s[p] = ' ') do Inc(p);
      i := p;
      while (i <= Length(s)) and (s[i] in ['0'..'9']) do Inc(i);
      if i > p then
        NewId := StrToIntDef(Copy(s, p, i - p), 0);
    end;
  end
  else
    // Fall back to base attention implementation if gdbus is unavailable
    inherited attention(topic, message);
{$ELSE}
  inherited attention(topic, message);
{$ENDIF}
end;
destructor TTrndiNativeLinux.Destroy;
begin
  if not noFree then
  begin
    ClearBadge;
    ShutdownBadge;
  end;
  if Assigned(Tray) then
    Tray.Free;
  if Assigned(inistore) then
    inistore.Free;
  inherited Destroy;
end;


function FindInPath(const FileName: string): string;
var
  PathVar, Dir: string;
  Paths: TStringList;
  i: Integer;
begin
  Result := '';
  PathVar := GetEnvironmentVariable('PATH');
  Paths := TStringList.Create;
  try
    Paths.Delimiter := ':';
    Paths.StrictDelimiter := True;
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

function GetSystemLangTag: string;
  function FirstSegment(const S, Sep: string): string;
  var P: SizeInt;
  begin
    Result := S;
    P := Pos(Sep, Result);
    if P > 0 then
      Result := Copy(Result, 1, P-1);
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
    L := Copy(L, 1, P-1);

  L := StringReplace(L, '_', '-', [rfReplaceAll]);

  L := LowerCase(L);
  P := Pos('-', L);
  if P > 0 then
    L := Copy(L, 1, P) + UpperCase(Copy(L, P+1, MaxInt));

  Result := L;
end;

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

procedure TTrndiNativeLinux.SetTray(const Value: string; BadgeColor: TColor;
                       badge_size_ratio: double = 0.8; min_font_size: integer = 8);
const
  INITIAL_FONT_SIZE_RATIO = 0.5;
  TEXT_PADDING = 3;
  CORNER_RADIUS = 6;
var
  BaseIcon, OutIcon: TIcon;
  Bmp: TBitmap;
  W, H, BadgeSize, Radius: Integer;
  BadgeRect: TRect;
  TextW, TextH: Integer;
  FontSize: Integer;
  TextColor: TColor;
  rgb: LongInt;
  r, g, b: Byte;
  BadgeText: string;
  dval: double;
begin
  // Ensure we have a tray icon instance
  if not Assigned(Tray) then
    Tray := TTrayIcon.Create(Application.MainForm);

  if Value = '' then
  begin
    // Reset to app icon and force refresh
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      Tray.Icon.Assign(Application.Icon);
    // Toggle visibility to force redraw in some tray implementations
    Tray.Visible := False;
    Tray.Visible := True;
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
  OutIcon  := TIcon.Create;
  Bmp      := TBitmap.Create;
  try
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      BaseIcon.Assign(Application.Icon)
    else
      BaseIcon.SetSize(24, 24);

    W := BaseIcon.Width;
    H := BaseIcon.Height;
    if (W <= 0) or (H <= 0) then
    begin
      W := 24; H := 24;
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
    r := Byte(rgb);
    g := Byte(rgb shr 8);
    b := Byte(rgb shr 16);
    if (0.299*r + 0.587*g + 0.114*b) > 128 then
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
        Rect(BadgeRect.Right - Radius, BadgeRect.Bottom - Radius, BadgeRect.Right, BadgeRect.Bottom)
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
      BadgeRect.Top  + ((BadgeRect.Bottom - BadgeRect.Top) - TextH) div 2,
      BadgeText
    );

    OutIcon.Assign(Bmp);
    Tray.Icon.Assign(OutIcon);
    Tray.Visible := False;
    Tray.Visible := True;
  finally
    Bmp.Free;
    OutIcon.Free;
    BaseIcon.Free;
  end;
end;

procedure TTrndiNativeLinux.SetBadge(const Value: string; BadgeColor: TColor;
                       badge_size_ratio: double; min_font_size: integer); 
var
  f: double;
begin
  f := 0.0;
  TryStrToFloat(value, f);
  if KDEBadge.GDesktopId = '' then
    InitializeBadge('com.slicke.trndi.desktop', 150, nil);
  ClearBadge;
  KDEBadge.SetBadge(f);
  SetTray(value,badgecolor,badge_size_ratio, min_font_size);
end;

procedure TTrndiNativeLinux.SetBadge(const Value: string; BadgeColor: TColor);
begin
  inherited SetBadge(Value, BadgeColor);
end;
class function TTrndiNativeLinux.setDarkMode: Boolean;
begin
  // Placeholder: switching dark mode programmatically is DE-specific and not supported here.
  // Return False to indicate no change was made.
  Result := False;
end;

function TTrndiNativeLinux.GetSetting(const keyname: string; def: string; global: boolean): string;
var
  key: string;
  raw: TStringList;
  i, p: Integer;
  line, k, v: string;
  path: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  // Try common sections used historically (backward compatibility)
  Result := inistore.ReadString('trndi', key, '');
  if Result = '' then
    Result := inistore.ReadString('settings', key, '');
  if Result = '' then
    Result := inistore.ReadString('Trndi', key, '');
  // Legacy .cfg files may have no sections. Parse raw lines as key=value.
  if Result = '' then
  begin
    raw := TStringList.Create;
    try
      // Read from the file backing inistore
      path := ResolveIniPath;
      if FileExists(path) then
      begin
        raw.LoadFromFile(path);
        for i := 0 to raw.Count-1 do
        begin
          line := Trim(raw[i]);
          if (line = '') or (line[1] = '#') or (line[1] = ';') then Continue;
          p := Pos('=', line);
          if p > 0 then
          begin
            k := Trim(Copy(line, 1, p-1));
            v := Trim(Copy(line, p+1, MaxInt));
            // Match either fully built key or plain keyname (no user prefix)
            // so older files without username scoping still load.
            if SameText(k, key) or SameText(k, keyname) then
            begin
              Result := v;
              Break;
            end;
          end;
        end;
      end;
    finally
      raw.Free;
    end;
  end;
  if Result = '' then
    Result := def;
end;

procedure TTrndiNativeLinux.SetSetting(const keyname: string; const val: string; global: boolean);
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  // Write under a canonical section
  inistore.WriteString('trndi', key, val);
  inistore.UpdateFile;
end;

procedure TTrndiNativeLinux.DeleteSetting(const keyname: string; global: boolean);
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  inistore.DeleteKey('trndi', key);
  // Also try alternative sections to be thorough
  inistore.DeleteKey('settings', key);
  inistore.DeleteKey('Trndi', key);
  inistore.UpdateFile;
end;

procedure TTrndiNativeLinux.ReloadSettings;
begin
  FreeAndNil(inistore);
  // will be recreated on next access
end;

end.
