(*
 * This file is part of Trndi (https://github.com/slicke/trndi or http://xxx.github.io).
 * Copyright (c) 2021-25 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

{**
  This base unit defines platform-neutral contracts for native features and
  holds small, safe helpers that can work across platforms. The heavy lifting
  is done in platform units (win/linux/mac) to keep this unit almost free of
  ifdefs. Avoid adding platform-specific code here.
}

unit trndi.native.base;

{$I ../../inc/native.inc}// Depending on your project setup, this may define X_WIN, X_PC, X_MAC, etc.

interface

{**
  @abstract(Base, platform-agnostic contract for native integrations.)

  This unit contains @link(TTrndiNativeBase), an abstract class that defines
  platform-neutral contracts for native features. Concrete implementations live
  in @code(trndi.native.win), @code(trndi.native.linux), and
  @code(trndi.native.mac). Use the façade unit @code(trndi.native) which exposes
  the alias @code(TrndiNative) resolving to the right subclass at compile time.

  Design rules for contributors:
  - Keep this unit free from platform ifdefs as much as possible.
  - Add new cross-platform contracts here; implement per-platform logic in
    platform units.
  - Prefer virtual methods (instance or class) for behavior that varies by
    platform.
}

uses
Classes, SysUtils, Graphics
{$IF DEFINED(X_MAC)}
, NSMisc, ns_url_request, CocoaAll, LCLType
{$ELSEIF DEFINED(X_WIN)}
, Windows, Registry, Dialogs, StrUtils, winhttpclient, shellapi, comobj,
Forms, variants, dwmapi
{$ELSEIF DEFINED(X_PC)}
, libpascurl, Dialogs, LCLType
{$ENDIF}
, process;

type
  {** WSL version detection result. }
TWSLVersion = (wslNone, wslVersion1, wslVersion2, wslUnknown);
  {** Ternary-style boolean with Unset/Unknown states for user overrides. }
TTrndiBool = (tbUnset, tbTrue, tbFalse, tbUnknown);

  {** Information about a WSL environment (Windows Subsystem for Linux). }
TWSLInfo = record
  IsWSL: boolean;       // True when running under WSL
  Version: TWSLVersion; // Detected version (1 or 2) when applicable
  DistroName: string;   // Optional distro name (if available)
  KernelVersion: string;// Kernel string as reported by /proc/version
end;
  // TrndiNative base contracts
{**
  @abstract(Base class providing contracts for native features.)
  Note: Implement platform-specific behavior in subclasses. Keep the API
  stable; avoid leaking OS details into the base.
}
TTrndiNativeBase = class
private
  cfguser: string;  // User prefix for config

  procedure updateLocale(const l: TFormatSettings);
protected
  fsettings: TFormatSettings;
    // HTTP defaults
  useragent: string;
  baseurl: string;
    // Note: Platform-specific settings storage (e.g., INI on Linux) is managed
    // by each platform unit, not the base class.
  {** Build a storage key. If not @param(global), prepend the current
    @code(configUser) with an underscore. }
  function buildKey(const key: string; global: boolean): string;
public
    // Config
  noFree: boolean;
  noticeDuration: integer;
class var touchOverride: TTrndiBool;
    // Indicates if the user system is in a "dark mode" theme
  dark: boolean;

    // Core actions
    {** Speak text using native TTS on the current platform. }
  procedure Speak(const Text: string); virtual; abstract;
    {** Show a desktop notification or equivalent attention cue. }
  procedure attention(topic, message: string); virtual;
  {**
    Send an HTTP request to the configured @code(baseurl).
    @param(post) True for POST, False for GET.
    @param(endpoint) Relative endpoint to append to baseurl.
    @param(params) Query string pairs like 'key=value' (GET or POST).
    @param(jsondata) Optional JSON payload for POST.
    @param(header) Optional single header string 'Key=Value'.
    @param(prefix) True to prefix endpoint with baseurl.
    @returns(Response body or error message.)
  }
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''; prefix: boolean = true): string;

    // Settings API
    {** Store a non-user-scoped key (global). }
  procedure SetRootSetting(keyname: string; const val: string);
    {** Store a string value under @param(keyname). @param(global) bypasses user scoping. }
  procedure SetSetting(const keyname: string; const val: string;
    global: boolean = false); virtual; abstract;
    {** Store a boolean setting (serialized as 'true'/'false'). }
  procedure SetBoolSetting(const keyname: string; const val: boolean);
    {** Store a float setting (using '.' decimal separator). }
  procedure SetFloatSetting(const keyname: string; const val: single);
    {** Store a color value (TColor serialized as integer). }
  procedure SetColorSetting(const keyname: string; val: TColor);
    {** Store a WideChar (WideChar serialized as string). }
  procedure SetWideCharSetting(const keyname: string; val: widechar);
    {** Retrieve a stored color or @param(def) if missing. }
  function GetColorSetting(const keyname: string; const def: TColor = $000000): TColor;
    {** Retrieve a stored WideChar or @param(def) if missing. }
  function GetWideCharSetting(const keyname: string;
    const def: widechar = widechar($2B24)): widechar;
    {** Delete a key (optionally global) from storage. }
  procedure DeleteSetting(const keyname: string; global: boolean = false);
    virtual; abstract;
    {** Delete a non-user-scoped key. }
  procedure DeleteRootSetting(keyname: string);
    {** Read a non-user-scoped key or default. }
  function GetRootSetting(const keyname: string; def: string = ''): string;
    {** Read a string setting or default. Empty is a valid value, @link(GetSettingEx) handles empty values as default; honor @param(global) scoping. }
  function GetSetting(const keyname: string; def: string = '';
    global: boolean = false): string; virtual; abstract;
    {** Read a char setting (first character), or @param(def). }
  function GetCharSetting(const keyname: string; def: char = #0): char;
    {** Read an integer setting or @param(def). }
  function GetIntSetting(const keyname: string; def: integer = -1): integer;
    {** Read a single-precision float setting or @param(def). }
  function GetFloatSetting(const keyname: string; def: single = -1): single;
    {** Read a boolean setting or @param(def). Accepts 'true'/'false'. }
  function GetBoolSetting(const keyname: string; def: boolean = false): boolean;
    {** Reload settings backend state (if any). }
  procedure ReloadSettings; virtual; abstract;
    {** Read setting, empty will return default. }
  function GetSettingEx(const keyname: string; def: string = '';
    global: boolean = false): string;
    {** Compare a setting to a value. }
  function CheckSetting(const keyname: string; def: string; match: string;
    global: boolean = false): boolean; virtual;

    // Theme/Env
    {** Determine if the OS/theme uses a dark appearance. Platforms override. }
  class function isDarkMode: boolean; virtual;
  class function DetectTouchScreen(out multi: boolean): boolean;
    {** Detect if the device has a touchscreen and whether it's multi-touch. }
  class function HasTouchScreen(out multi: boolean): boolean;
  class function HasTouchScreen: boolean;
    {** Simple HTTP GET helper; platform units implement. }
  class function getURL(const url: string; out res: string): boolean; virtual; abstract;
  class function GetOSLanguage: string;
  class function HasDangerousChars(const FileName: string): boolean; static;
  class function DetectWSL: TWSLInfo;
    // Notifications
    {** True if a native notification system is available (override per platform). }
  class function isNotificationSystemAvailable: boolean; virtual;
  {** Identify the notification backend in use on this platform.
      Examples: 'notify-send', 'gdbus', 'BurntToast', 'NSUserNotification', 'none', 'unknown'.
      Platforms should override to provide a concrete value. }
  class function getNotificationSystem: string; virtual;
    {** Alias for readability: forwards to @link(isNotificationSystemAvailable). }
  class function HasNotifications: boolean;

    // Lifecycle and UI
  destructor Destroy; override;
    {** Optional startup hook; platform units may override. }
  procedure start;
    {** Optional shutdown hook; platform units may override. }
  procedure done;
    // Badge: provide a convenience overload and a virtual full version
    {** Convenience overload for drawing a badge on the app icon/tray. }
  procedure setBadge(const Value: string; badgeColor: TColor); overload;
    {** Platform override for badge rendering control. }
  procedure setBadge(const Value: string; badgeColor: TColor;
    badge_size_ratio: double; min_font_size: integer); virtual; overload;
    {** Start flashing the badge (platform may animate icon). Base no-op. }
  procedure StartBadgeFlash(const Value: string; badgeColor: TColor;
    DurationMS: integer = 10000; CycleMS: integer = 400); virtual;
    {** Stop any active badge flashing and restore a static badge. Base no-op. }
  procedure StopBadgeFlash; virtual;
    {** Set native window titlebar colors if supported. }
  class function SetTitleColor(form: THandle; bg, Text: TColor): boolean; virtual;
    {** Play an audio file using native facilities (safe file check included). }
  class procedure PlaySound(const FileName: string);

    // Constructors
    {** Create with custom user-agent and base URL. }
  constructor Create(ua, base: string); overload;
    {** Create with custom user-agent and default base URL. }
  constructor Create(ua: string); overload;
    {** Create with default user-agent and base URL. }
  constructor Create; overload;

    // Properties
  property configUser: string read cfguser write cfguser;
  property locale: TFormatSettings read fsettings write updateLocale;

end;

  //procedure QWindow_setWindowBadge(window: QWindowH; badge: PChar); cdecl; external 'libQt6Gui.so.6';

const
DWMWA_CAPTION_COLOR = 35;
DWMWA_TEXT_COLOR = 36;
DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  // Default badge rendering parameters
DEFAULT_BADGE_SIZE_RATIO = 0.8;
DEFAULT_MIN_FONT_SIZE = 8;

  // (implementation continued)

implementation

// C-compatible write callback used by libcurl to collect response data.
function CurlWriteCallback(buffer: pchar; size, nmemb: longword;
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
{------------------------------------------------------------------------------
  TTrndiNativeBase.updateLocale
  -----------------------------
  Update decimal/thousand separators and other locale-driven formats.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.updateLocale(const l: TFormatSettings);
begin
  fsettings := l;
  DefaultFormatSettings := fsettings; // We need this for now
end;

{------------------------------------------------------------------------------
  TTrndiNativeBase.buildKey
  -------------------------
  Build a storage key. If not global, prepend user prefix when available.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.buildKey(const key: string; global: boolean): string;
begin
  if global then
    Result := key
  else
  if Trim(cfguser) <> '' then // Prepend the username and _
    Result := Format('%s_%s', [cfguser, key])
  else
    Result := key;
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable (class, virtual)
  ----------------------------------------------
  Platforms override; default returns True (assume available).
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.isNotificationSystemAvailable: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  getNotificationSystem (class, virtual)
  -------------------------------------
  Default returns 'unknown'; platform units should override with a concrete ID.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.getNotificationSystem: string;
begin
  Result := 'unknown';
end;

class function TTrndiNativeBase.HasNotifications: boolean;
begin
  // Forward to the virtual for platform-specific logic
  Result := isNotificationSystemAvailable;
end;

{$IFDEF Windows}
procedure TTrndiNativeBase.start;
begin

end;

procedure TTrndiNativeBase.done;
begin

end;
{$ENDIF}

{$IFNDEF Windows}
procedure TTrndiNativeBase.start;
begin

end;

procedure TTrndiNativeBase.done;
begin

end;
{$ENDIF}

{------------------------------------------------------------------------------
  setBadge
  --------
  Base convenience and default no-op; platform units override the 4-arg.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetBadge(const Value: string; badgeColor: TColor);
begin
  SetBadge(Value, badgeColor, DEFAULT_BADGE_SIZE_RATIO, DEFAULT_MIN_FONT_SIZE);
end;

procedure TTrndiNativeBase.SetBadge(const Value: string; BadgeColor: TColor;
badge_size_ratio: double; min_font_size: integer);
begin
  // No-op by default in base; platform classes override
end;

procedure TTrndiNativeBase.StartBadgeFlash(const Value: string;
badgeColor: TColor; DurationMS: integer; CycleMS: integer);
begin
  // Base: no animation. Platforms that support icon updates override.
  SetBadge(Value, badgeColor, DEFAULT_BADGE_SIZE_RATIO, DEFAULT_MIN_FONT_SIZE);
end;

procedure TTrndiNativeBase.StopBadgeFlash;
begin
  // Base: nothing to stop.
end;

{------------------------------------------------------------------------------
  PlaySound
  ---------
  Play an audio file by delegating to a native player per platform.
  Includes a basic validation on extension and characters to avoid shell issues.
 ------------------------------------------------------------------------------}
class procedure TTrndiNativeBase.PlaySound(const FileName: string);

function sIsValidAudioFile(const FileName: string): boolean;
  var
    Ext: string;
    ValidExtensions: array[0..6] of
    string = ('.wav', '.mp3', '.ogg', '.flac', '.aac', '.wma', '.m4a');
    i: integer;
  begin
    Result := false;

    if not FileExists(FileName) then
      Exit;

    Ext := LowerCase(ExtractFileExt(FileName));
    for i := 0 to High(ValidExtensions) do
      if Ext = ValidExtensions[i] then
      begin
        Result := true;
        Break;
      end;

    if not Result then
      Exit;

    if HasDangerousChars(FileName) then
      Exit;

    Result := true;
  end;

var
  Process: TProcess;
begin
  // Validate file before attempting to play
  if not sIsValidAudioFile(FileName) then
    Exit;

  Process := TProcess.Create(nil);

  try
    {$IFDEF X_WIN}
    Process.Executable := 'mplay32';
    Process.Parameters.Add('/play');
    Process.Parameters.Add('/close');
    Process.Parameters.Add(FileName);
    {$ENDIF}

    {$IFDEF X_LINUXBSD}
    Process.Executable := 'aplay';
    Process.Parameters.Add(FileName);
    {$ENDIF}

    {$IFDEF X_MAC}
    Process.Executable := 'afplay';
    Process.Parameters.Add(FileName);
    {$ENDIF}
    Process.Execute;
  finally
    Process.Free;
  end;
end;

{------------------------------------------------------------------------------
  Destroy
  -------
  Cleans up any allocated resources.
 ------------------------------------------------------------------------------}
destructor TTrndiNativeBase.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  DetectTouchScreen
  --------------------------
  Overridable touch screen setting
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.HasTouchScreen(out multi: boolean): boolean;
begin
  DetectTouchScreen(multi);
  Result := HasTouchScreen;
end;

{------------------------------------------------------------------------------
  HasTouchScreen
  --------------
  Detect if the system has a touchscreen. Honors the touchOverride flag first.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.HasTouchScreen: boolean;
var
  mt: boolean;
begin
  case touchOverride of
  tbTrue:
    Result := true;
  tbFalse:
    Result := false;
  else
    Result := DetectTouchScreen(mt);
  end;
end;

{------------------------------------------------------------------------------
  DetectTouchScreen (platform)
   --------------------------
   Platform-specific detection of touch hardware.
  ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
class function TTrndiNativeBase.DetectTouchScreen(out multi: boolean): boolean;
const
  TABLET_CONFIG_NONE = $00000000;
  NID_INTEGRATED_TOUCH = $00000001;
  NID_EXTERNAL_TOUCH = $00000002;
  NID_INTEGRATED_PEN = $00000004;
  NID_EXTERNAL_PEN = $00000008;
  NID_MULTI_INPUT = $00000040;
  NID_READY = $00000080;

function IsTouchReady: boolean;
  var
    Value: integer;
  begin
    Value := GetSystemMetrics(SM_DIGITIZER);
    Result := Value and NID_READY <> 0;
  end;

{------------------------------------------------------------------------------
  isMultiTouch
  ------------
  Detect more than one touch point
 ------------------------------------------------------------------------------}
function IsMultiTouch: boolean;
  var
    Value: integer;
  begin
    Value := GetSystemMetrics(SM_DIGITIZER);
    Result := Value and NID_MULTI_INPUT <> 0;
  end;

{------------------------------------------------------------------------------
  hasIntegratedTouch
  ------------------
  Detect a touch-first device
 ------------------------------------------------------------------------------}
function HasIntegratedTouch: boolean;
  var
    Value: integer;
  begin
    Value := GetSystemMetrics(SM_DIGITIZER);
    Result := Value and NID_INTEGRATED_TOUCH <> 0;
  end;

var
  val: integer;
const
  SM_MAXIMUMTOUCHES = 95;
begin
  Result := (HasIntegratedTouch) and (IsTouchReady);
  multi := IsMultiTouch;
end;
{$ELSEIF DEFINED(X_MAC)}
class function TTrndiNativeBase.DetectTouchScreen(out multi: boolean): boolean;
begin
  // macOS: Typically no standard touchscreen (unless iOS)
  Result := false;
  multi := false;
end;
{$ELSE}
class function TTrndiNativeBase.DetectTouchScreen(out multi: boolean): boolean;
var
  SL, Block: TStringList;
  i: integer;
  inBlock: boolean;
begin
  Result := false;
  multi := false;
  inBlock := false;
  Block := TStringList.Create;
  SL := TStringList.Create;
  try
    if FileExists('/proc/bus/input/devices') then
    begin
      SL.LoadFromFile('/proc/bus/input/devices');

      Block.Clear;
      for i := 0 to SL.Count - 1 do
        if Trim(SL[i]) = '' then
        begin
          // End of block; analyze accumulated lines
          if Block.Count > 0 then
          begin
            // Look for touch capability markers in this block
            if (Block.Text.ToLower.Contains('touch')) then
            begin
              Result := true;
              // Look for multi-touch markers in this block
              if (Block.Text.Contains('ABS_MT_POSITION')) or
                (Block.Text.Contains('ABS_MT_SLOT')) or
                (Block.Text.Contains('ABS_MT_TRACKING_ID')) then
                multi := true;
            end;
            Block.Clear;
          end;
        end
        else
          Block.Add(SL[i]);
  // Analyze the last block if the file does not end with a blank line
      if Block.Count > 0 then
        if (Block.Text.ToLower.Contains('touch')) then
        begin
          Result := true;
          if (Block.Text.Contains('ABS_MT_POSITION')) or
            (Block.Text.Contains('ABS_MT_SLOT')) or
            (Block.Text.Contains('ABS_MT_TRACKING_ID')) then
            multi := true;
        end;
    end;
  finally
    SL.Free;
    Block.Free;
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  create (default)
  ----------------------------
  An empty constructor that defaults useragent/baseurl to minimal placeholders.
 ------------------------------------------------------------------------------}
constructor TTrndiNativeBase.Create;
begin
  // Provide a default user-agent and empty base URL
  Create('Mozilla/5.0 (compatible; trndi) TrndiAPI', '');
end;

{------------------------------------------------------------------------------
  create (overload)
  ----------------------------
  Allow a custom user-agent.
 ------------------------------------------------------------------------------}
constructor TTrndiNativeBase.Create(ua: string);
begin
  // Provide a default user-agent and empty base URL
  Create(ua, '');
end;

{------------------------------------------------------------------------------
  create (overload)
  -----------------------------
  Allows specifying a custom user-agent and a base URL.
 -------------------------------------------------------
 -----------------------}
constructor TTrndiNativeBase.Create(ua, base: string);
begin
  useragent := ua;
  baseurl := base;
  // Check if we're in dark mode on creation
  dark := isDarkMode;
  fsettings := DefaultFormatSettings;
  if touchOverride = tbUnset then
    touchOverride := tbUnknown;
  cfguser := '';
  nofree := true;
  noticeDuration := 5000;
end;

{$IFDEF X_WIN}
  {------------------------------------------------------------------------------
    getLocaleInformation
  -------------------
    Windows only; Gets locale data
 ------------------------------------------------------------------------------}
function GetLocaleInformation(Flag: integer): string;
var
  wbuf: array[0..9] of widechar;
begin
  if GetLocaleInfoW(LOCALE_USER_DEFAULT,
    LOCALE_SISO639LANGNAME,
    wbuf, Length(wbuf)) > 0 then
    Result := UTF8Encode(widestring(wbuf))
  else
    Result := '';
end;

{$ENDIF}

{------------------------------------------------------------------------------
  GetOSLanguage
  -------------------
  Get the operating system's language/locale identifier.
  - Windows: locale via GetLocaleInfoW
  - macOS: NSLocale identifier
  - Linux/Unix: LANG environment variable
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.GetOSLanguage: string;
begin
  {$IFDEF X_WIN}
  Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
  {$ELSE}
  {$IFDEF X_MAC}
  result := NSLocale.currentLocale.localeIdentifier.utf8string;
  {$ELSE}
  Result := SysUtils.GetEnvironmentVariable('LANG');
  {$ENDIF}
  {$ENDIF}
end;


{------------------------------------------------------------------------------
  attention
  -------------------
  Surface a visual/sound notification depending on the platform.
  Platform units may override; base provides simple cross-platform attempts.
 ------------------------------------------------------------------------------}

procedure TTrndiNativeBase.attention(topic, message: string);
{$if  DEFINED(X_LINUXBSD)}
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
    StdoutS := '';
    StderrS := '';
    ExitCode := -1;

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

      // Drain stdout/stderr fully (works on Linux/Qt6)
      repeat
        while P.Output.NumBytesAvailable > 0 do
        begin
          n := P.Output.Read(Buf, SizeOf(Buf));
          if n > 0 then
            OutStr.WriteBuffer(Buf, n)
          else
            Break;
        end;
        while P.Stderr.NumBytesAvailable > 0 do
        begin
          n := P.Stderr.Read(Buf, SizeOf(Buf));
          if n > 0 then
            ErrStr.WriteBuffer(Buf, n)
          else
            Break;
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
      ErrStr.Free;
      OutStr.Free;
      P.Free;
    end;
  end;

procedure SendNotification(Title, Message: string);
  var
    AProcess: TProcess;
  begin
    {$IFDEF X_PC}
  // Linux unit may override attention or provide notification availability
    if isNotificationSystemAvailable then
    begin
      AProcess := TProcess.Create(nil);
      try
        AProcess.Executable := '/usr/bin/notify-send';
        AProcess.Parameters.Add(Title);
        AProcess.Parameters.Add(Message);
        AProcess.Options := AProcess.Options + [poNoConsole];
        AProcess.Execute;
      finally
        AProcess.Free;
      end;
    end
    else
   // ShowMessage('notify-send is required for Trndi to make notifications!');
    {$ENDIF}
  end;
  {$endif}
  {$if defined(X_WIN)}

  {------------------------------------------------------------------------------
    PSQuote
  -------------------
  Quote text for PowerShell
 ------------------------------------------------------------------------------}
function PSQuote(const S: unicodestring): unicodestring;
  begin
    // PowerShell single-quoted literal; escape embedded single quotes
    Result := '''' + StringReplace(S, '''', '''''', [rfReplaceAll]) + '''';
  end;

{------------------------------------------------------------------------------
  GetExePathW
  -------------------
  Get the app's path on Windows as a UTF8 string
 ------------------------------------------------------------------------------}
function GetExePathW: unicodestring;
  var
    Buf: array[0..32767] of widechar;
    Len: DWORD;
  begin
    Len := GetModuleFileNameW(0, @Buf[0], Length(Buf));
    SetString(Result, pwidechar(@Buf[0]), Len);
  end;

{------------------------------------------------------------------------------
  GetEnvVarW
  -------------------
  Get the PATH on Windows as a UTF8 string
 ------------------------------------------------------------------------------}
function GetEnvVarW(const Name: unicodestring): unicodestring;
  var
    Buf: array[0..32767] of widechar;
    Len: DWORD;
  begin
    Len := GetEnvironmentVariableW(pwidechar(Name), @Buf[0], Length(Buf));
    if Len = 0 then
      Result := ''
    else
      SetString(Result, pwidechar(@Buf[0]), Len);
  end;

{------------------------------------------------------------------------------
  SendNotification
  -------------------
  Send a notification to the desktop
 ------------------------------------------------------------------------------}
procedure SendNotification(const Title, Msg: unicodestring);
  var
    AppPath, TempDir, TempPng, LogPath: unicodestring;
    Script, CommandLine: unicodestring;
    SI: Windows.STARTUPINFOW;
    PI: Windows.PROCESS_INFORMATION;
  begin
    AppPath := GetExePathW;
    TempDir := GetEnvVarW('TEMP');
    if (TempDir <> '') and (TempDir[Length(TempDir)] <> '\') then
      TempDir := TempDir + '\';
    TempPng := TempDir + ExtractFileName(ChangeFileExt(AppPath, '')) + '-toast-logo.png';
    LogPath := TempDir + 'burnttoast-error.log';

    // PS script: try to extract icon -> PNG; if any error, write to log and still show toast without logo
    Script :=
      '$ErrorActionPreference = ''Stop''; ' + '$log = ' + PSQuote(LogPath) +
      '; ' + 'try { ' + 'Import-Module BurntToast; ' +
      'Add-Type -AssemblyName System.Drawing; ' + '$exe = ' +
      PSQuote(AppPath) + '; ' + '$png = ' + PSQuote(TempPng) + '; ' +
      '$ico = [System.Drawing.Icon]::ExtractAssociatedIcon($exe); ' +
      'if ($ico) { ' + '$bmp = $ico.ToBitmap(); ' +
      '$bmp2 = New-Object System.Drawing.Bitmap 64,64; ' +
      '$g = [System.Drawing.Graphics]::FromImage($bmp2); ' +
      '$g.Clear([System.Drawing.Color]::Transparent); ' +
      '$g.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic; '
      + '$g.DrawImage($bmp,0,0,64,64); ' +
      '$bmp2.Save($png, [System.Drawing.Imaging.ImageFormat]::Png); ' +
      '$g.Dispose(); $bmp.Dispose(); $bmp2.Dispose(); $ico.Dispose(); ' +
      '} ' + 'if (Test-Path $png) { ' +
      'New-BurntToastNotification -AppLogo $png -Text ' + PSQuote(Title) +
      ', ' + PSQuote(Msg) + '; ' + '} else { ' +
      'New-BurntToastNotification -Text ' + PSQuote(Title) + ', ' +
      PSQuote(Msg) + '; ' + '} ' + '} catch { ' +
      'try { $_ | Out-String | Set-Content -Path $log -Encoding UTF8 } catch {} ' +
      'New-BurntToastNotification -Text ' + PSQuote(Title) + ', ' +
      PSQuote(Msg) + '; ' + '}';

    CommandLine := 'powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "' +
      Script + '"';

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;

    UniqueString(CommandLine);

    if not Windows.CreateProcessW(nil, pwidechar(CommandLine),
      nil, nil, false, CREATE_NO_WINDOW, nil, nil, SI, PI) then
      RaiseLastOSError
    else
    begin
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
  end;

  {$endif}
  {$IF DEFINED(X_MAC)}
procedure SendNotification(const title, msg: string);
  var
    Notification: NSUserNotification;
  begin
    Notification := NSUserNotification.alloc.init;
    Notification.setTitle(NSSTR(Title));
    Notification.setInformativeText(NSSTR(Message));
    NSUserNotificationCenter.defaultUserNotificationCenter.deliverNotification(
      Notification);
    Notification.Release;
  end;
  {$endif}
begin
  SendNotification(topic, message);
end;

{------------------------------------------------------------------------------
  request
  -------------------
  Sends a GET or POST request, depending on the "post" parameter.
  Behavior differs by platform. Each platform has its own implementation block:
    - X_MAC uses TNSHTTPSendAndReceive
    - X_WIN uses TWinHTTPClient
    - X_PC (Linux) uses libCURL
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
function TTrndiNativeBase.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''; prefix: boolean = true): string;
var
  res, send: TStringStream;
  headers: TStringList;
  sx: string;
begin
  res := TStringStream.Create('');
  send := TStringStream.Create('');
  headers := TStringList.Create;
  try
    // We use a custom TNSHTTPSendAndReceive
    with TNSHTTPSendAndReceive.Create do
    try
      if prefix then
        address := Format('%s/%s', [baseurl, endpoint])
      else
        address := endpoint;
      if post then
        method := 'POST'
      else
        method := 'GET';

      // If a custom header is provided (single 'Key=Value')
      if header <> '' then
        Headers.Add(header);

      // If we have JSON data, we assume it's for POST and set headers accordingly
      if jsondata <> '' then
      begin
        Headers.Add('Content-Type=application/json');
        if useragent <> '' then
          Headers.Add('User-Agent=' + useragent);

        // Write JSON to send stream
        send.Write(jsondata[1], Length(jsondata));
        Headers.Add('Content-Length=' + IntToStr(send.Size));
      end
      else
      if Length(params) > 0 then
      begin
        // If we have query params, append them
        address := address + '?';
        for sx in params do
          address := address + '&' + sx;
      end;

      // Perform the request and normalize output to a trimmed string or error
      if SendAndReceive(send, res, headers) then
        Result := Trim(res.DataString)
      else
        Result := '+' + LastErrMsg;
    finally
      Free; // free the TNSHTTPSendAndReceive
    end;
  finally
    res.Free;
    send.Free;
    headers.Free;
  end;
end;

{$ELSEIF DEFINED(X_WIN)}
function TTrndiNativeBase.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''; prefix: boolean = true): string;
var
  client: TWinHTTPClient;
  sx, address: string;
  headers: array of string;
  hasParams: boolean;
  ResStr: string;
begin
  hasParams := (Length(params) > 0);
  client := TWinHTTPClient.Create(useragent);
  try
    if prefix then
  // Construct the full URL with normalized slashes (avoid '//' or missing '/')
      address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])])
    else
      address := endpoint;

    // Add default required headers
    client.AddHeader('User-Agent', useragent);

    // Add optional custom header
    if header <> '' then
    begin
      headers := header.Split(['=']);
      if Length(headers) = 2 then
        client.AddHeader(headers[0], headers[1]);
    end;

  // Handle JSON data (POST body) or query params
    if jsondata <> '' then
    begin
      client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      client.AddHeader('Accept', 'application/json');
      client.SetRequestBody(jsondata);
    end
    else
    if hasParams then
    begin
      address := address + '?';
      for sx in params do
        address := address + '&' + sx;
    end;

    // Perform the request (GET or POST)
    try
      if post then
        ResStr := client.Post(address)
      else
        ResStr := client.Get(address, params); // TWinHTTPClient supports passing params to Get
    except
      on E: Exception do
      begin
        Result := E.Message;
        Exit;
      end;
    end;

    Result := ResStr;
  finally
    client.Free;
  end;
end;

{$ELSE}
{$IFNDEF DARWIN}
function TTrndiNativeBase.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''; prefix: boolean = true): string;
var
  handle: CURL;
  headers: pcurl_slist;
  errCode: CURLcode;
  address, sx: string;
  p, i: integer;
  key, val: string;
  responseStream: TStringStream;
begin
  Result := '';

  // Build URL
  if prefix then
    address := Format('%s/%s', [baseurl, endpoint])
  else
    address := endpoint;

  // If not JSON and params present, append as query string
  if (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
  end;

  // Initialize libcurl (global init is safe to call repeatedly)
  curl_global_init(CURL_GLOBAL_DEFAULT);
  headers := nil;
  responseStream := TStringStream.Create('');
  try
    handle := curl_easy_init();
    if handle = nil then
    begin
      Result := 'curl: failed to init';
      Exit;
    end;

    // Set URL
    curl_easy_setopt(handle, CURLOPT_URL, pchar(address));

    // Follow redirects
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, longint(1));

    // Set user agent if provided
    if useragent <> '' then
      curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(useragent));

    // Optional custom header "Key=Value" -> "Key: Value"
    if header <> '' then
    begin
      p := Pos('=', header);
      if p > 0 then
      begin
        key := Trim(Copy(header, 1, p-1));
        val := Trim(Copy(header, p+1, MaxInt));
        if (key <> '') then
          headers := curl_slist_append(headers, pchar(Format('%s: %s', [key, val])));
      end;
    end;

    // If JSON body is provided, set content-type and POST data
    if jsondata <> '' then
    begin
      headers := curl_slist_append(headers, 'Content-Type: application/json; charset=UTF-8');
      headers := curl_slist_append(headers, 'Accept: application/json');
      curl_easy_setopt(handle, CURLOPT_POST, longint(1));
      curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(jsondata));
      curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, longint(Length(jsondata)));
    end
    else
    if post then
      if Length(params) > 0 then
      begin
        sx := '';
        for i := 0 to High(params) do
        begin
          if i > 0 then
            sx := sx + '&';
          sx := sx + params[i];
        end;
        curl_easy_setopt(handle, CURLOPT_POST, longint(1));
        curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(sx));
      end
      else
        curl_easy_setopt(handle, CURLOPT_POST, longint(1))// If POST with params but no json, send as form-urlencoded
    ;

    // Attach headers list if present
    if headers <> nil then
      curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

  // Set up write callback to capture response (use a C-compatible top-level function)
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallback));
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));

    // Perform the request
    errCode := curl_easy_perform(handle);
    if errCode <> CURLE_OK then
      Result := string(curl_easy_strerror(errCode))
    else
      Result := responseStream.DataString;

    // Cleanup
    curl_easy_cleanup(handle);
  finally
    if headers <> nil then
      curl_slist_free_all(headers);
    responseStream.Free;
    // global cleanup is optional and process-wide; leave it out to avoid interfering
    // with other curl usage: curl_global_cleanup();
  end;
end;
{$ENDIF}
{$ENDIF}

{------------------------------------------------------------------------------
  GetCharSetting
  -------------------
  Returns a char from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetCharSetting(const keyname: string; def: char = #0): char;
var
  res: string;
begin
  res := GetSetting(keyname, '');
  if res = '' then
    Result := def
  else
    Result := res[1];
end;

{------------------------------------------------------------------------------
  GetRootSetting
  -------------------
  Returns a bool from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetRootSetting(const keyname: string;
def: string = ''): string;
begin
  Result := GetSetting(keyname, def, true);
end;

{ Settings API implementations are now platform-specific and moved to
  trndi.native.win/mac/linux units. }

{------------------------------------------------------------------------------
  GetIntSetting
  -------------------------
  Returns an integer from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetIntSetting(const keyname: string;
def: integer = -1): integer;
var
  r: string;
begin
  r := GetSetting(keyname, 'fail');

  if not TryStrToInt(r, Result) then
    Result := def;
end;

{------------------------------------------------------------------------------
  GetFloatSetting
  -------------------------
  Returns a single from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetFloatSetting(const keyname: string;
def: single = -1): single;
var
  r: string;
  f: TFormatSettings;
begin
  r := GetSetting(keyname, 'fail');

  f := DefaultFormatSettings;
  f.DecimalSeparator := '.';
  Result := StrToFloatDef(r, def, f);
end;

{------------------------------------------------------------------------------
  GetSettingEx
  -------------------------
  Gets a setting, but if empty returns default value
  -----------------------------------------------------------------------------}
function TTrndiNativeBase.GetSettingEx(const keyname: string;
def: string = ''; global: boolean = false): string;
var
  res: string;
begin
  res := GetSetting(keyname, def, global);
  if res = '' then
    Result := def
  else
    Result := res;
end;

{------------------------------------------------------------------------------
  CheckSetting
  -------------------------
  Compares a setting result to an expected value
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.CheckSetting(const keyname: string; def: string; match: string;
  global: boolean = false): boolean;
var
  val: string;
begin
   val := GetSetting(keyname, def, global);
   val := Trim(val);
   val := LowerCase(val);
   result := SameText(val, LowerCase(match));
end;

{------------------------------------------------------------------------------
  GetBoolSetting
  -------------------------
  Returns a bool from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetBoolSetting(const keyname: string;
def: boolean = false): boolean;
var
  r: string;
begin
  r := GetSetting(keyname, '-');
  case r of
  'true':
    Result := true;
  '1':
    Result := true;
  '0':
    Result := false;
  'false':
    Result := false;
  else
    Result := def;
  end;
end;

{------------------------------------------------------------------------------
  SetColorSetting
  ----------------------
  Stores a TColor value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetColorSetting(const keyname: string; val: TColor);
begin
  SetSetting(keyname, IntToStr(integer(val)));
end;

{------------------------------------------------------------------------------
  SetWideCharSetting
  ----------------------
  Stores a WideChar value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetWideCharSetting(const keyname: string; val: widechar);
var
  code: string;
begin
  code := IntToHex(Ord(val), 4);
  SetSetting(keyname, code);
end;

{------------------------------------------------------------------------------
  GetWideCharSetting
  -------------------------
  Returns a WideChar from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetWideCharSetting(const keyname: string;
const def: widechar = widechar($2B24)): widechar;
var
  code: integer;
  s: string;
begin
  s := GetSetting(keyname, '');

  if s.IsEmpty then
    Exit(def);

  code := StrToInt('$' + s);

  Result := widechar(code);
end;

{------------------------------------------------------------------------------
  GetColorSetting
  -------------------------
  Returns a TColor from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetColorSetting(const keyname: string;
const def: TColor): TColor;
var
  i: integer;
  s: string;
begin
  s := GetSetting(keyname, IntToStr(integer(def)));

  if s.IsEmpty then
    Exit(def);

  if TryStrToInt(s, i) then
    Exit(TColor(i));

  Result := def;
end;

{------------------------------------------------------------------------------
  SetBoolSetting
  ----------------------
  Stores a bool value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetBoolSetting(const keyname: string; const val: boolean);
begin
  if val then
    SetSetting(keyname, 'true')
  else
    SetSetting(keyname, 'false');
end;

{------------------------------------------------------------------------------
  SetFloatSetting
  ----------------------
  Store a float value to platform-specific storage using '.' decimal separator.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetFloatSetting(const keyname: string; const val: single);
var
  f: TFormatSettings;
begin
  f := DefaultFormatSettings;
  f.DecimalSeparator := '.';
  SetSetting(keyname, FormatFloat('0.00', val, f));
end;

{------------------------------------------------------------------------------
  SetRootSetting
  ----------------------
  Stores a non-user specific string value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetRootSetting(keyname: string; const val: string);
begin
  SetSetting(keyname, val, true);
end;

{------------------------------------------------------------------------------
  DeleteRootSetting
  ----------------------
  Delete a non-user-scoped key from platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.DeleteRootSetting(keyname: string);
begin
  DeleteSetting(keyname, true);
end;

{------------------------------------------------------------------------------
  DeleteSetting
  -------------------------
  Deletes a stored key/value from platform-specific storage completely.
  - X_MAC sets key to blank
 ------------------------------------------------------------------------------}
{ Settings API implementations are now platform-specific and moved to
  trndi.native.win/mac/linux units. }


{------------------------------------------------------------------------------
  SetSetting
  ----------------------
  Stores a string value to platform-specific storage.
 ------------------------------------------------------------------------------}
{ Settings API implementations are now platform-specific and moved to
  trndi.native.win/mac/linux units. }

{------------------------------------------------------------------------------
  isDarkMode (class, virtual)
  ----------------------
  Default cross-platform heuristic: compare luminance of clWindow and clWindowText.
  Platform units may override for more accurate detection.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.isDarkMode: boolean;

function Brightness(C: TColor): double;
  begin
    Result := (Red(C) * 0.3) + (Green(C) * 0.59) + (Blue(C) * 0.11);
  end;

begin
  Result := (Brightness(ColorToRGB(clWindow)) < Brightness(ColorToRGB(clWindowText)));
end;

{------------------------------------------------------------------------------
  HasDangerousChars
  ----------------------
  Detects chars which the console is not fond of
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.HasDangerousChars(const FileName: string): boolean;

function HasCharsInSet(const Str: string; const CharSet: TSysCharSet): boolean;
  var
    i: integer;
  begin
    Result := false;
    for i := 1 to Length(Str) do
      if Str[i] in CharSet then
      begin
        Result := true;
        Exit;
      end;
  end;

var
  DangerousChars: TSysCharSet;
begin
  {$IFDEF WINDOWS}
  DangerousChars := ['&', '|', ';', '`', '$', '(', ')', '<', '>', '"', ''''];
  {$ELSE}
  DangerousChars := ['&', '|', ';', '`', '$', '(', ')', '<', '>', '"', '''', '\'];
  {$ENDIF}

  Result := HasCharsInSet(FileName, DangerousChars);
end;

{------------------------------------------------------------------------------
  DetectWSL
  ----------------------
  Detects if the app is running under Windows Subsystem for Linux
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.DetectWSL: TWSLInfo;
var
  Output: TStringList;
  Content: string;
  EnvVar: string;
begin
  // Initialize result
  Result.IsWSL := false;
  Result.Version := wslNone;
  Result.DistroName := '';
  Result.KernelVersion := '';

  {$IFDEF LINUX}
  // Kontrollera /proc/version
  if FileExists('/proc/version') then
  begin
    Output := TStringList.Create;
    try
      Output.LoadFromFile('/proc/version');
      if Output.Count > 0 then
      begin
        Content := Output[0];
        Result.KernelVersion := Content;

        Content := LowerCase(Content);
        if Pos('microsoft', Content) > 0 then
        begin
          Result.IsWSL := true;
          if Pos('wsl2', Content) > 0 then
            Result.Version := wslVersion2
          else
            Result.Version := wslVersion1;
        end
        else
        if Pos('wsl', Content) > 0 then
        begin
          Result.IsWSL := true;
          Result.Version := wslVersion2;
        end;
      end;
    finally
      Output.Free;
    end;
  end;

  // Kontrollera miljövariabler
  EnvVar := GetEnvironmentVariable('WSL_DISTRO_NAME');
  if EnvVar <> '' then
  begin
    Result.IsWSL := true;
    Result.DistroName := EnvVar;
    if Result.Version = wslNone then
      Result.Version := wslUnknown;
  end;

  // WSL_INTEROP (WSL2 specifik)
  if GetEnvironmentVariable('WSL_INTEROP') <> '' then
  begin
    Result.IsWSL := true;
    if Result.Version = wslNone then
      Result.Version := wslVersion2;
  end;

  // Ytterligare kontroller om vi inte hittat WSL än
  if not Result.IsWSL then
    if FileExists('/proc/sys/kernel/osrelease') then
    begin
      Output := TStringList.Create;
      try
        Output.LoadFromFile('/proc/sys/kernel/osrelease');
        if Output.Count > 0 then
        begin
          Content := LowerCase(Output[0]);
          if (Pos('microsoft', Content) > 0) or
            (Pos('wsl', Content) > 0) then
          begin
            Result.IsWSL := true;
            Result.Version := wslUnknown;
          end;
        end;
      finally
        Output.Free;
      end;
    end// Kontrollera /proc/sys/kernel/osrelease
  ;
  {$ENDIF}
end;

class function TTrndiNativeBase.SetTitleColor(form: THandle; bg, Text: TColor): boolean;
begin
  Result := false;
end;

end.
