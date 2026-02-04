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
Classes, SysUtils, Graphics, trndi.log
{$IF DEFINED(X_MAC)}
, NSMisc, ns_url_request, CocoaAll, LCLType, StrUtils
{$ELSEIF DEFINED(X_WIN)}
, Windows, Registry, Dialogs, StrUtils, winhttpclient, shellapi, comobj,
Forms, variants, dwmapi
{$ELSEIF DEFINED(X_PC)}
, libpascurl, Dialogs, LCLType, ctypes, StrUtils
{$ENDIF}
{$IF DEFINED(HAIKU)}
, fphttpclient, opensslsockets
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

  {** Enhanced HTTP response with headers, cookies, and redirect information. }
THTTPResponse = record
  Body: string;                  // Response body content
  Headers: TStringList;          // Response headers (key=value)
  Cookies: TStringList;          // Cookies received (Set-Cookie headers)
  StatusCode: integer;           // HTTP status code (200, 302, etc.)
  FinalURL: string;              // Final URL after redirects
  RedirectCount: integer;        // Number of redirects followed
  Success: boolean;              // True if request succeeded
  ErrorMessage: string;          // Error description if failed
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
  
  {**
    Enhanced HTTP request with full OAuth2 support.
    @param(post) True for POST, False for GET.
    @param(endpoint) Full URL or relative endpoint.
    @param(params) Query string pairs like 'key=value'.
    @param(jsondata) Optional JSON payload for POST.
    @param(cookieJar) Cookies to send (will be updated with received cookies).
    @param(followRedirects) True to automatically follow 302/301 redirects.
    @param(maxRedirects) Maximum number of redirects to follow (default 10).
    @param(customHeaders) Additional headers as TStringList (key=value).
    @param(prefix) True to prefix endpoint with baseurl.
    @returns(THTTPResponse with body, headers, cookies, and redirect info.)
  }
  function requestEx(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    cookieJar: TStringList = nil; followRedirects: boolean = true;
    maxRedirects: integer = 10; customHeaders: TStringList = nil;
    prefix: boolean = true): THTTPResponse;

    // Settings API
    {** Store a non-user-scoped key (global). }
  procedure SetRootSetting(keyname: string; const val: string);
    {** Store a string value under @param(keyname). @param(global) bypasses user scoping. }
  procedure SetSetting(const keyname: string; const val: string;
    global: boolean = false); virtual; abstract; overload;
    {** Store an integer value under @param(keyname). @param(global) bypasses user scoping. }
  procedure SetSetting(const keyname: string; const val: integer;
  global: boolean = false); overload;
    {** Store a bool value, represented by @param(a)/@param(b), under @param(keyname). @param(global) bypasses user scoping. }
  procedure SetSetting(const keyname: string; const val: boolean;
  global: boolean = false); overload;
    {** Store a float setting (using '.' decimal separator). }
  procedure SetFloatSetting(const keyname: string; const val: single; const global: boolean = false); overload;
    {** Store a boolean setting (serialized as a/b). }
  procedure SetBoolSetting(const keyname: string; const val: boolean; const a,b: string; const global: boolean = false);
    {** Store an array as CSV. }
  procedure SetCSVSetting(const keyname: string; const val: TStringArray; const global: boolean = false);
    {** Store a color value (TColor serialized as integer). }
  procedure SetColorSetting(const keyname: string; val: TColor);
    {** Store a WChar (WideChar serialized as string). }
  procedure SetWideCharSetting(const keyname: string; val: WChar);
    {** Retrieve a stored color or @param(def) if missing. }
  function GetColorSetting(const keyname: string; const def: TColor = $000000): TColor;
    {** Retrieve a stored WChar or @param(def) if missing. }
  function GetWideCharSetting(const keyname: string;
    const def: WChar = WChar($2B24)): WChar;
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
  {** Read setting, return true/false if it exists. }
  function TryGetSetting(const keyname: string; out res: string;
    global: boolean = false): boolean;
  {** Read settings list, return true/false if it exists. }
  function TryGetCSVSetting(const keyname: string; out res: TStringArray;
    global: boolean = false): boolean;
    {** Compare a setting to a value. }
  function CheckSetting(const keyname: string; def: string; match: string;
    global: boolean = false): boolean; virtual;

    // Theme/Env
    {** Determine if the OS/theme uses a dark appearance. Platforms override. }
  class function isDarkMode: boolean; virtual;
  {** Check whether native text-to-speech is available on this platform. }
  class function SpeakAvailable: boolean; virtual;
  {** Returns the name of the software used for speech on this platform (e.g., 'spd-say', 'SAPI', 'say'). }
  class function SpeakSoftwareName: string; virtual;
  {** Return a best-effort window manager name for the current platform.
      Examples: 'openbox', 'WindowServer', 'Windows Desktop', or empty string when unknown. }
  class function GetWindowManagerName: string; virtual;
  {** Check if the window manager is Sway (Wayland compositor). }
  class function nobuttonsVM: boolean; virtual;
  class function DetectTouchScreen(out multi: boolean): boolean;
    {** Detect if the device has a touchscreen and whether it's multi-touch. }
  class function HasTouchScreen(out multi: boolean): boolean;
  class function HasTouchScreen: boolean;
    {** Simple HTTP GET helper; platform units implement. }
  class function getURL(const url: string; out res: string): boolean; virtual; abstract;
    {**
      Attempt an HTTP GET through an explicit proxy (no automatic fallback).
      
      This is intended for UI "Test proxy" actions where we want to know if
      the configured proxy itself works.

      @param(url) URL to request.
      @param(proxyHost) Proxy hostname (or host:port). Empty means "no proxy".
      @param(proxyPort) Optional proxy port (string, numeric).
      @param(proxyUser) Optional proxy username.
      @param(proxyPass) Optional proxy password.
      @param(res) Response body on success or error message on failure.
      @returns True on success, False on failure.
    }
  class function TestProxyURL(const url: string; const proxyHost: string;
    const proxyPort: string; const proxyUser: string; const proxyPass: string;
    out res: string): boolean; virtual;
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
    {** Detect if a global menu exists (top bar). }
  class function HasGlobalMenu: boolean; virtual;
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

    // Desktop-indicator cache helpers (GNOME/KDE)
    {**
      Write extra metadata for desktop indicators that read the cache file
      (e.g. GNOME top-bar / KDE plasmoid). Platforms may override.

      @param(Value) The display value (same as badge text).
      @param(ReadingTime) Timestamp of the underlying reading.
      @param(FreshMinutes) Max age (minutes) before data is considered stale.
    }
  procedure WriteCurrentIndicatorCache(const Value: string;
    const ReadingTime: TDateTime; FreshMinutes: integer); virtual;
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
function CurlWriteCallback(buffer: pchar; size, nmemb: SizeUInt;
userdata: Pointer): SizeUInt; cdecl;
var
  stream: TStringStream;
  actualSize: SizeUInt;
  dataStr: string;
begin
  Result := size * nmemb;
  
  LogMessageToFile(Format('[CurlWriteCallback] Called: size=%d, nmemb=%d, Result=%d', [Int64(size), Int64(nmemb), Int64(Result)]));
  
  // Safety checks
  if (buffer = nil) or (size = 0) or (nmemb = 0) or (Result = 0) then
  begin
    LogMessageToFile('[CurlWriteCallback] Safety check failed, returning Result');
    Exit;
  end;
  
  // Prevent overflow
  if Result > 10485760 then  // 10MB limit
  begin
    LogMessageToFile('[CurlWriteCallback] Overflow check failed');
    Exit;
  end;
  
  actualSize := Result;
  stream := TStringStream(userdata);
  
  if stream = nil then
  begin
    LogMessageToFile('[CurlWriteCallback] Stream is nil');
    Exit;
  end;
  
  try
    // Write buffer to stream
    if actualSize > 0 then
    begin
      stream.WriteBuffer(buffer^, actualSize);
      LogMessageToFile(Format('[CurlWriteCallback] Wrote %d bytes to stream, returning %d', [Int64(actualSize), Int64(Result)]));
    end;
  except
    on E: Exception do
    begin
      LogMessageToFile('[CurlWriteCallback] Exception: ' + E.Message);
      // Return 0 on error to signal curl to abort
      Result := 0;
    end;
  end;
end;

// C-compatible write callback for requestEx (global, no nested/static link).
function CurlWriteCallbackEx(buffer: PChar; size, nitems: SizeUInt; userdata: Pointer): SizeUInt; cdecl;
var
  stream: TStringStream;
  actualSize: SizeUInt;
begin
  Result := size * nitems;

  // Safety checks
  if (buffer = nil) or (size = 0) or (nitems = 0) or (Result = 0) then
    Exit;

  if Result > 10485760 then // 10MB limit
    Exit;

  actualSize := Result;
  stream := TStringStream(userdata);

  if stream = nil then
    Exit;

  try
    if actualSize > 0 then
      stream.WriteBuffer(buffer^, actualSize);
  except
    Result := 0;
  end;
end;

// C-compatible header callback for requestEx (global, no nested/static link).
function CurlHeaderCallbackEx(buffer: PChar; size, nitems: SizeUInt; userdata: Pointer): SizeUInt; cdecl;
var
  stream: TStringStream;
  actualSize: SizeUInt;
  headerLine: string;
begin
  Result := size * nitems;

  if (buffer = nil) or (size = 0) or (nitems = 0) or (Result = 0) then
    Exit;

  if Result > 1048576 then // 1MB header limit
    Exit;

  actualSize := Result;
  stream := TStringStream(userdata);

  if stream = nil then
    Exit;

  try
    SetLength(headerLine, actualSize);
    if actualSize > 0 then
      Move(buffer^, headerLine[1], actualSize);
    stream.WriteString(headerLine);
  except
    Result := 0;
  end;
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

{------------------------------------------------------------------------------
  TestProxyURL (class, virtual)
  ----------------------------
  Default implementation: unsupported.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.TestProxyURL(const url: string;
  const proxyHost: string; const proxyPort: string; const proxyUser: string;
  const proxyPass: string; out res: string): boolean;
begin
  res := 'Proxy testing is not supported on this platform.';
  Result := false;
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

procedure TTrndiNativeBase.WriteCurrentIndicatorCache(const Value: string;
const ReadingTime: TDateTime; FreshMinutes: integer);
begin
  // Base: no-op. Platforms that expose indicator cache files override.
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
  SpeakAvailable
  --------------
  Default value: assume TTS is available. Platform units should override
  when they need to perform a runtime check (e.g., Linux checks for spd-say).
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.SpeakAvailable: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  SpeakSoftwareName
  -----------------
  Default implementation returns empty string. Platform units override to
  provide the name of the TTS tool used (e.g., 'spd-say' on Linux).
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.SpeakSoftwareName: string;
begin
  Result := '';
end;

{------------------------------------------------------------------------------
  GetWindowManagerName
  --------------------
  Default implementation returns empty string. Platform units override to
  provide a sensible value (Linux: env/DesktopHint, Windows/macOS: fixed
  names like 'Windows Desktop' / 'WindowServer').
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.GetWindowManagerName: string;
begin
  Result := '';
end;

{------------------------------------------------------------------------------
  nobuttonsVM (base)
  ------------------
  Default implementation returns false. Platform units override to detect Sway.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.nobuttonsVM: boolean;
begin
  Result := false;
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
  i, j: integer;
  inBlock: boolean;
  Line, Handler, DevPath: string;
  HasAccessibleDevice: boolean;
  F: Integer;
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
              // Verify device handlers are actually accessible
              HasAccessibleDevice := false;
              for j := 0 to Block.Count - 1 do
              begin
                Line := Block[j];
                if (Pos('H: Handlers=', Line) = 1) or (Pos('H: ', Line) = 1) then
                begin
                  // Extract event handlers (e.g., "event0", "event3")
                  if Pos('event', LowerCase(Line)) > 0 then
                  begin
                    // Try to actually open /dev/input/eventX device for reading
                    for Handler in Line.Split([' ', '=']) do
                      if (Pos('event', LowerCase(Handler)) > 0) then
                      begin
                        DevPath := '/dev/input/' + Handler;
                        if FileExists(DevPath) then
                        begin
                          F := FileOpen(DevPath, fmOpenRead);
                          if F <> -1 then
                          begin
                            FileClose(F);
                            HasAccessibleDevice := true;
                            Break;
                          end;
                        end;
                      end;
                  end;
                  if HasAccessibleDevice then
                    Break;
                end;
              end;
              
              if HasAccessibleDevice then
              begin
                Result := true;
                // Look for multi-touch markers in this block
                if (Block.Text.Contains('ABS_MT_POSITION')) or
                  (Block.Text.Contains('ABS_MT_SLOT')) or
                  (Block.Text.Contains('ABS_MT_TRACKING_ID')) then
                  multi := true;
              end;
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
          // Verify device handlers are actually accessible
          HasAccessibleDevice := false;
          for j := 0 to Block.Count - 1 do
          begin
            Line := Block[j];
            if (Pos('H: Handlers=', Line) = 1) or (Pos('H: ', Line) = 1) then
            begin
              // Extract event handlers (e.g., "event0", "event3")
              if Pos('event', LowerCase(Line)) > 0 then
              begin
                // Try to actually open /dev/input/eventX device for reading
                for Handler in Line.Split([' ', '=']) do
                  if (Pos('event', LowerCase(Handler)) > 0) then
                  begin
                    DevPath := '/dev/input/' + Handler;
                    if FileExists(DevPath) then
                    begin
                      F := FileOpen(DevPath, fmOpenRead);
                      if F <> -1 then
                      begin
                        FileClose(F);
                        HasAccessibleDevice := true;
                        Break;
                      end;
                    end;
                  end;
              end;
              if HasAccessibleDevice then
                Break;
            end;
          end;
          
          if HasAccessibleDevice then
          begin
            Result := true;
            if (Block.Text.Contains('ABS_MT_POSITION')) or
              (Block.Text.Contains('ABS_MT_SLOT')) or
              (Block.Text.Contains('ABS_MT_TRACKING_ID')) then
              multi := true;
          end;
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
  wbuf: array[0..9] of WChar;
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
function NormalizeLang(const s: string): string;
  var
    v: string;
    p: SizeInt;
  begin
    v := Trim(s);
    v := StringReplace(v, '-', '_', [rfReplaceAll]);

    // Strip encoding / modifiers if present (e.g. sv_SE.UTF-8, sv_SE@euro)
    p := Pos('.', v);
    if p > 0 then
      v := Copy(v, 1, p - 1);
    p := Pos('@', v);
    if p > 0 then
      v := Copy(v, 1, p - 1);

    // Prefer just the language code for Lazarus translations (e.g. sv_SE -> sv)
    p := Pos('_', v);
    if p > 0 then
      v := Copy(v, 1, p - 1);

    Result := LowerCase(Trim(v));
  end;

begin
  {$IFDEF X_WIN}
  Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
  {$ELSE}
  {$IFDEF X_MAC}
  // NSLocale.currentLocale.localeIdentifier reflects locale/region (can be en_SE).
  // For UI language, prefer the system preferred language list.
  Result := '';
  if (NSLocale.preferredLanguages <> nil) and (NSLocale.preferredLanguages.count > 0) then
    Result := UTF8Encode(NSString(NSLocale.preferredLanguages.objectAtIndex(0)).utf8string);
  if Result = '' then
    Result := UTF8Encode(NSLocale.currentLocale.localeIdentifier.utf8string);
  Result := NormalizeLang(Result);
  {$ELSE}
  Result := NormalizeLang(SysUtils.GetEnvironmentVariable('LANG'));
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
    Buf: array[0..32767] of WChar;
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
    Buf: array[0..32767] of WChar;
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

{$elseif defined(X_MAC)}
procedure SendNotification(const Title, Msg: string);
  var
    P: TProcess;
    OutStr, ErrStr: TStringStream;
    Buf: array[0..4095] of byte;
    n: SizeInt;
    ExitCode: integer;
    LogPath: string;
    SL: TStringList;
  begin
    // NSUserNotification is deprecated and may not show reliably on recent macOS.
    // We currently use AppleScript via /usr/bin/osascript.
    // Use argv to avoid AppleScript quoting/escaping issues.
    if not FileExists('/usr/bin/osascript') then
      Exit;

    P := TProcess.Create(nil);
    OutStr := TStringStream.Create('');
    ErrStr := TStringStream.Create('');
    try
      P.Executable := '/usr/bin/osascript';
      P.Parameters.Add('-e');
      P.Parameters.Add('on run argv');
      P.Parameters.Add('-e');
      P.Parameters.Add('display notification (item 1 of argv) with title (item 2 of argv)');
      P.Parameters.Add('-e');
      P.Parameters.Add('end run');

      // Pass message + title as argv items (ensure UTF-8).
      P.Parameters.Add(UTF8Encode(Msg));
      P.Parameters.Add(UTF8Encode(Title));

      P.Options := [poUsePipes, poWaitOnExit, poNoConsole];
      P.Execute;

      // Drain pipes (even with poWaitOnExit, data may remain buffered)
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

      ExitCode := P.ExitStatus;
      if ExitCode <> 0 then
      begin
        LogPath := GetTempDir(false) + 'trndi-notification-error.log';
        SL := TStringList.Create;
        try
          SL.Add('osascript notification failed');
          SL.Add('ExitStatus=' + IntToStr(ExitCode));
          SL.Add('');
          if OutStr.DataString <> '' then
          begin
            SL.Add('[stdout]');
            SL.Add(OutStr.DataString);
            SL.Add('');
          end;
          if ErrStr.DataString <> '' then
          begin
            SL.Add('[stderr]');
            SL.Add(ErrStr.DataString);
          end;
          SL.SaveToFile(LogPath);
        finally
          SL.Free;
        end;
      end;
    finally
      ErrStr.Free;
      OutStr.Free;
      P.Free;
    end;
  end;

{$elseif DEFINED(X_LINUXBSD) or DEFINED(BSD)}
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

procedure SendNotification(const Title, Msg: string);
  var
    AProcess: TProcess;
  begin
    // Linux/BSD unit may override attention or provide notification availability
    if isNotificationSystemAvailable then
    begin
      AProcess := TProcess.Create(nil);
      try
        AProcess.Executable := '/usr/bin/notify-send';
        AProcess.Parameters.Add(Title);
        AProcess.Parameters.Add(Msg);
        AProcess.Options := AProcess.Options + [poNoConsole];
        AProcess.Execute;
      finally
        AProcess.Free;
      end;
    end;
  end;

{$else}
// Fallback for platforms without specific notification implementation
// (Haiku and other platforms should override the attention method)
procedure SendNotification(const Title, Msg: string);
  begin
    // No-op: notification system not available on this platform
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
  proxyHost: string;
  proxyPortS: string;
  proxyPort: integer;
  proxyUser: string;
  proxyPass: string;

  procedure ConfigureClient(aClient: TWinHTTPClient);
  begin
    // Add default required headers
    aClient.AddHeader('User-Agent', useragent);

    // Add optional custom header
    if header <> '' then
    begin
      headers := header.Split(['=']);
      if Length(headers) = 2 then
        aClient.AddHeader(headers[0], headers[1]);
    end;

    // Handle JSON data (POST body)
    if jsondata <> '' then
    begin
      aClient.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      aClient.AddHeader('Accept', 'application/json');
      aClient.SetRequestBody(jsondata);
    end;
  end;

  function TryRequest(aClient: TWinHTTPClient; out outRes: string): boolean;
  begin
    try
      ConfigureClient(aClient);
      if post then
        outRes := aClient.Post(address)
      else
      begin
        // Don't pass params to Get() if already appended to URL
        if (jsondata = '') and hasParams then
          outRes := aClient.Get(address, [])
        else
          outRes := aClient.Get(address, params);
      end;
      Result := true;
      LogMessageToFile('Windows: Request succeeded');
    except
      on E: Exception do
      begin
        outRes := E.Message;
        LogMessageToFile('Windows: Request failed with exception: ' + E.Message);
        Result := false;
      end;
    end;
  end;
begin
  hasParams := (Length(params) > 0);

  if prefix then
    // Construct the full URL with normalized slashes (avoid '//' or missing '/')
    address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])])
  else
    address := endpoint;

  // If no JSON body but params present, append as query string
  if (jsondata = '') and hasParams then
  begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
  end;

  // Read proxy settings (global)
  proxyHost := Trim(GetRootSetting('proxy.host', ''));
  proxyPortS := Trim(GetRootSetting('proxy.port', ''));
  proxyPort := StrToIntDef(proxyPortS, 8080);
  proxyUser := GetRootSetting('proxy.user', '');
  proxyPass := GetRootSetting('proxy.pass', '');

  // If custom proxy configured: proxy-first, then forced-direct fallback
  if proxyHost <> '' then
  begin
    if (proxyUser <> '') or (proxyPass <> '') then
      client := TWinHTTPClient.Create(useragent, proxyHost, proxyPort, proxyUser, proxyPass)
    else
      client := TWinHTTPClient.Create(useragent, proxyHost, proxyPort);
    try
      if TryRequest(client, ResStr) then
      begin
        Result := ResStr;
        Exit;
      end;
    finally
      client.Free;
    end;

    // Direct fallback must not use system proxy
    client := TWinHTTPClient.Create(useragent, true);
    try
      if TryRequest(client, ResStr) then
        Result := ResStr
      else
        Result := ResStr;
    finally
      client.Free;
    end;
    Exit;
  end;

  // No custom proxy configured - use direct connection only (match other platforms)
  LogMessageToFile('Windows: Using direct connection (no proxy configured) to: ' + address);
  client := TWinHTTPClient.Create(useragent, true);
  try
    if TryRequest(client, ResStr) then
      Result := ResStr
    else
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
  maskedSx: string;
  p, i: integer;
  key, val: string;
  responseStream: TStringStream;
  proxyHost: string;
  proxyPortS: string;
  proxyUser: string;
  proxyPass: string;

  function PerformRequest(withProxy: boolean): boolean;
  var
    j: integer;
  begin
    Result := false;
    responseStream.Size := 0;
    responseStream.Position := 0;

    handle := curl_easy_init();
    if handle = nil then
    begin
      errCode := CURLE_FAILED_INIT;
      Exit(false);
    end;

    // Set URL + common options
    curl_easy_setopt(handle, CURLOPT_URL, pchar(address));
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(1));
    curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, clong(10));
    curl_easy_setopt(handle, CURLOPT_TIMEOUT, clong(30));

    if useragent <> '' then
      curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(useragent));

    // Proxy handling (proxy-first / forced-direct fallback)
    if withProxy and (proxyHost <> '') then
    begin
      curl_easy_setopt(handle, CURLOPT_PROXY, pchar(proxyHost));
      if proxyPortS <> '' then
        curl_easy_setopt(handle, CURLOPT_PROXYPORT, clong(StrToIntDef(proxyPortS, 8080)));
      if (proxyUser <> '') and (proxyPass <> '') then
        curl_easy_setopt(handle, CURLOPT_PROXYUSERPWD, pchar(proxyUser + ':' + proxyPass));
    end
    else
    if (not withProxy) and (proxyHost <> '') then
      curl_easy_setopt(handle, CURLOPT_PROXY, pchar(''));

    // Attach headers list if present
    if headers <> nil then
      curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

    // POST bodies
    if jsondata <> '' then
    begin
      curl_easy_setopt(handle, CURLOPT_POST, clong(1));
      curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(jsondata));
      curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, clong(Length(jsondata)));
    end
    else
    if post then
    begin
      if Length(params) > 0 then
      begin
        sx := '';
        for j := 0 to High(params) do
        begin
          if j > 0 then
            sx := sx + '&';
          sx := sx + params[j];
        end;
        curl_easy_setopt(handle, CURLOPT_POST, clong(1));
        curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(sx));
      end
      else
        curl_easy_setopt(handle, CURLOPT_POST, clong(1));
    end;

    // Capture response
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallback));
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));

    errCode := curl_easy_perform(handle);
    curl_easy_cleanup(handle);
    Result := (errCode = CURLE_OK);
  end;
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
    // Read proxy settings (global)
    proxyHost := Trim(GetRootSetting('proxy.host', ''));
    proxyPortS := Trim(GetRootSetting('proxy.port', ''));
    proxyUser := GetRootSetting('proxy.user', '');
    proxyPass := GetRootSetting('proxy.pass', '');

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
      headers := curl_slist_append(headers, pchar('Content-Type: application/json; charset=UTF-8'));
      headers := curl_slist_append(headers, pchar('Accept: application/json'));
    end
    ;

    // Attach headers list if present
    // If proxy configured: try via proxy first, then forced-direct fallback
    if proxyHost <> '' then
    begin
      if PerformRequest(true) then
        Result := responseStream.DataString
      else
      if PerformRequest(false) then
        Result := responseStream.DataString
      else
        Result := string(curl_easy_strerror(errCode));
    end
    else
    begin
      if PerformRequest(false) then
        Result := responseStream.DataString
      else
        Result := string(curl_easy_strerror(errCode));
    end;
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
  requestEx
  -------------------
  Enhanced HTTP request with OAuth2 support: cookies, redirect tracking,
  and full response header capture.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_PC)}
function TTrndiNativeBase.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
cookieJar: TStringList = nil; followRedirects: boolean = true;
maxRedirects: integer = 10; customHeaders: TStringList = nil;
prefix: boolean = true): THTTPResponse;
var
  handle: CURL;
  headers: pcurl_slist;
  errCode: CURLcode;
  address, sx: string;
  maskedSx: string;
  i, j: integer;
  responseStream: TStringStream;
  headerStream: TStringStream;
  proxyHost, proxyPortS, proxyUser, proxyPass: string;
  cookieData: string;
  responseLine: string;
  responseCode: clong;
  effectiveUrl: PChar;
  startTick: QWord;
  endTick: QWord;
  methodLabel: string;
  cookieVal: string;
  cookiePos: integer;
  function HasHeader(const AName: string): boolean;
  var
    k: integer;
    nameLower: string;
  begin
    Result := False;
    if customHeaders = nil then
      Exit;
    nameLower := LowerCase(AName) + ':';
    for k := 0 to customHeaders.Count - 1 do
      if Pos(nameLower, LowerCase(Trim(customHeaders[k]))) = 1 then
        Exit(True);
  end;

  // Helper to mask sensitive form parameters in a URL-encoded string
  procedure MaskParam(var S: string; const name: string);
  var
    p, valStart, q: integer;
  begin
    p := Pos(name + '=', S);
    if p = 0 then Exit;
    valStart := p + Length(name) + 1;
    q := PosEx('&', S, valStart);
    if q = 0 then
      q := Length(S) + 1;
    Delete(S, valStart, q - valStart);
    Insert('***', S, valStart);
  end;

begin
  // Initialize address from endpoint parameter
  address := endpoint;
  
  // Initialize Result
  Result.Body := '';
  Result.Headers := TStringList.Create;
  Result.Cookies := TStringList.Create;
  Result.Success := false;
  Result.StatusCode := 0;
  Result.RedirectCount := 0;
  Result.FinalURL := '';
  Result.ErrorMessage := '';
  
  // Append query params ONLY for GET requests (not POST)
  // For POST, params will be sent in the POST body as form data
  if (not post) and (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
  end;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  headers := nil;
  responseStream := TStringStream.Create('');
  headerStream := TStringStream.Create('');
  try
    if post then
      methodLabel := 'POST'
    else
      methodLabel := 'GET';
    startTick := GetTickCount64;
    LogMessageToFile(Format('HTTP %s (curl): %s', [methodLabel, address]));
    handle := curl_easy_init();
    if handle = nil then
    begin
      Result.ErrorMessage := 'Failed to initialize CURL';
      Exit;
    end;

    try
      // Basic setup
      curl_easy_setopt(handle, CURLOPT_URL, pchar(address));
      curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, clong(10));
      curl_easy_setopt(handle, CURLOPT_TIMEOUT, clong(30));
      // Accept and auto-decompress compressed responses (gzip/deflate/br where available)
      curl_easy_setopt(handle, CURLOPT_ACCEPT_ENCODING, pchar(''));
      
      // Enable verbose output for debugging
      curl_easy_setopt(handle, CURLOPT_VERBOSE, clong(1));
      
      // Disable SSL certificate verification (NOT RECOMMENDED FOR PRODUCTION)
      curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, clong(0));
      curl_easy_setopt(handle, CURLOPT_SSL_VERIFYHOST, clong(0));

      if useragent <> '' then
        curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(useragent));

      // Redirect handling
      if followRedirects then
      begin
        curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(1));
        curl_easy_setopt(handle, CURLOPT_MAXREDIRS, clong(maxRedirects));
      end
      else
        curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(0));

      // Cookie handling
      if (cookieJar <> nil) and (cookieJar.Count > 0) then
      begin
        cookieData := '';
        for i := 0 to cookieJar.Count - 1 do
        begin
          if Trim(cookieJar[i]) = '' then
            Continue;
          if cookieData <> '' then
            cookieData := cookieData + '; ';
          cookieData := cookieData + cookieJar[i];
        end;
        if cookieData <> '' then
          curl_easy_setopt(handle, CURLOPT_COOKIE, pchar(cookieData));
      end;

      // Custom headers
      if customHeaders <> nil then
      begin
        for i := 0 to customHeaders.Count - 1 do
          headers := curl_slist_append(headers, pchar(customHeaders[i]));
      end;

      // POST handling
      if jsondata <> '' then
      begin
        if not HasHeader('Content-Type') then
          headers := curl_slist_append(headers, pchar('Content-Type: application/json; charset=UTF-8'));
        if not HasHeader('Accept') then
          headers := curl_slist_append(headers, pchar('Accept: application/json'));
        curl_easy_setopt(handle, CURLOPT_POST, clong(1));
        curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(jsondata));
        curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, clong(Length(jsondata)));
      end
      else if post then
      begin
        // Add Content-Type for form data
        if not HasHeader('Content-Type') then
          headers := curl_slist_append(headers, pchar('Content-Type: application/x-www-form-urlencoded'));
        
        if Length(params) > 0 then
        begin
          sx := '';
          for j := 0 to High(params) do
          begin
            if j > 0 then
              sx := sx + '&';
            sx := sx + params[j];
          end;

          maskedSx := sx; // short-lived local copy
          MaskParam(maskedSx, 'code_verifier');
          MaskParam(maskedSx, 'code');
          MaskParam(maskedSx, 'password');
          MaskParam(maskedSx, 'client_secret');

          LogMessageToFile('HTTP POST body (masked): ' + Copy(maskedSx, 1, 2000));

          curl_easy_setopt(handle, CURLOPT_POST, clong(1));
          curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(sx));
          curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, clong(Length(sx)));
        end
        else
          curl_easy_setopt(handle, CURLOPT_POST, clong(1));
      end;

      if headers <> nil then
        curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

      // Response callbacks
      curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallbackEx));
      curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));
      curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION, Pointer(@CurlHeaderCallbackEx));
      curl_easy_setopt(handle, CURLOPT_HEADERDATA, Pointer(headerStream));

      // Perform request
      errCode := curl_easy_perform(handle);

      if errCode = CURLE_OK then
      begin
        endTick := GetTickCount64;
        Result.Success := true;
        Result.Body := responseStream.DataString;

        // Get status code
        curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, @responseCode);
        Result.StatusCode := responseCode;

        // Get final URL after redirects
        curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, @effectiveUrl);
        if effectiveUrl <> nil then
          Result.FinalURL := string(effectiveUrl);

        // Get redirect count
        curl_easy_getinfo(handle, CURLINFO_REDIRECT_COUNT, @Result.RedirectCount);

        LogMessageToFile(Format('HTTP %s (curl) ok: status=%d, bytes=%d, redirects=%d, ms=%d',
          [methodLabel, Result.StatusCode, Length(Result.Body), Result.RedirectCount, endTick - startTick]));

        // Parse headers
        headerStream.Position := 0;
        responseLine := '';
        while headerStream.Position < headerStream.Size do
        begin
          // Read header line by line
          responseLine := '';
          while (headerStream.Position < headerStream.Size) do
          begin
            i := Ord(headerStream.ReadByte);
            if i = 10 then // LF
              Break;
            if i <> 13 then // Skip CR
              responseLine := responseLine + Chr(i);
          end;
          
          responseLine := Trim(responseLine);
          if responseLine <> '' then
          begin
            // Check for Set-Cookie headers
            if Pos('Set-Cookie:', responseLine) = 1 then
            begin
              cookieVal := Trim(Copy(responseLine, 13, MaxInt));
              cookiePos := Pos(';', cookieVal);
              if cookiePos > 0 then
                cookieVal := Copy(cookieVal, 1, cookiePos - 1);
              if cookieVal <> '' then
              begin
                Result.Cookies.Add(cookieVal);
                if cookieJar <> nil then
                begin
                  if cookieJar.IndexOf(cookieVal) = -1 then
                    cookieJar.Add(cookieVal);
                end;
              end;
            end;
            Result.Headers.Add(responseLine);
          end;
        end;
      end
      else
      begin
        endTick := GetTickCount64;
        Result.Success := false;
        Result.ErrorMessage := string(curl_easy_strerror(errCode));
        LogMessageToFile(Format('HTTP %s (curl) error: code=%d, msg=%s, ms=%d',
          [methodLabel, Ord(errCode), Result.ErrorMessage, endTick - startTick]));
      end;

    finally
      curl_easy_cleanup(handle);
    end;

  finally
    if headers <> nil then
      curl_slist_free_all(headers);
    responseStream.Free;
    headerStream.Free;
  end;
end;
{$ELSEIF DEFINED(HAIKU)}
function TTrndiNativeBase.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
cookieJar: TStringList = nil; followRedirects: boolean = true;
maxRedirects: integer = 10; customHeaders: TStringList = nil;
prefix: boolean = true): THTTPResponse;
var
  HTTP: TFPHTTPClient;
  address, sx, maskedSx, bodyData, hdr, headerLine: string;
  i, j, redirectCount: integer;
  response: string;
  statusCode: integer;
  cookieVal: string;
  cookiePos: integer;
  currentPost: boolean;
  function HasHeaderLocal(const Name: string): boolean;
  var k: integer; s: string;
  begin
    Result := False;
    if customHeaders = nil then Exit;
    s := LowerCase(Name) + ':';
    for k := 0 to customHeaders.Count - 1 do
      if Pos(s, LowerCase(customHeaders[k])) = 1 then
        Exit(True);
  end;
begin
  // Initialize result structure
  Result.Body := '';
  Result.Headers := TStringList.Create;
  Result.Cookies := TStringList.Create;
  Result.Success := false;
  Result.StatusCode := 0;
  Result.RedirectCount := 0;
  Result.FinalURL := '';
  Result.ErrorMessage := '';

  // Start address
  address := endpoint;

  // Append query params ONLY for GET requests (not POST)
  if (not post) and (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
  end;

  // Prepare body for POST
  if post and (jsondata = '') and (Length(params) > 0) then
  begin
    bodyData := '';
    for j := 0 to High(params) do
    begin
      if j > 0 then bodyData := bodyData + '&';
      bodyData := bodyData + params[j];
    end;
  end
  else if jsondata <> '' then
    bodyData := jsondata
  else
    bodyData := '';

  redirectCount := 0;
  currentPost := post;

  // Manual redirect loop so we can observe headers and cookies
  while True do
  begin
    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.AllowRedirect := False; // handle redirects ourselves
      HTTP.IOTimeout := 30000; // 30s

      if useragent <> '' then
        HTTP.AddHeader('User-Agent', useragent);

      // Add custom headers (support 'Name: Value' or 'Name=Value')
      if customHeaders <> nil then
      begin
        for i := 0 to customHeaders.Count - 1 do
        begin
          hdr := customHeaders[i];
          if Pos(':', hdr) > 0 then
          begin
            HTTP.AddHeader(Trim(Copy(hdr, 1, Pos(':', hdr) - 1)), Trim(Copy(hdr, Pos(':', hdr) + 1, MaxInt)));
          end
          else if Pos('=', hdr) > 0 then
          begin
            HTTP.AddHeader(Trim(Copy(hdr, 1, Pos('=', hdr) - 1)), Trim(Copy(hdr, Pos('=', hdr) + 1, MaxInt)));
          end
          else
            HTTP.RequestHeaders.Add(hdr);
        end;
      end;

      // Add cookies to request if provided
      if (cookieJar <> nil) and (cookieJar.Count > 0) then
      begin
        sx := '';
        for i := 0 to cookieJar.Count - 1 do
        begin
          if Trim(cookieJar[i]) = '' then Continue;
          if sx <> '' then sx := sx + '; ';
          sx := sx + cookieJar[i];
        end;
        if sx <> '' then
          HTTP.AddHeader('Cookie', sx);
      end;

      // Execute request
      try
        if currentPost then
        begin
          if jsondata <> '' then
          begin
            if not HasHeaderLocal('Content-Type') then
              HTTP.AddHeader('Content-Type', 'application/json; charset=UTF-8');
            if not HasHeaderLocal('Accept') then
              HTTP.AddHeader('Accept', 'application/json');
            response := HTTP.Post(address, bodyData);
          end
          else if bodyData <> '' then
          begin
            if not HasHeaderLocal('Content-Type') then
              HTTP.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
            maskedSx := bodyData;
            // Simple masking of known sensitive params for logs
            maskedSx := StringReplace(maskedSx, 'code_verifier=', 'code_verifier=***', [rfIgnoreCase]);
            maskedSx := StringReplace(maskedSx, 'code=', 'code=***', [rfIgnoreCase]);
            maskedSx := StringReplace(maskedSx, 'password=', 'password=***', [rfIgnoreCase]);
            maskedSx := StringReplace(maskedSx, 'client_secret=', 'client_secret=***', [rfIgnoreCase]);
            LogMessageToFile('HTTP POST body (masked): ' + Copy(maskedSx, 1, 2000));
            response := HTTP.Post(address, bodyData);
          end
          else
            response := HTTP.Post(address, '');
        end
        else
          response := HTTP.Get(address);
      except
        on E: Exception do
        begin
          Result.Success := False;
          Result.ErrorMessage := E.Message;
          // Capture any headers available
          for i := 0 to HTTP.ResponseHeaders.Count - 1 do
            Result.Headers.Add(HTTP.ResponseHeaders[i]);
          Exit;
        end;
      end;

      // Capture headers and cookies
      for i := 0 to HTTP.ResponseHeaders.Count - 1 do
      begin
        headerLine := HTTP.ResponseHeaders[i];
        Result.Headers.Add(headerLine);
        if Pos(LowerCase('set-cookie'), LowerCase(headerLine)) > 0 then
        begin
          cookieVal := headerLine;
          cookiePos := Pos(':', cookieVal);
          if cookiePos > 0 then cookieVal := Trim(Copy(cookieVal, cookiePos + 1, MaxInt));
          cookiePos := Pos(';', cookieVal);
          if cookiePos > 0 then cookieVal := Copy(cookieVal, 1, cookiePos - 1);
          if cookieVal <> '' then
          begin
            Result.Cookies.Add(cookieVal);
            if cookieJar <> nil then
              if cookieJar.IndexOf(cookieVal) = -1 then
                cookieJar.Add(cookieVal);
          end;
        end;
      end;

      statusCode := HTTP.ResponseStatusCode;
      Result.StatusCode := statusCode;
      Result.Body := response;
      Result.FinalURL := address;

      // Redirect handling
      if followRedirects and (statusCode in [301, 302, 303, 307, 308]) then
      begin
        // Find Location header
        for i := 0 to Result.Headers.Count - 1 do
        begin
          headerLine := Result.Headers[i];
          if LowerCase(Copy(headerLine, 1, 9)) = 'location:' then
          begin
            address := Trim(Copy(headerLine, 10, MaxInt));
            Inc(redirectCount);
            Result.RedirectCount := redirectCount;
            if (statusCode = 303) or (statusCode = 302) or (statusCode = 301) then
              currentPost := False; // convert to GET on common redirects
            Break;
          end;
        end;

        if redirectCount >= maxRedirects then
        begin
          Result.Success := False;
          Result.ErrorMessage := 'Too many redirects';
          Exit;
        end;

        Continue; // follow the Location
      end
      else
      begin
        Result.Success := (statusCode >= 200) and (statusCode < 300);
        Break;
      end;

    finally
      HTTP.Free;
    end;
  end; // while
end;
{$ELSEIF DEFINED(X_WIN)}
function TTrndiNativeBase.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
cookieJar: TStringList = nil; followRedirects: boolean = true;
maxRedirects: integer = 10; customHeaders: TStringList = nil;
prefix: boolean = true): THTTPResponse;
var
  address, sx, currentUrl, bodyData, methodLabel: string;
  responseHeaders: TStringList;
  responseBody: string;
  statusCode: integer;
  locationHeader: string;
  proxyHost, proxyPortS, proxyUser, proxyPass: string;
  proxyPort: integer;
  currentPost: boolean;
  startTick: QWord;
  endTick: QWord;

  procedure ParseURLLocal(const URL: string; out ServerName, Path: string;
    out port: HTTPPort);
  var
    ProtocolPos, PathPos, PortPos: integer;
    PortStr: string;
  begin
    ProtocolPos := Pos('://', URL);

    port.secure := false;
    port.port := 80;

    if ProtocolPos > 0 then
    begin
      port.secure := URL[ProtocolPos - 1] = 's';
      if port.secure then
        port.port := 443;
      ProtocolPos := ProtocolPos + 3;
    end
    else
      ProtocolPos := 1;

    PathPos := PosEx('/', URL, ProtocolPos);
    PortPos := PosEx(':', URL, ProtocolPos);

    if (PortPos > 0) and ((PathPos = 0) or (PortPos < PathPos)) then
    begin
      ServerName := Copy(URL, ProtocolPos, PortPos - ProtocolPos);
      if PathPos > 0 then
        PortStr := Copy(URL, PortPos + 1, PathPos - PortPos - 1)
      else
        PortStr := Copy(URL, PortPos + 1, MaxInt);
      port.port := StrToIntDef(PortStr, port.port);
    end
    else if PathPos > 0 then
      ServerName := Copy(URL, ProtocolPos, PathPos - ProtocolPos)
    else
      ServerName := Copy(URL, ProtocolPos, Length(URL) - ProtocolPos + 1);

    if PathPos > 0 then
      Path := Copy(URL, PathPos, Length(URL) - PathPos + 1)
    else
      Path := '/';
  end;

  function HasHeader(const AName: string; AHeaders: TStringList): boolean;
  var
    k: integer;
    nameLower: string;
  begin
    Result := false;
    if AHeaders = nil then
      Exit;
    nameLower := LowerCase(AName) + ':';
    for k := 0 to AHeaders.Count - 1 do
      if Pos(nameLower, LowerCase(Trim(AHeaders[k]))) = 1 then
        Exit(true);
  end;

  function BuildCookieHeader: string;
  var
    i: integer;
    cookieData: string;
  begin
    Result := '';
    if cookieJar = nil then
      Exit;
    cookieData := '';
    for i := 0 to cookieJar.Count - 1 do
    begin
      if Trim(cookieJar[i]) = '' then
        Continue;
      if cookieData <> '' then
        cookieData := cookieData + '; ';
      cookieData := cookieData + cookieJar[i];
    end;
    Result := cookieData;
  end;

  procedure UpdateCookiesFromHeaders(const AHeaders: TStringList);
  var
    i: integer;
    lineLower: string;
    cookieVal: string;
    cookiePos: integer;
  begin
    if AHeaders = nil then
      Exit;
    for i := 0 to AHeaders.Count - 1 do
    begin
      lineLower := LowerCase(Trim(AHeaders[i]));
      if Pos('set-cookie:', lineLower) = 1 then
      begin
        cookieVal := Trim(Copy(AHeaders[i], 12, MaxInt));
        cookiePos := Pos(';', cookieVal);
        if cookiePos > 0 then
          cookieVal := Copy(cookieVal, 1, cookiePos - 1);
        if cookieVal <> '' then
        begin
          Result.Cookies.Add(cookieVal);
          if cookieJar <> nil then
          begin
            if cookieJar.IndexOf(cookieVal) = -1 then
              cookieJar.Add(cookieVal);
          end;
        end;
      end;
    end;
  end;

  function ExtractLocationHeader(const AHeaders: TStringList): string;
  var
    i: integer;
    lineLower: string;
  begin
    Result := '';
    if AHeaders = nil then
      Exit;
    for i := 0 to AHeaders.Count - 1 do
    begin
      lineLower := LowerCase(Trim(AHeaders[i]));
      if Pos('location:', lineLower) = 1 then
      begin
        Result := Trim(Copy(AHeaders[i], 10, MaxInt));
        Exit;
      end;
    end;
  end;

  function ParseStatusCodeFromHeaders(const AHeaders: TStringList): integer;
  var
    statusLine: string;
    p1, p2: integer;
  begin
    Result := 0;
    if (AHeaders = nil) or (AHeaders.Count = 0) then
      Exit;
    statusLine := Trim(AHeaders[0]);
    p1 := Pos(' ', statusLine);
    if p1 > 0 then
    begin
      p2 := PosEx(' ', statusLine, p1 + 1);
      if p2 > p1 then
        Result := StrToIntDef(Copy(statusLine, p1 + 1, p2 - p1 - 1), 0)
      else
        Result := StrToIntDef(Copy(statusLine, p1 + 1, MaxInt), 0);
    end;
  end;

  function ResolveUrl(const baseUrl, location: string): string;
  var
    lowerLoc: string;
    schemePos: integer;
    rootPos: integer;
    baseRoot: string;
    baseDir: string;
  begin
    Result := location;
    lowerLoc := LowerCase(location);
    if (Pos('http://', lowerLoc) = 1) or (Pos('https://', lowerLoc) = 1) then
      Exit;

    schemePos := Pos('://', baseUrl);
    if schemePos = 0 then
      Exit;

    rootPos := PosEx('/', baseUrl, schemePos + 3);
    if rootPos = 0 then
      baseRoot := baseUrl
    else
      baseRoot := Copy(baseUrl, 1, rootPos - 1);

    if (Length(location) > 0) and (location[1] = '/') then
      Result := baseRoot + location
    else
    begin
      baseDir := Copy(baseUrl, 1, LastDelimiter('/', baseUrl));
      Result := baseDir + location;
    end;
  end;

  function TryRequest(const url: string; const isPost: boolean; const requestBody: string;
    const useProxy: boolean; const forceNoProxy: boolean; out outBody: string;
    out outHeaders: TStringList; out outStatus: integer; out outLocation: string;
    out outError: string): boolean;
  var
    hSession, hConnect, hRequest: HINTERNET;
    serverName, path: string;
    port: HTTPPort;
    flags: DWORD;
    dwSize, dwDownloaded: DWORD;
    dwToRead: DWORD;
    buffer: array[0..8192] of byte;
    responseStream: TStringStream;
    headersToSend: TStringList;
    cookieHeader: string;
    headerLine: WideString;
    rawHeaderBuf: PWideChar;
    rawHeaderStr: WideString;
    index: DWORD;
    statusValue: DWORD;
    statusSize: DWORD;
    locBuf: PWideChar;
    locSize: DWORD;
    bodyPtr: Pointer;
    bodyLen: DWORD;
    sendVerb: PWideChar;
    i: integer;
    redirectPolicy: DWORD;
    cookieBuf: PWideChar;
    cookieSize: DWORD;
    cookieIndex: DWORD;
    cookieVal: WideString;
    function AppendSetCookieHeaders(hReq: HINTERNET; AHeaders: TStringList): boolean;
    begin
      Result := false;
      if AHeaders = nil then
        Exit;
      cookieIndex := 0;
      repeat
        cookieSize := 0;
        WinHttpQueryHeaders(hReq, WINHTTP_QUERY_SET_COOKIE, nil, nil, cookieSize, cookieIndex);
        if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (cookieSize > 0) then
        begin
          GetMem(cookieBuf, cookieSize);
          try
            if WinHttpQueryHeaders(hReq, WINHTTP_QUERY_SET_COOKIE, nil, cookieBuf, cookieSize, cookieIndex) then
            begin
              cookieVal := WideString(cookieBuf);
              if Trim(cookieVal) <> '' then
                AHeaders.Add('Set-Cookie: ' + string(cookieVal));
              Result := true;
            end;
          finally
            FreeMem(cookieBuf);
          end;
          Continue;
        end;
        Break;
      until false;
    end;
  begin
    Result := false;
    outBody := '';
    outHeaders := TStringList.Create;
    outHeaders.TextLineBreakStyle := tlbsCRLF;
    outStatus := 0;
    outLocation := '';
    outError := '';

    ParseURLLocal(url, serverName, path, port);

    if useProxy and (proxyHost <> '') then
      hSession := WinHttpOpen(pwidechar(widestring(useragent)), WINHTTP_ACCESS_TYPE_NAMED_PROXY,
        pwidechar(widestring(proxyHost + ':' + IntToStr(proxyPort))), WINHTTP_NO_PROXY_BYPASS, 0)
    else if forceNoProxy then
      hSession := WinHttpOpen(pwidechar(widestring(useragent)), WINHTTP_ACCESS_TYPE_NO_PROXY,
        WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0)
    else
      hSession := WinHttpOpen(pwidechar(widestring(useragent)), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
        WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);

    if hSession = nil then
    begin
      outError := 'WinHttpOpen failed: ' + SysErrorMessage(GetLastError);
      Exit(false);
    end;

    try
      if port.secure then
      begin
        flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
        if not WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @flags, SizeOf(flags)) then
        begin
          flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
          WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @flags, SizeOf(flags));
        end;
      end;

      if not WinHttpSetTimeouts(hSession, 15000, 15000, 30000, 120000) then
      begin
        outError := 'WinHttpSetTimeouts failed (' + IntToStr(GetLastError) + '): ' +
          SysErrorMessage(GetLastError);
        Exit(false);
      end;

      hConnect := WinHttpConnect(hSession, pwidechar(widestring(serverName)), port.port, 0);
      if hConnect = nil then
      begin
        outError := 'WinHttpConnect failed: ' + SysErrorMessage(GetLastError);
        Exit(false);
      end;

      try
        flags := 0;
        if port.secure then
          flags := WINHTTP_FLAG_SECURE;

        if isPost then
          sendVerb := 'POST'
        else
          sendVerb := 'GET';

        hRequest := WinHttpOpenRequest(hConnect, sendVerb, pwidechar(widestring(path)),
          nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, flags);
        if hRequest = nil then
        begin
          outError := 'WinHttpOpenRequest failed: ' + SysErrorMessage(GetLastError);
          Exit(false);
        end;

        try
          redirectPolicy := WINHTTP_OPTION_REDIRECT_POLICY_NEVER;
          WinHttpSetOption(hRequest, WINHTTP_OPTION_REDIRECT_POLICY, @redirectPolicy, SizeOf(redirectPolicy));

          if (useProxy and (proxyHost <> '')) and ((proxyUser <> '') or (proxyPass <> '')) then
          begin
            if proxyUser <> '' then
            begin
              headerLine := WideString(proxyUser);
              dwSize := (Length(headerLine) + 1) * SizeOf(WideChar);
              WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_USERNAME, PWideChar(headerLine), dwSize);
            end;
            if proxyPass <> '' then
            begin
              headerLine := WideString(proxyPass);
              dwSize := (Length(headerLine) + 1) * SizeOf(WideChar);
              WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_PASSWORD, PWideChar(headerLine), dwSize);
            end;
          end;

          headersToSend := TStringList.Create;
          try
            headersToSend.TextLineBreakStyle := tlbsCRLF;

            if customHeaders <> nil then
              headersToSend.AddStrings(customHeaders);

            cookieHeader := BuildCookieHeader;
            if cookieHeader <> '' then
              headersToSend.Add('Cookie: ' + cookieHeader);

            if jsondata <> '' then
            begin
              if not HasHeader('Content-Type', headersToSend) then
                headersToSend.Add('Content-Type: application/json; charset=UTF-8');
              if not HasHeader('Accept', headersToSend) then
                headersToSend.Add('Accept: application/json');
            end
            else if isPost and (Length(params) > 0) then
            begin
              if not HasHeader('Content-Type', headersToSend) then
                headersToSend.Add('Content-Type: application/x-www-form-urlencoded');
            end;

            for i := 0 to headersToSend.Count - 1 do
            begin
              if Trim(headersToSend[i]) = '' then
                Continue;
              headerLine := WideString(headersToSend[i] + #13#10);
              WinHttpAddRequestHeaders(hRequest, PWideChar(headerLine), Length(headerLine),
                WINHTTP_ADDREQ_FLAG_ADD);
            end;
          finally
            headersToSend.Free;
          end;

          if requestBody <> '' then
          begin
            bodyPtr := @requestBody[1];
            bodyLen := Length(requestBody);
          end
          else
          begin
            bodyPtr := nil;
            bodyLen := 0;
          end;

          if not WinHttpSendRequest(hRequest, nil, 0, bodyPtr, bodyLen, bodyLen, 0) then
          begin
            outError := 'WinHttpSendRequest failed (' + IntToStr(GetLastError) + '): ' +
              SysErrorMessage(GetLastError);
            Exit(false);
          end;

          if not WinHttpReceiveResponse(hRequest, nil) then
          begin
            outError := 'WinHttpReceiveResponse failed (' + IntToStr(GetLastError) + '): ' +
              SysErrorMessage(GetLastError);
            Exit(false);
          end;

          statusValue := 0;
          statusSize := SizeOf(statusValue);
          index := 0;
          if WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER,
            nil, @statusValue, statusSize, index) then
            outStatus := statusValue;

          dwSize := 0;
          index := 0;
          WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF, nil, nil, dwSize, index);
          if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (dwSize > 0) then
          begin
            GetMem(rawHeaderBuf, dwSize);
            try
              if WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF, nil, rawHeaderBuf, dwSize, index) then
              begin
                rawHeaderStr := WideString(rawHeaderBuf);
                outHeaders.Text := rawHeaderStr;
              end;
            finally
              FreeMem(rawHeaderBuf);
            end;
          end;

          AppendSetCookieHeaders(hRequest, outHeaders);

          if outStatus = 0 then
            outStatus := ParseStatusCodeFromHeaders(outHeaders);

          locSize := 0;
          index := 0;
          WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_LOCATION, nil, nil, locSize, index);
          if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (locSize > 0) then
          begin
            GetMem(locBuf, locSize);
            try
              if WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_LOCATION, nil, locBuf, locSize, index) then
              begin
                outLocation := WideString(locBuf);
              end;
            finally
              FreeMem(locBuf);
            end;
          end;

          if outLocation = '' then
            outLocation := ExtractLocationHeader(outHeaders);

          responseStream := TStringStream.Create;
          try
            repeat
              dwSize := 0;
              if not WinHttpQueryDataAvailable(hRequest, dwSize) then
              begin
                outError := 'WinHttpQueryDataAvailable failed (' + IntToStr(GetLastError) + '): ' +
                  SysErrorMessage(GetLastError);
                Exit(false);
              end;

              if dwSize = 0 then
                Break;

              dwToRead := dwSize;
              if dwToRead > SizeOf(buffer) then
                dwToRead := SizeOf(buffer);

              if not WinHttpReadData(hRequest, @buffer, dwToRead, dwDownloaded) then
              begin
                outError := 'WinHttpReadData failed (' + IntToStr(GetLastError) + '): ' +
                  SysErrorMessage(GetLastError);
                Exit(false);
              end;
              responseStream.WriteBuffer(buffer, dwDownloaded);
            until dwSize = 0;

            outBody := responseStream.DataString;
          finally
            responseStream.Free;
          end;

          Result := true;
        finally
          WinHttpCloseHandle(hRequest);
        end;
      finally
        WinHttpCloseHandle(hConnect);
      end;
    finally
      WinHttpCloseHandle(hSession);
    end;
    if not Result then
    begin
      outHeaders.Free;
      outHeaders := nil;
    end;
  end;

begin
  address := endpoint;
  Result.Body := '';
  Result.Headers := TStringList.Create;
  Result.Cookies := TStringList.Create;
  Result.Success := false;
  Result.StatusCode := 0;
  Result.RedirectCount := 0;
  Result.FinalURL := '';
  Result.ErrorMessage := '';

  if prefix then
    address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])])
  else
    address := endpoint;

  if (not post) and (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
  end;

  bodyData := '';
  if jsondata <> '' then
    bodyData := jsondata
  else if post and (Length(params) > 0) then
  begin
    for sx in params do
    begin
      if bodyData <> '' then
        bodyData := bodyData + '&';
      bodyData := bodyData + sx;
    end;
  end;

  if post then
    methodLabel := 'POST'
  else
    methodLabel := 'GET';

  currentUrl := address;
  currentPost := post;

  proxyHost := Trim(GetRootSetting('proxy.host', ''));
  proxyPortS := Trim(GetRootSetting('proxy.port', ''));
  proxyPort := StrToIntDef(proxyPortS, 8080);
  proxyUser := GetRootSetting('proxy.user', '');
  proxyPass := GetRootSetting('proxy.pass', '');

  repeat
    startTick := GetTickCount64;
    LogMessageToFile(Format('HTTP %s (winhttp): %s', [methodLabel, currentUrl]));

    if proxyHost <> '' then
    begin
      if not TryRequest(currentUrl, currentPost, bodyData, true, false,
        responseBody, responseHeaders, statusCode, locationHeader, Result.ErrorMessage) then
      begin
        if not TryRequest(currentUrl, currentPost, bodyData, false, true,
          responseBody, responseHeaders, statusCode, locationHeader, Result.ErrorMessage) then
          Exit;
      end;
    end
    else
    begin
      if not TryRequest(currentUrl, currentPost, bodyData, false, true,
        responseBody, responseHeaders, statusCode, locationHeader, Result.ErrorMessage) then
        Exit;
    end;

    endTick := GetTickCount64;

    Result.Body := responseBody;
    Result.StatusCode := statusCode;
    Result.Headers.Assign(responseHeaders);
    UpdateCookiesFromHeaders(responseHeaders);
    responseHeaders.Free;
    Result.FinalURL := currentUrl;

    LogMessageToFile(Format('HTTP %s (winhttp) status=%d, bytes=%d, redirects=%d, ms=%d',
      [methodLabel, Result.StatusCode, Length(Result.Body), Result.RedirectCount, endTick - startTick]));

    if not followRedirects then
      Break;

    if not ((Result.StatusCode = 301) or (Result.StatusCode = 302) or
      (Result.StatusCode = 303) or (Result.StatusCode = 307) or (Result.StatusCode = 308)) then
      Break;

    if locationHeader = '' then
      Break;

    Inc(Result.RedirectCount);
    if Result.RedirectCount > maxRedirects then
    begin
      Result.ErrorMessage := 'Too many redirects';
      Exit;
    end;

    currentUrl := ResolveUrl(currentUrl, locationHeader);
    Result.FinalURL := currentUrl;

    if (Result.StatusCode = 303) or (((Result.StatusCode = 301) or (Result.StatusCode = 302)) and currentPost) then
    begin
      currentPost := false;
      bodyData := '';
      methodLabel := 'GET';
    end;

  until false;

  Result.Success := true;
end;
{$ELSE}
// Fallback for non-Linux platforms (Mac/Windows)
function TTrndiNativeBase.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
cookieJar: TStringList = nil; followRedirects: boolean = true;
maxRedirects: integer = 10; customHeaders: TStringList = nil;
prefix: boolean = true): THTTPResponse;
var
  httpClient: TNSHTTPSendAndReceive;
  currentUrl: string;
  sendStream: TStringStream;
  respStream: TStringStream;
  responseHeaders: TStringList;
  requestHeaders: TStringList;
  status: Integer;
  location: string;
  isPost: boolean;
  sx: string;
  j: Integer;
  hIdx: Integer;
  tmpS, nm, vl: string;

  (* Update cookies from headers into Result.Cookies and cookieJar (if provided). *)
  procedure UpdateCookiesFromHeadersLocal(const AHeaders: TStringList);
  var
    i: integer;
    lineLower: string;
    cookieVal: string;
    cookiePos: integer;
  begin
    if AHeaders = nil then
      Exit;
    for i := 0 to AHeaders.Count - 1 do
    begin
      lineLower := LowerCase(Trim(AHeaders[i]));
      if Pos('set-cookie:', lineLower) = 1 then
      begin
        cookieVal := Trim(Copy(AHeaders[i], 12, MaxInt));
        cookiePos := Pos(';', cookieVal);
        if cookiePos > 0 then
          cookieVal := Copy(cookieVal, 1, cookiePos - 1);
        if cookieVal <> '' then
        begin
          Result.Cookies.Add(cookieVal);
          if cookieJar <> nil then
          begin
            if cookieJar.IndexOf(cookieVal) = -1 then
              cookieJar.Add(cookieVal);
          end;
        end;
      end;
    end;
  end;

  (* Extract a Location: header value if present *)
  function ExtractLocationHeaderLocal(const AHeaders: TStringList): string;
  var
    i: integer;
    lineLower: string;
  begin
    Result := '';
    if AHeaders = nil then
      Exit;
    for i := 0 to AHeaders.Count - 1 do
    begin
      lineLower := LowerCase(Trim(AHeaders[i]));
      if Pos('location:', lineLower) = 1 then
      begin
        Result := Trim(Copy(AHeaders[i], 10, MaxInt));
        Exit;
      end;
    end;
  end;

  (* Resolve a possibly-relative Location header against base URL *)
  function ResolveUrlLocal(const baseUrl, location: string): string;
  var
    lowerLoc: string;
    schemePos: integer;
    rootPos: integer;
    baseRoot: string;
    baseDir: string;
  begin
    Result := location;
    lowerLoc := LowerCase(location);
    if (Pos('http://', lowerLoc) = 1) or (Pos('https://', lowerLoc) = 1) then
      Exit;

    schemePos := Pos('://', baseUrl);
    if schemePos = 0 then
      Exit;

    rootPos := PosEx('/', baseUrl, schemePos + 3);
    if rootPos = 0 then
      baseRoot := baseUrl
    else
      baseRoot := Copy(baseUrl, 1, rootPos - 1);

    if (Length(location) > 0) and (location[1] = '/') then
      Result := baseRoot + location
    else
    begin
      baseDir := Copy(baseUrl, 1, LastDelimiter('/', baseUrl));
      Result := baseDir + location;
    end;
  end;

begin
  // Initialize result
  Result.Body := '';
  Result.Headers := TStringList.Create;
  Result.Cookies := TStringList.Create;
  Result.StatusCode := 0;
  Result.FinalURL := '';
  Result.RedirectCount := 0;
  Result.Success := false;
  Result.ErrorMessage := '';

  // Build initial URL
  if prefix then
    currentUrl := Format('%s/%s', [baseUrl, endpoint])
  else
    currentUrl := endpoint;

  // Append query params for GET requests
  if (not post) and (Length(params) > 0) then
  begin
    currentUrl := currentUrl + '?';
    for status := 0 to High(params) do
      currentUrl := currentUrl + '&' + params[status];
  end;

  // Prepare request headers and body
  requestHeaders := TStringList.Create;
  if customHeaders <> nil then
  begin
    requestHeaders.Assign(customHeaders);
    // Normalize headers: convert "Name: value" entries to Name=Value pairs
    // because mac NSURLRequest code expects Name=Value format when using
    // TStringList.Names/ValueFromIndex (unlike curl which expects 'Name: value' text).
    for hIdx := requestHeaders.Count - 1 downto 0 do
    begin
      tmpS := Trim(requestHeaders[hIdx]);
      if tmpS = '' then
        Continue;
      if Pos(':', tmpS) > 0 then
      begin
        nm := Trim(Copy(tmpS, 1, Pos(':', tmpS) - 1));
        vl := Trim(Copy(tmpS, Pos(':', tmpS) + 1, MaxInt));
        requestHeaders.Delete(hIdx);
        requestHeaders.Values[nm] := vl;
      end;
    end;
  end;
  if useragent <> '' then
    requestHeaders.Values['User-Agent'] := useragent;

  sendStream := TStringStream.Create('');
  isPost := post;
  if jsondata <> '' then
  begin
    // JSON -> POST
    isPost := true;
    sendStream.WriteString(jsondata);
    requestHeaders.Values['Content-Type'] := 'application/json';
    requestHeaders.Values['Content-Length'] := IntToStr(sendStream.Size);
  end
  else if isPost and (Length(params) > 0) then
  begin
    // Build form-encoded body from params for POST (application/x-www-form-urlencoded)
    sx := '';
    for j := 0 to High(params) do
    begin
      if j > 0 then
        sx := sx + '&';
      sx := sx + params[j];
    end;

    sendStream.WriteString(sx);
    requestHeaders.Values['Content-Type'] := 'application/x-www-form-urlencoded';
    requestHeaders.Values['Content-Length'] := IntToStr(sendStream.Size);
  end;

  try
    repeat
      respStream := TStringStream.Create('');
      responseHeaders := TStringList.Create;
      httpClient := TNSHTTPSendAndReceive.Create;
      try
        httpClient.address := currentUrl;
        if isPost then
          httpClient.method := 'POST'
        else
          httpClient.method := 'GET';

        // Perform request
        if not httpClient.SendAndReceiveEx(sendStream, respStream, requestHeaders, responseHeaders, status, currentUrl) then
        begin
          Result.ErrorMessage := httpClient.LastErrMsg;
          Exit;
        end;

        // Normalize response
        Result.Body := Trim(respStream.DataString);
        Result.StatusCode := status;
        Result.Headers.Assign(responseHeaders);
        Result.FinalURL := currentUrl;

        // Update cookies from headers
        UpdateCookiesFromHeadersLocal(responseHeaders);

        // Handle redirects if requested
        if (not followRedirects) then
          Break;

        if not ((Result.StatusCode = 301) or (Result.StatusCode = 302) or
          (Result.StatusCode = 303) or (Result.StatusCode = 307) or (Result.StatusCode = 308)) then
          Break;

        location := ExtractLocationHeaderLocal(responseHeaders);
        if location = '' then
          Break;

        Inc(Result.RedirectCount);
        if Result.RedirectCount > maxRedirects then
        begin
          Result.ErrorMessage := 'Too many redirects';
          Exit;
        end;

        currentUrl := ResolveUrlLocal(currentUrl, location);

        // per RFC, change method to GET in some cases
        if (Result.StatusCode = 303) or (((Result.StatusCode = 301) or (Result.StatusCode = 302)) and isPost) then
        begin
          isPost := false;
          sendStream.Size := 0;
          requestHeaders.Values['Content-Length'] := '0';
        end;

      finally
        httpClient.Free;
        respStream.Free;
        responseHeaders.Free;
      end;
    until false;

    Result.Success := True;
  finally
    requestHeaders.Free;
    sendStream.Free;
  end;
end;
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
  TryGetSetting
  -------------------------
  Gets a setting, returns true if the reading is non-empty and the value in def
  -----------------------------------------------------------------------------}
function TTrndiNativeBase.TryGetSetting(const keyname: string; out res: string;
global: boolean = false): boolean;
begin
  res := GetSetting(keyname, '', global);
  if res = '' then
    Result := false
  else
    Result := true;
end;

{------------------------------------------------------------------------------
  TryGetCSVSetting
  -------------------------
  Gets a list of settings, returns true if the reading is non-empty and the value in def
  -----------------------------------------------------------------------------}
function TTrndiNativeBase.TryGetCSVSetting(const keyname: string; out res: TStringArray;
global: boolean = false): boolean;
var
  val: string;
begin
  if self.TryGetSetting(keyname, val, global) then
    with TStringList.Create do
    begin
      Clear;
      CommaText := val;
      res := ToStringArray;
      Free;
    end
  else
    result := false;
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
  SetColorSetting
  ----------------------
  Overloaded function to save integers
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetSetting(const keyname: string; const val: integer;
global: boolean = false);
begin
  SetSetting(keyname, inttostr(val), global);
end;

{------------------------------------------------------------------------------
  SetColorSetting
  ----------------------
  Overloaded function to save integers
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetSetting(const keyname: string; const val: boolean;
global: boolean = false); overload;
begin
  if val then
    SetSetting(keyname, 'true', global)
  else
    SetSetting(keyname, 'false', global);
end;

{------------------------------------------------------------------------------
  SetWideCharSetting
  ----------------------
  Stores a WChar value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetWideCharSetting(const keyname: string; val: WChar);
var
  code: string;
begin
  code := IntToHex(Ord(val), 4);
  SetSetting(keyname, code);
end;

{------------------------------------------------------------------------------
  GetWideCharSetting
  -------------------------
  Returns a WChar from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetWideCharSetting(const keyname: string;
const def: WChar = WChar($2B24)): WChar;
var
  code: integer;
  s: string;
begin
  s := GetSetting(keyname, '');

  if s.IsEmpty then
    Exit(def);

  code := StrToInt('$' + s);

  Result := WChar(code);
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
  Stores a bool value to platform-specific storage, with custom values.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetBoolSetting(const keyname: string; const val: boolean; const a,b: string; const global: boolean = false);
begin
  if val then
    SetSetting(keyname, a, global)
  else
    SetSetting(keyname, b, global);
end;

{------------------------------------------------------------------------------
  SetCSVSetting
  ----------------------
  Stores a bool value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetCSVSetting(const keyname: string; const val: TStringArray; const global: boolean = false);
var
  s, res: string;
begin
  res := '';
  for s in val do
    res += s + ',';

  if Length(res) > 0 then
    Delete(res, Length(res), 1);

  SetSetting(keyname, res, global);
end;

{------------------------------------------------------------------------------
  SetFloatSetting
  ----------------------
  Store a float value to platform-specific storage using '.' decimal separator.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetFloatSetting(const keyname: string; const val: single; const global: boolean = false);
var
  f: TFormatSettings;
begin
  f := DefaultFormatSettings;
  f.DecimalSeparator := '.';
  SetSetting(keyname, FormatFloat('0.00', val, f), global);
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
  HasGlobalMenu (class, virtual)
  ------------------------------
  Default: platform implementations can override to indicate whether a
  global/appmenu service is available; base returns False.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.HasGlobalMenu: boolean;
begin
  Result := False;
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

  // Check environment variables
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

  // Additional checks if we haven't found WSL yet
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
