(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2026 Björn Lindh.
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
  Classes, SysUtils, Graphics, trndi.log, SyncObjs, StrUtils;

type
{$ifdef X_LINUXBSD}
  {** WSL version detection result. }
TWSLVersion = (wslNone, wslVersion1, wslVersion2, wslUnknown);
{$endif}
  {** Ternary-style boolean with Unset/Unknown states for user overrides. }
TTrndiBool = (tbUnset, tbTrue, tbFalse, tbUnknown);

{$ifdef X_LINUXBSD}
  {** Information about a WSL environment (Windows Subsystem for Linux). }
TWSLInfo = record
  IsWSL: boolean;       // True when running under WSL
  Version: TWSLVersion; // Detected version (1 or 2) when applicable
  DistroName: string;   // Optional distro name (if available)
  KernelVersion: string;// Kernel string as reported by /proc/version
end;
{$endif}

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
    // Track if we've shown a TTS error this session to avoid spam
  ttsErrorShown: boolean;

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
    const header: string = ''; prefix: boolean = true): string; virtual;
  
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
    prefix: boolean = true): THTTPResponse; virtual;
  {** Run the platform `requestEx` implementation on a background thread and
      wait for the result with a timeout. This preserves synchronous caller
      semantics without busy polling. Returns a THTTPResponse; on timeout the
      returned record has `Success = false` and `ErrorMessage = 'timeout'.` }
  function RequestExWait(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    cookieJar: TStringList = nil; followRedirects: boolean = true;
    maxRedirects: integer = 10; customHeaders: TStringList = nil;
    prefix: boolean = true; TimeoutMs: cardinal = 5000): THTTPResponse;

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
    {** Store an alignment setting. }
  procedure SetAlignmentSetting(const keyname: string; val: TAlignment; const global: boolean = false);
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
    {** Read an alignment setting or @param(def). }
  function GetAlignmentSetting(const keyname: string; def: TAlignment = taCenter): TAlignment;
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

    {** Export all settings to INI format string. }
  function ExportSettings: string; virtual; abstract;
    {** Import settings from INI format string. }
  procedure ImportSettings(const iniData: string); virtual; abstract;

    {**
      Process-wide cache for settings reads. The OS-level storage (Windows
      registry, Linux/Haiku ini, macOS NSUserDefaults) is a singleton, so the
      cache is too — multiple TrndiNative instances all observe the same
      cache and stay coherent with each other's writes.

      Platforms opt in by routing GetSetting/SetSetting/DeleteSetting through
      the helpers below. Keys stored in the cache are the @italic(full) key
      (post-buildKey).

      The "warm" flag means the cache holds a complete snapshot of the store,
      so a missed lookup is authoritative (return def). When false, a miss
      must fall through to the backing store.
    }
  class var FSettingsCache: TStringList;
  class var FSettingsCacheLock: SyncObjs.TCriticalSection;
  class var FSettingsCacheWarm: boolean;

    {** Lazy-init helper for the cache + lock (also called from the unit's
        initialization section). }
  class procedure EnsureSettingsCacheInit; static;
    {** Free the cache + lock on unit finalization. }
  class procedure ShutdownSettingsCache; static;
    {** Try to read a fully-resolved key from the cache. Returns True if a
        cached value was found OR the cache is warm and the key is absent
        (in which case @code(val) is empty). Returns False only when the
        cache is cold and the caller must consult the backing store. }
  class function TryGetCachedSetting(const fullKey: string;
    out val: string): boolean; static;
    {** Update or insert a single cached value. Safe to call on a cold
        cache — it only adds the single entry without flipping the warm
        flag. }
  class procedure SetCachedSetting(const fullKey, val: string); static;
    {** Remove a key from the cache (no-op if not present). }
  class procedure RemoveCachedSetting(const fullKey: string); static;
    {** Drop all cached values and clear the warm flag. The next read will
        re-warm via the platform's snapshot path (or fall through to the
        backing store and cache results lazily). }
  class procedure ClearSettingsCache; static;
    {** Replace the cache contents with a complete snapshot of the backing
        store and mark the cache warm. @code(Snapshot) must have entries in
        @code(name=value) form; ownership stays with the caller. }
  class procedure SeedSettingsCache(Snapshot: TStrings); static;

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
  class function DetectTouchScreen(out multi: boolean): boolean; virtual;
    {** Detect if the device has a touchscreen and whether it's multi-touch. }
  class function HasTouchScreen(out multi: boolean): boolean;
  class function HasTouchScreen: boolean;
    {** Simple HTTP GET helper; platform units implement. }
  class function getURL(const url: string; out res: string): boolean; virtual; abstract;
    {** Simple HTTP POST helper; platform units implement.
        @param(url) Full URL to POST to.
        @param(body) Request body (typically JSON).
        @param(contentType) Value for the Content-Type header (e.g. 'application/json').
                            Pass an empty string to omit.
        @param(res) Response body on success, error message on failure.
        @returns True on success. }
  class function postURL(const url: string; const body: string;
    const contentType: string; out res: string): boolean; virtual; abstract;
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
  class function GetOSLanguage: string; virtual;
  class function HasDangerousChars(const FileName: string): boolean; virtual;
    // Notifications
    {** True if a native notification system is available (override per platform). }
  class function isNotificationSystemAvailable: boolean; virtual;
  {** Identify the notification backend in use on this platform.
      Examples: 'notify-send', 'gdbus', 'WinRT-Toast', 'NSUserNotification', 'none', 'unknown'.
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
    {** Signal the start of a long-running update operation; platform units may
        show a progress indicator (taskbar, dock, etc.). Default no-op. }
  procedure updateBegin; virtual;
    {** Signal completion of a previously started update operation. Default no-op. }
  procedure updateDone; virtual;
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
  class function SetTitleColor(form: PtrUInt; bg, Text: TColor): boolean; virtual;
    {** Play an audio file using native facilities (safe file check included).
        Default base implementation is a no-op; platform units override to spawn
        the appropriate player (mplay32/aplay/afplay). }
  class procedure PlaySound(const FileName: string); virtual;
    {** Returns True when @code(FileName) is an audio file with a safe extension
        (.wav/.mp3/.ogg/.flac/.aac/.wma/.m4a), exists on disk, and contains no
        shell-dangerous characters. Used by platform @link(PlaySound) overrides. }
  class function IsValidAudioFile(const FileName: string): boolean;

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

type
TRequestExWaitThread = class(TThread)
private
  FOwner: TTrndiNativeBase;
  FPost: boolean;
  FEndpoint: string;
  FJsonData: string;
  FParams: TStringList;
  FCookieJar: TStringList;
  FFollowRedirects: boolean;
  FMaxRedirects: integer;
  FCustomHeaders: TStringList;
  FPrefix: boolean;
  FResponse: THTTPResponse;
  FDone: TEvent;
  FCookieJarOwned: TStringList;
  FCustomHeadersOwned: TStringList;
protected
  procedure Execute; override;
public
  constructor Create(AOwner: TTrndiNativeBase; const APost: boolean;
    const AEndpoint: string; const AParams: array of string; const AJsonData: string;
    ACookieJar: TStringList; AFollowRedirects: boolean; AMaxRedirects: integer;
    ACustomHeaders: TStringList; APrefix: boolean);
  destructor Destroy; override;
  property Response: THTTPResponse read FResponse;
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
  Settings cache helpers (class-level, process-wide)
 ------------------------------------------------------------------------------}
class procedure TTrndiNativeBase.EnsureSettingsCacheInit;
begin
  // Cache and lock are created in this unit's initialization section so they
  // are guaranteed available before any TrndiNative construction. This stub
  // is kept for callers that might invoke it explicitly; it must not race.
  if FSettingsCacheLock = nil then
    FSettingsCacheLock := SyncObjs.TCriticalSection.Create;
  if FSettingsCache = nil then
  begin
    FSettingsCache := TStringList.Create;
    FSettingsCache.CaseSensitive := true;
    FSettingsCache.NameValueSeparator := '=';
  end;
end;

class procedure TTrndiNativeBase.ShutdownSettingsCache;
begin
  // FSettingsCache and FSettingsCacheLock are class-lifetime singletons that
  // may be touched by background workers (e.g. TRequestExWorker -> requestEx
  // -> GetRootSetting for proxy.* lookups) which are not joined at shutdown.
  // Freeing them here would race with those workers, so leave the objects in
  // place for the process lifetime and just invalidate the warm snapshot.
  FSettingsCacheWarm := false;
end;

class function TTrndiNativeBase.TryGetCachedSetting(const fullKey: string;
out val: string): boolean;
var
  idx: integer;
begin
  Result := false;
  val := '';
  if FSettingsCacheLock = nil then
    Exit;
  FSettingsCacheLock.Enter;
  try
    idx := FSettingsCache.IndexOfName(fullKey);
    if idx >= 0 then
    begin
      val := FSettingsCache.ValueFromIndex[idx];
      Result := true;
    end
    else
    if FSettingsCacheWarm then
      // Authoritative miss — the snapshot did not contain this key.
      Result := true;
  finally
    FSettingsCacheLock.Leave;
  end;
end;

class procedure TTrndiNativeBase.SetCachedSetting(const fullKey, val: string);
begin
  if FSettingsCacheLock = nil then
    Exit;
  FSettingsCacheLock.Enter;
  try
    FSettingsCache.Values[fullKey] := val;
  finally
    FSettingsCacheLock.Leave;
  end;
end;

class procedure TTrndiNativeBase.RemoveCachedSetting(const fullKey: string);
var
  idx: integer;
begin
  if FSettingsCache = nil then
    Exit;
  FSettingsCacheLock.Enter;
  try
    idx := FSettingsCache.IndexOfName(fullKey);
    if idx >= 0 then
      FSettingsCache.Delete(idx);
  finally
    FSettingsCacheLock.Leave;
  end;
end;

class procedure TTrndiNativeBase.ClearSettingsCache;
begin
  if FSettingsCacheLock = nil then
    Exit;
  FSettingsCacheLock.Enter;
  try
    if FSettingsCache <> nil then
      FSettingsCache.Clear;
    FSettingsCacheWarm := false;
  finally
    FSettingsCacheLock.Leave;
  end;
end;

class procedure TTrndiNativeBase.SeedSettingsCache(Snapshot: TStrings);
var
  i: integer;
  k, v: string;
begin
  if FSettingsCacheLock = nil then
    Exit;
  FSettingsCacheLock.Enter;
  try
    FSettingsCache.Clear;
    if Snapshot <> nil then
      for i := 0 to Snapshot.Count - 1 do
      begin
        k := Snapshot.Names[i];
        if k = '' then
          Continue;
        v := Snapshot.ValueFromIndex[i];
        FSettingsCache.Values[k] := v;
      end;
    FSettingsCacheWarm := true;
  finally
    FSettingsCacheLock.Leave;
  end;
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
  TTrndiNativeBase.RequestExWait
  --------------------------------
  Run the platform-specific `requestEx` on a background thread and wait for
  the result with a timeout. Returns a THTTPResponse; on timeout the returned
  record has `Success = false` and `ErrorMessage = 'timeout'.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.RequestExWait(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
cookieJar: TStringList = nil; followRedirects: boolean = true;
maxRedirects: integer = 10; customHeaders: TStringList = nil;
prefix: boolean = true; TimeoutMs: cardinal = 5000): THTTPResponse;
var
  worker: TRequestExWaitThread;
  completed: boolean;
  i: integer;
  j: integer;
  cookieName: string;
  existingIndex: integer;
begin
  worker := TRequestExWaitThread.Create(Self, post, endpoint, params, jsondata,
    cookieJar, followRedirects, maxRedirects, customHeaders, prefix);
  worker.Start;
  // wait for the worker to signal completion or timeout
  if worker.FDone.WaitFor(TimeoutMs) = wrSignaled then
  begin
    // Copy response (deep copy lists) to avoid returning pointers owned
    // by the worker which will be freed when we destroy it.
    Result.Body := worker.Response.Body;
    Result.StatusCode := worker.Response.StatusCode;
    Result.FinalURL := worker.Response.FinalURL;
    Result.RedirectCount := worker.Response.RedirectCount;
    Result.Success := worker.Response.Success;
    Result.ErrorMessage := worker.Response.ErrorMessage;
    Result.Headers := TStringList.Create;
    if Assigned(worker.Response.Headers) then
      Result.Headers.Assign(worker.Response.Headers);
    Result.Cookies := TStringList.Create;
    if Assigned(worker.Response.Cookies) then
      Result.Cookies.Assign(worker.Response.Cookies);
    // Merge response cookies into caller's cookieJar, preserving existing cookies
    if Assigned(cookieJar) then
    begin
      for i := 0 to Result.Cookies.Count - 1 do
      begin
        // Try to update existing cookie by name (before '='), or add if not found
        cookieName := Copy(Result.Cookies[i], 1, Pos('=', Result.Cookies[i]) - 1);
        existingIndex := -1;
        for j := 0 to cookieJar.Count - 1 do
        begin
          if Copy(cookieJar[j], 1, Pos('=', cookieJar[j]) - 1) = cookieName then
          begin
            existingIndex := j;
            Break;
          end;
        end;
        if existingIndex >= 0 then
          cookieJar[existingIndex] := Result.Cookies[i]
        else
          cookieJar.Add(Result.Cookies[i]);
      end;
    end;
    worker.WaitFor;
    worker.Free;
  end
  else
  begin
    // timeout: return shaped timeout response and attempt cancellation
    Result.Body := '';
    Result.Headers := TStringList.Create;
    Result.Cookies := TStringList.Create;
    Result.StatusCode := -1;
    Result.FinalURL := '';
    Result.RedirectCount := 0;
    Result.Success := false;
    Result.ErrorMessage := 'timeout';
    try
      worker.Terminate;
      completed := worker.FDone.WaitFor(5000) = wrSignaled;
      if completed then
      begin
        worker.WaitFor;
        worker.Free;
      end;
      if not completed then
      begin
        // Prevent worker from dereferencing Self after timeout
        worker.FOwner := nil;
        worker.FreeOnTerminate := true;
      end;
    except end;
  end;
end;

{------------------------------------------------------------------------------
  TRequestExWaitThread
  --------------------
  Worker thread that executes requestEx off the main thread and stores the
  resulting THTTPResponse for the caller to collect after WaitFor returns.
 ------------------------------------------------------------------------------}
constructor TRequestExWaitThread.Create(AOwner: TTrndiNativeBase;
const APost: boolean; const AEndpoint: string; const AParams: array of string;
const AJsonData: string; ACookieJar: TStringList; AFollowRedirects: boolean;
AMaxRedirects: integer; ACustomHeaders: TStringList; APrefix: boolean);
var
  i: integer;
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FPost := APost;
  FEndpoint := AEndpoint;
  FJsonData := AJsonData;
  FParams := TStringList.Create;
  for i := Low(AParams) to High(AParams) do
    FParams.Add(AParams[i]);
  // make owned copies of incoming lists so the worker does not reference
  // caller-owned objects which may be freed while the worker runs
  if Assigned(ACookieJar) then
  begin
    FCookieJarOwned := TStringList.Create;
    FCookieJarOwned.Assign(ACookieJar);
    FCookieJar := FCookieJarOwned;
  end
  else
    FCookieJar := nil;
  FFollowRedirects := AFollowRedirects;
  FMaxRedirects := AMaxRedirects;
  if Assigned(ACustomHeaders) then
  begin
    FCustomHeadersOwned := TStringList.Create;
    FCustomHeadersOwned.Assign(ACustomHeaders);
    FCustomHeaders := FCustomHeadersOwned;
  end
  else
    FCustomHeaders := nil;
  FPrefix := APrefix;
  FResponse.Body := '';
  FResponse.Headers := nil;
  FResponse.Cookies := nil;
  FResponse.StatusCode := -1;
  FResponse.FinalURL := '';
  FResponse.RedirectCount := 0;
  FResponse.Success := false;
  FResponse.ErrorMessage := 'timeout';
  FDone := TEvent.Create(nil, true, false, '');
end;

destructor TRequestExWaitThread.Destroy;
begin
  FParams.Free;
  FCustomHeadersOwned.Free;
  FCookieJarOwned.Free;
  FDone.Free;
  // Free response lists that were created in Execute or timeout path
  FreeAndNil(FResponse.Headers);
  FreeAndNil(FResponse.Cookies);
  inherited Destroy;
end;

procedure TRequestExWaitThread.Execute;
var
  reqParams: array of string;
  i: integer;
begin
  SetLength(reqParams, FParams.Count);
  for i := 0 to FParams.Count - 1 do
    reqParams[i] := FParams[i];
  try
    try
      // Guard against FOwner being cleared during timeout
      if Assigned(FOwner) and not Terminated then
        FResponse := FOwner.requestEx(FPost, FEndpoint, reqParams, FJsonData,
          FCookieJar, FFollowRedirects, FMaxRedirects, FCustomHeaders, FPrefix)
      else
      begin
        // Timeout or early termination cleared FOwner; return failure
        FResponse.Success := false;
        FResponse.StatusCode := -1;
        FResponse.ErrorMessage := 'Cancelled';
      end;
    except
      on E: Exception do
      begin
        FResponse.Success := false;
        FResponse.StatusCode := -1;
        if Trim(E.Message) <> '' then
          FResponse.ErrorMessage := E.ClassName + ': ' + E.Message
        else
          FResponse.ErrorMessage := E.ClassName;
      end;
    end;
  finally
    FDone.SetEvent;
  end;
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

procedure TTrndiNativeBase.start;
begin
  // No-op default; platforms override.
end;

procedure TTrndiNativeBase.done;
begin
  // No-op default; platforms override.
end;

procedure TTrndiNativeBase.updateBegin;
begin
  // No-op default; platforms override.
end;

procedure TTrndiNativeBase.updateDone;
begin
  // No-op default; platforms override.
end;

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
  IsValidAudioFile
  ----------------
  Cross-platform validation: file exists, has an accepted audio extension, and
  contains no shell-dangerous characters. Shared helper for platform PlaySound.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.IsValidAudioFile(const FileName: string): boolean;
const
  ValidExtensions: array[0..6] of
    string = ('.wav', '.mp3', '.ogg', '.flac', '.aac', '.wma', '.m4a');
var
  Ext: string;
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
    Result := false;
end;

{------------------------------------------------------------------------------
  PlaySound (base)
  ----------------
  Default no-op. Platform units override to spawn the correct player.
 ------------------------------------------------------------------------------}
class procedure TTrndiNativeBase.PlaySound(const FileName: string);
begin
  // No-op default; platforms override.
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
  DetectTouchScreen (base)
  ------------------------
  Default: report no touchscreen. Platform units override (Win: SM_DIGITIZER,
  Linux: /proc/bus/input/devices).
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.DetectTouchScreen(out multi: boolean): boolean;
begin
  multi := false;
  Result := false;
end;

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

{------------------------------------------------------------------------------
  GetOSLanguage (base)
  --------------------
  Default implementation: derive the language code from the LANG environment
  variable (covers Linux/BSD/Haiku and any platform without a richer locale API).
  Windows and macOS override with native locale APIs.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.GetOSLanguage: string;
var
  v: string;
  p: SizeInt;
begin
  v := Trim(SysUtils.GetEnvironmentVariable('LANG'));
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


{------------------------------------------------------------------------------
  attention
  -------------------
  Surface a visual/sound notification depending on the platform.
  Platform units may override; base provides simple cross-platform attempts.
 ------------------------------------------------------------------------------}

procedure TTrndiNativeBase.attention(topic, message: string);
begin
  // Default no-op. Platform units override (Windows: WinRT toast,
  // Linux: gdbus/notify-send, macOS: UNUserNotificationCenter/osascript,
  // Haiku: BNotification).
end;

{------------------------------------------------------------------------------
  request (base)
  --------------
  Default: report that no platform implementation is wired in. Platform units
  override (Win: WinHTTPClient, Linux: libcurl, mac: TNSHTTPSendAndReceive,
  Haiku: TFPHTTPClient).
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''; prefix: boolean = true): string;
begin
  Result := 'request: no native HTTP implementation for this platform';
end;

{------------------------------------------------------------------------------
  requestEx (base)
  --------------
  Default: report that no platform implementation is wired in. Platform units
  override (Win: WinHTTP, Linux: libcurl, mac: TNSHTTPSendAndReceive, Haiku:
  TFPHTTPClient).
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
cookieJar: TStringList = nil; followRedirects: boolean = true;
maxRedirects: integer = 10; customHeaders: TStringList = nil;
prefix: boolean = true): THTTPResponse;
begin
  Result.Body := '';
  Result.Headers := TStringList.Create;
  Result.Cookies := TStringList.Create;
  Result.Success := false;
  Result.StatusCode := 0;
  Result.RedirectCount := 0;
  Result.FinalURL := '';
  Result.ErrorMessage := 'requestEx: no native HTTP implementation for this platform';
end;

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
  GetAlignmentSetting
  -------------------------
  Returns an alignment from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetAlignmentSetting(const keyname: string; def: TAlignment = taCenter): TAlignment;
var
  r: string;
begin
  r := GetSetting(keyname);

  case r of
  'left':
    result := taLeftJustify;
  'right':
    result := taRightJustify;
  'center':
    result := taCenter;
  else
    result := def;
  end;

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
  sl: TStringList;
begin
  Result := false;
  if self.TryGetSetting(keyname, val, global) then
  begin
    sl := TStringList.Create;
    try
      sl.Clear;
      sl.CommaText := val;
      res := sl.ToStringArray;
      Result := true;
    finally
      sl.Free;
    end;
  end
  else
    Result := false;
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
  SetAlignmentSetting
  -------------------------
  Stores an alignment to platofm-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetAlignmentSetting(const keyname: string; val: TAlignment; const global: boolean = false);
begin
  case val of
  taLeftJustify:
    SetSetting(keyname, 'left', global);
  taRightJustify:
    SetSetting(keyname, 'right', global);
  taCenter:
    SetSetting(keyname, 'center', global);
  end;
end;

{------------------------------------------------------------------------------
  SetCSVSetting
  ----------------------
  Stores a bool value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetCSVSetting(const keyname: string; const val: TStringArray; const global: boolean = false);
var
  s, res: string;
  i, totalLen: integer;
  p: pansichar;
begin
  if Length(val) = 0 then
  begin
    SetSetting(keyname, '', global);
    exit;
  end;

  totalLen := 0;
  for s in val do
    totalLen := totalLen + Length(s) + 1; // +1 for comma
  Dec(totalLen); // remove last comma

  SetLength(res, totalLen);
  p := pansichar(res);
  for i := 0 to High(val) do
  begin
    if i > 0 then
    begin
      p^ := ',';
      Inc(p);
    end;
    if Length(val[i]) > 0 then
    begin
      Move(pansichar(val[i])^, p^, Length(val[i]));
      Inc(p, Length(val[i]));
    end;
  end;

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
  Result := false;
end;

{------------------------------------------------------------------------------
  HasDangerousChars (base)
  ------------------------
  Default chars-the-shell-hates check. The base set includes a backslash since
  most shells treat it as an escape; Windows overrides to drop the backslash
  because it is the path separator there.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.HasDangerousChars(const FileName: string): boolean;
const
  DangerousChars: TSysCharSet =
    ['&', '|', ';', '`', '$', '(', ')', '<', '>', '"', '''', '\'];
var
  i: integer;
begin
  Result := false;
  for i := 1 to Length(FileName) do
    if FileName[i] in DangerousChars then
    begin
      Result := true;
      Exit;
    end;
end;

class function TTrndiNativeBase.SetTitleColor(form: PtrUInt; bg, Text: TColor): boolean;
begin
  Result := false;
end;

initialization
  TTrndiNativeBase.EnsureSettingsCacheInit;

finalization
  TTrndiNativeBase.ShutdownSettingsCache;

end.
