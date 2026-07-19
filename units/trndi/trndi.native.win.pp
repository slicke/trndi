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
 *
 * GitHub: https://github.com/slicke/trndi
 *)
unit trndi.native.win;

{**
  @abstract(Windows-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeWindows) which derives from
  @link(TTrndiNativeBase) and implements behaviors that require Windows APIs
  (SAPI for TTS, DWM for caption colors and immersive dark mode).

  Consumers should use the façade unit @code(trndi.native), which exposes the
  alias @code(TrndiNative) to the correct platform class at compile time.

  @bold(Key responsibilities)
  - Text-to-speech using SAPI (@link(TTrndiNativeWindows.Speak))
  - Toggle immersive dark mode on a window (@link(TTrndiNativeWindows.SetDarkMode))
  - Set window caption and text colors (@link(TTrndiNativeWindows.SetTitleColor))
  - Simple HTTP GET via WinHTTP (@link(TTrndiNativeWindows.getURL))
  - Persist settings in Windows Registry (@link(TTrndiNativeWindows.GetSetting))

  @seealso(TTrndiNativeBase)
}

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Graphics, Windows, Registry, Dialogs, StrUtils,
winutils.httpclient, winutils.wintaskbar, shellapi,
Forms, variants, dwmapi, trndi.native.base, ExtCtrls, IniFiles, trndi.log;

type
  {**
    @abstract(Windows implementation of @link(TTrndiNativeBase).)
    Uses SAPI for speech and DWM for window appearance tweaks.
  }
TTrndiNativeWindows = class(TTrndiNativeBase)
private
  FFlashTimer: TTimer;
  FFlashEnd: TDateTime;
  FFlashPhase: integer;
  FFlashValue: string;
  FFlashBaseColor: TColor;
  FFlashCycleMS: integer;
  procedure FlashTimerTick(Sender: TObject);
public
  destructor Destroy; override;
    {** Speaks @param(Text) using SAPI; falls back to default voice if a
        locale-matching voice is not found. }
  procedure Speak(const Text: string); override;
    {** Toggles immersive dark mode for @param(win).
        Requires Windows 10 1809+ (build >= 17763).
        @returns(True if the DWM call succeeds) }
  class function SetDarkMode(win: HWND; Enable: boolean = true): boolean;
    {** Opt the entire process into Windows' dark popup-menu / scrollbar / tooltip
        theme via the undocumented uxtheme.dll ordinal 135 (SetPreferredAppMode).
        Requires Windows 10 1809+ (build >= 17763); silently no-ops elsewhere.
        Call once after process start, e.g. when @link(isDarkMode) returns True.
        @returns(True if the call succeeded) }
  class function SetPreferredDarkMode: boolean;
    {** Applies caption (@param(bg)) and text (@param(text)) colors via DWM.
        @returns(True if both attributes are set successfully) }
  class function SetTitleColor(form: PtrUInt; bg, Text: TColor): boolean; override;
    {** Draw a badge with @param(Value) on the application icon.
        @param(BadgeColor Color of the badge circle/rounded rect)
        @param(badge_size_ratio Badge diameter relative to icon size)
        @param(min_font_size Minimum font size while fitting text) }
  procedure SetBadge(const Value: string; BadgeColor: TColor;
    badge_size_ratio: double; min_font_size: integer); override;
  procedure StartBadgeFlash(const Value: string; badgeColor: TColor;
    DurationMS: integer = 10000; CycleMS: integer = 400); override;
  procedure StopBadgeFlash; override;
  {** Simple HTTP GET using WinHTTP client with default UA.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
  class function getURL(const url: string; out res: string): boolean; override;
  {** Simple HTTP POST using WinHTTP client. }
  class function postURL(const url: string; const body: string;
    const contentType: string; out res: string): boolean; override;
  {** Test an HTTP GET through an explicit proxy only (no direct fallback). }
  class function TestProxyURL(const url: string; const proxyHost: string;
    const proxyPort: string; const proxyUser: string; const proxyPass: string;
    out res: string): boolean; override;
  {** Determine if Windows is using dark app theme (AppsUseLightTheme=0).
      @returns(True if dark mode is active) }
  class function isDarkMode: boolean; override;
  {** Returns True if PowerShell is available on the system. PowerShell is the
    host used to invoke the WinRT @code(ToastNotificationManager) API directly,
    so its presence is a sufficient proxy for native toast availability. }
  class function isNotificationSystemAvailable: boolean; override;
  {** Identify the notification backend for Windows.
      Returns @code('WinRT-Toast') when PowerShell is available (the toast is
      shown via @code(Windows.UI.Notifications) using PowerShell's AUMID);
      otherwise @code('none'). }
  class function getNotificationSystem: string; override;
    {** Check whether platform TTS is available. }
  class function SpeakAvailable: boolean; override;
    {** Name of the software used for speech on Windows (e.g., 'SAPI'). }
  class function SpeakSoftwareName: string; override;
    {** Best-effort window manager name for Windows. }
  class function GetWindowManagerName: string; override;

  {** Always True on Windows; start-on-login is implemented via the per-user
      Run registry key. }
  class function AutoStartAvailable: boolean; override;
  {** True when a "Trndi" value exists under
      @code(HKCU\Software\Microsoft\Windows\CurrentVersion\Run). }
  class function GetAutoStart: boolean; override;
  {** Add or remove the "Trndi" value under
      @code(HKCU\Software\Microsoft\Windows\CurrentVersion\Run). The value is
      the quoted full path of the current executable. }
  class function SetAutoStart(Enable: boolean): boolean; override;

  {** Settings API overrides (Windows Registry)
    Keys are stored under HKCU\Software\Trndi\ with the same scoping rules
    used by the base implementation. }
  {** Retrieve a setting from HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(def Default value if not present)
    @param(global If True, use global scope; otherwise per-user)
    @returns(Value if present, otherwise def) }
  function GetSetting(const keyname: string; def: string = '';
    global: boolean = false): string; override;
  {** Persist a setting to HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(val Value to write)
    @param(global If True, use global scope; otherwise per-user) }
  procedure SetSetting(const keyname: string; const val: string;
    global: boolean = false); override;
  {** Delete a setting from HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(global If True, use global scope; otherwise per-user) }
  procedure DeleteSetting(const keyname: string; global: boolean = false); override;
  {** Refresh settings cache, if any. Registry access is on-demand here,
    so nothing needs to be reloaded. }
  procedure ReloadSettings; override;
  {** Export all settings to INI format string. }
  function ExportSettings: string; override;
  {** Import settings from INI format string. }
  procedure ImportSettings(const iniData: string); override;
  {** Signal the start of a long-running update operation (show taskbar progress). }
  procedure updateBegin; override;
  {** Signal the completion of a long-running update operation (clear taskbar progress). }
  procedure updateDone; override;
  {** Simple HTTP GET/POST using WinHTTP, with proxy-first / direct fallback
      driven by the proxy.* root settings. }
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''; prefix: boolean = true): string; override;
  {** Enhanced HTTP request via WinHTTP: tracks cookies, follows redirects,
      captures response headers. }
  function requestEx(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    cookieJar: TStringList = nil; followRedirects: boolean = true;
    maxRedirects: integer = 10; customHeaders: TStringList = nil;
    prefix: boolean = true): THTTPResponse; override;
  {** Show a Windows toast via PowerShell + WinRT Windows.UI.Notifications.
      No third-party module (e.g. BurntToast) required; uses PowerShell's
      built-in AUMID so we don't have to register one ourselves. }
  procedure attention(topic, message: string); override;
  {** Detect a touchscreen via SM_DIGITIZER (NID_INTEGRATED_TOUCH + NID_READY).
      Sets @code(multi) when NID_MULTI_INPUT is reported. }
  class function DetectTouchScreen(out multi: boolean): boolean; override;
  {** Play an audio file asynchronously via MCI (winmm); .wav/.mp3/.wma
      decode natively on all Windows versions. }
  class procedure PlaySound(const FileName: string); override;
  {** Resolve the user's UI language via Windows locale APIs. }
  class function GetOSLanguage: string; override;
  {** Windows-flavoured shell-dangerous chars check (excludes backslash, since
      it is the path separator). }
  class function HasDangerousChars(const FileName: string): boolean; override;
  {** Subclass the main form's WndProc to catch @code(WM_POWERBROADCAST)
      / @code(PBT_APMRESUMESUSPEND) and invoke the wake callback on the
      main thread. Also calls @code(RegisterSuspendResumeNotification) on
      Windows 8+ so the message is reliably delivered even when no other
      app listens. Safe to call multiple times — replaces the prior
      callback. }
  procedure RegisterWakeCallback(const Callback: TTrndiWakeCallback); override;
  {** Unhook the WndProc subclass and release the suspend/resume
      notification registration. }
  procedure UnregisterWakeCallback; override;
end;

implementation

uses
ComObj, ActiveX, SyncObjs, base64;

type
  {** Background worker that owns a SAPI.SpVoice in an STA thread and
      processes a simple queue of text to speak. }
  TSpeechWorker = class(TThread)
  private
    FQueue: TStringList;
    FCS: TCriticalSection;
    FEvent: TEvent;
    FVoice: olevariant;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Text: string);
    procedure Execute; override;
  end;

var
  gSpeechWorker: TSpeechWorker = nil;
  // Pristine copy of Application.Icon captured before SetBadge ever mutates it.
  // SetBadge sources from here instead of Application.Icon so each call composites
  // onto the original logo rather than the previous badged result.
  gOriginalAppIcon: TIcon = nil;
  // HICON most recently handed to the shell via WM_SETICON. Explorer caches the
  // taskbar button icon by handle value and treats a WM_SETICON carrying an
  // unchanged value as "no change" — so the previous handle must stay alive
  // until the new one has been created and sent, or USER32 recycles the value
  // and badge updates never reach the taskbar.
  gLastBadgeIcon: HICON = 0;

procedure EnsureSpeechWorker;
begin
  if gSpeechWorker = nil then
    gSpeechWorker := TSpeechWorker.Create;
end;

procedure StopSpeechWorker;
begin
  if Assigned(gSpeechWorker) then
  begin
    gSpeechWorker.Terminate;
    gSpeechWorker.FEvent.SetEvent;
    gSpeechWorker.WaitFor;
    FreeAndNil(gSpeechWorker);
  end;
end;

procedure EnqueueSpeech(const Text: string);
begin
  EnsureSpeechWorker;
  if Assigned(gSpeechWorker) then
    gSpeechWorker.Enqueue(Text);
end;

{ TSpeechWorker }

constructor TSpeechWorker.Create;
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FQueue := TStringList.Create;
  FCS := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  
end;

destructor TSpeechWorker.Destroy;
begin
  FEvent.Free;
  FCS.Free;
  FQueue.Free;
  inherited Destroy;
end;

procedure TSpeechWorker.Enqueue(const Text: string);
begin
  FCS.Enter;
  try
    FQueue.Add(Text);
  finally
    FCS.Leave;
  end;
  FEvent.SetEvent;
end;

procedure TSpeechWorker.Execute;
var
  hr: HRESULT;
  textToSpeak: string;
  hasItem: Boolean;
begin
  // Initialize COM for this STA thread
  hr := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    try
      FVoice := CreateOleObject('SAPI.SpVoice');
    except
      on E: Exception do
        begin
        Exit;
      end;
    end;

    while not Terminated do
    begin
      // Wait for work or timeout to check termination
      FEvent.WaitFor(500);
      hasItem := False;
      textToSpeak := '';
      FCS.Enter;
      try
        if FQueue.Count > 0 then
        begin
          textToSpeak := FQueue[0];
          FQueue.Delete(0);
          hasItem := True;
        end;
      finally
        FCS.Leave;
      end;

      if hasItem then
      begin
        try
          // Speak synchronously on worker thread so multiple utterances are serialized
          FVoice.Speak(textToSpeak, 0);
        except
          on E: Exception do
            ;
        end;
      end;
    end;

  finally
    // Clean up COM/voice
    try
      FVoice := Unassigned;
    except
    end;
    CoUninitialize;
  end;
end;
{------------------------------------------------------------------------------
  IsPowerShellAvailable
  ---------------------
  Check whether Windows PowerShell (powershell.exe) exists at its canonical
  location under the system directory. PowerShell ships with Windows 7+ and
  is required to invoke the WinRT toast API from this process.
 ------------------------------------------------------------------------------}
{** Check whether powershell.exe is present at the standard system location.
    Implementation detail for @link(TTrndiNativeWindows.isNotificationSystemAvailable). }
function IsPowerShellAvailable: boolean;
var
  Buf: array[0..MAX_PATH-1] of WChar;
  Path: unicodestring;
  Len: UINT;
begin
  Result := false;
  Len := GetSystemDirectoryW(@Buf[0], Length(Buf));
  if Len = 0 then
    Exit;
  SetString(Path, pwidechar(@Buf[0]), Len);
  Path := Path + '\WindowsPowerShell\v1.0\powershell.exe';
  Result := GetFileAttributesW(pwidechar(Path)) <> INVALID_FILE_ATTRIBUTES;
end;

function EnumLogWnd_UpdateBegin(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  pid: DWORD;
  titlebuf: array[0..255] of WideChar;
  cnamebuf: array[0..255] of WideChar;
  visible: Boolean;
  owner: HWND;
  cap: string;
  wndClassName: string;
  es: NativeUInt;
begin
  GetWindowThreadProcessId(hwnd, @pid);
  if pid <> GetCurrentProcessId then
  begin
    Result := True; // continue enumeration
    Exit;
  end;
  visible := IsWindowVisible(hwnd);
  owner := GetWindow(hwnd, GW_OWNER);
  if GetWindowTextW(hwnd, titlebuf, Length(titlebuf)) > 0 then
    cap := Trim(string(titlebuf))
  else
    cap := '';
  if GetClassNameW(hwnd, cnamebuf, Length(cnamebuf)) > 0 then
    wndClassName := Trim(string(cnamebuf))
  else
    wndClassName := '';
  es := GetWindowLongPtr(hwnd, GWL_EXSTYLE);
  {$ifdef DEBUG}
  TrndiDLog(Format('  HWND=%d Title="%s" Class="%s" Visible=%s Owner=%d ExStyle=0x%8.8x ToolWindow=%s',
    [hwnd, cap, wndClassName, BoolToStr(visible, True), owner, UIntPtr(es), BoolToStr((es and WS_EX_TOOLWINDOW) <> 0, True)]));
  {$endif}
  Result := True;
end;

{------------------------------------------------------------------------------
  SpeakAvailable (Windows)
  ------------------------
  Check if SAPI is available by attempting to create the SpVoice object.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SpeakAvailable: boolean;
var
  Voice: olevariant;
begin
  Result := false;
  try
    Voice := CreateOleObject('SAPI.SpVoice');
    Result := true;
  except
    // SAPI not available
    Result := false;
  end;
end;

{------------------------------------------------------------------------------
  SpeakSoftwareName (Windows)
  ---------------------------
  Name of the speech backend used on Windows.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SpeakSoftwareName: string;
begin
  Result := 'SAPI';
end;

{------------------------------------------------------------------------------
  GetWindowManagerName (Windows)
  ------------------------------
  Return a stable identifier for Windows. No separate window manager process
  is exposed like on X11, so we return a human-friendly value.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.GetWindowManagerName: string;
begin
  Result := 'Windows Desktop';
end;

{------------------------------------------------------------------------------
  AutoStart (Windows)
  -------------------
  Backed by HKCU\Software\Microsoft\Windows\CurrentVersion\Run. The per-user
  Run key is owned by the user (no admin needed) and is honored by Explorer
  at every interactive logon. The value is the quoted full exe path; quoting
  is mandatory because Run values are parsed as command lines.
 ------------------------------------------------------------------------------}
const
  AUTOSTART_RUN_KEY  = 'Software\Microsoft\Windows\CurrentVersion\Run';
  AUTOSTART_VALUE    = 'Trndi';

class function TTrndiNativeWindows.AutoStartAvailable: boolean;
begin
  Result := true;
end;

class function TTrndiNativeWindows.GetAutoStart: boolean;
var
  reg: TRegistry;
begin
  Result := false;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(AUTOSTART_RUN_KEY) then
    try
      Result := reg.ValueExists(AUTOSTART_VALUE);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

class function TTrndiNativeWindows.SetAutoStart(Enable: boolean): boolean;
var
  reg: TRegistry;
  exe: string;
begin
  Result := false;
  reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(AUTOSTART_RUN_KEY, true) then
      Exit;
    try
      if Enable then
      begin
        exe := ParamStr(0);
        if exe = '' then
          exe := Application.ExeName;
        reg.WriteString(AUTOSTART_VALUE, '"' + exe + '"');
        Result := true;
      end
      else
      begin
        if reg.ValueExists(AUTOSTART_VALUE) then
          reg.DeleteValue(AUTOSTART_VALUE);
        Result := true;
      end;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TTrndiNativeWindows.FlashTimerTick(Sender: TObject);
var
  phaseColor: TColor;
  factor: double;
begin
  if (Now > FFlashEnd) or (FFlashValue = '') then
  begin
    StopBadgeFlash;
    Exit;
  end;

  // Simple 4-phase pulse: normal -> lighter -> normal -> darker
  case FFlashPhase mod 4 of
  0:
    factor := 1.0;   // base
  1:
    factor := 1.35;  // brighten
  2:
    factor := 1.0;   // base
  3:
    factor := 0.70;  // darken
  else
    factor := 1.0;
  end;

  // Adjust color
  phaseColor := RGB(Min(255, Round(GetRValue(ColorToRGB(FFlashBaseColor)) * factor)),
    Min(255, Round(GetGValue(ColorToRGB(FFlashBaseColor)) * factor)),
    Min(255, Round(GetBValue(ColorToRGB(FFlashBaseColor)) * factor)));

  // Draw badge with pulsed color
  SetBadge(FFlashValue, phaseColor, DEFAULT_BADGE_SIZE_RATIO, DEFAULT_MIN_FONT_SIZE);

  Inc(FFlashPhase);
end;

procedure TTrndiNativeWindows.StartBadgeFlash(const Value: string;
badgeColor: TColor; DurationMS: integer; CycleMS: integer);
begin
  // Initialize or update flashing parameters
  FFlashValue := Value;
  FFlashBaseColor := badgeColor;
  FFlashCycleMS := CycleMS;
  FFlashEnd := Now + (DurationMS / (24 * 60 * 60 * 1000)); // ms to TDateTime
  FFlashPhase := 0;

  if FFlashTimer = nil then
  begin
    FFlashTimer := TTimer.Create(nil);
    FFlashTimer.OnTimer := @FlashTimerTick;
  end;
  FFlashTimer.Interval := CycleMS;
  FFlashTimer.Enabled := true;

  // Immediate first frame
  FlashTimerTick(nil);
end;

destructor TTrndiNativeWindows.Destroy;
begin
  if Assigned(FFlashTimer) then
  begin
    FFlashTimer.Enabled := false;
    FreeAndNil(FFlashTimer);
  end;
  inherited Destroy;
end;

procedure TTrndiNativeWindows.StopBadgeFlash;
begin
  // Only disable the timer here — StopBadgeFlash is called from inside
  // FlashTimerTick, and freeing a TTimer from within its own OnTimer
  // handler risks a use-after-free when control returns to the LCL timer
  // dispatch. The instance is reused by StartBadgeFlash and released in
  // the destructor.
  if Assigned(FFlashTimer) then
    FFlashTimer.Enabled := false;
  // Restore static badge with base color if we still have a value
  if FFlashValue <> '' then
    SetBadge(FFlashValue, FFlashBaseColor, DEFAULT_BADGE_SIZE_RATIO,
      DEFAULT_MIN_FONT_SIZE);
  FFlashValue := '';
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable
  -----------------------------
  Returns True if native toast notifications are likely available. We invoke
  the WinRT ToastNotificationManager via PowerShell, so PowerShell's presence
  is the proxy.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.isNotificationSystemAvailable: boolean;
begin
  Result := IsPowerShellAvailable;
end;

{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Return 'WinRT-Toast' when PowerShell is present (the toast XML is dispatched
  through Windows.UI.Notifications using PowerShell's registered AUMID);
  otherwise 'none'.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.getNotificationSystem: string;
begin
  if IsPowerShellAvailable then
    Result := 'WinRT-Toast'
  else
    Result := 'none';
end;


{------------------------------------------------------------------------------
  isDarkMode
  ----------
  Detect Windows App theme: AppsUseLightTheme = 0 means dark mode for apps.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.isDarkMode: boolean;
const
  regtheme = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  reglight = 'AppsUseLightTheme';
var
  reg: TRegistry;
begin
  Result := false;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(regtheme) and reg.OpenKey(regtheme, false) then
    try
      // AppsUseLightTheme = 0 means dark mode for apps is enabled
      if reg.ValueExists(reglight) then
        Result := (reg.ReadInteger(reglight) = 0);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

{$ifdef Windows}
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
{$endif}

{ DWM attribute constants used for caption and text colors, and dark mode }
const
DWMWA_CAPTION_COLOR = 35;
DWMWA_TEXT_COLOR = 36;
DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

{------------------------------------------------------------------------------
  Speak
  -----
  Use SAPI to speak text asynchronously; pick a voice matching user locale when possible.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.Speak(const Text: string);
var
  Voice: olevariant;
begin
  // Enqueue speech to the background worker; if that fails, fall back to
  // a synchronous speak so the user still hears audio.
  try
    EnqueueSpeech(Text);
    Exit;
  except
    // Fall through to synchronous fallback
  end;

  try
    try
      Voice := CreateOleObject('SAPI.SpVoice');
      Voice.Speak(Text, 0);
    except
      // Ignore fallback failures; avoid crashing the caller
    end;
  finally
    Voice := Unassigned;
  end;
end;

{------------------------------------------------------------------------------
  getURL
  ------
  Simple HTTP GET using WinHTTP client with a default User-Agent.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  client: TWinHTTPClient;
  responseStr: string;
  proxyHost, proxyPort, proxyUser, proxyPass: string;
  tempInstance: TTrndiNativeWindows;

  function SafeUrlForLog(const s: string): string;
  var
    cut: integer;
  begin
    Result := s;
    cut := Pos('#', Result);
    if cut > 0 then
      Result := Copy(Result, 1, cut - 1);
    cut := Pos('?', Result);
    if cut > 0 then
      Result := Copy(Result, 1, cut - 1);
    if Length(Result) > 180 then
      Result := Copy(Result, 1, 180) + '...';
  end;

  procedure NormalizeProxyHostPort(var host: string; var port: string);
  var
    s: string;
    p: integer;
    hostPart, portPart: string;
  begin
    s := Trim(host);

    // Strip scheme if provided (e.g. http://proxy:3128)
    p := Pos('://', s);
    if p > 0 then
      s := Copy(s, p + 3, MaxInt);

    // Strip any path
    p := Pos('/', s);
    if p > 0 then
      s := Copy(s, 1, p - 1);

    // If host contains an explicit port, split it out
    p := LastDelimiter(':', s);
    if (p > 0) and (p < Length(s)) then
    begin
      hostPart := Copy(s, 1, p - 1);
      portPart := Copy(s, p + 1, MaxInt);
      if (hostPart <> '') and (StrToIntDef(portPart, -1) > 0) then
      begin
        s := hostPart;
        if port = '' then
          port := portPart;
      end;
    end;

    host := s;
    port := Trim(port);
  end;

  function PerformRequest(withProxy: boolean; forceNoProxy: boolean): boolean;
  begin
    Result := false;
    if withProxy and (proxyHost <> '') then
    begin
      if (proxyUser <> '') or (proxyPass <> '') then
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080), proxyUser, proxyPass)
      else
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080));
    end
    else if forceNoProxy then
    begin
      client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, true);
    end
    else
    begin
      client := TWinHTTPClient.Create(DEFAULT_USER_AGENT);
    end;

    try
      responseStr := client.Get(url, []);
      res := responseStr;
      Result := true;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
      end;
    end;
    client.Free;
  end;

begin
  res := '';

  tempInstance := TTrndiNativeWindows.Create;
  try
    // Check for custom proxy settings
    proxyHost := tempInstance.GetSetting('proxy.host', '', true);
    if proxyHost <> '' then
    begin
      proxyPort := tempInstance.GetSetting('proxy.port', '', true);
      proxyUser := tempInstance.GetSetting('proxy.user', '', true);
      proxyPass := tempInstance.GetSetting('proxy.pass', '', true);
      NormalizeProxyHostPort(proxyHost, proxyPort);
      if proxyPort = '' then
        proxyPort := '8080';
    end;

    {$ifdef DEBUG}
    if proxyHost <> '' then
      TrndiDLog(Format('HTTP GET: proxy configured (%s:%s); url=%s', [proxyHost, proxyPort, SafeUrlForLog(url)]))
    else
      TrndiDLog(Format('HTTP GET: no proxy configured; url=%s', [SafeUrlForLog(url)]));
    {$endif}

    // Try with proxy first if configured
    if proxyHost <> '' then
    begin
      {$ifdef DEBUG}
      TrndiDLog(Format('HTTP GET: attempting via proxy %s:%s', [proxyHost, proxyPort]));
      {$endif}
      if PerformRequest(true, false) then
      begin
        {$ifdef DEBUG}
        TrndiNetLog('HTTP GET: proxy attempt succeeded');
        {$endif}
        Result := true;
        Exit;
      end;

      {$ifdef DEBUG}
      TrndiNetLog('HTTP GET: proxy attempt failed: ' + res + ' ; retrying direct');
      {$endif}
    end;

    // Fallback: try without proxy
    {$ifdef DEBUG}
    if proxyHost <> '' then
      TrndiNetLog('HTTP GET: attempting direct (forcing no-proxy on WinHTTP)')
    else
      TrndiNetLog('HTTP GET: attempting direct');
    {$endif}
    if PerformRequest(false, proxyHost <> '') then
    begin
      {$ifdef DEBUG}
      TrndiNetLog('HTTP GET: direct attempt succeeded');
      {$endif}
      Result := true;
    end
    else
    begin
      {$ifdef DEBUG}
      TrndiNetLog('HTTP GET: direct attempt failed: ' + res);
      {$endif}
      Result := false;
    end;

  finally
    tempInstance.Free;
  end;
end;

{------------------------------------------------------------------------------
  postURL
  -------
  Simple HTTP POST using WinHTTP client. Mirrors getURL: respects the
  configured proxy with a direct fallback on failure.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.postURL(const url: string; const body: string;
  const contentType: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  client: TWinHTTPClient;
  responseStr: string;
  proxyHost, proxyPort, proxyUser, proxyPass: string;
  tempInstance: TTrndiNativeWindows;

  procedure NormalizeProxyHostPort(var host: string; var port: string);
  var
    s: string;
    p: integer;
    hostPart, portPart: string;
  begin
    s := Trim(host);
    p := Pos('://', s);
    if p > 0 then
      s := Copy(s, p + 3, MaxInt);
    p := Pos('/', s);
    if p > 0 then
      s := Copy(s, 1, p - 1);
    p := LastDelimiter(':', s);
    if (p > 0) and (p < Length(s)) then
    begin
      hostPart := Copy(s, 1, p - 1);
      portPart := Copy(s, p + 1, MaxInt);
      if (hostPart <> '') and (StrToIntDef(portPart, -1) > 0) then
      begin
        s := hostPart;
        if port = '' then
          port := portPart;
      end;
    end;
    host := s;
    port := Trim(port);
  end;

  function PerformRequest(withProxy: boolean; forceNoProxy: boolean): boolean;
  begin
    Result := false;
    if withProxy and (proxyHost <> '') then
    begin
      if (proxyUser <> '') or (proxyPass <> '') then
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080), proxyUser, proxyPass)
      else
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080));
    end
    else if forceNoProxy then
      client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, true)
    else
      client := TWinHTTPClient.Create(DEFAULT_USER_AGENT);

    try
      if contentType <> '' then
        client.AddHeader('Content-Type', contentType);
      client.SetRequestBody(body);
      try
        responseStr := client.Post(url);
        res := responseStr;
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
    end;
  end;

begin
  res := '';
  tempInstance := TTrndiNativeWindows.Create;
  try
    proxyHost := tempInstance.GetSetting('proxy.host', '', true);
    if proxyHost <> '' then
    begin
      proxyPort := tempInstance.GetSetting('proxy.port', '', true);
      proxyUser := tempInstance.GetSetting('proxy.user', '', true);
      proxyPass := tempInstance.GetSetting('proxy.pass', '', true);
      NormalizeProxyHostPort(proxyHost, proxyPort);
      if proxyPort = '' then
        proxyPort := '8080';
    end;

    if proxyHost <> '' then
    begin
      if PerformRequest(true, false) then
      begin
        Result := true;
        Exit;
      end;
    end;

    Result := PerformRequest(false, proxyHost <> '');
  finally
    tempInstance.Free;
  end;
end;

{------------------------------------------------------------------------------
  TestProxyURL
  ------------
  Proxy-only HTTP GET using WinHTTP client. No direct fallback.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.TestProxyURL(const url: string;
  const proxyHost: string; const proxyPort: string; const proxyUser: string;
  const proxyPass: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  client: TWinHTTPClient;
  host, portS, user, pass: string;

  procedure NormalizeProxyHostPort(var hostV: string; var portV: string);
  var
    s: string;
    p: integer;
    hostPart, portPart: string;
  begin
    s := Trim(hostV);

    p := Pos('://', s);
    if p > 0 then
      s := Copy(s, p + 3, MaxInt);

    p := Pos('/', s);
    if p > 0 then
      s := Copy(s, 1, p - 1);

    p := LastDelimiter(':', s);
    if (p > 0) and (p < Length(s)) then
    begin
      hostPart := Copy(s, 1, p - 1);
      portPart := Copy(s, p + 1, MaxInt);
      if (hostPart <> '') and (StrToIntDef(portPart, -1) > 0) then
      begin
        s := hostPart;
        if Trim(portV) = '' then
          portV := portPart;
      end;
    end;

    hostV := s;
    portV := Trim(portV);
  end;

begin
  res := '';
  Result := false;

  host := Trim(proxyHost);
  portS := Trim(proxyPort);
  user := Trim(proxyUser);
  pass := proxyPass;
  NormalizeProxyHostPort(host, portS);

  if host = '' then
  begin
    res := 'Proxy host is empty.';
    Exit(false);
  end;
  if portS = '' then
    portS := '8080';

  if (user <> '') or (pass <> '') then
    client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, host, StrToIntDef(portS, 8080), user, pass)
  else
    client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, host, StrToIntDef(portS, 8080));

  try
    try
      res := client.Get(url, []);
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
  end;
end;

{------------------------------------------------------------------------------
  SetDarkMode
  -----------
  Toggle immersive dark mode for a window (Windows 10 1809+ required).
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SetDarkMode(win: HWND;
Enable: boolean = true): boolean;
var
  Value: integer;
begin
  Result := false;
  // Check Windows version (Windows 10 1809+ required for immersive dark mode)
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and
    (Win32BuildNumber < 17763)) then
    Exit; // Windows 10 1809 (build 17763)

  Value := Ord(Enable);
  Result := Succeeded(DwmSetWindowAttribute(win, DWMWA_USE_IMMERSIVE_DARK_MODE,
    @Value, SizeOf(Value)));
end;

{------------------------------------------------------------------------------
  Dark mode for popup menus
  -------------------------
  Three steps are needed to fully darken Win11 popup menus:

  1. Opt the process into uxtheme's dark mode via SetPreferredAppMode
     (ordinal 135, Win10 1903+) or AllowDarkModeForApp (ordinal 132, Win10 1809).
  2. Install a per-thread WH_CBT hook that catches each popup-menu HWND (class
     "#32768") on creation and calls AllowDarkModeForWindow +
     SetWindowTheme(hwnd, 'DarkMode_Menu', nil) + immersive-dark on it.
  3. Subclass each popup-menu HWND to overpaint the frame. The visual style and
     DWM still draw a light gray 2-3 px border that none of (1)/(2) reach;
     Win11 renders the menu via WM_PRINT into an off-screen DC that DWM
     composites, so we hook WM_PRINT and stack a few dark FrameRects on top.

  uxtheme.dll is kept loaded for the lifetime of the process — releasing it
  would invalidate the hook procedure's resolved pointers.

  Item-level dark colors are handled separately via TMenuItem.OnDrawItem
  (see TfBG.pmSettingsDrawItem); this code only handles the popup frame
  + window-level theming so the two halves match.
 ------------------------------------------------------------------------------}
type
  TSetPreferredAppMode   = function(AppMode: integer): integer; stdcall;
  TAllowDarkModeForApp   = function(Allow: BOOL): BOOL; stdcall;
  TAllowDarkModeForWindow= function(hwnd: HWND; Allow: BOOL): BOOL; stdcall;
  TSetWindowThemeFn      = function(hwnd: HWND; pszSubAppName, pszSubIdList: PWideChar): HRESULT; stdcall;
  TFlushMenuThemes       = procedure; stdcall;

const
  DarkModeMenuTheme: array[0..13] of WideChar =
    ('D','a','r','k','M','o','d','e','_','M','e','n','u', #0);
  // Subclass id used for the menu border subclass. Unique per logical purpose.
  MENU_BORDER_SUBCLASS_ID = $7242D14;
  // Timer id used to re-overpaint chevrons. Hover-out repaints on Win11 popup
  // menus bypass WM_PRINT/WM_PRINTCLIENT/WM_PAINT (the theme paints directly),
  // so we re-decorate from a low-frequency timer for the popup's lifetime.
  MENU_CHEVRON_TIMER_ID  = $7242D15;
  MENU_CHEVRON_TIMER_MS  = 33;

type
  TSubclassProc = function(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
    uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;

function SetWindowSubclass(hWnd: HWND; pfnSubclass: TSubclassProc;
  uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): BOOL; stdcall;
  external 'comctl32.dll' name 'SetWindowSubclass';
function DefSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
  external 'comctl32.dll' name 'DefSubclassProc';
function RemoveWindowSubclass(hWnd: HWND; pfnSubclass: TSubclassProc;
  uIdSubclass: UINT_PTR): BOOL; stdcall;
  external 'comctl32.dll' name 'RemoveWindowSubclass';

var
  GUxThemeModule: HMODULE = 0;
  GAllowDarkModeForWindow: TAllowDarkModeForWindow = nil;
  GSetWindowThemeFn: TSetWindowThemeFn = nil;
  GMenuCBTHook: HHOOK = 0;

// Overpaint the submenu chevron for items with a submenu. The DarkMode_Menu
// theme draws a dark chevron in the normal state and a white one when the
// item is hovered, which gives an inconsistent two-tone look. After the
// default WM_PRINT paint we walk the menu, look up each submenu-bearing
// item's rect, and paint our own light chevron over the theme's output.
procedure PaintMenuChevrons(hwnd: HWND; dc: HDC);
const
  MN_GETHMENU_       = $01E1;
  MIIM_STATE_        = $0001;
  MIIM_SUBMENU_      = $0004;
  MFS_HILITE_        = $0080;
  BgNormal           = $00202020;
  BgSelected         = $00383838;
  ChevColor          = $00E0E0E0;
  ChevAreaRight      = 4;   // px from item's right edge to chevron right
  ChevAreaWidth      = 18;  // px wide swatch we overpaint
var
  menu: HMENU;
  i, count: integer;
  mii: TMenuItemInfoW;
  rcItem, rcWnd, rcCh: TRect;
  isHot: boolean;
  bgCol: COLORREF;
  bgBrush, chBrush, chPen, oldBrush, oldPen: HGDIOBJ;
  pts: array[0..2] of TPoint;
  cx, cy, h, w: integer;
begin
  menu := HMENU(SendMessage(hwnd, MN_GETHMENU_, 0, 0));
  if menu = 0 then
    Exit;
  GetWindowRect(hwnd, rcWnd);
  count := GetMenuItemCount(menu);

  for i := 0 to count - 1 do
  begin
    FillChar(mii, SizeOf(mii), 0);
    mii.cbSize := SizeOf(mii);
    mii.fMask := MIIM_SUBMENU_ or MIIM_STATE_;
    if not GetMenuItemInfoW(menu, i, true, @mii) then
      Continue;
    if mii.hSubMenu = 0 then
      Continue;
    if not GetMenuItemRect(hwnd, menu, i, rcItem) then
      Continue;

    // GetMenuItemRect returns screen coords; convert to window-DC space.
    OffsetRect(rcItem, -rcWnd.Left, -rcWnd.Top);
    isHot := (mii.fState and MFS_HILITE_) <> 0;
    if isHot then
      bgCol := BgSelected
    else
      bgCol := BgNormal;

    rcCh.Left   := rcItem.Right - ChevAreaWidth - ChevAreaRight;
    rcCh.Top    := rcItem.Top + 2;
    rcCh.Right  := rcItem.Right - ChevAreaRight;
    rcCh.Bottom := rcItem.Bottom - 2;
    bgBrush := CreateSolidBrush(bgCol);
    if bgBrush <> 0 then
    try
      FillRect(dc, rcCh, bgBrush);
    finally
      DeleteObject(bgBrush);
    end;

    // Draw our own chevron in a consistent light color, sized like the LCL menu.
    h := 4;
    w := 4;
    cx := rcItem.Right - ChevAreaRight - 6;
    cy := (rcItem.Top + rcItem.Bottom) div 2;
    pts[0].x := cx - w div 2; pts[0].y := cy - h;
    pts[1].x := cx + w div 2; pts[1].y := cy;
    pts[2].x := cx - w div 2; pts[2].y := cy + h;

    chBrush := CreateSolidBrush(ChevColor);
    chPen := CreatePen(PS_SOLID, 1, ChevColor);
    oldBrush := SelectObject(dc, chBrush);
    oldPen := SelectObject(dc, chPen);
    try
      Polygon(dc, pts[0], 3);
    finally
      SelectObject(dc, oldBrush);
      SelectObject(dc, oldPen);
      DeleteObject(chBrush);
      DeleteObject(chPen);
    end;
  end;
end;

// Subclass procedure attached to popup menu HWNDs so we can paint a dark
// border ourselves. Win11 draws the popup frame via the visual style and does
// not honor DWMWA_BORDER_COLOR on menu HWNDs (returns E_HANDLE). The menu is
// rendered via WM_PRINT into an off-screen DC that DWM composites, so
// WM_NCPAINT is never delivered — we hook WM_PRINT instead.
// Paint our dark border + light chevrons onto the popup's DC. Shared by the
// WM_PRINT (initial composite) and WM_PAINT (hover-driven repaint) branches.
procedure DecorateMenuPopup(hwnd: HWND; dc: HDC);
const
  BorderColor = $00202020;
var
  rcWnd: TRect;
  br: HBRUSH;
begin
  PaintMenuChevrons(hwnd, dc);

  GetWindowRect(hwnd, rcWnd);
  OffsetRect(rcWnd, -rcWnd.Left, -rcWnd.Top);
  br := CreateSolidBrush(BorderColor);
  if br <> 0 then
  try
    FrameRect(dc, rcWnd, br); InflateRect(rcWnd, -1, -1);
    FrameRect(dc, rcWnd, br); InflateRect(rcWnd, -1, -1);
    FrameRect(dc, rcWnd, br);
  finally
    DeleteObject(br);
  end;
end;

function MenuBorderSubclassProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM; uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
const
  WM_PAINT_        = $000F;
  WM_NCDESTROY_    = $0082;
  WM_TIMER_        = $0113;
  WM_PRINT_        = $0317;
  WM_PRINTCLIENT_  = $0318;
var
  dc: HDC;
begin
  case uMsg of
    WM_PRINT_, WM_PRINTCLIENT_:
    begin
      // Initial paint and any partial repaints that go through the print
      // messages: decorate over the default output.
      Result := DefSubclassProc(hwnd, uMsg, wParam, lParam);
      dc := HDC(wParam);
      if dc <> 0 then
        DecorateMenuPopup(hwnd, dc);
      // Install the chevron-overpaint timer for the rest of the popup's life.
      // Hover-out repaints bypass every window message we can hook, so a
      // low-frequency timer is the only reliable way to keep the chevrons
      // light. SetTimer is idempotent on the same (hwnd, id) pair.
      SetTimer(hwnd, MENU_CHEVRON_TIMER_ID, MENU_CHEVRON_TIMER_MS, nil);
      Exit;
    end;
    WM_PAINT_:
    begin
      Result := DefSubclassProc(hwnd, uMsg, wParam, lParam);
      dc := GetWindowDC(hwnd);
      if dc <> 0 then
      try
        DecorateMenuPopup(hwnd, dc);
      finally
        ReleaseDC(hwnd, dc);
      end;
      Exit;
    end;
    WM_TIMER_:
    begin
      if wParam = MENU_CHEVRON_TIMER_ID then
      begin
        dc := GetWindowDC(hwnd);
        if dc <> 0 then
        try
          PaintMenuChevrons(hwnd, dc);
        finally
          ReleaseDC(hwnd, dc);
        end;
        Result := 0;
        Exit;
      end;
    end;
    WM_NCDESTROY_:
    begin
      KillTimer(hwnd, MENU_CHEVRON_TIMER_ID);
      RemoveWindowSubclass(hwnd, TSubclassProc(@MenuBorderSubclassProc), uIdSubclass);
    end;
  end;
  Result := DefSubclassProc(hwnd, uMsg, wParam, lParam);
end;

function MenuDarkModeCBTProc(nCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
const
  PopupMenuClass = '#32768';
var
  cls: array[0..31] of AnsiChar;
begin
  if (nCode = HCBT_CREATEWND) and (HWND(wParam) <> 0) then
  begin
    if (GetClassNameA(HWND(wParam), @cls[0], SizeOf(cls)) > 0) and
       (StrComp(@cls[0], PopupMenuClass) = 0) then
    begin
      if Assigned(GAllowDarkModeForWindow) then
        GAllowDarkModeForWindow(HWND(wParam), true);
      if Assigned(GSetWindowThemeFn) then
        GSetWindowThemeFn(HWND(wParam), @DarkModeMenuTheme[0], nil);
      TTrndiNativeWindows.SetDarkMode(HWND(wParam), true);
      // Subclass the popup so WM_PRINT paints a dark border. SetWindowSubclass
      // is idempotent per (hwnd, proc, id); menu HWNDs are also torn down and
      // recreated each time the menu opens, so re-installs are safe.
      SetWindowSubclass(HWND(wParam), @MenuBorderSubclassProc,
        MENU_BORDER_SUBCLASS_ID, 0);
    end;
  end;
  Result := CallNextHookEx(GMenuCBTHook, nCode, wParam, lParam);
end;

class function TTrndiNativeWindows.SetPreferredDarkMode: boolean;
const
  PreferredAppMode_AllowDark = 1;
var
  fnSetPreferred: TSetPreferredAppMode;
  fnAllowApp: TAllowDarkModeForApp;
  fnFlush: TFlushMenuThemes;
begin
  Result := false;
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and
    (Win32BuildNumber < 17763)) then
    Exit;

  if GUxThemeModule = 0 then
    GUxThemeModule := LoadLibrary('uxtheme.dll');
  if GUxThemeModule = 0 then
    Exit;

  fnSetPreferred := TSetPreferredAppMode(GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(135)));
  if Assigned(fnSetPreferred) then
  begin
    fnSetPreferred(PreferredAppMode_AllowDark);
    Result := true;
  end
  else
  begin
    fnAllowApp := TAllowDarkModeForApp(GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(132)));
    if Assigned(fnAllowApp) then
    begin
      fnAllowApp(true);
      Result := true;
    end;
  end;

  if not Assigned(GAllowDarkModeForWindow) then
    GAllowDarkModeForWindow := TAllowDarkModeForWindow(
      GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(133)));
  if not Assigned(GSetWindowThemeFn) then
    GSetWindowThemeFn := TSetWindowThemeFn(GetProcAddress(GUxThemeModule, 'SetWindowTheme'));

  // Theme the AppHandle window (menu's owner in LCL's TrackPopupMenuEx call).
  // Windows uses the owner window's dark-mode flag when drawing popup frames.
  if Assigned(Application) and (Application.Handle <> 0) then
  begin
    if Assigned(GAllowDarkModeForWindow) then
      GAllowDarkModeForWindow(Application.Handle, true);
    SetDarkMode(Application.Handle, true);
  end;

  // Install the per-thread CBT hook so popup menu HWNDs get themed and
  // subclassed on creation. Idempotent — only installs once per process.
  if (GMenuCBTHook = 0) and Assigned(GSetWindowThemeFn) then
    GMenuCBTHook := SetWindowsHookEx(WH_CBT, @MenuDarkModeCBTProc, 0, GetCurrentThreadId);

  fnFlush := TFlushMenuThemes(GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(136)));
  if Assigned(fnFlush) then
    fnFlush;
end;

function SetDwmAttr(hWnd: HWND; Attr: DWORD; const Data; Size: DWORD): HRESULT;
begin
  Result := DwmSetWindowAttribute(hWnd, Attr, @Data, Size);
end;

function HrSucceeded(hr: HRESULT): boolean; inline;
begin
  Result := hr >= 0; // SUCCEEDED(hr)
end;

{------------------------------------------------------------------------------
  SetTitleColor
  -------------
  Apply caption and text colors to a window using DWM attributes.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SetTitleColor(form: PtrUInt;
bg, Text: TColor): boolean;
const
  MIN_DWM_COLOR_BUILD = 17763; // Win10 1809 (2018-10)
var
  bgColor, textColor: COLORREF;
  hrCaption, hrText: HRESULT;
begin
  // Guard: DWMWA_CAPTION_COLOR (35) & DWMWA_TEXT_COLOR (36) are supported from
  // Windows 10 1809 (build 17763, Oct 2018). Earlier versions will just fail.
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and
    (Win32BuildNumber < MIN_DWM_COLOR_BUILD)) then
    Exit(false);

  // TColor and COLORREF share 0x00BBGGRR layout; no byte swap required.
  bgColor := COLORREF(ColorToRGB(bg));
  textColor := COLORREF(ColorToRGB(Text));

  hrCaption := SetDwmAttr(form, DWMWA_CAPTION_COLOR, bgColor, SizeOf(bgColor));
  hrText := SetDwmAttr(form, DWMWA_TEXT_COLOR, textColor, SizeOf(textColor));

  Result := HrSucceeded(hrCaption) and HrSucceeded(hrText);
end;

{------------------------------------------------------------------------------
  SetBadge
  --------
  Compose app icon with a badge showing Value; applies to taskbar icon.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetBadge(const Value: string; BadgeColor: TColor;
badge_size_ratio: double; min_font_size: integer);
const
  INITIAL_FONT_SIZE_RATIO = 0.5;
  TEXT_PADDING = 4;
  CORNER_RADIUS = 6;
var
  AppIcon, TempIcon: TIcon;
  Bitmap: Graphics.TBitmap;
  BadgeText: string;
  TextWidth, TextHeight: integer;
  BadgeRect, TrendRect: Classes.TRect;
  IconWidth, IconHeight, BadgeSize, TrendSize: integer;
  FontSize, Radius, TrendRadius, py: integer;
  SmallIconSize: integer;
  TextColor: TColor;
  dval: double;
  borderColor: TColor;
  ShellIcon: HICON;

function Luminance(c: TColor): double;
  var
    rc: longint;
    r, g, b: byte;
  begin
    rc := ColorToRGB(c);
    r := GetRValue(rc);
    g := GetGValue(rc);
    b := GetBValue(rc);
    Result := 0.299 * r + 0.587 * g + 0.114 * b;
  end;

function AdjustColor(c: TColor; factor: double): TColor;
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

  // GDI canvas operations (RoundRect, TextOut) do not write the alpha channel
  // on 32-bit bitmaps. Walk every pixel in the given badge rect and set
  // alpha=255 for any pixel inside the rounded shape. Pixels in the rounded
  // corners (outside the shape) keep the alpha already written by DrawIconEx.
procedure FixBadgeAlpha(const R: Classes.TRect; ARadius: integer);
  var
    px, py: integer;
    pRow: PByte;
  begin
    // Push any batched GDI drawing into the DIB before touching raw pixels.
    GdiFlush;
    for py := R.Top to R.Bottom - 1 do
    begin
      pRow := PByte(Bitmap.ScanLine[py]);
      for px := R.Left to R.Right - 1 do
      begin
        if (px < R.Left + ARadius) and (py < R.Top + ARadius) then
        begin
          if Sqr(px - (R.Left + ARadius)) + Sqr(py - (R.Top + ARadius)) > Sqr(ARadius) then
            Continue;
        end
        else if (px >= R.Right - ARadius) and (py < R.Top + ARadius) then
        begin
          if Sqr(px - (R.Right - ARadius)) + Sqr(py - (R.Top + ARadius)) > Sqr(ARadius) then
            Continue;
        end
        else if (px < R.Left + ARadius) and (py >= R.Bottom - ARadius) then
        begin
          if Sqr(px - (R.Left + ARadius)) + Sqr(py - (R.Bottom - ARadius)) > Sqr(ARadius) then
            Continue;
        end
        else if (px >= R.Right - ARadius) and (py >= R.Bottom - ARadius) then
        begin
          if Sqr(px - (R.Right - ARadius)) + Sqr(py - (R.Bottom - ARadius)) > Sqr(ARadius) then
            Continue;
        end;
        (pRow + px * 4 + 3)^ := 255;
      end;
    end;
  end;

begin
  TrndiDLog(Format('SetBadge: Value="%s" BadgeColor=%d ratio=%.3f min_font=%d',
    [Value, integer(BadgeColor), badge_size_ratio, min_font_size]));

  // Lazily snapshot the pristine app icon the first time we run, before any badge
  // has mutated Application.Icon. All subsequent badge composites read from this
  // copy — preventing the upper-left "cascade" where each call would re-badge
  // its own previous output.
  if (gOriginalAppIcon = nil) and (Application.Icon <> nil) and
     (not Application.Icon.Empty) then
  begin
    gOriginalAppIcon := TIcon.Create;
    gOriginalAppIcon.Assign(Application.Icon);
  end;

  AppIcon := TIcon.Create;
  TempIcon := TIcon.Create;
  Bitmap := Graphics.TBitmap.Create;
  try
    if Value = '' then
    begin
      if (gOriginalAppIcon <> nil) and (not gOriginalAppIcon.Empty) then
        Application.Icon.Assign(gOriginalAppIcon)
      else
        Application.Icon.Assign(Application.MainForm.Icon);
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG, 0);
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL, 0);
      if gLastBadgeIcon <> 0 then
      begin
        DestroyIcon(gLastBadgeIcon);
        gLastBadgeIcon := 0;
      end;
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

    // Source from the cached pristine icon, never Application.Icon — we write
    // back to Application.Icon at the end, so reading from it would composite
    // onto the previous badge and produce a recursive cascade in the upper-left.
    if (gOriginalAppIcon <> nil) and (not gOriginalAppIcon.Empty) then
      AppIcon.Assign(gOriginalAppIcon)
    else
      AppIcon.Assign(Application.Icon);
    IconWidth := AppIcon.Width;
    IconHeight := AppIcon.Height;
    if (IconWidth <= 0) or (IconHeight <= 0) then
    begin
      IconWidth := 32;
      IconHeight := 32;
    end;

    if IconWidth < IconHeight then
      BadgeSize := Round(IconWidth * badge_size_ratio)
    else
      BadgeSize := Round(IconHeight * badge_size_ratio);

    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(IconWidth, IconHeight);
    for py := 0 to IconHeight - 1 do
      FillChar(Bitmap.ScanLine[py]^, IconWidth * 4, 0);

    DrawIconEx(Bitmap.Canvas.Handle, 0, 0, AppIcon.Handle, IconWidth,
      IconHeight, 0, 0, DI_NORMAL);

    // The trend badge occupies the same top-left corner as the small logo
    // copy; don't draw the logo just to paint over it.
    if FBadgeTrend = '' then
    begin
      SmallIconSize := Round(Min(IconWidth, IconHeight) * 0.4);
      if SmallIconSize < 6 then SmallIconSize := 6;
      DrawIconEx(Bitmap.Canvas.Handle, 0, 0, AppIcon.Handle, SmallIconSize,
        SmallIconSize, 0, 0, DI_NORMAL);
    end;

    // Compute a badge rectangle in the lower-right quadrant with size
    // proportional to the current icon dimensions.
    BadgeRect := Classes.Rect(IconWidth - BadgeSize, IconHeight -
      BadgeSize, IconWidth, IconHeight);

    if Luminance(BadgeColor) > 140 then
      borderColor := AdjustColor(BadgeColor, 0.55)
    else
      borderColor := AdjustColor(BadgeColor, 1.35);

    Bitmap.Canvas.Brush.Color := BadgeColor;
    Bitmap.Canvas.Pen.Color := borderColor;
    Bitmap.Canvas.Pen.Width := 1;
    Bitmap.Canvas.Pen.Style := psSolid;

    Radius := 0;
    if BadgeSize <= 12 then
      Bitmap.Canvas.FillRect(BadgeRect)
    else
    begin
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then
        Radius := 2;
      RoundRect(Bitmap.Canvas.Handle,
        BadgeRect.Left, BadgeRect.Top,
        BadgeRect.Right, BadgeRect.Bottom,
        Radius * 2, Radius * 2);
    end;

    // Choose text color based on perceived luminance for contrast.
    if (0.299 * GetRValue(BadgeColor) + 0.587 * GetGValue(BadgeColor) +
      0.114 * GetBValue(BadgeColor)) > 128 then
      TextColor := clBlack
    else
      TextColor := clWhite;

    Bitmap.Canvas.Font.Name := 'Segoe UI';
    Bitmap.Canvas.Font.Style := [fsBold];
    Bitmap.Canvas.Font.Color := TextColor;
    FontSize := Round(BadgeSize * INITIAL_FONT_SIZE_RATIO);
    if FontSize < min_font_size then
      FontSize := min_font_size;
    Bitmap.Canvas.Font.Size := FontSize;

    TextWidth := Bitmap.Canvas.TextWidth(BadgeText);
    TextHeight := Bitmap.Canvas.TextHeight(BadgeText);
    // Fit text within the badge; avoid shrinking below the requested minimum.
    while (TextWidth > (BadgeSize - TEXT_PADDING)) and (FontSize > min_font_size - 2) do
    begin
      Dec(FontSize);
      Bitmap.Canvas.Font.Size := FontSize;
      TextWidth := Bitmap.Canvas.TextWidth(BadgeText);
      TextHeight := Bitmap.Canvas.TextHeight(BadgeText);
    end;

    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.TextOut(
      BadgeRect.Left + ((BadgeRect.Right - BadgeRect.Left) - TextWidth) div 2,
      BadgeRect.Top + ((BadgeRect.Bottom - BadgeRect.Top) - TextHeight) div 2,
      BadgeText
      );

    // Optional trend arrow: a second, smaller badge in the top-left corner,
    // drawn in the same style/colors as the value badge. All canvas drawing
    // must finish before the ScanLine alpha pass below — mixing raw pixel
    // access with further canvas ops loses the composed image.
    TrendRadius := 0;
    if FBadgeTrend <> '' then
    begin
      TrendSize := Round(Min(IconWidth, IconHeight) * 0.45);
      if TrendSize < 8 then
        TrendSize := 8;
      TrendRect := Classes.Rect(0, 0, TrendSize, TrendSize);

      Bitmap.Canvas.Brush.Style := bsSolid;
      Bitmap.Canvas.Brush.Color := BadgeColor;
      Bitmap.Canvas.Pen.Color := borderColor;

      if TrendSize <= 12 then
        Bitmap.Canvas.FillRect(TrendRect)
      else
      begin
        TrendRadius := Round(CORNER_RADIUS * TrendSize / 32);
        if TrendRadius < 2 then
          TrendRadius := 2;
        RoundRect(Bitmap.Canvas.Handle,
          TrendRect.Left, TrendRect.Top,
          TrendRect.Right, TrendRect.Bottom,
          TrendRadius * 2, TrendRadius * 2);
      end;

      Bitmap.Canvas.Font.Color := TextColor;
      FontSize := Round(TrendSize * INITIAL_FONT_SIZE_RATIO);
      if FontSize < 5 then
        FontSize := 5;
      Bitmap.Canvas.Font.Size := FontSize;
      TextWidth := Bitmap.Canvas.TextWidth(FBadgeTrend);
      TextHeight := Bitmap.Canvas.TextHeight(FBadgeTrend);
      while (TextWidth > (TrendSize - 2)) and (FontSize > 5) do
      begin
        Dec(FontSize);
        Bitmap.Canvas.Font.Size := FontSize;
        TextWidth := Bitmap.Canvas.TextWidth(FBadgeTrend);
        TextHeight := Bitmap.Canvas.TextHeight(FBadgeTrend);
      end;

      Bitmap.Canvas.Brush.Style := bsClear;
      Bitmap.Canvas.TextOut(
        TrendRect.Left + ((TrendRect.Right - TrendRect.Left) - TextWidth) div 2,
        TrendRect.Top + ((TrendRect.Bottom - TrendRect.Top) - TextHeight) div 2,
        FBadgeTrend
        );
    end;

    FixBadgeAlpha(BadgeRect, Radius);
    if FBadgeTrend <> '' then
      FixBadgeAlpha(TrendRect, TrendRadius);

    // Assign the composed bitmap to the app icon and notify the window.
    TempIcon.Assign(Bitmap);
    // Duplicate the icon for the shell while the previously sent handle
    // (gLastBadgeIcon, or the initial Application.Icon.Handle) is still
    // alive — that forces a different numeric HICON value, which Explorer
    // requires before it re-reads the taskbar button icon.
    ShellIcon := CopyIcon(TempIcon.Handle);
    Application.Icon.Assign(TempIcon);
    if ShellIcon <> 0 then
    begin
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG, ShellIcon);
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL, ShellIcon);
      // The shell now references the new handle; retire the previous one.
      if gLastBadgeIcon <> 0 then
        DestroyIcon(gLastBadgeIcon);
      gLastBadgeIcon := ShellIcon;
    end
    else
    begin
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG,
        Application.Icon.Handle);
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL,
        Application.Icon.Handle);
    end;
  finally
    Bitmap.Free;
    AppIcon.Free;
    TempIcon.Free;
  end;
end;

{------------------------------------------------------------------------------
  WarmSettingsCache
  -----------------
  Snapshot every value under HKCU\Software\Trndi\ into the process-wide cache
  in a single registry transaction. After this runs, ordinary GetSetting calls
  are string-table lookups — the bulk of startup's ~70 registry transactions
  go away.
 ------------------------------------------------------------------------------}
procedure WarmSettingsCache;
var
  reg: TRegistry;
  names, snapshot: TStringList;
  i: integer;
  k: string;
begin
  // Hold the cache lock for the entire enum+seed so a concurrent SetSetting
  // can't interleave (write to registry, update cache) between our enum and
  // our seed — that interleaving would let SeedSettingsCache wipe the value
  // the writer just placed in the cache.
  TTrndiNativeBase.FSettingsCacheLock.Enter;
  try
    // Double-checked: another thread may have warmed while we waited for
    // the lock.
    if TTrndiNativeBase.FSettingsCacheWarm then
      Exit;
    names := TStringList.Create;
    snapshot := TStringList.Create;
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CURRENT_USER;
      if reg.OpenKeyReadOnly('\SOFTWARE\Trndi\') then
      begin
        reg.GetValueNames(names);
        for i := 0 to names.Count - 1 do
        begin
          k := names[i];
          snapshot.Add(k + '=' + reg.ReadString(k));
        end;
      end;
      TTrndiNativeBase.SeedSettingsCache(snapshot);
    finally
      reg.Free;
      snapshot.Free;
      names.Free;
    end;
  finally
    TTrndiNativeBase.FSettingsCacheLock.Leave;
  end;
end;

{------------------------------------------------------------------------------
  GetSetting
  ----------
  Read a value from HKCU\Software\Trndi\; returns def if not present.

  Backed by a process-wide cache: the first call enumerates the whole
  registry key in one open/close pair, and subsequent calls hit memory only.
  Writes (Set/Delete/Import) keep the cache coherent; ReloadSettings drops it.
 ------------------------------------------------------------------------------}
function TTrndiNativeWindows.GetSetting(const keyname: string; def: string;
global: boolean): string;
var
  reg: TRegistry;
  key, cached: string;
begin
  key := buildKey(keyname, global);

  if not TTrndiNativeBase.FSettingsCacheWarm then
    WarmSettingsCache;

  if TTrndiNativeBase.TryGetCachedSetting(key, cached) then
  begin
    // Legacy behaviour: an empty stored value and a missing key both collapse
    // to def. Preserve that so callers don't need to change.
    if cached = '' then
      Result := def
    else
      Result := cached;
    Exit;
  end;

  // Cold-cache fallback (only if warming somehow failed). Read from registry
  // and cache the result so subsequent reads are fast.
  Result := def;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('\SOFTWARE\Trndi\') then
      if reg.ValueExists(key) then
      begin
        Result := reg.ReadString(key);
        TTrndiNativeBase.SetCachedSetting(key, Result);
      end;
  finally
    reg.Free;
  end;
end;

{------------------------------------------------------------------------------
  SetSetting
  ----------
  Write a value to HKCU\Software\Trndi\.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetSetting(const keyname: string;
const val: string; global: boolean);
var
  reg: TRegistry;
  key: string;
begin
  key := buildKey(keyname, global);
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', true) then
    begin
      reg.WriteString(key, val);
      TTrndiNativeBase.SetCachedSetting(key, val);
    end
    else
      TrndiDLog('SetSetting: failed to open HKCU\SOFTWARE\Trndi for key ' + key);
  finally
    reg.Free;
  end;
end;

{------------------------------------------------------------------------------
  DeleteSetting
  -------------
  Delete a value from HKCU\Software\Trndi\ if it exists.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.DeleteSetting(const keyname: string; global: boolean);
var
  reg: TRegistry;
  key: string;
begin
  key := buildKey(keyname, global);
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', false) then
      if reg.ValueExists(key) then
        reg.DeleteValue(key);
  finally
    reg.Free;
  end;
  TTrndiNativeBase.RemoveCachedSetting(key);
end;

{------------------------------------------------------------------------------
  ReloadSettings
  --------------
  No-op for registry-backed settings (access is on-demand).
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.ReloadSettings;
begin
  // Drop the in-memory cache so the next read re-warms from the registry.
  // Use this after another process (or another TrndiNative instance via
  // ImportSettings) may have written to HKCU\Software\Trndi.
  TTrndiNativeBase.ClearSettingsCache;
end;

{------------------------------------------------------------------------------
  ExportSettings
  --------------
  Export all registry settings to INI format string.
 ------------------------------------------------------------------------------}
function TTrndiNativeWindows.ExportSettings: string;
var
  reg: TRegistry;
  sl: TStringList;
  i: integer;
  valueNames: TStringList;
  keyName: string;
begin
  Result := '';
  sl := TStringList.Create;
  valueNames := TStringList.Create;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('\SOFTWARE\Trndi\') then
    begin
      reg.GetValueNames(valueNames);
      sl.Add('[trndi]');
      for i := 0 to valueNames.Count - 1 do
      begin
        keyName := valueNames[i];
        sl.Add(keyName + '=' + reg.ReadString(keyName));
      end;
    end;
    Result := sl.Text;
  finally
    reg.Free;
    valueNames.Free;
    sl.Free;
  end;
end;

{------------------------------------------------------------------------------
  ImportSettings
  ---------------
  Import settings from INI format string to registry.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.ImportSettings(const iniData: string);
var
  sl: TStringList;
  mem: TMemoryStream;
  ini: TMemIniFile;
  sections, keys: TStringList;
  i, j: integer;
  section, key, value: string;
  reg: TRegistry;
begin
  if iniData = '' then
    Exit;
  sl := TStringList.Create;
  mem := TMemoryStream.Create;
  ini := nil;
  sections := TStringList.Create;
  keys := TStringList.Create;
  reg := TRegistry.Create;
  try
    mem.WriteBuffer(iniData[1], Length(iniData));
    mem.Position := 0;
    sl.LoadFromStream(mem);
    
    // Create a temporary INI file in memory
    ini := TMemIniFile.Create('');
    ini.SetStrings(sl);
    
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', true) then
    begin
      ini.ReadSections(sections);
      for i := 0 to sections.Count - 1 do
      begin
        section := sections[i];
        ini.ReadSection(section, keys);
        for j := 0 to keys.Count - 1 do
        begin
          key := keys[j];
          value := ini.ReadString(section, key, '');
          reg.WriteString(key, value);
        end;
      end;
    end;
  finally
    reg.Free;
    keys.Free;
    sections.Free;
    ini.Free;
    mem.Free;
    sl.Free;
  end;
  // Bulk write bypassed the per-key cache update path; drop the cache so the
  // next read picks up the imported values.
  TTrndiNativeBase.ClearSettingsCache;
end;

{------------------------------------------------------------------------------
  updateBegin
  -----------
  Signal the start of a long-running update operation (show taskbar progress).
 ------------------------------------------------------------------------------}
procedure EnsureGlobalTaskbar(const Context: string);
var
  chosenHandle: HWND;
begin
  // Idempotent lazy-init used by updateBegin/updateDone to ensure a
  // usable GlobalTaskbar instance exists (keeps DEBUG diagnostics identical)
  if (GlobalTaskbar = nil) or (not GlobalTaskbar.Initialized) then
  begin
    {$ifdef DEBUG}
    TrndiDLog(Format('%s: GlobalTaskbar nil/uninitialized — attempting lazy init', [Context]));
    TrndiDLog(PChar(Format('[Trndi] %s: attempting lazy GlobalTaskbar init', [Context])));
    {$endif}
    try
      if Assigned(GlobalTaskbar) then FreeAndNil(GlobalTaskbar);
      chosenHandle := 0;
      if Assigned(Application) and Assigned(Application.MainForm) then
        chosenHandle := Application.MainForm.Handle;
      GlobalTaskbar := TWinTaskbar.Create(chosenHandle);
      if Assigned(GlobalTaskbar) then
      begin
        {$ifdef DEBUG}
        TrndiDLog(PChar(Format('[Trndi] %s: lazy init result Initialized=%s handle=%d LastError=%s',
          [Context, BoolToStr(GlobalTaskbar.Initialized, True), GlobalTaskbar.WindowHandle, GlobalTaskbar.LastError])));
        TrndiDLog(Format('%s: lazy init result Initialized=%s, handle=%d, LastError=%s',
          [Context, BoolToStr(GlobalTaskbar.Initialized, True), GlobalTaskbar.WindowHandle, GlobalTaskbar.LastError]));
        {$endif}
      end
      else
      begin
        {$ifdef DEBUG}
        TrndiDLog(PChar(Format('[Trndi] %s: lazy init result = nil', [Context])));
        TrndiDLog(Format('%s: lazy init result = nil', [Context]));
        {$endif}
      end;
    except
      on E: Exception do
      begin
        {$ifdef DEBUG}
        TrndiDLog(PChar(Format('[Trndi] %s: lazy init exception: %s', [Context, E.Message])));
        TrndiDLog(Format('%s: lazy init exception: %s', [Context, E.Message]));
        {$endif}
        if Assigned(GlobalTaskbar) then FreeAndNil(GlobalTaskbar);
      end;
    end;
  end;
end;

procedure TTrndiNativeWindows.updateBegin;
var
  tb: TWinTaskbar;
  ok: Boolean;
  chosenHandle: HWND;
  // Diagnostics variables for taskbar HWND inspection
  wh: HWND;
  buf: array[0..511] of WideChar;
  cls: array[0..255] of WideChar;
  cap: string;
  wndClassName: string;
  exstyle: NativeUInt;
  style: NativeUInt;
begin
  {$ifdef DEBUG}
  // Always log attempt so we can diagnose Release builds
  TrndiDLog('updateBegin: Getting global taskbar');

  // Emit application/window diagnostics so we can verify which HWND we target.
  TrndiDLog(PChar(Format('[Trndi] MainFormOnTaskbar=%s MainForm.Handle=%d Application.Handle=%d',
    [BoolToStr(Application.MainFormOnTaskbar, True), PtrInt(Application.MainForm.Handle), PtrInt(Application.Handle)])));
  {$endif}
  // Use centralized lazy-init helper to avoid duplication and drift
  EnsureGlobalTaskbar('updateBegin');

  tb := GlobalTaskbar;
  // Emit an OS-level debug trace (visible with DebugView) in all builds
  {$ifdef DEBUG}
  if Assigned(tb) then
    TrndiDLog(PChar(Format('[Trndi] updateBegin: GlobalTaskbar initialized=%s handle=%d', [BoolToStr(tb.Initialized, True), tb.WindowHandle])))
  else
    TrndiDLog(PChar('[Trndi] updateBegin: GlobalTaskbar = nil'));
  {$endif}

  {$ifdef DEBUG}
  if Assigned(tb) then
    TrndiDLog(Format('updateBegin: GlobalTaskbar returned (Initialized=%s, handle=%d)',
      [BoolToStr(tb.Initialized, True), tb.WindowHandle]))
  else
    TrndiDLog('updateBegin: GlobalTaskbar returned nil');
  {$endif}

  if Assigned(tb) and tb.Initialized then
  begin
    // --- Additional diagnostics: log window title/class/styles for the chosen HWND ---
    {$ifdef DEBUG}
    try
      // Safe helper inline (avoid adding new global funcs): log info for the taskbar target
      wh := tb.WindowHandle;
      cap := '';
      wndClassName := '';

      if (wh <> 0) and IsWindow(wh) then
      begin
        if GetWindowTextW(wh, buf, Length(buf)) > 0 then
          cap := Trim(string(buf));
        if GetClassNameW(wh, cls, Length(cls)) > 0 then
          wndClassName := Trim(string(cls));
        TrndiDLog(Format('updateBegin: Taskbar target HWND=%d Title="%s" Class="%s" Visible=%s',
          [wh, cap, wndClassName, BoolToStr(IsWindowVisible(wh), True)]));

        // Log extended styles that may prevent a taskbar button (toolwindow etc.)
        exstyle := NativeUInt(GetWindowLongPtr(wh, GWL_EXSTYLE));
        style := NativeUInt(GetWindowLongPtr(wh, GWL_STYLE));
      end
      else
        TrndiDLog('updateBegin: Taskbar target HWND is invalid or not a window');

      // Enumerate top-level windows owned by this process and log candidates
      TrndiDLog('updateBegin: Enumerating top-level windows for this PID:');
      // Use a unit-level callback to avoid nested-declaration/calling-convention issues
      EnumWindows(@EnumLogWnd_UpdateBegin, 0);
    except
      on E: Exception do
        TrndiDLog('updateBegin: Diagnostics enumeration failed: ' + E.Message);
    end;
    {$endif}
    // --- end diagnostics ---

    // Ensure the main form is visible or minimized so progress can be shown on taskbar
    try
      if Assigned(Application.MainForm) and
         (not Application.MainForm.Visible) and
         (Application.MainForm.WindowState <> wsMinimized) then
      begin
        Application.MainForm.WindowState := wsMinimized;
        {$ifdef DEBUG}
        TrndiDLog(PChar('[Trndi] updateBegin: Minimized main form to show progress'));
       {$endif}
      end;
    except
      on E: Exception do
       {$ifdef DEBUG}TrndiDLog(PChar('[Trndi] updateBegin: Exception minimizing form: ' + E.Message));{$else};{$endif}
    end;

    // Use indeterminate progress during the fetch (more visible).
    // No SetProgressValue here: a value call would flip the button back to
    // TBPF_NORMAL and cancel the marquee.
    ok := tb.SetProgressState(tbpsIndeterminate);

    {$ifdef DEBUG}
    // Trace the API call result via TrndiDLog (always) and TrndiDLog (DEBUG only)
    if ok then
      TrndiDLog(PChar('[Trndi] updateBegin: SetProgressState(tbpsIndeterminate) succeeded'))
    else
      TrndiDLog(PChar('[Trndi] updateBegin: progress API call failed: ' + tb.LastError));

    if ok then
      TrndiDLog('updateBegin: SetProgressState(tbpsIndeterminate) succeeded')
    else
      TrndiDLog('updateBegin: progress API call failed: ' + tb.LastError);
    {$endif}
  end
  else
  begin
    {$ifdef DEBUG}
    TrndiDLog('updateBegin: GlobalTaskbar not available or not initialized');

    TrndiDLog(PChar('[Trndi] updateBegin: GlobalTaskbar not available or not initialized'));
    {$endif}
  end;
end;

{------------------------------------------------------------------------------
  updateDone
  ----------
  Signal the completion of a long-running update operation (clear taskbar progress).
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.updateDone;
var
  chosenHandle: HWND;
begin
  {$ifdef DEBUG}
  TrndiDLog('updateDone: Getting global taskbar');
  {$endif}
  
  // Use centralized lazy-init helper to avoid duplication and drift
  EnsureGlobalTaskbar('updateDone');

  if Assigned(GlobalTaskbar) and GlobalTaskbar.Initialized then
  begin
    {$ifdef DEBUG}
    TrndiDLog(PChar('[Trndi] updateDone: Clearing taskbar progress (tbpsNone)'));

    TrndiDLog('updateDone: Setting progress state to none');
    {$endif}
    GlobalTaskbar.SetProgressState(tbpsNone);
  end
  else
  begin
    {$ifdef DEBUG}
    TrndiDLog(PChar('[Trndi] updateDone: GlobalTaskbar not available or not initialized'));

    TrndiDLog('updateDone: GlobalTaskbar not available or not initialized');
    {$endif}
  end;
end;

{------------------------------------------------------------------------------
  request (Windows)
  -----------------
  HTTP GET/POST via WinHTTPClient. Honours proxy.* root settings: if a proxy
  is configured the request runs through it first, then re-tries direct on
  failure.
 ------------------------------------------------------------------------------}
function TTrndiNativeWindows.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
const header: string; prefix: boolean): string;
var
  client: TWinHTTPClient;
  address: string;
  p: integer;
  headerKey, headerVal: string;
  hasParams: boolean;
  ResStr: string;
  proxyHost: string;
  proxyPortS: string;
  proxyPort: integer;
  proxyUser: string;
  proxyPass: string;

  procedure ConfigureClient(aClient: TWinHTTPClient);
  begin
    aClient.AddHeader('User-Agent', useragent);

    if header <> '' then
    begin
      // Split on the first '=' only, like the Linux/macOS implementations —
      // header values may themselves contain '=' (e.g. base64 API secrets).
      p := Pos('=', header);
      if p > 0 then
      begin
        headerKey := Trim(Copy(header, 1, p - 1));
        headerVal := Trim(Copy(header, p + 1, MaxInt));
        if headerKey <> '' then
          aClient.AddHeader(headerKey, headerVal);
      end;
    end;

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
        if (jsondata = '') and hasParams then
          outRes := aClient.Get(address, [])
        else
          outRes := aClient.Get(address, params);
      end;
      Result := true;
      TrndiDLog('Windows: Request succeeded');
    except
      on E: Exception do
      begin
        outRes := E.Message;
        TrndiDLog('Windows: Request failed with exception: ' + E.Message);
        Result := false;
      end;
    end;
  end;

begin
  hasParams := (Length(params) > 0);

  if prefix then
    address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])])
  else
    address := endpoint;

  if (jsondata = '') and hasParams then
  begin
    address := address + '?' + params[0];
    for p := 1 to High(params) do
      address := address + '&' + params[p];
  end;

  proxyHost  := Trim(GetRootSetting('proxy.host', ''));
  proxyPortS := Trim(GetRootSetting('proxy.port', ''));
  proxyPort  := StrToIntDef(proxyPortS, 8080);
  proxyUser  := GetRootSetting('proxy.user', '');
  proxyPass  := GetRootSetting('proxy.pass', '');

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
      TryRequest(client, ResStr);
      Result := ResStr;
    finally
      client.Free;
    end;
    Exit;
  end;

  TrndiDLog('Windows: Using direct connection (no proxy configured) to: ' + address);
  client := TWinHTTPClient.Create(useragent, true);
  try
    TryRequest(client, ResStr);
    Result := ResStr;
  finally
    client.Free;
  end;
end;

{------------------------------------------------------------------------------
  requestEx (Windows)
  -------------------
  Cookie-aware, redirect-following HTTP via WinHTTP. Honours proxy.* root
  settings with the same proxy-first / direct fallback as @link(request).
 ------------------------------------------------------------------------------}
function TTrndiNativeWindows.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList;
prefix: boolean): THTTPResponse;
var
  address, sx, currentUrl, bodyData, methodLabel: string;
  responseHeaders: TStringList;
  responseBody: string;
  statusCode, j: integer;
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
    schemePos, rootPos: integer;
    baseRoot, baseDir: string;
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
      FreeAndNil(outHeaders);
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
                outHeaders.Text := UTF8Encode(rawHeaderStr);
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
                outLocation := UTF8Encode(WideString(locBuf));
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
      // Every failure path leaves via Exit(false), which would skip any
      // cleanup placed after this block — free the header list here so
      // failed attempts don't leak it. On success the caller owns it.
      if not Result then
        FreeAndNil(outHeaders);
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
    address := address + '?' + params[0];
    for j := 1 to High(params) do
      address := address + '&' + params[j];
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

  proxyHost  := Trim(GetRootSetting('proxy.host', ''));
  proxyPortS := Trim(GetRootSetting('proxy.port', ''));
  proxyPort  := StrToIntDef(proxyPortS, 8080);
  proxyUser  := GetRootSetting('proxy.user', '');
  proxyPass  := GetRootSetting('proxy.pass', '');

  repeat
    startTick := GetTickCount64;
    TrndiDLog(Format('HTTP %s (winhttp): %s', [methodLabel, currentUrl]));

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

    TrndiDLog(Format('HTTP %s (winhttp) status=%d, bytes=%d, redirects=%d, ms=%d',
      [methodLabel, Result.StatusCode, Length(Result.Body), Result.RedirectCount, endTick - startTick]));

    if not followRedirects then
      Break;

    if not ((Result.StatusCode = 301) or (Result.StatusCode = 302) or
            (Result.StatusCode = 303) or (Result.StatusCode = 307) or
            (Result.StatusCode = 308)) then
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

{------------------------------------------------------------------------------
  attention (Windows)
  -------------------
  Show a desktop toast using PowerShell + WinRT Windows.UI.Notifications.
  The XML is built via the DOM API so user-supplied title/message do not have
  to be XML-escaped manually. The app icon is rendered as the appLogoOverride.
  On any error we write the exception to a log and silently skip the toast.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.attention(topic, message: string);

  function PSQuote(const S: unicodestring): unicodestring;
  begin
    // PowerShell single-quoted literal; escape embedded single quotes
    // (UnicodeStringReplace: plain StringReplace would round-trip via ansi)
    Result := '''' + UnicodeStringReplace(S, '''', '''''', [rfReplaceAll]) + '''';
  end;

  function GetExePathW: unicodestring;
  var
    Buf: array[0..32767] of WChar;
    Len: DWORD;
  begin
    Len := GetModuleFileNameW(0, @Buf[0], Length(Buf));
    SetString(Result, pwidechar(@Buf[0]), Len);
  end;

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

  // Base name of a path without its extension, kept in UTF-16 throughout so a
  // non-ASCII install path is not mangled by the ansi file-name routines.
  function BaseNameNoExtW(const Path: unicodestring): unicodestring;
  var
    i: integer;
  begin
    Result := Path;
    for i := Length(Result) downto 1 do
      if (Result[i] = '\') or (Result[i] = '/') then
      begin
        Result := Copy(Result, i + 1, Length(Result));
        break;
      end;
    for i := Length(Result) downto 1 do
      if Result[i] = '.' then
      begin
        SetLength(Result, i - 1);
        break;
      end;
  end;

var
  AppPath, TempDir, TempPng, LogPath: unicodestring;
  Script, CommandLine: unicodestring;
  ScriptBytes: ansistring;
  SI: Windows.STARTUPINFOW;
  PI: Windows.PROCESS_INFORMATION;
  Title, Msg: unicodestring;
begin
  Title := unicodestring(topic);
  Msg := unicodestring(message);

  AppPath := GetExePathW;
  TempDir := GetEnvVarW('TEMP');
  if (TempDir <> '') and (TempDir[Length(TempDir)] <> '\') then
    TempDir := TempDir + '\';
  TempPng := TempDir + BaseNameNoExtW(AppPath) + '-toast-logo.png';
  LogPath := TempDir + 'trndi-toast-error.log';

  Script :=
    '$ErrorActionPreference = ''Stop''; ' + '$log = ' + PSQuote(LogPath) +
    '; ' + 'try { ' +
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
    '} ' +
    '[void][Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType=WindowsRuntime]; ' +
    '[void][Windows.UI.Notifications.ToastNotification, Windows.UI.Notifications, ContentType=WindowsRuntime]; ' +
    '[void][Windows.Data.Xml.Dom.XmlDocument, Windows.Data.Xml.Dom.XmlDocument, ContentType=WindowsRuntime]; ' +
    '$xml = New-Object Windows.Data.Xml.Dom.XmlDocument; ' +
    '$toastE = $xml.CreateElement(''toast''); ' +
    '$visualE = $xml.CreateElement(''visual''); ' +
    '$bindingE = $xml.CreateElement(''binding''); ' +
    '$bindingE.SetAttribute(''template'', ''ToastGeneric''); ' +
    '$t1 = $xml.CreateElement(''text''); ' +
    '$t2 = $xml.CreateElement(''text''); ' +
    '[void]$t1.AppendChild($xml.CreateTextNode(' + PSQuote(Title) + ')); ' +
    '[void]$t2.AppendChild($xml.CreateTextNode(' + PSQuote(Msg) + ')); ' +
    '[void]$bindingE.AppendChild($t1); ' +
    '[void]$bindingE.AppendChild($t2); ' +
    'if (Test-Path $png) { ' +
      '$imgE = $xml.CreateElement(''image''); ' +
      '$imgE.SetAttribute(''placement'', ''appLogoOverride''); ' +
      '$imgE.SetAttribute(''hint-crop'', ''circle''); ' +
      '$imgE.SetAttribute(''src'', $png); ' +
      '[void]$bindingE.AppendChild($imgE); ' +
    '} ' +
    '[void]$visualE.AppendChild($bindingE); ' +
    '[void]$toastE.AppendChild($visualE); ' +
    '[void]$xml.AppendChild($toastE); ' +
    '$appId = ''{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}\WindowsPowerShell\v1.0\powershell.exe''; ' +
    '$tn = [Windows.UI.Notifications.ToastNotification]::new($xml); ' +
    '[Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier($appId).Show($tn); ' +
    '} catch { ' +
    'try { $_ | Out-String | Set-Content -Path $log -Encoding UTF8 } catch {} ' +
    '}';

  // Pass the script via -EncodedCommand (Base64 of UTF-16LE) instead of
  // -Command "...": there is no outer quoting layer, so a '"' in the toast
  // title/message can neither break argument parsing nor inject PowerShell
  // tokens into the command line.
  SetLength(ScriptBytes, Length(Script) * SizeOf(widechar));
  if ScriptBytes <> '' then
    Move(Script[1], ScriptBytes[1], Length(ScriptBytes));
  CommandLine := 'powershell.exe -NoProfile -ExecutionPolicy Bypass -EncodedCommand ' +
    unicodestring(EncodeStringBase64(ScriptBytes));

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

{------------------------------------------------------------------------------
  DetectTouchScreen (Windows)
  ---------------------------
  Read SM_DIGITIZER and check NID_INTEGRATED_TOUCH + NID_READY for "has touch",
  NID_MULTI_INPUT for multi-touch capability.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.DetectTouchScreen(out multi: boolean): boolean;
const
  NID_INTEGRATED_TOUCH = $00000001;
  NID_MULTI_INPUT      = $00000040;
  NID_READY            = $00000080;
var
  Value: integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
  Result := ((Value and NID_INTEGRATED_TOUCH) <> 0) and
            ((Value and NID_READY) <> 0);
  multi  := (Value and NID_MULTI_INPUT) <> 0;
end;

{------------------------------------------------------------------------------
  PlaySound (Windows)
  -------------------
  Play a validated audio file in-process via MCI (winmm). Windows decodes
  .wav/.mp3/.wma natively; playback is asynchronous. A single alias is
  reused, so a new sound replaces any still-playing one.
 ------------------------------------------------------------------------------}
function mciSendStringW(lpszCommand: PWideChar; lpszReturnString: PWideChar;
  cchReturn: UINT; hwndCallback: HWND): DWORD; stdcall;
  external 'winmm.dll' name 'mciSendStringW';

class procedure TTrndiNativeWindows.PlaySound(const FileName: string);
begin
  if not IsValidAudioFile(FileName) then
    Exit;
  // Close any previous sound on the shared alias (harmless if none is open)
  mciSendStringW('close trndisnd', nil, 0, 0);
  if mciSendStringW(PWideChar(UnicodeString(
       'open "' + FileName + '" alias trndisnd')), nil, 0, 0) = 0 then
    mciSendStringW('play trndisnd', nil, 0, 0);
end;

{------------------------------------------------------------------------------
  GetOSLanguage (Windows)
  -----------------------
  Return the ISO 639 language code via the Win32 locale API.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.GetOSLanguage: string;
var
  wbuf: array[0..9] of WChar;
begin
  if GetLocaleInfoW(LOCALE_USER_DEFAULT, LOCALE_SISO639LANGNAME,
       wbuf, Length(wbuf)) > 0 then
    Result := UTF8Encode(widestring(wbuf))
  else
    Result := '';
end;

{------------------------------------------------------------------------------
  HasDangerousChars (Windows)
  ---------------------------
  Same as the base set but drops '\' since it is the Windows path separator.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.HasDangerousChars(const FileName: string): boolean;
const
  DangerousChars: TSysCharSet =
    ['&', '|', ';', '`', '$', '(', ')', '<', '>', '"', ''''];
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

{------------------------------------------------------------------------------
  Wake-from-sleep notification (Windows)
  --------------------------------------
  Hook the main form's WndProc to catch WM_POWERBROADCAST and additionally
  register for suspend/resume notifications on Windows 8+ so the message is
  reliably delivered. Only one process-wide hook is supported — a second
  Register call replaces the first.
 ------------------------------------------------------------------------------}
const
  WM_POWERBROADCAST_CONST = $0218;
  PBT_APMSUSPEND          = $0004;
  PBT_APMRESUMESUSPEND    = $0007;
  PBT_APMRESUMEAUTOMATIC  = $0012;
  DEVICE_NOTIFY_WINDOW_HANDLE = $00000000;

function RegisterSuspendResumeNotification(hRecipient: THandle;
  Flags: DWORD): THandle; stdcall; external 'user32.dll' name 'RegisterSuspendResumeNotification';
function UnregisterSuspendResumeNotification(Handle: THandle): BOOL; stdcall;
  external 'user32.dll' name 'UnregisterSuspendResumeNotification';

type
  // Tiny bridge object so we can hand Application.QueueAsyncCall a real
  // method-of-object pointer from the global WndProc.
  TWakeBridge = class
    Callback: TTrndiWakeCallback;
    Pending: boolean;
    procedure Fire(Data: PtrInt);
  end;

procedure TWakeBridge.Fire(Data: PtrInt);
begin
  Pending := false;
  if Assigned(Callback) then
    try
      Callback();
    except
      // Never let a callback exception unwind into the message loop
    end;
end;

var
  gWakeBridge: TWakeBridge = nil;
  gOldWndProc: PtrInt = 0;
  gHookedHWnd: HWND = 0;
  gPowerNotify: THandle = 0;

function WakeHookWndProc(hWnd: HWND; uMsg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if (uMsg = WM_POWERBROADCAST_CONST) and
     ((wParam = PBT_APMRESUMESUSPEND) or (wParam = PBT_APMRESUMEAUTOMATIC)) then
  begin
    // Coalesce: Windows may deliver both RESUMEAUTOMATIC and RESUMESUSPEND.
    // Re-arm only after the previous async fire completes.
    if Assigned(gWakeBridge) and (not gWakeBridge.Pending) then
    begin
      gWakeBridge.Pending := true;
      Application.QueueAsyncCall(@gWakeBridge.Fire, 0);
    end;
  end;
  Result := CallWindowProc(Windows.WNDPROC(gOldWndProc), hWnd, uMsg, wParam, lParam);
end;

procedure UnhookWakeWindow;
begin
  if (gHookedHWnd <> 0) and (gOldWndProc <> 0) then
  begin
    SetWindowLongPtr(gHookedHWnd, GWL_WNDPROC, gOldWndProc);
    gOldWndProc := 0;
  end;
  if gPowerNotify <> 0 then
  begin
    try
      UnregisterSuspendResumeNotification(gPowerNotify);
    except
      // API may not exist on pre-Win8; ignore
    end;
    gPowerNotify := 0;
  end;
  gHookedHWnd := 0;
end;

procedure TTrndiNativeWindows.RegisterWakeCallback(const Callback: TTrndiWakeCallback);
var
  targetHWnd: HWND;
begin
  inherited RegisterWakeCallback(Callback);
  if gWakeBridge = nil then
    gWakeBridge := TWakeBridge.Create;
  gWakeBridge.Callback := Callback;
  if not Assigned(Callback) then
  begin
    UnhookWakeWindow;
    Exit;
  end;
  if (Application = nil) or (Application.MainForm = nil)
     or (not Application.MainForm.HandleAllocated) then
  begin
    // No window yet — store the callback; caller should re-register after
    // the main form's handle is allocated (umain.pp does this in FormShow).
    Exit;
  end;
  targetHWnd := Application.MainForm.Handle;
  if gHookedHWnd <> 0 then
  begin
    if gHookedHWnd = targetHWnd then
      Exit; // already hooked the same window
    UnhookWakeWindow;
  end;
  gHookedHWnd := targetHWnd;
  gOldWndProc := SetWindowLongPtr(targetHWnd, GWL_WNDPROC,
    PtrInt(@WakeHookWndProc));
  // Best-effort: register for suspend/resume notifications on Win8+ so
  // the WM_POWERBROADCAST is delivered reliably even when no other app
  // listens. Silently no-op on older Windows where the API is absent.
  if gPowerNotify = 0 then
    try
      gPowerNotify := RegisterSuspendResumeNotification(targetHWnd,
        DEVICE_NOTIFY_WINDOW_HANDLE);
    except
      gPowerNotify := 0;
    end;
end;

procedure TTrndiNativeWindows.UnregisterWakeCallback;
begin
  UnhookWakeWindow;
  if Assigned(gWakeBridge) then
    gWakeBridge.Callback := nil;
  inherited UnregisterWakeCallback;
end;

finalization
  try
    // Ensure the background speech worker is cleanly stopped on shutdown so
    // COM/SAPI resources are released and the thread is joined.
    StopSpeechWorker;
  except
    // Swallow exceptions during finalization to avoid raising at process exit
  end;
  try
    UnhookWakeWindow;
  except
  end;
  FreeAndNil(gWakeBridge);
  FreeAndNil(gOriginalAppIcon);
  if gLastBadgeIcon <> 0 then
  begin
    DestroyIcon(gLastBadgeIcon);
    gLastBadgeIcon := 0;
  end;

end.
