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
Forms, variants, trndi.native.base, ExtCtrls, IniFiles, trndi.log,
slicke.wintools.dwm, slicke.wintools.menutheme;

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
  FKeepAwakeActive: boolean; // True while a SetKeepAwake(true) request stands
  procedure FlashTimerTick(Sender: TObject);
    {** Show the "no speech engine" message, at most once per session. }
  procedure ReportSpeechFailure;
public
  destructor Destroy; override;
    {** Speaks @param(Text) using SAPI on a background thread, honoring the
        @code(tts.voice.name) and @code(tts.rate) settings. With no voice
        configured, the first voice speaking the UI language is used; failing
        that, the engine's default voice. }
  procedure Speak(const Text: string); override;
    {** Toggles immersive dark mode for @param(win).
        Requires Windows 10 1809+ (build >= 17763).
        @returns(True if the DWM call succeeds) }
  {** Secure random bytes via RtlGenRandom (advapi32 SystemFunction036). }
  class function GetRandomBytes(Buf: PByte; Count: integer): boolean; override;
  {** Microsecond monotonic clock via QueryPerformanceCounter. }
  class function MonotonicMicroseconds: int64; override;
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
    {** @true — the multi-user name is shown as a layered title-bar badge. }
  class function SupportsUserBadge: boolean; override;
    {** Create/refresh a clickable layered badge on the title bar. See base. }
  function ShowUserBadge(const nick: string; bg, textColor: TColor;
    const onClick: TTrndiWakeCallback): boolean; override;
    {** Destroy the layered title-bar badge if present. }
  procedure HideUserBadge; override;
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

  {** Keep the system and display awake via SetThreadExecutionState.
      ES_CONTINUOUS makes the request stick until explicitly cleared, so
      disabling (and Destroy) resets to plain ES_CONTINUOUS. }
  procedure SetKeepAwake(Enable: boolean); override;

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
  {** Simple HTTP GET/POST using WinHTTP. A proxy.* root setting is used
      exclusively; without one WinHTTP follows the system configuration. }
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
  procedure attention(topic, message: string); override; overload;
  {** Detect an integrated or external touchscreen via SM_DIGITIZER.
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

resourcestring
RS_TTS_UNAVAILABLE =
  'Text-to-speech is not available: no SAPI speech engine could be started. Install a speech voice from Windows Settings to enable spoken readings.';

const
  // ISpVoice::Speak flags (sapi.h)
  SVSFDefault = 0;
  SVSFlagsAsync = 1;
  SVSFPurgeBeforeSpeak = 2;

  // Win32 primary language ids (the low 10 bits of an LCID). Declared here so
  // the mapping does not depend on which of these the RTL's Windows unit
  // happens to export.
  LANG_DANISH = $06;
  LANG_GERMAN = $07;
  LANG_ENGLISH = $09;
  LANG_FRENCH = $0C;
  LANG_NORWEGIAN = $14;
  LANG_SWEDISH = $1D;

  // Longest backlog the speech queue may hold. Utterances are glucose
  // readings, so a deep queue only reads out values that stopped being true
  // minutes ago; past this point the oldest entry is dropped. The cap also
  // bounds the queue when the worker has died and nothing drains it.
  MaxSpeechQueue = 8;

type
  {** Tri-state cache for @link(TTrndiNativeWindows.SpeakAvailable). }
  TSpeakAvail = (sapUnknown, sapNo, sapYes);

  {** One queued utterance together with the voice settings it should be
      spoken with. Settings are resolved by the caller because only it can
      reach the per-user settings store, and travel with the text. }
  TSpeechRequest = record
    Text: string;
    VoiceName: string;      // exact SAPI description; '' selects by LangTag
    LangTag: string;        // ISO 639 code of the UI language; '' = engine default
    Rate: integer;          // already scaled to SAPI's -10..10
  end;

  {** Background worker that owns a SAPI.SpVoice in an STA thread and
      processes a queue of utterances. }
  TSpeechWorker = class(TThread)
  private
    FQueue: array of TSpeechRequest;
    FCS: TCriticalSection;
    FEvent: TEvent;
    FVoice: olevariant;
    // Token SAPI had selected before Trndi touched it, restored whenever a
    // request asks for no particular voice.
    FDefaultVoice: olevariant;
    // Voice/language pair currently applied to FVoice, so repeat utterances
    // with unchanged settings skip the slow token enumeration.
    FActiveKey: string;
    FActiveRate: integer;
    procedure ApplyVoice(const Req: TSpeechRequest);
    procedure SpeakRequest(const Req: TSpeechRequest);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Req: TSpeechRequest);
    procedure Execute; override;
  end;

var
  gSpeechWorker: TSpeechWorker = nil;
  // Set by the worker when SAPI could not be brought up at all. Read from the
  // UI thread so the failure is reported once instead of the app silently
  // queueing utterances nobody will ever speak.
  gSpeechBroken: boolean = false;
  // Cached SpeakAvailable answer; creating an SpVoice just to ask is slow.
  gSpeakAvailable: TSpeakAvail = sapUnknown;
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
    // Utterances are spoken asynchronously and polled at 100 ms, so this
    // returns promptly even when the worker was mid-sentence.
    gSpeechWorker.WaitFor;
    FreeAndNil(gSpeechWorker);
  end;
end;

procedure EnqueueSpeech(const Req: TSpeechRequest);
begin
  EnsureSpeechWorker;
  if Assigned(gSpeechWorker) then
    gSpeechWorker.Enqueue(Req);
end;

{------------------------------------------------------------------------------
  PrimaryLangId
  -------------
  Map an ISO 639 code (the form Trndi stores in the 'locale' setting) onto the
  Win32 primary language id carried in the low 10 bits of every LCID. Only the
  languages Trndi ships translations for need an entry; anything else returns 0
  and leaves the engine's own default voice in place.
 ------------------------------------------------------------------------------}
function PrimaryLangId(const Tag: string): word;
var
  code: string;
begin
  code := LowerCase(Copy(Tag, 1, 2));
  if code = 'en' then
    Result := LANG_ENGLISH
  else
  if code = 'sv' then
    Result := LANG_SWEDISH
  else
  if code = 'da' then
    Result := LANG_DANISH
  else
  if code = 'de' then
    Result := LANG_GERMAN
  else
  if (code = 'nb') or (code = 'nn') or (code = 'no') then
    Result := LANG_NORWEGIAN
  else
  if code = 'fr' then
    Result := LANG_FRENCH
  else
  // No engine anywhere speaks Jämtlandic; Swedish is the nearest match and a
  // great deal closer than reading jamska with an English voice.
  if code = 'jm' then
    Result := LANG_SWEDISH
  else
    Result := 0;
end;

{------------------------------------------------------------------------------
  VoiceSpeaksLang
  ---------------
  Test a SAPI voice token against a primary language id. The token's 'Language'
  attribute is a semicolon separated list of hex LCIDs (e.g. '41d;406'), so
  every entry is masked down to its primary id before comparing — that way
  sv-FI matches sv-SE.
 ------------------------------------------------------------------------------}
function VoiceSpeaksLang(const Token: olevariant; LangId: word): boolean;
var
  attr: string;
  i, lcid: integer;
begin
  Result := false;
  if LangId = 0 then
    Exit;
  try
    attr := VarToStr(Token.GetAttribute('Language'));
  except
    // Tokens are free to omit the attribute; such a voice simply never matches
    Exit;
  end;
  for i := 1 to WordCount(attr, [';']) do
    if TryStrToInt('$' + Trim(ExtractWord(i, attr, [';'])), lcid) and
      ((lcid and $3FF) = LangId) then
      Exit(true);
end;

{ TSpeechWorker }

constructor TSpeechWorker.Create;
begin
  // Created suspended: Execute dereferences FCS and FEvent straight away, so
  // the thread must not be running before they exist.
  inherited Create(true);
  FreeOnTerminate := False;
  FCS := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  FActiveRate := MaxInt;    // force the first request to push a rate
  Start;
end;

destructor TSpeechWorker.Destroy;
begin
  FEvent.Free;
  FCS.Free;
  inherited Destroy;
end;

procedure TSpeechWorker.Enqueue(const Req: TSpeechRequest);
begin
  FCS.Enter;
  try
    // Drop from the front when full: of a backlog of glucose announcements the
    // newest is the one actually worth hearing.
    while Length(FQueue) >= MaxSpeechQueue do
      Delete(FQueue, 0, 1);
    SetLength(FQueue, Length(FQueue) + 1);
    FQueue[High(FQueue)] := Req;
  finally
    FCS.Leave;
  end;
  FEvent.SetEvent;
end;

{------------------------------------------------------------------------------
  TSpeechWorker.ApplyVoice
  ------------------------
  Point FVoice at the voice this request asks for and set its rate. An explicit
  voice name wins; with none, the first voice speaking the UI language is used;
  failing both, SAPI's own default is restored. Enumerating tokens is slow, so
  the resolved selection is cached and only redone when the request changes.
 ------------------------------------------------------------------------------}
procedure TSpeechWorker.ApplyVoice(const Req: TSpeechRequest);
var
  Voices, Token: olevariant;
  i, chosen: integer;
  wanted: word;
  key, desc: string;
begin
  if Req.Rate <> FActiveRate then
  try
    FVoice.Rate := Req.Rate;
    FActiveRate := Req.Rate;
  except
    // Rate is advisory — keep speaking at whatever the engine allows
  end;

  key := Req.VoiceName + #1 + Req.LangTag;
  if key = FActiveKey then
    Exit;
  // Set before resolving: a request that finds no match must not re-enumerate
  // every single utterance.
  FActiveKey := key;

  wanted := 0;
  if Req.VoiceName = '' then
    wanted := PrimaryLangId(Req.LangTag);

  try
    if (Req.VoiceName = '') and (wanted = 0) then
    begin
      // Nothing to match on. Restoring rather than leaving the previous token
      // matters when the user switches the setting back to 'Default'.
      if not VarIsEmpty(FDefaultVoice) then
        FVoice.Voice := FDefaultVoice;
      Exit;
    end;

    Voices := FVoice.GetVoices('', '');
    if VarIsEmpty(Voices) then
      Exit;

    chosen := -1;
    for i := 0 to Voices.Count - 1 do
    begin
      Token := Voices.Item(i);
      if Req.VoiceName <> '' then
      begin
        desc := '';
        try
          desc := VarToStr(Token.GetDescription(0));
        except
          // Unreadable description; it can never be the one that was picked
        end;
        if SameText(desc, Req.VoiceName) then
        begin
          chosen := i;
          Break;
        end;
      end
      else
      if VoiceSpeaksLang(Token, wanted) then
      begin
        chosen := i;
        Break;
      end;
    end;

    // No match means either a named voice that has since been uninstalled or
    // no voice for the UI language at all. SAPI's default covers both.
    if chosen >= 0 then
      FVoice.Voice := Voices.Item(chosen)
    else
    if not VarIsEmpty(FDefaultVoice) then
      FVoice.Voice := FDefaultVoice;
  except
    // Selection failed; the currently active voice still speaks
  end;
end;

{------------------------------------------------------------------------------
  TSpeechWorker.SpeakRequest
  --------------------------
  Speak one utterance. The call is asynchronous and polled rather than
  synchronous so Terminate is noticed mid-sentence — a blocking Speak would
  hold up application shutdown for the rest of the utterance.
 ------------------------------------------------------------------------------}
procedure TSpeechWorker.SpeakRequest(const Req: TSpeechRequest);
begin
  ApplyVoice(Req);

  FVoice.Speak(Req.Text, SVSFlagsAsync);
  while not Terminated do
    if FVoice.WaitUntilDone(100) then
      Exit;

  // Asked to stop while still talking: cut the utterance off.
  FVoice.Speak('', SVSFPurgeBeforeSpeak or SVSFlagsAsync);
end;

procedure TSpeechWorker.Execute;
var
  hr: HRESULT;
  comOwned: boolean;
  req: TSpeechRequest;
  hasItem: boolean;
begin
  // SAPI objects are apartment-threaded, so this thread must be an STA.
  // RPC_E_CHANGED_MODE says COM was already up here in another mode; in that
  // case the balancing CoUninitialize is not ours to make.
  hr := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  comOwned := Succeeded(hr);
  try
    try
      FVoice := CreateOleObject('SAPI.SpVoice');
    except
      // No speech engine. Flag it so the UI can say so once, and stop —
      // draining a queue we cannot speak would only burn cycles.
      gSpeechBroken := true;
      Exit;
    end;

    // Remember the engine's own choice so ApplyVoice can put it back.
    try
      FDefaultVoice := FVoice.Voice;
    except
      FDefaultVoice := Unassigned;
    end;

    while not Terminated do
    begin
      // Wait for work, waking periodically to re-check termination
      FEvent.WaitFor(500);
      hasItem := false;
      FCS.Enter;
      try
        if Length(FQueue) > 0 then
        begin
          req := FQueue[0];
          Delete(FQueue, 0, 1);
          hasItem := true;
        end;
      finally
        FCS.Leave;
      end;

      if hasItem then
      try
        // One utterance at a time, so queued announcements stay serialized
        SpeakRequest(req);
      except
        // A failed utterance must never take the worker down with it
      end;
    end;

  finally
    // Clean up COM/voice
    try
      FDefaultVoice := Unassigned;
      FVoice := Unassigned;
    except
    end;
    if comOwned then
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
  Check if SAPI is available by attempting to create the SpVoice object, once
  per process.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SpeakAvailable: boolean;
var
  Voice: olevariant;
begin
  // Creating an SpVoice costs a COM activation plus SAPI engine startup, and
  // this is called on UI paths, so the answer is resolved once per process.
  if gSpeakAvailable = sapUnknown then
  begin
    try
      Voice := CreateOleObject('SAPI.SpVoice');
      gSpeakAvailable := sapYes;
    except
      // SAPI not available
      gSpeakAvailable := sapNo;
    end;
    Voice := Unassigned;
  end;
  Result := gSpeakAvailable = sapYes;
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
  if FKeepAwakeActive then
    SetKeepAwake(false);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  SetKeepAwake
  ------------
  SetThreadExecutionState with ES_CONTINUOUS keeps the request active for this
  thread until it is explicitly replaced, so enabling is a single call and
  disabling resets to plain ES_CONTINUOUS. FPC 3.2.2's Windows unit does not
  declare the import, hence the local external. The flags are thread-scoped;
  both calls run on the main thread (kiosk toggling and Destroy).
------------------------------------------------------------------------------}
const
ES_CONTINUOUS = DWORD($80000000);
ES_SYSTEM_REQUIRED = DWORD($00000001);
ES_DISPLAY_REQUIRED = DWORD($00000002);

function WinSetThreadExecutionState(esFlags: DWORD): DWORD; stdcall;
  external 'kernel32.dll' name 'SetThreadExecutionState';

procedure TTrndiNativeWindows.SetKeepAwake(Enable: boolean);
begin
  if Enable then
    WinSetThreadExecutionState(ES_CONTINUOUS or ES_SYSTEM_REQUIRED or
      ES_DISPLAY_REQUIRED)
  else
    WinSetThreadExecutionState(ES_CONTINUOUS);
  FKeepAwakeActive := Enable;
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
// DWM attribute externals + DWMWA_* ids + SetDwmAttr/HrSucceeded live in
// slicke.wintools.dwm (shared with slicke.wintools.menutheme). UpdateLayeredWindow
// is a user32 layered-window call used only by the badge painter, so it stays here.
function UpdateLayeredWindow(hWnd: HWND; hdcDst: HDC; pptDst: Pointer;
  psize: Pointer; hdcSrc: HDC; pptSrc: Pointer; crKey: DWORD;
  pblend: Pointer; dwFlags: DWORD): BOOL; stdcall; external 'user32.dll';
{$endif}

{------------------------------------------------------------------------------
  Speak
  -----
  Resolve the configured voice and rate, then hand the utterance to the
  background worker. Settings are read here rather than in the worker because
  only the caller knows which user's settings are in scope.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.Speak(const Text: string);
var
  req: TSpeechRequest;
  Voice: olevariant;
  lang: string;
begin
  if Text = '' then
    Exit;

  if not SpeakAvailable then
  begin
    ReportSpeechFailure;
    Exit;
  end;

  req.Text := Text;
  req.VoiceName := GetSetting('tts.voice.name', '');
  // The setting holds the shared -100..100 scale (what spd-say takes on the
  // Unix side); SAPI's Rate property is -10..10.
  req.Rate := Round(GetIntSetting('tts.rate', 0) / 10);
  if req.Rate < -10 then
    req.Rate := -10
  else
  if req.Rate > 10 then
    req.Rate := 10;
  // Match the voice to the *UI* language, not the system one: what gets spoken
  // is Trndi's own translated strings.
  lang := GetSetting('locale', '');
  if (lang = '') or (LowerCase(lang) = 'auto') then
    lang := GetOSLanguage;
  req.LangTag := lang;

  // Enqueue for the background worker; if that fails, fall back to
  // a synchronous speak so the user still hears audio.
  try
    EnqueueSpeech(req);
    // The worker brings SAPI up on its own thread, so a failure there only
    // becomes visible after the fact.
    if gSpeechBroken then
      ReportSpeechFailure;
    Exit;
  except
    // Worker thread could not be started; fall through
  end;

  try
    try
      Voice := CreateOleObject('SAPI.SpVoice');
      // Deliberately synchronous, unlike the worker: releasing the object on
      // return would cut off an asynchronous utterance mid-word. This blocks
      // the caller, but only ever runs when a thread could not be created.
      Voice.Speak(Text, SVSFDefault);
    except
      // Ignore fallback failures; avoid crashing the caller
    end;
  finally
    Voice := Unassigned;
  end;
end;

{------------------------------------------------------------------------------
  ReportSpeechFailure
  -------------------
  Tell the user once per session that speech is dead. Without this a broken
  SAPI install is completely silent: announcements are enabled, nothing is
  spoken, and nothing explains why.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.ReportSpeechFailure;
begin
  if ttsErrorShown then
    Exit;
  ttsErrorShown := true;
  ShowMessage(RS_TTS_UNAVAILABLE);
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

  function PerformRequest(withProxy: boolean): boolean;
  begin
    Result := false;
    if withProxy and (proxyHost <> '') then
    begin
      if (proxyUser <> '') or (proxyPass <> '') then
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080), proxyUser, proxyPass)
      else
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080));
    end
    else
      client := TWinHTTPClient.Create(DEFAULT_USER_AGENT);

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

    // A configured proxy is the only route out: no direct fallback, or a
    // dead proxy would silently send the traffic around it.
    if proxyHost <> '' then
    begin
      {$ifdef DEBUG}
      TrndiDLog(Format('HTTP GET: attempting via proxy %s:%s', [proxyHost, proxyPort]));
      {$endif}
      Result := PerformRequest(true);
      {$ifdef DEBUG}
      if Result then
        TrndiNetLog('HTTP GET: proxy attempt succeeded')
      else
        TrndiNetLog('HTTP GET: proxy attempt failed: ' + res);
      {$endif}
      Exit;
    end;

    // Nothing configured: let WinHTTP use the system proxy configuration.
    {$ifdef DEBUG}
    TrndiNetLog('HTTP GET: attempting via system configuration');
    {$endif}
    Result := PerformRequest(false);
    {$ifdef DEBUG}
    if not Result then
      TrndiNetLog('HTTP GET: attempt failed: ' + res);
    {$endif}

  finally
    tempInstance.Free;
  end;
end;

{------------------------------------------------------------------------------
  postURL
  -------
  Simple HTTP POST using WinHTTP client. Mirrors getURL: a configured proxy is
  used exclusively, otherwise WinHTTP follows the system configuration.
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

  function PerformRequest(withProxy: boolean): boolean;
  begin
    Result := false;
    if withProxy and (proxyHost <> '') then
    begin
      if (proxyUser <> '') or (proxyPass <> '') then
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080), proxyUser, proxyPass)
      else
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080));
    end
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

    // Strict: a configured proxy is never bypassed (mirrors getURL).
    if proxyHost <> '' then
      Result := PerformRequest(true)
    else
      Result := PerformRequest(false);
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
begin
  // Immersive dark mode lives in slicke.wintools.menutheme so the popup-menu
  // hook can share it without a native<->slicke dependency cycle.
  Result := WinApplyImmersiveDark(win, Enable);
end;

class function TTrndiNativeWindows.SetPreferredDarkMode: boolean;
begin
  // Popup-menu dark theming (uxtheme opt-in + CBT hook + frame subclass) lives
  // in slicke.wintools.menutheme; this stays a class method for API stability.
  Result := EnableDarkPopupMenus;
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
  TREND_SUPERSAMPLE = 4;
var
  AppIcon, TempIcon: TIcon;
  Bitmap, TrendMask: Graphics.TBitmap;
  BadgeText: string;
  TextWidth, TextHeight: integer;
  BadgeRect, TrendRect: Classes.TRect;
  IconWidth, IconHeight, BadgeSize, TrendSize: integer;
  FontSize, Radius, TrendRadius, py: integer;
  SmallIconSize: integer;
  TextColor: TColor;
  borderColor: TColor;
  ShellIcon: HICON;
  hMain: HWND;

  // Application.MainForm is nil before the main form is built and again during
  // teardown, while queued WM_TIMER ticks can still reach FlashTimerTick and
  // land here. Reading .Handle directly would also force handle creation, so
  // every window-dependent step goes through this.
function MainFormHandle: HWND;
  begin
    if (Application <> nil) and (Application.MainForm <> nil) and
      Application.MainForm.HandleAllocated then
      Result := Application.MainForm.Handle
    else
      Result := 0;
  end;

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

  // Render the trend as a solid arrowhead instead of the source glyph. At badge
  // size a shafted character ("→", "↑") loses almost all of its ink to the thin
  // stem, so the head — the part that carries the direction — ends up only a
  // couple of pixels wide. A bare triangle spends the whole area on the head and
  // stays readable when the taskbar scales the icon down.
  //
  // The mask is drawn TREND_SUPERSAMPLE times oversized and in black/white only;
  // BlendTrendMask averages it down into coverage, which antialiases edges that
  // GDI would otherwise leave jagged at these sizes. Returns False for trends
  // with no arrow form ("?", "X") — those keep the text path.
function BuildTrendMask(const Trend: string; ASize: integer;
  out AMask: Graphics.TBitmap): boolean;
  const
    // UTF-8 byte sequences for the arrows in BG_TREND_ARROWS_UTF, spelled out so
    // the match never depends on this unit's source encoding.
    ARR_UP = #$E2#$86#$91;                 // U+2191
    ARR_UPRIGHT = #$E2#$86#$97;            // U+2197
    ARR_RIGHT = #$E2#$86#$92;              // U+2192
    ARR_DOWNRIGHT = #$E2#$86#$98;          // U+2198
    ARR_DOWN = #$E2#$86#$93;               // U+2193
    DIAG = 0.7071;
  var
    dx, dy, cx, cy, span, len, back, wid, b: double;
    offs: array[0..1] of double;
    pts: array[0..2] of TPoint;
    i, n, dim: integer;
  begin
    Result := false;
    AMask := nil;
    dx := 0;
    dy := 0;
    n := 1;
    // n = 2 marks the doubled trends, drawn as two stacked heads.
    if Trend = ARR_UP + ARR_UP then
    begin
      dy := -1;
      n := 2;
    end
    else if Trend = ARR_DOWN + ARR_DOWN then
    begin
      dy := 1;
      n := 2;
    end
    else if Trend = ARR_UP then
      dy := -1
    else if Trend = ARR_DOWN then
      dy := 1
    else if Trend = ARR_RIGHT then
      dx := 1
    else if Trend = ARR_UPRIGHT then
    begin
      dx := DIAG;
      dy := -DIAG;
    end
    else if Trend = ARR_DOWNRIGHT then
    begin
      dx := DIAG;
      dy := DIAG;
    end
    else
      Exit;

    dim := ASize * TREND_SUPERSAMPLE;
    if dim < TREND_SUPERSAMPLE then
      Exit;

    span := dim;
    cx := dim / 2;
    cy := dim / 2;
    // Proportions matter more than they look: the head must be longer than it
    // is wide, or the two base corners end up sharper than the tip and the eye
    // reads the arrow as pointing at a corner instead — badly wrong for the
    // 45-degree trends, where the corners land on the cardinal axes.
    if n = 2 then
    begin
      // Two heads stacked along the direction, spanning -0.34..+0.34 of the
      // badge with a gap between them so they don't merge into one blob.
      len := 0.21 * span;
      back := 0.10 * span;
      wid := 0.24 * span;
      offs[0] := 0.12 * span;
      offs[1] := -0.24 * span;
    end
    else
    begin
      len := 0.34 * span;
      back := 0.21 * span;
      wid := 0.26 * span;
      offs[0] := 0;
    end;

    // pf32bit like the icon bitmap, so the blend below can assume 4-byte pixels
    // rather than depend on how the widgetset expands a 24-bit DIB.
    AMask := Graphics.TBitmap.Create;
    AMask.PixelFormat := pf32bit;
    AMask.SetSize(dim, dim);
    AMask.Canvas.Brush.Style := bsSolid;
    AMask.Canvas.Brush.Color := clBlack;
    AMask.Canvas.FillRect(0, 0, dim, dim);
    AMask.Canvas.Brush.Color := clWhite;
    AMask.Canvas.Pen.Color := clWhite;

    for i := 0 to n - 1 do
    begin
      b := offs[i] - back;
      // Tip on the direction axis, base corners offset along the perpendicular
      // (-dy, dx).
      pts[0].X := Round(cx + dx * (offs[i] + len));
      pts[0].Y := Round(cy + dy * (offs[i] + len));
      pts[1].X := Round(cx + dx * b - dy * wid);
      pts[1].Y := Round(cy + dy * b + dx * wid);
      pts[2].X := Round(cx + dx * b + dy * wid);
      pts[2].Y := Round(cy + dy * b - dx * wid);
      AMask.Canvas.Polygon(pts);
    end;

    Result := true;
  end;

  // Composite the oversampled arrow mask onto the icon bitmap. Runs after all
  // canvas drawing (and after FixBadgeAlpha) because it writes pixels directly;
  // any further canvas op would discard the result. Alpha is left alone — the
  // arrow only covers pixels FixBadgeAlpha has already made opaque.
procedure BlendTrendMask(const R: Classes.TRect; AMask: Graphics.TBitmap;
AColor: TColor);
  var
    px, py, sx, sy, acc, size, rc: integer;
    cov: double;
    dst, src: PByte;
    cr, cg, cb: integer;
  begin
    GdiFlush;
    size := R.Right - R.Left;
    rc := ColorToRGB(AColor);
    cr := GetRValue(rc);
    cg := GetGValue(rc);
    cb := GetBValue(rc);

    for py := 0 to size - 1 do
    begin
      dst := PByte(Bitmap.ScanLine[R.Top + py]);
      for px := 0 to size - 1 do
      begin
        acc := 0;
        for sy := 0 to TREND_SUPERSAMPLE - 1 do
        begin
          src := PByte(AMask.ScanLine[py * TREND_SUPERSAMPLE + sy]);
          for sx := 0 to TREND_SUPERSAMPLE - 1 do
            Inc(acc, (src + (px * TREND_SUPERSAMPLE + sx) * 4)^);
        end;
        if acc = 0 then
          Continue;
        cov := acc / (TREND_SUPERSAMPLE * TREND_SUPERSAMPLE * 255);
        src := dst + (R.Left + px) * 4;
        // BGRA order.
        src^ := Round(src^ + (cb - src^) * cov);
        (src + 1)^ := Round((src + 1)^ + (cg - (src + 1)^) * cov);
        (src + 2)^ := Round((src + 2)^ + (cr - (src + 2)^) * cov);
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
  TrendMask := nil;
  try
    if Value = '' then
    begin
      hMain := MainFormHandle;
      if (gOriginalAppIcon <> nil) and (not gOriginalAppIcon.Empty) then
        Application.Icon.Assign(gOriginalAppIcon)
      else
      if Application.MainForm <> nil then
        Application.Icon.Assign(Application.MainForm.Icon);
      if hMain <> 0 then
      begin
        SendMessage(hMain, WM_SETICON, ICON_BIG, 0);
        SendMessage(hMain, WM_SETICON, ICON_SMALL, 0);
      end;
      // The cached handle is retired regardless of whether a window was still
      // around to notify, so teardown never strands it.
      if gLastBadgeIcon <> 0 then
      begin
        DestroyIcon(gLastBadgeIcon);
        gLastBadgeIcon := 0;
      end;
      Exit;
    end;

    // Printed exactly as the caller formatted it. Forcing one decimal here used
    // to turn an mg/dL reading of "148" into "148.0" — five glyphs in a badge
    // sized for three, so the fitting loop shrank the font for nothing. The
    // caller has already formatted for the active unit.
    BadgeText := Value;

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

      // Arrows are painted as shaft-less arrowheads after the alpha pass; only
      // the non-directional trends ("?", "X") still go out as text.
      if not BuildTrendMask(FBadgeTrend, TrendSize, TrendMask) then
      begin
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
    end;

    FixBadgeAlpha(BadgeRect, Radius);
    if FBadgeTrend <> '' then
      FixBadgeAlpha(TrendRect, TrendRadius);
    if TrendMask <> nil then
      BlendTrendMask(TrendRect, TrendMask, TextColor);

    // Assign the composed bitmap to the app icon and notify the window.
    TempIcon.Assign(Bitmap);
    // Duplicate the icon for the shell while the previously sent handle
    // (gLastBadgeIcon, or the initial Application.Icon.Handle) is still
    // alive — that forces a different numeric HICON value, which Explorer
    // requires before it re-reads the taskbar button icon.
    ShellIcon := CopyIcon(TempIcon.Handle);
    Application.Icon.Assign(TempIcon);
    hMain := MainFormHandle;
    if ShellIcon <> 0 then
    begin
      if hMain <> 0 then
      begin
        SendMessage(hMain, WM_SETICON, ICON_BIG, ShellIcon);
        SendMessage(hMain, WM_SETICON, ICON_SMALL, ShellIcon);
      end;
      // The shell now references the new handle; retire the previous one.
      // Tracked even when no window was notified, so the copy is still freed
      // by the next call rather than leaked.
      if gLastBadgeIcon <> 0 then
        DestroyIcon(gLastBadgeIcon);
      gLastBadgeIcon := ShellIcon;
    end
    else
    if hMain <> 0 then
    begin
      SendMessage(hMain, WM_SETICON, ICON_BIG,
        Application.Icon.Handle);
      SendMessage(hMain, WM_SETICON, ICON_SMALL,
        Application.Icon.Handle);
    end;
  finally
    TrendMask.Free;
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
          // HKCU\Software\Trndi is user-writable, so a value we never wrote
          // (REG_DWORD, REG_BINARY, ...) can turn up here. ReadString raises
          // ERegistryException on those, which would abort the enumeration
          // before SeedSettingsCache and leave the cache permanently cold.
          // Type-check first and skip what we cannot represent; the type query
          // reuses the already-open key, so this stays one open/close pair.
          if reg.GetDataType(k) in [rdString, rdExpandString] then
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

    // A hidden main form is left hidden: a background fetch must never force the
    // window into view (or onto the taskbar). Progress simply goes unshown then.

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
  HTTP GET/POST via WinHTTPClient. Honours proxy.* root settings: a configured
  proxy carries the request with no direct fallback, and with nothing
  configured WinHTTP follows the system proxy configuration.
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
  proxyUser  := GetRootSetting('proxy.user', '');
  proxyPass  := GetRootSetting('proxy.pass', '');
  // The host field holds whatever the user typed ('http://proxy:3128'), so it
  // has to be split the same way getURL and the settings dialog's test button
  // split it — WinHTTP wants a bare host.
  NormalizeProxyHostPort(proxyHost, proxyPortS);
  proxyPort  := StrToIntDef(proxyPortS, 8080);

  // A configured proxy is the only route out — no direct fallback, so a proxy
  // that is down surfaces as an error instead of quietly being bypassed.
  if proxyHost <> '' then
  begin
    if (proxyUser <> '') or (proxyPass <> '') then
      client := TWinHTTPClient.Create(useragent, proxyHost, proxyPort, proxyUser, proxyPass)
    else
      client := TWinHTTPClient.Create(useragent, proxyHost, proxyPort);
    try
      TryRequest(client, ResStr);
      Result := ResStr;
    finally
      client.Free;
    end;
    Exit;
  end;

  // Nothing configured: follow the system's own proxy configuration, which is
  // what every other Windows application does.
  TrndiDLog('Windows: Using system proxy configuration for: ' +
    TrndiSafeUrl(address));
  client := TWinHTTPClient.Create(useragent);
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
  settings the same way @link(request) does: a configured proxy carries every
  hop with no direct fallback, and with nothing configured WinHTTP follows the
  system proxy configuration.
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
    const useProxy: boolean; out outBody: string;
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
  proxyUser  := GetRootSetting('proxy.user', '');
  proxyPass  := GetRootSetting('proxy.pass', '');
  // See request(): the stored host may carry a scheme and/or a port.
  NormalizeProxyHostPort(proxyHost, proxyPortS);
  proxyPort  := StrToIntDef(proxyPortS, 8080);

  repeat
    startTick := GetTickCount64;
    TrndiDLog(Format('HTTP %s (winhttp): %s', [methodLabel, TrndiSafeUrl(currentUrl)]));

    // Configured proxy: no direct fallback on any hop, so a redirect chain
    // cannot start on the proxy and finish around it. Nothing configured: use
    // the system's proxy configuration.
    if not TryRequest(currentUrl, currentPost, bodyData, proxyHost <> '',
         responseBody, responseHeaders, statusCode, locationHeader, Result.ErrorMessage) then
      Exit;

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
    // Log the exception type and message only. Never "$_ | Out-String": an
    // ErrorRecord carries the failing source line, and the script embeds the
    // toast title/message as literals -- that would spill glucose values into
    // a plaintext temp file on every toast failure.
    'try { ($_.Exception.GetType().FullName + '': '' + $_.Exception.Message) | ' +
    'Set-Content -Path $log -Encoding UTF8 } catch {} ' +
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
  Read SM_DIGITIZER and require a ready integrated or external touch device.
  NID_MULTI_INPUT reports multi-touch capability.
 ------------------------------------------------------------------------------}
{------------------------------------------------------------------------------
  GetRandomBytes
  ---------------------------
  RtlGenRandom, exported from advapi32 under its ordinal name
  SystemFunction036. Preferred over the CryptoAPI/CNG entry points because it
  needs no provider handle and has been present since Windows XP.
 ------------------------------------------------------------------------------}
function RtlGenRandom(RandomBuffer: pointer; RandomBufferLength: ULONG): ByteBool;
  stdcall; external 'advapi32.dll' name 'SystemFunction036';

class function TTrndiNativeWindows.GetRandomBytes(Buf: PByte; Count: integer): boolean;
begin
  if (Buf = nil) or (Count <= 0) then
    Exit(false);
  try
    Result := RtlGenRandom(Buf, ULONG(Count));
  except
    Result := false;
  end;
end;

{------------------------------------------------------------------------------
  MonotonicMicroseconds
  ---------------------------
  QueryPerformanceCounter. The counter is scaled in two parts so the
  multiplication cannot overflow on a long-running system.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.MonotonicMicroseconds: int64;
var
  freq, cnt: int64;
begin
  if QueryPerformanceFrequency(freq) and (freq > 0) and QueryPerformanceCounter(cnt) then
    Result := (cnt div freq) * 1000000 + ((cnt mod freq) * 1000000) div freq
  else
    Result := inherited MonotonicMicroseconds;
end;

class function TTrndiNativeWindows.DetectTouchScreen(out multi: boolean): boolean;
const
  NID_INTEGRATED_TOUCH = $00000001;
  NID_EXTERNAL_TOUCH   = $00000002;
  NID_MULTI_INPUT      = $00000040;
  NID_READY            = $00000080;
var
  Value: integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
    Result := ((Value and (NID_INTEGRATED_TOUCH or NID_EXTERNAL_TOUCH)) <> 0) and
            ((Value and NID_READY) <> 0);
  multi := Result and ((Value and NID_MULTI_INPUT) <> 0);
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

{==============================================================================
  Multi-user title-bar badge
  --------------------------
  Renders the active user's nickname as a small clickable "pill" overlaid on
  the top-right of the native title bar, replacing the "[name] Trndi" caption
  prefix used on other platforms. The pill is a layered (per-pixel alpha) popup
  window owned by the main form; the owner's WndProc is subclassed so the pill
  tracks move/resize/maximize and DPI changes. A click opens Settings via the
  supplied callback. Painting reuses the pf32bit-DIB + manual-alpha technique
  from SetBadge, pushed to the window with UpdateLayeredWindow.
 ==============================================================================}
const
  BADGE_CLASS_NAME         = 'TrndiUserBadgeWnd';
  DWMWA_CAPTION_BTN_BOUNDS = 5;      // DWMWA_CAPTION_BUTTON_BOUNDS
  BADGE_GAP                = 8;      // px between the pill and the caption buttons
  WM_DPICHANGED_MSG        = $02E0;
  WM_ENTERSIZEMOVE_MSG     = $0231;
  WM_EXITSIZEMOVE_MSG      = $0232;
  CLEARTYPE_QUAL           = 5;      // CLEARTYPE_QUALITY (absent from FPC's Windows unit)

var
  gBadgeHWnd: HWND = 0;
  gBadgeOwner: HWND = 0;
  gBadgeOwnerOldProc: PtrInt = 0;
  gBadgeClassReg: boolean = false;
  gBadgeBridge: TWakeBridge = nil;
  gBadgeW: integer = 0;
  gBadgeH: integer = 0;
  gBadgeNick: string = '';
  gBadgeBg: TColor = clBlack;
  gBadgeText: TColor = clWhite;

// True when device pixel (px,py) lies inside a w×h rounded rectangle with the
// given corner radius. Mirrors the corner test used by SetBadge.FixBadgeAlpha.
function BadgePixelInside(px, py, w, h, r: integer): boolean;
begin
  Result := true;
  if (px < r) and (py < r) then
    Result := Sqr(px - r) + Sqr(py - r) <= Sqr(r)
  else if (px >= w - r) and (py < r) then
    Result := Sqr(px - (w - r - 1)) + Sqr(py - r) <= Sqr(r)
  else if (px < r) and (py >= h - r) then
    Result := Sqr(px - r) + Sqr(py - (h - r - 1)) <= Sqr(r)
  else if (px >= w - r) and (py >= h - r) then
    Result := Sqr(px - (w - r - 1)) + Sqr(py - (h - r - 1)) <= Sqr(r);
end;

procedure DestroyBadgeWindow;
begin
  if gBadgeHWnd <> 0 then
  begin
    DestroyWindow(gBadgeHWnd);
    gBadgeHWnd := 0;
  end;
end;

// Screen-space caption geometry of the owner: rightX = left edge of the
// caption buttons, [capTop..capBottom] = the caption's vertical extent. Uses
// the DWM caption-button bounds when available (works for normal and tool
// windows), else a style-aware system-metrics fallback. The main form is a
// bsSizeToolWin (tool window) — small caption, single close button.
function BadgeCaptionGeom(out rightX, capTop, capBottom: integer): boolean;
var
  wr, btn: TRect;
  exStyle: PtrInt;
  capH: integer;
begin
  Result := false;
  if (gBadgeOwner = 0) or (not GetWindowRect(gBadgeOwner, wr)) then
    Exit;
  if HrSucceeded(DwmGetWindowAttribute(gBadgeOwner, DWMWA_CAPTION_BTN_BOUNDS,
    @btn, SizeOf(btn))) and (btn.Right > btn.Left) and (btn.Bottom > btn.Top) then
  begin
    // btn is window-relative; convert to screen coordinates.
    rightX    := wr.Left + btn.Left;
    capTop    := wr.Top + btn.Top;
    capBottom := wr.Top + btn.Bottom;
  end
  else
  begin
    exStyle := GetWindowLongPtr(gBadgeOwner, GWL_EXSTYLE);
    if (exStyle and WS_EX_TOOLWINDOW) <> 0 then
    begin
      capH   := GetSystemMetrics(SM_CYSMCAPTION);
      rightX := wr.Right - GetSystemMetrics(SM_CXSIZE) - GetSystemMetrics(SM_CXFRAME);
    end
    else
    begin
      capH   := GetSystemMetrics(SM_CYCAPTION);
      rightX := wr.Right - GetSystemMetrics(SM_CXSIZE) * 3 - GetSystemMetrics(SM_CXFRAME);
    end;
    capTop    := wr.Top + GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CXPADDEDBORDER);
    capBottom := capTop + capH;
  end;
  Result := true;
end;

// Move the pill just left of the caption buttons, vertically centred.
procedure RepositionBadge;
var
  rightX, capTop, capBottom, x, y: integer;
begin
  if (gBadgeHWnd = 0) or (gBadgeOwner = 0) then
    Exit;
  if not BadgeCaptionGeom(rightX, capTop, capBottom) then
    Exit;
  x := rightX - gBadgeW - BADGE_GAP;
  y := capTop + ((capBottom - capTop) - gBadgeH) div 2;
  SetWindowPos(gBadgeHWnd, 0, x, y, 0, 0,
    SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

// Paint the pill into a 32-bit DIB and push it to the layered window. Sizes
// gBadgeW/gBadgeH from the current caption height and the nick text width.
procedure PaintBadge;
const
  PAD_X = 12;
var
  faceName: WideString;
  wnick: UnicodeString;
  hf, oldFont: HGDIOBJ;
  brush, pen, oldBrush, oldPen: HGDIOBJ;
  bi: BITMAPINFO;
  dib: HBITMAP;
  oldDib: HGDIOBJ;
  bits: Pointer;
  memDC, screenDC: HDC;
  txtSz: TSize;
  txtW, txtH, w, h, radius, capH, fontH, yy, xx: integer;
  rightX, capTop, capBottom: integer;
  blend: BLENDFUNCTION;
  ptSrc: TPoint;
  sz: TSize;
  pRow: PByte;
begin
  if gBadgeHWnd = 0 then
    Exit;

  // Pill height derives from the caption height (small for the tool window).
  if BadgeCaptionGeom(rightX, capTop, capBottom) then
    capH := capBottom - capTop
  else
    capH := GetSystemMetrics(SM_CYSMCAPTION);
  h := Max(14, capH - 4);
  fontH := Max(9, h - 6);

  faceName := 'Segoe UI';
  wnick := UTF8Decode(gBadgeNick);

  // Paint into a raw 32-bit top-down DIB section we own outright, NOT an LCL
  // TBitmap. UpdateLayeredWindow needs a clean per-pixel premultiplied surface;
  // mixing LCL Canvas draws with raw ScanLine alpha and then blitting from
  // Canvas.Handle desynced the surface, so the pill body kept alpha=0 and got
  // additively blended over the caption — the washed near-white pill with only
  // the text/border coloured. One DIB drives the GDI draw, the alpha pass, and
  // the blit, so no desync is possible.
  memDC := CreateCompatibleDC(0);
  if memDC = 0 then
    Exit;
  try
    hf := CreateFontW(-fontH, 0, 0, 0, FW_BOLD, 0, 0, 0, DEFAULT_CHARSET,
      OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, CLEARTYPE_QUAL,
      DEFAULT_PITCH or FF_DONTCARE, PWideChar(faceName));
    oldFont := SelectObject(memDC, hf);
    try
      txtSz.cx := 0;
      txtSz.cy := 0;
      GetTextExtentPoint32W(memDC, PWideChar(wnick), Length(wnick), txtSz);
      txtW := txtSz.cx;
      txtH := txtSz.cy;

      w := txtW + PAD_X * 2;
      radius := h div 2;
      gBadgeW := w;
      gBadgeH := h;

      FillChar(bi, SizeOf(bi), 0);
      bi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
      bi.bmiHeader.biWidth := w;
      bi.bmiHeader.biHeight := -h;        // negative => top-down rows
      bi.bmiHeader.biPlanes := 1;
      bi.bmiHeader.biBitCount := 32;
      bi.bmiHeader.biCompression := BI_RGB;
      bits := nil;
      dib := CreateDIBSection(memDC, bi, DIB_RGB_COLORS, bits, 0, 0);
      if (dib = 0) or (bits = nil) then
        Exit;
      oldDib := SelectObject(memDC, dib);
      try
        // Fully transparent to start (CreateDIBSection already zeroes, explicit
        // for clarity).
        FillChar(bits^, w * h * 4, 0);

        // Pill body.
        brush := CreateSolidBrush(DWORD(ColorToRGB(gBadgeBg)));
        pen := CreatePen(PS_SOLID, 1, DWORD(ColorToRGB(gBadgeBg)));
        oldBrush := SelectObject(memDC, brush);
        oldPen := SelectObject(memDC, pen);
        RoundRect(memDC, 0, 0, w, h, radius * 2, radius * 2);
        SelectObject(memDC, oldBrush);
        SelectObject(memDC, oldPen);
        DeleteObject(brush);
        DeleteObject(pen);

        // Name, centred.
        SetBkMode(memDC, TRANSPARENT);
        SetTextColor(memDC, DWORD(ColorToRGB(gBadgeText)));
        TextOutW(memDC, (w - txtW) div 2, (h - txtH) div 2,
          PWideChar(wnick), Length(wnick));

        GdiFlush;

        // Force alpha=255 for every pixel inside the rounded shape (with A=255
        // the straight RGB already equals its premultiplied value); clear the
        // four rounded corners to fully transparent (RGB+A=0, no additive halo).
        for yy := 0 to h - 1 do
        begin
          pRow := PByte(bits) + yy * w * 4;
          for xx := 0 to w - 1 do
            if BadgePixelInside(xx, yy, w, h, radius) then
              (pRow + xx * 4 + 3)^ := 255
            else
              PDWord(pRow + xx * 4)^ := 0;
        end;

        // Blit the DIB to the layered window (position kept; size follows psize).
        screenDC := GetDC(0);
        try
          blend.BlendOp := AC_SRC_OVER;
          blend.BlendFlags := 0;
          blend.SourceConstantAlpha := 255;
          blend.AlphaFormat := AC_SRC_ALPHA;
          sz.cx := w;
          sz.cy := h;
          ptSrc.x := 0;
          ptSrc.y := 0;
          UpdateLayeredWindow(gBadgeHWnd, screenDC, nil, @sz, memDC,
            @ptSrc, 0, @blend, ULW_ALPHA);
        finally
          ReleaseDC(0, screenDC);
        end;
      finally
        SelectObject(memDC, oldDib);
        DeleteObject(dib);
      end;
    finally
      SelectObject(memDC, oldFont);
      DeleteObject(hf);
    end;
  finally
    DeleteDC(memDC);
  end;
end;

// WndProc of the pill window: handle clicks and the hand cursor.
function BadgeWndProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
begin
  case uMsg of
    WM_LBUTTONUP:
    begin
      if Assigned(gBadgeBridge) and Assigned(gBadgeBridge.Callback)
        and (not gBadgeBridge.Pending) then
      begin
        gBadgeBridge.Pending := true;
        Application.QueueAsyncCall(@gBadgeBridge.Fire, 0);
      end;
      Exit(0);
    end;
    WM_SETCURSOR:
    begin
      Windows.SetCursor(Windows.LoadCursor(0, IDC_HAND));
      Exit(1);
    end;
    WM_NCHITTEST:
      Exit(HTCLIENT);
  end;
  Result := DefWindowProc(hWnd, uMsg, wParam, lParam);
end;

procedure EnsureBadgeClass;
var
  wc: WNDCLASS;
begin
  if gBadgeClassReg then
    Exit;
  FillChar(wc, SizeOf(wc), 0);
  wc.lpfnWndProc := WNDPROC(@BadgeWndProc);
  wc.hInstance := GetModuleHandle(nil);
  wc.hCursor := LoadCursor(0, IDC_HAND);
  wc.lpszClassName := BADGE_CLASS_NAME;
  RegisterClass(wc);
  gBadgeClassReg := true;
end;

// Subclass of the owner form: reposition the pill on move/resize, repaint on
// DPI change, and tear the pill down with the window.
function BadgeOwnerWndProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
begin
  Result := CallWindowProc(Windows.WNDPROC(gBadgeOwnerOldProc), hWnd, uMsg,
    wParam, lParam);
  case uMsg of
    // The pill is a separate owned popup, so it cannot be glued to the caption
    // during an interactive move/resize — it would trail a frame behind and
    // leave a lagging outline outside the window. Hide it for the duration of
    // the modal move/resize loop and snap it back into place on release.
    WM_ENTERSIZEMOVE_MSG:
      if gBadgeHWnd <> 0 then
        ShowWindow(gBadgeHWnd, SW_HIDE);
    WM_EXITSIZEMOVE_MSG:
      if gBadgeHWnd <> 0 then
      begin
        RepositionBadge;
        ShowWindow(gBadgeHWnd, SW_SHOWNOACTIVATE);
      end;
    WM_WINDOWPOSCHANGED:
      // Reposition for non-drag moves (Aero snap, maximise, programmatic). While
      // an interactive drag is in progress the pill is hidden, so this just moves
      // an invisible window and reshows nothing.
      if gBadgeHWnd <> 0 then
        RepositionBadge;
    WM_DPICHANGED_MSG:
      if gBadgeHWnd <> 0 then
      begin
        PaintBadge;
        RepositionBadge;
      end;
    WM_NCDESTROY:
    begin
      if (gBadgeOwner <> 0) and (gBadgeOwnerOldProc <> 0) then
        SetWindowLongPtr(gBadgeOwner, GWL_WNDPROC, gBadgeOwnerOldProc);
      DestroyBadgeWindow;
      gBadgeOwner := 0;
      gBadgeOwnerOldProc := 0;
    end;
  end;
end;

class function TTrndiNativeWindows.SupportsUserBadge: boolean;
begin
  Result := true;
end;

function TTrndiNativeWindows.ShowUserBadge(const nick: string;
bg, textColor: TColor; const onClick: TTrndiWakeCallback): boolean;
var
  owner: HWND;
begin
  Result := false;
  if (nick = '') or (Application = nil) or (Application.MainForm = nil)
    or (not Application.MainForm.HandleAllocated) then
    Exit;
  owner := Application.MainForm.Handle;

  gBadgeNick := nick;
  gBadgeBg := bg;
  gBadgeText := textColor;

  EnsureBadgeClass;
  gBadgeOwner := owner;

  if gBadgeHWnd = 0 then
  begin
    gBadgeHWnd := CreateWindowEx(
      WS_EX_LAYERED or WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW,
      BADGE_CLASS_NAME, nil, WS_POPUP,
      0, 0, 16, 16, owner, 0, GetModuleHandle(nil), nil);
    if gBadgeHWnd = 0 then
      Exit;
  end;

  if gBadgeBridge = nil then
    gBadgeBridge := TWakeBridge.Create;
  gBadgeBridge.Callback := onClick;
  gBadgeBridge.Pending := false;

  PaintBadge;

  // Subclass the owner once so the pill follows the window.
  if gBadgeOwnerOldProc = 0 then
    gBadgeOwnerOldProc := SetWindowLongPtr(owner, GWL_WNDPROC,
      PtrInt(@BadgeOwnerWndProc));

  RepositionBadge;
  ShowWindow(gBadgeHWnd, SW_SHOWNOACTIVATE);
  Result := true;
end;

procedure TTrndiNativeWindows.HideUserBadge;
begin
  // Destroy the pill but keep the (harmless) owner subclass; it is removed on
  // the owner's WM_NCDESTROY. Leaving it avoids WndProc-chain corruption.
  DestroyBadgeWindow;
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
  try
    DestroyBadgeWindow;
  except
  end;
  FreeAndNil(gWakeBridge);
  FreeAndNil(gBadgeBridge);
  FreeAndNil(gOriginalAppIcon);
  if gLastBadgeIcon <> 0 then
  begin
    DestroyIcon(gLastBadgeIcon);
    gLastBadgeIcon := 0;
  end;

end.
