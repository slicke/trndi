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
unit trndi.native.mac;

{**
  @abstract(macOS-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeMac) which derives from
  @link(TTrndiNativeBase) and implements:
  - Text-to-speech via the built-in @code(say) command
  - Enabling dark appearance via @code(SimpleDarkMode)
  - Dock badge label updates
  - Simple HTTP GET using an NS-based helper

  Use the façade unit @code(trndi.native) which provides the platform alias.
}

{$I ../../inc/native.inc}
{$linkframework UserNotifications}

interface

uses
  Classes, SysUtils, Graphics, nsutils.nsmisc, nsutils.web.urlrequest, CocoaAll, nsutils.simpledarkmode,
  nsutils.nshelpers, nsutils.cocoahelpers, IniFiles, dialogs, StrUtils, Forms,
  trndi.native.base;

type
  {!
    @abstract(macOS implementation of @link(TTrndiNativeBase).)
    Relies on system tools for speech and dark appearance toggling.
  }
  TTrndiNativeMac = class(TTrndiNativeBase)
  public
    {** Show a visual notification on macOS.
      Prefer native NSUserNotificationCenter (app-attributed), fall back to base AppleScript impl. }
    procedure attention(topic, message: string); override;
    {** Speak @param(Text) using the built-in 'say' command.
        Runs asynchronously via a worker thread that also reaps the child
        process; the call returns immediately. }
    procedure Speak(const Text: string); override;
    {** Enable dark appearance for the app UI via SimpleDarkMode.
        @returns(True once the request is made) }
    class function setDarkMode: boolean;
    {** Tint the window's title bar to match a custom color, mirroring the
        Windows DWMWA_CAPTION_COLOR behavior. The form body's color is left
        untouched so it can keep encoding the blood-glucose reading.
        @returns(True if the NSWindow was reachable and the color was applied) }
    class function SetTitleColor(form: PtrUInt; bg, Text: TColor): boolean; override;

    // Settings API overrides (NSUserDefaults/CFPreferences)
    {** Read a string from preferences; returns @param(def) when missing.
        Keys are scoped by @link(TTrndiNativeBase.buildKey). }
    function GetSetting(const keyname: string; def: string = '';
      global: boolean = False): string; override;
    {** Write a string to preferences under the scoped key. }
    procedure SetSetting(const keyname: string; const val: string;
      global: boolean = False); override;
    {** Delete a setting (removes the key from NSUserDefaults). }
    procedure DeleteSetting(const keyname: string; global: boolean = False); override;
    {** Preferences are live; nothing to reload. }
    procedure ReloadSettings; override;
    {** Export all settings to INI format string. }
    function ExportSettings: string; override;
    {** Import settings from INI format string. }
    procedure ImportSettings(const iniData: string); override;
    // Badge
    {** Set the dock tile badge label (text only). }
    procedure SetBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
    {** Ignore extra params and delegate to simple overload. }
    procedure SetBadge(const Value: string; BadgeColor: TColor;
      badge_size_ratio: double; min_font_size: integer); overload; override;
  {** Simple HTTP GET using NS HTTP helper with default UA.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
    class function getURL(const url: string; out res: string): boolean; override;
    {** Simple HTTP POST using NSURLSession-based helper. }
    class function postURL(const url: string; const body: string;
      const contentType: string; out res: string): boolean; override;
    {** True if AppleInterfaceStyle indicates dark mode. }
    class function isDarkMode: boolean; override;
    {** NSUserNotificationCenter is available on macOS. }
    class function isNotificationSystemAvailable: boolean; override;
    {** Identify the notification backend on macOS ('NSUserNotification'). }
    class function getNotificationSystem: string; override;
    {** Check whether platform TTS is available. }
    class function SpeakAvailable: boolean; override;
    {** Request notification authorization and perform other per-launch setup. }
    procedure start;    {** Name of the software used for speech on macOS (e.g., 'say'). }
    class function SpeakSoftwareName: string; override;
    {** Best-effort window manager name for macOS. }
    class function GetWindowManagerName: string; override;
    {** macOS always provides a system-wide global menu bar. }
    class function HasGlobalMenu: boolean; override;
    {** Auto-start is supported via a per-user LaunchAgent plist. }
    class function AutoStartAvailable: boolean; override;
    {** True when @code(~/Library/LaunchAgents/com.slicke.trndi.autostart.plist) exists. }
    class function GetAutoStart: boolean; override;
    {** Write or remove the LaunchAgent plist; best-effort launchctl
        load/unload so the change takes effect without a relog. The plist
        invokes the app via @code(/usr/bin/open -a <bundle>) so Launch
        Services drives startup with full bundle identity (icon, Dock,
        code-sign context). Falls back to the raw executable only when
        the binary is not inside a .app (dev builds). }
    class function SetAutoStart(Enable: boolean): boolean; override;
    {** Simple HTTP GET/POST using @code(TNSHTTPSendAndReceive). }
    function request(const post: boolean; const endpoint: string;
      const params: array of string; const jsondata: string = '';
      const header: string = ''; prefix: boolean = true): string; override;
    {** Enhanced HTTP request via TNSHTTPSendAndReceive: tracks cookies,
        follows redirects manually, captures response headers. }
    function requestEx(const post: boolean; const endpoint: string;
      const params: array of string; const jsondata: string = '';
      cookieJar: TStringList = nil; followRedirects: boolean = true;
      maxRedirects: integer = 10; customHeaders: TStringList = nil;
      prefix: boolean = true): THTTPResponse; override;
    {** Play an audio file via afplay. }
    class procedure PlaySound(const FileName: string); override;
    {** Resolve the user's preferred UI language via NSLocale. }
    class function GetOSLanguage: string; override;
    {** Subscribe to @code(NSWorkspaceDidWakeNotification) so the wake
        callback fires when the Mac resumes from sleep. The observer is
        installed on the shared NSWorkspace notification center and
        marshals delivery to the main thread via @code(Application.QueueAsyncCall). }
    procedure RegisterWakeCallback(const Callback: TTrndiWakeCallback); override;
    {** Remove the NSWorkspace wake observer and clear the callback. }
    procedure UnregisterWakeCallback; override;
  end;

implementation

uses
  DateUtils, trndi.native.async;

const
  ObjCLib = '/usr/lib/libobjc.A.dylib';
  CFLib   = '/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation';

type
  id  = Pointer;
  SEL = Pointer;

var
  CachedHasBundleIdentifier: Integer = -1;

// Typed imports of objc_msgSend with a few arities we need
function objc_msgSend0(obj: id; sel: SEL): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend1(obj: id; sel: SEL; p1: id): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend_uint(obj: id; sel: SEL): NativeUInt; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend3(obj: id; sel: SEL; p1: id; p2: id; p3: id): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend2_id_id(obj: id; sel: SEL; p1: id; p2: id): id; cdecl; external ObjCLib name 'objc_msgSend';
// NSInteger param (for setInterruptionLevel:)
function objc_msgSend1_ni(obj: id; sel: SEL; p1: NSInteger): id; cdecl; external ObjCLib name 'objc_msgSend';
// typed helper for sending a uint + id (used for requestAuthorizationWithOptions:completionHandler:)
function objc_msgSend_uint_id(obj: id; sel: SEL; p1: NativeUInt; p2: id): id; cdecl; external ObjCLib name 'objc_msgSend';
// BOOL return + SEL param (respondsToSelector:)
function objc_msgSend_bool_sel(obj: id; sel: SEL; p1: SEL): Boolean; cdecl; external ObjCLib name 'objc_msgSend';
function objc_getClass(name: MarshaledAString): id;        cdecl; external ObjCLib;
function sel_registerName(name: MarshaledAString): SEL;    cdecl; external ObjCLib;

function TrndiHasBundleIdentifier: Boolean;
var
  NSBundleClass, mainBundle, ident: id;
begin
  if CachedHasBundleIdentifier <> -1 then
    Exit(CachedHasBundleIdentifier = 1);

  Result := False;
  NSBundleClass := objc_getClass('NSBundle');
  if NSBundleClass = nil then Exit;
  mainBundle := objc_msgSend0(NSBundleClass, sel_registerName('mainBundle'));
  if mainBundle = nil then Exit;
  ident := objc_msgSend0(mainBundle, sel_registerName('bundleIdentifier'));
  if ident = nil then Exit;
  Result := objc_msgSend_uint(ident, sel_registerName('length')) > 0;

  // Having a CFBundleIdentifier is necessary but not sufficient: the
  // notification centers (UNUserNotificationCenter.currentNotificationCenter
  // and NSUserNotificationCenter.defaultUserNotificationCenter) execute a
  // brk trap — EXC_BAD_INSTRUCTION, uncatchable from Pascal — when
  // LaunchServices has no identity for the process ("bundleProxyForCurrent-
  // Process is nil"). That is the case when the binary is spawned directly
  // by a debugger/IDE instead of launched as a registered .app, even though
  // NSBundle still reads the identifier from Info.plist. NSRunningApplication
  // mirrors the LaunchServices view, so require it to agree.
  if Result then
    Result := NSRunningApplication.currentApplication.bundleIdentifier <> nil;

  if Result then
    CachedHasBundleIdentifier := 1
  else
    CachedHasBundleIdentifier := 0;
end;

{------------------------------------------------------------------------------
  UNUserNotificationCenter delegate + authorization state
  --------------------------------------------------------
  Two silent-failure modes are handled here:

  1) Foreground suppression: unless the center has a delegate implementing
     userNotificationCenter:willPresentNotification:withCompletionHandler:,
     macOS does NOT show banners while the posting app is frontmost — and
     Trndi is typically the frontmost/visible app when a glucose alert
     fires. The delegate below requests banner + sound presentation even
     in the foreground.

  2) requestAuthorizationWithOptions:completionHandler: declares its handler
     nonnull; the framework copies and invokes it, so the previous nil
     handler was undefined behavior. A capture-free global ObjC block now
     records the outcome, letting attention() route straight to the
     osascript fallback when the user has denied notification permission
     (a UN request would otherwise be dropped without a trace).
 ------------------------------------------------------------------------------}

const
  BLOCK_IS_GLOBAL = 1 shl 28;
  // UNNotificationPresentationOptions bits
  UNPresentSound  = 1 shl 1;
  UNPresentAlert  = 1 shl 2; // deprecated in macOS 11, only option before it
  UNPresentList   = 1 shl 3; // macOS 11+
  UNPresentBanner = 1 shl 4; // macOS 11+

type
  // ABI layout of an Objective-C block. For blocks we receive only `invoke`
  // matters; for the block we create the descriptor must be present too.
  PObjCBlockLiteral = ^TObjCBlockLiteral;
  TObjCBlockLiteral = record
    isa: Pointer;
    flags: LongInt;
    reserved: LongInt;
    invoke: Pointer;
    descriptor: Pointer;
  end;
  TObjCBlockDescriptor = record
    reserved: NativeUInt;
    size: NativeUInt;
  end;
  // void (^)(UNNotificationPresentationOptions options)
  TUNPresentHandler = procedure(block: Pointer; options: NSUInteger); cdecl;

  TUNCenterDelegate = objcclass(NSObject)
    procedure userNotificationCenter_willPresentNotification_withCompletionHandler(
      {%H-}center: Pointer; {%H-}notification: Pointer; completionHandler: Pointer);
      message 'userNotificationCenter:willPresentNotification:withCompletionHandler:';
  end;

var
  // Class object for capture-free blocks, exported by libSystem (Blocks ABI).
  _NSConcreteGlobalBlock: array[0..31] of Pointer; cvar; external;

var
  gUNSetupDone: Boolean = False;
  gUNDelegate: TUNCenterDelegate = nil;
  gNotifyAuthDenied: Boolean = False;
  gAuthBlockDesc: TObjCBlockDescriptor;
  gAuthBlock: TObjCBlockLiteral;

procedure TUNCenterDelegate.userNotificationCenter_willPresentNotification_withCompletionHandler(
  center: Pointer; notification: Pointer; completionHandler: Pointer);
var
  opts: NSUInteger;
begin
  // Banner/list options exist on macOS 11+ (AppKit 2022); older systems use
  // the then-current alert option. Sound is requested in both cases.
  if NSAppKitVersionNumber >= 2022 then
    opts := UNPresentBanner or UNPresentList or UNPresentSound
  else
    opts := UNPresentAlert or UNPresentSound;
  if completionHandler <> nil then
    TUNPresentHandler(PObjCBlockLiteral(completionHandler)^.invoke)(
      completionHandler, opts);
end;

// Matches void (^)(BOOL granted, NSError *error) after the implicit block arg.
procedure UNAuthCompletion({%H-}block: Pointer; granted: Boolean; {%H-}error: id); cdecl;
begin
  // Invoked on a framework background queue; a plain boolean store is fine.
  gNotifyAuthDenied := not granted;
end;

procedure EnsureUNCenterSetup;
var
  UNClass, Center: id;
begin
  if gUNSetupDone then Exit;
  // UNUserNotificationCenter asserts if there is no bundle identifier
  // (e.g. running the raw binary outside a .app bundle).
  if not TrndiHasBundleIdentifier then Exit;
  UNClass := objc_getClass('UNUserNotificationCenter');
  if UNClass = nil then Exit;
  Center := objc_msgSend0(UNClass, sel_registerName('currentNotificationCenter'));
  if Center = nil then Exit;
  gUNSetupDone := True;

  // Foreground presentation (see unit comment above). The center's delegate
  // property is weak; the global keeps the object alive for the app's life.
  gUNDelegate := TUNCenterDelegate.alloc.init;
  objc_msgSend1(Center, sel_registerName('setDelegate:'), id(gUNDelegate));

  // UNAuthorizationOptionAlert|Badge|Sound = (1<<2)|(1<<0)|(1<<1) = 7.
  // The completion handler is nonnull — pass a real capture-free block.
  gAuthBlockDesc.reserved := 0;
  gAuthBlockDesc.size := SizeOf(TObjCBlockLiteral);
  gAuthBlock.isa := @_NSConcreteGlobalBlock;
  gAuthBlock.flags := BLOCK_IS_GLOBAL;
  gAuthBlock.reserved := 0;
  gAuthBlock.invoke := CodePointer(@UNAuthCompletion);
  gAuthBlock.descriptor := @gAuthBlockDesc;
  objc_msgSend_uint_id(Center,
    sel_registerName('requestAuthorizationWithOptions:completionHandler:'),
    7, @gAuthBlock);
end;

{------------------------------------------------------------------------------
  attention (macOS)
  -----------------
  Try UNUserNotificationCenter -> NSUserNotification -> osascript.

  Behavior notes:
  - interruptionLevel = TimeSensitive: alerts break through Focus / Do Not
    Disturb (the user can still revoke this per-app in System Settings).
  - threadIdentifier and the request identifier are derived from `topic`, so
    repeated alerts of the same kind (e.g. successive HIGH readings) replace
    the previous one in Notification Center instead of stacking. This matches
    a glucose monitor's "newest reading wins" semantics.
  - trigger = nil delivers immediately rather than after a 1s schedule.
  - categoryIdentifier is set so we can later attach actions (Snooze, etc.).
  - Foreground banners require the UN center delegate installed by
    EnsureUNCenterSetup; without it macOS suppresses notifications while
    the app is frontmost.
 ------------------------------------------------------------------------------}

procedure TTrndiNativeMac.attention(topic, message: string);

  procedure OsascriptFallback(const Title, Msg: string);
  begin
    // NSUserNotification is deprecated and may not show reliably on recent macOS.
    // Final fallback: display the alert via AppleScript / osascript.
    // Use argv to avoid AppleScript quoting/escaping issues.
    // Fire-and-forget via the async worker, which drains stdout and reaps the
    // child; the old poUsePipes+poWaitOnExit TProcess pattern could deadlock
    // on large output and blocked the caller.
    if not FileExists('/usr/bin/osascript') then
      Exit;

    RunAndCaptureSimpleAsync('/usr/bin/osascript',
      ['-e', 'on run argv',
       '-e', 'display notification (item 1 of argv) with title (item 2 of argv)',
       '-e', 'end run',
       UTF8Encode(Msg), UTF8Encode(Title)], nil);
  end;

const
  UNNotificationInterruptionLevelTimeSensitive: NSInteger = 2;
var
  UNClass, Center, ContentClass, Content, ReqClass, UNReq, SoundClass, Sound: id;
  NSReq: NSUserNotification;
  IdStr, TitleStr, BodyStr, ThreadStr, CategoryStr: NSString;
  selCurrent, selAddReq, selNew, selSetTitle, selSetBody: SEL;
  ok: Boolean;
  sId: string;
begin
  ok := False;
  try
    // Use UNUserNotificationCenter when available (modern API).
    // Note: UNUserNotificationCenter asserts if there is no bundle identifier
    // (e.g. running the raw binary outside a .app bundle).
    // Late setup guard: normally done in start(), but attention() must also
    // work for callers that never ran start (e.g. the settings-form test).
    EnsureUNCenterSetup;
    // When the user has denied permission the center drops requests without
    // a trace — go straight to the osascript fallback instead.
    if TrndiHasBundleIdentifier and not gNotifyAuthDenied then
    begin
      UNClass := objc_getClass('UNUserNotificationCenter');
      if UNClass <> nil then
      begin
        selCurrent := sel_registerName('currentNotificationCenter');
        Center := objc_msgSend0(UNClass, selCurrent);
        if Center <> nil then
        begin
          // Create content
          ContentClass := objc_getClass('UNMutableNotificationContent');
          selNew := sel_registerName('new');
          Content := objc_msgSend0(ContentClass, selNew);

          // NSSTR decodes via the system C-string encoding and garbles UTF-8
          // (translated alert titles, trend arrows); build the strings
          // explicitly as UTF-8. stringWithUTF8String returns autoreleased
          // objects, so no Release here.
          TitleStr := NSString.stringWithUTF8String(PChar(topic));
          BodyStr := NSString.stringWithUTF8String(PChar(message));
          selSetTitle := sel_registerName('setTitle:');
          selSetBody := sel_registerName('setBody:');
          objc_msgSend1(Content, selSetTitle, TitleStr);
          objc_msgSend1(Content, selSetBody, BodyStr);

          // Default sound — without this the alert is silent for users who
          // haven't explicitly enabled sound for the app.
          SoundClass := objc_getClass('UNNotificationSound');
          if SoundClass <> nil then
          begin
            Sound := objc_msgSend0(SoundClass, sel_registerName('defaultSound'));
            if Sound <> nil then
              objc_msgSend1(Content, sel_registerName('setSound:'), Sound);
          end;

          // Time-sensitive: pierces Focus / DND for glucose alerts. The
          // selector only exists on macOS 12+; sending it unguarded raises
          // an ObjC exception (which a Pascal except cannot catch) on 11.
          if objc_msgSend_bool_sel(Content, sel_registerName('respondsToSelector:'),
            sel_registerName('setInterruptionLevel:')) then
            objc_msgSend1_ni(Content, sel_registerName('setInterruptionLevel:'),
              UNNotificationInterruptionLevelTimeSensitive);

          // Group alerts of the same kind together; topic is the natural key
          // (HIGH/LOW/sensor-fault all have distinct titles).
          ThreadStr := NSString.stringWithUTF8String(PChar(topic));
          objc_msgSend1(Content, sel_registerName('setThreadIdentifier:'), ThreadStr);

          // Category — placeholder for future actions (Snooze/Dismiss).
          CategoryStr := NSString.stringWithUTF8String(PChar('trndi.alert'));
          objc_msgSend1(Content, sel_registerName('setCategoryIdentifier:'), CategoryStr);

          // Stable identifier per topic: posting again with the same id
          // *replaces* the previous notification rather than stacking. For a
          // glucose monitor this is medically appropriate — the newest
          // reading is what matters.
          sId := 'trndi.alert.' + topic;
          IdStr := NSString.stringWithUTF8String(PChar(sId));

          // Create request with nil trigger -> deliver immediately.
          ReqClass := objc_getClass('UNNotificationRequest');
          UNReq := objc_msgSend3(ReqClass, sel_registerName('requestWithIdentifier:content:trigger:'), IdStr, Content, nil);

          // Add request (no completion handler)
          selAddReq := sel_registerName('addNotificationRequest:withCompletionHandler:');
          objc_msgSend2_id_id(Center, selAddReq, UNReq, nil);

          // Content came from 'new' (+1); the request retains it, so drop our
          // reference here or one content object leaks per notification.
          objc_msgSend0(Content, sel_registerName('release'));

          // Best-effort; do not raise on failures
          ok := True;
        end;
      end;
    end;
  except
    // Ignore and fall back.
  end;

  if ok then Exit;

  // Fallback to deprecated NSUserNotification implementation.
  // defaultUserNotificationCenter does NOT return nil for a process without
  // bundle identity (raw dev binary, debugger launch) — it trips a Foundation
  // assertion (brk -> EXC_BAD_INSTRUCTION) that no Pascal except can catch.
  // Only touch it when bundled; unbundled runs go straight to osascript.
  try
    Center := nil;
    if TrndiHasBundleIdentifier then
      Center := NSUserNotificationCenter.defaultUserNotificationCenter;
    if Center <> nil then
    begin
      NSReq := NSUserNotification.alloc.init;
      try
        // Autoreleased UTF-8 strings; NSSTR would garble non-ASCII and must
        // not be Released.
        TitleStr := NSString.stringWithUTF8String(PChar(topic));
        BodyStr := NSString.stringWithUTF8String(PChar(message));
        NSReq.setTitle(TitleStr);
        NSReq.setInformativeText(BodyStr);

        objc_msgSend1(Center, sel_registerName('deliverNotification:'), NSReq);
        Exit;
      finally
        NSReq.Release;
      end;
    end;
  except
    // Ignore and fall back.
  end;

  OsascriptFallback(topic, message);
end;
{------------------------------------------------------------------------------
  Speak
  -----
  Use the built-in 'say' tool to speak text asynchronously.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.Speak(const Text: string);
var
  VoiceName: string;
begin
  if not FileExists('/usr/bin/say') then
  begin
    if not ttsErrorShown then
    begin
      ShowMessage('TTS Error: /usr/bin/say not found');
      ttsErrorShown := true;
    end;
    Exit;
  end;

  // Fire-and-forget via the async worker: doesn't block the UI, and the
  // worker thread reaps the child process so zombies don't accumulate.
  VoiceName := GetSetting('tts.voice.name', '');
  if VoiceName <> '' then
    RunAndCaptureSimpleAsync('/usr/bin/say', ['-v', VoiceName, Text], nil)
  else
    RunAndCaptureSimpleAsync('/usr/bin/say', [Text], nil);
end;

{------------------------------------------------------------------------------
  SpeakAvailable (macOS)
  ----------------------
  macOS provides native TTS; assume available.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.SpeakAvailable: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  SpeakSoftwareName (macOS)
  -------------------------
  Name of the speech tool used on macOS.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.SpeakSoftwareName: string;
begin
  Result := 'say';
end;

{------------------------------------------------------------------------------
  GetWindowManagerName (macOS)
  -----------------------------
  Return the macOS window server identifier. Historically the macOS window
  system is provided by WindowServer.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.GetWindowManagerName: string;
begin
  Result := 'WindowServer';
end;

class function TTrndiNativeMac.HasGlobalMenu: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  AutoStart (macOS)
  -----------------
  Backed by a per-user LaunchAgent plist in ~/Library/LaunchAgents. The plist
  alone is sufficient because launchd reads the directory at login. We also
  best-effort launchctl load/unload so toggling takes effect immediately for
  the current session; failures of launchctl are ignored.

  The plist invokes the app via /usr/bin/open -a <bundle> rather than the
  inner Mach-O so Launch Services drives startup with full bundle identity
  (icon, Dock, code-sign context, TCC permissions). If the binary isn't
  inside a .app — e.g. a dev build run from the IDE — we fall back to
  pointing ProgramArguments at the raw executable.
 ------------------------------------------------------------------------------}
const
  MAC_AUTOSTART_LABEL = 'com.slicke.trndi.autostart';

function MacAutoStartPlistPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
    'Library/LaunchAgents/' + MAC_AUTOSTART_LABEL + '.plist';
end;

// Walk up from ParamStr(0) to find a .app container. Returns '' when the
// running binary isn't inside a bundle (typical for dev/test runs).
function MacBundlePath: string;
var
  p: string;
  i: Integer;
begin
  Result := '';
  p := ParamStr(0);
  // Typical layout: <bundle>.app/Contents/MacOS/<exe>
  for i := 1 to 3 do
    p := ExtractFileDir(p);
  if (p <> '') and (LowerCase(ExtractFileExt(p)) = '.app') and DirectoryExists(p) then
    Result := p;
end;

procedure RunLaunchctl(const action, plistPath: string);
var
  outS: string;
  exitCode: integer;
begin
  // Synchronous on purpose: the disable path must finish unloading the job
  // before the plist is deleted. The worker drains stdout and bounds the wait,
  // unlike the old poUsePipes+poWaitOnExit pattern which could deadlock.
  // Best-effort; the plist on disk is what makes it persistent.
  RunAndCaptureSimpleWait('/bin/launchctl', [action, '-w', plistPath],
    outS, exitCode, 5000);
end;

class function TTrndiNativeMac.AutoStartAvailable: boolean;
begin
  Result := true;
end;

class function TTrndiNativeMac.GetAutoStart: boolean;
begin
  Result := FileExists(MacAutoStartPlistPath);
end;

class function TTrndiNativeMac.SetAutoStart(Enable: boolean): boolean;
var
  path, exe, bundle: string;
  sl: TStringList;
begin
  Result := false;
  path := MacAutoStartPlistPath;
  if Enable then
  begin
    if not ForceDirectories(ExtractFilePath(path)) then
      Exit;
    exe := ParamStr(0);
    bundle := MacBundlePath;
    sl := TStringList.Create;
    try
      sl.Add('<?xml version="1.0" encoding="UTF-8"?>');
      sl.Add('<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
      sl.Add('<plist version="1.0">');
      sl.Add('<dict>');
      sl.Add('  <key>Label</key><string>' + MAC_AUTOSTART_LABEL + '</string>');
      sl.Add('  <key>ProgramArguments</key>');
      if bundle <> '' then
      begin
        sl.Add('  <array>');
        sl.Add('    <string>/usr/bin/open</string>');
        sl.Add('    <string>-a</string>');
        sl.Add('    <string>' + bundle + '</string>');
        sl.Add('  </array>');
      end
      else
        sl.Add('  <array><string>' + exe + '</string></array>');
      sl.Add('  <key>RunAtLoad</key><true/>');
      sl.Add('</dict>');
      sl.Add('</plist>');
      try
        sl.SaveToFile(path);
        RunLaunchctl('load', path);
        Result := true;
      except
        Result := false;
      end;
    finally
      sl.Free;
    end;
  end
  else
  begin
    if FileExists(path) then
    begin
      RunLaunchctl('unload', path);
      Result := DeleteFile(path);
    end
    else
      Result := true;
  end;
end;

{------------------------------------------------------------------------------
  getURL
  ------
  Simple HTTP GET using NS-based helper; returns response text or error.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  send, response: TStringStream;
  headers: TStringList;
  httpClient: TNSHTTPSendAndReceive;
  tempInstance: TTrndiNativeMac;
  proxyHost, proxyPort, proxyUser, proxyPass: string;
begin
  res := '';
  send := TStringStream.Create('');
  response := TStringStream.Create('');
  headers := TStringList.Create;
  httpClient := TNSHTTPSendAndReceive.Create;
  tempInstance := TTrndiNativeMac.Create;
  try
    try
      httpClient.address := url;
      httpClient.method := 'GET';
      headers.Add('User-Agent=' + DEFAULT_USER_AGENT);

      // Proxy settings (retrieved for global scope, but not set as TNSHTTPSendAndReceive doesn't support proxy)
      proxyHost := tempInstance.GetSetting('proxy.host', '', true);
      proxyPort := tempInstance.GetSetting('proxy.port', '8080', true);
      proxyUser := tempInstance.GetSetting('proxy.user', '', true);
      proxyPass := tempInstance.GetSetting('proxy.pass', '', true);

      if httpClient.SendAndReceive(send, response, headers) then
      begin
        res := Trim(response.DataString);
        Result := True;
      end
      else
      begin
        // Normalize an error: LastErrMsg usually contains the reason
        res := Trim(httpClient.LastErrMsg);
        Result := False;
      end;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := False;
      end;
    end;
  finally
    httpClient.Free;
    send.Free;
    response.Free;
    headers.Free;
    tempInstance.Free;
  end;
end;

{------------------------------------------------------------------------------
  postURL
  -------
  Simple HTTP POST using NS-based helper; returns response text or error.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.postURL(const url: string; const body: string;
  const contentType: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  send, response: TStringStream;
  headers: TStringList;
  httpClient: TNSHTTPSendAndReceive;
begin
  res := '';
  send := TStringStream.Create('');
  response := TStringStream.Create('');
  headers := TStringList.Create;
  httpClient := TNSHTTPSendAndReceive.Create;
  try
    try
      httpClient.address := url;
      httpClient.method := 'POST';
      headers.Add('User-Agent=' + DEFAULT_USER_AGENT);
      if contentType <> '' then
        headers.Add('Content-Type=' + contentType);

      if body <> '' then
      begin
        send.Write(body[1], Length(body));
        send.Position := 0;
        headers.Add('Content-Length=' + IntToStr(send.Size));
      end;

      if httpClient.SendAndReceive(send, response, headers) then
      begin
        res := Trim(response.DataString);
        Result := True;
      end
      else
      begin
        res := Trim(httpClient.LastErrMsg);
        Result := False;
      end;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := False;
      end;
    end;
  finally
    httpClient.Free;
    send.Free;
    response.Free;
    headers.Free;
  end;
end;

{------------------------------------------------------------------------------
  isDarkMode
  ----------
  Detect macOS dark appearance via AppleInterfaceStyle preference.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.isDarkMode: boolean;
begin
  Result := Pos('DARK', UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable
  -----------------------------
  NSUserNotificationCenter is present on macOS.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.isNotificationSystemAvailable: boolean;
begin
  // The native notification center is present on macOS.
  // Note: user-level notification permissions may still block delivery.
  Result := true;
end;

{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Identify the macOS notification backend used by this implementation.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.getNotificationSystem: string;
begin
  // Prefer native notifications; fall back to osascript in the base class.
  Result := 'NSUserNotification';
end;


{------------------------------------------------------------------------------
  setDarkMode
  -----------
  Enable dark appearance for the app using SimpleDarkMode.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.setDarkMode: boolean;
begin
  // Enable dark appearance for the app's UI via SimpleDarkMode
  nsutils.simpledarkmode.EnableAppDarkMode;
  Result := True;
end;

{------------------------------------------------------------------------------
  SetTitleColor
  -------------
  Paint the NSWindow's title-bar strip in the requested color so it blends
  into the form body. Caller picks Text=clBlack on a light bg or clWhite on
  a dark bg; we mirror that into Aqua / DarkAqua appearance so the title
  text and traffic-light buttons stay readable.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.SetTitleColor(form: PtrUInt;
  bg, Text: TColor): boolean;
var
  View    : NSView;
  Win     : NSWindow;
  RGB     : longint;
  R, G, B : Double;
  UseDark : Boolean;
begin
  Result := False;
  if form = 0 then
    Exit;
  try
    View := NSView(form);
    if View = nil then Exit;
    Win := View.window;
    if Win = nil then Exit;

    // TColor after ColorToRGB is $00BBGGRR: R = low byte, G = mid, B = high.
    RGB := ColorToRGB(bg);
    R := (RGB and $FF) / 255.0;
    G := ((RGB shr 8) and $FF) / 255.0;
    B := ((RGB shr 16) and $FF) / 255.0;
    // Caller computes Text via IsLightColor(bg): clBlack when bg is light,
    // clWhite when bg is dark. Mirror that into the macOS appearance choice.
    UseDark := ColorToRGB(Text) = ColorToRGB(clWhite);

    SetCocoaTitleBarColor(Win, R, G, B, UseDark);
    Result := True;
  except
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  start
  -----
  Best-effort notification setup on startup (no-op if the UserNotifications
  framework is not present): installs the foreground-presentation delegate
  and requests authorization. We don't wait for the completion handler to
  avoid blocking startup; the outcome is recorded asynchronously and read
  by attention().
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.start;
begin
  inherited start;
  try
    EnsureUNCenterSetup;
  except
    // ignore
  end;
end;

{------------------------------------------------------------------------------
  SetBadge (simple)
  -----------------
  Set the dock tile badge label with the provided value.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.SetBadge(const Value: string; BadgeColor: TColor);
var
  NSS: NSString;
  labelText: string;
begin
  if Value = '' then
  begin
    NSApp.dockTile.setBadgeLabel(nil);
    Exit;
  end;
  labelText := Value;
  // Optional trend arrow appended after the reading (badgeTrend property).
  if FBadgeTrend <> '' then
    labelText := labelText + ' ' + FBadgeTrend;
  // NSSTR decodes via the system C-string encoding and garbles UTF-8 (the
  // trend arrows became mojibake); build the string explicitly as UTF-8.
  // stringWithUTF8String returns an autoreleased object, so no Release here.
  NSS := NSString.stringWithUTF8String(PChar(labelText));
  NSApp.dockTile.setBadgeLabel(NSS);
end;

{------------------------------------------------------------------------------
  SetBadge (full signature)
  -------------------------
  Ignore extra parameters and delegate to the simple overload on macOS.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.SetBadge(const Value: string; BadgeColor: TColor;
  badge_size_ratio: double; min_font_size: integer);
begin
  // Ignore extra params on macOS and delegate to the simple overload
  SetBadge(Value, BadgeColor);
end;

{------------------------------------------------------------------------------
  Preferences encoding migration
  ------------------------------
  Historic builds wrote NSUserDefaults through StrToNSStr/NSStrToStr, whose
  default encoding is CP1252: the app's UTF-8 strings were decoded byte-by-
  byte as CP1252 before storage. ASCII survives that unchanged, but non-ASCII
  keys and values (profile-name key prefixes, nicknames, media file paths...)
  were stored as mojibake, and bytes undefined in CP1252 (0x81/0x8D/0x8F/
  0x90/0x9D) were lost outright.

  Settings now read/write proper UTF-8, so existing entries must be rewritten
  once. Encoding a stored (mojibake) string back through CP1252 recovers the
  original UTF-8 bytes exactly, because every stored string was created by a
  CP1252 decode. Pure-ASCII entries are identical in both encodings and are
  left untouched — this also avoids copying unrelated NSGlobalDomain keys
  (which dictionaryRepresentation includes) into the app's domain.

  A persistent ASCII marker key makes the migration run once per install; a
  process-local flag makes the check run once per session.
 ------------------------------------------------------------------------------}
const
  PREFS_ENCODING_KEY = 'trndi.prefs.encoding';
  PREFS_ENCODING_UTF8 = 'utf8';
  NSEncodingASCII = 1; // NSASCIIStringEncoding

var
  PrefsMigrationChecked: boolean = false;

procedure EnsurePrefsUtf8Migration;
var
  pool: NSAutoreleasePool;
  defaults: NSUserDefaults;
  dict: NSDictionary;
  enumerator: NSEnumerator;
  keyNS, newKeyNS, newValNS: NSString;
  valObj: NSObject;
  utf8Key, utf8Val: string;
begin
  if PrefsMigrationChecked then
    Exit;
  PrefsMigrationChecked := true;
  // Own pool: settings can be read before the LCL run loop provides one.
  pool := NSAutoreleasePool.alloc.init;
  try
    // Marker is ASCII, so it reads identically under either encoding.
    if GetPrefUTF8String(PREFS_ENCODING_KEY) = PREFS_ENCODING_UTF8 then
      Exit;
    defaults := NSUserDefaults.standardUserDefaults;
    // dictionaryRepresentation is a snapshot; mutating defaults inside the
    // loop is safe.
    dict := defaults.dictionaryRepresentation;
    enumerator := dict.keyEnumerator;
    keyNS := NSString(enumerator.nextObject);
    while keyNS <> nil do
    begin
      valObj := NSObject(dict.objectForKey(keyNS));
      // Only NSString values (Trndi writes nothing else), and only entries
      // where the key or value actually contains non-ASCII characters.
      if (valObj <> nil) and valObj.isKindOfClass(objc_getClass('NSString')) and
         ((not keyNS.canBeConvertedToEncoding(NSEncodingASCII)) or
          (not NSString(valObj).canBeConvertedToEncoding(NSEncodingASCII))) then
      begin
        // CP1252 re-encode recovers the original UTF-8 bytes.
        utf8Key := NSStrToStr(keyNS);
        // Same system-namespace filter as ExportSettings.
        if (Pos('Apple', utf8Key) <> 1) and (Pos('com.apple', utf8Key) <> 1) and
          (Pos('NS', utf8Key) <> 1) then
        begin
          utf8Val := NSStrToStr(NSString(valObj));
          // Build the replacement strings before touching anything: CFString
          // returns nil for invalid UTF-8, which happens when the entry was
          // NOT written by the old CP1252 path (foreign keys, or an entry a
          // previously interrupted run already migrated). Skipping those
          // makes the migration safe to re-run.
          newKeyNS := Utf8StrToNSStr(utf8Key);
          newValNS := Utf8StrToNSStr(utf8Val);
          if (newKeyNS <> nil) and (newValNS <> nil) then
          begin
            // Drop the mojibake key first: for ASCII keys the rewrite below
            // overwrites in place, for non-ASCII keys the name changes.
            defaults.removeObjectForKey(keyNS);
            defaults.setObject_forKey(newValNS, newKeyNS);
          end;
        end;
      end;
      keyNS := NSString(enumerator.nextObject);
    end;
    SetPrefUTF8String(PREFS_ENCODING_KEY, PREFS_ENCODING_UTF8);
    defaults.synchronize;
  finally
    pool.release;
  end;
end;

{------------------------------------------------------------------------------
  GetSetting / SetSetting / DeleteSetting / ReloadSettings
  -------------------------------------------------------
  Preferences-backed settings (NSUserDefaults, UTF-8 keys and values).
 ------------------------------------------------------------------------------}
function TTrndiNativeMac.GetSetting(const keyname: string; def: string;
  global: boolean): string;
var
  key: string;
begin
  EnsurePrefsUtf8Migration;
  key := buildKey(keyname, global);
  Result := GetPrefUTF8String(key);
  if Result = '' then
    Result := def;
end;

procedure TTrndiNativeMac.SetSetting(const keyname: string; const val: string;
  global: boolean);
var
  key: string;
begin
  EnsurePrefsUtf8Migration;
  key := buildKey(keyname, global);
  SetPrefUTF8String(key, val);
end;

procedure TTrndiNativeMac.DeleteSetting(const keyname: string; global: boolean);
begin
  EnsurePrefsUtf8Migration;
  RemovePrefKey(buildKey(keyname, global));
end;

procedure TTrndiNativeMac.ReloadSettings;
begin
  // NSUserDefaults is live; no explicit reload needed
end;

{------------------------------------------------------------------------------
  ExportSettings
  --------------
  Export all NSUserDefaults settings to INI format string.
 ------------------------------------------------------------------------------}
function TTrndiNativeMac.ExportSettings: string;
var
  defaults: NSUserDefaults;
  dict: NSDictionary;
  enumerator: NSEnumerator;
  key: NSString;
  value: NSObject;
  sl: TStringList;
  keyStr: string;
begin
  Result := '';
  EnsurePrefsUtf8Migration;
  sl := TStringList.Create;
  try
    defaults := NSUserDefaults.standardUserDefaults;
    dict := defaults.dictionaryRepresentation;
    enumerator := dict.keyEnumerator;

    sl.Add('[trndi]');
    key := enumerator.nextObject;
    while key <> nil do
    begin
      keyStr := NSStrToUtf8Str(key);
      // Exclude system namespaces (Apple/NS preferences and macOS internals).
      // Do NOT filter on case: cfguser-prefixed keys can be mixed-case
      // (e.g. "Bjorn_api.host") and would otherwise be silently dropped.
      if (Pos('Apple', keyStr) <> 1)
        and (Pos('com.apple', keyStr) <> 1)
        and (Pos('NS', keyStr) <> 1) then
      begin
        value := dict.objectForKey(key);
        if value.isKindOfClass(objc_getClass('NSString')) then
          sl.Add(keyStr + '=' + NSStrToUtf8Str(NSString(value)))
        else if value.isKindOfClass(objc_getClass('NSNumber')) then
          sl.Add(keyStr + '=' + NSNumber(value).stringValue.UTF8String);
        // Skip other types for now
      end;
      key := enumerator.nextObject;
    end;
    
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

{------------------------------------------------------------------------------
  ImportSettings
  ---------------
  Import settings from INI format string to NSUserDefaults.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.ImportSettings(const iniData: string);
var
  sl: TStringList;
  mem: TMemoryStream;
  ini: TMemIniFile;
  sections, keys: TStringList;
  i, j: integer;
  section, key, value: string;
  defaults: NSUserDefaults;
begin
  if iniData = '' then
    Exit;
  sl := TStringList.Create;
  mem := TMemoryStream.Create;
  ini := nil;
  sections := TStringList.Create;
  keys := TStringList.Create;
  defaults := NSUserDefaults.standardUserDefaults;
  EnsurePrefsUtf8Migration;
  try
    mem.WriteBuffer(iniData[1], Length(iniData));
    mem.Position := 0;
    sl.LoadFromStream(mem);
    
    // Create a temporary INI file in memory
    ini := TMemIniFile.Create('');
    ini.SetStrings(sl);
    
    ini.ReadSections(sections);
    for i := 0 to sections.Count - 1 do
    begin
      section := sections[i];
      ini.ReadSection(section, keys);
      for j := 0 to keys.Count - 1 do
      begin
        key := keys[j];
        value := ini.ReadString(section, key, '');
        SetPrefUTF8String(key, value);
      end;
    end;
    
    defaults.synchronize;
  finally
    keys.Free;
    sections.Free;
    ini.Free;
    mem.Free;
    sl.Free;
  end;
end;

{------------------------------------------------------------------------------
  request (macOS)
  ---------------
  HTTP GET/POST via TNSHTTPSendAndReceive.
 ------------------------------------------------------------------------------}
function TTrndiNativeMac.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
const header: string; prefix: boolean): string;
var
  res, send: TStringStream;
  headers: TStringList;
  i: integer;
begin
  res := TStringStream.Create('');
  send := TStringStream.Create('');
  headers := TStringList.Create;
  try
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

      if header <> '' then
        Headers.Add(header);

      if jsondata <> '' then
      begin
        Headers.Add('Content-Type=application/json');
        if useragent <> '' then
          Headers.Add('User-Agent=' + useragent);

        send.Write(jsondata[1], Length(jsondata));
        Headers.Add('Content-Length=' + IntToStr(send.Size));
      end
      else if Length(params) > 0 then
      begin
        address := address + '?' + params[0];
        for i := 1 to High(params) do
          address := address + '&' + params[i];
      end;

      if SendAndReceive(send, res, headers) then
        Result := Trim(res.DataString)
      else
        Result := '+' + LastErrMsg;
    finally
      Free;
    end;
  finally
    res.Free;
    send.Free;
    headers.Free;
  end;
end;

{------------------------------------------------------------------------------
  requestEx (macOS)
  -----------------
  Cookie-aware, manually-redirect-following HTTP via TNSHTTPSendAndReceive.
 ------------------------------------------------------------------------------}
function TTrndiNativeMac.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList;
prefix: boolean): THTTPResponse;
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
  jarRef: TStringList;
  cookieHeader, baseCookie: string;

  // Insert or replace a "name=value" cookie: a rotated session cookie must
  // supersede its old value, not be sent alongside it.
  procedure UpsertCookie(list: TStringList; const cookieVal: string);
  var
    eq, k: integer;
    namePrefix: string;
  begin
    if list = nil then
      Exit;
    eq := Pos('=', cookieVal);
    if eq <= 1 then
    begin
      if list.IndexOf(cookieVal) = -1 then
        list.Add(cookieVal);
      Exit;
    end;
    namePrefix := Copy(cookieVal, 1, eq); // includes '='
    for k := 0 to list.Count - 1 do
      if Pos(namePrefix, list[k]) = 1 then
      begin
        list[k] := cookieVal;
        Exit;
      end;
    list.Add(cookieVal);
  end;

  // Rebuild the Cookie header for a hop: caller-supplied base cookies are
  // kept unless the jar now holds a value under the same name — a rotated
  // cookie must be sent once, with the fresh jar value.
  function BuildCookieHeaderLocal(const base: string; jar: TStringList): string;
  var
    rest, part, namePrefix: string;
    p, eq, k: integer;
    superseded: boolean;
  begin
    Result := '';
    rest := base;
    while rest <> '' do
    begin
      p := Pos(';', rest);
      if p > 0 then
      begin
        part := Trim(Copy(rest, 1, p - 1));
        rest := Copy(rest, p + 1, MaxInt);
      end
      else
      begin
        part := Trim(rest);
        rest := '';
      end;
      if part = '' then
        Continue;
      superseded := false;
      eq := Pos('=', part);
      if (eq > 1) and (jar <> nil) then
      begin
        namePrefix := Copy(part, 1, eq); // includes '='
        for k := 0 to jar.Count - 1 do
          if Pos(namePrefix, Trim(jar[k])) = 1 then
          begin
            superseded := true;
            Break;
          end;
      end;
      if superseded then
        Continue;
      if Result <> '' then
        Result := Result + '; ';
      Result := Result + part;
    end;
    if jar <> nil then
      for k := 0 to jar.Count - 1 do
      begin
        if Trim(jar[k]) = '' then
          Continue;
        if Result <> '' then
          Result := Result + '; ';
        Result := Result + Trim(jar[k]);
      end;
  end;

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
          UpsertCookie(Result.Cookies, cookieVal);
          UpsertCookie(cookieJar, cookieVal);
        end;
      end;
    end;
  end;

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

  function ResolveUrlLocal(const baseUrl, location: string): string;
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

begin
  Result.Body := '';
  Result.Headers := TStringList.Create;
  Result.Cookies := TStringList.Create;
  Result.StatusCode := 0;
  Result.FinalURL := '';
  Result.RedirectCount := 0;
  Result.Success := false;
  Result.ErrorMessage := '';

  if prefix then
    currentUrl := Format('%s/%s', [baseUrl, endpoint])
  else
    currentUrl := endpoint;

  if (not post) and (Length(params) > 0) then
  begin
    currentUrl := currentUrl + '?' + params[0];
    for status := 1 to High(params) do
      currentUrl := currentUrl + '&' + params[status];
  end;

  requestHeaders := TStringList.Create;
  if customHeaders <> nil then
  begin
    requestHeaders.Assign(customHeaders);
    // Normalize "Name: value" lines to Name=Value pairs (TNSHTTPSendAndReceive
    // uses TStringList.Names/ValueFromIndex).
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

  // A Cookie header supplied by the caller is kept as the base; jar cookies
  // are appended to it on every hop.
  baseCookie := requestHeaders.Values['Cookie'];

  sendStream := TStringStream.Create('');
  isPost := post;
  if jsondata <> '' then
  begin
    isPost := true;
    sendStream.WriteString(jsondata);
    requestHeaders.Values['Content-Type'] := 'application/json';
    requestHeaders.Values['Content-Length'] := IntToStr(sendStream.Size);
  end
  else if isPost and (Length(params) > 0) then
  begin
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
      // Replay cookies on every hop (the transport never sends any on its
      // own): the caller's jar if one was passed, otherwise whatever this
      // redirect chain has collected so far.
      if cookieJar <> nil then
        jarRef := cookieJar
      else
        jarRef := Result.Cookies;
      cookieHeader := BuildCookieHeaderLocal(baseCookie, jarRef);
      if cookieHeader <> '' then
        requestHeaders.Values['Cookie'] := cookieHeader;

      respStream := TStringStream.Create('');
      responseHeaders := TStringList.Create;
      httpClient := TNSHTTPSendAndReceive.Create;
      try
        httpClient.address := currentUrl;
        if isPost then
          httpClient.method := 'POST'
        else
          httpClient.method := 'GET';

        if not httpClient.SendAndReceiveEx(sendStream, respStream, requestHeaders,
             responseHeaders, status, currentUrl) then
        begin
          Result.ErrorMessage := httpClient.LastErrMsg;
          Exit;
        end;

        Result.Body := Trim(respStream.DataString);
        Result.StatusCode := status;
        Result.Headers.Assign(responseHeaders);
        Result.FinalURL := currentUrl;

        UpdateCookiesFromHeadersLocal(responseHeaders);

        if not followRedirects then
          Break;

        if not ((Result.StatusCode = 301) or (Result.StatusCode = 302) or
                (Result.StatusCode = 303) or (Result.StatusCode = 307) or
                (Result.StatusCode = 308)) then
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

{------------------------------------------------------------------------------
  PlaySound (macOS)
  -----------------
  Spawn afplay for a validated audio file. Fire-and-forget via the async
  worker so the child is reaped instead of lingering as a zombie.
 ------------------------------------------------------------------------------}
class procedure TTrndiNativeMac.PlaySound(const FileName: string);
begin
  if not IsValidAudioFile(FileName) then
    Exit;
  RunAndCaptureSimpleAsync('/usr/bin/afplay', [FileName], nil);
end;

{------------------------------------------------------------------------------
  GetOSLanguage (macOS)
  ---------------------
  Prefer the system preferred language list; fall back to current locale id.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.GetOSLanguage: string;

  function NormalizeLang(const s: string): string;
  var
    v: string;
    p: SizeInt;
  begin
    v := Trim(s);
    v := StringReplace(v, '-', '_', [rfReplaceAll]);
    p := Pos('.', v);
    if p > 0 then
      v := Copy(v, 1, p - 1);
    p := Pos('@', v);
    if p > 0 then
      v := Copy(v, 1, p - 1);
    p := Pos('_', v);
    if p > 0 then
      v := Copy(v, 1, p - 1);
    Result := LowerCase(Trim(v));
  end;

begin
  Result := '';
  if (NSLocale.preferredLanguages <> nil) and (NSLocale.preferredLanguages.count > 0) then
    Result := UTF8Encode(NSString(NSLocale.preferredLanguages.objectAtIndex(0)).utf8string);
  if Result = '' then
    Result := UTF8Encode(NSLocale.currentLocale.localeIdentifier.utf8string);
  Result := NormalizeLang(Result);
end;

{------------------------------------------------------------------------------
  Wake-from-sleep notification (macOS)
  ------------------------------------
  Subclass NSObject to expose a selector NSWorkspace's notification center
  can invoke when NSWorkspaceDidWakeNotification fires. The selector posts
  to the main thread via Application.QueueAsyncCall so the user callback
  runs on the UI thread regardless of which thread NSWorkspace dispatches
  from.
 ------------------------------------------------------------------------------}
type
  TWakeBridge = class
    Callback: TTrndiWakeCallback;
    Pending: boolean;
    procedure Fire(Data: PtrInt);
  end;

  TWakeObserver = objcclass(NSObject)
    procedure systemDidWake(notification: NSNotification); message 'systemDidWake:';
  end;

var
  gWakeObserver: TWakeObserver = nil;
  gWakeBridge: TWakeBridge = nil;

procedure TWakeBridge.Fire(Data: PtrInt);
begin
  Pending := false;
  if Assigned(Callback) then
    try
      Callback();
    except
      // Swallow — never let user code crash the run loop
    end;
end;

procedure TWakeObserver.systemDidWake(notification: NSNotification);
begin
  if Assigned(gWakeBridge) and (not gWakeBridge.Pending) then
  begin
    gWakeBridge.Pending := true;
    Application.QueueAsyncCall(@gWakeBridge.Fire, 0);
  end;
end;

procedure TTrndiNativeMac.RegisterWakeCallback(const Callback: TTrndiWakeCallback);
begin
  inherited RegisterWakeCallback(Callback);
  if gWakeBridge = nil then
    gWakeBridge := TWakeBridge.Create;
  gWakeBridge.Callback := Callback;
  if not Assigned(Callback) then
  begin
    UnregisterWakeCallback;
    Exit;
  end;
  if gWakeObserver = nil then
  begin
    gWakeObserver := TWakeObserver.alloc.init;
    NSWorkspace.sharedWorkspace.notificationCenter.addObserver_selector_name_object(
      gWakeObserver,
      objcselector('systemDidWake:'),
      NSWorkspaceDidWakeNotification,
      nil);
  end;
end;

procedure TTrndiNativeMac.UnregisterWakeCallback;
begin
  if gWakeObserver <> nil then
  begin
    NSWorkspace.sharedWorkspace.notificationCenter.removeObserver(gWakeObserver);
    gWakeObserver.release;
    gWakeObserver := nil;
  end;
  if Assigned(gWakeBridge) then
    gWakeBridge.Callback := nil;
  inherited UnregisterWakeCallback;
end;

end.
