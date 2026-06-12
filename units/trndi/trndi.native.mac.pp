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
  nsutils.nshelpers, IniFiles, dialogs,
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
        Note: This call blocks until speech completes; dispatch from a
        background thread if you need non-blocking UI. }
    procedure Speak(const Text: string); override;
    {** Enable dark appearance for the app UI via SimpleDarkMode.
        @returns(True once the request is made) }
    class function setDarkMode: boolean;

    // Settings API overrides (NSUserDefaults/CFPreferences)
    {** Read a string from preferences; returns @param(def) when missing.
        Keys are scoped by @link(TTrndiNativeBase.buildKey). }
    function GetSetting(const keyname: string; def: string = '';
      global: boolean = False): string; override;
    {** Write a string to preferences under the scoped key. }
    procedure SetSetting(const keyname: string; const val: string;
      global: boolean = False); override;
    {** Delete a setting (sets to empty string as some backends lack delete). }
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
  end;

implementation

uses
  Process, DateUtils;

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
  if Result then
    CachedHasBundleIdentifier := 1
  else
    CachedHasBundleIdentifier := 0;
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
 ------------------------------------------------------------------------------}

procedure TTrndiNativeMac.attention(topic, message: string);
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
    if TrndiHasBundleIdentifier then
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

          TitleStr := NSSTR(topic);
          BodyStr := NSSTR(message);
          selSetTitle := sel_registerName('setTitle:');
          selSetBody := sel_registerName('setBody:');
          objc_msgSend1(Content, selSetTitle, TitleStr);
          objc_msgSend1(Content, selSetBody, BodyStr);
          TitleStr.Release;
          BodyStr.Release;

          // Default sound — without this the alert is silent for users who
          // haven't explicitly enabled sound for the app.
          SoundClass := objc_getClass('UNNotificationSound');
          if SoundClass <> nil then
          begin
            Sound := objc_msgSend0(SoundClass, sel_registerName('defaultSound'));
            if Sound <> nil then
              objc_msgSend1(Content, sel_registerName('setSound:'), Sound);
          end;

          // Time-sensitive: pierces Focus / DND for glucose alerts.
          objc_msgSend1_ni(Content, sel_registerName('setInterruptionLevel:'),
            UNNotificationInterruptionLevelTimeSensitive);

          // Group alerts of the same kind together; topic is the natural key
          // (HIGH/LOW/sensor-fault all have distinct titles).
          ThreadStr := NSSTR(topic);
          objc_msgSend1(Content, sel_registerName('setThreadIdentifier:'), ThreadStr);
          ThreadStr.Release;

          // Category — placeholder for future actions (Snooze/Dismiss).
          CategoryStr := NSSTR('trndi.alert');
          objc_msgSend1(Content, sel_registerName('setCategoryIdentifier:'), CategoryStr);
          CategoryStr.Release;

          // Stable identifier per topic: posting again with the same id
          // *replaces* the previous notification rather than stacking. For a
          // glucose monitor this is medically appropriate — the newest
          // reading is what matters.
          sId := 'trndi.alert.' + topic;
          IdStr := NSSTR(sId);

          // Create request with nil trigger -> deliver immediately.
          ReqClass := objc_getClass('UNNotificationRequest');
          UNReq := objc_msgSend3(ReqClass, sel_registerName('requestWithIdentifier:content:trigger:'), IdStr, Content, nil);

          // Release the identifier NSString we created earlier
          IdStr.Release;

          // Add request (no completion handler)
          selAddReq := sel_registerName('addNotificationRequest:withCompletionHandler:');
          objc_msgSend2_id_id(Center, selAddReq, UNReq, nil);

          // Best-effort; do not raise on failures
          ok := True;
        end;
      end;
    end;
  except
    // Ignore and fall back.
  end;

  if ok then Exit;

  // Fallback to deprecated NSUserNotification implementation
  try
    Center := NSUserNotificationCenter.defaultUserNotificationCenter;
    if Center <> nil then
    begin
      NSReq := NSUserNotification.alloc.init;
      try
        TitleStr := NSSTR(topic);
        BodyStr := NSSTR(message);
        try
          NSReq.setTitle(TitleStr);
          NSReq.setInformativeText(BodyStr);
        finally
          TitleStr.Release;
          BodyStr.Release;
        end;

        objc_msgSend1(Center, sel_registerName('deliverNotification:'), NSReq);
        Exit;
      finally
        NSReq.Release;
      end;
    end;
  except
    // Ignore and fall back.
  end;

  inherited attention(topic, message);
end;
{------------------------------------------------------------------------------
  Speak
  -----
  Use the built-in 'say' tool to speak text asynchronously.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.Speak(const Text: string);
var
  Proc: TProcess;
  VoiceName: string;
begin
  // Use the built-in macOS speech synthesis asynchronously
  Proc := TProcess.Create(nil);
  try
    try
      Proc.Executable := '/usr/bin/say';
      
      // Check if a specific voice is selected
      VoiceName := GetSetting('tts.voice.name', '');
      if VoiceName <> '' then
      begin
        Proc.Parameters.Add('-v');
        Proc.Parameters.Add(VoiceName);
      end;
      
      Proc.Parameters.Add(Text);
      Proc.Options := [];
      Proc.Execute;
    except
      on E: Exception do
      begin
        if not ttsErrorShown then
        begin
          ShowMessage('TTS Error: ' + E.Message);
          ttsErrorShown := true;
        end;
        Exit;
      end;
    end;
  finally
    Proc.Free; // ensure Proc is freed on both success and error paths
  end;
  // Async process will be cleaned up by OS
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
  start
  -----
  Best-effort request for notification authorization on startup (no-op if
  UserNotifications framework is not present). We don't wait for the
  completion handler to avoid blocking startup; this is a prompt-triggering
  best-effort call.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.start;
var
  UNClass, Center: id;
  selRequest: SEL;
begin
  inherited start;
  // UNUserNotificationCenter asserts if there is no bundle identifier
  // (e.g. running the raw binary outside a .app bundle).
  if not TrndiHasBundleIdentifier then Exit;
  try
    UNClass := objc_getClass('UNUserNotificationCenter');
    if UNClass = nil then Exit;
    Center := objc_msgSend0(UNClass, sel_registerName('currentNotificationCenter'));
    if Center = nil then Exit;

    selRequest := sel_registerName('requestAuthorizationWithOptions:completionHandler:');
    // UNAuthorizationOptionAlert|Badge|Sound is (1<<0)|(1<<1)|(1<<2) == 7
    // We pass nil for completion handler (best-effort)
    objc_msgSend_uint_id(Center, selRequest, 7, nil);
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
begin
  if Value = '' then
  begin
    NSApp.dockTile.setBadgeLabel(nil);
    Exit;
  end;
  NSS := NSSTR(Value);
  NSApp.dockTile.setBadgeLabel(NSS);
  NSS.Release;
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
  GetSetting / SetSetting / DeleteSetting / ReloadSettings
  -------------------------------------------------------
  Preferences-backed settings: read, write (no-op delete), and no reload.
 ------------------------------------------------------------------------------}
function TTrndiNativeMac.GetSetting(const keyname: string; def: string;
  global: boolean): string;
var
  key: string;
begin
  key := buildKey(keyname, global);
  Result := GetPrefString(key);
  if Result = '' then
    Result := def;
end;

procedure TTrndiNativeMac.SetSetting(const keyname: string; const val: string;
  global: boolean);
var
  key: string;
begin
  key := buildKey(keyname, global);
  SetPrefString(key, val);
end;

procedure TTrndiNativeMac.DeleteSetting(const keyname: string; global: boolean);
begin
  // No direct delete helper; set to empty string for now
  SetSetting(keyname, '', global);
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
  sl := TStringList.Create;
  try
    defaults := NSUserDefaults.standardUserDefaults;
    dict := defaults.dictionaryRepresentation;
    enumerator := dict.keyEnumerator;
    
    sl.Add('[trndi]');
    key := enumerator.nextObject;
    while key <> nil do
    begin
      keyStr := NSStrToStr(key);
      // Exclude system namespaces (Apple/NS preferences and macOS internals).
      // Do NOT filter on case: cfguser-prefixed keys can be mixed-case
      // (e.g. "Bjorn_api.host") and would otherwise be silently dropped.
      if (Pos('Apple', keyStr) <> 1)
        and (Pos('com.apple', keyStr) <> 1)
        and (Pos('NS', keyStr) <> 1) then
      begin
        value := dict.objectForKey(key);
        if value.isKindOfClass(objc_getClass('NSString')) then
          sl.Add(keyStr + '=' + NSStrToStr(NSString(value)))
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
  sl := TStringList.Create;
  mem := TMemoryStream.Create;
  ini := nil;
  sections := TStringList.Create;
  keys := TStringList.Create;
  defaults := NSUserDefaults.standardUserDefaults;
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
        SetPrefString(key, value);
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

end.
