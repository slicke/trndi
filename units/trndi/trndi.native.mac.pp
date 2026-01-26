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
  Classes, SysUtils, Graphics, NSMisc, ns_url_request, CocoaAll, SimpleDarkMode,
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

// Typed imports of objc_msgSend with a few arities we need
function objc_msgSend0(obj: id; sel: SEL): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend1(obj: id; sel: SEL; p1: id): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend2_d_b(obj: id; sel: SEL; p1: double; p2: boolean): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend3(obj: id; sel: SEL; p1: id; p2: id; p3: id): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend2_id_id(obj: id; sel: SEL; p1: id; p2: id): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_getClass(name: MarshaledAString): id;        cdecl; external ObjCLib;
function sel_registerName(name: MarshaledAString): SEL;    cdecl; external ObjCLib;

{------------------------------------------------------------------------------
  attention (macOS)
  -----------------
  Try UNUserNotificationCenter -> NSUserNotification -> osascript.
 ------------------------------------------------------------------------------}

procedure TTrndiNativeMac.attention(topic, message: string);
var
  UNClass, Center, ContentClass, Content, TriggerClass, Trigger, ReqClass, UNReq: id;
  NSReq: NSUserNotification;
  IdStr, TitleStr, BodyStr: NSString;
  selCurrent, selAddReq, selNew, selSetTitle, selSetBody: SEL;
  ok: Boolean;
  sId: string;
begin
  ok := False;
  try
    // Use UNUserNotificationCenter when available (modern API).
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

        // Create a trigger to fire almost immediately
        TriggerClass := objc_getClass('UNTimeIntervalNotificationTrigger');
        Trigger := objc_msgSend2_d_b(TriggerClass, sel_registerName('triggerWithTimeInterval:repeats:'), 1.0, False);

        // Create a unique identifier for the request
        sId := Format('trndi-%d', [DateTimeToUnix(Now)]);
        IdStr := NSSTR(sId);

        // Create request
        ReqClass := objc_getClass('UNNotificationRequest');
        UNReq := objc_msgSend3(ReqClass, sel_registerName('requestWithIdentifier:content:trigger:'), IdStr, Content, Trigger);

        // Release the identifier NSString we created earlier
        IdStr.Release;

        // Add request (no completion handler)
        selAddReq := sel_registerName('addNotificationRequest:withCompletionHandler:');
        objc_msgSend2_id_id(Center, selAddReq, UNReq, nil);

        // Best-effort; do not raise on failures
        ok := True;
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
  Use the built-in 'say' tool to speak text (synchronous).
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.Speak(const Text: string);
var
  o: string;
begin
  // Use the built-in macOS speech synthesis
  // Note: This is synchronous; consider running in a background process
  // if you need to keep the UI responsive during long messages.
  RunCommand('/usr/bin/say', [Text], o);
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
begin
  res := '';
  send := TStringStream.Create('');
  response := TStringStream.Create('');
  headers := TStringList.Create;
  httpClient := TNSHTTPSendAndReceive.Create;
  try
    try
      httpClient.address := url;
      httpClient.method := 'GET';
      headers.Add('User-Agent=' + DEFAULT_USER_AGENT);

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
  SimpleDarkMode.EnableAppDarkMode;
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
  // declare a typed call that accepts an options mask and an object (block)
  function objc_msgSend_uint_id(obj: id; sel: SEL; p1: NativeUInt; p2: id): id; cdecl; external ObjCLib name 'objc_msgSend';
begin
  inherited start;
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

end.
