(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
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
    function GetSetting(const keyname: string; def: string = ''; global: boolean = false): string; override;
    {** Write a string to preferences under the scoped key. }
    procedure SetSetting(const keyname: string; const val: string; global: boolean = false); override;
    {** Delete a setting (sets to empty string as some backends lack delete). }
    procedure DeleteSetting(const keyname: string; global: boolean = false); override;
    {** Preferences are live; nothing to reload. }
    procedure ReloadSettings; override;
    // Badge
  {** Set the dock tile badge label (text only). }
  procedure SetBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
  {** Ignore extra params and delegate to simple overload. }
  procedure SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer); overload; override;
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
  end;

implementation

uses
  Process;
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
        Result := true;
      end
      else
      begin
        // Normalize an error: LastErrMsg usually contains the reason
        res := Trim(httpClient.LastErrMsg);
        Result := false;
      end;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
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
  Result := True; // NSUserNotificationCenter exists
end;

{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Identify the macOS notification backend used by this implementation.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.getNotificationSystem: string;
begin
  Result := 'NSUserNotification';
end;


{------------------------------------------------------------------------------
  setDarkMode
  -----------
  Enable dark appearance for the app using SimpleDarkMode.
 ------------------------------------------------------------------------------}
class function TTrndiNativeMac.setDarkMode: Boolean;
begin
  // Enable dark appearance for the app's UI via SimpleDarkMode
  SimpleDarkMode.EnableAppDarkMode;
  Result := True;
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
  NSS.release;
end;

{------------------------------------------------------------------------------
  SetBadge (full signature)
  -------------------------
  Ignore extra parameters and delegate to the simple overload on macOS.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeMac.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer);
begin
  // Ignore extra params on macOS and delegate to the simple overload
  SetBadge(Value, BadgeColor);
end;

{------------------------------------------------------------------------------
  GetSetting / SetSetting / DeleteSetting / ReloadSettings
  -------------------------------------------------------
  Preferences-backed settings: read, write (no-op delete), and no reload.
 ------------------------------------------------------------------------------}
function TTrndiNativeMac.GetSetting(const keyname: string; def: string; global: boolean): string;
var
  key: string;
begin
  key := buildKey(keyname, global);
  Result := GetPrefString(key);
  if Result = '' then
    Result := def;
end;

procedure TTrndiNativeMac.SetSetting(const keyname: string; const val: string; global: boolean);
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
