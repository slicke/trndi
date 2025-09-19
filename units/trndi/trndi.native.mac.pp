unit trndi.native.mac;

{**
  @abstract(macOS-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeMac) which derives from
  @link(TTrndiNativeBase) and implements:
  - Text-to-speech via the built-in @code(say) command
  - Enabling dark appearance via @code(SimpleDarkMode)

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
    {** Speaks @param(Text) using the system @code(say) command. }
  {** Speak text via the built-in 'say' command. }
  procedure Speak(const Text: string); override;
    {** Enables dark appearance for the application via SimpleDarkMode. }
  {** Enable dark appearance for the app UI. }
  class function setDarkMode: boolean;

    // Settings API overrides (NSUserDefaults/CFPreferences)
    function GetSetting(const keyname: string; def: string = ''; global: boolean = false): string; override;
    procedure SetSetting(const keyname: string; const val: string; global: boolean = false); override;
    procedure DeleteSetting(const keyname: string; global: boolean = false); override;
    procedure ReloadSettings; override;
    // Badge
  {** Set the dock tile badge label (text only). }
  procedure SetBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
  {** Ignore extra params and delegate to simple overload. }
  procedure SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer); overload; override;
  {** Simple HTTP GET using NS HTTP helper with default UA. }
  class function getURL(const url: string; out res: string): boolean; override;
  {** True if AppleInterfaceStyle indicates dark mode. }
  class function isDarkMode: boolean; override;
  {** NSUserNotificationCenter is available on macOS. }
  class function isNotificationSystemAvailable: boolean; override;
  end;

implementation

uses
  Process;

procedure TTrndiNativeMac.Speak(const Text: string);
var
  o: string;
begin
  // Use the built-in macOS speech synthesis
  RunCommand('/usr/bin/say', [Text], o);
end;

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

class function TTrndiNativeMac.isDarkMode: boolean;
begin
  Result := Pos('DARK', UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
end;

class function TTrndiNativeMac.isNotificationSystemAvailable: boolean;
begin
  Result := True; // NSUserNotificationCenter exists
end;


class function TTrndiNativeMac.setDarkMode: Boolean;
begin
  // Enable dark appearance for the app's UI via SimpleDarkMode
  SimpleDarkMode.EnableAppDarkMode;
end;

procedure TTrndiNativeMac.SetBadge(const Value: string; BadgeColor: TColor);
var
  NSS: NSString;
begin
  NSS := NSSTR(Value);
  NSApp.dockTile.setBadgeLabel(NSS);
  NSS.release;
end;

procedure TTrndiNativeMac.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer);
begin
  // Ignore extra params on macOS and delegate to the simple overload
  SetBadge(Value, BadgeColor);
end;

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
