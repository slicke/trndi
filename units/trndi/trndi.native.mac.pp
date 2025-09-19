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
    procedure Speak(const Text: string); override;
    {** Enables dark appearance for the application via SimpleDarkMode. }
    class function setDarkMode: boolean;

    // Settings API overrides (NSUserDefaults/CFPreferences)
    function GetSetting(const keyname: string; def: string = ''; global: boolean = false): string; override;
    procedure SetSetting(const keyname: string; const val: string; global: boolean = false); override;
    procedure DeleteSetting(const keyname: string; global: boolean = false); override;
    procedure ReloadSettings; override;
    // Badge
    procedure SetBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
    procedure SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer); overload; override;
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
