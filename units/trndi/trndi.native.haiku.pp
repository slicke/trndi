unit trndi.native.haiku;

{**
  @abstract(Haiku-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeHaiku) which derives from
  @link(TTrndiNativeGeneric) — the portable INI + TFPHTTPClient
  implementation — and overrides only what is genuinely Haiku:
  - Text-to-speech via espeak (common on Haiku)
  - Notifications via Haiku's `notify` (or notify-send if present)
  - Settings stored under ~/config/settings/Trndi per Haiku conventions

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Graphics, trndi.native.generic;

type
  {!
    @abstract(Haiku implementation of @link(TTrndiNativeGeneric).)
    Uses espeak for speech; settings and HTTP come from the generic base.
  }
TTrndiNativeHaiku = class(TTrndiNativeGeneric)
protected
    {** Resolve the INI file path for Haiku.
        Uses ~/config/settings/Trndi/trndi.ini following Haiku conventions }
  function ResolveIniPath: string; override;
public
  {** Use the detected notifier (notify/notify-send) for notifications. }
  procedure attention(topic, message: string); override;
    {** Speaks @param(Text) using espeak, if available. }
  procedure Speak(const Text: string); override;
    {** Haiku badge support - placeholder for now. }
  procedure setBadge(const Value: string; BadgeColor: TColor;
    badge_size_ratio: double; min_font_size: integer); override;
    {** Returns True if a notification helper is available on this system. }
  class function isNotificationSystemAvailable: boolean; override;
    {** Identify notification backend: 'notify', 'notify-send' or 'none'. }
  class function getNotificationSystem: string; override;
  {** Check whether platform TTS is available (espeak on Haiku). }
  class function SpeakAvailable: boolean; override;
  {** Name of required software for TTS on this platform (e.g., 'espeak'). }
  class function SpeakSoftwareName: string; override;
  {** Window manager name for Haiku. }
  class function GetWindowManagerName: string; override;
  {** Placeholder for dark mode toggling on Haiku. }
  class function setDarkMode: boolean;
end;

implementation

uses
Process;

{------------------------------------------------------------------------------
  FindNotifyCmd
  -------------
  Probe PATH for a notification helper. Prefer 'notify' (Haiku) then
  'notify-send' (common on Linux). Returns the executable name or '' if none.
 ------------------------------------------------------------------------------}
function FindNotifyCmd: string;
begin
  if TTrndiNativeHaiku.ToolAvailable('notify') then
    Exit('notify');
  if TTrndiNativeHaiku.ToolAvailable('notify-send') then
    Exit('notify-send');
  Result := '';
end;

{------------------------------------------------------------------------------
  ResolveIniPath
  --------------
  Returns the path to the INI file following Haiku conventions.
  Haiku uses ~/config/settings/ for application settings.
 ------------------------------------------------------------------------------}
function TTrndiNativeHaiku.ResolveIniPath: string;
var
  ConfigDir: string;
begin
  ConfigDir := GetEnvironmentVariable('HOME');
  if ConfigDir = '' then
    ConfigDir := '/boot/home';

  ConfigDir := IncludeTrailingPathDelimiter(ConfigDir) + 'config/settings/Trndi';

  if not DirectoryExists(ConfigDir) then
    ForceDirectories(ConfigDir);

  Result := IncludeTrailingPathDelimiter(ConfigDir) + 'trndi.ini';
end;

{------------------------------------------------------------------------------
  attention
  ---------
  Use the detected notifier. Supports Haiku's `notify --title` syntax and
  `notify-send`. The icon parameter is optional and will be used if an icon
  file is found in common locations. Falls back to TTS or dialog when no
  notifier is available.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.attention(topic, message: string);
var
  AProcess: TProcess;
  cmd: string;
  iconPath: string;
  cfgDir: string;
begin
  cmd := FindNotifyCmd;

  // If no notifier exists, do nothing (Haiku systems have 'notify')
  if cmd = '' then
    Exit;

  // Try to locate an optional icon in common locations
  iconPath := '';
  cfgDir := ExtractFileDir(ResolveIniPath);
  if FileExists(IncludeTrailingPathDelimiter(cfgDir) + 'trndi.png') then
    iconPath := IncludeTrailingPathDelimiter(cfgDir) + 'trndi.png'
  else if FileExists(IncludeTrailingPathDelimiter(cfgDir) + 'trndi-icon.png') then
    iconPath := IncludeTrailingPathDelimiter(cfgDir) + 'trndi-icon.png'
  else if FileExists(ExtractFileDir(ParamStr(0)) + PathDelim + 'trndi.png') then
    iconPath := ExtractFileDir(ParamStr(0)) + PathDelim + 'trndi.png'
  else if FileExists('/boot/system/apps/Trndi/icon.png') then
    iconPath := '/boot/system/apps/Trndi/icon.png';

  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := cmd;

    if cmd = 'notify' then
    begin
      // Haiku syntax: notify --icon icon.png --title "Title" "Message"
      if iconPath <> '' then
      begin
        AProcess.Parameters.Add('--icon');
        AProcess.Parameters.Add(iconPath);
      end;
      AProcess.Parameters.Add('--title');
      AProcess.Parameters.Add(topic);
      AProcess.Parameters.Add(message);
    end
    else // notify-send
    begin
      if iconPath <> '' then
      begin
        AProcess.Parameters.Add('--icon');
        AProcess.Parameters.Add(iconPath);
      end;
      // notify-send <summary> <body>
      AProcess.Parameters.Add(topic);
      AProcess.Parameters.Add(message);
    end;

    AProcess.Options := [poNoConsole];
    try
      AProcess.Execute;
    except
      // Silently fail if notification doesn't work
    end;
  finally
    AProcess.Free;
  end;
end;

{------------------------------------------------------------------------------
  Speak
  -----
  Use espeak for text-to-speech on Haiku asynchronously.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.Speak(const Text: string);
var
  AProcess: TProcess;
begin
  if not SpeakAvailable then
    Exit;

  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'espeak';
    AProcess.Parameters.Add(Text);
    AProcess.Options := [poNoConsole];
    try
      AProcess.Execute;
    except
      // Silently fail if speech doesn't work
    end;
  except
    on E: Exception do
    begin
      // Silently fail
      AProcess.Free;
      Exit;
    end;
  end;
  // Async process will be cleaned up by OS
end;

{------------------------------------------------------------------------------
  setBadge
  --------
  Placeholder for Haiku badge support.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.setBadge(const Value: string; BadgeColor: TColor;
  badge_size_ratio: double; min_font_size: integer);
begin
  // No badge support for Haiku yet
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable
  ------------------------------
  Check if a notification helper is available (notify or notify-send).
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.isNotificationSystemAvailable: boolean;
begin
  Result := FindNotifyCmd <> '';
end;

{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Return the notification system in use.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.getNotificationSystem: string;
var
  cmd: string;
begin
  cmd := FindNotifyCmd;
  if cmd <> '' then
    Result := cmd
  else
    Result := 'none';
end;

{------------------------------------------------------------------------------
  SpeakAvailable
  --------------
  Check if espeak is available.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.SpeakAvailable: boolean;
begin
  Result := ToolAvailable('espeak');
end;

{------------------------------------------------------------------------------
  SpeakSoftwareName
  -----------------
  Return the name of the TTS software.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.SpeakSoftwareName: string;
begin
  Result := 'espeak';
end;

{------------------------------------------------------------------------------
  GetWindowManagerName
  --------------------
  Return Haiku's window manager name.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.GetWindowManagerName: string;
begin
  Result := 'Haiku Desktop';
end;

{------------------------------------------------------------------------------
  setDarkMode
  -----------
  Placeholder for dark mode toggling on Haiku.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.setDarkMode: boolean;
begin
  // No-op placeholder for Haiku
  Result := false;
end;

end.
