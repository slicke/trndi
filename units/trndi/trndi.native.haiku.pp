unit trndi.native.haiku;

{**
  @abstract(Haiku-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeHaiku) which derives from
  @link(TTrndiNativeBase) and implements behaviors using Haiku-specific tools
  and FPC facilities.

  Responsibilities include:
  - Text-to-speech via espeak (common on Haiku)
  - Notifications via notify-send (if available)
  - HTTP requests via TFPHTTPClient
  - Settings persisted in INI files

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Graphics, IniFiles, Dialogs,
ExtCtrls, Forms, Math, trndi.native.base, FileUtil,
fphttpclient, opensslsockets, DateUtils;

type
  {!
    @abstract(Haiku implementation of @link(TTrndiNativeBase).)
    Uses espeak for speech and TFPHTTPClient for web requests.
  }
TTrndiNativeHaiku = class(TTrndiNativeBase)
protected
  inistore: TIniFile; // Haiku-specific settings store
    {** Resolve the INI file path for Haiku.
        Uses ~/config/settings/Trndi/trndi.ini following Haiku conventions }
  function ResolveIniPath: string; virtual;
    {** Ensure the INI store is created and directory exists. }
  procedure EnsureIni; inline;
public
  {** Use notify-send for notifications if available. }
  procedure attention(topic, message: string); override;
    {** Free resources and close INI store. }
  destructor Destroy; override;
    {** Speaks @param(Text) using espeak, if available. }
  procedure Speak(const Text: string); override;
    {** Haiku badge support - placeholder for now. }
  procedure setBadge(const Value: string; BadgeColor: TColor;
    badge_size_ratio: double; min_font_size: integer); override;

    // Settings API overrides
    {** Read setting from INI. }
  function GetSetting(const keyname: string; def: string = '';
    global: boolean = false): string; override;
    {** Write setting to INI and flush to disk. }
  procedure SetSetting(const keyname: string; const val: string;
    global: boolean = false); override;
    {** Delete setting from INI. }
  procedure DeleteSetting(const keyname: string; global: boolean = false); override;
    {** Drop INI handle; re-created on demand. }
  procedure ReloadSettings; override;
  {** HTTP GET using TFPHTTPClient.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
  class function getURL(const url: string; out res: string): boolean; override;
  {** Dark mode detection for Haiku - uses base heuristic for now. }
  class function isDarkMode: boolean; override;
    {** Returns True if notify-send is available on this system. }
  class function isNotificationSystemAvailable: boolean; override;
    {** Identify notification backend: 'notify-send' or 'none'. }
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
Process, LCLType;

{------------------------------------------------------------------------------
  IsNotifySendAvailable
  ---------------------
  Check PATH for the 'notify-send' tool. Returns True if found.
 ------------------------------------------------------------------------------}
function IsNotifySendAvailable: boolean;
var
  AProcess: TProcess;
  OutputLines: TStringList;
begin
  Result := false;
  AProcess := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    AProcess.Executable := 'which';
    AProcess.Parameters.Add('notify-send');
    AProcess.Options := [poUsePipes, poWaitOnExit, poNoConsole];
    try
      AProcess.Execute;
      Result := AProcess.ExitStatus = 0;
    except
      Result := false;
    end;
  finally
    OutputLines.Free;
    AProcess.Free;
  end;
end;

{------------------------------------------------------------------------------
  IsEspeakAvailable
  -----------------
  Check if espeak is available on the system.
 ------------------------------------------------------------------------------}
function IsEspeakAvailable: boolean;
var
  AProcess: TProcess;
begin
  Result := false;
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'which';
    AProcess.Parameters.Add('espeak');
    AProcess.Options := [poUsePipes, poWaitOnExit, poNoConsole];
    try
      AProcess.Execute;
      Result := AProcess.ExitStatus = 0;
    except
      Result := false;
    end;
  finally
    AProcess.Free;
  end;
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
  EnsureIni
  ---------
  Create INI store if not present.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.EnsureIni;
begin
  if inistore = nil then
    inistore := TIniFile.Create(ResolveIniPath);
end;

{------------------------------------------------------------------------------
  Destroy
  -------
  Clean up INI store.
 ------------------------------------------------------------------------------}
destructor TTrndiNativeHaiku.Destroy;
begin
  if inistore <> nil then
    FreeAndNil(inistore);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  attention
  ---------
  Show a notification using notify-send if available.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.attention(topic, message: string);
var
  AProcess: TProcess;
begin
  if isNotificationSystemAvailable then
  begin
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'notify-send';
      AProcess.Parameters.Add(topic);
      AProcess.Parameters.Add(message);
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
end;

{------------------------------------------------------------------------------
  Speak
  -----
  Use espeak for text-to-speech on Haiku.
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
  finally
    AProcess.Free;
  end;
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
  GetSetting
  ----------
  Read a setting from INI file.
 ------------------------------------------------------------------------------}
function TTrndiNativeHaiku.GetSetting(const keyname: string; def: string = '';
  global: boolean = false): string;
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  Result := inistore.ReadString('trndi', key, def);
end;

{------------------------------------------------------------------------------
  SetSetting
  ----------
  Write a setting to INI file.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.SetSetting(const keyname: string; const val: string;
  global: boolean = false);
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  inistore.WriteString('trndi', key, val);
  inistore.UpdateFile;
end;

{------------------------------------------------------------------------------
  DeleteSetting
  -------------
  Delete a setting from INI file.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.DeleteSetting(const keyname: string;
  global: boolean = false);
var
  key: string;
begin
  EnsureIni;
  key := buildKey(keyname, global);
  inistore.DeleteKey('trndi', key);
  inistore.UpdateFile;
end;

{------------------------------------------------------------------------------
  ReloadSettings
  --------------
  Drop and recreate INI store.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.ReloadSettings;
begin
  if inistore <> nil then
    FreeAndNil(inistore);
end;

{------------------------------------------------------------------------------
  getURL
  ------
  HTTP GET using TFPHTTPClient.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.getURL(const url: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
begin
  Result := false;
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.AllowRedirect := true;
    try
      res := HTTP.Get(url);
      Result := true;
    except
      on E: Exception do
      begin
        res := 'Error: ' + E.Message;
        Result := false;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

{------------------------------------------------------------------------------
  isDarkMode
  ----------
  Dark mode detection - uses base heuristic for now.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.isDarkMode: boolean;
begin
  // Use base class heuristic for now
  Result := inherited isDarkMode;
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable
  ------------------------------
  Check if notify-send is available.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.isNotificationSystemAvailable: boolean;
begin
  Result := IsNotifySendAvailable;
end;

{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Return the notification system in use.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.getNotificationSystem: string;
begin
  if IsNotifySendAvailable then
    Result := 'notify-send'
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
  Result := IsEspeakAvailable;
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
