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
fphttpclient, opensslsockets, DateUtils{$ifdef DEBUG}, trndi.log{$endif};

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
    {** Export all settings to INI format string. }
  function ExportSettings: string; override;
    {** Import settings from INI format string. }
  procedure ImportSettings(const iniData: string); override;
  {** HTTP GET using TFPHTTPClient.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
  class function getURL(const url: string; out res: string): boolean; override;
  {** Test an HTTP GET through an explicit proxy only (no direct fallback). }
  class function TestProxyURL(const url: string; const proxyHost: string;
    const proxyPort: string; const proxyUser: string; const proxyPass: string;
    out res: string): boolean; override;
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
  FindNotifyCmd / IsNotifyAvailable
  ---------------------------------
  Probe PATH for a notification helper. Prefer 'notify' (Haiku) then
  'notify-send' (common on Linux). Returns the executable name or '' if none.
 ------------------------------------------------------------------------------}
function FindNotifyCmd: string;
var
  AProcess: TProcess;
begin
  Result := '';

  // Check for 'notify' first (Haiku style)
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'which';
    AProcess.Parameters.Add('notify');
    AProcess.Options := [poUsePipes, poWaitOnExit, poNoConsole];
    try
      AProcess.Execute;
      if AProcess.ExitStatus = 0 then
        Exit('notify');
    except
      // ignore
    end;
  finally
    AProcess.Free;
  end;

  // Fallback to notify-send
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'which';
    AProcess.Parameters.Add('notify-send');
    AProcess.Options := [poUsePipes, poWaitOnExit, poNoConsole];
    try
      AProcess.Execute;
      if AProcess.ExitStatus = 0 then
        Exit('notify-send');
    except
      // ignore
    end;
  finally
    AProcess.Free;
  end;
end;

function IsNotifyAvailable: boolean;
begin
  Result := FindNotifyCmd <> '';
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
  ExportSettings
  --------------
  Export all settings from the INI file to a string.
 ------------------------------------------------------------------------------}
function TTrndiNativeHaiku.ExportSettings: string;
var
  sl: TStringList;
  sections, keys: TStringList;
  i, j: integer;
  section, key, value: string;
begin
  if inistore = nil then
    inistore := TIniFile.Create(ResolveIniPath);
  sl := TStringList.Create;
  sections := TStringList.Create;
  keys := TStringList.Create;
  try
    inistore.ReadSections(sections);
    for i := 0 to sections.Count - 1 do
    begin
      section := sections[i];
      sl.Add('[' + section + ']');
      inistore.ReadSection(section, keys);
      for j := 0 to keys.Count - 1 do
      begin
        key := keys[j];
        value := inistore.ReadString(section, key, '');
        sl.Add(key + '=' + value);
      end;
      if i < sections.Count - 1 then
        sl.Add(''); // Add blank line between sections
    end;
    Result := sl.Text;
  finally
    keys.Free;
    sections.Free;
    sl.Free;
  end;
end;

{------------------------------------------------------------------------------
  ImportSettings
  ---------------
  Import settings from INI format string.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeHaiku.ImportSettings(const iniData: string);
var
  sl: TStringList;
  mem: TMemoryStream;
  ini: TMemIniFile;
  sections, keys: TStringList;
  i, j: integer;
  section, key, value: string;
begin
  if inistore = nil then
    inistore := TIniFile.Create(ResolveIniPath);
  sl := TStringList.Create;
  mem := TMemoryStream.Create;
  ini := nil;
  sections := TStringList.Create;
  keys := TStringList.Create;
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
        inistore.WriteString(section, key, value);
      end;
    end;
    inistore.UpdateFile;
  finally
    keys.Free;
    sections.Free;
    ini.Free;
    mem.Free;
    sl.Free;
  end;
end;

{------------------------------------------------------------------------------
  getURL
  ------
  HTTP GET using TFPHTTPClient.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.getURL(const url: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
  tempInstance: TTrndiNativeHaiku;
  proxyHost, proxyPort, proxyUser, proxyPass: string;

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

  function PerformRequest(withProxy: boolean): boolean;
  begin
    Result := false;
    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.AllowRedirect := true;
      HTTP.IOTimeout := 30000; // 30 seconds timeout

      // Set proxy if configured and requested
      if withProxy and (proxyHost <> '') then
      begin
        HTTP.Proxy.Host := proxyHost;
        HTTP.Proxy.Port := StrToIntDef(proxyPort, 8080);
        if proxyUser <> '' then
          HTTP.Proxy.Username := proxyUser;
        if proxyPass <> '' then
          HTTP.Proxy.Password := proxyPass;
      end;

      // Ensure a true direct attempt when falling back
      if (not withProxy) and (proxyHost <> '') then
      begin
        HTTP.Proxy.Host := '';
        HTTP.Proxy.Port := 0;
      end;

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

begin
  Result := false;
  tempInstance := TTrndiNativeHaiku.Create;
  try
    // Check for proxy settings
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
      LogMessageToFile(Format('HTTP GET: proxy configured (%s:%s); url=%s', [proxyHost, proxyPort, SafeUrlForLog(url)]))
    else
      LogMessageToFile(Format('HTTP GET: no proxy configured; url=%s', [SafeUrlForLog(url)]));
    {$endif}

    // Try with proxy first if configured
    if proxyHost <> '' then
    begin
      {$ifdef DEBUG}
      LogMessageToFile(Format('HTTP GET: attempting via proxy %s:%s', [proxyHost, proxyPort]));
      {$endif}
      if PerformRequest(true) then
      begin
        {$ifdef DEBUG}
        LogMessageToFile('HTTP GET: proxy attempt succeeded');
        {$endif}
        Result := true;
        Exit;
      end;

      {$ifdef DEBUG}
      LogMessageToFile('HTTP GET: proxy attempt failed: ' + res + ' ; retrying direct');
      {$endif}
    end;

    // Fallback: try without proxy
    {$ifdef DEBUG}
    if proxyHost <> '' then
      LogMessageToFile('HTTP GET: attempting direct (clearing proxy settings)')
    else
      LogMessageToFile('HTTP GET: attempting direct');
    {$endif}
    if PerformRequest(false) then
    begin
      {$ifdef DEBUG}
      LogMessageToFile('HTTP GET: direct attempt succeeded');
      {$endif}
      Result := true;
    end
    else
    begin
      {$ifdef DEBUG}
      LogMessageToFile('HTTP GET: direct attempt failed: ' + res);
      {$endif}
      Result := false;
    end;

  finally
    tempInstance.Free;
  end;
end;

{------------------------------------------------------------------------------
  TestProxyURL
  ------------
  Proxy-only HTTP GET using TFPHTTPClient. No direct fallback.
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.TestProxyURL(const url: string;
  const proxyHost: string; const proxyPort: string; const proxyUser: string;
  const proxyPass: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
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

  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.AllowRedirect := true;
    HTTP.IOTimeout := 30000;
    HTTP.Proxy.Host := host;
    HTTP.Proxy.Port := StrToIntDef(portS, 8080);
    if user <> '' then
      HTTP.Proxy.Username := user;
    if pass <> '' then
      HTTP.Proxy.Password := pass;

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
  Check if a notification helper is available (notify or notify-send).
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.isNotificationSystemAvailable: boolean;
begin
  Result := IsNotifyAvailable;
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
