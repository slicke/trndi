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
fphttpclient, opensslsockets, DateUtils, URIParser, trndi.log;

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
  {** Simple HTTP POST using TFPHTTPClient. }
  class function postURL(const url: string; const body: string;
    const contentType: string; out res: string): boolean; override;
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
  {** HTTP GET/POST via TFPHTTPClient. Earlier this fell through to base's
      libcurl path, but Haiku doesn't link libpascurl — this override gives
      Haiku a real implementation using the same client as @link(getURL). }
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''; prefix: boolean = true): string; override;
  {** Enhanced HTTP request via TFPHTTPClient: tracks cookies, follows
      redirects manually, captures response headers. }
  function requestEx(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    cookieJar: TStringList = nil; followRedirects: boolean = true;
    maxRedirects: integer = 10; customHeaders: TStringList = nil;
    prefix: boolean = true): THTTPResponse; override;
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
  if iniData = '' then
    Exit;
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
      TrndiDLog(Format('HTTP GET: proxy configured (%s:%s); url=%s', [proxyHost, proxyPort, SafeUrlForLog(url)]))
    else
      TrndiDLog(Format('HTTP GET: no proxy configured; url=%s', [SafeUrlForLog(url)]));
    {$endif}

    // Try with proxy first if configured
    if proxyHost <> '' then
    begin
      {$ifdef DEBUG}
      TrndiDLog(Format('HTTP GET: attempting via proxy %s:%s', [proxyHost, proxyPort]));
      {$endif}
      if PerformRequest(true) then
      begin
        {$ifdef DEBUG}
        TrndiNetLog('HTTP GET: proxy attempt succeeded');
        {$endif}
        Result := true;
        Exit;
      end;

      {$ifdef DEBUG}
      TrndiNetLog('HTTP GET: proxy attempt failed: ' + res + ' ; retrying direct');
      {$endif}
    end;

    // Fallback: try without proxy
    {$ifdef DEBUG}
    if proxyHost <> '' then
      TrndiNetLog('HTTP GET: attempting direct (clearing proxy settings)')
    else
      TrndiNetLog('HTTP GET: attempting direct');
    {$endif}
    if PerformRequest(false) then
    begin
      {$ifdef DEBUG}
      TrndiNetLog('HTTP GET: direct attempt succeeded');
      {$endif}
      Result := true;
    end
    else
    begin
      {$ifdef DEBUG}
      TrndiNetLog('HTTP GET: direct attempt failed: ' + res);
      {$endif}
      Result := false;
    end;

  finally
    tempInstance.Free;
  end;
end;

{------------------------------------------------------------------------------
  postURL
  -------
  Simple HTTP POST using TFPHTTPClient. Respects the configured proxy with a
  direct fallback on failure (mirrors getURL).
 ------------------------------------------------------------------------------}
class function TTrndiNativeHaiku.postURL(const url: string; const body: string;
  const contentType: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
  tempInstance: TTrndiNativeHaiku;
  proxyHost, proxyPort, proxyUser, proxyPass: string;

  function PerformRequest(withProxy: boolean): boolean;
  var
    bodyStream: TStringStream;
    respStream: TStringStream;
  begin
    Result := false;
    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.AllowRedirect := true;
      HTTP.IOTimeout := 30000;

      if withProxy and (proxyHost <> '') then
      begin
        HTTP.Proxy.Host := proxyHost;
        HTTP.Proxy.Port := StrToIntDef(proxyPort, 8080);
        if proxyUser <> '' then
          HTTP.Proxy.Username := proxyUser;
        if proxyPass <> '' then
          HTTP.Proxy.Password := proxyPass;
      end;
      if (not withProxy) and (proxyHost <> '') then
      begin
        HTTP.Proxy.Host := '';
        HTTP.Proxy.Port := 0;
      end;

      if contentType <> '' then
        HTTP.AddHeader('Content-Type', contentType);

      bodyStream := TStringStream.Create(body);
      respStream := TStringStream.Create('');
      try
        try
          HTTP.RequestBody := bodyStream;
          HTTP.Post(url, respStream);
          res := Trim(respStream.DataString);
          Result := true;
        except
          on E: Exception do
          begin
            res := 'Error: ' + E.Message;
            Result := false;
          end;
        end;
      finally
        HTTP.RequestBody := nil;
        bodyStream.Free;
        respStream.Free;
      end;
    finally
      HTTP.Free;
    end;
  end;

begin
  Result := false;
  tempInstance := TTrndiNativeHaiku.Create;
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

    if proxyHost <> '' then
    begin
      if PerformRequest(true) then
      begin
        Result := true;
        Exit;
      end;
    end;

    Result := PerformRequest(false);
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

{------------------------------------------------------------------------------
  requestEx (Haiku)
  -----------------
  Cookie-aware, manually-redirect-following HTTP via TFPHTTPClient.
 ------------------------------------------------------------------------------}
function TTrndiNativeHaiku.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList;
prefix: boolean): THTTPResponse;
var
  HTTP: TFPHTTPClient;
  address, sx, maskedSx, bodyData, hdr, headerLine: string;
  i, j, redirectCount: integer;
  response: string;
  statusCode: integer;
  cookieVal: string;
  cookiePos: integer;
  currentPost: boolean;
  newLocation, resolvedLocation: string;

  function HasHeaderLocal(const Name: string): boolean;
  var k: integer; s: string;
  begin
    Result := False;
    if customHeaders = nil then Exit;
    s := LowerCase(Name) + ':';
    for k := 0 to customHeaders.Count - 1 do
      if Pos(s, LowerCase(customHeaders[k])) = 1 then
        Exit(True);
  end;

begin
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
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
  end;

  if post and (jsondata = '') and (Length(params) > 0) then
  begin
    bodyData := '';
    for j := 0 to High(params) do
    begin
      if j > 0 then bodyData := bodyData + '&';
      bodyData := bodyData + params[j];
    end;
  end
  else if jsondata <> '' then
    bodyData := jsondata
  else
    bodyData := '';

  redirectCount := 0;
  currentPost := post;

  // Manual redirect loop so we can observe headers and cookies.
  while True do
  begin
    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.AllowRedirect := False;
      HTTP.IOTimeout := 30000;

      if useragent <> '' then
        HTTP.AddHeader('User-Agent', useragent);

      if customHeaders <> nil then
        for i := 0 to customHeaders.Count - 1 do
        begin
          hdr := customHeaders[i];
          if Pos(':', hdr) > 0 then
            HTTP.AddHeader(Trim(Copy(hdr, 1, Pos(':', hdr) - 1)),
              Trim(Copy(hdr, Pos(':', hdr) + 1, MaxInt)))
          else if Pos('=', hdr) > 0 then
            HTTP.AddHeader(Trim(Copy(hdr, 1, Pos('=', hdr) - 1)),
              Trim(Copy(hdr, Pos('=', hdr) + 1, MaxInt)))
          else
            HTTP.RequestHeaders.Add(hdr);
        end;

      if (cookieJar <> nil) and (cookieJar.Count > 0) then
      begin
        sx := '';
        for i := 0 to cookieJar.Count - 1 do
        begin
          if Trim(cookieJar[i]) = '' then Continue;
          if sx <> '' then sx := sx + '; ';
          sx := sx + cookieJar[i];
        end;
        if sx <> '' then
          HTTP.AddHeader('Cookie', sx);
      end;

      try
        if currentPost then
        begin
          if jsondata <> '' then
          begin
            if not HasHeaderLocal('Content-Type') then
              HTTP.AddHeader('Content-Type', 'application/json; charset=UTF-8');
            if not HasHeaderLocal('Accept') then
              HTTP.AddHeader('Accept', 'application/json');
            response := HTTP.Post(address, bodyData);
          end
          else if bodyData <> '' then
          begin
            if not HasHeaderLocal('Content-Type') then
              HTTP.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
            maskedSx := bodyData;
            maskedSx := StringReplace(maskedSx, 'code_verifier=', 'code_verifier=***', [rfIgnoreCase]);
            maskedSx := StringReplace(maskedSx, 'code=', 'code=***', [rfIgnoreCase]);
            maskedSx := StringReplace(maskedSx, 'password=', 'password=***', [rfIgnoreCase]);
            maskedSx := StringReplace(maskedSx, 'client_secret=', 'client_secret=***', [rfIgnoreCase]);
            TrndiNetLog('HTTP POST body (masked): ' + Copy(maskedSx, 1, 2000));
            response := HTTP.Post(address, bodyData);
          end
          else
            response := HTTP.Post(address, '');
        end
        else
          response := HTTP.Get(address);
      except
        on E: Exception do
        begin
          Result.Success := False;
          Result.ErrorMessage := E.Message;
          for i := 0 to HTTP.ResponseHeaders.Count - 1 do
            Result.Headers.Add(HTTP.ResponseHeaders[i]);
          Exit;
        end;
      end;

      for i := 0 to HTTP.ResponseHeaders.Count - 1 do
      begin
        headerLine := HTTP.ResponseHeaders[i];
        Result.Headers.Add(headerLine);
        if Pos('set-cookie', LowerCase(headerLine)) > 0 then
        begin
          cookieVal := headerLine;
          cookiePos := Pos(':', cookieVal);
          if cookiePos > 0 then cookieVal := Trim(Copy(cookieVal, cookiePos + 1, MaxInt));
          cookiePos := Pos(';', cookieVal);
          if cookiePos > 0 then cookieVal := Copy(cookieVal, 1, cookiePos - 1);
          if cookieVal <> '' then
          begin
            Result.Cookies.Add(cookieVal);
            if cookieJar <> nil then
              if cookieJar.IndexOf(cookieVal) = -1 then
                cookieJar.Add(cookieVal);
          end;
        end;
      end;

      statusCode := HTTP.ResponseStatusCode;
      Result.StatusCode := statusCode;
      Result.Body := response;
      Result.FinalURL := address;

      if followRedirects and (statusCode in [301, 302, 303, 307, 308]) then
      begin
        newLocation := '';
        for i := 0 to HTTP.ResponseHeaders.Count - 1 do
        begin
          headerLine := HTTP.ResponseHeaders[i];
          if LowerCase(Copy(headerLine, 1, 9)) = 'location:' then
          begin
            newLocation := Trim(Copy(headerLine, 10, MaxInt));
            Break;
          end;
        end;

        if newLocation = '' then
        begin
          Result.Success := False;
          Result.ErrorMessage := 'Redirect response missing Location header';
          Exit;
        end;

        if not ResolveRelativeURI(address, newLocation, resolvedLocation) then
          resolvedLocation := newLocation;
        address := resolvedLocation;

        Inc(redirectCount);
        Result.RedirectCount := redirectCount;
        if (statusCode = 303) or (statusCode = 302) or (statusCode = 301) then
          currentPost := False;

        if redirectCount >= maxRedirects then
        begin
          Result.Success := False;
          Result.ErrorMessage := 'Too many redirects';
          Exit;
        end;

        Continue;
      end
      else
      begin
        Result.Success := (statusCode >= 200) and (statusCode < 300);
        Break;
      end;
    finally
      HTTP.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  request (Haiku)
  ---------------
  HTTP GET/POST via TFPHTTPClient. Mirrors the simple shape of the libcurl
  path on Linux/BSD but without proxy support (Haiku has no proxy plumbing
  in the settings UI today).
 ------------------------------------------------------------------------------}
function TTrndiNativeHaiku.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
const header: string; prefix: boolean): string;
var
  HTTP: TFPHTTPClient;
  address, sx, body: string;
  i, p: integer;
  key, val: string;
begin
  if prefix then
    address := Format('%s/%s', [baseurl, endpoint])
  else
    address := endpoint;

  // GET: append query string. POST without JSON: use params as form body.
  if (not post) and (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
  end;

  HTTP := TFPHTTPClient.Create(nil);
  try
    if useragent <> '' then
      HTTP.AddHeader('User-Agent', useragent);

    if header <> '' then
    begin
      p := Pos('=', header);
      if p > 0 then
      begin
        key := Trim(Copy(header, 1, p - 1));
        val := Trim(Copy(header, p + 1, MaxInt));
        if key <> '' then
          HTTP.AddHeader(key, val);
      end;
    end;

    try
      if post then
      begin
        if jsondata <> '' then
        begin
          HTTP.AddHeader('Content-Type', 'application/json; charset=UTF-8');
          HTTP.AddHeader('Accept', 'application/json');
          body := jsondata;
        end
        else if Length(params) > 0 then
        begin
          HTTP.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
          body := '';
          for i := 0 to High(params) do
          begin
            if i > 0 then
              body := body + '&';
            body := body + params[i];
          end;
        end
        else
          body := '';
        Result := HTTP.Post(address, body);
      end
      else
        Result := HTTP.Get(address);
    except
      on E: Exception do
        Result := E.Message;
    end;
  finally
    HTTP.Free;
  end;
end;

end.
