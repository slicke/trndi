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
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

{**
  @abstract(Portable fallback implementation of @link(TTrndiNativeBase).)

  This unit defines @link(TTrndiNativeGeneric), a platform-agnostic
  implementation built only on FPC facilities:
  - Settings persisted in an INI file (@link(TTrndiNativeGeneric.ResolveIniPath)
    is virtual so platforms can follow their own filesystem conventions)
  - HTTP via TFPHTTPClient; a configured proxy is used exclusively, and with
    none configured the environment's proxy variables are followed
  - Tool discovery via `which` for POSIX-style platforms

  It exists so that bringing Trndi to a new platform only requires
  subclassing this class and overriding the genuinely native parts
  (notifications, TTS, dark mode, badges). Haiku is the first consumer.

  Platforms with real native stacks (Windows, macOS, Linux/libcurl) do NOT
  inherit from this unit — keeping it out of their uses closure avoids
  compiling fphttpclient/opensslsockets into their binaries.
}

unit trndi.native.generic;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, IniFiles, trndi.native.base;

type
  {!
    @abstract(Generic, platform-neutral implementation of @link(TTrndiNativeBase).)
    Uses INI files for settings and TFPHTTPClient for web requests. Subclass
    per platform and override the native-only members.
  }
TTrndiNativeGeneric = class(TTrndiNativeBase)
protected
  inistore: TIniFile; // INI-backed settings store
    {** Resolve the INI file path. Default: GetAppConfigDir + trndi.ini.
        Platforms override to follow their own conventions (e.g. Haiku's
        ~/config/settings/Trndi). }
  function ResolveIniPath: string; virtual;
    {** Ensure the INI store is created and directory exists. }
  procedure EnsureIni; inline;
public
    {** Free resources and close INI store. }
  destructor Destroy; override;

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
  {** HTTP GET/POST via TFPHTTPClient. Pure-FPC alternative to the libcurl/
      WinHTTP/NSURLSession paths used by the fully native platforms. }
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''; prefix: boolean = true): string; override;
  {** Enhanced HTTP request via TFPHTTPClient: tracks cookies, follows
      redirects manually, captures response headers. Honours the configured
      proxy exclusively (no direct fallback) and returns real HTTP status codes
      (4xx/5xx never raise), matching the Windows/Mac implementations. }
  function requestEx(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    cookieJar: TStringList = nil; followRedirects: boolean = true;
    maxRedirects: integer = 10; customHeaders: TStringList = nil;
    prefix: boolean = true): THTTPResponse; override;

  {** True when @param(tool) resolves in PATH (probed via `which`; POSIX-style
      platforms only — returns False where `which` is unavailable). }
  class function ToolAvailable(const tool: string): boolean;
end;

implementation

uses
fphttpclient, opensslsockets, URIParser, Process, StrUtils, trndi.log;

const
  // FPC's TSocketStream.SetIOTimeout only implements the setsockopt call for
  // Windows and UNIX; Haiku defines neither, so it raises a spurious
  // "Failed to set IO Timeout" from an uninitialized result. Leave the
  // timeout at 0 there — fphttpclient then never touches the socket option.
  HTTP_IO_TIMEOUT = {$IFDEF HAIKU}0{$ELSE}30000{$ENDIF};

{------------------------------------------------------------------------------
  ResolveEnvProxy
  ---------------
  Resolve the environment's proxy variables for a target URL. libcurl does this
  by itself on the Linux/BSD paths and WinHTTP follows the system configuration
  on Windows, but TFPHTTPClient never looks at the environment — so without
  this, "no proxy in the settings" would mean "connect directly" even where the
  system says otherwise.

  Follows curl's rules: the scheme-specific variable first, then all_proxy,
  with no_proxy as the bypass list. Uppercase HTTP_PROXY is deliberately not
  read (in CGI-style environments it is settable from a request header); every
  other variable is honoured in either case. no_proxy entries match a hostname
  or a domain suffix; a port inside an entry is not considered.

  Returns False, leaving host empty, when the environment names no proxy for
  this URL — the caller then behaves exactly as before.
 ------------------------------------------------------------------------------}
function ResolveEnvProxy(const url: string; out host: string; out port: string;
  out user: string; out pass: string): boolean;
var
  scheme, target, spec, creds, scratch: string;
  p: integer;

  function EnvAny(const names: array of string): string;
  var
    k: integer;
  begin
    Result := '';
    for k := Low(names) to High(names) do
    begin
      Result := Trim(GetEnvironmentVariable(names[k]));
      if Result <> '' then
        Exit;
    end;
  end;

  function Bypassed(const h: string): boolean;
  var
    list: TStringList;
    k: integer;
    rule, hn: string;
  begin
    Result := false;
    rule := EnvAny(['no_proxy', 'NO_PROXY']);
    if rule = '' then
      Exit;
    if rule = '*' then
      Exit(true);
    if h = '' then
      Exit;
    hn := LowerCase(h);
    list := TStringList.Create;
    try
      list.Delimiter := ',';
      list.StrictDelimiter := true;
      list.DelimitedText := LowerCase(rule);
      for k := 0 to list.Count - 1 do
      begin
        rule := Trim(list[k]);
        while (rule <> '') and (rule[1] in ['.', '*']) do
          Delete(rule, 1, 1);
        if rule = '' then
          Continue;
        if (hn = rule) or ((Length(hn) > Length(rule)) and
          (Copy(hn, Length(hn) - Length(rule), MaxInt) = '.' + rule)) then
          Exit(true);
      end;
    finally
      list.Free;
    end;
  end;

begin
  Result := false;
  host := '';
  port := '';
  user := '';
  pass := '';

  // Target host, for the no_proxy check and to pick the right variable
  target := Trim(url);
  scheme := '';
  p := Pos('://', target);
  if p > 0 then
    scheme := LowerCase(Copy(target, 1, p - 1));
  p := Pos('?', target);
  if p > 0 then
    target := Copy(target, 1, p - 1);
  p := Pos('#', target);
  if p > 0 then
    target := Copy(target, 1, p - 1);
  scratch := '';
  NormalizeProxyHostPort(target, scratch);

  if scheme = 'https' then
    spec := EnvAny(['https_proxy', 'HTTPS_PROXY'])
  else
    spec := EnvAny(['http_proxy']);
  if spec = '' then
    spec := EnvAny(['all_proxy', 'ALL_PROXY']);
  if spec = '' then
    Exit;

  if Bypassed(target) then
    Exit;

  // user:pass@host:port. Strip the scheme, then split the credentials at the
  // last '@' — before any path trimming, or a password holding a slash would
  // swallow the host. NormalizeProxyHostPort drops the path afterwards.
  p := Pos('://', spec);
  if p > 0 then
    spec := Copy(spec, p + 3, MaxInt);

  p := LastDelimiter('@', spec);
  if (p > 0) and (p < Length(spec)) then
  begin
    creds := Copy(spec, 1, p - 1);
    spec := Copy(spec, p + 1, MaxInt);
    p := Pos(':', creds);
    if p > 0 then
    begin
      user := Copy(creds, 1, p - 1);
      pass := Copy(creds, p + 1, MaxInt);
    end
    else
      user := creds;
  end;

  host := spec;
  NormalizeProxyHostPort(host, port);
  if host = '' then
    Exit;
  if port = '' then
    port := '8080';
  Result := true;
end;

{------------------------------------------------------------------------------
  ToolAvailable
  -------------
  Probe PATH for an executable via `which`. Silently returns False when the
  probe itself fails (e.g. no `which` on the system).
 ------------------------------------------------------------------------------}
class function TTrndiNativeGeneric.ToolAvailable(const tool: string): boolean;
var
  AProcess: TProcess;
begin
  Result := false;
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'which';
    AProcess.Parameters.Add(tool);
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
  Default INI location using FPC's per-user application config directory.
 ------------------------------------------------------------------------------}
function TTrndiNativeGeneric.ResolveIniPath: string;
var
  ConfigDir: string;
begin
  ConfigDir := GetAppConfigDir(false);
  if ConfigDir = '' then
    ConfigDir := ExtractFilePath(ParamStr(0));

  if not DirectoryExists(ConfigDir) then
    ForceDirectories(ConfigDir);

  Result := IncludeTrailingPathDelimiter(ConfigDir) + 'trndi.ini';
end;

{------------------------------------------------------------------------------
  EnsureIni
  ---------
  Create INI store if not present.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeGeneric.EnsureIni;
begin
  if inistore = nil then
    inistore := TIniFile.Create(ResolveIniPath);
end;

{------------------------------------------------------------------------------
  Destroy
  -------
  Clean up INI store.
 ------------------------------------------------------------------------------}
destructor TTrndiNativeGeneric.Destroy;
begin
  if inistore <> nil then
    FreeAndNil(inistore);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  GetSetting
  ----------
  Read a setting from INI file.
 ------------------------------------------------------------------------------}
function TTrndiNativeGeneric.GetSetting(const keyname: string; def: string = '';
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
procedure TTrndiNativeGeneric.SetSetting(const keyname: string; const val: string;
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
procedure TTrndiNativeGeneric.DeleteSetting(const keyname: string;
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
procedure TTrndiNativeGeneric.ReloadSettings;
begin
  if inistore <> nil then
    FreeAndNil(inistore);
end;

{------------------------------------------------------------------------------
  ExportSettings
  --------------
  Export all settings from the INI file to a string.
 ------------------------------------------------------------------------------}
function TTrndiNativeGeneric.ExportSettings: string;
var
  sl: TStringList;
  sections, keys: TStringList;
  i, j: integer;
  section, key, value: string;
begin
  EnsureIni;
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
procedure TTrndiNativeGeneric.ImportSettings(const iniData: string);
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
  EnsureIni;
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
  HTTP GET using TFPHTTPClient. The temporary instance for proxy lookup is
  created through Self so a subclass' settings store (ResolveIniPath
  override) is honoured.
 ------------------------------------------------------------------------------}
class function TTrndiNativeGeneric.getURL(const url: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
  tempInstance: TTrndiNativeGeneric;
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

  function PerformRequest(withProxy: boolean): boolean;
  begin
    Result := false;
    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.AllowRedirect := true;
      HTTP.IOTimeout := HTTP_IO_TIMEOUT; // 30 seconds timeout

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
  tempInstance := Self.Create;
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
    end
    else
      // Nothing configured: follow the environment's proxy variables, the way
      // curl does on Linux/BSD.
      ResolveEnvProxy(url, proxyHost, proxyPort, proxyUser, proxyPass);

    {$ifdef DEBUG}
    if proxyHost <> '' then
      TrndiDLog(Format('HTTP GET: proxy configured (%s:%s); url=%s', [proxyHost, proxyPort, SafeUrlForLog(url)]))
    else
      TrndiDLog(Format('HTTP GET: no proxy configured; url=%s', [SafeUrlForLog(url)]));
    {$endif}

    // A configured proxy is the only route out: no direct fallback, or a dead
    // proxy would silently send the traffic around it.
    if proxyHost <> '' then
    begin
      {$ifdef DEBUG}
      TrndiDLog(Format('HTTP GET: attempting via proxy %s:%s', [proxyHost, proxyPort]));
      {$endif}
      Result := PerformRequest(true);
      {$ifdef DEBUG}
      if Result then
        TrndiNetLog('HTTP GET: proxy attempt succeeded')
      else
        TrndiNetLog('HTTP GET: proxy attempt failed: ' + res);
      {$endif}
      Exit;
    end;

    {$ifdef DEBUG}
    TrndiNetLog('HTTP GET: attempting direct');
    {$endif}
    Result := PerformRequest(false);
    {$ifdef DEBUG}
    if not Result then
      TrndiNetLog('HTTP GET: direct attempt failed: ' + res);
    {$endif}

  finally
    tempInstance.Free;
  end;
end;

{------------------------------------------------------------------------------
  postURL
  -------
  Simple HTTP POST using TFPHTTPClient. A configured proxy is used exclusively,
  with no direct fallback (mirrors getURL).
 ------------------------------------------------------------------------------}
class function TTrndiNativeGeneric.postURL(const url: string; const body: string;
  const contentType: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
  tempInstance: TTrndiNativeGeneric;
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
      HTTP.IOTimeout := HTTP_IO_TIMEOUT;

      if withProxy and (proxyHost <> '') then
      begin
        HTTP.Proxy.Host := proxyHost;
        HTTP.Proxy.Port := StrToIntDef(proxyPort, 8080);
        if proxyUser <> '' then
          HTTP.Proxy.Username := proxyUser;
        if proxyPass <> '' then
          HTTP.Proxy.Password := proxyPass;
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
  tempInstance := Self.Create;
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
    end
    else
      ResolveEnvProxy(url, proxyHost, proxyPort, proxyUser, proxyPass);

    // Strict: a proxy, configured or from the environment, is never bypassed
    // (mirrors getURL).
    if proxyHost <> '' then
      Result := PerformRequest(true)
    else
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
class function TTrndiNativeGeneric.TestProxyURL(const url: string;
  const proxyHost: string; const proxyPort: string; const proxyUser: string;
  const proxyPass: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
  host, portS, user, pass: string;
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
    HTTP.IOTimeout := HTTP_IO_TIMEOUT;
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
  requestEx (generic)
  -------------------
  Cookie-aware, manually-redirect-following HTTP via TFPHTTPClient. Honours
  the configured proxy exclusively, with no direct fallback on any hop (like
  getURL/postURL). Requests
  go through HTTPMethod with an empty allowed-code list so redirects and
  4xx/5xx come back with their real status and body instead of raising,
  matching the Windows/Mac implementations.
 ------------------------------------------------------------------------------}
function TTrndiNativeGeneric.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList;
prefix: boolean): THTTPResponse;
var
  address, maskedSx, bodyData, headerLine: string;
  i, j, redirectCount: integer;
  response, hopErr: string;
  statusCode: integer;
  cookieVal: string;
  cookiePos: integer;
  currentPost, hopOk: boolean;
  newLocation, resolvedLocation: string;
  proxyHost, proxyPort, proxyUser, proxyPass: string;
  hopHeaders: TStringList;

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

  // Replace the value of a form parameter with *** so secrets never reach
  // the log (same helper as the Linux implementation).
  procedure MaskParam(var S: string; const name: string);
  var
    p, valStart, q: integer;
  begin
    p := Pos(name + '=', S);
    if p = 0 then
      Exit;
    valStart := p + Length(name) + 1;
    q := PosEx('&', S, valStart);
    if q = 0 then
      q := Length(S) + 1;
    Delete(S, valStart, q - valStart);
    Insert('***', S, valStart);
  end;

  // One HTTP exchange. False means a transport-level failure (connect/TLS/
  // socket); HTTP status codes never raise because HTTPMethod is called with
  // an empty allowed-code list (Get/Post would raise on anything but 200
  // with AllowRedirect off, hiding redirects and 4xx/5xx from the caller).
  function TryHop(const url: string; const isPost: boolean;
    const requestBody: string; const useProxy: boolean; out outBody: string;
    out outStatus: integer; outHeaders: TStringList;
    out outError: string): boolean;
  var
    HTTP: TFPHTTPClient;
    bodyStream, respStream: TStringStream;
    k: integer;
    hdr, cookieHdr: string;
  begin
    Result := false;
    outBody := '';
    outStatus := 0;
    outError := '';
    outHeaders.Clear;

    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.AllowRedirect := False;
      HTTP.IOTimeout := HTTP_IO_TIMEOUT;

      if useProxy then
      begin
        HTTP.Proxy.Host := proxyHost;
        HTTP.Proxy.Port := StrToIntDef(proxyPort, 8080);
        if proxyUser <> '' then
          HTTP.Proxy.Username := proxyUser;
        if proxyPass <> '' then
          HTTP.Proxy.Password := proxyPass;
      end;

      if useragent <> '' then
        HTTP.AddHeader('User-Agent', useragent);

      if customHeaders <> nil then
        for k := 0 to customHeaders.Count - 1 do
        begin
          hdr := customHeaders[k];
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
        cookieHdr := '';
        for k := 0 to cookieJar.Count - 1 do
        begin
          if Trim(cookieJar[k]) = '' then Continue;
          if cookieHdr <> '' then cookieHdr := cookieHdr + '; ';
          cookieHdr := cookieHdr + cookieJar[k];
        end;
        if cookieHdr <> '' then
          HTTP.AddHeader('Cookie', cookieHdr);
      end;

      if isPost then
      begin
        if jsondata <> '' then
        begin
          if not HasHeaderLocal('Content-Type') then
            HTTP.AddHeader('Content-Type', 'application/json; charset=UTF-8');
          if not HasHeaderLocal('Accept') then
            HTTP.AddHeader('Accept', 'application/json');
        end
        else if requestBody <> '' then
        begin
          if not HasHeaderLocal('Content-Type') then
            HTTP.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
        end;
      end;

      bodyStream := nil;
      respStream := TStringStream.Create('');
      try
        if isPost and (requestBody <> '') then
        begin
          bodyStream := TStringStream.Create(requestBody);
          HTTP.RequestBody := bodyStream;
        end;
        try
          if isPost then
            HTTP.HTTPMethod('POST', url, respStream, [])
          else
            HTTP.HTTPMethod('GET', url, respStream, []);
          Result := true;
        except
          on E: Exception do
            outError := E.Message;
        end;
        // Copy out whatever arrived even on failure: the status line and
        // headers may already be parsed when a later read fails.
        outBody := respStream.DataString;
        outStatus := HTTP.ResponseStatusCode;
        for k := 0 to HTTP.ResponseHeaders.Count - 1 do
          outHeaders.Add(HTTP.ResponseHeaders[k]);
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
    address := address + '?' + params[0];
    for j := 1 to High(params) do
      address := address + '&' + params[j];
  end;

  if post and (jsondata = '') and (Length(params) > 0) then
  begin
    bodyData := '';
    for j := 0 to High(params) do
    begin
      if j > 0 then bodyData := bodyData + '&';
      bodyData := bodyData + params[j];
    end;
    maskedSx := bodyData;
    MaskParam(maskedSx, 'code_verifier');
    MaskParam(maskedSx, 'code');
    MaskParam(maskedSx, 'password');
    MaskParam(maskedSx, 'client_secret');
    TrndiNetLog('HTTP POST body (masked): ' + Copy(maskedSx, 1, 2000));
  end
  else if jsondata <> '' then
    bodyData := jsondata
  else
    bodyData := '';

  // Same proxy keys as the Windows implementation; a configured proxy carries
  // every hop and is never bypassed (mirrors getURL/postURL).
  proxyHost := Trim(GetRootSetting('proxy.host', ''));
  proxyPort := Trim(GetRootSetting('proxy.port', ''));
  proxyUser := GetRootSetting('proxy.user', '');
  proxyPass := GetRootSetting('proxy.pass', '');
  if proxyHost <> '' then
  begin
    NormalizeProxyHostPort(proxyHost, proxyPort);
    if proxyPort = '' then
      proxyPort := '8080';
  end
  else
    ResolveEnvProxy(address, proxyHost, proxyPort, proxyUser, proxyPass);

  redirectCount := 0;
  currentPost := post;

  hopHeaders := TStringList.Create;
  try
    // Manual redirect loop so we can observe headers and cookies.
    while True do
    begin
      // No direct fallback on a configured proxy, so a redirect chain cannot
      // start on the proxy and finish around it.
      hopOk := TryHop(address, currentPost, bodyData, proxyHost <> '', response,
        statusCode, hopHeaders, hopErr);

      Result.Body := response;
      Result.StatusCode := statusCode;
      Result.Headers.Assign(hopHeaders);
      Result.FinalURL := address;

      if not hopOk then
      begin
        Result.Success := False;
        Result.ErrorMessage := hopErr;
        Exit;
      end;

      for i := 0 to hopHeaders.Count - 1 do
      begin
        headerLine := Trim(hopHeaders[i]);
        if Pos('set-cookie:', LowerCase(headerLine)) = 1 then
        begin
          cookieVal := Trim(Copy(headerLine, Pos(':', headerLine) + 1, MaxInt));
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

      if followRedirects and ((statusCode = 301) or (statusCode = 302) or
        (statusCode = 303) or (statusCode = 307) or (statusCode = 308)) then
      begin
        newLocation := '';
        for i := 0 to hopHeaders.Count - 1 do
        begin
          headerLine := Trim(hopHeaders[i]);
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
        if redirectCount > maxRedirects then
        begin
          Result.Success := False;
          Result.ErrorMessage := 'Too many redirects';
          Exit;
        end;

        if (statusCode = 303) or (((statusCode = 301) or (statusCode = 302)) and currentPost) then
        begin
          currentPost := False;
          bodyData := '';
        end;

        Continue;
      end;

      // Transport-level success: like Windows/Mac, 4xx/5xx still count as a
      // completed request (callers inspect StatusCode).
      Result.Success := true;
      Break;
    end;
  finally
    hopHeaders.Free;
  end;
end;

{------------------------------------------------------------------------------
  request (generic)
  -----------------
  HTTP GET/POST via TFPHTTPClient. Mirrors the simple shape of the libcurl
  path on Linux/BSD, including the exclusive use of a configured proxy — this
  is the path the backend drivers use, so a proxy that is ignored here is a
  proxy that is ignored for all data traffic.
 ------------------------------------------------------------------------------}
function TTrndiNativeGeneric.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
const header: string; prefix: boolean): string;
var
  address: string;
  i: integer;
  proxyHost, proxyPort, proxyUser, proxyPass: string;

  // Returns False (with the error message in response) when the attempt threw,
  // so the caller can retry without the proxy.
  function PerformRequest(withProxy: boolean; out response: string): boolean;
  var
    HTTP: TFPHTTPClient;
    body, key, val: string;
    bodyStream: TStringStream;
    p: integer;
  begin
    Result := false;
    response := '';

    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.IOTimeout := HTTP_IO_TIMEOUT;
      if useragent <> '' then
        HTTP.AddHeader('User-Agent', useragent);

      // A fresh client is proxy-less, so the direct attempt needs no undoing.
      if withProxy and (proxyHost <> '') then
      begin
        HTTP.Proxy.Host := proxyHost;
        HTTP.Proxy.Port := StrToIntDef(proxyPort, 8080);
        if proxyUser <> '' then
          HTTP.Proxy.Username := proxyUser;
        if proxyPass <> '' then
          HTTP.Proxy.Password := proxyPass;
      end;

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
          else
            body := ''; // params already in the query string; empty POST body
          if body <> '' then
          begin
            bodyStream := TStringStream.Create(body);
            try
              HTTP.RequestBody := bodyStream;
              response := HTTP.Post(address);
            finally
              HTTP.RequestBody := nil;
              bodyStream.Free;
            end;
          end
          else
            response := HTTP.Post(address);
        end
        else
          response := HTTP.Get(address);
        Result := true;
      except
        on E: Exception do
        begin
          response := E.Message;
          Result := false;
        end;
      end;
    finally
      HTTP.Free;
    end;
  end;

begin
  if prefix then
    address := Format('%s/%s', [baseurl, endpoint])
  else
    address := endpoint;

  // Append params as query string for GET and POST alike, matching the
  // Windows/Linux implementations — Dexcom Share expects POST parameters in
  // the URL and rejects them as a form body (415 Unsupported Media Type).
  if (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?' + params[0];
    for i := 1 to High(params) do
      address := address + '&' + params[i];
  end;

  proxyHost := Trim(GetRootSetting('proxy.host', ''));
  proxyPort := Trim(GetRootSetting('proxy.port', ''));
  proxyUser := GetRootSetting('proxy.user', '');
  proxyPass := GetRootSetting('proxy.pass', '');
  if proxyHost <> '' then
  begin
    NormalizeProxyHostPort(proxyHost, proxyPort);
    if proxyPort = '' then
      proxyPort := '8080';
  end
  else
    ResolveEnvProxy(address, proxyHost, proxyPort, proxyUser, proxyPass);

  // A configured proxy is used exclusively - no direct fallback, exactly as
  // getURL/postURL do.
  if proxyHost <> '' then
    PerformRequest(true, Result)
  else
    PerformRequest(false, Result);
end;

end.
