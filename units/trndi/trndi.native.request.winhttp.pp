(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)


{**
  @abstract(WinHTTP transport shared by the Windows-backed native classes.)

  Free functions carrying the HTTP implementation that used to live inside
  @code(TTrndiNativeWindows) (getURL, postURL, TestProxyURL, request,
  requestEx), so that both the LCL desktop class and the LCL-free console
  class (@code(trndi.native.console) built for Windows) can route their HTTP
  through WinHTTP — the system's own stack, with its TLS, CA store and proxy
  configuration — without shipping any third-party DLL. Mirrors the shape of
  @code(trndi.native.request.curl), which plays the same role for libcurl.

  The simple calls go through @code(TWinHTTPClient) (winutils.httpclient);
  requestEx drives the raw WinHTTP API itself because it has to disable the
  automatic redirect handling to keep cookies and the Location chain visible.

  Proxy contract, unchanged from the original methods: a configured proxy
  carries every request with no direct fallback (a dead proxy surfaces as an
  error instead of quietly being bypassed); with nothing configured WinHTTP
  follows the system proxy configuration, which is what every other Windows
  application does.

  MODIFICATION NOTICE (GPLv3 Section 5):
  - 2026-09-18: Unit created. The five WinHTTP methods moved here verbatim
    from trndi.native.win.pp as free functions taking the base URL, the user
    agent and a @link(TWinHttpProxy) instead of reading them from the class.
}

unit trndi.native.request.winhttp;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, StrUtils, Windows, winutils.httpclient, trndi.log,
trndi.native.base;

type
  {** Proxy configuration handed to every transport function. Empty
      @code(host) means "nothing configured" — WinHTTP then follows the
      system proxy configuration. }
TWinHttpProxy = record
  host: string;
  port: string;
  user: string;
  pass: string;
end;

{** Read the @code(proxy.*) root settings from @param(inst) and normalize the
    host/port split (the host field holds whatever the user typed, e.g.
    'http://proxy:3128'; WinHTTP wants a bare host). Callers own the instance;
    class-function call sites create a short-lived one, as the original
    Windows implementations did. }
function FetchWinHttpProxy(inst: TTrndiNativeBase): TWinHttpProxy;

{** Simple HTTP GET with a default User-Agent. }
function WinHttpGetURL(const url: string; const proxy: TWinHttpProxy;
out res: string): boolean;

{** Simple HTTP POST (mirrors @link(WinHttpGetURL)'s proxy contract). }
function WinHttpPostURL(const url, body, contentType: string;
const proxy: TWinHttpProxy; out res: string): boolean;

{** Proxy-only HTTP GET used by the settings dialog's "Test proxy" action.
    No direct fallback. }
function WinHttpTestProxyURL(const url: string; const proxyHost: string;
const proxyPort: string; const proxyUser: string; const proxyPass: string;
out res: string): boolean;

{** HTTP GET/POST returning the body (or the WinHTTP error message). }
function WinHttpRequest(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; const header: string; prefix: boolean;
const proxy: TWinHttpProxy): string;

{** Cookie-aware, redirect-following HTTP returning the full
    @link(THTTPResponse). }
function WinHttpRequestEx(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList; prefix: boolean;
const proxy: TWinHttpProxy): THTTPResponse;

implementation

const
DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';

function FetchWinHttpProxy(inst: TTrndiNativeBase): TWinHttpProxy;
begin
  Result.host := Trim(inst.GetSetting('proxy.host', '', true));
  Result.port := '';
  Result.user := '';
  Result.pass := '';
  if Result.host <> '' then
  begin
    Result.port := Trim(inst.GetSetting('proxy.port', '', true));
    Result.user := inst.GetSetting('proxy.user', '', true);
    Result.pass := inst.GetSetting('proxy.pass', '', true);
    // Split the host exactly like the settings dialog's test button does.
    NormalizeProxyHostPort(Result.host, Result.port);
  end;
end;

{------------------------------------------------------------------------------
  WinHttpGetURL
  -------------
  Simple HTTP GET using WinHTTP client with a default User-Agent.
 ------------------------------------------------------------------------------}
function WinHttpGetURL(const url: string; const proxy: TWinHttpProxy;
out res: string): boolean;
var
  client: TWinHTTPClient;
  responseStr: string;
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
    if withProxy and (proxyHost <> '') then
    begin
      if (proxyUser <> '') or (proxyPass <> '') then
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080), proxyUser, proxyPass)
      else
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080));
    end
    else
      client := TWinHTTPClient.Create(DEFAULT_USER_AGENT);

    try
      responseStr := client.Get(url, []);
      res := responseStr;
      Result := true;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
      end;
    end;
    client.Free;
  end;

begin
  res := '';

  proxyHost := proxy.host;
  proxyPort := proxy.port;
  proxyUser := proxy.user;
  proxyPass := proxy.pass;
  if (proxyHost <> '') and (proxyPort = '') then
    proxyPort := '8080';

  {$ifdef DEBUG}
  if proxyHost <> '' then
    TrndiDLog(Format('HTTP GET: proxy configured (%s:%s); url=%s', [proxyHost, proxyPort, SafeUrlForLog(url)]))
  else
    TrndiDLog(Format('HTTP GET: no proxy configured; url=%s', [SafeUrlForLog(url)]));
  {$endif}

  // A configured proxy is the only route out: no direct fallback, or a
  // dead proxy would silently send the traffic around it.
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

  // Nothing configured: let WinHTTP use the system proxy configuration.
  {$ifdef DEBUG}
  TrndiNetLog('HTTP GET: attempting via system configuration');
  {$endif}
  Result := PerformRequest(false);
  {$ifdef DEBUG}
  if not Result then
    TrndiNetLog('HTTP GET: attempt failed: ' + res);
  {$endif}
end;

{------------------------------------------------------------------------------
  WinHttpPostURL
  --------------
  Simple HTTP POST using WinHTTP client. Mirrors WinHttpGetURL: a configured
  proxy is used exclusively, otherwise WinHTTP follows the system
  configuration.
 ------------------------------------------------------------------------------}
function WinHttpPostURL(const url, body, contentType: string;
const proxy: TWinHttpProxy; out res: string): boolean;
var
  client: TWinHTTPClient;
  responseStr: string;
  proxyHost, proxyPort, proxyUser, proxyPass: string;

  function PerformRequest(withProxy: boolean): boolean;
  begin
    Result := false;
    if withProxy and (proxyHost <> '') then
    begin
      if (proxyUser <> '') or (proxyPass <> '') then
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080), proxyUser, proxyPass)
      else
        client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, proxyHost, StrToIntDef(proxyPort, 8080));
    end
    else
      client := TWinHTTPClient.Create(DEFAULT_USER_AGENT);

    try
      if contentType <> '' then
        client.AddHeader('Content-Type', contentType);
      client.SetRequestBody(body);
      try
        responseStr := client.Post(url);
        res := responseStr;
        Result := true;
      except
        on E: Exception do
        begin
          res := E.Message;
          Result := false;
        end;
      end;
    finally
      client.Free;
    end;
  end;

begin
  res := '';
  proxyHost := proxy.host;
  proxyPort := proxy.port;
  proxyUser := proxy.user;
  proxyPass := proxy.pass;
  if (proxyHost <> '') and (proxyPort = '') then
    proxyPort := '8080';

  // Strict: a configured proxy is never bypassed (mirrors WinHttpGetURL).
  if proxyHost <> '' then
    Result := PerformRequest(true)
  else
    Result := PerformRequest(false);
end;

{------------------------------------------------------------------------------
  WinHttpTestProxyURL
  -------------------
  Proxy-only HTTP GET using WinHTTP client. No direct fallback.
 ------------------------------------------------------------------------------}
function WinHttpTestProxyURL(const url: string; const proxyHost: string;
const proxyPort: string; const proxyUser: string; const proxyPass: string;
out res: string): boolean;
var
  client: TWinHTTPClient;
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

  if (user <> '') or (pass <> '') then
    client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, host, StrToIntDef(portS, 8080), user, pass)
  else
    client := TWinHTTPClient.Create(DEFAULT_USER_AGENT, host, StrToIntDef(portS, 8080));

  try
    try
      res := client.Get(url, []);
      Result := true;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
      end;
    end;
  finally
    client.Free;
  end;
end;

{------------------------------------------------------------------------------
  WinHttpRequest
  --------------
  HTTP GET/POST via WinHTTPClient. A configured proxy carries the request with
  no direct fallback, and with nothing configured WinHTTP follows the system
  proxy configuration.
 ------------------------------------------------------------------------------}
function WinHttpRequest(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; const header: string; prefix: boolean;
const proxy: TWinHttpProxy): string;
var
  client: TWinHTTPClient;
  address: string;
  p: integer;
  headerKey, headerVal: string;
  hasParams: boolean;
  ResStr: string;
  proxyHost: string;
  proxyPortS: string;
  proxyPort: integer;
  proxyUser: string;
  proxyPass: string;

  procedure ConfigureClient(aClient: TWinHTTPClient);
  begin
    aClient.AddHeader('User-Agent', useragent);

    if header <> '' then
    begin
      // Split on the first '=' only, like the Linux/macOS implementations —
      // header values may themselves contain '=' (e.g. base64 API secrets).
      p := Pos('=', header);
      if p > 0 then
      begin
        headerKey := Trim(Copy(header, 1, p - 1));
        headerVal := Trim(Copy(header, p + 1, MaxInt));
        if headerKey <> '' then
          aClient.AddHeader(headerKey, headerVal);
      end;
    end;

    if jsondata <> '' then
    begin
      aClient.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      aClient.AddHeader('Accept', 'application/json');
      aClient.SetRequestBody(jsondata);
    end;
  end;

  function TryRequest(aClient: TWinHTTPClient; out outRes: string): boolean;
  begin
    try
      ConfigureClient(aClient);
      if post then
        outRes := aClient.Post(address)
      else
      begin
        if (jsondata = '') and hasParams then
          outRes := aClient.Get(address, [])
        else
          outRes := aClient.Get(address, params);
      end;
      Result := true;
      TrndiDLog('Windows: Request succeeded');
    except
      on E: Exception do
      begin
        outRes := E.Message;
        TrndiDLog('Windows: Request failed with exception: ' + E.Message);
        Result := false;
      end;
    end;
  end;

begin
  hasParams := (Length(params) > 0);

  if prefix then
    address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])])
  else
    address := endpoint;

  if (jsondata = '') and hasParams then
  begin
    address := address + '?' + params[0];
    for p := 1 to High(params) do
      address := address + '&' + params[p];
  end;

  // FetchWinHttpProxy has already split whatever the user typed
  // ('http://proxy:3128') into a bare host and a port — WinHTTP wants a bare
  // host.
  proxyHost  := proxy.host;
  proxyPortS := proxy.port;
  proxyUser  := proxy.user;
  proxyPass  := proxy.pass;
  proxyPort  := StrToIntDef(proxyPortS, 8080);

  // A configured proxy is the only route out — no direct fallback, so a proxy
  // that is down surfaces as an error instead of quietly being bypassed.
  if proxyHost <> '' then
  begin
    if (proxyUser <> '') or (proxyPass <> '') then
      client := TWinHTTPClient.Create(useragent, proxyHost, proxyPort, proxyUser, proxyPass)
    else
      client := TWinHTTPClient.Create(useragent, proxyHost, proxyPort);
    try
      TryRequest(client, ResStr);
      Result := ResStr;
    finally
      client.Free;
    end;
    Exit;
  end;

  // Nothing configured: follow the system's own proxy configuration, which is
  // what every other Windows application does.
  TrndiDLog('Windows: Using system proxy configuration for: ' +
    TrndiSafeUrl(address));
  client := TWinHTTPClient.Create(useragent);
  try
    TryRequest(client, ResStr);
    Result := ResStr;
  finally
    client.Free;
  end;
end;

{------------------------------------------------------------------------------
  WinHttpRequestEx
  ----------------
  Cookie-aware, redirect-following HTTP via WinHTTP. Same proxy contract as
  @link(WinHttpRequest): a configured proxy carries every hop with no direct
  fallback, and with nothing configured WinHTTP follows the system proxy
  configuration.
 ------------------------------------------------------------------------------}
function WinHttpRequestEx(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList; prefix: boolean;
const proxy: TWinHttpProxy): THTTPResponse;
var
  address, sx, currentUrl, bodyData, methodLabel: string;
  responseHeaders: TStringList;
  responseBody: string;
  statusCode, j: integer;
  locationHeader: string;
  proxyHost, proxyPortS, proxyUser, proxyPass: string;
  proxyPort: integer;
  currentPost: boolean;
  startTick: QWord;
  endTick: QWord;

  procedure ParseURLLocal(const URL: string; out ServerName, Path: string;
    out port: HTTPPort);
  var
    ProtocolPos, PathPos, PortPos: integer;
    PortStr: string;
  begin
    ProtocolPos := Pos('://', URL);

    port.secure := false;
    port.port := 80;

    if ProtocolPos > 0 then
    begin
      port.secure := URL[ProtocolPos - 1] = 's';
      if port.secure then
        port.port := 443;
      ProtocolPos := ProtocolPos + 3;
    end
    else
      ProtocolPos := 1;

    PathPos := PosEx('/', URL, ProtocolPos);
    PortPos := PosEx(':', URL, ProtocolPos);

    if (PortPos > 0) and ((PathPos = 0) or (PortPos < PathPos)) then
    begin
      ServerName := Copy(URL, ProtocolPos, PortPos - ProtocolPos);
      if PathPos > 0 then
        PortStr := Copy(URL, PortPos + 1, PathPos - PortPos - 1)
      else
        PortStr := Copy(URL, PortPos + 1, MaxInt);
      port.port := StrToIntDef(PortStr, port.port);
    end
    else if PathPos > 0 then
      ServerName := Copy(URL, ProtocolPos, PathPos - ProtocolPos)
    else
      ServerName := Copy(URL, ProtocolPos, Length(URL) - ProtocolPos + 1);

    if PathPos > 0 then
      Path := Copy(URL, PathPos, Length(URL) - PathPos + 1)
    else
      Path := '/';
  end;

  function HasHeader(const AName: string; AHeaders: TStringList): boolean;
  var
    k: integer;
    nameLower: string;
  begin
    Result := false;
    if AHeaders = nil then
      Exit;
    nameLower := LowerCase(AName) + ':';
    for k := 0 to AHeaders.Count - 1 do
      if Pos(nameLower, LowerCase(Trim(AHeaders[k]))) = 1 then
        Exit(true);
  end;

  function BuildCookieHeader: string;
  var
    i: integer;
    cookieData: string;
  begin
    Result := '';
    if cookieJar = nil then
      Exit;
    cookieData := '';
    for i := 0 to cookieJar.Count - 1 do
    begin
      if Trim(cookieJar[i]) = '' then
        Continue;
      if cookieData <> '' then
        cookieData := cookieData + '; ';
      cookieData := cookieData + cookieJar[i];
    end;
    Result := cookieData;
  end;

  procedure UpdateCookiesFromHeaders(const AHeaders: TStringList);
  var
    i: integer;
    lineLower: string;
    cookieVal: string;
    cookiePos: integer;
  begin
    if AHeaders = nil then
      Exit;
    for i := 0 to AHeaders.Count - 1 do
    begin
      lineLower := LowerCase(Trim(AHeaders[i]));
      if Pos('set-cookie:', lineLower) = 1 then
      begin
        cookieVal := Trim(Copy(AHeaders[i], 12, MaxInt));
        cookiePos := Pos(';', cookieVal);
        if cookiePos > 0 then
          cookieVal := Copy(cookieVal, 1, cookiePos - 1);
        if cookieVal <> '' then
        begin
          Result.Cookies.Add(cookieVal);
          if cookieJar <> nil then
          begin
            if cookieJar.IndexOf(cookieVal) = -1 then
              cookieJar.Add(cookieVal);
          end;
        end;
      end;
    end;
  end;

  function ExtractLocationHeader(const AHeaders: TStringList): string;
  var
    i: integer;
    lineLower: string;
  begin
    Result := '';
    if AHeaders = nil then
      Exit;
    for i := 0 to AHeaders.Count - 1 do
    begin
      lineLower := LowerCase(Trim(AHeaders[i]));
      if Pos('location:', lineLower) = 1 then
      begin
        Result := Trim(Copy(AHeaders[i], 10, MaxInt));
        Exit;
      end;
    end;
  end;

  function ParseStatusCodeFromHeaders(const AHeaders: TStringList): integer;
  var
    statusLine: string;
    p1, p2: integer;
  begin
    Result := 0;
    if (AHeaders = nil) or (AHeaders.Count = 0) then
      Exit;
    statusLine := Trim(AHeaders[0]);
    p1 := Pos(' ', statusLine);
    if p1 > 0 then
    begin
      p2 := PosEx(' ', statusLine, p1 + 1);
      if p2 > p1 then
        Result := StrToIntDef(Copy(statusLine, p1 + 1, p2 - p1 - 1), 0)
      else
        Result := StrToIntDef(Copy(statusLine, p1 + 1, MaxInt), 0);
    end;
  end;

  function ResolveUrl(const baseUrl, location: string): string;
  var
    lowerLoc: string;
    schemePos, rootPos: integer;
    baseRoot, baseDir: string;
  begin
    Result := location;
    lowerLoc := LowerCase(location);
    if (Pos('http://', lowerLoc) = 1) or (Pos('https://', lowerLoc) = 1) then
      Exit;

    schemePos := Pos('://', baseUrl);
    if schemePos = 0 then
      Exit;

    rootPos := PosEx('/', baseUrl, schemePos + 3);
    if rootPos = 0 then
      baseRoot := baseUrl
    else
      baseRoot := Copy(baseUrl, 1, rootPos - 1);

    if (Length(location) > 0) and (location[1] = '/') then
      Result := baseRoot + location
    else
    begin
      baseDir := Copy(baseUrl, 1, LastDelimiter('/', baseUrl));
      Result := baseDir + location;
    end;
  end;

  function TryRequest(const url: string; const isPost: boolean; const requestBody: string;
    const useProxy: boolean; out outBody: string;
    out outHeaders: TStringList; out outStatus: integer; out outLocation: string;
    out outError: string): boolean;
  var
    hSession, hConnect, hRequest: HINTERNET;
    serverName, path: string;
    port: HTTPPort;
    flags: DWORD;
    dwSize, dwDownloaded: DWORD;
    dwToRead: DWORD;
    buffer: array[0..8192] of byte;
    responseStream: TStringStream;
    headersToSend: TStringList;
    cookieHeader: string;
    headerLine: WideString;
    rawHeaderBuf: PWideChar;
    rawHeaderStr: WideString;
    index: DWORD;
    statusValue: DWORD;
    statusSize: DWORD;
    locBuf: PWideChar;
    locSize: DWORD;
    bodyPtr: Pointer;
    bodyLen: DWORD;
    sendVerb: PWideChar;
    i: integer;
    redirectPolicy: DWORD;
    cookieBuf: PWideChar;
    cookieSize: DWORD;
    cookieIndex: DWORD;
    cookieVal: WideString;

    function AppendSetCookieHeaders(hReq: HINTERNET; AHeaders: TStringList): boolean;
    begin
      Result := false;
      if AHeaders = nil then
        Exit;
      cookieIndex := 0;
      repeat
        cookieSize := 0;
        WinHttpQueryHeaders(hReq, WINHTTP_QUERY_SET_COOKIE, nil, nil, cookieSize, cookieIndex);
        if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (cookieSize > 0) then
        begin
          GetMem(cookieBuf, cookieSize);
          try
            if WinHttpQueryHeaders(hReq, WINHTTP_QUERY_SET_COOKIE, nil, cookieBuf, cookieSize, cookieIndex) then
            begin
              cookieVal := WideString(cookieBuf);
              if Trim(cookieVal) <> '' then
                AHeaders.Add('Set-Cookie: ' + string(cookieVal));
              Result := true;
            end;
          finally
            FreeMem(cookieBuf);
          end;
          Continue;
        end;
        Break;
      until false;
    end;

  begin
    Result := false;
    outBody := '';
    outHeaders := TStringList.Create;
    outHeaders.TextLineBreakStyle := tlbsCRLF;
    outStatus := 0;
    outLocation := '';
    outError := '';

    ParseURLLocal(url, serverName, path, port);

    if useProxy and (proxyHost <> '') then
      hSession := WinHttpOpen(pwidechar(widestring(useragent)), WINHTTP_ACCESS_TYPE_NAMED_PROXY,
        pwidechar(widestring(proxyHost + ':' + IntToStr(proxyPort))), WINHTTP_NO_PROXY_BYPASS, 0)
    else
      hSession := WinHttpOpen(pwidechar(widestring(useragent)), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
        WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);

    if hSession = nil then
    begin
      outError := 'WinHttpOpen failed: ' + SysErrorMessage(GetLastError);
      FreeAndNil(outHeaders);
      Exit(false);
    end;

    try
      if port.secure then
      begin
        flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
        if not WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @flags, SizeOf(flags)) then
        begin
          flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
          WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @flags, SizeOf(flags));
        end;
      end;

      if not WinHttpSetTimeouts(hSession, 15000, 15000, 30000, 120000) then
      begin
        outError := 'WinHttpSetTimeouts failed (' + IntToStr(GetLastError) + '): ' +
          SysErrorMessage(GetLastError);
        Exit(false);
      end;

      hConnect := WinHttpConnect(hSession, pwidechar(widestring(serverName)), port.port, 0);
      if hConnect = nil then
      begin
        outError := 'WinHttpConnect failed: ' + SysErrorMessage(GetLastError);
        Exit(false);
      end;

      try
        flags := 0;
        if port.secure then
          flags := WINHTTP_FLAG_SECURE;

        if isPost then
          sendVerb := 'POST'
        else
          sendVerb := 'GET';

        hRequest := WinHttpOpenRequest(hConnect, sendVerb, pwidechar(widestring(path)),
          nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, flags);
        if hRequest = nil then
        begin
          outError := 'WinHttpOpenRequest failed: ' + SysErrorMessage(GetLastError);
          Exit(false);
        end;

        try
          redirectPolicy := WINHTTP_OPTION_REDIRECT_POLICY_NEVER;
          WinHttpSetOption(hRequest, WINHTTP_OPTION_REDIRECT_POLICY, @redirectPolicy, SizeOf(redirectPolicy));

          if (useProxy and (proxyHost <> '')) and ((proxyUser <> '') or (proxyPass <> '')) then
          begin
            if proxyUser <> '' then
            begin
              headerLine := WideString(proxyUser);
              dwSize := (Length(headerLine) + 1) * SizeOf(WideChar);
              WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_USERNAME, PWideChar(headerLine), dwSize);
            end;
            if proxyPass <> '' then
            begin
              headerLine := WideString(proxyPass);
              dwSize := (Length(headerLine) + 1) * SizeOf(WideChar);
              WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_PASSWORD, PWideChar(headerLine), dwSize);
            end;
          end;

          headersToSend := TStringList.Create;
          try
            headersToSend.TextLineBreakStyle := tlbsCRLF;

            if customHeaders <> nil then
              headersToSend.AddStrings(customHeaders);

            cookieHeader := BuildCookieHeader;
            if cookieHeader <> '' then
              headersToSend.Add('Cookie: ' + cookieHeader);

            if jsondata <> '' then
            begin
              if not HasHeader('Content-Type', headersToSend) then
                headersToSend.Add('Content-Type: application/json; charset=UTF-8');
              if not HasHeader('Accept', headersToSend) then
                headersToSend.Add('Accept: application/json');
            end
            else if isPost and (Length(params) > 0) then
            begin
              if not HasHeader('Content-Type', headersToSend) then
                headersToSend.Add('Content-Type: application/x-www-form-urlencoded');
            end;

            for i := 0 to headersToSend.Count - 1 do
            begin
              if Trim(headersToSend[i]) = '' then
                Continue;
              headerLine := WideString(headersToSend[i] + #13#10);
              WinHttpAddRequestHeaders(hRequest, PWideChar(headerLine), Length(headerLine),
                WINHTTP_ADDREQ_FLAG_ADD);
            end;
          finally
            headersToSend.Free;
          end;

          if requestBody <> '' then
          begin
            bodyPtr := @requestBody[1];
            bodyLen := Length(requestBody);
          end
          else
          begin
            bodyPtr := nil;
            bodyLen := 0;
          end;

          if not WinHttpSendRequest(hRequest, nil, 0, bodyPtr, bodyLen, bodyLen, 0) then
          begin
            outError := 'WinHttpSendRequest failed (' + IntToStr(GetLastError) + '): ' +
              SysErrorMessage(GetLastError);
            Exit(false);
          end;

          if not WinHttpReceiveResponse(hRequest, nil) then
          begin
            outError := 'WinHttpReceiveResponse failed (' + IntToStr(GetLastError) + '): ' +
              SysErrorMessage(GetLastError);
            Exit(false);
          end;

          statusValue := 0;
          statusSize := SizeOf(statusValue);
          index := 0;
          if WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER,
            nil, @statusValue, statusSize, index) then
            outStatus := statusValue;

          dwSize := 0;
          index := 0;
          WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF, nil, nil, dwSize, index);
          if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (dwSize > 0) then
          begin
            GetMem(rawHeaderBuf, dwSize);
            try
              if WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF, nil, rawHeaderBuf, dwSize, index) then
              begin
                rawHeaderStr := WideString(rawHeaderBuf);
                outHeaders.Text := UTF8Encode(rawHeaderStr);
              end;
            finally
              FreeMem(rawHeaderBuf);
            end;
          end;

          AppendSetCookieHeaders(hRequest, outHeaders);

          if outStatus = 0 then
            outStatus := ParseStatusCodeFromHeaders(outHeaders);

          locSize := 0;
          index := 0;
          WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_LOCATION, nil, nil, locSize, index);
          if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (locSize > 0) then
          begin
            GetMem(locBuf, locSize);
            try
              if WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_LOCATION, nil, locBuf, locSize, index) then
                outLocation := UTF8Encode(WideString(locBuf));
            finally
              FreeMem(locBuf);
            end;
          end;

          if outLocation = '' then
            outLocation := ExtractLocationHeader(outHeaders);

          responseStream := TStringStream.Create;
          try
            repeat
              dwSize := 0;
              if not WinHttpQueryDataAvailable(hRequest, dwSize) then
              begin
                outError := 'WinHttpQueryDataAvailable failed (' + IntToStr(GetLastError) + '): ' +
                  SysErrorMessage(GetLastError);
                Exit(false);
              end;

              if dwSize = 0 then
                Break;

              dwToRead := dwSize;
              if dwToRead > SizeOf(buffer) then
                dwToRead := SizeOf(buffer);

              if not WinHttpReadData(hRequest, @buffer, dwToRead, dwDownloaded) then
              begin
                outError := 'WinHttpReadData failed (' + IntToStr(GetLastError) + '): ' +
                  SysErrorMessage(GetLastError);
                Exit(false);
              end;
              responseStream.WriteBuffer(buffer, dwDownloaded);
            until dwSize = 0;

            outBody := responseStream.DataString;
          finally
            responseStream.Free;
          end;

          Result := true;
        finally
          WinHttpCloseHandle(hRequest);
        end;
      finally
        WinHttpCloseHandle(hConnect);
      end;
    finally
      WinHttpCloseHandle(hSession);
      // Every failure path leaves via Exit(false), which would skip any
      // cleanup placed after this block — free the header list here so
      // failed attempts don't leak it. On success the caller owns it.
      if not Result then
        FreeAndNil(outHeaders);
    end;
  end;

begin
  address := endpoint;
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

  bodyData := '';
  if jsondata <> '' then
    bodyData := jsondata
  else if post and (Length(params) > 0) then
  begin
    for sx in params do
    begin
      if bodyData <> '' then
        bodyData := bodyData + '&';
      bodyData := bodyData + sx;
    end;
  end;

  if post then
    methodLabel := 'POST'
  else
    methodLabel := 'GET';

  currentUrl := address;
  currentPost := post;

  // See WinHttpRequest: the caller has already normalized host and port.
  proxyHost  := proxy.host;
  proxyPortS := proxy.port;
  proxyUser  := proxy.user;
  proxyPass  := proxy.pass;
  proxyPort  := StrToIntDef(proxyPortS, 8080);

  repeat
    startTick := GetTickCount64;
    TrndiDLog(Format('HTTP %s (winhttp): %s', [methodLabel, TrndiSafeUrl(currentUrl)]));

    // Configured proxy: no direct fallback on any hop, so a redirect chain
    // cannot start on the proxy and finish around it. Nothing configured: use
    // the system's proxy configuration.
    if not TryRequest(currentUrl, currentPost, bodyData, proxyHost <> '',
         responseBody, responseHeaders, statusCode, locationHeader, Result.ErrorMessage) then
      Exit;

    endTick := GetTickCount64;

    Result.Body := responseBody;
    Result.StatusCode := statusCode;
    Result.Headers.Assign(responseHeaders);
    UpdateCookiesFromHeaders(responseHeaders);
    responseHeaders.Free;
    Result.FinalURL := currentUrl;

    TrndiDLog(Format('HTTP %s (winhttp) status=%d, bytes=%d, redirects=%d, ms=%d',
      [methodLabel, Result.StatusCode, Length(Result.Body), Result.RedirectCount, endTick - startTick]));

    if not followRedirects then
      Break;

    if not ((Result.StatusCode = 301) or (Result.StatusCode = 302) or
            (Result.StatusCode = 303) or (Result.StatusCode = 307) or
            (Result.StatusCode = 308)) then
      Break;

    if locationHeader = '' then
      Break;

    Inc(Result.RedirectCount);
    if Result.RedirectCount > maxRedirects then
    begin
      Result.ErrorMessage := 'Too many redirects';
      Exit;
    end;

    currentUrl := ResolveUrl(currentUrl, locationHeader);
    Result.FinalURL := currentUrl;

    if (Result.StatusCode = 303) or (((Result.StatusCode = 301) or (Result.StatusCode = 302)) and currentPost) then
    begin
      currentPost := false;
      bodyData := '';
      methodLabel := 'GET';
    end;
  until false;

  Result.Success := true;
end;

end.
