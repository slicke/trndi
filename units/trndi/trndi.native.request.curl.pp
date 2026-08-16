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
  @abstract(libcurl transport shared by the curl-backed native classes.)

  The bodies of @code(getURL), @code(postURL), @code(TestProxyURL),
  @code(request) and @code(requestEx) that historically lived inside
  @code(trndi.native.linux) — extracted verbatim so that any
  @link(TTrndiNativeBase) subclass can route its HTTP through libcurl.
  Consumers today: @code(trndi.native.linux) (desktop Linux/BSD) and
  @code(trndi.native.console) (LCL-free CLI/TUI front ends).

  Design:
  - Depends only on the RTL, the @code(trndi.curl) binding, @code(trndi.log)
    and @code(trndi.native.base) — no LCL, no widgetset.
  - Settings access stays in the calling class: callers resolve their proxy
    configuration (usually via @link(FetchCurlProxy)) and pass it in — no
    settings store is ever touched from here.
  - Behavior contract (same as the Linux implementation always had): a
    configured proxy is used exclusively with no direct fallback; with none
    configured, curl follows the environment's proxy variables. TLS verifies
    against the system CA store; DEBUG builds may set TRNDI_INSECURE_TLS=1
    to disable verification for driver development.
}

unit trndi.native.request.curl;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, StrUtils, ctypes, trndi.curl, trndi.log, trndi.native.base;

type
  {** Proxy configuration handed to every transport function. Empty
      @code(host) means "nothing configured" — curl then follows the
      environment's proxy variables. }
TCurlProxy = record
  host: string;
  port: string;
  user: string;
  pass: string;
end;

{** Read the @code(proxy.*) root settings from @param(inst) and normalize the
    host/port split. Callers own the instance; class-function call sites
    create a short-lived one (with @code(noFree) set) just like the original
    Linux implementations did. }
function FetchCurlProxy(inst: TTrndiNativeBase): TCurlProxy;

{** Simple HTTP GET. A configured proxy is used exclusively (no direct
    fallback); otherwise environment proxy variables apply. }
function CurlGetURL(const url: string; const proxy: TCurlProxy;
out res: string): boolean;

{** Simple HTTP POST (mirrors @link(CurlGetURL)'s proxy contract). }
function CurlPostURL(const url, body, contentType: string;
const proxy: TCurlProxy; out res: string): boolean;

{** Proxy-only HTTP GET used by the settings dialog's "Test proxy" action.
    No direct fallback. }
function CurlTestProxyURL(const url: string; const proxyHost: string;
const proxyPort: string; const proxyUser: string; const proxyPass: string;
out res: string): boolean;

{** HTTP GET/POST returning the body (or a curl error string). Includes the
    DNS-retry path for laptop wake/resume scenarios. }
function CurlRequest(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; const header: string; prefix: boolean;
const proxy: TCurlProxy): string;

{** Cookie-aware, redirect-following HTTP returning the full
    @link(THTTPResponse). }
function CurlRequestEx(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList; prefix: boolean;
const proxy: TCurlProxy): THTTPResponse;

implementation

{------------------------------------------------------------------------------
  Write/header callbacks. All C-compatible globals (no nested/static link).
  Two write variants are kept deliberately: the capped one (request/requestEx)
  aborts on >10MB chunks; the plain one (getURL/postURL/TestProxyURL) writes
  whatever arrives — preserving each call path's historical behavior.
 ------------------------------------------------------------------------------}
function CurlWriteCallback(buffer: pchar; size, nmemb: SizeUInt;
userdata: Pointer): SizeUInt; cdecl;
var
  stream: TStringStream;
  actualSize: SizeUInt;
begin
  Result := size * nmemb;

  if (buffer = nil) or (size = 0) or (nmemb = 0) or (Result = 0) then
    Exit;

  // 10MB single-chunk cap to avoid runaway allocations.
  if Result > 10485760 then
    Exit(0); // abort the transfer instead of silently dropping the chunk

  actualSize := Result;
  stream := TStringStream(userdata);
  if stream = nil then
    Exit;

  try
    if actualSize > 0 then
      stream.WriteBuffer(buffer^, actualSize);
  except
    Result := 0; // signal curl to abort
  end;
end;

function CurlWriteCallbackPlain(buffer: pchar; size, nmemb: SizeUInt;
userdata: Pointer): SizeUInt; cdecl;
var
  Bytes: SizeUInt;
  SS: TStringStream;
begin
  if (userdata = nil) or (buffer = nil) then
  begin
    Result := 0;
    Exit;
  end;
  SS := TStringStream(userdata);
  Bytes := size * nmemb;
  if Bytes > 0 then
    SS.WriteBuffer(buffer^, Bytes);
  Result := Bytes;
end;

function CurlHeaderCallbackEx(buffer: pchar; size, nitems: SizeUInt;
userdata: Pointer): SizeUInt; cdecl;
var
  stream: TStringStream;
  actualSize: SizeUInt;
  headerLine: string;
begin
  Result := size * nitems;

  if (buffer = nil) or (size = 0) or (nitems = 0) or (Result = 0) then
    Exit;

  if Result > 1048576 then // 1MB header limit
    Exit(0); // abort the transfer instead of silently dropping the header

  actualSize := Result;
  stream := TStringStream(userdata);

  if stream = nil then
    Exit;

  try
    SetLength(headerLine, actualSize);
    if actualSize > 0 then
      Move(buffer^, headerLine[1], actualSize);
    stream.WriteString(headerLine);
  except
    Result := 0;
  end;
end;

function FetchCurlProxy(inst: TTrndiNativeBase): TCurlProxy;
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
    // The host field holds whatever the user typed ('http://proxy:3128'), so
    // split it exactly like the settings dialog's test button does.
    NormalizeProxyHostPort(Result.host, Result.port);
  end;
end;

{------------------------------------------------------------------------------
  CurlGetURL
  ----------
  Simple GET using libcurl; returns response text or error.
 ------------------------------------------------------------------------------}
function CurlGetURL(const url: string; const proxy: TCurlProxy;
out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  handle: CURL;
  errCode: CURLcode;
  responseStream: TStringStream;

  {$ifdef DEBUG}
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
  {$endif}

  function PerformRequest(withProxy: boolean): boolean;
  begin
    Result := false;

    // Clear any prior attempt's response (proxy attempt may have written partial data)
    responseStream.Size := 0;
    responseStream.Position := 0;

    handle := curl_easy_init();
    if handle = nil then
    begin
      res := 'curl: failed to init';
      Exit;
    end;

    // Set URL and options
    curl_easy_setopt(handle, CURLOPT_URL, pchar(url));
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(1));
    curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(DEFAULT_USER_AGENT));
    curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, clong(10));
    curl_easy_setopt(handle, CURLOPT_TIMEOUT, clong(30));

    // Set proxy if configured and requested
    if withProxy and (proxy.host <> '') then
    begin
      curl_easy_setopt(handle, CURLOPT_PROXY, pchar(proxy.host));
      if proxy.port <> '' then
        curl_easy_setopt(handle, CURLOPT_PROXYPORT, clong(StrToIntDef(proxy.port, 8080)));
      if (proxy.user <> '') or (proxy.pass <> '') then
      begin
        curl_easy_setopt(handle, CURLOPT_PROXYUSERNAME, pchar(proxy.user));
        curl_easy_setopt(handle, CURLOPT_PROXYPASSWORD, pchar(proxy.pass));
      end;
    end;

    // Write callback
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallbackPlain));
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));

    errCode := curl_easy_perform(handle);
    if errCode <> CURLE_OK then
    begin
      res := string(curl_easy_strerror(errCode));
      Result := false;
    end
    else
    begin
      res := Trim(responseStream.DataString);
      Result := true;
    end;

    curl_easy_cleanup(handle);
  end;

begin
  res := '';
  responseStream := TStringStream.Create('');
  try
    {$ifdef DEBUG}
    if proxy.host <> '' then
    begin
      if (proxy.user <> '') and (proxy.pass <> '') then
        TrndiDLog(Format('HTTP GET: proxy configured (%s:%s) with auth; url=%s', [proxy.host, proxy.port, SafeUrlForLog(url)]))
      else
        TrndiDLog(Format('HTTP GET: proxy configured (%s:%s) no auth; url=%s', [proxy.host, proxy.port, SafeUrlForLog(url)]));
    end
    else
      TrndiDLog(Format('HTTP GET: no proxy configured; url=%s', [SafeUrlForLog(url)]));
    {$endif}

    // A configured proxy is the only route out: no direct fallback, or a dead
    // proxy would silently send the traffic around it.
    if proxy.host <> '' then
    begin
      {$ifdef DEBUG}
      TrndiDLog(Format('HTTP GET: attempting via proxy %s:%s', [proxy.host, proxy.port]));
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

    // Nothing configured: curl follows the environment's proxy variables.
    {$ifdef DEBUG}
    TrndiNetLog('HTTP GET: attempting via system/environment configuration');
    {$endif}
    Result := PerformRequest(false);
    {$ifdef DEBUG}
    if not Result then
      TrndiNetLog('HTTP GET: attempt failed: ' + res);
    {$endif}

  finally
    responseStream.Free;
  end;
end;

{------------------------------------------------------------------------------
  CurlPostURL
  -----------
  Simple HTTP POST using libcurl. A configured proxy is used exclusively, with
  no direct fallback (mirrors CurlGetURL).
 ------------------------------------------------------------------------------}
function CurlPostURL(const url, body, contentType: string;
const proxy: TCurlProxy; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  handle: CURL;
  headers: pcurl_slist;
  errCode: CURLcode;
  responseStream: TStringStream;

  function PerformRequest(withProxy: boolean): boolean;
  begin
    Result := false;
    responseStream.Size := 0;
    responseStream.Position := 0;

    handle := curl_easy_init();
    if handle = nil then
    begin
      res := 'curl: failed to init';
      Exit;
    end;

    curl_easy_setopt(handle, CURLOPT_URL, pchar(url));
    curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(DEFAULT_USER_AGENT));
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(1));
    curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, clong(10));
    curl_easy_setopt(handle, CURLOPT_TIMEOUT, clong(30));

    curl_easy_setopt(handle, CURLOPT_POST, clong(1));
    curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(body));
    curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, clong(Length(body)));

    headers := nil;
    if contentType <> '' then
      headers := curl_slist_append(headers, pchar('Content-Type: ' + contentType));
    if headers <> nil then
      curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

    if withProxy and (proxy.host <> '') then
    begin
      curl_easy_setopt(handle, CURLOPT_PROXY, pchar(proxy.host));
      if proxy.port <> '' then
        curl_easy_setopt(handle, CURLOPT_PROXYPORT, clong(StrToIntDef(proxy.port, 8080)));
      if (proxy.user <> '') or (proxy.pass <> '') then
      begin
        curl_easy_setopt(handle, CURLOPT_PROXYUSERNAME, pchar(proxy.user));
        curl_easy_setopt(handle, CURLOPT_PROXYPASSWORD, pchar(proxy.pass));
      end;
    end;
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallbackPlain));
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));

    errCode := curl_easy_perform(handle);
    if errCode <> CURLE_OK then
    begin
      res := string(curl_easy_strerror(errCode));
      Result := false;
    end
    else
    begin
      res := Trim(responseStream.DataString);
      Result := true;
    end;

    if headers <> nil then
    begin
      curl_slist_free_all(headers);
      headers := nil;
    end;
    curl_easy_cleanup(handle);
  end;

begin
  res := '';
  responseStream := TStringStream.Create('');
  try
    // Strict: a configured proxy is never bypassed (mirrors CurlGetURL).
    if proxy.host <> '' then
      Result := PerformRequest(true)
    else
      Result := PerformRequest(false);
  finally
    responseStream.Free;
  end;
end;

{------------------------------------------------------------------------------
  CurlTestProxyURL
  ----------------
  Proxy-only HTTP GET using cURL. No direct fallback.
 ------------------------------------------------------------------------------}
function CurlTestProxyURL(const url: string; const proxyHost: string;
const proxyPort: string; const proxyUser: string; const proxyPass: string;
out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  handle: CURL;
  errCode: CURLcode;
  responseStream: TStringStream;
  host, portS, user, pass: string;

begin
  res := '';
  Result := false;

  host := Trim(proxyHost);
  portS := Trim(proxyPort);
  user := Trim(proxyUser);
  pass := proxyPass; // keep password as-is (may contain spaces)
  NormalizeProxyHostPort(host, portS);

  if host = '' then
  begin
    res := 'Proxy host is empty.';
    Exit(false);
  end;

  responseStream := TStringStream.Create('');
  try
    handle := curl_easy_init();
    if handle = nil then
    begin
      res := 'curl: failed to init';
      Exit(false);
    end;

    try
      curl_easy_setopt(handle, CURLOPT_URL, pchar(url));
      curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(1));
      curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(DEFAULT_USER_AGENT));
      curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, clong(10));
      curl_easy_setopt(handle, CURLOPT_TIMEOUT, clong(30));

      curl_easy_setopt(handle, CURLOPT_PROXY, pchar(host));
      if portS <> '' then
        curl_easy_setopt(handle, CURLOPT_PROXYPORT, clong(StrToIntDef(portS, 8080)));
      if (user <> '') or (pass <> '') then
      begin
        curl_easy_setopt(handle, CURLOPT_PROXYUSERNAME, pchar(user));
        curl_easy_setopt(handle, CURLOPT_PROXYPASSWORD, pchar(pass));
      end;

      curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallbackPlain));
      curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));

      errCode := curl_easy_perform(handle);
      if errCode <> CURLE_OK then
      begin
        res := string(curl_easy_strerror(errCode));
        Result := false;
      end
      else
      begin
        res := Trim(responseStream.DataString);
        Result := true;
      end;
    finally
      curl_easy_cleanup(handle);
    end;
  finally
    responseStream.Free;
  end;
end;

{------------------------------------------------------------------------------
  CurlRequestEx
  -------------
  Cookie-aware, redirect-following HTTP via libcurl. TLS certificates are
  verified against the system CA store. DEBUG builds can disable verification
  via TRNDI_INSECURE_TLS=1 for driver development behind an intercepting proxy.
 ------------------------------------------------------------------------------}
function CurlRequestEx(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList; prefix: boolean;
const proxy: TCurlProxy): THTTPResponse;
var
  handle: CURL;
  headers: pcurl_slist;
  errCode: CURLcode;
  address, sx: string;
  maskedSx: string;
  i, j: integer;
  responseStream: TStringStream;
  headerStream: TStringStream;
  cookieData: string;
  responseLine: string;
  responseCode: clong;
  redirectCountVal: clong;
  effectiveUrl: pchar;
  startTick: QWord;
  endTick: QWord;
  methodLabel: string;
  cookieVal: string;
  cookiePos: integer;

  function HasHeader(const AName: string): boolean;
  var
    k: integer;
    nameLower: string;
  begin
    Result := false;
    if customHeaders = nil then
      Exit;
    nameLower := LowerCase(AName) + ':';
    for k := 0 to customHeaders.Count - 1 do
      if Pos(nameLower, LowerCase(Trim(customHeaders[k]))) = 1 then
        Exit(true);
  end;

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

begin
  if prefix then
    address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])])
  else
    address := endpoint;

  Result.Body := '';
  Result.Headers := TStringList.Create;
  Result.Cookies := TStringList.Create;
  Result.Success := false;
  Result.StatusCode := 0;
  Result.RedirectCount := 0;
  Result.FinalURL := '';
  Result.ErrorMessage := '';

  // GET: append query string. POST: query goes in the body, not the URL.
  if (not post) and (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?' + params[0];
    for j := 1 to High(params) do
      address := address + '&' + params[j];
  end;

  headers := nil;
  responseStream := TStringStream.Create('');
  headerStream := TStringStream.Create('');
  try
    if post then
      methodLabel := 'POST'
    else
      methodLabel := 'GET';
    startTick := GetTickCount64;
    TrndiDLog(Format('HTTP %s (curl): %s', [methodLabel, TrndiSafeUrl(address)]));
    handle := curl_easy_init();
    if handle = nil then
    begin
      Result.ErrorMessage := 'Failed to initialize CURL';
      Exit;
    end;

    try
      curl_easy_setopt(handle, CURLOPT_URL, pchar(address));
      curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, clong(10));
      curl_easy_setopt(handle, CURLOPT_TIMEOUT, clong(30));
      curl_easy_setopt(handle, CURLOPT_ACCEPT_ENCODING, pchar(''));
      {$ifdef DEBUG}
      curl_easy_setopt(handle, CURLOPT_VERBOSE, clong(1));
      // Allow intercepting proxies (e.g. mitmproxy) during driver development only
      if GetEnvironmentVariable('TRNDI_INSECURE_TLS') = '1' then
      begin
        curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, clong(0));
        curl_easy_setopt(handle, CURLOPT_SSL_VERIFYHOST, clong(0));
      end;
      {$endif}

      if useragent <> '' then
        curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(useragent));

      // Same proxy.* root settings as CurlRequest/CurlGetURL, and like them a
      // configured proxy is used exclusively — no direct fallback, so a
      // redirect chain cannot start on the proxy and finish around it.
      if proxy.host <> '' then
      begin
        curl_easy_setopt(handle, CURLOPT_PROXY, pchar(proxy.host));
        if proxy.port <> '' then
          curl_easy_setopt(handle, CURLOPT_PROXYPORT, clong(StrToIntDef(proxy.port, 8080)));
        if (proxy.user <> '') or (proxy.pass <> '') then
        begin
          curl_easy_setopt(handle, CURLOPT_PROXYUSERNAME, pchar(proxy.user));
          curl_easy_setopt(handle, CURLOPT_PROXYPASSWORD, pchar(proxy.pass));
        end;
      end;

      if followRedirects then
      begin
        curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(1));
        curl_easy_setopt(handle, CURLOPT_MAXREDIRS, clong(maxRedirects));
      end
      else
        curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(0));

      if (cookieJar <> nil) and (cookieJar.Count > 0) then
      begin
        cookieData := '';
        for i := 0 to cookieJar.Count - 1 do
        begin
          if Trim(cookieJar[i]) = '' then
            Continue;
          if cookieData <> '' then
            cookieData := cookieData + '; ';
          cookieData := cookieData + cookieJar[i];
        end;
        if cookieData <> '' then
          curl_easy_setopt(handle, CURLOPT_COOKIE, pchar(cookieData));
      end;

      if customHeaders <> nil then
        for i := 0 to customHeaders.Count - 1 do
          headers := curl_slist_append(headers, pchar(customHeaders[i]));

      if jsondata <> '' then
      begin
        if not HasHeader('Content-Type') then
          headers := curl_slist_append(headers, pchar('Content-Type: application/json; charset=UTF-8'));
        if not HasHeader('Accept') then
          headers := curl_slist_append(headers, pchar('Accept: application/json'));
        curl_easy_setopt(handle, CURLOPT_POST, clong(1));
        curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(jsondata));
        curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, clong(Length(jsondata)));
      end
      else if post then
      begin
        if not HasHeader('Content-Type') then
          headers := curl_slist_append(headers, pchar('Content-Type: application/x-www-form-urlencoded'));

        if Length(params) > 0 then
        begin
          sx := '';
          for j := 0 to High(params) do
          begin
            if j > 0 then
              sx := sx + '&';
            sx := sx + params[j];
          end;

          maskedSx := sx;
          MaskParam(maskedSx, 'code_verifier');
          MaskParam(maskedSx, 'code');
          MaskParam(maskedSx, 'password');
          MaskParam(maskedSx, 'client_secret');
          TrndiNetLog('HTTP POST body (masked): ' + Copy(maskedSx, 1, 2000));

          curl_easy_setopt(handle, CURLOPT_POST, clong(1));
          curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(sx));
          curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, clong(Length(sx)));
        end
        else
          curl_easy_setopt(handle, CURLOPT_POST, clong(1));
      end;

      if headers <> nil then
        curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

      curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallback));
      curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));
      curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION, Pointer(@CurlHeaderCallbackEx));
      curl_easy_setopt(handle, CURLOPT_HEADERDATA, Pointer(headerStream));

      errCode := curl_easy_perform(handle);

      if errCode = CURLE_OK then
      begin
        endTick := GetTickCount64;
        Result.Success := true;
        Result.Body := responseStream.DataString;

        curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, @responseCode);
        Result.StatusCode := responseCode;

        curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, @effectiveUrl);
        if effectiveUrl <> nil then
          Result.FinalURL := string(effectiveUrl);

        redirectCountVal := 0;
        curl_easy_getinfo(handle, CURLINFO_REDIRECT_COUNT, @redirectCountVal);
        Result.RedirectCount := redirectCountVal;

        TrndiDLog(Format('HTTP %s (curl) ok: status=%d, bytes=%d, redirects=%d, ms=%d',
          [methodLabel, Result.StatusCode, Length(Result.Body), Result.RedirectCount, endTick - startTick]));

        headerStream.Position := 0;
        while headerStream.Position < headerStream.Size do
        begin
          responseLine := '';
          while headerStream.Position < headerStream.Size do
          begin
            i := Ord(headerStream.ReadByte);
            if i = 10 then // LF
              Break;
            if i <> 13 then // skip CR
              responseLine := responseLine + Chr(i);
          end;

          responseLine := Trim(responseLine);
          if responseLine <> '' then
          begin
            // HTTP/2 and envoy-fronted servers send lowercase header names
            if Pos('set-cookie:', LowerCase(responseLine)) = 1 then
            begin
              cookieVal := Trim(Copy(responseLine, Length('set-cookie:') + 1, MaxInt));
              cookiePos := Pos(';', cookieVal);
              if cookiePos > 0 then
                cookieVal := Copy(cookieVal, 1, cookiePos - 1);
              if cookieVal <> '' then
              begin
                Result.Cookies.Add(cookieVal);
                if cookieJar <> nil then
                begin
                  // Replace a stale value for the same cookie name, so a
                  // rotated session cookie doesn't get sent twice
                  cookiePos := Pos('=', cookieVal);
                  j := -1;
                  if cookiePos > 0 then
                    for i := 0 to cookieJar.Count - 1 do
                      if Pos(Copy(cookieVal, 1, cookiePos), cookieJar[i]) = 1 then
                      begin
                        j := i;
                        Break;
                      end;
                  if j >= 0 then
                    cookieJar[j] := cookieVal
                  else if cookieJar.IndexOf(cookieVal) = -1 then
                    cookieJar.Add(cookieVal);
                end;
              end;
            end;
            Result.Headers.Add(responseLine);
          end;
        end;
      end
      else
      begin
        endTick := GetTickCount64;
        Result.Success := false;
        Result.ErrorMessage := string(curl_easy_strerror(errCode));
        TrndiDLog(Format('HTTP %s (curl) error: code=%d, msg=%s, ms=%d',
          [methodLabel, Ord(errCode), Result.ErrorMessage, endTick - startTick]));
      end;

    finally
      curl_easy_cleanup(handle);
    end;
  finally
    if headers <> nil then
      curl_slist_free_all(headers);
    responseStream.Free;
    headerStream.Free;
  end;
end;

{------------------------------------------------------------------------------
  CurlRequest
  -----------
  HTTP GET/POST via libcurl. A configured proxy carries the request with no
  direct fallback, and with nothing configured curl follows the environment's
  proxy variables. Includes a DNS-retry path for laptop wake/resume scenarios.
 ------------------------------------------------------------------------------}
function CurlRequest(const post: boolean;
const baseurl, useragent, endpoint: string; const params: array of string;
const jsondata: string; const header: string; prefix: boolean;
const proxy: TCurlProxy): string;
var
  handle: CURL;
  headers: pcurl_slist;
  errCode: CURLcode;
  address, sx: string;
  p: integer;
  key, val: string;
  responseStream: TStringStream;
  useProxy: boolean;

  function IsDnsResolveError(const code: CURLcode): boolean;
  begin
    Result := code = CURLE_COULDNT_RESOLVE_HOST;
  end;

  function PerformRequest(withProxy: boolean): boolean;
  var
    j: integer;
  begin
    Result := false;
    responseStream.Size := 0;
    responseStream.Position := 0;

    handle := curl_easy_init();
    if handle = nil then
    begin
      errCode := CURLE_FAILED_INIT;
      Exit(false);
    end;

    curl_easy_setopt(handle, CURLOPT_URL, pchar(address));
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, clong(1));
    curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, clong(10));
    curl_easy_setopt(handle, CURLOPT_TIMEOUT, clong(30));

    if useragent <> '' then
      curl_easy_setopt(handle, CURLOPT_USERAGENT, pchar(useragent));

    if withProxy and (proxy.host <> '') then
    begin
      curl_easy_setopt(handle, CURLOPT_PROXY, pchar(proxy.host));
      if proxy.port <> '' then
        curl_easy_setopt(handle, CURLOPT_PROXYPORT, clong(StrToIntDef(proxy.port, 8080)));
      if (proxy.user <> '') or (proxy.pass <> '') then
      begin
        curl_easy_setopt(handle, CURLOPT_PROXYUSERNAME, pchar(proxy.user));
        curl_easy_setopt(handle, CURLOPT_PROXYPASSWORD, pchar(proxy.pass));
      end;
    end;

    if headers <> nil then
      curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

    if jsondata <> '' then
    begin
      curl_easy_setopt(handle, CURLOPT_POST, clong(1));
      curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(jsondata));
      curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, clong(Length(jsondata)));
    end
    else if post then
    begin
      if Length(params) > 0 then
      begin
        sx := '';
        for j := 0 to High(params) do
        begin
          if j > 0 then
            sx := sx + '&';
          sx := sx + params[j];
        end;
        curl_easy_setopt(handle, CURLOPT_POST, clong(1));
        curl_easy_setopt(handle, CURLOPT_POSTFIELDS, pchar(sx));
      end
      else
        curl_easy_setopt(handle, CURLOPT_POST, clong(1));
    end;

    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, Pointer(@CurlWriteCallback));
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(responseStream));

    errCode := curl_easy_perform(handle);
    curl_easy_cleanup(handle);
    Result := (errCode = CURLE_OK);
  end;

begin
  Result := '';

  if prefix then
    address := Format('%s/%s', [baseurl, endpoint])
  else
    address := endpoint;

  if (jsondata = '') and (Length(params) > 0) then
  begin
    address := address + '?' + params[0];
    for p := 1 to High(params) do
      address := address + '&' + params[p];
  end;

  headers := nil;
  responseStream := TStringStream.Create('');
  try
    if header <> '' then
    begin
      p := Pos('=', header);
      if p > 0 then
      begin
        key := Trim(Copy(header, 1, p - 1));
        val := Trim(Copy(header, p + 1, MaxInt));
        if key <> '' then
          headers := curl_slist_append(headers, pchar(Format('%s: %s', [key, val])));
      end;
    end;

    if jsondata <> '' then
    begin
      headers := curl_slist_append(headers, pchar('Content-Type: application/json; charset=UTF-8'));
      headers := curl_slist_append(headers, pchar('Accept: application/json'));
    end;

    // A configured proxy carries the request or the request fails - it is
    // never bypassed. The DNS retry stays either way: after a laptop resume
    // the first attempt can fail to resolve the proxy just as easily as the
    // target host.
    useProxy := proxy.host <> '';
    if PerformRequest(useProxy) then
      Result := responseStream.DataString
    else if IsDnsResolveError(errCode) then
    begin
      Sleep(1500); // allow DNS/network stack to settle after resume
      if PerformRequest(useProxy) then
        Result := responseStream.DataString
      else
        Result := string(curl_easy_strerror(errCode));
    end
    else
      Result := string(curl_easy_strerror(errCode));
  finally
    if headers <> nil then
      curl_slist_free_all(headers);
    responseStream.Free;
  end;
end;

initialization
  // libcurl requires a one-shot global init before any per-handle use, ideally
  // before threads start. Doing it here (rather than lazily on first request)
  // means the first HTTP call can't race with init.
  curl_global_init(CURL_GLOBAL_DEFAULT);

end.
