
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

unit winhttpclient;

interface

uses 
Windows, SysUtils, Classes, StrUtils, winhttp, trndi.log;

type 
HINTERNET = Pointer;
INTERNET_PORT = word;

HTTPPort = record
  port: integer;
  secure: boolean;
end;

TWinHTTPClient = class
private
  FUserAgent: string;
  FHeaders: TStringList;
  FRequestBody: string;
  FProxyHost: string;
  FProxyPort: integer;
  FProxyUser: string;
  FProxyPass: string;
  FForceNoProxy: boolean;
public
  constructor Create(const UserAgent: string = 'Pascal User Agent'); overload;
  constructor Create(const UserAgent: string; ForceNoProxy: boolean); overload;
  constructor Create(const UserAgent: string; const ProxyHost: string; ProxyPort: integer = 8080); overload;
  constructor Create(const UserAgent: string; const ProxyHost: string; ProxyPort: integer; const ProxyUser: string; const ProxyPass: string); overload;
  destructor Destroy;
    override;

  procedure AddHeader(const Name, Value: string);
  procedure SetRequestBody(const Body: string);

  function Get(const URL: string; const Params: array of string): string;
  function Post(const URL: string): string;
end;

const 
winhttpdll = 'winhttp.dll';

function WinHttpOpen(pwszUserAgent: pwidechar; dwAccessType: DWORD;
pwszProxyName, pwszProxyBypass: pwidechar; dwFlags: DWORD): HINTERNET;
stdcall;
external winhttpdll;

function WinHttpConnect(hSession: HINTERNET; pswzServerName: pwidechar;
nServerPort: INTERNET_PORT; dwReserved: DWORD): HINTERNET;
stdcall;
external winhttpdll;

function WinHttpSetOption(hInternet: HINTERNET; dwOption: DWORD;
lpBuffer: Pointer; dwBufferLength: DWORD): BOOL;
stdcall;
external winhttpdll;

function WinHttpSetTimeouts(hInternet: HINTERNET; dwResolveTimeout: Integer;
  dwConnectTimeout: Integer; dwSendTimeout: Integer; dwReceiveTimeout: Integer): BOOL;
stdcall;
external winhttpdll;

function WinHttpOpenRequest(hConnect: HINTERNET; pwszVerb, pwszObjectName,
pwszVersion: pwidechar; pwszReferrer: pwidechar; ppwszAcceptTypes:
PPWideChar;
dwFlags: DWORD): HINTERNET;
stdcall;
external winhttpdll;

function WinHttpAddRequestHeaders(hRequest: HINTERNET; pwszHeaders: pwidechar;
dwHeadersLength: DWORD; dwModifiers: DWORD): BOOL;
stdcall;
external winhttpdll;

function WinHttpSendRequest(hRequest: HINTERNET; pwszHeaders: pwidechar;
dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD;
dwTotalLength: DWORD; dwContext: DWORD_PTR): BOOL;
stdcall;
external winhttpdll;

function WinHttpReceiveResponse(hRequest: HINTERNET; lpReserved: Pointer): BOOL;
stdcall;
external winhttpdll;

function WinHttpQueryDataAvailable(hRequest: HINTERNET; var lpdwNumberOfBytesAvailable: DWORD): BOOL
;
stdcall;
external winhttpdll;

function WinHttpReadData(hRequest: HINTERNET; lpBuffer: Pointer;
dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL;
stdcall;
external winhttpdll;

function WinHttpCloseHandle(hInternet: HINTERNET): BOOL;
stdcall;
external winhttpdll;

const 
  // Proxy and referrer constants
WINHTTP_NO_PROXY_NAME = nil;
WINHTTP_NO_PROXY_BYPASS = nil;
WINHTTP_NO_REFERER = nil;
WINHTTP_DEFAULT_ACCEPT_TYPES: PPWideChar = nil;

  // Access type values
WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
WINHTTP_ACCESS_TYPE_NO_PROXY = 1;
WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;

  // SSL/TLS Protocol flags
WINHTTP_FLAG_SECURE_PROTOCOL_SSL2 = $00000008;
WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 = $00000020;
WINHTTP_FLAG_SECURE_PROTOCOL_TLS1 = $00000080;
WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;
WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3 = $00002000;

WINHTTP_OPTION_SECURITY_FLAGS = $00000031;

// Security flags for WINHTTP_OPTION_SECURITY_FLAGS
SECURITY_FLAG_IGNORE_UNKNOWN_CA = $00000100;
SECURITY_FLAG_IGNORE_CERT_DATE_INVALID = $00002000;
SECURITY_FLAG_IGNORE_CERT_CN_INVALID = $00001000;
SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE = $00000200;

WINHTTP_ADDREQ_FLAG_ADD = $20000000;

// Select allowed SSL/TLS protocols for the session.
// (Do NOT confuse with WINHTTP_OPTION_SECURITY_FLAGS which is for cert validation flags.)
WINHTTP_OPTION_SECURE_PROTOCOLS = 84;

// NOTE: Prefer WinHttpSetTimeouts() over per-option timeouts; option IDs vary between
// headers/implementations and are easy to get wrong.

// WinHTTP option constants not always present in older Pascal headers
WINHTTP_OPTION_PROXY_USERNAME = 43;
WINHTTP_OPTION_PROXY_PASSWORD = 44;


implementation

constructor TWinHTTPClient.Create(const UserAgent: string);
begin
  FUserAgent := UserAgent;
  FHeaders := TStringList.Create;
  FHeaders.TextLineBreakStyle := tlbsCRLF;
  FProxyHost := '';
  FProxyPort := 0;
  FProxyUser := '';
  FProxyPass := '';
  FForceNoProxy := false;
end;

constructor TWinHTTPClient.Create(const UserAgent: string; ForceNoProxy: boolean);
begin
  FUserAgent := UserAgent;
  FHeaders := TStringList.Create;
  FHeaders.TextLineBreakStyle := tlbsCRLF;
  FProxyHost := '';
  FProxyPort := 0;
  FProxyUser := '';
  FProxyPass := '';
  FForceNoProxy := ForceNoProxy;
end;

constructor TWinHTTPClient.Create(const UserAgent: string; const ProxyHost: string; ProxyPort: integer = 8080);
begin
  FUserAgent := UserAgent;
  FHeaders := TStringList.Create;
  FHeaders.TextLineBreakStyle := tlbsCRLF;
  FProxyHost := ProxyHost;
  FProxyPort := ProxyPort;
  FProxyUser := '';
  FProxyPass := '';
  FForceNoProxy := false;
end;

constructor TWinHTTPClient.Create(const UserAgent: string; const ProxyHost: string; ProxyPort: integer; const ProxyUser: string; const ProxyPass: string);
begin
  FUserAgent := UserAgent;
  FHeaders := TStringList.Create;
  FHeaders.TextLineBreakStyle := tlbsCRLF;
  FProxyHost := ProxyHost;
  FProxyPort := ProxyPort;
  FProxyUser := ProxyUser;
  FProxyPass := ProxyPass;
  FForceNoProxy := false;
end;

destructor TWinHTTPClient.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure TWinHTTPClient.AddHeader(const Name, Value: string);
begin
  FHeaders.Add(Name + ': ' + Value);
end;

procedure TWinHTTPClient.SetRequestBody(const Body: string);
begin
  FRequestBody := Body;
end;

procedure ParseURL(const URL: string; out ServerName, Path: string; out port: HTTPPort);

var 
  ProtocolPos, PathPos, PortPos: integer;
  PortStr: string;
begin
  // Find the position of "://"
  ProtocolPos := Pos('://', URL);

  port.secure := false;
  port.port := 80;

  if ProtocolPos > 0 then
  begin
    port.secure := url[protocolpos-1] = 's';
    if port.secure then
      port.port := 443;

    ProtocolPos := ProtocolPos + 3;
  end
  else
    ProtocolPos := 1;
  // Start directly if no protocol is specified

  // Find the next "/" that indicates the beginning of the path
  PathPos := PosEx('/', URL, ProtocolPos);
  // Find if a port is specified after the domain name
  PortPos := PosEx(':', URL, ProtocolPos);

  if (PortPos > 0) and ((PathPos = 0) or (PortPos < PathPos)) then
  begin
      // Extrahera servernamnet fram till portpositionen
    ServerName := Copy(URL, ProtocolPos, PortPos - ProtocolPos);
      // Extrahera porten
    PortStr := Copy(URL, PortPos + 1, PathPos - PortPos - 1);
    Port.port := StrToIntDef(PortStr, Port.port);
      // Convert to integer, or keep default port if invalid
  end
  else
  if PathPos > 0 then
    ServerName := Copy(URL, ProtocolPos, PathPos - ProtocolPos)
  else
    ServerName := Copy(URL, ProtocolPos, Length(URL) - ProtocolPos + 1)// If there is no port, extract the server name up to the path or end
  ;

  // Set the path, if none is found set to "/"
  if PathPos > 0 then
    Path := Copy(URL, PathPos, Length(URL) - PathPos + 1)
  else
    Path := '/';
end;

function TWinHTTPClient.Get(const URL: string; const Params: array of string): string;

var 
  hSession, hConnect, hRequest: HINTERNET;
  ServerName, Path: string;
  Flags, i: DWORD;
  Port: HTTPPort;
  dwSize, dwDownloaded: DWORD;
  dwToRead: DWORD;
  Buffer: array[0..8192] of byte;
  ResponseStream: TStringStream;
  Headers: widestring;
  FullURL: string;
  proxyUserWS, proxyPassWS: WideString;
begin
  Result := '';

  // Bygg URL med GET-parametrar
  FullURL := URL;
  if Length(Params) > 0 then
  begin
    FullURL := FullURL + '?';
    for i := Low(Params) to High(Params) do
    begin
      if i > Low(Params) then
        FullURL := FullURL + '&';
      FullURL := FullURL + Params[i];
    end;
  end;


  ParseURL(FullURL, ServerName, Path, Port);

  // Skapa WinHTTP-session
  if FProxyHost <> '' then
  begin
    hSession := WinHttpOpen(pwidechar(widestring(FUserAgent)), WINHTTP_ACCESS_TYPE_NAMED_PROXY,
      pwidechar(widestring(FProxyHost + ':' + IntToStr(FProxyPort))), WINHTTP_NO_PROXY_BYPASS, 0);
  end
  else if FForceNoProxy then
  begin
    hSession := WinHttpOpen(pwidechar(widestring(FUserAgent)), WINHTTP_ACCESS_TYPE_NO_PROXY,
      WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  end
  else
  begin
    hSession := WinHttpOpen(pwidechar(widestring(FUserAgent)), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
      WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  end;
  if hSession = nil then
    raise Exception.Create('WinHttpOpen failed: ' + SysErrorMessage(GetLastError));

  // Configure TLS protocols for HTTPS on the session handle
  if Port.secure then
  begin
    Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
    if not WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @Flags, SizeOf(Flags)) then
    begin
      Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
      WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @Flags, SizeOf(Flags));
    end;
  end;

  // Set timeouts (DNS/Connect 15s, send 30s, receive 60s)
  if not WinHttpSetTimeouts(hSession, 15000, 15000, 30000, 60000) then
    raise Exception.Create('WinHttpSetTimeouts failed (' + IntToStr(GetLastError) + '): ' +
      SysErrorMessage(GetLastError));

  try
    hConnect := WinHttpConnect(hSession, pwidechar(widestring(ServerName)), Port.port, 0);
    if hConnect = nil then
      raise Exception.Create('WinHttpConnect failed: ' + SysErrorMessage(GetLastError));

    try
      // Set flags depending on whether the connection is secure (HTTPS)
      Flags := 0;
      if Port.secure then
        Flags := WINHTTP_FLAG_SECURE;

      hRequest := WinHttpOpenRequest(hConnect, 'GET', pwidechar(widestring(Path)),
        nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, Flags);
      if hRequest = nil then
        raise Exception.Create('WinHttpOpenRequest failed: ' + SysErrorMessage(GetLastError));

      try
        // For HTTPS, configure certificate validation based on proxy usage
        if Port.secure then
        begin
          if FProxyHost <> '' then
          begin
            // Proxy: ignore cert errors (proxies intercept HTTPS with their own certs)
            Flags := SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
                     SECURITY_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;
            LogMessageToFile('WinHTTP GET: Setting security flags for proxy (' + FProxyHost + '): ' + IntToStr(Flags));
            if not WinHttpSetOption(hRequest, WINHTTP_OPTION_SECURITY_FLAGS, @Flags, SizeOf(Flags)) then
              LogMessageToFile('WinHTTP GET: Failed to set security flags: ' + IntToStr(GetLastError));
          end
          else
          begin
            // Direct: enforce strict validation
            Flags := 0;
            WinHttpSetOption(hRequest, WINHTTP_OPTION_SECURITY_FLAGS, @Flags, SizeOf(Flags));
          end;
        end;

        // Set proxy credentials (proxy auth) when using a named proxy
        if (FProxyHost <> '') and ((FProxyUser <> '') or (FProxyPass <> '')) then
        begin
          if FProxyUser <> '' then
          begin
            proxyUserWS := WideString(FProxyUser);
            dwSize := (Length(proxyUserWS) + 1) * SizeOf(WideChar);
            WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_USERNAME, PWideChar(proxyUserWS), dwSize);
          end;
          if FProxyPass <> '' then
          begin
            proxyPassWS := WideString(FProxyPass);
            dwSize := (Length(proxyPassWS) + 1) * SizeOf(WideChar);
            WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_PASSWORD, PWideChar(proxyPassWS), dwSize);
          end;
        end;

        // Add headers
        if FHeaders.Count > 0 then
        begin
          Headers := widestring(FHeaders.Text);
          WinHttpAddRequestHeaders(hRequest, pwidechar(Headers), Length(Headers),
            WINHTTP_ADDREQ_FLAG_ADD);
        end;

        // Send request
        if not WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) then
          raise Exception.Create('WinHttpSendRequest failed: ' + SysErrorMessage(GetLastError));

        if not WinHttpReceiveResponse(hRequest, nil) then
          raise Exception.Create('WinHttpReceiveResponse failed (' + IntToStr(GetLastError) + '): ' +
            SysErrorMessage(GetLastError));

        // Läs svar
        ResponseStream := TStringStream.Create;
        try
          repeat
            dwSize := 0;
            if not WinHttpQueryDataAvailable(hRequest, dwSize) then
              raise Exception.Create('WinHttpQueryDataAvailable failed (' + IntToStr(GetLastError) + '): ' +
                SysErrorMessage(GetLastError));

            if dwSize = 0 then
              Break;

            // Never read more than our fixed buffer size
            dwToRead := dwSize;
            if dwToRead > SizeOf(Buffer) then
              dwToRead := SizeOf(Buffer);

            if not WinHttpReadData(hRequest, @Buffer, dwToRead, dwDownloaded) then
              raise Exception.Create('WinHttpReadData failed (' + IntToStr(GetLastError) + '): ' +
                SysErrorMessage(GetLastError));

            ResponseStream.WriteBuffer(Buffer, dwDownloaded);
          until dwSize = 0;

          Result := ResponseStream.DataString;
          LogMessageToFile('WinHTTP GET response length: ' + IntToStr(Length(Result)));
          if Length(Result) > 0 then
            LogMessageToFile('WinHTTP GET response preview: ' + Copy(Result, 1, 200));
        finally
          ResponseStream.Free;
        end;

      finally
        WinHttpCloseHandle(hRequest);
      end;
    finally
      WinHttpCloseHandle(hConnect);
    end;
  finally
    WinHttpCloseHandle(hSession);
  end;
end;



function TWinHTTPClient.Post(const URL: string): string;

var 
  hSession, hConnect, hRequest: HINTERNET;
  ServerName, Path: string;
  Flags: DWORD;
  Port: HTTPPort;
  dwSize, dwDownloaded: DWORD;
  dwToRead: DWORD;
  bodyPtr: Pointer;
  bodyLen: DWORD;
  Buffer: array[0..4095] of byte;
  ResponseStream: TStringStream;
  Headers: widestring;
  proxyUserWS, proxyPassWS: WideString;
begin
  Result := '';

  // Extract server name, path, and port
  ParseURL(URL, ServerName, Path, Port);

  // Skapa WinHTTP-session
  if FProxyHost <> '' then
  begin
    hSession := WinHttpOpen(pwidechar(widestring(FUserAgent)), WINHTTP_ACCESS_TYPE_NAMED_PROXY,
      pwidechar(widestring(FProxyHost + ':' + IntToStr(FProxyPort))), WINHTTP_NO_PROXY_BYPASS, 0);
  end
  else if FForceNoProxy then
  begin
    hSession := WinHttpOpen(pwidechar(widestring(FUserAgent)), WINHTTP_ACCESS_TYPE_NO_PROXY,
      WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  end
  else
  begin
    hSession := WinHttpOpen(pwidechar(widestring(FUserAgent)), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
      WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  end;
  if hSession = nil then
    raise Exception.Create('WinHttpOpen failed: ' + SysErrorMessage(GetLastError));

  // Set timeouts (DNS/Connect 15s, send 30s, receive 60s)
  if not WinHttpSetTimeouts(hSession, 15000, 15000, 30000, 60000) then
    raise Exception.Create('WinHttpSetTimeouts failed (' + IntToStr(GetLastError) + '): ' +
      SysErrorMessage(GetLastError));

  // Configure TLS protocols for HTTPS on the session handle
  if Port.secure then
  begin
    Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
    if not WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @Flags, SizeOf(Flags)) then
    begin
      Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
      WinHttpSetOption(hSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @Flags, SizeOf(Flags));
    end;
  end;

  try
    hConnect := WinHttpConnect(hSession, pwidechar(widestring(ServerName)), Port.port, 0);
    if hConnect = nil then
      raise Exception.Create('WinHttpConnect failed: ' + SysErrorMessage(GetLastError));

    try
      // Set flags depending on whether the connection is secure (HTTPS)
      Flags := 0;
      if Port.secure then
        Flags := WINHTTP_FLAG_SECURE;

      hRequest := WinHttpOpenRequest(hConnect, 'POST', pwidechar(widestring(Path)),
        nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, Flags);
      if hRequest = nil then
        raise Exception.Create('WinHttpOpenRequest failed: ' + SysErrorMessage(GetLastError));

      try
        // For HTTPS, configure certificate validation based on proxy usage
        if Port.secure then
        begin
          if FProxyHost <> '' then
          begin
            // Proxy: ignore cert errors (proxies intercept HTTPS with their own certs)
            Flags := SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
                     SECURITY_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;
            LogMessageToFile('WinHTTP POST: Setting security flags for proxy (' + FProxyHost + '): ' + IntToStr(Flags));
            if not WinHttpSetOption(hRequest, WINHTTP_OPTION_SECURITY_FLAGS, @Flags, SizeOf(Flags)) then
              LogMessageToFile('WinHTTP POST: Failed to set security flags: ' + IntToStr(GetLastError));
          end
          else
          begin
            // Direct: enforce strict validation
            Flags := 0;
            WinHttpSetOption(hRequest, WINHTTP_OPTION_SECURITY_FLAGS, @Flags, SizeOf(Flags));
          end;
        end;

        // Set proxy credentials (proxy auth) when using a named proxy
        if (FProxyHost <> '') and ((FProxyUser <> '') or (FProxyPass <> '')) then
        begin
          if FProxyUser <> '' then
          begin
            proxyUserWS := WideString(FProxyUser);
            dwSize := (Length(proxyUserWS) + 1) * SizeOf(WideChar);
            WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_USERNAME, PWideChar(proxyUserWS), dwSize);
          end;
          if FProxyPass <> '' then
          begin
            proxyPassWS := WideString(FProxyPass);
            dwSize := (Length(proxyPassWS) + 1) * SizeOf(WideChar);
            WinHttpSetOption(hRequest, WINHTTP_OPTION_PROXY_PASSWORD, PWideChar(proxyPassWS), dwSize);
          end;
        end;

        // Add headers
        if FHeaders.Count > 0 then
        begin
          Headers := widestring(FHeaders.Text);
          WinHttpAddRequestHeaders(hRequest, pwidechar(Headers), Length(Headers),
            WINHTTP_ADDREQ_FLAG_ADD);
        end;

        // Send POST request with body
        if Length(FRequestBody) > 0 then
        begin
          bodyPtr := @FRequestBody[1];
          bodyLen := Length(FRequestBody);
        end
        else
        begin
          bodyPtr := nil;
          bodyLen := 0;
        end;

        if not WinHttpSendRequest(hRequest, nil, 0, bodyPtr, bodyLen, bodyLen, 0) then
          raise Exception.Create('WinHttpSendRequest failed (' + IntToStr(GetLastError) + '): ' +
            SysErrorMessage(GetLastError));

        if not WinHttpReceiveResponse(hRequest, nil) then
          raise Exception.Create('WinHttpReceiveResponse failed (' + IntToStr(GetLastError) + '): ' +
            SysErrorMessage(GetLastError));

        // Read response
        ResponseStream := TStringStream.Create;
        try
          repeat
            dwSize := 0;
            if not WinHttpQueryDataAvailable(hRequest, dwSize) then
              raise Exception.Create('WinHttpQueryDataAvailable failed (' + IntToStr(GetLastError) + '): ' +
                SysErrorMessage(GetLastError));

            if dwSize = 0 then
              Break;

            // Never read more than our fixed buffer size
            dwToRead := dwSize;
            if dwToRead > SizeOf(Buffer) then
              dwToRead := SizeOf(Buffer);

            if not WinHttpReadData(hRequest, @Buffer, dwToRead, dwDownloaded) then
              raise Exception.Create('WinHttpReadData failed (' + IntToStr(GetLastError) + '): ' +
                SysErrorMessage(GetLastError));

            ResponseStream.WriteBuffer(Buffer, dwDownloaded);
          until dwSize = 0;

          Result := ResponseStream.DataString;
          LogMessageToFile('WinHTTP POST response length: ' + IntToStr(Length(Result)));
          if Length(Result) > 0 then
            LogMessageToFile('WinHTTP POST response preview: ' + Copy(Result, 1, 200));
        finally
          ResponseStream.Free;
        end;

      finally
        WinHttpCloseHandle(hRequest);
      end;
    finally
      WinHttpCloseHandle(hConnect);
    end;
  finally
    WinHttpCloseHandle(hSession);
  end;
end;



end.
