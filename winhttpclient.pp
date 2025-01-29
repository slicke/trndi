
(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit winhttpclient;

interface

uses 
Windows, SysUtils, Classes, StrUtils, winhttp;

type 
  HINTERNET = Pointer;
  INTERNET_PORT = Word;

  HTTPPort = record
    port: integer;
    secure: boolean;
  end;

  TWinHTTPClient = class
    private 
      FUserAgent: string;
      FHeaders: TStringList;
      FRequestBody: string;
    public 
      constructor Create(const UserAgent: string = 'Pascal User Agent');
      destructor Destroy;
      override;

      procedure AddHeader(const Name, Value: string);
      procedure SetRequestBody(const Body: string);

      function Get(const URL: string; const Params: array of string): string;
      function Post(const URL: string): string;
  end;

const 
  winhttpdll = 'winhttp.dll';

function WinHttpOpen(pwszUserAgent: PWideChar; dwAccessType: DWORD;
                     pwszProxyName, pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET;
stdcall;
external winhttpdll;

function WinHttpConnect(hSession: HINTERNET; pswzServerName: PWideChar;
                        nServerPort: INTERNET_PORT; dwReserved: DWORD): HINTERNET;
stdcall;
external winhttpdll;

function WinHttpSetOption(hInternet: HINTERNET; dwOption: DWORD;
                          lpBuffer: Pointer; dwBufferLength: DWORD): BOOL;
stdcall;
external winhttpdll;

function WinHttpOpenRequest(hConnect: HINTERNET; pwszVerb, pwszObjectName,
                            pwszVersion: PWideChar; pwszReferrer: PWideChar; ppwszAcceptTypes:
                            PPWideChar;
                            dwFlags: DWORD): HINTERNET;
stdcall;
external winhttpdll;

function WinHttpAddRequestHeaders(hRequest: HINTERNET; pwszHeaders: PWideChar;
                                  dwHeadersLength: DWORD; dwModifiers: DWORD): BOOL;
stdcall;
external winhttpdll;

function WinHttpSendRequest(hRequest: HINTERNET; pwszHeaders: PWideChar;
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
  WINHTTP_ADDREQ_FLAG_ADD = $20000000;

  WINHTTP_OPTION_CONNECT_TIMEOUT = 2;
  WINHTTP_OPTION_SEND_TIMEOUT = 4;
  WINHTTP_OPTION_RECEIVE_TIMEOUT = 5;


implementation

constructor TWinHTTPClient.Create(const UserAgent: string);
begin
  FUserAgent := UserAgent;
  FHeaders := TStringList.Create;
  FHeaders.TextLineBreakStyle := tlbsCRLF;
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
  ProtocolPos, PathPos, PortPos: Integer;
  PortStr: string;
begin
  // Hitta positionen för "://"
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
  // Börja direkt om inget protokoll anges

  // Hitta nästa "/" som anger början på sökvägen
  PathPos := PosEx('/', URL, ProtocolPos);
  // Hitta om en port är specificerad efter domännamnet
  PortPos := PosEx(':', URL, ProtocolPos);

  if (PortPos > 0) and ((PathPos = 0) or (PortPos < PathPos)) then
    begin
      // Extrahera servernamnet fram till portpositionen
      ServerName := Copy(URL, ProtocolPos, PortPos - ProtocolPos);
      // Extrahera porten
      PortStr := Copy(URL, PortPos + 1, PathPos - PortPos - 1);
      Port.port := StrToIntDef(PortStr, Port.port);
      // Konvertera till integer, eller behåll standardport om ogiltigt
    end
  else
    begin
      // Om det inte finns någon port, extrahera servernamnet fram till sökvägen eller slutet
      if PathPos > 0 then
        ServerName := Copy(URL, ProtocolPos, PathPos - ProtocolPos)
      else
        ServerName := Copy(URL, ProtocolPos, Length(URL) - ProtocolPos + 1);
    end;

  // Sätt sökvägen, om ingen hittades sätt till "/"
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
  Buffer: array[0..8192] of Byte;
  ResponseStream: TStringStream;
  Headers: WideString;
  FullURL: string;
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
  hSession := WinHttpOpen(PWideChar(WideString(FUserAgent)), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
              WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  if hSession = nil then
    raise Exception.Create('WinHttpOpen failed: ' + SysErrorMessage(GetLastError));

  try
    hConnect := WinHttpConnect(hSession, PWideChar(WideString(ServerName)), Port.port, 0);
    if hConnect = nil then
      raise Exception.Create('WinHttpConnect failed: ' + SysErrorMessage(GetLastError));

    try
      // Sätt flaggor beroende på om anslutningen är säker (HTTPS)
      Flags := 0;
      if Port.secure then
        Flags := WINHTTP_FLAG_SECURE;

      hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(WideString(Path)),
                  nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, Flags);
      if hRequest = nil then
        raise Exception.Create('WinHttpOpenRequest failed: ' + SysErrorMessage(GetLastError));

      try
        // Aktivera TLS 1.2 om anslutningen är säker
        if Port.secure then
          begin
            Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
            WinHttpSetOption(hRequest, WINHTTP_OPTION_SECURITY_FLAGS, @Flags, SizeOf(Flags));
          end;

        // Lägg till headers
        if FHeaders.Count > 0 then
          begin
            Headers := WideString(FHeaders.Text);
            WinHttpAddRequestHeaders(hRequest, PWideChar(Headers), Length(Headers),
            WINHTTP_ADDREQ_FLAG_ADD);
          end;

        // Skicka begäran
        if not WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) then
          raise Exception.Create('WinHttpSendRequest failed: ' + SysErrorMessage(GetLastError));

        if not WinHttpReceiveResponse(hRequest, nil) then
          raise Exception.Create('WinHttpReceiveResponse failed: ' + SysErrorMessage(GetLastError));

        // Läs svar
        ResponseStream := TStringStream.Create;
        try
          repeat
            dwSize := 0;
            if not WinHttpQueryDataAvailable(hRequest, dwSize) then
              raise Exception.Create('WinHttpQueryDataAvailable failed: ' + SysErrorMessage(
                                     GetLastError));

            if dwSize = 0 then Break;

            if not WinHttpReadData(hRequest, @Buffer, dwSize, dwDownloaded) then
              raise Exception.Create('WinHttpReadData failed: ' + SysErrorMessage(GetLastError));

            ResponseStream.WriteBuffer(Buffer, dwDownloaded);
          until dwSize = 0;

          Result := ResponseStream.DataString;
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
  Buffer: array[0..4095] of Byte;
  ResponseStream: TStringStream;
  Headers: WideString;
begin
  Result := '';

  // Extrahera servernamn, sökväg och port
  ParseURL(URL, ServerName, Path, Port);

  // Skapa WinHTTP-session
  hSession := WinHttpOpen(PWideChar(WideString(FUserAgent)), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
              WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  if hSession = nil then
    raise Exception.Create('WinHttpOpen failed: ' + SysErrorMessage(GetLastError));

  try
    hConnect := WinHttpConnect(hSession, PWideChar(WideString(ServerName)), Port.port, 0);
    if hConnect = nil then
      raise Exception.Create('WinHttpConnect failed: ' + SysErrorMessage(GetLastError));

    try
      // Sätt flaggor beroende på om anslutningen är säker (HTTPS)
      Flags := 0;
      if Port.secure then
        Flags := WINHTTP_FLAG_SECURE;

      hRequest := WinHttpOpenRequest(hConnect, 'POST', PWideChar(WideString(Path)),
                  nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, Flags);
      if hRequest = nil then
        raise Exception.Create('WinHttpOpenRequest failed: ' + SysErrorMessage(GetLastError));

      try
        // Aktivera TLS 1.2 om anslutningen är säker
        if Port.secure then
          begin
            Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
            WinHttpSetOption(hRequest, WINHTTP_OPTION_SECURITY_FLAGS, @Flags, SizeOf(Flags));
          end;

        // Lägg till headers
        if FHeaders.Count > 0 then
          begin
            Headers := WideString(FHeaders.Text);
            WinHttpAddRequestHeaders(hRequest, PWideChar(Headers), Length(Headers),
            WINHTTP_ADDREQ_FLAG_ADD);
          end;

        // Skicka POST-begäran med body
        if not WinHttpSendRequest(hRequest, nil, 0, PByte(FRequestBody), Length(FRequestBody),
           Length(FRequestBody), 0) then
          raise Exception.Create('WinHttpSendRequest failed: ' + SysErrorMessage(GetLastError));

        if not WinHttpReceiveResponse(hRequest, nil) then
          raise Exception.Create('WinHttpReceiveResponse failed: ' + SysErrorMessage(GetLastError));

        // Läs svar
        ResponseStream := TStringStream.Create;
        try
          repeat
            dwSize := 0;
            if not WinHttpQueryDataAvailable(hRequest, dwSize) then
              raise Exception.Create('WinHttpQueryDataAvailable failed: ' + SysErrorMessage(
                                     GetLastError));

            if dwSize = 0 then Break;

            if not WinHttpReadData(hRequest, @Buffer, dwSize, dwDownloaded) then
              raise Exception.Create('WinHttpReadData failed: ' + SysErrorMessage(GetLastError));

            ResponseStream.WriteBuffer(Buffer, dwDownloaded);
          until dwSize = 0;

          Result := ResponseStream.DataString;
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
