(*
 * This file is part of Trndi (https://github.com/xxxx or http://xxx.github.io).
 * Copyright (c) 2021-24 Björn Lindh.
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
{
 This file contains platform-native code, written to make Trndi faster, smoother
 and to minimize the need of 3rd party libraries.
 It also provides natibe features such as Windows registry and openssl for linux
}

unit trndi.native;

{$I native.inc}

interface

uses
  Classes, SysUtils, graphics
  {$IF DEFINED(X_MAC)},
  NSMisc,
  ns_url_request
  {$ELSEIF DEFINED(X_WIN)},
  Windows, registry, dialogs, StrUtils, winhttpclient
  {$ELSEIF DEFINED(X_PC)},
  fphttpclient, openssl, opensslsockets, IniFiles, dialogs
  {$ENDIF};


type
  TrndiNative = class
  public
    dark: boolean;
    function request(const post: boolean; const endpoint: string; const params: array of string; const jsondata: string = ''; const header: string = ''): string;
    procedure SetSetting(const key: string; const val: string);
    function GetSetting(const key: string; def: string = ''): string;
    function GetIntSetting(const key: string; def: integer = -1): integer;
    function isDarkMode: boolean;
    function HasTouchScreen: boolean;
    class function getURL(const url: string; out res: string): boolean; static;

    destructor  Destroy; override;
    constructor create(ua, base: string);
    constructor create;
  protected
    useragent, baseurl: string;
    {$IF DEFINED(X_WIN)}
//     usessl: boolean;
     {$ELSEIF DEFINED(X_PC)}
     inistore: TINIFile;
    {$ENDIF}

  end;


implementation
destructor TrndiNative.destroy;
begin
 {$IF DEFINED(X_PC)}
 inistore.free;
 {$ENDIF}
end;

{$IF DEFINED(X_WIN)}
function TrndiNative.HasTouchScreen: Boolean;
const
  SM_MAXIMUMTOUCHES = 95;
begin
  Result := GetSystemMetrics(SM_MAXIMUMTOUCHES) > 0;
end;
{$ELSEIF DEFINED(X_MAC}
function TrndiNative.HasTouchScreen: Boolean;
begin
  result := false; // iOS only
end;
{$ELSE}
function TrndiNative.HasTouchScreen: Boolean;
var
  SL: TStringList;
  i: Integer;
begin
  Result := False;
  SL := TStringList.Create;
  try
    if FileExists('/proc/bus/input/devices') then
    begin
      SL.LoadFromFile('/proc/bus/input/devices');
      for i := 0 to SL.Count - 1 do
      begin
        if Pos('Touchscreen', SL[i]) > 0 then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;
{$ENDif}

constructor TrndiNative.create;
begin
  create('Mozilla/5.0 (compatible; trndi) TrndiAPI', '');
end;

constructor TrndiNative.create(ua, base: string);
begin
  useragent := ua;
  baseUrl   := base;
  dark := isDarkMode;

end;

{$IFDEF X_MAC}
// Each ifdef needs a function header or the code formatter goes bananas
function TrndiNative.request(const post: boolean; const endpoint: string; const params: array of string; const jsondata: string = ''; const header: string = ''): string;
var
  res, send: TStringStream;
  headers:   TStringList;
  sparams:   string;
  sx: string;
begin
  res     := TStringStream.create(nil);
  send    := TStringStream.create(nil);
  headers := TStringList.create();

  with TNSHTTPSendAndReceive.create do
   try
      address := Format('%s/%s', [baseUrl, endpoint]);
      if post then
        method := 'POST'
      else
        method := 'GET';

      if header <> '' then
        Headers.Add(header);
      if jsondata <> '' then begin
        Headers.Add('Content-Type=application/json');
        // Not strictly required, but if the API changes it might be a good idea to pass a version we know works
        if useragent <> '' then
          Headers.Add('User-Agent=' + useragent);

        send.Write(jsondata[1], length(jsondata));
        Headers.Add('Content-Length=' + IntToStr(send.Size));
     end
     else if length(params) > 0 then begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
      end;


      if SendAndReceive(send, res, headers) then
        result := trim(res.DataString)
      else
        result := '+' + LastErrMsg;

      // showmessage(send.DataString+' -> ' + res.DataString);
   finally
      free;
      res.free;
      send.free;
      headers.free;
   end;
end;

{$ENDIF}
{$IFDEF WINDOWS}
function TrndiNative.request(const post: boolean; const endpoint: string; const params: array of string; const jsondata: string = ''; const header: string = ''): string;
var
  client: TWinHTTPClient;
  sx, address: string;
  headers: array of string;
  hasParams: boolean;
  res: string;
begin
  hasParams := Length(params) > 0;
  // Skapa klient och ställ in användaragent
  client := TWinHTTPClient.Create(useragent);
  try
    // Ange URL och headers
    address := Format('%s/%s', [TrimRightSet(baseUrl, ['/']), TrimLeftSet(endpoint, ['/'])]);

    client.AddHeader('User-Agent', useragent);

    // Om specifika headers anges, lägg till dem
    if header <> '' then
    begin
      headers := header.Split(['=']);
      if Length(headers) = 2 then
        client.AddHeader(headers[0], headers[1]);
    end;

    // Hantera JSON-data eller parametrar
    if jsondata <> '' then
    begin
      client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      client.AddHeader('Accept', 'application/json');
      client.SetRequestBody(jsondata);
    end
    else if hasParams then
    begin
      address := address + '?';
      for sx in params do
        address := address + '&' + sx;
    end;

    // Skicka GET eller POST-begäran
    try
      if post then
        Res := client.Post(address)
      else
        Res := client.Get(address, params);
    except
    on E: Exception do
      result := E.Message;
    end;

    result := res;
  finally
    client.Free;
  end;
end;

{$ELSE}
{$IFNDEF DARWIN}
function TrndiNative.request(const post: boolean; const endpoint: string; const params: array of string; const jsondata: string = ''; const header: string = ''): string;
var
  client:  TFPHttpClient;
  res:     TStringStream;
  sparams, sx, address: string;
  headers: array of string;
begin

  Client := TFPHttpClient.create(nil);
  Client.AddHeader('User-Agent', useragent);
  address := Format('%s/%s', [baseUrl, endpoint]);
  if header <> '' then begin
    headers := header.split('=');
    client.AddHeader(headers[0], headers[1]);
  end;

  if jsondata <> '' then begin
    Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json');

    client.RequestBody := TRawByteStringStream.create(jsondata);
 end
  else if length(params) > 0 then begin
    address := address + '?';
    for sx in params do
      address := address + '&' + sx;
 end;


  res := TStringStream.create('');

 try
   try
     if post then
      client.Post(address, res)
     else
      client.get(address,res);

      result := res.DataString;
      // writeln('Response Code is ' + inttostr(Client.ResponseStatusCode));   // better be 200

   except
     on E:EHttpClient do
        result := E.Message;
   end;
 finally
    Client.RequestBody.free;
    Client.free;
    Res.free;
 end;
end;

{$endif}
{$endif}

{$if defined(X_WIN)}
function TrndiNative.GetSetting(const key: string; def: string = ''): string;
var
  reg: TRegistry;
begin
  result := def;
  reg    := TRegistry.create;
 try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('\SOFTWARE\Trndi\') then
      result := reg.ReadString(key); //read the value of the default name
 finally
    reg.free;
 end;
end;

{$elseif defined(X_PC)}
function TrndiNative.GetSetting(const key: string; def: string = ''): string;
begin
  if not assigned(inistore) then
    inistore := TINIFile.create(GetAppConfigFile(false));
  result     := inistore.ReadString('trndi', key, def);
end;

{$elseif defined(X_MAC)}
function TrndiNative.GetSetting(const key: string; def: string = ''): string;
begin
  result := GetPrefString(key);
  if result = '' then
    result := def;
end;

{$endif}

function TrndiNative.GetIntSetting(const key: string; def: integer = -1): integer;
var
  r: string;
begin
  r := GetSetting(key, 'fail');
  if not TryStrToint(r, result) then
    result := def;
end;


{$if defined(X_MAC)}
procedure TrndiNative.SetSetting(const key: string; const val: string);
begin
  SetPrefString(key, val);
end;

{$elseif defined(X_WIN)}
procedure TrndiNative.SetSetting(const key: string; const val: string);
var
  reg: TRegistry;
begin
  reg := TRegistry.create;
 try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', true) then
      reg.WriteString(key, val) //read the value of the default name
    else
      ShowMessage('Error saving!');
 finally
    reg.free;
 end;
end;

{$elseif defined(X_PC)}
procedure TrndiNative.SetSetting(const key: string; const val: string);
begin
  if not assigned(inistore) then
    inistore := TINIFile.create(GetAppConfigFile(false));
  inistore.WriteString('trndi', key, val);
end;

{$endif}

function TrndiNative.isDarkMode: boolean;
{$if defined(X_MAC)}
begin
// NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
   result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle')))>0;
{$elseif defined(X_WIN)}
const
  regtheme: string = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  reglight: string = 'AppsUseLightTheme';
var
  reg: TRegistry;
 begin
    reg    := TRegistry.Create(KEY_READ);
  try
    result := false;
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(regtheme) then
      if Reg.OpenKey(regtheme, False) then
      try
        if Reg.ValueExists(reglight) then
          result := Reg.ReadInteger(reglight) = 0;
      finally
        reg.CloseKey;
      end;
  finally
    reg.Free;
  end;
  {$else}
   function _Level(C: TColor): double;
  begin
    Result:= Red(C)*0.3 + Green(C)*0.59 + Blue(C)*0.11;
  end;
begin
  Result:= _Level(ColorToRGB(clWindow)) < _Level(ColorToRGB(clWindowText));
{$endif}
end;

class function TrndiNative.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
{$IF DEFINED(X_MAC)}
var
  send, response: TStringStream;
  headers: TStringList;
  httpClient: TNSHTTPSendAndReceive;
begin
  res := '';
  send := TStringStream.Create('');
  response := TStringStream.Create('');
  headers := TStringList.Create();
  httpClient := TNSHTTPSendAndReceive.Create;
  try
    try
      httpClient.address := url;
      httpClient.method := 'GET';
      headers.Add('User-Agent=' + DEFAULT_USER_AGENT);
      httpClient.Headers := headers;

      if httpClient.SendAndReceive(send, response, headers) then
      begin
        res := Trim(response.DataString);
        Result := True;
      end
      else
      begin
        res := Trim(httpClient.LastErrMsg);
        Result := False;
      end;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := False;
      end;
    end;
  finally
    httpClient.Free;
    send.Free;
    response.Free;
    headers.Free;
  end;
end;
{$ELSEIF DEFINED(X_WIN)}
var
  client: TWinHTTPClient;
  responseStr: string;
begin
  res := '';
  client := TWinHTTPClient.Create(DEFAULT_USER_AGENT);
  try
    try
      responseStr := client.Get(url, []);
      res := responseStr;
      Result := True;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := False;
      end;
    end;
  finally
    client.Free;
  end;
end;
{$ELSE} // Linux/PC
var
  client: TFPHttpClient;
  responseStream: TStringStream;
begin
  res := '';
  client := TFPHttpClient.Create(nil);
  responseStream := TStringStream.Create('');
  try
    try
      client.AddHeader('User-Agent', DEFAULT_USER_AGENT);
      client.Get(url, responseStream);
      res := Trim(responseStream.DataString);
      Result := True;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := False;
      end;
    end;
  finally
    client.Free;
    responseStream.Free;
  end;
end;
{$ENDIF}

end.
