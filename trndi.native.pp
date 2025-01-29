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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

{
  This file contains platform-native code, written to make Trndi faster and smoother,
  and to minimize the need for 3rd-party libraries. It also provides native features
  such as Windows registry access and OpenSSL on Linux.
}

unit trndi.native;

{$I native.inc} // Depending on your project setup, this may define X_WIN, X_PC, X_MAC, etc.

interface

uses
Classes, SysUtils, Graphics
{$IF DEFINED(X_MAC)},
NSMisc,
ns_url_request
{$ELSEIF DEFINED(X_WIN)},
Windows, Registry, Dialogs, StrUtils, winhttpclient, shellapi
{$ELSEIF DEFINED(X_PC)},
fphttpclient, openssl, opensslsockets, IniFiles, Dialogs
{$ENDIF}
, process;

type
  { TrndiNative
    -----------
    Provides platform-native methods for:
    - Sending HTTP(S) requests (GET/POST)
    - Storing and retrieving settings (Windows Registry, .ini on Linux, etc.)
    - Checking for dark mode
    - Detecting touchscreen availability
    - Additional platform-specific utilities
  }
TrndiNative = class
public
    // Indicates if the user system is in a "dark mode" theme
  dark: boolean;


  { attention
    Flash the menu bar
  }
  procedure attention(message: string);
    { request
      -------
      Sends an HTTP request (either GET or POST) to the given endpoint.
      - post: True for POST, False for GET
      - endpoint: the relative or full endpoint
      - params: array of key-value pairs (e.g. 'name=value')
      - jsondata: string with JSON payload (only if POSTing JSON)
      - header: optional custom header, e.g. 'Content-Type=xxx'
      Returns the server response as a string.
    }
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''): string;

    { SetSetting
      ----------
      Stores a string setting, depending on the platform:
       - Windows: registry
       - Linux: INI file
       - macOS: NSUserDefaults (if implemented in your code)
    }
  procedure SetSetting(const key: string; const val: string);

    { GetSetting
      ----------
      Retrieves a string setting from the local store. If the key is not found,
      returns `def` by default.
    }
  function GetSetting(const key: string; def: string = ''): string;

    { GetIntSetting
      -------------
      Same as GetSetting, but returns an integer. Returns `def` if parse fails.
    }
  function GetIntSetting(const key: string; def: integer = -1): integer;

    { isDarkMode
      ----------
      Returns True if the system theme is "dark mode", else False. Implementation
      depends on the platform (Windows registry, macOS defaults, Linux color checks, etc.).
    }
  function isDarkMode: boolean;

    { HasTouchScreen
      --------------
      Returns True if the current machine supports a touch screen. This logic
      is platform-specific:
       - Windows uses `GetSystemMetrics(SM_MAXIMUMTOUCHES)`
       - macOS always returns False
       - Linux checks `/proc/bus/input/devices`
    }
  function HasTouchScreen(out multi: boolean): boolean;

    { getURL (class function)
      -----------------------
      A simple helper that performs a GET request for `url` and returns
      the response in `res`. Returns True on success, False on error.
      This is a static method, so it can be called without an instance.
    }
  class function getURL(const url: string; out res: string): boolean; static;

    // Constructor/Destructor
  destructor Destroy; override;

    { create
      ------
      1) Constructor with custom user-agent and base URL.
      2) A default parameterless constructor also exists.
    }
  constructor create(ua, base: string); overload;
  constructor create; overload;
protected
  useragent: string;  // HTTP User-Agent string
  baseurl:   string;  // Base URL for requests

  {$IF DEFINED(X_PC)}
  inistore: TINIFile; // Linux/PC settings store
  {$ENDIF}

end;

implementation

{------------------------------------------------------------------------------
  TrndiNative.Destroy
  -------------------
  Cleans up any allocated resources. On Linux/PC, frees the INI file handle.
 ------------------------------------------------------------------------------}
destructor TrndiNative.Destroy;
begin
  {$IF DEFINED(X_PC)}
  if Assigned(inistore) then
    inistore.Free;
  {$ENDIF}
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TrndiNative.HasTouchScreen
  --------------------------
  Platform-specific detection of touch hardware.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
function TrndiNative.HasTouchScreen(out multi: boolean): boolean;
const
  TABLET_CONFIG_NONE = $00000000;
  NID_INTEGRATED_TOUCH = $00000001;
  NID_EXTERNAL_TOUCH = $00000002;
  NID_INTEGRATED_PEN = $00000004;
  NID_EXTERNAL_PEN = $00000008;
  NID_MULTI_INPUT = $00000040;
  NID_READY = $00000080;
function IsTouchReady: boolean;
  var
    value: integer;
  begin
    value := GetSystemMetrics(SM_DIGITIZER);
    Result := value and NID_READY <> 0;
  end;

function IsMultiTouch: boolean;
  var
    value: integer;
  begin
    value := GetSystemMetrics(SM_DIGITIZER);
    Result := value and NID_MULTI_INPUT <> 0;
  end;

function HasIntegratedTouch: boolean;
  var
    value: integer;
  begin
    value := GetSystemMetrics(SM_DIGITIZER);
    Result := value and NID_INTEGRATED_TOUCH <> 0;
  end;
var
  val: integer;
const
  SM_MAXIMUMTOUCHES = 95;
begin
  result := (HasIntegratedTouch) and (IsTouchReady);
  multi := IsMultiTouch;
end;
{$ELSEIF DEFINED(X_MAC)}
function TrndiNative.HasTouchScreen(out multi: boolean): boolean;
begin
  // macOS: Typically no standard touchscreen (unless iOS)
  Result := false;
  multi := false;
end;
{$ELSE}
function TrndiNative.HasTouchScreen(out multi: boolean): boolean;
var
  SL: TStringList;
  i: integer;
  currentDevice: string;
  foundTouch: boolean;
begin
  Result := false;
  multi := false;
  foundTouch := false;

  SL := TStringList.Create;
  try
    if FileExists('/proc/bus/input/devices') then
    begin
      SL.LoadFromFile('/proc/bus/input/devices');
      currentDevice := '';

      for i := 0 to SL.Count - 1 do
      begin
        // Store current line for analysis
        currentDevice := SL[i];

        // Check for touchscreen presence
        if Pos('Touchscreen', currentDevice) > 0 then
        begin
          Result := true;
          foundTouch := true;
        end;

        // Check for multi-touch indicators
        // Common indicators in the device file
        if foundTouch and (
          (Pos('ABS_MT_POSITION', currentDevice) > 0) or
          (Pos('ABS_MT_SLOT', currentDevice) > 0) or
          (Pos('ABS_MT_TRACKING_ID', currentDevice) > 0))
        then
        begin
          multi := true;
          Break; // We found both touch and multi-touch
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.create (default)
  ----------------------------
  An empty constructor that defaults useragent/baseurl to minimal placeholders.
 ------------------------------------------------------------------------------}
constructor TrndiNative.create;
begin
  // Provide a default user-agent and empty base URL
  create('Mozilla/5.0 (compatible; trndi) TrndiAPI', '');
end;

{------------------------------------------------------------------------------
  TrndiNative.create (overload)
  -----------------------------
  Allows specifying a custom user-agent and a base URL.
 ------------------------------------------------------------------------------}
constructor TrndiNative.create(ua, base: string);
begin
  useragent := ua;
  baseurl   := base;
  // Check if we're in dark mode on creation
  dark := isDarkMode;
end;

{------------------------------------------------------------------------------
  TrndiNative.attention
  -------------------
  Flashes something depending on the system
 ------------------------------------------------------------------------------}
procedure TrndiNative.attention(message: string);
{$if  DEFINED(X_LINUX)}
function IsNotifySendAvailable: boolean;
  var
    AProcess: TProcess;
    OutputString: string;
    OutputLines: TStringList;
  begin
    Result := false;

  // Försök att hitta sökvägen till notify-send
    AProcess := TProcess.Create(nil);
    OutputLines := TStringList.Create;
    try
      AProcess.Executable := '/usr/bin/which';
      AProcess.Parameters.Add('notify-send');
      AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
      AProcess.Execute;

    // Läs utdata från processen
      OutputString := '';
      while AProcess.Output.NumBytesAvailable > 0 do
      begin
        SetLength(OutputString, Length(OutputString) + 1024);
        AProcess.Output.ReadBuffer(Pointer(OutputString)^, 1024);
      end;
      AProcess.WaitOnExit;

    // Rensa onödiga tecken
      OutputString := Trim(OutputString);

    // Kontrollera om sökvägen är returnerad
      if (Length(OutputString) > 0) and FileExists(OutputString) then
        Result := true;
    except
      on E: Exception do
      begin
      // Hantera eventuella fel, t.ex. logga eller ignorera
        Result := false;
      end;
    end;

    OutputLines.Free;
    AProcess.Free;
  end;

procedure SendNotification(Title, Message: string);
  var
    AProcess: TProcess;
  begin
    if IsNotifySendAvailable then
    begin
      AProcess := TProcess.Create(nil);
      try
        AProcess.Executable := '/usr/bin/notify-send';
        AProcess.Parameters.Add(Title);
        AProcess.Parameters.Add(Message);
        AProcess.Options := AProcess.Options + [poNoConsole];
        AProcess.Execute;
      finally
        AProcess.Free;
      end;
    end
    else
      ShowMessage('Notifieringsfunktionen är inte tillgänglig eftersom "notify-send" inte är installerat.')// Hantera fallet där notify-send inte är installerat
// Alternativt kan du välja att använda en annan notifieringsmetod eller inaktivera notifieringsfunktionen
    ;
  end;
  {$endif}
  {$if defined(X_WIN)}

procedure SendNotification(const title, msg: string); // Do this with create process as it seems to work best for PS
  var
    Command: string;
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    CommandLine: string;
  begin

    Command := Format(
      'New-BurntToastNotification ' +
    //'-AppId Trndi ' +
      '-AppLogo ' + ParamStr(0) +' '+  // This will just show up black
      '-Text ''%s'', ' +
    '''%s'' ',
      [title, msg]
      );


    CommandLine := 'powershell.exe -NoProfile -ExecutionPolicy Bypass -Command Import-Module BurntToast; ' + Command;

    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;

    if not CreateProcess(
      nil,
      pchar(CommandLine),
      nil,
      nil,
      false,
      CREATE_NO_WINDOW,
      nil,
      nil,
      StartupInfo,
      ProcessInfo
      ) then
      RaiseLastOSError
    else
    begin
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ProcessInfo.hProcess);
    end;
  end;

  {$endif}
begin
  SendNotification('Trndi', message);
end;

{------------------------------------------------------------------------------
  TrndiNative.request
  -------------------
  Sends a GET or POST request, depending on the "post" parameter.
  Behavior differs by platform. Each platform has its own implementation block:
    - X_MAC uses TNSHTTPSendAndReceive
    - X_WIN uses TWinHTTPClient
    - X_PC (Linux) uses TFPHttpClient with OpenSSL
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
function TrndiNative.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''): string;
var
  res, send: TStringStream;
  headers: TStringList;
  sx: string;
begin
  res := TStringStream.Create('');
  send := TStringStream.Create('');
  headers := TStringList.Create;
  try
    // We use a custom TNSHTTPSendAndReceive
    with TNSHTTPSendAndReceive.Create do
    try
      address := Format('%s/%s', [baseurl, endpoint]);
      if post then
        method := 'POST'
      else
        method := 'GET';

      // If a custom header is provided
      if header <> '' then
        Headers.Add(header);

      // If we have JSON data, we assume it's for POST
      if jsondata <> '' then
      begin
        Headers.Add('Content-Type=application/json');
        if useragent <> '' then
          Headers.Add('User-Agent=' + useragent);

        // Write JSON to send stream
        send.Write(jsondata[1], Length(jsondata));
        Headers.Add('Content-Length=' + IntToStr(send.Size));
      end
      else
      if Length(params) > 0 then
      begin
        // If we have query params, append them
        address := address + '?';
        for sx in params do
          address := address + '&' + sx;
      end;

      // Perform the request
      if SendAndReceive(send, res, headers) then
        Result := Trim(res.DataString)
      else
        Result := '+' + LastErrMsg;
    finally
      Free; // free the TNSHTTPSendAndReceive
    end;
  finally
    res.Free;
    send.Free;
    headers.Free;
  end;
end;

{$ELSEIF DEFINED(X_WIN)}
function TrndiNative.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''): string;
var
  client: TWinHTTPClient;
  sx, address: string;
  headers: array of string;
  hasParams: boolean;
  ResStr: string;
begin
  hasParams := (Length(params) > 0);
  client := TWinHTTPClient.Create(useragent);
  try
    // Construct the full URL
    address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])]);

    // Add default required headers
    client.AddHeader('User-Agent', useragent);

    // Add optional custom header
    if header <> '' then
    begin
      headers := header.Split(['=']);
      if Length(headers) = 2 then
        client.AddHeader(headers[0], headers[1]);
    end;

    // Handle JSON data or query params
    if jsondata <> '' then
    begin
      client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      client.AddHeader('Accept', 'application/json');
      client.SetRequestBody(jsondata);
    end
    else
    if hasParams then
    begin
      address := address + '?';
      for sx in params do
        address := address + '&' + sx;
    end;

    // Perform the request (GET or POST)
    try
      if post then
        ResStr := client.Post(address)
      else
        ResStr := client.Get(address, params); // TWinHTTPClient supports passing params to Get
    except
      on E: Exception do
      begin
        Result := E.Message;
        Exit;
      end;
    end;

    Result := ResStr;
  finally
    client.Free;
  end;
end;

{$ELSE}
{$IFNDEF DARWIN}
function TrndiNative.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''): string;
var
  client:  TFPHttpClient;
  res:     TStringStream;
  sx, address: string;
  headers: array of string;
begin
  client := TFPHttpClient.Create(nil);
  try
    // Set user-agent
    client.AddHeader('User-Agent', useragent);
    address := Format('%s/%s', [baseurl, endpoint]);

    // Add optional custom header
    if header <> '' then
    begin
      headers := header.Split(['=']);
      if Length(headers) = 2 then
        client.AddHeader(headers[0], headers[1]);
    end;

    // If we have JSON data
    if jsondata <> '' then
    begin
      client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      client.AddHeader('Accept', 'application/json');
      client.RequestBody := TRawByteStringStream.Create(jsondata);
    end
    else
    if Length(params) > 0 then
    begin
      // Build URL with query parameters
      address := address + '?';
      for sx in params do
        address := address + '&' + sx;
    end;

    // Prepare a response stream
    res := TStringStream.Create('');
    try
      // Send GET or POST
      if post then
        client.Post(address, res)
      else
        client.Get(address, res);

      // Return the server response as a string
      Result := res.DataString;
    except
      on E: EHttpClient do
        Result := E.Message;
    end;
  finally
    // Cleanup
    if Assigned(client.RequestBody) then
      client.RequestBody.Free;
    client.Free;
    res.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.GetSetting
  ----------------------
  Platform-specific string retrieval. Returns the default if the key isn’t found.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
function TrndiNative.GetSetting(const key: string; def: string = ''): string;
var
  reg: TRegistry;
begin
  Result := def;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('\SOFTWARE\Trndi\') then
      Result := reg.ReadString(key);
  finally
    reg.Free;
  end;
end;

{$ELSEIF DEFINED(X_PC)}
function TrndiNative.GetSetting(const key: string; def: string = ''): string;
begin
  if not Assigned(inistore) then
    inistore := TINIFile.Create(GetAppConfigFile(false));
  Result := inistore.ReadString('trndi', key, def);
end;

{$ELSEIF DEFINED(X_MAC)}
function TrndiNative.GetSetting(const key: string; def: string = ''): string;
begin
  Result := GetPrefString(key); // macOS-based method
  if Result = '' then
    Result := def;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.GetIntSetting
  -------------------------
  Returns an integer from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TrndiNative.GetIntSetting(const key: string; def: integer = -1): integer;
var
  r: string;
begin
  r := GetSetting(key, 'fail');
  if not TryStrToInt(r, Result) then
    Result := def;
end;

{------------------------------------------------------------------------------
  TrndiNative.SetSetting
  ----------------------
  Stores a string value to platform-specific storage.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
procedure TrndiNative.SetSetting(const key: string; const val: string);
begin
  SetPrefString(key, val); // macOS-based method
end;

{$ELSEIF DEFINED(X_WIN)}
procedure TrndiNative.SetSetting(const key: string; const val: string);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', true) then
      reg.WriteString(key, val)
    else
      ShowMessage('Error saving to registry!');
  finally
    reg.Free;
  end;
end;

{$ELSEIF DEFINED(X_PC)}
procedure TrndiNative.SetSetting(const key: string; const val: string);
begin
  if not Assigned(inistore) then
    inistore := TINIFile.Create(GetAppConfigFile(false));
  inistore.WriteString('trndi', key, val);
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.isDarkMode
  ----------------------
  Determines if the user’s system is in "dark mode," per platform.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
function TrndiNative.isDarkMode: boolean;
begin
  // Typically, AppleInterfaceStyle = 'Dark' if dark mode is active
  Result := Pos('DARK', UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
end;

{$ELSEIF DEFINED(X_WIN)}
function TrndiNative.isDarkMode: boolean;
const
  regtheme = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  reglight = 'AppsUseLightTheme';
var
  reg: TRegistry;
begin
  Result := false;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(regtheme) and reg.OpenKey(regtheme, false) then
    try
      if reg.ValueExists(reglight) then
        // If AppsUseLightTheme = 0 => dark mode
        Result := (reg.ReadInteger(reglight) = 0);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

{$ELSE}
function TrndiNative.isDarkMode: boolean;
  // A simplistic Linux approach:
  // Compare luminance of clWindow and clWindowText to guess if it's "dark".
function Brightness(C: TColor): double;
  begin
    // Simple formula for perceived luminance
    Result := (Red(C) * 0.3) + (Green(C) * 0.59) + (Blue(C) * 0.11);
  end;
begin
  // If the background (clWindow) is darker than the text (clWindowText), assume dark mode
  Result := (Brightness(ColorToRGB(clWindow)) < Brightness(ColorToRGB(clWindowText)));
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.getURL (class function)
  -----------------------------------
  A static method for a quick GET request. Returns True if successful, False on error.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
class function TrndiNative.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  send, response: TStringStream;
  headers: TStringList;
  httpClient: TNSHTTPSendAndReceive;
begin
  res := '';
  send := TStringStream.Create('');
  response := TStringStream.Create('');
  headers := TStringList.Create;
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
        Result := true;
      end
      else
      begin
        res := Trim(httpClient.LastErrMsg);
        Result := false;
      end;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
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
class function TrndiNative.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
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

{$ELSE}
// Linux/PC or other
class function TrndiNative.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
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
    responseStream.Free;
  end;
end;
{$ENDIF}

end.
