unit trndi.native.mock;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.native.base, fphttpclient, opensslsockets, IniFiles;

type
  { TTrndiNativeMock }
  TTrndiNativeMock = class(TTrndiNativeBase)
  private
    class var FMockSettings: TStringList;
  public
    // HTTP helpers
    class function getURL(const url: string; out res: string): boolean; override;
    class function TestProxyURL(const url: string; const proxyHost: string;
      const proxyPort: string; const proxyUser: string; const proxyPass: string;
      out res: string): boolean; override;

    // Settings API (simple in-memory store for tests)
    procedure SetSetting(const keyname: string; const val: string; global: boolean = false); override;
    function GetSetting(const keyname: string; def: string = ''; global: boolean = false): string; override;
    procedure DeleteSetting(const keyname: string; global: boolean = false); override;
    procedure ReloadSettings; override;
    function ExportSettings: string; override;
    procedure ImportSettings(const iniData: string); override;

    // TTS and environment
    procedure Speak(const Text: string); override;
    class function isDarkMode: boolean; override;
    class function isNotificationSystemAvailable: boolean; override;
    class function getNotificationSystem: string; override;
    class function SpeakAvailable: boolean; override;
    class function SpeakSoftwareName: string; override;
    class function GetWindowManagerName: string; override;
    class function nobuttonsVM: boolean; override;
  end;

implementation

{ TTrndiNativeMock }

class function TTrndiNativeMock.getURL(const url: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
begin
  res := '';
  Result := False;
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.AllowRedirect := True;
    HTTP.IOTimeout := 30000; // 30s
    try
      res := HTTP.Get(url);
      Result := True;
    except
      on E: Exception do
      begin
        res := 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

class function TTrndiNativeMock.TestProxyURL(const url: string; const proxyHost: string;
  const proxyPort: string; const proxyUser: string; const proxyPass: string;
  out res: string): boolean;
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
  Result := False;
  host := Trim(proxyHost);
  portS := Trim(proxyPort);
  user := Trim(proxyUser);
  pass := proxyPass;
  NormalizeProxyHostPort(host, portS);

  if host = '' then
  begin
    res := 'Proxy host is empty.';
    Exit(False);
  end;
  if portS = '' then
    portS := '8080';

  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.AllowRedirect := True;
    HTTP.IOTimeout := 30000;
    HTTP.Proxy.Host := host;
    HTTP.Proxy.Port := StrToIntDef(portS, 8080);
    if user <> '' then
      HTTP.Proxy.Username := user;
    if pass <> '' then
      HTTP.Proxy.Password := pass;
    try
      res := HTTP.Get(url);
      Result := True;
    except
      on E: Exception do
      begin
        res := 'Error: ' + E.Message;
        Result := False;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

procedure TTrndiNativeMock.SetSetting(const keyname: string; const val: string; global: boolean = false);
var
  key: string;
begin
  key := keyname; // For tests we ignore 'global' scoping
  if FMockSettings = nil then
    FMockSettings := TStringList.Create;
  FMockSettings.Values[key] := val;
end;

function TTrndiNativeMock.GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
var
  key: string;
begin
  key := keyname;
  if (FMockSettings <> nil) and (FMockSettings.IndexOfName(key) >= 0) then
    Result := FMockSettings.Values[key]
  else
    Result := def;
end;

procedure TTrndiNativeMock.DeleteSetting(const keyname: string; global: boolean = false);
var
  idx: integer;
begin
  if FMockSettings = nil then
    Exit;
  idx := FMockSettings.IndexOfName(keyname);
  if idx >= 0 then
    FMockSettings.Delete(idx);
end;

procedure TTrndiNativeMock.ReloadSettings;
begin
  // In-memory settings - nothing to reload. Provided for API completeness.
end;

function TTrndiNativeMock.ExportSettings: string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    if FMockSettings <> nil then
    begin
      sl.Add('[trndi]');
      sl.AddStrings(FMockSettings);
    end;
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TTrndiNativeMock.ImportSettings(const iniData: string);
var
  sl: TStringList;
  mem: TMemoryStream;
  ini: TMemIniFile;
  keys: TStringList;
  i: integer;
  key, value: string;
begin
  if FMockSettings = nil then
    FMockSettings := TStringList.Create;
    
  sl := TStringList.Create;
  mem := TMemoryStream.Create;
  ini := nil;
  keys := TStringList.Create;
  try
    mem.WriteBuffer(iniData[1], Length(iniData));
    mem.Position := 0;
    sl.LoadFromStream(mem);
    
    ini := TMemIniFile.Create('');
    ini.SetStrings(sl);
    
    ini.ReadSection('trndi', keys);
    FMockSettings.Clear;
    for i := 0 to keys.Count - 1 do
    begin
      key := keys[i];
      value := ini.ReadString('trndi', key, '');
      FMockSettings.Add(key + '=' + value);
    end;
  finally
    keys.Free;
    ini.Free;
    mem.Free;
    sl.Free;
  end;
end;

procedure TTrndiNativeMock.Speak(const Text: string);
begin
  // No-op in tests
end;

class function TTrndiNativeMock.isDarkMode: boolean;
begin
  Result := False;
end;

class function TTrndiNativeMock.isNotificationSystemAvailable: boolean;
begin
  Result := False;
end;

class function TTrndiNativeMock.getNotificationSystem: string;
begin
  Result := 'none';
end;

class function TTrndiNativeMock.SpeakAvailable: boolean;
begin
  Result := False;
end;

class function TTrndiNativeMock.SpeakSoftwareName: string;
begin
  Result := '';
end;

class function TTrndiNativeMock.GetWindowManagerName: string;
begin
  Result := '';
end;

class function TTrndiNativeMock.nobuttonsVM: boolean;
begin
  Result := False;
end;

initialization
  TTrndiNativeMock.FMockSettings := TStringList.Create;
finalization
  FreeAndNil(TTrndiNativeMock.FMockSettings);

end.
