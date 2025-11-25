unit RazerChromaLinux;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, RazerChroma;

type
  { TRazerChromaLinux }
  TRazerChromaLinux = class(TRazerChromaBase)
  private
    function ExecuteDBus(const Args: array of string): string;
    function CallMethod(const ASerial, AInterface, AMethod: string;
      const AParams: array of string): string;
    function CallMethodBool(const ASerial, AInterface, AMethod: string;
      const AParams: array of string): Boolean;
    function ParseDeviceType(const ATypeStr: string): TRazerDeviceType;
    function IsDaemonRunning: Boolean;
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;
    procedure DoRefreshDevices; override;
    
    function DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean; override;
    function DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean; override;
    function DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean; override;
    function DoSetBreathRandom(const ADevice: TRazerDevice): Boolean; override;
    function DoSetSpectrum(const ADevice: TRazerDevice): Boolean; override;
    function DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean; override;
    function DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean; override;
    function DoSetNone(const ADevice: TRazerDevice): Boolean; override;
    function DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean; override;
    function DoGetBrightness(const ADevice: TRazerDevice): Byte; override;
  public
    constructor Create; override;
  end;

implementation

{ TRazerChromaLinux }

constructor TRazerChromaLinux.Create;
begin
  inherited Create;
end;

function TRazerChromaLinux.ExecuteDBus(const Args: array of string): string;
var
  AProcess: TProcess;
  Output: TStringList;
  i: Integer;
begin
  Result := '';
  AProcess := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    AProcess.Executable := 'dbus-send';
    for i := 0 to High(Args) do
      AProcess.Parameters.Add(Args[i]);
    AProcess.Options := [poWaitOnExit, poUsePipes, poStderrToOutPut];
    try
      AProcess.Execute;
      Output.LoadFromStream(AProcess.Output);
      Result := Trim(Output.Text);
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        Result := '';
      end;
    end;
  finally
    Output.Free;
    AProcess.Free;
  end;
end;

function TRazerChromaLinux.CallMethod(const ASerial, AInterface, AMethod: string;
  const AParams: array of string): string;
var
  Args: array of string;
  i, BaseLen: Integer;
begin
  BaseLen := 5;
  SetLength(Args, BaseLen + Length(AParams));
  
  Args[0] := '--session';
  Args[1] := '--dest=org.razer';
  Args[2] := '--type=method_call';
  Args[3] := Format('/org/razer/device/%s', [ASerial]);
  Args[4] := Format('%s.%s', [AInterface, AMethod]);
  
  for i := 0 to High(AParams) do
    Args[BaseLen + i] := AParams[i];
  
  Result := ExecuteDBus(Args);
end;

function TRazerChromaLinux.CallMethodBool(const ASerial, AInterface, AMethod: string;
  const AParams: array of string): Boolean;
var
  Response: string;
begin
  Response := CallMethod(ASerial, AInterface, AMethod, AParams);
  // Check for errors in response
  Result := (Pos('Error', Response) = 0) and (Pos('error', Response) = 0);
  if not Result then
    FLastError := Response;
end;

function TRazerChromaLinux.ParseDeviceType(const ATypeStr: string): TRazerDeviceType;
var
  LowerType: string;
begin
  LowerType := LowerCase(ATypeStr);
  
  if Pos('keyboard', LowerType) > 0 then
    Result := rdtKeyboard
  else if Pos('mouse', LowerType) > 0 then
  begin
    if Pos('pad', LowerType) > 0 then
      Result := rdtMousepad
    else
      Result := rdtMouse;
  end
  else if Pos('headset', LowerType) > 0 then
    Result := rdtHeadset
  else if Pos('keypad', LowerType) > 0 then
    Result := rdtKeypad
  else if Pos('laptop', LowerType) > 0 then
    Result := rdtLaptop
  else if Pos('chroma', LowerType) > 0 then
    Result := rdtChromaLink
  else
    Result := rdtUnknown;
end;

function TRazerChromaLinux.IsDaemonRunning: Boolean;
var
  Response: string;
begin
  // Try to actually communicate with the daemon via D-Bus
  Response := ExecuteDBus([
    '--session',
    '--dest=org.razer',
    '--type=method_call',
    '--print-reply',
    '/org/razer',
    'razer.devices.getDevices'
  ]);
  
  // Check if we got a valid response (not an error)
  Result := (Pos('array', LowerCase(Response)) > 0) or 
            (Pos('string', LowerCase(Response)) > 0) or
            (Response <> '') and (Pos('Error', Response) = 0);
end;

function TRazerChromaLinux.DoInitialize: Boolean;
var
  TestResponse: string;
begin
  // Test D-Bus connectivity directly
  TestResponse := ExecuteDBus([
    '--session',
    '--dest=org.razer',
    '--type=method_call',
    '--print-reply',
    '/org/razer',
    'razer.devices.getDevices'
  ]);
  
  // Debug output
  WriteLn('DEBUG: D-Bus response = ', TestResponse);
  
  if Pos('Error', TestResponse) > 0 then
  begin
    FLastError := 'Cannot communicate with OpenRazer daemon: ' + TestResponse;
    Result := False;
  end
  else if TestResponse = '' then
  begin
    FLastError := 'No response from OpenRazer daemon. Check if dbus-send is installed.';
    Result := False;
  end
  else
    Result := True;
end;

procedure TRazerChromaLinux.DoFinalize;
begin
  // Nothing specific to clean up for D-Bus
end;

procedure TRazerChromaLinux.DoRefreshDevices;
var
  Output: string;
  Lines: TStringList;
  i: Integer;
  Serial, DeviceName, DeviceType: string;
  Device: TRazerDevice;
begin
  Output := ExecuteDBus([
    '--session',
    '--dest=org.razer',
    '--type=method_call',
    '--print-reply',
    '/org/razer',
    'razer.devices.getDevices'
  ]);
  
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    for i := 0 to Lines.Count - 1 do
    begin
      if Pos('string "', Lines[i]) > 0 then
      begin
        Serial := Lines[i];
        Delete(Serial, 1, Pos('"', Serial));
        Serial := Copy(Serial, 1, Pos('"', Serial) - 1);
        
        if Serial <> '' then
        begin
          Device := TRazerDevice.Create;
          Device.Serial := Serial;
          
          // Get device name
          DeviceName := CallMethod(Serial, 'razer.device.misc', 'getDeviceName', ['--print-reply']);
          if Pos('string "', DeviceName) > 0 then
          begin
            Delete(DeviceName, 1, Pos('"', DeviceName));
            DeviceName := Copy(DeviceName, 1, Pos('"', DeviceName) - 1);
          end;
          Device.Name := DeviceName;
          
          // Get device type
          DeviceType := CallMethod(Serial, 'razer.device.misc', 'getDeviceType', ['--print-reply']);
          if Pos('string "', DeviceType) > 0 then
          begin
            Delete(DeviceType, 1, Pos('"', DeviceType));
            DeviceType := Copy(DeviceType, 1, Pos('"', DeviceType) - 1);
          end;
          Device.DeviceType := ParseDeviceType(DeviceType);
          
          FDevices.Add(Device);
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TRazerChromaLinux.DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setStatic', [
    Format('byte:%d', [AColor.R]),
    Format('byte:%d', [AColor.G]),
    Format('byte:%d', [AColor.B])
  ]);
end;

function TRazerChromaLinux.DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setBreathSingle', [
    Format('byte:%d', [AColor.R]),
    Format('byte:%d', [AColor.G]),
    Format('byte:%d', [AColor.B])
  ]);
end;

function TRazerChromaLinux.DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setBreathDual', [
    Format('byte:%d', [AColor1.R]),
    Format('byte:%d', [AColor1.G]),
    Format('byte:%d', [AColor1.B]),
    Format('byte:%d', [AColor2.R]),
    Format('byte:%d', [AColor2.G]),
    Format('byte:%d', [AColor2.B])
  ]);
end;

function TRazerChromaLinux.DoSetBreathRandom(const ADevice: TRazerDevice): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setBreathRandom', []);
end;

function TRazerChromaLinux.DoSetSpectrum(const ADevice: TRazerDevice): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setSpectrum', []);
end;

function TRazerChromaLinux.DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setReactive', [
    Format('byte:%d', [Ord(ASpeed)]),
    Format('byte:%d', [AColor.R]),
    Format('byte:%d', [AColor.G]),
    Format('byte:%d', [AColor.B])
  ]);
end;

function TRazerChromaLinux.DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setWave', [
    Format('int32:%d', [ADirection])
  ]);
end;

function TRazerChromaLinux.DoSetNone(const ADevice: TRazerDevice): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.chroma', 'setNone', []);
end;

function TRazerChromaLinux.DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean;
begin
  Result := CallMethodBool(ADevice.Serial, 'razer.device.lighting.brightness', 'setBrightness', [
    Format('double:%f', [ABrightness / 255.0])
  ]);
end;

function TRazerChromaLinux.DoGetBrightness(const ADevice: TRazerDevice): Byte;
var
  Response: string;
  Value: Double;
begin
  Response := CallMethod(ADevice.Serial, 'razer.device.lighting.brightness', 'getBrightness', ['--print-reply']);
  // Parse double from response
  if Pos('double', Response) > 0 then
  begin
    Delete(Response, 1, Pos('double', Response) + 6);
    Response := Trim(Response);
    if TryStrToFloat(Response, Value) then
      Result := Round(Value * 255)
    else
      Result := 100;
  end
  else
    Result := 100;
end;

end.