unit RazerChromaWindows;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Windows, DynLibs, RazerChroma;

const
  // Chroma SDK effect types
  CYCLER_CHROMA_NONE = 0;
  CYCLER_CHROMA_STATIC = 1;
  CYCLER_CHROMA_BREATHING = 2;
  CYCLER_CHROMA_SPECTRUM_CYCLING = 3;
  CYCLER_CHROMA_WAVE = 4;
  CYCLER_CHROMA_REACTIVE = 5;

type
  TGUID = record
    Data1: LongWord;
    Data2: Word;
    Data3: Word;
    Data4: array[0..7] of Byte;
  end;
  
  // Chroma SDK function types
  TChromaInit = function: LongInt; cdecl;
  TChromaUnInit = function: LongInt; cdecl;
  TChromaCreateEffect = function(DeviceId: TGUID; Effect: Integer; 
    Param: Pointer; EffectId: Pointer): LongInt; cdecl;
  TChromaCreateKeyboardEffect = function(Effect: Integer; 
    Param: Pointer; EffectId: Pointer): LongInt; cdecl;
  TChromaCreateMouseEffect = function(Effect: Integer;
    Param: Pointer; EffectId: Pointer): LongInt; cdecl;
  TChromaCreateMousepadEffect = function(Effect: Integer;
    Param: Pointer; EffectId: Pointer): LongInt; cdecl;
  TChromaCreateHeadsetEffect = function(Effect: Integer;
    Param: Pointer; EffectId: Pointer): LongInt; cdecl;

  { Static effect parameter }
  TChromaStaticEffect = packed record
    Color: LongWord; // BGR format
  end;

  { Breathing effect parameter }
  TChromaBreathingEffect = packed record
    EffectType: Integer; // 1=one color, 2=two colors, 3=random
    Color1: LongWord;
    Color2: LongWord;
  end;

  { TRazerChromaWindows }
  TRazerChromaWindows = class(TRazerChromaBase)
  private
    FLibHandle: TLibHandle;
    FChromaInit: TChromaInit;
    FChromaUnInit: TChromaUnInit;
    FCreateKeyboardEffect: TChromaCreateKeyboardEffect;
    FCreateMouseEffect: TChromaCreateMouseEffect;
    FCreateMousepadEffect: TChromaCreateMousepadEffect;
    FCreateHeadsetEffect: TChromaCreateHeadsetEffect;
    
    function LoadSDK: Boolean;
    procedure UnloadSDK;
    function RGBToBGR(const AColor: TRGBColor): LongWord;
    function ApplyEffectToDevice(const ADevice: TRazerDevice; 
      Effect: Integer; Param: Pointer): Boolean;
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

const
  {$IFDEF WIN64}
  CHROMA_SDK_DLL = 'RzChromaSDK64.dll';
  {$ELSE}
  CHROMA_SDK_DLL = 'RzChromaSDK.dll';
  {$ENDIF}

{ TRazerChromaWindows }

constructor TRazerChromaWindows.Create;
begin
  inherited Create;
  FLibHandle := NilHandle;
end;

function TRazerChromaWindows.LoadSDK: Boolean;
begin
  Result := False;
  
  FLibHandle := LoadLibrary(CHROMA_SDK_DLL);
  if FLibHandle = NilHandle then
  begin
    FLastError := 'Failed to load ' + CHROMA_SDK_DLL + '. Ensure Razer Synapse is installed.';
    Exit;
  end;
  
  Pointer(FChromaInit) := GetProcAddress(FLibHandle, 'Init');
  Pointer(FChromaUnInit) := GetProcAddress(FLibHandle, 'UnInit');
  Pointer(FCreateKeyboardEffect) := GetProcAddress(FLibHandle, 'CreateKeyboardEffect');
  Pointer(FCreateMouseEffect) := GetProcAddress(FLibHandle, 'CreateMouseEffect');
  Pointer(FCreateMousepadEffect) := GetProcAddress(FLibHandle, 'CreateMousepadEffect');
  Pointer(FCreateHeadsetEffect) := GetProcAddress(FLibHandle, 'CreateHeadsetEffect');
  
  Result := Assigned(FChromaInit) and Assigned(FChromaUnInit);
  
  if not Result then
    FLastError := 'Failed to get Chroma SDK function pointers';
end;

procedure TRazerChromaWindows.UnloadSDK;
begin
  if FLibHandle <> NilHandle then
  begin
    if Assigned(FChromaUnInit) then
      FChromaUnInit();
    FreeLibrary(FLibHandle);
    FLibHandle := NilHandle;
  end;
end;

function TRazerChromaWindows.RGBToBGR(const AColor: TRGBColor): LongWord;
begin
  // Chroma SDK uses BGR format
  Result := (AColor.B shl 16) or (AColor.G shl 8) or AColor.R;
end;

function TRazerChromaWindows.ApplyEffectToDevice(const ADevice: TRazerDevice;
  Effect: Integer; Param: Pointer): Boolean;
var
  Res: LongInt;
begin
  Result := False;
  
  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
      if Assigned(FCreateKeyboardEffect) then
      begin
        Res := FCreateKeyboardEffect(Effect, Param, nil);
        Result := Res = 0;
      end;
    rdtMouse:
      if Assigned(FCreateMouseEffect) then
      begin
        Res := FCreateMouseEffect(Effect, Param, nil);
        Result := Res = 0;
      end;
    rdtMousepad:
      if Assigned(FCreateMousepadEffect) then
      begin
        Res := FCreateMousepadEffect(Effect, Param, nil);
        Result := Res = 0;
      end;
    rdtHeadset:
      if Assigned(FCreateHeadsetEffect) then
      begin
        Res := FCreateHeadsetEffect(Effect, Param, nil);
        Result := Res = 0;
      end;
  else
    // Try keyboard effect as fallback
    if Assigned(FCreateKeyboardEffect) then
    begin
      Res := FCreateKeyboardEffect(Effect, Param, nil);
      Result := Res = 0;
    end;
  end;
  
  if not Result then
    FLastError := Format('Effect failed with code: %d', [Res]);
end;

function TRazerChromaWindows.DoInitialize: Boolean;
var
  Res: LongInt;
begin
  Result := False;
  
  if not LoadSDK then
    Exit;
    
  Res := FChromaInit();
  if Res <> 0 then
  begin
    FLastError := Format('Chroma SDK Init failed with code: %d', [Res]);
    UnloadSDK;
    Exit;
  end;
  
  // Give SDK time to initialize
  Sleep(100);
  
  Result := True;
end;

procedure TRazerChromaWindows.DoFinalize;
begin
  UnloadSDK;
end;

procedure TRazerChromaWindows.DoRefreshDevices;
var
  Device: TRazerDevice;
begin
  // Windows Chroma SDK doesn't enumerate devices directly
  // We create virtual device entries for each supported type
  
  // Keyboard
  Device := TRazerDevice.Create;
  Device.Serial := 'KEYBOARD';
  Device.Name := 'Razer Keyboard';
  Device.DeviceType := rdtKeyboard;
  FDevices.Add(Device);
  
  // Mouse
  Device := TRazerDevice.Create;
  Device.Serial := 'MOUSE';
  Device.Name := 'Razer Mouse';
  Device.DeviceType := rdtMouse;
  FDevices.Add(Device);
  
  // Mousepad
  Device := TRazerDevice.Create;
  Device.Serial := 'MOUSEPAD';
  Device.Name := 'Razer Mousepad';
  Device.DeviceType := rdtMousepad;
  FDevices.Add(Device);
  
  // Headset
  Device := TRazerDevice.Create;
  Device.Serial := 'HEADSET';
  Device.Name := 'Razer Headset';
  Device.DeviceType := rdtHeadset;
  FDevices.Add(Device);
end;

function TRazerChromaWindows.DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
var
  Effect: TChromaStaticEffect;
begin
  Effect.Color := RGBToBGR(AColor);
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_STATIC, @Effect);
end;

function TRazerChromaWindows.DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
var
  Effect: TChromaBreathingEffect;
begin
  Effect.EffectType := 1; // Single color
  Effect.Color1 := RGBToBGR(AColor);
  Effect.Color2 := 0;
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_BREATHING, @Effect);
end;

function TRazerChromaWindows.DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean;
var
  Effect: TChromaBreathingEffect;
begin
  Effect.EffectType := 2; // Two colors
  Effect.Color1 := RGBToBGR(AColor1);
  Effect.Color2 := RGBToBGR(AColor2);
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_BREATHING, @Effect);
end;

function TRazerChromaWindows.DoSetBreathRandom(const ADevice: TRazerDevice): Boolean;
var
  Effect: TChromaBreathingEffect;
begin
  Effect.EffectType := 3; // Random
  Effect.Color1 := 0;
  Effect.Color2 := 0;
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_BREATHING, @Effect);
end;

function TRazerChromaWindows.DoSetSpectrum(const ADevice: TRazerDevice): Boolean;
begin
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_SPECTRUM_CYCLING, nil);
end;

function TRazerChromaWindows.DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean;
type
  TReactiveEffect = packed record
    Duration: Integer;
    Color: LongWord;
  end;
var
  Effect: TReactiveEffect;
begin
  Effect.Duration := Ord(ASpeed);
  Effect.Color := RGBToBGR(AColor);
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_REACTIVE, @Effect);
end;

function TRazerChromaWindows.DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean;
type
  TWaveEffect = packed record
    Direction: Integer;
  end;
var
  Effect: TWaveEffect;
begin
  Effect.Direction := ADirection;
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_WAVE, @Effect);
end;

function TRazerChromaWindows.DoSetNone(const ADevice: TRazerDevice): Boolean;
begin
  Result := ApplyEffectToDevice(ADevice, CYCLER_CHROMA_NONE, nil);
end;

function TRazerChromaWindows.DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean;
begin
  // Brightness control not directly available in basic Chroma SDK
  // Would need extended SDK calls
  FLastError := 'Brightness control not implemented for Windows';
  Result := False;
end;

function TRazerChromaWindows.DoGetBrightness(const ADevice: TRazerDevice): Byte;
begin
  Result := 100; // Default
end;

end.