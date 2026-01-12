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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)
{**
  @unit RazerChromaWindows
  @brief Windows driver that wraps the Razer Chroma SDK DLL.
  @details
    Dynamically loads `RzChromaSDK*.dll`, resolves `Create*Effect`, `SetEffect`, and
    `DeleteEffect`, and funnels every effect call through the shared `TRazerChromaBase` API.
}
unit RazerChromaWindows;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Windows, DynLibs, RazerChroma;

type
  TCreateEffectFunc = function(Effect: Integer; Param: Pointer; EffectId: Pointer): LongInt; stdcall;

  {** Windows-specific implementation that resolves SDK exports and executes effects. }
  TRazerChromaWindows = class(TRazerChromaBase)
  private
    FLibHandle: TLibHandle;

    // SDK Functions
    FInit: function: LongInt; stdcall;
    FUnInit: function: LongInt; stdcall;
    FCreateKeyboardEffect: function(Effect: Integer; Param: Pointer; EffectId: Pointer): LongInt; stdcall;
    FCreateMouseEffect: function(Effect: Integer; Param: Pointer; EffectId: Pointer): LongInt; stdcall;
    FCreateMousepadEffect: function(Effect: Integer; Param: Pointer; EffectId: Pointer): LongInt; stdcall;
    FCreateHeadsetEffect: function(Effect: Integer; Param: Pointer; EffectId: Pointer): LongInt; stdcall;
    FCreateKeypadEffect: function(Effect: Integer; Param: Pointer; EffectId: Pointer): LongInt; stdcall;
    FCreateChromaLinkEffect: function(Effect: Integer; Param: Pointer; EffectId: Pointer): LongInt; stdcall;
    FSetEffect: function(EffectId: Pointer): LongInt; stdcall;
    FDeleteEffect: function(EffectId: Pointer): LongInt; stdcall;

    function LoadSDK: Boolean;
    procedure UnloadSDK;
    function ColorToBGR(const AColor: TRGBColor): LongWord;
    function ExecuteEffect(CreateFunc: TCreateEffectFunc; const DeviceName: string;
      Effect: Integer; Param: Pointer): Boolean;
    function ApplyKeyboardEffect(Effect: Integer; Param: Pointer): Boolean;
    function ApplyMouseEffect(Effect: Integer; Param: Pointer): Boolean;
    function ApplyMousepadEffect(Effect: Integer; Param: Pointer): Boolean;
    function ApplyHeadsetEffect(Effect: Integer; Param: Pointer): Boolean;
    function ApplyKeypadEffect(Effect: Integer; Param: Pointer): Boolean;
    function ApplyChromaLinkEffect(Effect: Integer; Param: Pointer): Boolean;
    function ApplyToAllDeviceTypes(Effect: Integer; Param: Pointer): Boolean;
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

  // Keyboard effect ids (ChromaSDK::Keyboard::EFFECT_TYPE)
  KEYBOARD_EFFECT_NONE             = 0;
  KEYBOARD_EFFECT_BREATHING        = 1;
  KEYBOARD_EFFECT_CUSTOM           = 2;
  KEYBOARD_EFFECT_REACTIVE         = 3;
  KEYBOARD_EFFECT_STATIC           = 4;
  KEYBOARD_EFFECT_SPECTRUM         = 5;
  KEYBOARD_EFFECT_WAVE             = 6;

  // Mouse effect ids (ChromaSDK::Mouse::EFFECT_TYPE)
  MOUSE_EFFECT_NONE                = 0;
  MOUSE_EFFECT_BLINKING            = 1;
  MOUSE_EFFECT_BREATHING           = 2;
  MOUSE_EFFECT_CUSTOM              = 3;
  MOUSE_EFFECT_REACTIVE            = 4;
  MOUSE_EFFECT_SPECTRUM            = 5;
  MOUSE_EFFECT_STATIC              = 6;
  MOUSE_EFFECT_WAVE                = 7;

  // Mousepad effect ids (ChromaSDK::Mousepad::EFFECT_TYPE)
  MOUSEPAD_EFFECT_NONE             = 0;
  MOUSEPAD_EFFECT_BREATHING        = 1;
  MOUSEPAD_EFFECT_CUSTOM           = 2;
  MOUSEPAD_EFFECT_SPECTRUM         = 3;
  MOUSEPAD_EFFECT_STATIC           = 4;
  MOUSEPAD_EFFECT_WAVE             = 5;

  // Headset effect ids (ChromaSDK::Headset::EFFECT_TYPE)
  HEADSET_EFFECT_NONE              = 0;
  HEADSET_EFFECT_STATIC            = 1;
  HEADSET_EFFECT_BREATHING         = 2;
  HEADSET_EFFECT_SPECTRUM          = 3;

  // Keypad effect ids (ChromaSDK::Keypad::EFFECT_TYPE)
  KEYPAD_EFFECT_NONE               = 0;
  KEYPAD_EFFECT_BREATHING          = 1;
  KEYPAD_EFFECT_CUSTOM             = 2;
  KEYPAD_EFFECT_REACTIVE           = 3;
  KEYPAD_EFFECT_SPECTRUM           = 4;
  KEYPAD_EFFECT_STATIC             = 5;
  KEYPAD_EFFECT_WAVE               = 6;

  // Chroma Link effect ids (ChromaSDK::ChromaLink::EFFECT_TYPE)
  CHROMALINK_EFFECT_NONE           = 0;
  CHROMALINK_EFFECT_CUSTOM         = 1;
  CHROMALINK_EFFECT_STATIC         = 2;

  // Keyboard & keypad breathing types (per RzChromaSDKTypes.h)
  KEYBOARD_BREATHING_TWO_COLORS    = 1;
  KEYBOARD_BREATHING_RANDOM        = 2;
  KEYPAD_BREATHING_TWO_COLORS      = 1;
  KEYPAD_BREATHING_RANDOM          = 2;

  // Mouse breathing types
  MOUSE_BREATHING_ONE_COLOR        = 1;
  MOUSE_BREATHING_TWO_COLORS       = 2;
  MOUSE_BREATHING_RANDOM           = 3;

  // Mousepad breathing types
  MOUSEPAD_BREATHING_TWO_COLORS    = 1;
  MOUSEPAD_BREATHING_RANDOM        = 2;

  // Mouse LED helpers
  RZLED_ALL                        = $FFFF; // Applies effect to every LED

type
  // Keyboard & keypad static effect
  TStaticEffect = packed record
    Color: LongWord; // BGR format
  end;

  // Keyboard/keypad breathing effect
  TKeyboardBreathingEffect = packed record
    BreathingType: LongWord;
    Color1: LongWord;
    Color2: LongWord;
  end;

  // Keyboard/keypad wave effect
  TKeyboardWaveEffect = packed record
    Direction: LongWord; // 1 = left to right, 2 = right to left
  end;

  // Keyboard/keypad reactive effect
  TKeyboardReactiveEffect = packed record
    Duration: LongWord; // Ord(TRazerEffectSpeed)
    Color: LongWord;
  end;

  // Mouse static effect
  TMouseStaticEffect = packed record
    LEDId: LongWord;
    Color: LongWord;
  end;

  // Mouse breathing effect
  TMouseBreathingEffect = packed record
    BreathingType: LongWord;
    Color1: LongWord;
    Color2: LongWord;
    LEDId: LongWord;
  end;

  // Mousepad static effect
  TMousepadStaticEffect = packed record
    Color: LongWord;
  end;

  // Mousepad breathing effect
  TMousepadBreathingEffect = packed record
    BreathingType: LongWord;
    Color1: LongWord;
    Color2: LongWord;
  end;

  // Headset static/breathing effect
  THeadsetColorEffect = packed record
    Color: LongWord;
  end;

  // Chroma Link static effect
  TChromaLinkStaticEffect = packed record
    Color: LongWord;
  end;

{ TRazerChromaWindows }

constructor TRazerChromaWindows.Create;
begin
  inherited Create;
  FLibHandle := NilHandle;
end;

function TRazerChromaWindows.ColorToBGR(const AColor: TRGBColor): LongWord;
begin
  // Chroma SDK uses BGR format (0x00BBGGRR)
  Result := AColor.B shl 16 or AColor.G shl 8 or AColor.R;
end;

function TRazerChromaWindows.ExecuteEffect(CreateFunc: TCreateEffectFunc;
  const DeviceName: string; Effect: Integer; Param: Pointer): Boolean;
var
  EffectId: TGUID;
  Res: LongInt;
begin
  Result := False;

  if not Assigned(CreateFunc) then
  begin
    FLastError := DeviceName + ' effect function not available';
    Exit;
  end;

  if not Assigned(FSetEffect) then
  begin
    FLastError := 'Chroma SDK SetEffect function missing';
    Exit;
  end;

  Res := CreateFunc(Effect, Param, @EffectId);
  if Res = 0 then
  begin
    Res := FSetEffect(@EffectId);
    if Assigned(FDeleteEffect) then
      FDeleteEffect(@EffectId);
  end;

  Result := Res = 0;
  if not Result then
    FLastError := Format('%s effect failed: 0x%x', [DeviceName, Res]);
end;

function TRazerChromaWindows.LoadSDK: Boolean;
var
  DllPath: string;
begin
  Result := False;

  // Try loading from system paths
  FLibHandle := SafeLoadLibrary(CHROMA_SDK_DLL);

  // If not found, try Razer installation paths
  if FLibHandle = NilHandle then
  begin
    {$IFDEF WIN64}
    DllPath := 'C:\Program Files\Razer Chroma SDK\bin\RzChromaSDK64.dll';
    {$ELSE}
    DllPath := 'C:\Program Files (x86)\Razer Chroma SDK\bin\RzChromaSDK.dll';
    {$ENDIF}
    if FileExists(DllPath) then
      FLibHandle := SafeLoadLibrary(DllPath);
  end;

  if FLibHandle = NilHandle then
  begin
    FLastError := 'Failed to load Chroma SDK DLL. Make sure Razer Synapse 3 is installed.';
    Exit;
  end;

  // Get function pointers
  Pointer(FInit) := GetProcAddress(FLibHandle, 'Init');
  Pointer(FUnInit) := GetProcAddress(FLibHandle, 'UnInit');
  Pointer(FCreateKeyboardEffect) := GetProcAddress(FLibHandle, 'CreateKeyboardEffect');
  Pointer(FCreateMouseEffect) := GetProcAddress(FLibHandle, 'CreateMouseEffect');
  Pointer(FCreateMousepadEffect) := GetProcAddress(FLibHandle, 'CreateMousepadEffect');
  Pointer(FCreateHeadsetEffect) := GetProcAddress(FLibHandle, 'CreateHeadsetEffect');
  Pointer(FCreateKeypadEffect) := GetProcAddress(FLibHandle, 'CreateKeypadEffect');
  Pointer(FCreateChromaLinkEffect) := GetProcAddress(FLibHandle, 'CreateChromaLinkEffect');
  Pointer(FSetEffect) := GetProcAddress(FLibHandle, 'SetEffect');
  Pointer(FDeleteEffect) := GetProcAddress(FLibHandle, 'DeleteEffect');

  if not Assigned(FInit) or not Assigned(FUnInit) then
  begin
    FLastError := 'Failed to get Chroma SDK function pointers';
    FreeLibrary(FLibHandle);
    FLibHandle := NilHandle;
    Exit;
  end;

  Result := True;
end;

procedure TRazerChromaWindows.UnloadSDK;
begin
  if FLibHandle <> NilHandle then
  begin
    if Assigned(FUnInit) then
      FUnInit();
    FreeLibrary(FLibHandle);
    FLibHandle := NilHandle;
  end;
end;

function TRazerChromaWindows.ApplyKeyboardEffect(Effect: Integer; Param: Pointer): Boolean;
begin
  Result := ExecuteEffect(FCreateKeyboardEffect, 'Keyboard', Effect, Param);
end;

function TRazerChromaWindows.ApplyMouseEffect(Effect: Integer; Param: Pointer): Boolean;
begin
  Result := ExecuteEffect(FCreateMouseEffect, 'Mouse', Effect, Param);
end;

function TRazerChromaWindows.ApplyMousepadEffect(Effect: Integer; Param: Pointer): Boolean;
begin
  Result := ExecuteEffect(FCreateMousepadEffect, 'Mousepad', Effect, Param);
end;

function TRazerChromaWindows.ApplyHeadsetEffect(Effect: Integer; Param: Pointer): Boolean;
begin
  Result := ExecuteEffect(FCreateHeadsetEffect, 'Headset', Effect, Param);
end;

function TRazerChromaWindows.ApplyKeypadEffect(Effect: Integer; Param: Pointer): Boolean;
begin
  Result := ExecuteEffect(FCreateKeypadEffect, 'Keypad', Effect, Param);
end;

function TRazerChromaWindows.ApplyChromaLinkEffect(Effect: Integer; Param: Pointer): Boolean;
begin
  Result := ExecuteEffect(FCreateChromaLinkEffect, 'Chroma Link', Effect, Param);
end;

function TRazerChromaWindows.ApplyToAllDeviceTypes(Effect: Integer; Param: Pointer): Boolean;
begin
  // Apply to all device types, return true if any succeeds
  Result := ApplyKeyboardEffect(Effect, Param) or
            ApplyMouseEffect(Effect, Param) or
            ApplyMousepadEffect(Effect, Param) or
            ApplyHeadsetEffect(Effect, Param) or
            ApplyKeypadEffect(Effect, Param) or
            ApplyChromaLinkEffect(Effect, Param);
end;

function TRazerChromaWindows.DoInitialize: Boolean;
var
  Res: LongInt;
begin
  Result := False;

  if not LoadSDK then
    Exit;

  Res := FInit();
  if Res <> 0 then
  begin
    FLastError := Format('Chroma SDK Init failed: 0x%x', [Res]);
    UnloadSDK;
    Exit;
  end;

  // IMPORTANT: SDK needs time to establish connection with Synapse
  Sleep(1000);

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
  // Chroma SDK doesn't enumerate individual devices
  // Create virtual entries for each device type

  if Assigned(FCreateKeyboardEffect) then
  begin
    Device := TRazerDevice.Create;
    Device.Serial := 'KEYBOARD';
    Device.Name := 'Razer Keyboard';
    Device.DeviceType := rdtKeyboard;
    FDevices.Add(Device);
  end;

  if Assigned(FCreateMouseEffect) then
  begin
    Device := TRazerDevice.Create;
    Device.Serial := 'MOUSE';
    Device.Name := 'Razer Mouse';
    Device.DeviceType := rdtMouse;
    FDevices.Add(Device);
  end;

  if Assigned(FCreateMousepadEffect) then
  begin
    Device := TRazerDevice.Create;
    Device.Serial := 'MOUSEPAD';
    Device.Name := 'Razer Mousepad';
    Device.DeviceType := rdtMousepad;
    FDevices.Add(Device);
  end;

  if Assigned(FCreateHeadsetEffect) then
  begin
    Device := TRazerDevice.Create;
    Device.Serial := 'HEADSET';
    Device.Name := 'Razer Headset';
    Device.DeviceType := rdtHeadset;
    FDevices.Add(Device);
  end;

  if Assigned(FCreateKeypadEffect) then
  begin
    Device := TRazerDevice.Create;
    Device.Serial := 'KEYPAD';
    Device.Name := 'Razer Keypad';
    Device.DeviceType := rdtKeypad;
    FDevices.Add(Device);
  end;

  if Assigned(FCreateChromaLinkEffect) then
  begin
    Device := TRazerDevice.Create;
    Device.Serial := 'CHROMALINK';
    Device.Name := 'Razer Chroma Link';
    Device.DeviceType := rdtChromaLink;
    FDevices.Add(Device);
  end;
end;

function TRazerChromaWindows.DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
var
  StaticEffect: TStaticEffect;
  MouseEffect: TMouseStaticEffect;
  MousepadEffect: TMousepadStaticEffect;
  HeadsetEffect: THeadsetColorEffect;
  ChromaLinkEffect: TChromaLinkStaticEffect;
begin
  Result := False;

  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
    begin
      StaticEffect.Color := ColorToBGR(AColor);
      Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_STATIC, @StaticEffect);
    end;

    rdtMouse:
    begin
      MouseEffect.LEDId := RZLED_ALL;
      MouseEffect.Color := ColorToBGR(AColor);
      Result := ApplyMouseEffect(MOUSE_EFFECT_STATIC, @MouseEffect);
    end;

    rdtMousepad:
    begin
      MousepadEffect.Color := ColorToBGR(AColor);
      Result := ApplyMousepadEffect(MOUSEPAD_EFFECT_STATIC, @MousepadEffect);
    end;

    rdtHeadset:
    begin
      HeadsetEffect.Color := ColorToBGR(AColor);
      Result := ApplyHeadsetEffect(HEADSET_EFFECT_STATIC, @HeadsetEffect);
    end;

    rdtKeypad:
    begin
      StaticEffect.Color := ColorToBGR(AColor);
      Result := ApplyKeypadEffect(KEYPAD_EFFECT_STATIC, @StaticEffect);
    end;

    rdtChromaLink:
    begin
      ChromaLinkEffect.Color := ColorToBGR(AColor);
      Result := ApplyChromaLinkEffect(CHROMALINK_EFFECT_STATIC, @ChromaLinkEffect);
    end;
  else
    // Try keyboard as default
    StaticEffect.Color := ColorToBGR(AColor);
    Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_STATIC, @StaticEffect);
  end;
end;

function TRazerChromaWindows.DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
var
  KbEffect: TKeyboardBreathingEffect;
  MouseEffect: TMouseBreathingEffect;
  MousepadEffect: TMousepadBreathingEffect;
  HeadsetEffect: THeadsetColorEffect;
begin
  Result := False;

  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
    begin
      // Keyboard SDK only exposes two-color/random modes, so breathe to black manually
      KbEffect.BreathingType := KEYBOARD_BREATHING_TWO_COLORS;
      KbEffect.Color1 := ColorToBGR(AColor);
      KbEffect.Color2 := 0;
      Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_BREATHING, @KbEffect);
    end;

    rdtKeypad:
    begin
      KbEffect.BreathingType := KEYPAD_BREATHING_TWO_COLORS;
      KbEffect.Color1 := ColorToBGR(AColor);
      KbEffect.Color2 := 0;
      Result := ApplyKeypadEffect(KEYPAD_EFFECT_BREATHING, @KbEffect);
    end;

    rdtMouse:
    begin
      MouseEffect.BreathingType := MOUSE_BREATHING_ONE_COLOR;
      MouseEffect.Color1 := ColorToBGR(AColor);
      MouseEffect.Color2 := 0;
      MouseEffect.LEDId := RZLED_ALL;
      Result := ApplyMouseEffect(MOUSE_EFFECT_BREATHING, @MouseEffect);
    end;

    rdtMousepad:
    begin
      MousepadEffect.BreathingType := MOUSEPAD_BREATHING_TWO_COLORS;
      MousepadEffect.Color1 := ColorToBGR(AColor);
      MousepadEffect.Color2 := MousepadEffect.Color1;
      Result := ApplyMousepadEffect(MOUSEPAD_EFFECT_BREATHING, @MousepadEffect);
    end;

    rdtHeadset:
    begin
      HeadsetEffect.Color := ColorToBGR(AColor);
      Result := ApplyHeadsetEffect(HEADSET_EFFECT_BREATHING, @HeadsetEffect);
    end;
  else
    FLastError := 'Breathing effect not supported on this device';
  end;
end;

function TRazerChromaWindows.DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean;
var
  KbEffect: TKeyboardBreathingEffect;
  MouseEffect: TMouseBreathingEffect;
  MousepadEffect: TMousepadBreathingEffect;
  HeadsetEffect: THeadsetColorEffect;
begin
  Result := False;

  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
    begin
      KbEffect.BreathingType := KEYBOARD_BREATHING_TWO_COLORS;
      KbEffect.Color1 := ColorToBGR(AColor1);
      KbEffect.Color2 := ColorToBGR(AColor2);
      Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_BREATHING, @KbEffect);
    end;

    rdtKeypad:
    begin
      KbEffect.BreathingType := KEYPAD_BREATHING_TWO_COLORS;
      KbEffect.Color1 := ColorToBGR(AColor1);
      KbEffect.Color2 := ColorToBGR(AColor2);
      Result := ApplyKeypadEffect(KEYPAD_EFFECT_BREATHING, @KbEffect);
    end;

    rdtMouse:
    begin
      MouseEffect.BreathingType := MOUSE_BREATHING_TWO_COLORS;
      MouseEffect.Color1 := ColorToBGR(AColor1);
      MouseEffect.Color2 := ColorToBGR(AColor2);
      MouseEffect.LEDId := RZLED_ALL;
      Result := ApplyMouseEffect(MOUSE_EFFECT_BREATHING, @MouseEffect);
    end;

    rdtMousepad:
    begin
      MousepadEffect.BreathingType := MOUSEPAD_BREATHING_TWO_COLORS;
      MousepadEffect.Color1 := ColorToBGR(AColor1);
      MousepadEffect.Color2 := ColorToBGR(AColor2);
      Result := ApplyMousepadEffect(MOUSEPAD_EFFECT_BREATHING, @MousepadEffect);
    end;

    rdtHeadset:
    begin
      // Headsets only accept a single breathing color – use first color
      HeadsetEffect.Color := ColorToBGR(AColor1);
      Result := ApplyHeadsetEffect(HEADSET_EFFECT_BREATHING, @HeadsetEffect);
    end;
  else
    FLastError := 'Breathing dual-color effect not supported on this device';
  end;
end;

function TRazerChromaWindows.DoSetBreathRandom(const ADevice: TRazerDevice): Boolean;
var
  KbEffect: TKeyboardBreathingEffect;
  MouseEffect: TMouseBreathingEffect;
  MousepadEffect: TMousepadBreathingEffect;
begin
  Result := False;

  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
    begin
      KbEffect.BreathingType := KEYBOARD_BREATHING_RANDOM;
      KbEffect.Color1 := 0;
      KbEffect.Color2 := 0;
      Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_BREATHING, @KbEffect);
    end;

    rdtKeypad:
    begin
      KbEffect.BreathingType := KEYPAD_BREATHING_RANDOM;
      KbEffect.Color1 := 0;
      KbEffect.Color2 := 0;
      Result := ApplyKeypadEffect(KEYPAD_EFFECT_BREATHING, @KbEffect);
    end;

    rdtMouse:
    begin
      MouseEffect.BreathingType := MOUSE_BREATHING_RANDOM;
      MouseEffect.Color1 := 0;
      MouseEffect.Color2 := 0;
      MouseEffect.LEDId := RZLED_ALL;
      Result := ApplyMouseEffect(MOUSE_EFFECT_BREATHING, @MouseEffect);
    end;

    rdtMousepad:
    begin
      MousepadEffect.BreathingType := MOUSEPAD_BREATHING_RANDOM;
      MousepadEffect.Color1 := 0;
      MousepadEffect.Color2 := 0;
      Result := ApplyMousepadEffect(MOUSEPAD_EFFECT_BREATHING, @MousepadEffect);
    end;

    rdtHeadset:
      FLastError := 'Random breathing not supported on headsets';
  else
    FLastError := 'Breathing random effect not supported on this device';
  end;
end;

function TRazerChromaWindows.DoSetSpectrum(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;

  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
      Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_SPECTRUM, nil);
    rdtMouse:
      Result := ApplyMouseEffect(MOUSE_EFFECT_SPECTRUM, nil);
    rdtMousepad:
      Result := ApplyMousepadEffect(MOUSEPAD_EFFECT_SPECTRUM, nil);
    rdtHeadset:
      Result := ApplyHeadsetEffect(HEADSET_EFFECT_SPECTRUM, nil);
    rdtKeypad:
      Result := ApplyKeypadEffect(KEYPAD_EFFECT_SPECTRUM, nil);
  end;
end;

function TRazerChromaWindows.DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean;
var
  Effect: TKeyboardReactiveEffect;
begin
  Result := False;

  if ADevice.DeviceType in [rdtKeyboard, rdtLaptop] then
  begin
    Effect.Duration := Ord(ASpeed);
    Effect.Color := ColorToBGR(AColor);
    Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_REACTIVE, @Effect);
  end
  else if ADevice.DeviceType = rdtKeypad then
  begin
    Effect.Duration := Ord(ASpeed);
    Effect.Color := ColorToBGR(AColor);
    Result := ApplyKeypadEffect(KEYPAD_EFFECT_REACTIVE, @Effect);
  end
  else
    FLastError := 'Reactive effect only supported on keyboards/keypads';
end;

function TRazerChromaWindows.DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean;
var
  Effect: TKeyboardWaveEffect;
begin
  Result := False;

  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
    begin
      Effect.Direction := ADirection;
      Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_WAVE, @Effect);
    end;
    rdtKeypad:
    begin
      Effect.Direction := ADirection;
      Result := ApplyKeypadEffect(KEYPAD_EFFECT_WAVE, @Effect);
    end;
    rdtMouse:
    begin
      Effect.Direction := ADirection;
      Result := ApplyMouseEffect(MOUSE_EFFECT_WAVE, @Effect);
    end;
    rdtMousepad:
    begin
      Effect.Direction := ADirection;
      Result := ApplyMousepadEffect(MOUSEPAD_EFFECT_WAVE, @Effect);
    end;
  else
    FLastError := 'Wave effect not supported on this device';
  end;
end;

function TRazerChromaWindows.DoSetNone(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;

  case ADevice.DeviceType of
    rdtKeyboard, rdtLaptop:
      Result := ApplyKeyboardEffect(KEYBOARD_EFFECT_NONE, nil);
    rdtMouse:
      Result := ApplyMouseEffect(MOUSE_EFFECT_NONE, nil);
    rdtMousepad:
      Result := ApplyMousepadEffect(MOUSEPAD_EFFECT_NONE, nil);
    rdtHeadset:
      Result := ApplyHeadsetEffect(HEADSET_EFFECT_NONE, nil);
    rdtKeypad:
      Result := ApplyKeypadEffect(KEYPAD_EFFECT_NONE, nil);
    rdtChromaLink:
      Result := ApplyChromaLinkEffect(CHROMALINK_EFFECT_NONE, nil);
  end;
end;

function TRazerChromaWindows.DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean;
begin
  // Brightness not directly available in basic SDK
  FLastError := 'Brightness control requires Synapse';
  Result := False;
end;

function TRazerChromaWindows.DoGetBrightness(const ADevice: TRazerDevice): Byte;
begin
  Result := 255; // Default full brightness
end;

end.
