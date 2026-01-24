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
  @unit RazerChroma
  @brief Shared definitions for discovering and driving Razer Chroma devices.
  @details
    The unit introduces the cross-platform device model, the abstract effect API used by
    `TRazerChromaBase` descendants, and helpers for converting colors between `TRGBColor`
    records and SDK integers.
}
unit RazerChroma;

{$mode objfpc}{$H+}

interface

uses
SysUtils, Classes, contnrs;

type
  {** Packed RGB values used when issuing Chroma effects. }
TRGBColor = packed record
  R, G, B: byte;
end;

  {** Enumerates the Razer device categories exposed through the SDK/daemon. }
TRazerDeviceType = (
  rdtUnknown,
  rdtKeyboard,
  rdtMouse,
  rdtMousepad,
  rdtHeadset,
  rdtKeypad,
  rdtChromaLink,
  rdtLaptop
  );

  {** Represents a Razer device entry discovered during enumeration. }
TRazerDevice = class
private
  FSerial: string;
  FName: string;
  FDeviceType: TRazerDeviceType;
public
  property Serial: string read FSerial write FSerial;
  property Name: string read FName write FName;
  property DeviceType: TRazerDeviceType read FDeviceType write FDeviceType;
end;

  {** Owned list of `TRazerDevice` instances, keyed by serial number. }
TRazerDeviceList = class(TObjectList)
private
  function GetDevice(Index: integer): TRazerDevice;
public
  function Add(ADevice: TRazerDevice): integer;
    {** Returns the device matching `ASerial` or nil when the serial is unknown. }
  function FindBySerial(const ASerial: string): TRazerDevice;
  property Items[Index: integer]: TRazerDevice read GetDevice; default;
end;

  { ERazerException }
ERazerException = class(Exception);
ERazerNotInitialized = class(ERazerException);
ERazerDeviceNotFound = class(ERazerException);
ERazerEffectFailed = class(ERazerException);

  { TRazerEffectSpeed }
TRazerEffectSpeed = (resSlow = 1, resMedium = 2, resFast = 3);

  {**
    @class TRazerChromaBase
    @brief Abstract driver that normalizes initialization, device enumeration, and effect APIs.
    @details
      Each platform-specific descendant overrides `DoInitialize`/`DoSet*` when interacting with the
      native Razer SDK or OpenRazer daemon. The base class exposes strongly typed `Set*` helpers along
      with `LastError` tracking so callers can react when an effect fails.
  }
TRazerChromaBase = class abstract
protected
  FInitialized: boolean;
  FDevices: TRazerDeviceList;
  FLastError: string;
    
  procedure CheckInitialized;
  function DoInitialize: boolean; virtual; abstract;
  procedure DoFinalize; virtual; abstract;
  procedure DoRefreshDevices; virtual; abstract;
    
    // Abstract effect methods
  function DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): boolean; virtual; abstract;
  function DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): boolean; virtual; abstract;
  function DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): boolean; virtual; abstract;
  function DoSetBreathRandom(const ADevice: TRazerDevice): boolean; virtual; abstract;
  function DoSetSpectrum(const ADevice: TRazerDevice): boolean; virtual; abstract;
  function DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): boolean; virtual; abstract;
  function DoSetWave(const ADevice: TRazerDevice; ADirection: integer): boolean; virtual; abstract;
  function DoSetNone(const ADevice: TRazerDevice): boolean; virtual; abstract;
  function DoSetBrightness(const ADevice: TRazerDevice; ABrightness: byte): boolean; virtual; abstract;
  function DoGetBrightness(const ADevice: TRazerDevice): byte; virtual; abstract;
    
public
  constructor Create; virtual;
  destructor Destroy; override;
    
    // Initialization
  function Initialize: boolean;
  procedure Finalize;
  procedure RefreshDevices;
    
    // Device access
  function GetDeviceCount: integer;
  function GetDevice(Index: integer): TRazerDevice;
  function GetDeviceBySerial(const ASerial: string): TRazerDevice;
    
    // Effects - Single device
  procedure SetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor);
  procedure SetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor);
  procedure SetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor);
  procedure SetBreathRandom(const ADevice: TRazerDevice);
  procedure SetSpectrum(const ADevice: TRazerDevice);
  procedure SetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed = resMedium);
  procedure SetWave(const ADevice: TRazerDevice; ADirection: integer = 1);
  procedure SetNone(const ADevice: TRazerDevice);
  procedure SetBrightness(const ADevice: TRazerDevice; ABrightness: byte);
  function GetBrightness(const ADevice: TRazerDevice): byte;
    
    // Effects - All devices
  procedure SetStaticAll(const AColor: TRGBColor);
  procedure SetBreathDualAll(const AColor1, AColor2: TRGBColor);
  procedure SetBreathRandomAll;
  procedure SetSpectrumAll;
  procedure SetNoneAll;
  procedure FlashAll(const AColor1, AColor2: TRGBColor);
    
    // Convenience flash methods
  procedure StrobeFlash(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor;
    DurationMS: integer = 3000; IntervalMS: integer = 100);
  procedure StrobeFlashAll(const AColor1, AColor2: TRGBColor;
    DurationMS: integer = 3000; IntervalMS: integer = 100);
    
    // Properties
  property Initialized: boolean read FInitialized;
  property Devices: TRazerDeviceList read FDevices;
  property LastError: string read FLastError;
end;

{**
  @brief Create a `TRGBColor` from explicit channel components.
  @param R Red channel in the range 0..255.
  @param G Green channel in the range 0..255.
  @param B Blue channel in the range 0..255.
  @returns Constructed color record.
}
function RGB(R, G, B: byte): TRGBColor;

{**
  @brief Convert a color record to a packed integer accepted by the SDK.
  @param AColor Color to convert.
  @returns Integer value with RGB channels packed as `$RRGGBB`.
}
function RGBToInt(const AColor: TRGBColor): integer;

{**
  @brief Restore a `TRGBColor` from a packed integer value.
  @param AValue Packed `$RRGGBB` value.
  @returns Equivalent color record.
}
function IntToRGB(AValue: integer): TRGBColor;

{**
  @brief Human-readable name for a `TRazerDeviceType`.
  @param AType Device type to describe.
  @returns Description such as 'Keyboard' or 'Mousepad'.
}
function DeviceTypeToStr(AType: TRazerDeviceType): string;

// Predefined colors
const
clRazerRed: TRGBColor = (R: 255; G: 0; B: 0);
clRazerGreen: TRGBColor = (R: 0; G: 255; B: 0);
clRazerBlue: TRGBColor = (R: 0; G: 0; B: 255);
clRazerWhite: TRGBColor = (R: 255; G: 255; B: 255);
clRazerBlack: TRGBColor = (R: 0; G: 0; B: 0);
clRazerYellow: TRGBColor = (R: 255; G: 255; B: 0);
clRazerCyan: TRGBColor = (R: 0; G: 255; B: 255);
clRazerMagenta: TRGBColor = (R: 255; G: 0; B: 255);
clRazerOrange: TRGBColor = (R: 255; G: 128; B: 0);
clRazerPurple: TRGBColor = (R: 128; G: 0; B: 255);

implementation

{ Helper functions }

function RGB(R, G, B: byte): TRGBColor;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function RGBToInt(const AColor: TRGBColor): integer;
begin
  Result := (AColor.R shl 16) or (AColor.G shl 8) or AColor.B;
end;

function IntToRGB(AValue: integer): TRGBColor;
begin
  Result.R := (AValue shr 16) and $FF;
  Result.G := (AValue shr 8) and $FF;
  Result.B := AValue and $FF;
end;

function DeviceTypeToStr(AType: TRazerDeviceType): string;
begin
  case AType of
  rdtKeyboard:
    Result := 'Keyboard';
  rdtMouse:
    Result := 'Mouse';
  rdtMousepad:
    Result := 'Mousepad';
  rdtHeadset:
    Result := 'Headset';
  rdtKeypad:
    Result := 'Keypad';
  rdtChromaLink:
    Result := 'Chroma Link';
  rdtLaptop:
    Result := 'Laptop';
  else
    Result := 'Unknown';
  end;
end;

{ TRazerDeviceList }

function TRazerDeviceList.GetDevice(Index: integer): TRazerDevice;
begin
  Result := TRazerDevice(inherited Items[Index]);
end;

function TRazerDeviceList.Add(ADevice: TRazerDevice): integer;
begin
  Result := inherited Add(ADevice);
end;

function TRazerDeviceList.FindBySerial(const ASerial: string): TRazerDevice;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Serial = ASerial then
      Exit(Items[i]);
end;

{ TRazerChromaBase }

constructor TRazerChromaBase.Create;
begin
  inherited Create;
  FDevices := TRazerDeviceList.Create(true); // Owns objects
  FInitialized := false;
  FLastError := '';
end;

destructor TRazerChromaBase.Destroy;
begin
  if FInitialized then
    Finalize;
  FDevices.Free;
  inherited Destroy;
end;

procedure TRazerChromaBase.CheckInitialized;
begin
  if not FInitialized then
    raise ERazerNotInitialized.Create('Razer Chroma not initialized. Call Initialize first.');
end;

function TRazerChromaBase.Initialize: boolean;
begin
  if FInitialized then
    Exit(true);
    
  Result := DoInitialize;
  FInitialized := Result;
  
  if Result then
    RefreshDevices;
end;

procedure TRazerChromaBase.Finalize;
begin
  if not FInitialized then
    Exit;
    
  DoFinalize;
  FDevices.Clear;
  FInitialized := false;
end;

procedure TRazerChromaBase.RefreshDevices;
begin
  CheckInitialized;
  FDevices.Clear;
  DoRefreshDevices;
end;

function TRazerChromaBase.GetDeviceCount: integer;
begin
  Result := FDevices.Count;
end;

function TRazerChromaBase.GetDevice(Index: integer): TRazerDevice;
begin
  if (Index < 0) or (Index >= FDevices.Count) then
    raise ERazerDeviceNotFound.CreateFmt('Device index %d out of range', [Index]);
  Result := FDevices[Index];
end;

function TRazerChromaBase.GetDeviceBySerial(const ASerial: string): TRazerDevice;
begin
  Result := FDevices.FindBySerial(ASerial);
  if Result = nil then
    raise ERazerDeviceNotFound.CreateFmt('Device with serial "%s" not found', [ASerial]);
end;

// Single device effects

procedure TRazerChromaBase.SetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor);
begin
  CheckInitialized;
  if not DoSetStatic(ADevice, AColor) then
    raise ERazerEffectFailed.Create('Failed to set static effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor);
begin
  CheckInitialized;
  if not DoSetBreathSingle(ADevice, AColor) then
    raise ERazerEffectFailed.Create('Failed to set breath effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor);
begin
  CheckInitialized;
  if not DoSetBreathDual(ADevice, AColor1, AColor2) then
    raise ERazerEffectFailed.Create('Failed to set breath dual effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetBreathRandom(const ADevice: TRazerDevice);
begin
  CheckInitialized;
  if not DoSetBreathRandom(ADevice) then
    raise ERazerEffectFailed.Create('Failed to set breath random effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetSpectrum(const ADevice: TRazerDevice);
begin
  CheckInitialized;
  if not DoSetSpectrum(ADevice) then
    raise ERazerEffectFailed.Create('Failed to set spectrum effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed);
begin
  CheckInitialized;
  if not DoSetReactive(ADevice, AColor, ASpeed) then
    raise ERazerEffectFailed.Create('Failed to set reactive effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetWave(const ADevice: TRazerDevice; ADirection: integer);
begin
  CheckInitialized;
  if not DoSetWave(ADevice, ADirection) then
    raise ERazerEffectFailed.Create('Failed to set wave effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetNone(const ADevice: TRazerDevice);
begin
  CheckInitialized;
  if not DoSetNone(ADevice) then
    raise ERazerEffectFailed.Create('Failed to set none effect: ' + FLastError);
end;

procedure TRazerChromaBase.SetBrightness(const ADevice: TRazerDevice; ABrightness: byte);
begin
  CheckInitialized;
  if not DoSetBrightness(ADevice, ABrightness) then
    raise ERazerEffectFailed.Create('Failed to set brightness: ' + FLastError);
end;

function TRazerChromaBase.GetBrightness(const ADevice: TRazerDevice): byte;
begin
  CheckInitialized;
  Result := DoGetBrightness(ADevice);
end;

// All devices effects

procedure TRazerChromaBase.SetStaticAll(const AColor: TRGBColor);
var
  i: integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetStatic(FDevices[i], AColor);
end;

procedure TRazerChromaBase.SetBreathDualAll(const AColor1, AColor2: TRGBColor);
var
  i: integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetBreathDual(FDevices[i], AColor1, AColor2);
end;

procedure TRazerChromaBase.SetBreathRandomAll;
var
  i: integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetBreathRandom(FDevices[i]);
end;

procedure TRazerChromaBase.SetSpectrumAll;
var
  i: integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetSpectrum(FDevices[i]);
end;

procedure TRazerChromaBase.SetNoneAll;
var
  i: integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetNone(FDevices[i]);
end;

procedure TRazerChromaBase.FlashAll(const AColor1, AColor2: TRGBColor);
begin
  SetBreathDualAll(AColor1, AColor2);
end;

procedure TRazerChromaBase.StrobeFlash(const ADevice: TRazerDevice; 
const AColor1, AColor2: TRGBColor; DurationMS, IntervalMS: integer);
var
  Elapsed: integer;
  Toggle: boolean;
begin
  CheckInitialized;
  Elapsed := 0;
  Toggle := true;
  
  while Elapsed < DurationMS do
  begin
    if Toggle then
      DoSetStatic(ADevice, AColor1)
    else
      DoSetStatic(ADevice, AColor2);
    
    Toggle := not Toggle;
    Sleep(IntervalMS);
    Inc(Elapsed, IntervalMS);
  end;
end;

procedure TRazerChromaBase.StrobeFlashAll(const AColor1, AColor2: TRGBColor;
DurationMS, IntervalMS: integer);
var
  Elapsed: integer;
  Toggle: boolean;
begin
  CheckInitialized;
  Elapsed := 0;
  Toggle := true;
  
  while Elapsed < DurationMS do
  begin
    if Toggle then
      SetStaticAll(AColor1)
    else
      SetStaticAll(AColor2);
    
    Toggle := not Toggle;
    Sleep(IntervalMS);
    Inc(Elapsed, IntervalMS);
  end;
end;

end.
