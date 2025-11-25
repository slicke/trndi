unit RazerChroma;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, contnrs;

type
  { TRGBColor }
  TRGBColor = packed record
    R, G, B: Byte;
  end;

  { TRazerDeviceType }
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

  { TRazerDevice }
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

  { TRazerDeviceList }
  TRazerDeviceList = class(TObjectList)
  private
    function GetDevice(Index: Integer): TRazerDevice;
  public
    function Add(ADevice: TRazerDevice): Integer;
    function FindBySerial(const ASerial: string): TRazerDevice;
    property Items[Index: Integer]: TRazerDevice read GetDevice; default;
  end;

  { ERazerException }
  ERazerException = class(Exception);
  ERazerNotInitialized = class(ERazerException);
  ERazerDeviceNotFound = class(ERazerException);
  ERazerEffectFailed = class(ERazerException);

  { TRazerEffectSpeed }
  TRazerEffectSpeed = (resSlow = 1, resMedium = 2, resFast = 3);

  { TRazerChromaBase - Abstract base class }
  TRazerChromaBase = class abstract
  protected
    FInitialized: Boolean;
    FDevices: TRazerDeviceList;
    FLastError: string;
    
    procedure CheckInitialized;
    function DoInitialize: Boolean; virtual; abstract;
    procedure DoFinalize; virtual; abstract;
    procedure DoRefreshDevices; virtual; abstract;
    
    // Abstract effect methods
    function DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean; virtual; abstract;
    function DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean; virtual; abstract;
    function DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean; virtual; abstract;
    function DoSetBreathRandom(const ADevice: TRazerDevice): Boolean; virtual; abstract;
    function DoSetSpectrum(const ADevice: TRazerDevice): Boolean; virtual; abstract;
    function DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean; virtual; abstract;
    function DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean; virtual; abstract;
    function DoSetNone(const ADevice: TRazerDevice): Boolean; virtual; abstract;
    function DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean; virtual; abstract;
    function DoGetBrightness(const ADevice: TRazerDevice): Byte; virtual; abstract;
    
  public
    constructor Create; virtual;
    destructor Destroy; override;
    
    // Initialization
    function Initialize: Boolean;
    procedure Finalize;
    procedure RefreshDevices;
    
    // Device access
    function GetDeviceCount: Integer;
    function GetDevice(Index: Integer): TRazerDevice;
    function GetDeviceBySerial(const ASerial: string): TRazerDevice;
    
    // Effects - Single device
    procedure SetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor);
    procedure SetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor);
    procedure SetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor);
    procedure SetBreathRandom(const ADevice: TRazerDevice);
    procedure SetSpectrum(const ADevice: TRazerDevice);
    procedure SetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed = resMedium);
    procedure SetWave(const ADevice: TRazerDevice; ADirection: Integer = 1);
    procedure SetNone(const ADevice: TRazerDevice);
    procedure SetBrightness(const ADevice: TRazerDevice; ABrightness: Byte);
    function GetBrightness(const ADevice: TRazerDevice): Byte;
    
    // Effects - All devices
    procedure SetStaticAll(const AColor: TRGBColor);
    procedure SetBreathDualAll(const AColor1, AColor2: TRGBColor);
    procedure SetBreathRandomAll;
    procedure SetSpectrumAll;
    procedure SetNoneAll;
    procedure FlashAll(const AColor1, AColor2: TRGBColor);
    
    // Convenience flash methods
    procedure StrobeFlash(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor; 
      DurationMS: Integer = 3000; IntervalMS: Integer = 100);
    procedure StrobeFlashAll(const AColor1, AColor2: TRGBColor;
      DurationMS: Integer = 3000; IntervalMS: Integer = 100);
    
    // Properties
    property Initialized: Boolean read FInitialized;
    property Devices: TRazerDeviceList read FDevices;
    property LastError: string read FLastError;
  end;

// Helper functions
function RGB(R, G, B: Byte): TRGBColor;
function RGBToInt(const AColor: TRGBColor): Integer;
function IntToRGB(AValue: Integer): TRGBColor;
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

function RGB(R, G, B: Byte): TRGBColor;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function RGBToInt(const AColor: TRGBColor): Integer;
begin
  Result := (AColor.R shl 16) or (AColor.G shl 8) or AColor.B;
end;

function IntToRGB(AValue: Integer): TRGBColor;
begin
  Result.R := (AValue shr 16) and $FF;
  Result.G := (AValue shr 8) and $FF;
  Result.B := AValue and $FF;
end;

function DeviceTypeToStr(AType: TRazerDeviceType): string;
begin
  case AType of
    rdtKeyboard: Result := 'Keyboard';
    rdtMouse: Result := 'Mouse';
    rdtMousepad: Result := 'Mousepad';
    rdtHeadset: Result := 'Headset';
    rdtKeypad: Result := 'Keypad';
    rdtChromaLink: Result := 'Chroma Link';
    rdtLaptop: Result := 'Laptop';
  else
    Result := 'Unknown';
  end;
end;

{ TRazerDeviceList }

function TRazerDeviceList.GetDevice(Index: Integer): TRazerDevice;
begin
  Result := TRazerDevice(inherited Items[Index]);
end;

function TRazerDeviceList.Add(ADevice: TRazerDevice): Integer;
begin
  Result := inherited Add(ADevice);
end;

function TRazerDeviceList.FindBySerial(const ASerial: string): TRazerDevice;
var
  i: Integer;
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
  FDevices := TRazerDeviceList.Create(True); // Owns objects
  FInitialized := False;
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

function TRazerChromaBase.Initialize: Boolean;
begin
  if FInitialized then
    Exit(True);
    
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
  FInitialized := False;
end;

procedure TRazerChromaBase.RefreshDevices;
begin
  CheckInitialized;
  FDevices.Clear;
  DoRefreshDevices;
end;

function TRazerChromaBase.GetDeviceCount: Integer;
begin
  Result := FDevices.Count;
end;

function TRazerChromaBase.GetDevice(Index: Integer): TRazerDevice;
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

procedure TRazerChromaBase.SetWave(const ADevice: TRazerDevice; ADirection: Integer);
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

procedure TRazerChromaBase.SetBrightness(const ADevice: TRazerDevice; ABrightness: Byte);
begin
  CheckInitialized;
  if not DoSetBrightness(ADevice, ABrightness) then
    raise ERazerEffectFailed.Create('Failed to set brightness: ' + FLastError);
end;

function TRazerChromaBase.GetBrightness(const ADevice: TRazerDevice): Byte;
begin
  CheckInitialized;
  Result := DoGetBrightness(ADevice);
end;

// All devices effects

procedure TRazerChromaBase.SetStaticAll(const AColor: TRGBColor);
var
  i: Integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetStatic(FDevices[i], AColor);
end;

procedure TRazerChromaBase.SetBreathDualAll(const AColor1, AColor2: TRGBColor);
var
  i: Integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetBreathDual(FDevices[i], AColor1, AColor2);
end;

procedure TRazerChromaBase.SetBreathRandomAll;
var
  i: Integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetBreathRandom(FDevices[i]);
end;

procedure TRazerChromaBase.SetSpectrumAll;
var
  i: Integer;
begin
  CheckInitialized;
  for i := 0 to FDevices.Count - 1 do
    DoSetSpectrum(FDevices[i]);
end;

procedure TRazerChromaBase.SetNoneAll;
var
  i: Integer;
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
  const AColor1, AColor2: TRGBColor; DurationMS, IntervalMS: Integer);
var
  Elapsed: Integer;
  Toggle: Boolean;
begin
  CheckInitialized;
  Elapsed := 0;
  Toggle := True;
  
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
  DurationMS, IntervalMS: Integer);
var
  Elapsed: Integer;
  Toggle: Boolean;
begin
  CheckInitialized;
  Elapsed := 0;
  Toggle := True;
  
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
