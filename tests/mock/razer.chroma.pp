unit razer.chroma;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, contnrs;

type
  TRGBColor = packed record
    R, G, B: byte;
  end;

  function RGB(R, G, B: byte): TRGBColor;
  function RGBToInt(const AColor: TRGBColor): integer;

const
  clRazerBlack: TRGBColor = (R: 0; G: 0; B: 0);
  clRazerRed: TRGBColor = (R: 255; G: 0; B: 0);
  clRazerGreen: TRGBColor = (R: 0; G: 255; B: 0);
  clRazerBlue: TRGBColor = (R: 0; G: 0; B: 255);

type
  { Mock device used by the Chroma mock }
  TRazerDeviceMock = class(TObject)
  public
    Name: string;
    constructor Create(const AName: string); virtual;
  end;


  { Mock concrete implementation used by headless tests }
  TRazerChromaBase = class
  protected
    FInitialized: boolean;
    FDevices: TObjectList; // will contain simple mock device objects (TRazerDeviceMock)
    FLastError: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Supported: boolean; virtual;

    function Initialize: boolean; virtual;
    procedure Finalize; virtual;
    procedure RefreshDevices; virtual;

    function GetDeviceCount: integer; virtual;
    function GetDevice(Index: integer): TRazerDeviceMock; virtual;

    procedure SetStatic(ADevice: TRazerDeviceMock; const AColor: TRGBColor); virtual;
    procedure SetStaticAll(const AColor: TRGBColor); virtual;
    procedure SetBreathDualAll(const AColor1, AColor2: TRGBColor); virtual;
    procedure SetNoneAll; virtual;

    property LastError: string read FLastError;
    property Initialized: boolean read FInitialized;
  end;

implementation

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

{ TRazerChromaBase }

constructor TRazerChromaBase.Create;
begin
  inherited Create;
  FDevices := TObjectList.Create(true);
  FInitialized := False;
  FLastError := '';
end;

constructor TRazerDeviceMock.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
end;

destructor TRazerChromaBase.Destroy;
begin
  if FInitialized then
    Finalize;
  FDevices.Free;
  inherited Destroy;
end;

class function TRazerChromaBase.Supported: boolean;
begin
  Result := False;
end;

function TRazerChromaBase.Initialize: boolean;
begin
  if FInitialized then
    Exit(True);
  FInitialized := True;
  // Add a single mock device for enumeration
  FDevices.Clear;
  FDevices.Add(TRazerDeviceMock.Create('MockDevice0'));
  Result := True;
end;

function TRazerChromaBase.GetDeviceCount: integer;
begin
  Result := FDevices.Count;
end;

function TRazerChromaBase.GetDevice(Index: integer): TRazerDeviceMock;
begin
  Result := TRazerDeviceMock(FDevices[Index]);
end;

procedure TRazerChromaBase.Finalize;
begin
  if not FInitialized then
    Exit;
  FDevices.Clear;
  FInitialized := False;
end;

procedure TRazerChromaBase.RefreshDevices;
begin
  // In tests ensure there is at least one device
  if not FInitialized then
    Exit;
  if FDevices.Count = 0 then
    FDevices.Add(TRazerDeviceMock.Create('MockDevice0'));
end;

procedure TRazerChromaBase.SetStatic(ADevice: TRazerDeviceMock; const AColor: TRGBColor);
begin
  // no-op in headless mock
end;

procedure TRazerChromaBase.SetStaticAll(const AColor: TRGBColor);
begin
  // no-op for mock
end;

procedure TRazerChromaBase.SetBreathDualAll(const AColor1, AColor2: TRGBColor);
begin
  // no-op for mock
end;

procedure TRazerChromaBase.SetNoneAll;
begin
  // no-op for mock
end;

end.
