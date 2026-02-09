unit razer.chroma;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils;

type
  TRGBColor = packed record
    R, G, B: byte;
  end;

  function RGB(R, G, B: byte): TRGBColor;
  function RGBToInt(const AColor: TRGBColor): integer;

type
  TRazerChromaBase = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Supported: boolean; virtual;
    // Minimal convenience method used by callers in umain
    procedure SetStatic(ADevice: TObject; const AColor: TRGBColor); virtual;
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
end;

destructor TRazerChromaBase.Destroy;
begin
  inherited Destroy;
end;

class function TRazerChromaBase.Supported: boolean;
begin
  Result := False;
end;

procedure TRazerChromaBase.SetStatic(ADevice: TObject; const AColor: TRGBColor);
begin
  // no-op in headless mock
end;

end.
