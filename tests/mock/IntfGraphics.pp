unit IntfGraphics;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, FPImage, GraphType;

// Minimal placeholder for IntfGraphics used in headless builds.
// TLazIntfImage mirrors only the surface area touched by umain_dots.inc:
// construction with a size, assigning a TRawImageDescription, allocating
// pixel storage, and writing into the Colors[X, Y] array. Nothing here
// actually renders — tests don't paint.

type
  TLazIntfImage = class(TObject)
  private
    FWidth: Integer;
    FHeight: Integer;
    FDescription: TRawImageDescription;
    function GetColors(X, Y: Integer): TFPColor;
    procedure SetColors(X, Y: Integer; const AValue: TFPColor);
  public
    constructor Create(AWidth, AHeight: Integer);
    procedure CreateData; virtual;
    property DataDescription: TRawImageDescription read FDescription write FDescription;
    property Colors[X, Y: Integer]: TFPColor read GetColors write SetColors;
  end;

implementation

constructor TLazIntfImage.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TLazIntfImage.CreateData;
begin
  // no-op: no pixel buffer is allocated in the headless mock
end;

function TLazIntfImage.GetColors(X, Y: Integer): TFPColor;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TLazIntfImage.SetColors(X, Y: Integer; const AValue: TFPColor);
begin
  // no-op
end;

end.
