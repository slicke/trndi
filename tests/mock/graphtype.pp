unit GraphType;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

// Minimal placeholder for GraphType used in headless builds.
// Surfaces just the bits of TRawImageDescription referenced by umain's
// dot-rendering helpers — enough to compile, not enough to render.

type
  TRawImageDescription = record
    Width: Cardinal;
    Height: Cardinal;
    BitsPerPixel: Byte;
    procedure Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight: Integer);
  end;

implementation

procedure TRawImageDescription.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
  BitsPerPixel := 32;
end;

end.
