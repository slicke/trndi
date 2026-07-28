(* Shared 32-bit alpha-raster helpers for the TfBG painters.
   Extracted from inc/umain_dots.inc so inc/umain_paint.inc can reuse them.
   Included in the implementation section of umain.pp only, and before any
   include that rasterizes with alpha.

   The pattern these support: build an RGBA raster with analytical coverage
   (LCL's Ellipse/LineTo are strictly aliased even with AntialiasingMode
   := amOn), wrap it in the platform image type, then let the OS composite it
   over the destination. The OS reads the destination pixels itself, which a
   TGraphicControl canvas cannot reliably do during its initial paint or
   during a window drag.
*)

// Local copy of the Win32 BLENDFUNCTION struct + msimg32!AlphaBlend import.
// We avoid `uses Windows` because Windows.TBitmap (the GDI BITMAP record)
// would shadow Graphics.TBitmap in the rest of this implementation file.
{$ifdef Windows}
type
  TLocalBlendFunction = packed record
    BlendOp: byte;
    BlendFlags: byte;
    SourceConstantAlpha: byte;
    AlphaFormat: byte;
  end;
const
  LOCAL_AC_SRC_OVER  = $00;
  LOCAL_AC_SRC_ALPHA = $01;

function LocalAlphaBlend(
  hdcDest: HDC; xDest, yDest, wDest, hDest: integer;
  hdcSrc: HDC;  xSrc,  ySrc,  wSrc,  hSrc:  integer;
  ftn: TLocalBlendFunction): LongBool;
  stdcall; external 'msimg32' name 'AlphaBlend';
{$endif}

type
  // Finished, platform-ready alpha image: premultiplied 32-bit bitmap for the
  // Win32 AlphaBlend path, RGBA PNG for the LCL Draw path elsewhere.
  // TBitmap.Draw on GTK/Qt does not reliably composite an alpha channel
  // (the corners with alpha=0 blit as the bitmap's raw RGB — black for our
  // input). TPortableNetworkGraphic, in contrast, declares itself as a
  // proper RGBA image to LCL, so Canvas.Draw composites with alpha via
  // the backend (Cairo on GTK, QPainter on Qt, CoreGraphics on Cocoa).
  {$ifdef Windows}
  TAlphaImage = TBitmap;
  {$else}
  TAlphaImage = TPortableNetworkGraphic;
  {$endif}

const
  // Whether the platform blit consumes premultiplied RGB.
  //
  // Windows: AlphaBlend with AC_SRC_ALPHA is defined to take premultiplied
  // source pixels.
  //
  // Cocoa: less obvious, and it is a defect rather than a contract. LCL maps
  // the Init_BPP32_B8G8R8A8_BIO_TTB description below to cbtBGRA;
  // TCocoaBitmap.PreMultiplyAlpha bails out for anything that is not
  // cbtARGB/cbtRGBA, yet CreateHandle still hands the rep to CoreGraphics
  // without NSAlphaNonpremultipliedBitmapFormat. Straight RGB therefore
  // composites as C + dst*(1-a), and every partially transparent pixel blows
  // out toward the background — barely visible on a solid disc (its body is
  // alpha = 1), but it erased the prediction × (all thin antialiased stroke)
  // against the brighter out-of-range backgrounds.
  //
  // GTK (GdkPixbuf) and Qt (QImage::Format_ARGB32) both take straight alpha.
  {$if defined(Windows) or defined(DARWIN)}
  ALPHA_BLIT_PREMULTIPLIED = true;
  {$else}
  ALPHA_BLIT_PREMULTIPLIED = false;
  {$endif}

// Allocate a straight-alpha RGBA raster to render into.
function NewAlphaRaster(AWidth, AHeight: integer): TLazIntfImage;
var
  rawDesc: TRawImageDescription;
begin
  Result := TLazIntfImage.Create(0, 0);
  rawDesc.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
  Result.DataDescription := rawDesc;
  Result.CreateData;
end;

// Clamp to 0..255 and pack into the raster. Both Put* entry points funnel
// here so the channel packing exists once.
procedure StoreAlphaPixel(AIntf: TLazIntfImage; X, Y: integer;
  AR, AG, AB, AAlpha: integer);
var
  pix: TFPColor;
begin
  if AR > 255 then AR := 255 else if AR < 0 then AR := 0;
  if AG > 255 then AG := 255 else if AG < 0 then AG := 0;
  if AB > 255 then AB := 255 else if AB < 0 then AB := 0;
  if AAlpha > 255 then AAlpha := 255 else if AAlpha < 0 then AAlpha := 0;

  pix.red := AR shl 8 or AR;
  pix.green := AG shl 8 or AG;
  pix.blue := AB shl 8 or AB;
  pix.alpha := AAlpha shl 8 or AAlpha;
  AIntf.Colors[X, Y] := pix;
end;

// ALPHA_BLIT_PREMULTIPLIED is a compile-time constant, so FPC folds the
// branches below and flags the losing one as unreachable (6018). That is the
// intended outcome — a plain `if` keeps both branches type-checked on every
// platform, which an {$ifdef} would not — so silence the note for this pair.
{$push}
{$warn 6018 off}

// Write one straight-alpha pixel (RGB in 0..255, AAlpha in 0..1). Use this
// when the colour is known independently of its coverage — the usual case
// for a single-colour shape whose alpha is just edge antialiasing.
procedure PutAlphaPixel(AIntf: TLazIntfImage; X, Y: integer;
  AR, AG, AB, AAlpha: double);
begin
  if AAlpha < 0 then AAlpha := 0
  else if AAlpha > 1 then AAlpha := 1;

  if ALPHA_BLIT_PREMULTIPLIED then
    StoreAlphaPixel(AIntf, X, Y, Round(AR * AAlpha), Round(AG * AAlpha),
      Round(AB * AAlpha), Round(AAlpha * 255))
  else
    StoreAlphaPixel(AIntf, X, Y, Round(AR), Round(AG), Round(AB),
      Round(AAlpha * 255));
end;

// Write one pixel whose RGB is *already* premultiplied by AAlpha. Use this
// when the caller composites several sources into an accumulator, since
// Porter-Duff "over" produces premultiplied output naturally — going through
// PutAlphaPixel would mean dividing the alpha back out only for the platform
// to multiply it straight back in.
procedure PutPremultipliedPixel(AIntf: TLazIntfImage; X, Y: integer;
  APR, APG, APB, AAlpha: double);
begin
  if AAlpha < 0 then AAlpha := 0
  else if AAlpha > 1 then AAlpha := 1;

  if ALPHA_BLIT_PREMULTIPLIED then
    StoreAlphaPixel(AIntf, X, Y, Round(APR), Round(APG), Round(APB),
      Round(AAlpha * 255))
  else if AAlpha > 0 then
    StoreAlphaPixel(AIntf, X, Y, Round(APR / AAlpha), Round(APG / AAlpha),
      Round(APB / AAlpha), Round(AAlpha * 255))
  else
    // Fully transparent: no colour to recover, and dividing would trap.
    StoreAlphaPixel(AIntf, X, Y, 0, 0, 0, 0);
end;

{$pop}

// Wrap a finished raster in the platform image type.
function AlphaImageFromIntf(AIntf: TLazIntfImage;
  AWidth, AHeight: integer): TAlphaImage;
begin
  {$ifdef Windows}
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(AWidth, AHeight);
  Result.LoadFromIntfImage(AIntf);
  {$else}
  Result := TPortableNetworkGraphic.Create;
  Result.LoadFromIntfImage(AIntf);
  {$endif}
end;

// Composite a finished alpha image over ACanvas, scaling when the destination
// rectangle differs from the source size.
procedure BlitAlphaImage(ACanvas: TCanvas; AImage: TAlphaImage;
  ASrcW, ASrcH, ADestX, ADestY, ADestW, ADestH: integer);
{$ifdef Windows}
var
  blend: TLocalBlendFunction;
begin
  blend.BlendOp := LOCAL_AC_SRC_OVER;
  blend.BlendFlags := 0;
  blend.SourceConstantAlpha := 255;
  blend.AlphaFormat := LOCAL_AC_SRC_ALPHA;
  LocalAlphaBlend(ACanvas.Handle, ADestX, ADestY, ADestW, ADestH,
                  AImage.Canvas.Handle, 0, 0, ASrcW, ASrcH, blend);
end;
{$else}
begin
  if (ADestW = ASrcW) and (ADestH = ASrcH) then
    ACanvas.Draw(ADestX, ADestY, AImage)
  else
    ACanvas.StretchDraw(
      Classes.Rect(ADestX, ADestY, ADestX + ADestW, ADestY + ADestH), AImage);
end;
{$endif}
