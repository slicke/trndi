(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-09-20: Created by moving the alpha-raster primitives out of
 *   inc/umain_alphablit.inc and the smooth circle/×/polyline rasterizers out
 *   of inc/umain_dots.inc, so the history graph and the trend arrow can draw
 *   antialiased shapes through the same code as the main window. Added the
 *   stroke-list rasterizer (DrawSmoothStrokes) and the dashed polyline.
 * - 2026-09-20: Moved DrawRangeBands here from inc/umain_paint.inc and gave
 *   it a destination offset, so the history graph can tint its plot area
 *   with the same alpha bands as the main window.
 * - 2026-09-20: DrawSmoothDashedPolyline compares its dash phase with a
 *   tolerance; a float residue could stall the walk forever.
 *)

unit trndi.raster;

{**
  Antialiased shape rasterizers for the LCL canvas.

  LCL's Ellipse/LineTo are strictly aliased on GDI and Qt even with
  AntialiasingMode := amOn, so the shapes here are rendered at their final
  size into an RGBA raster with analytical pixel coverage, wrapped in the
  platform image type, and handed to the OS compositor. The OS reads the
  destination pixels itself, which a TGraphicControl canvas cannot reliably
  do during its initial paint or during a window drag.

  Everything is main-thread only: the rendered-shape caches are plain globals
  used from paint handlers and freed in this unit's finalization.
}

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Math, Graphics, GraphType, IntfGraphics, FPImage,
{$ifdef Windows}LCLType,{$endif} // HDC for the msimg32 AlphaBlend import
Generics.Collections;

type
  {** Finished, platform-ready alpha image: premultiplied 32-bit bitmap for the
      Win32 AlphaBlend path, RGBA PNG for the LCL Draw path elsewhere.
      TBitmap.Draw on GTK/Qt does not reliably composite an alpha channel
      (the corners with alpha=0 blit as the bitmap's raw RGB — black for our
      input). TPortableNetworkGraphic, in contrast, declares itself as a
      proper RGBA image to LCL, so Canvas.Draw composites with alpha via
      the backend (Cairo on GTK, QPainter on Qt, CoreGraphics on Cocoa). }
  {$ifdef Windows}
  TAlphaImage = TBitmap;
  {$else}
  TAlphaImage = TPortableNetworkGraphic;
  {$endif}

  {** One capsule stroke for DrawSmoothStrokes, in canvas coordinates. }
  TSmoothStroke = record
    X1, Y1, X2, Y2: double;
    Color: TColor;
  end;

  {** One horizontal band for DrawRangeBands. Top and Bottom are rows inside
      the band raster, inclusive; rows outside the raster are clipped. }
  TRangeBand = record
    Top, Bottom: integer;
    Color: TColor;
    Alpha: byte;
  end;

const
  {** Whether the platform blit consumes premultiplied RGB.

      Windows: AlphaBlend with AC_SRC_ALPHA is defined to take premultiplied
      source pixels.

      Cocoa: less obvious, and it is a defect rather than a contract. LCL maps
      the Init_BPP32_B8G8R8A8_BIO_TTB description to cbtBGRA;
      TCocoaBitmap.PreMultiplyAlpha bails out for anything that is not
      cbtARGB/cbtRGBA, yet CreateHandle still hands the rep to CoreGraphics
      without NSAlphaNonpremultipliedBitmapFormat. Straight RGB therefore
      composites as C + dst*(1-a), and every partially transparent pixel blows
      out toward the background — barely visible on a solid disc (its body is
      alpha = 1), but it erased the prediction × (all thin antialiased stroke)
      against the brighter out-of-range backgrounds.

      GTK (GdkPixbuf) and Qt (QImage::Format_ARGB32) both take straight alpha. }
  {$if defined(Windows) or defined(DARWIN)}
  ALPHA_BLIT_PREMULTIPLIED = true;
  {$else}
  ALPHA_BLIT_PREMULTIPLIED = false;
  {$endif}

{** Allocate a straight-alpha RGBA raster to render into. }
function NewAlphaRaster(AWidth, AHeight: integer): TLazIntfImage;

{** Write one straight-alpha pixel (RGB in 0..255, AAlpha in 0..1). Use this
    when the colour is known independently of its coverage — the usual case
    for a single-colour shape whose alpha is just edge antialiasing. }
procedure PutAlphaPixel(AIntf: TLazIntfImage; X, Y: integer;
  AR, AG, AB, AAlpha: double);

{** Write one pixel whose RGB is *already* premultiplied by AAlpha. Use this
    when the caller composites several sources into an accumulator, since
    Porter-Duff "over" produces premultiplied output naturally. }
procedure PutPremultipliedPixel(AIntf: TLazIntfImage; X, Y: integer;
  APR, APG, APB, AAlpha: double);

{** Wrap a finished raster in the platform image type. }
function AlphaImageFromIntf(AIntf: TLazIntfImage;
  AWidth, AHeight: integer): TAlphaImage;

{** Composite a finished alpha image over ACanvas, scaling when the
    destination rectangle differs from the source size. }
procedure BlitAlphaImage(ACanvas: TCanvas; AImage: TAlphaImage;
  ASrcW, ASrcH, ADestX, ADestY, ADestW, ADestH: integer);

{** Antialiased × mark: two corner-to-corner diagonals clipped by an
    ASize-square box at (AOffsetX, AOffsetY). AAlpha scales the whole mark's
    opacity. Rendered images are cached by (size, colour, thickness, alpha). }
procedure DrawSmoothX(ACanvas: TCanvas; ASize: integer; AColor: TColor;
  AThickness: integer; AOffsetX: integer = 0; AOffsetY: integer = 0;
  AAlpha: double = 1.0);

{** Antialiased disc of diameter ASize at (AOffsetX, AOffsetY), optionally
    rimmed by a ring of ARingWidth pixels in ARingColor. ADotColor = clNone
    draws no disc at all — a hollow ring only — so whatever sits behind the
    dot shows through the hole. Rendered images are cached by parameters. }
procedure DrawSmoothCircle(ACanvas: TCanvas; ASize: integer;
  ADotColor, ARingColor: TColor; ARingWidth: integer;
  AOffsetX: integer = 0; AOffsetY: integer = 0);

{** Antialiased capsule strokes, AThickness pixels wide, with round caps and
    round joints wherever strokes meet: overlapping strokes keep the strongest
    coverage, so joints never double-blend. Uncached — meant for shapes that
    are either drawn once into a cached surface or change every paint. }
procedure DrawSmoothStrokes(ACanvas: TCanvas;
  const AStrokes: array of TSmoothStroke; AThickness: integer);

{** Antialiased polyline through APts. AColors runs parallel to APts: each
    point's colour extends halfway toward both of its neighbours, so the
    trace switches colour mid-segment. With ACache the finished raster is
    kept in a single slot keyed by the trace's shape, so a repaint with an
    unmoved trace reuses it; pass false for a trace that is itself drawn into
    a cached bitmap, so it does not evict the live one. }
procedure DrawSmoothPolyline(ACanvas: TCanvas; const APts: array of TPoint;
  const AColors: array of TColor; AThickness: integer;
  ACache: boolean = true);

{** Antialiased dashed polyline through APts in one colour: ADashPx of ink,
    AGapPx of nothing, the pattern running continuously across the joints.
    Uncached. }
procedure DrawSmoothDashedPolyline(ACanvas: TCanvas;
  const APts: array of TPoint; AColor: TColor; AThickness, ADashPx,
  AGapPx: integer);

{** Paint a set of horizontal alpha bands, AWidth by AHeight, with the
    raster's origin at (ADestX, ADestY) on ACanvas.

    The bands are uniform horizontally, so they are rendered as a single
    premultiplied column that the OS stretches across AWidth. Bands composite
    in array order using Porter-Duff "over", so a band listed later and lying
    inside an earlier one reads as a deeper shade of it. Each band fades in
    over AGradientPx rows at its top and bottom edges; an edge that lies
    outside the raster is clipped without a fade. }
procedure DrawRangeBands(ACanvas: TCanvas; AWidth, AHeight: integer;
  const Bands: array of TRangeBand; AGradientPx: integer;
  ADestX: integer = 0; ADestY: integer = 0);

implementation

// Local copy of the Win32 BLENDFUNCTION struct + msimg32!AlphaBlend import.
// We avoid `uses Windows` because Windows.TBitmap (the GDI BITMAP record)
// would shadow Graphics.TBitmap in the rest of this unit.
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
  TShapeImageCache = specialize TObjectDictionary<string, TAlphaImage>;

const
  // Upper bound on distinct (shape, size, color, ...) combos kept alive. A
  // layout pass produces a handful of sizes and range colors; the cap only
  // stops unbounded growth across many window sizes. Clearing wholesale is
  // fine — entries are cheap to re-render once.
  SHAPE_IMAGE_CACHE_MAX = 32;

var
  // Rendered-shape cache. The main window repaints every dot on each tick
  // (tResize fires at 50 ms during drag) and the analytic-AA rasterizers cost
  // a per-pixel loop plus image-object churn per shape, while the dots share
  // a handful of (size, color) combos.
  ShapeImageCache: TShapeImageCache = nil;

  // Cached raster of the last connecting line drawn. One slot, not the shape
  // dictionary: the image is trace-sized (potentially megabytes at large
  // windows) and only one geometry is live at a time, so a dictionary would
  // just hoard stale traces until its wholesale clear. Keyed by the
  // bbox-relative points, so the vertical keep-in-view shifts and window
  // moves that don't change the trace's shape still hit.
  PolylineImage: TAlphaImage = nil;
  PolylineKey: string = '';
  PolylineW: integer = 0;
  PolylineH: integer = 0;

//------------------------------------------------------------------------------
// Raster primitives
//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
// Rendered-shape cache
//------------------------------------------------------------------------------

function GetCachedShapeImage(const AKey: string): TAlphaImage;
begin
  Result := nil;
  if Assigned(ShapeImageCache) then
    ShapeImageCache.TryGetValue(AKey, Result);
end;

procedure StoreCachedShapeImage(const AKey: string; AImage: TAlphaImage);
begin
  if ShapeImageCache = nil then
    ShapeImageCache := TShapeImageCache.Create([doOwnsValues]);
  if ShapeImageCache.Count >= SHAPE_IMAGE_CACHE_MAX then
    ShapeImageCache.Clear;
  ShapeImageCache.AddOrSetValue(AKey, AImage);
end;

// Composite a finished square shape image over ACanvas at the given offset.
// Shapes are always blitted 1:1 — they are rasterized at their final size.
procedure BlitShapeImage(ACanvas: TCanvas; AImage: TAlphaImage;
  ASize, AOffsetX, AOffsetY: integer); inline;
begin
  BlitAlphaImage(ACanvas, AImage, ASize, ASize, AOffsetX, AOffsetY,
    ASize, ASize);
end;

// Try to satisfy a paint entirely from the rendered-shape cache.
function TryBlitCachedShape(ACanvas: TCanvas; const AKey: string;
  ASize, AOffsetX, AOffsetY: integer): boolean;
var
  img: TAlphaImage;
begin
  img := GetCachedShapeImage(AKey);
  Result := img <> nil;
  if Result then
    BlitShapeImage(ACanvas, img, ASize, AOffsetX, AOffsetY);
end;

// Wrap the finished raster in the platform image, cache it under AKey and
// composite it onto the destination canvas. The caller still owns AIntf.
procedure FinishShapeRender(ACanvas: TCanvas; const AKey: string;
  AIntf: TLazIntfImage; ASize, AOffsetX, AOffsetY: integer);
var
  img: TAlphaImage;
begin
  img := AlphaImageFromIntf(AIntf, ASize, ASize);
  StoreCachedShapeImage(AKey, img);
  BlitShapeImage(ACanvas, img, ASize, AOffsetX, AOffsetY);
end;

//------------------------------------------------------------------------------
// Shapes
//------------------------------------------------------------------------------

// Alpha derives from the analytical distance to the nearest diagonal so the
// strokes stay smooth at small sizes without relying on GDI's aliased LineTo.
procedure DrawSmoothX(ACanvas: TCanvas; ASize: integer; AColor: TColor;
  AThickness: integer; AOffsetX: integer = 0; AOffsetY: integer = 0;
  AAlpha: double = 1.0);
var
  intf: TLazIntfImage;
  x, y: integer;
  cx, cy, dist1, dist2, dist, halfT, aPix, invSqrt2: double;
  fr, fg, fb: byte;
  key: string;
begin
  if (ASize < 2) or (AThickness < 1) or (AAlpha <= 0) then
    Exit;
  if AAlpha > 1.0 then
    AAlpha := 1.0;

  // The rendered image depends only on these parameters (offsets are applied
  // at blit time), so reuse the finished image across paints. Alpha is
  // quantized to percent so tiny confidence drift can't churn the cache.
  key := Format('x|%d|%d|%d|%d', [ASize, Integer(AColor), AThickness,
    Round(AAlpha * 100)]);
  if TryBlitCachedShape(ACanvas, key, ASize, AOffsetX, AOffsetY) then
    Exit;

  fr := Red(ColorToRGB(AColor));
  fg := Green(ColorToRGB(AColor));
  fb := Blue(ColorToRGB(AColor));

  cx := (ASize - 1) / 2.0;
  cy := (ASize - 1) / 2.0;
  halfT := AThickness / 2.0;
  invSqrt2 := 1.0 / Sqrt(2);

  intf := NewAlphaRaster(ASize, ASize);
  try
    for y := 0 to ASize - 1 do
      for x := 0 to ASize - 1 do
      begin
        dist1 := Abs((x - cx) - (y - cy)) * invSqrt2;
        dist2 := Abs((x - cx) + (y - cy)) * invSqrt2;
        if dist1 < dist2 then dist := dist1 else dist := dist2;

        if dist <= halfT - 0.5 then
          aPix := 1.0
        else if dist >= halfT + 0.5 then
          aPix := 0.0
        else
          aPix := halfT + 0.5 - dist;

        PutAlphaPixel(intf, x, y, fr, fg, fb, aPix * AAlpha);
      end;

    FinishShapeRender(ACanvas, key, intf, ASize, AOffsetX, AOffsetY);
  finally
    intf.Free;
  end;
end;

procedure DrawSmoothCircle(ACanvas: TCanvas; ASize: integer;
  ADotColor, ARingColor: TColor; ARingWidth: integer;
  AOffsetX: integer = 0; AOffsetY: integer = 0);
var
  intf: TLazIntfImage;
  x, y: integer;
  cx, cy, r, dist: double;
  ringOuter, ringInner: double;
  alphaDisc, alphaRing: double;
  fr, fg, fb: byte;
  rr, rg, rb: byte;
  invRing, aOut: double;
  premulR, premulG, premulB: double;
  hasRing, hasDisc: boolean;
  key: string;
begin
  if ASize < 2 then
    Exit;

  // The rendered image depends only on these parameters (offsets are applied
  // at blit time), so reuse the finished image across paints.
  key := Format('c|%d|%d|%d|%d', [ASize, Integer(ADotColor),
    Integer(ARingColor), ARingWidth]);
  if TryBlitCachedShape(ACanvas, key, ASize, AOffsetX, AOffsetY) then
    Exit;

  hasDisc := ADotColor <> clNone;
  if hasDisc then
  begin
    fr := Red(ColorToRGB(ADotColor));
    fg := Green(ColorToRGB(ADotColor));
    fb := Blue(ColorToRGB(ADotColor));
  end
  else
  begin
    fr := 0; fg := 0; fb := 0;
  end;
  hasRing := (ARingWidth > 0) and (ARingColor <> clNone);
  if hasRing then
  begin
    rr := Red(ColorToRGB(ARingColor));
    rg := Green(ColorToRGB(ARingColor));
    rb := Blue(ColorToRGB(ARingColor));
  end
  else
  begin
    rr := 0; rg := 0; rb := 0;
  end;

  cx := (ASize - 1) / 2.0;
  cy := (ASize - 1) / 2.0;
  // Half-pixel inset keeps the analytical AA edge inside the bounding box.
  r := ASize / 2.0 - 0.5;
  ringOuter := r;
  ringInner := r - ARingWidth;

  intf := NewAlphaRaster(ASize, ASize);
  try
    for y := 0 to ASize - 1 do
    begin
      for x := 0 to ASize - 1 do
      begin
        dist := Sqrt(Sqr(x - cx) + Sqr(y - cy));

        if not hasDisc then
          alphaDisc := 0.0
        else if dist <= r - 0.5 then
          alphaDisc := 1.0
        else if dist >= r + 0.5 then
          alphaDisc := 0.0
        else
          alphaDisc := r + 0.5 - dist;

        alphaRing := 0.0;
        if hasRing then
        begin
          if (dist >= ringInner - 0.5) and (dist <= ringOuter + 0.5) then
          begin
            if dist < ringInner + 0.5 then
              alphaRing := dist - (ringInner - 0.5)
            else if dist > ringOuter - 0.5 then
              alphaRing := (ringOuter + 0.5) - dist
            else
              alphaRing := 1.0;
            if alphaRing < 0 then alphaRing := 0
            else if alphaRing > 1 then alphaRing := 1;
          end;
        end;

        // Porter-Duff "ring over disc over transparent". The accumulator comes
        // out premultiplied by construction, so hand it over as-is instead of
        // dividing aOut back out only for the platform to multiply it in again.
        invRing := 1.0 - alphaRing;
        aOut := alphaRing + alphaDisc * invRing;

        premulR := rr * alphaRing + fr * alphaDisc * invRing;
        premulG := rg * alphaRing + fg * alphaDisc * invRing;
        premulB := rb * alphaRing + fb * alphaDisc * invRing;

        PutPremultipliedPixel(intf, x, y, premulR, premulG, premulB, aOut);
      end;
    end;

    FinishShapeRender(ACanvas, key, intf, ASize, AOffsetX, AOffsetY);
  finally
    intf.Free;
  end;
end;

//------------------------------------------------------------------------------
// Strokes
//------------------------------------------------------------------------------

// Rasterize a stroke list into a fresh straight-alpha image covering the
// strokes' padded bounding box, whose canvas position comes back in
// (AX, AY) and size in (AW, AH). Nil when there is nothing to draw. Every
// stroke is a capsule (round caps fall out of the point-to-segment
// distance), and overlapping strokes keep the strongest coverage in a
// shared buffer so joints and crossings don't double-blend. Only each
// stroke's own padded bbox is visited, so the cost tracks the ink rather
// than the raster's area.
function RasterizeStrokes(const AStrokes: array of TSmoothStroke;
  AThickness: integer; out AX, AY, AW, AH: integer): TLazIntfImage;
var
  cov: array of single;
  col: array of TColor;
  i, x, y, pad: integer;
  fMinX, fMinY, fMaxX, fMaxY, halfT: double;
  c: TColor;

  procedure RasterStroke(ax, ay, bx, by: double; AColor: TColor);
  var
    px, py, sx0, sy0, sx1, sy1: integer;
    dx, dy, len2, t, ddx, ddy, dist, a: double;
  begin
    dx := bx - ax;
    dy := by - ay;
    len2 := dx * dx + dy * dy;
    sx0 := Max(0, Math.Floor(Min(ax, bx)) - pad);
    sx1 := Min(AW - 1, Math.Ceil(Max(ax, bx)) + pad);
    sy0 := Max(0, Math.Floor(Min(ay, by)) - pad);
    sy1 := Min(AH - 1, Math.Ceil(Max(ay, by)) + pad);

    for py := sy0 to sy1 do
      for px := sx0 to sx1 do
      begin
        if len2 > 0 then
          t := EnsureRange(((px - ax) * dx + (py - ay) * dy) / len2, 0.0, 1.0)
        else
          t := 0;
        ddx := px - (ax + t * dx);
        ddy := py - (ay + t * dy);
        dist := Sqrt(ddx * ddx + ddy * ddy);

        if dist <= halfT - 0.5 then
          a := 1.0
        else if dist >= halfT + 0.5 then
          a := 0.0
        else
          a := halfT + 0.5 - dist;

        if a > cov[py * AW + px] then
        begin
          cov[py * AW + px] := a;
          col[py * AW + px] := AColor;
        end;
      end;
  end;

begin
  Result := nil;
  AX := 0; AY := 0; AW := 0; AH := 0;
  if (Length(AStrokes) = 0) or (AThickness < 1) then
    Exit;

  fMinX := Min(AStrokes[0].X1, AStrokes[0].X2);
  fMaxX := Max(AStrokes[0].X1, AStrokes[0].X2);
  fMinY := Min(AStrokes[0].Y1, AStrokes[0].Y2);
  fMaxY := Max(AStrokes[0].Y1, AStrokes[0].Y2);
  for i := 1 to High(AStrokes) do
  begin
    fMinX := Min(fMinX, Min(AStrokes[i].X1, AStrokes[i].X2));
    fMaxX := Max(fMaxX, Max(AStrokes[i].X1, AStrokes[i].X2));
    fMinY := Min(fMinY, Min(AStrokes[i].Y1, AStrokes[i].Y2));
    fMaxY := Max(fMaxY, Max(AStrokes[i].Y1, AStrokes[i].Y2));
  end;
  pad := AThickness div 2 + 2;
  AX := Math.Floor(fMinX) - pad;
  AY := Math.Floor(fMinY) - pad;
  AW := Math.Ceil(fMaxX) + pad - AX + 1;
  AH := Math.Ceil(fMaxY) + pad - AY + 1;
  if (AW < 1) or (AH < 1) then
    Exit;

  halfT := AThickness / 2.0;
  SetLength({%H-}cov, AW * AH); // fresh dynarrays: zero-initialized
  SetLength({%H-}col, AW * AH);

  for i := 0 to High(AStrokes) do
    RasterStroke(AStrokes[i].X1 - AX, AStrokes[i].Y1 - AY,
      AStrokes[i].X2 - AX, AStrokes[i].Y2 - AY, AStrokes[i].Color);

  Result := NewAlphaRaster(AW, AH);
  for y := 0 to AH - 1 do
    for x := 0 to AW - 1 do
    begin
      c := ColorToRGB(col[y * AW + x]);
      PutAlphaPixel(Result, x, y, Red(c), Green(c), Blue(c), cov[y * AW + x]);
    end;
end;

procedure DrawSmoothStrokes(ACanvas: TCanvas;
  const AStrokes: array of TSmoothStroke; AThickness: integer);
var
  intf: TLazIntfImage;
  img: TAlphaImage;
  x, y, w, h: integer;
begin
  intf := RasterizeStrokes(AStrokes, AThickness, x, y, w, h);
  if intf = nil then
    Exit;
  try
    img := AlphaImageFromIntf(intf, w, h);
    try
      BlitAlphaImage(ACanvas, img, w, h, x, y, w, h);
    finally
      img.Free;
    end;
  finally
    intf.Free;
  end;
end;

procedure DrawSmoothPolyline(ACanvas: TCanvas; const APts: array of TPoint;
  const AColors: array of TColor; AThickness: integer;
  ACache: boolean = true);
var
  intf: TLazIntfImage;
  img: TAlphaImage;
  strokes: array of TSmoothStroke;
  i, n, minX, minY, x, y, w, h: integer;
  mx, my: double;
  key: string;
begin
  if (Length(APts) < 2) or (Length(AColors) <> Length(APts))
    or (AThickness < 1) then
    Exit;

  if ACache then
  begin
    // The rendered image depends only on the trace's shape, colors and
    // stroke — position lands at blit time — so a repaint with an unmoved
    // trace (hover, overlapping window) reuses the finished raster. The key
    // is bbox-relative, and RasterizeStrokes pads the bbox by the same
    // amount every time, so the cached image lands at the same offset.
    minX := APts[0].X;
    minY := APts[0].Y;
    for i := 1 to High(APts) do
    begin
      minX := Min(minX, APts[i].X);
      minY := Min(minY, APts[i].Y);
    end;
    key := Format('pl|%d', [AThickness]);
    for i := 0 to High(APts) do
      key := key + Format('|%d;%d;%d', [APts[i].X - minX, APts[i].Y - minY,
        integer(AColors[i])]);
    if (PolylineImage <> nil) and (key = PolylineKey) then
    begin
      Dec(minX, AThickness div 2 + 2);
      Dec(minY, AThickness div 2 + 2);
      BlitAlphaImage(ACanvas, PolylineImage, PolylineW, PolylineH,
        minX, minY, PolylineW, PolylineH);
      Exit;
    end;
  end;

  // The color break sits halfway between the points: two strokes per
  // segment, each running from its point to the midpoint in that point's
  // color, so every point owns the piece of line on either side of it.
  SetLength({%H-}strokes, 2 * (Length(APts) - 1));
  n := 0;
  for i := 0 to High(APts) - 1 do
  begin
    mx := (APts[i].X + APts[i + 1].X) / 2.0;
    my := (APts[i].Y + APts[i + 1].Y) / 2.0;
    strokes[n].X1 := APts[i].X;
    strokes[n].Y1 := APts[i].Y;
    strokes[n].X2 := mx;
    strokes[n].Y2 := my;
    strokes[n].Color := AColors[i];
    Inc(n);
    strokes[n].X1 := mx;
    strokes[n].Y1 := my;
    strokes[n].X2 := APts[i + 1].X;
    strokes[n].Y2 := APts[i + 1].Y;
    strokes[n].Color := AColors[i + 1];
    Inc(n);
  end;

  intf := RasterizeStrokes(strokes, AThickness, x, y, w, h);
  if intf = nil then
    Exit;
  try
    img := AlphaImageFromIntf(intf, w, h);
  finally
    intf.Free;
  end;
  if ACache then
  begin
    FreeAndNil(PolylineImage);
    PolylineImage := img;
    PolylineKey := key;
    PolylineW := w;
    PolylineH := h;
    BlitAlphaImage(ACanvas, img, w, h, x, y, w, h);
  end
  else
    try
      BlitAlphaImage(ACanvas, img, w, h, x, y, w, h);
    finally
      img.Free;
    end;
end;

procedure DrawSmoothDashedPolyline(ACanvas: TCanvas;
  const APts: array of TPoint; AColor: TColor; AThickness, ADashPx,
  AGapPx: integer);
const
  // The phase is accumulated from float subtractions, so it can land a hair
  // short of the dash or gap length; without the tolerance that residue
  // would advance pos by less than its own ulp and the walk never ends.
  DASH_EPS = 1e-6;
var
  strokes: array of TSmoothStroke;
  i, n: integer;
  ax, ay, bx, by, dx, dy, len, pos, dashEnd, phase: double;
  inking: boolean;

  procedure AddStroke(x1, y1, x2, y2: double);
  begin
    if n >= Length(strokes) then
      SetLength(strokes, Length(strokes) * 2 + 8);
    strokes[n].X1 := x1;
    strokes[n].Y1 := y1;
    strokes[n].X2 := x2;
    strokes[n].Y2 := y2;
    strokes[n].Color := AColor;
    Inc(n);
  end;

begin
  if (Length(APts) < 2) or (AThickness < 1) or (ADashPx < 1)
    or (AGapPx < 0) then
    Exit;

  n := 0;
  SetLength({%H-}strokes, 0);
  // Walk the polyline with a running phase so the dash pattern continues
  // across the joints instead of restarting at every point. `inking` says
  // whether the phase is inside a dash; `phase` is the distance already
  // spent inside the current dash or gap.
  inking := true;
  phase := 0;
  for i := 0 to High(APts) - 1 do
  begin
    ax := APts[i].X;
    ay := APts[i].Y;
    bx := APts[i + 1].X;
    by := APts[i + 1].Y;
    dx := bx - ax;
    dy := by - ay;
    len := Sqrt(dx * dx + dy * dy);
    if len <= 0 then
      Continue;
    dx := dx / len;
    dy := dy / len;
    pos := 0;
    while pos < len do
    begin
      if inking then
      begin
        dashEnd := Min(len, pos + (ADashPx - phase));
        if dashEnd > pos then
          AddStroke(ax + dx * pos, ay + dy * pos, ax + dx * dashEnd,
            ay + dy * dashEnd);
        phase := phase + (dashEnd - pos);
        pos := dashEnd;
        if phase >= ADashPx - DASH_EPS then
        begin
          inking := false;
          phase := 0;
        end;
      end
      else
      begin
        dashEnd := Min(len, pos + (AGapPx - phase));
        phase := phase + (dashEnd - pos);
        pos := dashEnd;
        if phase >= AGapPx - DASH_EPS then
        begin
          inking := true;
          phase := 0;
        end;
      end;
    end;
  end;
  SetLength(strokes, n);
  if n > 0 then
    DrawSmoothStrokes(ACanvas, strokes, AThickness);
end;

// ---------------------------------------------------------------------------
// Horizontal alpha bands
// ---------------------------------------------------------------------------

procedure DrawRangeBands(ACanvas: TCanvas; AWidth, AHeight: integer;
  const Bands: array of TRangeBand; AGradientPx: integer;
  ADestX: integer; ADestY: integer);

  function EdgeCoverage(y, top, bottom, grad: integer): double;
  begin
    if (y < top) or (y > bottom) then
    begin
      Result := 0;
      Exit;
    end;
    if grad <= 0 then
    begin
      Result := 1;
      Exit;
    end;
    if y - top < bottom - y then
      Result := (y - top + 0.5) / grad
    else
      Result := (bottom - y + 0.5) / grad;
    if Result > 1 then Result := 1
    else if Result < 0 then Result := 0;
  end;

var
  intf: TLazIntfImage;
  img: TAlphaImage;
  y, i: integer;
  br, bg, bb: byte;
  dstR, dstG, dstB, dstA: double;
  srcA, invA: double;
begin
  if (AWidth <= 0) or (AHeight <= 0) or (Length(Bands) = 0) then
    Exit;

  intf := NewAlphaRaster(1, AHeight);
  try
    for y := 0 to AHeight - 1 do
    begin
      dstR := 0; dstG := 0; dstB := 0; dstA := 0;
      for i := Low(Bands) to High(Bands) do
      begin
        srcA := (Bands[i].Alpha / 255.0) *
                EdgeCoverage(y, Bands[i].Top, Bands[i].Bottom, AGradientPx);
        if srcA <= 0 then
          Continue;
        br := Red(ColorToRGB(Bands[i].Color));
        bg := Green(ColorToRGB(Bands[i].Color));
        bb := Blue(ColorToRGB(Bands[i].Color));
        invA := 1.0 - srcA;
        dstR := br * srcA + dstR * invA;
        dstG := bg * srcA + dstG * invA;
        dstB := bb * srcA + dstB * invA;
        dstA := srcA + dstA * invA;
      end;

      // dstR/dstG/dstB are accumulated in premultiplied form already
      // (Porter-Duff: dstR = br*srcA + dstR*invA -- the srcA factor is baked
      // in), which is exactly what PutPremultipliedPixel takes.
      PutPremultipliedPixel(intf, 0, y, dstR, dstG, dstB, dstA);
    end;

    // One column stretched across the full width -- the bands are uniform
    // horizontally, so there is nothing to gain from rasterizing AWidth
    // identical copies.
    img := AlphaImageFromIntf(intf, 1, AHeight);
    try
      BlitAlphaImage(ACanvas, img, 1, AHeight, ADestX, ADestY, AWidth, AHeight);
    finally
      img.Free;
    end;
  finally
    intf.Free;
  end;
end;

finalization
  FreeAndNil(ShapeImageCache);
  FreeAndNil(PolylineImage);

end.
