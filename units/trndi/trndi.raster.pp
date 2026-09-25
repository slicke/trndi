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
 * - 2026-09-25: SHAPE_IMAGE_CACHE_MAX raised to 64: the dot halos take the
 *   window gradient's tone at their row, so a paint pass needs more
 *   distinct shape images than the flat backdrop did.
 * - 2026-09-20: Rasters are rendered in device pixels. CanvasDeviceScale
 *   reads the Cocoa backing scale from the drawing context and every shape
 *   rasterizes at size times scale, then stretches into its canvas
 *   rectangle. Darwin now hands Cocoa an R8G8B8A8 raster it premultiplies
 *   itself, so the premultiplied blit is Windows-only. The Cocoa probe is
 *   gated on RASTER_COCOA (Darwin outside the -dTEST mock build).
 * - 2026-09-20: CanvasDeviceScale also reads the Qt6 device pixel ratio
 *   (QPainter's paint device), so rasters land 1:1 on a scaled Wayland or
 *   X11 desktop instead of being upsampled from logical pixels. The scaled
 *   blit goes straight to the QPainter there, because LCL's StretchDraw
 *   rescales the source to logical size first. Gated on RASTER_QT (LCLQt6
 *   outside the -dTEST mock build).
 * - 2026-09-20: Added DrawSmoothConvexPolygon, a signed-distance rasterizer
 *   for filled convex shapes with an optional mitred outline, so the warning
 *   triangle and the expand chevrons can leave Canvas.Polygon.
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

  Rasters are laid down in device pixels. On Cocoa the canvas counts points
  while a Retina backing store holds two pixels per point, so a raster drawn
  at point size would be upsampled and blurred next to the crisp text.
  Qt6 does the same on a scaled desktop: widgets are laid out in logical
  pixels and the painter's device pixel ratio says how many device pixels each
  one covers. CanvasDeviceScale reads the factor from the drawing context,
  every shape rasterizes at size times scale and is stretched back into its
  canvas rectangle, which lands its pixels 1:1 on the device. Elsewhere
  (Win32 draws in physical pixels already; GTK2 has no scaling) the scale is
  1 and the rasters are blitted as they are.

  Everything is main-thread only: the rendered-shape caches are plain globals
  used from paint handlers and freed in this unit's finalization.
}

{$mode objfpc}{$H+}

// The Cocoa backing-scale probe needs the LCL Cocoa widgetset unit, which the
// test build (tests/mock, -dTEST) does not carry; there the scale is 1.
{$if defined(DARWIN) and not defined(TEST)}
{$define RASTER_COCOA}
{$modeswitch objectivec1}
{$endif}

// The Qt6 probe needs the LCL Qt6 binding and device-context units, which the
// mock build does not carry either.
{$if defined(LCLQt6) and not defined(TEST)}
{$define RASTER_QT}
{$endif}

interface

uses
Classes, SysUtils, Math, Graphics, GraphType, IntfGraphics, FPImage,
{$ifdef Windows}LCLType,{$endif} // HDC for the msimg32 AlphaBlend import
{$ifdef RASTER_COCOA}MacOSAll, CocoaGDIObjects,{$endif} // context transform + TCocoaContext for CanvasDeviceScale
{$ifdef RASTER_QT}Types, qt6, qtobjects,{$endif} // PRect, QPainter + TQtDeviceContext for CanvasDeviceScale and the Qt blit
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

      Cocoa takes straight alpha, but only for the layouts it premultiplies
      itself: TCocoaBitmap.PreMultiplyAlpha runs for cbtARGB and cbtRGBA and
      bails out for everything else, while CreateHandle hands every layout to
      CoreGraphics without NSAlphaNonpremultipliedBitmapFormat. The
      B8G8R8A8 description the other widgetsets get maps to cbtBGRA there and
      composited as C + dst*(1-a), blowing every partially transparent pixel
      out toward the background (it erased the thin prediction × against the
      brighter out-of-range windows). NewAlphaRaster therefore uses R8G8B8A8
      on Darwin, which maps to cbtRGBA and takes the premultiplying path.
      Should that ever misbehave on a Mac, the old workaround is DARWIN in
      this constant plus B8G8R8A8 in NewAlphaRaster.

      GTK (GdkPixbuf) and Qt (QImage::Format_ARGB32) both take straight alpha. }
  {$ifdef Windows}
  ALPHA_BLIT_PREMULTIPLIED = true;
  {$else}
  ALPHA_BLIT_PREMULTIPLIED = false;
  {$endif}

{** Allocate a straight-alpha RGBA raster to render into. }
function NewAlphaRaster(AWidth, AHeight: integer): TLazIntfImage;

{** Device pixels per canvas unit on ACanvas: the Retina backing scale on
    Cocoa, read from the drawing context's user-to-device transform; 1 on
    every other widgetset and for any canvas without a live handle. The
    rasterizers render at this scale so their pixels land 1:1 on the device. }
function CanvasDeviceScale(ACanvas: TCanvas): double;

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

{** Antialiased filled convex polygon through APts, optionally outlined by an
    AOutlineWidth-pixel stroke in AOutlineColor centred on the edge with
    mitred corners, the way a GDI pen draws it. Vertices sit on pixel edges,
    so an axis-aligned rectangle covers exactly the pixels FillRect would.
    Concave input is not detected: the coverage is the distance to the
    nearest edge line, which only describes a convex outline. Uncached. }
procedure DrawSmoothConvexPolygon(ACanvas: TCanvas;
  const APts: array of TPoint; AFillColor: TColor;
  AOutlineColor: TColor = clNone; AOutlineWidth: integer = 0);

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
  // layout pass produces a handful of sizes and range colors, plus one halo
  // tone per gradient step the dots span; the cap only stops unbounded
  // growth across many window sizes. Clearing wholesale is fine — entries
  // are cheap to re-render once.
  SHAPE_IMAGE_CACHE_MAX = 64;

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
  PolylineW: integer = 0;      // raster size (device pixels)
  PolylineH: integer = 0;
  PolylineDestW: integer = 0;  // canvas size it stretches into
  PolylineDestH: integer = 0;

//------------------------------------------------------------------------------
// Raster primitives
//------------------------------------------------------------------------------

function NewAlphaRaster(AWidth, AHeight: integer): TLazIntfImage;
var
  rawDesc: TRawImageDescription;
begin
  Result := TLazIntfImage.Create(0, 0);
  // Darwin gets the layout Cocoa classes as cbtRGBA and premultiplies itself;
  // B8G8R8A8 would land as cbtBGRA and skip that (see ALPHA_BLIT_PREMULTIPLIED).
  {$ifdef DARWIN}
  rawDesc.Init_BPP32_R8G8B8A8_BIO_TTB(AWidth, AHeight);
  {$else}
  rawDesc.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
  {$endif}
  Result.DataDescription := rawDesc;
  Result.CreateData;
end;

// The Cocoa context's user-to-device transform carries the backing scale on
// its diagonal (the y term is negative in a flipped view, hence Abs). A
// bitmap context answers 1, so a shape rendered into an offscreen TBitmap is
// not scaled — that bitmap is blitted in points anyway.
function CanvasDeviceScale(ACanvas: TCanvas): double;
{$if defined(RASTER_COCOA)}
var
  cocoaCtx: TCocoaContext;
  t: CGAffineTransform;
begin
  Result := 1.0;
  if (ACanvas = nil) or not ACanvas.HandleAllocated then
    Exit;
  cocoaCtx := TCocoaContext(ACanvas.Handle);
  if (cocoaCtx = nil) or (cocoaCtx.ctx = nil) then
    Exit;
  t := CGContextGetUserSpaceToDeviceSpaceTransform(cocoaCtx.CGContext);
  Result := Abs(t.a);
  // Nothing plausible outside the real backing scales; a degenerate transform
  // is no reason to rasterize at a silly size.
  if (Result < 0.5) or (Result > 8.0) then
    Result := 1.0;
end;
{$elseif defined(RASTER_QT)}
// The Qt device context wraps a QPainter (its Widget field); the painter's
// paint device carries the ratio. A widget being painted on a scaled screen
// answers 2 (or 1.5 with fractional scaling); an offscreen QPixmap answers 1,
// so a shape rendered into a TBitmap is not scaled, matching Cocoa above.
var
  qtCtx: TQtDeviceContext;
  device: QPaintDeviceH;
begin
  Result := 1.0;
  if (ACanvas = nil) or not ACanvas.HandleAllocated then
    Exit;
  qtCtx := TQtDeviceContext(ACanvas.Handle);
  if (qtCtx = nil) or (qtCtx.Widget = nil) then
    Exit;
  device := QPainter_device(qtCtx.Widget);
  if device = nil then
    Exit;
  Result := QPaintDevice_devicePixelRatioF(device);
  if (Result < 0.5) or (Result > 8.0) then
    Result := 1.0;
end;
{$else}
begin
  Result := 1.0;
  if ACanvas = nil then
    Exit;
end;
{$endif}

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
{$if defined(Windows)}
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
{$elseif defined(RASTER_QT)}
// LCL's StretchDraw on Qt rescales the source pixmap to the logical target
// size (QPixmap_scaled in StretchMaskBlt) before Qt applies the device pixel
// ratio, so a device-pixel raster is thrown away and blown back up blocky.
// Hand the painter the raster and the logical target rectangle directly: the
// painter's device transform maps the target onto the same number of device
// pixels the raster holds, so the pixels land 1:1. Smooth sampling is an
// identity there and only matters for fractional ratios.
var
  qtCtx, imgCtx: TQtDeviceContext;
  target, source: TRect;
  hints: QPainterRenderHints;
begin
  if (ADestW = ASrcW) and (ADestH = ASrcH) then
  begin
    ACanvas.Draw(ADestX, ADestY, AImage);
    Exit;
  end;
  qtCtx := TQtDeviceContext(ACanvas.Handle);
  imgCtx := TQtDeviceContext(AImage.Canvas.Handle);
  if (qtCtx = nil) or (qtCtx.Widget = nil) or (imgCtx = nil) or
    (imgCtx.vImage = nil) or (imgCtx.vImage.Handle = nil) then
  begin
    ACanvas.StretchDraw(
      Classes.Rect(ADestX, ADestY, ADestX + ADestW, ADestY + ADestH), AImage);
    Exit;
  end;
  target := Classes.Rect(ADestX, ADestY, ADestX + ADestW, ADestY + ADestH);
  source := Classes.Rect(0, 0, ASrcW, ASrcH);
  hints := QPainter_renderHints(qtCtx.Widget);
  QPainter_setRenderHint(qtCtx.Widget, QPainterSmoothPixmapTransform, True);
  QPainter_drawImage(qtCtx.Widget, PRect(@target), imgCtx.vImage.Handle,
    PRect(@source));
  QPainter_setRenderHint(qtCtx.Widget, QPainterSmoothPixmapTransform,
    (hints and QPainterSmoothPixmapTransform) <> 0);
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

// Composite a finished square shape image over ACanvas at the given offset:
// ARasterSize pixels of image into an ADestSize box of canvas units — the
// same number at scale 1, the device-pixel count on Retina.
procedure BlitShapeImage(ACanvas: TCanvas; AImage: TAlphaImage;
  ARasterSize, ADestSize, AOffsetX, AOffsetY: integer); inline;
begin
  BlitAlphaImage(ACanvas, AImage, ARasterSize, ARasterSize, AOffsetX,
    AOffsetY, ADestSize, ADestSize);
end;

// Try to satisfy a paint entirely from the rendered-shape cache.
function TryBlitCachedShape(ACanvas: TCanvas; const AKey: string;
  ARasterSize, ADestSize, AOffsetX, AOffsetY: integer): boolean;
var
  img: TAlphaImage;
begin
  img := GetCachedShapeImage(AKey);
  Result := img <> nil;
  if Result then
    BlitShapeImage(ACanvas, img, ARasterSize, ADestSize, AOffsetX, AOffsetY);
end;

// Wrap the finished raster in the platform image, cache it under AKey and
// composite it onto the destination canvas. The caller still owns AIntf.
procedure FinishShapeRender(ACanvas: TCanvas; const AKey: string;
  AIntf: TLazIntfImage; ARasterSize, ADestSize, AOffsetX, AOffsetY: integer);
var
  img: TAlphaImage;
begin
  img := AlphaImageFromIntf(AIntf, ARasterSize, ARasterSize);
  StoreCachedShapeImage(AKey, img);
  BlitShapeImage(ACanvas, img, ARasterSize, ADestSize, AOffsetX, AOffsetY);
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
  x, y, px: integer;
  scale, cx, cy, dist1, dist2, dist, halfT, aPix, invSqrt2: double;
  fr, fg, fb: byte;
  key: string;
begin
  if (ASize < 2) or (AThickness < 1) or (AAlpha <= 0) then
    Exit;
  if AAlpha > 1.0 then
    AAlpha := 1.0;

  // Rendered in device pixels (CanvasDeviceScale) and stretched back into
  // the ASize box at blit time; px = ASize wherever the scale is 1.
  scale := CanvasDeviceScale(ACanvas);
  px := Max(2, Round(ASize * scale));

  // The rendered image depends only on these parameters (offsets are applied
  // at blit time), so reuse the finished image across paints. Alpha is
  // quantized to percent so tiny confidence drift can't churn the cache.
  key := Format('x|%d|%d|%d|%d|%d', [ASize, px, Integer(AColor), AThickness,
    Round(AAlpha * 100)]);
  if TryBlitCachedShape(ACanvas, key, px, ASize, AOffsetX, AOffsetY) then
    Exit;

  fr := Red(ColorToRGB(AColor));
  fg := Green(ColorToRGB(AColor));
  fb := Blue(ColorToRGB(AColor));

  cx := (px - 1) / 2.0;
  cy := (px - 1) / 2.0;
  halfT := AThickness * scale / 2.0;
  invSqrt2 := 1.0 / Sqrt(2);

  intf := NewAlphaRaster(px, px);
  try
    for y := 0 to px - 1 do
      for x := 0 to px - 1 do
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

    FinishShapeRender(ACanvas, key, intf, px, ASize, AOffsetX, AOffsetY);
  finally
    intf.Free;
  end;
end;

procedure DrawSmoothCircle(ACanvas: TCanvas; ASize: integer;
  ADotColor, ARingColor: TColor; ARingWidth: integer;
  AOffsetX: integer = 0; AOffsetY: integer = 0);
var
  intf: TLazIntfImage;
  x, y, px: integer;
  scale, cx, cy, r, dist: double;
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

  // Rendered in device pixels (CanvasDeviceScale) and stretched back into
  // the ASize box at blit time; px = ASize wherever the scale is 1.
  scale := CanvasDeviceScale(ACanvas);
  px := Max(2, Round(ASize * scale));

  // The rendered image depends only on these parameters (offsets are applied
  // at blit time), so reuse the finished image across paints.
  key := Format('c|%d|%d|%d|%d|%d', [ASize, px, Integer(ADotColor),
    Integer(ARingColor), ARingWidth]);
  if TryBlitCachedShape(ACanvas, key, px, ASize, AOffsetX, AOffsetY) then
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

  cx := (px - 1) / 2.0;
  cy := (px - 1) / 2.0;
  // Half-pixel inset keeps the analytical AA edge inside the bounding box.
  r := px / 2.0 - 0.5;
  ringOuter := r;
  ringInner := r - ARingWidth * scale;

  intf := NewAlphaRaster(px, px);
  try
    for y := 0 to px - 1 do
    begin
      for x := 0 to px - 1 do
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

    FinishShapeRender(ACanvas, key, intf, px, ASize, AOffsetX, AOffsetY);
  finally
    intf.Free;
  end;
end;

//------------------------------------------------------------------------------
// Strokes
//------------------------------------------------------------------------------

// Rasterize a stroke list into a fresh straight-alpha image covering the
// strokes' padded bounding box, whose canvas position comes back in
// (AX, AY) and canvas size in (AW, AH); the raster itself is that box at
// AScale device pixels per canvas unit, (ARW, ARH) — the same numbers at
// scale 1. The box is fixed in canvas units first so the raster's origin
// sits on a whole canvas unit and the stretch back lands pixel-exact. Nil
// when there is nothing to draw. Every stroke is a capsule (round caps fall
// out of the point-to-segment distance), and overlapping strokes keep the
// strongest coverage in a shared buffer so joints and crossings don't
// double-blend. Only each stroke's own padded bbox is visited, so the cost
// tracks the ink rather than the raster's area.
function RasterizeStrokes(const AStrokes: array of TSmoothStroke;
  AThickness: integer; AScale: double;
  out AX, AY, AW, AH, ARW, ARH: integer): TLazIntfImage;
var
  cov: array of single;
  col: array of TColor;
  i, x, y, pad, padPx: integer;
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
    sx0 := Max(0, Math.Floor(Min(ax, bx)) - padPx);
    sx1 := Min(ARW - 1, Math.Ceil(Max(ax, bx)) + padPx);
    sy0 := Max(0, Math.Floor(Min(ay, by)) - padPx);
    sy1 := Min(ARH - 1, Math.Ceil(Max(ay, by)) + padPx);

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

        if a > cov[py * ARW + px] then
        begin
          cov[py * ARW + px] := a;
          col[py * ARW + px] := AColor;
        end;
      end;
  end;

begin
  Result := nil;
  AX := 0; AY := 0; AW := 0; AH := 0; ARW := 0; ARH := 0;
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

  ARW := Max(1, Round(AW * AScale));
  ARH := Max(1, Round(AH * AScale));
  padPx := Math.Ceil(pad * AScale);
  halfT := AThickness * AScale / 2.0;
  SetLength({%H-}cov, ARW * ARH); // fresh dynarrays: zero-initialized
  SetLength({%H-}col, ARW * ARH);

  for i := 0 to High(AStrokes) do
    RasterStroke((AStrokes[i].X1 - AX) * AScale, (AStrokes[i].Y1 - AY) * AScale,
      (AStrokes[i].X2 - AX) * AScale, (AStrokes[i].Y2 - AY) * AScale,
      AStrokes[i].Color);

  Result := NewAlphaRaster(ARW, ARH);
  for y := 0 to ARH - 1 do
    for x := 0 to ARW - 1 do
    begin
      c := ColorToRGB(col[y * ARW + x]);
      PutAlphaPixel(Result, x, y, Red(c), Green(c), Blue(c), cov[y * ARW + x]);
    end;
end;

procedure DrawSmoothStrokes(ACanvas: TCanvas;
  const AStrokes: array of TSmoothStroke; AThickness: integer);
var
  intf: TLazIntfImage;
  img: TAlphaImage;
  x, y, w, h, rw, rh: integer;
begin
  intf := RasterizeStrokes(AStrokes, AThickness, CanvasDeviceScale(ACanvas),
    x, y, w, h, rw, rh);
  if intf = nil then
    Exit;
  try
    img := AlphaImageFromIntf(intf, rw, rh);
    try
      BlitAlphaImage(ACanvas, img, rw, rh, x, y, w, h);
    finally
      img.Free;
    end;
  finally
    intf.Free;
  end;
end;

//------------------------------------------------------------------------------
// Convex polygon
//------------------------------------------------------------------------------

// The polygon is described by its signed distance field: for a convex shape
// the inward distance to the nearest edge line is exact inside, and outside
// it reproduces the mitred corner a pen with a square join would draw. Fill
// coverage is that distance clamped over one pixel, the outline is a band of
// half the pen width either side of zero, and the two composite as outline
// over fill. Samples are taken at pixel centres in a raster laid down at
// device scale over the polygon's padded bounding box, the same way the
// stroke rasterizer places its box.
procedure DrawSmoothConvexPolygon(ACanvas: TCanvas;
  const APts: array of TPoint; AFillColor: TColor;
  AOutlineColor: TColor; AOutlineWidth: integer);
var
  intf: TLazIntfImage;
  img: TAlphaImage;
  n, i, j, x, y, pad, ax, ay, aw, ah, rw, rh, minX, minY, maxX, maxY: integer;
  scale, halfW, sx, sy, ex, ey, len, d, dEdge, orient: double;
  covFill, covLine, aOut, invLine: double;
  vx, vy, nx, ny: array of double;
  fr, fg, fb, lr, lg, lb: byte;
  fc, lc: TColor;
  hasLine: boolean;
begin
  n := Length(APts);
  if n < 3 then
    Exit;
  hasLine := (AOutlineColor <> clNone) and (AOutlineWidth > 0);
  if hasLine then
    halfW := AOutlineWidth / 2.0
  else
    halfW := 0.0;

  minX := APts[0].X; maxX := APts[0].X;
  minY := APts[0].Y; maxY := APts[0].Y;
  for i := 1 to n - 1 do
  begin
    minX := Min(minX, APts[i].X); maxX := Max(maxX, APts[i].X);
    minY := Min(minY, APts[i].Y); maxY := Max(maxY, APts[i].Y);
  end;
  // A mitre reaches halfW / sin(half the corner angle) past the vertex; two
  // pen widths holds every corner down to 60 degrees, which is the sharpest
  // shape drawn through here.
  pad := Math.Ceil(2 * halfW) + 2;
  ax := minX - pad;
  ay := minY - pad;
  aw := maxX + pad - ax;
  ah := maxY + pad - ay;
  if (aw < 1) or (ah < 1) then
    Exit;

  scale := CanvasDeviceScale(ACanvas);
  rw := Max(1, Round(aw * scale));
  rh := Max(1, Round(ah * scale));
  halfW := halfW * scale;

  // Edges in raster space with their unit normals; the sign that makes the
  // normals point inward comes from the polygon's winding (signed area).
  SetLength({%H-}vx, n); SetLength({%H-}vy, n);
  SetLength({%H-}nx, n); SetLength({%H-}ny, n);
  orient := 0;
  for i := 0 to n - 1 do
  begin
    vx[i] := (APts[i].X - ax) * scale;
    vy[i] := (APts[i].Y - ay) * scale;
  end;
  for i := 0 to n - 1 do
  begin
    j := (i + 1) mod n;
    orient := orient + vx[i] * vy[j] - vx[j] * vy[i];
  end;
  if orient < 0 then
    orient := -1.0
  else
    orient := 1.0;
  for i := 0 to n - 1 do
  begin
    j := (i + 1) mod n;
    ex := vx[j] - vx[i];
    ey := vy[j] - vy[i];
    len := Sqrt(ex * ex + ey * ey);
    if len < 1e-9 then
    begin
      nx[i] := 0; ny[i] := 0;
    end
    else
    begin
      // Left normal of the edge, flipped by the winding so it points inward.
      nx[i] := -ey / len * orient;
      ny[i] := ex / len * orient;
    end;
  end;

  fc := ColorToRGB(AFillColor);
  fr := Red(fc); fg := Green(fc); fb := Blue(fc);
  lr := 0; lg := 0; lb := 0;
  if hasLine then
  begin
    lc := ColorToRGB(AOutlineColor);
    lr := Red(lc); lg := Green(lc); lb := Blue(lc);
  end;

  intf := NewAlphaRaster(rw, rh);
  try
    for y := 0 to rh - 1 do
      for x := 0 to rw - 1 do
      begin
        sx := x + 0.5;
        sy := y + 0.5;
        d := Infinity;
        for i := 0 to n - 1 do
        begin
          if (nx[i] = 0) and (ny[i] = 0) then
            Continue;
          dEdge := (sx - vx[i]) * nx[i] + (sy - vy[i]) * ny[i];
          if dEdge < d then
            d := dEdge;
        end;

        covFill := EnsureRange(d + 0.5, 0.0, 1.0);
        if hasLine then
          covLine := EnsureRange(halfW + 0.5 - Abs(d), 0.0, 1.0)
        else
          covLine := 0.0;
        // Every pixel is written: a fresh raster carries whatever the
        // allocator left behind, so untouched pixels would blit opaque.
        invLine := 1.0 - covLine;
        aOut := covLine + covFill * invLine;
        PutPremultipliedPixel(intf, x, y,
          lr * covLine + fr * covFill * invLine,
          lg * covLine + fg * covFill * invLine,
          lb * covLine + fb * covFill * invLine, aOut);
      end;

    img := AlphaImageFromIntf(intf, rw, rh);
    try
      BlitAlphaImage(ACanvas, img, rw, rh, ax, ay, aw, ah);
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
  i, n, minX, minY, x, y, w, h, rw, rh: integer;
  mx, my, scale: double;
  key: string;
begin
  if (Length(APts) < 2) or (Length(AColors) <> Length(APts))
    or (AThickness < 1) then
    Exit;

  scale := CanvasDeviceScale(ACanvas);
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
    key := Format('pl|%d|%d', [AThickness, Round(scale * 100)]);
    for i := 0 to High(APts) do
      key := key + Format('|%d;%d;%d', [APts[i].X - minX, APts[i].Y - minY,
        integer(AColors[i])]);
    if (PolylineImage <> nil) and (key = PolylineKey) then
    begin
      Dec(minX, AThickness div 2 + 2);
      Dec(minY, AThickness div 2 + 2);
      BlitAlphaImage(ACanvas, PolylineImage, PolylineW, PolylineH,
        minX, minY, PolylineDestW, PolylineDestH);
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

  intf := RasterizeStrokes(strokes, AThickness, scale, x, y, w, h, rw, rh);
  if intf = nil then
    Exit;
  try
    img := AlphaImageFromIntf(intf, rw, rh);
  finally
    intf.Free;
  end;
  if ACache then
  begin
    FreeAndNil(PolylineImage);
    PolylineImage := img;
    PolylineKey := key;
    PolylineW := rw;
    PolylineH := rh;
    PolylineDestW := w;
    PolylineDestH := h;
    BlitAlphaImage(ACanvas, img, rw, rh, x, y, w, h);
  end
  else
    try
      BlitAlphaImage(ACanvas, img, rw, rh, x, y, w, h);
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
  y, i, rows, gradPx: integer;
  scale: double;
  br, bg, bb: byte;
  dstR, dstG, dstB, dstA: double;
  srcA, invA: double;
begin
  if (AWidth <= 0) or (AHeight <= 0) or (Length(Bands) = 0) then
    Exit;

  // The column is rendered in device rows (CanvasDeviceScale): Top..Bottom
  // are inclusive canvas rows, so a band covers rows Top*scale up to the row
  // before (Bottom+1)*scale, and the fade spans AGradientPx*scale rows.
  scale := CanvasDeviceScale(ACanvas);
  rows := Max(1, Round(AHeight * scale));
  gradPx := Round(AGradientPx * scale);

  intf := NewAlphaRaster(1, rows);
  try
    for y := 0 to rows - 1 do
    begin
      dstR := 0; dstG := 0; dstB := 0; dstA := 0;
      for i := Low(Bands) to High(Bands) do
      begin
        srcA := (Bands[i].Alpha / 255.0) *
                EdgeCoverage(y, Round(Bands[i].Top * scale),
                  Round((Bands[i].Bottom + 1) * scale) - 1, gradPx);
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
    img := AlphaImageFromIntf(intf, 1, rows);
    try
      BlitAlphaImage(ACanvas, img, 1, rows, ADestX, ADestY, AWidth, AHeight);
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
