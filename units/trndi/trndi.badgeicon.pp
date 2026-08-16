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
 *)
unit trndi.badgeicon;

{$mode ObjFPC}{$H+}

{!
  @abstract(Composes the application icon with a reading badge for the system tray.)

  Draws the same picture the Windows taskbar icon shows — the logo, a rounded
  value badge in the lower-right corner and, optionally, a trend arrow badge in
  the upper-left — but rasterizes it into a straight-alpha RGBA image with
  analytic antialiasing instead of GDI. Two reasons it does not simply reuse the
  widgetset canvas the way @code(trndi.native.win) does:

  @unorderedList(
    @item(LCL's @code(Ellipse)/@code(RoundRect) are strictly aliased, and a tray
      icon is small enough that jagged badge corners are the first thing a user
      notices.)
    @item(Canvas drawing never writes the alpha channel of a 32-bit bitmap, so a
      canvas-composed icon reaches the panel fully opaque — the black square
      behind the logo.)
  )

  Text and arrowheads still come off a canvas, because nothing else can shape
  glyphs, but only as a black/white coverage mask that is blended in afterwards.

  The base logo is downscaled once and cached (@link(TTrndiBadgeIcon.Render)
  is called on every reading), since the application icon is a 1024px asset and
  a tray icon is ~22px.
}

interface

uses
Classes, SysUtils, Graphics, GraphType, IntfGraphics, FPimage, Math;

const
  {** Edge length of the composed icon. Panels scale the icon to whatever the
      tray row is, so this only has to be comfortably above every plausible
      tray size — 128 covers 2x HiDPI panels without wasting real time. }
  BADGE_ICON_SIZE = 128;

type
  {** Straight-alpha BGRA raster, matching @code(Init_BPP32_B8G8R8A8_BIO_TTB).
      Rows are top-to-bottom, @code(Px) is @code(W * H) long. }
TArgbRaster = record
  W, H: integer;
  Px: array of cardinal;                 //< $AARRGGBB, straight (non-premultiplied)
end;

  {** Renderer for one tray icon, holding the downscaled logo between calls.

      Create it once per native-platform object and call @link(Render) whenever
      the reading changes; @link(SetBase) only has to run again if the
      application icon itself is replaced. }
TTrndiBadgeIcon = class
private
  FBase: TArgbRaster;                    //< Logo at BADGE_ICON_SIZE, or empty
  FSmall: TArgbRaster;                   //< Logo at the corner-badge size
  FSize: integer;
  FFontName: string;
public
  {** @param(ASize Edge length of the rendered icon; defaults to
          @link(BADGE_ICON_SIZE)) }
  constructor Create(ASize: integer = BADGE_ICON_SIZE);
    {** Take the logo to compose on. Downscales @param(AGraphic) with a box
        filter and keeps the result; safe to call with @nil, which leaves the
        badge to render on transparency. }
  procedure SetBase(AGraphic: TGraphic);
    {** @true once @link(SetBase) has stored a usable logo. }
  function HasBase: boolean;
    {** Compose the icon.
        @param(AValue Reading to print in the badge; empty renders the bare logo)
        @param(ATrend UTF-8 trend arrow for the upper-left badge, or empty)
        @param(ABadgeColor Fill of both badges)
        @param(ABadgeSizeRatio Value-badge edge relative to the icon edge)
        @returns(A new RGBA image the caller owns — PNG because that is the one
          LCL image type that carries an alpha channel across every widgetset) }
  function Render(const AValue, ATrend: string; ABadgeColor: TColor;
    ABadgeSizeRatio: double = 0.8): TPortableNetworkGraphic;
    {** Font used for the badge text. Empty means the widgetset default. }
  property FontName: string read FFontName write FFontName;
end;

implementation

const
  // Fraction of the icon edge taken by the upper-left trend badge, and by the
  // small logo that replaces it when there is no trend. Both match the Windows
  // painter so the two platforms read as the same icon.
  TREND_RATIO = 0.45;
  SMALL_LOGO_RATIO = 0.4;
  // Corner rounding and border thickness as fractions of a badge's own edge.
  // The Windows painter uses 6px at a 32px icon with a 1px pen; expressed as
  // ratios those carry over to any render size.
  CORNER_RATIO = 6 / 32;
  BORDER_RATIO = 1 / 32;
  // Glyph masks are drawn oversized and averaged down. Qt antialiases text on
  // its own, but the arrowhead polygons come out hard-edged, and one factor for
  // both keeps the mask helper simple.
  MASK_SUPERSAMPLE = 3;
  // Badge text starts at half the badge edge and shrinks until it fits with
  // this much room on either side.
  FONT_RATIO = 0.5;
  TEXT_PADDING_RATIO = 4 / 32;

  // UTF-8 byte sequences for the arrows in BG_TREND_ARROWS_UTF, spelled out so
  // the match never depends on this unit's source encoding.
  ARR_UP = #$E2#$86#$91;                 // U+2191
  ARR_UPRIGHT = #$E2#$86#$97;            // U+2197
  ARR_RIGHT = #$E2#$86#$92;              // U+2192
  ARR_DOWNRIGHT = #$E2#$86#$98;          // U+2198
  ARR_DOWN = #$E2#$86#$93;               // U+2193

{------------------------------------------------------------------------------
  Raster primitives
  -----------------
  Everything below works on TArgbRaster in straight alpha. "Coverage" is always
  0..1 and always means "how much of this pixel the shape occupies".
 ------------------------------------------------------------------------------}

// Allocate a fully transparent raster.
function NewRaster(AW, AH: integer): TArgbRaster;
begin
  Result.W := AW;
  Result.H := AH;
  SetLength(Result.Px, AW * AH);
  if Length(Result.Px) > 0 then
    FillChar(Result.Px[0], Length(Result.Px) * SizeOf(cardinal), 0);
end;

// Porter-Duff "over" of a solid colour at the given coverage. Straight alpha in,
// straight alpha out — the intermediate is premultiplied because that is the
// only form "over" composes in, and the result is divided back out.
procedure BlendPixel(var ARaster: TArgbRaster; X, Y: integer;
  AR, AG, AB: integer; ACoverage: double);
var
  idx: integer;
  dst: cardinal;
  da, dr, dg, db: double;
  oa: double;
begin
  if (ACoverage <= 0) or (X < 0) or (Y < 0) or (X >= ARaster.W) or (Y >= ARaster.H) then
    Exit;
  if ACoverage > 1 then
    ACoverage := 1;

  idx := Y * ARaster.W + X;
  dst := ARaster.Px[idx];
  da := ((dst shr 24) and $FF) / 255;
  dr := ((dst shr 16) and $FF);
  dg := ((dst shr 8) and $FF);
  db := (dst and $FF);

  oa := ACoverage + da * (1 - ACoverage);
  if oa <= 0 then
  begin
    ARaster.Px[idx] := 0;
    Exit;
  end;

  // Premultiplied "over", then unpremultiply by oa.
  dr := (AR * ACoverage + dr * da * (1 - ACoverage)) / oa;
  dg := (AG * ACoverage + dg * da * (1 - ACoverage)) / oa;
  db := (AB * ACoverage + db * da * (1 - ACoverage)) / oa;

  ARaster.Px[idx] :=
    (cardinal(EnsureRange(Round(oa * 255), 0, 255)) shl 24) or
    (cardinal(EnsureRange(Round(dr), 0, 255)) shl 16) or
    (cardinal(EnsureRange(Round(dg), 0, 255)) shl 8) or
    cardinal(EnsureRange(Round(db), 0, 255));
end;

// Composite one raster over another at an offset.
procedure BlendRaster(var ADest: TArgbRaster; const ASrc: TArgbRaster;
  AOffsetX, AOffsetY: integer);
var
  x, y: integer;
  p: cardinal;
begin
  for y := 0 to ASrc.H - 1 do
    for x := 0 to ASrc.W - 1 do
    begin
      p := ASrc.Px[y * ASrc.W + x];
      if (p shr 24) = 0 then
        Continue;
      BlendPixel(ADest, AOffsetX + x, AOffsetY + y,
        (p shr 16) and $FF, (p shr 8) and $FF, p and $FF,
        ((p shr 24) and $FF) / 255);
    end;
end;

// Signed distance from (APX, APY) to a rounded rectangle: negative inside,
// positive outside, in pixels. The usual box-SDF with the corner radius
// subtracted, which is exact for a rounded rect and cheap enough to run per
// pixel of a badge.
function RoundRectDistance(APX, APY, ALeft, ATop, ARight, ABottom,
  ARadius: double): double;
var
  cx, cy, hw, hh, qx, qy: double;
begin
  cx := (ALeft + ARight) / 2;
  cy := (ATop + ABottom) / 2;
  hw := (ARight - ALeft) / 2 - ARadius;
  hh := (ABottom - ATop) / 2 - ARadius;
  if hw < 0 then
    hw := 0;
  if hh < 0 then
    hh := 0;

  qx := Abs(APX - cx) - hw;
  qy := Abs(APY - cy) - hh;
  if qx < 0 then
    qx := 0;
  if qy < 0 then
    qy := 0;

  Result := Sqrt(qx * qx + qy * qy) - ARadius +
    Min(Max(Abs(APX - cx) - hw, Abs(APY - cy) - hh), 0);
end;

// Turn a signed distance into edge coverage. One pixel of linear ramp centred
// on the boundary — the standard SDF-to-alpha approximation, and visually
// indistinguishable from supersampling for shapes this size.
function CoverageFromDistance(ADistance: double): double;
begin
  Result := 0.5 - ADistance;
  if Result < 0 then
    Result := 0
  else if Result > 1 then
    Result := 1;
end;

// Paint a rounded rectangle with an outline. The border is the band between the
// outer edge and the same shape inset by ABorder, which keeps both edges
// antialiased instead of stroking a hairline the way a pen would.
procedure FillRoundRect(var ARaster: TArgbRaster;
  ALeft, ATop, ARight, ABottom, ARadius: double;
  AFill, ABorderColor: TColor; ABorder: double);
var
  x, y, x0, y0, x1, y1: integer;
  fr, fg, fb, br, bg, bb: integer;
  rgbFill, rgbBorder: longint;
  d, outer, inner: double;
begin
  rgbFill := ColorToRGB(AFill);
  fr := Red(rgbFill);
  fg := Green(rgbFill);
  fb := Blue(rgbFill);
  rgbBorder := ColorToRGB(ABorderColor);
  br := Red(rgbBorder);
  bg := Green(rgbBorder);
  bb := Blue(rgbBorder);

  x0 := Max(0, Floor(ALeft) - 1);
  y0 := Max(0, Floor(ATop) - 1);
  x1 := Min(ARaster.W - 1, Ceil(ARight) + 1);
  y1 := Min(ARaster.H - 1, Ceil(ABottom) + 1);

  for y := y0 to y1 do
    for x := x0 to x1 do
    begin
      d := RoundRectDistance(x + 0.5, y + 0.5, ALeft, ATop, ARight, ABottom, ARadius);
      outer := CoverageFromDistance(d);
      if outer <= 0 then
        Continue;
      // Fill first, then lay the border band on top of it, so the two never
      // leave a seam of background between them.
      inner := CoverageFromDistance(d + ABorder);
      if inner > 0 then
        BlendPixel(ARaster, x, y, fr, fg, fb, inner);
      if ABorder > 0 then
        BlendPixel(ARaster, x, y, br, bg, bb, outer - inner);
    end;
end;

{------------------------------------------------------------------------------
  Glyph masks
  -----------
  A canvas is the only thing that can shape text, so shapes that need one are
  drawn white-on-black at MASK_SUPERSAMPLE times the final size and averaged
  down into coverage. That both antialiases the aliased polygons and preserves
  the widgetset's own text antialiasing.
 ------------------------------------------------------------------------------}

type
  // Coverage map, 0..1 per pixel, ASize x ASize.
TCoverageMask = array of double;

// Average an oversized black/white bitmap down into coverage.
function MaskFromBitmap(ABitmap: TBitmap; ASize: integer): TCoverageMask;
var
  intf: TLazIntfImage;
  x, y, sx, sy: integer;
  acc: double;
  c: TFPColor;
begin
  Result := nil;
  SetLength(Result, ASize * ASize);
  intf := ABitmap.CreateIntfImage;
  try
    for y := 0 to ASize - 1 do
      for x := 0 to ASize - 1 do
      begin
        acc := 0;
        for sy := 0 to MASK_SUPERSAMPLE - 1 do
          for sx := 0 to MASK_SUPERSAMPLE - 1 do
          begin
            c := intf.Colors[x * MASK_SUPERSAMPLE + sx, y * MASK_SUPERSAMPLE + sy];
            // White ink on black: any channel is the coverage, but averaging
            // the three also copes with subpixel-antialiased text.
            acc := acc + (c.red + c.green + c.blue) / (3 * 65535);
          end;
        Result[y * ASize + x] := acc / (MASK_SUPERSAMPLE * MASK_SUPERSAMPLE);
      end;
  finally
    intf.Free;
  end;
end;

// Blend a coverage mask onto the raster in a single colour.
procedure BlendMask(var ARaster: TArgbRaster; const AMask: TCoverageMask;
  ASize, AOffsetX, AOffsetY: integer; AColor: TColor);
var
  x, y: integer;
  rgb: longint;
  r, g, b: integer;
  cov: double;
begin
  rgb := ColorToRGB(AColor);
  r := Red(rgb);
  g := Green(rgb);
  b := Blue(rgb);
  for y := 0 to ASize - 1 do
    for x := 0 to ASize - 1 do
    begin
      cov := AMask[y * ASize + x];
      if cov > 0.002 then
        BlendPixel(ARaster, AOffsetX + x, AOffsetY + y, r, g, b, cov);
    end;
end;

// Allocate the oversized black canvas both mask builders draw on.
function NewMaskBitmap(ASize: integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.SetSize(ASize * MASK_SUPERSAMPLE, ASize * MASK_SUPERSAMPLE);
  Result.Canvas.Brush.Style := bsSolid;
  Result.Canvas.Brush.Color := clBlack;
  Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);
  Result.Canvas.Font.Color := clWhite;
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.Pen.Color := clWhite;
end;

// Shape AText centred in an ASize box, shrinking the font until it fits inside
// ASize - 2 * APadding. Returns the coverage mask.
function BuildTextMask(const AText: string; ASize, APadding: integer;
  const AFontName: string; AStartFontHeight, AMinFontHeight: integer): TCoverageMask;
var
  bmp: TBitmap;
  h, tw, th, avail: integer;
begin
  bmp := NewMaskBitmap(ASize);
  try
    if AFontName <> '' then
      bmp.Canvas.Font.Name := AFontName;
    bmp.Canvas.Font.Style := [fsBold];
    bmp.Canvas.Font.Color := clWhite;

    avail := (ASize - 2 * APadding) * MASK_SUPERSAMPLE;
    if avail < MASK_SUPERSAMPLE then
      avail := MASK_SUPERSAMPLE;

    h := AStartFontHeight * MASK_SUPERSAMPLE;
    bmp.Canvas.Font.Height := h;
    tw := bmp.Canvas.TextWidth(AText);
    th := bmp.Canvas.TextHeight(AText);
    while (tw > avail) and (h > AMinFontHeight * MASK_SUPERSAMPLE) do
    begin
      // Scale straight to the width that fits rather than stepping down one
      // pixel at a time; a 3x oversampled canvas makes each measure expensive.
      h := Max(AMinFontHeight * MASK_SUPERSAMPLE, Trunc(h * avail / tw));
      bmp.Canvas.Font.Height := h;
      tw := bmp.Canvas.TextWidth(AText);
      th := bmp.Canvas.TextHeight(AText);
    end;

    bmp.Canvas.Brush.Style := bsClear;
    bmp.Canvas.TextOut((bmp.Width - tw) div 2, (bmp.Height - th) div 2, AText);
    Result := MaskFromBitmap(bmp, ASize);
  finally
    bmp.Free;
  end;
end;

// Shape a trend as a shaft-less arrowhead. At badge size a glyph with a stem
// ("→", "↑") spends most of its ink on the stem, leaving the head — the part
// that carries the direction — only a couple of pixels wide. A bare triangle
// spends the whole area on the head and survives the panel's downscale.
//
// Returns @false for trends with no arrow form ("?", "X"); those fall back to
// the text path.
function BuildTrendMask(const ATrend: string; ASize: integer;
  out AMask: TCoverageMask): boolean;
const
  DIAG = 0.7071;
var
  bmp: TBitmap;
  dx, dy, cx, cy, span, len, back, wid, b: double;
  offs: array[0..1] of double;
  pts: array[0..2] of TPoint;
  i, n, dim: integer;
begin
  Result := false;
  dx := 0;
  dy := 0;
  n := 1;
  // n = 2 marks the doubled trends, drawn as two stacked heads.
  if ATrend = ARR_UP + ARR_UP then
  begin
    dy := -1;
    n := 2;
  end
  else if ATrend = ARR_DOWN + ARR_DOWN then
  begin
    dy := 1;
    n := 2;
  end
  else if ATrend = ARR_UP then
    dy := -1
  else if ATrend = ARR_DOWN then
    dy := 1
  else if ATrend = ARR_RIGHT then
    dx := 1
  else if ATrend = ARR_UPRIGHT then
  begin
    dx := DIAG;
    dy := -DIAG;
  end
  else if ATrend = ARR_DOWNRIGHT then
  begin
    dx := DIAG;
    dy := DIAG;
  end
  else
    Exit;

  if ASize < 1 then
    Exit;

  bmp := NewMaskBitmap(ASize);
  try
    dim := bmp.Width;
    span := dim;
    cx := dim / 2;
    cy := dim / 2;
    // Proportions matter more than they look: the head must be longer than it
    // is wide, or the two base corners end up sharper than the tip and the eye
    // reads the arrow as pointing at a corner instead — badly wrong for the
    // 45-degree trends, where the corners land on the cardinal axes.
    if n = 2 then
    begin
      // Two heads stacked along the direction, spanning -0.34..+0.34 of the
      // badge with a gap between them so they don't merge into one blob.
      len := 0.21 * span;
      back := 0.10 * span;
      wid := 0.24 * span;
      offs[0] := 0.12 * span;
      offs[1] := -0.24 * span;
    end
    else
    begin
      len := 0.34 * span;
      back := 0.21 * span;
      wid := 0.26 * span;
      offs[0] := 0;
      offs[1] := 0;
    end;

    for i := 0 to n - 1 do
    begin
      b := offs[i] - back;
      // Tip on the direction axis, base corners offset along the perpendicular
      // (-dy, dx).
      pts[0].X := Round(cx + dx * (offs[i] + len));
      pts[0].Y := Round(cy + dy * (offs[i] + len));
      pts[1].X := Round(cx + dx * b - dy * wid);
      pts[1].Y := Round(cy + dy * b + dx * wid);
      pts[2].X := Round(cx + dx * b + dy * wid);
      pts[2].Y := Round(cy + dy * b - dx * wid);
      bmp.Canvas.Polygon(pts);
    end;

    AMask := MaskFromBitmap(bmp, ASize);
    Result := true;
  finally
    bmp.Free;
  end;
end;

{------------------------------------------------------------------------------
  Image conversion
 ------------------------------------------------------------------------------}

// Read any LCL graphic into a straight-alpha raster. Icons and bitmaps that
// carry a transparency mask instead of an alpha channel are honoured through
// TLazIntfImage.Masked.
function RasterFromGraphic(AGraphic: TGraphic): TArgbRaster;
var
  intf: TLazIntfImage;
  x, y: integer;
  c: TFPColor;
  a: cardinal;
  hasAlpha: boolean;
begin
  Result := NewRaster(0, 0);
  if (AGraphic = nil) or AGraphic.Empty or not (AGraphic is TRasterImage) then
    Exit;
  if (AGraphic.Width <= 0) or (AGraphic.Height <= 0) then
    Exit;

  intf := TRasterImage(AGraphic).CreateIntfImage;
  try
    if (intf.Width <= 0) or (intf.Height <= 0) then
      Exit;
    hasAlpha := intf.DataDescription.AlphaPrec > 0;
    Result := NewRaster(intf.Width, intf.Height);
    for y := 0 to intf.Height - 1 do
      for x := 0 to intf.Width - 1 do
      begin
        c := intf.Colors[x, y];
        if hasAlpha then
          a := c.alpha shr 8
        else
        if intf.Masked[x, y] then
          a := 0
        else
          a := 255;
        Result.Px[y * Result.W + x] :=
          (a shl 24) or ((c.red shr 8) shl 16) or ((c.green shr 8) shl 8) or
          (c.blue shr 8);
      end;
  finally
    intf.Free;
  end;
end;

// Area-average downscale. Accumulation is premultiplied so transparent pixels
// cannot bleed their (undefined) colour into the edges of the logo — the
// classic halo you get from averaging straight alpha.
function DownscaleRaster(const ASrc: TArgbRaster; ADestSize: integer): TArgbRaster;
var
  x, y, sx, sy, sx0, sx1, sy0, sy1: integer;
  scaleX, scaleY, ar, ag, ab, aa, w: double;
  p: cardinal;
begin
  Result := NewRaster(ADestSize, ADestSize);
  if (ASrc.W <= 0) or (ASrc.H <= 0) or (ADestSize <= 0) then
    Exit;

  scaleX := ASrc.W / ADestSize;
  scaleY := ASrc.H / ADestSize;

  for y := 0 to ADestSize - 1 do
  begin
    sy0 := Floor(y * scaleY);
    sy1 := Min(ASrc.H - 1, Max(sy0, Ceil((y + 1) * scaleY) - 1));
    for x := 0 to ADestSize - 1 do
    begin
      sx0 := Floor(x * scaleX);
      sx1 := Min(ASrc.W - 1, Max(sx0, Ceil((x + 1) * scaleX) - 1));

      ar := 0;
      ag := 0;
      ab := 0;
      aa := 0;
      w := 0;
      for sy := sy0 to sy1 do
        for sx := sx0 to sx1 do
        begin
          p := ASrc.Px[sy * ASrc.W + sx];
          aa := aa + ((p shr 24) and $FF);
          ar := ar + ((p shr 16) and $FF) * ((p shr 24) and $FF) / 255;
          ag := ag + ((p shr 8) and $FF) * ((p shr 24) and $FF) / 255;
          ab := ab + (p and $FF) * ((p shr 24) and $FF) / 255;
          w := w + 1;
        end;
      if w = 0 then
        Continue;

      aa := aa / w;
      if aa < 0.5 then
        Continue;                        // stays fully transparent
      // Unpremultiply back to straight alpha for storage.
      ar := ar / w * 255 / aa;
      ag := ag / w * 255 / aa;
      ab := ab / w * 255 / aa;

      Result.Px[y * ADestSize + x] :=
        (cardinal(EnsureRange(Round(aa), 0, 255)) shl 24) or
        (cardinal(EnsureRange(Round(ar), 0, 255)) shl 16) or
        (cardinal(EnsureRange(Round(ag), 0, 255)) shl 8) or
        cardinal(EnsureRange(Round(ab), 0, 255));
    end;
  end;
end;

// Wrap a finished raster in a PNG. PNG rather than TBitmap because it is the
// one LCL image type that declares itself RGBA to every widgetset, so the
// alpha channel survives the trip into the tray.
function PngFromRaster(const ARaster: TArgbRaster): TPortableNetworkGraphic;
var
  intf: TLazIntfImage;
  desc: TRawImageDescription;
  x, y: integer;
  p: cardinal;
  c: TFPColor;
begin
  intf := TLazIntfImage.Create(0, 0);
  try
    desc.Init_BPP32_B8G8R8A8_BIO_TTB(ARaster.W, ARaster.H);
    intf.DataDescription := desc;
    intf.CreateData;
    for y := 0 to ARaster.H - 1 do
      for x := 0 to ARaster.W - 1 do
      begin
        p := ARaster.Px[y * ARaster.W + x];
        c.red := ((p shr 16) and $FF) * 257;
        c.green := ((p shr 8) and $FF) * 257;
        c.blue := (p and $FF) * 257;
        c.alpha := ((p shr 24) and $FF) * 257;
        intf.Colors[x, y] := c;
      end;
    Result := TPortableNetworkGraphic.Create;
    Result.LoadFromIntfImage(intf);
  finally
    intf.Free;
  end;
end;

{------------------------------------------------------------------------------
  Colour helpers
 ------------------------------------------------------------------------------}

function Luminance(AColor: TColor): double;
var
  rgb: longint;
begin
  rgb := ColorToRGB(AColor);
  Result := 0.299 * Red(rgb) + 0.587 * Green(rgb) + 0.114 * Blue(rgb);
end;

// Outline for a badge of the given fill: a darker shade of it when the fill is
// light, a lighter one when it is dark.
//
// The lighter direction interpolates towards white rather than scaling the
// channels up, which is what the Windows painter does. Scaling cannot lift a
// channel that is already zero, so the stale-reading badge — filled clBlack —
// would come out with a black border and no visible edge at all against a dark
// panel.
function BorderShade(AFill: TColor): TColor;
const
  DARKEN = 0.55;                         //< light fills: fraction kept
  LIGHTEN = 0.35;                        //< dark fills: fraction of the way to white
var
  rgb: longint;

function Mix(AChannel: integer): integer;
  begin
    if Luminance(AFill) > 140 then
      Result := Round(AChannel * DARKEN)
    else
      Result := Round(AChannel + (255 - AChannel) * LIGHTEN);
    Result := EnsureRange(Result, 0, 255);
  end;

begin
  rgb := ColorToRGB(AFill);
  Result := RGBToColor(Mix(Red(rgb)), Mix(Green(rgb)), Mix(Blue(rgb)));
end;

{------------------------------------------------------------------------------
  TTrndiBadgeIcon
 ------------------------------------------------------------------------------}

constructor TTrndiBadgeIcon.Create(ASize: integer);
begin
  inherited Create;
  if ASize < 16 then
    ASize := 16;
  FSize := ASize;
  FBase := NewRaster(0, 0);
  FSmall := NewRaster(0, 0);
end;

procedure TTrndiBadgeIcon.SetBase(AGraphic: TGraphic);
var
  src: TArgbRaster;
begin
  src := RasterFromGraphic(AGraphic);
  if (src.W <= 0) or (src.H <= 0) then
  begin
    FBase := NewRaster(0, 0);
    FSmall := NewRaster(0, 0);
    Exit;
  end;
  FBase := DownscaleRaster(src, FSize);
  // Scaled straight from the source, not from FBase — going through the
  // already-reduced copy would compound the filtering and mush the logo.
  FSmall := DownscaleRaster(src, Max(1, Round(FSize * SMALL_LOGO_RATIO)));
end;

function TTrndiBadgeIcon.HasBase: boolean;
begin
  Result := (FBase.W > 0) and (FBase.H > 0);
end;

function TTrndiBadgeIcon.Render(const AValue, ATrend: string;
ABadgeColor: TColor; ABadgeSizeRatio: double): TPortableNetworkGraphic;
var
  dst: TArgbRaster;
  badgeSize, trendSize, padding: integer;
  border, radius: double;
  left, top: double;
  textColor, borderColor: TColor;
  mask: TCoverageMask;
begin
  dst := NewRaster(FSize, FSize);

  // With no reading to show the icon is just the logo. With one, the logo is
  // *not* drawn full size underneath: the value badge covers four fifths of it,
  // and all the uncovered part contributes is a stray sliver of the droplet's
  // tip poking out above the badge. The corner mark below carries the identity
  // instead, which leaves a far cleaner icon at tray size.
  if AValue = '' then
  begin
    if HasBase then
      BlendRaster(dst, FBase, 0, 0);
    Exit(PngFromRaster(dst));
  end;

  borderColor := BorderShade(ABadgeColor);
  if Luminance(ABadgeColor) > 128 then
    textColor := clBlack
  else
    textColor := clWhite;

  badgeSize := Max(8, Round(FSize * ABadgeSizeRatio));
  border := Max(1.0, FSize * BORDER_RATIO);
  radius := Max(2.0, badgeSize * CORNER_RATIO);

  // Value badge, lower-right. Inset by half the border so the outline lands
  // inside the icon instead of being clipped by its edge.
  left := FSize - badgeSize + border / 2;
  top := FSize - badgeSize + border / 2;
  FillRoundRect(dst, left, top, FSize - border / 2, FSize - border / 2,
    radius, ABadgeColor, borderColor, border);

  padding := Max(1, Round(badgeSize * TEXT_PADDING_RATIO));
  mask := BuildTextMask(AValue, badgeSize, padding, FFontName,
    Max(6, Round(badgeSize * FONT_RATIO)), 6);
  BlendMask(dst, mask, badgeSize, FSize - badgeSize, FSize - badgeSize, textColor);

  // Upper-left corner: the trend badge when there is a trend, otherwise a small
  // copy of the logo so the icon still reads as Trndi's.
  if ATrend <> '' then
  begin
    trendSize := Max(8, Round(FSize * TREND_RATIO));
    radius := Max(2.0, trendSize * CORNER_RATIO);
    FillRoundRect(dst, border / 2, border / 2, trendSize - border / 2,
      trendSize - border / 2, radius, ABadgeColor, borderColor, border);

    if not BuildTrendMask(ATrend, trendSize, mask) then
      mask := BuildTextMask(ATrend, trendSize, Max(1, trendSize div 8), FFontName,
        Max(5, Round(trendSize * FONT_RATIO)), 5);
    BlendMask(dst, mask, trendSize, 0, 0, textColor);
  end
  else
  if (FSmall.W > 0) and (FSmall.H > 0) then
    BlendRaster(dst, FSmall, 0, 0);

  Result := PngFromRaster(dst);
end;

end.
