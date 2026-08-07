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
(* Paint event handlers for TfBG panels and the main form.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

// MacOSAll exports a classic-Mac `Point` record type that shadows
// Types.Point(x, y), turning `Point(a, b)` into a one-arg type cast and
// breaking the build on Darwin. MkPoint sidesteps the name clash.
function MkPoint(AX, AY: integer): TPoint; inline;
begin
  Result.X := AX;
  Result.Y := AY;
end;

// Draw a warning-triangle vector glyph (equilateral triangle with rounded tip
// and a centered exclamation bar+dot). Replaces the previous "⚠️" emoji in
// captions so the panel renders identically across platforms.
procedure DrawWarningGlyph(Canvas: TCanvas; CenterX, CenterY, Size: integer;
  Fill, Stroke, AccentColor: TColor);
var
  pts: array[0..2] of TPoint;
  halfSize, barW, barH, barX, barTopY, dotY: integer;
begin
  if Size < 8 then
    Size := 8;
  halfSize := Size div 2;

  // Equilateral-ish triangle, flat base
  pts[0] := MkPoint(CenterX, CenterY - halfSize);
  pts[1] := MkPoint(CenterX - halfSize, CenterY + halfSize);
  pts[2] := MkPoint(CenterX + halfSize, CenterY + halfSize);

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Fill;
  Canvas.Pen.Color := Stroke;
  Canvas.Pen.Width := Max(1, Size div 16);
  Canvas.Pen.Style := psSolid;
  Canvas.Polygon(pts);

  // Exclamation bar
  barW := Max(2, Size div 9);
  barH := Max(4, Size div 3);
  barX := CenterX - (barW div 2);
  barTopY := CenterY - halfSize + (Size div 3);
  Canvas.Brush.Color := AccentColor;
  Canvas.Pen.Style := psClear;
  Canvas.FillRect(Classes.Rect(barX, barTopY, barX + barW, barTopY + barH));

  // Exclamation dot
  dotY := CenterY + halfSize - Max(4, Size div 6);
  Canvas.Ellipse(barX, dotY, barX + barW, dotY + barW);
end;

// Draw the stale-data glyph: a clock face with hour/minute hands, struck
// through by a diagonal bar once the outage is long enough to read as a lost
// connection rather than a late reading. Vector rather than an emoji so the
// card renders identically on every widgetset.
procedure DrawStaleGlyph(Canvas: TCanvas; CenterX, CenterY, Size: integer;
  Fill, Stroke: TColor; Struck: boolean);
var
  radius, handLen, ringW, off: integer;
begin
  if Size < 12 then
    Size := 12;
  radius := Size div 2;
  ringW := Max(2, Size div 10);

  // Face
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Fill;
  Canvas.Pen.Color := Stroke;
  Canvas.Pen.Width := ringW;
  Canvas.Pen.Style := psSolid;
  Canvas.Ellipse(CenterX - radius, CenterY - radius,
    CenterX + radius, CenterY + radius);

  // Hands frozen at 10:10 — a stopped clock reads as "time has passed", and
  // the symmetric pose stays legible at the small sizes the card uses.
  Canvas.Pen.Width := Max(2, Size div 11);
  handLen := round(radius * 0.55);
  Canvas.MoveTo(CenterX, CenterY);
  Canvas.LineTo(CenterX, CenterY - handLen);
  Canvas.MoveTo(CenterX, CenterY);
  Canvas.LineTo(CenterX + round(handLen * 0.75), CenterY + round(handLen * 0.4));

  if Struck then
  begin
    // The slash is drawn twice: once in the fill color as a slightly wider
    // "cut", then the stroke on top, so it reads as a gap through the face
    // rather than a line lying over it.
    off := round(radius * 0.78);
    Canvas.Pen.Color := Fill;
    Canvas.Pen.Width := Max(4, Size div 5);
    Canvas.MoveTo(CenterX - off, CenterY + off);
    Canvas.LineTo(CenterX + off, CenterY - off);
    Canvas.Pen.Color := Stroke;
    Canvas.Pen.Width := Max(2, Size div 10);
    Canvas.MoveTo(CenterX - off, CenterY + off);
    Canvas.LineTo(CenterX + off, CenterY - off);
  end;
end;

type
  TRangeBand = record
    Top, Bottom: integer;
    Color: TColor;
    Alpha: byte;
  end;

// DrawRangeBands: paint a set of horizontal alpha bands onto ACanvas via a
// 1-pixel-wide premultiplied-alpha bitmap that the OS stretches across the
// destination width. Bands composite in array order using Porter-Duff "over"
// — a custom range drawn on top of a hi/lo range naturally appears slightly
// deeper without needing the old triple-BlendColors math.
//
// Each band fades in over AGradientPx rows at the top and bottom edges,
// giving the band a soft transition into the surrounding background instead
// of a hard rectangle. Raster and blit go through the shared primitives in
// inc/umain_alphablit.inc.
procedure DrawRangeBands(ACanvas: TCanvas; AWidth, AHeight: integer;
  const Bands: array of TRangeBand; AGradientPx: integer);

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
      // (Porter-Duff: dstR = br*srcA + dstR*invA — the srcA factor is baked
      // in), which is exactly what PutPremultipliedPixel takes.
      PutPremultipliedPixel(intf, 0, y, dstR, dstG, dstB, dstA);
    end;

    // One column stretched across the full width — the bands are uniform
    // horizontally, so there is nothing to gain from rasterizing AWidth
    // identical copies.
    img := AlphaImageFromIntf(intf, 1, AHeight);
    try
      BlitAlphaImage(ACanvas, img, 1, AHeight, 0, 0, AWidth, AHeight);
    finally
      img.Free;
    end;
  finally
    intf.Free;
  end;
end;

procedure TfBG.pnWarningPaint(Sender: TObject);
const
  RadiusCard = 18;
  RadiusBanner = 14;
var
  P: TPanel;
  panelFillColor, strokeColor, accentColor, glyphFill: TColor;
  tintColor: TColor;
  opacityTmp: byte;
  layout: TWarnLayout;
  radius: integer;
  glyphSize, glyphX, glyphY: integer;
  chevronCX, chevronCY, chevronS: integer;
  chevPts: array[0..2] of TPoint;
  pulseAmt: double;
  headRowH, accentW: integer;
begin
  P := TPanel(Sender);
  GetWarnSeverityVisual(FWarnSeverity, tintColor, opacityTmp, layout);

  if layout = wlStatusCard then
    radius := RadiusCard
  else
    radius := RadiusBanner;

  with P.Canvas do
  begin
    Brush.Color := fBG.Color;
    Brush.Style := bsSolid;
    FillRect(0, 0, P.Width, P.Height);

    {$ifndef X_WIN}
    panelFillColor := BlendColors(tintColor, fBG.Color, opacityTmp / 255.0);
    {$else}
    panelFillColor := tintColor;
    {$endif}

    // For wsSoon, pulse the tint between base orange and a brighter shade on a
    // 2-second cadence. Driven by tProgressTimer; mirrors the alternation
    // pattern already used in pnNextProgressPaint for overdue readings.
    if FWarnSeverity = wsSoon then
    begin
      if (SecondOf(Now) mod 2) = 0 then
        pulseAmt := 0.45
      else
        pulseAmt := 0.0;
      panelFillColor := BlendColors(panelFillColor,
        LightenColor(panelFillColor, 0.30), pulseAmt);
    end;

    Brush.Color := panelFillColor;
    strokeColor := BlendColors(panelFillColor, clBlack, 0.30);
    Pen.Color := strokeColor;
    Pen.Width := 1;
    Pen.Style := psSolid;
    RoundRect(0, 0, P.Width, P.Height, radius, radius);

    // Subtle top highlight
    Pen.Color := LightenColor(panelFillColor, 0.20);
    MoveTo(radius div 2, 2);
    LineTo(P.Width - (radius div 2), 2);

    accentColor := DarkenColor(panelFillColor, 0.45);
    glyphFill := BlendColors(clWhite, panelFillColor, 0.85);

    if layout = wlStatusCard then
    begin
      // ----- Stale status card -----
      // The headline row is whatever fixWarningPanel gave lMissing; deriving
      // the glyph and chevron from those bounds keeps them aligned with the
      // text and unaffected by the expanded detail rows growing below.
      headRowH := lMissing.Height;
      if headRowH <= 0 then
        headRowH := P.Height;

      // Severity is carried by a bright bar down the leading edge rather than
      // by the whole surface, so escalating from slate to red doesn't turn the
      // card into a block of alarm color.
      accentW := Max(3, P.Width div 90);
      Brush.Color := LightenColor(tintColor, 0.45);
      Pen.Style := psClear;
      FillRect(0, radius div 2, accentW, P.Height - (radius div 2));
      Pen.Style := psSolid;

      // Clock glyph centered in the gutter fixWarningPanel reserved to the left
      // of the headline (0 .. lMissing.Left).
      glyphSize := Max(16, Min(round(headRowH * 0.72),
        lMissing.Left - accentW - 10));
      glyphX := Max(accentW + 4 + (glyphSize div 2), lMissing.Left div 2);
      glyphY := lMissing.Top + (headRowH div 2);
      DrawStaleGlyph(P.Canvas, glyphX, glyphY, glyphSize,
        panelFillColor, glyphFill, FStaleStage = stLost);

      if WarnShowDetails then
      begin
        chevronS := Max(5, P.Width div 60);
        chevronCX := P.Width - (chevronS * 2) - 6;
        chevronCY := lMissing.Top + (headRowH div 2);
        if FWarnExpanded then
        begin
          // Up chevron (▴)
          chevPts[0] := MkPoint(chevronCX, chevronCY - chevronS div 2);
          chevPts[1] := MkPoint(chevronCX - chevronS, chevronCY + chevronS div 2);
          chevPts[2] := MkPoint(chevronCX + chevronS, chevronCY + chevronS div 2);
        end
        else
        begin
          // Down chevron (▾)
          chevPts[0] := MkPoint(chevronCX - chevronS, chevronCY - chevronS div 2);
          chevPts[1] := MkPoint(chevronCX + chevronS, chevronCY - chevronS div 2);
          chevPts[2] := MkPoint(chevronCX, chevronCY + chevronS div 2);
        end;
        Brush.Color := BlendColors(glyphFill, panelFillColor, 0.35);
        Pen.Style := psClear;
        Polygon(chevPts);
      end;
    end
    else
    begin
      // Banner: glyph and chevron live within the collapsed (base) area, so they
      // don't move when the expanded detail row appears below.
      headRowH := FWarnBannerBaseH;
      if headRowH <= 0 then
        headRowH := P.Height;

      glyphSize := Max(20, headRowH - 20);
      glyphX := 12 + glyphSize div 2;
      glyphY := headRowH div 2;
      DrawWarningGlyph(P.Canvas, glyphX, glyphY, glyphSize,
        glyphFill, accentColor, accentColor);

      // Expand chevron on the right (only when there's something to expand to)
      if WarnShowDetails then
      begin
        chevronS := 6;
        chevronCX := P.Width - 16;
        chevronCY := headRowH div 2;
        if FWarnExpanded then
        begin
          // Up chevron (▴)
          chevPts[0] := MkPoint(chevronCX, chevronCY - chevronS div 2);
          chevPts[1] := MkPoint(chevronCX - chevronS, chevronCY + chevronS div 2);
          chevPts[2] := MkPoint(chevronCX + chevronS, chevronCY + chevronS div 2);
        end
        else
        begin
          // Down chevron (▾)
          chevPts[0] := MkPoint(chevronCX - chevronS, chevronCY - chevronS div 2);
          chevPts[1] := MkPoint(chevronCX + chevronS, chevronCY - chevronS div 2);
          chevPts[2] := MkPoint(chevronCX, chevronCY + chevronS div 2);
        end;
        Brush.Color := accentColor;
        Pen.Style := psClear;
        Polygon(chevPts);
      end;
    end;
  end;
end;

procedure TfBG.FormPaint(Sender: TObject);
const
  // Alphas chosen to mirror the prior BlendColors(*, fBG, 0.3) and
  // DarkenColor(fBG, 0.9 / 0.85) mixes so the displayed band intensity stays
  // close to what existed before the refactor.
  // Alphas were originally tuned against a double-premultiply bug in
  // DrawRangeBands that darkened bands by an extra factor of ~dstA. After
  // fixing the math, these are roughly halved to preserve the previously
  // approved visual; retune if a different intensity is preferred.
  HiLoAlphaWithColor   = 38;   // was 77 under the double-premultiply bug
  HiLoAlphaFallback    = 13;   // was 26
  // Custom band still sits visibly above the hi/lo (higher alpha + small
  // pigment pre-darken) so the user's "ideal goal" inside the acceptable
  // range reads as a deeper sweet spot.
  CustomAlphaWithColor = 82;   // was 165
  CustomAlphaFallback  = 32;   // was 64
  CustomPigmentDarken  = 0.8;  // pre-darken applied to tir_custom_bg
  BandEdgeGradientPx   = 3;
var
  loY, hiY, rangeLoY, rangeHiY: integer;
  cnv: TCanvas;
  lineColor, edgeColor: TColor;
  hiloDisplayedColor, customDisplayedColor: TColor;
  drawLo, drawHi, drawRangeLo, drawRangeHi: boolean;
  rowOffset: integer;
  clientH: integer;
  dotHeight: integer;
  bands: array of TRangeBand;

  debugY: integer;
  debugValue: single;
  debugText: string;
  i: integer;

function ValueToY(const Value: single): integer;
  var
    Padding, UsableHeight, Position: integer;
    v: single;
  begin
    Padding := Round(clientH * 0.1);
    UsableHeight := clientH - (Padding * 2);
    v := Value;
    if v < BG_API_MIN then
      v := BG_API_MIN;
    if v > BG_API_MAX then
      v := BG_API_MAX;
    Position := Padding + Round((v - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) *
      UsableHeight);
    Result := (clientH - Position) - 1;
  end;

begin
  if native.getboolsetting('main.high_contrast', false) then
    Exit;

  if not Assigned(Self) then
    Exit;

  if not (PaintRange or PaintRangeCGMRange) then
    Exit;

  cnv := Self.Canvas;
  if (Length(TrendDots) > 1) and Assigned(TrendDots[1]) and
    Assigned(TrendDots[1].Parent) then
    clientH := TrendDots[1].Parent.ClientHeight
  else
    clientH := Self.ClientHeight;

  dotHeight := 0;
  if (Length(TrendDots) > 1) and Assigned(TrendDots[1]) then
    // Drawn dots are sized as a square in ResizeDot; the control's Height
    // *is* the diameter, so we can read it directly.
    dotHeight := Max(TrendDots[1].Height, TrendDots[1].Font.Size);

  // ValueToY returns the dot *center*; threshold lines, band edges, and the
  // DOT_LINES debug guides all need to sit at the dot bottom so a dot whose
  // value equals the threshold rests flush against the line/band edge.
  rowOffset := (dotHeight div 2) + DOT_OFFSET_RANGE;

  // Skip threshold/range bands until the first fetch has landed — drawing
  // them over an empty form (no dot to anchor them against) just produces
  // the "what is this?" lines visible in the boot screenshot.
  //
  // The stale card gets the same treatment: the bands and their hairlines
  // grade a *current* value against the thresholds, and there isn't one. Left
  // on, they float over a dimmed reading nobody should be acting on. The card
  // being up (wsCritical) is exactly the "data is not current" condition —
  // cheaper to read here than recomputing freshness on every paint.
  if FBootFetchPending or
     (Assigned(pnWarning) and pnWarning.Visible and (FWarnSeverity = wsCritical)) then
  begin
    drawLo := false;
    drawHi := false;
    drawRangeLo := false;
    drawRangeHi := false;
  end
  else
  begin
    drawLo := (Assigned(api) and (api.cgmLo <> 0) and PaintRange);
    drawHi := (Assigned(api) and (api.cgmHi <> 0) and PaintRange);
    drawRangeLo := (Assigned(api) and (api.cgmRangeLo <> 0) and PaintRangeCGMRange);
    drawRangeHi := (Assigned(api) and (api.cgmRangeHi > 0) and (api.cgmRangeHi < 500) and PaintRangeCGMRange);
  end;

  if drawLo then
    loY := ValueToY(api.cgmLo * BG_CONVERTIONS[mmol][mgdl]) + rowOffset;

  if drawHi then
    hiY := ValueToY(api.cgmHi * BG_CONVERTIONS[mmol][mgdl]) + rowOffset;

  if drawRangeLo then
    rangeLoY := ValueToY(api.cgmRangeLo * BG_CONVERTIONS[mmol][mgdl]) + rowOffset;

  if drawRangeHi then
    rangeHiY := ValueToY(api.cgmRangeHi * BG_CONVERTIONS[mmol][mgdl]) + rowOffset;

  // Build the band list. Hi/lo first so the custom band composites on top
  // inside the overlap region (the custom range is constrained to live
  // inside the hi/lo range — see rs_custom_range_help).
  SetLength(bands, 0);
  if drawLo and drawHi then
  begin
    SetLength(bands, Length(bands) + 1);
    with bands[High(bands)] do
    begin
      Top := hiY;
      Bottom := loY;
      if tir_bg = -1 then
      begin
        Color := clBlack;
        Alpha := HiLoAlphaFallback;
      end
      else
      begin
        Color := tir_bg;
        Alpha := HiLoAlphaWithColor;
      end;
    end;
  end;

  if drawRangeLo and drawRangeHi then
  begin
    SetLength(bands, Length(bands) + 1);
    with bands[High(bands)] do
    begin
      Top := rangeHiY;
      Bottom := rangeLoY;
      if tir_custom_bg = -1 then
      begin
        Color := clBlack;
        Alpha := CustomAlphaFallback;
      end
      else
      begin
        Color := DarkenColor(tir_custom_bg, CustomPigmentDarken);
        Alpha := CustomAlphaWithColor;
      end;
    end;
  end;

  if Length(bands) > 0 then
    DrawRangeBands(cnv, Self.ClientWidth, clientH, bands, BandEdgeGradientPx);

  // Subtle hairline at each band edge so the threshold has a defined
  // boundary even when PaintRangeLines is off. Skipped when PaintRangeLines
  // is on so we don't stack a thin and a thick line at the same Y.
  if not PaintRangeLines then
  begin
    cnv.Brush.Style := bsClear;
    cnv.Pen.Style := psSolid;
    cnv.Pen.Width := 1;

    if drawLo and drawHi then
    begin
      if tir_bg = -1 then
        hiloDisplayedColor := DarkenColor(fBG.Color, 0.9)
      else
        hiloDisplayedColor := BlendColors(tir_bg, fBG.Color, HiLoAlphaWithColor / 255.0);
      edgeColor := LightenColor(hiloDisplayedColor, 0.35);
      cnv.Pen.Color := edgeColor;
      cnv.MoveTo(0, hiY); cnv.LineTo(Self.ClientWidth, hiY);
      cnv.MoveTo(0, loY); cnv.LineTo(Self.ClientWidth, loY);
    end;

    if drawRangeLo and drawRangeHi then
    begin
      if tir_custom_bg = -1 then
        customDisplayedColor := DarkenColor(fBG.Color, 0.75)
      else
      if tir_bg = -1 then
        customDisplayedColor := BlendColors(DarkenColor(tir_custom_bg, CustomPigmentDarken),
          fBG.Color, CustomAlphaWithColor / 255.0)
      else
        customDisplayedColor := BlendColors(DarkenColor(tir_custom_bg, CustomPigmentDarken),
          BlendColors(tir_bg, fBG.Color, HiLoAlphaWithColor / 255.0),
          CustomAlphaWithColor / 255.0);
      edgeColor := LightenColor(customDisplayedColor, 0.35);
      cnv.Pen.Color := edgeColor;
      cnv.MoveTo(0, rangeHiY); cnv.LineTo(Self.ClientWidth, rangeHiY);
      cnv.MoveTo(0, rangeLoY); cnv.LineTo(Self.ClientWidth, rangeLoY);
    end;
  end;

  cnv.Brush.Style := bsClear;

  if PaintRangeLines then
  begin
    if drawLo then
    begin
      lineColor := LightenColor(bg_rel_color_lo);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 2;
      cnv.MoveTo(0, loY);
      cnv.LineTo(Self.ClientWidth, loY);
    end;

    if drawHi then
    begin
      lineColor := LightenColor(bg_rel_color_hi);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 2;
      cnv.MoveTo(0, hiY);
      cnv.LineTo(Self.ClientWidth, hiY);
    end;

    // Custom-range threshold lines: tint of the user's chosen custom color
    // (parallels the hi/lo lines using bg_rel_color_lo/hi). The old code
    // used LightenColor(fBG.Color, 0.5) which disappears against the lighter
    // hi/lo band; tying it to tir_custom_bg keeps it readable.
    if drawRangeLo or drawRangeHi then
    begin
      if tir_custom_bg = -1 then
        lineColor := LightenColor(fBG.Color, 0.5)
      else
        lineColor := LightenColor(tir_custom_bg, 0.4);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 1;
      if drawRangeLo then
      begin
        cnv.MoveTo(0, rangeLoY);
        cnv.LineTo(Self.ClientWidth, rangeLoY);
      end;
      if drawRangeHi then
      begin
        cnv.MoveTo(0, rangeHiY);
        cnv.LineTo(Self.ClientWidth, rangeHiY);
      end;
    end;
  end;

  if DOT_LINES then
  begin
    cnv.Brush.Style := bsClear;
    cnv.Pen.Style := psDot;
    cnv.Pen.Width := 1;
    cnv.Pen.Color := LightenColor(fBG.Color, 0.3);
    cnv.Font.Size := 8;
    cnv.Font.Color := LightenColor(fBG.Color, 0.4);

    for i := 1 to 4 do
    begin
      debugValue := i * 5.0;
      if (debugValue >= BG_API_MIN) and (debugValue <= BG_API_MAX) then
      begin
        // Guidelines label where a dot of that value would sit; align with
        // the dot centre (ValueToY) rather than the dot bottom that the
        // threshold/band lines use.
        debugY := ValueToY(debugValue);
        cnv.MoveTo(0, debugY);
        cnv.LineTo(Self.ClientWidth, debugY);
        debugText := Format('%.0f', [debugValue]);
        cnv.TextOut(Self.ClientWidth - cnv.TextWidth(debugText) - 5,
          debugY - (cnv.TextHeight(debugText) div 2),
          debugText);
      end;
    end;
  end;
end;

procedure TfBG.pnOffReadingPaint(Sender: TObject);
var
  Panel: TPanel;
  X, Y: integer;
  TextStr: string;
  TextW, TextH: integer;
begin
  Panel := Sender as TPanel;
  TextStr := Panel.Caption;
  if TextStr = '' then
    Exit;
  with Panel.Canvas do
  begin
    Brush.Color := Panel.Color;
    Brush.Style := bsSolid;
    FillRect(Classes.Rect(0, 0, Panel.Width, Panel.Height));
    Font.Assign(Panel.Font);
    Font.Orientation := 900;

    font.Height := panel.Width;
    Brush.Style := bsClear;
    TextW := TextWidth(TextStr);
    TextH := TextHeight(TextStr);
    X := (Panel.Width - TextH) div 2;
    Y := (Panel.Height + TextW) div 2;
    TextOut(X, Y, TextStr);
  end;
end;

// Paint handler for the left-side "next reading" progress indicator.
//
// The indicator is a capsule-shaped tube that fills from the bottom as the next
// reading approaches. Every pixel column is derived from the capsule's own
// geometry instead of from Canvas.RoundRect: no widgetset gives us an
// anti-aliased rounded rect — let alone a gradient clipped to one — so the caps
// would be visibly stepped and the fill would push square corners out past the
// rounded outline. Columns are emitted as vertical runs of equal colour, which
// keeps a repaint at a few dozen FillRects rather than a per-pixel loop.
procedure TfBG.pnNextProgressPaint(Sender: TObject);
const
  WARM_FROM  = 0.90;    // fraction at which the fill starts warming toward amber
  MENISCUS_H = 2;       // px of bright surface line drawn on top of the liquid
  SHADOW_H   = 6.0;     // px of contact shadow where the liquid meets the cap
  PULSE_MS   = 1600;    // one full breath while the reading is overdue
  OVERDUE_GRACE_MS = 60000;  // grace past the due time before the tube pulses
var
  pnl: TPanel;
  cvs: TCanvas;
  bgCol, trackCol, fillCol, runCol, rowCol: TColor;
  elapsedMs, spanMs: int64;
  frac, warmth, pulse: double;
  trackW, cx, rad, dx, dy, capTop, capBot, xn, shade, lvlTop, lvlBot, cov: double;
  m, w, h, x, y, runTop: integer;

  function Shaded(base: TColor; amt: double): TColor;
  begin
    if amt >= 0 then
      Result := LightenColor(base, amt)
    else
      Result := DarkenColor(base, 1.0 + amt);
  end;

  // Colour of the tube at row yy of the column being painted, before the
  // capsule's edge coverage is applied.
  function SurfaceAt(yy: integer): TColor;
  var
    below, depth: double;
  begin
    // How much of this pixel row sits below the liquid surface
    below := Min(1.0, Max(0.0, (yy + 1) - lvlTop));
    if below <= 0 then
      // Empty part of the tube: a hair darker down the middle, so the channel
      // reads as recessed rather than as a flat stripe.
      Exit(Shaded(trackCol, -0.06 * exp(-sqr(xn - 0.5) / 0.10)));

    depth := Min(1.0, Max(0.0, (lvlBot - yy) / Max(1.0, lvlBot - lvlTop)));
    Result := BlendColors(LightenColor(fillCol, 0.10 * depth), fillCol, 0.55);
    // Contact shadow where the liquid sits in the bottom cap. Kept at a fixed
    // few pixels: it is a lighting detail, not something that should grow with
    // the fill — and every distinct row colour costs one more FillRect.
    if lvlBot - yy < SHADOW_H then
      Result := DarkenColor(Result, 0.90 + 0.10 * ((lvlBot - yy) / SHADOW_H));
    if (yy < lvlTop + MENISCUS_H) and (lvlBot - lvlTop > 4) then
      Result := LightenColor(Result, 0.38);     // bright meniscus at the surface
    Result := Shaded(Result, shade);
    if below < 1.0 then                         // the surface row is a blend of both
      Result := BlendColors(Result, Shaded(trackCol, -0.06), below);
  end;

  procedure FlushRun(y0, y1: integer; col: TColor);
  begin
    if (y0 < 0) or (y1 <= y0) then
      Exit;
    cvs.Brush.Color := col;
    cvs.FillRect(Classes.Rect(x, y0, x + 1, y1));
  end;

begin
  pnl := Sender as TPanel;
  cvs := pnl.Canvas;
  w := pnl.Width;
  h := pnl.Height;
  bgCol := fBG.Color;

  // The panel is painted edge to edge every time — the capsule is drawn onto
  // the window colour, so nothing of the previous frame can survive around it.
  cvs.Brush.Style := bsSolid;
  cvs.Pen.Style := psClear;
  cvs.Brush.Color := bgCol;
  cvs.FillRect(Classes.Rect(0, 0, w, h));

  if (bgs = nil) or (Length(bgs) = 0) then
    Exit;

  m := Max(1, w div 10);          // margin, so the caps never touch the panel edge
  trackW := w - 2 * m;
  if (trackW < 2) or (h < 4 * m) then
    Exit;
  cx := m + trackW / 2;
  rad := trackW / 2;

  // Signed on purpose: MilliSecondsBetween is Abs()-based, which turns a
  // future-dated reading (uploader clock ahead of ours — the same skew
  // TrndiAPI.timeDiff exists for) into a large positive "age" and pegs the tube
  // full. Subtracting directly keeps the sign so the guard below can catch it.
  elapsedMs := Round((Now - lastReading.date) * MSecsPerDay);
  if elapsedMs < 0 then
    elapsedMs := 0;

  // Count the cycle the scheduler *targets*: BG_REFRESH plus the resync padding
  // SetNextUpdateTimer adds to give the CGM time to report. A flat BG_REFRESH
  // leaves the tube pegged full through that padding every single cycle.
  //
  // Deliberately not the literal next-fetch time: when a reading lands already
  // old, SetNextUpdateTimer clamps to a MIN_REFRESH_INTERVAL_MS retry, and
  // stretching the fill to reach full at that retry would slow the tube down
  // exactly when data is late — six minutes in and still filling. Late data
  // should read as full and overdue; the retry countdown belongs to the stale
  // card, which already shows it.
  spanMs := BG_REFRESH + REFRESH_RESYNC_BUFFER_MS;

  if spanMs > 0 then
    frac := Min(1.0, elapsedMs / spanMs)
  else
    frac := 1.0;
  // A minute past the top before the tube starts breathing red — the same grace
  // as before, now counted from the end of the padded cycle rather than from a
  // bare BG_REFRESH, so it lands 15 s later than it used to.
  FProgressPulsing := elapsedMs > spanMs + OVERDUE_GRACE_MS;

  // Track: a recessed channel, darker than the window on light backgrounds and
  // lighter on dark ones, so the empty part of the tube stays visible either way.
  if IsLightColor(bgCol) then
    trackCol := BlendColors(clBlack, bgCol, 0.16)
  else
    trackCol := BlendColors(clWhite, bgCol, 0.10);

  // Fill: teal for most of the cycle → amber as the reading falls due →
  // pomegranate once it is late. The teal stage only brightens as it rises;
  // blending teal straight into amber across the whole cycle would spend
  // minutes in the muddy olive midpoint the two colours share.
  if FProgressPulsing then
  begin
    // Breathe rather than flash: tProgressTimer speeds up while a reading is
    // overdue, so this ramp is sampled often enough to read as continuous.
    pulse := 0.5 - 0.5 * cos(2 * Pi * ((GetTickCount64 mod PULSE_MS) / PULSE_MS));
    fillCol := BlendColors(RGBToColor(255, 105, 66), RGBToColor(183, 47, 36), pulse);
  end
  else if frac >= WARM_FROM then
  begin
    warmth := (frac - WARM_FROM) / (1.0 - WARM_FROM);
    warmth := warmth * warmth * (3 - 2 * warmth);   // ease through the mixed shades
    fillCol := BlendColors(RGBToColor(230, 126, 34), RGBToColor(42, 182, 166), warmth);
  end
  else
    fillCol := BlendColors(RGBToColor(42, 182, 166), RGBToColor(34, 138, 138),
      frac / WARM_FROM);

  lvlBot := h - m;
  lvlTop := lvlBot - frac * (h - 2 * m);

  for x := 0 to w - 1 do
  begin
    dx := (x + 0.5) - cx;
    if Abs(dx) >= rad then
      Continue;
    // Where this column enters and leaves the capsule — the straight sides are
    // pixel aligned, so only the caps need the circle.
    dy := sqrt(rad * rad - dx * dx);
    capTop := (m + rad) - dy;
    capBot := (h - m - rad) + dy;

    // Soft cylindrical shading: a specular band left of centre, a gentle
    // falloff to the right, which is what makes the thin bar read as a tube.
    xn := ((x + 0.5) - m) / trackW;
    shade := 0.22 * exp(-sqr(xn - 0.30) / 0.07) - 0.18 * Max(0.0, xn - 0.55);

    runTop := -1;
    runCol := clNone;
    // Qualified: MacOSAll re-exports C floor/ceil (Double), which would
    // otherwise shadow Math's Integer-returning versions on macOS.
    for y := Math.Floor(capTop) to Math.Ceil(capBot) - 1 do
    begin
      if (y < 0) or (y >= h) then
        Continue;
      cov := Max(0.0, Min(Min(1.0, (y + 1) - capTop), Min(1.0, capBot - y)));
      if cov <= 0 then
        Continue;
      rowCol := SurfaceAt(y);
      if cov < 1.0 then                  // anti-alias the cap against the window
        rowCol := BlendColors(rowCol, bgCol, cov);
      if (runTop >= 0) and (rowCol = runCol) then
        Continue;
      FlushRun(runTop, y, runCol);
      runTop := y;
      runCol := rowCol;
    end;
    FlushRun(runTop, Min(h, Math.Ceil(capBot)), runCol);
  end;
end;
