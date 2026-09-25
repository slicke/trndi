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
 * - 2026-09-25: FormPaint records the range bands it composited in
 *   FPaintedBands so the trend surface can match the backdrop under each
 *   dot for its knockout halo. It also paints the delta pill and its
 *   direction chevron behind lDiff (PaintDeltaPill).
 * - 2026-09-20: The warning and stale glyphs and the expand chevrons are
 *   drawn through trndi.raster (convex polygons, capsule strokes, smooth
 *   discs) instead of the aliased Canvas.Polygon/Ellipse/LineTo primitives,
 *   so they match the antialiased trend dots on GDI and Qt.
 * - 2026-08-27: FormPaint anchors the threshold lines on the layout pass's
 *   FTrendDotDiameter instead of probing the removed per-dot controls.
 * - 2026-08-21: The shutdown screen's accent gradients call Classes.Rect
 *   explicitly - on Darwin, MacOSAll's QuickDraw Rect type shadows the RTL
 *   function, which broke the macOS build.
 *)
(* Paint event handlers for TfBG panels and the main form.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

// Draw a warning-triangle vector glyph (equilateral triangle with rounded tip
// and a centered exclamation bar+dot). Replaces the previous "⚠️" emoji in
// captions so the panel renders identically across platforms.
procedure DrawWarningGlyph(Canvas: TCanvas; CenterX, CenterY, Size: integer;
  Fill, Stroke, AccentColor: TColor);
var
  pts: array[0..2] of TPoint;
  bar: array[0..3] of TPoint;
  halfSize, barW, barH, barX, barTopY, dotY: integer;
begin
  if Size < 8 then
    Size := 8;
  halfSize := Size div 2;

  // Equilateral-ish triangle, flat base, outlined with a mitred pen
  pts[0] := MkPoint(CenterX, CenterY - halfSize);
  pts[1] := MkPoint(CenterX - halfSize, CenterY + halfSize);
  pts[2] := MkPoint(CenterX + halfSize, CenterY + halfSize);
  DrawSmoothConvexPolygon(Canvas, pts, Fill, Stroke, Max(1, Size div 16));

  // Exclamation bar
  barW := Max(2, Size div 9);
  barH := Max(4, Size div 3);
  barX := CenterX - (barW div 2);
  barTopY := CenterY - halfSize + (Size div 3);
  bar[0] := MkPoint(barX, barTopY);
  bar[1] := MkPoint(barX + barW, barTopY);
  bar[2] := MkPoint(barX + barW, barTopY + barH);
  bar[3] := MkPoint(barX, barTopY + barH);
  DrawSmoothConvexPolygon(Canvas, bar, AccentColor);

  // Exclamation dot
  dotY := CenterY + halfSize - Max(4, Size div 6);
  DrawSmoothCircle(Canvas, barW, AccentColor, clNone, 0, barX, dotY);
end;

// Draw the stale-data glyph: a clock face with hour/minute hands, struck
// through by a diagonal bar once the outage is long enough to read as a lost
// connection rather than a late reading. Vector rather than an emoji so the
// card renders identically on every widgetset.
procedure DrawStaleGlyph(Canvas: TCanvas; CenterX, CenterY, Size: integer;
  Fill, Stroke: TColor; Struck: boolean);

  function Seg(x1, y1, x2, y2: double; c: TColor): TSmoothStroke;
  begin
    Result.X1 := x1; Result.Y1 := y1;
    Result.X2 := x2; Result.Y2 := y2;
    Result.Color := c;
  end;

var
  radius, handLen, ringW, off, faceD: integer;
  hands: array[0..1] of TSmoothStroke;
  slash: array[0..0] of TSmoothStroke;
begin
  if Size < 12 then
    Size := 12;
  radius := Size div 2;
  ringW := Max(2, Size div 10);

  // Face: the pen used to straddle the circle, so the rimmed disc is one
  // ring width wider than Size to keep the same footprint.
  faceD := Size + ringW;
  DrawSmoothCircle(Canvas, faceD, Fill, Stroke, ringW,
    CenterX - faceD div 2, CenterY - faceD div 2);

  // Hands frozen at 10:10 — a stopped clock reads as "time has passed", and
  // the symmetric pose stays legible at the small sizes the card uses.
  handLen := round(radius * 0.55);
  hands[0] := Seg(CenterX, CenterY, CenterX, CenterY - handLen, Stroke);
  hands[1] := Seg(CenterX, CenterY, CenterX + handLen * 0.75,
    CenterY + handLen * 0.4, Stroke);
  DrawSmoothStrokes(Canvas, hands, Max(2, Size div 11));

  if Struck then
  begin
    // The slash is drawn twice: once in the fill color as a slightly wider
    // "cut", then the stroke on top, so it reads as a gap through the face
    // rather than a line lying over it.
    off := round(radius * 0.78);
    slash[0] := Seg(CenterX - off, CenterY + off, CenterX + off, CenterY - off,
      Fill);
    DrawSmoothStrokes(Canvas, slash, Max(4, Size div 5));
    slash[0].Color := Stroke;
    DrawSmoothStrokes(Canvas, slash, Max(2, Size div 10));
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
    // pattern already used in pbNextProgressPaint for overdue readings.
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
        DrawSmoothConvexPolygon(P.Canvas, chevPts,
          BlendColors(glyphFill, panelFillColor, 0.35));
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
        DrawSmoothConvexPolygon(P.Canvas, chevPts, accentColor);
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
  // Candidate spacings for the guide-line scale, per display unit, coarsening
  // left to right; the first one that clears the minimum gap wins. On a normal
  // window that lands on 5 mmol/L (the 5/10/15/20 ladder this always drew) and
  // on the mg/dL equivalent 100/200/300/400 -- the old code drew the mmol
  // values regardless of unit and labelled them "5".."20" on a mg/dL display.
  GUIDE_STEPS: array[BGUnit, 0..2] of single =
    ((2.5, 5, 10), (50, 100, 200));
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

  // "Show Guide Lines" (service menu): a light scale so a dot's height can be
  // read back as a value. Unlike the threshold bands this is a plain reading
  // aid, so it is drawn in the user's display unit, it survives high contrast
  // and range painting being off, and it is anchored on the dot *centre*
  // (ValueToY) rather than the dot bottom the threshold lines use.
  procedure PaintGuideLines;
  const
    LabelPad = 6;
  var
    lo, hi, step, v: single;
    pxPerUnit: double;
    k, y, tw, th, lineEnd, labelX, minGapPx: integer;
    txt: string;
    ink, lineCol, textCol: TColor;
  begin
    if (not DOT_LINES) or (clientH < 80) or (Self.ClientWidth < 40) then
      Exit;

    lo := BG_API_MIN * BG_CONVERTIONS[un][mmol];
    hi := BG_API_MAX * BG_CONVERTIONS[un][mmol];
    // ValueToY spreads the value range over the middle 80% of clientH.
    pxPerUnit := (clientH - 2 * Round(clientH * 0.1)) / (hi - lo);
    // At most ~8 lines, and never closer than 30px on a small window: this is
    // a reading aid, not graph paper.
    minGapPx := Max(30, clientH div 8);

    step := GUIDE_STEPS[un][High(GUIDE_STEPS[un])];
    for k := Low(GUIDE_STEPS[un]) to High(GUIDE_STEPS[un]) do
      if GUIDE_STEPS[un][k] * pxPerUnit >= minGapPx then
      begin
        step := GUIDE_STEPS[un][k];
        Break;
      end;

    // The form colour swings from a calm in-range tint to an alarm red, so the
    // ink is picked by luminance instead of tinting the background colour --
    // the old LightenColor(fBG.Color) guides washed out on the darker alarms.
    if IsLightColor(Self.Color) then
      ink := clBlack
    else
      ink := clWhite;
    lineCol := BlendColors(ink, Self.Color, 0.20);
    textCol := BlendColors(ink, Self.Color, 0.55);

    cnv.Brush.Style := bsClear;
    cnv.Pen.Style := psDot;
    cnv.Pen.Width := 1;
    cnv.Pen.Color := lineCol;
    cnv.Font.Style := [];
    // Negative Height is a pixel size, so the labels scale with the window
    // (and thus with DPI) instead of staying 8pt on a 4K kiosk screen.
    cnv.Font.Height := -Max(11, Round(clientH * 0.022));
    cnv.Font.Color := textCol;

    for k := Math.Ceil(lo / step) to Math.Floor(hi / step) do
    begin
      v := k * step;
      y := ValueToY(v * BG_CONVERTIONS[mmol][un]);
      if (y < 0) or (y >= clientH) then
        Continue;

      if Frac(step) = 0 then
        txt := Format('%.0f', [v])
      else
        txt := Format('%.1f', [v]);
      // Name the unit once, on the topmost line, so the scale is not just a
      // column of bare numbers.
      if k = Math.Floor(hi / step) then
        txt := txt + ' ' + BG_UNIT_NAMES[un];

      tw := cnv.TextWidth(txt);
      th := cnv.TextHeight(txt);
      labelX := Self.ClientWidth - tw - LabelPad;
      // Break the line before the label instead of striking through it.
      lineEnd := labelX - LabelPad;
      if lineEnd < Self.ClientWidth div 3 then
        lineEnd := Self.ClientWidth               // too narrow to label
      else
        cnv.TextOut(labelX, y - th div 2, txt);
      cnv.MoveTo(0, y);
      cnv.LineTo(lineEnd, y);
    end;
  end;

begin
  if not Assigned(Self) then
    Exit;

  cnv := Self.Canvas;
  // Nothing composited yet this pass; the band branch below fills this in.
  SetLength(FPaintedBands, 0);

  // Shutdown screen (ShowShutdownScreen): a plain splash-style backdrop with
  // the splash's green accent glow along the top — no bands, no guide lines.
  // The explicit fill also covers LCL-GTK3, where TForm.Color is ignored.
  if FShutdownScreen then
  begin
    cnv.Brush.Style := bsSolid;
    cnv.Brush.Color := Self.Color;
    cnv.FillRect(0, 0, Self.ClientWidth, Self.ClientHeight);
    // Classes.Rect: on Darwin, MacOSAll's QuickDraw Rect type shadows the
    // RTL function, turning a bare Rect(a,b,c,d) into a failed typecast.
    cnv.GradientFill(Classes.Rect(0, 0, Self.ClientWidth div 2, Scale96ToForm(3)),
      $00002214, $0000DC84, gdHorizontal);
    cnv.GradientFill(Classes.Rect(Self.ClientWidth div 2, 0, Self.ClientWidth,
      Scale96ToForm(3)), $0000DC84, $00002214, gdHorizontal);
    Exit;
  end;

  {$ifdef LCLGTK3}
  // LCL-GTK3 ignores TForm.Color: TGtk3Widget.SetColor's body sits behind an
  // `if IsWidgetOK and (0 > 1)` (gtk3widgets.pas), so the window keeps the GTK
  // theme background and the whole glucose-coloured backdrop never appears.
  // Everything below composites *onto* that backdrop -- the range bands are
  // alpha blends and the label inks come from GetTextColorForBackground(Color)
  // -- so without this fill they land on white and wash out. Must precede the
  // high-contrast early exit below, or unstyled mode stays white too.
  cnv.Brush.Style := bsSolid;
  cnv.Brush.Color := Self.Color;
  cnv.FillRect(0, 0, Self.ClientWidth, Self.ClientHeight);
  {$endif}

  // The dots render on the client-sized trend surface, so the form's own
  // client height is the value axis they share.
  clientH := Self.ClientHeight;

  // High contrast drops the range overlay, and the range overlay can be turned
  // off entirely -- but the guide lines are their own menu item, so they still
  // get drawn.
  if native.getboolsetting('main.high_contrast', false) or
    not (PaintRange or PaintRangeCGMRange) then
  begin
    PaintGuideLines;
    PaintDeltaPill(cnv);
    Exit;
  end;

  // The uniform history-dot diameter of the last layout pass; 0 until the
  // first pass has run.
  dotHeight := FTrendDotDiameter;

  // ValueToY returns the dot *center*; threshold lines and band edges need to
  // sit at the dot bottom so a dot whose value equals the threshold rests
  // flush against the line/band edge.
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
  // Kept for TrendBackdropColorAt: the child controls (the trend surface among
  // them) paint after this handler in the same pass, so what it reads is what
  // is on screen under them.
  FPaintedBands := Copy(bands);

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

  // Last, so the scale sits on top of the range bands rather than under them.
  PaintGuideLines;
  // And the pill over the scale: the delta is a reading, the scale is paper.
  PaintDeltaPill(cnv);
end;

// The delta pill behind lDiff: a capsule in the reading's ink, faintly, wide
// enough for the text and the direction chevron LayoutDeltaPill left room
// for. The chevron says which way the number points before it is read --
// up/down for any rising or falling trend, a dash for flat -- in the label's
// own colour, which DiffLabelColor already tints for a fast change. The label
// paints its text over this in the same pass (children paint after the form).
procedure TfBG.PaintDeltaPill(ACanvas: TCanvas);
var
  tw, th, chev, gap, textX, x0, x1, cy, padX, padY, pillH, ccx: integer;
  ink, backdrop, fill, glyph: TColor;
  showChev: boolean;
  capsule: array[0..0] of TSmoothStroke;
  pts: array[0..2] of TPoint;
begin
  if not (Assigned(lDiff) and lDiff.Visible and (lDiff.Caption <> '')) then
    Exit;
  if FShutdownScreen or native.GetBoolSetting('main.high_contrast', false) then
    Exit;

  ACanvas.Font.Assign(lDiff.Font);
  tw := ACanvas.TextWidth(lDiff.Caption);
  th := ACanvas.TextHeight(lDiff.Caption);
  if (tw <= 0) or (th <= 0) then
    Exit;

  showChev := DeltaChevronShown;
  chev := Round(th * DELTA_CHEVRON_FRAC);
  gap := Max(3, th div 4);

  // Where the label puts its single centred line (TCustomLabel.Paint).
  case lDiff.Alignment of
  taLeftJustify:
    textX := lDiff.Left;
  taRightJustify:
    textX := lDiff.Left + lDiff.Width - tw;
  else
    textX := lDiff.Left + (lDiff.Width - tw) div 2;
  end;
  cy := lDiff.Top + lDiff.Height div 2;

  x0 := textX;
  if showChev then
    x0 := x0 - gap - chev;
  x1 := textX + tw;
  padX := Round(th * DELTA_PILL_PAD_X);
  padY := Round(th * DELTA_PILL_PAD_Y);
  pillH := th + 2 * padY;

  // Tinted from what is actually under it (band included), so the pill reads
  // as a lift of the surface rather than a patch laid on it.
  ink := lVal.Font.Color;
  backdrop := TrendBackdropColorAt(cy);
  fill := BlendColors(ink, backdrop, DELTA_PILL_TINT);

  // One capsule stroke as thick as the pill: its round caps are the pill's
  // ends, so the endpoints sit half a height inside the padded extent.
  capsule[0].X1 := x0 - padX + pillH / 2;
  capsule[0].X2 := Max(capsule[0].X1, x1 + padX - pillH / 2);
  capsule[0].Y1 := cy;
  capsule[0].Y2 := cy;
  capsule[0].Color := fill;
  DrawSmoothStrokes(ACanvas, capsule, pillH);

  if not showChev then
    Exit;
  glyph := lDiff.Font.Color;
  ccx := x0 + chev div 2;
  case CalculateTrendFromDelta(FDiffRateMgdl) of
  TdDoubleUp, TdSingleUp, TdFortyFiveUp:
  begin
    pts[0] := MkPoint(ccx, cy - chev div 2);
    pts[1] := MkPoint(ccx - chev div 2, cy + chev div 2);
    pts[2] := MkPoint(ccx + chev div 2, cy + chev div 2);
    DrawSmoothConvexPolygon(ACanvas, pts, glyph);
  end;
  TdFortyFiveDown, TdSingleDown, TdDoubleDown:
  begin
    pts[0] := MkPoint(ccx - chev div 2, cy - chev div 2);
    pts[1] := MkPoint(ccx + chev div 2, cy - chev div 2);
    pts[2] := MkPoint(ccx, cy + chev div 2);
    DrawSmoothConvexPolygon(ACanvas, pts, glyph);
  end;
  else
  begin
    // Flat: a short dash on the text's mid-line.
    capsule[0].X1 := ccx - chev / 2;
    capsule[0].X2 := ccx + chev / 2;
    capsule[0].Color := glyph;
    DrawSmoothStrokes(ACanvas, capsule, Max(2, chev div 4));
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

// Shared state computation for the progress bar: the paint handler needs it
// to draw, tProgressTimer needs it (via ProgressFrameChanged) to decide whether
// drawing would change anything, and StartProgressDrain needs it to know the
// level a fresh reading resets the bar to.
//
// The bar is two parallel lines on a shared time scale of one refresh cycle
// each: the primary line fills over the cycle the scheduler targets (full
// exactly when the next fetch fires), then the overtime line fills over a
// second cycle-length — the retry window. Both are strictly monotone; v1's
// stretched-scale-plus-marker rework of the level on every reschedule read as
// the gauge running backwards.
procedure TfBG.ComputeProgressState(out AFrac1, AFrac2: double;
  out APulsing, ADraining: boolean);
const
  DRAIN_MS = 400;             // length of the reading-arrival drain animation
var
  elapsedMs, spanMs: int64;
  total, t: double;
begin
  ADraining := false;

  // Signed on purpose: MilliSecondsBetween is Abs()-based, which turns a
  // future-dated reading (uploader clock ahead of ours — the same skew
  // TrndiAPI.timeDiff exists for) into a large positive "age" and pegs the bar
  // full. Subtracting directly keeps the sign so the clamp below can catch it.
  elapsedMs := Round((Now - lastReading.date) * MSecsPerDay);
  if elapsedMs < 0 then
    elapsedMs := 0;

  // Count the cycle the scheduler *targets*: BG_REFRESH plus the resync padding
  // SetNextUpdateTimer adds to give the CGM time to report. A flat BG_REFRESH
  // leaves the primary line pegged full through that padding every cycle.
  spanMs := BG_REFRESH + REFRESH_RESYNC_BUFFER_MS;

  // Combined progress on a 0..2 scale: 0..1 is the primary line, 1..2 the
  // overtime line. Once both are full the bar breathes until a reading lands —
  // no extra grace: the whole second line *was* the grace.
  total := Min(2.0, elapsedMs / spanMs);
  APulsing := total >= 2.0;

  // Reading-arrival drain: instead of snapping from full to empty between two
  // frames, ease from the level last painted down to the fresh level. The
  // eased fraction converges on the real one, so the animation can never
  // leave the bar out of sync with the schedule. Run on the combined scale,
  // the drain empties the overtime line first, then the primary — the fill
  // order in reverse.
  if FProgressDrainFrom > 0 then
  begin
    t := (GetTickCount64 - FProgressDrainStart) / DRAIN_MS;
    if t >= 1.0 then
      FProgressDrainFrom := 0
    else
    begin
      t := 1 - (1 - t) * (1 - t) * (1 - t);   // ease-out cubic
      total := FProgressDrainFrom + (total - FProgressDrainFrom) * t;
      ADraining := true;
    end;
  end;

  AFrac1 := Min(1.0, total);
  AFrac2 := Max(0.0, total - 1.0);
end;

// Fill colour: teal for most of the cycle → amber as the reading falls due →
// pomegranate once it is late. The teal stage only brightens as it rises;
// blending teal straight into amber across the whole cycle would spend
// minutes in the muddy olive midpoint the two colours share.
function TfBG.ProgressFillColor(const AFrac: double; const APulsing: boolean): TColor;
const
  WARM_FROM = 0.90;     // fraction at which the fill starts warming toward amber
  PULSE_MS  = 1600;     // one full breath while the reading is overdue
var
  warmth, pulse: double;
begin
  if APulsing then
  begin
    // Breathe rather than flash: tProgressTimer speeds up while a reading is
    // overdue, so this ramp is sampled often enough to read as continuous.
    pulse := 0.5 - 0.5 * cos(2 * Pi * ((GetTickCount64 mod PULSE_MS) / PULSE_MS));
    Result := BlendColors(RGBToColor(255, 105, 66), RGBToColor(183, 47, 36), pulse);
  end
  else if AFrac >= WARM_FROM then
  begin
    warmth := (AFrac - WARM_FROM) / (1.0 - WARM_FROM);
    warmth := warmth * warmth * (3 - 2 * warmth);   // ease through the mixed shades
    Result := BlendColors(RGBToColor(230, 126, 34), RGBToColor(42, 182, 166), warmth);
  end
  else
    Result := BlendColors(RGBToColor(42, 182, 166), RGBToColor(34, 138, 138),
      AFrac / WARM_FROM);
end;

// Colour of the overtime line — the retry window. Kept in the overdue palette
// so it reads as "the late part", distinct from the primary line's cycle
// colours. Steady mid-tone while it is still filling, the breathing blend once
// both lines are full (the pulse ramp is the same one ProgressFillColor uses).
function TfBG.ProgressOvertimeColor(const APulsing: boolean): TColor;
begin
  if APulsing then
    Result := ProgressFillColor(1.0, true)
  else
    Result := BlendColors(RGBToColor(255, 105, 66), RGBToColor(183, 47, 36), 0.5);
end;

// Whether repainting the bar now would produce different pixels than the
// frame pbNextProgressPaint last recorded. At 1 Hz the fill often moves less
// than a pixel between ticks, so tProgressTimer uses this to skip repaints
// that would reproduce the same image. Pulse and drain frames always differ.
function TfBG.ProgressFrameChanged: boolean;
var
  frac1, frac2: double;
  pulsing, draining: boolean;
  span: integer;
begin
  if (bgs = nil) or (Length(bgs) = 0) then
    Exit(false);
  // A resize since the last full frame means what is on screen may be partial:
  // widgetsets only redraw the newly exposed region when a control grows, so
  // pixels from the old scale can survive next to the new ones (the boot-time
  // layout does several such resizes). Force one full redraw per size. Recorded
  // here and not in the paint handler — a system-triggered partial paint runs
  // the same handler, and must not count as having covered the new size.
  if (pbNextProgress.Width <> FProgressPaintedW) or
     (pbNextProgress.Height <> FProgressPaintedH) then
  begin
    FProgressPaintedW := pbNextProgress.Width;
    FProgressPaintedH := pbNextProgress.Height;
    Exit(true);
  end;
  ComputeProgressState(frac1, frac2, pulsing, draining);
  if pulsing or draining then
    Exit(true);
  // Same quantisation the paint records, so equal values mean equal frames.
  span := pbNextProgress.Height - 2 * Max(1, pbNextProgress.Width div 10);
  Result := (Round(frac1 * span) <> FProgressPaintedLvl1) or
    (Round(frac2 * span) <> FProgressPaintedLvl2) or
    (ProgressFillColor(frac1, false) <> FProgressPaintedColor);
end;

// Called from SetNextUpdateTimer when a fresh reading has landed: arm the
// drain animation from the level the bar last showed. Skipped when the drop
// is too small to read as motion (a re-applied unchanged reading, or a bar
// that was nearly empty anyway).
procedure TfBG.StartProgressDrain;
var
  frac1, frac2: double;
  pulsing, draining: boolean;
begin
  if not (Assigned(pbNextProgress) and pbNextProgress.Visible) then
    Exit;
  FProgressDrainFrom := 0;   // measure the fresh level, not a still-running drain
  ComputeProgressState(frac1, frac2, pulsing, draining);
  if FProgressPaintedFrac < frac1 + frac2 + 0.05 then
    Exit;
  FProgressDrainFrom := FProgressPaintedFrac;
  FProgressDrainStart := GetTickCount64;
  // Queue the first frame rather than painting synchronously — this runs mid
  // fetch-apply, before DeferredPostFetchResize has laid the panel out, and a
  // forced paint at stale geometry would be wasted. The fast interval keeps
  // the frames coming even when the timer is enabled slightly later on the
  // fetch path (FinalizeUpdate).
  if Assigned(tProgress) then
    tProgress.Interval := PROGRESS_DRAIN_TICK_MS;
  pbNextProgress.Invalidate;
end;

// Paint handler for the left-side "next reading" progress indicator.
//
// Two parallel vertical lines on recessed tracks, both filling bottom-up:
// the wider primary line covers one refresh cycle (full exactly when the
// next fetch fires), the slimmer overtime line beside it covers the retry
// window after that. The overtime line is only drawn once it holds some fill —
// during a normal cycle a permanently visible empty second channel just begs
// the question of what it is for; its slot stays reserved so nothing shifts
// when it appears. Flat FillRects only — the lines are a few pixels wide, so
// any capsule/shading treatment just reads as noise at this size.
procedure TfBG.pbNextProgressPaint(Sender: TObject);
var
  box: TPaintBox;
  cvs: TCanvas;
  bgCol, trackCol, fillCol, overCol: TColor;
  frac1, frac2: double;
  pulsing, draining: boolean;
  m, w, h, span, iw, w1, w2: integer;

  // One line: recessed track over the full span, fill from the bottom.
  procedure DrawLine(x0, lineW: integer; const frac: double; col: TColor);
  var
    lvl: integer;
  begin
    cvs.Brush.Color := trackCol;
    cvs.FillRect(Classes.Rect(x0, m, x0 + lineW, h - m));
    lvl := Round(frac * span);
    if lvl > 0 then
    begin
      cvs.Brush.Color := col;
      cvs.FillRect(Classes.Rect(x0, h - m - lvl, x0 + lineW, h - m));
    end;
  end;

begin
  box := Sender as TPaintBox;
  cvs := box.Canvas;
  w := box.Width;
  h := box.Height;
  bgCol := fBG.Color;

  // The box is painted edge to edge every time — the lines are drawn onto the
  // window colour, so nothing of the previous frame can survive around them.
  cvs.Brush.Style := bsSolid;
  cvs.Pen.Style := psClear;
  cvs.Brush.Color := bgCol;
  cvs.FillRect(Classes.Rect(0, 0, w, h));

  if (bgs = nil) or (Length(bgs) = 0) then
    Exit;

  m := Max(1, w div 10);          // margin, so the lines never touch the box edge
  iw := w - 2 * m;                // inner width shared by both lines
  span := h - 2 * m;              // full-fill height of either line
  if (iw < 3) or (span < 8) then
    Exit;
  // The lines sit flush against each other — they describe one continuous
  // wait, so they should read as one unit. While the overtime line shows, the
  // primary is always pegged full (amber/red against the recessed track), so
  // the colours keep the two legible without a gap. The overtime line is a
  // thin accent stroke beside the bar, not a second bar: overtime is the
  // exception, and its width should say so.
  w2 := Max(1, iw div 5);
  w1 := iw - w2;

  // Elapsed/span/drain arithmetic lives in ComputeProgressState so the
  // timer's skip check and the drain trigger see exactly what will be painted.
  ComputeProgressState(frac1, frac2, pulsing, draining);
  FProgressPulsing := pulsing;

  // Track: a recessed channel, darker than the window on light backgrounds and
  // lighter on dark ones, so the empty part of a line stays visible either way.
  if IsLightColor(bgCol) then
    trackCol := BlendColors(clBlack, bgCol, 0.16)
  else
    trackCol := BlendColors(clWhite, bgCol, 0.10);

  // Primary carries the cycle colours (teal → amber as the reading falls due);
  // the overtime line carries the retry red. Once both are full the same
  // breathing blend takes both over, which is the whole "overdue" signal.
  fillCol := ProgressFillColor(frac1, pulsing);
  overCol := ProgressOvertimeColor(pulsing);

  DrawLine(m, w1, frac1, fillCol);
  // Same quantisation as the recorded level, so visibility and the frame-skip
  // gate agree on when the line winks in and out.
  if Round(frac2 * span) > 0 then
    DrawLine(m + w1, w2, frac2, overCol);

  // Record what this frame showed: ProgressFrameChanged compares against these
  // to skip identical repaints, and StartProgressDrain animates from the total.
  FProgressPaintedFrac := frac1 + frac2;
  FProgressPaintedLvl1 := Round(frac1 * span);
  FProgressPaintedLvl2 := Round(frac2 * span);
  FProgressPaintedColor := ProgressFillColor(frac1, false);
end;
