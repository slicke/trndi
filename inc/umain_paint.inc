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
// of a hard rectangle. Reuses the LocalAlphaBlend import from
// inc/umain_dots.inc on Windows; uses a TPortableNetworkGraphic on
// GTK/Qt/Cocoa where TBitmap alpha compositing is unreliable.
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
  rawDesc: TRawImageDescription;
  y, i: integer;
  br, bg, bb: byte;
  dstR, dstG, dstB, dstA: double;
  srcA, invA: double;
  pix: TFPColor;
  outR, outG, outB, outA: integer;
  {$ifdef Windows}
  bmp: TBitmap;
  blend: TLocalBlendFunction;
  {$else}
  png: TPortableNetworkGraphic;
  {$endif}
begin
  if (AWidth <= 0) or (AHeight <= 0) or (Length(Bands) = 0) then
    Exit;

  intf := TLazIntfImage.Create(0, 0);
  try
    rawDesc.Init_BPP32_B8G8R8A8_BIO_TTB(1, AHeight);
    intf.DataDescription := rawDesc;
    intf.CreateData;

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

      if dstA > 1 then dstA := 1
      else if dstA < 0 then dstA := 0;

      // dstR/dstG/dstB are accumulated in premultiplied form already
      // (Porter-Duff: dstR = br*srcA + dstR*invA — the srcA factor is baked
      // in, no division by aOut like DrawSmoothCircle's straight-form loop).
      {$ifdef Windows}
      // Win32 AlphaBlend with AC_SRC_ALPHA expects premultiplied RGB — write
      // dstR directly. Multiplying by dstA again would darken twice.
      outR := Round(dstR);
      outG := Round(dstG);
      outB := Round(dstB);
      {$else}
      // PNG / cross-platform draw expects straight (non-premultiplied) RGB,
      // so divide the premultiplied accumulator by dstA to recover the
      // original colour. Guard dstA = 0 (fully transparent row).
      if dstA > 0 then
      begin
        outR := Round(dstR / dstA);
        outG := Round(dstG / dstA);
        outB := Round(dstB / dstA);
      end
      else
      begin
        outR := 0;
        outG := 0;
        outB := 0;
      end;
      {$endif}
      outA := Round(dstA * 255);

      if outR > 255 then outR := 255 else if outR < 0 then outR := 0;
      if outG > 255 then outG := 255 else if outG < 0 then outG := 0;
      if outB > 255 then outB := 255 else if outB < 0 then outB := 0;
      if outA > 255 then outA := 255 else if outA < 0 then outA := 0;

      pix.red := outR shl 8 or outR;
      pix.green := outG shl 8 or outG;
      pix.blue := outB shl 8 or outB;
      pix.alpha := outA shl 8 or outA;
      intf.Colors[0, y] := pix;
    end;

    {$ifdef Windows}
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32bit;
      bmp.SetSize(1, AHeight);
      bmp.LoadFromIntfImage(intf);
      blend.BlendOp := LOCAL_AC_SRC_OVER;
      blend.BlendFlags := 0;
      blend.SourceConstantAlpha := 255;
      blend.AlphaFormat := LOCAL_AC_SRC_ALPHA;
      LocalAlphaBlend(ACanvas.Handle, 0, 0, AWidth, AHeight,
                      bmp.Canvas.Handle, 0, 0, 1, AHeight, blend);
    finally
      bmp.Free;
    end;
    {$else}
    png := TPortableNetworkGraphic.Create;
    try
      png.LoadFromIntfImage(intf);
      ACanvas.StretchDraw(Classes.Rect(0, 0, AWidth, AHeight), png);
    finally
      png.Free;
    end;
    {$endif}
  finally
    intf.Free;
  end;
end;

procedure TfBG.pnWarningPaint(Sender: TObject);
const
  RadiusFull = 20;
  RadiusBanner = 14;
var
  P: TPanel;
  panelFillColor, strokeColor, accentColor, glyphFill: TColor;
  tintColor: TColor;
  opacityTmp: byte;
  isFullCover: boolean;
  alphaRatio: double;
  radius: integer;
  glyphSize, glyphX, glyphY: integer;
  chevronCX, chevronCY, chevronS: integer;
  chevPts: array[0..2] of TPoint;
  pulseAmt: double;
  bannerBaseH: integer;
begin
  P := TPanel(Sender);
  GetWarnSeverityVisual(FWarnSeverity, tintColor, opacityTmp, isFullCover);

  if isFullCover then
    radius := RadiusFull
  else
    radius := RadiusBanner;

  with P.Canvas do
  begin
    Brush.Color := fBG.Color;
    Brush.Style := bsSolid;
    FillRect(0, 0, P.Width, P.Height);

    {$ifndef X_WIN}
    alphaRatio := opacityTmp / 255.0;
    panelFillColor := BlendColors(tintColor, fBG.Color, alphaRatio);
    {$else}
    panelFillColor := tintColor;
    alphaRatio := 1.0;
    {$endif}

    // For wsSoon, pulse the tint between base orange and a brighter shade on a
    // 2-second cadence. Driven by tProgressTimer; mirrors the alternation
    // pattern already used in pnNextProgressPaint for overdue readings.
    if (FWarnSeverity = wsSoon) and not isFullCover then
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

    // Vector severity glyph
    accentColor := DarkenColor(panelFillColor, 0.45);
    glyphFill := BlendColors(clWhite, panelFillColor, 0.85);
    if isFullCover then
    begin
      // Watermark behind the caption: large, low-contrast triangle.
      // Heavy bias toward the panel color so it stays a subtle watermark and
      // doesn't reduce contrast with the caption text drawn on top.
      glyphSize := Max(48, Min(P.Width, P.Height) div 4);
      glyphX := P.Width div 2;
      glyphY := glyphSize div 2 + 16;
      glyphFill := BlendColors(clWhite, panelFillColor, 0.80);
      DrawWarningGlyph(P.Canvas, glyphX, glyphY, glyphSize,
        glyphFill, accentColor, accentColor);
    end
    else
    begin
      // Banner: glyph and chevron live within the collapsed (base) area, so they
      // don't move when the expanded detail row appears below.
      bannerBaseH := FWarnBannerBaseH;
      if bannerBaseH <= 0 then
        bannerBaseH := P.Height;

      glyphSize := Max(20, bannerBaseH - 20);
      glyphX := 12 + glyphSize div 2;
      glyphY := bannerBaseH div 2;
      DrawWarningGlyph(P.Canvas, glyphX, glyphY, glyphSize,
        glyphFill, accentColor, accentColor);

      // Expand chevron on the right (only when there's something to expand to)
      if WarnShowDetails then
      begin
        chevronS := 6;
        chevronCX := P.Width - 16;
        chevronCY := bannerBaseH div 2;
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

  drawLo := (Assigned(api) and (api.cgmLo <> 0) and PaintRange);
  drawHi := (Assigned(api) and (api.cgmHi <> 0) and PaintRange);
  drawRangeLo := (Assigned(api) and (api.cgmRangeLo <> 0) and PaintRangeCGMRange);
  drawRangeHi := (Assigned(api) and (api.cgmRangeHi > 0) and (api.cgmRangeHi < 500) and PaintRangeCGMRange);

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
        debugY := ValueToY(debugValue) + rowOffset;
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

// Paint handler for the left-side "next reading" progress indicator
procedure TfBG.pnNextProgressPaint(Sender: TObject);
var
  pnl: TPanel;
  elapsedMs, overdueMs: int64;
  frac: double;
  fillR: TRect;
  fillCol, trackCol: TColor;
  cr, fillH, y: integer;
  isOverdue: boolean;
  t: double;
begin
  pnl := Sender as TPanel;
  if (bgs = nil) or (Length(bgs) = 0) then
    Exit;

  elapsedMs := MilliSecondsBetween(Now, lastReading.date);
  if elapsedMs < 0 then
    elapsedMs := 0;
  if BG_REFRESH > 0 then
    frac := Min(1.0, elapsedMs / BG_REFRESH)
  else
    frac := 1.0;
  overdueMs := BG_REFRESH + 60000;
  isOverdue := elapsedMs > overdueMs;

  // Track: dark recessed pill channel
  trackCol := BlendColors(fBG.Color, clBlack, 0.72);

  // Fill: teal (fresh reading) → amber (reading due) → pomegranate (overdue, pulsing)
  // Colors mirror the modern theme palette so the bar feels consistent.
  if isOverdue then
  begin
    // When past 6 min: pulse the entire bar between deep pomegranate and a
    // bright fiery red-orange to grab attention without text/badges.
    if (SecondOf(Now) mod 2 = 0) then
      fillCol := RGBToColor(192, 57, 43)     // pomegranate
    else
      fillCol := RGBToColor(255, 110, 70);   // bright alert red-orange
  end
  else if frac >= 0.8 then
    fillCol := RGBToColor(230, 126, 34)   // pumpkin      — matches bg_color_hi
  else
    fillCol := BlendColors(
      RGBToColor(230, 126, 34),  // amber (reached at frac = 0.8)
      RGBToColor(46, 150, 136),  // teal  (at frac = 0, just after a reading)
      frac / 0.8
    );

  // Capsule radius: half the panel width gives a perfect pill shape
  cr := Max(3, pnl.Width div 2);

  // Step 1 — draw the full-panel track as a dark pill
  with pnl.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := trackCol;
    Pen.Style := psClear;
    RoundRect(0, 0, pnl.Width, pnl.Height, cr * 2, cr * 2);
  end;

  if frac <= 0 then
    Exit;

  // Step 2 — fill: flat-top rectangle with vertical gradient (grows upward from the bottom)
  fillH := Max(1, Round(frac * (pnl.Height - 2)));
  fillR := Classes.Rect(1, pnl.Height - 1 - fillH, pnl.Width - 1, pnl.Height - 1);

  with pnl.Canvas do
  begin
    Pen.Style := psClear;
    Brush.Style := bsSolid;
    for y := fillR.Top to fillR.Bottom - 1 do
    begin
      t := (y - fillR.Top) / Max(1.0, double(fillR.Bottom - 1 - fillR.Top));
      Brush.Color := BlendColors(fillCol, LightenColor(fillCol, 0.18), t);
      FillRect(Classes.Rect(fillR.Left, y, fillR.Right, y + 1));
    end;
  end;

  // Step 3 — edge highlights: bright left strip (specular), subtle right shadow
  with pnl.Canvas do
  begin
    Pen.Style := psClear;
    Brush.Color := LightenColor(fillCol, 0.40);
    FillRect(Classes.Rect(fillR.Left, fillR.Top, fillR.Left + 1, fillR.Bottom));
    Brush.Color := DarkenColor(fillCol, 0.12);
    FillRect(Classes.Rect(fillR.Right - 1, fillR.Top, fillR.Right, fillR.Bottom));
  end;

end;
