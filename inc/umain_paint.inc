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
  halfSize, barW, barH, barX, barTopY, dotY: integer;
begin
  if Size < 8 then
    Size := 8;
  halfSize := Size div 2;

  // Equilateral-ish triangle, flat base
  pts[0] := Point(CenterX, CenterY - halfSize);
  pts[1] := Point(CenterX - halfSize, CenterY + halfSize);
  pts[2] := Point(CenterX + halfSize, CenterY + halfSize);

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
      // Watermark behind the caption: large, low-contrast triangle
      glyphSize := Max(48, Min(P.Width, P.Height) div 4);
      glyphX := P.Width div 2;
      glyphY := glyphSize div 2 + 16;
      glyphFill := BlendColors(clWhite, panelFillColor, 0.55);
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
          chevPts[0] := Point(chevronCX, chevronCY - chevronS div 2);
          chevPts[1] := Point(chevronCX - chevronS, chevronCY + chevronS div 2);
          chevPts[2] := Point(chevronCX + chevronS, chevronCY + chevronS div 2);
        end
        else
        begin
          // Down chevron (▾)
          chevPts[0] := Point(chevronCX - chevronS, chevronCY - chevronS div 2);
          chevPts[1] := Point(chevronCX + chevronS, chevronCY - chevronS div 2);
          chevPts[2] := Point(chevronCX, chevronCY + chevronS div 2);
        end;
        Brush.Color := accentColor;
        Pen.Style := psClear;
        Polygon(chevPts);
      end;
    end;
  end;
end;

procedure TfBG.FormPaint(Sender: TObject);
var
  loY, hiY, rangeLoY, rangeHiY: integer;
  cnv: TCanvas;
  lineColor: TColor;
  drawLo, drawHi, drawRangeLo, drawRangeHi: boolean;
  tmp: integer;
  clientH: integer;
  dotHeight: integer;

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
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) and
    Assigned(TrendDots[1].Parent) then
    clientH := TrendDots[1].Parent.ClientHeight
  else
    clientH := Self.ClientHeight;

  dotHeight := 0;
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) then
    // Drawn dots are sized as a square in ResizeDot; the control's Height
    // *is* the diameter, so we can read it directly.
    dotHeight := Max(TrendDots[1].Height, TrendDots[1].Font.Size);

  drawLo := (Assigned(api) and (api.cgmLo <> 0) and PaintRange);
  drawHi := (Assigned(api) and (api.cgmHi <> 0) and PaintRange);
  drawRangeLo := (Assigned(api) and (api.cgmRangeLo <> 0) and PaintRangeCGMRange);
  drawRangeHi := (Assigned(api) and (api.cgmRangeHi > 0) and (api.cgmRangeHi < 500) and PaintRangeCGMRange);

  if drawLo then
    loY := ValueToY(api.cgmLo * BG_CONVERTIONS[mmol][mgdl]);

  if drawHi then
    hiY := ValueToY(api.cgmHi * BG_CONVERTIONS[mmol][mgdl]);

  if drawRangeLo then
    rangeLoY := ValueToY(api.cgmRangeLo * BG_CONVERTIONS[mmol][mgdl]);

  if drawRangeHi then
    rangeHiY := ValueToY(api.cgmRangeHi * BG_CONVERTIONS[mmol][mgdl]);

  if drawLo and drawHi then
  begin
    cnv.Brush.Style := bsSolid;
    if tir_bg = -1 then
      cnv.Brush.Color := DarkenColor(fBG.Color, 0.9)
    else
      cnv.Brush.Color := BlendColors(tir_bg, fBG.Color, 0.3);
    cnv.Pen.Style := psClear;
    cnv.FillRect(Classes.Rect(0, hiY, Self.ClientWidth, loY));
  end;

  if drawRangeLo and drawRangeHi then
  begin
    cnv.Brush.Style := bsSolid;
    if tir_custom_bg = -1 then
      cnv.Brush.Color := DarkenColor(fBG.Color, 0.85)
    else
    if tir_bg = -1 then
      cnv.Brush.Color := BlendColors(tir_custom_bg, fBG.Color, 0.3)
    else
      cnv.Brush.Color := BlendColors(tir_custom_bg, BlendColors(tir_bg, fBG.Color, 0.3), 0.3);
    cnv.Pen.Style := psClear;
    cnv.FillRect(Classes.Rect(0, rangeHiY, Self.ClientWidth, rangeLoY));
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
      tmp := loY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    if drawHi then
    begin
      lineColor := LightenColor(bg_rel_color_hi);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 2;
      tmp := hiY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    if drawRangeLo then
    begin
      lineColor := LightenColor(fBG.Color, 0.5);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 1;
      tmp := rangeLoY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    if drawRangeHi then
    begin
      lineColor := LightenColor(fBG.Color, 0.5);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 1;
      tmp := rangeHiY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
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
        debugY := ValueToY(debugValue) + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
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
