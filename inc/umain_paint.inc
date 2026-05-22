(* Paint event handlers for TfBG panels and the main form.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

procedure TfBG.pnWarningPaint(Sender: TObject);
const
  radius = 20;
var
  P: TPanel;
  panelFillColor: TColor;
  {$ifndef X_WIN}
  alphaRatio: double;
  {$endif}
begin
  P := TPanel(Sender);

  with P.Canvas do
  begin
    Brush.Color := fBG.Color;
    FillRect(0, 0, P.Width, P.Height);

    {$ifndef X_WIN}
    alphaRatio := LAST_WARN_OPACITY / 255.0;
    panelFillColor := BlendColors(P.Color, fBG.Color, alphaRatio);
    Brush.Color := panelFillColor;
    {$else}
    panelFillColor := P.Color;
    Brush.Color := panelFillColor;
    {$endif}
    Pen.Color := BlendColors(panelFillColor, fBG.Color, 0.45);
    Pen.Width := 1;
    RoundRect(0, 0, P.Width, P.Height, Radius, Radius);

    Pen.Color := LightenColor(panelFillColor, 0.2);
    MoveTo(Radius div 2, 2);
    LineTo(P.Width - (Radius div 2), 2);

    {$ifndef X_WIN}
    PaintWarningOverlay(P, Radius, Brush.Color, alphaRatio);
    {$endif}
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
  begin
    TrendDots[1].Canvas.Font.Assign(TrendDots[1].Font);
    dotHeight := Max(TrendDots[1].Canvas.TextHeight(DOT_GRAPH), TrendDots[1].Font.Size);
  end;

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
  fillCol, trackCol, edgeCol: TColor;
  cr, fillH: integer;
  isOverdue: boolean;
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

  // Fill: teal (fresh reading) → amber (reading due) → pomegranate (overdue)
  // Colors mirror the modern theme palette so the bar feels consistent.
  if isOverdue then
    fillCol := RGBToColor(192, 57, 43)    // pomegranate  — matches bg_color_lo
  else if frac >= 0.8 then
    fillCol := RGBToColor(230, 126, 34)   // pumpkin      — matches bg_color_hi
  else
    fillCol := BlendColors(
      RGBToColor(230, 126, 34),  // amber (reached at frac = 0.8)
      RGBToColor(46, 150, 136),  // teal  (at frac = 0, just after a reading)
      frac / 0.8
    );

  // 1px lighter right-edge highlight for depth
  edgeCol := LightenColor(fillCol, 0.30);

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

  // Step 2 — compute fill rectangle (grows upward from the bottom)
  fillH := Max(cr * 2, Round(frac * (pnl.Height - 2)));
  fillR := Classes.Rect(1, pnl.Height - 1 - fillH, pnl.Width - 1, pnl.Height - 1);

  with pnl.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := fillCol;
    Pen.Style := psClear;

    // Rounded cap at the active (top) tip
    RoundRect(fillR.Left, fillR.Top, fillR.Right, fillR.Top + cr * 2, cr * 2, cr * 2);

    // Rectangular body from below the cap down to the base
    if fillH > cr * 2 then
      FillRect(Classes.Rect(fillR.Left, fillR.Top + cr, fillR.Right, fillR.Bottom));
  end;

  // Step 3 — 1px lighter strip on the right edge of the body (skip the cap area)
  with pnl.Canvas do
  begin
    Brush.Color := edgeCol;
    Pen.Style := psClear;
    if fillH > cr * 3 then
      FillRect(Classes.Rect(fillR.Right - 1, fillR.Top + cr * 2, fillR.Right, fillR.Bottom));
  end;
end;
