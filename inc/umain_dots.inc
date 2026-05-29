(* Trend dot creation, layout, painting and interaction for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

// DrawSmoothCircle: Antialiased disc (with optional outer ring) for the
// drawn-dot mode. GDI's TCanvas.Ellipse is not antialiased even with
// AntialiasingMode := amOn — it calls the standard Ellipse() API which is
// strictly aliased.
//
// Approach: build a 32-bit premultiplied-alpha bitmap with the disc rendered
// at its final size using analytical pixel coverage (`r + 0.5 - distance`,
// clamped to [0,1] at the boundary). Then ask the OS to alpha-blend it over
// the destination — Windows AlphaBlend reads the destination pixels directly
// so we don't need to capture the underlying background ourselves (which was
// unreliable on a TGraphicControl's canvas during the initial paint and
// during window drag).
{$ifdef Windows}
type
  // Local copy of the Win32 BLENDFUNCTION struct + msimg32!AlphaBlend import.
  // We avoid `uses Windows` because Windows.TBitmap (the GDI BITMAP record)
  // would shadow Graphics.TBitmap in the rest of this implementation file.
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

procedure DrawSmoothCircle(ACanvas: TCanvas; ASize: integer;
  ADotColor, ARingColor: TColor; ARingWidth: integer);
var
  intf: TLazIntfImage;
  rawDesc: TRawImageDescription;
  x, y: integer;
  cx, cy, r, dist: double;
  ringOuter, ringInner: double;
  alphaDisc, alphaRing: double;
  fr, fg, fb: byte;
  rr, rg, rb: byte;
  pix: TFPColor;
  invRing, aOut: double;
  straightR, straightG, straightB: double;
  outR, outG, outB, outA: integer;
  hasRing: boolean;
  {$ifdef Windows}
  bmp: TBitmap;
  blend: TLocalBlendFunction;
  {$else}
  png: TPortableNetworkGraphic;
  {$endif}
begin
  if ASize < 2 then
    Exit;

  fr := Red(ColorToRGB(ADotColor));
  fg := Green(ColorToRGB(ADotColor));
  fb := Blue(ColorToRGB(ADotColor));
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

  intf := TLazIntfImage.Create(0, 0);
  try
    rawDesc.Init_BPP32_B8G8R8A8_BIO_TTB(ASize, ASize);
    intf.DataDescription := rawDesc;
    intf.CreateData;

    for y := 0 to ASize - 1 do
    begin
      for x := 0 to ASize - 1 do
      begin
        dist := Sqrt(Sqr(x - cx) + Sqr(y - cy));

        if dist <= r - 0.5 then
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

        // Porter-Duff "ring over disc over transparent", written in
        // straight-alpha form. Premultiplied conversion (if needed) happens
        // when we hand the data to the OS below.
        invRing := 1.0 - alphaRing;
        aOut := alphaRing + alphaDisc * invRing;

        if aOut > 0 then
        begin
          straightR := (rr * alphaRing + fr * alphaDisc * invRing) / aOut;
          straightG := (rg * alphaRing + fg * alphaDisc * invRing) / aOut;
          straightB := (rb * alphaRing + fb * alphaDisc * invRing) / aOut;
        end
        else
        begin
          straightR := 0; straightG := 0; straightB := 0;
        end;

        {$ifdef Windows}
        // Win32 AlphaBlend with AC_SRC_ALPHA expects premultiplied RGB.
        outR := Round(straightR * aOut);
        outG := Round(straightG * aOut);
        outB := Round(straightB * aOut);
        {$else}
        // PNG / cross-platform draw expects straight (non-premultiplied) RGB.
        outR := Round(straightR);
        outG := Round(straightG);
        outB := Round(straightB);
        {$endif}
        outA := Round(aOut * 255);

        if outR > 255 then outR := 255 else if outR < 0 then outR := 0;
        if outG > 255 then outG := 255 else if outG < 0 then outG := 0;
        if outB > 255 then outB := 255 else if outB < 0 then outB := 0;
        if outA > 255 then outA := 255 else if outA < 0 then outA := 0;

        pix.red := outR shl 8 or outR;
        pix.green := outG shl 8 or outG;
        pix.blue := outB shl 8 or outB;
        pix.alpha := outA shl 8 or outA;
        intf.Colors[x, y] := pix;
      end;
    end;

    {$ifdef Windows}
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32bit;
      bmp.SetSize(ASize, ASize);
      bmp.LoadFromIntfImage(intf);
      blend.BlendOp := LOCAL_AC_SRC_OVER;
      blend.BlendFlags := 0;
      blend.SourceConstantAlpha := 255;
      blend.AlphaFormat := LOCAL_AC_SRC_ALPHA;
      LocalAlphaBlend(ACanvas.Handle, 0, 0, ASize, ASize,
                      bmp.Canvas.Handle, 0, 0, ASize, ASize, blend);
    finally
      bmp.Free;
    end;
    {$else}
    // TBitmap.Draw on GTK/Qt does not reliably composite an alpha channel
    // (the corners with alpha=0 blit as the bitmap's raw RGB — black for our
    // input). TPortableNetworkGraphic, in contrast, declares itself as a
    // proper RGBA image to LCL, so Canvas.Draw composites with alpha via
    // the backend (Cairo on GTK, QPainter on Qt).
    png := TPortableNetworkGraphic.Create;
    try
      png.LoadFromIntfImage(intf);
      ACanvas.Draw(0, 0, png);
    finally
      png.Free;
    end;
    {$endif}
  finally
    intf.Free;
  end;
end;

procedure TfBG.DotPaint(Sender: TObject);
var
  S, fontn: string;
  L: TDotControl;
  hasfont: boolean;
  needsRecalc: boolean;
  isGlyph: boolean;
  d, ringWidth: integer;
  dotColor: TColor;
begin
  L := Sender as TDotControl;
  S := L.Caption;
  L.AutoSize := false;

  isGlyph := (S = DOT_GRAPH) or (S = DOT_FRESH);

  if FUseDrawnDots and isGlyph then
  begin
    // Square the bounds — ResizeDot pre-sized to a square slot. Use the smaller
    // edge so a sloppy resize round can't crop the circle.
    d := Min(L.Width, L.Height);
    if d < 2 then
      Exit;

    dotColor := L.Font.Color;
    if S = DOT_FRESH then
      ringWidth := Max(2, d div 12)
    else
      ringWidth := 0;

    DrawSmoothCircle(L.Canvas, d, dotColor, dotColor, ringWidth);
    Exit;
  end;

  if S = DOT_GRAPH then
    hasfont := FontGUIInList(fontn)
  else
    hasfont := FontTXTInList(fontn);

  needsRecalc := (FCachedFontSize <> L.Font.Size) or (FCachedFontName <> fontn) or
    (FCachedTextWidth = 0) or (FCachedTextHeight = 0) or (S <> DOT_GRAPH);

  with L.Canvas do
  begin
    Brush.Style := bsClear;

    if hasfont then
      Font.Name := fontn;

    Font.Size := L.Font.Size;

    if needsRecalc then
    begin
      FCachedTextWidth := TextWidth(S);
      FCachedTextHeight := TextHeight(S);
      if S = DOT_GRAPH then
      begin
        FCachedFontSize := L.Font.Size;
        FCachedFontName := fontn;
      end;
    end;

    if S = DOT_GRAPH then
    begin
      L.Width := FCachedTextWidth;
      L.Height := FCachedTextHeight;
    end
    else
    begin
      L.Width := TextWidth(S) + 4;
      L.Height := TextHeight(S) + 2;
    end;

    TextOut(0, 0, S);
  end;
end;

procedure TfBG.BuildDotWindowMenu;
const
  WindowSizes: array[0..4] of integer = (6, 10, 18, 24, 36);
var
  i: integer;
  item: TMenuItem;
begin
  if not Assigned(FDotWindowMenu) then
  begin
    FDotWindowMenu := TMenuItem.Create(pmSettings);
    FDotWindowMenu.Caption := sTrendWindow;
    miSplit4.Parent.Insert(miSplit4.MenuIndex, FDotWindowMenu);
    for i := 0 to High(WindowSizes) do
    begin
      item := TMenuItem.Create(FDotWindowMenu);
      item.Caption := Format(sTrendWindowFmt,
        [WindowSizes[i] * INTERVAL_MINUTES, WindowSizes[i]]);
      item.Tag := WindowSizes[i];
      item.RadioItem := true;
      item.OnClick := @miDotWindowClick;
      FDotWindowMenu.Add(item);
    end;
  end;
  for i := 0 to FDotWindowMenu.Count - 1 do
    FDotWindowMenu.Items[i].Checked :=
      FDotWindowMenu.Items[i].Tag = ActiveDots;
end;

procedure TfBG.miDotWindowClick(Sender: TObject);
var
  i: integer;
  newCount: integer;
  ownerMenu: TMenuItem;
begin
  if not (Sender is TMenuItem) then Exit;
  newCount := TMenuItem(Sender).Tag;
  if (newCount < 1) or (newCount > MAX_DOT_COUNT) then Exit;

  ownerMenu := TMenuItem(Sender).Parent;
  if Assigned(ownerMenu) then
    for i := 0 to ownerMenu.Count - 1 do
      ownerMenu.Items[i].Checked := false;
  TMenuItem(Sender).Checked := true;

  native.SetSetting('ux.dot_count', newCount);
  ActiveDots := newCount;
  if MAX_MIN < ActiveDots * INTERVAL_MINUTES + 10 then
    MAX_MIN := ActiveDots * INTERVAL_MINUTES + 10;

  for i := 1 to High(TrendDots) do
    if Assigned(TrendDots[i]) then
    begin
      TrendDots[i].Free;
      TrendDots[i] := nil;
    end;
  SetLength(TrendDots, 0);
  for i := 1 to High(FWarningDots) do
    if Assigned(FWarningDots[i]) then
    begin
      FWarningDots[i].Free;
      FWarningDots[i] := nil;
    end;
  SetLength(FWarningDots, 0);

  // MAX_RESULT is a reading count, not minutes. Scale by the API's reporting
  // interval so backends that report more often than every INTERVAL_MINUTES
  // still produce enough readings to fill all dot slots.
  if Assigned(api) then
    MAX_RESULT := Max(MAX_RESULT,
      ActiveDots * Max(1, (INTERVAL_MINUTES + api.getReportingInterval - 1)
                   div api.getReportingInterval))
  else if MAX_RESULT < ActiveDots then
    MAX_RESULT := ActiveDots;

  CreateTrendDots;

  // Force PlaceTrendDots to reprocess even though bgs hasn't changed.
  FLastReadingsHash := 0;

  Application.ProcessMessages;

  tResize.OnTimer(Self);

  if Length(bgs) > 0 then
    PlaceTrendDots(bgs);
  UpdateTrendDots;
  AdjustGraph;
end;

// Post-process the graph
procedure TfBG.AdjustGraph;
var
  da: integer;
  centerShift: integer;
begin
  // SetPointHeight places dots by their Top edge. The legacy unicode glyph
  // happens to draw its visible mass near that top edge, but a Canvas.Ellipse
  // is geometrically centred in its bounding box — so a drawn dot is rendered
  // half a diameter below the intended value-Y unless we shift the whole row.
  if FUseDrawnDots and (Length(TrendDots) > 1) and Assigned(TrendDots[1]) then
  begin
    centerShift := -(TrendDots[1].Height div 2);
    if centerShift <> 0 then
      ApplyTrendDotTopOffset(centerShift);
  end;

  ApplyTrendDotTopOffset(round(ClientHeight * DOT_ADJUST));

  da := dotsInView;
  if da <> 0 then
  begin
    ApplyTrendDotTopOffset(-da);
    da := dotsInView;
    if da <> 0 then
      ApplyTrendDotTopOffset(-da);
  end;

  if (Length(TrendDots) > 1) and TrendDots[1].Visible then
  begin
    lRef.Top     := TrendDots[1].Top;
    lRef.Caption := TrendDots[1].Hint;
  end
  else
    lRef.Caption := '';
  Update;

  RepaintVisibleTrendDots;
end;

// Create trend dot controls dynamically at runtime.
// TrendDots is 1-based: index 0 is unused, valid range is 1..ActiveDots.
procedure TfBG.CreateTrendDots;
type
  PDotControl = ^TDotControl;
const
  MAX_NAMED = 10;
var
  i: integer;
  namedDots: array[1..MAX_NAMED] of PDotControl;
begin
  namedDots[1]  := @lDot1;
  namedDots[2]  := @lDot2;
  namedDots[3]  := @lDot3;
  namedDots[4]  := @lDot4;
  namedDots[5]  := @lDot5;
  namedDots[6]  := @lDot6;
  namedDots[7]  := @lDot7;
  namedDots[8]  := @lDot8;
  namedDots[9]  := @lDot9;
  namedDots[10] := @lDot10;

  SetLength(TrendDots, ActiveDots + 1);

  for i := 1 to ActiveDots do
  begin
    TrendDots[i] := TPaintBox.Create(Self);
    with TPaintBox(TrendDots[i]) do
    begin
      Parent := Self;
      AutoSize := false;
      OnPaint := @DotPaint;
    end;

    with TrendDots[i] do
    begin
      Left := 72;
      Top := 191;
      Width := 33;
      Height := 49;
      // Set a non-zero default so SetPointHeight has a usable Font.Size proxy
      // if it runs (via UpdateLabelForReading) before ResizeDot has sized the dot.
      Font.Size := 24;
      Visible := false;
      Caption := DOT_GRAPH;
      PopupMenu := pmSettings;
      OnClick := @onTrendClick;
    end;

    if i <= MAX_NAMED then
      namedDots[i]^ := TrendDots[i];
  end;

  for i := ActiveDots + 1 to MAX_NAMED do
    namedDots[i]^ := nil;

  TrndiDLog(Format('Trend dots created: %d dots (%d min window)',
    [ActiveDots, ActiveDots * INTERVAL_MINUTES]));
end;

procedure TfBG.initDot(l: TDotControl; c, ix: integer);
begin
  {$ifdef Windows}
  l.Font.Name := 'DejaVu Sans Mono';
  {$endif}
  {$ifdef DARWIN}
  l.Font.Name := 'Arial'; // Better circle shape on macOS
  {$endif}
end;

function TfBG.CalculateDotVisualOffset: integer;
var
  bmp: TBitmap;
  y, x: integer;
  fontSize: integer;
  halfHeight: integer;
  firstPixelY: integer;
  bgColor, pixelColor: TColor;
  found: boolean;
  fontName: string;
begin
  Result := 0;
  bmp := TBitmap.Create;
  try
    try
      fontSize := 24;
      bmp.Width := fontSize * 2;
      bmp.Height := fontSize * 2;

      {$ifdef DARWIN}
      if bmp.Canvas.Handle = 0 then ;
      {$endif}

      bgColor := clWhite;
      bmp.Canvas.Brush.Color := bgColor;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

      if FontGUIInList(fontName) then
        bmp.Canvas.Font.Name := fontName
      else
        bmp.Canvas.Font.Name := 'default';
      bmp.Canvas.Font.Size := fontSize;
      bmp.Canvas.Font.Color := clBlack;

      bmp.Canvas.TextOut(fontSize div 2, fontSize div 2, DOT_GRAPH);

      firstPixelY := -1;
      found := false;

      for y := 0 to bmp.Height - 1 do
      begin
        for x := 0 to bmp.Width - 1 do
        begin
          pixelColor := bmp.Canvas.Pixels[x, y];
          if pixelColor <> bgColor then
          begin
            firstPixelY := y;
            found := true;
            break;
          end;
        end;
        if found then
          break;
      end;

      if found then
      begin
        halfHeight := bmp.Height div 2;
        Result := firstPixelY - halfHeight;
        TrndiDLog(Format(
          'DOT_VISUAL_OFFSET calculated: font=%s, firstPixel=%d, halfHeight=%d, offset=%d',
          [fontName, firstPixelY, halfHeight, Result]));
      end;
    except
      on E: Exception do
      begin
        {$ifdef DARWIN}
        TrndiDLog('CalculateDotVisualOffset error on macOS: ' + E.Message);
        Result := 0;
        {$else}
        raise;
        {$endif}
      end;
    end;
  finally
    bmp.Free;
  end;
end;

// Expands a trend dot to show actual bg value with highlighting for latest reading
procedure TfBG.ExpandDot(l: TDotControl; c, ix: integer);
var
  isDot: boolean;
begin
  if privacyMode then
    exit;

  if l.hint = '' then
    Exit;

  isDot := l.Caption = DOT_GRAPH;

  if ix = ActiveDots then
    l.Caption := IfThen(isDot, DOT_FRESH, DOT_GRAPH)
  else
    l.Caption := IfThen(isDot, l.Hint, DOT_GRAPH);

  if not isDot then
  begin
    ResizeDot(l, c, ix);
  end
  else
  begin
    l.font.Size := (lVal.Font.Size div c);
    FCachedTextWidth := 0;
    FCachedTextHeight := 0;
    FCachedFontSize := 0;
    FCachedFontName := '';
    l.Invalidate;
  end;
end;

// Hides a dot
procedure TfBG.HideDot(l: TDotControl; c, ix: integer);
begin
  l.Visible := false;
end;

procedure TfBG.ApplyTrendDotTopOffset(const Offset: integer);
var
  l: TDotControl;
begin
  if Offset = 0 then
    Exit;

  for l in TrendDots do
  begin
    if not Assigned(l) then Continue;
    l.Top := l.Top + Offset;
  end;
end;

procedure TfBG.RepaintVisibleTrendDots;
var
  l: TDotControl;
begin
  for l in TrendDots do
  begin
    if not Assigned(l) then Continue;
    if l.Visible then
      l.Repaint;
  end;
end;

// Shows a dot only if it has valid data (non-empty Hint)
procedure TfBG.ShowDot(l: TDotControl; c, ix: integer);
begin
  if l.Hint <> '' then
    l.Visible := true;
end;

// Scales a dot to fill ~85% of its slot width so adjacent dots never overlap.
procedure TfBG.ResizeDot(l: TDotControl; c, ix: integer);
var
  th, tw, slotW, d: integer;
  isGlyph: boolean;
begin
  slotW := Max(1, fBG.ClientWidth div Max(c, 1));
  isGlyph := (l.Caption = DOT_GRAPH) or (l.Caption = DOT_FRESH);
  l.AutoSize := false;
  if FUseDrawnDots and isGlyph then
  begin
    // Drawn-circle mode: compute diameter directly from the slot. 0.6 matches
    // the visible disc area of the legacy ● glyph rendered at 85% slot width,
    // and skips the Canvas.TextWidth feedback loop entirely.
    d := Max(8, Round(slotW * 0.6));
    l.Width := d;
    l.Height := d;
    // Keep Font.Size proportional so consumers that copy the dot's font
    // (lRef, warning overlays) get a sane point size.
    l.Font.Size := Max(8, Round(d * 0.6));
    TrndiDLog(Format('TrendDots[%d] resized (circle) diameter=%d.', [ix, d]));
  end
  else
  begin
    l.Font.Size := Max(8, slotW);
    {$ifndef DARWIN}
    // Canvas.TextWidth must not be called outside drawRect on Cocoa —
    // it corrupts the NSGraphicsContext (CheckDC bug). DotPaint will
    // correct the bounds when it renders.
    tw := l.Canvas.TextWidth(DOT_GRAPH);
    if tw > 0 then
      l.Font.Size := Max(8, Round(l.Font.Size * (slotW * 85 div 100) / tw));
    tw := l.Canvas.TextWidth(DOT_GRAPH);
    th := l.Canvas.TextHeight(DOT_GRAPH);
    l.Width := Max(1, tw);
    l.Height := Max(th, l.Font.Size);
    TrndiDLog(Format('TrendDots[%d] resized with Font Size = %d, W=%d, H=%d.',
      [ix, l.Font.Size, l.Width, l.Height]));
    {$endif}
  end;
end;

// Centers a dot within its slot across ClientWidth.
procedure TfBG.SetDotWidth(l: TDotControl; c, ix: integer; ls: array of TDotControl);
var
  leftPx, rightPx, slotWidth: integer;
  cw: integer;
begin
  if c < 1 then Exit;
  cw        := fBG.ClientWidth;
  leftPx    := Round((ix - 1) * cw / c);
  rightPx   := Round(ix       * cw / c);
  if rightPx > cw then rightPx := cw;
  slotWidth := rightPx - leftPx;
  l.Left    := Max(0, Min(leftPx + (slotWidth - l.Width) div 2, cw - l.Width));
  TrndiDLog(Format('TrendDots[%d] Left=%d Width=%d.', [ix, l.Left, l.Width]));
end;

// Swap dots with their readings
procedure TfBG.onTrendClick(Sender: TObject);
var
  isdot: boolean;
  l: TDotControl;
  {$ifdef TrndiExt}
  fs: TFormatSettings;
  {$endif}
begin
  l := Sender as TDotControl;

  actOnTrend(@ExpandDot);
  isDot := UnicodeSameText(l.Caption, DOT_GRAPH);

  {$ifdef TrndiExt}
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  callFunc('dotClicked',[
    isDot,
    StrToFloat(l.Hint) * BG_CONVERTIONS[mgdl][un],
    StrToFloat(l.Hint) * BG_CONVERTIONS[mmol][un],
    l.tag
    ]);
  {$endif}

  if isDot then
    tResize.OnTimer(self);
end;
