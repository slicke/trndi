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

type
  // Finished, platform-ready dot image: premultiplied 32-bit bitmap for the
  // Win32 AlphaBlend path, RGBA PNG for the LCL Draw path elsewhere.
  // TBitmap.Draw on GTK/Qt does not reliably composite an alpha channel
  // (the corners with alpha=0 blit as the bitmap's raw RGB — black for our
  // input). TPortableNetworkGraphic, in contrast, declares itself as a
  // proper RGBA image to LCL, so Canvas.Draw composites with alpha via
  // the backend (Cairo on GTK, QPainter on Qt).
  {$ifdef Windows}
  TDotImage = TBitmap;
  {$else}
  TDotImage = TPortableNetworkGraphic;
  {$endif}
  TDotImageCache = specialize TObjectDictionary<string, TDotImage>;

const
  // Upper bound on distinct (shape, size, color, ...) combos kept alive. A
  // layout pass produces a handful of sizes and range colors; the cap only
  // stops unbounded growth across many window sizes. Clearing wholesale is
  // fine — entries are cheap to re-render once.
  DOT_IMAGE_CACHE_MAX = 32;

var
  // Rendered-dot cache. DotPaint repaints every dot on each tick (tResize
  // fires at 50 ms during drag) and the analytic-AA rasterizers below cost a
  // per-pixel loop plus image-object churn per dot, while the dots share a
  // handful of (size, color) combos. Main-thread only (paint handlers);
  // freed in the unit finalization of umain.pp.
  DotImageCache: TDotImageCache = nil;

function GetCachedDotImage(const AKey: string): TDotImage;
begin
  Result := nil;
  if Assigned(DotImageCache) then
    DotImageCache.TryGetValue(AKey, Result);
end;

procedure StoreCachedDotImage(const AKey: string; AImage: TDotImage);
begin
  if DotImageCache = nil then
    DotImageCache := TDotImageCache.Create([doOwnsValues]);
  if DotImageCache.Count >= DOT_IMAGE_CACHE_MAX then
    DotImageCache.Clear;
  DotImageCache.AddOrSetValue(AKey, AImage);
end;

// Wrap a rasterized RGBA buffer in the platform image object.
function DotImageFromIntf(intf: TLazIntfImage; ASize: integer): TDotImage;
begin
  {$ifdef Windows}
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(ASize, ASize);
  Result.LoadFromIntfImage(intf);
  {$else}
  Result := TPortableNetworkGraphic.Create;
  Result.LoadFromIntfImage(intf);
  {$endif}
end;

// Composite a finished dot image over ACanvas at the given offset.
procedure BlitDotImage(ACanvas: TCanvas; AImage: TDotImage;
  ASize, AOffsetX, AOffsetY: integer);
{$ifdef Windows}
var
  blend: TLocalBlendFunction;
begin
  blend.BlendOp := LOCAL_AC_SRC_OVER;
  blend.BlendFlags := 0;
  blend.SourceConstantAlpha := 255;
  blend.AlphaFormat := LOCAL_AC_SRC_ALPHA;
  LocalAlphaBlend(ACanvas.Handle, AOffsetX, AOffsetY, ASize, ASize,
                  AImage.Canvas.Handle, 0, 0, ASize, ASize, blend);
end;
{$else}
begin
  ACanvas.Draw(AOffsetX, AOffsetY, AImage);
end;
{$endif}

// Try to satisfy a paint entirely from the rendered-dot cache.
function TryBlitCachedDot(ACanvas: TCanvas; const AKey: string;
  ASize, AOffsetX, AOffsetY: integer): boolean;
var
  img: TDotImage;
begin
  img := GetCachedDotImage(AKey);
  Result := img <> nil;
  if Result then
    BlitDotImage(ACanvas, img, ASize, AOffsetX, AOffsetY);
end;

// Allocate the square RGBA raster both dot shapes render into.
function NewDotIntf(ASize: integer): TLazIntfImage;
var
  rawDesc: TRawImageDescription;
begin
  Result := TLazIntfImage.Create(0, 0);
  rawDesc.Init_BPP32_B8G8R8A8_BIO_TTB(ASize, ASize);
  Result.DataDescription := rawDesc;
  Result.CreateData;
end;

// Write one straight-alpha pixel (RGB in 0..255, AAlpha in 0..1), converting
// to what the platform blit expects: premultiplied RGB for Win32 AlphaBlend,
// straight RGB for the PNG / LCL Draw path.
procedure PutDotPixel(AIntf: TLazIntfImage; X, Y: integer;
  AR, AG, AB, AAlpha: double);
var
  outR, outG, outB, outA: integer;
  pix: TFPColor;
begin
  if AAlpha < 0 then AAlpha := 0
  else if AAlpha > 1 then AAlpha := 1;

  {$ifdef Windows}
  // Win32 AlphaBlend with AC_SRC_ALPHA expects premultiplied RGB.
  outR := Round(AR * AAlpha);
  outG := Round(AG * AAlpha);
  outB := Round(AB * AAlpha);
  {$else}
  // PNG / cross-platform draw expects straight (non-premultiplied) RGB.
  outR := Round(AR);
  outG := Round(AG);
  outB := Round(AB);
  {$endif}
  outA := Round(AAlpha * 255);

  if outR > 255 then outR := 255 else if outR < 0 then outR := 0;
  if outG > 255 then outG := 255 else if outG < 0 then outG := 0;
  if outB > 255 then outB := 255 else if outB < 0 then outB := 0;

  pix.red := outR shl 8 or outR;
  pix.green := outG shl 8 or outG;
  pix.blue := outB shl 8 or outB;
  pix.alpha := outA shl 8 or outA;
  AIntf.Colors[X, Y] := pix;
end;

// Wrap the finished raster in the platform image, cache it under AKey and
// composite it onto the destination canvas. The caller still owns AIntf.
procedure FinishDotRender(ACanvas: TCanvas; const AKey: string;
  AIntf: TLazIntfImage; ASize, AOffsetX, AOffsetY: integer);
var
  img: TDotImage;
begin
  img := DotImageFromIntf(AIntf, ASize);
  StoreCachedDotImage(AKey, img);
  BlitDotImage(ACanvas, img, ASize, AOffsetX, AOffsetY);
end;

// Antialiased X mark, same compositing approach as DrawSmoothCircle. Two
// corner-to-corner diagonals clipped by the bounding box; alpha derives from
// the analytical distance to the nearest diagonal so the strokes stay smooth
// at small sizes without relying on GDI's aliased LineTo.
procedure DrawSmoothX(ACanvas: TCanvas; ASize: integer; AColor: TColor;
  AThickness: integer; AOffsetX: integer = 0; AOffsetY: integer = 0);
var
  intf: TLazIntfImage;
  x, y: integer;
  cx, cy, dist1, dist2, dist, halfT, aPix, invSqrt2: double;
  fr, fg, fb: byte;
  key: string;
begin
  if (ASize < 2) or (AThickness < 1) then
    Exit;

  // The rendered image depends only on these parameters (offsets are applied
  // at blit time), so reuse the finished image across paints.
  key := Format('x|%d|%d|%d', [ASize, Integer(AColor), AThickness]);
  if TryBlitCachedDot(ACanvas, key, ASize, AOffsetX, AOffsetY) then
    Exit;

  fr := Red(ColorToRGB(AColor));
  fg := Green(ColorToRGB(AColor));
  fb := Blue(ColorToRGB(AColor));

  cx := (ASize - 1) / 2.0;
  cy := (ASize - 1) / 2.0;
  halfT := AThickness / 2.0;
  invSqrt2 := 1.0 / Sqrt(2);

  intf := NewDotIntf(ASize);
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

        PutDotPixel(intf, x, y, fr, fg, fb, aPix);
      end;

    FinishDotRender(ACanvas, key, intf, ASize, AOffsetX, AOffsetY);
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
  straightR, straightG, straightB: double;
  hasRing: boolean;
  key: string;
begin
  if ASize < 2 then
    Exit;

  // The rendered image depends only on these parameters (offsets are applied
  // at blit time), so reuse the finished image across paints.
  key := Format('c|%d|%d|%d|%d', [ASize, Integer(ADotColor),
    Integer(ARingColor), ARingWidth]);
  if TryBlitCachedDot(ACanvas, key, ASize, AOffsetX, AOffsetY) then
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

  intf := NewDotIntf(ASize);
  try
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
        // in PutDotPixel when the data is handed to the OS.
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

        PutDotPixel(intf, x, y, straightR, straightG, straightB, aOut);
      end;
    end;

    FinishDotRender(ACanvas, key, intf, ASize, AOffsetX, AOffsetY);
  finally
    intf.Free;
  end;
end;

procedure TfBG.DotPaint(Sender: TObject);
var
  S, fontn: string;
  L: TDotControl;
  d, ringWidth, xSize, xOff: integer;
  dotColor, ringColor: TColor;
begin
  L := Sender as TDotControl;
  S := L.Caption;

  if (S = DOT_GRAPH) or (S = DOT_FRESH) or (S = DOT_PREDICT) then
  begin
    // Square the bounds — ResizeDot pre-sized to a square slot. Use the smaller
    // edge so a sloppy resize round can't crop the circle.
    d := Min(L.Width, L.Height);
    if d < 2 then
      Exit;

    dotColor := L.Font.Color;
    if S = DOT_PREDICT then
    begin
      // When the engine reports low confidence in its forecast, draw a ?
      // instead of the × so the uncertainty is visible at a glance. Clicking
      // the dot shows the exact confidence percentage.
      if PredictionConfidence < PREDICTION_UNCERTAIN_BELOW then
      begin
        with L.Canvas do
        begin
          Brush.Style := bsClear;
          Font.Assign(L.Font);
          Font.Color := dotColor;
          Font.Style := [fsBold];
          Font.Height := Max(8, Round(d * 0.9));
          TextOut((d - TextWidth('?')) div 2, (d - TextHeight('?')) div 2, '?');
        end;
        Exit;
      end;

      // Compact × (multiplication-sign) shape: shorter, thinner strokes inset
      // from the slot edges, centred in the same square the history dot would
      // occupy. Drawn rather than typeset so we don't depend on a font that
      // ships the glyph at consistent metrics across platforms.
      xSize := Max(6, Round(d * 0.6));
      xOff  := (d - xSize) div 2;
      DrawSmoothX(L.Canvas, xSize, dotColor, Max(2, xSize div 8), xOff, xOff);
      Exit;
    end;

    if S = DOT_FRESH then
    begin
      // Rim the newest reading in a contrasting tone of its range color; a
      // same-color ring over a solid disc would be invisible.
      ringWidth := Max(2, d div 12);
      ringColor := GetTextColorForBackground(dotColor);
    end
    else
    begin
      ringWidth := 0;
      ringColor := dotColor;
    end;

    DrawSmoothCircle(L.Canvas, d, dotColor, ringColor, ringWidth);
    Exit;
  end;

  // Expanded display: render the BG value (or other text caption) directly.
  // Width/Height are set in ResizeDot — never resize the control inside its
  // own OnPaint. On macOS Cocoa, doing so re-enters paint with a stale DC and
  // crashes inside TCocoaGDIUtil.checkDC.
  with L.Canvas do
  begin
    Brush.Style := bsClear;
    if FontTXTInList(fontn) then
      Font.Name := fontn;
    Font.Size := L.Font.Size;
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
    FDotWindowMenu.Caption := RS_UX_TREND_WINDOW;
    miSplit4.Parent.Insert(miSplit4.MenuIndex, FDotWindowMenu);
    for i := 0 to High(WindowSizes) do
    begin
      item := TMenuItem.Create(FDotWindowMenu);
      item.Caption := Format(RS_UX_TREND_WINDOW_FMT,
        [WindowSizes[i] * INTERVAL_MINUTES, WindowSizes[i]]);
      item.Tag := WindowSizes[i];
      item.RadioItem := true;
      item.OnClick := @miDotWindowClick;
      FDotWindowMenu.Add(item);
    end;
  end;
  for i := 0 to FDotWindowMenu.Count - 1 do
    FDotWindowMenu.Items[i].Checked :=
      FDotWindowMenu.Items[i].Tag = ACTIVE_DOTS;
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

  ApplyTrendDotCount(newCount);
end;

// Free and clear the trend-dot and warning-overlay dot controls so
// CreateTrendDots can rebuild them at a new ACTIVE_DOTS count. Both arrays
// are 1-based (index 0 unused). Callers must follow up with CreateTrendDots,
// which also re-points the named lDotN aliases.
procedure TfBG.FreeTrendDotControls;
var
  i: integer;
begin
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
end;

// Apply a new main-trend dot count: persist it, rebuild the dot controls and
// reflow the graph. Shared by the trend-window submenu and the manual
// "Set Dot Count" service-menu entry.
procedure TfBG.ApplyTrendDotCount(newCount: integer);
begin
  if (newCount < 1) or (newCount > MAX_DOT_COUNT) then Exit;

  native.SetSetting('ux.dot_count', newCount);
  ACTIVE_DOTS := newCount;
  if MAX_MIN < ACTIVE_DOTS * INTERVAL_MINUTES + 10 then
    MAX_MIN := ACTIVE_DOTS * INTERVAL_MINUTES + 10;

  FreeTrendDotControls;

  // MAX_RESULT is a reading count, not minutes. Scale by the API's reporting
  // interval so backends that report more often than every INTERVAL_MINUTES
  // still produce enough readings to fill all dot slots.
  if Assigned(api) then
    MAX_RESULT := Max(MAX_RESULT,
      ACTIVE_DOTS * Max(1, (INTERVAL_MINUTES + api.getReportingInterval - 1)
                   div api.getReportingInterval))
  else if MAX_RESULT < ACTIVE_DOTS then
    MAX_RESULT := ACTIVE_DOTS;

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
  // SetPointHeight places dots by their Top edge, but a drawn circle is
  // geometrically centred in its bounding box — shift the whole row up by
  // half a diameter so the dot's centre lands on the intended value-Y.
  if (Length(TrendDots) > 1) and Assigned(TrendDots[1]) then
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
// TrendDots is 1-based: index 0 is unused, valid range is 1..ACTIVE_DOTS.
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

  SetLength(TrendDots, ACTIVE_DOTS + 1);

  for i := 1 to ACTIVE_DOTS do
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

  for i := ACTIVE_DOTS + 1 to MAX_NAMED do
    namedDots[i]^ := nil;

  TrndiDLog(Format('Trend dots created: %d dots (%d min window)',
    [ACTIVE_DOTS, ACTIVE_DOTS * INTERVAL_MINUTES]));

  // Prediction dots have a fixed count and survive dot-window changes, so this
  // creates them once and is a no-op on subsequent CreateTrendDots calls.
  CreatePredictionDots;
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

// Toggles a trend dot between its marker (dot/fresh ring) and its bg value.
procedure TfBG.ExpandDot(l: TDotControl; c, ix: integer);
var
  isDot: boolean;
begin
  if privacyMode then
    exit;

  if l.hint = '' then
    Exit;

  isDot := (l.Caption = DOT_GRAPH) or (l.Caption = DOT_FRESH);

  if isDot then
    l.Caption := l.Hint
  else
  if (ix = ACTIVE_DOTS) and FLatestIsFresh then
    l.Caption := DOT_FRESH
  else
    l.Caption := DOT_GRAPH;

  // Always size the dot here, never inside DotPaint. Resizing during paint
  // re-enters with a stale DC on Cocoa and crashes in TCocoaGDIUtil.checkDC.
  ResizeDot(l, c, ix);
  l.Invalidate;
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
  // Prediction dots share the trend's vertical transform so they line up with
  // the history dots on the same value axis.
  for l in PredictionDots do
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
  for l in PredictionDots do
  begin
    if not Assigned(l) then Continue;
    if l.Visible then
      l.Repaint;
  end;
end;

// Total horizontal trend slots: the history dots plus prediction slots when
// the dot view is active (so history compresses to free room on the right).
function TfBG.TrendColumnCount: integer;
begin
  Result := ACTIVE_DOTS;
  if PredictionDotsActive then
    Inc(Result, PREDICTION_DOT_COUNT);
end;

// True when predictions should be drawn as hollow dots on the trend rather
// than written into the lPredict label. Requires the dots to exist.
function TfBG.PredictionDotsActive: boolean;
begin
  Result := PredictGlucoseReading and PredictDotMode
    and (Length(PredictionDots) = PREDICTION_DOT_COUNT);
end;

// Create the fixed set of hollow prediction-dot paint boxes. Their count is
// independent of ACTIVE_DOTS, so this runs once and is a no-op thereafter
// (unlike CreateTrendDots, which is rebuilt when the dot window changes).
procedure TfBG.CreatePredictionDots;
var
  i: integer;
begin
  if Length(PredictionDots) = PREDICTION_DOT_COUNT then
    Exit;

  SetLength(PredictionDots, PREDICTION_DOT_COUNT);
  for i := 0 to PREDICTION_DOT_COUNT - 1 do
  begin
    PredictionDots[i] := TPaintBox.Create(Self);
    with TPaintBox(PredictionDots[i]) do
    begin
      Parent := Self;
      AutoSize := false;
      OnPaint := @DotPaint;
      Left := 0;
      Top := 0;
      Width := 33;
      Height := 33;
      Font.Size := 24;
      Visible := false;
      Caption := DOT_PREDICT;
      PopupMenu := pmSettings;
      OnClick := @PredictionDotClick;
    end;
    initDot(PredictionDots[i], PREDICTION_DOT_COUNT, i + 1);
  end;

  TrndiDLog(Format('Prediction dots created: %d hollow dots.', [PREDICTION_DOT_COUNT]));
end;

// Size and horizontally position the prediction dots in the trailing slots of
// the (compressed) trend grid. Vertical placement is handled by
// UpdatePredictionDotHeights + the shared ApplyTrendDotTopOffset cascade.
procedure TfBG.LayoutPredictionDots;
var
  i, cols, ix, cw, leftPx, rightPx, slotWidth: integer;
  l: TDotControl;
begin
  if not PredictionDotsActive then
    Exit;

  cols := Max(TrendColumnCount, 1);
  cw   := fBG.ClientWidth;
  for i := 0 to High(PredictionDots) do
  begin
    l := PredictionDots[i];
    if not Assigned(l) then Continue;

    // Reuse the trend-dot circle sizing so prediction dots match the history
    // dots' diameter (same slot width via TrendColumnCount).
    ResizeDot(l, ACTIVE_DOTS, ACTIVE_DOTS + i + 1);

    ix        := ACTIVE_DOTS + i + 1;
    leftPx    := Round((ix - 1) * cw / cols);
    rightPx   := Round(ix       * cw / cols);
    if rightPx > cw then rightPx := cw;
    slotWidth := rightPx - leftPx;
    l.Left    := Max(0, Min(leftPx + (slotWidth - l.Width) div 2, cw - l.Width));
  end;
end;

// Re-apply value-based vertical positions to visible prediction dots from
// their cached Hint values (mirrors UpdateTrendDots for the history dots).
procedure TfBG.UpdatePredictionDotHeights;
var
  l: TDotControl;
  Value: glucose;
begin
  if not PredictionDotsActive then
    Exit;

  for l in PredictionDots do
  begin
    if not Assigned(l) then Continue;
    if not l.Visible then Continue;

    if TryStrToFloat(l.Hint, Value, native.locale) then
    begin
      if un = mgdl then
        Value := Value * BG_CONVERTIONS[mmol][mgdl];

      if Assigned(l.Parent) then
        SetPointHeight(l, Value, l.Parent.ClientHeight)
      else
        SetPointHeight(l, Value, fBG.ClientHeight);
    end;
  end;
end;

procedure TfBG.HidePredictionDots;
var
  l: TDotControl;
begin
  for l in PredictionDots do
    if Assigned(l) then
      l.Visible := false;
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
  slotW, d: integer;
  fontn, s: string;
begin
  // Divide the width by the full column count (history + any prediction slots)
  // so history dots compress to leave room for future dots on the right.
  slotW := Max(1, fBG.ClientWidth div Max(TrendColumnCount, 1));
  l.AutoSize := false;
  if (l.Caption = DOT_GRAPH) or (l.Caption = DOT_FRESH) or (l.Caption = DOT_PREDICT) then
  begin
    // Square sizing — DotPaint draws a circle inscribed in this box. 0.6
    // matches the visible disc area of the legacy ● glyph at 85% slot width.
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
    // Expanded numeric display — derive font size from lVal so the dot text
    // tracks the main BG display, then size Width/Height to fit the caption.
    // Measure via a temp TBitmap rather than the control's Canvas: doing the
    // resize here (not in DotPaint) avoids re-entering paint with a stale DC
    // on Cocoa, which crashes in TCocoaGDIUtil.checkDC.
    l.Font.Size := Max(8, lVal.Font.Size div Max(c, 1));
    s := l.Caption;
    with TBitmap.Create do
    try
      SetSize(1, 1);
      Canvas.Font.Assign(l.Font);
      if FontTXTInList(fontn) then
        Canvas.Font.Name := fontn;
      l.Width := Canvas.TextWidth(s) + 4;
      l.Height := Canvas.TextHeight(s) + 2;
    finally
      Free;
    end;
    TrndiDLog(Format('TrendDots[%d] resized (text) Font Size = %d W=%d H=%d.',
      [ix, l.Font.Size, l.Width, l.Height]));
  end;
end;

// Centers a dot within its slot across ClientWidth.
procedure TfBG.SetDotWidth(l: TDotControl; c, ix: integer; ls: array of TDotControl);
var
  leftPx, rightPx, slotWidth: integer;
  cw, cols: integer;
begin
  if c < 1 then Exit;
  // Position within the full column grid (history + prediction slots) so the
  // history dots occupy the left portion and leave room on the right.
  cols      := Max(TrendColumnCount, 1);
  cw        := fBG.ClientWidth;
  leftPx    := Round((ix - 1) * cw / cols);
  rightPx   := Round(ix       * cw / cols);
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
  hintVal: double;
  {$endif}
begin
  l := Sender as TDotControl;

  actOnTrend(@ExpandDot);
  isDot := (l.Caption = DOT_GRAPH) or (l.Caption = DOT_FRESH);

  {$ifdef TrndiExt}
  // The Hint is written with the app-wide format settings (updateLocale keeps
  // DefaultFormatSettings and native.locale in sync), so parse with the same.
  if TryStrToFloat(l.Hint, hintVal, native.locale) then
    callFunc('dotClicked',[
      isDot,
      hintVal * BG_CONVERTIONS[mgdl][un],
      hintVal * BG_CONVERTIONS[mmol][un],
      l.tag
      ]);
  {$endif}

  if isDot then
    tResize.OnTimer(self);
end;

// Show how much the prediction engine trusts the forecast behind the dots.
procedure TfBG.PredictionDotClick(Sender: TObject);
begin
  ShowMessage(Format(RS_PREDICTION_CONFIDENCE, [PredictionConfidence * 100]));
end;
