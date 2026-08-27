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
(* Trend dot creation, layout, painting and interaction for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)
(* MODIFICATION NOTICE (2026-08-27): Dot state now lives in the TTrendSlot
   model (TrendSlots/PredictionSlots on TfBG) instead of being encoded into
   the controls' Caption/Hint/Tag/Font.Color; Hint is written as the hover
   tooltip only and is never parsed back. Click-expansion is the shared
   FDotsExpanded flag rather than a caption swap. *)
(* MODIFICATION NOTICE (2026-08-27, second pass): The per-dot TPaintBox
   controls are gone. The whole trend renders on one client-sized surface
   (TTrendSurface) that is mouse-transparent outside the dots via CM_HITTEST;
   UpdateTrendDots computes every slot's box in one pass (grid, diameter,
   value-Y, DOT_ADJUST, keep-in-view) into FTrendLayout/FPredictLayout,
   replacing the ResizeDot/SetDotWidth/SetPointHeight/CenterShift/TopOffset/
   dotsInView control-mutation cascade. Clicks, hover hints and the popup
   menu's dot entry go through DotHitAt. *)

// Trend-dot rasterizers. GDI's TCanvas.Ellipse is not antialiased even with
// AntialiasingMode := amOn — it calls the standard Ellipse() API which is
// strictly aliased — so the shapes below are rendered at their final size
// with analytical pixel coverage and handed to the OS compositor. See
// inc/umain_alphablit.inc for the shared raster/blit primitives.
type
  TDotImageCache = specialize TObjectDictionary<string, TAlphaImage>;

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

function GetCachedDotImage(const AKey: string): TAlphaImage;
begin
  Result := nil;
  if Assigned(DotImageCache) then
    DotImageCache.TryGetValue(AKey, Result);
end;

procedure StoreCachedDotImage(const AKey: string; AImage: TAlphaImage);
begin
  if DotImageCache = nil then
    DotImageCache := TDotImageCache.Create([doOwnsValues]);
  if DotImageCache.Count >= DOT_IMAGE_CACHE_MAX then
    DotImageCache.Clear;
  DotImageCache.AddOrSetValue(AKey, AImage);
end;

// Composite a finished dot image over ACanvas at the given offset. Dots are
// always blitted 1:1 — they are rasterized at their final size.
procedure BlitDotImage(ACanvas: TCanvas; AImage: TAlphaImage;
  ASize, AOffsetX, AOffsetY: integer); inline;
begin
  BlitAlphaImage(ACanvas, AImage, ASize, ASize, AOffsetX, AOffsetY,
    ASize, ASize);
end;

// Try to satisfy a paint entirely from the rendered-dot cache.
function TryBlitCachedDot(ACanvas: TCanvas; const AKey: string;
  ASize, AOffsetX, AOffsetY: integer): boolean;
var
  img: TAlphaImage;
begin
  img := GetCachedDotImage(AKey);
  Result := img <> nil;
  if Result then
    BlitDotImage(ACanvas, img, ASize, AOffsetX, AOffsetY);
end;

// Wrap the finished raster in the platform image, cache it under AKey and
// composite it onto the destination canvas. The caller still owns AIntf.
procedure FinishDotRender(ACanvas: TCanvas; const AKey: string;
  AIntf: TLazIntfImage; ASize, AOffsetX, AOffsetY: integer);
var
  img: TAlphaImage;
begin
  img := AlphaImageFromIntf(AIntf, ASize, ASize);
  StoreCachedDotImage(AKey, img);
  BlitDotImage(ACanvas, img, ASize, AOffsetX, AOffsetY);
end;

// Antialiased X mark, same compositing approach as DrawSmoothCircle. Two
// corner-to-corner diagonals clipped by the bounding box; alpha derives from
// the analytical distance to the nearest diagonal so the strokes stay smooth
// at small sizes without relying on GDI's aliased LineTo. AAlpha scales the
// whole mark's opacity — DotPaint uses it to fade the prediction marks by
// confidence and horizon.
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
  if TryBlitCachedDot(ACanvas, key, ASize, AOffsetX, AOffsetY) then
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

    FinishDotRender(ACanvas, key, intf, ASize, AOffsetX, AOffsetY);
  finally
    intf.Free;
  end;
end;

// ADotColor = clNone draws no disc at all — a hollow ring only — so the
// pixels inside the rim stay transparent and whatever sits behind the dot
// (range bands, the window color) shows through.
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
  if TryBlitCachedDot(ACanvas, key, ASize, AOffsetX, AOffsetY) then
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

    FinishDotRender(ACanvas, key, intf, ASize, AOffsetX, AOffsetY);
  finally
    intf.Free;
  end;
end;

var
  // Cached raster of the last connecting line drawn (ux.dot_line). One slot,
  // not the dot dictionary: the image is trace-sized (potentially megabytes at
  // large windows) and only one geometry is live at a time, so a dictionary
  // would just hoard stale traces until its wholesale clear. Keyed by the
  // bbox-relative points, so the vertical keep-in-view shifts and window moves
  // that don't change the trace's shape still hit. Main-thread only (paint
  // handlers); freed in the unit finalization of umain.pp.
  TrendLineImage: TAlphaImage = nil;
  TrendLineKey: string = '';
  TrendLineW: integer = 0;
  TrendLineH: integer = 0;

// Antialiased polyline through APts, same analytic-coverage approach as the
// dot rasterizers above (GDI/Qt canvas lines are aliased). AColors runs
// parallel to APts: each point's color extends halfway toward both of its
// neighbors, so the trace switches color mid-segment — every reading owns
// the piece of line on either side of its dot. Round joints and caps fall
// out of the per-stroke capsule distance, and overlapping strokes keep the
// strongest coverage in a shared buffer so the joints don't double-blend
// (the two strokes meeting at a dot share that dot's color anyway; at the
// midpoint seam the winner rule cuts cleanly between the two colors).
procedure DrawSmoothPolyline(ACanvas: TCanvas; const APts: array of TPoint;
  const AColors: array of TColor; AThickness: integer);
var
  intf: TLazIntfImage;
  cov: array of single;
  col: array of TColor;
  i, x, y, minX, minY, maxX, maxY, w, h, pad: integer;
  halfT, mx, my: double;
  c: TColor;
  key: string;

  // Max-combine one capsule stroke from (ax,ay) to (bx,by), raster-relative,
  // in AColor. Only the stroke's own padded bbox is visited, so the cost
  // tracks the trace's length rather than the raster's area.
  procedure RasterStroke(ax, ay, bx, by: double; AColor: TColor);
  var
    px, py, sx0, sy0, sx1, sy1: integer;
    dx, dy, len2, t, ddx, ddy, dist, a: double;
  begin
    dx := bx - ax;
    dy := by - ay;
    len2 := dx * dx + dy * dy;
    sx0 := Max(0, Math.Floor(Min(ax, bx)) - pad);
    sx1 := Min(w - 1, Math.Ceil(Max(ax, bx)) + pad);
    sy0 := Max(0, Math.Floor(Min(ay, by)) - pad);
    sy1 := Min(h - 1, Math.Ceil(Max(ay, by)) + pad);

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

        if a > cov[py * w + px] then
        begin
          cov[py * w + px] := a;
          col[py * w + px] := AColor;
        end;
      end;
  end;

begin
  if (Length(APts) < 2) or (Length(AColors) <> Length(APts))
    or (AThickness < 1) then
    Exit;

  minX := APts[0].X;
  maxX := APts[0].X;
  minY := APts[0].Y;
  maxY := APts[0].Y;
  for i := 1 to High(APts) do
  begin
    minX := Min(minX, APts[i].X);
    maxX := Max(maxX, APts[i].X);
    minY := Min(minY, APts[i].Y);
    maxY := Max(maxY, APts[i].Y);
  end;
  pad := AThickness div 2 + 2;
  Dec(minX, pad);
  Dec(minY, pad);
  w := maxX + pad - minX + 1;
  h := maxY + pad - minY + 1;
  if (w < 1) or (h < 1) then
    Exit;

  // The rendered image depends only on the trace's shape, colors and stroke —
  // position lands at blit time — so a repaint with an unmoved trace (hover,
  // overlapping window) reuses the finished raster.
  key := Format('pl|%d', [AThickness]);
  for i := 0 to High(APts) do
    key := key + Format('|%d;%d;%d', [APts[i].X - minX, APts[i].Y - minY,
      integer(AColors[i])]);
  if (TrendLineImage <> nil) and (key = TrendLineKey) then
  begin
    BlitAlphaImage(ACanvas, TrendLineImage, TrendLineW, TrendLineH,
      minX, minY, TrendLineW, TrendLineH);
    Exit;
  end;

  halfT := AThickness / 2.0;
  SetLength({%H-}cov, w * h); // fresh dynarrays: zero-initialized
  SetLength({%H-}col, w * h);

  for i := 0 to High(APts) - 1 do
  begin
    // The color break sits halfway between the dots: two strokes per
    // segment, each running from its dot's center to the midpoint in that
    // dot's color.
    mx := (APts[i].X + APts[i + 1].X) / 2.0 - minX;
    my := (APts[i].Y + APts[i + 1].Y) / 2.0 - minY;
    RasterStroke(APts[i].X - minX, APts[i].Y - minY, mx, my, AColors[i]);
    RasterStroke(mx, my, APts[i + 1].X - minX, APts[i + 1].Y - minY,
      AColors[i + 1]);
  end;

  intf := NewAlphaRaster(w, h);
  try
    for y := 0 to h - 1 do
      for x := 0 to w - 1 do
      begin
        c := ColorToRGB(col[y * w + x]);
        PutAlphaPixel(intf, x, y, Red(c), Green(c), Blue(c), cov[y * w + x]);
      end;
    FreeAndNil(TrendLineImage);
    TrendLineImage := AlphaImageFromIntf(intf, w, h);
    TrendLineKey := key;
    TrendLineW := w;
    TrendLineH := h;
  finally
    intf.Free;
  end;
  BlitAlphaImage(ACanvas, TrendLineImage, w, h, minX, minY, w, h);
end;

// Stroke width in pixels for the connecting line at dot diameter ADiameter,
// per the thin/normal/thick pick (ux.dot_line_width). Proportional to the
// dots, so the chosen weight holds up across window sizes; the floors keep
// each step distinguishable even at the smallest dots. Takes the step as a
// parameter so the settings preview can render the dialog's unsaved pick.
function TrendLineStrokePx(ADiameter, AStep: integer): integer;
begin
  case AStep of
  1:
    Result := Max(1, ADiameter div 10);
  3:
    Result := Max(3, ADiameter div 4);
  else
    Result := Max(2, ADiameter div 6);
  end;
end;

// Ring marking the newest reading's dot. Rim it in a contrasting tone of its
// range color; a same-color ring over a solid disc would be invisible. The
// step is taken in the same direction the dot itself was pushed (away from the
// background) rather than from the dot's own lightness: a ring lightened off
// an already-dark dot lands back on a light background and the rim dissolves
// into it.
//
// Classic keeps its own rim rule (a plain black/white pick off the dot's
// lightness): the mode exists to reproduce the old look, and the
// background-directed step is part of what changed.
// Outline mode already rims every dot, so a rim is no longer what marks the
// newest reading — a heavier one is. Same pole, so the field of dots still
// reads as one set.
//
// Extracted from DotPaint so the settings-dialog preview renders the fresh
// marker through the same rule.
procedure TfBG.FreshDotRing(AMode: TDotColorMode; ADotColor,
  ABackground: TColor; ADiameter: integer; out ARingColor: TColor;
  out ARingWidth: integer);
begin
  if AMode = dcmOutline then
    ARingWidth := Max(2, ADiameter div 7)
  else
    ARingWidth := Max(2, ADiameter div 12);
  if AMode = dcmOutline then
    ARingColor := DotOutlineColor(ABackground)
  else
  if AMode = dcmClassic then
    ARingColor := GetTextColorForBackground(ADotColor)
  else
  if AMode = dcmMono then
  begin
    // Stepping away from the background is what the branch below does, and in
    // this mode the dot is already standing there. The only tone left that the
    // disc cannot swallow is the other pole.
    if ADotColor = clBlack then
      ARingColor := clWhite
    else
      ARingColor := clBlack;
  end
  else
  if IsLightColor(ABackground) then
    ARingColor := DarkenColor(ADotColor, 0.45)
  else
    ARingColor := LightenColor(ADotColor, 0.55);
end;

// The trend surface is transparent to the mouse everywhere except over a
// visible dot: LCL's ControlAtPos asks each control CM_HITTEST before routing
// a mouse message to it, so a "miss" here sends the event to whatever sits
// beneath (the labels, the form) exactly as if the surface did not exist.
// The point arrives control-relative; the surface sits at (0,0) client-sized,
// so it is also a form-client point.
procedure TTrendSurface.CMHitTest(var Message: TCMHitTest);
begin
  if Assigned(FOnHitTest)
    and FOnHitTest(MkPoint(Message.XPos, Message.YPos)) then
    Message.Result := 1
  else
    Message.Result := 0;
end;

// The slot whose laid-out box contains P (surface/client coords). Transient
// use only — the pointer aims into a dynamic array that InitTrendModel
// reallocates. History slots win over prediction slots on overlap.
function TfBG.DotHitAt(const P: TPoint; out AIsPredict: boolean;
  out AIndex: integer): PTrendSlot;
var
  i: integer;
begin
  Result := nil;
  AIsPredict := false;
  AIndex := -1;
  for i := 1 to Min(High(TrendSlots), High(FTrendLayout)) do
    if (i <= High(FTrendVisible)) and FTrendVisible[i]
      and PtInRect(FTrendLayout[i], P) then
    begin
      AIndex := i;
      Exit(@TrendSlots[i]);
    end;
  for i := 0 to Min(High(PredictionSlots), High(FPredictLayout)) do
    if (i <= High(FPredictVisible)) and FPredictVisible[i]
      and PtInRect(FPredictLayout[i], P) then
    begin
      AIsPredict := true;
      AIndex := i;
      Exit(@PredictionSlots[i]);
    end;
end;

function TfBG.TrendSurfaceHit(const P: TPoint): boolean;
var
  isPredict: boolean;
  ix: integer;
begin
  Result := DotHitAt(P, isPredict, ix) <> nil;
end;

procedure TfBG.TrendSurfacePaint(Sender: TObject);
var
  cnv: TCanvas;
  fontn: string;
  slot: PTrendSlot;
  r: TRect;
  ix, d, ringWidth, xSize, xOff, xThick, fs, nPts: integer;
  dotColor, ringColor: TColor;
  xAlpha: double;
  linePts: array of TPoint;
  lineCols: array of TColor;
begin
  if FTrendSurface = nil then
    Exit;
  cnv := FTrendSurface.Canvas;

  // The optional connecting line (ux.dot_line): consecutive visible history
  // slots joined center to center, drawn first so the dots sit on top of it.
  // Gap markers keep the trace continuous — their interpolated height is on
  // the straight line between the flanking readings anyway — and the
  // prediction × marks stay unconnected: a forecast is not a trace. Skipped
  // while the dots are expanded into their value text.
  if TrendLineEnabled and not FDotsExpanded then
  begin
    SetLength({%H-}linePts, Length(FTrendLayout));
    SetLength({%H-}lineCols, Length(FTrendLayout));
    nPts := 0;
    for ix := 1 to Min(High(TrendSlots), High(FTrendLayout)) do
      if FTrendVisible[ix] then
      begin
        r := FTrendLayout[ix];
        linePts[nPts] := MkPoint(r.Left + r.Width div 2,
          r.Top + r.Height div 2);
        // The line wears the dots' own colors — through the same mode
        // pipeline as the dots, softened toward the window so the trace
        // stays quieter than the readings it connects. A gap slot has no
        // reading and thus no range color: its stretch of line takes the
        // same background-derived tone as the gap ring it runs through.
        if TrendSlots[ix].kind = dkGap then
          lineCols[nPts] := BlendColors(GetTextColorForBackground(fBG.Color),
            fBG.Color, GAP_DOT_BLEND)
        else
          lineCols[nPts] := BlendColors(DotDisplayColor(TrendSlots[ix].ident,
            DotBandPartner(TrendSlots[ix].ident), fBG.Color, lVal.Font.Color,
            DotColorMode), fBG.Color, TREND_LINE_BLEND);
        Inc(nPts);
      end;
    SetLength(linePts, nPts);
    SetLength(lineCols, nPts);
    if nPts >= 2 then
      DrawSmoothPolyline(cnv, linePts, lineCols,
        TrendLineStrokePx(FTrendDotDiameter, TrendLineWidthStep));
  end;

  for ix := 1 to Min(High(TrendSlots), High(FTrendLayout)) do
  begin
    if not FTrendVisible[ix] then
      Continue;
    slot := @TrendSlots[ix];
    r := FTrendLayout[ix];

    // Reading dots expand into their value text on click; gap markers never do.
    if FDotsExpanded and (slot^.kind = dkReading) then
    begin
      // The color has to be transformed here: the slot carries the dot's
      // *identity* color — the range color, which is the window background's
      // own color whenever the reading falls in the band the window is
      // currently showing. Run the mode's transform first so a clicked dot
      // keeps the look the user picked, then apply the text floor on top:
      // whichever mode is active, a number the user asked to see has to be
      // readable. That includes Classic, which leaves the dot *marker*
      // unguarded on purpose.
      cnv.Brush.Style := bsClear;
      if FontTXTInList(fontn) then
        cnv.Font.Name := fontn;
      fs := Max(8, lVal.Font.Size div Max(ACTIVE_DOTS, 1));
      cnv.Font.Size := fs;
      cnv.Font.Color := EnsureContrast(
        DotDisplayColor(slot^.ident, DotBandPartner(slot^.ident), fBG.Color,
        lVal.Font.Color, DotColorMode), fBG.Color, DOT_TEXT_MIN_CONTRAST);
      cnv.TextOut(r.Left, r.Top, slot^.text);
      Continue;
    end;

    d := r.Width;
    if d < 2 then
      Continue;

    if slot^.kind = dkGap then
    begin
      // A gap marker is deliberately quiet: a thin hollow ring sunk most of
      // the way into the background, sitting at the trace's interpolated
      // height, so a missing reading says "something should be here" without
      // competing with the real dots around it. Slightly smaller than the
      // data dots, and ring-only (clNone disc) so the range bands behind it
      // stay visible through the hole. Drawn from the window tone — a gap has
      // no reading and thus no range color.
      xSize := Max(MIN_DOT_DIAMETER_PX, Round(d * 0.8));
      xOff  := (d - xSize) div 2;
      DrawSmoothCircle(cnv, xSize, clNone,
        BlendColors(GetTextColorForBackground(fBG.Color), fBG.Color,
        GAP_DOT_BLEND), Max(1, xSize div 8), r.Left + xOff, r.Top + xOff);
      Continue;
    end;

    // DetermineColorForReading stored the dot's range color; separating it
    // from the background is this side's job, because only here is the
    // background final (see the note there). fBG.Color is the reference even
    // inside a range band: the bands composite at a low alpha and only ever
    // darken, so a dot cleared against the untinted color is cleared against
    // the band too.
    dotColor := DotDisplayColor(slot^.ident, DotBandPartner(slot^.ident),
      fBG.Color, lVal.Font.Color, DotColorMode);

    // The newest reading carries the fresh ring while the data is fresh —
    // derived here rather than stored, so a freshness flip repaints right.
    if (ix = ACTIVE_DOTS) and FLatestIsFresh then
      FreshDotRing(DotColorMode, dotColor, fBG.Color, d, ringColor, ringWidth)
    else
    if DotColorMode = dcmOutline then
    begin
      // The rim is the whole visibility budget in this mode: the fill is the
      // untouched range color and may be the window's own color exactly. Thin,
      // because it has to sit inside dots that get small on a dense trend
      // without eating the color it exists to protect.
      ringWidth := Max(1, d div 14);
      ringColor := DotOutlineColor(fBG.Color);
    end
    else
    begin
      ringWidth := 0;
      ringColor := dotColor;
    end;

    DrawSmoothCircle(cnv, d, dotColor, ringColor, ringWidth, r.Left, r.Top);
  end;

  // Prediction × marks in the trailing slots.
  for ix := 0 to Min(High(PredictionSlots), High(FPredictLayout)) do
  begin
    if not FPredictVisible[ix] then
      Continue;
    slot := @PredictionSlots[ix];
    r := FPredictLayout[ix];
    d := r.Width;
    if d < 2 then
      Continue;

    dotColor := DotDisplayColor(slot^.ident, DotBandPartner(slot^.ident),
      fBG.Color, lVal.Font.Color, DotColorMode);
    // A × is strokes, not a disc, and nothing rims it — so the one mode that
    // leaves the fill unlifted has to lift it here or the forecast mark
    // disappears on the window its own range owns.
    if DotColorMode = dcmOutline then
      dotColor := EnsureContrast(dotColor, fBG.Color, DOT_MIN_CONTRAST);

    // Uncertainty is opacity: the × fades as the engine's confidence in the
    // forecast drops, and steps down once more per horizon slot so the
    // 15-minute mark always reads as less certain than the 5-minute one.
    // PREDICTION_ALPHA_MIN keeps the mark findable even at zero confidence;
    // clicking the dot still shows the exact percentage.
    xAlpha := (PREDICTION_ALPHA_MIN + (1.0 - PREDICTION_ALPHA_MIN)
      * EnsureRange(PredictionConfidence, 0.0, 1.0))
      * (1.0 - ix * PREDICTION_HORIZON_FADE);

    // Compact × (multiplication-sign) shape: shorter, thinner strokes inset
    // from the slot edges, centred in the same square a history dot would
    // occupy. Drawn rather than typeset so we don't depend on a font that
    // ships the glyph at consistent metrics across platforms.
    xSize := Max(MIN_DOT_DIAMETER_PX, Round(d * 0.6));
    xOff  := (d - xSize) div 2;
    xThick := Max(2, xSize div 8);
    // A forecast at or below the low threshold is the one prediction that
    // must not fade into the background: full opacity and a heavier stroke,
    // whatever the confidence fade would otherwise have done to it, so an
    // incoming low stands out at a glance.
    if slot^.lowPredict then
    begin
      xAlpha := 1.0;
      xThick := Max(3, xSize div 5);
    end;
    DrawSmoothX(cnv, xSize, dotColor, xThick, r.Left + xOff, r.Top + xOff,
      xAlpha);
  end;
end;

procedure TfBG.TrendSurfaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Remember which dot the press landed on: release must hit the same dot to
  // count as a click (mirrors the old per-control Click semantics).
  DotHitAt(MkPoint(X, Y), FDownIsPredict, FDownDotIx);
end;

procedure TfBG.TrendSurfaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  slot: PTrendSlot;
  isPredict, isDot: boolean;
  ix: integer;
  H, M, S, MS: word;
begin
  if Button <> mbLeft then
    Exit;
  slot := DotHitAt(MkPoint(X, Y), isPredict, ix);
  if (slot = nil) or (isPredict <> FDownIsPredict) or (ix <> FDownDotIx) then
    Exit;

  // Show how much the prediction engine trusts the forecast behind the ×.
  if slot^.kind = dkPredict then
  begin
    ShowMessage(Format(RS_PREDICTION_CONFIDENCE, [PredictionConfidence * 100]));
    Exit;
  end;

  DecodeTime(slot^.time, H, M, S, MS);

  // A gap marker has no reading behind it: nothing to expand, and its
  // synthetic placement value must not reach the JS bridge. Explain the hole
  // instead — the slot carries its nominal time.
  if slot^.kind = dkGap then
  begin
    ShowMessage(Format(RS_UX_GAP_HERE, [H, M]));
    Exit;
  end;

  // All reading dots expand or collapse together; privacy mode keeps the
  // values hidden, so the view state stays put there.
  if not privacyMode then
    FDotsExpanded := not FDotsExpanded;
  UpdateTrendDots;
  isDot := not FDotsExpanded;

  {$ifdef TrndiExt}
  callFunc('dotClicked',[
    isDot,
    slot^.value * BG_CONVERTIONS[mgdl][mmol],
    slot^.value,
    // The bridge's historical time encoding — keep it stable for extensions.
    H * 100 + M
    ]);
  {$endif}
end;

// Hover tooltip: the surface's Hint follows whichever dot the cursor is over.
// Between dots the surface is hit-transparent, so leaving a dot leaves the
// control and the next dot re-arms the hint like the per-dot controls did.
procedure TfBG.TrendSurfaceMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  slot: PTrendSlot;
  isPredict: boolean;
  ix: integer;
begin
  if FTrendSurface = nil then
    Exit;
  slot := DotHitAt(MkPoint(X, Y), isPredict, ix);
  if (slot <> nil) and (slot^.text <> '') and not privacyMode then
  begin
    FTrendSurface.Hint := slot^.text;
    FTrendSurface.ShowHint := true;
  end
  else
    FTrendSurface.ShowHint := false;
end;

// The sample strip under the settings dialog's dot-coloring options: three
// miniature Trndi windows (in-range, high, low), each with a rising trend that
// visits every range band and a hero reading behind it. Every color runs
// through the same pipeline as the live dots — DotDisplayColor, FreshDotRing,
// DotOutlineColor — so the preview cannot drift from what the mode really
// does. ATheme carries the dialog's current picker colors, which may be
// unsaved; that is the point, the preview answers "what would this look like"
// before the user commits.
// Identity/partner colors per range band (0 = low .. 4 = high), exactly as
// DetermineColorForReading assigns them, with partners per DotBandPartner —
// in-range is the inverted pair, the text half is the identity. Shared by
// both settings-dialog previews so they cannot disagree.
procedure PreviewBandColors(const ATheme: TTrndiTheme; ABand: integer;
  out AIdent, APartner: TColor);
begin
  case ABand of
  0:
  begin
    AIdent := ATheme.ColorLow;
    APartner := ATheme.ColorLowText;
  end;
  1:
  begin
    AIdent := ATheme.ColorRangeLow;
    APartner := ATheme.ColorRangeLowText;
  end;
  2:
  begin
    AIdent := ATheme.ColorOkText;
    APartner := ATheme.ColorOk;
  end;
  3:
  begin
    AIdent := ATheme.ColorRangeHigh;
    APartner := ATheme.ColorRangeHighText;
  end;
  else
  begin
    AIdent := ATheme.ColorHigh;
    APartner := ATheme.ColorHighText;
  end;
  end;
end;

procedure TfBG.RenderDotModePreview(ACanvas: TCanvas; const ARect: TRect;
  AModeIndex: integer; const ATheme: TTrndiTheme);
const
  // The digits exist to show dcmAuto's digit-avoidance, so any number works;
  // no locale separator, to stay unit- and locale-neutral.
  SAMPLE_VALUE = '123';
  GAP = 4; // between window swatches
  PAD = 6; // swatch-internal margin
var
  mode: TDotColorMode;
  winBgs: array[0..2] of TColor;
  i, j, d, x, y, ringWidth, swW, swH: integer;
  sw: TRect;
  valColor, dotColor, ringColor, ident, partner: TColor;
begin
  mode := DotColorModeFromSetting(AModeIndex);

  // One miniature window per background the app actually paints with a trend
  // on it: in-range, high, low.
  winBgs[0] := ATheme.ColorOk;
  winBgs[1] := ATheme.ColorHigh;
  winBgs[2] := ATheme.ColorLow;

  swW := (ARect.Width - 2 * GAP) div 3;
  swH := ARect.Height;
  if (swW < 2 * PAD + 8) or (swH < 2 * PAD + 8) then
    Exit;
  d := Max(MIN_DOT_DIAMETER_PX, Min(swH div 3, (swW - 2 * PAD) div 7));

  for i := 0 to 2 do
  begin
    // Classes.Rect: on macOS the MacOSAll unit's QuickDraw Rect type shadows
    // the RTL function, turning a bare Rect(a,b,c,d) into a failed typecast.
    sw := Classes.Rect(ARect.Left + i * (swW + GAP), ARect.Top,
      ARect.Left + i * (swW + GAP) + swW, ARect.Top + swH);
    if i = 2 then // absorb the integer-division remainder
      sw.Right := ARect.Right;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := winBgs[i];
    ACanvas.FillRect(sw);

    // The hero reading, colored as UpdateUIColors would color it on this
    // window, so dcmAuto's digit-avoidance is part of what the preview shows.
    valColor := GetTextColorForBackground(winBgs[i]);
    ACanvas.Font.Color := valColor;
    ACanvas.Font.Height := -(swH * 3 div 5);
    ACanvas.Font.Style := [fsBold];
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(sw.Left + (sw.Width - ACanvas.TextWidth(SAMPLE_VALUE)) div 2,
      sw.Top + (swH - ACanvas.TextHeight(SAMPLE_VALUE)) div 2, SAMPLE_VALUE);

    // Oldest to newest, climbing through the bands; the newest dot carries the
    // fresh ring like the live graph's does.
    for j := 0 to 4 do
    begin
      PreviewBandColors(ATheme, j, ident, partner);
      dotColor := DotDisplayColor(ident, partner, winBgs[i], valColor, mode);
      if j = 4 then
        FreshDotRing(mode, dotColor, winBgs[i], d, ringColor, ringWidth)
      else
      if mode = dcmOutline then
      begin
        // Same rim rule as DotPaint's non-fresh outline branch.
        ringWidth := Max(1, d div 14);
        ringColor := DotOutlineColor(winBgs[i]);
      end
      else
      begin
        ringWidth := 0;
        ringColor := dotColor;
      end;
      x := sw.Left + PAD + j * (sw.Width - 2 * PAD - d) div 4;
      y := sw.Top + PAD + (4 - j) * (swH - 2 * PAD - d) div 4;
      DrawSmoothCircle(ACanvas, d, dotColor, ringColor, ringWidth, x, y);
    end;
  end;
end;

// The Display tab's miniature main window: one full mock of the live screen —
// ago text, TIR, trend dots, reading, arrow and delta — drawn with the fonts
// being picked in the dialog and the colors currently in the pickers, in the
// range state the dialog's switcher selects. Every color comes from the same
// functions the live UI uses (UpdateUIColors' rules, DotDisplayColor,
// FreshDotRing), so the preview cannot drift from what saving would produce.
// The captions are read from the live labels, so the mock shows the user's
// actual current reading rather than canned data.
procedure TfBG.RenderDisplayPreview(ACanvas: TCanvas; const ARect: TRect;
  AModeIndex: integer; const ATheme: TTrndiTheme;
  const AData: TDisplayPreviewData; out AZones: TDisplayPreviewZones);
const
  PAD = 6;
  DOT_COUNT = 10; // matches the default trend window
  clGoodGreen = $00005900; // keep in sync with UpdateUIColors
var
  mode: TDotColorMode;
  bg, valColor, subColor, arrowColor, tirColor: TColor;
  dotColor, ringColor, ident, partner: TColor;
  valText, arrowText, agoText, diffText, tirText: string;
  w, h, i, d, x, y, tw, th, ringWidth, r, cols, gs: integer;
  linePts: array of TPoint;
  lineCols: array of TColor;

function HasDigit(const s: string): boolean;
  var
    c: char;
  begin
    Result := false;
    for c in s do
      if c in ['0'..'9'] then
        exit(true);
  end;

  // The dialog's separator field applies to the reading on save; showing it
  // in the mock is the point of previewing.
function SwapSep(const s: string): string;
  begin
    Result := s;
    if AData.DecimalSep <> '' then
    begin
      Result := StringReplace(Result, ',', AData.DecimalSep, [rfReplaceAll]);
      Result := StringReplace(Result, '.', AData.DecimalSep, [rfReplaceAll]);
    end;
  end;

procedure SetPreviewFont(const AName: string; AHeight: integer;
  const AStyle: TFontStyles);
  begin
    if AName <> '' then
      ACanvas.Font.Name := AName
    else
      ACanvas.Font.Name := 'default';
    ACanvas.Font.Height := -AHeight;
    ACanvas.Font.Style := AStyle;
  end;

begin
  AZones.ValRect := Classes.Rect(0, 0, 0, 0);
  AZones.ArrowRect := Classes.Rect(0, 0, 0, 0);
  AZones.AgoRect := Classes.Rect(0, 0, 0, 0);

  w := ARect.Width;
  h := ARect.Height;
  if (w < 40) or (h < 30) then
    Exit;

  mode := DotColorModeFromSetting(AModeIndex);

  case AData.State of
  1:
    bg := ATheme.ColorHigh;
  2:
    bg := ATheme.ColorLow;
  else
    bg := ATheme.ColorOk;
  end;

  // The same rules UpdateUIColors applies to the live labels.
  valColor := GetTextColorForBackground(bg);
  subColor := GetTextColorForBackground(bg, 0.6, 0.4);
  arrowColor := LightenColor(bg, 0.5);

  // Live captions, with fallbacks for the states that have none (first run,
  // rotating-arrow overlay active).
  valText := lVal.Caption;
  if not HasDigit(valText) then
    valText := '5.5';
  valText := SwapSep(valText);
  arrowText := lArrow.Caption;
  if (arrowText = '') or (arrowText = 'lArrow') then
    arrowText := '→';
  agoText := lAgo.Caption;
  if (agoText = '') or (agoText = 'lAgo') then
    agoText := '10 min';
  diffText := lDiff.Caption;
  if (diffText = '') or (diffText = 'lDiff') then
    diffText := '+0.0';
  diffText := SwapSep(diffText);
  tirText := lTir.Caption;

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := bg;
  ACanvas.FillRect(ARect);
  ACanvas.Brush.Style := bsClear;

  // Ago readout, top-left like the live window.
  SetPreviewFont(AData.AgoFont, Max(11, h div 9), []);
  ACanvas.Font.Color := subColor;
  ACanvas.TextOut(ARect.Left + PAD, ARect.Top + PAD div 2 + 2, agoText);

  // TIR, top-right, with UpdateUIColors' good/bad tinting when the live
  // label carries the percentage in its hint.
  if tirText <> '' then
  begin
    tirColor := subColor;
    if TryStrToInt(lTir.Hint, r) then
      if r < bad_tir then
        tirColor := GetAdjustedColorForBackground(clMaroon, bg, 0.6, 0.4, true)
      else
      if r >= good_tir then
        tirColor := GetAdjustedColorForBackground(clGoodGreen, bg, 0.6, 0.4, true);
    ACanvas.Font.Color := tirColor;
    ACanvas.TextOut(ARect.Left + w - ACanvas.TextWidth(tirText) - PAD,
      ARect.Top + PAD div 2 + 2, tirText);
  end;

  // Arrow before reading: on the live window lArrow spans the whole client
  // area with the glyph centered (ScaleLbl fits it to the full window) and
  // sits behind lVal — a big faint glyph under the number, not a row of its
  // own. Mirror that: centered on the box, sized near its height.
  SetPreviewFont(AData.ArrowFont, (h * 4) div 5, [fsBold]);
  ACanvas.Font.Color := arrowColor;
  tw := ACanvas.TextWidth(arrowText);
  th := ACanvas.TextHeight(arrowText);
  ACanvas.TextOut(ARect.Left + (w - tw) div 2,
    ARect.Top + (h - th) div 2, arrowText);

  // The hero reading, centered on the window like the live lVal.
  SetPreviewFont(AData.ValFont, (h * 2) div 5, []);
  ACanvas.Font.Color := valColor;
  tw := ACanvas.TextWidth(valText);
  th := ACanvas.TextHeight(valText);
  ACanvas.TextOut(ARect.Left + (w - tw) div 2,
    ARect.Top + (h - th) div 2, valText);

  // Delta, bottom edge, honoring the saved lDiff alignment.
  SetPreviewFont('', Max(10, h div 11), []);
  ACanvas.Font.Color := subColor;
  tw := ACanvas.TextWidth(diffText);
  th := ACanvas.TextHeight(diffText);
  case lDiff.Alignment of
  taLeftJustify:
    x := ARect.Left + PAD;
  taRightJustify:
    x := ARect.Left + w - tw - PAD;
  else
    x := ARect.Left + (w - tw) div 2;
  end;
  ACanvas.TextOut(x, ARect.Top + h - th - 3, diffText);

  // A rising trend visiting every range band, sized and slotted like the live
  // dots (0.6 of the slot width, clamped), newest on the right carrying the
  // fresh ring when the option previews it. Prediction slots compress the
  // history to the left, exactly as TrendColumnCount does on the main window.
  cols := DOT_COUNT;
  if AData.PredictDots then
    Inc(cols, PREDICTION_DOT_COUNT);
  d := Max(MIN_DOT_DIAMETER_PX, Min(h div 8, (w * 6) div (cols * 10)));

  // The connecting line, when the option previews it: the climb's dot
  // centers joined in the dots' own softened colors, with the same
  // mid-segment color breaks and under-the-dots order as the live surface,
  // so ticking the box shows exactly what the graph will do. The gap slot's
  // stretch takes the gap-ring tone, like DotPaint's.
  if AData.TrendLine then
  begin
    SetLength({%H-}linePts, DOT_COUNT);
    SetLength({%H-}lineCols, DOT_COUNT);
    for i := 0 to DOT_COUNT - 1 do
    begin
      linePts[i] := MkPoint(
        ARect.Left + Round((i + 0.5) * w / cols),
        ARect.Top + PAD + (((DOT_COUNT - 1 - i) * (h - 2 * PAD - d))
        div (DOT_COUNT - 1) * 3) div 4 + d div 2);
      if AData.GapRing and (i = DOT_COUNT div 2) then
        lineCols[i] := BlendColors(GetTextColorForBackground(bg), bg,
          GAP_DOT_BLEND)
      else
      begin
        PreviewBandColors(ATheme, i div 2, ident, partner);
        lineCols[i] := BlendColors(DotDisplayColor(ident, partner, bg,
          valColor, mode), bg, TREND_LINE_BLEND);
      end;
    end;
    DrawSmoothPolyline(ACanvas, linePts, lineCols,
      TrendLineStrokePx(d, AData.LineWidth));
  end;

  for i := 0 to DOT_COUNT - 1 do
  begin
    x := ARect.Left + Round((i + 0.5) * w / cols) - d div 2;
    // Compress the climb into the upper ~3/4 so the delta row stays readable.
    y := ARect.Top + PAD + (((DOT_COUNT - 1 - i) * (h - 2 * PAD - d))
      div (DOT_COUNT - 1) * 3) div 4;

    // One mid-trend slot renders as a sensor gap when the option is on — the
    // same quiet hollow ring DotPaint draws, at the height the climb passes
    // through anyway (the preview's trace is linear).
    if AData.GapRing and (i = DOT_COUNT div 2) then
    begin
      gs := Max(MIN_DOT_DIAMETER_PX, Round(d * 0.8));
      DrawSmoothCircle(ACanvas, gs, clNone,
        BlendColors(GetTextColorForBackground(bg), bg, GAP_DOT_BLEND),
        Max(1, gs div 8), x + (d - gs) div 2, y + (d - gs) div 2);
      Continue;
    end;

    PreviewBandColors(ATheme, i div 2, ident, partner);
    dotColor := DotDisplayColor(ident, partner, bg, valColor, mode);
    if (i = DOT_COUNT - 1) and AData.FreshRing then
      FreshDotRing(mode, dotColor, bg, d, ringColor, ringWidth)
    else
    if mode = dcmOutline then
    begin
      // Same rim rule as DotPaint's non-fresh outline branch.
      ringWidth := Max(1, d div 14);
      ringColor := DotOutlineColor(bg);
    end
    else
    begin
      ringWidth := 0;
      ringColor := dotColor;
    end;
    DrawSmoothCircle(ACanvas, d, dotColor, ringColor, ringWidth, x, y);
  end;

  // Prediction × marks in the trailing slots, colored through the same
  // pipeline as DotPaint's dkPredict branch and carrying the same
  // per-horizon fade (at full confidence — the preview has no engine to ask).
  // Flat, at the newest dot's height: the preview forecasts "steady".
  if AData.PredictDots then
  begin
    PreviewBandColors(ATheme, 4, ident, partner);
    dotColor := DotDisplayColor(ident, partner, bg, valColor, mode);
    if mode = dcmOutline then
      dotColor := EnsureContrast(dotColor, bg, DOT_MIN_CONTRAST);
    gs := Max(MIN_DOT_DIAMETER_PX, Round(d * 0.6));
    y := ARect.Top + PAD; // the climb's endpoint (i = DOT_COUNT - 1)
    for i := 0 to PREDICTION_DOT_COUNT - 1 do
    begin
      x := ARect.Left + Round((DOT_COUNT + i + 0.5) * w / cols) - d div 2;
      DrawSmoothX(ACanvas, gs, dotColor, Max(2, gs div 8),
        x + (d - gs) div 2, y + (d - gs) div 2,
        1.0 - i * PREDICTION_HORIZON_FADE);
    end;
  end;

  // Click targets for the dialog's font pickers: ago strip on top, reading
  // band in the middle, arrow band below it. The glyph and the number overlap
  // visually (as on the live window), so the arrow's target is the lower part
  // of the glyph, clear of the digits.
  // Anchored on ARect like every draw call above, so the zones stay aligned
  // with what was painted even if the box is ever given a non-origin rect.
  AZones.AgoRect := Classes.Rect(ARect.Left, ARect.Top,
    ARect.Left + w div 2, ARect.Top + h div 5);
  AZones.ValRect := Classes.Rect(ARect.Left, ARect.Top + h div 5,
    ARect.Left + w, ARect.Top + (h * 3) div 5);
  AZones.ArrowRect := Classes.Rect(ARect.Left, ARect.Top + (h * 3) div 5,
    ARect.Left + w, ARect.Top + (h * 9) div 10);
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

// Free and clear the trend-dot controls so CreateTrendDots can rebuild them at
// a new ACTIVE_DOTS count. The array is 1-based (index 0 unused). Callers must
// follow up with CreateTrendDots, which also re-points the named lDotN aliases.
// (Re)allocate the slot model for the current ACTIVE_DOTS and make sure the
// trend surface exists. History slots come back zeroed (dkEmpty) for
// PlaceTrendDots to refill; prediction slots survive dot-count changes, so
// they are only allocated when missing.
procedure TfBG.InitTrendModel;
begin
  SetLength(TrendSlots, 0); // force zero-init of the re-allocated slots
  SetLength(TrendSlots, ACTIVE_DOTS + 1);
  SetLength(FTrendLayout, ACTIVE_DOTS + 1);
  SetLength(FTrendVisible, 0);
  SetLength(FTrendVisible, ACTIVE_DOTS + 1);

  if Length(PredictionSlots) <> PREDICTION_DOT_COUNT then
  begin
    SetLength(PredictionSlots, PREDICTION_DOT_COUNT);
    SetLength(FPredictLayout, PREDICTION_DOT_COUNT);
    SetLength(FPredictVisible, PREDICTION_DOT_COUNT);
  end;

  if FTrendSurface = nil then
  begin
    FTrendSurface := TTrendSurface.Create(Self);
    with FTrendSurface do
    begin
      Parent := Self;
      AutoSize := false;
      SetBounds(0, 0, Self.ClientWidth, Self.ClientHeight);
      OnHitTest := @TrendSurfaceHit;
      OnPaint := @TrendSurfacePaint;
      OnMouseDown := @TrendSurfaceMouseDown;
      OnMouseUp := @TrendSurfaceMouseUp;
      OnMouseMove := @TrendSurfaceMouseMove;
      PopupMenu := pmSettings;
      Visible := true;
    end;
  end;

  TrndiDLog(Format('Trend model initialized: %d slots (%d min window)',
    [ACTIVE_DOTS, ACTIVE_DOTS * INTERVAL_MINUTES]));
end;

// Apply a new main-trend dot count: persist it, resize the slot model and
// reflow the graph. Shared by the trend-window submenu and the manual
// "Set Dot Count" service-menu entry.
procedure TfBG.ApplyTrendDotCount(newCount: integer);
begin
  if (newCount < 1) or (newCount > MAX_DOT_COUNT) then Exit;

  native.SetSetting('ux.dot_count', newCount);
  ACTIVE_DOTS := newCount;
  if MAX_MIN < ACTIVE_DOTS * INTERVAL_MINUTES + 10 then
    MAX_MIN := ACTIVE_DOTS * INTERVAL_MINUTES + 10;

  // MAX_RESULT is a reading count, not minutes. Scale by the API's reporting
  // interval so backends that report more often than every INTERVAL_MINUTES
  // still produce enough readings to fill all dot slots.
  if Assigned(api) then
    MAX_RESULT := Max(MAX_RESULT,
      ACTIVE_DOTS * Max(1, (INTERVAL_MINUTES + api.getReportingInterval - 1)
                   div api.getReportingInterval))
  else if MAX_RESULT < ACTIVE_DOTS then
    MAX_RESULT := ACTIVE_DOTS;

  InitTrendModel;

  // Force PlaceTrendDots to reprocess even though bgs hasn't changed.
  FLastReadingsHash := 0;

  if Length(bgs) > 0 then
    PlaceTrendDots(bgs);
  UpdateTrendDots;
end;

// Wheel-driven dot-count adjustment: each notch up adds a trend dot, each
// notch down removes one. ApplyTrendDotCount rebuilds the slot model and
// re-places the readings, so the apply is debounced through tDotScroll — a
// fast flick lands as one rebuild at the final count instead of one per
// notch. Wired as
// fBG.OnMouseWheel; the LCL bubbles wheel events unhandled by the child
// labels/panels up to the form, so scrolling works anywhere over the window.
procedure TfBG.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
const
  WHEEL_NOTCH = 120; // Standard delta per detent; touchpads send fractions
  DEBOUNCE_MS = 200;
var
  steps: integer;
begin
  Handled := true;

  // Accumulate sub-notch deltas (smooth-scrolling touchpads) and pull out the
  // whole notches gathered so far. FPC's div truncates toward zero, so the
  // arithmetic is symmetric in both scroll directions.
  Inc(FDotScrollAccum, WheelDelta);
  steps := FDotScrollAccum div WHEEL_NOTCH;
  if steps = 0 then
    Exit;
  Dec(FDotScrollAccum, steps * WHEEL_NOTCH);

  if FDotScrollTarget = 0 then
    FDotScrollTarget := ACTIVE_DOTS;
  FDotScrollTarget := EnsureRange(FDotScrollTarget + steps, 1, MAX_DOT_COUNT);

  if tDotScroll = nil then
  begin
    tDotScroll := TTimer.Create(Self);
    tDotScroll.Enabled := false;
    tDotScroll.Interval := DEBOUNCE_MS;
    tDotScroll.OnTimer := @tDotScrollTimer;
  end;
  // Restart the debounce window on every notch
  tDotScroll.Enabled := false;
  tDotScroll.Enabled := true;
end;

// Debounce expiry: apply the accumulated wheel target in a single rebuild.
procedure TfBG.tDotScrollTimer(Sender: TObject);
var
  newCount: integer;
begin
  tDotScroll.Enabled := false;
  newCount := FDotScrollTarget;
  FDotScrollTarget := 0;
  FDotScrollAccum := 0;
  if (newCount > 0) and (newCount <> ACTIVE_DOTS) then
    ApplyTrendDotCount(newCount);
end;

// Mark empty trend slots that provably sit between two real readings as
// sensor gaps. The gap slot carries a linearly interpolated value so the
// shared layout pipeline (UpdateTrendDots → SetPointHeight → AdjustGraph)
// places it where the trace would pass; the dkGap kind keeps that synthetic
// value out of tooltips, click-expansion and the JS bridge. An empty slot
// only becomes a gap when readings on both sides prove one is missing
// between them: interior emptiness always qualifies, and a leading run (the
// oldest slots) qualifies when the caller found a reading beyond the
// window's left edge — AOlderPos/AOlderVal then anchor the interpolation at
// that reading's fractional slot position (≤ 0), so the leading gaps ride
// the true time span instead of pretending the outside reading sits one slot
// away. Trailing emptiness (an outage marching the trace left) is never a
// gap — the stale UI owns that story. Runs at the end of every placement
// pass, which fully rebuilds slot state first, so stale gap markers from the
// previous pass cannot survive here.
procedure TfBG.MarkTrendGaps(AHasOlder: boolean; AOlderPos, AOlderVal: double;
  AAnchor: TDateTime);
var
  ix, g: integer;
  prevPos, vPrev, vNext: double;
  havePrev: boolean;

  // A slot counts as filled only when it holds a real reading: gap slots are
  // excluded by kind so a previous pass's markers can't seed new ones.
  function ParseSlot(i: integer; out v: double): boolean;
  begin
    v := 0;
    Result := TrendSlots[i].kind = dkReading;
    if Result then
      v := TrendSlots[i].value;
  end;

begin
  // Optional, like the fresh ring (ux.dot_fresh). Placement rebuilt every
  // slot before this runs, so turning the option off simply leaves the empty
  // slots hidden — no stale markers to clean up.
  if not native.GetBoolSetting('ux.dot_gaps', true) then
    Exit;

  // The left anchor starts as the caller's outside-the-window reading (if
  // any) and becomes each filled slot in turn.
  havePrev := AHasOlder;
  prevPos := AOlderPos;
  vPrev := AOlderVal;

  for ix := 1 to High(TrendSlots) do
  begin
    if not ParseSlot(ix, vNext) then
      Continue;

    if havePrev then
      // Floor(prevPos) + 1 is the first whole slot right of the anchor; for a
      // filled-slot anchor that is simply the next slot.
      for g := Max(1, Math.Floor(prevPos) + 1) to ix - 1 do
      begin
        TrendSlots[g].kind := dkGap;
        // Interpolated in mmol like every slot value; linear in slot
        // position, which is linear in time on the fixed slot grid.
        TrendSlots[g].value := vPrev + (vNext - vPrev) * (g - prevPos)
          / (ix - prevPos);
        // A gap has no reading: no text, so tooltips and click-expansion
        // can never surface the synthetic value.
        TrendSlots[g].text := '';
        // The slot's nominal reading time, so the popup menu and the click
        // message can say *when* the reading is missing.
        TrendSlots[g].time :=
          IncMinute(AAnchor, -INTERVAL_MINUTES * (ACTIVE_DOTS - g));
      end;

    havePrev := true;
    prevPos := ix;
    vPrev := vNext;
  end;
end;

// Keep the graph honest for the duration of an outage. Dot slots are anchored
// to wall-clock INTERVAL_MINUTES boundaries, so with no new reading arriving the
// trace has to march left and leave the newest slots empty — otherwise the last
// dot sits in the "now" column for the whole outage and the graph reads as live
// while the card underneath says there is no data. The fresh path gets this for
// free from ApplyFetchedReadings; the stale path never reached PlaceTrendDots.
//
// Called once a second from tMissed, so the work has to be conditional:
// PlaceTrendDots exits untouched until the slot grid or the readings move and
// reports which of the two happened, so the relayout below runs only on the
// ticks that actually moved a dot.
//
// The relayout is this function's own job. On the fresh path PlaceTrendDots
// is followed by an immediate relayout and FinalizeUpdate's deferred resize;
// nothing schedules those during an outage.
procedure TfBG.RefreshStaleTrendDots;
begin
  if firstboot or FShuttingDown then
    Exit;
  if Length(bgs) = 0 then
    Exit;

  if not PlaceTrendDots(bgs) then
    Exit;

  UpdateTrendDots;
end;

// The whole layout pipeline in one pass: for every slot, the column grid
// gives the X, the value gives the Y (SetPointHeight's formula, centred on
// the diameter), DOT_ADJUST shifts the row, and the keep-in-view correction
// pulls the trace back inside the window. The results land in
// FTrendLayout/FPredictLayout for the surface's paint and hit test.
procedure TfBG.UpdateTrendDots;
var
  cw, ch, cols, stride, ix, d, pd, y, da, pass: integer;
  infos: array of TDotInfo;
  bmp: TBitmap;
  fontn: string;

  // SetPointHeight's value→Top mapping, minus the control: 10% padding at
  // both edges, values clamped to the API's representable range, +Y down.
  function ValueTop(v: single): integer;
  var
    Padding, UsableHeight, Position: integer;
  begin
    Padding := Round(ch * 0.1);
    UsableHeight := ch - (Padding * 2);
    if v < BG_API_MIN then
      v := BG_API_MIN
    else if v > BG_API_MAX then
      v := BG_API_MAX;
    Position := Padding + Round((v - BG_API_MIN) / (BG_API_MAX - BG_API_MIN)
      * UsableHeight);
    Result := (ch - Position) - 1;
  end;

  // Centered X of grid column aix (1-based over the full history+prediction
  // grid) for a box adiam wide, clamped inside the window.
  function SlotLeft(aix, adiam: integer): integer;
  var
    leftPx, rightPx: integer;
  begin
    leftPx := Round((aix - 1) * cw / cols);
    rightPx := Round(aix * cw / cols);
    if rightPx > cw then rightPx := cw;
    Result := Max(0, Min(leftPx + (rightPx - leftPx - adiam) div 2, cw - adiam));
  end;

begin
  if (FTrendSurface = nil) or FShuttingDown then
    Exit;

  cw := ClientWidth;
  ch := ClientHeight;
  FTrendSurface.SetBounds(0, 0, cw, ch);

  cols := Max(TrendColumnCount, 1);
  stride := DotStride;
  // 0.6 of the slot width matches the visible disc area of the legacy ● glyph
  // at 85% slot width. A history dot the narrow-window clamp kept also owns
  // the columns of the dots it stands in for (the stride factor), so it grows
  // into that room rather than sitting at the floor with wide gaps. The floor
  // has to stay below 0.6 * MIN_DOT_COLUMN_PX or dots overlap at the very
  // width the clamp considers acceptable.
  d := Max(MIN_DOT_DIAMETER_PX, Round(Max(1, cw div cols) * stride * 0.6));
  FTrendDotDiameter := d;
  // Prediction dots keep single-column sizing: their count is fixed and they
  // stay one column apart.
  pd := Max(MIN_DOT_DIAMETER_PX, Round(Max(1, cw div cols) * 0.6));

  // Guard against a model/layout size mismatch mid-transition.
  if Length(FTrendLayout) <> Length(TrendSlots) then
    SetLength(FTrendLayout, Length(TrendSlots));
  if Length(FTrendVisible) <> Length(TrendSlots) then
    SetLength(FTrendVisible, Length(TrendSlots));

  bmp := nil;
  try
    for ix := 1 to High(TrendSlots) do
    begin
      FTrendVisible[ix] := (TrendSlots[ix].kind <> dkEmpty)
        and DotSlotVisible(ix);
      y := ValueTop(TrendSlots[ix].value) - (d div 2)
        + Round(ch * DOT_ADJUST);

      if FDotsExpanded and (TrendSlots[ix].kind = dkReading) then
      begin
        // Expanded value text: sized to the reading at a font derived from
        // lVal so the dot text tracks the main BG display, anchored at the
        // circle's top-left. Measured via a temp TBitmap — the surface's
        // canvas belongs to the paint cycle.
        if bmp = nil then
        begin
          bmp := TBitmap.Create;
          bmp.SetSize(1, 1);
          if FontTXTInList(fontn) then
            bmp.Canvas.Font.Name := fontn;
          bmp.Canvas.Font.Size := Max(8, lVal.Font.Size div Max(ACTIVE_DOTS, 1));
        end;
        FTrendLayout[ix] := Classes.Rect(SlotLeft(ix, d), y,
          SlotLeft(ix, d) + bmp.Canvas.TextWidth(TrendSlots[ix].text) + 4,
          y + bmp.Canvas.TextHeight(TrendSlots[ix].text) + 2);
      end
      else
        FTrendLayout[ix] := Classes.Rect(SlotLeft(ix, d), y,
          SlotLeft(ix, d) + d, y + d);
    end;
  finally
    bmp.Free;
  end;

  if Length(FPredictLayout) <> Length(PredictionSlots) then
    SetLength(FPredictLayout, Length(PredictionSlots));
  if Length(FPredictVisible) <> Length(PredictionSlots) then
    SetLength(FPredictVisible, Length(PredictionSlots));
  for ix := 0 to High(PredictionSlots) do
  begin
    FPredictVisible[ix] := PredictionDotsActive
      and (PredictionSlots[ix].kind = dkPredict);
    y := ValueTop(PredictionSlots[ix].value) - (pd div 2)
      + Round(ch * DOT_ADJUST);
    FPredictLayout[ix] := Classes.Rect(SlotLeft(ACTIVE_DOTS + ix + 1, pd), y,
      SlotLeft(ACTIVE_DOTS + ix + 1, pd) + pd, y + pd);
  end;

  // Keep-in-view: when the trace sticks out of the window (extreme values,
  // DOT_ADJUST), shift the whole row — history probes the overflow, the
  // prediction dots ride along so they stay on the same value axis. Two
  // rounds, like the control-based layout always ran: the first shift can
  // push the other edge out.
  SetLength({%H-}infos, Max(0, Length(TrendSlots) - 1));
  for pass := 1 to 2 do
  begin
    for ix := 1 to High(TrendSlots) do
    begin
      infos[ix - 1].Top := FTrendLayout[ix].Top;
      infos[ix - 1].Height := FTrendLayout[ix].Height;
      infos[ix - 1].Visible := FTrendVisible[ix];
    end;
    da := DotsInViewFromInfos(infos, ch);
    if da = 0 then
      break;
    for ix := 1 to High(TrendSlots) do
      Types.OffsetRect(FTrendLayout[ix], 0, -da);
    for ix := 0 to High(PredictionSlots) do
      Types.OffsetRect(FPredictLayout[ix], 0, -da);
  end;

  // Reading slots only: a gap slot's value is synthetic and has no text, so
  // the reference label stays blank rather than quoting an interpolation.
  if (Length(TrendSlots) > 1) and FTrendVisible[1]
    and (TrendSlots[1].kind = dkReading) then
  begin
    lRef.Top     := FTrendLayout[1].Top;
    lRef.Caption := TrendSlots[1].text;
  end
  else
    lRef.Caption := '';

  // Keep the trace above the labels among the non-windowed controls (the
  // clock/arrow swaps raise lVal now and then); windowed surfaces like the
  // warning card and the buttons stay on top regardless.
  FTrendSurface.BringToFront;
  FTrendSurface.Invalidate;

  if pnWarning.Visible then
    pnWarning.Refresh;
end;

// Total horizontal trend slots: the history dots plus prediction slots when
// the dot view is active (so history compresses to free room on the right).
function TfBG.TrendColumnCount: integer;
begin
  Result := ACTIVE_DOTS;
  if PredictionDotsActive then
    Inc(Result, PREDICTION_DOT_COUNT);
end;

// Slot-skip factor for the current window width — see TrendDotStride. Plain
// integer math on the live ClientWidth, so callers just recompute it instead
// of caching a value that every resize would invalidate.
function TfBG.DotStride: integer;
begin
  Result := TrendDotStride(fBG.ClientWidth, Max(TrendColumnCount, 1));
end;

// True when the trend slot at ix (1-based; ACTIVE_DOTS is the newest) is
// rendered at the current window width. Slots this hides keep their reading
// and their Hint — only Visible is withheld — so widening the window brings
// them straight back on the next resize pass.
function TfBG.DotSlotVisible(ix: integer): boolean;
begin
  Result := TrendDotVisibleInStride(ix, ACTIVE_DOTS, DotStride);
end;

// True when predictions should be drawn as hollow dots on the trend rather
// than written into the lPredict label. Requires the slots to exist.
function TfBG.PredictionDotsActive: boolean;
begin
  Result := PredictGlucoseReading and PredictDotMode
    and (Length(PredictionSlots) = PREDICTION_DOT_COUNT);
end;

// Take the forecast marks off the trend (stale data, predictions disabled).
// Clears the slots — RenderPredictionDots refills them when a fresh forecast
// arrives — and relayouts so the marks disappear immediately.
procedure TfBG.HidePredictionDots;
var
  i: integer;
begin
  for i := 0 to High(PredictionSlots) do
  begin
    PredictionSlots[i].kind := dkEmpty;
    PredictionSlots[i].text := '';
    PredictionSlots[i].lowPredict := false;
  end;
  UpdateTrendDots;
end;
