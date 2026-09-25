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
(* MODIFICATION NOTICE (2026-09-20): A placement that advanced the slot anchor
   no longer snaps the row one column left: UpdateTrendDots keeps the boxes it
   is replacing and StartTrendSlide eases every reading from its old box to
   its new one over TREND_SLIDE_MS (tTrendSlide / ApplyTrendSlideFrame), with
   arriving readings entering from the right. Any other relayout while the
   slide runs just moves its finish line. *)
(* MODIFICATION NOTICE (2026-09-25): Every history dot is drawn on a knockout
   halo in the backdrop color (TrendBackdropColorAt, DOT_HALO_FRACTION) so a
   dot crossing the reading's digits or the arrow glyph keeps a clean edge.
   The settings previews draw the same halo. Older history dots (and the
   connecting line and gap rings with them) fade toward the backdrop by slot
   age (DotAgeKeep, ux.dot_fade) so the newest reading dominates. The
   connecting line runs through the dots as a monotone cubic curve
   (SmoothTrace) instead of straight segments. The Display-tab preview
   shows the delta as the pill under the reading, like the live window. *)

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
      // Types-qualified: MacOSAll's QuickDraw PtInRect (args swapped)
      // shadows the RTL one on Darwin, like Point/Rect elsewhere in here.
      and Types.PtInRect(FTrendLayout[i], P) then
    begin
      AIndex := i;
      Exit(@TrendSlots[i]);
    end;
  for i := 0 to Min(High(PredictionSlots), High(FPredictLayout)) do
    if (i <= High(FPredictVisible)) and FPredictVisible[i]
      and Types.PtInRect(FPredictLayout[i], P) then
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

function TfBG.TrendBackdropColorAt(AY: integer): TColor;
var
  i: integer;
begin
  // The gradient's tone at this row (quantised, see BACKDROP_GRADIENT_STEPS)
  // is the base the bands composite onto.
  Result := BackdropColorAt(AY, true);
  // Same order and the same "over" composite DrawRangeBands uses, so a band
  // listed inside an earlier one deepens it here too. The few edge-gradient
  // rows are ignored: a halo straddling a band edge is off by a shade nobody
  // can see.
  for i := Low(FPaintedBands) to High(FPaintedBands) do
    if (AY >= FPaintedBands[i].Top) and (AY <= FPaintedBands[i].Bottom) then
      Result := BlendColors(FPaintedBands[i].Color, Result,
        FPaintedBands[i].Alpha / 255.0);
end;

// The knockout halo under a history dot: a disc in the backdrop color a
// little wider than the dot, drawn first so the dot's own edge lands on a
// clean field whatever the digits, arrow or connecting line put there.
procedure DrawDotHalo(ACanvas: TCanvas; ADiameter, ALeft, ATop: integer;
  ABackdrop: TColor);
var
  halo: integer;
begin
  halo := Max(DOT_HALO_MIN_PX, Round(ADiameter * DOT_HALO_FRACTION));
  DrawSmoothCircle(ACanvas, ADiameter + 2 * halo, ABackdrop, clNone, 0,
    ALeft - halo, ATop - halo);
end;

// How much of a history marker's own color survives the age fade: 1.0 for the
// newest slot, falling linearly to 1 - DOT_AGE_FADE_MAX at slot 1. Shared by
// the live surface and the Display-tab preview. Always 1.0 with the fade off.
function DotAgeKeep(ASlotIx, ANewestIx: integer): double;
begin
  if (not TrendDotFade) or (ANewestIx <= 1) then
    Exit(1.0);
  Result := 1.0 - DOT_AGE_FADE_MAX * (ANewestIx - ASlotIx) / (ANewestIx - 1);
  Result := EnsureRange(Result, 1.0 - DOT_AGE_FADE_MAX, 1.0);
end;

// Bend the connecting line: expand the dot centres into a curve through them.
// Fritsch-Carlson monotone cubic Hermite in x: the trace is a value over time
// with x increasing left to right, and a monotone spline never overshoots a
// dot -- a Catmull-Rom would dip below a low or arc above a high between two
// readings, drawing glucose that was never measured. Every original point is
// kept, and each sub-point takes the colour of the nearer original so
// DrawSmoothPolyline's mid-segment colour break lands where it did before.
// A segment whose x does not advance (a mid-slide collision) stays straight.
procedure SmoothTrace(const APts: array of TPoint; const ACols: array of TColor;
  out AOutPts: TPointArray; out AOutCols: TColorArray);
const
  PX_PER_STEP = 6;   // one sub-point per this many pixels of x
  MAX_STEPS = 12;
var
  n, i, k, steps, count: integer;
  h, delta, m: array of double;
  t, t2, t3, h00, h10, h01, h11, y: double;
begin
  n := Length(APts);
  SetLength(AOutPts, 0);
  SetLength(AOutCols, 0);
  if n < 2 then
    Exit;

  SetLength(h, n - 1);
  SetLength(delta, n - 1);
  SetLength(m, n);
  for i := 0 to n - 2 do
  begin
    h[i] := APts[i + 1].X - APts[i].X;
    if h[i] > 0 then
      delta[i] := (APts[i + 1].Y - APts[i].Y) / h[i]
    else
      delta[i] := 0;
  end;
  // Endpoint tangents follow their one segment; interior tangents are the
  // weighted harmonic mean of the two slopes, zero at a local extremum so the
  // curve turns flat through a peak or trough instead of overshooting it.
  m[0] := delta[0];
  m[n - 1] := delta[n - 2];
  for i := 1 to n - 2 do
    if (delta[i - 1] * delta[i] <= 0) or (h[i - 1] <= 0) or (h[i] <= 0) then
      m[i] := 0
    else
      m[i] := 3 * (h[i - 1] + h[i]) /
        ((2 * h[i] + h[i - 1]) / delta[i - 1] + (h[i] + 2 * h[i - 1]) / delta[i]);

  // Upper bound first, trimmed to what was written.
  SetLength(AOutPts, (n - 1) * MAX_STEPS + 1);
  SetLength(AOutCols, Length(AOutPts));
  count := 0;
  for i := 0 to n - 2 do
  begin
    AOutPts[count] := APts[i];
    AOutCols[count] := ACols[i];
    Inc(count);
    if h[i] <= 0 then
      Continue;
    steps := EnsureRange(Round(h[i] / PX_PER_STEP), 2, MAX_STEPS);
    for k := 1 to steps - 1 do
    begin
      t := k / steps;
      t2 := t * t;
      t3 := t2 * t;
      h00 := 2 * t3 - 3 * t2 + 1;
      h10 := t3 - 2 * t2 + t;
      h01 := -2 * t3 + 3 * t2;
      h11 := t3 - t2;
      y := h00 * APts[i].Y + h10 * h[i] * m[i] + h01 * APts[i + 1].Y +
        h11 * h[i] * m[i + 1];
      AOutPts[count] := MkPoint(Round(APts[i].X + t * h[i]), Round(y));
      if t < 0.5 then
        AOutCols[count] := ACols[i]
      else
        AOutCols[count] := ACols[i + 1];
      Inc(count);
    end;
  end;
  AOutPts[count] := APts[n - 1];
  AOutCols[count] := ACols[n - 1];
  Inc(count);
  SetLength(AOutPts, count);
  SetLength(AOutCols, count);
end;

procedure TfBG.TrendSurfacePaint(Sender: TObject);
var
  cnv: TCanvas;
  fontn: string;
  slot: PTrendSlot;
  r: TRect;
  ix, d, ringWidth, xSize, xOff, xThick, fs, nPts: integer;
  dotColor, ringColor, backdrop: TColor;
  xAlpha, keep: double;
  linePts, curvePts: TPointArray;
  lineCols, curveCols: TColorArray;
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
        // The trace ages with the dots it connects, toward the tone under it.
        lineCols[nPts] := BlendColors(lineCols[nPts],
          TrendBackdropColorAt(linePts[nPts].Y), DotAgeKeep(ix, ACTIVE_DOTS));
        Inc(nPts);
      end;
    SetLength(linePts, nPts);
    SetLength(lineCols, nPts);
    if nPts >= 2 then
    begin
      SmoothTrace(linePts, lineCols, curvePts, curveCols);
      DrawSmoothPolyline(cnv, curvePts, curveCols,
        TrendLineStrokePx(FTrendDotDiameter, TrendLineWidthStep));
    end;
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

    // Age fade: every marker in this slot blends this far toward the backdrop
    // at its own height (the halo's color), so old dots recede on the band
    // exactly as they do on the plain window.
    backdrop := TrendBackdropColorAt(r.Top + d div 2);
    keep := DotAgeKeep(ix, ACTIVE_DOTS);

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
        BlendColors(BlendColors(GetTextColorForBackground(fBG.Color),
        fBG.Color, GAP_DOT_BLEND), backdrop, keep),
        Max(1, xSize div 8), r.Left + xOff, r.Top + xOff);
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

    // Fade after the ring pick: FreshDotRing and the outline rim derive from
    // the unfaded dot, and the newest slot keeps everything anyway.
    dotColor := BlendColors(dotColor, backdrop, keep);
    if ringWidth > 0 then
      ringColor := BlendColors(ringColor, backdrop, keep);

    // The halo takes the backdrop at the dot's centre row, so it is invisible
    // on the plain window and on the band alike, and only shows as a knockout
    // where a digit or the arrow runs under the dot.
    DrawDotHalo(cnv, d, r.Left, r.Top, backdrop);
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
      DrawDotHalo(ACanvas, d, x, y, winBgs[i]);
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
  valTop, valTh, chev, gap, padX, padY, pillH, cy, ccx: integer;
  linePts, curvePts: TPointArray;
  lineCols, curveCols: TColorArray;
  capsule: array[0..0] of TSmoothStroke;
  pts: array[0..2] of TPoint;

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
  agoText := FAgoBadge.Caption;
  if (agoText = '') or (agoText = 'lAgo') then
    agoText := '10 min';
  diffText := lDiff.Caption;
  if (diffText = '') or (diffText = 'lDiff') then
    diffText := '+0.0';
  diffText := SwapSep(diffText);
  tirText := FTirBadge.Caption;

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
    if TryStrToInt(FTirBadge.Hint, r) then
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
  valTh := ACanvas.TextHeight(valText);
  valTop := ARect.Top + (h - valTh) div 2;
  ACanvas.TextOut(ARect.Left + (w - tw) div 2, valTop, valText);

  // Delta: the pill under the reading's baseline, honoring the saved lDiff
  // alignment -- the same capsule, tint, padding and chevron PaintDeltaPill
  // draws live (the chevron from the delta's sign; the preview has no rate).
  SetPreviewFont('', Max(10, h div 11), []);
  tw := ACanvas.TextWidth(diffText);
  th := ACanvas.TextHeight(diffText);
  chev := Round(th * DELTA_CHEVRON_FRAC);
  gap := Max(3, th div 4);
  padX := Round(th * DELTA_PILL_PAD_X);
  padY := Round(th * DELTA_PILL_PAD_Y);
  pillH := th + 2 * padY;
  case lDiff.Alignment of
  taLeftJustify:
    x := ARect.Left + PAD + chev + gap;
  taRightJustify:
    x := ARect.Left + w - tw - PAD;
  else
    x := ARect.Left + (w - tw + chev + gap) div 2;
  end;
  cy := Min(valTop + Round(valTh * 0.8) + padY + pillH div 2,
    ARect.Bottom - pillH div 2 - 2);
  capsule[0].X1 := x - gap - chev - padX + pillH / 2;
  capsule[0].X2 := Max(capsule[0].X1, x + tw + padX - pillH / 2);
  capsule[0].Y1 := cy;
  capsule[0].Y2 := cy;
  capsule[0].Color := BlendColors(valColor, bg, DELTA_PILL_TINT);
  DrawSmoothStrokes(ACanvas, capsule, pillH);
  ccx := x - gap - chev div 2;
  if Pos('+', diffText) = 1 then
  begin
    pts[0] := MkPoint(ccx, cy - chev div 2);
    pts[1] := MkPoint(ccx - chev div 2, cy + chev div 2);
    pts[2] := MkPoint(ccx + chev div 2, cy + chev div 2);
    DrawSmoothConvexPolygon(ACanvas, pts, subColor);
  end
  else
  if Pos('-', diffText) = 1 then
  begin
    pts[0] := MkPoint(ccx - chev div 2, cy - chev div 2);
    pts[1] := MkPoint(ccx + chev div 2, cy - chev div 2);
    pts[2] := MkPoint(ccx, cy + chev div 2);
    DrawSmoothConvexPolygon(ACanvas, pts, subColor);
  end
  else
  begin
    capsule[0].X1 := ccx - chev / 2;
    capsule[0].X2 := ccx + chev / 2;
    capsule[0].Color := subColor;
    DrawSmoothStrokes(ACanvas, capsule, Max(2, chev div 4));
  end;
  ACanvas.Font.Color := subColor;
  ACanvas.TextOut(x, cy - th div 2, diffText);

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
      lineCols[i] := BlendColors(lineCols[i], bg,
        DotAgeKeep(i + 1, DOT_COUNT));
    end;
    SmoothTrace(linePts, lineCols, curvePts, curveCols);
    DrawSmoothPolyline(ACanvas, curvePts, curveCols,
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
        BlendColors(BlendColors(GetTextColorForBackground(bg), bg,
        GAP_DOT_BLEND), bg, DotAgeKeep(i + 1, DOT_COUNT)),
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
    // The live surface's age fade, slot 1 oldest.
    dotColor := BlendColors(dotColor, bg, DotAgeKeep(i + 1, DOT_COUNT));
    if ringWidth > 0 then
      ringColor := BlendColors(ringColor, bg, DotAgeKeep(i + 1, DOT_COUNT));
    DrawDotHalo(ACanvas, d, x, y, bg);
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
  cw, ch, cols, stride, ix, d, pd, y, da, pass, slideBy: integer;
  infos: array of TDotInfo;
  bmp: TBitmap;
  fontn: string;
  prevLayout: array of TRect;
  prevVisible: array of boolean;

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

  // The boxes on screen right now — mid-slide ones included, so a second
  // step that lands while the row is still moving continues from where the
  // dots are, not from where they were meant to be.
  prevLayout := Copy(FTrendLayout);
  prevVisible := Copy(FTrendVisible);
  slideBy := FTrendSlideBy;
  FTrendSlideBy := 0;

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

  // A placement that stepped the anchor on slides the row from the boxes
  // captured above. Every other relayout (resize, a settings change, the
  // deferred post-fetch pass that follows a placement within the slide's
  // own lifetime) keeps a running slide going toward the fresh layout
  // rather than cutting it short — the resting boxes just moved.
  if slideBy > 0 then
    StartTrendSlide(prevLayout, prevVisible, slideBy)
  else
  if Assigned(tTrendSlide) and tTrendSlide.Enabled then
  begin
    if Length(FTrendSlideTo) = Length(FTrendLayout) then
    begin
      FTrendSlideTo := Copy(FTrendLayout);
      ApplyTrendSlideFrame;
    end
    else
      tTrendSlide.Enabled := false;
  end;

  FTrendSurface.Invalidate;

  if pnWarning.Visible then
    pnWarning.Refresh;
end;

// Reading-arrival slide. A new reading (or the clock crossing a slot
// boundary) moves every reading one slot left, and until now the row simply
// snapped there between two frames. Instead, each history slot eases from
// the box its reading was drawn in before the placement — the slot AShiftBy
// columns to its right in the previous layout — to the box UpdateTrendDots
// just gave it, so the eye can follow the readings stepping on and sees the
// time pass. Readings with no previous box (the arrival itself, or one that
// was hidden by the narrow-window stride) start AShiftBy columns to the
// right of their new box, which is where their column was. Only the history
// row moves: the forecast marks stay in their fixed columns and simply take
// their new values. Interpolating FTrendLayout itself, rather than a paint-
// time offset, keeps the connecting line, the fresh ring, the hit test and
// the popup menu's dots-in-view probe all on the moving boxes for free.
procedure TfBG.StartTrendSlide(const APrev: array of TRect;
  const APrevVisible: array of boolean; AShiftBy: integer);
var
  ix, src, colW: integer;
begin
  // A whole window's worth of shift (an outage, a wake from sleep) has no
  // readings in common with the last frame, so there is nothing to follow.
  if (FTrendSurface = nil) or FShuttingDown or (WindowState = wsMinimized)
    or (AShiftBy >= ACTIVE_DOTS) or (Length(FTrendLayout) < 2) then
    Exit;

  colW := Round(ClientWidth / Max(TrendColumnCount, 1));
  FTrendSlideTo := Copy(FTrendLayout);
  SetLength(FTrendSlideFrom, Length(FTrendLayout));
  for ix := 1 to High(FTrendLayout) do
  begin
    src := ix + AShiftBy;
    if (src <= High(APrev)) and (src <= High(APrevVisible))
      and APrevVisible[src] and (APrev[src].Width > 0) then
      FTrendSlideFrom[ix] := APrev[src]
    else
    begin
      FTrendSlideFrom[ix] := FTrendSlideTo[ix];
      Types.OffsetRect(FTrendSlideFrom[ix], AShiftBy * colW, 0);
    end;
  end;

  if tTrendSlide = nil then
  begin
    tTrendSlide := TTimer.Create(Self);
    tTrendSlide.Enabled := false;
    tTrendSlide.Interval := TREND_SLIDE_TICK_MS;
    tTrendSlide.OnTimer := @tTrendSlideTimer;
  end;
  FTrendSlideStart := GetTickCount64;
  ApplyTrendSlideFrame;   // frame 0: the row where it was
  tTrendSlide.Enabled := true;
end;

// One frame of the slide: FTrendLayout takes the eased mix of the start and
// resting boxes. Ease-out cubic, like the progress bar's drain — the row
// leaves quickly and settles softly. Past the end the resting boxes are
// written exactly (no rounding residue) and the tick stands down.
procedure TfBG.ApplyTrendSlideFrame;
var
  ix: integer;
  t: double;

  function Mix(a, b: integer): integer;
  begin
    Result := a + Round((b - a) * t);
  end;

begin
  if (Length(FTrendSlideTo) <> Length(FTrendLayout))
    or (Length(FTrendSlideFrom) <> Length(FTrendLayout)) then
  begin
    if Assigned(tTrendSlide) then
      tTrendSlide.Enabled := false;
    Exit;
  end;

  t := (GetTickCount64 - FTrendSlideStart) / TREND_SLIDE_MS;
  if t >= 1.0 then
  begin
    t := 1.0;
    if Assigned(tTrendSlide) then
      tTrendSlide.Enabled := false;
  end
  else
    t := 1 - (1 - t) * (1 - t) * (1 - t);

  for ix := 1 to High(FTrendLayout) do
    FTrendLayout[ix] := Classes.Rect(
      Mix(FTrendSlideFrom[ix].Left, FTrendSlideTo[ix].Left),
      Mix(FTrendSlideFrom[ix].Top, FTrendSlideTo[ix].Top),
      Mix(FTrendSlideFrom[ix].Right, FTrendSlideTo[ix].Right),
      Mix(FTrendSlideFrom[ix].Bottom, FTrendSlideTo[ix].Bottom));
end;

procedure TfBG.tTrendSlideTimer(Sender: TObject);
begin
  if FShuttingDown or (FTrendSurface = nil) then
  begin
    tTrendSlide.Enabled := false;
    Exit;
  end;
  ApplyTrendSlideFrame;
  FTrendSurface.Invalidate;
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
// than written into the forecast strip. Requires the slots to exist.
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
