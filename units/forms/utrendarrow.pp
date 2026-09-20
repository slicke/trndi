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
 * - 2026-09-13: Exposed OnMouseDown, OnMouseMove and OnMouseUp so a host
 *   form can treat the arrow like its labels.
 * - 2026-09-20: The shaft and head are rasterized antialiased through
 *   trndi.raster instead of the aliased Canvas.Line.
 *)

unit utrendarrow;

{*
  A lightweight, cross-platform trend arrow that can be drawn at an arbitrary
  rotation angle. Used to render a continuously rotating trend arrow whose
  angle reflects the actual rate of blood-glucose change, instead of the
  discrete 8-direction arrow glyph.

  The control is a TGraphicControl, so it owns no window handle and paints
  itself directly onto the parent's canvas inside Paint. All drawing happens
  in Paint and the control is never resized from within Paint, which keeps it
  safe on the Cocoa widgetset (assigning bounds or touching a label canvas
  outside of a paint cycle crashes there).

  Angle convention: degrees, 0 = pointing right (steady / flat), positive =
  upward (rising), negative = downward (falling). See CalculateTrendAngle in
  trndi.funcs for the delta -> angle mapping.
*}

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, Math, trndi.raster;

type
  {** A vector arrow drawn at a settable rotation angle. }
  TTrendArrow = class(TGraphicControl)
  private
    FAngle: single;          //< Rotation in degrees (0 = right, + = up, - = down)
    FArrowColor: TColor;     //< Stroke colour
    FThickness: single;      //< Stroke width as a fraction of the short side
    procedure SetAngle(AValue: single);
    procedure SetArrowColor(AValue: TColor);
    procedure SetThickness(AValue: single);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    {** Rotation in degrees. 0 = flat/steady, positive = rising, negative = falling. }
    property Angle: single read FAngle write SetAngle;
    {** Colour of the arrow strokes. }
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    {** Stroke width as a fraction of the shorter side (0..1). }
    property Thickness: single read FThickness write SetThickness;
    {** Mouse events, exposed so a host form can treat the arrow like its labels. }
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

constructor TTrendArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAngle := 0;
  FArrowColor := clGray;
  FThickness := 0.11;
  // Do not paint an opaque background; let the parent show through so the
  // arrow behaves like the old semi-transparent background glyph.
  ControlStyle := ControlStyle - [csOpaque];
end;

procedure TTrendArrow.SetAngle(AValue: single);
begin
  // Defensive bound at straight up/down. The angle calculations stay well
  // inside it: CalculateTrendAngle clamps to +/-ARROW_MAX_ANGLE (70) and
  // TrendToAngle tops out at +/-60.
  if AValue > 90 then
    AValue := 90
  else if AValue < -90 then
    AValue := -90;
  if SameValue(FAngle, AValue, 0.05) then
    Exit;
  FAngle := AValue;
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

procedure TTrendArrow.SetArrowColor(AValue: TColor);
begin
  if FArrowColor = AValue then
    Exit;
  FArrowColor := AValue;
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

procedure TTrendArrow.SetThickness(AValue: single);
begin
  if SameValue(FThickness, AValue, 0.001) then
    Exit;
  FThickness := AValue;
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

procedure TTrendArrow.Paint;
var
  cx, cy, halfLen, headLen, headSpread, rad, dx, dy, bx, by: single;
  penW: integer;

  { Rotate vector (vx,vy) by deg degrees (screen space, y grows downward). }
  procedure Rot(vx, vy, deg: single; out ox, oy: single);
  var
    r, c, s: single;
  begin
    r := DegToRad(deg);
    c := cos(r);
    s := sin(r);
    ox := vx * c - vy * s;
    oy := vx * s + vy * c;
  end;

var
  shortSide: integer;
  tailX, tailY, headX, headY: single;
  strokes: array[0..2] of TSmoothStroke;
begin
  shortSide := Min(Width, Height);
  if shortSide <= 4 then
    Exit;

  cx := Width / 2;
  cy := Height / 2;
  halfLen := shortSide * 0.36;      // half the shaft length
  headLen := shortSide * 0.24;      // length of each arrow-head wing
  headSpread := 32;                 // half-angle of the head, in degrees

  // Direction unit vector. 0 deg = right; positive angle points up, so the
  // screen-space y component is negated.
  rad := DegToRad(FAngle);
  dx := cos(rad);
  dy := -sin(rad);

  penW := Max(2, Round(shortSide * FThickness));

  tailX := cx - dx * halfLen;
  tailY := cy - dy * halfLen;
  headX := cx + dx * halfLen;
  headY := cy + dy * halfLen;

  // Three capsule strokes rasterized together: the shaft, and the two head
  // wings fanned out by +/-headSpread from the reversed direction. The
  // capsules give the round caps and the round joint at the tip that the
  // old pecRound/pjsRound pen asked for, and the analytical coverage gives
  // the smooth edges the LCL pen never did on GDI or Qt. Sub-pixel
  // endpoints are kept, so the arrow rotates without snapping.
  strokes[0].X1 := tailX;
  strokes[0].Y1 := tailY;
  strokes[0].X2 := headX;
  strokes[0].Y2 := headY;

  Rot(-dx, -dy, headSpread, bx, by);
  strokes[1].X1 := headX;
  strokes[1].Y1 := headY;
  strokes[1].X2 := headX + bx * headLen;
  strokes[1].Y2 := headY + by * headLen;

  Rot(-dx, -dy, -headSpread, bx, by);
  strokes[2].X1 := headX;
  strokes[2].Y1 := headY;
  strokes[2].X2 := headX + bx * headLen;
  strokes[2].Y2 := headY + by * headLen;

  strokes[0].Color := FArrowColor;
  strokes[1].Color := FArrowColor;
  strokes[2].Color := FArrowColor;

  DrawSmoothStrokes(Canvas, strokes, penW);
end;

end.
