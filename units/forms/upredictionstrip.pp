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

unit upredictionstrip;

{*
  The text rendering of the glucose forecast: a small strip in the lower-right
  corner of the main window showing the predicted readings as a row of cells,
  one per horizon. Each cell carries a muted header with the minutes until the
  prediction ("+5 min") over a larger value line ("↗ 6.2") coloured by the
  range the predicted reading lands in, with thin dividers between the cells.
  It replaced a single TLabel that packed the same data into one line of
  "⏱5' ↗ 6.2 | ⏱10' ↗ 6.5 | ⏱15' → 6.6".

  The control is a TGraphicControl like TTrendArrow: it owns no window handle,
  paints straight onto the parent inside Paint, and never touches a canvas
  outside a paint cycle (which crashes on Cocoa). It sizes its own fonts to
  fit whatever bounds the form's layout hands it — a binary search over the
  value font height, with the header at a fixed ratio of that — so the form
  never has to measure text on its behalf.

  The strip knows nothing about glucose: the form fills in TPredictionCell
  records (already-formatted text and colours) or a single Message string
  for the "no change predicted" / "unavailable" states.
*}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math;

type
  {** One horizon of the forecast, as text ready to draw. }
  TPredictionCell = record
    Valid: boolean;   //< False draws a dash in place of the value line
    Header: string;   //< Small caption above the value; '' hides the header row
    Arrow: string;    //< Trend glyph(s) leading the value line; may be ''
    Value: string;    //< Formatted predicted reading; '' for arrow-only cells
    Color: TColor;    //< Colour of the value line (the range the reading lands in)
  end;
  TPredictionCells = array of TPredictionCell;

  {** Draws forecast cells (or a single message) fitted to its own bounds. }
  TPredictionStrip = class(TGraphicControl)
  private
    FCells: TPredictionCells;
    FMessage: string;
    FTextColor: TColor;
    FMutedColor: TColor;
    FDividerColor: TColor;
    FAlignment: TAlignment;
    FBold: boolean;
    function ContentKey: string;
    procedure SetMessage(const AValue: string);
    procedure SetTextColor(AValue: TColor);
    procedure SetMutedColor(AValue: TColor);
    procedure SetDividerColor(AValue: TColor);
    procedure SetAlignment(AValue: TAlignment);
    procedure SetBold(AValue: boolean);
    procedure PaintMessage;
    procedure PaintCells;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    {** Show these cells; clears any Message. Repaints only when the content
        actually changed, so the periodic countdown refresh is free on the
        ticks where no minute has passed. }
    procedure SetCells(const ACells: TPredictionCells);
    {** Drop every cell and the message. }
    procedure Clear;
    {** The cells currently shown (empty while a Message is up). }
    property Cells: TPredictionCells read FCells;
    {** A single line drawn alone instead of cells, e.g. "Predictions
        unavailable". Setting it drops the cells. }
    property Message: string read FMessage write SetMessage;
    {** Colour of a Message and of any cell without a colour of its own. }
    property TextColor: TColor read FTextColor write SetTextColor;
    {** Colour of the headers and of the dash in an invalid cell. }
    property MutedColor: TColor read FMutedColor write SetMutedColor;
    {** Colour of the thin lines between cells. }
    property DividerColor: TColor read FDividerColor write SetDividerColor;
    {** Where the content sits when it is narrower than the strip. }
    property Alignment: TAlignment read FAlignment write SetAlignment;
    {** Bold value line — used for the largest single-arrow size. }
    property Bold: boolean read FBold write SetBold;
    property Font;
    property Visible;
    property OnClick;
  end;

implementation

const
  MIN_FONT_PX = 6;        // Smallest value-line font the fit will go down to
  HEADER_RATIO = 0.5;     // Header font height as a fraction of the value font
  EDGE_PAD = 2;           // Pixels kept clear inside the control's bounds
  CELL_PAD_RATIO = 0.35;  // Horizontal padding inside a cell, per value font px
  ROW_GAP_RATIO = 0.08;   // Space between header and value rows, per value font px
  INVALID_MARK = '–';     // Drawn for a horizon with no matching prediction

constructor TPredictionStrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextColor := clWhite;
  FMutedColor := clGray;
  FDividerColor := clGray;
  FAlignment := taRightJustify;
  FBold := false;
  FMessage := '';
  SetLength(FCells, 0);
  // Let the parent show through around the glyphs, like the label it replaced.
  ControlStyle := ControlStyle - [csOpaque];
end;

// A flat rendering of everything that affects the paint, so the setters can
// skip the Invalidate when a refresh hands over identical content.
function TPredictionStrip.ContentKey: string;
var
  i: integer;
begin
  Result := FMessage;
  for i := 0 to High(FCells) do
    with FCells[i] do
      Result := Result + '|' + BoolToStr(Valid) + #1 + Header + #1 + Arrow + #1 +
        Value + #1 + IntToStr(Color);
end;

procedure TPredictionStrip.SetCells(const ACells: TPredictionCells);
var
  before: string;
  i: integer;
begin
  before := ContentKey;
  FMessage := '';
  SetLength(FCells, Length(ACells));
  for i := 0 to High(ACells) do
    FCells[i] := ACells[i];
  if ContentKey <> before then
    Invalidate;
end;

procedure TPredictionStrip.Clear;
begin
  if (FMessage = '') and (Length(FCells) = 0) then
    Exit;
  FMessage := '';
  SetLength(FCells, 0);
  Invalidate;
end;

procedure TPredictionStrip.SetMessage(const AValue: string);
begin
  if (FMessage = AValue) and (Length(FCells) = 0) then
    Exit;
  SetLength(FCells, 0);
  FMessage := AValue;
  Invalidate;
end;

procedure TPredictionStrip.SetTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;
  FTextColor := AValue;
  Invalidate;
end;

procedure TPredictionStrip.SetMutedColor(AValue: TColor);
begin
  if FMutedColor = AValue then
    Exit;
  FMutedColor := AValue;
  Invalidate;
end;

procedure TPredictionStrip.SetDividerColor(AValue: TColor);
begin
  if FDividerColor = AValue then
    Exit;
  FDividerColor := AValue;
  Invalidate;
end;

procedure TPredictionStrip.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TPredictionStrip.SetBold(AValue: boolean);
begin
  if FBold = AValue then
    Exit;
  FBold := AValue;
  Invalidate;
end;

procedure TPredictionStrip.Paint;
begin
  inherited Paint;
  if (Width <= 0) or (Height <= 0) then
    Exit;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);
  Canvas.Font.Style := [];
  if FMessage <> '' then
    PaintMessage
  else
  if Length(FCells) > 0 then
    PaintCells;
end;

// -----------------------------------------------------------------------------
// The single-line states ("Predictions unavailable", "No change predicted →")
// -----------------------------------------------------------------------------
procedure TPredictionStrip.PaintMessage;
var
  lo, hi, mid, best, tw, th, x: integer;
  maxW, maxH: integer;
begin
  maxW := Width - 2 * EDGE_PAD;
  maxH := Height - 2 * EDGE_PAD;
  if (maxW <= 0) or (maxH <= 0) then
    Exit;

  // Largest font height whose rendering of the message fits the bounds.
  best := MIN_FONT_PX;
  lo := MIN_FONT_PX;
  hi := maxH;
  while lo <= hi do
  begin
    mid := (lo + hi) div 2;
    Canvas.Font.Height := -mid;
    if (Canvas.TextWidth(FMessage) <= maxW) and (Canvas.TextHeight(FMessage) <= maxH) then
    begin
      best := mid;
      lo := mid + 1;
    end
    else
      hi := mid - 1;
  end;

  Canvas.Font.Height := -best;
  Canvas.Font.Color := FTextColor;
  tw := Canvas.TextWidth(FMessage);
  th := Canvas.TextHeight(FMessage);
  case FAlignment of
  taLeftJustify: x := EDGE_PAD;
  taCenter: x := (Width - tw) div 2;
  else
    x := Width - EDGE_PAD - tw;
  end;
  // Bottom-anchored, like the cells: the strip hugs the window corner.
  Canvas.TextOut(x, Height - EDGE_PAD - th, FMessage);
end;

// -----------------------------------------------------------------------------
// The cell row. One measurement pass per candidate font height, run inside a
// binary search for the largest value font whose row still fits; then a draw
// pass at that size.
// -----------------------------------------------------------------------------
procedure TPredictionStrip.PaintCells;
type
  TCellMetrics = record
    Line: string;        // The value line as drawn (arrow + value, or the dash)
    CellW: integer;      // Widest of header/value plus padding
  end;
var
  n, i, lo, hi, mid, best: integer;
  maxW, maxH: integer;
  metrics: array of TCellMetrics;
  headerH, valueH, rowGap, cellPad, gap, totalW, totalH: integer;
  anyHeader: boolean;
  x, y, cx, headerY, valueY: integer;
  dividerTop, dividerBottom: integer;

  function ValueLine(const c: TPredictionCell): string;
  begin
    if not c.Valid then
      Exit(INVALID_MARK);
    if (c.Arrow <> '') and (c.Value <> '') then
      Result := c.Arrow + ' ' + c.Value
    else
      Result := c.Arrow + c.Value;
  end;

  // Fills metrics/headerH/valueH/rowGap/cellPad/gap/totalW/totalH for a value
  // font of `px` pixels and reports whether the row fits the bounds.
  function Measure(px: integer): boolean;
  var
    j, hw, vw: integer;
  begin
    cellPad := Max(2, Round(px * CELL_PAD_RATIO));
    gap := cellPad;
    rowGap := Round(px * ROW_GAP_RATIO);

    headerH := 0;
    if anyHeader then
    begin
      Canvas.Font.Style := [];
      Canvas.Font.Height := -Max(MIN_FONT_PX, Round(px * HEADER_RATIO));
      headerH := Canvas.TextHeight('0');
    end;

    if FBold then
      Canvas.Font.Style := [fsBold]
    else
      Canvas.Font.Style := [];
    Canvas.Font.Height := -px;
    valueH := Canvas.TextHeight('0');

    totalW := 0;
    for j := 0 to n - 1 do
    begin
      vw := Canvas.TextWidth(metrics[j].Line);
      metrics[j].CellW := vw + 2 * cellPad;
      Inc(totalW, metrics[j].CellW);
    end;
    if anyHeader then
    begin
      Canvas.Font.Style := [];
      Canvas.Font.Height := -Max(MIN_FONT_PX, Round(px * HEADER_RATIO));
      totalW := 0;
      for j := 0 to n - 1 do
      begin
        hw := Canvas.TextWidth(FCells[j].Header) + 2 * cellPad;
        if hw > metrics[j].CellW then
          metrics[j].CellW := hw;
        Inc(totalW, metrics[j].CellW);
      end;
    end;
    Inc(totalW, (n - 1) * gap);

    totalH := valueH;
    if anyHeader then
      Inc(totalH, headerH + rowGap);

    Result := (totalW <= maxW) and (totalH <= maxH);
  end;

begin
  n := Length(FCells);
  maxW := Width - 2 * EDGE_PAD;
  maxH := Height - 2 * EDGE_PAD;
  if (maxW <= 0) or (maxH <= 0) then
    Exit;

  SetLength(metrics, n);
  anyHeader := false;
  for i := 0 to n - 1 do
  begin
    metrics[i].Line := ValueLine(FCells[i]);
    if FCells[i].Header <> '' then
      anyHeader := true;
  end;

  best := MIN_FONT_PX;
  lo := MIN_FONT_PX;
  hi := maxH;
  while lo <= hi do
  begin
    mid := (lo + hi) div 2;
    if Measure(mid) then
    begin
      best := mid;
      lo := mid + 1;
    end
    else
      hi := mid - 1;
  end;
  Measure(best);

  case FAlignment of
  taLeftJustify: x := EDGE_PAD;
  taCenter: x := (Width - totalW) div 2;
  else
    x := Width - EDGE_PAD - totalW;
  end;
  y := Height - EDGE_PAD - totalH;
  headerY := y;
  valueY := y;
  if anyHeader then
    valueY := y + headerH + rowGap;

  // Dividers span the value row only, so they read as separators between the
  // numbers rather than as a frame around the whole strip.
  dividerTop := valueY + valueH div 5;
  dividerBottom := valueY + valueH - valueH div 5;

  cx := x;
  for i := 0 to n - 1 do
  begin
    if anyHeader and (FCells[i].Header <> '') then
    begin
      Canvas.Font.Style := [];
      Canvas.Font.Height := -Max(MIN_FONT_PX, Round(best * HEADER_RATIO));
      Canvas.Font.Color := FMutedColor;
      Canvas.TextOut(cx + (metrics[i].CellW - Canvas.TextWidth(FCells[i].Header)) div 2,
        headerY, FCells[i].Header);
    end;

    if FBold then
      Canvas.Font.Style := [fsBold]
    else
      Canvas.Font.Style := [];
    Canvas.Font.Height := -best;
    if FCells[i].Valid then
      Canvas.Font.Color := FCells[i].Color
    else
      Canvas.Font.Color := FMutedColor;
    Canvas.TextOut(cx + (metrics[i].CellW - Canvas.TextWidth(metrics[i].Line)) div 2,
      valueY, metrics[i].Line);

    if i < n - 1 then
    begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := FDividerColor;
      Canvas.Line(cx + metrics[i].CellW + gap div 2, dividerTop,
        cx + metrics[i].CellW + gap div 2, dividerBottom);
    end;

    Inc(cx, metrics[i].CellW + gap);
  end;
end;

end.
