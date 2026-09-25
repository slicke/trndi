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
 * - 2026-09-25: Created.
 *)

unit utabularlabel;

{*
  A TLabel that can typeset its digits in equal-width cells -- "tabular
  figures" -- so a reading that ticks from 9.9 to 10.0, or from 11.6 to 12.3,
  keeps every digit in place instead of the whole number reflowing as narrow
  ones give way to wide zeros. Most system faces (San Francisco, Segoe UI)
  ship proportional figures by default and the LCL exposes no OpenType feature
  switch, so the cells are laid out here: every digit is measured, the widest
  advance becomes the cell, and each digit is centred in its cell while every
  other character keeps its natural width.

  The class is an interposer named TLabel: a form unit listing this unit after
  StdCtrls gets this class for every TLabel it declares, and the streaming
  reader resolves the .lfm's "TLabel" through the form's field table, so the
  designer keeps working with the stock control while the running form gets
  this one. Nothing changes until TabularDigits is set.

  Paint only ever touches the canvas inside Paint, which keeps it safe on the
  Cocoa widgetset (a label's canvas outside a paint cycle aborts there).
*}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, Math;

type
  {** A label whose digits can share one cell width (see the unit comment). }
  TLabel = class(StdCtrls.TLabel)
  private
    FTabularDigits: boolean;
    procedure SetTabularDigits(AValue: boolean);
  protected
    procedure Paint; override;
  public
    {** Width of AText in ACanvas's current font with every digit widened to
        the widest digit's advance: what Paint will lay out, so a fitter
        (ScaleLbl) can size the font against the tabular width instead of
        the proportional one. }
    class function TabularTextWidth(ACanvas: TCanvas; const AText: string): integer;
    {** Typeset digits in equal-width cells. Off, the label paints exactly as
        a stock TLabel does. Ignored for word-wrapped or rotated text, and
        whenever the cells would not fit the label's width. }
    property TabularDigits: boolean read FTabularDigits write SetTabularDigits;
  end;

implementation

// Length in bytes of the UTF-8 sequence starting with lead byte B.
function Utf8Len(B: byte): integer;
begin
  if B < $80 then
    Result := 1
  else if (B and $E0) = $C0 then
    Result := 2
  else if (B and $F0) = $E0 then
    Result := 3
  else if (B and $F8) = $F0 then
    Result := 4
  else
    Result := 1;   // stray continuation byte: pass it through alone
end;

function IsAsciiDigit(const S: string): boolean;
begin
  Result := (Length(S) = 1) and (S[1] in ['0'..'9']);
end;

function HasDigit(const S: string): boolean;
var
  i: integer;
begin
  for i := 1 to Length(S) do
    if S[i] in ['0'..'9'] then
      Exit(true);
  Result := false;
end;

// The widest digit advance in the canvas's font: the cell every digit gets.
function DigitCellWidth(ACanvas: TCanvas): integer;
var
  c: char;
begin
  Result := 0;
  for c := '0' to '9' do
    Result := Max(Result, ACanvas.TextWidth(c));
end;

class function TLabel.TabularTextWidth(ACanvas: TCanvas; const AText: string): integer;
var
  i, n, cell: integer;
  ch: string;
begin
  Result := 0;
  cell := DigitCellWidth(ACanvas);
  i := 1;
  while i <= Length(AText) do
  begin
    n := Utf8Len(Ord(AText[i]));
    ch := Copy(AText, i, n);
    if IsAsciiDigit(ch) then
      Inc(Result, cell)
    else
      Inc(Result, ACanvas.TextWidth(ch));
    Inc(i, n);
  end;
end;

procedure TLabel.SetTabularDigits(AValue: boolean);
begin
  if FTabularDigits = AValue then
    Exit;
  FTabularDigits := AValue;
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

procedure TLabel.Paint;
var
  txt, ch: string;
  i, n, cell, w, total, th, x, y: integer;
begin
  txt := Caption;
  // Only the plain single-line case is typeset here; everything else -- and a
  // caption with nothing to align -- is the stock label's job.
  if (not FTabularDigits) or WordWrap or (Font.Orientation <> 0) or
    (not HasDigit(txt)) then
  begin
    inherited Paint;
    Exit;
  end;

  Canvas.Font.Assign(Font);
  cell := DigitCellWidth(Canvas);
  total := TabularTextWidth(Canvas, txt);
  th := Canvas.TextHeight(txt);
  // The cells are wider than the proportional run whenever the number holds
  // a narrow digit; if the fitter did not leave room for that, the stock
  // rendering at least keeps the whole number on the label.
  if (total <= 0) or (total > Width) then
  begin
    inherited Paint;
    Exit;
  end;

  if not Transparent then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(0, 0, Width, Height);
  end;
  Canvas.Brush.Style := bsClear;

  case Alignment of
  taLeftJustify:
    x := 0;
  taRightJustify:
    x := Width - total;
  else
    x := (Width - total) div 2;
  end;
  case Layout of
  tlTop:
    y := 0;
  tlBottom:
    y := Height - th;
  else
    y := (Height - th) div 2;
  end;

  i := 1;
  while i <= Length(txt) do
  begin
    n := Utf8Len(Ord(txt[i]));
    ch := Copy(txt, i, n);
    w := Canvas.TextWidth(ch);
    if IsAsciiDigit(ch) then
    begin
      // Centred in its cell, so the figure sits where a tabular font would
      // put it rather than flush left with air on the right.
      Canvas.TextOut(x + (cell - w) div 2, y, ch);
      Inc(x, cell);
    end
    else
    begin
      Canvas.TextOut(x, y, ch);
      Inc(x, w);
    end;
    Inc(i, n);
  end;
end;

end.
