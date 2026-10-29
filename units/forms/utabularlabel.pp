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
 * - 2026-09-25: Created. Struck-out captions use the stock rendering.
 * - 2026-09-27: Disabled labels and captions with an accelerator '&' use the
 *   stock rendering.
 * - 2026-10-29: RollOnChange: a number giving way to another rolls over like
 *   an odometer wheel instead of blinking.
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

  With RollOnChange set, a number giving way to another rolls over the way
  the minute badge does (see ustatbadge): the digits that differ slide up out
  of the line while the new ones rise in from below, and the digits shared at
  either end stand still, so 5.4 -> 5.6 turns one wheel and 9.9 -> 10.0 turns
  them all. Only a number changing into a number rolls; "Setup", "--", the
  boot spinner, HIGH/LOW and a privacy glyph appear in one step, as does the
  first value on a label that showed nothing. Works with or without tabular
  digits, but not on a struck-out caption, where the stock rendering stands.

  Paint only ever touches the canvas inside Paint, which keeps it safe on the
  Cocoa widgetset (a label's canvas outside a paint cycle aborts there).
*}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, ExtCtrls, Math;

type
  {** A label whose digits can share one cell width and whose numbers can roll
      over on change (see the unit comment). }
  TLabel = class(StdCtrls.TLabel)
  private
    FTabularDigits: boolean;
    FRollOnChange: boolean;
    FRollFrom: string;      // Caption being rolled away; '' when no roll is running
    FRollStart: QWord;      // GetTickCount64 when the roll began
    FRollTimer: TTimer;     // Frame tick of the roll; nil until the first roll, off between rolls
    procedure SetTabularDigits(AValue: boolean);
    procedure RollTick({%H-}Sender: TObject);
    // Width of ACh in the canvas's font: the digit cell when tabular, the
    // glyph's own advance otherwise.
    function GlyphAdvance(ACanvas: TCanvas; const ACh: string; ACell: integer): integer;
    // Width of AText laid out as Paint lays it out, tabular or not.
    function RunWidth(ACanvas: TCanvas; const AText: string; ACell: integer): integer;
    // Draws AText glyph by glyph from (AX, AY), digits centred in their cells
    // when tabular; returns the x past the run.
    function DrawRun(ACanvas: TCanvas; AX, AY: integer; const AText: string;
      ACell: integer): integer;
  protected
    procedure Paint; override;
    procedure RealSetText(const AValue: TCaption); override;
  public
    destructor Destroy; override;
    {** Width of AText in ACanvas's current font with every digit widened to
        the widest digit's advance: what Paint will lay out, so a fitter
        (ScaleLbl) can size the font against the tabular width instead of
        the proportional one. }
    class function TabularTextWidth(ACanvas: TCanvas; const AText: string): integer;
    {** Typeset digits in equal-width cells. Off, the label paints exactly as
        a stock TLabel does. Ignored for word-wrapped or rotated text, and
        whenever the cells would not fit the label's width. }
    property TabularDigits: boolean read FTabularDigits write SetTabularDigits;
    {** Roll a number over into the next one instead of repainting it in one
        step (see the unit comment). Off, a caption change paints at once. }
    property RollOnChange: boolean read FRollOnChange write FRollOnChange;
  end;

implementation

const
  ROLL_MS = 320;            // Length of the roll, the minute badge's pace
  ROLL_TICK_MS = 16;        // Its frame tick

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

// Splits an old and a new value into the bytes they share at either end and
// the differing middle, so only "4" rolls in "5.4" -> "5.6" and everything in
// "9.9" -> "10.0". The cuts are backed off to UTF-8 code point starts so a
// glyph that differs in its tail bytes rolls whole. Mirrors ustatbadge.
procedure SplitRoll(const AOld, ANew: string; out APrefix, AOldMid, ANewMid,
  ASuffix: string);
var
  p, sfx, oldLen, newLen: integer;
begin
  oldLen := Length(AOld);
  newLen := Length(ANew);
  p := 0;
  while (p < oldLen) and (p < newLen) and (AOld[p + 1] = ANew[p + 1]) do
    Inc(p);
  while (p > 0) and (p < oldLen) and (Ord(AOld[p + 1]) and $C0 = $80) do
    Dec(p);
  sfx := 0;
  while (sfx < oldLen - p) and (sfx < newLen - p) and
    (AOld[oldLen - sfx] = ANew[newLen - sfx]) do
    Inc(sfx);
  while (sfx > 0) and (Ord(AOld[oldLen - sfx + 1]) and $C0 = $80) do
    Dec(sfx);
  APrefix := Copy(AOld, 1, p);
  AOldMid := Copy(AOld, p + 1, oldLen - p - sfx);
  ANewMid := Copy(ANew, p + 1, newLen - p - sfx);
  ASuffix := Copy(AOld, oldLen - sfx + 1, sfx);
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

destructor TLabel.Destroy;
begin
  if Assigned(FRollTimer) then
    FRollTimer.Enabled := false;
  inherited Destroy;
end;

procedure TLabel.RealSetText(const AValue: TCaption);
var
  old: string;
begin
  old := Caption;
  // One showing number giving way to another rolls over; anything else --
  // the first value, a word, a clear, a hidden label -- just appears. A roll
  // already under way restarts from the value it was heading for.
  if FRollOnChange and (old <> '') and (AValue <> '') and (old <> AValue)
    and HasDigit(old) and HasDigit(AValue) and Visible and Assigned(Parent)
    and not (csDestroying in ComponentState) then
  begin
    if FRollTimer = nil then
    begin
      FRollTimer := TTimer.Create(Self);
      FRollTimer.Interval := ROLL_TICK_MS;
      FRollTimer.Enabled := false;
      FRollTimer.OnTimer := @RollTick;
    end;
    FRollFrom := old;
    FRollStart := GetTickCount64;
    FRollTimer.Enabled := true;
  end;
  inherited RealSetText(AValue);
end;

procedure TLabel.RollTick(Sender: TObject);
begin
  if GetTickCount64 - FRollStart >= ROLL_MS then
  begin
    FRollFrom := '';
    FRollTimer.Enabled := false;
  end;
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

function TLabel.GlyphAdvance(ACanvas: TCanvas; const ACh: string; ACell: integer): integer;
begin
  if FTabularDigits and IsAsciiDigit(ACh) then
    Result := ACell
  else
    Result := ACanvas.TextWidth(ACh);
end;

function TLabel.RunWidth(ACanvas: TCanvas; const AText: string; ACell: integer): integer;
var
  i, n: integer;
begin
  Result := 0;
  i := 1;
  while i <= Length(AText) do
  begin
    n := Utf8Len(Ord(AText[i]));
    Inc(Result, GlyphAdvance(ACanvas, Copy(AText, i, n), ACell));
    Inc(i, n);
  end;
end;

function TLabel.DrawRun(ACanvas: TCanvas; AX, AY: integer; const AText: string;
  ACell: integer): integer;
var
  i, n, w: integer;
  ch: string;
begin
  i := 1;
  while i <= Length(AText) do
  begin
    n := Utf8Len(Ord(AText[i]));
    ch := Copy(AText, i, n);
    w := ACanvas.TextWidth(ch);
    if FTabularDigits and IsAsciiDigit(ch) then
    begin
      // Centred in its cell, so the figure sits where a tabular font would
      // put it rather than flush left with air on the right.
      ACanvas.TextOut(AX + (ACell - w) div 2, AY, ch);
      Inc(AX, ACell);
    end
    else
    begin
      ACanvas.TextOut(AX, AY, ch);
      Inc(AX, w);
    end;
    Inc(i, n);
  end;
  Result := AX;
end;

procedure TLabel.Paint;
var
  txt, prefix, oldMid, newMid, suffix: string;
  cell, total, totalFrom, th, x, y, dy, oldMidW, newMidW: integer;
  rolling: boolean;
  t: double;
begin
  txt := Caption;
  rolling := FRollFrom <> '';
  // Only the plain single-line case is typeset here; everything else -- and a
  // caption with nothing to align or roll -- is the stock label's job.
  // Struck-out text too: the OS draws the strike per TextOut call, so
  // glyph-by-glyph cells would leave the line broken at every cell edge, and
  // the strike only marks a stale reading, where a stable layout matters
  // least. Disabled text (the label's own flag or a disabled parent, as the
  // stock paint checks it) and accelerator markers are also the stock
  // label's: TextOut draws neither the greyed look nor the underline, and
  // would print the '&' itself.
  if ((not FTabularDigits) and (not rolling)) or WordWrap or
    (Font.Orientation <> 0) or (fsStrikeOut in Font.Style) or
    (not HasDigit(txt)) or (not IsEnabled) or
    (ShowAccelChar and (Pos('&', txt) > 0)) then
  begin
    inherited Paint;
    Exit;
  end;

  Canvas.Font.Assign(Font);
  cell := DigitCellWidth(Canvas);
  total := RunWidth(Canvas, txt, cell);
  th := Canvas.TextHeight(txt);
  // The cells are wider than the proportional run whenever the number holds
  // a narrow digit; if the fitter did not leave room for that, the stock
  // rendering at least keeps the whole number on the label.
  if (total <= 0) or (total > Width) then
  begin
    inherited Paint;
    Exit;
  end;

  t := 0;
  if rolling then
  begin
    // Ease-out, so the new value lands rather than stops. The run's width
    // glides from the old value's to the new one's ("9.9" -> "10.0"), so a
    // centred number slides over rather than jumping.
    t := Min(1.0, (GetTickCount64 - FRollStart) / ROLL_MS);
    t := 1 - (1 - t) * (1 - t) * (1 - t);
    totalFrom := RunWidth(Canvas, FRollFrom, cell);
    if totalFrom > Width then
      totalFrom := total;
    total := Round(totalFrom + (total - totalFrom) * t);
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

  if not rolling then
  begin
    DrawRun(Canvas, x, y, txt, cell);
    Exit;
  end;

  // Mid-roll: the shared head and tail stand still while the differing
  // middle turns over, old sliding up out of the line as new rises into it
  // from below. Clipped to the text's own line so nothing shows above or
  // below it; the parent's SaveDC/RestoreDC pair around Paint scopes the
  // change, and the box clip is put back for anything that follows.
  SplitRoll(FRollFrom, txt, prefix, oldMid, newMid, suffix);
  oldMidW := RunWidth(Canvas, oldMid, cell);
  newMidW := RunWidth(Canvas, newMid, cell);
  dy := Round(t * th);
  Canvas.ClipRect := Rect(0, y, Width, y + th);
  Canvas.Clipping := true;
  x := DrawRun(Canvas, x, y, prefix, cell);
  DrawRun(Canvas, x, y - dy, oldMid, cell);
  DrawRun(Canvas, x, y + th - dy, newMid, cell);
  if suffix <> '' then
    DrawRun(Canvas, x + Round(oldMidW + (newMidW - oldMidW) * t), y, suffix, cell);
  Canvas.ClipRect := Rect(0, 0, Width, Height);
end;

end.
