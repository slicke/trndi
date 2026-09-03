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

unit ustatbadge;

{*
  A stacked statistic badge: a value with a muted caption under it, sized to
  the value's width. Two live on the main window's top band — the reading
  age on the left ("3 min", or the clock time over "last reading") and
  time-in-range on the right ("85%" over "in range"). Each replaced a TLabel: "🕑 3 min", and a bare
  "85%" (or a mood emoji) optionally prefixed with the mean as "6.4 | 85%".

  Beyond the value and caption a badge can carry an optional mean as its
  own muted "avg 6.4" part beside the stack, and a thin bar under the text
  filled to a percentage — shown only while the pointer is over the badge,
  since always on it pulled the eye away from the reading. The value and bar
  take an accent colour the form picks (good/bad tint, contrast-lifted) or
  the plain text colour otherwise. Alignment says which edge the stack hugs.

  TGraphicControl, like the prediction strip: no window handle, all drawing
  and measuring inside Paint or against a canvas the form hands in, never a
  control canvas outside a paint cycle (Cocoa). The form asks PreferredSize
  with a scratch bitmap canvas so the badge occupies exactly its content and
  the click target stays the size of what is shown.

  Caption is kept in sync with the old labels' text ("6.4 | 85%", "3 min")
  because the extension hook and the settings preview read it.
*}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math;

type
  {** Value over caption, optional mean beside it, optional hover bar. }
  TStatBadge = class(TGraphicControl)
  private
    FPercent: integer;      // -1 = nothing to measure (placeholder shown)
    FValueText: string;     // "85%", an emoji, "--%" — or '' to draw nothing
    FMeanText: string;      // Formatted mean or ''
    FRangeCaption: string;  // The muted "in range" caption
    FMeanCaption: string;   // The muted "avg" caption in front of the mean
    FTextColor: TColor;
    FAccentColor: TColor;
    FTrackColor: TColor;
    FHover: boolean;        // Pointer over the badge: the bar is drawn
    FShowBar: boolean;      // Whether a bar is drawn at all (on hover)
    FAlignment: TAlignment; // Edge the stack hugs: taLeftJustify or taRightJustify
    procedure SetTextColor(AValue: TColor);
    procedure SetAccentColor(AValue: TColor);
    procedure SetTrackColor(AValue: TColor);
    procedure SetRangeCaption(const AValue: string);
    procedure SetMeanCaption(const AValue: string);
    procedure SetShowBar(AValue: boolean);
    procedure SetAlignment(AValue: TAlignment);
    procedure Measure(ACanvas: TCanvas; availW: integer; out valueW, valueH,
      capPx, capW, capH, meanPx, meanW, meanH, gap, barH, barGap, totalW,
      totalH: integer; out showMeanCaption, showMean: boolean);
  protected
    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    {** Show a percentage (-1 for "nothing measured"), the text drawn for it
        (a number with %, an emoji, or the placeholder) and the optional mean.
        Updates Caption to the legacy "mean | value" string. }
    procedure SetState(APercent: integer; const AValueText, AMeanText: string);
    {** Draw nothing (stale data); Caption becomes ''. }
    procedure Clear;
    {** Content size for the current Font, measured on `ACanvas` — pass a
        scratch bitmap's canvas from layout code. `AvailWidth` is the most
        the badge may take: when the full row would not fit, the "avg" word
        is dropped first and then the mean, so the percentage and its
        caption always survive. }
    procedure PreferredSize(ACanvas: TCanvas; AvailWidth: integer;
      out AWidth, AHeight: integer);
    {** The percentage last set, -1 when there was nothing to measure. }
    property Percent: integer read FPercent;
    {** Colour of the captions, the mean and a neutral percentage. }
    property TextColor: TColor read FTextColor write SetTextColor;
    {** Colour of the percentage and the bar fill; the form sets the good/bad
        tint here, or TextColor when neither threshold is crossed. }
    property AccentColor: TColor read FAccentColor write SetAccentColor;
    {** Colour of the unfilled part of the bar. }
    property TrackColor: TColor read FTrackColor write SetTrackColor;
    {** The muted caption drawn under the value ("in range", "ago"). }
    property RangeCaption: string read FRangeCaption write SetRangeCaption;
    {** Draw the hover bar (time in range) or never (the ago readout). }
    property ShowBar: boolean read FShowBar write SetShowBar;
    {** Which edge the stack hugs inside the box. Right by default. }
    property Alignment: TAlignment read FAlignment write SetAlignment;
    {** The muted caption drawn before the mean ("avg"). }
    property MeanCaption: string read FMeanCaption write SetMeanCaption;
    property Font;
    property Visible;
    property Caption;
    property Hint;
    property PopupMenu;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
  end;

implementation

const
  CAPTION_MAX_RATIO = 0.7;  // Caption font never taller than this share of the value font
  MIN_CAPTION_PX = 6;
  MEAN_RATIO = 0.62;        // Mean font height as a fraction of the value font
  WORD_GAP_RATIO = 0.4;     // Space between the mean and the stack, per value font px
  CAPTION_TUCK = 0.18;      // How far the caption rides up into the value's descender space
  BAR_RATIO = 0.16;         // Bar height as a fraction of the value text height
  BAR_GAP_RATIO = 0.12;     // Space between caption and bar, likewise

constructor TStatBadge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPercent := -1;
  FValueText := '';
  FMeanText := '';
  FRangeCaption := '';
  FMeanCaption := '';
  FTextColor := clWhite;
  FAccentColor := clWhite;
  FTrackColor := clGray;
  FHover := false;
  FShowBar := true;
  FAlignment := taRightJustify;
  ControlStyle := ControlStyle - [csOpaque];
end;

procedure TStatBadge.SetShowBar(AValue: boolean);
begin
  if FShowBar = AValue then
    Exit;
  FShowBar := AValue;
  Invalidate;
end;

procedure TStatBadge.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TStatBadge.MouseEnter;
begin
  inherited MouseEnter;
  if FHover then
    Exit;
  FHover := true;
  Invalidate;
end;

procedure TStatBadge.MouseLeave;
begin
  inherited MouseLeave;
  if not FHover then
    Exit;
  FHover := false;
  Invalidate;
end;

procedure TStatBadge.SetState(APercent: integer; const AValueText, AMeanText: string);
begin
  if (FPercent = APercent) and (FValueText = AValueText) and (FMeanText = AMeanText) then
    Exit;
  FPercent := APercent;
  FValueText := AValueText;
  FMeanText := AMeanText;
  if FMeanText <> '' then
    Caption := FMeanText + ' | ' + FValueText
  else
    Caption := FValueText;
  Invalidate;
end;

procedure TStatBadge.Clear;
begin
  SetState(-1, '', '');
end;

procedure TStatBadge.SetTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;
  FTextColor := AValue;
  Invalidate;
end;

procedure TStatBadge.SetAccentColor(AValue: TColor);
begin
  if FAccentColor = AValue then
    Exit;
  FAccentColor := AValue;
  Invalidate;
end;

procedure TStatBadge.SetTrackColor(AValue: TColor);
begin
  if FTrackColor = AValue then
    Exit;
  FTrackColor := AValue;
  Invalidate;
end;

procedure TStatBadge.SetRangeCaption(const AValue: string);
begin
  if FRangeCaption = AValue then
    Exit;
  FRangeCaption := AValue;
  Invalidate;
end;

procedure TStatBadge.SetMeanCaption(const AValue: string);
begin
  if FMeanCaption = AValue then
    Exit;
  FMeanCaption := AValue;
  Invalidate;
end;

// One measuring pass shared by PreferredSize and Paint. The value font is
// this control's Font (the form hands over the "ago" label's), the caption
// under it is sized so it spans the value's width, and the mean beside them
// is a fixed fraction of the value font. When the row would exceed availW
// (> 0), the "avg" word goes first and then the mean itself.
procedure TStatBadge.Measure(ACanvas: TCanvas; availW: integer; out valueW, valueH,
  capPx, capW, capH, meanPx, meanW, meanH, gap, barH, barGap, totalW,
  totalH: integer; out showMeanCaption, showMean: boolean);
var
  valuePx, lo, hi, mid, meanOnlyW, meanCapW: integer;

  procedure Total;
  begin
    meanW := 0;
    if showMean then
    begin
      meanW := meanOnlyW;
      if showMeanCaption then
        meanW := meanW + meanCapW;
    end;
    totalW := valueW;
    if meanW > 0 then
      totalW := totalW + meanW + gap;
  end;

begin
  ACanvas.Font.Assign(Font);
  ACanvas.Font.Style := [];
  valueH := ACanvas.TextHeight('0%');
  valueW := ACanvas.TextWidth(FValueText);
  valuePx := Abs(ACanvas.Font.Height);
  if valuePx = 0 then
    valuePx := valueH;
  gap := Max(2, Round(valuePx * WORD_GAP_RATIO));

  // Caption: the largest font (up to a share of the value's) whose rendering
  // of the text is no wider than the percentage above it.
  capPx := 0;
  capW := 0;
  capH := 0;
  if FRangeCaption <> '' then
  begin
    capPx := MIN_CAPTION_PX;
    lo := MIN_CAPTION_PX;
    hi := Max(MIN_CAPTION_PX, Round(valuePx * CAPTION_MAX_RATIO));
    while lo <= hi do
    begin
      mid := (lo + hi) div 2;
      ACanvas.Font.Height := -mid;
      if ACanvas.TextWidth(FRangeCaption) <= valueW then
      begin
        capPx := mid;
        lo := mid + 1;
      end
      else
        hi := mid - 1;
    end;
    ACanvas.Font.Height := -capPx;
    capW := ACanvas.TextWidth(FRangeCaption);
    capH := ACanvas.TextHeight(FRangeCaption);
  end;

  meanPx := Max(MIN_CAPTION_PX, Round(valuePx * MEAN_RATIO));
  ACanvas.Font.Height := -meanPx;
  meanH := ACanvas.TextHeight('0');
  meanOnlyW := 0;
  meanCapW := 0;
  if FMeanText <> '' then
  begin
    meanOnlyW := ACanvas.TextWidth(FMeanText);
    if FMeanCaption <> '' then
      meanCapW := ACanvas.TextWidth(FMeanCaption + ' ');
  end;

  barH := Max(2, Round(valueH * BAR_RATIO));
  barGap := Max(1, Round(valueH * BAR_GAP_RATIO));

  showMean := FMeanText <> '';
  showMeanCaption := showMean and (FMeanCaption <> '');
  Total;
  if (availW > 0) and (totalW > availW) and showMeanCaption then
  begin
    showMeanCaption := false;
    Total;
  end;
  if (availW > 0) and (totalW > availW) and showMean then
  begin
    showMean := false;
    Total;
  end;

  totalH := valueH;
  if capH > 0 then
    totalH := totalH + capH - Round(valueH * CAPTION_TUCK);
  if FShowBar then
    totalH := totalH + barGap + barH;
end;

procedure TStatBadge.PreferredSize(ACanvas: TCanvas; AvailWidth: integer;
  out AWidth, AHeight: integer);
var
  valueW, valueH, capPx, capW, capH, meanPx, meanW, meanH, gap, barH, barGap: integer;
  showMeanCaption, showMean: boolean;
begin
  if FValueText = '' then
  begin
    AWidth := 0;
    AHeight := 0;
    Exit;
  end;
  Measure(ACanvas, AvailWidth, valueW, valueH, capPx, capW, capH, meanPx, meanW,
    meanH, gap, barH, barGap, AWidth, AHeight, showMeanCaption, showMean);
end;

procedure TStatBadge.Paint;
var
  valueW, valueH, capPx, capW, capH, meanPx, meanW, meanH, gap, barH, barGap,
  totalW, totalH: integer;
  showMeanCaption, showMean: boolean;
  stackX, x, capY, barTop, fillW: integer;
begin
  inherited Paint;
  if (FValueText = '') or (Width <= 0) or (Height <= 0) then
    Exit;

  Canvas.Brush.Style := bsClear;
  // Fit to the box the layout gave us: the same shedding PreferredSize did.
  Measure(Canvas, Width, valueW, valueH, capPx, capW, capH, meanPx, meanW, meanH,
    gap, barH, barGap, totalW, totalH, showMeanCaption, showMean);

  // The stack hugs the edge of the box the badge sits against: a shorter
  // text keeps its outer edge put. The mean sits on the inner side.
  if FAlignment = taLeftJustify then
    stackX := 0
  else
    stackX := Width - valueW;

  // The percentage, in the value font and the accent colour.
  Canvas.Font.Assign(Font);
  Canvas.Font.Style := [];
  Canvas.Font.Color := FAccentColor;
  Canvas.TextOut(stackX, 0, FValueText);

  // Its caption, centred under it and tucked up into the descender space so
  // the two read as one unit.
  capY := valueH;
  if capH > 0 then
  begin
    capY := valueH - Round(valueH * CAPTION_TUCK);
    Canvas.Font.Height := -capPx;
    Canvas.Font.Color := FTextColor;
    Canvas.TextOut(stackX + (valueW - capW) div 2, capY, FRangeCaption);
    Inc(capY, capH);
  end;

  // The mean, muted, beside the stack on the value's baseline.
  if showMean then
  begin
    Canvas.Font.Height := -meanPx;
    Canvas.Font.Color := FTextColor;
    if FAlignment = taLeftJustify then
      x := stackX + valueW + gap
    else
      x := stackX - gap - meanW;
    if showMeanCaption then
    begin
      Canvas.TextOut(x, valueH - meanH, FMeanCaption + ' ');
      Inc(x, Canvas.TextWidth(FMeanCaption + ' '));
    end;
    Canvas.TextOut(x, valueH - meanH, FMeanText);
  end;

  // The bar, on hover only and only where wanted: a track as wide as the
  // stack, filled from the left to the percentage. An unmeasured window (-1)
  // shows an empty track. Its row is always reserved so the badge doesn't
  // jump when it appears.
  if not (FHover and FShowBar) then
    Exit;
  barTop := Height - barH;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := FTrackColor;
  Canvas.FillRect(stackX, barTop, stackX + valueW, Height);
  if FPercent > 0 then
  begin
    fillW := Round(valueW * Min(FPercent, 100) / 100);
    Canvas.Brush.Color := FAccentColor;
    Canvas.FillRect(stackX, barTop, stackX + fillW, Height);
  end;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
end;

end.
