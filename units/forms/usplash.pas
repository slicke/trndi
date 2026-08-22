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
unit usplash;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, LCLType,
LCLIntf, Math;

type

  { TfSplash }

TfSplash = class(TForm)
  Image1: TImage;
  lTrndi: TLabel;
  lVersion: TLabel;
  lInfo: TLabel;
  lSplashWarn: TLabel;
  pbProgress: TPaintBox;
  procedure FormCreate(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  procedure FormKeyPress(Sender: TObject; var Key: char);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure Image1Click(Sender: TObject);
  procedure pbProgressPaint(Sender: TObject);
private
  FProgress: integer;    // Target percentage set by incProgress
  FShown: double;        // Displayed percentage, eased toward FProgress
  FCardRect: TRect;      // Warning card, computed from the scaled label bounds
  FDividerY: integer;    // Divider under the warning heading, inside the card
  FUseFade: boolean;     // AlphaBlend fades enabled (skipped on problematic WMs)
  FFadedIn: boolean;     // Fade-in already ran (Show is called more than once)
  FCentered: boolean;    // Re-centered after the dynamic height was applied
  lWarnTitle: TLabel;    // Heading split off the translated warning caption
  lPct: TLabel;          // Percentage readout beside the status line
  tAnim: TTimer;
  procedure AnimTick(Sender: TObject);
  procedure SplitWarning;
  procedure UpdateLayout;
public
  procedure incProgress(const proc: integer; const title: string);
  procedure FadeOutAndClose;
end;

var
fSplash: TfSplash;

implementation

{$R *.lfm}

uses
buildinfo, slicke.versioninfo, slicke.ux.native;

const
  CARD_PAD    = 14;  // Inner padding between the card edge and the warning text
  CARD_CORNER = 18;
  EDGE_PAD    = 16;  // Outer padding for the status line and progress bar
  FADE_STEPS  = 10;  // × 12 ms sleep ≈ 120 ms per fade

  COL_CARD    = $001E1E1E;
  COL_BORDER  = $00363636;
  COL_HILIGHT = $004E4E4E;
  COL_TRACK   = $00262626;
  COL_ACCENT  = $0000DC84; // Trndi green, matches the progress fill
  COL_ACCENT_DIM = $00002214;
  COL_BG_TOP  = $001A1A1A; // Depth gradient, fades to black at the bottom
  COL_WARN    = $0020B0FF; // Amber, for the medical-warning heading

{ TfSplash }

procedure TfSplash.incProgress(const proc: integer; const title: string);
begin
  lInfo.Caption := title;
  if proc < 1 then
    Inc(FProgress, -proc)
  else
    FProgress := proc;
  if FProgress > 100 then
    FProgress := 100;
  tAnim.Enabled := true;
  Application.ProcessMessages;
end;

// The warning text ships as one translated caption (heading, blank line,
// body). Move the heading into its own styled label so it can be emphasized
// without splitting the .po string into separate translation units.
procedure TfSplash.SplitWarning;
var
  s, first, rest: string;
  p: integer;
begin
  s := StringReplace(lSplashWarn.Caption, #13, '', [rfReplaceAll]);
  p := Pos(#10, s);
  if p = 0 then
    Exit;
  first := Trim(Copy(s, 1, p - 1));
  rest := s;
  Delete(rest, 1, p);
  rest := TrimLeft(rest);
  if (first = '') or (rest = '') then
    Exit;
  lWarnTitle.Caption := first;
  lSplashWarn.Caption := rest;
end;

// Ease the displayed bar toward the target so progress glides instead of
// jumping. Ticks are pumped by the ProcessMessages calls sprinkled through
// boot, so the bar keeps moving between incProgress steps.
procedure TfSplash.AnimTick({%H-}Sender: TObject);
var
  diff: double;
begin
  diff := FProgress - FShown;
  if Abs(diff) < 0.5 then
  begin
    FShown := FProgress;
    tAnim.Enabled := false;
  end
  else
    FShown := FShown + Max(1.0, diff * 0.25) * Sign(diff);
  lPct.Caption := IntToStr(Round(FShown)) + '%';
  pbProgress.Invalidate;
end;

// Size the warning card to the text it actually holds. Translated warnings run
// longer than the English original, and on HiDPI displays the labels scale
// while painted pixels do not — so both the card rect and the form height are
// derived from the scaled label bounds instead of design-time constants.
procedure TfSplash.UpdateLayout;
var
  pad, gap, inner, cardL, cardR, cardT, cardB, warnH, infoH, titleH,
  contentB, pctW: integer;
  r: TRect;
  txt: string;
begin
  if not lSplashWarn.Visible then
  begin
    // Problematic-WM mini splash lays itself out in umain_init
    lWarnTitle.Visible := false;
    lPct.Visible := false;
    Exit;
  end;

  pad   := Scale96ToForm(EDGE_PAD);
  gap   := Scale96ToForm(10);
  inner := Scale96ToForm(CARD_PAD);

  cardL := Image1.Left + Image1.Width + pad;
  cardR := ClientWidth - Scale96ToForm(12);

  // Title and version sit above the card
  lTrndi.Left  := cardL;
  lTrndi.Width := cardR - cardL;
  lTrndi.Top   := Scale96ToForm(12);
  lVersion.Left  := cardL;
  lVersion.Width := cardR - cardL;
  lVersion.Top   := lTrndi.Top + lTrndi.Height + Scale96ToForm(2);

  cardT := lVersion.Top + lVersion.Height + gap;

  // Heading centered at the top of the card, divider below it
  Canvas.Font.Assign(lWarnTitle.Font);
  titleH := Canvas.TextHeight(lWarnTitle.Caption) + Scale96ToForm(2);
  lWarnTitle.SetBounds(cardL + inner, cardT + inner,
    cardR - cardL - 2 * inner, titleH);
  FDividerY := lWarnTitle.Top + titleH + gap;

  // Measure the (possibly translated) warning at its wrap width
  lSplashWarn.Left  := cardL + inner;
  lSplashWarn.Width := cardR - cardL - 2 * inner;
  lSplashWarn.Top   := FDividerY + gap;
  txt := lSplashWarn.Caption;
  r := Rect(0, 0, lSplashWarn.Width, 0);
  Canvas.Font.Assign(lSplashWarn.Font);
  DrawText(Canvas.Handle, PChar(txt), Length(txt), r, DT_CALCRECT or DT_WORDBREAK);
  warnH := r.Bottom - r.Top;
  lSplashWarn.Height := warnH;

  cardB := lSplashWarn.Top + warnH + inner;
  FCardRect := Rect(cardL, cardT, cardR, cardB);

  // Center the logo on the card
  Image1.Top := Max(lTrndi.Top, cardT + ((cardB - cardT) - Image1.Height) div 2);

  // Status line + progress bar (alBottom) close out the form
  contentB := Max(cardB, Image1.Top + Image1.Height);
  Canvas.Font.Assign(lInfo.Font);
  infoH := Canvas.TextHeight('Pq');
  ClientHeight := contentB + gap + infoH + Scale96ToForm(8) + pbProgress.Height;
  lInfo.Left := pad;
  lInfo.Top  := contentB + gap;
  pctW := Scale96ToForm(48);
  lPct.SetBounds(ClientWidth - pad - pctW, lInfo.Top, pctW, infoH);

  // poDesktopCenter used the design height; re-center once with the real one
  if not FCentered then
  begin
    FCentered := true;
    SetBounds((Screen.Width - Width) div 2, (Screen.Height - Height) div 2,
      Width, Height);
  end;
end;

procedure TfSplash.FormPaint({%H-}Sender: TObject);
var
  corner, accentH, mid, fadeB: integer;
begin
  // Subtle depth gradient behind the title so the form isn't a flat black
  // slab. It must reach pure black before the logo starts: the logo PNG has
  // its background baked in as solid black, so any lighter backdrop there
  // would show as a square around it.
  if Image1.Visible then
    fadeB := Image1.Top
  else
    fadeB := Height div 3;
  Canvas.GradientFill(Rect(0, 0, Width, fadeB), COL_BG_TOP, clBlack, gdVertical);

  // Accent glow along the very top, brightest in the middle
  accentH := Scale96ToForm(3);
  mid := Width div 2;
  Canvas.GradientFill(Rect(0, 0, mid, accentH),
    COL_ACCENT_DIM, COL_ACCENT, gdHorizontal);
  Canvas.GradientFill(Rect(mid, 0, Width, accentH),
    COL_ACCENT, COL_ACCENT_DIM, gdHorizontal);

  if IsRectEmpty(FCardRect) then
    Exit; // Mini-splash mode: no warning card

  corner := Scale96ToForm(CARD_CORNER);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := COL_CARD;
  Canvas.Pen.Style   := psClear;
  Canvas.RoundRect(FCardRect, corner, corner);
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Width   := 1;
  Canvas.Pen.Color   := COL_BORDER;
  Canvas.RoundRect(FCardRect, corner, corner);
  Canvas.Pen.Color := COL_HILIGHT;
  Canvas.MoveTo(FCardRect.Left + corner div 2, FCardRect.Top + 1);
  Canvas.LineTo(FCardRect.Right - corner div 2, FCardRect.Top + 1);

  // Divider between the heading and the warning body
  if FDividerY > 0 then
  begin
    Canvas.Pen.Color := COL_BORDER;
    Canvas.MoveTo(FCardRect.Left + Scale96ToForm(CARD_PAD), FDividerY);
    Canvas.LineTo(FCardRect.Right - Scale96ToForm(CARD_PAD), FDividerY);
  end;
end;

procedure TfSplash.FormShow({%H-}Sender: TObject);
var
  i: integer;
begin
  UpdateLayout;
  Invalidate;
  if FFadedIn then
    Exit;
  FFadedIn := true;
  if FUseFade then
  begin
    for i := 1 to FADE_STEPS do
    begin
      AlphaBlendValue := (255 * i) div FADE_STEPS;
      Application.ProcessMessages;
      Sleep(12);
    end;
    AlphaBlendValue := 255;
  end;
end;

// Fade the splash away before closing; boot calls this instead of Close.
// On WMs where fades are skipped this is just Close.
procedure TfSplash.FadeOutAndClose;
var
  i: integer;
begin
  if FUseFade and Visible then
    for i := FADE_STEPS - 1 downto 0 do
    begin
      AlphaBlendValue := (255 * i) div FADE_STEPS;
      Application.ProcessMessages;
      Sleep(12);
    end;
  Close;
end;

procedure TfSplash.pbProgressPaint({%H-}Sender: TObject);
var
  inset, radius, fillW: integer;
  track, bar: TRect;
begin
  with pbProgress.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Self.Color;
    Pen.Style   := psClear;
    FillRect(pbProgress.ClientRect);

    inset := Scale96ToForm(EDGE_PAD);
    if not lSplashWarn.Visible then
      inset := 0; // Mini-splash mode keeps the full-bleed bar
    track := Rect(inset, 0, pbProgress.Width - inset,
      pbProgress.Height - Scale96ToForm(2));
    radius := track.Bottom - track.Top;
    Brush.Color := COL_TRACK;
    RoundRect(track, radius, radius);

    fillW := Round((track.Right - track.Left) * FShown / 100);
    if fillW > 0 then
    begin
      fillW := Max(fillW, radius); // Keep the leading cap round
      bar := Rect(track.Left, track.Top, track.Left + fillW, track.Bottom);
      Brush.Color := COL_ACCENT;
      RoundRect(bar, radius, radius);
    end;
  end;
end;

procedure TfSplash.Image1Click({%H-}Sender: TObject);
begin

end;

procedure TfSplash.FormCreate({%H-}Sender: TObject);
var
  ver: string;
begin
  FProgress := 0;
  FShown := 0;
  FCardRect := Rect(0, 0, 0, 0);
  FDividerY := 0;

  // Heading label styled from the body font: bold amber, one step larger.
  // Created in code so the .lfm (and its translations) stay untouched.
  lWarnTitle := TLabel.Create(Self);
  lWarnTitle.Parent := Self;
  lWarnTitle.AutoSize := false;
  lWarnTitle.Alignment := taCenter;
  lWarnTitle.Transparent := true;
  lWarnTitle.Font.Assign(lSplashWarn.Font);
  lWarnTitle.Font.Color := COL_WARN;
  lWarnTitle.Font.Style := [fsBold];
  lWarnTitle.Font.Height := (lSplashWarn.Font.Height * 7) div 6;
  SplitWarning;

  lPct := TLabel.Create(Self);
  lPct.Parent := Self;
  lPct.AutoSize := false;
  lPct.Alignment := taRightJustify;
  lPct.Transparent := true;
  lPct.Font.Assign(lInfo.Font);
  lPct.Font.Color := COL_ACCENT;
  lPct.Caption := '0%';

  tAnim := TTimer.Create(Self);
  tAnim.Interval := 16;
  tAnim.OnTimer := @AnimTick;
  tAnim.Enabled := false;

  // Same version string Settings shows: product version, plus the CI build
  // number when one was stamped in.
  ver := GetProductVersionMajorMinor('12.x');
  {$PUSH}{$WARN 6018 OFF} // CI/BUILD_NUMBER are compile-time constants
  if CI and (BUILD_NUMBER <> 'dev') then
    ver := ver + '.' + BUILD_NUMBER
  else
    ver := ver + '-dev';
  {$POP}
  lVersion.Caption := 'v' + ver;

  // Fades are pointless (and slow) on WMs that get the mini splash — the
  // same condition umain_init uses to strip the splash down (GNOME counts as
  // problematic but keeps the full splash, so it keeps the fade too)
  FUseFade := not (IsProblematicWM and not IsSemiProblematicWM);
  if FUseFade then
  begin
    AlphaBlend := true;
    AlphaBlendValue := 0;
  end;
end;

procedure TfSplash.FormKeyDown({%H-}Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
begin
  if key = VK_ESCAPE then
    Hide;
end;

procedure TfSplash.FormKeyPress({%H-}Sender: TObject; var {%H-}Key: char);
begin

end;

end.
