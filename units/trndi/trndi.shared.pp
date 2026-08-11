(*
 * Trndi Shared Utilities
 * Autonomous utility functions extracted from umain.pp
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *)

unit trndi.shared;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Graphics, Controls, ExtCtrls, Math,
{$ifdef WINDOWS}
LCLType, Windows,
{$endif}
{$ifdef LINUX}
FileUtil, StrUtils,
{$endif}
LCLIntf;

// Color utility functions
function DarkenColor(originalColor: TColor; factor: double = 0.8): TColor;
function LightenColor(originalColor: TColor; factor: double = 0.8): TColor;
function RelativeLuminance(color: TColor): double;
function IsLightColor(bgColor: TColor): boolean;
function ColorChroma(color: TColor): integer;
function ColorDistance(colorA, colorB: TColor): integer;
function ContrastRatio(colorA, colorB: TColor): double;
procedure ColorToOKLCh(color: TColor; out L, C, H: double);
function OKLChToColor(L, C, H: double): TColor;
function EnsureContrast(foreground, background: TColor;
  minRatio: double = 3.0): TColor;
function BlendColors(foreground, background: TColor; alpha: double = 0.5): TColor;

// System information functions  
function GetLinuxSystem: string;
function GetLinuxDistro(out ver: string): string;
function ScanLinuxDistro(const opts: TStringArray): string;

// UI utility functions
procedure ApplyRoundedCorners(APanel: TPanel; Radius: integer);
procedure ApplyAlphaControl(Control: TWinControl; Alpha: byte);

// Text measurement functions
function CharsFit(Canvas: TCanvas; C: char; TotalWidth: integer): integer;

implementation

// Color utility functions
function DarkenColor(originalColor: TColor; factor: double = 0.8): TColor;
var
  r, g, b: byte;
begin
  // Extract RGB components
  r := GetRValue(originalColor);
  g := GetGValue(originalColor);
  b := GetBValue(originalColor);

  // Multiply by factor
  r := Round(r * factor);
  g := Round(g * factor);
  b := Round(b * factor);

  // Create new color
  Result := RGB(r, g, b);
end;

function LightenColor(originalColor: TColor; factor: double = 0.8): TColor;
var
  r, g, b: integer; // Use Integer to hold intermediate results
begin
  // Extract RGB components
  r := GetRValue(originalColor);
  g := GetGValue(originalColor);
  b := GetBValue(originalColor);
  // Add factor * (255 - component) to each component
  r := Round(r + (factor * (255 - r)));
  g := Round(g + (factor * (255 - g)));
  b := Round(b + (factor * (255 - b)));
  // Clip the values to the range 0..255
  r := Min(255, Max(0, r));
  g := Min(255, Max(0, g));
  b := Min(255, Max(0, b));
  // Create new color
  Result := RGB(r, g, b);
end;

// WCAG relative luminance (0 = black, 1 = white). Shared by IsLightColor and
// ContrastRatio so the gamma math lives in exactly one place.
function RelativeLuminance(color: TColor): double;
var
  rgb: array[0..2] of double;
  c: TColor;
  i: integer;
begin
  // ColorToRGB first: a system color (clWindow, clBtnFace) carries an index,
  // not channel bytes, and would otherwise decode as garbage.
  c := ColorToRGB(color);
  rgb[0] := GetRValue(c) / 255.0;
  rgb[1] := GetGValue(c) / 255.0;
  rgb[2] := GetBValue(c) / 255.0;

  // Correct gamma
  for i := 0 to 2 do
    if rgb[i] <= 0.04045 then
      rgb[i] := rgb[i] / 12.92
    else
      rgb[i] := Power((rgb[i] + 0.055) / 1.055, 2.4);

  Result := 0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2];
end;

function IsLightColor(bgColor: TColor): boolean;
begin
  // If L > 0.179 black is more suitable than white
  Result := RelativeLuminance(bgColor) > 0.179;
end;

// How much colour a colour carries, in channel units: 0 for any grey, 255 for
// a pure hue at full brightness.
//
// Deliberately not HSV saturation. That measure is scale-invariant — it divides
// the spread by the brightest channel — so a near-black maroon scores higher
// than a bright pink, and a dark olive scores as high as a vivid orange. For
// judging which of two colours will still look like a colour once it has been
// darkened, the undivided spread is the honest number.
function ColorChroma(color: TColor): integer;
var
  c: TColor;
begin
  c := ColorToRGB(color);
  Result := Max(GetRValue(c), Max(GetGValue(c), GetBValue(c))) -
    Min(GetRValue(c), Min(GetGValue(c), GetBValue(c)));
end;

// Straight-line distance between two colours in RGB space, 0 (identical) .. 441
// (black to white). Order of the arguments does not matter.
//
// Companion to ContrastRatio rather than a replacement for it: contrast answers
// "can this be made out against that", which is a question about luminance
// alone, so two colours of the same lightness in different hues score 1.0 no
// matter how obviously different they look. This answers the other question —
// "would anyone take these for the same colour" — which is what matters when
// something is drawn on top of something else that is already legible.
function ColorDistance(colorA, colorB: TColor): integer;
var
  a, b: TColor;
begin
  a := ColorToRGB(colorA);
  b := ColorToRGB(colorB);
  Result := Round(Sqrt(Sqr(GetRValue(a) - GetRValue(b)) +
    Sqr(GetGValue(a) - GetGValue(b)) + Sqr(GetBValue(a) - GetBValue(b))));
end;

// WCAG contrast ratio, 1.0 (identical) .. 21.0 (black on white). Order of the
// arguments does not matter.
function ContrastRatio(colorA, colorB: TColor): double;
var
  lighter, darker, tmp: double;
begin
  lighter := RelativeLuminance(colorA);
  darker := RelativeLuminance(colorB);
  if darker > lighter then
  begin
    tmp := lighter;
    lighter := darker;
    darker := tmp;
  end;
  Result := (lighter + 0.05) / (darker + 0.05);
end;

// ---- OKLab / OKLCh ---------------------------------------------------------
// Björn Ottosson's OKLab, via its polar form OKLCh: L is perceptual lightness
// (0..1), C is chroma (0 for grey, ~0.32 at the most saturated sRGB corner) and
// H the hue angle in radians. Used by EnsureContrast so a lift can change how
// *light* a color is without changing which color it is — blending toward a
// pole in sRGB bleeds chroma along the way, which is what used to wash lifted
// dots out on dark windows.

// Linear-light cube root; the OKLab transform needs it and Power() faults on a
// zero base, which black produces.
function CubeRoot(x: double): double;
begin
  if x <= 0 then
    Result := 0
  else
    Result := exp(ln(x) / 3.0);
end;

function SrgbToLinear(v: double): double;
begin
  if v <= 0.04045 then
    Result := v / 12.92
  else
    Result := Power((v + 0.055) / 1.055, 2.4);
end;

function LinearToSrgb(v: double): double;
begin
  if v <= 0.0031308 then
    Result := 12.92 * v
  else
    Result := 1.055 * Power(v, 1 / 2.4) - 0.055;
end;

procedure ColorToOKLCh(color: TColor; out L, C, H: double);
var
  c0: TColor;
  r, g, b, lm, mm, sm, a, bb: double;
begin
  // ColorToRGB first — same reason as RelativeLuminance: system colors carry
  // an index, not channel bytes.
  c0 := ColorToRGB(color);
  r := SrgbToLinear(GetRValue(c0) / 255.0);
  g := SrgbToLinear(GetGValue(c0) / 255.0);
  b := SrgbToLinear(GetBValue(c0) / 255.0);

  lm := CubeRoot(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b);
  mm := CubeRoot(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b);
  sm := CubeRoot(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b);

  L  := 0.2104542553 * lm + 0.7936177850 * mm - 0.0040720468 * sm;
  a  := 1.9779984951 * lm - 2.4285922050 * mm + 0.4505937099 * sm;
  bb := 0.0259040371 * lm + 0.7827717662 * mm - 0.8086757660 * sm;

  C := Sqrt(Sqr(a) + Sqr(bb));
  H := ArcTan2(bb, a);
end;

// The inverse, into linear sRGB. Split out so the gamut probe below can test a
// candidate without paying the transfer curve.
procedure OKLabToLinear(L, a, b: double; out r, g, bl: double);
var
  lm, mm, sm: double;
begin
  lm := L + 0.3963377774 * a + 0.2158037573 * b;
  mm := L - 0.1055613458 * a - 0.0638541728 * b;
  sm := L - 0.0894841775 * a - 1.2914855480 * b;
  lm := lm * lm * lm;
  mm := mm * mm * mm;
  sm := sm * sm * sm;

  r  := +4.0767416621 * lm - 3.3077115913 * mm + 0.2309699292 * sm;
  g  := -1.2684380046 * lm + 2.6097574011 * mm - 0.3413193965 * sm;
  bl := -0.0041960863 * lm - 0.7034186147 * mm + 1.7076147010 * sm;
end;

// Realize an OKLCh color in sRGB, keeping as much of the requested chroma as
// the gamut allows at that lightness: hue and lightness are honored exactly,
// chroma is reduced only when the (L, C, H) point falls outside sRGB. At L 0
// and 1 every hue's gamut closes to a point, so the extremes come out as pure
// black and white — which is what keeps EnsureContrast's search able to reach
// any contrast the old pole blend could.
function OKLChToColor(L, C, H: double): TColor;
const
  GAMUT_EPS = 0.0005; // channel slack before a candidate counts as outside
var
  cosH, sinH, lo, hi, mid, r, g, b: double;
  i: integer;

  function InGamut(cc: double): boolean;
  var
    rr, gg, bb: double;
  begin
    OKLabToLinear(L, cc * cosH, cc * sinH, rr, gg, bb);
    Result := (rr >= -GAMUT_EPS) and (rr <= 1 + GAMUT_EPS) and
      (gg >= -GAMUT_EPS) and (gg <= 1 + GAMUT_EPS) and
      (bb >= -GAMUT_EPS) and (bb <= 1 + GAMUT_EPS);
  end;

begin
  if L <= 0 then Exit(clBlack);
  if L >= 1 then Exit(clWhite);
  cosH := Cos(H);
  sinH := Sin(H);

  if not InGamut(C) then
  begin
    // Largest in-gamut chroma at this L and H, by bisection. 0 is always in
    // gamut (the grey axis), so the bracket is sound.
    lo := 0;
    hi := C;
    for i := 1 to 12 do
    begin
      mid := (lo + hi) / 2;
      if InGamut(mid) then
        lo := mid
      else
        hi := mid;
    end;
    C := lo;
  end;

  OKLabToLinear(L, C * cosH, C * sinH, r, g, b);
  Result := RGB(
    Round(EnsureRange(LinearToSrgb(EnsureRange(r, 0, 1)), 0, 1) * 255),
    Round(EnsureRange(LinearToSrgb(EnsureRange(g, 0, 1)), 0, 1) * 255),
    Round(EnsureRange(LinearToSrgb(EnsureRange(b, 0, 1)), 0, 1) * 255));
end;

// Lift a foreground color away from the background it will be drawn on until
// it reaches minRatio, and no further — the point is legibility without
// throwing away the color's identity. The lift travels the OKLCh lightness
// axis: hue is held exactly and chroma is kept as high as the gamut allows at
// each stop, so an amber pushed toward white arrives as a *light* amber, not
// the washed pastel the old sRGB pole blend produced. The path still ends at
// pure black or white (the gamut closes there), so anything the pole blend
// could reach, this reaches too.
//
// The direction is picked to move away from the background (darken on a light
// background, lighten on a dark one); the opposite direction is tried as a
// fallback because a mid-tone background caps how much contrast one side can
// yield. If neither reaches the target, the most separated candidate found is
// returned rather than failing back to the original.
function EnsureContrast(foreground, background: TColor;
  minRatio: double = 3.0): TColor;
const
  // 5% steps. Fine enough that the result sits just past the target instead of
  // overshooting into a needlessly washed-out or muddy tone.
  STEPS = 20;
var
  poleL: array[0..1] of double;
  candidate, best: TColor;
  ratio, bestRatio, bgLum, L0, C0, H0: double;
  pole, i: integer;

  // The background is fixed for the whole search, so hold its luminance rather
  // than re-running the gamma curve on it for all 40 candidates.
  function RatioAgainstBg(color: TColor): double;
  var
    lum: double;
  begin
    lum := RelativeLuminance(color);
    if lum > bgLum then
      Result := (lum + 0.05) / (bgLum + 0.05)
    else
      Result := (bgLum + 0.05) / (lum + 0.05);
  end;

begin
  Result := foreground;
  if minRatio <= 1.0 then
    Exit;

  bgLum := RelativeLuminance(background);
  bestRatio := RatioAgainstBg(foreground);
  if bestRatio >= minRatio then
    Exit;

  ColorToOKLCh(foreground, L0, C0, H0);

  // IsLightColor's threshold, applied to the luminance already in hand.
  if bgLum > 0.179 then
  begin
    poleL[0] := 0.0;
    poleL[1] := 1.0;
  end
  else
  begin
    poleL[0] := 1.0;
    poleL[1] := 0.0;
  end;

  best := foreground;
  for pole := 0 to 1 do
    for i := 1 to STEPS do
    begin
      candidate := OKLChToColor(L0 + (poleL[pole] - L0) * (i / STEPS), C0, H0);
      ratio := RatioAgainstBg(candidate);
      if ratio > bestRatio then
      begin
        bestRatio := ratio;
        best := candidate;
      end;
      if ratio >= minRatio then
        Exit(candidate);
    end;

  Result := best;
end;

function BlendColors(foreground, background: TColor; alpha: double = 0.5): TColor;
var
  fgR, fgG, fgB: byte;
  bgR, bgG, bgB: byte;
  outR, outG, outB: byte;
begin
  // Extract RGB components from foreground
  fgR := GetRValue(foreground);
  fgG := GetGValue(foreground);
  fgB := GetBValue(foreground);

  // Extract RGB components from background
  bgR := GetRValue(background);
  bgG := GetGValue(background);
  bgB := GetBValue(background);

  // Blend: output = (foreground * alpha) + (background * (1 - alpha))
  outR := Round(fgR * alpha + bgR * (1 - alpha));
  outG := Round(fgG * alpha + bgG * (1 - alpha));
  outB := Round(fgB * alpha + bgB * (1 - alpha));

  // Create blended color
  Result := RGB(outR, outG, outB);
end;

// System information functions
function GetLinuxSystem: string;
const
  Issue = '/etc/os-release';
begin
  {$ifdef LINUX}
  if FileExists(Issue) then
    Result := ReadFileToString(Issue)
  else
    Result := '';
  {$else}
  Result := '';
  {$endif}
end;

function GetLinuxDistro(out ver: string): string;
  {$ifdef LINUX}
var
  sys, s: string;
  start, stop: integer;
  {$endif}
begin
  Result := '';
  ver := '';
  {$ifdef LINUX}
  sys := GetLinuxSystem;

  start := Pos('ID=', sys)+3; // ID=...
  if start > 0 then
  begin
    s := Copy(sys, start);
    stop := Pos(#10, s);
    result := Copy(s, 0, stop-1);
    result := TrimSet(result, ['"', #10]);
  end
  else result := '';

  if (result.IsEmpty) or (result[1] in ['0'..'9']) then
  begin
    start := Pos('NAME=', sys)+5; // NAME=...
    if start > 0 then
    begin
      s := Copy(sys, start);
      stop := Pos(#10, s);
      result := Copy(s, 0, stop-1);
      result := TrimSet(result, ['"', #10]);
    end
    else result := 'unknown';
  end;

  start := Pos('VERSION=', sys)+8; // VERSION="..."
  if start > 0 then
  begin
    s := Copy(sys, start);
    stop := Pos(#10, s);
    ver := Copy(s, 0, stop-1);
    ver := TrimSet(ver, ['"', #10]);
  end;
  {$endif}
end;

function ScanLinuxDistro(const opts: TStringArray): string;
  {$ifdef LINUX}
var
  s, sys: string;
  {$endif}
begin
  {$ifdef LINUX}
  sys := LowerCase(GetLinuxDistro(s));
  result := s;
  for s in opts do
    if Pos(LowerCase(s), sys) > -1 then
    begin
      result := s;
      Exit;
    end;
  {$else}
  Result := '';
  {$endif}
end;

// UI utility functions
procedure ApplyRoundedCorners(APanel: TPanel; Radius: integer);
{$IFDEF WINDOWS}
var
  Rgn: HRGN;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Windows: Set a real rounded window region
  Rgn := CreateRoundRectRgn(0, 0, APanel.Width, APanel.Height, Radius, Radius);
  SetWindowRgn(APanel.Handle, Rgn, true);
  {$ENDIF}
end;

procedure ApplyAlphaControl(Control: TWinControl; Alpha: byte);
{$IFDEF WINDOWS}
var
  ExStyle: longint;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  ExStyle := GetWindowLong(Control.Handle, GWL_EXSTYLE);
  SetWindowLong(Control.Handle, GWL_EXSTYLE, ExStyle or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Control.Handle, 0, Alpha, LWA_ALPHA);
  {$ENDIF}
end;

// Text measurement functions
function CharsFit(Canvas: TCanvas; C: char; TotalWidth: integer): integer;
var
  CharWidth: integer;
begin
  CharWidth := Canvas.TextWidth(C);
  if CharWidth > 0 then
    Result := TotalWidth div CharWidth
  else
    Result := 0;
end;

end.
