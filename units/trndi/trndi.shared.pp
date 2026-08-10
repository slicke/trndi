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
function ContrastRatio(colorA, colorB: TColor): double;
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

// Lift a foreground color away from the background it will be drawn on until
// it reaches minRatio, and no further — the point is legibility without
// throwing away the color's identity. Blending toward pure black or pure white
// scales all three channels together, so the hue survives the adjustment.
//
// The pole is picked to move away from the background (darken on a light
// background, lighten on a dark one); the opposite pole is tried as a fallback
// because a mid-tone background caps how much contrast one direction can yield.
// If neither reaches the target, the most separated candidate found is
// returned rather than failing back to the original.
function EnsureContrast(foreground, background: TColor;
  minRatio: double = 3.0): TColor;
const
  // 5% steps. Fine enough that the result sits just past the target instead of
  // overshooting into a needlessly washed-out or muddy tone.
  STEPS = 20;
var
  poles: array[0..1] of TColor;
  candidate, best: TColor;
  ratio, bestRatio, bgLum: double;
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

  // IsLightColor's threshold, applied to the luminance already in hand.
  if bgLum > 0.179 then
  begin
    poles[0] := clBlack;
    poles[1] := clWhite;
  end
  else
  begin
    poles[0] := clWhite;
    poles[1] := clBlack;
  end;

  best := foreground;
  for pole := 0 to 1 do
    for i := 1 to STEPS do
    begin
      candidate := BlendColors(poles[pole], foreground, i / STEPS);
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
