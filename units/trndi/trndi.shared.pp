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
  LCLIntf
  ;

// Color utility functions
function DarkenColor(originalColor: TColor; factor: double = 0.8): TColor;
function LightenColor(originalColor: TColor; factor: double = 0.8): TColor;
function IsLightColor(bgColor: TColor): boolean;

// System information functions  
function GetLinuxSystem: string;
function GetLinuxDistro(out ver: string): string;
function ScanLinuxDistro(const opts: TStringArray): string;

// UI utility functions
procedure ApplyRoundedCorners(APanel: TPanel; Radius: Integer);

// Text measurement functions
function CharsFit(Canvas: TCanvas; C: Char; TotalWidth: Integer): Integer;

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
  r, g, b: Integer; // Use Integer to hold intermediate results
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

function IsLightColor(bgColor: TColor): boolean;
var
  R, G, B: byte;
  r2, g2, b2: double;
  L: double;
begin
  // Get RBG
  R := GetRValue(bgColor);
  G := GetGValue(bgColor);
  B := GetBValue(bgColor);

  // Convert to 0-1
  r2 := R / 255.0;
  g2 := G / 255.0;
  b2 := B / 255.0;

  // Correct gamma
  if r2 <= 0.04045 then
    r2 := r2 / 12.92 else r2 := Power((r2 + 0.055) / 1.055, 2.4);
  if g2 <= 0.04045 then
    g2 := g2 / 12.92 else g2 := Power((g2 + 0.055) / 1.055, 2.4);
  if b2 <= 0.04045 then
    b2 := b2 / 12.92 else b2 := Power((b2 + 0.055) / 1.055, 2.4);

  // Calculate luminance
  L := 0.2126 * r2 + 0.7152 * g2 + 0.0722 * b2;

  // If L > 0.179 black is more suitable than white
  Result := (L > 0.179);
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
  if start > 0 then begin
    s := Copy(sys, start);
    stop := Pos(#10, s);
    result := Copy(s, 0, stop-1);
    result := TrimSet(result, ['"', #10]);
  end else result := '';

  if (result.IsEmpty) or (result[1] in ['0'..'9']) then begin
    start := Pos('NAME=', sys)+5; // NAME=...
    if start > 0 then begin
      s := Copy(sys, start);
      stop := Pos(#10, s);
      result := Copy(s, 0, stop-1);
      result := TrimSet(result, ['"', #10]);
    end else result := 'unknown';
  end;

  start := Pos('VERSION=', sys)+8; // VERSION="..."
  if start > 0 then begin
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
    if Pos(LowerCase(s), sys) > -1 then begin
      result := s;
      Exit;
    end;
  {$else}
  result := '';
  {$endif}
end;

// UI utility functions
procedure ApplyRoundedCorners(APanel: TPanel; Radius: Integer);
{$IFDEF WINDOWS}
var
  Rgn: HRGN;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Windows: Set a real rounded window region
  Rgn := CreateRoundRectRgn(0, 0, APanel.Width, APanel.Height, Radius, Radius);
  SetWindowRgn(APanel.Handle, Rgn, True);
  {$ENDIF}
end;

// Text measurement functions
function CharsFit(Canvas: TCanvas; C: Char; TotalWidth: Integer): Integer;
var
  CharWidth: Integer;
begin
  CharWidth := Canvas.TextWidth(C);
  if CharWidth > 0 then
    Result := TotalWidth div CharWidth
  else
    Result := 0;
end;

end.