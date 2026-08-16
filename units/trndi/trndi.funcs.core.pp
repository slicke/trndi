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

{**
  @abstract(UI-free reading and trend helpers shared by every Trndi front end.)

  This unit holds the parts of @code(trndi.funcs) that are pure data logic —
  sorting @link(BGReading) arrays and deriving a trend (discrete or as an
  angle) from a glucose delta. It deliberately depends on nothing beyond the
  RTL and @code(trndi.types).

  It exists so that the @code(units/trndi/api/) drivers — which need exactly
  these helpers and nothing else from @code(trndi.funcs) — can be compiled
  without pulling the LCL (@code(Controls), @code(Forms), @code(Graphics))
  into their uses closure. That is what lets a console or TUI front end reuse
  the backend drivers unchanged.

  @code(trndi.funcs) re-declares these routines as thin forwarders, so LCL
  callers keep working without adding this unit to their uses clause.
}

unit trndi.funcs.core;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, trndi.types;

{$ifdef DEBUG}
type
  _arr2 = array[1..2] of string;
  _arr3 = array[1..3] of string;
{$endif}

const
  {** Smallest lean shown once the value is actually moving (degrees); keeps
      direction readable. }
ARROW_MIN_TILT = 10;
  {** Cap for the rotating trend arrow (degrees from flat); keeps it a lean,
      never vertical. }
ARROW_MAX_ANGLE = 70;

{** Sort readings newest-first, in place, by @code(date). }
procedure SortReadingsDescending(var Readings: array of BGReading);
{** Sort readings oldest-first, in place, by @code(date). }
procedure SortReadingsAscending(var Readings: array of BGReading);
{** Map a 5-minute glucose delta to a discrete CGM trend arrow. }
function CalculateTrendFromDelta(delta: single): BGTrend;
{** Map a 5-minute glucose delta to a continuous arrow rotation angle. }
function CalculateTrendAngle(delta: single): single;
{** Representative rotation angle for a discrete trend arrow. }
function TrendToAngle(trend: BGTrend): single;

{$ifdef DEBUG}
{** Join parameters into one ` :: `-delimited line for debug logging. }
function debugParams(arr: TStringArray): string; overload;
function debugParams(arr: TStringList): string; overload;
function debugParams(arr: _arr2): string; overload;
function debugParams(arr: _arr3): string; overload;
{$endif}

{$ifdef DEBUG}
var
  {** Debug switch: log alert evaluation. Lives here rather than in
      @code(trndi.funcs) because the API drivers read it and must stay
      LCL-free; a @code(var) cannot be forwarded, so it has exactly one home. }
DEBUG_LOG_ALERT: boolean = false;
{$endif}

implementation

{$ifdef DEBUG}
function debugParams(arr: _arr2): string;
begin
  result := Format('%s :: %s', [arr[1], arr[2]]);
end;

function debugParams(arr: _arr3): string;
begin
  result := Format('%s :: %s :: %s', [arr[1], arr[2], arr[3]]);
end;

function debugParams(arr: TStringArray): string;
begin
  result := string.Join(' :: ', arr);
end;

function debugParams(arr: TStringList): string;
var
s: TStringList;
begin
  s := TStringList.Create;
  s.AddDelimitedText(arr.DelimitedText, arr.Delimiter, true);
  s.Delimiter := '`';
  result := StringReplace(s.DelimitedText, '`', ' :: ', [rfReplaceAll]);
  s.free;
end;
{$endif}

{------------------------------------------------------------------------------
  Shared quicksort over BGReading.date. `ascending` picks the comparison
  direction so both public entry points share one implementation.
 ------------------------------------------------------------------------------}
procedure SortReadingsCore(var Readings: array of BGReading; ascending: boolean);
  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: BGReading;
  begin
    repeat
      I := L;
      J := R;
      P := Readings[(L + R) div 2];
      repeat
        if ascending then
        begin
          while Readings[I].date < P.date do Inc(I);
          while Readings[J].date > P.date do Dec(J);
        end
        else
        begin
          while Readings[I].date > P.date do Inc(I);
          while Readings[J].date < P.date do Dec(J);
        end;
        if I <= J then
        begin
          T := Readings[I];
          Readings[I] := Readings[J];
          Readings[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;
begin
  if Length(Readings) > 1 then
    QuickSort(Low(Readings), High(Readings));
end;

procedure SortReadingsDescending(var Readings: array of BGReading);
begin
  SortReadingsCore(Readings, false);
end;

procedure SortReadingsAscending(var Readings: array of BGReading);
begin
  SortReadingsCore(Readings, true);
end;

function CalculateTrendFromDelta(delta: single): BGTrend;
begin
  // Calculate trend based on delta in mg/dL over 5 minutes
  // Based on standard CGM trend arrow thresholds
  if delta <= -15 then          // ≤-3 mg/dL/min
    Result := TdDoubleDown
  else
  if delta <= -10 then     // ≤-2 mg/dL/min
    Result := TdSingleDown
  else
  if delta <= -5 then      // ≤-1 mg/dL/min
    Result := TdFortyFiveDown
  else
  if delta < 5 then        // -1 to +1 mg/dL/min
    Result := TdFlat
  else
  if delta < 10 then       // +1 to +2 mg/dL/min
    Result := TdFortyFiveUp
  else
  if delta < 15 then       // +2 to +3 mg/dL/min
    Result := TdSingleUp
  else                          // ≥+3 mg/dL/min
    Result := TdDoubleUp;
end;

{------------------------------------------------------------------------------
  Maps a glucose delta to a continuous arrow rotation angle.

  Deliberately gentle, but always readable: only near-zero noise (within a tiny
  dead-band) stays flat. As soon as the value is genuinely moving the arrow
  leans by at least ARROW_MIN_TILT so the direction is unmistakable, then the
  tilt grows linearly with the rate of change — reaching 45° at a clearly-moving
  ~15 mg/dL (~0.8 mmol/L) over 5 minutes and capped at ARROW_MAX_ANGLE so even a
  fast move reads as a lean rather than a vertical spike.
 ------------------------------------------------------------------------------}
function CalculateTrendAngle(delta: single): single;
const
  DEAD_BAND = 0.5;         // mg/dL per 5 min; only near-zero noise stays flat
  DELTA_AT_45 = 15.0;      // mg/dL per 5 min mapped to 45°
var
  mag: single;
begin
  if Abs(delta) <= DEAD_BAND then
    Exit(0);
  // Magnitude: linear, but never so shallow it looks flat, never past the cap.
  mag := (Abs(delta) / DELTA_AT_45) * 45.0;
  if mag < ARROW_MIN_TILT then
    mag := ARROW_MIN_TILT
  else
  if mag > ARROW_MAX_ANGLE then
    mag := ARROW_MAX_ANGLE;
  if delta < 0 then
    Result := -mag
  else
    Result := mag;
end;

{------------------------------------------------------------------------------
  Representative rotation angle for a discrete trend arrow.

  Used as a fallback for the rotating arrow when no usable delta is available
  (e.g. the first reading, or a stale gap), so the arrow still points in the
  direction the CGM reported. Returns 0 for flat and unknown trends and at most
  ±60°, staying inside the ±ARROW_MAX_ANGLE that CalculateTrendAngle clamps to.
 ------------------------------------------------------------------------------}
function TrendToAngle(trend: BGTrend): single;
begin
  // Soft angles consistent with CalculateTrendAngle's gentle scale.
  case trend of
  TdDoubleUp:
    Result := 60;
  TdSingleUp:
    Result := 40;
  TdFortyFiveUp:
    Result := 20;
  TdFortyFiveDown:
    Result := -20;
  TdSingleDown:
    Result := -40;
  TdDoubleDown:
    Result := -60;
  else                          // TdFlat, TdNotComputable, TdPlaceholder
    Result := 0;
  end;
end;

end.
