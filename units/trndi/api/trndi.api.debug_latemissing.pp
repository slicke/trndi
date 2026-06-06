
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

(*
 * Debug backend: Late-missing simulation.
 *
 * Behaviour:
 *   - Returns 2 days of history with a smooth sinusoidal glucose curve
 *     between 4 mmol/L (72 mg/dL) and 12 mmol/L (216 mg/dL).
 *   - The newest reading is anchored at (boot time − 14 minutes) so it is
 *     almost-but-not-yet stale when the app starts.
 *   - Once 1 minute of real time has elapsed since startup, that reading
 *     is 15+ minutes old and the API returns an empty result, triggering
 *     the "no readings received" state during normal operation.
 *
 * Use this backend to verify that the app handles mid-session data loss
 * correctly (as opposed to data that was already missing at boot).
 *)

unit trndi.api.debug_latemissing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.types, trndi.api, trndi.native, trndi.funcs,
  dateutils, Math;

type
  DebugLateMissingAPI = class(TrndiAPI)
  private
    FBootTime: TDateTime;
  public
    constructor Create(user, pass: string); override;
    function connect: boolean; override;
    function getReadings(min, maxNum: integer; extras: string;
      out res: string; noCache: boolean): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  protected
    function getLimitHigh: integer; override;
    function getLimitLow: integer; override;
    function getSystemName: string; override;
  end;

implementation

const
  // At boot the newest reading is this many minutes old
  INITIAL_AGE_MIN  = 14;
  // When the reading reaches this age the API returns nothing
  DROPOUT_AGE_MIN  = 15;

  // Sine curve parameters (values in mg/dL)
  CURVE_CENTER_MGDL = 144;   // 8.0 mmol/L
  CURVE_AMP_MGDL    = 72;    // ±4.0 mmol/L  → range 72–216 mg/dL (4–12 mmol/L)
  CURVE_PERIOD_MIN  = 720;   // 12-hour period for a realistic diurnal-ish shape

constructor DebugLateMissingAPI.Create(user, pass: string);
begin
  inherited Create(user, pass);
  FBootTime := Now;
end;

function DebugLateMissingAPI.Connect: boolean;
begin
  cgmHi      := 180;   // ~10 mmol/L
  cgmLo      := 72;    //  ~4 mmol/L
  cgmRangeHi := 162;   //  ~9 mmol/L
  cgmRangeLo := 90;    //  ~5 mmol/L
  TimeDiff   := 0;
  Result     := true;
end;

function DebugLateMissingAPI.getSystemName: string;
begin
  Result := 'Debug Late-Missing API';
end;

function DebugLateMissingAPI.getLimitHigh: integer;
begin
  Result := 400;
end;

function DebugLateMissingAPI.getLimitLow: integer;
begin
  Result := 40;
end;

class function DebugLateMissingAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  Result := inherited ParamLabel(LabelName);
  case LabelName of
    APLUser:
      Result := '(ignored)';
    APLPass:
      Result := '(ignored)';
    APLDesc:
      Result := Result +
        'Debug backend. Last reading is 14 min old at startup; goes missing ' +
        'after 1 minute of operation (reading turns 15 min old). ' +
        'History covers 2 days with a sinusoidal 4–12 mmol/L curve.';
    APLDescHTML:
      Result := Result +
        'Debug backend. Last reading is <b>14 min old</b> at startup; goes ' +
        '<b>missing after 1 minute</b> of operation (reading turns 15 min old). ' +
        'History covers 2 days with a sinusoidal 4–12 mmol/L curve.';
    APLCopyright:
      Result := 'Björn Lindh <github.com/slicke>';
  end;
end;

function DebugLateMissingAPI.getReadings(min, maxNum: integer; extras: string;
  out res: string; {%H-}noCache: boolean): BGResults;

  // Glucose value (mg/dL) for a given TDateTime using a repeating sine wave.
  function CurveAt(const T: TDateTime): integer;
  var
    minsInPeriod: integer;
  begin
    minsInPeriod := (HourOf(T) * 60 + MinuteOf(T)) mod CURVE_PERIOD_MIN;
    Result := Round(CURVE_CENTER_MGDL +
      CURVE_AMP_MGDL * Sin(2 * Pi * minsInPeriod / CURVE_PERIOD_MIN));
  end;

var
  newestReading: TDateTime;
  count, i: integer;
  slotTime: TDateTime;
  val, prevVal, delta: integer;
begin
  res    := '';
  Result := nil;

  // Newest reading time is fixed relative to when the app started.
  newestReading := IncMinute(FBootTime, -INITIAL_AGE_MIN);

  // Once that reading is DROPOUT_AGE_MIN minutes old, return nothing.
  if MinutesBetween(Now, newestReading) >= DROPOUT_AGE_MIN then
    Exit;

  // Return at most maxNum readings going back by 5-minute slots.
  count := min div 5;
  if count > maxNum then count := maxNum;
  if count < 1     then count := 1;

  SetLength(Result, count);
  for i := 0 to count - 1 do
  begin
    slotTime := IncMinute(newestReading, -(i * 5));
    val      := CurveAt(slotTime);
    prevVal  := CurveAt(IncMinute(slotTime, -5));
    delta    := val - prevVal;

    Result[i].Init(mgdl, systemName);
    Result[i].date  := slotTime;
    Result[i].update(val, delta);
    Result[i].trend := CalculateTrendFromDelta(delta);
    Result[i].level := getLevel(Result[i].val);
  end;
end;

end.
