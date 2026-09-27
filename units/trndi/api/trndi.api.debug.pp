
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
 * - 2026-08-16: Uses trndi.funcs.core (UI-free helper split) instead of
 *   trndi.funcs, and dropped the unused Dialogs import so the unit compiles in
 *   LCL-free (console) builds.
 * - 2026-09-27: Readings honour the requested window and default to 24 hours
 *   of history (DebugSlotCount), generated from a smooth daily curve with meal
 *   peaks instead of the old 30-hour sawtooth.
 *)

unit trndi.api.debug;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, trndi.types, trndi.api, trndi.funcs.core,
fpjson, jsonparser, dateutils;

const
  {** Readings a debug backend returns when the caller sets no limit: 24 hours
      at the 5-minute cadence. }
  DEBUG_DEFAULT_SLOTS = 288;

type
  // Main class
DebugAPI = class(TrndiAPI)
protected
public
  constructor Create(user, pass: string); override;
  function connect: boolean; override;
  function getReadings(min, maxNum: integer; extras: string; out res: string;
    noCache: boolean): BGResults; override;
  class function ParamLabel(LabelName: APIParamLabel): string; override;
private

published
  property remote: string read baseUrl;

protected
    {** Get the value which represents the maximum reading for the backend
  }
  function getLimitHigh: integer; override;

    {** Get the value which represents the minimum reading for the backend
  }
  function getLimitLow: integer; override;

  function getSystemName: string; override;

    {** 5-minute-aligned timestamp, minOffset minutes back from Now.
  }
  function FakeTime(minOffset: integer): TDateTime; overload;

    {** 5-minute-aligned timestamp, minOffset minutes back from base.
  }
  function FakeTime(minOffset: integer; const base: TDateTime): TDateTime; overload;

    {** Deterministic synthetic reading (mg/dL) for the given timestamp.
  }
  function FakeReading(const ts: TDateTime): integer;

    {** Fill r with the FakeReading curve at ts: value, delta, trend and level.
        The environment (sensor text, RSSI, noise) is left to the caller. }
  procedure FakeCurveReading(var r: BGReading; const ts: TDateTime);
end;

{** Number of 5-minute readings a debug backend returns for a getReadings
    request: enough to cover @code(minutes), capped at @code(maxNum). A value of
    zero or less means "no limit" for either; with neither set the answer is
    @link(DEBUG_DEFAULT_SLOTS). Never less than one. }
function DebugSlotCount(minutes, maxNum: integer): integer;

implementation

function DebugSlotCount(minutes, maxNum: integer): integer;
begin
  if minutes > 0 then
    Result := minutes div 5
  else
    Result := DEBUG_DEFAULT_SLOTS;
  if (maxNum > 0) and (Result > maxNum) then
    Result := maxNum;
  if Result < 1 then
    Result := 1;
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DebugAPI.getSystemname: string;
begin
  result := 'Debug API';
end;

{------------------------------------------------------------------------------
  Constructor
------------------------------------------------------------------------------}
constructor DebugAPI.Create(user, pass: string);
begin
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := user;
  //key     := pass;
  inherited;
end;

{------------------------------------------------------------------------------
  Connect: set deterministic thresholds and zero time diff
------------------------------------------------------------------------------}
function DebugAPI.Connect: boolean;
begin
  cgmHi := 160;
  cgmLo := 60;
  cgmRangeHi := 140;
  cgmRangeLo := 90;

  TimeDiff := 0;

  Result := true;
end;

{------------------------------------------------------------------------------
  FakeTime
  --------------------
  Returns a 5-minute-aligned timestamp, minOffset minutes back from a base time.
------------------------------------------------------------------------------}
function DebugAPI.FakeTime(minOffset: integer): TDateTime;
begin
  Result := FakeTime(minOffset, Now);
end;

function DebugAPI.FakeTime(minOffset: integer; const base: TDateTime): TDateTime;
var
  baseTime: TDateTime;
  minutesFromBase: integer;
begin
  baseTime := IncMinute(base, -minOffset);
  minutesFromBase := (MinuteOf(baseTime) div 5) * 5;

  Result := RecodeMinute(baseTime, minutesFromBase);
  Result := RecodeSecond(Result, 0);
  Result := RecodeMilliSecond(Result, 0);
end;

{------------------------------------------------------------------------------
  FakeReading
  --------------------
  Deterministic synthetic reading (mg/dL) derived from the timestamp: a daily
  shape with a quick rise and slow fall after breakfast, lunch and dinner and a
  dip in the small hours, on top of a slow 7-hour drift so no two days are
  identical. Spans roughly 55-230 mg/dL, so a day of history crosses low, in
  range and high.
------------------------------------------------------------------------------}
function DebugAPI.FakeReading(const ts: TDateTime): integer;
var
  unixMin: int64;
  dayMin: integer;
  v: double;

  // Skewed bell around centre (minute of day), wrapping across midnight
  function Bump(centre, height, rise, fall: integer): double;
  var
    d, w: integer;
  begin
    d := dayMin - centre;
    if d >= 720 then
      Dec(d, 1440)
    else
    if d < -720 then
      Inc(d, 1440);
    if d < 0 then
      w := rise
    else
      w := fall;
    Result := height * Exp(-Sqr(d) / (2 * Sqr(w)));
  end;

begin
  unixMin := DateTimeToUnix(ts) div 60;
  dayMin := unixMin mod 1440;

  v := 115 + 20 * Sin(2 * Pi * unixMin / 420)
    + Bump(8 * 60, 85, 25, 70)        // Breakfast
    + Bump(12 * 60 + 30, 60, 25, 70)  // Lunch
    + Bump(18 * 60 + 30, 95, 25, 80)  // Dinner
    - Bump(3 * 60, 40, 90, 90);       // Night dip

  Result := Round(v);
  if Result < 40 then
    Result := 40
  else
  if Result > 400 then
    Result := 400;
end;

{------------------------------------------------------------------------------
  FakeCurveReading
  --------------------
  One reading on the FakeReading curve, delta taken against the slot before.
------------------------------------------------------------------------------}
procedure DebugAPI.FakeCurveReading(var r: BGReading; const ts: TDateTime);
var
  val, diff: integer;
begin
  val := FakeReading(ts);
  diff := val - FakeReading(IncMinute(ts, -5));

  r.Init(mgdl, self.systemname);
  r.date := ts;
  r.update(val, diff);
  r.trend := CalculateTrendFromDelta(diff);
  r.level := getLevel(r.val);
end;

{------------------------------------------------------------------------------
  Generate fake readings at 5-minute intervals ending now, covering the
  requested window (24 hours when the caller sets no limit)
------------------------------------------------------------------------------}
function DebugAPI.getReadings(min, maxNum: integer; extras: string;
out res: string; {%H-}noCache: boolean): BGResults;
var
  i: integer;
  newest: TDateTime;
  rssi, noise: maybeint;
begin
  res := '';
  noise.exists := true;
  rssi.exists := true;

  newest := FakeTime(0);
  SetLength(Result, DebugSlotCount(min, maxNum));
  for i := 0 to High(Result) do
  begin
    FakeCurveReading(Result[i], IncMinute(newest, -(i * 5)));
    rssi.value := Random(100);
    noise.value := random(25);
    Result[i].updateEnv('Debug', rssi, noise);
  end;

end;

function DebugAPI.getLimitHigh: integer;
begin
  Result := 400; // Debug maximum high limit
end;

function DebugAPI.getLimitLow: integer;
begin
  Result := 40; // Debug minimum low limit
end;

class function DebugAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  case LabelName of
  APLUser:
    Result := '(ignored for debug backend)';
  APLPass:
    Result := '(ignored for debug backend)';
  APLDesc:
    Result := result + 'This is a special debug backend for testing purposes only. It does not connect to any real service.';
  APLDescHTML:
    Result := result + 'This is a special <b>debug backend</b> for testing purposes <u>only</u>. It does <i>not</i> connect to any real service.';
  APLCopyright:
    Result := 'Björn Lindh <github.com/slicke>';
  end;
end;

end.
