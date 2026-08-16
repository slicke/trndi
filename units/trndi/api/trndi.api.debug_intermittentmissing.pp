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
 *)
(*
 * Trndi
 * Debug Intermittent Missing Backend
 *)

unit trndi.api.debug_intermittentmissing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.types, trndi.api, trndi.native, trndi.funcs.core,
  fpjson, jsonparser, dateutils, trndi.api.debug, trndi.log;

type
  // Main class
  DebugIntermittentMissingAPI = class(DebugAPI)
  protected
    MinMissing: integer;
    MaxMissing: integer;
    function getSystemName: string; override;
  public
    constructor Create(user, pass: string); override;
    function getReadings(min, maxNum: integer; extras: string; out res: string;
      noCache: boolean): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

{------------------------------------------------------------------------------
  Constructor - parse user parameter as "N" or "min-max" (e.g. "2-4")
------------------------------------------------------------------------------}
constructor DebugIntermittentMissingAPI.Create(user, pass: string);
var
  p: integer;
  s: string;
begin
  inherited Create(user, pass);
  // Defaults
  MinMissing := 2;
  MaxMissing := 4;
  s := Trim(user);
  if s = '' then exit;
  // user can be single number or range like "2-4"
  p := Pos('-', s);
  if p > 0 then
  begin
    MinMissing := StrToIntDef(Trim(Copy(s, 1, p - 1)), MinMissing);
    MaxMissing := StrToIntDef(Trim(Copy(s, p + 1, MaxInt)), MaxMissing);
  end
  else
  begin
    MinMissing := StrToIntDef(s, MinMissing);
    MaxMissing := MinMissing;
  end;
  if MinMissing < 0 then MinMissing := 0;
  if MaxMissing < MinMissing then MaxMissing := MinMissing;
  Randomize;
end;

function DebugIntermittentMissingAPI.getSystemName: string;
begin
  result := 'Debug Intermittent Missing API';
end;

// getReadings - randomly clear between MinMissing and MaxMissing readings
// among the most recent N readings (default N = 10) to simulate intermittent
// gaps and try to break UI/mapping logic.
function DebugIntermittentMissingAPI.getReadings(min, maxNum: integer; extras: string;
  out res: string; noCache: boolean): BGResults;
var
  i, toClear, rangeLen, rng: integer;
  idx, pickedCount: integer;
  picked: array of integer;
  isPicked: array of boolean;
  logmsg: string;
begin
  result := inherited getReadings(min, maxNum, extras, res, noCache);
  if Length(result) < 1 then
    Exit;
  // Only consider the newest N values so we don't clear very old data
  if Length(result) < 10 then rangeLen := Length(result) else rangeLen := 10;
  // Calculate a safe random range and clamp
  rng := MaxMissing - MinMissing + 1;
  if rng < 1 then rng := 1;
  toClear := MinMissing + Random(rng);
  if toClear > rangeLen then toClear := rangeLen;

  SetLength(isPicked, rangeLen);
  SetLength(picked, toClear);
  pickedCount := 0;

  // Choose toClear unique indices in [0..rangeLen-1] using O(1) membership checks.
  while pickedCount < toClear do
  begin
    idx := Random(rangeLen); // 0-based
    if not isPicked[idx] then
    begin
      isPicked[idx] := true;
      picked[pickedCount] := idx;
      Inc(pickedCount);
    end;
  end;

  logmsg := Format('DebugIntermittentMissing: Clearing %d readings at indices: ', [pickedCount]);
  for i := 0 to pickedCount - 1 do
  begin
    idx := picked[i];
    // Convert to array index (newest at Result[0]) -> keep same mapping
    // Clear selected reading
    result[idx].Clear;
    logmsg := logmsg + Format('%d(%s) ', [idx, DateTimeToStr(result[idx].date)]);
  end;

  log(logmsg);
end;

class function DebugIntermittentMissingAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  if LabelName = APLUser then
    Result := 'Number of missing readings (N) or range (min-max), e.g. "2" or "2-4"';
  if LabelName = APLDesc then
    Result := result + sLineBreak + sLineBreak + 'This debug backend randomly clears between the specified number of newest readings (default 2-4), selecting them from the most recent 10 readings. Use to reproduce intermittent/misaligned missing values.';
  if LabelName = APLDescHTML then
    Result := result + sLineBreak + sLineBreak + 'Use <b>"2"</b> or <b>"2-4"</b> in the username field to control how many readings are cleared. Readings are chosen randomly from the most recent 10 values.';
  // Copyright inherits DebugAPI's shared default.
end;

end.
