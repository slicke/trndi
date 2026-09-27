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
 * - 2026-09-27: Returns the requested window, 24 hours by default; the
 *   scenario stays in the newest readings and older history follows the
 *   regular debug curve.
 *)

unit trndi.api.debug_lowsoon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.types, trndi.api, trndi.funcs.core,
  fpjson, jsonparser, dateutils, trndi.api.debug;

type
  DebugLowSoonAPI = class(DebugAPI)
  protected
    function getSystemName: string; override;
  public
    function getReadings(min, maxNum: integer; extras: string; out res: string;
      noCache: boolean): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

function DebugLowSoonAPI.getSystemName: string;
begin
  Result := 'Debug Low Soon API';
end;

function DebugLowSoonAPI.getReadings(min, maxNum: integer; extras: string;
  out res: string; {%H-}noCache: boolean): BGResults;
const
  // Linear fall from ~10 mmol/L (180 mg/dL) down to ~3.7 mmol/L (67 mg/dL)
  // over FALL_STEPS readings (i=FALL_STEPS at 180, i=0 newest).
  START_MGDL = 180;
  END_MGDL   = 67;
  FALL_STEPS = 10;
  // Readings kept on that line. predictReadings fits the newest 12, so the
  // fall covers all of them; anything older follows the regular debug curve.
  SCENARIO_SLOTS = 12;
var
  i: integer;
  readingValue, readingDelta: integer;
  rssi, noise: MaybeInt;
  newestTime: TDateTime;

  function LineValue(slot: integer): integer;
  begin
    Result := END_MGDL + (slot * (START_MGDL - END_MGDL)) div FALL_STEPS;
  end;

begin
  res := '';
  rssi.exists := true;
  noise.exists := true;
  rssi.value := 80;
  noise.value := 2;

  // Readings are spaced at the main window's 5-minute slot interval so they
  // render as distinct dots, and the steep falling slope drives the prediction
  // path toward low within the existing "soon" warning window.
  newestTime := RecodeMilliSecond(Now, 0);
  SetLength(Result, DebugSlotCount(min, maxNum));
  for i := 0 to High(Result) do
  begin
    if i >= SCENARIO_SLOTS then
    begin
      FakeCurveReading(Result[i], IncMinute(newestTime, -(i * 5)));
      Result[i].updateEnv('Debug', rssi, noise);
      Continue;
    end;

    readingValue := LineValue(i);
    readingDelta := readingValue - LineValue(i + 1);
    // The oldest scripted reading sits on the generated history, so measure
    // its change against that rather than the script's own continuation.
    if (i = SCENARIO_SLOTS - 1) and (i < High(Result)) then
      readingDelta := readingValue - FakeReading(IncMinute(newestTime, -((i + 1) * 5)));

    Result[i].Init(mgdl, self.systemname);
    Result[i].date := IncMinute(newestTime, -(i * 5));
    Result[i].update(readingValue, readingDelta);
    Result[i].trend := CalculateTrendFromDelta(readingDelta);
    Result[i].level := getLevel(Result[i].val);
    Result[i].updateEnv('Debug', rssi, noise);
  end;
end;

class function DebugLowSoonAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  // User/pass/copyright inherit DebugAPI's shared defaults; only the
  // backend-specific description is customised here.
  Result := inherited ParamLabel(LabelName);
  case LabelName of
  APLDesc:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend generates a short falling sequence where the 7th prediction crosses low so the prediction warning shows the "Low Predicted soon!" path.';
  APLDescHTML:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend generates a short falling sequence where the <b>7th prediction</b> crosses low so the prediction warning shows the <b>Low Predicted soon!</b> path.';
  end;
end;

end.