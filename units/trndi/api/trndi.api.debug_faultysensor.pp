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

unit trndi.api.debug_faultysensor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
  fpjson, jsonparser, dateutils, trndi.api.debug;

type
  DebugFaultySensorAPI = class(DebugAPI)
  protected
    function getSystemName: string; override;
  public
    function getReadings(min, maxNum: integer; extras: string; out res: string;
      noCache: boolean): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

function DebugFaultySensorAPI.getSystemName: string;
begin
  Result := 'Debug Faulty Sensor API';
end;

function DebugFaultySensorAPI.getReadings(min, maxNum: integer; extras: string;
  out res: string; {%H-}noCache: boolean): BGResults;
const
  // 2 -> 16 -> 5 -> 17 -> 3 -> 20 mmol/L in mg/dL
  FaultyPattern: array[0..5] of integer = (36, 288, 90, 306, 54, 360);
var
  i: integer;
  readingValue: integer;
  previousValue: integer;
  deltaValue: integer;
  newestTime: TDateTime;
  rssi, noise: MaybeInt;
begin
  res := '';
  rssi.exists := true;
  noise.exists := true;
  rssi.value := 55;
  noise.value := 22;

  newestTime := RecodeSecond(RecodeMilliSecond(Now, 0), 0);
  SetLength(Result, 11);
  for i := 0 to High(Result) do
  begin
    readingValue := FaultyPattern[i mod Length(FaultyPattern)];
    previousValue := FaultyPattern[(i + 1) mod Length(FaultyPattern)];
    deltaValue := readingValue - previousValue;

    Result[i].Init(mgdl, Self.systemName);
    Result[i].date := IncMinute(newestTime, -(i * 5));
    Result[i].update(readingValue, deltaValue);
    Result[i].trend := CalculateTrendFromDelta(deltaValue);
    Result[i].level := getLevel(Result[i].val);
    Result[i].updateEnv('Debug', rssi, noise);
  end;
end;

class function DebugFaultySensorAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  // User/pass/copyright inherit DebugAPI's shared defaults; only the
  // backend-specific description is customised here.
  Result := inherited ParamLabel(LabelName);
  case LabelName of
  APLDesc:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend simulates a faulty sensor by rapidly jumping between extreme values. The generated repeating pattern is 2, 16, 5, 17, 3, 20 mmol/L.';
  APLDescHTML:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend simulates a <b>faulty sensor</b> by rapidly jumping between extreme values. The generated repeating pattern is <b>2, 16, 5, 17, 3, 20 mmol/L</b>.';
  end;
end;

end.
