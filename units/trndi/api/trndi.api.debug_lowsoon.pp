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

unit trndi.api.debug_lowsoon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
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
  // across the 11 generated readings (i=10 oldest, i=0 newest).
  START_MGDL = 180;
  END_MGDL   = 67;
var
  i: integer;
  readingValue, nextValue, readingDelta: integer;
  rssi, noise: MaybeInt;
  newestTime: TDateTime;
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
  SetLength(Result, 11);
  for i := 0 to High(Result) do
  begin
    readingValue := END_MGDL + (i * (START_MGDL - END_MGDL)) div High(Result);
    if i < High(Result) then
    begin
      nextValue := END_MGDL + ((i + 1) * (START_MGDL - END_MGDL)) div High(Result);
      readingDelta := readingValue - nextValue;
    end
    else
      readingDelta := -((START_MGDL - END_MGDL) div High(Result));

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