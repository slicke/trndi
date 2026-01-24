
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

unit trndi.api.debug_custom;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types,
trndi.api.debug, fpjson, jsonparser, dateutils, trndi.api;

type
  // Main class
DebugCustomAPI = class(DebugAPI)
protected
  function getSystemName: string; override;
public
  setval: integer;
  function getReadings({%H-}min, {%H-}maxNum: integer; {%H-}extras: string;
    out res: string): BGResults; override;
  constructor Create(user, pass: string); override;

  class function ParamLabel(LabelName: APIParamLabel): string; override;
end;

implementation

function DebugCustomAPI.getSystemName: string;
begin
  result := 'Debug Custom API';
end;

constructor DebugCustomAPI.Create(user, pass: string);
var
  mmol: glucose;
  fs: TFormatSettings;
begin
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := user;
  //key     := pass;
  if pass = 'mmol' then begin
    fs.DecimalSeparator := '.';
    if TryStrToFloat(user, mmol, fs) then
      setval := round(mmol * TrndiAPI.toMGDL)
    else
      setval := 99;
  end else
  if not TryStrToInt(user, setval) then
    setval := 99;
  inherited;
end;

function DebugCustomAPI.getReadings(min, maxNum: integer; extras: string;
out res: string): BGResults;

function getFakeTime(const min: integer): TDateTime;
  var
    currentTime: TDateTime;
    baseTime: TDateTime;
    minutesFromBase: integer;
  begin
    res := '';
    // Get the current time and the 5 minutes to act on
    currentTime := Now;
    baseTime := IncMinute(currentTime, -min);
    minutesFromBase := (MinuteOf(baseTime) div 5) * 5;

    Result := RecodeMinute(baseTime, minutesFromBase);
    Result := RecodeSecond(Result, 0);
    Result := RecodeMilliSecond(Result, 0);
  end;

var
  nodata: maybeint;
  i: integer;
begin
  nodata.exists := false;
  SetLength(Result, 11);
  for i := 0 to 10 do
  begin
    Result[i].Init(mgdl, self.systemName);
    Result[i].date := getFakeTime(i * 5); // Get the time
    Result[i].update(setval, 0); // Set reading
    Result[i].trend := tdFlat;  // Always flat
    Result[i].level := BGRange;
    Result[i].updateEnv('Debug', nodata, nodata);
  end;

end;

class function DebugCustomAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  case LabelName of
   APLUser:
    Result := 'Reading to fake in mg/DL';
   APLPass:
    Result := 'Enter "mmol" exactly to provide a mmol/L reading above (no quotes; use . as separator)';
   APLDesc:
    Result := result + sLineBreak + sLineBreak + 'You can enter any reading (in mg/dL) as username, to have Trndi use that as reading';
   APLDescHTML:
    Result := result + '<br><br>You can enter <b>any</b> reading <i>(in mg/dL)</i> as <u>username</u>, to have Trndi use that as reading';
   APLCopyright:
    Result := 'Björn Lindh <github.com/slicke>';
  end;
end;

end.
