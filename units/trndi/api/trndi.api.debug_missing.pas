
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

unit trndi.api.debug_missing;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
trndi.api.debug, trndi.funcs, fpjson, jsonparser, dateutils;

type
  // Main class
DebugMissingAPI = class(DebugAPI)
protected
  function  getSystemName: string; override;
public
  function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
    override;
end;

implementation

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DebugMissingAPI.getSystemName: string;
begin
  result := 'Debug Missing API';
end;

function DebugMissingAPI.getReadings(min, maxNum: integer; extras: string;
out res: string): BGResults;
var
  fNow: TDateTime;

function getFakeVals(const min: integer; out reading, delta: integer): TDateTime;
  var
    currentTime: TDateTime;
    baseTime: TDateTime;
    minutesFromBase: integer;
    previousReading: integer;  // We're generating a delta
  begin
    res := '';
    // Get the current time and the 5 minutes to act on
    currentTime := fNow;
    baseTime := IncMinute(currentTime, -min);
    minutesFromBase := (MinuteOf(baseTime) div 5) * 5;

    Result := RecodeMinute(baseTime, minutesFromBase);
    Result := RecodeSecond(Result, 0);
    Result := RecodeMilliSecond(Result, 0);


    // Generate a fake reading
    reading := 40 + ((DateTimeToUnix(Result) div 300) mod 360);

    // Generate the previous 5 min reading
    previousReading := 40 + ((DateTimeToUnix(IncMinute(Result, -5)) div 300) mod 360);

    // Set the delta
    delta := reading - previousReading;
  end;

var
  i: integer;
  val, diff: integer;
  nodata: maybeint;
begin
  nodata.exists := false;
  fNow := IncHour(Now, -2);
  SetLength(Result, 11);
  for i := 0 to 10 do
  begin
    Result[i].Init(mgdl, self.systemName);
    Result[i].date := getFakeVals(i * 5, val, diff);
    Result[i].update(val, diff);
    Result[i].trend := CalculateTrendFromDelta(diff);
    Result[i].level := getLevel(Result[i].val);
    Result[i].updateEnv('Debug', nodata, nodata);
  end;

end;

end.
