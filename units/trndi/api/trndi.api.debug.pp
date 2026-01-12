
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

unit trndi.api.debug;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
fpjson, jsonparser, dateutils;

type
  // Main class
DebugAPI = class(TrndiAPI)
protected
public
  constructor Create(user, pass: string); override;
  function connect: boolean; override;
  function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
    override;
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
end;

implementation

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
  Generate fake readings over the last 50 minutes at 5-minute intervals
------------------------------------------------------------------------------}
function DebugAPI.getReadings(min, maxNum: integer; extras: string;
out res: string): BGResults;

function getFakeVals(const min: integer; out reading, delta: integer): TDateTime;
  var
    currentTime: TDateTime;
    baseTime: TDateTime;
    minutesFromBase: integer;
    previousReading: integer;  // We're generating a delta
  begin
    res := '';
    // Get the current time and the 5 minutes to act on
    currentTime := Now;
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
  rssi, noise: maybeint;
begin
  noise.exists := true;
  rssi.exists := true;

  SetLength(Result, 11);
  for i := 0 to 10 do
  begin
    Result[i].Init(mgdl, self.systemname);
    Result[i].date := getFakeVals(i * 5, val, diff);
    Result[i].update(val, diff);
    Result[i].trend := CalculateTrendFromDelta(diff);
    Result[i].level := getLevel(Result[i].val);
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

end.
