
(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit trndi.api.debug_missing;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
trndi.api.debug, fpjson, jsonparser, dateutils;

type
  // Main class
DebugMissingAPI = class(DebugAPI)
protected
public
  function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
    override;
end;

implementation


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

function guessTrend(diff: integer): BGTrend;
  begin
    if diff < -20 then
      Result := TdDoubleDown
    else
    if diff < -15 then
      Result := TdSingleDown
    else
    if diff < -10 then
      Result := TdFortyFiveDown
    else
    if diff < 5 then
      Result := TdFlat
    else
    if diff < 10 then
      Result := TdFortyFiveUp
    else
    if diff < 15 then
      Result := TdSingleUp
    else
      Result := TdDoubleUp;
  end;

var
  i: integer;
  val, diff: integer;
begin
  fNow := IncHour(Now, -2);
  SetLength(Result, 11);
  for i := 0 to 10 do
  begin
    Result[i].Init(mgdl);
    Result[i].date := getFakeVals(i * 5, val, diff);
    Result[i].update(val, diff);
    Result[i].trend := guessTrend(diff);
    Result[i].level := getLevel(Result[i].val);
    Result[i].updateEnv('Debug');
  end;

end;

end.
