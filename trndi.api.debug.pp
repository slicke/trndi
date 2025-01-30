
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

unit trndi.api.debug;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
fpjson, jsonparser, dateutils;


type
  // Main class
DebugAPI = class(TrndiAPI)
protected
public
  constructor create(user, pass, extra: string);
    override;
  function connect: boolean;
    override;
  function getReadings(min, maxNum: integer; extras: string = ''): BGResults;
    override;
private

published
  property remote: string read baseUrl;
end;

implementation

constructor DebugAPI.create(user, pass, extra: string);
begin
  ua      := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := user;
  //key     := pass;
  inherited;
end;

function DebugAPI.Connect: boolean;
begin
  cgmHi      := 160;
  cgmLo      := 60;
  cgmRangeHi := 140;
  cgmRangeLo := 90;

  TimeDiff := 0;

  Result := true;
end;


function DebugAPI.getReadings(min, maxNum: integer; extras: string = ''): BGResults;
function getFakeVals(const min: integer; out reading, delta: integer): TDateTime;
  var
    currentTime: TDateTime;
    baseTime: TDateTime;
    minutesFromBase: integer;
    previousReading: integer;  // We're generating a delta
  begin
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

function guessTrend(diff: integer): BGTrend;
  begin
    if diff < -20 then
      result := TdDoubleDown
    else
    if diff < -15 then
      result := TdSingleDown
    else
    if diff < -10 then
      result := TdFortyFiveDown
    else
    if diff < 5 then
      result := TdFlat
    else
    if diff < 10 then
      result := TdFortyFiveUp
    else
    if diff < 15 then
      result := TdSingleUp
    else
      result := TdDoubleUp
  end;

var
  i: integer;
  val, diff: integer;
begin
  SetLength(result, 11);
  for i := 0 to 10 do
  begin
    result[i].Init(mgdl);
    result[i].date := getFakeVals(i*5,val,diff);
    result[i].update(val, diff);
    result[i].trend := guessTrend(diff);
    result[i].level := getLevel(result[i].val);
  end;


end;

end.
