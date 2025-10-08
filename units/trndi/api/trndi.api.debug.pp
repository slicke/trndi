
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
    constructor Create(user, pass, extra: string); override;
    function connect: boolean; override;
    function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
      override;
  private

  published
    property remote: string read baseUrl;
  end;

implementation

{------------------------------------------------------------------------------
  Constructor
------------------------------------------------------------------------------}
constructor DebugAPI.Create(user, pass, extra: string);
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

  Result := True;
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
