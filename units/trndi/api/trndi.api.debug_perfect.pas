
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

unit trndi.api.debug_perfect; // 5.5 mmol/L

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types,
  trndi.api.debug, fpjson, jsonparser, dateutils;

type
  // Main class
  DebugPerfectAPI = class(DebugAPI)
  protected
  public
    function getReadings({%H-}min, {%H-}maxNum: integer; {%H-}extras: string;
      out res: string): BGResults; override;
  end;

implementation


function DebugPerfectAPI.getReadings(min, maxNum: integer; extras: string;
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
  i: integer;
begin
  SetLength(Result, 11);
  for i := 0 to 10 do
  begin
    Result[i].Init(mgdl);
    Result[i].date := getFakeTime(i * 5); // Get the time
    Result[i].update(99, 0); // Set reading
    Result[i].trend := tdFlat;  // Always flat
    Result[i].level := BGRange;
    Result[i].updateEnv('Debug');
  end;

end;

end.
