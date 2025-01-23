
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
fpjson, jsonparser, dateutils, StrUtils, sha1, math, jsonscanner;


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
  minutesToSubtract: Integer;
begin
  // Hämta den aktuella tiden
  currentTime := Now;

  // Beräkna hur många minuter vi ska subtrahera
  minutesToSubtract := (MinuteOf(currentTime) mod 5) + (min * 5);

  // Fake mgdl val (reproducable)
  reading := 39+ round(minutesToSubtract*6.8);

  if (minutesToSubtract mod 3) <> 0 then
     reading := reading + (min*4);


  delta := math.min(minutesToSubtract, minutesToSubtract div 3);

  // Justera tiden genom att subtrahera minuter
  Result := IncMinute(currentTime, -minutesToSubtract);
end;

var
  i: integer;
  val, diff: integer;
begin
  SetLength(result, 11);
  for i := 0 to 10 do begin
      result[i].Init(mgdl);
      result[i].date := getFakeVals(i,val,diff);
      result[i].update(val, diff);

      if diff > 1 then
         result[i].trend := BGTrend.TdFortyFiveUp
      else if diff < 0 then
         result[i].trend := BGTrend.TdFortyFiveDown
      else
         result[i].trend := BGTrend.TdFlat;

      result[i].level := getLevel(result[i].val);
    end;


end;

end.
