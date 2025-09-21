
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

unit trndi.api.debug_edge;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
fpjson, jsonparser, dateutils;


type
  // Main class
DebugEdgeAPI = class(TrndiAPI)
protected
public
  constructor create(user, pass, extra: string);
    override;
  function connect: boolean;
    override;
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
constructor DebugEdgeAPI.create(user, pass, extra: string);
begin
  ua      := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := user;
  inherited;
end;

{------------------------------------------------------------------------------
  Connect: set extreme thresholds and zero time diff for edge-case testing
------------------------------------------------------------------------------}
function DebugEdgeAPI.Connect: boolean;
begin
  cgmHi      := 160;
  cgmLo      := 60;
  cgmRangeHi := 140;
  cgmRangeLo := 90;

  TimeDiff := 0;

  Result := true;
end;

{------------------------------------------------------------------------------
  Generate alternating high/low extreme readings to exercise edge conditions
------------------------------------------------------------------------------}
function DebugEdgeAPI.getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
var
  i: integer;
  val, diff: integer;
  dbase: TDateTime;
  hi: boolean;
begin
  res := '';
  SetLength(result, 11);
  dbase := IncMinute(now, 5);
  for i := 0 to 10 do
  begin
    hi := MinuteOf(dbase) mod 2 = 0;
    dbase := IncMinute(dbase, -5);
    result[i].Init(mgdl);
    result[i].date := dbase;
    if hi then begin
       result[i].update(400, cgmHi);
       result[i].trend := BGTrend.TdDoubleUp;
       result[i].level := BGHigh;
    end
    else begin
       result[i].update(40, -cgmHi);
       result[i].trend := BGTrend.TdDoubleDown;
       result[i].level := BGLOW;
    end;
    result[i].updateEnv('DebugEdge');
  end;


end;

end.
