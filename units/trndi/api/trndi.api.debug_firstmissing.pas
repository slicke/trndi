
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

unit trndi.api.debug_firstmissing;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
fpjson, jsonparser, dateutils, trndi.api.debug;

type
  // Main class
DebugFirstMissingAPI = class(DebugAPI)
protected
public
  function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
    override;
end;

implementation

{------------------------------------------------------------------------------
  Generate fake readings over the last 50 minutes at 5-minute intervals,
  with the first reading missing
------------------------------------------------------------------------------}
function DebugFirstMissingAPI.getReadings(min, maxNum: integer; extras: string;
out res: string): BGResults;
var
  i: integer;
begin
    result := inherited getReadings(min, maxNum, extras, res);
    for  i := Low(result) to High(result) do begin
      result[i].date := result[i].date - EncodeTime(0, 10, 0, 0);
    end;
end;

end.
