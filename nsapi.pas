(*
 * This file is part of Trndi (https://github.com/xxxx or http://xxx.github.io).
 * Copyright (c) 2021 Björn Lindh.
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

unit nsapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
  fpjson, jsonparser, dateutils, StrUtils, sha1, math;

const
  NS_STATUS = 'status.json';

const
  NS_URL_BASE = '/api/v1/';

const
  NS_READINGS = 'entries/sgv.json';

type
  // Main class
  NightScout = class(TrndiAPI)
  protected
    // NS API key
    key: string;
  public
    constructor create(user, pass, extra: string); override;
    function connect: boolean; override;
    function getReadings(min, maxNum: integer; extras: string = ''): BGResults; override;
  private

  published
    property remote: string read baseUrl;
  end;

implementation

constructor NightScout.create(user, pass, extra: string);
begin
  ua      := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := TrimRightSet(user, ['/']) + NS_URL_BASE;
  key     := IfThen(pass <> '', 'API-SECRET=' + SHA1Print(SHA1String(pass)), '');
  inherited;
end;

function NightScout.connect: boolean;
var
  y, r:  string;
  td: tdatetime;
  i: int64;
begin
  // Check that username / pass is OK!

  if Copy(baseUrl, 1, 4) <> 'http' then begin
     result := false;
     lasterr := 'Invalid address. It must start with http:// or https://!';
     Exit;
  end;
  y := native.request(false, NS_STATUS, [], '', key);
  if Trim(y) = '' then begin
  lasterr := 'Did not recieve any data from the server!';
  result := false;
  Exit;
  end;



  if y[1] = '+' then begin
    result  := false;
    lastErr := TrimLeftSet(y, ['+']);
    exit;
 end;
  r := TrimRightSet(copy(y, pos('bgHigh":', y)+8, 3), [',']);
  if TryStrToInt64(r,i) then
    cgmHi := i;

  r := TrimRightSet(copy(y, pos('bgLow":', y)+7, 3), [',']);
  if TryStrToInt64(r,i) then
    cgmLo := i;

  r := TrimRightSet(copy(y, pos('bgTargetTop":', y)+13, 3), [',']);
  if TryStrToInt64(r,i) then
    cgmRangeHi := i;

  r := TrimRightSet(copy(y, pos('bgTargetBottom":', y)+16, 3), [',']);
  if TryStrToInt64(r,i) then
    cgmRangeLo := i;

  y := copy(y, pos('serverTimeEpoch":', y) + 17, 13);

  if Pos('Unau', y) > 0 then begin
    result  := false;
    lastErr := 'Incorrect access code for NightScout';
    Exit;
 end;

  if not TryStrToInt64(y, i) then begin
    result := false;
    abort;
  end;
  td := UnixToDateTime(i div 1000);
  timeDiff := SecondsBetween(td, LocalTimeToUniversal(now));
  if timeDiff < 0 then
    timeDiff := 0;

  timeDiff := -1 * timeDiff;


  result   := true;
end;


// extras = path
function NightScout.getReadings(min, maxNum: integer; extras: string = ''): BGResults;
var
  js:     TJSONData;
  i:   integer;
  t: BGTrend;
  s: string;
  params: array[1..1] of string;
begin
  if extras = '' then extras := NS_READINGS;

   params[1] := 'count=' + IntToStr(maxNum);

 try
    js := GetJSON(native.request(false, extras, params, '', key));
 except
    Exit;
 end;

  SetLength(result, js.count);

  for i := 0 to js.count - 1 do
    with js.FindPath(Format('[%d]', [i])) do begin
      result[i].Init(mgdl, self.ToString);
      result[i].update(FindPath('sgv').AsInteger, single(FindPath('delta').AsFloat));

      s := FindPath('direction').AsString;

      for t in BGTrend do begin
        if BG_TRENDS_STRING[t] = s then begin
          result[i].trend := t;
          break;
        end;
        result[i].trend := TdNotComputable;
      end;

      result[i].date := JSToDateTime(FindPath('date').AsInt64);
      result[i].level := getLevel(result[i].val);
   end;


end;

end.
