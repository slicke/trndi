
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

unit trndi.api;

{$mode ObjFPC}{$H+}

interface


uses 
Classes, SysUtils, trndi.types, dateutils, trndi.native, dialogs;

type 
  CGMCore = record
    hi, lo, top, bottom: integer;
  end;

  // Main class
  TrndiAPI = class
    protected 
      timeDiff: integer;
      // The time adjustment
      tz:      integer;
      // Native functions
      native: TrndiNative;
      // HTTP address to API
      ua: string;
      baseUrl: string;
      lastErr: string;
      core: CGMCore;

      procedure setTZ(secs: integer);
      function encodeStr(src: string): string;
      virtual;
      final;
      function getLevel(v: BGValLevel): single;
      virtual;
      function getCGMCore: CGMCore;
      procedure initCGMCore;
    public 
      function getReadings(min, maxNum: integer; extras: string = ''): BGResults;
      virtual;
      abstract;
      constructor create(user, pass, extra: string);
      virtual;
      function connect: boolean;
      virtual;
      abstract;
      function getLevel(v: single): BGValLevel;
      virtual;

      function getLast(var res: BGReading): boolean;
      function getCurrent(var res: BGReading): boolean;
      function getBasetime: int64;

      function JSToDateTime(ts: int64; correct: boolean = true): TDateTime;
      virtual;
      property threshold[lvl: BGValLevel]: single read getLevel;
      property cgm: CGMCore read getCGMCore;
      property cgmHi: integer read core.hi write core.hi;
      property cgmLo: integer read core.lo write core.lo;
      property cgmRangeHi: integer read core.top write core.top;
      property cgmRangeLo: integer read core.bottom write core.bottom;

    published 
      property offset: integer read timeDiff;
      property timezone: integer write setTZ;
      property errormsg: string read lastErr;
  end;


implementation

constructor TrndiAPI.create(user, pass, extra: string);
begin
  timezone := GetLocalTimeOffset;
  native := TrndiNative.create(ua, baseUrl);
  initCGMCore;
end;


function TrndiAPI.getLevel(v: BGValLevel): single;
begin
  case v of 
    BGRangeHI: result := core.hi;
    BGRangeLO: result := core.lo;
  end;
end;

function TrndiAPI.getLevel(v: single): BGValLevel;
begin
  if v > core.hi then
    result := BGRangeHI
  else if v < core.lo then
         result := BGRangeLO
  else
    result := BGRange;
end;

procedure TrndiAPI.initCGMCore;
begin
  core.hi := 401;
  core.lo := 40;
  core.top := 401;
  core.bottom := 40;
end;

function TrndiAPI.getCGMCore: CGMCore;
begin
  result := core;
end;

function TrndiAPI.encodeStr(src: string): string;

var 
  i: integer;
begin
  result := '';
  for i := 1 to length(src) do
    if not (src[i] in ['A'..'Z', 'a'..'z', '0', '1'..'9', '-', '_', '~', '.']) then
      result := result + '%' + inttohex(Ord(src[i]), 2)
    else
      result := result + src[i];
end;


procedure TrndiAPI.setTZ(secs: integer);
begin
  tz := secs * 60;
end;


function TrndiAPI.getBasetime: int64;
begin
  result := DateTimeToUnix(IncSecond(now, timeDiff));
end;

function TrndiAPI.getLast(var res: BGReading): boolean;

var 
  r: BGResults;
begin
  result := false;
  r      := getReadings(1440, 1);
  if length(r) > 0 then
    begin
      res    := r[0];
      result := true;
    end;
end;

function TrndiAPI.getCurrent(var res: BGReading): boolean;

var 
  r: BGResults;
begin
  result := false;
  r      := getReadings(10, 1);

  if length(r) > 0 then
    begin
      res    := r[0];
      result := true;
    end;
end;


function TrndiAPI.JSToDateTime(ts: int64; correct: boolean = true): TDateTime;
begin
  if correct then
    result := UnixToDateTime((ts div 1000) - tz)
  else
    result := UnixToDateTime((ts div 1000));
end;


end.
