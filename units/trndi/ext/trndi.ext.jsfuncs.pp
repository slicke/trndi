
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
// Handle JS functions

unit trndi.ext.jsfuncs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.ext.functions, trndi.types, slicke.ux.alert,
  trndi.api, trndi.ext.engine,
  Trndi.Native, fpjson,
  Dialogs, Math;

type
  TJSFuncs = class(TObject)
  public
    function asyncget(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function jsonget(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function bgDump(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function setLimits(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function querySvc(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function runCMD(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;

    constructor Create(cgm: TrndiAPI);

  private
    tapi: TrndiAPI;
    procedure ShowMsg(const str: string);
  end;

implementation

// Create this function JS handler
constructor TJSFuncs.Create(cgm: TrndiAPI);
begin
  tapi := cgm;

  with TTrndiExtEngine.Instance do
  begin
    // Register the functions in JS
    AddPromise('bgDump', JSCallbackFunction(@bgDump));
    AddPromise('asyncGet', JSCallbackFunction(@asyncGet));
    AddPromise('jsonGet', JSCallbackFunction(@jsonGet), 2);
    AddPromise('runCMD', JSCallbackFunction(@asyncGet));
    AddPromise('querySvc', JSCallbackFunction(@querySvc));
    AddPromise('setLimits', JSCallbackFunction(@setLimits), 2, 5);
    AddPromise('setLevelColor', JSCallbackFunction(@setLimits), 3, 6);
  end;
end;

procedure TJSFuncs.ShowMsg(const str: string);
begin
  ExtLog(uxdAuto, 'Message from Extension', 'An extension triggered a message',
    str, uxmtSquare);
end;

// Blood Glucose dump, from JS
function TJSFuncs.bgDump(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
var
  i: integer;
  r: BGResults;
begin

  if params[1]^.Data.match <> JD_INT then
  begin
    ShowMsg('Unknown paramter #1');
    Exit(False);
  end;
  if params[2]^.Data.match <> JD_INT then
  begin
    ShowMsg('Unknown paramter #2');
    Exit(False);
  end;
  tapi.getReadings(params[0]^.Data.Int32Val, params[1]^.Data.Int32Val);
  //@fixme not done
  Result := True;
end;

// backend for asyncGet in JS
// Fetches a file from the web
function TJSFuncs.asyncget(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
var
  s, r: string;
  v: JSValueVal;
begin
  v := params[0]^;
  if not v.mustbe(JD_STR, func, 0) then
  begin
    Result := False;
    r := 'Wrong data type for URL';
    v := StringToValueVal(r);
    Exit(False);
  end;

  if not TrndiNative.getURL(v.Data.StrVal, s) then
  begin
    Result := False;
    r := 'Cannot fetch URL ' + v.Data.strval;
  end
  else
  begin
    r := s;
    Result := True;
  end;
  v := StringToValueVal(r);
  res := v;
end;

function TJSFuncs.jsonget(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
var
  s, r: string;
  v, v2: JSValueVal;
  jsonData, jval: TJSONData;
begin
  v := params[0]^;
  v2 := params[1]^;

  // URL must be a string (param 0) and path must be a string (param 1)
  if not v.mustbe(JD_STR, func, 0) then
  begin
    Result := False;
    r := 'Wrong data type for URL';
    v := StringToValueVal(r);
    res := v;
    Exit(False);
  end;
  if not v2.mustbe(JD_STR, func, 1) then
  begin
    Result := False;
    r := 'Wrong data type for JSON path';
    v := StringToValueVal(r);
    res := v;
    Exit(False);
  end;

  if not TrndiNative.getURL(v.Data.StrVal, s) then
  begin
    Result := False;
    r := 'Cannot fetch URL ' + v.Data.StrVal;
    v := StringToValueVal(r);
    res := v;
    Exit(False);
  end;

  // Try to parse JSON and find the path. Uses fpjson.GetJSON and FindPath
  jsonData := nil;
  try
    jsonData := GetJSON(s);
    jval := jsonData.FindPath(v2.Data.StrVal); // path like "a.b[0].c"
    if jval = nil then
    begin
      Result := False;
      r := 'JSON path not found: ' + v2.Data.StrVal;
    end
    else
    begin
      // Prefer native string for JSON strings, otherwise return JSON text for objects/numbers/arrays
      if jval.JSONType = jtString then
        r := jval.AsString
      else
        r := jval.AsJSON;
      Result := True;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      r := 'JSON parse error: ' + E.Message;
    end;
  end;
  if Assigned(jsonData) then
    jsonData.Free;

  v := StringToValueVal(r);
  res := v;
end;

// Set high/low values
function TJSFuncs.setLimits(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
var
  v: JSValueVal; // Return data
  times: single; // Unit multiplier
  f: boolean;
begin
  // Values has to be int and might have a bool
  if checkJSParams(params, [JD_INT, JD_INT, JD_INT, JD_INT], [JD_INT, JD_INT]) =
    JS_PARAM_OK then
  begin
    times := 1;
    f := False;
  end
  else
  if checkJSParams(params, [JD_F64, JD_F64, JD_F64, JD_F64], [JD_F64, JD_F64]) =
    JS_PARAM_OK then
  begin
    times := 18.0182;
    f := True;
  end
  else
  begin
    Result := False;
    res.Data.Int32Val := -1;
    Exit(False);
  end;

  tapi.cgmLo := round(params[0]^.floatify * times);
  tapi.cgmHi := round(params[1]^.floatify * times);
  if params.Count = 4 then
  begin
    tapi.cgmRangeLo := round(params[2]^.floatify * times);
    tapi.cgmRangeHi := round(params[3]^.floatify * times);
  end;


  v := IntToValueVal(tapi.cgmHi);
  res := v;
  Result := True;
end;

// Query the backend via JS
function TJSFuncs.querySvc(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
const
  QUERY = 0;
var
  sd: string;
  v: JSValueVal;
begin
  //params[QUERY].mustbe(JD_STR, func);
  //params^[QUERY].data.StrVal;

  //params^[PROPS].mustbe(JD_ARRAY, func);

  //res := params[1][2][0];

end;

// Query the backend via JS
function TJSFuncs.runCMD(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
begin
  if not checkJSParams(params, [JD_STR], [JD_STR, JD_STR, JD_STR]) = JS_PARAM_OK then
  begin
    Result := False;
    res.Data.Int32Val := -1;
    Exit(False);
  end;

  if params.Count = 2 then
    res.Data.Int32Val := ExecuteProcess(params[0]^.stringify, [])
  else
    res.Data.Int32Val := ExecuteProcess(params[0]^.stringify,
      params[1]^.stringify.Split(params[2]^.stringify));

  Result := True;
end;

end.
