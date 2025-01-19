
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
Classes, SysUtils, trndi.ext.functions, trndi.types, slicke.ux.alert, trndi.api, trndi.ext.engine,
Trndi.Native,
dialogs, math;

type 
  TJSFuncs = class(TObject)
    public 
      function asyncget(ctx: pointer; const func: string; const params: JSParameters; out res:
                        JSValueVal): boolean;
      function bgDump(ctx: pointer; const func: string; const params: JSParameters; out res:
                      JSValueVal): boolean;
      function setLimits(ctx: pointer; const func: string; const params: JSParameters; out res:
                         JSValueVal): boolean;
      function querySvc(ctx: pointer; const func: string; const params: JSParameters; out res:
                        JSValueVal): boolean;

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
      AddPromise('querySvc', JSCallbackFunction(@querySvc));
      AddPromise('setLimits', JSCallbackFunction(@setLimits), 2, 5);
      AddPromise('setLevelColor', JSCallbackFunction(@setLimits), 3, 6);
    end;
end;

procedure TJSFuncs.ShowMsg(const str: string);
begin
  ExtLog('Message from Extension','An extension triggered a message', str, WideChar($274F));
end;

// Blood Glucose dump, from JS
function TJSFuncs.bgDump(ctx: pointer; const func: string; const params: JSParameters; out res:
                         JSValueVal): boolean;

var 
  i: integer;
  r: BGResults;
begin

  if params[1]^.data.match <> JD_INT then
    begin
      ShowMsg('Unknown paramter #1');
      Exit(false);
    end;
  if params[2]^.data.match <> JD_INT then
    begin
      ShowMsg('Unknown paramter #2');
      Exit(false);
    end;
  tapi.getReadings(params[0]^.data.Int32Val, params[1]^.data.Int32Val);
  //@fixme not done
  result := true;
end;

// backend for asyncGet in JS
// Fetches a file from the web
function TJSFuncs.asyncget(ctx: pointer; const func: string; const params: JSParameters; out res:
                           JSValueVal): boolean;

var 
  s,r: string;
  v: JSValueVal;
begin
  v := params[0]^;
  if not v.mustbe(JD_STR, func, 0) then
    begin
      result := false;
      r := 'Wrong data type for URL';
      v := StringToValueVal(r);
      Exit(false);
    end;

  if not TrndiNative.getURL(v.data.StrVal, s) then
    begin
      result := false;
      r := 'Cannot fetch URL ' + v.data.strval;
    end
  else
    begin
      r := s;
      result := true;
    end;
  v := StringToValueVal(r);
  res := v;
end;

// Set high/low values
function TJSFuncs.setLimits(ctx: pointer; const func: string; const params: JSParameters; out res:
                            JSValueVal): boolean;

var
  v: JSValueVal; // Return data
  times: integer; // Unit multiplier
  f: boolean;
begin
  // Values has to be int and might have a bool
  if checkJSParams(params, [JD_INT, JD_INT, JD_INT, JD_INT], [JD_INT, JD_INT]) = JS_PARAM_OK then begin
      times := 1;
      f := false;
  end
  else if checkJSParams(params, [JD_F64, JD_F64, JD_F64, JD_F64], [JD_F64, JD_F64]) = JS_PARAM_OK then begin
      times := 18;
      f := true;
  end
  else begin
      result := false;
      res.data.Int32Val := -1;
      Exit(false);
    end;

    tapi.cgmLo := round(params[0]^.floatify * times);
    tapi.cgmHi := round(params[1]^.floatify * times);
    if params.Count = 4 then begin
      tapi.cgmRangeLo := round(params[2]^.floatify * times);
      tapi.cgmRangeHi := round(params[3]^.floatify * times);
    end;


  v := IntToValueVal(tapi.cgmHi);
  res := v;
  result := true;
end;

// Query the backend via JS
function TJSFuncs.querySvc(ctx: pointer; const func: string; const params: JSParameters; out res:
                           JSValueVal): boolean;
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

end.
