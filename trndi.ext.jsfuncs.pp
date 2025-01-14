(*
(c) 2024-2025 github.com/slicke - See LICENSE file, GPLv3, Written with the aid of GPT
*)
// Handle JS functions

unit trndi.ext.jsfuncs;

{$mode ObjFPC}{$H+}

interface

uses 
Classes, SysUtils, trndi.ext.functions, trndi.types, slicke.ux.alert, trndi.api, trndi.ext.engine,
Trndi.Native,
dialogs;

type 
  TJSFuncs = class(TObject)
    public 
      function asyncget(ctx: pointer; const func: string; constref params: JSParameters; out res:
                        JSValueVal): boolean;
      function bgDump(ctx: pointer; const func: string; constref params: JSParameters; out res:
                      JSValueVal): boolean;
      function querySvc(ctx: pointer; const func: string; constref params: JSParameters; out res:
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
    end;
end;

procedure TJSFuncs.ShowMsg(const str: string);
begin
  ExtLog('Message from Extension','An extension triggered a message', str, WideChar($274F));
end;

// Blood Glucose dump, from JS
function TJSFuncs.bgDump(ctx: pointer; const func: string; constref params: JSParameters; out res:
                         JSValueVal): boolean;

var 
  i: integer;
  r: BGResults;
begin

  if params[1].data.match <> JD_INT then
    begin
      ShowMsg('Unknown paramter #1');
      Exit(false);
    end;
  if params[2].data.match <> JD_INT then
    begin
      ShowMsg('Unknown paramter #2');
      Exit(false);
    end;
  tapi.getReadings(params[0].data.Int32Val, params[1].data.Int32Val);
  //@fixme not done
  result := true;
end;

// backend for asyncGet in JS
// Fetches a file from the web
function TJSFuncs.asyncget(ctx: pointer; const func: string; constref params: JSParameters; out res:
                           JSValueVal): boolean;

var 
  s,r: string;
  v: JSValueVal;
begin
  v := params[0];
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

// Query the backend via JS
function TJSFuncs.querySvc(ctx: pointer; const func: string; constref params: JSParameters; out res:
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
