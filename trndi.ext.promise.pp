
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
// JS promise handler

unit trndi.ext.promise;

{$mode ObjFPC}{$H+}

interface

uses 
Classes, SysUtils, mORMot.lib.quickjs, mormot.core.base, dialogs, trndi.ext.functions, slicke.ux.
alert, fgl;

type 
  JSDoubleVal = array[0..1] of JSValueRaw;
  // Responses to JS (ok or err)
  PJSDoubleVal = ^JSValueRaw;

  TJSAsyncTask = class(TThread)
    private 
      FContext: JSContext;
      funcs: JSDoubleVal;
      FPromise: PJSCallback ;
      FResult: JSVAlueVal;
      FSuccess: boolean;
    protected 
      procedure Execute;
      override;
      procedure ProcessResult;
    public 
      constructor Create(Context: JSContext; func: PJSCallback; cbfunc: PJSDoubleVal);
  end;

const
  JprResolve = 0;
  // const for "async function worked"
  JprReject = 1;
  // for "didnt work"

implementation
resourcestring
sTypeErrCapt = 'A data type differes from what was expected';
sTypeErrDesc = 'The extension was stopped';
sTypeErrmsg = 'A data type was expected, but another was found';
sPromErrCapt = 'The asyncronous function %s failed to complete';


constructor TJSAsyncTask.Create(Context: JSContext; func: PJSCallback; cbfunc: PJSDoubleVal);
begin
  FContext := Context;
  FPromise := func;
  funcs := cbfunc;
  FreeOnTerminate := True;
  inherited Create(False);
  // This will start the thread
end;

procedure TJSAsyncTask.Execute;

var 
  xres: JSValue;
begin

  if Assigned(FPromise^.callback) then // Run the main function, if it's actually set
    Synchronize(@ProcessResult)
  else
    begin
      // Complain that it wasn't set
      ExtError('Error: Missing Function');
      FSuccess := false;
      Exit;
    end;

  xres := JSValueValToValue(FContext, FResult);
  // Convert our result to something we can retur to JS

  if FSuccess then
    JS_Call(FContext, funcs[JprResolve], JS_UNDEFINED, 1, @xres)
  else
    JS_Call(FContext, funcs[JprReject], JS_UNDEFINED, 1, @xres);

  //    FContext^.Free(JSValue(Promise)); -- should free automatically
  //    JS_Free(FContext, @Promise);
  FPromise^.params.values.data.Free;
  self.Terminate;
end;

// Run the set function for the JS func
procedure TJSAsyncTask.ProcessResult;
begin
  with FPromise^ do
    begin
      if Assigned(Callback) then
        begin
          // This should already be checked, but let's be safe
          try
            FSuccess := FPromise^.Callback(@FContext, func, params.values.data, FResult);
            // Run the callback that was set when the function was defined
          except
            on E: EInvalidCast do
                  begin
                    FSuccess := false;
                    ExtError(sTypeErrMsg, e.message);
                  end;
            on E: Exception do
                  begin
                    fsuccess := false;
                    ExtError(Format(sPromErrCapt, [func]),e.Message);
                  end;
        end;
    end
    else fsuccess := false;
end;
end;

end.
