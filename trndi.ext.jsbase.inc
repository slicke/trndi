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


function TrndiConstructor(ctx: JSContext; this_val: JSValueRaw; argc: Integer; argv: PJSValues): JSValueRaw; cdecl;
begin
  // Create a new object of the Trndi class
  Result := JS_NewObjectClass(ctx, TrndiClassID);

  // Initialize any native data here if necessary
  // For example, associate native pointers with the JS object
end;

function JSDoLog(ctx: JSContext; this_val: JSValueRaw; argc: Integer; argv: PJSValues): JSValueRaw; cdecl;
var 
  i: Integer;
  buffer: TSynTempBuffer;
  res: RawUtf8;
begin
  TTrndiExtEngine.Instance.SetOutput(@ctx, argv, argc-1);

  Result := JS_UNDEFINED;
end;

function JSDoAlert(ctx: JSContext; this_val: JSValueRaw; argc: Integer; argv: PJSValues): JSValueRaw; cdecl;
begin
  with TTrndiExtEngine.Instance do
    begin
      SetOutput(ctx, argv, argc-1);
      alert(Output);

  Result := JS_UNDEFINED;
  end;
end;


// fetch()
function JSFetch(ctx: JSContext; this_val: JSValueRaw; argc: Integer; argv: PJSValues): JSValueRaw; cdecl;
var
  URL: string;
  response : RawUtf8;
begin
  if argc > 0 then
    begin
      url := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0);
      TTrndiExtEngine.native.request(false,url,[]);
      Result := JS_NewString(ctx, PAnsiChar(Response));
    end
  else
    Result := JS_UNDEFINED;
end;

// provide try/catch


(* // try {
  throw new Error("Something went wrong");
} catch (error) {
  handleError(error.message);
}  *)
function JSHandleError(ctx: JSContext; this_val: JSValueRaw; argc: Integer; argv: PJSValues): JSValueRaw; cdecl;
var 
  err: string;
begin
  if argc > 0 then
    begin
      err := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0);
      //    ;                                     ctx^^.ErrorDump(true, nil)IsException
      //  TTrndiExtEngine.Instance.excepion(err,);
    end;
  Result := JS_UNDEFINED;
end;

// const data = readFile('test.txt');
function JSReadFile(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues):

                                                                                          JSValueRaw
;
cdecl;

var 
  FileName, FileContent: RawUtf8;
begin
  if argc > 0 then
    begin
      FileName := TTrndiExtEngine.Instance.ParseArgv(ctx, argv, 0);
      FileContent := StringFromFile(UTF8ToString(FileName));
      Result := JS_NewString(ctx^, PAnsiChar(FileContent));
    end
  else
    Result := JS_UNDEFINED;
end;

// console.log(data);
// writeFile('test.txt', 'Hello from JavaScript!');
function JSWriteFile(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues):

                                                                                          JSValueRaw
;
cdecl;

var 
  FileName, FileContent: RawUtf8;
begin
  if argc > 1 then
    begin
      FileName := TTrndiExtEngine.Instance.ParseArgv(ctx, argv, 0);
      FileContent := TTrndiExtEngine.Instance.ParseArgv(ctx, argv, 1);

      if FileFromString(FileContent, UTF8ToString(FileName)) then
        Result := JS_ToBool(ctx^, JS_TRUE)
      else
        Result := JS_ToBool(ctx^, JS_FALSE);
    end
  else
    Result := JS_UNDEFINED;
end;

procedure PromiseRejectionTracker(ctx: PJSContext; promise, reason: JSValueConst; is_handled:
                                  LongBool; opaque: Pointer);
cdecl;

var 
  error_msg: PChar;
begin
  if not is_handled then
    begin
      // Hämta felmeddelandet från reason (anledningen till avvisningen)
      error_msg := JS_ToCString(ctx^, reason);
      try
        // Logga meddelandet om det finns
        if error_msg <> nil then
          TTrndiExtEngine.Instance.alert('Unhandled Promise rejection: ' + error_msg)
        else
          TTrndiExtEngine.Instance.alert('Unhandled Promise rejection: [no error message]');
      finally
        // Frigör C-strängen efter användning
        JS_FreeCString(ctx^, error_msg);
    end;
end;
end;



//function AsyncTask(ctx: JSContext; this_val: JSValueConst; argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
function AsyncTask(ctx: JSContext; this_val: JSValueConst; argc: integer; argv: PJSValueConst; magic
                   : integer; func_data: PJSValueRaw): JSValueRaw;
cdecl;

var 
  func: string;
  cb: PJSCallback;
  params: integer;
  funcs: JSDoubleVal;
  promise: JSValueRaw;
  res: array[0..0] of JSValue;
begin
  func := JSValueConstToUtf8 (ctx, func_data^);
  if func = '' then
    raise Exception.Create('Missing function to run!');

  try
    cb := TTrndiExtEngine.Instance.promise[func];
    if cb^.func = '' then
      raise Exception.Create('Missing function definition!');
  except
    raise Exception.Create('Missing function definition!');
end;


cb^.params.values.data := JSParseParameters(ctx, argc, argv);
// Free senare

params := cb^.params.values.data.Count;



(*   if (params > 0) and (cb^.params.values.data[0].data.match = JD_UNKNOWN) then
     ShowMessage('Inga parametrar'); *)
if (not ((params >= cb^.params.min) and (params <= cb^.params.max))) and (cb^.params.min > -1) then
  //Use minus for feely varying
  if cb^.params.min = cb^.params.max then
    ShowMessage(Format('[%s] Needs %d pameters, but we got %d', [func, cb^.params.min, params]))
else
  ShowMessage(Format('[%s] Needs %d - %d pameters, but we got %d', [func, cb^.params.min, cb^.params
              .max, params]));

// Setup promise
try
  Promise := JS_NewPromiseCapability(ctx, @funcs[0]);
except
  on E: Exception do
        begin
          ShowMessage('Init failed');
          Exit;
        end;
end;

if JS_IsError(ctx, promise) then
  begin
    //    JS_Free(ctx, @taskinfo.funcs[0]);
    // Hantera fel
  end;


TJSAsyncTask.Create(ctx, cb, funcs);

result := promise;

end;
