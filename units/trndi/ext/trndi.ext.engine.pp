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
unit trndi.ext.engine;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.test,
  mormot.lib.quickjs,
  dialogs,
  classes,
  trndi.native,
  trndi.ext.promise,
  trndi.ext.functions,
  fgl,
  ExtCtrls,
  fpTimer,
  forms,
  controls,
  Graphics,
  math,
  StdCtrls,
  slicke.ux.alert,
  trndi.strings,

  fpimage, IntfGraphics, GraphType, EasyLazFreeType, LazFreeTypeIntfDrawer;

type
  {** Callback signature invoked when JavaScript code emits output via this engine. }
  TOutputCallback = procedure (const Msg: RawUtf8) of object;

  {** Alias to the JS function type used by QuickJS bindings. }
  ExtFunction = JSFunction;

  {** Placeholder alias for argv-like arrays. @fixme Consider removing if unused. }
  ExtArgv = array of ExtFunction;

  {** Convenience alias for passing string arguments to JS calls. }
  JSArray = array of RawUtf8;

  {** 64-bit integer array helper. }
  QWordArray = array of qword;

  {** Descriptor for a registered extension function. Compares by @code(name). }
  TTrndiExtFunc = record
  private
    args: ExtArgv;
    function getArgCount: integer;
  public
    {** Function name (identifier exposed to JS). }
    name: RawUtf8;
    {** Number of arguments registered for this function. }
    property argc: integer read getArgCount;
    {** Synonym of @code(argc). }
    property count: integer read getArgCount;

    {** Equality operator compares on @code(name) only. }
    class operator = (const a, b: TTrndiExtFunc): boolean; overload;
  end;

  {** List of known/registered extension functions. }
  TExtFuncList = specialize TFPGList<TTrndiExtFunc>;

  {** List of registered callbacks (e.g., for Promises and async tasks). }
  TCallbacks = specialize TFPGList<PJSCallback>;
  {** Alias: promises are managed as callbacks. }
  TPromises = TCallbacks;

  {** Embedded JavaScript engine wrapper using QuickJS via mORMot bindings.

      Responsibilities:
      - Create and own a JS runtime/context
      - Register Pascal functions and classes to JS global scope
      - Execute JS strings and files
      - Manage Promises/jobs via a periodic timer
      - Provide convenience helpers for globals, calls, and UI alerts

      Pattern:
      - Implemented as a singleton via @code(Instance)/@code(ReleaseInstance).
   }
  TTrndiExtEngine = class
  private
  class
    var
      {** Singleton instance. Use @code(Instance) to access. }
      FInstance: TTrndiExtEngine;

  private
    {** Definition for the 'Trndi' class exposed to JS. }
    TrndiClass: JSClassDef;
    {** Registry of known/added functions (optional bookkeeping). }
    knownfunc: TExtFuncList;
    {** Periodic timer to pump QuickJS job queue (Promises/microtasks). }
    eventTimer: TFPTimer;
    {** QuickJS runtime handle. }
    FRuntime: JSRuntime;
    {** QuickJS context handle. }
    FContext: JSContext;
    {** Accumulated output produced by JS. }
    FOutput: RawUtf8;
    {** Output sink callback invoked by @code(SetOutput). }
    OutCallback: TOutputCallback;
    {** Native helper for OS/HTTP glue (Trndi platform integration). }
    native: TrndiNative;
    {** Registered promises. }
    promises: TPromises;

    function GetOutput: RawUtf8;
    procedure SetOutput(const val: RawUtf8);

    {** Show a UX dialog and return the button pressed.

        @param(dialogType Message dialog type)
        @param(msg        Dialog message text)
        @param(titleadd   Extra title suffix/prefix)
        @returns(Integer code based on selected button) }
    function uxResponse(const dialogType: TMsgDlgType; const msg: string; const titleadd: string): integer;

    {** Find a registered callback by name.

        @param(func Callback identifier)
        @returns(TJSCallback record; undefined if not found) }
    function findCallback(const func: string): TJSCallback;

    {** Find a registered promise callback by name.

        @param(func Promise/callback identifier)
        @returns(Pointer to callback record; may raise if not found) }
    function findPromise(const func: string): PJSCallback;

  public
    {** All registered callbacks (promise handlers etc.). }
    callbacks: TCallbacks;

    {** Construct the JS engine: runtime, context, 'Trndi' class, timers, base functions. }
    constructor Create;
    {** Destroy and cleanup: timers, callbacks, context/runtime, singleton instance. }
    destructor Destroy; override;

    {** Execute a JavaScript source string in the global scope.

        @param(Script JavaScript source as UTF-8)
        @param(name   Optional script name for diagnostics; default '<script>')
        @returns(Output or result string; on error returns formatted error text) }
    function Execute(const Script: RawUtf8; name: string = '<script>'): RawUtf8;

    {** Load a JS file from disk and execute its contents.

        @param(FileName Path to script file)
        @returns(Result string from @code(Execute))
        @raises(Exception if file not found) }
    function ExecuteFile(const FileName: string): RawUtf8;

    {** Get the singleton instance of the engine (creates on first use). }
    class function Instance: TTrndiExtEngine;
    {** Release the singleton instance (frees resources). }
    class procedure ReleaseInstance;

    {** Append values to @code(Output) by converting JS values to UTF-8.

        @param(ctx  JS context)
        @param(vals Pointer to first JS value)
        @param(len  Number of items-1 to read; iterates inclusive from 0..len) }
    procedure SetOutput(ctx: JSContext; const vals: PJSValues; const len: integer);

    {** Clear the accumulated @code(Output). }
    procedure ClearOutput;

    {** Accessor to accumulated output; writes also trigger @code(OutCallback). }
    property Output: RawUtf8 read GetOutput write SetOutput;

    {** Register a global JS function in the current context.

        @param(id    Function name in global scope)
        @param(func  Native function pointer)
        @param(argc  Declared arity; 0 means variable/unspecified) }
    procedure addFunction(const id: string; const func: JSFunction; const argc: integer = 0);

    {** Register a function as a method of the 'Trndi' JS class.

        @param(id    Method name)
        @param(func  Native function pointer)
        @param(argc  Declared arity; use -1 for variadic) }
    procedure addClassFunction(const id: string; const func: JSFunction; const argc: integer = 0);

    {** Call a global JS function by name with string arguments.

        @param(FuncName Global function name)
        @param(Args     Array of string arguments)
        @returns(Stringified result; empty string if call failed/not found) }
  function CallFunction(const FuncName: RawUtf8; const Args: JSArray): RawUtf8; overload;

   {** Call a global JS function by name with integer arguments.

        @param(FuncName Global function name)
        @param(Args     Array of integer arguments)
        @returns(Stringified result; empty string if call failed/not found) }
  function CallFunction(const FuncName: RawUtf8; const Args: array of integer): RawUtf8; overload;

  {** Call a global JS function by name with different arguments.

      @param(FuncName Global function name)
      @param(Args     Array of const arguments)
      @returns(Stringified result; empty string if call failed/not found) }
  function CallFunction(const FuncName: RawUtf8; const Args: array of const): RawUtf8; overload;

    {** Set a global JS variable (string).

        @param(VarName Variable name)
        @param(Value   String value)
        @param(obj     Unused placeholder; reserved) }
    procedure SetGlobalVariable(const VarName: RawUtf8; const Value: RawUtf8; const obj: string = '');

    {** Set a global JS variable (int64).

        @param(VarName Variable name)
        @param(Value   64-bit integer value)
        @param(obj     Unused placeholder; reserved) }
    procedure SetGlobalVariable(const VarName: RawUtf8; const Value: int64; const obj: string = '');

    {** Create an empty global JS object with the given name.

        @param(name Global identifier for the new object) }
    procedure CreateNewObject(const name: string);

    {** Register a Promise-style async entry point with fixed arity.

        @param(funcName  Name of the JS function to expose)
        @param(cbfunc    Pascal callback to run when job executes)
        @param(params    Exact number of parameters expected) }
    procedure AddPromise(const funcName: string; cbfunc: JSCallbackFunction; params: integer = 1);

    {** Register a Promise-style async entry point with min/max arity.

        @param(funcName  Name of the JS function to expose)
        @param(cbfunc    Pascal callback to run when job executes)
        @param(minParams Minimum number of parameters allowed)
        @param(maxParams Maximum number of parameters allowed) }
    procedure AddPromise(const funcName: string; cbfunc: JSCallbackFunction; minParams, maxParams: integer);

    {** Raise an EJSException carrying a message and file name.

        @param(message Error message)
        @param(fn      Source file or logical script name) }
    procedure excepion(const message, fn: string);

    {** Convert an argument at @code(pos) from JS to a pascal @code(PChar).

        Note: Implementation relies on a trick to coerce RawUtf8 to PChar and
        should be used with care regarding lifetime.

        @param(ctx JS context)
        @param(vals Pointer to argument array)
        @param(pos  Index of argument to convert)
        @returns(PChar pointing to temporary-encoded UTF-8 buffer) }
    class function ParseArgv(ctx: PJSContext; const vals: PJSValues; const pos: integer): pchar;

    {** Show an informational alert dialog to the user. }
    procedure alert(const msg: string);

    {** Access a registered callback by name. }
    property callback [f: string]: TJSCallback read findCallback;
    {** Access a registered promise callback pointer by name. }
    property promise  [f: string]: PJSCallback read findPromise;

    {** Timer callback to process pending JS jobs (Promises/microtasks). }
    procedure OnJSTimer(Sender: TObject);

    {** Check whether a global function with @code(FuncName) exists in JS.

        @param(FuncName Global function identifier)
        @returns(True if a callable object is found) }
    function FunctionExists(const FuncName: string): boolean;
  end;

  {** Exception type for JS-related errors with filename context. }
  EJSException = class(Exception)
  private
    FFilename: string;
  public
    {** Create exception with a message and file name context. }
    constructor CreateWithName(const msg: string; const AFileName: string);
    {** Stringify exception as 'ClassName: FileName<newline>Message'. }
    function ToString : string; override;
    {** Name of the source file or logical script name. }
    property Filename: string read FFilename write FFilename;
  end;

var
  {** Class ID used for the 'Trndi' class in QuickJS context. }
  TrndiClassID: JSClassID;

implementation

{$I trndi.ext.jsbase.inc }

{******************************************************************************
  TTrndiExtFunc
******************************************************************************}

{** Compare two function descriptors by @code(name) only. }
class operator TTrndiExtFunc. = (const a, b: TTrndiExtFunc): boolean; overload;
begin
  result := a.name = b.name
end;

function TTrndiExtFunc.getArgCount: integer;
begin
  result := length(args);
end;

{******************************************************************************
  EJSException
******************************************************************************}

{** Construct with message and file name. }
constructor EJSException.CreateWithName(const msg: string; const AFileName: string);
begin
  inherited Create(msg);
  FFilename := AFilename;
end;

{** Return 'ClassName: FileName' + newline + message. }
function EJSException.ToString: string;
begin
  Result := ClassName + ': ' + FFilename + LineEnding + Message;
end;

{******************************************************************************
  TTrndiExtEngine: dialogs, errors, alerts
******************************************************************************}

{** Show a simple informational alert. }
procedure TTrndiExtEngine.alert(const msg: string);
begin
  uxResponse(mtInformation, msg, sExtUserInfo);
end;

{** Show a UX dialog and return the button pressed. }
function TTrndiExtEngine.uxResponse(const dialogType: TMsgDlgType; const msg: string; const titleadd: string): integer;
var
  btns: TUXMsgDlgBtns;
  header, title: string;
begin
  title := titleadd;

  case dialogType of
    mtWarning:
      begin
        btns := [mbOK];
        title := Format('[%s] %s', [sExtWarn,title]);
        header := sExtWarn;
      end;
    mtError:
      begin
        btns := [mbAbort];
        title := Format('[%s] %s', [sExtErr, title]);
        header := sExtErr;
      end;
    mtInformation:
      begin
        btns := [mbOK];
        title := Format('[%s] %s', [sExtMsg, title]);
        header := sExtMsg;
      end;
    mtConfirmation:
      begin
        btns := [mbYes, mbNo];
        title := Format('[%s] %s', [sextConfirm, title]);
        header := sExtConfirm;
      end;
  else
      begin
        btns := [mbOK];
        title := Format('[%s] %s', [sExtEvent, title]);
        header := sExtEvent;
      end;
  end;

  result := UXDialog(uxdAuto, header, title, msg, btns, dialogType);
end;

{** Raise JS exception with filename context. }
procedure TTrndiExtEngine.excepion(const message, fn: string);
begin
  raise EJSException.CreateWithName(message,fn);
end;

{******************************************************************************
  TTrndiExtEngine: callback/promise lookup and registration
******************************************************************************}

{** Find a registered callback by name. Returns default value if not found. }
function TTrndiExtEngine.findCallback(const func: string): TJSCallback;
var
  i: integer;
  ok: boolean;
begin
  ok := false;
  for i := 0 to callbacks.Count-1 do
    if callbacks[i]^.func = func then
    begin
      ok := true;
      break;
    end;

  if ok then
    result := callbacks[i]^;
end;

{** Find a registered promise by name or alert if missing.

    Note: The alert dereferences @code(promises[i]) after the loop if not found,
    which could be invalid; consider refactoring to avoid deref when not found. }
function TTrndiExtEngine.findPromise(const func: string): PJSCallback;
var
  i: integer;
begin
  for i := 0 to promises.Count-1 do
    if (promises[i] <> nil) and (promises[i]^.func = func) then
      Exit(promises[i]);

  // Not found: inform the user; avoid dereferencing promises[i] here in production
  TTrndiExtEngine.instance.alert('Required function not found: ' + func);
end;

{** Add Promise helper with fixed @code(params) expected (min=max). }
procedure TTrndiExtEngine.AddPromise(const funcName: string; cbfunc: JSCallbackFunction; params: integer = 1);
begin
  AddPromise(funcName, cbfunc, params, params);
end;

{** Register a Promise entry point by exposing an async task wrapper in JS
    and storing the associated Pascal callback with arity constraints. }
procedure TTrndiExtEngine.AddPromise(const funcName: string; cbfunc: JSCallbackFunction; minParams, maxParams: integer);
var
  data: JSValueConst;
  cb: PJSCallback;
begin
  // Expose an async task function in global scope that will route to our callback
  data := JS_NewString(FContext, pansichar(funcname));
  JS_SetPropertyStr(
    FContext,
    JS_GetGlobalObject(FContext),
    pchar(funcname),
    JS_NewCFunctionData(FContext, PJSCFunctionData(@AsyncTask), 1, 0, 1, @data)
  );

  // Track the Pascal callback and expected parameter range
  New(cb);
  cb^.func := funcname;
  cb^.callback := cbfunc;
  cb^.params.min := minParams;
  cb^.params.max := maxParams;

  promises.Add(cb);
end;

{******************************************************************************
  TTrndiExtEngine: output helpers
******************************************************************************}

{** Return a @code(PChar) for argument @code(pos). Use with care re: lifetime. }
class function TTrndiExtEngine.ParseArgv(ctx: PJSContext; const vals: PJSValues; const pos: integer): pchar;
begin
  // Hack to coerce RawUtf8 to PChar. Ensure caller uses the pointer immediately.
  result := pchar(result + ctx^^.ToUtf8(vals^[pos]));
end;

{** Append JS values (0..len) to @code(Output) as UTF-8 strings. }
procedure TTrndiExtEngine.SetOutput(ctx: JSContext; const vals: PJSValues; const len: integer);
var
  i: integer;
begin
  for i := 0 to len do
    output := output + ctx^.ToUtf8(vals^[i]);
end;

{** Clear accumulated output buffer. }
procedure TTrndiExtEngine.ClearOutput();
begin
  output := '';
end;

{** Assign output and notify optional callback. }
procedure TTrndiExtEngine.SetOutput(const val: RawUtf8);
begin
  FOutput := val;
  if assigned(OutCallback) then
    OutCallback(val);
end;

{** Read accumulated output. }
function TTrndiExtEngine.GetOutput: RawUtf8;
begin
  result := FOutput;
end;

{******************************************************************************
  Module loader
******************************************************************************}

{** Module loader for QuickJS: reads module source from disk.

    @returns(Allocated C-string with module source, or nil on error) }
function TrndiModuleLoader(ctx: JSContext; module_name: PAnsiChar; opaque: pointer): PAnsiChar; cdecl;
var
  FileName: string;
  Script: RawUtf8;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  // Map module_name to a file path; improve mapping as appropriate
  FileName := string(module_name);

  if not FileExists(FileName) then
  begin
    Result := nil; // signal load error to QuickJS
    Exit;
  end;

  // Load file contents into Script as UTF-8
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  StringStream := TStringStream.Create;
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    Script := StringStream.DataString;
  finally
    StringStream.Free;
    FileStream.Free;
  end;

  // Return C-allocated buffer (QuickJS expects module source ownership)
  Result := StrNew(PAnsiChar(Script));
end;

{******************************************************************************
  Construction / destruction
******************************************************************************}

{** Create QuickJS runtime/context, expose 'Trndi', set up promises and timer, register base functions. }
constructor TTrndiExtEngine.Create;
var
  proto: JSValueRaw;
begin
  inherited Create;

  // Create the QuickJS runtime
  FRuntime := JS_NewRuntime;
  if FRuntime = nil then
    raise Exception.Create('Failed to create JS runtime');

  // Create the context
  FContext := JS_NewContext(FRuntime);
  if FContext = nil then
    raise Exception.Create('Failed to create JS context');

  // Enable module loading via our loader
  JS_SetModuleLoaderFunc(FRuntime, nil, PJSModuleLoaderFunc(@TrndiModuleLoader), nil);

  // Allow callbacks to find this engine from JS context
  JS_SetContextOpaque(FContext, Self);

  // Define 'Trndi' JS class
  TrndiClass := Default(JSClassDef);
  TrndiClass.class_name := 'Trndi';

  // Create class ID and register class on runtime
  JS_NewClassID(@TrndiClassId);
  if JS_NewClass(FRuntime, TrndiClassID, @TrndiClass) < 0 then
    raise Exception.Create('Failed to create JS class');

  // Acquire prototype (unused in this snippet but kept for completeness)
  Proto := JS_GetClassProto(FContext, TrndiClassID);

  // Store opaque back-reference (note: typically used with instances, not class ID)
  JS_SetOpaque(TrndiClassId, Pointer(self));

  // Expose 'Trndi' constructor in global scope
  JS_SetPropertyStr(
    FContext,
    JS_GetGlobalObject(FContext),
    'Trndi',
    JS_NewCFunction2(FContext, PJSCFunction(@TrndiConstructor), 'Trndi', 0, JS_CFUNC_constructor, 0)
  );

  // Add essential intrinsics
  JS_AddIntrinsicPromise(FContext);
  JS_AddIntrinsicRegExp(FContext);
  JS_AddIntrinsicDate(FContext);
  JS_SetHostPromiseRejectionTracker(FRuntime, PJSHostPromiseRejectionTracker(@PromiseRejectionTracker), nil);

  // Initialize timer to pump pending JS jobs (Promises/microtasks)
  eventTimer := TFPTimer.Create(nil);
  eventTimer.Interval := 50;       // run every 50ms
  eventTimer.OnTimer := @self.OnJSTimer;
  eventTimer.Enabled := true;

  // Collections
  promises  := TPromises.Create;
  knownfunc := TExtFuncList.Create;
  native    := TrndiNative.create;
  callbacks := TCallbacks.Create;

  // Register base UI/log functions as both class and globals
  addClassFunction('alert',   @JSDoAlert, 1);
  addClassFunction('confirm', @JSDoYesNo, 1);
  addClassFunction('prompt',  @JSInput,   4);
  addClassFunction('select',  @JSCombo,  -1);
  addClassFunction('log',     ExtFunction(@JSDoLog), 1);

  addFunction('alert', ExtFunction(@JSDoAlert), 1);

  // Provide a neutral console.log to avoid reference errors in scripts
  RegisterConsoleLog(@FContext);
end;

{** Free context/resources, dispose registered callbacks, stop timer, and clear singleton. }
destructor TTrndiExtEngine.Destroy;
var
  cb: PJSCallback;
begin
  try
    if FContext <> nil then
      FContext^.Done;
  except
    on E: Exception do
      ExtError(uxdAuto, 'An error occured while shutting down extensions: ' + E.Message);
  end;

  if FRuntime <> nil then
  try
    // Managed by mORMot GC; explicit free may be unnecessary or harmful here.
    // JS_FreeRuntime(@FRuntime);
    // FRuntime^.DoneSafe;
  except
    on E: Exception do
      ExtError(uxdAuto, 'An error occured while shutting down extensions: ' + E.Message);
  end;

  eventTimer.free;

  // Dispose dynamically allocated callbacks
  for cb in promises do
    Dispose(cb);
  promises.Free;

  for cb in callbacks do
    Dispose(cb);
  callbacks.Free;

  FreeAndNil(FInstance);
  inherited Destroy;
end;

{******************************************************************************
  Registration helpers and globals
******************************************************************************}

{** Add a global JS function. }
procedure TTrndiExtEngine.addFunction(const id: string; const func: JSFunction; const argc: integer = 0);
begin
  FContext^.SetFunction([], pchar(id), func, argc);
end;

{** Add a JS method under the 'Trndi' class. }
procedure TTrndiExtEngine.addClassFunction(const id: string; const func: JSFunction; const argc: integer = 0);
var
  this: JSValue;
begin
  if not FContext^.GetValue('Trndi', this) then
  begin
    ExtError(uxdAuto, 'Cannot locate the Trndi class while initializing extensions');
    Exit;
  end;

  FContext^.SetFunction(this, pchar(id), func, argc);
end;

{** Create an empty global object named @code(name). }
procedure TTrndiExtEngine.CreateNewObject(const name: string);
var
  GlobalObj, JSObject: JSValueRaw;
begin
  GlobalObj := JS_GetGlobalObject(FContext);
  JSObject  := JS_NewObject(FContext);
  JS_SetPropertyStr(FContext, GlobalObj, pansichar(name), JSObject);
  // Note: GlobalObj/JSObject lifetime managed by QuickJS; avoid freeing raw values improperly
end;

{** Set a global JS string variable. }
procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8; const Value: RawUtf8; const obj: string = '');
var
  JValue, GlobalObj: JSValueRaw;
begin
  GlobalObj := JS_GetGlobalObject(FContext);
  JValue    := JS_NewString(FContext, pansichar(Value));
  JS_SetPropertyStr(FContext, GlobalObj, pansichar(VarName), JValue);
end;

{** Set a global JS int64 variable. }
procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8; const Value: int64; const obj: string = '');
var
  GlobalObj, JValue: JSValueRaw;
begin
  GlobalObj := JS_GetGlobalObject(FContext);
  JValue    := JS_NewBigInt64(FContext, Value);
  JS_SetPropertyStr(FContext, GlobalObj, pansichar(VarName), JValue);
end;

{******************************************************************************
  Execution and invocation
******************************************************************************}

{** Load and execute a JS file from disk, returning result as string. }
function TTrndiExtEngine.ExecuteFile(const FileName: string): RawUtf8;
var
  Script: RawUtf8;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := '';
  if not FileExists(FileName) then
    raise Exception.CreateFmt(sExtFile, [sExtFile, FileName]);

  // Read file contents into Script
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  StringStream := TStringStream.Create;
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    Script := StringStream.DataString;
  finally
    StringStream.Free;
    FileStream.Free;
  end;

  // Execute with the file name for better diagnostics
  Result := Execute(Script, ExtractFileName(filename));
end;

{** Execute a JS string in global scope and return its stringified result or error text. }
function TTrndiExtEngine.Execute(const Script: RawUtf8; name: string = '<script>'): RawUtf8;
var
  EvalResult: JSValue;
  ResultStr: pansichar;
  err: RawUtf8;
begin
  FOutput := '';

  EvalResult := FContext^.Eval(Script, name, JS_EVAL_TYPE_GLOBAL, err);

  if EvalResult.IsException then
  try
    // Present error and capture exception string
    ExtError(uxdAuto, 'Error loading', err);
    ResultStr := JS_ToCString(FContext, JS_GetException(FContext));
    Result := 'Error: ' + ResultStr + err;
    JS_FreeCString(FContext, ResultStr);

    // Optional detailed analysis hook
    ExtError(uxdAuto, analyze(FContext, @evalresult));
  except
    on E: Exception do
      ExtError(uxdAuto, 'An extension''s code resulted in an error: ' + e.message);
  end
  else
  begin
    // Convert non-exception value to string
    ResultStr := JS_ToCString(FContext, EvalResult.Raw);
    Result := ResultStr;
    JS_FreeCString(FContext, ResultStr);
  end;

  // Release evaluation result
  FContext^.Free(EvalResult);
end;

{** Call a global JS function with string arguments; return the result as UTF-8. }
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8; const Args: JSArray): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  i: integer;
  StrResult: pansichar;
begin
  if not TTrndiExtEngine.Instance.FunctionExists(funcName) then
    Exit('');

  Result := '';
  GlobalObj := JS_GetGlobalObject(FContext);

  // Retrieve function from global object
  FuncObj := JS_GetPropertyStr(FContext, GlobalObj, pchar(FuncName));

  // Ensure it's callable
  if not JS_IsFunction(FContext, FuncObj) then
  begin
    JS_Free(FContext, @GlobalObj);
    JS_Free(FContext, @FuncObj);
    ExtError(uxdAuto, 'No such function or it is not callable: ' + FuncName);
    Exit('');
  end;

  // Build argument values
  SetLength(ArgArray, Length(Args));
  for i := 0 to High(Args) do
    ArgArray[i] := JS_NewString(FContext, pchar(Args[i]));

  // Invoke function
  RetVal := JS_Call(FContext, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);

  if JS_IsError(FContext, RetVal) then
  begin
    ExtError(uxdAuto, 'Cannot call Extension function ' + funcname);
    js_std_dump_error(FContext);
    Result := '';
  end
  else
  begin
    // Convert return value to string if possible
    StrResult := JS_ToCString(FContext, RetVal);
    if StrResult <> nil then
    begin
      Result := StrResult;              // copy into our Pascal string
      JS_FreeCString(FContext, StrResult);
    end
    else
      Result := '';                     // not convertible to string
  end;

  // NOTE: Consider freeing RetVal / ArgArray values if ownership rules require it.
end;

{** Call a global JS function with integer arguments; return the result as UTF-8.
    Note: Integers are marshalled as JS number (Int32). If you need 64-bit, add an overload using JS_NewBigInt64. }
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8; const Args: array of integer): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  i: integer;
  StrResult: pansichar;
begin
  if not TTrndiExtEngine.Instance.FunctionExists(funcName) then
    Exit('');

  Result := '';
  GlobalObj := JS_GetGlobalObject(FContext);

  // Retrieve function from global object
  FuncObj := JS_GetPropertyStr(FContext, GlobalObj, pchar(FuncName));

  // Ensure it's callable
  if not JS_IsFunction(FContext, FuncObj) then
  begin
    JS_Free(FContext, @GlobalObj);
    JS_Free(FContext, @FuncObj);
    ExtError(uxdAuto, 'No such function or it is not callable: ' + FuncName);
    Exit('');
  end;

  // Build integer argument values (use BigInt64 for lack of Int32 constructor in bindings)
  SetLength(ArgArray, Length(Args));
  for i := 0 to High(Args) do
  begin
    ArgArray[i] := JS_NewBigInt64(FContext, Args[i]);
  end;

  // Invoke function
  RetVal := JS_Call(FContext, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);

  if JS_IsError(FContext, RetVal) then
  begin
    ExtError(uxdAuto, 'Cannot call Extension function ' + funcname);
    js_std_dump_error(FContext);
    Result := '';
  end
  else
  begin
    // Convert return value to string if possible
    StrResult := JS_ToCString(FContext, RetVal);
    if StrResult <> nil then
    begin
      Result := StrResult;              // copy into our Pascal string
      JS_FreeCString(FContext, StrResult);
    end
    else
      Result := '';                     // not convertible to string
  end;
end;

{** Call a global JS function with mixed Pascal arguments (array of const).
    Supported TVarRec kinds are mapped as:
    - vtInteger, vtShortInt, vtSmallint, vtLongint: JS int32
    - vtInt64: JS BigInt64
    - vtBoolean: JS boolean (fallback to 'true'/'false' string if no bool ctor)
    - vtExtended, vtSingle, vtDouble, vtCurrency: JS number (string fallback if no float ctor)
    - vtChar, vtPChar, vtAnsiString, vtUnicodeString, vtWideString, vtString: JS string
    Others are stringified via Pascal's default conversions. }
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8; const Args: array of const): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  i: integer;
  StrResult: pansichar;
  tmpStrs: array of RawUtf8; // hold conversions' lifetime until call returns
  s: RawUtf8;
  tmpv: JSValue;
begin
  if not TTrndiExtEngine.Instance.FunctionExists(funcName) then
    Exit('');

  Result := '';
  GlobalObj := JS_GetGlobalObject(FContext);

  // Retrieve function from global object
  FuncObj := JS_GetPropertyStr(FContext, GlobalObj, pchar(FuncName));

  // Ensure it's callable
  if not JS_IsFunction(FContext, FuncObj) then
  begin
    JS_Free(FContext, @GlobalObj);
    JS_Free(FContext, @FuncObj);
    ExtError(uxdAuto, 'No such function or it is not callable: ' + FuncName);
    Exit('');
  end;

  // Build mixed argument values
  SetLength(ArgArray, Length(Args));
  SetLength(tmpStrs, Length(Args));
  for i := 0 to High(Args) do
  begin
    case Args[i].VType of
      // Numbers
      vtInteger:
        begin
          tmpv.From32(Args[i].VInteger);
          ArgArray[i] := tmpv.Raw;
        end;

      vtInt64:
        begin
          tmpv.from64(Args[i].VInt64^);
          ArgArray[i] := tmpv.Raw;
        end;

      vtExtended:
        begin
          tmpv.FromFloat(Args[i].VExtended^);
          ArgArray[i] := tmpv.Raw;
        end;

      // Booleans
      vtBoolean:
        begin
          tmpv.From(Args[i].VBoolean);
          ArgArray[i] := tmpv.Raw;
        end;

      // Strings
      vtChar:
        begin
          s := RawUtf8(Args[i].VChar);
          tmpStrs[i] := s;
          ArgArray[i] := JS_NewString(FContext, PChar(tmpStrs[i]));
        end;

      vtPChar:
        ArgArray[i] := JS_NewString(FContext, Args[i].VPChar);

      vtAnsiString:
        begin
          s := RawUtf8(AnsiString(Args[i].VAnsiString));
          tmpStrs[i] := s;
          ArgArray[i] := JS_NewString(FContext, PChar(tmpStrs[i]));
        end;

      vtUnicodeString, vtWideString:
        begin
          s := RawUtf8(UnicodeString(Args[i].VUnicodeString));
          tmpStrs[i] := s;
          ArgArray[i] := JS_NewString(FContext, PChar(tmpStrs[i]));
        end;

      vtString:
        begin
          s := RawUtf8(ShortString(Args[i].VString^));
          tmpStrs[i] := s;
          ArgArray[i] := JS_NewString(FContext, PChar(tmpStrs[i]));
        end;

    else
      begin
        // Fallback: stringify unsupported types
        s := RawUtf8('{unsupported}');
        tmpStrs[i] := s;
        ArgArray[i] := JS_NewString(FContext, PChar(tmpStrs[i]));
      end;
    end;
  end;

  // Invoke function
  RetVal := JS_Call(FContext, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);

  if JS_IsError(FContext, RetVal) then
  begin
    ExtError(uxdAuto, 'Cannot call Extension function ' + funcname);
    js_std_dump_error(FContext);
    Result := '';
  end
  else
  begin
    // Convert return value to string if possible
    StrResult := JS_ToCString(FContext, RetVal);
    if StrResult <> nil then
    begin
      Result := StrResult;              // copy into our Pascal string
      JS_FreeCString(FContext, StrResult);
    end
    else
      Result := '';                     // not convertible to string
  end;
end;

{******************************************************************************
  Runtime jobs processing and discovery
******************************************************************************}

{** Pump the QuickJS job queue (Promises/microtasks) on each timer tick. }
procedure TTrndiExtEngine.OnJSTimer(Sender: TObject);
begin
  while JS_IsJobPending(FRuntime) do
    if JS_ExecutePendingJob(FRuntime, @FContext) <= 0 then
      Break; // Exit when the queue is empty or on error
end;

{** Determine whether a global function exists and is callable. }
function TTrndiExtEngine.FunctionExists(const FuncName: string): boolean;
var
  Func: JSValue;
  res: boolean;
begin
  res := FContext^.GetValue(pchar(FuncName), func);
  if res = false then
    result := res
  else
    result := func.IsObject;
end;

{******************************************************************************
  Singleton management
******************************************************************************}

{** Retrieve or lazily create the singleton engine instance. }
class function TTrndiExtEngine.Instance: TTrndiExtEngine;
begin
  if FInstance = nil then
    FInstance := TTrndiExtEngine.Create;
  Result := FInstance;
end;

{** Release the singleton engine instance. }
class procedure TTrndiExtEngine.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

end.
