(*
(c) 2024-2025 github.com/slicke - Written with the aid of GPT
*)
unit trndiext.engine;
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
  trndiext.promise,
  trndiext.functions,
  fgl,
  ExtCtrls,
  fpTimer,
  forms,
  controls,
  Graphics,
  math,
  StdCtrls,
  slicke.ux.alert,


    fpimage, IntfGraphics, GraphType, EasyLazFreeType, LazFreeTypeIntfDrawer;

resourcestring
    sExtErr = 'Extension Error';
    sExtMsg = 'Extension Message';
    sExtConfirm = 'Extension Confirmation';
    sExtEvent = 'Extension Event';
    sExtWarn = 'Extension Warning';
    sExtFile = 'File "%s" not found';
type
   // Callback for JS engine out data
   TOutputCallback = procedure(const Msg: RawUtf8) of object;

   ExtFunction = JSFunction; // Alias @fixme
   ExtArgv = array of ExtFunction;
   JSArray = array of RawUtf8;

   QWordArray = array of qword; // 64 bit value array

   TTrndiExtFunc = record
     private
        args:ExtArgv;
        function getArgCount: integer;
     public
        name: RawUtf8;
        property argc: integer read getArgCount;
        property count: integer read getArgCount;

        class operator =(const a, b: TTrndiExtFunc): Boolean; overload;
     end;

   // List for known functions
   TExtFuncList = specialize TFPGList<TTrndiExtFunc>;

  // List of callbacks registered (eg for promises)
  TCallbacks = specialize TFPGList<PJSCallback>;
  TPromises = TCallbacks;  // Promises no longer have a separate definition
   TTrndiExtEngine = class
  private
    class var FInstance: TTrndiExtEngine; // Singleton
    TrndiClass: JSClassDef;   // THe TrndiClass in JS
    knownfunc: TExtFuncList;
    eventTimer: TFPTimer;  // We need a timer to execute queued stuff in QuickJS
    FRuntime: JSRuntime;
    FContext: JSContext;
    FOutput: RawUtf8;
    OutCallback: TOutputCallback;
    native: TrndiNative;
    promises: TPromises;
    function getoutput: RawUtf8;   // This is string data really
    procedure SetOutput(const val: RawUtf8);
    function uxResponse(const dialogType: TMsgDlgType; const msg: string; const titleadd: string): integer;
    function findCallback(const func: string): TJSCallback;
    function findPromise(const func: string): PJSCallback;

  public
    callbacks: TCallbacks;
    constructor Create;
    destructor Destroy; override;
    function Execute(const Script: RawUtf8; name: string = '<script>'): RawUtf8;
    function ExecuteFile(const FileName: string): RawUtf8;
    class function Instance: TTrndiExtEngine;
    class procedure ReleaseInstance;
    procedure SetOutput(ctx: PJSContext; const vals: PJSValues; const len: integer);
    property Output: RawUtf8 read GetOutput write SetOutput;
    procedure addFunction(const id: string; const func: JSFunction; const argc: integer = 0);
    function CallFunction(const FuncName: RawUtf8; const Args: JSArray): RawUtf8;
    procedure SetGlobalVariable(const VarName: RawUtf8; const Value: RawUtf8; const obj: string = '');
    procedure SetGlobalVariable(const VarName: RawUtf8; const Value: Int64; const obj: string = '');
    procedure CreateNewObject(const name: string);
    procedure AddMethod(const objectname, name: string; const func: PJSCFunction; const argc: integer = 0);
    procedure AddPromise(const funcName: string; cbfunc: JSCallbackFunction; params: integer = 1);
    procedure AddPromise(const funcName: string; cbfunc: JSCallbackFunction; minParams, maxParams: integer);
    procedure excepion(const message, fn: string);
    class function ParseArgv(ctx: PJSContext; const vals: PJSValues; const pos: integer): PChar;
    procedure alert(const msg: string);
    property callback [f: string]: TJSCallback read findCallback;
    property promise [f: string]: PJSCallback read findPromise;
    procedure OnJSTimer(Sender: TObject);
    function FunctionExists(const FuncName: string): boolean;
  end;

  EJSException = class(Exception)
  private
    FFilename: String;
  public
    constructor CreateWithName(const msg: string; const AFileName: String);
    function ToString : String; override;
    property Filename: String read FFilename write FFilename;
  end;

const
  TrndiClassID: JSClassID = 0;

implementation
{$I trndi.ext.jsbase.inc }

{ TTrndiExtEngine }
// We cant do standard comparisons, as we work with the name
class operator TTrndiExtFunc.= (const a, b: TTrndiExtFunc): Boolean; overload;
begin
  result := a.name = b.name
end;

procedure TTrndiExtEngine.alert(const msg: string);
begin
  uxResponse(mtInformation, msg, 'User Information');
end;

// Shows a message box, and returns the answer
function TTrndiExtEngine.uxResponse(const dialogType: TMsgDlgType; const msg: string; const titleadd: string): integer;
var
  btns: TMsgDlgButtons;
  title: string;
begin
title := titleadd;

  case dialogType of
    mtWarning: begin
      btns := [TMsgDlgBtn.mbOK];
      title := Format('[%s] %s', [sExtWarn,title]);
    end;
    mtError: begin
      btns := [TMsgDlgBtn.mbAbort];
      title := Format('[%s] %s', [sExtErr, title]);
    end;
    mtInformation: begin
      btns := [TMsgDlgBtn.mbOK];
      title := Format('[%s] %s', [sExtMsg, title]);
    end;
    mtConfirmation: begin
      btns := mbYesNo;
      title := Format('[%s] %s', [sextConfirm, title]);
    end;
    else begin
      btns := [TMsgDlgBtn.mbOK];
      title := Format('[%s] %s', [sExtEvent, title]);
    end;
  end;

  result := MessageDlg(title, msg, dialogType, btns,'');
end;

// Our custom exception handler, wants the filename too
constructor EJSException.CreateWithName(const msg: string;
  const AFileName: String);
begin
  inherited Create(msg);
  FFilename := AFilename;
end;

function EJSException.ToString: String;
begin
  Result := ClassName + ': ' + FFilename + LineEnding + Message;
end;

procedure TTrndiExtEngine.excepion(const message, fn: string);
begin
  raise EJSException.CreateWithName(message,fn);
end;

{
// Dummy function to test promises
function JSNOPromise(name: string; out res: string): boolean;
begin
   res := name;
   result := true;
end;
         }

// Lookup a callback, eg from a Promise
function TTrndiExtEngine.findCallback(const func: string): TJSCallback;
var
 i: Integer;
 ok: Boolean;
begin
   ok := false;
   for i := 0 to callbacks.Count-1 do begin
     if callbacks[i]^.func = func then begin
       ok := true;
       break;
     end;
     end;

  if ok then
    result := callbacks[i]^;

end;

// Find a queued/registered promise
function TTrndiExtEngine.findPromise(const func: string): PJSCallback;
var
 i: Integer;
begin
   for i := 0 to promises.Count-1 do begin
     if promises[i]^.func = func then
       Exit(promises[i])
   end;


   TTrndiExtEngine.instance.alert('Required function not found: ' +promises[i]^.func);
end;

// @see next comment
procedure TTrndiExtEngine.AddPromise(const funcName: string; cbfunc: JSCallbackFunction; params: integer = 1);
begin
   AddPromise(funcName, cbfunc, params, params);
end;

// Adds a new promise to QuickJS, adding the asyncTask function
// which in turn runs the correct callback when ran though "funcname"
procedure TTrndiExtEngine.AddPromise(const funcName: string; cbfunc: JSCallbackFunction; minParams, maxParams: integer);
var
  data: JSValueConst;
  cb: PJSCallback;
begin
  data := JS_NewString(FContext, PAnsiChar(funcname));

  JS_SetPropertyStr(
    FContext,
    JS_GetGlobalObject(FContext),
    PChar(funcname),
    JS_NewCFunctionData(FContext, PJSCFunctionData(@AsyncTask), 1, 0, 1, @data)

  );

    New(cb);
    cb^.func := funcname;
    cb^.callback := cbfunc;
    cb^.params.min := minParams;
    cb^.params.max := maxParams;

     promises.Add(cb);
//   promises.Add(TJSCallback(callback: @cbfunc; func: funcName; params.expected: params));
end;

function TTrndiExtFunc.getArgCount: integer;
begin
  result := length(args);
end;

// Returns a char pointer to a parameter position in JS, note the hack of result + to force convert the rawutf8.
class function TTrndiExtEngine.ParseArgv(ctx: PJSContext; const vals: PJSValues; const pos: integer): PChar;
begin
    result := PChar(result + ctx^^.ToUtf8(vals^[pos]));
end;

// Assigns output data by looping the vals provided
procedure TTrndiExtEngine.SetOutput(ctx: PJSContext; const vals: PJSValues; const len: integer);
var
i: integer;
begin
  for i := 0 to len do
  begin
    output := output + ctx^^.ToUtf8(vals^[i]);
  end;
end;

// Assigns output data from a JS string and runs the "outCallback"
procedure TTrndiExtEngine.SetOutput(const val: RawUtf8);
begin
  FOutput := val;
  if assigned(OutCallback) then
    OutCallback(val);
end;

// Returns stored outout
function TTrndiExtEngine.GetOutput: RawUtf8;
begin
  result := FOutput;
end;

// Constructor
constructor TTrndiExtEngine.Create;
begin
  inherited Create;

  // Create the QuickJS runtime
  FRuntime := JS_NewRuntime;
  if FRuntime = nil then
    raise Exception.Create('Failed to create JS runtime');
  // Store the context in this class
  FContext := JS_NewContext(FRuntime);
  if FContext = nil then
    raise Exception.Create('Failed to create JS context');

  // Point back to this class via the context
    JS_SetContextOpaque(FContext, Self);

  // Create the JS TrndiClass
  TrndiClass := Default(JSClassDef);
  TrndiClass.class_name := 'Trndi';

  // Create a unique ID for the class
  JS_NewClassID(@TrndiClassId);

  // Add to JS runtime
  if JS_NewClass(FRuntime, TrndiClassID, @TrndiClass) < 0 then
      raise Exception.Create('Failed to create JS class');

  // Point bacj to the class id
  JS_SetOpaque(TrndiClassId, Pointer(self));
  // Add support for promises, regex and dates in JS
  JS_AddIntrinsicPromise(FContext);
  JS_AddIntrinsicRegExp(FContext);
  JS_AddIntrinsicDate(FContext);

  // Add a tracker for unhandled promise rejections
  JS_SetHostPromiseRejectionTracker(FRuntime, PJSHostPromiseRejectionTracker(@PromiseRejectionTracker), nil);

  // Initialize a timer, to be used with the JS engine. This will execute promises in queue
  eventTimer := TFPTimer.Create(nil);
  eventTimer.Interval := 50; // Run every 50ms
  eventTimer.OnTimer := @self.OnJSTimer; // Assign the callback
  eventTimer.Enabled := True;

//  JS_AddIntrinsicJSON(FContext);
  // Create lists for promises, funcitons, callbacks etc
  promises := TPromises.Create;
  knownfunc := TExtFuncList.Create;
  native := TrndiNative.create;
  callbacks := TCallbacks.Create;

  // Add our base functions
  addFunction('log', ExtFunction(@JSDoLog), 1);
  addFunction('alert', ExtFunction(@JSDoAlert), 1);
  RegisterConsoleLog(@FContext);         // We register a "neutral" console.log outside of this object s to avoid errors here
end;

// Adds a global JS function
procedure TTrndiExtEngine.addFunction(const id: string; const func: JSFunction; const argc: integer = 0);
begin
(*  JS_SetPropertyStr(FContext, JS_GetGlobalObject(FContext), PChar(id),
  JS_NewCFunction(FContext, func, PChar(id), argc));*)
  FContext^.SetFunction([], PChar(id), func,argc);
end;

destructor TTrndiExtEngine.Destroy;
var
  cb: PJSCallback;
begin
  if FContext <> nil then
    JS_FreeContext(FContext);
  if FRuntime <> nil then
    JS_FreeRuntime(FRuntime);

  eventTimer.free;

  for cb in promises do
    Dispose(cb);
  promises.Free;

  for cb in callbacks do
    Dispose(cb);
  callbacks.Free;

  inherited Destroy;
end;

// Load a JS file from disk, and run the code inside it
function TTrndiExtEngine.ExecuteFile(const FileName: string): RawUtf8;
var
  Script: RawUtf8;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := '';
  if not FileExists(FileName) then
    raise Exception.CreateFmt(sExtFile, [sExtFile, FileName]);

  // Get file contetns as a RawUtf8 string
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  StringStream := TStringStream.Create;
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    Script := StringStream.DataString;
  finally
    StringStream.Free;
    FileStream.Free;
  end;

  // Runs the script
  Result := Execute(Script, ExtractFileName(filename));
end;

// Execute a JS string
function TTrndiExtEngine.Execute(const Script: RawUtf8; name: string = '<script>'): RawUtf8;
var
  EvalResult: JSValue;
  ResultStr: PAnsiChar;
  err: RawUtf8;
begin
  FOutput := '';

  EvalResult := FContext^.Eval(Script, name, JS_EVAL_TYPE_GLOBAL,err);

  if EvalResult.IsException then
  begin
    try
    //  TTrndiExtEngine.Instance.alert('An error occured while running extension ' + name + #13#10+err);
      ExtError('Error loading', err);
      ResultStr := JS_ToCString(FContext, JS_GetException(FContext));
      Result := 'Error: ' + ResultStr + err;
      JS_FreeCString(FContext, ResultStr);
      Showmessage(analyze(FContext, @evalresult));;
    except
      on E: Exception do SHowmessage('An extension''s code rasulted in an error: ' + e.message);
    end;
  end
  else
  begin

    // Convert errors to string
    ResultStr := JS_ToCString(FContext, EvalResult.Raw);
    Result := ResultStr;
    JS_FreeCString(FContext, ResultStr);
  end;

  FContext^.Free(EvalResult);
end;

// Create a global JS object
procedure TTrndiExtEngine.CreateNewObject(const name: string);
var
  GlobalObj, JSObject, JSValue: JSValueRaw;
begin
  // Get the global object to add to
  GlobalObj := JS_GetGlobalObject(FContext);
  // Create new object
  JSObject := JS_NewObject(FContext);

  // Add object as a global property
  JS_SetPropertyStr(FContext, GlobalObj, PAnsiChar(name), JSObject);

//  js_free(FContext, JSValue(GlobalObj)):
end;

// Add a method, to an existing object in JS
procedure TTrndiExtEngine.AddMethod(const objectname, name: string; const func: PJSCFunction; const argc: integer = 0);
var
  GlobalObj, JSObject: JSValueRaw;
begin
  // Get the global object (where our object is regged...)
  GlobalObj := JS_GetGlobalObject(FContext);
  // Find the object
  JSObject := JS_GetPropertyStr(FContext, GlobalObj, PAnsiChar(objectname));

  // Add the new method
  JS_DefinePropertyValueStr(FContext, JSObject, PAnsiChar(name), JS_NewCFunction(FContext, @func, PAnsiChar(name), argc), JS_PROP_CONFIGURABLE or JS_PROP_ENUMERABLE);
end;

// Sets a JS global variable
procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8; const Value: RawUtf8; const obj: string = '');
var
  JSValue, GlobalObj: JSValueRaw;
begin
  // Global object to add to
  GlobalObj := JS_GetGlobalObject(FContext);
  // Create the string
  JSValue := JS_NewString(FContext, PAnsiChar(Value));
  // Set it
  JS_SetPropertyStr(FContext, GlobalObj, PAnsiChar(VarName), JSValue);
end;

procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8; const Value: Int64; const obj: string = '');
var
  GlobalObj, JSValue: JSValueRaw;
begin
  // Global object to add to
  GlobalObj := JS_GetGlobalObject(FContext);
  // Create the int
  JSValue := JS_NewBigInt64(FContext, Value);
  // Set it
  JS_SetPropertyStr(FContext, GlobalObj, PAnsiChar(VarName), JSValue);
end;


function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8; const Args: JSArray): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  i: Integer;
  StrResult: PAnsiChar;
begin
  Result := '';
  GlobalObj := JS_GetGlobalObject(FContext);

  // Get the function/property
  FuncObj := JS_GetPropertyStr(FContext, GlobalObj, PChar(FuncName));

  // Check that we found a JS function
  if not JS_IsFunction(FContext, FuncObj) then
  begin
    // If it's not a function, free references and exit
    JS_Free(FContext, @GlobalObj);
    JS_Free(FContext, @FuncObj);
    ShowMessage('No such function or it is not callable: ' + FuncName);
    Exit('');
  end;

  // Prepare the parameters
  SetLength(ArgArray, Length(Args));
  for i := 0 to High(Args) do
    ArgArray[i] := JS_NewString(FContext, PChar(Args[i]));


  // Call the JS function
  RetVal := JS_Call(FContext, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);

  // Look for errors
  if JS_IsError(FContext, RetVal) then
  begin
    // Dump or retrieve the error
    ExtError('Cannot call Extension function ' + funcname);
    js_std_dump_error(FContext);
    Result := '';
  end
  else
  begin
    // Get a string
    StrResult := JS_ToCString(FContext, RetVal);
    if StrResult <> nil then
    begin
      Result := StrResult; // copy into our Pascal string
      JS_FreeCString(FContext, StrResult);
    end
    else
      Result := ''; // possibly the result wasn't convertible to string
  end;

  // Free stuff that wont get cleared automatically
//  js_freevalue(FContext, RetVal );
  for i := 0 to High(ArgArray) do
   // JS_FreeCString(FContext, @ArgArray[i]);
//  JS_Freevalue(FContext, @FuncObj);
end;

{
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8; const Args: JSArray): RawUtf8;
type
  // Helper array type for building a "dynamic" array of TVarRec, as const of array is a pain
  TVarRecArray = array of TVarRec;
var
  ArgValues: array of JSValueRaw;  // Will hold the JS string values
  AOC: TVarRecArray;               // Will try to pass this as "array of const"
  FuncResult: JSValue;
  i: Integer;
begin
  // Build a JSValueRaw array from the incoming string arguments
  SetLength(ArgValues, Length(Args));
  for i := 0 to High(Args) do
    ArgValues[i] := JS_NewString(FContext, PChar(Args[i]));

  // Build a dynamic array of TVarRec, one for each JSValueRaw
  SetLength(AOC, Length(ArgValues));
  for i := 0 to High(ArgValues) do
  begin
    AOC[i].VType    := vtPointer;            // We want to store it as a pointer
    AOC[i].VPointer := Pointer(ArgValues[i]);
  end;

  // Attempt to call FContext^.Call using the TVarRec array.
  // The hope is that FPC will accept our AOC as "array of const".
//  FuncResult := FContext^.Call('', FuncName, AOC);
  FuncResult := JS_Call(FContext, JS_GetGlobalObject(FContext); func_obj: JSValueConst; this_obj: JSValueConst;
  argc: integer; argv: PJSValueConstArr): JSValueRaw;

  // Check if the JS call resulted in an exception
  if FuncResult.IsException then
  begin
    // If so, retrieve an error message and maybe display it to the user
    FContext^.ErrorMessage(True, Result, FuncResult.Ptr);
    MessageDlg('[JS Function: ' + FuncName + '] Calling Error',
               Result, mtError, [TMsgDlgBtn.mbOK], '');
    Result := '';
  end
  else
  begin
    // If no error, convert the result to a string
    Result := FContext^.ToUtf8(FuncResult);
  end;

  // Free the function result
  FContext^.Free(FuncResult);

  // Free all JSValueRaw references that we created
  for i := 0 to High(ArgValues) do
    FContext^.Free(JSValue(ArgValues[i]));
end;
      }
  {
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8; const Args: JSArray): RawUtf8;
var
  ArgValues: array of JSValueRaw;
  FuncResult: JSValue;
  i: Integer;
begin

    case length(args) of    // Im really tired with passing the const array and at the same time freeing up memory, so I just gave in and made it like this
      0: begin
               ArgValues := [];
               FuncResult := FContext^.Call('', FuncName, []);
         end;
      1: begin
              ArgValues := [JS_NewString(FContext, PChar(Args[0]))];
              FuncResult := FContext^.Call('', FuncName, [ArgValues[0]]);
         end;
      2: begin
              ArgValues := [JS_NewString(FContext, PChar(Args[0])), JS_NewString(FContext, PChar(Args[1]))];
              FuncResult := FContext^.Call('', FuncName, [ArgValues[0],ArgValues[1]]);
         end;
      3: begin
              ArgValues := [JS_NewString(FContext, PChar(Args[0])), JS_NewString(FContext, PChar(Args[1])), JS_NewString(FContext, PChar(Args[2])), JS_NewString(FContext, PChar(Args[3]))];
              FuncResult := FContext^.Call('', FuncName, [ArgValues[0],ArgValues[1],ArgValues[2]]);
         end;
      4: begin
              ArgValues := [JS_NewString(FContext, PChar(Args[0])), JS_NewString(FContext, PChar(Args[1])), JS_NewString(FContext, PChar(Args[2])), JS_NewString(FContext, PChar(Args[3])), JS_NewString(FContext, PChar(Args[4]))];
              FuncResult := FContext^.Call('', FuncName, [ArgValues[0],ArgValues[1],ArgValues[2],ArgValues[3],ArgValues[3]]);
         end;
     5: begin
              ArgValues := [JS_NewString(FContext, PChar(Args[0])), JS_NewString(FContext, PChar(Args[1])), JS_NewString(FContext, PChar(Args[2])), JS_NewString(FContext, PChar(Args[3])), JS_NewString(FContext, PChar(Args[4]))];
              FuncResult := FContext^.Call('', FuncName, [ArgValues[0],ArgValues[1],ArgValues[2],ArgValues[3],ArgValues[4]]);
        end
      else
        ShowMessage('Trndi does not suppport more than 5 arguments at this time, you can create a pull request however in "RegisterJSArray"');
    end;


    if FuncResult.IsException then
  begin
    FContext^.ErrorMessage(true,result,FuncResult.Ptr);
    MessageDlg('[JS Function: '+funcname+'] Calling Error', result, mtError, [TMsgDlgBtn.mbOK], ''); //FContext^.ErrorDump(true, FuncResult.Ptr); //FContext^.ToUtf8(JSValue(FuncResult));
    result := '';
    FContext^.Free(FuncResult);
    Exit;
  end;

  // If no error, result to string
  Result := FContext^.ToUtf8(FuncResult);

//  ShowMessage(PChar(result));

  // Free stuff
  for i := 0 to High(ArgValues) do
    FContext^.Free(JSValue(ArgValues[i]));
  FContext^.Free(FuncResult);
end;
}

// Callback for timer, for running JS runtime jobs in queue (like promises !)
procedure TTrndiExtEngine.OnJSTimer(Sender: TObject);
begin
  while JS_IsJobPending(FRuntime) do
  begin
    if JS_ExecutePendingJob(FRuntime, @FContext) <= 0 then
      Break; // Exit whe the queue is empty
  end;
end;

// Check if a funciton is known to JS (global)
function TTrndiExtEngine.FunctionExists(const FuncName: string): boolean;
var
  Func: JSValue;
  res: boolean;
begin
  res := FContext^.GetValue(PChar(FuncName), func);
  if res = false then
     result := res
  else
     result := func.IsObject;
end;

// Return this class/object - singleton
class function TTrndiExtEngine.Instance: TTrndiExtEngine;
begin
  if FInstance = nil then
    FInstance := TTrndiExtEngine.Create;
  Result := FInstance;
end;

// Free this singleton
class procedure TTrndiExtEngine.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;
end.
