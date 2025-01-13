(*
(c) 2024 github.com/slicke - Written with GPT4 and reworked by slicke to work dynamically

Install duktape via your package manager, brew on macOS or https://github.com/mengmo/Duktape-Windows/releases/tag/v2.7.0 on Windows
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

   TOutputCallback = procedure(const Msg: RawUtf8) of object;
   ExtFunction = JSFunction;
   ExtArgv = array of ExtFunction;
   JSArray = array of RawUtf8;

   QWordArray = array of qword;

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

   TExtFuncList = specialize TFPGList<TTrndiExtFunc>;

//  TPromises = specialize TFPGList<TJSCallback>;
  TCallbacks = specialize TFPGList<PJSCallback>;
  TPromises = TCallbacks;
   TTrndiExtEngine = class
  private
    class var FInstance: TTrndiExtEngine;
    TrndiClass: JSClassDef;
    knownfunc: TExtFuncList;
    eventTimer: TFPTimer;
    FRuntime: JSRuntime;
    FContext: JSContext;
    FOutput: RawUtf8;
    OutCallback: TOutputCallback;
    native: TrndiNative;
    promises: TPromises;
    function getoutput: RawUtf8;
    procedure SetOutput(const val: RawUtf8);
    function uxResponse(const dialogType: TMsgDlgType; const msg: string; const titleadd: string): integer;
    function findCallback(const func: string): TJSCallback;
    function findPromise(const func: string): PJSCallback;
//    function callbackIndex(const func: string): integer;
//    function RunCallbackFunction(const name: string; params: JSArray): RawUtf8;

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
//    procedure error(const msg: string);
//    procedure errorRaise(const msg, err: string);
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
class operator TTrndiExtFunc.= (const a, b: TTrndiExtFunc): Boolean; overload;
begin
  result := a.name = b.name
end;

procedure TTrndiExtEngine.alert(const msg: string);
begin
  uxResponse(mtInformation, msg, 'User Information');
end;

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

function JSNOPromise(name: string; out res: string): boolean;
begin
   res := name;
   result := true;
end;


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


procedure TTrndiExtEngine.AddPromise(const funcName: string; cbfunc: JSCallbackFunction; params: integer = 1);
begin
   AddPromise(funcName, cbfunc, params, params);
end;

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

class function TTrndiExtEngine.ParseArgv(ctx: PJSContext; const vals: PJSValues; const pos: integer): PChar;
begin
    result := PChar(result + ctx^^.ToUtf8(vals^[pos]));
end;

procedure TTrndiExtEngine.SetOutput(ctx: PJSContext; const vals: PJSValues; const len: integer);
var
i: integer;
begin
  for i := 0 to len do
  begin
    output := output + ctx^^.ToUtf8(vals^[i]);
  end;
end;


procedure TTrndiExtEngine.SetOutput(const val: RawUtf8);
begin
  FOutput := val;
  if assigned(OutCallback) then
    OutCallback(val);
end;

function TTrndiExtEngine.GetOutput: RawUtf8;
begin
  result := FOutput;
end;

constructor TTrndiExtEngine.Create;
begin
  inherited Create;
  FRuntime := JS_NewRuntime;
  if FRuntime = nil then
    raise Exception.Create('Failed to create JS runtime');
  FContext := JS_NewContext(FRuntime);
  if FContext = nil then
    raise Exception.Create('Failed to create JS context');
  JS_SetContextOpaque(FContext, Self);

  TrndiClass := Default(JSClassDef);
  TrndiClass.class_name := 'Trndi';
  JS_NewClassID(@TrndiClassId);
  if JS_NewClass(FRuntime, TrndiClassID, @TrndiClass) < 0 then
      raise Exception.Create('Failed to create JS class');

  JS_SetOpaque(TrndiClassId, Pointer(self));
  JS_AddIntrinsicPromise(FContext);
  JS_AddIntrinsicRegExp(FContext);
  JS_AddIntrinsicDate(FContext);

  JS_SetHostPromiseRejectionTracker(FRuntime, PJSHostPromiseRejectionTracker(@PromiseRejectionTracker), nil);

  eventTimer := TFPTimer.Create(nil);
  eventTimer.Interval := 50; // Kör jobbkön var 50 ms
  eventTimer.OnTimer := @self.OnJSTimer;
  eventTimer.Enabled := True;

//  JS_AddIntrinsicJSON(FContext);
  promises := TPromises.Create;
  knownfunc := TExtFuncList.Create;
  native := TrndiNative.create;
  callbacks := TCallbacks.Create;

  addFunction('log', ExtFunction(@JSDoLog), 1);
  addFunction('alert', ExtFunction(@JSDoAlert), 1);
  RegisterConsoleLog(@FContext);         // We register a "neutral" console.log outside of this object s to avoid errors here
end;

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

function TTrndiExtEngine.ExecuteFile(const FileName: string): RawUtf8;
var
  Script: RawUtf8;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := '';
  if not FileExists(FileName) then
    raise Exception.CreateFmt(sExtFile, [sExtFile, FileName]);

  // Läsa in filens innehåll till en RawUtf8-sträng
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  StringStream := TStringStream.Create;
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    Script := StringStream.DataString;
  finally
    StringStream.Free;
    FileStream.Free;
  end;

  // Exekvera skriptet från filen
  Result := Execute(Script, ExtractFileName(filename));
end;

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
      ExtError('Fel vid laddning', err);
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

    ResultStr := JS_ToCString(FContext, EvalResult.Raw);
    Result := ResultStr;
    JS_FreeCString(FContext, ResultStr);
  end;

  FContext^.Free(EvalResult);
end;

procedure TTrndiExtEngine.CreateNewObject(const name: string);
var
  GlobalObj, JSObject, JSValue: JSValueRaw;
begin
  GlobalObj := JS_GetGlobalObject(FContext);
  // Skapa ett nytt objekt
  JSObject := JS_NewObject(FContext);

  // Registrera objektet som en global variabel
  JS_SetPropertyStr(FContext, GlobalObj, PAnsiChar(name), JSObject);

//  js_free(FContext, JSValue(GlobalObj)):
end;


procedure TTrndiExtEngine.AddMethod(const objectname, name: string; const func: PJSCFunction; const argc: integer = 0);
var
  GlobalObj, JSObject: JSValueRaw;
begin

  GlobalObj := JS_GetGlobalObject(FContext);
  // Hämta objektet
  JSObject := JS_GetPropertyStr(FContext, GlobalObj, PAnsiChar(objectname));

  // Lägg till en metod
  JS_DefinePropertyValueStr(FContext, JSObject, PAnsiChar(name), JS_NewCFunction(FContext, @func, PAnsiChar(name), argc), JS_PROP_CONFIGURABLE or JS_PROP_ENUMERABLE);
end;


procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8; const Value: RawUtf8; const obj: string = '');
var
  JSValue, GlobalObj: JSValueRaw;
begin
  GlobalObj := JS_GetGlobalObject(FContext);
  JSValue := JS_NewString(FContext, PAnsiChar(Value));
  JS_SetPropertyStr(FContext, GlobalObj, PAnsiChar(VarName), JSValue);
end;

procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8; const Value: Int64; const obj: string = '');
var
  GlobalObj, JSValue: JSValueRaw;
begin
  GlobalObj := JS_GetGlobalObject(FContext);
  JSValue := JS_NewBigInt64(FContext, Value);
  JS_SetPropertyStr(FContext, GlobalObj, PAnsiChar(VarName), JSValue);
end;


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

  // Konvertera resultatet till en sträng om inget fel inträffade
  Result := FContext^.ToUtf8(FuncResult);

//  ShowMessage(PChar(result));

  // Frigör argument och funktionsresultat
  for i := 0 to High(ArgValues) do
    FContext^.Free(JSValue(ArgValues[i]));
  FContext^.Free(FuncResult);
end;

procedure TTrndiExtEngine.OnJSTimer(Sender: TObject);
begin
  while JS_IsJobPending(FRuntime) do
  begin
    if JS_ExecutePendingJob(FRuntime, @FContext) <= 0 then
      Break; // Avsluta om inga fler jobb kan köras
  end;
end;

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



class function TTrndiExtEngine.Instance: TTrndiExtEngine;
begin
  if FInstance = nil then
    FInstance := TTrndiExtEngine.Create;
  Result := FInstance;
end;

class procedure TTrndiExtEngine.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;
end.
