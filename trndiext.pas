unit trndiext;

{$mode delphi}{$H+}{$M+}
{$PackRecords C}

interface

uses
  quickjs,
  dialogs;


type
TrndiExtension = class
   private
    API_Class_id : JSClassID;
    API_Class_Proto : JSValue;
    JClass : JSClassDef;
    tab : array [0..1] of JSCFunctionListEntry;
   public
    procedure RegisterExtension(const ctx : JSContext; module: JSModuleDef = nil); cdecl;
    procedure RegisterModule(var val: JSValue; var ctx : JSContext; name, export: string; module: JSModuleDef = nil);
    class function VerifySource(const ctx: JSContext; Buf: PChar; buf_len: integer;
  filename: PChar; is_main : boolean; eval_flags: integer = -1): JSValue;


    constructor Create;
end;

implementation

class function TrndiExtension.VerifySource(const ctx: JSContext; Buf: PChar; buf_len: integer;
  filename: PChar; is_main : boolean; eval_flags: integer = -1): JSValue;
var
  ret: JSValue;
begin
  if eval_flags = -1 then
  begin
    if JS_DetectModule(Buf,buf_len) then
      eval_flags := JS_EVAL_TYPE_MODULE
    else
      eval_flags := JS_EVAL_TYPE_GLOBAL;
  end;

  if (eval_flags and JS_EVAL_TYPE_MASK) = JS_EVAL_TYPE_MODULE then
  begin
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags or JS_EVAL_FLAG_COMPILE_ONLY);
    if not JS_IsException(ret) then
    begin
      js_module_set_import_meta(ctx, ret, True, is_main);
      ret := JS_EvalFunction(ctx, ret);
      Showmessage('??');
    end;
  end
  else
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags);

  if JS_IsException(ret) then
  begin
    js_std_dump_error(ctx);
    Result := JS_NULL;
  end
  else
    Result := ret;
end;


procedure TrndiExtension.RegisterModule(var val: JSValue; var ctx : JSContext; name, export: string; module: JSModuleDef = nil);
function xVerifySource(const ctx: JSContext; Buf: PChar; buf_len: integer;
  filename: PChar; is_main : boolean; eval_flags: integer = -1): JSValue;
var
  ret: JSValue;
begin
  if eval_flags = -1 then
  begin
    if JS_DetectModule(Buf,buf_len) then
      eval_flags := JS_EVAL_TYPE_MODULE
    else
      eval_flags := JS_EVAL_TYPE_GLOBAL;
  end;

  if (eval_flags and JS_EVAL_TYPE_MASK) = JS_EVAL_TYPE_MODULE then
  begin
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags or JS_EVAL_FLAG_COMPILE_ONLY);
    if not JS_IsException(ret) then
    begin
      js_module_set_import_meta(ctx, ret, True, is_main);
      ret := JS_EvalFunction(ctx, ret);
      Showmessage('?..?');
    end;
  end
  else
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags);

  if JS_IsException(ret) then
  begin
    js_std_dump_error(ctx);
    Result := JS_NULL;
  end
  else
    Result := ret;
end;

  function init(var ctx : JSContext; m : JSModuleDef): Integer;cdecl;
  begin
    RegisterExtension(ctx,m);
    Result := 0;
  end;

const
  std_hepler : PChar =
    'import * as std from ''std'';'#10+
    'import * as os from ''os'';'#10+
    'import * as TrndiJS from ''TrndiJS'';'#10+
    'globalThis.std = std;'#10+
    'globalThis.os = os;'#10+
    'globalThis.TrndiJS = TrndiJS;'#10;
var
  m: JSModuleDef;
begin
  m := JS_NewCModule(ctx, PChar(name), @init);
  JS_AddModuleExport(ctx,m,PChar(export));
  VerifySource(ctx, std_hepler, strlen(std_hepler), '<global_helper>', False, JS_EVAL_TYPE_MODULE);

  val := JS_GetGlobalObject(ctx);
end;

constructor TrndiExtension.Create;
begin
  API_Class_id := 0;
//  API_Class_Proto := JSValue;
  with JClass do  begin
    class_name := 'TrndiExtension';
    finalizer := nil;
    gc_mark := nil;
    call := nil;
    exotic := nil;
  end;

end;



procedure TrndiExtension.RegisterExtension(const ctx : JSContext; module: JSModuleDef = nil); cdecl;
  function install(var ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
  var
    Module,API : PChar;
    OnCallBack,res : JSValue;
  begin
    Result := JS_UNDEFINED;
    if argc >= 2 then
    begin
      try
        Module := JS_ToCString(ctx, argv[0]);
        API := JS_ToCString(ctx, argv[1]);

        OnCallBack := JS_GetPropertyStr(ctx,this_val,'OnCallBack');
        if JS_IsFunction(ctx,OnCallBack) then
        begin
          res := JS_Call(ctx,OnCallBack,this_val,argc,argv);
          if JS_IsException(res) then
             exit(res);
          Writeln('OnCallBack return = ', JS_ToBool(ctx,res));
        end;
      finally
        JS_FreeValue(ctx,OnCallBack);
        JS_FreeCString(ctx, Module);
        JS_FreeCString(ctx, API);
      end;
    end;
  end;

    function New(var ctx : JSContext; new_target : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
  begin
    Result := JS_NewObjectClass(ctx,API_Class_id);
    // New Array for every new instance.
    JS_DefinePropertyValueStr(ctx,Result,'args',JS_NewArray(ctx),JS_PROP_CONFIGURABLE or JS_PROP_WRITABLE);
  end;

var
  obj,global : JSValue;
begin
  if module <> nil then begin
    obj := JS_NewCFunction2(ctx, PJSCFunction(@New), 'TrndiExtension', 1, JS_CFUNC_constructor, 0);
    JS_SetModuleExport(ctx, module, 'TrndiExtension', obj);
    Exit;
  end;

  // Create New Class id.
  JS_NewClassID(@API_Class_id);
  // Create the Class Name and other stuff.
  JS_NewClass(JS_GetRuntime(ctx),API_Class_id,@JClass);

  // Properties list.
  tab[0] := JS_CFUNC_DEF('install', 1, @install);
  tab[1] := JS_PROP_INT32_DEF('version', 1337, JS_PROP_CONFIGURABLE);// add "or JS_PROP_WRITABLE" to make it writable.

  // New Object act as Prototype for the Class.
  API_Class_Proto := JS_NewObject(ctx);

  // Set list of Properties to the prototype Object.
  JS_SetPropertyFunctionList(ctx,API_Class_Proto,@tab,Length(tab));

  // Set the Prototype to the Class.
  JS_SetClassProto(ctx, API_Class_id, API_Class_Proto);

  // Set the Class native constructor.
  obj := JS_NewCFunction2(ctx, @New, 'TrndiExtension', 1, JS_CFUNC_constructor, 0);

  // Add the Class to Global Object so we can use it.
  global := JS_GetGlobalObject(ctx);
  JS_SetPropertyStr(ctx,global,'TrndiExtension',obj);
  JS_FreeValue(ctx,global);
end;

end.


