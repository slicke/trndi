// Extension manager
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
    API_Class_id : JSClassID; // The ID for the Trndi class
    API_Class_Proto : JSValue; // Prototype, to hold funcs etc
    JClass : JSClassDef; // Class definition in JS
    tab : array [0..1] of JSCFunctionListEntry; // Our props/funcs regged in QuickJS  (@see RegisterExtension)
   public
    procedure RegisterExtension(const ctx : JSContext; module: JSModuleDef = nil); cdecl;
    procedure RegisterModule(var val: JSValue; var ctx : JSContext; name, export: string; module: JSModuleDef = nil);
    class function VerifySource(const ctx: JSContext; Buf: PChar; buf_len: integer;
  filename: PChar; is_main : boolean; eval_flags: integer = -1): JSValue;


    constructor Create;
end;

implementation

// Runs code from file
// eval_flags = -1 will auto-determine JS_EVAL_TYPE_MODULE or JS_EVAL_TYPE_GLOBAL
// Returns JS_NULL if an error occurs!
class function TrndiExtension.VerifySource(const ctx: JSContext; Buf: PChar; buf_len: integer;
  filename: PChar; is_main : boolean; eval_flags: integer = -1): JSValue;
var
  ret: JSValue;
begin
  // Determine eval flags, if not set (= -1)
  if eval_flags = -1 then
  begin
    if JS_DetectModule(Buf,buf_len) then
      eval_flags := JS_EVAL_TYPE_MODULE
    else
      eval_flags := JS_EVAL_TYPE_GLOBAL;
  end;

  // If module, compile first and then run if it was successful
  if (eval_flags and JS_EVAL_TYPE_MASK) = JS_EVAL_TYPE_MODULE then
  begin
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags or JS_EVAL_FLAG_COMPILE_ONLY);
    if not JS_IsException(ret) then
    begin
      // Set the meta data, ie if it's the main module
      js_module_set_import_meta(ctx, ret, True, is_main);
      ret := JS_EvalFunction(ctx, ret);
    end;
  end
  else
    // Execute gobally
    ret := JS_Eval(ctx, buf, buf_len, filename, eval_flags);

  // Manage errors
  if JS_IsException(ret) then
  begin
    js_std_dump_error(ctx);
    Result := JS_NULL;
  end
  else
    Result := ret;
end;


// Register a new module in QuickJS (= creates a "C module" with the given name and exports export
// Note: std, os and the TrndiJS helpers are auto-provided to the JS
procedure TrndiExtension.RegisterModule(var val: JSValue; var ctx : JSContext; name, export: string; module: JSModuleDef = nil);
  // Verifies/executes code, like verifySource but with other error handling
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

  // Callable by JS_NewCModule to add the extension
  function init(var ctx : JSContext; m : JSModuleDef): Integer;cdecl;
  begin
    RegisterExtension(ctx,m);
    Result := 0;
  end;

const
  // Helper code to setup the base JS environment
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
  m := JS_NewCModule(ctx, PChar(name), @init); // Create a new "C module", using init
  JS_AddModuleExport(ctx,m,PChar(export)); // Add export
  // Execute the script as a module
  VerifySource(ctx, std_hepler, strlen(std_hepler), '<global_helper>', False, JS_EVAL_TYPE_MODULE);

  // Return a reference
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


// Registers the extension in QuickJS with a class id, functions, properties etc
procedure TrndiExtension.RegisterExtension(const ctx : JSContext; module: JSModuleDef = nil); cdecl;
  // JS accessible method
  function install(var ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
  var
    Module,API : PChar;
    OnCallBack,res : JSValue;
  begin
    Result := JS_UNDEFINED;
    if argc >= 2 then
    begin
      try
        // argv 0 = module name / 1 = API name
        Module := JS_ToCString(ctx, argv[0]);
        API := JS_ToCString(ctx, argv[1]);

        // Get the callback method provided
        OnCallBack := JS_GetPropertyStr(ctx,this_val,'OnCallBack');
        if JS_IsFunction(ctx,OnCallBack) then
        begin
          // Call the callback within JS
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

  // JS constructor for TrndiExt
  function New(var ctx : JSContext; new_target : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
  begin
    // Create object tied to our class id
    Result := JS_NewObjectClass(ctx,API_Class_id);
    // New Array for every new instance.
    JS_DefinePropertyValueStr(ctx,Result,'args',JS_NewArray(ctx),JS_PROP_CONFIGURABLE or JS_PROP_WRITABLE);
  end;

var
  obj,global : JSValue;
begin
  // Install module?
  if module <> nil then begin
    // Create the constructor
    obj := JS_NewCFunction2(ctx, PJSCFunction(@New), 'TrndiExtension', 1, JS_CFUNC_constructor, 0);
    // Export the constructor
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


