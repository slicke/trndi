(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)
unit trndi.ext.engine;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
SysUtils,
mormot.core.base,
mormot.core.os,
mormot.core.Text,
mormot.core.buffers,
mormot.core.unicode,
mormot.core.datetime,
mormot.core.rtti,
mormot.crypt.core,
mormot.core.Data,
mormot.core.variants,
mormot.core.json,
mormot.core.log,
mormot.core.perf,
mormot.core.test,
mormot.lib.quickjs,
Dialogs,
Classes,
trndi.native,
trndi.ext.promise,
trndi.ext.functions,
trndi.ext.perm,
fgl,
ExtCtrls,
fpTimer,
Forms,
Controls,
Graphics,
Math,
StdCtrls,
slicke.ux.alert,
trndi.strings,
fpimage, IntfGraphics, GraphType, EasyLazFreeType, LazFreeTypeIntfDrawer;

type
  {** Callback signature invoked when JavaScript code emits output via this engine. }
TOutputCallback = procedure(const Msg: RawUtf8) of object;

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
  Name: RawUtf8;
    {** Number of arguments registered for this function. }
  property argc: integer read getArgCount;
    {** Synonym of @code(argc). }
  property Count: integer read getArgCount;

    {** Equality operator compares on @code(name) only. }
  class operator =(const a, b: TTrndiExtFunc): boolean; overload;
end;

  {** List of known/registered extension functions. }
TExtFuncList = specialize TFPGList<TTrndiExtFunc>;

  {** List of registered callbacks (e.g., for Promises and async tasks). }
TCallbacks = specialize TFPGList<PJSCallback>;
  {** Alias: promises are managed as callbacks. }
TPromises = TCallbacks;

  {** Simple helper class to bridge timer events to JS function calls.
      Each timer gets its own handler instance with the timer info in the Tag. }
TJSTimerHandler = class
public
  procedure OnTimer(Sender: TObject);
end;

  {** Information for a registered JS timer (setTimeout/setInterval). }
TJSTimerInfo = record
  TimerID: integer;           // Unique ID returned to JS
  Timer: TFPTimer;            // The actual timer object
  Handler: TJSTimerHandler;   // Handler object for timer events
  Callback: JSValueRaw;       // Duplicated JS function value; owned, freed on disposal
  Args: array of JSValueRaw;  // Duplicated extra args (setTimeout(fn, ms, ...args)); owned
  IsInterval: boolean;        // True for setInterval, False for setTimeout
  Context: JSContext;         // QuickJS context for calling the function
end;

PJSTimerInfo = ^TJSTimerInfo;

  {** Map of timer ID to timer information. }
TJSTimerMap = specialize TFPGMap<integer, PJSTimerInfo>;

  {** Timers awaiting disposal. A timer (and its handler) must never be freed
      from inside its own OnTimer event, so cancelled/expired timers are parked
      here and disposed later by the engine's job pump. }
TJSTimerInfoList = specialize TFPGList<PJSTimerInfo>;

  {** A promise rejection noted by the QuickJS tracker but not yet reported.
      QuickJS fires the tracker the moment a promise is rejected with no
      handler attached, then fires it again (is_handled=true) if a handler
      arrives later in the same script — so reporting must be deferred until
      the job queue drains. Key holds the promise's raw value bits. }
TPendingRejection = record
  Key: UInt64;
  Msg: string;
end;

  {** Per-extension JavaScript context (Path B isolation).
      Each .js file gets its own JSContext (sharing the runtime), so the
      function registry exposed to one extension is decoupled from another. }
PExtContextInfo = ^TExtContextInfo;
TExtContextInfo = record
  Ctx: JSContext;            // QuickJS context, owned by this record
  FileName: string;          // absolute path to the .js file
  ExtId: string;             // stable id derived from filename
  Hash: string;              // sha256 of file contents (re-prompt on change)
  DisplayName: string;       // parsed from header (first line)
  Author: string;            // parsed from header ("(c) ..." line)
  Granted: TExtPermSet;      // baseline + user-approved promptable perms
end;

TExtContextList = specialize TFPGList<PExtContextInfo>;

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
    {** Map of active JS timers (setTimeout/setInterval). }
  FJSTimers: TJSTimerMap;
    {** Counter for generating unique timer IDs. }
  FNextTimerID: integer;
    {** Cancelled/expired timers pending disposal (see TJSTimerInfoList). }
  FDeadTimers: TJSTimerInfoList;
    {** >0 while a JS timer callback is on the stack; blocks FDeadTimers
        draining so a timer is never freed beneath its own event. }
  FInTimerCallback: integer;
    {** Rejections seen by PromiseRejectionTracker, reported (or cancelled)
        on the next job-pump tick. }
  FPendingRejections: array of TPendingRejection;

    {** Loaded per-extension contexts (Path B). Each user .js script lives in its own ctx. }
  FExtContexts: TExtContextList;
    {** Active "target" context for registration helpers (addFunction/addClassFunction/AddPromise).
        nil means write to FContext (legacy/admin path). Set by BeginRegistration. }
  FCurrentRegCtx: JSContext;
    {** Active permissions for the current registration target. Used by *If gated helpers. }
  FCurrentRegPerms: TExtPermSet;

  function GetOutput: RawUtf8;
  procedure SetOutput(const val: RawUtf8);

    {** Dispose timers parked in FDeadTimers. Safe to call anytime; no-ops
        while a timer callback is running. }
  procedure DrainDeadTimers;

    {** Show a UX dialog and return the button pressed.

        @param(dialogType Message dialog type)
        @param(msg        Dialog message text)
        @param(titleadd   Extra title suffix/prefix)
        @returns(Integer code based on selected button) }
  function uxResponse(const dialogType: TMsgDlgType; const msg: string;
    const titleadd: string): integer;

    {** Find a registered callback by name.

        @param(func Callback identifier)
        @returns(TJSCallback record; undefined if not found) }
  function findCallback(const func: string): TJSCallback;

    {** Find a registered promise callback by name.

        @param(func Promise/callback identifier)
        @returns(Pointer to callback record; may raise if not found) }
  function findPromise(const func: string): PJSCallback;
    {** Internal unified call helper dispatching all JS function invocations.

        RawPrefix values are caller-prepared @code(JSValueRaw) arguments (arrays, objects, scalars)
        passed directly without re-marshalling. @code(Rest) is an open array of const which is
        marshalled on-the-fly to temporary JS values (numbers/strings/booleans).

        Freeing semantics:
        - @code(freeRaw=true) frees every element of @code(RawPrefix) after the call returns
          (use only for one-shot arguments you won't reuse).
        - @code(freeRest=true) frees each marshalled temporary produced from @code(Rest).
        - Set corresponding flags to false if you will retain and reuse values (only meaningful for RawPrefix).

        Duplicating the exact same JSValueRaw twice in @code(RawPrefix) while @code(freeRaw=true)
        would cause double-free attempts; avoid by constructing separate values if needed.

        @param(FuncName   Global JS function name to invoke)
        @param(RawPrefix  Pre-built JSValueRaw arguments supplied as-is)
        @param(Rest       Pascal arguments to marshal into JS values)
        @param(freeRaw    Whether to free RawPrefix JS values after invocation)
        @param(freeRest   Whether to free marshalled Rest JS temporaries)
        @returns(Stringified JS return value; empty string on error or if function missing) }
  function InternalCall(const FuncName: RawUtf8; const RawPrefix: array of JSValueRaw;
    const Rest: array of const; freeRaw, freeRest: boolean): RawUtf8;
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
  function Execute(const Script: RawUtf8; Name: string = '<script>'): RawUtf8;

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
  procedure addFunction(const id: string; const func: JSFunction;
    const argc: integer = 0);

    {** Register a function as a method of the 'Trndi' JS class.

        @param(id    Method name)
        @param(func  Native function pointer)
        @param(argc  Declared arity; use -1 for variadic) }
  procedure addClassFunction(const id: string; const func: JSFunction;
    const argc: integer = 0);
    // (Removed prior experimental overload infrastructure)

    {** Call a global JS function by name with string arguments.

        @param(FuncName Global function name)
        @param(Args     Array of string arguments)
        @returns(Stringified result; empty string if call failed/not found) }
  function CallFunction(const FuncName: RawUtf8; const Args: JSArray): RawUtf8;
    overload;

   {** Call a global JS function by name with integer arguments.

        @param(FuncName Global function name)
        @param(Args     Array of integer arguments)
        @returns(Stringified result; empty string if call failed/not found) }
  function CallFunction(const FuncName: RawUtf8;
    const Args: array of integer): RawUtf8;
    overload;

  {** Call a global JS function by name with different arguments.

      @param(FuncName Global function name)
      @param(Args     Array of const arguments)
      @returns(Stringified result; empty string if call failed/not found) }
  function CallFunction(const FuncName: RawUtf8; const Args: array of const): RawUtf8;
    overload;
  {** Build a QuickJS array from a Pascal open array of const values.
    Supported element kinds mirror the mixed CallFunction overload.
    Returns JS_UNDEFINED if context invalid or on allocation failure. }
  function CreateJSArray(const Values: array of const): JSValueRaw;
  {** Convenience: call a JS function passing a single array argument built
    from Values. Equivalent JS: func([v0,v1,...]) }
  function CallFunctionWithArrayArg(const FuncName: RawUtf8;
    const Values: array of const): RawUtf8;
  {** Call where the first argument is a pre-built @code(JSValueRaw) (e.g. Array/Object) and the rest are Pascal values.

      Ownership:
      - @code(autoFree=true) frees ONLY the marshalled @code(Rest) arguments created internally.
      - @code(autoFreeFirst=true) additionally frees @code(FirstArg) (use for one-shot values).
        Leave it false if you will reuse @code(FirstArg).

      @param(FuncName      Global JS function name)
      @param(FirstArg      Pre-built JSValueRaw passed as first argument)
      @param(Rest          Pascal open array of const to marshal into trailing JS args)
      @param(autoFree      Free marshalled Rest JS values after call; default true)
      @param(autoFreeFirst Free supplied FirstArg after call; default false)
      @returns(Stringified result; empty string on failure) }
  function CallFunctionArrayFirst(const FuncName: RawUtf8;
    const FirstArg: JSValueRaw; const Rest: array of const;
    autoFree: boolean = true; autoFreeFirst: boolean = false): RawUtf8;
  {** Call with an array of pre-built @code(JSValueRaw) arguments followed by marshalled Pascal args.

      Freeing rules:
      - @code(rawAutoFree=true)  frees each element of @code(RawArgs) after invocation (one-shot usage).
      - @code(restAutoFree=true) frees each marshalled temporary created from @code(Rest).

      Example:
        raws[0] := eng.CreateJSArray([1,2,3]);
        res := eng.CallFunctionMixed('demo', raws, ['label', 42]);

      @param(FuncName     Global JS function name)
      @param(RawArgs      Array of pre-built JSValueRaw arguments)
      @param(Rest         Pascal values to marshal)
      @param(restAutoFree Free marshalled Rest values; default true)
      @param(rawAutoFree  Free provided RawArgs values; default false)
      @returns(Stringified result or empty string on error) }
  function CallFunctionMixed(const FuncName: RawUtf8;
    const RawArgs: array of JSValueRaw; const Rest: array of const;
    restAutoFree: boolean = true; rawAutoFree: boolean = false): RawUtf8;
  {** Factories to create standalone JS values you can mix with arrays:
       js := eng.MakeJSArray([1,2,3]);
       s  := eng.MakeJSString('hi');
       i  := eng.MakeJSInt64(42);
       eng.CallFunctionJS('foo',[s, js, i]);
     All returned JSValueRaw should normally be freed with JS_FreeValue after use
     unless you pass autoFree=true to CallFunctionJS (default). }
  function MakeJSString(const S: RawUtf8): JSValueRaw; inline;
  function MakeJSInt64(const V: int64): JSValueRaw; inline;
  function MakeJSFloat(const V: double): JSValueRaw; inline;
  function MakeJSBool(const V: boolean): JSValueRaw; inline;
    {** Convenience alias: build a JS array from Pascal values (same as CreateJSArray). }
  function MakeJSArray(const Values: array of const): JSValueRaw; inline;
  {** Call a function with pre-built @code(JSValueRaw) arguments (scalars, arrays, objects).
    @param(FuncName Global JS function name)
    @param(Args     Array of pre-built JSValueRaw values)
    @param(autoFree Free each value in Args after the call; default true for convenience)
    @returns(Stringified result or empty string on error) }
  function CallFunctionJS(const FuncName: RawUtf8; const Args: array of JSValueRaw;
    autoFree: boolean = true): RawUtf8;
  {** Example mixed usage:
      eng.Execute('function demo(label, data, count){ return label+":"+data.length+":"+count; }');
      arr := eng.CreateJSArray([1,2,3]);
      res := eng.CallFunctionJS('demo', [eng.MakeJSString('sizes'), arr, eng.MakeJSInt64(3)]);
      // res -> 'sizes:3:3'
    }
    {** Example:
        // JS side:
        // function sumArr(arr) { return arr.reduce((a,b)=>a+b,0); }

        // Pascal side:
        // eng.Execute('function sumArr(arr){return arr.reduce((a,b)=>a+b,0);}');
        // res := eng.CallFunctionWithArrayArg('sumArr',[1,2,3,4]); // => '10'
      }

    {** Set a global JS variable (string).

        @param(VarName Variable name)
        @param(Value   String value)
        @param(obj     Unused placeholder; reserved) }
  procedure SetGlobalVariable(const VarName: RawUtf8; const Value: RawUtf8;
    const obj: string = '');

    {** Set a global JS variable (int64).

        @param(VarName Variable name)
        @param(Value   64-bit integer value)
        @param(obj     Unused placeholder; reserved) }
  procedure SetGlobalVariable(const VarName: RawUtf8; const Value: int64;
    const obj: string = '');

    {** Create an empty global JS object with the given name.

        @param(name Global identifier for the new object) }
  procedure CreateNewObject(const Name: string);

    {** Register a Promise-style async entry point with fixed arity.

        @param(funcName  Name of the JS function to expose)
        @param(cbfunc    Pascal callback to run when job executes)
        @param(params    Exact number of parameters expected)
        @param(threaded  True to run the callback on the worker thread; the
                         callback must not touch UI or the JS context then) }
  procedure AddPromise(const funcName: string; cbfunc: JSCallbackFunction;
    params: integer = 1; threaded: boolean = false);

    {** Register a Promise-style async entry point with min/max arity.

        @param(funcName  Name of the JS function to expose)
        @param(cbfunc    Pascal callback to run when job executes)
        @param(minParams Minimum number of parameters allowed)
        @param(maxParams Maximum number of parameters allowed)
        @param(threaded  True to run the callback on the worker thread; the
                         callback must not touch UI or the JS context then) }
  procedure AddPromise(const funcName: string; cbfunc: JSCallbackFunction;
    minParams, maxParams: integer; threaded: boolean = false);

    {** Raise an EJSException carrying a message and file name.

        @param(message Error message)
        @param(fn      Source file or logical script name) }
  procedure excepion(const message, fn: string);

    {** Convert an argument at @code(pos) from JS to UTF-8.

        @param(ctx JS context)
        @param(vals Pointer to argument array)
        @param(pos  Index of argument to convert)
        @returns(UTF-8 copy of the argument value) }
  class function ParseArgv(ctx: PJSContext; const vals: PJSValues;
    const pos: integer): RawUtf8;

    {** Show an informational alert dialog to the user. }
  procedure alert(const msg: string);

    {** Access a registered callback by name. }
  property callback[f: string]: TJSCallback read findCallback;
    {** Access a registered promise callback pointer by name. }
  property promise[f: string]: PJSCallback read findPromise;

    {** Timer callback to process pending JS jobs (Promises/microtasks). }
  procedure OnJSTimer(Sender: TObject);

    {** Record a rejection with no handler yet (from PromiseRejectionTracker). }
  procedure NoteRejection(key: UInt64; const msg: string);
    {** Cancel a pending rejection report: a handler was attached after all. }
  procedure NoteRejectionHandled(key: UInt64);
    {** Alert any rejections still unhandled once the job queue has drained. }
  procedure FlushPendingRejections;

    {** Check whether a global function with @code(FuncName) exists in JS.

        Looks across all loaded extension contexts; falls back to the admin
        context (FContext) when no extensions are loaded. @returns(True if any
        ext context (or FContext when none) exposes a callable with this name). }
  function FunctionExists(const FuncName: string): boolean;

    {** Active target context for registration helpers. Falls back to FContext when
        no extension is currently being provisioned. }
  function CurrentRegistrationContext: JSContext;

    {** True if the permission is granted for the registration currently in
        progress. Always true outside a BeginRegistration block (legacy/admin path). }
  function CanRegister(const grp: TExtPermGroup): boolean;

    {** Create a fresh JSContext for an extension and prep it (intrinsics + Trndi class +
        opaque pointer). Caller is responsible for invoking BeginRegistration / running
        the gated registration block / ExtExecuteFile / EndRegistration. }
  function NewExtensionContext(const ExtId, FileName, Hash, DisplayName,
    Author: string; Granted: TExtPermSet): PExtContextInfo;

    {** Set the registration target to a freshly-created extension context. Subsequent
        addFunction/addClassFunction/AddPromise calls write into Ext.Ctx and are gated
        by Ext.Granted via the *If helpers. }
  procedure BeginRegistration(Ext: PExtContextInfo);

    {** Restore the registration target to the admin/legacy default (FContext). }
  procedure EndRegistration;

    {** Convenience: register a Trndi class method only if @code(grp) is granted
        in the current registration. }
  procedure addClassFunctionIf(const grp: TExtPermGroup; const id: string;
    const func: JSFunction; const argc: integer = 0);

    {** Convenience: register a global JS function only if @code(grp) is granted. }
  procedure addFunctionIf(const grp: TExtPermGroup; const id: string;
    const func: JSFunction; const argc: integer = 0);

    {** Convenience: register a promise-style function only if @code(grp) is granted. }
  procedure AddPromiseIf(const grp: TExtPermGroup; const funcName: string;
    cbfunc: JSCallbackFunction; minParams, maxParams: integer;
    threaded: boolean = false); overload;
  procedure AddPromiseIf(const grp: TExtPermGroup; const funcName: string;
    cbfunc: JSCallbackFunction; params: integer = 1;
    threaded: boolean = false); overload;

    {** Evaluate @code(Script) in the current registration context (the extension
        ctx during provisioning, otherwise FContext). Used to inject JS-side
        shims such as fetch(). Returns false if evaluation raised. }
  function ExecuteInCurrent(const Script: RawUtf8;
    const Name: string = '<shim>'): boolean;

    {** Load the file at Ext.FileName into Ext.Ctx and evaluate it. Returns the
        evaluation result (or 'Error: ...' on failure, matching ExecuteFile). }
  function ExtExecuteFile(Ext: PExtContextInfo): RawUtf8;

    {** Iterate every loaded extension context. Lifetime of returned pointers
        is owned by the engine; do not free. }
  function ExtensionCount: integer;
  function ExtensionAt(idx: integer): PExtContextInfo;

    {** Tear down every per-extension context: cancel JS timers, drain pending
        jobs, free each @code(JSContext) and clear the registry. The shared
        runtime stays alive so a fresh @code(LoadExtensions) can repopulate
        without recreating the engine singleton. }
  procedure UnloadExtensions;

    {** Register the engine-internal baseline (alert/confirm/prompt/select/log,
        setTimeout/setInterval/clear*, console.*) into the current registration
        context, gated by the current registration permissions.

        Called from @code(Create) (writes into FContext with no perm filtering)
        and from @code(NewExtensionContext) inside a Begin/End block (writes
        into the extension's ctx, filtered by its grants). }
  procedure RegisterEngineBaselineForCurrent;
end;

  {** Exception type for JS-related errors with filename context. }
EJSException = class(Exception)
private
  FFilename: string;
public
    {** Create exception with a message and file name context. }
  constructor CreateWithName(const msg: string; const AFileName: string);
    {** Stringify exception as 'ClassName: FileName<newline>Message'. }
  function ToString: string; override;
    {** Name of the source file or logical script name. }
  property Filename: string read FFilename write FFilename;
end;

{** Emergency shutdown control functions }
{** Set the global shutdown flag - call this as early as possible during app termination }
procedure SetGlobalShutdown;
{** Check if application is shutting down }
function IsGlobalShutdown: boolean;

{** Check whether @code(ctx) exposes a callable named @code(FuncName) in its global scope.
    Used by the per-extension broadcast paths in @code(inc/umain_ext.inc). }
function ContextHasFunction(ctx: JSContext; const FuncName: string): boolean;

var
  {** Class ID used for the 'Trndi' class in QuickJS context. }
TrndiClassID: JSClassID;

  {** Global flag to indicate application is shutting down - set as early as possible }
GlobalShutdownInProgress: boolean = false;

implementation

{$I trndi.ext.jsbase.inc }

{******************************************************************************
  TJSTimerHandler - Helper class for JS timer events
  Modified: January 16, 2026
******************************************************************************}

{** Free a retired timer record and the duplicated JS values it owns.
    Every call site runs while the timer's context is still alive: DrainDeadTimers
    only fires during normal operation, and Destroy/UnloadExtensions tear timers
    down before freeing any context. }
procedure DisposeTimerInfo(info: PJSTimerInfo);
var
  i: integer;
begin
  if info = nil then
    Exit;
  if Assigned(info^.Timer) then
  begin
    info^.Timer.Enabled := false;
    info^.Timer.Free;
  end;
  if Assigned(info^.Handler) then
    info^.Handler.Free;
  if info^.Context <> nil then
  begin
    info^.Context^.FreeInlined(PJSValue(@info^.Callback));
    for i := 0 to High(info^.Args) do
      info^.Context^.FreeInlined(PJSValue(@info^.Args[i]));
  end;
  Dispose(info);
end;

procedure TJSTimerHandler.OnTimer(Sender: TObject);
var
  TimerInfo: PJSTimerInfo;
  Engine: TTrndiExtEngine;
  RetVal: JSValueRaw;
  idx, nargs: integer;
  Timer: TFPTimer;
  argp: PJSValueConstArr;
  err: RawUtf8;
begin
  if not (Sender is TFPTimer) then
    Exit;

  Timer := TFPTimer(Sender);
  TimerInfo := PJSTimerInfo(Timer.Tag);

  if TimerInfo = nil then
    Exit;

  Engine := TTrndiExtEngine.Instance;
  if (Engine = nil) or IsGlobalShutdown then
    Exit;

  Inc(Engine.FInTimerCallback);
  try
    // Invoke the stored callback value directly (works for anonymous/arrow
    // functions too), forwarding any extra setTimeout/setInterval arguments.
    nargs := Length(TimerInfo^.Args);
    if nargs > 0 then
      argp := PJSValueConstArr(@TimerInfo^.Args[0])
    else
      argp := nil;
    RetVal := JS_UNDEFINED;
    try
      RetVal := JS_Call(TimerInfo^.Context, TimerInfo^.Callback,
        JS_UNDEFINED, nargs, argp);
      if PJSValue(@RetVal)^.IsException then
      begin
        err := '';
        TimerInfo^.Context^.ErrorMessage(true, err, nil);
        Engine.SetOutput('Timer callback error: ' + string(err));
      end;
    except on E: Exception do
        TTrndiExtEngine.Instance.SetOutput('Timer exception: ' + E.Message);
    end;

    // Release the call result; the callback/args stay owned by TimerInfo.
    TimerInfo^.Context^.FreeInlined(PJSValue(@RetVal));

    // If this is a setTimeout (one-shot), retire it. The JS callback may already
    // have cancelled us via clearTimeout, in which case we're no longer in the map.
    if not TimerInfo^.IsInterval then
    begin
      idx := Engine.FJSTimers.IndexOf(TimerInfo^.TimerID);
      if idx >= 0 then
      begin
        Timer.Enabled := false;
        Engine.FJSTimers.Delete(idx);
        // Freeing the timer/handler here would run their destructors beneath
        // this very event; park them for the engine's job pump instead.
        Engine.FDeadTimers.Add(TimerInfo);
      end;
    end;
  finally
    Dec(Engine.FInTimerCallback);
  end;
end;

{******************************************************************************
  Global shutdown control functions
******************************************************************************}

procedure SetGlobalShutdown;
begin
  GlobalShutdownInProgress := true;
end;

function IsGlobalShutdown: boolean;
begin
  Result := GlobalShutdownInProgress;
end;

{******************************************************************************
  TTrndiExtFunc
******************************************************************************}

{** Compare two function descriptors by @code(name) only. }
class operator TTrndiExtFunc.=(const a, b: TTrndiExtFunc): boolean; overload;
begin
  Result := a.Name = b.Name;
end;

function TTrndiExtFunc.getArgCount: integer;
begin
  Result := length(args);
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
  uxResponse(mtInformation, msg, RS_EXT_USER_INFO);
end;

{** Show a UX dialog and return the button pressed. }
function TTrndiExtEngine.uxResponse(const dialogType: TMsgDlgType;
const msg: string; const titleadd: string): integer;
var
  btns: TUXMsgDlgBtns;
  header, title: string;
begin
  title := titleadd;

  case dialogType of
  mtWarning:
  begin
    btns := [mbOK];
    title := Format('[%s] %s', [RS_EXT_WARN, title]);
    header := RS_EXT_WARN;
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
    title := Format('[%s] %s', [RS_EXT_MSG, title]);
    header := RS_EXT_MSG;
  end;
  mtConfirmation:
  begin
    btns := [mbYes, mbNo];
    title := Format('[%s] %s', [RS_EXT_CONFIRM, title]);
    header := RS_EXT_CONFIRM;
  end;
  else
  begin
    btns := [mbOK];
    title := Format('[%s] %s', [RS_EXT_EVENT, title]);
    header := RS_EXT_EVENT;
  end;
  end;

  Result := UXDialog(uxdAuto, header, title, msg, btns, dialogType);
end;

{** Raise JS exception with filename context. }
procedure TTrndiExtEngine.excepion(const message, fn: string);
begin
  raise EJSException.CreateWithName(message, fn);
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
  Result.func := '';
  Result.callback := nil;
  Result.params.min := 0;
  Result.params.max := 0;
  ok := false;
  for i := 0 to callbacks.Count - 1 do
    if (callbacks[i] <> nil) and (callbacks[i]^.func = func) then
    begin
      ok := true;
      break;
    end;

  if ok then
    Result := callbacks[i]^;
  // else leave Result default-initialized
end;

{** Find a registered promise by name or alert if missing.

    Note: The alert dereferences @code(promises[i]) after the loop if not found,
    which could be invalid; consider refactoring to avoid deref when not found. }
function TTrndiExtEngine.findPromise(const func: string): PJSCallback;
var
  i: integer;
begin
  for i := 0 to promises.Count - 1 do
    if (promises[i] <> nil) and (promises[i]^.func = func) then
      Exit(promises[i]);

  // Not found: return nil without alerting (safe for teardown paths)
  Result := nil;
end;

{** Add Promise helper with fixed @code(params) expected (min=max). }
procedure TTrndiExtEngine.AddPromise(const funcName: string;
cbfunc: JSCallbackFunction; params: integer = 1; threaded: boolean = false);
begin
  AddPromise(funcName, cbfunc, params, params, threaded);
end;

{** Register a Promise entry point by exposing an async task wrapper in JS
    and storing the associated Pascal callback with arity constraints.
    Writes the JS shim into the current registration context (FCurrentRegCtx
    when provisioning an extension, otherwise FContext). The Pascal callback
    record is stored on the shared promises list. }
procedure TTrndiExtEngine.AddPromise(const funcName: string;
cbfunc: JSCallbackFunction; minParams, maxParams: integer;
threaded: boolean = false);
var
  Data: JSValueConst;
  cb: PJSCallback;
  ctx: JSContext;
begin
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;

  // Expose an async task function in global scope that will route to our callback
  Data := JS_NewString(ctx, pansichar(funcname));
  JS_SetPropertyStr(
    ctx,
    JS_GetGlobalObject(ctx),
    pchar(funcname),
    JS_NewCFunctionData(ctx, PJSCFunctionData(@AsyncTask), 1, 0, 1, @Data)
    );

  // Track the Pascal callback and expected parameter range. Only add once —
  // promises is keyed by name and re-registration is idempotent.
  if findPromise(funcName) = nil then
  begin
    New(cb);
    cb^.func := funcname;
    cb^.callback := cbfunc;
    cb^.params.min := minParams;
    cb^.params.max := maxParams;
    cb^.threaded := threaded;
    promises.Add(cb);
  end;
end;

procedure TTrndiExtEngine.AddPromiseIf(const grp: TExtPermGroup;
const funcName: string; cbfunc: JSCallbackFunction;
minParams, maxParams: integer; threaded: boolean = false);
begin
  if CanRegister(grp) then
    AddPromise(funcName, cbfunc, minParams, maxParams, threaded);
end;

procedure TTrndiExtEngine.AddPromiseIf(const grp: TExtPermGroup;
const funcName: string; cbfunc: JSCallbackFunction; params: integer = 1;
threaded: boolean = false);
begin
  if CanRegister(grp) then
    AddPromise(funcName, cbfunc, params, threaded);
end;

{******************************************************************************
  TTrndiExtEngine: output helpers
******************************************************************************}

{** Return a UTF-8 copy of argument @code(pos). }
class function TTrndiExtEngine.ParseArgv(ctx: PJSContext; const vals: PJSValues;
const pos: integer): RawUtf8;
begin
  Result := ctx^^.ToUtf8(vals^[pos]);
end;

{** Append JS values (0..len) to @code(Output) as UTF-8 strings. }
procedure TTrndiExtEngine.SetOutput(ctx: JSContext; const vals: PJSValues;
const len: integer);
var
  i: integer;
  totalLen: integer;
  temp: rawutf8;
  p: PAnsiChar;
begin
  if len < 0 then Exit;
  totalLen := 0;
  for i := 0 to len do
    Inc(totalLen, Length(ctx^.ToUtf8(vals^[i])));
  SetLength(temp, totalLen);
  p := PAnsiChar(temp);
  for i := 0 to len do
  begin
    Move(PAnsiChar(ctx^.ToUtf8(vals^[i]))^, p^, Length(ctx^.ToUtf8(vals^[i])));
    Inc(p, Length(ctx^.ToUtf8(vals^[i])));
  end;
  output := output + temp;
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
  Result := FOutput;
end;

{******************************************************************************
  Module loader
******************************************************************************}

{** Module loader for QuickJS: reads module source from disk.

    @returns(Allocated C-string with module source, or nil on error) }
function TrndiModuleLoader(ctx: JSContext; module_name: pansichar;
opaque: pointer): pansichar; cdecl;
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
  Result := StrNew(pansichar(Script));
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

  // Clear shutdown guard at startup
  SetExtShuttingDown(false);

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

  // Note: Do NOT call JS_SetOpaque on a class id. JS_SetOpaque expects a JSValue
  // instance, not a class id. Keeping only the context opaque above is correct.

  // Expose 'Trndi' constructor in global scope
  JS_SetPropertyStr(
    FContext,
    JS_GetGlobalObject(FContext),
    'Trndi',
    JS_NewCFunction2(FContext, PJSCFunction(@TrndiConstructor), 'Trndi', 0,
    JS_CFUNC_constructor, 0)
    );

  // Add essential intrinsics
  JS_AddIntrinsicPromise(FContext);
  JS_AddIntrinsicRegExp(FContext);
  JS_AddIntrinsicDate(FContext);
  JS_SetHostPromiseRejectionTracker(FRuntime,
    PJSHostPromiseRejectionTracker(@PromiseRejectionTracker), nil);

  // Initialize timer to pump pending JS jobs (Promises/microtasks)
  eventTimer := TFPTimer.Create(nil);
  eventTimer.Interval := 50;       // run every 50ms
  eventTimer.OnTimer := @self.OnJSTimer;
  eventTimer.Enabled := true;

  // Collections
  promises := TPromises.Create;
  knownfunc := TExtFuncList.Create;
  native := TrndiNative.Create;
  callbacks := TCallbacks.Create;
  
  // Initialize timer tracking (Modified: January 16, 2026)
  FJSTimers := TJSTimerMap.Create;
  FNextTimerID := 0;
  FDeadTimers := TJSTimerInfoList.Create;
  FInTimerCallback := 0;

  // Per-extension contexts (Path B). FCurrentRegCtx=nil routes legacy
  // bootstrap registrations to FContext (admin/template context).
  FExtContexts := TExtContextList.Create;
  FCurrentRegCtx := nil;
  FCurrentRegPerms := [];

  // Register baseline engine-internal functions into FContext (admin/template).
  // Per-extension contexts will re-run this gated to their grants in NewExtensionContext.
  RegisterEngineBaselineForCurrent;
end;

{** Register UI/timer/console baseline. Uses CurrentRegistrationContext + CanRegister
    so the same code path drives both the admin-context bootstrap and each
    per-extension provisioning. }
procedure TTrndiExtEngine.RegisterEngineBaselineForCurrent;
const
  {** Pure-JS polyfills for common web globals QuickJS lacks: atob/btoa
      (WHATWG base64, Latin1 range) and queueMicrotask. Ungated — they touch
      no native functionality, so every context gets them regardless of
      granted permissions. }
  BASE_SHIM: string =
    '(function () {' + LineEnding +
    '  var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";' + LineEnding +
    '  if (typeof globalThis.btoa !== "function")' + LineEnding +
    '    globalThis.btoa = function (input) {' + LineEnding +
    '      var str = String(input), out = "", i, a, b, c;' + LineEnding +
    '      for (i = 0; i < str.length; i++)' + LineEnding +
    '        if (str.charCodeAt(i) > 255)' + LineEnding +
    '          throw new TypeError("btoa: character out of Latin1 range");' + LineEnding +
    '      for (i = 0; i < str.length; i += 3) {' + LineEnding +
    '        a = str.charCodeAt(i); b = str.charCodeAt(i + 1); c = str.charCodeAt(i + 2);' + LineEnding +
    '        out += chars[a >> 2];' + LineEnding +
    '        out += chars[((a & 3) << 4) | (isNaN(b) ? 0 : b >> 4)];' + LineEnding +
    '        out += isNaN(b) ? "=" : chars[((b & 15) << 2) | (isNaN(c) ? 0 : c >> 6)];' + LineEnding +
    '        out += isNaN(c) ? "=" : chars[c & 63];' + LineEnding +
    '      }' + LineEnding +
    '      return out;' + LineEnding +
    '    };' + LineEnding +
    '  if (typeof globalThis.atob !== "function")' + LineEnding +
    '    globalThis.atob = function (input) {' + LineEnding +
    '      var str = String(input).replace(/[\t\n\f\r ]+/g, "");' + LineEnding +
    '      if (!/^[A-Za-z0-9+/]*={0,2}$/.test(str))' + LineEnding +
    '        throw new TypeError("atob: invalid base64");' + LineEnding +
    '      str = str.replace(/=+$/, "");' + LineEnding +
    '      if (str.length % 4 === 1)' + LineEnding +
    '        throw new TypeError("atob: invalid base64");' + LineEnding +
    '      var out = "", buffer = 0, bits = 0, i;' + LineEnding +
    '      for (i = 0; i < str.length; i++) {' + LineEnding +
    '        buffer = (buffer << 6) | chars.indexOf(str[i]);' + LineEnding +
    '        bits += 6;' + LineEnding +
    '        if (bits >= 8) {' + LineEnding +
    '          bits -= 8;' + LineEnding +
    '          out += String.fromCharCode((buffer >> bits) & 255);' + LineEnding +
    '        }' + LineEnding +
    '      }' + LineEnding +
    '      return out;' + LineEnding +
    '    };' + LineEnding +
    '  if (typeof globalThis.queueMicrotask !== "function")' + LineEnding +
    '    globalThis.queueMicrotask = function (cb) {' + LineEnding +
    '      if (typeof cb !== "function")' + LineEnding +
    '        throw new TypeError("queueMicrotask: argument must be a function");' + LineEnding +
    '      Promise.resolve().then(cb);' + LineEnding +
    '    };' + LineEnding +
    '})();';

  {** ES2019–ES2022 library methods the bundled quickjspp fork predates:
      String.replaceAll, Object.hasOwn/fromEntries, Array/String at,
      Array findLast/findLastIndex, Promise.allSettled/any (+ AggregateError).
      All guarded, added non-enumerable so for..in loops stay clean. Ungated. }
  ES_SHIM: string =
    '(function () {' + LineEnding +
    '  function def(obj, name, fn) {' + LineEnding +
    '    if (!obj[name])' + LineEnding +
    '      Object.defineProperty(obj, name, { value: fn, writable: true, configurable: true });' + LineEnding +
    '  }' + LineEnding +
    '  function escRe(s) { return String(s).replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); }' + LineEnding +
    '  def(String.prototype, "replaceAll", function (search, replacement) {' + LineEnding +
    '    if (search instanceof RegExp) {' + LineEnding +
    '      if (String(search.flags).indexOf("g") < 0)' + LineEnding +
    '        throw new TypeError("replaceAll must be called with a global RegExp");' + LineEnding +
    '      return this.replace(search, replacement);' + LineEnding +
    '    }' + LineEnding +
    '    return this.replace(new RegExp(escRe(search), "g"), replacement);' + LineEnding +
    '  });' + LineEnding +
    '  def(Object, "hasOwn", function (obj, key) {' + LineEnding +
    '    return Object.prototype.hasOwnProperty.call(obj, key);' + LineEnding +
    '  });' + LineEnding +
    '  def(Object, "fromEntries", function (entries) {' + LineEnding +
    '    var obj = {}, it = entries[Symbol.iterator](), r;' + LineEnding +
    '    for (r = it.next(); !r.done; r = it.next()) obj[r.value[0]] = r.value[1];' + LineEnding +
    '    return obj;' + LineEnding +
    '  });' + LineEnding +
    '  function atImpl(i) {' + LineEnding +
    '    i = Math.trunc(i) || 0;' + LineEnding +
    '    if (i < 0) i += this.length;' + LineEnding +
    '    return (i < 0 || i >= this.length) ? undefined : this[i];' + LineEnding +
    '  }' + LineEnding +
    '  def(Array.prototype, "at", atImpl);' + LineEnding +
    '  def(String.prototype, "at", atImpl);' + LineEnding +
    '  def(Array.prototype, "findLast", function (fn, thisArg) {' + LineEnding +
    '    for (var i = this.length - 1; i >= 0; i--)' + LineEnding +
    '      if (fn.call(thisArg, this[i], i, this)) return this[i];' + LineEnding +
    '    return undefined;' + LineEnding +
    '  });' + LineEnding +
    '  def(Array.prototype, "findLastIndex", function (fn, thisArg) {' + LineEnding +
    '    for (var i = this.length - 1; i >= 0; i--)' + LineEnding +
    '      if (fn.call(thisArg, this[i], i, this)) return i;' + LineEnding +
    '    return -1;' + LineEnding +
    '  });' + LineEnding +
    '  if (typeof globalThis.AggregateError !== "function")' + LineEnding +
    '    globalThis.AggregateError = function (errors, message) {' + LineEnding +
    '      var e = new Error(message === undefined ? "" : String(message));' + LineEnding +
    '      e.name = "AggregateError";' + LineEnding +
    '      e.errors = Array.prototype.slice.call(errors);' + LineEnding +
    '      return e;' + LineEnding +
    '    };' + LineEnding +
    '  def(Promise, "allSettled", function (list) {' + LineEnding +
    '    return Promise.all(Array.prototype.map.call(list, function (p) {' + LineEnding +
    '      return Promise.resolve(p).then(' + LineEnding +
    '        function (v) { return { status: "fulfilled", value: v }; },' + LineEnding +
    '        function (e) { return { status: "rejected", reason: e }; });' + LineEnding +
    '    }));' + LineEnding +
    '  });' + LineEnding +
    '  def(Promise, "any", function (list) {' + LineEnding +
    '    var arr = Array.prototype.slice.call(list);' + LineEnding +
    '    if (arr.length === 0)' + LineEnding +
    '      return Promise.reject(new AggregateError([], "All promises were rejected"));' + LineEnding +
    '    return new Promise(function (resolve, reject) {' + LineEnding +
    '      var errs = new Array(arr.length), pending = arr.length;' + LineEnding +
    '      arr.forEach(function (p, i) {' + LineEnding +
    '        Promise.resolve(p).then(resolve, function (e) {' + LineEnding +
    '          errs[i] = e;' + LineEnding +
    '          pending--;' + LineEnding +
    '          if (pending === 0) reject(new AggregateError(errs, "All promises were rejected"));' + LineEnding +
    '        });' + LineEnding +
    '      });' + LineEnding +
    '    });' + LineEnding +
    '  });' + LineEnding +
    '})();';

  {** URLSearchParams, a basic absolute-URL URL (with searchParams reflection
      and simple relative resolution against a base; no mailto:/data: schemes),
      and UTF-8 TextEncoder/TextDecoder. Pure JS, guarded, ungated. }
  WEB_SHIM: string =
    '(function () {' + LineEnding +
    '  function dec(s) { try { return decodeURIComponent(String(s).replace(/\+/g, " ")); } catch (e) { return String(s); } }' + LineEnding +
    '  function enc(s) { return encodeURIComponent(String(s)).replace(/%20/g, "+"); }' + LineEnding +
    '  function USP(init) {' + LineEnding +
    '    this._pairs = [];' + LineEnding +
    '    var i, k, s, parts, eq;' + LineEnding +
    '    if (init === undefined || init === null) return;' + LineEnding +
    '    if (init instanceof USP) {' + LineEnding +
    '      for (i = 0; i < init._pairs.length; i++)' + LineEnding +
    '        this._pairs.push([init._pairs[i][0], init._pairs[i][1]]);' + LineEnding +
    '    } else if (init instanceof Array) {' + LineEnding +
    '      for (i = 0; i < init.length; i++)' + LineEnding +
    '        this._pairs.push([String(init[i][0]), String(init[i][1])]);' + LineEnding +
    '    } else if (typeof init === "object") {' + LineEnding +
    '      for (k in init) this._pairs.push([k, String(init[k])]);' + LineEnding +
    '    } else {' + LineEnding +
    '      s = String(init);' + LineEnding +
    '      if (s.charAt(0) === "?") s = s.slice(1);' + LineEnding +
    '      parts = s === "" ? [] : s.split("&");' + LineEnding +
    '      for (i = 0; i < parts.length; i++) {' + LineEnding +
    '        if (parts[i] === "") continue;' + LineEnding +
    '        eq = parts[i].indexOf("=");' + LineEnding +
    '        if (eq < 0) this._pairs.push([dec(parts[i]), ""]);' + LineEnding +
    '        else this._pairs.push([dec(parts[i].slice(0, eq)), dec(parts[i].slice(eq + 1))]);' + LineEnding +
    '      }' + LineEnding +
    '    }' + LineEnding +
    '  }' + LineEnding +
    '  USP.prototype.append = function (n, v) { this._pairs.push([String(n), String(v)]); };' + LineEnding +
    '  USP.prototype.delete = function (n) {' + LineEnding +
    '    n = String(n);' + LineEnding +
    '    this._pairs = this._pairs.filter(function (p) { return p[0] !== n; });' + LineEnding +
    '  };' + LineEnding +
    '  USP.prototype.get = function (n) {' + LineEnding +
    '    n = String(n);' + LineEnding +
    '    for (var i = 0; i < this._pairs.length; i++)' + LineEnding +
    '      if (this._pairs[i][0] === n) return this._pairs[i][1];' + LineEnding +
    '    return null;' + LineEnding +
    '  };' + LineEnding +
    '  USP.prototype.getAll = function (n) {' + LineEnding +
    '    n = String(n);' + LineEnding +
    '    return this._pairs.filter(function (p) { return p[0] === n; }).map(function (p) { return p[1]; });' + LineEnding +
    '  };' + LineEnding +
    '  USP.prototype.has = function (n) { return this.get(n) !== null; };' + LineEnding +
    '  USP.prototype.set = function (n, v) {' + LineEnding +
    '    n = String(n); v = String(v);' + LineEnding +
    '    var i, seen = false, out = [];' + LineEnding +
    '    for (i = 0; i < this._pairs.length; i++) {' + LineEnding +
    '      if (this._pairs[i][0] === n) {' + LineEnding +
    '        if (!seen) { out.push([n, v]); seen = true; }' + LineEnding +
    '      } else out.push(this._pairs[i]);' + LineEnding +
    '    }' + LineEnding +
    '    if (!seen) out.push([n, v]);' + LineEnding +
    '    this._pairs = out;' + LineEnding +
    '  };' + LineEnding +
    '  USP.prototype.sort = function () {' + LineEnding +
    '    this._pairs.sort(function (a, b) { return a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0; });' + LineEnding +
    '  };' + LineEnding +
    '  USP.prototype.forEach = function (fn, thisArg) {' + LineEnding +
    '    for (var i = 0; i < this._pairs.length; i++)' + LineEnding +
    '      fn.call(thisArg, this._pairs[i][1], this._pairs[i][0], this);' + LineEnding +
    '  };' + LineEnding +
    '  USP.prototype.keys = function () { return this._pairs.map(function (p) { return p[0]; })[Symbol.iterator](); };' + LineEnding +
    '  USP.prototype.values = function () { return this._pairs.map(function (p) { return p[1]; })[Symbol.iterator](); };' + LineEnding +
    '  USP.prototype.entries = function () { return this._pairs.map(function (p) { return [p[0], p[1]]; })[Symbol.iterator](); };' + LineEnding +
    '  USP.prototype[Symbol.iterator] = USP.prototype.entries;' + LineEnding +
    '  USP.prototype.toString = function () {' + LineEnding +
    '    return this._pairs.map(function (p) { return enc(p[0]) + "=" + enc(p[1]); }).join("&");' + LineEnding +
    '  };' + LineEnding +
    '  if (typeof globalThis.URLSearchParams !== "function") globalThis.URLSearchParams = USP;' + LineEnding +
    '  function hostString(p) { return p.hostname + (p.port ? ":" + p.port : ""); }' + LineEnding +
    '  function normPath(path) {' + LineEnding +
    '    var parts = path.split("/"), out = [], i;' + LineEnding +
    '    for (i = 0; i < parts.length; i++) {' + LineEnding +
    '      if (parts[i] === ".") continue;' + LineEnding +
    '      if (parts[i] === "..") { if (out.length > 1) out.pop(); continue; }' + LineEnding +
    '      out.push(parts[i]);' + LineEnding +
    '    }' + LineEnding +
    '    return out.join("/") || "/";' + LineEnding +
    '  }' + LineEnding +
    '  function parseAbs(s) {' + LineEnding +
    '    var m = /^([A-Za-z][A-Za-z0-9+.\-]*):\/\/([^\/?#]*)([^?#]*)(\?[^#]*)?(#.*)?$/.exec(s);' + LineEnding +
    '    if (!m) return null;' + LineEnding +
    '    var auth = m[2], username = "", password = "", host = auth, at, cred, col, close, pc;' + LineEnding +
    '    at = auth.lastIndexOf("@");' + LineEnding +
    '    if (at >= 0) {' + LineEnding +
    '      cred = auth.slice(0, at);' + LineEnding +
    '      host = auth.slice(at + 1);' + LineEnding +
    '      col = cred.indexOf(":");' + LineEnding +
    '      if (col < 0) username = cred;' + LineEnding +
    '      else { username = cred.slice(0, col); password = cred.slice(col + 1); }' + LineEnding +
    '    }' + LineEnding +
    '    var hostname = host, port = "";' + LineEnding +
    '    if (host.charAt(0) === "[") {' + LineEnding +
    '      close = host.indexOf("]");' + LineEnding +
    '      hostname = host.slice(0, close + 1);' + LineEnding +
    '      if (host.charAt(close + 1) === ":") port = host.slice(close + 2);' + LineEnding +
    '    } else {' + LineEnding +
    '      pc = host.indexOf(":");' + LineEnding +
    '      if (pc >= 0) { hostname = host.slice(0, pc); port = host.slice(pc + 1); }' + LineEnding +
    '    }' + LineEnding +
    '    var protocol = m[1].toLowerCase() + ":";' + LineEnding +
    '    if ((protocol === "http:" && port === "80") || (protocol === "https:" && port === "443")) port = "";' + LineEnding +
    '    if (hostname === "") return null;' + LineEnding +
    '    return {' + LineEnding +
    '      protocol: protocol, username: username, password: password,' + LineEnding +
    '      hostname: hostname.toLowerCase(), port: port,' + LineEnding +
    '      pathname: m[3] === "" ? "/" : m[3],' + LineEnding +
    '      search: m[4] || "", hash: m[5] || ""' + LineEnding +
    '    };' + LineEnding +
    '  }' + LineEnding +
    '  function URLCtor(url, base) {' + LineEnding +
    '    url = String(url);' + LineEnding +
    '    var p = parseAbs(url), b, origin, abs, rest, q, h, hi, qi, dir, self = this;' + LineEnding +
    '    if (!p) {' + LineEnding +
    '      if (base === undefined || base === null) throw new TypeError("Invalid URL: " + url);' + LineEnding +
    '      b = parseAbs(base && base.href !== undefined ? String(base.href) : String(base));' + LineEnding +
    '      if (!b) throw new TypeError("Invalid base URL: " + String(base));' + LineEnding +
    '      origin = b.protocol + "//" + hostString(b);' + LineEnding +
    '      if (url.slice(0, 2) === "//") abs = b.protocol + url;' + LineEnding +
    '      else if (url.charAt(0) === "/") abs = origin + url;' + LineEnding +
    '      else if (url.charAt(0) === "?") abs = origin + b.pathname + url;' + LineEnding +
    '      else if (url.charAt(0) === "#") abs = origin + b.pathname + b.search + url;' + LineEnding +
    '      else if (url === "") abs = origin + b.pathname + b.search;' + LineEnding +
    '      else {' + LineEnding +
    '        rest = url; q = ""; h = "";' + LineEnding +
    '        hi = rest.indexOf("#");' + LineEnding +
    '        if (hi >= 0) { h = rest.slice(hi); rest = rest.slice(0, hi); }' + LineEnding +
    '        qi = rest.indexOf("?");' + LineEnding +
    '        if (qi >= 0) { q = rest.slice(qi); rest = rest.slice(0, qi); }' + LineEnding +
    '        dir = b.pathname.slice(0, b.pathname.lastIndexOf("/") + 1);' + LineEnding +
    '        abs = origin + normPath(dir + rest) + q + h;' + LineEnding +
    '      }' + LineEnding +
    '      p = parseAbs(abs);' + LineEnding +
    '      if (!p) throw new TypeError("Invalid URL: " + url);' + LineEnding +
    '    }' + LineEnding +
    '    this.protocol = p.protocol;' + LineEnding +
    '    this.username = p.username;' + LineEnding +
    '    this.password = p.password;' + LineEnding +
    '    this.hostname = p.hostname;' + LineEnding +
    '    this.port = p.port;' + LineEnding +
    '    this.pathname = p.pathname;' + LineEnding +
    '    this.hash = p.hash;' + LineEnding +
    '    this.searchParams = new globalThis.URLSearchParams(p.search);' + LineEnding +
    '    Object.defineProperty(this, "search", {' + LineEnding +
    '      get: function () { var s = self.searchParams.toString(); return s === "" ? "" : "?" + s; },' + LineEnding +
    '      set: function (v) { self.searchParams = new globalThis.URLSearchParams(String(v)); },' + LineEnding +
    '      configurable: true' + LineEnding +
    '    });' + LineEnding +
    '    Object.defineProperty(this, "host", {' + LineEnding +
    '      get: function () { return hostString(self); },' + LineEnding +
    '      configurable: true' + LineEnding +
    '    });' + LineEnding +
    '    Object.defineProperty(this, "origin", {' + LineEnding +
    '      get: function () { return self.protocol + "//" + hostString(self); },' + LineEnding +
    '      configurable: true' + LineEnding +
    '    });' + LineEnding +
    '    Object.defineProperty(this, "href", {' + LineEnding +
    '      get: function () {' + LineEnding +
    '        var cred = "";' + LineEnding +
    '        if (self.username !== "" || self.password !== "")' + LineEnding +
    '          cred = self.username + (self.password !== "" ? ":" + self.password : "") + "@";' + LineEnding +
    '        return self.protocol + "//" + cred + hostString(self) + self.pathname + self.search + self.hash;' + LineEnding +
    '      },' + LineEnding +
    '      configurable: true' + LineEnding +
    '    });' + LineEnding +
    '  }' + LineEnding +
    '  URLCtor.prototype.toString = function () { return this.href; };' + LineEnding +
    '  URLCtor.prototype.toJSON = function () { return this.href; };' + LineEnding +
    '  if (typeof globalThis.URL !== "function") globalThis.URL = URLCtor;' + LineEnding +
    '  function TE() { this.encoding = "utf-8"; }' + LineEnding +
    '  TE.prototype.encode = function (input) {' + LineEnding +
    '    var str = input === undefined ? "" : String(input);' + LineEnding +
    '    var bytes = [], i, c;' + LineEnding +
    '    for (i = 0; i < str.length; i++) {' + LineEnding +
    '      c = str.codePointAt(i);' + LineEnding +
    '      if (c > 0xFFFF) i++;' + LineEnding +
    '      if (c >= 0xD800 && c <= 0xDFFF) c = 0xFFFD;' + LineEnding +
    '      if (c < 0x80) bytes.push(c);' + LineEnding +
    '      else if (c < 0x800) bytes.push(0xC0 | (c >> 6), 0x80 | (c & 63));' + LineEnding +
    '      else if (c < 0x10000) bytes.push(0xE0 | (c >> 12), 0x80 | ((c >> 6) & 63), 0x80 | (c & 63));' + LineEnding +
    '      else bytes.push(0xF0 | (c >> 18), 0x80 | ((c >> 12) & 63), 0x80 | ((c >> 6) & 63), 0x80 | (c & 63));' + LineEnding +
    '    }' + LineEnding +
    '    return new Uint8Array(bytes);' + LineEnding +
    '  };' + LineEnding +
    '  if (typeof globalThis.TextEncoder !== "function") globalThis.TextEncoder = TE;' + LineEnding +
    '  function TD(label, options) {' + LineEnding +
    '    label = label === undefined ? "utf-8" : String(label).toLowerCase().trim();' + LineEnding +
    '    if (label !== "utf-8" && label !== "utf8" && label !== "unicode-1-1-utf-8")' + LineEnding +
    '      throw new RangeError("TextDecoder: only utf-8 is supported");' + LineEnding +
    '    this.encoding = "utf-8";' + LineEnding +
    '    this.fatal = !!(options && options.fatal);' + LineEnding +
    '  }' + LineEnding +
    '  TD.prototype.decode = function (input) {' + LineEnding +
    '    if (input === undefined) return "";' + LineEnding +
    '    var bytes;' + LineEnding +
    '    if (input instanceof Uint8Array) bytes = input;' + LineEnding +
    '    else if (input instanceof ArrayBuffer) bytes = new Uint8Array(input);' + LineEnding +
    '    else if (ArrayBuffer.isView(input)) bytes = new Uint8Array(input.buffer, input.byteOffset, input.byteLength);' + LineEnding +
    '    else bytes = new Uint8Array(input);' + LineEnding +
    '    var out = "", i = 0, b, c, n, j, valid;' + LineEnding +
    '    while (i < bytes.length) {' + LineEnding +
    '      b = bytes[i];' + LineEnding +
    '      if (b < 0x80) { c = b; n = 0; }' + LineEnding +
    '      else if ((b & 0xE0) === 0xC0) { c = b & 31; n = 1; }' + LineEnding +
    '      else if ((b & 0xF0) === 0xE0) { c = b & 15; n = 2; }' + LineEnding +
    '      else if ((b & 0xF8) === 0xF0) { c = b & 7; n = 3; }' + LineEnding +
    '      else {' + LineEnding +
    '        if (this.fatal) throw new TypeError("TextDecoder: invalid UTF-8");' + LineEnding +
    '        out += String.fromCharCode(0xFFFD); i++; continue;' + LineEnding +
    '      }' + LineEnding +
    '      valid = true;' + LineEnding +
    '      for (j = 1; j <= n; j++) {' + LineEnding +
    '        if (i + j >= bytes.length || (bytes[i + j] & 0xC0) !== 0x80) { valid = false; break; }' + LineEnding +
    '        c = (c << 6) | (bytes[i + j] & 63);' + LineEnding +
    '      }' + LineEnding +
    '      if (valid && n === 1 && c < 0x80) valid = false;' + LineEnding +
    '      if (valid && n === 2 && (c < 0x800 || (c >= 0xD800 && c <= 0xDFFF))) valid = false;' + LineEnding +
    '      if (valid && n === 3 && (c < 0x10000 || c > 0x10FFFF)) valid = false;' + LineEnding +
    '      if (!valid) {' + LineEnding +
    '        if (this.fatal) throw new TypeError("TextDecoder: invalid UTF-8");' + LineEnding +
    '        out += String.fromCharCode(0xFFFD); i++; continue;' + LineEnding +
    '      }' + LineEnding +
    '      out += String.fromCodePoint(c);' + LineEnding +
    '      i += n + 1;' + LineEnding +
    '    }' + LineEnding +
    '    return out;' + LineEnding +
    '  };' + LineEnding +
    '  if (typeof globalThis.TextDecoder !== "function") globalThis.TextDecoder = TD;' + LineEnding +
    '})();';

  {** Wraps console.* so non-string arguments are JSON.stringify''d instead of
      collapsing to "[object Object]". Runs after RegisterConsoleLog; no-op in
      contexts without the console object (epUI not granted). }
  CONSOLE_FMT_SHIM: string =
    '(function () {' + LineEnding +
    '  if (typeof console !== "object" || console === null) return;' + LineEnding +
    '  function fmt(a) {' + LineEnding +
    '    if (typeof a === "string") return a;' + LineEnding +
    '    if (a instanceof Error) return (a.name || "Error") + ": " + a.message;' + LineEnding +
    '    if (typeof a === "object" && a !== null) {' + LineEnding +
    '      try {' + LineEnding +
    '        var s = JSON.stringify(a);' + LineEnding +
    '        if (s !== undefined) return s;' + LineEnding +
    '      } catch (e) { }' + LineEnding +
    '    }' + LineEnding +
    '    return String(a);' + LineEnding +
    '  }' + LineEnding +
    '  var names = ["log", "error", "warn", "info", "debug", "push"], i;' + LineEnding +
    '  for (i = 0; i < names.length; i++)' + LineEnding +
    '    (function (name, orig) {' + LineEnding +
    '      if (typeof orig !== "function") return;' + LineEnding +
    '      console[name] = function () {' + LineEnding +
    '        var out = [], j;' + LineEnding +
    '        for (j = 0; j < arguments.length; j++) out.push(fmt(arguments[j]));' + LineEnding +
    '        return orig.apply(console, out);' + LineEnding +
    '      };' + LineEnding +
    '    })(names[i], console[names[i]]);' + LineEnding +
    '})();';

  {** performance.now() (native monotonic clock, µs precision), WebCrypto
      essentials (crypto.getRandomValues/randomUUID over the native AES-PRNG
      hex source) and structuredClone (deep clone incl. Date/RegExp/Map/Set/
      typed arrays, preserves cycles and shared buffers; functions/symbols
      throw TypeError). Guarded so host-provided versions win; the crypto and
      performance parts no-op if their natives are absent. Ungated. }
  MISC_SHIM: string =
    '(function () {' + LineEnding +
    '  if (typeof globalThis.performance !== "object" || globalThis.performance === null)' + LineEnding +
    '    globalThis.performance = {};' + LineEnding +
    '  if (typeof globalThis.performance.now !== "function" && typeof trndiPerfNow === "function") {' + LineEnding +
    '    globalThis.performance.now = function () { return trndiPerfNow(); };' + LineEnding +
    '    globalThis.performance.timeOrigin = Date.now() - trndiPerfNow();' + LineEnding +
    '  }' + LineEnding +
    '  if (!globalThis.crypto && typeof trndiRandomHex === "function") {' + LineEnding +
    '    var randBytes = function (n) {' + LineEnding +
    '      if (n === 0) return new Uint8Array(0);' + LineEnding +
    '      var hx = trndiRandomHex(n), out = new Uint8Array(n), i;' + LineEnding +
    '      if (typeof hx !== "string" || hx.length !== n * 2)' + LineEnding +
    '        throw new Error("crypto: randomness unavailable");' + LineEnding +
    '      for (i = 0; i < n; i++) out[i] = parseInt(hx.substr(i * 2, 2), 16);' + LineEnding +
    '      return out;' + LineEnding +
    '    };' + LineEnding +
    '    globalThis.crypto = {' + LineEnding +
    '      getRandomValues: function (arr) {' + LineEnding +
    '        if (!ArrayBuffer.isView(arr) || arr instanceof Float32Array || arr instanceof Float64Array || arr instanceof DataView)' + LineEnding +
    '          throw new TypeError("getRandomValues: argument must be an integer typed array");' + LineEnding +
    '        if (arr.byteLength > 65536)' + LineEnding +
    '          throw new RangeError("getRandomValues: quota of 65536 bytes exceeded");' + LineEnding +
    '        new Uint8Array(arr.buffer, arr.byteOffset, arr.byteLength).set(randBytes(arr.byteLength));' + LineEnding +
    '        return arr;' + LineEnding +
    '      },' + LineEnding +
    '      randomUUID: function () {' + LineEnding +
    '        var b = randBytes(16), hx = "", i;' + LineEnding +
    '        b[6] = (b[6] & 15) | 64;' + LineEnding +
    '        b[8] = (b[8] & 63) | 128;' + LineEnding +
    '        for (i = 0; i < 16; i++) {' + LineEnding +
    '          hx += (b[i] < 16 ? "0" : "") + b[i].toString(16);' + LineEnding +
    '          if (i === 3 || i === 5 || i === 7 || i === 9) hx += "-";' + LineEnding +
    '        }' + LineEnding +
    '        return hx;' + LineEnding +
    '      }' + LineEnding +
    '    };' + LineEnding +
    '  }' + LineEnding +
    '  if (typeof globalThis.structuredClone !== "function") {' + LineEnding +
    '    var sc = function (v, seen) {' + LineEnding +
    '      if (v === null || typeof v !== "object") {' + LineEnding +
    '        if (typeof v === "function" || typeof v === "symbol")' + LineEnding +
    '          throw new TypeError("structuredClone: a " + typeof v + " cannot be cloned");' + LineEnding +
    '        return v;' + LineEnding +
    '      }' + LineEnding +
    '      if (seen.has(v)) return seen.get(v);' + LineEnding +
    '      var out, i, k, buf;' + LineEnding +
    '      if (v instanceof Date) return new Date(v.getTime());' + LineEnding +
    '      if (v instanceof RegExp) return new RegExp(v.source, v.flags);' + LineEnding +
    '      if (v instanceof ArrayBuffer) {' + LineEnding +
    '        buf = new ArrayBuffer(v.byteLength);' + LineEnding +
    '        new Uint8Array(buf).set(new Uint8Array(v));' + LineEnding +
    '        seen.set(v, buf);' + LineEnding +
    '        return buf;' + LineEnding +
    '      }' + LineEnding +
    '      if (v instanceof DataView)' + LineEnding +
    '        return new DataView(sc(v.buffer, seen), v.byteOffset, v.byteLength);' + LineEnding +
    '      if (ArrayBuffer.isView(v))' + LineEnding +
    '        return new v.constructor(sc(v.buffer, seen), v.byteOffset, v.length);' + LineEnding +
    '      if (v instanceof Map) {' + LineEnding +
    '        out = new Map(); seen.set(v, out);' + LineEnding +
    '        v.forEach(function (val, key) { out.set(sc(key, seen), sc(val, seen)); });' + LineEnding +
    '        return out;' + LineEnding +
    '      }' + LineEnding +
    '      if (v instanceof Set) {' + LineEnding +
    '        out = new Set(); seen.set(v, out);' + LineEnding +
    '        v.forEach(function (val) { out.add(sc(val, seen)); });' + LineEnding +
    '        return out;' + LineEnding +
    '      }' + LineEnding +
    '      if (v instanceof Error) {' + LineEnding +
    '        out = new Error(v.message);' + LineEnding +
    '        out.name = v.name;' + LineEnding +
    '        return out;' + LineEnding +
    '      }' + LineEnding +
    '      if (v instanceof Array) {' + LineEnding +
    '        out = []; seen.set(v, out);' + LineEnding +
    '        for (i = 0; i < v.length; i++) out[i] = sc(v[i], seen);' + LineEnding +
    '        return out;' + LineEnding +
    '      }' + LineEnding +
    '      out = {}; seen.set(v, out);' + LineEnding +
    '      for (k in v)' + LineEnding +
    '        if (Object.prototype.hasOwnProperty.call(v, k)) out[k] = sc(v[k], seen);' + LineEnding +
    '      return out;' + LineEnding +
    '    };' + LineEnding +
    '    globalThis.structuredClone = function (value) { return sc(value, new Map()); };' + LineEnding +
    '  }' + LineEnding +
    '})();';
var
  ctx: JSContext;
begin
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;

  // Trndi.alert/.confirm/.prompt/.select/.log and the bare alert() global → UI group.
  addClassFunctionIf(epUI, 'alert',   ExtFunction(@JSDoAlert), 1);
  addClassFunctionIf(epUI, 'confirm', ExtFunction(@JSDoYesNo), 1);
  addClassFunctionIf(epUI, 'prompt',  ExtFunction(@JSInput),   4);
  addClassFunctionIf(epUI, 'select',  ExtFunction(@JSCombo),  -1);
  addClassFunctionIf(epUI, 'log',     ExtFunction(@JSDoLog),   1);
  addFunctionIf     (epUI, 'alert',   ExtFunction(@JSDoAlert), 1);

  // Timer family → timers group (baseline, always granted today).
  addFunctionIf(epTimers, 'setTimeout',    ExtFunction(@JSSetTimeout),  2);
  addFunctionIf(epTimers, 'setInterval',   ExtFunction(@JSSetInterval), 2);
  addFunctionIf(epTimers, 'clearTimeout',  ExtFunction(@JSClearTimer),  1);
  addFunctionIf(epTimers, 'clearInterval', ExtFunction(@JSClearTimer),  1);

  // console.log/push/logs object — UI group. RegisterConsoleLog writes into ctx directly.
  if CanRegister(epUI) then
    RegisterConsoleLog(@ctx);

  // Native backends for performance.now() and the crypto shim. Permission-free:
  // a monotonic clock and random bytes touch nothing user-sensitive.
  addFunction('trndiPerfNow',   ExtFunction(@JSPerfNow),   0);
  addFunction('trndiRandomHex', ExtFunction(@JSRandomHex), 1);

  // Permission-free polyfills for every context: ES library methods,
  // atob/btoa/queueMicrotask, URL/URLSearchParams/TextEncoder/TextDecoder,
  // performance/crypto/structuredClone, then the console argument formatter
  // (needs console to exist already).
  ExecuteInCurrent(ES_SHIM, '<es shim>');
  ExecuteInCurrent(BASE_SHIM, '<base shim>');
  ExecuteInCurrent(WEB_SHIM, '<web shim>');
  ExecuteInCurrent(MISC_SHIM, '<misc shim>');
  ExecuteInCurrent(CONSOLE_FMT_SHIM, '<console fmt shim>');
end;

{** Free context/resources, dispose registered callbacks, stop timer, and clear singleton. }
destructor TTrndiExtEngine.Destroy;
var
  cb: PJSCallback;
  tmpCtx: JSContext;
  timeoutCounter: integer;
  globalObj, timersObj: JSValueRaw;
  i: integer;
begin
  // ULTRA-EARLY EXIT: If application is terminating or global shutdown flag is set,
  // skip ALL cleanup operations and let OS handle memory deallocation
  if Application.Terminated or IsGlobalShutdown then
  begin
    // Clear references only - no QuickJS API calls whatsoever
    FContext := nil;
    FRuntime := nil;
    FInstance := nil;

    // Free native helper safely without any complex cleanup
    try
      if Assigned(native) then
        FreeAndNil(native);
    except
    end;

    // Clear singleton and exit immediately
    inherited Destroy;
    Exit;
  end;

  // Normal shutdown path - only execute when application is NOT terminating
  // Stop the job-pumping timer ASAP to avoid re-entrancy during teardown
  try
    if Assigned(eventTimer) then
    begin
      eventTimer.Enabled := false;
      FreeAndNil(eventTimer);
    end;
  except
  end;

  // Clean up all active JS timers (Modified: January 16, 2026)
  if Assigned(FJSTimers) then
  try
    while FJSTimers.Count > 0 do
    begin
      DisposeTimerInfo(FJSTimers.Data[0]);
      FJSTimers.Delete(0);
    end;
    FreeAndNil(FJSTimers);
  except
      // Silently handle any errors during timer cleanup
  end;

  // Dispose retired timers that the job pump hadn't drained yet
  try
    DrainDeadTimers;
    FreeAndNil(FDeadTimers);
  except
  end;

  // Signal extension shutdown to background tasks
  SetExtShuttingDown(true);

  // Give background threads time to notice shutdown and complete
  // This is critical to avoid access violations from promise threads
  timeoutCounter := 0;
  while (timeoutCounter < 400) do  // max 2000ms wait (increased from 1000ms)
  begin
    Application.ProcessMessages;
    Sleep(5);
    Inc(timeoutCounter);

    // Early exit if no more threads are likely running
    if timeoutCounter > 100 then  // After 500ms, reduce processing frequency
      Sleep(10);  // Sleep longer to give threads more time to exit
  end;

  // Unregister host promise rejection tracker to avoid callbacks during teardown
  if FRuntime <> nil then
    JS_SetHostPromiseRejectionTracker(FRuntime, nil, nil);

  // Normal shutdown path continues - application is not terminating
  // Drain pending jobs, if any, before freeing context/runtime
  if (FRuntime <> nil) and (FContext <> nil) then
  begin
    // More aggressive job draining with timeout
    timeoutCounter := 0;
    while JS_IsJobPending(FRuntime) and (timeoutCounter < 100) do  // Max 100 iterations
    begin
      if JS_ExecutePendingJob(FRuntime, @tmpCtx) <= 0 then
        Break;
      Inc(timeoutCounter);

      // Give a small pause every few iterations
      if timeoutCounter mod 10 = 0 then
        Sleep(1);
    end;
  end;

  // Force garbage collection to clean up remaining objects
  if (FRuntime <> nil) then
  begin
    // Run garbage collection multiple times to ensure cleanup
    JS_RunGC(FRuntime);
    JS_RunGC(FRuntime);  // Run twice to handle circular references
  end;

  // Dispose dynamically allocated callbacks
  for cb in promises do
    Dispose(cb);
  FreeAndNil(promises);

  for cb in callbacks do
    Dispose(cb);
  FreeAndNil(callbacks);

  // Free per-extension contexts. Each ctx is allocated against FRuntime; we
  // free them here while the runtime is still alive.
  if Assigned(FExtContexts) then
  try
    for i := 0 to FExtContexts.Count - 1 do
      if FExtContexts[i] <> nil then
      begin
        if FExtContexts[i]^.Ctx <> nil then
        try
          JS_FreeContext(FExtContexts[i]^.Ctx);
        except
        end;
        Dispose(FExtContexts[i]);
      end;
    FreeAndNil(FExtContexts);
  except
  end;

  // Free auxiliary structures
  FreeAndNil(knownfunc);

  // Force final garbage collection before freeing context
  if (FRuntime <> nil) then
    JS_RunGC(FRuntime);

  // Normal context cleanup
  if Assigned(FContext) then
  begin
    try
      // Ensure no more jobs are pending before freeing context
      if (FRuntime <> nil) then
      begin
        // Final check for any remaining jobs
        tmpCtx := FContext;
        timeoutCounter := 0;
        while JS_IsJobPending(FRuntime) and (timeoutCounter < 10) do
        begin
          JS_ExecutePendingJob(FRuntime, @tmpCtx);
          Inc(timeoutCounter);
        end;

        // Final garbage collection before context destruction
        JS_RunGC(FRuntime);
      end;

      // Now free the context
      JS_FreeContext(FContext);
    except
      // If context freeing fails, just continue - the runtime cleanup will handle it
    end;
    FContext := nil;
  end;

  // Normal runtime cleanup
  if Assigned(FRuntime) then
  begin
    try
      {$IFDEF DEBUG}
      // In debug builds, skip QuickJS runtime cleanup to avoid debugger issues
      // This prevents debugger stops at JS_FreeRuntime while maintaining clean shutdown
      // The OS will handle memory cleanup when the process terminates
      {$ELSE}
      // Try to run final garbage collection cycles to clean up remaining objects
      JS_RunGC(FRuntime);
      JS_RunGC(FRuntime);  // Multiple passes for circular references

      // Wait a bit more to ensure all cleanup is done
      Sleep(50);

      // Now attempt to free the runtime
      JS_FreeRuntime(FRuntime);
      {$ENDIF}
    except
      on E: Exception do
      begin
        // Log the error for debugging but don't crash
        {$IFDEF DEBUG}
        // Only log in debug mode to avoid user-facing errors
        try
          ExtError(uxdAuto, 'QuickJS Runtime Cleanup', 'Non-critical cleanup error: ' + E.Message);
        except
          // If even error reporting fails, just ignore completely
        end;
        {$ENDIF}
      end;
    end;
    FRuntime := nil;
  end;

  // Free native helper at the very end
  FreeAndNil(native);

  // Clear singleton reference without freeing again
  FInstance := nil;

  inherited Destroy;
end;

{******************************************************************************
  Registration helpers and globals
******************************************************************************}

{** Resolve the active registration target. During BeginRegistration..EndRegistration
    this is the extension context being provisioned; otherwise it is FContext
    (the admin/legacy bootstrap context). }
function TTrndiExtEngine.CurrentRegistrationContext: JSContext;
begin
  if FCurrentRegCtx <> nil then
    Result := FCurrentRegCtx
  else
    Result := FContext;
end;

{** Permission gate. Outside a BeginRegistration block all groups are
    considered granted so legacy/internal registrations still work. }
function TTrndiExtEngine.CanRegister(const grp: TExtPermGroup): boolean;
begin
  if FCurrentRegCtx = nil then
    Exit(true);
  Result := grp in FCurrentRegPerms;
end;

{** Add a global JS function. Writes into the current registration context. }
procedure TTrndiExtEngine.addFunction(const id: string; const func: JSFunction;
const argc: integer = 0);
var
  ctx: JSContext;
begin
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;
  ctx^.SetFunction([], pchar(id), func, argc);
end;

{** Add a JS method under the 'Trndi' class in the current registration context. }
procedure TTrndiExtEngine.addClassFunction(const id: string;
const func: JSFunction; const argc: integer = 0);
var
  this: JSValue;
  ctx: JSContext;
begin
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;
  if not ctx^.GetValue('Trndi', this) then
  begin
    ExtError(uxdAuto, 'Cannot locate the Trndi class while initializing extensions');
    Exit;
  end;

  ctx^.SetFunction(this, pchar(id), func, argc);
end;

{** Gated variant: only register if the permission group is granted. }
procedure TTrndiExtEngine.addClassFunctionIf(const grp: TExtPermGroup;
const id: string; const func: JSFunction; const argc: integer = 0);
begin
  if CanRegister(grp) then
    addClassFunction(id, func, argc);
end;

procedure TTrndiExtEngine.addFunctionIf(const grp: TExtPermGroup;
const id: string; const func: JSFunction; const argc: integer = 0);
begin
  if CanRegister(grp) then
    addFunction(id, func, argc);
end;

{** Create an empty global object named @code(name) in the current registration context. }
procedure TTrndiExtEngine.CreateNewObject(const Name: string);
var
  GlobalObj, JSObject: JSValueRaw;
  ctx: JSContext;
begin
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;
  GlobalObj := JS_GetGlobalObject(ctx);
  JSObject := JS_NewObject(ctx);
  JS_SetPropertyStr(ctx, GlobalObj, pansichar(Name), JSObject);
  // Note: GlobalObj/JSObject lifetime managed by QuickJS; avoid freeing raw values improperly
end;

{** Set a global JS string variable in the current registration context. }
procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8;
const Value: RawUtf8; const obj: string = '');
var
  JValue, GlobalObj: JSValueRaw;
  ctx: JSContext;
begin
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;
  GlobalObj := JS_GetGlobalObject(ctx);
  JValue := JS_NewString(ctx, pansichar(Value));
  JS_SetPropertyStr(ctx, GlobalObj, pansichar(VarName), JValue);
end;

{** Set a global JS int64 variable in the current registration context. }
procedure TTrndiExtEngine.SetGlobalVariable(const VarName: RawUtf8;
const Value: int64; const obj: string = '');
var
  GlobalObj, JValue: JSValueRaw;
  ctx: JSContext;
begin
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;
  GlobalObj := JS_GetGlobalObject(ctx);
  JValue := JS_NewBigInt64(ctx, Value);
  JS_SetPropertyStr(ctx, GlobalObj, pansichar(VarName), JValue);
end;

{******************************************************************************
  Per-extension contexts (Path B isolation)
******************************************************************************}

function TTrndiExtEngine.ExtensionCount: integer;
begin
  if Assigned(FExtContexts) then
    Result := FExtContexts.Count
  else
    Result := 0;
end;

function TTrndiExtEngine.ExtensionAt(idx: integer): PExtContextInfo;
begin
  if (not Assigned(FExtContexts)) or (idx < 0) or (idx >= FExtContexts.Count) then
    Exit(nil);
  Result := FExtContexts[idx];
end;

procedure TTrndiExtEngine.UnloadExtensions;
var
  i, timeoutCounter: integer;
  tmpCtx: JSContext;
begin
  if IsGlobalShutdown or Application.Terminated then Exit;

  // Stop and drop every JS timer (setTimeout/setInterval). All live timers belong
  // to extension contexts since baseline contexts don't schedule any; their
  // held JS values must be released before those contexts die below.
  if Assigned(FJSTimers) then
  try
    while FJSTimers.Count > 0 do
    begin
      DisposeTimerInfo(FJSTimers.Data[0]);
      FJSTimers.Delete(0);
    end;
  except
  end;

  // Dispose retired timers parked for the job pump; their contexts die below.
  try
    DrainDeadTimers;
  except
  end;

  // Signal in-flight promise threads to bail before we free their target ctxs.
  SetExtShuttingDown(true);
  try
    timeoutCounter := 0;
    while timeoutCounter < 200 do  // up to ~1s
    begin
      Application.ProcessMessages;
      Sleep(5);
      Inc(timeoutCounter);
    end;

    // Drain any pending microtasks/promise jobs queued against soon-to-die ctxs.
    if FRuntime <> nil then
    begin
      timeoutCounter := 0;
      while JS_IsJobPending(FRuntime) and (timeoutCounter < 100) do
      begin
        if JS_ExecutePendingJob(FRuntime, @tmpCtx) <= 0 then Break;
        Inc(timeoutCounter);
      end;
      JS_RunGC(FRuntime);
    end;

    // Free per-extension contexts. Runtime stays alive.
    if Assigned(FExtContexts) then
    try
      for i := 0 to FExtContexts.Count - 1 do
        if FExtContexts[i] <> nil then
        begin
          if FExtContexts[i]^.Ctx <> nil then
          try
            JS_FreeContext(FExtContexts[i]^.Ctx);
          except
          end;
          Dispose(FExtContexts[i]);
        end;
      FExtContexts.Clear;
    except
    end;
  finally
    // Re-arm so a subsequent LoadExtensions can register and run scripts again.
    SetExtShuttingDown(false);
  end;
end;

{** Create a fresh extension JSContext on the shared runtime, with intrinsics, the
    'Trndi' class and the opaque engine pointer set up. The new context is appended
    to FExtContexts and returned to the caller, which should call BeginRegistration
    to set it as the active registration target. }
function TTrndiExtEngine.NewExtensionContext(const ExtId, FileName, Hash,
DisplayName, Author: string; Granted: TExtPermSet): PExtContextInfo;
var
  ctx: JSContext;
  Info: PExtContextInfo;
begin
  Result := nil;
  if FRuntime = nil then Exit;

  ctx := JS_NewContext(FRuntime);
  if ctx = nil then Exit;

  // Wire the engine pointer so native callbacks can locate us from a ctx.
  JS_SetContextOpaque(ctx, Self);

  // Standard intrinsics so Promises/RegExp/Date work as in the legacy engine.
  JS_AddIntrinsicPromise(ctx);
  JS_AddIntrinsicRegExp(ctx);
  JS_AddIntrinsicDate(ctx);

  // Expose 'Trndi' constructor in this context's global scope so addClassFunction
  // calls during provisioning can attach methods to Trndi.*
  JS_SetPropertyStr(
    ctx,
    JS_GetGlobalObject(ctx),
    'Trndi',
    JS_NewCFunction2(ctx, PJSCFunction(@TrndiConstructor), 'Trndi', 0,
      JS_CFUNC_constructor, 0)
  );

  New(Info);
  Info^.Ctx := ctx;
  Info^.FileName := FileName;
  Info^.ExtId := ExtId;
  Info^.Hash := Hash;
  Info^.DisplayName := DisplayName;
  Info^.Author := Author;
  Info^.Granted := Granted;

  // Provision engine-internal baselines (alert/confirm/prompt/select/log/timers/console)
  // into this ctx, gated by Granted. Caller will follow up with BeginRegistration to
  // register the umain-level Trndi.* methods and then ExtExecuteFile.
  // Only publish into FExtContexts after baseline provisioning succeeds, so
  // FunctionExists/broadcast loops never observe a half-initialized context.
  try
    BeginRegistration(Info);
    try
      RegisterEngineBaselineForCurrent;
    finally
      EndRegistration;
    end;
  except
    JS_FreeContext(ctx);
    Dispose(Info);
    raise;
  end;

  FExtContexts.Add(Info);

  Result := Info;
end;

procedure TTrndiExtEngine.BeginRegistration(Ext: PExtContextInfo);
begin
  if Ext = nil then Exit;
  FCurrentRegCtx := Ext^.Ctx;
  FCurrentRegPerms := Ext^.Granted;
end;

procedure TTrndiExtEngine.EndRegistration;
begin
  FCurrentRegCtx := nil;
  FCurrentRegPerms := [];
end;

const
  JSIdentChars = ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'];

{** True when a failed eval looks like the bundled quickjspp parser rejecting
    top-level await (it predates JS_EVAL_FLAG_ASYNC, so await is only legal
    inside async functions). Checks for a SyntaxError plus the word 'await'
    anywhere in the source; a false positive is harmless since the wrapped
    retry just fails with an equivalent error. }
function IsTopLevelAwaitError(const Script, Err: RawUtf8): boolean;
var
  p: integer;
begin
  Result := false;
  if Pos('SyntaxError', Err) = 0 then
    Exit;
  p := Pos('await', Script);
  while p > 0 do
  begin
    if ((p = 1) or not (Script[p - 1] in JSIdentChars)) and
      ((p + 5 > Length(Script)) or not (Script[p + 5] in JSIdentChars)) then
      Exit(true);
    p := PosEx('await', Script, p + 1);
  end;
end;

{** Collect 'function name(' / 'async function name(' declarations and return
    JS that copies each onto globalThis. The async-IIFE wrapper turns top-level
    declarations into locals, but Pascal looks callbacks (clockView,
    fetchCallback...) up as global properties, so they must be re-exported.
    Nested functions matched by accident are filtered out at runtime by the
    typeof guard: at the top of the body they are simply not in scope. }
function BuildFunctionHoists(const Script: RawUtf8): RawUtf8;
var
  lines, seen: TStringList;
  line, name: string;
  i, p: integer;
begin
  Result := '';
  lines := TStringList.Create;
  seen := TStringList.Create;
  try
    seen.Sorted := true;
    seen.Duplicates := dupIgnore;
    lines.Text := string(Script);
    for i := 0 to lines.Count - 1 do
    begin
      line := TrimLeft(lines[i]);
      if Copy(line, 1, 6) = 'async ' then
        line := TrimLeft(Copy(line, 7, MaxInt));
      if (Copy(line, 1, 8) <> 'function') or (Length(line) < 9) or
        not (line[9] in [' ', #9, '*']) then
        Continue;
      line := TrimLeft(Copy(line, 9, MaxInt));
      if (line <> '') and (line[1] = '*') then // generator
        line := TrimLeft(Copy(line, 2, MaxInt));
      p := 1;
      while (p <= Length(line)) and (line[p] in JSIdentChars) do
        Inc(p);
      name := Copy(line, 1, p - 1);
      if (name = '') or (name[1] in ['0'..'9']) or
        (Copy(TrimLeft(Copy(line, p, MaxInt)), 1, 1) <> '(') then
        Continue; // anonymous or not a declaration
      if seen.IndexOf(name) >= 0 then
        Continue;
      seen.Add(name);
      Result := Result + RawUtf8('if(typeof ' + name +
        '==="function")globalThis.' + name + '=' + name + ';');
    end;
  finally
    seen.Free;
    lines.Free;
  end;
end;

{** Wrap a script that uses top-level await in an async IIFE so it parses.
    The prefix shares the script's first line so error/stack line numbers are
    unchanged. Function hoists run before the first await so name-based
    callbacks are visible as soon as the extension loads, and a leading
    'use strict' directive is re-emitted since the hoists would otherwise
    push it out of the directive prologue. Rejections that escape the body
    are logged to the extension console when one is registered. }
function WrapTopLevelAwait(const Script: RawUtf8): RawUtf8;
var
  head: RawUtf8;
begin
  head := TrimLeft(Script);
  if (Copy(head, 1, 12) = '"use strict"') or
    (Copy(head, 1, 12) = '''use strict''') then
    head := '"use strict";'
  else
    head := '';
  Result := '(async()=>{' + head + BuildFunctionHoists(Script) + Script +
    #10 + '})().catch(function(e){if(typeof console!=="undefined"&&console.log)' +
    'console.log("Unhandled async error: "+e);});';
end;

{** Read the extension's file and evaluate it in its own context. Mirrors the
    legacy ExecuteFile contract (error string starts with 'Error:'). }
function TTrndiExtEngine.ExtExecuteFile(Ext: PExtContextInfo): RawUtf8;
var
  Script: RawUtf8;
  FileStream: TFileStream;
  StringStream: TStringStream;
  EvalResult: JSValue;
  ResultStr: pansichar;
  err: RawUtf8;
  ctx: JSContext;
begin
  Result := '';
  if (Ext = nil) or IsGlobalShutdown then Exit;
  ctx := Ext^.Ctx;
  if ctx = nil then Exit;

  if not FileExists(Ext^.FileName) then
    raise Exception.CreateFmt(RS_EXT_FILE, [Ext^.FileName]);

  FileStream := TFileStream.Create(Ext^.FileName, fmOpenRead or fmShareDenyWrite);
  StringStream := TStringStream.Create;
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    Script := StringStream.DataString;
  finally
    StringStream.Free;
    FileStream.Free;
  end;

  FOutput := '';
  EvalResult := ctx^.Eval(Script, ExtractFileName(Ext^.FileName),
    JS_EVAL_TYPE_GLOBAL, err);
  // quickjspp has no top-level await; when the parse fails and the script
  // uses await, retry it wrapped in an async IIFE (see WrapTopLevelAwait)
  if EvalResult.IsException and IsTopLevelAwaitError(Script, err) then
  begin
    ctx^.Free(EvalResult);
    EvalResult := ctx^.Eval(WrapTopLevelAwait(Script),
      ExtractFileName(Ext^.FileName), JS_EVAL_TYPE_GLOBAL, err);
  end;
  if EvalResult.IsException then
  try
    ExtError(uxdAuto, 'Error loading', err);
    ResultStr := JS_ToCString(ctx, JS_GetException(ctx));
    Result := 'Error: ' + ResultStr + err;
    JS_FreeCString(ctx, ResultStr);
    ExtError(uxdAuto, analyze(ctx, @evalresult));
  except
    on E: Exception do
      ExtError(uxdAuto, 'An extension''s code resulted in an error: ' + e.message);
  end
  else
  begin
    ResultStr := JS_ToCString(ctx, EvalResult.Raw);
    Result := ResultStr;
    JS_FreeCString(ctx, ResultStr);
  end;
  ctx^.Free(EvalResult);
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
  // Emergency exit if application is shutting down
  if IsGlobalShutdown then
  begin
    Result := '';
    Exit;
  end;

  Result := '';
  if not FileExists(FileName) then
    raise Exception.CreateFmt(RS_EXT_FILE, [FileName]);

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
function TTrndiExtEngine.Execute(const Script: RawUtf8;
Name: string = '<script>'): RawUtf8;
var
  EvalResult: JSValue;
  ResultStr: pansichar;
  err: RawUtf8;
begin
  // Emergency exit if application is shutting down
  if IsGlobalShutdown then
  begin
    Result := '';
    Exit;
  end;

  FOutput := '';

  EvalResult := FContext^.Eval(Script, Name, JS_EVAL_TYPE_GLOBAL, err);

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

{** Evaluate a script in the current registration context. Unlike Execute this
    targets the extension ctx being provisioned, so shims land in the right
    global scope and are gone when that extension's context is freed. }
function TTrndiExtEngine.ExecuteInCurrent(const Script: RawUtf8;
const Name: string = '<shim>'): boolean;
var
  ctx: JSContext;
  EvalResult: JSValue;
  err: RawUtf8;
begin
  Result := false;
  if IsGlobalShutdown then Exit;
  ctx := CurrentRegistrationContext;
  if ctx = nil then Exit;

  EvalResult := ctx^.Eval(Script, Name, JS_EVAL_TYPE_GLOBAL, err);
  Result := not EvalResult.IsException;
  if not Result then
    ExtError(uxdAuto, 'Error loading ' + Name, err);
  ctx^.Free(EvalResult);
end;

{** Call @code(FuncName) on @code(ctx) with string arguments. Returns the call
    result stringified, or '' if the function is missing/not callable. }
function CallStringArgsInContext(ctx: JSContext; const FuncName: RawUtf8;
const Args: JSArray): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  i: integer;
  StrResult: pansichar;
begin
  Result := '';
  if ctx = nil then Exit;
  if not ContextHasFunction(ctx, string(FuncName)) then Exit;

  GlobalObj := JS_GetGlobalObject(ctx);
  FuncObj := JS_GetPropertyStr(ctx, GlobalObj, pchar(FuncName));
  RetVal := JS_UNDEFINED;
  try
    if not JS_IsFunction(ctx, FuncObj) then Exit;

    SetLength(ArgArray, Length(Args));
    for i := 0 to High(Args) do
      ArgArray[i] := JS_NewString(ctx, pchar(Args[i]));

    RetVal := JS_Call(ctx, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);
    if JS_IsError(ctx, RetVal) then
    begin
      js_std_dump_error(ctx);
      Exit;
    end;

    StrResult := JS_ToCString(ctx, RetVal);
    if StrResult <> nil then
    begin
      Result := StrResult;
      JS_FreeCString(ctx, StrResult);
    end;
  finally
    for i := 0 to High(ArgArray) do
      ctx^.FreeInlined(PJSValue(@ArgArray[i]));
    ctx^.FreeInlined(PJSValue(@RetVal));
    ctx^.FreeInlined(PJSValue(@FuncObj));
    ctx^.FreeInlined(PJSValue(@GlobalObj));
  end;
end;

{** Call a global JS function with string arguments; broadcasts across every
    loaded extension context. Returns the last non-empty result (last-writer-wins
    semantics for callbacks like clockView). }
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8;
const Args: JSArray): RawUtf8;
var
  i: integer;
  partial: RawUtf8;
begin
  Result := '';
  if IsExtShuttingDown or (FRuntime = nil) then Exit;

  if Assigned(FExtContexts) and (FExtContexts.Count > 0) then
  begin
    for i := 0 to FExtContexts.Count - 1 do
      if FExtContexts[i] <> nil then
      begin
        partial := CallStringArgsInContext(FExtContexts[i]^.Ctx, FuncName, Args);
        if partial <> '' then Result := partial;
      end;
    Exit;
  end;

  // Legacy fallback: no extensions loaded.
  Result := CallStringArgsInContext(FContext, FuncName, Args);
end;

{** Call @code(FuncName) on @code(ctx) with integer arguments. }
function CallIntArgsInContext(ctx: JSContext; const FuncName: RawUtf8;
const Args: array of integer): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  i: integer;
  StrResult: pansichar;
begin
  Result := '';
  if ctx = nil then Exit;
  if not ContextHasFunction(ctx, string(FuncName)) then Exit;

  GlobalObj := JS_GetGlobalObject(ctx);
  FuncObj := JS_GetPropertyStr(ctx, GlobalObj, pchar(FuncName));
  RetVal := JS_UNDEFINED;
  try
    if not JS_IsFunction(ctx, FuncObj) then Exit;

    SetLength(ArgArray, Length(Args));
    for i := 0 to High(Args) do
      ArgArray[i] := JS_NewBigInt64(ctx, Args[i]);

    RetVal := JS_Call(ctx, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);
    if JS_IsError(ctx, RetVal) then
    begin
      js_std_dump_error(ctx);
      Exit;
    end;

    StrResult := JS_ToCString(ctx, RetVal);
    if StrResult <> nil then
    begin
      Result := StrResult;
      JS_FreeCString(ctx, StrResult);
    end;
  finally
    for i := 0 to High(ArgArray) do
      ctx^.FreeInlined(PJSValue(@ArgArray[i]));
    ctx^.FreeInlined(PJSValue(@RetVal));
    ctx^.FreeInlined(PJSValue(@FuncObj));
    ctx^.FreeInlined(PJSValue(@GlobalObj));
  end;
end;

{** Call a global JS function with integer arguments; broadcasts across loaded
    extension contexts. Integers are marshalled as JS BigInt64. }
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8;
const Args: array of integer): RawUtf8;
var
  i: integer;
  partial: RawUtf8;
begin
  Result := '';
  if IsExtShuttingDown or (FRuntime = nil) then Exit;

  if Assigned(FExtContexts) and (FExtContexts.Count > 0) then
  begin
    for i := 0 to FExtContexts.Count - 1 do
      if FExtContexts[i] <> nil then
      begin
        partial := CallIntArgsInContext(FExtContexts[i]^.Ctx, FuncName, Args);
        if partial <> '' then Result := partial;
      end;
    Exit;
  end;

  Result := CallIntArgsInContext(FContext, FuncName, Args);
end;

{** Marshal an array-of-const into JSValueRaw values bound to @code(ctx) and call
    @code(FuncName). Returns the stringified result or '' on absence/error. }
function CallVarRecArgsInContext(ctx: JSContext; const FuncName: RawUtf8;
const Args: array of const): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  i: integer;
  StrResult: pansichar;
  tmpStrs: array of RawUtf8;
  s: RawUtf8;
  tmpv: JSValue;
begin
  Result := '';
  if ctx = nil then Exit;
  if not ContextHasFunction(ctx, string(FuncName)) then Exit;

  GlobalObj := JS_GetGlobalObject(ctx);
  FuncObj := JS_GetPropertyStr(ctx, GlobalObj, pchar(FuncName));
  RetVal := JS_UNDEFINED;
  try
    if not JS_IsFunction(ctx, FuncObj) then Exit;

    SetLength(ArgArray, Length(Args));
    SetLength(tmpStrs, Length(Args));
    for i := 0 to High(Args) do
      case Args[i].VType of
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
      vtBoolean:
      begin
        tmpv.From(Args[i].VBoolean);
        ArgArray[i] := tmpv.Raw;
      end;
      vtChar:
      begin
        s := RawUtf8(Args[i].VChar);
        tmpStrs[i] := s;
        ArgArray[i] := JS_NewString(ctx, pchar(tmpStrs[i]));
      end;
      vtPChar:
        ArgArray[i] := JS_NewString(ctx, Args[i].VPChar);
      vtAnsiString:
      begin
        s := RawUtf8(ansistring(Args[i].VAnsiString));
        tmpStrs[i] := s;
        ArgArray[i] := JS_NewString(ctx, pchar(tmpStrs[i]));
      end;
      vtUnicodeString, vtWideString:
      begin
        s := RawUtf8(unicodestring(Args[i].VUnicodeString));
        tmpStrs[i] := s;
        ArgArray[i] := JS_NewString(ctx, pchar(tmpStrs[i]));
      end;
      vtString:
      begin
        s := RawUtf8(shortstring(Args[i].VString^));
        tmpStrs[i] := s;
        ArgArray[i] := JS_NewString(ctx, pchar(tmpStrs[i]));
      end;
      else
      begin
        s := RawUtf8('{unsupported}');
        tmpStrs[i] := s;
        ArgArray[i] := JS_NewString(ctx, pchar(tmpStrs[i]));
      end;
      end;

    RetVal := JS_Call(ctx, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);
    if JS_IsError(ctx, RetVal) then
    begin
      js_std_dump_error(ctx);
      Exit;
    end;
    StrResult := JS_ToCString(ctx, RetVal);
    if StrResult <> nil then
    begin
      Result := StrResult;
      JS_FreeCString(ctx, StrResult);
    end;
  finally
    for i := 0 to High(ArgArray) do
      ctx^.FreeInlined(PJSValue(@ArgArray[i]));
    ctx^.FreeInlined(PJSValue(@RetVal));
    ctx^.FreeInlined(PJSValue(@FuncObj));
    ctx^.FreeInlined(PJSValue(@GlobalObj));
  end;
end;

{** Call a global JS function with mixed Pascal arguments (array of const).
    Broadcasts across all loaded extension contexts; returns the last non-empty
    result (last-writer-wins for callbacks with return values). }
function TTrndiExtEngine.CallFunction(const FuncName: RawUtf8;
const Args: array of const): RawUtf8;
var
  i: integer;
  partial: RawUtf8;
begin
  Result := '';
  if IsExtShuttingDown or (FRuntime = nil) then Exit;

  if Assigned(FExtContexts) and (FExtContexts.Count > 0) then
  begin
    for i := 0 to FExtContexts.Count - 1 do
      if FExtContexts[i] <> nil then
      begin
        partial := CallVarRecArgsInContext(FExtContexts[i]^.Ctx, FuncName, Args);
        if partial <> '' then Result := partial;
      end;
    Exit;
  end;

  Result := CallVarRecArgsInContext(FContext, FuncName, Args);
end;

{** Build a JS Array value from an array of const. }
function TTrndiExtEngine.CreateJSArray(const Values: array of const): JSValueRaw;
var
  arr: JSValueRaw;
  i: integer;
  tmp: JSValue;
  s: RawUtf8;
begin
  Result := JS_UNDEFINED;
  tmp.Raw := JS_UNDEFINED;
  if IsExtShuttingDown or (FContext = nil) or (FRuntime = nil) then
    Exit;
  arr := JS_NewArray(FContext);
  for i := 0 to High(Values) do
    case Values[i].VType of
    vtInteger:
    begin
      tmp.From32(Values[i].VInteger);
      JS_SetPropertyUint32(FContext, arr, i, tmp.Raw);
    end;
    vtInt64:
    begin
      tmp.From64(Values[i].VInt64^);
      JS_SetPropertyUint32(FContext, arr, i, tmp.Raw);
    end;
    vtExtended:
    begin
      tmp.FromFloat(Values[i].VExtended^);
      JS_SetPropertyUint32(FContext, arr, i, tmp.Raw);
    end;
    vtBoolean:
    begin
      tmp.From(Values[i].VBoolean);
      JS_SetPropertyUint32(FContext, arr, i, tmp.Raw);
    end;
    vtChar:
    begin
      s := RawUtf8(Values[i].VChar);
      JS_SetPropertyUint32(FContext, arr, i, JS_NewString(FContext, pchar(s)));
    end;
    vtPChar:
      JS_SetPropertyUint32(FContext, arr, i, JS_NewString(FContext, Values[i].VPChar));
    vtAnsiString:
    begin
      s := RawUtf8(ansistring(Values[i].VAnsiString));
      JS_SetPropertyUint32(FContext, arr, i, JS_NewString(FContext, pchar(s)));
    end;
    vtUnicodeString, vtWideString:
    begin
      s := RawUtf8(unicodestring(Values[i].VUnicodeString));
      JS_SetPropertyUint32(FContext, arr, i, JS_NewString(FContext, pchar(s)));
    end;
    vtString:
    begin
      s := RawUtf8(shortstring(Values[i].VString^));
      JS_SetPropertyUint32(FContext, arr, i, JS_NewString(FContext, pchar(s)));
    end;
    else
    begin
      s := RawUtf8('{unsupported}');
      JS_SetPropertyUint32(FContext, arr, i, JS_NewString(FContext, pchar(s)));
    end;
    end;
  Result := arr;
end;

{** Call function passing a single JS array argument. }
function TTrndiExtEngine.CallFunctionWithArrayArg(const FuncName: RawUtf8;
const Values: array of const): RawUtf8;
var
  arr: JSValueRaw;
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  StrResult: pansichar;
  ctx: JSContext;
  i: integer;
begin
  Result := '';
  if IsExtShuttingDown or (FContext = nil) or (FRuntime = nil) then
    Exit;
  if not FunctionExists(string(FuncName)) then
    Exit;

  // Mirror FunctionExists' lookup: prefer the per-extension context (Path B)
  // that actually hosts the function; fall back to FContext for the legacy path.
  ctx := nil;
  if Assigned(FExtContexts) and (FExtContexts.Count > 0) then
    for i := 0 to FExtContexts.Count - 1 do
      if (FExtContexts[i] <> nil) and
        ContextHasFunction(FExtContexts[i]^.Ctx, string(FuncName)) then
      begin
        ctx := FExtContexts[i]^.Ctx;
        Break;
      end;
  if ctx = nil then
    ctx := FContext;
  if ctx = nil then Exit;

  arr := CreateJSArray(Values);
  GlobalObj := JS_GetGlobalObject(ctx);
  FuncObj := JS_GetPropertyStr(ctx, GlobalObj, pchar(FuncName));
  if not JS_IsFunction(ctx, FuncObj) then
    Exit('');
  RetVal := JS_Call(ctx, FuncObj, GlobalObj, 1, @arr);
  if JS_IsError(ctx, RetVal) then
  begin
    js_std_dump_error(ctx);
    Exit('');
  end;
  StrResult := JS_ToCString(ctx, RetVal);
  if StrResult <> nil then
  begin
    Result := StrResult;
    JS_FreeCString(ctx, StrResult);
  end;
end;

// JS value factories
function TTrndiExtEngine.MakeJSString(const S: RawUtf8): JSValueRaw; inline;
begin
  if (FContext = nil) then
    exit(JS_UNDEFINED);
  Result := JS_NewString(FContext, pansichar(S));
end;

function TTrndiExtEngine.MakeJSInt64(const V: int64): JSValueRaw; inline;
begin
  if (FContext = nil) then
    exit(JS_UNDEFINED);
  Result := JS_NewBigInt64(FContext, V);
end;

function TTrndiExtEngine.MakeJSFloat(const V: double): JSValueRaw; inline;
var
  tmp: JSValue;
begin
  if (FContext = nil) then
    exit(JS_UNDEFINED);
  tmp.Raw := JS_UNDEFINED;
  tmp.FromFloat(V);
  Result := tmp.Raw;
end;

function TTrndiExtEngine.MakeJSBool(const V: boolean): JSValueRaw; inline;
var
  tmp: JSValue;
begin
  if (FContext = nil) then
    exit(JS_UNDEFINED);
  tmp.Raw := JS_UNDEFINED;
  tmp.From(V);
  Result := tmp.Raw;
end;

function TTrndiExtEngine.MakeJSArray(const Values: array of const): JSValueRaw; inline;
begin
  Result := CreateJSArray(Values);
end;

{** Call JS with prepared JSValueRaw arguments. }
function TTrndiExtEngine.CallFunctionJS(const FuncName: RawUtf8;
const Args: array of JSValueRaw; autoFree: boolean): RawUtf8;
begin
  // Raw only; no Rest
  Result := InternalCall(FuncName, Args, [], autoFree, false);
end;

{** Call a JS function supplying a pre-built first JS argument (e.g. an Array) followed by
  marshalled Pascal open-array-of-const arguments.
  autoFree      => free the marshalled Rest arguments (default true)
  autoFreeFirst => additionally free the supplied FirstArg (default false) }
function TTrndiExtEngine.CallFunctionArrayFirst(const FuncName: RawUtf8;
const FirstArg: JSValueRaw; const Rest: array of const; autoFree: boolean;
autoFreeFirst: boolean): RawUtf8;
begin
  Result := InternalCall(FuncName, [FirstArg], Rest, autoFreeFirst, autoFree);
end;

{** Call with an array of pre-built JSValueRaw arguments followed by marshalled Pascal args. }
function TTrndiExtEngine.CallFunctionMixed(const FuncName: RawUtf8;
const RawArgs: array of JSValueRaw; const Rest: array of const;
restAutoFree: boolean; rawAutoFree: boolean): RawUtf8;
begin
  Result := InternalCall(FuncName, RawArgs, Rest, rawAutoFree, restAutoFree);
end;

// Internal unified call implementation
function TTrndiExtEngine.InternalCall(const FuncName: RawUtf8;
const RawPrefix: array of JSValueRaw; const Rest: array of const;
freeRaw, freeRest: boolean): RawUtf8;
var
  GlobalObj, FuncObj, RetVal: JSValueRaw;
  ArgArray: array of JSValueRaw;
  tmpStrs: array of RawUtf8;
  tmpv: JSValue;
  i, base: integer;
  s: RawUtf8;
  StrResult: pansichar;
  ctx: JSContext;
begin
  { InternalCall
    Unified backend used by:
      - CallFunctionJS         (raw only)
      - CallFunctionArrayFirst (one raw + marshalled tail)
      - CallFunctionMixed      (N raw + marshalled tail)

    Memory ownership / freeing rules:
      RawPrefix: caller-created JSValueRaw values (via MakeJSString/Int/... or other constructors).
        These are freed here only if freeRaw=true.
      Rest: values provided as "array of const" and marshalled to temporary JS strings/numbers.
        These temporaries are freed here only if freeRest=true.

    Typical usage pattern for callers wanting automatic cleanup of throw-away values:
      - Pass freeRaw=true (or autoFree/autoFreeFirst/rawAutoFree at public wrapper level).
      - Do NOT reuse RawPrefix values after the call when freeRaw=true.

    Reuse scenario:
      - Pass freeRaw=false for any JSValueRaw you intend to call with again later, and free it manually once.

    Edge case note:
      - Do not duplicate the very same JSValueRaw object multiple times inside RawPrefix when freeRaw=true
        (would attempt to free it multiple times). If needed, build separate JS values.
  }
  Result := '';
  if IsExtShuttingDown or (FContext = nil) or (FRuntime = nil) then
    Exit;
  if not FunctionExists(string(FuncName)) then
    Exit;

  // Mirror FunctionExists' lookup: prefer the per-extension context (Path B)
  // that actually hosts the function; fall back to FContext for the legacy path.
  // FExtContexts share FRuntime with FContext, so JSValueRaw values constructed
  // via MakeJS* on FContext remain valid in any ctx on the same runtime.
  ctx := nil;
  if Assigned(FExtContexts) and (FExtContexts.Count > 0) then
    for i := 0 to FExtContexts.Count - 1 do
      if (FExtContexts[i] <> nil) and
        ContextHasFunction(FExtContexts[i]^.Ctx, string(FuncName)) then
      begin
        ctx := FExtContexts[i]^.Ctx;
        Break;
      end;
  if ctx = nil then
    ctx := FContext;
  if ctx = nil then Exit;

  GlobalObj := JS_GetGlobalObject(ctx);
  FuncObj := JS_GetPropertyStr(ctx, GlobalObj, pchar(FuncName));
  if not JS_IsFunction(ctx, FuncObj) then
  begin
    ctx^.FreeInlined(PJSValue(@GlobalObj));
    ctx^.FreeInlined(PJSValue(@FuncObj));
    Exit('');
  end;

  SetLength(ArgArray, Length(RawPrefix) + Length(Rest));
  for i := 0 to High(RawPrefix) do
    ArgArray[i] := RawPrefix[i];
  base := Length(RawPrefix);
  SetLength(tmpStrs, Length(Rest));
  for i := 0 to High(Rest) do
    case Rest[i].VType of
    vtInteger:
    begin
      tmpv.From32(Rest[i].VInteger);
      ArgArray[base + i] := tmpv.Raw;
    end;
    vtInt64:
    begin
      tmpv.From64(Rest[i].VInt64^);
      ArgArray[base + i] := tmpv.Raw;
    end;
    vtExtended:
    begin
      tmpv.FromFloat(Rest[i].VExtended^);
      ArgArray[base + i] := tmpv.Raw;
    end;
    vtBoolean:
    begin
      tmpv.From(Rest[i].VBoolean);
      ArgArray[base + i] := tmpv.Raw;
    end;
    vtChar:
    begin
      s := RawUtf8(Rest[i].VChar);
      tmpStrs[i] := s;
      ArgArray[base + i] := JS_NewString(ctx, pchar(tmpStrs[i]));
    end;
    vtPChar:
      ArgArray[base + i] := JS_NewString(ctx, Rest[i].VPChar);
    vtAnsiString:
    begin
      s := RawUtf8(ansistring(Rest[i].VAnsiString));
      tmpStrs[i] := s;
      ArgArray[base + i] := JS_NewString(ctx, pchar(tmpStrs[i]));
    end;
    vtUnicodeString, vtWideString:
    begin
      s := RawUtf8(unicodestring(Rest[i].VUnicodeString));
      tmpStrs[i] := s;
      ArgArray[base + i] := JS_NewString(ctx, pchar(tmpStrs[i]));
    end;
    vtString:
    begin
      s := RawUtf8(shortstring(Rest[i].VString^));
      tmpStrs[i] := s;
      ArgArray[base + i] := JS_NewString(ctx, pchar(tmpStrs[i]));
    end;
    else
    begin
      s := RawUtf8('{unsupported}');
      tmpStrs[i] := s;
      ArgArray[base + i] := JS_NewString(ctx, pchar(tmpStrs[i]));
    end;
    end;

  if Length(ArgArray) = 0 then
    RetVal := JS_Call(ctx, FuncObj, GlobalObj, 0, nil)
  else
    RetVal := JS_Call(ctx, FuncObj, GlobalObj, Length(ArgArray), @ArgArray[0]);
  if JS_IsError(ctx, RetVal) then
  begin
    js_std_dump_error(ctx);
    Exit('');
  end;
  StrResult := JS_ToCString(ctx, RetVal);
  if StrResult <> nil then
  begin
    Result := StrResult;
    JS_FreeCString(ctx, StrResult);
  end;

  if freeRest then
    for i := base to High(ArgArray) do
      ctx^.FreeInlined(PJSValue(@ArgArray[i]));
  if freeRaw then
    for i := 0 to base - 1 do
      ctx^.FreeInlined(PJSValue(@ArgArray[i]));
end;

{******************************************************************************
  Runtime jobs processing and discovery
******************************************************************************}

{** Pump the QuickJS job queue (Promises/microtasks) on each timer tick.
    JS_ExecutePendingJob writes the context that handled the job into its
    second arg, so we feed it a local — passing &FContext would overwrite the
    admin context with whichever ext ctx ran the job. }
procedure TTrndiExtEngine.OnJSTimer(Sender: TObject);
var
  runCtx: JSContext;
begin
  if IsExtShuttingDown or (FRuntime = nil) then
    Exit;

  // Dispose timers retired since the last tick (never while one is firing).
  DrainDeadTimers;

  runCtx := FContext;
  while JS_IsJobPending(FRuntime) do
    if JS_ExecutePendingJob(FRuntime, @runCtx) <= 0 then
      Break;

  // Rejections that survived a full job-queue drain are genuinely unhandled.
  FlushPendingRejections;
end;

procedure TTrndiExtEngine.NoteRejection(key: UInt64; const msg: string);
var
  i: integer;
begin
  for i := 0 to High(FPendingRejections) do
    if FPendingRejections[i].Key = key then
    begin
      FPendingRejections[i].Msg := msg;
      Exit;
    end;
  SetLength(FPendingRejections, Length(FPendingRejections) + 1);
  FPendingRejections[High(FPendingRejections)].Key := key;
  FPendingRejections[High(FPendingRejections)].Msg := msg;
end;

procedure TTrndiExtEngine.NoteRejectionHandled(key: UInt64);
var
  i, j: integer;
begin
  for i := 0 to High(FPendingRejections) do
    if FPendingRejections[i].Key = key then
    begin
      for j := i to High(FPendingRejections) - 1 do
        FPendingRejections[j] := FPendingRejections[j + 1];
      SetLength(FPendingRejections, Length(FPendingRejections) - 1);
      Exit;
    end;
end;

{** The list is detached before alerting: the modal alert pumps messages,
    which re-enters OnJSTimer and would otherwise report entries twice. }
procedure TTrndiExtEngine.FlushPendingRejections;
var
  pending: array of TPendingRejection;
  i: integer;
begin
  if Length(FPendingRejections) = 0 then
    Exit;
  pending := FPendingRejections;
  FPendingRejections := nil;
  for i := 0 to High(pending) do
    if pending[i].Msg <> '' then
      alert('Unhandled Promise rejection: ' + pending[i].Msg)
    else
      alert('Unhandled Promise rejection: [no error message]');
end;

{** Free retired timers parked by TJSTimerHandler.OnTimer / JSClearTimer.
    Skipped while a timer callback is on the stack: a nested message pump
    (e.g. a modal dialog opened from JS) must not free the very timer whose
    OnTimer frame is still live. }
procedure TTrndiExtEngine.DrainDeadTimers;
var
  info: PJSTimerInfo;
begin
  if (not Assigned(FDeadTimers)) or (FInTimerCallback > 0) then
    Exit;
  while FDeadTimers.Count > 0 do
  begin
    info := FDeadTimers[0];
    FDeadTimers.Delete(0);
    DisposeTimerInfo(info);
  end;
end;

{** Helper: does a specific context expose @code(FuncName) as a callable? }
function ContextHasFunction(ctx: JSContext; const FuncName: string): boolean;
var
  Func: JSValue;
begin
  if ctx = nil then Exit(false);
  if not ctx^.GetValue(pchar(FuncName), func) then
    Exit(false);
  Result := func.IsObject;
end;

{** Determine whether a global function exists and is callable.

    With Path B isolation, user scripts live in per-extension contexts. This
    method returns true if @i(any) loaded extension context exposes the function.
    When no extensions are loaded it falls back to the admin/template context
    (FContext) so engine-internal/test paths continue to work. }
function TTrndiExtEngine.FunctionExists(const FuncName: string): boolean;
var
  i: integer;
begin
  if IsExtShuttingDown or (FRuntime = nil) then
    Exit(false);

  if Assigned(FExtContexts) and (FExtContexts.Count > 0) then
  begin
    for i := 0 to FExtContexts.Count - 1 do
      if (FExtContexts[i] <> nil) and ContextHasFunction(FExtContexts[i]^.Ctx, FuncName) then
        Exit(true);
    Exit(false);
  end;

  // Legacy fallback when no per-extension contexts are loaded.
  Result := ContextHasFunction(FContext, FuncName);
end;

{******************************************************************************
  Singleton management
******************************************************************************}

{** Retrieve or lazily create the singleton engine instance. }
class function TTrndiExtEngine.Instance: TTrndiExtEngine;
begin
  // Don't create new instances during shutdown
  if IsExtShuttingDown then
  begin
    Result := nil;
    Exit;
  end;

  if FInstance = nil then
    FInstance := TTrndiExtEngine.Create;
  Result := FInstance;
end;

{** Release the singleton engine instance. }
class procedure TTrndiExtEngine.ReleaseInstance;
begin
  // Signal shutdown first to prevent recreation
  SetExtShuttingDown(true);

  // Only free if instance exists (safe for multiple calls)
  if FInstance <> nil then
  try
    FreeAndNil(FInstance);
  except
      // Ignore any errors during shutdown cleanup
      // The OS will clean up remaining resources anyway
  end;
end;

end.
