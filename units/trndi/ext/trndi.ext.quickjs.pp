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

{** Direct QuickJS binding for Trndi, replacing @code(mormot.lib.quickjs).

    Binds quickjs-ng via two shared libraries: @code(libqjs) (the unmodified
    engine) and @code(tqshim) (a thin C shim, source in @code(externals/quickjs)).

    @bold(Why a shim exists.) quickjs-ng's @code(JSValue) is a 16-byte struct on
    64-bit targets. Passing or returning it by value across an FPC/C @code(cdecl)
    boundary depends on platform struct-classification rules that FPC and GCC are
    not guaranteed to agree on. The shim therefore moves every @code(JSValue)
    across by pointer, and routes JS-to-Pascal calls through a single C
    trampoline so that no Pascal function ever returns a struct to C.

    @bold(Binding rule.) Every @code(JSValue) parameter of a @code(tq_*) import
    is declared as an explicit pointer. Never write @code(const v: JSValue) for
    these - under @code(cdecl) FPC passes a 16-byte record BY VALUE, which the
    shim would then dereference as an address.

    The public @code(JS_*) wrappers below keep the signatures Trndi already used
    under mORMot, so calling code needs no changes.
}
unit trndi.ext.quickjs;

{$mode objfpc}{$H+}
{ Record methods and "record helper for" are not available in plain objfpc. }
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  SysUtils;

type
  {** UTF-8 string type. Compatible with mORMot's @code(RawUtf8). }
  RawUtf8 = UTF8String;

const
  {$IF DEFINED(WINDOWS)}
  {** Engine library. On Windows FPC imports by DLL filename directly. }
  QJSLIB = 'libqjs.dll';
  {** ABI shim library. }
  TQLIB = 'tqshim.dll';
  {$ELSEIF DEFINED(DARWIN)}
  QJSLIB = 'libqjs.dylib';
  TQLIB = 'libtqshim.dylib';
  {$ELSE}
  QJSLIB = 'qjs';
  TQLIB = 'tqshim';
  {$ENDIF}

  {** ABI revision of the shim this unit was written against. }
  TQ_EXPECTED_ABI = 1;

type
  {** Opaque QuickJS runtime record. Only ever used through @code(JSRuntime). }
  JSRuntimeRec = record
    Opaque: byte;
  end;
  {** Handle to a QuickJS runtime. }
  JSRuntime = ^JSRuntimeRec;

  {** Opaque QuickJS context record. Carries the context helper methods. }
  JSContextRec = record
    Opaque: byte;
  end;
  {** Handle to a QuickJS context. }
  JSContext = ^JSContextRec;

  {** Class identifier as used by @code(JS_NewClassID). }
  JSClassID = cardinal;
  {** Interned property name identifier. }
  JSAtom = cardinal;

  {** Payload half of a @code(JSValue). Mirrors quickjs-ng's @code(JSValueUnion). }
  JSValueUnion = record
    case integer of
      0: (int32_: longint);
      1: (float64_: double);
      2: (ptr: pointer);
  end;

  {** A JavaScript value: 16 bytes, payload plus tag.

      Note this is a record, not the scalar QWord mORMot used - mORMot linked a
      NaN-boxed QuickJS fork. Never pass this by value across the C boundary. }
  JSValue = record
    u: JSValueUnion;
    tag: int64;
  end;
  {** Pointer to a single value. }
  PJSValue = ^JSValue;
  {** Retained for source compatibility with the previous mORMot binding. }
  JSValueRaw = JSValue;
  {** Retained for source compatibility with the previous mORMot binding. }
  JSValueConst = JSValue;
  PJSValueRaw = ^JSValue;
  PJSValueConst = ^JSValue;

  {** Open array of values, as handed to callbacks as @code(argv). }
  TJSValueArray = array[0..(MaxInt div SizeOf(JSValue)) - 1] of JSValue;
  {** Pointer to a callback argument array. }
  PJSValues = ^TJSValueArray;

  {** Pointer to a context handle, as used by the engine's callback signatures. }
  PJSContext = ^JSContext;

  {** The resolve/reject pair filled in by @code(JS_NewPromiseCapability).
      Index with @code(JprResolve) and @code(JprReject). }
  JSDoubleVal = array[0..1] of JSValue;

const
  { Value tags. }
  JS_TAG_FIRST = -9;
  JS_TAG_BIG_INT = -9;
  JS_TAG_SYMBOL = -8;
  JS_TAG_STRING = -7;
  JS_TAG_STRING_ROPE = -6;
  JS_TAG_MODULE = -3;
  JS_TAG_FUNCTION_BYTECODE = -2;
  JS_TAG_OBJECT = -1;
  JS_TAG_INT = 0;
  JS_TAG_BOOL = 1;
  JS_TAG_NULL = 2;
  JS_TAG_UNDEFINED = 3;
  JS_TAG_UNINITIALIZED = 4;
  JS_TAG_CATCH_OFFSET = 5;
  JS_TAG_EXCEPTION = 6;
  JS_TAG_SHORT_BIG_INT = 7;
  JS_TAG_FLOAT64 = 8;

  { Eval flags. }
  JS_EVAL_TYPE_GLOBAL = 0;
  JS_EVAL_TYPE_MODULE = 1;
  JS_EVAL_TYPE_MASK = 3;
  JS_EVAL_FLAG_STRICT = 1 shl 3;
  JS_EVAL_FLAG_COMPILE_ONLY = 1 shl 5;
  JS_EVAL_FLAG_BACKTRACE_BARRIER = 1 shl 6;
  JS_EVAL_FLAG_ASYNC = 1 shl 7;

  { Property enumeration flags. }
  JS_GPN_STRING_MASK = 1 shl 0;
  JS_GPN_SYMBOL_MASK = 1 shl 1;
  JS_GPN_PRIVATE_MASK = 1 shl 2;
  JS_GPN_ENUM_ONLY = 1 shl 4;
  JS_GPN_SET_ENUM = 1 shl 5;

  { Indices into JSDoubleVal, matching QuickJS's resolving_funcs order. }
  JprResolve = 0;
  JprReject = 1;

  { Property attribute flags. }
  JS_PROP_CONFIGURABLE = 1 shl 0;
  JS_PROP_WRITABLE = 1 shl 1;
  JS_PROP_ENUMERABLE = 1 shl 2;
  JS_PROP_C_W_E = JS_PROP_CONFIGURABLE or JS_PROP_WRITABLE or JS_PROP_ENUMERABLE;

  { Well-known singleton values. Declared as typed constants so that existing
    code of the form "Result := JS_UNDEFINED" continues to compile unchanged. }
  JS_UNDEFINED: JSValue = (u: (int32_: 0); tag: JS_TAG_UNDEFINED);
  JS_NULL: JSValue = (u: (int32_: 0); tag: JS_TAG_NULL);
  JS_FALSE: JSValue = (u: (int32_: 0); tag: JS_TAG_BOOL);
  JS_TRUE: JSValue = (u: (int32_: 1); tag: JS_TAG_BOOL);
  JS_EXCEPTION: JSValue = (u: (int32_: 0); tag: JS_TAG_EXCEPTION);
  JS_UNINITIALIZED: JSValue = (u: (int32_: 0); tag: JS_TAG_UNINITIALIZED);

type
  {** A Pascal function callable from JavaScript.

      Callbacks never return a @code(JSValue) - they write the result through
      @code(res). Registration happens via @code(RegisterNative), and the
      returned index is handed to QuickJS as the function's @code(magic), which
      the shim trampoline uses to route the call back here.

      @param(res)      Result slot; leave untouched to return @code(undefined))
      @param(ctx)      Calling context)
      @param(this_val) The JavaScript @code(this))
      @param(argc)     Argument count)
      @param(argv)     Argument array, valid for the duration of the call) }
  TJSNativeFunc = procedure(res: PJSValue; ctx: JSContext; this_val: PJSValue;
    argc: integer; argv: PJSValues);

  {** Context helper providing the value conversions the engine code relies on.
      Preserves the @code(ctx^.Method) call shape used with mORMot. }
  JSContextHelper = record helper for JSContextRec
    {** Convert a value to UTF-8, as JavaScript's String() would. }
    function ToUtf8(const v: JSValue): RawUtf8;
    {** Create a JavaScript string value from UTF-8. }
    function From(const s: RawUtf8): JSValue;
    {** Create a numeric value. }
    function FromFloat(d: double): JSValue;
    {** Create an integer value. }
    function FromInt(i: longint): JSValue;
    {** Create a boolean value. }
    function FromBool(b: boolean): JSValue;
    {** Release a value owned by this context. }
    procedure Free(const v: JSValue);
    {** Release a value referenced by pointer.
        Kept for source compatibility with the previous mORMot binding. }
    procedure FreeInlined(v: PJSValue);
  end;

{ ------------------------------------------------------------------ }
{ Runtime and context lifecycle - scalar-only, imported directly.     }
{ ------------------------------------------------------------------ }

function JS_NewRuntime: JSRuntime; cdecl; external QJSLIB;
procedure JS_FreeRuntime(rt: JSRuntime); cdecl; external QJSLIB;
procedure JS_RunGC(rt: JSRuntime); cdecl; external QJSLIB;
function JS_NewContext(rt: JSRuntime): JSContext; cdecl; external QJSLIB;
procedure JS_FreeContext(ctx: JSContext); cdecl; external QJSLIB;
procedure JS_SetContextOpaque(ctx: JSContext; opaque: pointer); cdecl; external QJSLIB;
function JS_GetContextOpaque(ctx: JSContext): pointer; cdecl; external QJSLIB;
function JS_GetRuntime(ctx: JSContext): JSRuntime; cdecl; external QJSLIB;

function JS_AddIntrinsicDate(ctx: JSContext): integer; cdecl; external QJSLIB;
function JS_AddIntrinsicPromise(ctx: JSContext): integer; cdecl; external QJSLIB;
function JS_AddIntrinsicRegExp(ctx: JSContext): integer; cdecl; external QJSLIB;

procedure JS_FreeCString(ctx: JSContext; s: pansichar); cdecl; external QJSLIB;
procedure JS_FreeAtom(ctx: JSContext; a: JSAtom); cdecl; external QJSLIB;
procedure js_free(ctx: JSContext; p: pointer); cdecl; external QJSLIB;

function JS_NewClassID(rt: JSRuntime; pid: PCardinal): JSClassID; cdecl; external QJSLIB;

{ NOTE: quickjs-ng returns C99 bool (one byte) here, hence ByteBool. Binding
  these as LongBool would read three bytes of adjacent garbage. }
function JS_IsJobPending(rt: JSRuntime): ByteBool; cdecl; external QJSLIB;
function JS_ExecutePendingJob(rt: JSRuntime; pctx: pointer): integer; cdecl; external QJSLIB;

{ ------------------------------------------------------------------ }
{ Shim imports. Every JSValue crosses by pointer - see unit header.   }
{ ------------------------------------------------------------------ }

function tq_sizeof_jsvalue: integer; cdecl; external TQLIB;
function tq_abi_version: integer; cdecl; external TQLIB;

{ ------------------------------------------------------------------ }
{ Public API - signatures preserved from the previous mORMot binding. }
{ ------------------------------------------------------------------ }

{** Verify that the loaded shim matches this unit's expectations.
    Raises an exception on mismatch; call once during engine startup. }
procedure CheckQuickJSAbi;

{** Evaluate source text. The caller owns the returned value. }
function JS_Eval(ctx: JSContext; const src, filename: RawUtf8; flags: integer): JSValue;
{** Invoke a callable value. }
function JS_Call(ctx: JSContext; const func, this_obj: JSValue; argc: integer;
  argv: PJSValues): JSValue;
{** Retrieve and clear the pending exception. }
function JS_GetException(ctx: JSContext): JSValue;
{** The global object; caller owns the reference. }
function JS_GetGlobalObject(ctx: JSContext): JSValue;

function JS_NewObject(ctx: JSContext): JSValue;
function JS_NewArray(ctx: JSContext): JSValue;
function JS_NewObjectClass(ctx: JSContext; classID: JSClassID): JSValue;
function JS_NewString(ctx: JSContext; const s: RawUtf8): JSValue; overload;
{** Overload for call sites that already hold a null-terminated buffer. }
function JS_NewString(ctx: JSContext; s: pansichar): JSValue; overload;
function JS_NewInt32(ctx: JSContext; v: longint): JSValue;
function JS_NewInt64(ctx: JSContext; v: int64): JSValue;
function JS_NewFloat64(ctx: JSContext; v: double): JSValue;
function JS_NewBool(ctx: JSContext; v: boolean): JSValue;
function JS_NewBigInt64(ctx: JSContext; v: int64): JSValue;

function JS_GetPropertyStr(ctx: JSContext; const obj: JSValue; const prop: RawUtf8): JSValue;
function JS_GetPropertyUint32(ctx: JSContext; const obj: JSValue; idx: cardinal): JSValue;
{** Set a property. Takes ownership of @code(val). }
function JS_SetPropertyStr(ctx: JSContext; const obj: JSValue; const prop: RawUtf8;
  const val: JSValue): integer;
{** Set an indexed property. Takes ownership of @code(val). }
function JS_SetPropertyUint32(ctx: JSContext; const obj: JSValue; idx: cardinal;
  const val: JSValue): integer;
function JS_SetOpaque(const obj: JSValue; opaque: pointer): integer;

procedure JS_FreeValue(ctx: JSContext; const v: JSValue);
function JS_DupValue(ctx: JSContext; const v: JSValue): JSValue;

function JS_ToCString(ctx: JSContext; const v: JSValue): pansichar;
{** Convert to UTF-8 and release the intermediate C string. }
function JS_ToUtf8(ctx: JSContext; const v: JSValue): RawUtf8;
{** Convert a value to UTF-8. Retained under its mORMot-era name. }
function JSValueConstToUtf8(ctx: JSContext; const v: JSValue): RawUtf8;
{** Stable identity for a heap-allocated value, for use as a dictionary key.

    Under mORMot's NaN-boxed fork a @code(JSValue) could simply be cast to
    @code(UInt64). It is a struct here, so identity comes from the payload
    pointer instead. Only meaningful for reference-counted tags such as
    objects and promises. }
function JSValueId(const v: JSValue): UInt64;
function JS_ToBool(ctx: JSContext; const v: JSValue): integer;
function JS_ToInt32(ctx: JSContext; out res: longint; const v: JSValue): integer;
function JS_ToFloat64(ctx: JSContext; out res: double; const v: JSValue): integer;
function JS_AtomToCString(ctx: JSContext; a: JSAtom): pansichar;

function JS_VALUE_GET_TAG(const v: JSValue): integer; inline;
function JS_VALUE_GET_PTR(const v: JSValue): pointer; inline;
function JS_IsUndefined(const v: JSValue): boolean; inline;
function JS_IsNull(const v: JSValue): boolean; inline;
function JS_IsException(const v: JSValue): boolean;
function JS_IsObject(const v: JSValue): boolean;
function JS_IsString(const v: JSValue): boolean;
function JS_IsNumber(const v: JSValue): boolean;
function JS_IsArray(const v: JSValue): boolean;
function JS_IsError(const v: JSValue): boolean;
function JS_IsFunction(ctx: JSContext; const v: JSValue): boolean;

{** Create a promise plus its resolve/reject pair.
    @param(resolvingFuncs) Receives the two functions; caller owns both. }
function JS_NewPromiseCapability(ctx: JSContext; resolvingFuncs: PJSValues): JSValue;

{ ------------------------------------------------------------------ }
{ Native callback registration                                        }
{ ------------------------------------------------------------------ }

{** Register a Pascal function so JavaScript can call it.
    @returns(The magic index to pass to @code(JS_NewFunction)) }
function RegisterNative(const name: RawUtf8; fn: TJSNativeFunc): integer;
{** Look up a previously registered magic index, or -1. }
function FindNative(const name: RawUtf8): integer;
{** Drop all registrations. Intended for engine teardown and tests. }
procedure ClearNatives;

{** Create a JS-callable function object bound to a registered native. }
function JS_NewFunction(ctx: JSContext; const name: RawUtf8; argCount, magic: integer): JSValue;
{** Create a constructor bound to a registered native. }
function JS_NewConstructor(ctx: JSContext; const name: RawUtf8; argCount, magic: integer): JSValue;
{** Create a function carrying bound data, for async and promise plumbing. }
function JS_NewFunctionData(ctx: JSContext; argCount, magic, dataLen: integer;
  data: PJSValues): JSValue;

implementation

{ --- shim imports ------------------------------------------------- }

procedure tq_undefined(res: PJSValue); cdecl; external TQLIB;
procedure tq_new_bool(res: PJSValue; ctx: JSContext; v: integer); cdecl; external TQLIB;
procedure tq_new_int32(res: PJSValue; ctx: JSContext; v: longint); cdecl; external TQLIB;
procedure tq_new_int64(res: PJSValue; ctx: JSContext; v: int64); cdecl; external TQLIB;
procedure tq_new_float64(res: PJSValue; ctx: JSContext; v: double); cdecl; external TQLIB;
procedure tq_new_bigint64(res: PJSValue; ctx: JSContext; v: int64); cdecl; external TQLIB;
procedure tq_new_string_len(res: PJSValue; ctx: JSContext; s: pansichar;
  len: NativeUInt); cdecl; external TQLIB;
procedure tq_new_object(res: PJSValue; ctx: JSContext); cdecl; external TQLIB;
procedure tq_new_array(res: PJSValue; ctx: JSContext); cdecl; external TQLIB;
procedure tq_new_object_class(res: PJSValue; ctx: JSContext; id: JSClassID); cdecl; external TQLIB;

function tq_tag(v: PJSValue): integer; cdecl; external TQLIB;
function tq_get_ptr(v: PJSValue): pointer; cdecl; external TQLIB;
function tq_is_exception(v: PJSValue): integer; cdecl; external TQLIB;
function tq_is_object(v: PJSValue): integer; cdecl; external TQLIB;
function tq_is_string(v: PJSValue): integer; cdecl; external TQLIB;
function tq_is_number(v: PJSValue): integer; cdecl; external TQLIB;
function tq_is_array(v: PJSValue): integer; cdecl; external TQLIB;
function tq_is_error(v: PJSValue): integer; cdecl; external TQLIB;
function tq_is_function(ctx: JSContext; v: PJSValue): integer; cdecl; external TQLIB;

procedure tq_free_value(ctx: JSContext; v: PJSValue); cdecl; external TQLIB;
procedure tq_dup_value(res: PJSValue; ctx: JSContext; v: PJSValue); cdecl; external TQLIB;

procedure tq_get_property_str(res: PJSValue; ctx: JSContext; obj: PJSValue;
  prop: pansichar); cdecl; external TQLIB;
procedure tq_get_property_uint32(res: PJSValue; ctx: JSContext; obj: PJSValue;
  idx: cardinal); cdecl; external TQLIB;
function tq_set_property_str(ctx: JSContext; obj: PJSValue; prop: pansichar;
  val: PJSValue): integer; cdecl; external TQLIB;
function tq_set_property_uint32(ctx: JSContext; obj: PJSValue; idx: cardinal;
  val: PJSValue): integer; cdecl; external TQLIB;
function tq_set_opaque(obj: PJSValue; opaque: pointer): integer; cdecl; external TQLIB;

function tq_to_cstring(ctx: JSContext; v: PJSValue): pansichar; cdecl; external TQLIB;
function tq_atom_to_cstring(ctx: JSContext; a: JSAtom): pansichar; cdecl; external TQLIB;
function tq_to_bool(ctx: JSContext; v: PJSValue): integer; cdecl; external TQLIB;
function tq_to_int32(ctx: JSContext; pres: PLongint; v: PJSValue): integer; cdecl; external TQLIB;
function tq_to_float64(ctx: JSContext; pres: PDouble; v: PJSValue): integer; cdecl; external TQLIB;

procedure tq_eval(res: PJSValue; ctx: JSContext; input: pansichar; len: NativeUInt;
  filename: pansichar; flags: integer); cdecl; external TQLIB;
procedure tq_call(res: PJSValue; ctx: JSContext; func, this_obj: PJSValue;
  argc: integer; argv: PJSValues); cdecl; external TQLIB;
procedure tq_get_exception(res: PJSValue; ctx: JSContext); cdecl; external TQLIB;
procedure tq_get_global_object(res: PJSValue; ctx: JSContext); cdecl; external TQLIB;
procedure tq_new_promise_capability(res: PJSValue; ctx: JSContext;
  resolving: PJSValues); cdecl; external TQLIB;

procedure tq_new_function(res: PJSValue; ctx: JSContext; name: pansichar;
  len, magic: integer); cdecl; external TQLIB;
procedure tq_new_constructor(res: PJSValue; ctx: JSContext; name: pansichar;
  len, magic: integer); cdecl; external TQLIB;
procedure tq_new_function_data(res: PJSValue; ctx: JSContext; len, magic, dataLen: integer;
  data: PJSValues); cdecl; external TQLIB;

type
  TDispatchFn = procedure(res: PJSValue; ctx: JSContext; this_val: PJSValue;
    argc: integer; argv: PJSValues; magic: integer;
    func_data: PJSValue; func_data_len: integer); cdecl;

procedure tq_set_dispatch(fn: TDispatchFn); cdecl; external TQLIB;

{ ================================================================== }
{ Native registry                                                     }
{ ================================================================== }

type
  TNativeEntry = record
    Name: RawUtf8;
    Func: TJSNativeFunc;
  end;

var
  Natives: array of TNativeEntry;
  DispatchInstalled: boolean = False;

{ The single entry point the C trampoline calls. `magic` indexes Natives. }
procedure NativeDispatch(res: PJSValue; ctx: JSContext; this_val: PJSValue;
  argc: integer; argv: PJSValues; magic: integer;
  func_data: PJSValue; func_data_len: integer); cdecl;
begin
  // The trampoline pre-sets res to undefined, so an out-of-range magic or a
  // callback that writes nothing simply yields undefined rather than garbage.
  if (magic < 0) or (magic > High(Natives)) then
    Exit;
  if not Assigned(Natives[magic].Func) then
    Exit;
  Natives[magic].Func(res, ctx, this_val, argc, argv);
end;

procedure EnsureDispatch;
begin
  if DispatchInstalled then
    Exit;
  tq_set_dispatch(@NativeDispatch);
  DispatchInstalled := True;
end;

function RegisterNative(const name: RawUtf8; fn: TJSNativeFunc): integer;
begin
  EnsureDispatch;
  Result := FindNative(name);
  if Result >= 0 then
  begin
    Natives[Result].Func := fn;
    Exit;
  end;
  Result := Length(Natives);
  SetLength(Natives, Result + 1);
  Natives[Result].Name := name;
  Natives[Result].Func := fn;
end;

function FindNative(const name: RawUtf8): integer;
var
  i: integer;
begin
  for i := 0 to High(Natives) do
    if Natives[i].Name = name then
      Exit(i);
  Result := -1;
end;

procedure ClearNatives;
begin
  SetLength(Natives, 0);
end;

{ ================================================================== }
{ ABI check                                                           }
{ ================================================================== }

procedure CheckQuickJSAbi;
begin
  if tq_sizeof_jsvalue <> SizeOf(JSValue) then
    raise Exception.CreateFmt(
      'QuickJS shim JSValue size mismatch: shim %d, binding %d',
      [tq_sizeof_jsvalue, SizeOf(JSValue)]);
  if tq_abi_version <> TQ_EXPECTED_ABI then
    raise Exception.CreateFmt('QuickJS shim ABI %d, expected %d',
      [tq_abi_version, TQ_EXPECTED_ABI]);
end;

{ ================================================================== }
{ Value construction                                                  }
{ ================================================================== }

function JS_NewObject(ctx: JSContext): JSValue;
begin
  tq_new_object(@Result, ctx);
end;

function JS_NewArray(ctx: JSContext): JSValue;
begin
  tq_new_array(@Result, ctx);
end;

function JS_NewObjectClass(ctx: JSContext; classID: JSClassID): JSValue;
begin
  tq_new_object_class(@Result, ctx, classID);
end;

function JS_NewString(ctx: JSContext; const s: RawUtf8): JSValue;
begin
  tq_new_string_len(@Result, ctx, pansichar(s), Length(s));
end;

function JS_NewString(ctx: JSContext; s: pansichar): JSValue;
begin
  if s = nil then
    tq_new_string_len(@Result, ctx, nil, 0)
  else
    tq_new_string_len(@Result, ctx, s, StrLen(s));
end;

function JS_NewInt32(ctx: JSContext; v: longint): JSValue;
begin
  tq_new_int32(@Result, ctx, v);
end;

function JS_NewInt64(ctx: JSContext; v: int64): JSValue;
begin
  tq_new_int64(@Result, ctx, v);
end;

function JS_NewFloat64(ctx: JSContext; v: double): JSValue;
begin
  tq_new_float64(@Result, ctx, v);
end;

function JS_NewBool(ctx: JSContext; v: boolean): JSValue;
begin
  tq_new_bool(@Result, ctx, ord(v));
end;

function JS_NewBigInt64(ctx: JSContext; v: int64): JSValue;
begin
  tq_new_bigint64(@Result, ctx, v);
end;

{ ================================================================== }
{ Eval and call                                                       }
{ ================================================================== }

function JS_Eval(ctx: JSContext; const src, filename: RawUtf8; flags: integer): JSValue;
begin
  tq_eval(@Result, ctx, pansichar(src), Length(src), pansichar(filename), flags);
end;

function JS_Call(ctx: JSContext; const func, this_obj: JSValue; argc: integer;
  argv: PJSValues): JSValue;
begin
  tq_call(@Result, ctx, @func, @this_obj, argc, argv);
end;

function JS_GetException(ctx: JSContext): JSValue;
begin
  tq_get_exception(@Result, ctx);
end;

function JS_GetGlobalObject(ctx: JSContext): JSValue;
begin
  tq_get_global_object(@Result, ctx);
end;

function JS_NewPromiseCapability(ctx: JSContext; resolvingFuncs: PJSValues): JSValue;
begin
  tq_new_promise_capability(@Result, ctx, resolvingFuncs);
end;

{ ================================================================== }
{ Properties                                                          }
{ ================================================================== }

function JS_GetPropertyStr(ctx: JSContext; const obj: JSValue; const prop: RawUtf8): JSValue;
begin
  tq_get_property_str(@Result, ctx, @obj, pansichar(prop));
end;

function JS_GetPropertyUint32(ctx: JSContext; const obj: JSValue; idx: cardinal): JSValue;
begin
  tq_get_property_uint32(@Result, ctx, @obj, idx);
end;

function JS_SetPropertyStr(ctx: JSContext; const obj: JSValue; const prop: RawUtf8;
  const val: JSValue): integer;
begin
  Result := tq_set_property_str(ctx, @obj, pansichar(prop), @val);
end;

function JS_SetPropertyUint32(ctx: JSContext; const obj: JSValue; idx: cardinal;
  const val: JSValue): integer;
begin
  Result := tq_set_property_uint32(ctx, @obj, idx, @val);
end;

function JS_SetOpaque(const obj: JSValue; opaque: pointer): integer;
begin
  Result := tq_set_opaque(@obj, opaque);
end;

{ ================================================================== }
{ Lifetime                                                            }
{ ================================================================== }

procedure JS_FreeValue(ctx: JSContext; const v: JSValue);
begin
  tq_free_value(ctx, @v);
end;

function JS_DupValue(ctx: JSContext; const v: JSValue): JSValue;
begin
  tq_dup_value(@Result, ctx, @v);
end;

{ ================================================================== }
{ Conversion                                                          }
{ ================================================================== }

function JS_ToCString(ctx: JSContext; const v: JSValue): pansichar;
begin
  Result := tq_to_cstring(ctx, @v);
end;

function JS_ToUtf8(ctx: JSContext; const v: JSValue): RawUtf8;
var
  p: pansichar;
begin
  p := tq_to_cstring(ctx, @v);
  if p = nil then
    Exit('');
  try
    Result := p;
  finally
    JS_FreeCString(ctx, p);
  end;
end;

function JSValueConstToUtf8(ctx: JSContext; const v: JSValue): RawUtf8;
begin
  Result := JS_ToUtf8(ctx, v);
end;

function JSValueId(const v: JSValue): UInt64;
begin
  // Reference-counted tags are negative and carry a pointer payload; anything
  // else has no stable heap identity, so report 0 rather than a stack address.
  if v.tag < 0 then
    Result := UInt64(PtrUInt(v.u.ptr))
  else
    Result := 0;
end;

function JS_ToBool(ctx: JSContext; const v: JSValue): integer;
begin
  Result := tq_to_bool(ctx, @v);
end;

function JS_ToInt32(ctx: JSContext; out res: longint; const v: JSValue): integer;
begin
  Result := tq_to_int32(ctx, @res, @v);
end;

function JS_ToFloat64(ctx: JSContext; out res: double; const v: JSValue): integer;
begin
  Result := tq_to_float64(ctx, @res, @v);
end;

function JS_AtomToCString(ctx: JSContext; a: JSAtom): pansichar;
begin
  Result := tq_atom_to_cstring(ctx, a);
end;

{ ================================================================== }
{ Inspection                                                          }
{ ================================================================== }

function JS_VALUE_GET_TAG(const v: JSValue): integer;
begin
  Result := v.tag;
end;

function JS_VALUE_GET_PTR(const v: JSValue): pointer;
begin
  Result := v.u.ptr;
end;

function JS_IsUndefined(const v: JSValue): boolean;
begin
  Result := v.tag = JS_TAG_UNDEFINED;
end;

function JS_IsNull(const v: JSValue): boolean;
begin
  Result := v.tag = JS_TAG_NULL;
end;

function JS_IsException(const v: JSValue): boolean;
begin
  Result := tq_is_exception(@v) <> 0;
end;

function JS_IsObject(const v: JSValue): boolean;
begin
  Result := tq_is_object(@v) <> 0;
end;

function JS_IsString(const v: JSValue): boolean;
begin
  Result := tq_is_string(@v) <> 0;
end;

function JS_IsNumber(const v: JSValue): boolean;
begin
  Result := tq_is_number(@v) <> 0;
end;

function JS_IsArray(const v: JSValue): boolean;
begin
  Result := tq_is_array(@v) <> 0;
end;

function JS_IsError(const v: JSValue): boolean;
begin
  Result := tq_is_error(@v) <> 0;
end;

function JS_IsFunction(ctx: JSContext; const v: JSValue): boolean;
begin
  Result := tq_is_function(ctx, @v) <> 0;
end;

{ ================================================================== }
{ Function objects                                                    }
{ ================================================================== }

function JS_NewFunction(ctx: JSContext; const name: RawUtf8; argCount, magic: integer): JSValue;
begin
  EnsureDispatch;
  tq_new_function(@Result, ctx, pansichar(name), argCount, magic);
end;

function JS_NewConstructor(ctx: JSContext; const name: RawUtf8; argCount, magic: integer): JSValue;
begin
  EnsureDispatch;
  tq_new_constructor(@Result, ctx, pansichar(name), argCount, magic);
end;

function JS_NewFunctionData(ctx: JSContext; argCount, magic, dataLen: integer;
  data: PJSValues): JSValue;
begin
  EnsureDispatch;
  tq_new_function_data(@Result, ctx, argCount, magic, dataLen, data);
end;

{ ================================================================== }
{ Context helper                                                      }
{ ================================================================== }

function JSContextHelper.ToUtf8(const v: JSValue): RawUtf8;
begin
  Result := JS_ToUtf8(@Self, v);
end;

function JSContextHelper.From(const s: RawUtf8): JSValue;
begin
  Result := JS_NewString(@Self, s);
end;

function JSContextHelper.FromFloat(d: double): JSValue;
begin
  Result := JS_NewFloat64(@Self, d);
end;

function JSContextHelper.FromInt(i: longint): JSValue;
begin
  Result := JS_NewInt32(@Self, i);
end;

function JSContextHelper.FromBool(b: boolean): JSValue;
begin
  Result := JS_NewBool(@Self, b);
end;

procedure JSContextHelper.Free(const v: JSValue);
begin
  JS_FreeValue(@Self, v);
end;

procedure JSContextHelper.FreeInlined(v: PJSValue);
begin
  if v <> nil then
    JS_FreeValue(@Self, v^);
end;

end.
