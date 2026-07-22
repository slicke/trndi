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

unit ext_js_tests;

{ Runtime tests for the QuickJS binding (trndi.ext.quickjs).

  These exercise the FFI boundary itself, which is where the binding can fail in
  ways the compiler cannot catch: a JSValue is a 16-byte struct, and how it
  crosses cdecl differs per platform, so every value moves by pointer through
  the C shim. A mismatch there does not produce a type error, it produces a
  crash or silent garbage - hence tests that actually run JavaScript.

  Only trndi.ext.quickjs is under test. The engine layer above it
  (trndi.ext.engine) pulls in the LCL and the permission system, which the
  console runner has no room for. }

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, SysUtils, Variants, trndi.ext.quickjs;

type
  TQuickJSBindingTests = class(TTestCase)
  private
    FRuntime: JSRuntime;
    FContext: JSContext;
    { Evaluate src and return it as a string, failing the test on a JS exception. }
    function EvalToString(const src: RawUtf8): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAbiVersionMatches;
    procedure TestJSValueSizeMatches;
    procedure TestEvalReturnsInteger;
    procedure TestEvalReturnsString;
    procedure TestEvalReturnsFloat;
    procedure TestEvalReturnsBool;
    procedure TestValuePredicates;
    procedure TestExceptionIsReported;
    procedure TestEvalConsumesExceptionOnce;
    procedure TestThrowFromCallIsExceptionNotError;
    procedure TestNativeFunctionIsCalled;
    procedure TestNativeFunctionReceivesArguments;
    procedure TestPropertyRoundTrip;
    procedure TestUtf8RoundTrip;
    procedure TestBigIntToInt64;
    procedure TestBigIntToVariant;
    procedure TestGetValueReferenceIsOwned;
    procedure TestModuleLoaderReturnsModuleDef;
    procedure TestPromiseJobsRun;
  end;

implementation

var
  { Set by the native callbacks below so the tests can observe that the
    trampoline actually reached Pascal. }
  LastNativeCallCount: integer;
  LastNativeArgSum: integer;
  LastNativeArgText: string;

{ ---------------------------------------------------------------------------
  Native callbacks. These are what QuickJS reaches through the C trampoline:
  the result is written through res, so no Pascal function ever returns a
  struct to C.
  --------------------------------------------------------------------------- }

procedure NativePing(res: PJSValue; ctx: JSContext; this_val: PJSValue;
  argc: integer; argv: PJSValues; magic: integer);
begin
  Inc(LastNativeCallCount);
  res^ := JS_NewInt32(ctx, 42);
end;

procedure NativeSum(res: PJSValue; ctx: JSContext; this_val: PJSValue;
  argc: integer; argv: PJSValues; magic: integer);
var
  i, v: longint;
begin
  LastNativeArgSum := 0;
  LastNativeArgText := '';
  for i := 0 to argc - 1 do
  begin
    if JS_IsString(argv^[i]) then
      LastNativeArgText := LastNativeArgText + string(JS_ToUtf8(ctx, argv^[i]))
    else
    begin
      JS_ToInt32(ctx, v, argv^[i]);
      Inc(LastNativeArgSum, v);
    end;
  end;
  res^ := JS_NewInt32(ctx, LastNativeArgSum);
end;

{ ---------------------------------------------------------------------------
  Fixture
  --------------------------------------------------------------------------- }

procedure TQuickJSBindingTests.SetUp;
begin
  LastNativeCallCount := 0;
  LastNativeArgSum := 0;
  LastNativeArgText := '';
  FRuntime := JS_NewRuntime;
  AssertTrue('JS_NewRuntime returned nil', FRuntime <> nil);
  FContext := JS_NewContext(FRuntime);
  AssertTrue('JS_NewContext returned nil', FContext <> nil);
end;

procedure TQuickJSBindingTests.TearDown;
begin
  ClearNatives;
  if FContext <> nil then
    JS_FreeContext(FContext);
  if FRuntime <> nil then
    JS_FreeRuntime(FRuntime);
  FContext := nil;
  FRuntime := nil;
end;

function TQuickJSBindingTests.EvalToString(const src: RawUtf8): string;
var
  v: JSValue;
begin
  v := JS_Eval(FContext, src, 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertFalse('JS threw evaluating: ' + string(src), JS_IsException(v));
    Result := string(JS_ToUtf8(FContext, v));
  finally
    JS_FreeValue(FContext, v);
  end;
end;

{ ---------------------------------------------------------------------------
  ABI
  --------------------------------------------------------------------------- }

procedure TQuickJSBindingTests.TestAbiVersionMatches;
begin
  // The shim and this unit are versioned together; a mismatch means the
  // prebuilt library is older or newer than the binding expects.
  AssertEquals('tq_abi_version', TQ_EXPECTED_ABI, tq_abi_version);
end;

procedure TQuickJSBindingTests.TestJSValueSizeMatches;
begin
  // If these ever disagree, every value crossing the boundary is misread.
  AssertEquals('sizeof(JSValue)', tq_sizeof_jsvalue, SizeOf(JSValue));
end;

{ ---------------------------------------------------------------------------
  Values out of JavaScript
  --------------------------------------------------------------------------- }

procedure TQuickJSBindingTests.TestEvalReturnsInteger;
var
  v: JSValue;
  i: longint;
begin
  v := JS_Eval(FContext, '20 + 22', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertFalse('unexpected exception', JS_IsException(v));
    AssertTrue('expected a number', JS_IsNumber(v));
    JS_ToInt32(FContext, i, v);
    AssertEquals(42, i);
  finally
    JS_FreeValue(FContext, v);
  end;
end;

procedure TQuickJSBindingTests.TestEvalReturnsString;
begin
  AssertEquals('trndi', EvalToString('"trn" + "di"'));
end;

procedure TQuickJSBindingTests.TestEvalReturnsFloat;
var
  v: JSValue;
  d: double;
begin
  v := JS_Eval(FContext, '5.5 / 2', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertFalse('unexpected exception', JS_IsException(v));
    JS_ToFloat64(FContext, d, v);
    AssertEquals(2.75, d, 0.0001);
  finally
    JS_FreeValue(FContext, v);
  end;
end;

procedure TQuickJSBindingTests.TestEvalReturnsBool;
var
  v: JSValue;
begin
  v := JS_Eval(FContext, '1 < 2', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertEquals('JS_ToBool', 1, JS_ToBool(FContext, v));
  finally
    JS_FreeValue(FContext, v);
  end;
end;

procedure TQuickJSBindingTests.TestValuePredicates;
var
  v: JSValue;
begin
  // The predicates return C99 bool (one byte); a wrong binding width here
  // reads adjacent stack and passes or fails at random.
  v := JS_Eval(FContext, '({a: 1})', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertTrue('object', JS_IsObject(v));
    AssertFalse('not a string', JS_IsString(v));
    AssertFalse('not an array', JS_IsArray(v));
    AssertFalse('not an error', JS_IsError(v));
  finally
    JS_FreeValue(FContext, v);
  end;

  v := JS_Eval(FContext, '[1, 2, 3]', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertTrue('array', JS_IsArray(v));
    AssertTrue('array is an object', JS_IsObject(v));
  finally
    JS_FreeValue(FContext, v);
  end;

  AssertTrue('undefined', JS_IsUndefined(JS_UNDEFINED));
  AssertTrue('null', JS_IsNull(JS_NULL));
end;

procedure TQuickJSBindingTests.TestExceptionIsReported;
var
  v, err: JSValue;
  msg: string;
begin
  v := JS_Eval(FContext, 'throw new Error("boom")', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertTrue('expected an exception', JS_IsException(v));
  finally
    JS_FreeValue(FContext, v);
  end;

  err := JS_GetException(FContext);
  try
    AssertTrue('exception should be an Error', JS_IsError(err));
    msg := string(JS_ToUtf8(FContext, err));
    AssertTrue('message should mention boom (got: ' + msg + ')', Pos('boom', msg) > 0);
  finally
    JS_FreeValue(FContext, err);
  end;
end;

procedure TQuickJSBindingTests.TestEvalConsumesExceptionOnce;
var
  v, second: JSValue;
  err: RawUtf8;
  again: string;
begin
  // JSContextHelper.Eval clears the exception and hands back the text, so err is
  // the only surviving copy. The engine used to ask the context a second time
  // and paste the answer into the message the user sees - which is how
  // "Error: [uninitialized]" ended up in front of real error text.
  v := FContext^.Eval('throw new Error("boom")', 'test.js', JS_EVAL_TYPE_GLOBAL, err);
  try
    AssertTrue('expected an exception', v.IsException);
  finally
    JS_FreeValue(FContext, v);
  end;

  AssertTrue('Eval must report the message (got: ' + string(err) + ')',
    Pos('boom', string(err)) > 0);
  AssertTrue('Eval must report the stack too (got: ' + string(err) + ')',
    Pos('test.js', string(err)) > 0);

  // The context is clean afterwards: nothing is left to fetch.
  second := JS_GetException(FContext);
  try
    again := string(JS_ToUtf8(FContext, second));
    AssertFalse('a second fetch must not still be holding the error (got: '
      + again + ')', Pos('boom', again) > 0);
  finally
    JS_FreeValue(FContext, second);
  end;
end;

procedure TQuickJSBindingTests.TestThrowFromCallIsExceptionNotError;
var
  global, fn, ret, err: JSValue;
  msg: string;
begin
  // The distinction every JS_Call site in the engine depends on: a callback
  // that throws hands back the exception marker, NOT an Error object. The
  // broadcast paths used to test this with JS_IsError, which is false here, so
  // the error was dropped silently.
  JS_FreeValue(FContext, JS_Eval(FContext,
    'function boom(){ throw new Error("from JS"); }', 'test.js',
    JS_EVAL_TYPE_GLOBAL));

  global := JS_GetGlobalObject(FContext);
  try
    fn := JS_GetPropertyStr(FContext, global, 'boom');
    try
      AssertTrue('boom should be callable', JS_IsFunction(FContext, fn));
      ret := JS_Call(FContext, fn, global, 0, nil);
      try
        AssertTrue('a throw yields the exception marker', JS_IsException(ret));
        AssertFalse('the marker is not an Error object', JS_IsError(ret));
      finally
        JS_FreeValue(FContext, ret);
      end;
    finally
      JS_FreeValue(FContext, fn);
    end;
  finally
    JS_FreeValue(FContext, global);
  end;

  // And the throw is still pending afterwards: the caller has to take it off
  // the context (DumpJSError does, in the engine) or the next call made there
  // fails with an error it never raised.
  err := JS_GetException(FContext);
  try
    msg := string(JS_ToUtf8(FContext, err));
    AssertTrue('the throw must still be pending (got: ' + msg + ')',
      Pos('from JS', msg) > 0);
  finally
    JS_FreeValue(FContext, err);
  end;
end;

{ ---------------------------------------------------------------------------
  Pascal called from JavaScript
  --------------------------------------------------------------------------- }

procedure TQuickJSBindingTests.TestNativeFunctionIsCalled;
var
  global, fn, v: JSValue;
  i: longint;
begin
  global := JS_GetGlobalObject(FContext);
  try
    fn := JS_NewFunction(FContext, 'ping', 0, RegisterNative('ping', @NativePing));
    // JS_SetPropertyStr consumes fn.
    JS_SetPropertyStr(FContext, global, 'ping', fn);
  finally
    JS_FreeValue(FContext, global);
  end;

  v := JS_Eval(FContext, 'ping()', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertFalse('unexpected exception', JS_IsException(v));
    JS_ToInt32(FContext, i, v);
    AssertEquals('return value from Pascal', 42, i);
  finally
    JS_FreeValue(FContext, v);
  end;
  AssertEquals('native should have been entered once', 1, LastNativeCallCount);
end;

procedure TQuickJSBindingTests.TestNativeFunctionReceivesArguments;
var
  global, fn, v: JSValue;
  i: longint;
begin
  global := JS_GetGlobalObject(FContext);
  try
    fn := JS_NewFunction(FContext, 'sum', 3, RegisterNative('sum', @NativeSum));
    JS_SetPropertyStr(FContext, global, 'sum', fn);
  finally
    JS_FreeValue(FContext, global);
  end;

  v := JS_Eval(FContext, 'sum(1, 2, 3, "x")', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertFalse('unexpected exception', JS_IsException(v));
    JS_ToInt32(FContext, i, v);
    AssertEquals('sum of numeric arguments', 6, i);
  finally
    JS_FreeValue(FContext, v);
  end;
  AssertEquals('string argument reached Pascal', 'x', LastNativeArgText);
end;

{ ---------------------------------------------------------------------------
  Objects and strings
  --------------------------------------------------------------------------- }

procedure TQuickJSBindingTests.TestPropertyRoundTrip;
var
  obj, prop: JSValue;
  i: longint;
begin
  obj := JS_NewObject(FContext);
  try
    JS_SetPropertyStr(FContext, obj, 'answer', JS_NewInt32(FContext, 42));
    JS_SetPropertyStr(FContext, obj, 'name', JS_NewString(FContext, 'trndi'));

    prop := JS_GetPropertyStr(FContext, obj, 'answer');
    try
      JS_ToInt32(FContext, i, prop);
      AssertEquals('numeric property', 42, i);
    finally
      JS_FreeValue(FContext, prop);
    end;

    prop := JS_GetPropertyStr(FContext, obj, 'name');
    try
      AssertEquals('string property', 'trndi', string(JS_ToUtf8(FContext, prop)));
    finally
      JS_FreeValue(FContext, prop);
    end;

    prop := JS_GetPropertyStr(FContext, obj, 'missing');
    try
      AssertTrue('absent property is undefined', JS_IsUndefined(prop));
    finally
      JS_FreeValue(FContext, prop);
    end;
  finally
    JS_FreeValue(FContext, obj);
  end;
end;

procedure TQuickJSBindingTests.TestUtf8RoundTrip;
const
  { Non-ASCII on both sides: strings cross the boundary as byte counts, not
    character counts. Spelled out as bytes so the assertion tests what QuickJS
    emits rather than how FPC decoded this source file.
    'Blodsocker: 5,6 mmol/L - hoegt', with 'oe' as U+00F6 = C3 B6. }
  Text: RawUtf8 = 'Blodsocker: 5,6 mmol/L - h' + #$C3#$B6 + 'gt';
  { 'hoegt <warning sign>': U+00F6 = C3 B6, U+26A0 = E2 9A A0. }
  FromJS: RawUtf8 = 'h' + #$C3#$B6 + 'gt ' + #$E2#$9A#$A0;
var
  v, back: JSValue;
  got: RawUtf8;
begin
  v := JS_NewString(FContext, Text);
  try
    AssertTrue('should be a string', JS_IsString(v));
    got := JS_ToUtf8(FContext, v);
    // Compared as UTF-8, not as `string`: on Windows the latter is CP1252 and
    // the conversion would lose the very bytes under test.
    AssertTrue('round trip (got ' + IntToStr(Length(got)) + ' bytes)', got = Text);
  finally
    JS_FreeValue(FContext, v);
  end;

  // ...and the same text produced by JavaScript itself.
  back := JS_Eval(FContext, '"' + FromJS + '"', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    got := JS_ToUtf8(FContext, back);
    AssertTrue('from JS (got ' + IntToStr(Length(got)) + ' bytes)', got = FromJS);
  finally
    JS_FreeValue(FContext, back);
  end;
end;

{ A BigInt outside the inline payload range is heap-allocated, and the payload
  of a heap value says nothing about its magnitude - JSValue.int64 reads 0 for
  it. QuickJS's own JS_ToInt64 is no help either: it applies ToNumber, which
  throws a TypeError for every BigInt. Hence ctx^.ToInt64. }
procedure TQuickJSBindingTests.TestBigIntToInt64;

  procedure Expect(const src: RawUtf8; wanted: int64);
  var
    v: JSValue;
    got: int64;
  begin
    v := JS_Eval(FContext, src, 'test.js', JS_EVAL_TYPE_GLOBAL);
    try
      AssertTrue(string(src) + ' converts', FContext^.ToInt64(v, got));
      AssertEquals(string(src), wanted, got);
    finally
      JS_FreeValue(FContext, v);
    end;
  end;

  procedure ExpectRejected(const src: RawUtf8);
  var
    v: JSValue;
    got: int64;
  begin
    v := JS_Eval(FContext, src, 'test.js', JS_EVAL_TYPE_GLOBAL);
    try
      AssertFalse(string(src) + ' is rejected', FContext^.ToInt64(v, got));
      AssertEquals(string(src) + ' leaves 0', 0, got);
    finally
      JS_FreeValue(FContext, v);
    end;
  end;

begin
  // Small enough to live inline...
  Expect('42n', 42);
  Expect('-1n', -1);
  // ...and past that, where the value is heap-allocated.
  Expect('2n ** 40n', 1099511627776);
  Expect('-1234567890123n', -1234567890123);
  Expect('9223372036854775807n', High(int64));
  Expect('-9223372036854775808n', Low(int64));

  // Ordinary values still work through the same entry point.
  Expect('123', 123);
  Expect('true', 1);
  Expect('2.75', 2);
  Expect('"77"', 77);

  // Too wide, or not a number at all.
  ExpectRejected('2n ** 100n');
  ExpectRejected('1e300');
  ExpectRejected('({})');
  ExpectRejected('undefined');
  // NaN must not raise: FPC leaves the invalid-operation trap unmasked, so an
  // ordered compare against it would throw EInvalidOp rather than return False.
  ExpectRejected('NaN');

  // None of the above may leave an exception pending on the context.
  AssertEquals('context still usable', '2', EvalToString('1 + 1'));
end;

procedure TQuickJSBindingTests.TestBigIntToVariant;
var
  v: JSValue;
  got: variant;
begin
  // A BigInt has to arrive as a number, not as its string form - natives test
  // VarType before using the value.
  v := JS_Eval(FContext, '2n ** 40n', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    FContext^.ToVariant(v, got);
    AssertEquals('heap BigInt is an Int64 variant', varInt64, VarType(got));
    AssertEquals('heap BigInt value', 1099511627776, int64(got));
  finally
    JS_FreeValue(FContext, v);
  end;

  // Beyond Int64 the exact digits are the only thing a Variant can carry.
  v := JS_Eval(FContext, '2n ** 100n', 'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    FContext^.ToVariant(v, got);
    AssertEquals('too-wide BigInt keeps its digits',
      '1267650600228229401496703205376', string(got));
  finally
    JS_FreeValue(FContext, v);
  end;
end;

procedure TQuickJSBindingTests.TestGetValueReferenceIsOwned;

  { Plant a global function and a WeakRef to it, run probe, then drop the
    global and collect. The WeakRef can still see the function only if some
    reference is outstanding - which is exactly what forgetting to free a
    GetValue result looks like from the JS side. }
  function OutlivesGC(freeIt: boolean): boolean;
  var
    f: JSValue;
    i: integer;
  begin
    EvalToString('globalThis.probeFn = function probeFn() {};' +
      'globalThis.probeRef = new WeakRef(globalThis.probeFn); "ok"');

    for i := 1 to 20 do
      if FContext^.GetValue('probeFn', f) and freeIt then
        JS_FreeValue(FContext, f);

    EvalToString('delete globalThis.probeFn; "ok"');
    // Twice: the first pass can leave cycle members for the second.
    JS_RunGC(FRuntime);
    JS_RunGC(FRuntime);
    Result := EvalToString('String(globalThis.probeRef.deref() !== undefined)') = 'true';
  end;

begin
  // GetValue hands back an owned reference. Nothing in the type system says so,
  // and the leak it causes is invisible until an extension has been broadcast
  // to a few thousand times - so pin the contract here.
  if EvalToString('typeof WeakRef') <> 'function' then
    Ignore('this QuickJS build has no WeakRef; ownership is untestable from JS');

  AssertTrue('dropping the GetValue result should keep the function alive ' +
    '(if this fails the test can no longer detect a leak)', OutlivesGC(false));
  AssertFalse('a freed GetValue result should not keep the function alive',
    OutlivesGC(true));
end;

var
  { Source the test module loader below hands to the compiler, and the name it
    answers to. Set by TestModuleLoaderReturnsModuleDef before the import runs. }
  TestModuleName: string;
  TestModuleSource: RawUtf8;
  TestModuleTag: system.int64;

{ A module loader shaped the way QuickJS expects: it compiles the source and
  returns the JSModuleDef, never the source text. }
function TestModuleLoader(ctx: JSContext; module_name: pansichar;
opaque: pointer): pointer; cdecl;
var
  compiled: JSValue;
begin
  Result := nil;
  if string(module_name) <> TestModuleName then
    Exit;
  compiled := JS_Eval(ctx, TestModuleSource, RawUtf8(TestModuleName),
    JS_EVAL_TYPE_MODULE or JS_EVAL_FLAG_COMPILE_ONLY);
  TestModuleTag := compiled.tag;
  if JS_IsException(compiled) then
    Exit;
  Result := JS_VALUE_GET_PTR(compiled);
  JS_FreeValue(ctx, compiled);
end;

procedure TQuickJSBindingTests.TestModuleLoaderReturnsModuleDef;
var
  v: JSValue;
  jobCtx: JSContext;
  pumped: integer;
begin
  // QuickJS uses the loader's return value as a JSModuleDef*. Returning the
  // module source text instead type-checks (both are pointers) and then
  // segfaults on the first import, so pin the compile-to-JSModuleDef contract.
  TestModuleName := 'trndi-test-module.js';
  TestModuleSource := 'export const answer = 42;';
  TestModuleTag := 0;
  JS_SetModuleLoaderFunc(FRuntime, nil, @TestModuleLoader, nil);

  v := JS_Eval(FContext,
    'import { answer } from "trndi-test-module.js"; globalThis.imported = answer;',
    'test.js', JS_EVAL_TYPE_MODULE);
  try
    AssertFalse('importing should not raise', JS_IsException(v));
  finally
    JS_FreeValue(FContext, v);
  end;

  AssertEquals('a compiled module must carry JS_TAG_MODULE - anything else means '
    + 'the loader is handing back the wrong kind of pointer',
    JS_TAG_MODULE, TestModuleTag);

  // Module evaluation completes through the job queue.
  jobCtx := FContext;
  pumped := 0;
  while JS_IsJobPending(FRuntime) and (pumped < 100) do
  begin
    if JS_ExecutePendingJob(FRuntime, @jobCtx) <= 0 then
      Break;
    Inc(pumped);
  end;

  AssertEquals('the imported binding should be visible', '42',
    EvalToString('String(globalThis.imported)'));
end;

procedure TQuickJSBindingTests.TestPromiseJobsRun;
var
  v: JSValue;
  pumped: integer;
  { QuickJS writes the context that ran the job here; it must be a real
    variable, not nil. }
  jobCtx: JSContext;
begin
  // Promise callbacks only run when the host drains the job queue, which is
  // what the engine's timer loop does; verify the plumbing works here.
  v := JS_Eval(FContext,
    'globalThis.done = 0; Promise.resolve(7).then(function (n) { globalThis.done = n; });',
    'test.js', JS_EVAL_TYPE_GLOBAL);
  try
    AssertFalse('unexpected exception', JS_IsException(v));
  finally
    JS_FreeValue(FContext, v);
  end;

  AssertTrue('a job should be pending', JS_IsJobPending(FRuntime));
  pumped := 0;
  while JS_IsJobPending(FRuntime) and (pumped < 100) do
  begin
    AssertTrue('job raised', JS_ExecutePendingJob(FRuntime, @jobCtx) >= 0);
    Inc(pumped);
  end;

  AssertEquals('then() should have run', '7', EvalToString('String(globalThis.done)'));
end;

initialization
  RegisterTest(TQuickJSBindingTests);
end.
