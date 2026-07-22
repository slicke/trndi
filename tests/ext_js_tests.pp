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
  fpcunit, testregistry, SysUtils, trndi.ext.quickjs;

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
    procedure TestNativeFunctionIsCalled;
    procedure TestNativeFunctionReceivesArguments;
    procedure TestPropertyRoundTrip;
    procedure TestUtf8RoundTrip;
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
