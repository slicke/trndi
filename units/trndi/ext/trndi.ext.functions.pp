(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2026 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit trndi.ext.functions;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch typehelpers}

interface

{**
  @abstract(Helpers for Trndi extensions and QuickJS bindings.)
  Provides records, enums, and utility types to marshal values between
  QuickJS and Pascal, plus common callback and list helpers used by
  trndi.ext.* units.
}

uses
Classes, SysUtils, mormot.lib.quickjs, mormot.core.base, strutils, fgl,
Dialogs, slicke.ux.alert, Math, types, trndi.strings, trndi.native;

var
  ConsoleBuffer: TStringList;

(*
  Resource strings (can be translated if needed):
    - sNoTrace, sUnknownErr, sStackErrMsg, etc.
*)

type
  {** Forward pointer to @link(JSValueVal) to prevent circular references. }
PJSValueVal = ^JSValueVal;

  {** List of @code(PJSValueVal) pointers (FGL-based list). }
TJSValList = specialize TFPGList<PJSValueVal>;
PJSValList = ^TJSValList;

  {** Enum of supported value kinds, extending QuickJS tags with extras like
      @code(JD_ARRAY), @code(JD_UNSET), etc. }
JDValue = (
  JD_UNINITIALIZED = JS_TAG_UNINITIALIZED, // 0
  JD_INT = JS_TAG_INT,           // 1
  JD_BOOL = JS_TAG_BOOL,          // 2
  JD_NULL = JS_TAG_NULL,          // 3
  JD_F64 = JS_TAG_FLOAT64,       // 7
  JD_OBJ = JS_TAG_OBJECT,        // 8
  JD_STR = JS_TAG_STRING,        // 11
  JD_BFLOAT = JS_TAG_BIG_FLOAT,     // 13
  JD_BINT = JS_TAG_BIG_INT,       // 14
  JD_BDECIMAL = JS_TAG_BIG_DECIMAL,   // 15
  JD_ARRAY = 150,
  JD_UNSET = 151,
  JD_NAN = 152,
  JD_UNASSIGNED = 153,
  JD_UNKNOWN = 154,
  JD_UNDEFINED = 155,
  JD_FUNC = 156
  );

JDValues = set of JDValue;
JDTypes = array of JDValue;

  {** Helper for @link(JDValue). }
{$IFNDEF PASDOC}
JDValueHelper = type helper for JDValue
  function code: integer;
end;
{$ENDIF}

  {** QuickJS value wrapper with typed variants (bigint, float, string, bool,
      array, object...). Provides conversion helpers and array access. }
JSValueVal = record
  Data: packed record
    case match: JDValue of
      JD_BINT: (BigInt: int64);
      JD_BFLOAT: (BigFloat: double);
      JD_BDECIMAL: (BigDecimal: double);
      JD_INT: (Int32Val: int32);
      JD_F64: (FloatVal: double);
      JD_STR: (StrVal: string[255]);
      JD_BOOL: (BoolVal: boolean);
      JD_FUNC: (Func: Pointer);
      JD_OBJ: (ObjectVal: Pointer);
      JD_UNKNOWN: (Parsed: string[255]);
      JD_ARRAY: (ArrayVal: TJSValList);
    end;

    // Returns the JDValue type as a string (e.g., "int", "bool", etc.)
  function valtype: string;

    // Converts the record to a string representation
  function stringify: string;

    // Converts the record to a floating-point, if compatible
  function floatify: double;

    // Converts the record to an integer, if compatible
  function intify: int64;

    // Gets an element from the embedded array by index
  function _a(i: integer): JSValueVal;

    // Enforces that the `val` parameter must match a given JDValue type
    // otherwise fill with defaults or handle as needed.
  function acttype(const val: JSValueVal; want: JDValue): JSValueVal;

    // Returns true if the `self` matches the specified JDValue type.
  function match(a: JDValue): boolean;

    // Raises an exception if the `self` does not match the specified JDValue.
    // Optionally includes the function name (func) and parameter index (ppos).
  function mustbe(a: JDValue; func: string = ''; ppos: integer = -1): boolean;

    // Default property so you can use e.g. myValueVal[i] to get array elements
  property arrval[i: integer]: JSValueVal read _a; default;

    // Equality operator, returns true if they have the same JDValue tag
  class operator =(const a, b: JSValueVal): boolean; overload; inline;

    // Type conversion operators
  class operator explicit(a: JSValueVal): integer; inline;
  class operator explicit(a: JSValueVal): TJSValList; inline;
  class operator explicit(a: JSValueVal): string; inline;
end;

  // Alias for an array of JSValueVal
JSValueValArray = array of JSValueVal;

  {** Simple container for a single @link(JSValueVal) or an array of them. }
JSValueParam = record
  BigInt: int64;
  BigFloat: double;
  BigDecimal: double;
  Int32Val: int32;
  FloatVal: double;
  StrVal: string;
  BoolVal: boolean;
  Func: Pointer;
  ObjectVal: Pointer;
  match: JDValue;
  arrayVal: array of JSValueParam;
end;

  // An array of JSValueParam
JSValuePArray = array of JSValueParam;

  {** Holds one or many @link(JSValueParam) values, tracked by a JDValue tag. }
JSValueParams = record
  match: JDValue;
  Data: array of JSValueParam;
  Count: integer;
  arrayVal: JSValuePArray;
end;

  // Specialized list of JSParameters
JSParameters = TJSValList;
PJSParameters = ^JSParameters;

  {** Callback signature used by extensions to call into Pascal from JS.
      @param(ctx QuickJS context)
      @param(func Function name)
      @param(params Arguments list)
      @param(res Out parameter receiving the result)
      @returns(True on success) }
JSCallbackFunction = function(const ctx: PJSContext; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean of object;
PJSCallbackFunction = ^JSCallbackFunction;

  {
    TJSGValList is a generic TFPList-based container for any type T.
    Exposes GetInline for direct item retrieval.
  }
generic TJSGValList<T> = class(specialize TFPGList<T>)
public
  function GetInline(Index: longint): T; inline;
end;

  {
    TJSCallback is a record storing callback information including:
      - A list (data) of parameters
      - minimum and maximum param counts
      - the function name (func)
      - the callback function pointer
  }
TJSCallback = record
  params: record
    values: packed record
      case boolean of
        true: (Data: JSParameters);
        false: ();
      end;
    min, max: integer;
    end;
  func: string;
  callback: JSCallbackFunction;
  class operator =(const a, b: TJSCallback): boolean; overload; inline;
end;

PJSCallback = ^TJSCallback;

// Function declarations
function JSValueToString(ctx: JSContext; Value: JSValue): string;
function JSValueConstToUtf8(ctx: JSContext; Value: JSValueConst): RawUtf8;
function JSFunctionParams(ctx: JSContext; argc: integer;
argv: PJSValueConstArr): TStringList;
function JSParseValue(ctx: JSContext; val: JSValue): JSValueVal; overload;
function JSParseValue(ctx: JSContext; val: JSValueRaw): JSValueVal; overload;
function JSParseParameters(ctx: JSContext; argc: integer;
argv: PJSValueConstArr): JSParameters; overload;
function JSParseParameters(ctx: JSContext; argc: integer;
argv: PJSValueConst): JSParameters; overload;
function JSDumpObject(ctx: JSContext; obj: JSValueConst; var dump: string): boolean;
function JSTryToString(ctx: JSContext; Data: JSValue; out str: string): boolean;
function JSStringifyValue(val: JSValueVal): string;
function analyze(ctx: JSContext; EvalResult: PJSValue; loop: boolean = false): string;
function JSConsoleLog(ctx: JSContext; this_val: JSValueConst;
argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
function JSConsolePush(ctx: JSContext; this_val: JSValueConst;
argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
function JSConsoleLogs(ctx: JSContext; this_val: JSValueConst;
argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
procedure RegisterConsoleLog(ctx: PJSContext);
function JSValueValToValue(ctx: JSContext; val: JSValueVal): JSValue;
function JSValueValArrayToArray(ctx: JSContext;
val: JSValueVal): JSValueValArray; inline;
function StringToValueVal(const s: string): JSValueVal;
function IntToValueVal(const i: integer): JSValueVal;

function JSValueParamCheck(const params: PJSParameters;
const fmts: array of JDValue): boolean;
function valTypeToStr(val: JDValue): string;
function checkJSParams(params: JSParameters; expect: JDTypes): integer; overload;
function checkJSParams(params: JSParameters; expect, expect2: JDTypes): integer;
overload;

const
JS_TAG_UNKNOWN = -10;
JS_PARAM_MISSMATCH = 150;
JS_PARAM_OK = -1;

implementation

{------------------------------------------------------------------------------
  checkJSParams
  Verifies that the number and types of parameters match the expected types.
  Returns JS_PARAM_OK if correct, or the first mismatched index if not.
-------------------------------------------------------------------------------}
function checkJSParams(params: JSParameters; expect: JDTypes): integer;
var
  i: integer;
begin
  // Check count
  if params.Count <> Length(expect) then
    Exit(JS_PARAM_MISSMATCH);

  // Check each parameter type
  for i := 0 to params.Count - 1 do
    if params[i]^.Data.match <> expect[i] then
      Exit(i);

  Result := JS_PARAM_OK;
end;

function checkJSParams(params: JSParameters; expect, expect2: JDTypes): integer;
var
  i: integer;
begin
  // Check if the count matches either of the expected sets
  if params.Count <> Length(expect) then
    if params.Count <> Length(expect2) then
      Exit(JS_PARAM_MISSMATCH);

  // Check each parameter type
  for i := 0 to params.Count - 1 do
    if params[i]^.Data.match <> expect[i] then
      if params[i]^.Data.match <> expect2[i] then
        Exit(i);

  Result := JS_PARAM_OK;
end;

{ JDValueHelper: returns the ordinal value of the JDValue enum. }
function JDValueHelper.code: integer;
begin
  Result := Ord(self);
end;

{ TJSGValList<T> }

function TJSGValList.GetInline(Index: longint): T;
begin
  Result := Get(Index);
end;

{ JSValueVal }

function JSValueVal.valtype: string;
begin
  Result := valTypeToStr(self.Data.match);
end;

function JSValueVal.intify: int64;
begin
  case self.Data.match of
  JD_BINT:
    Result := self.Data.BigInt;
  JD_INT:
    Result := self.Data.Int32Val;
  JD_STR:
      // Attempt string -> int64
    if not TryStrToInt64(self.Data.StrVal, Result) then
      raise Exception.Create('Could not interpret string as int');
  else
      // For other types, you can choose whether to raise an error or return 0
    raise Exception.Create('Cannot convert this JDValue to integer');
  end;
end;

function JSValueVal.floatify: double;
begin
  case self.Data.match of
  JD_BINT:
    Result := self.Data.BigInt;
  JD_BFLOAT:
    Result := self.Data.BigFloat;
  JD_BDECIMAL:
    Result := self.Data.BigDecimal;
  JD_INT:
    Result := self.Data.Int32Val;
  JD_F64:
    Result := self.Data.FloatVal;
  JD_STR:
      // Attempt string -> float
    if not TryStrToFloat(self.Data.StrVal, Result) then
      raise Exception.Create('Could not interpret string as float');
  else
      // For other types, handle as you see fit
    raise Exception.Create('Cannot convert this JDValue to float');
  end;
end;

function JSValueVal.stringify: string;

  // Helper function for arrays
function doArray(arr: JSParameters; depth: integer): string;
  var
    i: integer;
  begin
    Result := Format('%s'#13#10, [StringOfChar('>', depth)]);
    // Recursively print array items
    for i := 0 to arr.Count - 1 do
      if (arr[i]^).Data.match = JD_ARRAY then
        // If nested array, recurse
        Result := Result + doArray(JSParameters((arr[i]^).Data.ArrayVal), depth + 1)
      else
      if depth > 5 then
      begin
        // Prevent deep recursion
        Result := Result + '-- Reached 5 levels, aborting array printing --'#13#10;
        Break;
      end
      else
        // Print the item, showing type and stringified representation
        Result := Result + Format('%s [%s] %s'#13#10,
          [StringOfChar('>', depth), (arr[i]^).valtype, (arr[i]^).stringify]);
  end;

begin
  // If array, handle specially
  if self.Data.match = JD_ARRAY then
  begin
    Result := doArray(JSParameters(self.Data.ArrayVal), 0);
    Exit;
  end;

  // Otherwise, convert based on match type
  case self.Data.match of
  JD_UNSET:
    Result := 'Unset';
  JD_NAN:
    Result := 'NaN';
  JD_UNASSIGNED:
    Result := 'Unassigned';
  JD_UNKNOWN:
    Result := 'Unknown';
  JD_UNINITIALIZED:
    Result := 'Uninitialized';
  JD_NULL:
    Result := 'null';
  JD_BINT:
    Result := IntToStr(self.Data.BigInt);
  JD_BFLOAT:
    Result := FloatToStr(self.Data.BigFloat);
  JD_BDECIMAL:
    Result := FloatToStr(self.Data.BigDecimal);
  JD_INT:
    Result := IntToStr(self.Data.Int32Val);
  JD_F64:
    Result := FloatToStr(self.Data.FloatVal);
  JD_STR:
    Result := self.Data.StrVal;
  JD_BOOL:
    Result := BoolToStr(self.Data.BoolVal, 'true', 'false');
  JD_OBJ:
    Result := '<object>';
  else
      // Default if not recognized
    Result := 'unknown ' + self.Data.Parsed;
  end;
end;

function JSValueVal._a(i: integer): JSValueVal;
begin
  // Safely retrieve array elements
  if (i >= 0) and (i < self.Data.ArrayVal.Count) then
    Result := (self.Data.ArrayVal[i]^)
  else
    raise EListError.CreateFmt('Index %d out of bounds', [i]);
end;

function JSValueVal.acttype(const val: JSValueVal; want: JDValue): JSValueVal;
begin
  // Example logic: If val matches the wanted type, return it; otherwise return empty
  if val.Data.match = want then
    Result := val
  else
    FillChar(Result, SizeOf(JSValueVal), 0);
end;

function JSValueVal.match(a: JDValue): boolean;
begin
  Result := (self.Data.match = a);
end;

function JSValueVal.mustbe(a: JDValue; func: string = ''; ppos: integer = -1): boolean;
begin
  Result := false;
  if not match(a) then
    if func = '' then
      raise EInvalidCast.Create(Format(sDataTypeErr, [valtype, valTypeToStr(a)]))
    else
    if ppos > -1 then
      raise EInvalidCast.Create(Format(sDataTypeErrPos,
        [valtype, valTypeToStr(a), func, ppos]))
    else
      raise EInvalidCast.Create(Format(sDataTypeErrFunc,
        [valtype, valTypeToStr(a), func]))
  // If mismatch, raise an exception with contextual info
  ;
  Result := true;
end;

class operator JSValueVal.=(const a, b: JSValueVal): boolean;
begin
  // Simple check: compare the JDValue tag
  Result := (a.Data.match = b.Data.match);
end;

class operator JSValueVal.explicit(a: JSValueVal): integer;
begin
  // Convert to integer (prefers Int32Val, else BigInt)
  if a.Data.match = JD_INT then
    Result := a.Data.Int32Val
  else
    Result := a.Data.BigInt;
end;

class operator JSValueVal.explicit(a: JSValueVal): TJSValList;
begin
  // Convert to TJSValList if it is an array; else return nil
  if a.Data.match = JD_ARRAY then
    Result := a.Data.ArrayVal
  else
    Result := nil;
end;

class operator JSValueVal.explicit(a: JSValueVal): string;
begin
  // Stringify the value
  Result := a.stringify;
end;

{ TJSCallback }

class operator TJSCallback.=(const a, b: TJSCallback): boolean;
begin
  // Two TJSCallback records are equal if they have the same func name
  Result := a.func = b.func;
end;

{ Utility Functions }

function JSValueToString(ctx: JSContext; Value: JSValue): string;
begin
  // Convert a JSValue to a UTF-8 string via mORMot's "ToUtf8"
  Result := ctx^.ToUtf8(Value);
end;

function JSValueConstToUtf8(ctx: JSContext; Value: JSValueConst): RawUtf8;
var
  cStr: pansichar;
begin
  Result := '';
  try
    // QuickJS C API call - handle potential errors gracefully
    cStr := JS_ToCString(ctx, Value);
    if cStr <> nil then
    begin
      Result := RawUtf8(cStr);
      JS_FreeCString(ctx, cStr);
    end;
  except
    // If conversion fails, return empty string
    Result := '';
  end;
end;

function JSFunctionParams(ctx: JSContext; argc: integer;
argv: PJSValueConstArr): TStringList;
var
  i: integer;
  argstr: RawUtf8;
begin
  Result := TStringList.Create;
  for i := 0 to argc - 1 do
    if JSValue(argv^[i]).IsString then
    begin
      argStr := JSValueConstToUtf8(ctx, argv^[i]);
      Result.AddPair(IntToStr(i), argStr);
    end
    else
      Result.AddPair(IntToStr(i), '#-type error-#');
end;

function JSParseParameters(ctx: JSContext; argc: integer;
argv: PJSValueConstArr): JSParameters;
var
  i: integer;
  pVal: PJSValueVal;
begin
  Result := TJSValList.Create;
  try
    for i := 0 to argc - 1 do
    begin
      New(pVal);
      pVal^ := JSParseValue(ctx, argv^[i]);
      Result.Add(pVal);
    end;
  except
    // Free allocated pVal pointers in case of exception
    for i := 0 to Result.Count - 1 do
      Dispose(Result[i]);
    Result.Free;
    raise;
  end;
end;

function JSParseParameters(ctx: JSContext; argc: integer;
argv: PJSValueConst): JSParameters;
var
  i: integer;
  pVal: PJSValueVal;
begin
  Result := TJSValList.Create;
  try
    for i := 0 to argc - 1 do
    begin
      New(pVal);
      pVal^ := JSParseValue(ctx, argv[i]);
      Result.Add(pVal);
    end;
  except
    // Free allocated pVal pointers in case of exception
    for i := 0 to Result.Count - 1 do
      Dispose(Result[i]);
    Result.Free;
    raise;
  end;
end;

function JSTryToString(ctx: JSContext; Data: JSValue; out str: string): boolean;
begin
  // Attempt to parse the JSValue as a string
  with JSParseValue(ctx, Data) do
    if Data.match <> JD_STR then
      Result := false
    else
    begin
      str := Data.StrVal;
      Result := true;
    end;
end;

function JSParseValue(ctx: JSContext; val: JSValueRaw): JSValueVal;
begin
  // Overload: parse from JSValueRaw by converting to JSValue
  Result := JSParseValue(ctx, JSValue(val));
end;

function JSParseValue(ctx: JSContext; val: JSValue): JSValueVal;
var
  arrVal: JSValueRaw;
  len, i: integer;
  pVal: PJSValueVal;
begin
  // If val is an object or recognized as an array
  if val.IsObject or (JS_IsArray(ctx, val.Raw) = JS_TRUE) then
  begin
    arrVal := JS_GetPropertyStr(ctx, val.Raw, 'length');
    if (not val.IsUndefined) and (JS_ToInt32(ctx, @len, arrVal) = 0) then
    begin
      // It's an array
      Result.Data.match := JD_ARRAY;
      Result.Data.ArrayVal := TJSValList.Create;

      // Extract elements
      for i := 0 to len - 1 do
      begin
        New(pVal);
        pVal^ := JSParseValue(ctx, JS_GetPropertyUint32(ctx, val.Raw, i));
        Result.Data.ArrayVal.Add(pVal);
      end;
    end
    else
    begin
      // Not an array => treat as an object
      Result.Data.match := JD_OBJ;
      Result.Data.ObjectVal := val.Ptr;
    end;
    Exit;
  end;

  // Handle other types
  if val.NormTag = JS_TAG_BOOL then
  begin
    Result.Data.match := JD_BOOL;
    Result.Data.BoolVal := val.Bool;
  end
  else
  if val.IsBigDecimal then
  begin
    Result.Data.match := JD_BDECIMAL;
    Result.Data.BigDecimal := val.F64;
  end
  else
  if val.IsBigFloat then
  begin
    Result.Data.match := JD_BFLOAT;
    Result.Data.BigFloat := val.F64;
  end
  else
  if val.IsBigInt then
  begin
    Result.Data.match := JD_INT;
    Result.Data.BigInt := val.int64;
  end
  else
  if val.IsInt32 then
  begin
    Result.Data.match := JD_INT;
    Result.Data.Int32Val := val.int32;
  end
  else
  if (val.IsFloat) or (val.NormTag = JS_TAG_FLOAT64) then
  begin
    Result.Data.match := JD_F64;
    Result.Data.FloatVal := val.F64;
  end
  else
  if val.IsString then
  begin
    Result.Data.match := JD_STR;
    Result.Data.StrVal := JSValueToString(ctx, val);
  end
  else
  if val.IsUndefined then
    Result.Data.match := JD_UNDEFINED
  else
  if val.IsObject then
  begin
    Result.Data.match := JD_OBJ;
    Result.Data.ObjectVal := val.Ptr;
  end
  else
  if JS_IsFunction(ctx, val.Raw) then
  begin
    Result.Data.match := JD_FUNC;
    Result.Data.Func := val.Ptr;
  end
  else
  if val.IsNan then
    Result.Data.match := JD_NAN
  else
  if not val.IsUndefined then
    Result.Data.match := JD_UNDEFINED
  else
  if val.IsNull then
    Result.Data.match := JD_NULL
  else
  if not val.IsUninitialized then
    Result.Data.match := JD_UNINITIALIZED
  else
  begin
    // Fallback if none of the above
    Result.Data.Parsed := JSValueToString(ctx, val);
    Result.Data.match := JD_UNKNOWN;
  end;
end;

function JSStringifyValue(val: JSValueVal): string;
var
  s: string;
begin
  // Converts a JSValueVal to a Pascal string, depending on the match type
  case val.Data.match of
  JD_BDECIMAL:
    s := FloatToStr(val.Data.BigDecimal);
  JD_BFLOAT:
    s := FloatToStr(val.Data.BigFloat);
  JD_BINT:
    s := IntToStr(val.Data.BigInt);
  JD_F64:
    s := FloatToStr(val.Data.FloatVal);
  JD_INT:
    s := IntToStr(val.Data.Int32Val);
  JD_STR:
    s := val.Data.StrVal;
  JD_BOOL:
    s := BoolToStr(val.Data.BoolVal);
  else
    s := 'Unknown';
  end;
  Result := s;
end;

function valTypeToStr(val: JDValue): string;
begin
  // Returns a textual description of a JDValue
  case val of
  JD_ARRAY:
    Result := 'array';
  JD_UNSET:
    Result := 'Unset';
  JD_NAN:
    Result := 'NaN';
  JD_UNASSIGNED:
    Result := 'Unassigned';
  JD_UNKNOWN:
    Result := 'Unknown';
  JD_UNINITIALIZED:
    Result := 'Uninitialized';
  JD_NULL:
    Result := 'null';
  JD_BINT:
    Result := 'bigint';
  JD_BFLOAT:
    Result := 'bigfloat';
  JD_BDECIMAL:
    Result := 'bigdecimal';
  JD_INT:
    Result := 'int';
  JD_F64:
    Result := 'float64';
  JD_STR:
    Result := 'string';
  JD_BOOL:
    Result := 'bool';
  JD_OBJ:
    Result := 'object';
  else
    Result := 'unknown';
  end;
end;

function JSValueValToValue(ctx: JSContext; val: JSValueVal): JSValue;

  // Helper function to convert arrays
function doArray(arr: JSParameters; depth: integer): JSValue;
  var
    i: integer;
    retarr: JSValueRaw;
    tmp: JSValue;
  begin
    retarr := JS_NewArray(ctx);

    // Loop all JSValues in the provided array
    for i := 0 to arr.Count - 1 do
      if (arr[i]^).Data.match = JD_ARRAY then
        // Recursively build nested arrays
        JS_SetPropertyUint32(ctx, retarr, i,
          doArray(JSParameters((arr[i]^).Data.ArrayVal), depth + 1).Raw)
      else
      begin
        tmp := JSValueValToValue(ctx, (arr[i]^));
        JS_SetPropertyUint32(ctx, retarr, i, tmp.Raw);
      end;
    Result := JSValue(retarr);
  end;

begin
  // If array, convert each element
  if val.Data.match = JD_ARRAY then
  begin
    Result := doArray(JSParameters(val.Data.ArrayVal), 0);
    Exit;
  end;

  // Otherwise handle each type
  case val.Data.match of
  JD_BINT:
    Result.from64(val.Data.BigInt);
  JD_BFLOAT:
    Result.FromFloat(val.Data.BigFloat);
  JD_BDECIMAL:
    Result.fromFloat(val.Data.BigDecimal);
  JD_INT:
    Result.From32(val.Data.Int32Val);
  JD_F64:
    Result.FromFloat(val.Data.FloatVal);
  JD_STR:
    Result := ctx^.From(val.Data.StrVal);
  JD_BOOL:
    Result.From(val.Data.BoolVal);
  JD_OBJ:
      // If ObjectVal is already a JSValue object pointer
    Result := JSValue(val.Data.ObjectVal);
  else
      // For non-supported types, return `undefined`
    Result := JSValue(JS_UNDEFINED);
  end;
end;

function JSValueValArrayToArray(ctx: JSContext;
val: JSValueVal): JSValueValArray; inline;
var
  i: integer;
begin
  // Convert a TJSValList to a dynamic array of JSValueVal
  SetLength(Result, val.Data.ArrayVal.Count);
  for i := 0 to val.Data.ArrayVal.Count - 1 do
    Result[i] := (val.Data.ArrayVal[i]^);
end;

function JSDumpObject(ctx: JSContext; obj: JSValueConst; var dump: string): boolean;
var
  propEnum: PJSPropertyEnum;
  propCount: uint32;
  i: integer;
  propName: pchar;
begin
  // Retrieve property names from the object
  if JS_GetOwnPropertyNames(ctx, @propEnum, @propCount, obj, JS_GPN_STRING_MASK) = 0 then
  try
    Result := false;
    for i := 0 to propCount - 1 do
    begin
      propName := JS_AtomToCString(ctx, propEnum[i].atom);
      try
        // Write out or handle each property name
        dump := Format('%s%s%s: %s', [IfThen(i = 0, '', #13#10),
          dump, 'Property Name', propName]);
        Result := true;
      finally
        // Free the property name
        JS_FreeCString(ctx, propName);
      end;

      // Free the atom
      JS_FreeAtom(ctx, propEnum[i].atom);
    end;
  finally
    // Free the enumerated property list
    JS_Free(ctx, propEnum);
  end
  else
    Result := false;
end;

function analyze(ctx: JSContext; EvalResult: PJSValue; loop: boolean = false): string;
var
  messageVal, stackVal: JSValue;
  messageStr, stackStr: pchar;
  err: string;
begin
  // Retrieve the exception object and its 'message'
  messageVal := JSValue(JS_GetPropertyStr(ctx, JS_GetException(ctx), pchar('message')));
  if messageVal.IsString then
    messageStr := JS_ToCString(ctx, messageVal.raw)
  else
    messageStr := pchar(sUnknownErr);

  // Retrieve the 'stack' from the exception object if available
  stackVal := JSValue(JS_GetPropertyStr(ctx, JS_GetException(ctx), 'stack'));
  if stackVal.IsString then
    stackStr := JS_ToCString(ctx, stackVal.Raw)
  else
  if not loop then
  begin
    // Attempt to fix if first pass fails
    Result := sStackFailed + analyze(ctx, @stackVal, true);
    Exit;
  end
  else
    stackStr := pchar(sNoTrace);

  try
    // Format the error message
    Result := Format(sStackErrMsg, [messageStr, stackStr, err]);
  except
    // Fallback on internal error
    Result := 'Internal error';
  end;

  // Freed memory / cleanup omitted depending on actual QuickJS usage patterns
end;

function JSConsoleLog(ctx: JSContext; this_val: JSValueConst;
argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
var
  msg: pchar;
  i: integer;
  fullMsg: string;
begin
  // Concatenate all arguments into a single string
  fullMsg := '';
  for i := 0 to argc - 1 do
  begin
    msg := JS_ToCString(ctx, argv^[i]);
    try
      if i > 0 then
        fullMsg := fullMsg + sLineBreak;
      fullMsg := fullMsg + string(msg);
    finally
      JS_FreeCString(ctx, msg);
    end;
  end;

  // Log via external logging function
  if fullMsg = '' then
    fullMsg := sLogEmptyMsg;
  ExtLog(uxdAuto, sLogRecevive, sLogDesc, fullMsg);

  // Return undefined
  Result := JS_UNDEFINED;
end;

function JSConsolePush(ctx: JSContext; this_val: JSValueConst;
argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
var
  msg: pchar;
  i: integer;
  fullMsg: string;
begin
  // Concatenate all arguments into a single string
  fullMsg := '';
  for i := 0 to argc - 1 do
  begin
    msg := JS_ToCString(ctx, argv^[i]);
    try
      if i > 0 then
        fullMsg := fullMsg + ' ';
      fullMsg := fullMsg + string(msg);
    finally
      JS_FreeCString(ctx, msg);
    end;
  end;

  // Add to buffer
  if fullMsg = '' then
    fullMsg := sLogEmptyMsg;
  
  if ConsoleBuffer = nil then
    ConsoleBuffer := TStringList.Create;
  
  ConsoleBuffer.Add(fullMsg);

  // Return undefined
  Result := JS_UNDEFINED;
end;

function JSConsoleLogs(ctx: JSContext; this_val: JSValueConst;
argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
var
  fullMsg: string;
begin
  // Display all buffered messages
  if (ConsoleBuffer <> nil) and (ConsoleBuffer.Count > 0) then
  begin
    fullMsg := ConsoleBuffer.Text;
    ExtLog(uxdAuto, sLogRecevive, sLogDesc, fullMsg);
    ConsoleBuffer.Clear;
  end
  else
  begin
    ShowMessage(sLogNoBuffered);
  end;

  // Return undefined
  Result := JS_UNDEFINED;
end;

procedure RegisterConsoleLog(ctx: PJSContext);
var
  consoleObj, logFunc, pushFunc, logsFunc: JSValueRaw;
begin
  // Create or retrieve the "console" object in the global scope
  consoleObj := JS_GetPropertyStr(ctx^, JS_GetGlobalObject(ctx^), 'console');
  if consoleObj = JS_UNDEFINED then
  begin
    consoleObj := JS_NewObject(ctx^);
    JS_SetPropertyStr(ctx^, JS_GetGlobalObject(ctx^), 'console', consoleObj);
  end;

  // Create a new log function and attach it to "console.log"
  logFunc := JS_NewCFunction(ctx^, PJSCFunction(@JSConsoleLog), 'log', 1);
  JS_SetPropertyStr(ctx^, consoleObj, 'log', logFunc);

  // Create console.push function
  pushFunc := JS_NewCFunction(ctx^, PJSCFunction(@JSConsolePush), 'push', 1);
  JS_SetPropertyStr(ctx^, consoleObj, 'push', pushFunc);

  // Create console.logs function
  logsFunc := JS_NewCFunction(ctx^, PJSCFunction(@JSConsoleLogs), 'logs', 0);
  JS_SetPropertyStr(ctx^, consoleObj, 'logs', logsFunc);

  // Optionally free consoleObj if the QuickJS binding requires it
  // (Commented out as usage may vary depending on the QuickJS binding)
  // JS_Free(ctx^, consoleObj);
end;

function JSValueParamCheck(const params: PJSParameters;
const fmts: array of JDValue): boolean;
var
  i: integer;
  v: JSValueVal;
begin
  // Quick check for count
  if params^.Count <> Length(fmts) then
    Exit(false);

  // Check each parameter's JDValue match
  for i := 0 to params^.Count - 1 do
  begin
    v := (params^[i]^);
    if v.Data.match <> fmts[i] then
      Exit(false);
  end;
  Result := true;
end;

function IntToValueVal(const i: integer): JSValueVal;
begin
  // Create a JSValueVal from an integer
  Result.Data.match := JD_INT;
  Result.Data.Int32Val := i;
end;

function StringToValueVal(const s: string): JSValueVal;
begin
  // Create a JSValueVal from a string
  FillChar(Result, SizeOf(JSValueVal), 0);
  Result.Data.match := JD_STR;
  Result.Data.StrVal := s;
end;

initialization
  ConsoleBuffer := nil;

finalization
  if ConsoleBuffer <> nil then
    ConsoleBuffer.Free;

end.
