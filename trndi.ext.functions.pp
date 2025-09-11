(*
  trndi.ext.functions
  ------------------------------------------------------------------
  This unit provides extended functionality for JavaScript bindings
  (using QuickJS via mORMot) and includes helper types, records,
  constants, and various utility functions for parsing and handling
  JSValue objects.

  Dependencies:
    - mormot.lib.quickjs
    - mormot.core.base
    - TFPGList-based collections
    - Slicke.ux.alert for logging

  Author:    Björn Lindh (github.com/slicke)
  License:   GNU General Public License (v3)
  Repository: https://github.com/slicke/trndi
*)

unit trndi.ext.functions;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses
Classes, SysUtils, mormot.lib.quickjs, mormot.core.base, strutils, fgl,
Dialogs, slicke.ux.alert, Math, types, trndi.strings, trndi.native;

(*
  Resource strings in Swedish (can be translated if needed):
    - sNoTrace, sUnknownErr, sStackErrMsg, etc.
*)

type
  {
    Forward declaration to prevent circular references.
    PJSValueVal is a pointer to a JSValueVal record.
  }
PJSValueVal = ^JSValueVal;

  {
    TJSValList is a list of PJSValueVal pointers (FGL-based list).
  }
TJSValList = specialize TFPGList<PJSValueVal>;
PJSValList = ^TJSValList;

  {
    JDValue is an enum that extends QuickJS tags to cover additional
    custom types, such as JD_ARRAY, JD_UNSET, etc.
  }
JDValue = (
  JD_UNINITIALIZED = JS_TAG_UNINITIALIZED, // 0
  JD_INT           = JS_TAG_INT,           // 1
  JD_BOOL          = JS_TAG_BOOL,          // 2
  JD_NULL          = JS_TAG_NULL,          // 3
  JD_F64           = JS_TAG_FLOAT64,       // 7
  JD_OBJ           = JS_TAG_OBJECT,        // 8
  JD_STR           = JS_TAG_STRING,        // 11
  JD_BFLOAT        = JS_TAG_BIG_FLOAT,     // 13
  JD_BINT          = JS_TAG_BIG_INT,       // 14
  JD_BDECIMAL      = JS_TAG_BIG_DECIMAL,   // 15
  JD_ARRAY         = 150,
  JD_UNSET         = 151,
  JD_NAN           = 152,
  JD_UNASSIGNED    = 153,
  JD_UNKNOWN       = 154,
  JD_UNDEFINED     = 155,
  JD_FUNC          = 156
  );

JDValues = set of JDValue;
JDTypes  = array of JDValue;

  {
    JDValueHelper adds methods to the JDValue enum.
    For instance, `code` returns the ordinal value of the enum case.
  }
JDValueHelper = type helper for JDValue
  function code: integer;
end;

  {
    JSValueVal is a record encapsulating a QuickJS value, with possible
    subtypes for various data (bigint, float, string, boolean, array, etc.)
    It includes methods to convert between different types, compare
    types, and retrieve array elements.
  }
JSValueVal = record
  data: packed record
    case match: JDValue of
      JD_BINT:      (BigInt: int64);
      JD_BFLOAT:    (BigFloat: double);
      JD_BDECIMAL:  (BigDecimal: double);
      JD_INT:       (Int32Val: int32);
      JD_F64:       (FloatVal: double);
      JD_STR:       (StrVal: string[255]);
      JD_BOOL:      (BoolVal: boolean);
      JD_FUNC:      (Func: Pointer);
      JD_OBJ:       (ObjectVal: Pointer);
      JD_UNKNOWN:   (Parsed: string[255]);
      JD_ARRAY:     (ArrayVal: TJSValList);
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

    // Default property so you can use e.g. `myValueVal[i]` to get array elements
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

  {
    JSValueParam is a simpler record used to store a single JSValueVal
    or arrays of them. This can be used in function parameters.
  }
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

  {
    JSValueParams can hold either a single value or an array of values,
    tracked by its JDValue 'match' type.
  }
JSValueParams = record
  match: JDValue;
  data: array of JSValueParam;
  count: integer;
  arrayVal: JSValuePArray;
end;

  // Specialized list of JSParameters
JSParameters = TJSValList;
PJSParameters = ^JSParameters;

  {
    JSCallbackFunction is a function type for callbacks from JavaScript
    to Pascal code. The function returns a boolean indicating success or
    failure, and can store a result in `res` (JSValueVal).
  }
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
        true:  (data: JSParameters);
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
function JSValueToString(ctx: JSContext; value: JSValue): string;
function JSValueConstToUtf8(ctx: JSContext; value: JSValueConst): RawUtf8;
function JSFunctionParams(ctx: JSContext; argc: integer; argv: PJSValueConstArr): TStringList;
function JSParseValue(ctx: JSContext; val: JSValue): JSValueVal; overload;
function JSParseValue(ctx: JSContext; val: JSValueRaw): JSValueVal; overload;
function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConstArr): JSParameters; overload;
function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConst): JSParameters; overload;
function JSDumpObject(ctx: JSContext; obj: JSValueConst; var dump: string): boolean;
function JSTryToString(ctx: JSContext; data: JSValue; out str: string): boolean;
function JSStringifyValue(val: JSValueVal): string;
function analyze(ctx: JSContext; EvalResult: PJSValue; loop: boolean = false): string;
function JSConsoleLog(ctx: JSContext; this_val: JSValueConst; argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
procedure RegisterConsoleLog(ctx: PJSContext);
function JSValueValToValue(ctx: JSContext; val: JSValueVal): JSValue;
function JSValueValArrayToArray(ctx: JSContext; val: JSValueVal): JSValueValArray; inline;
function StringToValueVal(const s: string): JSValueVal;
function IntToValueVal(const i: integer): JSValueVal;

function JSValueParamCheck(const params: PJSParameters; const fmts: array of JDValue): boolean;
function valTypeToStr(val: JDValue): string;
function checkJSParams(params: JSParameters; expect: JDTypes): integer; overload;
function checkJSParams(params: JSParameters; expect, expect2: JDTypes): integer; overload;

const
JS_TAG_UNKNOWN     = -10;
JS_PARAM_MISSMATCH = 150;
JS_PARAM_OK        = -1;

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
    if params[i]^.data.match <> expect[i] then
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
    if params[i]^.data.match <> expect[i] then
      if params[i]^.data.match <> expect2[i] then
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
  Result := valTypeToStr(self.data.match);
end;

function JSValueVal.intify: int64;
begin
  case self.data.match of
  JD_BINT:
    Result := self.data.BigInt;
  JD_INT:
    Result := self.data.Int32Val;
  JD_STR:
      // Attempt string -> int64
    if not TryStrToInt64(self.data.StrVal, Result) then
      raise Exception.Create('Could not interpret string as int');
  else
    // For other types, you can choose whether to raise an error or return 0
    raise Exception.Create('Cannot convert this JDValue to integer');
  end;
end;

function JSValueVal.floatify: double;
begin
  case self.data.match of
  JD_BINT:
    Result := self.data.BigInt;
  JD_BFLOAT:
    Result := self.data.BigFloat;
  JD_BDECIMAL:
    Result := self.data.BigDecimal;
  JD_INT:
    Result := self.data.Int32Val;
  JD_F64:
    Result := self.data.FloatVal;
  JD_STR:
      // Attempt string -> float
    if not TryStrToFloat(self.data.StrVal, Result) then
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
      if (arr[i]^).data.match = JD_ARRAY then
        // If nested array, recurse
        Result := Result + doArray(JSParameters((arr[i]^).data.ArrayVal), depth + 1)
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
  if self.data.match = JD_ARRAY then
  begin
    Result := doArray(JSParameters(self.data.ArrayVal), 0);
    Exit;
  end;

  // Otherwise, convert based on match type
  case self.data.match of
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
    Result := IntToStr(self.data.BigInt);
  JD_BFLOAT:
    Result := FloatToStr(self.data.BigFloat);
  JD_BDECIMAL:
    Result := FloatToStr(self.data.BigDecimal);
  JD_INT:
    Result := IntToStr(self.data.Int32Val);
  JD_F64:
    Result := FloatToStr(self.data.FloatVal);
  JD_STR:
    Result := self.data.StrVal;
  JD_BOOL:
    Result := BoolToStr(self.data.BoolVal, 'true', 'false');
  JD_OBJ:
    Result := '<object>';
  else
    // Default if not recognized
    Result := 'unknown ' + self.data.Parsed;
  end;
end;

function JSValueVal._a(i: integer): JSValueVal;
begin
  // Safely retrieve array elements
  if (i >= 0) and (i < self.data.ArrayVal.Count) then
    Result := (self.data.ArrayVal[i]^)
  else
    raise EListError.CreateFmt('Index %d out of bounds', [i]);
end;

function JSValueVal.acttype(const val: JSValueVal; want: JDValue): JSValueVal;
begin
  // Example logic: If val matches the wanted type, return it; otherwise return empty
  if val.data.match = want then
    Result := val
  else
    FillChar(Result, SizeOf(JSValueVal), 0);
end;

function JSValueVal.match(a: JDValue): boolean;
begin
  Result := (self.data.match = a);
end;

function JSValueVal.mustbe(a: JDValue; func: string = ''; ppos: integer = -1): boolean;
begin
  Result := false;
  if not match(a) then
    if func = '' then
      raise EInvalidCast.Create(Format(sDataTypeErr, [valtype, valTypeToStr(a)]))
    else
    if ppos > -1 then
      raise EInvalidCast.Create(Format(sDataTypeErrPos, [valtype, valTypeToStr(a), func, ppos]))
    else
      raise EInvalidCast.Create(Format(sDataTypeErrFunc, [valtype, valTypeToStr(a), func]))// If mismatch, raise an exception with contextual info
  ;
  Result := true;
end;

class operator JSValueVal.=(const a, b: JSValueVal): boolean;
begin
  // Simple check: compare the JDValue tag
  Result := (a.data.match = b.data.match);
end;

class operator JSValueVal.explicit(a: JSValueVal): integer;
begin
  // Convert to integer (prefers Int32Val, else BigInt)
  if a.data.match = JD_INT then
    Result := a.data.Int32Val
  else
    Result := a.data.BigInt;
end;

class operator JSValueVal.explicit(a: JSValueVal): TJSValList;
begin
  // Convert to TJSValList if it is an array; else return nil
  if a.data.match = JD_ARRAY then
    Result := a.data.ArrayVal
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

function JSValueToString(ctx: JSContext; value: JSValue): string;
begin
  // Convert a JSValue to a UTF-8 string via mORMot's "ToUtf8"
  Result := ctx^.ToUtf8(value);
end;

function JSValueConstToUtf8(ctx: JSContext; value: JSValueConst): RawUtf8;
var
  cStr: pansichar;
begin
  cStr := JS_ToCString(ctx, value);
  if cStr <> nil then
  begin
    Result := RawUtf8(cStr);
    JS_FreeCString(ctx, cStr);
  end
  else
    Result := '';
end;

function JSFunctionParams(ctx: JSContext; argc: integer; argv: PJSValueConstArr): TStringList;
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

function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConstArr): JSParameters;
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

function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConst): JSParameters;
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

function JSTryToString(ctx: JSContext; data: JSValue; out str: string): boolean;
begin
  // Attempt to parse the JSValue as a string
  with JSParseValue(ctx, data) do
    if data.match <> JD_STR then
      Result := false
    else
    begin
      str := data.StrVal;
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
      Result.data.match := JD_ARRAY;
      Result.data.ArrayVal := TJSValList.Create;

      // Extract elements
      for i := 0 to len - 1 do
      begin
        New(pVal);
        pVal^ := JSParseValue(ctx, JS_GetPropertyUint32(ctx, val.Raw, i));
        Result.data.ArrayVal.Add(pVal);
      end;
    end
    else
    begin
      // Not an array => treat as an object
      Result.data.match := JD_OBJ;
      Result.data.ObjectVal := val.Ptr;
    end;
    Exit;
  end;

  // Handle other types
  if val.NormTag = JS_TAG_BOOL then
  begin
    Result.data.match := JD_BOOL;
    Result.data.BoolVal := val.Bool;
  end
  else
  if val.IsBigDecimal then
  begin
    Result.data.match := JD_BDECIMAL;
    Result.data.BigDecimal := val.F64;
  end
  else
  if val.IsBigFloat then
  begin
    Result.data.match := JD_BFLOAT;
    Result.data.BigFloat := val.F64;
  end
  else
  if val.IsBigInt then
  begin
    Result.data.match := JD_INT;
    Result.data.BigInt := val.int64;
  end
  else
  if val.IsInt32 then
  begin
    Result.data.match := JD_INT;
    Result.data.Int32Val := val.int32;
  end
  else
  if (val.IsFloat) or (val.NormTag = JS_TAG_FLOAT64) then
  begin
    Result.data.match := JD_F64;
    Result.data.FloatVal := val.F64;
  end
  else
  if val.IsString then
  begin
    Result.data.match := JD_STR;
    Result.data.StrVal := JSValueToString(ctx, val);
  end
  else
  if val.IsUndefined then
    Result.data.match := JD_UNDEFINED
  else
  if val.IsObject then
  begin
    Result.data.match := JD_OBJ;
    Result.data.ObjectVal := val.Ptr;
  end
  else
  if JS_IsFunction(ctx, val.Raw) then
  begin
    Result.data.match := JD_FUNC;
    Result.data.Func := val.Ptr;
  end
  else
  if val.IsNan then
    Result.data.match := JD_NAN
  else
  if not val.IsUndefined then
    Result.data.match := JD_UNDEFINED
  else
  if val.IsNull then
    Result.data.match := JD_NULL
  else
  if not val.IsUninitialized then
    Result.data.match := JD_UNINITIALIZED
  else
  begin
    // Fallback if none of the above
    Result.data.Parsed := JSValueToString(ctx, val);
    Result.data.match := JD_UNKNOWN;
  end;
end;

function JSStringifyValue(val: JSValueVal): string;
var
  s: string;
begin
  // Converts a JSValueVal to a Pascal string, depending on the match type
  case val.data.match of
  JD_BDECIMAL:
    s := FloatToStr(val.data.BigDecimal);
  JD_BFLOAT:
    s := FloatToStr(val.data.BigFloat);
  JD_BINT:
    s := IntToStr(val.data.BigInt);
  JD_F64:
    s := FloatToStr(val.data.FloatVal);
  JD_INT:
    s := IntToStr(val.data.Int32Val);
  JD_STR:
    s := val.data.StrVal;
  JD_BOOL:
    s := BoolToStr(val.data.BoolVal);
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
      if (arr[i]^).data.match = JD_ARRAY then
        // Recursively build nested arrays
        JS_SetPropertyUint32(ctx, retarr, i,
          doArray(JSParameters((arr[i]^).data.ArrayVal), depth + 1).Raw)
      else
      begin
        tmp := JSValueValToValue(ctx, (arr[i]^));
        JS_SetPropertyUint32(ctx, retarr, i, tmp.Raw);
      end;
    Result := JSValue(retarr);
  end;

begin
  // If array, convert each element
  if val.data.match = JD_ARRAY then
  begin
    Result := doArray(JSParameters(val.data.ArrayVal), 0);
    Exit;
  end;

  // Otherwise handle each type
  case val.data.match of
  JD_BINT:
    Result.from64(val.data.BigInt);
  JD_BFLOAT:
    Result.FromFloat(val.data.BigFloat);
  JD_BDECIMAL:
    Result.fromFloat(val.data.BigDecimal);
  JD_INT:
    Result.From32(val.data.Int32Val);
  JD_F64:
    Result.FromFloat(val.data.FloatVal);
  JD_STR:
    Result := ctx^.From(val.data.StrVal);
  JD_BOOL:
    Result.From(val.data.BoolVal);
  JD_OBJ:
      // If ObjectVal is already a JSValue object pointer
    Result := JSValue(val.data.ObjectVal);
  else
    // For non-supported types, return `undefined`
    Result := JSValue(JS_UNDEFINED);
  end;
end;

function JSValueValArrayToArray(ctx: JSContext; val: JSValueVal): JSValueValArray; inline;
var
  i: integer;
begin
  // Convert a TJSValList to a dynamic array of JSValueVal
  SetLength(Result, val.data.ArrayVal.Count);
  for i := 0 to val.data.ArrayVal.Count - 1 do
    Result[i] := (val.data.ArrayVal[i]^);
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
        dump := Format('%s%s%s: %s', [
          IfThen(i = 0, '', #13#10), dump,
          'Property Name', propName
          ]);
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

function JSConsoleLog(ctx: JSContext; this_val: JSValueConst; argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
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
  ExtLog(uxdAuto, sLogRecevive, sLogDesc, fullMsg);

  // Return undefined
  Result := JS_UNDEFINED;
end;

procedure RegisterConsoleLog(ctx: PJSContext);
var
  consoleObj, logFunc: JSValueRaw;
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

  // Optionally free consoleObj if the QuickJS binding requires it
  // (Commented out as usage may vary depending on the QuickJS binding)
  // JS_Free(ctx^, consoleObj);
end;

function JSValueParamCheck(const params: PJSParameters; const fmts: array of JDValue): boolean;
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
    if v.data.match <> fmts[i] then
      Exit(false);
  end;
  Result := true;
end;

function IntToValueVal(const i: integer): JSValueVal;
begin
  // Create a JSValueVal from an integer
  Result.data.match := JD_INT;
  Result.data.Int32Val := i;
end;

function StringToValueVal(const s: string): JSValueVal;
begin
  // Create a JSValueVal from a string
  FillChar(Result, SizeOf(JSValueVal), 0);
  Result.data.match := JD_STR;
  Result.data.StrVal := s;
end;

end.
