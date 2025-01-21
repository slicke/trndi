
unit trndi.ext.functions;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses 
Classes, SysUtils, mormot.lib.quickjs, mormot.core.base, strutils, fgl,
Dialogs, slicke.ux.alert, Math, types;

resourcestring
sNoTrace =  'No stack trace available';
sUnknownErr = 'Unknown error';
sStackErrMsg = 'Error: %s '#13#10'Stack: %s %s';
sLogRecevive = 'Output through console.log has been received';
sLogDesc = 'Trndi has captured data sent to console.log in a JavaScript extension';
sStackFailed = 'An error occurred, and the stacktrace could not be loaded. Showing backtrace.';
sDataTypeErr = 'Datatype %s was not expected, expected in function %s';
sDataTypeErrPos = 'Datatype %s was not expected, expected in function %s, parameter %d';
sDataTypeErrFunc = 'Datatype "%s" was not expected, in function "%s". Expected "%s"';

type 
  // Forward declaration för att undvika cirkulära beroenden
PJSValueVal = ^JSValueVal;

  // Typalias för en lista av pekare till JSValueVal
TJSValList = specialize TFPGList<PJSValueVal>;
PJSValList = ^TJSValList;

  // Definiera JDValue enum med extra typer
JDValue = (
  JD_UNINITIALIZED = JS_TAG_UNINITIALIZED, // 0
  JD_INT          = JS_TAG_INT,           // 1
  JD_BOOL         = JS_TAG_BOOL,          // 2
  JD_NULL         = JS_TAG_NULL,          // 3
  JD_F64          = JS_TAG_FLOAT64,       // 7
  JD_OBJ          = JS_TAG_OBJECT,        // 8
  JD_STR          = JS_TAG_STRING,        // 11
  JD_BFLOAT       = JS_TAG_BIG_FLOAT,     // 13
  JD_BINT         = JS_TAG_BIG_INT,       // 14
  JD_BDECIMAL     = JS_TAG_BIG_DECIMAL,   // 15
  JD_ARRAY        = 150,
  JD_UNSET        = 151,
  JD_NAN          = 152,
  JD_UNASSIGNED   = 153,
  JD_UNKNOWN      = 154,
  JD_UNDEFINED    = 155,
  JD_FUNC         = 156
  );

JDValues = set of JDValue;

JDTypes = array of JDValue;

  // Typhelper för JDValue (en enum, ordinal typ)
JDValueHelper =

  type helper for JDValue
  function code: integer;
end;

                                  // Definiera JSValueVal record med inlinade klassoperatorer
JSValueVal = record
  data: packed record
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
                                  // Använd TJSValList som TFPGList<PJSValueVal>
    end;
  function valtype: string;
  // Convert different values
  function stringify: string;
  function floatify: double;
  function intify: int64;

  function _a(i: integer): JSValueVal;
  function acttype(const val: JSValueVal; want: JDValue): JSValueVal
  ;
  function match(a: JDValue): boolean;
  function mustbe(a: JDValue; func: string = ''; ppos: integer = -1)
    : boolean;
  property arrval[i: integer]: JSValueVal read _a; default;

                                  // Klassoperatorer markerade som inline
  class operator =(const a, b: JSValueVal): boolean; overload;
    inline;
  class operator explicit(a: JSValueVal): integer; inline;
  class operator explicit(a: JSValueVal): TJSValList; inline;
  class operator explicit(a: JSValueVal): string; inline;
end;

JSValueValArray = array of JSValueVal;

                                  // Enskilt värde
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

                                  // Array av JSValueParam
JSValuePArray = array of JSValueParam;

                                  // Wrapper som håller antingen ett enskilt värde eller en array
JSValueParams = record
  match: JDValue;
  data: array of JSValueParam;
  count: integer;
  arrayVal: JSValuePArray;
end;

                                  // Specialiserad generisk lista och enumerator
JSParameters = TJSValList;
PJSParameters = ^JSParameters;


                             // Definiera funktionstyp med funktioner markerade som inline i klassen
JSCallbackFunction = function(const ctx: PJSContext; const func:
  string; const params: JSParameters; out res: JSValueVal): boolean
  of object;
PJSCallbackFunction = ^JSCallbackFunction;


generic TJSGValList<T> = class(specialize TFPGList<T>)
public
  function GetInline(Index: longint): T; inline;
end;

                                  // Definiera TJSCallback med inline klassoperator
TJSCallback = record
  params: record
    values: packed record
      case boolean of
        true: (data: JSParameters);
        false: ();
      end;
    min, max: integer;
    end;
  func: string;
  callback: JSCallbackFunction;
  class operator =(const a, b: TJSCallback): boolean; overload;
    inline;
end;
PJSCallback = ^TJSCallback;

                                  // Funktioner deklarerade i interface
function JSValueToString(ctx: JSContext; value: JSValue): string;
function JSValueConstToUtf8(ctx: JSContext; value: JSValueConst):
RawUtf8;
function JSFunctionParams(ctx: JSContext; argc: integer; argv:
PJSValueConstArr): TStringList;
function JSParseValue(ctx: JSContext; val: JSValue): JSValueVal;
function JSParseParameters(ctx: JSContext; argc: integer; argv:
PJSValueConstArr): JSParameters;
function JSParseParameters(ctx: JSContext; argc: integer; argv:
PJSValueConst): JSParameters;
function JSDumpObject(ctx: JSContext; obj: JSValueConst; var dump:
string): boolean;
function JSTryToString(ctx: JSContext; data: JSValue; out str:
string): boolean;
function JSStringifyValue(val: JSValueVal): string;
function JSParseValue(ctx: JSContext; val: JSValueRaw): JSValueVal
;
function analyze(ctx: JSContext; EvalResult: PJSValue; loop:
boolean = false): string;
function JSConsoleLog(ctx: JSContext; this_val: JSValueConst; argc
: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
procedure RegisterConsoleLog(ctx: PJSContext);
function JSValueValToValue(ctx: JSContext; val: JSValueVal):
JSValue;
function JSValueValArrayToArray(ctx: JSContext; val: JSValueVal):
JSValueValArray; inline;
function StringToValueVal(const s: string): JSValueVal;
function IntToValueVal(const i: integer): JSValueVal;

function JSValueParamCheck(const params: PJSParameters; const fmts
: array of JDValue): boolean;
function valTypeToStr(val: JDValue): string;
function checkJSParams(params: JSParameters; expect: JDTypes): integer;
function checkJSParams(params: JSParameters; expect, expect2: JDTypes): integer;

const
JS_TAG_UNKNOWN = -10;
JS_PARAM_MISSMATCH = 150;
JS_PARAM_OK = -1;

implementation

function checkJSParams(params: JSParameters; expect: JDTypes): integer;
var
  i: integer;
begin
  if params.Count <> length(expect) then
    exit(JS_PARAM_MISSMATCH);
  for i := 0 to params.Count-1 do
    if params[i]^.data.match <> expect[i] then
      Exit(i);

  result := JS_PARAM_OK;
end;

function checkJSParams(params: JSParameters; expect, expect2: JDTypes): integer;
var
  i: integer;
begin
  if params.Count <> length(expect) then
    if params.Count <> length(expect2) then
      exit(JS_PARAM_MISSMATCH);

  for i := 0 to params.Count-1 do
    if params[i]^.data.match <> expect[i] then
      if params[i]^.data.match <> expect2[i] then
        Exit(i);

  result := JS_PARAM_OK;
end;

{ JDValueHelper }

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
  result := valTypeToStr(self.data.match);
end;

function JSValueVal.intify: int64;
begin
  case self.data.match of
  JD_BINT:
    Result := self.data.BigInt;
  JD_INT:
    Result := self.data.Int32Val;
  JD_STR:
    if not TryStrToInt64(self.data.StrVal, result) then
      raise Exception.Create('Could not interpret string as int');
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
    if not TryStrToFloat(self.data.StrVal, result) then
      raise Exception.Create('Could not interpret string as float');
  end;

end;

function JSValueVal.stringify: string;
function doArray(arr: JSParameters; depth: integer): string;
  var
    i: integer;
  begin
    result := Format('%s'#13#10, [StringOfChar('>', depth)]);
                                  // Indikera djup

                                  // Loop all JSValues in the provided array
    for i := 0 to arr.Count-1 do
      if (arr[i]^).data.match = JD_ARRAY then
        result := result + doArray(JSParameters((arr[i]^).data.ArrayVal)
          , depth + 1)
      else
      if depth > 5 then
      begin
        result := result +
          '-- Reached 5 levels, aborting array printing --'#13#10;
        break;
      end
      else
        result := result + Format('%s [%s] %s'#13#10, [StringOfChar('>',
          depth), (arr[i]^).valtype, (arr[i]^).stringify]);
  end;
begin
  if self.data.match = JD_ARRAY then
  begin
    result := doArray(JSParameters(self.data.ArrayVal), 0);
    Exit;
  end;

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
    Result := 'unknown ' + self.data.Parsed;
  end;
end;

function JSValueVal._a(i: integer): JSValueVal;
begin
  if (i >= 0) and (i < self.data.ArrayVal.Count) then
    Result := (self.data.ArrayVal[i]^)
  else
    raise EListError.CreateFmt('Index %d out of bounds', [i]);
end;

function JSValueVal.acttype(const val: JSValueVal; want: JDValue): JSValueVal;
begin
  // Implementera logiken här baserat på din applikations behov
  // Exempel:
  if val.data.match = want then
    Result := val
  else
    FillChar(Result, SizeOf(JSValueVal), 0);
  // Eller någon annan standard
end;

function JSValueVal.match(a: JDValue): boolean;
begin
  result := self.data.match = a;
end;

function JSValueVal.mustbe(a: JDValue; func: string = ''; ppos: integer = -1): boolean;

var 
  extra: string;
begin
  result := false;
  if not match(a) then
    if func = '' then
      raise EInvalidCast.Create(Format(sDataTypeErr, [valtype, valTypeToStr(a)]))
    else
    if ppos > -1 then
      raise EInvalidCast.Create(Format(sDataTypeErrPos, [valtype, valTypeToStr(a), func, ppos
        ]))
    else
      raise EInvalidCast.Create(Format(sDataTypeErrFunc, [valtype, valTypeToStr(a), func]));
  result := true;
end;

class operator JSValueVal. = (const a, b: JSValueVal): boolean;
begin
  result := (a.data.match = b.data.match);
    // En enkel typjämförelse
end;

class operator JSValueVal.explicit(a: JSValueVal): integer;
begin
  if a.data.match = JD_INT then
    result := a.data.Int32Val
  else
    result := a.data.BigInt;
end;

class operator JSValueVal.explicit(a: JSValueVal): TJSValList;
begin
  if a.data.match = JD_ARRAY then
    result := a.data.ArrayVal
  else
    result := nil;
end;

class operator JSValueVal.explicit(a: JSValueVal): string;
begin
  result := a.stringify;
end;

{ TJSCallback }

class operator TJSCallback. = (const a, b: TJSCallback): boolean;
begin
  result := a.func = b.func;
end;

{ Utility Functions }

function JSValueToString(ctx: JSContext; value: JSValue): string;
begin
  result := ctx^.ToUtf8(value);
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

function JSFunctionParams(ctx: JSContext; argc: integer; argv: PJSValueConstArr):
TStringList
;

var
  i: integer;
  argstr: RawUtf8;
begin
  result := TStringList.Create;
  for i := 0 to argc - 1 do
    if JSValue(argv^[i]).IsString then
    begin
      argStr := JSValueConstToUtf8(ctx, argv^[i]);
      result.AddPair(IntToStr(i), argStr);
    end
    else
      result.AddPair(IntToStr(i), '#-type error-#');
end;

function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConstArr):
JSParameters
;

var
  i: integer;
  pVal: PJSValueVal;
begin
  result := TJSValList.Create;
  try
    for i := 0 to argc - 1 do
    begin
      New(pVal);
      pVal^ := JSParseValue(ctx, argv^[i]);
      result.Add(pVal);
    end;
  except
              // Free all allocated pVal if an exception occurs
    for i := 0 to result.Count - 1 do
      Dispose(result[i]);
    result.Free;
    raise;
  end;
end;

function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConst): JSParameters
;

var
  i: integer;
  pVal: PJSValueVal;
begin
  result := TJSValList.Create;
  try
    for i := 0 to argc - 1 do
    begin
      New(pVal);
      pVal^ := JSParseValue(ctx, argv[i]);
      result.Add(pVal);
    end;
  except
            // Free all allocated pVal if an exception occurs
    for i := 0 to result.Count - 1 do
      Dispose(result[i]);
    result.Free;
    raise;
  end;
end;

function JSTryToString(ctx: JSContext; data: JSValue; out str: string): boolean;
begin
  with JSParseValue(ctx, data) do
    if data.match <> JD_STR then
      result := false
    else
    begin
      str := data.StrVal;
      result := true;
    end;
end;

function JSParseValue(ctx: JSContext; val: JSValueRaw): JSValueVal;
begin
  result := JSParseValue(ctx, JSValue(val));
end;

function JSParseValue(ctx: JSContext; val: JSValue): JSValueVal;

var
  arrVal: JSValueRaw;
  len, i: integer;
  pVal: PJSValueVal;
        // Tillfällig pekare
begin
  if val.IsObject or (JS_IsArray(ctx, val.Raw) = JS_TRUE) then
  begin
    arrVal := JS_GetPropertyStr(ctx, val.raw, 'length');
    if (not val.IsUndefined) and (JS_ToInt32(ctx, @len, arrVal) = 0) then
    begin
      result.data.match := JD_ARRAY;
      result.data.ArrayVal := TJSValList.Create;
      for i := 0 to len - 1 do
      begin
        New(pVal);
                    // Allokera minne för pVal
        pVal^ := JSParseValue(ctx, JS_GetPropertyUint32(ctx, val.Raw, i));
                    // Tilldela värdet
        result.data.ArrayVal.Add(pVal);
                    // Lägg till pekaren i listan
      end;
                // Notera: Beroende på QuickJS-bindningarna kan du behöva frigöra arrVal
    end
    else
    begin
                // Hantera som objekt om det inte är en array
      result.data.match := JD_OBJ;
      result.data.ObjectVal := val.Ptr;
    end;
    Exit;
  end;


        // Hantera andra typer...
  if val.NormTag = JS_TAG_BOOL then
  begin
    result.data.match := JD_BOOL;
    result.data.BoolVal := val.Bool;
  end
  else
  if val.IsBigDecimal then
  begin
    result.data.match := JD_BDECIMAL;
    result.data.BigDecimal := val.F64;
  end
  else
  if val.IsBigFloat then
  begin
    result.data.match := JD_BFLOAT;
    result.data.BigFloat := val.F64;
  end
  else
  if val.IsBigInt then
  begin
    result.data.match := JD_INT;
    result.data.BigInt := val.int64;
  end
  else
  if val.IsInt32 then
  begin
    result.data.match := JD_INT;
    result.data.Int32Val := val.int32;
  end
  else
  if (val.IsFloat) or (val.NormTag = JS_TAG_FLOAT64) then
  begin
    result.data.match := JD_F64;
    result.data.FloatVal := val.F64;
  end
  else
  if val.IsString then
  begin
    result.data.match := JD_STR;
    result.data.StrVal := JSValueToString(ctx, val);
  end
  else
  if val.IsUndefined then
    result.data.match := JD_UNDEFINED
  else
  if val.IsObject then
  begin
    result.data.match := JD_OBJ;
    result.data.ObjectVal := val.Ptr;
  end
  else
  if JS_IsFunction(ctx, val.raw) then
  begin
    result.data.match := JD_FUNC;
    result.data.Func := val.Ptr;
  end
  else
  if val.IsNan then
    result.data.match := JD_NAN
  else
  if not val.IsUndefined then
    result.data.match := JD_UNDEFINED
  else
  if val.IsNull then
    result.data.match := JD_NULL
  else
  if not val.IsUninitialized then
    result.data.match := JD_UNINITIALIZED
  else
  begin
    result.data.Parsed := JSValueToString(ctx, val);
    result.data.match := JD_UNKNOWN;
  end;
end;
function JSStringifyValue(val: JSValueVal): string;

var
  s: string;
begin
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
    s := BoolToStr(val.data.BoolVal)
  else
    s := 'Unknown';
  end;

  result := s;
end;

function valTypeToStr(val: JDValue): string;
begin
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
        JS_SetPropertyUint32(ctx, retarr, i, doArray(JSParameters((arr[i]^).data.ArrayVal),
          depth + 1).Raw)
      else
      begin
        tmp := JSValueValToValue(ctx, (arr[i]^));
        JS_SetPropertyUint32(ctx, retarr, i, tmp.Raw);
      end;
    Result := JSValue(retarr);
  end;
begin
  if val.data.match = JD_ARRAY then
  begin
    result := doArray(JSParameters(val.data.ArrayVal), 0);
    Exit;
  end;

  case val.data.match of
  JD_BINT:
    Result.from64(val.data.BigInt);
          // Antag att BigInt är en Int64
  JD_BFLOAT:
    Result.FromFloat(val.data.BigFloat);
          // Antag att BigFloat är en FloatVal
  JD_BDECIMAL:
    Result.fromFloat(val.data.BigDecimal);
          // För BigDecimal (om stöds)
  JD_INT:
    Result.From32(val.data.Int32Val);
          // Heltal (int32)
  JD_F64:
    Result.FromFloat(val.data.FloatVal);
          // Flyttal (float64)
  JD_STR:
    Result := ctx^.From(val.data.StrVal);
          // Sträng (antagen Pascal-sträng)
  JD_BOOL:
    Result.From(val.data.BoolVal);
          // Booleskt värde
  JD_OBJ:
    Result := JSValue(val.data.ObjectVal);
          // Om detta redan är ett JSValue-objekt
  else
    Result := JSValue(JS_UNDEFINED);
          // För icke-stödda typer, returnera `undefined`
  end;
end;

function JSValueValArrayToArray(ctx: JSContext; val: JSValueVal): JSValueValArray;
inline;

var
  i: integer;
begin
  SetLength(result, val.data.ArrayVal.Count);
  for i := 0 to val.data.ArrayVal.Count - 1 do
    result[i] := (val.data.ArrayVal[i]^);
end;

function JSDumpObject(ctx: JSContext; obj: JSValueConst; var dump: string): boolean;

var
  propEnum: PJSPropertyEnum;
  propCount: uint32;
  i: integer;
  propName: pchar;
begin
        // Anropa JS_GetOwnPropertyNames för att hämta en lista med objektets egenskaper
  if JS_GetOwnPropertyNames(ctx, @propEnum, @propCount, obj, JS_GPN_STRING_MASK) = 0 then
  try
    result := false;
    for i := 0 to propCount - 1 do
    begin
                  // Konvertera egenskapens namn till en Pascal-sträng och skriv ut det
      propName := JS_AtomToCString(ctx, propEnum[i].atom);
      try
                    // Här kan vi skriva ut eller bearbeta varje property
        dump := Format('%s%s%s: %s', [IfThen(i = 0, '', #13#10), dump, 'Property Name',
          propName]);
        result := true;
      finally
                    // Frigör atomsträngen efter användning
        JS_FreeCString(ctx, propName);
      end;

              // Frigör atomen efter att den använts
      JS_FreeAtom(ctx, propEnum[i].atom);
    end;
  finally
        // Frigör hela listan med egenskaper efter att iterationen är klar
    JS_Free(ctx, propEnum);
  end
  else
    result := false;
end;

function analyze(ctx: JSContext; EvalResult: PJSValue; loop: boolean = false): string;

var 
  messageVal, stackVal: JSValue;
  messageStr, stackStr: pchar;
  err: string;
begin
  // Hämta undantagsobjektet
  messageVal := JSValue(JS_GetPropertyStr(ctx, JS_GetException(ctx), pchar('message')));
  if messageVal.IsString then
    messageStr := JS_ToCString(ctx, messageVal.raw)
  else
    messageStr := pchar(sUnknownErr);

  // Hämta 'stack' från undantagsobjektet, om den finns
  stackVal := JSValue(JS_GetPropertyStr(ctx, JS_GetException(ctx), 'stack'));
  if stackVal.IsString then
    stackStr := JS_ToCString(ctx, stackVal.Raw)
  else
  if not loop then
  begin
    Result := sStackFailed + analyze(ctx, @stackVal, true);
    Exit;
  end
  else
    stackStr := pchar(sNoTrace);

  try
    // Kombinera felen och stackspårningen i Result
    Result := Format(sStackErrMsg, [messageStr, stackStr, err]);
  except
    Result := 'Internal error';
  end;

// Frigör C-strängarna och JSValues efter användning
//  JS_FreeCString(ctx, messageStr);
//JS_FreeCString(ctx, stackStr);
// JS_Free(ctx, @stackVal); // JS_Free används inte på detta sätt
// JS_Free(ctx, EvalResult); // JS_Free används inte på detta sätt
end;

function JSConsoleLog(ctx: JSContext; this_val: JSValueConst; argc: integer; argv: PJSValueConstArr)
: JSValueRaw;
cdecl;

var 
  msg: pchar;
  i: integer;
  fullMsg: string;
begin
  // Konkatenera alla argument som strängar
  fullMsg := '';
  for i := 0 to argc - 1 do
  begin
    msg := JS_ToCString(ctx, argv^[i]);
    try
      if i > 0 then
        fullMsg := fullMsg + ' ';
      fullMsg := fullMsg + string(msg);
        // Konvertera till Pascal-sträng utan radbrytningar
    finally
      JS_FreeCString(ctx, msg);
    end;
  end;

  ExtLog(sLogRecevive, sLogDesc, fullMsg);

// Returnera undefined
  Result := JS_UNDEFINED;
end;

procedure RegisterConsoleLog(ctx: PJSContext);

var 
  consoleObj, logFunc: JSValueRaw;
begin
  // Skapa ett "console" objekt om det inte redan finns
  consoleObj := JS_GetPropertyStr(ctx^, JS_GetGlobalObject(ctx^), 'console');
  if consoleObj = JS_UNDEFINED then
  begin
    consoleObj := JS_NewObject(ctx^);
    JS_SetPropertyStr(ctx^, JS_GetGlobalObject(ctx^), 'console', consoleObj);
  end;

  // Skapa log-funktionen och lägg till den i "console"
  logFunc := JS_NewCFunction(ctx^, PJSCFunction(@JSConsoleLog), 'log', 1);
  // todo verofy cast
  JS_SetPropertyStr(ctx^, consoleObj, 'log', logFunc);

  // Frigör console-objektet efter användning
  // JS_Free(ctx^, consoleObj); // JS_Free används inte på detta sätt
end;

function JSValueParamCheck(const params: PJSParameters; const fmts: array of JDValue): boolean;

var 
  i: integer;
  v: JSValueVal;
begin
  if params^.Count <> Length(fmts) then
    Exit(false);

  for i := 0 to params^.Count - 1 do
  begin
    v := (params^[i]^);
    if v.data.match <> fmts[i] then
      Exit(false);
  end;

  result := true;
end;


function IntToValueVal(const i: integer): JSValueVal;
begin
  result.data.match := JD_INT;
  result.data.Int32Val := i;
end;

function StringToValueVal(const s: string): JSValueVal;
begin
  FillChar(result, SizeOf(JSValueVal), 0);
  result.data.match := JD_STR;
  result.data.StrVal := s;
end;

end.
