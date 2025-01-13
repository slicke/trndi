unit trndiext.functions;

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
   sStackFailed = 'An error occured, and the stacktrace could not be loaded. Showing backtrace.';
   sDataTypeErr = 'Datatype %s was not expected, expected in function %s';
   sDataTypeErrPos = 'Datatype %s was not expected, expected in function %s, parameter %d';
   sDataTypeErrFunc = 'Datatype "%s" was not expected, in function "%s". Expected "%s"';

type

  JDValue = (   JD_UNINITIALIZED = JS_TAG_UNINITIALIZED,  JD_NULL = JS_TAG_NULL,  JD_BINT = JS_TAG_BIG_INT,  JD_BFLOAT = JS_TAG_BIG_FLOAT,  JD_BDECIMAL = JS_TAG_BIG_DECIMAL,  JD_INT = JS_TAG_INT,  JD_F64 = JS_TAG_FLOAT64,  JD_STR = JS_TAG_STRING,  JD_BOOL = JS_TAG_BOOL,  JD_OBJ = JS_TAG_OBJECT,
  JD_ARRAY = 150,   JD_UNSET = 151,   JD_NAN = 152,   JD_UNASSIGNED = 153,   JD_UNKNOWN = 154,  JD_UNDEFINED = 155,  JD_FUNC = 156);

  JDValues = set of JDValue;

  JDValueHelper = type helper for JDValues
     function code: integer;
  end;

  JSValueVal = record
    data: packed record
      case match: JDValue of
        JD_BINT: (BigInt: Int64);
        JD_BFLOAT: (BigFloat: Double);
        JD_BDECIMAL: (BigDecimal: Double);
        JD_INT: (Int32Val: Int32);
        JD_F64: (FloatVal: Double);
        JD_STR: (StrVal: string[255]);
        JD_BOOL: (BoolVal: Boolean);
        JD_FUNC: (Func: Pointer);
        JD_OBJ: (ObjectVal: Pointer);
        JD_ARRAY: (ArrayVal: specialize TFPGList<JSValueVal>);
      end;
      function valtype: string;
      function stringify: string;
      function _a(i: integer): JSValueVal;
      function acttype(const val: JSValueVal; want: JDValue): JSValueVal;
      function match(a: JDValue): boolean;
      function mustbe(a: JDValue; func: string = ''; ppos: integer = -1):boolean;
      property arrval[i: integer]: JSValueVal read _a; default;

      class operator =(const a, b: JSValueVal): Boolean; overload;
      class operator explicit(a: JSValueVal): Integer;
      class operator explicit(a: JSValueVal): specialize TFPGList<JSValueVal>;
      class operator explicit(a: JSValueVal): string;
  end;

  PJSValueVal = ^JSValueVal;

  JSValueValArray = array of JSValueVal;


  // Enskilt värde
  JSValueParam = record
    BigInt: Int64;
    BigFloat: Double;
    BigDecimal: Double;
    Int32Val: Int32;
    FloatVal: Double;
    StrVal: string;
    BoolVal: Boolean;
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

  generic TJSValList<T> = class(specialize TFPGList<T>)
  public
  // Kanske jämföra suff?
  end;
  //---

  JSParameters = specialize TJSValList<JSValueVal>;
  PJSParameters = ^JSParameters;
  JSCallbackFunction = function(const ctx: PJSContext; const func: string; constref params: JSParameters; out res: JSValueVal): boolean of object;
  PJSCallbackFunction = ^JSCallbackFunction;

  TJSCallback = record
    params: record
      values: packed record case boolean of
                    true: (data: JSParameters);
                    false: ();
      end;
      min, max: integer;
    end;
    func: string;
    callback: JSCallbackFunction;
    class operator =(const a, b: TJSCallback): Boolean; overload;
  end;
  PJSCallback = ^TJSCallback;


function JSValueToString(ctx: JSContext; value: JSValue): string;
function JSValueConstToUtf8(ctx: JSContext; value: JSValueConst): RawUtf8;
function JSFunctionParams(ctx: JSContext; argc: integer; argv: PJSValueConstArr): TStringList;
function JSParsevalue(ctx: JSContext; val: JSValue): JSValueVal;
function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConstArr): JSParameters;
function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConst): JSParameters;
function JSDumpObject(ctx: JSContext; obj: JSValueConst; var dump: string): boolean;
function JSTryToString(ctx: JSContext; data: JSValue; out str: string): boolean;
function JSStringifyValue(val:JSValueVal): string;
function JSParseValue(ctx: JSContext; val: JSValueRaw): JSValueVal;
function analyze(ctx: JSContext; EvalResult: PJSValue; loop: boolean = false): string;
function JSConsoleLog(ctx: JSContext; this_val: JSValueConst; argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
procedure RegisterConsoleLog(ctx: PJSContext);
function JSValueValToValue(ctx: JSContext; val: JSValueVal): JSValue;
function JSValueValArrayToArray(ctx: JSContext; val: JSValueVal): JSValueValArray;
function StringToValueVal(const s: string): JSValueVal;

function JSValueParamCheck(const params: PJSParameters; const fmts: array of JDValue): boolean;
const
  JS_TAG_UNKNOWN = -10;

implementation

{ TJSValList }



function JSValueParamCheck(const params: PJSParameters; const fmts: array of JDValue): boolean;
var
 i: integer;
 v: JSValueVal;
begin
 if params^.Count <> length(fmts) then
   Exit(false);

 for i := 0 to params^.Count-1 do begin
   v := params^[i];
   if v.data.match  <> fmts[i] then
     Exit(false);
 end;

 result := true;
end;


  function JDValueHelper.code: integer;
  begin

  end;

  function JSValueVal.acttype(const val: JSValueVal; want: JDValue): JSValueVal;
  begin

  end;

function StringToValueVal(const s: string): JSValueVal;
begin
  result.data.match := JD_STR;
  result.data.StrVal := s;
end;

function valTypeToStr(val: JDValue): string;
begin
 case val of
    JD_ARRAY: Result := 'array';

    JD_UNSET: Result := 'Uset';
    JD_NAN: Result := 'NaN';
    JD_UNASSIGNED: Result := 'Unassigned';
    JD_UNKNOWN:   Result := 'Unknown';

    JD_UNINITIALIZED: result := 'Unititialized';
    JD_NULL:     Result := 'null';
    JD_BINT:     Result := 'bigint';
    JD_BFLOAT:   Result := 'bigfloat';
    JD_BDECIMAL: Result := 'bigdecimal';
    JD_INT:      Result := 'int';
    JD_F64:      Result := 'float64';
    JD_STR:      Result := 'string';
    JD_BOOL:     Result := 'bool';
    JD_OBJ:      Result := 'object';
  else
    Result := 'unknown';
  end;
end;


function JSValueVal.mustbe(a: JDValue; func: string = ''; ppos: integer = -1):boolean; // do error - return function name if available
var
  extra: string;
begin
  result := false;
  if not match(a) then
    if  func = '' then
      raise EInvalidCast.Create(Format(sDataTypeErr, [valtype, valTypeToStr(a)]) + extra)
    else if ppos > -1 then
      raise EInvalidCast.Create(Format(sDataTypeErrPos, [valtype, valTypeToStr(a), func, ppos]) + extra)
    else
      raise EInvalidCast.Create(Format(sDataTypeErrFunc, [valtype, valTypeToStr(a), func]) + extra);

    result := true;
end;

function JSValueVal.match(a: JDValue): boolean; // Do the type match with X
begin
  result := self.data.match = a;
end;

function JSValueVal.stringify: string;
  function doArray(arr: JSParameters; depth: integer): string;
  var
    i: integer;

  begin
    result := Format('%s'+#13#10, [StringOfChar('>', depth)]); // Set the - to indicate depth

    // Loop all JSValues in the provided array
    for i := 0 to arr.Count-1 do begin
        if (arr[i].data.match = JD_ARRAY) and (depth <= 5) then
          result := result + doArray(JSParameters(arr[i].data.ArrayVal), depth+1)
        else if depth > 5 then begin
           result := result + '-- Reached 5 levels, aboring array printing --';
           break;
        end
        else
          result := result + Format('%s [%s] %s'#13#10, [StringOfChar('>', depth), arr[i].valtype, arr[i].stringify]);
      end;

  end;
begin

  if self.data.match = JD_ARRAY then begin
    result := doArray(JSParameters(self.data.ArrayVal), 0);
    Exit;
  end;
case self.data.match of
    JD_UNSET:     Result := 'Uset';
    JD_NAN:       Result := 'NaN';
    JD_UNASSIGNED: Result := 'Unassigned';
    JD_UNKNOWN:   Result := 'Unknown';

    JD_UNINITIALIZED: result := 'Unititialized';
    JD_NULL:     Result := 'null';
    JD_BINT:     Result := IntToStr(self.data.BigInt);
    JD_BFLOAT:   Result := FloatToStr(self.data.BigFloat);
    JD_BDECIMAL: Result := FloatToStr(self.data.BigDecimal);
    JD_INT:      Result := IntToStr(self.data.Int32Val);
    JD_F64:      Result := FloatToStr(self.data.FloatVal);
    JD_STR:      Result :=  self.data.StrVal;
    JD_BOOL:     Result := BoolToStr(self.data.BoolVal, 'true', 'false');
    JD_OBJ:      Result := '<object>';
  else
    Result := 'unknown';
  end;
end;

function JSValueVal.valType: string;
begin
   result := ValTypeToStr(self.data.match);
end;

class operator JSValueVal.explicit (a: JSValueVal): Integer;
begin
if a.data.match = JD_INT
 then
  result := a.data.Int32Val
  else
  result := a.data.BigInt;
end;
class operator JSValueVal.explicit (a: JSValueVal): specialize TFPGList<JSValueVal>;
begin
   result := a.data.ArrayVal;
end;

function JSValueVal._a(i: integer): JSValueVal;
begin
   result := self.data.ArrayVal[i];
end;

class operator JSValueVal.explicit (a: JSValueVal): string;
begin
   result := a.stringify;
end;

class operator TJSCallback.= (const a, b: TJSCallback): Boolean; overload;
begin
  result := a.func = b.func
end;

class operator JSValueVal. =(const a, b: JSValueVal): Boolean; overload;
begin
   result := (a.data.match = b.data.match); // Solve w/o ctx?
end;

function JSValueToString(ctx: JSContext; value: JSValue): string;
begin
  result := ctx^.ToUtf8(value);
end;

function JSValueConstToUtf8(ctx: JSContext; value: JSValueConst): RawUtf8;
var
  cStr: PAnsiChar;
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
  argstr: rawutf8;
begin
result := TStringList.Create;
  for i := 0 to argc - 1 do
  begin
     if JSValue(argv^[i]).IsString then begin
      argStr := JSValueConstToUtf8(ctx, argv^[i]);
      result.AddPair(IntToStr(i), argStr);
    end else
      result.AddPair(IntToStr(i), '#-type error-#');
  end;
end;

function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConst): JSParameters;
var
i: integer;
begin
   result := JSParameters.Create;
   for i := 0 to argc-1 do begin
      result.Add(JSParseValue(ctx, argv[i]));
   end;

end;
function JSParseParameters(ctx: JSContext; argc: integer; argv: PJSValueConstArr): JSParameters;
var
i: integer;
begin
   result := JSParameters.Create;
   for i := 0 to argc-1 do begin
      result.Add(JSParseValue(ctx, argv^[i]));
   end;
end;

function JSTryToString(ctx: JSContext; data: JSValue; out str: string): boolean;
begin
    with JSParsevalue(ctx, data) do begin
       if data.match <> JD_STR then
         result := false
       else begin
         result := true;
     //   str := data.StrVal;
       end;
   end;
end;

function JSParseValue(ctx: JSContext; val: JSValueRaw): JSValueVal;
begin
  result := JSParseValue(ctx, JSValue(val));
end;

function JSParseValue(ctx: JSContext; val: JSValue): JSValueVal;
var
  s: string;
  rlen, arr: jsvalueraw;
  len, i: integer;
begin


  if val.IsObject or (JS_IsArray(ctx, val.Raw) = JS_TRUE) then begin
     rlen := JS_GetPropertyStr(ctx, val.raw, 'length');
     if JS_ToInt32(ctx, @len, rlen) <> 0 then
    begin
      JS_Free(ctx, @rlen);
      // Will be handled as obevt later on
    end else begin
//    JS_Free(ctx, @rlen);

    result.data.match := JD_ARRAY;
    result.data.ArrayVal := JSParameters.Create;
    for i := 0 to len-1 do begin
      arr := JS_GetPropertyUint32(ctx, val.Raw, i);
      result.data.ArrayVal.Add(JSParseValue(ctx, arr));

 //     JS_FRee(ctx, @arr);
    end;
    Exit;
  end;
  end;

  if val.IsBigDecimal then
  begin
    result.data.match := JD_BDECIMAL;
    result.data.BigDecimal := val.F64;
  end
  else if val.IsBigFloat then
  begin
    result.data.match := JD_BFLOAT;
    result.data.BigFloat := val.F64;
  end
  else if val.IsBigInt then
  begin
    result.data.match := JD_INT;
    result.data.BigInt := val.Int64;
  end
  else if val.IsFloat then
  begin
    result.data.match := JD_F64;
    result.data.FloatVal := val.F64;
  end
  else if val.IsInt32 then
  begin
    result.data.match := JD_INT;
    result.data.Int32Val := val.Int32;
  end
  else if val.IsString then
  begin
    result.data.match := JD_STR;
    result.data.StrVal := JSValueToString(ctx, val);
  end
  else if val.IsUndefined then
  begin
    result.data.match := JD_UNDEFINED;
  end
  else if val.IsObject then
  begin
    result.data.match := JD_OBJ;
    result.data.ObjectVal := val.Ptr;
  end else if JS_IsFunction(ctx, val.raw) then begin
    result.data.match := JD_FUNC;
    result.data.func := val.Ptr;
  end else
  if val.IsNan then
    result.data.match := JD_NAN
  else if not val.IsUndefined then
    result.data.match := JD_UNDEFINED
  else if val.isnull then
    Result.data.match := JD_NULL
  else if not val.IsUninitialized then
    Result.data.match := JD_UNINITIALIZED
    else begin
    result.data.match := JD_UNKNOWN;
  end;
end;

function JSStringifyValue(val:JSValueVal): string;
var
  s: string;
begin
  case val.data.match of
  JD_BDECIMAL:
     s := val.data.BigDecimal.ToString;
  JD_BFLOAT:
    s := val.data.BigFloat.ToString;
  JD_BINT:
    s := val.data.BigInt.ToString;
  JD_F64:
    s := val.data.FloatVal.ToString;
  JD_INT:
    s := val.data.Int32Val.ToString;
  JD_STR:
    s := val.data.StrVal;
  else
    s := 'Unknown';
  end;

result := s;
end;


function JSValueValArrayToArray(ctx: JSContext; val: JSValueVal): JSValueValArray;
var
i: integer;
begin
  for i := 0 to val.data.ArrayVal.Count-1 do begin
     SetLength(result, length(result)+1);
     if val.data.ArrayVal[i].data.match <> JD_ARRAY then
        result[length(result)] := val.data.arrayval[i];
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
    for i := 0 to arr.Count-1 do begin
        if (arr[i].data.match = JD_ARRAY) and (depth <= 5) then begin

           JS_SetPropertyUint32(ctx, retarr, i, doArray(JSParameters(arr[i].data.ArrayVal), depth+1).Raw)
        end
        else begin
         tmp := JSValueValToValue(ctx,arr[i]);
         JS_SetPropertyUint32(ctx, retarr, i,tmp.raw);
        end;
      end;
   result := JSValue(retarr);
  end;
var
  v: JSValueVal;
  element: JSValueRaw;
  i: Integer;
begin

if val.data.match = JD_ARRAY then begin
   result := doArray(JSParameters(val.data.ArrayVal), 0);
   Exit;
end;

  case val.data.match of
    JD_BINT:
      Result.from64(val.data.BigInt); // Antag att BigInt är en Int64
    JD_BFLOAT:
      Result.FromFloat(val.data.BigFloat); // Antag att BigFloat är en FloatVal
    JD_BDECIMAL:
      Result.fromFloat(val.data.BigDecimal); // För BigDecimal (om stöds)
    JD_INT:
      Result.From32(val.data.Int32Val); // Heltal (int32)
    JD_F64:
      Result.FromFloat(val.data.FloatVal); // Flyttal (float64)
    JD_STR:
      Result :=  ctx^.From(val.data.StrVal); // Sträng (antagen Pascal-sträng)
    JD_BOOL:
      Result.From(val.data.BoolVal); // Booleskt värde
    JD_OBJ:
      Result := JSValue(val.data.ObjectVal); // Om detta redan är ett JSValue-objekt
    else
      Result := JSValue(JS_UNDEFINED); // För icke-stödda typer, returnera `undefined`
  end;
end;





function JSDumpObject(ctx: JSContext; obj: JSValueConst; var dump: string): boolean;
var
  propEnum: PJSPropertyEnum;
  propCount: uint32;
  i: Integer;
  propName: PChar;
begin
  // Anropa JS_GetOwnPropertyNames för att hämta en lista med objektets egenskaper
  if JS_GetOwnPropertyNames(ctx, @propEnum, @propCount, obj, JS_GPN_STRING_MASK) = 0 then
  begin
    try
    result := false;
      for i := 0 to propCount - 1 do
      begin
        // Konvertera egenskapens namn till en Pascal-sträng och skriv ut det
        propName := JS_AtomToCString(ctx, propEnum[i].atom);
        try
          // Här kan vi skriva ut eller bearbeta varje property
          dump := Format('%s%s%s: %s', [IfThen(i = 0, '', #13#10), dump, 'Property Name', propName]);
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
    end;
  end;
end;


function analyze(ctx: JSContext; EvalResult: PJSValue; loop: boolean = false): string;
var
  ResultStr: PAnsiChar;
  err: RawUtf8;
  exceptionVal: JSValueConst;
  messageVal, stackVal: JSValue;
  messageStr, stackStr: PChar;
  begin
/////     JS_ToCString(FContext, JS_GetException(FContext));
    // Hämta undantagsobjektet
    exceptionVal := JS_GetException(ctx);

    // Hämta 'message' från undantagsobjektet
    messageVal := JSValue(JS_GetPropertyStr(ctx, exceptionVal, PChar('message')));
    if messageVal.IsString then
      messageStr := JS_ToCString(ctx, messageVal.raw)
    else
      messageStr := PChar(sUnknownErr);

    // Hämta 'stack' från undantagsobjektet, om den finns
    stackVal := JSValue(JS_GetPropertyStr(ctx, exceptionVal, 'stack'));
    if stackVal.IsString then
      stackStr := JS_ToCString(ctx, stackVal.Raw)
    else begin
    if not loop then begin
         Result := sStackFailed + analyze(ctx, @stackVal, true);

     Exit;
     enD;
//      else ;
      //stackStr := PChar(sNoTrace);
    end;

    try
    // Kombinera felen och stackspårningen i Result
       Result := Format(sStackErrMsg, [messageStr,stackStr, err]);
    except
       Result := 'Internal error';

    end;
    // Frigör C-strängarna och JSValues efter användning
    //JS_FreeCString(ctx, messageStr);
   // JS_FreeCString(ctx, stackStr);
(*    JS_Free(ctx, messageVal.
    JS_Free(ctx, stackVal);
    JS_Free(ctx, exceptionVal);*)
  end;



  function JSConsoleLog(ctx: JSContext; this_val: JSValueConst; argc: integer; argv: PJSValueConstArr): JSValueRaw; cdecl;
var
  msg: PChar;
  i: Integer;
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
      fullMsg := fullMsg + #13#10 + string(msg); // Konvertera till Pascal-sträng
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
  JS_SetPropertyStr(ctx^, consoleObj, 'log', logFunc);

  // Frigör console-objektet efter användning
//  JS_Free(ctx^, consoleObj);
end;

end.

