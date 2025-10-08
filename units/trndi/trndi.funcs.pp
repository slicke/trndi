unit trndi.funcs;

{$mode ObjFPC}{$H+}
{$ifdef DARWIN}
  {$ModeSwitch objectivec1}
{$endif}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Graphics, trndi.types, Forms, Math,
  fpjson, jsonparser, dateutils
  {$ifdef TrndiExt},trndi.ext.engine, mormot.lib.quickjs, mormot.core.base{$endif}
  {$ifdef DARWIN}, CocoaAll{$endif};

procedure CenterPanelToCaption(Panel: TPanel; margin: integer = 10);
function GetAppPath: string;
function GetLangPath: string;
procedure PaintLbl(Sender: TLabel; OutlineWidth: integer = 1;
  OutlineColor: TColor = clBlack);
procedure LogMessage(const Msg: string);
procedure SortReadingsDescending(var Readings: array of BGReading);
procedure SetPointHeight(l: TPaintBox; Value: single; clientHeight: integer);
function HasNewerRelease(const JsonResponse: string;
  out ReleaseName: string;
  IncludePrerelease: boolean = False): boolean;
function ParseCompilerDate: TDateTime;
function GetNewerVersionURL(const JsonResponse: string;
  IncludePrerelease: boolean = False;
  Platform: string = ''): string;

function privacyIcon(const v: trndi.types.BGValLevel): string;

{$ifdef TrndiExt}
// ---------------------------------------------------------------------------
// Extension Engine Interop Helpers - Ownership Summary
// ---------------------------------------------------------------------------
// callFunc(...) / funcInt/Float/Bool: marshal every Pascal param each call.
// callFuncArrayFirst: firstArray is caller-owned unless autoFreeFirst=true.
// callFuncMixed: raw[] are pre-built JSValueRaw (optionally freed with rawAutoFree);
//                rest[] are marshalled Pascal values (freed when restAutoFree=true).
// callFuncRaw: all args already JSValueRaw; autoFree controls freeing them.
// Helper JSValueRaw* constructors: produce JS values you must free unless you
// pass them through a call with the appropriate autoFree/rawAutoFree set.
// MakeJSArray alias (engine) and JSValueRawArray wrapper create array handles.
// ---------------------------------------------------------------------------
function callFunc(const func: string; params: array of const; out exists: boolean): string;
function callFunc(const func: string; params: array of const): string;
function funcBool(const func: string; params: array of const; const nofunc: boolean): boolean;
function funcInt(const func: string; params: array of const; const nofunc: int64): Int64;
function funcFloat(const func: string; params: array of const; const nofunc: double): double;
// Call a JS function where the first parameter is an already created JS array (JSValueRaw)
// Note: No default for autoFree here to avoid ambiguity with the overload without 'out exists'
// Call a JS function where the first parameter is an already created JS value (Array/Object/etc.).
// autoFree controls freeing of marshalled Rest args; autoFreeFirst controls freeing of the supplied firstArray.
// No defaults on the overload with 'out exists' to avoid ambiguity.
function callFuncArrayFirst(const func: string; const firstArray: JSValueRaw; rest: array of const; out exists: boolean; autoFree: boolean; autoFreeFirst: boolean): string;
function callFuncArrayFirst(const func: string; const firstArray: JSValueRaw; rest: array of const; autoFree: boolean = true; autoFreeFirst: boolean = false): string;
// Mixed: multiple raw JSValueRaw args followed by marshalled Pascal args
function callFuncMixed(const func: string; const raw: array of JSValueRaw; rest: array of const; out exists: boolean; restAutoFree: boolean = true; rawAutoFree: boolean = false): string;
function callFuncMixed(const func: string; const raw: array of JSValueRaw; rest: array of const): string;
// Pure raw list (no Pascal marshalling) using engine.CallFunctionJS
function callFuncRaw(const func: string; const raw: array of JSValueRaw; out exists: boolean; autoFree: boolean = true): string;
function callFuncRaw(const func: string; const raw: array of JSValueRaw): string;
// Helper constructors for readable building of raw argument arrays
function JSValueRawString(const s: RawUtf8): JSValueRaw;
function JSValueRawInt(const v: int64): JSValueRaw;
function JSValueRawFloat(const v: double): JSValueRaw;
function JSValueRawBool(const v: boolean): JSValueRaw;
function JSValueRawArray(const values: array of const): JSValueRaw;
// Convert array of const to array of JSValueRaw with proper type matching
procedure ConvertVarRecsToJSValueRaw(const params: array of const; var jsValues: array of JSValueRaw);
{$endif}

const
  INTERVAL_MINUTES = 5; // Each time interval is 5 minutes
  NUM_DOTS = 10;        // Total number of labels (lDot1 - lDot10)

  BG_API_MIN = 2; // NS can't read lower
  BG_API_MAX = 22.2; // NS can't read higher
  BG_REFRESH = 300000; // 5 min refresh

  APP_BUILD_DATE = {$I %DATE%}; // Returns "2025/07/21"
  APP_BUILD_TIME = {$I %TIME%}; // Returns "14:30:25"

var
  DOT_GRAPH: unicodestring = widechar($2B24);  // Circle
  DOT_FRESH: unicodestring = widechar($2600);  // Sun
  DOT_ADJUST: single = 0; // Multiplyer where dots appear
  DOT_VISUAL_OFFSET: integer = 0;
  // Vertical offset to align limit lines with visual center of dot (compensates for internal whitespace in dot character)
  MAX_MIN: integer = 1440; // Max time to request
  MAX_RESULT: integer = 25; // Max results
  DATA_FRESHNESS_THRESHOLD_MINUTES: integer = 11;
  // Max minutes before data is considered outdated

implementation

procedure CenterPanelToCaption(Panel: TPanel; margin: integer = 10);
var
  TextWidth, PanelWidth, Padding: integer;
  ParentWidth: integer;
begin
  // Calculate text width using the panel's font
  Panel.Canvas.Font := Panel.Font;
  TextWidth := Panel.Canvas.TextWidth(Panel.Caption);

  Padding := margin * 2; // Add 20 pixels (10 on each side, adjust as needed)
  PanelWidth := TextWidth + Padding;

  Panel.Width := PanelWidth;

  // Use parent's client width (TPanel may be placed on form or another control)
  if Assigned(Panel.Parent) then
    ParentWidth := Panel.Parent.ClientWidth
  else
    ParentWidth := Screen.Width; // Fallback

  // Center panel
  Panel.Left := (ParentWidth - Panel.Width) div 2;
end;

{$ifdef darwin}
function GetAppPath: string;
var
  NSAppBundle: NSBundle;
begin
  NSAppBundle := NSBundle.mainBundle;
  Result := UTF8ToString(NSAppBundle.bundlePath.UTF8String);
  result := ExtractFilePath(result);
end;
function getLangPath: string;
var
  bin: string;
begin
  bin := ExtractFilePath(Application.ExeName);
  if DirectoryExists(bin + 'lang') then
    result := bin + 'lang/'
  else
    result := GetAppPath + 'lang/';
end;

{$else}

function GetAppPath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

function getLangPath: string;
begin
  Result := GetAppPath + 'lang/';
end;
{$endif}

procedure PaintLbl(Sender: TLabel; OutlineWidth: integer = 1;
  OutlineColor: TColor = clBlack);
var
  X, Y: integer;
  OriginalColor: TColor;
  TextRect: TRect;
  TextStyle: TTextStyle;
begin
  with Sender as TLabel do
  begin
    // Create draw area
    TextRect := ClientRect;

    // Set the text
    TextStyle := Canvas.TextStyle;
    TextStyle.Alignment := Alignment;
    TextStyle.Layout := Layout;
    TextStyle.Wordbreak := WordWrap;
    TextStyle.SingleLine := not WordWrap;
    TextStyle.Clipping := True;

    // Remember original color
    OriginalColor := Font.Color;

    // Set canvas font
    Canvas.Font := Font;

    // Paint contour ("outline color")
    Canvas.Font.Color := outlinecolor;

    for X := -OutlineWidth to OutlineWidth do
      for Y := -OutlineWidth to OutlineWidth do
        if (X <> 0) or (Y <> 0) then
          Canvas.TextRect(
            Classes.Rect(TextRect.Left + X, TextRect.Top + Y,
            TextRect.Right + X, TextRect.Bottom + Y),
            0, 0, // Not used with text style
            Caption,
            TextStyle)// Make a copy
    ;

    // Re-draw original color
    Canvas.Font.Color := OriginalColor;
    Canvas.TextRect(TextRect, 0, 0, Caption, TextStyle);
  end;
end;

{$ifdef DEBUG}
procedure LogMessage(const Msg: string);
const
  MaxLines = 500; // Max lines in file
var
  LogLines: TStringList;
begin
  LogLines := TStringList.Create;
  try
    // Load log if exists
    if FileExists('trndi.log') then
      LogLines.LoadFromFile('trndi.log');

    // Delete overflowing lines
    while LogLines.Count >= MaxLines do
      LogLines.Delete(0);

    // Add new message
    LogLines.Add('['+DateTimeToStr(Now) + '] ' + Msg);

    // Save
    LogLines.SaveToFile('trndi.log');
  finally
    LogLines.Free;
  end;
end;
{$else}
// Remove when launching
procedure LogMessage(const Msg: string);
begin

end;
{$endif}

// Implement a simple insertion sort for BGReading
procedure SortReadingsDescending(var Readings: array of BGReading);
var
  i, j: integer;
  temp: BGReading;
begin
  for i := 1 to High(Readings) do
  begin
    temp := Readings[i];
    j := i - 1;
    while (j >= 0) and (Readings[j].date < temp.date) do
    begin
      Readings[j + 1] := Readings[j];
      Dec(j);
    end;
    Readings[j + 1] := temp;
  end;
end;

// SetPointHeight procedure
procedure SetPointHeight(L: TPaintBox; Value: single; clientHeight: integer);
var
  Padding, UsableHeight, Position: integer;
begin
  // Define padding and usable height for scaling based on provided client height
  Padding := Round(clientHeight * 0.1); // 10% of the client height
  UsableHeight := clientHeight - (Padding * 2);

  // Clamp Value within range
  if Value < BG_API_MIN then
    Value := BG_API_MIN
  else if Value > BG_API_MAX then
    Value := BG_API_MAX;

  // Calculate position as a proportion of the usable height
  Position := Padding + Round((Value - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) *
    UsableHeight);

  // Apply the calculated position to the label's Top property
  // Place dot relative to the same clientHeight reference. Keep 1px inside bottom edge
  L.Top := (clientHeight - Position) - 1;

  // Optional debug/logging to verify placement
  LogMessage(Format('Label %s: Value=%.2f, Top=%d', [L.Name, Value, L.Top]));
end;

procedure setTimeRange(mins, Count: integer);
begin
  MAX_MIN := min(mins, 20); // Max time to request
  MAX_RESULT := min(Count, 2); // Max results
end;

function ParseCompilerDate: TDateTime;
var
  DateParts: TStringArray;
  TimeParts: TStringArray;
  Year, Month, Day, Hour, Min, Sec: word;
begin
  // Parse date: "2025/07/21"
  DateParts := APP_BUILD_DATE.Split('/');
  Year := StrToInt(DateParts[0]);
  Month := StrToInt(DateParts[1]);
  Day := StrToInt(DateParts[2]);

  // Parse time: "14:30:25"
  TimeParts := APP_BUILD_TIME.Split(':');
  Hour := StrToInt(TimeParts[0]);
  Min := StrToInt(TimeParts[1]);
  Sec := StrToInt(TimeParts[2]);

  Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
end;

function HasNewerRelease(const JsonResponse: string;
  out ReleaseName: string;
  IncludePrerelease: boolean = False): boolean;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  ReleaseDateStr: string;
  ReleaseDate, BuildDate: TDateTime;
  Year, Month, Day, Hour, Min, Sec: word;
  i: integer;
  IsPrerelease: boolean;
begin
  Result := False;
  ReleaseName := '';
  JsonData := nil;

  try
    BuildDate := ParseCompilerDate;
    JsonData := GetJSON(JsonResponse);

    if JsonData is TJSONArray then
    begin
      JsonArray := TJSONArray(JsonData);
      for i := 0 to JsonArray.Count - 1 do
      begin
        JsonObj := TJSONObject(JsonArray[i]);
        IsPrerelease := JsonObj.Get('prerelease', False);
        if not IncludePrerelease and IsPrerelease then
          Continue;

        ReleaseDateStr := JsonObj.Get('published_at', '');
        if Length(ReleaseDateStr) >= 19 then
        begin
          Year := StrToInt(Copy(ReleaseDateStr, 1, 4));
          Month := StrToInt(Copy(ReleaseDateStr, 6, 2));
          Day := StrToInt(Copy(ReleaseDateStr, 9, 2));
          Hour := StrToInt(Copy(ReleaseDateStr, 12, 2));
          Min := StrToInt(Copy(ReleaseDateStr, 15, 2));
          Sec := StrToInt(Copy(ReleaseDateStr, 18, 2));
          ReleaseDate := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);

          if ReleaseDate > BuildDate then
          begin
            ReleaseName := JsonObj.Get('name', JsonObj.Get('tag_name', ''));
            Result := True;
            Exit;
          end;
        end;
      end;
    end
    else if JsonData is TJSONObject then
    begin
      JsonObj := TJSONObject(JsonData);
      ReleaseDateStr := JsonObj.Get('published_at', '');
      if Length(ReleaseDateStr) >= 19 then
      begin
        Year := StrToInt(Copy(ReleaseDateStr, 1, 4));
        Month := StrToInt(Copy(ReleaseDateStr, 6, 2));
        Day := StrToInt(Copy(ReleaseDateStr, 9, 2));
        Hour := StrToInt(Copy(ReleaseDateStr, 12, 2));
        Min := StrToInt(Copy(ReleaseDateStr, 15, 2));
        Sec := StrToInt(Copy(ReleaseDateStr, 18, 2));
        ReleaseDate := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);

        if ReleaseDate > BuildDate then
        begin
          ReleaseName := JsonObj.Get('name', JsonObj.Get('tag_name', ''));
          Result := True;
        end;
      end;
    end;

  except
    Result := False;
    ReleaseName := '';
  end;

  if Assigned(JsonData) then
    JsonData.Free;
end;

function GetNewerVersionURL(const JsonResponse: string;
  IncludePrerelease: boolean = False;
  Platform: string = ''): string;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  ReleaseDateStr, ReleaseTitle: string;
  ReleaseDate, BuildDate: TDateTime;
  Year, Month, Day, Hour, Min, Sec: word;
  i: integer;
  IsPrerelease, MatchesPlatform: boolean;
begin
  Result := ''; // Empty if no newer version
  JsonData := nil;

  try
    BuildDate := ParseCompilerDate;
    JsonData := GetJSON(JsonResponse);

    // Handle single release object (from /releases/latest)
    if JsonData is TJSONObject then
    begin
      JsonObj := TJSONObject(JsonData);

      // Check platform filter
      if Platform <> '' then
      begin
        ReleaseTitle := JsonObj.Get('name', JsonObj.Get('tag_name', ''));
        MatchesPlatform := False;

        case LowerCase(Platform) of
          'windows': MatchesPlatform := Pos('windows', LowerCase(ReleaseTitle)) > 0;
          'mac', 'macos': MatchesPlatform :=
              (Pos('mac', LowerCase(ReleaseTitle)) > 0) or
              (Pos('macos', LowerCase(ReleaseTitle)) > 0);
          'linux': MatchesPlatform := Pos('linux', LowerCase(ReleaseTitle)) > 0;
          else
            MatchesPlatform := Pos(LowerCase(Platform), LowerCase(ReleaseTitle)) > 0;
        end;

        if not MatchesPlatform then
          Exit; // No match, return empty
      end;

      // Check date
      ReleaseDateStr := JsonObj.Get('published_at', '');
      if Length(ReleaseDateStr) >= 19 then
      begin
        Year := StrToInt(Copy(ReleaseDateStr, 1, 4));
        Month := StrToInt(Copy(ReleaseDateStr, 6, 2));
        Day := StrToInt(Copy(ReleaseDateStr, 9, 2));
        Hour := StrToInt(Copy(ReleaseDateStr, 12, 2));
        Min := StrToInt(Copy(ReleaseDateStr, 15, 2));
        Sec := StrToInt(Copy(ReleaseDateStr, 18, 2));

        ReleaseDate := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);

        if ReleaseDate > BuildDate then
          Result := JsonObj.Get('html_url', ''); // Return URL for single release
      end;
    end

    // Handle array of releases (from /releases)
    else if JsonData is TJSONArray then
    begin
      JsonArray := TJSONArray(JsonData);

      for i := 0 to JsonArray.Count - 1 do
      begin
        JsonObj := TJSONObject(JsonArray[i]);
        IsPrerelease := JsonObj.Get('prerelease', False);

        // Filter by prerelease setting
        if IncludePrerelease then
        begin
          if not IsPrerelease then Continue; // Want prerelease, skip stable
        end
        else
        begin
          if IsPrerelease then Continue; // Want stable, skip prerelease
        end;

        // Check platform filter
        if Platform <> '' then
        begin
          ReleaseTitle := JsonObj.Get('name', JsonObj.Get('tag_name', ''));
          MatchesPlatform := False;

          case LowerCase(Platform) of
            'windows': MatchesPlatform := Pos('windows', LowerCase(ReleaseTitle)) > 0;
            'mac', 'macos': MatchesPlatform :=
                (Pos('mac', LowerCase(ReleaseTitle)) > 0) or
                (Pos('macos', LowerCase(ReleaseTitle)) > 0);
            'linux': MatchesPlatform := Pos('linux', LowerCase(ReleaseTitle)) > 0;
            else
              MatchesPlatform := Pos(LowerCase(Platform), LowerCase(ReleaseTitle)) > 0;
          end;

          if not MatchesPlatform then
            Continue;
        end;

        // Check date
        ReleaseDateStr := JsonObj.Get('published_at', '');
        if Length(ReleaseDateStr) >= 19 then
        begin
          Year := StrToInt(Copy(ReleaseDateStr, 1, 4));
          Month := StrToInt(Copy(ReleaseDateStr, 6, 2));
          Day := StrToInt(Copy(ReleaseDateStr, 9, 2));
          Hour := StrToInt(Copy(ReleaseDateStr, 12, 2));
          Min := StrToInt(Copy(ReleaseDateStr, 15, 2));
          Sec := StrToInt(Copy(ReleaseDateStr, 18, 2));

          ReleaseDate := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);

          if ReleaseDate > BuildDate then
          begin
            Result := JsonObj.Get('html_url', ''); // Return URL
            Exit; // Found it, exit
          end;
        end;
      end;
    end;

  except
    Result := '';
  end;

  if Assigned(JsonData) then
    JsonData.Free;
end;

function privacyIcon(const v: trndi.types.BGValLevel): string;
begin
  Result := '?';
  case v of
    trndi.types.BGHigh: begin
      Result := '⭱';
    end;
    trndi.types.BGLOW: begin
      Result := '⭳';
    end;
    trndi.types.BGRange: begin
      Result := '✓';
    end;
    trndi.types.BGRangeHI: begin
      Result := '✓⁺';
    end;
    trndi.types.BGRangeLO: begin
      Result := '✓⁻';
    end;
  end;
end;

{$ifdef TrndiExt}
// Call a JS function and check that it exists too
function callFunc(const func: string; params: array of const; out exists: boolean): string;
begin
  result := '';
  if not Assigned(TTrndiExtEngine.Instance) then begin  // Safe
    exists := false;
    exit;
  end;

  exists := TTrndiExtEngine.Instance.FunctionExists(func); // We assign the out var

  if not exists then // Exit if the function doesnt exist
    Exit;

  result := TTrndiExtEngine.Instance.CallFunction(func, params); // Call the function, gives a string
end;

function callFunc(const func: string; params: array of const): string;
var
  ex: boolean;
begin
  result := callFunc(func, params, ex);
end;

// Calls a function and converts it as bool
function funcBool(const func: string; params: array of const; const nofunc: boolean): boolean;
var
  ex: boolean;
begin
  result := callFunc(func,params,ex) = 'true';
  if not ex then
    result := nofunc;
end;

function funcInt(const func: string; params: array of const; const nofunc: Int64): Int64;
var
  ex: boolean;
  res: string;
  i: int64;
begin
  result := 0;
  res := callFunc(func,params,ex);

  if not ex then
    result := nofunc;

  if TryStrToInt64(res, i) then
    result := i
  else
    raise Exception.Create('Cannot interpret extension value '+res+' as integer!');
end;

function funcFloat(const func: string; params: array of const; const nofunc: double): double;
var
  d: double;
  fs: TFormatSettings;
  res: string;
  ex: boolean;
begin
  result := 0;
  res := callFunc(func,params,ex);
  if not ex then
    result := nofunc;

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  if TryStrToFloat(res, d, fs) then
    result := d
  else
    raise Exception.Create('Cannot interpret extension value '+res+' as float!');
end;

// Wrapper around engine.CallFunctionArrayFirst
function callFuncArrayFirst(const func: string; const firstArray: JSValueRaw; rest: array of const; out exists: boolean; autoFree: boolean; autoFreeFirst: boolean): string;
begin
  result := '';
  if not Assigned(TTrndiExtEngine.Instance) then begin exists := false; exit; end;
  exists := TTrndiExtEngine.Instance.FunctionExists(func);
  if not exists then exit;
  result := TTrndiExtEngine.Instance.CallFunctionArrayFirst(func, firstArray, rest, autoFree, autoFreeFirst);
end;

function callFuncArrayFirst(const func: string; const firstArray: JSValueRaw; rest: array of const; autoFree: boolean; autoFreeFirst: boolean): string;
var ex: boolean; 
begin
  result := callFuncArrayFirst(func, firstArray, rest, ex, autoFree, autoFreeFirst);
end;

// Mixed raw + marshalled
function callFuncMixed(const func: string; const raw: array of JSValueRaw; rest: array of const; out exists: boolean; restAutoFree: boolean; rawAutoFree: boolean): string;
begin
  result := '';
  if not Assigned(TTrndiExtEngine.Instance) then begin exists := false; exit; end;
  exists := TTrndiExtEngine.Instance.FunctionExists(func);
  if not exists then exit;
  result := TTrndiExtEngine.Instance.CallFunctionMixed(func, raw, rest, restAutoFree, rawAutoFree);
end;

function callFuncMixed(const func: string; const raw: array of JSValueRaw; rest: array of const): string;
var ex: boolean;
begin
  result := callFuncMixed(func, raw, rest, ex, true, false);
end;

// Pure raw argument list (no additional marshalled values)
function callFuncRaw(const func: string; const raw: array of JSValueRaw; out exists: boolean; autoFree: boolean): string;
begin
  Result := '';
  if not Assigned(TTrndiExtEngine.Instance) then begin exists := false; exit; end;
  exists := TTrndiExtEngine.Instance.FunctionExists(func);
  if not exists then exit;
  Result := TTrndiExtEngine.Instance.CallFunctionJS(func, raw, autoFree);
end;

function callFuncRaw(const func: string; const raw: array of JSValueRaw): string;
var ex: boolean;
begin
  result := callFuncRaw(func, raw, ex, true);
end;

// Helper constructors
function JSValueRawString(const s: RawUtf8): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSString(s);
end;

function JSValueRawInt(const v: int64): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSInt64(v);
end;

function JSValueRawFloat(const v: double): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSFloat(v);
end;

function JSValueRawBool(const v: boolean): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSBool(v);
end;

function JSValueRawArray(const values: array of const): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSArray(values);
end;

procedure ConvertVarRecsToJSValueRaw(const params: array of const; var jsValues: array of JSValueRaw);
var
  i: integer;
begin
  for i := 0 to High(params) do
  begin
    case params[i].VType of
      vtString:      jsValues[i] := JSValueRawString(RawUtf8(ShortString(params[i].VString^)));
      vtAnsiString:  jsValues[i] := JSValueRawString(RawUtf8(AnsiString(params[i].VAnsiString)));
      vtUnicodeString: jsValues[i] := JSValueRawString(RawUtf8(UnicodeString(params[i].VUnicodeString)));
      vtInteger:     jsValues[i] := JSValueRawInt(params[i].VInteger);
      vtInt64:       jsValues[i] := JSValueRawInt(params[i].VInt64^);
      vtExtended:    jsValues[i] := JSValueRawFloat(params[i].VExtended^);
      vtBoolean:     jsValues[i] := JSValueRawBool(params[i].VBoolean);
    else
      jsValues[i] := JSValueRawString('unsupported_type');
    end;
  end;
end;
{$endif}

end.
