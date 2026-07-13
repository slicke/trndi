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
unit trndi.funcs;

{$mode ObjFPC}{$H+}
{$ifdef DARWIN}
{$ModeSwitch objectivec1}
{$endif}

interface

uses
Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Graphics, trndi.types, Forms, Math,
fpjson, jsonparser, dateutils, buildinfo, trndi.log
{$ifdef TrndiExt},trndi.ext.engine, mormot.lib.quickjs, mormot.core.base{$endif}
{$ifdef DARWIN}, CocoaAll, nsutils.nshelpers{$endif}
{$ifdef DEBUG}, slicke.ux.alert{$endif};

{$ifdef DEBUG}
type
  _arr2 = array[1..2] of string;
  _arr3 = array[1..3] of string;
{$endif}

procedure CenterPanelToCaption(Panel: TPanel; margin: integer = 10);
function GetAppPath: string;
function GetLangPath: string;
procedure PaintLbl(Sender: TLabel; OutlineWidth: integer = 1;
OutlineColor: TColor = clBlack);
procedure SortReadingsDescending(var Readings: array of BGReading);
procedure SortReadingsAscending(var Readings: array of BGReading);
procedure SetPointHeight(l: TControl; Value: single; clientHeight: integer);
function CalculateTrendFromDelta(delta: single): BGTrend;
function CalculateTrendAngle(delta: single): single;
function TrendToAngle(trend: BGTrend): single;
function HasNewerRelease(const JsonResponse: string;
out ReleaseName: string;
IncludePrerelease: boolean = false): boolean;
function ParseCompilerDate: TDateTime;
function GetNewerVersionURL(const JsonResponse: string;
IncludePrerelease: boolean = false;
Platform: string = ''): string;
function IsPRBuild: boolean;
function GetLatestReleaseName(const JsonResponse: string): string;

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
function funcInt(const func: string; params: array of const; const nofunc: int64): int64;
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
{$ifdef DEBUG}
function debugParams(arr: TStringArray): string; overload;
function debugParams(arr: TStringList): string; overload;
function debugParams(arr: _arr2): string; overload;
function debugParams(arr: _arr3): string; overload;
{$endif}

const
INTERVAL_MINUTES = 5; // Each time interval is 5 minutes
NUM_DOTS = 10;        // Total number of named dot fields (lDot1 - lDot10); also the default
MAX_DOT_COUNT = 288;  // Maximum trend dots supported (24 h at 5-min intervals)

BG_API_MIN = 2; // NS can't read lower
BG_API_MAX = 22.2; // NS can't read higher
BG_REFRESH = 300000; // 5 min refresh

APP_BUILD_DATE = {$I %DATE%}; // Returns "2025/07/21"
APP_BUILD_TIME = {$I %TIME%}; // Returns "14:30:25"

// Sentinel caption values that flag a dot's display state. They are never
// rendered as text — DotPaint draws a circle when it sees them — so any
// otherwise-unused control codes work.
DOT_GRAPH = #1;  // Normal circle
DOT_FRESH = #2;  // Most-recent reading (drawn with an outer ring)
DOT_PREDICT = #3; // Predicted future reading (drawn as a hollow ring)

PREDICTION_DOT_COUNT = 3; // Future prediction dots shown on the main trend (5/10/15 min)

ARROW_MIN_TILT = 10;  // Smallest lean shown once the value is actually moving (degrees); keeps direction readable
ARROW_MAX_ANGLE = 70; // Cap for the rotating trend arrow (degrees from flat); keeps it a lean, never vertical

var
DOT_ADJUST: single = 0; // Multiplyer where dots appear
MAX_MIN: integer = 1440; // Max time to request
MAX_RESULT: integer = 25; // Max results (scaled up after API creation based on reporting interval)
ACTIVE_DOTS: integer = 10; // Runtime dot count for the main trend display (saved as 'ux.dot_count')
DATA_FRESHNESS_THRESHOLD_MINUTES: integer = 11; // Max minutes before data is considered outdated

{$ifdef DEBUG}
DEBUG_LOG_ALERT: boolean = false;
TRNDI_DEBUG_LOG_ALERT: boolean = false;
TRNDI_DEBUG_LOG_ALERT_SNOOZE: TDateTime;
{$endif}
implementation


{$ifdef DEBUG}
function debugParams(arr: _arr2): string;
begin
  result := Format('%s :: %s', [arr[1], arr[2]]);
end;
function debugParams(arr: _arr3): string;
begin
  result := Format('%s :: %s :: %s', [arr[1], arr[2], arr[3]]);
end;
function debugParams(arr: TStringArray): string;
var
s: string;
begin
  for s in arr do
    result := result.join(' :: ', s);
end;

function debugParams(arr: TStringList): string;
var
s: TStringList;
begin
  s := TStringList.Create;
  s.AddDelimitedText(arr.DelimitedText, arr.Delimiter, true);
  s.Delimiter := '`';
  result := StringReplace(s.DelimitedText, '`', ' :: ', [rfReplaceAll]);
  s.free;
end;
{$endif}

procedure CenterPanelToCaption(Panel: TPanel; margin: integer = 10);
var
  TextWidth, PanelWidth, Padding: integer;
  ParentWidth: integer;
  Bmp: TBitmap;
begin
  // Measure caption via a temporary TBitmap. Accessing TPanel/TLabel.Canvas
  // outside a paint event can SIGABRT on the Cocoa widgetset; see
  // memory `cocoa-label-canvas` for the full explanation.
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(1, 1);
    Bmp.Canvas.Font.Assign(Panel.Font);
    TextWidth := Bmp.Canvas.TextWidth(Panel.Caption);
  finally
    Bmp.Free;
  end;

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
  Result := UTF8ToString(NSAppBundle.bundlePath.utf8string);
  result := ExtractFilePath(result);
end;
function getLangPath: string;
var
  bin: string;
  parent: string;
begin
  bin := ExtractFilePath(Application.ExeName);
  if DirectoryExists(bin + 'lang') then
    result := bin + 'lang/'
  else
  begin
    parent := ExtractFilePath(ExcludeTrailingPathDelimiter(bin));
    if DirectoryExists(parent + 'lang') then
      result := parent + 'lang/'
    else
      result := GetAppPath + 'lang/';
  end;
end;

{$else}

function GetAppPath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

function getLangPath: string;
begin
  Result := GetAppPath + 'lang/';
  if not DirectoryExists(Result) then
  begin
    Result := ExtractFilePath(ExcludeTrailingPathDelimiter(GetAppPath)) + 'lang/';
    if not DirectoryExists(Result) then
      Result := GetAppPath + 'lang/';
  end;
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
    TextStyle.Clipping := true;

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

procedure SortReadingsCore(var Readings: array of BGReading; ascending: boolean);
  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: BGReading;
  begin
    repeat
      I := L;
      J := R;
      P := Readings[(L + R) div 2];
      repeat
        if ascending then
        begin
          while Readings[I].date < P.date do Inc(I);
          while Readings[J].date > P.date do Dec(J);
        end
        else
        begin
          while Readings[I].date > P.date do Inc(I);
          while Readings[J].date < P.date do Dec(J);
        end;
        if I <= J then
        begin
          T := Readings[I];
          Readings[I] := Readings[J];
          Readings[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;
begin
  if Length(Readings) > 1 then
    QuickSort(Low(Readings), High(Readings));
end;

procedure SortReadingsDescending(var Readings: array of BGReading);
begin
  SortReadingsCore(Readings, false);
end;

procedure SortReadingsAscending(var Readings: array of BGReading);
begin
  SortReadingsCore(Readings, true);
end;

// SetPointHeight procedure
procedure SetPointHeight(L: TControl; Value: single; clientHeight: integer);
var
  Padding, UsableHeight, Position: integer;
  {$ifdef DARWIN}
  LimitTop: Integer;
  HalfGlyph: integer;
  {$endif}
begin
  // Define padding and usable height for scaling based on provided client height
  Padding := Round(clientHeight * 0.1); // 10% of the client height
  UsableHeight := clientHeight - (Padding * 2);

  // Clamp Value within range
  if Value < BG_API_MIN then
    Value := BG_API_MIN
  else
  if Value > BG_API_MAX then
    Value := BG_API_MAX;

  // Calculate position as a proportion of the usable height
  Position := Padding + Round((Value - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) *
    UsableHeight);

  // Apply the calculated position to the label's Top property
  // Place dot relative to the same clientHeight reference. Keep 1px inside bottom edge
  {$ifdef DARWIN}
  // On macOS, TLabel draws its text centred vertically (Layout = tlCenter).
  // Position the label so the visual centre of the glyph sits at the glucose
  // value's Y coordinate. Font.Size is a reliable proxy for the circle glyph's
  // visual height and is identical across all active dots (set in ResizeDot),
  // so HalfGlyph is consistent — no per-dot unevenness, no excess clamping.
  HalfGlyph := Max(L.Font.Size div 2, 4);
  L.Top := (clientHeight - Position) - HalfGlyph;

  // Ensure Top is within a valid range. Compute a non-negative lower bound
  // for the maximum allowed Top to avoid negative positions when the
  // available clientHeight is smaller than the font size.
  LimitTop := clientHeight - L.Font.Size;
  if LimitTop < 0 then
    LimitTop := 0;
  L.Top := Min(Max(L.Top, 0), LimitTop);

  TrndiDLog(Format('Label %s: Value=%.2f, Top=%d, FontSize=%d',
    [L.Name, Value, L.Top, L.Font.Size]));
  {$else}
  L.Top := (clientHeight - Position) - 1;
  TrndiDLog(Format('Label %s: Value=%.2f, Top=%d, Height=%d', [L.Name, Value, L.Top, L.Height]));
  {$endif}
end;

function CalculateTrendFromDelta(delta: single): BGTrend;
begin
  // Calculate trend based on delta in mg/dL over 5 minutes
  // Based on standard CGM trend arrow thresholds
  if delta <= -15 then          // ≤-3 mg/dL/min
    Result := TdDoubleDown
  else
  if delta <= -10 then     // ≤-2 mg/dL/min
    Result := TdSingleDown
  else
  if delta <= -5 then      // ≤-1 mg/dL/min
    Result := TdFortyFiveDown
  else
  if delta < 5 then        // -1 to +1 mg/dL/min
    Result := TdFlat
  else
  if delta < 10 then       // +1 to +2 mg/dL/min
    Result := TdFortyFiveUp
  else
  if delta < 15 then       // +2 to +3 mg/dL/min
    Result := TdSingleUp
  else                          // ≥+3 mg/dL/min
    Result := TdDoubleUp;
end;

{ Maps a glucose delta to a continuous arrow rotation angle.

  Deliberately gentle, but always readable: only near-zero noise (within a tiny
  dead-band) stays flat. As soon as the value is genuinely moving the arrow
  leans by at least ARROW_MIN_TILT so the direction is unmistakable, then the
  tilt grows linearly with the rate of change — reaching 45° at a clearly-moving
  ~15 mg/dL (~0.8 mmol/L) over 5 minutes and capped at ARROW_MAX_ANGLE so even a
  fast move reads as a lean rather than a vertical spike.

  @param(delta Change in mg/dL over a 5-minute interval.)
  @returns(Rotation in degrees: 0 = flat, positive = rising, negative = falling;
           magnitude is either 0 or within ARROW_MIN_TILT..ARROW_MAX_ANGLE.) }
function CalculateTrendAngle(delta: single): single;
const
  DEAD_BAND = 0.5;         // mg/dL per 5 min; only near-zero noise stays flat
  DELTA_AT_45 = 15.0;      // mg/dL per 5 min mapped to 45°
var
  mag: single;
begin
  if Abs(delta) <= DEAD_BAND then
    Exit(0);
  // Magnitude: linear, but never so shallow it looks flat, never past the cap.
  mag := (Abs(delta) / DELTA_AT_45) * 45.0;
  if mag < ARROW_MIN_TILT then
    mag := ARROW_MIN_TILT
  else
  if mag > ARROW_MAX_ANGLE then
    mag := ARROW_MAX_ANGLE;
  if delta < 0 then
    Result := -mag
  else
    Result := mag;
end;

{ Representative rotation angle for a discrete trend arrow.

  Used as a fallback for the rotating arrow when no usable delta is available
  (e.g. the first reading, or a stale gap), so the arrow still points in the
  direction the CGM reported. Mirrors CalculateTrendAngle's ±90° range.

  @param(trend The discrete trend enumeration.)
  @returns(Rotation in degrees: 0 = flat, positive = rising, negative = falling.) }
function TrendToAngle(trend: BGTrend): single;
begin
  // Soft angles consistent with CalculateTrendAngle's gentle scale.
  case trend of
  TdDoubleUp:
    Result := 60;
  TdSingleUp:
    Result := 40;
  TdFortyFiveUp:
    Result := 20;
  TdFortyFiveDown:
    Result := -20;
  TdSingleDown:
    Result := -40;
  TdDoubleDown:
    Result := -60;
  else                          // TdFlat, TdNotComputable, TdPlaceholder
    Result := 0;
  end;
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

function TryExtractBuildNumber(const Value: string; out BuildNumber: int64): boolean;
var
  digits: string;
  i: integer;
begin
  digits := '';
  for i := 1 to Length(Value) do
    if Value[i] in ['0'..'9'] then
      digits := digits + Value[i];

  Result := (digits <> '') and TryStrToInt64(digits, BuildNumber);
end;

function TryExtractBuildNumberFromJson(const JsonObj: TJSONObject; out BuildNumber: int64): boolean;
begin
  Result := TryExtractBuildNumber(JsonObj.Get('tag_name', ''), BuildNumber);
  if not Result then
    Result := TryExtractBuildNumber(JsonObj.Get('name', ''), BuildNumber);
end;

function TryExtractPublishedAt(const JsonObj: TJSONObject; out PublishedAt: TDateTime): boolean;
var
  ReleaseDateStr: string;
  Year, Month, Day, Hour, Min, Sec: integer;
begin
  ReleaseDateStr := JsonObj.Get('published_at', '');
  if Length(ReleaseDateStr) < 19 then
    Exit(false);

  Year := StrToIntDef(Copy(ReleaseDateStr, 1, 4), -1);
  Month := StrToIntDef(Copy(ReleaseDateStr, 6, 2), -1);
  Day := StrToIntDef(Copy(ReleaseDateStr, 9, 2), -1);
  Hour := StrToIntDef(Copy(ReleaseDateStr, 12, 2), -1);
  Min := StrToIntDef(Copy(ReleaseDateStr, 15, 2), -1);
  Sec := StrToIntDef(Copy(ReleaseDateStr, 18, 2), -1);

  if (Year < 0) or (Month < 0) or (Day < 0) or (Hour < 0) or (Min < 0) or (Sec < 0) then
    Exit(false);

  try
    PublishedAt := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
    // GitHub `published_at` is in UTC — convert to local time for comparisons
    PublishedAt := UniversalTimeToLocal(PublishedAt);
    Result := true;
  except
    Result := false;
  end;
end;

// The binary's build timestamp used for release comparisons. CI stamps
// BUILD_DATE as ISO 8601 UTC ('2026-06-13T17:05:00Z'); prefer it so PR builds
// (which have no numeric build id) compare by when CI actually built them.
// Falls back to the compile-time date when BUILD_DATE is not set ('local').
function GetBuildDateTime: TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: integer;
begin
  if Length(BUILD_DATE) >= 19 then
  begin
    Year := StrToIntDef(Copy(BUILD_DATE, 1, 4), -1);
    Month := StrToIntDef(Copy(BUILD_DATE, 6, 2), -1);
    Day := StrToIntDef(Copy(BUILD_DATE, 9, 2), -1);
    Hour := StrToIntDef(Copy(BUILD_DATE, 12, 2), -1);
    Min := StrToIntDef(Copy(BUILD_DATE, 15, 2), -1);
    Sec := StrToIntDef(Copy(BUILD_DATE, 18, 2), -1);

    if (Year >= 0) and (Month >= 0) and (Day >= 0) and (Hour >= 0) and
      (Min >= 0) and (Sec >= 0) then
    try
      // BUILD_DATE is UTC — convert to local time like `published_at`
      Exit(UniversalTimeToLocal(EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0)));
    except
      // fall through to the compile-time date
    end;
  end;

  Result := ParseCompilerDate;
end;

function HasNewerRelease(const JsonResponse: string;
out ReleaseName: string;
IncludePrerelease: boolean = false): boolean;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  BuildDate: TDateTime;
  BuildDateValid: boolean;
  CurrentBuildNumber, ReleaseBuildNumber: int64;
  HasNumericBuild: boolean;
  CurrentTag: string;
  i: integer;

function EnsureBuildDate: TDateTime;
  begin
    if not BuildDateValid then
    begin
      BuildDate := GetBuildDateTime;
      BuildDateValid := true;
    end;
    Result := BuildDate;
  end;

function EvaluateRelease(const Obj: TJSONObject): boolean;
  var
    CandidateName, ReleaseTag: string;
    PublishedAt: TDateTime;
  begin
    Result := false;

    if (not IncludePrerelease) and Obj.Get('prerelease', false) then
      Exit;

    ReleaseTag := Obj.Get('tag_name', '');
    if (CurrentTag <> '') and (CurrentTag <> 'dev') and (ReleaseTag <> '') and SameText(ReleaseTag, CurrentTag) then
      Exit;

    CandidateName := Obj.Get('name', ReleaseTag);

    if HasNumericBuild and TryExtractBuildNumberFromJson(Obj, ReleaseBuildNumber) then
    begin
      if ReleaseBuildNumber > CurrentBuildNumber then
      begin
        ReleaseName := CandidateName;
        Exit(true);
      end;
      Exit;
    end;

    if TryExtractPublishedAt(Obj, PublishedAt) and (PublishedAt > EnsureBuildDate) then
    begin
      ReleaseName := CandidateName;
      Exit(true);
    end;
  end;
begin
  Result := false;
  ReleaseName := '';
  JsonData := nil;

  try
    HasNumericBuild := TryStrToInt64(BUILD_NUMBER, CurrentBuildNumber);
    CurrentTag := BUILD_TAG;
    BuildDateValid := false;
    JsonData := GetJSON(JsonResponse);

    if JsonData is TJSONArray then
    begin
      JsonArray := TJSONArray(JsonData);
      for i := 0 to JsonArray.Count - 1 do
      begin
        JsonObj := TJSONObject(JsonArray[i]);
        if EvaluateRelease(JsonObj) then
        begin
          Result := true;
          Exit;
        end;
      end;
    end
    else
    if JsonData is TJSONObject then
    begin
      JsonObj := TJSONObject(JsonData);
      if EvaluateRelease(JsonObj) then
        Result := true;
    end;

  except
    Result := false;
    ReleaseName := '';
  end;

  if Assigned(JsonData) then
    JsonData.Free;
end;

function GetNewerVersionURL(const JsonResponse: string;
IncludePrerelease: boolean = false;
Platform: string = ''): string;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  BuildDate: TDateTime;
  BuildDateValid: boolean;
  CurrentBuildNumber, ReleaseBuildNumber: int64;
  HasNumericBuild: boolean;
  CurrentTag: string;
  PublishedAt: TDateTime;
  i: integer;
  IsPrerelease: boolean;

function EnsureBuildDate: TDateTime;
  begin
    if not BuildDateValid then
    begin
      BuildDate := GetBuildDateTime;
      BuildDateValid := true;
    end;
    Result := BuildDate;
  end;

function MatchesPlatformFilter(const Title: string): boolean;
  var
    LowerTitle, LowerPlatform: string;
  begin
    if Platform = '' then
      Exit(true);

    LowerTitle := LowerCase(Title);
    LowerPlatform := LowerCase(Platform);

    case LowerPlatform of
    'windows':
      Result := Pos('windows', LowerTitle) > 0;
    'mac', 'macos':
      Result := (Pos('mac', LowerTitle) > 0) or (Pos('macos', LowerTitle) > 0);
    'linux':
      Result := Pos('linux', LowerTitle) > 0;
    else
      Result := Pos(LowerPlatform, LowerTitle) > 0;
    end;
  end;

function EvaluateRelease(const Obj: TJSONObject): string;
  var
    ReleaseTag, Title, Url: string;
  begin
    Result := '';

    IsPrerelease := Obj.Get('prerelease', false);
    if IncludePrerelease then
    begin
      if not IsPrerelease then
        Exit;
    end
    else
    if IsPrerelease then
      Exit;

    Title := Obj.Get('name', Obj.Get('tag_name', ''));
    if not MatchesPlatformFilter(Title) then
      Exit;

    Url := Obj.Get('html_url', '');
    if Url = '' then
      Exit;

    ReleaseTag := Obj.Get('tag_name', '');
    if (CurrentTag <> '') and (CurrentTag <> 'dev') and (ReleaseTag <> '') and SameText(ReleaseTag, CurrentTag) then
      Exit;

    if HasNumericBuild and TryExtractBuildNumberFromJson(Obj, ReleaseBuildNumber) then
    begin
      if ReleaseBuildNumber > CurrentBuildNumber then
        Result := Url;
      Exit;
    end;

    if TryExtractPublishedAt(Obj, PublishedAt) and (PublishedAt > EnsureBuildDate) then
      Result := Url;
  end;
begin
  Result := ''; // Empty if no newer version
  JsonData := nil;

  try
    HasNumericBuild := TryStrToInt64(BUILD_NUMBER, CurrentBuildNumber);
    CurrentTag := BUILD_TAG;
    BuildDateValid := false;
    JsonData := GetJSON(JsonResponse);

    // Handle single release object (from /releases/latest)
    if JsonData is TJSONObject then
    begin
      JsonObj := TJSONObject(JsonData);

      Result := EvaluateRelease(JsonObj);
    end

    // Handle array of releases (from /releases)
    else
    if JsonData is TJSONArray then
    begin
      JsonArray := TJSONArray(JsonData);

      for i := 0 to JsonArray.Count - 1 do
      begin
        JsonObj := TJSONObject(JsonArray[i]);
        Result := EvaluateRelease(JsonObj);
        if Result <> '' then
          Exit;
      end;
    end;

  except
    Result := '';
  end;

  if Assigned(JsonData) then
    JsonData.Free;
end;

// True when this binary was produced by CI for a pull request
// (write-buildinfo sets BUILD_NUMBER = 'PR-<n>' on pull_request events)
function IsPRBuild: boolean;
begin
  Result := CI and (Copy(BUILD_NUMBER, 1, 3) = 'PR-');
end;

// Extract the display name (or tag) of a release from a GitHub
// /releases/latest JSON response; empty string if it cannot be parsed
function GetLatestReleaseName(const JsonResponse: string): string;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
begin
  Result := '';
  JsonData := nil;

  try
    JsonData := GetJSON(JsonResponse);
    if JsonData is TJSONObject then
    begin
      JsonObj := TJSONObject(JsonData);
      Result := JsonObj.Get('name', JsonObj.Get('tag_name', ''));
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
  trndi.types.BGHigh:
    Result := '⭱';
  trndi.types.BGLOW:
    Result := '⭳';
  trndi.types.BGRange:
    Result := '✓';
  trndi.types.BGRangeHI:
    Result := '✓⁺';
  trndi.types.BGRangeLO:
    Result := '✓⁻';
  end;
end;

{$ifdef TrndiExt}
// Call a JS function and check that it exists too
function callFunc(const func: string; params: array of const; out exists: boolean): string;
begin
  result := '';
  if not Assigned(TTrndiExtEngine.Instance) then
  begin  // Safe
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

function funcInt(const func: string; params: array of const; const nofunc: int64): int64;
var
  ex: boolean;
  res: string;
  i: int64;
begin
  result := 0;
  res := callFunc(func,params,ex);

  if not ex then
    Exit(nofunc);

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
    Exit(nofunc);

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
  if not Assigned(TTrndiExtEngine.Instance) then
  begin exists := false; exit; end;
  exists := TTrndiExtEngine.Instance.FunctionExists(func);
  if not exists then
    exit;
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
  if not Assigned(TTrndiExtEngine.Instance) then
  begin exists := false; exit; end;
  exists := TTrndiExtEngine.Instance.FunctionExists(func);
  if not exists then
    exit;
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
  if not Assigned(TTrndiExtEngine.Instance) then
  begin exists := false; exit; end;
  exists := TTrndiExtEngine.Instance.FunctionExists(func);
  if not exists then
    exit;
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
  if not Assigned(TTrndiExtEngine.Instance) then
    exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSString(s);
end;

function JSValueRawInt(const v: int64): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then
    exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSInt64(v);
end;

function JSValueRawFloat(const v: double): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then
    exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSFloat(v);
end;

function JSValueRawBool(const v: boolean): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then
    exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSBool(v);
end;

function JSValueRawArray(const values: array of const): JSValueRaw;
begin
  if not Assigned(TTrndiExtEngine.Instance) then
    exit(JS_UNDEFINED);
  Result := TTrndiExtEngine.Instance.MakeJSArray(values);
end;

procedure ConvertVarRecsToJSValueRaw(const params: array of const; var jsValues: array of JSValueRaw);
var
  i: integer;
begin
  for i := 0 to High(params) do
    case params[i].VType of
    vtString:
      jsValues[i] := JSValueRawString(RawUtf8(shortstring(params[i].VString^)));
    vtAnsiString:
      jsValues[i] := JSValueRawString(RawUtf8(ansistring(params[i].VAnsiString)));
    vtUnicodeString:
      jsValues[i] := JSValueRawString(RawUtf8(unicodestring(params[i].VUnicodeString)));
    vtInteger:
      jsValues[i] := JSValueRawInt(params[i].VInteger);
    vtInt64:
      jsValues[i] := JSValueRawInt(params[i].VInt64^);
    vtExtended:
      jsValues[i] := JSValueRawFloat(params[i].VExtended^);
    vtBoolean:
      jsValues[i] := JSValueRawBool(params[i].VBoolean);
    else
      jsValues[i] := JSValueRawString('unsupported_type');
    end;
end;
{$endif}

end.
