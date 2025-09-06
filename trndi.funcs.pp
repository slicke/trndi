unit trndi.funcs;

{$mode ObjFPC}{$H+}
{$ifdef DARWIN}
  {$ModeSwitch objectivec1}
{$endif}

interface

uses
  Classes, SysUtils, ExtCtrls, stdctrls, graphics, trndi.types, forms, math,
  fpjson, jsonparser, dateutils
  {$ifdef DARWIN}, CocoaAll{$endif};


procedure CenterPanelToCaption(Panel: TPanel; margin: integer = 10);
function GetAppPath: string;
function GetLangPath: string;
procedure PaintLbl(Sender: TLabel; OutlineWidth: integer = 1; OutlineColor: TColor = clBlack);
procedure LogMessage(const Msg: string);
procedure SortReadingsDescending(var Readings: array of BGReading);
procedure SetPointHeight(L: TLabel; Value: Single; clientHeight: integer);
function HasNewerRelease(const JsonResponse: string;
                         out ReleaseName: string;
                         IncludePrerelease: Boolean = False): Boolean;
function ParseCompilerDate: TDateTime;
function GetNewerVersionURL(const JsonResponse: string;
                            IncludePrerelease: Boolean = False;
                            Platform: string = ''): string;

const
INTERVAL_MINUTES = 5; // Each time interval is 5 minutes
NUM_DOTS = 10;        // Total number of labels (lDot1 - lDot10)
DATA_FRESHNESS_THRESHOLD_MINUTES = 11; // Max minutes before data is considered outdated

BG_API_MIN = 2; // NS can't read lower
BG_API_MAX = 22.2; // NS can't read higher
BG_REFRESH = 300000; // 5 min refresh

DOT_GRAPH =  '•';
DOT_FRESH = '☉';

APP_BUILD_DATE = {$I %DATE%}; // Returns "2025/07/21"
APP_BUILD_TIME = {$I %TIME%}; // Returns "14:30:25"

var
DOT_ADJUST: single = 0; // Multiplyer where dots appear
MAX_MIN: integer = 1440; // Max time to request
MAX_RESULT: integer = 25; // Max results

implementation

procedure CenterPanelToCaption(Panel: TPanel; margin: integer = 10);
var
  TextWidth, PanelWidth, Padding: Integer;
  ParentWidth: Integer;
begin
  // Calculate text width using the panel's font
  Panel.Canvas.Font := Panel.Font;
  TextWidth := Panel.Canvas.TextWidth(Panel.Caption);

  Padding := margin*2; // Add 20 pixels (10 on each side, adjust as needed)
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
  result := ExtractFilePath(Application.ExeName);
end;
function getLangPath: string;
begin
  result := GetAppPath + 'lang/';
end;
{$endif}

procedure PaintLbl(Sender: TLabel; OutlineWidth: integer = 1; OutlineColor: TColor = clBlack);
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
procedure SetPointHeight(L: TLabel; Value: Single; clientHeight: integer);
const
  GraphMin = 2;
  GraphMax = 22;
var
  Padding, UsableHeight, Position: Integer;
begin
  // Define padding and usable height for scaling
  Padding := Round(ClientHeight * 0.1); // 10% of the client height
  UsableHeight := ClientHeight - (Padding * 2);

  // Clamp Value within range
  if Value < GraphMin then
    Value := GraphMin
  else if Value > GraphMax then
    Value := GraphMax;

  // Calculate position as a proportion of the usable height
  Position := Padding + Round((Value - GraphMin) / (GraphMax - GraphMin) * UsableHeight);

  // Apply the calculated position to the label's Top property
  L.Top := ClientHeight - Position;

  // Optional debug/logging to verify placement
  LogMessage(Format('Label %s: Value=%.2f, Top=%d', [L.Name, Value, L.Top]));
end;

procedure setTimeRange(mins, count: integer);
begin
  MAX_MIN := min(mins, 20); // Max time to request
  MAX_RESULT := min(count, 2); // Max results
end;

function ParseCompilerDate: TDateTime;
var
  DateParts: TStringArray;
  TimeParts: TStringArray;
  Year, Month, Day, Hour, Min, Sec: Word;
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
                         IncludePrerelease: Boolean = False): Boolean;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  ReleaseDateStr: string;
  ReleaseDate, BuildDate: TDateTime;
  Year, Month, Day, Hour, Min, Sec: Word;
  i: Integer;
  IsPrerelease: Boolean;
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
                                  IncludePrerelease: Boolean = False;
                                  Platform: string = ''): string;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  ReleaseDateStr, ReleaseTitle: string;
  ReleaseDate, BuildDate: TDateTime;
  Year, Month, Day, Hour, Min, Sec: Word;
  i: Integer;
  IsPrerelease, MatchesPlatform: Boolean;
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
          'mac', 'macos': MatchesPlatform := (Pos('mac', LowerCase(ReleaseTitle)) > 0) or
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
            'mac', 'macos': MatchesPlatform := (Pos('mac', LowerCase(ReleaseTitle)) > 0) or
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

end.

