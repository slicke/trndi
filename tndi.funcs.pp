unit tndi.funcs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


procedure CenterPanelToCaption(Panel: TPanel);
function GetAppPath: string;
function GetLangPath: string;
procedure PaintLbl(Sender: TLabel; OutlineWidth: integer = 1; OutlineColor: TColor = clBlack);
procedure LogMessage(const Msg: string);
procedure SortReadingsDescending(var Readings: array of BGReading);
procedure SetPointHeight(L: TLabel; Value: Single; clientHeight: integer);
implementation

procedure CenterPanelToCaption(Panel: TPanel);
var
  TextWidth, PanelWidth, Padding: Integer;
  ParentWidth: Integer;
begin
  // Calculate text width using the panel's font
  Panel.Canvas.Font := Panel.Font;
  TextWidth := Panel.Canvas.TextWidth(Panel.Caption);

  Padding := 20; // Add 20 pixels (10 on each side, adjust as needed)
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


end.

