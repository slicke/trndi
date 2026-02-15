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

{**
  uhistorygraph - A lightweight history visualization form for BG readings.

  This unit implements TfHistoryGraph, a minimal, dependency-free plotting
  form used to visualize a sequence of BG readings as dots linked by a
  polyline. It is intentionally simple and does not rely on external charting
  libraries to keep bundle size, compilation time and runtime footprint
  small.

  Key features:
  - Plot readings across a time axis and a value axis (units aware).
  - Click a dot to show a small details popup (same format as the existing
    history list popup in main UI).
  - Lightweight, single-form instance reused via ShowHistoryGraph().

  Developer notes:
  - The layout is intentionally simple: change GRAPH_MARGIN_* constants if
    the UI needs to be more compact or if panels overflow.
  - The color mapping is done in LevelColor() and should match other
    application UI where possible — keeping the same color scheme improves
    accessibility (and is easier for users to read).
  - The form uses `DoubleBuffered` to reduce flicker; any additional
    interaction (zooming/panning) should preserve the double buffering.

  The file uses PasDoc comments and conservative layout so it is friendly to
  extend or port between widgetsets.
}
unit uhistorygraph;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Math, Menus,
trndi.types, trndi.api, trndi.strings, slicke.ux.alert, dateutils, {$ifdef WINDOWS}trndi.native,{$endif}
ExtDlgs, IntfGraphics, FPImage, FPWritePNG;

type
  {** THistoryGraphPalette
      Shared palette values used by the history graph to map BG levels to
      consistent colors. Each field corresponds to the LevelColor cases and
      allows the main UI to provide its runtime palette while keeping this
      unit decoupled from `umain`. }
THistoryGraphPalette = record
  Range: TColor;
  RangeHigh: TColor;
  RangeLow: TColor;
  High: TColor;
  Low: TColor;
  Unknown: TColor;
end;

  {** TfHistoryGraph
      Primary lightweight form used to render BG readings as a simple dot
      graph with a time (X) axis and value (Y) axis. The class stores
      the plotting series in `FPoints` and exposes a single method:
      `SetReadings` which populates the internal point array and updates the
      plot. This class is intentionally minimal — features like zoom,
      panning, tooltips and export are omitted to keep the UI fast and
      maintainable.
  }
  { TfHistoryGraph }
TfHistoryGraph = class(TForm)
private
  type
        {** TGraphPoint: Internal structure that ties a BGReading with a
            converted numeric value (in the configured preferred unit).
            Storing a separate Value avoids repeated unit conversions while
            drawing and simplifies sorting/comparison operations. }
    TGraphPoint = record
      Reading: BGReading;
      Value: double;
    end;
private
  FPoints: array of TGraphPoint; // Array of converted readings (value in preferred unit)
  FUnit: BGUnit;
  FMinValue: double;
  FMaxValue: double;
  FMinTime: TDateTime;
  FMaxTime: TDateTime;
  FDotRadius: integer; // Dot radius in pixels
  FPalette: THistoryGraphPalette; // Runtime palette supplied by main UI
  FCgmHi: integer; // High threshold in mg/dL
  FCgmLo: integer; // Low threshold in mg/dL
  FCgmRangeHi: integer; // Range high threshold in mg/dL
  FCgmRangeLo: integer; // Range low threshold in mg/dL
  FPopupMenu: TPopupMenu; // Context menu for right-click actions
  FBasalProfile: TBasalProfile; // Optional basal profile to draw as overlay
  FShowBasal: boolean; // Whether to render basal overlay
  FMaxBasal: single; // Maximum basal rate for scaling (U/hr)
    {** GetPlotRect: Determine the plotting rectangle inside the form where
      dots and lines are drawn. Respects the margins defined above. }
  function GetPlotRect: TRect;
    {** SortPointsByTime: Ensure FPoints are sorted chronologically (ascending).
      This keeps both the polyline and chart axes consistent. }
  procedure SortPointsByTime;
    {** UpdateExtents: Recompute min/max values and times used to map
      value/time to device coordinates. Adds a small padding to avoid
      degenerate spans. }
  procedure UpdateExtents;
    {** DrawAxesAndGrid: Draws a simple XY-grid and labels for the value
      (left) and time (bottom) axes. Uses the plot extents from
      UpdateExtents. }
  procedure DrawAxesAndGrid(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawThresholdLines: Draws horizontal lines at each threshold level
      using the corresponding level colors. }
  procedure DrawThresholdLines(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawBasalOverlay: Renders daily repeating basal schedule as a small
      area strip at the bottom of the plot. Values are scaled to
      `FMaxBasal` and clipped to the plot range. }
    procedure DrawBasalOverlay(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawPolyline: Connects the chronological points with a thin
      line to indicate trend (optional visual aid). }
  procedure DrawPolyline(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawPoints: Draws a filled circle for each reading using
      colors determined by LevelColor. Clicking a dot triggers
      ShowReadingDetails. }
  procedure DrawPoints(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawLegend: Draws a small key and information panel to the right
      of the plot showing counts, time-range and color legend. }
  procedure DrawLegend(ACanvas: TCanvas; const PlotRect: TRect);
    {** TimeToX: Map a timestamp into an X coordinate inside PlotRect.
      @param(TimeValue Input timestamp) @returns X coordinate in pixels. }
  function TimeToX(const TimeValue: TDateTime; const PlotRect: TRect): integer;
    {** ValueToY: Map a glucose value into a Y coordinate inside PlotRect.
      @param(Value Numeric value in the configured unit) @returns Y coordinate in pixels. }
  function ValueToY(const Value: double; const PlotRect: TRect): integer;
    {** LevelColor: Convert a BGValLevel into a display TColor used for
      dot-fill and legend chips. Maintains consistent color usage
      between the main UI and the graph. }
  function LevelColor(const Level: BGValLevel): TColor;
    {** PointAt: Returns the index of a point if the (X,Y) is within a small
      distance of a drawn dot, otherwise -1. Used to detect clicks. }
  function PointAt(const X, Y: integer): integer;
  function HasData: boolean;
  procedure ShowReadingDetails(const Reading: BGReading);
protected
  procedure Paint; override;
  procedure Resize; override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    override;
  procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  procedure DoClose(var CloseAction: TCloseAction); override;
public
    {** Create: Construct a new TfHistoryGraph instance. The form
      uses Double-buffering to reduce flicker and has configurable
      initial size. }
  constructor Create(AOwner: TComponent); override;
    {** Destroy: Free the form and any references; resets the global
      `fHistoryGraph` variable if it points to the closed instance. }
  destructor Destroy; override;
    {** SetReadings: Populate the graph with an array of BGReadings.
      The method will drop empty readings, convert values to the
      requested unit and compute plot extents before repainting.
      @param(Readings Array of BGReading objects to draw)
      @param(UnitPref Preferred output unit for formatting/drawing) }
  procedure SetReadings(const Readings: BGResults; UnitPref: BGUnit);
    {** SetPalette: Allow callers to inject the palette used when
      drawing level colors so the graph matches the main UI. }
  procedure SetPalette(const Palette: THistoryGraphPalette);
    {** SetThresholds: Inject the CGM thresholds (in mg/dL) for display in the legend. }
  procedure SetThresholds(const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
    {** SaveAsPNG: Export the current graph to a PNG file. Shows a save dialog
      and renders the full graph to the selected file. }
  procedure SaveAsPNG(Sender: TObject);
    {** SaveAsCSV: Export the readings data to a CSV file for analysis in
      spreadsheet applications. }
  procedure SaveAsCSV(Sender: TObject);
    {** SetBasalProfile: Provide a repeating daily basal profile to be drawn
      on the graph. The profile repeats every 24h. @param(maxBasal) is the
      maximum expected basal used to scale the overlay height in pixels. }
  procedure SetBasalProfile(const profile: TBasalProfile; const maxBasal: single = 3.0);
    {** Enable or disable basal overlay rendering. } 
  procedure SetBasalOverlayEnabled(aEnabled: boolean);
end;

  {** Display a dot-based history plot for the supplied readings. Reuses the
    same form instance between invocations to avoid repeated allocations. }
procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit); overload;
procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette); overload;
procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette; const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer); overload;

var
  {** fHistoryGraph: A single, reusable instance of the history graph.
      The ShowHistoryGraph helper will create the form once and re-use it
      to preserve user position and keep memory allocations lower. }
fHistoryGraph: TfHistoryGraph = nil;

implementation

uses
LCLType, trndi.funcs;

resourcestring
RS_HISTORY_GRAPH_TITLE = 'History graph';
RS_HISTORY_GRAPH_EMPTY = 'No history data to plot';
RS_HISTORY_GRAPH_HELP = 'Click a dot to see the full reading details';
RS_HISTORY_GRAPH_POINT_COUNT = '%d readings';
RS_HISTORY_GRAPH_RANGE = '%s – %s';
RS_HISTORY_GRAPH_UNIT_FMT = 'Readings (%s)';
RS_HISTORY_GRAPH_AXIS_TIME = 'Time';
RS_HISTORY_GRAPH_KEY_TITLE = 'Legend';
RS_HISTORY_GRAPH_KEY_RANGE = 'In range';
RS_HISTORY_GRAPH_KEY_RANGE_HI = 'Range high';
RS_HISTORY_GRAPH_KEY_RANGE_LO = 'Range low';
RS_HISTORY_GRAPH_KEY_HIGH = 'High';
RS_HISTORY_GRAPH_KEY_LOW = 'Low';
RS_HISTORY_GRAPH_KEY_UNKNOWN = 'Unknown';
RS_HISTORY_GRAPH_KEY_BASAL = 'Basal';
RS_HISTORY_GRAPH_SAVE_TITLE = 'Save graph as PNG';
RS_HISTORY_GRAPH_SAVE_SUCCESS = 'Graph saved successfully';
RS_HISTORY_GRAPH_SAVE_ERROR = 'Failed to save graph: %s';
RS_HISTORY_GRAPH_MENU_SAVE = 'Save as Image...';
RS_HISTORY_GRAPH_MENU_SAVE_CSV = 'Save as CSV...';
RS_HISTORY_GRAPH_CSV_TITLE = 'Save readings as CSV';

{** Constants used for layout and division handling in this graph unit.
  Changing these values will affect overall margins and grid density. }
const
GRAPH_MARGIN_LEFT = 72;
GRAPH_MARGIN_TOP = 40;
GRAPH_MARGIN_RIGHT = 220;
GRAPH_MARGIN_BOTTOM = 120;
GRAPH_DIVISIONS = 5;

function DefaultHistoryGraphPalette: THistoryGraphPalette; inline;
begin
  Result.Range := RGBToColor(64, 145, 108);
  Result.RangeHigh := RGBToColor(64, 145, 108);
  Result.RangeLow := RGBToColor(33, 99, 174);
  Result.High := RGBToColor(217, 95, 2);
  Result.Low := RGBToColor(33, 99, 174);
  Result.Unknown := RGBToColor(180, 180, 180);
end;

{ TfHistoryGraph }

constructor TfHistoryGraph.Create(AOwner: TComponent);
var
  menuItem: TMenuItem;
begin
  inherited CreateNew(AOwner, 0);
  Caption := RS_HISTORY_GRAPH_TITLE;
  Width := 760;
  Height := 460;
  DoubleBuffered := true;
  Position := poMainFormCenter;
  BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  Color := clWhite;
  FDotRadius := 5;
  FPalette := DefaultHistoryGraphPalette;
  
  // Create context menu
  FPopupMenu := TPopupMenu.Create(Self);
  menuItem := TMenuItem.Create(FPopupMenu);
  menuItem.Caption := RS_HISTORY_GRAPH_MENU_SAVE;
  menuItem.OnClick := @SaveAsPNG;
  FPopupMenu.Items.Add(menuItem);
  menuItem := TMenuItem.Create(FPopupMenu);
  menuItem.Caption := RS_HISTORY_GRAPH_MENU_SAVE_CSV;
  menuItem.OnClick := @SaveAsCSV;
  FPopupMenu.Items.Add(menuItem);
  PopupMenu := FPopupMenu;
end;

procedure TfHistoryGraph.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  inherited DoClose(CloseAction);
end;

destructor TfHistoryGraph.Destroy;
begin
  if fHistoryGraph = Self then
    fHistoryGraph := nil;
  inherited Destroy;
end;

procedure TfHistoryGraph.DrawAxesAndGrid(ACanvas: TCanvas; const PlotRect: TRect);
var
  i: integer;
  value: double;
  timeVal: TDateTime;
  y, x: integer;
  labelText: string;
  lineColor: TColor;
  apiMinVal, apiMaxVal: double;
  apiMinY, apiMaxY: integer;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clSilver;
  ACanvas.Font.Color := clBlack;
  ACanvas.Rectangle(PlotRect);

  lineColor := $00E0E0E0;

  for i := 0 to GRAPH_DIVISIONS do
  begin
    value := FMinValue + (FMaxValue - FMinValue) * (i / GRAPH_DIVISIONS);
    y := ValueToY(value, PlotRect);
    ACanvas.Pen.Color := lineColor;
    ACanvas.MoveTo(PlotRect.Left, y);
    ACanvas.LineTo(PlotRect.Right, y);

    // Draw value tick labels to the left of the plot. Use BG_MSG_SHORT to
    // follow the same unit display rules used across the app (1-decimal for
    // mmol/L, no decimals for mg/dL).
    labelText := Format(BG_MSG_SHORT[FUnit], [value]);
    ACanvas.Pen.Color := clGray;
    ACanvas.TextOut(PlotRect.Left - 48,
      y - ACanvas.TextHeight(labelText) div 2, labelText);
  end;

  {** Draw API min/max horizontal markers (the API's acceptable value range).
      These are drawn after the grid so the user immediately sees the
      full low/high range even if there is limited data. }
  begin
    // BG_API_MIN/MAX are in mmol/L; convert to the display unit (FUnit)
    // by using BG_CONVERTIONS[mmol][FUnit].
    apiMinVal := BG_API_MIN * BG_CONVERTIONS[FUnit][mmol];
    apiMaxVal := BG_API_MAX * BG_CONVERTIONS[FUnit][mmol];
    apiMinY := ValueToY(apiMinVal, PlotRect);
    apiMaxY := ValueToY(apiMaxVal, PlotRect);
    // Draw thin dashed lines for min/max
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Color := clGray;
    ACanvas.MoveTo(PlotRect.Left, apiMinY);
    ACanvas.LineTo(PlotRect.Right, apiMinY);
    ACanvas.MoveTo(PlotRect.Left, apiMaxY);
    ACanvas.LineTo(PlotRect.Right, apiMaxY);
    ACanvas.Pen.Style := psSolid;
    // No explicit numeric labels for API min/max; the dashed lines suffice.
  end;

  for i := 0 to GRAPH_DIVISIONS do
  begin
    timeVal := FMinTime + (FMaxTime - FMinTime) * (i / GRAPH_DIVISIONS);
    x := TimeToX(timeVal, PlotRect);
    ACanvas.Pen.Color := lineColor;
    ACanvas.MoveTo(x, PlotRect.Top);
    ACanvas.LineTo(x, PlotRect.Bottom);

    // X-axis tick labels show only hours and minutes; keep them compact
    // so they never overlap grid lines or form borders.
    if Dateof(timeVal) = DateOf(Now) then
      labelText := FormatDateTime('hh:nn', timeVal)
    else
      labelText := FormatDateTime('ddd hh:nn', timeVal);
    ACanvas.Pen.Color := clGray;
    ACanvas.TextOut(x - ACanvas.TextWidth(labelText) div 2,
      PlotRect.Bottom + 8, labelText);
  end;

  ACanvas.Font.Style := [fsBold];
  labelText := RS_HISTORY_GRAPH_AXIS_TIME;
  ACanvas.TextOut(
    (PlotRect.Left + PlotRect.Right - ACanvas.TextWidth(labelText)) div 2,
    PlotRect.Bottom + 60, labelText);

  labelText := Format(RS_HISTORY_GRAPH_UNIT_FMT, [BG_UNIT_NAMES[FUnit]]);
  ACanvas.Font.Orientation := 900;
  ACanvas.TextOut(PlotRect.Left - GRAPH_MARGIN_LEFT + 8,
    (PlotRect.Top + PlotRect.Bottom + ACanvas.TextWidth(labelText)) div 2,
    labelText);
  ACanvas.Font.Orientation := 0;
  ACanvas.Font.Style := [];
end;

procedure TfHistoryGraph.DrawThresholdLines(ACanvas: TCanvas; const PlotRect: TRect);
var
  hiVal, loVal, rangeHiVal, rangeLoVal: double;
  hiY, loY, rangeHiY, rangeLoY: integer;
  lineColor: TColor;
  alpha: byte;
begin
  // Convert thresholds from mg/dL to display unit
  hiVal := FCgmHi * BG_CONVERTIONS[FUnit][mgdl];
  loVal := FCgmLo * BG_CONVERTIONS[FUnit][mgdl];
  rangeHiVal := FCgmRangeHi * BG_CONVERTIONS[FUnit][mgdl];
  rangeLoVal := FCgmRangeLo * BG_CONVERTIONS[FUnit][mgdl];

  // Calculate Y positions
  hiY := ValueToY(hiVal, PlotRect);
  loY := ValueToY(loVal, PlotRect);
  rangeHiY := ValueToY(rangeHiVal, PlotRect);
  rangeLoY := ValueToY(rangeLoVal, PlotRect);

  ACanvas.Pen.Width := 2;
  ACanvas.Pen.Style := psDash;

  // Draw High threshold line
  lineColor := LevelColor(BGHigh);
  ACanvas.Pen.Color := lineColor;
  ACanvas.MoveTo(PlotRect.Left, hiY);
  ACanvas.LineTo(PlotRect.Right, hiY);

  // Draw Low threshold line
  lineColor := LevelColor(BGLOW);
  ACanvas.Pen.Color := lineColor;
  ACanvas.MoveTo(PlotRect.Left, loY);
  ACanvas.LineTo(PlotRect.Right, loY);

  // Draw Range High threshold line (if not disabled)
  if FCgmRangeHi <> 500 then
  begin
    lineColor := LevelColor(BGRangeHI);
    ACanvas.Pen.Color := lineColor;
    ACanvas.MoveTo(PlotRect.Left, rangeHiY);
    ACanvas.LineTo(PlotRect.Right, rangeHiY);
  end;

  // Draw Range Low threshold line (if not disabled)
  if FCgmRangeLo <> 0 then
  begin
    lineColor := LevelColor(BGRangeLO);
    ACanvas.Pen.Color := lineColor;
    ACanvas.MoveTo(PlotRect.Left, rangeLoY);
    ACanvas.LineTo(PlotRect.Right, rangeLoY);
  end;

  // Reset pen style
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
end;

procedure TfHistoryGraph.DrawBasalOverlay(ACanvas: TCanvas; const PlotRect: TRect);
var
  dayStart, dayEnd: TDateTime;
  d, j: integer;
  startMin, endMin: integer;
  startDT, endDT, s, e: TDateTime;
  x1, x2: integer;
  basalHeight, h: integer;
  rateFrac: double;
  basalColor: TColor;
  nextIdx: integer;
begin
  if (not FShowBasal) or (Length(FBasalProfile) = 0) then
    Exit;

  // Determine basal strip height (small fraction of plot height)
  basalHeight := Min(Max(24, (PlotRect.Bottom - PlotRect.Top) div 8), 80);
  basalColor := RGBToColor(120, 170, 255);

  // Iterate each day in the plot range; basal profile repeats daily
  for d := Trunc(FMinTime) to Trunc(FMaxTime) do
  begin
    for j := 0 to High(FBasalProfile) do
    begin
      startMin := FBasalProfile[j].startMin;
      // Determine end minute (next entry or end of day)
      if j < High(FBasalProfile) then
        endMin := FBasalProfile[j + 1].startMin
      else
        endMin := 24 * 60;

      startDT := d + (startMin / 1440);
      endDT := d + (endMin / 1440);

      // Clip to visible plot range
      if (endDT <= FMinTime) or (startDT >= FMaxTime) then
        Continue;

      s := Max(startDT, FMinTime);
      e := Min(endDT, FMaxTime);
      x1 := TimeToX(s, PlotRect);
      x2 := TimeToX(e, PlotRect);

      // Height scaled by FMaxBasal
      rateFrac := FBasalProfile[j].value / Max(0.001, FMaxBasal);
      rateFrac := EnsureRange(rateFrac, 0, 1);
      h := Round(rateFrac * basalHeight);
      // Draw from PlotRect.Bottom - h up to bottom
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Pen.Style := psClear;
      ACanvas.Brush.Color := basalColor;
      ACanvas.Rectangle(x1, PlotRect.Bottom - h, x2, PlotRect.Bottom);
    end;
  end;

  // Restore styles
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
end;

procedure TfHistoryGraph.DrawLegend(ACanvas: TCanvas; const PlotRect: TRect);
const
  INFO_PADDING = 6;
  KEY_BOX = 14;
var
  info, rangeFirst, rangeSecond: string;
  firstStamp, lastStamp: string;
  infoRect, helpRect, keyRect: TRect;
  textY, lineHeight: integer;
  keyX, keyY: integer;
function LegendBackground: TColor; inline;
  begin
    Result := RGBToColor(246, 246, 246);
  end;

  {** DrawInfoPanel: Internal helper that renders a rounded information box
      containing the point count and the time range. It is positioned in the
      right margin and visible for most window sizes. }
procedure DrawInfoPanel;
  var
    hInfo1, hInfo2, hInfo3: integer;
  begin
    // Dynamically calculate the height for the info rect to ensure its
    // contents never overflow regardless of font size.
    hInfo1 := ACanvas.TextHeight(info);
    hInfo2 := ACanvas.TextHeight(rangeFirst);
    hInfo3 := ACanvas.TextHeight(rangeSecond);
    infoRect := Rect(PlotRect.Right + 12,
      keyRect.Bottom + 12,
      ClientWidth - 12,
      keyRect.Bottom + 12 + (hInfo1 + hInfo2 + hInfo3) + (INFO_PADDING * 4));
    if infoRect.Right - infoRect.Left < 160 then
      infoRect.Right := infoRect.Left + 160;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := LegendBackground;
    ACanvas.Pen.Color := $00C8C8C8;
    ACanvas.RoundRect(infoRect, 6, 6);
    ACanvas.Brush.Style := bsClear;
    textY := infoRect.Top + INFO_PADDING;
    ACanvas.Font.Style := [fsBold];
    ACanvas.TextOut(infoRect.Left + INFO_PADDING, textY, info);
    Inc(textY, lineHeight + 2);
    ACanvas.Font.Style := [];
    ACanvas.TextOut(infoRect.Left + INFO_PADDING, textY, rangeFirst);
    Inc(textY, lineHeight + 2);
    ACanvas.TextOut(infoRect.Left + INFO_PADDING, textY, rangeSecond);
  end;

  {** DrawHelpPanel: Internal helper to render a single-line help banner
      beneath the chart with instructions for interacting with the graph. }
procedure DrawHelpPanel;
  var
    panelTop: integer;
  begin
    panelTop := PlotRect.Bottom + 92;
    helpRect := Rect(PlotRect.Left - 12,
      panelTop,
      PlotRect.Right + 12,
      panelTop + lineHeight + INFO_PADDING * 2);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := LegendBackground;
    ACanvas.Pen.Color := $00C8C8C8;
    ACanvas.RoundRect(helpRect, 6, 6);
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(helpRect.Left + INFO_PADDING,
      helpRect.Top + INFO_PADDING, RS_HISTORY_GRAPH_HELP);
  end;

  {** DrawKeyEntry: Render a single key entry (small colored box + label)
      used by DrawKeyPanel to fill the legend with items for each level. }
procedure DrawKeyEntry(const Caption: string; const Color: TColor);
  var
    textOffset: integer;
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := Color;
    ACanvas.Pen.Color := clGray;
    ACanvas.Rectangle(keyX, keyY, keyX + KEY_BOX, keyY + KEY_BOX);
    ACanvas.Brush.Style := bsClear;
    textOffset := keyY + (KEY_BOX - lineHeight) div 2;
    if textOffset < keyY then
      textOffset := keyY;
    ACanvas.TextOut(keyX + KEY_BOX + 8, textOffset, Caption);
    Inc(keyY, KEY_BOX + 6);
  end;

  {** DrawKeyPanel: Build the legend panel showing color chips and text.
      The panel is rendered to the right of the plot area to avoid covering
      the most recent readings. }
procedure DrawKeyPanel;
  var
    unitStr, rangeStr, rangeHiStr, rangeLoStr, hiStr, loStr: string;
    rangeHiVal, rangeLoVal, hiVal, loVal: double;
  begin
    // Determine unit string
    if FUnit = mmol then
      unitStr := 'mmol/L'
    else
      unitStr := 'mg/dL';
    
    // Convert thresholds to display unit
    hiVal := FCgmHi * BG_CONVERTIONS[FUnit][mgdl];
    loVal := FCgmLo * BG_CONVERTIONS[FUnit][mgdl];
    rangeHiVal := FCgmRangeHi * BG_CONVERTIONS[FUnit][mgdl];
    rangeLoVal := FCgmRangeLo * BG_CONVERTIONS[FUnit][mgdl];
    
    // Format strings with threshold values
    if FUnit = mmol then
    begin
      hiStr := Format('%s (%.1f %s+)', [RS_HISTORY_GRAPH_KEY_HIGH, hiVal, unitStr]);
      loStr := Format('%s (%.1f %s−)', [RS_HISTORY_GRAPH_KEY_LOW, loVal, unitStr]);
      rangeHiStr := Format('%s (%.1f %s+)', [RS_HISTORY_GRAPH_KEY_RANGE_HI, rangeHiVal, unitStr]);
      rangeLoStr := Format('%s (%.1f %s−)', [RS_HISTORY_GRAPH_KEY_RANGE_LO, rangeLoVal, unitStr]);
      rangeStr := Format('%s (%.1f - %.1f %s)', [RS_HISTORY_GRAPH_KEY_RANGE, rangeLoVal, rangeHiVal, unitStr]);
    end
    else
    begin
      hiStr := Format('%s (%d %s+)', [RS_HISTORY_GRAPH_KEY_HIGH, Round(hiVal), unitStr]);
      loStr := Format('%s (%d %s−)', [RS_HISTORY_GRAPH_KEY_LOW, Round(loVal), unitStr]);
      rangeHiStr := Format('%s (%d %s+)', [RS_HISTORY_GRAPH_KEY_RANGE_HI, Round(rangeHiVal), unitStr]);
      rangeLoStr := Format('%s (%d %s−)', [RS_HISTORY_GRAPH_KEY_RANGE_LO, Round(rangeLoVal), unitStr]);
      rangeStr := Format('%s (%d - %d %s)', [RS_HISTORY_GRAPH_KEY_RANGE, Round(rangeLoVal), Round(rangeHiVal), unitStr]);
    end;
    
    keyRect := Rect(PlotRect.Right + 12, PlotRect.Top,
      ClientWidth - 12,
      PlotRect.Top + (KEY_BOX + 6) * 6 + INFO_PADDING * 3 + lineHeight);
    if keyRect.Right - keyRect.Left < 160 then
      keyRect.Right := keyRect.Left + 160;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := LegendBackground;
    ACanvas.Pen.Color := $00C8C8C8;
    ACanvas.RoundRect(keyRect, 6, 6);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Style := [fsBold];
    ACanvas.TextOut(keyRect.Left + INFO_PADDING,
      keyRect.Top + INFO_PADDING, RS_HISTORY_GRAPH_KEY_TITLE);
    ACanvas.Font.Style := [];
    keyX := keyRect.Left + INFO_PADDING;
    keyY := keyRect.Top + INFO_PADDING + lineHeight + 4;
    DrawKeyEntry(rangeStr, LevelColor(BGRange));
    DrawKeyEntry(rangeHiStr, LevelColor(BGRangeHI));
    DrawKeyEntry(rangeLoStr, LevelColor(BGRangeLO));
    DrawKeyEntry(hiStr, LevelColor(BGHigh));
    DrawKeyEntry(loStr, LevelColor(BGLOW));
    DrawKeyEntry(RS_HISTORY_GRAPH_KEY_BASAL, RGBToColor(120,170,255));
    DrawKeyEntry(RS_HISTORY_GRAPH_KEY_UNKNOWN, FPalette.Unknown);
  end;

begin
  ACanvas.Font.Color := clBlack;
  info := Format(RS_HISTORY_GRAPH_POINT_COUNT, [Length(FPoints)]);
  firstStamp := FormatDateTime('ddd dd mmm hh:nn', FPoints[0].Reading.date);
  lastStamp := FormatDateTime('ddd dd mmm hh:nn',
    FPoints[High(FPoints)].Reading.date);
  rangeFirst := firstStamp;
  rangeSecond := '→ ' + lastStamp;
  lineHeight := ACanvas.TextHeight('Hg');

  DrawKeyPanel;
  DrawInfoPanel;
  DrawHelpPanel;
end;

procedure TfHistoryGraph.DrawPoints(ACanvas: TCanvas; const PlotRect: TRect);
var
  i: integer;
  x, y: integer;
  radius: integer;
begin
  radius := FDotRadius;
  ACanvas.Pen.Color := clBlack;
  for i := 0 to High(FPoints) do
  begin
    x := TimeToX(FPoints[i].Reading.date, PlotRect);
    y := ValueToY(FPoints[i].Value, PlotRect);
    ACanvas.Brush.Color := LevelColor(FPoints[i].Reading.level);
    ACanvas.Ellipse(x - radius, y - radius, x + radius, y + radius);
  end;
end;

procedure TfHistoryGraph.DrawPolyline(ACanvas: TCanvas; const PlotRect: TRect);
var
  i: integer;
  gapMinutes: integer;
begin
  if Length(FPoints) < 2 then
    Exit;

  ACanvas.Pen.Color := clSilver;
  ACanvas.Brush.Style := bsClear;
  ACanvas.MoveTo(TimeToX(FPoints[0].Reading.date, PlotRect),
    ValueToY(FPoints[0].Value, PlotRect));
  for i := 1 to High(FPoints) do
  begin
    gapMinutes := MinutesBetween(FPoints[i].Reading.date, FPoints[i - 1].Reading.date);

    // If the time gap is large, don't connect points with a line.
    // This makes missing samples appear as a visual break.
    if gapMinutes >= (INTERVAL_MINUTES * 2) then
      ACanvas.MoveTo(TimeToX(FPoints[i].Reading.date, PlotRect),
        ValueToY(FPoints[i].Value, PlotRect))
    else
      ACanvas.LineTo(TimeToX(FPoints[i].Reading.date, PlotRect),
        ValueToY(FPoints[i].Value, PlotRect));
  end;
end;

function TfHistoryGraph.GetPlotRect: TRect;
var
  rightEdge, bottomEdge: integer;
begin
  rightEdge := ClientWidth - GRAPH_MARGIN_RIGHT;
  bottomEdge := ClientHeight - GRAPH_MARGIN_BOTTOM;
  if rightEdge <= GRAPH_MARGIN_LEFT + 10 then
    rightEdge := GRAPH_MARGIN_LEFT + 10;
  if bottomEdge <= GRAPH_MARGIN_TOP + 10 then
    bottomEdge := GRAPH_MARGIN_TOP + 10;
  Result := Rect(GRAPH_MARGIN_LEFT, GRAPH_MARGIN_TOP, rightEdge, bottomEdge);
end;

function TfHistoryGraph.HasData: boolean;
begin
  Result := Length(FPoints) > 0;
end;

procedure TfHistoryGraph.MouseDown(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
var
  idx: integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  
  if not HasData then
    Exit;

  // Handle left-click on dots to show details
  if Button = mbLeft then
  begin
    idx := PointAt(X, Y);
    if idx > -1 then
      ShowReadingDetails(FPoints[idx].Reading);
  end;
  
  // Right-click shows context menu (handled automatically by PopupMenu property)
end;

procedure TfHistoryGraph.Paint;
var
  plotRect: TRect;
  messageText: string;
begin
  // Main paint handler - clears the background and draws the grid, polyline,
  // points and legend. Avoids unnecessary drawing if there is no data.
  inherited Paint;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  if not HasData then
  begin
    messageText := RS_HISTORY_GRAPH_EMPTY;
    Canvas.Font.Size := 12;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut((ClientWidth - Canvas.TextWidth(messageText)) div 2,
      (ClientHeight - Canvas.TextHeight(messageText)) div 2, messageText);
    Exit;
  end;

  plotRect := GetPlotRect;
  DrawAxesAndGrid(Canvas, plotRect);
  DrawThresholdLines(Canvas, plotRect);
  DrawBasalOverlay(Canvas, plotRect);
  DrawPolyline(Canvas, plotRect);
  DrawPoints(Canvas, plotRect);
  DrawLegend(Canvas, plotRect);
end;

function TfHistoryGraph.PointAt(const X, Y: integer): integer;
var
  i: integer;
  plotRect: TRect;
  dotX, dotY: integer;
  thresholdSq, distSq: integer;
begin
  Result := -1;
  if not HasData then
    Exit;

  plotRect := GetPlotRect;
  thresholdSq := sqr(FDotRadius + 4);
  for i := 0 to High(FPoints) do
  begin
    dotX := TimeToX(FPoints[i].Reading.date, plotRect);
    dotY := ValueToY(FPoints[i].Value, plotRect);
    distSq := sqr(dotX - X) + sqr(dotY - Y);
    if distSq <= thresholdSq then
      Exit(i);
  end;
end;

procedure TfHistoryGraph.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TfHistoryGraph.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_S) and (Shift = [ssCtrl]) then
  begin
    SaveAsPNG(nil);
    Key := 0;
  end;
end;

procedure TfHistoryGraph.SetReadings(const Readings: BGResults;
UnitPref: BGUnit);
var
  i, idx: integer;
begin
  FUnit := UnitPref;
  SetLength(FPoints, 0);

  if Length(Readings) = 0 then
  begin
    Caption := Format('%s (0)', [RS_HISTORY_GRAPH_TITLE]);
    Invalidate;
    Exit;
  end;

  SetLength(FPoints, Length(Readings));
  idx := 0;
  for i := Low(Readings) to High(Readings) do
  begin
    if Readings[i].empty then
      Continue;
    FPoints[idx].Reading := Readings[i];
    FPoints[idx].Value := Readings[i].convert(UnitPref);
    Inc(idx);
  end;
  SetLength(FPoints, idx);

  if idx = 0 then
  begin
    Caption := Format('%s (0)', [RS_HISTORY_GRAPH_TITLE]);
    Invalidate;
    Exit;
  end;

  SortPointsByTime;
  UpdateExtents;
  Caption := Format('%s (%d)', [RS_HISTORY_GRAPH_TITLE, Length(FPoints)]);
  Invalidate;
end;

procedure TfHistoryGraph.SetPalette(const Palette: THistoryGraphPalette);
begin
  FPalette := Palette;
end;

procedure TfHistoryGraph.SetThresholds(const cgmHi, cgmLo, cgmRangeHi,
  cgmRangeLo: integer);
begin
  FCgmHi := cgmHi;
  FCgmLo := cgmLo;
  FCgmRangeHi := cgmRangeHi;
  FCgmRangeLo := cgmRangeLo;
end;

procedure TfHistoryGraph.SetBasalProfile(const profile: TBasalProfile; const maxBasal: single = 3.0);
begin
  FBasalProfile := Copy(profile);
  FMaxBasal := maxBasal;
  Invalidate;
end;

procedure TfHistoryGraph.SetBasalOverlayEnabled(aEnabled: boolean);
begin
  FShowBasal := aEnabled;
  Invalidate;
end;

procedure TfHistoryGraph.SaveAsPNG(Sender: TObject);
var
  saveDialog: TSavePictureDialog;
  bmp: TBitmap;
  intfImg: TLazIntfImage;
  writer: TFPWriterPNG;
  plotRect: TRect;
begin
  // Can be called from keyboard shortcut or context menu
  if not HasData then
    Exit;

  saveDialog := TSavePictureDialog.Create(nil);
  try
    saveDialog.Title := RS_HISTORY_GRAPH_SAVE_TITLE;
    saveDialog.Filter := 'PNG Images|*.png';
    saveDialog.DefaultExt := 'png';
    saveDialog.FileName := Format('trndi-history-%s.png',
      [FormatDateTime('yyyy-mm-dd-hhnnss', Now)]);

    if not saveDialog.Execute then
      Exit;

    bmp := TBitmap.Create;
    try
      bmp.SetSize(ClientWidth, ClientHeight);
      bmp.Canvas.Brush.Color := Color;
      bmp.Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));

      plotRect := GetPlotRect;
      DrawAxesAndGrid(bmp.Canvas, plotRect);
      DrawThresholdLines(bmp.Canvas, plotRect);
      DrawBasalOverlay(bmp.Canvas, plotRect);
      DrawPolyline(bmp.Canvas, plotRect);
      DrawPoints(bmp.Canvas, plotRect);
      DrawLegend(bmp.Canvas, plotRect);

      intfImg := TLazIntfImage.Create(0, 0);
      try
        intfImg.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);
        writer := TFPWriterPNG.Create;
        try
          writer.Indexed := false;
          writer.WordSized := false;
          writer.UseAlpha := false;
          intfImg.SaveToFile(saveDialog.FileName, writer);
        finally
          writer.Free;
        end;
      finally
        intfImg.Free;
      end;
    finally
      bmp.Free;
    end;
  except
    on E: Exception do
      ExtHTML(uxdAuto, 'Error', Format(RS_HISTORY_GRAPH_SAVE_ERROR, [E.Message]),
        [mbOK], uxmtError, 12.5);
  end;
  saveDialog.Free;
end;

procedure TfHistoryGraph.SaveAsCSV(Sender: TObject);
var
  saveDialog: TSaveDialog;
  csvFile: TextFile;
  i: integer;
  line: string;
  dateStr, timeStr, valueStr, deltaStr, trendStr, levelStr: string;
  rssi, noise: integer;
  rssiStr, noiseStr: string;
begin
  if not HasData then
    Exit;

  saveDialog := TSaveDialog.Create(nil);
  try
    saveDialog.Title := RS_HISTORY_GRAPH_CSV_TITLE;
    saveDialog.Filter := 'CSV Files|*.csv|All Files|*.*';
    saveDialog.DefaultExt := 'csv';
    saveDialog.FileName := Format('trndi-history-%s.csv',
      [FormatDateTime('yyyy-mm-dd-hhnnss', Now)]);

    if not saveDialog.Execute then
      Exit;

    AssignFile(csvFile, saveDialog.FileName);
    try
      Rewrite(csvFile);
      
      // Write CSV header
      WriteLn(csvFile, 'Date,Time,Value,Unit,Delta,Trend,Level,RSSI,Noise,Source,Sensor');
      
      // Write data rows
      for i := 0 to High(FPoints) do
      begin
        dateStr := FormatDateTime('yyyy-mm-dd', FPoints[i].Reading.date);
        timeStr := FormatDateTime('hh:nn:ss', FPoints[i].Reading.date);
        valueStr := Format(BG_MSG_SHORT[FUnit], [FPoints[i].Value]);
        deltaStr := FPoints[i].Reading.format(FUnit, BG_MSG_SIG_SHORT, BGDelta);
        trendStr := BG_TRENDS[FPoints[i].Reading.trend];
        
        case FPoints[i].Reading.level of
          BGRange: levelStr := 'In Range';
          BGRangeHI: levelStr := 'Range High';
          BGRangeLO: levelStr := 'Range Low';
          BGHigh: levelStr := 'High';
          BGLOW: levelStr := 'Low';
        else
          levelStr := 'Unknown';
        end;
        
        if FPoints[i].Reading.TryGetRSSI(rssi) then
          rssiStr := IntToStr(rssi)
        else
          rssiStr := '';
          
        if FPoints[i].Reading.TryGetNoise(noise) then
          noiseStr := IntToStr(noise)
        else
          noiseStr := '';
        
        line := Format('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s',
          [dateStr, timeStr, valueStr, BG_UNIT_NAMES[FUnit], deltaStr, 
           trendStr, levelStr, rssiStr, noiseStr, 
           FPoints[i].Reading.Source, FPoints[i].Reading.sensor]);
        WriteLn(csvFile, line);
      end;
      
      CloseFile(csvFile);
    except
      on E: Exception do
      begin
        CloseFile(csvFile);
        ExtHTML(uxdAuto, 'Error', Format(RS_HISTORY_GRAPH_SAVE_ERROR, [E.Message]),
          [mbOK], uxmtError, 12.5);
      end;
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TfHistoryGraph.ShowReadingDetails(const Reading: BGReading);
var
  xval: integer;
  rssi, noise: string;
begin
  // Build the RSSI and noise fields for the popup; these use the same
  // getters and formatting as the main UI for consistency.
  if Reading.TryGetRSSI(xval) then
    rssi := xval.ToString
  else
    rssi := RS_RH_UNKNOWN;

  if Reading.TryGetNoise(xval) then
    noise := xval.ToString
  else
    noise := RS_RH_UNKNOWN;

  ExtHTML(uxdAuto, RS_RH_READING, '<font size="4"><u></u>'+TimeToStr(Reading.date) +
    '</font><font size="3">' + sHTMLLineBreak+
    StringReplace(Format(RS_HISTORY_ITEM,
    [Reading.format(FUnit, BG_MSG_SHORT, BGPrimary),
    Reading.format(FUnit, BG_MSG_SIG_SHORT, BGDelta),
    Reading.trend.Img, rssi, noise, Reading.Source, Reading.sensor]), sLineBreak, sHTMLLineBreak, [rfReplaceAll]), [mbOK],uxmtInformation,12.5);
end;

procedure TfHistoryGraph.SortPointsByTime;
var
  i, j: integer;
  tmp: TGraphPoint;
begin
  // Use a simple insertion sort because the number of readings is small
  // (a few dozen at most) and the algorithm is simple and stable.
  for i := 1 to High(FPoints) do
  begin
    tmp := FPoints[i];
    j := i - 1;
    while (j >= 0) and (FPoints[j].Reading.date > tmp.Reading.date) do
    begin
      FPoints[j + 1] := FPoints[j];
      Dec(j);
    end;
    FPoints[j + 1] := tmp;
  end;
end;

function TfHistoryGraph.LevelColor(const Level: BGValLevel): TColor;
begin
  // Return a color which represents the given BG level using the current
  // palette supplied by the main UI. This keeps the graph in sync with the
  // user's configured theme.
  case Level of
  BGRange:
    Result := FPalette.Range;
  BGRangeHI:
    Result := FPalette.RangeHigh;
  BGRangeLO:
    Result := FPalette.RangeLow;
  BGHigh:
    Result := FPalette.High;
  BGLOW:
    Result := FPalette.Low;
  else
    Result := FPalette.Unknown;
  end;
end;

function TfHistoryGraph.TimeToX(const TimeValue: TDateTime;
const PlotRect: TRect): integer;
var
  span, ratio: double;
begin
  span := FMaxTime - FMinTime;
  if IsZero(span) then
    Exit((PlotRect.Left + PlotRect.Right) div 2);

  ratio := (TimeValue - FMinTime) / span;
  ratio := EnsureRange(ratio, 0, 1);
  Result := PlotRect.Left + Round(ratio * (PlotRect.Right - PlotRect.Left));
end;

procedure TfHistoryGraph.UpdateExtents;
var
  i: integer;
  padding: double;
  apiMin, apiMax: double;
begin
  if not HasData then
    Exit;

  FMinTime := FPoints[0].Reading.date;
  FMaxTime := FPoints[0].Reading.date;
  FMinValue := FPoints[0].Value;
  FMaxValue := FPoints[0].Value;

  for i := 1 to High(FPoints) do
  begin
    if FPoints[i].Reading.date < FMinTime then
      FMinTime := FPoints[i].Reading.date;
    if FPoints[i].Reading.date > FMaxTime then
      FMaxTime := FPoints[i].Reading.date;
    if FPoints[i].Value < FMinValue then
      FMinValue := FPoints[i].Value;
    if FPoints[i].Value > FMaxValue then
      FMaxValue := FPoints[i].Value;
  end;

  if IsZero(FMaxTime - FMinTime) then
    FMaxTime := FMaxTime + EncodeTime(0, 5, 0, 0);

  // Ensure the graph spans at least the API-defined limits. The API min/max
  // (BG_API_MIN and BG_API_MAX) are defined in mmol/L in `trndi.funcs`.
  // Convert to the configured display unit using BG_CONVERTIONS before
  // deciding extents so the full low-high range is always visible.
  apiMin := BG_API_MIN * BG_CONVERTIONS[FUnit][mmol];
  apiMax := BG_API_MAX * BG_CONVERTIONS[FUnit][mmol];

  if FMinValue > apiMin then
    FMinValue := apiMin;
  if FMaxValue < apiMax then
    FMaxValue := apiMax;

  if SameValue(FMaxValue, FMinValue) then
  begin
    padding := Math.Max(0.5, Abs(FMaxValue) * 0.05);
    FMinValue := FMinValue - padding;
    FMaxValue := FMaxValue + padding;
  end;
end;

function TfHistoryGraph.ValueToY(const Value: double;
const PlotRect: TRect): integer;
var
  span, ratio: double;
begin
  span := FMaxValue - FMinValue;
  if IsZero(span) then
    Exit((PlotRect.Top + PlotRect.Bottom) div 2);

  ratio := (Value - FMinValue) / span;
  ratio := EnsureRange(ratio, 0, 1);
  Result := PlotRect.Bottom - Round(ratio * (PlotRect.Bottom - PlotRect.Top));
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette);
begin
  ShowHistoryGraph(Readings, UnitPref, Palette, 180, 60, 160, 80);
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette; const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
begin
  if not Assigned(fHistoryGraph) then begin
    fHistoryGraph := TfHistoryGraph.Create(Application);
    {$ifdef windows}
    if TrndiNative.isDarkMode then
      TrndiNative.setDarkMode(fHistoryGraph.Handle);
    {$endif}
  end;

  fHistoryGraph.SetPalette(Palette);
  fHistoryGraph.SetThresholds(cgmHi, cgmLo, cgmRangeHi, cgmRangeLo);
  fHistoryGraph.SetReadings(Readings, UnitPref);
  fHistoryGraph.Show;
  fHistoryGraph.BringToFront;
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit);
begin
  ShowHistoryGraph(Readings, UnitPref, DefaultHistoryGraphPalette, 180, 60, 160, 80);
end;

end.
