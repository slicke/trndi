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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-09-20: The dots, the trace, the hover ring and the prediction
 *   overlay are drawn antialiased through trndi.raster instead of the
 *   aliased canvas Ellipse/LineTo primitives.
 * - 2026-09-20: The threshold range is tinted as translucent bands behind
 *   the plot, like the main window, with a hairline at each threshold in
 *   place of the earlier dashed lines.
 * - 2026-09-20: Margins, dot radius, overlay geometry and the panel
 *   paddings are 96 dpi design values scaled to the form's DPI through Px,
 *   so the graph keeps its proportions on high-DPI screens.
 * - 2026-09-20: Hovering follows the reading nearest the pointer anywhere in
 *   the plot instead of needing a hit on a dot, clears on MouseLeave, and
 *   draws a vertical hairline plus a two-line box with the value in its
 *   range colour, the time, the delta and the trend.
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
  - Hover anywhere in the plot to see the nearest reading's value, time,
    delta and trend in a box beside a hairline through the reading.
  - Lightweight, single-form instance reused via ShowHistoryGraph().

  Developer notes:
  - The layout is intentionally simple: change GRAPH_MARGIN_* constants if
    the UI needs to be more compact or if panels overflow. All pixel
    constants in this unit are 96 dpi design values; Px scales them to the
    form's actual DPI at draw time.
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
trndi.types, trndi.api, trndi.strings, slicke.ux.alert, dateutils,{$ifdef WINDOWS}trndi.native,{$endif}
ExtDlgs, IntfGraphics, FPImage, FPWritePNG, trndi.raster;

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
  FAllPoints: array of TGraphPoint; // Full set before range filtering
  FUnit: BGUnit;
  FMinValue: double;
  FMaxValue: double;
  FMinTime: TDateTime;
  FMaxTime: TDateTime;
  FDotRadius: integer; // Dot radius in 96 dpi pixels; see Px
  FPalette: THistoryGraphPalette; // Runtime palette supplied by main UI
  FCgmHi: integer; // High threshold in mg/dL
  FCgmLo: integer; // Low threshold in mg/dL
  FCgmRangeHi: integer; // Range high threshold in mg/dL
  FCgmRangeLo: integer; // Range low threshold in mg/dL
  FPopupMenu: TPopupMenu; // Context menu for right-click actions
  FRangeMenu: TMenuItem; // Time range submenu
  FSelectedRangeMinutes: integer; // 0 = all
  FHoveredPoint: integer; // Index in visible points, -1 when none
  FBasalProfile: TBasalProfile; // Optional basal profile to draw as overlay
  FShowBasal: boolean; // Whether to render basal overlay
  FMaxBasal: single; // Maximum basal rate for scaling (U/hr)
  FBoluses: TBolusList; // Optional insulin deliveries to draw as overlay
  FShowBolus: boolean; // Whether to render bolus overlay
  FShowAutoBolus: boolean; // Whether automatic micro-deliveries are included
  FCarbs: TCarbList; // Optional carbohydrate entries to draw as overlay
  FShowCarbs: boolean; // Whether to render carbohydrate overlay
  FPredictions: array of TGraphPoint; // Predicted future readings
  FInvTimeSpan: double; // 1 / (FMaxTime - FMinTime); 0 if degenerate
  FInvValueSpan: double; // 1 / (FMaxValue - FMinValue); 0 if degenerate
  FBackground: TBitmap; // Cached static layers (axes, dots, legend, ...)
  FBackgroundValid: boolean; // False when data/extents/size changed
  procedure InvalidateBackground;
  procedure RenderBackground(ABmp: TBitmap; const PlotRect: TRect);
  procedure DrawHoverRing(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawHoverOverlay: Draws the hover layer on the live canvas: a
      vertical hairline through the hovered reading, the ring around its
      dot and a two-line box with its value, time, delta and trend. }
  procedure DrawHoverOverlay(ACanvas: TCanvas; const PlotRect: TRect);
    {** GetPlotRect: Determine the plotting rectangle inside the form where
      dots and lines are drawn. Respects the margins defined above. }
  function GetPlotRect: TRect;
    {** SortPointsByTime: Ensure FPoints are sorted chronologically (ascending).
      This keeps both the polyline and chart axes consistent. }
  procedure SortPointsByTime;
    {** UpdateExtents: Recompute min/max values and times used to map
      value/time to device coordinates. Adds a small padding to avoid
      degenerate spans. The time axis always runs up to the current time, so
      stale data reads as a gap on the right rather than reaching the edge. }
  procedure UpdateExtents;
    {** DrawAxesAndGrid: Draws a simple XY-grid and labels for the value
      (left) and time (bottom) axes. Uses the plot extents from
      UpdateExtents. }
  procedure DrawAxesAndGrid(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawThresholdBands: Tints the hi/lo range as a translucent band
      across the plot, with the personal range as a deeper band inside it,
      the same way the main window shades its trend surface. Drawn under
      the grid so the grid lines stay crisp. }
  procedure DrawThresholdBands(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawThresholdLines: Draws a hairline at each threshold level in the
      corresponding level color, marking the band edges. }
  procedure DrawThresholdLines(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawBasalOverlay: Renders daily repeating basal schedule as a small
      area strip at the bottom of the plot. Values are scaled to
      `FMaxBasal` and clipped to the plot range.

      This is the *programmed* schedule, repeated across every day in view.
      Temporary rates, suspends, profile switches and anything a closed loop
      commanded are not in it, and a day drawn here is the profile in force
      now rather than the one in force then. }
  procedure DrawBasalOverlay(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawBolusOverlay: Renders insulin deliveries as stems rising from the
      bottom axis, scaled against the largest delivery in view. Automatic
      micro-deliveries are drawn thinner and paler than deliveries the user
      asked for, and only the manual ones are labelled. }
  procedure DrawBolusOverlay(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawCarbOverlay: Renders carbohydrate entries as discs in a fixed lane
      just above the bottom axis, sized by amount and labelled in grams. Kept
      to its own lane so it reads as a separate quantity from the insulin
      stems it is drawn over. }
  procedure DrawCarbOverlay(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawPolyline: Connects the chronological points with a thin
      line to indicate trend (optional visual aid). }
  procedure DrawPolyline(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawPoints: Draws a filled circle for each reading using
      colors determined by LevelColor. Clicking a dot triggers
      ShowReadingDetails. }
  procedure DrawPoints(ACanvas: TCanvas; const PlotRect: TRect);
    {** DrawPredictionOverlay: Draws predicted future readings as a dashed
      line and hollow circles continuing from the last real reading. }
  procedure DrawPredictionOverlay(ACanvas: TCanvas; const PlotRect: TRect);
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
    {** NearestPointAt: Returns the index of the point whose time position
      is closest to X while (X,Y) is inside the plot, otherwise -1. Used for
      hovering, so the pointer need not sit on a dot. }
  function NearestPointAt(const X, Y: integer): integer;
    {** Px: Scales a 96 dpi design length to the form's DPI, so margins,
      radii and paddings stay proportional on high-DPI screens. }
  function Px(const ASize: integer): integer;
  function HasData: boolean;
  procedure HoverTexts(const Index: integer; out ValueText, DetailText: string);
  procedure ApplyRangeFilter;
  procedure HandleRangeMenuClick(Sender: TObject);
  procedure UpdateRangeMenuChecks;
  procedure ShowReadingDetails(const Reading: BGReading);
protected
  procedure Paint; override;
  procedure Resize; override;
  procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
    const AXProportion, AYProportion: double); override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    override;
  procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  procedure MouseLeave; override;
  procedure KeyDown(var Key: word; Shift: TShiftState); override;
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
      on the graph. The profile repeats every 24h. @param(maxBasal) is the rate
      the strip's full height stands for; pass 0 (the default) to take it from
      the profile's own highest rate, which is what keeps the strip readable
      for a 0.4 U/hr profile and a 4 U/hr one alike. }
  procedure SetBasalProfile(const profile: TBasalProfile; const maxBasal: single = 0);
    {** Enable or disable basal overlay rendering. }
  procedure SetBasalOverlayEnabled(aEnabled: boolean);
    {** SetBoluses: Provide the insulin deliveries to draw as stems along the
      bottom axis. Pass an empty array to hide the overlay. Entries outside the
      visible time range are ignored rather than clamped to the edge. }
  procedure SetBoluses(const Boluses: TBolusList);
    {** Enable or disable bolus overlay rendering.
      @param(aEnabled Draw the overlay at all)
      @param(aIncludeAutomatic Also draw pump-initiated micro-deliveries. On an
        automated pump these arrive every few minutes and crowd out the
        deliveries the user actually asked for, so they are off by default.) }
  procedure SetBolusOverlayEnabled(aEnabled: boolean;
    aIncludeAutomatic: boolean = false);
    {** SetCarbs: Provide the carbohydrate entries to draw along the bottom
      axis. Pass an empty array to hide the overlay. }
  procedure SetCarbs(const Carbs: TCarbList);
    {** Enable or disable carbohydrate overlay rendering. }
  procedure SetCarbOverlayEnabled(aEnabled: boolean);
    {** SetPredictions: Supply predicted future readings to overlay on the graph
      as a dashed continuation past the last real reading. Pass an empty array
      to hide the overlay. }
  procedure SetPredictions(const Predictions: BGResults);
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
RS_HISTORY_GRAPH_HELP_INTERACT =
  'Hover or click dots for details. Right-click to choose a time range.';
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
RS_HISTORY_GRAPH_KEY_BOLUS = 'Bolus';
RS_HISTORY_GRAPH_KEY_BOLUS_AUTO = 'Automatic insulin';
RS_HISTORY_GRAPH_KEY_CARBS = 'Carbohydrates';
RS_HISTORY_GRAPH_SAVE_TITLE = 'Save graph as PNG';
RS_HISTORY_GRAPH_SAVE_SUCCESS = 'Graph saved successfully';
RS_HISTORY_GRAPH_SAVE_ERROR = 'Failed to save graph: %s';
RS_HISTORY_GRAPH_MENU_SAVE = 'Save as Image...';
RS_HISTORY_GRAPH_MENU_SAVE_CSV = 'Save as CSV...';
RS_HISTORY_GRAPH_CSV_TITLE = 'Save readings as CSV';
RS_HISTORY_GRAPH_MENU_RANGE = 'Time range';
RS_HISTORY_GRAPH_MENU_RANGE_ALL = 'All';
RS_HISTORY_GRAPH_MENU_RANGE_1H = 'Last 1h';
RS_HISTORY_GRAPH_MENU_RANGE_3H = 'Last 3h';
RS_HISTORY_GRAPH_MENU_RANGE_6H = 'Last 6h';
RS_HISTORY_GRAPH_MENU_RANGE_12H = 'Last 12h';
RS_HISTORY_GRAPH_MENU_RANGE_24H = 'Last 24h';
RS_HISTORY_GRAPH_KEY_PREDICT = 'Predicted';

{** Constants used for layout and division handling in this graph unit.
  Changing these values will affect overall margins and grid density. The
  margins are 96 dpi design values; GetPlotRect scales them with Px. }
const
GRAPH_MARGIN_LEFT = 72;
GRAPH_MARGIN_TOP = 40;
GRAPH_MARGIN_RIGHT = 220;
GRAPH_MARGIN_BOTTOM = 120;
GRAPH_DIVISIONS = 5;
  // Hover overlay: the box keeps a fixed light face whatever the widgetset
  // theme, since the graph background is light too, and the hairline is a
  // mid grey that reads over both the bands and the white plot.
HOVER_BOX_COLOR = $00F6F6F6;
HOVER_BORDER_COLOR = $00B8B8B8;
HOVER_LINE_COLOR = $00A8A8A8;
HOVER_SEP = ' · ';

  // Treatment overlay colours. Named because each is used twice — once to draw
  // and once for the legend chip — and a legend that disagrees with the chart
  // is worse than no legend.
function BolusColor: TColor; inline;
begin
  Result := RGBToColor(96, 76, 195);
end;

function AutoBolusColor: TColor; inline;
begin
  Result := RGBToColor(176, 168, 224);
end;

function CarbColor: TColor; inline;
begin
  Result := RGBToColor(226, 145, 42);
end;

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

procedure AddRangeItem(const ACaption: string; const AMinutes: integer);
  var
    rangeItem: TMenuItem;
  begin
    rangeItem := TMenuItem.Create(FRangeMenu);
    rangeItem.Caption := ACaption;
    rangeItem.Tag := AMinutes;
    rangeItem.RadioItem := true;
    rangeItem.GroupIndex := 1;
    rangeItem.OnClick := @HandleRangeMenuClick;
    FRangeMenu.Add(rangeItem);
  end;
begin
  inherited CreateNew(AOwner, 0);
  Caption := RS_HISTORY_GRAPH_TITLE;
  // 96 dpi design size; the LCL rescales the form to the monitor's DPI in
  // AfterConstruction, so no Px here.
  Width := 760;
  Height := 460;
  DoubleBuffered := true;
  Position := poMainFormCenter;
  BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  Color := clWhite;
  FDotRadius := 5;
  FPalette := DefaultHistoryGraphPalette;
  FSelectedRangeMinutes := 0;
  FHoveredPoint := -1;
  
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

  FRangeMenu := TMenuItem.Create(FPopupMenu);
  FRangeMenu.Caption := RS_HISTORY_GRAPH_MENU_RANGE;
  FPopupMenu.Items.Add(FRangeMenu);
  AddRangeItem(RS_HISTORY_GRAPH_MENU_RANGE_ALL, 0);
  AddRangeItem(RS_HISTORY_GRAPH_MENU_RANGE_1H, 60);
  AddRangeItem(RS_HISTORY_GRAPH_MENU_RANGE_3H, 180);
  AddRangeItem(RS_HISTORY_GRAPH_MENU_RANGE_6H, 360);
  AddRangeItem(RS_HISTORY_GRAPH_MENU_RANGE_12H, 720);
  AddRangeItem(RS_HISTORY_GRAPH_MENU_RANGE_24H, 1440);
  UpdateRangeMenuChecks;

  PopupMenu := FPopupMenu;
end;

procedure TfHistoryGraph.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  inherited DoClose(CloseAction);
end;

destructor TfHistoryGraph.Destroy;
begin
  FBackground.Free;
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
    ACanvas.TextOut(PlotRect.Left - Px(48),
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
      PlotRect.Bottom + Px(8), labelText);
  end;

  ACanvas.Font.Style := [fsBold];
  labelText := RS_HISTORY_GRAPH_AXIS_TIME;
  ACanvas.TextOut(
    (PlotRect.Left + PlotRect.Right - ACanvas.TextWidth(labelText)) div 2,
    PlotRect.Bottom + Px(60), labelText);

  labelText := Format(RS_HISTORY_GRAPH_UNIT_FMT, [BG_UNIT_NAMES[FUnit]]);
  ACanvas.Font.Orientation := 900;
  ACanvas.TextOut(PlotRect.Left - Px(GRAPH_MARGIN_LEFT) + Px(8),
    (PlotRect.Top + PlotRect.Bottom + ACanvas.TextWidth(labelText)) div 2,
    labelText);
  ACanvas.Font.Orientation := 0;
  ACanvas.Font.Style := [];
end;

procedure TfHistoryGraph.DrawThresholdBands(ACanvas: TCanvas; const PlotRect: TRect);
const
  // Tuned for the white graph background: the hi/lo wash stays faint enough
  // for the grid and the silver trace to read through it, and the personal
  // range stacks on top as a visibly deeper shade of the same colour.
  HILO_ALPHA = 30;
  CUSTOM_ALPHA = 44;
  EDGE_GRADIENT_PX = 3;
var
  bands: array of TRangeBand;
  plotW, plotH: integer;

  // Band rows are relative to the plot's top edge; DrawRangeBands clips
  // whatever falls outside the plot, so a threshold above the visible value
  // range simply fades the band into the plot border instead of stopping short.
  procedure AddBand(const hiMgdl, loMgdl: integer; const AColor: TColor;
    const AAlpha: byte);
  begin
    SetLength(bands, Length(bands) + 1);
    with bands[High(bands)] do
    begin
      Top := ValueToY(hiMgdl * BG_CONVERTIONS[FUnit][mgdl], PlotRect) - PlotRect.Top;
      Bottom := ValueToY(loMgdl * BG_CONVERTIONS[FUnit][mgdl], PlotRect) - PlotRect.Top;
      Color := AColor;
      Alpha := AAlpha;
    end;
  end;

begin
  plotW := PlotRect.Right - PlotRect.Left;
  plotH := PlotRect.Bottom - PlotRect.Top;
  if (plotW <= 0) or (plotH <= 0) then
    Exit;

  bands := nil;
  if (FCgmHi > 0) and (FCgmLo > 0) and (FCgmHi > FCgmLo) then
    AddBand(FCgmHi, FCgmLo, LevelColor(BGRange), HILO_ALPHA);

  // The personal range uses the disabled sentinels (500 / 0) when a backend
  // does not supply one, matching the checks in the legend and the hairlines.
  if (FCgmRangeHi < 500) and (FCgmRangeLo > 0) and (FCgmRangeHi > FCgmRangeLo) then
    AddBand(FCgmRangeHi, FCgmRangeLo, LevelColor(BGRange), CUSTOM_ALPHA);

  if Length(bands) > 0 then
    DrawRangeBands(ACanvas, plotW, plotH, bands, Px(EDGE_GRADIENT_PX),
      PlotRect.Left, PlotRect.Top);
end;

procedure TfHistoryGraph.DrawThresholdLines(ACanvas: TCanvas; const PlotRect: TRect);

  procedure Hairline(const valueMgdl: integer; const level: BGValLevel);
  var
    y: integer;
  begin
    y := ValueToY(valueMgdl * BG_CONVERTIONS[FUnit][mgdl], PlotRect);
    if (y < PlotRect.Top) or (y > PlotRect.Bottom) then
      Exit;
    ACanvas.Pen.Color := LevelColor(level);
    ACanvas.MoveTo(PlotRect.Left, y);
    ACanvas.LineTo(PlotRect.Right, y);
  end;

begin
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;

  Hairline(FCgmHi, BGHigh);
  Hairline(FCgmLo, BGLOW);

  // Personal range edges, skipped at the disabled sentinels
  if FCgmRangeHi <> 500 then
    Hairline(FCgmRangeHi, BGRangeHI);
  if FCgmRangeLo <> 0 then
    Hairline(FCgmRangeLo, BGRangeLO);
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
  basalHeight := Min(Max(Px(24), (PlotRect.Bottom - PlotRect.Top) div 8), Px(80));
  basalColor := RGBToColor(120, 170, 255);

  // Iterate each day in the plot range; basal profile repeats daily
  for d := Trunc(FMinTime) to Trunc(FMaxTime) do
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

  // Restore styles
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
end;

{ Insulin deliveries as stems rising from the bottom axis.

  The two kinds are drawn differently on purpose. A delivery the user asked for
  is a single event worth finding on the chart; an automated pump's corrections
  are a continuous drip of hundredths of a unit, and drawing them alike would
  bury the meal bolus among a hundred hairlines. Automatic deliveries are
  therefore thin, pale and unlabelled, and are only drawn at all when the
  caller asks for them.

  Heights are scaled against the largest delivery in view rather than a fixed
  ceiling, so the overlay stays readable whether the window holds a 12 U meal
  bolus or nothing but 0.05 U corrections. That means stem height is only
  meaningful relative to the other stems on screen — hence the labels. }
procedure TfHistoryGraph.DrawBolusOverlay(ACanvas: TCanvas; const PlotRect: TRect);
const
  MIN_STEM = 3;      // Keeps the smallest dose from vanishing into the axis
  LABEL_GAP = 4;     // Clear space between two labels before one is dropped
var
  i, x, h, stemHeight, plotHeight: integer;
  minStem, labelGap, autoWidth, manualWidth: integer;
  maxUnits: single;
  labelText: string;
  labelWidth, labelX, labelY, lastLabelRight: integer;
  manualColor, autoColor: TColor;

  function Visible(const AEntry: TBolusEntry): boolean;
  begin
    Result := (AEntry.units > 0) and
      (AEntry.time >= FMinTime) and (AEntry.time <= FMaxTime) and
      (FShowAutoBolus or (not AEntry.automatic));
  end;

begin
  if (not FShowBolus) or (Length(FBoluses) = 0) then
    Exit;

  plotHeight := PlotRect.Bottom - PlotRect.Top;
  if plotHeight <= 0 then
    Exit;

  // A third of the plot at most: tall enough to compare doses, short enough to
  // leave the glucose curve — the actual subject of the chart — unobscured.
  stemHeight := Min(Max(Px(30), plotHeight div 5), plotHeight div 3);
  minStem := Px(MIN_STEM);
  labelGap := Px(LABEL_GAP);
  // Stem widths in device pixels; the thin automatic stem must never round
  // away entirely.
  autoWidth := Max(1, Px(1));
  manualWidth := Max(3, Px(3));

  maxUnits := 0;
  for i := 0 to High(FBoluses) do
    if Visible(FBoluses[i]) and (FBoluses[i].units > maxUnits) then
      maxUnits := FBoluses[i].units;

  // Nothing in range, or every entry filtered out.
  if maxUnits <= 0 then
    Exit;

  manualColor := BolusColor;
  autoColor := AutoBolusColor;

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psClear;

  // Automatic first so a manual stem sharing a pixel column stays on top.
  for i := 0 to High(FBoluses) do
  begin
    if (not Visible(FBoluses[i])) or (not FBoluses[i].automatic) then
      Continue;
    x := TimeToX(FBoluses[i].time, PlotRect);
    h := Max(minStem, Round((FBoluses[i].units / maxUnits) * stemHeight));
    ACanvas.Brush.Color := autoColor;
    ACanvas.Rectangle(x, PlotRect.Bottom - h, x + autoWidth, PlotRect.Bottom);
  end;

  for i := 0 to High(FBoluses) do
  begin
    if (not Visible(FBoluses[i])) or FBoluses[i].automatic then
      Continue;
    x := TimeToX(FBoluses[i].time, PlotRect);
    h := Max(minStem, Round((FBoluses[i].units / maxUnits) * stemHeight));
    ACanvas.Brush.Color := manualColor;
    ACanvas.Rectangle(x - manualWidth div 2, PlotRect.Bottom - h,
      x - manualWidth div 2 + manualWidth, PlotRect.Bottom);
  end;

  // Labels last, in a second pass, so no stem can be drawn over one.
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := manualColor;
  lastLabelRight := Low(integer);

  for i := 0 to High(FBoluses) do
  begin
    if (not Visible(FBoluses[i])) or FBoluses[i].automatic then
      Continue;

    labelText := Format('%.2fU', [FBoluses[i].units]);
    labelWidth := ACanvas.TextWidth(labelText);
    x := TimeToX(FBoluses[i].time, PlotRect);
    labelX := x - (labelWidth div 2);

    // Drop a label rather than overprint the one before it; the stem is still
    // drawn, so a dose is never hidden — only its number is.
    if labelX < (lastLabelRight + labelGap) then
      Continue;
    lastLabelRight := labelX + labelWidth;

    h := Max(minStem, Round((FBoluses[i].units / maxUnits) * stemHeight));
    labelY := PlotRect.Bottom - h - ACanvas.TextHeight(labelText) - Px(2);
    if labelY < PlotRect.Top then
      labelY := PlotRect.Top;
    ACanvas.TextOut(labelX, labelY, labelText);
  end;

  ACanvas.Font.Color := clBlack;
  ACanvas.Brush.Style := bsClear;
end;

{ Carbohydrates as discs in a fixed lane just above the bottom axis.

  Grams and units are different quantities and must not share a scale, so carbs
  get their own visual language: a disc at a constant height rather than a stem
  of variable height. The radius still varies with the amount, enough to tell a
  snack from a meal at a glance, but the number on the label is what counts.

  The lane deliberately sits over the base of the insulin stems. A meal and the
  bolus that covered it happen at the same moment, and letting the two overlap
  is what shows they belong together; the disc is outlined so it stays legible
  against a stem behind it. }
procedure TfHistoryGraph.DrawCarbOverlay(ACanvas: TCanvas; const PlotRect: TRect);
const
  LANE_HEIGHT = 12;  // Disc centre above the bottom axis
  MIN_RADIUS = 4;
  MAX_RADIUS = 9;
  LABEL_GAP = 4;
var
  i, x, y, radius, plotHeight: integer;
  minRadius, maxRadius, labelGap: integer;
  maxGrams: single;
  labelText: string;
  labelWidth, labelX, labelY, lastLabelRight: integer;

  function Visible(const AEntry: TCarbEntry): boolean;
  begin
    Result := (AEntry.grams > 0) and
      (AEntry.time >= FMinTime) and (AEntry.time <= FMaxTime);
  end;

begin
  if (not FShowCarbs) or (Length(FCarbs) = 0) then
    Exit;

  plotHeight := PlotRect.Bottom - PlotRect.Top;
  if plotHeight <= 0 then
    Exit;

  maxGrams := 0;
  for i := 0 to High(FCarbs) do
    if Visible(FCarbs[i]) and (FCarbs[i].grams > maxGrams) then
      maxGrams := FCarbs[i].grams;

  if maxGrams <= 0 then
    Exit;

  y := PlotRect.Bottom - Px(LANE_HEIGHT);
  minRadius := Px(MIN_RADIUS);
  maxRadius := Px(MAX_RADIUS);
  labelGap := Px(LABEL_GAP);

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := CarbColor;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clWhite;
  ACanvas.Pen.Width := 1;

  for i := 0 to High(FCarbs) do
  begin
    if not Visible(FCarbs[i]) then
      Continue;
    x := TimeToX(FCarbs[i].time, PlotRect);
    radius := minRadius +
      Round((FCarbs[i].grams / maxGrams) * (maxRadius - minRadius));
    ACanvas.Ellipse(x - radius, y - radius, x + radius, y + radius);
  end;

  // Labels in a second pass so no disc can be drawn over one.
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := CarbColor;
  lastLabelRight := Low(integer);

  for i := 0 to High(FCarbs) do
  begin
    if not Visible(FCarbs[i]) then
      Continue;

    labelText := Format('%.0fg', [FCarbs[i].grams]);
    labelWidth := ACanvas.TextWidth(labelText);
    x := TimeToX(FCarbs[i].time, PlotRect);
    labelX := x - (labelWidth div 2);

    // Drop the number rather than overprint the one before it; the disc still
    // marks the meal.
    if labelX < (lastLabelRight + labelGap) then
      Continue;
    lastLabelRight := labelX + labelWidth;

    radius := minRadius +
      Round((FCarbs[i].grams / maxGrams) * (maxRadius - minRadius));
    labelY := y - radius - ACanvas.TextHeight(labelText) - Px(1);
    if labelY < PlotRect.Top then
      labelY := PlotRect.Top;
    ACanvas.TextOut(labelX, labelY, labelText);
  end;

  ACanvas.Font.Color := clBlack;
  ACanvas.Pen.Color := clBlack;
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
  pad, keyBox, gap, corner, minPanelWidth: integer;
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
    infoRect := Rect(PlotRect.Right + gap,
      keyRect.Bottom + gap,
      ClientWidth - gap,
      keyRect.Bottom + gap + (hInfo1 + hInfo2 + hInfo3) + (pad * 4));
    if infoRect.Right - infoRect.Left < minPanelWidth then
      infoRect.Right := infoRect.Left + minPanelWidth;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := LegendBackground;
    ACanvas.Pen.Color := $00C8C8C8;
    ACanvas.RoundRect(infoRect, corner, corner);
    ACanvas.Brush.Style := bsClear;
    textY := infoRect.Top + pad;
    ACanvas.Font.Style := [fsBold];
    ACanvas.TextOut(infoRect.Left + pad, textY, info);
    Inc(textY, lineHeight + Px(2));
    ACanvas.Font.Style := [];
    ACanvas.TextOut(infoRect.Left + pad, textY, rangeFirst);
    Inc(textY, lineHeight + Px(2));
    ACanvas.TextOut(infoRect.Left + pad, textY, rangeSecond);
  end;

  {** DrawHelpPanel: Internal helper to render a single-line help banner
      beneath the chart with instructions for interacting with the graph. }
procedure DrawHelpPanel;
  var
    panelTop: integer;
  begin
    panelTop := PlotRect.Bottom + Px(92);
    helpRect := Rect(PlotRect.Left - gap,
      panelTop,
      PlotRect.Right + gap,
      panelTop + lineHeight + pad * 2);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := LegendBackground;
    ACanvas.Pen.Color := $00C8C8C8;
    ACanvas.RoundRect(helpRect, corner, corner);
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(helpRect.Left + pad,
      helpRect.Top + pad, RS_HISTORY_GRAPH_HELP_INTERACT);
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
    ACanvas.Rectangle(keyX, keyY, keyX + keyBox, keyY + keyBox);
    ACanvas.Brush.Style := bsClear;
    textOffset := keyY + (keyBox - lineHeight) div 2;
    if textOffset < keyY then
      textOffset := keyY;
    ACanvas.TextOut(keyX + keyBox + Px(8), textOffset, Caption);
    Inc(keyY, keyBox + pad);
  end;

  {** DrawKeyPanel: Build the legend panel showing color chips and text.
      The panel is rendered to the right of the plot area to avoid covering
      the most recent readings. }
procedure DrawKeyPanel;
  var
    unitStr, rangeStr, rangeHiStr, rangeLoStr, hiStr, loStr: string;
    rangeHiVal, rangeLoVal, hiVal, loVal: double;
    hasRange: boolean;
    entries: integer;
  begin
    // A backend without a personal range reports the disabled sentinels, so
    // its key entries would read "Range (0.0 - 27.8 mmol/L)". Leave them out,
    // matching the threshold lines, which are skipped for the same reason.
    hasRange := (FCgmRangeHi <> TrndiAPI.CGM_RANGE_HI_DISABLED) and
      (FCgmRangeLo <> TrndiAPI.CGM_RANGE_LO_DISABLED);
    entries := 3;
    if hasRange then
      Inc(entries, 3);
    // The treatment keys only earn their space when the overlay is actually on.
    if FShowBasal and (Length(FBasalProfile) > 0) then
      Inc(entries);
    if FShowBolus and (Length(FBoluses) > 0) then
    begin
      Inc(entries);
      if FShowAutoBolus then
        Inc(entries);
    end;
    if FShowCarbs and (Length(FCarbs) > 0) then
      Inc(entries);

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
    
    keyRect := Rect(PlotRect.Right + gap, PlotRect.Top,
      ClientWidth - gap,
      PlotRect.Top + (keyBox + pad) * (entries - 1) + pad * 3 + lineHeight);
    if keyRect.Right - keyRect.Left < minPanelWidth then
      keyRect.Right := keyRect.Left + minPanelWidth;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := LegendBackground;
    ACanvas.Pen.Color := $00C8C8C8;
    ACanvas.RoundRect(keyRect, corner, corner);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Style := [fsBold];
    ACanvas.TextOut(keyRect.Left + pad,
      keyRect.Top + pad, RS_HISTORY_GRAPH_KEY_TITLE);
    ACanvas.Font.Style := [];
    keyX := keyRect.Left + pad;
    keyY := keyRect.Top + pad + lineHeight + Px(4);
    if hasRange then
    begin
      DrawKeyEntry(rangeStr, LevelColor(BGRange));
      DrawKeyEntry(rangeHiStr, LevelColor(BGRangeHI));
      DrawKeyEntry(rangeLoStr, LevelColor(BGRangeLO));
    end;
    DrawKeyEntry(hiStr, LevelColor(BGHigh));
    DrawKeyEntry(loStr, LevelColor(BGLOW));
    // The strip is scaled to the profile, so the height of a bar means nothing
    // without saying what the full height stands for.
    if FShowBasal and (Length(FBasalProfile) > 0) then
      DrawKeyEntry(Format('%s (0−%s U/hr)',
        [RS_HISTORY_GRAPH_KEY_BASAL, FormatFloat('0.0##', FMaxBasal)]),
        RGBToColor(120,170,255));
    if FShowBolus and (Length(FBoluses) > 0) then
    begin
      DrawKeyEntry(RS_HISTORY_GRAPH_KEY_BOLUS, BolusColor);
      if FShowAutoBolus then
        DrawKeyEntry(RS_HISTORY_GRAPH_KEY_BOLUS_AUTO, AutoBolusColor);
    end;
    if FShowCarbs and (Length(FCarbs) > 0) then
      DrawKeyEntry(RS_HISTORY_GRAPH_KEY_CARBS, CarbColor);
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
  pad := Px(INFO_PADDING);
  keyBox := Px(KEY_BOX);
  gap := Px(12);
  corner := Px(6);
  minPanelWidth := Px(160);

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
  radius := Px(FDotRadius);
  // Same disc-plus-rim the old Ellipse drew (level colour inside a 1 px black
  // outline), but rasterized with analytical coverage: the LCL Ellipse is
  // strictly aliased on GDI and Qt, and the dots are the graph's data.
  for i := 0 to High(FPoints) do
  begin
    x := TimeToX(FPoints[i].Reading.date, PlotRect);
    y := ValueToY(FPoints[i].Value, PlotRect);
    DrawSmoothCircle(ACanvas, 2 * radius, LevelColor(FPoints[i].Reading.level),
      clBlack, Max(1, Px(1)), x - radius, y - radius);
  end;
end;

procedure TfHistoryGraph.DrawHoverRing(ACanvas: TCanvas; const PlotRect: TRect);
var
  x, y, radius: integer;
begin
  if (FHoveredPoint < 0) or (FHoveredPoint > High(FPoints)) then
    Exit;
  radius := Px(FDotRadius + 3);
  x := TimeToX(FPoints[FHoveredPoint].Reading.date, PlotRect);
  y := ValueToY(FPoints[FHoveredPoint].Value, PlotRect);
  // Hollow ring (clNone disc) so the dot it circles stays visible inside.
  DrawSmoothCircle(ACanvas, 2 * radius, clNone, clBlack, Max(2, Px(2)),
    x - radius, y - radius);
end;

procedure TfHistoryGraph.InvalidateBackground;
begin
  FBackgroundValid := false;
end;

procedure TfHistoryGraph.RenderBackground(ABmp: TBitmap; const PlotRect: TRect);
begin
  // The form font is the one the LCL rescales with the form's DPI; a fresh
  // bitmap canvas would otherwise label a high-DPI plot in the 96 dpi default.
  // TFont.Assign copies the point size, not the pixel height, when the two
  // fonts disagree on DPI, so align the DPI first to carry the height over.
  ABmp.Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  ABmp.Canvas.Font.Assign(Font);
  ABmp.Canvas.Brush.Color := Color;
  ABmp.Canvas.FillRect(Rect(0, 0, ABmp.Width, ABmp.Height));
  DrawThresholdBands(ABmp.Canvas, PlotRect);
  DrawAxesAndGrid(ABmp.Canvas, PlotRect);
  DrawThresholdLines(ABmp.Canvas, PlotRect);
  DrawBasalOverlay(ABmp.Canvas, PlotRect);
  DrawBolusOverlay(ABmp.Canvas, PlotRect);
  DrawCarbOverlay(ABmp.Canvas, PlotRect);
  DrawPolyline(ABmp.Canvas, PlotRect);
  DrawPoints(ABmp.Canvas, PlotRect);
  DrawPredictionOverlay(ABmp.Canvas, PlotRect);
  DrawLegend(ABmp.Canvas, PlotRect);
end;

procedure TfHistoryGraph.DrawPolyline(ACanvas: TCanvas; const PlotRect: TRect);
const
  TRACE_WIDTH_PX = 2;
var
  i, n: integer;
  gapMinutes: integer;
  run: array of TPoint;
  runColors: array of TColor;

  // Draw the n connected points gathered in `run` as one antialiased trace.
  // A run of one point has nothing to connect.
  procedure FlushRun;
  var
    pts: array of TPoint;
    k: integer;
  begin
    if n >= 2 then
    begin
      SetLength({%H-}pts, n);
      for k := 0 to n - 1 do
        pts[k] := run[k];
      // The trace lands in the cached background bitmap, so it must not take
      // over the single-slot polyline cache the main window's live trend
      // line relies on.
      DrawSmoothPolyline(ACanvas, pts, Copy(runColors, 0, n),
        Max(1, Px(TRACE_WIDTH_PX)), false);
    end;
    n := 0;
  end;

begin
  if Length(FPoints) < 2 then
    Exit;

  SetLength({%H-}run, Length(FPoints));
  SetLength({%H-}runColors, Length(FPoints));
  for i := 0 to High(runColors) do
    runColors[i] := clSilver;

  n := 0;
  for i := 0 to High(FPoints) do
  begin
    // If the time gap is large, don't connect points with a line.
    // This makes missing samples appear as a visual break.
    if i > 0 then
    begin
      gapMinutes := MinutesBetween(FPoints[i].Reading.date,
        FPoints[i - 1].Reading.date);
      if gapMinutes >= (INTERVAL_MINUTES * 2) then
        FlushRun;
    end;
    run[n] := Point(TimeToX(FPoints[i].Reading.date, PlotRect),
      ValueToY(FPoints[i].Value, PlotRect));
    Inc(n);
  end;
  FlushRun;
end;

function TfHistoryGraph.GetPlotRect: TRect;
var
  leftEdge, topEdge, rightEdge, bottomEdge, minSpan: integer;
begin
  leftEdge := Px(GRAPH_MARGIN_LEFT);
  topEdge := Px(GRAPH_MARGIN_TOP);
  minSpan := Px(10);
  rightEdge := ClientWidth - Px(GRAPH_MARGIN_RIGHT);
  bottomEdge := ClientHeight - Px(GRAPH_MARGIN_BOTTOM);
  if rightEdge <= leftEdge + minSpan then
    rightEdge := leftEdge + minSpan;
  if bottomEdge <= topEdge + minSpan then
    bottomEdge := topEdge + minSpan;
  Result := Rect(leftEdge, topEdge, rightEdge, bottomEdge);
end;

function TfHistoryGraph.Px(const ASize: integer): integer;
begin
  Result := Scale96ToForm(ASize);
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

procedure TfHistoryGraph.MouseMove(Shift: TShiftState; X, Y: integer);
var
  idx: integer;
begin
  inherited MouseMove(Shift, X, Y);

  if not HasData then
    Exit;

  idx := NearestPointAt(X, Y);
  if idx <> FHoveredPoint then
  begin
    FHoveredPoint := idx;
    Invalidate;
  end;
end;

procedure TfHistoryGraph.MouseLeave;
begin
  inherited MouseLeave;
  // Without this the ring and box stay behind when the pointer leaves the
  // window without passing a spot NearestPointAt rejects.
  if FHoveredPoint <> -1 then
  begin
    FHoveredPoint := -1;
    Invalidate;
  end;
end;

procedure TfHistoryGraph.Paint;
var
  plotRect: TRect;
  messageText: string;
begin
  // Static layers (axes, grid, threshold lines, basal overlay, polyline,
  // dots, predictions, legend) only change when data/extents/size change.
  // Cache them in FBackground and blit; draw the hover overlay on top so
  // MouseMove repaints stay cheap.
  inherited Paint;

  if not HasData then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
    messageText := RS_HISTORY_GRAPH_EMPTY;
    Canvas.Font.Size := 12;
    Canvas.Font.Color := clBlack;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut((ClientWidth - Canvas.TextWidth(messageText)) div 2,
      (ClientHeight - Canvas.TextHeight(messageText)) div 2, messageText);
    Exit;
  end;

  plotRect := GetPlotRect;

  if FBackground = nil then
    FBackground := TBitmap.Create;
  if (FBackground.Width <> ClientWidth) or (FBackground.Height <> ClientHeight) then
  begin
    FBackground.SetSize(ClientWidth, ClientHeight);
    FBackgroundValid := false;
  end;
  if not FBackgroundValid then
  begin
    RenderBackground(FBackground, plotRect);
    FBackgroundValid := true;
  end;
  Canvas.Draw(0, 0, FBackground);

  DrawHoverOverlay(Canvas, plotRect);
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
  thresholdSq := sqr(Px(FDotRadius + 4));
  for i := 0 to High(FPoints) do
  begin
    dotX := TimeToX(FPoints[i].Reading.date, plotRect);
    dotY := ValueToY(FPoints[i].Value, plotRect);
    distSq := sqr(dotX - X) + sqr(dotY - Y);
    if distSq <= thresholdSq then
      Exit(i);
  end;
end;

function TfHistoryGraph.NearestPointAt(const X, Y: integer): integer;
var
  i, dist, bestDist, slack: integer;
  plotRect: TRect;
begin
  Result := -1;
  if not HasData then
    Exit;

  plotRect := GetPlotRect;
  // A little slack around the plot so the hover survives the pointer
  // brushing an axis label or the top margin.
  slack := Px(FDotRadius + 4);
  if (X < plotRect.Left - slack) or (X > plotRect.Right + slack) or
    (Y < plotRect.Top - slack) or (Y > plotRect.Bottom + slack) then
    Exit;

  // Nearest along the time axis only: the reading under a vertical line
  // through the pointer, which is what a sweep across the plot should pick.
  bestDist := MaxInt;
  for i := 0 to High(FPoints) do
  begin
    dist := Abs(TimeToX(FPoints[i].Reading.date, plotRect) - X);
    if dist < bestDist then
    begin
      bestDist := dist;
      Result := i;
    end;
  end;
end;

procedure TfHistoryGraph.DrawHoverOverlay(ACanvas: TCanvas; const PlotRect: TRect);
var
  dotX, dotY, pad, lineGap, sideGap, valueH, detailH, boxW, boxH: integer;
  valueText, detailText: string;
  box: TRect;
  hairline: array[0..0] of TSmoothStroke;
  savedStyle: TFontStyles;

  procedure ShiftRect(var R: TRect; const DX, DY: integer);
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
  end;

begin
  if (FHoveredPoint < 0) or (FHoveredPoint > High(FPoints)) then
    Exit;

  dotX := TimeToX(FPoints[FHoveredPoint].Reading.date, PlotRect);
  dotY := ValueToY(FPoints[FHoveredPoint].Value, PlotRect);

  // The hairline goes first so the ring and the box sit on top of it. A
  // capsule stroke on integer coordinates covers exactly one column, so the
  // line stays crisp while matching the antialiased trace and dots.
  hairline[0].X1 := dotX;
  hairline[0].Y1 := PlotRect.Top;
  hairline[0].X2 := dotX;
  hairline[0].Y2 := PlotRect.Bottom;
  hairline[0].Color := HOVER_LINE_COLOR;
  DrawSmoothStrokes(ACanvas, hairline, Max(1, Px(1)));
  DrawHoverRing(ACanvas, PlotRect);

  HoverTexts(FHoveredPoint, valueText, detailText);
  pad := Px(8);
  lineGap := Px(2);
  sideGap := Px(12);

  savedStyle := ACanvas.Font.Style;
  ACanvas.Font.Style := [fsBold];
  valueH := ACanvas.TextHeight(valueText);
  boxW := ACanvas.TextWidth(valueText);
  ACanvas.Font.Style := [];
  detailH := ACanvas.TextHeight(detailText);
  boxW := Max(boxW, ACanvas.TextWidth(detailText)) + 2 * pad;
  boxH := valueH + lineGap + detailH + 2 * pad;

  // Above and to the right of the dot; flipped to the left of the hairline
  // near the right edge and clamped into the plot vertically, so the box
  // never covers the reading it describes and never leaves the plot.
  box := Rect(dotX + sideGap, dotY - boxH - Px(4), dotX + sideGap + boxW,
    dotY - Px(4));
  if box.Right > PlotRect.Right then
    ShiftRect(box, -(boxW + 2 * sideGap), 0);
  if box.Left < PlotRect.Left then
    ShiftRect(box, (PlotRect.Left - box.Left) + Px(4), 0);
  if box.Top < PlotRect.Top then
    ShiftRect(box, 0, (PlotRect.Top - box.Top) + Px(4));
  if box.Bottom > PlotRect.Bottom then
    ShiftRect(box, 0, (PlotRect.Bottom - box.Bottom) - Px(4));

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := HOVER_BOX_COLOR;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := HOVER_BORDER_COLOR;
  ACanvas.RoundRect(box, Px(6), Px(6));

  // The live canvas inherits the widgetset's font colour (white on dark
  // themes), so both lines pin their colour against the fixed light box.
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := LevelColor(FPoints[FHoveredPoint].Reading.level);
  ACanvas.TextOut(box.Left + pad, box.Top + pad, valueText);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := clBlack;
  ACanvas.TextOut(box.Left + pad, box.Top + pad + valueH + lineGap, detailText);
  ACanvas.Font.Style := savedStyle;
end;

procedure TfHistoryGraph.Resize;
begin
  inherited Resize;
  InvalidateBackground;
  Invalidate;
end;

procedure TfHistoryGraph.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
const AXProportion, AYProportion: double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  // A DPI change (the window dragged to another monitor) changes what Px
  // returns, so the cached static layers are stale even if the client size
  // happens to stay the same.
  if AMode = lapAutoAdjustForDPI then
  begin
    InvalidateBackground;
    Invalidate;
  end;
end;

procedure TfHistoryGraph.KeyDown(var Key: word; Shift: TShiftState);
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
  SetLength(FAllPoints, 0);
  SetLength(FPredictions, 0);
  FHoveredPoint := -1;
  InvalidateBackground;

  if Length(Readings) = 0 then
  begin
    Caption := Format('%s (0)', [RS_HISTORY_GRAPH_TITLE]);
    Invalidate;
    Exit;
  end;

  SetLength(FAllPoints, Length(Readings));
  idx := 0;
  for i := Low(Readings) to High(Readings) do
  begin
    if Readings[i].empty then
      Continue;
    FAllPoints[idx].Reading := Readings[i];
    FAllPoints[idx].Value := Readings[i].convert(UnitPref);
    Inc(idx);
  end;
  SetLength(FAllPoints, idx);

  if idx = 0 then
  begin
    Caption := Format('%s (0)', [RS_HISTORY_GRAPH_TITLE]);
    Invalidate;
    Exit;
  end;

  ApplyRangeFilter;
  if not HasData then
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

{** ApplyRangeFilter: Rebuild the working point list in FPoints from FAllPoints
  using FSelectedRangeMinutes as the active filter. When the range is set,
  the routine drops readings older than the computed cutoff, counted back from
  the current time (or from the newest reading when that is later, e.g. an
  uploader whose clock runs ahead); when the range is zero, all points are kept.
  FHoveredPoint is reset so hover state never points at an index that may no
  longer exist after filtering. }
procedure TfHistoryGraph.ApplyRangeFilter;
var
  i, idx: integer;
  maxStamp, cutoff: TDateTime;
begin
  SetLength(FPoints, 0);
  if Length(FAllPoints) = 0 then
    Exit;

  maxStamp := FAllPoints[0].Reading.date;
  for i := 1 to High(FAllPoints) do
    if FAllPoints[i].Reading.date > maxStamp then
      maxStamp := FAllPoints[i].Reading.date;

  // Same reasoning as UpdateExtents: the window the user picked is a wall-clock
  // one. Counting it back from the newest reading silently slides it into the
  // past during an outage — "last 3 hours" would show 15:00-18:00 an hour after
  // the data stopped at 18:00. Anchoring on Now can leave the selection empty
  // when nothing arrived inside it, which is the honest answer.
  if maxStamp < Now then
    maxStamp := Now;

  if FSelectedRangeMinutes > 0 then
    cutoff := IncMinute(maxStamp, -FSelectedRangeMinutes)
  else
    cutoff := 0;

  SetLength(FPoints, Length(FAllPoints));
  idx := 0;
  for i := 0 to High(FAllPoints) do
  begin
    if (FSelectedRangeMinutes > 0) and (FAllPoints[i].Reading.date < cutoff) then
      Continue;
    FPoints[idx] := FAllPoints[i];
    Inc(idx);
  end;
  SetLength(FPoints, idx);
  FHoveredPoint := -1;
end;

{** HandleRangeMenuClick: Process a range-menu selection from FRangeMenu.
  Sender is expected to be a TMenuItem whose Tag stores the selected range
  in minutes; the handler copies that value into FSelectedRangeMinutes,
  refreshes the menu checks, reapplies filtering and updates the chart. }
procedure TfHistoryGraph.HandleRangeMenuClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then
    Exit;

  FSelectedRangeMinutes := TMenuItem(Sender).Tag;
  UpdateRangeMenuChecks;
  ApplyRangeFilter;
  if HasData then
  begin
    SortPointsByTime;
    UpdateExtents;
  end;
  Caption := Format('%s (%d)', [RS_HISTORY_GRAPH_TITLE, Length(FPoints)]);
  InvalidateBackground;
  Invalidate;
end;

{** UpdateRangeMenuChecks: Synchronize the checked state of FRangeMenu items
  with FSelectedRangeMinutes so the active range remains visible in the menu.
  Each menu item's Tag is treated as its range-in-minutes identifier and the
  matching item is marked checked. }
procedure TfHistoryGraph.UpdateRangeMenuChecks;
var
  i: integer;
  item: TMenuItem;
begin
  if not Assigned(FRangeMenu) then
    Exit;

  for i := 0 to FRangeMenu.Count - 1 do
  begin
    item := FRangeMenu.Items[i];
    item.Checked := item.Tag = FSelectedRangeMinutes;
  end;
end;

{** HoverTexts: Build the two hover box lines for the point at Index: the
    value with its unit, and the reading time followed by the delta and the
    trend glyph. The delta is the reading's own when the backend supplied
    one, otherwise the difference to the previous plotted point. }
procedure TfHistoryGraph.HoverTexts(const Index: integer; out ValueText,
  DetailText: string);
var
  delta: double;
  sign: string;
begin
  ValueText := Format(BG_MSG_DEF[FUnit], [FPoints[Index].Value]);
  DetailText := FormatDateTime('ddd hh:nn', FPoints[Index].Reading.date);

  if not FPoints[Index].Reading.deltaEmpty then
    DetailText := DetailText + HOVER_SEP +
      FPoints[Index].Reading.format(FUnit, BG_MSG_SIG_SHORT, BGDelta)
  else if Index > 0 then
  begin
    delta := FPoints[Index].Value - FPoints[Index - 1].Value;
    if delta > 0 then
      sign := '+'
    else if delta < 0 then
      sign := ''
    else
      sign := '±';
    DetailText := DetailText + HOVER_SEP + Format(StringReplace(
      BG_MSG_SIG_SHORT[FUnit], '%+', sign, [rfReplaceAll]), [delta]);
  end;

  // The two non-directional trends ('?' and the placeholder) tell the
  // reader nothing and look like a rendering bug, so they are left out.
  if FPoints[Index].Reading.trend in [TdDoubleUp..TdDoubleDown] then
    DetailText := DetailText + HOVER_SEP + FPoints[Index].Reading.trend.Img;
end;

procedure TfHistoryGraph.SetPalette(const Palette: THistoryGraphPalette);
begin
  FPalette := Palette;
  InvalidateBackground;
end;

procedure TfHistoryGraph.SetThresholds(const cgmHi, cgmLo, cgmRangeHi,
cgmRangeLo: integer);
begin
  FCgmHi := cgmHi;
  FCgmLo := cgmLo;
  FCgmRangeHi := cgmRangeHi;
  FCgmRangeLo := cgmRangeLo;
  InvalidateBackground;
end;

procedure TfHistoryGraph.SetBasalProfile(const profile: TBasalProfile; const maxBasal: single);
var
  i: integer;
  peak: single;
begin
  FBasalProfile := Copy(profile);

  if maxBasal > 0 then
    FMaxBasal := maxBasal
  else
  begin
    // Scale to the profile's own peak, the way the bolus stems scale to the
    // largest dose in view. A fixed ceiling cannot suit both ends of the range
    // it has to cover: against 3 U/hr a 0.4 U/hr day is a flat sliver with no
    // readable shape, and every rate at or above the ceiling draws at full
    // height, so a 3 and a 6 look identical.
    peak := 0;
    for i := 0 to High(FBasalProfile) do
      if FBasalProfile[i].value > peak then
        peak := FBasalProfile[i].value;
    // Headroom keeps the tallest bar off the top edge of the strip, and the
    // floor stops an all-zero profile dividing the height by nothing.
    FMaxBasal := Max(peak * 1.15, 0.1);
  end;

  InvalidateBackground;
  Invalidate;
end;

procedure TfHistoryGraph.SetBasalOverlayEnabled(aEnabled: boolean);
begin
  FShowBasal := aEnabled;
  InvalidateBackground;
  Invalidate;
end;

procedure TfHistoryGraph.SetBoluses(const Boluses: TBolusList);
begin
  FBoluses := Copy(Boluses);
  InvalidateBackground;
  Invalidate;
end;

procedure TfHistoryGraph.SetBolusOverlayEnabled(aEnabled: boolean;
aIncludeAutomatic: boolean = false);
begin
  FShowBolus := aEnabled;
  FShowAutoBolus := aIncludeAutomatic;
  InvalidateBackground;
  Invalidate;
end;

procedure TfHistoryGraph.SetCarbs(const Carbs: TCarbList);
begin
  FCarbs := Copy(Carbs);
  InvalidateBackground;
  Invalidate;
end;

procedure TfHistoryGraph.SetCarbOverlayEnabled(aEnabled: boolean);
begin
  FShowCarbs := aEnabled;
  InvalidateBackground;
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

        // The same static render Paint caches in FBackground, so the export is
        // what is on screen minus the hover overlay. Repeating the individual
        // draw calls here is what let the bolus and carb overlays fall out of
        // exported images while they were visible in the window.
        plotRect := GetPlotRect;
        RenderBackground(bmp, plotRect);

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
        SlickeHTMLMsg(sdsAuto, 'Error', Format(RS_HISTORY_GRAPH_SAVE_ERROR, [E.Message]),
          [mbOK], uxmtError, 12.5);
    end;
  finally
    saveDialog.Free;
  end;
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
        BGRange:
          levelStr := 'In Range';
        BGRangeHI:
          levelStr := 'Range High';
        BGRangeLO:
          levelStr := 'Range Low';
        BGHigh:
          levelStr := 'High';
        BGLOW:
          levelStr := 'Low';
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
        SlickeHTMLMsg(sdsAuto, 'Error', Format(RS_HISTORY_GRAPH_SAVE_ERROR, [E.Message]),
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

  SlickeHTMLMsg(sdsAuto, RS_RH_READING, '<font size="4"><u></u>'+TimeToStr(Reading.date) +
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
  ratio: double;
begin
  // FInvTimeSpan is precomputed in UpdateExtents so this hot inner-loop path
  // multiplies instead of dividing.
  if FInvTimeSpan = 0 then
    Exit((PlotRect.Left + PlotRect.Right) div 2);

  ratio := (TimeValue - FMinTime) * FInvTimeSpan;
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

  for i := 0 to High(FPredictions) do
  begin
    if FPredictions[i].Reading.date > FMaxTime then
      FMaxTime := FPredictions[i].Reading.date;
    if FPredictions[i].Value < FMinValue then
      FMinValue := FPredictions[i].Value;
    if FPredictions[i].Value > FMaxValue then
      FMaxValue := FPredictions[i].Value;
  end;

  // The time axis has to end at the wall clock, not at the data. Taking
  // FMaxTime from the newest reading pins that reading to the right edge of
  // the plot for the whole of an outage, so an hour-old trace is drawn exactly
  // like a live one — the graph keeps claiming to reach "now" when it doesn't.
  // Extending to Now leaves the gap visible as empty axis on the right.
  // Predictions are future-dated and already folded in above, so this only
  // ever widens the span when the data itself has fallen behind.
  if FMaxTime < Now then
    FMaxTime := Now;

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

  if (FMaxTime - FMinTime) > 0 then
    FInvTimeSpan := 1.0 / (FMaxTime - FMinTime)
  else
    FInvTimeSpan := 0;
  if (FMaxValue - FMinValue) > 0 then
    FInvValueSpan := 1.0 / (FMaxValue - FMinValue)
  else
    FInvValueSpan := 0;
end;

function TfHistoryGraph.ValueToY(const Value: double;
const PlotRect: TRect): integer;
var
  ratio: double;
begin
  if FInvValueSpan = 0 then
    Exit((PlotRect.Top + PlotRect.Bottom) div 2);

  ratio := (Value - FMinValue) * FInvValueSpan;
  ratio := EnsureRange(ratio, 0, 1);
  Result := PlotRect.Bottom - Round(ratio * (PlotRect.Bottom - PlotRect.Top));
end;

procedure TfHistoryGraph.SetPredictions(const Predictions: BGResults);
var
  i, idx: integer;
begin
  SetLength(FPredictions, Length(Predictions));
  idx := 0;
  for i := Low(Predictions) to High(Predictions) do
  begin
    if Predictions[i].empty then
      Continue;
    FPredictions[idx].Reading := Predictions[i];
    FPredictions[idx].Value   := Predictions[i].convert(FUnit);
    Inc(idx);
  end;
  SetLength(FPredictions, idx);
  if HasData then
    UpdateExtents;
  InvalidateBackground;
  Invalidate;
end;

procedure TfHistoryGraph.DrawPredictionOverlay(ACanvas: TCanvas;
const PlotRect: TRect);
const
  PREDICT_COLOR: TColor = $00C88050; // muted steel-blue (BGR: R=80 G=128 B=200)
  DASH_PX = 6;
  GAP_PX = 4;
var
  i, n: integer;
  x, y, radius: integer;
  pts: array of TPoint;
begin
  if Length(FPredictions) = 0 then
    Exit;

  radius := Max(2, Px(FDotRadius - 1));

  // Dashed line from the last real point through each prediction. Anchored
  // on the last reading when there is one, so the forecast visibly continues
  // the trace rather than floating beside it.
  SetLength({%H-}pts, Length(FPredictions) + 1);
  n := 0;
  if Length(FPoints) > 0 then
  begin
    pts[n] := Point(TimeToX(FPoints[High(FPoints)].Reading.date, PlotRect),
      ValueToY(FPoints[High(FPoints)].Value, PlotRect));
    Inc(n);
  end;
  for i := 0 to High(FPredictions) do
  begin
    pts[n] := Point(TimeToX(FPredictions[i].Reading.date, PlotRect),
      ValueToY(FPredictions[i].Value, PlotRect));
    Inc(n);
  end;
  SetLength(pts, n);
  DrawSmoothDashedPolyline(ACanvas, pts, PREDICT_COLOR, Max(1, Px(1)),
    Px(DASH_PX), Px(GAP_PX));

  // Hollow circles at each predicted point
  for i := 0 to High(FPredictions) do
  begin
    x := TimeToX(FPredictions[i].Reading.date, PlotRect);
    y := ValueToY(FPredictions[i].Value, PlotRect);
    DrawSmoothCircle(ACanvas, 2 * radius, clNone, PREDICT_COLOR, Max(1, Px(1)),
      x - radius, y - radius);
  end;
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette);
begin
  ShowHistoryGraph(Readings, UnitPref, Palette, 180, 60, 160, 80);
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette; const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
begin
  if not Assigned(fHistoryGraph) then
    fHistoryGraph := TfHistoryGraph.Create(Application){$ifdef windows}{$endif};

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
