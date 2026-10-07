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
 * - 2026-10-07: New unit. The plotting half of uhistorygraph (data, extents,
 *   drawing, hover, export render) moved here as THistoryGraphView, a
 *   TCustomControl the history window and the web API's PNG render share.
 *   The plot itself was redrawn: no frame, "nice" tick steps on whole hours
 *   and round values, an axis that scales to the readings and thresholds
 *   instead of the sensor's limits, rimless dots sized to the cadence, a
 *   legend of chips above the plot in place of the side panels, a light and
 *   a dark theme, and zooming (wheel, keys) and panning (drag, keys) of the
 *   time axis.
 *)

{**
  uhistorygraphview - The history plot as a reusable control.

  THistoryGraphView draws a sequence of BG readings as dots joined by a
  trace over a time axis, with the threshold bands behind it and optional
  treatment overlays (basal schedule, insulin deliveries, carbohydrates)
  along the bottom and a dashed forecast past the last reading. The window
  in uhistorygraph wraps it with a toolbar, a context menu and the export
  dialogs; the web API renders it off-screen through RenderToStream.

  The visible stretch of the time axis is a window inside the data span.
  The presets a caller hands SetRangeMinutes anchor it on the wall clock
  ("the last 3 hours"), and the wheel, a drag or the arrow keys move and
  resize it freely from there. Everything drawn is clipped to the window,
  so a reading outside it is neither drawn nor hovered.

  Nothing here opens a dialog or reads a resource string: the captions
  the plot needs arrive through the Captions record, and a click on a dot
  is reported through OnReadingClick for the host to act on. That keeps
  the control embeddable in a form that is never shown (the web render)
  and free of any LCL unit a headless build would have to stub.

  All pixel constants are 96 dpi design values; Px scales them to the
  owning form's DPI at draw time. Text is measured against the canvas
  being drawn on, never against the control's own canvas outside Paint
  (Cocoa).
}
unit uhistorygraphview;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Controls, Graphics, Math, DateUtils, LCLType,
IntfGraphics, FPWritePNG, trndi.types, trndi.api, trndi.raster;

type
  {** THistoryGraphPalette
      Level colours the plot maps BG levels to. The main window hands its
      runtime palette in so the graph matches the rest of the UI; the
      defaults are the muted set the window falls back to. }
THistoryGraphPalette = record
  Range: TColor;
  RangeHigh: TColor;
  RangeLow: TColor;
  High: TColor;
  Low: TColor;
  Unknown: TColor;
end;

  {** THistoryGraphTheme
      Surface, text and line colours of the plot. Two presets exist
      (LightHistoryGraphTheme, DarkHistoryGraphTheme); the level palette
      stays the same across them. }
THistoryGraphTheme = record
  Background: TColor;   // The whole control face
  Text: TColor;         // Labels and hover text
  Muted: TColor;        // Tick labels, status line, chip captions
  Grid: TColor;         // Horizontal grid lines
  GridMinor: TColor;    // Vertical grid lines at the time ticks
  Axis: TColor;         // Baseline under the plot
  Trace: TColor;        // The line joining the dots
  ChipFill: TColor;     // Legend chip face
  ChipBorder: TColor;   // Legend chip outline
  HoverFill: TColor;    // Hover box face
  HoverBorder: TColor;  // Hover box outline
  HoverLine: TColor;    // Hairline through the hovered reading
  BandAlpha: byte;      // Opacity of the hi/lo threshold band
  RangeAlpha: byte;     // Opacity of the personal range band stacked on it
  Dark: boolean;
end;

  {** THistoryGraphCaptions
      The translatable strings the plot draws. The host assigns its own
      resourcestrings; the defaults are English. }
THistoryGraphCaptions = record
  Empty: string;         // Shown when there is nothing to plot
  ReadingCount: string;  // '%d readings'
  KeyRange: string;      // Personal range chip: 'In range'
  KeyRangeHigh: string;  // 'Range high'
  KeyRangeLow: string;   // 'Range low'
  KeyHigh: string;       // 'High'
  KeyLow: string;        // 'Low'
  KeyUnknown: string;    // 'Unknown'
  KeyBasal: string;      // 'Basal'
  KeyBolus: string;      // 'Bolus'
  KeyBolusAuto: string;  // 'Automatic insulin'
  KeyCarbs: string;      // 'Carbohydrates'
  KeyPredict: string;    // 'Predicted'
end;

THistoryGraphReadingEvent = procedure(Sender: TObject;
  const Reading: BGReading) of object;

  {** THistoryGraphView
      The plot. Feed it readings with SetReadings, the thresholds, palette
      and theme through their setters, and overlays through the Set*
      methods; pick a time window with SetRangeMinutes or let the user
      zoom and pan. OnViewChanged fires whenever the window, the data or
      an overlay flag changes, so a host can keep its toolbar in step. }
THistoryGraphView = class(TCustomControl)
private
  type
    TGraphPoint = record
      Reading: BGReading;
      Value: double;      // Reading converted to FUnit once, at load
    end;
    TGraphPoints = array of TGraphPoint;
    TPointRun = array of TPoint;
    TPointRuns = array of TPointRun;
    TValueTick = record
      Value: double;
      Text: string;
    end;
    TTimeTick = record
      Time: TDateTime;
      Text: string;
      DayStart: boolean;  // Midnight: labelled with the day, drawn darker
    end;
    TChip = record
      Text: string;
      Color: TColor;
      Hollow: boolean;    // Ring instead of a disc (the forecast)
      Width: integer;
    end;
private
  FPoints: TGraphPoints;       // All readings, chronological
  FPredictions: TGraphPoints;  // Forecast, chronological
  FUnit: BGUnit;
  FPalette: THistoryGraphPalette;
  FTheme: THistoryGraphTheme;
  FCaptions: THistoryGraphCaptions;
  FCgmHi, FCgmLo, FCgmRangeHi, FCgmRangeLo: integer; // mg/dL
  FBasalProfile: TBasalProfile;
  FShowBasal: boolean;
  FMaxBasal: single;           // Rate the basal strip's full height stands for
  FBoluses: TBolusList;
  FShowBolus: boolean;
  FShowAutoBolus: boolean;
  FCarbs: TCarbList;
  FShowCarbs: boolean;
  // Time window
  FDataStart, FDataEnd: TDateTime;     // Oldest reading .. max(now, newest, forecast)
  FWindowStart, FWindowEnd: TDateTime; // The visible stretch of that
  FRangeMinutes: integer;              // Preset in force: 0 all, -1 custom (zoomed/panned)
  FInvWindow: double;                  // 1 / window span, 0 when degenerate
  // Value axis, settled per render since the tick step depends on the height
  FMinValue, FMaxValue: double;
  FInvValueSpan: double;
  FValueTicks: array of TValueTick;
  FTimeTicks: array of TTimeTick;
  // Layout of the last render, for hit testing
  FPlot: TRect;
  FLayoutValid: boolean;
  FDotRadius: integer;         // Device pixels, settled per render from the cadence
  // Interaction
  FHovered: integer;           // Index into FPoints, -1 when none
  FDragging: boolean;
  FDragMoved: boolean;
  FDragX: integer;
  FDragWindowStart: TDateTime;
  FWheelRemainder: integer;
  // Cached static layers
  FBackground: TBitmap;
  FBackgroundValid: boolean;
  FOnReadingClick: THistoryGraphReadingEvent;
  FOnViewChanged: TNotifyEvent;
  function Px(const ASize: integer): integer;
  function Conv(const AMgdl: integer): double;
  function LevelColor(const Level: BGValLevel): TColor;
  function FormatValue(const AValue: double): string;
  function TimeToX(const ATime: TDateTime; const APlot: TRect): integer;
  function XToTime(const AX: integer; const APlot: TRect): TDateTime;
  function ValueToY(const AValue: double; const APlot: TRect): integer;
  function InWindow(const ATime: TDateTime): boolean;
  function HasData: boolean;
  procedure InvalidateBackground;
  procedure SortPoints(var APoints: TGraphPoints);
  procedure UpdateDataSpan;
  procedure ApplyRange;
  procedure SetWindow(AStart, AEnd: TDateTime);
  procedure ViewChanged;
  procedure PrepareValueAxis(const APlotHeight: integer);
  procedure PrepareTimeAxis(ACanvas: TCanvas; const APlotWidth: integer);
  procedure ClipSeries(const ASeries: TGraphPoints; const APlot: TRect;
    const ABreakGapMinutes: integer; out ARuns: TPointRuns);
  procedure BuildChips(ACanvas: TCanvas; var AChips: array of TChip;
    out ACount: integer);
  function LayoutChips(ACanvas: TCanvas; var AChips: array of TChip;
    const ACount, AWidth: integer; const ADraw: boolean): integer;
  procedure DrawThresholdBands(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawAxes(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawThresholdLines(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawBasalOverlay(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawBolusOverlay(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawCarbOverlay(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawTrace(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawDots(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawPredictions(ACanvas: TCanvas; const APlot: TRect);
  procedure DrawStatus(ACanvas: TCanvas; const AWidth, AHeight: integer);
  procedure DrawEmpty(ACanvas: TCanvas; const AWidth, AHeight: integer);
  procedure DrawHoverOverlay(ACanvas: TCanvas; const APlot: TRect);
  procedure HoverTexts(const AIndex: integer; out AValueText,
    ADetailText: string);
  function PointAt(const X, Y: integer): integer;
  function NearestPointAt(const X, Y: integer): integer;
  procedure SetHovered(const AIndex: integer);
  procedure SetPalette(const AValue: THistoryGraphPalette);
  procedure SetTheme(const AValue: THistoryGraphTheme);
  procedure SetCaptions(const AValue: THistoryGraphCaptions);
  function GetVisibleCount: integer;
  function GetHasBasal: boolean;
  function GetHasBolus: boolean;
  function GetHasAutoBolus: boolean;
  function GetHasCarbs: boolean;
protected
  procedure Paint; override;
  procedure Resize; override;
  procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
    const AXProportion, AYProportion: double); override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: integer); override;
  procedure MouseLeave; override;
  function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
    MousePos: TPoint): boolean; override;
  procedure KeyDown(var Key: word; Shift: TShiftState); override;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
    {** SetReadings: Load the readings to plot. Empty readings are dropped,
      the rest converted to @code(UnitPref) and sorted. The current range
      preset is re-applied to the new data; a custom window falls back to
      the whole span. }
  procedure SetReadings(const Readings: BGResults; UnitPref: BGUnit);
    {** SetPredictions: Forecast readings to draw as a dashed continuation
      past the last real one. An empty array hides the forecast. }
  procedure SetPredictions(const Predictions: BGResults);
    {** SetThresholds: The CGM thresholds in mg/dL that place the bands,
      the hairlines and the legend chips. }
  procedure SetThresholds(const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
    {** SetBasalProfile: A daily repeating basal schedule drawn as a strip
      along the bottom. @code(maxBasal) is the rate the strip's full height
      stands for; 0 takes the profile's own peak. }
  procedure SetBasalProfile(const profile: TBasalProfile;
    const maxBasal: single = 0);
  procedure SetBasalOverlayEnabled(aEnabled: boolean);
    {** SetBoluses: Insulin deliveries drawn as stems from the bottom axis.
      An empty array hides the overlay. }
  procedure SetBoluses(const Boluses: TBolusList);
    {** SetBolusOverlayEnabled: @code(aIncludeAutomatic) also draws the
      pump's own micro-deliveries, thin and pale, which are otherwise left
      out so they cannot bury the doses the user asked for. }
  procedure SetBolusOverlayEnabled(aEnabled: boolean;
    aIncludeAutomatic: boolean = false);
    {** SetCarbs: Carbohydrate entries drawn as discs in a lane above the
      bottom axis. An empty array hides the overlay. }
  procedure SetCarbs(const Carbs: TCarbList);
  procedure SetCarbOverlayEnabled(aEnabled: boolean);
    {** SetRangeMinutes: Show the last @code(AMinutes), counted back from
      now (or from the newest reading or forecast when that is later); 0
      shows everything. Replaces any zoom or pan. }
  procedure SetRangeMinutes(const AMinutes: integer);
    {** ZoomBy: Scale the window by @code(AFactor) (> 1 zooms in) around the
      time under @code(AAnchorX), or around the window's centre when the
      anchor is negative. The window never grows past the data span nor
      shrinks under fifteen minutes. }
  procedure ZoomBy(const AFactor: double; const AAnchorX: integer = -1);
    {** PanBy: Slide the window by @code(AFraction) of its own span,
      positive toward the present, clamped to the data span. }
  procedure PanBy(const AFraction: double);
    {** ResetView: Back to the whole data span (the "All" preset). }
  procedure ResetView;
    {** RenderTo: Draw the static plot (everything but the hover overlay)
      at @code(AWidth) by @code(AHeight) into @code(ACanvas). Needs no
      window handle. }
  procedure RenderTo(ACanvas: TCanvas; const AWidth, AHeight: integer);
    {** RenderToStream: RenderTo, encoded as a PNG. }
  procedure RenderToStream(AStream: TStream; const AWidth, AHeight: integer);
    {** VisibleReadings: The readings inside the current window,
      chronological - what the CSV export writes. }
  function VisibleReadings: BGResults;
  property VisibleCount: integer read GetVisibleCount;
    {** RangeMinutes: The preset in force: 0 for everything, -1 once the
      user has zoomed or panned away from a preset. }
  property RangeMinutes: integer read FRangeMinutes;
  property Palette: THistoryGraphPalette read FPalette write SetPalette;
  property Theme: THistoryGraphTheme read FTheme write SetTheme;
  property Captions: THistoryGraphCaptions read FCaptions write SetCaptions;
  property DisplayUnit: BGUnit read FUnit;
  property HasReadings: boolean read HasData;
  property HasBasal: boolean read GetHasBasal;
  property HasBolus: boolean read GetHasBolus;
  property HasAutoBolus: boolean read GetHasAutoBolus;
  property HasCarbs: boolean read GetHasCarbs;
  property BasalVisible: boolean read FShowBasal;
  property BolusVisible: boolean read FShowBolus;
  property AutoBolusVisible: boolean read FShowAutoBolus;
  property CarbsVisible: boolean read FShowCarbs;
    {** OnReadingClick: A click on a dot (not a drag). }
  property OnReadingClick: THistoryGraphReadingEvent read FOnReadingClick
    write FOnReadingClick;
    {** OnViewChanged: The window, the data or an overlay flag changed. }
  property OnViewChanged: TNotifyEvent read FOnViewChanged write FOnViewChanged;
  property Align;
  property Anchors;
  property Color;
  property Font;
  property ParentFont;
  property PopupMenu;
  property ShowHint;
  property ParentShowHint;
  property TabStop;
  property OnKeyDown;
end;

function DefaultHistoryGraphPalette: THistoryGraphPalette;
function DefaultHistoryGraphCaptions: THistoryGraphCaptions;
function LightHistoryGraphTheme: THistoryGraphTheme;
function DarkHistoryGraphTheme: THistoryGraphTheme;

implementation

uses
trndi.funcs;

const
  // 96 dpi design values; see Px
MARGIN_SIDE = 12;
MARGIN_TOP = 10;
CHIP_GAP = 6;
CHIP_PAD_X = 9;
CHIP_PAD_Y = 4;
CHIP_DOT = 8;
AXIS_LABEL_GAP = 8;
TRACE_WIDTH = 2;
DOT_MIN = 2;
DOT_MAX = 4;
MIN_WINDOW_MINUTES = 15;
HOVER_SEP = ' · ';
WHEEL_NOTCH = 120;
ZOOM_STEP = 1.25;

  // Treatment overlay colours. Named because each is used twice - once to draw
  // and once for the legend chip - and a legend that disagrees with the chart
  // is worse than no legend.
function BasalColor: TColor; inline;
begin
  Result := RGBToColor(120, 170, 255);
end;

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

function PredictColor: TColor; inline;
begin
  Result := RGBToColor(80, 128, 200);
end;

function DefaultHistoryGraphPalette: THistoryGraphPalette;
begin
  Result.Range := RGBToColor(64, 145, 108);
  Result.RangeHigh := RGBToColor(64, 145, 108);
  Result.RangeLow := RGBToColor(33, 99, 174);
  Result.High := RGBToColor(217, 95, 2);
  Result.Low := RGBToColor(33, 99, 174);
  Result.Unknown := RGBToColor(180, 180, 180);
end;

function DefaultHistoryGraphCaptions: THistoryGraphCaptions;
begin
  Result.Empty := 'No history data to plot';
  Result.ReadingCount := '%d readings';
  Result.KeyRange := 'In range';
  Result.KeyRangeHigh := 'Range high';
  Result.KeyRangeLow := 'Range low';
  Result.KeyHigh := 'High';
  Result.KeyLow := 'Low';
  Result.KeyUnknown := 'Unknown';
  Result.KeyBasal := 'Basal';
  Result.KeyBolus := 'Bolus';
  Result.KeyBolusAuto := 'Automatic insulin';
  Result.KeyCarbs := 'Carbohydrates';
  Result.KeyPredict := 'Predicted';
end;

function LightHistoryGraphTheme: THistoryGraphTheme;
begin
  Result.Background := clWhite;
  Result.Text := RGBToColor(32, 32, 32);
  Result.Muted := RGBToColor(110, 110, 110);
  Result.Grid := RGBToColor(232, 232, 232);
  Result.GridMinor := RGBToColor(243, 243, 243);
  Result.Axis := RGBToColor(200, 200, 200);
  Result.Trace := RGBToColor(188, 188, 188);
  Result.ChipFill := RGBToColor(246, 246, 246);
  Result.ChipBorder := RGBToColor(222, 222, 222);
  Result.HoverFill := clWhite;
  Result.HoverBorder := RGBToColor(190, 190, 190);
  Result.HoverLine := RGBToColor(170, 170, 170);
  // Tuned for the white face: the hi/lo wash stays faint enough for the grid
  // and the trace to read through it, and the personal range stacks on top
  // as a visibly deeper shade of the same colour.
  Result.BandAlpha := 30;
  Result.RangeAlpha := 44;
  Result.Dark := false;
end;

function DarkHistoryGraphTheme: THistoryGraphTheme;
begin
  // The web dashboard's dark palette, so the PNG sits flush in its card.
  Result.Background := RGBToColor(18, 20, 26);
  Result.Text := RGBToColor(231, 233, 239);
  Result.Muted := RGBToColor(152, 160, 176);
  Result.Grid := RGBToColor(42, 47, 59);
  Result.GridMinor := RGBToColor(31, 35, 45);
  Result.Axis := RGBToColor(70, 76, 92);
  Result.Trace := RGBToColor(100, 106, 122);
  Result.ChipFill := RGBToColor(27, 30, 39);
  Result.ChipBorder := RGBToColor(42, 47, 59);
  Result.HoverFill := RGBToColor(27, 30, 39);
  Result.HoverBorder := RGBToColor(70, 76, 92);
  Result.HoverLine := RGBToColor(120, 126, 142);
  // A wash over a dark face needs more opacity to register at all.
  Result.BandAlpha := 46;
  Result.RangeAlpha := 64;
  Result.Dark := true;
end;

{ THistoryGraphView }

constructor THistoryGraphView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Everything is blitted from the cached bitmap, so the widgetset need not
  // clear the face first - that clear is the flicker on a resize.
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := true;
  FPalette := DefaultHistoryGraphPalette;
  FTheme := LightHistoryGraphTheme;
  FCaptions := DefaultHistoryGraphCaptions;
  Color := FTheme.Background;
  FHovered := -1;
  FRangeMinutes := 0;
  FCgmRangeHi := TrndiAPI.CGM_RANGE_HI_DISABLED;
  FCgmRangeLo := TrndiAPI.CGM_RANGE_LO_DISABLED;
end;

destructor THistoryGraphView.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

// ---------------------------------------------------------------------------
// Small helpers
// ---------------------------------------------------------------------------

function THistoryGraphView.Px(const ASize: integer): integer;
begin
  Result := Scale96ToForm(ASize);
end;

// mg/dL to the display unit
function THistoryGraphView.Conv(const AMgdl: integer): double;
begin
  Result := AMgdl * BG_CONVERTIONS[FUnit][mgdl];
end;

function THistoryGraphView.LevelColor(const Level: BGValLevel): TColor;
begin
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

function THistoryGraphView.FormatValue(const AValue: double): string;
begin
  // The app-wide display rules: one decimal for mmol/L, none for mg/dL.
  Result := Format(BG_MSG_SHORT[FUnit], [AValue]);
end;

function THistoryGraphView.TimeToX(const ATime: TDateTime;
const APlot: TRect): integer;
var
  ratio: double;
begin
  if FInvWindow = 0 then
    Exit((APlot.Left + APlot.Right) div 2);
  ratio := EnsureRange((ATime - FWindowStart) * FInvWindow, 0, 1);
  Result := APlot.Left + Round(ratio * (APlot.Right - APlot.Left));
end;

function THistoryGraphView.XToTime(const AX: integer; const APlot: TRect): TDateTime;
var
  w: integer;
begin
  w := APlot.Right - APlot.Left;
  if (w <= 0) or (FInvWindow = 0) then
    Exit(FWindowStart);
  Result := FWindowStart + ((AX - APlot.Left) / w) * (FWindowEnd - FWindowStart);
end;

function THistoryGraphView.ValueToY(const AValue: double;
const APlot: TRect): integer;
var
  ratio: double;
begin
  if FInvValueSpan = 0 then
    Exit((APlot.Top + APlot.Bottom) div 2);
  ratio := EnsureRange((AValue - FMinValue) * FInvValueSpan, 0, 1);
  Result := APlot.Bottom - Round(ratio * (APlot.Bottom - APlot.Top));
end;

function THistoryGraphView.InWindow(const ATime: TDateTime): boolean;
begin
  Result := (ATime >= FWindowStart) and (ATime <= FWindowEnd);
end;

function THistoryGraphView.HasData: boolean;
begin
  Result := Length(FPoints) > 0;
end;

function THistoryGraphView.GetVisibleCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(FPoints) do
    if InWindow(FPoints[i].Reading.date) then
      Inc(Result);
end;

function THistoryGraphView.GetHasBasal: boolean;
begin
  Result := Length(FBasalProfile) > 0;
end;

function THistoryGraphView.GetHasBolus: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to High(FBoluses) do
    if (FBoluses[i].units > 0) and (not FBoluses[i].automatic) then
      Exit(true);
end;

function THistoryGraphView.GetHasAutoBolus: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to High(FBoluses) do
    if (FBoluses[i].units > 0) and FBoluses[i].automatic then
      Exit(true);
end;

function THistoryGraphView.GetHasCarbs: boolean;
begin
  Result := Length(FCarbs) > 0;
end;

procedure THistoryGraphView.InvalidateBackground;
begin
  FBackgroundValid := false;
end;

procedure THistoryGraphView.ViewChanged;
begin
  InvalidateBackground;
  Invalidate;
  if Assigned(FOnViewChanged) then
    FOnViewChanged(Self);
end;

procedure THistoryGraphView.SortPoints(var APoints: TGraphPoints);
var
  i, j: integer;
  tmp: TGraphPoint;
begin
  // Insertion sort: the readings arrive nearly sorted and number in the
  // hundreds, where this beats a general sort and stays stable.
  for i := 1 to High(APoints) do
  begin
    tmp := APoints[i];
    j := i - 1;
    while (j >= 0) and (APoints[j].Reading.date > tmp.Reading.date) do
    begin
      APoints[j + 1] := APoints[j];
      Dec(j);
    end;
    APoints[j + 1] := tmp;
  end;
end;

// ---------------------------------------------------------------------------
// Data and the time window
// ---------------------------------------------------------------------------

procedure THistoryGraphView.SetReadings(const Readings: BGResults;
UnitPref: BGUnit);
var
  i, n: integer;
begin
  FUnit := UnitPref;
  FHovered := -1;
  SetLength(FPoints, Length(Readings));
  n := 0;
  for i := Low(Readings) to High(Readings) do
  begin
    if Readings[i].empty then
      Continue;
    FPoints[n].Reading := Readings[i];
    FPoints[n].Value := Readings[i].convert(UnitPref);
    Inc(n);
  end;
  SetLength(FPoints, n);
  SortPoints(FPoints);
  // The forecast was converted in the previous unit; redo it in this one.
  for i := 0 to High(FPredictions) do
    FPredictions[i].Value := FPredictions[i].Reading.convert(UnitPref);
  // A custom window belonged to the old data.
  if FRangeMinutes < 0 then
    FRangeMinutes := 0;
  UpdateDataSpan;
  ApplyRange;
end;

procedure THistoryGraphView.SetPredictions(const Predictions: BGResults);
var
  i, n: integer;
begin
  SetLength(FPredictions, Length(Predictions));
  n := 0;
  for i := Low(Predictions) to High(Predictions) do
  begin
    if Predictions[i].empty then
      Continue;
    FPredictions[n].Reading := Predictions[i];
    FPredictions[n].Value := Predictions[i].convert(FUnit);
    Inc(n);
  end;
  SetLength(FPredictions, n);
  SortPoints(FPredictions);
  UpdateDataSpan;
  if FRangeMinutes >= 0 then
    ApplyRange
  else
    ViewChanged;
end;

{ The span the window may roam: from the oldest reading to the wall clock, or
  later when the newest reading or the forecast lies ahead of it. Ending at
  the clock rather than at the data is what makes an outage visible - the
  trace stops short of the right edge instead of being pinned to it. }
procedure THistoryGraphView.UpdateDataSpan;
begin
  if HasData then
  begin
    FDataStart := FPoints[0].Reading.date;
    FDataEnd := Max(Now, FPoints[High(FPoints)].Reading.date);
  end
  else
  begin
    FDataEnd := Now;
    FDataStart := IncHour(FDataEnd, -3);
  end;
  if Length(FPredictions) > 0 then
    FDataEnd := Max(FDataEnd, FPredictions[High(FPredictions)].Reading.date);
  if FDataEnd - FDataStart < MIN_WINDOW_MINUTES / MinsPerDay then
    FDataStart := FDataEnd - MIN_WINDOW_MINUTES / MinsPerDay;
end;

{ A preset is a wall-clock window: "the last 3 hours" ends now, not at the
  newest reading, so it does not slide into the past during an outage. The
  honest answer to a window with nothing in it is an empty plot. }
procedure THistoryGraphView.ApplyRange;
begin
  if FRangeMinutes > 0 then
    SetWindow(FDataEnd - FRangeMinutes / MinsPerDay, FDataEnd)
  else
    SetWindow(FDataStart, FDataEnd);
end;

procedure THistoryGraphView.SetWindow(AStart, AEnd: TDateTime);
var
  span, minSpan: TDateTime;
begin
  minSpan := MIN_WINDOW_MINUTES / MinsPerDay;
  span := AEnd - AStart;
  if span < minSpan then
    span := minSpan;
  if span > FDataEnd - FDataStart then
    span := FDataEnd - FDataStart;
  // Keep the whole window inside the data span.
  if AStart < FDataStart then
    AStart := FDataStart;
  if AStart + span > FDataEnd then
    AStart := FDataEnd - span;
  FWindowStart := AStart;
  FWindowEnd := AStart + span;
  if FWindowEnd - FWindowStart > 0 then
    FInvWindow := 1 / (FWindowEnd - FWindowStart)
  else
    FInvWindow := 0;
  FHovered := -1;
  ViewChanged;
end;

procedure THistoryGraphView.SetRangeMinutes(const AMinutes: integer);
begin
  FRangeMinutes := Max(0, AMinutes);
  UpdateDataSpan;
  ApplyRange;
end;

procedure THistoryGraphView.ZoomBy(const AFactor: double; const AAnchorX: integer);
var
  anchor, span, newSpan, start: TDateTime;
  frac: double;
begin
  if (AFactor <= 0) or (FInvWindow = 0) then
    Exit;
  span := FWindowEnd - FWindowStart;
  newSpan := span / AFactor;
  if (AAnchorX >= 0) and FLayoutValid then
    anchor := XToTime(AAnchorX, FPlot)
  else
    anchor := FWindowStart + span / 2;
  anchor := EnsureRange(anchor, FWindowStart, FWindowEnd);
  // The moment under the pointer stays under the pointer.
  frac := (anchor - FWindowStart) / span;
  start := anchor - frac * newSpan;
  // Zoomed back out to everything: that is the "All" preset again.
  if newSpan >= FDataEnd - FDataStart then
    FRangeMinutes := 0
  else
    FRangeMinutes := -1;
  SetWindow(start, start + newSpan);
end;

procedure THistoryGraphView.PanBy(const AFraction: double);
var
  span, shift: TDateTime;
begin
  if FInvWindow = 0 then
    Exit;
  span := FWindowEnd - FWindowStart;
  if span >= FDataEnd - FDataStart then
    Exit; // Everything is in view; nothing to slide
  shift := span * AFraction;
  FRangeMinutes := -1;
  SetWindow(FWindowStart + shift, FWindowEnd + shift);
end;

procedure THistoryGraphView.ResetView;
begin
  SetRangeMinutes(0);
end;

function THistoryGraphView.VisibleReadings: BGResults;
var
  i, n: integer;
begin
  Result := nil;
  SetLength(Result, Length(FPoints));
  n := 0;
  for i := 0 to High(FPoints) do
    if InWindow(FPoints[i].Reading.date) then
    begin
      Result[n] := FPoints[i].Reading;
      Inc(n);
    end;
  SetLength(Result, n);
end;

// ---------------------------------------------------------------------------
// Setters
// ---------------------------------------------------------------------------

procedure THistoryGraphView.SetThresholds(const cgmHi, cgmLo, cgmRangeHi,
cgmRangeLo: integer);
begin
  FCgmHi := cgmHi;
  FCgmLo := cgmLo;
  FCgmRangeHi := cgmRangeHi;
  FCgmRangeLo := cgmRangeLo;
  InvalidateBackground;
  Invalidate;
end;

procedure THistoryGraphView.SetPalette(const AValue: THistoryGraphPalette);
begin
  FPalette := AValue;
  InvalidateBackground;
  Invalidate;
end;

procedure THistoryGraphView.SetTheme(const AValue: THistoryGraphTheme);
begin
  FTheme := AValue;
  Color := FTheme.Background;
  InvalidateBackground;
  Invalidate;
end;

procedure THistoryGraphView.SetCaptions(const AValue: THistoryGraphCaptions);
begin
  FCaptions := AValue;
  InvalidateBackground;
  Invalidate;
end;

procedure THistoryGraphView.SetBasalProfile(const profile: TBasalProfile;
const maxBasal: single);
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
    // largest dose in view: a fixed ceiling flattens a 0.4 U/hr day into a
    // sliver and draws a 3 and a 6 U/hr rate alike. The headroom keeps the
    // tallest bar off the top of the strip; the floor stops an all-zero
    // profile dividing the height by nothing.
    peak := 0;
    for i := 0 to High(FBasalProfile) do
      if FBasalProfile[i].value > peak then
        peak := FBasalProfile[i].value;
    FMaxBasal := Max(peak * 1.15, 0.1);
  end;
  ViewChanged;
end;

procedure THistoryGraphView.SetBasalOverlayEnabled(aEnabled: boolean);
begin
  FShowBasal := aEnabled;
  ViewChanged;
end;

procedure THistoryGraphView.SetBoluses(const Boluses: TBolusList);
begin
  FBoluses := Copy(Boluses);
  ViewChanged;
end;

procedure THistoryGraphView.SetBolusOverlayEnabled(aEnabled: boolean;
aIncludeAutomatic: boolean);
begin
  FShowBolus := aEnabled;
  FShowAutoBolus := aIncludeAutomatic;
  ViewChanged;
end;

procedure THistoryGraphView.SetCarbs(const Carbs: TCarbList);
begin
  FCarbs := Copy(Carbs);
  ViewChanged;
end;

procedure THistoryGraphView.SetCarbOverlayEnabled(aEnabled: boolean);
begin
  FShowCarbs := aEnabled;
  ViewChanged;
end;

// ---------------------------------------------------------------------------
// Axes
// ---------------------------------------------------------------------------

{ The value axis is settled per render because the tick step depends on how
  many labels the plot height has room for. The range covers the readings in
  the window, the forecast and the hi/lo thresholds, padded by half a mmol/L
  and rounded outward to the tick step, with a floor on the span so a flat
  night is not blown up into a mountain range. }
procedure THistoryGraphView.PrepareValueAxis(const APlotHeight: integer);
const
  STEPS_MMOL: array[0..4] of double = (0.5, 1, 2, 5, 10);
  STEPS_MGDL: array[0..4] of double = (10, 20, 25, 50, 100);
var
  i, n, target: integer;
  lo, hi, margin, minSpan, step, v: double;
  any: boolean;
begin
  lo := MaxDouble;
  hi := -MaxDouble;
  any := false;
  for i := 0 to High(FPoints) do
    if InWindow(FPoints[i].Reading.date) then
    begin
      lo := Min(lo, FPoints[i].Value);
      hi := Max(hi, FPoints[i].Value);
      any := true;
    end;
  for i := 0 to High(FPredictions) do
    if InWindow(FPredictions[i].Reading.date) then
    begin
      lo := Min(lo, FPredictions[i].Value);
      hi := Max(hi, FPredictions[i].Value);
      any := true;
    end;
  if (FCgmLo > 0) and (FCgmHi > FCgmLo) then
  begin
    lo := Min(lo, Conv(FCgmLo));
    hi := Max(hi, Conv(FCgmHi));
    any := true;
  end;
  if not any then
  begin
    lo := 3 * BG_CONVERTIONS[FUnit][mmol];
    hi := 12 * BG_CONVERTIONS[FUnit][mmol];
  end;

  margin := 0.5 * BG_CONVERTIONS[FUnit][mmol];
  minSpan := 6 * BG_CONVERTIONS[FUnit][mmol];
  lo := Max(0, lo - margin);
  hi := hi + margin;
  if hi - lo < minSpan then
    hi := lo + minSpan;

  // The coarsest step that still gives the plot a handful of lines.
  target := Max(2, APlotHeight div Px(44));
  step := 0;
  for i := 0 to 4 do
  begin
    if FUnit = mmol then
      step := STEPS_MMOL[i]
    else
      step := STEPS_MGDL[i];
    if (hi - lo) / step <= target then
      Break;
  end;

  FMinValue := Floor(lo / step) * step;
  FMaxValue := Ceil(hi / step) * step;
  if FMaxValue - FMinValue > 0 then
    FInvValueSpan := 1 / (FMaxValue - FMinValue)
  else
    FInvValueSpan := 0;

  n := Round((FMaxValue - FMinValue) / step);
  SetLength(FValueTicks, n + 1);
  for i := 0 to n do
  begin
    v := FMinValue + i * step;
    FValueTicks[i].Value := v;
    FValueTicks[i].Text := FormatValue(v);
  end;
end;

{ Time ticks fall on round local times - every 5, 10, 15, 30 minutes, every
  hour, 2, 3, 4, 6, 12 hours or every day - whichever coarsest step keeps the
  labels from touching. Midnight ticks carry the day name instead of 00:00. }
procedure THistoryGraphView.PrepareTimeAxis(ACanvas: TCanvas;
const APlotWidth: integer);
const
  STEPS: array[0..11] of integer = (5, 10, 15, 30, 60, 120, 180, 240, 360,
    720, 1440, 2880);
var
  i, n, step, labelW, maxTicks, offset: integer;
  spanMinutes: double;
  dayStart, t: TDateTime;
  daily: boolean;
begin
  SetLength(FTimeTicks, 0);
  if FInvWindow = 0 then
    Exit;
  spanMinutes := (FWindowEnd - FWindowStart) * MinsPerDay;
  labelW := ACanvas.TextWidth('00:00') + Px(18);
  maxTicks := Max(2, APlotWidth div labelW);
  step := STEPS[High(STEPS)];
  for i := 0 to High(STEPS) do
    if spanMinutes / STEPS[i] <= maxTicks then
    begin
      step := STEPS[i];
      Break;
    end;
  daily := step >= MinsPerDay;

  // Walk in whole minutes from the midnight before the window, so the
  // ticks land exactly on the step boundaries whatever the window's start.
  dayStart := Trunc(FWindowStart);
  offset := Ceil((FWindowStart - dayStart) * MinsPerDay / step) * step;
  n := 0;
  SetLength(FTimeTicks, Round(spanMinutes / step) + 2);
  while n < Length(FTimeTicks) do
  begin
    t := dayStart + offset / MinsPerDay;
    if t > FWindowEnd + 1 / SecsPerDay then
      Break;
    FTimeTicks[n].Time := t;
    FTimeTicks[n].DayStart := offset mod MinsPerDay = 0;
    if daily then
      FTimeTicks[n].Text := FormatDateTime('ddd d', t)
    else if FTimeTicks[n].DayStart then
      FTimeTicks[n].Text := FormatDateTime('ddd', t)
    else
      FTimeTicks[n].Text := FormatDateTime('hh:nn', t);
    Inc(n);
    Inc(offset, step);
  end;
  SetLength(FTimeTicks, n);
end;

// ---------------------------------------------------------------------------
// Drawing
// ---------------------------------------------------------------------------

{ Cut a chronological series into runs of connected device points inside the
  window. A gap of ABreakGapMinutes or more between two readings ends a run,
  so missing samples read as a break in the trace, and a segment that crosses
  the window's edge is cut at the edge rather than pinned to it. }
procedure THistoryGraphView.ClipSeries(const ASeries: TGraphPoints;
const APlot: TRect; const ABreakGapMinutes: integer; out ARuns: TPointRuns);
var
  i, runCount, n: integer;
  run: TPointRun;
  aT, bT, aV, bV, f: double;
  lastT: double;
  haveLast: boolean;

  procedure Flush;
  begin
    if n >= 2 then
    begin
      SetLength(ARuns, runCount + 1);
      ARuns[runCount] := Copy(run, 0, n);
      Inc(runCount);
    end;
    n := 0;
    haveLast := false;
  end;

  procedure Push(const T, V: double);
  begin
    if haveLast and SameValue(T, lastT) then
      Exit;
    if n >= Length(run) then
      SetLength(run, n + 16);
    run[n] := Point(TimeToX(T, APlot), ValueToY(V, APlot));
    lastT := T;
    haveLast := true;
    Inc(n);
  end;

begin
  ARuns := nil;
  runCount := 0;
  n := 0;
  haveLast := false;
  SetLength(run, Length(ASeries));
  for i := 1 to High(ASeries) do
  begin
    aT := ASeries[i - 1].Reading.date;
    bT := ASeries[i].Reading.date;
    aV := ASeries[i - 1].Value;
    bV := ASeries[i].Value;
    if (ABreakGapMinutes > 0) and
      ((bT - aT) * MinsPerDay >= ABreakGapMinutes) then
    begin
      Flush;
      Continue;
    end;
    if (bT < FWindowStart) or (aT > FWindowEnd) or (bT <= aT) then
    begin
      Flush;
      Continue;
    end;
    // Clip the segment to the window by interpolation.
    if aT < FWindowStart then
    begin
      f := (FWindowStart - aT) / (bT - aT);
      aV := aV + f * (bV - aV);
      aT := FWindowStart;
    end;
    if bT > FWindowEnd then
    begin
      f := (FWindowEnd - aT) / (bT - aT);
      bV := aV + f * (bV - aV);
      bT := FWindowEnd;
    end;
    Push(aT, aV);
    Push(bT, bV);
  end;
  Flush;
end;

procedure THistoryGraphView.BuildChips(ACanvas: TCanvas;
var AChips: array of TChip; out ACount: integer);
var
  hasRange, hasUnknown: boolean;
  i: integer;

  procedure Add(const AText: string; const AColor: TColor;
    const AHollow: boolean = false);
  begin
    if ACount > High(AChips) then
      Exit;
    AChips[ACount].Text := AText;
    AChips[ACount].Color := AColor;
    AChips[ACount].Hollow := AHollow;
    AChips[ACount].Width := ACanvas.TextWidth(AText) + Px(CHIP_DOT) +
      Px(CHIP_PAD_X) * 2 + Px(6);
    Inc(ACount);
  end;

begin
  ACount := 0;
  // A backend without a personal range reports the disabled sentinels; its
  // chips would read "In range 0.0–27.8". Leave them out, matching the
  // hairlines, which are skipped for the same reason.
  hasRange := (FCgmRangeHi <> TrndiAPI.CGM_RANGE_HI_DISABLED) and
    (FCgmRangeLo <> TrndiAPI.CGM_RANGE_LO_DISABLED) and
    (FCgmRangeHi > FCgmRangeLo);
  if hasRange then
  begin
    Add(Format('%s %s–%s', [FCaptions.KeyRange, FormatValue(Conv(FCgmRangeLo)),
      FormatValue(Conv(FCgmRangeHi))]), LevelColor(BGRange));
    Add(Format('%s ≥ %s', [FCaptions.KeyRangeHigh,
      FormatValue(Conv(FCgmRangeHi))]), LevelColor(BGRangeHI));
    Add(Format('%s ≤ %s', [FCaptions.KeyRangeLow,
      FormatValue(Conv(FCgmRangeLo))]), LevelColor(BGRangeLO));
  end;
  if (FCgmHi > 0) and (FCgmLo > 0) then
  begin
    Add(Format('%s ≥ %s', [FCaptions.KeyHigh, FormatValue(Conv(FCgmHi))]),
      LevelColor(BGHigh));
    Add(Format('%s ≤ %s', [FCaptions.KeyLow, FormatValue(Conv(FCgmLo))]),
      LevelColor(BGLOW));
  end;
  // The treatment keys only earn their space when the overlay is on.
  if FShowBasal and HasBasal then
    Add(Format('%s 0–%s U/h', [FCaptions.KeyBasal,
      FormatFloat('0.0##', FMaxBasal)]), BasalColor);
  if FShowBolus and (Length(FBoluses) > 0) then
  begin
    Add(FCaptions.KeyBolus, BolusColor);
    if FShowAutoBolus then
      Add(FCaptions.KeyBolusAuto, AutoBolusColor);
  end;
  if FShowCarbs and HasCarbs then
    Add(FCaptions.KeyCarbs, CarbColor);
  if Length(FPredictions) > 0 then
    Add(FCaptions.KeyPredict, PredictColor, true);
  // "Unknown" only when a reading in view actually is.
  hasUnknown := false;
  for i := 0 to High(FPoints) do
    if InWindow(FPoints[i].Reading.date) and
      not (FPoints[i].Reading.level in [BGRange, BGRangeHI, BGRangeLO,
      BGHigh, BGLOW]) then
    begin
      hasUnknown := true;
      Break;
    end;
  if hasUnknown then
    Add(FCaptions.KeyUnknown, FPalette.Unknown);
end;

{ Flow the chips into rows across AWidth, drawing them when asked, and
  return the height the rows take. Called twice per render: once to measure,
  so the plot can start below them, and once to draw. }
function THistoryGraphView.LayoutChips(ACanvas: TCanvas;
var AChips: array of TChip; const ACount, AWidth: integer;
const ADraw: boolean): integer;
var
  i, x, y, chipH, gap, rowLeft, rowRight, dot, textH, textY, dotY: integer;
  r: TRect;
begin
  textH := ACanvas.TextHeight('Hg');
  chipH := textH + Px(CHIP_PAD_Y) * 2;
  gap := Px(CHIP_GAP);
  rowLeft := Px(MARGIN_SIDE);
  rowRight := AWidth - Px(MARGIN_SIDE);
  dot := Px(CHIP_DOT);
  x := rowLeft;
  y := Px(MARGIN_TOP);
  Result := 0;
  if ACount = 0 then
    Exit;
  for i := 0 to ACount - 1 do
  begin
    if (x > rowLeft) and (x + AChips[i].Width > rowRight) then
    begin
      x := rowLeft;
      Inc(y, chipH + gap);
    end;
    if ADraw then
    begin
      r := Rect(x, y, x + AChips[i].Width, y + chipH);
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := FTheme.ChipFill;
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Width := 1;
      ACanvas.Pen.Color := FTheme.ChipBorder;
      ACanvas.RoundRect(r, chipH, chipH);
      dotY := y + (chipH - dot) div 2;
      if AChips[i].Hollow then
        DrawSmoothCircle(ACanvas, dot, clNone, AChips[i].Color,
          Max(1, Px(1)), x + Px(CHIP_PAD_X), dotY)
      else
        DrawSmoothCircle(ACanvas, dot, AChips[i].Color, clNone, 0,
          x + Px(CHIP_PAD_X), dotY);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := FTheme.Text;
      textY := y + (chipH - textH) div 2;
      ACanvas.TextOut(x + Px(CHIP_PAD_X) + dot + Px(6), textY, AChips[i].Text);
    end;
    Inc(x, AChips[i].Width + gap);
  end;
  Result := y + chipH - Px(MARGIN_TOP);
end;

procedure THistoryGraphView.DrawThresholdBands(ACanvas: TCanvas;
const APlot: TRect);
const
  EDGE_GRADIENT_PX = 3;
var
  bands: array of TRangeBand;
  plotW, plotH: integer;

  // Band rows are relative to the plot's top edge; DrawRangeBands clips
  // whatever falls outside the plot.
  procedure AddBand(const hiMgdl, loMgdl: integer; const AColor: TColor;
    const AAlpha: byte);
  begin
    SetLength(bands, Length(bands) + 1);
    with bands[High(bands)] do
    begin
      Top := ValueToY(Conv(hiMgdl), APlot) - APlot.Top;
      Bottom := ValueToY(Conv(loMgdl), APlot) - APlot.Top;
      Color := AColor;
      Alpha := AAlpha;
    end;
  end;

begin
  plotW := APlot.Right - APlot.Left;
  plotH := APlot.Bottom - APlot.Top;
  if (plotW <= 0) or (plotH <= 0) then
    Exit;
  bands := nil;
  if (FCgmHi > 0) and (FCgmLo > 0) and (FCgmHi > FCgmLo) then
    AddBand(FCgmHi, FCgmLo, LevelColor(BGRange), FTheme.BandAlpha);
  if (FCgmRangeHi < TrndiAPI.CGM_RANGE_HI_DISABLED) and
    (FCgmRangeLo > TrndiAPI.CGM_RANGE_LO_DISABLED) and
    (FCgmRangeHi > FCgmRangeLo) then
    AddBand(FCgmRangeHi, FCgmRangeLo, LevelColor(BGRange), FTheme.RangeAlpha);
  if Length(bands) > 0 then
    DrawRangeBands(ACanvas, plotW, plotH, bands, Px(EDGE_GRADIENT_PX),
      APlot.Left, APlot.Top);
end;

procedure THistoryGraphView.DrawAxes(ACanvas: TCanvas; const APlot: TRect);
var
  i, x, y, textH: integer;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  textH := ACanvas.TextHeight('Hg');

  // Faint verticals at the time ticks, behind the horizontals.
  ACanvas.Pen.Color := FTheme.GridMinor;
  for i := 0 to High(FTimeTicks) do
  begin
    x := TimeToX(FTimeTicks[i].Time, APlot);
    ACanvas.MoveTo(x, APlot.Top);
    ACanvas.LineTo(x, APlot.Bottom);
  end;

  // Horizontals with the value at the left; the bottom one is the baseline.
  for i := 0 to High(FValueTicks) do
  begin
    y := ValueToY(FValueTicks[i].Value, APlot);
    if i = 0 then
      ACanvas.Pen.Color := FTheme.Axis
    else
      ACanvas.Pen.Color := FTheme.Grid;
    ACanvas.MoveTo(APlot.Left, y);
    ACanvas.LineTo(APlot.Right, y);
    ACanvas.Font.Color := FTheme.Muted;
    ACanvas.TextOut(APlot.Left - Px(AXIS_LABEL_GAP) -
      ACanvas.TextWidth(FValueTicks[i].Text), y - textH div 2,
      FValueTicks[i].Text);
  end;

  // Time labels under the baseline; a day's first tick in the text colour
  // so the day boundaries stand out of the run of times.
  for i := 0 to High(FTimeTicks) do
  begin
    x := TimeToX(FTimeTicks[i].Time, APlot);
    if FTimeTicks[i].DayStart then
      ACanvas.Font.Color := FTheme.Text
    else
      ACanvas.Font.Color := FTheme.Muted;
    ACanvas.TextOut(x - ACanvas.TextWidth(FTimeTicks[i].Text) div 2,
      APlot.Bottom + Px(AXIS_LABEL_GAP), FTimeTicks[i].Text);
  end;
  ACanvas.Font.Color := FTheme.Text;
end;

procedure THistoryGraphView.DrawThresholdLines(ACanvas: TCanvas;
const APlot: TRect);

  procedure Hairline(const valueMgdl: integer; const level: BGValLevel);
  var
    y: integer;
  begin
    y := ValueToY(Conv(valueMgdl), APlot);
    if (y <= APlot.Top) or (y >= APlot.Bottom) then
      Exit;
    ACanvas.Pen.Color := LevelColor(level);
    ACanvas.MoveTo(APlot.Left, y);
    ACanvas.LineTo(APlot.Right, y);
  end;

begin
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
  if (FCgmHi > 0) and (FCgmLo > 0) then
  begin
    Hairline(FCgmHi, BGHigh);
    Hairline(FCgmLo, BGLOW);
  end;
  if FCgmRangeHi <> TrndiAPI.CGM_RANGE_HI_DISABLED then
    Hairline(FCgmRangeHi, BGRangeHI);
  if FCgmRangeLo <> TrndiAPI.CGM_RANGE_LO_DISABLED then
    Hairline(FCgmRangeLo, BGRangeLO);
end;

{ The programmed basal schedule, repeated across every day in view, as a
  strip along the bottom. Temporary rates, suspends and whatever a closed
  loop commanded are not in it, and every day drawn is the profile in force
  now rather than the one in force then. }
procedure THistoryGraphView.DrawBasalOverlay(ACanvas: TCanvas;
const APlot: TRect);
var
  d, j, x1, x2, stripH, h: integer;
  startDT, endDT, s, e: TDateTime;
  endMin: integer;
  rateFrac: double;
begin
  if (not FShowBasal) or (not HasBasal) then
    Exit;
  stripH := Min(Max(Px(24), (APlot.Bottom - APlot.Top) div 8), Px(80));
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := BasalColor;
  ACanvas.Pen.Style := psClear;
  for d := Trunc(FWindowStart) to Trunc(FWindowEnd) do
    for j := 0 to High(FBasalProfile) do
    begin
      if j < High(FBasalProfile) then
        endMin := FBasalProfile[j + 1].startMin
      else
        endMin := MinsPerDay;
      startDT := d + FBasalProfile[j].startMin / MinsPerDay;
      endDT := d + endMin / MinsPerDay;
      if (endDT <= FWindowStart) or (startDT >= FWindowEnd) then
        Continue;
      s := Max(startDT, FWindowStart);
      e := Min(endDT, FWindowEnd);
      x1 := TimeToX(s, APlot);
      x2 := TimeToX(e, APlot);
      rateFrac := EnsureRange(FBasalProfile[j].value / Max(0.001, FMaxBasal), 0, 1);
      h := Round(rateFrac * stripH);
      if (h > 0) and (x2 > x1) then
        ACanvas.FillRect(x1, APlot.Bottom - h, x2, APlot.Bottom);
    end;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
end;

{ Insulin deliveries as stems rising from the bottom axis.

  A delivery the user asked for is a single event worth finding on the chart;
  an automated pump's corrections are a continuous drip of hundredths of a
  unit, and drawing them alike would bury the meal bolus among a hundred
  hairlines. Automatic deliveries are therefore thin, pale and unlabelled,
  and only drawn when the caller asks for them.

  Heights are scaled against the largest delivery in view rather than a fixed
  ceiling, so the overlay stays readable whether the window holds a 12 U meal
  bolus or nothing but 0.05 U corrections - hence the labels. }
procedure THistoryGraphView.DrawBolusOverlay(ACanvas: TCanvas;
const APlot: TRect);
const
  MIN_STEM = 3;   // Keeps the smallest dose from vanishing into the axis
  LABEL_GAP = 4;  // Clear space between two labels before one is dropped
var
  i, x, h, stemH, plotH, minStem, labelGap, autoW, manualW: integer;
  maxUnits: single;
  labelText: string;
  labelW, labelX, labelY, lastLabelRight: integer;

  function Visible(const AEntry: TBolusEntry): boolean;
  begin
    Result := (AEntry.units > 0) and InWindow(AEntry.time) and
      (FShowAutoBolus or (not AEntry.automatic));
  end;

begin
  if (not FShowBolus) or (Length(FBoluses) = 0) then
    Exit;
  plotH := APlot.Bottom - APlot.Top;
  if plotH <= 0 then
    Exit;
  // A third of the plot at most: tall enough to compare doses, short enough
  // to leave the glucose trace, the subject of the chart, unobscured.
  stemH := Min(Max(Px(30), plotH div 5), plotH div 3);
  minStem := Px(MIN_STEM);
  labelGap := Px(LABEL_GAP);
  autoW := Max(1, Px(1));
  manualW := Max(3, Px(3));

  maxUnits := 0;
  for i := 0 to High(FBoluses) do
    if Visible(FBoluses[i]) and (FBoluses[i].units > maxUnits) then
      maxUnits := FBoluses[i].units;
  if maxUnits <= 0 then
    Exit;

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psClear;
  // Automatic first so a manual stem sharing a pixel column stays on top.
  ACanvas.Brush.Color := AutoBolusColor;
  for i := 0 to High(FBoluses) do
  begin
    if (not Visible(FBoluses[i])) or (not FBoluses[i].automatic) then
      Continue;
    x := TimeToX(FBoluses[i].time, APlot);
    h := Max(minStem, Round((FBoluses[i].units / maxUnits) * stemH));
    ACanvas.FillRect(x, APlot.Bottom - h, x + autoW, APlot.Bottom);
  end;
  ACanvas.Brush.Color := BolusColor;
  for i := 0 to High(FBoluses) do
  begin
    if (not Visible(FBoluses[i])) or FBoluses[i].automatic then
      Continue;
    x := TimeToX(FBoluses[i].time, APlot);
    h := Max(minStem, Round((FBoluses[i].units / maxUnits) * stemH));
    ACanvas.FillRect(x - manualW div 2, APlot.Bottom - h,
      x - manualW div 2 + manualW, APlot.Bottom);
  end;

  // Labels last, in a second pass, so no stem can be drawn over one. A label
  // that would overprint the one before it is dropped; the stem still shows.
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := BolusColor;
  lastLabelRight := Low(integer);
  for i := 0 to High(FBoluses) do
  begin
    if (not Visible(FBoluses[i])) or FBoluses[i].automatic then
      Continue;
    labelText := Format('%.2fU', [FBoluses[i].units]);
    labelW := ACanvas.TextWidth(labelText);
    x := TimeToX(FBoluses[i].time, APlot);
    labelX := x - labelW div 2;
    if labelX < lastLabelRight + labelGap then
      Continue;
    lastLabelRight := labelX + labelW;
    h := Max(minStem, Round((FBoluses[i].units / maxUnits) * stemH));
    labelY := Max(APlot.Top, APlot.Bottom - h - ACanvas.TextHeight(labelText) - Px(2));
    ACanvas.TextOut(labelX, labelY, labelText);
  end;
  ACanvas.Font.Color := FTheme.Text;
end;

{ Carbohydrates as discs in a fixed lane just above the bottom axis. Grams
  and units are different quantities and must not share a scale, so carbs
  get a disc at constant height whose radius grows a little with the amount;
  the number on the label is what counts. The lane sits over the base of the
  insulin stems on purpose: a meal and the bolus that covered it happen at
  the same moment, and the overlap is what shows they belong together. }
procedure THistoryGraphView.DrawCarbOverlay(ACanvas: TCanvas;
const APlot: TRect);
const
  LANE_HEIGHT = 12;
  MIN_RADIUS = 4;
  MAX_RADIUS = 9;
  LABEL_GAP = 4;
var
  i, x, y, radius, minR, maxR, labelGap: integer;
  maxGrams: single;
  labelText: string;
  labelW, labelX, labelY, lastLabelRight: integer;

  function Visible(const AEntry: TCarbEntry): boolean;
  begin
    Result := (AEntry.grams > 0) and InWindow(AEntry.time);
  end;

  function RadiusFor(const AGrams: single): integer;
  begin
    Result := minR + Round((AGrams / maxGrams) * (maxR - minR));
  end;

begin
  if (not FShowCarbs) or (not HasCarbs) then
    Exit;
  maxGrams := 0;
  for i := 0 to High(FCarbs) do
    if Visible(FCarbs[i]) and (FCarbs[i].grams > maxGrams) then
      maxGrams := FCarbs[i].grams;
  if maxGrams <= 0 then
    Exit;
  y := APlot.Bottom - Px(LANE_HEIGHT);
  minR := Px(MIN_RADIUS);
  maxR := Px(MAX_RADIUS);
  labelGap := Px(LABEL_GAP);

  for i := 0 to High(FCarbs) do
  begin
    if not Visible(FCarbs[i]) then
      Continue;
    x := TimeToX(FCarbs[i].time, APlot);
    radius := RadiusFor(FCarbs[i].grams);
    // Rimmed in the face colour so the disc stays legible over a stem.
    DrawSmoothCircle(ACanvas, 2 * radius, CarbColor, FTheme.Background,
      Max(1, Px(1)), x - radius, y - radius);
  end;

  // Labels beside the discs, in a second pass so no disc is drawn over one.
  // Beside rather than above: above is where the bolus stem and its label
  // stand, since a meal and its bolus share the moment.
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := CarbColor;
  lastLabelRight := Low(integer);
  for i := 0 to High(FCarbs) do
  begin
    if not Visible(FCarbs[i]) then
      Continue;
    labelText := Format('%.0fg', [FCarbs[i].grams]);
    labelW := ACanvas.TextWidth(labelText);
    x := TimeToX(FCarbs[i].time, APlot);
    radius := RadiusFor(FCarbs[i].grams);
    labelX := x + radius + Px(3);
    if labelX < lastLabelRight + labelGap then
      Continue;
    lastLabelRight := labelX + labelW;
    labelY := y - ACanvas.TextHeight(labelText) div 2;
    ACanvas.TextOut(labelX, labelY, labelText);
  end;
  ACanvas.Font.Color := FTheme.Text;
end;

procedure THistoryGraphView.DrawTrace(ACanvas: TCanvas; const APlot: TRect);
var
  runs: TPointRuns;
  colors: array of TColor;
  i, k: integer;
begin
  if Length(FPoints) < 2 then
    Exit;
  colors := nil;
  ClipSeries(FPoints, APlot, INTERVAL_MINUTES * 2, runs);
  for i := 0 to High(runs) do
  begin
    SetLength(colors, Length(runs[i]));
    for k := 0 to High(colors) do
      colors[k] := FTheme.Trace;
    // Drawn into the cached background, so it must not take over the single
    // polyline cache slot the main window's live trend line relies on.
    DrawSmoothPolyline(ACanvas, runs[i], colors, Max(1, Px(TRACE_WIDTH)), false);
  end;
end;

procedure THistoryGraphView.DrawDots(ACanvas: TCanvas; const APlot: TRect);
var
  i, x, y, r: integer;
begin
  r := FDotRadius;
  for i := 0 to High(FPoints) do
  begin
    if not InWindow(FPoints[i].Reading.date) then
      Continue;
    x := TimeToX(FPoints[i].Reading.date, APlot);
    y := ValueToY(FPoints[i].Value, APlot);
    DrawSmoothCircle(ACanvas, 2 * r, LevelColor(FPoints[i].Reading.level),
      clNone, 0, x - r, y - r);
  end;
end;

procedure THistoryGraphView.DrawPredictions(ACanvas: TCanvas;
const APlot: TRect);
const
  DASH_PX = 6;
  GAP_PX = 4;
var
  series: TGraphPoints;
  runs: TPointRuns;
  i, x, y, r: integer;
begin
  if Length(FPredictions) = 0 then
    Exit;
  // Anchored on the last real reading, so the forecast visibly continues the
  // trace rather than floating beside it.
  series := nil;
  if HasData then
  begin
    SetLength(series, Length(FPredictions) + 1);
    series[0] := FPoints[High(FPoints)];
    for i := 0 to High(FPredictions) do
      series[i + 1] := FPredictions[i];
  end
  else
    series := Copy(FPredictions);
  ClipSeries(series, APlot, 0, runs);
  for i := 0 to High(runs) do
    DrawSmoothDashedPolyline(ACanvas, runs[i], PredictColor, Max(1, Px(1)),
      Px(DASH_PX), Px(GAP_PX));

  r := Max(2, FDotRadius - 1);
  for i := 0 to High(FPredictions) do
  begin
    if not InWindow(FPredictions[i].Reading.date) then
      Continue;
    x := TimeToX(FPredictions[i].Reading.date, APlot);
    y := ValueToY(FPredictions[i].Value, APlot);
    DrawSmoothCircle(ACanvas, 2 * r, clNone, PredictColor, Max(1, Px(1)),
      x - r, y - r);
  end;
end;

{ Reading count and the span in view, small and muted under the time
  labels, flush right. }
procedure THistoryGraphView.DrawStatus(ACanvas: TCanvas; const AWidth,
AHeight: integer);
var
  first, last: TDateTime;
  i, n: integer;
  s, lastText: string;
begin
  n := 0;
  first := 0;
  last := 0;
  for i := 0 to High(FPoints) do
    if InWindow(FPoints[i].Reading.date) then
    begin
      if n = 0 then
        first := FPoints[i].Reading.date;
      last := FPoints[i].Reading.date;
      Inc(n);
    end;
  s := Format(FCaptions.ReadingCount, [n]);
  if n > 0 then
  begin
    if DateOf(first) = DateOf(last) then
      lastText := FormatDateTime('hh:nn', last)
    else
      lastText := FormatDateTime('ddd d mmm hh:nn', last);
    s := s + HOVER_SEP + FormatDateTime('ddd d mmm hh:nn', first) + ' – ' + lastText;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := FTheme.Muted;
  ACanvas.TextOut(AWidth - Px(MARGIN_SIDE) - ACanvas.TextWidth(s),
    AHeight - Px(MARGIN_TOP) - ACanvas.TextHeight(s), s);
  ACanvas.Font.Color := FTheme.Text;
end;

procedure THistoryGraphView.DrawEmpty(ACanvas: TCanvas; const AWidth,
AHeight: integer);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := FTheme.Muted;
  ACanvas.TextOut((AWidth - ACanvas.TextWidth(FCaptions.Empty)) div 2,
    (AHeight - ACanvas.TextHeight(FCaptions.Empty)) div 2, FCaptions.Empty);
end;

{ The static plot at a given size. Paint calls this into the cache; the
  export and the web API call it into a bitmap of their own. The layout it
  settles on (plot rectangle, axis ranges, dot radius) is what hit testing
  uses, so the last call must always be for the control's own size - the
  off-screen renders use a separate instance. }
procedure THistoryGraphView.RenderTo(ACanvas: TCanvas; const AWidth,
AHeight: integer);
var
  chips: array[0..15] of TChip;
  chipCount, chipsH, labelW, textH, i, spacing: integer;
  plot: TRect;
begin
  // The owner form's font is the one the LCL rescales with the DPI; a fresh
  // bitmap canvas would otherwise label a high-DPI plot in the 96 dpi
  // default. TFont.Assign copies the point size, not the pixel height, when
  // the two fonts disagree on DPI, so align the DPI first.
  ACanvas.Font.PixelsPerInch := Font.PixelsPerInch;
  ACanvas.Font.Assign(Font);
  ACanvas.Font.Style := [];
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FTheme.Background;
  ACanvas.FillRect(Rect(0, 0, AWidth, AHeight));

  if not HasData then
  begin
    FLayoutValid := false;
    DrawEmpty(ACanvas, AWidth, AHeight);
    Exit;
  end;

  textH := ACanvas.TextHeight('Hg');
  for i := 0 to High(chips) do
    chips[i].Text := '';
  BuildChips(ACanvas, chips, chipCount);
  chipsH := LayoutChips(ACanvas, chips, chipCount, AWidth, false);
  // A small render (the dashboard's phone-width card) cannot spare a third
  // of its height for the key; the plot is the point, so the key goes.
  if chipsH > AHeight div 3 then
  begin
    chipCount := 0;
    chipsH := 0;
  end;

  // Vertical extent first: the value ticks depend on the height alone, and
  // the left margin then follows from the widest tick label.
  plot.Top := Px(MARGIN_TOP) + chipsH + Px(14);
  plot.Bottom := AHeight - Px(MARGIN_TOP) - textH - Px(4) - textH - Px(AXIS_LABEL_GAP);
  if plot.Bottom < plot.Top + Px(40) then
    plot.Bottom := plot.Top + Px(40);
  PrepareValueAxis(plot.Bottom - plot.Top);
  labelW := 0;
  for i := 0 to High(FValueTicks) do
    labelW := Max(labelW, ACanvas.TextWidth(FValueTicks[i].Text));
  plot.Left := Px(MARGIN_SIDE) + labelW + Px(AXIS_LABEL_GAP);
  plot.Right := AWidth - Px(MARGIN_SIDE) - ACanvas.TextWidth('00:00') div 2;
  if plot.Right < plot.Left + Px(40) then
    plot.Right := plot.Left + Px(40);
  PrepareTimeAxis(ACanvas, plot.Right - plot.Left);

  // Dots sized to the cadence: far enough apart to stand alone over a few
  // hours, merging into a band over a day rather than piling into a blob.
  if FInvWindow > 0 then
    spacing := Round((plot.Right - plot.Left) * INTERVAL_MINUTES /
      ((FWindowEnd - FWindowStart) * MinsPerDay))
  else
    spacing := Px(DOT_MAX);
  FDotRadius := EnsureRange(Round(spacing * 0.42), Px(DOT_MIN), Px(DOT_MAX));
  FPlot := plot;
  FLayoutValid := true;

  DrawThresholdBands(ACanvas, plot);
  DrawAxes(ACanvas, plot);
  DrawThresholdLines(ACanvas, plot);
  DrawBasalOverlay(ACanvas, plot);
  DrawBolusOverlay(ACanvas, plot);
  DrawCarbOverlay(ACanvas, plot);
  DrawTrace(ACanvas, plot);
  DrawDots(ACanvas, plot);
  DrawPredictions(ACanvas, plot);
  if chipCount > 0 then
    LayoutChips(ACanvas, chips, chipCount, AWidth, true);
  DrawStatus(ACanvas, AWidth, AHeight);
end;

procedure THistoryGraphView.RenderToStream(AStream: TStream; const AWidth,
AHeight: integer);
var
  bmp: TBitmap;
  intfImg: TLazIntfImage;
  writer: TFPWriterPNG;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(Max(1, AWidth), Max(1, AHeight));
    RenderTo(bmp.Canvas, bmp.Width, bmp.Height);
    // The render settled the hit-testing layout for the export's size; redo
    // the live one so a save from the open window does not leave the hover
    // geometry sized for the file.
    if (FBackground <> nil) and (FBackground.Width = ClientWidth) and
      (FBackground.Height = ClientHeight) then
    begin
      RenderTo(FBackground.Canvas, FBackground.Width, FBackground.Height);
      FBackgroundValid := true;
    end
    else
    begin
      FLayoutValid := false;
      InvalidateBackground;
    end;
    intfImg := TLazIntfImage.Create(0, 0);
    try
      intfImg.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);
      writer := TFPWriterPNG.Create;
      try
        writer.Indexed := false;
        writer.WordSized := false;
        writer.UseAlpha := false;
        intfImg.SaveToStream(AStream, writer);
      finally
        writer.Free;
      end;
    finally
      intfImg.Free;
    end;
  finally
    bmp.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Hover
// ---------------------------------------------------------------------------

procedure THistoryGraphView.HoverTexts(const AIndex: integer; out AValueText,
ADetailText: string);
var
  delta: double;
  sign: string;
  prev: integer;
begin
  AValueText := Format(BG_MSG_DEF[FUnit], [FPoints[AIndex].Value]);
  ADetailText := FormatDateTime('ddd hh:nn', FPoints[AIndex].Reading.date);
  // The reading's own delta when the backend supplied one, otherwise the
  // difference to the previous reading.
  if not FPoints[AIndex].Reading.deltaEmpty then
    ADetailText := ADetailText + HOVER_SEP +
      FPoints[AIndex].Reading.format(FUnit, BG_MSG_SIG_SHORT, BGDelta)
  else
  begin
    prev := AIndex - 1;
    if prev >= 0 then
    begin
      delta := FPoints[AIndex].Value - FPoints[prev].Value;
      if delta > 0 then
        sign := '+'
      else if delta < 0 then
        sign := ''
      else
        sign := '±';
      ADetailText := ADetailText + HOVER_SEP + Format(StringReplace(
        BG_MSG_SIG_SHORT[FUnit], '%+', sign, [rfReplaceAll]), [delta]);
    end;
  end;
  // The two non-directional trends ('?' and the placeholder) tell the reader
  // nothing and look like a rendering bug, so they are left out.
  if FPoints[AIndex].Reading.trend in [TdDoubleUp..TdDoubleDown] then
    ADetailText := ADetailText + HOVER_SEP + FPoints[AIndex].Reading.trend.Img;
end;

procedure THistoryGraphView.DrawHoverOverlay(ACanvas: TCanvas;
const APlot: TRect);
var
  dotX, dotY, pad, lineGap, sideGap, valueH, detailH, boxW, boxH, ring: integer;
  valueText, detailText: string;
  box: TRect;
  hairline: array[0..0] of TSmoothStroke;

  procedure ShiftRect(var R: TRect; const DX, DY: integer);
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
  end;

begin
  if (FHovered < 0) or (FHovered > High(FPoints)) or (not FLayoutValid) then
    Exit;
  dotX := TimeToX(FPoints[FHovered].Reading.date, APlot);
  dotY := ValueToY(FPoints[FHovered].Value, APlot);

  // The hairline goes first so the ring and the box sit on top of it. A
  // capsule stroke on integer coordinates covers exactly one column.
  hairline[0].X1 := dotX;
  hairline[0].Y1 := APlot.Top;
  hairline[0].X2 := dotX;
  hairline[0].Y2 := APlot.Bottom;
  hairline[0].Color := FTheme.HoverLine;
  DrawSmoothStrokes(ACanvas, hairline, Max(1, Px(1)));
  // A hollow ring, so the dot it circles stays visible inside.
  ring := FDotRadius + Px(3);
  DrawSmoothCircle(ACanvas, 2 * ring, clNone,
    LevelColor(FPoints[FHovered].Reading.level), Max(2, Px(2)),
    dotX - ring, dotY - ring);

  HoverTexts(FHovered, valueText, detailText);
  pad := Px(8);
  lineGap := Px(2);
  sideGap := Px(12);
  ACanvas.Font.Style := [fsBold];
  valueH := ACanvas.TextHeight(valueText);
  boxW := ACanvas.TextWidth(valueText);
  ACanvas.Font.Style := [];
  detailH := ACanvas.TextHeight(detailText);
  boxW := Max(boxW, ACanvas.TextWidth(detailText)) + 2 * pad;
  boxH := valueH + lineGap + detailH + 2 * pad;

  // Above and to the right of the dot; flipped to the left near the right
  // edge and clamped into the plot vertically, so the box never covers the
  // reading it describes and never leaves the plot.
  box := Rect(dotX + sideGap, dotY - boxH - Px(4), dotX + sideGap + boxW,
    dotY - Px(4));
  if box.Right > APlot.Right then
    ShiftRect(box, -(boxW + 2 * sideGap), 0);
  if box.Left < APlot.Left then
    ShiftRect(box, (APlot.Left - box.Left) + Px(4), 0);
  if box.Top < APlot.Top then
    ShiftRect(box, 0, (APlot.Top - box.Top) + Px(4));
  if box.Bottom > APlot.Bottom then
    ShiftRect(box, 0, (APlot.Bottom - box.Bottom) - Px(4));

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FTheme.HoverFill;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := FTheme.HoverBorder;
  ACanvas.RoundRect(box, Px(8), Px(8));
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := LevelColor(FPoints[FHovered].Reading.level);
  ACanvas.TextOut(box.Left + pad, box.Top + pad, valueText);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := FTheme.Text;
  ACanvas.TextOut(box.Left + pad, box.Top + pad + valueH + lineGap, detailText);
end;

function THistoryGraphView.PointAt(const X, Y: integer): integer;
var
  i, dotX, dotY, thresholdSq: integer;
begin
  Result := -1;
  if (not HasData) or (not FLayoutValid) then
    Exit;
  thresholdSq := Sqr(FDotRadius + Px(4));
  for i := 0 to High(FPoints) do
  begin
    if not InWindow(FPoints[i].Reading.date) then
      Continue;
    dotX := TimeToX(FPoints[i].Reading.date, FPlot);
    dotY := ValueToY(FPoints[i].Value, FPlot);
    if Sqr(dotX - X) + Sqr(dotY - Y) <= thresholdSq then
      Exit(i);
  end;
end;

{ The reading under a vertical line through the pointer, anywhere in the
  plot (with a little slack around it), so a sweep across the plot follows
  the trace without needing a hit on a dot. }
function THistoryGraphView.NearestPointAt(const X, Y: integer): integer;
var
  i, dist, bestDist, slack: integer;
begin
  Result := -1;
  if (not HasData) or (not FLayoutValid) then
    Exit;
  slack := FDotRadius + Px(4);
  if (X < FPlot.Left - slack) or (X > FPlot.Right + slack) or
    (Y < FPlot.Top - slack) or (Y > FPlot.Bottom + slack) then
    Exit;
  bestDist := MaxInt;
  for i := 0 to High(FPoints) do
  begin
    if not InWindow(FPoints[i].Reading.date) then
      Continue;
    dist := Abs(TimeToX(FPoints[i].Reading.date, FPlot) - X);
    if dist < bestDist then
    begin
      bestDist := dist;
      Result := i;
    end;
  end;
end;

procedure THistoryGraphView.SetHovered(const AIndex: integer);
begin
  if AIndex = FHovered then
    Exit;
  FHovered := AIndex;
  Invalidate;
end;

// ---------------------------------------------------------------------------
// LCL overrides
// ---------------------------------------------------------------------------

procedure THistoryGraphView.Paint;
begin
  inherited Paint;
  if FBackground = nil then
    FBackground := TBitmap.Create;
  if (FBackground.Width <> ClientWidth) or (FBackground.Height <> ClientHeight) then
  begin
    FBackground.SetSize(Max(1, ClientWidth), Max(1, ClientHeight));
    FBackgroundValid := false;
  end;
  // The static layers only change with the data, the window or the size;
  // cache them and draw the hover layer on top so MouseMove repaints stay cheap.
  if not FBackgroundValid then
  begin
    RenderTo(FBackground.Canvas, FBackground.Width, FBackground.Height);
    FBackgroundValid := true;
  end;
  Canvas.Draw(0, 0, FBackground);
  if HasData then
  begin
    Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
    Canvas.Font.Assign(Font);
    DrawHoverOverlay(Canvas, FPlot);
  end;
end;

procedure THistoryGraphView.Resize;
begin
  inherited Resize;
  InvalidateBackground;
  Invalidate;
end;

procedure THistoryGraphView.DoAutoAdjustLayout(
const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  // A DPI change (the window dragged to another monitor) changes what Px
  // returns, so the cached layers are stale even at the same size.
  if AMode = lapAutoAdjustForDPI then
  begin
    InvalidateBackground;
    Invalidate;
  end;
end;

procedure THistoryGraphView.MouseDown(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if CanFocus and (not Focused) then
    SetFocus;
  if (Button = mbLeft) and HasData then
  begin
    // A press starts a possible drag; MouseUp decides whether it was a
    // click on a dot instead.
    FDragging := true;
    FDragMoved := false;
    FDragX := X;
    FDragWindowStart := FWindowStart;
  end;
end;

procedure THistoryGraphView.MouseMove(Shift: TShiftState; X, Y: integer);
var
  dx, plotW: integer;
  slide: TDateTime;
begin
  inherited MouseMove(Shift, X, Y);
  if not HasData then
    Exit;
  if FDragging and FLayoutValid then
  begin
    dx := X - FDragX;
    if (not FDragMoved) and (Abs(dx) < Px(4)) then
      Exit;
    FDragMoved := true;
    plotW := FPlot.Right - FPlot.Left;
    if (plotW <= 0) or (FWindowEnd - FWindowStart >= FDataEnd - FDataStart) then
      Exit;
    Cursor := crSizeWE;
    // Dragging right pulls earlier readings into view.
    slide := -(dx / plotW) * (FWindowEnd - FWindowStart);
    FRangeMinutes := -1;
    SetWindow(FDragWindowStart + slide, FDragWindowStart + slide +
      (FWindowEnd - FWindowStart));
    Exit;
  end;
  SetHovered(NearestPointAt(X, Y));
end;

procedure THistoryGraphView.MouseUp(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
var
  idx: integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button <> mbLeft then
    Exit;
  if FDragging and (not FDragMoved) then
  begin
    idx := PointAt(X, Y);
    if (idx >= 0) and Assigned(FOnReadingClick) then
      FOnReadingClick(Self, FPoints[idx].Reading);
  end;
  FDragging := false;
  FDragMoved := false;
  Cursor := crDefault;
end;

procedure THistoryGraphView.MouseLeave;
begin
  inherited MouseLeave;
  // Without this the ring and box stay behind when the pointer leaves the
  // control without passing a spot NearestPointAt rejects.
  SetHovered(-1);
end;

function THistoryGraphView.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
MousePos: TPoint): boolean;
var
  steps: integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result or (not HasData) then
    Exit;
  // Gather sub-notch deltas (smooth-scrolling touchpads) into whole notches.
  Inc(FWheelRemainder, WheelDelta);
  steps := FWheelRemainder div WHEEL_NOTCH;
  FWheelRemainder := FWheelRemainder - steps * WHEEL_NOTCH;
  if steps = 0 then
    Exit(true);
  // Horizontal scrolling (shift held) slides; vertical zooms about the pointer.
  if ssShift in Shift then
    PanBy(-steps * 0.1)
  else
    ZoomBy(Power(ZOOM_STEP, steps), MousePos.X);
  if FLayoutValid then
    SetHovered(NearestPointAt(MousePos.X, MousePos.Y));
  Result := true;
end;

procedure THistoryGraphView.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Shift <> []) and (Shift <> [ssShift]) then
    Exit;
  case Key of
  VK_ADD, VK_OEM_PLUS:
    ZoomBy(ZOOM_STEP);
  VK_SUBTRACT, VK_OEM_MINUS:
    ZoomBy(1 / ZOOM_STEP);
  VK_LEFT:
    PanBy(-0.2);
  VK_RIGHT:
    PanBy(0.2);
  VK_HOME, VK_0, VK_NUMPAD0:
    ResetView;
  else
    Exit;
  end;
  Key := 0;
end;

end.
