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
 * - 2026-10-07: The plot moved out to THistoryGraphView (uhistorygraphview);
 *   this unit is now the window around it: a toolbar with the time-range
 *   presets, the overlay toggles and the export buttons, the context menu,
 *   the reading-details popup and the save dialogs. The window follows the
 *   system's light or dark appearance, and SetDarkTheme lets the web API
 *   pick the theme for its PNG. The help banner, its SetShowHelpPanel and
 *   the "Time"/"Readings" axis titles are gone; the toolbar and the hover
 *   box carry what they said.
 * - 2026-10-07: Added SetShowHelpPanel so a static render (the web API's
 *   /history.png) can drop the hover/right-click hint and give the plot
 *   the height the banner took.
 * - 2026-10-06: Added RenderToStream, the PNG render behind SaveAsPNG and
 *   the web API's /history.png, and SetRangeMinutes so a caller can pick
 *   the time window the context menu offers.
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
  uhistorygraph - The history window.

  TfHistoryGraph hosts a THistoryGraphView (uhistorygraphview, where all
  the plotting lives) under a toolbar: the time-range presets, toggles for
  the treatment overlays that have data, and the PNG/CSV export buttons.
  A right-click menu repeats the ranges and the exports and adds "Reset
  zoom"; a click on a dot opens the reading-details popup. The window is
  reused between openings through ShowHistoryGraph.

  The public Set* methods forward to the view so the main window and the
  web API (which renders a never-shown instance through RenderToStream)
  need not know about the split.
}
unit uhistorygraph;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
Buttons, ExtDlgs, trndi.types, trndi.api, trndi.strings, slicke.ux.alert,
trndi.native, uhistorygraphview;

type
  {** The level palette the view takes; re-exported so callers keep using
      this unit's name for it. }
THistoryGraphPalette = uhistorygraphview.THistoryGraphPalette;

  {** TfHistoryGraph
      The history window: a toolbar over a THistoryGraphView. }
TfHistoryGraph = class(TForm)
private
  FView: THistoryGraphView;
  FToolbar: TPanel;
  FRangeButtons: array of TSpeedButton;
  FBasalButton: TSpeedButton;
  FBolusButton: TSpeedButton;
  FCarbButton: TSpeedButton;
  FPngButton: TSpeedButton;
  FCsvButton: TSpeedButton;
  FPopupMenu: TPopupMenu;
  FRangeMenu: TMenuItem;
  FDark: boolean;
  function NewToolButton(const ACaption, AHint: string;
    const AGroup: integer): TSpeedButton;
  procedure LayoutToolbar;
  procedure SyncToolbar;
  procedure ApplyTheme;
  procedure HandleRangeButton(Sender: TObject);
  procedure HandleRangeMenuClick(Sender: TObject);
  procedure HandleOverlayButton(Sender: TObject);
  procedure HandleResetZoom({%H-}Sender: TObject);
  procedure HandleViewChanged({%H-}Sender: TObject);
  procedure HandleReadingClick({%H-}Sender: TObject; const Reading: BGReading);
  procedure ShowReadingDetails(const Reading: BGReading);
  function Px(const ASize: integer): integer;
protected
  procedure Resize; override;
  procedure KeyDown(var Key: word; Shift: TShiftState); override;
  procedure DoClose(var CloseAction: TCloseAction); override;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
    {** SetReadings: Populate the graph with an array of BGReadings.
      Empty readings are dropped and values converted to the requested
      unit before the plot is redrawn.
      @param(Readings Array of BGReading objects to draw)
      @param(UnitPref Preferred output unit for formatting/drawing) }
  procedure SetReadings(const Readings: BGResults; UnitPref: BGUnit);
    {** SetPalette: Inject the palette used for the level colours so the
      graph matches the main UI. }
  procedure SetPalette(const Palette: THistoryGraphPalette);
    {** SetThresholds: Inject the CGM thresholds (in mg/dL) that place the
      bands, the hairlines and the legend. }
  procedure SetThresholds(const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
    {** SetDarkTheme: Draw the plot on the dark theme (true) or the light
      one. A new window picks the system appearance itself; the web API
      sets it from the browser's. }
  procedure SetDarkTheme(const ADark: boolean);
    {** SaveAsPNG: Export the plot as it stands to a PNG file through a
      save dialog. }
  procedure SaveAsPNG({%H-}Sender: TObject);
    {** RenderToStream: Encode the plot - the same static render the window
      shows, minus the hover overlay - as a PNG of the form's client size
      into @code(AStream). Needs no window handle, so a form that has never
      been shown renders too; size it with ClientWidth and ClientHeight
      first. }
  procedure RenderToStream(AStream: TStream);
    {** SetRangeMinutes: Limit the plot to the last @code(AMinutes) of data,
      counted back from now; 0 shows everything. The same presets the
      toolbar and the context menu offer, and reflected there. }
  procedure SetRangeMinutes(const AMinutes: integer);
    {** SaveAsCSV: Export the readings in view to a CSV file for analysis in
      spreadsheet applications. }
  procedure SaveAsCSV({%H-}Sender: TObject);
    {** SetBasalProfile: Provide a repeating daily basal profile to draw on
      the graph. @param(maxBasal) is the rate the strip's full height stands
      for; 0 (the default) takes the profile's own highest rate. }
  procedure SetBasalProfile(const profile: TBasalProfile; const maxBasal: single = 0);
    {** Enable or disable basal overlay rendering. }
  procedure SetBasalOverlayEnabled(aEnabled: boolean);
    {** SetBoluses: Provide the insulin deliveries to draw as stems along the
      bottom axis. Pass an empty array to hide the overlay. }
  procedure SetBoluses(const Boluses: TBolusList);
    {** Enable or disable bolus overlay rendering.
      @param(aEnabled Draw the overlay at all)
      @param(aIncludeAutomatic Also draw pump-initiated micro-deliveries,
        off by default since they crowd out the deliveries the user asked for) }
  procedure SetBolusOverlayEnabled(aEnabled: boolean;
    aIncludeAutomatic: boolean = false);
    {** SetCarbs: Provide the carbohydrate entries to draw along the bottom
      axis. Pass an empty array to hide the overlay. }
  procedure SetCarbs(const Carbs: TCarbList);
    {** Enable or disable carbohydrate overlay rendering. }
  procedure SetCarbOverlayEnabled(aEnabled: boolean);
    {** SetPredictions: Supply predicted future readings to overlay on the
      graph as a dashed continuation past the last real reading. Pass an
      empty array to hide the overlay. }
  procedure SetPredictions(const Predictions: BGResults);
    {** The plot itself, for a host that wants to drive it directly. }
  property View: THistoryGraphView read FView;
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
LCLType;

resourcestring
RS_HISTORY_GRAPH_TITLE = 'History graph';
RS_HISTORY_GRAPH_EMPTY = 'No history data to plot';
RS_HISTORY_GRAPH_POINT_COUNT = '%d readings';
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
RS_HISTORY_GRAPH_KEY_PREDICT = 'Predicted';
RS_HISTORY_GRAPH_SAVE_TITLE = 'Save graph as PNG';
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
RS_HISTORY_GRAPH_MENU_RESET = 'Reset zoom';
RS_HISTORY_GRAPH_TB_RANGE_HINT = 'Show the last %s';
RS_HISTORY_GRAPH_TB_ALL_HINT = 'Show everything';
RS_HISTORY_GRAPH_TB_BASAL = 'Basal';
RS_HISTORY_GRAPH_TB_BASAL_HINT = 'Show the programmed basal schedule';
RS_HISTORY_GRAPH_TB_BOLUS = 'Insulin';
RS_HISTORY_GRAPH_TB_BOLUS_HINT = 'Show insulin deliveries';
RS_HISTORY_GRAPH_TB_CARBS = 'Carbs';
RS_HISTORY_GRAPH_TB_CARBS_HINT = 'Show carbohydrates';
RS_HISTORY_GRAPH_TB_PNG = 'PNG';
RS_HISTORY_GRAPH_TB_PNG_HINT = 'Save the graph as an image (Ctrl+S)';
RS_HISTORY_GRAPH_TB_CSV = 'CSV';
RS_HISTORY_GRAPH_TB_CSV_HINT = 'Save the readings in view as CSV';

const
  // 96 dpi design values for the toolbar; the LCL scales the form itself.
TOOLBAR_HEIGHT = 38;
TOOLBAR_PAD = 8;
BUTTON_HEIGHT = 26;
BUTTON_GAP = 2;
GROUP_GAP = 14;
RANGE_BUTTON_WIDTH = 40;
OVERLAY_BUTTON_WIDTH = 62;
EXPORT_BUTTON_WIDTH = 46;

  // The presets, in minutes; 0 is everything.
PRESET_MINUTES: array[0..5] of integer = (0, 60, 180, 360, 720, 1440);

// The short caption the toolbar shows for a preset.
function PresetCaption(const AMinutes: integer): string;
begin
  if AMinutes = 0 then
    Result := RS_HISTORY_GRAPH_MENU_RANGE_ALL
  else
    Result := Format('%dh', [AMinutes div 60]);
end;

{ TfHistoryGraph }

constructor TfHistoryGraph.Create(AOwner: TComponent);
var
  menuItem, rangeItem: TMenuItem;
  captions: THistoryGraphCaptions;
  i: integer;
begin
  inherited CreateNew(AOwner, 0);
  Caption := RS_HISTORY_GRAPH_TITLE;
  // 96 dpi design size; the LCL rescales the form to the monitor's DPI in
  // AfterConstruction, so no Px here.
  Width := 860;
  Height := 520;
  DoubleBuffered := true;
  Position := poMainFormCenter;
  BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  KeyPreview := true;
  ShowHint := true;

  FToolbar := TPanel.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align := alTop;
  FToolbar.Height := TOOLBAR_HEIGHT;
  FToolbar.BevelOuter := bvNone;
  FToolbar.BevelInner := bvNone;
  FToolbar.ParentColor := false;

  SetLength(FRangeButtons, Length(PRESET_MINUTES));
  for i := 0 to High(PRESET_MINUTES) do
  begin
    if PRESET_MINUTES[i] = 0 then
      FRangeButtons[i] := NewToolButton(PresetCaption(0),
        RS_HISTORY_GRAPH_TB_ALL_HINT, 1)
    else
      FRangeButtons[i] := NewToolButton(PresetCaption(PRESET_MINUTES[i]),
        Format(RS_HISTORY_GRAPH_TB_RANGE_HINT,
        [PresetCaption(PRESET_MINUTES[i])]), 1);
    FRangeButtons[i].Tag := PRESET_MINUTES[i];
    FRangeButtons[i].OnClick := @HandleRangeButton;
  end;
  FBasalButton := NewToolButton(RS_HISTORY_GRAPH_TB_BASAL,
    RS_HISTORY_GRAPH_TB_BASAL_HINT, 2);
  FBolusButton := NewToolButton(RS_HISTORY_GRAPH_TB_BOLUS,
    RS_HISTORY_GRAPH_TB_BOLUS_HINT, 3);
  FCarbButton := NewToolButton(RS_HISTORY_GRAPH_TB_CARBS,
    RS_HISTORY_GRAPH_TB_CARBS_HINT, 4);
  FBasalButton.OnClick := @HandleOverlayButton;
  FBolusButton.OnClick := @HandleOverlayButton;
  FCarbButton.OnClick := @HandleOverlayButton;
  FPngButton := NewToolButton(RS_HISTORY_GRAPH_TB_PNG,
    RS_HISTORY_GRAPH_TB_PNG_HINT, 0);
  FCsvButton := NewToolButton(RS_HISTORY_GRAPH_TB_CSV,
    RS_HISTORY_GRAPH_TB_CSV_HINT, 0);
  FPngButton.OnClick := @SaveAsPNG;
  FCsvButton.OnClick := @SaveAsCSV;

  FView := THistoryGraphView.Create(Self);
  FView.Parent := Self;
  FView.Align := alClient;
  FView.ParentFont := true;
  FView.OnReadingClick := @HandleReadingClick;
  FView.OnViewChanged := @HandleViewChanged;
  captions := DefaultHistoryGraphCaptions;
  captions.Empty := RS_HISTORY_GRAPH_EMPTY;
  captions.ReadingCount := RS_HISTORY_GRAPH_POINT_COUNT;
  captions.KeyRange := RS_HISTORY_GRAPH_KEY_RANGE;
  captions.KeyRangeHigh := RS_HISTORY_GRAPH_KEY_RANGE_HI;
  captions.KeyRangeLow := RS_HISTORY_GRAPH_KEY_RANGE_LO;
  captions.KeyHigh := RS_HISTORY_GRAPH_KEY_HIGH;
  captions.KeyLow := RS_HISTORY_GRAPH_KEY_LOW;
  captions.KeyUnknown := RS_HISTORY_GRAPH_KEY_UNKNOWN;
  captions.KeyBasal := RS_HISTORY_GRAPH_KEY_BASAL;
  captions.KeyBolus := RS_HISTORY_GRAPH_KEY_BOLUS;
  captions.KeyBolusAuto := RS_HISTORY_GRAPH_KEY_BOLUS_AUTO;
  captions.KeyCarbs := RS_HISTORY_GRAPH_KEY_CARBS;
  captions.KeyPredict := RS_HISTORY_GRAPH_KEY_PREDICT;
  FView.Captions := captions;

  // Context menu: the exports, the presets and a way back from a zoom.
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
  for i := 0 to High(PRESET_MINUTES) do
  begin
    rangeItem := TMenuItem.Create(FRangeMenu);
    case PRESET_MINUTES[i] of
    0:
      rangeItem.Caption := RS_HISTORY_GRAPH_MENU_RANGE_ALL;
    60:
      rangeItem.Caption := RS_HISTORY_GRAPH_MENU_RANGE_1H;
    180:
      rangeItem.Caption := RS_HISTORY_GRAPH_MENU_RANGE_3H;
    360:
      rangeItem.Caption := RS_HISTORY_GRAPH_MENU_RANGE_6H;
    720:
      rangeItem.Caption := RS_HISTORY_GRAPH_MENU_RANGE_12H;
    else
      rangeItem.Caption := RS_HISTORY_GRAPH_MENU_RANGE_24H;
    end;
    rangeItem.Tag := PRESET_MINUTES[i];
    rangeItem.RadioItem := true;
    rangeItem.GroupIndex := 1;
    rangeItem.OnClick := @HandleRangeMenuClick;
    FRangeMenu.Add(rangeItem);
  end;
  menuItem := TMenuItem.Create(FPopupMenu);
  menuItem.Caption := RS_HISTORY_GRAPH_MENU_RESET;
  menuItem.OnClick := @HandleResetZoom;
  FPopupMenu.Items.Add(menuItem);
  FView.PopupMenu := FPopupMenu;

  SetDarkTheme(TrndiNative.isDarkMode);
  SyncToolbar;
end;

destructor TfHistoryGraph.Destroy;
begin
  if fHistoryGraph = Self then
    fHistoryGraph := nil;
  inherited Destroy;
end;

function TfHistoryGraph.Px(const ASize: integer): integer;
begin
  Result := Scale96ToForm(ASize);
end;

function TfHistoryGraph.NewToolButton(const ACaption, AHint: string;
const AGroup: integer): TSpeedButton;
begin
  Result := TSpeedButton.Create(Self);
  Result.Parent := FToolbar;
  Result.Caption := ACaption;
  Result.Hint := AHint;
  Result.ShowHint := true;
  Result.Flat := true;
  Result.GroupIndex := AGroup;
  // The presets share a group so one is down at a time, and may all be up
  // once the user has zoomed away from them; each overlay toggle is a
  // group of its own.
  Result.AllowAllUp := AGroup > 0;
end;

{ Place the buttons by hand: the overlay toggles come and go with the data,
  and an anchor chain through a hidden control would leave its gap behind. }
procedure TfHistoryGraph.LayoutToolbar;
var
  x, y, h, i: integer;

  procedure Place(AButton: TSpeedButton; const AWidth: integer);
  begin
    AButton.SetBounds(x, y, Px(AWidth), h);
    if AButton.Visible then
      Inc(x, Px(AWidth) + Px(BUTTON_GAP));
  end;

begin
  h := Px(BUTTON_HEIGHT);
  y := (FToolbar.ClientHeight - h) div 2;
  x := Px(TOOLBAR_PAD);
  for i := 0 to High(FRangeButtons) do
    Place(FRangeButtons[i], RANGE_BUTTON_WIDTH);
  Inc(x, Px(GROUP_GAP));
  Place(FBasalButton, OVERLAY_BUTTON_WIDTH);
  Place(FBolusButton, OVERLAY_BUTTON_WIDTH);
  Place(FCarbButton, OVERLAY_BUTTON_WIDTH);
  // Exports flush right.
  x := FToolbar.ClientWidth - Px(TOOLBAR_PAD) - Px(EXPORT_BUTTON_WIDTH);
  FCsvButton.SetBounds(x, y, Px(EXPORT_BUTTON_WIDTH), h);
  Dec(x, Px(EXPORT_BUTTON_WIDTH) + Px(BUTTON_GAP));
  FPngButton.SetBounds(x, y, Px(EXPORT_BUTTON_WIDTH), h);
end;

{ Mirror the view's state on the toolbar and the menu: which preset is in
  force (none, after a zoom), which overlays exist and are on. }
procedure TfHistoryGraph.SyncToolbar;
var
  i: integer;
begin
  for i := 0 to High(FRangeButtons) do
    FRangeButtons[i].Down := FRangeButtons[i].Tag = FView.RangeMinutes;
  if Assigned(FRangeMenu) then
    for i := 0 to FRangeMenu.Count - 1 do
      FRangeMenu.Items[i].Checked := FRangeMenu.Items[i].Tag = FView.RangeMinutes;
  FBasalButton.Visible := FView.HasBasal;
  FBasalButton.Down := FView.BasalVisible;
  FBolusButton.Visible := FView.HasBolus or FView.HasAutoBolus;
  FBolusButton.Down := FView.BolusVisible;
  FCarbButton.Visible := FView.HasCarbs;
  FCarbButton.Down := FView.CarbsVisible;
  FPngButton.Enabled := FView.HasReadings;
  FCsvButton.Enabled := FView.HasReadings;
  LayoutToolbar;
end;

procedure TfHistoryGraph.ApplyTheme;
var
  theme: THistoryGraphTheme;
  i: integer;

  procedure Tint(AButton: TSpeedButton);
  begin
    AButton.Font.Color := theme.Text;
  end;

begin
  if FDark then
    theme := DarkHistoryGraphTheme
  else
    theme := LightHistoryGraphTheme;
  FView.Theme := theme;
  Color := theme.Background;
  FToolbar.Color := theme.Background;
  for i := 0 to High(FRangeButtons) do
    Tint(FRangeButtons[i]);
  Tint(FBasalButton);
  Tint(FBolusButton);
  Tint(FCarbButton);
  Tint(FPngButton);
  Tint(FCsvButton);
end;

procedure TfHistoryGraph.HandleRangeButton(Sender: TObject);
begin
  if Sender is TSpeedButton then
    FView.SetRangeMinutes(TSpeedButton(Sender).Tag);
end;

procedure TfHistoryGraph.HandleRangeMenuClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    FView.SetRangeMinutes(TMenuItem(Sender).Tag);
end;

procedure TfHistoryGraph.HandleOverlayButton(Sender: TObject);
begin
  // The speed button has already flipped its Down state.
  if Sender = FBasalButton then
    FView.SetBasalOverlayEnabled(FBasalButton.Down)
  else if Sender = FBolusButton then
    FView.SetBolusOverlayEnabled(FBolusButton.Down, FView.AutoBolusVisible)
  else if Sender = FCarbButton then
    FView.SetCarbOverlayEnabled(FCarbButton.Down);
end;

procedure TfHistoryGraph.HandleResetZoom(Sender: TObject);
begin
  FView.ResetView;
end;

procedure TfHistoryGraph.HandleViewChanged(Sender: TObject);
begin
  Caption := Format('%s (%d)', [RS_HISTORY_GRAPH_TITLE, FView.VisibleCount]);
  SyncToolbar;
end;

procedure TfHistoryGraph.HandleReadingClick(Sender: TObject;
const Reading: BGReading);
begin
  ShowReadingDetails(Reading);
end;

procedure TfHistoryGraph.Resize;
begin
  inherited Resize;
  // Fires while the constructor is still sizing the form, before the
  // toolbar exists.
  if Assigned(FCsvButton) then
    LayoutToolbar;
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

procedure TfHistoryGraph.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  inherited DoClose(CloseAction);
end;

// ---------------------------------------------------------------------------
// Forwarders
// ---------------------------------------------------------------------------

procedure TfHistoryGraph.SetReadings(const Readings: BGResults;
UnitPref: BGUnit);
begin
  FView.SetReadings(Readings, UnitPref);
end;

procedure TfHistoryGraph.SetPalette(const Palette: THistoryGraphPalette);
begin
  FView.Palette := Palette;
end;

procedure TfHistoryGraph.SetThresholds(const cgmHi, cgmLo, cgmRangeHi,
cgmRangeLo: integer);
begin
  FView.SetThresholds(cgmHi, cgmLo, cgmRangeHi, cgmRangeLo);
end;

procedure TfHistoryGraph.SetDarkTheme(const ADark: boolean);
begin
  FDark := ADark;
  ApplyTheme;
end;

procedure TfHistoryGraph.SetRangeMinutes(const AMinutes: integer);
begin
  FView.SetRangeMinutes(AMinutes);
end;

procedure TfHistoryGraph.SetBasalProfile(const profile: TBasalProfile;
const maxBasal: single);
begin
  FView.SetBasalProfile(profile, maxBasal);
end;

procedure TfHistoryGraph.SetBasalOverlayEnabled(aEnabled: boolean);
begin
  FView.SetBasalOverlayEnabled(aEnabled);
end;

procedure TfHistoryGraph.SetBoluses(const Boluses: TBolusList);
begin
  FView.SetBoluses(Boluses);
end;

procedure TfHistoryGraph.SetBolusOverlayEnabled(aEnabled: boolean;
aIncludeAutomatic: boolean);
begin
  FView.SetBolusOverlayEnabled(aEnabled, aIncludeAutomatic);
end;

procedure TfHistoryGraph.SetCarbs(const Carbs: TCarbList);
begin
  FView.SetCarbs(Carbs);
end;

procedure TfHistoryGraph.SetCarbOverlayEnabled(aEnabled: boolean);
begin
  FView.SetCarbOverlayEnabled(aEnabled);
end;

procedure TfHistoryGraph.SetPredictions(const Predictions: BGResults);
begin
  FView.SetPredictions(Predictions);
end;

// ---------------------------------------------------------------------------
// Export
// ---------------------------------------------------------------------------

procedure TfHistoryGraph.RenderToStream(AStream: TStream);
begin
  FView.RenderToStream(AStream, ClientWidth, ClientHeight);
end;

procedure TfHistoryGraph.SaveAsPNG(Sender: TObject);
var
  saveDialog: TSavePictureDialog;
  fileStream: TFileStream;
begin
  if not FView.HasReadings then
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
      fileStream := TFileStream.Create(saveDialog.FileName, fmCreate);
      try
        // The plot alone, at the size it is shown; the toolbar is not part
        // of the picture.
        FView.RenderToStream(fileStream, FView.ClientWidth, FView.ClientHeight);
      finally
        fileStream.Free;
      end;
    except
      on E: Exception do
        SlickeHTMLMsg(sdsAuto, 'Error', Format(RS_HISTORY_GRAPH_SAVE_ERROR,
          [E.Message]), [mbOK], uxmtError, 12.5);
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TfHistoryGraph.SaveAsCSV(Sender: TObject);
var
  saveDialog: TSaveDialog;
  lines: TStringList;
  readings: BGResults;
  i, rssi, noise: integer;
  un: BGUnit;
  levelStr, rssiStr, noiseStr: string;
begin
  if not FView.HasReadings then
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

    un := FView.DisplayUnit;
    readings := FView.VisibleReadings;
    lines := TStringList.Create;
    try
      try
        lines.Add('Date,Time,Value,Unit,Delta,Trend,Level,RSSI,Noise,Source,Sensor');
        for i := 0 to High(readings) do
        begin
          case readings[i].level of
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
          if readings[i].TryGetRSSI(rssi) then
            rssiStr := IntToStr(rssi)
          else
            rssiStr := '';
          if readings[i].TryGetNoise(noise) then
            noiseStr := IntToStr(noise)
          else
            noiseStr := '';
          lines.Add(Format('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s',
            [FormatDateTime('yyyy-mm-dd', readings[i].date),
            FormatDateTime('hh:nn:ss', readings[i].date),
            readings[i].format(un, BG_MSG_SHORT, BGPrimary),
            BG_UNIT_NAMES[un],
            readings[i].format(un, BG_MSG_SIG_SHORT, BGDelta),
            BG_TRENDS[readings[i].trend], levelStr, rssiStr, noiseStr,
            readings[i].Source, readings[i].sensor]));
        end;
        lines.SaveToFile(saveDialog.FileName);
      except
        on E: Exception do
          SlickeHTMLMsg(sdsAuto, 'Error', Format(RS_HISTORY_GRAPH_SAVE_ERROR,
            [E.Message]), [mbOK], uxmtError, 12.5);
      end;
    finally
      lines.Free;
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
  // The same getters and formatting as the main UI's history list popup.
  if Reading.TryGetRSSI(xval) then
    rssi := xval.ToString
  else
    rssi := RS_RH_UNKNOWN;
  if Reading.TryGetNoise(xval) then
    noise := xval.ToString
  else
    noise := RS_RH_UNKNOWN;

  SlickeHTMLMsg(sdsAuto, RS_RH_READING, '<font size="4"><u></u>' +
    TimeToStr(Reading.date) + '</font><font size="3">' + sHTMLLineBreak +
    StringReplace(Format(RS_HISTORY_ITEM,
    [Reading.format(FView.DisplayUnit, BG_MSG_SHORT, BGPrimary),
    Reading.format(FView.DisplayUnit, BG_MSG_SIG_SHORT, BGDelta),
    Reading.trend.Img, rssi, noise, Reading.Source, Reading.sensor]),
    sLineBreak, sHTMLLineBreak, [rfReplaceAll]), [mbOK], uxmtInformation, 12.5);
end;

// ---------------------------------------------------------------------------
// Entry points
// ---------------------------------------------------------------------------

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette);
begin
  ShowHistoryGraph(Readings, UnitPref, Palette, 180, 60, 160, 80);
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit;
const Palette: THistoryGraphPalette; const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
begin
  if not Assigned(fHistoryGraph) then
    fHistoryGraph := TfHistoryGraph.Create(Application);
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
