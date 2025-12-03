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

unit uhistorygraph;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Math,
  trndi.types, trndi.strings;

type
  { TfHistoryGraph }
  TfHistoryGraph = class(TForm)
  private
    type
      TGraphPoint = record
        Reading: BGReading;
        Value: double;
      end;
  private
    FPoints: array of TGraphPoint;
    FUnit: BGUnit;
    FMinValue: double;
    FMaxValue: double;
    FMinTime: TDateTime;
    FMaxTime: TDateTime;
    FDotRadius: integer;
    function GetPlotRect: TRect;
    procedure SortPointsByTime;
    procedure UpdateExtents;
    procedure DrawAxesAndGrid(ACanvas: TCanvas; const PlotRect: TRect);
    procedure DrawPolyline(ACanvas: TCanvas; const PlotRect: TRect);
    procedure DrawPoints(ACanvas: TCanvas; const PlotRect: TRect);
    procedure DrawLegend(ACanvas: TCanvas; const PlotRect: TRect);
    function TimeToX(const TimeValue: TDateTime; const PlotRect: TRect): integer;
    function ValueToY(const Value: double; const PlotRect: TRect): integer;
    function LevelColor(const Level: BGValLevel): TColor;
    function PointAt(const X, Y: integer): integer;
    function HasData: boolean;
    procedure ShowReadingDetails(const Reading: BGReading);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetReadings(const Readings: BGResults; UnitPref: BGUnit);
  end;

  {** Display a dot-based history plot for the supplied readings. Reuses the
    same form instance between invocations to avoid repeated allocations. }
  procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit);

var
  fHistoryGraph: TfHistoryGraph = nil;

implementation

uses
  LCLType;

resourcestring
  RS_HISTORY_GRAPH_TITLE = 'History graph';
  RS_HISTORY_GRAPH_EMPTY = 'No history data to plot';
  RS_HISTORY_GRAPH_HELP = 'Click a dot to see the full reading details';
  RS_HISTORY_GRAPH_POINT_COUNT = '%d readings';
  RS_HISTORY_GRAPH_RANGE = '%s – %s';
  RS_HISTORY_GRAPH_UNIT_FMT = 'Values in %s';

const
  GRAPH_MARGIN_LEFT = 72;
  GRAPH_MARGIN_TOP = 40;
  GRAPH_MARGIN_RIGHT = 36;
  GRAPH_MARGIN_BOTTOM = 72;
  GRAPH_DIVISIONS = 5;

{ TfHistoryGraph }

constructor TfHistoryGraph.Create(AOwner: TComponent);
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
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clSilver;
  ACanvas.Rectangle(PlotRect);

  lineColor := $00E0E0E0;

  for i := 0 to GRAPH_DIVISIONS do
  begin
    value := FMinValue + (FMaxValue - FMinValue) * (i / GRAPH_DIVISIONS);
    y := ValueToY(value, PlotRect);
    ACanvas.Pen.Color := lineColor;
    ACanvas.MoveTo(PlotRect.Left, y);
    ACanvas.LineTo(PlotRect.Right, y);

    labelText := FormatFloat('0.0', value);
    ACanvas.Pen.Color := clGray;
    ACanvas.TextOut(PlotRect.Left - 48,
      y - ACanvas.TextHeight(labelText) div 2, labelText);
  end;

  for i := 0 to GRAPH_DIVISIONS do
  begin
    timeVal := FMinTime + (FMaxTime - FMinTime) * (i / GRAPH_DIVISIONS);
    x := TimeToX(timeVal, PlotRect);
    ACanvas.Pen.Color := lineColor;
    ACanvas.MoveTo(x, PlotRect.Top);
    ACanvas.LineTo(x, PlotRect.Bottom);

    labelText := FormatDateTime('hh:nn', timeVal);
    ACanvas.Pen.Color := clGray;
    ACanvas.TextOut(x - ACanvas.TextWidth(labelText) div 2,
      PlotRect.Bottom + 8, labelText);
  end;

  ACanvas.TextOut(PlotRect.Left, PlotRect.Bottom + 28,
    Format(RS_HISTORY_GRAPH_UNIT_FMT, [BG_UNIT_NAMES[FUnit]]));
end;

procedure TfHistoryGraph.DrawLegend(ACanvas: TCanvas; const PlotRect: TRect);
var
  info, rangeInfo: string;
  firstStamp, lastStamp: string;
  bottomY: integer;
begin
  info := Format(RS_HISTORY_GRAPH_POINT_COUNT, [Length(FPoints)]);
  firstStamp := FormatDateTime('ddd dd mmm hh:nn', FPoints[0].Reading.date);
  lastStamp := FormatDateTime('ddd dd mmm hh:nn',
    FPoints[High(FPoints)].Reading.date);
  rangeInfo := Format(RS_HISTORY_GRAPH_RANGE, [firstStamp, lastStamp]);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [];
  ACanvas.TextOut(PlotRect.Left, 12, info);
  ACanvas.TextOut(PlotRect.Left, 12 + ACanvas.TextHeight(info) + 2, rangeInfo);

  bottomY := ClientHeight - 26;
  ACanvas.TextOut(PlotRect.Left,
    bottomY, RS_HISTORY_GRAPH_HELP);
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
begin
  if Length(FPoints) < 2 then
    Exit;

  ACanvas.Pen.Color := clSilver;
  ACanvas.Brush.Style := bsClear;
  ACanvas.MoveTo(TimeToX(FPoints[0].Reading.date, PlotRect),
    ValueToY(FPoints[0].Value, PlotRect));
  for i := 1 to High(FPoints) do
    ACanvas.LineTo(TimeToX(FPoints[i].Reading.date, PlotRect),
      ValueToY(FPoints[i].Value, PlotRect));
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
  if (Button <> mbLeft) or (not HasData) then
    Exit;

  idx := PointAt(X, Y);
  if idx > -1 then
    ShowReadingDetails(FPoints[idx].Reading);
end;

procedure TfHistoryGraph.Paint;
var
  plotRect: TRect;
  messageText: string;
begin
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

procedure TfHistoryGraph.ShowReadingDetails(const Reading: BGReading);
var
  xval: integer;
  rssi, noise: string;
begin
  if Reading.getRSSI(xval) then
    rssi := xval.ToString
  else
    rssi := RS_RH_UNKNOWN;

  if Reading.getNoise(xval) then
    noise := xval.ToString
  else
    noise := RS_RH_UNKNOWN;

  ShowMessage(TimeToStr(Reading.date) + sLineBreak+
    Format(RS_HISTORY_ITEM,
    [Reading.format(FUnit, BG_MSG_SHORT, BGPrimary),
    Reading.format(FUnit, BG_MSG_SIG_SHORT, BGDelta),
    Reading.trend.Img, rssi, noise, Reading.Source, Reading.sensor]));
end;

procedure TfHistoryGraph.SortPointsByTime;
var
  i, j: integer;
  tmp: TGraphPoint;
begin
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
  case Level of
    BGRange, BGRangeHI:
      Result := RGBToColor(64, 145, 108);
    BGRangeLO, BGLOW:
      Result := RGBToColor(33, 99, 174);
    BGHigh:
      Result := RGBToColor(217, 95, 2);
  else
    Result := RGBToColor(180, 180, 180);
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

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit);
begin
  if not Assigned(fHistoryGraph) then
    fHistoryGraph := TfHistoryGraph.Create(Application);

  fHistoryGraph.SetReadings(Readings, UnitPref);
  fHistoryGraph.Show;
  fHistoryGraph.BringToFront;
end;

end.
