unit uhistorygraph;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Menus, trndi.types, trndi.api;

type
  THistoryGraphPalette = record
    Range: TColor;
    RangeHigh: TColor;
    RangeLow: TColor;
    High: TColor;
    Low: TColor;
    Unknown: TColor;
  end;

  { Minimal, headless stub of the real TfHistoryGraph used by the UI. }
  TfHistoryGraph = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetReadings(const Readings: BGResults; UnitPref: BGUnit);
    procedure SetPalette(const Palette: THistoryGraphPalette);
    procedure SetThresholds(const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
    procedure SaveAsPNG(Sender: TObject);
    procedure SaveAsCSV(Sender: TObject);
    procedure SetBasalProfile(const profile: TBasalProfile; const maxBasal: single = 3.0);
    procedure SetBasalOverlayEnabled(aEnabled: boolean);
  end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit); overload;
procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit; const Palette: THistoryGraphPalette); overload;
procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit; const Palette: THistoryGraphPalette; const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer); overload;

var
  fHistoryGraph: TfHistoryGraph = nil;

implementation

{ TfHistoryGraph }

constructor TfHistoryGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // No GUI in headless tests; keep minimal state only.
end;

destructor TfHistoryGraph.Destroy;
begin
  inherited Destroy;
end;

procedure TfHistoryGraph.SetReadings(const Readings: BGResults; UnitPref: BGUnit);
begin
  // no-op for tests
end;

procedure TfHistoryGraph.SetPalette(const Palette: THistoryGraphPalette);
begin
  // no-op for tests
end;

procedure TfHistoryGraph.SetThresholds(const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
begin
  // no-op for tests
end;

procedure TfHistoryGraph.SaveAsPNG(Sender: TObject);
begin
  // no-op for tests
end;

procedure TfHistoryGraph.SaveAsCSV(Sender: TObject);
begin
  // no-op for tests
end;

procedure TfHistoryGraph.SetBasalProfile(const profile: TBasalProfile; const maxBasal: single);
begin
  // no-op for tests
end;

procedure TfHistoryGraph.SetBasalOverlayEnabled(aEnabled: boolean);
begin
  // no-op for tests
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit);
begin
  if fHistoryGraph = nil then
    fHistoryGraph := TfHistoryGraph.Create(nil);
  fHistoryGraph.SetReadings(Readings, UnitPref);
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit; const Palette: THistoryGraphPalette);
begin
  if fHistoryGraph = nil then
    fHistoryGraph := TfHistoryGraph.Create(nil);
  fHistoryGraph.SetPalette(Palette);
  fHistoryGraph.SetReadings(Readings, UnitPref);
end;

procedure ShowHistoryGraph(const Readings: BGResults; const UnitPref: BGUnit; const Palette: THistoryGraphPalette; const cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer);
begin
  if fHistoryGraph = nil then
    fHistoryGraph := TfHistoryGraph.Create(nil);
  fHistoryGraph.SetPalette(Palette);
  fHistoryGraph.SetThresholds(cgmHi, cgmLo, cgmRangeHi, cgmRangeLo);
  fHistoryGraph.SetReadings(Readings, UnitPref);
end;

end.
