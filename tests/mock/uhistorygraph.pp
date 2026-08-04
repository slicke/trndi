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
    procedure SetPredictions(const Predictions: BGResults);
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

procedure TfHistoryGraph.SetPredictions(const Predictions: BGResults);
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
