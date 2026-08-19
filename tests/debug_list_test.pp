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

unit debug_list_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  dateutils, trndi.api, trndi.api.debug_list, trndi.types;

type
  TDebugListTester = class(TTestCase)
  published
    procedure TestMmolListNewestLast;
    procedure TestMgdlListWhenNoDecimalPoint;
    procedure TestUnderscoreMarksMissing;
    procedure TestUnderscoreCountRepeatsGap;
    procedure TestShortListPadsOlderSlotsAsMissing;
    procedure TestUnitHintOverridesAutodetect;
    procedure TestEmptyListFallsBackToDefaultSeries;
    procedure TestDeltaSkipsMissingReadings;
  end;

implementation

const
  // '4.0 5.5 5.0 2.7' in mg/dL, as the backend converts it
  MMOL_4_0 = 72;   // round(4.0 * 18.0156)
  MMOL_2_7 = 49;   // round(2.7 * 18.0156)

function Readings(const user, pass: string): BGResults;
var
  api: DebugListAPI;
  resStr: string;
begin
  api := DebugListAPI.Create(user, pass);
  try
    api.connect;
    Result := api.getReadings(0, 11, '', resStr, False);
  finally
    api.Free;
  end;
end;

{ The list is typed oldest first, so the last value typed is the current reading
  at index 0. }
procedure TDebugListTester.TestMmolListNewestLast;
var
  r: BGResults;
begin
  r := Readings('4.0 5.5 5.0 2.7', '');
  AssertEquals('newest reading', MMOL_2_7, System.Round(r[0].val));
  AssertEquals('oldest typed reading', MMOL_4_0, System.Round(r[3].val));
  AssertTrue('newest is 5 minutes after the one before it',
    Abs(MinuteSpan(r[0].date, r[1].date) - 5) < 0.01);
end;

{ Without a '.' anywhere the values are taken as mg/dL and used verbatim. }
procedure TDebugListTester.TestMgdlListWhenNoDecimalPoint;
var
  r: BGResults;
begin
  r := Readings('120 90 55', '');
  AssertEquals('newest reading', 55, System.Round(r[0].val));
  AssertEquals('middle reading', 90, System.Round(r[1].val));
  AssertEquals('oldest reading', 120, System.Round(r[2].val));
end;

procedure TDebugListTester.TestUnderscoreMarksMissing;
var
  r: BGResults;
begin
  r := Readings('120 _ 90 55', '');
  AssertFalse('newest present', r[0].empty);
  AssertFalse('second present', r[1].empty);
  AssertTrue('third is the marked gap', r[2].empty);
  AssertFalse('fourth present', r[3].empty);
  AssertEquals('value after the gap', 120, System.Round(r[3].val));
end;

procedure TDebugListTester.TestUnderscoreCountRepeatsGap;
var
  r: BGResults;
  i, gaps: integer;
begin
  r := Readings('120 _3 55', '');
  gaps := 0;
  for i := 1 to 3 do
    if r[i].empty then
      Inc(gaps);
  AssertEquals('_3 leaves three consecutive gaps', 3, gaps);
  AssertEquals('reading before the gap', 120, System.Round(r[4].val));
end;

{ A list shorter than the usual window reports the remaining, older slots as
  missing rather than silently shrinking the series. }
procedure TDebugListTester.TestShortListPadsOlderSlotsAsMissing;
var
  r: BGResults;
  i: integer;
begin
  r := Readings('4.0 5.5 5.0 2.7', '');
  AssertEquals('series padded to the usual window', 11, Length(r));
  for i := 0 to 3 do
    AssertFalse('typed reading present', r[i].empty);
  for i := 4 to High(r) do
    AssertTrue('older slot missing', r[i].empty);
end;

procedure TDebugListTester.TestUnitHintOverridesAutodetect;
var
  r: BGResults;
begin
  r := Readings('4 5 6', 'mmol');
  AssertEquals('whole numbers read as mmol/L', System.Round(6 * TrndiAPI.toMGDL),
    System.Round(r[0].val));
  r := Readings('120.0 90.0', 'mgdl');
  AssertEquals('decimals read as mg/dL', 90, System.Round(r[0].val));
end;

procedure TDebugListTester.TestEmptyListFallsBackToDefaultSeries;
var
  r: BGResults;
begin
  r := Readings('', '');
  AssertEquals('default debug series returned', 11, Length(r));
  AssertFalse('newest present', r[0].empty);
end;

{ A reading right after a gap still gets a delta, taken against the nearest
  older reading that exists. }
procedure TDebugListTester.TestDeltaSkipsMissingReadings;
var
  r: BGResults;
begin
  r := Readings('100 _ 130', '');
  AssertFalse('newest present', r[0].empty);
  AssertEquals('delta measured across the gap', 30, System.Round(r[0].delta));
end;

initialization
  RegisterTest(TDebugListTester);

end.
