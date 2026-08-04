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
unit debug_intermit_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  trndi.api.debug_intermittentmissing, trndi.types;

type
  TIntermittentTester = class(TTestCase)
  published
    procedure TestClearsWithinNewestRange;
    procedure TestClearedCountWithinBounds;
  end;

implementation

procedure TIntermittentTester.TestClearsWithinNewestRange;
var
  api: DebugIntermittentMissingAPI;
  resStr: string;
  r: BGResults;
  i, idx: Integer;
  anyClearedInFront: Boolean;
begin
  api := DebugIntermittentMissingAPI.Create('2-4', '');
  try
    anyClearedInFront := False;
    for i := 1 to 50 do
    begin
      r := api.getReadings(0, 11, '', resStr, False);
      // ensure we have at least 1 reading
      AssertTrue('Should return readings', Length(r) > 0);
      // check that cleared entries (exists=false) are within the first 10 (newest)
      for idx := 0 to Length(r)-1 do
      begin
        // cleared readings are indicated by empty=true
        if r[idx].empty then
        begin
          // cleared index must be within the most recent 10
          AssertTrue('Cleared index should be among newest 10', idx < 10);
          if idx < 3 then
            anyClearedInFront := True;
        end;
      end;
    end;
    // ensure some clears hit the very front region (newest few indices)
    AssertTrue('At least one cleared reading should be among the very newest indices', anyClearedInFront);
  finally
    api.Free;
  end;
end;

procedure TIntermittentTester.TestClearedCountWithinBounds;
var
  api: DebugIntermittentMissingAPI;
  resStr: string;
  r: BGResults;
  i, cleared, j: Integer;
begin
  api := DebugIntermittentMissingAPI.Create('2-4', '');
  try
    for i := 1 to 100 do
    begin
      r := api.getReadings(0, 11, '', resStr, False);
      cleared := 0;
      for j := 0 to High(r) do
        if r[j].empty then Inc(cleared);
      // Cleared should be between 2 and 4, not exceeding 10
      AssertTrue('Cleared count >= 2', cleared >= 2);
      AssertTrue('Cleared count <= 4', cleared <= 4);
      AssertTrue('Cleared count <= length', cleared <= Length(r));
    end;
  finally
    api.Free;
  end;
end;

initialization
  RegisterTest(TIntermittentTester);

end.
