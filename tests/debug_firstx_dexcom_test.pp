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
unit debug_firstx_dexcom_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  trndi.api.debug_firstxmissing, trndi.types, dateutils;

type
  TDexcomFirstXTester = class(TTestCase)
  published
    procedure TestDexcomModeSetsDeviceAndSource;
    procedure TestDexcomModeCreatesGap;
    procedure TestDexcomFutureTimestamp;
    procedure TestDexcomMissingDeltaClearsDelta;
  end;

implementation

procedure TDexcomFirstXTester.TestDexcomModeSetsDeviceAndSource;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('3:dexcom', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 3;
    // After removing the first N readings the remaining count should be reduced
    AssertEquals('Remaining count should equal original minus missing', 11 - missing, Length(r));
    // ensure the earliest remaining reading looks Dexcom-like
    AssertEquals('Source should be Dexcom', 'Dexcom', r[0].Source);
    AssertTrue('Device should mention Dexcom', Pos('Dexcom', r[0].sensor) > 0);
  finally
    api.Free;
  end;
end;

procedure TDexcomFirstXTester.TestDexcomModeCreatesGap;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
  gap: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:dexcom', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 2;
    gap := MinutesBetween(r[0].date, r[1].date);
    // Expect gap to be at least larger than normal 5-minute interval
    AssertTrue('Gap should be larger than a normal 5-minute interval', gap >= 15);
  finally
    api.Free;
  end;
end;

procedure TDexcomFirstXTester.TestDexcomFutureTimestamp;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
  secs: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:dexcom=future', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 2;
    secs := SecondsBetween(r[0].date, r[1].date);
    // Future mode should produce a timestamp slightly later than the next (but not huge)
    AssertTrue('Future-mode timestamp should be slightly later than the next reading', (secs > 0) and (secs < 300));
  finally
    api.Free;
  end;
end;

procedure TDexcomFirstXTester.TestDexcomMissingDeltaClearsDelta;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:dexcom=missing-delta', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 2;
    AssertTrue('Delta should be empty for Dexcom missing-delta mode', r[0].deltaEmpty);
    AssertEquals('Source should be Dexcom', 'Dexcom', r[0].Source);
  finally
    api.Free;
  end;
end;

initialization
  RegisterTest(TDexcomFirstXTester);

end.
