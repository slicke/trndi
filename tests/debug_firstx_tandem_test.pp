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
unit debug_firstx_tandem_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  trndi.api.debug_firstxmissing, trndi.types, dateutils;

type
  TTandemFirstXTester = class(TTestCase)
  published
    procedure TestTandemModeSetsDeviceAndSource;
    procedure TestTandemModeCreatesGap;
    procedure TestTandemFutureTimestamp;
    procedure TestTandemDuplicateTimestamp;
    procedure TestTandemBackwardsTimestamp;
    procedure TestTandemMissingDeltaClearsDelta;
    procedure TestTandemTargetSecond;
    procedure TestTandemTargetMultiple;
  end;

implementation

procedure TTandemFirstXTester.TestTandemModeSetsDeviceAndSource;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('3:tandem', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 3;
    AssertEquals('Remaining count should equal original minus missing', 11 - missing, Length(r));
    // ensure the earliest remaining reading looks Tandem-like
    AssertEquals('Source should be Tandem', 'Tandem', r[0].Source);
    AssertTrue('Sensor should mention Tandem', Pos('Tandem', r[0].sensor) > 0);
  finally
    api.Free;
  end;
end;

procedure TTandemFirstXTester.TestTandemModeCreatesGap;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
  gap: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:tandem', '');
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

procedure TTandemFirstXTester.TestTandemFutureTimestamp;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
  secs: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:tandem=future', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 2;
    secs := SecondsBetween(r[0].date, r[1].date);
    AssertTrue('Future-mode timestamp should be slightly later than the next reading', (secs > 0) and (secs < 300));
  finally
    api.Free;
  end;
end;

procedure TTandemFirstXTester.TestTandemDuplicateTimestamp;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:tandem=duplicate', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 2;
    AssertEquals('Duplicate timestamp should equal the following reading timestamp', r[0].date, r[1].date);
  finally
    api.Free;
  end;
end;

procedure TTandemFirstXTester.TestTandemBackwardsTimestamp;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:tandem=backwards', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 2;
    // Backwards mode should produce a timestamp earlier than the next reading
    AssertTrue('Backwards-mode timestamp should be earlier than the next reading', r[0].date < r[1].date);
  finally
    api.Free;
  end;
end;

procedure TTandemFirstXTester.TestTandemMissingDeltaClearsDelta;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:tandem=missing-delta', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 2;
    AssertTrue('Delta should be empty for Tandem missing-delta mode', r[0].deltaEmpty);
  finally
    api.Free;
  end;
end;

procedure TTandemFirstXTester.TestTandemTargetSecond;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
  idx: integer;
begin
  api := DebugFirstXMissingAPI.Create('3:tandem=duplicate@1', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 3;
    idx := 1; // offset 1 relative to first remaining
    // Duplicate target at second remaining: its timestamp should equal the next reading
    AssertEquals('Second-target duplicate timestamp should equal following', r[idx].date, r[idx+1].date);
  finally
    api.Free;
  end;
end;

procedure TTandemFirstXTester.TestTandemTargetMultiple;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('4:tandem=duplicate@0,1', '');
  try
    r := api.getReadings(0, 11, '', resStr, false);
    missing := 4;
    // Both first and second remaining should be duplicate timestamps relative to their following readings
    AssertEquals('First target duplicate timestamp should equal following', r[0].date, r[1].date);
    AssertEquals('Second target duplicate timestamp should equal following', r[1].date, r[2].date);
  finally
    api.Free;
  end;
end;

initialization
  RegisterTest(TTandemFirstXTester);

end.
