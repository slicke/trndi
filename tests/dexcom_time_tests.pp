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
unit dexcom_time_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, SysUtils, DateUtils,
  trndi.api.dexcom_time;

type
  TDexcomTimeParsingTests = class(TTestCase)
  published
    procedure ParsesXmlSystemTime;
    procedure ParsesDateMs;
    procedure ParsesDateMsWithOffsetSuffix;
    procedure ParsesJsonServerTimeDateMs;
    procedure ParsesJsonServerTimeNumericMs;
    procedure ParsesJsonDateTimeKey;
    procedure ParsesIsoZ;
    procedure RejectsInvalid;
  end;

implementation

const
  ONE_SECOND: TDateTime = 1 / (24 * 60 * 60);

procedure TDexcomTimeParsingTests.ParsesXmlSystemTime;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('<SystemTime>2021-01-12T12:32:04</SystemTime>', dt));
  expected := EncodeDateTime(2021, 1, 12, 12, 32, 4, 0);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesDateMs;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('/Date(1610464324000)/', dt));
  expected := UnixToDateTime(1610464324, False);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

// Dexcom's DT field carries the offset inside the parentheses, e.g.
// "Date(1786478432000+0000)". The suffix is informational: the millisecond
// value is a UTC epoch, so the result must equal the plain form's.
procedure TDexcomTimeParsingTests.ParsesDateMsWithOffsetSuffix;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('Date(1610464324000+0000)', dt));
  expected := UnixToDateTime(1610464324, False);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);

  // A non-zero offset must not shift the instant either
  AssertTrue(ParseDexcomTime('/Date(1610464324000-0500)/', dt));
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesJsonServerTimeDateMs;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('{"ServerTime":"/Date(1610464324000)/"}', dt));
  expected := UnixToDateTime(1610464324, False);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

// The live SystemUtcTime response uses "DateTime", not "ServerTime", and pairs
// it with OffsetMinutes -- which means wall-clock semantics, so with offset 0
// the result is UTC rather than local.
procedure TDexcomTimeParsingTests.ParsesJsonDateTimeKey;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime(
    '{"DateTime":"\/Date(1610464324000)\/","OffsetMinutes":0}', dt));
  expected := UnixToDateTime(1610464324, True);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesJsonServerTimeNumericMs;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('{"ServerTime":1610464324000}', dt));
  expected := UnixToDateTime(1610464324, False);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesIsoZ;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('2021-01-12T12:32:04Z', dt));
  // Note: ParseDexcomTime currently parses the timestamp and ignores the trailing 'Z'
  // (no timezone conversion), so expected is the literal date/time.
  expected := EncodeDateTime(2021, 1, 12, 12, 32, 4, 0);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.RejectsInvalid;
var
  dt: TDateTime;
begin
  AssertFalse(ParseDexcomTime('Banana', dt));
end;

initialization
  RegisterTest(TDexcomTimeParsingTests);

end.
