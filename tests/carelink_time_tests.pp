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
unit carelink_time_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, SysUtils, DateUtils,
  trndi.types, trndi.api.carelink;

type
  TCareLinkTimeParsingTests = class(TTestCase)
  published
    procedure ParsesLocalTimeNoOffset;
    procedure ParsesFractionalSeconds;
    procedure ParsesUtcZulu;
    procedure ParsesPositiveOffsetWithColon;
    procedure ParsesNegativeOffsetCompact;
    procedure RejectsTooShort;
    procedure RejectsGarbage;
    procedure RejectsInvalidCalendarDate;
  end;

  TCareLinkTrendMappingTests = class(TTestCase)
  published
    procedure MapsNoneToFlat;
    procedure MapsSingleArrows;
    procedure MapsDoubleArrows;
    procedure MapsTripleArrowsToDouble;
    procedure UnknownIsPlaceholder;
    procedure IsCaseAndWhitespaceInsensitive;
  end;

implementation

const
  ONE_SECOND: TDateTime = 1 / (24 * 60 * 60);

procedure TCareLinkTimeParsingTests.ParsesLocalTimeNoOffset;
var
  dt, expected: TDateTime;
begin
  // No zone designator: taken as already local
  AssertTrue(ParseCareLinkTime('2026-03-15T08:30:45', dt));
  expected := EncodeDateTime(2026, 3, 15, 8, 30, 45, 0);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TCareLinkTimeParsingTests.ParsesFractionalSeconds;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseCareLinkTime('2026-03-15T08:30:45.123456', dt));
  expected := EncodeDateTime(2026, 3, 15, 8, 30, 45, 0);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TCareLinkTimeParsingTests.ParsesUtcZulu;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseCareLinkTime('2026-03-15T08:30:45Z', dt));
  expected := UniversalTimeToLocal(EncodeDateTime(2026, 3, 15, 8, 30, 45, 0));
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TCareLinkTimeParsingTests.ParsesPositiveOffsetWithColon;
var
  dt, expected: TDateTime;
begin
  // 12:00 at +02:00 is 10:00 UTC
  AssertTrue(ParseCareLinkTime('2026-06-01T12:00:00+02:00', dt));
  expected := UniversalTimeToLocal(EncodeDateTime(2026, 6, 1, 10, 0, 0, 0));
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TCareLinkTimeParsingTests.ParsesNegativeOffsetCompact;
var
  dt, expected: TDateTime;
begin
  // 12:00 at -0500 is 17:00 UTC
  AssertTrue(ParseCareLinkTime('2026-06-01T12:00:00-0500', dt));
  expected := UniversalTimeToLocal(EncodeDateTime(2026, 6, 1, 17, 0, 0, 0));
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TCareLinkTimeParsingTests.RejectsTooShort;
var
  dt: TDateTime;
begin
  AssertFalse(ParseCareLinkTime('2026-03-15T08:30', dt));
  AssertFalse(ParseCareLinkTime('', dt));
end;

procedure TCareLinkTimeParsingTests.RejectsGarbage;
var
  dt: TDateTime;
begin
  AssertFalse(ParseCareLinkTime('not a valid timestamp!', dt));
  AssertFalse(ParseCareLinkTime('yyyy-mm-ddThh:nn:ss', dt));
end;

procedure TCareLinkTimeParsingTests.RejectsInvalidCalendarDate;
var
  dt: TDateTime;
begin
  AssertFalse(ParseCareLinkTime('2026-13-01T00:00:00', dt)); // month 13
  AssertFalse(ParseCareLinkTime('2026-02-30T00:00:00', dt)); // Feb 30
end;

procedure TCareLinkTrendMappingTests.MapsNoneToFlat;
begin
  AssertEquals(Ord(TdFlat), Ord(CareLinkTrendToBG('NONE')));
end;

procedure TCareLinkTrendMappingTests.MapsSingleArrows;
begin
  AssertEquals(Ord(TdSingleUp), Ord(CareLinkTrendToBG('UP')));
  AssertEquals(Ord(TdSingleDown), Ord(CareLinkTrendToBG('DOWN')));
end;

procedure TCareLinkTrendMappingTests.MapsDoubleArrows;
begin
  AssertEquals(Ord(TdDoubleUp), Ord(CareLinkTrendToBG('UP_DOUBLE')));
  AssertEquals(Ord(TdDoubleDown), Ord(CareLinkTrendToBG('DOWN_DOUBLE')));
end;

procedure TCareLinkTrendMappingTests.MapsTripleArrowsToDouble;
begin
  // Trndi tops out at double arrows
  AssertEquals(Ord(TdDoubleUp), Ord(CareLinkTrendToBG('UP_TRIPLE')));
  AssertEquals(Ord(TdDoubleDown), Ord(CareLinkTrendToBG('DOWN_TRIPLE')));
end;

procedure TCareLinkTrendMappingTests.UnknownIsPlaceholder;
begin
  AssertEquals(Ord(TdPlaceholder), Ord(CareLinkTrendToBG('SIDEWAYS')));
  AssertEquals(Ord(TdPlaceholder), Ord(CareLinkTrendToBG('')));
end;

procedure TCareLinkTrendMappingTests.IsCaseAndWhitespaceInsensitive;
begin
  AssertEquals(Ord(TdSingleUp), Ord(CareLinkTrendToBG(' up ')));
  AssertEquals(Ord(TdDoubleDown), Ord(CareLinkTrendToBG('down_double')));
end;

initialization
  RegisterTest(TCareLinkTimeParsingTests);
  RegisterTest(TCareLinkTrendMappingTests);

end.
