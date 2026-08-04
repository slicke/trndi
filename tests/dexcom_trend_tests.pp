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
unit dexcom_trend_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  trndi.types, trndi.api.dexcom_helpers;

type
  TDexcomTrendMappingTests = class(TTestCase)
  published
    procedure MapsNumericZeroBased;
    procedure MapsNumericOneBased;
    procedure MapsTextualStandard;
    procedure MapsTextualCamelCase;
    procedure UnknownMapsToPlaceholder;
  end;

implementation

procedure TDexcomTrendMappingTests.MapsNumericZeroBased;
begin
  AssertEquals(Ord(TdDoubleUp), Ord(MapDexcomTrendToEnum('0')));
  AssertEquals(Ord(TdFlat), Ord(MapDexcomTrendToEnum('3')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('7')));
end;

procedure TDexcomTrendMappingTests.MapsNumericOneBased;
begin
  AssertEquals(Ord(TdSingleUp), Ord(MapDexcomTrendToEnum('1')));
  AssertEquals(Ord(TdFortyFiveDown), Ord(MapDexcomTrendToEnum('4')));
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('8')));
end;

procedure TDexcomTrendMappingTests.MapsTextualStandard;
begin
  AssertEquals(Ord(TdFlat), Ord(MapDexcomTrendToEnum('Flat')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('NOT COMPUTABLE')));
end;

procedure TDexcomTrendMappingTests.MapsTextualCamelCase;
begin
  AssertEquals(Ord(TdDoubleUp), Ord(MapDexcomTrendToEnum('DoubleUp')));
  AssertEquals(Ord(TdSingleUp), Ord(MapDexcomTrendToEnum('SingleUp')));
  AssertEquals(Ord(TdFortyFiveUp), Ord(MapDexcomTrendToEnum('FortyFiveUp')));
  AssertEquals(Ord(TdFortyFiveDown), Ord(MapDexcomTrendToEnum('FortyFiveDown')));
  AssertEquals(Ord(TdDoubleDown), Ord(MapDexcomTrendToEnum('DoubleDown')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('NotComputable')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('RateOutOfRange')));
end;

procedure TDexcomTrendMappingTests.UnknownMapsToPlaceholder;
begin
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('Banana')));
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('9')));
end;

initialization
  RegisterTest(TDexcomTrendMappingTests);

end.
