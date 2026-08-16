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
    procedure MapsNumericDexcomCodes;
    procedure MapsNumericEdgeCodes;
    procedure MapsTextualStandard;
    procedure MapsTextualCamelCase;
    procedure UnknownMapsToPlaceholder;
    procedure AuthFailureMessages;
  end;

implementation

procedure TDexcomTrendMappingTests.MapsNumericDexcomCodes;
begin
  // Dexcom Share's legacy integer codes, which are BGTrend's order shifted by
  // one: 1=DoubleUp .. 7=DoubleDown, 8=NotComputable. Verified against
  // pydexcom's DEXCOM_TREND_DIRECTIONS and the trend table in
  // FreemanConsultingServices/dexcom-tesla-display.
  //
  // These used to be read 0-based, so every arrow came out one step off -- a
  // Dexcom 4 ("Flat") displayed as TdFortyFiveDown. The old tests asserted
  // that skew as correct, which is why it survived.
  AssertEquals(Ord(TdDoubleUp), Ord(MapDexcomTrendToEnum('1')));
  AssertEquals(Ord(TdSingleUp), Ord(MapDexcomTrendToEnum('2')));
  AssertEquals(Ord(TdFortyFiveUp), Ord(MapDexcomTrendToEnum('3')));
  AssertEquals(Ord(TdFlat), Ord(MapDexcomTrendToEnum('4')));
  AssertEquals(Ord(TdFortyFiveDown), Ord(MapDexcomTrendToEnum('5')));
  AssertEquals(Ord(TdSingleDown), Ord(MapDexcomTrendToEnum('6')));
  AssertEquals(Ord(TdDoubleDown), Ord(MapDexcomTrendToEnum('7')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('8')));
end;

procedure TDexcomTrendMappingTests.MapsNumericEdgeCodes;
begin
  // 0 is Dexcom's "None" -- absence of a trend, not an arrow.
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('0')));
  // 9 is RateOutOfRange, which follows its textual alias to TdNotComputable
  // rather than degrading to a placeholder.
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('9')));
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('10')));
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('-1')));
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
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('')));
end;

procedure TDexcomTrendMappingTests.AuthFailureMessages;
var
  msg: string;
begin
  // The codes pydexcom's _handle_error_code singles out. These must be
  // recognized as terminal credential problems, never as recoverable session
  // failures -- retrying them costs a second failed sign-in per poll and walks
  // the account toward SSO_AuthenticateMaxAttemptsExceeded.
  AssertTrue('AccountPasswordInvalid recognized',
    DexcomAuthFailureMessage('{"Code":"AccountPasswordInvalid"}', msg));
  AssertTrue('AccountPasswordInvalid has a message', msg <> '');

  AssertTrue('Max attempts recognized',
    DexcomAuthFailureMessage('{"Code":"SSO_AuthenticateMaxAttemptsExceeded"}', msg));
  AssertTrue('Max attempts has a distinct message',
    Pos('locked', LowerCase(msg)) > 0);

  AssertTrue('SSO_InternalError with authenticate message recognized',
    DexcomAuthFailureMessage(
    '{"Code":"SSO_InternalError","Message":"Cannot Authenticate by AccountName"}',
    msg));

  // SSO_InternalError on its own is a server fault, not a credential problem.
  AssertFalse('Bare SSO_InternalError not treated as a credential failure',
    DexcomAuthFailureMessage('{"Code":"SSO_InternalError"}', msg));
  AssertFalse('Session errors are not credential failures',
    DexcomAuthFailureMessage('{"Code":"SessionIdNotFound"}', msg));
  AssertFalse('Empty response', DexcomAuthFailureMessage('', msg));
end;

initialization
  RegisterTest(TDexcomTrendMappingTests);

end.
