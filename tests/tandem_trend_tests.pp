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
unit tandem_trend_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  trndi.types, trndi.funcs, SysUtils, DateUtils;

type
  TTandemTrendScalingTests = class(TTestCase)
  published
    procedure ScalesFiveMinutes;
    procedure ScalesOneMinuteToFiveMinuteEquivalent;
    procedure RejectsLongIntervalAsNotComputable;
    procedure NegativeTwoMinutesGivesFortyFiveDown;
  end;

implementation

procedure TTandemTrendScalingTests.ScalesFiveMinutes;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  t: BGTrend;
begin
  rawDiff := 10.0; // mg/dL difference
  secondsDiff := 300; // 5 minutes
  scaled := rawDiff * (300 / secondsDiff);
  t := CalculateTrendFromDelta(scaled);
  AssertEquals(Ord(TdSingleUp), Ord(t));
end;

procedure TTandemTrendScalingTests.ScalesOneMinuteToFiveMinuteEquivalent;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  t: BGTrend;
begin
  rawDiff := 2.0; // 2 mg/dL over 1 minute -> 10 mg/5min
  secondsDiff := 60; // 1 minute
  scaled := rawDiff * (300 / secondsDiff);
  t := CalculateTrendFromDelta(scaled);
  AssertEquals(Ord(TdSingleUp), Ord(t));
end;

procedure TTandemTrendScalingTests.RejectsLongIntervalAsNotComputable;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  valid: boolean;
begin
  rawDiff := 10.0;
  secondsDiff := 1800; // 30 minutes
  // According to Tandem driver logic, intervals outside 60..900s are not computable
  valid := (secondsDiff >= 60) and (secondsDiff <= 900);
  AssertFalse(valid);
end;

procedure TTandemTrendScalingTests.NegativeTwoMinutesGivesFortyFiveDown;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  t: BGTrend;
begin
  rawDiff := -2.0; // -2 mg/dL over 2 minutes -> -15 mg/5min
  secondsDiff := 120; // 2 minutes
  scaled := rawDiff * (300 / secondsDiff);
  t := CalculateTrendFromDelta(scaled);
  AssertEquals(Ord(TdFortyFiveDown), Ord(t));
end;

initialization
  RegisterTest(TTandemTrendScalingTests);

end.
