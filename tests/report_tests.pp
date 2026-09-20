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
unit report_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math,
  fpcunit, testregistry,
  trndi.types, trndi.report;

type
  {** Tests for the summary statistics behind the report dialog.

    Every case pins the end of the reporting window explicitly (TrndiBuildReport
    takes nowTime) and builds its readings backwards from that instant, so the
    suite never reads the wall clock and cannot drift across a midnight or a
    slow machine. }
  TReportTests = class(TTestCase)
  private
    FNow: TDateTime;
    {** A trace of `count` readings ending at FNow, `everyMin` apart, taking each
      value from `values` in turn (cycled if the trace is longer). }
    function Trace(const values: array of double; const everyMin: integer;
      const u: BGUnit = BGUnit.mmol): BGResults;
    function Reading(const value: double; const at: TDateTime;
      const u: BGUnit = BGUnit.mmol): BGReading;
    function Placeholder(const at: TDateTime): BGReading;
    function MmolLimits: TTrndiReportLimits;
  protected
    procedure SetUp; override;
  published
    // Window and input handling
    procedure EmptyInputIsNotValid;
    procedure OnlyPlaceholdersIsNotValid;
    procedure PlaceholdersAreExcludedFromTheMean;
    procedure ReadingsOlderThanTheWindowAreDropped;
    procedure ReadingsAfterTheWindowAreDropped;
    procedure ZeroWindowKeepsEverything;
    procedure UnsortedInputIsOrderedByTime;
    // Bands
    procedure BandsSplitAtTheClinicalThresholds;
    procedure PersonalRangeSplitsTheMiddleBand;
    procedure DisabledPersonalRangeLeavesOneMiddleBand;
    procedure CrossedPersonalRangeIsIgnored;
    procedure BandPercentagesSumToAHundred;
    // Central figures
    procedure MeanAndMedianOverAnEvenCount;
    procedure MedianIgnoresAnOutlierTheMeanFollows;
    procedure StandardDeviationIsTheSampleForm;
    procedure SingleReadingHasNoSpread;
    procedure CoefficientOfVariationIsSdOverMean;
    procedure GmiRestatesTheMeanOnTheA1cScale;
    procedure GmiIsUnitIndependent;
    // Coverage and gaps
    procedure CompleteTraceIsFullyCovered;
    procedure MissingReadingsLowerCoverage;
    procedure CadenceFollowsTheSensorNotFiveMinutes;
    procedure LongestGapIsReportedWithItsStart;
    // Excursions
    procedure OneStrayReadingIsNotAnExcursion;
    procedure TwoConsecutiveReadingsAreOneExcursion;
    procedure SeparatedRunsCountSeparately;
    procedure HighAndLowExcursionsAreCountedApart;
    // Sparkline
    procedure SparklineHasOneCellPerBucket;
    procedure SparklineMarksEmptyBuckets;
    procedure SparklineRisesWithTheTrace;
    procedure FlatTraceSitsMidRamp;
    procedure InvalidReportHasNoSparkline;
  end;

implementation

// The sparkline mixes three-byte ramp glyphs with a two-byte gap marker, so
// the assertions below count and index cells, not bytes. Written here rather
// than pulled from LazUTF8: the test runner builds against the LCL mocks.
function CellCount(const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if (Ord(s[i]) and $C0) <> $80 then
      Inc(Result);
end;

function CellAt(const s: string; const index_: integer): string;
var
  i, seen, start: integer;
begin
  Result := '';
  seen := 0;
  start := 0;
  for i := 1 to Length(s) do
    if (Ord(s[i]) and $C0) <> $80 then
    begin
      Inc(seen);
      if seen = index_ then
        start := i
      else if seen = index_ + 1 then
        Exit(Copy(s, start, i - start));
    end;
  if start > 0 then
    Result := Copy(s, start, MaxInt);
end;

const
  // The window every test measures, and the limits the bands are cut at.
  LO = 4.0;
  HI = 10.0;
  RANGE_LO = 4.5;
  RANGE_HI = 8.0;

procedure TReportTests.SetUp;
begin
  // A fixed instant, not Now: the window arithmetic is what is under test.
  FNow := EncodeDateTime(2026, 3, 14, 12, 0, 0, 0);
end;

function TReportTests.Reading(const value: double; const at: TDateTime;
  const u: BGUnit = BGUnit.mmol): BGReading;
begin
  Result.Init(u, u, 'test');
  Result.update(value, BGPrimary, u);
  Result.date := at;
  Result.trend := TdFlat;
end;

function TReportTests.Placeholder(const at: TDateTime): BGReading;
begin
  Result.Init(BGUnit.mmol, BGUnit.mmol, 'test');
  Result.Clear;
  Result.date := at;
  Result.trend := TdPlaceholder;
end;

function TReportTests.Trace(const values: array of double;
  const everyMin: integer; const u: BGUnit = BGUnit.mmol): BGResults;
var
  i, n: integer;
begin
  n := Length(values);
  SetLength(Result, n);
  // Oldest first, newest landing exactly on FNow.
  for i := 0 to n - 1 do
    Result[i] := Reading(values[i], IncMinute(FNow, -everyMin * (n - 1 - i)), u);
end;

function TReportTests.MmolLimits: TTrndiReportLimits;
begin
  Result := TrndiMakeReportLimits(LO, HI, RANGE_LO, RANGE_HI, true);
end;

// =============================================================================
// Window and input handling
// =============================================================================

procedure TReportTests.EmptyInputIsNotValid;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(nil, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertFalse('empty input must not produce a report', st.valid);
  AssertEquals('no readings counted', 0, st.count);
end;

procedure TReportTests.OnlyPlaceholdersIsNotValid;
var
  bgs: BGResults;
  st: TTrndiReportStats;
begin
  SetLength(bgs, 2);
  bgs[0] := Placeholder(IncMinute(FNow, -10));
  bgs[1] := Placeholder(IncMinute(FNow, -5));
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertFalse('a window of gaps has nothing to measure', st.valid);
end;

procedure TReportTests.PlaceholdersAreExcludedFromTheMean;
var
  bgs: BGResults;
  st: TTrndiReportStats;
begin
  SetLength(bgs, 3);
  bgs[0] := Reading(6.0, IncMinute(FNow, -10));
  bgs[1] := Placeholder(IncMinute(FNow, -5));
  bgs[2] := Reading(8.0, FNow);
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('only the two real readings count', 2, st.count);
  AssertEquals('the placeholder must not drag the mean', 7.0, st.mean, 0.001);
end;

procedure TReportTests.ReadingsOlderThanTheWindowAreDropped;
var
  bgs: BGResults;
  st: TTrndiReportStats;
begin
  SetLength(bgs, 2);
  bgs[0] := Reading(5.0, IncMinute(FNow, -400));
  bgs[1] := Reading(9.0, IncMinute(FNow, -10));
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('only the reading inside three hours', 1, st.count);
  AssertEquals(9.0, st.mean, 0.001);
end;

procedure TReportTests.ReadingsAfterTheWindowAreDropped;
var
  bgs: BGResults;
  st: TTrndiReportStats;
begin
  // A backend with a skewed clock can hand back a reading stamped in the
  // future; it must not extend the window it is reported over.
  SetLength(bgs, 2);
  bgs[0] := Reading(6.0, IncMinute(FNow, -5));
  bgs[1] := Reading(20.0, IncMinute(FNow, 30));
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('the future reading is not measured', 1, st.count);
  AssertEquals(6.0, st.mean, 0.001);
end;

procedure TReportTests.ZeroWindowKeepsEverything;
var
  bgs: BGResults;
  st: TTrndiReportStats;
begin
  SetLength(bgs, 2);
  bgs[0] := Reading(5.0, IncMinute(FNow, -4000));
  bgs[1] := Reading(9.0, IncMinute(FNow, -10));
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 0, FNow);
  AssertEquals('no window means every reading', 2, st.count);
end;

procedure TReportTests.UnsortedInputIsOrderedByTime;
var
  bgs: BGResults;
  st: TTrndiReportStats;
begin
  SetLength(bgs, 3);
  bgs[0] := Reading(7.0, IncMinute(FNow, -5));
  bgs[1] := Reading(5.0, IncMinute(FNow, -15));
  bgs[2] := Reading(6.0, IncMinute(FNow, -10));
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('oldest reading opens the window',
    MinutesBetween(FNow, IncMinute(FNow, -15)), MinutesBetween(FNow, st.first));
  AssertEquals('newest reading closes it',
    MinutesBetween(FNow, IncMinute(FNow, -5)), MinutesBetween(FNow, st.last));
  AssertEquals('span runs first to last', 10, st.spanMinutes);
end;

// =============================================================================
// Bands
// =============================================================================

procedure TReportTests.BandsSplitAtTheClinicalThresholds;
var
  st: TTrndiReportStats;
  limits: TTrndiReportLimits;
begin
  limits := TrndiMakeReportLimits(LO, HI, 0, 0, false);
  // Exactly on a threshold is inside it: only strictly past counts as out.
  st := TrndiBuildReport(Trace([3.9, 4.0, 10.0, 10.1], 5), BGUnit.mmol, limits, 180, FNow);
  AssertEquals('below low', 1, st.bands[rbLow]);
  AssertEquals('above high', 1, st.bands[rbHigh]);
  AssertEquals('both boundary readings are in range', 2, st.bands[rbInRange]);
end;

procedure TReportTests.PersonalRangeSplitsTheMiddleBand;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([4.2, 6.0, 9.0], 5), BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('under the personal band', 1, st.bands[rbBelowRange]);
  AssertEquals('inside it', 1, st.bands[rbInRange]);
  AssertEquals('over it', 1, st.bands[rbAboveRange]);
  AssertEquals('none past the clinical low', 0, st.bands[rbLow]);
  AssertEquals('none past the clinical high', 0, st.bands[rbHigh]);
end;

procedure TReportTests.DisabledPersonalRangeLeavesOneMiddleBand;
var
  st: TTrndiReportStats;
  limits: TTrndiReportLimits;
begin
  limits := TrndiMakeReportLimits(LO, HI, 0, 0, false);
  st := TrndiBuildReport(Trace([4.2, 6.0, 9.0], 5), BGUnit.mmol, limits, 180, FNow);
  AssertEquals('everything between the thresholds is simply in range',
    3, st.bands[rbInRange]);
  AssertEquals(0, st.bands[rbBelowRange]);
  AssertEquals(0, st.bands[rbAboveRange]);
end;

procedure TReportTests.CrossedPersonalRangeIsIgnored;
var
  limits: TTrndiReportLimits;
begin
  // A band whose lower bound sits above its upper one would carve bands that
  // overlap; the whole band is dropped rather than half-applied.
  limits := TrndiMakeReportLimits(LO, HI, 9.0, 5.0, true);
  AssertFalse('crossed bounds switch the band off', limits.hasRange);
  // Likewise a band that reaches outside the thresholds it lives inside.
  limits := TrndiMakeReportLimits(LO, HI, 3.0, 8.0, true);
  AssertFalse('a band below the clinical low is not a band', limits.hasRange);
end;

procedure TReportTests.BandPercentagesSumToAHundred;
var
  st: TTrndiReportStats;
  b: TTrndiReportBand;
  total: double;
begin
  st := TrndiBuildReport(Trace([3.0, 4.2, 6.0, 9.0, 12.0], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  total := 0;
  for b := Low(TTrndiReportBand) to High(TTrndiReportBand) do
    total := total + TrndiReportBandPercent(st, b);
  AssertEquals('the bands partition the readings', 100.0, total, 0.001);
end;

// =============================================================================
// Central figures
// =============================================================================

procedure TReportTests.MeanAndMedianOverAnEvenCount;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([4.0, 6.0, 8.0, 10.0], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals(7.0, st.mean, 0.001);
  AssertEquals('median of an even count averages the two middles',
    7.0, st.median, 0.001);
end;

procedure TReportTests.MedianIgnoresAnOutlierTheMeanFollows;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([5.0, 5.0, 5.0, 5.0, 25.0], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals(9.0, st.mean, 0.001);
  AssertEquals(5.0, st.median, 0.001);
end;

procedure TReportTests.StandardDeviationIsTheSampleForm;
var
  st: TTrndiReportStats;
begin
  // 2,4,4,4,5,5,7,9: population SD is 2, sample SD (n-1) is ~2.138.
  st := TrndiBuildReport(Trace([2, 4, 4, 4, 5, 5, 7, 9], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals(5.0, st.mean, 0.001);
  AssertEquals(2.13809, st.sd, 0.0001);
end;

procedure TReportTests.SingleReadingHasNoSpread;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([6.0], 5), BGUnit.mmol, MmolLimits, 180, FNow);
  AssertTrue('one reading is still a report', st.valid);
  AssertEquals('no second reading to vary from', 0.0, st.sd, 0.0001);
  AssertEquals(0.0, st.cv, 0.0001);
  AssertEquals('a single reading spans no time', 0, st.spanMinutes);
end;

procedure TReportTests.CoefficientOfVariationIsSdOverMean;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([2, 4, 4, 4, 5, 5, 7, 9], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals((st.sd / st.mean) * 100, st.cv, 0.0001);
end;

procedure TReportTests.GmiRestatesTheMeanOnTheA1cScale;
var
  st: TTrndiReportStats;
begin
  // The published worked example: a mean of 154 mg/dL restates as 7.0%.
  st := TrndiBuildReport(Trace([154, 154], 5, BGUnit.mgdl), BGUnit.mgdl,
    TrndiMakeReportLimits(70, 180, 0, 0, false), 180, FNow);
  AssertEquals(154.0, st.mean, 0.001);
  AssertEquals(7.0, st.gmiPercent, 0.01);
  AssertEquals('and on the IFCC scale', 53.0, st.gmiMmolMol, 0.5);
end;

procedure TReportTests.GmiIsUnitIndependent;
var
  inMgdl, inMmol: TTrndiReportStats;
begin
  // Same physiology, different display unit: the indicator must not move.
  inMgdl := TrndiBuildReport(Trace([180, 180], 5, BGUnit.mgdl), BGUnit.mgdl,
    TrndiMakeReportLimits(70, 180, 0, 0, false), 180, FNow);
  inMmol := TrndiBuildReport(Trace([180, 180], 5, BGUnit.mgdl), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals(inMgdl.gmiPercent, inMmol.gmiPercent, 0.01);
end;

// =============================================================================
// Coverage and gaps
// =============================================================================

procedure TReportTests.CompleteTraceIsFullyCovered;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6], 5),
    BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('an unbroken trace covers its window', 100.0, st.coverage, 0.001);
  AssertEquals('with nothing missing, the widest spacing is just the cadence',
    5, st.longestGap);
end;

procedure TReportTests.MissingReadingsLowerCoverage;
var
  bgs: BGResults;
  st: TTrndiReportStats;
  i, n: integer;
begin
  // A five-minute trace with a 40-minute hole bitten out of the middle: nine
  // readings, the outage, then five more.
  SetLength(bgs, 14);
  n := 0;
  for i := 0 to 8 do
  begin
    bgs[n] := Reading(6.0, IncMinute(FNow, -(100 - i * 5)));
    Inc(n);
  end;
  for i := 0 to 4 do
  begin
    bgs[n] := Reading(6.0, IncMinute(FNow, -(20 - i * 5)));
    Inc(n);
  end;
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('the outage must not move the cadence', 5.0,
    st.cadenceMinutes, 0.001);
  AssertEquals('the outage is the widest hole', 40, st.longestGap);
  // 100 minutes at five-minute cadence would be 21 readings; 14 arrived.
  AssertEquals(100.0 * 14 / 21, st.coverage, 0.5);
end;

procedure TReportTests.CadenceFollowsTheSensorNotFiveMinutes;
var
  st: TTrndiReportStats;
begin
  // A one-minute sensor reporting without a break is complete, not 20% covered.
  st := TrndiBuildReport(Trace([6, 6, 6, 6, 6, 6, 6, 6, 6, 6], 1), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals(1.0, st.cadenceMinutes, 0.001);
  AssertEquals(100.0, st.coverage, 0.001);
end;

procedure TReportTests.LongestGapIsReportedWithItsStart;
var
  bgs: BGResults;
  st: TTrndiReportStats;
begin
  SetLength(bgs, 4);
  bgs[0] := Reading(6.0, IncMinute(FNow, -120));
  bgs[1] := Reading(6.0, IncMinute(FNow, -115));
  bgs[2] := Reading(6.0, IncMinute(FNow, -40));   // 75-minute hole opens here
  bgs[3] := Reading(6.0, IncMinute(FNow, -35));
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals(75, st.longestGap);
  AssertEquals('the gap is dated from the reading before it',
    MinutesBetween(FNow, IncMinute(FNow, -115)),
    MinutesBetween(FNow, st.longestGapAt));
end;

// =============================================================================
// Excursions
// =============================================================================

procedure TReportTests.OneStrayReadingIsNotAnExcursion;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([6, 6, 12, 6, 6], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals('a single sample past the limit is noise', 0, st.highExcursions);
end;

procedure TReportTests.TwoConsecutiveReadingsAreOneExcursion;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([6, 12, 12, 12, 6], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals('one run, counted once', 1, st.highExcursions);
end;

procedure TReportTests.SeparatedRunsCountSeparately;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([12, 12, 6, 6, 12, 12], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals(2, st.highExcursions);
end;

procedure TReportTests.HighAndLowExcursionsAreCountedApart;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([3, 3, 6, 6, 12, 12, 12], 5), BGUnit.mmol,
    MmolLimits, 180, FNow);
  AssertEquals(1, st.lowExcursions);
  AssertEquals(1, st.highExcursions);
end;

// =============================================================================
// Sparkline
// =============================================================================

procedure TReportTests.SparklineHasOneCellPerBucket;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(Trace([5, 6, 7, 8, 9, 8, 7, 6], 15), BGUnit.mmol,
    MmolLimits, 0, FNow);
  AssertEquals('one cell per slice of the window',
    TRNDI_REPORT_BUCKETS, CellCount(TrndiReportSparkline(st)));
end;

procedure TReportTests.SparklineMarksEmptyBuckets;
var
  bgs: BGResults;
  st: TTrndiReportStats;
  line: string;
begin
  // Two readings at the ends of a long window: every slice between them is a
  // hole and must read as one, not as a value invented to fill it.
  SetLength(bgs, 2);
  bgs[0] := Reading(5.0, IncMinute(FNow, -240));
  bgs[1] := Reading(9.0, FNow);
  st := TrndiBuildReport(bgs, BGUnit.mmol, MmolLimits, 0, FNow);
  line := TrndiReportSparkline(st);
  AssertTrue('the middle of the window is drawn as gaps',
    Pos(TRNDI_REPORT_RAMP_GAP, line) > 0);
end;

procedure TReportTests.SparklineRisesWithTheTrace;
var
  st: TTrndiReportStats;
  line: string;
  firstCell, lastCell: string;
begin
  st := TrndiBuildReport(Trace([4, 5, 6, 7, 8, 9, 10, 11], 30), BGUnit.mmol,
    MmolLimits, 0, FNow);
  line := TrndiReportSparkline(st);
  firstCell := CellAt(line, 1);
  lastCell := CellAt(line, CellCount(line));
  AssertEquals('a rising trace starts at the floor of the ramp',
    TRNDI_REPORT_RAMP[Low(TRNDI_REPORT_RAMP)], firstCell);
  AssertEquals('and ends at its top',
    TRNDI_REPORT_RAMP[High(TRNDI_REPORT_RAMP)], lastCell);
end;

procedure TReportTests.FlatTraceSitsMidRamp;
var
  st: TTrndiReportStats;
  line: string;
begin
  st := TrndiBuildReport(Trace([6, 6, 6, 6, 6, 6, 6, 6], 30), BGUnit.mmol,
    MmolLimits, 0, FNow);
  line := TrndiReportSparkline(st);
  AssertEquals('a level window has no shape to scale, so it draws level',
    TRNDI_REPORT_RAMP[High(TRNDI_REPORT_RAMP) div 2], CellAt(line, 1));
end;

procedure TReportTests.InvalidReportHasNoSparkline;
var
  st: TTrndiReportStats;
begin
  st := TrndiBuildReport(nil, BGUnit.mmol, MmolLimits, 180, FNow);
  AssertEquals('', TrndiReportSparkline(st));
end;

initialization
  RegisterTest(TReportTests);

end.
