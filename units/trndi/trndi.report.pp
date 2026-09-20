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
unit trndi.report;

{**
  Descriptive statistics over a window of readings, for the summary the user
  can copy out of Trndi or save to a file.

  Deliberately free of LCL and of resourcestrings: everything here is arithmetic
  over @code(BGResults), so it is unit-testable without a widgetset and the
  wording stays in the UI layer (inc/umain_report.inc).

  Nothing here interprets a result. The numbers are the ones a CGM viewer can
  state about the samples it happens to hold — not a clinical assessment, and
  not comparable to a laboratory measurement.
}

{$mode ObjFPC}{$H+}

interface

uses
SysUtils, DateUtils, Math, trndi.types;

type
  {** Where a reading falls relative to the user's limits.

      The five bands mirror @code(BGValLevel): @code(rbLow) and @code(rbHigh)
      are the backend's clinical thresholds, @code(rbBelowRange) and
      @code(rbAboveRange) the optional personal band inside them. With the
      personal band switched off the two middle bands stay empty and
      @code(rbInRange) covers everything between low and high. }
TTrndiReportBand = (rbLow, rbBelowRange, rbInRange, rbAboveRange, rbHigh);

  {** Reading counts per band; indexes into it are exhaustive and disjoint. }
TTrndiReportBandCounts = array[TTrndiReportBand] of integer;

  {** The thresholds a report is measured against, in the report's own unit.
      Build one with @link(TrndiMakeReportLimits) rather than by hand: it
      applies the disabled-band sentinels. }
TTrndiReportLimits = record
  lo: double;        //< Clinical low; below this is @code(rbLow)
  hi: double;        //< Clinical high; above this is @code(rbHigh)
  rangeLo: double;   //< Personal lower bound; meaningless when @code(hasRange) is false
  rangeHi: double;   //< Personal upper bound; meaningless when @code(hasRange) is false
  hasRange: boolean; //< Whether the personal band is in use at all
end;

  {** One slice of the reporting window, for the shape-of-the-day sparkline. }
TTrndiReportBucket = record
  starts: TDateTime; //< When this slice begins
  count: integer;    //< Readings that fell in it (0 means a gap)
  mean: double;      //< Mean of those readings; undefined when @code(count) is 0
end;

TTrndiReportBuckets = array of TTrndiReportBucket;

  {** Everything the summary dialog prints. @code(valid) is false when the
      window held nothing to measure; every other field is then meaningless. }
TTrndiReportStats = record
  valid: boolean;              //< False when no reading fell inside the window
  units: BGUnit;               //< Unit every value below is expressed in
  limits: TTrndiReportLimits;  //< The limits the bands were cut at
  first: TDateTime;            //< Timestamp of the oldest reading used
  last: TDateTime;             //< Timestamp of the newest reading used
  spanMinutes: integer;        //< Wall-clock minutes from first to last
  count: integer;              //< Readings used
  bands: TTrndiReportBandCounts;
  mean: double;
  median: double;
  sd: double;                  //< Sample standard deviation (n-1)
  cv: double;                  //< Coefficient of variation, percent
  gmiPercent: double;          //< Glucose Management Indicator, NGSP percent
  gmiMmolMol: double;          //< The same indicator in IFCC mmol/mol
  lowest: double;
  lowestAt: TDateTime;
  highest: double;
  highestAt: TDateTime;
  cadenceMinutes: double;      //< Median spacing between consecutive readings
  coverage: double;            //< Percent of the readings the cadence implies
  longestGap: integer;         //< Widest spacing between consecutive readings; equals the cadence when nothing is missing
  longestGapAt: TDateTime;     //< The reading that spacing starts from
  lowExcursions: integer;      //< Runs of 2+ consecutive readings below `lo`
  highExcursions: integer;     //< Runs of 2+ consecutive readings above `hi`
  buckets: TTrndiReportBuckets;
end;

const
  {** Slices the reporting window is cut into for the sparkline. }
TRNDI_REPORT_BUCKETS = 24;

  {** Consecutive readings past a clinical threshold before the run counts as
      an excursion. One stray sample past a limit is noise in a CGM trace, not
      an event, and counting it as one inflates the tally on every sensor. }
TRNDI_REPORT_EXCURSION_MIN = 2;

  {** Ramp used by @link(TrndiReportSparkline), lowest bucket to highest. }
TRNDI_REPORT_RAMP: array[0..7] of string =
  ('▁', '▂', '▃', '▄', '▅', '▆', '▇', '█');

  {** Stands in for a slice of the window that holds no reading. }
TRNDI_REPORT_RAMP_GAP = '·';

  {** Build limits from the four threshold values Trndi carries, applying the
      @code(TrndiAPI) sentinels that switch the personal band off.
      @param(lo Clinical low, in @code(u))
      @param(hi Clinical high, in @code(u))
      @param(rangeLo Personal lower bound, in @code(u))
      @param(rangeHi Personal upper bound, in @code(u))
      @param(rangeEnabled False when the backend or the user has no personal band) }
function TrndiMakeReportLimits(const lo, hi, rangeLo, rangeHi: double;
  const rangeEnabled: boolean): TTrndiReportLimits;

  {** Summarise the readings that fall inside a window.
      @param(readings Any order; placeholders and out-of-window entries are dropped)
      @param(u Unit to express every result in)
      @param(limits Thresholds to cut the bands at, in @code(u))
      @param(sinceMinutes Window length back from @code(nowTime); 0 or less means all readings)
      @param(nowTime End of the window; 0 means @code(Now))
      @returns(Statistics, with @code(valid) false when the window was empty) }
function TrndiBuildReport(const readings: BGResults; const u: BGUnit;
  const limits: TTrndiReportLimits; const sinceMinutes: integer;
  const nowTime: TDateTime = 0): TTrndiReportStats;

  {** Share of the readings that fell in one band, 0..100. }
function TrndiReportBandPercent(const stats: TTrndiReportStats;
  const band: TTrndiReportBand): double;

  {** The window drawn as one line of block characters, oldest slice first.
      Scaled between the lowest and the highest slice mean, so it shows shape,
      never absolute level. Empty for an invalid report. }
function TrndiReportSparkline(const stats: TTrndiReportStats): string;

  {** Classify a single value the same way the band counts do. }
function TrndiReportBandOf(const value: double;
  const limits: TTrndiReportLimits): TTrndiReportBand;

implementation

type
  {** A reading reduced to what the arithmetic needs, sorted by time. }
TSample = record
  when: TDateTime;
  value: double;
end;
TSamples = array of TSample;

// =============================================================================
// Limits
// =============================================================================

function TrndiMakeReportLimits(const lo, hi, rangeLo, rangeHi: double;
  const rangeEnabled: boolean): TTrndiReportLimits;
begin
  Result.lo := lo;
  Result.hi := hi;
  Result.rangeLo := rangeLo;
  Result.rangeHi := rangeHi;
  // A band whose bounds sit outside the clinical thresholds, or cross each
  // other, would produce bands that overlap or that can never be entered. The
  // caller has already mapped the API's disabled sentinels to rangeEnabled;
  // this is the guard against a nonsensical pair surviving that mapping.
  Result.hasRange := rangeEnabled and (rangeLo < rangeHi) and
    (rangeLo >= lo) and (rangeHi <= hi);
end;

function TrndiReportBandOf(const value: double;
  const limits: TTrndiReportLimits): TTrndiReportBand;
begin
  if value < limits.lo then
    Exit(rbLow);
  if value > limits.hi then
    Exit(rbHigh);
  if not limits.hasRange then
    Exit(rbInRange);
  if value < limits.rangeLo then
    Exit(rbBelowRange);
  if value > limits.rangeHi then
    Exit(rbAboveRange);
  Result := rbInRange;
end;

// =============================================================================
// Collection
// =============================================================================

// Pull the usable readings out of the batch, in time order. Placeholders are
// skipped the way CalcRangeTime and PlaceTrendDots skip them: a backend that
// could not parse an entry leaves a reading with a real timestamp and no value,
// and averaging that in would drag every figure below toward BG_NO_VAL.
function CollectSamples(const readings: BGResults; const u: BGUnit;
  const cutoff, until_: TDateTime): TSamples;
var
  i, n, j: integer;
  r: BGReading;
  tmp: TSample;
begin
  Result := nil;
  SetLength(Result, Length(readings));
  n := 0;
  for i := 0 to High(readings) do
  begin
    r := readings[i];
    if r.empty then
      continue;
    if (r.date <= 0) or (r.date < cutoff) or (r.date > until_) then
      continue;
    Result[n].when := r.date;
    Result[n].value := r.convert(u);
    Inc(n);
  end;
  SetLength(Result, n);

  // Insertion sort: the caller's batch is all but sorted already (backends
  // hand back one run of readings), and n is a few hundred at most.
  for i := 1 to n - 1 do
  begin
    tmp := Result[i];
    j := i - 1;
    while (j >= 0) and (Result[j].when > tmp.when) do
    begin
      Result[j + 1] := Result[j];
      Dec(j);
    end;
    Result[j + 1] := tmp;
  end;
end;

function MinutesApart(const a, b: TDateTime): double;
begin
  Result := Abs(b - a) * MinsPerDay;
end;

// =============================================================================
// Derived figures
// =============================================================================

// Median spacing rather than mean spacing: one overnight gap in an otherwise
// five-minute trace would drag a mean far enough to make the coverage figure
// below meaningless, while the median still reports the sensor's real cadence.
function MedianCadence(const s: TSamples): double;
var
  gaps: array of double;
  i, j, n: integer;
  t: double;
begin
  n := Length(s) - 1;
  if n < 1 then
    Exit(0);
  SetLength(gaps, n);
  for i := 0 to n - 1 do
    gaps[i] := MinutesApart(s[i].when, s[i + 1].when);

  for i := 1 to n - 1 do
  begin
    t := gaps[i];
    j := i - 1;
    while (j >= 0) and (gaps[j] > t) do
    begin
      gaps[j + 1] := gaps[j];
      Dec(j);
    end;
    gaps[j + 1] := t;
  end;

  if Odd(n) then
    Result := gaps[n div 2]
  else
    Result := (gaps[(n div 2) - 1] + gaps[n div 2]) / 2;
end;

function MedianValue(const s: TSamples): double;
var
  vals: array of double;
  i, j, n: integer;
  t: double;
begin
  n := Length(s);
  if n = 0 then
    Exit(0);
  SetLength(vals, n);
  for i := 0 to n - 1 do
    vals[i] := s[i].value;
  for i := 1 to n - 1 do
  begin
    t := vals[i];
    j := i - 1;
    while (j >= 0) and (vals[j] > t) do
    begin
      vals[j + 1] := vals[j];
      Dec(j);
    end;
    vals[j + 1] := t;
  end;
  if Odd(n) then
    Result := vals[n div 2]
  else
    Result := (vals[(n div 2) - 1] + vals[n div 2]) / 2;
end;

// The Glucose Management Indicator (Bergenstal et al., 2018), which restates a
// CGM mean on the scale people already read their A1c on. It is a restatement
// of *this* mean over *this* window and nothing else -- not a laboratory value,
// and not an estimate of one. The caller labels it accordingly.
procedure ComputeGMI(const meanValue: double; const u: BGUnit;
  out ngsp, ifcc: double);
var
  asMgdl: double;
begin
  if u = BGUnit.mgdl then
    asMgdl := meanValue
  else
    // BG_CONVERTIONS is indexed [target][source]: the factor that turns a
    // mmol/L figure into mg/dL is the one in the mg/dL row.
    asMgdl := meanValue * BG_CONVERTIONS[BGUnit.mgdl][BGUnit.mmol];
  ngsp := 3.31 + (0.02392 * asMgdl);
  // The NGSP-to-IFCC conversion, so the figure can be read on either scale.
  ifcc := (ngsp - 2.15) * 10.929;
end;

// Runs of consecutive readings past a threshold. Counted on consecutive
// samples, not on elapsed time: a run interrupted by a sensor gap is two runs
// here, which understates rather than invents events.
procedure CountExcursions(const s: TSamples; const limits: TTrndiReportLimits;
  out lows, highs: integer);
var
  i, lowRun, highRun: integer;
begin
  lows := 0;
  highs := 0;
  lowRun := 0;
  highRun := 0;
  for i := 0 to High(s) do
  begin
    if s[i].value < limits.lo then
    begin
      Inc(lowRun);
      if lowRun = TRNDI_REPORT_EXCURSION_MIN then
        Inc(lows);
    end
    else
      lowRun := 0;

    if s[i].value > limits.hi then
    begin
      Inc(highRun);
      if highRun = TRNDI_REPORT_EXCURSION_MIN then
        Inc(highs);
    end
    else
      highRun := 0;
  end;
end;

procedure BuildBuckets(const s: TSamples; const first, last: TDateTime;
  out buckets: TTrndiReportBuckets);
var
  i, idx: integer;
  width: double;
  sums: array[0..TRNDI_REPORT_BUCKETS - 1] of double;
begin
  SetLength(buckets, TRNDI_REPORT_BUCKETS);
  width := (last - first) / TRNDI_REPORT_BUCKETS;
  for i := 0 to TRNDI_REPORT_BUCKETS - 1 do
  begin
    sums[i] := 0;
    buckets[i].count := 0;
    buckets[i].mean := 0;
    if width > 0 then
      buckets[i].starts := first + (width * i)
    else
      buckets[i].starts := first;
  end;

  for i := 0 to High(s) do
  begin
    if width > 0 then
      // Floor, then clamp: the newest reading sits exactly on the far edge and
      // would otherwise index one past the last slice.
      idx := Min(TRNDI_REPORT_BUCKETS - 1, Max(0, Floor((s[i].when - first) / width)))
    else
      idx := 0;
    sums[idx] := sums[idx] + s[i].value;
    Inc(buckets[idx].count);
  end;

  for i := 0 to TRNDI_REPORT_BUCKETS - 1 do
    if buckets[i].count > 0 then
      buckets[i].mean := sums[i] / buckets[i].count;
end;

// =============================================================================
// Report
// =============================================================================

function TrndiBuildReport(const readings: BGResults; const u: BGUnit;
  const limits: TTrndiReportLimits; const sinceMinutes: integer;
  const nowTime: TDateTime = 0): TTrndiReportStats;
var
  s: TSamples;
  i: integer;
  cutoff, until_: TDateTime;
  sum, sqsum, gap: double;
  expected: double;
begin
  Result := Default(TTrndiReportStats);
  Result.units := u;
  Result.limits := limits;

  until_ := nowTime;
  if until_ <= 0 then
    until_ := Now;
  if sinceMinutes > 0 then
    cutoff := IncMinute(until_, -sinceMinutes)
  else
    cutoff := 0;

  s := CollectSamples(readings, u, cutoff, until_);
  if Length(s) = 0 then
    Exit;

  Result.valid := true;
  Result.count := Length(s);
  Result.first := s[0].when;
  Result.last := s[High(s)].when;
  Result.spanMinutes := Round(MinutesApart(Result.first, Result.last));

  sum := 0;
  Result.lowest := s[0].value;
  Result.lowestAt := s[0].when;
  Result.highest := s[0].value;
  Result.highestAt := s[0].when;
  for i := 0 to High(s) do
  begin
    sum := sum + s[i].value;
    Inc(Result.bands[TrndiReportBandOf(s[i].value, limits)]);
    if s[i].value < Result.lowest then
    begin
      Result.lowest := s[i].value;
      Result.lowestAt := s[i].when;
    end;
    if s[i].value > Result.highest then
    begin
      Result.highest := s[i].value;
      Result.highestAt := s[i].when;
    end;
  end;
  Result.mean := sum / Result.count;
  Result.median := MedianValue(s);

  // Sample standard deviation (n-1): these readings are a sample of the wearer's
  // glucose over the window, not the whole of it, and the CV below is compared
  // against figures that are quoted the same way.
  if Result.count > 1 then
  begin
    sqsum := 0;
    for i := 0 to High(s) do
      sqsum := sqsum + Sqr(s[i].value - Result.mean);
    Result.sd := Sqrt(sqsum / (Result.count - 1));
  end
  else
    Result.sd := 0;

  if Result.mean <> 0 then
    Result.cv := (Result.sd / Result.mean) * 100
  else
    Result.cv := 0;

  ComputeGMI(Result.mean, u, Result.gmiPercent, Result.gmiMmolMol);
  CountExcursions(s, limits, Result.lowExcursions, Result.highExcursions);

  Result.cadenceMinutes := MedianCadence(s);
  for i := 0 to High(s) - 1 do
  begin
    gap := MinutesApart(s[i].when, s[i + 1].when);
    if Round(gap) > Result.longestGap then
    begin
      Result.longestGap := Round(gap);
      Result.longestGapAt := s[i].when;
    end;
  end;

  // How much of the window the sensor actually covered, judged against the
  // cadence the sensor itself set rather than an assumed five minutes -- the
  // backends disagree (Libre reports once a minute, Dexcom once every five),
  // and a fixed divisor would report a complete trace as a third of one.
  if Result.cadenceMinutes > 0 then
  begin
    expected := (Result.spanMinutes / Result.cadenceMinutes) + 1;
    if expected > 0 then
      Result.coverage := Min(100, (Result.count / expected) * 100);
  end
  else
    Result.coverage := 100;

  BuildBuckets(s, Result.first, Result.last, Result.buckets);
end;

function TrndiReportBandPercent(const stats: TTrndiReportStats;
  const band: TTrndiReportBand): double;
begin
  if (not stats.valid) or (stats.count = 0) then
    Exit(0);
  Result := (stats.bands[band] / stats.count) * 100;
end;

function TrndiReportSparkline(const stats: TTrndiReportStats): string;
var
  i, step: integer;
  lo, hi: double;
  seen: boolean;
begin
  Result := '';
  if (not stats.valid) or (Length(stats.buckets) = 0) then
    Exit;

  seen := false;
  lo := 0;
  hi := 0;
  for i := 0 to High(stats.buckets) do
    if stats.buckets[i].count > 0 then
    begin
      if (not seen) or (stats.buckets[i].mean < lo) then
        lo := stats.buckets[i].mean;
      if (not seen) or (stats.buckets[i].mean > hi) then
        hi := stats.buckets[i].mean;
      seen := true;
    end;
  if not seen then
    Exit;

  for i := 0 to High(stats.buckets) do
    if stats.buckets[i].count = 0 then
      Result := Result + TRNDI_REPORT_RAMP_GAP
    else
    begin
      // A flat window has no range to scale against; put it in the middle
      // rather than pinning every slice to the floor of the ramp.
      if hi <= lo then
        step := High(TRNDI_REPORT_RAMP) div 2
      else
        step := Round(((stats.buckets[i].mean - lo) / (hi - lo)) *
          High(TRNDI_REPORT_RAMP));
      Result := Result + TRNDI_REPORT_RAMP[step];
    end;
end;

end.
