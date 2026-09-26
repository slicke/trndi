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
(* The summary report: the readings Trndi currently holds, reduced to the
   figures a wearer would otherwise work out by hand, shown in a dialog and
   saveable as a plain text file.

   The arithmetic is in units/trndi/trndi.report.pp, which knows nothing about
   the LCL; everything here is wording and layout.
   Included in the implementation section of umain.pp only.
*)

const
  {** Width of the distribution bars, and what one cell of them is worth. }
REPORT_BAR_CELLS = 10;
REPORT_BAR_PERCENT = 100 / REPORT_BAR_CELLS;
  {** Rows the HTML shape chart is drawn in. }
REPORT_SHAPE_ROWS = 4;
  {** How much wider than the sensor's own spacing a hole has to be before it
      is worth naming. Below this it is jitter, and calling it "the longest
      gap" invents an outage the user does not have. }
REPORT_GAP_FACTOR = 1.5;

// Hex for the HTML dialog. TColorToHTML inside slicke.ux.alert is private to
// that unit, and TColor is $00bbggrr, so the channels come back out by hand.
function ReportColorHex(const c: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x',
    [c and $FF, (c shr 8) and $FF, (c shr 16) and $FF]);
end;

// Hard spaces: the renderer collapses ordinary ones, so padding inside a
// monospace run has to be spelled out.
function ReportHTMLNbsp(const count: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to count do
    Result := Result + '&nbsp;';
end;

// A run of `filled` marks padded out to `width`, for a bar that keeps its
// alignment inside <tt> whatever the label beside it is.
function ReportBar(const filled, width: integer; const color: TColor): string;
begin
  Result := Format('<tt><font color="%s">%s</font>%s</tt>',
    [ReportColorHex(color), StringOfChar('#', filled),
    ReportHTMLNbsp(width - filled)]);
end;

// The shape of the period, as a small chart of monospace cells: one column per
// slice of the window, its height the slice's mean and its colour the band that
// mean falls in.
//
// Not the block-character sparkline the text form uses. TIpHtmlPanel renders
// the dialogs, and it cannot draw anything above Latin-1 -- U+2581 and its
// neighbours come out as empty boxes whether they are passed as UTF-8 or as a
// numeric character reference -- so the on-screen form is built from marks that
// every font has.
function ReportShapeHTML(const st: TTrndiReportStats;
  const colors: TReportBandColors): string;
var
  row, i, level: integer;
  lo, hi: double;
  seen: boolean;
  line: string;
begin
  Result := '';
  if Length(st.buckets) = 0 then
    Exit;

  seen := false;
  lo := 0;
  hi := 0;
  for i := 0 to High(st.buckets) do
    if st.buckets[i].count > 0 then
    begin
      if (not seen) or (st.buckets[i].mean < lo) then
        lo := st.buckets[i].mean;
      if (not seen) or (st.buckets[i].mean > hi) then
        hi := st.buckets[i].mean;
      seen := true;
    end;
  if not seen then
    Exit;

  // Top row down, so the highest slices are drawn first.
  for row := REPORT_SHAPE_ROWS - 1 downto 0 do
  begin
    line := '';
    for i := 0 to High(st.buckets) do
    begin
      if st.buckets[i].count = 0 then
      begin
        // A gap is marked once, on the floor, rather than left blank: an empty
        // column and a column that happens to sit low look the same otherwise.
        if row = 0 then
          line := line + '.'
        else
          line := line + '&nbsp;';
        continue;
      end;
      // A window with no spread has no shape to scale; put it on the floor
      // rather than dividing by nothing.
      if hi <= lo then
        level := 0
      else
        level := Round(((st.buckets[i].mean - lo) / (hi - lo)) *
          (REPORT_SHAPE_ROWS - 1));
      if level = row then
        line := line + Format('<font color="%s">#</font>',
          [ReportColorHex(colors[TrndiReportBandOf(st.buckets[i].mean,
          st.limits)])])
      else
        line := line + '&nbsp;';
    end;
    Result := Result + '<tt>' + line + '</tt>' + sHTMLLineBreak;
  end;
end;

// "6 h 12 min", or just the minutes below an hour. Not FormatDateTime: a span
// of 26 hours has to read as 26, not roll over into a second day.
function ReportDuration(const minutes: integer): string;
begin
  if minutes >= 60 then
    Result := Format(RS_REPORT_SPAN_HM, [minutes div 60, minutes mod 60])
  else
    Result := Format(RS_REPORT_SPAN_M, [minutes]);
end;

// A timestamp inside the reporting window. The date is only worth printing
// when the window spans more than one day -- on the usual few-hour report it
// is the same date on every line and just crowds them.
function ReportStamp(const when: TDateTime; const spanMinutes: integer): string;
begin
  if spanMinutes >= MinsPerDay then
    Result := FormatDateTime('yyyy-mm-dd hh:nn', when)
  else
    Result := FormatDateTime('hh:nn', when);
end;

procedure ReportBandLabels(const st: TTrndiReportStats;
  out labels: TReportBandLabels; out colors: TReportBandColors);
var
  lo, hi, rlo, rhi: string;
begin
  lo := Format(BG_MSG_SHORT[st.units], [st.limits.lo]);
  hi := Format(BG_MSG_SHORT[st.units], [st.limits.hi]);
  rlo := Format(BG_MSG_SHORT[st.units], [st.limits.rangeLo]);
  rhi := Format(BG_MSG_SHORT[st.units], [st.limits.rangeHi]);

  labels[rbLow] := Format(RS_REPORT_BAND_LOW, [lo]);
  labels[rbHigh] := Format(RS_REPORT_BAND_HIGH, [hi]);
  if st.limits.hasRange then
  begin
    labels[rbBelowRange] := Format(RS_REPORT_BAND_UNDER, [lo, rlo]);
    labels[rbInRange] := Format(RS_REPORT_BAND_IN, [rlo, rhi]);
    labels[rbAboveRange] := Format(RS_REPORT_BAND_OVER, [rhi, hi]);
  end
  else
  begin
    labels[rbBelowRange] := '';
    labels[rbInRange] := Format(RS_REPORT_BAND_IN, [lo, hi]);
    labels[rbAboveRange] := '';
  end;

  // The same five colours the window and the dots use, so a band in the
  // summary is recognisable as the state the user has already seen on screen.
  colors[rbLow] := bg_color_lo;
  colors[rbBelowRange] := bg_rel_color_lo;
  colors[rbInRange] := bg_color_ok;
  colors[rbAboveRange] := bg_rel_color_hi;
  colors[rbHigh] := bg_color_hi;
end;

// Collect the readings currently loaded. The window is whatever the backend
// last handed over, not a fixed number of hours: the report then describes
// exactly the data on screen, and says so on its first line.
function TfBG.BuildGlucoseReport: TTrndiReportStats;
var
  limits: TTrndiReportLimits;
  hasRange: boolean;
begin
  // No backend yet (the menu is reachable before a driver has connected):
  // hand back an invalid report so the caller shows RS_REPORT_NODATA.
  if api = nil then
    Exit(Default(TTrndiReportStats));

  hasRange := (api.cgmRangeHi <> TrndiAPI.CGM_RANGE_HI_DISABLED) and
    (api.cgmRangeLo <> TrndiAPI.CGM_RANGE_LO_DISABLED);

  // Thresholds are held in mg/dL whatever the display unit; BG_CONVERTIONS is
  // indexed [target][source], as the limit captions in umain_timers.inc are.
  limits := TrndiMakeReportLimits(
    api.cgmLo * BG_CONVERTIONS[un][BGUnit.mgdl],
    api.cgmHi * BG_CONVERTIONS[un][BGUnit.mgdl],
    api.cgmRangeLo * BG_CONVERTIONS[un][BGUnit.mgdl],
    api.cgmRangeHi * BG_CONVERTIONS[un][BGUnit.mgdl],
    hasRange);

  Result := TrndiBuildReport(bgs, un, limits, 0);
end;

// The saveable form: fixed-width, so the bands line up in any text editor and
// the file can be pasted into a message without turning into a wall of prose.
function TfBG.ReportAsText(const st: TTrndiReportStats): string;
var
  band: TTrndiReportBand;
  labels: TReportBandLabels;
  colors: TReportBandColors;
  sb: TStringList;
  pct: double;
  bars: integer;

  procedure Row(const name, value: string);
  begin
    sb.Add(Format('%-30s %s', [name + ':', value]));
  end;

begin
  if not st.valid then
    Exit(RS_REPORT_NODATA);

  ReportBandLabels(st, labels, colors);
  sb := TStringList.Create;
  try
    sb.Add(RS_REPORT_TITLE);
    sb.Add(StringOfChar('=', Length(RS_REPORT_TITLE)));
    sb.Add(Format(RS_REPORT_PERIOD, [FormatDateTime('yyyy-mm-dd hh:nn', st.first),
      FormatDateTime('yyyy-mm-dd hh:nn', st.last), ReportDuration(st.spanMinutes)]));
    sb.Add(Format(RS_REPORT_SAMPLES, [st.count,
      ReportDuration(Round(st.cadenceMinutes)), st.coverage]));
    sb.Add('');
    sb.Add(RS_REPORT_SHAPE);
    sb.Add(TrndiReportSparkline(st));
    sb.Add('');
    sb.Add(RS_REPORT_DISTRIBUTION);
    for band := Low(TTrndiReportBand) to High(TTrndiReportBand) do
    begin
      if labels[band] = '' then
        continue;
      pct := TrndiReportBandPercent(st, band);
      // One block per five percent: a 20-cell bar that a proportional font
      // cannot stretch out of alignment.
      bars := Round(pct / REPORT_BAR_PERCENT);
      sb.Add(TrimRight(Format('  %-26s %5.1f%%  %s', [labels[band], pct,
        StringOfChar('#', bars)])));
    end;
    sb.Add('');
    sb.Add(RS_REPORT_NUMBERS);
    Row(RS_REPORT_MEAN, Format(BG_MSG_DEF[st.units], [st.mean]));
    Row(RS_REPORT_MEDIAN, Format(BG_MSG_DEF[st.units], [st.median]));
    Row(RS_REPORT_SD, Format(BG_MSG_DEF[st.units], [st.sd]));
    Row(RS_REPORT_CV, Format('%.1f%%', [st.cv]));
    Row(RS_REPORT_GMI, Format(RS_REPORT_GMI_VALUE, [st.gmiPercent, st.gmiMmolMol]));
    Row(RS_REPORT_LOWEST, Format(RS_REPORT_AT, [Format(BG_MSG_DEF[st.units],
      [st.lowest]), ReportStamp(st.lowestAt, st.spanMinutes)]));
    Row(RS_REPORT_HIGHEST, Format(RS_REPORT_AT, [Format(BG_MSG_DEF[st.units],
      [st.highest]), ReportStamp(st.highestAt, st.spanMinutes)]));
    Row(RS_REPORT_EXCURSIONS, Format(RS_REPORT_EXCURSIONS_VALUE,
      [st.lowExcursions, st.highExcursions, TRNDI_REPORT_EXCURSION_MIN]));
    // A gap no wider than twice the cadence is the sensor reporting normally,
    // not an outage, and naming it as "the longest gap" invents a problem.
    if st.longestGap > (st.cadenceMinutes * REPORT_GAP_FACTOR) then
      Row(RS_REPORT_GAP, Format(RS_REPORT_GAP_VALUE, [st.longestGap,
        ReportStamp(st.longestGapAt, st.spanMinutes)]))
    else
      Row(RS_REPORT_GAP, RS_REPORT_GAP_NONE);
    sb.Add('');
    sb.Add(RS_REPORT_FOOTER);
    Result := sb.Text;
  finally
    sb.Free;
  end;
end;

// The on-screen form. Label-colon-value rather than a table: TIpHtmlPanel is
// the renderer behind the dialogs, and nothing else in Trndi asks it for table
// layout, so the bands are drawn as coloured runs of blocks instead.
function TfBG.ReportAsHTML(const st: TTrndiReportStats): string;
var
  band: TTrndiReportBand;
  labels: TReportBandLabels;
  colors: TReportBandColors;
  pct: double;
  bars: integer;
  sb: TStringList;

  procedure Row(const name, value: string);
  begin
    sb.Add(Format('<b>%s:</b> %s%s', [name, value, sHTMLLineBreak]));
  end;

begin
  if not st.valid then
    Exit(RS_REPORT_NODATA);

  ReportBandLabels(st, labels, colors);
  sb := TStringList.Create;
  try
    sb.Add(Format('<b>%s</b>%s', [Format(RS_REPORT_PERIOD,
      [FormatDateTime('yyyy-mm-dd hh:nn', st.first),
      FormatDateTime('yyyy-mm-dd hh:nn', st.last),
      ReportDuration(st.spanMinutes)]), sHTMLLineBreak]));
    sb.Add(Format(RS_REPORT_SAMPLES, [st.count,
      ReportDuration(Round(st.cadenceMinutes)), st.coverage]) +
      sHTMLLineBreak + sHTMLLineBreak);

    sb.Add(Format('<b>%s</b>%s', [RS_REPORT_SHAPE, sHTMLLineBreak]));
    sb.Add(ReportShapeHTML(st, colors) + sHTMLLineBreak);

    sb.Add(Format('<b>%s</b>%s', [RS_REPORT_DISTRIBUTION, sHTMLLineBreak]));
    for band := Low(TTrndiReportBand) to High(TTrndiReportBand) do
    begin
      if labels[band] = '' then
        continue;
      pct := TrndiReportBandPercent(st, band);
      bars := Round(pct / REPORT_BAR_PERCENT);
      // A band that holds something but rounds to no bar still gets one, so a
      // rare low does not disappear into the same blank line as a band of zero.
      if (bars = 0) and (st.bands[band] > 0) then
        bars := 1;
      sb.Add(Format('%s <tt>%s%.1f%%</tt> &nbsp;%s%s',
        [ReportBar(bars, REPORT_BAR_CELLS, colors[band]),
        ReportHTMLNbsp(IfThen(pct < 10, 1, 0)), pct,
        labels[band], sHTMLLineBreak]));
    end;
    sb.Add(sHTMLLineBreak);

    sb.Add(Format('<b>%s</b>%s', [RS_REPORT_NUMBERS, sHTMLLineBreak]));
    Row(RS_REPORT_MEAN, Format(BG_MSG_DEF[st.units], [st.mean]));
    Row(RS_REPORT_MEDIAN, Format(BG_MSG_DEF[st.units], [st.median]));
    Row(RS_REPORT_SD, Format(BG_MSG_DEF[st.units], [st.sd]));
    Row(RS_REPORT_CV, Format('%.1f%%', [st.cv]));
    Row(RS_REPORT_GMI, Format(RS_REPORT_GMI_VALUE, [st.gmiPercent, st.gmiMmolMol]));
    Row(RS_REPORT_LOWEST, Format(RS_REPORT_AT, [Format(BG_MSG_DEF[st.units],
      [st.lowest]), ReportStamp(st.lowestAt, st.spanMinutes)]));
    Row(RS_REPORT_HIGHEST, Format(RS_REPORT_AT, [Format(BG_MSG_DEF[st.units],
      [st.highest]), ReportStamp(st.highestAt, st.spanMinutes)]));
    Row(RS_REPORT_EXCURSIONS, Format(RS_REPORT_EXCURSIONS_VALUE,
      [st.lowExcursions, st.highExcursions, TRNDI_REPORT_EXCURSION_MIN]));
    if st.longestGap > (st.cadenceMinutes * REPORT_GAP_FACTOR) then
      Row(RS_REPORT_GAP, Format(RS_REPORT_GAP_VALUE, [st.longestGap,
        ReportStamp(st.longestGapAt, st.spanMinutes)]))
    else
      Row(RS_REPORT_GAP, RS_REPORT_GAP_NONE);

    sb.Add(sHTMLLineBreak + '<i>' + RS_REPORT_FOOTER + '</i>');
    Result := sb.Text;
  finally
    sb.Free;
  end;
end;

procedure TfBG.SaveGlucoseReport(const body: string);
var
  dlg: TSaveDialog;
  f: TStringList;
begin
  dlg := TSaveDialog.Create(nil);
  try
    dlg.Title := RS_REPORT_SAVE_TITLE;
    dlg.Filter := RS_REPORT_SAVE_FILTER;
    dlg.DefaultExt := 'txt';
    dlg.FileName := Format('trndi-summary-%s.txt',
      [FormatDateTime('yyyy-mm-dd-hhnn', Now)]);
    if not dlg.Execute then
      Exit;
    try
      f := TStringList.Create;
      try
        f.Text := body;
        f.SaveToFile(dlg.FileName);
      finally
        f.Free;
      end;
      ShowMessage(Format(RS_REPORT_SAVE_OK, [dlg.FileName]));
    except
      on E: Exception do
        ShowMessage(Format(RS_REPORT_SAVE_ERROR, [E.Message]));
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfBG.miReportClick(Sender: TObject);
var
  st: TTrndiReportStats;
  saved: string;
begin
  st := BuildGlucoseReport;
  if not st.valid then
  begin
    ShowMessage(RS_REPORT_NODATA);
    Exit;
  end;

  // The save button borrows mbSlickeOpenFile, the toolkit's spare labelled
  // slot, and wears its own caption for the length of this dialog. langs is
  // the unit's documented override point; it is put back either way, so no
  // other dialog inherits the wording.
  saved := langs[mbSlickeOpenFile];
  langs[mbSlickeOpenFile] := RS_REPORT_SAVE;
  try
    // The vertical multiplier is the only handle SlickeHTMLMsg gives on the
    // panel's height, and the report is around twenty lines long.
    if SlickeHTMLMsg(sdsAuto, RS_REPORT_TITLE, ReportAsHTML(st),
      [mbSlickeOpenFile, mbClose], uxmtInformation, 42, mbClose) =
      mrSlickeOpenFile then
      SaveGlucoseReport(ReportAsText(st));
  finally
    langs[mbSlickeOpenFile] := saved;
  end;
end;
