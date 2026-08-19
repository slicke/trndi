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

{** Debug backend that replays an explicit list of readings typed into the
    username field, oldest first — e.g. @code(4.0 5.5 5.0 2.7) makes 2.7 the
    current reading. A @code(_) token leaves that slot missing. Any '.' in the
    list switches the whole list to mmol/L; otherwise values are mg/dL. }
unit trndi.api.debug_list;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, trndi.types, trndi.api, trndi.funcs.core,
trndi.api.debug, dateutils;

type
  // Main class
DebugListAPI = class(DebugAPI)
protected
    {** Parsed series in the order it was typed (oldest first), mg/dL, with
        @code(BG_NO_VAL) for the slots marked missing. }
  series: array of integer;
  function getSystemName: string; override;

    {** Split the username into the series; pass may force the input unit. }
  procedure ParseSeries(const list, unitHint: string);
public
  constructor Create(user, pass: string); override;
  function getReadings({%H-}min, {%H-}maxNum: integer; {%H-}extras: string;
    out res: string; noCache: boolean): BGResults; override;

  class function ParamLabel(LabelName: APIParamLabel): string; override;
end;

implementation

const
  // Readings the graph normally gets from the other debug backends. A shorter
  // list is padded out with missing slots rather than returning a stub series,
  // so "the rest is missing" is what the UI actually sees.
  MinSlots = 11;
  // A token of just '_' marks one missing reading; '_3' marks three in a row.
  MissingMarker = '_';

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DebugListAPI.getSystemName: string;
begin
  result := 'Debug Reading List API';
end;

{------------------------------------------------------------------------------
  ParseSeries
  --------------------
  Tokenizes the typed list. Separators are spaces, tabs, commas and semicolons;
  the decimal separator is always '.', regardless of locale. The unit is taken
  from unitHint ('mmol'/'mgdl') when given, otherwise a '.' anywhere in the list
  means the whole list is mmol/L.
------------------------------------------------------------------------------}
procedure DebugListAPI.ParseSeries(const list, unitHint: string);
var
  tokens: TStringArray;
  tok, hint: string;
  fs: TFormatSettings;
  mmol: boolean;
  gap, i: integer;
  v: double;
begin
  SetLength(series, 0);
  tokens := Trim(list).Split([' ', #9, ',', ';'], TStringSplitOptions.ExcludeEmpty);
  if Length(tokens) = 0 then
    Exit;

  hint := LowerCase(Trim(unitHint));
  if (hint = 'mmol') or (hint = 'mmol/l') then
    mmol := true
  else
  if (hint = 'mgdl') or (hint = 'mg/dl') then
    mmol := false
  else
    mmol := Pos('.', list) > 0;

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  for tok in tokens do
  begin
    if Copy(tok, 1, 1) = MissingMarker then
    begin
      // '_' is one missing reading, '_N' is N of them in a row
      gap := StrToIntDef(Copy(tok, 2, MaxInt), 1);
      if gap < 1 then
        gap := 1;
      for i := 1 to gap do
      begin
        SetLength(series, Length(series) + 1);
        series[High(series)] := BG_NO_VAL;
      end;
      Continue;
    end;

    if not TryStrToFloat(tok, v, fs) then
      Continue;  // Anything unparsable is dropped rather than faking a value

    SetLength(series, Length(series) + 1);
    if mmol then
      series[High(series)] := System.Round(v * TrndiAPI.toMGDL)
    else
      series[High(series)] := System.Round(v);
  end;
end;

{------------------------------------------------------------------------------
  Constructor
------------------------------------------------------------------------------}
constructor DebugListAPI.Create(user, pass: string);
begin
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := user;
  ParseSeries(user, pass);
  inherited;
end;

{------------------------------------------------------------------------------
  getReadings
  --------------------
  Replays the parsed series, newest first (the list is typed oldest first), on
  the usual 5-minute grid ending at now. Slots older than the list are returned
  as missing, so a short list renders as "these readings, nothing before them".
  Deltas are taken against the nearest older reading that exists, so a value
  after a gap still gets a trend.
------------------------------------------------------------------------------}
function DebugListAPI.getReadings(min, maxNum: integer; extras: string;
out res: string; noCache: boolean): BGResults;
var
  vals: array of integer;
  total, i, srcIdx, prevIdx, diff: integer;
  nodata: maybeint;
begin
  res := '';

  // Nothing usable typed - behave like the plain debug backend
  if Length(series) = 0 then
    Exit(inherited getReadings(min, maxNum, extras, res, noCache));

  nodata.exists := false;

  total := Length(series);
  if total < MinSlots then
    total := MinSlots;

  // Flip to the newest-first order the UI expects, padding the older end
  SetLength(vals, total);
  for i := 0 to total - 1 do
  begin
    srcIdx := High(series) - i;
    if srcIdx >= 0 then
      vals[i] := series[srcIdx]
    else
      vals[i] := BG_NO_VAL;
  end;

  SetLength(Result, total);
  for i := 0 to total - 1 do
  begin
    Result[i].Init(mgdl, self.systemName);
    Result[i].date := FakeTime(i * 5);
    Result[i].updateEnv('Debug', nodata, nodata);

    if vals[i] = BG_NO_VAL then
    begin
      Result[i].Clear;
      Result[i].trend := TdPlaceholder;
      Continue;
    end;

    diff := 0;
    for prevIdx := i + 1 to total - 1 do
      if vals[prevIdx] <> BG_NO_VAL then
      begin
        diff := vals[i] - vals[prevIdx];
        Break;
      end;

    Result[i].update(vals[i], diff);
    Result[i].trend := CalculateTrendFromDelta(diff);
    Result[i].level := getLevel(Result[i].val);
  end;
end;

class function DebugListAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  case LabelName of
  APLUser:
    Result := 'Readings, oldest first, e.g. "4.0 5.5 5.0 2.7" - use _ for a missing one';
  APLPass:
    Result := 'Optional: "mmol" or "mgdl" to force the unit of the list above';
  APLDesc:
    Result := result + sLineBreak + sLineBreak +
      'Type the readings you want to see in the username field, oldest first, separated by spaces. ' +
      'The last one becomes the current reading, and anything older than the list is reported missing. ' +
      'Use _ for a missing reading inside the list (_3 for three in a row). ' +
      'A "." anywhere in the list means the values are mmol/L, otherwise they are mg/dL.';
  APLDescHTML:
    Result := result + '<br><br>Type the readings in the <u>username</u> field, <b>oldest first</b>: ' +
      '<i>4.0 5.5 5.0 2.7</i> makes <b>2.7</b> the current reading and leaves everything older missing.<br>' +
      'Use <b>_</b> for a missing reading (<b>_3</b> for three in a row). A "<b>.</b>" anywhere means <b>mmol/L</b>, otherwise <b>mg/dL</b>.';
   // Copyright inherits DebugAPI's shared default.
  end;
end;

end.
