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
unit trndi.api.dexcom_helpers;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, trndi.types;

{** Escape a string for safe inclusion in a JSON value. Worst-case size is 2x
    the input (every char escaped); never under-allocates. }
function JSONEscape(const S: string): string;

{** Map a Dexcom trend representation (string or numeric) into the internal
    `BGTrend` enum. Prefers textual mapping via `BG_TRENDS_STRING`. If the value
    is numeric, accepts both 0-based (BGTrend ordinal) and 1-based codes. As a
    final fallback, recognizes the Dexcom Share API's CamelCase textual trend
    names and converts them to the corresponding enum. }
function MapDexcomTrendToEnum(const S: string): BGTrend;

{** Heuristic: does a Dexcom Share response body indicate a dead/rejected
    session (so the caller should re-authenticate)? Matches both prose
    ("Session ID not found") and the CamelCase error codes Dexcom actually
    sends ("SessionIdNotFound", "SessionNotValid") by comparing with spaces
    stripped. }
function DexcomLooksLikeSessionFailure(const Response: string): boolean;

implementation

function JSONEscape(const S: string): string;
var
  i, idx: integer;
  c: char;
begin
  SetLength(Result, Length(S) * 2);
  idx := 1;
  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
    '"':
      begin Result[idx] := '\'; Inc(idx); Result[idx] := '"'; Inc(idx); end;
    '\':
      begin Result[idx] := '\'; Inc(idx); Result[idx] := '\'; Inc(idx); end;
    #8:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'b'; Inc(idx); end;
    #9:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 't'; Inc(idx); end;
    #10:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'n'; Inc(idx); end;
    #12:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'f'; Inc(idx); end;
    #13:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'r'; Inc(idx); end;
    else
      begin Result[idx] := c; Inc(idx); end;
    end;
  end;
  SetLength(Result, idx - 1);
end;

function MapDexcomTrendToEnum(const S: string): BGTrend;
const
  // Dexcom Share API CamelCase textual trend names (alias of BG_TRENDS_STRING,
  // which holds the uppercased/spaced variant). Indexed by BGTrend ordinal (0..7).
  // 'RateOutOfRange' is handled as an alias of 'NotComputable' below.
  DEXCOM_TREND_NAMES: array[0..7] of string = (
    'DoubleUp', 'SingleUp', 'FortyFiveUp', 'Flat',
    'FortyFiveDown', 'SingleDown', 'DoubleDown', 'NotComputable'
  );
var
  code, idx: integer;
  L: string;
begin
  L := Trim(S);

  // 1) Canonical textual mapping (BG_TRENDS_STRING)
  for Result := Low(BGTrend) to High(BGTrend) do
    if BG_TRENDS_STRING[Result] = L then
      Exit;

  // 2) Numeric: accept either 0-based (BGTrend ordinal) or 1-based codes
  if TryStrToInt(L, code) then
  begin
    if (code >= Ord(Low(BGTrend))) and (code <= Ord(High(BGTrend))) then
      Result := BGTrend(code)
    else if (code - 1 >= Ord(Low(BGTrend))) and (code - 1 <= Ord(High(BGTrend))) then
      Result := BGTrend(code - 1)
    else
      Result := TdPlaceholder;
    Exit;
  end;

  // 3) Dexcom CamelCase textual trend names
  idx := -1;
  if L = 'RateOutOfRange' then
    idx := 7
  else
    for code := 0 to High(DEXCOM_TREND_NAMES) do
      if DEXCOM_TREND_NAMES[code] = L then
      begin
        idx := code;
        Break;
      end;

  if (idx >= Ord(Low(BGTrend))) and (idx <= Ord(High(BGTrend))) then
    Result := BGTrend(idx)
  else
    Result := TdPlaceholder;
end;

function DexcomLooksLikeSessionFailure(const Response: string): boolean;
var
  L: string;
begin
  // Strip spaces before lowercasing so "Session ID not found" and
  // "SessionIdNotFound" both reduce to the same needle. Glucose payloads
  // never contain the word "session", so this cannot misfire on real data.
  L := LowerCase(StringReplace(Response, ' ', '', [rfReplaceAll]));
  Result :=
    ((Pos('session', L) > 0) and
    ((Pos('invalid', L) > 0) or (Pos('expired', L) > 0) or
    (Pos('notvalid', L) > 0) or (Pos('notfound', L) > 0) or
    (Pos('sessionidnull', L) > 0))) or
    (Pos('unauthorized', L) > 0) or
    (Pos('forbidden', L) > 0) or
    (Pos('accountpassword', L) > 0);
end;

end.
