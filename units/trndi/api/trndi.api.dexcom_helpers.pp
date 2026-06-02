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

end.
