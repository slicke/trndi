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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-08-17: The smart-quote normalization in ParseDexcomTime replaced
 *   ASCII '"' with itself (a no-op); it now replaces the actual U+201C/U+201D
 *   characters, spelled as UTF-8 byte sequences.
 *)
unit trndi.api.dexcom_time;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, dateutils, fpjson, jsonparser, StrUtils;

{** Parse a Dexcom server-time response into a TDateTime.

    @param(AsUTC When False (default), a bare millisecond epoch with no
      OffsetMinutes is returned as *local* time -- the convention every
      reading-timestamp caller wants. When True, it is returned as a UTC-valued
      TDateTime instead, for callers (time-sync/clock-skew calibration) that
      compare the result against another UTC value such as
      @code(LocalTimeToUniversal(Now)). The XML and bare-ISO paths are always
      UTC-valued regardless of AsUTC, since they parse the server's UTC digits
      verbatim with no timezone conversion.) }
function ParseDexcomTime(const S: string; out DT: TDateTime; AsUTC: boolean = False): boolean;

implementation

function TryParseISODateTime(const S: string; out DT: TDateTime): boolean;
var
  L: string;
  Year, Month, Day, Hour, Min, Sec: integer;
begin
  Result := False;
  L := Trim(S);
  
  // Strip surrounding quotes (may be multiple layers)
  while (Length(L) > 1) and (L[1] = '"') and (L[Length(L)] = '"') do
    L := Trim(Copy(L, 2, Length(L) - 2));
  
  // Skip if it starts with { (JSON object)
  if (Length(L) > 0) and (L[1] = '{') then
    Exit;
  
  if Length(L) >= 19 then
  begin
    try
      // Parse YYYY-MM-DDTHH:nn:ss
      Year := StrToInt(Copy(L, 1, 4));
      Month := StrToInt(Copy(L, 6, 2));
      Day := StrToInt(Copy(L, 9, 2));
      Hour := StrToInt(Copy(L, 12, 2));
      Min := StrToInt(Copy(L, 15, 2));
      Sec := StrToInt(Copy(L, 18, 2));
      
      DT := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
      Result := True;
      Exit;
    except
      Result := False;
    end;
  end;
end;

function ParseDexcomTime(const S: string; out DT: TDateTime; AsUTC: boolean): boolean;
var
  LTimeStr: string;
  i, j: integer;
  serverTimeData, offsetData, js: TJSONData;
  NormalizedS: string;
  OffsetMinutes: integer;
  HasOffset: boolean;
  UnixMs: int64;

  // Extract ms from "/Date(NNNN)/", "/Date(NNNN+hhmm)/" or a bare numeric string.
  //
  // The .NET form may carry a timezone offset inside the parentheses: Dexcom's
  // own DT field arrives as "Date(1786478432000+0000)" (confirmed against a live
  // Share payload). The suffix is stripped rather than applied, because the
  // millisecond value is a UTC epoch either way -- the offset only records which
  // wall clock the server considered it -- and callers converting a reading
  // timestamp want that instant rendered in local time, which adding the offset
  // would shift. Only the server-time endpoint's separate "OffsetMinutes" field
  // asks for wall-clock semantics, and MsToDT handles that via HasOffset.
  //
  // Before this, an offset suffix made TryStrToInt64 fail and ExtractUnixMs
  // return False with no fallback, so ParseDexcomTime rejected the value
  // outright -- despite both Dexcom drivers documenting the form as supported.
  function ExtractUnixMs(const Raw: string; out Ms: int64): boolean;
  var
    a, b, k: integer;
    inner: string;
  begin
    Result := False;
    a := Pos('(', Raw);
    if a > 0 then
    begin
      b := PosEx(')', Raw, a + 1);
      if b > a then
      begin
        inner := Trim(Copy(Raw, a + 1, b - a - 1));
        // Scan from the right for the offset's sign. Starting at 2 leaves the
        // leading '-' of a pre-1970 (negative) epoch alone.
        for k := Length(inner) downto 2 do
          if (inner[k] = '+') or (inner[k] = '-') then
          begin
            inner := Copy(inner, 1, k - 1);
            Break;
          end;
        Result := TryStrToInt64(inner, Ms);
        Exit;
      end;
    end;
    Result := TryStrToInt64(Trim(Raw), Ms);
  end;

  // Convert Unix ms to TDateTime. When OffsetMinutes was supplied alongside the
  // timestamp, interpret the result as the server's wall-clock time (UTC + offset);
  // otherwise honor AsUTC (local time by default, matching the historical
  // behavior reading-timestamp callers rely on).
  function MsToDT(Ms: int64): TDateTime;
  begin
    if HasOffset then
      Result := UnixToDateTime(Ms div 1000, True) + (OffsetMinutes / MinsPerDay)
    else
      Result := UnixToDateTime(Ms div 1000, AsUTC);
  end;

begin
  Result := False;
  DT := 0;
  OffsetMinutes := 0;
  HasOffset := False;
  if Trim(S) = '' then Exit;

  // Normalize quotes to standard ASCII quotes (in case smart quotes are
  // present). The literals are spelled as UTF-8 byte sequences (U+201C/U+201D)
  // so an ASCII re-save of this file cannot silently turn them into plain '"'
  // again, which would make both calls no-ops.
  NormalizedS := StringReplace(S, #$E2#$80#$9C, '"', [rfReplaceAll]);
  NormalizedS := StringReplace(NormalizedS, #$E2#$80#$9D, '"', [rfReplaceAll]);

  // 1) XML-like: <SystemTime>YYYY-MM-DDTHH:mm:ss</SystemTime>
  if Pos('<', NormalizedS) > 0 then
  begin
    // Extract between first '>' and next '<' after it
    i := Pos('>', NormalizedS);
    j := PosEx('<', NormalizedS, i + 1);
    if (i > 0) and (j > i) then
    begin
      LTimeStr := Copy(NormalizedS, i + 1, j - i - 1);
      if TryParseISODateTime(LTimeStr, DT) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  // 2) JSON object: pick up OffsetMinutes if present, then parse the timestamp from
  //    ServerTime or DateTime. Must run before the bare /Date(ms)/ extraction below,
  //    otherwise that branch grabs the ms straight out of the JSON and we never see
  //    the OffsetMinutes alongside it.
  if (Pos('{', NormalizedS) > 0) or (Pos('[', NormalizedS) > 0) then
  try
    js := GetJSON(NormalizedS);
    try
      if (js <> nil) and (js.JSONType = jtObject) then
      begin
        offsetData := TJSONObject(js).Find('OffsetMinutes');
        if (offsetData <> nil) and (offsetData.JSONType = jtNumber) then
        begin
          OffsetMinutes := offsetData.AsInteger;
          HasOffset := True;
        end;

        serverTimeData := TJSONObject(js).Find('ServerTime');
        if serverTimeData = nil then
          serverTimeData := TJSONObject(js).Find('DateTime');

        if serverTimeData <> nil then
        begin
          if serverTimeData.JSONType = jtNumber then
          begin
            DT := MsToDT(Trunc(serverTimeData.AsFloat));
            Result := True;
            Exit;
          end;

          LTimeStr := Trim(serverTimeData.AsString);
          if LTimeStr <> '' then
          begin
            if ExtractUnixMs(LTimeStr, UnixMs) then
            begin
              DT := MsToDT(UnixMs);
              Result := True;
              Exit;
            end;
            // ISO inside JSON: the literal is already the server's wall clock,
            // so OffsetMinutes does not apply here.
            if TryParseISODateTime(LTimeStr, DT) then
            begin
              Result := True;
              Exit;
            end;
          end;
        end;
      end;
    finally
      js.Free;
    end;
  except
    // ignore JSON parse errors and fallthrough
  end;

  // 3) Bare /Date(1610464324000)/ or digits in parentheses
  if ExtractUnixMs(NormalizedS, UnixMs) then
  begin
    DT := MsToDT(UnixMs);
    Result := True;
    Exit;
  end;

  // 4) Try to parse bare ISO-like strings directly
  if TryParseISODateTime(Trim(NormalizedS), DT) then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
end;

end.
