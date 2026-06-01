unit trndi.api.dexcom_time;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, dateutils, fpjson, jsonparser, StrUtils;

function ParseDexcomTime(const S: string; out DT: TDateTime): boolean;

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

function ParseDexcomTime(const S: string; out DT: TDateTime): boolean;
var
  LTimeStr: string;
  i, j: integer;
  serverTimeData, offsetData, js: TJSONData;
  NormalizedS: string;
  OffsetMinutes: integer;
  HasOffset: boolean;
  UnixMs: int64;

  // Extract ms either from "/Date(NNNN)/" or a bare numeric string.
  function ExtractUnixMs(const Raw: string; out Ms: int64): boolean;
  var
    a, b: integer;
  begin
    Result := False;
    a := Pos('(', Raw);
    if a > 0 then
    begin
      b := PosEx(')', Raw, a + 1);
      if b > a then
      begin
        Result := TryStrToInt64(Trim(Copy(Raw, a + 1, b - a - 1)), Ms);
        Exit;
      end;
    end;
    Result := TryStrToInt64(Trim(Raw), Ms);
  end;

  // Convert Unix ms to TDateTime. When OffsetMinutes was supplied alongside the
  // timestamp, interpret the result as the server's wall-clock time (UTC + offset);
  // otherwise preserve the historical behavior of returning the user's local time.
  function MsToDT(Ms: int64): TDateTime;
  begin
    if HasOffset then
      Result := UnixToDateTime(Ms div 1000, True) + (OffsetMinutes / MinsPerDay)
    else
      Result := UnixToDateTime(Ms div 1000, False);
  end;

begin
  Result := False;
  DT := 0;
  OffsetMinutes := 0;
  HasOffset := False;
  if Trim(S) = '' then Exit;

  // Normalize quotes to standard ASCII quotes (in case smart quotes are present)
  NormalizedS := StringReplace(S, '"', '"', [rfReplaceAll]);
  NormalizedS := StringReplace(NormalizedS, '"', '"', [rfReplaceAll]);

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
