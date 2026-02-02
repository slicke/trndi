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
  serverTimeData, js: TJSONData;
  NormalizedS: string;

begin
  Result := False;
  DT := 0;
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

  // 2) /Date(1610464324000)/ or digits in parentheses
  i := Pos('(', NormalizedS);
  if i > 0 then
  begin
    j := Pos(')', NormalizedS, i + 1);
    if j > i then
    begin
      LTimeStr := Copy(NormalizedS, i + 1, j - i - 1);
      // Trim any non-digit chars
      LTimeStr := StringReplace(LTimeStr, '"', '', [rfReplaceAll]);
      LTimeStr := Trim(LTimeStr);
      if (LTimeStr <> '') and (AnsiMatchStr(LTimeStr, [LTimeStr])) then
      begin
        try
          DT := UnixToDateTime(StrToInt64(LTimeStr) div 1000, False);
          Result := True;
          Exit;
        except
          // fallthrough
        end;
      end;
    end;
  end;

  // 3) JSON object with ServerTime (could be "/Date(ms)/", numeric ms or ISO string)
  try
    // Only try JSON parsing if it looks like JSON

    if (Pos('{', NormalizedS) > 0) or (Pos('[', NormalizedS) > 0) then
    begin
      js := GetJSON(NormalizedS);
      try
        if js.JSONType = jtObject then
        begin
          // Try to get ServerTime field
          if TJSONObject(js).Find('ServerTime') <> nil then
          begin
            serverTimeData := TJSONObject(js).Find('ServerTime');
            if serverTimeData <> nil then
            begin
              // Handle numeric milliseconds
              if serverTimeData.JSONType = jtNumber then
              begin
                try
                  DT := UnixToDateTime(Trunc(serverTimeData.AsFloat / 1000), False);
                  Result := True;
                  Exit;
                except
                end;
              end;
              
              LTimeStr := serverTimeData.AsString;
              LTimeStr := Trim(LTimeStr);
              if LTimeStr <> '' then
              begin
                // If value like /Date(.....)/, extract digits
                i := Pos('(', LTimeStr);
                if i > 0 then
                begin
                  j := Pos(')', LTimeStr, i + 1);
                  if (j > i) then
                  begin
                    LTimeStr := Copy(LTimeStr, i + 1, j - i - 1);
                    try
                      DT := UnixToDateTime(StrToInt64(LTimeStr) div 1000, False);
                      Result := True;
                      Exit;
                    except
                    end;
                  end;
                end;
                // Try numeric ms (string representation)
                try
                  DT := UnixToDateTime(StrToInt64(LTimeStr) div 1000, False);
                  Result := True;
                  Exit;
                except
                end;
                // Try ISO parse
                if TryParseISODateTime(LTimeStr, DT) then
                begin
                  Result := True;
                  Exit;
                end;
              end;
            end;
          end;
        end;
      finally
        js.Free;
      end;
    end;
  except
    // ignore JSON parse errors and fallthrough
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
