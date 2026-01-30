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
begin
  Result := False;
  L := Trim(S);
  if Length(L) >= 19 then
  begin
    try
      DT := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', Copy(L, 1, 19));
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
  js: TJSONData;
begin
  Result := False;
  DT := 0;
  if Trim(S) = '' then Exit;

  // 1) XML-like: <SystemTime>YYYY-MM-DDTHH:mm:ss</SystemTime>
  if Pos('<', S) > 0 then
  begin
    // Extract between first '>' and next '<' after it
    i := Pos('>', S);
    j := PosEx('<', S, i + 1);
    if (i > 0) and (j > i) then
    begin
      LTimeStr := Copy(S, i + 1, j - i - 1);
      if TryParseISODateTime(LTimeStr, DT) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  // 2) /Date(1610464324000)/ or digits in parentheses
  i := Pos('(', S);
  if i > 0 then
  begin
    j := Pos(')', S, i + 1);
    if j > i then
    begin
      LTimeStr := Copy(S, i + 1, j - i - 1);
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
    js := GetJSON(S);
    try
      if js.JSONType = jtObject then
      begin
        LTimeStr := TJSONObject(js).Get('ServerTime', '');
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
          // Try numeric ms
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
    finally
      js.Free;
    end;
  except
    // ignore JSON parse errors and fallthrough
  end;

  // 4) Try to parse bare ISO-like strings directly
  if TryParseISODateTime(Trim(S), DT) then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
end;

end.
