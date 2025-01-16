
(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit nsapi;

{$mode ObjFPC}{$H+}

interface

uses 
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
fpjson, jsonparser, dateutils, StrUtils, sha1, math, jsonscanner;

const 
  NS_STATUS = 'status.json';

const 
  NS_URL_BASE = '/api/v1/';

const 
  NS_READINGS = 'entries/sgv.json';

type 
  // Main class
  NightScout = class(TrndiAPI)
    protected 
      // NS API key
      key: string;
    public 
      constructor create(user, pass, extra: string);
      override;
      function connect: boolean;
      override;
      function getReadings(min, maxNum: integer; extras: string = ''): BGResults;
      override;
    private 

    published 
      property remote: string read baseUrl;
  end;

implementation

constructor NightScout.create(user, pass, extra: string);
begin
  ua      := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := TrimRightSet(user, ['/']) + NS_URL_BASE;
  key     := IfThen(pass <> '', 'API-SECRET=' + SHA1Print(SHA1String(pass)), '');
  inherited;
end;

function NightScout.Connect: Boolean;
var
  ResponseStr   : string;            // Holds JSON response from Nightscout
  JSONParser    : TJSONParser;       // Parser to convert string to JSON
  JSONData      : TJSONData;         // Base class for JSON data
  RootObject    : TJSONObject;       // The root JSON object
  SettingsObj   : TJSONObject;       // The "settings" object
  ThresholdsObj : TJSONObject;       // The "thresholds" object
  ServerEpoch   : Int64;             // Will hold the serverTimeEpoch value
  UTCDateTime   : TDateTime;         // Server time converted to TDateTime
begin
  // 1. Validate BaseURL (example check, adjust to your needs)
  if (Copy(BaseUrl, 1, 4) <> 'http') then
  begin
    Result  := False;
    LastErr := 'Invalid address. Must start with http:// or https://!';
    Exit;
  end;

  // 2. Call your method to get the JSON data from Nightscout
  //    Adjust the parameters to your actual function signature
  ResponseStr := Native.Request(False, NS_STATUS, [], '', Key);

  // 3. Check for empty response
  if Trim(ResponseStr) = '' then
  begin
    Result  := False;
    LastErr := 'Did not receive any data from the server!';
    Exit;
  end;

  // 4. If the response starts with '+', treat it as an error (from your original code)
  if (ResponseStr[1] = '+') then
  begin
    Result  := False;
    LastErr := TrimLeftSet(ResponseStr, ['+']);
    Exit;
  end;

  // 5. Check if response contains something like 'Unauthorized'
  //    (in your original code, you looked for 'Unau')
  if Pos('Unau', ResponseStr) > 0 then
  begin
    Result  := False;
    LastErr := 'Incorrect access code for NightScout';
    Exit;
  end;

  // 6. Parse the JSON into objects
  try
    JSONParser := TJSONParser.Create(ResponseStr, [joUTF8, joIgnoreTrailingComma]);
    try
      JSONData := JSONParser.Parse;
    finally
      JSONParser.Free;
    end;

    // Ensure top-level is a JSON object
    if not (JSONData is TJSONObject) then
    begin
      Result  := False;
      LastErr := 'Unexpected JSON structure (not a JSON object).';
      JSONData.Free;
      Exit;
    end;
    RootObject := TJSONObject(JSONData);

    // 7. Extract "serverTimeEpoch" from the root
    //    If it doesn't exist, default to 0
    ServerEpoch := RootObject.Get('serverTimeEpoch', Int64(0));

    // 8. Navigate to settings.thresholds (nested object)
    SettingsObj := RootObject.FindPath('settings') as TJSONObject;
    if Assigned(SettingsObj) then
    begin
      ThresholdsObj := SettingsObj.FindPath('thresholds') as TJSONObject;
      if Assigned(ThresholdsObj) then
      begin
        // Example variables you want to fill, adjust to your code
        cgmHi      := ThresholdsObj.Get('bgHigh', 0);
        cgmLo      := ThresholdsObj.Get('bgLow', 0);
        cgmRangeHi := ThresholdsObj.Get('bgTargetTop', 0);
        cgmRangeLo := ThresholdsObj.Get('bgTargetBottom', 0);
      end;
    end;

    // Always free JSONData when done parsing
    JSONData.Free;
  except
    on E: Exception do
    begin
      Result  := False;
      LastErr := 'JSON parse error: ' + E.Message;
      Exit;
    end;
  end;

  // 9. Validate serverTimeEpoch
  if ServerEpoch <= 0 then
  begin
    Result  := False;
    LastErr := 'Invalid or missing serverTimeEpoch in JSON.';
    Exit;
  end;

  // 10. Convert ms-based Unix epoch to TDateTime
  //     (UnixToDateTime expects seconds, so we do "div 1000")
  UTCDateTime := UnixToDateTime(ServerEpoch div 1000);

  // 11. Calculate time difference
  //     - This is an example only; adjust to match your needs.
  //       If you want Nightscout time minus local time, for instance:
  TimeDiff := SecondsBetween(UTCDateTime, LocalTimeToUniversal(Now));
  if TimeDiff < 0 then
    TimeDiff := 0;
  TimeDiff := -TimeDiff;  // or however you interpret it in your environment

  // If we get here, everything succeeded
  Result := True;
end;


// extras = path
function NightScout.getReadings(min, maxNum: integer; extras: string = ''): BGResults;

var 
  js:     TJSONData;
  i:   integer;
  t: BGTrend;
  s: string;
  params: array[1..1] of string;
begin
  if extras = '' then extras := NS_READINGS;

  params[1] := 'count=' + IntToStr(maxNum);

  try
    js := GetJSON(native.request(false, extras, params, '', key));
  except
    Exit;
end;

SetLength(result, js.count);

for i := 0 to js.count - 1 do
  with js.FindPath(Format('[%d]', [i])) do
    begin
      result[i].Init(mgdl, self.ToString);
      result[i].update(FindPath('sgv').AsInteger, single(FindPath('delta').AsFloat));

      s := FindPath('direction').AsString;

      for t in BGTrend do
        begin
          if BG_TRENDS_STRING[t] = s then
            begin
              result[i].trend := t;
              break;
            end;
          result[i].trend := TdNotComputable;
        end;

      result[i].date := JSToDateTime(FindPath('date').AsInt64);
      result[i].level := getLevel(result[i].val);
    end;


end;

end.
