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
unit trndi.api.nightscout;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
fpjson, jsonparser, dateutils, StrUtils, sha1, Math, jsonscanner;

const
  {** Relative filename for the Nightscout status endpoint used to probe
      server health, time, and thresholds. }
NS_STATUS = 'status.json';

const
  {** Base path for Nightscout v1 API endpoints (appended to the provided base URL). }
NS_URL_BASE = '/api/v1/';

const
  {** Default path for SGV (sensor glucose value) entries. }
NS_READINGS = 'entries/sgv.json';

type
  {** NightScout API client.
      Provides methods to connect to a Nightscout instance and fetch CGM readings,
      mapping responses into Trndi’s internal types.

      @seealso(TrndiAPI)
   }
NightScout = class(TrndiAPI)
protected
    {** Nightscout API secret header value (computed as
        'API-SECRET=' + SHA1(pass) when a secret is supplied). }
  key: string;

public
    {** Create a NightScout API client.
        Initializes the HTTP User-Agent, derives the API base URL from @code(user),
        prepares the API secret header from @code(pass), and calls the inherited
        constructor.

        @param(user  Base Nightscout URL, including scheme, e.g. https://example.com)
        @param(pass  Nightscout API secret in plain text; hashed to SHA1 for header)
        @param(extra Reserved for future use)
     }
  constructor Create(user, pass, extra: string); override;

    {** Connect to Nightscout, retrieve server status, thresholds, and set time offset.
        Requests @code(NS_STATUS), validates/handles errors, parses
        @code(serverTimeEpoch) and @code(settings.thresholds), and computes
        @code(timeDiff) relative to the local machine time.

        @returns(True on success; False otherwise. On failure, @code(errormsg) is set.)
     }
  function connect: boolean; override;

    {** Fetch SGV readings and map them to @code(BGResults).
        If @code(extras) is empty, uses @code(NS_READINGS). Sends @code(count=maxNum)
        as a query parameter. Populates each reading with value, delta, trend,
        environment (device, rssi, noise), timestamp, and derived level.

        @param(minNum Unused here; time-span can be endpoint-dependent)
        @param(maxNum Maximum number of readings to fetch)
        @param(extras Optional endpoint path override, e.g. 'entries/sgv.json')
        @param(res    Out parameter receiving the raw JSON response string)
        @returns(Array of @code(BGReading); empty on errors or unauthorized)
     }
  function getReadings(minNum, maxNum: integer; extras: string;
    out res: string): BGResults; override;
    {** UI parameter label provider (override).
      1: NightScout URL
      2: API Secret (plain text)
      3: (unused)
     }
  class function ParamLabel(Index: integer): string; override;
private
    // (no private members)

published
    {** The remote Nightscout base URL actually used (read-only proxy to baseUrl). }
  property remote: string read baseUrl;
end;

implementation

{------------------------------------------------------------------------------
  create (constructor)
  --------------------
  Initialize UA, derive API base URL from user, compute API-SECRET if present,
  and call inherited base constructor.
 ------------------------------------------------------------------------------}
constructor NightScout.Create(user, pass, extra: string);
begin
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  // Ensure no trailing slash before appending API base path.
  baseUrl := TrimRightSet(user, ['/']) + NS_URL_BASE;

  // Build API-SECRET header value if a secret is provided.
  // Nightscout expects a SHA1 hex digest of the plain-text secret.
  key := IfThen(pass <> '', 'API-SECRET=' + SHA1Print(SHA1String(pass)), '');

  // Call the base class constructor with the same signature.
  inherited;
end;

{------------------------------------------------------------------------------
  Connect
  -------
  Fetch status.json, validate connectivity/authorization, read thresholds,
  and compute timeDiff vs local system time.
 ------------------------------------------------------------------------------}
function NightScout.Connect: boolean;
var
  ResponseStr: string;            // Raw response from Nightscout
  JSONParser: TJSONParser;       // Parser to convert string to JSON
  JSONData: TJSONData;         // Base JSON data type
  RootObject: TJSONObject;       // Root JSON object
  SettingsObj: TJSONObject;       // "settings" object
  ThresholdsObj: TJSONObject;       // "thresholds" object under settings
  ServerEpoch: int64;             // serverTimeEpoch (milliseconds)
  UTCDateTime: TDateTime;         // Server time as TDateTime (UTC)
begin
  // 1) Quick sanity check on URL; avoids obvious mistakes early.
  if (Copy(BaseUrl, 1, 4) <> 'http') then
  begin
    Result := false;
    LastErr := 'Invalid address. Must start with http:// or https://!';
    Exit;
  end;

  // 2) Probe server status and settings.
  //    Native.Request(signature): (useGet, path, params, body, header)
  ResponseStr := Native.Request(false, NS_STATUS, [], '', Key);

  // 3) Basic validation for empty payloads.
  if Trim(ResponseStr) = '' then
  begin
    Result := false;
    LastErr := 'Did not receive any data from the server!';
    Exit;
  end;

  // 4) Some backends may prefix '+' to indicate application-level errors.
  if (ResponseStr[1] = '+') then
  begin
    Result := false;
    LastErr := TrimLeftSet(ResponseStr, ['+']);
    Exit;
  end;

  // 5) Coarse unauthorized detection (Nightscout messages vary by version).
  if Pos('Unau', ResponseStr) > 0 then
  begin
    Result := false;
    LastErr := 'Incorrect access code for NightScout';
    Exit;
  end;

  // 6) JSON parsing and extraction.
  try
    // Allow UTF-8 and trailing commas for robustness against minor formatting.
    JSONParser := TJSONParser.Create(ResponseStr, [joUTF8, joIgnoreTrailingComma]);
    try
      JSONData := JSONParser.Parse;
    finally
      JSONParser.Free;
    end;

    if not (JSONData is TJSONObject) then
    begin
      Result := false;
      LastErr := 'Unexpected JSON structure (not a JSON object).';
      JSONData.Free;
      Exit;
    end;

    RootObject := TJSONObject(JSONData);

    // 7) Server time epoch in milliseconds (0 if missing).
    ServerEpoch := RootObject.Get('serverTimeEpoch', int64(0));

    // 8) Optional thresholds from settings.thresholds.
    SettingsObj := RootObject.FindPath('settings') as TJSONObject;
    if Assigned(SettingsObj) then
    begin
      ThresholdsObj := SettingsObj.FindPath('thresholds') as TJSONObject;
      if Assigned(ThresholdsObj) then
      begin
        // These map directly into TrndiAPI’s exposed properties.
        cgmHi := ThresholdsObj.Get('bgHigh', 0);
        cgmLo := ThresholdsObj.Get('bgLow', 0);
        cgmRangeHi := ThresholdsObj.Get('bgTargetTop', 0);
        cgmRangeLo := ThresholdsObj.Get('bgTargetBottom', 0);
      end;
    end;

    // Done with JSON tree.
    JSONData.Free;
  except
    on E: Exception do
    begin
      Result := false;
      LastErr := 'JSON parse error: ' + E.Message;
      Exit;
    end;
  end;

  // 9) Require a valid epoch to establish time calibration.
  if ServerEpoch <= 0 then
  begin
    Result := false;
    LastErr := 'Invalid or missing serverTimeEpoch in JSON.';
    Exit;
  end;

  // 10) Convert milliseconds to TDateTime (UnixToDateTime expects seconds).
  UTCDateTime := UnixToDateTime(ServerEpoch div 1000);

  // 11) Compute timeDiff as the difference between server UTC and local time.
  //     Calculate time difference: server time (UTC) minus local time (not converted to UTC)
  //     This accounts for both clock skew and timezone offset
  TimeDiff := Round((UTCDateTime - Now) * 86400);
  // Set tz so JSToDateTime applies the correction to reading timestamps
  tz := TimeDiff;

  Result := true;
end;

{------------------------------------------------------------------------------
  getReadings
  -----------
  Fetch SGV entries (defaults to NS_READINGS) and map JSON array into
  BGResults: value, delta, environment, trend, date, and derived level.
 ------------------------------------------------------------------------------}
function NightScout.getReadings(minNum, maxNum: integer; extras: string;
out res: string): BGResults;
var
  js: TJSONData;
  i: integer;
  t: BGTrend;
  s, resp, dev: string;
  params: array[1..1] of string;
  deltaField, rssiField, noiseField: TJSONData;
  deltaValue: single;
  rssiValue, noiseValue, currentSgv, prevSgv: integer;
begin
  // Default to SGV endpoint if caller provided no override.
  if extras = '' then
    extras := NS_READINGS;

  // Nightscout supports a 'count' param to limit the number of returned entries.
  params[1] := 'count=' + IntToStr(maxNum);

  try
    resp := native.request(false, extras, params, '', key);
    js := GETJSON(resp); // Expecting an array of entries
  except
    // Any exception during request/parse yields an empty result.
    Exit;
  end;

  res := resp;

  // Bail on unauthorized responses early without raising.
  if Pos('Unauthorized', resp) > 0 then
    Exit;

  // Pre-size the results array to the number of JSON items.
  SetLength(Result, js.Count);

  for i := 0 to js.Count - 1 do
    with js.FindPath(Format('[%d]', [i])) do
    begin
      dev := FindPath('device').AsString;

      // Initialize reading in mg/dL; source identifier uses class name.
      Result[i].Init(mgdl, self.ToString);

      // Get current SGV value
      currentSgv := FindPath('sgv').AsInteger;

      // Value and trend delta.
      // Some Nightscout entries may not include delta field
      deltaField := FindPath('delta');
      if Assigned(deltaField) then
        deltaValue := single(deltaField.AsFloat)
      else
      begin
        // Calculate delta manually from previous reading
        // Nightscout returns entries in reverse chronological order (newest first)
        if i < js.Count - 1 then
        begin
          // Get the previous (older) reading's SGV
          prevSgv := js.FindPath(Format('[%d].sgv', [i + 1])).AsInteger;
          deltaValue := single(currentSgv - prevSgv);
        end
        else
          // Last (oldest) entry has no previous reading to compare
          deltaValue := 0;
      end;
      Result[i].update(currentSgv, deltaValue);

      // Receiver environment details (optional fields in Nightscout).
      rssiField := FindPath('rssi');
      noiseField := FindPath('noise');
      if Assigned(rssiField) then
        rssiValue := rssiField.AsInteger
      else
        rssiValue := 0;
      if Assigned(noiseField) then
        noiseValue := noiseField.AsInteger
      else
        noiseValue := 0;
      Result[i].updateEnv(dev, rssiValue, noiseValue);

      // Translate Nightscout 'direction' string to BGTrend enum.
      s := FindPath('direction').AsString;
      for t in BGTrend do
      begin
        if BG_TRENDS_STRING[t] = s then
        begin
          Result[i].trend := t;
          break;
        end;
        // Default if no mapping matched this iteration (will be overwritten if matched later)
        Result[i].trend := TdNotComputable;
      end;

      // Nightscout dates are ms since epoch; JSToDateTime will apply tz correction if configured.
      Result[i].date := JSToDateTime(FindPath('date').AsInt64);

      // Classify reading level relative to configured thresholds.
      Result[i].level := getLevel(Result[i].val);
    end;
end;

{------------------------------------------------------------------------------
  Provide parameter label captions for Settings UI (NightScout backend).
------------------------------------------------------------------------------}
class function NightScout.ParamLabel(Index: integer): string;
begin
  case Index of
  1:
    Result := 'NightScout URL';
  2:
    Result := 'API Secret';
  else
    Result := inherited ParamLabel(Index);
  end;
end;

end.
