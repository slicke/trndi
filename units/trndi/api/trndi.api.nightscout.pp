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

  {** Get the value which represents the maximum reading for the backend
   }
  function getLimitHigh: integer; override;
 
  {** Get the value which represents the minimum reading for the backend
   }
  function getLimitLow: integer; override;

  {** Get the name of the system }
  function getSystemName: string; override;
  

public
    {** Create a NightScout API client.
        Initializes the HTTP User-Agent, derives the API base URL from @code(user),
        prepares the API secret header from @code(pass), and calls the inherited
        constructor.

        @param(user  Base Nightscout URL, including scheme, e.g. https://example.com)
        @param(pass  Nightscout API secret in plain text; hashed to SHA1 for header)
        @param(extra Reserved for future use)
     }
  constructor Create(user, pass: string); override;

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
  class function ParamLabel(LabelName: APIParamLabel): string; override;
    {** Test NightScout credentials
    }
  class function testConnection(user, pass, extra: string): byte; override;

  function getMaxAge: integer; override;

    {** Retrieve the current basal rate from the Nightscout server.
        Fetches basal rate data from the server's profile/basal endpoints.
        @returns(Current basal rate in U/hr, or 0 if unavailable)
     }
  function getBasalRate: single; override;
private
    // (no private members)

published
    {** The remote Nightscout base URL actually used (read-only proxy to baseUrl). }
  property remote: string read baseUrl;
end;

implementation

resourcestring
sParamUsername = 'NightScout URL';
sParamPassword = 'API Secret';
sParamDesc = 'NightScout v2 setup (use FULL access token):' + #13#10#13#10 +
  '1) Open your NightScout site (e.g., https://your-site).' + #13#10 +
  '2) Go to Admin -> Tokens — or API Secret.' + #13#10 +
  '3) If you use Tokens:' + #13#10 + '   - Create a token with at least READ scope.' +
  #13#10 + '   - Copy the FULL access token value exactly as shown.' + #13#10 +
  '4) In Trndi:' + #13#10 + '   - Address: enter your NightScout URL' + #13#10 +
  '   - Auth: paste the FULL access token (not just a suffix).' + #13#10 + #13#10 +
  'Note: If you instead use the legacy API Secret, paste your API Secret value as-is.';
sParamDescHTML =
  '<div style="font-family: Arial, sans-serif; line-height: 1.6;">' +
  '<h2 style="margin-bottom: 10px;">🌙 NightScout v2 Setup</h2>' +
  '<p style="color: #7f8c8d; font-style: italic; margin-bottom: 15px;">(use FULL access token)</p>' +
  '<ol style="padding-left: 20px;">' +
  '<li style="margin-bottom: 10px;">Open your NightScout site (e.g., <code style="background: #6495ED; padding: 2px 6px; border-radius: 3px;">https://your-site</code>).</li>' +
  '<li style="margin-bottom: 10px;">Go to <strong>Admin → Tokens</strong> — or <strong>API Secret</strong>.</li>' +
  '<li style="margin-bottom: 10px;">If you use Tokens:' +
  '<ul style="margin-top: 5px; padding-left: 20px;">' +
  '<li>Create a token with at least <strong>READ</strong> scope.</li>' +
  '<li>Copy the <strong>FULL</strong> access token value exactly as shown.</li>' +
  '</ul>' +
  '</li>' +
  '<li style="margin-bottom: 10px;">In Trndi:' +
  '<ul style="margin-top: 5px; padding-left: 20px;">' +
  '<li><strong>Address:</strong> enter your NightScout URL</li>' +
  '<li><strong>Auth:</strong> paste the FULL access token (not just a suffix).</li>' +
  '</ul>' +
  '</li>' +
  '</ol>' +
  '<p style="border-left: 4px solid #ffc107; padding: 10px; margin-top: 15px;">' +
  '<strong>⚠️ Note:</strong> If you instead use the legacy API Secret, paste your API Secret value as-is.</p>' +
  '</div>';


{------------------------------------------------------------------------------
  getMaxAge
  --------------------
  Returns the maximum age (in minutes) of readings provided by the backend
 ------------------------------------------------------------------------------}
function NightScout.getMaxAge: integer;
begin 
  result := -1; // No specific maximum age enforced
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function NightScout.getSystemName: string;
begin
  result := 'NightScout';
end;

{------------------------------------------------------------------------------
  create (constructor)
  --------------------
  Initialize UA, derive API base URL from user, compute API-SECRET if present,
  and call inherited base constructor.
 ------------------------------------------------------------------------------}
constructor NightScout.Create(user, pass: string);
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
  deltaValue: glucose;
  currentSgv, prevSgv: integer;
  rssivalue, noiseValue: MaybeInt;
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
      Result[i].Init(mgdl, self.systemName);

      // Get current SGV value
      currentSgv := FindPath('sgv').AsInteger;

      // Value and trend delta.
      // Some Nightscout entries may not include delta field
      deltaField := FindPath('delta');
      if Assigned(deltaField) then
        deltaValue := single(deltaField.AsFloat)
      else
      if i < js.Count - 1 then
      begin
          // Get the previous (older) reading's SGV
        prevSgv := js.FindPath(Format('[%d].sgv', [i + 1])).AsInteger;
        deltaValue := single(currentSgv - prevSgv);
      end
      else
          // Last (oldest) entry has no previous reading to compare
        deltaValue := 0// Calculate delta manually from previous reading
// Nightscout returns entries in reverse chronological order (newest first)
      ;
      Result[i].update(currentSgv, deltaValue);

      // Receiver environment details (optional fields in Nightscout).
      rssiField := FindPath('rssi');
      noiseField := FindPath('noise');
      if Assigned(rssiField) then
      begin
        rssiValue.value := rssiField.AsInteger;
        rssivalue.exists := rssivalue.value <> -1;
      end
      else
      begin
        rssiValue.value := 0;
        rssivalue.exists := true;
      end;
      if Assigned(noiseField) then
      begin
        noiseValue.value := noiseField.AsInteger;
        noiseValue.exists := noiseValue.value <> -1;
      end
      else
      begin
        noiseValue.value := 0;
        noisevalue.exists := true;
      end;
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
class function NightScout.ParamLabel(LabelName: APIParamLabel): string;
begin
  case LabelName of
  APLUser:
    Result := sParamUsername;
  APLPass:
    Result := sParamPassword;
  APLDesc:
    Result := sParamDesc;
  APLDescHTML:
    Result := sParamDescHTML;
  APLCopyright:
    Result := 'Björn Lindh <github.com/slicke>';
  else
    Result := inherited ParamLabel(LabelName);
  end;
end;

{------------------------------------------------------------------------------
  Test if the connection data is correct
------------------------------------------------------------------------------}
class function NightScout.testConnection(user, pass, extra: string): byte;
var
  password, responseStr: string;
  tn: TrndiNative;
  JSONParser: TJSONParser;
  JSONData: TJSONData;
  RootObject: TJSONObject;
  ServerEpoch: int64;
begin
  // Create native with a correctly configured base URL (like constructor does)
  tn := TrndiNative.Create('Mozilla/5.0 (compatible; trndi) TrndiAPI',
    TrimRightSet(user, ['/']) + NS_URL_BASE);
    // 1) Quick sanity check on URL; avoids obvious mistakes early.
  if (Copy(user, 1, 4) <> 'http') then
  begin
    Result := 1;
    tn.Free;
    Exit;
  end;

  // 2) Probe server status and settings.
  //    Native.Request(signature): (useGet, path, params, body, header)
  password := IfThen(pass <> '', 'API-SECRET=' + SHA1Print(SHA1String(pass)), '');
  ResponseStr := tn.Request(false, NS_STATUS, [], '', password);

  // 3) Basic validation for empty payloads.
  if Trim(ResponseStr) = '' then
  begin
    Result := 1;
    tn.Free;
    Exit;
  end;

  // 4) Some backends may prefix '+' to indicate application-level errors.
  if (ResponseStr[1] = '+') then
  begin
    Result := 1;
    tn.Free;
    Exit;
  end;

  // 5) Coarse unauthorized detection (Nightscout messages vary by version).
  if Pos('Unau', ResponseStr) > 0 then
  begin
    Result := 1;
    tn.Free;
    Exit;
  end;
  // Optional: verify we got a valid JSON object and a reasonable server epoch
  try
    JSONParser := TJSONParser.Create(ResponseStr, [joUTF8, joIgnoreTrailingComma]);
    try
      JSONData := JSONParser.Parse;
    finally
      JSONParser.Free;
    end;

    if (JSONData is TJSONObject) then
    begin
      RootObject := TJSONObject(JSONData);
      ServerEpoch := RootObject.Get('serverTimeEpoch', int64(0));
      JSONData.Free;
      if ServerEpoch <= 0 then
      begin
        Result := 1; // No server epoch values: treat as failure for probing
        tn.Free;
        Exit;
      end;
    end
    else
    begin
      Result := 1;
      JSONData.Free;
      tn.Free;
      Exit;
    end;
  except
    on E: Exception do
    begin
      Result := 1;
      tn.Free;
      Exit;
    end;
  end;

  Result := 0; // success
  tn.Free;
end;

function NightScout.getLimitHigh: integer;
begin
  result := 400;
end; 

function NightScout.getLimitLow: integer;
begin
  result := 40;
end;

{------------------------------------------------------------------------------
  getBasalRate
  ------------
  Retrieve the current basal rate from the Nightscout server.
  This fetches basal rate data from the server's profile endpoints.
 ------------------------------------------------------------------------------}
function NightScout.getBasalRate: single;
var
  ResponseStr: string;
  JSONParser: TJSONParser;
  JSONData: TJSONData;
  RootObject: TJSONObject;
  StoreArray: TJSONArray;
  DefaultProfile: TJSONObject;
  BasalArray: TJSONArray;
  BasalEntry: TJSONObject;
  CurrentTime: TDateTime;
  CurrentMinutes: integer;
  i: integer;
begin
  result := 0;
  
  // Fetch basal rate from Nightscout API
  try
    ResponseStr := Native.Request(false, 'profile.json', [], '', Key);
    
    if Trim(ResponseStr) = '' then
    begin
      lastErr := 'No basal rate data received from server';
      Exit;
    end;

    // Parse JSON response
    JSONParser := TJSONParser.Create(ResponseStr, [joUTF8, joIgnoreTrailingComma]);
    try
      JSONData := JSONParser.Parse;
      
      if not (JSONData is TJSONObject) then
      begin
        JSONData.Free;
        Exit;
      end;
      
      RootObject := TJSONObject(JSONData);
      
      // Navigate to store array -> default profile -> basal array
      StoreArray := RootObject.FindPath('store') as TJSONArray;
      if Assigned(StoreArray) and (StoreArray.Count > 0) then
      begin
        DefaultProfile := StoreArray.Objects[0].FindPath('defaultProfile') as TJSONObject;
        if not Assigned(DefaultProfile) then
          DefaultProfile := StoreArray.Objects[0].FindPath('Default') as TJSONObject;
          
        if Assigned(DefaultProfile) then
        begin
          BasalArray := DefaultProfile.FindPath('basal') as TJSONArray;
          if Assigned(BasalArray) and (BasalArray.Count > 0) then
          begin
            // Get current time in minutes since midnight
            CurrentTime := Now;
            CurrentMinutes := HourOf(CurrentTime) * 60 + MinuteOf(CurrentTime);
            
            // Find the applicable basal rate for current time
            for i := BasalArray.Count - 1 downto 0 do
            begin
              BasalEntry := BasalArray.Objects[i];
              if Assigned(BasalEntry) then
              begin
                // Basal entries have 'time' and 'value' fields
                result := BasalEntry.Get('value', single(0));
                break;
              end;
            end;
          end;
        end;
      end;
      
      JSONData.Free;
    finally
      JSONParser.Free;
    end;
  except
    on E: Exception do
    begin
      lastErr := 'Error fetching basal rate: ' + E.Message;
      result := 0;
    end;
  end;
end;

end.
