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
 * - 2026-02-03: Nightscout v3 driver now prefers APIv3 endpoints without the
 *   legacy ".json" suffix (with automatic fallback), and normalizes the v2
 *   access token so both "abc123" and "token=abc123" work.
 *)
unit trndi.api.nightscout3;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, trndi.types, trndi.api, trndi.native, trndi.funcs,
fpjson, jsonparser, jsonscanner, dateutils, StrUtils, trndi.log;

const
  {** Base path for Nightscout v3 API endpoints (appended to the provided base URL). }
NS3_URL_BASE = '/api/v3/';

  {** Default paths/endpoints. }
// APIv3 tutorial uses endpoints without the legacy ".json" suffix.
// Keep ".json" variants for compatibility with older deployments.
NS3_STATUS = 'status';
NS3_STATUS_JSON = 'status.json';
NS3_ENTRIES = 'entries';
NS3_ENTRIES_JSON = 'entries.json';
NS3_SETTINGS = 'settings';
NS3_SETTINGS_JSON = 'settings.json';

type
  {** NightScout v3 API client with bearer authorization.
      Obtains a JWT using the v2 authorization request flow, then performs
      v3 API calls using the Authorization: Bearer header. Compatible with
      TrndiAPI. }
NightScout3 = class(TrndiAPI)
private
  FSiteBase: string;    // Base site URL (no trailing slash), e.g. https://example.com
  FToken: string;       // Bearer JWT token (obtained via v2 authorization)
  FTokenSuffix: string; // Token suffix used in v2 authorization

  class function NormalizeV2AccessToken(const AccessToken: string): string;
  function TryRequestV3(const PathPreferred, PathLegacyJson: string;
    const Params: array of string; out Resp: string): boolean;

  function BuildAuthURL: string;
  function GetAuthToken(out Err: string): boolean;
  function BearerHeader: string;
    // Fetch thresholds using legacy Nightscout status (v1) similar to the v2 controller
  function FetchLegacyThresholds: boolean;
public
  constructor Create(user, pass: string); overload;
  function connect: boolean; override;
  function getReadings(minNum, maxNum: integer; extras: string;
    out res: string): BGResults; override;
  function supportsBasal: boolean; override;
  function getBasalProfile(out profile: TBasalProfile): boolean; override;
    {** Test NightScout credentials
    }   
  class function testConnection(user, pass: string; var res: string): maybebool; override;
    {** UI parameter label provider (override).
        1: NightScout URL
        2: Auth token suffix (v2)
        3: (unused)
     }
  class function ParamLabel(LabelName: APIParamLabel): string; override;
published
  property siteBase: string read FSiteBase;  // e.g. https://example.com
  property token: string read FToken;        // JWT when connected
    // For parity with v2 unit; exposes the effective API base URL in use
  property remote: string read baseUrl;
protected
    {** Get the value which represents the maximum reading for the backend
     }
  function getLimitHigh: integer; override;
  
    {** Get the value which represents the minimum reading for the backend
     }
  function getLimitLow: integer; override;

    {** gets the name of the API
    }
  function getSystemName: string; override;

{** Get the maximum age (in minutes) of readings provided by the backend
        @returns(Maximum age in minutes)
     }
  function getMaxAge: integer; override;

    {** Retrieve the current basal rate from the Nightscout server.
        Fetches basal rate data from the server's profile/basal endpoints.
        @returns(Current basal rate in U/hr, or 0 if unavailable)
     }
  function getBasalRate: single; override;
end;

implementation

resourcestring
sParamUsername = 'NightScout URL';
sParamPassword = 'Auth token';
sParamDesc = '** ALPHA DRIVER - Please use "NightScout" for daily use! **' + #13#10 +
  'NightScout v3 setup (use FULL access token):' + #13#10#13#10 +
  '1) Open your NightScout site (e.g., https://your-site).' + #13#10 +
  '2) Go to Admin -> Tokens — or API Secret.' + #13#10 +
  '3) If you use Tokens:' + #13#10 + '   - Create a token with at least READ scope.' +
  #13#10 + '   - Copy the FULL access token value exactly as shown.' + #13#10 +
  '4) In Trndi:' + #13#10 + '   - Address: enter your NightScout URL' + #13#10 +
  '   - Auth: paste the FULL access token.' + #13#10 + #13#10 +
  'Tip: Both "abc123" and "token=abc123" formats are accepted.' + #13#10 +
  'Note: If you instead use the legacy API Secret, paste your API Secret value as-is.' + #10#13 +
  'Note 2: Your access token should look like: trndi-abc123 (or whatever name you chose)';
sParamDescHTML =
  '<div style="font-family: Arial, sans-serif; line-height: 1.6;">' +
  '<div style="background: #dc3545; color: white; padding: 15px; border-radius: 6px; margin-bottom: 20px; font-weight: bold; text-align: center; border: 2px solid #c82333;">' +
  '⚠️ ALPHA DRIVER - Please use "NightScout" for daily use! ⚠️' +
  '</div>' +
  '<h2 style="margin-bottom: 10px;">🌙 NightScout v3 Setup</h2>' +
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
  '<li><strong>Auth:</strong> paste the FULL access token.</li>' +
  '</ul>' +
  '</li>' +
  '</ol>' +
  '<div style="border-left: 4px solid #0d6efd; padding: 12px; margin-top: 15px; border-radius: 4px;">' +
  '<p style="margin: 0;"><strong>💡 Tip:</strong> Both <code style="background: #6F8FAF; padding: 2px 6px; border-radius: 3px;">abc123</code> and <code style="background: #6F8FAF; padding: 2px 6px; border-radius: 3px;">token=abc123</code> are accepted.</p>' +
  '</div>' +
  '<div style="border-left: 4px solid #ffc107; padding: 12px; margin-top: 15px; border-radius: 4px;">' +
  '<p style="margin: 0 0 8px 0;"><strong>📝 Note:</strong> If you instead use the legacy API Secret, paste your API Secret value as-is.</p>' +
  '<p style="margin: 0;"><strong>📝 Note 2:</strong> Your access token should look like: <code style="background: #6F8FAF; padding: 2px 6px; border-radius: 3px;">trndi-abc123</code> (or whatever name you chose).</p>' +
  '</div>' +
  '</div>';

class function NightScout3.NormalizeV2AccessToken(const AccessToken: string): string;
var
  t: string;
begin
  t := Trim(AccessToken);
  if t = '' then
    Exit('');

  // Nightscout's v2 auth request expects a path segment like "token=<name>-<secret>".
  // Accept both raw token value and already-prefixed input.
  if (Length(t) >= 6) and (LowerCase(Copy(t, 1, 6)) = 'token=') then
    Exit(t);

  Exit('token=' + t);
end;

function NightScout3.TryRequestV3(const PathPreferred, PathLegacyJson: string;
  const Params: array of string; out Resp: string): boolean;
begin
  Resp := native.request(false, PathPreferred, Params, '', BearerHeader);
  if (Trim(Resp) <> '') and not ((Resp <> '') and (Resp[1] = '+')) then
    Exit(true);

  Resp := native.request(false, PathLegacyJson, Params, '', BearerHeader);
  Result := (Trim(Resp) <> '') and not ((Resp <> '') and (Resp[1] = '+'));
end;

{------------------------------------------------------------------------------
  getMaxAge
  --------------------
  Returns the maximum age (in minutes) of readings provided by the backend
 ------------------------------------------------------------------------------}
function NightScout3.getMaxAge: integer;
begin
  result := -1; // No specific maximum age enforced
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function NightScout3.getSystemName: string;
begin
  result := 'NightScout v3';
end;

{------------------------------------------------------------------------------
  Helper: Normalize and store site base and API base URL.
 ------------------------------------------------------------------------------}
constructor NightScout3.Create(user, pass: string);
begin
  // Normalize site base (no trailing slash)
  FSiteBase := TrimRightSet(user, ['/']);
  // Nightscout v3 uses v2 auth request flow, which expects a path segment
  // like "token=<accessToken>".
  FTokenSuffix := NormalizeV2AccessToken(pass);

  // Set UA and API base URL before inherited (so native is initialized correctly)
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := FSiteBase + NS3_URL_BASE;

  inherited;
end;

function NightScout3.BearerHeader: string;
begin
  if FToken <> '' then
    Result := 'Authorization=Bearer ' + FToken
  else
    Result := '';
end;

function NightScout3.BuildAuthURL: string;
begin
  // v2 authorization endpoint lives under /api/v2/
  Result := FSiteBase + '/api/v2/authorization/request/' + FTokenSuffix;
end;

{------------------------------------------------------------------------------
  GetAuthToken
  -------------
  Perform a GET against the v2 authorization endpoint to retrieve a JWT.
 ------------------------------------------------------------------------------}
function NightScout3.GetAuthToken(out Err: string): boolean;
var
  url, res: string;
  js: TJSONData;
begin
  Result := false;
  Err := '';

  if FTokenSuffix = '' then
  begin
    Err := 'Missing access token for Nightscout v2 authorization.';
    Exit;
  end;

  url := BuildAuthURL;
  if not TrndiNative.getURL(url, res) then
  begin
    Err := 'Failed to contact Nightscout auth endpoint';
    Exit;
  end;

  if Trim(res) = '' then
  begin
    Err := 'Empty response from Nightscout auth endpoint';
    Exit;
  end;

  try
    js := GetJSON(res);
    try
      if (js.JSONType = jtObject) and (TJSONObject(js).IndexOfName('token') <> -1) then
      begin
        FToken := TJSONObject(js).Get('token');
        Result := FToken <> '';
        if not Result then
          Err := 'Auth token missing from response';
      end
      else
        Err := 'Unexpected auth JSON shape';
    finally
      js.Free;
    end;
  except
    on E: Exception do
      Err := 'Auth JSON parse error: ' + E.Message;
  end;
end;

{------------------------------------------------------------------------------
  connect
  -------
  Acquire bearer JWT, fetch status.json from v3 for time calibration,
  and always fetch thresholds from settings.json.
 ------------------------------------------------------------------------------}
function NightScout3.connect: boolean;
var
  resp: string;
  js: TJSONData;
  o, topObj, settings, thresholds: TJSONObject;
  serverEpoch: int64;
  UTCDateTime: TDateTime;
  authErr: string;
  node: TJSONData;
  // no thresholds parsing from v3 status; handled by FetchLegacyThresholds
  // we no longer try to read thresholds from status
begin
  Result := false;
  lastErr := '';

  // 1) Acquire JWT via v2 authorization flow (only if a token suffix is configured)
  if FTokenSuffix <> '' then
  begin
    if not GetAuthToken(authErr) then
    begin
      lastErr := authErr;
      Exit;
    end;
  end;

  // 2) Fetch v3 status (bearer). Prefer /api/v3/status, fall back to status.json.
  TryRequestV3(NS3_STATUS, NS3_STATUS_JSON, [], resp);
  {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams([])]));{$endif}

  if Trim(resp) = '' then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := 'Did not receive any data from the server!';
      Exit;
    end// Try fallback to v1 status.json without relying on baseUrl
  ;

  if (resp <> '') and (resp[1] = '+') then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := TrimLeftSet(resp, ['+']);
      Exit;
    end// Try fallback to v1 status.json as absolute URL
  ;

  if Pos('Unau', resp) > 0 then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := 'Unauthorized: invalid or expired token';
      Exit;
    end// Fallback to v1 status.json (some setups may allow public status)
  ;

  // 3) Parse JSON
  try
    js := GetJSON(resp);
    try
      if js.JSONType <> jtObject then
      begin
        lastErr := 'Unexpected JSON structure (not an object).';
        Exit;
      end;

      o := TJSONObject(js);

      // Some Nightscout v3 endpoints wrap payload in { status, result: { ... } }
      topObj := o;
      if (o.IndexOfName('result') <> -1) and (o.Find('result').JSONType = jtObject) then
        topObj := TJSONObject(o.Find('result'));

      // server time in ms epoch: prefer srvDate (v3), else fall back to serverTimeEpoch
      serverEpoch := topObj.Get('srvDate', int64(0));
      if serverEpoch <= 0 then
        serverEpoch := topObj.Get('serverTimeEpoch', int64(0));

      // thresholds are not parsed from status anymore

    finally
      // keep js for serverEpoch check below? No, parsed needed values already
      js.Free;
    end;
  except
    on E: Exception do
    begin
      lastErr := 'JSON parse error: ' + E.Message;
      Exit;
    end;
  end;

  // 4) Time calibration
  // serverEpoch now set during JSON parse; no further JSON access here

  if serverEpoch > 0 then
  begin
    UTCDateTime := UnixToDateTime(serverEpoch div 1000);
    // Calculate time difference: server time (UTC) minus local time (not converted to UTC)
    // This accounts for both clock skew and timezone offset
    timeDiff := Round((UTCDateTime - Now) * 86400);
    // Set tz so JSToDateTime applies the correction to reading timestamps
    tz := timeDiff;
  end
  else
  begin
    timeDiff := 0;
    tz := 0;
  end;

  // 5) Fetch thresholds using legacy status (aligns with v2 controller semantics)
  FetchLegacyThresholds; // thresholds remain defaults if this fails

  Result := true;
end;

{------------------------------------------------------------------------------
  FetchLegacyThresholds
  ---------------------
  Try to read thresholds from Nightscout v1 status.json (settings.thresholds),
  first attempting an authenticated request via native.request with baseUrl
  temporarily redirected to /api/v1/, then falling back to an unauthenticated
  absolute GET. Returns true if thresholds were found and set.
 ------------------------------------------------------------------------------}
function NightScout3.FetchLegacyThresholds: boolean;
var
  resp: string;
  js: TJSONData;
  o, settings, th: TJSONObject;
begin
  Result := false;

  // Attempt via native.request using absolute v1 URL and bearer header (no prefix)
  resp := native.request(false, FSiteBase + '/api/v1/status.json',
    [], '', BearerHeader, false {no prefix});
  {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams([])]));{$endif}

  // If empty or app-level error, try plain GET
  if (Trim(resp) = '') or ((resp <> '') and (resp[1] = '+')) then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
      Exit;

  try
    js := GetJSON(resp);
  except
    Exit;
  end;

  try
    if js.JSONType <> jtObject then
      Exit;
    o := TJSONObject(js);
    settings := nil;
    if (o.IndexOfName('settings') <> -1) and (o.Find('settings').JSONType =
      jtObject) then
      settings := TJSONObject(o.Find('settings'));
    if Assigned(settings) and (settings.Find('thresholds') <> nil) and
      (settings.Find('thresholds').JSONType = jtObject) then
    begin
      th := TJSONObject(settings.Find('thresholds'));
      cgmHi := th.Get('bgHigh', 0);
      cgmLo := th.Get('bgLow', 0);
      cgmRangeHi := th.Get('bgTargetTop', 0);
      cgmRangeLo := th.Get('bgTargetBottom', 0);
      Result := true;
    end;
  finally
    js.Free;
  end;
end;

{------------------------------------------------------------------------------
  getReadings
  -----------
  Fetch SGV entries via v3 entries.json. Supports either the v3 object shape
  {status, result: [...]} or a direct array for robustness.
 ------------------------------------------------------------------------------}
function NightScout3.getReadings(minNum, maxNum: integer; extras: string;
out res: string): BGResults;
var
  resp: string;
  js, arrNode: TJSONData;
  i: integer;
  t: BGTrend;
  s, dev: string;
  params: array of string;
  fbparams: array of string;
  oldBase: string;
  deltaField, rssiField, noiseField: TJSONData;
  deltaValue: glucose;
  currentSgv, prevSgv: integer;
  j: integer;
  tempReading: BGReading;
  rssiValue, noiseValue: maybeInt;

function ExtractArrayNode(const jd: TJSONData): TJSONData;
  var
    jo: TJSONObject;
  begin
    Result := nil;
    if not Assigned(jd) then
      Exit;

    if jd.JSONType = jtArray then
      Exit(jd)
    else
    if jd.JSONType = jtObject then
    begin
      jo := TJSONObject(jd);
      if (jo.IndexOfName('result') <> -1) and (jo.Find('result').JSONType = jtArray) then
        Exit(jo.Find('result'))
      else
        Exit(nil);
    end
    else
      Exit(nil);
  end;

var ts, ts2: int64;
LDateMs: int64;
LDateStr: string ;
LUtcOffset: integer;
LMethod: string;
LUnixTrue, LUnixFalse: TDateTime;
LLocalOffsetMin: integer;


begin
  // Default endpoint
  if extras = '' then
    extras := NS3_ENTRIES;

  // Build v3 query params
  // Nightscout v3 API may have different sort syntax than v1/v2
  // Try using sort$desc for descending order
  SetLength(params, 3);
  params[0] := 'limit=' + IntToStr(maxNum);
  params[1] := 'sort$desc=date';  // Try v3 syntax for descending sort
  params[2] := 'fields=date,sgv,delta,direction,device,rssi,noise';

  try
    // Prefer /api/v3/entries and fall back to entries.json when using default.
    if extras = NS3_ENTRIES then
    begin
      if not TryRequestV3(NS3_ENTRIES, NS3_ENTRIES_JSON, params, resp) then
        resp := '';
    end
    else
      resp := native.request(false, extras, params, '', BearerHeader);
    {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams(params)]));{$endif}
  except
    lastErr := 'Could not contact Nightscout entries endpoint (request failed)';
    Exit; // return empty set
  end;

  res := resp;

  // If v3 denied or errored, fall back to v1 entries
  if Trim(resp) = '' then
  begin
    lastErr := 'Empty response from Nightscout v3 entries endpoint (auth may be required)';
  end
  else if Pos('Unauthorized', resp) > 0 then
  begin
    lastErr := 'Unauthorized accessing Nightscout v3 entries (invalid or missing token)';
  end;

  if (Trim(resp) = '') or (Pos('Unauthorized', resp) > 0) or
    ((resp <> '') and (resp[1] = '+')) then
  begin
    // v1 typically supports count parameter and returns newest-first
    SetLength(fbparams, 1);
    fbparams[0] := 'count=' + IntToStr(maxNum);
    resp := native.request(false, FSiteBase + '/api/v1/entries.json',
      fbparams, '', BearerHeader, false {no prefix});
    {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams(fbParams)]));{$endif}
    if Trim(resp) = '' then
    begin
      lastErr := 'Empty response from Nightscout v1 entries endpoint (fallback failed)';
      Exit;
    end;
    res := resp;
  end;

  // Parse JSON and extract entries array
  js := nil;
  try
    js := GetJSON(resp);
  except
    on E: Exception do
      Exit; // broken payload; return empty
  end;

  arrNode := ExtractArrayNode(js);
  if (arrNode = nil) or (arrNode.JSONType <> jtArray) then
  begin
    // Try v1 fallback if we haven't already and current payload isn't an array
    SetLength(fbparams, 1);
    fbparams[0] := 'count=' + IntToStr(maxNum);
    resp := native.request(false, FSiteBase + '/api/v1/entries.json',
      fbparams, '', BearerHeader, false {no prefix});
    {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams(fbparams)]));{$endif}
    if Trim(resp) = '' then
    begin
      js.Free;
      Exit;
    end;
    FreeAndNil(js);
    try
      js := GetJSON(resp);
    except
      on E: Exception do
      begin
        js.Free;
        Exit;
      end;
    end;
    arrNode := ExtractArrayNode(js);
    if (arrNode = nil) or (arrNode.JSONType <> jtArray) then
    begin
      js.Free;
      Exit;
    end;
  end;

  SetLength(Result, arrNode.Count);
  for i := 0 to arrNode.Count - 1 do
    with arrNode.FindPath(Format('[%d]', [i])) do
    begin
      dev := FindPath('device').AsString;

      Result[i].Init(mgdl, Self.SystemName);

      // Get current SGV value
      currentSgv := FindPath('sgv').AsInteger;

      // Value and trend delta.
      // Some Nightscout entries may not include delta field
      deltaField := FindPath('delta');
      if Assigned(deltaField) then
        deltaValue := single(deltaField.AsFloat)
      else
      if i < arrNode.Count - 1 then
      begin
          // Get the previous (older) reading's SGV
        prevSgv := arrNode.FindPath(Format('[%d].sgv', [i + 1])).AsInteger;
        deltaValue := single(currentSgv - prevSgv);
      end
      else
          // Last (oldest) entry has no previous reading to compare
        deltaValue := 0// Calculate delta manually from previous reading
// Nightscout returns entries in reverse chronological order (newest first)
      ;

      // Receiver environment details (optional fields in Nightscout).
      rssiField := FindPath('rssi');
      noiseField := FindPath('noise');
      if Assigned(rssiField) then
      begin
        rssiValue.value := rssiField.AsInteger;
        rssivalue.exists := rssiValue.value <> -1;
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
        noiseValue.exists := true;
      end;

      Result[i].update(currentSgv, deltaValue);
      Result[i].updateEnv(dev, rssiValue, noiseValue);

      // Trend mapping by name
      s := FindPath('direction').AsString;
      // Default to not computable, then try to find a matching textual mapping
      Result[i].trend := TdNotComputable;
      for t := Low(BGTrend) to High(BGTrend) do
      begin
        if BG_TRENDS_STRING[t] = s then
        begin
          Result[i].trend := t;
          Break;
        end;
      end;

      // Use ms epoch when available. Prefer explicit utcOffset when provided
      LDateMs := 0;
      LDateStr := '';
      LUtcOffset := 0;

      if Assigned(FindPath('date')) then
        LDateMs := FindPath('date').AsInt64;
      if Assigned(FindPath('dateString')) then
        LDateStr := FindPath('dateString').AsString;
      if Assigned(FindPath('utcOffset')) then
        LUtcOffset := FindPath('utcOffset').AsInteger;

      if LDateMs <> 0 then
      begin
        // Timestamp in ms since epoch
        ts := LDateMs div 1000;
        // Prefer explicit ISO date string (UTC) when available to avoid double-applying UTC offsets.
        if LDateStr <> '' then
        begin
          // dateString is ISO 8601 with Z (UTC).
          // If we have a tz calibration, apply it (JSToDateTime) so we correct for server clock skew.
          // If tz is zero (no calibration), convert to system local time using UnixToDateTime(ts, True).
          if tz <> 0 then
          begin
            Result[i].date := JSToDateTime(LDateMs, True);
            LMethod := 'dateString';
          end
          else
          begin
            Result[i].date := UnixToDateTime(ts, False); // system-local conversion (UseUTC=false gives local time on this platform)
            LMethod := 'dateString+local';
          end;
        end
        else if LUtcOffset <> 0 then
        begin
          // No dateString available: apply server-provided utcOffset to UTC epoch
          Result[i].date := UnixToDateTime(ts, False) + (LUtcOffset / 1440); // minutes -> days
          LMethod := 'utcOffset';
        end
        else
        begin
          Result[i].date := JSToDateTime(LDateMs, True);
          LMethod := 'JSTo';
        end;
      end
      else if Assigned(FindPath('srvModified')) then
      begin
        LDateMs := FindPath('srvModified').AsInt64;
        ts2 := LDateMs div 1000;
        if LDateStr <> '' then
        begin
          Result[i].date := UnixToDateTime(ts2, False);
          LMethod := 'dateString';
        end
        else if Assigned(FindPath('utcOffset')) then
        begin
          Result[i].date := UnixToDateTime(ts2, False) + (FindPath('utcOffset').AsInteger / 1440);
          LMethod := 'utcOffset';
        end
        else
        begin
          Result[i].date := JSToDateTime(LDateMs, True);
          LMethod := 'JSTo';
        end;
      end
      else
      begin
        Result[i].date := Now; // fallback
        LMethod := 'Now';
      end;

      // Extra diagnostics: compare UnixToDateTime(true/false) and show system local offset
      try
        LUnixTrue := UnixToDateTime(ts, True);
        LUnixFalse := UnixToDateTime(ts, False);
        LLocalOffsetMin := Round((Now - LocalTimeToUniversal(Now)) * 1440); // minutes
        LogMessageToFile('[' + {$i %file%} + ':' + {$i %Line%} + '] debug: unixTrue=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', LUnixTrue) +
          ' unixFalse=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', LUnixFalse) + ' localOffsetMin=' + IntToStr(LLocalOffsetMin));
      except
        // ignore diagnostics failure
      end;

      // Diagnostic log to help debug timezone/timestamp issues
      try
        // Use simple concatenation to avoid Format exceptions while debugging
        LogMessageToFile('[' + {$i %file%} + ':' + {$i %Line%} + '] NightScout entry ' + IntToStr(i) +
          ': dateMs=' + IntToStr(LDateMs) + ' dateString="' + LDateStr + '" utcOffset=' + IntToStr(LUtcOffset) + ' method=' + LMethod + ' tz=' + IntToStr(tz) +
          ' computed=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Result[i].date));
      except
        on E: Exception do
          LogMessageToFile('[' + {$i %file%} + ':' + {$i %Line%} + '] NightScout entry ' + IntToStr(i) + ': diagnostic log failed: ' + E.Message);
      end;

      Result[i].level := getLevel(Result[i].val);
    end;

  js.Free;

  // Data from Nightscout v3 should come in descending order (newest first) due to sort=-date
  // However, check and reverse if needed for compatibility
  if (Length(Result) > 1) and (Result[0].date < Result[Length(Result) - 1].date) then
    for i := 0 to (Length(Result) div 2) - 1 do
    begin
      j := Length(Result) - 1 - i;
      tempReading := Result[i];
      Result[i] := Result[j];
      Result[j] := tempReading;
    end// Data came in ascending order (oldest first) - need to reverse
  ;

  // Recalculate deltas for all readings (newest should be first)
  for i := 0 to Length(Result) - 1 do
    if i < Length(Result) - 1 then
    begin
      deltaValue := single(Result[i].val - Result[i + 1].val);
      Result[i].update(Result[i].val, deltaValue);
    end
    else
      Result[i].update(Result[i].val, 0)// Last (oldest) entry has no previous reading
  ;
end;

{------------------------------------------------------------------------------
  Provide parameter label captions for Settings UI (NightScout v3 backend).
------------------------------------------------------------------------------}
class function NightScout3.ParamLabel(LabelName: APIParamLabel): string;
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
  Test connection details for Nightscout v3
  - Performs a v2 authorization request if a token suffix (pass) is provided.
  - Probes v3 status.json using the retrieved bearer token (if available),
    falling back to a public v3 or v1 status endpoint where applicable.
 ------------------------------------------------------------------------------}
class function NightScout3.testConnection(user, pass: string; var res: string): MaybeBool;
var
  tn: TrndiNative;
  base, authURL, resp, localToken, xres: string;
  js: TJSONData;
  rootObj, topObj: TJSONObject;
  serverEpoch: int64;
begin
  res := 'An unknown error occured';
  Result := MaybeBool.False; // default to failure

  // Basic sanity checks for URL
  if (Copy(user, 1, 4) <> 'http') then
    Exit;

  base := TrimRightSet(user, ['/']);

  // If pass is provided, try v2 auth flow to obtain a token
  localToken := '';
  if (Trim(pass) <> '') then
  begin
    authURL := base + '/api/v2/authorization/request/' + NightScout3.NormalizeV2AccessToken(pass);
    if TrndiNative.getURL(authURL, xres) then
    begin
      // parse JSON and extract token
      try
        js := GetJSON(xres);
        try
          if (js.JSONType = jtObject) and (TJSONObject(js).IndexOfName('token') <> -1) then
            localToken := TJSONObject(js).Get('token');
        finally
          js.Free;
        end;
      except
        // JSON parse error -> treat as failure
        res := 'Could not read the response from the server';
        Result := MaybeBool.False;
        Exit;
      end;
    end
    else
    begin
      // Couldn't contact auth endpoint
      res := 'Could not connect to the authentication endpoint';
      Result := MaybeBool.False;
      Exit;
    end;
  end;

  // Try the v3 status endpoint with bearer header if we have a token
  tn := TrndiNative.Create('Mozilla/5.0 (compatible; trndi) TrndiAPI',
    base + NS3_URL_BASE);
  try
    if localToken <> '' then
    begin
      resp := tn.Request(false, NS3_STATUS, [], '', 'Authorization=Bearer ' + localToken);
      if (Trim(resp) = '') or ((resp <> '') and (resp[1] = '+')) then
        resp := tn.Request(false, NS3_STATUS_JSON, [], '', 'Authorization=Bearer ' + localToken);
    end
    else
    begin
      resp := tn.Request(false, NS3_STATUS, [], '', '');
      if (Trim(resp) = '') or ((resp <> '') and (resp[1] = '+')) then
        resp := tn.Request(false, NS3_STATUS_JSON, [], '', '');
    end;

    if Trim(resp) = '' then
      if not TrndiNative.getURL(base + '/api/v1/status.json', resp) then
      begin
        res := 'Could not fetch the glucose data endpoint';
        Result := MaybeBool.False;
        Exit;
      end// Try fallback to v1 absolute URL without auth
    ;

    // Application-level errors prefixed with '+'
    if (resp <> '') and (resp[1] = '+') then
    begin
      res := 'An application-level error occured: ' + resp;
      Result := MaybeBool.False;
      Exit;
    end;

    // Basic unauthorized check
    if Pos('Unau', resp) > 0 then
    begin
      res := 'The api key is wrong, or Trndi does not have enough access rights';
      Result := MaybeBool.False;
      Exit;
    end;

    // Parse JSON and require a valid server time
    try
      js := GetJSON(resp);
      try
        if js.JSONType <> jtObject then
        begin
          res := 'The response could not be parsed';
          Result := MaybeBool.False;
          Exit;
        end;
        rootObj := TJSONObject(js);
        topObj := rootObj;
        // v3 may have a result wrapper
        if (rootObj.IndexOfName('result') <> -1) and
          (rootObj.Find('result').JSONType = jtObject) then
          topObj := TJSONObject(rootObj.Find('result'));

        serverEpoch := topObj.Get('srvDate', int64(0));
        if serverEpoch <= 0 then
          serverEpoch := topObj.Get('serverTimeEpoch', int64(0));
        if serverEpoch <= 0 then
        begin
          res := 'The server time is not correct';
          Result := MaybeBool.False;
          Exit;
        end;
      finally
        js.Free;
      end;
    except
      on E: Exception do
      begin
        res := 'An error occured trying to contact the server: '#10 + E.Message;
        Result := MaybeBool.False;
        Exit;
      end;
    end;

    Result := MaybeBool.True; // success
  finally
    tn.Free;
  end;
end;

function NightScout3.getLimitHigh: integer;
begin
  result := 400;
end;

function NightScout3.getLimitLow: integer;
begin
  result := 40;
end;

function NightScout3.supportsBasal: boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  getBasalRate
  ------------
  Retrieve the current basal rate from the Nightscout v3 server.
  This fetches basal rate data from the server's profile endpoints.
 ------------------------------------------------------------------------------}
function NightScout3.getBasalRate: single;
var
  ResponseStr: string;
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
  
  // Fetch basal rate from Nightscout v3 API
  try
    ResponseStr := Native.Request(false, 'profile.json', [], '', BearerHeader);
    {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, 'profile.json', responsestr, debugParams([])]));{$endif}
    
    if Trim(ResponseStr) = '' then
    begin
      lastErr := 'No basal rate data received from server';
      Exit;
    end;

    // Parse JSON response
    try
      JSONData := GetJSON(ResponseStr);
      
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
    except
      on E: Exception do
      begin
        lastErr := 'Error parsing basal rate JSON: ' + E.Message;
        result := 0;
      end;
    end;
  except
    on E: Exception do
    begin
      lastErr := 'Error fetching basal rate: ' + E.Message;
      result := 0;
    end;
  end;
end;

function NightScout3.getBasalProfile(out profile: TBasalProfile): boolean;
var
  ResponseStr, defName: string;
  JSONData: TJSONData;
  RootObject, StoreObj: TJSONObject;
  StoreArray: TJSONArray;
  DefaultProfile: TJSONObject;
  BasalArray: TJSONArray;
  BasalObj: TJSONObject;
  ResNode, StoreNode: TJSONData;
  i: integer;
  tstr: string;
  h, m: integer;
  be: TBasalEntry;
begin
  Result := False;
  SetLength(profile, 0);
  try
    ResponseStr := Native.Request(false, 'profile.json', [], '', BearerHeader);
   {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, 'profile.json', responseStr, debugParams([])]));{$endif}
  except
    lastErr := 'HTTP request failed while fetching profile.json';
    Exit;
  end;

  if Trim(ResponseStr) = '' then
  begin
    lastErr := 'Empty response from profile.json';
    Exit;
  end;

  try
    JSONData := GetJSON(ResponseStr);
  except
    lastErr := 'Failed to parse JSON from profile.json';
    Exit;
  end;

  try
    if not (JSONData is TJSONObject) then
    begin
      lastErr := 'profile.json is not a JSON object';
      Exit;
    end;
    RootObject := TJSONObject(JSONData);
    // Some Nightscout instances wrap payloads in {"status":..,"result":[{...}]}
   ResNode := RootObject.FindPath('result');
    if Assigned(ResNode) and (ResNode.InheritsFrom(TJSONArray)) and (TJSONArray(ResNode).Count > 0) and (TJSONArray(ResNode).Items[0] is TJSONObject) then
      RootObject := TJSONObject(TJSONArray(ResNode).Items[0]);

    // `store` can be an array (old shape) or an object keyed by profile id (observed)
    StoreNode := RootObject.FindPath('store');
    if Assigned(StoreNode) then
    begin
      if StoreNode.InheritsFrom(TJSONArray) then
      begin
        StoreArray := TJSONArray(StoreNode);
        if StoreArray.Count = 0 then
        begin
          try
            with TStringList.Create do
            try
              Text := ResponseStr;
              SaveToFile('/tmp/trndi_profile_debug.json');
            finally
              Free;
            end;
            lastErr := 'Empty "store" array in profile.json (raw saved to /tmp/trndi_profile_debug.json)';
          except
            lastErr := 'Empty "store" array in profile.json (failed to save raw JSON)';
          end;
          Exit;
        end;
        DefaultProfile := StoreArray.Objects[0];
      end
      else if StoreNode.InheritsFrom(TJSONObject) then
      begin
        StoreObj := TJSONObject(StoreNode);
        defName := RootObject.Get('defaultProfile', '');
        if defName = '' then
          defName := RootObject.Get('Default', '');
        if defName <> '' then
          DefaultProfile := StoreObj.FindPath(defName) as TJSONObject
        else
        begin
          // fallback: take the first property object found inside store
          if StoreObj.Count > 0 then
            DefaultProfile := StoreObj.Items[0] as TJSONObject;
        end;
      end;
    end
    else
    begin
      try
        with TStringList.Create do
        try
          Text := ResponseStr;
          SaveToFile('/tmp/trndi_profile_debug.json');
        finally
          Free;
        end;
        lastErr := 'No "store" element in profile.json (raw saved to /tmp/trndi_profile_debug.json)';
      except
        lastErr := 'No "store" element in profile.json (failed to save raw JSON)';
      end;
      Exit;
    end;

    if not Assigned(DefaultProfile) then
    begin
      lastErr := 'No default profile found in profile.json store';
      Exit;
    end;

    BasalArray := DefaultProfile.FindPath('basal') as TJSONArray;
    if not Assigned(BasalArray) then
    begin
      lastErr := 'No "basal" array in default profile';
      Exit;
    end;

    SetLength(profile, BasalArray.Count);
    for i := 0 to BasalArray.Count - 1 do
    begin
      BasalObj := BasalArray.Objects[i];
      tstr := BasalObj.Get('time', '00:00');
      h := 0; m := 0;
      if Pos(':', tstr) > 0 then
      begin
        h := StrToIntDef(Copy(tstr, 1, Pos(':', tstr) - 1), 0);
        m := StrToIntDef(Copy(tstr, Pos(':', tstr) + 1, 2), 0);
      end
      else
      begin
        // If time is numeric (minutes), try parse as integer
        h := StrToIntDef(tstr, 0) div 60;
        m := StrToIntDef(tstr, 0) mod 60;
      end;

      be.startMin := (h * 60) + m;
      be.value := BasalObj.Get('value', single(0));
      be.name := BasalObj.Get('name', '');
      profile[i] := be;
    end;

    Result := True;
  finally
    JSONData.Free;
  end;
end;

end.
