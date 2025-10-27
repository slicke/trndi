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
unit trndi.api.nightscout3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.types, trndi.api, trndi.native,
  fpjson, jsonparser, jsonscanner, dateutils, StrUtils;

const
  {** Base path for Nightscout v3 API endpoints (appended to the provided base URL). }
  NS3_URL_BASE = '/api/v3/';

  {** Default paths/endpoints. }
  NS3_STATUS = 'status.json';
  NS3_ENTRIES = 'entries.json';
  NS3_SETTINGS = 'settings.json';

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

    function BuildAuthURL: string;
    function GetAuthToken(out Err: string): boolean;
    function BearerHeader: string;
    // Fetch thresholds using legacy Nightscout status (v1) similar to the v2 controller
    function FetchLegacyThresholds: boolean;
  public
    constructor Create(user, pass, extra: string); override;
    function connect: boolean; override;
    function getReadings(minNum, maxNum: integer; extras: string;
      out res: string): BGResults; override;
    {** UI parameter label provider (override).
        1: NightScout URL
        2: Auth token suffix (v2)
        3: (unused)
     }
    class function ParamLabel(Index: integer): string; override;
  published
    property siteBase: string read FSiteBase;  // e.g. https://example.com
    property token: string read FToken;        // JWT when connected
    // For parity with v2 unit; exposes the effective API base URL in use
    property remote: string read baseUrl;
  end;

implementation

type
  // Minimal auth response structure from /api/v2/authorization/request/<suffix>
  TNSAuthResponse = class
  public
    token: string;
  end;

{------------------------------------------------------------------------------
  Helper: Normalize and store site base and API base URL.
 ------------------------------------------------------------------------------}
constructor NightScout3.Create(user, pass, extra: string);
begin
  // Normalize site base (no trailing slash)
  FSiteBase := TrimRightSet(user, ['/']);
  FTokenSuffix := pass; // In v3, we expect 'pass' to be the token suffix for v2 auth

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
  Result := False;
  Err := '';

  if FTokenSuffix = '' then
  begin
    Err := 'Missing token suffix for Nightscout v2 authorization.';
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
      begin
        Err := 'Unexpected auth JSON shape';
      end;
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
  Result := False;
  lastErr := '';

  // 1) Acquire JWT via v2 authorization flow
  if not GetAuthToken(authErr) then
  begin
    lastErr := authErr;
    Exit;
  end;

  // 2) Fetch v3 status (bearer)
  resp := native.request(False, NS3_STATUS, [], '', BearerHeader);

  if Trim(resp) = '' then
  begin
    // Try fallback to v1 status.json without relying on baseUrl
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := 'Did not receive any data from the server!';
      Exit;
    end;
  end;

  if (resp <> '') and (resp[1] = '+') then
  begin
    // Try fallback to v1 status.json as absolute URL
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := TrimLeftSet(resp, ['+']);
      Exit;
    end;
  end;

  if Pos('Unau', resp) > 0 then
  begin
    // Fallback to v1 status.json (some setups may allow public status)
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := 'Unauthorized: invalid or expired token';
      Exit;
    end;
  end;

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

  Result := True;
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
  Result := False;

  // Attempt via native.request using absolute v1 URL and bearer header (no prefix)
  resp := native.request(False, FSiteBase + '/api/v1/status.json',
    [], '', BearerHeader, False {no prefix});

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
    if js.JSONType <> jtObject then Exit;
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
      Result := True;
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
  deltaValue: single;
  rssiValue, noiseValue, currentSgv, prevSgv: integer;
  j: integer;
  tempReading: BGReading;

  function ExtractArrayNode(const jd: TJSONData): TJSONData;
  var
    jo: TJSONObject;
  begin
    Result := nil;
    if not Assigned(jd) then Exit;

    if jd.JSONType = jtArray then
      Exit(jd)
    else if jd.JSONType = jtObject then
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
    resp := native.request(False, extras, params, '', BearerHeader);
  except
    Exit; // return empty set
  end;

  res := resp;

  // If v3 denied or errored, fall back to v1 entries
  if (Trim(resp) = '') or (Pos('Unauthorized', resp) > 0) or
    ((resp <> '') and (resp[1] = '+')) then
  begin
    // v1 typically supports count parameter and returns newest-first
    SetLength(fbparams, 1);
    fbparams[0] := 'count=' + IntToStr(maxNum);
    resp := native.request(False, FSiteBase + '/api/v1/entries.json',
      fbparams, '', BearerHeader, False {no prefix});
    if Trim(resp) = '' then Exit;
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
    resp := native.request(False, FSiteBase + '/api/v1/entries.json',
      fbparams, '', BearerHeader, False {no prefix});
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

      Result[i].Init(mgdl, Self.ToString);

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
        if i < arrNode.Count - 1 then
        begin
          // Get the previous (older) reading's SGV
          prevSgv := arrNode.FindPath(Format('[%d].sgv', [i + 1])).AsInteger;
          deltaValue := single(currentSgv - prevSgv);
        end
        else
          // Last (oldest) entry has no previous reading to compare
          deltaValue := 0;
      end;

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

      Result[i].update(currentSgv, deltaValue);
      Result[i].updateEnv(dev, rssiValue, noiseValue);

      // Trend mapping by name
      s := FindPath('direction').AsString;
      for t in BGTrend do
      begin
        if BG_TRENDS_STRING[t] = s then
        begin
          Result[i].trend := t;
          break;
        end;
        Result[i].trend := TdNotComputable;
      end;

      // Use ms epoch when available
      if Assigned(FindPath('date')) then
        Result[i].date := JSToDateTime(FindPath('date').AsInt64)
      else if Assigned(FindPath('srvModified')) then
        Result[i].date := JSToDateTime(FindPath('srvModified').AsInt64)
      else
        Result[i].date := Now; // fallback

      Result[i].level := getLevel(Result[i].val);
    end;

  js.Free;

  // Data from Nightscout v3 should come in descending order (newest first) due to sort=-date
  // However, check and reverse if needed for compatibility
  if (Length(Result) > 1) and (Result[0].date < Result[Length(Result) - 1].date) then
  begin
    // Data came in ascending order (oldest first) - need to reverse
    for i := 0 to (Length(Result) div 2) - 1 do
    begin
      j := Length(Result) - 1 - i;
      tempReading := Result[i];
      Result[i] := Result[j];
      Result[j] := tempReading;
    end;
  end;

  // Recalculate deltas for all readings (newest should be first)
  for i := 0 to Length(Result) - 1 do
  begin
    if i < Length(Result) - 1 then
    begin
      deltaValue := single(Result[i].val - Result[i + 1].val);
      Result[i].update(Result[i].val, deltaValue);
    end
    else
    begin
      // Last (oldest) entry has no previous reading
      Result[i].update(Result[i].val, 0);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Provide parameter label captions for Settings UI (NightScout v3 backend).
------------------------------------------------------------------------------}
class function NightScout3.ParamLabel(Index: integer): string;
begin
  case Index of
    1: Result := 'NightScout URL';
    2: Result := 'Auth token suffix';
    else
      Result := inherited ParamLabel(Index);
  end;
end;

end.
