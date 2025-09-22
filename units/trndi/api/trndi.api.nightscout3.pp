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
  NS3_STATUS   = 'status.json';
  NS3_ENTRIES  = 'entries.json';

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

  public
    constructor create(user, pass, extra: string); override;
    function connect: boolean; override;
    function getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults; override;
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
constructor NightScout3.create(user, pass, extra: string);
begin
  // Normalize site base (no trailing slash)
  FSiteBase := TrimRightSet(user, ['/']);
  FTokenSuffix := pass; // In v3, we expect 'pass' to be the token suffix for v2 auth

  // Set UA and API base URL before inherited (so native is initialized correctly)
  ua      := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
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
  Acquire bearer JWT, fetch status.json from v3, and extract thresholds and
  server time calibration.
 ------------------------------------------------------------------------------}
function NightScout3.connect: boolean;
var
  resp: string;
  js: TJSONData;
  o, topObj, settings, thresholds: TJSONObject;
  serverEpoch: int64;
  UTCDateTime: TDateTime;
  authErr: string;
  v1resp: string;
  v1js: TJSONData;
begin
  Result := false;
  lastErr := '';

  // 1) Acquire JWT via v2 authorization flow
  if not GetAuthToken(authErr) then
  begin
    lastErr := authErr;
    Exit;
  end;

  // 2) Fetch v3 status (bearer)
  resp := native.request(false, NS3_STATUS, [], '', BearerHeader);

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

      // serverTimeEpoch (milliseconds)
      serverEpoch := topObj.Get('serverTimeEpoch', int64(0));

      // Optional thresholds in settings.thresholds
      settings := topObj.Find('settings') as TJSONObject;
      if Assigned(settings) then
      begin
        thresholds := settings.Find('thresholds') as TJSONObject;
        if Assigned(thresholds) then
        begin
          cgmHi      := thresholds.Get('bgHigh', 0);
          cgmLo      := thresholds.Get('bgLow', 0);
          cgmRangeHi := thresholds.Get('bgTargetTop', 0);
          cgmRangeLo := thresholds.Get('bgTargetBottom', 0);
        end;
      end;

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
  if serverEpoch <= 0 then
  begin
    // Try fallback to v1 status.json to obtain server time (and optionally thresholds)
    if TrndiNative.getURL(FSiteBase + '/api/v1/status.json', v1resp) then
    begin
      v1js := nil;
      try
        v1js := GetJSON(v1resp);
        if Assigned(v1js) and (v1js.JSONType = jtObject) then
        begin
          serverEpoch := TJSONObject(v1js).Get('serverTimeEpoch', int64(0));

          // thresholds may also be present in v1; backfill if missing
          if (not Assigned(thresholds)) or
             ((cgmHi = 0) and (cgmLo = 0) and (cgmRangeHi = 0) and (cgmRangeLo = 0)) then
          begin
            settings := TJSONObject(v1js).Find('settings') as TJSONObject;
            if Assigned(settings) then
            begin
              thresholds := settings.Find('thresholds') as TJSONObject;
              if Assigned(thresholds) then
              begin
                if cgmHi = 0 then      cgmHi      := thresholds.Get('bgHigh', 0);
                if cgmLo = 0 then      cgmLo      := thresholds.Get('bgLow', 0);
                if cgmRangeHi = 0 then cgmRangeHi := thresholds.Get('bgTargetTop', 0);
                if cgmRangeLo = 0 then cgmRangeLo := thresholds.Get('bgTargetBottom', 0);
              end;
            end;
          end;
        end;
      finally
        if Assigned(v1js) then v1js.Free;
      end;
    end;

    // If still missing, do not hard fail at startup; continue with neutral timeDiff
    if serverEpoch <= 0 then
    begin
      serverEpoch := 0; // mark as missing, timeDiff will be 0
    end;
  end;

  if serverEpoch > 0 then
  begin
    UTCDateTime := UnixToDateTime(serverEpoch div 1000);
    timeDiff := SecondsBetween(UTCDateTime, LocalTimeToUniversal(Now));
    if timeDiff < 0 then
      timeDiff := 0;
    timeDiff := -timeDiff;
  end
  else
  begin
    timeDiff := 0;
  end;

  Result := true;
end;

{------------------------------------------------------------------------------
  getReadings
  -----------
  Fetch SGV entries via v3 entries.json. Supports either the v3 object shape
  {status, result: [...]} or a direct array for robustness.
 ------------------------------------------------------------------------------}
function NightScout3.getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults;
var
  resp: string;
  js, arrNode: TJSONData;
  i: integer;
  t: BGTrend;
  s, dev: string;
  params: array of string;

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
  SetLength(params, 2);
  params[0] := 'limit=' + IntToStr(maxNum);
  params[1] := 'sort$desc=date';

  try
    resp := native.request(false, extras, params, '', BearerHeader);
  except
    Exit; // return empty set
  end;

  res := resp;

  if Pos('Unauthorized', resp) > 0 then
    Exit;

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
    js.Free;
    Exit;
  end;

  SetLength(Result, arrNode.Count);
  for i := 0 to arrNode.Count - 1 do
  with arrNode.FindPath(Format('[%d]', [i])) do
  begin
    dev := FindPath('device').AsString;

    Result[i].Init(mgdl, Self.ToString);

    // Values
    Result[i].update(FindPath('sgv').AsInteger, single(FindPath('delta').AsFloat));
    Result[i].updateEnv(dev, FindPath('rssi').AsInteger, FindPath('noise').AsInteger);

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
