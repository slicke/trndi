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
unit trndi.api.xdrip;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, StrUtils, sha1,
  // Parent classes and modules
trndi.api.nightscout, trndi.api, trndi.native, trndi.types,
  // FPC/Lazarus units
DateUtils, Dialogs, fpjson, jsonparser;

(*******************************************************************************
  Constants for xDrip endpoints
 ******************************************************************************)

const
  {** Default endpoint path for SGV (sensor glucose value) entries in xDrip. }
XDRIP_READINGS = 'sgv.json';

const
  {** Endpoint used to fetch pebble-style status information (contains "now" timestamp). }
XDRIP_STATUS = 'pebble';

const
  {** Endpoint that may include current BG range settings (hi/lo targets). }
XDRIP_RANGES = 'status.json';

  {** Default REST API port used by xDrip+. Applied automatically when the
      user-supplied URL contains no explicit port number. }
XDRIP_DEFAULT_PORT = '17580';

type
  {** xDrip client.
      Inherits from @code(NightScout) and adapts certain behaviors for xDrip-based
      endpoints. Provides methods to:
      - Connect to the xDrip server
      - Retrieve glucose readings
      - Synchronize local time offset
      - Fetch BG threshold values (hi/lo)

      @seealso(NightScout)
   }
xDrip = class(NightScout)
public
    {** Create an xDrip API client.

        Initializes:
        - HTTP User-Agent string
        - @code(baseUrl) = trimmed @code(user) + '/'
        - API secret header value as @code('api-secret=' + SHA1(pass)) if provided
        - @code(timezone) and @code(native) HTTP helper

        Note:
        - This constructor does not call @code(inherited Create); if base-class
          initialization changes (e.g., default thresholds via @code(initCGMCore)),
          consider invoking it or duplicating that logic here.

        @param(user  Base xDrip URL, including scheme, e.g. https://host.example)
        @param(pass  xDrip API secret in plain text; hashed to SHA1 for header usage)
        @param(extra Reserved for future use)
     }
  constructor Create(user, pass: string); override;

    {** Fetch BG readings from xDrip.

        Behavior:
        - If @code(path) is empty, uses @code(XDRIP_READINGS)
        - Calls @code(inherited GetReadings) to reuse NightScout parsing/mapping
        - Returns @code(BGResults) and the raw response in @code(res)

        @param(min     Minutes of history to request (endpoint-dependent))
        @param(maxNum  Maximum number of results to fetch)
        @param(path    Optional custom endpoint path; defaults to @code(XDRIP_READINGS))
        @param(res     Out parameter receiving the raw JSON/text response)
        @returns(Array of @code(BGReading); may be empty on errors)
     }
  function GetReadings(min, maxNum: integer; path: string;
    out res: string): BGResults; override;

    {** Connect to the xDrip server and initialize time offset and thresholds.

        Steps:
        1) Verify URL scheme (@code(http)/@code(https))
        2) Request @code(XDRIP_STATUS) ("pebble") and extract server @code(now) timestamp
        3) Compute @code(timeDiff) relative to local time
        4) Request @code(XDRIP_RANGES) and parse @code(bgHigh)/@code(bgLow) thresholds (fallbacks if absent)

        Notes:
        - Uses fpjson to parse both the pebble status and the ranges response.

        @returns(True on success; otherwise False and @code(errormsg) is set)
     }
  function Connect: boolean; override;

    {** UI parameter label provider (override).
        1: xDrip URL
        2: API Secret (plain text)
        3: (unused)
     }
  class function ParamLabel(LabelName: APIParamLabel): string; override;
protected
  {** Get the value which represents the maximum reading for the backend
  }
  function getLimitHigh: integer; override;
  
  {** Get the value which represents the minimum reading for the backend
  }
  function getLimitLow: integer; override;

  {** Gets the name of this API
  }
  function getSystemName: string; override;

  {** xDrip can upload as frequently as every minute }
  function getReportingInterval: integer; override;
end;

implementation

resourcestring
sParamUsername = 'xDrip URL';
sParamPassword = 'API Secret';
sParamDesc ='xDrip setup:'#13#10''#13#10'Address: your xDrip REST endpoint (base URL).'#13#10'Auth: API secret (plain text; server hashes it).';
sParamDescHTML ='<b>xDrip setup:</b><br><br>Address: your xDrip REST endpoint (base URL).<br>Auth: API secret (plain text; server hashes it).';

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function xDrip.getSystemName: string;
begin
  result := 'xDrip+';
end;

function xDrip.getReportingInterval: integer;
begin
  Result := 1;
end;

{------------------------------------------------------------------------------
  Create an xDrip API client.

  Sets a descriptive User-Agent, normalizes the base URL, computes the API secret
  header, initializes timezone offset, and creates the native HTTP client.

  Note:
  - inherited Create is not called here; if base initialization (e.g. default
    thresholds) is needed, ensure it is handled elsewhere before use.
------------------------------------------------------------------------------}
constructor xDrip.Create(user, pass: string);
var
  schemeEnd: integer;
  hostPart: string;
begin
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';

  baseUrl := TrimRightSet(user, ['/']);

  // If the URL has no explicit port, append xDrip+'s default REST port.
  schemeEnd := Pos('://', baseUrl);
  if schemeEnd > 0 then
    hostPart := Copy(baseUrl, schemeEnd + 3, MaxInt)
  else
    hostPart := baseUrl;
  if Pos(':', hostPart) = 0 then
    baseUrl := baseUrl + ':' + XDRIP_DEFAULT_PORT;

  // xDrip expects the API secret as a SHA1 hex digest, commonly as a query/header
  if pass <> '' then
    key := 'api-secret=' + SHA1Print(SHA1String(pass))
  else
    key := '';

  // xDrip timestamps are treated as UTC epochs; keep timezone correction neutral.
  timezone := 0;

  // Create the native HTTP helper bound to this UA and base URL
  native := TrndiNative.Create(ua, baseUrl);
end;

{------------------------------------------------------------------------------
  Fetch BG readings from xDrip, defaulting the path if omitted, then defer to NightScout.

  The NightScout implementation handles JSON parsing and mapping to BGResults.
------------------------------------------------------------------------------}
function xDrip.GetReadings(min, maxNum: integer; path: string;
out res: string): BGResults;
var
  LJSONData: TJSONData;
  LEntries: TJSONArray;
  LEntry: TJSONObject;
  LDateData: TJSONData;
  LDateMs: int64;
  i, LCount: integer;
begin
  // If path is empty, default to the xDrip sgv.json endpoint
  if path = '' then
    path := XDRIP_READINGS;

  // Reuse NightScout's implementation to perform the request and mapping
  Result := inherited GetReadings(min, maxNum, path, res);

  if (res = '') or (Length(Result) = 0) then
    Exit;

  try
    LJSONData := GetJSON(res);
    try
      if not (LJSONData is TJSONArray) then
        Exit;

      LEntries := TJSONArray(LJSONData);
      LCount := Length(Result);
      if LEntries.Count < LCount then
        LCount := LEntries.Count;

      for i := 0 to LCount - 1 do
      begin
        if not (LEntries.Items[i] is TJSONObject) then
          Continue;

        LEntry := TJSONObject(LEntries.Items[i]);
        LDateData := LEntry.Find('date');
        if not Assigned(LDateData) then
          Continue;

        LDateMs := LDateData.AsInt64;

        // Treat xDrip date as UTC epoch milliseconds.
        Result[i].date := UnixToDateTime(LDateMs div 1000, False);
      end;
    finally
      LJSONData.Free;
    end;
  except
    // Preserve the inherited result if the payload cannot be reparsed.
  end;
end;

{------------------------------------------------------------------------------
  Connect to xDrip, synchronize time, and read thresholds.

  - Probes pebble for "now" (milliseconds since epoch).
  - Calculates timeDiff used elsewhere for timestamp normalization.
  - Attempts to extract bgHigh/bgLow from status.json; uses defaults if absent.
------------------------------------------------------------------------------}
function xDrip.Connect: boolean;
var
  LResponse: string;
  LTimeStamp: int64;
  LServerUTCUnix: int64;
  LLocalUTCUnix: int64;
  js, node: TJSONData;
begin
  if Copy(baseUrl, 1, 4) <> 'http' then
  begin
    Result := false;
    lastErr := 'Invalid address. Must begin with http:// or https://!';
    Exit;
  end;

  LResponse := native.Request(false, XDRIP_STATUS, [], '', key);

  if (LResponse = '') or ((LResponse <> '') and (LResponse[1] = '+')) or
    (Pos('Could not connect', LResponse) > 0) or
    (Pos('Failed to connect', LResponse) > 0) then
  begin
    lastErr := 'Cannot connect to xDrip server at ' + baseUrl + '. Check URL and network.';
    Result := false;
    Exit;
  end;

  if Pos('uthentication failed', LResponse) > 0 then
  begin
    lastErr := 'Wrong API secret. xDrip rejected authentication.';
    Result := false;
    Exit;
  end;

  // Parse "now" (ms epoch) from the pebble JSON response.
  // xDrip+ nests it as: {"status":[{"now":...}], "bgs":[...]}
  LTimeStamp := 0;
  try
    js := GetJSON(LResponse);
    try
      node := js.FindPath('status[0].now');
      if Assigned(node) then
        LTimeStamp := node.AsInt64
      else
      begin
        // Older/alternate shape: "now" at root level
        node := js.FindPath('now');
        if Assigned(node) then
          LTimeStamp := node.AsInt64;
      end;
    finally
      js.Free;
    end;
  except
    // parse failure handled below
  end;

  if LTimeStamp = 0 then
  begin
    lastErr := 'Wrong response format from ' + XDRIP_STATUS +
      ' endpoint: "now" field missing or unparseable.' +
      ' Response: ' + Copy(LResponse, 1, 200);
    Result := false;
    Exit;
  end;

  tz := 0;
  LServerUTCUnix := LTimeStamp div 1000;
  LLocalUTCUnix  := DateTimeToUnix(LocalTimeToUniversal(Now));
  timeDiff       := LServerUTCUnix - LLocalUTCUnix;

  // Parse bgHigh / bgLow from status.json.
  cgmHi := 160;
  cgmLo := 60;
  LResponse := native.Request(false, XDRIP_RANGES, [], '', key);
  if Trim(LResponse) <> '' then
  try
    js := GetJSON(LResponse);
    try
      if js.JSONType = jtObject then
      begin
        node := TJSONObject(js).Find('bgHigh');
        if Assigned(node) then
          cgmHi := node.AsInteger;
        node := TJSONObject(js).Find('bgLow');
        if Assigned(node) then
          cgmLo := node.AsInteger;
      end;
    finally
      js.Free;
    end;
  except
    // keep defaults
  end;

  cgmRangeHi := CGM_RANGE_HI_DISABLED;
  cgmRangeLo := CGM_RANGE_LO_DISABLED;

  Result := true;
end;

{------------------------------------------------------------------------------
  Provide parameter label captions for Settings UI (xDrip backend).
------------------------------------------------------------------------------}
class function xDrip.ParamLabel(LabelName: APIParamLabel): string;
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

function xDrip.getLimitHigh: integer;
begin
  Result := 600; // xDrip typical high limit
end;

function xDrip.getLimitLow: integer;
begin
  Result := 40; // xDrip typical low limit 
end;

end.
