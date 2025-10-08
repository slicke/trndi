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
unit trndi.api.xdrip;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, sha1,
  // Parent classes and modules
  trndi.api.nightscout, trndi.native, trndi.types,
  // FPC/Lazarus units
  DateUtils, Dialogs;

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
    constructor Create(user, pass, extra: string); override;

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
        - This implementation parses values from raw JSON strings using simple substring
          operations; consider switching to a JSON parser for robustness.

        @returns(True on success; otherwise False and @code(errormsg) is set)
     }
    function Connect: boolean; override;

    {** UI parameter label provider (override).
        1: xDrip URL
        2: API Secret (plain text)
        3: (unused)
     }
    class function ParamLabel(Index: integer): string; override;
  end;

implementation

{------------------------------------------------------------------------------
  Create an xDrip API client.

  Sets a descriptive User-Agent, normalizes the base URL, computes the API secret
  header, initializes timezone offset, and creates the native HTTP client.

  Note:
  - inherited Create is not called here; if base initialization (e.g. default
    thresholds) is needed, ensure it is handled elsewhere before use.
------------------------------------------------------------------------------}
constructor xDrip.Create(user, pass, extra: string);
begin
  // Use a standard user agent for server logs/diagnostics
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';

  // Ensure trailing slash so relative paths append correctly
  baseUrl := TrimRightSet(user, ['/']) + '/';

  // xDrip expects the API secret as a SHA1 hex digest, commonly as a query/header
  if pass <> '' then
    key := 'api-secret=' + SHA1Print(SHA1String(pass))
  else
    key := '';

  // Initialize timezone offset (minutes -> seconds via base class setter is not used here)
  // We store the OS local offset now for clock skew correction later.
  timezone := GetLocalTimeOffset;

  // Create the native HTTP helper bound to this UA and base URL
  native := TrndiNative.Create(ua, baseUrl);
end;

{------------------------------------------------------------------------------
  Fetch BG readings from xDrip, defaulting the path if omitted, then defer to NightScout.

  The NightScout implementation handles JSON parsing and mapping to BGResults.
------------------------------------------------------------------------------}
function xDrip.GetReadings(min, maxNum: integer; path: string;
  out res: string): BGResults;
begin
  // If path is empty, default to the xDrip sgv.json endpoint
  if path = '' then
    path := XDRIP_READINGS;

  // Reuse NightScout's implementation to perform the request and mapping
  Result := inherited GetReadings(min, maxNum, path, res);
end;

{------------------------------------------------------------------------------
  Connect to xDrip, synchronize time, and read thresholds.

  - Probes pebble for "now" (milliseconds since epoch).
  - Calculates timeDiff used elsewhere for timestamp normalization.
  - Attempts to extract bgHigh/bgLow from status.json; uses defaults if absent.

  Implementation note:
  - String slicing is used instead of JSON parsing for speed/simplicity; robust code
    should parse JSON explicitly to avoid format brittleness.
------------------------------------------------------------------------------}
function xDrip.Connect: boolean;
var
  LResponse: string;
  LTimeStamp: int64;
  LDateTime: TDateTime;
  i: integer;
begin
  // Basic check for protocol correctness to catch obvious misconfiguration early
  if Copy(baseUrl, 1, 4) <> 'http' then
  begin
    Result := False;
    lastErr := 'Invalid address. Must begin with http:// or https://!';
    Exit;
  end;

  // Fetch xDrip "pebble" info. If the secret fails, xDrip often replies with "Authentication failed"
  // Match a substring ("uthentication failed") to avoid case sensitivity issues.
  LResponse := native.Request(False, XDRIP_STATUS, [], '', key);
  if Pos('uthentication failed', LResponse) > 0 then
  begin
    lastErr := 'Access token rejected by xDrip. Is it correct?';
    Result := False;
    Exit;
  end;

  // Extract the server time "now" from the pebble JSON by slicing:
  // e.g., ..."now":1618353053000,..."  -> take 13 chars after the value starts
  // NOTE: This assumes a 13-digit millisecond timestamp; prefer JSON parsing for safety.
  LResponse := Copy(LResponse, Pos('"now":', LResponse) + 6, 13);

  // Convert the extracted substring to int64 milliseconds
  if not TryStrToInt64(LResponse, LTimeStamp) then
  begin
    lastErr := 'xDrip could not initialize. Cannot sync clocks; xDrip may be offline.';
    Result := False;
    Exit;
  end;

  // Convert milliseconds to seconds, then to TDateTime (UTC)
  LDateTime := UnixToDateTime(LTimeStamp div 1000);

  // Compute timeDiff between server UTC and local UTC
  timeDiff := SecondsBetween(LDateTime, LocalTimeToUniversal(Now));
  if timeDiff < 0 then
    timeDiff := 0;
  timeDiff := -1 * timeDiff; // Negative to match the expected adjustment direction

  // Retrieve hi/lo ranges from xDrip (status.json); parse simple integers by slicing
  LResponse := native.Request(False, XDRIP_RANGES, [], '', key);

  // Parse bgHigh, e.g. ..."bgHigh":160,...
  if TryStrToInt(TrimSet(Copy(LResponse, Pos('bgHigh', LResponse) + 8, 4),
    [' ', ',', '}']), i) then
    cgmHi := i
  else
    cgmHi := 160; // sensible fallback if not present

  // Parse bgLow, e.g. ..."bgLow":60,...
  if TryStrToInt(TrimSet(Copy(LResponse, Pos('bgLow', LResponse) + 7, 4),
    [' ', ',', '}']), i) then
    cgmLo := i
  else
    cgmLo := 60; // sensible fallback if not present

  // Personalized in-range bounds are not provided here; keep sentinels
  cgmRangeHi := 500;
  cgmRangeLo := 0;

  Result := True;
end;

{------------------------------------------------------------------------------
  Provide parameter label captions for Settings UI (xDrip backend).
------------------------------------------------------------------------------}
class function xDrip.ParamLabel(Index: integer): string;
begin
  case Index of
    1: Result := 'xDrip URL';
    2: Result := 'API Secret';
    else
      Result := inherited ParamLabel(Index);
  end;
end;

end.
