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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
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
XDRIP_READINGS = 'sgv.json';
XDRIP_STATUS   = 'pebble';
XDRIP_RANGES   = 'status.json';
  // XDRIP_RANGES can include the current BG range settings

type
  { xDrip
    -----
    Inherits from NightScout, overriding certain behaviors for xDrip-based
    endpoints. Provides methods to:
      - Connect to the xDrip server
      - Retrieve glucose readings
      - Sync local time offsets
      - Fetch BG threshold (hi/lo)
  }
xDrip = class(NightScout)
public
  constructor Create(user, pass, extra: string); override;
  function GetReadings(min, maxNum: integer; path: string = ''): BGResults; override;
  function Connect: boolean; override;
end;

implementation

{------------------------------------------------------------------------------
  xDrip.Create
  ------------
  Initializes the user agent, baseUrl, and secret key for xDrip integration.
  - user: typically the server URL (xDrip web endpoint)
  - pass: xDrip API secret (optional)
  - extra: not currently used here
 ------------------------------------------------------------------------------}
constructor xDrip.Create(user, pass, extra: string);
begin
  // Use a standard user agent
  ua      := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';

  // The 'user' param is actually the xDrip base URL
  baseUrl := TrimRightSet(user, ['/']) + '/';

  // xDrip expects the API secret in a "sha1" format, often appended as ?api-secret=<hash>
  // If pass is empty, no secret is used
  key := IfThen(pass <> '',
    'api-secret=' + SHA1Print(SHA1String(pass)),
    '');

  // Initialize timezone offset and create the native HTTP client
  timezone := GetLocalTimeOffset;
  native   := TrndiNative.Create(ua, baseUrl);
end;

{------------------------------------------------------------------------------
  xDrip.GetReadings
  -----------------
  Overridden from NightScout to provide a default endpoint if 'path' is omitted.
  - min: number of minutes of history to fetch
  - maxNum: maximum number of BG results
  - path: optional custom path; defaults to XDRIP_READINGS
 ------------------------------------------------------------------------------}
function xDrip.GetReadings(min, maxNum: integer; path: string = ''): BGResults;
begin
  // If path is empty, default to the xDrip sgv.json endpoint
  if path = '' then
    path := XDRIP_READINGS;

  // Call the inherited NightScout getReadings method
  Result := inherited GetReadings(min, maxNum, path);
end;

{------------------------------------------------------------------------------
  xDrip.Connect
  -------------
  1. Verifies the xDrip server is reachable by calling the XDRIP_STATUS (pebble) endpoint.
  2. Parses the server's "now" timestamp to sync local time offset (timeDiff).
  3. Optionally retrieves BG hi/lo thresholds from the XDRIP_RANGES (status.json).
 ------------------------------------------------------------------------------}
function xDrip.Connect: boolean;
var
  LResponse: string;
  LTimeStamp: int64;
  LDateTime: TDateTime;
  i: integer;
begin
  // Basic check for protocol correctness
  if Copy(baseUrl, 1, 4) <> 'http' then
  begin
    Result := false;
    lastErr := 'Invalid address. Must begin with http:// or https://!';
    Exit;
  end;

  // Fetch xDrip "pebble" info. If the secret fails, we get "uthentication failed"
  LResponse := native.Request(false, XDRIP_STATUS, [], '', key);

  if Pos('uthentication failed', LResponse) > 0 then
  begin
    lastErr := 'Access token rejected by xDrip. Is it correct?';
    Result := false;
    Exit;
  end;

  // Extract the server time "now" from the pebble JSON
  // e.g., "now":1618353053000,
  LResponse := Copy(LResponse, Pos('"now":', LResponse) + 6, 13);

  // Convert the extracted substring to int64
  if not TryStrToInt64(LResponse, LTimeStamp) then
  begin
    lastErr := 'xDrip could not initialize. Cannot sync clocks; xDrip may be offline.';
    Result := false;
    Exit;
  end;

  // Convert the Unix timestamp to TDateTime
  LDateTime := UnixToDateTime(LTimeStamp div 1000);
  timeDiff := SecondsBetween(LDateTime, LocalTimeToUniversal(Now));
  if timeDiff < 0 then
    timeDiff := 0;
  timeDiff := -1 * timeDiff;

  // Retrieve hi/lo ranges from xDrip
  LResponse := native.Request(false, XDRIP_RANGES, [], '', key);

  // If the JSON contains "bgHigh":160, parse out 160
  if TryStrToInt(TrimSet(Copy(LResponse, Pos('bgHigh', LResponse) + 8, 4), [' ', ',', '}']), i) then
    cgmHi := i
  else
    cgmHi := 160;

  // If the JSON contains "bgLow":60, parse out 60
  if TryStrToInt(TrimSet(Copy(LResponse, Pos('bgLow', LResponse) + 7, 4), [' ', ',', '}']), i) then
    cgmLo := i
  else
    cgmLo := 60;

  // Typically, xDrip "range" might be up to 500 mg/dL
  cgmRangeHi := 500;
  cgmRangeLo := 0;

  Result := true;
end;

end.
