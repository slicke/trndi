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
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit trndi.api;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, trndi.types, dateutils, trndi.native, dialogs;

type
  {
    CGMCore holds four threshold values:
      - hi      : The high boundary for blood glucose.
      - lo      : The low boundary for blood glucose.
      - top     : The upper boundary for a personalized "in-range" interval.
      - bottom  : The lower boundary for a personalized "in-range" interval.
  }
CGMCore = record
  hi, lo, top, bottom: integer;
end;

  {
    TrndiAPI is the main class to interact with CGM (Continuous Glucose
    Monitoring) data. It provides methods to retrieve readings and determine
    the current status of the glucose levels relative to configured thresholds.
  }
TrndiAPI = class
protected
    // Time difference (offset) used for computations
  timeDiff: integer;
    // Timezone offset in seconds
  tz: integer;
    // Reference to native (platform-specific) functions
  native: TrndiNative;
    // User agent string (or similar) for HTTP requests
  ua: string;
    // Base URL for the API
  baseUrl: string;
    // Last encountered error message
  lastErr: string;
    // Record containing all CGM thresholds
  core: CGMCore;

    {
      Sets the time zone offset (in seconds).
      Multiply by 60 internally to convert from minutes if needed.
    }
  procedure setTZ(secs: integer);

    {
      Encodes a string to be URL-safe (percent-encoding).
      This method is marked final (cannot be overridden further).
    }
  function encodeStr(src: string): string; virtual; final;

    {
      Retrieves a threshold value by BGValLevel (e.g., BGHIGH, BGLOW, etc.).
      Override this method if you need custom logic for retrieving a threshold.
    }
  function getLevel(v: BGValLevel): single; virtual;

    {
      Returns the current CGMCore thresholds in one structure.
    }
  function getCGMCore: CGMCore;

    {
      Initializes the CGMCore thresholds to default values.
    }
  procedure initCGMCore;

    {
      Checks if the API is currently "active" by verifying if a BG reading
      can be retrieved.
    }
  function checkActive: boolean;

public
    {
      Retrieves BG readings. Should be implemented in subclasses to actually
      perform the data fetch, e.g., from a remote API.
        min    : The timespan in minutes (or similar).
        maxNum : Maximum number of readings to retrieve.
        extras : Any additional parameters or filters.
    }
  function getReadings(min, maxNum: integer; extras: string = ''): BGResults; virtual; abstract;

    {
      Constructor to create a new instance of TrndiAPI.
      Parameters might be user credentials or other relevant config for a data source.
    }
  constructor create(user, pass, extra: string); virtual;

  destructor destroy; virtual;
    {
      Connects to the underlying data source. Must be implemented in subclasses.
    }
  function connect: boolean; virtual; abstract;

    {
      Returns the BGValLevel (e.g., BGHIGH, BGLOW, etc.) based on a given numeric value.
    }
  function getLevel(v: single): BGValLevel; virtual;

    {
      Retrieves the most recent (last) reading in a larger time window (e.g., 24 hours).
      Returns true if successfully retrieved, false otherwise.
    }
  function getLast(var res: BGReading): boolean;

    {
      Retrieves the most current reading in a short timespan (e.g., last 10 minutes).
      Returns true if successfully retrieved, false otherwise.
    }
  function getCurrent(var res: BGReading): boolean;

    {
      Returns the current timestamp in Unix epoch format (adjusted with timeDiff).
    }
  function getBasetime: int64;

    {
      Converts a JavaScript timestamp (milliseconds) to a TDateTime.
      If correct = true, applies the time zone offset (tz).
    }
  function JSToDateTime(ts: int64; correct: boolean = true): TDateTime; virtual;

    // -------- Properties --------

    // Get a threshold by level using getLevel (read)
  property threshold[lvl: BGValLevel]: single read getLevel;

    // Directly retrieve the CGMCore record
  property cgm: CGMCore read getCGMCore;

    // Access to the individual CGM threshold values
  property cgmHi: integer read core.hi write core.hi;
  property cgmLo: integer read core.lo write core.lo;
  property cgmRangeHi: integer read core.top write core.top;
  property cgmRangeLo: integer read core.bottom write core.bottom;

published
    // Time difference offset (readonly)
  property offset: integer read timeDiff;

    // Write-only property to set the timezone offset
  property timezone: integer write setTZ;

    // The last error message generated by any operation
  property errormsg: string read lastErr;

    // Indicates whether the API is active (e.g., if data retrieval is possible)
  property active: boolean read checkActive;
end;

implementation

{ Checks if the API is active by attempting to fetch a current reading. }
function TrndiAPI.checkActive: boolean;
var
  bgr: BGReading;
begin
  // If we cannot get a current reading, set result to false
  if not getCurrent(bgr) then
    Result := false
  else
    // Arbitrary check: consider "active" if timestamp is larger than 1000
    Result := bgr.date > 1000;
end;

{ Constructor. Initializes timezone, native functionality, and CGM thresholds. }
constructor TrndiAPI.create(user, pass, extra: string);
begin
  timezone := GetLocalTimeOffset;
  native := TrndiNative.create(ua, baseUrl);
  initCGMCore;
end;

destructor TrndiAPI.destroy;
begin
  native.Free;
end;

{ Returns the threshold as a single-precision float based on BGValLevel. }
function TrndiAPI.getLevel(v: BGValLevel): single;
begin
  case v of
  BGHIGH:
    Result := core.hi;
  BGLOW:
    Result := core.lo;
  BGRangeHI:
    Result := core.top;
  BGRangeLO:
    Result := core.bottom;
  end;
end;

{ Determines the BGValLevel based on a numeric BG value. }
function TrndiAPI.getLevel(v: single): BGValLevel;
begin
  if v >= core.hi then
    Result := BGHIGH
  else
  if v <= core.lo then
    Result := BGLOW
  else
  begin
    // Default range classification
    Result := BGRange;
    // If "top" is set to something below 500 and value is >= top
    if (core.top <> 500) and (v >= core.top) then
      Result := BGRangeHI
    // If "bottom" is set above 0 and value is <= bottom
    else
    if (core.bottom <> 0) and (v <= core.bottom) then
      Result := BGRangeLO;
  end;
end;

{ Sets default CGMCore values. }
procedure TrndiAPI.initCGMCore;
begin
  core.hi := 401;
  core.lo := 40;
  core.top := 500;
  core.bottom := 0;
end;

{ Retrieves the current CGMCore record. }
function TrndiAPI.getCGMCore: CGMCore;
begin
  Result := core;
end;

{ URL-encodes a string to make it safe for transmission in a query/URI. }
function TrndiAPI.encodeStr(src: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(src) do
    if not (src[i] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '~', '.']) then
      Result := Result + '%' + IntToHex(Ord(src[i]), 2)
    else
      Result := Result + src[i]// Check if character is within allowed set
  ;
end;

{ Sets the timezone offset in seconds. Multiplies by 60 internally if needed. }
procedure TrndiAPI.setTZ(secs: integer);
begin
  tz := secs * 60;
end;

{ Returns the current time as a Unix timestamp, adjusted by timeDiff. }
function TrndiAPI.getBasetime: int64;
begin
  Result := DateTimeToUnix(IncSecond(Now, timeDiff));
end;

{ Attempts to retrieve the last reading within a larger time window. }
function TrndiAPI.getLast(var res: BGReading): boolean;
var
  r: BGResults;
begin
  Result := false;
  // Example: requesting readings from the past 1440 minutes (24 hours), limit 1
  r := getReadings(1440, 1);
  if Length(r) > 0 then
  begin
    res := r[0];
    Result := true;
  end;
end;

{ Attempts to retrieve the most recent reading within a short time window. }
function TrndiAPI.getCurrent(var res: BGReading): boolean;
var
  r: BGResults;
begin
  Result := false;
  // Example: requesting readings from the past 10 minutes, limit 1
  r := getReadings(10, 1);

  if Length(r) > 0 then
  begin
    res := r[0];
    Result := true;
  end;
end;

{ Converts a JavaScript timestamp in milliseconds to a TDateTime.
  If correct = true, subtracts the configured time zone offset (tz). }
function TrndiAPI.JSToDateTime(ts: int64; correct: boolean): TDateTime;
begin
  if correct then
    Result := UnixToDateTime((ts div 1000) - tz)
  else
    Result := UnixToDateTime(ts div 1000);
end;

end.
