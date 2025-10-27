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
Classes, SysUtils, trndi.types, dateutils, trndi.native, Dialogs;

type
  {** CGMCore holds thresholds used to classify blood glucose values.
      - @code(hi):    High boundary for blood glucose (BG).
      - @code(lo):    Low boundary for BG.
      - @code(top):   Upper bound of personalized in-range interval (optional).
      - @code(bottom):Lower bound of personalized in-range interval (optional).

      Notes:
      - @code(top=500) and @code(bottom=0) act as sentinels meaning “unused”.
   }
CGMCore = record
  hi, lo, top, bottom: integer;
end;

  {** TrndiAPI is the abstract base class for accessing CGM data sources.
      It defines common thresholds, time handling, and helper routines to read
      and classify BG readings. Subclasses implement connectivity and fetching.

  Typical use:
      - Construct subclass with credentials/config.
      - Call @code(connect).
      - Call @code(getReadings), @code(getCurrent), or @code(getLast).

  See also: BGReading, BGResults, BGValLevel.
   }
TrndiAPI = class
protected
    /// Time difference in seconds used to adjust clock skew vs. data source.
  timeDiff: integer;

    /// Timezone offset in seconds (applied in @code(JSToDateTime) when requested).
  tz: integer;

    /// Reference to platform-specific/native functions (HTTP, etc.).
  native: TrndiNative;

    /// User-Agent string used for outbound HTTP requests (if applicable).
  ua: string;

    /// Base URL for the API endpoint (subclasses typically set this).
  baseUrl: string;

    /// Last error message reported by operations in this instance.
  lastErr: string;

    /// Collection of BG threshold values.
  core: CGMCore;

    {** Set the timezone offset.
        Note:
          Despite the parameter name, this implementation expects minutes
          and multiplies by 60 to store the value in seconds.

        @param(secs Timezone offset in minutes; will be multiplied by 60.)
     }
  procedure setTZ(secs: integer);

    {** URL-encode a string using percent-encoding for non-unreserved characters.
        Unreserved characters are [A-Z a-z 0-9 - _ ~ .]

        @param(src Input string)
        @returns(Percent-encoded string)
     }
  function encodeStr(src: string): string; virtual; final;

    {** Get a threshold by level.
        Returns the configured limit for a @code(BGValLevel) such as @code(BGHIGH),
        @code(BGLOW), @code(BGRangeHI), or @code(BGRangeLO).

        @param(v Level selector)
        @returns(Threshold value as @code(single))
     }
  function getLevel(v: BGValLevel): single; virtual;

    {** Retrieve the current @code(CGMCore) thresholds in one record. }
  function getCGMCore: CGMCore;

    {** Initialize @code(CGMCore) thresholds to defaults.

        Defaults:
        - hi=401, lo=40, top=500 (disabled), bottom=0 (disabled)
     }
  procedure initCGMCore;

    {** Determine whether the API appears to be active by fetching a current reading.
        Returns @code(True) if a reading is available and has a timestamp > 1000.

        @returns(@code(True) if a plausible current reading exists)
     }
  function checkActive: boolean;
public
const
  toMMOL = 0.05555555555555556; // Factor to multiply mg/dL by to get mmol/L
const
  toMGDL = 18.0156; // Facvtor to multiply mmol/L to get mg/dL
    {** Provide a backend-specific caption for parameter labels in Settings.
        Index mapping (by convention):
        - 1: Label above the first edit (e.g., server URL or username)
        - 2: Label above the second edit (e.g., API key or password)
        - 3: Label above the optional third edit ("Extra"), if used

        Subclasses should override to supply meaningful texts.

        @param(Index 1..3 selecting which label to return)
        @returns(Caption text; empty string if not applicable)
     }
  class function ParamLabel(Index: integer): string; virtual;
    {** Retrieve BG readings with optional extras path/params.
        This overload captures the raw response in a string.

        Subclasses must implement the abstract version. This overload typically
        forwards to it.

        @param(minNum  Time span hint (minutes), usage depends on subclass)
        @param(maxNum  Maximum number of readings to retrieve)
        @param(extras  Additional path or query hints; default empty)
        @returns(Array of BGReading)
     }
  function getReadings(minNum, maxNum: integer; extras: string = ''): BGResults;

    {** Retrieve BG readings with raw JSON/text result returned by out-parameter.

        Subclasses must implement this method.

        @param(minNum  Time span hint (minutes), usage depends on subclass)
        @param(maxNum  Maximum number of readings to retrieve)
        @param(extras  Additional path or query hints)
        @param(res     Out parameter receiving raw response text)
        @returns(Array of BGReading)
     }
  function getReadings(minNum, maxNum: integer; extras: string;
    out res: string): BGResults; virtual; abstract;

    {** Construct a new API object.
        Subclasses may interpret @code(user), @code(pass), and @code(extra) as needed.

        Base class behavior:
        - Sets @code(timezone) from @code(GetLocalTimeOffset).
        - Creates @code(native).
        - Initializes default CGM thresholds.

        @param(user   Implementation-defined (e.g., base URL))
        @param(pass   Implementation-defined (e.g., password or token))
        @param(extra  Implementation-defined extra parameter)
     }
  constructor Create(user, pass, extra: string); virtual;

    {** Destructor; releases owned resources. }
  destructor Destroy; virtual;

    {** Establish connectivity to the underlying data source.
        Subclasses must implement.

        @returns(True if connection and initial probing succeeded)
     }
  function connect: boolean; virtual; abstract;

    {** Classify a numeric BG value into a @code(BGValLevel) (e.g. high/low/range).

        Rules:
        - @code(v >= hi) => @code(BGHIGH)
        - @code(v <= lo) => @code(BGLOW)
        - Else => in-range; optional sub-classification:
          - If @code(top <> 500) and @code(v >= top) => @code(BGRangeHI)
          - Else if @code(bottom <> 0) and @code(v <= bottom) => @code(BGRangeLO)
          - Else => @code(BGRange)

        @param(v BG value)
        @returns(BG level classification)
     }
  function getLevel(v: single): BGValLevel; virtual;

    {** Get the last reading in a larger time window (e.g., 24 hours).
        Convenience that returns a single last sample.

        @param(res Out parameter receiving the reading)
        @returns(True if a reading was returned)
     }
  function getLast(var res: BGReading): boolean;

    {** Get the most current reading within a short time window (e.g., ~10 minutes).

        @param(res Out parameter receiving the reading)
        @returns(True if a reading was returned)
     }
  function getCurrent(var res: BGReading): boolean;

    {** Get current base time as Unix epoch (seconds), adjusted by @code(timeDiff).
        @returns(Unix timestamp in seconds)
     }
  function getBasetime: int64;

    {** Convert a JavaScript timestamp (milliseconds) to @code(TDateTime).
        When @code(correct) is true, apply the timezone offset @code(tz).

        @param(ts      Timestamp in milliseconds since Unix epoch)
        @param(correct Whether to subtract @code(tz) before conversion)
        @returns(@code(TDateTime) in local or adjusted time)
     }
  function JSToDateTime(ts: int64; correct: boolean = true): TDateTime; virtual;

    {** Predict future blood glucose readings using linear regression.
        Uses recent historical readings to calculate a trend line and projects
        future values. The prediction quality depends on the stability of the trend.

        Algorithm:
        - Fetches recent readings (last 30-60 minutes)
        - Applies linear regression on time vs. BG value
        - Extrapolates forward to predict future values
        - Each prediction is spaced by the average interval between historical readings

        @param(numPredictions Number of future readings to predict (1-10 recommended))
        @param(predictions    Out parameter receiving array of predicted readings)
        @returns(True if prediction succeeded; False if insufficient data)

        Notes:
        - Requires at least 3 historical readings for reliable prediction
        - Predictions become less accurate further into the future
        - Does not account for meals, insulin, or other external factors
        - Predicted readings have their @code(trend) field set to BGUnknown
     }
  function predictReadings(numPredictions: integer; out predictions: BGResults): boolean;

    // -------- Properties --------

    {** Indexed read-only access to thresholds by @code(BGValLevel). }
  property threshold[lvl: BGValLevel]: single read getLevel;

    {** Read-only access to the full threshold record. }
  property cgm: CGMCore read getCGMCore;

    /// High threshold (modifiable).
  property cgmHi: integer read core.hi write core.hi;

    /// Low threshold (modifiable).
  property cgmLo: integer read core.lo write core.lo;

    /// Personalized in-range upper bound (optional; 500 disables).
  property cgmRangeHi: integer read core.top write core.top;

    /// Personalized in-range lower bound (optional; 0 disables).
  property cgmRangeLo: integer read core.bottom write core.bottom;

published
    /// Read-only time difference offset (seconds).
  property offset: integer read timeDiff;

    {** Write-only timezone offset setter.
        Note: Pass minutes (multiplied by 60 internally). }
  property timezone: integer write setTZ;

    /// Last error message produced by operations on this instance.
  property errormsg: string read lastErr;

    {** Convenience status flag determined via @code(checkActive). }
  property active: boolean read checkActive;
end;

implementation

{------------------------------------------------------------------------------
  Determine whether the API appears to be active by fetching a current reading.
  Returns True if a reading is available and its timestamp is > 1000.
------------------------------------------------------------------------------}
function TrndiAPI.checkActive: boolean;
var
  bgr: BGReading;
begin
  // Try to obtain a “current” reading using a short lookback window
  if not getCurrent(bgr) then
    Result := false
  else
    // Lightweight plausibility check on timestamp
    Result := bgr.date > 1000;
end;

{------------------------------------------------------------------------------
  Base constructor.
  Initializes timezone from the OS, sets up native interface, and default CGM thresholds.
------------------------------------------------------------------------------}
constructor TrndiAPI.Create(user, pass, extra: string);
begin
  // Store local timezone offset (minutes) via property; internally becomes seconds.
  timezone := GetLocalTimeOffset;

  // Native helper for HTTP and other platform-specific operations.
  native := TrndiNative.Create(ua, baseUrl);

  // Defaults for thresholds (can be overwritten later).
  initCGMCore;
end;

{------------------------------------------------------------------------------
  Base destructor; frees native helper.
------------------------------------------------------------------------------}
destructor TrndiAPI.Destroy;
begin
  native.Free;
end;

{------------------------------------------------------------------------------
  Resolve a threshold value by BGValLevel.
------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Classify a numeric BG value into a BGValLevel.
------------------------------------------------------------------------------}
function TrndiAPI.getLevel(v: single): BGValLevel;
begin
  if v >= core.hi then
    Result := BGHIGH
  else
  if v <= core.lo then
    Result := BGLOW
  else
  begin
    // Default in-range classification
    Result := BGRange;

    // Optional upper/lower in-range refinement when sentinels are not used
    if (core.top <> 500) and (v >= core.top) then
      Result := BGRangeHI
    else
    if (core.bottom <> 0) and (v <= core.bottom) then
      Result := BGRangeLO;
  end;
end;

{------------------------------------------------------------------------------
  Initialize CGM thresholds to sensible defaults.
------------------------------------------------------------------------------}
procedure TrndiAPI.initCGMCore;
begin
  core.hi := 401;
  core.lo := 40;
  core.top := 500;  // 500 => “unused” for personalized upper bound
  core.bottom := 0;    // 0   => “unused” for personalized lower bound
end;

{------------------------------------------------------------------------------
  Return the current CGMCore record.
------------------------------------------------------------------------------}
function TrndiAPI.getCGMCore: CGMCore;
begin
  Result := core;
end;

{------------------------------------------------------------------------------
  URL-encode non-unreserved characters.
  Unreserved allowed set: [A-Z a-z 0-9 - _ ~ .]
------------------------------------------------------------------------------}
function TrndiAPI.encodeStr(src: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(src) do
    if not (src[i] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '~', '.']) then
      Result := Result + '%' + IntToHex(Ord(src[i]), 2)
    else
      Result := Result + src[i]; // passthrough allowed characters
end;

{------------------------------------------------------------------------------
  Set timezone offset.
  Note: secs is treated as minutes and multiplied by 60 to store seconds.
------------------------------------------------------------------------------}
procedure TrndiAPI.setTZ(secs: integer);
begin
  tz := secs * 60;
end;

{------------------------------------------------------------------------------
  Get current base time as Unix epoch (seconds), adjusted by timeDiff.
------------------------------------------------------------------------------}
function TrndiAPI.getBasetime: int64;
begin
  Result := DateTimeToUnix(IncSecond(Now, timeDiff));
end;

{------------------------------------------------------------------------------
  Get the last reading across a larger window (e.g., 24 hours).
------------------------------------------------------------------------------}
function TrndiAPI.getLast(var res: BGReading): boolean;
var
  r: BGResults;
begin
  Result := false;

  // Request readings from the past 1440 minutes (24 hours), limit 1
  r := getReadings(1440, 1);

  if Length(r) > 0 then
  begin
    res := r[0];
    Result := true;
  end;
end;

{------------------------------------------------------------------------------
  Get the most current reading across a short window (e.g., last 10 minutes).
------------------------------------------------------------------------------}
function TrndiAPI.getCurrent(var res: BGReading): boolean;
var
  r: BGResults;
begin
  Result := false;

  // Request readings from the past 10 minutes, limit 1
  r := getReadings(10, 1);

  if Length(r) > 0 then
  begin
    res := r[0];
    Result := true;
  end;
end;

{------------------------------------------------------------------------------
  Convert a JavaScript millisecond epoch to TDateTime.
  When correct is true, subtracts tz before conversion.
------------------------------------------------------------------------------}
function TrndiAPI.JSToDateTime(ts: int64; correct: boolean): TDateTime;
begin
  if correct then
    Result := UnixToDateTime((ts div 1000) - tz)
  else
    Result := UnixToDateTime(ts div 1000);
end;

{------------------------------------------------------------------------------
  Convenience overload that forwards to the abstract variant and discards raw text.
------------------------------------------------------------------------------}
function TrndiAPI.getReadings(minNum, maxNum: integer; extras: string = ''): BGResults;
var
  res: string;
begin
  Result := getReadings(minNum, maxNum, extras, res);
end;

{------------------------------------------------------------------------------
  Default parameter label provider for Settings UI.
  Subclasses override to provide backend-specific captions.
------------------------------------------------------------------------------}
class function TrndiAPI.ParamLabel(Index: integer): string;
begin
  case Index of
  1:
    Result := 'Server Address';
  2:
    Result := 'API Key';
  3:
    Result := 'Extra (optional)';
  else
    Result := '';
  end;
end;

{------------------------------------------------------------------------------
  Predict future blood glucose readings using linear regression.
  Returns True if prediction succeeded; False if insufficient data.
------------------------------------------------------------------------------}
function TrndiAPI.predictReadings(numPredictions: integer; 
  out predictions: BGResults): boolean;
var
  historicalReadings: BGResults;
  n, i: integer;
  sumX, sumY, sumXY, sumX2: double;
  meanX, meanY, slope, intercept: double;
  timeValues: array of double;
  bgValues: array of double;
  avgInterval: double;
  lastTime: TDateTime;
  predictedTime: TDateTime;
  predictedValue: double;
  minReadings: integer;
begin
  Result := False;
  SetLength(predictions, 0);
  
  // Validate input
  if numPredictions < 1 then
    Exit;
  
  // Clamp to reasonable range
  if numPredictions > 20 then
    numPredictions := 20;
  
  // Minimum readings needed for prediction
  minReadings := 3;
  
  // Fetch recent readings (last 60 minutes, up to 12 readings = ~5 min intervals)
  historicalReadings := getReadings(60, 12);
  n := Length(historicalReadings);
  
  if n < minReadings then
  begin
    lastErr := 'Insufficient data for prediction (need at least ' + 
               IntToStr(minReadings) + ' readings)';
    Exit;
  end;
  
  // Prepare arrays for linear regression
  SetLength(timeValues, n);
  SetLength(bgValues, n);
  
  // Convert readings to time/value pairs
  // Use minutes since first reading as X axis for numerical stability
  for i := 0 to n - 1 do
  begin
    timeValues[i] := (historicalReadings[i].date - historicalReadings[0].date) * 24 * 60;
    bgValues[i] := historicalReadings[i].convert(BGUnit.mgdl);
  end;
  
  // Calculate linear regression: y = slope * x + intercept
  sumX := 0;
  sumY := 0;
  sumXY := 0;
  sumX2 := 0;
  
  for i := 0 to n - 1 do
  begin
    sumX := sumX + timeValues[i];
    sumY := sumY + bgValues[i];
    sumXY := sumXY + (timeValues[i] * bgValues[i]);
    sumX2 := sumX2 + (timeValues[i] * timeValues[i]);
  end;
  
  meanX := sumX / n;
  meanY := sumY / n;
  
  // Calculate slope and intercept
  if (sumX2 - (sumX * sumX / n)) <> 0 then
    slope := (sumXY - (sumX * sumY / n)) / (sumX2 - (sumX * sumX / n))
  else
  begin
    lastErr := 'Cannot calculate trend (invalid time distribution)';
    Exit;
  end;
  
  intercept := meanY - (slope * meanX);
  
  // Calculate average interval between readings
  if n > 1 then
    avgInterval := (historicalReadings[n-1].date - historicalReadings[0].date) / (n - 1)
  else
    avgInterval := 5.0 / (24 * 60); // Default to 5 minutes
  
  // Generate predictions
  SetLength(predictions, numPredictions);
  lastTime := historicalReadings[n-1].date;
  
  for i := 0 to numPredictions - 1 do
  begin
    // Time of prediction (in minutes from first reading)
    predictedTime := lastTime + (avgInterval * (i + 1));
    
    // Calculate predicted BG value using linear regression
    predictedValue := slope * ((predictedTime - historicalReadings[0].date) * 24 * 60) + intercept;
    
    // Clamp to reasonable range
    if predictedValue < 20 then
      predictedValue := 20;
    if predictedValue > 600 then
      predictedValue := 600;
    
    // Create predicted reading
    predictions[i].Init(BGUnit.mgdl, BGUnit.mgdl);
    predictions[i].update(predictedValue, BGPrimary, BGUnit.mgdl);
    predictions[i].date := predictedTime;
    predictions[i].trend := TdNotComputable;
    
    // Classify the predicted level using current thresholds
    predictions[i].level := getLevel(predictedValue);
  end;
  
  Result := True;
end;

end.
