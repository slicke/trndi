(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2026 Björn Lindh.
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
Classes, SysUtils, trndi.types, dateutils, trndi.native, trndi.funcs;

type
  {** CGMCore holds thresholds used to classify blood glucose values.
      - @code(hi):    High boundary for blood glucose (BG).
      - @code(lo):    Low boundary for BG.
      - @code(top):   Upper bound of personalized in-range interval (optional).
      - @code(bottom):Lower bound of personalized in-range interval (optional).

      Notes:
      - @code(top=CGM_RANGE_HI_DISABLED) and @code(bottom=CGM_RANGE_LO_DISABLED) act as sentinels meaning “unused”.
   }
CGMCore = record
  hi, lo, top, bottom: integer;
end;

  {** Basal profile entry: start minute (minutes since midnight) and rate U/hr }
TBasalEntry = record
  startMin: integer; { minutes since midnight }
  value: single;     { U/hr }
  name: string;      { optional label }
end;

TBasalProfile = array of TBasalEntry;

  {** Unit for storing glucose values }
glucose = single;

APIParamLabel = (APLUser, APLPass, APLDesc, APLDescHTML, APLCopyright);

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

    /// Wall-clock time when the most recent non-empty BGResults arrived via
    /// @code(noteReadings). Used by @code(checkActive) so the @code(active)
    /// property is a cheap cached check rather than a fresh HTTP round-trip.
    /// Zero (0.0) means no readings have been observed yet.
  lastReadingAt: TDateTime;

    /// Set to True by predictReadings when the recent trend is flat enough
    /// that showing individual predictions would be misleading noise.
  predictionStable: boolean;

    /// 0..1 score set by predictReadings: how cleanly the recent readings fit
    /// the regression after outlier rejection. Exposed as predictionConfidence.
  predictionQuality: double;

    /// Collection of BG threshold values.
  core: CGMCore;

    {** Set the timezone offset.
        Note:
          Despite the parameter name, this implementation expects minutes
          and multiplies by 60 to store the value in seconds.

        @param(mins Timezone offset in minutes; will be multiplied by 60.)
     }
  procedure setTZ(mins: integer);

  {** Get the value which represents the maximum reading for the backend
   }
  function getLimitHigh: integer; virtual; abstract;
 
  {** Get the value which represents the minimum reading for the backend
   }
  function getLimitLow: integer; virtual; abstract;
  
  {** Get the URL to the remote source
   }
  function getAPIUrl: string;

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
        @returns(Threshold value as @code(glucose))
     }
  function getLevel(v: BGValLevel): glucose; virtual;

    {** Retrieve the current @code(CGMCore) thresholds in one record. }
  function getCGMCore: CGMCore;

    {** Initialize @code(CGMCore) thresholds to defaults.

        Defaults:
        - hi=401, lo=40, top=CGM_RANGE_HI_DISABLED (disabled), bottom=CGM_RANGE_LO_DISABLED (disabled)
     }
  procedure initCGMCore;

    {** Determine whether the API appears to be active using cached state.
        Returns @code(True) when a non-empty result has been observed via
        @code(noteReadings) within the last @code(getMaxAge) minutes.
        Performs no network I/O — safe to call from UI paint/resize handlers.

        @returns(@code(True) if a recent successful fetch is on record)
     }
  function checkActive: boolean;




private
  emitter: TTrndiAPIEmitter;
  credsEmitter: TTrndiCredentialsChanged;
  procedure setEmitter(e: TTrndiAPIEmitter);
  procedure setCredsEmitter(e: TTrndiCredentialsChanged);
public
const
  toMMOL = 0.05555555555555556; // Factor to multiply mg/dL by to get mmol/L
  toMGDL = 18.0156;             // Factor to multiply mmol/L to get mg/dL

  {** Sentinel: cgmRangeHi set to this value means "personalised upper bound disabled". }
  CGM_RANGE_HI_DISABLED = 500;
  {** Sentinel: cgmRangeLo set to this value means "personalised lower bound disabled". }
  CGM_RANGE_LO_DISABLED = 0;
    {** Provide a backend-specific caption for parameter labels in Settings.
        Index mapping (by convention):
        - 1: Label above the first edit (e.g., server URL or username)
        - 2: Label above the second edit (e.g., API key or password)
        - 3: Label above the optional third edit ("Extra"), if used

        Subclasses should override to supply meaningful texts.

        @param(Index 1..3 selecting which label to return)
        @returns(Caption text; empty string if not applicable)
     }
  class function ParamLabel(LabelName: APIParamLabel): string; virtual;
    {** Retrieve BG readings with optional extras path/params.
        This overload captures the raw response in a string.

        Subclasses must implement the abstract version. This overload typically
        forwards to it.

        @param(minNum  Time span hint (minutes), usage depends on subclass)
        @param(maxNum  Maximum number of readings to retrieve)
        @param(extras  Additional path or query hints; default empty)
        @returns(Array of BGReading)
     }
  function getReadings(minNum, maxNum: integer; extras: string = '';
    noCache: boolean = false): BGResults;

    {** Retrieve BG readings with raw JSON/text result returned by out-parameter.

        Subclasses must implement this method.

        @param(minNum   Time span hint (minutes), usage depends on subclass)
        @param(maxNum   Maximum number of readings to retrieve)
        @param(extras   Additional path or query hints)
        @param(res      Out parameter receiving raw response text)
        @param(noCache  When True, HTTP-based backends append a cache-busting
                        query param (`_=<timestamp>`) so intermediaries and
                        client-side HTTP caches return a fresh response.
                        Synthetic/debug backends ignore the flag.)
        @returns(Array of BGReading)
     }
  function getReadings(minNum, maxNum: integer; extras: string;
    out res: string; noCache: boolean): BGResults; virtual; abstract;

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
  constructor Create(user, pass: string); virtual;

    {** Destructor; releases owned resources. }
  destructor Destroy; override;

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
          - If @code(top <> CGM_RANGE_HI_DISABLED) and @code(v >= top) => @code(BGRangeHI)
          - Else if @code(bottom <> CGM_RANGE_LO_DISABLED) and @code(v <= bottom) => @code(BGRangeLO)
          - Else => @code(BGRange)

        @param(v BG value)
        @returns(BG level classification)
     }
  function getLevel(v: glucose): BGValLevel; virtual;

    {** Record that a fresh batch of readings has been observed.
        Callers (typically the main-thread fetch-result handler) pass the
        readings produced by a successful @code(getReadings) call. Non-empty
        batches stamp @code(lastReadingAt) with the current wall-clock time,
        which the @code(active) property then reads without doing I/O.

        @param(r Readings just produced; ignored if empty)
     }
  procedure noteReadings(const r: BGResults);

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
        - Applies time-weighted linear regression on time vs. BG value
        - Robustly refits, down-weighting outlier readings (sensor spikes,
          compression lows) so a single bad point cannot dominate the forecast
        - Estimates curvature via a weighted quadratic fit over the same
          robust weights, gently bending the forecast when the trend is
          accelerating or decelerating
        - Extrapolates forward to predict future values
        - Each prediction is spaced by the average interval between historical readings

        Sets @code(predictionConfidence) to a 0..1 score describing how cleanly
        the recent data fit the model — callers can use it to de-emphasize or
        hide forecasts built on noisy data.

        @param(numPredictions Number of future readings to predict; clamped to 1–20)
        @param(predictions    Out parameter receiving array of predicted readings)
        @returns(True if prediction succeeded; False if insufficient data)

        Notes:
        - Requires at least 3 historical readings for reliable prediction
        - Predictions become less accurate further into the future
        - Does not account for meals, insulin, or other external factors
        - Predicted readings have their @code(trend) field set to BGUnknown
     }
  function predictReadings(numPredictions: integer; out predictions: BGResults): boolean;

     {** Test a connection before using it
        @param(user The username, or URL)
        @param(pass The password or auth token)
        @param(extra Extra info)

        @returns(0 if it works, 1 if it doesnt, 3 if not supported)
     }
  class function testConnection(user, pass: string; var res: string): MaybeBool; virtual;

    {** Whether this backend needs an assisted browser login to obtain its
        credentials (rather than a plain username/password). When True the
        settings UI shows a "Log in" button and hides the username field, since
        the credential is captured via an external login helper (see
        guides/CareLink.md). Default: False.
        @returns(True if the backend uses the assisted login flow) }
  class function supportsWebLogin: boolean; virtual;

    {** Path of a Node.js helper script that performs the assisted login and
        prints the resulting credential (a JSON blob) to stdout. The path is
        relative to the Trndi executable directory; only its directory and
        file name are used when @code(WriteAssets) supplies the script instead
        (see below). When non-empty (and @code(supportsWebLogin) is True), the
        settings UI can run the script via Node and capture its output
        straight into the credential field instead of asking the user to run
        it in a terminal. Default: '' (no script — the UI falls back to
        showing manual instructions).
        @param(args Receives extra command-line arguments for the script,
                    e.g. a region flag like "--us"; empty when none.)
        @returns(Script path relative to the exe dir, or '' when unsupported) }
  class function webLoginScript(out args: string): string; virtual;

    {** Write this backend's Node.js login-helper assets (script + package
        manifest) into AFolder, overwriting whatever is already there so the
        copy always matches the running Trndi build. The settings UI calls
        this (with a folder under Trndi's writable settings directory) before
        running the helper named by @code(webLoginScript), since the folder
        next to the executable may not be writable (Program Files, a signed
        .app bundle, a read-only AppImage mount). Backends that ship a helper
        embed its files as compiled-in resources and extract them here.
        Default: no embedded assets.
        @param(AFolder Destination directory; created if missing)
        @returns(True if assets were written; False if this backend has none
                — the caller then falls back to the exe-relative path) }
  class function WriteAssets(const AFolder: string): boolean; virtual;

    {** Returns the name of the API
        @returns(Name of the API)
     }
  function getSystemName: string; virtual; abstract;

     {** Get the maximum age (in minutes) of readings provided by the backend
          @returns(Maximum age in minutes)
       }
  function getMaxAge: integer; virtual;

    {** How often this backend produces a new reading, in minutes.
        Used to calculate how many results to request so that all dot slots
        can be filled even at sub-5-minute reporting frequencies.
        Default: 5. Override in backends that upload more frequently (e.g. xDrip).
        @returns(Reporting interval in minutes, minimum 1)
     }
  function getReportingInterval: integer; virtual;

    {** Retrieve the current basal rate from the backend.
        Base implementation returns 0. Subclasses may override to fetch
        basal rate information from the data source.
        @returns(Current basal rate in U/hr, or 0 if unavailable)
     }
  function getBasalRate: single; virtual;
  {** Report whether this backend supports fetching basal rates.
      Default: False. Backends that implement reliable basal retrieval
      (e.g., NightScout v3) should override to return True. }
  function supportsBasal: boolean; virtual;
  {** Fetch the basal profile/schedule from the backend.
      @param(profile Out parameter receiving an array of basal entries)
      @returns(True if a profile was obtained; False otherwise)
}
  function getBasalProfile(out profile: TBasalProfile): boolean; virtual;

  {** Notify the owner that this backend's stored credentials changed
      (e.g. a rotated OAuth2 refresh token) and must be re-persisted.
      No-op when no handler is attached.
    @param(newCreds The new credential string to store as remote.creds)
 }
  procedure credentialsChanged(const newCreds: string);

  {** Emits an alert
    @param(msg The message)
 }
  procedure alert(const msg: string);

    {** Emits a (debug) log to be saved
    @param(msg The message)
 }
  procedure log(const msg: string);

    {** Emits a status message
        @param(msg The message)
     }
  procedure status(const msg: string);

    {** Emits a notice
        @param(msg The message)
     }
  procedure notice(const msg: string);

    // -------- Properties --------

  {** Backend's maximum reading. }
  property limitHI: integer read getLimitHigh;

  {** Backend's minimum reading. }
  property limitLO: integer read getLimitLow;

    {** Indexed read-only access to thresholds by @code(BGValLevel). }
  property threshold[lvl: BGValLevel]: glucose read getLevel;

    {** Read-only access to the full threshold record. }
  property cgm: CGMCore read getCGMCore;

    /// High threshold (modifiable).
  property cgmHi: integer read core.hi write core.hi;

    /// Low threshold (modifiable).
  property cgmLo: integer read core.lo write core.lo;

    /// Personalized in-range upper bound (CGM_RANGE_HI_DISABLED to disable).
  property cgmRangeHi: integer read core.top write core.top;

    /// Personalized in-range lower bound (CGM_RANGE_LO_DISABLED to disable).
  property cgmRangeLo: integer read core.bottom write core.bottom;

published
    /// Read-only time difference offset (seconds).
  property offset: integer read timeDiff;

    {** Write-only timezone offset setter.
        Note: Pass minutes (multiplied by 60 internally). }
  property timezone: integer write setTZ;

    /// Last error message produced by operations on this instance.
  property errormsg: string read lastErr;

    /// True when the last predictReadings call found a flat, non-accelerating
    /// trend — the UI can substitute a "no change" label instead of showing
    /// numerically identical predictions.
  property stablePrediction: boolean read predictionStable;

    /// Confidence (0..1) in the most recent predictReadings result, derived
    /// from the robust fit's residual error and the share of reading weight
    /// rejected as outliers. Near 1 means the recent data follows a clean
    /// trend; low values mean the data is too noisy for the forecast to be
    /// trusted. Zero when no prediction has been made (or the last one failed).
  property predictionConfidence: double read predictionQuality;

    {** Convenience status flag determined via @code(checkActive). }
  property active: boolean read checkActive;

    {** The source URL for the API, useful for eg alive testing. }
  property source: string read getApiUrl;

    {** The name of this API. }
  property systemName: string read getSystemName;

  property APIEmitter: TTrndiAPIEmitter write setEmitter;

    {** Handler invoked when the backend rotates its stored credentials. }
  property OnCredentialsChanged: TTrndiCredentialsChanged write setCredsEmitter;
end;

  {** Metaclass reference for TrndiAPI, so the UI can query class-level backend
      capabilities (ParamLabel, testConnection, supportsWebLogin) generically. }
  TrndiAPIClass = class of TrndiAPI;

implementation

procedure TrndiAPI.setEmitter(e: TTrndiAPIEmitter);
begin
  emitter := e;
end;

procedure TrndiAPI.setCredsEmitter(e: TTrndiCredentialsChanged);
begin
  credsEmitter := e;
end;

procedure TrndiAPI.credentialsChanged(const newCreds: string);
begin
  if assigned(credsEmitter) then
    credsEmitter(newCreds);
end;

procedure TrndiAPI.alert(const msg: string);
begin
  if assigned(emitter) then
    emitter(msg, TrndiAPIMsg.alert);
end;

procedure TrndiAPI.notice(const msg: string);
begin
  if assigned(emitter) then
    emitter(msg, TrndiAPIMsg.notice);
end;

procedure TrndiAPI.status(const msg: string);
begin
  if assigned(emitter) then
    emitter(msg, TrndiAPIMsg.status);
end;

procedure TrndiAPI.log(const msg: string);
begin
  if assigned(emitter) then
    emitter(msg, TrndiAPIMsg.log);
end;

  {------------------------------------------------------------------------------
  Get the maximum age (in minutes) of readings provided by the backend
  ------------------------------------------------------------------------------}
function TrndiAPI.getMaxAge: integer;
begin
  result := 1440; // Default to 24 hours
end;

function TrndiAPI.getReportingInterval: integer;
begin
  Result := 5; // Standard CGM interval
end;

{------------------------------------------------------------------------------
  Determine whether the API appears to be active without doing any I/O.
  Reads the timestamp stamped by noteReadings on the most recent successful
  fetch and returns True when it falls inside the backend's stated maximum
  reading age. Pre-fetch (lastReadingAt = 0) always reports inactive.
------------------------------------------------------------------------------}
function TrndiAPI.checkActive: boolean;
var
  ageMinutes: double;
begin
  if lastReadingAt <= 0 then
    Exit(false);

  if getMaxAge < 0 then
    Exit(true);

  ageMinutes := (Now - lastReadingAt) * (24 * 60);
  Result := (ageMinutes >= 0) and (ageMinutes <= getMaxAge);
end;

{------------------------------------------------------------------------------
  Record that a fresh batch of readings has been observed. Non-empty batches
  stamp lastReadingAt so the cached `active` check can answer without I/O.
------------------------------------------------------------------------------}
procedure TrndiAPI.noteReadings(const r: BGResults);
begin
  if Length(r) > 0 then
    lastReadingAt := Now;
end;

{------------------------------------------------------------------------------
  Base constructor.
  Initializes timezone from the OS, sets up native interface, and default CGM thresholds.
------------------------------------------------------------------------------}
constructor TrndiAPI.Create(user, pass: string);
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
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Resolve a threshold value by BGValLevel.
------------------------------------------------------------------------------}
function TrndiAPI.getLevel(v: BGValLevel): glucose;
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
  else
    Result := 0;
  end;
end;

{------------------------------------------------------------------------------
  Classify a numeric BG value into a BGValLevel.
------------------------------------------------------------------------------}
function TrndiAPI.getLevel(v: glucose): BGValLevel;
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
    if (core.top <> CGM_RANGE_HI_DISABLED) and (v >= core.top) then
      Result := BGRangeHI
    else
    if (core.bottom <> CGM_RANGE_LO_DISABLED) and (v <= core.bottom) then
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
  core.top    := CGM_RANGE_HI_DISABLED;
  core.bottom := CGM_RANGE_LO_DISABLED;
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
  Pre-sizes the result to the worst case (3 bytes per source byte) and writes
  by index — avoids the O(n²) reallocation pattern of repeated `+=` on
  ansistring.
------------------------------------------------------------------------------}
function TrndiAPI.encodeStr(src: string): string;
const
  HEX: array[0..15] of char = '0123456789ABCDEF';
var
  i, w: integer;
  c: char;
begin
  SetLength(Result, Length(src) * 3);
  w := 0;
  for i := 1 to Length(src) do
  begin
    c := src[i];
    if c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '~', '.'] then
    begin
      Inc(w);
      Result[w] := c;
    end
    else
    begin
      Result[w + 1] := '%';
      Result[w + 2] := HEX[(Ord(c) shr 4) and $0F];
      Result[w + 3] := HEX[Ord(c) and $0F];
      Inc(w, 3);
    end;
  end;
  SetLength(Result, w);
end;

{------------------------------------------------------------------------------
  Set timezone offset.
  Note: secs is treated as minutes and multiplied by 60 to store seconds.
------------------------------------------------------------------------------}
procedure TrndiAPI.setTZ(mins: integer);
begin
  tz := mins * 60;
end;

{------------------------------------------------------------------------------
  Get current base time as Unix epoch (seconds), adjusted by timeDiff.
------------------------------------------------------------------------------}
function TrndiAPI.getBasetime: int64;
begin
  Result := DateTimeToUnix(IncSecond(Now, timeDiff));
end;

{------------------------------------------------------------------------------
  Whether this backend supports fetching basal rates.
  Default implementation returns False; subclasses may override.
------------------------------------------------------------------------------}
function TrndiAPI.supportsBasal: boolean;
begin
  Result := false;
end;

{------------------------------------------------------------------------------
  Default implementation: no basal profile support.
------------------------------------------------------------------------------}
function TrndiAPI.getBasalProfile(out profile: TBasalProfile): boolean;
begin
  SetLength(profile, 0);
  Result := false;
end;

{------------------------------------------------------------------------------
  Get the last reading across a larger window (e.g., 24 hours).
------------------------------------------------------------------------------}
function TrndiAPI.getLast(var res: BGReading): boolean;
var
  r: BGResults;
begin
  Result := false;

  // Request readings spanning the backend's stated maximum age, limit 1
  r := getReadings(getMaxAge, 1);

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

  // 3× the backend's reporting cadence covers one fresh reading plus upload delay
  r := getReadings(getReportingInterval * 3, 1);

  if Length(r) > 0 then
  begin
    res := r[0];
    Result := true;
  end;
end;

{------------------------------------------------------------------------------
  Convert a JavaScript millisecond epoch to TDateTime.
  When @code(correct) is true, apply timezone calibration (tz) to correct server
  clock skew and return a local TDateTime. The function interprets @code(ts)
  as milliseconds since Unix epoch (UTC).
------------------------------------------------------------------------------}
function TrndiAPI.JSToDateTime(ts: int64; correct: boolean): TDateTime;
var
  unix_ts: int64;
begin
  // Interpret epoch milliseconds as seconds since Unix epoch
  unix_ts := ts div 1000;
  if correct then
    // tz is stored in seconds; subtract it from the epoch seconds
    unix_ts := unix_ts - tz;
  // Return as system-local TDateTime (UseUTC = False)
  Result := UnixToDateTime(unix_ts, false);
end;

{------------------------------------------------------------------------------
  Convenience overload that forwards to the abstract variant and discards raw text.
------------------------------------------------------------------------------}
function TrndiAPI.getReadings(minNum, maxNum: integer; extras: string = '';
  noCache: boolean = false): BGResults;
var
  res: string;
begin
  lastErr := '';
  Result := getReadings(minNum, maxNum, extras, res, noCache);
end;

{------------------------------------------------------------------------------
  Default parameter label provider for Settings UI.
  Subclasses override to provide backend-specific captions.
------------------------------------------------------------------------------}
class function TrndiAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  case LabelName of
  APLUser:
    Result := 'Server Address';
  APLPass:
    Result := 'API Key';
  APLDesc:
    Result := '';
  APLDescHTML:
    Result := '';
  APLCopyright:
    Result := 'Trndi contributors';
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
  n, i, j, m, gapCutIdx, minutesPerReport, robustPass: integer;
  sumW, sumWX, sumWY, sumWXX, sumWXY: double;
  slope, intercept, weight, alpha: double;
  timeValues: array of double;
  bgValues: array of double;
  weights: array of double;
  residuals, sortedRes, candWeights, pairSlopes: array of double;
  medRes, scaleMAD, cutoff, resid: double;
  sumW0, sumWr2, rmse, keptRatio: double;
  avgInterval, minutesDiff: double;
  lastTime: TDateTime;
  predictedTime: TDateTime;
  predictedValue, prevValue: double;
  trendDelta: single;
  accel, tFromLast: double;
  s0, s1, s2, s3, s4, sy, sxy, sx2y, det, xc: double;

  // Sorts the first count entries in place (insertion sort; n ≤ 12 here,
  // pair count ≤ 66) and returns their median.
  function medianOf(var values: array of double; count: integer): double;
  var
    a, b: integer;
    v: double;
  begin
    for a := 1 to count - 1 do
    begin
      v := values[a];
      b := a - 1;
      while (b >= 0) and (values[b] > v) do
      begin
        values[b + 1] := values[b];
        Dec(b);
      end;
      values[b + 1] := v;
    end;
    if (count mod 2) = 1 then
      Result := values[count div 2]
    else
      Result := (values[count div 2 - 1] + values[count div 2]) / 2;
  end;

begin
  Result := false;
  predictionStable := false;
  predictionQuality := 0;
  lastErr := '';
  SetLength(predictions, 0);

  if numPredictions < 1 then
    Exit;

  if numPredictions > 20 then
    numPredictions := 20;

  minutesPerReport := getReportingInterval;

  // Fetch enough history to cover 12 reporting intervals regardless of backend cadence
  historicalReadings := getReadings(minutesPerReport * 12, 12);
  n := Length(historicalReadings);

  if n < 3 then
  begin
    lastErr := 'Insufficient data for prediction (need at least 3 readings)';
    Exit;
  end;

  // Sort oldest first — regression expects ascending time order
  SortReadingsAscending(historicalReadings);

  // Gap detection: scan backward to find the most recent gap > 2× the backend's
  // reporting interval. Readings before a gap describe a different signal context
  // — a sensor dropout, session restart, or period of missing data — and would
  // pull the regression toward a stale trend.
  gapCutIdx := 0;
  for i := n - 1 downto 1 do
    if (historicalReadings[i].date - historicalReadings[i - 1].date) > ((minutesPerReport * 2.0) / (24 * 60)) then
    begin
      gapCutIdx := i;
      Break;
    end;

  if (gapCutIdx > 0) and ((n - gapCutIdx) >= 3) then
  begin
    historicalReadings := Copy(historicalReadings, gapCutIdx, n - gapCutIdx);
    n := Length(historicalReadings);
  end;

  SetLength(timeValues, n);
  SetLength(bgValues, n);
  SetLength(weights, n);

  // Exponential weights decaying by actual elapsed time: oldest reading gets
  // weight 1, newer readings get exponentially higher weight with a half-life
  // of 15 minutes. Using real elapsed time (not reading index) means irregular
  // backends — where gaps between readings vary — are weighted correctly.
  alpha := Ln(2) / 15.0;
  for i := 0 to n - 1 do
  begin
    timeValues[i] := (historicalReadings[i].date - historicalReadings[0].date) * 24 * 60;
    bgValues[i]   := historicalReadings[i].convert(BGUnit.mgdl);
    weights[i]    := Exp(alpha * timeValues[i]);
  end;

  // Weighted linear regression: y = slope * x + intercept
  sumW   := 0;
  sumWX  := 0;
  sumWY  := 0;
  sumWXX := 0;
  sumWXY := 0;

  for i := 0 to n - 1 do
  begin
    weight  := weights[i];
    sumW    := sumW   + weight;
    sumWX   := sumWX  + weight * timeValues[i];
    sumWY   := sumWY  + weight * bgValues[i];
    sumWXX  := sumWXX + weight * timeValues[i] * timeValues[i];
    sumWXY  := sumWXY + weight * timeValues[i] * bgValues[i];
  end;

  if (sumW * sumWXX - sumWX * sumWX) = 0 then
  begin
    lastErr := 'Cannot calculate trend (invalid time distribution)';
    Exit;
  end;

  slope     := (sumW * sumWXY - sumWX * sumWY) / (sumW * sumWXX - sumWX * sumWX);
  intercept := (sumWY - slope * sumWX) / sumW;

  // Robust refit: a single spurious reading — compression low, sensor jitter —
  // would otherwise dominate the fit, because the exponential time weighting
  // hands the newest points the largest weights. Start from a Theil–Sen fit
  // (median of all pairwise slopes): unlike the least-squares fit above, it
  // tolerates outliers even in the newest reading, where high weight plus high
  // leverage make least squares chase the spike and hide it from residual-based
  // rejection. Two reweighting passes then measure residuals against the current
  // fit, estimate the noise scale robustly (median absolute residual),
  // down-weight points whose residual is large relative to that scale, and
  // refit with the exponential recency weights. Clean data is untouched: with
  // residuals below the noise floor the biweight factors stay ≈1 and the fit
  // matches plain weighted least squares.
  sumW0 := sumW;
  SetLength(residuals, n);
  SetLength(sortedRes, n);
  SetLength(candWeights, n);
  SetLength(pairSlopes, (n * (n - 1)) div 2);
  m := 0;
  for i := 0 to n - 2 do
    for j := i + 1 to n - 1 do
      if timeValues[j] > timeValues[i] then
      begin
        pairSlopes[m] := (bgValues[j] - bgValues[i]) / (timeValues[j] - timeValues[i]);
        Inc(m);
      end;
  if m > 0 then
  begin
    slope := medianOf(pairSlopes, m);
    for i := 0 to n - 1 do
      sortedRes[i] := bgValues[i] - slope * timeValues[i];
    intercept := medianOf(sortedRes, n);
  end;

  for robustPass := 1 to 2 do
  begin
    for i := 0 to n - 1 do
    begin
      residuals[i] := Abs(bgValues[i] - (slope * timeValues[i] + intercept));
      sortedRes[i] := residuals[i];
    end;
    medRes := medianOf(sortedRes, n);

    // 1.4826 × MAD estimates the Gaussian noise sigma. Floor at 2.5 mg/dL so
    // ordinary CGM quantization noise never triggers rejection on clean data.
    scaleMAD := 1.4826 * medRes;
    if scaleMAD < 2.5 then
      scaleMAD := 2.5;
    cutoff := 4.685 * scaleMAD;

    sumW   := 0;
    sumWX  := 0;
    sumWY  := 0;
    sumWXX := 0;
    sumWXY := 0;
    for i := 0 to n - 1 do
    begin
      // Rebuild from the base exponential weight each pass (true IRLS) rather
      // than compounding biweight factors across passes.
      if residuals[i] >= cutoff then
        weight := 0
      else
        weight := Exp(alpha * timeValues[i]) * Sqr(1 - Sqr(residuals[i] / cutoff));
      candWeights[i] := weight;
      sumW   := sumW   + weight;
      sumWX  := sumWX  + weight * timeValues[i];
      sumWY  := sumWY  + weight * bgValues[i];
      sumWXX := sumWXX + weight * timeValues[i] * timeValues[i];
      sumWXY := sumWXY + weight * timeValues[i] * bgValues[i];
    end;

    // Rejection left fewer than two distinct points — keep the previous fit
    if Abs(sumW * sumWXX - sumWX * sumWX) < 1e-6 then
      Break;

    for i := 0 to n - 1 do
      weights[i] := candWeights[i];
    slope     := (sumW * sumWXY - sumWX * sumWY) / (sumW * sumWXX - sumWX * sumWX);
    intercept := (sumWY - slope * sumWX) / sumW;
  end;

  // Confidence: how well the trusted data fits a line (weighted RMSE mapped
  // onto 0..1, hitting 0 at 15 mg/dL of residual noise), degraded further by
  // the share of exponential weight the robust pass had to throw away.
  sumWr2 := 0;
  sumW   := 0;
  for i := 0 to n - 1 do
  begin
    resid  := bgValues[i] - (slope * timeValues[i] + intercept);
    sumWr2 := sumWr2 + weights[i] * resid * resid;
    sumW   := sumW + weights[i];
  end;
  if (sumW > 0) and (sumW0 > 0) then
  begin
    rmse := Sqrt(sumWr2 / sumW);
    keptRatio := sumW / sumW0;
    if keptRatio > 1 then
      keptRatio := 1;
    predictionQuality := (1 - rmse / 15.0) * keptRatio;
    if predictionQuality < 0 then
      predictionQuality := 0;
  end;

  // Second-derivative acceleration from a weighted quadratic fit
  // y = c0 + c1·x + c2·x², accel = 2·c2. Uses the same robust weights the
  // linear fit settled on, so a rejected outlier cannot bend the curvature
  // estimate, and for perfectly linear data c2 is exactly zero. Times are
  // centered on the last reading for numerical conditioning (the curvature
  // coefficient is shift-invariant). Capped to ±0.03 mg/dL/min² so the
  // quadratic correction stays subtle — CGM noise makes aggressive
  // acceleration estimates unreliable, so we treat this as a gentle curve
  // hint rather than a hard model.
  s0 := 0; s1 := 0; s2 := 0; s3 := 0; s4 := 0;
  sy := 0; sxy := 0; sx2y := 0;
  for i := 0 to n - 1 do
  begin
    xc     := timeValues[i] - timeValues[n-1];
    weight := weights[i];
    s0   := s0   + weight;
    s1   := s1   + weight * xc;
    s2   := s2   + weight * xc * xc;
    s3   := s3   + weight * xc * xc * xc;
    s4   := s4   + weight * xc * xc * xc * xc;
    sy   := sy   + weight * bgValues[i];
    sxy  := sxy  + weight * xc * bgValues[i];
    sx2y := sx2y + weight * xc * xc * bgValues[i];
  end;
  det := s0 * (s2 * s4 - s3 * s3) - s1 * (s1 * s4 - s3 * s2) +
    s2 * (s1 * s3 - s2 * s2);
  // Fewer than three distinct time points leaves the quadratic
  // underdetermined — fall back to no curvature (relative threshold: the
  // determinant scales like s0·s2·s4).
  if Abs(det) > 1e-9 * (Abs(s0 * s2 * s4) + 1) then
    accel := 2 * (s0 * (s2 * sx2y - s3 * sxy) - s1 * (s1 * sx2y - s2 * sxy) +
      sy * (s1 * s3 - s2 * s2)) / det
  else
    accel := 0;
  if accel >  0.03 then accel :=  0.03;
  if accel < -0.03 then accel := -0.03;

  // Stable when slope is well below the FortyFive threshold (±1 mg/dL/min)
  // and acceleration is negligible — predictions would be noise.
  predictionStable := (Abs(slope) < 0.5) and (Abs(accel) < 0.005);

  if n > 1 then
    avgInterval := (historicalReadings[n-1].date - historicalReadings[0].date) / (n - 1)
  else
    avgInterval := 5.0 / (24 * 60);

  // Precompute once — used for both rate-of-change clamp and trend classification
  minutesDiff := avgInterval * 24 * 60;

  SetLength(predictions, numPredictions);
  lastTime  := historicalReadings[n-1].date;

  // Anchor the rate-of-change clamp on the fitted value at the last reading's
  // time rather than the raw reading: if the newest point is a spike the
  // robust fit rejected, clamping against it would drag every prediction
  // toward the spike anyway.
  prevValue := slope * timeValues[n-1] + intercept;

  for i := 0 to numPredictions - 1 do
  begin
    predictedTime  := lastTime + (avgInterval * (i + 1));
    tFromLast      := (i + 1) * minutesDiff;
    predictedValue := slope * ((predictedTime - historicalReadings[0].date) * 24 * 60) + intercept;
    predictedValue := predictedValue + 0.5 * accel * tFromLast * tFromLast;

    // Instantaneous rate at this prediction's time, expressed as delta over one
    // interval — accounts for acceleration so later predictions get a different
    // trend arrow than earlier ones.
    trendDelta := (slope + accel * tFromLast) * minutesDiff;

    // Clamp rate of change to ±3 mg/dL/min relative to the previous value
    if predictedValue < prevValue - (3.0 * minutesDiff) then
      predictedValue := prevValue - (3.0 * minutesDiff);
    if predictedValue > prevValue + (3.0 * minutesDiff) then
      predictedValue := prevValue + (3.0 * minutesDiff);

    // Clamp to physiological range
    if predictedValue < 20 then
      predictedValue := 20;
    if predictedValue > 600 then
      predictedValue := 600;

    predictions[i].Init(BGUnit.mgdl, BGUnit.mgdl, 'PredictionAPI');
    predictions[i].update(predictedValue, BGPrimary, BGUnit.mgdl);
    predictions[i].date  := predictedTime;
    predictions[i].trend := CalculateTrendFromDelta(trendDelta);
    predictions[i].level := getLevel(predictedValue);

    prevValue := predictedValue;
  end;

  Result := true;
end;

{------------------------------------------------------------------------------
  Test connection details
------------------------------------------------------------------------------}
class function TrndiAPI.testConnection(user, pass: string; var res: string): maybeBool;
begin
  res := 'This API has no implemented test function';
  result := maybebool.none; // Not supported
end;

{------------------------------------------------------------------------------
  Assisted-login capability. Default: unsupported. Backends that require a
  browser login helper (e.g. CareLink) override this to True.
------------------------------------------------------------------------------}
class function TrndiAPI.supportsWebLogin: boolean;
begin
  Result := false;
end;

{------------------------------------------------------------------------------
  Assisted-login helper script. Default: none. Backends that ship a runnable
  Node.js login helper (e.g. CareLink) override this to return the script path.
------------------------------------------------------------------------------}
class function TrndiAPI.webLoginScript(out args: string): string;
begin
  args := '';
  Result := '';
end;

{------------------------------------------------------------------------------
  Assisted-login helper assets. Default: none. Backends that embed a Node.js
  login helper as compiled-in resources (e.g. CareLink) override this to
  extract them into AFolder.
------------------------------------------------------------------------------}
class function TrndiAPI.WriteAssets(const AFolder: string): boolean;
begin
  Result := false;
end;

{------------------------------------------------------------------------------
  Retrieve the current basal rate from the backend.
  Base implementation returns 0.
------------------------------------------------------------------------------}
function TrndiAPI.getBasalRate: single;
begin
  result := 0; // Base implementation returns 0
  // Subclasses may override to fetch basal rate from their data source
end;

function TrndiAPI.getAPIUrl: string;
begin
  result := baseUrl;
end;

end.
