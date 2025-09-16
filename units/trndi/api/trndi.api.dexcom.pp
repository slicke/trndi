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
unit trndi.api.dexcom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  // Trndi units
  trndi.types, trndi.api, trndi.native,
  // FPC units
  fpjson, jsonparser, dateutils, StrUtils;

(*******************************************************************************
  Dexcom Share API endpoint definitions
 ******************************************************************************)

const
  {** Dexcom Share login by account name endpoint. }
  DEXCOM_LOGIN_ENDPOINT                = 'General/LoginPublisherAccountByName';
  {** Dexcom Share authenticate account endpoint. }
  DEXCOM_AUTHENTICATE_ENDPOINT         = 'General/AuthenticatePublisherAccount';
  {** Verify receiver/transmitter serial assignment status. }
  DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT = 'Publisher/CheckMonitoredReceiverAssignmentStatus';
  {** Retrieve system UTC time from Dexcom Share. }
  DEXCOM_TIME_ENDPOINT                 = 'General/SystemUtcTime';
  {** Fetch latest glucose values. }
  DEXCOM_GLUCOSE_READINGS_ENDPOINT     = 'Publisher/ReadPublisherLatestGlucoseValues';
  {** Fetch alert settings (may not always be returned). }
  DEXCOM_ALERT_ENDPOINT                = 'Publisher/ReadSubscriberAlertSettings';

  {** Dexcom Share application ID used by mobile apps (commonly reused). }
  DEXCOM_APPLICATION_ID = 'd89443d2-327c-4a6f-89e5-496bbb0317db';

  {** Base URL (US region) for Dexcom Share services. }
  DEXCOM_BASE_URL_US    = 'https://share2.dexcom.com/ShareWebServices/Services';
  {** Base URL (Worldwide) for Dexcom Share services. }
  DEXCOM_BASE_URL_WORLD = 'https://shareous1.dexcom.com/ShareWebServices/Services';

  {** Host (US region) for Dexcom Share. }
  DEXCOM_HOST_US    = 'share2.dexcom.com';
  {** Host (Worldwide) for Dexcom Share. }
  DEXCOM_HOST_WORLD = 'shareous1.dexcom.com';

  {** Helper array mapping region selector to base URL.
      Index: False = WORLD, True = USA (when extra = 'usa'). }
  DEXCOM_BASE_URLS: array[false..true] of string =
    (DEXCOM_BASE_URL_WORLD, DEXCOM_BASE_URL_US);

  {** Helper array mapping region selector to base host.
      Index: False = WORLD, True = USA (when extra = 'usa'). }
  DEXCOM_BASE_HOSTS: array[false..true] of string =
    (DEXCOM_HOST_WORLD, DEXCOM_HOST_US);

type
  (*******************************************************************************
    Dexcom class

    Inherits from @code(TrndiAPI) and implements connectivity and reading
    retrieval for Dexcom Share. Responsible for session handling, time-sync,
    and mapping Share responses into @code(BGResults).
   ******************************************************************************)
  Dexcom = class(TrndiAPI)
  private
    FBaseHost:  string;   /// The chosen Dexcom host (USA or Worldwide)
    FUserName:  string;   /// Dexcom Share account username
    FPassword:  string;   /// Dexcom Share account password
    FSessionID: string;   /// Session ID returned by Dexcom after authentication
    FCalcDiff:  boolean;  /// If True, compute deltas between consecutive readings

    {** Check that the current session token looks valid (non-empty and not dummy). }
    function CheckSession: boolean;

    {** Check whether a Dexcom receiver/transmitter serial is assigned to the account. }
    function CheckSerialNumber(const ASerial: string): boolean;

  public
    {** Create a Dexcom API client.
        Matches the parent signature. Defaults to calculating deltas.

        @param(user   Dexcom username)
        @param(pass   Dexcom password)
        @param(extra  Region selector: use 'usa' for US servers; otherwise WORLD)
     }
    constructor Create(user, pass, extra: string); override;

    {** Overloaded constructor that allows explicitly enabling/disabling delta calc.

        @param(user     Dexcom username)
        @param(pass     Dexcom password)
        @param(extra    Region selector: use 'usa' for US servers; otherwise WORLD)
        @param(ACalcDiff Whether to compute deltas between consecutive readings)
     }
    constructor Create(user, pass, extra: string; ACalcDiff: boolean); overload;

    {** Authenticate with Dexcom Share, establish a session, and synchronize time.

        Workflow:
        1) Authenticate to get initial session token.
        2) Validate session.
        3) Call login-by-name to finalize session.
        4) Retrieve server UTC time and compute @code(timeDiff).

        @returns(True if session established and time synchronized; otherwise False
                 and @code(errormsg) is set.)
     }
    function Connect: boolean; override;

    {** Retrieve latest glucose readings.

        @param(AMinutes  Time window in minutes to fetch)
        @param(AMaxCount Maximum number of readings to return)
        @param(extras    Reserved; not used here)
        @param(res       Out param receiving raw JSON payload of readings)
        @returns(Array of @code(BGReading); may be empty if none/failed)
     }
    function GetReadings(AMinutes, AMaxCount: integer; extras: string; out res: string): BGResults; override;

  published
    {** The effective base URL used for API requests. }
    property Remote       : string  read baseUrl;
    {** Dexcom username for this client instance. }
    property User         : string  read FUserName;
    {** Current Dexcom session token (when connected). }
    property Session      : string  read FSessionID;
    {** Whether this client computes deltas between consecutive readings. }
    property CalculateDiff: boolean read FCalcDiff;
  end;

implementation

resourcestring
  sErrDexPostLogin = 'Login error during post-authentication steps';
  sErrDexPass      = 'Invalid Dexcom password or account credentials';
  sErrDexLogin     = 'Login error: Could not establish a valid session';

{** Constructor (override).
    Delegates to the overloaded constructor with @code(ACalcDiff=True). }
constructor Dexcom.Create(user, pass, extra: string);
begin
  // Call overloaded constructor with default: calculate deltas
  Create(user, pass, extra, true);
end;

{** Constructor (overload).
    Configure user-agent, region (base URL and host), credentials, and delta flag.
    Calls inherited base constructor to init common pieces (thresholds, native, tz). }
constructor Dexcom.Create(user, pass, extra: string; ACalcDiff: boolean);
begin
  // Common Dexcom Share user-agent string observed in official apps
  ua := 'Dexcom Share/3.0.2.11 CFNetwork/711.2.23 Darwin/14.0.0';

  // Select region-specific base URL and host (extra='usa' selects US)
  baseUrl   := DEXCOM_BASE_URLS[extra = 'usa'];
  FBaseHost := DEXCOM_BASE_HOSTS[extra = 'usa'];

  // Store credentials and preferences
  FUserName := user;
  FPassword := pass;
  FCalcDiff := ACalcDiff;

  // Parent ctor sets timezone, allocates native helper, and initializes thresholds
  inherited Create(user, pass, extra);
end;

{** Connect to Dexcom Share and establish a valid session.
    Performs authentication sequence and time synchronization. }
function Dexcom.Connect: boolean;
var
  LBody, LResponse, LTimeResponse, LTimeString: string;
  LServerDateTime: TDateTime;
begin
  // Prepare JSON payload for authentication
  LBody := Format(
    '{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
    [FUserName, FPassword, DEXCOM_APPLICATION_ID]
  );

  // 1) Authenticate to obtain preliminary session token
  FSessionID := StringReplace(
    native.Request(true, DEXCOM_AUTHENTICATE_ENDPOINT, [], LBody),
    '"', '', [rfReplaceAll]
  );

  // If response indicates password/credential issues, fail early
  if Pos('AccountPassword', FSessionID) > 0 then
  begin
    Result  := false;
    lastErr := sErrDexPass + ' (Dex1)';
    Exit;
  end;

  // 2) Validate current session token
  if not CheckSession then
  begin
    Result  := false;
    lastErr := sErrDexLogin + ' (Dex2)';
    Exit;
  end;

  // 3) Finalize session by logging in by account name
  FSessionID := StringReplace(
    native.Request(true, DEXCOM_LOGIN_ENDPOINT, [], LBody),
    '"', '', [rfReplaceAll]
  );

  // Validate once more
  if not CheckSession then
  begin
    Result  := false;
    lastErr := sErrDexPostLogin + ' (Dex3)';
    Exit;
  end;

  // 4) Retrieve system UTC time for time-diff calibration
  LTimeResponse := native.Request(false, DEXCOM_TIME_ENDPOINT, [], '');

  // Dexcom may respond as XML-like <SystemTime> or JSON-ish /Date(ms)/ format
  if Pos('>', LTimeResponse) > 0 then
  begin
    // Example: <SystemTime>YYYY-MM-DDTHH:mm:ss</SystemTime>
    LTimeString := ExtractDelimited(5, LTimeResponse, ['>', '<']);
    if LTimeString <> '' then
      // Parse the ISO-like timestamp (truncate to 19 chars to ignore fractions/timezone)
      LServerDateTime := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', Copy(LTimeString, 1, 19));
  end
  else
  begin
    // Example: {"ServerTime":"/Date(1610464324000)/"} or similar payload
    LTimeString := ExtractDelimited(2, LTimeResponse, ['(', ')']);
    if LTimeString <> '' then
      // LTimeString in ms; JSToDateTime expects milliseconds when correct=false path used
      LServerDateTime := JSToDateTime(StrToInt64(LTimeString), false);
  end;

  // If we failed to parse any time value, abort with error
  if LTimeString = '' then
  begin
    lastErr := 'Cannot parse Dexcom time/zone data';
    Result  := false;
    Exit;
  end;

  // Compute time difference between server UTC and local UTC
  timeDiff := SecondsBetween(LServerDateTime, LocalTimeToUniversal(Now));
  if timeDiff < 0 then
    timeDiff := 0;
  // Store negative offset to match consumer logic elsewhere in the codebase
  timeDiff := -1 * timeDiff;

  Result := true;
end;

{** Validate the current session token for obvious invalid values. }
function Dexcom.CheckSession: boolean;
begin
  Result :=
    (FSessionID <> '') and
    (FSessionID <> '00000000-0000-0000-0000-000000000000');
end;

{** Check if a Dexcom receiver/transmitter serial is assigned to this account.
    Returns True if the server reports 'AssignedToYou'. }
function Dexcom.CheckSerialNumber(const ASerial: string): boolean;
var
  LParams: array[1..2] of string;
  LResponse: string;
begin
  Result := false;

  // Encode parameters to be safe in URLs
  LParams[1] := 'sessionId='    + encodeStr(FSessionID);
  LParams[2] := 'serialNumber=' + encodeStr(ASerial);

  // Dexcom returns 'AssignedToYou' when serial number is associated
  LResponse := native.Request(true, DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT, LParams, '');
  if LResponse = 'AssignedToYou' then
    Result := true;
end;

{** Retrieve glucose readings for the last @code(AMinutes) minutes.
    Returns up to @code(AMaxCount) samples, optionally computing deltas. }
function Dexcom.GetReadings(AMinutes, AMaxCount: integer; extras: string; out res: string): BGResults;

  // Helper: convert Dexcom /Date(ms)/ string to TDateTime
  function DexTimeToTDateTime(const S: string): TDateTime;
  var
    LMsString: string;
    LMs: int64;
  begin
    // Example: /Date(1610464324000)/
    LMsString := Copy(S, 6, Length(S) - 6);               // drop '/Date('
    LMsString := StringReplace(LMsString, ')', '', []);   // drop ')'
    LMs       := StrToInt64(LMsString);
    Result    := UnixToDateTime(LMs div 1000, false);     // milliseconds -> seconds
  end;

var
  LParams:  array[1..3] of string;
  LGlucoseJSON, LAlertJSON, LTrendStr: string;
  LData:    TJSONData;
  i, LTrendCode: integer;
  LTrendEnum: BGTrend;
begin
  // Guard input ranges based on common Share limits
  if (AMinutes < 1) or (AMinutes > 1440) then
    raise Exception.Create('GetReadings error: AMinutes out of valid range (1..1440)');
  if (AMaxCount < 1) or (AMaxCount > 288) then
    raise Exception.Create('GetReadings error: AMaxCount out of valid range (1..288)');

  // Build query parameters for Dexcom Share
  LParams[1] := 'sessionId=' + FSessionID;
  LParams[2] := 'minutes='   + IntToStr(AMinutes);
  LParams[3] := 'maxCount='  + IntToStr(AMaxCount);

  // Fetch glucose values; some deployments also allow reading alert settings
  LGlucoseJSON := native.Request(true, DEXCOM_GLUCOSE_READINGS_ENDPOINT, LParams, '');
  LAlertJSON   := native.Request(true, DEXCOM_ALERT_ENDPOINT,          LParams, '');

  res := LGlucoseJSON;

  // Return empty result on empty payload
  if LGlucoseJSON = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // Parse JSON array
  LData := GetJSON(LGlucoseJSON);
  SetLength(Result, LData.Count);

  // Iterate items and map to BGReading
  for i := 0 to LData.Count - 1 do
  try
    // Initialize reading (mg/dL units)
    Result[i].Init(mgdl);
    // Mark source for downstream consumers/debugging
    Result[i].updateEnv('Dexcom');

    // Compute BG value and optional delta
    if (FCalcDiff) and (i > 0) then
      Result[i].Update(
        LData.Items[i].FindPath('Value').AsFloat,
        LData.Items[i].FindPath('Value').AsFloat -
        LData.Items[i - 1].FindPath('Value').AsFloat
      )
    else if FCalcDiff then
      // First item: delta not computable; use 0
      Result[i].Update(LData.Items[i].FindPath('Value').AsFloat, 0)
    else
      // Delta not requested; store only BG value (using sentinel)
      Result[i].Update(LData.Items[i].FindPath('Value').AsFloat, BG_NO_VAL);

    // Interpret "Trend" (may be numeric code or string)
    LTrendStr := LData.Items[i].FindPath('Trend').AsString;

    // If Trend could be a numeric code, you can map codes as needed here.
    if not TryStrToInt(LTrendStr, LTrendCode) then
    begin
      // Treat as text; map to enum via lookup table
      for LTrendEnum in BGTrend do
      begin
        if BG_TRENDS_STRING[LTrendEnum] = LTrendStr then
        begin
          Result[i].trend := LTrendEnum;
          Break;
        end;
        // Default until proven otherwise in this loop iteration
        Result[i].trend := TdPlaceholder;
      end;
    end
    else
    begin
      // Numeric mapping not defined; keep placeholder unless you add a map
      Result[i].trend := TdPlaceholder;
    end;

    // Convert Dexcom timestamp "/Date(ms)/" to TDateTime
    Result[i].date := DexTimeToTDateTime(LData.Items[i].FindPath('ST').AsString);

    // Classify level per thresholds
    Result[i].level := getLevel(Result[i].val);

  except
    on E: Exception do
    begin
      // If parsing fails for an item, clear that reading to keep array shape
      Result[i].Clear;
    end;
  end;

  // Set default thresholds (can be made configurable or sourced from alerts)
  cgmHi      := 160;
  cgmLo      :=  60;
  cgmRangeHi := 159;
  cgmRangeLo :=  61;
end;

end.
