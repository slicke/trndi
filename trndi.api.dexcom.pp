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
  // Common Dexcom endpoints
DEXCOM_LOGIN_ENDPOINT                = 'General/LoginPublisherAccountByName';
DEXCOM_AUTHENTICATE_ENDPOINT         = 'General/AuthenticatePublisherAccount';
DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT = 'Publisher/CheckMonitoredReceiverAssignmentStatus';
DEXCOM_TIME_ENDPOINT                 = 'General/SystemUtcTime';
DEXCOM_GLUCOSE_READINGS_ENDPOINT     = 'Publisher/ReadPublisherLatestGlucoseValues';
DEXCOM_ALERT_ENDPOINT                = 'Publisher/ReadSubscriberAlertSettings';

  // Dexcom application ID (commonly used in Share)
DEXCOM_APPLICATION_ID = 'd89443d2-327c-4a6f-89e5-496bbb0317db';

  // Base URLs for Dexcom Share
DEXCOM_BASE_URL_US    = 'https://share2.dexcom.com/ShareWebServices/Services';
DEXCOM_BASE_URL_WORLD = 'https://shareous1.dexcom.com/ShareWebServices/Services';

  // Host names for Dexcom Share
DEXCOM_HOST_US    = 'share2.dexcom.com';
DEXCOM_HOST_WORLD = 'shareous1.dexcom.com';

  // Helper arrays to choose region
DEXCOM_BASE_URLS: array[false..true] of string =
  (DEXCOM_BASE_URL_WORLD, DEXCOM_BASE_URL_US);
DEXCOM_BASE_HOSTS: array[false..true] of string =
  (DEXCOM_HOST_WORLD, DEXCOM_HOST_US);

type
  (*******************************************************************************
    Dexcom class
    ------------
    Inherits from TrndiAPI, implementing the `connect` and `getReadings` methods
    for Dexcom Share. It handles session creation, endpoint calls, and glucose
    reading retrieval.
   ******************************************************************************)
Dexcom = class(TrndiAPI)
private
  FBaseHost:  string;  // The chosen Dexcom host (USA or Worldwide)
  FUserName:  string;  // Dexcom Share account username
  FPassword:  string;  // Dexcom Share account password
  FSessionID: string;  // Session ID returned by Dexcom on successful login
  FCalcDiff:  boolean; // If True, calculates the delta (difference) between consecutive readings

    // Internal helpers
  function CheckSession: boolean;
  function CheckSerialNumber(const ASerial: string): boolean;
public
    // Override must match the parent's signature exactly
  constructor Create(user, pass, extra: string); override;

    // Overloaded constructor for additional parameter
  constructor Create(user, pass, extra: string; ACalcDiff: boolean); overload;

    // Required overrides from TrndiAPI
  function Connect: boolean; override;
  function GetReadings(AMinutes, AMaxCount: integer; extras: string = ''): BGResults; override;

published
    // Expose some properties
  property Remote       : string  read baseUrl;      // Full base URL
  property User         : string  read FUserName;    // Dexcom username
  property Session      : string  read FSessionID;   // Current Dexcom session ID
  property CalculateDiff: boolean read FCalcDiff;    // Whether differences are calculated
end;


implementation

resourcestring
sErrDexPostLogin = 'Login error during post-authentication steps';
sErrDexPass      = 'Invalid Dexcom password or account credentials';
sErrDexLogin     = 'Login error: Could not establish a valid session';

{-------------------------------------------------------------------------------
  Dexcom.Create (Override)
  ------------------------
  Matches the parent constructor signature: (user, pass, extra: string).
  Defaults to FCalcDiff = True.
-------------------------------------------------------------------------------}
constructor Dexcom.Create(user, pass, extra: string);
begin
  // Call overloaded constructor with ACalcDiff = True
  Create(user, pass, extra, true);
end;

{-------------------------------------------------------------------------------
  Dexcom.Create (Overload)
  ------------------------
  Allows specifying whether to calculate deltas for consecutive BG readings.
-------------------------------------------------------------------------------}
constructor Dexcom.Create(user, pass, extra: string; ACalcDiff: boolean);
begin
  // Common Dexcom Share user-agent
  ua := 'Dexcom Share/3.0.2.11 CFNetwork/711.2.23 Darwin/14.0.0';

  // Select region-based URL and host
  baseUrl   := DEXCOM_BASE_URLS[extra = 'usa'];
  FBaseHost := DEXCOM_BASE_HOSTS[extra = 'usa'];

  // Store credentials
  FUserName := user;
  FPassword := pass;
  FCalcDiff := ACalcDiff;

  // Invoke the parent constructor to set up native, timezone, etc.
  inherited Create(user, pass, extra);
end;

{-------------------------------------------------------------------------------
  Dexcom.Connect
  --------------
  Main method to:
   1) Authenticate and obtain a session token.
   2) Validate the session.
   3) Perform a final "login" call to confirm the session.
   4) Retrieve server time for offset calculations.
-------------------------------------------------------------------------------}
function Dexcom.Connect: boolean;
var
  LBody, LResponse, LTimeResponse, LTimeString: string;
  LServerDateTime: TDateTime;
begin
  // Prepare JSON payload for Dexcom Share authentication
  LBody := Format(
    '{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
    [FUserName, FPassword, DEXCOM_APPLICATION_ID]
    );

  // 1) First step: authenticate to get a preliminary session ID
  FSessionID := StringReplace(
    native.Request(true, DEXCOM_AUTHENTICATE_ENDPOINT, [], LBody),
    '"', '', [rfReplaceAll]
    );

  // Check if the response indicates a password/credential error
  if Pos('AccountPassword', FSessionID) > 0 then
  begin
    Result  := false;
    lastErr := sErrDexPass + ' (Dex1)';
    Exit;
  end;

  // 2) Validate the session ID so far
  if not CheckSession then
  begin
    Result  := false;
    lastErr := sErrDexLogin + ' (Dex2)';
    Exit;
  end;

  // 3) Call LoginPublisherAccountByName to finalize the session
  FSessionID := StringReplace(
    native.Request(true, DEXCOM_LOGIN_ENDPOINT, [], LBody),
    '"', '', [rfReplaceAll]
    );

  // Validate once again
  if not CheckSession then
  begin
    Result  := false;
    lastErr := sErrDexPostLogin + ' (Dex3)';
    Exit;
  end;

  // 4) Retrieve system UTC time from Dexcom to sync offset
  LTimeResponse := native.Request(false, DEXCOM_TIME_ENDPOINT, [], '');

  // Dexcom may return XML-style or JSON-style date/time
  if Pos('>', LTimeResponse) > 0 then
  begin
    // Possibly an XML-like response: <SystemTime>YYYY-MM-DDTHH:mm:ss</SystemTime>
    LTimeString := ExtractDelimited(5, LTimeResponse, ['>', '<']);
    if LTimeString <> '' then
      LServerDateTime := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', Copy(LTimeString, 1, 19));
  end
  else
  begin
    // Possibly JSON with /Date(1234567890123)/ format
    LTimeString := ExtractDelimited(2, LTimeResponse, ['(', ')']);
    if LTimeString <> '' then
      LServerDateTime := JSToDateTime(StrToInt64(LTimeString), false);
  end;

  // If no valid time data is found, store an error
  if LTimeString = '' then
  begin
    lastErr := 'Cannot parse Dexcom time/zone data';
    Result  := false;
    Exit;
  end;

  // Calculate the difference between server time and local time
  timeDiff := SecondsBetween(LServerDateTime, LocalTimeToUniversal(Now));
  if timeDiff < 0 then
    timeDiff := 0;

  // Store negative offset if desired
  timeDiff := -1 * timeDiff;

  // If we reach here, we have a valid session
  Result := true;
end;

{-------------------------------------------------------------------------------
  Dexcom.CheckSession
  -------------------
  Helper to verify the Dexcom session ID is valid (not empty and not a dummy ID).
-------------------------------------------------------------------------------}
function Dexcom.CheckSession: boolean;
begin
  Result :=
    (FSessionID <> '') and
    (FSessionID <> '00000000-0000-0000-0000-000000000000');
end;

{-------------------------------------------------------------------------------
  Dexcom.CheckSerialNumber
  ------------------------
  Example function (not used often) that checks whether a given Dexcom
  receiver/transmitter serial number is assigned to this account.
-------------------------------------------------------------------------------}
function Dexcom.CheckSerialNumber(const ASerial: string): boolean;
var
  LParams: array[1..2] of string;
  LResponse: string;
begin
  Result := false;

  LParams[1] := 'sessionId='   + encodeStr(FSessionID);
  LParams[2] := 'serialNumber=' + encodeStr(ASerial);

  // Dexcom returns 'AssignedToYou' if valid
  LResponse := native.Request(true, DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT, LParams, '');
  if LResponse = 'AssignedToYou' then
    Result := true;
end;

{-------------------------------------------------------------------------------
  Dexcom.GetReadings
  ------------------
  Retrieves glucose readings from Dexcom for the past AMinutes minutes,
  returning up to AMaxCount results. If FCalcDiff is True, calculates
  the difference between consecutive readings.
-------------------------------------------------------------------------------}
function Dexcom.GetReadings(AMinutes, AMaxCount: integer; extras: string = ''): BGResults;

  // Helper to convert Dexcom /Date(ms)/ timestamps to TDateTime
function DexTimeToTDateTime(const S: string): TDateTime;
  var
    LMsString: string;
    LMs: int64;
  begin
    // Example format: /Date(1610464324000)/
    LMsString := Copy(S, 6, Length(S) - 6);   // Removes '/Date('
    LMsString := StringReplace(LMsString, ')', '', []); // Removes trailing ')'
    LMs       := StrToInt64(LMsString);
    // Convert from epoch-based milliseconds to TDateTime
    Result := UnixToDateTime(LMs div 1000, false);
  end;

var
  LParams:  array[1..3] of string;
  LGlucoseJSON, LAlertJSON, LTrendStr: string;
  LData:    TJSONData;
  i, LTrendCode: integer;
  LTrendEnum: BGTrend;
begin
  // Validate input ranges
  if (AMinutes < 1) or (AMinutes > 1440) then
    raise Exception.Create('GetReadings error: AMinutes out of valid range (1..1440)');

  if (AMaxCount < 1) or (AMaxCount > 288) then
    raise Exception.Create('GetReadings error: AMaxCount out of valid range (1..288)');

  // Build query parameters
  LParams[1] := 'sessionId=' + FSessionID;
  LParams[2] := 'minutes='   + IntToStr(AMinutes);
  LParams[3] := 'maxCount='  + IntToStr(AMaxCount);

  // Fetch latest glucose values
  LGlucoseJSON := native.Request(true, DEXCOM_GLUCOSE_READINGS_ENDPOINT, LParams, '');
  // (Optional) fetch alert settings, though often not used or returned
  LAlertJSON   := native.Request(true, DEXCOM_ALERT_ENDPOINT, LParams, '');

  // If no readings, return empty
  if LGlucoseJSON = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // Parse JSON array of readings
  LData := GetJSON(LGlucoseJSON);
  SetLength(Result, LData.Count);

  // Iterate through each reading item
  for i := 0 to LData.Count - 1 do
  try
      // Initialize reading record with mg/dL
    Result[i].Init(mgdl);

      // If the user wants differences (delta) and we have a previous reading
    if (FCalcDiff) and (i > 0) then
      Result[i].Update(
        LData.Items[i].FindPath('Value').AsFloat,
        LData.Items[i].FindPath('Value').AsFloat -
        LData.Items[i - 1].FindPath('Value').AsFloat
        )
    else
    if FCalcDiff then
      Result[i].Update(LData.Items[i].FindPath('Value').AsFloat, 0)// First item: no previous => delta = 0

    else
      Result[i].Update(LData.Items[i].FindPath('Value').AsFloat, BG_NO_VAL)// Delta not requested => store only BG value
    ;

      // Interpret "Trend" field (could be numeric or string)
    LTrendStr := LData.Items[i].FindPath('Trend').AsString;

      // If Trend is an integer code, you could interpret that here
    if not TryStrToInt(LTrendStr, LTrendCode) then
    begin
        // If Trend is textual, match it to the BGTrend enum
      for LTrendEnum in BGTrend do
      begin
        if BG_TRENDS_STRING[LTrendEnum] = LTrendStr then
        begin
          Result[i].trend := LTrendEnum;
          Break;
        end;
          // If no match was found, default to a placeholder
        Result[i].trend := TdPlaceholder;
      end;
    end
    else
        // If numeric, you might want to map LTrendCode -> BGTrend.
      Result[i].trend := TdPlaceholder;

      // Convert the Dexcom server time
    Result[i].date := DexTimeToTDateTime(LData.Items[i].FindPath('ST').AsString);

      // Determine the level classification (Normal, High, Low, etc.)
    Result[i].level := getLevel(Result[i].val);

  except
    on E: Exception do
    begin
        // If a JSON field is missing or invalid, clear that reading
      Result[i].Clear;
    end;
  end;

  // Example of setting CGM thresholds. You can adjust these or parse from Dexcom.
  cgmHi      := 160;  // typical "high" threshold
  cgmLo      :=  60;  // typical "low" threshold
  cgmRangeHi := 159;  // upper bound of the "in-range" window
  cgmRangeLo :=  61;  // lower bound of the "in-range" window
end;

end.
