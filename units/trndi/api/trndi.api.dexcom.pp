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
unit trndi.api.dexcom;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs,
  // Trndi units
trndi.types, trndi.api, trndi.native, trndi.funcs,
  // FPC units
fpjson, jsonparser, dateutils, StrUtils;

(*******************************************************************************
  Dexcom Share API endpoint definitions
 ******************************************************************************)

const
  {** Dexcom Share authenticate endpoint (returns account ID from username/email/phone). }
DEXCOM_AUTHENTICATE_ENDPOINT = 'General/AuthenticatePublisherAccount';
  {** Dexcom Share login by account ID endpoint (returns session ID from account ID). }
DEXCOM_LOGIN_BY_ID_ENDPOINT = 'General/LoginPublisherAccountById';
  {** Dexcom Share login by account name endpoint (returns session ID from username only). }
DEXCOM_LOGIN_BY_NAME_ENDPOINT = 'General/LoginPublisherAccountByName';
  {** Verify receiver/transmitter serial assignment status. }
DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT =
  'Publisher/CheckMonitoredReceiverAssignmentStatus';
  {** Retrieve system UTC time from Dexcom Share. }
DEXCOM_TIME_ENDPOINT = 'General/SystemUtcTime';
  {** Fetch latest glucose values. }
DEXCOM_GLUCOSE_READINGS_ENDPOINT = 'Publisher/ReadPublisherLatestGlucoseValues';
  {** Fetch alert settings (may not always be returned). }
DEXCOM_ALERT_ENDPOINT = 'Publisher/ReadSubscriberAlertSettings';

  {** Dexcom Share application ID used by mobile apps (commonly reused). }
DEXCOM_APPLICATION_ID = 'd89443d2-327c-4a6f-89e5-496bbb0317db';

  {** Base URL (US region) for Dexcom Share services. }
DEXCOM_BASE_URL_US = 'https://share2.dexcom.com/ShareWebServices/Services';
  {** Base URL (Worldwide) for Dexcom Share services. }
DEXCOM_BASE_URL_WORLD = 'https://shareous1.dexcom.com/ShareWebServices/Services';

  {** Host (US region) for Dexcom Share. }
DEXCOM_HOST_US = 'share2.dexcom.com';
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
    Dexcom class (abstract base)

    Inherits from @code(TrndiAPI) and implements connectivity and reading
    retrieval for Dexcom Share. Responsible for session handling, time-sync,
    and mapping Share responses into @code(BGResults).

    Concrete region-specific subclasses:
    - @code(DexcomUSA)    -> US Share endpoints
    - @code(DexcomWorld)  -> Worldwide Share endpoints
    - @code(DexcomCustom) -> Explicit URL/region overrides (tests)
   ******************************************************************************)
Dexcom = class abstract(TrndiAPI)
private
  FBaseHost: string;   /// The chosen Dexcom host (USA or Worldwide)
  FUserName: string;   /// Dexcom Share account username
  FPassword: string;   /// Dexcom Share account password
  FSessionID: string;   /// Session ID returned by Dexcom after authentication
  FCalcDiff: boolean;  /// If True, compute deltas between consecutive readings

    {** Check that the current session token looks valid (non-empty and not dummy). }
  function CheckSession: boolean;

    {** Check whether a Dexcom receiver/transmitter serial is assigned to the account. }
  function CheckSerialNumber(const ASerial: string): boolean;
public
    {** Create a Dexcom API client.
        Matches the parent signature. Defaults to calculating deltas.

        @param(user   Dexcom username)
        @param(pass   Dexcom password)
        @param(region  Region selector: use 'usa' for US servers; otherwise WORLD)
    }

  constructor Create(user, pass: string); overload;

  constructor Create(user, pass, region: string); overload;

    {** Overloaded constructor that allows explicitly enabling/disabling delta calc.

        @param(user     Dexcom username)
        @param(pass     Dexcom password)
        @param(region    Region selector: use 'usa' for US servers; otherwise WORLD)
        @param(ACalcDiff Whether to compute deltas between consecutive readings)
     }
  constructor Create(user, pass, region: string; ACalcDiff: boolean); overload;

    {** Authenticate with Dexcom Share, establish a session, and synchronize time.

        Workflow:
        1) Login using LoginPublisherAccountByName to get session token.
        2) Validate session.
        3) Retrieve server UTC time and compute @code(timeDiff).

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
  function GetReadings(AMinutes, AMaxCount: integer; extras: string;
    out res: string): BGResults; override;
    {** UI parameter label provider (override).
        1: Dexcom Username
        2: Dexcom Password
        3: Region (use "usa" for US servers)
     }
  class function ParamLabel(LabelName: APIParamLabel): string; override;
  {** Test connection for Dexcom Share API
      - Performs authentication sequence similar to Connect to validate credentials
      - Probes /General/SystemUtcTime to ensure server responds with time info
  }
  class function testConnection(user, pass: string; var res: string; extra: string): MaybeBool; overload;

published
    {** The effective base URL used for API requests. }
  property Remote: string read baseUrl;
    {** Dexcom username for this client instance. }
  property User: string read FUserName;
    {** Current Dexcom session token (when connected). }
  property Session: string read FSessionID;
    {** Whether this client computes deltas between consecutive readings. }
  property CalculateDiff: boolean read FCalcDiff;
protected
    {** Get the value which represents the maximum reading for the backend
     }
  function getLimitHigh: integer; override;

    {** Get the value which represents the minimum reading for the backend
     }
  function getLimitLow: integer; override;

    {** Gets the API's name }
  function getSystemName: string; override;
end;

  (*******************************************************************************
    Region-specific concrete Dexcom implementations
   ******************************************************************************)
DexcomUSA = class(Dexcom)
protected
  function getSystemName: string; reintroduce; override;
public
  constructor Create(const AUser, APass: string); reintroduce; overload;
  constructor Create(const AUser, APass: string; ACalcDiff: boolean); reintroduce; overload;
  class function testConnection(username, pass: string; var res: string): MaybeBool; overload;
end;

DexcomWorld = class(Dexcom)
protected
  function getSystemName: string; reintroduce; override;
public
  constructor Create(const AUser, APass: string); reintroduce; overload;
  constructor Create(const AUser, APass: string; ACalcDiff: boolean); reintroduce; overload;
  class function testConnection(username, pass: string; var res: string): MaybeBool; overload;
end;

DexcomCustom = class(Dexcom);

implementation

resourcestring
sErrDexPass = 'Invalid Dexcom password or account credentials';
sErrDexLogin = 'Login error: Could not establish a valid session';
sParamUserName = 'Dexcom Username';
sParamPassword = 'Dexcom Password';

sParamDesc =
  'Dexcom region selection:'#13#10''#13#10'' +
  'Choose the server based on your account region:' + LineEnding +
  '• Dexcom (USA): for accounts served by share2.dexcom.com' + LineEnding +
  '• Dexcom (Outside USA): for accounts served by shareous1.dexcom.com' +
  LineEnding + LineEnding +
  'If you are unsure, try “Outside USA” first if you live outside the US.' +
  LineEnding + 'Your username and password are your Dexcom Account (not Share) credentials.';
sParamDescHTML =
  '<b>Dexcom</b> region selection:<br><br>'+
  'Choose the server based on your <u>account region</u>:<br>' +
  '• Dexcom (USA): for accounts <i>(served by share2.dexcom.com)</i><br>' +
  '• Dexcom (Outside USA): for accounts <i>(served by shareous1.dexcom.com)</i>' +
  '<br><br>' +
  'If you are unsure, try <b>Outside USA</b> first, if you live outside the US.' +
  LineEnding + 'Your username and password are your Dexcom Account (not Share) credentials.';

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function Dexcom.getSystemName: string;
begin
  result := 'Dexcom';
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DexcomUSA.getSystemName: string;
begin
  result := 'Dexcom (USA)';
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DexcomWorld.getSystemName: string;
begin
  result := 'Dexcom (Outide USA)';
end;

{------------------------------------------------------------------------------
  Constructor (override).
  Delegates to the overloaded constructor with ACalcDiff=True.
------------------------------------------------------------------------------}
constructor Dexcom.Create(user, pass, region: string);
begin
  // Call overloaded constructor with default: calculate deltas
  Create(user, pass, region, true);
end;

constructor Dexcom.Create(user, pass: string);
begin
  raise Exception.Create('Cannot be called without region!');
end;

{------------------------------------------------------------------------------
  Constructor (overload).
  Configure user-agent, region (base URL and host), credentials, and delta flag.
  Calls inherited base constructor to init common pieces (thresholds, native, tz).
------------------------------------------------------------------------------}
constructor Dexcom.Create(user, pass, region: string; ACalcDiff: boolean);
begin
  // Common Dexcom Share user-agent string observed in official apps
  ua := 'Dexcom Share/3.0.2.11 CFNetwork/711.2.23 Darwin/14.0.0';

  // Select base URL/host.
  // - If extra is a full URL (http/https), treat it as an override. This is
  //   primarily used by integration tests that run against a local fake server.
  // - Otherwise, extra='usa' selects US endpoints; anything else uses WORLD.
  if (LeftStr(LowerCase(Trim(region)), 7) = 'http://') or
    (LeftStr(LowerCase(Trim(region)), 8) = 'https://') then
  begin
    baseUrl := Trim(region);
    FBaseHost := '';
  end
  else
  begin
    baseUrl := DEXCOM_BASE_URLS[region = 'usa'];
    FBaseHost := DEXCOM_BASE_HOSTS[region = 'usa'];
  end;

  // Store credentials and preferences
  FUserName := user;
  FPassword := pass;
  FCalcDiff := ACalcDiff;

  // Parent ctor sets timezone, allocates native helper, and initializes thresholds
  inherited Create(user, pass);
end;

{------------------------------------------------------------------------------
  Concrete constructors for region-specific subclasses
------------------------------------------------------------------------------}
constructor DexcomUSA.Create(const AUser, APass: string);
begin
  inherited Create(AUser, APass, 'usa');
end;

constructor DexcomUSA.Create(const AUser, APass: string; ACalcDiff: boolean);
begin
  inherited Create(AUser, APass, 'usa', ACalcDiff);
end;

class function DexcomUSA.testConnection(username, pass: string; var res: string): MaybeBool;
begin
  result := inherited testConnection(username, pass, res, 'usa');
end;

constructor DexcomWorld.Create(const AUser, APass: string);
begin
  inherited Create(AUser, APass, 'world');
end;

constructor DexcomWorld.Create(const AUser, APass: string; ACalcDiff: boolean);
begin
  inherited Create(AUser, APass, 'world', ACalcDiff);
end;

class function DexcomWorld.testConnection(username, pass: string; var res: string): MaybeBool;
begin
  result := inherited testConnection(username, pass, res, 'world');
end;

{------------------------------------------------------------------------------
  Connect to Dexcom Share and establish a valid session.
  Performs authentication sequence and time synchronization.
------------------------------------------------------------------------------}
function Dexcom.Connect: boolean;

  // Helper: Escape a string for safe inclusion in JSON
function JSONEscape(const S: string): string;
  var
    i: integer;
    c: char;
  begin
    Result := '';
    for i := 1 to Length(S) do
    begin
      c := S[i];
      case c of
      '"':
        Result := Result + '\"';
      '\':
        Result := Result + '\\';
      #8:
        Result := Result + '\b';   // backspace
      #9:
        Result := Result + '\t';   // tab
      #10:
        Result := Result + '\n';   // newline
      #12:
        Result := Result + '\f';   // form feed
      #13:
        Result := Result + '\r';   // carriage return
      else
        Result := Result + c;
      end;
    end;
  end;

var
  LBody, LResponse, LTimeResponse, LTimeString, LAccountID: string;
  LServerDateTime: TDateTime;
  LUseEmailAuth: boolean;
  resp: string;
begin
  // Detect if user provided email (contains @) or phone (starts with +)
  // These require two-step auth: AuthenticatePublisherAccount → LoginPublisherAccountById
  LUseEmailAuth := (Pos('@', FUserName) > 0) or (Pos('+', FUserName) = 1);

  // Prepare JSON payload for authentication with properly escaped credentials
  LBody := Format('{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
    [JSONEscape(FUserName), JSONEscape(FPassword), DEXCOM_APPLICATION_ID]);

  // 1) Authenticate to obtain session token
  // Note: native.Request automatically adds Content-Type and Accept headers when jsondata is provided
  
  if LUseEmailAuth then
  begin
    // Two-step authentication for email/phone:
    // Step 1: Get account ID from email/phone
    resp := native.Request(true, DEXCOM_AUTHENTICATE_ENDPOINT, [], LBody);
    LAccountID := StringReplace(
      resp,
      '"', '', [rfReplaceAll]);
    {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_AUTHENTICATE_ENDPOINT, resp, '']));{$endif}
    
    // Check for authentication errors
    if (LAccountID = '') or (Pos('error', LowerCase(LAccountID)) > 0) or
      (Pos('invalid', LowerCase(LAccountID)) > 0) or
      (Pos('AccountPassword', LAccountID) > 0) then
    begin
      Result := false;
      if Pos('AccountPassword', LAccountID) > 0 then
        lastErr := sErrDexPass + ' (Dex1)'
      else
        lastErr := sErrDexLogin + ' (Dex1a): ' + LAccountID;
      Exit;
    end;
    
    // Step 2: Use account ID to get session ID
    LBody := Format('{ "accountId": "%s", "password": "%s", "applicationId": "%s" }',
      [LAccountID, JSONEscape(FPassword), DEXCOM_APPLICATION_ID]);

    resp := native.Request(true, DEXCOM_LOGIN_BY_ID_ENDPOINT, [], LBody);
    FSessionID := StringReplace(
      resp,
      '"', '', [rfReplaceAll]);
        {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_LOGIN_BY_ID_ENDPOINT, resp, '']));{$endif}
  end
  else begin
    resp := native.Request(true, DEXCOM_LOGIN_BY_NAME_ENDPOINT, [], LBody);
    FSessionID := StringReplace(
    resp,
      '"', '', [rfReplaceAll]);// Single-step authentication for plain usernames

    {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_LOGIN_BY_NAME_ENDPOINT, resp, '']));{$endif}
  end;

  // Check for various error responses before validation
  if (FSessionID = '') or (Pos('error', LowerCase(FSessionID)) > 0) or
    (Pos('invalid', LowerCase(FSessionID)) > 0) then
  begin
    Result := false;
    lastErr := sErrDexLogin + ' (Dex1a): ' + FSessionID;
    Exit;
  end;

  // If response indicates password/credential issues, fail early
  if Pos('AccountPassword', FSessionID) > 0 then
  begin
    Result := false;
    lastErr := sErrDexPass + ' (Dex1)';
    Exit;
  end;

  // 2) Validate current session token
  if not CheckSession then
  begin
    Result := false;
    // Null GUID indicates authentication rejection (not wrong password)
    if FSessionID = '00000000-0000-0000-0000-000000000000' then
      lastErr := 'Dexcom authentication rejected. Possible causes: ' +
        '1) Wrong region (try using another region), ' +
        '2) Dexcom Share not enabled in official app, ' +
        '3) Account requires action in official Dexcom app. (Dex2)'
    else
      lastErr := sErrDexLogin + ' (Dex2): Received invalid session ID: ' + FSessionID;
    Exit;
  end;

  // 3) Retrieve system UTC time for time-diff calibration
  LTimeResponse := native.Request(false, DEXCOM_TIME_ENDPOINT, [], '', 'Accept=application/json');
  {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_TIME_ENDPOINT, LTimeResponse, '']));{$endif}

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
    Result := false;
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

{------------------------------------------------------------------------------
  Validate the current session token for obvious invalid values.
------------------------------------------------------------------------------}
function Dexcom.CheckSession: boolean;
begin
  Result :=
    (FSessionID <> '') and (FSessionID <> '00000000-0000-0000-0000-000000000000');
end;

{------------------------------------------------------------------------------
  Check if a Dexcom receiver/transmitter serial is assigned to this account.
  Returns True if the server reports 'AssignedToYou'.
------------------------------------------------------------------------------}
function Dexcom.CheckSerialNumber(const ASerial: string): boolean;
var
  LParams: array[1..2] of string;
  LResponse: string;
begin
  Result := false;

  // Encode parameters to be safe in URLs
  LParams[1] := 'sessionId=' + encodeStr(FSessionID);
  LParams[2] := 'serialNumber=' + encodeStr(ASerial);

  // Dexcom returns 'AssignedToYou' when serial number is associated
  LResponse := native.Request(true, DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT, LParams, '', 'Accept=application/json');
  {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT, LResponse, debugParams(LParams)]));{$endif}
  if LResponse = 'AssignedToYou' then
    Result := true;
end;

{------------------------------------------------------------------------------
  Retrieve glucose readings for the last AMinutes minutes.
  Returns up to AMaxCount samples, optionally computing deltas.
------------------------------------------------------------------------------}
function Dexcom.GetReadings(AMinutes, AMaxCount: integer; extras: string;
out res: string): BGResults;

  // Helper: convert Dexcom /Date(ms)/ string to TDateTime
function DexTimeToTDateTime(const S: string): TDateTime;
  var
    LMsString: string;
    LMs: int64;
  begin
    // Example: /Date(1610464324000)/
    LMsString := Copy(S, 6, Length(S) - 6);               // drop '/Date('
    LMsString := StringReplace(LMsString, ')', '', []);   // drop ')'
    LMs := StrToInt64(LMsString);
    Result := UnixToDateTime(LMs div 1000, false);     // milliseconds -> seconds
  end;

  // Helper: safely extract numeric 'Value' from a JSON item (handles null/missing)
function SafeValue(Item: TJSONData; out Ok: boolean): double;
  var
    VData: TJSONData;
  begin
    Ok := false;
    Result := 0;
    if Item = nil then
      Exit;
    VData := Item.FindPath('Value');
    if VData = nil then
      Exit;
    try
      Result := VData.AsFloat;
      Ok := true;
    except
      Ok := false;
    end;
  end;

  // Helper: safely extract string at given path from a JSON item (handles nil/missing)
function SafeString(Item: TJSONData; const Path: string): string;
var
  J: TJSONData;
begin
  Result := '';
  if Item = nil then
    Exit;
  J := Item.FindPath(Path);
  if J = nil then
    Exit;
  try
    Result := J.AsString;
  except
    Result := '';
  end;
end;

var
  LParams: array[1..3] of string;
  LGlucoseJSON, LAlertJSON, LTrendStr, LSTStr: string;
  LData: TJSONData;
  i, LTrendCode: integer;
  LTrendEnum: BGTrend;
  noval: MaybeInt;
  CurVal, PrevVal: double;
  CurOk, PrevOk: boolean;
begin
  // Initialize the noval
  noval.exists := false;
  PrevVal := BG_NO_VAL;
  // Guard input ranges based on common Share limits
  if (AMinutes < 1) or (AMinutes > 1440) then
    raise Exception.Create('GetReadings error: AMinutes out of valid range (1..1440)');
  if (AMaxCount < 1) or (AMaxCount > 288) then
    raise Exception.Create('GetReadings error: AMaxCount out of valid range (1..288)');

  // Build query parameters for Dexcom Share
  LParams[1] := 'sessionId=' + FSessionID;
  LParams[2] := 'minutes=' + IntToStr(AMinutes);
  LParams[3] := 'maxCount=' + IntToStr(AMaxCount);

  // Fetch glucose values; some deployments also allow reading alert settings
  LGlucoseJSON := native.Request(true, DEXCOM_GLUCOSE_READINGS_ENDPOINT, LParams, '', 'Accept=application/json');
  {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_GLUCOSE_READINGS_ENDPOINT, LGlucoseJSON, debugParams(lparams)]));{$endif}
  LAlertJSON := native.Request(true, DEXCOM_ALERT_ENDPOINT, LParams, '', 'Accept=application/json');
  {$ifdef DEBUG} if debug_log_api then LogMessageToFile(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_ALERT_ENDPOINT, LAlertJSON, debugParams(LPARAMS)]));{$endif}

  res := LGlucoseJSON;

  // Return empty result on empty payload
  if LGlucoseJSON = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // Parse JSON array with error handling for malformed responses
  try
    LData := GetJSON(LGlucoseJSON);
  except
    on E: Exception do
    begin
      // Handle JSONDecodeError - return empty array if JSON parsing fails
      SetLength(Result, 0);
      Exit;
    end;
  end;

  try
    // Set result length to match JSON count
    SetLength(Result, LData.Count);
    // Iterate items and map to BGReading
    for i := 0 to LData.Count - 1 do
    try
      // Initialize reading (mg/dL units)
      Result[i].Init(mgdl, self.systemName);
      // Mark source for downstream consumers/debugging
      Result[i].updateEnv('Dexcom', noval, noval);

      // Compute BG value and optional delta, handling missing/null 'Value' gracefully
      CurVal := SafeValue(LData.Items[i], CurOk);

      // Parse trend and date for all entries
      LTrendStr := SafeString(LData.Items[i], 'Trend');
      // Map trend string to enum
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
        Result[i].trend := TdPlaceholder; // Numeric mapping not defined; keep placeholder unless you add a map

      // Convert Dexcom timestamp "/Date(ms)/" to TDateTime (safely)
      LSTStr := SafeString(LData.Items[i], 'ST');
      if LSTStr <> '' then
        Result[i].date := DexTimeToTDateTime(LSTStr)
      else
        Result[i].date := 0;

      if CurOk then
      begin
        // Compute delta as current - previous valid reading
        if FCalcDiff and (PrevVal <> BG_NO_VAL) then
          Result[i].update(CurVal, CurVal - PrevVal)
        else
          Result[i].update(CurVal, 0);

        // Classify level per thresholds
        Result[i].level := getLevel(Result[i].val);

        // Update PrevVal for next delta
        PrevVal := CurVal;
      end
      else
        Result[i].Clear// Missing value: clear the reading
      ;
    except
      on E: Exception do
      begin
        // If parsing fails for an item, clear it
        Result[i].Clear;
      end;
    end;
  finally
    LData.Free;
  end;

  // Note: Dexcom API does not provide threshold information
  // Thresholds should be set via override settings or use API defaults from Connect()
end;

{------------------------------------------------------------------------------
  Provide parameter label captions for Settings UI (Dexcom backend).
------------------------------------------------------------------------------}
class function Dexcom.ParamLabel(LabelName: APIParamLabel): string;
begin
  case LabelName of
  APLUser:
    Result := sParamUserName;
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

{------------------------------------------------------------------------------
  Test connection for Dexcom Share API
  - Performs authentication sequence similar to Connect to validate credentials
  - Probes /General/SystemUtcTime to ensure server responds with time info
 ------------------------------------------------------------------------------}
class function Dexcom.testConnection(user, pass: string; var res: string; extra: string): MaybeBool;
var
  tn: TrndiNative;
  base, body, resp, accountId, sessionId, timeResp, timeStr: string;
  js: TJSONData;
  useEmailAuth: boolean;
  LServerDateTime: TDateTime;
  i: integer;
function JSONEscape(const S: string): string;
  var
    i: integer;
    c: char;
  begin
    Result := '';
    for i := 1 to Length(S) do
    begin
      c := S[i];
      case c of
      '"':
        Result := Result + '\"';
      '\':
        Result := Result + '\\';
      #8:
        Result := Result + '\b';
      #9:
        Result := Result + '\t';
      #10:
        Result := Result + '\n';
      #12:
        Result := Result + '\f';
      #13:
        Result := Result + '\r';
      else
        Result := Result + c;
      end;
    end;
  end;
begin
  Result := maybebool.false; // default failure
  res := 'An unknown error occured'; // @see DexcomNew
  // Basic parameter checks
  if Trim(user) = '' then
    Exit;

  base := DEXCOM_BASE_URLS[extra = 'usa'];
  // Create native with same UA as client
  tn := TrndiNative.Create('Dexcom Share/3.0.2.11 CFNetwork/711.2.23 Darwin/14.0.0', base);
  try
    useEmailAuth := (Pos('@', user) > 0) or (Pos('+', user) = 1);
    body := Format('{"accountName":"%s","password":"%s","applicationId":"%s"}',
      [JSONEscape(user), JSONEscape(pass), DEXCOM_APPLICATION_ID]);

    // 1) Authenticate
    if useEmailAuth then
    begin
      accountId := StringReplace(tn.Request(true, DEXCOM_AUTHENTICATE_ENDPOINT, [], body), '"', '', [rfReplaceAll]);
      if (accountId = '') or (Pos('error', LowerCase(accountId)) > 0) or (Pos('invalid', LowerCase(accountId)) > 0) then
        Exit;

      body := Format('{"accountId":"%s","password":"%s","applicationId":"%s"}',
        [accountId, JSONEscape(pass), DEXCOM_APPLICATION_ID]);

      sessionId := StringReplace(tn.Request(true, DEXCOM_LOGIN_BY_ID_ENDPOINT, [], body), '"', '', [rfReplaceAll]);
    end
    else
      sessionId := StringReplace(tn.Request(true, DEXCOM_LOGIN_BY_NAME_ENDPOINT, [], body), '"', '', [rfReplaceAll]);

    // 2) Basic checks on session token
    if (sessionId = '') or (Pos('error', LowerCase(sessionId)) > 0) or (Pos('invalid', LowerCase(sessionId)) > 0) or (Pos('AccountPassword', sessionId) > 0) then
      Exit;

    // 3) Time probe
    timeResp := tn.Request(false, DEXCOM_TIME_ENDPOINT, [], '', 'Accept=application/json');
    if Trim(timeResp) = '' then
      Exit;

    // Parse time response: try '<SystemTime>...' or '/Date(ms)/' or JSON object
    if Pos('>', timeResp) > 0 then
    begin
      // Example: <SystemTime>YYYY-MM-DDTHH:mm:ss</SystemTime>
      timeStr := ExtractDelimited(2, timeResp, ['>', '<']);
      if timeStr = '' then
        Exit;
      try
        LServerDateTime := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', Copy(timeStr, 1, 19));
      except
        Exit;
      end;
    end
    else
    begin
      // JSON or /Date(ms)/ format. Find digits between '(' and ')'
      timeStr := '';
      i := Pos('(', timeResp);
      if i > 0 then
        timeStr := Copy(timeResp, i + 1, Pos(')', timeResp) - i - 1)
      else
      try
        js := GetJSON(timeResp);
        try
          if js.JSONType = jtObject then
            timeStr := TJSONObject(js).Get('ServerTime', '');
        finally
          js.Free;
        end;
      except
        on E: Exception do
          // Handle JSONDecodeError - malformed JSON from time endpoint
          timeStr := '';
      end// Try a simple JSON parse for numeric ServerTime value
      ;

      if timeStr = '' then
        Exit;
      try
        LServerDateTime := UnixToDateTime(StrToInt64(timeStr) div 1000, false);
      except
        Exit;
      end;
    end;

    // If all above succeeded, we consider this a success
    Result := MaybeBool.true;
  finally
    tn.Free;
  end;
end;

function Dexcom.getLimitHigh: integer;
begin
  Result := 400; // Dexcom typical high limit
end;

function Dexcom.getLimitLow: integer;
begin
  Result := 40; // Dexcom typical low limit
end;

end.
