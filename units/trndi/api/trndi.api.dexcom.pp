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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-08-16: Readings are dated from the WT field, falling back to DT and
 *   then ST, rather than from ST alone -- the receiver's own system clock,
 *   which drifts. The trend and session-failure changes this driver inherits
 *   are recorded in trndi.api.dexcom_helpers. testConnection now fails on a
 *   recognised Dexcom authentication error or a null-GUID account id, instead
 *   of reporting a locked-out or unmatched account as a working connection,
 *   and reports why rather than leaving the generic unknown-error text. Connect
 *   and testConnection apply that same check to the plain-username login, which
 *   is the only path an account without an e-mail address takes.
 * - 2026-08-16: Uses trndi.funcs.core (UI-free helper split) instead of
 *   trndi.funcs, and dropped the unused Dialogs import so the unit compiles in
 *   LCL-free (console) builds.
 * - 2026-09-23: testConnection rejects a null-GUID session instead of
 *   reporting it as a working connection, matching Connect.
 * - 2026-10-16: Sign-in moved to Login and takes the two-step route
 *   (AuthenticatePublisherAccount, then LoginPublisherAccountById) for every
 *   account type, with LoginPublisherAccountByName as the fallback; the
 *   authenticate reply may now be an {"accountId"} object, which the old
 *   quote-stripping mangled. testConnection runs Connect on a throwaway
 *   instance instead of a second copy of the flow, and after a password
 *   rejection asks the other regions whether they accept the account.
 *   GetReadings no longer fetches ReadSubscriberAlertSettings on every poll
 *   (the reply was discarded), clamps the window instead of raising, drops
 *   duplicate readings, and computes deltas after the dedupe.
 *)
unit trndi.api.dexcom;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils,   // Trndi units
trndi.types, trndi.api, {$ifdef debug} trndi.log, trndi.funcs.core,{$endif}
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
  FPasswordRejected: boolean; /// Last Login ended in Dexcom's AccountPasswordInvalid

    {** Check that the current session token looks valid (non-empty and not dummy). }
  function CheckSession: boolean;

    {** Sign in and store the session id in @code(FSessionID). Sets
        @code(lastErr) and returns False on failure. }
  function Login: boolean;

    {** Check whether a Dexcom receiver/transmitter serial is assigned to the account. }
  function CheckSerialNumber(const ASerial: string): boolean;
public
    {** Create a Dexcom API client.
        Matches the parent signature. Defaults to calculating deltas.

        @param(user   Dexcom username)
        @param(pass   Dexcom password)
        @param(region  Region selector: use 'usa' for US servers; otherwise WORLD)
    }

  constructor Create(user, pass: string); override; overload;

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
    out res: string; noCache: boolean): BGResults; override;
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
    {** True when the last sign-in failed with Dexcom's AccountPasswordInvalid,
        which is also the answer for an account on another region's server. }
  property PasswordRejected: boolean read FPasswordRejected;
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
  constructor Create(AUser, APass: string); override; overload;
  constructor Create(const AUser, APass: string; ACalcDiff: boolean); reintroduce; overload;
  class function testConnection(username, pass: string; var res: string): MaybeBool; override; overload;
end;

DexcomWorld = class(Dexcom)
protected
  function getSystemName: string; reintroduce; override;
public
  constructor Create(AUser, APass: string); override; overload;
  constructor Create(const AUser, APass: string; ACalcDiff: boolean); reintroduce; overload;
  class function testConnection(username, pass: string; var res: string): MaybeBool; override; overload;
end;

DexcomCustom = class(Dexcom);

implementation

uses trndi.api.dexcom_helpers;

resourcestring
sErrDexLogin = 'Login error: Could not establish a valid session';
sErrDexNoAccount = 'No Dexcom account matched those details. Check the ' +
  'username or e-mail address.';
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
  result := 'Dexcom (Outside USA)';
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
constructor DexcomUSA.Create(AUser, APass: string);
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

constructor DexcomWorld.Create(AUser, APass: string);
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
  Sign in and store the session id in FSessionID.

  Every account type takes the same route: AuthenticatePublisherAccount
  resolves the username, e-mail address or phone number to an account id, and
  LoginPublisherAccountById turns that into a session. It is the only route
  the Share clients kept current use (pydexcom, xDrip+, Loop since 2023,
  nightscout-connect). The one-step LoginPublisherAccountByName, which plain
  usernames took here before, has been seen to answer with no session at all;
  it stays as the fallback for a server that does not answer the two-step
  route.

  Credential failures are terminal on every step: a retry would only resend
  the same rejected password and count toward Dexcom's lockout.
------------------------------------------------------------------------------}
function Dexcom.Login: boolean;
var
  LBody, LResponse, LAccountID, LAuthErr: string;
begin
  Result := false;
  FSessionID := '';
  FPasswordRejected := false;

  LBody := Format('{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
    [JSONEscape(FUserName), JSONEscape(FPassword), DEXCOM_APPLICATION_ID]);

  // 1) Username / e-mail / phone number -> account id
  // Note: native.Request automatically adds Content-Type and Accept headers when jsondata is provided
  LResponse := native.Request(true, DEXCOM_AUTHENTICATE_ENDPOINT, [], LBody);
  {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_AUTHENTICATE_ENDPOINT, LResponse, '']));{$endif}
  if DexcomAuthFailureMessage(LResponse, LAuthErr) then
  begin
    FPasswordRejected := DexcomIsPasswordRejected(LResponse);
    setUserError(LAuthErr + ' (Dex1)');
    Exit;
  end;
  // The reply is a quoted GUID, or an {"accountId"} object for newer accounts;
  // DexcomExtractId reads both and yields '' for anything else.
  LAccountID := DexcomExtractId(LResponse);
  // A null GUID means the lookup ran but matched no account. Rejected here
  // rather than sent on to the login call, which would fail with a vaguer
  // error one step later.
  if LAccountID = DEXCOM_NULL_UUID then
  begin
    setUserError(sErrDexNoAccount + ' (Dex1a)');
    Exit;
  end;

  // 2) Account id -> session
  if LAccountID <> '' then
  begin
    LBody := Format('{ "accountId": "%s", "password": "%s", "applicationId": "%s" }',
      [LAccountID, JSONEscape(FPassword), DEXCOM_APPLICATION_ID]);
    LResponse := native.Request(true, DEXCOM_LOGIN_BY_ID_ENDPOINT, [], LBody);
    {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_LOGIN_BY_ID_ENDPOINT, LResponse, '']));{$endif}
    if DexcomAuthFailureMessage(LResponse, LAuthErr) then
    begin
      FPasswordRejected := DexcomIsPasswordRejected(LResponse);
      setUserError(LAuthErr + ' (Dex1b)');
      Exit;
    end;
    FSessionID := DexcomExtractId(LResponse);
  end;

  // 3) Fallback: one-step login by account name
  if FSessionID = '' then
  begin
    LBody := Format('{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
      [JSONEscape(FUserName), JSONEscape(FPassword), DEXCOM_APPLICATION_ID]);
    LResponse := native.Request(true, DEXCOM_LOGIN_BY_NAME_ENDPOINT, [], LBody);
    {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_LOGIN_BY_NAME_ENDPOINT, LResponse, '']));{$endif}
    if DexcomAuthFailureMessage(LResponse, LAuthErr) then
    begin
      FPasswordRejected := DexcomIsPasswordRejected(LResponse);
      setUserError(LAuthErr + ' (Dex1c)');
      Exit;
    end;
    FSessionID := DexcomExtractId(LResponse);
    if FSessionID = '' then
    begin
      lastErr := sErrDexLogin + ' (Dex1c): ' + Copy(Trim(LResponse), 1, 200);
      Exit;
    end;
  end;

  // 4) Validate the session token
  if not CheckSession then
  begin
    // Null GUID indicates authentication rejection (not wrong password)
    setUserError('Dexcom authentication rejected. Possible causes: ' +
      '1) Wrong region (try using another region), ' +
      '2) Dexcom Share not enabled in official app, ' +
      '3) Account requires action in official Dexcom app. (Dex2)');
    Exit;
  end;

  Result := true;
end;

{------------------------------------------------------------------------------
  Connect to Dexcom Share and establish a valid session.
  Signs in (see Login) and synchronizes time.
------------------------------------------------------------------------------}
function Dexcom.Connect: boolean;
var
  LTimeResponse, LTimeString: string;
  LServerDateTime: TDateTime;
  LTimeMs: int64;
begin
  Result := false;
  if not Login then
    Exit;

  // 3) Retrieve system UTC time for time-diff calibration
  LTimeResponse := native.Request(false, DEXCOM_TIME_ENDPOINT, [], '', 'Accept=application/json');
  {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_TIME_ENDPOINT, LTimeResponse, '']));{$endif}

  // Dexcom may respond as XML-like <SystemTime> or JSON-ish /Date(ms)/ format
  if Pos('>', LTimeResponse) > 0 then
  begin
    // Example: <SystemTime>YYYY-MM-DDTHH:mm:ss</SystemTime>
    LTimeString := ExtractDelimited(5, LTimeResponse, ['>', '<']);
    if LTimeString <> '' then
    try
      // Parse the ISO-like timestamp (truncate to 19 chars to ignore fractions/timezone)
      LServerDateTime := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', Copy(LTimeString, 1, 19));
    except
      // Malformed payload (e.g. an error page); take the parse-error path below
      LTimeString := '';
    end;
  end
  else
  begin
    // Example: {"ServerTime":"/Date(1610464324000)/"} or similar payload
    LTimeString := ExtractDelimited(2, LTimeResponse, ['(', ')']);
    // LTimeString is ms since epoch, UTC. UnixToDateTime(..., True) keeps it as
    // a UTC-valued TDateTime to match the XML branch above (ScanDateTime parses
    // the server's UTC digits verbatim, with no local conversion) -- both feed
    // the signed timeDiff computation below, which expects UTC on both sides.
    // JSToDateTime(..., False) would convert to *local* time instead, which
    // silently baked the machine's UTC offset into timeDiff on this branch.
    if TryStrToInt64(LTimeString, LTimeMs) then
      LServerDateTime := UnixToDateTime(LTimeMs div 1000, True)
    else
      LTimeString := '';
  end;

  // If we failed to parse any time value, abort with error
  if LTimeString = '' then
  begin
    lastErr := 'Cannot parse Dexcom time/zone data';
    Result := false;
    Exit;
  end;

  // Compute time difference between server UTC and local UTC (signed: positive
  // when the server clock is ahead of local). SecondsBetween returns an
  // unsigned magnitude, which used to be forced negative regardless of which
  // clock was actually ahead; a plain signed difference matches the timeDiff
  // convention every other backend uses (e.g. NightScout.Connect).
  timeDiff := Round((LServerDateTime - LocalTimeToUniversal(Now)) * 86400);

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
  {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT, LResponse, debugParams(LParams)]));{$endif}
  if LResponse = 'AssignedToYou' then
    Result := true;
end;

{------------------------------------------------------------------------------
  Retrieve glucose readings for the last AMinutes minutes.
  Returns up to AMaxCount samples, optionally computing deltas.
------------------------------------------------------------------------------}
function Dexcom.GetReadings(AMinutes, AMaxCount: integer; extras: string;
out res: string; {%H-}noCache: boolean): BGResults;
// Dexcom Share fetches readings via a POST to ReadPublishedSamples, which is
// not subject to HTTP GET caching. noCache is accepted for interface
// compatibility but has no effect on this backend.


  // Helper: safely extract numeric 'Value' from a JSON item (handles null/missing)
function SafeValue(Item: TJSONData; out Ok: boolean): double;
  var
    VData: TJSONData;
  begin
    Ok := false;
    Result := 0;
    if (Item = nil) or (Item.JSONType <> jtObject) then
      Exit;
    VData := TJSONObject(Item).Find('Value');
    if VData = nil then
      Exit;
    try
      Result := VData.AsFloat;
      Ok := true;
    except
      Ok := false;
    end;
  end;

  // Helper: safely extract a string field from a JSON object item (handles nil/missing)
function SafeString(Item: TJSONData; const FieldName: string): string;
var
  J: TJSONData;
begin
  Result := '';
  if (Item = nil) or (Item.JSONType <> jtObject) then
    Exit;
  J := TJSONObject(Item).Find(FieldName);
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
  LGlucoseJSON, LTrendStr: string;
  LData: TJSONData;
  i: integer;
  noval: MaybeInt;
  CurVal: double;
  CurOk: boolean;
  attempt: integer;
begin
  // Initialize the noval
  noval.exists := false;
  // The window comes from user-editable settings; shrink an oversized one to
  // what Share accepts instead of refusing the poll.
  DexcomClampWindow(AMinutes, AMaxCount);

  // Build query parameters for Dexcom Share
  LParams[1] := 'sessionId=' + FSessionID;
  LParams[2] := 'minutes=' + IntToStr(AMinutes);
  LParams[3] := 'maxCount=' + IntToStr(AMaxCount);

  for attempt := 0 to 1 do
  begin
    LGlucoseJSON := native.Request(true, DEXCOM_GLUCOSE_READINGS_ENDPOINT, LParams, '', 'Accept=application/json');
    {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, DEXCOM_GLUCOSE_READINGS_ENDPOINT, LGlucoseJSON, debugParams(lparams)]));{$endif}

    res := LGlucoseJSON;

    if (attempt = 0) and DexcomLooksLikeSessionFailure(LGlucoseJSON) then
      if Connect then
      begin
        LParams[1] := 'sessionId=' + FSessionID;
        Continue;
      end;

    Break;
  end;

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

  // Readings must be a JSON array. Anything else — typically an error object
  // like {"Code":"SessionIdNotFound","Message":...} — must not fall through to
  // the loop below: an object's Count is its member count, so it would be
  // mapped into cleared placeholder readings (val BG_NO_VAL, date 0) that
  // downstream code renders as data from 1899.
  if LData.JSONType <> jtArray then
  begin
    lastErr := Trim('Dexcom error: ' + Trim(SafeString(LData, 'Code') + ' ' +
      SafeString(LData, 'Message')));
    if lastErr = 'Dexcom error:' then
      lastErr := 'Unexpected Dexcom response: ' + Copy(LGlucoseJSON, 1, 200);
    LData.Free;
    SetLength(Result, 0);
    Exit;
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
      Result[i].trend := MapDexcomTrendToEnum(LTrendStr);

      // Date the reading from WT, then DT, then ST. Each candidate has to parse
      // to be used, so a malformed field steps aside for the next one; the
      // helper leaves date 0 when none of the three is usable.
      if not DexcomReadingTime(SafeString(LData.Items[i], 'WT'),
        SafeString(LData.Items[i], 'DT'),
        SafeString(LData.Items[i], 'ST'), Result[i].date) then
        Result[i].date := 0;

      if CurOk then
      begin
        // Deltas are filled in below, once duplicates are gone; a delta taken
        // here against a duplicate neighbour would be 0.
        Result[i].update(CurVal, BGPrimary);

        // Classify level per thresholds
        Result[i].level := getLevel(Result[i].val);
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

  // G7 Direct-to-Watch publishes each sample from both the watch and the
  // phone, a second apart, and Share returns both.
  DexcomDropDuplicateReadings(Result);
  // Dexcom Share returns readings newest-first, so the reading preceding
  // each one in time is the next index. A reading whose neighbour is missing
  // keeps change = BG_NO_VAL (set by Init), so umain falls back to computing
  // the delta from bgs[0] - bgs[1] instead of formatting a bogus 0.0.
  if FCalcDiff then
    DexcomComputeDeltas(Result);

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
  - Runs Connect on a throwaway instance: the same sign-in and time probe the
    live backend performs, so the test cannot pass what Connect then refuses
  - After a password rejection, asks the other regions whether they accept
    the account, since the wrong region is reported as a wrong password
 ------------------------------------------------------------------------------}
class function Dexcom.testConnection(user, pass: string; var res: string; extra: string): MaybeBool;
var
  inst: Dexcom;
  region: string;
begin
  Result := MaybeBool.false; // default failure
  res := 'An unknown error occured'; // @see DexcomNew
  // Basic parameter checks
  if Trim(user) = '' then
    Exit;

  inst := DexcomCustom.Create(user, pass, extra);
  try
    if inst.Connect then
    begin
      Result := MaybeBool.true;
      Exit;
    end;
    res := inst.errormsg;
    if inst.PasswordRejected and
      DexcomFindAccountRegion(user, pass, inst.Remote, region) then
      res := DexcomWrongRegionMessage(region);
  finally
    inst.Free;
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
