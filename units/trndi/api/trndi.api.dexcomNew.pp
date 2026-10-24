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
 *   liability for any damage, injury, or harm, direct or incidental, arising from
 *   its use.
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
 *   which drifts. A null-GUID account id is rejected at the authenticate step
 *   instead of being passed on to the login call. Recognised Dexcom
 *   authentication errors (AccountPasswordInvalid,
 *   SSO_AuthenticateMaxAttemptsExceeded, SSO_InternalError) now produce their
 *   own messages, and the debug payload census records the timestamp field
 *   actually used. DEXCOM_NULL_UUID moved to trndi.api.dexcom_helpers so both
 *   Share drivers share one definition. testConnection applies the auth-error
 *   check to the plain-username login as well, not only the e-mail one.
 * - 2026-08-16: Uses trndi.funcs.core (UI-free helper split) instead of
 *   trndi.funcs, and dropped the unused Dialogs import so the unit compiles in
 *   LCL-free (console) builds.
 * - 2026-09-23: testConnection rejects a null-GUID session instead of
 *   reporting it as a working connection, matching Connect.
 * - 2026-10-16: Sign-in moved to Login and takes the two-step route
 *   (AuthenticatePublisherAccount, then LoginPublisherAccountById) for every
 *   account type, with LoginPublisherAccountByName as the fallback; reply ids
 *   are read by the shared DexcomExtractId, so TryGetTokenOrError is gone.
 *   testConnection runs Connect on a throwaway instance instead of a second
 *   copy of the flow, and after a password rejection asks the other regions
 *   whether they accept the account. GetReadings clamps the window instead
 *   of raising, drops duplicate readings, and computes deltas after the
 *   dedupe.
 *)
unit trndi.api.dexcomNew;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, // Trndi units
trndi.types, trndi.api, trndi.native, trndi.native.base,
{$ifdef debug} trndi.log, trndi.funcs.core,{$endif}
// FPC units
fpjson, jsonparser, dateutils, StrUtils;

(*******************************************************************************
  Dexcom Share API endpoint definitions (New Driver)
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
  {** Dexcom Share application ID for Japan. }
DEXCOM_APPLICATION_ID_JAPAN = 'd8665ade-9673-4e27-9ff6-92db4ce13d13';

  {** Base URL (US region) for Dexcom Share services. }
DEXCOM_BASE_URL_US = 'https://share2.dexcom.com/ShareWebServices/Services';
  {** Base URL (Worldwide) for Dexcom Share services. }
DEXCOM_BASE_URL_WORLD = 'https://shareous1.dexcom.com/ShareWebServices/Services';
  {** Base URL (Japan) for Dexcom Share services. }
DEXCOM_BASE_URL_JAPAN = 'https://share.dexcom.jp/ShareWebServices/Services';

  {** Host (US region) for Dexcom Share. }
DEXCOM_HOST_US = 'share2.dexcom.com';
  {** Host (Worldwide) for Dexcom Share. }
DEXCOM_HOST_WORLD = 'shareous1.dexcom.com';
  {** Host (Japan) for Dexcom Share. }
DEXCOM_HOST_JAPAN = 'share.dexcom.jp';

type
  {** Enum for Dexcom regions to allow easy extension. }
TDexcomRegion = (drWorld, drUS, drJapan);

const
  {** Helper array mapping region enum to base URL. }
DEXCOM_BASE_URLS: array[TDexcomRegion] of string =
  (DEXCOM_BASE_URL_WORLD, DEXCOM_BASE_URL_US, DEXCOM_BASE_URL_JAPAN);

  {** Helper array mapping region enum to base host. }
DEXCOM_BASE_HOSTS: array[TDexcomRegion] of string =
  (DEXCOM_HOST_WORLD, DEXCOM_HOST_US, DEXCOM_HOST_JAPAN);

  {** Helper array mapping region enum to application ID. }
DEXCOM_APPLICATION_IDS: array[TDexcomRegion] of string =
  (DEXCOM_APPLICATION_ID, DEXCOM_APPLICATION_ID, DEXCOM_APPLICATION_ID_JAPAN);

type
  (*******************************************************************************
    DexcomNew class (abstract base)

    Inherits from @code(TrndiAPI) and implements connectivity and reading
    retrieval for Dexcom Share. Responsible for session handling, time-sync,
    and mapping Share responses into @code(BGResults).

    Concrete region-specific subclasses:
    - @code(DexcomUSANew)    -> US Share endpoints
    - @code(DexcomWorldNew)  -> Worldwide Share endpoints
    - @code(DexcomJapanNew)  -> Japan Share endpoints
    - @code(DexcomCustomNew) -> Explicit URL/region overrides (tests)
   *******************************************************************************)
DexcomNew = class abstract(TrndiAPI)
private
  FBaseHost: string;   /// The chosen Dexcom host (USA or Worldwide)
  FUserName: string;   /// Dexcom Share account username
  FPassword: string;   /// Dexcom Share account password
  FSessionID: string;   /// Session ID returned by Dexcom after authentication
  FCalcDiff: boolean;  /// If True, compute deltas between consecutive readings
  FRegion: TDexcomRegion; /// The selected region enum
  FPasswordRejected: boolean; /// Last Login ended in Dexcom's AccountPasswordInvalid

    {** Check that the current session token looks valid (non-empty and not dummy). }
  function CheckSession: boolean;

    {** Sign in and store the session id in @code(FSessionID). Sets
        @code(lastErr) and returns False on failure. }
  function Login: boolean;

    {** POST a JSON payload and hand back the body. Returns False on a
        transport failure, with the reason in @code(ATransportErr). }
  function PostJSON(const AEndpoint, APayload: string;
    out ABody, ATransportErr: string): boolean;

    {** Check whether a Dexcom receiver/transmitter serial is assigned to the account. }
  function CheckSerialNumber(const ASerial: string): boolean;
public
    {** Create a DexcomNew API client.
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
  class function testConnection(user, pass: string; var res: string; extra: string): maybebool; overload;

published
    {** The effective base URL used for API requests. }
  property Remote: string read baseUrl;
    {** Dexcom username for this client instance. }
  property User: string read FUserName;
    {** Current Dexcom session token (when connected). }
  property Session: string read FSessionID;
    {** True when the last sign-in failed with Dexcom's AccountPasswordInvalid,
        which is also the answer for an account on another region's server. }
  property PasswordRejected: boolean read FPasswordRejected;
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
    Region-specific concrete DexcomNew implementations
   *******************************************************************************)
DexcomUSANew = class(DexcomNew)
protected
  function getSystemName: string; reintroduce; override;
public
  constructor Create(AUser, APass: string); override; overload;
  constructor Create(const AUser, APass: string; ACalcDiff: boolean); reintroduce; overload;
  class function testConnection(username, pass: string; var res: string): MaybeBool; override; overload;
end;

DexcomWorldNew = class(DexcomNew)
protected
  function getSystemName: string; reintroduce; override;
public
  constructor Create(AUser, APass: string); override; overload;
  constructor Create(const AUser, APass: string; ACalcDiff: boolean); reintroduce; overload;
  class function testConnection(username, pass: string; var res: string): MaybeBool; override; overload;
end;

DexcomJapanNew = class(DexcomNew)
protected
  function getSystemName: string; reintroduce; override;
public
  constructor Create(AUser, APass: string); override; overload;
  constructor Create(const AUser, APass: string; ACalcDiff: boolean); reintroduce; overload;
  class function testConnection(username, pass: string; var res: string): MaybeBool; override; overload;
end;

DexcomCustomNew = class(DexcomNew);

implementation

uses trndi.api.dexcom_time, trndi.api.dexcom_helpers;

{** Free the owned Headers/Cookies streams on a THTTPResponse. Leaves the
    scalar fields (Body, Success, ErrorMessage, ...) intact so callers can
    still inspect them after releasing the lists. }
procedure FreeResponse(var AResp: THTTPResponse);
begin
  if AResp.Headers <> nil then
    FreeAndNil(AResp.Headers);
  if AResp.Cookies <> nil then
    FreeAndNil(AResp.Cookies);
end;

{$ifdef DEBUG}
(*******************************************************************************
  Payload census (debug builds only)

  Trndi decodes two Dexcom fields whose wire format has never been confirmed
  against a real account, and both are guesses that cannot be settled by
  reading the code:

  - Trend. MapDexcomTrendToEnum tries a textual match first, then a numeric
    one. The numeric branch now reads the code as 1-based, matching pydexcom's
    DEXCOM_TREND_DIRECTIONS; pydexcom reports that current Share sends the
    textual names and dropped integer support entirely, so the branch may well
    be dead. The census logs the field's JSON *type* alongside its value, which
    is what settles whether it still fires against a real account.

  - The SystemUtcTime response shape. Trndi has two different extractions of
    the XML form in the tree (trndi.api.dexcom takes the 5th '>'/'<' delimited
    token, dexcom_time takes the first), so at most one of them is right.

  Unlike the DEBUG_LOG_ALERT-gated request tracing elsewhere in the drivers,
  this is deliberately ungated and self-limiting: it fires on the first payload
  that actually carries readings and then stays quiet for the rest of the
  session. A user capturing a diagnostic should not have to find a menu item
  first, and one representative sample answers both questions.

  The dump contains that account's glucose readings. It stays on the user's own
  machine and only in a debug build, but a log being sent on for diagnosis
  should be read first. Session ids and passwords are never logged.
 ******************************************************************************)
var
  DexcomNewCensusDone: boolean = false;

function DexcomJSONTypeName(AType: TJSONType): string;
begin
  case AType of
  jtNumber:
    Result := 'number';
  jtString:
    Result := 'string';
  jtBoolean:
    Result := 'boolean';
  jtNull:
    Result := 'null';
  jtArray:
    Result := 'array';
  jtObject:
    Result := 'object';
  else
    Result := 'unknown';
  end;
end;

{** Describe one field of a reading: present or not, the JSON type it arrived
    as, and its literal value. The type is the part that matters -- see the
    banner above. }
function DexcomDescribeField(AItem: TJSONData; const AName: string): string;
var
  d: TJSONData;
begin
  if (AItem = nil) or (AItem.JSONType <> jtObject) then
    Exit(AName + '=<not an object>');
  d := TJSONObject(AItem).Find(AName);
  if d = nil then
    Exit(AName + '=<absent>');
  Result := Format('%s=%s:%s', [AName, DexcomJSONTypeName(d.JSONType), d.AsJSON]);
end;

procedure LogDexcomPayloadCensus(const ARaw: string; AData: TJSONData);
const
  MAX_RAW_CHARS = 2000;
  MAX_ENTRIES = 5;
var
  i: integer;
  item, trendData: TJSONData;
  raw, trendRaw, timeRaw, line: string;
  parsedTime: TDateTime;
begin
  if DexcomNewCensusDone then
    Exit;
  if (AData = nil) or (AData.JSONType <> jtArray) or (AData.Count = 0) then
    Exit;
  DexcomNewCensusDone := true;

  raw := ARaw;
  if Length(raw) > MAX_RAW_CHARS then
    raw := Copy(raw, 1, MAX_RAW_CHARS) + ' ...[truncated]';
  TrndiDLog(Format('DexcomNew.Census: %d entries; raw payload: %s',
    [AData.Count, raw]));

  for i := 0 to AData.Count - 1 do
  begin
    if i >= MAX_ENTRIES then
    begin
      TrndiDLog(Format('DexcomNew.Census: ...%d further entries not shown',
        [AData.Count - MAX_ENTRIES]));
      Break;
    end;

    item := AData.Items[i];
    line := Format('DexcomNew.Census: entry[%d] %s | %s | %s | %s | %s',
      [i,
       DexcomDescribeField(item, 'Value'),
       DexcomDescribeField(item, 'Trend'),
       DexcomDescribeField(item, 'ST'),
       DexcomDescribeField(item, 'WT'),
       DexcomDescribeField(item, 'DT')]);

    // What the driver actually decodes the guessed fields into, so the log
    // shows the mapping rather than only the input to it.
    trendRaw := '';
    if (item <> nil) and (item.JSONType = jtObject) then
    begin
      trendData := TJSONObject(item).Find('Trend');
      if trendData <> nil then
        trendRaw := trendData.AsString;
    end;
    line := line + Format(' -> trend=%s', [MapDexcomTrendToEnum(trendRaw).Text]);

    // Decode the same field GetReadings dates the reading by. The per-field
    // dump above is what settles the open question: whether DT's milliseconds
    // match WT's, or are shifted by the offset it carries.
    timeRaw := '';
    if (item <> nil) and (item.JSONType = jtObject) then
    begin
      timeRaw := TJSONObject(item).Get('WT', '');
      if timeRaw = '' then
        timeRaw := TJSONObject(item).Get('DT', '');
      if timeRaw = '' then
        timeRaw := TJSONObject(item).Get('ST', '');
    end;
    if ParseDexcomTime(timeRaw, parsedTime) then
      line := line + Format(' time->%s',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', parsedTime)])
    else
      line := line + ' time->unparsed';

    TrndiDLog(line);
  end;
end;
{$endif}

resourcestring
sDexNewErrPass = 'Incorrect username or password combination';
sDexNewErrLogin = 'Login error: Could not establish a valid session';
sDexNewErrNoAccount = 'No Dexcom account matched those details. Check the ' +
  'username, or whether the account belongs to another region';
sDexNewParamUserName = 'Dexcom Account Username';
sDexNewParamPassword = 'Password';
sDexNewParamRegion = 'Region ("usa", "japan"/"jp", or empty)';
sDexNewParamDesc =
  'Dexcom region selection:'#13#10''#13#10'' +
  'Choose the server based on your account region:' + LineEnding +
  '• Dexcom (USA): for accounts served by share2.dexcom.com' + LineEnding +
  '• Dexcom (Outside USA): for accounts served by shareous1.dexcom.com' + LineEnding +
  '• Dexcom (Japan): for accounts served by share.dexcom.jp' +
  LineEnding + LineEnding +
  'If you are unsure, try "Outside USA" first if you live outside the US.' +
  LineEnding + 'Your username and password are your Dexcom Account (not Share) credentials.';
sDexNewParamDescHTML =
  '<b>Dexcom</b> region selection:<br><br>'+
  'Choose the server based on your <u>account region</u>:<br>' +
  '• Dexcom (USA): for accounts <i>(served by share2.dexcom.com)</i><br>' +
  '• Dexcom (Outside USA): for accounts <i>(served by shareous1.dexcom.com)</i><br>' +
  '• Dexcom (Japan): for accounts <i>(served by share.dexcom.jp)</i>' +
  '<br><br>' +
  'If you are unsure, try <b>Outside USA</b> first, if you live outside the US.' +
  LineEnding + 'Your username and password are your Dexcom Account (not Share) credentials.';

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DexcomNew.getSystemName: string;
begin
  result := 'Dexcom';
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DexcomUSANew.getSystemName: string;
begin
  result := 'Dexcom (USA)';
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DexcomWorldNew.getSystemName: string;
begin
  result := 'Dexcom (Outside USA)';
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DexcomJapanNew.getSystemName: string;
begin
  result := 'Dexcom (Japan)';
end;

{------------------------------------------------------------------------------
  Constructor (override).
  Delegates to the overloaded constructor with ACalcDiff=True.
------------------------------------------------------------------------------}
constructor DexcomNew.Create(user, pass, region: string);
begin
  // Call overloaded constructor with default: calculate deltas
  Create(user, pass, region, true);
end;

constructor DexcomNew.Create(user, pass: string);
begin
  raise Exception.Create('Cannot be called without region!');
end;

{------------------------------------------------------------------------------
  Constructor (overload).
  Configure user-agent, region (base URL and host), credentials, and delta flag.
  Calls inherited base constructor to init common pieces (thresholds, native, tz).
------------------------------------------------------------------------------}
constructor DexcomNew.Create(user, pass, region: string; ACalcDiff: boolean);
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
    case region of
    'usa':
    begin
      FRegion := drUS;
      baseUrl := DEXCOM_BASE_URLS[drUS];
      FBaseHost := DEXCOM_BASE_HOSTS[drUS];
    end;
    'japan', 'jp':
    begin
      FRegion := drJapan;
      baseUrl := DEXCOM_BASE_URLS[drJapan];
      FBaseHost := DEXCOM_BASE_HOSTS[drJapan];
    end;
    else
    begin
      FRegion := drWorld;
      baseUrl := DEXCOM_BASE_URLS[drWorld];
      FBaseHost := DEXCOM_BASE_HOSTS[drWorld];
    end;
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
constructor DexcomUSANew.Create(AUser, APass: string);
begin
  inherited Create(AUser, APass, 'usa');
end;

constructor DexcomUSANew.Create(const AUser, APass: string; ACalcDiff: boolean);
begin
  inherited Create(AUser, APass, 'usa', ACalcDiff);
end;

class function DexcomUSANew.testConnection(username, pass: string; var res: string): MaybeBool;
begin
  result := inherited testConnection(username, pass, res, 'usa');
end;

constructor DexcomWorldNew.Create(AUser, APass: string);
begin
  inherited Create(AUser, APass, 'world');
end;

constructor DexcomWorldNew.Create(const AUser, APass: string; ACalcDiff: boolean);
begin
  inherited Create(AUser, APass, 'world', ACalcDiff);
end;

class function DexcomWorldNew.testConnection(username, pass: string; var res: string): MaybeBool;
begin
  result := inherited testConnection(username, pass, res, 'world');
end;

constructor DexcomJapanNew.Create(AUser, APass: string);
begin
  inherited Create(AUser, APass, 'japan');
end;

constructor DexcomJapanNew.Create(const AUser, APass: string; ACalcDiff: boolean);
begin
  inherited Create(AUser, APass, 'japan', ACalcDiff);
end;

class function DexcomJapanNew.testConnection(username, pass: string; var res: string): MaybeBool;
begin
  result := inherited testConnection(username, pass, res, 'japan');
end;

{------------------------------------------------------------------------------
  POST a JSON payload through the structured request path, so a transport
  failure is reported as one instead of being read as a token.
------------------------------------------------------------------------------}
function DexcomNew.PostJSON(const AEndpoint, APayload: string;
  out ABody, ATransportErr: string): boolean;
var
  LResp: THTTPResponse;
begin
  LResp := native.RequestExWait(true, AEndpoint, [], APayload, nil, true, 10, nil, true);
  Result := LResp.Success;
  ABody := LResp.Body;
  ATransportErr := LResp.ErrorMessage;
  FreeResponse(LResp);
  // Fallback for environments where requestEx may return a successful
  // status with an empty body.
  if Result and (Trim(ABody) = '') then
    ABody := native.request(true, AEndpoint, [], APayload, 'Accept=application/json', true);
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
  the same rejected password and count toward Dexcom's lockout. Dexcom
  reports them as HTTP 500 with a Code/Message JSON body, so the transport
  completes and the codes have to be recognized in the payload.
------------------------------------------------------------------------------}
function DexcomNew.Login: boolean;
var
  LBody, LResponse, LAccountID, LAuthErr, LTransportErr: string;
begin
  Result := false;
  FSessionID := '';
  FPasswordRejected := false;

  LBody := Format('{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
    [JSONEscape(FUserName), JSONEscape(FPassword), DEXCOM_APPLICATION_IDS[FRegion]]);

  // 1) Username / e-mail / phone number -> account id. A transport failure
  //    here leaves the by-name fallback to report its own.
  LAccountID := '';
  if PostJSON(DEXCOM_AUTHENTICATE_ENDPOINT, LBody, LResponse, LTransportErr) then
  begin
    if DexcomAuthFailureMessage(LResponse, LAuthErr) then
    begin
      FPasswordRejected := DexcomIsPasswordRejected(LResponse);
      setUserError(LAuthErr + ' (Dex1a)');
      Exit;
    end;
    // The reply is a quoted GUID, or an {"accountId"} object for newer
    // accounts; DexcomExtractId reads both and yields '' for anything else.
    LAccountID := DexcomExtractId(LResponse);
    // A null-GUID account id means the endpoint answered but matched no
    // account. Caught here rather than passed to LoginPublisherAccountById,
    // which would fail with a vaguer session error one step later; pydexcom
    // rejects it at the same point (ACCOUNT_ID_DEFAULT).
    if LAccountID = DEXCOM_NULL_UUID then
    begin
      setUserError(sDexNewErrNoAccount + ' (Dex1a)');
      Exit;
    end;
  end;

  // 2) Account id -> session
  if LAccountID <> '' then
  begin
    LBody := Format('{ "accountId": "%s", "password": "%s", "applicationId": "%s" }',
      [LAccountID, JSONEscape(FPassword), DEXCOM_APPLICATION_IDS[FRegion]]);
    if PostJSON(DEXCOM_LOGIN_BY_ID_ENDPOINT, LBody, LResponse, LTransportErr) then
    begin
      if DexcomAuthFailureMessage(LResponse, LAuthErr) then
      begin
        FPasswordRejected := DexcomIsPasswordRejected(LResponse);
        setUserError(LAuthErr + ' (Dex1b)');
        Exit;
      end;
      FSessionID := DexcomExtractId(LResponse);
    end;
  end;

  // 3) Fallback: one-step login by account name
  if FSessionID = '' then
  begin
    LBody := Format('{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
      [JSONEscape(FUserName), JSONEscape(FPassword), DEXCOM_APPLICATION_IDS[FRegion]]);
    if not PostJSON(DEXCOM_LOGIN_BY_NAME_ENDPOINT, LBody, LResponse, LTransportErr) then
    begin
      lastErr := sDexNewErrLogin + ' (Dex1c): ' + LTransportErr;
      Exit;
    end;
    if DexcomAuthFailureMessage(LResponse, LAuthErr) then
    begin
      FPasswordRejected := DexcomIsPasswordRejected(LResponse);
      setUserError(LAuthErr + ' (Dex1c)');
      Exit;
    end;
    FSessionID := DexcomExtractId(LResponse);
    if FSessionID = '' then
    begin
      lastErr := sDexNewErrLogin + ' (Dex1c): ' + Copy(Trim(LResponse), 1, 200);
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
function DexcomNew.Connect: boolean;
var
  LTimeResponse: string;
  LServerDateTime: TDateTime;
  LHTTPRespTime: THTTPResponse;
  hdrs: TStringList;
begin
  Result := false;
  if not Login then
    Exit;

  // 3) Retrieve system UTC time for time-diff calibration
  hdrs := TStringList.Create;
  LHTTPRespTime := Default(THTTPResponse);
  try
    hdrs.Add('Accept: application/json');
    LHTTPRespTime := native.RequestExWait(false, DEXCOM_TIME_ENDPOINT, [], '', nil, true, 10, hdrs, true);
    LTimeResponse := LHTTPRespTime.Body;
    FreeResponse(LHTTPRespTime);
    // Verbatim, because the two XML extractions in the tree disagree about
    // which delimited token holds the timestamp -- see the census banner.
    {$ifdef DEBUG}
    TrndiDLog('DexcomNew.Connect: SystemUtcTime raw response: ' +
      Copy(LTimeResponse, 1, 500));
    {$endif}
    if not LHTTPRespTime.Success then
    begin
      lastErr := 'Cannot fetch Dexcom time: ' + LHTTPRespTime.ErrorMessage;
      Result := false;
      Exit;
    end;
    // Parse server time using centralized helper (handles XML, /Date(ms)/, JSON ServerTime, ISO).
    // AsUTC=True: this feeds the signed timeDiff computation below, which
    // compares against LocalTimeToUniversal(Now) and needs a UTC-valued result
    // -- unlike the reading-timestamp callers of this helper, which want local.
    if not ParseDexcomTime(LTimeResponse, LServerDateTime, true) then
    begin
      lastErr := 'Cannot parse Dexcom time/zone data';
      Result := false;
      Exit;
    end;
    {$ifdef DEBUG}
    TrndiDLog(Format('DexcomNew.Connect: SystemUtcTime parsed (UTC)=%s localUTC=%s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', LServerDateTime),
       FormatDateTime('yyyy-mm-dd hh:nn:ss', LocalTimeToUniversal(Now))]));
    {$endif}
  finally
    FreeResponse(LHTTPRespTime);
    hdrs.Free;
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
function DexcomNew.CheckSession: boolean;
begin
  Result :=
    (FSessionID <> '') and (FSessionID <> DEXCOM_NULL_UUID);
end;

{------------------------------------------------------------------------------
  Check if a Dexcom receiver/transmitter serial is assigned to this account.
  Returns True if the server reports 'AssignedToYou'.
------------------------------------------------------------------------------}
function DexcomNew.CheckSerialNumber(const ASerial: string): boolean;
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
  if LResponse = 'AssignedToYou' then
    Result := true;
end;

{------------------------------------------------------------------------------
  Retrieve glucose readings for the last AMinutes minutes.
  Returns up to AMaxCount samples, optionally computing deltas.
------------------------------------------------------------------------------}
function DexcomNew.GetReadings(AMinutes, AMaxCount: integer; extras: string;
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
  i, LTrendCode: integer;
  LTrendEnum: BGTrend;
  noval: MaybeInt;
  CurVal: double;
  CurOk: boolean;
  LTimeProbe, LAlertProbe: string;
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
    res := LGlucoseJSON;

    if (attempt = 0) and DexcomLooksLikeSessionFailure(LGlucoseJSON) then
      if Connect then
      begin
        LParams[1] := 'sessionId=' + FSessionID;
        Continue;
      end;

    Break;
  end;

  // Defensive: ensure we received JSON (Dexcom sometimes returns HTML/redirects on auth errors)
  if Trim(LGlucoseJSON) = '' then
  begin
    // Collect diagnostic responses to help debug email-authenticated accounts
    LTimeProbe := native.Request(false, DEXCOM_TIME_ENDPOINT, ['sessionId=' + FSessionID], '', 'Accept=application/json');
    LAlertProbe := native.Request(true, DEXCOM_ALERT_ENDPOINT, LParams, '', 'Accept=application/json');
    SetLength(Result, 0);
    lastErr := 'Empty payload from Dexcom glucose endpoint.' + LineEnding +
      'GlucoseResp: ' + Copy(LGlucoseJSON, 1, 400) + LineEnding +
      'TimeProbe: ' + Copy(LTimeProbe, 1, 400) + LineEnding +
      'AlertProbe: ' + Copy(LAlertProbe, 1, 400);
    Exit;
  end;
  // Expect an array; if not JSON array/object, abort with diagnostic
  if not (TrimLeft(LGlucoseJSON).StartsWith('[') or TrimLeft(LGlucoseJSON).StartsWith('{')) then
  begin
    SetLength(Result, 0);
    lastErr := 'Unexpected non-JSON response from Dexcom: ' + Copy(LGlucoseJSON, 1, 400);
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

  // One-shot diagnostic: records the wire format of the fields Trndi currently
  // has to guess at. Self-limiting, so this costs one log burst per session.
  {$ifdef DEBUG}
  LogDexcomPayloadCensus(LGlucoseJSON, LData);
  {$endif}

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

      // Compute BG value
      CurVal := SafeValue(LData.Items[i], CurOk);
      if CurOk then
      begin
        // Parse trend (Dexcom may return numeric code or textual string)
        LTrendStr := SafeString(LData.Items[i], 'Trend');
        // Use dedicated mapper which handles textual names and numeric codes
        Result[i].trend := MapDexcomTrendToEnum(LTrendStr);

        // Date the reading from WT, then DT, then ST. Each candidate has to
        // parse to be used, so a malformed field steps aside for the next one;
        // the helper leaves date 0 when none of the three is usable.
        if not DexcomReadingTime(SafeString(LData.Items[i], 'WT'),
          SafeString(LData.Items[i], 'DT'),
          SafeString(LData.Items[i], 'ST'), Result[i].date) then
          Result[i].date := 0;

        // Deltas are filled in below, once duplicates are gone; a delta taken
        // here against a duplicate neighbour would be 0.
        Result[i].update(CurVal, BGPrimary);

        // Classify level per thresholds
        Result[i].level := getLevel(Result[i].val);
      end
      else
        Result[i].Clear;
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
class function DexcomNew.ParamLabel(LabelName: APIParamLabel): string;
begin
  case LabelName of
  APLUser:
    Result := sDexNewParamUserName;
  APLPass:
    Result := sDexNewParamPassword;
  APLDesc:
    Result := sDexNewParamDesc;
  APLDescHTML:
    Result := sDexNewParamDescHTML;
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
class function DexcomNew.testConnection(user, pass: string; var res: string; extra: string): MaybeBool;
var
  inst: DexcomNew;
  region: string;
begin
  Result := MaybeBool.False; // default failure
  // Basic parameter checks
  if Trim(user) = '' then
    Exit;

  inst := DexcomCustomNew.Create(user, pass, extra);
  try
    if inst.Connect then
    begin
      Result := MaybeBool.True;
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

function DexcomNew.getLimitHigh: integer;
begin
  Result := 400; // Dexcom typical high limit
end;

function DexcomNew.getLimitLow: integer;
begin
  Result := 40; // Dexcom typical low limit
end;

end.
