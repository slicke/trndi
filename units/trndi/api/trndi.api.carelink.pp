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
unit trndi.api.carelink;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // Trndi units
  trndi.types, trndi.api, trndi.native.base, trndi.funcs.core,
  // FPC units
  fpjson, jsonparser, dateutils, base64;

(*******************************************************************************
  Medtronic CareLink (follower) API

  Mirrors carelink-python-client v1.4 (https://github.com/ondrej1024/carelink-python-client)
  and the CareLink Connect app it emulates. Medtronic gates the interactive
  login behind reCAPTCHA, so this backend never sees a password: the stored
  credential is the token data captured once in a browser (see
  guides/CareLink.md), and the refresh token keeps the session alive from
  then on.

  All hosts are resolved at runtime from Medtronic's discovery endpoint —
  a single URL serving every region. From its response we take:
  - baseUrlCumulus:  data API (display/message)
  - baseUrlCareLink: user/patient API (users/me, links/patients)
  - the SSO configuration, which yields the OAuth2 token-refresh endpoint
  The credential blob's token_url / data_url keys override discovery.
 ******************************************************************************)

const
  {** Endpoint discovery; one URL for all regions }
  CARELINK_DISCOVERY_URL = 'https://clcloud.minimed.eu/connect/carepartner/v13/discover/android/3.6';

  {** Refresh this many minutes before the access token's JWT exp claim
      (the reference client refreshes below 600 s remaining) }
  CARELINK_TOKEN_REFRESH_MARGIN_MIN = 10;

  {** The CareLink cloud expects the Connect app's Android UA }
  CARELINK_USER_AGENT = 'Dalvik/2.1.0 (Linux; U; Android 10; Nexus 5X Build/QQ3A.200805.001)';

  {** Node.js login helper shipped with Trndi (see tools/carelink-login), path
      relative to the executable directory. Used by TrndiAPI.webLoginScript so
      the settings UI can run the browser login and capture the token itself. }
  CARELINK_LOGIN_SCRIPT = 'tools/carelink-login/carelink-login.mjs';

type
  {** Region selector for CareLink servers }
  TCareLinkRegion = (crUS, crEU);

  (*******************************************************************************
    CareLink class (abstract base)

    Inherits from @code(TrndiAPI) and implements reading retrieval for the
    Medtronic CareLink follower ("care partner") API. Handles endpoint
    discovery, OAuth2 token refresh, patient selection and mapping of SG
    entries into @code(BGResults).

    Credentials: @code(user) is the CareLink account username (optional; when
    blank it is read from the access token's claims); @code(pass) is a JSON
    blob with the captured token data (access_token, refresh_token, client_id,
    and — pre-Auth0 — client_secret and mag-identifier). When the refresh
    token rotates, the updated blob is emitted through
    @code(OnCredentialsChanged) so the owner can persist it.

    Concrete region-specific subclasses:
    - @code(CareLinkUS) -> US region
    - @code(CareLinkEU) -> EU/rest-of-world region
   ******************************************************************************)
  CareLink = class abstract(TrndiAPI)
  private
    FUsername: string;      /// CareLink account username (from user field or JWT claims)
    FRawCreds: string;      /// Credential blob as passed in (JSON)
    FCreds: TJSONObject;    /// Parsed credential blob; owned by this instance
    FRegion: TCareLinkRegion; /// Server region

    FAccessToken: string;   /// OAuth2 access token (JWT)
    FRefreshToken: string;  /// OAuth2 refresh token (rotates on use)
    FClientId: string;      /// OAuth2 client id from the captured login
    FClientSecret: string;  /// OAuth2 client secret (absent on Auth0 logins)
    FMagIdentifier: string; /// Medtronic MAG device header (absent on Auth0 logins)
    FAccessTokenExpiresAt: TDateTime; /// From the access token's exp claim; 0 = unknown

    FBaseUrlCumulus: string;  /// Data API base, from discovery
    FBaseUrlCareLink: string; /// User/patient API base, from discovery
    FTokenUrl: string;        /// OAuth2 token endpoint, from discovery

    FRole: string;          /// 'carepartner' or 'patient' (from users/me)
    FPatientId: string;     /// Followed patient (care-partner accounts)

    FActiveInsulin: single;       /// Last reported IOB in units; -1 = unknown
    FActiveInsulinAt: TDateTime;  /// Timestamp of the IOB report

    FDeviceStatus: TCGMDeviceStatus; /// Sensor/pump status from the last fetch
    FDeviceStatusValid: boolean;     /// True once a payload has filled it
    FBasalRate: single;              /// Auto-basal U/hr from the last fetch; -1 unknown

    FBoluses: TBolusList;            /// Insulin deliveries from the last fetch
    FBolusesValid: boolean;          /// True once a payload has been walked

    FCarbs: TCarbList;               /// Carbohydrate entries from the last fetch
    FCarbsValid: boolean;            /// True once a payload has been walked

    {** Map the account's high/low limits onto cgmHi/cgmLo. The payload
        carries a schedule; the entry in effect now is used. }
    procedure ApplyLimits(const ARoot: TJSONObject);

    {** Set timeDiff from currentServerTime (a true UTC epoch in ms). }
    procedure ApplyServerTime(const ARoot: TJSONObject);

    {** Fill FDeviceStatus from the sensor/pump housekeeping fields. }
    procedure ApplyDeviceStatus(const ARoot: TJSONObject);

    {** Read activeInsulin (IOB) into FActiveInsulin/FActiveInsulinAt. }
    procedure ApplyActiveInsulin(const ARoot: TJSONObject);

    {** Derive the delivered basal rate in U/hr by summing the last hour of
        AUTO_BASAL_DELIVERY markers. Returns -1 when the payload carries no
        auto-basal data. }
    function ComputeAutoBasalRate(const ARoot: TJSONObject): single;

    {** Collect every insulin delivery in the markers list into FBoluses,
        oldest first. }
    procedure ExtractBoluses(const ARoot: TJSONObject);

    {** Collect carbohydrate entries into FCarbs, oldest first. Reads FBoluses,
        so it must run after ExtractBoluses. }
    procedure ExtractCarbs(const ARoot: TJSONObject);


    {** Load token data from the credential blob. Sets lastErr on failure. }
    function ParseCredentials: boolean;

    {** Read a blob key, accepting snake_case / camelCase / dashed spellings. }
    function CredValue(const AKeys: array of string): string;

    {** Decode a JWT's payload segment. Caller frees; nil on failure. }
    function DecodeJWTPayload(const AToken: string): TJSONObject;

    {** Decode the access token's exp claim into FAccessTokenExpiresAt and
        fill FUsername from its claims when the user field was left blank. }
    procedure ApplyTokenClaims;

    {** Resolve baseUrlCumulus/baseUrlCareLink/token endpoint from the
        discovery service. Blob token_url/data_url overrides win. }
    function DiscoverEndpoints: boolean;

    {** Refresh the access token; rotates the refresh token and emits the
        updated blob via credentialsChanged. }
    function RefreshAccessToken: boolean;

    {** Refresh only when the access token is missing or about to expire. }
    function EnsureFreshToken: boolean;

    {** Standard headers for authenticated cloud calls. Caller frees. }
    function BuildAuthHeaders: TStringList;

    {** GET users/me to learn whether this account is a care partner or the
        patient. Best-effort; defaults to care partner. }
    procedure FetchUserRole;

    {** For care-partner accounts without an explicit patientId: fetch the
        followed-patient list and select the first entry. Best-effort. }
    procedure SelectPatient;

    {** POST the display-message request. Refreshes the token once on HTTP
        401/403, and retries transient 5xx responses with a short backoff.
        Returns the raw response body. }
    function FetchDisplayMessage(out ABody: string): boolean;

  public
    {** Create a CareLink API client.
        @param(AUser  CareLink account username; may be blank (read from token))
        @param(ACreds Captured token data as JSON (see guides/CareLink.md))
        @param(ARegion Region selector) }
    constructor Create(AUser, ACreds: string; ARegion: TCareLinkRegion); reintroduce; overload;

    destructor Destroy; override;

    {** Validate the stored token data, discover endpoints, refresh if
        needed, resolve the followed patient and probe the data endpoint.
        @returns(True if a session is usable; otherwise False with lastErr set) }
    function Connect: boolean; override;

    {** Retrieve latest glucose readings.
        @param(AMinutes  Time window in minutes to fetch)
        @param(AMaxCount Maximum number of readings to return)
        @param(AExtras   Reserved; not used here)
        @param(ARes      Out param receiving a status message)
        @returns(Array of @code(BGReading); may be empty if none/failed) }
    function GetReadings(AMinutes, AMaxCount: integer; AExtras: string;
      out ARes: string; noCache: boolean): BGResults; override;

    {** Delivered basal rate in U/hr, from the last fetch.

        SmartGuard pumps have no fixed rate — they micro-bolus every five
        minutes — so this is the sum of the last hour of AUTO_BASAL_DELIVERY
        markers, i.e. what was actually delivered rather than a programmed
        setting. Answers from cached data; returns 0 (the base class's
        "unavailable") before the first fetch or on a payload without
        auto-basal data. }
    function getBasalRate: single; override;

    {** Sensor/pump housekeeping from the last fetch. Cached; no request. }
    function getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean; override;

    {** Insulin deliveries from the last fetch, oldest first. Cached; no
        request. Covers the window the payload's markers list covers, which is
        shorter than the reading history. }
    function getBoluses(out ABoluses: TBolusList): boolean; override;

    {** True — the payload's markers list carries insulin deliveries. }
    function supportsBoluses: boolean; override;

    {** Carbohydrate entries from the last fetch, oldest first. Cached; no
        request. Carbs reported both as a meal marker and on the bolus that
        covered them are already reconciled into one entry. }
    function getCarbs(out ACarbs: TCarbList): boolean; override;

    {** True — the payload's markers list carries carbohydrate entries. }
    function supportsCarbs: boolean; override;

    {** UI parameter label provider (override). }
    class function ParamLabel(LabelName: APIParamLabel): string; override;

    {** CareLink requires the assisted browser login helper (see
        guides/CareLink.md and tools/carelink-login). }
    class function supportsWebLogin: boolean; override;

    {** Returns the bundled Node.js login helper (tools/carelink-login). The
        base class leaves @code(args) empty (EU/rest-of-world); CareLinkUS adds
        the region flag. }
    class function webLoginScript(out args: string): string; override;

    {** Extracts the embedded login helper (carelink-login.mjs, package.json,
        package-lock.json — compiled in as Lazarus resources, see
        carelink_assets.lrs) into AFolder. }
    class function WriteAssets(const AFolder: string): boolean; override;

    {** Test connection for the CareLink follower API }
    class function testConnection(AUser, ACreds: string; var ARes: string; AExtra: string): MaybeBool; overload;

  published
    {** The effective base URL used for API requests. }
    property Remote: string read baseUrl;
    {** CareLink username for this client instance. }
    property Username: string read FUsername;
    {** Followed patient id (care-partner accounts). }
    property PatientId: string read FPatientId;
    {** Last reported active insulin (IOB) in units; -1 when unknown. }
    property ActiveInsulin: single read FActiveInsulin;
    {** When the active insulin value was reported. }
    property ActiveInsulinTime: TDateTime read FActiveInsulinAt;

  protected
    {** Resolve the object carrying the payload's fields; some versions nest
        everything under "patientData". Returns nil when the shape is wrong. }
    function PayloadRoot(AData: TJSONData): TJSONObject;

    {** Apply everything the display-message payload carries besides the
        readings: account limits, server clock, device status, IOB, basal,
        insulin deliveries and carbohydrates. Called on every successful fetch,
        so later payloads win.

        Protected rather than private so the tests can drive it from a captured
        payload: reaching it through the real endpoint would mean standing up
        the whole discovery and token-refresh flow against a fake server, and
        what is worth testing is what the payload is turned into, not how it
        was fetched. }
    procedure ApplyPayloadMetadata(const ARoot: TJSONObject);

    {** Get the value which represents the maximum reading for the backend }
    function getLimitHigh: integer; override;

    {** Get the value which represents the minimum reading for the backend }
    function getLimitLow: integer; override;

    {** Gets the API's name }
    function getSystemName: string; override;
  end;

  (*******************************************************************************
    Region-specific concrete CareLink implementations
   ******************************************************************************)
  CareLinkUS = class(CareLink)
  protected
    function getSystemName: string; override;
  public
    constructor Create(AUser, ACreds: string); override; overload;
    class function testConnection(AUser, ACreds: string; var ARes: string): MaybeBool; override; overload;
    {** Same helper as the base class, with the US region flag. }
    class function webLoginScript(out args: string): string; override;
  end;

  CareLinkEU = class(CareLink)
  protected
    function getSystemName: string; override;
  public
    constructor Create(AUser, ACreds: string); override; overload;
    class function testConnection(AUser, ACreds: string; var ARes: string): MaybeBool; override; overload;
  end;

{$IFDEF TEST}
{** Map a CareLink trend string to Trndi's arrow enum. Medtronic reports
    single/double/triple arrows; Trndi tops out at double. Unknown values map
    to @code(TdPlaceholder). Exposed for the test suite only. }
function CareLinkTrendToBG(const ATrend: string): BGTrend;

{** Parse a CareLink ISO-8601 timestamp into a local TDateTime. Accepts
    'yyyy-mm-ddThh:nn:ss', optional fractional seconds, and an optional
    'Z' / '+hh:mm' / '-hhmm' offset. Values with an offset are converted to
    local time; values without one are taken as already local. Exposed for
    the test suite only. }
function ParseCareLinkTime(const S: string; out ADate: TDateTime): boolean;
{$ENDIF}

implementation

{$ifndef X_CONSOLE}
uses
  LResources; // TLazarusResourceStream for the embedded login-helper assets
{$endif}

resourcestring
  sErrCareLinkNoCreds = 'No CareLink token data found. Paste the captured login data (JSON) as the credential — see the CareLink guide.';
  sErrCareLinkBadCreds = 'Could not read the CareLink token data. It must be the JSON captured at login (access_token/refresh_token etc).';
  sErrCareLinkRefresh = 'CareLink token refresh failed. The saved login expires while Trndi is not running — sometimes overnight — so you may need to capture a fresh login.';
  sErrCareLinkDiscover = 'Could not resolve the CareLink endpoints from Medtronic''s discovery service.';
  sCareLinkManyPatients = 'This CareLink account follows %d patients; Trndi is showing %s. Set "patientId" in the token data, or use a separate care partner account, to watch someone else.';
  sParamUserName = 'CareLink Username (may be left empty)';
  sParamPassword = 'CareLink Token Data (JSON)';
  sParamDesc =
    '** BETA DRIVER - It may not work as intended; check the CareLink app '
    + 'before acting on a reading! **'#13#10#13#10 +
    'Medtronic CareLink follower connection.'#13#10#13#10 +
    'Medtronic requires a one-time login in your web browser (with CAPTCHA); ' +
    'Trndi then keeps the session alive automatically.' + LineEnding +
    'Paste the captured token data (JSON) as the credential.' + LineEnding + LineEnding +
    'Use a Care Partner (follower) account, not the patient account.' + LineEnding +
    'See guides/CareLink.md for a step-by-step walkthrough.';
  sParamDescHTML =
    '<div style="background: #dc3545; color: white; padding: 15px; border-radius: 6px; margin-bottom: 20px; font-weight: bold; text-align: center; border: 2px solid #c82333;">' +
    '⚠️ BETA DRIVER - It may not work as intended; check the CareLink app before acting on a reading! ⚠️' +
    '</div>' +
    '<b>Medtronic CareLink</b> follower connection.<br><br>' +
    'Medtronic requires a <u>one-time login in your web browser</u> (with CAPTCHA); ' +
    'Trndi then keeps the session alive automatically.<br>' +
    'Paste the captured token data (JSON) as the credential.<br><br>' +
    'Use a <u>Care Partner</u> (follower) account, not the patient account.<br>' +
    'See the CareLink guide for a step-by-step walkthrough.';

{------------------------------------------------------------------------------
  Map a CareLink trend string to Trndi's arrow enum.
  Medtronic reports single/double/triple arrows; Trndi tops out at double.
 ------------------------------------------------------------------------------}
function CareLinkTrendToBG(const ATrend: string): BGTrend;
begin
  case UpperCase(Trim(ATrend)) of
  'NONE':
    Result := TdFlat;
  'UP':
    Result := TdSingleUp;
  'DOWN':
    Result := TdSingleDown;
  'UP_DOUBLE', 'UP_TRIPLE':
    Result := TdDoubleUp;
  'DOWN_DOUBLE', 'DOWN_TRIPLE':
    Result := TdDoubleDown;
  else
    Result := TdPlaceholder;
  end;
end;

{------------------------------------------------------------------------------
  Read a numeric field out of a marker's dataValues.

  The payload is inconsistent about this: the same kind of quantity arrives as
  a JSON number in one marker type and as a quoted string in another. Strings
  are always dot-decimal, so the caller supplies format settings fixed to '.'
  rather than letting a comma-decimal system locale mangle them.

  Returns False when the key is absent or unusable, so callers can probe
  several spellings and skip a marker they cannot read instead of recording a
  zero that would look like a real measurement.
 ------------------------------------------------------------------------------}
function CareLinkMarkerAmount(const AObj: TJSONObject; const AKey: string;
  const AFmt: TFormatSettings; out AValue: double): boolean;
var
  d: TJSONData;
begin
  Result := false;
  AValue := 0;
  if AObj = nil then
    Exit;
  d := AObj.Find(AKey);
  if d = nil then
    Exit;
  if d is TJSONNumber then
  begin
    AValue := d.AsFloat;
    Result := true;
  end
  else
  if d.JSONType = jtString then
    Result := TryStrToFloat(d.AsString, AValue, AFmt);
end;

{------------------------------------------------------------------------------
  Parse a CareLink ISO-8601 timestamp into a local TDateTime.
  Accepts 'yyyy-mm-ddThh:nn:ss', optional fractional seconds, and an optional
  'Z' / '+hh:mm' / '-hhmm' offset. Values with an offset are converted to
  local time; values without one are taken as already local.
 ------------------------------------------------------------------------------}
function ParseCareLinkTime(const S: string; out ADate: TDateTime): boolean;
var
  txt: string;
  y, mo, d, h, n, sec: integer;
  offMinutes: integer;
  hasOffset, negOffset: boolean;
  p, offH, offM: integer;
  utc: TDateTime;
begin
  Result := false;
  ADate := 0;
  txt := Trim(S);

  // Minimum: yyyy-mm-ddThh:nn:ss
  if (Length(txt) < 19) or (txt[5] <> '-') or (txt[8] <> '-') or
     (txt[14] <> ':') or (txt[17] <> ':') then
    Exit;

  if not TryStrToInt(Copy(txt, 1, 4), y) then Exit;
  if not TryStrToInt(Copy(txt, 6, 2), mo) then Exit;
  if not TryStrToInt(Copy(txt, 9, 2), d) then Exit;
  if not TryStrToInt(Copy(txt, 12, 2), h) then Exit;
  if not TryStrToInt(Copy(txt, 15, 2), n) then Exit;
  if not TryStrToInt(Copy(txt, 18, 2), sec) then Exit;

  if not TryEncodeDateTime(y, mo, d, h, n, sec, 0, ADate) then
    Exit;

  // Skip fractional seconds, then look for a zone designator
  p := 20;
  if (p <= Length(txt)) and (txt[p] = '.') then
  begin
    Inc(p);
    while (p <= Length(txt)) and (txt[p] in ['0'..'9']) do
      Inc(p);
  end;

  hasOffset := false;
  offMinutes := 0;
  if p <= Length(txt) then
  begin
    case txt[p] of
    'Z', 'z':
      hasOffset := true;
    '+', '-':
    begin
      negOffset := txt[p] = '-';
      Inc(p);
      // +hh:mm or +hhmm or +hh
      if not TryStrToInt(Copy(txt, p, 2), offH) then Exit;
      Inc(p, 2);
      if (p <= Length(txt)) and (txt[p] = ':') then
        Inc(p);
      offM := 0;
      if p + 1 <= Length(txt) then
        if not TryStrToInt(Copy(txt, p, 2), offM) then
          offM := 0;
      offMinutes := offH * 60 + offM;
      if negOffset then
        offMinutes := -offMinutes;
      hasOffset := true;
    end;
    end;
  end;

  if hasOffset then
  begin
    utc := IncMinute(ADate, -offMinutes);
    ADate := UniversalTimeToLocal(utc);
  end;

  Result := true;
end;

{------------------------------------------------------------------------------
  Free a THTTPResponse's owned lists
 ------------------------------------------------------------------------------}
procedure FreeResponse(var AResponse: THTTPResponse);
begin
  if Assigned(AResponse.Headers) then
    FreeAndNil(AResponse.Headers);
  if Assigned(AResponse.Cookies) then
    FreeAndNil(AResponse.Cookies);
end;

{------------------------------------------------------------------------------
  getSystemName
 ------------------------------------------------------------------------------}
function CareLink.getSystemName: string;
begin
  Result := 'CareLink Follower';
end;

function CareLinkUS.getSystemName: string;
begin
  Result := 'CareLink Follower (USA)';
end;

function CareLinkEU.getSystemName: string;
begin
  Result := 'CareLink Follower (EU/Other)';
end;

{------------------------------------------------------------------------------
  Credential blob access
 ------------------------------------------------------------------------------}
function CareLink.CredValue(const AKeys: array of string): string;
var
  key: string;
  data: TJSONData;
begin
  Result := '';
  if FCreds = nil then
    Exit;
  for key in AKeys do
  begin
    data := FCreds.Find(key);
    if (data <> nil) and not (data is TJSONObject) and not (data is TJSONArray) then
      Exit(data.AsString);
  end;
end;

function CareLink.ParseCredentials: boolean;
var
  jsonData: TJSONData;
begin
  Result := false;

  if Trim(FRawCreds) = '' then
  begin
    lastErr := sErrCareLinkNoCreds;
    Exit;
  end;

  try
    jsonData := GetJSON(FRawCreds);
  except
    lastErr := sErrCareLinkBadCreds;
    Exit;
  end;

  if not (jsonData is TJSONObject) then
  begin
    jsonData.Free;
    lastErr := sErrCareLinkBadCreds;
    Exit;
  end;

  FreeAndNil(FCreds);
  FCreds := TJSONObject(jsonData);

  FAccessToken := CredValue(['access_token', 'accessToken']);
  FRefreshToken := CredValue(['refresh_token', 'refreshToken']);
  FClientId := CredValue(['client_id', 'clientId']);
  FClientSecret := CredValue(['client_secret', 'clientSecret']);
  FMagIdentifier := CredValue(['mag-identifier', 'mag_identifier', 'magIdentifier']);

  if CredValue(['role']) <> '' then
    FRole := LowerCase(CredValue(['role']));
  if CredValue(['patientId', 'patient_id', 'patient']) <> '' then
    FPatientId := CredValue(['patientId', 'patient_id', 'patient']);
  if (FUsername = '') and (CredValue(['username']) <> '') then
    FUsername := CredValue(['username']);

  if FRefreshToken = '' then
  begin
    lastErr := sErrCareLinkBadCreds;
    Exit;
  end;

  ApplyTokenClaims;
  Result := true;
end;

{------------------------------------------------------------------------------
  Decode a JWT's payload segment (no signature validation; the claims only
  steer our own behavior — refresh timing and default username).
 ------------------------------------------------------------------------------}
function CareLink.DecodeJWTPayload(const AToken: string): TJSONObject;
var
  parts: TStringArray;
  payload, decoded: string;
  jsonData: TJSONData;
begin
  Result := nil;
  if AToken = '' then
    Exit;

  try
    parts := AToken.Split('.');
    if Length(parts) <> 3 then
      Exit;

    payload := StringReplace(parts[1], '-', '+', [rfReplaceAll]);
    payload := StringReplace(payload, '_', '/', [rfReplaceAll]);
    while (Length(payload) mod 4) <> 0 do
      payload := payload + '=';

    decoded := DecodeStringBase64(payload);
    jsonData := GetJSON(decoded);
    if jsonData is TJSONObject then
      Result := TJSONObject(jsonData)
    else
      jsonData.Free;
  except
    Result := nil;
  end;
end;

{------------------------------------------------------------------------------
  Read exp (refresh scheduling) and token_details.preferred_username (default
  username) from the access token's claims.
 ------------------------------------------------------------------------------}
procedure CareLink.ApplyTokenClaims;
var
  payload, details: TJSONObject;
  expVal: int64;
begin
  FAccessTokenExpiresAt := 0;

  payload := DecodeJWTPayload(FAccessToken);
  if payload = nil then
    Exit;

  try
    expVal := payload.Get('exp', int64(0));
    if expVal > 0 then
      FAccessTokenExpiresAt := UnixToDateTime(expVal, false);

    if payload.Find('token_details') is TJSONObject then
    begin
      details := TJSONObject(payload.Find('token_details'));
      // The data request's username must match the authenticated identity,
      // so the token claim always wins over a manually entered name
      if details.Get('preferred_username', '') <> '' then
      begin
        if (FUsername <> '') and (FUsername <> details.Get('preferred_username', '')) then
          log('CareLink: entered username "' + FUsername +
            '" differs from token identity "' + details.Get('preferred_username', '') +
            '"; using the token identity');
        FUsername := details.Get('preferred_username', '');
      end;
    end;
  finally
    payload.Free;
  end;
end;

{------------------------------------------------------------------------------
  Resolve API endpoints from the discovery service:
  1. GET the discovery document (regionless URL).
  2. Pick the CP entry for our region -> baseUrlCumulus / baseUrlCareLink.
  3. GET the SSO configuration the entry points at (the UseSSOConfiguration
     field names which key holds the right URL — plain SSO or Auth0).
  4. token endpoint = https://server.hostname:port/prefix + token_endpoint_path.
 ------------------------------------------------------------------------------}
function CareLink.DiscoverEndpoints: boolean;
var
  customHeaders: TStringList;
  httpResponse: THTTPResponse;
  jsonData, ssoData: TJSONData;
  cpArr: TJSONArray;
  entry, ssoObj, serverObj, epObj: TJSONObject;
  i: integer;
  wantRegion, ssoKey, ssoUrl, ssoBase, tokenPath, prefix: string;
begin
  Result := false;

  // Blob overrides make live testing possible without recompiling. With both
  // endpoints overridden discovery is unnecessary (FetchDisplayMessage reads
  // data_url directly; users/me and links/patients degrade gracefully).
  FTokenUrl := CredValue(['token_url', 'tokenUrl']);
  if (FTokenUrl <> '') and (CredValue(['data_url', 'dataUrl']) <> '') then
    Exit(true);

  if FRegion = crUS then
    wantRegion := 'US'
  else
    wantRegion := 'EU';

  customHeaders := TStringList.Create;
  try
    customHeaders.Add('Accept: application/json');
    customHeaders.Add('User-Agent: ' + CARELINK_USER_AGENT);
    httpResponse := native.RequestExWait(false, CARELINK_DISCOVERY_URL,
      [], '', nil, true, 10, customHeaders, false);
  finally
    customHeaders.Free;
  end;

  log(Format('CareLink.DiscoverEndpoints: status=%d bytes=%d',
    [httpResponse.StatusCode, Length(httpResponse.Body)]));

  try
    if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) or
       (Trim(httpResponse.Body) = '') then
    begin
      lastErr := sErrCareLinkDiscover;
      Exit;
    end;

    try
      jsonData := GetJSON(httpResponse.Body);
    except
      lastErr := sErrCareLinkDiscover;
      Exit;
    end;

    ssoUrl := '';
    try
      cpArr := nil;
      if (jsonData is TJSONObject) and
         (TJSONObject(jsonData).Find('CP') is TJSONArray) then
        cpArr := TJSONArray(TJSONObject(jsonData).Find('CP'));

      if cpArr <> nil then
        for i := 0 to cpArr.Count - 1 do
        begin
          if not (cpArr[i] is TJSONObject) then
            Continue;
          entry := TJSONObject(cpArr[i]);
          if UpperCase(entry.Get('region', '')) <> wantRegion then
            Continue;

          FBaseUrlCumulus := entry.Get('baseUrlCumulus', '');
          FBaseUrlCareLink := entry.Get('baseUrlCareLink', '');

          // UseSSOConfiguration names the key holding the SSO config URL
          // (plain MAG SSO or Auth0, depending on migration status)
          ssoKey := entry.Get('UseSSOConfiguration', '');
          if ssoKey <> '' then
            ssoUrl := entry.Get(ssoKey, '');
          Break;
        end;
    finally
      jsonData.Free;
    end;

    if FBaseUrlCumulus = '' then
    begin
      lastErr := sErrCareLinkDiscover;
      Exit;
    end;

    log('CareLink.DiscoverEndpoints: cumulus=' + FBaseUrlCumulus +
      ' carelink=' + FBaseUrlCareLink);

    // Resolve the token endpoint from the SSO config, unless overridden
    if (FTokenUrl = '') and (ssoUrl <> '') then
    begin
      FreeResponse(httpResponse);
      customHeaders := TStringList.Create;
      try
        customHeaders.Add('Accept: application/json');
        customHeaders.Add('User-Agent: ' + CARELINK_USER_AGENT);
        httpResponse := native.RequestExWait(false, ssoUrl, [], '', nil, true, 10,
          customHeaders, false);
      finally
        customHeaders.Free;
      end;

      log(Format('CareLink.DiscoverEndpoints: sso status=%d bytes=%d',
        [httpResponse.StatusCode, Length(httpResponse.Body)]));

      if (httpResponse.StatusCode >= 200) and (httpResponse.StatusCode < 300) and
         (Trim(httpResponse.Body) <> '') then
      try
        ssoData := GetJSON(httpResponse.Body);
        try
          if ssoData is TJSONObject then
          begin
            ssoObj := TJSONObject(ssoData);
            if ssoObj.Find('server') is TJSONObject then
            begin
              serverObj := TJSONObject(ssoObj.Find('server'));
              prefix := serverObj.Get('prefix', '');
              ssoBase := 'https://' + serverObj.Get('hostname', '') + ':' +
                IntToStr(serverObj.Get('port', 443));
              if (prefix <> '') and (prefix <> '/') then
                ssoBase := ssoBase + '/' + prefix;

              // Auth0 configs expose system_endpoints at top level, the
              // older MAG SSO config nests them under "oauth"
              epObj := nil;
              if ssoObj.Find('system_endpoints') is TJSONObject then
                epObj := TJSONObject(ssoObj.Find('system_endpoints'))
              else if (ssoObj.Find('oauth') is TJSONObject) and
                (TJSONObject(ssoObj.Find('oauth')).Find('system_endpoints') is TJSONObject) then
                epObj := TJSONObject(TJSONObject(ssoObj.Find('oauth')).Find('system_endpoints'));

              if epObj <> nil then
              begin
                tokenPath := epObj.Get('token_endpoint_path', '');
                if tokenPath <> '' then
                  FTokenUrl := ssoBase + tokenPath;
              end;
            end;
          end;
        finally
          ssoData.Free;
        end;
      except
        // fall through to the error below when no token url was found
      end;
    end;

    if FTokenUrl = '' then
    begin
      lastErr := sErrCareLinkDiscover;
      Exit;
    end;

    log('CareLink.DiscoverEndpoints: token_url=' + FTokenUrl);
    baseUrl := FBaseUrlCumulus;
    Result := true;
  finally
    FreeResponse(httpResponse);
  end;
end;

{------------------------------------------------------------------------------
  CareLink credentials come from the assisted browser login helper
  (tools/carelink-login), so the settings UI offers a login button instead of a
  username/password. See guides/CareLink.md.
 ------------------------------------------------------------------------------}
class function CareLink.supportsWebLogin: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  The bundled Node.js login helper. Base class targets EU/rest-of-world (no
  region flag); CareLinkUS overrides to add --us. See tools/carelink-login.
 ------------------------------------------------------------------------------}
class function CareLink.webLoginScript(out args: string): string;
begin
  args := '';
  Result := CARELINK_LOGIN_SCRIPT;
end;

class function CareLinkUS.webLoginScript(out args: string): string;
begin
  Result := inherited webLoginScript(args);
  args := '--us';
end;

{------------------------------------------------------------------------------
  Extract one compiled-in resource (carelink_assets.lrs) to AFolder/AFileName,
  overwriting any existing copy. Raises on I/O failure so WriteAssets can fail
  as a whole rather than run the helper against a half-written folder.
 ------------------------------------------------------------------------------}
{$ifndef X_CONSOLE}
procedure ExtractCareLinkAsset(const AResName: string; AResType: PChar;
  const AFolder, AFileName: string);
var
  res: TLazarusResourceStream;
  outFile: TFileStream;
begin
  res := TLazarusResourceStream.Create(AResName, AResType);
  try
    outFile := TFileStream.Create(
      IncludeTrailingPathDelimiter(AFolder) + AFileName, fmCreate);
    try
      res.Position := 0;
      outFile.CopyFrom(res, res.Size);
    finally
      outFile.Free;
    end;
  finally
    res.Free;
  end;
end;

{------------------------------------------------------------------------------
  Write the CareLink login helper (carelink-login.mjs + package manifests,
  embedded via carelink_assets.lrs) into AFolder, overwriting so the copy
  always matches this Trndi build. See guides/CareLink.md.
 ------------------------------------------------------------------------------}
class function CareLink.WriteAssets(const AFolder: string): boolean;
begin
  if not ForceDirectories(AFolder) then
    Exit(false);

  // Callers fall back to the bundle next to the executable on a False result,
  // so a locked or read-only target must return False rather than escape as an
  // exception — that fallback is unreachable if this raises.
  try
    ExtractCareLinkAsset('carelink-login', 'MJS', AFolder, 'carelink-login.mjs');
    ExtractCareLinkAsset('package', 'JSON', AFolder, 'package.json');
    ExtractCareLinkAsset('package-lock', 'JSON', AFolder, 'package-lock.json');
    Result := true;
  except
    Result := false;
  end;
end;
{$else}
{------------------------------------------------------------------------------
  Console builds carry no Lazarus resource bundles (TLazarusResourceStream is
  LCL). Returning False sends callers to the documented fallback: the helper
  bundle shipped next to the executable.
 ------------------------------------------------------------------------------}
class function CareLink.WriteAssets(const AFolder: string): boolean;
begin
  Result := false;
end;
{$endif}

{------------------------------------------------------------------------------
  Refresh the access token. Medtronic rotates the refresh token on every use,
  so the updated blob is pushed to the owner for persistence — losing it
  invalidates the session permanently.
 ------------------------------------------------------------------------------}
function CareLink.RefreshAccessToken: boolean;
var
  params: TStringArray;
  customHeaders: TStringList;
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
  newAccess, newRefresh: string;
begin
  Result := false;

  if FRefreshToken = '' then
  begin
    lastErr := sErrCareLinkNoCreds;
    Exit;
  end;

  if (FTokenUrl = '') and not DiscoverEndpoints then
    Exit;

  SetLength(params, 3);
  params[0] := 'grant_type=refresh_token';
  params[1] := 'refresh_token=' + encodeStr(FRefreshToken);
  params[2] := 'client_id=' + encodeStr(FClientId);
  // client_secret is only present on pre-Auth0 (MAG) logins
  if FClientSecret <> '' then
  begin
    SetLength(params, 4);
    params[3] := 'client_secret=' + encodeStr(FClientSecret);
  end;

  customHeaders := TStringList.Create;
  try
    customHeaders.Add('User-Agent: ' + CARELINK_USER_AGENT);
    if FMagIdentifier <> '' then
      customHeaders.Add('mag-identifier: ' + FMagIdentifier);

    httpResponse := native.RequestExWait(true, FTokenUrl, params, '', nil, true, 10, customHeaders, false);
  finally
    customHeaders.Free;
  end;

  log(Format('CareLink.RefreshAccessToken: status=%d bytes=%d err=%s',
    [httpResponse.StatusCode, Length(httpResponse.Body), httpResponse.ErrorMessage]));

  try
    if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) or
       (Trim(httpResponse.Body) = '') then
    begin
      lastErr := sErrCareLinkRefresh + ' (HTTP ' + IntToStr(httpResponse.StatusCode) + ')';
      Exit;
    end;

    try
      jsonData := GetJSON(httpResponse.Body);
    except
      lastErr := sErrCareLinkRefresh;
      Exit;
    end;

    try
      if not (jsonData is TJSONObject) then
      begin
        lastErr := sErrCareLinkRefresh;
        Exit;
      end;

      jsonObj := TJSONObject(jsonData);
      newAccess := jsonObj.Get('access_token', '');
      newRefresh := jsonObj.Get('refresh_token', '');

      if newAccess = '' then
      begin
        lastErr := sErrCareLinkRefresh;
        Exit;
      end;

      FAccessToken := newAccess;
      if newRefresh <> '' then
        FRefreshToken := newRefresh;
      ApplyTokenClaims;

      // Write the rotated tokens back into the blob and hand it to the owner
      // for persistence; everything else in the blob is preserved as-is.
      if FCreds <> nil then
      begin
        if FCreds.Find('access_token') <> nil then
          FCreds.Strings['access_token'] := FAccessToken
        else
          FCreds.Add('access_token', FAccessToken);
        if newRefresh <> '' then
        begin
          if FCreds.Find('refresh_token') <> nil then
            FCreds.Strings['refresh_token'] := FRefreshToken
          else
            FCreds.Add('refresh_token', FRefreshToken);
        end;
        FRawCreds := FCreds.AsJSON;
        credentialsChanged(FRawCreds);
      end;

      log('CareLink.RefreshAccessToken: token refreshed, rotated=' +
        BoolToStr(newRefresh <> '', true));
      Result := true;
    finally
      jsonData.Free;
    end;
  finally
    FreeResponse(httpResponse);
  end;
end;

function CareLink.EnsureFreshToken: boolean;
begin
  // Unknown expiry (0) counts as stale so a bad/opaque token gets replaced
  // by a working one up front rather than failing on the data call.
  if (FAccessToken <> '') and (FAccessTokenExpiresAt > 0) and
     (Now < IncMinute(FAccessTokenExpiresAt, -CARELINK_TOKEN_REFRESH_MARGIN_MIN)) then
    Exit(true);

  Result := RefreshAccessToken;
end;

{------------------------------------------------------------------------------
  Common headers for authenticated cloud calls
 ------------------------------------------------------------------------------}
function CareLink.BuildAuthHeaders: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('Authorization: Bearer ' + FAccessToken);
  Result.Add('Accept: application/json');
  Result.Add('Content-Type: application/json');
  Result.Add('User-Agent: ' + CARELINK_USER_AGENT);
  if FMagIdentifier <> '' then
    Result.Add('mag-identifier: ' + FMagIdentifier);
end;

{------------------------------------------------------------------------------
  GET users/me: tells us whether this account is a care partner (needs a
  patientId in the data request) or the patient themselves.
 ------------------------------------------------------------------------------}
procedure CareLink.FetchUserRole;
var
  authHeaders: TStringList;
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  roleStr: string;
begin
  if (FBaseUrlCareLink = '') or (FRole <> '') then
    Exit;

  authHeaders := BuildAuthHeaders;
  try
    httpResponse := native.RequestExWait(false, FBaseUrlCareLink + '/users/me',
      [], '', nil, true, 10, authHeaders, false);
  finally
    authHeaders.Free;
  end;

  log(Format('CareLink.FetchUserRole: status=%d bytes=%d',
    [httpResponse.StatusCode, Length(httpResponse.Body)]));

  try
    if (httpResponse.StatusCode >= 200) and (httpResponse.StatusCode < 300) and
       (Trim(httpResponse.Body) <> '') then
    try
      jsonData := GetJSON(httpResponse.Body);
      try
        if jsonData is TJSONObject then
        begin
          roleStr := UpperCase(TJSONObject(jsonData).Get('role', ''));
          // CARE_PARTNER / CARE_PARTNER_OUS are followers; anything else is
          // treated as the patient's own account
          if Pos('CARE_PARTNER', roleStr) = 1 then
            FRole := 'carepartner'
          else if roleStr <> '' then
            FRole := 'patient';
          log('CareLink.FetchUserRole: role=' + roleStr + ' -> ' + FRole);
        end;
      finally
        jsonData.Free;
      end;
    except
      // best-effort; Connect falls back to the care-partner default
    end;
  finally
    FreeResponse(httpResponse);
  end;
end;

{------------------------------------------------------------------------------
  Resolve the followed patient for care-partner accounts. The data request
  identifies the patient by their CareLink username.
 ------------------------------------------------------------------------------}
procedure CareLink.SelectPatient;
var
  authHeaders: TStringList;
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  arr: TJSONArray;
  entry: TJSONObject;
begin
  if (FPatientId <> '') or (FRole <> 'carepartner') or (FBaseUrlCareLink = '') then
    Exit;

  authHeaders := BuildAuthHeaders;
  try
    httpResponse := native.RequestExWait(false, FBaseUrlCareLink + '/links/patients',
      [], '', nil, true, 10, authHeaders, false);
  finally
    authHeaders.Free;
  end;

  log(Format('CareLink.SelectPatient: status=%d bytes=%d',
    [httpResponse.StatusCode, Length(httpResponse.Body)]));

  try
    if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) or
       (Trim(httpResponse.Body) = '') then
      Exit;

    try
      jsonData := GetJSON(httpResponse.Body);
    except
      Exit;
    end;

    try
      if not (jsonData is TJSONArray) then
        Exit;
      arr := TJSONArray(jsonData);
      // Only reached when the blob carried no patientId (see the guard at the
      // top). Taking the first patient is deliberate rather than a stand-in
      // for a settings picker: a window shows one person at a time, so the
      // supported route for a second patient is a second care partner account
      // run as its own Trndi user, with patientId as the override for accounts
      // that already follow several. See guides/CareLink.md.
      if (arr.Count > 0) and (arr[0] is TJSONObject) then
      begin
        entry := TJSONObject(arr[0]);
        FPatientId := entry.Get('username', '');
        if FPatientId = '' then
          FPatientId := entry.Get('patientId', '');
        log('CareLink.SelectPatient: following patient=' + FPatientId +
          ' of ' + IntToStr(arr.Count));
        if arr.Count > 1 then
          notice(Format(sCareLinkManyPatients, [arr.Count, FPatientId]));
      end;
    finally
      jsonData.Free;
    end;
  finally
    FreeResponse(httpResponse);
  end;
end;

{------------------------------------------------------------------------------
  POST the display-message request.
  - On 401/403 the access token is refreshed once and the call retried.
  - On 5xx (server error) the call is retried a couple of times with a short
    backoff: Medtronic's data endpoint transiently 500s on the first contact
    with a freshly established follower session (which is why the classic
    workaround has been "restart and it works"). Retrying in-place makes the
    first fetch after setup succeed without a restart.
 ------------------------------------------------------------------------------}
function CareLink.FetchDisplayMessage(out ABody: string): boolean;
const
  MAX_ATTEMPTS = 3; /// 1 initial + up to 2 retries for transient failures
var
  payload: TJSONObject;
  payloadStr, dataUrl: string;
  attempt, httpStatus: integer;
  refreshed: boolean;
  authHeaders: TStringList;
  httpResponse: THTTPResponse;
begin
  Result := false;
  ABody := '';
  refreshed := false;

  dataUrl := CredValue(['data_url', 'dataUrl']);
  if dataUrl = '' then
    dataUrl := FBaseUrlCumulus + '/display/message';

  payload := TJSONObject.Create;
  try
    payload.Add('username', FUsername);
    if FRole = 'patient' then
      payload.Add('role', 'patient')
    else
    begin
      payload.Add('role', 'carepartner');
      if FPatientId <> '' then
        payload.Add('patientId', FPatientId);
    end;
    payloadStr := payload.AsJSON;
  finally
    payload.Free;
  end;

  log('CareLink.FetchDisplayMessage: payload=' + payloadStr);

  for attempt := 1 to MAX_ATTEMPTS do
  begin
    authHeaders := BuildAuthHeaders;
    try
      httpResponse := native.RequestExWait(true, dataUrl, [], payloadStr,
        nil, true, 10, authHeaders, false);
    finally
      authHeaders.Free;
    end;

    httpStatus := httpResponse.StatusCode;
    log(Format('CareLink.FetchDisplayMessage: attempt=%d httpStatus=%d bytes=%d',
      [attempt, httpStatus, Length(httpResponse.Body)]));

    try
      if (httpStatus >= 200) and (httpStatus < 300) and (Trim(httpResponse.Body) <> '') then
      begin
        ABody := httpResponse.Body;
        Exit(true);
      end;

      // Auth rejected: refresh the access token once, then retry immediately.
      if ((httpStatus = 401) or (httpStatus = 403)) and not refreshed then
      begin
        refreshed := true;
        log('CareLink.FetchDisplayMessage: auth rejected; refreshing token');
        if not RefreshAccessToken then
          Exit;
        Continue;
      end;

      // Transient server error (or no response): back off briefly and retry.
      if ((httpStatus >= 500) or (httpStatus = 0)) and (attempt < MAX_ATTEMPTS) then
      begin
        log(Format('CareLink.FetchDisplayMessage: transient HTTP %d; retrying', [httpStatus]));
        Sleep(400 * attempt);
        Continue;
      end;

      if Trim(httpResponse.Body) <> '' then
        log('CareLink.FetchDisplayMessage: error body prefix=' + Copy(httpResponse.Body, 1, 200));
      lastErr := 'Failed to get CareLink data: HTTP ' + IntToStr(httpStatus);
      Exit;
    finally
      FreeResponse(httpResponse);
    end;
  end;

  // Reached only by falling off the end of the loop — a token refresh on the
  // final attempt leaves no attempt to use it. Callers surface lastErr, so it
  // must not be left holding an unrelated (or empty) message.
  lastErr := Format('Failed to get CareLink data after %d attempts', [MAX_ATTEMPTS]);
end;

{------------------------------------------------------------------------------
  Constructors / destructor
 ------------------------------------------------------------------------------}
constructor CareLink.Create(AUser, ACreds: string; ARegion: TCareLinkRegion);
begin
  ua := CARELINK_USER_AGENT;

  FUsername := Trim(AUser);
  FRawCreds := ACreds;
  FRegion := ARegion;
  FActiveInsulin := -1;
  FBasalRate := -1;
  FDeviceStatusValid := false;
  clearDeviceStatus(FDeviceStatus);
  FBolusesValid := false;
  SetLength(FBoluses, 0);
  FCarbsValid := false;
  SetLength(FCarbs, 0);

  baseUrl := CARELINK_DISCOVERY_URL; // replaced by baseUrlCumulus after discovery

  // Parent ctor sets timezone, allocates native helper, and initializes thresholds
  inherited Create(AUser, ACreds);
end;

destructor CareLink.Destroy;
begin
  FreeAndNil(FCreds);
  inherited Destroy;
end;

constructor CareLinkUS.Create(AUser, ACreds: string);
begin
  inherited Create(AUser, ACreds, crUS);
end;

constructor CareLinkEU.Create(AUser, ACreds: string);
begin
  inherited Create(AUser, ACreds, crEU);
end;

{------------------------------------------------------------------------------
  Connect: parse the blob, discover endpoints, get a fresh access token,
  resolve role and patient, and probe the data endpoint once.
 ------------------------------------------------------------------------------}
function CareLink.Connect: boolean;
var
  body: string;
  jsonData: TJSONData;
begin
  Result := false;
  log('CareLink.Connect: start');

  if not ParseCredentials then
    Exit;

  if not DiscoverEndpoints then
    Exit;

  if not EnsureFreshToken then
    Exit;

  if FUsername = '' then
    log('CareLink.Connect: warning — no username from settings, blob or token claims');

  FetchUserRole;
  if FRole = '' then
    FRole := 'carepartner';
  SelectPatient;

  if not FetchDisplayMessage(body) then
    Exit;

  // The probe payload also carries the account's limits, the server clock and
  // the device status, so the session starts with real thresholds instead of
  // the base class's placeholder ones. A malformed body here is not fatal —
  // the session is already proven — so failures only cost the metadata.
  try
    jsonData := GetJSON(body);
    try
      ApplyPayloadMetadata(PayloadRoot(jsonData));
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
      log('CareLink.Connect: could not read payload metadata: ' + E.Message);
  end;

  log('CareLink.Connect: session established');
  Result := true;
end;

{------------------------------------------------------------------------------
  Resolve the object carrying the payload's fields. Some payload versions nest
  everything under "patientData".
 ------------------------------------------------------------------------------}
function CareLink.PayloadRoot(AData: TJSONData): TJSONObject;
begin
  Result := nil;
  if not (AData is TJSONObject) then
    Exit;
  Result := TJSONObject(AData);
  if Result.Find('patientData') is TJSONObject then
    Result := TJSONObject(Result.Find('patientData'));
end;

{------------------------------------------------------------------------------
  Map the account's own high/low limits onto Trndi's thresholds.

  The payload carries a schedule: an array of blocks, each stamped with the
  moment it takes effect, so the entry in force now is the newest one not in
  the future. Values are mg/dL like the sg readings, regardless of the
  account's bgUnits display preference.
 ------------------------------------------------------------------------------}
procedure CareLink.ApplyLimits(const ARoot: TJSONObject);
var
  arr: TJSONArray;
  entry, best: TJSONObject;
  i, hi, lo: integer;
  ts, bestTs: TDateTime;
begin
  if not (ARoot.Find('limits') is TJSONArray) then
    Exit;
  arr := TJSONArray(ARoot.Find('limits'));

  best := nil;
  bestTs := 0;
  for i := 0 to arr.Count - 1 do
  begin
    if not (arr[i] is TJSONObject) then
      Continue;
    entry := TJSONObject(arr[i]);
    if not ParseCareLinkTime(entry.Get('timestamp', ''), ts) then
      ts := 0;  // undated entries lose to any dated one
    if (ts <= Now) and ((best = nil) or (ts >= bestTs)) then
    begin
      best := entry;
      bestTs := ts;
    end;
  end;

  // Every block stamped in the future (clock skew): fall back to the last,
  // which is the newest given the payload's ascending order.
  if (best = nil) and (arr.Count > 0) and (arr[arr.Count - 1] is TJSONObject) then
    best := TJSONObject(arr[arr.Count - 1]);
  if best = nil then
    Exit;

  hi := best.Get('highLimit', 0);
  lo := best.Get('lowLimit', 0);

  // Keep the safe defaults rather than trusting a nonsensical pair — a bad
  // threshold silently disables the user's high/low alerts.
  if (hi <= 0) or (lo <= 0) or (hi <= lo) then
  begin
    log(Format('CareLink.ApplyLimits: ignoring implausible limits hi=%d lo=%d',
      [hi, lo]));
    Exit;
  end;

  cgmHi := hi;
  cgmLo := lo;
  log(Format('CareLink.ApplyLimits: hi=%d lo=%d mg/dL (from %d blocks)',
    [hi, lo, arr.Count]));
end;

{------------------------------------------------------------------------------
  Set timeDiff from currentServerTime.

  currentServerTime is a true UTC epoch in milliseconds. Note that the
  neighbouring medicalDeviceTime is NOT — it is the pump's local wall clock
  stuffed into an epoch field, so it reads as the UTC offset ahead of the
  server. Only currentServerTime is usable here.
 ------------------------------------------------------------------------------}
procedure CareLink.ApplyServerTime(const ARoot: TJSONObject);
var
  serverMs: int64;
begin
  serverMs := ARoot.Get('currentServerTime', int64(0));
  if serverMs <= 0 then
    Exit;

  timeDiff := (serverMs div 1000) - DateTimeToUnix(LocalTimeToUniversal(Now));
  log(Format('CareLink.ApplyServerTime: timeDiff=%ds', [timeDiff]));
end;

{------------------------------------------------------------------------------
  Fill the sensor/pump housekeeping record from the payload.
 ------------------------------------------------------------------------------}
procedure CareLink.ApplyDeviceStatus(const ARoot: TJSONObject);
var
  state: string;
begin
  clearDeviceStatus(FDeviceStatus);

  FDeviceStatus.sensorDurationHours :=
    ARoot.Get('sensorDurationHours', DEVICE_STATUS_UNKNOWN);
  FDeviceStatus.reservoirPercent :=
    ARoot.Get('reservoirLevelPercent', DEVICE_STATUS_UNKNOWN);
  FDeviceStatus.reservoirUnits :=
    ARoot.Get('reservoirRemainingUnits', double(DEVICE_STATUS_UNKNOWN));
  FDeviceStatus.pumpBatteryPercent :=
    ARoot.Get('pumpBatteryLevelPercent', DEVICE_STATUS_UNKNOWN);
  FDeviceStatus.pumpSuspended := ARoot.Get('pumpSuspended', false);

  // gstBatteryLevel is the CGM transmitter; 255 is the device's "no reading"
  // marker rather than a full battery.
  FDeviceStatus.transmitterBatteryPercent :=
    ARoot.Get('gstBatteryLevel', DEVICE_STATUS_UNKNOWN);
  if FDeviceStatus.transmitterBatteryPercent > 100 then
    FDeviceStatus.transmitterBatteryPercent := DEVICE_STATUS_UNKNOWN;

  state := ARoot.Get('sensorState', '');
  FDeviceStatus.sensorState := state;
  // Only an explicit non-OK state counts as a fault; an absent field must not
  // make a healthy sensor look broken.
  FDeviceStatus.sensorOK := (state = '') or (UpperCase(state) = 'NO_ERROR_MESSAGE');

  state := ARoot.Get('systemStatusMessage', '');
  if UpperCase(state) <> 'NO_ERROR_MESSAGE' then
    FDeviceStatus.statusMessage := state;

  FDeviceStatusValid := true;
end;

{------------------------------------------------------------------------------
  Read activeInsulin (IOB). Older payload variants reported a bare number.
 ------------------------------------------------------------------------------}
procedure CareLink.ApplyActiveInsulin(const ARoot: TJSONObject);
var
  propData: TJSONData;
  stamp: TDateTime;
begin
  FActiveInsulin := -1;
  FActiveInsulinAt := 0;

  propData := ARoot.Find('activeInsulin');
  if propData is TJSONObject then
  begin
    FActiveInsulin := TJSONObject(propData).Get('amount', -1.0);
    if ParseCareLinkTime(TJSONObject(propData).Get('datetime', ''), stamp) then
      FActiveInsulinAt := stamp;
  end
  else
  if propData is TJSONNumber then
    FActiveInsulin := propData.AsFloat;
end;

{------------------------------------------------------------------------------
  Derive the delivered basal rate from the markers list.

  SmartGuard pumps deliver auto-basal as a micro-bolus every five minutes
  rather than running a programmed rate, so "basal" is null and the real
  figure is the sum of the last hour of AUTO_BASAL_DELIVERY amounts (already
  U/hr, being a full hour's worth). Manual-mode pumps are expected to populate
  the "basal" object instead; its shape is unknown, so that path is left to
  whoever captures one.
 ------------------------------------------------------------------------------}
function CareLink.ComputeAutoBasalRate(const ARoot: TJSONObject): single;
const
  WINDOW_MIN = 60;
var
  arr: TJSONArray;
  entry, dataObj, valuesObj: TJSONObject;
  i, counted: integer;
  cutoff, stamp: TDateTime;
  total, amount: double;
  fs: TFormatSettings;
begin
  Result := -1;
  if not (ARoot.Find('markers') is TJSONArray) then
    Exit;
  arr := TJSONArray(ARoot.Find('markers'));

  cutoff := IncMinute(Now, -WINDOW_MIN);
  total := 0;
  counted := 0;

  // bolusAmount is a string in this payload and always dot-decimal, so it must
  // not be read through a comma-decimal system locale.
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  for i := 0 to arr.Count - 1 do
  begin
    if not (arr[i] is TJSONObject) then
      Continue;
    entry := TJSONObject(arr[i]);
    if UpperCase(entry.Get('type', '')) <> 'AUTO_BASAL_DELIVERY' then
      Continue;
    if not ParseCareLinkTime(entry.Get('timestamp', ''), stamp) then
      Continue;
    if stamp < cutoff then
      Continue;

    if not (entry.Find('data') is TJSONObject) then
      Continue;
    dataObj := TJSONObject(entry.Find('data'));
    if not (dataObj.Find('dataValues') is TJSONObject) then
      Continue;
    valuesObj := TJSONObject(dataObj.Find('dataValues'));

    if TryStrToFloat(valuesObj.Get('bolusAmount', ''), amount, fs) then
    begin
      total := total + amount;
      Inc(counted);
    end;
  end;

  if counted = 0 then
    Exit;

  Result := total;
  log(Format('CareLink.ComputeAutoBasalRate: %.3f U/hr from %d markers',
    [total, counted]));
end;

{------------------------------------------------------------------------------
  Collect the markers list's insulin deliveries.

  Two marker types carry insulin. AUTO_BASAL_DELIVERY is the SmartGuard
  micro-bolus and is confirmed against a live 780G payload. INSULIN is the
  user- or pump-requested bolus; its field names follow the shape the community
  CareLink clients use, but we have not yet seen one in a capture, so every
  field is probed rather than assumed and an unusable marker is skipped instead
  of contributing a zero. MEAL markers are carbohydrate entries, not insulin,
  and are deliberately not deliveries — they are attached to the bolus they
  accompany only when the pump reports the carbs on the bolus itself.

  Marker types we do not recognise are counted into the summary log line, which
  is how the next unfamiliar payload announces itself.
 ------------------------------------------------------------------------------}
procedure CareLink.ExtractBoluses(const ARoot: TJSONObject);
var
  arr: TJSONArray;
  entry, dataObj, valuesObj: TJSONObject;
  i, j, count, autoCount, skipped: integer;
  stamp: TDateTime;
  amount, carbAmount: double;
  fs: TFormatSettings;
  kindStr: string;
  swap: TBolusEntry;

  function MarkerAmount(const AObj: TJSONObject; const AKey: string;
    out AValue: double): boolean;
  begin
    Result := CareLinkMarkerAmount(AObj, AKey, fs, AValue);
  end;

begin
  SetLength(FBoluses, 0);
  FBolusesValid := false;
  if not (ARoot.Find('markers') is TJSONArray) then
    Exit;
  arr := TJSONArray(ARoot.Find('markers'));

  // Delivery amounts are dot-decimal regardless of the user's locale.
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  SetLength(FBoluses, arr.Count);
  count := 0;
  autoCount := 0;
  skipped := 0;

  for i := 0 to arr.Count - 1 do
  begin
    if not (arr[i] is TJSONObject) then
      Continue;
    entry := TJSONObject(arr[i]);
    kindStr := UpperCase(entry.Get('type', ''));

    if (kindStr <> 'AUTO_BASAL_DELIVERY') and (kindStr <> 'INSULIN') then
      Continue;

    if not ParseCareLinkTime(entry.Get('timestamp', ''), stamp) then
    begin
      Inc(skipped);
      Continue;
    end;

    valuesObj := nil;
    if entry.Find('data') is TJSONObject then
    begin
      dataObj := TJSONObject(entry.Find('data'));
      if dataObj.Find('dataValues') is TJSONObject then
        valuesObj := TJSONObject(dataObj.Find('dataValues'))
      else
        valuesObj := dataObj;
    end;
    if valuesObj = nil then
    begin
      Inc(skipped);
      Continue;
    end;

    FBoluses[count] := Default(TBolusEntry);
    FBoluses[count].time := stamp;
    FBoluses[count].kind := kindStr;

    if kindStr = 'AUTO_BASAL_DELIVERY' then
    begin
      if not MarkerAmount(valuesObj, 'bolusAmount', amount) then
      begin
        Inc(skipped);
        Continue;
      end;
      FBoluses[count].automatic := true;
      Inc(autoCount);
    end
    else
    begin
      // Prefer what actually went in over what was programmed: an interrupted
      // bolus delivers less than it was asked for, and the delivered figure is
      // the one that matches the glucose curve.
      if not MarkerAmount(valuesObj, 'deliveredFastAmount', amount) then
        if not MarkerAmount(valuesObj, 'programmedFastAmount', amount) then
          if not MarkerAmount(valuesObj, 'bolusAmount', amount) then
          begin
            Inc(skipped);
            Continue;
          end;

      // The pump's own corrections arrive as INSULIN markers too; only the
      // activation type separates them from a bolus the user asked for.
      kindStr := UpperCase(valuesObj.Get('activationType', ''));
      FBoluses[count].automatic := (kindStr = 'AUTOCORRECTION') or
        (kindStr = 'AUTO_CORRECTION');
      if kindStr <> '' then
        FBoluses[count].kind := kindStr;
      if FBoluses[count].automatic then
        Inc(autoCount);

      if MarkerAmount(valuesObj, 'carbInput', carbAmount) and (carbAmount > 0) then
        FBoluses[count].carbs := carbAmount;
    end;

    // A zero delivery is a record of nothing happening; drawing it would put a
    // marker on the graph for insulin that was never given.
    if amount <= 0 then
    begin
      Inc(skipped);
      Continue;
    end;

    FBoluses[count].units := amount;
    Inc(count);
  end;

  SetLength(FBoluses, count);
  FBolusesValid := true;

  // Oldest first. The payload has arrived in order so far, but the overlay
  // draws in array order and must not depend on that holding.
  for i := 1 to High(FBoluses) do
  begin
    swap := FBoluses[i];
    j := i - 1;
    while (j >= 0) and (FBoluses[j].time > swap.time) do
    begin
      FBoluses[j + 1] := FBoluses[j];
      Dec(j);
    end;
    FBoluses[j + 1] := swap;
  end;

  log(Format('CareLink.ExtractBoluses: %d deliveries (%d automatic, %d manual)'
    + ', %d markers skipped, %d markers total',
    [count, autoCount, count - autoCount, skipped, arr.Count]));
end;

{------------------------------------------------------------------------------
  Collect carbohydrate entries.

  Carbs reach us by two routes: a dedicated MEAL marker, and the carb figure
  attached to the INSULIN marker of the bolus that covered the meal. A meal the
  user bolused for normally produces both, so taking them at face value would
  draw every meal twice.

  MEAL markers are therefore the source of record, and a bolus's carbs are only
  added when no meal marker sits near it in time — which covers a pump that
  reports carbs on the bolus alone. The matching window is deliberately loose:
  entering carbs and confirming the bolus are separate actions a minute or two
  apart, and double-counting a meal is a worse error than merging two genuinely
  separate snacks eaten within a quarter of an hour.

  The gram field's spelling is probed rather than assumed — see ExtractBoluses
  for why — and a MEAL marker whose amount we cannot read is counted into the
  summary log line instead of becoming a 0 g entry.
 ------------------------------------------------------------------------------}
procedure CareLink.ExtractCarbs(const ARoot: TJSONObject);
const
  // Plausible spellings of the gram field, best-known first.
  GRAM_KEYS: array[0..3] of string = ('amount', 'carbInput', 'carbs', 'mealAmount');
  MEAL_MATCH_MIN = 15;
var
  arr: TJSONArray;
  entry, dataObj, valuesObj: TJSONObject;
  i, j, k, count, mealCount, skipped: integer;
  stamp: TDateTime;
  grams: double;
  fs: TFormatSettings;
  swap: TCarbEntry;
  matched, gotGrams: boolean;
begin
  SetLength(FCarbs, 0);
  FCarbsValid := false;
  if not (ARoot.Find('markers') is TJSONArray) then
    Exit;
  arr := TJSONArray(ARoot.Find('markers'));

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  SetLength(FCarbs, arr.Count + Length(FBoluses));
  count := 0;
  skipped := 0;

  for i := 0 to arr.Count - 1 do
  begin
    if not (arr[i] is TJSONObject) then
      Continue;
    entry := TJSONObject(arr[i]);
    if UpperCase(entry.Get('type', '')) <> 'MEAL' then
      Continue;

    if not ParseCareLinkTime(entry.Get('timestamp', ''), stamp) then
    begin
      Inc(skipped);
      Continue;
    end;

    valuesObj := nil;
    if entry.Find('data') is TJSONObject then
    begin
      dataObj := TJSONObject(entry.Find('data'));
      if dataObj.Find('dataValues') is TJSONObject then
        valuesObj := TJSONObject(dataObj.Find('dataValues'))
      else
        valuesObj := dataObj;
    end;

    gotGrams := false;
    for k := Low(GRAM_KEYS) to High(GRAM_KEYS) do
      if CareLinkMarkerAmount(valuesObj, GRAM_KEYS[k], fs, grams) then
      begin
        gotGrams := true;
        Break;
      end;

    if (not gotGrams) or (grams <= 0) then
    begin
      Inc(skipped);
      Continue;
    end;

    FCarbs[count] := Default(TCarbEntry);
    FCarbs[count].time := stamp;
    FCarbs[count].grams := grams;
    FCarbs[count].kind := 'MEAL';
    Inc(count);
  end;

  mealCount := count;

  // Fall back to the bolus's own carb figure only where no meal marker claims
  // the same moment.
  for i := 0 to High(FBoluses) do
  begin
    if FBoluses[i].carbs <= 0 then
      Continue;

    matched := false;
    for j := 0 to mealCount - 1 do
      if Abs(MinutesBetween(FCarbs[j].time, FBoluses[i].time)) <= MEAL_MATCH_MIN then
      begin
        matched := true;
        Break;
      end;
    if matched then
      Continue;

    FCarbs[count] := Default(TCarbEntry);
    FCarbs[count].time := FBoluses[i].time;
    FCarbs[count].grams := FBoluses[i].carbs;
    FCarbs[count].kind := FBoluses[i].kind;
    Inc(count);
  end;

  SetLength(FCarbs, count);
  FCarbsValid := true;

  // Oldest first, as for boluses: the overlay draws in array order.
  for i := 1 to High(FCarbs) do
  begin
    swap := FCarbs[i];
    j := i - 1;
    while (j >= 0) and (FCarbs[j].time > swap.time) do
    begin
      FCarbs[j + 1] := FCarbs[j];
      Dec(j);
    end;
    FCarbs[j + 1] := swap;
  end;

  log(Format('CareLink.ExtractCarbs: %d entries (%d meal markers, %d from '
    + 'boluses), %d meal markers skipped',
    [count, mealCount, count - mealCount, skipped]));
end;

{------------------------------------------------------------------------------
  Apply everything the payload says apart from the readings themselves.
 ------------------------------------------------------------------------------}
procedure CareLink.ApplyPayloadMetadata(const ARoot: TJSONObject);
begin
  if ARoot = nil then
    Exit;
  ApplyLimits(ARoot);
  ApplyServerTime(ARoot);
  ApplyDeviceStatus(ARoot);
  ApplyActiveInsulin(ARoot);
  FBasalRate := ComputeAutoBasalRate(ARoot);
  ExtractBoluses(ARoot);
  // Reads the boluses it just collected, so the order here matters.
  ExtractCarbs(ARoot);
end;

{------------------------------------------------------------------------------
  Cached accessors. Both answer from the last fetch rather than issuing a
  request: readings are refreshed every few minutes anyway, and each data call
  costs a token use.
 ------------------------------------------------------------------------------}
function CareLink.getBasalRate: single;
begin
  // The base class documents 0 as "unavailable"; our own sentinel is -1.
  if FBasalRate < 0 then
    Result := 0
  else
    Result := FBasalRate;
end;

function CareLink.getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean;
begin
  if not FDeviceStatusValid then
  begin
    clearDeviceStatus(AStatus);
    Exit(false);
  end;
  AStatus := FDeviceStatus;
  Result := true;
end;

function CareLink.getBoluses(out ABoluses: TBolusList): boolean;
begin
  ABoluses := Copy(FBoluses);
  Result := FBolusesValid and (Length(ABoluses) > 0);
end;

function CareLink.supportsBoluses: boolean;
begin
  Result := true;
end;

function CareLink.getCarbs(out ACarbs: TCarbList): boolean;
begin
  ACarbs := Copy(FCarbs);
  Result := FCarbsValid and (Length(ACarbs) > 0);
end;

function CareLink.supportsCarbs: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  Retrieve glucose readings from the CareLink display-message payload.
  The payload carries roughly the last 24 hours of SG entries plus the
  device's own trend arrow and active insulin (IOB).
 ------------------------------------------------------------------------------}
function CareLink.GetReadings(AMinutes, AMaxCount: integer; {%H-}AExtras: string;
  out ARes: string; {%H-}noCache: boolean): BGResults;
// CareLink data is fetched via an authenticated POST that intermediaries do
// not cache; noCache is accepted for interface compatibility only.
var
  body: string;
  jsonData: TJSONData;
  root, entry: TJSONObject;
  sgsArr: TJSONArray;
  i, count, resultIdx: integer;
  sgValue, bgValue: integer;
  dtStr, serverTrend: string;
  entryTime: TDateTime;
  startDate: TDateTime;
  readingsList: array of BGReading;
  rawDiff, scaledDelta: double;
  secondsDiff: integer;
  procedure SortReadingsNewestFirst(var AReadings: BGResults);
    procedure QuickSort(L, R: Integer);
    var
      I, J: Integer;
      P, T: BGReading;
    begin
      repeat
        I := L;
        J := R;
        P := AReadings[(L + R) div 2];
        repeat
          while AReadings[I].date > P.date do Inc(I);
          while AReadings[J].date < P.date do Dec(J);
          if I <= J then
          begin
            T := AReadings[I];
            AReadings[I] := AReadings[J];
            AReadings[J] := T;
            Inc(I);
            Dec(J);
          end;
        until I > J;
        if L < J then QuickSort(L, J);
        L := I;
      until I >= R;
    end;
  begin
    if Length(AReadings) > 1 then
      QuickSort(Low(AReadings), High(AReadings));
  end;
begin
  SetLength(Result, 0);
  ARes := '';

  if FRefreshToken = '' then
  begin
    lastErr := 'Not authenticated. Call Connect first.';
    ARes := lastErr;
    Exit;
  end;

  try
    if AMinutes <= 0 then
      AMinutes := 1440;
    startDate := Now - (AMinutes / 1440.0);

    if not EnsureFreshToken then
    begin
      ARes := lastErr;
      Exit;
    end;

    if not FetchDisplayMessage(body) then
    begin
      ARes := lastErr;
      Exit;
    end;

    try
      jsonData := GetJSON(body);
    except
      lastErr := 'Unsupported CareLink response format';
      ARes := lastErr;
      Exit;
    end;

    try
      root := PayloadRoot(jsonData);
      if root = nil then
      begin
        lastErr := 'Unsupported CareLink response format';
        ARes := lastErr;
        Exit;
      end;

      // Thresholds, server clock, device status, IOB and basal all come from
      // this same payload; refresh them before touching the readings.
      ApplyPayloadMetadata(root);

      serverTrend := root.Get('lastSGTrend', '');

      sgsArr := nil;
      if root.Find('sgs') is TJSONArray then
        sgsArr := TJSONArray(root.Find('sgs'));

      if sgsArr = nil then
      begin
        // A valid session with no sensor data (warm-up, no uploads) is not
        // an error; report an empty result.
        ARes := 'No CGM readings found in CareLink data';
        Exit;
      end;

      SetLength(readingsList, 0);
      count := 0;

      for i := 0 to sgsArr.Count - 1 do
      begin
        if not (sgsArr[i] is TJSONObject) then
          Continue;
        entry := TJSONObject(sgsArr[i]);

        // sg = 0 marks gaps (warm-up, lost sensor); skip those slots
        sgValue := entry.Get('sg', 0);
        if sgValue <= 0 then
          Continue;

        // SG entries carry "timestamp" (pump-local, no offset); other payload
        // variants have used "datetime"
        dtStr := entry.Get('timestamp', '');
        if dtStr = '' then
          dtStr := entry.Get('datetime', '');
        if dtStr = '' then
          dtStr := entry.Get('dateTime', '');
        if not ParseCareLinkTime(dtStr, entryTime) then
          Continue;

        if entryTime < startDate then
          Continue;

        // Medtronic clamps to 40–400; keep Trndi's out-of-range sentinels
        if sgValue < 40 then
          bgValue := 39
        else if sgValue > 400 then
          bgValue := 401
        else
          bgValue := sgValue;

        SetLength(readingsList, count + 1);
        readingsList[count].Init(mgdl, self.systemName);
        readingsList[count].update(bgValue, 0);
        readingsList[count].date := entryTime;
        readingsList[count].trend := TdPlaceholder;
        readingsList[count].level := getLevel(bgValue);
        Inc(count);
      end;

      if count = 0 then
      begin
        ARes := 'No CGM readings found in specified time range';
        Exit;
      end;

      SetLength(Result, count);
      for resultIdx := 0 to count - 1 do
        Result[resultIdx] := readingsList[resultIdx];
      SortReadingsNewestFirst(Result);

      // Compute deltas/trends between consecutive readings (same scheme as
      // the Tandem backend): store the observed delta, derive the arrow from
      // a 5-minute-normalized rate of change.
      for resultIdx := 0 to High(Result) do
      begin
        if resultIdx < High(Result) then
        begin
          rawDiff := Result[resultIdx].convert(mgdl) - Result[resultIdx + 1].convert(mgdl);
          secondsDiff := Round((Result[resultIdx].date - Result[resultIdx + 1].date) * 86400);
          Result[resultIdx].update(rawDiff, BGDelta, mgdl);
          if (secondsDiff >= 60) and (secondsDiff <= 900) then
          begin
            scaledDelta := rawDiff * (300 / secondsDiff);
            Result[resultIdx].trend := CalculateTrendFromDelta(scaledDelta);
          end
          else
            Result[resultIdx].trend := TdNotComputable;
        end
        else
        begin
          Result[resultIdx].update(0, BGDelta, mgdl);
          Result[resultIdx].trend := TdFlat;
        end;
      end;

      // The newest reading gets the device's own arrow when provided — it is
      // more authoritative than our computed one
      if (serverTrend <> '') and (CareLinkTrendToBG(serverTrend) <> TdPlaceholder) then
        Result[0].trend := CareLinkTrendToBG(serverTrend);

      if (AMaxCount > 0) and (Length(Result) > AMaxCount) then
        SetLength(Result, AMaxCount);

      ARes := 'Retrieved ' + IntToStr(Length(Result)) + ' CGM readings';
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
    begin
      lastErr := 'Error retrieving readings: ' + E.Message;
      ARes := lastErr;
      SetLength(Result, 0);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Parameter labels for UI
 ------------------------------------------------------------------------------}
class function CareLink.ParamLabel(LabelName: APIParamLabel): string;
begin
  case LabelName of
    APLUser: Result := sParamUserName;
    APLPass: Result := sParamPassword;
    APLDesc: Result := sParamDesc;
    APLDescHTML: Result := sParamDescHTML;
    APLCopyright: Result := 'Björn Lindh <github.com/slicke>';
    else Result := '';
  end;
end;

{------------------------------------------------------------------------------
  Test connection
 ------------------------------------------------------------------------------}
class function CareLink.testConnection(AUser, ACreds: string; var ARes: string; AExtra: string): MaybeBool;
var
  api: CareLink;
begin
  if UpperCase(Trim(AExtra)) = 'EU' then
    api := CareLinkEU.Create(AUser, ACreds)
  else
    api := CareLinkUS.Create(AUser, ACreds);
  try
    if api.Connect then
    begin
      ARes := 'Successfully connected to CareLink';
      Result := MaybeBool.true;
    end
    else
    begin
      ARes := api.errormsg;
      Result := MaybeBool.false;
    end;
  finally
    api.Free;
  end;
end;

class function CareLinkUS.testConnection(AUser, ACreds: string; var ARes: string): MaybeBool;
begin
  Result := inherited testConnection(AUser, ACreds, ARes, 'US');
end;

class function CareLinkEU.testConnection(AUser, ACreds: string; var ARes: string): MaybeBool;
begin
  Result := inherited testConnection(AUser, ACreds, ARes, 'EU');
end;

{------------------------------------------------------------------------------
  Get limit values
 ------------------------------------------------------------------------------}
function CareLink.getLimitHigh: integer;
begin
  Result := 400; // Guardian/Simplera sensors report up to 400 mg/dL
end;

function CareLink.getLimitLow: integer;
begin
  Result := 40; // ... and down to 40 mg/dL
end;

{$ifndef X_CONSOLE}
initialization
  // Register the embedded login-helper assets (carelink-login.mjs and the
  // package manifests) so CareLink.WriteAssets can extract them at runtime.
  // The .lrs lives in the repo-root assets/ folder (committed resource bundles);
  // the path is relative to this unit. Regenerate with: make assets (or
  // make.ps1 assets).
  {$I ../../../assets/carelink_assets.lrs}
{$endif}

end.
