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
unit trndi.api.librelinkup;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  // Trndi units
  trndi.types, trndi.api, trndi.native.base, trndi.funcs, slicke.sha256,
  // FPC units
  fpjson, jsonparser, dateutils;

(*******************************************************************************
  Abbott FreeStyle Libre — LibreLinkUp (follower) API

  Modelled on pylibrelinkup (https://github.com/robberwick/pylibrelinkup) and
  the LibreLinkUp Android app it emulates. LibreLinkUp is the *follower* service
  for FreeStyle Libre: the person wearing the sensor shares their readings from
  the LibreLink app, and this backend reads the share.

  The whole surface is two authenticated GETs plus a login POST:
  - POST llu/auth/login                       -> bearer token + account id
  - GET  llu/connections                      -> followed patients
  - GET  llu/connections/{patientId}/graph    -> latest reading + ~12 h history

  Every authenticated call carries three things Abbott checks: the bearer
  token, an `account-id` header holding the SHA-256 of the account id, and the
  `product`/`version` pair identifying the client. Abbott has rejected stale
  client versions in the past, so LLU_VERSION is the constant to bump if the
  API starts refusing requests.

  Regions: Abbott runs a host per region and tells us which one an account
  belongs to — a login against the wrong host answers with a redirect naming
  the right region rather than an error. Connect follows that redirect, so
  the user never has to pick a region.
 ******************************************************************************)

const
  {** Default login host. Accounts belonging elsewhere are redirected from here. }
  LLU_DEFAULT_HOST = 'https://api.libreview.io';

  {** Client identification Abbott's API checks on every call. }
  LLU_PRODUCT = 'llu.android';
  LLU_VERSION = '4.16.0';

  {** The LibreLinkUp Android client's user agent. }
  LLU_USER_AGENT = 'Mozilla/5.0 (Linux; Android 13; Pixel 6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Mobile Safari/537.36';

  {** Minimum seconds between two graph fetches. Abbott rate-limits the follower
      API aggressively (429 + Retry-After); a sensor produces a reading a minute
      at best, so re-serving the cached payload inside this window costs nothing
      and keeps Trndi well clear of the limit. }
  LLU_MIN_FETCH_INTERVAL_SEC = 55;

  {** Re-authenticate this many minutes before the auth ticket's stated expiry. }
  LLU_TOKEN_REFRESH_MARGIN_MIN = 60;

  {** FreeStyle Libre sensors report between these values; outside the range the
      payload carries isHigh/isLow instead of a usable number. }
  LLU_SENSOR_MIN = 40;
  LLU_SENSOR_MAX = 500;

type
  {** One regional API host. Abbott names regions with the short codes used in
      the login redirect ('de', 'eu2', …). }
  TLLURegion = record
    code: string;
    url: string;
  end;

const
  {** Every known LibreLinkUp regional host, keyed by the code the login
      redirect reports. Mirrors pylibrelinkup's APIUrl enum. }
  LLU_REGIONS: array[0..11] of TLLURegion = (
    (code: 'ae';  url: 'https://api-ae.libreview.io'),
    (code: 'ap';  url: 'https://api-ap.libreview.io'),
    (code: 'au';  url: 'https://api-au.libreview.io'),
    (code: 'ca';  url: 'https://api-ca.libreview.io'),
    (code: 'de';  url: 'https://api-de.libreview.io'),
    (code: 'eu';  url: 'https://api-eu.libreview.io'),
    (code: 'eu2'; url: 'https://api-eu2.libreview.io'),
    (code: 'fr';  url: 'https://api-fr.libreview.io'),
    (code: 'jp';  url: 'https://api-jp.libreview.io'),
    (code: 'la';  url: 'https://api-la.libreview.io'),
    (code: 'ru';  url: 'https://api.libreview.ru'),
    (code: 'us';  url: 'https://api.libreview.io')
  );

type
  (*******************************************************************************
    LibreLinkUp class

    Inherits from @code(TrndiAPI) and implements reading retrieval for Abbott's
    LibreLinkUp follower service.

    Credentials: @code(user) is the LibreLinkUp account email, @code(pass) its
    password. Both belong to the *follower* account — the one that accepted the
    sharing invitation — not to the LibreLink account wearing the sensor.

    A single graph request returns both the current reading (with Abbott's own
    trend arrow) and roughly twelve hours of history, so one fetch per cycle
    serves the whole UI.
   ******************************************************************************)
  LibreLinkUp = class(TrndiAPI)
  private
    FEmail: string;         /// LibreLinkUp account email
    FPassword: string;      /// LibreLinkUp account password
    FToken: string;         /// Bearer token from the auth ticket
    FTokenExpires: TDateTime; /// When the auth ticket expires (local time)
    FAccountIdHash: string; /// SHA-256 hex of the account id, for `account-id`
    FPatientId: string;     /// UUID of the followed patient we read
    FPatientName: string;   /// Display name of that patient, for logging
    FHost: string;          /// Resolved regional host (no trailing slash)
    FCacheBody: string;     /// Last graph payload, re-served inside the window
    FCacheAt: TDateTime;    /// When FCacheBody arrived
    FRetryAfter: TDateTime; /// Set from a 429's Retry-After; no calls before it

    {** Headers common to every call; adds the bearer token and account-id
        once authentication has produced them. }
    function BuildHeaders: TStringList;

    {** POST llu/auth/login against AHost. On success the token, expiry and
        account-id hash are stored. When Abbott answers with a region redirect,
        ARedirectHost receives the host to retry against and the result is
        False without an error being recorded. }
    function Login(const AHost: string; out ARedirectHost: string): boolean;

    {** Log in, following at most one region redirect. }
    function Authenticate: boolean;

    {** True when a usable, unexpired token is held; re-authenticates when not. }
    function EnsureToken: boolean;

    {** GET llu/connections and adopt the first followed patient. }
    function SelectPatient: boolean;

    {** GET the patient's graph payload, honouring the fetch-interval cache and
        the rate-limit backoff, and re-authenticating once on a 401. }
    function FetchGraph(out ABody: string): boolean;

    {** Copy the alarm thresholds and target range out of a connection object
        into the inherited CGM threshold properties. }
    procedure ApplyThresholds(AConnection: TJSONObject);

    {** Build one reading from a glucose-measurement object. Returns False when
        the entry carries no usable value or timestamp. }
    function ReadingFromEntry(AEntry: TJSONObject; out AReading: BGReading): boolean;
  public
    {** Create a LibreLinkUp client.
        @param(user LibreLinkUp account email)
        @param(pass LibreLinkUp account password) }
    constructor Create(user, pass: string); override;

    {** Authenticate, resolve the region and the followed patient, and verify
        that the graph endpoint answers.
        @returns(True when a session was established) }
    function connect: boolean; override;

    {** Fetch readings from the patient's graph payload — the current reading
        plus roughly the last twelve hours of history.
        @param(minNum  Only readings newer than this many minutes are kept)
        @param(maxNum  Cap on the number of readings returned; 0 for no cap)
        @param(extras  Unused)
        @param(res     Receives a short status line, or the error on failure)
        @param(noCache Unused; the fetch-interval cache governs freshness)
        @returns(Readings, newest first) }
    function getReadings(minNum, maxNum: integer; extras: string;
      out res: string; noCache: boolean): BGResults; override;

    {** Turn a graph payload into readings without touching the network. The
        I/O-free half of @code(getReadings); also the seam the offline tests
        drive with a recorded payload.
        @param(ABody     Raw JSON as returned by the graph endpoint)
        @param(AMinutes  Only readings newer than this many minutes are kept;
                         0 or less keeps everything in the payload)
        @param(AMaxCount Cap on the number of readings returned; 0 for no cap)
        @param(AReadings Receives the readings, newest first)
        @returns(True when the payload parsed; False sets @code(errormsg)) }
    function ParseGraphPayload(const ABody: string; AMinutes, AMaxCount: integer;
      out AReadings: BGResults): boolean;

    {** UI parameter labels.
        1: account email, 2: password }
    class function ParamLabel(LabelName: APIParamLabel): string; override;

    {** Verify credentials by performing a full connect. }
    class function testConnection(user, pass: string; var res: string): MaybeBool; override;

    {** Name of the followed patient, once connected; empty before that. }
    property patientName: string read FPatientName;
  protected
    {** Sensors report up to 500 mg/dL; above that the payload says isHigh. }
    function getLimitHigh: integer; override;

    {** Sensors report down to 40 mg/dL; below that the payload says isLow. }
    function getLimitLow: integer; override;

    {** Name of this API }
    function getSystemName: string; override;
  end;

{$IFDEF TEST}
{** Map a LibreLinkUp TrendArrow (1..5) to Trndi's arrow enum. Abbott reports
    five arrows and has no double-arrow state, so the mapping is exact and
    never produces @code(TdDoubleUp)/@code(TdDoubleDown). Out-of-range values
    map to @code(TdPlaceholder). Exposed for the test suite only. }
function LibreTrendToBG(ATrend: integer): BGTrend;

{** Parse a LibreLinkUp timestamp ('M/D/YYYY h:mm:ss AM') into a TDateTime.
    Field widths vary, so the value is tokenised rather than pattern-matched,
    and parsing never consults @code(DefaultFormatSettings) — Trndi rewrites
    those to follow the UI locale. When AsUTC the result is converted from UTC
    to local time. Exposed for the test suite only. }
function ParseLibreTime(const S: string; AsUTC: boolean; out ADate: TDateTime): boolean;

{** Resolve a region code from a login redirect to its API host; empty when the
    code is not one Trndi knows. Exposed for the test suite only. }
function LibreRegionHost(const ACode: string): string;
{$ENDIF}

implementation

resourcestring
  sErrLLUNoEmail = 'No LibreLinkUp email address given.';
  sErrLLUNoPassword = 'No LibreLinkUp password given.';
  sErrLLUBadCredentials = 'LibreLinkUp rejected the email or password.';
  sErrLLUTerms = 'LibreLinkUp needs you to accept its terms of use. Open the LibreLinkUp app or LibreView website, accept them, then try again.';
  sErrLLUPrivacy = 'LibreLinkUp needs you to accept its privacy policy. Open the LibreLinkUp app or LibreView website, accept it, then try again.';
  sErrLLUVerifyEmail = 'LibreLinkUp needs you to verify your email address. Check your inbox, verify the account, then try again.';
  sErrLLUUnknownRegion = 'LibreLinkUp redirected this account to an unknown region (%s).';
  sErrLLUNoPatients = 'No shared readings found. The person wearing the sensor must invite this account in the LibreLink app, and the invitation must be accepted.';
  sErrLLUNotAuthenticated = 'Not authenticated. Call Connect first.';
  sErrLLURateLimited = 'LibreLinkUp is rate-limiting this account; waiting before the next request.';
  sErrLLUBadResponse = 'Unsupported LibreLinkUp response format';
  sErrLLUHttp = 'LibreLinkUp request failed (HTTP %d)';
  sParamUserName = 'LibreLinkUp Email';
  sParamPassword = 'LibreLinkUp Password';
  sParamDesc =
    'Abbott FreeStyle Libre, via LibreLinkUp.' + LineEnding + LineEnding +
    'Use the LibreLinkUp follower account — the one that accepted the sharing ' +
    'invitation — not the LibreLink account on the phone wearing the sensor.' + LineEnding + LineEnding +
    'Sharing must already be set up: in the LibreLink app the sensor wearer ' +
    'invites this email, and the invitation has to be accepted in LibreLinkUp ' +
    'once before Trndi can read anything.' + LineEnding + LineEnding +
    'Your region is detected automatically.';
  sParamDescHTML =
    '<b>Abbott FreeStyle Libre</b>, via LibreLinkUp.<br><br>' +
    'Use the <u>LibreLinkUp follower account</u> — the one that accepted the ' +
    'sharing invitation — not the LibreLink account on the phone wearing the sensor.<br><br>' +
    'Sharing must already be set up: in the LibreLink app the sensor wearer ' +
    'invites this email, and the invitation has to be accepted in LibreLinkUp ' +
    'once before Trndi can read anything.<br><br>' +
    'Your region is detected automatically.';

{------------------------------------------------------------------------------
  Lowercase hex of a SHA-256 digest — the form Abbott's account-id header takes.
 ------------------------------------------------------------------------------}
function SHA256Hex(const S: string): string;
const
  HEX: array[0..15] of char = '0123456789abcdef';
var
  digest: TSHA256Digest;
  i: integer;
begin
  digest := SHA256String(S);
  SetLength(Result, Length(digest) * 2);
  for i := 0 to High(digest) do
  begin
    Result[i * 2 + 1] := HEX[(digest[i] shr 4) and $0F];
    Result[i * 2 + 2] := HEX[digest[i] and $0F];
  end;
end;

{------------------------------------------------------------------------------
  Read a response header by name.

  requestEx hands back raw 'Name: value' lines rather than a name/value list,
  and header names arrive lowercased over HTTP/2 and from some proxies, so the
  match has to be case-insensitive and split on the first colon.
 ------------------------------------------------------------------------------}
function HeaderValue(AHeaders: TStringList; const AName: string): string;
var
  i, colon: integer;
begin
  Result := '';
  if not Assigned(AHeaders) then
    Exit;
  for i := 0 to AHeaders.Count - 1 do
  begin
    colon := Pos(':', AHeaders[i]);
    if colon < 2 then
      Continue;
    if SameText(Trim(Copy(AHeaders[i], 1, colon - 1)), AName) then
      Exit(Trim(Copy(AHeaders[i], colon + 1, MaxInt)));
  end;
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
  Map a LibreLinkUp TrendArrow (1..5) to Trndi's arrow enum.
  Abbott's five arrows line up one-for-one with the middle five of Trndi's
  nine; there is no double-arrow state to map to.
 ------------------------------------------------------------------------------}
function LibreTrendToBG(ATrend: integer): BGTrend;
begin
  case ATrend of
  1:
    Result := TdSingleDown;
  2:
    Result := TdFortyFiveDown;
  3:
    Result := TdFlat;
  4:
    Result := TdFortyFiveUp;
  5:
    Result := TdSingleUp;
  else
    Result := TdPlaceholder;
  end;
end;

{------------------------------------------------------------------------------
  Parse a LibreLinkUp timestamp into a TDateTime.

  The API formats these as 'M/D/YYYY h:mm:ss AM' with no zero padding, so field
  positions shift between values and a fixed-width parse would break on the
  10th of the month. Tokenising also keeps the parse away from
  DefaultFormatSettings, which Trndi rewrites whenever the UI locale changes
  (see updateLocale) — StrToDateTime here would start failing the moment a user
  switched language.
 ------------------------------------------------------------------------------}
function ParseLibreTime(const S: string; AsUTC: boolean; out ADate: TDateTime): boolean;
var
  txt, datePart, timePart, ampmPart: string;
  sp1, sp2: integer;
  d1, d2, y, h, n, sec: integer;
  mo, day: integer;

  // Split "a/b/c" or "a:b:c" into three integers
  function SplitTriplet(const src: string; sepA: char; out v1, v2, v3: integer): boolean;
  var
    p1, p2: integer;
  begin
    Result := false;
    p1 := Pos(sepA, src);
    if p1 < 2 then
      Exit;
    p2 := PosEx(sepA, src, p1 + 1);
    if p2 < p1 + 2 then
      Exit;
    if not TryStrToInt(Copy(src, 1, p1 - 1), v1) then
      Exit;
    if not TryStrToInt(Copy(src, p1 + 1, p2 - p1 - 1), v2) then
      Exit;
    if not TryStrToInt(Copy(src, p2 + 1, MaxInt), v3) then
      Exit;
    Result := true;
  end;

begin
  Result := false;
  ADate := 0;
  txt := Trim(S);
  if txt = '' then
    Exit;

  sp1 := Pos(' ', txt);
  if sp1 < 2 then
    Exit;
  datePart := Copy(txt, 1, sp1 - 1);

  ampmPart := '';
  timePart := Trim(Copy(txt, sp1 + 1, MaxInt));
  sp2 := Pos(' ', timePart);
  if sp2 > 0 then
  begin
    ampmPart := UpperCase(Trim(Copy(timePart, sp2 + 1, MaxInt)));
    timePart := Copy(timePart, 1, sp2 - 1);
  end;

  if not SplitTriplet(datePart, '/', d1, d2, y) then
    Exit;
  if not SplitTriplet(timePart, ':', h, n, sec) then
    Exit;

  // Abbott serves M/D/YYYY everywhere, but a value whose first field cannot be
  // a month while the second can is unambiguously D/M/YYYY — accept it rather
  // than dropping the reading.
  if (d1 > 12) and (d2 <= 12) then
  begin
    day := d1;
    mo := d2;
  end
  else
  begin
    mo := d1;
    day := d2;
  end;

  // 12-hour clock when an AM/PM marker is present; 24-hour when it is not
  if ampmPart <> '' then
  begin
    if (Copy(ampmPart, 1, 1) = 'P') and (h < 12) then
      Inc(h, 12)
    else
    if (Copy(ampmPart, 1, 1) = 'A') and (h = 12) then
      h := 0;
  end;

  if not TryEncodeDateTime(y, mo, day, h, n, sec, 0, ADate) then
    Exit;

  if AsUTC then
    ADate := UniversalTimeToLocal(ADate);

  Result := true;
end;

{------------------------------------------------------------------------------
  Resolve a region code from a login redirect to its API host.
 ------------------------------------------------------------------------------}
function LibreRegionHost(const ACode: string): string;
var
  wanted: string;
  i: integer;
begin
  Result := '';
  wanted := LowerCase(Trim(ACode));
  if wanted = '' then
    Exit;
  for i := Low(LLU_REGIONS) to High(LLU_REGIONS) do
    if LLU_REGIONS[i].code = wanted then
      Exit(LLU_REGIONS[i].url);
end;

{------------------------------------------------------------------------------
  getSystemName
 ------------------------------------------------------------------------------}
function LibreLinkUp.getSystemName: string;
begin
  Result := 'LibreLinkUp';
end;

{------------------------------------------------------------------------------
  Construct. Nothing talks to the network until Connect.
 ------------------------------------------------------------------------------}
constructor LibreLinkUp.Create(user, pass: string);
begin
  ua := LLU_USER_AGENT;
  FEmail := Trim(user);
  FPassword := pass;
  FHost := LLU_DEFAULT_HOST;
  baseUrl := FHost;
  FToken := '';
  FTokenExpires := 0;
  FAccountIdHash := '';
  FPatientId := '';
  FPatientName := '';
  FCacheBody := '';
  FCacheAt := 0;
  FRetryAfter := 0;

  inherited Create(user, pass);
end;

{------------------------------------------------------------------------------
  Headers every call carries. The product/version pair identifies the client to
  Abbott; the bearer token and account-id are added once login has produced them.
 ------------------------------------------------------------------------------}
function LibreLinkUp.BuildHeaders: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('accept: application/json');
  Result.Add('content-type: application/json');
  Result.Add('cache-control: no-cache');
  Result.Add('product: ' + LLU_PRODUCT);
  Result.Add('version: ' + LLU_VERSION);
  Result.Add('User-Agent: ' + LLU_USER_AGENT);
  if FToken <> '' then
    Result.Add('authorization: Bearer ' + FToken);
  if FAccountIdHash <> '' then
    Result.Add('account-id: ' + FAccountIdHash);
end;

{------------------------------------------------------------------------------
  POST llu/auth/login.

  Three shapes come back from this endpoint: a redirect naming the account's
  real region, a consent/verification gate the user has to clear elsewhere, or
  a successful ticket. Only the last one authenticates us.
 ------------------------------------------------------------------------------}
function LibreLinkUp.Login(const AHost: string; out ARedirectHost: string): boolean;
var
  headers: TStringList;
  httpResponse: THTTPResponse;
  payload: TJSONObject;
  jsonData: TJSONData;
  root, dataObj, stepObj, ticketObj, userObj: TJSONObject;
  regionCode, accountId, stepType: string;
begin
  Result := false;
  ARedirectHost := '';

  // Login must not carry a stale token from a previous host
  FToken := '';
  FAccountIdHash := '';

  payload := TJSONObject.Create;
  try
    payload.Add('email', FEmail);
    payload.Add('password', FPassword);
    headers := BuildHeaders;
    try
      httpResponse := native.RequestExWait(true, AHost + '/llu/auth/login',
        [], payload.AsJSON, nil, true, 10, headers, false);
    finally
      headers.Free;
    end;
  finally
    payload.Free;
  end;

  try
    log(Format('LibreLinkUp.Login: host=%s status=%d bytes=%d',
      [AHost, httpResponse.StatusCode, Length(httpResponse.Body)]));

    if httpResponse.StatusCode = 429 then
    begin
      lastErr := sErrLLURateLimited;
      Exit;
    end;

    if Trim(httpResponse.Body) = '' then
    begin
      lastErr := Format(sErrLLUHttp, [httpResponse.StatusCode]);
      Exit;
    end;

    try
      jsonData := GetJSON(httpResponse.Body);
    except
      lastErr := sErrLLUBadResponse;
      Exit;
    end;

    try
      if not (jsonData is TJSONObject) then
      begin
        lastErr := sErrLLUBadResponse;
        Exit;
      end;
      root := TJSONObject(jsonData);

      dataObj := nil;
      if root.Find('data') is TJSONObject then
        dataObj := TJSONObject(root.Find('data'));

      // Wrong region: Abbott answers with the code of the right one
      if Assigned(dataObj) and dataObj.Get('redirect', false) then
      begin
        regionCode := dataObj.Get('region', '');
        ARedirectHost := LibreRegionHost(regionCode);
        if ARedirectHost = '' then
          lastErr := Format(sErrLLUUnknownRegion, [regionCode]);
        log('LibreLinkUp.Login: redirected to region ' + regionCode);
        Exit;
      end;

      // Consent and verification gates. None of these can be cleared from
      // Trndi — the user has to do it in Abbott's own app or website.
      if Assigned(dataObj) and (dataObj.Find('step') is TJSONObject) then
      begin
        stepObj := TJSONObject(dataObj.Find('step'));
        stepType := LowerCase(stepObj.Get('type', ''));
        case stepType of
        'tou':
          lastErr := sErrLLUTerms;
        'pp':
          lastErr := sErrLLUPrivacy;
        'verifyemail':
          lastErr := sErrLLUVerifyEmail;
        else
          lastErr := sErrLLUBadCredentials;
        end;
        log('LibreLinkUp.Login: blocked by step=' + stepType);
        Exit;
      end;

      if not Assigned(dataObj) or not (dataObj.Find('authTicket') is TJSONObject) then
      begin
        lastErr := sErrLLUBadCredentials;
        Exit;
      end;

      ticketObj := TJSONObject(dataObj.Find('authTicket'));
      FToken := ticketObj.Get('token', '');
      if FToken = '' then
      begin
        lastErr := sErrLLUBadCredentials;
        Exit;
      end;

      // `expires` is a Unix timestamp; treat a missing/zero value as "unknown",
      // which EnsureToken then refreshes on every call rather than trusting.
      if ticketObj.Get('expires', Int64(0)) > 0 then
        FTokenExpires := UnixToDateTime(ticketObj.Get('expires', Int64(0)), false)
      else
        FTokenExpires := 0;

      if dataObj.Find('user') is TJSONObject then
      begin
        userObj := TJSONObject(dataObj.Find('user'));
        accountId := userObj.Get('id', '');
        if accountId <> '' then
          FAccountIdHash := SHA256Hex(accountId);
      end;

      if FAccountIdHash = '' then
      begin
        // Without the account-id header the data endpoints reject us, so this
        // is a failure even though a token was issued.
        lastErr := sErrLLUBadResponse;
        FToken := '';
        Exit;
      end;

      FHost := AHost;
      baseUrl := AHost;
      log('LibreLinkUp.Login: authenticated against ' + AHost);
      Result := true;
    finally
      jsonData.Free;
    end;
  finally
    FreeResponse(httpResponse);
  end;
end;

{------------------------------------------------------------------------------
  Log in, following at most one region redirect. One hop is enough: the
  redirect names the account's home region outright, so a second redirect would
  mean the API is bouncing us in a loop.
 ------------------------------------------------------------------------------}
function LibreLinkUp.Authenticate: boolean;
var
  redirectHost: string;
begin
  lastErr := '';

  if FEmail = '' then
  begin
    lastErr := sErrLLUNoEmail;
    Exit(false);
  end;
  if FPassword = '' then
  begin
    lastErr := sErrLLUNoPassword;
    Exit(false);
  end;

  Result := Login(FHost, redirectHost);
  if Result or (redirectHost = '') then
    Exit;

  Result := Login(redirectHost, redirectHost);
end;

{------------------------------------------------------------------------------
  Re-authenticate when the ticket is missing, expired, or close enough to
  expiry that a fetch might outlive it.
 ------------------------------------------------------------------------------}
function LibreLinkUp.EnsureToken: boolean;
begin
  if (FToken <> '') and (FTokenExpires > 0) and
    (Now < IncMinute(FTokenExpires, -LLU_TOKEN_REFRESH_MARGIN_MIN)) then
    Exit(true);

  Result := Authenticate;
end;

{------------------------------------------------------------------------------
  GET llu/connections: the accounts sharing readings with this follower.

  Trndi shows one value, so we read the first share. A follower watching more
  than one person gets a notice rather than a silent pick.
 ------------------------------------------------------------------------------}
function LibreLinkUp.SelectPatient: boolean;
var
  headers: TStringList;
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  root: TJSONObject;
  arr: TJSONArray;
  entry: TJSONObject;
begin
  Result := false;

  headers := BuildHeaders;
  try
    httpResponse := native.RequestExWait(false, FHost + '/llu/connections',
      [], '', nil, true, 10, headers, false);
  finally
    headers.Free;
  end;

  try
    log(Format('LibreLinkUp.SelectPatient: status=%d bytes=%d',
      [httpResponse.StatusCode, Length(httpResponse.Body)]));

    if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) or
      (Trim(httpResponse.Body) = '') then
    begin
      lastErr := Format(sErrLLUHttp, [httpResponse.StatusCode]);
      Exit;
    end;

    try
      jsonData := GetJSON(httpResponse.Body);
    except
      lastErr := sErrLLUBadResponse;
      Exit;
    end;

    try
      if not (jsonData is TJSONObject) then
      begin
        lastErr := sErrLLUBadResponse;
        Exit;
      end;
      root := TJSONObject(jsonData);

      if not (root.Find('data') is TJSONArray) then
      begin
        lastErr := sErrLLUNoPatients;
        Exit;
      end;
      arr := TJSONArray(root.Find('data'));

      if arr.Count = 0 then
      begin
        lastErr := sErrLLUNoPatients;
        Exit;
      end;

      if not (arr[0] is TJSONObject) then
      begin
        lastErr := sErrLLUBadResponse;
        Exit;
      end;

      entry := TJSONObject(arr[0]);
      FPatientId := entry.Get('patientId', '');
      if FPatientId = '' then
      begin
        lastErr := sErrLLUNoPatients;
        Exit;
      end;

      FPatientName := Trim(entry.Get('firstName', '') + ' ' + entry.Get('lastName', ''));
      // Thresholds are on the connection object here too, so a follower who
      // never reaches the graph call still gets sensible limits.
      ApplyThresholds(entry);

      log(Format('LibreLinkUp.SelectPatient: following %s (%s) of %d',
        [FPatientName, FPatientId, arr.Count]));
      if arr.Count > 1 then
        notice('This LibreLinkUp account follows more than one person; Trndi is showing the first.');

      Result := true;
    finally
      jsonData.Free;
    end;
  finally
    FreeResponse(httpResponse);
  end;
end;

{------------------------------------------------------------------------------
  GET the graph payload.

  Serves the cached body inside the fetch window, respects a rate-limit
  backoff, and re-authenticates once if the token turns out to be dead.
 ------------------------------------------------------------------------------}
function LibreLinkUp.FetchGraph(out ABody: string): boolean;
var
  headers: TStringList;
  httpResponse: THTTPResponse;
  retryAfterSec: integer;
  attempt: integer;
  retried: boolean;
begin
  Result := false;
  ABody := '';

  // Abbott produces at most one reading a minute; re-serving the last payload
  // inside the window keeps repeated UI refreshes off the network entirely.
  if (FCacheBody <> '') and (FCacheAt > 0) and
    (SecondsBetween(Now, FCacheAt) < LLU_MIN_FETCH_INTERVAL_SEC) then
  begin
    ABody := FCacheBody;
    Exit(true);
  end;

  if (FRetryAfter > 0) and (Now < FRetryAfter) then
  begin
    // Inside a rate-limit backoff: a stale payload beats hammering the API
    if FCacheBody <> '' then
    begin
      ABody := FCacheBody;
      Exit(true);
    end;
    lastErr := sErrLLURateLimited;
    Exit;
  end;

  retried := false;
  for attempt := 1 to 2 do
  begin
    headers := BuildHeaders;
    try
      httpResponse := native.RequestExWait(false,
        FHost + '/llu/connections/' + FPatientId + '/graph',
        [], '', nil, true, 10, headers, false);
    finally
      headers.Free;
    end;

    try
      log(Format('LibreLinkUp.FetchGraph: status=%d bytes=%d attempt=%d',
        [httpResponse.StatusCode, Length(httpResponse.Body), attempt]));

      if httpResponse.StatusCode = 429 then
      begin
        if not TryStrToInt(HeaderValue(httpResponse.Headers, 'Retry-After'), retryAfterSec) then
          retryAfterSec := 60;
        FRetryAfter := IncSecond(Now, retryAfterSec);
        lastErr := sErrLLURateLimited;
        log(Format('LibreLinkUp.FetchGraph: rate-limited, backing off %d s', [retryAfterSec]));
        Exit;
      end;

      // A dead token looks like a 401; re-login once and try the fetch again
      if (httpResponse.StatusCode = 401) and not retried then
      begin
        retried := true;
        log('LibreLinkUp.FetchGraph: token rejected, re-authenticating');
        FToken := '';
        FTokenExpires := 0;
        if not Authenticate then
          Exit;
        Continue;
      end;

      if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) or
        (Trim(httpResponse.Body) = '') then
      begin
        lastErr := Format(sErrLLUHttp, [httpResponse.StatusCode]);
        Exit;
      end;

      ABody := httpResponse.Body;
      FCacheBody := ABody;
      FCacheAt := Now;
      FRetryAfter := 0;
      Exit(true);
    finally
      FreeResponse(httpResponse);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Copy LibreLinkUp's alarm thresholds and target range into the inherited CGM
  properties.

  alarmRules.h/l are the user's own high and low alarms, so they map to the
  hi/lo thresholds the way NightScout's bgHigh/bgLow do; targetHigh/targetLow
  describe the in-range band. Values are always mg/dL here (the `thmm` twins
  carry the mmol/L equivalents, which Trndi converts for itself).
 ------------------------------------------------------------------------------}
procedure LibreLinkUp.ApplyThresholds(AConnection: TJSONObject);
var
  rules, rule: TJSONObject;
  v: integer;
begin
  if not Assigned(AConnection) then
    Exit;

  if AConnection.Find('alarmRules') is TJSONObject then
  begin
    rules := TJSONObject(AConnection.Find('alarmRules'));

    if rules.Find('h') is TJSONObject then
    begin
      rule := TJSONObject(rules.Find('h'));
      v := rule.Get('th', 0);
      if v > 0 then
        cgmHi := v;
    end;

    if rules.Find('l') is TJSONObject then
    begin
      rule := TJSONObject(rules.Find('l'));
      v := rule.Get('th', 0);
      if v > 0 then
        cgmLo := v;
    end;
  end;

  v := AConnection.Get('targetHigh', 0);
  if v > 0 then
    cgmRangeHi := v;

  v := AConnection.Get('targetLow', 0);
  if v > 0 then
    cgmRangeLo := v;
end;

{------------------------------------------------------------------------------
  Build one reading from a glucose-measurement object.

  FactoryTimestamp is the sensor's own UTC stamp; Timestamp is the same instant
  rendered in the *account's* timezone, which is not necessarily this machine's.
  Reading the UTC one and converting locally is therefore the only mapping that
  survives a follower in a different timezone from the sensor wearer.
 ------------------------------------------------------------------------------}
function LibreLinkUp.ReadingFromEntry(AEntry: TJSONObject; out AReading: BGReading): boolean;
var
  mgdlValue: double;
  value: integer;
  entryTime: TDateTime;
begin
  Result := false;

  if not Assigned(AEntry) then
    Exit;

  if not ParseLibreTime(AEntry.Get('FactoryTimestamp', ''), true, entryTime) then
    // No UTC stamp: the local-time twin is better than dropping the reading
    if not ParseLibreTime(AEntry.Get('Timestamp', ''), false, entryTime) then
      Exit;

  mgdlValue := AEntry.Get('ValueInMgPerDl', 0.0);

  // Outside the sensor's range Abbott reports the clamped number with a flag.
  // Push those past Trndi's limits so the UI renders HIGH/LOW instead of a
  // value the sensor never actually measured.
  if AEntry.Get('isHigh', false) or (mgdlValue > LLU_SENSOR_MAX) then
    value := LLU_SENSOR_MAX + 1
  else
  if AEntry.Get('isLow', false) or ((mgdlValue > 0) and (mgdlValue < LLU_SENSOR_MIN)) then
    value := LLU_SENSOR_MIN - 1
  else
  if mgdlValue <= 0 then
    // A zero value is a gap marker (warm-up, lost signal), not a reading
    Exit
  else
    value := Round(mgdlValue);

  AReading.Init(mgdl, self.systemName);
  AReading.update(value, 0);
  AReading.date := entryTime;
  AReading.trend := TdPlaceholder;
  AReading.level := getLevel(value);
  Result := true;
end;

{------------------------------------------------------------------------------
  Turn a graph payload into readings. No network access — getReadings supplies
  the body, and the offline tests supply a recorded one.
 ------------------------------------------------------------------------------}
function LibreLinkUp.ParseGraphPayload(const ABody: string; AMinutes, AMaxCount: integer;
  out AReadings: BGResults): boolean;
var
  jsonData: TJSONData;
  root, dataObj, conn, current: TJSONObject;
  graphArr: TJSONArray;
  i, count: integer;
  startDate: TDateTime;
  reading: BGReading;
  readingsList: BGResults;
  serverTrend: integer;
  haveCurrent: boolean;
  currentDate: TDateTime;
  rawDiff, scaledDelta: double;
  secondsDiff: integer;
begin
  Result := false;
  SetLength(AReadings, 0);

  if Trim(ABody) = '' then
  begin
    lastErr := sErrLLUBadResponse;
    Exit;
  end;

  try
    jsonData := GetJSON(ABody);
  except
    lastErr := sErrLLUBadResponse;
    Exit;
  end;

  try
    if not (jsonData is TJSONObject) then
    begin
      lastErr := sErrLLUBadResponse;
      Exit;
    end;
    root := TJSONObject(jsonData);

    if not (root.Find('data') is TJSONObject) then
    begin
      lastErr := sErrLLUBadResponse;
      Exit;
    end;
    dataObj := TJSONObject(root.Find('data'));

    conn := nil;
    if dataObj.Find('connection') is TJSONObject then
      conn := TJSONObject(dataObj.Find('connection'));
    ApplyThresholds(conn);

    if AMinutes > 0 then
      startDate := Now - (AMinutes / 1440.0)
    else
      startDate := 0;

    SetLength(readingsList, 0);
    count := 0;

    // The current reading first: it is the only entry carrying Abbott's arrow,
    // and it is normally newer than anything in graphData.
    haveCurrent := false;
    currentDate := 0;
    serverTrend := 0;
    if Assigned(conn) and (conn.Find('glucoseMeasurement') is TJSONObject) then
    begin
      current := TJSONObject(conn.Find('glucoseMeasurement'));
      if ReadingFromEntry(current, reading) then
      begin
        serverTrend := current.Get('TrendArrow', 0);
        currentDate := reading.date;
        haveCurrent := true;
        if reading.date >= startDate then
        begin
          SetLength(readingsList, count + 1);
          readingsList[count] := reading;
          Inc(count);
        end;
      end;
    end;

    if dataObj.Find('graphData') is TJSONArray then
    begin
      graphArr := TJSONArray(dataObj.Find('graphData'));
      for i := 0 to graphArr.Count - 1 do
      begin
        if not (graphArr[i] is TJSONObject) then
          Continue;
        if not ReadingFromEntry(TJSONObject(graphArr[i]), reading) then
          Continue;
        if reading.date < startDate then
          Continue;
        // graphData can repeat the current reading; keep the current one,
        // which is the copy with the trend arrow on it.
        if haveCurrent and (Abs(SecondsBetween(reading.date, currentDate)) < 30) then
          Continue;
        SetLength(readingsList, count + 1);
        readingsList[count] := reading;
        Inc(count);
      end;
    end;

    if count = 0 then
    begin
      // A working session with no readings in the window (sensor warm-up, a
      // share that has gone quiet) is not an error.
      Result := true;
      Exit;
    end;

    SetLength(AReadings, count);
    for i := 0 to count - 1 do
      AReadings[i] := readingsList[i];
    SortReadingsDescending(AReadings);

    // Deltas between consecutive readings, with the arrow derived from a
    // 5-minute-normalised rate — the same scheme the Tandem and CareLink
    // backends use. The stored delta stays raw; the UI normalises it.
    for i := 0 to High(AReadings) do
    begin
      if i < High(AReadings) then
      begin
        rawDiff := AReadings[i].convert(mgdl) - AReadings[i + 1].convert(mgdl);
        secondsDiff := Round((AReadings[i].date - AReadings[i + 1].date) * 86400);
        AReadings[i].update(rawDiff, BGDelta, mgdl);
        if (secondsDiff >= 60) and (secondsDiff <= 900) then
        begin
          scaledDelta := rawDiff * (300 / secondsDiff);
          AReadings[i].trend := CalculateTrendFromDelta(scaledDelta);
        end
        else
          AReadings[i].trend := TdNotComputable;
      end
      else
      begin
        AReadings[i].update(0, BGDelta, mgdl);
        AReadings[i].trend := TdFlat;
      end;
    end;

    // Abbott's own arrow on the newest reading beats anything we computed
    if haveCurrent and (LibreTrendToBG(serverTrend) <> TdPlaceholder) and
      (Abs(SecondsBetween(AReadings[0].date, currentDate)) < 30) then
      AReadings[0].trend := LibreTrendToBG(serverTrend);

    if (AMaxCount > 0) and (Length(AReadings) > AMaxCount) then
      SetLength(AReadings, AMaxCount);

    Result := true;
  finally
    jsonData.Free;
  end;
end;

{------------------------------------------------------------------------------
  Connect: authenticate (following the region redirect), pick up the followed
  patient, and prove the graph endpoint answers.
 ------------------------------------------------------------------------------}
function LibreLinkUp.connect: boolean;
var
  body: string;
  readings: BGResults;
begin
  Result := false;
  log('LibreLinkUp.Connect: start');

  if not Authenticate then
    Exit;

  if not SelectPatient then
    Exit;

  if not FetchGraph(body) then
    Exit;

  // Parsing here is what populates the thresholds before the first fetch
  if not ParseGraphPayload(body, 0, 0, readings) then
    Exit;

  log(Format('LibreLinkUp.Connect: session established, %d readings available',
    [Length(readings)]));
  Result := true;
end;

{------------------------------------------------------------------------------
  Retrieve readings from the patient's graph payload.
 ------------------------------------------------------------------------------}
function LibreLinkUp.getReadings(minNum, maxNum: integer; {%H-}extras: string;
  out res: string; {%H-}noCache: boolean): BGResults;
// noCache is accepted for interface compatibility: the payload is fetched over
// an authenticated request that intermediaries do not cache, and freshness is
// governed by LLU_MIN_FETCH_INTERVAL_SEC instead.
var
  body: string;
begin
  SetLength(Result, 0);
  res := '';
  lastErr := '';

  if FPatientId = '' then
  begin
    lastErr := sErrLLUNotAuthenticated;
    res := lastErr;
    Exit;
  end;

  try
    if not EnsureToken then
    begin
      res := lastErr;
      Exit;
    end;

    if not FetchGraph(body) then
    begin
      res := lastErr;
      Exit;
    end;

    if not ParseGraphPayload(body, minNum, maxNum, Result) then
    begin
      res := lastErr;
      Exit;
    end;

    if Length(Result) = 0 then
      res := 'No CGM readings found in specified time range'
    else
      res := 'Retrieved ' + IntToStr(Length(Result)) + ' CGM readings';
  except
    on E: Exception do
    begin
      lastErr := 'Error retrieving readings: ' + E.Message;
      res := lastErr;
      SetLength(Result, 0);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Parameter labels for UI
 ------------------------------------------------------------------------------}
class function LibreLinkUp.ParamLabel(LabelName: APIParamLabel): string;
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
    Result := '';
  end;
end;

{------------------------------------------------------------------------------
  Test connection
 ------------------------------------------------------------------------------}
class function LibreLinkUp.testConnection(user, pass: string; var res: string): MaybeBool;
var
  api: LibreLinkUp;
begin
  api := LibreLinkUp.Create(user, pass);
  try
    if api.connect then
    begin
      res := 'Connected to LibreLinkUp';
      if api.patientName <> '' then
        res := res + ' (following ' + api.patientName + ')';
      Result := MaybeBool.true;
    end
    else
    begin
      res := api.errormsg;
      Result := MaybeBool.false;
    end;
  finally
    api.Free;
  end;
end;

{------------------------------------------------------------------------------
  Get limit values
 ------------------------------------------------------------------------------}
function LibreLinkUp.getLimitHigh: integer;
begin
  Result := LLU_SENSOR_MAX; // FreeStyle Libre sensors report up to 500 mg/dL
end;

function LibreLinkUp.getLimitLow: integer;
begin
  Result := LLU_SENSOR_MIN; // ... and down to 40 mg/dL
end;

end.
