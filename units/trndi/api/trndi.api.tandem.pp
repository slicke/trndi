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
unit trndi.api.tandem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  // Trndi units
  trndi.types, trndi.api, trndi.native, trndi.native.base, trndi.funcs, trndi.log, math,
  // FPC units
  fpjson, jsonparser, dateutils, StrUtils, base64, sha1, sha256;

(*******************************************************************************
  Tandem Source API endpoint definitions
  Based on tconnectsync v2 (https://github.com/jwoglom/tconnectsync)
 ******************************************************************************)

const
  {** Tandem t:connect login page URL }
  TANDEM_LOGIN_PAGE_URL = 'https://sso.tandemdiabetes.com/';
  
  {** US Region URLs }
  TANDEM_LOGIN_API_URL_US = 'https://tdcservices.tandemdiabetes.com/accounts/api/login';
  TANDEM_SOURCE_URL_US = 'https://source.tandemdiabetes.com/';
  TANDEM_OIDC_CLIENT_ID_US = '0oa27ho9tpZE9Arjy4h7';
  TANDEM_OIDC_ISSUER_US = 'https://tdcservices.tandemdiabetes.com/accounts/api';
  TANDEM_OIDC_JWKS_URL_US = 'https://tdcservices.tandemdiabetes.com/accounts/api/.well-known/openid-configuration/jwks';
  TANDEM_TOKEN_ENDPOINT_US = 'https://tdcservices.tandemdiabetes.com/accounts/api/connect/token';
  TANDEM_AUTHORIZATION_ENDPOINT_US = 'https://tdcservices.tandemdiabetes.com/accounts/api/connect/authorize';
  TANDEM_REDIRECT_URI_US = 'https://sso.tandemdiabetes.com/auth/callback';
  TANDEM_CONTROLIQ_BASE_URL_US = 'https://tdcservices.tandemdiabetes.com/';

  {** EU Region URLs }
  TANDEM_LOGIN_API_URL_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/login';
  TANDEM_SOURCE_URL_EU = 'https://source.eu.tandemdiabetes.com/';
  TANDEM_OIDC_CLIENT_ID_EU = '1519e414-eeec-492e-8c5e-97bea4815a10';
  TANDEM_OIDC_ISSUER_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api';
  TANDEM_OIDC_JWKS_URL_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/.well-known/openid-configuration/jwks';
  TANDEM_TOKEN_ENDPOINT_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/connect/token';
  TANDEM_AUTHORIZATION_ENDPOINT_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/connect/authorize';
  TANDEM_REDIRECT_URI_EU = 'https://source.eu.tandemdiabetes.com/authorize/callback';
  TANDEM_CONTROLIQ_BASE_URL_EU = 'https://tdcservices.eu.tandemdiabetes.com/';

  {** Tandem Android API (used as fallback for Control-IQ auth) }
  TANDEM_ANDROID_BASE_URL_US = 'https://tdcservices.tandemdiabetes.com/';
  TANDEM_ANDROID_BASE_URL_EU = 'https://tdcservices.eu.tandemdiabetes.com/';
  TANDEM_ANDROID_OAUTH_TOKEN_PATH = 'cloud/oauth2/token';
  TANDEM_ANDROID_OAUTH_SCOPES = 'cloud.account cloud.upload cloud.accepttcpp cloud.email cloud.password';
  TANDEM_ANDROID_API_USERNAME_B64 = 'QzIzMzFDRDYtRDQ1MC00OTVFLTlDMTktNjcyMTUyMzBDODVD';
  TANDEM_ANDROID_API_PASSWORD_B64 = 'dHo0MzNLVzVRREM5VjdmIXo2QF4ybyZZNlNHR1lo';
  TANDEM_ANDROID_USER_AGENT = 'Dalvik/2.1.0 (Linux; U; Android 12; Pixel 4a Build/SP2A.220305.012)';
  TANDEM_TCONNECT_WEB_USERNAME_B64 = 'M0U2MzU3QkEtRjYyNS00REQyLUI2NUYtNEI1RTgxNDRBQTZG';
  TANDEM_TCONNECT_WEB_PASSWORD_B64 = 'cUMyaXFIc2w3OFFoR0RYdCpMenFwb1pxZTl3eHN6';
  TANDEM_BASE_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.63 Safari/537.36';
  TANDEM_SOURCE_DEFAULT_EVENT_IDS = '229,5,28,4,26,99,279,3,16,59,21,55,20,280,64,65,66,61,33,371,171,369,460,172,370,461,372,399,256,213,406,394,212,404,214,405,447,313,60,14,6,90,230,140,12,11,53,13,63,203,307,191';
  TANDEM_EPOCH = 1199145600;

type
  {** Region selector for Tandem t:connect servers }
  TTandemRegion = (trUS, trEU);

  (*******************************************************************************
    Tandem class (abstract base)

    Inherits from @code(TrndiAPI) and implements connectivity and reading
    retrieval for Tandem Source API. Responsible for OAuth2/OIDC authentication,
    session handling, and mapping Tandem pump events into @code(BGResults).

    Concrete region-specific subclasses:
    - @code(TandemUSA)    -> US Tandem Source endpoints
    - @code(TandemEU)     -> EU Tandem Source endpoints
   ******************************************************************************)
  Tandem = class abstract(TrndiAPI)
  private
    FEmail: string;         /// Tandem t:connect account email
    FPassword: string;      /// Tandem t:connect account password
    FRegion: TTandemRegion; /// Server region (US or EU)
    FAccessToken: string;   /// OAuth2 access token
    FIdToken: string;       /// OIDC ID token
    FAccessTokenExpiresAt: TDateTime; /// When the access token expires
    FDeviceId: string;      /// The selected pump device ID
    FPumperId: string;      /// Pumper ID from JWT
    FAccountId: string;     /// Account ID from JWT
    FControlIqUserGuid: string; /// Control-IQ user GUID (web login)
    FControlIqAccessToken: string; /// Control-IQ access token (web login)
    FControlIqAccessTokenExpiresAt: string; /// Control-IQ token expiry (web login)
    FCalcDiff: boolean;     /// If True, compute deltas between consecutive readings

    {** Get region-specific URLs }
    function GetLoginApiUrl: string;
    function GetSourceUrl: string;
    function GetOidcClientId: string;
    function GetOidcIssuer: string;
    function GetOidcJwksUrl: string;
    function GetTokenEndpoint: string;
    function GetAuthorizationEndpoint: string;
    function GetRedirectUri: string;
    function GetControlIqBaseUrl: string;

    {** Control-IQ web login (tconnect UI) }
    function ControlIqLogin: boolean;
    function FindCookieValue(ACookies: TStringList; const AName: string): string;

    {** Android API login fallback (for Control-IQ auth) }
    function AndroidLogin(out AUserGuid, AAccessToken, AAccessTokenExpiresAt: string): boolean;

    {** Generate PKCE code verifier and challenge for OAuth2 }
    function GenerateCodeVerifier: string;
    function GenerateCodeChallenge(const AVerifier: string): string;

    {** Parse JWT token to extract claims }
    function ExtractJWT(const AIdToken: string): boolean;

    {** Select a device from available pumps }
    function SelectDevice: boolean;

    {** Extract authorization code from redirect URL }
    function ExtractAuthCodeFromURL(const AURL: string): string;

  public
    {** Create a Tandem API client.
        
        @param(AEmail    Tandem t:connect email)
        @param(APassword Tandem t:connect password)
        @param(ARegion   Region selector: 'US' for US servers, 'EU' for EU servers)
    }
    constructor Create(AEmail, APassword: string; ARegion: TTandemRegion); reintroduce; overload;

    {** Create with region string instead of enum }
    constructor Create(AEmail, APassword, ARegionStr: string); reintroduce; overload;

    {** Overloaded constructor that allows explicitly enabling/disabling delta calc.
        
        @param(AEmail    Tandem t:connect email)
        @param(APassword Tandem t:connect password)
        @param(ARegion   Region selector)
        @param(ACalcDiff Whether to compute deltas between consecutive readings)
    }
    constructor Create(AEmail, APassword: string; ARegion: TTandemRegion; ACalcDiff: boolean); reintroduce; overload;

    {** Authenticate with Tandem Source, establish a session.

        Workflow:
        1) Login to get initial auth
        2) Perform OIDC flow to get access token
        3) Extract JWT claims
        4) Select device

        @returns(True if session established; otherwise False and lastErr is set)
    }
    function Connect: boolean; override;

    {** Retrieve latest glucose readings.

        @param(AMinutes  Time window in minutes to fetch)
        @param(AMaxCount Maximum number of readings to return)
        @param(AExtras   Reserved; not used here)
        @param(ARes      Out param receiving raw JSON payload)
        @returns(Array of @code(BGReading); may be empty if none/failed)
    }
    function GetReadings(AMinutes, AMaxCount: integer; AExtras: string;
      out ARes: string): BGResults; override;

    {** UI parameter label provider (override).
        1: Tandem Email
        2: Tandem Password
        3: Region (use "US" for US servers, "EU" for EU servers)
    }
    class function ParamLabel(LabelName: APIParamLabel): string; override;

    {** Test connection for Tandem Source API }
    class function testConnection(AEmail, APass: string; var ARes: string; AExtra: string): MaybeBool; overload;

  published
    {** The effective base URL used for API requests. }
    property Remote: string read baseUrl;
    {** Tandem email for this client instance. }
    property Email: string read FEmail;
    {** Current access token (when connected). }
    property AccessToken: string read FAccessToken;
    {** Whether this client computes deltas between consecutive readings. }
    property CalculateDiff: boolean read FCalcDiff;

  protected
    {** Get the value which represents the maximum reading for the backend }
    function getLimitHigh: integer; override;

    {** Get the value which represents the minimum reading for the backend }
    function getLimitLow: integer; override;

    {** Gets the API's name }
    function getSystemName: string; override;
  end;

  (*******************************************************************************
    Region-specific concrete Tandem implementations
   ******************************************************************************)
  TandemUSA = class(Tandem)
  protected
    function getSystemName: string; override;
  public
    constructor Create(const AEmail, APass: string); reintroduce; overload;
    constructor Create(const AEmail, APass: string; ACalcDiff: boolean); reintroduce; overload;
    class function testConnection(AEmail, APass: string; var ARes: string): MaybeBool; overload;
  end;

  TandemEU = class(Tandem)
  protected
    function getSystemName: string; override;
  public
    constructor Create(const AEmail, APass: string); reintroduce; overload;
    constructor Create(const AEmail, APass: string; ACalcDiff: boolean); reintroduce; overload;
    class function testConnection(AEmail, APass: string; var ARes: string): MaybeBool; overload;
  end;

implementation

resourcestring
  sErrTandemPass = 'Invalid Tandem t:connect credentials';
  sErrTandemLogin = 'Login error: Could not establish a valid session';
  sErrTandemNoDevice = 'No pump device found or selected';
  sParamUserName = 'Tandem Email';
  sParamPassword = 'Tandem Password';
  sParamDesc =
    'Tandem t:connect region selection:'#13#10''#13#10'' +
    'Choose the server based on your account region:' + LineEnding +
    '• Tandem (USA): for US accounts' + LineEnding +
    '• Tandem (EU): for European accounts' +
    LineEnding + LineEnding +
    'Your credentials are your Tandem t:connect account email and password.';
  sParamDescHTML =
    '<b>Tandem t:connect</b> region selection:<br><br>'+
    'Choose the server based on your <u>account region</u>:<br>' +
    '• Tandem (USA): for US accounts<br>' +
    '• Tandem (EU): for European accounts' +
    '<br><br>' +
    'Your credentials are your Tandem t:connect account email and password.';

{------------------------------------------------------------------------------
  getSystemName
  Returns the name of this API
 ------------------------------------------------------------------------------}
function Tandem.getSystemName: string;
begin
  Result := 'Tandem t:connect';
end;

function TandemUSA.getSystemName: string;
begin
  Result := 'Tandem t:connect (USA)';
end;

function TandemEU.getSystemName: string;
begin
  Result := 'Tandem t:connect (EU)';
end;

{------------------------------------------------------------------------------
  Get region-specific URLs
 ------------------------------------------------------------------------------}
function Tandem.GetLoginApiUrl: string;
begin
  if FRegion = trUS then
    Result := TANDEM_LOGIN_API_URL_US
  else
    Result := TANDEM_LOGIN_API_URL_EU;
end;

function Tandem.GetSourceUrl: string;
begin
  if FRegion = trUS then
    Result := TANDEM_SOURCE_URL_US
  else
    Result := TANDEM_SOURCE_URL_EU;
end;

function Tandem.GetOidcClientId: string;
begin
  if FRegion = trUS then
    Result := TANDEM_OIDC_CLIENT_ID_US
  else
    Result := TANDEM_OIDC_CLIENT_ID_EU;
end;

function Tandem.GetOidcIssuer: string;
begin
  if FRegion = trUS then
    Result := TANDEM_OIDC_ISSUER_US
  else
    Result := TANDEM_OIDC_ISSUER_EU;
end;

function Tandem.GetOidcJwksUrl: string;
begin
  if FRegion = trUS then
    Result := TANDEM_OIDC_JWKS_URL_US
  else
    Result := TANDEM_OIDC_JWKS_URL_EU;
end;

function Tandem.GetTokenEndpoint: string;
begin
  if FRegion = trUS then
    Result := TANDEM_TOKEN_ENDPOINT_US
  else
    Result := TANDEM_TOKEN_ENDPOINT_EU;
end;

function Tandem.GetAuthorizationEndpoint: string;
begin
  if FRegion = trUS then
    Result := TANDEM_AUTHORIZATION_ENDPOINT_US
  else
    Result := TANDEM_AUTHORIZATION_ENDPOINT_EU;
end;

function Tandem.GetRedirectUri: string;
begin
  if FRegion = trUS then
    Result := TANDEM_REDIRECT_URI_US
  else
    Result := TANDEM_REDIRECT_URI_EU;
end;

function Tandem.GetControlIqBaseUrl: string;
begin
  Result := TANDEM_CONTROLIQ_BASE_URL_US;
end;

function Tandem.FindCookieValue(ACookies: TStringList; const AName: string): string;
var
  i, eqPos: integer;
  namePrefix: string;
begin
  Result := '';
  if ACookies = nil then
    Exit;

  namePrefix := AName + '=';
  for i := 0 to ACookies.Count - 1 do
  begin
    if Pos(namePrefix, ACookies[i]) = 1 then
    begin
      eqPos := Length(namePrefix);
      Result := Copy(ACookies[i], eqPos + 1, MaxInt);
      Exit;
    end;
  end;
end;

function Tandem.AndroidLogin(out AUserGuid, AAccessToken, AAccessTokenExpiresAt: string): boolean;
var
  headers: TStringList;
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  jsonObj, userObj: TJSONObject;
  authUser, authPass, authCreds, authHeader: string;
  primaryBaseUrl, fallbackBaseUrl: string;
  params: TStringArray;
  function UrlEncode(const S: string): string;
  var
    i: integer;
    c: char;
    sb: TStringBuilder;
  begin
    sb := TStringBuilder.Create;
    try
      for i := 1 to Length(S) do
      begin
        c := S[i];
        if (c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~']) then
          sb.Append(c)
        else if c = ' ' then
          sb.Append('+')
        else
          sb.Append('%').Append(IntToHex(Ord(c), 2));
      end;
      Result := sb.ToString;
    finally
      sb.Free;
    end;
  end;
  function TryLoginAt(const ABaseUrl: string): boolean;
  begin
    Result := False;
    headers.Clear;
    headers.Add('Content-Type: application/x-www-form-urlencoded; charset=UTF-8');
    headers.Add('User-Agent: ' + TANDEM_ANDROID_USER_AGENT);
    headers.Add('Accept: application/json');
    headers.Add(authHeader);

    SetLength(params, 4);
    params[0] := 'username=' + UrlEncode(FEmail);
    params[1] := 'password=' + UrlEncode(FPassword);
    params[2] := 'grant_type=password';
    params[3] := 'scope=' + UrlEncode(TANDEM_ANDROID_OAUTH_SCOPES);

    httpResponse := native.RequestEx(true, ABaseUrl + TANDEM_ANDROID_OAUTH_TOKEN_PATH, params, '', nil, true, 10, headers, false);
    try
      if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) then
      begin
        if httpResponse.Body <> '' then
          LogMessageToFile('Android login failed: HTTP ' + IntToStr(httpResponse.StatusCode) + ' body=' + Copy(httpResponse.Body, 1, 200))
        else
          LogMessageToFile('Android login failed: HTTP ' + IntToStr(httpResponse.StatusCode));
        Exit(False);
      end;

      jsonData := GetJSON(httpResponse.Body);
      try
        if not (jsonData is TJSONObject) then
          Exit(False);

        jsonObj := TJSONObject(jsonData);
        AAccessToken := jsonObj.Get('accessToken', '');
        AAccessTokenExpiresAt := jsonObj.Get('accessTokenExpiresAt', '');

        if (jsonObj.Find('user') <> nil) and (jsonObj.Find('user') is TJSONObject) then
        begin
          userObj := TJSONObject(jsonObj.Find('user'));
          AUserGuid := userObj.Get('id', '');
        end
        else
          AUserGuid := '';

        Result := (AAccessToken <> '') and (AAccessTokenExpiresAt <> '');
      finally
        jsonData.Free;
      end;
    finally
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
    end;
  end;
begin
  Result := False;
  AUserGuid := '';
  AAccessToken := '';
  AAccessTokenExpiresAt := '';

  headers := TStringList.Create;
  try
    authUser := DecodeStringBase64(TANDEM_ANDROID_API_USERNAME_B64);
    authPass := DecodeStringBase64(TANDEM_ANDROID_API_PASSWORD_B64);
    authCreds := authUser + ':' + authPass;
    authHeader := 'Authorization: Basic ' + StringReplace(StringReplace(EncodeStringBase64(authCreds), #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]);

    primaryBaseUrl := TANDEM_ANDROID_BASE_URL_US;
    fallbackBaseUrl := '';

    Result := TryLoginAt(primaryBaseUrl);

    if not Result then
      lastErr := 'Android API login failed';
  finally
    headers.Free;
  end;
end;

function Tandem.ControlIqLogin: boolean;
const
  LOGIN_URL = 'https://tconnect.tandemdiabetes.com/login.aspx?ReturnUrl=%2f';
var
  cookieJar: TStringList;
  headers: TStringList;
  postHeaders: TStringList;
  resp: THTTPResponse;
  viewState, viewStateGen, eventValidation: string;
  postParams: array of string;
  urlEncodedEmail, urlEncodedPass: string;
  emailState, passState: string;
  locationHeader: string;
  i: integer;
  loginUrls: array[0..0] of string;
  loginUrlUsed: string;
  lastBody: string;
  lastFinalUrl: string;
  lastStatus: integer;
  function ExtractHiddenValue(const AHtml, AName: string): string;
  var
    htmlLower, nameLower, keyName, keyId, sub: string;
    p, q: integer;
    function ExtractValueFrom(const S: string): string;
    var
      vPos, endPos: integer;
      quoteChar: char;
    begin
      Result := '';
      vPos := Pos('value=', S);
      if vPos = 0 then
        Exit;
      vPos := vPos + 6;
      if (vPos <= Length(S)) and (S[vPos] in ['"', '''']) then
      begin
        quoteChar := S[vPos];
        Inc(vPos);
        endPos := Pos(quoteChar, Copy(S, vPos, MaxInt));
        if endPos > 0 then
          Result := Copy(S, vPos, endPos - 1);
      end
      else
      begin
        endPos := Pos(' ', Copy(S, vPos, MaxInt));
        if endPos = 0 then
          endPos := Pos('>', Copy(S, vPos, MaxInt));
        if endPos > 0 then
          Result := Copy(S, vPos, endPos - 1)
        else
          Result := Copy(S, vPos, MaxInt);
      end;
    end;
  begin
    Result := '';
    htmlLower := LowerCase(AHtml);
    nameLower := LowerCase(AName);
    keyName := 'name="' + nameLower + '"';
    keyId := 'id="' + nameLower + '"';

    p := Pos(keyName, htmlLower);
    if p = 0 then
      p := Pos(keyId, htmlLower);
    if p = 0 then
      Exit;

    sub := Copy(AHtml, p, 2048);
    Result := ExtractValueFrom(sub);
    if Result <> '' then
      Exit;

    // Try single-quote variant
    keyName := 'name=''' + nameLower + '''';
    keyId := 'id=''' + nameLower + '''';
    p := Pos(keyName, htmlLower);
    if p = 0 then
      p := Pos(keyId, htmlLower);
    if p = 0 then
      Exit;
    sub := Copy(AHtml, p, 2048);
    Result := ExtractValueFrom(sub);
  end;
  function TryGetLoginPage(const AUrl: string; out ABody, AFinalUrl: string; out AStatus: integer): boolean;
  var
    localResp: THTTPResponse;
    localLocation: string;
    localLower: string;
    j: integer;
  begin
    Result := False;
    ABody := '';
    AFinalUrl := '';
    AStatus := 0;

    localResp := native.RequestEx(false, AUrl, [], '', cookieJar, true, 10, headers, false);
    try
      AStatus := localResp.StatusCode;
      ABody := localResp.Body;
      AFinalUrl := localResp.FinalURL;
      Result := ABody <> '';
    finally
      localResp.Headers.Free;
      localResp.Cookies.Free;
    end;
  end;
  function UrlEncode(const S: string): string;
  var
    i: integer;
    c: char;
    sb: TStringBuilder;
  begin
    sb := TStringBuilder.Create;
    try
      for i := 1 to Length(S) do
      begin
        c := S[i];
        if (c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~']) then
          sb.Append(c)
        else if c = ' ' then
          sb.Append('+')
        else
          sb.Append('%').Append(IntToHex(Ord(c), 2));
      end;
      Result := sb.ToString;
    finally
      sb.Free;
    end;
  end;
begin
  Result := False;
  lastErr := '';

  cookieJar := TStringList.Create;
  headers := TStringList.Create;
  postHeaders := TStringList.Create;
  try
    headers.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);

    loginUrls[0] := LOGIN_URL;

    viewState := '';
    viewStateGen := '';
    eventValidation := '';
    loginUrlUsed := '';
    lastBody := '';
    lastFinalUrl := '';
    lastStatus := 0;

    for i := 0 to High(loginUrls) do
    begin
      if not TryGetLoginPage(loginUrls[i], lastBody, lastFinalUrl, lastStatus) then
        Continue;

      viewState := ExtractHiddenValue(lastBody, '__VIEWSTATE');
      viewStateGen := ExtractHiddenValue(lastBody, '__VIEWSTATEGENERATOR');
      eventValidation := ExtractHiddenValue(lastBody, '__EVENTVALIDATION');
      if (viewState <> '') and (viewStateGen <> '') and (eventValidation <> '') then
      begin
        loginUrlUsed := loginUrls[i];
        Break;
      end;
    end;

    if (viewState = '') or (viewStateGen = '') or (eventValidation = '') then
    begin
      if lastFinalUrl <> '' then
        LogMessageToFile('Control-IQ login parse failed. finalUrl=' + lastFinalUrl)
      else
        LogMessageToFile('Control-IQ login parse failed. status=' + IntToStr(lastStatus));
      if lastBody <> '' then
        LogMessageToFile('Control-IQ login page prefix=' + Copy(lastBody, 1, 300));
      lastErr := 'Control-IQ login parse failed: missing hidden fields';
      Exit;
    end;

    urlEncodedEmail := UrlEncode(FEmail);
    urlEncodedPass := UrlEncode(FPassword);
    emailState := UrlEncode(Format('{"enabled":true,"emptyMessage":"","validationText":"%s","valueAsString":"%s","lastSetTextBoxValue":"%s"}',
      [FEmail, FEmail, FEmail]));
    passState := UrlEncode(Format('{"enabled":true,"emptyMessage":"","validationText":"%s","valueAsString":"%s","lastSetTextBoxValue":"%s"}',
      [FPassword, FPassword, FPassword]));

    SetLength(postParams, 10);
    postParams[0] := '__LASTFOCUS=';
    postParams[1] := '__EVENTTARGET=ctl00$ContentBody$LoginControl$linkLogin';
    postParams[2] := '__EVENTARGUMENT=';
    postParams[3] := '__VIEWSTATE=' + UrlEncode(viewState);
    postParams[4] := '__VIEWSTATEGENERATOR=' + UrlEncode(viewStateGen);
    postParams[5] := '__EVENTVALIDATION=' + UrlEncode(eventValidation);
    postParams[6] := 'ctl00$ContentBody$LoginControl$txtLoginEmailAddress=' + urlEncodedEmail;
    postParams[7] := 'txtLoginEmailAddress_ClientState=' + emailState;
    postParams[8] := 'ctl00$ContentBody$LoginControl$txtLoginPassword=' + urlEncodedPass;
    postParams[9] := 'txtLoginPassword_ClientState=' + passState;

    postHeaders.AddStrings(headers);
    postHeaders.Add('Referer: ' + loginUrlUsed);
    resp := native.RequestEx(true, loginUrlUsed, postParams, '', cookieJar, false, 0, postHeaders, false);
    try
      if resp.StatusCode = 200 then
      begin
        lastErr := 'Control-IQ login failed: invalid credentials';
        Exit;
      end;
      if resp.StatusCode <> 302 then
      begin
        lastErr := 'Control-IQ login failed: HTTP ' + IntToStr(resp.StatusCode);
        Exit;
      end;

      // Follow redirect to finalize cookies
      locationHeader := '';
      for i := 0 to resp.Headers.Count - 1 do
        if Pos('Location:', resp.Headers[i]) = 1 then
          locationHeader := Trim(Copy(resp.Headers[i], 10, MaxInt));

      if locationHeader <> '' then
      begin
        if locationHeader[1] = '/' then
          locationHeader := 'https://tconnect.tandemdiabetes.com' + locationHeader;
        resp.Headers.Free;
        resp.Cookies.Free;
        resp := native.RequestEx(true, locationHeader, [], '', cookieJar, true, 10, headers, false);
      end;
    finally
      resp.Headers.Free;
      resp.Cookies.Free;
    end;

    // Extract Control-IQ cookies
    FControlIqUserGuid := FindCookieValue(cookieJar, 'UserGUID');
    FControlIqAccessToken := FindCookieValue(cookieJar, 'accessToken');
    FControlIqAccessTokenExpiresAt := FindCookieValue(cookieJar, 'accessTokenExpiresAt');

    if (FControlIqAccessToken = '') or (FControlIqUserGuid = '') then
    begin
      if AndroidLogin(FControlIqUserGuid, FControlIqAccessToken, FControlIqAccessTokenExpiresAt) then
        LogMessageToFile('Control-IQ login fallback: Android API token acquired')
      else
        LogMessageToFile('Control-IQ login fallback failed: Android API login failed');
    end;

    if (FControlIqUserGuid = '') then
    begin
      if FPumperId <> '' then
        FControlIqUserGuid := FPumperId
      else if FAccountId <> '' then
        FControlIqUserGuid := FAccountId;
    end;

    Result := (FControlIqUserGuid <> '') and (FControlIqAccessToken <> '');
    if not Result then
      lastErr := 'Control-IQ login failed: missing cookies/token';
  finally
    cookieJar.Free;
    headers.Free;
    postHeaders.Free;
  end;
end;

{------------------------------------------------------------------------------
  Generate PKCE code verifier (high-entropy random string)
 ------------------------------------------------------------------------------}
function Tandem.GenerateCodeVerifier: string;
var
  i: integer;
begin
  // Generate 64 random bytes
  SetLength(Result, 64);
  for i := 1 to 64 do
    Result[i] := Chr(Random(256));
  
  // Base64url encode (remove padding)
  Result := EncodeStringBase64(Result);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

{------------------------------------------------------------------------------
  Generate PKCE code challenge from verifier (SHA256 hash)
 ------------------------------------------------------------------------------}
function Tandem.GenerateCodeChallenge(const AVerifier: string): string;
var
  digest: TSHA256Digest;
  hashStr: string;
  i: integer;
begin
  // PKCE requires SHA256 for S256 code_challenge
  digest := SHA256String(AVerifier);
  
  SetLength(hashStr, 32);
  for i := 0 to 31 do
    hashStr[i+1] := Chr(digest[i]);
  
  // Base64url encode
  Result := EncodeStringBase64(hashStr);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

{------------------------------------------------------------------------------
  Extract JWT claims from ID token
  Note: This is a simplified implementation. In production, you'd want to
  properly validate the JWT signature using the JWKS endpoint.
 ------------------------------------------------------------------------------}
function Tandem.ExtractJWT(const AIdToken: string): boolean;
var
  parts: TStringArray;
  payload: string;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
  decodedPayload: string;
  guidCandidate: string;
  i: integer;
  keyName: string;
  keyList: string;
  valueStr: string;
  lowerKey: string;
  function LooksLikeGuid(const S: string): boolean;
  begin
    Result := (Length(S) = 36) and (Pos('-', S) > 0);
  end;
  function MaskValue(const S: string): string;
  begin
    if Length(S) <= 8 then
      Result := S
    else
      Result := Copy(S, 1, 4) + '...' + Copy(S, Length(S) - 3, 4);
  end;
begin
  Result := False;
  
  try
    // JWT format: header.payload.signature
    parts := AIdToken.Split('.');
    if Length(parts) <> 3 then
      Exit;
    
    // Decode the payload (base64url)
    payload := parts[1];
    // Add padding if needed
    while (Length(payload) mod 4) <> 0 do
      payload := payload + '=';
    
    payload := StringReplace(payload, '-', '+', [rfReplaceAll]);
    payload := StringReplace(payload, '_', '/', [rfReplaceAll]);
    
    decodedPayload := DecodeStringBase64(payload);
    
    // Parse JSON
    jsonData := GetJSON(decodedPayload);
    try
      if jsonData is TJSONObject then
      begin
        jsonObj := TJSONObject(jsonData);

        keyList := '';
        for i := 0 to jsonObj.Count - 1 do
        begin
          keyName := jsonObj.Names[i];
          if keyList <> '' then
            keyList := keyList + ',';
          keyList := keyList + keyName;

          valueStr := jsonObj.Get(keyName, '');
          if LooksLikeGuid(valueStr) then
            LogMessageToFile('Tandem.Connect: JWT id claim ' + keyName + '=' + MaskValue(valueStr));
        end;
        if keyList <> '' then
          LogMessageToFile('Tandem.Connect: JWT claims=' + keyList);
        
        // Extract claims
        if jsonObj.Find('pumperId') <> nil then
          FPumperId := jsonObj.Get('pumperId', '');
        
        if jsonObj.Find('accountId') <> nil then
          FAccountId := jsonObj.Get('accountId', '');

        if FControlIqUserGuid = '' then
        begin
          guidCandidate := '';
          if (guidCandidate = '') and (jsonObj.Find('userGuid') <> nil) then
            guidCandidate := jsonObj.Get('userGuid', '');
          if (guidCandidate = '') and (jsonObj.Find('userId') <> nil) then
            guidCandidate := jsonObj.Get('userId', '');
          if (guidCandidate = '') and (jsonObj.Find('tconnectUserId') <> nil) then
            guidCandidate := jsonObj.Get('tconnectUserId', '');
          if (guidCandidate = '') and (jsonObj.Find('sub') <> nil) then
            guidCandidate := jsonObj.Get('sub', '');

          if (guidCandidate = '') then
          begin
            for i := 0 to jsonObj.Count - 1 do
            begin
              keyName := jsonObj.Names[i];
              lowerKey := LowerCase(keyName);
              if (Pos('user', lowerKey) > 0) or (Pos('guid', lowerKey) > 0) then
              begin
                valueStr := jsonObj.Get(keyName, '');
                if LooksLikeGuid(valueStr) then
                begin
                  guidCandidate := valueStr;
                  Break;
                end;
              end;
            end;
          end;

          if guidCandidate <> '' then
          begin
            FControlIqUserGuid := guidCandidate;
            LogMessageToFile('Tandem.Connect: JWT controlIqUserGuid=' + FControlIqUserGuid);
          end;
        end;
        
        Result := (FPumperId <> '') and (FAccountId <> '');
      end;
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
    begin
      lastErr := 'Error parsing JWT: ' + E.Message;
      Result := False;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Extract authorization code from redirect callback URL
 ------------------------------------------------------------------------------}
function Tandem.ExtractAuthCodeFromURL(const AURL: string): string;
var
  codePos, ampPos: integer;
begin
  Result := '';
  
  // Look for code= parameter in URL
  codePos := Pos('code=', AURL);
  if codePos > 0 then
  begin
    codePos := codePos + 5; // Skip 'code='
    ampPos := Pos('&', Copy(AURL, codePos, MaxInt));
    
    if ampPos > 0 then
      Result := Copy(AURL, codePos, ampPos - 1)
    else
      Result := Copy(AURL, codePos, MaxInt);
  end;
end;

{------------------------------------------------------------------------------
  Extract authorization code from any text (HTML/JS) containing code=
 ------------------------------------------------------------------------------}
function ExtractAuthCodeFromText(const AText: string): string;
var
  codePos, endPos, i: integer;
  ch: char;
  textLower: string;
  encodedPos: integer;
  token: string;
  posCode: integer;
  valuePos: integer;
  quoteChar: char;
  function ExtractQuotedValue(const S: string; StartPos: integer): string;
  var
    j: integer;
    q: char;
  begin
    Result := '';
    if (StartPos <= 0) or (StartPos > Length(S)) then
      Exit;
    q := S[StartPos];
    if (q <> '"') and (q <> '''') then
      Exit;
    for j := StartPos + 1 to Length(S) do
    begin
      if S[j] = q then
      begin
        Result := Copy(S, StartPos + 1, j - StartPos - 1);
        Exit;
      end;
    end;
  end;
  function ExtractInputCodeValue(const S: string): string;
  var
    lowerS: string;
    p, v: integer;
  begin
    Result := '';
    lowerS := LowerCase(S);
    p := Pos('name="code"', lowerS);
    if p = 0 then
      p := Pos('name=''code''', lowerS);
    if p = 0 then
      Exit;
    v := PosEx('value=', lowerS, p);
    if v = 0 then
      Exit;
    Inc(v, Length('value='));
    Result := ExtractQuotedValue(S, v);
  end;
  function UrlDecode(const S: string): string;
  var
    idx: integer;
    hexStr: string;
    code: integer;
  begin
    Result := '';
    idx := 1;
    while idx <= Length(S) do
    begin
      if S[idx] = '%' then
      begin
        if idx + 2 <= Length(S) then
        begin
          hexStr := Copy(S, idx + 1, 2);
          if TryStrToInt('$' + hexStr, code) then
          begin
            Result := Result + Chr(code);
            Inc(idx, 3);
            Continue;
          end;
        end;
      end
      else if S[idx] = '+' then
      begin
        Result := Result + ' ';
        Inc(idx);
        Continue;
      end;
      Result := Result + S[idx];
      Inc(idx);
    end;
  end;
begin
  Result := '';
  codePos := Pos('code=', AText);
  if codePos = 0 then
    Exit;
  codePos := codePos + 5;
  endPos := Length(AText) + 1;
  for i := codePos to Length(AText) do
  begin
    ch := AText[i];
    if (ch = '&') or (ch = '"') or (ch = '''') or (ch = '<') or (ch = ' ') or (ch = #13) or (ch = #10) then
    begin
      endPos := i;
      Break;
    end;
  end;
  if endPos > codePos then
    Result := Copy(AText, codePos, endPos - codePos);

  if Result <> '' then
    Exit;

  Result := ExtractInputCodeValue(AText);
  if Result <> '' then
    Exit;

  textLower := LowerCase(AText);
  posCode := Pos('"code":"', textLower);
  if posCode = 0 then
    posCode := Pos('''code'':', textLower);
  if posCode > 0 then
  begin
    valuePos := PosEx(':', textLower, posCode);
    if valuePos > 0 then
    begin
      Inc(valuePos);
      while (valuePos <= Length(AText)) and (AText[valuePos] = ' ') do
        Inc(valuePos);
      if (valuePos <= Length(AText)) and ((AText[valuePos] = '"') or (AText[valuePos] = '''')) then
      begin
        quoteChar := AText[valuePos];
        Result := ExtractQuotedValue(AText, valuePos);
        if Result <> '' then
          Exit;
      end;
    end;
  end;

  textLower := LowerCase(AText);
  encodedPos := Pos('code%3d', textLower);
  if encodedPos = 0 then
    encodedPos := Pos('code%3D', AText);
  if encodedPos > 0 then
  begin
    codePos := encodedPos + 7;
    endPos := Length(AText) + 1;
    for i := codePos to Length(AText) do
    begin
      ch := AText[i];
      if (ch = '&') or (ch = '"') or (ch = '''') or (ch = '<') or (ch = ' ') or (ch = #13) or (ch = #10) then
      begin
        endPos := i;
        Break;
      end;
    end;
    if endPos > codePos then
    begin
      token := Copy(AText, codePos, endPos - codePos);
      Result := UrlDecode(token);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Select a pump device from available devices
 ------------------------------------------------------------------------------}
function Tandem.SelectDevice: boolean;
var
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  jsonObj: TJSONObject;
  i: integer;
  maxDate: TDateTime;
  selectedIdx: integer;
  authHeaders: TStringList;
begin
  Result := False;
  authHeaders := TStringList.Create;
  
  try
    authHeaders.Add('Authorization: Bearer ' + FAccessToken);
    authHeaders.Add('Accept: application/json');
    authHeaders.Add('Origin: https://tconnect.tandemdiabetes.com');
    authHeaders.Add('Referer: https://tconnect.tandemdiabetes.com/');
    authHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);
    
    // Get pump event metadata
    httpResponse := native.RequestEx(false, GetSourceUrl + 'api/reports/reportsfacade/' + FPumperId + '/pumpeventmetadata',
      [], '', nil, true, 10, authHeaders, false);

    LogMessageToFile(Format('Tandem.SelectDevice: status=%d bytes=%d',
      [httpResponse.StatusCode, Length(httpResponse.Body)]));
    if Length(httpResponse.Body) > 0 then
      LogMessageToFile('Tandem.SelectDevice: body=' + Copy(httpResponse.Body, 1, 500));
    
    if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) then
    begin
      lastErr := 'Failed to get device list: HTTP ' + IntToStr(httpResponse.StatusCode);
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
      Exit;
    end;
    
    jsonData := GetJSON(httpResponse.Body);
    try
      if jsonData is TJSONObject then
      begin
        jsonObj := TJSONObject(jsonData);
        if jsonObj.Find('pumpEventMetadata') <> nil then
          jsonArray := jsonObj.Arrays['pumpEventMetadata']
        else
        if jsonObj.Find('data') <> nil then
          jsonArray := jsonObj.Arrays['data']
        else
          jsonArray := nil;
      end
      else
        jsonArray := TJSONArray(jsonData);

      if Assigned(jsonArray) then
      begin
        // Ensure array is used even if wrapped
        
        if jsonArray.Count = 0 then
        begin
          lastErr := sErrTandemNoDevice;
          httpResponse.Headers.Free;
          httpResponse.Cookies.Free;
          Exit;
        end;
        
        // Select the pump with the most recent data
        maxDate := 0;
        selectedIdx := 0;
        
        LogMessageToFile(Format('Tandem.SelectDevice: array count=%d', [jsonArray.Count]));
        for i := 0 to jsonArray.Count - 1 do
        begin
          if jsonArray[i] is TJSONObject then
          begin
            jsonObj := TJSONObject(jsonArray[i]);
            // For simplicity, just select the first pump
            // In production, you'd want to let the user choose or pick based on maxDateWithEvents
            if i = 0 then
            begin
              if jsonObj.Find('tconnectDeviceId') <> nil then
                FDeviceId := IntToStr(jsonObj.Integers['tconnectDeviceId'])
              else
                FDeviceId := '';
              selectedIdx := i;
              LogMessageToFile('Tandem.SelectDevice: selected tconnectDeviceId=' + FDeviceId);
              Break;
            end;
          end;
        end;
        
        Result := FDeviceId <> '';
        if not Result then
          lastErr := sErrTandemNoDevice;
      end;
    finally
      jsonData.Free;
      if Assigned(httpResponse.Headers) then
        FreeAndNil(httpResponse.Headers);
      if Assigned(httpResponse.Cookies) then
        FreeAndNil(httpResponse.Cookies);
    end;
  finally
    authHeaders.Free;
  end;
  
  if not Result then
  begin
    if lastErr = '' then
      lastErr := 'Error selecting device';
  end;
end;

{------------------------------------------------------------------------------
  Constructors
 ------------------------------------------------------------------------------}
constructor Tandem.Create(AEmail, APassword: string; ARegion: TTandemRegion);
begin
  Create(AEmail, APassword, ARegion, True);
end;

constructor Tandem.Create(AEmail, APassword, ARegionStr: string);
var
  region: TTandemRegion;
begin
  if UpperCase(Trim(ARegionStr)) = 'EU' then
    region := trEU
  else
    region := trUS;
  
  Create(AEmail, APassword, region, True);
end;

constructor Tandem.Create(AEmail, APassword: string; ARegion: TTandemRegion; ACalcDiff: boolean);
begin
  ua := 'Trndi/1.0 (Tandem Source Client)';
  
  FEmail := AEmail;
  FPassword := APassword;
  FRegion := ARegion;
  FCalcDiff := ACalcDiff;
  
  baseUrl := GetSourceUrl;
  
  // Parent ctor sets timezone, allocates native helper, and initializes thresholds
  inherited Create(AEmail, APassword);
end;

{------------------------------------------------------------------------------
  Region-specific constructors
 ------------------------------------------------------------------------------}
constructor TandemUSA.Create(const AEmail, APass: string);
begin
  inherited Create(AEmail, APass, trUS);
end;

constructor TandemUSA.Create(const AEmail, APass: string; ACalcDiff: boolean);
begin
  inherited Create(AEmail, APass, trUS, ACalcDiff);
end;

class function TandemUSA.testConnection(AEmail, APass: string; var ARes: string): MaybeBool;
begin
  Result := inherited testConnection(AEmail, APass, ARes, 'US');
end;

constructor TandemEU.Create(const AEmail, APass: string);
begin
  inherited Create(AEmail, APass, trEU);
end;

constructor TandemEU.Create(const AEmail, APass: string; ACalcDiff: boolean);
begin
  inherited Create(AEmail, APass, trEU, ACalcDiff);
end;

class function TandemEU.testConnection(AEmail, APass: string; var ARes: string): MaybeBool;
begin
  Result := inherited testConnection(AEmail, APass, ARes, 'EU');
end;

{------------------------------------------------------------------------------
  Connect to Tandem Source and establish a valid session.
  Performs OAuth2/OIDC authentication sequence with full cookie/redirect support.
 ------------------------------------------------------------------------------}
function Tandem.Connect: boolean;
var
  codeVerifier, codeChallenge: string;
  authUrl, loginUrl, tokenUrl: string;
  httpResponse: THTTPResponse;
  loginOk: boolean;
  authCode, idToken: string;
  tokenOk: boolean;
  params: TStringArray;
  jsonData: TJSONData;
  jsonObj, loginJson: TJSONObject;
  cookieJar: TStringList;
  statusVal: string;
  customHeaders: TStringList;
  locationHeader: string;
  headerPreview: string;
  bodyPreview: string;
  headerPreviewLocal: string;
  cookiesCount: integer;
  i: integer;

  function GetHeaderValue(const AHeaders: TStringList; const AName: string): string;
  var
    k: integer;
    nameLower, lineLower: string;
  begin
    Result := '';
    if AHeaders = nil then
      Exit;
    nameLower := LowerCase(AName) + ':';
    for k := 0 to AHeaders.Count - 1 do
    begin
      lineLower := LowerCase(Trim(AHeaders[k]));
      if Pos(nameLower, lineLower) = 1 then
      begin
        Result := Trim(Copy(AHeaders[k], Length(AName) + 2, MaxInt));
        Exit;
      end;
    end;
  end;
begin
  Result := False;
  cookieJar := TStringList.Create;
  customHeaders := TStringList.Create;
  
  try
    LogMessageToFile('Tandem.Connect: start');
    // Generate PKCE parameters
    codeVerifier := GenerateCodeVerifier;
    codeChallenge := GenerateCodeChallenge(codeVerifier);
    
    // Step 1: Build authorization URL
    authUrl := GetAuthorizationEndpoint +
      '?client_id=' + GetOidcClientId +
      '&redirect_uri=' + GetRedirectUri +
      '&response_type=code' +
      '&response_mode=query' +
      '&scope=openid%20email%20profile' +
      '&code_challenge=' + codeChallenge +
      '&code_challenge_method=S256';
    
    // Step 1.5: Prime SSO cookies
    customHeaders.Clear;
    customHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);
    httpResponse := native.RequestEx(false, TANDEM_LOGIN_PAGE_URL, [], '', cookieJar, true, 10, customHeaders, false);
    LogMessageToFile(Format('Tandem.Connect: login page success=%s status=%d bytes=%d err=%s',
      [BoolToStr(httpResponse.Success, true), httpResponse.StatusCode, Length(httpResponse.Body), httpResponse.ErrorMessage]));

    // Step 2: Login (POST credentials as JSON) with cookie jar
    loginUrl := GetLoginApiUrl;
    loginJson := TJSONObject.Create;
    try
      loginJson.Add('username', FEmail);
      loginJson.Add('password', FPassword);

      customHeaders.Clear;
      customHeaders.Add('Referer: ' + TANDEM_LOGIN_PAGE_URL);
      customHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);
      httpResponse := native.RequestEx(true, loginUrl, [], loginJson.AsJSON, cookieJar, true, 10, customHeaders, false);
    finally
      loginJson.Free;
    end;
    LogMessageToFile(Format('Tandem.Connect: login response success=%s status=%d bytes=%d err=%s',
      [BoolToStr(httpResponse.Success, true), httpResponse.StatusCode, Length(httpResponse.Body), httpResponse.ErrorMessage]));
    
    loginOk := httpResponse.Success;
    if (not loginOk) and (httpResponse.StatusCode = 200) then
    begin
      try
        jsonData := GetJSON(httpResponse.Body);
        try
          if jsonData is TJSONObject then
          begin
            jsonObj := TJSONObject(jsonData);
            statusVal := UpperCase(Trim(jsonObj.Get('status', '')));
            if statusVal = 'SUCCESS' then
              loginOk := true;
          end;
        finally
          jsonData.Free;
        end;
      except
        // ignore JSON parse errors here
      end;
    end;
    LogMessageToFile(Format('Tandem.Connect: loginOk=%s statusVal=%s',
      [BoolToStr(loginOk, true), statusVal]));

    if not loginOk then
    begin
      lastErr := 'Login failed: ' + httpResponse.ErrorMessage;
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
      Exit;
    end;
    
    httpResponse.Headers.Free;
    httpResponse.Cookies.Free;
    
    // Step 3: Follow authorization flow with cookies to get redirect with auth code
    customHeaders.Clear;
    customHeaders.Add('Referer: ' + TANDEM_LOGIN_PAGE_URL);
    customHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);
    customHeaders.Add('Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
    httpResponse := native.RequestEx(false, authUrl, [], '', cookieJar, true, 10, customHeaders, false);
    LogMessageToFile(Format('Tandem.Connect: auth response success=%s status=%d finalUrl=%s err=%s',
      [BoolToStr(httpResponse.Success, true), httpResponse.StatusCode, httpResponse.FinalURL, httpResponse.ErrorMessage]));
    
    if (httpResponse.StatusCode >= 400) and (not httpResponse.Success) then
    begin
      lastErr := 'Authorization flow failed: ' + httpResponse.ErrorMessage;
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
      Exit;
    end;
    
    // Extract authorization code from final redirect URL or Location header
    authCode := ExtractAuthCodeFromURL(httpResponse.FinalURL);
    if authCode = '' then
    begin
      locationHeader := GetHeaderValue(httpResponse.Headers, 'Location');
      if locationHeader <> '' then
        authCode := ExtractAuthCodeFromURL(locationHeader);
    end;
    if (authCode = '') and (Length(httpResponse.Body) > 0) then
      authCode := ExtractAuthCodeFromText(httpResponse.Body);
    LogMessageToFile(Format('Tandem.Connect: auth code length=%d', [Length(authCode)]));
    
    if authCode = '' then
    begin
      headerPreview := '';
      if httpResponse.Headers <> nil then
      begin
        for i := 0 to httpResponse.Headers.Count - 1 do
        begin
          if i >= 15 then
          begin
            headerPreview := headerPreview + '...';
            Break;
          end;
          headerPreview := headerPreview + httpResponse.Headers[i] + #13#10;
        end;
        LogMessageToFile('Tandem.Connect: auth response headers (preview):'#13#10 + headerPreview);
      end
      else
        LogMessageToFile('Tandem.Connect: auth response headers (preview): <nil>');
      if locationHeader <> '' then
        LogMessageToFile('Tandem.Connect: auth response Location=' + locationHeader);
      lastErr := 'Failed to extract authorization code from redirect. ' +
        'Check credentials or see redirect URL: ' + httpResponse.FinalURL;
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
      Exit;
    end;

    httpResponse.Headers.Free;
    httpResponse.Cookies.Free;
    
    // Step 4: Exchange authorization code for tokens
    tokenUrl := GetTokenEndpoint;
    SetLength(params, 5);
    params[0] := 'grant_type=authorization_code';
    params[1] := 'code=' + authCode;
    params[2] := 'client_id=' + GetOidcClientId;
    params[3] := 'redirect_uri=' + GetRedirectUri;
    params[4] := 'code_verifier=' + codeVerifier;
    
    customHeaders.Clear;
    customHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);
    httpResponse := native.RequestEx(true, tokenUrl, params, '', cookieJar, true, 10, customHeaders, false);
    LogMessageToFile(Format('Tandem.Connect: token response success=%s status=%d bytes=%d err=%s',
      [BoolToStr(httpResponse.Success, true), httpResponse.StatusCode, Length(httpResponse.Body), httpResponse.ErrorMessage]));

    tokenOk := (httpResponse.StatusCode >= 200) and (httpResponse.StatusCode < 300) and (Length(httpResponse.Body) > 0);
    if not tokenOk then
    begin
      // Detailed logging to help diagnose token exchange failures (status, body preview, headers, cookies)
      bodyPreview := Copy(httpResponse.Body, 1, 2000);
      headerPreviewLocal := '';
      cookiesCount := 0;
      if httpResponse.Headers <> nil then
      begin
        for i := 0 to httpResponse.Headers.Count - 1 do
        begin
          if i >= 30 then
          begin
            headerPreviewLocal := headerPreviewLocal + '...';
            Break;
          end;
          headerPreviewLocal := headerPreviewLocal + httpResponse.Headers[i] + #13#10;
        end;
      end;
      if httpResponse.Cookies <> nil then
        cookiesCount := httpResponse.Cookies.Count;

      LogMessageToFile(Format('Tandem.Connect: token exchange failed: status=%d success=%s error=%s bytes=%d body_preview=%s headers=%s cookies=%d',
        [httpResponse.StatusCode, BoolToStr(httpResponse.Success, true), httpResponse.ErrorMessage, Length(httpResponse.Body), bodyPreview, headerPreviewLocal, cookiesCount]));

      lastErr := 'Token exchange failed: ' + httpResponse.ErrorMessage;
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
      Exit;
    end;
    
    // Parse token response
    jsonData := GetJSON(httpResponse.Body);
    try
      if jsonData is TJSONObject then
      begin
        jsonObj := TJSONObject(jsonData);
        
        FAccessToken := jsonObj.Get('access_token', '');
        idToken := jsonObj.Get('id_token', '');

        LogMessageToFile(Format('Tandem.Connect: token parsed access_token_len=%d id_token_len=%d',
          [Length(FAccessToken), Length(idToken)]));
        
        if FAccessToken = '' then
        begin
          lastErr := 'No access token received';
          httpResponse.Headers.Free;
          httpResponse.Cookies.Free;
          Exit;
        end;
        
        // Step 5: Extract account info from JWT
        if not ExtractJWT(idToken) then
        begin
          lastErr := 'Failed to extract user info from JWT';
          Exit;
        end;

        LogMessageToFile(Format('Tandem.Connect: JWT pumperId=%s accountId=%s', [FPumperId, FAccountId]));
        
        // Step 6: Select a device
        if not SelectDevice then
        begin
          Exit;
        end;
        
        Result := True;
      end
      else
      begin
        lastErr := 'Invalid token response format';
      end;
    finally
      jsonData.Free;
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
    end;
    
  except
    on E: Exception do
    begin
      lastErr := 'OAuth2 error: ' + E.Message;
      Result := False;
    end;
  end;
  
  cookieJar.Free;
  customHeaders.Free;
end;

{------------------------------------------------------------------------------
  Retrieve glucose readings from Tandem Source
  
  Fetches CGM data from the therapy timeline API and parses pump events
  to extract EGV (estimated glucose value) readings.
 ------------------------------------------------------------------------------}
function Tandem.GetReadings(AMinutes, AMaxCount: integer; AExtras: string;
  out ARes: string): BGResults;
var
  startDate, endDate: TDateTime;
  url: string;
  controliqBase: string;
  controliqUserId: string;
  controliqCandidates: array of string;
  attemptIdx: integer;
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  jsonObj: TJSONObject;
  egvObj: TJSONObject;
  eventObj: TJSONObject;
  i, count: integer;
  bgValue: double;
  eventTime: TDateTime;
  timeStr: string;
  readingsList: array of BGReading;
  rawDiff: double;
  secondsDiff: integer;
  scaledDelta: double;
  authHeaders: TStringList;
  trimmedBody: string;
  previewBody: string;
  requestOk: boolean;
  ws2Response: THTTPResponse;
  ws2UserGuid: string;
  ws2Lines: TStringList;
  ws2Sections: TStringList;
  ws2Section: string;
  ws2Line: string;
  ws2StartDateStr, ws2EndDateStr: string;
  ws2HeaderFields: TStringArray;
  ws2Fields: TStringArray;
  ws2EventIdx, ws2ValueIdx: integer;
  ws2BgStr: string;
  procedure FreeResponse(var AResp: THTTPResponse);
  begin
    if Assigned(AResp.Headers) then
      FreeAndNil(AResp.Headers);
    if Assigned(AResp.Cookies) then
      FreeAndNil(AResp.Cookies);
  end;
  function BuildControlIqUrl(const ABaseWithPath: string; const AUserId: string; const AStart, AEnd: TDateTime): string;
  begin
    Result := ABaseWithPath + 'therapytimeline/users/' + AUserId
      + '?startDate=' + FormatDateTime('mm-dd-yyyy', AStart)
      + '&endDate=' + FormatDateTime('mm-dd-yyyy', AEnd);
  end;
  procedure AddControlIqCandidate(const ABaseUrl, APathPrefix: string);
  var
    idx: integer;
  begin
    if ABaseUrl = '' then
      Exit;
    idx := Length(controliqCandidates);
    SetLength(controliqCandidates, idx + 1);
    controliqCandidates[idx] := ABaseUrl + APathPrefix;
  end;
  function SplitSections(const S: string): TStringList;
  var
    normalized: string;
    parts: TStringArray;
    part: string;
  begin
    Result := TStringList.Create;
    normalized := StringReplace(S, #13#10, #10, [rfReplaceAll]);
    normalized := StringReplace(normalized, #13, #10, [rfReplaceAll]);
    parts := normalized.Split(#10#10);
    for part in parts do
      if Trim(part) <> '' then
        Result.Add(part);
  end;
  function ParseCsvLine(const S: string): TStringArray;
  var
    iPos: integer;
    ch: char;
    inQuote: boolean;
    field: string;
    fieldsList: TStringList;
  begin
    fieldsList := TStringList.Create;
    try
      inQuote := False;
      field := '';
      iPos := 1;
      while iPos <= Length(S) do
      begin
        ch := S[iPos];
        if ch = '"' then
        begin
          if inQuote and (iPos < Length(S)) and (S[iPos + 1] = '"') then
          begin
            field := field + '"';
            Inc(iPos);
          end
          else
            inQuote := not inQuote;
        end
        else if (ch = ',') and (not inQuote) then
        begin
          fieldsList.Add(field);
          field := '';
        end
        else
          field := field + ch;
        Inc(iPos);
      end;
      fieldsList.Add(field);

      SetLength(Result, fieldsList.Count);
      for iPos := 0 to fieldsList.Count - 1 do
        Result[iPos] := fieldsList[iPos];
    finally
      fieldsList.Free;
    end;
  end;
  function ByteAt(const S: string; AOffset: integer): byte;
  var
    idx: integer;
  begin
    idx := AOffset + 1;
    if (idx < 1) or (idx > Length(S)) then
      Result := 0
    else
      Result := Ord(S[idx]);
  end;
  function ReadUInt16BE(const S: string; AOffset: integer): word;
  begin
    Result := (Word(ByteAt(S, AOffset)) shl 8) or Word(ByteAt(S, AOffset + 1));
  end;
  function ReadUInt32BE(const S: string; AOffset: integer): cardinal;
  begin
    Result := (Cardinal(ByteAt(S, AOffset)) shl 24)
      or (Cardinal(ByteAt(S, AOffset + 1)) shl 16)
      or (Cardinal(ByteAt(S, AOffset + 2)) shl 8)
      or Cardinal(ByteAt(S, AOffset + 3));
  end;
  function ExtractPumpEventsPayload(AData: TJSONData; out ARaw: string): boolean;
  var
    dataObj: TJSONObject;
    dataArr: TJSONArray;
    item: TJSONData;
  begin
    Result := False;
    ARaw := '';
    if AData = nil then
      Exit;

    if AData is TJSONString then
    begin
      ARaw := TJSONString(AData).AsString;
      Result := ARaw <> '';
      Exit;
    end;

    if AData is TJSONObject then
    begin
      dataObj := TJSONObject(AData);
      if (dataObj.Find('eventData') <> nil) and (dataObj.Find('eventData') is TJSONString) then
        ARaw := dataObj.Get('eventData', '')
      else if (dataObj.Find('data') <> nil) and (dataObj.Find('data') is TJSONString) then
        ARaw := dataObj.Get('data', '')
      else if (dataObj.Find('pumpEvents') <> nil) and (dataObj.Find('pumpEvents') is TJSONString) then
        ARaw := dataObj.Get('pumpEvents', '')
      else if (dataObj.Find('pumpevents') <> nil) and (dataObj.Find('pumpevents') is TJSONString) then
        ARaw := dataObj.Get('pumpevents', '');

      Result := ARaw <> '';
      Exit;
    end;

    if AData is TJSONArray then
    begin
      dataArr := TJSONArray(AData);
      if dataArr.Count > 0 then
      begin
        item := dataArr.Items[0];
        if item is TJSONString then
        begin
          ARaw := TJSONString(item).AsString;
          Result := ARaw <> '';
        end;
      end;
    end;
  end;
  procedure SortReadingsNewestFirst(var AReadings: BGResults);
  var
    i, j: integer;
    tmp: BGReading;
  begin
    for i := Low(AReadings) to High(AReadings) do
      for j := i + 1 to High(AReadings) do
        if AReadings[j].date > AReadings[i].date then
        begin
          tmp := AReadings[i];
          AReadings[i] := AReadings[j];
          AReadings[j] := tmp;
        end;
  end;
  function TrySourcePumpEventsReadings(out AResults: BGResults): boolean;
  var
    sourceHeaders: TStringList;
    sourceResponse: THTTPResponse;
    sourceUrl: string;
    minDateStr: string;
    maxDateStr: string;
    eventIdsParam: string;
    rawPayload: string;
    decodedEvents: string;
    eventsLen: integer;
    eventOffset: integer;
    eventId: word;
    egvTimestamp: cardinal;
    currentValue: word;
    eventTime: TDateTime;
    resultIdx: integer;
    rawDiff: double;
    secondsDiff: integer;
    scaledDelta: double;
  begin
    Result := False;
    SetLength(AResults, 0);
    sourceResponse.Headers := nil;
    sourceResponse.Cookies := nil;

    if (FPumperId = '') or (FDeviceId = '') then
      Exit;

    if (startDate = 0) or (endDate = 0) then
    begin
      if AMinutes <= 0 then
        AMinutes := 1440;
      endDate := Now;
      startDate := endDate - (AMinutes / 1440.0);
    end;
    minDateStr := FormatDateTime('yyyy-mm-dd', DateOf(startDate));
    maxDateStr := FormatDateTime('yyyy-mm-dd', DateOf(endDate));
    eventIdsParam := StringReplace(TANDEM_SOURCE_DEFAULT_EVENT_IDS, ',', '%2C', [rfReplaceAll]);

    sourceUrl := GetSourceUrl + 'api/reports/reportsfacade/pumpevents/' + FPumperId + '/' + FDeviceId
      + '?minDate=' + minDateStr + '&maxDate=' + maxDateStr + '&eventIds=' + eventIdsParam;

    sourceHeaders := TStringList.Create;
    try
      sourceHeaders.Add('Authorization: Bearer ' + FAccessToken);
      sourceHeaders.Add('Accept: application/json');
      sourceHeaders.Add('Origin: https://tconnect.tandemdiabetes.com');
      sourceHeaders.Add('Referer: https://tconnect.tandemdiabetes.com/');
      sourceHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);

      LogMessageToFile('Tandem.GetReadings: source pumpevents request=' + sourceUrl);
      sourceResponse := native.RequestEx(false, sourceUrl, [], '', nil, true, 10, sourceHeaders, false);
      LogMessageToFile(Format('Tandem.GetReadings: source pumpevents status=%d bytes=%d',
        [sourceResponse.StatusCode, Length(sourceResponse.Body)]));

      if (sourceResponse.StatusCode < 200) or (sourceResponse.StatusCode >= 300) then
        Exit;

      if Trim(sourceResponse.Body) = '' then
        Exit;

      jsonData := GetJSON(sourceResponse.Body);
      try
        if not ExtractPumpEventsPayload(jsonData, rawPayload) then
        begin
          LogMessageToFile('Tandem.GetReadings: source pumpevents missing eventData payload');
          Exit;
        end;
      finally
        jsonData.Free;
      end;

      if rawPayload = '' then
        Exit;

      decodedEvents := DecodeStringBase64(rawPayload);
      eventsLen := Length(decodedEvents);
      if eventsLen < 26 then
        Exit;

      SetLength(readingsList, 0);
      count := 0;
      eventOffset := 0;
      while eventOffset + 25 <= eventsLen do
      begin
        eventId := ReadUInt16BE(decodedEvents, eventOffset) and $0FFF;
        if (eventId = 256) or (eventId = 372) or (eventId = 399) then
        begin
          currentValue := ReadUInt16BE(decodedEvents, eventOffset + 14);
          egvTimestamp := ReadUInt32BE(decodedEvents, eventOffset + 18);
          eventTime := UnixToDateTime(Int64(TANDEM_EPOCH) + Int64(egvTimestamp), True);
          if (eventTime >= startDate) and (eventTime <= (endDate + 1)) then
          begin
            SetLength(readingsList, count + 1);
            readingsList[count].Init(mgdl, self.systemName);
            readingsList[count].update(currentValue, 0);
            readingsList[count].date := eventTime;
            readingsList[count].trend := TdPlaceholder;
            readingsList[count].level := getLevel(currentValue);
            Inc(count);
          end;
        end;
        Inc(eventOffset, 26);
      end;

      if count > 0 then
      begin
        SetLength(AResults, count);
        for resultIdx := 0 to count - 1 do
          AResults[resultIdx] := readingsList[resultIdx];
        SortReadingsNewestFirst(AResults);
        if FCalcDiff and (Length(AResults) > 1) then
        begin
          for resultIdx := 0 to High(AResults) do
          begin
            if resultIdx < High(AResults) then
            begin
              // Raw difference between consecutive readings (mg/dL)
              rawDiff := AResults[resultIdx].convert(mgdl) - AResults[resultIdx + 1].convert(mgdl);

              // Compute time between samples in seconds
              secondsDiff := Round((AResults[resultIdx].date - AResults[resultIdx + 1].date) * 86400);

              // Store the actual observed delta (rawDiff) in BGDelta
              AResults[resultIdx].update(rawDiff, BGDelta, mgdl);

              // Normalize delta to a 5 minute window (300s) for trend calculation
              if (secondsDiff >= 60) and (secondsDiff <= 900) then
              begin
                scaledDelta := rawDiff * (300 / secondsDiff);
                AResults[resultIdx].trend := CalculateTrendFromDelta(scaledDelta);
              end
              else
                AResults[resultIdx].trend := TdNotComputable;
            end
            else
            begin
              AResults[resultIdx].update(0, BGDelta, mgdl);
              AResults[resultIdx].trend := TdFlat;
            end;
          end;
        end
        else
        begin
          for resultIdx := 0 to High(AResults) do
            AResults[resultIdx].trend := TdFlat;
        end;
        for resultIdx := 0 to Min(Length(AResults) - 1, 4) do
          LogMessageToFile(Format('Tandem.GetReadings: source pumpevents top[%d]=%s val=%.1f',
            [resultIdx, FormatDateTime('yyyy-mm-dd hh:nn', AResults[resultIdx].date),
             AResults[resultIdx].convert(mmol)]));
        if (AMaxCount > 0) and (Length(AResults) > AMaxCount) then
          SetLength(AResults, AMaxCount);
        Result := True;
      end;
    finally
      sourceHeaders.Free;
      if Assigned(sourceResponse.Headers) then
        FreeAndNil(sourceResponse.Headers);
      if Assigned(sourceResponse.Cookies) then
        FreeAndNil(sourceResponse.Cookies);
    end;
  end;
  function TryWs2CsvReadings(out AResults: BGResults): boolean;
  var
    ws2Url: string;
    ws2Base: string;
    ws2AltBase: string;
    ws2Headers: TStringList;
    ws2DateFormats: array[0..1] of string;
    fmtIdx: integer;
    lineParts: TStringArray;
    lineCount: integer;
    sectionIdx: integer;
    headerIdx: integer;
    resultIdx: integer;
    lineIdx: integer;
    function AttemptWs2(const ABase, AId: string): boolean;
    begin
      Result := False;
      if AId = '' then
        Exit;
      ws2Url := ABase + 'therapytimeline2csv/' + AId + '/' +
        ws2StartDateStr + '/' + ws2EndDateStr + '?format=csv';
      LogMessageToFile('Tandem.GetReadings: ws2 request=' + ws2Url);
      ws2Response := native.RequestEx(false, ws2Url, [], '', nil, true, 10, ws2Headers, false);
      if (ws2Response.StatusCode >= 200) and (ws2Response.StatusCode < 300) then
      begin
        Result := True;
        Exit;
      end;
      LogMessageToFile('Tandem.GetReadings: ws2 status=' + IntToStr(ws2Response.StatusCode));
      if Trim(ws2Response.Body) <> '' then
        LogMessageToFile('Tandem.GetReadings: ws2 error body prefix=' + Copy(ws2Response.Body, 1, 200));
    end;
    procedure FreeWs2Response;
    begin
      if Assigned(ws2Response.Headers) then
        FreeAndNil(ws2Response.Headers);
      if Assigned(ws2Response.Cookies) then
        FreeAndNil(ws2Response.Cookies);
    end;
    function AttemptWs2Base(const ABase: string): boolean;
    begin
      Result := False;
      if AttemptWs2(ABase, FControlIqUserGuid) then
        Exit(True);
      FreeWs2Response;
      if AttemptWs2(ABase, FAccountId) then
        Exit(True);
      FreeWs2Response;
      if AttemptWs2(ABase, FPumperId) then
        Exit(True);
      FreeWs2Response;
      if (FDeviceId <> '') and AttemptWs2(ABase, FDeviceId) then
        Exit(True);
      FreeWs2Response;
    end;
  begin
    Result := False;
    SetLength(AResults, 0);
    ws2Response.Headers := nil;
    ws2Response.Cookies := nil;

    if (FControlIqUserGuid = '') and (FAccountId = '') and (FPumperId = '') and (FDeviceId = '') then
      Exit;

    ws2DateFormats[0] := 'mm-dd-yyyy';
    ws2DateFormats[1] := 'yyyy-mm-dd';
    ws2Base := 'https://tconnectws2.tandemdiabetes.com/';
    ws2AltBase := '';

    ws2Headers := TStringList.Create;
    try
      ws2Headers.Add('Accept: text/csv');
      ws2Headers.Add('Origin: https://tconnect.tandemdiabetes.com');
      ws2Headers.Add('Referer: https://tconnect.tandemdiabetes.com/');
      ws2Headers.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);

      for fmtIdx := 0 to High(ws2DateFormats) do
      begin
        ws2StartDateStr := FormatDateTime(ws2DateFormats[fmtIdx], startDate);
        ws2EndDateStr := FormatDateTime(ws2DateFormats[fmtIdx], endDate);
        if AttemptWs2Base(ws2Base) then
          Break;
      end;

      if (ws2Response.StatusCode < 200) or (ws2Response.StatusCode >= 300) then
        Exit;
      if Trim(ws2Response.Body) = '' then
        Exit;

      ws2Sections := SplitSections(ws2Response.Body);
      try
        ws2Section := '';
        for sectionIdx := 0 to ws2Sections.Count - 1 do
        begin
          ws2Line := Trim(ws2Sections[sectionIdx]);
          if (ws2Line <> '') then
          begin
            ws2Lines := TStringList.Create;
            try
              ws2Lines.Text := StringReplace(ws2Sections[sectionIdx], #13#10, #10, [rfReplaceAll]);
              if (ws2Lines.Count >= 2) and (Pos('DeviceType', ws2Lines[0]) > 0) then
              begin
                ws2Section := ws2Sections[sectionIdx];
                Break;
              end;
            finally
              ws2Lines.Free;
            end;
          end;
        end;

        if ws2Section = '' then
          Exit;

        ws2Lines := TStringList.Create;
        try
          ws2Lines.Text := StringReplace(ws2Section, #13#10, #10, [rfReplaceAll]);
          if ws2Lines.Count < 2 then
            Exit;

          ws2HeaderFields := ParseCsvLine(ws2Lines[0]);
          ws2EventIdx := -1;
          ws2ValueIdx := -1;
          for headerIdx := 0 to High(ws2HeaderFields) do
          begin
            if Trim(ws2HeaderFields[headerIdx]) = 'EventDateTime' then
              ws2EventIdx := headerIdx
            else if Trim(ws2HeaderFields[headerIdx]) = 'Readings (CGM / BGM)' then
              ws2ValueIdx := headerIdx;
          end;

          if (ws2EventIdx < 0) or (ws2ValueIdx < 0) then
            Exit;

          SetLength(readingsList, 0);
          count := 0;
          lineCount := ws2Lines.Count;
          for lineIdx := 1 to lineCount - 1 do
          begin
            ws2Line := Trim(ws2Lines[lineIdx]);
            if ws2Line = '' then
              Continue;
            lineParts := ParseCsvLine(ws2Line);
            if (ws2EventIdx > High(lineParts)) or (ws2ValueIdx > High(lineParts)) then
              Continue;

            timeStr := StringReplace(lineParts[ws2EventIdx], '"', '', [rfReplaceAll]);
            ws2BgStr := StringReplace(lineParts[ws2ValueIdx], '"', '', [rfReplaceAll]);
            ws2BgStr := StringReplace(ws2BgStr, ',', '', [rfReplaceAll]);
            if not TryStrToFloat(ws2BgStr, bgValue) then
              Continue;

            try
              eventTime := ISO8601ToDate(timeStr);
            except
              Continue;
            end;

            SetLength(readingsList, count + 1);
            readingsList[count].Init(mgdl, self.systemName);
            readingsList[count].update(bgValue, 0);
            readingsList[count].date := eventTime;
            readingsList[count].trend := TdPlaceholder;
            readingsList[count].level := getLevel(bgValue);
            Inc(count);

            if (AMaxCount > 0) and (count >= AMaxCount) then
              Break;
          end;

          if count > 0 then
          begin
            SetLength(AResults, count);
            for resultIdx := 0 to count - 1 do
              AResults[resultIdx] := readingsList[resultIdx];
            Result := True;
          end;
        finally
          ws2Lines.Free;
        end;
      finally
        ws2Sections.Free;
      end;
    finally
      ws2Headers.Free;
      FreeWs2Response;
    end;
  end;
begin
  SetLength(Result, 0);
  ARes := '';
  
  if FAccessToken = '' then
  begin
    lastErr := 'Not authenticated. Call Connect first.';
    ARes := lastErr;
    Exit;
  end;
  
  if FDeviceId = '' then
  begin
    lastErr := 'No device selected';
    ARes := lastErr;
    Exit;
  end;
  
  authHeaders := TStringList.Create;
  try
    // Calculate time range
    if AMinutes <= 0 then
      AMinutes := 1440;
    endDate := Now;
    startDate := endDate - (AMinutes / 1440.0); // Convert minutes to days

    if (FControlIqAccessToken = '') or (FControlIqUserGuid = '') then
    begin
      if not ControlIqLogin then
      begin
        if TrySourcePumpEventsReadings(Result) then
        begin
          ARes := 'Retrieved ' + IntToStr(Length(Result)) + ' CGM readings (Tandem Source pumpevents)';
          Exit;
        end;
        ARes := lastErr;
        Exit;
      end;
    end;

    authHeaders.Add('Authorization: Bearer ' + FControlIqAccessToken);
    authHeaders.Add('Accept: application/json');
    authHeaders.Add('Origin: https://tconnect.tandemdiabetes.com');
    authHeaders.Add('Referer: https://tconnect.tandemdiabetes.com/');
    authHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);
  
    // Build Control-IQ therapy timeline request (matches tconnectsync)
    controliqUserId := FControlIqUserGuid;
    if controliqUserId = '' then
      controliqUserId := FPumperId;
    if controliqUserId = '' then
      controliqUserId := FAccountId;

    LogMessageToFile('Tandem.GetReadings: controliqUserId=' + controliqUserId);

    SetLength(controliqCandidates, 0);
    controliqBase := GetControlIqBaseUrl;
    AddControlIqCandidate(controliqBase, 'tconnect/controliq/api/');

    requestOk := False;
    for attemptIdx := 0 to High(controliqCandidates) do
    begin
      url := BuildControlIqUrl(controliqCandidates[attemptIdx], controliqUserId, startDate, endDate);
      LogMessageToFile('Tandem.GetReadings: request=' + url);
      httpResponse := native.RequestEx(false, url, [], '', nil, true, 10, authHeaders, false);
      LogMessageToFile(Format('Tandem.GetReadings: status=%d bytes=%d',
        [httpResponse.StatusCode, Length(httpResponse.Body)]));

      if httpResponse.StatusCode = 500 then
      begin
        FreeResponse(httpResponse);
        httpResponse := native.RequestEx(false, url, [], '', nil, true, 10, authHeaders, false);
        LogMessageToFile(Format('Tandem.GetReadings: retry status=%d bytes=%d',
          [httpResponse.StatusCode, Length(httpResponse.Body)]));
      end;

      if httpResponse.StatusCode = 401 then
      begin
        FControlIqAccessToken := '';
        FControlIqUserGuid := '';
        if ControlIqLogin then
        begin
          authHeaders[0] := 'Authorization: Bearer ' + FControlIqAccessToken;
          LogMessageToFile('Tandem.GetReadings: retry after Control-IQ login');
          FreeResponse(httpResponse);
          httpResponse := native.RequestEx(false, url, [], '', nil, true, 10, authHeaders, false);
          LogMessageToFile(Format('Tandem.GetReadings: status=%d bytes=%d',
            [httpResponse.StatusCode, Length(httpResponse.Body)]));
        end;
      end;

      requestOk := (httpResponse.StatusCode >= 200) and (httpResponse.StatusCode < 300);
      if requestOk then
      begin
        trimmedBody := TrimLeft(httpResponse.Body);
        if (trimmedBody <> '') and (trimmedBody[1] = '<') then
        begin
          LogMessageToFile('Tandem.GetReadings: HTML response; skipping candidate');
          requestOk := False;
        end
        else
          Break;
      end;

      if (httpResponse.StatusCode <> 404) and (httpResponse.StatusCode <> 401) then
        Break;

      FreeResponse(httpResponse);
    end;

    if not requestOk then
    begin
      if TrySourcePumpEventsReadings(Result) then
      begin
        ARes := 'Retrieved ' + IntToStr(Length(Result)) + ' CGM readings (Tandem Source pumpevents)';
        Exit;
      end;
      if TryWs2CsvReadings(Result) then
      begin
        ARes := 'Retrieved ' + IntToStr(Length(Result)) + ' CGM readings (ws2 CSV)';
        Exit;
      end;
      if httpResponse.Body <> '' then
      begin
        previewBody := Copy(httpResponse.Body, 1, 200);
        previewBody := StringReplace(previewBody, #13, ' ', [rfReplaceAll]);
        previewBody := StringReplace(previewBody, #10, ' ', [rfReplaceAll]);
        LogMessageToFile('Tandem.GetReadings: error body prefix=' + previewBody);
      end;
      lastErr := 'Failed to get readings: HTTP ' + IntToStr(httpResponse.StatusCode);
      ARes := lastErr;
      FreeResponse(httpResponse);
      Exit;
    end;
    
    if httpResponse.Body = '' then
    begin
      lastErr := 'Empty response from Tandem API';
      ARes := lastErr;
      FreeResponse(httpResponse);
      Exit;
    end;

    trimmedBody := Trim(httpResponse.Body);
    if (trimmedBody = '[]') or (trimmedBody = '{}') or (trimmedBody = 'null') then
    begin
      // No pump events in the requested time range
      lastErr := '';
      ARes := 'No CGM readings found in specified time range';
      FreeResponse(httpResponse);
      Exit;
    end;

    if (Length(trimmedBody) > 0) and not (trimmedBody[1] in ['[', '{']) then
    begin
      previewBody := Copy(httpResponse.Body, 1, 80);
      previewBody := StringReplace(previewBody, #13, ' ', [rfReplaceAll]);
      previewBody := StringReplace(previewBody, #10, ' ', [rfReplaceAll]);
      LogMessageToFile('Tandem.GetReadings: non-JSON response prefix=' + previewBody);
      lastErr := 'Unsupported pump events response format';
      ARes := lastErr;
      FreeResponse(httpResponse);
      Exit;
    end;
    
    // Parse therapy events JSON
    jsonData := GetJSON(httpResponse.Body);
    try
      jsonArray := nil;
      if jsonData is TJSONObject then
      begin
        jsonObj := TJSONObject(jsonData);
        if (jsonObj.Find('event') <> nil) and (jsonObj.Find('event') is TJSONArray) then
          jsonArray := TJSONArray(jsonObj.Find('event'));
      end
      else if jsonData is TJSONArray then
        jsonArray := TJSONArray(jsonData);

      if jsonArray = nil then
      begin
        lastErr := 'Invalid response format: expected event array';
        ARes := lastErr;
        Exit;
      end;
      SetLength(readingsList, 0);
      count := 0;
      
      // Iterate through therapy events looking for CGM data
      for i := 0 to jsonArray.Count - 1 do
      begin
        if not (jsonArray[i] is TJSONObject) then
          Continue;
        
        eventObj := TJSONObject(jsonArray[i]);
        
        // Look for CGM events with EGV data
        if eventObj.Find('type') = nil then
          Continue;

        if eventObj.Get('type', '') = 'CGM' then
        begin
          if (eventObj.Find('egv') <> nil) and (eventObj.Find('egv') is TJSONObject) then
          begin
            egvObj := TJSONObject(eventObj.Find('egv'));
            if egvObj.Find('estimatedGlucoseValue') <> nil then
            begin
              bgValue := egvObj.Get('estimatedGlucoseValue', 0.0);

              // Extract timestamp
              if eventObj.Find('eventDateTime') <> nil then
              begin
                timeStr := eventObj.Get('eventDateTime', '');
                eventTime := ISO8601ToDate(timeStr);

                // Skip if outside our time range
                if (eventTime < startDate) or (eventTime > (endDate + 1)) then
                  Continue;

                // Add reading
                SetLength(readingsList, count + 1);
                readingsList[count].Init(mgdl, self.systemName);
                readingsList[count].update(bgValue, 0); // Value in mg/dL, no delta
                readingsList[count].date := eventTime;
                readingsList[count].trend := TdPlaceholder; // Tandem doesn't provide trend arrows
                readingsList[count].level := getLevel(bgValue);
                Inc(count);
              end;
            end;
          end;
        end;
      end;
      
      // Copy to result array
      SetLength(Result, count);
      for i := 0 to count - 1 do
        Result[i] := readingsList[i];
      if count > 1 then
        SortReadingsNewestFirst(Result);
      if FCalcDiff and (Length(Result) > 1) then
      begin
        for i := 0 to High(Result) do
        begin
          if i < High(Result) then
          begin
            rawDiff := Result[i].convert(mgdl) - Result[i + 1].convert(mgdl);
            secondsDiff := Round((Result[i].date - Result[i + 1].date) * 86400);
            Result[i].update(rawDiff, BGDelta, mgdl);
            if (secondsDiff >= 60) and (secondsDiff <= 900) then
            begin
              scaledDelta := rawDiff * (300 / secondsDiff);
              Result[i].trend := CalculateTrendFromDelta(scaledDelta);
            end
            else
              Result[i].trend := TdNotComputable;
          end
          else
          begin
            Result[i].update(0, BGDelta, mgdl);
            Result[i].trend := TdFlat;
          end;
        end;
      end
      else
      begin
        for i := 0 to High(Result) do
          Result[i].trend := TdFlat;
      end;
      if (AMaxCount > 0) and (Length(Result) > AMaxCount) then
        SetLength(Result, AMaxCount);
      
      if count > 0 then
        ARes := 'Retrieved ' + IntToStr(count) + ' CGM readings'
      else
      begin
        lastErr := '';
        ARes := 'No CGM readings found in specified time range';
      end;
        
    finally
      jsonData.Free;
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
    end;
    
  except
    on E: Exception do
    begin
      lastErr := 'Error retrieving readings: ' + E.Message;
      ARes := lastErr;
      SetLength(Result, 0);
    end;
  end;
  
  authHeaders.Free;
end;

{------------------------------------------------------------------------------
  Parameter labels for UI
 ------------------------------------------------------------------------------}
class function Tandem.ParamLabel(LabelName: APIParamLabel): string;
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
class function Tandem.testConnection(AEmail, APass: string; var ARes: string; AExtra: string): MaybeBool;
var
  api: Tandem;
  region: TTandemRegion;
begin
  Result := MaybeBool.none;
  
  if UpperCase(Trim(AExtra)) = 'EU' then
    region := trEU
  else
    region := trUS;
  
  api := Tandem.Create(AEmail, APass, region);
  try
    if api.Connect then
    begin
      ARes := 'Successfully connected to Tandem t:connect';
      Result := MaybeBool.true;
    end
    else
    begin
      ARes := api.lastErr;
      Result := MaybeBool.false;
    end;
  finally
    api.Free;
  end;
end;

{------------------------------------------------------------------------------
  Get limit values
 ------------------------------------------------------------------------------}
function Tandem.getLimitHigh: integer;
begin
  Result := 400; // Tandem typically shows up to 400 mg/dL
end;

function Tandem.getLimitLow: integer;
begin
  Result := 40; // Tandem typically shows down to 40 mg/dL
end;

end.
