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
  trndi.types, trndi.api, trndi.native.base, trndi.funcs, trndi.log, math,
  // FPC units
  fpjson, jsonparser, dateutils, StrUtils, base64, slicke.sha256;

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
  TANDEM_OIDC_CLIENT_ID_US = '0oa4wnbvtladeyVZX4h7';
  TANDEM_OIDC_ISSUER_US = 'https://tdcservices.tandemdiabetes.com/accounts/api';
  TANDEM_OIDC_JWKS_URL_US = 'https://tdcservices.tandemdiabetes.com/accounts/api/.well-known/openid-configuration/jwks';
  TANDEM_TOKEN_ENDPOINT_US = 'https://tdcservices.tandemdiabetes.com/accounts/api/connect/token';
  TANDEM_AUTHORIZATION_ENDPOINT_US = 'https://tdcservices.tandemdiabetes.com/accounts/api/connect/authorize';
  TANDEM_REDIRECT_URI_US = 'https://sso.tandemdiabetes.com/auth/callback';

  {** EU Region URLs }
  TANDEM_LOGIN_API_URL_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/login';
  TANDEM_SOURCE_URL_EU = 'https://source.eu.tandemdiabetes.com/';
  TANDEM_OIDC_CLIENT_ID_EU = '1519e414-eeec-492e-8c5e-97bea4815a10';
  TANDEM_OIDC_ISSUER_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api';
  TANDEM_OIDC_JWKS_URL_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/.well-known/openid-configuration/jwks';
  TANDEM_TOKEN_ENDPOINT_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/connect/token';
  TANDEM_AUTHORIZATION_ENDPOINT_EU = 'https://tdcservices.eu.tandemdiabetes.com/accounts/api/connect/authorize';
  TANDEM_REDIRECT_URI_EU = 'https://source.eu.tandemdiabetes.com/authorize/callback';

  TANDEM_BASE_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.63 Safari/537.36';
  {** Event-id filter for the BFF pump-logs endpoint; mirrors the Tandem Source
      web app's getLogIDList() as used by tconnectsync v3 }
  TANDEM_SOURCE_DEFAULT_EVENT_IDS = '229,5,28,4,26,99,279,3,16,59,21,55,20,280,64,65,66,61,33,371,171,369,460,172,370,461,372,480,399,256,213,406,477,394,212,404,214,405,486,447,313,60,14,6,90,230,140,12,11,53,13,63,203,307,191';
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
    FDeviceId: string;      /// The selected pump device id (BFF assignmentId UUID)
    FPumperId: string;      /// Pumper ID from JWT
    FAccountId: string;     /// Account ID from JWT
    FCalcDiff: boolean;     /// If True, compute deltas between consecutive readings

    FBoluses: TBolusList;   /// Insulin deliveries from the last fetch, oldest first
    FBolusesValid: boolean; /// True once a payload has been walked
    FCarbs: TCarbList;      /// Carbohydrate entries from the last fetch, oldest first
    FCarbsValid: boolean;   /// True once a payload has been walked
    FDeviceStatus: TCGMDeviceStatus; /// Pump housekeeping from the last fetch
    FDeviceStatusValid: boolean;     /// True once a payload has filled it
    FBasalRate: single;     /// Last rate the pump was commanded to run; <0 unknown
    FBasalAt: TDateTime;    /// When that command was recorded; 0 unknown
    FBasalSource: integer;  /// eventCode the commanded rate came from; <0 unknown
    FProfileRate: single;   /// Programmed profile rate beside it; <0 unknown
    FProfileRateAt: TDateTime; /// When the profile rate was recorded; 0 unknown

    {** Get region-specific URLs }
    function GetLoginApiUrl: string;
    function GetSourceUrl: string;
    function GetOidcClientId: string;
    function GetOidcIssuer: string;
    function GetOidcJwksUrl: string;
    function GetTokenEndpoint: string;
    function GetAuthorizationEndpoint: string;
    function GetRedirectUri: string;

    {** Generate PKCE code verifier and challenge for OAuth2 }
    function GenerateCodeVerifier: string;
    function GenerateCodeChallenge(const AVerifier: string): string;

    {** Parse JWT token to extract claims }
    function ExtractJWT(const AIdToken: string): boolean;

    {** Select a device from available pumps }
    function SelectDevice: boolean;

    {** Extract authorization code from redirect URL }
    function ExtractAuthCodeFromURL(const AURL: string): string;

    {** Log what a pump-logs payload actually contains, for working out the
        BFF's property names. Debug builds only, since @code(log) is a no-op
        elsewhere. }
    procedure LogEventCensus(AEvents: TJSONArray);

    {** Forget both basal rates, so a payload that carries neither cannot leave
        the previous fetch's figures reading as current. }
    procedure clearBasalCache;

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
      out ARes: string; noCache: boolean): BGResults; override;

    {** UI parameter label provider (override).
        1: Tandem Email
        2: Tandem Password
        3: Region (use "US" for US servers, "EU" for EU servers)
    }
    class function ParamLabel(LabelName: APIParamLabel): string; override;

    {** Test connection for Tandem Source API }
    class function testConnection(AEmail, APass: string; var ARes: string; AExtra: string): MaybeBool; overload;

    {** Insulin deliveries the pump recorded over the window the last fetch
        covered, assembled from the bolus event family. }
    function getBoluses(out ABoluses: TBolusList): boolean; override;

    {** True: the pump-logs payload carries insulin deliveries. }
    function supportsBoluses: boolean; override;

    {** Carbohydrates entered into the bolus calculator over the window the
        last fetch covered. }
    function getCarbs(out ACarbs: TCarbList): boolean; override;

    {** True: the pump-logs payload carries carbohydrate entries. }
    function supportsCarbs: boolean; override;

    {** Reservoir, battery and suspend state as of the last event in the window
        the last fetch covered. }
    function getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean; override;

    {** The last basal rate Control-IQ commanded, in U/hr. This is what the
        pump was told to run, not the programmed profile rate. }
    function getBasalRate: single; override;

    {** Both rates the pump-logs payload reports -- what Control-IQ commanded
        and the programmed profile rate beside it -- with the time and event
        code they came from. }
    function getBasalStatus(out AStatus: TBasalStatus): boolean; override;

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
    {** Fill the bolus, carbohydrate, pump-status and basal caches from the
        non-CGM events of a pump-logs payload. Replaces whatever the previous
        fetch left, so it must only run on a payload that actually parsed.

        Protected rather than private so the tests can drive it from a fixture
        payload; reaching the real one would otherwise mean standing up the
        whole OIDC flow against a fake server. }
    procedure ExtractTreatments(AEvents: TJSONArray);

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
    constructor Create(AEmail, APass: string); override; overload;
    constructor Create(const AEmail, APass: string; ACalcDiff: boolean); reintroduce; overload;
    class function testConnection(AEmail, APass: string; var ARes: string): MaybeBool; override; overload;
  end;

  TandemEU = class(Tandem)
  protected
    function getSystemName: string; override;
  public
    constructor Create(AEmail, APass: string); override; overload;
    constructor Create(const AEmail, APass: string; ACalcDiff: boolean); reintroduce; overload;
    class function testConnection(AEmail, APass: string; var ARes: string): MaybeBool; override; overload;
  end;

implementation

resourcestring
  sErrTandemPass = 'Invalid Tandem t:connect credentials';
  sErrTandemLogin = 'Login error: Could not establish a valid session';
  sErrTandemNoDevice = 'No pump device found or selected';
  sParamUserName = 'Tandem Email';
  sParamPassword = 'Tandem Password';
  sParamDesc =
    '** BETA DRIVER - It may not work as intended; check the t:connect app '
    + 'before acting on a reading! **' + LineEnding + LineEnding +
    'Tandem t:connect region selection:'#13#10''#13#10'' +
    'Choose the server based on your account region:' + LineEnding +
    '• Tandem (USA): for US accounts' + LineEnding +
    '• Tandem (EU): for European accounts' +
    LineEnding + LineEnding +
    'Your credentials are your Tandem t:connect account email and password.' +
    LineEnding + LineEnding +
    'Note: some accounts only get new readings from Tandem Source once an hour. '
    + 'Trndi then keeps showing the last reading it received, and may report it as '
    + 'outdated. This is a Tandem Source limitation, not a Trndi bug.';
  sParamDescHTML =
    '<div style="background: #dc3545; color: white; padding: 15px; border-radius: 6px; margin-bottom: 20px; font-weight: bold; text-align: center; border: 2px solid #c82333;">'
    + '⚠️ BETA DRIVER - It may not work as intended; check the t:connect app before acting on a reading! ⚠️'
    + '</div>' +
    '<b>Tandem t:connect</b> region selection:<br><br>'+
    'Choose the server based on your <u>account region</u>:<br>' +
    '• Tandem (USA): for US accounts<br>' +
    '• Tandem (EU): for European accounts' +
    '<br><br>' +
    'Your credentials are your Tandem t:connect account email and password.' +
    '<br><br>' +
    '<b>Note:</b> some accounts only get new readings from Tandem Source <u>once an hour</u>. '
    + 'Trndi then keeps showing the last reading it received, and may report it as '
    + 'outdated. This is a Tandem Source limitation, not a Trndi bug.';

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
  i: integer;
  keyName: string;
  keyList: string;
  valueStr: string;
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
            log('Tandem.Connect: JWT id claim ' + keyName + '=' + MaskValue(valueStr));
        end;
        if keyList <> '' then
          log('Tandem.Connect: JWT claims=' + keyList);
        
        // Extract claims
        if jsonObj.Find('pumperId') <> nil then
          FPumperId := jsonObj.Get('pumperId', '');
        
        if jsonObj.Find('accountId') <> nil then
          FAccountId := jsonObj.Get('accountId', '');

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

  Uses the Tandem Source BFF pumper endpoint (replaces the retired
  reportsfacade pumpeventmetadata endpoint). The device id is the pump's
  assignmentId UUID, which the pump-logs endpoint expects.
 ------------------------------------------------------------------------------}
function Tandem.SelectDevice: boolean;
var
  httpResponse: THTTPResponse;
  jsonData: TJSONData;
  pumpsArray: TJSONArray;
  jsonObj, pumpObj: TJSONObject;
  i: integer;
  assignmentId: string;
  fallbackId: string;
  pumpDate: string;
  bestDate: string;
  authHeaders: TStringList;
begin
  Result := False;
  FDeviceId := '';
  authHeaders := TStringList.Create;

  try
    authHeaders.Add('Authorization: Bearer ' + FAccessToken);
    authHeaders.Add('Accept: application/json');
    // The WAF requires same-origin Origin/Referer matching the Source host
    authHeaders.Add('Origin: ' + TrimRightSet(GetSourceUrl, ['/']));
    authHeaders.Add('Referer: ' + GetSourceUrl);
    authHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);

    // Get the pumper profile including the pumps on the account
    httpResponse := native.RequestExWait(false, GetSourceUrl + 'api/reports/bff/pumper/' + FPumperId,
      [], '', nil, true, 10, authHeaders, false);

    log(Format('Tandem.SelectDevice: status=%d bytes=%d',
      [httpResponse.StatusCode, Length(httpResponse.Body)]));

    if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode >= 300) then
    begin
      if Length(httpResponse.Body) > 0 then
        log('Tandem.SelectDevice: error body prefix=' + Copy(httpResponse.Body, 1, 200));
      lastErr := 'Failed to get device list: HTTP ' + IntToStr(httpResponse.StatusCode);
      httpResponse.Headers.Free;
      httpResponse.Cookies.Free;
      Exit;
    end;

    jsonData := GetJSON(httpResponse.Body);
    try
      pumpsArray := nil;
      if jsonData is TJSONObject then
      begin
        jsonObj := TJSONObject(jsonData);
        if jsonObj.Find('pumps') is TJSONArray then
          pumpsArray := TJSONArray(jsonObj.Find('pumps'));
      end;

      if (pumpsArray = nil) or (pumpsArray.Count = 0) then
      begin
        lastErr := sErrTandemNoDevice;
        Exit;
      end;

      log(Format('Tandem.SelectDevice: pump count=%d', [pumpsArray.Count]));

      // Select the pump with the most recent data. maxDateOfEvents is an
      // ISO-8601 timestamp, so string comparison orders correctly; pumps
      // that never uploaded lack it and are only used as a last resort.
      bestDate := '';
      fallbackId := '';
      for i := 0 to pumpsArray.Count - 1 do
      begin
        if not (pumpsArray[i] is TJSONObject) then
          Continue;
        pumpObj := TJSONObject(pumpsArray[i]);
        assignmentId := pumpObj.Get('assignmentId', '');
        if assignmentId = '' then
          Continue;
        if fallbackId = '' then
          fallbackId := assignmentId;
        pumpDate := pumpObj.Get('maxDateOfEvents', '');
        if (pumpDate <> '') and ((bestDate = '') or (pumpDate > bestDate)) then
        begin
          bestDate := pumpDate;
          FDeviceId := assignmentId;
        end;
      end;
      if FDeviceId = '' then
        FDeviceId := fallbackId;

      log('Tandem.SelectDevice: selected assignmentId=' + FDeviceId + ' lastEvents=' + bestDate);
      Result := FDeviceId <> '';
      if not Result then
        lastErr := sErrTandemNoDevice;
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

  // Nothing fetched yet: the accessors must report "not reported" rather than
  // an empty delivery list, which would read as "no insulin given".
  FBolusesValid := false;
  SetLength(FBoluses, 0);
  FCarbsValid := false;
  SetLength(FCarbs, 0);
  FDeviceStatusValid := false;
  clearDeviceStatus(FDeviceStatus);
  clearBasalCache;

  baseUrl := GetSourceUrl;
  
  // Parent ctor sets timezone, allocates native helper, and initializes thresholds
  inherited Create(AEmail, APassword);
end;

{------------------------------------------------------------------------------
  Region-specific constructors
 ------------------------------------------------------------------------------}
constructor TandemUSA.Create(AEmail, APass: string);
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

constructor TandemEU.Create(AEmail, APass: string);
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
  try
    log('Tandem.Connect: start');
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
    httpResponse := native.RequestExWait(false, TANDEM_LOGIN_PAGE_URL, [], '', cookieJar, true, 10, customHeaders, false);
    log(Format('Tandem.Connect: login page success=%s status=%d bytes=%d err=%s',
      [BoolToStr(httpResponse.Success, true), httpResponse.StatusCode, Length(httpResponse.Body), httpResponse.ErrorMessage]));
    
    // Free the SSO priming response before reassigning
    if httpResponse.Headers <> nil then
      httpResponse.Headers.Free;
    if httpResponse.Cookies <> nil then
      httpResponse.Cookies.Free;

    // Step 2: Login (POST credentials as JSON) with cookie jar
    loginUrl := GetLoginApiUrl;
    loginJson := TJSONObject.Create;
    try
      loginJson.Add('username', FEmail);
      loginJson.Add('password', FPassword);

      customHeaders.Clear;
      customHeaders.Add('Referer: ' + TANDEM_LOGIN_PAGE_URL);
      customHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);
      httpResponse := native.RequestExWait(true, loginUrl, [], loginJson.AsJSON, cookieJar, true, 10, customHeaders, false);
    finally
      loginJson.Free;
    end;
    log(Format('Tandem.Connect: login response success=%s status=%d bytes=%d err=%s',
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
    log(Format('Tandem.Connect: loginOk=%s statusVal=%s',
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
    httpResponse := native.RequestExWait(false, authUrl, [], '', cookieJar, true, 10, customHeaders, false);
    log(Format('Tandem.Connect: auth response success=%s status=%d finalUrl=%s err=%s',
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
    log(Format('Tandem.Connect: auth code length=%d', [Length(authCode)]));
    
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
        log('Tandem.Connect: auth response headers (preview):'#13#10 + headerPreview);
      end
      else
        log('Tandem.Connect: auth response headers (preview): <nil>');
      if locationHeader <> '' then
        log('Tandem.Connect: auth response Location=' + locationHeader);
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
    httpResponse := native.RequestExWait(true, tokenUrl, params, '', cookieJar, true, 10, customHeaders, false);
    log(Format('Tandem.Connect: token response success=%s status=%d bytes=%d err=%s',
      [BoolToStr(httpResponse.Success, true), httpResponse.StatusCode, Length(httpResponse.Body), httpResponse.ErrorMessage]));

    tokenOk := (httpResponse.StatusCode >= 200) and (httpResponse.StatusCode < 300) and (Length(httpResponse.Body) > 0);
    if not tokenOk then
    begin
      // Keep diagnostics but avoid logging auth response body/headers.
      cookiesCount := 0;
      if httpResponse.Cookies <> nil then
        cookiesCount := httpResponse.Cookies.Count;

      log(Format('Tandem.Connect: token exchange failed: status=%d success=%s error=%s bytes=%d cookies=%d',
        [httpResponse.StatusCode, BoolToStr(httpResponse.Success, true), httpResponse.ErrorMessage, Length(httpResponse.Body), cookiesCount]));

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

        log(Format('Tandem.Connect: token parsed access_token_len=%d id_token_len=%d',
          [Length(FAccessToken), Length(idToken)]));
        
        if FAccessToken = '' then
        begin
          lastErr := 'No access token received';
          Exit;
        end;
        
        // Step 5: Extract account info from JWT
        if not ExtractJWT(idToken) then
        begin
          lastErr := 'Failed to extract user info from JWT';
          Exit;
        end;

        log(Format('Tandem.Connect: JWT pumperId=%s accountId=%s', [FPumperId, FAccountId]));
        
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
  finally
    cookieJar.Free;
    customHeaders.Free;
  end;
end;

(*******************************************************************************
  Pump-logs event decoding

  Event codes and their eventProperties names below were read off a live EU
  account's payload (t:slim X2, Dexcom G7, Control-IQ) rather than guessed --
  see LogEventCensus, which is what produced them.
 ******************************************************************************)

const
  {** Bolus request part 1: carbohydrates, BG and the ratios used }
  TANDEM_EV_BOLUS_REQUEST1 = 64;
  {** Bolus request part 3: the food/correction/total split, in units }
  TANDEM_EV_BOLUS_REQUEST3 = 66;
  {** Bolus activated: delivery is starting }
  TANDEM_EV_BOLUS_ACTIVATED = 55;
  {** Bolus completed: carries what was actually delivered }
  TANDEM_EV_BOLUS_COMPLETED = 20;
  {** Bolus delivery record, twice per bolus }
  TANDEM_EV_BOLUS_DELIVERY = 280;
  {** Periodic pump state: reservoir, battery bars, IOB }
  TANDEM_EV_PUMP_STATE = 9;
  {** Battery detail, on charge start and end }
  TANDEM_EV_BATTERY_A = 34;
  TANDEM_EV_BATTERY_B = 35;
  {** Periodic totals: last basal rate, daily total, battery }
  TANDEM_EV_DAILY_STATE = 81;
  {** Pump control-mode change; carries the suspend flag }
  TANDEM_EV_CONTROL_MODE = 230;
  {** Basal rate change, as a float U/hr }
  TANDEM_EV_BASAL_CHANGE = 90;
  {** Control-IQ rate command, in milliunits per hour }
  TANDEM_EV_ALGORITHM_RATE = 279;

  {** Control-IQ's temp-rate field when no temp rate is running }
  TANDEM_RATE_UNSET = 65535;

type
  {** One bolus, assembled from the several events that describe it.

      The pump splits a single delivery across up to six events sharing a
      @code(bolusId), and none of them is complete on its own: the
      carbohydrates appear only in the first request message, the
      food/correction split only in the third, and what was actually delivered
      only in the completion. They are collected here as the payload is walked
      and turned into one entry at the end. }
  TTandemBolusBuild = record
    bolusId: int64;
    requestTime, activateTime, completeTime: TDateTime;
    hasRequest, hasActivate, hasComplete: boolean;
    carbs: double;           // 64 carbAmount, grams
    foodSize: double;        // 66 foodBolusSize, U
    correctionSize: double;  // 66 correctionBolusSize, U
    totalSize: double;       // 66 totalBolusSize, U
    hasTotal: boolean;
    activatedSize: double;   // 55 bolusSize, U
    hasActivated: boolean;
    delivered: double;       // 20 insulinDelivered, U
    hasDelivered: boolean;
    completionStatus: integer;
    bolusType: integer;      // 64 bolusType
    bolusSource: integer;    // 280 bolusSource
    hasSource: boolean;
  end;

{------------------------------------------------------------------------------
  eventProperties key lookup.

  The BFF serializes the keys in camelCase, but matching is done on a lowercased
  alphanumeric-only form so a change of casing or punctuation on Tandem's side
  does not silently stop a field being read.
 ------------------------------------------------------------------------------}
function TandemNormalizePropName(const AName: string): string;
var
  ch: char;
begin
  Result := '';
  for ch in LowerCase(AName) do
    if ch in ['a'..'z', '0'..'9'] then
      Result := Result + ch;
end;

function TandemFindProp(AProps: TJSONObject; const ANormName: string): TJSONData;
var
  k: integer;
begin
  Result := nil;
  if AProps = nil then
    Exit;
  for k := 0 to AProps.Count - 1 do
    if TandemNormalizePropName(AProps.Names[k]) = ANormName then
      Exit(AProps.Items[k]);
end;

{------------------------------------------------------------------------------
  Read a numeric eventProperty. Absent or non-numeric returns False rather than
  a zero, so a field we cannot read never becomes a dose of nothing.
 ------------------------------------------------------------------------------}
function TandemPropNum(AProps: TJSONObject; const ANormName: string;
  out AValue: double): boolean;
var
  data: TJSONData;
begin
  AValue := 0;
  data := TandemFindProp(AProps, ANormName);
  Result := data is TJSONNumber;
  if Result then
    AValue := data.AsFloat;
end;

function TandemPropInt(AProps: TJSONObject; const ANormName: string;
  ADefault: integer): integer;
var
  value: double;
begin
  if TandemPropNum(AProps, ANormName, value) then
    Result := Round(value)
  else
    Result := ADefault;
end;

{------------------------------------------------------------------------------
  Parse a pumpDateTime, e.g. "2026-08-10T11:34:49".

  This is the pump's own wall clock with no zone on it, and it is the same time
  base that the CGM events' egvTimeStamp decodes to -- verified against a
  payload where an event's egvTimeStamp and its pumpDateTime agreed to within
  three seconds. Treatments and readings therefore land on one timeline with no
  conversion between them, which is the whole point of preferring this field
  over the estimatedDateTime sitting next to it.
 ------------------------------------------------------------------------------}
function ParseTandemPumpTime(const AValue: string; out ATime: TDateTime): boolean;
var
  y, mo, d, h, mi, s: integer;
begin
  Result := false;
  ATime := 0;
  if Length(AValue) < 19 then
    Exit;
  if not TryStrToInt(Copy(AValue, 1, 4), y) then Exit;
  if not TryStrToInt(Copy(AValue, 6, 2), mo) then Exit;
  if not TryStrToInt(Copy(AValue, 9, 2), d) then Exit;
  if not TryStrToInt(Copy(AValue, 12, 2), h) then Exit;
  if not TryStrToInt(Copy(AValue, 15, 2), mi) then Exit;
  if not TryStrToInt(Copy(AValue, 18, 2), s) then Exit;
  Result := TryEncodeDateTime(y, mo, d, h, mi, s, 0, ATime);
end;

{------------------------------------------------------------------------------
  Census of a pump-logs payload: how many of each eventCode came back, and one
  whole sample event per code.

  The driver reads only the four CGM event codes. Everything else the eventIds
  filter asks for -- boluses, carbohydrates, cartridge and battery events --
  arrives and is dropped, because Tandem does not publish the BFF's property
  names and we have never seen a real payload's non-CGM events. Guessing them
  would mean putting invented insulin figures on the graph, so this dumps what
  an account actually returns instead.

  One sample per code, not all of them: the number of codes is bounded by the
  eventIds filter we send, so the output is bounded too, and the second event
  of a code says nothing the first did not.

  The samples are raw event objects, so they carry that account's glucose,
  insulin and carbohydrate figures. It stays on the user's own machine and only
  in a debug build, but a log being sent on for diagnosis should be read first.
 ------------------------------------------------------------------------------}
procedure Tandem.LogEventCensus(AEvents: TJSONArray);
const
  // The filter we send names 55 codes; the cap is only there so a payload that
  // ignores the filter cannot make this loop unbounded.
  MAX_CODES = 96;
  MAX_SAMPLE_CHARS = 1500;
var
  codes, counts: array of integer;
  samples: array of string;
  distinct, i, j, k, code, slot, swapInt: integer;
  swapStr: string;
  eventObj: TJSONObject;
  summary: string;
begin
  if (AEvents = nil) or (AEvents.Count = 0) then
    Exit;

  SetLength(codes, MAX_CODES);
  SetLength(counts, MAX_CODES);
  SetLength(samples, MAX_CODES);
  distinct := 0;

  for i := 0 to AEvents.Count - 1 do
  begin
    if not (AEvents[i] is TJSONObject) then
      Continue;
    eventObj := TJSONObject(AEvents[i]);
    code := eventObj.Get('eventCode', -1);

    slot := -1;
    for j := 0 to distinct - 1 do
      if codes[j] = code then
      begin
        slot := j;
        Break;
      end;

    if slot < 0 then
    begin
      // Past the cap we stop learning new codes but keep counting the known
      // ones, so the summary still adds up against AEvents.Count.
      if distinct >= MAX_CODES then
        Continue;
      slot := distinct;
      codes[slot] := code;
      counts[slot] := 0;
      samples[slot] := eventObj.AsJSON;
      if Length(samples[slot]) > MAX_SAMPLE_CHARS then
        samples[slot] := Copy(samples[slot], 1, MAX_SAMPLE_CHARS) + ' ...[truncated]';
      Inc(distinct);
    end;
    Inc(counts[slot]);
  end;

  // Ascending by code, so the same account read twice produces comparable logs
  for i := 1 to distinct - 1 do
  begin
    swapInt := codes[i];
    k := counts[i];
    swapStr := samples[i];
    j := i - 1;
    while (j >= 0) and (codes[j] > swapInt) do
    begin
      codes[j + 1] := codes[j];
      counts[j + 1] := counts[j];
      samples[j + 1] := samples[j];
      Dec(j);
    end;
    codes[j + 1] := swapInt;
    counts[j + 1] := k;
    samples[j + 1] := swapStr;
  end;

  summary := '';
  for j := 0 to distinct - 1 do
    summary := summary + Format(' %d=%d', [codes[j], counts[j]]);
  log(Format('Tandem.EventCensus: %d events, %d distinct codes (code=count):%s',
    [AEvents.Count, distinct, summary]));

  for j := 0 to distinct - 1 do
    log(Format('Tandem.EventCensus: code %d sample=%s', [codes[j], samples[j]]));
end;

{------------------------------------------------------------------------------
  Build the bolus, carbohydrate, pump-status and basal caches from one payload.

  Boluses are assembled by bolusId across the request, activation, delivery and
  completion events, because the figures we want are spread over all of them.
  What went in is taken from the completion's insulinDelivered in preference to
  anything that was merely requested: an interrupted bolus delivers less than
  it asked for, and the delivered figure is the one the glucose curve reflects.

  Every bolus here is reported as manual. Control-IQ does deliver automatic
  corrections, but the payload we have decoded does not yet say which field
  distinguishes them -- bolusSource, bolusType and correctionBolusIncluded are
  all candidates and we have seen only one value of each. Marking boluses
  automatic on a guess would hide real ones behind an off-by-default setting,
  whereas leaving them all visible is at worst untidy: Control-IQ corrects at
  most hourly, so a Tandem overlay does not face the hundred-hairlines problem
  that made the split necessary for SmartGuard. The raw values are logged per
  bolus so the mapping can be settled from a payload that contains both kinds.

  Carbohydrates come only from the bolus calculator's carbAmount. Tandem has no
  standalone meal event, so unlike CareLink there is nothing to reconcile and
  no double-counting to avoid.

  Status and basal fields are taken from the most recent event that carries
  them, which is why each is tracked with its own timestamp rather than simply
  overwritten -- the payload is ordered in practice but the overlay must not
  depend on that.
 ------------------------------------------------------------------------------}
procedure Tandem.ExtractTreatments(AEvents: TJSONArray);
var
  builds: array of TTandemBolusBuild;
  buildCount, i, j, slot, code, intVal: integer;
  eventObj, propsObj: TJSONObject;
  stamp: TDateTime;
  idVal, numVal: double;
  bolusId: int64;
  carbCount, skipped: integer;
  swapBolus: TBolusEntry;
  swapCarb: TCarbEntry;
  reservoirAt, batteryAt, suspendAt, basalAt, profileAt: TDateTime;
  kindStr, sourceLog: string;

  function FindBuild(AId: int64): integer;
  var
    k: integer;
  begin
    for k := 0 to buildCount - 1 do
      if builds[k].bolusId = AId then
        Exit(k);
    if buildCount >= Length(builds) then
      SetLength(builds, Length(builds) + 32);
    builds[buildCount] := Default(TTandemBolusBuild);
    builds[buildCount].bolusId := AId;
    builds[buildCount].completionStatus := -1;
    builds[buildCount].bolusType := -1;
    Result := buildCount;
    Inc(buildCount);
  end;

  // A percentage the pump could not read comes back as 255; treat anything
  // outside 0..100 as absent rather than as a reading.
  function AsPercent(AValue: double): integer;
  begin
    Result := Round(AValue);
    if (Result < 0) or (Result > 100) then
      Result := DEVICE_STATUS_UNKNOWN;
  end;

begin
  SetLength(FBoluses, 0);
  FBolusesValid := false;
  SetLength(FCarbs, 0);
  FCarbsValid := false;
  clearDeviceStatus(FDeviceStatus);
  FDeviceStatusValid := false;
  clearBasalCache;

  if (AEvents = nil) or (AEvents.Count = 0) then
    Exit;

  SetLength(builds, 64);
  buildCount := 0;
  reservoirAt := 0;
  batteryAt := 0;
  suspendAt := 0;
  basalAt := 0;
  profileAt := 0;

  for i := 0 to AEvents.Count - 1 do
  begin
    if not (AEvents[i] is TJSONObject) then
      Continue;
    eventObj := TJSONObject(AEvents[i]);
    if not (eventObj.Find('eventProperties') is TJSONObject) then
      Continue;
    propsObj := TJSONObject(eventObj.Find('eventProperties'));
    if not ParseTandemPumpTime(eventObj.Get('pumpDateTime', ''), stamp) then
      Continue;

    code := eventObj.Get('eventCode', -1);

    case code of
    TANDEM_EV_BOLUS_REQUEST1, TANDEM_EV_BOLUS_REQUEST3,
    TANDEM_EV_BOLUS_ACTIVATED, TANDEM_EV_BOLUS_COMPLETED,
    TANDEM_EV_BOLUS_DELIVERY:
    begin
      if not TandemPropNum(propsObj, 'bolusid', idVal) then
        Continue;
      bolusId := Round(idVal);
      slot := FindBuild(bolusId);

      case code of
      TANDEM_EV_BOLUS_REQUEST1:
      begin
        builds[slot].requestTime := stamp;
        builds[slot].hasRequest := true;
        if TandemPropNum(propsObj, 'carbamount', numVal) and (numVal > 0) then
          builds[slot].carbs := numVal;
        builds[slot].bolusType := TandemPropInt(propsObj, 'bolustype', -1);
      end;
      TANDEM_EV_BOLUS_REQUEST3:
      begin
        if not builds[slot].hasRequest then
        begin
          builds[slot].requestTime := stamp;
          builds[slot].hasRequest := true;
        end;
        TandemPropNum(propsObj, 'foodbolussize', builds[slot].foodSize);
        TandemPropNum(propsObj, 'correctionbolussize', builds[slot].correctionSize);
        builds[slot].hasTotal :=
          TandemPropNum(propsObj, 'totalbolussize', builds[slot].totalSize);
      end;
      TANDEM_EV_BOLUS_ACTIVATED:
      begin
        builds[slot].activateTime := stamp;
        builds[slot].hasActivate := true;
        builds[slot].hasActivated :=
          TandemPropNum(propsObj, 'bolussize', builds[slot].activatedSize);
      end;
      TANDEM_EV_BOLUS_COMPLETED:
      begin
        builds[slot].completeTime := stamp;
        builds[slot].hasComplete := true;
        builds[slot].hasDelivered :=
          TandemPropNum(propsObj, 'insulindelivered', builds[slot].delivered);
        builds[slot].completionStatus :=
          TandemPropInt(propsObj, 'completionstatus', -1);
      end;
      TANDEM_EV_BOLUS_DELIVERY:
        if not builds[slot].hasSource then
        begin
          builds[slot].bolusSource := TandemPropInt(propsObj, 'bolussource', -1);
          builds[slot].hasSource := true;
        end;
      end;
    end;

    TANDEM_EV_PUMP_STATE:
    begin
      if stamp >= reservoirAt then
      begin
        // "insulin" is the reservoir remaining. Its companion iob in this
        // event is scaled by 100 (109 for 1.09 U), so the scale here is worth
        // checking against the pump display before this figure is shown to a
        // user; it is logged below for exactly that reason.
        if TandemPropNum(propsObj, 'insulin', numVal) and (numVal >= 0) then
        begin
          FDeviceStatus.reservoirUnits := numVal;
          reservoirAt := stamp;
        end;
      end;
      if (stamp >= batteryAt) and TandemPropNum(propsObj, 'ibc', numVal) then
      begin
        intVal := AsPercent(numVal);
        if intVal <> DEVICE_STATUS_UNKNOWN then
        begin
          FDeviceStatus.pumpBatteryPercent := intVal;
          batteryAt := stamp;
        end;
      end;
    end;

    TANDEM_EV_BATTERY_A, TANDEM_EV_BATTERY_B:
      if (stamp >= batteryAt) and TandemPropNum(propsObj, 'ibc', numVal) then
      begin
        intVal := AsPercent(numVal);
        if intVal <> DEVICE_STATUS_UNKNOWN then
        begin
          FDeviceStatus.pumpBatteryPercent := intVal;
          batteryAt := stamp;
        end;
      end;

    TANDEM_EV_CONTROL_MODE:
      if stamp >= suspendAt then
      begin
        FDeviceStatus.pumpSuspended := TandemPropInt(propsObj, 'pumpsuspended', 0) <> 0;
        suspendAt := stamp;
      end;

    TANDEM_EV_BASAL_CHANGE:
      if (stamp >= basalAt) and
        TandemPropNum(propsObj, 'commandedbasalrate', numVal) and (numVal >= 0) then
      begin
        FBasalRate := numVal;
        FBasalSource := code;
        basalAt := stamp;
      end;

    TANDEM_EV_ALGORITHM_RATE:
    begin
      if (stamp >= basalAt) and
        TandemPropNum(propsObj, 'commandedrate', numVal) and
        (numVal >= 0) and (numVal <> TANDEM_RATE_UNSET) then
      begin
        // Milliunits per hour here, unlike the float U/hr of code 90.
        FBasalRate := numVal / 1000;
        FBasalSource := code;
        basalAt := stamp;
      end;
      // The programmed rate rides along in the same event, in the same units.
      // It is tracked separately because only this event carries it: the
      // commanded rate can come from a newer code 90 or 81 without the profile
      // rate being restated, and the two must not be reported as one figure.
      if (stamp >= profileAt) and
        TandemPropNum(propsObj, 'profilebasalrate', numVal) and
        (numVal >= 0) and (numVal <> TANDEM_RATE_UNSET) then
      begin
        FProfileRate := numVal / 1000;
        profileAt := stamp;
      end;
    end;

    TANDEM_EV_DAILY_STATE:
      if (stamp >= basalAt) and
        TandemPropNum(propsObj, 'lastbasalrate', numVal) and (numVal >= 0) then
      begin
        FBasalRate := numVal;
        FBasalSource := code;
        basalAt := stamp;
      end;
    end;
  end;

  FBasalAt := basalAt;
  FProfileRateAt := profileAt;

  // Turn the assembled boluses into entries, dropping any that delivered
  // nothing -- a record of no insulin given should not put a stem on a graph.
  SetLength(FBoluses, buildCount);
  SetLength(FCarbs, buildCount);
  j := 0;
  carbCount := 0;
  skipped := 0;
  sourceLog := '';

  for i := 0 to buildCount - 1 do
  begin
    if builds[i].hasDelivered then
      numVal := builds[i].delivered
    else if builds[i].hasTotal then
      numVal := builds[i].totalSize
    else if builds[i].hasActivated then
      numVal := builds[i].activatedSize
    else
    begin
      Inc(skipped);
      Continue;
    end;

    // Delivery start, not completion: an extended bolus finishes hours after
    // the insulin began going in, and the start is where it belongs on a graph.
    if builds[i].hasActivate then
      stamp := builds[i].activateTime
    else if builds[i].hasRequest then
      stamp := builds[i].requestTime
    else
      stamp := builds[i].completeTime;

    if builds[i].carbs > 0 then
      kindStr := 'MEAL'
    else if builds[i].correctionSize > 0 then
      kindStr := 'CORRECTION'
    else if builds[i].foodSize > 0 then
      kindStr := 'FOOD'
    else
      kindStr := '';

    if numVal > 0 then
    begin
      FBoluses[j] := Default(TBolusEntry);
      FBoluses[j].time := stamp;
      FBoluses[j].units := numVal;
      FBoluses[j].carbs := builds[i].carbs;
      FBoluses[j].kind := kindStr;
      FBoluses[j].automatic := false;
      Inc(j);
    end
    else
      Inc(skipped);

    if builds[i].carbs > 0 then
    begin
      FCarbs[carbCount] := Default(TCarbEntry);
      FCarbs[carbCount].time := stamp;
      FCarbs[carbCount].grams := builds[i].carbs;
      FCarbs[carbCount].kind := kindStr;
      Inc(carbCount);
    end;

    // Until the automatic/manual mapping is settled, every bolus reports the
    // fields that might carry it.
    if Length(sourceLog) < 400 then
      sourceLog := sourceLog + Format(' [id=%d src=%d type=%d st=%d u=%.3f c=%.0f]',
        [builds[i].bolusId, builds[i].bolusSource, builds[i].bolusType,
         builds[i].completionStatus, numVal, builds[i].carbs]);
  end;

  SetLength(FBoluses, j);
  SetLength(FCarbs, carbCount);
  FBolusesValid := true;
  FCarbsValid := true;
  FDeviceStatusValid := (FDeviceStatus.reservoirUnits >= 0) or
    (FDeviceStatus.pumpBatteryPercent <> DEVICE_STATUS_UNKNOWN) or
    (suspendAt > 0);

  // Oldest first: the overlay draws in array order and must not rely on the
  // payload having arrived sorted.
  for i := 1 to High(FBoluses) do
  begin
    swapBolus := FBoluses[i];
    j := i - 1;
    while (j >= 0) and (FBoluses[j].time > swapBolus.time) do
    begin
      FBoluses[j + 1] := FBoluses[j];
      Dec(j);
    end;
    FBoluses[j + 1] := swapBolus;
  end;

  for i := 1 to High(FCarbs) do
  begin
    swapCarb := FCarbs[i];
    j := i - 1;
    while (j >= 0) and (FCarbs[j].time > swapCarb.time) do
    begin
      FCarbs[j + 1] := FCarbs[j];
      Dec(j);
    end;
    FCarbs[j + 1] := swapCarb;
  end;

  log(Format('Tandem.ExtractTreatments: %d boluses, %d carb entries, %d skipped'
    + ' (from %d bolus ids in %d events)',
    [Length(FBoluses), carbCount, skipped, buildCount, AEvents.Count]));
  if sourceLog <> '' then
    log('Tandem.ExtractTreatments: bolus fields:' + sourceLog);
  log(Format('Tandem.ExtractTreatments: reservoir=%.1fU battery=%d%% '
    + 'suspended=%s basal=%.3fU/hr',
    [FDeviceStatus.reservoirUnits, FDeviceStatus.pumpBatteryPercent,
     BoolToStr(FDeviceStatus.pumpSuspended, true), FBasalRate]));
end;

{------------------------------------------------------------------------------
  Treatment accessors. All answer from what the last fetch cached rather than
  issuing a request, so the window they cover is whatever that fetch covered.
 ------------------------------------------------------------------------------}
function Tandem.getBoluses(out ABoluses: TBolusList): boolean;
begin
  ABoluses := Copy(FBoluses);
  Result := FBolusesValid and (Length(ABoluses) > 0);
end;

function Tandem.supportsBoluses: boolean;
begin
  Result := true;
end;

function Tandem.getCarbs(out ACarbs: TCarbList): boolean;
begin
  ACarbs := Copy(FCarbs);
  Result := FCarbsValid and (Length(ACarbs) > 0);
end;

function Tandem.supportsCarbs: boolean;
begin
  Result := true;
end;

function Tandem.getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean;
begin
  if not FDeviceStatusValid then
  begin
    clearDeviceStatus(AStatus);
    Exit(false);
  end;
  AStatus := FDeviceStatus;
  Result := true;
end;

{------------------------------------------------------------------------------
  The last rate Control-IQ commanded, in U/hr.

  supportsBasal stays False: the graph's basal overlay wants a repeating daily
  schedule, and while the payload does carry the programmed profileBasalRate
  alongside each command, one payload only covers the window that was fetched
  and cannot be read as a full day's schedule.
 ------------------------------------------------------------------------------}
function Tandem.getBasalRate: single;
begin
  if FBasalRate < 0 then
    Exit(0);
  Result := FBasalRate;
end;

procedure Tandem.clearBasalCache;
begin
  FBasalRate := -1;
  FBasalAt := 0;
  FBasalSource := -1;
  FProfileRate := -1;
  FProfileRateAt := 0;
end;

{------------------------------------------------------------------------------
  Both rates, and where they came from.

  The commanded rate on its own is what a looping pump was last told to run,
  which is not the rate the user set and can be hours old once Control-IQ stops
  commanding -- the payload carries the programmed rate too, so report both and
  let the caller show which is which.
 ------------------------------------------------------------------------------}
function Tandem.getBasalStatus(out AStatus: TBasalStatus): boolean;
begin
  clearBasalStatus(AStatus);

  if FBasalRate >= 0 then
  begin
    AStatus.commanded := FBasalRate;
    AStatus.time := FBasalAt;
    AStatus.source := Format('pump event %d', [FBasalSource]);
  end;

  if FProfileRate >= 0 then
  begin
    AStatus.programmed := FProfileRate;
    // Whichever figure is newer dates the pair, so an age shown next to them
    // is never older than the freshest thing they were read from.
    if FProfileRateAt > AStatus.time then
      AStatus.time := FProfileRateAt;
    if AStatus.source = '' then
      AStatus.source := Format('pump event %d', [TANDEM_EV_ALGORITHM_RATE]);
  end;

  Result := (AStatus.commanded >= 0) or (AStatus.programmed >= 0);
end;

{------------------------------------------------------------------------------
  Retrieve glucose readings from Tandem Source

  Fetches pre-decoded pump events from the Tandem Source BFF pump-logs API
  and extracts EGV (estimated glucose value) readings from the CGM data
  events (Dexcom G6/G7, FreeStyle Libre 2/3).
 ------------------------------------------------------------------------------}
function Tandem.GetReadings(AMinutes, AMaxCount: integer; AExtras: string;
  out ARes: string; {%H-}noCache: boolean): BGResults;
// Tandem fetches readings via authenticated calls to internal report endpoints
// that are not subject to HTTP GET caching. noCache is accepted for interface
// compatibility but has no effect on this backend.
var
  startDate, endDate: TDateTime;
  count: integer;
  readingsList: array of BGReading;
  fetchOk: boolean; // true once the pump-logs request itself succeeded
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
  // eventProperties keys are matched case-insensitively (lowercased, with
  // non-alphanumerics stripped) since the BFF serializes them in camelCase
  function NormalizePropName(const AName: string): string;
  var
    ch: char;
  begin
    Result := '';
    for ch in LowerCase(AName) do
      if ch in ['a'..'z', '0'..'9'] then
        Result := Result + ch;
  end;
  function FindEventProp(AProps: TJSONObject; const ANormName: string): TJSONData;
  var
    k: integer;
  begin
    Result := nil;
    if AProps = nil then
      Exit;
    for k := 0 to AProps.Count - 1 do
      if NormalizePropName(AProps.Names[k]) = ANormName then
        Exit(AProps.Items[k]);
  end;
  function TrySourcePumpLogsReadings(out AResults: BGResults): boolean;
  var
    sourceHeaders: TStringList;
    sourceResponse: THTTPResponse;
    sourceUrl: string;
    minDateStr: string;
    maxDateStr: string;
    eventIdsParam: string;
    jsonData: TJSONData;
    eventsArr: TJSONArray;
    eventObj: TJSONObject;
    propsObj: TJSONObject;
    propData: TJSONData;
    i: integer;
    eventCode: integer;
    glucoseValue: integer;
    statusValue: integer;
    egvTimestamp: int64;
    bgValue: integer;
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

    minDateStr := FormatDateTime('yyyy-mm-dd', DateOf(startDate));
    maxDateStr := FormatDateTime('yyyy-mm-dd', DateOf(endDate));
    eventIdsParam := StringReplace(TANDEM_SOURCE_DEFAULT_EVENT_IDS, ',', '%2C', [rfReplaceAll]);

    // BFF pump-logs endpoint (replaces the retired reportsfacade/pumpevents).
    // The server caps each request at roughly four weeks; Trndi never asks
    // for more than a few days, so no date-window paging is needed.
    sourceUrl := GetSourceUrl + 'api/reports/bff/pump-logs/' + FDeviceId
      + '?pumperId=' + FPumperId
      + '&startDate=' + minDateStr + 'T00%3A00%3A00Z'
      + '&endDate=' + maxDateStr + 'T23%3A59%3A59Z'
      + '&eventIds=' + eventIdsParam;

    sourceHeaders := TStringList.Create;
    try
      sourceHeaders.Add('Authorization: Bearer ' + FAccessToken);
      sourceHeaders.Add('Accept: application/json');
      // The WAF requires same-origin Origin/Referer matching the Source host
      sourceHeaders.Add('Origin: ' + TrimRightSet(GetSourceUrl, ['/']));
      sourceHeaders.Add('Referer: ' + GetSourceUrl);
      sourceHeaders.Add('User-Agent: ' + TANDEM_BASE_USER_AGENT);

      log('Tandem.GetReadings: pump-logs request=' + sourceUrl);
      sourceResponse := native.RequestExWait(false, sourceUrl, [], '', nil, true, 10, sourceHeaders, false);
      log(Format('Tandem.GetReadings: pump-logs status=%d bytes=%d',
        [sourceResponse.StatusCode, Length(sourceResponse.Body)]));

      if sourceResponse.StatusCode = 401 then
      begin
        // Access token expired: re-run the full login once and retry
        log('Tandem.GetReadings: HTTP 401; attempting re-login');
        if Assigned(sourceResponse.Headers) then
          FreeAndNil(sourceResponse.Headers);
        if Assigned(sourceResponse.Cookies) then
          FreeAndNil(sourceResponse.Cookies);
        if not Connect then
          Exit;
        // Connect re-runs SelectDevice, so rebuild the URL and auth header
        sourceUrl := GetSourceUrl + 'api/reports/bff/pump-logs/' + FDeviceId
          + '?pumperId=' + FPumperId
          + '&startDate=' + minDateStr + 'T00%3A00%3A00Z'
          + '&endDate=' + maxDateStr + 'T23%3A59%3A59Z'
          + '&eventIds=' + eventIdsParam;
        sourceHeaders[0] := 'Authorization: Bearer ' + FAccessToken;
        sourceResponse := native.RequestExWait(false, sourceUrl, [], '', nil, true, 10, sourceHeaders, false);
        log(Format('Tandem.GetReadings: pump-logs retry status=%d bytes=%d',
          [sourceResponse.StatusCode, Length(sourceResponse.Body)]));
      end;

      if (sourceResponse.StatusCode < 200) or (sourceResponse.StatusCode >= 300) then
      begin
        if Trim(sourceResponse.Body) <> '' then
          log('Tandem.GetReadings: pump-logs error body prefix=' + Copy(sourceResponse.Body, 1, 200));
        lastErr := 'Failed to get readings: HTTP ' + IntToStr(sourceResponse.StatusCode);
        Exit;
      end;

      if Trim(sourceResponse.Body) = '' then
      begin
        fetchOk := True;
        Exit;
      end;

      SetLength(readingsList, 0);
      count := 0;

      jsonData := GetJSON(sourceResponse.Body);
      try
        eventsArr := nil;
        if (jsonData is TJSONObject) and
          (TJSONObject(jsonData).Find('events') is TJSONArray) then
          eventsArr := TJSONArray(TJSONObject(jsonData).Find('events'));

        if eventsArr = nil then
        begin
          log('Tandem.GetReadings: pump-logs response missing events array');
          lastErr := 'Unsupported pump-logs response format';
          Exit;
        end;

        fetchOk := True;

        // Debug builds only: report what the payload holds beyond the CGM
        // events, which is how the property names below were established and
        // how an unfamiliar payload announces itself.
        LogEventCensus(eventsArr);

        // Boluses, carbohydrates, reservoir/battery and the commanded basal
        // rate all come out of the same payload the readings do.
        ExtractTreatments(eventsArr);

        for i := 0 to eventsArr.Count - 1 do
        begin
          if not (eventsArr[i] is TJSONObject) then
            Continue;
          eventObj := TJSONObject(eventsArr[i]);

          // CGM data events: 256 = G6 (GXB), 372 = FSL2, 399 = G7, 480 = FSL3
          eventCode := eventObj.Get('eventCode', 0);
          if not ((eventCode = 256) or (eventCode = 372) or (eventCode = 399) or (eventCode = 480)) then
            Continue;

          if not (eventObj.Find('eventProperties') is TJSONObject) then
            Continue;
          propsObj := TJSONObject(eventObj.Find('eventProperties'));

          propData := FindEventProp(propsObj, 'currentglucosedisplayvalue');
          if not (propData is TJSONNumber) then
            Continue;
          glucoseValue := propData.AsInteger;

          statusValue := 0;
          propData := FindEventProp(propsObj, 'glucosevaluestatus');
          if propData is TJSONNumber then
            statusValue := propData.AsInteger;

          propData := FindEventProp(propsObj, 'egvtimestamp');
          if not (propData is TJSONNumber) then
            Continue;
          egvTimestamp := propData.AsInt64;

          // Mirror the Tandem Source frontend: 0 = precise value, 1 = special
          // high, 2 = special low; out-of-range readings become the sentinel
          // values 39/401. Anything else (e.g. G7 "do not show") is skipped.
          case statusValue of
          0:
            if glucoseValue < 40 then
              bgValue := 39
            else if glucoseValue > 400 then
              bgValue := 401
            else
              bgValue := glucoseValue;
          1:
            bgValue := 401;
          2:
            bgValue := 39;
          else
            Continue;
          end;

          // egvTimeStamp is seconds since the Tandem epoch, expressed in the
          // pump's local wall-clock time
          eventTime := UnixToDateTime(Int64(TANDEM_EPOCH) + egvTimestamp, True);
          if (eventTime < startDate) or (eventTime > (endDate + 1)) then
            Continue;

          SetLength(readingsList, count + 1);
          readingsList[count].Init(mgdl, self.systemName);
          readingsList[count].update(bgValue, 0);
          readingsList[count].date := eventTime;
          readingsList[count].trend := TdPlaceholder;
          readingsList[count].level := getLevel(bgValue);
          Inc(count);
        end;
      finally
        jsonData.Free;
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
          log(Format('Tandem.GetReadings: pump-logs top[%d]=%s val=%.1f',
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
  
  try
    // Calculate time range
    if AMinutes <= 0 then
      AMinutes := 1440;
    endDate := Now;
    startDate := endDate - (AMinutes / 1440.0); // Convert minutes to days

    fetchOk := False;
    if TrySourcePumpLogsReadings(Result) then
      ARes := 'Retrieved ' + IntToStr(Length(Result)) + ' CGM readings'
    else if fetchOk then
    begin
      // The request succeeded but the window contained no CGM events
      lastErr := '';
      ARes := 'No CGM readings found in specified time range';
    end
    else
    begin
      if lastErr = '' then
        lastErr := 'Failed to get readings';
      ARes := lastErr;
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
  
  if region = trEU then api := TandemEU.Create(AEmail, APass, region) else api := TandemUSA.Create(AEmail, APass, region);
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
