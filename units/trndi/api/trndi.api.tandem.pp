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
  
  cookieJar.Free;
  customHeaders.Free;
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
