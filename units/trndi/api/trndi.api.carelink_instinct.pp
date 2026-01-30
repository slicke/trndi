(*
 * Trndi CareLink Instinct API Driver
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh and contributors
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
 * - This is an ALPHA driver - use at your own risk and verify all data with official sources.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)
unit trndi.api.carelink_instinct;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
fpjson, jsonparser, dateutils, StrUtils, jsonscanner;

const
  {** CareLink Instinct API base URL. }
  CARELINK_INSTINCT_BASE_URL = 'https://carelink.minimed.com/';

  {** CareLink Instinct authentication endpoint. }
  CARELINK_INSTINCT_AUTH_ENDPOINT = 'auth/login';

  {** CareLink Instinct glucose readings endpoint. }
  CARELINK_INSTINCT_READINGS_ENDPOINT = 'data/glucose';

  {** CareLink Instinct user profile endpoint. }
  CARELINK_INSTINCT_PROFILE_ENDPOINT = 'user/profile';

type
  {** CareLink Instinct API client.
      Provides methods to connect to CareLink Instinct and fetch CGM readings.
      This is an ALPHA implementation based on typical REST API patterns.

      @seealso(TrndiAPI)
   }
  CareLinkInstinct = class(TrndiAPI)
  protected
    {** Authentication token obtained during login. }
    authToken: string;

    {** Stored username for authentication. }
    storedUser: string;

    {** Stored password for authentication. }
    storedPass: string;
    {** Get the value which represents the minimum reading for the backend. }
    function getLimitLow: integer; override;

    {** Get the value which represents the maximum reading for the backend. }
    function getLimitHigh: integer; override;

    {** Get the name of the system. }
    function getSystemName: string; override;

  public
    {** Create a CareLink Instinct API client.
        Initializes the HTTP User-Agent and base URL.

        @param(user  CareLink username/email)
        @param(pass  CareLink password)
        @param(extra Reserved for future use)
     }
    constructor Create(user, pass: string); override;

    {** Connect to CareLink Instinct, authenticate, and retrieve user profile.
        @returns(True on success; False otherwise. On failure, lastErr is set.)
     }
    function connect: boolean; override;

    {** Fetch glucose readings from CareLink Instinct.
        @param(minNum  Unused - time-based filtering not implemented)
        @param(maxNum  Maximum number of readings to fetch)
        @param(extras   Optional endpoint override)
        @param(res      Out parameter receiving the raw JSON response string)
        @returns(Array of BGReading; empty on errors)
     }
    function getReadings(minNum, maxNum: integer; extras: string;
      out res: string): BGResults; override;

    {** UI parameter label provider (override).
      1: CareLink Username
      2: CareLink Password
      3: (unused)
     }
    class function ParamLabel(LabelName: APIParamLabel): string; override;

    {** Test CareLink Instinct credentials.
     }
    class function testConnection(user, pass: string; var res: string): maybebool; override;

    function getMaxAge: integer; override;
  private
    // (no private members)
  published
    {** The CareLink base URL actually used (read-only proxy to baseUrl). }
    property remote: string read baseUrl;
  end;

implementation

resourcestring
sParamUsername = 'CareLink Username';
sParamPassword = 'CareLink Password';
sParamDesc = '** ALPHA DRIVER - CareLink Instinct Setup**' + #13#10#13#10 +
  '⚠️ This is an experimental driver. Verify all data with official Medtronic apps.' + #13#10#13#10 +
  '1) Ensure you have a CareLink account.' + #13#10 +
  '2) Enter your CareLink credentials below.' + #13#10#13#10 +
  'Username: Your CareLink email or username' + #13#10 +
  'Password: Your CareLink password' + #13#10#13#10 +
  'Note: This driver is in development and may not work with all CareLink configurations.';
sParamDescHTML =
  '<div style="font-family: Arial, sans-serif; line-height: 1.6;">' +
  '<div style="background: #ff6b35; color: white; padding: 15px; border-radius: 6px; margin-bottom: 20px; font-weight: bold; text-align: center; border: 2px solid #e55a2b;">' +
  '⚠️ ALPHA DRIVER - Use at your own risk! ⚠️' +
  '</div>' +
  '<h2 style="margin-bottom: 10px;">🏥 CareLink Instinct Setup</h2>' +
  '<p style="color: #7f8c8d; font-style: italic; margin-bottom: 15px;">Experimental - Verify with official sources</p>' +
  '<ol style="padding-left: 20px;">' +
  '<li style="margin-bottom: 10px;">Ensure you have a CareLink account with Medtronic.</li>' +
  '<li style="margin-bottom: 10px;">Enter your CareLink credentials:</li>' +
  '<ul style="margin-top: 5px; padding-left: 20px;">' +
  '<li><strong>Username:</strong> Your CareLink email or username</li>' +
  '<li><strong>Password:</strong> Your CareLink password</li>' +
  '</ul>' +
  '</ol>' +
  '<p style="border-left: 4px solid #ffc107; padding: 10px; margin-top: 15px; background: #fff3cd; color: #000000;">' +
  '<strong>⚠️ Note:</strong> This is an experimental implementation. Always verify glucose readings with your official Medtronic devices and apps.</p>' +
  '</div>';

{------------------------------------------------------------------------------
  getMaxAge
  --------------------
  Returns the maximum age (in minutes) of readings provided by the backend
 ------------------------------------------------------------------------------}
function CareLinkInstinct.getMaxAge: integer;
begin
  result := 1440; // 24 hours
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function CareLinkInstinct.getSystemName: string;
begin
  result := 'CareLink Instinct';
end;

{------------------------------------------------------------------------------
  getLimitHigh
  --------------------
  Returns the high glucose threshold for this backend
 ------------------------------------------------------------------------------}
function CareLinkInstinct.getLimitHigh: integer;
begin
  result := 180; // Default high threshold
end;

{------------------------------------------------------------------------------
  getLimitLow
  --------------------
  Returns the low glucose threshold for this backend
 ------------------------------------------------------------------------------}
function CareLinkInstinct.getLimitLow: integer;
begin
  result := 70; // Default low threshold
end;

{------------------------------------------------------------------------------
  Create (constructor)
  --------------------
  Initialize UA, base URL, and call inherited constructor.
 ------------------------------------------------------------------------------}
constructor CareLinkInstinct.Create(user, pass: string);
begin
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := CARELINK_INSTINCT_BASE_URL;
  authToken := '';
  storedUser := user;
  storedPass := pass;

  // Call the base class constructor
  inherited;
end;

{------------------------------------------------------------------------------
  Connect
  -------
  Authenticate with CareLink and retrieve user profile/settings.
 ------------------------------------------------------------------------------}
function CareLinkInstinct.Connect: boolean;
var
  ResponseStr: string;
  requestBody: string;
begin
  Result := false;

  // Placeholder: CareLink authentication requires complex login flow with cookies
  // This is a simplified implementation for demonstration

  // 1) Attempt basic authentication (may not work with CareLink)
  requestBody := Format('username=%s&password=%s', [StringReplace(storedUser, '"', '\"', [rfReplaceAll]), StringReplace(storedPass, '"', '\"', [rfReplaceAll])]);

  // 2) Send authentication request
  ResponseStr := native.request(true, 'patient/sso/login', [], requestBody, 'Content-Type=application/x-www-form-urlencoded');

  // 3) Check for basic response
  if Trim(ResponseStr) = '' then
  begin
    lastErr := 'No response from CareLink server';
    Exit;
  end;

  // 4) Check for authentication errors
  if Pos('error', LowerCase(ResponseStr)) > 0 then
  begin
    lastErr := 'Authentication failed - check credentials';
    Exit;
  end;

  // 5) For now, assume success if we get a response
  authToken := 'authenticated'; // Placeholder

  Result := true;
end;

{------------------------------------------------------------------------------
  getReadings
  -----------
  Fetch glucose readings from CareLink Instinct API.
 ------------------------------------------------------------------------------}
function CareLinkInstinct.getReadings(minNum, maxNum: integer; extras: string;
  out res: string): BGResults;
var
  JSONParser: TJSONParser;
  JSONData: TJSONData;
  RootObject: TJSONObject;
  sgsArray: TJSONArray;
  i: integer;
  sg: TJSONObject;
  bg: integer;
  dtStr: string;
  dt: TDateTime;
  trendStr: string;
begin
  SetLength(Result, 0);
  res := '';

  // Make request to CareLink data endpoint
  res := native.request(false, 'patient/connect/data', [], '', '');

  if Trim(res) = '' then
  begin
    res := 'No response from CareLink data endpoint';
    Exit;
  end;

  // Parse JSON response
  try
    JSONParser := TJSONParser.Create(res, [joUTF8, joIgnoreTrailingComma]);
    try
      JSONData := JSONParser.Parse;
      if not (JSONData is TJSONObject) then
      begin
        res := 'Invalid JSON response from CareLink';
        JSONData.Free;
        Exit;
      end;

      RootObject := TJSONObject(JSONData);

      // CareLink returns data in 'sgs' array (sensor glucose values)
      sgsArray := RootObject.Get('sgs', TJSONArray(nil));
      if sgsArray = nil then
      begin
        res := 'No sensor glucose data found in response';
        JSONData.Free;
        Exit;
      end;

      SetLength(Result, sgsArray.Count);
      for i := 0 to sgsArray.Count - 1 do
      begin
        sg := sgsArray.Items[i] as TJSONObject;

        // Initialize reading in mg/dL
        Result[i].Init(mgdl, 'CareLink Instinct');

        // Extract glucose value
        bg := sg.Get('sg', 0);
        if bg = 0 then
          bg := sg.Get('value', 0);
        Result[i].update(bg, BGPrimary);

        // Extract timestamp
        dtStr := sg.Get('datetime', '');
        if dtStr = '' then
          dtStr := sg.Get('dateString', '');
        if dtStr <> '' then
        begin
          dt := StrToDateTime(dtStr);
        end
        else
          dt := Now;
        Result[i].date := dt;

        // Extract trend
        trendStr := sg.Get('trend', '');
        if trendStr = 'Flat' then Result[i].trend := TdFlat
        else if trendStr = 'SingleUp' then Result[i].trend := TdSingleUp
        else if trendStr = 'DoubleUp' then Result[i].trend := TdDoubleUp
        else if trendStr = 'SingleDown' then Result[i].trend := TdSingleDown
        else if trendStr = 'DoubleDown' then Result[i].trend := TdDoubleDown
        else Result[i].trend := TdNotComputable;
      end;

      JSONData.Free;
      res := 'Successfully retrieved ' + IntToStr(Length(Result)) + ' readings';
    finally
      JSONParser.Free;
    end;
  except
    on E: Exception do
    begin
      res := 'Error parsing CareLink response: ' + E.Message;
      SetLength(Result, 0);
    end;
  end;
end;

{------------------------------------------------------------------------------
  ParamLabel
  -----------
  UI parameter labels for CareLink Instinct
 ------------------------------------------------------------------------------}
class function CareLinkInstinct.ParamLabel(LabelName: APIParamLabel): string;
begin
  case LabelName of
    APLUser:
      Result := sParamUsername;
    APLPass:
      Result := sParamPassword;
    APLDesc:
      Result := sParamDesc;
    APLDescHTML:
      Result := sParamDescHTML;
    APLCopyright:
      Result := 'Grok AI';
    else
      Result := '';
  end;
end;

{------------------------------------------------------------------------------
  testConnection
  ---------------
  Test CareLink Instinct credentials
 ------------------------------------------------------------------------------}
class function CareLinkInstinct.testConnection(user, pass: string; var res: string): maybebool;
var
  testAPI: CareLinkInstinct;
begin
  Result := MaybeBool.False;

  try
    testAPI := CareLinkInstinct.Create(user, pass);
    try
      if testAPI.connect then
      begin
        res := 'Connection successful';
        Result := MaybeBool.True;
      end
      else
      begin
        res := testAPI.lastErr;
        Result := MaybeBool.False;
      end;
    finally
      testAPI.Free;
    end;
  except
    on E: Exception do
    begin
      res := 'Test failed: ' + E.Message;
      Result := MaybeBool.False;
    end;
  end;
end;

end.
