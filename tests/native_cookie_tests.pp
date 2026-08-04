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
unit native_cookie_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, trndi.native, trndi.native.base, test_server_helper, SysUtils, Classes;

type
  TNativeCookieTests = class(TTestCase)
  published
    procedure TestCookieSetAndJar;
    procedure TestCookieRedirectUpdatesJar;
  end;

implementation

procedure TNativeCookieTests.TestCookieSetAndJar;
var
  BaseURL: string;
  n: TTrndiNativeBase;
  cookieJar: TStringList;
  resp: THTTPResponse;
begin
  // Ensure the embedded test server is available (TRNDI_TEST_SERVER_URL reuses an external one)
  if not StartOrUseTestServer(BaseURL) then
  begin
    Writeln('Skipping TestCookieSetAndJar: embedded test server unavailable (set TRNDI_TEST_SERVER_URL or unset TRNDI_NO_TESTSERVER)');
    Exit;
  end;    

  n := TrndiNative.Create;
  cookieJar := TStringList.Create;
  try
    // Call endpoint that sets a cookie
    resp := n.RequestExWait(false, BaseURL + '/cookie/set?name=unittestcookie&value=abc123', [], '', cookieJar, false, 0, nil, false);
    // Some platforms may not set Result.Success reliably; assert on status code instead
    AssertEquals(200, resp.StatusCode);
    AssertTrue('Expected no error message (err: ' + resp.ErrorMessage + ')', resp.ErrorMessage = '');

    // Response should include Set-Cookie header parsed into resp.Cookies
    AssertTrue('Expected Set-Cookie in response.Cookies (have: ' + IntToStr(resp.Cookies.Count) + ')', resp.Cookies.Count > 0);
    // cookieJar should have been updated with the cookie value
    AssertTrue('cookieJar should contain unittestcookie=abc123 (have: ' + cookieJar.Text + ')', cookieJar.IndexOf('unittestcookie=abc123') <> -1);

    // Now echo endpoint should show the cookie was sent back to server
    resp := n.RequestExWait(false, BaseURL + '/cookie/echo?name=unittestcookie', [], '', cookieJar, false, 0, nil, false);
    AssertEquals(200, resp.StatusCode);
    AssertTrue('Expected no error message (err: ' + resp.ErrorMessage + ')', resp.ErrorMessage = '');
    AssertTrue('cookie echo should contain unittestcookie (body: ' + resp.Body + ')', Pos('"unittestcookie"', resp.Body) > 0);
    AssertTrue('cookie echo should contain value abc123 (body: ' + resp.Body + ')', Pos('abc123', resp.Body) > 0);
  finally
    cookieJar.Free;
    n.Free;
    StopLocalTestServer;
  end;
end;

procedure TNativeCookieTests.TestCookieRedirectUpdatesJar;
var
  BaseURL: string;
  n: TTrndiNativeBase;
  cookieJar: TStringList;
  resp: THTTPResponse;
begin
  if not StartOrUseTestServer(BaseURL) then
  begin
    Writeln('Skipping TestCookieRedirectUpdatesJar: embedded test server unavailable (set TRNDI_TEST_SERVER_URL or unset TRNDI_NO_TESTSERVER)');
    Exit;
  end;

  n := TrndiNative.Create;
  cookieJar := TStringList.Create;
  try
    // Follow redirect; cookie should be set during redirect response and then seen after redirect
    resp := n.RequestExWait(false, BaseURL + '/cookie/set-redirect?name=redircookie&value=xyz', [], '', cookieJar, true, 5, nil, false);
    // Some platforms may not set Result.Success reliably when following redirects; assert on status code and lack of error
    AssertEquals(200, resp.StatusCode);
    AssertTrue('Expected no error message (err: ' + resp.ErrorMessage + ')', resp.ErrorMessage = '');
    // After following redirect, the final body should be the JSON echo for the cookie
    AssertTrue('Expected cookie echoed after redirect (body: ' + resp.Body + ')', Pos('"redircookie"', resp.Body) > 0);
    // cookieJar must include the cookie from redirect
    AssertTrue('cookieJar should have redircookie=xyz after redirect (have: ' + cookieJar.Text + ')', cookieJar.IndexOf('redircookie=xyz') <> -1);
  finally
    cookieJar.Free;
    n.Free;
    StopLocalTestServer;
  end;
end;

initialization
  RegisterTest(TNativeCookieTests);

end.
