unit native_cookie_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, trndi.native, trndi.native.base, php_server_helper, Process, SysUtils, Classes;

type
  TNativeCookieTests = class(TTestCase)
  published
    procedure TestCookieSetAndJar;
    procedure TestCookieRedirectUpdatesJar;
  end;

implementation

procedure TNativeCookieTests.TestCookieSetAndJar;
var
  PHPProcess: TProcess;
  BaseURL: string;
  n: TTrndiNativeBase;
  cookieJar: TStringList;
  resp: THTTPResponse;
begin
  // Ensure the local PHP test server is available (can be overridden with TRNDI_TEST_SERVER_URL)
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
  begin
    Writeln('Skipping TestCookieSetAndJar: PHP test server not available (set TRNDI_TEST_SERVER_URL or TRNDI_PHP_EXECUTABLE)');
    Exit;
  end;    

  n := TrndiNative.Create;
  cookieJar := TStringList.Create;
  try
    // Call endpoint that sets a cookie
    resp := n.RequestEx(false, BaseURL + '/cookie/set?name=unittestcookie&value=abc123', [], '', cookieJar, false, 0, nil, false);
    // Some platforms may not set Result.Success reliably; assert on status code instead
    AssertEquals(200, resp.StatusCode);
    AssertTrue('Expected no error message (err: ' + resp.ErrorMessage + ')', resp.ErrorMessage = '');

    // Response should include Set-Cookie header parsed into resp.Cookies
    AssertTrue('Expected Set-Cookie in response.Cookies (have: ' + IntToStr(resp.Cookies.Count) + ')', resp.Cookies.Count > 0);
    // cookieJar should have been updated with the cookie value
    AssertTrue('cookieJar should contain unittestcookie=abc123 (have: ' + cookieJar.Text + ')', cookieJar.IndexOf('unittestcookie=abc123') <> -1);

    // Now echo endpoint should show the cookie was sent back to server
    resp := n.RequestEx(false, BaseURL + '/cookie/echo?name=unittestcookie', [], '', cookieJar, false, 0, nil, false);
    AssertEquals(200, resp.StatusCode);
    AssertTrue('Expected no error message (err: ' + resp.ErrorMessage + ')', resp.ErrorMessage = '');
    AssertTrue('cookie echo should contain unittestcookie (body: ' + resp.Body + ')', Pos('"unittestcookie"', resp.Body) > 0);
    AssertTrue('cookie echo should contain value abc123 (body: ' + resp.Body + ')', Pos('abc123', resp.Body) > 0);
  finally
    cookieJar.Free;
    n.Free;
    StopLocalTestServer(PHPProcess);
  end;
end;

procedure TNativeCookieTests.TestCookieRedirectUpdatesJar;
var
  PHPProcess: TProcess;
  BaseURL: string;
  n: TTrndiNativeBase;
  cookieJar: TStringList;
  resp: THTTPResponse;
begin
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
  begin
    Writeln('Skipping TestCookieRedirectUpdatesJar: PHP test server not available (set TRNDI_TEST_SERVER_URL or TRNDI_PHP_EXECUTABLE)');
    Exit;
  end;

  n := TrndiNative.Create;
  cookieJar := TStringList.Create;
  try
    // Follow redirect; cookie should be set during redirect response and then seen after redirect
    resp := n.RequestEx(false, BaseURL + '/cookie/set-redirect?name=redircookie&value=xyz', [], '', cookieJar, true, 5, nil, false);
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
    StopLocalTestServer(PHPProcess);
  end;
end;

initialization
  RegisterTest(TNativeCookieTests);

end.
