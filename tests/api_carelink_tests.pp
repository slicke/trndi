unit api_carelink_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.api, trndi.api.carelink, trndi.types, dateutils,
test_server_helper;

type

TAPICareLinkTester = class(TTestCase)
private
  FCredsChangedCount: integer;
  FLastCreds: string;
  procedure HandleCredsChanged(const newCreds: string);
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  // Offline: credential-blob validation
  procedure TestEmptyCredentials;
  procedure TestNonJsonCredentials;
  procedure TestMissingRefreshToken;
  procedure TestGetReadingsWithoutConnect;
  // Offline: class-level metadata
  procedure TestSupportsWebLogin;
  procedure TestParamLabels;
  // Integration against the embedded test server
  procedure TestCareLinkLocalServer;
  procedure TestTransientServerErrorFailsConnect;
end;

implementation

procedure TAPICareLinkTester.SetUp;
begin
  FCredsChangedCount := 0;
  FLastCreds := '';
end;

procedure TAPICareLinkTester.TearDown;
begin

end;

procedure TAPICareLinkTester.HandleCredsChanged(const newCreds: string);
begin
  Inc(FCredsChangedCount);
  FLastCreds := newCreds;
end;

procedure TAPICareLinkTester.TestEmptyCredentials;
var
  api: TrndiAPI;
begin
  // ParseCredentials rejects an empty blob before any network access
  api := CareLinkEU.Create('someuser', '');
  try
    AssertFalse('Connect must fail without token data', api.connect);
    AssertTrue('Error message mentions CareLink', Pos('CareLink', api.errormsg) > 0);
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestNonJsonCredentials;
var
  api: TrndiAPI;
begin
  api := CareLinkEU.Create('', 'this is not a json blob');
  try
    AssertFalse('Connect must fail on unparsable token data', api.connect);
    AssertTrue('Error message set', api.errormsg <> '');
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestMissingRefreshToken;
var
  api: TrndiAPI;
begin
  // A blob without refresh_token cannot keep the session alive
  api := CareLinkEU.Create('', '{"access_token":"abc","client_id":"x"}');
  try
    AssertFalse('Connect must fail without a refresh token', api.connect);
    AssertTrue('Error message set', api.errormsg <> '');
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestGetReadingsWithoutConnect;
var
  api: CareLink;
  readings: BGResults;
  res: string;
begin
  api := CareLinkEU.Create('', '{"refresh_token":"r","client_id":"c"}');
  try
    readings := api.GetReadings(1440, 10, '', res, false);
    AssertEquals('No readings before Connect', 0, Length(readings));
    AssertTrue('Status message says not authenticated', Pos('Not authenticated', res) > 0);
  finally
    api.Free;
  end;
end;

procedure TAPICareLinkTester.TestSupportsWebLogin;
begin
  // CareLink credentials come from the assisted browser login helper
  AssertTrue('CareLink uses the web login helper', CareLink.supportsWebLogin);
end;

procedure TAPICareLinkTester.TestParamLabels;
begin
  AssertTrue('User label set', CareLink.ParamLabel(APLUser) <> '');
  AssertTrue('Pass label set', CareLink.ParamLabel(APLPass) <> '');
  AssertTrue('Description mentions the browser login',
    Pos('browser', LowerCase(CareLink.ParamLabel(APLDesc))) > 0);
end;

procedure TAPICareLinkTester.TestCareLinkLocalServer;
var
  api: CareLink;
  readings: BGResults;
  BaseURL, creds, res: string;
  refreshesAfterConnect: integer;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestCareLinkLocalServer: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // token_url + data_url overrides skip live endpoint discovery entirely;
    // no access_token in the blob forces a refresh during Connect.
    creds := Format(
      '{"refresh_token":"initial-refresh","client_id":"test-client",' +
      '"token_url":"%s/carelink/token","data_url":"%s/carelink/display/message",' +
      '"patientId":"PAT1"}', [BaseURL, BaseURL]);

    api := CareLinkEU.Create('', creds);
    try
      api.OnCredentialsChanged := @HandleCredsChanged;

      if not api.Connect then
        Fail('CareLink connects to local fake server. Error: ' + api.errormsg);

      // Username comes from the access token's preferred_username claim
      AssertEquals('Username read from JWT claims', 'carelinkuser', api.Username);

      // The refresh rotated the token; the updated blob must be emitted for
      // persistence and keep the untouched keys as-is
      AssertEquals('One refresh during Connect', 1, FCredsChangedCount);
      AssertTrue('Rotated refresh token in emitted blob',
        Pos('rotated-refresh-', FLastCreds) > 0);
      AssertTrue('client_id preserved in emitted blob',
        Pos('"client_id" : "test-client"', FLastCreds) > 0);
      refreshesAfterConnect := FCredsChangedCount;

      readings := api.GetReadings(1440, 100, '', res, false);

      // 6 sgs entries, one of which is a gap (sg=0) that must be skipped
      AssertEquals('Gap slot skipped', 5, Length(readings));

      // Newest first after sorting
      AssertEquals('Newest reading value', 120.0, readings[0].convert(mgdl), 0.01);
      AssertTrue('Newest is most recent', readings[0].date > readings[1].date);

      // The device arrow (lastSGTrend=UP) overrides the computed trend on the
      // newest reading; older ones get the computed 5-min-normalized trend
      AssertEquals('Server arrow on newest', Ord(TdSingleUp), Ord(readings[0].trend));
      AssertEquals('Computed trend +10 mg/dL per 5 min', Ord(TdSingleUp), Ord(readings[1].trend));

      // Delta between consecutive readings (120 - 110)
      AssertEquals('Newest delta', 10.0, readings[0].convert(mgdl, BGDelta), 0.01);

      // Out-of-range values keep Trndi's sentinels: 450 -> 401, 30 -> 39
      AssertEquals('High clamp', 401.0, readings[3].convert(mgdl), 0.01);
      AssertEquals('Low clamp', 39.0, readings[4].convert(mgdl), 0.01);

      // Active insulin (IOB) parsed from the payload
      AssertEquals('Active insulin', 2.5, api.ActiveInsulin, 0.01);
      AssertTrue('Active insulin timestamp set', api.ActiveInsulinTime > 0);

      // The fresh token is valid for an hour: a second fetch must not refresh
      readings := api.GetReadings(1440, 100, '', res, false);
      AssertEquals('No extra refresh while token is fresh',
        refreshesAfterConnect, FCredsChangedCount);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

procedure TAPICareLinkTester.TestTransientServerErrorFailsConnect;
var
  api: TrndiAPI;
  BaseURL, creds: string;
begin
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
  begin
    Writeln('Skipping TestTransientServerErrorFailsConnect: embedded test server disabled (TRNDI_NO_TESTSERVER=1)');
    Exit;
  end;

  if not StartOrUseTestServer(BaseURL) then
    Fail('Failed to start or reach test server');

  try
    // Token refresh works, but the data endpoint 500s persistently: the
    // retry loop must give up and surface the HTTP status
    creds := Format(
      '{"refresh_token":"initial-refresh","client_id":"test-client",' +
      '"token_url":"%s/carelink/token","data_url":"%s/error500"}',
      [BaseURL, BaseURL]);

    api := CareLinkEU.Create('', creds);
    try
      AssertFalse('Connect fails when the data endpoint keeps erroring', api.connect);
      AssertTrue('Error message carries the HTTP status',
        Pos('HTTP 500', api.errormsg) > 0);
    finally
      api.Free;
    end;
  finally
    StopLocalTestServer;
  end;
end;

initialization

RegisterTest(TAPICareLinkTester);
end.
