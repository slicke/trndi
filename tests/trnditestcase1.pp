unit trndiTestCase1;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
process, php_server_helper;

type

TAPITester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestBaseLevelClassification;
  procedure TestJSToDateTimeNoCrash;
  procedure TestJSToDateTimeCorrection;
  procedure TestPredictReadingsInsufficientData;
  procedure TestDexcom;
  procedure TestDexcomLocalServer;
  procedure TestXDrip;
  procedure TestXDripLocalServer;
  procedure TestNightscoutInvalidUrl;
  procedure TestNightscoutUnauthorized;
  procedure TestNightscoutGetReadingsRespectsMax;
  procedure TestNightScout;
end;

implementation

type
  TFakeAPI = class(TrndiAPI)
  private
    FReadings: BGResults;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function connect: boolean; override;
    function getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults; override;
    procedure SetReadings(const AReadings: BGResults);
    // Test helper to set tz (minutes) since setTZ is protected
    procedure SetTZPublic(secsMin: integer);
  end;

constructor TFakeAPI.Create;
begin
  inherited Create('', '');
  SetLength(FReadings, 0);
end;

destructor TFakeAPI.Destroy;
begin
  SetLength(FReadings, 0);
  inherited Destroy;
end;

function TFakeAPI.connect: boolean;
begin
  Result := true;
end;

function TFakeAPI.getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults;
var
  n, i: integer;
begin
  res := '';
  n := Length(FReadings);
  if (maxNum > 0) and (n > maxNum) then
    n := maxNum;
  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := FReadings[i];
end;

procedure TFakeAPI.SetReadings(const AReadings: BGResults);
var
  i: integer;
begin
  SetLength(FReadings, Length(AReadings));
  for i := 0 to High(AReadings) do
    FReadings[i] := AReadings[i];
end;

// Test helper to set tz (minutes)
procedure TFakeAPI.SetTZPublic(secsMin: integer);
begin
  setTZ(secsMin);
end;

procedure TAPITester.TestJSToDateTimeCorrection;
var
  api: TFakeAPI;
  ts_ms: int64;
  dt, expected: TDateTime;
begin
  api := TFakeAPI.Create;
  try
    // 2020-01-01T00:00:00Z
    ts_ms := 1577836800000;

    // Without correction, timestamp should be interpreted as UTC and converted to local
    dt := api.JSToDateTime(ts_ms, False);
    expected := UnixToDateTime(ts_ms div 1000, False);
    AssertEquals('JSToDateTime without correction matches UTC->local',
      FormatDateTime('yyyy-mm-dd hh:nn:ss', expected),
      FormatDateTime('yyyy-mm-dd hh:nn:ss', dt));

    // With tz set (60 minutes), applying correction should shift by tz seconds
    api.SetTZPublic(60); // 60 minutes -> tz = 3600s
    dt := api.JSToDateTime(ts_ms, True);
    expected := UnixToDateTime((ts_ms div 1000) - 3600, False);
    AssertEquals('JSToDateTime with tz applied shifts time by tz',
      FormatDateTime('yyyy-mm-dd hh:nn:ss', expected),
      FormatDateTime('yyyy-mm-dd hh:nn:ss', dt));
  finally
    api.Free;
  end;
end;

procedure TAPITester.TestBaseLevelClassification;
var
  api: NightScout;
begin
  api := NightScout.create('http://localhost', 'x');
  try
    api.cgmHi := 180;
    api.cgmLo := 70;
    api.cgmRangeHi := 160;
    api.cgmRangeLo := 80;

    AssertEquals('High boundary is high', Ord(BGHIGH), Ord(api.getLevel(180)));
    AssertEquals('Low boundary is low', Ord(BGLOW), Ord(api.getLevel(70)));
    AssertEquals('Top-range boundary is BGRangeHI', Ord(BGRangeHI), Ord(api.getLevel(160)));
    AssertEquals('Bottom-range boundary is BGRangeLO', Ord(BGRangeLO), Ord(api.getLevel(80)));
    AssertEquals('In-range value is BGRange', Ord(BGRange), Ord(api.getLevel(120)));
  finally
    api.Free;
  end;
end;

procedure TAPITester.TestJSToDateTimeNoCrash;
var
  api: NightScout;
  dt1, dt2: TDateTime;
begin
  api := NightScout.create('http://localhost', 'x');
  try
    dt1 := api.JSToDateTime(0, true);
    dt2 := api.JSToDateTime(0, false);
    AssertTrue('JSToDateTime returns valid TDateTime', (dt1 > 0) or (dt1 = 0));
    AssertTrue('JSToDateTime correct flag does not crash', (dt2 > 0) or (dt2 = 0));
  finally
    api.Free;
  end;
end;

procedure TAPITester.TestPredictReadingsInsufficientData;
var
  api: TFakeAPI;
  preds: BGResults;
  readings: BGResults;
begin
  api := TFakeAPI.Create;
  try
    // 0 readings
    SetLength(readings, 0);
    api.SetReadings(readings);
    AssertFalse('predictReadings fails with 0 readings', api.predictReadings(3, preds));

    // 2 readings
    SetLength(readings, 2);
    readings[0].Init(mgdl);
    readings[0].update(100, BGPrimary, mgdl);
    readings[0].date := Now - EncodeTime(0, 10, 0, 0);
    readings[1].Init(mgdl);
    readings[1].update(105, BGPrimary, mgdl);
    readings[1].date := Now;
    api.SetReadings(readings);
    AssertFalse('predictReadings fails with 2 readings', api.predictReadings(3, preds));
  finally
    api.Free;
  end;
end;

procedure TAPITester.TestDexcom;
var
  api: TrndiAPI;
begin
  // Can create
  api := DexcomUSA.Create('test', 'test');
  // Test if the connect function runs
  AssertFalse('API Connect Fail', api.connect);
  asserttrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  api.Free;
end;

procedure TAPITester.TestDexcomLocalServer;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  res: string;
  readings: BGResults;
  BaseURL: string;
begin
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    // Use the new "full URL" override for Dexcom baseUrl.
    api := DexcomCustom.Create('anyuser', 'anypass', BaseURL + '/ShareWebServices/Services');
    try
      AssertTrue('Dexcom connects to local fake server', api.connect);
      readings := api.getReadings(30, 3, '', res);
      AssertTrue('Dexcom returns at least one reading', Length(readings) > 0);
      AssertTrue('Dexcom reading value set', readings[0].val > 0);
      AssertTrue('Dexcom reading timestamp set', readings[0].date > 0);
    finally
      api.Free;
    end;
  finally
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;

procedure TAPITester.TestXDrip;
var
  api: TrndiAPI;
begin
  // Can create
  api := xDrip.create('http://localhost:8080', 'testsecret');
  try
    // Test if the connect function runs (will fail without server)
    AssertFalse('API Connect should fail without server', api.connect);
    AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  finally
    api.Free;
  end;
end;

procedure TAPITester.TestXDripLocalServer;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  res: string;
  readings: BGResults;
  bg: BGReading;
  BaseURL: string;
begin
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    api := xDrip.create(BaseURL, 'test22');
    try
      if not api.connect then
        Fail('xDrip connects to local fake server. Error: ' + api.errormsg);
      AssertTrue('xDrip connected', true);
      
      // Test thresholds from status.json (same as Nightscout)
      AssertEquals('xDrip bgHigh threshold mapped', 260, api.cgmHi);
      AssertEquals('xDrip bgLow threshold mapped', 55, api.cgmLo);
      
      // Test getting current reading from pebble endpoint
      bg.Clear;
      AssertTrue('xDrip getCurrent returns data', api.getCurrent(bg));
      AssertTrue('Current reading has plausible value', bg.val > 0);
      AssertTrue('Current reading has plausible timestamp', bg.date > 0);
      
      // Test getting multiple readings
      readings := api.getReadings(30, 3, '', res);
      AssertTrue('xDrip returns at least one reading', Length(readings) > 0);
      AssertTrue('xDrip reading value set', readings[0].val > 0);
      AssertTrue('xDrip reading timestamp set', readings[0].date > 0);
    finally
      api.Free;
    end;
  finally
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;

procedure TAPITester.TestNightscoutInvalidUrl;
var
  api: NightScout;
begin
  api := NightScout.create('not-a-url', 'x');
  try
    AssertFalse('Invalid URL should not connect', api.connect);
  finally
    api.Free;
  end;
end;

procedure TAPITester.TestNightscoutUnauthorized;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  BaseURL: string;
begin
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    // Wrong secret should yield Unauthorized from the fake server
    api := NightScout.create(BaseURL, 'wrongsecret');
    AssertFalse('Connect should fail for unauthorized secret', api.connect);
    api.Free;
  finally
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;

procedure TAPITester.TestNightscoutGetReadingsRespectsMax;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  res: string;
  readings: BGResults;
  BaseURL: string;
begin
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    api := NightScout.create(BaseURL, 'test22');
    try
      AssertTrue('Connect to local NS', api.connect);

      readings := api.getReadings(0, 3, '', res);
      AssertTrue('Should not exceed requested max count', Length(readings) <= 3);
    finally
      api.Free;
    end;
  finally
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;

procedure TAPITester.TestNightscout;
var
  api: TrndiAPI;
  bg: BGreading;
  PHPProcess: TProcess;
  BaseURL: string;
begin
  bg.Clear;
  // Allow skipping integration tests via TRNDI_NO_PHP=1
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
    Exit; // Skip integration test when PHP is intentionally disabled

  // Start or reuse PHP server (use TRNDI_TEST_SERVER_URL if set)
  PHPProcess := nil;
  if not StartOrUseTestServer(PHPProcess, BaseURL) then
    Fail('Failed to start or reach test PHP server');

  try
    // Your existing test code
    api := NightScout.create('test','test');
    try
      AssertFalse('API Connect Fail', api.connect);
      AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
    finally
      api.Free;
    end;

    api := NightScout.create(BaseURL,'test22');
    try
      asserttrue('Connect to local NS', api.connect);
      // Thresholds are provided by the fake server in status.json
      AssertEquals('NS bgHigh threshold mapped', 260, api.cgmHi);
      AssertEquals('NS bgLow threshold mapped', 55, api.cgmLo);
      AssertEquals('NS bgTargetTop threshold mapped', 180, api.cgmRangeHi);
      AssertEquals('NS bgTargetBottom threshold mapped', 80, api.cgmRangeLo);
      asserttrue('Get readings', api.getCurrent(bg));
      AssertTrue('Current reading has plausible value', (bg.val > 0));
      AssertTrue('Current reading has plausible timestamp', (bg.date > 0));
    finally
      api.free;
    end;

  finally
    // Terminate & free PHP server
    if Assigned(PHPProcess) then
    begin
      PHPProcess.Terminate(0);
      PHPProcess.Free;
    end;
  end;
end;


procedure TAPITester.SetUp;
begin

end;

procedure TAPITester.TearDown;
begin

end;

initialization

RegisterTest(TAPITester);
end.
