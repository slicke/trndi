unit trndiTestCase1;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
process;

type

TAPITester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestBaseLevelClassification;
  procedure TestJSToDateTimeNoCrash;
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
begin
  // Start PHP server (same harness as Nightscout tests)
  PHPProcess := TProcess.Create(nil);
  try
    PHPProcess.Executable :={$ifdef Windows}'c:\tools\php84\'+{$endif}'php';
    PHPProcess.Parameters.Add('-S');
    PHPProcess.Parameters.Add('localhost:8080');
    PHPProcess.Parameters.Add('-t');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'testserver');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'router.php');
    PHPProcess.Options := [poNoConsole];
    PHPProcess.Execute;
    Sleep(1000);

    try
      // Use the new "full URL" override for Dexcom baseUrl.
      api := DexcomCustom.Create('anyuser', 'anypass', 'http://localhost:8080/ShareWebServices/Services');
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
      PHPProcess.Terminate(0);
    end;
  finally
    PHPProcess.Free;
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
begin
  // Start PHP server
  PHPProcess := TProcess.Create(nil);
  try
    PHPProcess.Executable :={$ifdef Windows}'c:\tools\php84\'+{$endif}'php';
    PHPProcess.Parameters.Add('-S');
    PHPProcess.Parameters.Add('localhost:8080');
    PHPProcess.Parameters.Add('-t');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'testserver');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'router.php');
    PHPProcess.Options := [poNoConsole];
    PHPProcess.Execute;
    Sleep(1000);

    try
      api := xDrip.create('http://localhost:8080', 'test22');
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
      PHPProcess.Terminate(0);
    end;
  finally
    PHPProcess.Free;
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
begin
  // Start PHP server
  PHPProcess := TProcess.Create(nil);
  try
    PHPProcess.Executable :={$ifdef Windows}'c:\tools\php84\'+{$endif}'php';
    PHPProcess.Parameters.Add('-S');
    PHPProcess.Parameters.Add('localhost:8080');
    PHPProcess.Parameters.Add('-t');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'testserver');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'router.php');
    PHPProcess.Options := [poNoConsole];
    PHPProcess.Execute;
    Sleep(1000);

    try
      // Wrong secret should yield Unauthorized from the fake server
      api := NightScout.create('http://localhost:8080', 'wrongsecret');
      AssertFalse('Connect should fail for unauthorized secret', api.connect);
      api.Free;
    finally
      PHPProcess.Terminate(0);
    end;
  finally
    PHPProcess.Free;
  end;
end;

procedure TAPITester.TestNightscoutGetReadingsRespectsMax;
var
  api: TrndiAPI;
  PHPProcess: TProcess;
  res: string;
  readings: BGResults;
begin
  PHPProcess := TProcess.Create(nil);
  try
    PHPProcess.Executable :={$ifdef Windows}'c:\tools\php84\'+{$endif}'php';
    PHPProcess.Parameters.Add('-S');
    PHPProcess.Parameters.Add('localhost:8080');
    PHPProcess.Parameters.Add('-t');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'testserver');
    PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'router.php');
    PHPProcess.Options := [poNoConsole];
    PHPProcess.Execute;
    Sleep(1000);

    try
      api := NightScout.create('http://localhost:8080', 'test22');
      AssertTrue('Connect to local NS', api.connect);

      readings := api.getReadings(0, 3, '', res);
      AssertTrue('Should not exceed requested max count', Length(readings) <= 3);
      api.Free;
    finally
      PHPProcess.Terminate(0);
    end;
  finally
    PHPProcess.Free;
  end;
end;

procedure TAPITester.TestNightscout;
var
  api: TrndiAPI;
  bg: BGreading;
  PHPProcess: TProcess;
begin
  bg.Clear;
  // Start PHP server
  PHPProcess := TProcess.Create(nil);
  try
    PHPProcess.Executable :={$ifdef Windows}'c:\tools\php84\'+{$endif}'php'; // Choco path for win
    PHPProcess.Parameters.Add('-S');
    PHPProcess.Parameters.Add('localhost:8080');
      PHPProcess.Parameters.Add('-t');
      // Use OS-agnostic path joining so tests work on both Windows and Unix-like systems
      PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'testserver');
      // Use the router so the built-in server forwards API paths to index.php
      PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'router.php');
    PHPProcess.Options := [poNoConsole];  // Run without console window
    PHPProcess.Execute;

    // Wait a bit for the server to start
    Sleep(1000);  // Give it a second to start up

    try
      // Your existing test code
      api := NightScout.create('test','test');
      AssertFalse('API Connect Fail', api.connect);
      AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
      api.Free;

      api := NightScout.create('http://localhost:8080','test22');
      asserttrue('Connect to local NS', api.connect);
      // Thresholds are provided by the fake server in status.json
      AssertEquals('NS bgHigh threshold mapped', 260, api.cgmHi);
      AssertEquals('NS bgLow threshold mapped', 55, api.cgmLo);
      AssertEquals('NS bgTargetTop threshold mapped', 180, api.cgmRangeHi);
      AssertEquals('NS bgTargetBottom threshold mapped', 80, api.cgmRangeLo);
      asserttrue('Get readings', api.getCurrent(bg));
      AssertTrue('Current reading has plausible value', (bg.val > 0));
      AssertTrue('Current reading has plausible timestamp', (bg.date > 0));
      api.free;

    finally
      // Terminate PHP server
      PHPProcess.Terminate(0);
    end;

  finally
    PHPProcess.Free;
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
