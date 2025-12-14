unit trndiTestCase1;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.types, dialogs, dateutils,
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
  inherited Create('', '', '');
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
  api := NightScout.create('http://localhost', 'x', '');
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
  api := NightScout.create('http://localhost', 'x', '');
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
  api := Dexcom.create('test','test','usa');
  // Test if the connect function runs
  AssertFalse('API Connect Fail', api.connect);
  asserttrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  api.Free;
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
      api := NightScout.create('test','test','blah');
      AssertFalse('API Connect Fail', api.connect);
      AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
      api.Free;

      api := NightScout.create('http://localhost:8080','test22','');
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
