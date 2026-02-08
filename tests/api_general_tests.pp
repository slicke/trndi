unit api_general_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
process, php_server_helper;

type

TAPIGeneralTester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestBaseLevelClassification;
  procedure TestJSToDateTimeNoCrash;
  procedure TestJSToDateTimeCorrection;
  procedure TestPredictReadingsInsufficientData;
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

procedure TAPIGeneralTester.TestJSToDateTimeCorrection;
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

procedure TAPIGeneralTester.TestBaseLevelClassification;
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

procedure TAPIGeneralTester.TestJSToDateTimeNoCrash;
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

procedure TAPIGeneralTester.TestPredictReadingsInsufficientData;
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

procedure TAPIGeneralTester.SetUp;
begin

end;

procedure TAPIGeneralTester.TearDown;
begin

end;

initialization

RegisterTest(TAPIGeneralTester);
end.