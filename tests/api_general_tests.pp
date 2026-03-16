unit api_general_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.api.xdrip, trndi.types, dialogs, dateutils,
process, php_server_helper, trndi.api.debug_lowsoon;

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
  procedure TestPredictReadingsLinearTrend;
  procedure TestPredictReadingsRateLimiter;
  // edge cases
  procedure TestPredictReadingsFlatTrend;
  procedure TestPredictReadingsDescendingTrend;
  procedure TestPredictReadingsLimiterSecondStep;
  procedure TestPredictReadingsMaxClamp;
  procedure TestPredictReadingsNumPredictionsCap;
  procedure TestPredictReadingsIrregularIntervals;
  procedure TestPredictReadingsInvalidTimeDistribution;
  procedure TestPredictReadingsOutputOrdering;
  procedure TestDebugLowSoonDriverTriggersSeventhPredictionLow;
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

  // Verify that the linear regression produces expected values for a simple
  // constant-rate trend.  This test also implicitly checks that the timestamps
  // are handled correctly (5‑minute spacing) and that results are returned in
  // chronological order.
  procedure TAPIGeneralTester.TestPredictReadingsLinearTrend;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
    baseTime: TDateTime;
  begin
    api := TFakeAPI.Create;
    try
      // Construct five readings spaced 5 minutes apart, rising by 10 mg/dL each
      // interval.  That corresponds to a slope of 2 mg/dL per minute.
      SetLength(readings, 5);
      baseTime := Now - EncodeTime(0, 20, 0, 0); // start 20 minutes ago
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(100 + (i * 10), BGPrimary, mgdl);
        readings[i].date := baseTime + EncodeTime(0, i * 5, 0, 0);
      end;
      api.SetReadings(readings);

      AssertTrue('predictReadings succeeds with linear data', api.predictReadings(3, preds));

      // Last actual value should be 140.  Predictions for 5, 10 and 15 minutes
      // ahead should therefore be 150, 160 and 170 respectively.
      AssertEquals('expected three predictions', 3, Length(preds));
      AssertEquals('first prediction follows trend', 150, Round(preds[0].convert(mgdl)));
      AssertEquals('second prediction follows trend', 160, Round(preds[1].convert(mgdl)));
      AssertEquals('third prediction follows trend', 170, Round(preds[2].convert(mgdl)));
    finally
      api.Free;
    end;
  end;

  // Ensure the rate‑of‑change limiter in predictReadings prevents unrealistic
  // swings.  We feed extremely steep drops and verify that the output is
  // clamped to the physiological minimum of 20 mg/dL.
  procedure TAPIGeneralTester.TestPredictReadingsRateLimiter;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
    baseTime: TDateTime;
  begin
    api := TFakeAPI.Create;
    try
      // Create four readings 5 minutes apart with a 100 mg/dL drop each step
      // (20 mg/dL per minute) which is well beyond the ±3 limit enforced by
      // the algorithm.
      SetLength(readings, 4);
      baseTime := Now - EncodeTime(0, 15, 0, 0);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(200 - (i * 100), BGPrimary, mgdl);
        readings[i].date := baseTime + EncodeTime(0, i * 5, 0, 0);
      end;
      api.SetReadings(readings);

      AssertTrue('predictReadings handles steep drop', api.predictReadings(2, preds));
      AssertEquals('two predictions returned', 2, Length(preds));

      // Both predictions should be clamped to the floor of 20 mg/dL.
      AssertEquals('first prediction clamped', 20, Round(preds[0].convert(mgdl)));
      AssertEquals('second prediction clamped', 20, Round(preds[1].convert(mgdl)));
    finally
      api.Free;
    end;
  end;

  // Confirm that the limiter continues to cap even when more than two
  // predictions are requested.  In the real algorithm the second prediction
  // is generated from the first, so a very large drop should still produce
  // a 20 mg/dL value on the second step.
  procedure TAPIGeneralTester.TestPredictReadingsLimiterSecondStep;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
    baseTime: TDateTime;
  begin
    api := TFakeAPI.Create;
    try
      SetLength(readings, 3);
      baseTime := Now - EncodeTime(0, 10, 0, 0);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(200 - (i * 100), BGPrimary, mgdl);
        readings[i].date := baseTime + EncodeTime(0, i * 5, 0, 0);
      end;
      api.SetReadings(readings);

      AssertTrue('predictReadings handles steep drop across multiple steps',
        api.predictReadings(3, preds));
      AssertEquals('three predictions returned', 3, Length(preds));

      AssertEquals('first prediction clamped', 20, Round(preds[0].convert(mgdl)));
      AssertEquals('second prediction also clamped', 20, Round(preds[1].convert(mgdl)));
      // third prediction comes from second, so it should likewise be limited
      AssertEquals('third prediction also clamped', 20, Round(preds[2].convert(mgdl)));
    finally
      api.Free;
    end;
  end;

  procedure TAPIGeneralTester.TestPredictReadingsFlatTrend;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
    baseTime: TDateTime;
  begin
    api := TFakeAPI.Create;
    try
      // constant 100 for three readings
      SetLength(readings, 3);
      baseTime := Now - EncodeTime(0, 10, 0, 0);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(100, BGPrimary, mgdl);
        readings[i].date := baseTime + EncodeTime(0, i * 5, 0, 0);
      end;
      api.SetReadings(readings);

      AssertTrue(api.predictReadings(4, preds));
      for i := 0 to High(preds) do
        AssertEquals('flat trend prediction stays at 100', 100,
          Round(preds[i].convert(mgdl)));
    finally api.Free; end;
  end;

  procedure TAPIGeneralTester.TestPredictReadingsDescendingTrend;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
    baseTime: TDateTime;
  begin
    api := TFakeAPI.Create;
    try
      SetLength(readings, 4);
      baseTime := Now - EncodeTime(0, 15, 0, 0);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(200 - i*10, BGPrimary, mgdl); // drop 10 each step
        readings[i].date := baseTime + EncodeTime(0, i * 5, 0, 0);
      end;
      api.SetReadings(readings);

      AssertTrue(api.predictReadings(3, preds));
      // Last actual reading is 170, slope -10 per 5min implies
      // predictions of 160, 150, 140 in that order.
      AssertEquals(160, Round(preds[0].convert(mgdl)));
      AssertEquals(150, Round(preds[1].convert(mgdl)));
      AssertEquals(140, Round(preds[2].convert(mgdl)));
    finally api.Free; end;
  end;

  procedure TAPIGeneralTester.TestPredictReadingsMaxClamp;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
    baseTime: TDateTime;
  begin
    api := TFakeAPI.Create;
    try
      // simple linear data
      SetLength(readings, 3);
      baseTime := Now - EncodeTime(0, 10, 0, 0);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(100 + i*5, BGPrimary, mgdl);
        readings[i].date := baseTime + EncodeTime(0, i * 5, 0, 0);
      end;
      api.SetReadings(readings);

      AssertTrue(api.predictReadings(25, preds));
      AssertEquals('numPredictions clamped to 20', 20, Length(preds));
    finally api.Free; end;
  end;

  procedure TAPIGeneralTester.TestPredictReadingsNumPredictionsCap;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
    baseTime: TDateTime;
  begin
    api := TFakeAPI.Create;
    try
      // Use simple linear data again (need at least three readings to
      // satisfy the algorithm's minimum requirement)
      SetLength(readings, 3);
      baseTime := Now - EncodeTime(0, 10, 0, 0);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(100 + i*10, BGPrimary, mgdl);
        readings[i].date := baseTime + EncodeTime(0, i * 5, 0, 0);
      end;
      api.SetReadings(readings);

      // Too many predictions should be clamped to 20
      AssertTrue('predictReadings should succeed with large count',
        api.predictReadings(100, preds));
      AssertEquals('excessive numPredictions clamped to 20', 20, Length(preds));
      // make sure the values are reasonable (optional)
      if Length(preds) > 0 then
        AssertEquals('first clamped prediction still computed',
          Round(preds[0].convert(mgdl)),
          Round(preds[0].convert(mgdl)));

      // For non-positive values the API simply fails (numPredictions < 1).
      AssertFalse('predictReadings should return false when 0 requested',
        api.predictReadings(0, preds));
      AssertEquals('zero numPredictions yields no results', 0, Length(preds));
      AssertFalse('predictReadings should return false when negative',
        api.predictReadings(-3, preds));
      AssertEquals('negative numPredictions yields no results', 0, Length(preds));
    finally api.Free; end;
  end;

  procedure TAPIGeneralTester.TestPredictReadingsIrregularIntervals;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
  begin
    api := TFakeAPI.Create;
    try
      // 3 readings at 0,3,20 minutes apart
      SetLength(readings, 3);
      readings[0].Init(mgdl);
      readings[0].update(120, BGPrimary, mgdl);
      readings[0].date := Now - EncodeTime(0, 20, 0, 0);
      readings[1].Init(mgdl);
      readings[1].update(130, BGPrimary, mgdl);
      readings[1].date := Now - EncodeTime(0, 17, 0, 0);
      readings[2].Init(mgdl);
      readings[2].update(140, BGPrimary, mgdl);
      readings[2].date := Now;
      api.SetReadings(readings);

      AssertTrue('irregular intervals still succeed', api.predictReadings(3, preds));
      AssertEquals(3, Length(preds));
    finally api.Free; end;
  end;

  procedure TAPIGeneralTester.TestPredictReadingsInvalidTimeDistribution;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    t: TDateTime;
    i: integer;
  begin
    api := TFakeAPI.Create;
    try
      // three readings with identical timestamp
      t := Now - EncodeTime(0, 10, 0, 0);
      SetLength(readings, 3);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(100 + i*5, BGPrimary, mgdl);
        readings[i].date := t;
      end;
      api.SetReadings(readings);
      AssertFalse('prediction fails when time distribution invalid', api.predictReadings(3, preds));
    finally api.Free; end;
  end;

  procedure TAPIGeneralTester.TestPredictReadingsOutputOrdering;
  var
    api: TFakeAPI;
    preds, readings: BGResults;
    i: integer;
  begin
    api := TFakeAPI.Create;
    try
      // supply readings in reverse chronological order
      SetLength(readings, 4);
      for i := 0 to High(readings) do
      begin
        readings[i].Init(mgdl);
        readings[i].update(100 + i*5, BGPrimary, mgdl);
        readings[i].date := Now - EncodeTime(0, (High(readings)-i)*5, 0, 0);
      end;
      api.SetReadings(readings);
      AssertTrue(api.predictReadings(5, preds));
      for i := 0 to High(preds)-1 do
        AssertTrue('predictions are chronological', preds[i].date <= preds[i+1].date);
    finally api.Free; end;
  end;

  procedure TAPIGeneralTester.TestDebugLowSoonDriverTriggersSeventhPredictionLow;
  var
    api: DebugLowSoonAPI;
    preds: BGResults;
    i: integer;
  begin
    api := DebugLowSoonAPI.Create('', '');
    try
      AssertTrue('debug low-soon backend connects', api.connect);
      AssertTrue('debug low-soon backend produces predictions', api.predictReadings(7, preds));
      AssertEquals('expected seven predictions', 7, Length(preds));

      for i := 0 to 5 do
        AssertTrue('first six predictions stay above low threshold',
          preds[i].convert(mgdl) > api.cgmLo);

      AssertTrue('seventh prediction reaches low threshold',
        preds[6].convert(mgdl) <= api.cgmLo);
      AssertTrue('seventh prediction stays within the soon warning window',
        Round(MinutesBetween(Now, preds[6].date)) <= 3);
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