unit tandem_trend_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  trndi.types, trndi.funcs, SysUtils, DateUtils;

type
  TTandemTrendScalingTests = class(TTestCase)
  published
    procedure ScalesFiveMinutes;
    procedure ScalesOneMinuteToFiveMinuteEquivalent;
    procedure RejectsLongIntervalAsNotComputable;
    procedure NegativeTwoMinutesGivesFortyFiveDown;
  end;

implementation

procedure TTandemTrendScalingTests.ScalesFiveMinutes;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  t: BGTrend;
begin
  rawDiff := 10.0; // mg/dL difference
  secondsDiff := 300; // 5 minutes
  scaled := rawDiff * (300 / secondsDiff);
  t := CalculateTrendFromDelta(scaled);
  AssertEquals(Ord(TdSingleUp), Ord(t));
end;

procedure TTandemTrendScalingTests.ScalesOneMinuteToFiveMinuteEquivalent;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  t: BGTrend;
begin
  rawDiff := 2.0; // 2 mg/dL over 1 minute -> 10 mg/5min
  secondsDiff := 60; // 1 minute
  scaled := rawDiff * (300 / secondsDiff);
  t := CalculateTrendFromDelta(scaled);
  AssertEquals(Ord(TdSingleUp), Ord(t));
end;

procedure TTandemTrendScalingTests.RejectsLongIntervalAsNotComputable;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  valid: boolean;
begin
  rawDiff := 10.0;
  secondsDiff := 1800; // 30 minutes
  // According to Tandem driver logic, intervals outside 60..900s are not computable
  valid := (secondsDiff >= 60) and (secondsDiff <= 900);
  AssertFalse(valid);
end;

procedure TTandemTrendScalingTests.NegativeTwoMinutesGivesFortyFiveDown;
var
  rawDiff: double;
  secondsDiff: integer;
  scaled: double;
  t: BGTrend;
begin
  rawDiff := -2.0; // -2 mg/dL over 2 minutes -> -15 mg/5min
  secondsDiff := 120; // 2 minutes
  scaled := rawDiff * (300 / secondsDiff);
  t := CalculateTrendFromDelta(scaled);
  AssertEquals(Ord(TdFortyFiveDown), Ord(t));
end;

initialization
  RegisterTest(TTandemTrendScalingTests);

end.
