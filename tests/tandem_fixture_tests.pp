unit tandem_fixture_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTandemFixtureTests = class(TTestCase)
  published
    procedure TestParseCsvFixture;
  end;

implementation

procedure TTandemFixtureTests.TestParseCsvFixture;
var
  L: TStringList;
  i: Integer;
  parts: TStringArray;
  times: array of string;
  vals: array of Double;
  v: Double;
  minV, maxV: Double;
  totalDeltas: Integer;
  largeDeltas: Integer;
  delta: Double;
begin
  // Ensure '.' is used as decimal separator for reproducible parsing
  DecimalSeparator := '.';

  L := TStringList.Create;
  try
    L.LoadFromFile('tests/fixtures/tandem_1.csv');
    for i := 0 to L.Count - 1 do
    begin
      parts := L[i].Split(',');
      if Length(parts) >= 5 then
      begin
        if Trim(parts[2]) = 'EGV' then
        begin
          if TryStrToFloat(parts[4], v) then
          begin
            SetLength(vals, Length(vals) + 1);
            SetLength(times, Length(times) + 1);
            vals[High(vals)] := v;
            times[High(times)] := Trim(parts[3]);
          end;
        end;
      end;
    end;
  finally
    L.Free;
  end;

  // Basic assertions
  AssertTrue('No EGV readings found in fixture', Length(vals) > 0);
  AssertTrue('Insufficient EGV readings (expect > 100)', Length(vals) > 100);

  // Timestamps should be strictly increasing (ISO8601 lexicographic compare)
  for i := 0 to High(times) - 1 do
    AssertTrue(Format('Timestamps not increasing at index %d (%s >= %s)', [i, times[i], times[i+1]]), times[i] < times[i+1]);

  // Values should be within plausible mmol/L bounds and mostly smooth
  minV := vals[0]; maxV := vals[0];
  totalDeltas := 0; largeDeltas := 0;
  for i := 1 to High(vals) do
  begin
    if vals[i] < minV then minV := vals[i];
    if vals[i] > maxV then maxV := vals[i];
    delta := Abs(vals[i] - vals[i-1]);
    Inc(totalDeltas);
    if delta > 1.0 then // threshold: >1.0 mmol/L is considered a large jump
      Inc(largeDeltas);
  end;

  AssertTrue(Format('Min value too low: %.2f', [minV]), minV >= 2.0);
  AssertTrue(Format('Max value too high: %.2f', [maxV]), maxV <= 22.0);

  // Require that at least 95% of successive deltas are <= 1.0 mmol/L
  AssertTrue(Format('Too many large jumps: %d/%d', [largeDeltas, totalDeltas]), (totalDeltas > 0) and (largeDeltas * 100 / totalDeltas <= 5));
end;

initialization
  RegisterTest(TTandemFixtureTests);

end.
