unit dexcom_time_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, SysUtils, DateUtils,
  trndi.api.dexcom_time;

type
  TDexcomTimeParsingTests = class(TTestCase)
  published
    procedure ParsesXmlSystemTime;
    procedure ParsesDateMs;
    procedure ParsesJsonServerTimeDateMs;
    procedure ParsesJsonServerTimeNumericMs;
    procedure ParsesIsoZ;
    procedure RejectsInvalid;
  end;

implementation

const
  ONE_SECOND: TDateTime = 1 / (24 * 60 * 60);

procedure TDexcomTimeParsingTests.ParsesXmlSystemTime;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('<SystemTime>2021-01-12T12:32:04</SystemTime>', dt));
  expected := EncodeDateTime(2021, 1, 12, 12, 32, 4, 0);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesDateMs;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('/Date(1610464324000)/', dt));
  expected := UnixToDateTime(1610464324, False);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesJsonServerTimeDateMs;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('{"ServerTime":"/Date(1610464324000)/"}', dt));
  expected := UnixToDateTime(1610464324, False);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesJsonServerTimeNumericMs;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('{"ServerTime":1610464324000}', dt));
  expected := UnixToDateTime(1610464324, False);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.ParsesIsoZ;
var
  dt, expected: TDateTime;
begin
  AssertTrue(ParseDexcomTime('2021-01-12T12:32:04Z', dt));
  // Note: ParseDexcomTime currently parses the timestamp and ignores the trailing 'Z'
  // (no timezone conversion), so expected is the literal date/time.
  expected := EncodeDateTime(2021, 1, 12, 12, 32, 4, 0);
  AssertTrue(Abs(dt - expected) < ONE_SECOND);
end;

procedure TDexcomTimeParsingTests.RejectsInvalid;
var
  dt: TDateTime;
begin
  AssertFalse(ParseDexcomTime('Banana', dt));
end;

initialization
  RegisterTest(TDexcomTimeParsingTests);

end.
