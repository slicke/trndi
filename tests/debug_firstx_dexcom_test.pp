unit debug_firstx_dexcom_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  trndi.api.debug_firstxmissing, trndi.types, dateutils;

type
  TDexcomFirstXTester = class(TTestCase)
  published
    procedure TestDexcomModeSetsDeviceAndSource;
    procedure TestDexcomModeCreatesGap;
    procedure TestDexcomFutureTimestamp;
    procedure TestDexcomMissingDeltaClearsDelta;
  end;

implementation

procedure TDexcomFirstXTester.TestDexcomModeSetsDeviceAndSource;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('3:dexcom', '');
  try
    r := api.getReadings(0, 11, '', resStr);
    missing := 3;
    // After removing the first N readings the remaining count should be reduced
    AssertEquals('Remaining count should equal original minus missing', 11 - missing, Length(r));
    // ensure the earliest remaining reading looks Dexcom-like
    AssertEquals('Source should be Dexcom', 'Dexcom', r[0].Source);
    AssertTrue('Device should mention Dexcom', Pos('Dexcom', r[0].sensor) > 0);
  finally
    api.Free;
  end;
end;

procedure TDexcomFirstXTester.TestDexcomModeCreatesGap;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
  gap: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:dexcom', '');
  try
    r := api.getReadings(0, 11, '', resStr);
    missing := 2;
    gap := MinutesBetween(r[0].date, r[1].date);
    // Expect gap to be at least larger than normal 5-minute interval
    AssertTrue('Gap should be larger than a normal 5-minute interval', gap >= 15);
  finally
    api.Free;
  end;
end;

procedure TDexcomFirstXTester.TestDexcomFutureTimestamp;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
  secs: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:dexcom=future', '');
  try
    r := api.getReadings(0, 11, '', resStr);
    missing := 2;
    secs := SecondsBetween(r[0].date, r[1].date);
    // Future mode should produce a timestamp slightly later than the next (but not huge)
    AssertTrue('Future-mode timestamp should be slightly later than the next reading', (secs > 0) and (secs < 300));
  finally
    api.Free;
  end;
end;

procedure TDexcomFirstXTester.TestDexcomMissingDeltaClearsDelta;
var
  api: DebugFirstXMissingAPI;
  resStr: string;
  r: BGResults;
  missing: integer;
begin
  api := DebugFirstXMissingAPI.Create('2:dexcom=missing-delta', '');
  try
    r := api.getReadings(0, 11, '', resStr);
    missing := 2;
    AssertTrue('Delta should be empty for Dexcom missing-delta mode', r[0].deltaEmpty);
    AssertEquals('Source should be Dexcom', 'Dexcom', r[0].Source);
  finally
    api.Free;
  end;
end;

initialization
  RegisterTest(TDexcomFirstXTester);

end.
