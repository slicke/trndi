unit debug_intermit_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  trndi.api.debug_intermittentmissing, trndi.types;

type
  TIntermittentTester = class(TTestCase)
  published
    procedure TestClearsWithinNewestRange;
    procedure TestClearedCountWithinBounds;
  end;

implementation

procedure TIntermittentTester.TestClearsWithinNewestRange;
var
  api: DebugIntermittentMissingAPI;
  resStr: string;
  r: BGResults;
  i, idx: Integer;
  anyClearedInFront: Boolean;
begin
  api := DebugIntermittentMissingAPI.Create('2-4', '');
  try
    anyClearedInFront := False;
    for i := 1 to 50 do
    begin
      r := api.getReadings(0, 11, '', resStr);
      // ensure we have at least 1 reading
      AssertTrue('Should return readings', Length(r) > 0);
      // check that cleared entries (exists=false) are within the first 10 (newest)
      for idx := 0 to Length(r)-1 do
      begin
        if not r[idx].exists then
        begin
          // cleared index must be within the most recent 10
          AssertTrue('Cleared index should be among newest 10', idx < 10);
          if idx < 3 then
            anyClearedInFront := True;
        end;
      end;
    end;
    // ensure some clears hit the very front region (newest few indices)
    AssertTrue('At least one cleared reading should be among the very newest indices', anyClearedInFront);
  finally
    api.Free;
  end;
end;

procedure TIntermittentTester.TestClearedCountWithinBounds;
var
  api: DebugIntermittentMissingAPI;
  resStr: string;
  r: BGResults;
  i, cleared, j: Integer;
begin
  api := DebugIntermittentMissingAPI.Create('2-4', '');
  try
    for i := 1 to 100 do
    begin
      r := api.getReadings(0, 11, '', resStr);
      cleared := 0;
      for j := 0 to High(r) do
        if not r[j].exists then Inc(cleared);
      // Cleared should be between 2 and 4, not exceeding 10
      AssertTrue('Cleared count >= 2', cleared >= 2);
      AssertTrue('Cleared count <= 4', cleared <= 4);
      AssertTrue('Cleared count <= length', cleared <= Length(r));
    end;
  finally
    api.Free;
  end;
end;

initialization
  RegisterTest(TIntermittentTester);

end.
