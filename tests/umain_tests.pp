unit umain_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, umain, SysUtils, StdCtrls, ExtCtrls, Classes, trndi.native;

type
  TUmainTests = class(TTestCase)
  published
    procedure TestGetValidatedPositionDefault;
    procedure TestGetValidatedPositionValid;
    procedure TestDotsInViewTopOverflow;
    procedure TestDotsInViewBottomOverflow;
    procedure TestDotsInViewNoDots;
    procedure TestDotsInViewIntegration;
  end;

implementation

procedure TUmainTests.TestGetValidatedPositionDefault;
var
  g: TfBG;
  n: TrndiNative;
begin
  // Ensure we use the mock native implementation for tests
  n := TrndiNative.Create;
  try
    native := n;
    native.SetSetting('position.main', '9999'); // invalid/unknown value
    g := TfBG.Create;
    try
      fBG := g; // set global reference used by helpers
      AssertEquals('Invalid position should fall back to center', Ord(tpoCenter), Ord(g.GetValidatedPositionForTests));
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    n.Free;
    native := nil;
  end;
end;

procedure TUmainTests.TestGetValidatedPositionValid;
var
  g: TfBG;
  n: TrndiNative;
begin
  n := TrndiNative.Create;
  try
    native := n;
    native.SetSetting('position.main', IntToStr(Ord(tpoBottomRight)));
    g := TfBG.Create;
    try
      fBG := g;
      AssertEquals('Valid position stored should be returned', Ord(tpoBottomRight), Ord(g.GetValidatedPositionForTests));
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    n.Free;
    native := nil;
  end;
end;

procedure TUmainTests.TestDotsInViewTopOverflow;
var
  g: TfBG;
  n: TrndiNative;
  i: integer;
  dots: array[1..10] of TDotInfo;
  expected: integer;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      // Parent height (client) for the test

      // Initialize all dots to invisible
      for i := 1 to 10 do
      begin
        dots[i].Visible := False;
        dots[i].Top := 0;
        dots[i].Height := 0;
      end;

      // Make one dot overflow above the top
      dots[3].Top := -12;
      dots[3].Height := 8;
      dots[3].Visible := True;

      // Ensure others remain invisible
      try
        AssertEquals('Top overflow should be detected as negative offset', -12, g.DotsInViewForTestsFromInfos(dots, 40));
      except
        on E: Exception do
          Fail('DotsInView crashed: ' + E.ClassName + ': ' + E.Message);
      end;
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    n.Free;
    native := nil;
  end;
end;

procedure TUmainTests.TestDotsInViewBottomOverflow;
var
  g: TfBG;
  n: TrndiNative;
  i: integer;
  dots: array[1..10] of TDotInfo;
  expectedOverflow: integer;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;

      // Initialize all dots to invisible
      for i := 1 to 10 do
      begin
        dots[i].Visible := False;
        dots[i].Top := 0;
        dots[i].Height := 0;
      end;

      // Make one dot overflow below the bottom
      dots[7].Top := 38; // bottom = Top + Height -> 38 + 10 = 48
      dots[7].Height := 10;
      dots[7].Visible := True;

      expectedOverflow := (dots[7].Top + dots[7].Height) - (40 + 5); // Tol=5 in implementation
      try
        AssertEquals('Bottom overflow should return positive overflow value', expectedOverflow, g.DotsInViewForTestsFromInfos(dots, 40));
      except
        on E: Exception do
          Fail('DotsInView crashed: ' + E.ClassName + ': ' + E.Message);
      end;
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    n.Free;
    native := nil;
  end;
end;

procedure TUmainTests.TestDotsInViewNoDots;
var
  g: TfBG;
  n: TrndiNative;
  emptyDots: array of TDotInfo;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      // No dots set, should return 0
      try
        // Call the pure helper with an empty array
        SetLength(emptyDots, 0);
        AssertEquals('No dots should yield zero offset', 0, g.DotsInViewForTestsFromInfos(emptyDots, 40));
      except
        on E: Exception do
          Fail('DotsInView crashed (no dots): ' + E.ClassName + ': ' + E.Message);
      end;
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    n.Free;
    native := nil;
  end;
end;

initialization
  RegisterTest(TUmainTests);

end.
