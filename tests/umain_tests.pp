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
  end;

implementation

procedure TUmainTests.TestGetValidatedPositionDefault;
var
  g: TfBG;
  n: TTrndiNativeBase;
begin
  // Ensure we use the mock native implementation for tests
  n := TrndiNative.Create;
  try
    native := n;
    native.SetSetting('position.main', '9999'); // invalid/unknown value
    g := TfBG.Create;
    try
      fBG := g; // set global reference used by helpers
      AssertEquals('Invalid position should fall back to center', Ord(tpoCenter), Ord(g.GetValidatedPosition));
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
  n: TTrndiNativeBase;
begin
  n := TrndiNative.Create;
  try
    native := n;
    native.SetSetting('position.main', IntToStr(Ord(tpoBottomRight)));
    g := TfBG.Create;
    try
      fBG := g;
      AssertEquals('Valid position stored should be returned', Ord(tpoBottomRight), Ord(g.GetValidatedPosition));
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
  n: TTrndiNativeBase;
  p: TPanel;
  i: integer;
  lbl: TLabel;
  created: array[1..10] of TLabel;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      // Create a parent panel with limited client height
      p := TPanel.Create;
      p.Height := 40;

      // Initialize TrendDots with labels and attach to parent
      for i := 1 to 10 do
      begin
        created[i] := TLabel.Create;
        created[i].Parent := p;
        created[i].Visible := False;
        g.TrendDots[i] := TDotControl(created[i]);
      end;

      // Make one dot overflow above the top
      lbl := created[3];
      lbl.Top := -12;
      lbl.Height := 8;
      lbl.Visible := True;

      // Ensure others remain invisible
      AssertEquals('Top overflow should be detected as negative offset', -12, g.dotsInView);
    finally
      // Free created labels and panel
      for i := 1 to 10 do
        created[i].Free;
      p.Free;
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
  n: TTrndiNativeBase;
  p: TPanel;
  i: integer;
  lbl: TLabel;
  created: array[1..10] of TLabel;
  expectedOverflow: integer;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      // Create a parent panel with limited client height
      p := TPanel.Create;
      p.Height := 40;

      // Initialize TrendDots with labels and attach to parent
      for i := 1 to 10 do
      begin
        created[i] := TLabel.Create;
        created[i].Parent := p;
        created[i].Visible := False;
        g.TrendDots[i] := TDotControl(created[i]);
      end;

      // Make one dot overflow below the bottom
      lbl := created[7];
      lbl.Top := 38; // bottom = Top + Height -> 38 + 10 = 48
      lbl.Height := 10;
      lbl.Visible := True;

      expectedOverflow := (lbl.Top + lbl.Height) - (p.ClientHeight + 5); // Tol=5 in implementation
      AssertEquals('Bottom overflow should return positive overflow value', expectedOverflow, g.dotsInView);
    finally
      for i := 1 to 10 do
        created[i].Free;
      p.Free;
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
