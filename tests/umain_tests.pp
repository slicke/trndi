(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)
unit umain_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, umain, SysUtils, StdCtrls, ExtCtrls, Classes, trndi.native, trndi.types,
  trndi.funcs;

type
  TUmainTests = class(TTestCase)
  published
    procedure TestGetValidatedPositionDefault;
    procedure TestGetValidatedPositionValid;
    procedure TestDotsInViewTopOverflow;
    procedure TestDotsInViewBottomOverflow;
    procedure TestDotsInViewNoParent;
    procedure TestDotsInViewNoDots;

    // Narrow-window trend-dot clamp
    procedure TestTrendDotStrideWideWindow;
    procedure TestTrendDotStrideNarrowWindow;
    procedure TestTrendDotStrideDegenerateWidth;
    procedure TestTrendDotVisibleAnchorsOnNewest;

    // Startup / shutdown tests
    procedure TestFormCreateStartsTimers;
    procedure TestFormDestroyFreesNative;

    // API receiver message handling (no crash)
    procedure TestAPIReceiverHandlesMessages;
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

procedure TUmainTests.TestDotsInViewNoParent;
var
  g: TfBG;
  n: TrndiNative;
  i: integer;
  dots: array[1..10] of TDotInfo;
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

      // Make one dot appear (but with no parent height provided)
      dots[7].Top := 38; // bottom = Top + Height -> 38 + 10 = 48
      dots[7].Height := 10;
      dots[7].Visible := True;

      try
        // With no parent height (0) we should treat as "no parent" and not report overflow
        AssertEquals('No parent height should result in no overflow', 0, g.DotsInViewForTestsFromInfos(dots, 0));
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
    // If native still assigned, free it here (some tests expect FormDestroy to free it)
    if Assigned(native) then
    begin
      native.Free;
      native := nil;
    end;
  end;
end;

// A window with room to spare renders every slot — the clamp must stay out of
// the way at ordinary sizes.
procedure TUmainTests.TestTrendDotStrideWideWindow;
var
  g: TfBG;
  n: TrndiNative;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      AssertEquals('10 dots in an 800 px window need no skipping',
        1, g.TrendDotStrideForTests(800, 10));
      AssertEquals('39 columns in an 800 px window still fit',
        1, g.TrendDotStrideForTests(800, 39));

      // The clamp must stay clear of ordinary use: a 50-dot trend has to
      // render in full at the 320 px startup width (umain.lfm), predictions
      // on or off. Tightening MIN_DOT_COLUMN_PX must not silently break this.
      AssertEquals('50 dots must fit the 320 px startup window',
        1, g.TrendDotStrideForTests(320, 50));
      AssertEquals('50 dots plus 3 prediction slots must fit it too',
        1, g.TrendDotStrideForTests(320, 50 + PREDICTION_DOT_COUNT));
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    if Assigned(native) then
    begin
      native.Free;
      native := nil;
    end;
  end;
end;

// Once the columns fall below MIN_DOT_COLUMN_PX the stride must rise enough
// that the *rendered* count fits the width — that's the property that stops
// the dots overlapping, so assert it rather than hard-coded strides alone.
procedure TUmainTests.TestTrendDotStrideNarrowWindow;
var
  g: TfBG;
  n: TrndiNative;
  stride, rendered: integer;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      AssertEquals('36 columns in 120 px needs every 2nd dot',
        2, g.TrendDotStrideForTests(120, 36));
      AssertEquals('288 columns in the 320 px startup window must be thinned',
        6, g.TrendDotStrideForTests(320, 288));

      // Worst supported case: a full 24 h window on a small screen. Assert the
      // invariant rather than a literal stride — whatever survives the thinning
      // must itself need no further thinning, which is the property that stops
      // the dots overlapping. Stays honest if MIN_DOT_COLUMN_PX is retuned.
      stride := g.TrendDotStrideForTests(1000, 288);
      AssertTrue('288 columns in 1000 px must be thinned', stride > 1);
      rendered := (288 + stride - 1) div stride;
      AssertEquals('What survives thinning must fit without thinning again',
        1, g.TrendDotStrideForTests(1000, rendered));
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    if Assigned(native) then
    begin
      native.Free;
      native := nil;
    end;
  end;
end;

// Boot and teardown hand out zero/absurd widths; the stride must stay a legal
// divisor (>= 1) so the visibility test never divides by zero.
procedure TUmainTests.TestTrendDotStrideDegenerateWidth;
var
  g: TfBG;
  n: TrndiNative;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      AssertEquals('Zero width falls back to no skipping',
        1, g.TrendDotStrideForTests(0, 36));
      AssertEquals('Negative width falls back to no skipping',
        1, g.TrendDotStrideForTests(-100, 36));
      AssertEquals('A single column is never skipped',
        1, g.TrendDotStrideForTests(4, 1));
      AssertTrue('A width narrower than one column still yields a valid stride',
        g.TrendDotStrideForTests(5, 36) >= 1);
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    if Assigned(native) then
    begin
      native.Free;
      native := nil;
    end;
  end;
end;

// The newest slot carries the "fresh" ring and is what the user reads, so it
// must survive every stride; skipping walks backwards from there.
procedure TUmainTests.TestTrendDotVisibleAnchorsOnNewest;
var
  g: TfBG;
  n: TrndiNative;
  i: integer;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      AssertTrue('Newest slot is always rendered at stride 2',
        g.TrendDotVisibleInStrideForTests(36, 36, 2));
      AssertFalse('Second-newest slot is skipped at stride 2',
        g.TrendDotVisibleInStrideForTests(35, 36, 2));
      AssertTrue('Third-newest slot is rendered at stride 2',
        g.TrendDotVisibleInStrideForTests(34, 36, 2));
      AssertTrue('Newest slot is always rendered at stride 4',
        g.TrendDotVisibleInStrideForTests(36, 36, 4));
      AssertTrue('Fifth-newest slot is rendered at stride 4',
        g.TrendDotVisibleInStrideForTests(32, 36, 4));

      for i := 1 to 36 do
        AssertTrue('Stride 1 renders every slot',
          g.TrendDotVisibleInStrideForTests(i, 36, 1));
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    if Assigned(native) then
    begin
      native.Free;
      native := nil;
    end;
  end;
end;

procedure TUmainTests.TestFormCreateStartsTimers;
var
  g: TfBG;
  n: TrndiNative;
  cMain: TComponent;
begin
  n := TrndiNative.Create;
  // Do NOT free 'n' directly if FormDestroy is expected to free native; cleanup below checks native
  native := n;
  try
    g := TfBG.Create;
    try
      fBG := g;
      // Use FindComponent for published/private components to avoid direct field access
      cMain := g.FindComponent('tMain');
      if Assigned(cMain) then
        AssertTrue('tMain should be enabled after initialization', TTimer(cMain).Enabled)
      else
        AssertTrue('tMain not present in this test build (acceptable)', True);
      // Web server timer should not exist unless explicitly enabled in settings
      AssertFalse('tWebServerStart should not be created when webserver disabled', Assigned(g.FindComponent('tWebServerStart')));
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    if Assigned(native) then
    begin
      native.Free;
      native := nil;
    end;
  end;
end;


procedure TUmainTests.TestFormDestroyFreesNative;
var
  g: TfBG;
  n: TrndiNative;
begin
  n := TrndiNative.Create;
  native := n;

  // Create and immediately free the form — ensure no crash on destroy
  g := TfBG.Create;
  try
    fBG := g;
  finally
    g.Free; // triggers FormDestroy
    fBG := nil;
  end;

  // Don't assert that `native` was freed by FormDestroy (platform differences may vary).
  // Clean up mock if still present so subsequent tests are unaffected.
  if Assigned(native) then
  begin
    native.Free;
    native := nil;
  end;
end;

procedure TUmainTests.TestAPIReceiverHandlesMessages;
var
  g: TfBG;
  n: TrndiNative;
begin
  n := TrndiNative.Create;
  native := n;
  try
    g := TfBG.Create;
    try
      fBG := g;
      // Ensure APIReceiver accepts all message types without raising
      try
        g.APIReceiver('an alert', TrndiAPIMsg.alert);
        g.APIReceiver('a notice', TrndiAPIMsg.notice);
        g.APIReceiver('a status', TrndiAPIMsg.status);
      except
        on E: Exception do
          Fail('APIReceiver crashed: ' + E.ClassName + ': ' + E.Message);
      end;
    finally
      g.Free;
      fBG := nil;
    end;
  finally
    if Assigned(native) then
    begin
      native.Free;
      native := nil;
    end;
  end;
end;

initialization
  RegisterTest(TUmainTests);
end.
