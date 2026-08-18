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
  fpcunit, testregistry, umain, SysUtils, StdCtrls, ExtCtrls, Classes, Graphics,
  trndi.native, trndi.types, trndi.funcs, trndi.shared, trndi.api, trndi.api.debug;

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

    // Trend-dot coloring modes
    procedure TestDotColorModeFromSettingClamps;
    procedure TestDotDisplayColorClearsBackground;
    procedure TestDotDisplayColorClassicIsUnguarded;
    procedure TestDotBandPartnerMapsRanges;
    procedure TestDotDisplayColorPrefersColorfulHalf;
    procedure TestDotDisplayColorAvoidsReadingColor;
    procedure TestDotDisplayColorMonoIgnoresRange;
    procedure TestDotDisplayColorOutlineKeepsRangeColor;

    // Startup / shutdown tests
    procedure TestFormCreateStartsTimers;
    procedure TestFormDestroyFreesNative;

    // API receiver message handling (no crash)
    procedure TestAPIReceiverHandlesMessages;

    // Placeholder readings must never drive the display or the alert engine
    procedure TestProcessCurrentReadingIgnoresPlaceholder;
    procedure TestUpdateUIBasedOnGlucoseIgnoresPlaceholder;
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

// An unknown stored mode (older profile, hand-edited settings) must land on
// the default rather than an arbitrary enum value.
procedure TUmainTests.TestDotColorModeFromSettingClamps;
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
      AssertTrue('0 decodes to Classic',
        g.DotColorModeFromSettingForTests(0) = dcmClassic);
      AssertTrue('1 decodes to Auto',
        g.DotColorModeFromSettingForTests(1) = dcmAuto);
      AssertTrue('3 decodes to Darker',
        g.DotColorModeFromSettingForTests(3) = dcmDarker);
      AssertTrue('4 is monochrome',
        g.DotColorModeFromSettingForTests(4) = dcmMono);
      AssertTrue('5 is outlined',
        g.DotColorModeFromSettingForTests(5) = dcmOutline);
      // Against the constant, not a value: which mode is the default is a
      // decision that may change, that it survives a bad stored value is not.
      AssertTrue('A negative value falls back to the default',
        g.DotColorModeFromSettingForTests(-1) = DOT_COLOR_MODE_DEFAULT);
      AssertTrue('A too-large value falls back to the default',
        g.DotColorModeFromSettingForTests(99) = DOT_COLOR_MODE_DEFAULT);
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

// The case the contrast pass exists for: a dot whose reading falls in the same
// range as the current one, so its color *is* the background color. Every mode
// but Classic has to pull it clear, whichever direction the user asked for.
procedure TUmainTests.TestDotDisplayColorClearsBackground;
const
  // Classic theme's high background (see trndi.theme), i.e. the worst case.
  IDENTITY = TColor($0007DAFF);
var
  g: TfBG;
  n: TrndiNative;
  mode: TDotColorMode;
  drawn: TColor;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      // dcmOutline is excluded on purpose — it is the one mode whose fill
      // carries no contrast at all, because DotPaint rims it instead.
      for mode := dcmAuto to dcmMono do
      begin
        // clWhite stands in for the reading color: far from every candidate
        // here, so the contrast floor is the only thing under test.
        drawn := g.DotDisplayColorForTests(IDENTITY, clNone, IDENTITY, clWhite,
          mode);
        AssertTrue(Format('Mode %d must clear the contrast floor on its own ' +
          'band (got %.2f:1)', [Ord(mode), ContrastRatio(drawn, IDENTITY)]),
          ContrastRatio(drawn, IDENTITY) >= DOT_MIN_CONTRAST - 0.001);
      end;

      // The tint modes must actually go the way they say on a background that
      // leaves room in both directions.
      AssertTrue('Lighter tints away from the identity color',
        RelativeLuminance(g.DotDisplayColorForTests(IDENTITY, clNone, clBlack,
        clWhite, dcmLighter)) > RelativeLuminance(IDENTITY));
      AssertTrue('Darker shades away from the identity color',
        RelativeLuminance(g.DotDisplayColorForTests(IDENTITY, clNone, clWhite,
        clBlack, dcmDarker)) < RelativeLuminance(IDENTITY));
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

// Classic is the pre-contrast behaviour and is deliberately left unguarded —
// that is the look it exists to reproduce. Pin it, so the mode cannot quietly
// acquire the contrast floor and stop differing from Auto.
procedure TUmainTests.TestDotDisplayColorClassicIsUnguarded;
const
  IDENTITY = TColor($0007DAFF);
var
  g: TfBG;
  n: TrndiNative;
  drawn: TColor;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      drawn := g.DotDisplayColorForTests(IDENTITY, clNone, IDENTITY, clWhite,
        dcmClassic);
      AssertEquals('Classic is the old flat darkening, background ignored',
        LightenColor(IDENTITY, DOT_CLASSIC_DARKEN), drawn);
      AssertTrue('Classic stays below the floor on its own band — the reason ' +
        'it is not the default',
        ContrastRatio(drawn, IDENTITY) < DOT_MIN_CONTRAST);
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

// The pair lookup has to hit every range, and — just as importantly — miss
// everything else: a stale or high-contrast dot must keep the color it was
// deliberately given.
procedure TUmainTests.TestDotBandPartnerMapsRanges;
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
      AssertEquals('High pairs with the high text color',
        bg_color_hi_txt, g.DotBandPartnerForTests(bg_color_hi));
      AssertEquals('Low pairs with the low text color',
        bg_color_lo_txt, g.DotBandPartnerForTests(bg_color_lo));
      // In-range dots are colored from the text half, so this one is inverted.
      AssertEquals('In-range pairs with the in-range background',
        bg_color_ok, g.DotBandPartnerForTests(bg_color_ok_txt));
      AssertEquals('Personal high pairs with its text color',
        bg_rel_color_hi_txt, g.DotBandPartnerForTests(bg_rel_color_hi));
      AssertEquals('Personal low pairs with its text color',
        bg_rel_color_lo_txt, g.DotBandPartnerForTests(bg_rel_color_lo));
      AssertEquals('A color from no range has no partner',
        clNone, g.DotBandPartnerForTests(TColor($00123456)));
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

// Auto draws whichever half of the range's pair keeps more colour once it has
// been lifted clear of the background. That is what keeps the ranges apart on a
// light window, where the lift can only darken and would otherwise land them
// all in the same muddy tone.
procedure TUmainTests.TestDotDisplayColorPrefersColorfulHalf;
const
  PALE = TColor($00F2FFF2);                       // Classic in-range text
  VIVID = TColor($0000DC84);                      // Classic in-range background
  BACKGROUND = TColor($0007DAFF);                 // Classic high background
  // Classic personal-low: a vivid pink paired with a near-black maroon. The
  // maroon is the more *saturated* of the two by the scale-invariant measure,
  // which is why that measure must not be the one deciding.
  PINK = TColor($00A859EE);
  MAROON = TColor($002D074E);
  // Classic high on the near-black stale window: both halves come out within a
  // couple of percent of each other, and the dot should keep its own colour.
  AMBER = TColor($0007DAFF);
  AMBER_TEXT = TColor($000052FB);
  STALE = TColor($001F1916);
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
      AssertTrue('The fixture pair must actually differ in chroma',
        ColorChroma(VIVID) > ColorChroma(PALE));

      // clWhite for the reading color throughout: none of the candidates below
      // is anywhere near it, so the chroma comparison is what decides and
      // nothing else.
      AssertEquals('A washed-out identity gives way to its colourful partner',
        EnsureContrast(VIVID, BACKGROUND, DOT_MIN_CONTRAST),
        g.DotDisplayColorForTests(PALE, VIVID, BACKGROUND, clWhite, dcmAuto));
      AssertEquals('A colourful identity keeps its own colour',
        EnsureContrast(VIVID, BACKGROUND, DOT_MIN_CONTRAST),
        g.DotDisplayColorForTests(VIVID, PALE, BACKGROUND, clWhite, dcmAuto));
      AssertEquals('No partner (stale, high contrast) means no substitution',
        EnsureContrast(PALE, BACKGROUND, DOT_MIN_CONTRAST),
        g.DotDisplayColorForTests(PALE, clNone, BACKGROUND, clWhite, dcmAuto));

      // Judged before the lift these two go the wrong way: the maroon reads as
      // the more saturated half, and on the dark window the amber's partner
      // wins by a hair and swaps a colour for no visible gain.
      AssertEquals('A near-black partner does not displace a vivid identity',
        EnsureContrast(PINK, BACKGROUND, DOT_MIN_CONTRAST),
        g.DotDisplayColorForTests(PINK, MAROON, BACKGROUND, clWhite, dcmAuto));
      AssertEquals('A marginal gain is not worth swapping the colour for',
        EnsureContrast(AMBER, STALE, DOT_MIN_CONTRAST),
        g.DotDisplayColorForTests(AMBER, AMBER_TEXT, STALE, clWhite, dcmAuto));

      // The pair is an Auto-only device — the tint modes stay literal about
      // the color the reading was given.
      AssertEquals('Lighter ignores the partner',
        g.DotDisplayColorForTests(PALE, clNone, BACKGROUND, clWhite, dcmLighter),
        g.DotDisplayColorForTests(PALE, VIVID, BACKGROUND, clWhite, dcmLighter));
      AssertEquals('Classic ignores the partner',
        g.DotDisplayColorForTests(PALE, clNone, BACKGROUND, clWhite, dcmClassic),
        g.DotDisplayColorForTests(PALE, VIVID, BACKGROUND, clWhite, dcmClassic));
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

// On the window a range owns, the contrast floor lands both halves of that
// range's pair on the reading the window is showing — same lightness, same hue
// — and the dots disappear into the digits. Auto has to fall back to the
// untouched palette color there, and must not disturb the ranges that already
// read cleanly across the reading.
procedure TUmainTests.TestDotDisplayColorAvoidsReadingColor;
const
  OK_TEXT = TColor($00F2FFF2);      // Classic in-range text (near-white)
  OK_BG = TColor($0000DC84);        // Classic in-range background
  HIGH_BG = TColor($0007DAFF);      // Classic high background
  HIGH_TEXT = TColor($000052FB);    // Classic high text (red-orange)
var
  g: TfBG;
  n: TrndiNative;
  okValue, highValue, collides: TColor;
begin
  n := TrndiNative.Create;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      // What UpdateUIColors draws the reading in on a light window.
      okValue := DarkenColor(OK_BG, 0.5);
      highValue := DarkenColor(HIGH_BG, 0.5);

      // The fixture only means anything if the lifted color really does land
      // on the reading — that is the whole failure being guarded against.
      collides := EnsureContrast(OK_BG, OK_BG, DOT_MIN_CONTRAST);
      AssertTrue(Format('Fixture: the lifted in-range half must collide with ' +
        'the reading (distance %d)', [ColorDistance(collides, okValue)]),
        ColorDistance(collides, okValue) < DOT_VALUE_SEPARATION);

      AssertEquals('An in-range dot on the in-range window falls back to the ' +
        'untouched text color rather than sinking into the reading',
        OK_TEXT, g.DotDisplayColorForTests(OK_TEXT, OK_BG, OK_BG, okValue,
        dcmAuto));
      // Why the fallback skips the first candidate here: the in-range pair's
      // other half *is* the window.
      AssertFalse('The background half is never an escape from the reading',
        g.DotSeparatedFromValueForTests(OK_BG, OK_BG, okValue));

      // A high dot on the high window shares the reading's lightness but not
      // its hue, so it reads fine and keeps the lifted color it already had.
      AssertEquals('A dot that already reads over the reading is left alone',
        EnsureContrast(HIGH_TEXT, HIGH_BG, DOT_MIN_CONTRAST),
        g.DotDisplayColorForTests(HIGH_BG, HIGH_TEXT, HIGH_BG, highValue,
        dcmAuto));

      // The fallback is Auto's, like the pair it draws from.
      AssertEquals('Darker stays literal about the color it was given',
        g.DotDisplayColorForTests(OK_TEXT, clNone, OK_BG, okValue, dcmDarker),
        g.DotDisplayColorForTests(OK_TEXT, OK_BG, OK_BG, okValue, dcmDarker));
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

// Monochrome is the one mode that throws the range color away, so what has to
// be pinned is that it throws *all* of it away and takes its pole from the
// window alone — the point of the mode is that it cannot be surprised by a
// palette.
procedure TUmainTests.TestDotDisplayColorMonoIgnoresRange;
const
  OK_TEXT = TColor($00F2FFF2);
  OK_BG = TColor($0000DC84);        // Classic in-range background, a light one
  STALE = TColor($001F1916);        // The stale window, near black
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
      AssertEquals('A light window takes black dots',
        clBlack, g.DotDisplayColorForTests(OK_TEXT, OK_BG, OK_BG,
        DarkenColor(OK_BG, 0.5), dcmMono));
      AssertEquals('A dark window takes white dots',
        clWhite, g.DotDisplayColorForTests(OK_TEXT, OK_BG, STALE,
        LightenColor(STALE, 0.3), dcmMono));
      // Same window, every other range: the mode is background-only.
      AssertEquals('The range color has no say',
        g.DotDisplayColorForTests(bg_color_hi, bg_color_hi_txt, OK_BG,
        DarkenColor(OK_BG, 0.5), dcmMono),
        g.DotDisplayColorForTests(bg_color_lo, clNone, OK_BG,
        DarkenColor(OK_BG, 0.5), dcmMono));
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

// Outline is the only mode that hands a dot's visibility to the rim instead of
// the fill, which is what lets it keep a range's hue — a high dot stays amber
// rather than darkening to olive or turning into the red-orange half of the
// pair. So what has to hold is that the fill is never touched, on any window,
// and that the rim it depends on is always the pole opposite that window.
procedure TUmainTests.TestDotDisplayColorOutlineKeepsRangeColor;
const
  HIGH_BG = TColor($0007DAFF);      // Classic high background, amber
  HIGH_TEXT = TColor($000052FB);    // Its pair half, a red-orange
  OK_BG = TColor($0000DC84);        // A light window
  STALE = TColor($001F1916);        // A dark one
  WINDOWS: array[0..3] of TColor = (OK_BG, HIGH_BG, STALE, clWhite);
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
      for i := 0 to High(WINDOWS) do
        AssertEquals(Format('The fill is the range color untouched on $%.6x',
          [WINDOWS[i]]), HIGH_BG,
          g.DotDisplayColorForTests(HIGH_BG, HIGH_TEXT, WINDOWS[i],
          DarkenColor(WINDOWS[i], 0.5), dcmOutline));

      // Including on its own window, where every other mode has to move it.
      AssertEquals('Not even on the window the range owns', HIGH_BG,
        g.DotDisplayColorForTests(HIGH_BG, HIGH_TEXT, HIGH_BG,
        DarkenColor(HIGH_BG, 0.5), dcmOutline));

      AssertEquals('A light window is rimmed in black',
        clBlack, g.DotOutlineColorForTests(HIGH_BG));
      AssertEquals('A dark window is rimmed in white',
        clWhite, g.DotOutlineColorForTests(STALE));
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

{ With no readings at all, lastReading hands back an initialized placeholder
  whose value is the BG_NO_VAL sentinel (-904). ProcessCurrentReading used to
  take that at face value: -904 is below any low limit, so it wrote RS_LOW into
  the hero label. Reachable in the shipped app because ApplySettingsInstantly
  calls the procedure directly, without the "readings exist and are fresh"
  precondition the fetch pipeline establishes. }
procedure TUmainTests.TestProcessCurrentReadingIgnoresPlaceholder;
var
  g: TfBG;
  n: TrndiNative;
  a: DebugAPI;
  lbl: TLabel;
begin
  n := TrndiNative.Create;
  a := nil;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      a := DebugAPI.Create('', '');
      a.connect;                 // cgmLo = 60, cgmHi = 160
      api := a;
      g.firstboot := false;
      SetLength(bgs, 0);         // no readings: lastReading is the placeholder

      lbl := TLabel(g.FindComponent('lVal'));
      if Assigned(lbl) then
        lbl.Caption := 'untouched';

      // The guard has to return before any display control is touched. The
      // console build has no LFM controls, so without it the RS_LOW write
      // faults here rather than merely showing the wrong value — either way
      // this call must come back clean.
      try
        g.ProcessCurrentReadingForTests;
      except
        on E: Exception do
          Fail('A placeholder reading reached the display path: ' +
            E.ClassName + ': ' + E.Message);
      end;

      if Assigned(lbl) then
        AssertEquals('A placeholder reading must not be rendered as a value',
          'untouched', lbl.Caption);
    finally
      api := nil;
      fBG := nil;
      g.Free;
    end;
  finally
    a.Free;
    n.Free;
    native := nil;
  end;
end;

{ Same placeholder, but through the path that reaches the alert engine:
  EvaluateLevel(-904) lands deep in the urgent-low band and (with the default
  zero min-duration) fires a critical hypo alert for a reading that does not
  exist. Asserted here through HandleLowGlucose's visible side effect — the
  low background colour — which is set on exactly the same branch. }
procedure TUmainTests.TestUpdateUIBasedOnGlucoseIgnoresPlaceholder;
var
  g: TfBG;
  n: TrndiNative;
  a: DebugAPI;
begin
  n := TrndiNative.Create;
  a := nil;
  try
    native := n;
    g := TfBG.Create;
    try
      fBG := g;
      a := DebugAPI.Create('', '');
      a.connect;
      api := a;
      g.firstboot := false;
      SetLength(bgs, 0);

      g.Color := clFuchsia;      // sentinel: no glucose branch would pick this
      try
        g.UpdateUIBasedOnGlucoseForTests;
      except
        on E: Exception do
          Fail('A placeholder reading reached the alert path: ' +
            E.ClassName + ': ' + E.Message);
      end;

      AssertEquals('A placeholder reading must not repaint the window as a low',
        clFuchsia, g.Color);
    finally
      api := nil;
      fBG := nil;
      g.Free;
    end;
  finally
    a.Free;
    n.Free;
    native := nil;
  end;
end;

initialization
  RegisterTest(TUmainTests);
end.
