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
unit api_librelinkup_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testregistry,
trndi.api, trndi.api.librelinkup, trndi.types, dateutils;

type

TAPILibreLinkUpTester = class(TTestCase)
private
  FApi: LibreLinkUp;
  FPayload: string;
  {** Load the recorded graph payload; fails the test if it is missing. }
  function LoadFixture: string;
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  // Offline: payload parsing
  procedure TestFixtureReadingCount;
  procedure TestFixtureNewestReadingAndArrow;
  procedure TestFixtureOrderIsNewestFirst;
  procedure TestFixtureOutOfRangeMapping;
  procedure TestFixtureThresholdsApplied;
  procedure TestFixtureMaxCountCaps;
  procedure TestEmptyPayloadFails;
  // Offline: helpers
  procedure TestTrendMapping;
  procedure TestTimeParsing;
  procedure TestTimeParsingRejectsGarbage;
  procedure TestRegionLookup;
  // Offline: guard rails
  procedure TestGetReadingsWithoutConnect;
  procedure TestConnectWithoutCredentials;
  procedure TestParamLabels;
end;

implementation

const
  FIXTURE_PATH = 'tests/fixtures/librelinkup_graph.json';

procedure TAPILibreLinkUpTester.SetUp;
begin
  FApi := LibreLinkUp.Create('follower@example.com', 'not-a-real-password');
  FPayload := '';
end;

procedure TAPILibreLinkUpTester.TearDown;
begin
  FreeAndNil(FApi);
end;

function TAPILibreLinkUpTester.LoadFixture: string;
var
  fixture: TStringList;
begin
  if FPayload <> '' then
    Exit(FPayload);

  AssertTrue('Fixture missing: ' + FIXTURE_PATH, FileExists(FIXTURE_PATH));
  fixture := TStringList.Create;
  try
    fixture.LoadFromFile(FIXTURE_PATH);
    FPayload := fixture.Text;
  finally
    fixture.Free;
  end;
  Result := FPayload;
end;

{ The fixture holds seven graph points plus the current reading. One point is a
  zero-value gap marker and one repeats the current reading's timestamp, so six
  readings should survive. }
procedure TAPILibreLinkUpTester.TestFixtureReadingCount;
var
  readings: BGResults;
begin
  AssertTrue('Payload must parse', FApi.ParseGraphPayload(LoadFixture, 0, 0, readings));
  AssertEquals('Gap marker skipped and duplicate deduped', 6, Length(readings));
end;

{ The newest reading is the current measurement, which is the only entry
  carrying Abbott's own arrow (TrendArrow 2 = falling slightly). }
procedure TAPILibreLinkUpTester.TestFixtureNewestReadingAndArrow;
var
  readings: BGResults;
begin
  AssertTrue('Payload must parse', FApi.ParseGraphPayload(LoadFixture, 0, 0, readings));
  AssertEquals('Newest value', 112.0, readings[0].convert(mgdl), 0.5);
  AssertEquals('Server arrow wins on the newest reading',
    Ord(TdFortyFiveDown), Ord(readings[0].trend));
  // 112 now against 120 five minutes ago
  AssertEquals('Raw delta against the previous reading',
    -8.0, readings[0].convert(mgdl, BGDelta), 0.5);
end;

procedure TAPILibreLinkUpTester.TestFixtureOrderIsNewestFirst;
var
  readings: BGResults;
  i: integer;
begin
  AssertTrue('Payload must parse', FApi.ParseGraphPayload(LoadFixture, 0, 0, readings));
  for i := 0 to High(readings) - 1 do
    AssertTrue(Format('Reading %d is not newer than %d', [i, i + 1]),
      readings[i].date > readings[i + 1].date);
end;

{ Values outside the sensor's range arrive clamped with isHigh/isLow set; they
  must land past Trndi's limits so the UI renders HIGH/LOW rather than a number
  the sensor never measured. }
procedure TAPILibreLinkUpTester.TestFixtureOutOfRangeMapping;
var
  readings: BGResults;
  i: integer;
  sawHigh, sawLow: boolean;
  v: double;
begin
  AssertTrue('Payload must parse', FApi.ParseGraphPayload(LoadFixture, 0, 0, readings));
  sawHigh := false;
  sawLow := false;
  for i := 0 to High(readings) do
  begin
    v := readings[i].convert(mgdl);
    if v > FApi.limitHI then
      sawHigh := true;
    if v < FApi.limitLO then
      sawLow := true;
  end;
  AssertTrue('isHigh entry must exceed limitHI', sawHigh);
  AssertTrue('isLow entry must fall below limitLO', sawLow);
end;

procedure TAPILibreLinkUpTester.TestFixtureThresholdsApplied;
var
  readings: BGResults;
begin
  AssertTrue('Payload must parse', FApi.ParseGraphPayload(LoadFixture, 0, 0, readings));
  AssertEquals('High alarm from alarmRules.h.th', 240, FApi.cgmHi);
  AssertEquals('Low alarm from alarmRules.l.th', 60, FApi.cgmLo);
  AssertEquals('Range top from targetHigh', 180, FApi.cgmRangeHi);
  AssertEquals('Range bottom from targetLow', 70, FApi.cgmRangeLo);
end;

procedure TAPILibreLinkUpTester.TestFixtureMaxCountCaps;
var
  readings: BGResults;
begin
  AssertTrue('Payload must parse', FApi.ParseGraphPayload(LoadFixture, 0, 3, readings));
  AssertEquals('Result capped to the requested count', 3, Length(readings));
end;

procedure TAPILibreLinkUpTester.TestEmptyPayloadFails;
var
  readings: BGResults;
begin
  AssertFalse('Empty body must not parse', FApi.ParseGraphPayload('', 0, 0, readings));
  AssertFalse('Non-JSON body must not parse',
    FApi.ParseGraphPayload('definitely not json', 0, 0, readings));
  AssertTrue('Error message set', FApi.errormsg <> '');
end;

{ Abbott reports five arrows and has no double-arrow state, so the mapping must
  never produce one. }
procedure TAPILibreLinkUpTester.TestTrendMapping;
begin
  AssertEquals('1 = falling', Ord(TdSingleDown), Ord(LibreTrendToBG(1)));
  AssertEquals('2 = falling slightly', Ord(TdFortyFiveDown), Ord(LibreTrendToBG(2)));
  AssertEquals('3 = steady', Ord(TdFlat), Ord(LibreTrendToBG(3)));
  AssertEquals('4 = rising slightly', Ord(TdFortyFiveUp), Ord(LibreTrendToBG(4)));
  AssertEquals('5 = rising', Ord(TdSingleUp), Ord(LibreTrendToBG(5)));
  AssertEquals('0 is not an arrow', Ord(TdPlaceholder), Ord(LibreTrendToBG(0)));
  AssertEquals('6 is not an arrow', Ord(TdPlaceholder), Ord(LibreTrendToBG(6)));
end;

procedure TAPILibreLinkUpTester.TestTimeParsing;
var
  parsed: TDateTime;
begin
  // Unpadded month and a two-digit day
  AssertTrue('M/D/YYYY h:mm:ss PM parses',
    ParseLibreTime('3/10/2026 6:05:09 PM', false, parsed));
  AssertEquals('Afternoon hour shifted past noon',
    EncodeDateTime(2026, 3, 10, 18, 5, 9, 0), parsed, 1 / 86400);

  // Midnight and noon are the two the 12-hour clock gets wrong most easily
  AssertTrue('12 AM parses', ParseLibreTime('7/4/2026 12:00:00 AM', false, parsed));
  AssertEquals('12 AM is midnight',
    EncodeDateTime(2026, 7, 4, 0, 0, 0, 0), parsed, 1 / 86400);

  AssertTrue('12 PM parses', ParseLibreTime('7/4/2026 12:30:00 PM', false, parsed));
  AssertEquals('12 PM is noon',
    EncodeDateTime(2026, 7, 4, 12, 30, 0, 0), parsed, 1 / 86400);

  // A 24-hour value without an AM/PM marker must survive too
  AssertTrue('24-hour value parses', ParseLibreTime('7/4/2026 21:15:00', false, parsed));
  AssertEquals('24-hour value kept as-is',
    EncodeDateTime(2026, 7, 4, 21, 15, 0, 0), parsed, 1 / 86400);

  // FactoryTimestamp is UTC; asking for the conversion must apply this
  // machine's offset rather than returning the literal value.
  AssertTrue('UTC value parses', ParseLibreTime('3/10/2026 6:00:00 PM', true, parsed));
  AssertEquals('UTC converted to local time',
    UniversalTimeToLocal(EncodeDateTime(2026, 3, 10, 18, 0, 0, 0)), parsed, 1 / 86400);
end;

procedure TAPILibreLinkUpTester.TestTimeParsingRejectsGarbage;
var
  parsed: TDateTime;
begin
  AssertFalse('Empty string rejected', ParseLibreTime('', false, parsed));
  AssertFalse('Missing time rejected', ParseLibreTime('3/10/2026', false, parsed));
  AssertFalse('Nonsense rejected', ParseLibreTime('not a timestamp', false, parsed));
  AssertFalse('Impossible date rejected',
    ParseLibreTime('13/45/2026 6:00:00 PM', false, parsed));
end;

procedure TAPILibreLinkUpTester.TestRegionLookup;
begin
  AssertEquals('German region', 'https://api-de.libreview.io', LibreRegionHost('de'));
  AssertEquals('Region codes are case-insensitive',
    'https://api-eu2.libreview.io', LibreRegionHost('EU2'));
  AssertEquals('Russia is on its own domain',
    'https://api.libreview.ru', LibreRegionHost('ru'));
  AssertEquals('Unknown region yields no host', '', LibreRegionHost('atlantis'));
  AssertEquals('Empty region yields no host', '', LibreRegionHost(''));
end;

{ Without a patient there is nothing to request; getReadings must say so
  rather than attempt a call. }
procedure TAPILibreLinkUpTester.TestGetReadingsWithoutConnect;
var
  readings: BGResults;
  res: string;
begin
  readings := FApi.getReadings(60, 10, '', res, false);
  AssertEquals('No readings without a session', 0, Length(readings));
  AssertTrue('Error explains the missing session', FApi.errormsg <> '');
end;

{ Missing credentials are rejected before any network access. }
procedure TAPILibreLinkUpTester.TestConnectWithoutCredentials;
var
  api: TrndiAPI;
begin
  api := LibreLinkUp.Create('', '');
  try
    AssertFalse('Connect must fail without an email', api.connect);
    AssertTrue('Error message set', api.errormsg <> '');
  finally
    api.Free;
  end;

  api := LibreLinkUp.Create('follower@example.com', '');
  try
    AssertFalse('Connect must fail without a password', api.connect);
    AssertTrue('Error message set', api.errormsg <> '');
  finally
    api.Free;
  end;
end;

procedure TAPILibreLinkUpTester.TestParamLabels;
begin
  AssertTrue('User label set', LibreLinkUp.ParamLabel(APLUser) <> '');
  AssertTrue('Password label set', LibreLinkUp.ParamLabel(APLPass) <> '');
  AssertTrue('Description set', LibreLinkUp.ParamLabel(APLDesc) <> '');
  AssertTrue('HTML description set', LibreLinkUp.ParamLabel(APLDescHTML) <> '');
  AssertFalse('Assisted browser login is not used',
    LibreLinkUp.supportsWebLogin);
end;

initialization
  RegisterTest(TAPILibreLinkUpTester);

end.
