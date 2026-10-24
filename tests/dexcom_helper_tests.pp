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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-10-16: Created, covering the helpers both Dexcom Share drivers share:
 *   id extraction from the login replies, the password-rejection check, the
 *   duplicate-reading dedupe, delta computation and the window clamp.
 *)
unit dexcom_helper_tests;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, fpcunit, testregistry,
  trndi.types, trndi.api.dexcom_helpers;

type
  TDexcomHelperTests = class(TTestCase)
  published
    procedure ExtractsQuotedId;
    procedure ExtractsObjectId;
    procedure ExtractsBareToken;
    procedure RejectsErrorsAndNoise;
    procedure DetectsPasswordRejection;
    procedure DropsDuplicateReadings;
    procedure KeepsUndatedReadings;
    procedure ComputesDeltasAfterDedupe;
    procedure ClampsWindow;
    procedure WrongRegionMessageNamesRegion;
  end;

implementation

{------------------------------------------------------------------------------
  Build a newest-first list of readings dated AgoSeconds[i] seconds ago, with
  values Vals[i]. A value of 0 leaves the reading empty.
 ------------------------------------------------------------------------------}
function MakeReadings(const AgoSeconds: array of integer;
  const Vals: array of integer): BGResults;
var
  i: integer;
  base: TDateTime;
begin
  Result := nil;
  base := Now;
  SetLength(Result, Length(AgoSeconds));
  for i := 0 to High(AgoSeconds) do
  begin
    Result[i].Init(mgdl, 'test');
    Result[i].date := IncSecond(base, -AgoSeconds[i]);
    if Vals[i] <> 0 then
      Result[i].update(Vals[i], BGPrimary);
  end;
end;

procedure TDexcomHelperTests.ExtractsQuotedId;
begin
  AssertEquals('JSON string', 'abc-123', DexcomExtractId('"abc-123"'));
  AssertEquals('Whitespace around the string', 'abc-123', DexcomExtractId('  "abc-123" '));
  AssertEquals('Null GUID passes through for the caller to judge',
    DEXCOM_NULL_UUID, DexcomExtractId('"' + DEXCOM_NULL_UUID + '"'));
end;

procedure TDexcomHelperTests.ExtractsObjectId;
begin
  // The shape newer (G7-era) accounts get from AuthenticatePublisherAccount
  AssertEquals('accountId member', 'x-1', DexcomExtractId('{"accountId":"x-1"}'));
  AssertEquals('sessionId member', 's-1', DexcomExtractId('{"sessionId":"s-1"}'));
  AssertEquals('Capitalised member', 'x-2', DexcomExtractId('{"AccountId":"x-2"}'));
end;

procedure TDexcomHelperTests.ExtractsBareToken;
begin
  AssertEquals('Unquoted token', 'BARE-TOKEN', DexcomExtractId('BARE-TOKEN'));
end;

procedure TDexcomHelperTests.RejectsErrorsAndNoise;
begin
  AssertEquals('Empty body', '', DexcomExtractId(''));
  AssertEquals('Dexcom error object', '',
    DexcomExtractId('{"Code":"SessionIdNotFound","Message":"Failed to find session object"}'));
  AssertEquals('Fake-server error object', '',
    DexcomExtractId('{"error":"Unknown Dexcom endpoint","path":"x"}'));
  AssertEquals('Object without an id member', '', DexcomExtractId('{"foo":"bar"}'));
  AssertEquals('Transport error text', '', DexcomExtractId('+Connection refused'));
  AssertEquals('Plain-text server error', '', DexcomExtractId('Internal Server Error'));
  AssertEquals('HTML error page', '', DexcomExtractId('<html><body>502</body></html>'));
  AssertEquals('Readings array', '',
    DexcomExtractId('[{"WT":"/Date(1610464324000)/","Value":120,"Trend":"Flat"}]'));
  AssertEquals('Malformed JSON', '', DexcomExtractId('{"accountId":'));
end;

procedure TDexcomHelperTests.DetectsPasswordRejection;
begin
  AssertTrue('AccountPasswordInvalid code',
    DexcomIsPasswordRejected('{"Code":"AccountPasswordInvalid","Message":"Publisher account password failed"}'));
  AssertFalse('Lockout is not a password rejection',
    DexcomIsPasswordRejected('{"Code":"SSO_AuthenticateMaxAttemptsExceeded"}'));
  AssertFalse('Session failure is not a password rejection',
    DexcomIsPasswordRejected('{"Code":"SessionIdNotFound"}'));
  AssertFalse('Empty body', DexcomIsPasswordRejected(''));
end;

procedure TDexcomHelperTests.DropsDuplicateReadings;
var
  r: BGResults;
begin
  // Two samples, each uploaded twice one second apart (phone and watch)
  r := MakeReadings([0, 1, 300, 301], [120, 120, 115, 115]);
  DexcomDropDuplicateReadings(r);
  AssertEquals('One reading per sample', 2, Length(r));
  AssertEquals('Newer upload of the first sample kept', 120.0, r[0].val, 0.01);
  AssertEquals('Newer upload of the second sample kept', 115.0, r[1].val, 0.01);
  AssertTrue('Order stays newest-first', r[0].date > r[1].date);

  // Readings a full interval apart are untouched
  r := MakeReadings([0, 300, 600], [120, 115, 110]);
  DexcomDropDuplicateReadings(r);
  AssertEquals('Distinct samples all kept', 3, Length(r));

  // A single reading, or none, is left alone
  r := MakeReadings([0], [120]);
  DexcomDropDuplicateReadings(r);
  AssertEquals('Single reading kept', 1, Length(r));
  SetLength(r, 0);
  DexcomDropDuplicateReadings(r);
  AssertEquals('Empty list stays empty', 0, Length(r));
end;

procedure TDexcomHelperTests.KeepsUndatedReadings;
var
  r: BGResults;
begin
  // A reading that could not be dated (date 0) is never a duplicate of its
  // neighbour, and never makes its neighbour one
  r := MakeReadings([0, 1, 2], [120, 120, 120]);
  r[1].date := 0;
  DexcomDropDuplicateReadings(r);
  AssertEquals('Undated reading and its neighbours kept', 3, Length(r));
end;

procedure TDexcomHelperTests.ComputesDeltasAfterDedupe;
var
  r: BGResults;
begin
  r := MakeReadings([0, 1, 300, 301, 600], [120, 120, 115, 115, 0]);
  DexcomDropDuplicateReadings(r);
  DexcomComputeDeltas(r);
  AssertEquals('Three readings remain', 3, Length(r));
  AssertEquals('Delta against the previous sample, not its duplicate', 5.0, r[0].delta, 0.01);
  AssertTrue('Neighbour without a value leaves the delta unset', r[1].deltaEmpty);
  AssertTrue('Last reading has no older neighbour', r[2].deltaEmpty);
end;

procedure TDexcomHelperTests.ClampsWindow;
var
  mins, cnt: integer;
begin
  mins := 5000; cnt := 1000;
  DexcomClampWindow(mins, cnt);
  AssertEquals('Minutes clamped to the Share maximum', DEXCOM_MAX_MINUTES, mins);
  AssertEquals('Count clamped to the Share maximum', DEXCOM_MAX_COUNT, cnt);

  mins := 0; cnt := -3;
  DexcomClampWindow(mins, cnt);
  AssertEquals('Minutes raised to one', 1, mins);
  AssertEquals('Count raised to one', 1, cnt);

  mins := 30; cnt := 3;
  DexcomClampWindow(mins, cnt);
  AssertEquals('In-range minutes untouched', 30, mins);
  AssertEquals('In-range count untouched', 3, cnt);
end;

procedure TDexcomHelperTests.WrongRegionMessageNamesRegion;
begin
  AssertTrue('Message carries the region name',
    Pos('Japan', DexcomWrongRegionMessage('Japan')) > 0);
end;

initialization
  RegisterTest(TDexcomHelperTests);
end.
