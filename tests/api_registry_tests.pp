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
unit api_registry_tests;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.api, trndi.api.registry;

type

{** Covers the backend registry that the settings form, first-run wizard and
    main form all resolve backends through: code/name round-trips, generic
    instantiation via the virtual constructors, and the shared credential
    rules. }
TAPIRegistryTester = class(TTestCase)
published
  procedure TestCodeNameRoundTrip;
  procedure TestCreateBackendInstantiatesRegisteredClass;
  procedure TestCreateBackendAcceptsLegacyDisplayNames;
  procedure TestUnknownBackendFallbacks;
  procedure TestBackendExistsSeparatesKnownFromUnknown;
  procedure TestIsDebugBackendRecognizesCodesAndNames;
  procedure TestCredentialRules;
end;

implementation

procedure TAPIRegistryTester.TestCodeNameRoundTrip;
var
  e: TBackendEntry;
begin
  for e in BackendRegistry do
  begin
    AssertEquals('code -> name for ' + e.code, e.name, BackendDisplayName(e.code));
    AssertEquals('name -> code for ' + e.code, e.code, BackendCode(e.name));
    AssertTrue('class of code ' + e.code, BackendClassOf(e.code) = e.cls);
    AssertTrue('class of name ' + e.name, BackendClassOf(e.name) = e.cls);
  end;
end;

{ The regional backend classes are instantiated through the TrndiAPI metaclass,
  which requires their two-argument constructors to be virtual overrides; a
  missing `override` would silently construct the base class instead. }
procedure TAPIRegistryTester.TestCreateBackendInstantiatesRegisteredClass;
var
  e: TBackendEntry;
  api: TrndiAPI;
begin
  for e in BackendRegistry do
  begin
    api := CreateBackend(e.code, 'https://example.com', '{}');
    try
      AssertNotNull('instance for ' + e.code, api);
      AssertEquals('class for ' + e.code, e.cls.ClassName, api.ClassName);
    finally
      api.Free;
    end;
  end;
end;

procedure TAPIRegistryTester.TestCreateBackendAcceptsLegacyDisplayNames;
var
  api: TrndiAPI;
begin
  // Old installations stored the display name in 'remote.type'
  api := CreateBackend(BackendDisplayName('API_DEX_USA'), 'user', 'password');
  try
    AssertNotNull(api);
    AssertEquals('DexcomUSA', api.ClassName);
  finally
    api.Free;
  end;
end;

procedure TAPIRegistryTester.TestUnknownBackendFallbacks;
begin
  AssertTrue('unknown class is nil', BackendClassOf('bogus') = nil);
  AssertNull('unknown backend not created', CreateBackend('bogus', 'a', 'b'));
  // Load/save fall back to NightScout, matching the historical behavior
  AssertEquals('API_NS', BackendCode('bogus'));
  AssertEquals(BackendDisplayName('API_NS'), BackendDisplayName('bogus'));
end;

{ Those NightScout fallbacks are why callers that persist 'remote.type' must ask
  BackendExists first — otherwise an unresolvable stored value silently becomes
  a valid-looking one and overwrites the user's configuration. }
procedure TAPIRegistryTester.TestBackendExistsSeparatesKnownFromUnknown;
var
  e: TBackendEntry;
begin
  for e in BackendRegistry do
  begin
    AssertTrue('code exists: ' + e.code, BackendExists(e.code));
    AssertTrue('name exists: ' + e.name, BackendExists(e.name));
  end;
  AssertFalse('bogus does not exist', BackendExists('bogus'));
  AssertFalse('empty does not exist', BackendExists(''));
  // The reported symptom: a real code with one stray character appended.
  AssertFalse('near-miss code does not exist', BackendExists('API_D_FIRSTXd'));
end;

{ IsDebugBackend answers from the code/name markers rather than the registry, so
  a release build — where the debug entries are compiled out — still recognizes
  a debug value it can never resolve. }
procedure TAPIRegistryTester.TestIsDebugBackendRecognizesCodesAndNames;
begin
  AssertTrue('debug code', IsDebugBackend('API_D_FIRSTX'));
  // Unresolvable but still identifiable as a debug value
  AssertTrue('near-miss debug code', IsDebugBackend('API_D_FIRSTXd'));
  AssertTrue('legacy debug display name', IsDebugBackend('* Debug Backend *'));
  AssertFalse('real backend code', IsDebugBackend('API_NS'));
  AssertFalse('real backend name', IsDebugBackend('NightScout'));
  AssertFalse('empty', IsDebugBackend(''));
end;

procedure TAPIRegistryTester.TestCredentialRules;

procedure Check(const msg: string; expected: TBackendCredError;
  const backend, addr, pass: string);
  begin
    AssertEquals(msg, Ord(expected), Ord(CheckBackendCredentials(backend, addr, pass)));
  end;

begin
  Check('NS needs http', bceAddress, 'API_NS', 'example.com', '');
  Check('NS ok', bceNone, 'API_NS', 'https://example.com', '');
  // Rules resolve display names too (the forms pass the picker text)
  Check('NS by name', bceAddress, BackendDisplayName('API_NS'), 'example.com', '');
  Check('Tandem needs email', bceEmail, 'API_TANDEM_USA', 'foo', '12345');
  Check('Tandem needs password', bcePassword, 'API_TANDEM_USA', 'a@b.c', '1234');
  Check('Tandem ok', bceNone, 'API_TANDEM_USA', 'a@b.c', '12345');
  Check('Dexcom needs password', bcePassword, 'API_DEX_USA', 'user', '1234');
  Check('Dexcom ok', bceNone, 'API_DEX_USA', 'user', '12345');
  Check('CareLink needs token blob', bceToken, 'API_CARELINK_US', 'carelink', 'notjson');
  Check('CareLink ok', bceNone, 'API_CARELINK_US', 'carelink', '  {"token":1}');
  Check('LibreLinkUp needs email', bceEmail, 'API_LLU', 'follower', '12345');
  Check('LibreLinkUp needs password', bcePassword, 'API_LLU', 'a@b.c', '1234');
  Check('LibreLinkUp ok', bceNone, 'API_LLU', 'a@b.c', '12345');
  Check('xDrip has no rule', bceNone, 'API_XDRIP', '', '');
  Check('unknown has no rule', bceNone, 'bogus', '', '');
end;

initialization
  RegisterTest(TAPIRegistryTester);
end.
