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
  Check('xDrip has no rule', bceNone, 'API_XDRIP', '', '');
  Check('unknown has no rule', bceNone, 'bogus', '', '');
end;

initialization
  RegisterTest(TAPIRegistryTester);
end.
