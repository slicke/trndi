program TestTrndi;

{$mode ObjFPC}{$H+}

uses
  SysUtils, fpcunit, testregistry, testreport,
  debug_firstx_tandem_test, debug_firstx_dexcom_test;

type
  TTrndiTest = class(TTestCase)
  published
    procedure TestExample;
  end;

procedure TTrndiTest.TestExample;
begin
  AssertEquals('1+1=2', 2, 1 + 1);
end;

begin
  RunRegisteredTests;
end.
