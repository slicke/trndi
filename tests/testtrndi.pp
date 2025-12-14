program TestTrndi;

{$mode ObjFPC}{$H+}

uses
  SysUtils, fpcunit, testregistry, testreport;

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
