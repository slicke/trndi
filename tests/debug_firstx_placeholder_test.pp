unit debug_firstx_placeholder_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry;

type
  TPlaceholderTest = class(TTestCase)
  published
    procedure Dummy;
  end;

implementation

procedure TPlaceholderTest.Dummy;
begin
  AssertTrue(True);
end;

initialization
  RegisterTest(TPlaceholderTest);

end.
