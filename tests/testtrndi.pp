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

var
  LResult: TTestResult;
  LWriter: TPlainResultsWriter;

begin
  LResult := TTestResult.Create;
  try
    LWriter := TPlainResultsWriter.Create;
    try
      LResult.AddListener(LWriter);
      GetTestRegistry.Run(LResult);
      LWriter.WriteResult(LResult);
      Halt(LResult.NumberOfFailures + LResult.NumberOfErrors);
    finally
      LWriter.Free;
    end;
  finally
    LResult.Free;
  end;
end.
