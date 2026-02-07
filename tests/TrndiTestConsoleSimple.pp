program TrndiTestConsoleSimple;

{$mode ObjFPC}{$H+}

uses
  SysUtils, fpcunit, testregistry, testreport,
  dexcom_time_tests,
  dexcom_trend_tests,
  tandem_trend_tests,
  debug_intermit_test,
  debug_firstx_dexcom_test,
  debug_firstx_tandem_test,
  debug_firstx_placeholder_test,
  trndi_native_mock_test;

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
