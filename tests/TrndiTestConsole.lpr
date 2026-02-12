program TrndiTestConsole;

{$mode objfpc}{$H+}
{$DEFINE TEST}

uses
  {$IFDEF UNIX}cthreads, {$ENDIF}
  sysutils, fpcunit, testregistry, testreport,
  dexcom_time_tests,
  dexcom_trend_tests,
  tandem_trend_tests,
  tandem_fixture_tests,
  debug_intermit_test,
  debug_firstx_dexcom_test,
  debug_firstx_tandem_test,
  trndi_native_mock_test,
  umain_tests,
  native_cookie_tests,
  api_general_tests,
  api_dexcom_tests,
  api_xdrip_tests,
  api_nightscout_tests,
  system_media_controller_tests;

var
  LResult: TTestResult;
  LWriter: TPlainResultsWriter;

begin
  // Run all registered tests and report results to stdout
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
