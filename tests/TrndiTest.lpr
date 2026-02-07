program TrndiTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  trndiTestCase1,
  dexcom_time_tests,
  dexcom_trend_tests,
  tandem_trend_tests,
  debug_intermit_test,
  debug_firstx_dexcom_test;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

