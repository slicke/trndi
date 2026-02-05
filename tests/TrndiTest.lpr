program TrndiTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  trndiTestCase1,
  dexcom_time_tests,
  dexcom_trend_tests,
  tandem_trend_tests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

