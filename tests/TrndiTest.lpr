program TrndiTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, trndiTestCase1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

