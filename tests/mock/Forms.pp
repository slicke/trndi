unit Forms;

{$mode ObjFPC}{$H+}

interface

uses Controls;

type
  TForm = class(TWinControl)
  end;

  TApplication = class(TComponent)
  private
    FExeName: string;
  public
    property ExeName: string read FExeName write FExeName;
  end;

var
  Application: TApplication;

implementation

initialization
  Application := TApplication.Create;
  Application.ExeName := ParamStr(0);

end.
