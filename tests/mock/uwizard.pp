unit uwizard;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Controls, Forms, Dialogs;

type
  TfWizard = class(TForm)
  public
    constructor Create(AOwner: Controls.TComponent; ANative: TObject); reintroduce;
    function ShowModal: integer;
  end;

implementation

constructor TfWizard.Create(AOwner: Controls.TComponent; ANative: TObject);
begin
  inherited Create(AOwner);
end;

function TfWizard.ShowModal: integer;
begin
  Result := mrOk;
end;

end.
