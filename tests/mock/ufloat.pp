unit ufloat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

// Minimal mock of the TfFloat form used in the app so tests can run headless.
// Only expose fields and methods that other units/tests reference.

type
  TfFloat = class(TObject)
  public
    // Lightweight placeholders for commonly-used properties
    procedure Show; virtual;
    procedure Hide; virtual;
    class function Instance: TfFloat; static;
  end;

procedure ShowMessage(const S: string);

implementation

var
  _FloatInstance: TfFloat = nil;

procedure TfFloat.Show;
begin
  // no-op for headless tests
end;

procedure TfFloat.Hide;
begin
  // no-op for headless tests
end;

class function TfFloat.Instance: TfFloat;
begin
  if _FloatInstance = nil then
    _FloatInstance := TfFloat.Create;
  Result := _FloatInstance;
end;

procedure ShowMessage(const S: string);
begin
  // no-op in tests
end;

initialization
finalization
  _FloatInstance.Free;

end.
