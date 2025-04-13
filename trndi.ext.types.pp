                       {
unit trndi.ext.types;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses 
fgl, trndi.ext.shared;

type 
  // Specialisering för JSValueVal
  generic TJSValList<T> = class(specialize TFPGList<T>)
    public 
      // Metod för att hitta en post baserat på en specifik funktion
      function FindByFunc(FuncPtr: Pointer): T;
  end;

  // Specialisering för JSValueVal
  TJSValListJSValueVal = specialize TJSValList<JSValueVal>;

implementation

{ TJSValList<T> }

function TJSValList<T>.FindByFunc(FuncPtr: Pointer): T;

var 
  i: Integer;
begin
  Result := Default(T);
  // Eller ett annat lämpligt defaultvärde beroende på T
  for i := 0 to Self.Count - 1 do
    begin
      // Antag att T är JSValueVal
      if (Self[i].data.match = JD_FUNC) and (Self[i].data.Func = FuncPtr) then
        begin
          Result := Self[i];
          Exit;
        end;
    end;
end;

end.                  }
