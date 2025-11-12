                       {
unit trndi.ext.types;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses 
fgl, trndi.ext.shared;

type 
  // Specialization for JSValueVal
  generic TJSValList<T> = class(specialize TFPGList<T>)
    public 
      // Method to find an entry based on a specific function
      function FindByFunc(FuncPtr: Pointer): T;
  end;

  // Specialization for JSValueVal
  TJSValListJSValueVal = specialize TJSValList<JSValueVal>;

implementation

{ TJSValList<T> }

function TJSValList<T>.FindByFunc(FuncPtr: Pointer): T;

var 
  i: Integer;
begin
  Result := Default(T);
  // Or another appropriate default value depending on T
  for i := 0 to Self.Count - 1 do
    begin
      // Assume T is JSValueVal
      if (Self[i].data.match = JD_FUNC) and (Self[i].data.Func = FuncPtr) then
        begin
          Result := Self[i];
          Exit;
        end;
    end;
end;

end.                  }
