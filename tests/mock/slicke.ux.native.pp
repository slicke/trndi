unit slicke.ux.native;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Forms;

function FontGUIInList(out fname: string): boolean;
function FontTXTInList(out fname: string): boolean;
function IsProblematicWM: boolean;
function IsSemiProblematicWM: boolean;
function ShowFormModalSafe(aForm: TForm): integer;

implementation

function FontGUIInList(out fname: string): boolean;
begin
  fname := 'font';
  Result := true;
end;

function FontTXTInList(out fname: string): boolean;
begin
  fname := 'font';
  Result := true;
end;

function IsProblematicWM: boolean;
begin
  Result := false;
end;

function IsSemiProblematicWM: boolean;
begin
  Result := false;
end;

function ShowFormModalSafe(aForm: TForm): integer;
begin
  if not Assigned(aForm) then
    Exit(0);
  Result := 0; // mrNone
end;

end.
