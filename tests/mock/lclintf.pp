unit LclIntf;

{$mode ObjFPC}{$H+}

interface

uses Classes, LCLType, Controls;

// Minimal placeholder for LclIntf used in headless builds.
function getKeyShiftState: Controls.TShiftState;
procedure OpenURL(const url: AnsiString);
procedure OpenDocument(const path: AnsiString);

implementation

function getKeyShiftState: Controls.TShiftState;
begin
  Result := [];
end;

procedure OpenURL(const url: AnsiString);
begin
  // headless no-op
end;

procedure OpenDocument(const path: AnsiString);
begin
  // headless no-op
end;

end.
