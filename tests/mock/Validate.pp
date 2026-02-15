unit Validate;

{$mode ObjFPC}{$H+}

interface

// Minimal test-only Validate unit to satisfy newer Lazarus/LCL expectations.
// Keeps tests self-contained on macOS where system validate.ppu references
// includes that are not available in developer systems.

implementation

function IsValidUTF8(const S: string): Boolean;
begin
  // Tests don't rely on strict validation; return true to be permissive.
  Result := True;
end;

end.
