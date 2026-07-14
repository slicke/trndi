unit LResources;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// Minimal placeholder for LResources used in headless builds. Real Lazarus
// resources (see units/trndi/api/carelink_assets.lrs) are compiled-in binary
// data that headless console tests never need to read; LazarusResources.Add
// is a no-op here and TLazarusResourceStream always yields an empty stream.

type
  TLResourceList = class(TObject)
  public
    procedure Add(const Name, ValueType: AnsiString; const Values: array of string);
  end;

  TLazarusResourceStream = class(TMemoryStream)
  public
    constructor Create(const ResName: string; ResType: PChar);
  end;

var
  LazarusResources: TLResourceList;

implementation

procedure TLResourceList.Add(const Name, ValueType: AnsiString; const Values: array of string);
begin
  // no-op: headless test builds don't need the embedded asset bytes
end;

constructor TLazarusResourceStream.Create(const ResName: string; ResType: PChar);
begin
  inherited Create;
end;

initialization
  LazarusResources := TLResourceList.Create;

finalization
  LazarusResources.Free;

end.
