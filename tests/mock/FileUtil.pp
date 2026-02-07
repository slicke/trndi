unit FileUtil;

{$mode ObjFPC}{$H+}

interface

function ExtractFilePath(const S: string): string; inline;

implementation

function ExtractFilePath(const S: string): string; inline;
begin
  Result := S;
end;

end.