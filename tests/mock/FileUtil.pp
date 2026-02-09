unit FileUtil;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils;

function ExtractFilePath(const S: string): string; inline;
function ReadFileToString(const AFileName: string): string;

implementation

function ExtractFilePath(const S: string): string; inline;
begin
  Result := S;
end;

function ReadFileToString(const AFileName: string): string;
var
  SL: TStringList;
begin
  Result := '';
  if not FileExists(AFileName) then Exit;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.