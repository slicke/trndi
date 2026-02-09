unit LazFileUtils;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, Classes;

function GetAppConfigDirUTF8(LocalAppData: Boolean = False; OnlyCreate: Boolean = False): string;
function ForceDirectoriesUTF8(const Dir: string): Boolean;

implementation

function GetAppConfigDirUTF8(LocalAppData: Boolean = False; OnlyCreate: Boolean = False): string;
begin
  // Return a reasonable per-user config dir in tests (relative path)
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'config' + DirectorySeparator;
  if OnlyCreate then
    ForceDirectoriesUTF8(Result);
end;

function ForceDirectoriesUTF8(const Dir: string): Boolean;
begin
  // Use SysUtils.ForceDirectories to create directories
  Result := ForceDirectories(Dir);
end;

end.