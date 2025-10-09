unit VersionInfo;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils
{$IFDEF WINDOWS}, Windows{$ENDIF},
{$IFDEF FPC}
  // FCL enheter för versionsinformation
fileinfo, versiontypes, versionresource
{$ENDIF};

function GetProductVersion(const def: string): string;
{ Return MAJOR.MINOR only (e.g., "2.4").
  Fallbacks: parse from GetProductVersion(def); if missing, return def. }
function GetProductVersionMajorMinor(const def: string): string;

implementation

{$IFDEF WINDOWS}
// Windows API-definitioner som behövs om vi inte använder FCL
type
VS_FIXEDFILEINFO = record
  dwSignature: DWORD;
  dwStrucVersion: DWORD;
  dwFileVersionMS: DWORD;
  dwFileVersionLS: DWORD;
  dwProductVersionMS: DWORD;
  dwProductVersionLS: DWORD;
  dwFileFlagsMask: DWORD;
  dwFileFlags: DWORD;
  dwFileOS: DWORD;
  dwFileType: DWORD;
  dwFileSubtype: DWORD;
  dwFileDateMS: DWORD;
  dwFileDateLS: DWORD;
end;
PVSFixedFileInfo = ^VS_FIXEDFILEINFO;

// Importera version.dll funktioner direkt
function GetFileVersionInfoSizeA(lptstrFilename: pchar; var lpdwHandle: DWORD): DWORD; stdcall; external 'version.dll' name 'GetFileVersionInfoSizeA';
function GetFileVersionInfoA(lptstrFilename: pchar; dwHandle: DWORD; dwLen: DWORD; lpData: Pointer): BOOL; stdcall; external 'version.dll' name 'GetFileVersionInfoA';
function VerQueryValueA(pBlock: Pointer; lpSubBlock: pchar; var lplpBuffer: Pointer; var puLen: DWORD): BOOL; stdcall; external 'version.dll' name 'VerQueryValueA';

// Här använder vi alias för att kunna använda samma funktionsnamn oavsett om det är Unicode eller inte
function GetFileVersionInfoSize(lptstrFilename: pchar; var lpdwHandle: DWORD): DWORD; stdcall;
begin
  Result := GetFileVersionInfoSizeA(lptstrFilename, lpdwHandle);
end;

function GetFileVersionInfo(lptstrFilename: pchar; dwHandle: DWORD; dwLen: DWORD; lpData: Pointer): BOOL; stdcall;
begin
  Result := GetFileVersionInfoA(lptstrFilename, dwHandle, dwLen, lpData);
end;

function VerQueryValue(pBlock: Pointer; lpSubBlock: pchar; var lplpBuffer: Pointer; var puLen: DWORD): BOOL; stdcall;
begin
  Result := VerQueryValueA(pBlock, lpSubBlock, lplpBuffer, puLen);
end;

// Hjälpfunktioner för manipulering av DWORD-värden
function HiWord(Value: DWORD): word;
begin
  Result := word(Value shr 16);
end;

function LoWord(Value: DWORD): word;
begin
  Result := word(Value);
end;
{$ENDIF}

function GetProductVersion(const def: string): string;
  {$IFDEF FPC}
var
  FileVerInfo: TFileVersionInfo;
  {$ELSE}
  {$IFDEF WINDOWS}
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerValue: PVSFixedFileInfo;
  FileInfo: Pointer;
  FileName: string;
  {$ENDIF}
  {$ENDIF}
begin
  Result := def; // Standardvärde som används om inget annat hittas

  {$IFDEF FPC}
  // FCL-baserad versionsdetektion som fungerar på alla plattformar
  try
    FileVerInfo := TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName := ParamStr(0);
      FileVerInfo.ReadFileInfo;

      // Försök först med produktversionssträngen
      Result := FileVerInfo.VersionStrings.Values['ProductVersion'];

      // Om det inte fungerar, pröva fileversionssträngen
      if (Result = '') or (Result = '0.0.0.0') then
        Result := FileVerInfo.VersionStrings.Values['FileVersion'];

      // Om vi fortfarande inte har något, använd numeriska värden
      if (Result = '') or (Result = '0.0.0.0') then
        Result := Format('%s.%s.%s.%s', [
          FileVerInfo.VersionStrings[0],
          FileVerInfo.VersionStrings[1],
          FileVerInfo.VersionStrings[2],
          FileVerInfo.VersionStrings[3]
          ]);

      // Om det fortfarande är 0.0.0.0, använd standardvärdet
      if (Result = '') or (Result = '0.0.0.0') then
        Result := def;
    finally
      FileVerInfo.Free;
    end;
  except
    // Om något går fel, använd standardvärdet
    Result := def;
  end;
  {$ELSE}
  {$IFDEF WINDOWS}
  // Windows-specifik implementering baserad på Windows API
  FileName := ParamStr(0);
  VerInfoSize := GetFileVersionInfoSize(pchar(FileName), Dummy);

  if VerInfoSize > 0 then
  begin
    GetMem(FileInfo, VerInfoSize);
    try
      if GetFileVersionInfo(pchar(FileName), 0, VerInfoSize, FileInfo) then
        if VerQueryValue(FileInfo, '\', Pointer(VerValue), VerValueSize) then
          with VerValue^ do
          begin
            Result := Format('%d.%d.%d.%d', [
              HiWord(dwProductVersionMS),
              LoWord(dwProductVersionMS),
              HiWord(dwProductVersionLS),
              LoWord(dwProductVersionLS)
              ]);

            // Om resultatet är tomt eller nollor, använd standardvärdet
            if (Result = '') or (Result = '0.0.0.0') then
              Result := def;
          end;
    finally
      FreeMem(FileInfo, VerInfoSize);
    end;
  end;
  {$ENDIF}
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  GetProductVersionMajorMinor
  ---------------------------
  Returns only the first two numeric components (MAJOR.MINOR) of the product
  version. It parses the result of GetProductVersion(def) and extracts the two
  leading integer groups, ignoring any non-digit separators or suffixes.
  Examples:
    "2.4.1.0"   -> "2.4"
    "v3.10-beta"-> "3.10"
    "1"         -> "1.0"
    ""          -> def
 ------------------------------------------------------------------------------}
function GetProductVersionMajorMinor(const def: string): string;
var
  full: string;
  i, nCount, cur: integer;
  inNum: boolean;
  nums: array[0..3] of integer;
begin
  full := GetProductVersion(def);
  if Trim(full) = '' then
    Exit(def);

  for i := 0 to High(nums) do
    nums[i] := -1;
  nCount := 0; cur := 0; inNum := false;

  for i := 1 to Length(full) do
    if (full[i] >= '0') and (full[i] <= '9') then
    begin
      inNum := true;
      cur := cur * 10 + Ord(full[i]) - Ord('0');
    end
    else
    if inNum then
    begin
      if nCount <= High(nums) then
      begin
        nums[nCount] := cur;
        Inc(nCount);
      end;
      cur := 0;
      inNum := false;
      if nCount >= 2 then
        Break; // we have major & minor
    end;

  // Flush final number if string ended with digits
  if inNum and (nCount <= High(nums)) then
  begin
    nums[nCount] := cur;
    Inc(nCount);
  end;

  if (nCount >= 2) and (nums[0] >= 0) and (nums[1] >= 0) then
    Result := IntToStr(nums[0]) + '.' + IntToStr(nums[1])
  else
  if (nCount >= 1) and (nums[0] >= 0) then
    Result := IntToStr(nums[0]) + '.0'
  else
    Result := def;
end;

end.
