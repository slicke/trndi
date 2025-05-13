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

function GetProductVersion(const def: string): String;

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
function GetFileVersionInfoSizeA(lptstrFilename: PChar; var lpdwHandle: DWORD): DWORD; stdcall; external 'version.dll' name 'GetFileVersionInfoSizeA';
function GetFileVersionInfoA(lptstrFilename: PChar; dwHandle: DWORD; dwLen: DWORD; lpData: Pointer): BOOL; stdcall; external 'version.dll' name 'GetFileVersionInfoA';
function VerQueryValueA(pBlock: Pointer; lpSubBlock: PChar; var lplpBuffer: Pointer; var puLen: DWORD): BOOL; stdcall; external 'version.dll' name 'VerQueryValueA';

// Här använder vi alias för att kunna använda samma funktionsnamn oavsett om det är Unicode eller inte
function GetFileVersionInfoSize(lptstrFilename: PChar; var lpdwHandle: DWORD): DWORD; stdcall;
begin
  Result := GetFileVersionInfoSizeA(lptstrFilename, lpdwHandle);
end;

function GetFileVersionInfo(lptstrFilename: PChar; dwHandle: DWORD; dwLen: DWORD; lpData: Pointer): BOOL; stdcall;
begin
  Result := GetFileVersionInfoA(lptstrFilename, dwHandle, dwLen, lpData);
end;

function VerQueryValue(pBlock: Pointer; lpSubBlock: PChar; var lplpBuffer: Pointer; var puLen: DWORD): BOOL; stdcall;
begin
  Result := VerQueryValueA(pBlock, lpSubBlock, lplpBuffer, puLen);
end;

// Hjälpfunktioner för manipulering av DWORD-värden
function HiWord(Value: DWORD): Word;
begin
  Result := Word(Value shr 16);
end;

function LoWord(Value: DWORD): Word;
begin
  Result := Word(Value);
end;
{$ENDIF}

function GetProductVersion(const def: string): String;
{$IFDEF FPC}
var
  FileVerInfo: TFileVersionInfo;
{$ELSE}
{$IFDEF WINDOWS}
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerValue: PVSFixedFileInfo;
  FileInfo: Pointer;
  FileName: String;
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
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);

  if VerInfoSize > 0 then
  begin
    GetMem(FileInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, FileInfo) then
      begin
        if VerQueryValue(FileInfo, '\', Pointer(VerValue), VerValueSize) then
        begin
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
        end;
      end;
    finally
      FreeMem(FileInfo, VerInfoSize);
    end;
  end;
  {$ENDIF}
  {$ENDIF}
end;

end.
