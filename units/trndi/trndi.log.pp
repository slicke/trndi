(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Proxy/HTTP logging helper.
 *
 * This unit is intentionally dependency-light so it can be used from native
 * platform units without introducing circular dependencies with the extension
 * engine (trndi.ext.engine).
 *)
unit trndi.log;

{$mode ObjFPC}{$H+}
{$ifdef DARWIN}
{$ModeSwitch objectivec1}
{$endif}

interface

procedure LogMessageToFile(const Msg: string);

implementation

uses
  Classes, SysUtils
  {$ifdef DARWIN}
  , CocoaAll, nsutils.nshelpers
  {$endif}
  ;

function FallbackAppPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

{$ifdef DEBUG}
procedure LogMessageToFile(const Msg: string);
const
  MaxLines = 500;
var
  LogLines: TStringList;
  LogFilePath: string;
  {$ifdef DARWIN}
  BundleID: string;
  {$endif}
begin
  // Determine a writable log file path. On macOS we prefer Application Support
  // to avoid permission issues.
  {$ifdef DARWIN}
  try
    LogFilePath := NSStrToStr(
      NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, True)
        .objectAtIndex(0));
    BundleID := NSStrToStr(NSBundle.mainBundle.objectForInfoDictionaryKey(StrToNSStr('CFBundleIdentifier')));
    if (BundleID = '') or SameText(BundleID, 'com.company.trndi') then
      BundleID := 'com.slicke.trndi';
    LogFilePath := IncludeTrailingPathDelimiter(LogFilePath) + BundleID + PathDelim + 'trndi.log';
    if not DirectoryExists(ExtractFilePath(LogFilePath)) then
      ForceDirectories(ExtractFilePath(LogFilePath));
  except
    LogFilePath := FallbackAppPath + 'trndi.log';
  end;
  {$else}
  LogFilePath := 'trndi.log';
  {$endif}

  LogLines := TStringList.Create;
  try
    if FileExists(LogFilePath) then
      LogLines.LoadFromFile(LogFilePath);

    while LogLines.Count >= MaxLines do
      LogLines.Delete(0);

    LogLines.Add('[' + DateTimeToStr(Now) + '] ' + Msg);
    LogLines.SaveToFile(LogFilePath);
  finally
    LogLines.Free;
  end;
end;
{$else}
procedure LogMessageToFile(const Msg: string);
begin
  // Keep parameter referenced to avoid unused-parameter hints in non-DEBUG builds.
  if Msg = '' then
    Exit;
end;
{$endif}

end.
