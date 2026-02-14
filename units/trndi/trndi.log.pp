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

var
LogFilePath: string;
FLogFile: TextFile;

function FallbackAppPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

{$ifdef DEBUG}
procedure LogMessageToFile(const Msg: string);
const
  MaxAttempts = 6;
  AttemptDelayMs = 120; // ms
var
  LogFilePath: string;
  attempt: Integer;
  wroteOk: Boolean;
  F: TextFile;
  Line: string;
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

  Line := '[' + DateTimeToStr(Now) + '] ' + Msg;

  // Try appending the single line with retries; on persistent failure write to .locked
  wroteOk := False;
  for attempt := 1 to MaxAttempts do
  begin
    try
      AssignFile(F, LogFilePath);
      {$I-}
      if not FileExists(LogFilePath) then
        Rewrite(F)
      else
        Append(F);
      {$I+}
      if IOResult = 0 then
      begin
        Writeln(F, Line);
        CloseFile(F);
        wroteOk := True;
        Break;
      end
      else
      begin
        // Could not open (possibly locked) — wait and retry
        try CloseFile(F) except end;
        Sleep(AttemptDelayMs);
      end;
    except
      on E: Exception do
      begin
        try CloseFile(F) except end;
        Sleep(AttemptDelayMs);
      end;
    end;
  end;

  if not wroteOk then
  begin
    try
      AssignFile(F, LogFilePath + '.locked');
      {$I-}
      if not FileExists(LogFilePath + '.locked') then
        Rewrite(F)
      else
        Append(F);
      {$I+}
      if IOResult = 0 then
      begin
        Writeln(F, Line);
        CloseFile(F);
      end;
    except
      // Swallow errors — logger must not raise during debugging
    end;
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

initialization
  {$ifdef DEBUG}
  // Truncate the debug log at process start so we append during runtime.
  try
    {$ifdef DARWIN}
    var BundleID: string;
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

    // Best-effort truncate; if locked, ignore and continue.
    try
      AssignFile(FLogFile, LogFilePath);
      {$I-}
      Rewrite(FLogFile);
      {$I+}
      if IOResult = 0 then
      begin
        Writeln(FLogFile, '[' + DateTimeToStr(Now) + '] ' + 'trndi.log: truncated at startup');
        CloseFile(FLogFile);
      end;
    except
      // ignore
    end;
  except
    // ignore
  end;
  {$endif}

finalization
  // nothing
end.
