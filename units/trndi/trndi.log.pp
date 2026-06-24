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
{$if defined(DEBUG) and defined(DARWIN)}
{$ModeSwitch objectivec1}
{$endif}

interface

procedure TrndiDLog(const Msg: string); // Debug log entry; only active in DEBUG builds
procedure TrndiELog(const Msg: string); // Error log entry
procedure TrndiWLog(const Msg: string); // Warning log entry
procedure TrndiNetLog(const Msg: string); // Network log entry (debug only)


implementation

{$ifdef DEBUG}
uses
Classes, SysUtils
{$ifdef DARWIN}
  , CocoaAll, nsutils.nshelpers
{$endif}
;

const
  TimestampFmt = 'yyyy-mm-dd hh:nn:ss.zzz';

var
LogFilePath: string;
LogLock: TRTLCriticalSection;
LogLockInited: boolean = False;
FInitLog: TextFile;

function FallbackAppPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function ComputeLogFilePath: string;
{$ifdef DARWIN}
var
  BundleID: string;
{$endif}
begin
{$ifdef DARWIN}
  try
    Result := NSStrToStr(
      NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, True)
        .objectAtIndex(0));
    BundleID := NSStrToStr(NSBundle.mainBundle.objectForInfoDictionaryKey(StrToNSStr('CFBundleIdentifier')));
    if (BundleID = '') or SameText(BundleID, 'com.company.trndi') then
      BundleID := 'com.slicke.trndi';
    Result := IncludeTrailingPathDelimiter(Result) + BundleID + PathDelim + 'trndi.log';
    if not DirectoryExists(ExtractFilePath(Result)) then
      ForceDirectories(ExtractFilePath(Result));
  except
    Result := FallbackAppPath + 'trndi.log';
  end;
{$else}
  Result := 'trndi.log';
{$endif}
end;

procedure TrndiELog(const Msg: string);
begin
  TrndiDLog('[ERROR] ' + Msg);
end;

procedure TrndiWLog(const Msg: string);
begin
  TrndiDLog('[WARNING] ' + Msg);
end;

procedure TrndiNetLog(const Msg: string);
begin
  TrndiDLog('[NETWORK] ' + Msg);
end;

procedure TrndiDLog(const Msg: string);
const
  MaxAttempts = 6;
  AttemptDelayMs = 120; // ms
var
  attempt: integer;
  wroteOk: boolean;
  F: TextFile;
  Line: string;
begin
  if LogFilePath = '' then
    LogFilePath := ComputeLogFilePath;

  Line := '[' + FormatDateTime(TimestampFmt, Now) + '] ' + Msg;

  if LogLockInited then
    EnterCriticalSection(LogLock);
  try
    // Try appending the single line with retries; on persistent failure write to .locked
    wroteOk := false;
    for attempt := 1 to MaxAttempts do
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
        wroteOk := true;
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

    if not wroteOk then
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
  finally
    if LogLockInited then
      LeaveCriticalSection(LogLock);
  end;
end;

initialization
InitCriticalSection(LogLock);
LogLockInited := True;
try
  LogFilePath := ComputeLogFilePath;

  // Best-effort truncate; if locked, ignore and continue.
  try
    AssignFile(FInitLog, LogFilePath);
    {$I-}
    Rewrite(FInitLog);
    {$I+}
    if IOResult = 0 then
    begin
      Writeln(FInitLog, '[' + FormatDateTime(TimestampFmt, Now) + '] ' + 'trndi.log: truncated at startup');
      CloseFile(FInitLog);
    end;
  except
    // ignore
  end;
except
  // ignore
end;

finalization
if LogLockInited then
begin
  DoneCriticalSection(LogLock);
  LogLockInited := False;
end;

{$else}

procedure TrndiDLog(const Msg: string); begin if Msg = '' then Exit; end;
procedure TrndiELog(const Msg: string); begin if Msg = '' then Exit; end;
procedure TrndiWLog(const Msg: string); begin if Msg = '' then Exit; end;
procedure TrndiNetLog(const Msg: string); begin if Msg = '' then Exit; end;

{$endif}

end.
