(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)
(*
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

{** Blank out credential values in a URL's query string so a request URL can be
    logged in full without putting a live token in trndi.log.

    Backends authenticate via the query string as well as via headers -- Dexcom
    Share passes @code(sessionId), which grants read access to the account's
    glucose data until it expires -- and trndi.log is exactly the file a user
    attaches to a bug report. Parameter names and every non-secret value are
    kept, since those (@code(minutes), @code(maxCount), ...) are the diagnostic
    point of logging the URL at all.

    Distinct from the nested @code(SafeUrlForLog) helpers in the native units,
    which drop the query string wholesale; those log URLs whose parameters are
    never interesting. Implemented with System-unit routines only, so it is
    available in release builds too (where the log procedures are no-ops).
    @param(AUrl URL to sanitise)
    @returns(The URL with secret parameter values replaced by "<redacted>") }
function TrndiSafeUrl(const AUrl: string): string;

implementation

{$ifdef DEBUG}
uses
Classes, SysUtils
{$ifdef DARWIN}
  , CocoaAll, nsutils.nshelpers
{$endif}
;
{$endif}

{------------------------------------------------------------------------------
  Redact credential values from a URL's query string. Defined outside the
  DEBUG conditional (and written against the System unit only) so callers can
  sanitise a URL regardless of build mode.
 ------------------------------------------------------------------------------}
function TrndiSafeUrl(const AUrl: string): string;
const
  // Matched as substrings, so 'token' also covers access_token/refresh_token
  // and 'key' covers apikey/api_key. Over-redacting a harmless parameter costs
  // nothing; missing a credential does not.
  SECRET_MARKERS: array[0..8] of string = (
    'session', 'token', 'secret', 'password', 'passwd',
    'key', 'auth', 'signature', 'sig');
  REDACTED = '<redacted>';
var
  qpos, cut, eq: integer;
  query, part, acc, nm: string;

  function AsciiLower(const S: string): string;
  var
    k: integer;
  begin
    Result := S;
    for k := 1 to Length(Result) do
      if (Result[k] >= 'A') and (Result[k] <= 'Z') then
        Result[k] := Chr(Ord(Result[k]) + 32);
  end;

  function IsSecretName(const AName: string): boolean;
  var
    k: integer;
    lowered: string;
  begin
    Result := false;
    lowered := AsciiLower(AName);
    for k := Low(SECRET_MARKERS) to High(SECRET_MARKERS) do
      if Pos(SECRET_MARKERS[k], lowered) > 0 then
        Exit(true);
  end;

begin
  Result := AUrl;
  qpos := Pos('?', Result);
  if qpos <= 0 then
    Exit;

  query := Copy(Result, qpos + 1, Length(Result));
  Result := Copy(Result, 1, qpos); // keep the '?'

  acc := '';
  while query <> '' do
  begin
    cut := Pos('&', query);
    if cut > 0 then
    begin
      part := Copy(query, 1, cut - 1);
      query := Copy(query, cut + 1, Length(query));
    end
    else
    begin
      part := query;
      query := '';
    end;

    if acc <> '' then
      acc := acc + '&';

    eq := Pos('=', part);
    if eq > 0 then
    begin
      nm := Copy(part, 1, eq - 1);
      if IsSecretName(nm) then
        part := nm + '=' + REDACTED;
    end;
    acc := acc + part;
  end;

  Result := Result + acc;
end;

{$ifdef DEBUG}

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
