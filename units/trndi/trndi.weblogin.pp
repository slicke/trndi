(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2026 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * GitHub: https://github.com/slicke/trndi
 *)

{** Shared runner for backend "assisted login" helper scripts (see
    TrndiAPI.webLoginScript). A backend can ship a small Node.js script that
    performs an interactive browser login and prints the resulting credential
    (a JSON blob) to stdout; this unit locates Node, installs the helper's
    dependencies on first use, runs it while keeping the UI responsive, and
    returns the captured credential. Both the settings form and the first-run
    wizard use it so the behaviour stays identical. }
unit trndi.weblogin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  {** Progress callback: invoked with a short status caption as the login
      proceeds (e.g. to update a button caption). May be nil. }
  TWebLoginProgress = procedure(const ACaption: string) of object;

  {** Outcome of RunNodeLoginHelper. Callers map failures to their own
      localized message so this unit stays UI/locale-agnostic. }
  TWebLoginResult = (
    wlrOK,           /// Credential captured successfully
    wlrNoScript,     /// The helper script was not found next to the executable
    wlrNoNode,       /// Node.js is not available on PATH
    wlrNpmFailed,    /// `npm install` of the helper's dependencies failed
    wlrLaunchFailed, /// The helper process could not be started
    wlrNoOutput,     /// The helper produced no token (cancelled/timed out)
    wlrBadOutput     /// The helper's output was not a valid credential blob
  );

{** Run a backend's Node.js login helper and capture the credential it prints.

    @param(AScriptRel  Helper path relative to the executable directory, using
                       forward slashes, as returned by TrndiAPI.webLoginScript)
    @param(AScriptArgs Extra command-line argument for the script, e.g. a region
                       flag like "--us"; empty for none)
    @param(ACaptionInstalling Progress caption reported while `npm install` runs)
    @param(ACaptionWaiting    Progress caption reported while awaiting the login)
    @param(AOnProgress Optional progress callback; may be nil)
    @param(ACred       Receives the captured credential (JSON) on success)
    @returns(@code(wlrOK) on success; otherwise a failure code) }
function RunNodeLoginHelper(const AScriptRel, AScriptArgs: string;
  const ACaptionInstalling, ACaptionWaiting: string;
  const AOnProgress: TWebLoginProgress;
  out ACred: string): TWebLoginResult;

implementation

uses
  Forms, process, pipes, FileUtil, lclintf, math;

const
  PUMP_MS = 40;               // message-pump / poll interval
  MAX_RUN_MS = 7 * 60 * 1000; // hard guard; the helper self-times-out at 5 min

{ Read whatever bytes are currently available on a pipe, as a string. Returns
  '' when nothing is waiting so the caller never blocks. }
function DrainPipe(pipe: TInputPipeStream): string;
var
  avail, got: longint;
begin
  Result := '';
  if pipe = nil then
    Exit;
  avail := pipe.NumBytesAvailable;
  if avail <= 0 then
    Exit;
  SetLength(Result, avail);
  got := pipe.Read(Result[1], avail);
  if got <> avail then
    SetLength(Result, Max(got, 0));
end;

{ Run an executable to completion, pumping the UI. Captures stdout (the token
  stream) into AStdOut; stderr is drained but discarded (the helper logs
  progress there). Returns the exit code, or -1 if it could not be started. }
function RunProc(const AExe: string; const AParams: array of string;
  const AWorkDir: string; out AStdOut: string): integer;
var
  proc: TProcess;
  i: integer;
  started: QWord;
begin
  Result := -1;
  AStdOut := '';
  proc := TProcess.Create(nil);
  try
    proc.Executable := AExe;
    for i := Low(AParams) to High(AParams) do
      proc.Parameters.Add(AParams[i]);
    if AWorkDir <> '' then
      proc.CurrentDirectory := AWorkDir;
    proc.Options := [poUsePipes, poNoConsole];
    proc.ShowWindow := swoHIDE;
    try
      proc.Execute;
    except
      Exit(-1);
    end;

    started := GetTickCount64;
    repeat
      AStdOut := AStdOut + DrainPipe(proc.Output);
      DrainPipe(proc.Stderr); // drain so a full stderr pipe never blocks the child
      Application.ProcessMessages;
      Sleep(PUMP_MS);
      if (GetTickCount64 - started) > MAX_RUN_MS then
      begin
        proc.Terminate(1);
        Break;
      end;
    until not proc.Running;

    // Drain anything buffered after exit.
    AStdOut := AStdOut + DrainPipe(proc.Output);
    DrainPipe(proc.Stderr);
    Result := proc.ExitStatus;
  finally
    proc.Free;
  end;
end;

procedure Report(const AOnProgress: TWebLoginProgress; const ACaption: string);
begin
  if Assigned(AOnProgress) then
    AOnProgress(ACaption);
  Application.ProcessMessages;
end;

function RunNodeLoginHelper(const AScriptRel, AScriptArgs: string;
  const ACaptionInstalling, ACaptionWaiting: string;
  const AOnProgress: TWebLoginProgress;
  out ACred: string): TWebLoginResult;
var
  scriptPath, helperDir, nodeExe, npmExe, npmOut: string;
  npmParams: array of string;
  code: integer;
begin
  ACred := '';

  if AScriptRel = '' then
    Exit(wlrNoScript);

  // Resolve the helper next to the executable (forward slashes in the relative
  // path become the platform separator).
  scriptPath := ExtractFilePath(Application.ExeName) +
    StringReplace(AScriptRel, '/', PathDelim, [rfReplaceAll]);
  if not FileExists(scriptPath) then
    Exit(wlrNoScript);
  helperDir := ExtractFileDir(scriptPath);

  nodeExe := FindDefaultExecutablePath('node');
  if nodeExe = '' then
    Exit(wlrNoNode);

  // First run: fetch the helper's dependencies (Puppeteer bundles a browser).
  if not DirectoryExists(helperDir + PathDelim + 'node_modules') then
  begin
    Report(AOnProgress, ACaptionInstalling);
    {$ifdef WINDOWS}
    // npm ships as npm.cmd on Windows, which CreateProcess won't run directly;
    // go through the command interpreter.
    npmExe := GetEnvironmentVariable('ComSpec');
    if npmExe = '' then
      npmExe := 'cmd.exe';
    npmParams := ['/c', 'npm', 'install', '--no-audit', '--no-fund'];
    {$else}
    npmExe := FindDefaultExecutablePath('npm');
    if npmExe = '' then
      npmExe := 'npm';
    npmParams := ['install', '--no-audit', '--no-fund'];
    {$endif}
    code := RunProc(npmExe, npmParams, helperDir, npmOut);
    if code <> 0 then
      Exit(wlrNpmFailed);
  end;

  // Run the login helper. It opens a browser and prints the token JSON on
  // stdout when the sign-in completes.
  Report(AOnProgress, ACaptionWaiting);
  if AScriptArgs <> '' then
    code := RunProc(nodeExe, [scriptPath, AScriptArgs], helperDir, ACred)
  else
    code := RunProc(nodeExe, [scriptPath], helperDir, ACred);

  ACred := Trim(ACred);
  if code = -1 then
    Exit(wlrLaunchFailed);
  if ACred = '' then
    Exit(wlrNoOutput);
  // The credential is always a JSON blob; a non-zero exit or non-JSON output
  // means the login didn't produce a usable token.
  if (code <> 0) or (ACred[1] <> '{') then
    Exit(wlrBadOutput);

  Result := wlrOK;
end;

end.
