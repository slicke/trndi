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
  Classes, SysUtils, StdCtrls, Forms, trndi.api;

type
  {** Progress callback: invoked with a short status caption as the login
      proceeds (e.g. to update a button caption). May be nil. }
  TWebLoginProgress = procedure(const ACaption: string) of object;

  {** Localized texts for RunAssistedWebLogin. Both callers keep their own
      translated resourcestrings and fill this record from them, so the flow
      stays identical while the translation keys stay stable. }
  TWebLoginTexts = record
    RunTitle: string;      // Confirm + success dialog title
    RunPrompt: string;     // Pre-flight yes/no prompt
    Installing: string;    // Progress caption while `npm install` runs
    Waiting: string;       // Progress caption while awaiting the browser login
    CapturedOK: string;    // Success message
    FailTitle: string;     // Error dialog title
    NoScript: string;      // TWebLoginResult failure texts...
    NoNode: string;
    NpmFailed: string;
    NoOutput: string;
    BadOutput: string;
    HelpTitle: string;     // Manual instructions dialog title
    HelpBody: string;      // Manual instructions; Format args: helper folder, command
  end;

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

    The helper is extracted fresh (via @code(ACls.WriteAssets)) into a writable
    folder under Trndi's settings directory on every run, so the copy always
    matches this build and the folder next to the executable need not be
    writable. Backends without embedded assets fall back to the exe-relative
    helper named by @code(AScriptRel).

    @param(ACls        Backend class, used to extract its embedded helper assets)
    @param(AScriptRel  Helper path relative to the executable directory, using
                       forward slashes, as returned by TrndiAPI.webLoginScript;
                       its folder name and file name drive extraction)
    @param(AScriptArgs Extra command-line argument for the script, e.g. a region
                       flag like "--us"; empty for none)
    @param(ACaptionInstalling Progress caption reported while `npm install` runs)
    @param(ACaptionWaiting    Progress caption reported while awaiting the login)
    @param(AOnProgress Optional progress callback; may be nil)
    @param(ACred       Receives the captured credential (JSON) on success)
    @returns(@code(wlrOK) on success; otherwise a failure code) }
function RunNodeLoginHelper(ACls: TrndiAPIClass;
  const AScriptRel, AScriptArgs: string;
  const ACaptionInstalling, ACaptionWaiting: string;
  const AOnProgress: TWebLoginProgress;
  out ACred: string): TWebLoginResult;

{** Complete assisted-login flow shared by the settings form and the first-run
    wizard: confirm with the user, run the backend's Node helper (reporting
    progress on ALoginButton), and on success drop the captured credential into
    APassEdit, revealed so the user can eyeball it before testing. On failure
    the error is explained and the manual helper instructions are offered.
    AIdleCaption is restored on the button afterwards; ASender centers the
    dialogs. }
procedure RunAssistedWebLogin(ACls: TrndiAPIClass; ALoginButton: TButton;
  APassEdit: TEdit; const AIdleCaption: string; const T: TWebLoginTexts;
  ASender: TForm);

implementation

uses
  process, pipes, FileUtil, lclintf, math, Controls, slicke.ux.alert;

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
  progress there). When APathEnv is non-empty the child runs with that PATH
  instead of ours (needed on macOS, where the child re-resolves node via PATH).
  Returns the exit code, or -1 if it could not be started. }
function RunProc(const AExe: string; const AParams: array of string;
  const AWorkDir: string; out AStdOut: string;
  const APathEnv: string = ''): integer;
var
  proc: TProcess;
  i: integer;
  started: QWord;
  envLine: string;
  havePath: boolean;
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
    if APathEnv <> '' then
    begin
      // An empty TProcess.Environment inherits ours; a non-empty one replaces
      // it wholesale, so copy everything and swap in the requested PATH.
      havePath := false;
      for i := 1 to GetEnvironmentVariableCount do
      begin
        envLine := GetEnvironmentString(i);
        if Copy(envLine, 1, 5) = 'PATH=' then
        begin
          proc.Environment.Add('PATH=' + APathEnv);
          havePath := true;
        end
        else
          proc.Environment.Add(envLine);
      end;
      if not havePath then
        proc.Environment.Add('PATH=' + APathEnv);
    end;
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

{$ifdef DARWIN}
{ Apps launched from Finder/Dock (as opposed to a terminal) inherit launchd's
  minimal PATH (/usr/bin:/bin:/usr/sbin:/sbin), which omits Homebrew
  (/opt/homebrew/bin, /usr/local/bin) and version managers like nvm/fnm/volta.
  Ask the user's login shell for its real PATH so `node`/`npm` installed
  there can still be found. Markers guard against MOTD/rc-file chatter on
  stdout; RunProc is declared above us in this unit. }
function MacLoginShellPath: string;
const
  MARK_LO = '__TRNDI_PATH_LO__';
  MARK_HI = '__TRNDI_PATH_HI__';
var
  shellExe, shellOut: string;
  code, p1, p2: integer;
begin
  Result := '';
  shellExe := GetEnvironmentVariable('SHELL');
  if shellExe = '' then
    shellExe := '/bin/zsh';
  code := RunProc(shellExe,
    ['-ilc', 'echo ' + MARK_LO + '"$PATH"' + MARK_HI], '', shellOut);
  if code <> 0 then
    Exit;
  p1 := Pos(MARK_LO, shellOut);
  p2 := Pos(MARK_HI, shellOut);
  if (p1 > 0) and (p2 > p1) then
    Result := Copy(shellOut, p1 + Length(MARK_LO), p2 - (p1 + Length(MARK_LO)));
end;

{ Search a ':'-delimited path list (as returned by MacLoginShellPath) for a
  named executable. }
function FindOnPathList(const AName, APathList: string): string;
var
  dirs: TStringList;
  i: integer;
  cand: string;
begin
  Result := '';
  if APathList = '' then
    Exit;
  dirs := TStringList.Create;
  try
    dirs.Delimiter := ':';
    dirs.StrictDelimiter := true;
    dirs.DelimitedText := APathList;
    for i := 0 to dirs.Count - 1 do
    begin
      cand := IncludeTrailingPathDelimiter(dirs[i]) + AName;
      if FileExists(cand) then
        Exit(cand);
    end;
  finally
    dirs.Free;
  end;
end;
{$endif}

{ Writable per-backend folder under Trndi's settings dir where the assisted-
  login helper is extracted, named after the helper script's own folder (e.g.
  tools/carelink-login/... -> <config>/carelink-login). Kept separate from the
  read-only bundle next to the executable, which may live in Program Files, a
  signed .app bundle, or a read-only AppImage mount. }
function WebLoginAssetDir(const AScriptRel: string): string;
var
  rel, folderName: string;
begin
  rel := StringReplace(AScriptRel, '/', PathDelim, [rfReplaceAll]);
  folderName := ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(rel)));
  if folderName = '' then
    folderName := 'weblogin';
  Result := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + folderName;
end;

function RunNodeLoginHelper(ACls: TrndiAPIClass;
  const AScriptRel, AScriptArgs: string;
  const ACaptionInstalling, ACaptionWaiting: string;
  const AOnProgress: TWebLoginProgress;
  out ACred: string): TWebLoginResult;
var
  scriptName, scriptPath, helperDir, nodeExe, npmExe, npmOut, childPath: string;
  npmParams: array of string;
  code: integer;
  usedAssets: boolean;
  {$ifdef DARWIN}
  macPath: string;
  {$endif}
begin
  ACred := '';

  if AScriptRel = '' then
    Exit(wlrNoScript);

  scriptName := ExtractFileName(StringReplace(AScriptRel, '/', PathDelim, [rfReplaceAll]));

  // Prefer a fresh copy the backend writes into a writable settings folder, so
  // the helper always matches this build and the exe-relative folder need not
  // be writable. Fall back to the bundled copy when there are no embedded
  // assets.
  usedAssets := false;
  if Assigned(ACls) then
  begin
    helperDir := IncludeTrailingPathDelimiter(WebLoginAssetDir(AScriptRel));
    usedAssets := ACls.WriteAssets(helperDir);
  end;

  if not usedAssets then
  begin
    // Resolve the helper next to the executable (forward slashes in the
    // relative path become the platform separator).
    helperDir := ExtractFilePath(ExtractFilePath(Application.ExeName) +
      StringReplace(AScriptRel, '/', PathDelim, [rfReplaceAll]));
  end;
  scriptPath := helperDir + scriptName;
  if not FileExists(scriptPath) then
    Exit(wlrNoScript);

  nodeExe := FindDefaultExecutablePath('node');
  {$ifdef DARWIN}
  if nodeExe = '' then
  begin
    macPath := MacLoginShellPath;
    nodeExe := FindOnPathList('node', macPath);
  end;
  {$endif}
  if nodeExe = '' then
    Exit(wlrNoNode);

  // Install dependencies every run (Puppeteer bundles a browser). WriteAssets
  // may have just refreshed package.json for a newer helper, and a stale
  // node_modules would otherwise mask the dependency change; npm is a fast
  // no-op when everything is already up to date.
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
  {$ifdef DARWIN}
  // npm ships alongside node; try that directory before falling back to a
  // fresh login-shell PATH lookup (Homebrew/nvm/etc. absent from launchd's).
  if (npmExe = '') and
     FileExists(ExtractFilePath(nodeExe) + 'npm') then
    npmExe := ExtractFilePath(nodeExe) + 'npm';
  if npmExe = '' then
  begin
    if macPath = '' then
      macPath := MacLoginShellPath;
    npmExe := FindOnPathList('npm', macPath);
  end;
  {$endif}
  if npmExe = '' then
    npmExe := 'npm';
  npmParams := ['install', '--no-audit', '--no-fund'];
  {$endif}
  childPath := '';
  {$ifdef DARWIN}
  // npm is itself a Node script behind a `#!/usr/bin/env node` shebang, and
  // it spawns node again for lifecycle scripts — so the child re-resolves
  // node via PATH, and the launchd PATH that failed above would fail again
  // inside npm. Run the children with the login shell's PATH instead, with
  // node's own directory in front.
  if macPath <> '' then
    childPath := ExcludeTrailingPathDelimiter(ExtractFilePath(nodeExe)) +
      ':' + macPath;
  {$endif}
  code := RunProc(npmExe, npmParams, helperDir, npmOut, childPath);
  if code <> 0 then
    Exit(wlrNpmFailed);

  // Run the login helper. It opens a browser and prints the token JSON on
  // stdout when the sign-in completes.
  Report(AOnProgress, ACaptionWaiting);
  if AScriptArgs <> '' then
    code := RunProc(nodeExe, [scriptPath, AScriptArgs], helperDir, ACred, childPath)
  else
    code := RunProc(nodeExe, [scriptPath], helperDir, ACred, childPath);

  // The credential must be a single line on every platform: the Linux/BSD
  // settings store is an ini file (values cannot span lines) and the target
  // edit field is single-line. Raw line breaks in JSON only occur between
  // tokens, so stripping them never alters the payload.
  ACred := StringReplace(ACred, #13, '', [rfReplaceAll]);
  ACred := StringReplace(ACred, #10, '', [rfReplaceAll]);
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

type
  { Adapter so the flow can feed helper progress into the login button; the
    TWebLoginProgress callback type is "of object". }
  TButtonProgressAdapter = class
    btn: TButton;
    procedure Report(const ACaption: string);
  end;

procedure TButtonProgressAdapter.Report(const ACaption: string);
begin
  btn.Caption := ACaption;
  Application.ProcessMessages;
end;

{ Point the user at the backend's login helper (e.g. tools/carelink-login),
  which captures the token in a real browser and prints it for pasting into
  the credential field. Optionally opens the helper folder. Used as the
  fallback when the automatic Node login isn't available. }
procedure ShowManualWebLoginHelp(ACls: TrndiAPIClass; const T: TWebLoginTexts);
var
  scriptRel, args, helperDir, cmd, scriptName: string;
begin
  args := '';
  if Assigned(ACls) then
    scriptRel := ACls.webLoginScript(args)
  else
    scriptRel := '';

  if scriptRel <> '' then
  begin
    scriptName := ExtractFileName(StringReplace(scriptRel, '/', PathDelim, [rfReplaceAll]));
    // Drop a fresh copy of the helper into the writable settings folder so the
    // manual `npm install` + run steps work from there; if the backend has no
    // embedded assets, point at the bundle next to the executable instead.
    helperDir := IncludeTrailingPathDelimiter(WebLoginAssetDir(scriptRel));
    if not (Assigned(ACls) and ACls.WriteAssets(helperDir)) then
      helperDir := ExtractFileDir(ExtractFilePath(Application.ExeName) +
        StringReplace(scriptRel, '/', PathDelim, [rfReplaceAll]));
    cmd := 'npm install && node ' + scriptName;
    if args <> '' then
      cmd := cmd + ' ' + args;
  end
  else
  begin
    // No helper shipped for this backend; point at the bundled CareLink one.
    helperDir := ExtractFilePath(Application.ExeName) + 'tools' + PathDelim + 'carelink-login';
    cmd := 'npm install && node carelink-login.mjs';
  end;

  if SlickeMsgYesNo(T.HelpTitle, Format(T.HelpBody, [helperDir, cmd]), uxmtInformation, 20) then
  begin
    if DirectoryExists(helperDir) then
      OpenDocument(helperDir)
    else
      OpenURL('https://github.com/slicke/trndi/tree/main/tools/carelink-login');
  end;
end;

procedure RunAssistedWebLogin(ACls: TrndiAPIClass; ALoginButton: TButton;
  APassEdit: TEdit; const AIdleCaption: string; const T: TWebLoginTexts;
  ASender: TForm);

  function ErrText(r: TWebLoginResult): string;
  begin
    case r of
    wlrNoScript:  Result := T.NoScript;
    wlrNoNode:    Result := T.NoNode;
    wlrNpmFailed: Result := T.NpmFailed;
    wlrBadOutput: Result := T.BadOutput;
    else          Result := T.NoOutput; // launch failed / no output
    end;
  end;

var
  cred, scriptArgs, scriptRel: string;
  res: TWebLoginResult;
  progress: TButtonProgressAdapter;
begin
  scriptArgs := '';
  if Assigned(ACls) then
    scriptRel := ACls.webLoginScript(scriptArgs)
  else
    scriptRel := '';

  // No runnable helper for this backend — keep the manual instructions.
  if scriptRel = '' then
  begin
    ShowManualWebLoginHelp(ACls, T);
    Exit;
  end;

  // Heads-up before we open a browser and (on first run) install dependencies.
  if not SlickeMsgYesNo(T.RunTitle, T.RunPrompt, uxmtInformation, 20) then
    Exit;

  progress := TButtonProgressAdapter.Create;
  Screen.Cursor := crHourGlass;
  ALoginButton.Enabled := false;
  try
    progress.btn := ALoginButton;
    res := RunNodeLoginHelper(ACls, scriptRel, scriptArgs,
      T.Installing, T.Waiting, @progress.Report, cred);
  finally
    ALoginButton.Caption := AIdleCaption;
    ALoginButton.Enabled := true;
    Screen.Cursor := crDefault;
    progress.Free;
  end;

  if res = wlrOK then
  begin
    APassEdit.Text := cred;
    // Reveal what we captured so the user can eyeball it before testing.
    APassEdit.EchoMode := emNormal;
    APassEdit.PasswordChar := #0;
    SlickeMessage(T.RunTitle, T.CapturedOK, uxmtOK, ASender);
    Exit;
  end;

  // Automatic path unavailable/failed — explain, then offer the manual route.
  ExtError(sdsAuto, T.FailTitle, ErrText(res), uxmtWarning);
  ShowManualWebLoginHelp(ACls, T);
end;

end.
