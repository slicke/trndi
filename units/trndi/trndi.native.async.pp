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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-08-17: Removed the unused RequestExAsync/RequestExWait wrappers and
 *   TRequestExWorker (every caller uses TTrndiNativeBase.RequestExWait
 *   instead). TRunAndCaptureWorker now drains and discards the child's
 *   stderr pipe and gates stdout reads on NumBytesAvailable, so a
 *   stderr-chatty or silent child can no longer wedge the worker.
 *)
unit trndi.native.async;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, SyncObjs, process, trndi.native.base;

type
  {
  TRunAndCaptureCallback:
    Callback invoked with captured stdout and an exit code. Called on the
    worker thread. On timeout the ExitCode will be -1 and Stdout contains
    any data captured until termination.
  }
TRunAndCaptureCallback = procedure(const OutS: string; ExitCode: integer) of object;

TRunAndCaptureWorker = class(TThread)
private
  FExec: string;
  FParams: TStringList;
  FCallback: TRunAndCaptureCallback;
  FStdoutS: string;
  FExitCode: integer;
  FDone: TEvent;
  FTerminatedByCaller: boolean;
protected
  procedure Execute; override;
public
  constructor Create(const AExec: string; const AParams: array of string;
    ACallback: TRunAndCaptureCallback);
  destructor Destroy; override;
  property StdoutS: string read FStdoutS;
  property ExitCode: integer read FExitCode;
end;

function RunAndCaptureSimpleAsync(const Exec: string;
const Params: array of string; onFinish: TRunAndCaptureCallback): TThread;

function RunAndCaptureSimpleWait(const Exec: string; const Params: array of string;
out StdoutS: string; out ExitCode: integer; TimeoutMs: cardinal = 2000): boolean;

implementation

function WaitForCaptureWorkerTermination(worker: TRunAndCaptureWorker; TimeoutMs: cardinal): boolean;
var
  deadline: QWord;
begin
  deadline := GetTickCount64 + TimeoutMs;
  Result := worker.FDone.WaitFor(50) = wrSignaled;
  while (not Result) and (GetTickCount64 < deadline) do
  begin
    Sleep(10);
    Result := worker.FDone.WaitFor(50) = wrSignaled;
  end;
end;

constructor TRunAndCaptureWorker.Create(const AExec: string;
const AParams: array of string; ACallback: TRunAndCaptureCallback);
var
  i: integer;
begin
  inherited Create(true);
  FreeOnTerminate := false; // caller will free after wait
  FExec := AExec;
  FParams := TStringList.Create;
  for i := Low(AParams) to High(AParams) do
    FParams.Add(AParams[i]);
  FCallback := ACallback;
  FDone := TEvent.Create(nil, true, false, '');
end;

destructor TRunAndCaptureWorker.Destroy;
begin
  FParams.Free;
  FDone.Free;
  inherited Destroy;
end;

procedure TRunAndCaptureWorker.Execute;
var
  Proc: TProcess;
  buf: array[0..4095] of byte;
  n: integer;
  waitLoops: integer;
  ms: TMemoryStream;
begin
  FStdoutS := '';
  FExitCode := -1;
  Proc := TProcess.Create(nil);
  ms := TMemoryStream.Create;
  try
    Proc.Executable := FExec;
    Proc.Options := Proc.Options + [poUsePipes];
    Proc.ShowWindow := swoHide;
    Proc.Parameters.Assign(FParams);
    Proc.Execute;
    // Accumulate stdout in a stream — string concatenation in this loop is
    // O(n^2) in total bytes copied, which becomes painful for large outputs.
    // Reads are gated on NumBytesAvailable: an unconditional Read blocks on
    // an empty pipe, which both made Terminate unnoticeable while a silent
    // child ran and could hang a SafeThreadJoin caller behind it.
    while (Proc.Running or (Proc.Output.NumBytesAvailable > 0)) and (not Terminated) do
    begin
      // Drain and discard stderr: nobody consumes it, and poUsePipes created
      // the pipe — a child that fills it (~64 KB) would block on the write
      // forever, with no timeout on the fire-and-forget paths.
      while Proc.Stderr.NumBytesAvailable > 0 do
        if Proc.Stderr.Read(buf, SizeOf(buf)) <= 0 then
          Break;
      if Proc.Output.NumBytesAvailable > 0 then
      begin
        n := Proc.Output.Read(buf, SizeOf(buf));
        if n > 0 then
          ms.WriteBuffer(buf, n);
      end
      else
        Sleep(5);
    end;
    // If termination requested while process running, stop child process
    // before finishing so timeout callers do not leave it behind.
    if Terminated and Proc.Running then
    begin
      try
        {$IF DEFINED(UNIX)}
        Proc.Terminate(15); // SIGTERM on Unix/Linux
        {$ELSE}
        Proc.Terminate(1);  // Windows: argument is the forced exit code
        {$ENDIF}
      except end;
      // Give the process a short grace period to exit.
      waitLoops := 0;
      while Proc.Running and (waitLoops < 100) do
      begin
        Sleep(10);
        Inc(waitLoops);
      end;

      // Escalate if still running after grace period.
      if Proc.Running then
      begin
        try
          {$IF DEFINED(UNIX)}
          Proc.Terminate(9); // SIGKILL on Unix/Linux
          {$ELSE}
          Proc.Terminate(1);  // best effort on Windows
          {$ENDIF}
        except end;

        waitLoops := 0;
        while Proc.Running and (waitLoops < 300) do
        begin
          Sleep(10);
          Inc(waitLoops);
        end;
      end;
    end;

    // capture exit status if available
    if Proc.Running then
      FExitCode := -1
    else
    try
      FExitCode := Proc.ExitStatus;
    except
      FExitCode := -1;
    end;
    if ms.Size > 0 then
      SetString(FStdoutS, PAnsiChar(ms.Memory), ms.Size);
    if Assigned(FCallback) then
      FCallback(FStdoutS, FExitCode);
  finally
    ms.Free;
    Proc.Free;
    FDone.SetEvent;
  end;
end;

function RunAndCaptureSimpleAsync(const Exec: string;
const Params: array of string; onFinish: TRunAndCaptureCallback): TThread;
begin
  Result := TRunAndCaptureWorker.Create(Exec, Params, onFinish);
  Result.FreeOnTerminate := true;
  Result.Start;
end;

function RunAndCaptureSimpleWait(const Exec: string; const Params: array of string;
out StdoutS: string; out ExitCode: integer; TimeoutMs: cardinal = 2000): boolean;
var
  worker: TRunAndCaptureWorker;
begin
  worker := TRunAndCaptureWorker.Create(Exec, Params, nil);
  worker.Start;
  if worker.FDone.WaitFor(TimeoutMs) = wrSignaled then
  begin
    StdoutS := worker.StdoutS;
    ExitCode := worker.ExitCode;
    Result := ExitCode = 0;
    SafeThreadJoin(worker);
    worker.Free;
  end
  else
  try
    worker.Terminate;
    if not WaitForCaptureWorkerTermination(worker, TimeoutMs) then
    begin
      StdoutS := '';
      ExitCode := -1;
      Result := false;
    end
    else
    begin
      StdoutS := worker.StdoutS;
      ExitCode := worker.ExitCode;
      Result := ExitCode = 0;
    end;
    SafeThreadJoin(worker);
    worker.Free;
  except
    StdoutS := '';
    ExitCode := -1;
    Result := false;
  end// timeout: request termination and wait for worker cleanup completion
  ;
end;

end.
