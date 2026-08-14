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
unit linutils.kdebadge;

{
  KDE/Plasma launcher badge & progress helper (Unity Launcher API over DBus).

  by Björn Lindh <github.com/slicke>; semi-vibe coded. License: LGPL3+.

  Features:
    - Safe payloads: always sends count/progress + their visibility with explicit types
    - Locale-agnostic: forces dot decimal for progress
    - Debounce: coalesces rapid updates (default 150 ms); set to 0 to disable
    - Change detection: avoids re-sending identical dicts
    - Optional logging: hook to capture transport output and actions
    - No LCL dependencies (threaded debouncer inside)

  Usage:
    1) In your .lpr/.pas (on startup):
         InitializeBadge('org.kde.yourapp.desktop', 150, @MyLogProc);
         ClearBadge; // optional: start clean

    2) During runtime:
         SetBadge(4.5);           // count=4, progress=0.5 shown
         ShowOnlyCount(7);        // show count only
         ShowOnlyProgress(0.33);  // show progress only
         ClearBadge;              // hide both

    3) On shutdown:
         ClearBadge;              // leave panel clean
         ShutdownBadge;

  Requires:
    - Runtime: libdbus-1 (loaded on demand via linutils.dbus), or `gdbus` in
      PATH as a fallback. The signal goes out per reading, so the direct bus
      path also spares a fork+exec every time the badge changes.
    - Plasma 5/6 with Unity Launcher bridge (standard in KDE)

  Notes:
    - Desktop ID must include ".desktop" and match the pinned launcher exactly.
    - If you must change desktop id at runtime: SetDesktopId('id.desktop').
}

{$mode objfpc}{$H+}

interface

uses
SysUtils, Classes;

type
TBadgeLogProc = procedure(const Msg: string);

procedure InitializeBadge(const DesktopIdWithDotDesktop: string;
const DebounceMs: cardinal = 150;
const LogProc: TBadgeLogProc = nil;
const BusAvailable: boolean = true);
procedure ShutdownBadge;

procedure SetDesktopId(const DesktopIdWithDotDesktop: string);
procedure SetDebounceMs(const Ms: cardinal);
procedure SetLogProc(const LogProc: TBadgeLogProc);

procedure SetBadge(const Value: double);
// count=int(Value), progress=Frac(Value)
procedure ShowOnlyCount(const Count: integer);      // progress hidden
procedure ShowOnlyProgress(const Progress: double); // count hidden
procedure ClearBadge;                               // hides both

// Advanced: send prebuilt dict safely through the same pipeline. Text only,
// so this goes out via gdbus even when libdbus is available.
procedure EmitRawDict(const Dict: string);

var
GDesktopId: string = '';

implementation

uses
Process, SyncObjs, linutils.dbus;

var
GDebounceMs: QWord = 150;
GLog: TBadgeLogProc = nil;
GBusEnabled: Boolean = true;

type
  // One badge state on its way to the panel. The values are carried as
  // themselves for the D-Bus path, and Text is their GVariant rendering -
  // what the gdbus fallback needs, and what change detection compares.
  // Typed is false for a caller-supplied raw dict, which only the fallback
  // can send.
TBadgeUpdate = record
  Text: string;
  Count: integer;
  CountVisible: boolean;
  Progress: double;
  ProgressVisible: boolean;
  Typed: boolean;
end;

TBadgeWorker = class(TThread)
private
  FLock: TRTLCriticalSection;
  FEvt: TEvent;
  FPending: boolean;
  FPendingUpdate: TBadgeUpdate;
  FLastRequestTick: QWord;
  FLastSentText: string;
protected
  procedure Execute; override;
public
  constructor Create;
  destructor Destroy; override;
  procedure Submit(const U: TBadgeUpdate);
  procedure Flush;
end;

var
GWorker: TBadgeWorker = nil;

function EnsureDotDesktop(const Id: string): string;
begin
  if Id = '' then
    Exit('');
  if (Length(Id) >= 8) and (CompareText(Copy(Id, Length(Id) - 7, 8), '.desktop') = 0) then
    Result := Id
  else
    Result := Id + '.desktop';
end;

procedure Log(const S: string);
begin
  if Assigned(GLog) then
    GLog(S);
end;

function DotFloat(const V: double; Digits: integer = 3): string;
var
  FS: TFormatSettings;
  Fmt: string;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  case Digits of
  0:
    Fmt := '0';
  1:
    Fmt := '0.0';
  2:
    Fmt := '0.00';
  3:
    Fmt := '0.000';
  4:
    Fmt := '0.0000';
  else
    Fmt := '0.###';
  end;
  Result := FormatFloat(Fmt, V, FS);
end;

function ClampInt32(const V: int64): integer;
begin
  if V > High(integer) then
    Exit(High(integer));
  if V < Low(integer) then
    Exit(Low(integer));
  Result := integer(V);
end;

function BuildDict(const Count: integer; const CountVisible: boolean;
const Progress: double; const ProgressVisible: boolean): string;
begin
  // Always include all four keys; explicit types for safety
  Result :=
    Format('{' + '''count'': <int32 %d>, ' +
           '''count-visible'': <%s>, ' + '''progress'': <%s>, ' +
           '''progress-visible'': <%s>' + '}',
    [Count, LowerCase(BoolToStr(CountVisible, true)),
    DotFloat(Progress), LowerCase(BoolToStr(ProgressVisible, true))]);
end;

function BuildUpdate(const Count: integer; const CountVisible: boolean;
const Progress: double; const ProgressVisible: boolean): TBadgeUpdate;
begin
  Result.Count := Count;
  Result.CountVisible := CountVisible;
  Result.Progress := Progress;
  Result.ProgressVisible := ProgressVisible;
  Result.Typed := true;
  Result.Text := BuildDict(Count, CountVisible, Progress, ProgressVisible);
end;

// Which transport an emit would take right now; for the init log line.
function BadgeTransportName: string;
begin
  if DBusAvailable then
    Result := 'libdbus'
  else
    Result := 'gdbus';
end;

{------------------------------------------------------------------------------
  EmitViaDBus
  -----------
  Broadcast the Update signal straight onto the session bus. Returns False for
  a raw dict (nothing to marshal from) or when libdbus is unavailable, leaving
  the caller to shell out instead.
 ------------------------------------------------------------------------------}
function EmitViaDBus(const DesktopIdWithDotDesktop: string;
const U: TBadgeUpdate): boolean;
var
  conn: TDBusConn;
  msg: TDBusMessage;
begin
  Result := false;
  if (not U.Typed) or (not DBusAvailable) then
    Exit;
  conn := TDBusConn.Create(dbSession);
  try
    msg := conn.NewSignal('/com/canonical/Unity/LauncherEntry',
      'com.canonical.Unity.LauncherEntry', 'Update');
    if msg = nil then
      Exit;
    msg.AddString('application://' + DesktopIdWithDotDesktop);
    msg.OpenDict;
    // int32 count, matching what the gdbus path has always sent - Plasma
    // takes either width, but there is no reason to change the wire now.
    msg.DictAddInt32('count', U.Count);
    msg.DictAddBool('count-visible', U.CountVisible);
    msg.DictAddDouble('progress', U.Progress);
    msg.DictAddBool('progress-visible', U.ProgressVisible);
    msg.CloseDict;
    Result := conn.Send(msg);
  finally
    conn.Free;
  end;
end;

procedure EmitUnityLauncherUpdate(const DesktopIdWithDotDesktop: string;
const U: TBadgeUpdate);
var
  P: TProcess;
  OutStr, ErrStr: TStringStream;
  Dict: string;
begin
  if DesktopIdWithDotDesktop = '' then
  begin
    Log('Emit skipped: desktop id not set');
    Exit;
  end;

  if not GBusEnabled then
  begin
    Log('Emit skipped: no D-Bus transport');
    Exit;
  end;

  if EmitViaDBus(DesktopIdWithDotDesktop, U) then
    Exit;

  Dict := U.Text;
  P := TProcess.Create(nil);
  OutStr := TStringStream.Create('');
  ErrStr := TStringStream.Create('');
  try
    P.Executable := 'gdbus';
    P.Parameters.Add('emit');
    P.Parameters.Add('--session');
    P.Parameters.Add('--object-path');
    P.Parameters.Add('/com/canonical/Unity/LauncherEntry');
    P.Parameters.Add('--signal');
    P.Parameters.Add('com.canonical.Unity.LauncherEntry.Update');
    P.Parameters.Add('application://' + DesktopIdWithDotDesktop);
    P.Parameters.Add(Dict);
    P.Options := [poWaitOnExit, poUsePipes];
    P.ShowWindow := swoHIDE;

    P.Execute;

    OutStr.CopyFrom(P.Output, P.Output.NumBytesAvailable);
    ErrStr.CopyFrom(P.Stderr, P.Stderr.NumBytesAvailable);

    if Assigned(GLog) then
    begin
      if OutStr.DataString <> '' then
        Log('gdbus stdout: ' + OutStr.DataString);
      if ErrStr.DataString <> '' then
        Log('gdbus stderr: ' + ErrStr.DataString);
    end;

    if P.ExitStatus <> 0 then
      raise Exception.CreateFmt('gdbus failed (exit %d). Dict=%s', [P.ExitStatus, Dict]);
  finally
    ErrStr.Free;
    OutStr.Free;
    P.Free;
  end;
end;

{ TBadgeWorker }

constructor TBadgeWorker.Create;
begin
  inherited Create(true);
  InitCriticalSection(FLock);
  FEvt := TEvent.Create(nil, false, false, '');
  FreeOnTerminate := false;
  FPending := false;
  FPendingUpdate := Default(TBadgeUpdate);
  FLastRequestTick := 0;
  FLastSentText := '';
  Resume;
end;

destructor TBadgeWorker.Destroy;
begin
  FEvt.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TBadgeWorker.Submit(const U: TBadgeUpdate);
begin
  EnterCriticalSection(FLock);
  try
    if U.Text = FLastSentText then
      Exit; // no change; skip
    FPendingUpdate := U;
    FPending := true;
    FLastRequestTick := GetTickCount64;
    FEvt.SetEvent;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TBadgeWorker.Flush;
var
  ToSend: TBadgeUpdate;
begin
  // Force send latest pending immediately
  EnterCriticalSection(FLock);
  try
    if not FPending then
      Exit;
    ToSend := FPendingUpdate;
    FPending := false;
  finally
    LeaveCriticalSection(FLock);
  end;

  if ToSend.Text <> '' then
  try
    EmitUnityLauncherUpdate(GDesktopId, ToSend);
    EnterCriticalSection(FLock);
    try
      FLastSentText := ToSend.Text;
    finally
      LeaveCriticalSection(FLock);
    end;
  except
    on E: Exception do
      Log('Emit error (flush): ' + E.Message);
  end;
end;

procedure TBadgeWorker.Execute;
var
  ToSend: TBadgeUpdate;
  NowTick: QWord;
begin
  while not Terminated do
  begin
    // Wake up periodically or when new request arrives
    FEvt.WaitFor(50);

    ToSend := Default(TBadgeUpdate);
    EnterCriticalSection(FLock);
    try
      if FPending then
      begin
        NowTick := GetTickCount64;
        if (GDebounceMs = 0) or (NowTick - FLastRequestTick >= GDebounceMs) then
        begin
          ToSend := FPendingUpdate;
          FPending := false;
        end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;

    if ToSend.Text <> '' then
    try
      EmitUnityLauncherUpdate(GDesktopId, ToSend);
      EnterCriticalSection(FLock);
      try
        FLastSentText := ToSend.Text;
      finally
        LeaveCriticalSection(FLock);
      end;
    except
      on E: Exception do
        Log('Emit error: ' + E.Message);
    end;
  end;
end;

procedure EnsureWorker;
begin
  if (GWorker = nil) then
    GWorker := TBadgeWorker.Create;
end;

procedure InitializeBadge(const DesktopIdWithDotDesktop: string;
const DebounceMs: cardinal;
const LogProc: TBadgeLogProc;
const BusAvailable: boolean);
begin
  GDesktopId := EnsureDotDesktop(DesktopIdWithDotDesktop);
  GDebounceMs := DebounceMs;
  GLog := LogProc;
  GBusEnabled := BusAvailable;
  EnsureWorker;
  if GBusEnabled then
    Log(Format('KDEBadge initialized (id=%s, debounce=%d ms, transport=%s)',
      [GDesktopId, DebounceMs, BadgeTransportName]))
  else
    Log(Format('KDEBadge initialized but disabled (no D-Bus, id=%s)', [GDesktopId]));
end;

procedure ShutdownBadge;
begin
  if Assigned(GWorker) then
  begin
    Log('KDEBadge shutting down...');
    GWorker.Terminate;
    // Force flush any pending send before exit
    GWorker.Flush;
    GWorker.WaitFor;
    FreeAndNil(GWorker);
  end;
end;

procedure SetDesktopId(const DesktopIdWithDotDesktop: string);
begin
  GDesktopId := EnsureDotDesktop(DesktopIdWithDotDesktop);
  Log('DesktopId set to ' + GDesktopId);
end;

procedure SetDebounceMs(const Ms: cardinal);
begin
  GDebounceMs := Ms;
  Log(Format('Debounce set to %d ms', [Ms]));
end;

procedure SetLogProc(const LogProc: TBadgeLogProc);
begin
  GLog := LogProc;
end;

procedure SubmitUpdate(const U: TBadgeUpdate);
begin
  if U.Text = '' then
    Exit;
  if not GBusEnabled then
    Exit;
  EnsureWorker;
  if GDebounceMs = 0 then
  try
    EmitUnityLauncherUpdate(GDesktopId, U);
  except
    on E: Exception do
      Log('Emit error (immediate): ' + E.Message);
  end// Immediate mode

  else
    GWorker.Submit(U);
end;

procedure SetBadge(const Value: double);
var
  Count: integer;
  FracPart: double;
begin
  if not GBusEnabled then
    Exit;
  Count := ClampInt32(Trunc(Value));
  FracPart := Frac(Value);
  // Show count; progress only if > 0
  SubmitUpdate(BuildUpdate(Count, true, FracPart, FracPart > 0.0));
end;

procedure ShowOnlyCount(const Count: integer);
begin
  SubmitUpdate(BuildUpdate(ClampInt32(Count), true, 0.0, false));
end;

procedure ShowOnlyProgress(const Progress: double);
var
  P: double;
begin
  // Clamp to [0,1]
  if Progress < 0 then
    P := 0
  else
  if Progress > 1 then
    P := 1
  else
    P := Progress;

  SubmitUpdate(BuildUpdate(0, false, P, true));
end;

procedure ClearBadge;
begin
  if not GBusEnabled then
    Exit;
  // Safe "clear": zero values and both visibility flags false
  SubmitUpdate(BuildUpdate(0, false, 0.0, false));
end;

procedure EmitRawDict(const Dict: string);
var
  U: TBadgeUpdate;
begin
  // Caller-supplied GVariant text: nothing to marshal, so this one can only
  // go out through gdbus.
  U := Default(TBadgeUpdate);
  U.Text := Dict;
  U.Typed := false;
  SubmitUpdate(U);
end;

initialization

finalization
ShutdownBadge;

end.
