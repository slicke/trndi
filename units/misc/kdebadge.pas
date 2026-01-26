unit KDEBadge;

{
  KDE/Plasma launcher badge & progress helper (Unity Launcher API over DBus).

  Features:
    - Safe payloads: always sends count/progress + their visibility with explicit types
    - Locale-agnostic: forces dot decimal for progress
    - Debounce: coalesces rapid updates (default 150 ms); set to 0 to disable
    - Change detection: avoids re-sending identical dicts
    - Optional logging: hook to capture gdbus stdout/stderr and actions
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
    - Runtime: `gdbus` in PATH (from GLib/DBus tools)
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
const GDBusAvailable: boolean = true);
procedure ShutdownBadge;

procedure SetDesktopId(const DesktopIdWithDotDesktop: string);
procedure SetDebounceMs(const Ms: cardinal);
procedure SetLogProc(const LogProc: TBadgeLogProc);

procedure SetBadge(const Value: double);
// count=int(Value), progress=Frac(Value)
procedure ShowOnlyCount(const Count: integer);      // progress hidden
procedure ShowOnlyProgress(const Progress: double); // count hidden
procedure ClearBadge;                               // hides both

// Advanced: send prebuilt dict safely through the same pipeline
procedure EmitRawDict(const Dict: string);

var
GDesktopId: string = '';

implementation

uses
Process, SyncObjs;

var
GDebounceMs: QWord = 150;
GLog: TBadgeLogProc = nil;
GDBusEnabled: Boolean = true;

type
TBadgeWorker = class(TThread)
private
  FLock: TRTLCriticalSection;
  FEvt: TEvent;
  FPending: boolean;
  FPendingDict: string;
  FLastRequestTick: QWord;
  FLastSentDict: string;
protected
  procedure Execute; override;
public
  constructor Create;
  destructor Destroy; override;
  procedure Submit(const Dict: string);
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

procedure EmitUnityLauncherUpdate(const DesktopIdWithDotDesktop, Dict: string);
var
  P: TProcess;
  OutStr, ErrStr: TStringStream;
begin
  if DesktopIdWithDotDesktop = '' then
  begin
    Log('Emit skipped: desktop id not set');
    Exit;
  end;

  if not GDBusEnabled then
  begin
    Log('Emit skipped: gdbus not available');
    Exit;
  end;

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
  FPendingDict := '';
  FLastRequestTick := 0;
  FLastSentDict := '';
  Resume;
end;

destructor TBadgeWorker.Destroy;
begin
  FEvt.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TBadgeWorker.Submit(const Dict: string);
begin
  EnterCriticalSection(FLock);
  try
    if Dict = FLastSentDict then
      Exit; // no change; skip
    FPendingDict := Dict;
    FPending := true;
    FLastRequestTick := GetTickCount64;
    FEvt.SetEvent;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TBadgeWorker.Flush;
var
  ToSend: string;
begin
  // Force send latest pending immediately
  EnterCriticalSection(FLock);
  try
    if not FPending then
      Exit;
    ToSend := FPendingDict;
    FPending := false;
  finally
    LeaveCriticalSection(FLock);
  end;

  if ToSend <> '' then
  try
    EmitUnityLauncherUpdate(GDesktopId, ToSend);
    EnterCriticalSection(FLock);
    try
      FLastSentDict := ToSend;
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
  ToSend: string;
  NowTick: QWord;
begin
  while not Terminated do
  begin
    // Wake up periodically or when new request arrives
    FEvt.WaitFor(50);

    ToSend := '';
    EnterCriticalSection(FLock);
    try
      if FPending then
      begin
        NowTick := GetTickCount64;
        if (GDebounceMs = 0) or (NowTick - FLastRequestTick >= GDebounceMs) then
        begin
          ToSend := FPendingDict;
          FPending := false;
        end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;

    if ToSend <> '' then
    try
      EmitUnityLauncherUpdate(GDesktopId, ToSend);
      EnterCriticalSection(FLock);
      try
        FLastSentDict := ToSend;
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
const GDBusAvailable: boolean);
begin
  GDesktopId := EnsureDotDesktop(DesktopIdWithDotDesktop);
  GDebounceMs := DebounceMs;
  GLog := LogProc;
  GDBusEnabled := GDBusAvailable;
  EnsureWorker;
  if GDBusEnabled then
    Log(Format('KDEBadge initialized (id=%s, debounce=%d ms)', [GDesktopId, DebounceMs]))
  else
    Log(Format('KDEBadge initialized but disabled (gdbus not available, id=%s)', [GDesktopId]));
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

procedure SubmitDict(const Dict: string);
begin
  if Dict = '' then
    Exit;
  if not GDBusEnabled then
    Exit;
  EnsureWorker;
  if GDebounceMs = 0 then
  try
    EmitUnityLauncherUpdate(GDesktopId, Dict);
  except
    on E: Exception do
      Log('Emit error (immediate): ' + E.Message);
  end// Immediate mode

  else
    GWorker.Submit(Dict);
end;

procedure SetBadge(const Value: double);
var
  Count: integer;
  FracPart: double;
  Dict: string;
begin
  if not GDBusEnabled then
    Exit;
  Count := ClampInt32(Trunc(Value));
  FracPart := Frac(Value);
  // Show count; progress only if > 0
  Dict := BuildDict(Count, true, FracPart, FracPart > 0.0);
  SubmitDict(Dict);
end;

procedure ShowOnlyCount(const Count: integer);
var
  Dict: string;
begin
  Dict := BuildDict(ClampInt32(Count), true, 0.0, false);
  SubmitDict(Dict);
end;

procedure ShowOnlyProgress(const Progress: double);
var
  P: double;
  Dict: string;
begin
  // Clamp to [0,1]
  if Progress < 0 then
    P := 0
  else
  if Progress > 1 then
    P := 1
  else
    P := Progress;

  Dict := BuildDict(0, false, P, true);
  SubmitDict(Dict);
end;

procedure ClearBadge;
var
  Dict: string;
begin
  if not GDBusEnabled then
    Exit;
  // Safe "clear": zero values and both visibility flags false
  Dict := BuildDict(0, false, 0.0, false);
  SubmitDict(Dict);
end;

procedure EmitRawDict(const Dict: string);
begin
  SubmitDict(Dict);
end;

initialization

finalization
ShutdownBadge;

end.
