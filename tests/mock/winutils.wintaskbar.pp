unit winutils.wintaskbar;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Windows;

const
  // Taskbar Progress States (kept for compatibility with real unit)
  TBPF_NOPROGRESS     = $0000;
  TBPF_INDETERMINATE  = $0001;
  TBPF_NORMAL         = $0002;
  TBPF_ERROR          = $0004;
  TBPF_PAUSED         = $0008;

type
  // Progress style enum (must match real unit)
  TTaskBarProgressStyle = (tbpsNone, tbpsIndeterminate, tbpsNormal, tbpsError, tbpsPaused);

  // Minimal test double for TWinTaskbar used by production code.
  TWinTaskbar = class
  private
    FInitialized: Boolean;
    FWindowHandle: HWND;
    FLastError: string;
  public
    constructor Create(WindowHandle: HWND = 0);
    destructor Destroy; override;

    function SetProgressValue(Current, Max: UInt64): Boolean;
    function SetProgressState(ProgressState: TTaskBarProgressStyle): Boolean;
    function SetBadge(const Text: string): Boolean;
    function ClearBadge: Boolean;

    property Initialized: Boolean read FInitialized;
    property LastError: string read FLastError;
    property WindowHandle: HWND read FWindowHandle;
  end;

var
  GlobalTaskbar: TWinTaskbar;

implementation

{ TWinTaskbar - test stub }

constructor TWinTaskbar.Create(WindowHandle: HWND = 0);
begin
  inherited Create;
  FInitialized := True;            // pretend taskbar is available in tests
  FWindowHandle := WindowHandle;   // allow tests to inspect/set if needed
  FLastError := '';
end;

destructor TWinTaskbar.Destroy;
begin
  FInitialized := False;
  inherited Destroy;
end;

function TWinTaskbar.SetProgressValue(Current, Max: UInt64): Boolean;
begin
  // No-op successful stub
  Result := FInitialized;
  if not Result then
    FLastError := 'Not initialized (mock)';
end;

function TWinTaskbar.SetProgressState(ProgressState: TTaskBarProgressStyle): Boolean;
begin
  Result := FInitialized;
  if not Result then
    FLastError := 'Not initialized (mock)';
end;

function TWinTaskbar.SetBadge(const Text: string): Boolean;
begin
  Result := FInitialized;
  if not Result then
    FLastError := 'Not initialized (mock)';
end;

function TWinTaskbar.ClearBadge: Boolean;
begin
  Result := FInitialized;
  if not Result then
    FLastError := 'Not initialized (mock)';
end;

initialization
  GlobalTaskbar := nil;

finalization
  if Assigned(GlobalTaskbar) then
    FreeAndNil(GlobalTaskbar);

end.
