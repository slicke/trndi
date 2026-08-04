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
