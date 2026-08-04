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
unit Forms;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Controls, Menus, SysUtils, Graphics;

type
  TCloseAction = (caNone, caHide, caFree, caMinimize);

  // Minimal window/form enums used by umain
  TWindowState = (wsNormal, wsMinimized, wsMaximized, wsFullScreen);
  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsSizeToolWin, bsToolWindow);
  TFormStyle = (fsNormal, fsStayOnTop, fsMDIChild);

  // Minimal application properties type used on forms
  TApplicationProperties = class(TComponent)
  public
    ShowHint: Boolean;
    Title: string;
    MainFormOnTaskBar: Boolean;
  end;

  TShowInTaskBar = (stDefault, stNever, stAlways);

  TForm = class(TWinControl)
  private
    FShowing: Boolean;
    FMenu: TMainMenu;
    FBorderStyle: TFormBorderStyle;
    FCursor: Integer;
    FWindowState: TWindowState;
    FFormStyle: TFormStyle;
    FHandle: PtrUInt;
    FShowInTaskBar: TShowInTaskBar;
    FIcon: TIcon;
  public
    procedure Close; virtual;
    procedure DoClose(var CloseAction: TCloseAction); virtual;
    property Showing: Boolean read FShowing write FShowing;
    property Menu: TMainMenu read FMenu write FMenu;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle;
    property Cursor: Integer read FCursor write FCursor;
    property WindowState: TWindowState read FWindowState write FWindowState;
    property FormStyle: TFormStyle read FFormStyle write FFormStyle;
    property Handle: PtrUInt read FHandle write FHandle;
    property ShowInTaskBar: TShowInTaskBar read FShowInTaskBar write FShowInTaskBar;
    property Icon: TIcon read FIcon write FIcon;
    function HandleAllocated: Boolean; virtual;
    procedure Repaint; virtual;
  end;

  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;
  TDataEvent = procedure(Data: PtrInt) of object;

  TApplication = class(TComponent)
  private
    FExeName: string;
    FIcon: TIcon;
    FOnException: TExceptionEvent;
    FCursor: Integer;
    FTitle: string;
    FMainForm: TForm;
    FShowHint: Boolean;
    FMainFormOnTaskBar: Boolean;
    FShowMainForm: Boolean;
    FHandle: PtrUInt;
  public
    constructor Create(AOwner: TComponent = nil); virtual;
    property ExeName: string read FExeName write FExeName;
    property Icon: TIcon read FIcon write FIcon;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property Cursor: Integer read FCursor write FCursor;
    property Title: string read FTitle write FTitle;
    property MainForm: TForm read FMainForm write FMainForm;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property MainFormOnTaskBar: Boolean read FMainFormOnTaskBar write FMainFormOnTaskBar;
    property ShowMainForm: Boolean read FShowMainForm write FShowMainForm;
    property Handle: PtrUInt read FHandle write FHandle;
    procedure ProcessMessages; virtual;
    procedure Terminate; virtual;
    procedure BringToFront; virtual;
    procedure QueueAsyncCall(const AMethod: TDataEvent; Data: PtrInt); virtual;
    procedure RemoveAsyncCalls(const AnObject: TObject); virtual;
  end;

var
  Application: TApplication;

implementation

constructor TApplication.Create(AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  FIcon := TIcon.Create;
  FOnException := nil;
  FCursor := 0;
  FTitle := '';
  FMainForm := nil;
  FShowHint := False;
  FMainFormOnTaskBar := False;
  FShowMainForm := True;
end;

procedure TApplication.ProcessMessages;
begin
  // no-op in test environment
end;

procedure TApplication.Terminate;
begin
  // no-op in test environment
end;

procedure TApplication.BringToFront;
begin
  // no-op in test environment
end;

procedure TApplication.QueueAsyncCall(const AMethod: TDataEvent; Data: PtrInt);
begin
  // no-op in test environment — production code uses this to defer work to
  // the next message-loop iteration; tests run headless so we just drop it.
end;

procedure TApplication.RemoveAsyncCalls(const AnObject: TObject);
begin
  // no-op in test environment — QueueAsyncCall drops calls, so there is
  // never anything queued to remove.
end;

function TForm.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TForm.Close;
begin
  // no-op for headless
end;

procedure TForm.Repaint;
begin
  // no-op for headless tests
end;

procedure TForm.DoClose(var CloseAction: TCloseAction);
begin
  // no-op for headless
end;

initialization
  Application := TApplication.Create;
  Application.ExeName := ParamStr(0);

end.
