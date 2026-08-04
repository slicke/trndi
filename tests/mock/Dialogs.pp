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
unit Dialogs;

{$mode ObjFPC}{$H+}

interface

uses Classes;

const
  mtWarning = 2;
  mtInformation = 1;
  mtError = 3;

  // Button constants (mb*) used by UX dialogs
  mbOK = 1;
  mbCancel = 2;
  mbAbort = 3;
  mbRetry = 4;
  mbIgnore = 5;
  mbYes = 6;
  mbNo = 7;
  mbAll = 14;
  mbClose = 15;
  mbSlickeMinimize = 16;

  // Modal result constants (mr*) returned by dialogs
  mrNone = 0;
  mrOk = 1;
  mrCancel = 2;
  mrAbort = 3;
  mrRetry = 4;
  mrIgnore = 5;
  mrYes = 6;
  mrNo = 7;
  mrClose = 15;
  mrNoToAll = 16;
  mrYesToAll = 17;

function MessageDlg(const Msg: string; Flags: integer; Buttons: integer; HelpCtx: longint): integer; overload;
procedure ShowMessage(const Msg: string); overload;

type
  TOpenDialog = class
  public
    Title: string;
    Filter: string;
    DefaultExt: string;
    FileName: string;
    constructor Create(AOwner: TComponent = nil);
    function Execute: Boolean;
  end;

  TSaveDialog = class(TOpenDialog)
  end;

implementation

function MessageDlg(const Msg: string; Flags: integer; Buttons: integer; HelpCtx: longint): integer; overload;
begin
  Result := 0;
end;

procedure ShowMessage(const Msg: string); overload;
begin
  // no-op in tests (headless)
end;

constructor TOpenDialog.Create(AOwner: TComponent = nil);
begin
  FileName := '';
end;

function TOpenDialog.Execute: Boolean;
begin
  Result := True;
end;

end.
