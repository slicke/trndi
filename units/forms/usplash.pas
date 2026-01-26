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
unit usplash;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
LCLType, ComCtrls, IpHtml;

type

  { TfSplash }

TfSplash = class(TForm)
  Image1: TImage;
  lTrndi: TLabel;
  lInfo: TLabel;
  lSplashWarn: TLabel;
  pInfo: TProgressBar;
  procedure FormCreate(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  procedure FormKeyPress(Sender: TObject; var Key: char);
  procedure Image1Click(Sender: TObject);
private

public
public procedure incProgress(const proc: integer; const title: string);
end;

var
fSplash: TfSplash;

implementation

{$R *.lfm}

{ TfSplash }

procedure TfSplash.incProgress(const proc: integer; const title: string);
begin
  lInfo.Caption := title;
  if proc < 1 then
    pInfo.Position := pInfo.Position + (-1 * proc)
  else
    pInfo.Position := proc;
  Application.ProcessMessages;
end;

procedure TfSplash.Image1Click(Sender: TObject);
begin

end;

procedure TfSplash.FormCreate(Sender: TObject);
begin
  {$ifdef X_LINUXBSD}
  lSplashWarn.Font.Size := 8;
  {$endif}
  {$ifdef HAIKU}
  lSplashWarn.Font.Size := 8;
  {$endif}
end;

procedure TfSplash.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = VK_ESCAPE then
    Hide;
end;

procedure TfSplash.FormKeyPress(Sender: TObject; var Key: char);
begin

end;

end.
