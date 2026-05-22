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
Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, LCLType;

type

  { TfSplash }

TfSplash = class(TForm)
  Image1: TImage;
  lTrndi: TLabel;
  lInfo: TLabel;
  lSplashWarn: TLabel;
  pbProgress: TPaintBox;
  procedure FormCreate(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  procedure FormKeyPress(Sender: TObject; var Key: char);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure Image1Click(Sender: TObject);
  procedure pbProgressPaint(Sender: TObject);
private
  FProgress: integer;
public
  procedure incProgress(const proc: integer; const title: string);
end;

var
fSplash: TfSplash;

implementation

{$R *.lfm}

const
  CARD_L      = 220;
  CARD_T      = 68;
  CARD_R      = 636;
  CARD_B      = 336; // 254 px inner — fits 17 lines of original text at -10 font
  CARD_MARGIN = 14;
  CARD_CORNER = 18;

{ TfSplash }

procedure TfSplash.incProgress(const proc: integer; const title: string);
begin
  lInfo.Caption := title;
  if proc < 1 then
    Inc(FProgress, -proc)
  else
    FProgress := proc;
  pbProgress.Invalidate;
  Application.ProcessMessages;
end;

procedure TfSplash.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := $001E1E1E;
  Canvas.Pen.Style   := psClear;
  Canvas.RoundRect(CARD_L, CARD_T, CARD_R, CARD_B, CARD_CORNER, CARD_CORNER);
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Width   := 1;
  Canvas.Pen.Color   := $00363636;
  Canvas.RoundRect(CARD_L, CARD_T, CARD_R, CARD_B, CARD_CORNER, CARD_CORNER);
  Canvas.Pen.Color := $004E4E4E;
  Canvas.MoveTo(CARD_L + CARD_CORNER div 2, CARD_T + 1);
  Canvas.LineTo(CARD_R - CARD_CORNER div 2, CARD_T + 1);
end;

procedure TfSplash.FormShow({%H-}Sender: TObject);
begin
  Invalidate;
end;

procedure TfSplash.pbProgressPaint(Sender: TObject);
var
  fillW: integer;
begin
  with pbProgress.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := $00141414;
    Pen.Style   := psClear;
    FillRect(pbProgress.ClientRect);
    if FProgress > 0 then
    begin
      fillW := pbProgress.Width * FProgress div 100;
      Brush.Color := $0000DC84;
      FillRect(Rect(0, 0, fillW, pbProgress.Height));
    end;
  end;
end;

procedure TfSplash.Image1Click({%H-}Sender: TObject);
begin

end;

procedure TfSplash.FormCreate({%H-}Sender: TObject);
begin
  FProgress := 0;
  {$ifdef X_LINUXBSD}
  lSplashWarn.Font.Size := 8;
  {$endif}
  {$ifdef HAIKU}
  lSplashWarn.Font.Size := 8;
  {$endif}
end;

procedure TfSplash.FormKeyDown({%H-}Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
begin
  if key = VK_ESCAPE then
    Hide;
end;

procedure TfSplash.FormKeyPress({%H-}Sender: TObject; var {%H-}Key: char);
begin

end;

end.
