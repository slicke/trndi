(*
 * This file is part of Trndi (https://github.com/xxxx or http://xxx.github.io).
 * Copyright (c) 2021-2024 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)
unit usplash;

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLType;

type

  { TfSplash }

  TfSplash = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    lInfo: TLabel;
    lSplashWarn: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  fSplash: TfSplash;

implementation

{$R *.lfm}

{ TfSplash }

procedure TfSplash.Image1Click(Sender: TObject);
begin

end;

procedure TfSplash.FormCreate(Sender: TObject);
begin
  {$ifdef X_LINUXBSD}
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
