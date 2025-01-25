
(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
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

unit uconf;

{$mode ObjFPC}{$H+}

interface

uses 
Classes, ExtCtrls,StdCtrls,SysUtils, Forms, Controls, Graphics, Dialogs, LCLTranslator;

type 

  { TfConf }

TfConf = class(TForm)
  Bevel1: TBevel;
  cbPrivacy:TCheckBox;
  eExt: TLabeledEdit;
  lLicense: TButton;
  cbSys: TComboBox;
  Image1: TImage;
  lCopyright: TLabel;
  eAddr: TLabeledEdit;
  ePass: TLabeledEdit;
  lAck: TButton;
  lTitle: TLabel;
  lVersion: TLabel;
  rbUnit: TRadioGroup;
  procedure bLimitsClick(Sender:TObject);
  procedure cbSysChange(Sender:TObject);
  procedure FormCreate(Sender:TObject);
  procedure lAckClick(Sender:TObject);
  procedure lLicenseClick(Sender:TObject);
private

public

end;

var 
fConf: TfConf;

implementation

{$R *.lfm}

procedure TfConf.lAckClick(Sender:TObject);
begin
  ShowMessage('Trndi makes use of the following 3rd party libraries:'#10+
    'macOS native code libraries by Phil Hess'#10+
    'WinStyles library by Espectr0'#10#10+
    'Extensions use JavaScript engine QuickJS by Fabrice Bellard and Charlie Gordo'#10+

    'integration of said engine is made possible with mORMot2 by Synopse Informatique - Arnaud Bouchez'
    +#10#10+
    'Built in Object Pascal, using the Lazarus component library (LCL) and FreePascal.');
end;

procedure TfConf.cbSysChange(Sender:TObject);
begin

end;

procedure TfConf.bLimitsClick(Sender:TObject);
begin
end;

procedure TfConf.FormCreate(Sender:TObject);
begin
  lVersion.Caption := lVersion.Caption + ' | Test Build '+{$I %DATE%};
  lversion.left := lversion.left - 20;
end;

procedure TfConf.lLicenseClick(Sender:TObject);
begin
  ShowMessage('Trndi - CGM viewer'#10'A re-imagination of TrayTrend by Björn Lindh'#10#10+
    'Copyright (C) 2017-2025 Björn Lindh'#10#10+

    'This program is free software: you can redistribute it and/or modify it'#10+
    'under the terms of the GNU General Public License version 3 as published'#10+
    'by the Free Software Foundation.'#10#10+

    'For more information, refer to the accompanying license file or visit:'#10+
    'https://www.gnu.org/licenses/gpl-3.0'#10#10+
    'Trndi is hobby project, pleae verify all data with an officially apprived'#10+
    'medical app before acting on any shown data!');
end;

end.
