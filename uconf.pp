
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
Classes,ExtCtrls,Spin,StdCtrls,SysUtils,Forms,Controls,Graphics,Dialogs,
LCLTranslator;

type

  { TfConf }

TfConf = class(TForm)
  Bevel1: TBevel;
  bOverrideHelp:TButton;
  bPrivacyHelp:TButton;
  cbCust:TCheckBox;
  cbPrivacy:TCheckBox;
  eExt: TLabeledEdit;
  fsHi:TFloatSpinEdit;
  fsLo:TFloatSpinEdit;
  gbAdvanced:TGroupBox;
  gbOverride:TGroupBox;
  lHiOver:TLabel;
  lLicense: TButton;
  cbSys: TComboBox;
  Image1: TImage;
  lCopyright: TLabel;
  eAddr: TLabeledEdit;
  ePass: TLabeledEdit;
  lAck: TButton;
  lLounder:TLabel;
  lTitle: TLabel;
  lVersion: TLabel;
  rbUnit: TRadioGroup;
  tbAdvanced:TToggleBox;
  procedure bLimitsClick(Sender:TObject);
  procedure bOverrideHelpClick(Sender:TObject);
  procedure bPrivacyHelpClick(Sender:TObject);
  procedure cbCustChange(Sender:TObject);
  procedure cbSysChange(Sender:TObject);
  procedure FormCreate(Sender:TObject);
  procedure lAckClick(Sender:TObject);
  procedure lLicenseClick(Sender:TObject);
  procedure rbUnitClick(Sender:TObject);
  procedure tbAdvancedChange(Sender:TObject);
private

public

end;

resourcestring
RS_OVERRIDE_HELP =
  'Setting values here allows you to define your own high and low blood sugar limits in Trndi.'
  +#10+#10+
  'NightScout:'#10+
  'Trndi automatically retrieves your custom high and low settings from NightScout, so manually setting them here is usually unnecessary.'
  +#10+#10+
  'Dexcom:'+#10+
  'Dexcom servers do not provide custom high and low values. By setting them here, you can establish your own thresholds for Dexcom data.';

RS_PRIVACY_HELP =
  'When in Privacy Mode, the actual blood glucose value is hidden. Trndi will only tell the user if it''s good, high or low.';

RS_DEX =
  'Dexcom servers do not provide custom high and low blood sugar values. Please set your own thresholds at the bottom of this window.';

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
  gbOverride.Color := clDefault;
  if Pos('Dexcom', cbSys.Text) > 0 then
  begin
    if not tbAdvanced.Checked then
      tbAdvanced.Checked := true;
    gbOverride.Color := $00D3D2EE;
    ShowMessage(RS_DEX);
  end
  else
  if tbAdvanced.Checked and (not cbCust.Checked) then
    tbAdvanced.Checked := false;
end;

procedure TfConf.bLimitsClick(Sender:TObject);
begin
end;

procedure TfConf.bOverrideHelpClick(Sender:TObject);
begin
  ShowMessage(RS_OVERRIDE_HELP);
end;

procedure TfConf.bPrivacyHelpClick(Sender:TObject);
begin
  ShowMessage(RS_PRIVACY_HELP);
end;

procedure TfConf.cbCustChange(Sender:TObject);
begin
  fsHi.Enabled :=  cbCust.Checked;
  fsLo.Enabled :=  cbCust.Checked;
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

procedure TfConf.rbUnitClick(Sender:TObject);
begin
  if (sender is TForm) or (rbUnit.ItemIndex = 0) then begin
    fsHi.DecimalPlaces := 1;
    fsHi.Value := round(fsHi.Value * 0.0555 * 10) / 10; // Do the / 10 thing to keep the decimal
    fsLo.Value := round(fsLo.Value * 0.0555 * 10) / 10;
  end
  else begin
    fsHi.Value := round(fsHi.Value * 18.0182);
    fsLo.Value := round(fsLo.Value * 18.0182);
    fsHi.DecimalPlaces := 0;
  end;

  fsLo.DecimalPlaces := fsHi.DecimalPlaces;
end;

procedure TfConf.tbAdvancedChange(Sender:TObject);
begin
  if tbAdvanced.Checked then
    ClientHeight := gbAdvanced.top+gbAdvanced.height+7
  else
    ClientHeight := tbAdvanced.top+tbAdvanced.Height+7
end;

end.
