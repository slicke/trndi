
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

(*
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-05-16: Initial implementation of first-run setup wizard.
 *)

unit uwizard;

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs,
  trndi.native, trndi.api, trndi.api.nightscout, trndi.api.nightscout3,
  trndi.api.dexcom, trndi.api.dexcomNew, trndi.api.tandem, trndi.api.xdrip,
  trndi.types, trndi.strings;

{$I ../../inc/defines.inc}

type
  TfWizard = class(TForm)
  private
    FStep: integer;
    FNative: TrndiNative;

    // Header
    pnHeader: TPanel;
    lTitle: TLabel;
    lStep: TLabel;

    // Content area (pages overlap; only the active one is shown)
    pnContent: TPanel;
    pnWelcome: TPanel;
    pnConnection: TPanel;
    pnUnit: TPanel;

    // Connection step controls
    lSysLabel: TLabel;
    cbSys: TComboBox;
    bSysHelp: TButton;
    lAddrLabel: TLabel;
    eAddr: TEdit;
    lPassLabel: TLabel;
    ePass: TEdit;
    bTest: TButton;
    lTestStatus: TLabel;

    // Unit step controls
    rbMmol: TRadioButton;
    rbMgdl: TRadioButton;

    // Navigation
    pnNav: TPanel;
    bBack: TButton;
    bNext: TButton;

    procedure ShowStep(step: integer);
    procedure UpdateNav;
    procedure UpdateConnectionLabels;
    function  APIToCode(const displayName: string): string;
    procedure bNextClick(Sender: TObject);
    procedure bBackClick(Sender: TObject);
    procedure bTestClick(Sender: TObject);
    procedure cbSysChange(Sender: TObject);
    procedure bSysHelpClick(Sender: TObject);
    procedure ePassEnter(Sender: TObject);
    procedure ePassExit(Sender: TObject);
    procedure BuildWelcomeStep;
    procedure BuildConnectionStep;
    procedure BuildUnitStep;
    procedure SaveSettings;
    function  ValidateConnection(out errMsg: string): boolean;
  public
    { Pass the app's existing TrndiNative so the wizard writes to the same
      user-scoped key as the rest of the init. Wizard does NOT own it. }
    constructor Create(AOwner: TComponent; ANative: TrndiNative); reintroduce;
    destructor  Destroy; override;
  end;

implementation

resourcestring
  // Validation messages (parallel set to the ones in uconf.pp, needed here too)
  WZ_ERR_ADDRESS  = 'Address must start with http(s)://';
  WZ_ERR_EMAIL    = 'You must enter a valid e-mail address';
  WZ_ERR_PASSWORD = 'You must enter a password';
  WZ_ERR_NEED_ADDR = 'Please enter a server address or username.';
  WZ_TEST_NO_SUPPORT = 'Connection testing is not supported for this service.';

const
  WIZARD_STEPS = 3;
  FORM_WIDTH   = 500;
  FORM_HEIGHT  = 450;

{-- TfWizard -----------------------------------------------------------------}

constructor TfWizard.Create(AOwner: TComponent; ANative: TrndiNative);
var
  sep: TBevel;
  pnRight: TPanel;
begin
  inherited CreateNew(AOwner);

  Caption     := RS_WIZARD_TITLE;
  Width       := FORM_WIDTH;
  Height      := FORM_HEIGHT;
  Position    := poDesktopCenter;
  BorderStyle := bsDialog;

  FNative := ANative;  // Borrowed — caller owns it; wizard must NOT free it.

  { Coloured header -------------------------------------------------------- }
  pnHeader := TPanel.Create(Self);
  pnHeader.Parent := Self;
  pnHeader.Align  := alTop;
  pnHeader.Height := 72;
  pnHeader.BevelOuter := bvNone;
  pnHeader.ParentBackground := false;
  pnHeader.Color  := $00204060;

  lTitle := TLabel.Create(pnHeader);
  lTitle.Parent  := pnHeader;
  lTitle.Caption := RS_WIZARD_TITLE;
  lTitle.Font.Size  := 15;
  lTitle.Font.Color := clWhite;
  lTitle.Font.Style := [fsBold];
  lTitle.Left := 16;
  lTitle.Top  := 10;
  lTitle.AutoSize := true;

  lStep := TLabel.Create(pnHeader);
  lStep.Parent  := pnHeader;
  lStep.Caption := '';
  lStep.Font.Color := $00AACCEE;
  lStep.Left := 16;
  lStep.Top  := 44;
  lStep.AutoSize := true;

  { Navigation bar --------------------------------------------------------- }
  pnNav := TPanel.Create(Self);
  pnNav.Parent := Self;
  pnNav.Align  := alBottom;
  pnNav.Height := 48;
  pnNav.BevelOuter := bvNone;
  pnNav.ParentBackground := false;

  sep := TBevel.Create(Self);
  sep.Parent := Self;
  sep.Align  := alBottom;
  sep.Height := 1;
  sep.Shape  := bsTopLine;

  bBack := TButton.Create(pnNav);
  bBack.Parent  := pnNav;
  bBack.Caption := RS_WIZARD_BACK;
  bBack.Left    := 10;
  bBack.Top     := 10;
  bBack.Width   := 90;
  bBack.Height  := 28;
  bBack.OnClick := @bBackClick;

  pnRight := TPanel.Create(pnNav);
  pnRight.Parent := pnNav;
  pnRight.Align  := alRight;
  pnRight.Width  := 110;
  pnRight.BevelOuter := bvNone;
  pnRight.ParentBackground := false;

  bNext := TButton.Create(pnRight);
  bNext.Parent  := pnRight;
  bNext.Align   := alClient;
  bNext.Caption := RS_WIZARD_NEXT;
  bNext.BorderSpacing.Around := 10;
  bNext.OnClick := @bNextClick;

  { Content area ----------------------------------------------------------- }
  pnContent := TPanel.Create(Self);
  pnContent.Parent := Self;
  pnContent.Align  := alClient;
  pnContent.BevelOuter := bvNone;
  pnContent.ParentBackground := false;

  BuildWelcomeStep;
  BuildConnectionStep;
  BuildUnitStep;

  FStep := 1;
  ShowStep(1);
end;

destructor TfWizard.Destroy;
begin
  inherited;  // FNative is owned by the caller; do not free it here.
end;

{-- Step builders ------------------------------------------------------------}

procedure TfWizard.BuildWelcomeStep;
var
  pnInner: TPanel;
  lHead, lBody: TLabel;
begin
  pnWelcome := TPanel.Create(pnContent);
  pnWelcome.Parent := pnContent;
  pnWelcome.Align  := alClient;
  pnWelcome.BevelOuter := bvNone;
  pnWelcome.ParentBackground := false;

  pnInner := TPanel.Create(pnWelcome);
  pnInner.Parent := pnWelcome;
  pnInner.Align  := alClient;
  pnInner.BevelOuter := bvNone;
  pnInner.BorderSpacing.Around := 24;
  pnInner.ParentBackground := false;

  { Body first so it stacks below the heading. }
  lBody := TLabel.Create(pnInner);
  lBody.Parent   := pnInner;
  lBody.Caption  := RS_WIZARD_WELCOME_BODY;
  lBody.Align    := alTop;
  lBody.AutoSize := false;
  lBody.Height   := 120;
  lBody.WordWrap := true;

  lHead := TLabel.Create(pnInner);
  lHead.Parent  := pnInner;
  lHead.Caption := RS_WIZARD_WELCOME_HEAD;
  lHead.Align   := alTop;
  lHead.AutoSize := false;
  lHead.Height   := 28;
  lHead.Font.Size  := 13;
  lHead.Font.Style := [fsBold];
  lHead.BorderSpacing.Bottom := 16;
end;

procedure TfWizard.BuildConnectionStep;
var
  pnInner, pnSysRow: TPanel;
begin
  pnConnection := TPanel.Create(pnContent);
  pnConnection.Parent  := pnContent;
  pnConnection.Align   := alClient;
  pnConnection.Visible := false;
  pnConnection.BevelOuter := bvNone;
  pnConnection.ParentBackground := false;

  pnInner := TPanel.Create(pnConnection);
  pnInner.Parent := pnConnection;
  pnInner.Align  := alClient;
  pnInner.BevelOuter := bvNone;
  pnInner.BorderSpacing.Around := 20;
  pnInner.ParentBackground := false;

  { Build bottom-to-top: in LCL alTop stacks last-created at the top. }

  { Test status + button (bottom of the form) }
  lTestStatus := TLabel.Create(pnInner);
  lTestStatus.Parent   := pnInner;
  lTestStatus.Caption  := '';
  lTestStatus.Align    := alTop;
  lTestStatus.Height   := 20;
  lTestStatus.AutoSize := false;

  bTest := TButton.Create(pnInner);
  bTest.Parent   := pnInner;
  bTest.Caption  := RS_WIZARD_TEST;
  bTest.Align    := alTop;
  bTest.Height   := 28;
  bTest.BorderSpacing.Bottom := 8;
  bTest.OnClick  := @bTestClick;

  { Credentials }
  ePass := TEdit.Create(pnInner);
  ePass.Parent       := pnInner;
  ePass.Align        := alTop;
  ePass.EchoMode     := emPassword;
  ePass.PasswordChar := '*';
  ePass.BorderSpacing.Bottom := 10;
  ePass.OnEnter      := @ePassEnter;
  ePass.OnExit       := @ePassExit;

  lPassLabel := TLabel.Create(pnInner);
  lPassLabel.Parent   := pnInner;
  lPassLabel.Caption  := '';
  lPassLabel.Align    := alTop;
  lPassLabel.Height   := 18;
  lPassLabel.AutoSize := false;
  lPassLabel.BorderSpacing.Bottom := 4;

  { Address }
  eAddr := TEdit.Create(pnInner);
  eAddr.Parent := pnInner;
  eAddr.Align  := alTop;
  eAddr.BorderSpacing.Bottom := 10;

  lAddrLabel := TLabel.Create(pnInner);
  lAddrLabel.Parent   := pnInner;
  lAddrLabel.Caption  := '';
  lAddrLabel.Align    := alTop;
  lAddrLabel.Height   := 18;
  lAddrLabel.AutoSize := false;
  lAddrLabel.BorderSpacing.Bottom := 4;

  { Data source row (top of the form) }
  pnSysRow := TPanel.Create(pnInner);
  pnSysRow.Parent := pnInner;
  pnSysRow.Align  := alTop;
  pnSysRow.Height := 34;
  pnSysRow.BevelOuter := bvNone;
  pnSysRow.BorderSpacing.Bottom := 12;
  pnSysRow.ParentBackground := false;

  bSysHelp := TButton.Create(pnSysRow);
  bSysHelp.Parent  := pnSysRow;
  bSysHelp.Align   := alRight;
  bSysHelp.Width   := 30;
  bSysHelp.Caption := '?';
  bSysHelp.OnClick := @bSysHelpClick;

  cbSys := TComboBox.Create(pnSysRow);
  cbSys.Parent    := pnSysRow;
  cbSys.Align     := alClient;
  cbSys.Style     := csDropDownList;
  cbSys.Items.AddStrings([
    API_NS, API_NS3,
    API_DEX_USA, API_DEX_EU,
    API_DEX_NEW_USA, API_DEX_NEW_EU, API_DEX_NEW_JP,
    API_TANDEM_USA, API_TANDEM_EU,
    API_XDRIP
  ]);
  cbSys.ItemIndex := 0;
  cbSys.OnChange  := @cbSysChange;

  lSysLabel := TLabel.Create(pnInner);
  lSysLabel.Parent  := pnInner;
  lSysLabel.Caption := RS_WIZARD_SOURCE;
  lSysLabel.Align   := alTop;
  lSysLabel.Height  := 18;
  lSysLabel.AutoSize := false;
  lSysLabel.BorderSpacing.Bottom := 4;

  UpdateConnectionLabels;
end;

procedure TfWizard.BuildUnitStep;
var
  pnInner: TPanel;
  lHead: TLabel;
begin
  pnUnit := TPanel.Create(pnContent);
  pnUnit.Parent  := pnContent;
  pnUnit.Align   := alClient;
  pnUnit.Visible := false;
  pnUnit.BevelOuter := bvNone;
  pnUnit.ParentBackground := false;

  pnInner := TPanel.Create(pnUnit);
  pnInner.Parent := pnUnit;
  pnInner.Align  := alClient;
  pnInner.BevelOuter := bvNone;
  pnInner.BorderSpacing.Around := 24;
  pnInner.ParentBackground := false;

  { Bottom-to-top creation order for LCL alTop stacking. }
  rbMgdl := TRadioButton.Create(pnInner);
  rbMgdl.Parent  := pnInner;
  rbMgdl.Caption := RS_WIZARD_UNIT_MGDL;
  rbMgdl.Align   := alTop;
  rbMgdl.Height  := 28;

  rbMmol := TRadioButton.Create(pnInner);
  rbMmol.Parent  := pnInner;
  rbMmol.Caption := RS_WIZARD_UNIT_MMOL;
  rbMmol.Align   := alTop;
  rbMmol.Height  := 28;
  rbMmol.Checked := true;
  rbMmol.BorderSpacing.Bottom := 8;

  lHead := TLabel.Create(pnInner);
  lHead.Parent   := pnInner;
  lHead.Caption  := RS_WIZARD_UNIT_HEAD;
  lHead.Align    := alTop;
  lHead.AutoSize := false;
  lHead.Height   := 36;
  lHead.WordWrap := true;
  lHead.BorderSpacing.Bottom := 20;
end;

{-- Navigation ---------------------------------------------------------------}

procedure TfWizard.ShowStep(step: integer);
begin
  FStep := step;
  pnWelcome.Visible    := (step = 1);
  pnConnection.Visible := (step = 2);
  pnUnit.Visible       := (step = 3);
  lStep.Caption := Format(RS_WIZARD_STEP_FMT, [step, WIZARD_STEPS]);
  UpdateNav;
end;

procedure TfWizard.UpdateNav;
begin
  bBack.Enabled := (FStep > 1);
  if FStep = WIZARD_STEPS then
    bNext.Caption := RS_WIZARD_FINISH
  else
    bNext.Caption := RS_WIZARD_NEXT;
end;

procedure TfWizard.bBackClick(Sender: TObject);
begin
  if FStep > 1 then
    ShowStep(FStep - 1);
end;

procedure TfWizard.bNextClick(Sender: TObject);
var
  err: string;
begin
  if FStep = 2 then
    if not ValidateConnection(err) then
    begin
      ShowMessage(err);
      Exit;
    end;

  if FStep < WIZARD_STEPS then
    ShowStep(FStep + 1)
  else
  begin
    SaveSettings;
    ModalResult := mrOK;
  end;
end;

{-- Connection step ----------------------------------------------------------}

procedure TfWizard.cbSysChange(Sender: TObject);
begin
  lTestStatus.Caption := '';
  UpdateConnectionLabels;
end;

procedure TfWizard.UpdateConnectionLabels;
var
  u, p: string;
begin
  u := ''; p := '';
  if      cbSys.Text = API_NS        then begin u := NightScout.ParamLabel(APLUser);  p := NightScout.ParamLabel(APLPass); end
  else if cbSys.Text = API_NS3       then begin u := NightScout3.ParamLabel(APLUser); p := NightScout3.ParamLabel(APLPass); end
  else if (cbSys.Text = API_DEX_USA) or (cbSys.Text = API_DEX_EU) then
                                          begin u := Dexcom.ParamLabel(APLUser);     p := Dexcom.ParamLabel(APLPass); end
  else if (cbSys.Text = API_DEX_NEW_USA) or (cbSys.Text = API_DEX_NEW_EU)
       or (cbSys.Text = API_DEX_NEW_JP) then
                                          begin u := DexcomNew.ParamLabel(APLUser);  p := DexcomNew.ParamLabel(APLPass); end
  else if cbSys.Text = API_TANDEM_USA then begin u := TandemUSA.ParamLabel(APLUser); p := TandemUSA.ParamLabel(APLPass); end
  else if cbSys.Text = API_TANDEM_EU  then begin u := TandemEU.ParamLabel(APLUser);  p := TandemEU.ParamLabel(APLPass); end
  else if cbSys.Text = API_XDRIP      then begin u := xDrip.ParamLabel(APLUser);     p := xDrip.ParamLabel(APLPass); end;
  lAddrLabel.Caption := u;
  lPassLabel.Caption := p;
end;

procedure TfWizard.bSysHelpClick(Sender: TObject);
var
  s: string;
begin
  s := '';
  if      cbSys.Text = API_NS        then s := NightScout.ParamLabel(APLDesc)
  else if cbSys.Text = API_NS3       then s := NightScout3.ParamLabel(APLDesc)
  else if cbSys.Text = API_DEX_USA   then s := DexcomUSA.ParamLabel(APLDesc)
  else if cbSys.Text = API_DEX_EU    then s := DexcomWorld.ParamLabel(APLDesc)
  else if cbSys.Text = API_DEX_NEW_USA then s := DexcomUSANew.ParamLabel(APLDesc)
  else if cbSys.Text = API_DEX_NEW_EU  then s := DexcomWorldNew.ParamLabel(APLDesc)
  else if cbSys.Text = API_DEX_NEW_JP  then s := DexcomJapanNew.ParamLabel(APLDesc)
  else if cbSys.Text = API_TANDEM_USA  then s := TandemUSA.ParamLabel(APLDesc)
  else if cbSys.Text = API_TANDEM_EU   then s := TandemEU.ParamLabel(APLDesc)
  else if cbSys.Text = API_XDRIP       then s := xDrip.ParamLabel(APLDesc);
  if s <> '' then
    ShowMessage(s);
end;

procedure TfWizard.ePassEnter(Sender: TObject);
begin
  ePass.EchoMode := emNormal;
  ePass.PasswordChar := #0;
end;

procedure TfWizard.ePassExit(Sender: TObject);
begin
  ePass.EchoMode := emPassword;
  ePass.PasswordChar := '*';
end;

procedure TfWizard.bTestClick(Sender: TObject);
var
  res: MaybeBool;
  err: string;
begin
  lTestStatus.Caption := RS_WIZARD_TESTING;
  Application.ProcessMessages;
  err := '';

  if cbSys.Text = API_NS then
    res := NightScout.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_NS3 then
    res := NightScout3.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_DEX_USA then
    res := DexcomUSA.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_DEX_EU then
    res := DexcomWorld.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_DEX_NEW_USA then
    res := DexcomUSANew.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_DEX_NEW_EU then
    res := DexcomWorldNew.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_DEX_NEW_JP then
    res := DexcomJapanNew.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_TANDEM_USA then
    res := TandemUSA.testConnection(eAddr.Text, ePass.Text, err)
  else if cbSys.Text = API_TANDEM_EU then
    res := TandemEU.testConnection(eAddr.Text, ePass.Text, err)
  else
  begin
    lTestStatus.Caption := WZ_TEST_NO_SUPPORT;
    Exit;
  end;

  case res of
  MaybeBool.true:
    lTestStatus.Caption := RS_WIZARD_TEST_OK;
  MaybeBool.false:
    if err <> '' then
      lTestStatus.Caption := RS_WIZARD_TEST_FAIL + ' ' + err
    else
      lTestStatus.Caption := RS_WIZARD_TEST_FAIL;
  else
    lTestStatus.Caption := WZ_TEST_NO_SUPPORT;
  end;
end;

function TfWizard.ValidateConnection(out errMsg: string): boolean;
begin
  Result := true;
  errMsg := '';
  if Trim(eAddr.Text) = '' then
  begin
    errMsg := WZ_ERR_NEED_ADDR;
    Result := false;
    Exit;
  end;
  case cbSys.Text of
  API_NS, API_NS3:
    if Copy(eAddr.Text, 1, 4) <> 'http' then
    begin
      errMsg := WZ_ERR_ADDRESS;
      Result := false;
    end;
  API_TANDEM_EU, API_TANDEM_USA:
    begin
      if Pos('@', eAddr.Text) = 0 then
      begin
        errMsg := WZ_ERR_EMAIL;
        Result := false;
        Exit;
      end;
      if Length(ePass.Text) < 5 then
      begin
        errMsg := WZ_ERR_PASSWORD;
        Result := false;
      end;
    end;
  API_DEX_EU, API_DEX_USA,
  API_DEX_NEW_EU, API_DEX_NEW_USA, API_DEX_NEW_JP:
    if Length(ePass.Text) < 5 then
    begin
      errMsg := WZ_ERR_PASSWORD;
      Result := false;
    end;
  end;
end;

{-- Settings -----------------------------------------------------------------}

function TfWizard.APIToCode(const displayName: string): string;
begin
  case displayName of
  API_NS:          Result := 'API_NS';
  API_NS3:         Result := 'API_NS3';
  API_DEX_USA:     Result := 'API_DEX_USA';
  API_DEX_EU:      Result := 'API_DEX_EU';
  API_DEX_NEW_USA: Result := 'API_DEX_NEW_USA';
  API_DEX_NEW_EU:  Result := 'API_DEX_NEW_EU';
  API_DEX_NEW_JP:  Result := 'API_DEX_NEW_JP';
  API_TANDEM_USA:  Result := 'API_TANDEM_USA';
  API_TANDEM_EU:   Result := 'API_TANDEM_EU';
  API_XDRIP:       Result := 'API_XDRIP';
  else             Result := 'API_NS';
  end;
end;

procedure TfWizard.SaveSettings;
begin
  with FNative do
  begin
    SetSetting('remote.type',   APIToCode(cbSys.Text), false);
    SetSetting('remote.target', eAddr.Text, false);
    SetSetting('remote.creds',  ePass.Text, false);
    SetBoolSetting('unit', rbMmol.Checked, 'mmol', 'mgdl');
  end;
end;

end.
