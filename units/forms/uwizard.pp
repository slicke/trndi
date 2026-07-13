
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
 * - 2026-05-23: Added trend window step (step 4) — lets users choose how many
 *               readings to display in the main graph during first-run setup.
 * - 2026-07-13: Backend selection, credential validation, connection testing and
 *               the assisted browser login now go through the shared
 *               trndi.api.registry and trndi.weblogin units.
 *)

unit uwizard;

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs, lclintf,
  trndi.native, trndi.api, trndi.api.registry,
  trndi.types, trndi.strings, trndi.weblogin, slicke.ux.alert;

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
    bLogin: TButton;
    bTest: TButton;
    lTestStatus: TLabel;

    // Unit step controls
    rbMmol: TRadioButton;
    rbMgdl: TRadioButton;

    // Trend window step controls
    pnTrendWindow: TPanel;
    rbDots6, rbDots10, rbDots18, rbDots24, rbDots36: TRadioButton;

    // Threshold step controls
    pnThreshold: TPanel;
    lHiLabel: TLabel;
    eHi: TEdit;
    lLoLabel: TLabel;
    eLo: TEdit;

    // Navigation
    pnNav: TPanel;
    bBack: TButton;
    bNext: TButton;

    FThreshUnit: string;

    procedure ShowStep(step: integer);
    procedure UpdateNav;
    procedure UpdateConnectionLabels;
    function  selectedAPIClass: TrndiAPIClass;
    procedure updateWebLoginUI;
    procedure UpdateThresholdLabels;
    procedure bNextClick(Sender: TObject);
    procedure bBackClick(Sender: TObject);
    procedure bTestClick(Sender: TObject);
    procedure bLoginClick(Sender: TObject);
    procedure cbSysChange(Sender: TObject);
    procedure bSysHelpClick(Sender: TObject);
    procedure ePassEnter(Sender: TObject);
    procedure ePassExit(Sender: TObject);
    procedure BuildWelcomeStep;
    procedure BuildConnectionStep;
    procedure BuildUnitStep;
    procedure BuildTrendWindowStep;
    procedure BuildThresholdStep;
    procedure SaveSettings;
    function  ValidateConnection(out errMsg: string): boolean;
    function  ValidateThresholds(out errMsg: string): boolean;
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
  WZ_ERR_CARELINK_TOKEN = 'The credential must be the captured CareLink token data (JSON, starting with "{")';
  WZ_ERR_NEED_ADDR = 'Please enter a server address or username.';
  WZ_TEST_NO_SUPPORT = 'Connection testing is not supported for this service.';
  WZ_WEBLOGIN_BUTTON = 'Get CareLink token…';
  WZ_WEBLOGIN_TITLE = 'CareLink login helper';
  WZ_WEBLOGIN_HELP =
    'CareLink needs a one-time browser login (with CAPTCHA), so Trndi uses a small ' +
    'login helper to capture your token.'#13#10#13#10 +
    'It needs Node.js installed. In a terminal, run:'#13#10#13#10 +
    '    cd "%s"'#13#10 +
    '    npm install'#13#10 +
    '    %s'#13#10#13#10 +
    'A browser opens — sign in with your Care Partner account and solve the CAPTCHA. ' +
    'The helper then prints a block of JSON: copy it into the token field and click Test.'#13#10#13#10 +
    'Open the helper folder now?';
  WZ_WEBLOGIN_RUN_TITLE = 'CareLink login';
  WZ_WEBLOGIN_RUN_PROMPT =
    'Trndi will open a browser window for you to sign in to CareLink (with CAPTCHA), ' +
    'then capture the token automatically.'#13#10#13#10 +
    'The first run also downloads the login helper''s dependencies, which can take a ' +
    'minute. Keep this window open and complete the sign-in in the browser.'#13#10#13#10 +
    'Start now?';
  WZ_WEBLOGIN_INSTALLING = 'Installing login helper dependencies (first run only)…';
  WZ_WEBLOGIN_WAITING = 'Waiting for the browser sign-in to complete…';
  WZ_WEBLOGIN_OK = 'Login captured. Click Test to verify.';
  WZ_WEBLOGIN_NONODE =
    'Node.js was not found on this system, so Trndi cannot run the login helper for you. ' +
    'Install Node.js (22.12+) — or run the helper manually.';
  WZ_WEBLOGIN_NOSCRIPT =
    'The bundled CareLink login helper was not found next to Trndi. ' +
    'You can still run it manually.';
  WZ_WEBLOGIN_NPM_FAIL = 'Could not install the login helper''s dependencies (npm install failed).';
  WZ_WEBLOGIN_NO_OUTPUT = 'The login helper did not return any token data. The sign-in may have been cancelled or timed out.';
  WZ_WEBLOGIN_BAD_OUTPUT = 'The login helper did not return valid token data.';
  WZ_WEBLOGIN_FAIL_TITLE = 'Automatic login unavailable';
  WZ_ERR_THRESH_HI    = 'Please enter a valid high threshold (a number greater than zero).';
  WZ_ERR_THRESH_LO    = 'Please enter a valid low threshold (a number greater than zero).';
  WZ_ERR_THRESH_ORDER = 'The high threshold must be greater than the low threshold.';

const
  WIZARD_STEPS = 5;
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
  bBack.Parent    := pnNav;
  bBack.Caption   := RS_WIZARD_BACK;
  bBack.Left      := 10;
  bBack.Top       := 10;
  bBack.Width     := 90;
  bBack.Height    := 28;
  bBack.OnClick   := @bBackClick;
  bBack.TabStop   := false;  // Back is not part of the forward Tab sequence

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
  pnContent.Parent    := Self;
  pnContent.Align     := alClient;
  pnContent.BevelOuter := bvNone;
  pnContent.ParentBackground := false;
  pnContent.TabOrder  := 0;  // Content fields tab before the nav bar
  pnNav.TabOrder      := 1;

  BuildWelcomeStep;
  BuildConnectionStep;
  BuildUnitStep;
  BuildTrendWindowStep;
  BuildThresholdStep;

  FStep := 1;
  FThreshUnit := '';
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
  bTest.Parent    := pnInner;
  bTest.Caption   := RS_WIZARD_TEST;
  bTest.Align     := alTop;
  bTest.Height    := 28;
  bTest.BorderSpacing.Bottom := 8;
  bTest.OnClick   := @bTestClick;
  bTest.TabOrder  := 3;

  { Credentials }
  ePass := TEdit.Create(pnInner);
  ePass.Parent       := pnInner;
  ePass.Align        := alTop;
  ePass.EchoMode     := emPassword;
  ePass.PasswordChar := '*';
  ePass.BorderSpacing.Bottom := 10;
  ePass.OnEnter      := @ePassEnter;
  ePass.OnExit       := @ePassExit;
  ePass.TabOrder     := 2;

  lPassLabel := TLabel.Create(pnInner);
  lPassLabel.Parent   := pnInner;
  lPassLabel.Caption  := '';
  lPassLabel.Align    := alTop;
  lPassLabel.Height   := 18;
  lPassLabel.AutoSize := false;
  lPassLabel.BorderSpacing.Bottom := 4;

  { Browser-login button, shown instead of the address field for backends that
    capture their credentials via an in-app browser login (CareLink). }
  bLogin := TButton.Create(pnInner);
  bLogin.Parent   := pnInner;
  bLogin.Caption  := WZ_WEBLOGIN_BUTTON;
  bLogin.Align    := alTop;
  bLogin.Height   := 28;
  bLogin.BorderSpacing.Bottom := 10;
  bLogin.OnClick  := @bLoginClick;
  bLogin.TabOrder := 1;
  bLogin.Visible  := false;

  { Address }
  eAddr := TEdit.Create(pnInner);
  eAddr.Parent    := pnInner;
  eAddr.Align     := alTop;
  eAddr.BorderSpacing.Bottom := 10;
  eAddr.TabOrder  := 1;

  lAddrLabel := TLabel.Create(pnInner);
  lAddrLabel.Parent   := pnInner;
  lAddrLabel.Caption  := '';
  lAddrLabel.Align    := alTop;
  lAddrLabel.Height   := 18;
  lAddrLabel.AutoSize := false;
  lAddrLabel.BorderSpacing.Bottom := 4;

  { Data source row (top of the form) }
  pnSysRow := TPanel.Create(pnInner);
  pnSysRow.Parent    := pnInner;
  pnSysRow.Align     := alTop;
  pnSysRow.Height    := 34;
  pnSysRow.BevelOuter := bvNone;
  pnSysRow.BorderSpacing.Bottom := 12;
  pnSysRow.ParentBackground := false;
  pnSysRow.TabOrder  := 0;

  bSysHelp := TButton.Create(pnSysRow);
  bSysHelp.Parent    := pnSysRow;
  bSysHelp.Align     := alRight;
  bSysHelp.Width     := 30;
  bSysHelp.Caption   := '?';
  bSysHelp.OnClick   := @bSysHelpClick;
  bSysHelp.TabOrder  := 1;

  cbSys := TComboBox.Create(pnSysRow);
  cbSys.Parent    := pnSysRow;
  cbSys.Align     := alClient;
  cbSys.Style     := csDropDownList;
  cbSys.TabOrder  := 0;
  ListBackendNames(cbSys.Items, false);  // No debug backends during first-run
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
  pnWelcome.Visible     := (step = 1);
  pnConnection.Visible  := (step = 2);
  pnUnit.Visible        := (step = 3);
  pnTrendWindow.Visible := (step = 4);
  pnThreshold.Visible   := (step = 5);
  if step = 5 then
    UpdateThresholdLabels;
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

  if FStep = WIZARD_STEPS then
    if not ValidateThresholds(err) then
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
  cls: TrndiAPIClass;
begin
  cls := selectedAPIClass;
  if cls = nil then
  begin
    lAddrLabel.Caption := '';
    lPassLabel.Caption := '';
  end
  else
  begin
    lAddrLabel.Caption := cls.ParamLabel(APLUser);
    lPassLabel.Caption := cls.ParamLabel(APLPass);
  end;
  updateWebLoginUI;
end;

{------------------------------------------------------------------------------
  Metaclass of the backend currently selected in cbSys, or nil. Used for
  class-level capability queries (e.g. supportsWebLogin).
------------------------------------------------------------------------------}
function TfWizard.selectedAPIClass: TrndiAPIClass;
begin
  Result := BackendClassOf(cbSys.Text);
end;

{------------------------------------------------------------------------------
  Show the browser-login button (and hide the username field) for backends that
  offer an in-app login, seeding a non-empty username so a target is stored.
  See the twin in uconf.pp.
------------------------------------------------------------------------------}
procedure TfWizard.updateWebLoginUI;
var
  cls: TrndiAPIClass;
  webLogin: boolean;
begin
  cls := selectedAPIClass;
  webLogin := Assigned(cls) and cls.supportsWebLogin;

  { The connection step is an alTop stack, and LCL slots a re-shown control by
    its stale Top from before it was hidden. Pin the address row (or the login
    button) just above the password row before making it visible again, or it
    ends up below ePass after switching backends. Only pin on an actual
    hidden-to-shown transition (at build time the stack is not laid out yet
    and lPassLabel.Top is still 0), and freeze alignment for the whole swap so
    the hide doesn't shift lPassLabel up and invalidate the pins. }
  eAddr.Parent.DisableAlign;
  try
    if webLogin and not bLogin.Visible then
      bLogin.Top := lPassLabel.Top - 1
    else if not webLogin and not eAddr.Visible then
    begin
      lAddrLabel.Top := lPassLabel.Top - 2;
      eAddr.Top      := lPassLabel.Top - 1;
    end;

    bLogin.Visible := webLogin;
    lAddrLabel.Visible := not webLogin;
    eAddr.Visible := not webLogin;
  finally
    eAddr.Parent.EnableAlign;
  end;

  if webLogin and (Trim(eAddr.Text) = '') then
    eAddr.Text := 'carelink';
end;

{------------------------------------------------------------------------------
  Assisted CareLink login. The whole flow (confirm, run the backend's Node.js
  helper, capture the token into the token field, manual-instructions fallback)
  lives in trndi.weblogin so the wizard and the settings form behave the same;
  this handler only supplies the localized texts.
------------------------------------------------------------------------------}
procedure TfWizard.bLoginClick(Sender: TObject);
var
  T: TWebLoginTexts;
begin
  T.RunTitle   := WZ_WEBLOGIN_RUN_TITLE;
  T.RunPrompt  := WZ_WEBLOGIN_RUN_PROMPT;
  T.Installing := WZ_WEBLOGIN_INSTALLING;
  T.Waiting    := WZ_WEBLOGIN_WAITING;
  T.CapturedOK := WZ_WEBLOGIN_OK;
  T.FailTitle  := WZ_WEBLOGIN_FAIL_TITLE;
  T.NoScript   := WZ_WEBLOGIN_NOSCRIPT;
  T.NoNode     := WZ_WEBLOGIN_NONODE;
  T.NpmFailed  := WZ_WEBLOGIN_NPM_FAIL;
  T.NoOutput   := WZ_WEBLOGIN_NO_OUTPUT;
  T.BadOutput  := WZ_WEBLOGIN_BAD_OUTPUT;
  T.HelpTitle  := WZ_WEBLOGIN_TITLE;
  T.HelpBody   := WZ_WEBLOGIN_HELP;

  RunAssistedWebLogin(selectedAPIClass, bLogin, ePass, WZ_WEBLOGIN_BUTTON, T, Self);
end;

procedure TfWizard.bSysHelpClick(Sender: TObject);
var
  cls: TrndiAPIClass;
  s: string;
begin
  s := '';
  cls := selectedAPIClass;
  if cls <> nil then
    s := cls.ParamLabel(APLDesc);
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
  cls: TrndiAPIClass;
begin
  lTestStatus.Caption := RS_WIZARD_TESTING;
  Application.ProcessMessages;
  err := '';

  cls := selectedAPIClass;
  if cls = nil then
  begin
    lTestStatus.Caption := WZ_TEST_NO_SUPPORT;
    Exit;
  end;
  res := cls.testConnection(eAddr.Text, ePass.Text, err);

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
  errMsg := '';
  if Trim(eAddr.Text) = '' then
  begin
    errMsg := WZ_ERR_NEED_ADDR;
    Exit(false);
  end;
  case CheckBackendCredentials(cbSys.Text, eAddr.Text, ePass.Text) of
  bceAddress:
    errMsg := WZ_ERR_ADDRESS;
  bceEmail:
    errMsg := WZ_ERR_EMAIL;
  bcePassword:
    errMsg := WZ_ERR_PASSWORD;
  bceToken:
    errMsg := WZ_ERR_CARELINK_TOKEN;
  else
    Exit(true);
  end;
  Result := false;
end;

{-- Settings -----------------------------------------------------------------}

procedure TfWizard.BuildTrendWindowStep;
var
  pnInner: TPanel;
  lHead, lBody: TLabel;

  function MakeRB(const cap: string; checked: boolean): TRadioButton;
  begin
    Result := TRadioButton.Create(pnInner);
    Result.Parent  := pnInner;
    Result.Caption := cap;
    Result.Align   := alTop;
    Result.Height  := 26;
    Result.Checked := checked;
    Result.BorderSpacing.Bottom := 4;
  end;

begin
  pnTrendWindow := TPanel.Create(pnContent);
  pnTrendWindow.Parent  := pnContent;
  pnTrendWindow.Align   := alClient;
  pnTrendWindow.Visible := false;
  pnTrendWindow.BevelOuter := bvNone;
  pnTrendWindow.ParentBackground := false;

  pnInner := TPanel.Create(pnTrendWindow);
  pnInner.Parent := pnTrendWindow;
  pnInner.Align  := alClient;
  pnInner.BevelOuter := bvNone;
  pnInner.BorderSpacing.Around := 24;
  pnInner.ParentBackground := false;

  { Bottom-to-top creation order for LCL alTop stacking. }
  rbDots36 := MakeRB(RS_WIZARD_TREND_180, false);
  rbDots24 := MakeRB(RS_WIZARD_TREND_120, false);
  rbDots18 := MakeRB(RS_WIZARD_TREND_90,  false);
  rbDots10 := MakeRB(RS_WIZARD_TREND_50,  true);   // default
  rbDots6  := MakeRB(RS_WIZARD_TREND_30,  false);

  lBody := TLabel.Create(pnInner);
  lBody.Parent   := pnInner;
  lBody.Caption  := RS_WIZARD_TREND_BODY;
  lBody.Align    := alTop;
  lBody.AutoSize := false;
  lBody.Height   := 36;
  lBody.WordWrap := true;
  lBody.BorderSpacing.Bottom := 12;

  lHead := TLabel.Create(pnInner);
  lHead.Parent   := pnInner;
  lHead.Caption  := RS_WIZARD_TREND_HEAD;
  lHead.Align    := alTop;
  lHead.AutoSize := false;
  lHead.Height   := 28;
  lHead.Font.Size  := 13;
  lHead.Font.Style := [fsBold];
  lHead.BorderSpacing.Bottom := 8;
end;

procedure TfWizard.BuildThresholdStep;
var
  pnInner: TPanel;
  lHead, lBody: TLabel;
begin
  pnThreshold := TPanel.Create(pnContent);
  pnThreshold.Parent  := pnContent;
  pnThreshold.Align   := alClient;
  pnThreshold.Visible := false;
  pnThreshold.BevelOuter := bvNone;
  pnThreshold.ParentBackground := false;

  pnInner := TPanel.Create(pnThreshold);
  pnInner.Parent := pnThreshold;
  pnInner.Align  := alClient;
  pnInner.BevelOuter := bvNone;
  pnInner.BorderSpacing.Around := 24;
  pnInner.ParentBackground := false;

  { Bottom-to-top creation order for LCL alTop stacking. }
  eLo := TEdit.Create(pnInner);
  eLo.Parent := pnInner;
  eLo.Align  := alTop;
  eLo.BorderSpacing.Bottom := 16;

  lLoLabel := TLabel.Create(pnInner);
  lLoLabel.Parent   := pnInner;
  lLoLabel.Caption  := '';
  lLoLabel.Align    := alTop;
  lLoLabel.Height   := 18;
  lLoLabel.AutoSize := false;
  lLoLabel.BorderSpacing.Bottom := 4;

  eHi := TEdit.Create(pnInner);
  eHi.Parent := pnInner;
  eHi.Align  := alTop;
  eHi.BorderSpacing.Bottom := 16;

  lHiLabel := TLabel.Create(pnInner);
  lHiLabel.Parent   := pnInner;
  lHiLabel.Caption  := '';
  lHiLabel.Align    := alTop;
  lHiLabel.Height   := 18;
  lHiLabel.AutoSize := false;
  lHiLabel.BorderSpacing.Bottom := 4;

  lBody := TLabel.Create(pnInner);
  lBody.Parent   := pnInner;
  lBody.Caption  := RS_WIZARD_THRESH_BODY;
  lBody.Align    := alTop;
  lBody.AutoSize := false;
  lBody.Height   := 60;
  lBody.WordWrap := true;
  lBody.BorderSpacing.Bottom := 16;

  lHead := TLabel.Create(pnInner);
  lHead.Parent   := pnInner;
  lHead.Caption  := RS_WIZARD_THRESH_HEAD;
  lHead.Align    := alTop;
  lHead.AutoSize := false;
  lHead.Height   := 28;
  lHead.Font.Size  := 13;
  lHead.Font.Style := [fsBold];
  lHead.BorderSpacing.Bottom := 8;
end;

procedure TfWizard.UpdateThresholdLabels;
var
  isMmol: boolean;
  newUnit: string;
begin
  isMmol := rbMmol.Checked;
  if isMmol then
    newUnit := 'mmol'
  else
    newUnit := 'mgdl';

  if isMmol then
  begin
    lHiLabel.Caption := RS_WIZARD_THRESH_HI + ' (mmol/L)';
    lLoLabel.Caption := RS_WIZARD_THRESH_LO + ' (mmol/L)';
  end
  else
  begin
    lHiLabel.Caption := RS_WIZARD_THRESH_HI + ' (mg/dL)';
    lLoLabel.Caption := RS_WIZARD_THRESH_LO + ' (mg/dL)';
  end;

  if (FThreshUnit <> newUnit) or (eHi.Text = '') or (eLo.Text = '') then
  begin
    if isMmol then
    begin
      eHi.Text := FormatFloat('0.0', 10.0);
      eLo.Text := FormatFloat('0.0', 4.0);
    end
    else
    begin
      eHi.Text := '180';
      eLo.Text := '70';
    end;
    FThreshUnit := newUnit;
  end;
end;

function TfWizard.ValidateThresholds(out errMsg: string): boolean;
var
  hiF, loF: single;
  hiI, loI: integer;
begin
  Result := true;
  errMsg := '';

  if rbMmol.Checked then
  begin
    if not TryStrToFloat(eHi.Text, hiF) or (hiF <= 0) then
    begin
      errMsg := WZ_ERR_THRESH_HI;
      Result := false;
      Exit;
    end;
    if not TryStrToFloat(eLo.Text, loF) or (loF <= 0) then
    begin
      errMsg := WZ_ERR_THRESH_LO;
      Result := false;
      Exit;
    end;
    if loF >= hiF then
    begin
      errMsg := WZ_ERR_THRESH_ORDER;
      Result := false;
    end;
  end
  else
  begin
    if not TryStrToInt(eHi.Text, hiI) or (hiI <= 0) then
    begin
      errMsg := WZ_ERR_THRESH_HI;
      Result := false;
      Exit;
    end;
    if not TryStrToInt(eLo.Text, loI) or (loI <= 0) then
    begin
      errMsg := WZ_ERR_THRESH_LO;
      Result := false;
      Exit;
    end;
    if loI >= hiI then
    begin
      errMsg := WZ_ERR_THRESH_ORDER;
      Result := false;
    end;
  end;
end;

procedure TfWizard.SaveSettings;
var
  hiF, loF: single;
  hiI, loI: integer;
begin
  // Convert threshold inputs to mg/dL integers for storage
  if rbMmol.Checked then
  begin
    if not TryStrToFloat(eHi.Text, hiF) then hiF := 10.0;
    if not TryStrToFloat(eLo.Text, loF) then loF := 4.0;
    hiI := Round(hiF * 18.0);
    loI := Round(loF * 18.0);
  end
  else
  begin
    if not TryStrToInt(eHi.Text, hiI) then hiI := 180;
    if not TryStrToInt(eLo.Text, loI) then loI := 70;
  end;

  with FNative do
  begin
    SetSetting('remote.type',   BackendCode(cbSys.Text), false);
    SetSetting('remote.target', eAddr.Text, false);
    SetSetting('remote.creds',  ePass.Text, false);
    SetBoolSetting('unit', rbMmol.Checked, 'mmol', 'mgdl');
    SetSetting('wizard.hi', hiI);
    SetSetting('wizard.lo', loI);
    if      rbDots6.Checked  then SetSetting('ux.dot_count',  6)
    else if rbDots18.Checked then SetSetting('ux.dot_count', 18)
    else if rbDots24.Checked then SetSetting('ux.dot_count', 24)
    else if rbDots36.Checked then SetSetting('ux.dot_count', 36)
    else                          SetSetting('ux.dot_count', 10);  // default
  end;
end;

end.
