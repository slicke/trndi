unit uconf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Spin, Forms, razer.chroma;

const
  RS_DEFAULT_ACCOUNT = 'Default';
  // New/Update related resource strings used by umain
  RS_NEWVER = 'Version %s is available, would you like to go to the downloads page? You can also ignore this warning for 2 weeks.';
  RS_NEWVER_PRE = '';
  RS_NEWVER_CAPTION = 'New version available';
  RS_UPDATE_SNOOZE = 'Snooze update';
  RS_UPTODATE = 'Up-to-date';

type
  TfConf = class(TForm)
  public
    cbSys: TComboBox;
    lbChroma: TListBox;
    lbUsers: TListBox;
    lbExtensions: TListBox;
    lOS: TLabel;
    lWidgetset: TLabel;
    lWM: TLabel;
    lArch: TLabel;
    lExtCount: TLabel;
    tsExt: TPanel;
    Chroma: TRazerChromaBase;

    eAddr: TEdit;
    ePass: TEdit;
    rbUnit: TRadioGroup;
    spTHRESHOLD: TSpinEdit;
    spDeltaMax: TSpinEdit;
    fsLo: TFloatSpinEdit;
    fsHi: TFloatSpinEdit;
    fsLoRange: TFloatSpinEdit;
    fsHiRange: TFloatSpinEdit;
    cbTIR: TCheckBox;
    seTir: TSpinEdit;
    cbTirIcon: TCheckBox;
    cbShowMean: TCheckBox;
    cbOffBar: TCheckBox;
    cbPaintHiLo: TCheckBox;
    cbPaintLines: TCheckBox;
    cbPaintHiLoRange: TCheckBox;
    cbCust: TCheckBox;
    cbCustRange: TCheckBox;
    cbPredictions: TCheckBox;
    cbWebAPI: TCheckBox;
    cbPredictShort: TCheckBox;
    cbPredictShortFullArrows: TCheckBox;
    rbPredictShortShowValue: TRadioButton;
    rbPredictShortArrowOnly: TRadioButton;
    cbPredictShortSize: TComboBox;
    cbPredictShortMinutes: TComboBox;

    // Additional UI fields used by umain
    edCommaSep: TEdit;
    edTray: TSpinEdit;
    edMusicHigh: TEdit;
    edMusicLow: TEdit;
    edMusicPerfect: TEdit;
    edURLHigh: TEdit;
    edURLLow: TEdit;
    edURLPerfect: TEdit;
    edProxyHost: TEdit;
    edProxyPort: TEdit;
    edProxyUser: TEdit;
    edProxyPass: TEdit;
    cbChroma: TCheckBox;
    cbChromaNormal: TCheckBox;
    cbChromaHigh: TComboBox;
    cbChromaLow: TComboBox;
    cbMusicPause: TCheckBox;
    cbUserColor: TColorButton;
    cbPos: TComboBox;
    cbSize: TCheckBox;
    cbFlashHi: TCheckBox;
    cbFlashLow: TCheckBox;
    cbFlashPerfect: TCheckBox;
    cbAlertHiLo: TCheckBox;
    cbAlertMissing: TCheckBox;
    cbLang: TComboBox;

    cbFonts: TGroupBox;
    lDot: TLabel;
    lDot1: TLabel;
    lDot2: TLabel;
    lDot3: TLabel;
    lDotCurr: TLabel;
    eDot: TEdit;
    lDotNow: TLabel;
    eDotNow: TEdit;

    // Color buttons and related controls
    cl_ok_bg: TColorButton;
    cl_hi_bg: TColorButton;
    cl_lo_bg: TColorButton;
    cl_ok_txt: TColorButton;
    cl_hi_txt: TColorButton;
    cl_lo_txt: TColorButton;
    cl_hi_bg_cust: TColorButton;
    cl_lo_bg_cust: TColorButton;
    cl_hi_txt_cust: TColorButton;
    cl_lo_txt_cust: TColorButton;

    cbTirBar: TColorButton;
    cbTirBarCustom: TColorButton;

    pnDisplay: TPanel;
    cbTitleColor: TCheckBox;
    cbTirColor: TRadioButton;
    cbTirColorBg: TRadioButton;
    cbTirColorBgCustom: TRadioButton;
    cbTirColorCustom: TRadioButton;

    eExt: TEdit;
    bExtOpen: TButton;
    cbPrivacy: TCheckBox;
    cbTimeStamp: TCheckBox;
    cbTouch: TCheckBox;
    cbMultiTouch: TCheckBox;

    // Added UI methods used by umain
    procedure cbSysChange(Sender: TObject);
    procedure rbUnitClick(Sender: TObject);

    constructor Create(AOwner: TComponent = nil);
    destructor Destroy; override;
  end;

// Simple language helper stubs used by umain
procedure ListLanguageFiles(list: TStrings; const Path: string);
function GetLanguageName(const ACode: string): string;
function ExtractLangCode(const AText: string): string;

implementation

procedure TfConf.cbSysChange(Sender: TObject);
begin
  // no-op in tests
end;

function ExtractLangCode(const AText: string): string;
begin
  // Simple stub: take first 2 letters if present
  if Length(AText) >= 2 then
    Result := Copy(AText, 1, 2)
  else
    Result := AText;
end;

procedure TfConf.rbUnitClick(Sender: TObject);
begin
  // no-op in tests
end;

procedure ListLanguageFiles(list: TStrings; const Path: string);
begin
  // simple stub used by tests
  list.Clear;
  // Provide a couple of fake entries for LoadLanguageSettings to iterate
  list.Add('Trndi.en');
  list.Add('Trndi.auto');
end;

function GetLanguageName(const ACode: string): string;
begin
  Result := ACode;
end;

constructor TfConf.Create(AOwner: TComponent = nil);
begin
  inherited Create;
  cbSys := TComboBox.Create(nil);
  lbChroma := TListBox.Create(nil);
  lbUsers := TListBox.Create(nil);
  lbExtensions := TListBox.Create(nil);
  lOS := TLabel.Create(nil);
  lWidgetset := TLabel.Create(nil);
  lWM := TLabel.Create(nil);
  lArch := TLabel.Create(nil);
  lExtCount := TLabel.Create(nil);
  tsExt := TPanel.Create(nil);
  Chroma := nil; // tests or caller may assign via TRazerChromaFactory

  // Create additional mocked controls
  eAddr := TEdit.Create(nil);
  ePass := TEdit.Create(nil);
  rbUnit := TRadioGroup.Create(nil);
  spTHRESHOLD := TSpinEdit.Create;
  spDeltaMax := TSpinEdit.Create;
  fsLo := TFloatSpinEdit.Create;
  fsHi := TFloatSpinEdit.Create;
  fsLoRange := TFloatSpinEdit.Create;
  fsHiRange := TFloatSpinEdit.Create;
  cbTIR := TCheckBox.Create(nil);
  seTir := TSpinEdit.Create;
  cbTirIcon := TCheckBox.Create(nil);
  cbShowMean := TCheckBox.Create(nil);
  cbOffBar := TCheckBox.Create(nil);
  cbPaintHiLo := TCheckBox.Create(nil);
  cbPaintLines := TCheckBox.Create(nil);
  cbPaintHiLoRange := TCheckBox.Create(nil);
  cbCust := TCheckBox.Create(nil);
  cbCustRange := TCheckBox.Create(nil);
  cbPredictions := TCheckBox.Create(nil);
  cbWebAPI := TCheckBox.Create(nil);
  cbPredictShort := TCheckBox.Create(nil);
  cbPredictShortFullArrows := TCheckBox.Create(nil);
  rbPredictShortShowValue := TRadioButton.Create(nil);
  rbPredictShortArrowOnly := TRadioButton.Create(nil);
  cbPredictShortSize := TComboBox.Create(nil);
  cbPredictShortMinutes := TComboBox.Create(nil);

  // Create new controls
  edCommaSep := TEdit.Create(nil);
  edTray := TSpinEdit.Create;
  edMusicHigh := TEdit.Create(nil);
  edMusicLow := TEdit.Create(nil);
  edMusicPerfect := TEdit.Create(nil);
  edURLHigh := TEdit.Create(nil);
  edURLLow := TEdit.Create(nil);
  edURLPerfect := TEdit.Create(nil);
  edProxyHost := TEdit.Create(nil);
  edProxyPort := TEdit.Create(nil);
  edProxyUser := TEdit.Create(nil);
  edProxyPass := TEdit.Create(nil);
  cbChroma := TCheckBox.Create(nil);
  cbChromaNormal := TCheckBox.Create(nil);
  cbChromaHigh := TComboBox.Create(nil);
  cbChromaLow := TComboBox.Create(nil);
  cbMusicPause := TCheckBox.Create(nil);
  cbUserColor := TColorButton.Create(nil);
  cbPos := TComboBox.Create(nil);
  cbSize := TCheckBox.Create(nil);
  cbFlashHi := TCheckBox.Create(nil);
  cbFlashLow := TCheckBox.Create(nil);
  cbFlashPerfect := TCheckBox.Create(nil);
  cbAlertHiLo := TCheckBox.Create(nil);
  cbAlertMissing := TCheckBox.Create(nil);
  cbLang := TComboBox.Create(nil);

  cbFonts := TGroupBox.Create(nil);
  lDot := TLabel.Create(nil);
  lDot1 := TLabel.Create(nil);
  lDot2 := TLabel.Create(nil);
  lDot3 := TLabel.Create(nil);
  lDotCurr := TLabel.Create(nil);
  eDot := TEdit.Create(nil);
  lDotNow := TLabel.Create(nil);
  eDotNow := TEdit.Create(nil);

  // Color buttons and related controls
  cl_ok_bg := TColorButton.Create(nil);
  cl_hi_bg := TColorButton.Create(nil);
  cl_lo_bg := TColorButton.Create(nil);
  cl_ok_txt := TColorButton.Create(nil);
  cl_hi_txt := TColorButton.Create(nil);
  cl_lo_txt := TColorButton.Create(nil);
  cl_hi_bg_cust := TColorButton.Create(nil);
  cl_lo_bg_cust := TColorButton.Create(nil);
  cl_hi_txt_cust := TColorButton.Create(nil);
  cl_lo_txt_cust := TColorButton.Create(nil);

  cbTirBar := TColorButton.Create(nil);
  cbTirBarCustom := TColorButton.Create(nil);

  pnDisplay := TPanel.Create(nil);
  cbTitleColor := TCheckBox.Create(nil);
  cbTirColor := TRadioButton.Create(nil);
  cbTirColorBg := TRadioButton.Create(nil);
  cbTirColorBgCustom := TRadioButton.Create(nil);
  cbTirColorCustom := TRadioButton.Create(nil);

    // Initialize some defaults used by umain
    cbTirColor.Checked := false;
    cbTirColorBg.Checked := false;
    cbTitleColor.Checked := true;
  cbTouch := TCheckBox.Create(nil);
  cbMultiTouch := TCheckBox.Create(nil);

end;


destructor TfConf.Destroy;
begin
  // Free created controls in reverse order of creation
  cbMultiTouch.Free;
  cbTouch.Free;
  cbTimeStamp.Free;
  cbPrivacy.Free;
  bExtOpen.Free;
  eExt.Free;

  cbTirColorCustom.Free;
  cbTirColorBgCustom.Free;
  cbTirColorBg.Free;
  cbTirColor.Free;
  cbTitleColor.Free;
  pnDisplay.Free;
  cbTirBarCustom.Free;
  cbTirBar.Free;
  cl_lo_txt_cust.Free;
  cl_hi_txt_cust.Free;
  cl_lo_bg_cust.Free;
  cl_hi_bg_cust.Free;
  cl_lo_txt.Free;
  cl_hi_txt.Free;
  cl_ok_txt.Free;
  cl_lo_bg.Free;
  cl_hi_bg.Free;
  cl_ok_bg.Free;
  eDotNow.Free;
  lDotNow.Free;
  eDot.Free;
  lDotCurr.Free;
  lDot3.Free;
  lDot2.Free;
  lDot1.Free;
  lDot.Free;
  cbFonts.Free;

  cbLang.Free;
  cbAlertMissing.Free;
  cbAlertHiLo.Free;
  cbFlashPerfect.Free;
  cbFlashLow.Free;
  cbFlashHi.Free;
  cbSize.Free;
  cbPos.Free;
  cbUserColor.Free;
  cbMusicPause.Free;
  cbChromaLow.Free;
  cbChromaHigh.Free;
  cbChromaNormal.Free;
  cbChroma.Free;
  edProxyPass.Free;
  edProxyUser.Free;
  edProxyPort.Free;
  edProxyHost.Free;
  edURLPerfect.Free;
  edURLLow.Free;
  edURLHigh.Free;
  edMusicPerfect.Free;
  edMusicLow.Free;
  edMusicHigh.Free;
  edTray.Free;
  edCommaSep.Free;

  cbPredictShortMinutes.Free;
  cbPredictShortSize.Free;
  rbPredictShortArrowOnly.Free;
  rbPredictShortShowValue.Free;
  cbPredictShortFullArrows.Free;
  cbPredictShort.Free;
  cbWebAPI.Free;
  cbPredictions.Free;
  cbCustRange.Free;
  cbCust.Free;
  cbPaintHiLoRange.Free;
  cbPaintLines.Free;
  cbPaintHiLo.Free;
  cbOffBar.Free;
  cbShowMean.Free;
  cbTirIcon.Free;
  seTir.Free;
  cbTIR.Free;
  fsHiRange.Free;
  fsLoRange.Free;
  fsHi.Free;
  fsLo.Free;
  spDeltaMax.Free;
  spTHRESHOLD.Free;
  rbUnit.Free;
  ePass.Free;
  eAddr.Free;

  tsExt.Free;
  lExtCount.Free;
  lArch.Free;
  lWM.Free;
  lWidgetset.Free;
  lOS.Free;
  lbExtensions.Free;
  lbUsers.Free;
  lbChroma.Free;
  cbSys.Free;
  inherited Destroy;
end;

end.
