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
unit uconf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Spin, Forms, razer.chroma,
  trndi.theme;

const
  RS_DEFAULT_ACCOUNT = 'Default';
  // New/Update related resource strings used by umain
  RS_NEWVER = 'Version %s is available, would you like to go to the downloads page? You can also ignore this warning for 2 weeks.';
  RS_NEWVER_PRE = '';
  RS_NEWVER_CAPTION = 'New version available';
  RS_UPDATE_SNOOZE = 'Snooze update';
  RS_UPTODATE = 'Up-to-date';
  RS_UPTODATE_PR = 'You are using a newer temporary Trndi build, the latest stable release is: %s';
  RS_NEWVER_PR = 'This temporary Trndi build is now outdated, a newer stable version has been released: %s. Would you like to go to the downloads page?';

type
  // Dot preview renderer callback (see real uconf). The mock never paints, so
  // the type only has to match for umain's assignment to compile.
  TDotPreviewEvent = procedure(ACanvas: TCanvas; const ARect: TRect;
    AModeIndex: integer; const ATheme: TTrndiTheme) of object;

  // Display miniature renderer plumbing (see real uconf); same deal — the
  // types only have to match for umain to compile.
  TDisplayPreviewData = record
    ValFont, ArrowFont, AgoFont: string;
    State: integer;
    FreshRing: boolean;
    DecimalSep: string;
  end;

  TDisplayPreviewZones = record
    ValRect, ArrowRect, AgoRect: TRect;
  end;

  TDisplayPreviewEvent = procedure(ACanvas: TCanvas; const ARect: TRect;
    AModeIndex: integer; const ATheme: TTrndiTheme;
    const AData: TDisplayPreviewData;
    out AZones: TDisplayPreviewZones) of object;

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
    // Deferred TTS voice selection (see real uconf: EnsureTTSVoices)
    pendingTTSVoiceIndex: integer;
    pendingTTSVoiceName: string;
    // Real form defers voice enumeration to the Accessibility tab; the mock
    // pretends the list is always loaded so save paths stay exercised.
    TTSVoicesLoaded: boolean;
    // Stored 'remote.type' that no longer resolves; a property on the real form
    // (see uconf), a plain field here.
    UnknownBackend: string;
    // 'remote.creds' as it stood when the dialog loaded, so the save path can
    // skip writing a blob the user never edited (see uconf). Property there,
    // plain field here.
    LoadedCreds: string;
    // Dot preview renderer umain assigns before showing the dialog. Property
    // on the real form, plain field here; the mock never invokes it.
    OnDotPreview: TDotPreviewEvent;
    // Display miniature renderer, same arrangement as OnDotPreview.
    OnDisplayPreview: TDisplayPreviewEvent;
    // Preview fonts umain seeds and reads back on save. Properties on the
    // real form (owned TFont objects), plain fields here.
    FontVal: TFont;
    FontArrow: TFont;
    FontAgo: TFont;
    // Raised while umain fills the dialog, so OnChange handlers keep their
    // explanation popups to themselves (see uconf). Property there, plain
    // field here; nothing in the mock reacts to it.
    SettingsLoading: boolean;

    eAddr: TEdit;
    ePass: TEdit;
    rbUnit: TRadioGroup;
    spTHRESHOLD: TSpinEdit;
    spDeltaMax: TSpinEdit;
    fsLo: TFloatSpinEdit;
    fsHi: TFloatSpinEdit;
    fsLoRange: TFloatSpinEdit;
    fsHiRange: TFloatSpinEdit;
    // new spin edits added for label scaling settings that umain reads/writes
    fsDiffScale: TFloatSpinEdit;
    fsPredictScale: TFloatSpinEdit;
    cbMoveDIffRight: TCheckBox;
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
    cbPredictDots: TCheckBox;
    cbRotatingArrow: TCheckBox;
    cbBolusOverlay: TCheckBox;
    cbBolusOverlayAuto: TCheckBox;
    cbCarbOverlay: TCheckBox;
    cbDotFresh: TCheckBox;
    cbWebAPI: TCheckBox;
    cbAutoStart: TCheckBox;
    cbBadgeTrend: TCheckBox;
    cbPredictShort: TCheckBox;
    cbPredictShortFullArrows: TCheckBox;
    rbPredictShortShowValue: TRadioButton;
    rbPredictShortArrowOnly: TRadioButton;
    cbPredictShortSize: TComboBox;
    cbPredictShortMinutes: TComboBox;
    // progress/warning checkboxes used by umain
    cbProgress: TCheckBox;
    cbWarnLoHi: TCheckBox;

    // Additional UI fields used by umain
    edCommaSep: TEdit;

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
    // Text-to-speech & accessibility controls (also used by umain)
    cbTTS: TCheckBox;
    cbHContrast: TCheckBox;
    cbTTSVoice: TComboBox;
    seTTSRate: TSpinEdit;
    cbUserColor: TColorButton;
    cbPos: TComboBox;
    cbSize: TCheckBox;
    cbHints: TCheckBox;
    cbFlash: TCheckGroup;
    cbRangeColor: TCheckBox;
    cbClock: TCheckBox;
    cbAlternate: TCheckBox;
    cbOnTop: TCheckBox;
    cbNoBorders: TCheckBox;
    rbTrendWindow: TRadioGroup;
    rgDots: TRadioGroup;
    cbFlashHi: TCheckBox;
    cbFlashLow: TCheckBox;
    cbFlashPerfect: TCheckBox;
    cbAlertHiLo: TCheckBox;
    cbAlertMissing: TCheckBox;
    cbAlertReservoir: TCheckBox;
    cbAlertSensor: TCheckBox;
    cbAlertBattery: TCheckBox;
    spAlertDurHi: TSpinEdit;
    spAlertDurLo: TSpinEdit;
    spAlertDurUrg: TSpinEdit;
    fsAlertHystHi: TFloatSpinEdit;
    fsAlertHystLo: TFloatSpinEdit;
    fsAlertHystUrg: TFloatSpinEdit;
    cbConnectivityButton: TCheckBox;
    cbShowSensorExpiry: TCheckBox;
    cbLang: TComboBox;

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
    cbMediaDisable: TCheckBox;

    // Added UI methods used by umain
    procedure cbSysChange(Sender: TObject);
    procedure rbUnitClick(Sender: TObject);
    procedure cbMediaDisableChange(Sender: TObject);
    procedure UpdatePredictionStates;
    procedure ApplyRangeBounds;

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

procedure TfConf.cbMediaDisableChange(Sender: TObject);
begin
  // no-op in tests
end;

procedure TfConf.UpdatePredictionStates;
begin
  // no-op in tests
end;

procedure TfConf.ApplyRangeBounds;
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
  pendingTTSVoiceIndex := 0;
  pendingTTSVoiceName := '';
  TTSVoicesLoaded := true;

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
  // create additional controls that umain expects
  fsDiffScale := TFloatSpinEdit.Create;
  fsPredictScale := TFloatSpinEdit.Create;
  // initialize reasonable defaults
  fsDiffScale.Value := 1.0;
  fsPredictScale.Value := 1.0;
  cbMoveDIffRight := TCheckBox.Create(nil);
  cbProgress := TCheckBox.Create(nil);
  cbWarnLoHi := TCheckBox.Create(nil);
  // some defaults used by umain code
  cbProgress.Checked := false;
  cbWarnLoHi.Checked := false;
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
  cbPredictDots := TCheckBox.Create(nil);
  cbRotatingArrow := TCheckBox.Create(nil);
  cbBolusOverlay := TCheckBox.Create(nil);
  cbBolusOverlayAuto := TCheckBox.Create(nil);
  cbCarbOverlay := TCheckBox.Create(nil);
  cbWebAPI := TCheckBox.Create(nil);
  cbAutoStart := TCheckBox.Create(nil);
  cbBadgeTrend := TCheckBox.Create(nil);
  cbDotFresh := TCheckBox.Create(nil);
  cbPredictShort := TCheckBox.Create(nil);
  cbPredictShortFullArrows := TCheckBox.Create(nil);
  rbPredictShortShowValue := TRadioButton.Create(nil);
  rbPredictShortArrowOnly := TRadioButton.Create(nil);
  cbPredictShortSize := TComboBox.Create(nil);
  cbPredictShortMinutes := TComboBox.Create(nil);

  // Create new controls
  edCommaSep := TEdit.Create(nil);

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
  // TTS / accessibility mocks
  cbTTS := TCheckBox.Create(nil);
  cbHContrast := TCheckBox.Create(nil);
  cbTTSVoice := TComboBox.Create(nil);
  cbTTSVoice.ItemIndex := 0; // default selection
  seTTSRate := TSpinEdit.Create;
  seTTSRate.Value := 0;
  cbUserColor := TColorButton.Create(nil);
  cbPos := TComboBox.Create(nil);
  cbSize := TCheckBox.Create(nil);
  cbHints := TCheckBox.Create(nil);
  // Compatibility mocks: group and common checkboxes referenced by umain includes
  cbFlash := TCheckGroup.Create(nil);
  cbFlash.Items.Add('Hi');
  cbFlash.Items.Add('Low');
  cbFlash.Items.Add('Perfect');
  cbRangeColor := TCheckBox.Create(nil);
  cbClock := TCheckBox.Create(nil);
  cbAlternate := TCheckBox.Create(nil);
  cbOnTop := TCheckBox.Create(nil);
  cbNoBorders := TCheckBox.Create(nil);
  rbTrendWindow := TRadioGroup.Create(nil);
  rgDots := TRadioGroup.Create(nil);
  cbFlashHi := TCheckBox.Create(nil);
  cbFlashLow := TCheckBox.Create(nil);
  cbFlashPerfect := TCheckBox.Create(nil);
  cbAlertHiLo := TCheckBox.Create(nil);
  cbAlertMissing := TCheckBox.Create(nil);
  cbAlertReservoir := TCheckBox.Create(nil);
  cbAlertSensor := TCheckBox.Create(nil);
  cbAlertBattery := TCheckBox.Create(nil);
  spAlertDurHi := TSpinEdit.Create;
  spAlertDurLo := TSpinEdit.Create;
  spAlertDurUrg := TSpinEdit.Create;
  fsAlertHystHi := TFloatSpinEdit.Create;
  fsAlertHystLo := TFloatSpinEdit.Create;
  fsAlertHystUrg := TFloatSpinEdit.Create;
  cbConnectivityButton := TCheckBox.Create(nil);
  cbShowSensorExpiry := TCheckBox.Create(nil);
  cbLang := TComboBox.Create(nil);

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
  FontVal := TFont.Create;
  FontArrow := TFont.Create;
  FontAgo := TFont.Create;
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
  cbMediaDisable := TCheckBox.Create(nil);

end;


destructor TfConf.Destroy;
begin
  // Free created controls in reverse order of creation
  cbMediaDisable.Free;
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
  FontAgo.Free;
  FontArrow.Free;
  FontVal.Free;
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

  cbLang.Free;
  fsAlertHystUrg.Free;
  fsAlertHystLo.Free;
  fsAlertHystHi.Free;
  spAlertDurUrg.Free;
  spAlertDurLo.Free;
  spAlertDurHi.Free;
  cbAlertBattery.Free;
  cbAlertSensor.Free;
  cbAlertReservoir.Free;
  cbAlertMissing.Free;
  cbAlertHiLo.Free;
  cbShowSensorExpiry.Free;
  cbConnectivityButton.Free;
  rgDots.Free;
  rbTrendWindow.Free;
  cbNoBorders.Free;
  cbOnTop.Free;
  cbAlternate.Free;
  cbClock.Free;
  cbRangeColor.Free;
  cbFlash.Free;
  cbFlashPerfect.Free;
  cbFlashLow.Free;
  cbFlashHi.Free;
  cbHints.Free;
  cbSize.Free;
  cbPos.Free;
  cbUserColor.Free;
  // Free TTS / accessibility mocks
  seTTSRate.Free;
  cbTTSVoice.Free;
  cbHContrast.Free;
  cbTTS.Free;
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

  edCommaSep.Free;

  cbPredictShortMinutes.Free;
  // free progress/warning boxes in reverse creation order
  cbWarnLoHi.Free;
  cbProgress.Free;
  cbPredictShortSize.Free;
  rbPredictShortArrowOnly.Free;
  rbPredictShortShowValue.Free;
  cbPredictShortFullArrows.Free;
  cbPredictShort.Free;
  cbDotFresh.Free;
  cbBadgeTrend.Free;
  cbAutoStart.Free;
  cbWebAPI.Free;
  cbCarbOverlay.Free;
  cbBolusOverlayAuto.Free;
  cbBolusOverlay.Free;
  cbRotatingArrow.Free;
  cbPredictDots.Free;
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
  // free newly added spin edits before fsHiRange
  fsPredictScale.Free;
  fsDiffScale.Free;
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
