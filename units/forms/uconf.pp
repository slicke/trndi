
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
 * - 2026-01-11: Added settings UI controls for configuring Razer Chroma behavior
 *   separately for high/low alerts (dropdowns), in addition to the existing enable
 *   + normal-state options.
 * - 2026-02-03: Added a Proxy "Test proxy" button plus wiring so proxy settings
 *   load/save from the settings store, and a proxy-only connectivity test.
 * - 2026-07-13: Backend selection, credential validation, connection testing and
 *   the assisted browser login now go through the shared trndi.api.registry and
 *   trndi.weblogin units instead of per-form dispatch chains.
 * - 2026-07-15: TTS voice enumeration and Razer Chroma device detection are now
 *   deferred to their tabs' OnShow instead of running before the dialog opens,
 *   so the settings window appears faster.
 * - 2026-07-16: The extension list (reading + parsing every extension script)
 *   is likewise deferred to the Extensions tab's OnShow.
 * - 2026-07-16: The extension info panel now shows the granted permission
 *   groups (or pending/changed status) next to what the script requests.
 * - 2026-07-19: Settings export/import strings made translatable, overwrite
 *   prompt on export, empty-file guard on import; multi-user name validation
 *   no longer accepts [\]^_` characters; removed unused CodepointHex.
 *)

unit uconf;

{$I ../../inc/native.inc}

interface

uses
Classes, CheckLst, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, Forms, Controls,
Graphics, Dialogs, LCLTranslator, trndi.native, lclintf, process, FileUtil, trndi.weblogin{$ifdef X_MAC}, CocoaAll, nsutils.nshelpers{$endif},
slicke.ux.alert, slicke.ux.native, slicke.versioninfo, trndi.funcs, buildinfo, StrUtils, trndi.api, trndi.api.registry, razer.chroma, razer.chroma.factory, math, trndi.types, trndi.theme, base64, Variants{$ifdef TrndiExt}, trndi.ext.perm{$endif}{$ifdef X_WIN}, ComObj{$endif};

{$I ../../inc/defines.inc}
type

  { TfConf }

TfConf = class(TForm)
  bAdd: TButton;
  bAlertDurHelp: TButton;
  bAlertHystHelp: TButton;
  bBackendHelp: TButton;
  bDisplayGeneralHelp: TButton;
  bDisplayAdvancedHelp: TButton;
  bDisplayWindowHelp: TButton;
  bConnectHelp: TButton;
  bDecimalHelp: TButton;
  bDeltaMaxHelp: TButton;
  bExportSettings: TButton;
  bExtOpen: TButton;
  bFontReading: TButton;
  bFontArrow: TButton;
  bFontTime: TButton;
  bFontReset: TButton;
  bWarnHiLowHelp: TButton;
  bImportSettings: TButton;
  bLanguageHelp: TButton;
  bNotificationHelp: TButton;
  bColorGraphHelp: TButton;
  bMultiUserHelp: TButton;
  bOutdatedHelp: TButton;
  bOutdatedHelp1: TButton;
  bOverrideHelp1: TButton;
  bShortModeHelp: TButton;
  bPredictHorizon: TButton;
  bDiffRightHelp: TButton;
  bScaleHelp: TButton;
  bPredScaleHelp: TButton;
  bFullArrowSetHelp: TButton;
  bRotatingArrowHelp: TButton;
  bTestSpeech: TButton;
  bTimeStampHelp: TButton;
  bUseURLHelp: TButton;
  bThreasholdLinesHelp: TButton;
  bBadgeFlashHelp: TButton;
  bPrivacyHelp: TButton;
  bPredictHelp: TButton;
  bTest: TButton;
  bLogin: TButton;
  bTestProxy: TButton;
  bOverrideHelp: TButton;
  bRemove: TButton;
  bSysNotice: TButton;
  bMinMinutesHelp: TButton;
  bCustomRangeHelp: TButton;
  bCommon: TButton;
  bDisableMediaHelp: TButton;
  bWebAPI: TButton;
  bSysTouch: TButton;
  bTestAnnounce: TButton;
  btReset: TButton;
  btUserSave: TButton;
  Button3: TButton;
  bvExt: TBevel;
  bvExt1: TBevel;
  cbAlternate: TCheckBox;
  cbAutoStart: TCheckBox;
  cbBadgeTrend: TCheckBox;
  cbChromaNormal: TCheckBox;
  cbChromaHigh: TComboBox;
  cbChromaLow: TComboBox;
  cbClock: TCheckBox;
  cbConnectivityButton: TCheckBox;
  cbCust: TCheckBox;
  cbCust1: TCheckBox;
  cbCustRange: TCheckBox;
  cbDotFresh: TCheckBox;
  cbAlertMissing: TCheckBox;
  cbLang: TComboBox;
  cbMoveDIffRight: TCheckBox;
  cbNoBorders: TCheckBox;
  cbOnTop: TCheckBox;
  cbPredictDots: TCheckBox;
  cbProgress: TCheckBox;
  cbRangeColor: TCheckBox;
  cbShowSensorExpiry: TCheckBox;
  cbWarnLoHi: TCheckBox;
  cbTTS: TCheckBox;
  cbTTSVoice: TComboBox;
  cgNotifications: TCheckGroup;
  cbAlertHiLo: TCheckBox;
  cbFlashLow: TCheckBox;
  cbFlashPerfect: TCheckBox;
  cbMultiTouch: TCheckBox;
  cbMusicPause: TCheckBox;
  cbNotice: TCheckBox;
  cbPredictions: TCheckBox;
  cbRotatingArrow: TCheckBox;
  cbPredictShort: TCheckBox;
  cbPredictShortFullArrows: TCheckBox;
  cbPredictShortMinutes: TComboBox;
  cbPredictShortSize: TComboBox;
  cbPrivacy: TCheckBox;
  cbHContrast: TCheckBox;
  cbMediaDisable: TCheckBox;
  edCommaSep: TEdit;
  eExt: TEdit;
  fsDiffScale: TFloatSpinEdit;
  fsPredictScale: TFloatSpinEdit;
  GroupBox10: TGroupBox;
  GroupBox11: TGroupBox;
  GroupBox12: TGroupBox;
  GroupBox13: TGroupBox;
  Label1: TLabel;
  Label13: TLabel;
  Label25: TLabel;
  Label36: TLabel;
  Label37: TLabel;
  Label38: TLabel;
  Label39: TLabel;
  Label40: TLabel;
  Label41: TLabel;
  Label42: TLabel;
  lExt: TLabel;
  Panel18: TPanel;
  Panel21: TPanel;
  Panel22: TPanel;
  Panel23: TPanel;
  Panel24: TPanel;
  edCommaSep1: TEdit;
  edProxyHost: TEdit;
  edProxyPass: TEdit;
  edProxyPort: TEdit;
  edProxyUser: TEdit;
  fsHi1: TFloatSpinEdit;
  fsLo1: TFloatSpinEdit;
  gbNetwork: TGroupBox;
  gbOverride1: TGroupBox;
  gbSettings: TGroupBox;
  Label17: TLabel;
  Label33: TLabel;
  Label34: TLabel;
  Label35: TLabel;
  LabelProxyHost: TLabel;
  LabelProxyPass: TLabel;
  LabelProxyPort: TLabel;
  LabelProxyUser: TLabel;
  lConfigPredict: TLabel;
  lHiOver3: TLabel;
  lLounder2: TLabel;
  lProxyTitle: TLabel;
  lSettingsHelp: TLabel;
  lSysWarnInfo: TLabel;
  lProxyDesc: TLabel;
  Panel19: TPanel;
  Panel20: TPanel;
  Panel25: TPanel;
  Panel26: TPanel;
  Panel27: TPanel;
  Panel3: TPanel;
  PanelProxyActions: TPanel;
  pDecimal: TPanel;
  pnDeltaMax: TPanel;
  pnFontButtons: TPanel;
  pnMisc: TPanel;
  pnMisc1: TPanel;
  pnProxyConnection: TPanel;
  pnProxyCreds: TPanel;
  pnProxyHost: TPanel;
  pnProxyPass: TPanel;
  pnProxyPort: TPanel;
  pnProxyUser: TPanel;
  pnSysWarn: TPanel;
  rbPredictShortArrowOnly: TRadioButton;
  rbPredictShortShowValue: TRadioButton;
  cbTimeStamp: TCheckBox;
  cbTirIcon: TCheckBox;
  cbShowMean: TCheckBox;
  cbWebAPI: TCheckBox;
  cbOffBar: TCheckBox;
  cbPaintHiLo: TCheckBox;
  cbPaintHiLoRange: TCheckBox;
  cbPaintLines: TCheckBox;
  cbPos: TComboBox;
  cbSize: TCheckBox;
  cbSys: TComboBox;
  cbTIR: TCheckBox;
  cbTirColorBgCustom: TRadioButton;
  cbTirColor: TRadioButton;
  cbTirColorCustom: TRadioButton;
  cbTitleColor: TCheckBox;
  cbTouch: TCheckBox;
  cbUser: TColorButton;
  cbUserColor: TCheckBox;
  cbFlash: TCheckGroup;
  cbFlashHi: TCheckBox;
  cbTirBar: TColorButton;
  cbTirBarCustom: TColorButton;
  cbChroma: TCheckBox;
  cl_hi_bg: TColorButton;
  cl_hi_bg_cust: TColorButton;
  cl_hi_txt: TColorButton;
  cl_hi_txt_cust: TColorButton;
  cl_lo_bg: TColorButton;
  cl_lo_bg_cust: TColorButton;
  cl_lo_txt: TColorButton;
  cl_lo_txt_cust: TColorButton;
  cl_ok_bg: TColorButton;
  cl_ok_txt: TColorButton;
  eAddr: TEdit;
  edURLHigh: TEdit;
  edURLLow: TEdit;
  edURLPerfect: TEdit;
  edNick: TEdit;

  ePass: TEdit;
  edMusicHigh: TEdit;
  edMusicLow: TEdit;
  edMusicPerfect: TEdit;
  fsHi: TFloatSpinEdit;
  fsHiRange: TFloatSpinEdit;
  fsLo: TFloatSpinEdit;
  fsLoRange: TFloatSpinEdit;
  gbDisplayPrefs: TGroupBox;
  gbMulti: TGroupBox;
  gbOverride: TGroupBox;
  gbAlertBehavior: TGroupBox;
  lAlertHiCol: TLabel;
  lAlertLoCol: TLabel;
  lAlertUrgCol: TLabel;
  lAlertDurRow: TLabel;
  lAlertHystRow: TLabel;
  gbMedia: TGroupBox;
  gbURL: TGroupBox;
  GroupBox1: TGroupBox;
  GroupBox2: TGroupBox;
  GroupBox3: TGroupBox;
  GroupBox4: TGroupBox;
  GroupBox5: TGroupBox;
  GroupBox6: TGroupBox;
  GroupBox7: TGroupBox;
  GroupBox8: TGroupBox;
  GroupBox9: TGroupBox;
  Image1: TImage;
  Label14: TLabel;
  Label18: TLabel;
  Label19: TLabel;
  Label20: TLabel;
  Label21: TLabel;
  Label22: TLabel;
  Label23: TLabel;
  Label24: TLabel;
  Label26: TLabel;
  Label27: TLabel;
  Label28: TLabel;
  Label29: TLabel;
  Label30: TLabel;
  Label31: TLabel;
  Label32: TLabel;
  lChromaHigh: TLabel;
  lChromaLow: TLabel;
  lbExtensions: TCheckListBox;
  bExtReload: TButton;
  lCopyright: TLabel;
  lExtCopyright: TLabel;
  lExtCount: TLabel;
  lArch: TLabel;
  lExtName: TLabel;
  lExtPerms: TLabel;
  lbChroma: TListBox;
  lHiOver2: TLabel;
  lOS: TLabel;
  lPredictShortMinutes: TLabel;
  lPredictShortSize: TLabel;
  lProblematic: TLabel;
  lTestAnnounce: TLabel;
  lTitle: TLabel;
  Label7: TLabel;
  Label8: TLabel;
  lHiOver1: TLabel;
  lLounder1: TLabel;

  Label3: TLabel;
  Label5: TLabel;
  lUserName: TLabel;
  lWaitSys: TLabel;
  Label10: TLabel;
  Label11: TLabel;
  Label12: TLabel;
  Label15: TLabel;
  lPass: TLabel;
  lUserTrack: TLabel;
  Label4: TLabel;
  Label6: TLabel;
  Label9: TLabel;
  lAck: TButton;
  lAgo: TLabel;
  lArrow: TLabel;
  lbUsers: TListBox;
  lHiOver: TLabel;
  lLicense: TButton;
  lLounder: TLabel;
  lDiff: TLabel;
  lVal: TLabel;
  lVersion: TLabel;
  lWarnPredict: TLabel;
  lWidgetset: TLabel;
  lWM: TLabel;
  Panel10: TPanel;
  Panel13: TPanel;
  Panel14: TPanel;
  Panel15: TPanel;
  Panel16: TPanel;
  Panel17: TPanel;
  pnSysInfo: TPanel;
  pcColors: TPageControl;
  Panel1: TPanel;
  Panel7: TPanel;
  Panel11: TPanel;
  Panel12: TPanel;
  Panel2: TPanel;
  Panel4: TPanel;
  Panel5: TPanel;
  Panel6: TPanel;
  Panel8: TPanel;
  Panel9: TPanel;
  pnBackend: TPanel;
  pnHelp: TPanel;

  pUserColor: TPanel;
  pcMain: TPageControl;
  pnDisplay: TPanel;
  pUserNick: TPanel;
  pUserSave2: TPanel;
  cbTirColorBg: TRadioButton;
  rbUnit: TRadioGroup;
  rbTrendWindow: TRadioGroup;
  seTIR: TSpinEdit;
  seTTSRate: TSpinEdit;
  spDeltaMax: TSpinEdit;
  spTHRESHOLD: TSpinEdit;
  spTHRESHOLD1: TSpinEdit;
  spAlertDurHi: TSpinEdit;
  spAlertDurLo: TSpinEdit;
  spAlertDurUrg: TSpinEdit;
  fsAlertHystHi: TFloatSpinEdit;
  fsAlertHystLo: TFloatSpinEdit;
  fsAlertHystUrg: TFloatSpinEdit;
  tsAccess: TTabSheet;
  tsProxy: TTabSheet;
  tsCommon: TTabSheet;
  tsAdvanced: TTabSheet;
  tsPredictions: TTabSheet;
  tsTir: TTabSheet;
  tsChroma: TTabSheet;
  tsExt: TTabSheet;
  tsBackgrounds: TTabSheet;
  tsTexts: TTabSheet;
  tsCustomRange: TTabSheet;
  tsColors: TTabSheet;
  tsCustom: TTabSheet;
  tsDisplay: TTabSheet;
  tsGeneral: TTabSheet;
  tsIntegration: TTabSheet;
  tsMulti: TTabSheet;
  tsSystem: TTabSheet;
  procedure bConnectHelpClick(Sender: TObject);
  procedure bDecimalHelpClick(Sender: TObject);
  procedure bDiffRightHelpClick(Sender: TObject);
  procedure bDisableMediaHelpClick(Sender: TObject);
  procedure bDisplayAdvancedHelpClick(Sender: TObject);
  procedure bDisplayGeneralHelpClick(Sender: TObject);
  procedure bDisplayWindowHelpClick(Sender: TObject);
  procedure bFullArrowSetHelpClick(Sender: TObject);
  procedure bRotatingArrowHelpClick(Sender: TObject);
  procedure bPredScaleHelpClick(Sender: TObject);
  procedure bScaleAgoClick(Sender: TObject);
  procedure bScaleHelpClick(Sender: TObject);
  procedure bShortModeHelpClick(Sender: TObject);
  procedure bWarnHiLowHelpClick(Sender: TObject);
  procedure cbMediaDisableChange(Sender: TObject);
  function validateUser(var error: string): boolean;
  procedure bAddClick({%H-}Sender: TObject);
  procedure bBadgeFlashHelpClick({%H-}Sender: TObject);
  procedure bColorGraphHelpClick({%H-}Sender: TObject);
  procedure bCommonClick({%H-}Sender: TObject);
  procedure bCustomRangeHelpClick({%H-}Sender: TObject);
  procedure bExtOpenClick({%H-}Sender: TObject);
  procedure bExtReloadClick({%H-}Sender: TObject);
  procedure bFontReadingClick(Sender: TObject);
  procedure bFontArrowClick(Sender: TObject);
  procedure bFontTimeClick(Sender: TObject);
  procedure bFontResetClick(Sender: TObject);
  procedure bLanguageHelpClick(Sender: TObject);
  procedure bLimitsClick(Sender: TObject);
  procedure bAlertDurHelpClick({%H-}Sender: TObject);
  procedure bAlertHystHelpClick({%H-}Sender: TObject);
  procedure bMinMinutesHelpClick(Sender: TObject);
  procedure bMultiUserHelpClick(Sender: TObject);
  procedure bNotificationHelpClick(Sender: TObject);
  procedure bDeltaMaxHelpClick(Sender: TObject);
  procedure bOutdatedHelpClick(Sender: TObject);
  procedure bOverrideHelpClick({%H-}Sender: TObject);
  procedure bPredictHelpClick({%H-}Sender: TObject);
  procedure bPredictHorizonClick({%H-}Sender: TObject);
  procedure bPrivacyHelpClick({%H-}Sender: TObject);
  procedure bRemoveClick({%H-}Sender: TObject);
  procedure bBackendHelpClick({%H-}Sender: TObject);
  procedure bSysNoticeClick({%H-}Sender: TObject);
  procedure bSysTouchClick({%H-}Sender: TObject);
  procedure bTestAnnounceClick(Sender: TObject);
  procedure bTestClick(Sender: TObject);
  procedure bLoginClick(Sender: TObject);
  procedure bTestProxyClick(Sender: TObject);
  procedure bTestSpeechClick(Sender: TObject);
  procedure cbTTSChange(Sender: TObject);
  procedure PopulateTTSVoices;
  procedure bThreasholdLinesHelpClick(Sender: TObject);
  procedure bTimeStampHelpClick(Sender: TObject);
  procedure bExportSettingsClick(Sender: TObject);
  procedure bImportSettingsClick(Sender: TObject);
  procedure btResetClick(Sender: TObject);
  procedure btUserSaveClick(Sender: TObject);
  procedure bUseURLHelpClick(Sender: TObject);
  procedure Button1Click(Sender: TObject);
  procedure Button3Click(Sender: TObject);
  procedure bWebAPIClick(Sender: TObject);
  procedure cbCust1Change(Sender: TObject);
  procedure cbCustChange(Sender: TObject);
  procedure cbCustRangeChange(Sender: TObject);
  procedure cbPosChange(Sender: TObject);
  procedure cbPredictionsChange(Sender: TObject);
  procedure cbPredictShortChange(Sender: TObject);
  procedure cbPredictShortFullArrowsChange(Sender: TObject);
  procedure cbPredictShortMinutesChange(Sender: TObject);
  procedure cbPredictShortSizeChange(Sender: TObject);
  procedure cbSysChange({%H-}Sender: TObject);
  procedure cbUserClick({%H-}Sender: TObject);
  procedure cbUserColorChanged({%H-}Sender: TObject);
  procedure edCommaSep1Change(Sender: TObject);
  procedure edCommaSepChange(Sender: TObject);
  procedure edNickChange(Sender: TObject);
  procedure ePassEnter(Sender: TObject);
  procedure ePassExit(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure ProxyEditChange({%H-}Sender: TObject);
  procedure FormCreate({%H-}Sender: TObject);
  procedure FormDestroy({%H-}Sender: TObject);
  procedure FormResize({%H-}Sender: TObject);
  procedure fsHi1Change({%H-}Sender: TObject);
  procedure fsHiChange({%H-}Sender: TObject);
  procedure fsLo1Change({%H-}Sender: TObject);
  procedure fsLoChange({%H-}Sender: TObject);
  procedure Label12Click(Sender: TObject);
  procedure lAckClick(Sender: TObject);
  procedure lbExtensionsClickCheck(Sender: TObject);
  procedure lbExtensionsDblClick(Sender: TObject);
  procedure lbExtensionsSelectionChange(Sender: TObject; User: boolean);
  procedure lbUsersEnter(Sender: TObject);
  procedure lbUsersSelectionChange(Sender: TObject; User: boolean);
  procedure lConfigPredictClick({%H-}Sender: TObject);
  procedure lLicenseClick({%H-}Sender: TObject);
  procedure lSysWarnInfoClick({%H-}Sender: TObject);
  procedure lValClick({%H-}Sender: TObject);
  procedure pcColorsChange({%H-}Sender: TObject);
  procedure pcMainChange({%H-}Sender: TObject);
  procedure rbUnitClick({%H-}Sender: TObject);
  procedure spTHRESHOLD1Change({%H-}Sender: TObject);
  procedure spTHRESHOLDChange({%H-}Sender: TObject);
  procedure tbAdvancedChange({%H-}Sender: TObject);
  procedure ToggleBox1Change(Sender: TObject);
  procedure tsAccessShow({%H-}Sender: TObject);
  procedure tsChromaShow({%H-}Sender: TObject);
  procedure tsCommonShow(Sender: TObject);
  procedure tsDisplayShow(Sender: TObject);
  procedure tsExtShow({%H-}Sender: TObject);
  procedure tsProxyShow(Sender: TObject);
  procedure tsSystemShow(Sender: TObject);
  procedure closeClick(Sender: TObject);
private
  FProxyLoading: boolean;
  FTTSVoicesLoaded: boolean;
  FChromaListLoaded: boolean;
  {** Extensions folder stashed by DeferExtensionList; scanned on the
      Extensions tab's first OnShow, then cleared. }
  FExtDeferredPath: string;
  FExtPaths: TStringList;
  FOnReloadExtensions: TNotifyEvent;
  procedure LoadProxySettingsIntoUI;
  procedure SaveProxySettingsFromUI;
  procedure getAPILabels(out user, pass: string);
  {** Metaclass of the backend currently selected in cbSys, or nil. }
  function selectedAPIClass: TrndiAPIClass;
  {** Show/hide the browser-login button and username field for the current
      backend, based on selectedAPIClass.supportsWebLogin. }
  procedure updateWebLoginUI;
public
  chroma: TRazerChromaBase;
  {** Saved TTS voice selection, applied by EnsureTTSVoices once the voice
      list has actually been enumerated (deferred to the Accessibility tab). }
  pendingTTSVoiceIndex: integer;
  pendingTTSVoiceName: string;
  procedure EnsureTTSVoices;
  procedure PopulateChromaDevices;
  procedure UpdatePredictionStates;
  procedure LoadExtensionList(const ExtensionsPath: string);
  procedure DeferExtensionList(const ExtensionsPath: string);
  {** True once the TTS voice list has been enumerated; while false the
      cbTTSVoice selection is not meaningful and must not be persisted. }
  property TTSVoicesLoaded: boolean read FTTSVoicesLoaded;
  property OnReloadExtensions: TNotifyEvent read FOnReloadExtensions
    write FOnReloadExtensions;
end;

var
tnative: TrndiNative;

resourcestring
RS_ALERT_HYSTERESIS_MGDL = 'Clear margin (mg/dL)';
RS_ALERT_HYSTERESIS_MMOL = 'Clear margin (mmol/L)';
RS_MUST_PERSIST_HELP = 'An alert will only trigger if the condition has been true continuously for this many minutes. Set to 0 to trigger immediately.';
RS_CLEAR_MARGIN_HELP = 'Once an alert triggers, the reading must recover this many units past the threshold before the alert can fire again.';
RS_EMPTY_PROXY = 'Proxy host is empty.';

RS_EXPORT_TITLE = 'Export Settings';
RS_IMPORT_TITLE = 'Import Settings';
RS_SETTINGS_FILTER = 'Settings files (*.trndi)|*.trndi|All files (*.*)|*.*';
RS_EXPORT_EMPTY = 'No settings to export.';
RS_EXPORT_OK = 'Settings exported successfully.';
RS_IMPORT_EMPTY = 'The selected file is empty.';
RS_IMPORT_OK = 'Settings imported successfully. You need to restart Trndi for all changes to take effect. Do NOT save when exiting the settings dialog!';
RS_IMPORT_FAIL = 'Error importing settings: %s';

RS_DRIVER_CONTRIBUTOR = 'Driver contributor: ';

RS_DEBUG_BACKEND_LABEL = '(Ignored for debug backend)';

RS_Display_General_Help = '- A progressbar can show the time until the next update on the left hand side of the screen'+LineEnding+
'- The time can we displayed every 20 seconds, instead of the Reading'+LineEnding+
'- The reading and trend arrow can alternate positions';

RS_DISPLAY_ADVANCED_HELP = 'When supported by the backend, the sensor expiration time can be shown in Trndi';

RS_DISPLAY_WINDOW_HELP = '- The background color can be set to the current glucose range color'+LineEnding+
'- The window can stay over other windows'+LineEnding+
'- The window''s borders can be hidden (may depend on operating system)';

RS_Multi_User_Help =
  'Trndi supports <b>more than one user</b>, this is called the <i>multi user mode</i>.'+sHTMLLineBreak+'In this section you can add/remove accounts. There''s <i>always</i> a default account which cannot be deleted.'+LineEnding+
  'Accounts have their own settings and remote servers, and are <i>"sandboxed"</i>.'+sHTMLLineBreak+sHTMLLineBreak+'Start a new instance of Trndi to log in to a given user account.';

RS_Use_URL_Help =
  'Trndi can load a URL when high or low in your default web browser.';

RS_Custom_Range_Help =
  'Override/set your own limits for the custom range. This is your personal ideal goal, within the high/low area.';

RS_Threashold_Lines_Help =
  'Trndi can draw a line on the graph''s background, to represent where your high/low limits are.';

RS_BADGE_FLASH_HELP =
  'This controls how Trndi''s icon in the status bar and/or tray will notify you visually.';

RS_Warn_HiLow =
  'This will show an overlay on the screen when a low or high is predicted';

RS_SHORT_MODE_HELP =
  'Only one reading will show, as just an arrow or a single prediction';

RS_ROTATING_ARROW_HELP =
  'Instead of the fixed up/steady/down arrows, the trend arrow is rotated smoothly to match how fast your glucose is changing: nearly flat for small changes and leaning further down (or up) the faster it moves.';

RS_NOTIFICATION_HELP =
  'Trndi will show a system notification (if available) when your reading is in a dangerous state. Alerts will trigger once when hi/low and every 15 minutes when readings are missing.';

RS_COLOR_BG =
  'Trndi can draw a rectangle/background in the graph''s background, to visually represent your high and low limits.';

RS_OVERRIDE_LANGUAGE =
  'Choose which language to run Trndi in. Should you choose the wrong one, you can press the Windows/"meta" button at start to force English!';

RS_FONT_HELP =
  'You can change the font of the reading, time and arrow. Click on the respective item here to change the font';

RS_OVERRIDE_HELP =
  'Setting values here allows you to define your own high and low blood sugar limits in Trndi.'
  + #10 + #10 + 'NightScout:'#10 +
  'Trndi automatically retrieves your custom high and low settings from NightScout, so manually setting them here is usually unnecessary.'
  + #10 + #10 + 'Dexcom:' + #10 +
  'Dexcom servers do not provide custom high and low values. By setting them here, you can establish your own thresholds for Dexcom data.';
RS_OVERRIDE_NS =
  'You are using the NightScout backend, you should set these values on your server (if possible), as Trndi uses your NightScout preferences by default';

RS_PRIVACY_HELP =
  'When in Privacy Mode, the actual blood glucose value is hidden. Trndi will only tell the user if it''s good, high or low.';

RS_PREDICTION_HORIZON =
  'The prediction horizon is how far in the future Trndi will show predictions, when just showing one. This is not an exact time, it may vary, it''s just a target time.';

RS_PREDICTION_HELP =
  'Trndi can predict/guess what your reading will be in the future. This will be shown in the lower right part of the screen.';

RS_OUTDATED_HELP =
  'This is the time after which Trndi will show the "no recent" readings overlay. Trndi checks this when updating from the remote server, not every minute.';

RS_DELTA_MAX =
  'When the previous reading is missing, use the next available reading within this many 5-minute-intervals to calculate delta.';

RS_TIMESTAMP_HELP =
  'By default Trndi shows the amount of minutes since the last reading, this setting allows you to display the time of the last reading instead.';

RS_DEX =
  'Dexcom servers do not provide custom high and low blood sugar values.'+sLineBreak+'Please set your own thresholds in the Customization tab.';

RS_BETA =
  'This backend is in a beta stage, it may not work as intended!'+sLineBreak+' If possible, choose another backend.';

RS_BETA_DEX =
  'This is a new Deccom backend, it''s in a beta stage! If possible, choose the old backend.'+sLineBreak+'Please set your own thresholds in the Customization tab.';

RS_XDRIP =
  'Make sure you are on the same network as the xDrip app.'+sLineBreak+'Make sure that web access is turned on.';

RS_TANDEM =
  'This backend is in alpha stage, it may not work as intended!'+sLineBreak+'Please set your own thresholds in the Customization tab.';

RS_CARELINK =
  'This backend is experimental!'+sLineBreak+'It needs token data captured from a one-time browser login — see the CareLink guide.'+sLineBreak+'Use a Care Partner (follower) account. Data may lag behind the pump.';

RS_DEBUG_WARN =
  'This is a debug backend. It''s used for testing purposes only!'+sLineBreak+'No data will be sent to any remote server.';

RS_DISABLE_MEDIA = 'This turns off all media-related features. Can speed up start.';

RS_SCALE_DIFF_HELP = 'You can change the scale/size of the difference in reading since last, by entering a scale factor';

RS_PRED_SCALE_HELP = 'When predictions are enabled, this controls the size adjustment of the predictions';

RS_CONNECT_HELP = 'This will display a badge showing the internet connectivity status in the top.';

RS_DIFF_RIGHT_HELP = 'This controls if the difference/delta will show center or right (when predictions are on, this will swap the position of the values)';

RS_Full_Arrow_Set_Help = 'This will display all prediction arrows, by default Trndi just shows going up, down or steady';

RS_DECIMAL_HELP = 'This replaces the decimal separator, eg setting "," will show the reading as 5,5 instead of 5.5';

RS_ERR_PASSWORD = 'You must enter a password';
RS_ERR_EMAIL = 'You must enter a valid e-mail address';
RS_ERR_ADDRESS = 'Address must start with http(s)://';
RS_ERR_CARELINK_TOKEN = 'The credential must be the captured CareLink token data (JSON, starting with "{")';
RS_ENTER_USER = 'Enter a username';
RS_ENTER_NAME = 'Letters, space and numbers only';
RS_ENTER_ANY = 'Please enter a name';
RS_DUPE_NAME = 'Names must be unique';
RS_CURRENT_ACC = 'This only applies for the current loaded account:  %s';
RS_CURRENT_ACC_NO = 'These settings are available when using a multi-account';
RS_CURRENT_ACC_DEF = 'Settings for "default" only apply while multi-user is active';
RS_REMOVE_ACC =
  'Removed accounts are made inactive, and can be restored by adding the same name again';
RS_AUTO = 'Auto-detect';
RS_UPTODATE = 'You are up to date';
RS_NEWVER = 'Version %s is available, would you like to go to the downloads page? You can also ignore this warning for 2 weeks.';
RS_NEWVER_PR = 'This temporary Trndi build is now outdated, a newer stable version has been released: %s. Would you like to go to the downloads page?';
RS_NEWVER_PRE =
  'A new pre-release for %s is available, would you like to go to the downloads page?';
RS_NEWVER_CAPTION = 'New version available';
RS_UPTODATE_DEV = 'You are using a development version, there''s no stable release newer than this! The most recent stable release is %s';
RS_UPTODATE_PR = 'You are using a newer temporary Trndi build, the latest stable release is: %s';
RS_SELECT_FONT = 'Select a font';
RS_SELECT_FONT_DESC = 'Choose a font to use';
RS_SELECT_FONT_ARROW = 'Select a font for the arrow';
RS_SELECT_FONT_READING = 'Select a font for the reading';
RS_SELECT_FONT_TIME = 'Select a font for the time';
RS_UPDATE_SNOOZE = 'You will be alerted again after %s';

RS_MIN_MINUTES = 'When calculating time-in-range, do so over this amount of minutes. If the number is higher than the data available, the max time will be used - this is currently %d minutes.';

RS_NOTIFICATIONS = 'Notifications';
RS_NOTIFY_TITLE = 'A notification system is required';
RS_NOTIFY_TXT =
  'Trndi uses a system called "%s" to send desktop notices, you need to have this system installed in order to receive notices.';
RS_NOTIFY_SYSTEM =
  'Notifications will appear where you normally get notification messages.';

RS_HASTOUCH = 'Shows if Trndi detected a touch screen';
RS_WEBAPI = 'Trndi can expose a <b>WebAPI</b> for use with <i>third-party systems</i>.<br><br>This is an advanced feature, you can safely disregard it if you don''t know what it does!';

RS_Saftey_Hi =
  'Trndi won''t allow a larger limit, for your own safety. This can be overridden manually/via plugin';
RS_Saftey_Low =
  'Trndi won''t allow a lower limit, the backend system only reports values every 5 minutes. 6 = one reading missing.';

RS_DEFAULT_ACCOUNT = 'Default';
RS_CHOOSE_SYSTEM = 'Select your CGM system in the list, then enter your credentials';

RS_WAYLAND =
  'Your desktop session is Wayland. Wayland compositors (including KWin) restrict third-party apps from forcing windows above others.' +
  LineEnding + 'Right-click the title bar, select more options and check the always-on-top option (or similar)';

RS_PredictionWarn = 'Trndi will try to predict future readings. This is experimental and potentially unsafe! Do not make any treatment decisions based on these values! This feature is used on your own risk and responsibility!';

RS_ExtCount = 'Extensions count: %d';

RS_NO_COPYRIGHT = '- Extension has no copyright info -';

RS_EXT_REQUESTS = 'Requests: ';
RS_EXT_GRANTED = 'Granted: ';
RS_EXT_NOT_APPROVED = ' (not yet approved)';
RS_EXT_CHANGED = ' (changed since approval — Trndi will ask again)';

RS_TEST_UNSUPPORTED = 'Sorry! Trndi does not (yet) support connection testing for this service!';
RS_TEST_SUCCESS = 'Successfully connected!';
RS_TEST_FAIL = 'Could not connect!';

RS_WEBLOGIN_BUTTON = 'Get CareLink token…';
RS_WEBLOGIN_TITLE = 'CareLink login helper';
RS_WEBLOGIN_HELP =
  'CareLink needs a one-time browser login (with CAPTCHA), so Trndi uses a small ' +
  'login helper to capture your token.'+ sHTMLLineBreak + sHTMLLineBreak +
  'It needs Node.js installed. In a terminal, run:' + sHTMLLineBreak + sHTMLLineBreak +
  '> cd "%s"'+ sHTMLLineBreak +
  '> npm install'+ sHTMLLineBreak +
  '> %s'+ sHTMLLineBreak+ sHTMLLineBreak +
  'A browser opens — sign in with your Care Partner account and solve the CAPTCHA. ' +
  'The helper then prints a block of JSON: copy it into the token field below and click Test.'+ sHTMLLineBreak+ sHTMLLineBreak +
  'Open the helper folder now?';

RS_WEBLOGIN_RUN_TITLE = 'CareLink login';
RS_WEBLOGIN_RUN_PROMPT =
  'Trndi will open a browser window for you to sign in to CareLink (with CAPTCHA), ' +
  'then capture the token/login information automatically.' + sHTMLLineBreak + sHTMLLineBreak +
  'The first run also downloads the login helper''s dependencies, which can take a ' +
  'minute. Keep this window open and complete the sign-in in the browser.' + sHTMLLineBreak + sHTMLLineBreak +
  'Start now?';
RS_WEBLOGIN_BTN_BUSY = 'Signing in…';
RS_WEBLOGIN_INSTALLING = 'Installing login helper dependencies (first run only)…';
RS_WEBLOGIN_WAITING = 'Waiting for the browser sign-in to complete…';
RS_WEBLOGIN_OK = 'Login captured. Click Test to verify, then Save.';
RS_WEBLOGIN_NONODE =
  'Node.js was not found on this system, so Trndi cannot run the login helper for you. ' +
  'Install Node.js (22.12+) — or run the helper manually.';
RS_WEBLOGIN_NOSCRIPT =
  'The bundled CareLink login helper was not found next to Trndi. ' +
  'You can still run it manually.';
RS_WEBLOGIN_NPM_FAIL = 'Could not install the login helper''s dependencies (npm install failed).';
RS_WEBLOGIN_NO_OUTPUT = 'The login helper did not return any token data. The sign-in may have been cancelled or timed out.';
RS_WEBLOGIN_BAD_OUTPUT = 'The login helper did not return valid token data.';
RS_WEBLOGIN_FAIL_TITLE = 'Automatic login unavailable';

RS_POS_TITLE = 'Restricted Feature';
RS_POS = 'This feature depends on your Window Manager (WM). %s may not support this feature.';
RS_POS_UNKNOWN = 'This feature depends on your Window Manager. It may not support this feature.';

RS_SHORTMODE_FULL = 'By default, Trndi only shows up/down/straight. You can enable the full range of arrows here - however you must realize that this is very experimental and potentially less accurate!';

RS_NO_EXTENSIONS = 'This version of Trndi does not support extensions';
RS_NO_EXTENSIONS_COPYRIGHT = 'Please download a version of Trndi that supports extensions';
RS_NO_EXTENSIONS_SYSTEM = 'Due to hardware limitations, %s on %s cannot support extensions';

RS_EXT_RELOAD_TITLE = 'Reload Extensions';
RS_EXT_RELOAD_DONE = 'Extensions were reloaded.';

RS_Announce_Not_Available = 'The text-to-speech (TTS) software "%s" is not available.';

var
fConf: TfConf;

procedure ListLanguageFiles(list: TStrings; const Path: string);
function GetLanguageName(const ACode: string): string;
function ExtractLangCode(const AText: string): string;

implementation

{$R *.lfm}

{$I ../../inc/languages.inc }

procedure ShowMessage(const str: string);
begin
  UXMessage(sSuccTitle, str, uxmtInformation);
end;

procedure ShowHTMLMessage(const html: string);
begin
  ExtHTML(uxdAuto,'',html);
end;

function ExtractLangCode(const AText: string): string;
var
  L, R: integer;
begin
  L := LastDelimiter('(', AText);
  R := LastDelimiter(')', AText);
  if (L > 0) and (R > L) then
    Result := Copy(AText, L + 1, R - L - 1)
  else
    Result := '';
end;


procedure ListLanguageFiles(list: TStrings; const Path: string);
var
  sr: TSearchRec;
begin
  list.Clear;
  // Search for all .po files in the specified directory
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*.po',
    faAnyFile, sr) = 0 then
  begin
    repeat
      // Skip subdirectories
      if (sr.Attr and faDirectory) = 0 then
        // Extract language code without ".po"
        list.Add(ChangeFileExt(sr.Name, ''));
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TfConf.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfConf.lAckClick(Sender: TObject);
const
  txt = 'Trndi makes use of the following 3rd party libraries:' + sHTMLLineBreak +
    'macOS native code libraries by <i>Phil Hess</i>.'#10 + sHTMLLineBreak +
    'Windows DirectX headers by <i>CMC Development Team</i>.'#10 + sHTMLLineBreak + sHTMLLineBreak +
    'Extensions use the JavaScript engine <i>QuickJS</i> by <i>Fabrice Bellard</i> and <i>Charlie Gordo</i>.'#10 + sHTMLLineBreak +
    'Integration of said engine is made possible with mORMot2 by Synopse Informatique - <i>Arnaud Bouchez</i>.' + sHTMLLineBreak + sHTMLLineBreak +

    'Haiku specific: OpenSSL' + sHTMLLineBreak +
    'Linux/BSD specific: Qt6 and libCurl (usually with OpenSSL)' + sHTMLLineBreak + sHTMLLineBreak +

    'While Trndi has been built ground-up, it has been inspired by the Python library <i>pydexcom</i> and Tandem tool <i>tconnectsync</i>, aswell as the <i>NightScout</i> project.' + sHTMLLineBreak + sHTMLLineBreak +
    'Built in <b>Object Pascal</b>, using the <b>Lazarus</b> component library (LCL) and <b>FreePascal</b>.' + sHTMLLineBreak + sHTMLLineBreak +
    'Follow Trndi on Discord and Github! Contributions of code and translations are very welcome!';
begin
  ExtHTML(uxdAuto, 'Trndi', txt,[mbOK],uxmtInformation,25);
end;

procedure TfConf.lbExtensionsSelectionChange(Sender: TObject; User: boolean);
{$ifdef TrndiExt}
var
  fileStream: TFileStream;
  ss: TStringStream;
  path, scriptText, permsCsv, extId, storedHash: string;
  manifest: TExtManifest;
  granted: TExtPermSet;
{$endif}
begin
  {$ifdef TrndiExt}
  if (FExtPaths = nil) or (lbExtensions.ItemIndex < 0) or
     (lbExtensions.ItemIndex >= FExtPaths.Count) then
    Exit;

  lExtName.Caption := '';
  lExtCopyright.Caption := '';
  lExtPerms.Caption := '';

  path := FExtPaths[lbExtensions.ItemIndex];
  scriptText := '';
  // Raw read (no TStringList line-ending normalization) so HashScript below
  // produces the same digest as the load-time grant flow in umain_ext.inc.
  try
    fileStream := TFileStream.Create(path, fmOpenRead or fmShareDenyWrite);
    ss := TStringStream.Create;
    try
      ss.CopyFrom(fileStream, fileStream.Size);
      scriptText := ss.DataString;
    finally
      ss.Free;
      fileStream.Free;
    end;
  except
    // unreadable: treat as empty so the no-copyright fallback fires
  end;

  manifest := ParseExtManifest(scriptText);

  if manifest.DisplayName <> '' then
    lExtName.Caption := manifest.DisplayName
  else
    lExtName.Caption := ExtractFileName(path);

  if manifest.Author <> '' then
    lExtCopyright.Caption := manifest.Author
  else
    lExtCopyright.Caption := RS_NO_COPYRIGHT;

  // Grant status for the promptable groups. Grants are keyed to the file's
  // hash (ext.perm.<id>.hash/.granted), so an edited file shows as pending.
  permsCsv := PermSetToCSV(manifest.Requested * PermPromptable);
  if permsCsv <> '' then
  begin
    extId := ExtIdFromPath(path);
    storedHash := tnative.GetSetting('ext.perm.' + extId + '.hash', '');
    if (storedHash <> '') and (storedHash = HashScript(scriptText)) then
    begin
      granted := CSVToPermSet(
        tnative.GetSetting('ext.perm.' + extId + '.granted', '')) * PermPromptable;
      if granted <> [] then
        lExtPerms.Caption := RS_EXT_GRANTED + PermSetToCSV(granted)
      else
        lExtPerms.Caption := RS_EXT_REQUESTS + permsCsv + RS_EXT_NOT_APPROVED;
    end
    else if storedHash <> '' then
      lExtPerms.Caption := RS_EXT_REQUESTS + permsCsv + RS_EXT_CHANGED
    else
      lExtPerms.Caption := RS_EXT_REQUESTS + permsCsv + RS_EXT_NOT_APPROVED;
  end;
  {$endif}
end;

procedure TfConf.lbExtensionsDblClick(Sender: TObject);
{$ifdef TrndiExt}
var
  path: string;
{$endif}
begin
  {$ifdef TrndiExt}
  if (FExtPaths = nil) or (lbExtensions.ItemIndex < 0) or
     (lbExtensions.ItemIndex >= FExtPaths.Count) then
    Exit;

  path := FExtPaths[lbExtensions.ItemIndex];
  if FileExists(path) then
    OpenDocument(path);
  {$endif}
end;

procedure TfConf.LoadExtensionList(const ExtensionsPath: string);
{$ifdef TrndiExt}
var
  extFiles, scriptBuf: TStringList;
  scriptPath, displayName, extId: string;
  manifest: TExtManifest;
  i, addedIdx: integer;
{$endif}
begin
  {$ifdef TrndiExt}
  if FExtPaths = nil then
    FExtPaths := TStringList.Create;
  FExtPaths.Clear;
  lbExtensions.Clear;
  lExtName.Caption := '';
  lExtCopyright.Caption := '';
  lExtPerms.Caption := '';

  extFiles := FindAllFiles(ExtensionsPath, '*.js', false);
  try
    scriptBuf := TStringList.Create;
    try
      for i := 0 to extFiles.Count - 1 do
      begin
        scriptPath := extFiles[i];
        manifest.DisplayName := '';
        manifest.Author := '';
        manifest.Requested := [];
        try
          scriptBuf.LoadFromFile(scriptPath);
          manifest := ParseExtManifest(scriptBuf.Text);
        except
        end;
        if manifest.DisplayName <> '' then
          displayName := manifest.DisplayName
        else
          displayName := ExtractFileName(scriptPath);
        addedIdx := lbExtensions.Items.Add(displayName);
        FExtPaths.Add(scriptPath);
        extId := ExtIdFromPath(scriptPath);
        // Default true so the box is checked for extensions that have never
        // been toggled — matches the load-skip default in inc/umain_ext.inc.
        lbExtensions.Checked[addedIdx] :=
          tnative.GetBoolSetting('ext.enabled.' + extId, true);
      end;
    finally
      scriptBuf.Free;
    end;
  finally
    extFiles.Free;
  end;
  lExtCount.Caption := Format(RS_ExtCount, [lbExtensions.Count]);
  {$endif}
end;

{------------------------------------------------------------------------------
  Stash the extensions folder so the directory scan + manifest parse of every
  extension file runs when the Extensions tab is first shown, not while the
  dialog is opening. The reload button still calls LoadExtensionList directly.
------------------------------------------------------------------------------}
procedure TfConf.DeferExtensionList(const ExtensionsPath: string);
begin
  FExtDeferredPath := ExtensionsPath;
end;

procedure TfConf.tsExtShow(Sender: TObject);
begin
  if FExtDeferredPath = '' then
    Exit;
  LoadExtensionList(FExtDeferredPath);
  FExtDeferredPath := '';
end;

procedure TfConf.lbUsersEnter(Sender: TObject);
begin

end;

procedure TfConf.lbUsersSelectionChange(Sender: TObject; User: boolean);
var
  u: string;
begin
  btUserSave.Enabled := false;
  if lbUsers.ItemIndex < 0 then
  begin
    gbMulti.Enabled := false;
    Exit;
  end;

  u := lbUsers.Items[lbusers.ItemIndex];

  if u[1] = '-' then
  begin
    tNative.configUser := '';
    bRemove.Enabled := false;
  end
  else
  begin
    tNative.configUser := u;
    bRemove.Enabled := true;
  end;

  cbUser.ButtonColor := tNative.GetColorSetting('user.color', clBlack);
  edNick.Text := tNative.GetSetting('user.nick', '');
  lUserName.Caption := u;

  gbMulti.Enabled := true;
  btUserSave.Enabled := false; // Twice as the fields change during update
end;

procedure TfConf.lConfigPredictClick(Sender: TObject);
begin
  pcMain.ActivePage := tsPredictions;
end;

procedure TfConf.getAPILabels(out user, pass: string);
var
  sys: TrndiAPIClass;
begin
  sys := BackendClassOf(cbSys.Text);
  if sys = nil then
    sys := TrndiAPI;

  user := sys.ParamLabel(APLUser);
  pass := sys.ParamLabel(APLPass);

  {$ifdef DEBUG}
  // Debug backends that don't provide their own labels get a generic hint
  if (cbSys.Text in API_DEBUG) and (user = TrndiAPI.ParamLabel(APLUser)) then
  begin
    user := RS_DEBUG_BACKEND_LABEL;
    pass := RS_DEBUG_BACKEND_LABEL;
  end;
  {$endif}
end;

procedure TfConf.cbSysChange({%H-}Sender: TObject);
var user, pass: string;
procedure WarnUnstableAPI;
  const
    warn: string = '(!) ';
    info: string =  '';
  begin
    pnSysWarn.Color := $0000FBF4;
    case cbSys.Text of
    API_DEX_USA,
    API_DEX_EU:
    begin
      gbOverride.Color := uxclLightBlue;
      lLoUnder.Font.Color := clBlack;
      lHiOver.Font.Color := clBlack;
      cbCust.Font.Color := clBlack;

      pnSysWarn.Show;
      lSysWarnInfo.Caption := info + RS_DEX;
      pnSysWarn.Color := $0053A2E8;
    end;
    API_NS3:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := warn + RS_BETA;
    end;
    API_DEX_NEW_EU,
    API_DEX_NEW_USA,
    API_DEX_NEW_JP:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := warn + RS_BETA_DEX;
    end;
    API_XDRIP:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := info + RS_XDRIP;
    end;
    API_TANDEM_EU,
    API_TANDEM_USA:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := warn + RS_TANDEM;
    end;
    API_CARELINK_US,
    API_CARELINK_EU:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := warn + RS_CARELINK;
    end;
    end;
    {$ifdef DEBUG}
    if cbSys.Text in API_DEBUG then
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := warn + RS_DEBUG_WARN;
    end;
    {$endif}
  end;

begin
  pnSysWarn.Hide;
  gbOverride.Color := clDefault;
  //if not (sender is TfConf) then
  WarnUnstableAPI;
  // Update parameter labels above edits based on backend
  getAPILabels(user, pass);
  label15.caption := user;
  lPass.Caption := pass;
  updateWebLoginUI;
end;

procedure TfConf.cbUserClick(Sender: TObject);
begin

end;

procedure TfConf.cbUserColorChanged(Sender: TObject);
begin
  btUserSave.Enabled := true;
end;

procedure TfConf.edCommaSep1Change(Sender: TObject);
begin
  edCommaSep.Text := edCommaSep1.Text;
end;

procedure TfConf.edCommaSepChange(Sender: TObject);
begin
  edCommaSep1.Text := edCommaSep.Text;
end;

procedure TfConf.edNickChange(Sender: TObject);
begin
  btUserSave.Enabled := true;
end;

procedure TfConf.ePassEnter({%H-}Sender: TObject);
begin
  ePass.PasswordChar := #0;
end;

procedure TfConf.ePassExit({%H-}Sender: TObject);
begin
  ePass.PasswordChar := '*';
end;

procedure TfConf.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  err: string;
begin
  if not validateUser(err) then
  begin
    SHowMessage(err);
    closeaction := caNone;
  end;
end;

procedure TfConf.bLimitsClick({%H-}Sender: TObject);
begin
end;

procedure TfConf.bAlertDurHelpClick({%H-}Sender: TObject);
begin
  ShowMessage(RS_MUST_PERSIST_HELP);
end;

procedure TfConf.bAlertHystHelpClick({%H-}Sender: TObject);
begin
  ShowMessage(RS_CLEAR_MARGIN_HELP);
end;

procedure TfConf.bMinMinutesHelpClick({%H-}Sender: TObject);
begin
  ShowMessage(Format(RS_MIN_MINUTES, [MAX_MIN]));
end;

procedure TfConf.bMultiUserHelpClick({%H-}Sender: TObject);
begin
  ShowHTMLMessage(RS_Multi_User_Help);
end;

procedure TfConf.bNotificationHelpClick({%H-}Sender: TObject);
begin
  ShowMessage(RS_NOTIFICATION_HELP);
end;

procedure TfConf.bDeltaMaxHelpClick({%H-}Sender: TObject);
begin
  if UXDialog(uxdAuto,'Delta', RS_DELTA_MAX,[mbOK, mbUxRead]) <> mrOK then
    OpenURL('https://github.com/slicke/trndi/blob/main/doc/DeltaMax.md');
end;

procedure TfConf.bOutdatedHelpClick({%H-}Sender: TObject);
begin
  ShowMessage(RS_OUTDATED_HELP);
end;

function TfConf.validateUser(var error: string): boolean;
begin
  case CheckBackendCredentials(cbSys.Text, eAddr.Text, ePass.Text) of
  bceAddress:
    error := RS_ERR_ADDRESS;
  bceEmail:
    error := RS_ERR_EMAIL;
  bcePassword:
    error := RS_ERR_PASSWORD;
  bceToken:
    error := RS_ERR_CARELINK_TOKEN;
  else
    Exit(true);
  end;
  Result := false;
end;

procedure TfConf.cbMediaDisableChange(Sender: TObject);
begin
  gbMedia.Enabled := not cbMediaDisable.checked;
end;

procedure TfConf.bDisableMediaHelpClick(Sender: TObject);
begin
  ShowMessage(RS_DISABLE_MEDIA);
end;

procedure TfConf.bDisplayAdvancedHelpClick(Sender: TObject);
begin
  ShowMessage(RS_DISPLAY_ADVANCED_HELP);
end;

procedure TfConf.bDisplayGeneralHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Display_General_Help);
end;

procedure TfConf.bDisplayWindowHelpClick(Sender: TObject);
begin
  ShowMessage(RS_DISPLAY_WINDOW_HELP);
end;

procedure TfConf.bFullArrowSetHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Full_Arrow_Set_Help);
end;

procedure TfConf.bRotatingArrowHelpClick(Sender: TObject);
begin
  ShowMessage(RS_ROTATING_ARROW_HELP);
end;

procedure TfConf.bDiffRightHelpClick(Sender: TObject);
begin
  ShowMessage(RS_DIFF_RIGHT_HELP);
end;

procedure TfConf.bDecimalHelpClick(Sender: TObject);
begin
  ShowMessage(RS_DECIMAL_HELP);
end;

procedure TfConf.bConnectHelpClick(Sender: TObject);
begin
  ShowMessage(RS_CONNECT_HELP);
end;

procedure TfConf.bPredScaleHelpClick(Sender: TObject);
begin
 ShowMessage(RS_PRED_SCALE_HELP);
end;

procedure TfConf.bScaleAgoClick(Sender: TObject);
begin

end;

procedure TfConf.bScaleHelpClick(Sender: TObject);
begin
  ShowMessage(RS_SCALE_DIFF_HELP);
end;

procedure TfConf.bShortModeHelpClick(Sender: TObject);
begin
  ShowMessage(RS_SHORT_MODE_HELP);
end;

procedure TfConf.bWarnHiLowHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Warn_HiLow);
end;

procedure TfConf.bAddClick(Sender: TObject);
var
  s, x: string;
  c: char;
  mr: TModalResult;
begin
  s := ExtInput(uxdAuto, RS_ENTER_USER, RS_ENTER_USER, RS_ENTER_NAME, '', mr);
  if mr = mrOk then
  begin
    if Trim(s) = '' then
    begin
      ShowMessage(RS_ENTER_ANY);
      Exit;
    end;
    for c in s do
      if not (c in ['0'..'9', 'A'..'Z', 'a'..'z', ' ']) then
      begin
        ShowMessage(RS_ENTER_NAME);
        Exit;
      end;

    // No duplicates!
    for x in lbusers.Items do
      if s = x then
      begin
        ShowMessage(RS_DUPE_NAME);
        Exit;
      end;
    lbUsers.AddItem(s, nil);
    lbUsers.Enabled := true;
  end;
end;

procedure TfConf.bBadgeFlashHelpClick(Sender: TObject);
begin
  ShowMessage(RS_BADGE_FLASH_HELP);
end;

procedure TfConf.bColorGraphHelpClick(Sender: TObject);
begin
  ShowMessage(RS_COLOR_BG);
end;

procedure TfConf.bCommonClick(Sender: TObject);
begin
  {$ifdef x_mac}
  tsCommon.tabvisible := true;
  {$endif}
  pcMain.ActivePage := tsCommon;
end;

procedure TfConf.bCustomRangeHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Custom_Range_Help);
end;

procedure TfConf.bExtOpenClick(Sender: TObject);
begin
  OpenDocument(eExt.Text);
end;

procedure TfConf.bExtReloadClick(Sender: TObject);
{$ifdef TrndiExt}
var
  extensionsPath: string;
{$endif}
begin
  {$ifdef TrndiExt}
  if Assigned(FOnReloadExtensions) then
    FOnReloadExtensions(Sender);
  // Refresh the list so newly enabled/disabled extensions and their counts
  // reflect the post-reload state.
  extensionsPath := eExt.Text;
  if extensionsPath <> '' then
    LoadExtensionList(extensionsPath);
  UXMessage(RS_EXT_RELOAD_TITLE, RS_EXT_RELOAD_DONE, uxmtInformation);
  {$endif}
end;

procedure TfConf.lbExtensionsClickCheck(Sender: TObject);
{$ifdef TrndiExt}
var
  idx: integer;
  extId: string;
{$endif}
begin
  {$ifdef TrndiExt}
  idx := lbExtensions.ItemIndex;
  if (FExtPaths = nil) or (idx < 0) or (idx >= FExtPaths.Count) then
    Exit;
  extId := ExtIdFromPath(FExtPaths[idx]);
  tnative.SetSetting('ext.enabled.' + extId, lbExtensions.Checked[idx]);
  {$endif}
end;

procedure TfConf.bFontReadingClick(Sender: TObject);
begin
  lValClick(lVal);
end;

procedure TfConf.bFontArrowClick(Sender: TObject);
begin
  lValClick(lArrow);
end;

procedure TfConf.bFontTimeClick(Sender: TObject);
begin
  lValClick(lAgo);
end;

procedure TfConf.bLanguageHelpClick(Sender: TObject);
begin
  ShowMEssage(RS_OVERRIDE_LANGUAGE);
end;

procedure TfConf.bOverrideHelpClick(Sender: TObject);
begin
  ShowMessage(RS_OVERRIDE_HELP);
end;

procedure TfConf.bPredictHelpClick(Sender: TObject);
begin
  ShowMessage(RS_PREDICTION_HELP);
end;

procedure TfConf.bPredictHorizonClick(Sender: TObject);
begin
  ShowMessage(RS_PREDICTION_HORIZON);
end;

procedure TfConf.bPrivacyHelpClick(Sender: TObject);
begin
  ShowMessage(RS_PRIVACY_HELP);
end;

procedure TfConf.bRemoveClick(Sender: TObject);
begin
  if lbUsers.ItemIndex > -1 then
    lbUsers.DeleteSelected;
  if lbUsers.Items.Count <= 1 then
  begin
    lbUsers.Enabled := false;
    gbMulti.Enabled := false;
  end;

  ShowMessage(RS_REMOVE_ACC);
  bRemove.Enabled := false; // No item selexted now
end;

procedure TfConf.bBackendHelpClick(Sender: TObject);
function HtmlEscapeBasic(const S: string): string;
  begin
    Result := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  end;
var
  s, c, x: string;
  sys: TrndiAPIClass;
begin

  s := '';
  c := '';
  sys := BackendClassOf(cbSys.Text);

  if not assigned(sys) then
    s := RS_CHOOSE_SYSTEM
  else
  begin
    s := sys.ParamLabel(APLDescHTML);
    c := HTMLEscapeBasic(sys.paramLabel(APLCopyright));
  end;

  x := StringOfChar('-', Length(RS_DRIVER_CONTRIBUTOR + c));

  if s <> '' then
  begin
    s := s + sHTMLLineBreak + sHTMLLineBreak + x + sHTMLLineBreak + RS_DRIVER_CONTRIBUTOR + c;
    ExtMsg(uxdAuto, 'API', s, [mbClose],uxmtInformation,20, 1.2);
  end;

end;

procedure TfConf.bSysNoticeClick(Sender: TObject);
var
  ns, url: string;
begin
  ns := tnative.getNotificationSystem;
  url := '';

  case ns of
  'notify-send':
    url := 'https://www.google.com/search?q=notify-send';
  end;
  // Natives:
  // NSUserNotification
  // gdbus
  // WinRT-Toast

  if url <> '' then
  begin
    if ExtMsg(uxdAuto, RS_NOTIFICATIONS, RS_NOTIFY_TITLE,
      Format(RS_NOTIFY_TXT, [ns]), '', uxclWhite, uxclRed, [mbOK, mbUXRead],
      uxmtCustom, 0) <> mrOk then
      OpenURL(url);
  end
  else
    UXMessage(RS_NOTIFICATIONS, RS_NOTIFY_SYSTEM);

end;

procedure TfConf.bSysTouchClick(Sender: TObject);
begin
  ShowMessage(RS_HASTOUCH);
end;

procedure TfConf.bTestAnnounceClick(Sender: TObject);
begin
  if not tnative.SpeakAvailable then
    ShowMessage(Format(Rs_Announce_Not_Available, [tnative.SpeakSoftwareName]))
  else
    tnative.attention('Trndi Test', 'test');
end;

procedure TfConf.bTestClick(Sender: TObject);
var
  res: maybeBool;
  err: string;
  sys: TrndiAPIClass;
begin
  err := '';
  sys := selectedAPIClass;
  if sys = nil then
  begin
    ShowMessage(RS_TEST_UNSUPPORTED);
    exit;
  end;
  res := sys.testConnection(eAddr.text, ePass.text, err);

  case res of
  MaybeBool.false:
  begin
    ShowMessage(RS_TEST_FAIL);
    if ssShift in getKeyShiftState then
      ShowMessage(err);
  end;
  MaybeBool.true:
    ShowMessage(RS_TEST_SUCCESS);
  else
    ShowMessage(RS_TEST_UNSUPPORTED)
  end;

end;

{------------------------------------------------------------------------------
  Resolve the metaclass of the backend currently selected in cbSys. Used for
  class-level capability queries (e.g. supportsWebLogin). Returns nil for
  anything unmapped.
------------------------------------------------------------------------------}
function TfConf.selectedAPIClass: TrndiAPIClass;
begin
  Result := BackendClassOf(cbSys.Text);
end;

{------------------------------------------------------------------------------
  For backends that offer an in-app browser login (CareLink), show the "Log in"
  button and hide the username field — the username is captured from the token
  during login. Other backends keep the normal username entry.
------------------------------------------------------------------------------}
procedure TfConf.updateWebLoginUI;
var
  cls: TrndiAPIClass;
  webLogin: boolean;
begin
  cls := selectedAPIClass;
  webLogin := Assigned(cls) and cls.supportsWebLogin;

  bLogin.Visible := webLogin;
  Label15.Visible := not webLogin;
  eAddr.Visible := not webLogin;

  // The username is captured from the token, so the field is hidden. Seed a
  // non-empty value so a target is always stored (keeps the first-run guide
  // from triggering) even if the user pastes token data manually instead of
  // using the browser login; the login flow overwrites it with the real name.
  if webLogin and (Trim(eAddr.Text) = '') then
    eAddr.Text := 'carelink';
end;

{------------------------------------------------------------------------------
  Assisted CareLink login. The whole flow (confirm, run the backend's Node.js
  helper, capture the token into the credential field, manual-instructions
  fallback) lives in trndi.weblogin so the settings form and the first-run
  wizard behave the same; this handler only supplies the localized texts.
------------------------------------------------------------------------------}
procedure TfConf.bLoginClick(Sender: TObject);
var
  T: TWebLoginTexts;
begin
  T.RunTitle   := RS_WEBLOGIN_RUN_TITLE;
  T.RunPrompt  := RS_WEBLOGIN_RUN_PROMPT;
  T.Installing := RS_WEBLOGIN_INSTALLING;
  T.Waiting    := RS_WEBLOGIN_WAITING;
  T.CapturedOK := RS_WEBLOGIN_OK;
  T.FailTitle  := RS_WEBLOGIN_FAIL_TITLE;
  T.NoScript   := RS_WEBLOGIN_NOSCRIPT;
  T.NoNode     := RS_WEBLOGIN_NONODE;
  T.NpmFailed  := RS_WEBLOGIN_NPM_FAIL;
  T.NoOutput   := RS_WEBLOGIN_NO_OUTPUT;
  T.BadOutput  := RS_WEBLOGIN_BAD_OUTPUT;
  T.HelpTitle  := RS_WEBLOGIN_TITLE;
  T.HelpBody   := RS_WEBLOGIN_HELP;

  RunAssistedWebLogin(selectedAPIClass, bLogin, ePass, RS_WEBLOGIN_BUTTON, T, Self);
end;

procedure TfConf.bTestSpeechClick(Sender: TObject);
begin
  if rbUnit.ItemIndex = 0 then
    tnative.speak('test 5.5')
  else
    tnative.speak('test 100');
end;

procedure TfConf.cbTTSChange(Sender: TObject);
begin
  cbTTSVoice.Enabled := cbTTS.Checked;
  seTTSRate.Enabled := cbTTS.Checked;
  Label36.Enabled := cbTTS.Checked;
  Label37.Enabled := cbTTS.Checked;
  bTestSpeech.Enabled := cbTTS.Checked;
end;

procedure TfConf.PopulateTTSVoices;
{$ifdef X_WIN}
var
  Voice: olevariant;
  Voices: olevariant;
  i: integer;
  voiceName: string;
{$endif}
{$ifdef X_MAC}
var
  Proc: TProcess;
  Output: string;
  Lines: TStringList;
  i, j, hashPos, lastSpace: integer;
  voiceName: string;
  Voices: NSArray;
  vID: NSString;
  attrs: NSDictionary;
  nameObj: NSString;
{$endif}
begin
  cbTTSVoice.Items.Clear;
  cbTTSVoice.Items.Add('Default');

  {$ifdef X_WIN}
  // Creating the SpVoice object doubles as the availability check; going
  // through SpeakAvailable first would create the (slow) COM object twice.
  try
    Voice := CreateOleObject('SAPI.SpVoice');
    Voices := Voice.GetVoices('', '');
    if not VarIsEmpty(Voices) then
    begin
      for i := 0 to Voices.Count - 1 do
      begin
          try
            voiceName := VarToStr(Voices.Item(i).GetDescription(0));
            cbTTSVoice.Items.Add(voiceName);
        except
          // Skip voices that can't provide a description
          cbTTSVoice.Items.Add('Voice ' + IntToStr(i + 1));
        end;
      end;
    end;
  except
    // SAPI not available or failed
  end;
  {$endif}

  {$ifdef X_MAC}
  if tnative.SpeakAvailable then
  begin
    // Prefer Cocoa API on macOS (more reliable than parsing `say` output)
    try
      Voices := NSSpeechSynthesizer.availableVoices;
      if Voices <> nil then
      begin
        for i := 0 to Integer(Voices.count) - 1 do
        begin
          vID := NSString(Voices.objectAtIndex(i));
          if vID = nil then Continue;
          attrs := NSSpeechSynthesizer.attributesForVoice(vID);
          if attrs <> nil then
          begin
            try
              nameObj := NSString(attrs.objectForKey(StrToNSStr('NSVoiceName')));
              voiceName := '';
              if nameObj <> nil then
                voiceName := NSStrToStr(nameObj);
              if voiceName = '' then
                voiceName := NSStrToStr(NSString(vID));
              if voiceName <> '' then
                cbTTSVoice.Items.Add(voiceName)
              else
                cbTTSVoice.Items.Add('Voice ' + IntToStr(i + 1));
            except
              cbTTSVoice.Items.Add('Voice ' + IntToStr(i + 1));
            end;
          end;
        end;
      end;
    except
      // Fallback: keep default-only list when Cocoa query fails
    end;
  end;
  {$endif}

  {$ifdef X_PC}  // Linux
  // For Linux speech-dispatcher, add the standard voice types
  cbTTSVoice.Items.Add('Male 1');
  cbTTSVoice.Items.Add('Male 2');
  cbTTSVoice.Items.Add('Male 3');
  cbTTSVoice.Items.Add('Female 1');
  cbTTSVoice.Items.Add('Female 2');
  cbTTSVoice.Items.Add('Female 3');
  {$endif}
end;

{------------------------------------------------------------------------------
  Enumerate TTS voices on first demand and apply the selection stashed by
  LoadUserSettings. Enumeration (SAPI COM on Windows, Cocoa on macOS) is slow,
  so it runs when the Accessibility tab is shown, not when the dialog opens.
------------------------------------------------------------------------------}
procedure TfConf.EnsureTTSVoices;
begin
  if FTTSVoicesLoaded then
    Exit;
  FTTSVoicesLoaded := true;

  PopulateTTSVoices;

  if (pendingTTSVoiceName <> '') and
    (cbTTSVoice.Items.IndexOf(pendingTTSVoiceName) >= 0) then
    cbTTSVoice.ItemIndex := cbTTSVoice.Items.IndexOf(pendingTTSVoiceName)
  else
  if (pendingTTSVoiceIndex >= 0) and
    (pendingTTSVoiceIndex < cbTTSVoice.Items.Count) then
    cbTTSVoice.ItemIndex := pendingTTSVoiceIndex
  else
    cbTTSVoice.ItemIndex := 0;
end;

procedure TfConf.tsAccessShow(Sender: TObject);
begin
  EnsureTTSVoices;
end;

{------------------------------------------------------------------------------
  Fill the Razer device list on first demand. Initializing the Chroma SDK can
  block for over a second while it connects to Synapse, so it is deferred to
  the Chroma tab's OnShow. When the main app already holds an initialized
  instance (razer.enabled), the caller passes it via the chroma field and it
  is reused instead of re-initializing.
------------------------------------------------------------------------------}
procedure TfConf.PopulateChromaDevices;
var
  own: TRazerChromaBase;
  i: integer;
begin
  if FChromaListLoaded then
    Exit;
  FChromaListLoaded := true;

  if Assigned(chroma) and chroma.Initialized then
  begin
    for i := 0 to chroma.GetDeviceCount - 1 do
      lbChroma.Items.Add(chroma.GetDevice(i).Name);
    Exit;
  end;

  own := TRazerChromaFactory.CreateInstance;
  try
    if not own.Initialize then
      lbChroma.Items.Add('No Razer driver detected')
    else
      for i := 0 to own.GetDeviceCount - 1 do
        lbChroma.Items.Add(own.GetDevice(i).Name);
  finally
    own.Free;
  end;
end;

procedure TfConf.tsChromaShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    PopulateChromaDevices;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfConf.bThreasholdLinesHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Threashold_Lines_Help);
end;

procedure TfConf.bTimeStampHelpClick(Sender: TObject);
begin
  ShowMessage(RS_TIMESTAMP_HELP);
end;

procedure TfConf.bExportSettingsClick(Sender: TObject);
var
  settingsData, encodedData: string;
  dlg: TSaveDialog;
begin
  settingsData := tnative.ExportSettings;
  if settingsData = '' then
  begin
    ShowMessage(RS_EXPORT_EMPTY);
    Exit;
  end;

  encodedData := EncodeStringBase64(settingsData);

  dlg := TSaveDialog.Create(nil);
  try
    dlg.Title := RS_EXPORT_TITLE;
    dlg.Filter := RS_SETTINGS_FILTER;
    dlg.DefaultExt := 'trndi';
    dlg.FileName := 'trndi_settings.trndi';
    dlg.Options := dlg.Options + [ofOverwritePrompt];

    if dlg.Execute then
    begin
      with TFileStream.Create(dlg.FileName, fmCreate) do
      try
        WriteBuffer(encodedData[1], Length(encodedData));
      finally
        Free;
      end;
      ShowMessage(RS_EXPORT_OK);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfConf.bImportSettingsClick(Sender: TObject);
var
  encodedData, settingsData: string;
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(nil);
  try
    dlg.Title := RS_IMPORT_TITLE;
    dlg.Filter := RS_SETTINGS_FILTER;
    dlg.DefaultExt := 'trndi';

    if dlg.Execute then
    begin
      with TFileStream.Create(dlg.FileName, fmOpenRead) do
      try
        SetLength(encodedData, Size);
        if Size > 0 then
          ReadBuffer(encodedData[1], Size);
      finally
        Free;
      end;

      if Trim(encodedData) = '' then
      begin
        ShowMessage(RS_IMPORT_EMPTY);
        Exit;
      end;

      try
        settingsData := DecodeStringBase64(encodedData);
        tnative.ImportSettings(settingsData);
        ShowMessage(RS_IMPORT_OK);
      except
        on E: Exception do
          ShowMessage(Format(RS_IMPORT_FAIL, [E.Message]));
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfConf.btResetClick(Sender: TObject);
var
  theme: TTrndiTheme;
begin
  // Same palette as the first-run defaults in umain — both come from trndi.theme.
  theme := TrndiThemeClassic;

  cl_ok_bg.ButtonColor := theme.ColorOk;
  cl_hi_bg.ButtonColor := theme.ColorHigh;
  cl_lo_bg.ButtonColor := theme.ColorLow;

  cl_ok_txt.ButtonColor := theme.ColorOkText;
  cl_hi_txt.ButtonColor := theme.ColorHighText;
  cl_lo_txt.ButtonColor := theme.ColorLowText;

  cl_hi_bg_cust.ButtonColor := theme.ColorRangeHigh;
  cl_lo_bg_cust.ButtonColor := theme.ColorRangeLow;

  cl_hi_txt_cust.ButtonColor := theme.ColorRangeHighText;
  cl_lo_txt_cust.ButtonColor := theme.ColorRangeLowText;
end;

procedure TfConf.btUserSaveClick(Sender: TObject);
begin
  if lUserName.Caption[1] = '-' then
    tNative.configUser := ''
  else
    tNative.configUser := lUserName.Caption;

  tNative.SetColorSetting('user.color', cbUser.ButtonColor);
  tNative.SetSetting('user.nick', edNick.Text);

  btUserSave.Enabled := false;
end;

procedure TfConf.bUseURLHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Use_URL_Help);
end;

procedure TfConf.Button1Click(Sender: TObject);
begin
  Openurl('https://github.com/slicke/trndi/blob/main/doc/LANGUAGES.md');
end;

{------------------------------------------------------------------------------
  Apply the display panel's font to preview labels.

  This lets the user quickly see how the selected UI font looks on key
  readouts (value, arrow, and "ago") without changing global styles.
------------------------------------------------------------------------------}
procedure TfConf.bFontResetClick(Sender: TObject);
begin
  lVal.Font.Name := pnDisplay.Font.Name;
  lArrow.Font.Name := pnDisplay.Font.Name;
  lAgo.Font.Name := pnDisplay.Font.Name;
end;

{------------------------------------------------------------------------------
  Check for the latest Trndi release (final, all platforms) and offer to open
  the downloads page. Pre-releases and platform-specific filtering are no
  longer used.

  Flow:
  - Fetch https://api.github.com/repos/slicke/trndi/releases/latest
  - Compare with current version via HasNewerRelease(..., pre=false)
  - Resolve a suitable asset URL via GetNewerVersionURL(res)
  - Prompt the user to open the browser
------------------------------------------------------------------------------}
procedure TfConf.Button3Click(Sender: TObject);
var
  res,   // raw JSON from GitHub API
  r,     // download URL
  rn,    // latest version name/tag
  s: string;
  rok: boolean;
begin
  TrndiNative.getURL('https://api.github.com/repos/slicke/trndi/releases/latest', res);
  rok := HasNewerRelease(res, rn, false);

  if rok then
  begin
    r := GetNewerVersionURL(res);
    if IsPRBuild then
      s := Format(RS_NEWVER_PR, [rn])
    else
      s := Format(RS_NEWVER, [rn]);
    if UXDialog(uxdAuto, RS_NEWVER_CAPTION, s, [mbYes, mbNo], mtInformation) = mrYes then
      OpenURL(r);
  end
  else begin
    if IsPRBuild then
    begin
      // PR builds carry 'PR-<n>' as build number; don't pretend they are a
      // stable release, point the user to the latest one instead
      rn := GetLatestReleaseName(res);
      if rn = '' then
        rn := GetProductVersionMajorMinor('12.x');
      ShowMessage(Format(RS_UPTODATE_PR, [rn]));
    end
    // CI/BUILD_NUMBER are compile-time constants stamped by CI, so one branch
    // is always "unreachable" in any given build — both are needed
    {$PUSH}{$WARN 6018 OFF}
    else if CI and (BUILD_NUMBER <> 'dev') then
      ShowMessage(RS_UPTODATE)
    else
      ShowMessage(Format(RS_UPTODATE_DEV, [GetProductVersionMajorMinor('12.x')]));
    {$POP}
  end;
end;

procedure TfConf.bWebAPIClick(Sender: TObject);
begin

  if ExtHTML(uxdAuto,'WebAPI', RS_WEBAPI, [mbOK, mbUxRead], uxmtOK) <> mrOk then
    OpenURL('https://github.com/slicke/trndi/blob/main/doc/WebAPI.md');
end;

procedure TfConf.cbCust1Change(Sender: TObject);
begin
  cbCust.Checked := cbCust1.Checked;
  fsHi1.Enabled := fsHi.Enabled;
  fsLo1.Enabled := fsLo.Enabled;
end;

procedure TfConf.cbCustChange(Sender: TObject);
begin
  fsHi.Enabled := cbCust.Checked;
  fsLo.Enabled := cbCust.Checked;
  if (cbCust.Checked) and (cbSys.Text = 'NightScout') then
    ShowMessage(RS_OVERRIDE_NS);
  cbCust1.Checked := cbCust.Checked;
end;

procedure TfConf.cbCustRangeChange(Sender: TObject);
begin
  fsHiRange.Enabled := cbCustRange.Checked;
  fsLoRange.Enabled := cbCustRange.Checked;
  if (cbCustRange.Checked) and (cbSys.Text = 'NightScout') then
    ShowMessage(RS_OVERRIDE_NS);
end;

procedure TfConf.cbPosChange(Sender: TObject);
{$ifdef X_LINUXBSD}
var
  s: string;
{$endif}
begin
  {$ifdef X_LINUXBSD}
  s := GetWindowManagerName;
  ExtHTML(uxdAuto, RS_POS_TITLE, IfThen(s = '', RS_POS_UNKNOWN, Format(RS_POS, [s])));
  {$endif}
end;

procedure TfConf.UpdatePredictionStates;
var
  predOn, shortOn: boolean;
begin
  predOn  := cbPredictions.Checked;
  shortOn := predOn and cbPredictShort.Checked;

  cbPredictShort.Enabled            := predOn;
  cbPredictDots.Enabled             := predOn;
  cbWarnLoHi.Enabled                := predOn;
  cbPredictShortFullArrows.Enabled  := shortOn;
  rbPredictShortShowValue.Enabled   := shortOn;
  rbPredictShortArrowOnly.Enabled   := shortOn;
  cbPredictShortMinutes.Enabled     := shortOn;
end;

procedure TfConf.cbPredictionsChange(Sender: TObject);
begin
  if cbPredictions.Checked and self.Showing then
    ShowMessage(RS_PredictionWarn);
  UpdatePredictionStates;
end;

procedure TfConf.cbPredictShortChange(Sender: TObject);
begin
  UpdatePredictionStates;
end;

procedure TfConf.cbPredictShortFullArrowsChange(Sender: TObject);
begin
  if cbPredictShortFullArrows.Checked and (self.Showing) then
    ShowMessage(RS_SHORTMODE_FULL);
end;

procedure TfConf.cbPredictShortMinutesChange(Sender: TObject);
begin
  // Persist short prediction horizon immediately so the main UI can reload it
  if cbPredictShortMinutes.ItemIndex >= 0 then
    case cbPredictShortMinutes.ItemIndex of
    0:
      tNative.SetSetting('predictions.short.minutes', '5');
    2:
      tNative.SetSetting('predictions.short.minutes', '15');
    else
      tNative.SetSetting('predictions.short.minutes', '10');
    end;
end;

procedure TfConf.cbPredictShortSizeChange(Sender: TObject);
begin
  // Apply short size selection to native settings immediately; the main UI will
  // reload settings after the dialog is closed and apply layout changes.
  if cbPredictShortSize.ItemIndex >= 0 then
    tNative.SetSetting('predictions.short.size', IntToStr(cbPredictShortSize.ItemIndex + 1));
  // We do not touch the main form from the settings form to avoid circular
  // dependencies; the main UI reads this setting and will refresh on dialog
  // close. If you want immediate preview, close the settings dialog.
end;

procedure TfConf.FormCreate(Sender: TObject);
var
  {$ifdef LCLGtk2}
  wc: TWinControl;
  wi: integer;
  {$endif}
  os, arch: string;
  bottombar: tpanel;
  bottomclose: tbutton;
begin
  // Base app version + build date + widgetset + target CPU
  lVersion.Caption := GetProductVersionMajorMinor('12.x');
  // If CI embedded a real build number, append it. CI/BUILD_NUMBER are
  // compile-time constants, so one branch is always "unreachable" per build.
  {$PUSH}{$WARN 6018 OFF}
  if CI and (BUILD_NUMBER <> 'dev') then
    lVersion.Caption := lVersion.Caption + '.' + BUILD_NUMBER
  else
    lVersion.Caption := Format('%s-dev (%s)', [lVersion.Caption,
      StringReplace({$I %DATE%}, '/', '-', [rfReplaceAll])]);
  {$POP}
  lversion.left := lversion.left - 20;

  pcMain.ActivePage := tsGeneral;
  {$ifdef X_MAC}
  self.width := self.width + (self.width div 9);
  {$endif}
  {$ifdef lclqt6}
  self.font.size := 10;
  {$endif}
  {$ifdef HAIKU}
  lSysWarnInfo.font.size := 9;
  {$endif}
  {$ifdef lclgtk2}
  self.font.size := 10;
  for wi := 0 to self.ComponentCount-1 do
    if (self.components[wi] is TEdit) or (self.components[wi] is TSpinEdit) then
    begin
      wc := self.components[wi] as TWinControl;
      wc.Font.Color := clBlack;
    end;

  {$endif}
  {$ifdef lclqt6}
  self.height := self.height + 20;
  {$endif}
  tnative := TrndiNative.Create;
  tnative.noFree := true;
  if tnative.isDarkMode then
    tnative.setDarkMode
  {$ifdef X_WIN}
    (self.Handle)
  {$endif}
  ;

  // TTS voice enumeration (SAPI/Cocoa) is deferred to the Accessibility tab's
  // OnShow — it is too slow to run every time the dialog opens.

  // Initialize parameter labels for current backend selection
  cbSysChange(Self);

  // Encode the gear glyph explicitly so the concat stays UTF-8 throughout
  bcommon.Caption := UTF8Encode(WideString(WChar(2699))) + ' ' + bCommon.Caption;

  {$IFNDEF TRNDIEXT}
  lExtName.Caption := RS_NO_EXTENSIONS;


  {$IF (DEFINED(WINDOWS) OR DEFINED(LINUX)) AND DEFINED(CPUAMD64)}
  lExtCopyright.Caption := RS_NO_EXTENSIONS_COPYRIGHT;
  {$ELSE}
  arch := {$I %FPCTARGETCPU%};
  os := {$I %FPCTARGETOS%};
  lExtCopyright.Caption := Format(RS_NO_EXTENSIONS_SYSTEM, [os, arch]);
  {$ENDIF}
  {$endif}

  {$ifdef X_WIN}
  cbTitleColor.Visible := false;
  {$endif}
  if tnative.nobuttonsVM then
  begin
    bottombar := TPanel.Create(self);
    bottombar.Align:=alBottom;
    bottombar.height := 30;
    bottombar.parent := self;

    bottomclose := TButton.Create(bottombar);
    bottomclose.Caption := 'Close';
    bottomclose.parent := bottombar;
    bottomclose.left := 5;
    bottomclose.onclick := @CloseClick;

    self.height := self.height + 30;
  end;

  // Initialize TTS controls
  cbTTSChange(Self);
end;

procedure TfConf.FormDestroy(Sender: TObject);
begin
  tnative.Free;
  FExtPaths.Free;
end;

procedure TfConf.FormResize(Sender: TObject);
begin
  eAddr.Width := cbSys.Width;
  ePass.Width := cbSys.Width;
  bvExt.Width := cbSys.Width;
  eExt.Width := cbSys.Width;
end;

procedure TfConf.fsHi1Change(Sender: TObject);
begin
  fsHi.Value := fsHi1.Value;
end;

procedure TfConf.fsHiChange(Sender: TObject);
begin
  fsHi1.DecimalPlaces := fsHi.DecimalPlaces;
  fsHi1.Value := fsHi.Value;
end;

procedure TfConf.fsLo1Change(Sender: TObject);
begin
  fsLo.Value := fsLo1.Value;
end;

procedure TfConf.fsLoChange(Sender: TObject);
begin
  fsLo1.DecimalPlaces := fsLo.DecimalPlaces;
  fsLo1.Value := fsLo.Value;
end;

procedure TfConf.Label12Click(Sender: TObject);
begin

end;

procedure TfConf.lLicenseClick(Sender: TObject);
const
  txt ={$ifndef HAIKU}'<img src="https://trndi.app/doc/img/trndi-logo.png">' + sHTMLLineBreak +{$endif}
    '<b>Trndi - CGM viewer</b>' + sHTMLLineBreak +
    '<i>A re-imagination of TrayTrend by Björn Lindh</i>' + sHTMLLineBreak +
    'Copyright (C) 2017-2026 Björn Lindh' + sHTMLLineBreak + sHTMLLineBreak +
    'This program is free software: you can redistribute it and/or modify it' + sHTMLLineBreak +
    'under the terms of the GNU General Public License version 3 as published' + sHTMLLineBreak +
    'by the Free Software Foundation.' + sHTMLLineBreak +
    'For more information, refer to the accompanying license file or visit:' + sHTMLLineBreak +
    'https://www.gnu.org/licenses/gpl-3.0' + sHTMLLineBreak +
    'Trndi is hobby project, verify all data with an officially approved' + sHTMLLineBreak +
    'medical app before acting on any shown data!' + sHTMLLineBreak +
    'This app is NOT a medical device and is NOT intended for:' + sHTMLLineBreak +
    '- Medical diagnosis, treatment, or prevention'#10 + '- Making medical decisions' + sHTMLLineBreak +
    '- Replacing your CGM app or medical devices'#10 + '- Emergency medical situations' + sHTMLLineBreak + sHTMLLineBreak +
    '<b>### IMPORTANT WARNINGS ###</b>' + sHTMLLineBreak +
    '<ul>'+
    '<li>Data displayed may be inaccurate, delayed, or unavailable</li>' +
    '<li> Always verify readings with your official CGM device</li>' +
    '<li> Never make medical decisions based solely on this app</li>' +
    '<li> Consult healthcare professionals for medical advice</li>'+
    '</ul>' + sHTMLLineBreak +
    '<b>### BY USING THIS APP, YOU ACKNOWLEDGE THAT ###</b>' + sHTMLLineBreak +
    '- The developers assume NO LIABILITY for any harm, injury, or damages' + sHTMLLineBreak +
    '- You use this app entirely at your own risk' + sHTMLLineBreak +
    '- This app may contain bugs or errors that could display incorrect data' + sHTMLLineBreak + sHTMLLineBreak +
    '<b><i>- IF YOU DO NOT AGREE WITH THESE TERMS, DO NOT USE THIS APP. -</i></b>';
begin
  if ExtMsg(uxdAuto, 'License', txt, [mbOK, mbUxRead], uxmtOK, uxscHuge) <> mrOk then
    OpenURL('https://github.com/slicke/trndi/blob/main/LICENSE.md');
end;

procedure TfConf.lSysWarnInfoClick(Sender: TObject);
begin
  case cbSys.Text of
  API_DEX_USA, API_DEX_EU,
  API_DEX_NEW_USA, API_DEX_NEW_EU, API_DEX_NEW_JP, API_TANDEM_USA, API_TANDEM_EU:
    pcMain.ActivePage := tsCustom;
  end;
end;

procedure TfConf.lValClick(Sender: TObject);
var
  f: TFont;
  mr: TModalResult;
  title: string;
begin

  case (sender as TLabel).Name of
  'lArrow':
    title := RS_SELECT_FONT_ARROW;
  'lVal':
    title := RS_SELECT_FONT_READING;
  'lAgo':
    title := RS_SELECT_FONT_TIME;
  else
    title := RS_SELECT_FONT_DESC;
  end;
  f := slicke.ux.alert.ExtFontPicker(uxdAuto,RS_SELECT_FONT, RS_SELECT_FONT, title, (sender as TLabel).font, (sender as TLabel).caption, mr);
  if mr = mrOK then
    (Sender as TLabel).Font := f;
end;

procedure TfConf.pcColorsChange(Sender: TObject);
begin

end;

procedure TfConf.pcMainChange(Sender: TObject);
begin
  {$ifdef x_mac}
  tsCommon.tabvisible := false;
  {$endif}
end;

procedure TfConf.rbUnitClick(Sender: TObject);
function RoundMMOL(const v: double): double;
  begin
    Result := round(v * TrndiAPI.toMMOL * 10) / 10;
  end;
begin
  if (Sender is TForm) or (rbUnit.ItemIndex = 0) then
  begin
    fsHi.DecimalPlaces := 1;
    fsLo.DecimalPlaces := 1;
    fsHiRange.DecimalPlaces := 1;
    fsLoRange.DecimalPlaces := 1;

    fsHi.Value := RoundMMOL(fsHi.Value);
    fsLo.Value := RoundMMOL(fsLo.Value);
    fsLoRange.Value := RoundMMOL(fsLoRange.Value);
    fsHiRange.Value := RoundMMOL(fsHiRange.Value);

    // Set max after, as mmol will be lower
    fsHi.MaxValue := 33.3;
    fsLo.MaxValue := 33.3;
    fsHiRange.MaxValue := 33.3;
    fsLoRange.MaxValue := 33.3;

    fsAlertHystHi.DecimalPlaces  := 1;
    fsAlertHystLo.DecimalPlaces  := 1;
    fsAlertHystUrg.DecimalPlaces := 1;
    fsAlertHystHi.Increment      := 0.1;
    fsAlertHystLo.Increment      := 0.1;
    fsAlertHystUrg.Increment     := 0.1;
    fsAlertHystHi.Value          := RoundMMOL(fsAlertHystHi.Value);
    fsAlertHystLo.Value          := RoundMMOL(fsAlertHystLo.Value);
    fsAlertHystUrg.Value         := RoundMMOL(fsAlertHystUrg.Value);
    fsAlertHystHi.MaxValue       := 2.8;
    fsAlertHystLo.MaxValue       := 2.8;
    fsAlertHystUrg.MaxValue      := 2.8;
    lAlertHystRow.Caption        := RS_ALERT_HYSTERESIS_MMOL;

    rbPredictShortShowValue.Caption := StringReplace(rbPredictShortShowValue.Caption, '100', '5.5', [rfReplaceAll]);
  end
  else
  begin
    // Se the max first as mgdl will be higher
    fsHi.MaxValue := 600;
    fsLo.MaxValue := 600;
    fsHiRange.MaxValue := 600;
    fsLoRange.MaxValue := 600;

    fsHi.Value := round(fsHi.Value * TrndiAPI.toMgdl);
    fsLo.Value := round(fsLo.Value * TrndiAPI.toMgdl);
    fsHiRange.Value := round(fsHiRange.Value * TrndiAPI.toMgdl);
    fsLoRange.Value := round(fsLoRange.Value * TrndiAPI.toMgdl);
    fsHi.DecimalPlaces := 0;
    fsLo.DecimalPlaces := 0;
    fsHiRange.DecimalPlaces := 0;
    fsLoRange.DecimalPlaces := 0;

    fsAlertHystHi.MaxValue       := 50;
    fsAlertHystLo.MaxValue       := 50;
    fsAlertHystUrg.MaxValue      := 50;
    fsAlertHystHi.Value          := round(fsAlertHystHi.Value * TrndiAPI.toMgdl);
    fsAlertHystLo.Value          := round(fsAlertHystLo.Value * TrndiAPI.toMgdl);
    fsAlertHystUrg.Value         := round(fsAlertHystUrg.Value * TrndiAPI.toMgdl);
    fsAlertHystHi.DecimalPlaces  := 0;
    fsAlertHystLo.DecimalPlaces  := 0;
    fsAlertHystUrg.DecimalPlaces := 0;
    fsAlertHystHi.Increment      := 1;
    fsAlertHystLo.Increment      := 1;
    fsAlertHystUrg.Increment     := 1;
    lAlertHystRow.Caption        := RS_ALERT_HYSTERESIS_MGDL;

    rbPredictShortShowValue.Caption := StringReplace(rbPredictShortShowValue.Caption, '5.5', '100', [rfReplaceAll]);
  end;
end;

procedure TfConf.spTHRESHOLD1Change(Sender: TObject);
begin
  spTHRESHOLD.Value := spTHRESHOLD1.value;
end;

procedure TfConf.spTHRESHOLDChange(Sender: TObject);
begin
  if spTHRESHOLD.Value > 30 then
  begin
    spTHRESHOLD.Value := 30;
    ShowMessage(RS_Saftey_Hi);
  end
  else
  if spTHRESHOLD.Value < 6 then
  begin
    spTHRESHOLD.Value := 6;
    ShowMessage(RS_Saftey_Low);
  end;

  spTHRESHOLD1.Value := spTHRESHOLD.value;
end;

procedure TfConf.tbAdvancedChange(Sender: TObject);
begin

end;

procedure TfConf.ToggleBox1Change(Sender: TObject);
begin

end;

procedure TfConf.tsCommonShow(Sender: TObject);
begin
  fsHi1.Enabled := fsHi.Enabled;
  fsLo1.Enabled := fsLo.Enabled;
end;

procedure TfConf.tsDisplayShow(Sender: TObject);
begin
  if lVal.Font.color = lVal.Parent.Color then
    lVal.Font.color := IfThen(lVal.Parent.Color = clBlack, clWhite, clBlack);

  if lArrow.Font.color = lArrow.Parent.Color then
    lArrow.Font.color := IfThen(lArrow.Parent.Color = clBlack, clWhite, clBlack);
  if lArrow.Caption = '' then
    lArrow.caption := '→';

  if lAgo.Font.color = lAgo.Parent.Color then
    lAgo.Font.color := IfThen(lAgo.Parent.Color = clBlack, clWhite, clBlack);

  if lDiff.Font.color = lDiff.Parent.Color then
    lDiff.Font.color := IfThen(lDiff.Parent.Color = clBlack, clWhite, clBlack);
  if (lDiff.Caption = '') or (lDiff.Caption = 'lDiff') then
    lDiff.Caption := '+0,0';
end;

procedure TfConf.LoadProxySettingsIntoUI;
var
  hostV: string;
begin
  if tnative = nil then
    Exit;

  FProxyLoading := true;
  try
    hostV := tnative.GetSetting('proxy.host', '', true);
    edProxyHost.Text := hostV;
    edProxyPort.Text := tnative.GetSetting('proxy.port', '', true);
    edProxyUser.Text := tnative.GetSetting('proxy.user', '', true);
    edProxyPass.Text := tnative.GetSetting('proxy.pass', '', true);

    if (Trim(hostV) <> '') and (Trim(edProxyPort.Text) = '') then
      edProxyPort.Text := '8080';
  finally
    FProxyLoading := false;
  end;
end;

procedure TfConf.SaveProxySettingsFromUI;
var
  hostV, portV, userV, passV: string;
begin
  if tnative = nil then
    Exit;

  hostV := Trim(edProxyHost.Text);
  portV := Trim(edProxyPort.Text);
  userV := Trim(edProxyUser.Text);
  passV := edProxyPass.Text;

  // Persist proxy settings globally (not per-user)
  tnative.SetSetting('proxy.host', hostV, true);
  if hostV = '' then
  begin
    // Clear auxiliary fields when proxy is disabled
    tnative.SetSetting('proxy.port', '', true);
    tnative.SetSetting('proxy.user', '', true);
    tnative.SetSetting('proxy.pass', '', true);
    Exit;
  end;

  tnative.SetSetting('proxy.port', portV, true);
  tnative.SetSetting('proxy.user', userV, true);
  tnative.SetSetting('proxy.pass', passV, true);
end;

procedure TfConf.ProxyEditChange(Sender: TObject);
begin
  if FProxyLoading then
    Exit;
  SaveProxySettingsFromUI;
end;

procedure TfConf.tsProxyShow(Sender: TObject);
begin
  {$ifdef X_MAC}
  gbNetwork.Enabled := false;
  {$endif}

  LoadProxySettingsIntoUI;
end;

procedure TfConf.bTestProxyClick(Sender: TObject);
const
  TEST_URL_HTTP = 'http://example.com/';
  TEST_URL_HTTPS = 'https://example.com/';
var
  hostV, portV, userV, passV: string;
  resp: string;
  okHttp: boolean;
  okHttps: boolean;
  respHttp: string;
  respHttps: string;
begin
  hostV := Trim(edProxyHost.Text);
  portV := Trim(edProxyPort.Text);
  userV := Trim(edProxyUser.Text);
  passV := edProxyPass.Text;

  if hostV = '' then
  begin
    UXMessage('Proxy', RS_EMPTY_PROXY, uxmtInformation);
    Exit;
  end;

  respHttp := '';
  respHttps := '';
  okHttp := TrndiNative.TestProxyURL(TEST_URL_HTTP, hostV, portV, userV, passV, respHttp);
  okHttps := TrndiNative.TestProxyURL(TEST_URL_HTTPS, hostV, portV, userV, passV, respHttps);

  if okHttp and okHttps then
    ShowMessage(RS_TEST_SUCCESS)
  else
  if okHttp and (not okHttps) then
    UXMessage('Proxy',
      'HTTP via proxy works, but HTTPS via proxy failed. Trndi uses HTTPS endpoints, so this proxy configuration will not work for normal operation.' +
      LineEnding + LineEnding + respHttps,
      uxmtWarning)
  else
  begin
    ShowMessage(RS_TEST_FAIL);
    if ssShift in getKeyShiftState then
    begin
      resp := 'HTTP test (' + TEST_URL_HTTP + '): ' + IfThen(okHttp, 'OK', 'FAIL') + LineEnding + respHttp + LineEnding + LineEnding +
        'HTTPS test (' + TEST_URL_HTTPS + '): ' + IfThen(okHttps, 'OK', 'FAIL') + LineEnding + respHttps;
      ShowMessage(resp);
    end;
  end;
end;

procedure TfConf.tsSystemShow(Sender: TObject);
begin
  pnSysInfo.Hide;
  lWaitSys.Show;
  tsSystem.Update;
  Application.ProcessMessages;

  cbNotice.Checked := TrndiNative.isNotificationSystemAvailable;
  cbAutoStart.Enabled := TrndiNative.AutoStartAvailable;
  if cbAutoStart.Enabled then
    cbAutoStart.Checked := TrndiNative.GetAutoStart
  else
    cbAutoStart.Checked := false;
  lWaitSys.Visible := false;
  lProblematic.Visible := false;
  if IsProblematicWM then
    if not IsSemiProblematicWM then
      lProblematic.Visible := true;

  pnSysInfo.Show;
end;

end.
