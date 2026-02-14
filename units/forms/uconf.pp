
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
 *)

unit uconf;

{$I ../../inc/native.inc}

interface

uses
Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, Forms, Controls,
Graphics, Dialogs, LCLTranslator, trndi.native, lclintf,
slicke.ux.alert, slicke.ux.native, VersionInfo, trndi.funcs, buildinfo, StrUtils, trndi.api, trndi.api.nightscout, trndi.api.nightscout3, trndi.api.dexcom, trndi.api.dexcomNew, trndi.api.tandem, trndi.api.xdrip, razer.chroma, math, trndi.types, trndi.api.debug_firstXmissing, trndi.api.debug_intermittentmissing, trndi.api.debug_custom, trndi.api.debug, trndi.api.debug_slow, base64, Variants{$ifdef X_WIN}, ComObj{$endif};

{$I ../../inc/defines.inc}
type

  { TfConf }

TfConf = class(TForm)
  bAdd: TButton;
  bBackendHelp: TButton;
  bDotHelp: TButton;
  bExportSettings: TButton;
  bFontHelp: TButton;
  bImportSettings: TButton;
  bLanguageHelp: TButton;
  bNotificationHelp: TButton;
  bColorGraphHelp: TButton;
  bMultiUserHelp: TButton;
  bDeltaMaxHelp: TButton;
  bOutdatedHelp1: TButton;
  bOverrideHelp1: TButton;
  bPredictHorizon: TButton;
  bTemplateCurrent: TButton;
  bTemplateTrend: TButton;
  bTestSpeech: TButton;
  bTimeStampHelp: TButton;
  bUseURLHelp: TButton;
  bThreasholdLinesHelp: TButton;
  bOutdatedHelp: TButton;
  bBadgeFlashHelp: TButton;
  bPrivacyHelp: TButton;
  bPredictHelp: TButton;
  bTest: TButton;
  bTestProxy: TButton;
  bOverrideHelp: TButton;
  bRemove: TButton;
  bSysNotice: TButton;
  bMinMinutesHelp: TButton;
  bCustomRangeHelp: TButton;
  bExtOpen: TButton;
  bCommon: TButton;
  Button2: TButton;
  Button4: TButton;
  bWebAPI: TButton;
  bSysTouch: TButton;
  bTestAnnounce: TButton;
  btReset: TButton;
  btUserSave: TButton;
  Button3: TButton;
  bvExt: TBevel;
  bvExt1: TBevel;
  cbChromaNormal: TCheckBox;
  cbChromaHigh: TComboBox;
  cbChromaLow: TComboBox;
  cbCust: TCheckBox;
  cbCust1: TCheckBox;
  cbCustRange: TCheckBox;
  cbAlertMissing: TCheckBox;
  cbLang: TComboBox;
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
  cbPredictShort: TCheckBox;
  cbPredictShortFullArrows: TCheckBox;
  cbPredictShortMinutes: TComboBox;
  cbPredictShortSize: TComboBox;
  cbPrivacy: TCheckBox;
  cbHContrast: TCheckBox;
  cbMediaDisable: TCheckBox;
  Label36: TLabel;
  Label37: TLabel;
  Label38: TLabel;
  Panel21: TPanel;
  Panel22: TPanel;
  Panel23: TPanel;
  Panel24: TPanel;
  edCommaSep1: TEdit;
  eDot: TEdit;
  eDotNow: TEdit;
  edProxyHost: TEdit;
  edProxyPass: TEdit;
  edProxyPort: TEdit;
  edProxyUser: TEdit;
  eExt: TEdit;
  cbFonts: TGroupBox;
  fsHi1: TFloatSpinEdit;
  fsLo1: TFloatSpinEdit;
  gbNetwork: TGroupBox;
  gbOverride1: TGroupBox;
  gbSettings: TGroupBox;
  Label16: TLabel;
  Label17: TLabel;
  Label2: TLabel;
  Label25: TLabel;
  Label33: TLabel;
  Label34: TLabel;
  Label35: TLabel;
  LabelProxyHost: TLabel;
  LabelProxyPass: TLabel;
  LabelProxyPort: TLabel;
  LabelProxyUser: TLabel;
  lConfigPredict: TLabel;
  lDot: TLabel;
  lDot1: TLabel;
  lDot2: TLabel;
  lDot3: TLabel;
  lDotNow: TLabel;
  lExt: TLabel;
  lHiOver3: TLabel;
  lLounder2: TLabel;
  lProxyTitle: TLabel;
  lSettingsHelp: TLabel;
  lSysWarnInfo: TLabel;
  lProxyDesc: TLabel;
  Panel18: TPanel;
  Panel19: TPanel;
  Panel20: TPanel;
  Panel3: TPanel;
  PanelProxyActions: TPanel;
  pnDeltaMax: TPanel;
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
  edCommaSep: TEdit;
  edURLHigh: TEdit;
  edURLLow: TEdit;
  edURLPerfect: TEdit;
  edNick: TEdit;
  edTray: TSpinEdit;
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
  Label1: TLabel;
  Label13: TLabel;
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
  lbExtensions: TListBox;
  lCopyright: TLabel;
  lExtCopyright: TLabel;
  lExtCount: TLabel;
  lArch: TLabel;
  lExtName: TLabel;
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
  lDotCurr: TLabel;
  lHiOver1: TLabel;
  lLounder1: TLabel;
  lTray: TLabel;
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
  pnMisc: TPanel;
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
  pDecimal: TPanel;
  pnBackend: TPanel;
  pnHelp: TPanel;
  pTray: TPanel;
  pUserColor: TPanel;
  pcMain: TPageControl;
  pnDisplay: TPanel;
  pUserNick: TPanel;
  pUserSave2: TPanel;
  cbTirColorBg: TRadioButton;
  rbUnit: TRadioGroup;
  seTIR: TSpinEdit;
  seTTSRate: TSpinEdit;
  spTHRESHOLD: TSpinEdit;
  spDeltaMax: TSpinEdit;
  spTHRESHOLD1: TSpinEdit;
  tsAccess: TTabSheet;
  tsProxy: TTabSheet;
  tsCommon: TTabSheet;
  tsAdvanced: TTabSheet;
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
  procedure cbMediaDisableChange(Sender: TObject);
  function validateUser(var error: string): boolean;
  procedure bAddClick({%H-}Sender: TObject);
  procedure bBadgeFlashHelpClick({%H-}Sender: TObject);
  procedure bColorGraphHelpClick({%H-}Sender: TObject);
  procedure bCommonClick({%H-}Sender: TObject);
  procedure bCustomRangeHelpClick({%H-}Sender: TObject);
  procedure bDotHelpClick({%H-}Sender: TObject);
  procedure bExtOpenClick({%H-}Sender: TObject);
  procedure bFontHelpClick({%H-}Sender: TObject);
  procedure bLanguageHelpClick(Sender: TObject);
  procedure bLimitsClick(Sender: TObject);
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
  procedure bTemplateCurrentClick(Sender: TObject);
  procedure bTemplateTrendClick(Sender: TObject);
  procedure bTestAnnounceClick(Sender: TObject);
  procedure bTestClick(Sender: TObject);
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
  procedure Button2Click(Sender: TObject);
  procedure Button3Click(Sender: TObject);
  procedure Button4Click(Sender: TObject);
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
  procedure dotClick(Sender: TObject);
  procedure edCommaSep1Change(Sender: TObject);
  procedure edCommaSepChange(Sender: TObject);
  procedure edNickChange(Sender: TObject);
  procedure eDotChange(Sender: TObject);
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
  procedure lbExtensionsSelectionChange(Sender: TObject; User: boolean);
  procedure lbUsersEnter(Sender: TObject);
  procedure lbUsersSelectionChange(Sender: TObject; User: boolean);
  procedure lConfigPredictClick({%H-}Sender: TObject);
  procedure lDot1Click({%H-}Sender: TObject);
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
  procedure tsCommonShow(Sender: TObject);
  procedure tsDisplayShow(Sender: TObject);
  procedure tsProxyShow(Sender: TObject);
  procedure tsSystemShow(Sender: TObject);
  procedure closeClick(Sender: TObject);
private
  FProxyLoading: boolean;
  procedure LoadProxySettingsIntoUI;
  procedure SaveProxySettingsFromUI;
  procedure getAPILabels(out user, pass: string);
public
  chroma: TRazerChromaBase;
end;

var
tnative: TrndiNative;

resourcestring
RS_EMPTY_PROXY = 'Proxy host is empty.';

RS_DRIVER_CONTRIBUTOR = 'Driver contributor: ';

RS_DEBUG_BACKEND_LABEL = '(Ignored for debug backend)';

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

RS_NOTIFICATION_HELP =
  'Trndi will show a system notification (if available) when your reading is in a dangerous state. Alerts will trigger once when hi/low and every 15 minutes when readings are missing.';

RS_COLOR_BG =
  'Trndi can draw a rectangle/background in the graph''s background, to visually represent your high and low limits.';

RS_OVERRIDE_LANGUAGE =
  'Choose which language to run Trndi in. Should you choose the wrong one, you can press the Windows/"meta" button at start to force English!';

RS_DOT_CLICK =
  'Dot appearance is modified below!';

RS_DOT_HELP_TITLE =
  'Changing the trend dots';

RS_DOT_HELP =
  'You can change the design of the trend dots here, by using Unicode characters. Enter the Unicode, or use a template.'+sLineBreak+'There are two dots, the normal dot and the current reading (when the dots are expanded/clicked)';

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

RS_DEBUG_WARN =
  'This is a debug backend. It''s used for testing purposes only!'+sLineBreak+'No data will be sent to any remote server.';

RS_ERR_PASSWORD = 'You must enter a password';
RS_ERR_EMAIL = 'You must enter a valid e-mail address';
RS_ERR_ADDRESS = 'Address must start with http(s)://';
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
RS_NEWVER_PRE =
  'A new pre-release for %s is available, would you like to go to the downloads page?';
RS_NEWVER_CAPTION = 'New version available';
RS_SELECT_FONT = 'Select a font';
RS_SELECT_FONT_DESC = 'Choose a font to use';
RS_SELECT_FONT_ARROW = 'Select a font for the arrow';
RS_SELECT_FONT_READING = 'Select a font for the reading';
RS_SELECT_FONT_TIME = 'Select a font for the time';
RS_UPDATE_SNOOZE = 'You will be alerted again after %s';

RS_MIN_MINUTES = 'When calculatingg time-in-range, do so over this amount of minutes. If the number is higher than the data available, the max time will be used - this is currently %d minutes.';

RS_NOTIFICATIONS = 'Notifications';
RS_NOTIFY_TITLE = 'A notification system is required';
RS_NOTIFY_TXT =
  'Trndi uses a system called "%s" to send desktop notices, you need to have this system installed in order to recieve notices.';
RS_NOTIFY_SYSTEM =
  'Notifications will appear where you normally get notification messages.';

RS_HASTOUCH = 'Shows if Trndi detected a touch screen';
RS_WEBAPI = 'Trndi can expose a <b>WebAPI</b> for use with <i>third-party systems</i>.<br><br>This is an advanced feature, you can safely disregard it if you don''t know what it does!';

RS_Saftey_Hi =
  'Trndi won''t allow a larger limit, for your own saftey. This can be overridden manually/via plugin';
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

RS_TEST_UNSUPPORTED = 'Sorry! Trndi does not (yet) support connection testing for this service!';
RS_TEST_SUCCESS = 'Successfully connected!';
RS_TEST_FAIL = 'Could not connect!';

RS_GRAPH_ICON_TITLE = 'Choose icon';
RS_GRAPH_ICON_GRAPH = 'Choose an icon for points in the graph';
RS_GRAPH_ICON_GRAPH_DESC = 'This will be used for graph points';
RS_GRAPH_ICON_CURRENT = 'Choose an icon for the current point in the graph';
RS_GRAPH_ICON_CURRENT_DESC = 'This will be used for curent graph point';

RS_POS_TITLE = 'Restricted Feature';
RS_POS = 'This feature depends on your Window Manager (WM). %s may not support this feature.';
RS_POS_UNKNOWN = 'This feature depends on your Window Manager. It may not support this feature.';

RS_SHORTMODE_FULL = 'By default, Trndi only shows up/down/straight. You can enable the full range of arrows here - however you must realize that this is very experimental and potentially less accurate!';

RS_NO_EXTENSIONS = 'This version of Trndi does not support extensions';
RS_NO_EXTENSIONS_COPYRIGHT = 'Please download a version of Trndi that supports extensions';
RS_NO_EXTENSIONS_SYSTEM = 'Due to hardware limitations, %s on %s cannot support extensions';

RS_Announce_Not_Available = 'The text-to-speech (TTS) software "%s" is not available.';

var
fConf: TfConf;

const
GRAPH_ICONS: array[0..9] of unicodestring = (
  WChar($2B24), // \u2b24 BLACK LARGE CIRCLE (default)
  WChar($25CF), // \u25cf BLACK CIRCLE
  WChar($26AB), // \u26ab MEDIUM BLACK CIRCLE
  WChar($25CB), // \u25cb WHITE CIRCLE
  WChar($26AA), // \u26aa MEDIUM WHITE CIRCLE
  WChar($2022), // \u2022 BULLET
  WChar($2219), // \u2219 BULLET OPERATOR (thinner)
  WChar($25AA), // \u25aa BLACK SMALL SQUARE
  WChar($25A0), // \u25a0 BLACK SQUARE
  WChar($25C6)  // \u25c6 BLACK DIAMOND
  );

FRESH_ICONS: array[0..9] of unicodestring = (
  WChar($2600), // \u2600 SUN (default)
  WChar($2605), // \u2605 BLACK STAR
  WChar($2606), // \u2606 WHITE STAR
  WChar($2736), // \u2736 SIX POINTED BLACK STAR
  WChar($272A), // \u272a CIRCLED WHITE STAR
  WChar($25B6), // \u25b6 BLACK RIGHT-POINTING TRIANGLE (\u201dnu\u201d-arrow)
  WChar($25C0), // \u25c0 BLACK LEFT-POINTING TRIANGLE
  WChar($25B2), // \u25b2 BLACK UP-POINTING TRIANGLE
  WChar($25BC), // \u25bc BLACK DOWN-POINTING TRIANGLE
  WChar($25CE)  // \u25ce BULLSEYE
  );

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


function CodepointHex(const s: unicodestring): string;
begin
  if s = '' then
    Exit('');
  Result := IntToHex(Ord(s[1]), 4);
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
var
  f: TStringList;
  l: string;
begin
  l := lbExtensions.GetSelectedText;
  f := TStringList.Create;
  f.Delimiter := #10;
  f.LoadFromFile(l);
  lExtName.Caption := '';
  lExtCopyright.Caption := '';


  if (f.Count > 1 ) and (length(f.Strings[0]) > 2) and (f.Strings[0][2] = '*') then
  begin // /*
    lExtName.Caption := TrimLeftSet(f.Strings[0], ['*', '/', ' ']);

    if (length(f.strings[1]) > 2) and (f.Strings[1][length(f.strings[1])-1] = '*') then // */
      lExtCopyright.Caption := TrimRightSet(f.Strings[1], ['*', '/']);
  end
  else
    lExtName.Caption := RS_NO_COPYRIGHT;
  f.Free;
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
  pcMain.ActivePage := tsAdvanced;
end;

procedure TfConf.lDot1Click(Sender: TObject);
begin
  ShowMessage(RS_DOT_CLICK);
end;

procedure TfConf.getAPILabels(out user, pass: string);
var
  sys: class of TrndiAPI;
begin
  sys := TrndiAPI;

  case cbSys.Text of
  API_NS:
    sys := NightScout;
  API_NS3:
    sys := NightScout3;
  API_DEX_USA:
    sys := Dexcom;
  API_DEX_EU:
    sys := Dexcom;
  API_DEX_NEW_USA:
    sys := DexcomNew;
  API_DEX_NEW_EU:
    sys := DexcomNew;
  API_DEX_NEW_JP:
    sys := DexcomNew;
  API_TANDEM_USA:
    sys := TandemUSA;
  API_TANDEM_EU:
    sys := TandemEU;
  API_XDRIP:
    sys := xDrip;
  {$ifdef Debug}
  API_D_FIRSTX:
    sys := DebugFirstXMissingAPI;
  API_D_INTERMITTENT:
    sys := DebugIntermittentMissingAPI;
  API_D_SLOW:
    sys := DebugSlowAPI;
  API_D_CUSTOM:
    sys := DebugCustomAPI;
  {$endif}
  end;

  user := sys.ParamLabel(APLUser);
  pass := sys.ParamLabel(APLPass);

  {$ifdef DEBUG}
  if (cbSys.Text in API_DEBUG) and (sys = TrndiAPI) then
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
    warn: unicodestring = '(!) ';
    info: unicodestring =  '';
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
      lSysWarnInfo.Caption := UnicodeString(info+UnicodeString(RS_DEX));
      pnSysWarn.Color := $0053A2E8;
    end;
    API_NS3:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := UnicodeString(warn+UnicodeString(RS_BETA));
    end;
    API_DEX_NEW_EU,
    API_DEX_NEW_USA,
    API_DEX_NEW_JP:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := UnicodeString(warn+UnicodeString(RS_BETA_DEX));
    end;
    API_XDRIP:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := UnicodeString(info+UnicodeString(RS_XDRIP));
    end;
    API_TANDEM_EU,
    API_TANDEM_USA:
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := UnicodeString(warn+UnicodeString(RS_TANDEM));
    end;
    end;
    {$ifdef DEBUG}
    if cbSys.Text in API_DEBUG then
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := UnicodeString(warn+UnicodeString(RS_DEBUG_WARN));
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
end;

procedure TfConf.cbUserClick(Sender: TObject);
begin

end;

procedure TfConf.cbUserColorChanged(Sender: TObject);
begin
  btUserSave.Enabled := true;
end;

procedure TfConf.dotClick(Sender: TObject);
var
  bg: tcolor;
begin
  eDot.SetFocus;
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

procedure TfConf.eDotChange(Sender: TObject);
var
  i: integer;
  lbl: TLabel;
begin
  if Sender = eDot then
    lbl := lDot
  else
    lbl := lDotNow;

  if tryStrToInt('$' + (Sender as TEdit).Text, i) then
  begin
    lbl.Caption := UnicodeString(WChar(i));
    if Sender = eDot then
    begin
      lDot1.Caption := lbl.Caption;
      lDot2.Caption := lbl.Caption;
      lDot3.Caption := lbl.Caption;
    end
    else
      lDotCurr.Caption := lbl.Caption;
  end
  else
    lbl.Caption := '- ERROR -';
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
var
  usr,pass: string;
begin
  usr := eAddr.Text;
  pass := ePass.Text;

  result := true;
  case cbSys.Text of
  API_NS,
  API_NS3:
  begin
    result := usr.StartsWith('http');
    error := RS_ERR_ADDRESS;
  end;
  API_TANDEM_EU,
  API_TANDEM_USA:
  begin
    result := usr.Contains('@');
    error := RS_ERR_EMAIL;
    if not result then
      Exit;

    result := pass.Length > 4;
    error := RS_ERR_PASSWORD;
  end;
  API_DEX_EU,
  API_DEX_USA,
  API_DEX_NEW_EU,
  API_DEX_NEW_USA,
  API_DEX_NEW_JP:
  begin
    result := pass.Length > 4;
    error := RS_ERR_PASSWORD;
  end;
  end;

end;

procedure TfConf.cbMediaDisableChange(Sender: TObject);
begin
  gbMedia.Enabled := not cbMediaDisable.checked;
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
      if not (c in ['0'..'9', 'A'..'z', ' ']) then
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

procedure TfConf.bDotHelpClick(Sender: TObject);
begin
  if ExtText(uxdAuto, RS_DOT_HELP_TITLE, RS_DOT_HELP, [mbOK, mbUXRead]) <> mrOK then
    OpenURL('https://www.compart.com/en/unicode/U+2B24');
end;

procedure TfConf.bExtOpenClick(Sender: TObject);
begin
  OpenDocument(eExt.Text);
end;

procedure TfConf.bFontHelpClick(Sender: TObject);
begin
  ShowMessage(RS_FONT_HELP);
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
  i: integer;
  sys: class of TrndiAPI;
begin

  s := '';

  {$ifdef DEBUG}
  if cbSys.Text in API_DEBUG then
    sys := DebugAPI;
  {$endif}

  case cbSys.Text of
  API_NS:
    sys := NightScout;
  API_NS3:
    sys := NightScout3;
  API_DEX_USA,
  API_DEX_EU:
    sys := Dexcom;
  API_DEX_NEW_USA,
  API_DEX_NEW_EU,
  API_DEX_NEW_JP:
    sys := DexcomNew;
  API_TANDEM_USA:
    sys := TandemUSA;
  API_TANDEM_EU:
    sys := TandemEU;
  API_XDRIP:
    sys := xDrip;
  {$ifdef Debug}
  API_D_FIRSTX:
    sys := DebugFirstXMissingAPI;
  API_D_CUSTOM:
    sys := DebugCustomAPI;
  API_D_SLOW:
    sys := DebugSlowAPI;
  {$endif}
  end;

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
  'BurntToast':
    url := 'https://www.powershellgallery.com/packages/BurntToast';
  'notify-send':
    url := 'https://www.google.com/search?q=notify-send';
  end;
  // Natives:
  // NSUserNotification
  // gdbus

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

procedure TfConf.bTemplateCurrentClick(Sender: TObject);
var
  res: integer;
begin
  res := ExtList(uxdAuto,RS_GRAPH_ICON_TITLE,RS_GRAPH_ICON_CURRENT,RS_GRAPH_ICON_CURRENT_DESC, FRESH_ICONS);
  if res < 0 then
    Exit;

  eDotNow.Text := CodePointHex(FRESH_ICONS[res]);
end;

procedure TfConf.bTemplateTrendClick(Sender: TObject);
var
  res: integer;
begin
  res := ExtList(uxdAuto,RS_GRAPH_ICON_TITLE, RS_GRAPH_ICON_GRAPH, RS_GRAPH_ICON_GRAPH_DESC, GRAPH_ICONS);

  if res < 0 then
    Exit;

  eDot.Text := CodePointHex(GRAPH_ICONS[res]);
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
begin
  if cbSys.Text = API_NS then
    res := NightScout.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_NS3 then
    res := NightScout3.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_DEX_USA then
    res := DexcomUSA.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_DEX_EU then
    res := DexcomWorld.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_DEX_NEW_USA then
    res := DexcomUSANew.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_DEX_NEW_EU then
    res := DexcomWorldNew.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_DEX_NEW_JP then
    res := DexcomNew.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_TANDEM_USA then
    res := TandemUSA.testConnection(eAddr.text,ePass.text,err)
  else
  if cbSys.Text = API_TANDEM_EU then
    res := TandemEU.testConnection(eAddr.text,ePass.text,err)
  else
  begin
    ShowMessage(RS_TEST_UNSUPPORTED);
    exit;
  end;

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
begin
  cbTTSVoice.Items.Clear;
  cbTTSVoice.Items.Add('Default');

  {$ifdef X_WIN}
  if tnative.SpeakAvailable then
  begin
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
  end;
  {$endif}
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
    ShowMessage('No settings to export.');
    Exit;
  end;
  
  encodedData := EncodeStringBase64(settingsData);
  
  dlg := TSaveDialog.Create(nil);
  try
    dlg.Title := 'Export Settings';
    dlg.Filter := 'Settings files (*.trndi)|*.trndi|All files (*.*)|*.*';
    dlg.DefaultExt := 'trndi';
    dlg.FileName := 'trndi_settings.trndi';
    
    if dlg.Execute then
    begin
      with TFileStream.Create(dlg.FileName, fmCreate) do
      try
        WriteBuffer(encodedData[1], Length(encodedData));
      finally
        Free;
      end;
      ShowMessage('Settings exported successfully.');
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
    dlg.Title := 'Import Settings';
    dlg.Filter := 'Settings files (*.trndi)|*.trndi|All files (*.*)|*.*';
    dlg.DefaultExt := 'trndi';
    
    if dlg.Execute then
    begin
      with TFileStream.Create(dlg.FileName, fmOpenRead) do
      try
        SetLength(encodedData, Size);
        ReadBuffer(encodedData[1], Size);
      finally
        Free;
      end;
      
      try
        settingsData := DecodeStringBase64(encodedData);
        tnative.ImportSettings(settingsData);
        ShowMessage('Settings imported successfully. You need to restart Trndi for all changes to take effect. Do NOT save when exiting the settings dialog!');
      except
        on E: Exception do
          ShowMessage('Error importing settings: ' + E.Message);
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfConf.btResetClick(Sender: TObject);
const
  bg_color_ok: TColor = $0000DC84;
  bg_color_ok_txt: TColor = $00F2FFF2;
  // Hi
  bg_color_hi: TColor = $0007DAFF;
  bg_color_hi_txt: TColor = $000052FB;
  // Low
  bg_color_lo: TColor = $00FFBE0B;
  bg_color_lo_txt: TColor = $00FFFEE9;

  // Personal hi
  bg_rel_color_lo: TColor = $00A859EE;
  bg_rel_color_lo_txt: TColor = $002D074E;
  // Personal low
  bg_rel_color_hi: TColor = $0072C9DE;
  bg_rel_color_hi_txt: TColor = $001C6577;
begin
  cl_ok_bg.ButtonColor := bg_color_ok;
  cl_hi_bg.ButtonColor := bg_color_hi;
  cl_lo_bg.ButtonColor := bg_color_lo;

  cl_ok_txt.ButtonColor := bg_color_ok_txt;
  cl_hi_txt.ButtonColor := bg_color_hi_txt;
  cl_lo_txt.ButtonColor := bg_color_lo_txt;

  cl_hi_bg_cust.ButtonColor := bg_rel_color_hi;
  cl_lo_bg_cust.ButtonColor := bg_rel_color_lo;

  cl_hi_txt_cust.ButtonColor := bg_rel_color_hi_txt;
  cl_lo_txt_cust.ButtonColor := bg_rel_color_lo_txt;
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
  Openurl('https://github.com/slicke/trndi/blob/main/LANGUAGES.md');
end;

{------------------------------------------------------------------------------
  Apply the display panel's font to preview labels.

  This lets the user quickly see how the selected UI font looks on key
  readouts (value, arrow, and "ago") without changing global styles.
------------------------------------------------------------------------------}
procedure TfConf.Button2Click(Sender: TObject);
begin
  // Mirror the panel font to individual preview labels
  lVal.Font.Name := pnDisplay.Font.Name;   // main value readout
  lArrow.Font.Name := pnDisplay.Font.Name; // trend arrow glyph/text
  lAgo.Font.Name := pnDisplay.Font.Name;   // time since last reading
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
    s := Format(RS_NEWVER, [rn]);
    if UXDialog(uxdAuto, RS_NEWVER_CAPTION, s, [mbYes, mbNo], mtInformation) = mrYes then
      OpenURL(r);
  end
  else
    ShowMessage(RS_UPTODATE);
end;

procedure TfConf.Button4Click(Sender: TObject);
begin
  eDot.Text := '2B24';
  eDotNow.Text := '2600';
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

procedure TfConf.cbPredictionsChange(Sender: TObject);
begin
  if (cbPredictions.Checked) and self.Showing then
    ShowMessage(RS_PredictionWarn);

  cbPredictShort.Enabled := cbPredictions.Checked;
  cbPredictShortFullArrows.Enabled := cbPredictShort.Enabled;
  cbPredictShortMinutes.Enabled := cbPredictShort.Enabled;

end;

procedure TfConf.cbPredictShortChange(Sender: TObject);
begin
  cbPredictShortFullArrows.Enabled := cbPredictShort.Checked;
  rbPredictShortShowValue.Enabled := cbPredictShort.Checked;
  rbPredictShortArrowOnly.Enabled := cbPredictShort.Checked;
  cbPredictShortMinutes.Enabled := cbPredictShort.Checked;
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
  lVersion.Caption := GetProductVersionMajorMinor('2.x');
  // If CI embedded a real build number, append it
  if CI and (BUILD_NUMBER <> 'dev') then
    lVersion.Caption := lVersion.Caption + '.' + BUILD_NUMBER
  else
    lVersion.Caption := Format('%s-dev (%s)', [lVersion.Caption,
      StringReplace({$I %DATE%}, '/', '-', [rfReplaceAll])]);
  lversion.left := lversion.left - 20;

  pcMain.ActivePage := tsGeneral;
  {$ifdef X_MAC}
  self.width := self.width + (self.width div 9);
  {$endif}
  {$ifdef lclqt6}
  self.font.size := 10;
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

  // Populate TTS voice list
  PopulateTTSVoices;

  {$ifdef X_MAC}
  edTray.Enabled := false; // No support
  {$endif}

  // Initialize parameter labels for current backend selection
  cbSysChange(Self);

  bcommon.Caption := WChar(2699) + ' ' + bCommon.Caption;

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
var
  l: tlabel;
begin
  // The label caption looks ugly and invalidate doesnt help. Neither does redraw at this stage
  for l in [lDot1, lDot2, lDot3, lDotCurr] do
    l.Caption := '';
  // Update preview glyphs from current edit values so they appear when the tab is shown
  eDot.OnChange(eDot);
  eDotNow.OnChange(eDotNow);

  if lVal.Font.color = lVal.Parent.Color then
    lVal.Font.color := IfThen(lVal.Parent.Color = clBlack, clWhite, clBlack);

  if lArrow.Font.color = lArrow.Parent.Color then
    lArrow.Font.color := IfThen(lArrow.Parent.Color = clBlack, clWhite, clBlack);
  if lArrow.Caption = '' then
    lArrow.caption := '→';

  if lAgo.Font.color = lAgo.Parent.Color then
    lAgo.Font.color := IfThen(lAgo.Parent.Color = clBlack, clWhite, clBlack);

  if lDot1.font.color = lDot1.Parent.color then
  begin
    lDot1.Font.color := IfThen(lDot1.Parent.Color = clBlack, clWhite, clBlack);
    lDot2.Font.color := ldot1.font.color;
    lDot3.Font.color := ldot1.font.color;
  end;
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
  lWaitSys.Visible := false;
  lProblematic.Visible := false;
  if IsProblematicWM then
    if not IsSemiProblematicWM then
      lProblematic.Visible := true;

  pnSysInfo.Show;
end;

end.
