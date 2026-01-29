
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
 *)

unit uconf;

{$I ../../inc/native.inc}

interface

uses
Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, Forms, Controls,
Graphics, Dialogs, LCLTranslator, trndi.native, lclintf,
slicke.ux.alert, slicke.ux.native, VersionInfo, trndi.funcs, buildinfo, StrUtils,
  // Backend APIs for label captions
trndi.api, trndi.api.nightscout, trndi.api.nightscout3, trndi.api.dexcom, trndi.api.debug_custom,
trndi.api.debug, trndi.api.debug_firstXmissing, trndi.api.xdrip, RazerChroma;

type

  { TfConf }

TfConf = class(TForm)
  bAdd: TButton;
  bBackendHelp: TButton;
  bDotHelp: TButton;
  bFontHelp: TButton;
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
  bTimeStampHelp: TButton;
  bUseURLHelp: TButton;
  bThreasholdLinesHelp: TButton;
  bOutdatedHelp: TButton;
  bBadgeFlashHelp: TButton;
  bPrivacyHelp: TButton;
  bPredictHelp: TButton;
  bTest: TButton;
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
  bTestSpeech: TButton;
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
  edCommaSep1: TEdit;
  eDot: TEdit;
  eDotNow: TEdit;
  eExt: TEdit;
  cbFonts: TGroupBox;
  fsHi1: TFloatSpinEdit;
  fsLo1: TFloatSpinEdit;
  gbOverride1: TGroupBox;
  Label16: TLabel;
  Label17: TLabel;
  Label2: TLabel;
  Label25: TLabel;
  Label33: TLabel;
  Label34: TLabel;
  lConfigPredict: TLabel;
  lDot: TLabel;
  lDot1: TLabel;
  lDot2: TLabel;
  lDot3: TLabel;
  lDotNow: TLabel;
  lExt: TLabel;
  lHiOver3: TLabel;
  lLounder2: TLabel;
  lSysWarnInfo: TLabel;
  Panel18: TPanel;
  Panel19: TPanel;
  Panel20: TPanel;
  Panel3: TPanel;
  pnDeltaMax: TPanel;
  pnMisc1: TPanel;
  pnSysWarn: TPanel;
  rbPredictShortArrowOnly: TRadioButton;
  rbPredictShortShowValue: TRadioButton;
  cbTimeStamp: TCheckBox;
  cbTirIcon: TCheckBox;
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
  gbOverride2: TGroupBox;
  gbOverride3: TGroupBox;
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
  lTestAnnounce1: TLabel;
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
  spTHRESHOLD: TSpinEdit;
  spDeltaMax: TSpinEdit;
  spTHRESHOLD1: TSpinEdit;
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
  procedure bAddClick(Sender: TObject);
  procedure bBadgeFlashHelpClick(Sender: TObject);
  procedure bColorGraphHelpClick(Sender: TObject);
  procedure bCommonClick(Sender: TObject);
  procedure bCustomRangeHelpClick(Sender: TObject);
  procedure bDotHelpClick(Sender: TObject);
  procedure bExtOpenClick(Sender: TObject);
  procedure bFontHelpClick(Sender: TObject);
  procedure bLanguageHelpClick(Sender: TObject);
  procedure bLimitsClick(Sender: TObject);
  procedure bMinMinutesHelpClick(Sender: TObject);
  procedure bMultiUserHelpClick(Sender: TObject);
  procedure bNotificationHelpClick(Sender: TObject);
  procedure bDeltaMaxHelpClick(Sender: TObject);
  procedure bOutdatedHelpClick(Sender: TObject);
  procedure bOverrideHelpClick(Sender: TObject);
  procedure bPredictHelpClick(Sender: TObject);
  procedure bPredictHorizonClick(Sender: TObject);
  procedure bPrivacyHelpClick(Sender: TObject);
  procedure bRemoveClick(Sender: TObject);
  procedure bBackendHelpClick(Sender: TObject);
  procedure bSysNoticeClick(Sender: TObject);
  procedure bSysTouchClick(Sender: TObject);
  procedure bTemplateCurrentClick(Sender: TObject);
  procedure bTemplateTrendClick(Sender: TObject);
  procedure bTestAnnounceClick(Sender: TObject);
  procedure bTestClick(Sender: TObject);
  procedure bTestSpeechClick(Sender: TObject);
  procedure bThreasholdLinesHelpClick(Sender: TObject);
  procedure bTimeStampHelpClick(Sender: TObject);
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
  procedure cbSysChange(Sender: TObject);
  procedure cbUserClick(Sender: TObject);
  procedure cbUserColorChanged(Sender: TObject);
  procedure dotClick(Sender: TObject);
  procedure edCommaSep1Change(Sender: TObject);
  procedure edCommaSepChange(Sender: TObject);
  procedure edNickChange(Sender: TObject);
  procedure eDotChange(Sender: TObject);
  procedure ePassEnter(Sender: TObject);
  procedure ePassExit(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure FormResize(Sender: TObject);
  procedure fsHi1Change(Sender: TObject);
  procedure fsHiChange(Sender: TObject);
  procedure fsLo1Change(Sender: TObject);
  procedure fsLoChange(Sender: TObject);
  procedure Label12Click(Sender: TObject);
  procedure lAckClick(Sender: TObject);
  procedure lbExtensionsSelectionChange(Sender: TObject; User: boolean);
  procedure lbUsersEnter(Sender: TObject);
  procedure lbUsersSelectionChange(Sender: TObject; User: boolean);
  procedure lConfigPredictClick(Sender: TObject);
  procedure lDot1Click(Sender: TObject);
  procedure lLicenseClick(Sender: TObject);
  procedure lSysWarnInfoClick(Sender: TObject);
  procedure lValClick(Sender: TObject);
  procedure pcColorsChange(Sender: TObject);
  procedure pcMainChange(Sender: TObject);
  procedure rbUnitClick(Sender: TObject);
  procedure spTHRESHOLD1Change(Sender: TObject);
  procedure spTHRESHOLDChange(Sender: TObject);
  procedure tbAdvancedChange(Sender: TObject);
  procedure ToggleBox1Change(Sender: TObject);
  procedure tsCommonShow(Sender: TObject);
  procedure tsDisplayShow(Sender: TObject);
  procedure tsSystemShow(Sender: TObject);
private
  procedure getAPILabels(out user, pass: string);
public
  chroma: TRazerChromaBase;
end;

const
API_NS = 'NightScout';
API_NS3 = 'NightScout v3';
API_DEX_USA = 'Dexcom (USA)';
API_DEX_EU = 'Dexcom (Outside USA)';
API_XDRIP = 'xDrip';
{$ifdef DEBUG}
API_D_DEBUG =  '* Debug Backend *';
API_D_MISSING = '* Debug Missing Backend *';
API_D_PERFECT = '* Debug Perfect Backend *';
API_D_CUSTOM = '* Debug Custom Backend *';
API_D_EDGE = '* Debug Edge Backend *';
API_D_FIRST = '* Debug First Reading Missing *';
API_D_SECOND = '* Debug Second Reading Missing *';
API_D_FIRSTX = '* Debug First X Readings Missing *';

API_DEBUG: array of string = (API_D_DEBUG, API_D_MISSING, API_D_PERFECT, API_D_CUSTOM, API_D_EDGE, API_D_FIRST, API_D_SECOND, API_D_FIRSTX);
{$endif}
var
tnative: TrndiNative;

resourcestring
RS_DRIVER_CONTRIBUTOR = 'Driver contributor: ';

RS_DEBUG_BACKEND_LABEL = '(Ignored for debug backend)';

RS_Multi_User_Help =
  'Trndi supports more than one user, this is called the multi user mode.'+LineEnding+'In this section you can add/remove accounts. There''s always a default account which cannot be deleted.'+LineEnding+
  'Accounts have their own settings and remote servers, and are "sandboxed". Start a new instance of Trndi to log in to a given user account.';

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

RS_XDRIP =
  'Make sure you are on the same network as the xDrip app.'+sLineBreak+'Make sure that web access is turned on.';

RS_DEBUG_WARN =
  'This is a debug backend. It''s used for testing purposes only!'+sLineBreak+'No data will be sent to any remote server.';

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

procedure ShowMessage(const str: string);
begin
  UXMessage(sSuccTitle, str, uxmtInformation);
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


function GetLanguageName(const ACode: string): string;
var
  C: string;
begin
  C := LowerCase(Trim(ACode));

  case C of
  'aa':
    Result := 'Afar';
  'ab':
    Result := 'Abkhazian';
  'ae':
    Result := 'Avestan';
  'af':
    Result := 'Afrikaans';
  'ak':
    Result := 'Akan';
  'sq':
    Result := 'Albanian';
  'am':
    Result := 'Amharic';
  'ar':
    Result := 'Arabic';
  'an':
    Result := 'Aragonese';
  'hy':
    Result := 'Armenian';
  'as':
    Result := 'Assamese';
  'av':
    Result := 'Avaric';
  'ay':
    Result := 'Aymara';
  'az':
    Result := 'Azerbaijani';
  'bm':
    Result := 'Bambara';
  'ba':
    Result := 'Bashkir';
  'eu':
    Result := 'Basque';
  'be':
    Result := 'Belarusian';
  'bn':
    Result := 'Bengali';
  'bh':
    Result := 'Bihari';
  'bi':
    Result := 'Bislama';
  'bs':
    Result := 'Bosnian';
  'br':
    Result := 'Breton';
  'bg':
    Result := 'Bulgarian';
  'my':
    Result := 'Burmese';
  'ca':
    Result := 'Catalan';
  'ch':
    Result := 'Chamorro';
  'ce':
    Result := 'Chechen';
  'ny':
    Result := 'Chichewa';        // also Nyanja
  'zh':
    Result := 'Chinese';
  'cu':
    Result := 'Church Slavonic';
  'cv':
    Result := 'Chuvash';
  'kw':
    Result := 'Cornish';
  'co':
    Result := 'Corsican';
  'cr':
    Result := 'Cree';
  'hr':
    Result := 'Croatian';
  'cs':
    Result := 'Czech';
  'da':
    Result := 'Danish';
  'dv':
    Result := 'Divehi';
  'nl':
    Result := 'Dutch';
  'dz':
    Result := 'Dzongkha';
  'en':
    Result := 'English';
  'eo':
    Result := 'Esperanto';
  'et':
    Result := 'Estonian';
  'ee':
    Result := 'Ewe';
  'fo':
    Result := 'Faroese';
  'fj':
    Result := 'Fijian';
  'fi':
    Result := 'Finnish';
  'fr':
    Result := 'French';
  'ff':
    Result := 'Fulah';
  'gl':
    Result := 'Galician';
  'ka':
    Result := 'Georgian';
  'de':
    Result := 'German';
  'el':
    Result := 'Greek';
  'gu':
    Result := 'Gujarati';
  'ht':
    Result := 'Haitian Creole';
  'ha':
    Result := 'Hausa';
  'he':
    Result := 'Hebrew';
  'hi':
    Result := 'Hindi';
  'ho':
    Result := 'Hiri Motu';
  'hu':
    Result := 'Hungarian';
  'is':
    Result := 'Icelandic';
  'io':
    Result := 'Ido';
  'ig':
    Result := 'Igbo';
  'id':
    Result := 'Indonesian';
  'ia':
    Result := 'Interlingua';
  'ie':
    Result := 'Interlingue';
  'iu':
    Result := 'Inuktitut';
  'ik':
    Result := 'Inupiaq';
  'ga':
    Result := 'Irish';
  'it':
    Result := 'Italian';
  'ja':
    Result := 'Japanese';
  'jv':
    Result := 'Javanese';
  'kn':
    Result := 'Kannada';
  'kr':
    Result := 'Kanuri';
  'ks':
    Result := 'Kashmiri';
  'kk':
    Result := 'Kazakh';
  'km':
    Result := 'Khmer';
  'ki':
    Result := 'Kikuyu';
  'rw':
    Result := 'Kinyarwanda';
  'rn':
    Result := 'Rundi';
  'kv':
    Result := 'Komi';
  'kg':
    Result := 'Kongo';
  'ko':
    Result := 'Korean';
  'kj':
    Result := 'Kuanyama';
  'ku':
    Result := 'Kurdish';
  'ky':
    Result := 'Kyrgyz';
  'lo':
    Result := 'Lao';
  'la':
    Result := 'Latin';
  'lv':
    Result := 'Latvian';
  'lt':
    Result := 'Lithuanian';
  'lb':
    Result := 'Luxembourgish';
  'mk':
    Result := 'Macedonian';
  'mg':
    Result := 'Malagasy';
  'ms':
    Result := 'Malay';
  'ml':
    Result := 'Malayalam';
  'mt':
    Result := 'Maltese';
  'mi':
    Result := 'Maori';
  'mr':
    Result := 'Marathi';
  'mn':
    Result := 'Mongolian';
  'na':
    Result := 'Nauru';
  'nv':
    Result := 'Navajo';
  'ng':
    Result := 'Ndonga';
  'ne':
    Result := 'Nepali';
  'no':
    Result := 'Norwegian';
  'nb':
    Result := 'Norwegian Bokmål';
  'nn':
    Result := 'Norwegian Nynorsk';
  'nd':
    Result := 'North Ndebele';
  'nr':
    Result := 'South Ndebele';
  'oc':
    Result := 'Occitan';
  'oj':
    Result := 'Ojibwe';
  'om':
    Result := 'Oromo';
  'or':
    Result := 'Odia';
  'os':
    Result := 'Ossetian';
  'pa':
    Result := 'Punjabi';
  'pi':
    Result := 'Pali';
  'pl':
    Result := 'Polish';
  'ps':
    Result := 'Pashto';
  'fa':
    Result := 'Persian';
  'pt':
    Result := 'Portuguese';
  'qu':
    Result := 'Quechua';
  'rm':
    Result := 'Romansh';
  'ro':
    Result := 'Romanian';
  'ru':
    Result := 'Russian';
  'se':
    Result := 'Northern Sami';
  'sm':
    Result := 'Samoan';
  'sg':
    Result := 'Sango';
  'sa':
    Result := 'Sanskrit';
  'sc':
    Result := 'Sardinian';
  'sd':
    Result := 'Sindhi';
  'si':
    Result := 'Sinhala';
  'sk':
    Result := 'Slovak';
  'sl':
    Result := 'Slovenian';
  'so':
    Result := 'Somali';
  'es':
    Result := 'Spanish';
  'su':
    Result := 'Sundanese';
  'sw':
    Result := 'Swahili';
  'ss':
    Result := 'Swati';
  'sv':
    Result := 'Swedish';
  'jm':
    Result := 'Jämtländska (Jamska)';
  'tl':
    Result := 'Tagalog';
  'ty':
    Result := 'Tahitian';
  'tg':
    Result := 'Tajik';
  'ta':
    Result := 'Tamil';
  'tt':
    Result := 'Tatar';
  'te':
    Result := 'Telugu';
  'th':
    Result := 'Thai';
  'bo':
    Result := 'Tibetan';
  'ti':
    Result := 'Tigrinya';
  'ts':
    Result := 'Tsonga';
  'tn':
    Result := 'Tswana';
  'tr':
    Result := 'Turkish';
  'tk':
    Result := 'Turkmen';
  'tw':
    Result := 'Twi';
  'ug':
    Result := 'Uyghur';
  'uk':
    Result := 'Ukrainian';
  'ur':
    Result := 'Urdu';
  'uz':
    Result := 'Uzbek';
  've':
    Result := 'Venda';
  'vi':
    Result := 'Vietnamese';
  'vo':
    Result := 'Volapük';
  'wa':
    Result := 'Walloon';
  'cy':
    Result := 'Welsh';
  'wo':
    Result := 'Wolof';
  'fy':
    Result := 'Western Frisian';
  'xh':
    Result := 'Xhosa';
  'yi':
    Result := 'Yiddish';
  'yo':
    Result := 'Yoruba';
  'za':
    Result := 'Zhuang';
  'zu':
    Result := 'Zulu';
  'auto':
    Result := RS_AUTO;
  else
    Result := c;
  end;
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

procedure TfConf.lAckClick(Sender: TObject);
const
  txt = 'Trndi makes use of the following 3rd party libraries:'#10 +
    'macOS native code libraries by Phil Hess'#10 +
    'WinStyles library by Espectr0'#10#10 +
    'Extensions use JavaScript engine QuickJS by Fabrice Bellard and Charlie Gordo'#10 +
    'integration of said engine is made possible with mORMot2 by Synopse Informatique - Arnaud Bouchez'
    + #10#10 + 'Built in Object Pascal, using the Lazarus component library (LCL) and FreePascal.';
begin
  ExtSucc(uxdAuto, 'Trndi', 'Libraries', txt, $00AA6004, $00FDD8AA);
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
  API_XDRIP:
    sys := xDrip;
  {$ifdef Debug}
  API_D_FIRSTX:
    sys := DebugFirstXMissingAPI;
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

procedure TfConf.cbSysChange(Sender: TObject);
procedure WarnUnstableAPI;
  var
    i : integer;
  begin
    if (cbSys.Text = API_DEX_USA) or (cbSys.Text = API_DEX_EU) then
    begin
      gbOverride.Color := uxclLightBlue;
       lLoUnder.Font.Color := clBlack;
       lHiOver.Font.Color := clBlack;
       cbCust.Font.Color := clBlack;

      pnSysWarn.Show;
      lSysWarnInfo.Caption := RS_DEX;
    end;
    if cbSys.Text = API_NS3 then
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := RS_BETA;
    end;
    if cbSys.Text = API_XDRIP then
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := RS_XDRIP;
    end;
    {$ifdef DEBUG}
    if cbSys.Text in API_DEBUG then
    begin
      pnSysWarn.Show;
      lSysWarnInfo.Caption := RS_DEBUG_WARN;
    end;
    {$endif}
  end;

var user, pass: string;
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
    lbl.Caption := WChar(i);
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

procedure TfConf.ePassEnter(Sender: TObject);
begin
  ePass.PasswordChar := #0;
end;

procedure TfConf.ePassExit(Sender: TObject);
begin
  ePass.PasswordChar := '*';
end;

procedure TfConf.bLimitsClick(Sender: TObject);
begin
end;

procedure TfConf.bMinMinutesHelpClick(Sender: TObject);
begin
  ShowMessage(Format(RS_MIN_MINUTES, [MAX_MIN]));
end;

procedure TfConf.bMultiUserHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Multi_User_Help);
end;

procedure TfConf.bNotificationHelpClick(Sender: TObject);
begin
  ShowMessage(RS_NOTIFICATION_HELP);
end;

procedure TfConf.bDeltaMaxHelpClick(Sender: TObject);
begin
  if UXDialog(uxdAuto,'Delta', RS_DELTA_MAX,[mbOK, mbUxRead]) <> mrOK then
    OpenURL('https://github.com/slicke/trndi/blob/main/doc/DeltaMax.md');
end;

procedure TfConf.bOutdatedHelpClick(Sender: TObject);
begin
  ShowMessage(RS_OUTDATED_HELP);
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
  API_DEX_USA:
    sys := Dexcom;
  API_DEX_EU:
    sys := Dexcom;
  API_XDRIP:
    sys := xDrip;
  {$ifdef Debug}
  API_D_FIRSTX:
    sys := DebugFirstXMissingAPI;
  API_D_CUSTOM:
    sys := DebugCustomAPI;
    {$endif}
  end;

  if not assigned(sys) then
    s := RS_CHOOSE_SYSTEM
  else
  begin
    s := sys.ParamLabel(APLDescHTML);
    c := sys.ParamLabel(APLCopyright);
  end;

  for i := 0 to Length(RS_DRIVER_CONTRIBUTOR + c) do
    x += '-';

  if s <> '' then begin
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
  tnative.attention('Trndi Test', 'test');
end;

procedure TfConf.bTestClick(Sender: TObject);
var
  res: integer;
begin
  if cbSys.Text = API_NS then
    res := NightScout.testConnection(eAddr.text,ePass.text,'')
  else
  if cbSys.Text = API_NS3 then
    res := NightScout3.testConnection(eAddr.text,ePass.text,'')
  else
  if cbSys.Text = API_DEX_USA then
    res := Dexcom.testConnection(eAddr.text,ePass.text,'usa')
  else
  if cbSys.Text = API_DEX_EU then
    res := Dexcom.testConnection(eAddr.text,ePass.text,'eu')
  else
  begin
    ShowMessage(RS_TEST_UNSUPPORTED);
    exit;
  end;

  if res = 0 then
    ShowMessage(RS_TEST_SUCCESS)
  else
    ShowMessage(RS_TEST_FAIL);
end;

procedure TfConf.bTestSpeechClick(Sender: TObject);
begin
  if rbUnit.ItemIndex = 0 then
    tnative.speak('test 5.5')
  else
    tnative.speak('test 100');
end;

procedure TfConf.bThreasholdLinesHelpClick(Sender: TObject);
begin
  ShowMessage(RS_Threashold_Lines_Help);
end;

procedure TfConf.bTimeStampHelpClick(Sender: TObject);
begin
  ShowMessage(RS_TIMESTAMP_HELP);
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

  {$ifdef X_MAC}
  edTray.Enabled := false; // No support
  {$endif}

  // Initialize parameter labels for current backend selection
  cbSysChange(Self);

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
  txt = {$ifndef HAIKU}'<img src="https://trndi.app/doc/img/trndi-logo.png">' + sHTMLLineBreak + {$endif}
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
    API_DEX_USA, API_DEX_EU:
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
  f := ExtFontPicker(uxdAuto,RS_SELECT_FONT, RS_SELECT_FONT, title, (sender as TLabel).font, (sender as TLabel).caption, mr);
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
