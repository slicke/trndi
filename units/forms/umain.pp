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

unit umain;

{$I ../../inc/native.inc}

{$ifdef Darwin}
{$modeswitch objectivec1}
{$endif}


interface

uses
trndi.strings, LCLTranslator, Classes, Menus, SysUtils, Forms, Controls,
Graphics, Dialogs, StdCtrls, ExtCtrls,
trndi.api.dexcom, trndi.api.nightscout, trndi.api.nightscout3, trndi.types,
Math, DateUtils, FileUtil, LclIntf, TypInfo, LResources,
slicke.ux.alert, slicke.ux.native, usplash, Generics.Collections, trndi.funcs,
Trndi.native.base, trndi.shared, trndi.api.debug_custom, buildinfo, fpjson, jsonparser,
SystemMediaController,
{$ifdef TrndiExt}
trndi.Ext.Engine, trndi.Ext.jsfuncs, trndi.ext.promise, mormot.core.base,
{$endif}
{$ifdef Darwin}
CocoaAll, MacOSAll,
{$endif}
{$ifdef LINUX}
kdebadge,
{$endif}
LazFileUtils, uconf, trndi.native, Trndi.API,
trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug, trndi.api.debug_edge, trndi.api.debug_missing, trndi.api.debug_perfect,{$endif}
{$ifdef LCLQt6}Qt6, QtWidgets,{$endif}
StrUtils, TouchDetection, ufloat, LCLType, trndi.webserver.threaded;

type
TFloatIntDictionary = specialize TDictionary<single, integer>;
  // Specialized TDictionary
  // Procedures which are applied to the trend drawing
TTrendProc = procedure(l: TPaintBox; c, ix: integer) of object;
TTrendProcLoop = procedure(l: TPaintBox; c, ix: integer;
  ls: array of TPaintbox) of object;
TrndiPos = (tpoCenter = 0, tpoBottomLeft = 1, tpoBottomRight = 2,
  tpoCustom = 3, tpoTopRight = 4);
TPONames = array[TrndiPos] of string;

var
TrndiPosNames: TPONames = (RS_tpoCenter, RS_tpoBottomLeft,
  RS_tpoBottomRight, RS_tpoCustom, RS_tpoTopRight);

type
  { TfBG }

  {$ifdef Darwin}
{ Custom NSApplicationDelegate class }
TMyAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
public
  function applicationShouldHandleReopen_hasVisibleWindows(
    sender: NSApplication; hasVisibleWindows: Boolean): Boolean; message 'applicationShouldHandleReopen:hasVisibleWindows:';
  function applicationDockMenu(sender: NSApplication): NSMenu; message 'applicationDockMenu:';
    procedure miSettingsMacClick(sender: id); message 'miSettings:';
end;
  {$endif}

TfBG = class(TForm)
  apMain: TApplicationProperties;
  bSettings: TButton;
  lPredict: TLabel;
  miPredict: TMenuItem;
  pnWarnlast: TLabel;
  lRef: TLabel;
  lDot10: TPaintBox;
  lDot2: TPaintBox;
  lDot3: TPaintBox;
  lDot4: TPaintBox;
  lDot5: TPaintBox;
  lDot6: TPaintBox;
  lDot7: TPaintBox;
  lDot8: TPaintBox;
  lDot9: TPaintBox;
  lMissing: TLabel;
  lTir: TLabel;
  lAgo: TLabel;
  miADotAdjust: TMenuItem;
  miDotsInView: TMenuItem;
  miExit: TMenuItem;
  miCustomDots: TMenuItem;
  miDebugBackend: TMenuItem;
  miDotVal: TMenuItem;
  miATouchAuto: TMenuItem;
  miATouchNo: TMenuItem;
  miATouchYes: TMenuItem;
  miASystemInfo: TMenuItem;
  miADots: TMenuItem;
  miATouch: TMenuItem;
  miAdvanced: TMenuItem;
  lDot1: TPaintBox;
  misep: TMenuItem;
  miSplit6: TMenuItem;
  miSplit5: TMenuItem;
  miAnnounce: TMenuItem;
  miDotHuge: TMenuItem;
  miDotBig: TMenuItem;
  miDotNormal: TMenuItem;
  miDotSize: TMenuItem;
  miAlternate: TMenuItem;
  miHistory: TMenuItem;
  miClock: TMenuItem;
  miRangeColor: TMenuItem;
  miPref: TMenuItem;
  miFloatOn: TMenuItem;
  pnOffReading: TPanel;
  pnWarning: TPanel;
  pnMultiUser: TPanel;
  pnOffRangeBar: TPanel;
  Separator1: TMenuItem;
  miBorders: TMenuItem;
  miFullScreen: TMenuItem;
  miOnTop: TMenuItem;
  miRefresh: TMenuItem;
  miSplit4: TMenuItem;
  miLimitExplain: TMenuItem;
  miSplit3: TMenuItem;
  miRangeLo: TMenuItem;
  miRangeHi: TMenuItem;
  miSplit2: TMenuItem;
  miLO: TMenuItem;
  miHi: TMenuItem;
  miInfo: TMenuItem;
  miSplit1: TMenuItem;
  miForce: TMenuItem;
  pnOffRange: TPanel;
  lArrow: TLabel;
  lDiff: TLabel;
  lVal: TLabel;
  miSettings: TMenuItem;
  pmSettings: TPopupMenu;
  mSplit5: TMenuItem;
  Separator4: TMenuItem;
  tAgo: TTimer;
  tClock: TTimer;
  tSetup: TTimer;
  tInit: TTimer;
  tSwap: TTimer;
  tResize: TTimer;
  tMissed: TTimer;
  tTouch: TTimer;
  tMain: TTimer;
  procedure AdjustGraph;
  procedure bSettingsClick(Sender: TObject);
  procedure fbReadingsDblClick(Sender: TObject);
  procedure FormActivate(Sender: TObject);
  procedure FormClick(Sender: TObject);
  procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure FormDblClick(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure FormKeyPress(Sender: TObject; var Key: char);
  procedure DotPaint(Sender: TObject);
  procedure lDiffClick(Sender: TObject);
  procedure lPredictClick(Sender: TObject);
  procedure miDotNormalDrawItem(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; AState: TOwnerDrawState);
  procedure miDotNormalMeasureItem(Sender: TObject; ACanvas: TCanvas;
    var AWidth, AHeight: integer);
  procedure miDotsInViewClick(Sender: TObject);
  procedure miExitClick(Sender: TObject);
  procedure miCustomDotsClick(Sender: TObject);
  procedure miATouchAutoClick(Sender: TObject);
  procedure miADotAdjustClick(Sender: TObject);
  procedure miADotScaleClick(Sender: TObject);
  procedure miADotsClick(Sender: TObject);
  procedure miASystemInfoClick(Sender: TObject);
  procedure miATouchClick(Sender: TObject);
  procedure miATouchNoClick(Sender: TObject);
  procedure miATouchYesClick(Sender: TObject);
  procedure miDebugBackendClick(Sender: TObject);
  procedure miPredictClick(Sender: TObject);
  procedure pmSettingsClose(Sender: TObject);
  procedure pnWarningClick(Sender: TObject);
  procedure pnWarningPaint(Sender: TObject);
  procedure speakReading;
  procedure FormMouseLeave(Sender: TObject);
  procedure FormMouseMove(Sender: TObject;{%H-}Shift: TShiftState; X, Y: integer);
  procedure FormResize(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure lAgoClick(Sender: TObject);
  procedure lArrowClick(Sender: TObject);
  procedure lDiffDblClick(Sender: TObject);
  procedure lDot7DblClick(Sender: TObject);
  procedure lgMainClick(Sender: TObject);
  procedure lTirClick(Sender: TObject);
  procedure lValClick(Sender: TObject);
  procedure lValDblClick(Sender: TObject);
  procedure lValMouseDown(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; X, Y: integer);
  procedure lValMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
  procedure lValStartDrag(Sender: TObject; var {%H-}DragObject: TDragObject);
  procedure miAnnounceClick(Sender: TObject);
  procedure miAlternateClick(Sender: TObject);
  procedure miClockClick(Sender: TObject);
  procedure miDotNormalClick(Sender: TObject);
  procedure miFloatOnClick(Sender: TObject);
  procedure miHistoryClick(Sender: TObject);
  procedure miRangeColorClick(Sender: TObject);
  procedure miBordersClick(Sender: TObject);
  procedure miForceClick(Sender: TObject);
  procedure miLimitExplainClick(Sender: TObject);
  procedure miOnTopClick(Sender: TObject);
  procedure miSettingsClick(Sender: TObject);
  procedure onTrendClick(Sender: TObject);
  procedure pnOffReadingPaint(Sender: TObject);
  procedure pmSettingsMeasureItem(Sender: TObject; ACanvas: TCanvas;
    var AWidth, AHeight: integer);
  procedure pmSettingsPopup(Sender: TObject);
  procedure pnMultiUserClick(Sender: TObject);
  procedure pnOffRangeClick(Sender: TObject);
  procedure tAgoTimer(Sender: TObject);
  procedure tClockTimer(Sender: TObject);
  procedure tEdgesTimer(Sender: TObject);
  procedure tInitTimer(Sender: TObject);
  procedure tResizeTimer(Sender: TObject);
  procedure tMainTimer(Sender: TObject);
  procedure tMissedTimer(Sender: TObject);
  procedure tSetupTimer(Sender: TObject);
  procedure tSwapTimer(Sender: TObject);
  procedure tTouchTimer(Sender: TObject);
  procedure TfFloatOnHide(Sender: TObject);
private
  FStoredWindowInfo: record
    Left, Top, Width, Height: integer;
    WindowState: TWindowState;
    BorderStyle: TFormBorderStyle;
    FormStyle: TFormStyle;
    Initialized: boolean;
    end;
  titlecolor: boolean;
  FShuttingDown: boolean; // Flag to prevent recursive shutdown calls

    // Performance optimization fields
  FCachedTextWidth: integer;
  FCachedTextHeight: integer;
  FCachedFontSize: integer;
  FCachedFontName: string;
  FLastReadingsHash: cardinal;
  FLastAPICall: TDateTime;
  FCachedReadings: array of BGReading;
  FLastUIColor: TColor;
  FLastUICaption: string;
  FLastTir: string;
  FLastTirColor: TColor;

    // Array to hold references to lDot1 - lDot10
  TrendDots: array[1..10] of TPaintBox;
  multi: boolean; // Multi user
  multinick: string;
  MediaController: TSystemMediaController;
  FWebServer: TObject; // TTrndiWebServer - using TObject to avoid circular dependency
  tWebServerStart: TTimer;

  function dotsInView: integer;
  function setColorMode: boolean;
  function setColorMode(bg: tColor; const nocolor: boolean = false): boolean;
  function setSingleColorMode: boolean;
  procedure SetLang;
  procedure fixWarningPanel;
  procedure showWarningPanel(const message: string;
    clearDisplayValues: boolean = false);
  procedure CalcRangeTime;
  function GetValidatedPosition: TrndiPos;
  function updateReading(boot: boolean = false): boolean;
  procedure PlaceTrendDots(const Readings: array of BGReading);
  procedure actOnTrend(proc: TTrendProc);
  procedure actOnTrend(proc: TTrendProcLoop);
  procedure setDotWidth(l: TPaintBox; c, ix: integer; {%H-}ls: array of TPaintBox);
  procedure HideDot(l: TPaintBox; {%H-}c, {%H-}ix: integer);
  procedure showDot(l: TPaintBox; {%H-}c, {%H-}ix: integer);
  procedure ResizeDot(l: TPaintBox; {%H-}c, ix: integer);
  procedure initDot(l: TPaintBox; c, ix: integer);
  procedure ExpandDot(l: TPaintBox; c, ix: integer);
  procedure placeForm;
  function CalculateDotVisualOffset: integer;

    // Helper methods for update procedure
  function FetchAndValidateReadings: boolean;
  function DoFetchAndValidateReadings(const ForceRefresh: boolean): boolean;
    // Common implementation
  procedure ProcessCurrentReading;
  function IsDataFresh: boolean;
  procedure SetNextUpdateTimer(const LastReadingTime: TDateTime);
  procedure UpdateUIBasedOnGlucose;
  procedure CompleteUIUpdate;
  procedure FinalizeUIUpdate;
  procedure HandleHighGlucose(const {%H-}b: BGReading);
  procedure HandleLowGlucose(const {%H-}b: BGReading);
  procedure HandleNormalGlucose(const b: BGReading);
  procedure UpdateOffRangePanel(const Value: double);
  procedure DisplayLowRange;
  procedure DisplayHighRange;
  procedure FinalizeUpdate;
  procedure UpdateFloatingWindow;
  procedure UpdateUIColors;
  function GetTextColorForBackground(const BgColor: TColor;
    const DarkenFactor: double = 0.5; const LightenFactor: double = 0.3): TColor;
  function GetAdjustedColorForBackground(const BaseColor: TColor;
    const BgColor: TColor; const DarkenFactor: double = 0.6;
    const LightenFactor: double = 0.4; const PreferLighter: boolean = false): TColor;

  // Web server methods
  procedure StartWebServer;
  procedure StopWebServer;
  function GetCurrentReadingForWeb: BGResults;
  function GetPredictionsForWeb: BGResults;
  function WebServerActive: Boolean;
  procedure tWebServerStartTimer(Sender: TObject);

  procedure UpdateTrendElements;
  procedure UpdateApiInformation;
  procedure ResizeUIElements;
  procedure UpdateTrendDots;
  procedure ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
    customTl: TTextLayout = tlCenter);

    // Performance optimization methods
  function CalculateReadingsHash(const Readings: array of BGReading): cardinal;
  function ShouldUpdateUI(const NewColor: TColor; const NewCaption: string;
    const NewTIR: string; const NewTIRColor: TColor): boolean;
  procedure CacheUIState(const UIColor: TColor; const UICaption: string;
    const UITir: string; const UITirColor: TColor);
  function FetchAndValidateReadingsForced: boolean;
    // Force fresh API call bypassing cache

  procedure HandleLatestReadingFreshness(const LatestReading: BGReading;
    CurrentTime: TDateTime);
  procedure ProcessTimeIntervals(const SortedReadings: array of BGReading;
    CurrentTime: TDateTime);
  function UpdateLabelForReading(SlotIndex: integer;
    const Reading: BGReading): boolean;
  function DetermineColorForReading(const Reading: BGReading): TColor;
  procedure DoFullScreen;
  procedure UpdatePredictionLabel;
  {$ifdef DARWIN}
  procedure ToggleFullscreenMac;
  {$endif}

  {$ifdef TrndiExt}
  procedure LoadExtensions;
  {$endif}

    // FormCreate refactored methods
  {$ifdef DARWIN}
  procedure InitializePlatformMenus;
  {$endif}
  {$ifdef X_LINUXBSD}
  procedure InitializeLinuxPlatform;
  {$endif}
  procedure InitializeUIComponents;
  procedure InitializeSplashScreen;
  procedure LoadUserProfile;
  procedure CheckAndAcceptLicense;
  function InitializeAPI: boolean;
  procedure CheckForUpdates(ShowUpToDateMessage: boolean = false);
public
  firstboot: boolean;
  procedure AppExceptionHandler(Sender: TObject; {%H-}E: Exception);
  procedure onGH({%H-}Sender: TObject);
  function lastReading: BGReading;
  function tryLastReading(out bg: BGReading): boolean;
end;


{$ifdef DARWIN}
function CFStringCreateWithUTF8String(const utf8Str: pansichar): CFStringRef; external name '_CFStringCreateWithUTF8String';
{$endif}

var
customTitlebar: boolean = true;
clockInterval: integer = 20000;
clockDisplay: integer = 5000;
fSplash: TfSplash;
native: TrndiNative;
{$ifdef X_LINUXBSD}
isWSL : boolean = false;
{$endif}
applocale: string;
dotscale: integer = 1;
badge_adjust: single = 0;
highAlerted: boolean = false; // A high alert is active
lowAlerted: boolean = false; // A low alert is active
perfectTriggered: boolean = false; // A perfect reading is active
PaintRange: boolean = true;
PaintRangeCGMRange: boolean = true; // Show cgmRangeLo/cgmRangeHi inner threshold lines
PaintRangeLines: boolean = false;
PredictGlucoseReading: boolean = false;
PredictShortMode: boolean = false;
  // Show threshold lines (if false, only filled areas are drawn)
{$ifdef darwin}
MacAppDelegate: TMyAppDelegate;
upMenu: TMenuItem;
DOT_OFFSET_RANGE: integer = 0; // Fine-tune vertical alignment of threshold lines with dots
{$endif}
{$ifdef windows}
DOT_OFFSET_RANGE: integer = 3; // Fine-tune vertical alignment of threshold lines with dots
{$endif}
{$ifdef X_LINUXBSD}
DOT_OFFSET_RANGE: integer = -15; // Fine-tune vertical alignment of threshold lines with dots
{$endif}

var
last_popup: TDateTime = 0;
bg_alert: boolean = false;
  // If the BG is high/low since before, so we don't spam notifications
placed: boolean = false; // If the window has been placed at setup

username: string = '';
lastup: tdatetime;
  // Colors (b)lood(g)lucose (c)olor XX
  // In range
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
  // When the TIR is bad
bad_tir: integer = 5;
  // WHen the TIR is good
good_tir: integer = 75;

tir_bg: TColor = -1;
tir_custom_bg: TColor = -1;

fBG: TfBG;
api: TrndiAPI;
un: BGUnit = BGUnit.mmol;
bgs: BGResults;
{$ifdef TrndiExt}
jsFuncs: TJSfuncs;
{$endif}
badge_width: double = 0.8;
badge_font: integer = 8;

  // Touch screen
StartTouch: TDateTime;
IsTouched: boolean;
HasTouch: boolean;
HasMultiTouch: boolean;
touchHelper: TTouchDetector;

privacyMode: boolean = false;

  // Handle dragging on window
DraggingWin: boolean;
PX, PY: integer;

{$ifdef X_LINUXBSD}
IsRaspberry: boolean;
{$endif}

implementation

{$R *.lfm}

{$I ../../inc/tfuncs.inc}
{$I ../../inc/umain_ext.inc}
{$I ../../inc/umain_helpers.inc}
{$I ../../inc/umain_init.inc}

procedure TfBG.FormDblClick(Sender: TObject);
begin
  if HasTouch then
    DoFullScreen;
end;

function TfBG.tryLastReading(out bg: BGReading): boolean;
begin
  Result := (bgs <> nil) and (length(bgs) > 0);
  if Result then
    bg := lastReading;
end;

function TfBG.lastReading: BGReading;
begin
  try
    Result := bgs[Low(bgs)];
  finally
  end;
end;

procedure TfBG.FormDestroy(Sender: TObject);
begin
  // Ensure shutdown flag is set
  FShuttingDown := true;

  // Stop web server first to prevent callbacks during shutdown
  StopWebServer;

  // These should already be freed in FormClose, but check just to be safe
  if assigned(native) then
  begin
    native.Free;
    native := nil;
  end;
  if assigned(api) then
  begin
    api.Free;
    api := nil;
  end;

  // Note: TTrndiExtEngine.ReleaseInstance is already called in FormClose
  // The extension engine should already be shut down properly
end;

procedure TfBG.speakReading;
begin
  if not privacyMode then
    native.Speak(lVal.Caption)
  else
    case lastReading.level of
    trndi.types.BGHigh:
      native.speak('High');
    trndi.types.BGLOW:
      native.speak('Low');
    trndi.types.BGRange:
      native.speak('Good');
    trndi.types.BGRangeHI:
      native.speak('Going high');
    trndi.types.BGRangeLO:
      native.speak('Going low');
    end;
end;

procedure TfBG.FormKeyPress(Sender: TObject; var Key: char);
begin
  case key of
  #27:
  begin
    lDiffDblClick(self);
    key := #0; // Disable future escapes
  end;
  'f', 'F':
    lDiffDblClick(self);
  's', 'S':
    speakReading;
  'A', 'a':
    miAnnounce.Click;
  'R', 'r':
    if slicke.UX.alert.UXDialog(uxdAuto, sRefrshQ, sForceRefresh,
      [mbYes, mbNo]) = mrYes then
      miForce.Click;
  'I', 'i':
    miSettings.Click;
  'U', 'u':
    CheckForUpdates(true);
  end;
end;

procedure TfBG.DotPaint(Sender: TObject);
var
  tw, th: integer;
  S, fontn: string;
  L: TPaintBox;
  hasfont: boolean;
  needsRecalc: boolean;
begin
  L := Sender as TPaintBox;
  S := L.Caption;
  L.AutoSize := false;

  if S = DOT_GRAPH then
    hasFont := FontGUIInList(fontn)
  else
    hasFont := FontTXTInList(fontn);

  // Check if we need to recalculate font metrics
  needsRecalc := (FCachedFontSize <> L.Font.Size) or (FCachedFontName <> fontn) or
    (FCachedTextWidth = 0) or (FCachedTextHeight = 0) or (S <> DOT_GRAPH);
  // Always recalculate for non-dot text

  with L.Canvas do
  begin
    // Transparent background
    Brush.Style := bsClear;

    if hasFont then
      Font.Name := fontn;

    Font.Size := L.Font.Size;

    // Use cached measurements if available, otherwise calculate
    if needsRecalc then
    begin
      FCachedTextWidth := TextWidth(S);
      FCachedTextHeight := TextHeight(S);
      // Only cache for dot characters, not expanded text
      if S = DOT_GRAPH then
      begin
        FCachedFontSize := L.Font.Size;
        FCachedFontName := fontn;
      end;
    end;

    // Size the paintbox to fit the text (use fresh calculation for non-dots)
    if S = DOT_GRAPH then
    begin
      L.Width := FCachedTextWidth;
      L.Height := FCachedTextHeight;
    end
    else
    begin
      // For expanded text, always use fresh measurements with padding
      L.Width := TextWidth(S) + 4;
      L.Height := TextHeight(S) + 2;
    end;

    // Draw at (0,0); since control fits text, no centering or offsets needed
    TextOut(0, 0, S);
  end;
end;

procedure TfBG.lDiffClick(Sender: TObject);
begin
  ShowMessage(RS_DIFF);
end;

procedure TfBG.lPredictClick(Sender: TObject);
begin
  SHowMessage(RS_PREDICT);
end;

procedure TfBG.miDotNormalDrawItem(Sender: TObject; ACanvas: TCanvas;
ARect: TRect; AState: TOwnerDrawState);
begin

end;

procedure TfBG.miDotNormalMeasureItem(Sender: TObject; ACanvas: TCanvas;
var AWidth, AHeight: integer);
begin

end;

procedure TfBG.miDotsInViewClick(Sender: TObject);
var
  i: integer;
begin
  i := dotsInView;
  ShowMessage(IfThen(i = 0, 'Yes', 'Offset: ' + i.toString));
end;

procedure TfBG.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfBG.miCustomDotsClick(Sender: TObject);
var
  mr: TModalResult;
  dots: integer;
begin
  dots := ExtIntInput(uxdAuto, sDotSize, sCustomiseDotSize, sEnterDotSize,
    dotscale, mr);
  if mr = mrOk then
  begin
    native.SetSetting('ux.dot_scale', dots.tostring);
    dotscale := dots;
  end;
end;

procedure TfBG.miATouchAutoClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;

  native.touchOverride := tbUnknown;
end;

procedure TfBG.miADotAdjustClick(Sender: TObject);
var
  mr: TModalResult;
  da: single;
begin
  da := (ExtNumericInput(uxdAuto, 'Dot Adjustment', 'Add dot adjustment',
    'You can enter plus or minus. Plus = down. 0 = neutral', DOT_ADJUST *
    100, false, mr) / 100);

  if mr = mrOk then
  begin
    DOT_ADJUST := da;

    if ExtMsg(uxdAuto, sChangesSave, sChangesRemember, sChangesApply,
      FormatFloat('0.00', da), uxclWhite, uxclRed, [mbYes, mbNo]) = mrYes then
      native.SetFloatSetting('ux.dot_adjust', da);
  end;
end;

procedure TfBG.miADotScaleClick(Sender: TObject);
var
  mr: TModalResult;
begin
  //  dotscale := round(ExtNumericInput(uxdAuto, 'Dot Adjustment','Add dot adjustment','You can enter plus or minus (+/- 0.x)',dotscale,false,mr));
end;

procedure TfBG.miADotsClick(Sender: TObject);
begin
  ShowMessage(Format('Adjust: %f%% / Scale: %dx', [DOT_ADJUST * 100, dotscale]));
end;

procedure TfBG.miASystemInfoClick(Sender: TObject);
var
  sysver, s: string;
  {$ifdef Linux}
  ver: string;
  {$endif}
begin
  {$if defined(LCLWin32)}
  sysver := SysUtils.Win32MajorVersion.tostring + '.' + SysUtils.Win32MinorVersion.tostring + ' - Build ' +  Win32BuildNumber.ToString;
  {$elseif defined(Linux)}
  s := getlinuxdistro(ver);
  sysver := s + ' ' + ver;
  {$endif}

  {$if defined(LCLQt6)}
  s := 'QT6 - ' + qtVersion + ' - ' + sysver;
  {$elseif defined(LCLGTK2)}
  s := 'GTK2 - '  + sysver;
  {$elseif defined(LCLGTK3)}
  s := 'GTK3';
  {$elseif defined(LCLWIN32)}
  s := 'Windows Native - ' + sysver;
  {$elseif defined(LCLCocoa)}
  s := 'macOS Native';
  {$else}
  s := 'unsupportd widgetset';
  {$endif}

  ShowMessage({$I %FPCTargetOS%} + '(' + {$I %FPCTargetCPU%} + ')' + LineEnding +
    s +
    LineEnding + 'Default separator: ' + DefaultFormatSettings.DecimalSeparator
    );

end;

procedure TfBG.miATouchClick(Sender: TObject);
begin

end;

procedure TfBG.miATouchNoClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;
  native.touchOverride := tbFalse;

end;

procedure TfBG.miATouchYesClick(Sender: TObject);
begin
  miATouchYes.Checked := false;
  miATouchNo.Checked := false;
  miATouchAuto.Checked := false;
  (Sender as TMenuItem).Checked := true;
  native.touchOverride := tbTrue;
end;

procedure TfBG.miDebugBackendClick(Sender: TObject);
begin
  miDebugBackend.Checked := not miDebugBackend.Checked;
end;

procedure TfBG.miPredictClick(Sender: TObject);
var
  bgr: BGResults;
  i: integer;
  msg: string;
begin
  if not api.predictReadings(3, bgr) then
  begin
    ShowMessage('Unable to predict: ' + api.errormsg);
    Exit;
  end;

  msg := 'Predictions:' + LineEnding;
  for i := 0 to High(bgr) do
  begin
    msg := msg + Format('Reading %d: %.1f %s at %s', [
      i + 1,
      bgr[i].convert(un),
      BG_UNIT_NAMES[un],
      FormatDateTime('hh:nn', bgr[i].date)  // Show time
    ]) + LineEnding;
  end;

  ShowMessage(msg);
end;

procedure TfBG.pmSettingsClose(Sender: TObject);
begin
  last_popup := now;
end;

procedure TfBG.pnWarningClick(Sender: TObject);
begin
  {$ifdef TrndiExt}
  if not funcBool('uxClick',
    ['no-reading'], true) then
    Exit;
  {$endif}
  ShowMessage(RS_NO_BOOT_READING);
end;

procedure TfBG.pnWarningPaint(Sender: TObject);
const
  radius = 20;
var
  P: TPanel;
  {$ifndef X_WIN}
  lValRelativeX, lValRelativeY: integer;
  {$endif}
begin
  // Use manual drawing for rounded corners on all platforms
  // This prevents interference with scaling operations
  P := TPanel(Sender);

  with P.Canvas do
  begin
    // First, fill with dark background color in corners
    Brush.Color := fBG.Color;
    FillRect(0, 0, P.Width, P.Height);

    // Now draw the rounded panel
    Brush.Color := P.Color;
    Pen.Color := clBlack;
    Pen.Width := 1;
    RoundRect(0, 0, P.Width, P.Height, Radius, Radius);

    {$ifndef X_WIN}
    // On non-Windows platforms, ApplyAlphaControl doesn't work, so draw lVal as backdrop
    // to simulate transparency effect
    if lVal.Visible and (lVal.Caption <> '') then
    begin
      // Calculate lVal position relative to pnWarning
      lValRelativeX := lVal.Left - P.Left;
      lValRelativeY := lVal.Top - P.Top;

      // Set up text rendering to match lVal
      Font.Assign(lVal.Font);

      // Simulate the original lVal text at 92% transparency (0.92 where 1=fully transparent)
      // Formula: blended = (originalTextColor * opacity + panelColor * transparency)
      // where transparency = 0.92 and opacity = 0.08
      Font.Color := RGB(Round(GetRValue(lVal.Font.Color) * 0.18 +
        GetRValue(P.Color) * 0.82), Round(GetGValue(lVal.Font.Color) *
        0.18 + GetGValue(P.Color) * 0.82), Round(GetBValue(lVal.Font.Color) *
        0.18 + GetBValue(P.Color) * 0.82));

      Brush.Style := bsClear; // Transparent background for text

      // Calculate proper text positioning based on lVal's alignment
      case lVal.Alignment of
      taLeftJustify:
        TextOut(lValRelativeX, lValRelativeY, lVal.Caption);
      taRightJustify:
        TextOut(lValRelativeX + lVal.Width - TextWidth(lVal.Caption),
          lValRelativeY, lVal.Caption);
      taCenter:
        TextOut(lValRelativeX + (lVal.Width - TextWidth(lVal.Caption)) div
          2, lValRelativeY, lVal.Caption);
      end;

      // Restore brush style
      Brush.Style := bsSolid;
    end;
    {$endif}
  end;
end;

procedure TfBG.FormMouseLeave(Sender: TObject);
begin

end;

procedure TfBG.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if DraggingWin then
  begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
    tTouch.Enabled := false; // Dont popup stuff while moving
    // Use the resize timer with a very short interval for smooth panel scaling during drag
    // This prevents too frequent calls while still providing responsive scaling
    if not tResize.Enabled then
    begin
      tResize.Interval := 50; // Very short interval during dragging for responsiveness
      tResize.Enabled := true;
    end;
  end
  else
  // Don't enable touch timer on general mouse movement - only on actual lVal interaction
// The timer will be enabled in lValMouseDown when there's actual touch/click on lVal
  ;
end;

// FormCloseQuery event handler - called BEFORE FormClose
procedure TfBG.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {$ifdef TrndiExt}
  // Show immediate user feedback for shutdown process using big lVal text
  lVal.Caption := 'Shutting down extensions...';
  lVal.Visible := true;
  Application.ProcessMessages; // Ensure caption is updated immediately
  
  // Set global shutdown flag immediately to prevent any new JS operations
  try
    trndi.ext.engine.SetGlobalShutdown;
  except
    // Ignore any errors during shutdown flag setting
  end;
  {$endif}

  // CRITICAL: Set Application.Terminated FIRST to prevent any QuickJS operations
  // This is called even earlier than FormClose for maximum protection
  Application.Terminate;  // This sets Application.Terminated := True

  // Always allow close to proceed
  CanClose := true;
end;

// FormClose event handler
procedure TfBG.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  {$ifdef Darwin}
var
  mr: TModalResult;
  {$endif}
begin
  // Prevent recursive shutdown calls
  if FShuttingDown then
    Exit;
  FShuttingDown := true;

  // Save window position if using custom positioning
  if GetValidatedPosition = tpoCustom then
  begin
    native.SetSetting('position.last.left', IntToStr(Left));
    native.SetSetting('position.last.top', IntToStr(Top));
  end;

  {$ifdef Darwin}
  if not firstboot then
    if self.Showing then
    begin
      mr := UXDialog(uxdAuto, 'Quit or Minimize?', 'Would you like to minimize to the Dock, or close Trndi?',
        [mbClose, mbUXMinimize, mbCancel]);
      case mr of
      mrClose:
        CloseAction := caFree;
      mrCancel: 
      begin
        FShuttingDown := false; // Reset flag if user cancels
        Abort;
      end;
      else
      begin
        CloseAction := caHide;
        FShuttingDown := false; // Reset flag for hide
        Exit;
      end;
      end;

    end;
  {$else}

  if not firstboot then
    if UXDialog(uxdAuto, RS_QUIT_CAPTION, RS_QUIT_MSG, [mbYes, mbNo], uxmtOK) = mrNo then
    begin
      FShuttingDown := false; // Reset flag if user cancels
      Abort;
    end;

  // Explicitly set CloseAction to ensure the form is actually freed
  CloseAction := caFree;
  {$endif}

  {$ifdef TrndiExt}
  // NOTE: Application.Terminate is now called in FormCloseQuery for earlier detection
  
  // CRITICAL: Signal extension shutdown FIRST before stopping timers
  // This prevents new async operations from starting
  try
    // Signal shutdown to all extension components immediately
    SetExtShuttingDown(true);
    
    // Give more time for background threads to see the shutdown signal
    Application.ProcessMessages;
    Sleep(150);  // Increased even more
    Application.ProcessMessages;
    
    // Now stop all timers to prevent new operations
    if Assigned(tAgo) then
      tAgo.Enabled := false;
    if Assigned(tClock) then
      tClock.Enabled := false;
    if Assigned(tSwap) then
      tSwap.Enabled := false;
    if Assigned(tResize) then
      tResize.Enabled := false;
    if Assigned(tMissed) then
      tMissed.Enabled := false;
    if Assigned(tTouch) then
      tTouch.Enabled := false;
    if Assigned(tMain) then
      tMain.Enabled := false;
    
    // Process any remaining messages/synchronize calls with extended wait
    Application.ProcessMessages;
    Sleep(250);  // Increased significantly 
    Application.ProcessMessages;
    Sleep(150);  // Additional wait
    Application.ProcessMessages;
    
    // Update user feedback before engine cleanup
    lVal.Caption := RS_CLEANUP;
    actOnTrend(@HideDot);
    lTir.Hide;
    lArrow.hide;
//    lDiff.Hide;
    lDiff.Caption := RS_CLEANUP_WAIT;
    lAgo.hide;
    lPredict.hide;
    Application.ProcessMessages;
    
    // Now safely shutdown the extension engine
    TTrndiExtEngine.ReleaseInstance;
  except
    // Ignore shutdown errors - the OS will clean up remaining resources
  end;
  {$endif}

  // Stop web server timer if it's still running
  if Assigned(tWebServerStart) then
  begin
    tWebServerStart.Enabled := false;
    FreeAndNil(tWebServerStart);
  end;
  
  // Stop web server
  StopWebServer;

  // Let normal form closure process continue with CloseAction := caFree
end;

procedure TfBG.fbReadingsDblClick(Sender: TObject);
begin

end;

procedure TfBG.FormActivate(Sender: TObject);
begin
  StopFlashing;
end;

procedure TfBG.FormClick(Sender: TObject);
begin
  StopFlashing;
end;

// Post-process the graph
procedure TfBG.AdjustGraph;
var
  l: TPaintBox;
  da: integer;
begin
  for l in TrendDots do
    l.top := l.top + round(ClientHeight * DOT_ADJUST);

  da := dotsInView;

  if da <> 0 then
    for l in TrendDots do
      l.top := l.top - da// da is negative on top so this is valid both ways
  ;

  lRef.Top := lDot1.Top;
  lRef.Caption := lDot1.Hint;
  // Redraw form so range indicators follow dot positions (Update forces immediate paint)
  Update;
  
  // Force all dots to repaint on top of the filled areas (after form has painted)
  for l in TrendDots do
    if l.Visible then
      l.Repaint;

end;

procedure TfBG.FormPaint(Sender: TObject);
var
  loY, hiY, rangeLoY, rangeHiY: integer;
  cnv: TCanvas;
  lineColor: TColor;
  drawLo, drawHi, drawRangeLo, drawRangeHi: boolean;
  tmp: integer;
  clientH: integer;
  dotHeight: integer;
  bmp: TBitmap;
// Helper to map a BG value in internal units to a Y coordinate matching SetPointHeight
function ValueToY(const Value: single): integer;
  var
    Padding, UsableHeight, Position: integer;
    v: single;
  begin
    // clientH is determined in the outer scope to match SetPointHeight's clientHeight
    Padding := Round(clientH * 0.1);
    UsableHeight := clientH - (Padding * 2);
    v := Value;
    if v < BG_API_MIN then
      v := BG_API_MIN;
    if v > BG_API_MAX then
      v := BG_API_MAX;
    Position := Padding + Round((v - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) *
      UsableHeight);
    Result := (clientH - Position) - 1;
  end;

begin
  // Only draw when form has been created and API thresholds available
  if not Assigned(Self) then
    Exit;

  // Exit early if nothing is enabled
  if not (PaintRange or PaintRangeCGMRange) then
    Exit;

  cnv := Self.Canvas;
  // Prefer the TrendDots parent client height if available so mapping matches SetPointHeight
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) and
    Assigned(TrendDots[1].Parent) then
    clientH := TrendDots[1].Parent.ClientHeight
  else
    clientH := Self.ClientHeight;

  // Get dot height for centering lines through the middle of dots
  // Calculate using a temporary bitmap to avoid affecting actual dots during paint
  // IMPORTANT: Must match the exact calculation used in ResizeDot to ensure alignment
  dotHeight := 0;
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Canvas.Font.Assign(TrendDots[1].Font);
      // Use the same font size formula as ResizeDot: (lVal.Font.Size div 8) * dotscale
      bmp.Canvas.Font.Size := Max((lVal.Font.Size div 8) * dotscale, 28);
      // Use the same height calculation as ResizeDot: Max(TextHeight, Font.Size)
      dotHeight := Max(bmp.Canvas.TextHeight(DOT_GRAPH), bmp.Canvas.Font.Size);
    finally
      bmp.Free;
    end;
  end;

  // Decide whether to draw low/high range indicators (0 disables)
  drawLo := (Assigned(api) and (api.cgmLo <> 0) and PaintRange);
  drawHi := (Assigned(api) and (api.cgmHi <> 0) and PaintRange);
  drawRangeLo := (Assigned(api) and (api.cgmRangeLo <> 0) and PaintRangeCGMRange);
  drawRangeHi := (Assigned(api) and (api.cgmRangeHi <> 0) and PaintRangeCGMRange);

  // Calculate Y positions for low and high thresholds (static positions, no dot adjustments)
  if drawLo then
  begin
    loY := ValueToY(api.cgmLo * BG_CONVERTIONS[mmol][mgdl]);
    // Center line through visual center of dot: ValueToY gives top of control,
    // add half the dot height to reach the vertical center (using Round for better precision)
    // Apply DOT_OFFSET_RANGE for fine-tuning alignment
    loY := loY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
  end;

  if drawHi then
  begin
    hiY := ValueToY(api.cgmHi * BG_CONVERTIONS[mmol][mgdl]);
    // Center line through visual center of dot (using Round for better precision)
    // Apply DOT_OFFSET_RANGE for fine-tuning alignment
    hiY := hiY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
  end;

  // Calculate Y positions for range low and high thresholds
  if drawRangeLo then
  begin
    rangeLoY := ValueToY(api.cgmRangeLo * BG_CONVERTIONS[mmol][mgdl]);
    // Center line through visual center of dot (using Round for better precision)
    // Apply DOT_OFFSET_RANGE for fine-tuning alignment
    rangeLoY := rangeLoY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
  end;

  if drawRangeHi then
  begin
    rangeHiY := ValueToY(api.cgmRangeHi * BG_CONVERTIONS[mmol][mgdl]);
    // Center line through visual center of dot (using Round for better precision)
    // Apply DOT_OFFSET_RANGE for fine-tuning alignment
    rangeHiY := rangeHiY + Round(dotHeight / 2.0) + DOT_OFFSET_RANGE;
  end;

  // Fill the area between low and high thresholds with a darkened background color
  if drawLo and drawHi then
  begin
    cnv.Brush.Style := bsSolid;
    if tir_bg = -1 then
      cnv.Brush.Color := DarkenColor(fBG.Color, 0.9)
    else
      cnv.Brush.Color := BlendColors(tir_bg, fBG.Color, 0.3); // 30% tir_bg, 70% background
    cnv.Pen.Style := psClear;
    cnv.FillRect(Classes.Rect(0, hiY, Self.ClientWidth, loY));
  end;

  // Fill the area between inner range thresholds with a lightened background color
  if drawRangeLo and drawRangeHi then
  begin
    cnv.Brush.Style := bsSolid;
    if tir_custom_bg = -1 then
      cnv.Brush.Color := DarkenColor(fBG.Color, 0.85)
    else
    begin
      // Blend tir_custom_bg with the base layer (either blended tir_bg or fBG.Color)
      if tir_bg = -1 then
        cnv.Brush.Color := BlendColors(tir_custom_bg, fBG.Color, 0.3) // Blend with background
      else
        cnv.Brush.Color := BlendColors(tir_custom_bg, BlendColors(tir_bg, fBG.Color, 0.3), 0.3); // Blend with tir_bg layer
    end;
    cnv.Pen.Style := psClear;
    cnv.FillRect(Classes.Rect(0, rangeHiY, Self.ClientWidth, rangeLoY));
  end;

  // Use semi-transparent versions of range colors
  cnv.Brush.Style := bsClear;

  // Only draw lines if PaintRangeLines is enabled
  if PaintRangeLines then
  begin
    if drawLo then
    begin
      // Draw horizontal line for low threshold
      lineColor := LightenColor(bg_rel_color_lo);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 2;
      tmp := loY;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    if drawHi then
    begin
      // Draw horizontal line for high threshold
      lineColor := LightenColor(bg_rel_color_hi);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 2;
      tmp := hiY;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    // Draw range threshold lines (inner thresholds within the main range)
    if drawRangeLo then
    begin
      // Draw horizontal line for range low threshold
      lineColor := LightenColor(fBG.Color, 0.5);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 1;
      tmp := rangeLoY;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;

    if drawRangeHi then
    begin
      // Draw horizontal line for range high threshold
      lineColor := LightenColor(fBG.Color, 0.5);
      cnv.Pen.Color := lineColor;
      cnv.Pen.Style := psSolid;
      cnv.Pen.Width := 1;
      tmp := rangeHiY;
      cnv.MoveTo(0, tmp);
      cnv.LineTo(Self.ClientWidth, tmp);
    end;
  end;
end;

procedure TfBG.bSettingsClick(Sender: TObject);
begin
  ShowMessage(RS_RIGHT_CLICK);
  miSettings.Click;
end;

procedure TfBG.initDot(l: TPaintBox; c, ix: integer);
begin
  {$ifdef Windows}
  l.Font.Name := 'DejaVu Sans Mono';
  {$endif}
end;

// Calculate the vertical offset needed to align limit lines with the visual center of the dot character
// by analyzing where the first non-transparent pixel appears in the rendered dot
function TfBG.CalculateDotVisualOffset: integer;
var
  bmp: TBitmap;
  y, x: integer;
  fontSize: integer;
  halfHeight: integer;
  firstPixelY: integer;
  bgColor, pixelColor: TColor;
  found: boolean;
  fontName: string;
begin
  Result := 0;
  bmp := TBitmap.Create;
  try
    // Use a reasonable font size for measurement (similar to what dots use)
    fontSize := 24;
    bmp.Width := fontSize * 2;
    bmp.Height := fontSize * 2;

    // Set background color and clear bitmap
    bgColor := clWhite;
    bmp.Canvas.Brush.Color := bgColor;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

    // Set font properties matching the dots - use GUI font on all platforms
    if FontGUIInList(fontName) then
      bmp.Canvas.Font.Name := fontName
    else
      bmp.Canvas.Font.Name := 'default';
    bmp.Canvas.Font.Size := fontSize;
    bmp.Canvas.Font.Color := clBlack;

    // Draw the dot character centered
    bmp.Canvas.TextOut(fontSize div 2, fontSize div 2, DOT_GRAPH);

    // Find the first row with a non-background pixel (scanning from top)
    firstPixelY := -1;
    found := false;

    for y := 0 to bmp.Height - 1 do
    begin
      for x := 0 to bmp.Width - 1 do
      begin
        pixelColor := bmp.Canvas.Pixels[x, y];
        // Check if pixel is significantly different from background
        if pixelColor <> bgColor then
        begin
          firstPixelY := y;
          found := true;
          break;
        end;
      end;
      if found then
        break;
    end;

    // Calculate offset based on where we found the first pixel
    if found then
    begin
      // The visual center should be at halfHeight
      halfHeight := bmp.Height div 2;
      // If first pixel is below center, offset is positive (move lines down)
      // If first pixel is above center, offset is negative (move lines up)
      Result := firstPixelY - halfHeight;
      LogMessage(Format(
        'DOT_VISUAL_OFFSET calculated: font=%s, firstPixel=%d, halfHeight=%d, offset=%d',
        [
        fontName, firstPixelY, halfHeight, Result]));
    end;

  finally
    bmp.Free;
  end;
end;

// Expands a trend dot to show actual bg value with highlighting for latest reading
procedure TfBG.ExpandDot(l: TPaintBox; c, ix: integer);
var
  isDot: boolean;
begin

  if privacyMode then // We dont show values while in privacy mode
    exit;

  if l.hint = '' then  // Don't process non-value items
    Exit;

  // Check if label currently shows a dot
  isDot := l.Caption = DOT_GRAPH;

  // Handle differently based on position in trend sequence
  if ix = NUM_DOTS then
    // Latest reading: toggle between fresh indicator and dot
    l.Caption := IfThen(isDot, DOT_FRESH, DOT_GRAPH)
  else
    // Earlier readings: toggle between actual value and dot
    l.Caption := IfThen(isDot, l.Hint, DOT_GRAPH);

  //  l.Caption := IfThen(isDot, l.Caption, l.Caption);
  // Adjust size based on current state
  if not isDot then
  begin // Returning to dot
    ResizeDot(l, c, ix);
    l.Font.Size := (ClientWidth div 24) * dotscale;
  end
  else
  begin
    // Expanding to show actual value - need to resize for text
    l.font.Size := (lVal.Font.Size div c);
    // Clear cached font metrics to force recalculation with new text/font
    FCachedTextWidth := 0;
    FCachedTextHeight := 0;
    FCachedFontSize := 0;
    FCachedFontName := '';
    // Force repaint with new dimensions - DotPaint will recalculate size
    l.Invalidate;
  end;

end;

// Hides a dot
procedure TfBG.HideDot(l: TPaintBox; c, ix: integer);
begin
  l.Visible := false;
end;

// Shows a dot only if it has valid data (non-empty Hint)
procedure TfBG.ShowDot(l: TPaintBox; c, ix: integer);
begin
  // Only show dots that have been populated with data
  if l.Hint <> '' then
    l.Visible := true;
end;

// Scales a dot's font size
procedure TfBG.ResizeDot(l: TPaintBox; c, ix: integer);
var
  th, tw, minSize: integer;
begin
  l.AutoSize := false;
  l.Font.Size := Max((lVal.Font.Size div 8) * dotscale, 28); // Ensure minimum font size
  // Tighten control size to actual text metrics of the dot glyph
  tw := l.Canvas.TextWidth(DOT_GRAPH);
  th := l.Canvas.TextHeight(DOT_GRAPH);
  minSize := Max(th, l.Font.Size);
  l.Width := tw;
  l.Height := minSize;
  LogMessage(Format('TrendDots[%d] resized with Font Size = %d, W=%d, H=%d.',
    [ix, l.Font.Size, l.Width, l.Height]));
end;

// Sets the width (NOT the font) of a dot
procedure TfBG.SetDotWidth(l: TPaintBox; c, ix: integer; ls: array of TPaintBox);
var
  spacing: integer;
begin
  // Calculate spacing based on label width to prevent overlap
  spacing := (fBG.Width - (c * l.Width)) div (c + 1);

  // Position each label with equal spacing from the left
  l.Left := spacing + (spacing + l.Width) * (ix - 1);
  LogMessage(Format('TrendDots[%d] positioned at Left = %d.', [ix, l.Left]));
end;

// FormResize event handler
procedure TfBG.FormResize(Sender: TObject);
begin
  if Sender = lval then
    tResize.OnTimer(self)
  else
  begin
    // Normal resize handling - let the timer handle dragging vs non-dragging logic
    if not tResize.Enabled then
      tResize.Enabled := true
    else
    if not DraggingWin then
    begin
      // Only restart timer when not dragging to prevent rapid resize interruptions
      tResize.Enabled := false;
      tResize.Enabled := true;
    end;

    if not DraggingWin then
    begin
      // Only hide labels during resize if they don't have valid content
      if lVal.Caption = '' then
        lVal.Visible := false;
      lAgo.Visible := false;
      lTir.Visible := false;
    end;

    // Apply alpha control only - rounded corners are handled by pnWarningPaint
    ApplyAlphaControl(pnWarning, 235);
  end;
end;

procedure TfBG.FormShow(Sender: TObject);
begin
  placeForm;
  placed := true;
  lVal.font.Quality := fqCleartype;
  
  // Check for updates on startup (non-blocking)
  CheckForUpdates;
end;

procedure TfBG.lAgoClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  if firstboot then
    exit; // Dont trigger lastReading

  s := miRefresh.Caption;

  if lastReading.getRSSI(i) then
    s += LineEnding + Format(sRSSI, [i]);
  if lastReading.getNoise(i) then
    s += LineEnding + Format(sNoise, [i]);

  s += LineEnding + Format(sDevice, [lastReading.sensor]);
  ;

  ShowMessage(s);

end;

procedure TfBG.lArrowClick(Sender: TObject);
begin

end;

{$ifdef DARWIN}
procedure TfBG.ToggleFullscreenMac;
var
  macWindow: NSWindow;
begin
  // Try to get the main window or fallback to the key window
  macWindow := NSApplication.sharedApplication.mainWindow;
  if macWindow = nil then
    macWindow := NSApplication.sharedApplication.keyWindow;

  // If still nil, try to bridge from the Lazarus form handle
  if macWindow = nil then
    macWindow := NSWindow(Tfbg(self).Handle);

  if Assigned(macWindow) then
    macWindow.toggleFullScreen(nil)
  else
    ShowMessage('Unable to toggle fullscreen. Main window not found.');
end;
{$endif}

procedure TfBG.lDiffDblClick(Sender: TObject);
begin
  DoFullscreen;
end;

procedure TfBG.DoFullScreen;
var
  IsCurrentlyFullscreen: boolean;
  SavedBounds: TRect;
begin
  {$ifdef DARWIN}
  ToggleFullscreenMac;
  {$endif}
  // Store the current form bounds before making any changes
  SavedBounds := BoundsRect;

  // Determine if form is currently in fullscreen mode
  // This needs to check multiple conditions as WindowState alone isn't reliable
  IsCurrentlyFullscreen := (BorderStyle = bsNone) and
    ((WindowState = wsFullScreen) or (BoundsRect.Width >= Screen.Width) and
    (BoundsRect.Height >= Screen.Height));

  // Remember the window position for restoration
  if not FStoredWindowInfo.Initialized and not IsCurrentlyFullscreen then
  begin
    FStoredWindowInfo.Left := Left;
    FStoredWindowInfo.Top := Top;
    FStoredWindowInfo.Width := Width;
    FStoredWindowInfo.Height := Height;
    FStoredWindowInfo.WindowState := WindowState;
    FStoredWindowInfo.BorderStyle := BorderStyle;
    FStoredWindowInfo.FormStyle := FormStyle;
    FStoredWindowInfo.Initialized := true;
  end;

  if IsCurrentlyFullscreen then
  begin
    // First, change the window state to normal
    WindowState := wsNormal;
    Application.ProcessMessages;
    Screen.Cursor := crDefault;

    // Then restore border styles
    BorderStyle := bsSizeToolWin;
    FormStyle := fsNormal;
    Application.ProcessMessages;

    // Restore previous window size and position
    if FStoredWindowInfo.Initialized then
    begin
      SetBounds(FStoredWindowInfo.Left, FStoredWindowInfo.Top,
        FStoredWindowInfo.Width, FStoredWindowInfo.Height);
      WindowState := FStoredWindowInfo.WindowState;
      FStoredWindowInfo.Initialized := false;
    end
    else
    begin
      // Fallback if stored position is not available
      Width := Max(Screen.Width div 5, 200);
      Height := Max(Screen.Height div 5, 300);
      Top := (Screen.Height div 2) - (Height div 2);
      Left := (Screen.Width div 2) - (Width div 2);
    end;
  end
  else
  begin
    // Enter fullscreen mode

    // First, make sure we're in normal state to avoid issues
    if WindowState = wsMaximized then
      WindowState := wsNormal;
    Application.ProcessMessages;

    // Remove borders first
    BorderStyle := bsNone;
    FormStyle := fsStayOnTop;
    Application.ProcessMessages;

    // Finally set to fullscreen
    WindowState := wsFullScreen;

    // Force repaint to ensure display updates properly
    Invalidate;

    Screen.Cursor := crNone;
    // FIx Raspbian issues
    if IsProblematicWM then begin
      if not IsSemiProblematicWM then begin
       miBorders.Checked := false;
       BorderStyle := bsToolWindow;
      end;
    end;
  end;

  // Adjust for dark mode if applicable
  setColorMode;
end;

procedure TfBG.lDot7DblClick(Sender: TObject);
begin
end;

// Empty event handler
procedure TfBG.lgMainClick(Sender: TObject);
begin
  // Event handler can be left empty if not used
end;

procedure TfBG.lTirClick(Sender: TObject);
var
  minTotal, hours, mins: integer;
  msg: string;
  hi, lo, rhi, rlo: double;
begin
  minTotal := MinutesBetween(now, bgs[High(bgs)].date);
  {$ifdef TrndiExt}
  if not funcBool('uxClick',[
    'tir',
    mintotal,
    lTir.Caption
    ], true) then
    Exit;
  {$endif}

  hi := api.cgmHi * BG_CONVERTIONS[un][mgdl];
  lo := api.cgmLo * BG_CONVERTIONS[un][mgdl];
  rhi := api.cgmRangeHi * BG_CONVERTIONS[un][mgdl];
  rlo := api.cgmRangeLo * BG_CONVERTIONS[un][mgdl];

  if minTotal < 60 then
    msg := Format(RS_TIR_M, [minTotal, lo, hi, rlo, rhi])
  else
  begin
    hours := minTotal div 60;
    mins := minTotal mod 60;
    if hours < 2 then
      msg := Format(RS_TIR_H1, [hours, mins, lo, hi, rlo, rhi])
    else
      msg := Format(RS_TIR_H, [hours, mins, lo, hi, rlo, rhi]);
  end;

  ShowMessage(msg + LineEnding);
end;

// Handle lVal click
procedure TfBG.lValClick(Sender: TObject);
begin
  if (lVal.Caption = RS_SETUP) then
    miSettings.Click;
  if firstboot then
    exit;
  StopFlashing;
end;

procedure TfBG.lValDblClick(Sender: TObject);
begin
  if HasTouch then
    DoFullScreen;
end;

// Handle mouse down on lVal
procedure TfBG.lValMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
  if ((Button = mbLeft) and (self.BorderStyle = bsNone)) or (Button = mbMiddle) then
  begin   // Handle window moving
    DraggingWin := true;
    PX := X;
    PY := Y;
    if not hasTouch then
      Exit;
  end;


  if not hasTouch then
    Exit;

  // Handle touch screens
  StartTouch := Now;
  IsTouched := true;
  tTouch.Enabled := true;

end;

// Handle mouse up on lVal
procedure TfBG.lValMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
  IsTouched := false;
  tTouch.Enabled := false;
  StartTouch := 0; // Reset the touch start time

  if DraggingWin then
  begin
    DraggingWin := false;
    // Restore normal timer interval
    tResize.Interval := 500; // Back to normal interval from form design
    // Trigger a full resize operation now that dragging is complete
    // This ensures all UI elements are properly scaled for the final window size
    FormResize(Self);
  end;
end;

// Empty drag event handler
procedure TfBG.lValStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  // Event handler can be left empty if not used
end;

procedure TfBG.miAnnounceClick(Sender: TObject);
begin
  miAnnounce.Checked := not miAnnounce.Checked;
  native.SetBoolSetting('main.announce', miAnnounce.Checked);
  native.speak(IfThen(miAnnounce.Checked, sAnnounceOn, sAnnounceOff));
end;

procedure TfBG.miAlternateClick(Sender: TObject);
begin
  miAlternate.Checked := not miAlternate.Checked;
  tSwap.Enabled := miAlternate.Checked;
end;

procedure TfBG.miClockClick(Sender: TObject);
begin
  miClock.Checked := not miClock.Checked;
  tClock.Enabled := miClock.Checked;
  native.SetBoolSetting('main.clock', tClock.Enabled);
end;

procedure TfBG.miDotNormalClick(Sender: TObject);
begin
  dotscale := StrToInt((Sender as TMenuItem).Hint);
  native.SetSetting('ux.dot_scale', dotscale.toString);
  FormResize(fBG);
end;

procedure TfBG.TfFloatOnHide(Sender: TObject);
begin
  miFloatOn.Checked := fFloat.Showing;
end;

procedure TfBG.miFloatOnClick(Sender: TObject);
begin
  {$ifdef LCLGTK3}
  Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
  Exit;
  {$endif}
  if fFloat.Showing then
    fFloat.Hide
  else
  begin
    fFloat.Show;
    if not assigned(fFloat.onhide) then
      ffloat.OnHide := @TfFloatOnHide;
    fFloat.Color := fBg.Color;
    fFloat.lVal.Caption := lval.Caption;
    fFloat.lArrow.Caption := lArrow.Caption;
    if pnMultiUser.Visible then
    begin
      fFloat.pnMultiUser.Visible := true;
      fFloat.pnMultiUser.Color := pnMultiUser.Color;
    end;
  end;
  miFloatOn.Checked := fFloat.Showing;
end;

procedure TfBG.miHistoryClick(Sender: TObject);
var
  i: integer;
  keys, vals: TStringArray;
  b: BGReading;
  xval: integer;
  rssi, noise: string;
begin
  SetLength({%H-}keys, high(bgs) + 1);
  SetLength({%H-}vals, high(bgs) + 1);

  for i := Low(bgs) to High(bgs) do
  begin
    if bgs[i].empty then
      continue;

    keys[i] := TimeToStr(bgs[i].date);
    vals[i] := bgs[i].format(un, BG_MSG_SHORT, BGPrimary);
  end;


  i := ExtTable(uxdAuto, RS_RHISTORY, RS_RH_TITLE, RS_RH_INFO, keys,
    vals, uxmtCustom, RS_RH_TIME, RS_RH_READING);
  if i > 0 then
  begin
    b := bgs[i - 1];
    if b.getRSSI(xval) then
      rssi := xval.ToString
    else
      rssi := RS_RH_UNKNOWN;

    if b.getNoise(xval) then
      noise := xval.ToString
    else
      noise := RS_RH_UNKNOWN;

    ShowMessage(TimeToStr(b.date), Format(RS_HISTORY_ITEM,
      [b.format(un, BG_MSG_SHORT, BGPrimary), b.format(un,
      BG_MSG_SIG_SHORT, BGDelta), b.trend.Img, rssi, noise,
      b.Source, b.sensor]));
  end;
end;

procedure TfBG.miRangeColorClick(Sender: TObject);
begin
  miRangeColor.Checked := not miRangeColor.Checked;
  miForce.Click;
  native.SetBoolSetting('ux.range_color', miRangeColor.Checked);
end;

procedure TfBG.miBordersClick(Sender: TObject);
begin
  {$ifdef LCLGTK3}
  Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
  Exit;
  {$endif}
  miBorders.Checked := not miBorders.Checked;
  if miBorders.Checked then
    self.BorderStyle := bsNone
  else
    {$ifdef DARWIN}
    BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}

  setColorMode;
end;

// Force update on menu click
procedure TfBG.miForceClick(Sender: TObject);
const
  API_CACHE_SECONDS = 10; // Same as in FetchAndValidateReadings
var
  secondsSinceLastCall: integer;
  waitTime: integer;
  msg: string;
  Result: integer;
begin
  if firstboot then
    exit;
  
  // Check if we have made an API call before
  if FLastAPICall = 0 then
  begin
    // No previous API call, just do a normal update
    updateReading;
    Exit;
  end;
  
  // Check if we're still within the cache window
  secondsSinceLastCall := SecondsBetween(Now, FLastAPICall);
  if secondsSinceLastCall < API_CACHE_SECONDS then
  begin
    waitTime := API_CACHE_SECONDS - secondsSinceLastCall;
    msg := Format(sForceRefreshCached, [secondsSinceLastCall, waitTime]);

    // Use ExtMsg with custom buttons for better UX
    Result := slicke.UX.alert.ExtMsg(uxdAuto, sRefrshQ, msg,
      sForceRefreshDetail, '', uxclWhite, uxclRed, [mbRetry, mbCancel],
      uxmtInformation);

    if Result = mrRetry then // mbRetry returns mrRetry
    begin
      // User chose to force - bypass cache completely
      FetchAndValidateReadingsForced;
      // Update the rest of the UI manually since we bypassed normal flow
      CompleteUIUpdate;
    end;
    // If Cancel was chosen, do nothing - let user wait
  end
  else
    updateReading// Cache has expired, normal update is fine
  ;
end;

// Explain limit menu click
procedure TfBG.miLimitExplainClick(Sender: TObject);
begin
  //  MessageDlg('Trndi', RS_LIMIT_EXPLAIN_TEXT, mtInformation, [mbOK], '');
  ShowMessage(RS_LIMIT_EXPLAIN_TEXT);
end;

procedure TfBG.miOnTopClick(Sender: TObject);
{$ifdef LCLQt6}
var
  QtWidget: TQtWidget;
  flags: nativeuint;
  sessionType: string;
const
  // Qt::WindowStaysOnTopHint (use numeric mask to avoid missing symbol on some setups)
  Qt_WindowStaysOnTopHint = $00008000;
  {$endif}
begin
  miOnTop.Checked := not miOnTop.Checked;
  if miOnTop.Checked then
  begin
    {$ifdef LCLQt6}
    // Use fsStayOnTop for Qt widgetset; it's better respected by some WMs.
    self.FormStyle := fsStayOnTop;
    {$else}
    self.FormStyle := fsSystemStayOnTop;
    {$endif}
  end
  else
    self.FormStyle := fsNormal;

  {$ifdef LCLQt6}
  // KDE/Qt may ignore LCL's FormStyle mapping; force Qt's WindowStaysOnTopHint.
  // Detect session type: Wayland sessions often prevent external programs from
  // forcing window stacking. Inform the user instead of attempting external hacks.
  sessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');
  if LowerCase(sessionType) = 'wayland' then
  begin
    LogMessage('OnTop requested but session is Wayland; compositor may ignore programmatic hints.');
    // Friendly guidance for users running KDE/Wayland
    if miOnTop.Checked then
      ShowMessage(RS_WAYLAND);
    // Continue with LCL/Qt hints — they may or may not be honored.
  end;
  try
    if HandleAllocated then
    begin
      QtWidget := TQtWidget(Handle);
      if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
      begin
        flags := QtWidget.windowFlags;
        if miOnTop.Checked then
        begin
          QtWidget.setWindowFlags(flags or Qt_WindowStaysOnTopHint);
          // Use LCL methods to bring the form forward and ensure it's visible
          Self.Visible := true;
          Self.Show;
          Self.BringToFront;
          Self.SetFocus;
          // Also request the application to bring windows forward
          try
            Application.BringToFront;
          except end;
          // If the window is off-screen (some WMs may move it), center it as a safeguard
          if (Left < -1000) or (Top < -1000) or (Left > Screen.Width) or (Top > Screen.Height) then
          begin
            Width := Min(Width, Screen.Width - 40);
            Height := Min(Height, Screen.Height - 40);
            Left := (Screen.Width - Width) div 2;
            Top := (Screen.Height - Height) div 2;
            Application.ProcessMessages;
          end;
        end
        else
          QtWidget.setWindowFlags(flags and (not Qt_WindowStaysOnTopHint));
      end;
    end;
  except
    // Ignore any failures - fallback is LCL behavior
  end;
  {$endif}

  setColorMode;
end;

// Handle settings menu click
procedure TfBG.miSettingsClick(Sender: TObject);
var
  fConf: TfConf;
  lastUsers: integer;

procedure LoadUserSettings(f: TfConf);
  var
    s: string;
    i: integer;
    posValue: integer;
    po: TrndiPos;
  begin
    with f, native do
    begin
      // Remote and user settings
      s := GetSetting('remote.type');
      cbSys.ItemIndex := 0; // Default driver
      for i := 0 to cbSys.Items.Count - 1 do
        if cbSys.Items[i] = s then
          cbSys.ItemIndex := i;
      f.cbSysChange(f); // Update labels
      eAddr.Text := GetSetting('remote.target');
      ePass.Text := GetSetting('remote.creds');
      rbUnit.ItemIndex := IfThen(GetSetting('unit', 'mmol') = 'mmol', 0, 1);
      spTHRESHOLD.Value := native.GetIntSetting('system.fresh_threshold',
        DATA_FRESHNESS_THRESHOLD_MINUTES);

      // Override range settings
      if api = nil then
      begin
        fsLo.Value := GetIntSetting('override.lo', 60);
        fsHi.Value := GetIntSetting('override.hi', 160);
        fsLoRange.Value := GetIntSetting('override.rangelo', 80);
        fsHiRange.Value := GetIntSetting('override.rangehi', 180);
      end
      else
      begin
        fsLo.Value := GetIntSetting('override.lo', api.cgmLo);
        fsHi.Value := GetIntSetting('override.hi', api.cgmHi);
        fsLoRange.Value := GetIntSetting('override.rangelo', api.cgmRangeLo);
        fsHiRange.Value := GetIntSetting('override.rangehi', api.cgmRangeHi);
      end;

      cbTIR.Checked := native.GetBoolSetting('range.custom', true);

      cbOffBar.Checked := native.GetBoolSetting('ux.off_bar', false);
      cbPaintHiLo.Checked := native.GetBoolSetting('ux.paint_range', true);
      cbPaintLines.Checked := native.GetBoolSetting('ux.paint_range_lines', false);
      cbPaintHiLoRange.Checked :=
        native.GetBoolSetting('ux.paint_range_cgmrange', false);
      edCommaSep.Text := GetCharSetting('locale.separator', '.');
      edTray.Value := GetIntSetting('ux.badge_size', 0);

      if CheckSetting('unit', 'mmol', 'mmol') then
        rbUnitClick(Self);

      cbCust.Checked := GetBoolSetting('override.enabled');
      cbCustRange.Checked := GetBoolSetting('override.range');
      cbPredictions.Checked := GetBoolSetting('predictions.enable');
      cbPredictShort.Checked := GetBoolSetting('predictions.short');

      edMusicHigh.Text := GetSetting('media.url_high', '');
      edMusicLow.Text := GetSetting('media.url_low', '');
      edMusicPerfect.Text := GetSetting('media.url_perfect', '');
      cbMusicPause.Checked := GetBoolSetting('media.pause');
      fsHi.Enabled := cbCust.Checked;
      fsLo.Enabled := cbCust.Checked;
      fsHiRange.Enabled := cbCustRange.Checked;
      fsLoRange.Enabled := cbCustRange.Checked;

      // User customizations
      s := GetRootSetting('users.names', '');
      lbUsers.Clear;
      lbUsers.Items.CommaText := s;
      if lbUsers.Items.Count < 1 then
        lbUsers.Enabled := false;

      lbUsers.Items.Add('- ' + RS_DEFAULT_ACCOUNT + ' -');
      cbUserColor.Checked := native.GetRootSetting('users.colorbox', 'true') = 'true';
      // Load position settings
      posValue := native.GetIntSetting('position.main', Ord(tpoCenter));

      cbPos.Items.Clear;
      for po in TrndiPos do
      begin
        s := TrndiPosNames[po];
        cbPos.Items.Add(s);

        // Match enum order with saved position
        if Ord(po) = posValue then
          cbPos.ItemIndex := Ord(po);
      end;

      // Fallback to first item if no valid match
      if cbPos.ItemIndex = -1 then
        cbPos.ItemIndex := 0;

      cbSize.Checked := GetBoolSetting('size.main');
      cbFlashHi.Checked := native.getBoolSetting('alerts.flash.high', false);
      cbFlashLow.Checked := native.getBoolSetting('alerts.flash.low', false);
      cbFlashPerfect.Checked := native.getBoolSetting('alerts.flash.perfect', false);
    end;

  end;

procedure LoadLanguageSettings(f: TfConf);
  var
    i: integer;
    s: string;
  begin
    with f, native do
    begin
      // Load language files
      ListLanguageFiles(cbLang.Items, GetLangPath);
      cbLang.Items.Add('Trndi.en');
      cbLang.Items.Add('Trndi.auto');
      for i := 0 to cbLang.Items.Count - 1 do
      begin
        s := cbLang.Items[i];
        cbLang.Items[i] := ExtractDelimited(2, s, ['.']);
        s := cbLang.Items[i];
        cbLang.Items[i] := Format('%s (%s)', [GetLanguageName(s), s]);
        if CheckSetting('locale', '', s) then
          cbLang.ItemIndex := i;
      end;
      if cbLang.ItemIndex = -1 then
        cbLang.ItemIndex := cbLang.Items.Count - 1;
    end;
  end;

procedure SetupUIElements(f: TfConf);
  const
    dotdef = $2b24;
  begin
    with f do
    begin
      f.lDot.Caption := native.GetWideCharSetting('font.dot', System.widechar(dotdef));
      eDot.Text := native.GetSetting('font.dot', '2B24');
      lDotNow.Caption := native.GetWideCharSetting('font.dot_fresh',
        System.widechar(dotdef));
      eDotNow.Text := native.GetSetting('font.dot_fresh', '2600');
      f.lDot1.Caption := f.lDot.Caption;
      f.lDot2.Caption := f.lDot.Caption;
      f.lDot3.Caption := f.lDot.Caption;
      f.lDotCurr.Caption := f.lDotNow.Caption;
      f.Invalidate;

      // UI updates
      lVal.Font.Name := Self.lVal.Font.Name;
      lArrow.Font.Name := Self.lArrow.Font.Name;
      lAgo.Font.Name := Self.lAgo.Font.Name;
      lVal.Font.Color := Self.lVal.Font.Color;
      lArrow.Font.Color := Self.lArrow.Font.Color;
      lAgo.Font.Color := Self.lAgo.Font.Color;
      lVal.Caption := Self.lVal.Caption;
      lArrow.Caption := Self.lArrow.Caption;
      lAgo.Caption := Self.lAgo.Caption;
      pnDisplay.Color := Self.Color;
      pnDisplay.Font := fBG.Font;
      pnFonts.Font := eAddr.Font; // Override fBG.font for the panel

      lDot1.Font.Color := self.lDot1.Font.Color;
      lDot2.Font.Color := self.lDot1.Font.Color;
      lDot3.Font.Color := self.lDot1.Font.Color;

      cl_ok_bg.ButtonColor := native.GetColorSetting('ux.bg_color_ok', bg_color_ok);
      cl_hi_bg.ButtonColor := native.GetColorSetting('ux.bg_color_hi', bg_color_hi);
      cl_lo_bg.ButtonColor := native.GetColorSetting('ux.bg_color_lo', bg_color_lo);

      cl_ok_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_ok_txt', bg_color_ok_txt);
      cl_hi_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_hi_txt', bg_color_hi_txt);
      cl_lo_txt.ButtonColor :=
        native.GetColorSetting('ux.bg_color_lo_txt', bg_color_lo_txt);

      cl_hi_bg_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_hi', bg_rel_color_hi);
      cl_lo_bg_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_lo', bg_rel_color_lo);

      cl_hi_txt_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_hi_txt', bg_rel_color_hi_txt);
      cl_lo_txt_cust.ButtonColor :=
        native.GetColorSetting('ux.bg_rel_color_lo_txt', bg_rel_color_lo_txt);

      cbTitleColor.Checked := native.GetBoolSetting('ux.title_color', true);

      cbTirColor.Checked := native.GetBoolSetting('ux.tir_color_on');
      cbTirColorCustom.Checked := native.GetBoolSetting('ux.tir_color_custom_on');

      cbTirColorBg.Checked := not cbTirColor.Checked;
      cbTirColorBgCustom.Checked := not cbTirColorCustom.Checked;

      cbTirBar.ButtonColor := native.GetColorSetting('ux.tir_color');
      cbTirBarCustom.ButtonColor := native.GetColorSetting('ux.tir_color_custom');
    end;
  end;

procedure SetupExtensions(f: TfConf);
  begin
    with f, native do
    begin
      {$ifdef TrndiExt}
      eExt.Text := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
      {$else}
      eExt.Text := '- ' + RS_noPlugins + ' -';
      eExt.Enabled := false;
      {$endif}
      cbPrivacy.Checked := GetBoolSetting('ext.privacy');
    end;
  end;

procedure SetupTouchAndNotifications(f: TfConf);
  begin
    with f, native do
    begin
      cbTouch.Checked := HasTouch;
      cbMultiTouch.Checked := HasMultiTouch;
    end;
  end;

procedure SaveUserSettings(f: TfConf);
  var
    s: string;
    i: integer;
  begin
    with f, native do
    begin
      if TryStrToInt('$' + eDot.Text, i) then
        SetWideCharSetting('font.dot', System.widechar(i));
      if TryStrToInt('$' + eDotNow.Text, i) then
        SetWideCharSetting('font.dot_fresh', System.widechar(i));

      SetSetting('font.val', lVal.Font.Name);
      SetSetting('font.arrow', lArrow.Font.Name);
      SetSetting('font.ago', lAgo.Font.Name);
      s := ExtractLangCode(cbLang.Items[cbLang.ItemIndex]);
      SetSetting('locale', s);
      native.SetSetting('position.main', IntToStr(cbPos.ItemIndex));
      native.setBoolSetting('size.main', cbSize.Checked);
      native.setBoolSetting('alerts.flash.high', cbFlashHi.Checked);
      native.setBoolSetting('alerts.flash.low', cbFlashLow.Checked);
      native.setBoolSetting('alerts.flash.perfect', cbFlashPerfect.Checked);

      for i := lbUsers.Items.Count - 1 downto 0 do
        if lbUsers.items[i][1] = '-' then
          lbUsers.items.Delete(i);

      // Handle user list changes
      if lbUsers.Count > 0 then
        SetSetting('users.names', lbUsers.Items.CommaText)
      else
        SetSetting('users.names', '');

      native.SetRootSetting('users.colorbox', IfThen(cbUserColor.Checked,
        'true', 'false'));

      // Save remote and override settings
      SetSetting('remote.type', cbSys.Text);
      SetSetting('remote.target', eAddr.Text);
      SetSetting('remote.creds', ePass.Text);
      SetSetting('unit', IfThen(rbUnit.ItemIndex = 0, 'mmol', 'mgdl'));
      SetBoolSetting('ext.privacy', cbPrivacy.Checked);

      SetSetting('system.fresh_threshold', IntToStr(spTHRESHOLD.Value));

      // Save unit-specific settings
      if rbUnit.ItemIndex = 0 then
      begin // mmol
        SetSetting('override.lo', Round(fsLo.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.hi', Round(fsHi.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.rangelo', Round(fsLoRange.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.rangehi', Round(fsHiRange.Value * TrndiAPI.toMGDL).ToString);
      end
      else
      begin
        SetSetting('override.lo', Round(fsLo.Value).ToString);
        SetSetting('override.hi', Round(fsHi.Value).ToString);
        SetSetting('override.rangelo', Round(fsLoRange.Value).ToString);
        SetSetting('override.rangehi', Round(fsHiRange.Value).ToString);
      end;

      native.SetBoolSetting('range.custom', cbTIR.Checked);
      native.SetBoolSetting('ux.off_bar', cbOffBar.Checked);
      native.SetBoolSetting('ux.paint_range', cbPaintHiLo.Checked);
      native.SetBoolSetting('ux.paint_range_lines', cbPaintLines.Checked);
      native.SetBoolSetting('ux.paint_range_cgmrange', cbPaintHiLoRange.Checked);
      native.SetSetting('locale.separator', edCommaSep.Text);
      native.SetSetting('ux.badge_size', edTray.Value.ToString);

      SetBoolSetting('override.enabled', cbCust.Checked);
      SetBoolSetting('override.range', cbCustRange.Checked);
      SetBoolSetting('predictions.enable', cbPredictions.Checked);
      SetBoolSetting('predictions.short', cbPredictShort.Checked);
      SetSetting('media.url_high', edMusicHigh.Text);
      SetSetting('media.url_low', edMusicLow.Text);
      SetSetting('media.url_perfect', edMusicPerfect.Text);
      SetBoolSetting('media.pause', cbMusicPause.Checked);

      SetColorSetting('ux.bg_color_ok', cl_ok_bg.ButtonColor);
      SetColorSetting('ux.bg_color_hi', cl_hi_bg.ButtonColor);
      SetColorSetting('ux.bg_color_lo', cl_lo_bg.ButtonColor);

      SetColorSetting('ux.bg_color_ok_txt', cl_ok_txt.ButtonColor);
      SetColorSetting('ux.bg_color_hi_txt', cl_hi_txt.ButtonColor);
      SetColorSetting('ux.bg_color_lo_txt', cl_lo_txt.ButtonColor);

      SetColorSetting('ux.bg_rel_color_hi', cl_hi_bg_cust.ButtonColor);
      SetColorSetting('ux.bg_rel_color_lo', cl_lo_bg_cust.ButtonColor);

      SetColorSetting('ux.bg_rel_color_hi_txt', cl_hi_txt_cust.ButtonColor);
      SetColorSetting('ux.bg_rel_color_lo_txt', cl_lo_txt_cust.ButtonColor);

      native.SetBoolSetting('ux.title_color', cbTitleColor.Checked);

      native.SetBoolSetting('ux.tir_color_on', cbTirColor.Checked);
      native.SetBoolSetting('ux.tir_color_custom_on', cbTirColorCustom.Checked);

      native.SetColorSetting('ux.tir_color', cbTirBar.ButtonColor);
      native.SetColorSetting('ux.tir_color_custom', cbTirBarCustom.ButtonColor);
    end;
  end;

var
  s: string;
begin
  fConf := TfConf.Create(Self);
  try
    with native do
    begin
      fConf.cbSys.Items.Clear;
      // Populate available backends
      fConf.cbSys.Items.Add('NightScout');
      fConf.cbSys.Items.Add('NightScout v3');
      fConf.cbSys.Items.Add('Dexcom (USA)');
      fConf.cbSys.Items.Add('Dexcom (Outside USA)');
      fConf.cbSys.Items.Add('xDrip');
      {$ifdef DEBUG}
      fConf.cbSys.Items.Add('* Debug Backend *');
      fConf.cbSys.Items.Add('* Debug Missing Backend *');
      fConf.cbSys.Items.Add('* Debug Perfect Backend *');
      fConf.cbSys.Items.Add('* Debug Custom Backend *');
      fConf.cbSys.Items.Add('* Debug Edge Backend *');
      {$endif}
    end;

    // Initialize form with user settings
    LoadUserSettings(fConf);
    LoadLanguageSettings(fConf);
    SetupUIElements(fConf);
    SetupExtensions(fConf);
    SetupTouchAndNotifications(fConf);

    // Store current user count for comparison later
    lastUsers := fConf.lbUsers.Count;

    {$if defined(X_PC)}
    fConf.lOS.Caption := GetLinuxDistro(s) + ' ' + s;

    {$if defined(LCLQt6)}
    fConf.lWidgetset.Caption := 'QT6 ' + qtVersion;
    {$elseif defined(LCLGTK2)}
    fConf.lWidgetset.Caption := 'GTK2';
    {$elseif defined(LCLGTK3)}
    fConf.lWidgetset.Caption := 'GTK3';
    {$endif}
    {$elseif defined(X_MAC)}
    fConf.lOS.Caption := 'macOS';
    fConf.lWidgetset.Caption := 'Native Apple Coca';
    {$else}
    fConf.lOS.Caption := 'Windows'  + SysUtils.Win32MajorVersion.tostring + '.' + SysUtils.Win32MinorVersion.tostring + ' - Build ' +  Win32BuildNumber.ToString;
    fConf.lWidgetset.Caption := 'Native Windows';
    {$endif}
    fConf.lArch.Caption := {$I %FPCTargetOS%} + '/' + {$I %FPCTARGETCPU%};
    // Show dialog (use safe helper that handles problematic WMs)

    {$ifdef TrndiExt}
     s := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
    // Find extensions folder
     ForceDirectoriesUTF8(s);
    // Create the directory if it doesn't exist
    fConf.lbExtensions.Items := FindAllFiles(s, '*.js', false);
    fConf.lExtCount.Caption := Format(RS_ExtCount, [fConf.lbExtensions.Count]);
    {$else}
      fConf.tsExt.Enabled := false;
      fConf.tsExt.Visible := false;
    {$endif}
    ShowFormModalSafe(fConf);

    if not firstboot then
    begin
      if IsProblematicWM then
        fBG.Hide;
      if ExtMsg(uxdAuto, RS_SETTINGS_SAVE, RS_SETTINGS_SAVE,
        RS_SETTINGS_SAVE_DESC, '', uxclWhite, uxclRed, [mbYes, mbNo]) <> mrYes then
      begin
        fBG.Show;
        Exit; // FConf.Free will run later
      end;
      fBG.Show;
    end;
    // Reload settings, needed on X_PC
    native.ReloadSettings;

    // Reload prediction settings
    PredictGlucoseReading := native.GetBoolSetting('predictions.enable', false);
    PredictShortMode := native.GetBoolSetting('predictions.short', false);
    lPredict.Visible := PredictGlucoseReading;

    // Save settings when dialog closes
    SaveUserSettings(fConf);
    ShowMessage(RS_RESTART_APPLY);

    if firstboot then
      exit;
    //    SetLang;
    miForce.Click;
  finally
    fConf.Free;
  end;
end;

// Swap dots with their readings
procedure TfBG.onTrendClick(Sender: TObject);
var
  isdot: boolean;
  l: TPaintbox;
  {$ifdef TrndiExt}
  fs: TFormatSettings;
  {$endif}
begin
  l := Sender as tPaintbox;

  actOnTrend(@ExpandDot);
  isDot := UnicodeSameText(l.Caption, DOT_GRAPH);

  {$ifdef TrndiExt}
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  callFunc('dotClicked',[
    IfThen(isDot, 'false', 'true'), // is the dot "open" as in viewing the value
    StrToFloat(l.Hint) * BG_CONVERTIONS[mgdl][un],
    StrToFloat(l.Hint) * BG_CONVERTIONS[mmol][un],
    l.tag
    ]);
  {$endif}

  if isDot then
    tResize.OnTimer(self);
end;

procedure TfBG.pnOffReadingPaint(Sender: TObject);
var
  Panel: TPanel;
  X, Y: integer;
  TextStr: string;
  TextW, TextH: integer;
begin
  Panel := Sender as TPanel;
  TextStr := Panel.Caption;
  if TextStr = '' then
    Exit;
  with Panel.Canvas do
  begin
    Brush.Color := Panel.Color;
    Brush.Style := bsSolid;
    FillRect(Classes.Rect(0, 0, Panel.Width, Panel.Height));
    Font.Assign(Panel.Font);
    Font.Orientation := 900; // 2700;

    font.Height := panel.Width;
    Brush.Style := bsClear;
    TextW := TextWidth(TextStr);
    TextH := TextHeight(TextStr);
    // Center
    X := (Panel.Width - TextH) div 2;
    Y := (Panel.Height + TextW) div 2;
    TextOut(X, Y, TextStr);
  end;
end;

procedure TfBG.pmSettingsMeasureItem(Sender: TObject; ACanvas: TCanvas;
var AWidth, AHeight: integer);
const
  PaddingHorizontal = 40; // Additional width padding for icons or spacing
  PaddingVertical = 8;    // Vertical padding for spacing
  MinItemWidth = 100;     // Minimum width for menu items
  MinItemHeight = 25;     // Minimum height for menu items
begin
  // Validate the input
  if not (Sender is TMenuItem) then
    Exit;

  with TMenuItem(Sender) do
  begin
    // Skip separators (e.g., "-")
    if Caption = '-' then
      Exit;

    // Set desired font properties
    with ACanvas.Font do
    begin
      Name := 'Arial';
      Size := 16;
      Style := [fsBold];
    end;

    // Calculate text dimensions
    AWidth := ACanvas.TextWidth(Caption) + PaddingHorizontal;
    AHeight := ACanvas.TextHeight(Caption) + PaddingVertical;

    // Enforce minimum dimensions
    AWidth := Max(AWidth, MinItemWidth);
    AHeight := Max(AHeight, MinItemHeight);
  end;
end;

procedure TfBG.pmSettingsPopup(Sender: TObject);
{$ifdef LCLQt6}
function SafeQtStyle(Handle: QWidgetH; const Style: string): boolean;
  var
    ws: widestring;
  begin
    Result := false;

    // Safey checks for Qt6
    if Handle = nil then
      Exit;
    if PtrUInt(Handle) < $1000 then
      Exit;

    try
      // Test a simple operation
      if not QWidget_isEnabled(Handle) and QWidget_isEnabled(Handle) then
        Exit; // If false, the handle is not OK

      ws := UTF8Decode(Style);
      QWidget_setStyleSheet(Handle, PWideString(@ws));
      Result := true;

    except
      Result := false;
    end;
  end;
  {$endif}
var
  tpb: TPaintbox;
  H, M: integer;
  mi: integer;
begin
  // Shift down
  miAdvanced.Visible := ssShift in GetKeyShiftState;
  miATouch.Checked := HasTouch;
  {$ifdef DEBUG}
  miDebugBackend.Visible := true;
  {$endif}

  miDotNormal.Checked := false;
  miDotBig.Checked := false;
  miDotHuge.Checked := false;
  miDotVal.Checked := false;

  case dotscale of
  1:
    miDotNormal.Checked := true;
  2:
    miDotBig.Checked := true;
  3:
    miDotHuge.Checked := true;
  else
    miDotVal.Checked := true;
  end;

  if (Sender as TPopupMenu).PopupComponent is TPaintBox then
  begin
    tpb := (Sender as TPopupMenu).PopupComponent as TPaintBox;
    H := tpb.Tag div 100;
    M := tpb.Tag mod 100;

    miDotVal.Visible := true;
    miDotVal.Caption := Format(sReadingHere, [tpb.hint, H, M]);
  end
  else
    miDotVal.Visible := false;
  {$ifdef LCLQt6}
  Exit; // This crashes!
  if pmSettings.Tag <> 1 then
  begin
    if not SafeQtStyle(QWidgetH(pmSettings.Handle),
      'QMenu { font-size: 16pt; font-weight: bold; }' + LineEnding +
      'QMenu::item { padding: 8px 12px; }' + LineEnding +
      'QMenu::item:selected { background-color: highlight; }') then
      ShowMessage('Qt styling failed, using fallback menu'){$IFDEF DEBUG}{$ENDIF};

    pmSettings.Tag := 1;
  end;
  {$endif}
  last_popup := now;
end;

procedure TfBG.pnMultiUserClick(Sender: TObject);
begin
  if username <> '' then
  begin
    if multinick = username then
      ShowMessage(Format(RS_MULTINAME, [username]))
    else
      ShowMessage(Format(RS_MULTINAME_NAMED, [multinick, username]));
  end
  else
  if multinick <> RS_DEFAULT_ACCOUNT then
    ShowMessage(Format(RS_MULTINAME_DEF_NAMED, [multinick]))
  else
    ShowMessage(RS_MULTINAME_DEF);
end;
// Handle off range panel click
procedure TfBG.pnOffRangeClick(Sender: TObject);
var
  ishigh: boolean;
  {$ifdef TrndiExt}

  {$endif}
begin
  ishigh := (Sender as TPanel).Color = bg_rel_color_hi;
  {$ifdef TrndiExt}
  if not funcBool('uxClick',[
    'range',
    ishigh
    ], true) then
    Exit;
  {$endif}

  ShowMessage(Format(RS_RANGE_EXPLANATION, [IfThen(ishigh, RS_OVER, RS_UNDER)]));
end;

procedure TfBG.tAgoTimer(Sender: TObject);
var
  d: TDateTime;
  min: int64;
begin
  if firstboot then
    exit; // Dont run on first boot

  if sizeof(bgs) < 1 then
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO
  else
  try
    d := lastReading.date; // Last reading time
    min := MilliSecondsBetween(Now, d) div 60000;  // Minutes since last
    {$ifndef lclgtk2}// UTF support IS LIMITED
    lAgo.Caption := '🕑 ' + Format(RS_LAST_UPDATE, [min]);
    {$else}
    lAgo.Caption := '⌚ ' + Format(RS_LAST_UPDATE, [min]);
    {$endif}
  except
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO;
  end;
end;

procedure TfBG.tClockTimer(Sender: TObject);
var
  s: string;
  {$ifdef TrndiExt}
  ex: boolean; // If the function exists
  {$endif}
begin
  tClock.Enabled := false;
  if tClock.Interval <> clockDisplay then
  begin
    {$ifdef TrndiExt}
    s := '';
      // Check if JS engine is still available before calling
    s := callFuncWithBGReadings('clockView', [DateTimeToStr(Now)], ex);
    if ex = false then
      lval.caption := FormatDateTime(ShortTimeFormat, Now)
    else
      lval.caption := s;
    {$else}
    lval.Caption := FormatDateTime(ShortTimeFormat, Now);
    {$endif}
    tClock.Interval := clockDisplay;
    lArrow.Visible := false;
  end
  else
  begin
    lval.Caption := lval.hint;
    tClock.Interval := clockInterval;
    lArrow.Visible := true;
  end;
  tClock.Enabled := true;
end;

procedure TfBG.tEdgesTimer(Sender: TObject);
begin

end;

procedure TfBG.tInitTimer(Sender: TObject);
begin
  tInit.Enabled := false;
  self.Width := self.Width + 1;
  // Force a natural resize call, calling onResize doesnt work
end;

procedure TfBG.tResizeTimer(Sender: TObject);
begin
  tResize.Enabled := false;

  if DraggingWin then
  begin
    // During dragging, only update the warning panel and essential elements
    // Skip complex operations that could interfere with dragging performance
    if pnWarning.Visible then
    begin
      fixWarningPanel;
      ApplyAlphaControl(pnWarning, 235);
    end;
    Exit; // Don't do full resize operations during dragging
  end;

  // Normal full resize operations when not dragging
  actOnTrend(@HideDot);

  // Update trend related elements
  UpdateTrendElements;

  if not Assigned(api) then
    Exit;

  // Update meny and intervall
  UpdateApiInformation;

  if not api.active then
    Exit;

  // Adjust UI based on form size
  ResizeUIElements;

  // Place the dots
  UpdateTrendDots;
  actOnTrend(@showDot);

  // Post-process the dots
  AdjustGraph;

  fixWarningPanel;

  if lDot1.Caption <> DOT_GRAPH then
    actOnTrend(@ExpandDot);
end;

procedure TfBG.UpdateTrendElements;
begin
  // Change dot placement
  actOnTrend(@SetDotWidth);

  // Change dot size
  actOnTrend(@ResizeDot);
end;

procedure TfBG.UpdateApiInformation;
begin
  // Update high and low info label/menu item
  miHi.Caption := Format(RS_HI_LEVEL, [api.cgmHi * BG_CONVERTIONS[un][mgdl]]);
  miLo.Caption := Format(RS_LO_LEVEL, [api.cgmLo * BG_CONVERTIONS[un][mgdl]]);

  // Manage top intervall
  if api.cgmRangeHi <> 500 then
    miRangeHi.Caption := Format(RS_RANGE_HI, [api.cgmRangeHi * BG_CONVERTIONS[un][mgdl]])
  else
    miRangeHi.Caption := RS_RANGE_HI_UNSUPPORTED;

  // Hantera nedre intervall
  if api.cgmRangeLo <> 0 then
    miRangeLo.Caption := Format(RS_RANGE_LO, [api.cgmRangeLo * BG_CONVERTIONS[un][mgdl]])
  else
  begin
    miRangeLo.Caption := RS_RANGE_LO_UNSUPPORTED;
    miRangeColor.Enabled := false;
  end;
end;

procedure TfBG.ResizeUIElements;
begin
  // Anpassa storleken på panelen
  pnOffRange.Height := ClientHeight div 10;

  pnOffRange.Font.Size := 7 + pnOffRange.Height div 5;

  CenterPanelToCaption(pnOffRange);
  pnOffRangeBar.Height := pnOffRange.Height div 4;
  pnOffRangeBar.Width := ClientWidth + 10;

  // Anpassa huvudetiketterna
  ScaleLbl(lVal);

  // Konfigurera differensvisning
  lDiff.Width := ClientWidth;
  lDiff.Height := (ClientHeight div 9) - 10;
  lDiff.Top := ClientHeight - lDiff.Height + 1;
  ScaleLbl(lDiff);

  // Konfigurera tidsvisning
  lAgo.Width := ClientWidth div 2;
  lAgo.Height := ClientHeight div 9;
  lAgo.Left := 5;
  lAgo.Top := 1 + IfThen(pnOffRange.Visible, pnOffRange.Height, 3);
  ScaleLbl(lAgo, taLeftJustify);

  // Konfigurera trendpil

  lArrow.Height := ClientHeight;
  lArrow.Width := ClientWidth;
  lArrow.left := 0;
  lArrow.top := 0;
  ScaleLbl(lArrow);

  // TIR
  ScaleLbl(lTir);
  lTir.BringToFront;
  lTir.AutoSize := false;
  lTir.font := lAgo.Font;
  lTir.Height := lTir.Canvas.TextHeight(lTir.Caption);
  lTir.Width := lTir.Canvas.TextWidth(lTir.Caption);
  {$ifdef LCLQt6}
  if isWSL then
    lTir.Width := 50;
  {$endif}
  lTir.left := ClientWidth - lTir.Width - 5;
  lTir.top := 0;


  //  lArrow.font.color := LightenColor(fBG.Color, 0.8);
  //  ScaleLbl(lArrow, taRightJustify, tlTop);
  lArrow.OptimalFill := true;

  pnMultiUser.Width := min(max(clientwidth div 30, 5), 50);
  pnMultiUser.Height := clientheight;
  pnMultiUser.top := 0;
  pnMultiUser.left := 0;
  
  // Position and scale lPredict in lower-right corner
  if Assigned(lPredict) and lPredict.Visible then
  begin
    if PredictShortMode then
    begin
      // Short mode: larger area for bigger single arrow
      lPredict.Width := ClientWidth div 2;
      lPredict.Height := ClientHeight div 6;
    end
    else
    begin
      // Full mode: normal size for detailed predictions
      lPredict.Width := ClientWidth div 3;
      lPredict.Height := ClientHeight div 12;
    end;
    lPredict.Left := ClientWidth - lPredict.Width - 5;
    lPredict.Top := ClientHeight - lPredict.Height - 5;
    ScaleLbl(lPredict, taRightJustify, tlBottom);
  end;
end;

procedure TfBG.UpdateTrendDots;
var
  Dot: TPaintBox;
  Value: single; // Parsed from hint (user unit), then normalized to mmol/L
  ok: boolean;
  wasVisible: boolean;
begin
  for Dot in TrendDots do
  begin
    // Remember if dot was marked visible by PlaceTrendDots
    wasVisible := Dot.Visible;
    
    ok := TryStrToFloat(Dot.Hint, Value, native.locale);

    Dot.Font.Size := (ClientWidth div 24) * dotscale;
    if ok then
    begin
      // Normalize value to mmol/L for placement math
      if un = mgdl then
        Value := Value * BG_CONVERTIONS[mmol][mgdl];

      // Use same placement routine as initial draw to keep behavior consistent
      if Assigned(Dot.Parent) then
        SetPointHeight(Dot, Value, Dot.Parent.ClientHeight)
      else
        SetPointHeight(Dot, Value, fBG.ClientHeight);

      Dot.Visible := true;
    end
    else if Dot.Hint <> '' then
    begin
      // Hint is set but can't be parsed - this is a problem
      // Keep visibility as it was set by PlaceTrendDots, but log the issue
      LogMessage(Format('Warning: Could not parse Hint "%s" for dot. Keeping visibility=%s',
        [Dot.Hint, BoolToStr(wasVisible, true)]));
      Dot.Visible := wasVisible;
    end
    else
    begin
      // No hint means no data for this dot
      Dot.Visible := false;
    end;
  end;
end;

procedure TfBG.ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
customTl: TTextLayout = tlCenter);
var
  Low, High, Mid: integer;
  MaxWidth, MaxHeight: integer;
  TextWidth, TextHeight: integer;
  OptimalSize: integer;
begin
  if not ALabel.Visible then
    ALabel.Visible := true;

  if ALabel.Caption = '' then
    Exit; // No text

  // Check size
  if (ALabel.Width <= 0) or (ALabel.Height <= 0) then
  begin
    ALabel.Width := 250;
    ALabel.Height := 250;
  end;

  // Format
  ALabel.AutoSize := false;
  ALabel.WordWrap := false;
  ALabel.Alignment := customAl;
  ALabel.Layout := customTl;

  // Check visibility
  if ALabel.Font.Color = ALabel.Color then
    ALabel.Font.Color := clBlack;

  // Max size
  MaxWidth := ALabel.Width - 4; // Add padding
  MaxHeight := ALabel.Height - 4;

  // Find best font size
  Low := 1;
  High := 150;
  OptimalSize := 1;

  while Low <= High do
  begin
    Mid := (Low + High) div 2;
    ALabel.Font.Size := Mid;

    TextWidth := ALabel.Canvas.TextWidth(ALabel.Caption);
    TextHeight := ALabel.Canvas.TextHeight(ALabel.Caption);

    if (TextWidth <= MaxWidth) and (TextHeight <= MaxHeight) then
    begin
      OptimalSize := Mid;
      Low := Mid + 1;
    end
    else
      High := Mid - 1;
  end;

  // Set best font size
  ALabel.Font.Size := OptimalSize;

  // Se till att inställningarna används
  ALabel.Refresh;
end;
// Update remote on timer
procedure TfBG.tMainTimer(Sender: TObject);
{$ifdef TrndiExt}
var
  bgvals: JSValueRaw;
  {$endif}
begin
  updateReading;
  {$ifdef TrndiExt}
  try

    bgvals := getBGResults;
    callFuncWithBGReadings('updateCallback', [DateTimeToStr(Now)]);
  finally
  end;
  {$endif}
end;

procedure TfBG.tMissedTimer(Sender: TObject);
var
  d: TDateTime;
  min, sec: int64;
begin
  if firstboot then
    exit;

  d := lastReading.date; // Last reading time

  min := MilliSecondsBetween(Now, d) div 60000;  // Minutes since last
  sec := (MilliSecondsBetween(Now, d) mod 60000) div 1000; // Seconds since last

  lDiff.Caption := Format(RS_OUTDATED_TIME, [FormatDateTime('H:mm', d), min, sec]);
end;

procedure TfBG.tSetupTimer(Sender: TObject);
begin
  tSetup.Enabled := false;
  UpdateUIColors; // Force an update of TIR
end;

procedure TfBG.tSwapTimer(Sender: TObject);
var
  c: TColor;
  s: string;
begin
  // Dont swap when the reading is old
  if fsStrikeOut in Lval.Font.Style then
  begin
    lArrow.Visible := true;
    Exit;
  end;

  tSwap.Enabled := false;
  s := lval.Caption;

  if s.IndexOf(':') > 0 then // Clock showing
    UpdateUIColors   // Resets standard coloring
  else
  if lVal.font.color <> lArrow.font.color then
  begin // Neutral mode
    lArrow.BringToFront;
    c := lArrow.Font.Color;
    lArrow.Font.color := lVal.Font.color;
    lVal.Font.color := c;
  end
  else
  begin
    UpdateUIColors;   // Resets standard coloring
    lArrow.SendToBack;
    lVal.BringToFront;
    lArrow.SendToBack;
  end;
  tSwap.Enabled := true;
end;

// Handle a touch screen's long touch
procedure TfBG.tTouchTimer(Sender: TObject);
var
  p: TPoint;
  touchDuration: integer;
begin
  tTouch.Enabled := false;
  // Ensure we have a valid touch state and touch screen capability
  if IsTouched and HasTouch and (StartTouch > 0) then
  begin
    // Check if this is actually a long press (at least 450ms to account for timer precision)
    touchDuration := MilliSecondsBetween(Now, StartTouch);
    if touchDuration >= 450 then
    begin
      p := Mouse.CursorPos;
      if SecondsBetween(Now, last_popup) > 2 then
      begin
        pmSettings.PopUp(p.X, p.Y);
        last_popup := now;
      end;
    end;
  end;
end;

// Request data from the backend and update GUI
procedure TfBG.ProcessCurrentReading;
var
  b: BGReading;
begin
  if firstboot then
    exit;
  b := lastReading;

  // Update value label
  if not privacyMode then
  begin
    if b.val > 400 then
      lVal.Caption := RS_HIGH
    else
    if b.val < 40 then
      lVal.Caption := RS_LOW
    else
      lVal.Caption := b.format(un, BG_MSG_SHORT, BGPrimary);
  end
  else
    lVal.Caption := '';

  lval.hint := lval.Caption;

  // Update other UI elements
  lDiff.Caption := b.format(un, BG_MSG_SIG_SHORT, BGDelta);
  lArrow.Caption := b.trend.Img;
  lVal.Font.Style := [];

  // Log latest reading
  LogMessage(Format(RS_LATEST_READING, [b.val, DateTimeToStr(b.date)]));

  // Announce
  if miAnnounce.Checked then
    speakReading;

  // Set next update time
  SetNextUpdateTimer(b.date);
end;

function TfBG.IsDataFresh: boolean;
var
  b: BGReading;   // Holder for the latest (newest) reading
  i: integer;     // Temp int used when checking if caption starts with a digit
begin
  if firstboot then
    exit;

  b := lastReading; // Pick the most recent reading from the buffer

  // Consider data fresh if the latest reading is within the configured threshold (in minutes)
  Result := MinutesBetween(Now, b.date) <= DATA_FRESHNESS_THRESHOLD_MINUTES;

  if not Result then
  begin
    tMissed.OnTimer(tMissed);         // Immediately trigger the "missed" handler
    lVal.Font.Style := [fsStrikeOut]; // Strike through the value to indicate stale data
    // If the value label isn't numeric (e.g., "Setup") or arrow is a placeholder, show neutral placeholders
    if (not TryStrToInt(lVal.Caption[1], i)) or (lArrow.Caption = 'lArrow') then
    begin // Dont show "Setup" or similar on boot
      lVal.Caption := '--';           // Placeholder when data is stale
      lArrow.Caption := '';           // Hide trend arrow when stale
    end;
    setColorMode(clBlack);
    lVal.Font.Color := clWhite;       // High-contrast text on dark background
    tMissed.Enabled := true;          // Keep the missed timer running
    lArrow.Caption := '';             // Dont show arrow when not fresh
    native.setBadge('--', clBlack, badge_width + badge_adjust,
      badge_font + round(badge_adjust * 10)); // Update system/taskbar badge
    native.StopBadgeFlash;            // Stop any flashing when stale
  end
  else
  begin
    tMissed.Enabled := false;         // Data is fresh; stop missed timer
    bg_alert := true;                 // Allow alerts again
  end;
end;

procedure TfBG.SetNextUpdateTimer(const LastReadingTime: TDateTime);
var
  i: int64;
begin
  if firstboot then
    exit;
  tMain.Enabled := false;

  i := SecondsBetween(LastReadingTime, now); // Seconds from last
  i := min(BG_REFRESH, BG_REFRESH - (i * 1000));
  // 5 min or less if there's a recent reading
  i := max(120000, i); // Don't allow too small refresh time (min 2 minutes)

  tMain.Interval := i + 15000; // Add 15 secs to allow sync
  tMain.Enabled := true;

  miRefresh.Caption := Format(RS_REFRESH, [TimeToStr(LastReadingTime),
    TimeToStr(IncMilliSecond(Now, tMain.Interval))]);
  {$ifdef Darwin}
  upMenu.Caption:= miRefresh.Caption;
  {$endif}
end;

// Calculate time in range
procedure tfBG.CalcRangeTime;
var
  b: BGReading; //< Holder for current reading
  range, //< The OK range
  ok, //< OK count
  no: integer; //< Not OK count
  ranges: set of trndi.types.BGValLevel; //< Types of readings to count as OK
begin
  ok := 0;
  no := 0;
  if native.GetBoolSetting('range.custom', false) then
    ranges := [BGRange, BGRangeHI, BGRangeLO]
  else
    ranges := [BGRange];

  for b in bgs do
    if b.level in ranges then
      Inc(ok)
    else
      Inc(no);

  if (ok + no) > 0 then
    range := round((ok / (ok + no)) * 100)
  else
    range := 0;

  lTir.Caption := range.toString + '%';
  lTir.Hint := range.toString;
  lTir.Visible := true; // Ensure TIR label is visible after calculation
end;

function TfBG.updateReading(boot: boolean = false): boolean;
begin
  Result := false;
  lAgo.Caption := '⟳' + lAgo.Caption;

  native.start;
  lastup := 0;

  if not tAgo.Enabled then
    lAgo.Caption := '🕑 ' + RS_UNKNOWN_TIME;


  // Fetch readings and exit if no data
  if not FetchAndValidateReadings then
    Exit;

  // Process the newest reading
  ProcessCurrentReading;

  // Handle data freshness
  if not IsDataFresh then
  begin
    if not boot then
      Exit;
  end
  else
    Result := true;

  // Complete the UI update sequence
  FinalizeUIUpdate;

  {$ifdef TrndiExt}
  // Check if JS engine is still available before calling
  callFunc('fetchCallback',[
    bgs[0].format(mgdl, BG_MSG_SHORT), //mgdl reading
    bgs[0].format(mmol, BG_MSG_SHORT), //mmol reading
    bgs[0].format(mgdl, BG_MSG_SIG_SHORT, BGDelta), //mgdl diff
    bgs[0].format(mmol, BG_MSG_SHORT, BGDelta), //mmol diff
    not bgs[0].empty // has reading?
    ]);
  {$endif}
end;

procedure TfBG.FinalizeUpdate;
begin
  lastup := Now;

  // Handle privacy mode display
  if privacyMode then
  begin
    lval.Caption := privacyIcon(lastReading.level);
    lVal.hint := lval.Caption;
  end;

  // Update timers and UI
  tAgo.Enabled := true;
  tAgo.OnTimer(self);
  Self.OnResize(lVal);

  // Update floating window if assigned
  UpdateFloatingWindow;

  // Update prediction label if enabled
  UpdatePredictionLabel;

  // Update text colors based on background only if UI changed
  if ShouldUpdateUI(fBG.Color, lVal.Caption, lTir.Caption, lTir.Color) then
  begin
    UpdateUIColors;
    CacheUIState(fBG.Color, lVal.Caption, lTir.Caption, lTir.color);
  end;

  // Update system integration
  native.setBadge(lVal.Caption, fBG.Color
    {$ifdef lclwin32}
    ,badge_width,badge_font
    {$endif}
    );
  native.done;

  if privacyMode then
    DOT_ADJUST := randomrange(-3, 3) / 10;

end;

procedure TfBG.UpdatePredictionLabel;
var
  bgr: BGResults;
  pred1, pred2, pred3: string;
  lastReadingTime: TDateTime;
  lastReadingValue: single;
  i, closest5, closest10, closest15: integer;
  diff5, diff10, diff15, currentDiff: integer;
  delta: single;
  trend: BGTrend;
  minutes: integer;
  validCount: integer;
begin
  // Safety check - api might not be initialized yet
  if not Assigned(api) then
  begin
    lPredict.Visible := false;
    Exit;
  end;
  
  if not PredictGlucoseReading then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  lPredict.Visible := true;

  // Request more predictions to get ones closer to 5, 10, 15 minutes
  if not api.predictReadings(9, bgr) then
  begin
    lPredict.Caption := RS_PREDICTIONS_UNAVAILABLE;
    Exit;
  end;

  // Get the last reading time and value to calculate trend
  lastReadingTime := lastReading.date;
  lastReadingValue := lastReading.convert(mgdl);

  // Find predictions closest to 5, 10, and 15 minutes
  // Ensure we pick different predictions for each slot
  closest5 := -1;
  closest10 := -1;
  closest15 := -1;
  diff5 := MaxInt;
  diff10 := MaxInt;
  diff15 := MaxInt;

  for i := 0 to High(bgr) do
  begin
    currentDiff := Round(MinutesBetween(bgr[i].date, lastReadingTime));
    
    // Skip predictions that are too close to current time (less than 2 minutes ahead)
    if currentDiff < 2 then
      continue;
    
    // Find closest to 5 minutes
    if Abs(currentDiff - 5) < diff5 then
    begin
      diff5 := Abs(currentDiff - 5);
      closest5 := i;
    end;
    
    // Find closest to 10 minutes (but different from closest5)
    if (Abs(currentDiff - 10) < diff10) and (i <> closest5) then
    begin
      diff10 := Abs(currentDiff - 10);
      closest10 := i;
    end;
    
    // Find closest to 15 minutes (but different from closest5 and closest10)
    if (Abs(currentDiff - 15) < diff15) and (i <> closest5) and (i <> closest10) then
    begin
      diff15 := Abs(currentDiff - 15);
      closest15 := i;
    end;
  end;

  // Check if we have at least one valid prediction
  validCount := 0;
  if closest5 >= 0 then Inc(validCount);
  if closest10 >= 0 then Inc(validCount);
  if closest15 >= 0 then Inc(validCount);
  
  if validCount = 0 then
  begin
    lPredict.Visible := false;
    Exit;
  end;

  // Format predictions with clock emoji, trend arrows, and values
  if PredictShortMode then
  begin
    // Short mode: show only middle prediction with simplified arrow
    if closest10 >= 0 then
    begin
      delta := bgr[closest10].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      // Map to simplified arrows: up=↗, flat=→, down=↘
      case trend of
        TdDoubleUp, TdSingleUp, TdFortyFiveUp: lPredict.Caption := '↗';
        TdFlat: lPredict.Caption := '→';
        TdFortyFiveDown, TdSingleDown, TdDoubleDown: lPredict.Caption := '↘';
        else lPredict.Caption := '?';
      end;
    end
    else
      lPredict.Caption := '?';
  end
  else
  begin
    // Full mode: show time, trend, and value
    if closest5 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest5].date, lastReadingTime));
      delta := bgr[closest5].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred1 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest5].convert(un)]);
    end
    else
      pred1 := '?';

    if closest10 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest10].date, lastReadingTime));
      delta := bgr[closest10].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred2 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest10].convert(un)]);
    end
    else
      pred2 := '?';

    if closest15 >= 0 then
    begin
      minutes := Round(MinutesBetween(bgr[closest15].date, lastReadingTime));
      delta := bgr[closest15].convert(mgdl) - lastReadingValue;
      trend := CalculateTrendFromDelta(delta);
      pred3 := Format('⏱%d'' %s %.1f', [minutes, BG_TREND_ARROWS_UTF[trend], bgr[closest15].convert(un)]);
    end
    else
      pred3 := '?';

    lPredict.Caption := Format('%s | %s | %s', [pred1, pred2, pred3]);
  end;
end;

procedure TfBG.CompleteUIUpdate;
begin
  // Process the current reading and update UI
  ProcessCurrentReading;
  UpdateUIBasedOnGlucose;
  FinalizeUpdate;
  CalcRangeTime;
end;

procedure TfBG.FinalizeUIUpdate;
begin
  // Complete UI update sequence after data processing
  UpdateUIBasedOnGlucose;
  FinalizeUpdate;
  CalcRangeTime;
end;

procedure TfBG.UpdateFloatingWindow;
begin
  if Assigned(fFloat) then
  begin
    fFloat.Color := fBg.Color;
    fFloat.lVal.Caption := lval.Caption;
    fFloat.lArrow.Caption := lArrow.Caption;
  end;
end;

procedure TfBG.UpdateUIBasedOnGlucose;
var
  b: BGReading;
  col: TColor;
  txt: string;
begin
  b := lastReading;

  if b.val >= api.cgmHi then
    HandleHighGlucose(b)
  else
  if b.val <= api.cgmLo then
    HandleLowGlucose(b)
  else
    HandleNormalGlucose(b);

  pnOffReading.Visible := native.GetBoolSetting('ux.off_bar', false);
  case b.level of
  trndi.types.BGHigh:
    txt := RS_HIGH;
  trndi.types.BGLOW:
    txt := RS_LOW;
  trndi.types.BGRange:
  begin
    txt := '';
    pnOffReading.Visible := false;
  end;
  trndi.types.BGRangeHI:
    txt := RS_OFF_HI;
  trndi.types.BGRangeLO:
    txt := RS_OFF_LO;
  end;

  pnOffReading.Caption := txt;

  pnOffReading.color := GetAdjustedColorForBackground(fBG.Color, fBG.Color);
  pnOffReading.font.color := GetTextColorForBackground(pnOffReading.Color, 0, 0.7);
end;

procedure TfBG.HandleHighGlucose(const b: BGReading);
var
  url: string;
  doFlash: boolean;
begin
  setColorMode(bg_color_hi);

  if not bg_alert then
    native.attention(ifthen(multi, multinick, RS_WARN_BG_HI_TITLE),
      Format(RS_WARN_BG_HI, [lVal.Caption]));

  if highAlerted then
    Exit;

  if native.GetBoolSetting('media.pause') then
    MediaController.Pause;

  if native.TryGetSetting('media.url_high', url) then
  begin
    highAlerted := true;
    MediaController.PlayTrackFromURL(url);
  end;
  doFlash := native.GetBoolSetting('alerts.flash.high', false);
  if (not highAlerted) and doFlash then
    native.StartBadgeFlash(lVal.Caption, bg_color_hi, 15000, 450);
end;

procedure TfBG.HandleLowGlucose(const b: BGReading);
var
  url: string;
  doFlash: boolean;
begin
  SetColorMode(bg_color_lo);

  if not bg_alert then
    native.attention(ifthen(multi, multinick, RS_WARN_BG_LO_TITLE),
      Format(RS_WARN_BG_LO, [lVal.Caption]));

  if lowAlerted then
    exit;

  if native.GetBoolSetting('media.pause') then
    MediaController.Pause;

  if native.TryGetSetting('media.url_low', url) then
  begin
    lowAlerted := true;
    MediaController.PlayTrackFromURL(url);
  end;
  doFlash := native.GetBoolSetting('alerts.flash.low', false);
  if (not lowAlerted) and doFlash then
    native.StartBadgeFlash(lVal.Caption, bg_color_lo, 20000, 400);
end;

procedure TfBG.HandleNormalGlucose(const b: BGReading);
var
  s, url: string;
  i: integer;
  f: single;
  go: boolean = false;
begin
  bg_alert := false;
  SetColorMode(bg_color_ok);
  highAlerted := false;
  lowAlerted := false;
  native.StopBadgeFlash; // cease alerts when normal

  if un = mmol then
  begin
    s := b.format(mmol, BG_MSG_SHORT, BGPrimary);
    if (TryStrToFloat(s, f, native.locale)) and (f = 5.5) then
      go := true
    else
      perfecttriggered := false;
  end
  else
  begin
    s := b.format(mgdl, BG_MSG_SHORT, BGPrimary);
    if (TryStrToInt(s, i)) and (i = 100) then
      go := true
    else
      perfecttriggered := false;
  end;

  if go and (not perfecttriggered) then
  begin
    perfectTriggered := true;

    if native.TryGetSetting('media.url_perfect', url) then
      MediaController.PlayTrackFromURL(url);
    if native.GetBoolSetting('alerts.flash.perfect', false) then
      native.StartBadgeFlash(lVal.Caption, bg_color_ok, 6000, 500);
    // subtle celebratory pulse
  end;

  UpdateOffRangePanel(b.val);
end;

procedure TfBG.fixWarningPanel;
var
  padding: integer;
  calculatedFontSize: integer;
  i: integer;
  bg: BGReading;
  last, val: string;
  tf: TFormatSettings;
begin
  // Calculate padding consistently
  padding := (ClientWidth div 25);
  if not native.HasTouchScreen then
    padding := padding * 2;

  // First, set panel dimensions and ensure it's properly configured
  pnwarning.AutoSize := false;
  pnwarning.Width := ClientWidth - (padding * 2);  // Use padding on both sides
  pnWarning.left := padding;
  pnwarning.top := padding;
  pnWarning.Height := ClientHeight - (padding * 2);  // Use padding on top and bottom

  // Configure the main warning label first
  if Pos(sLineBreak, lMissing.Caption) < 1 then // Ugly solution
    lMissing.Caption := '🕑' + sLineBreak + lMissing.Caption;

  lMissing.AutoSize := false;
  lMissing.left := 5;
  lMissing.top := 5;
  lMissing.Width := pnWarning.Width - 10;
  lMissing.Height := pnWarning.Height - 60; // Leave room for pnWarnLast at bottom
  lMissing.wordwrap := true;
  lMissing.OptimalFill := true;

  // Use the same scaling method as resize operations for consistency
  // This ensures the font size matches what you get when manually resizing
  ScaleLbl(lMissing, taCenter, tlCenter);

  // Ensure font color is visible
  if native.HasTouchScreen then
    pnWarning.Font.Color := pnWarning.color;

  // Configure other UI elements
  pnOffReading.Height := ClientHeight;
  pnOffReading.ClientWidth := ClientWidth div 35;

  if tryLastReading(bg) then
  begin
    val := bg.format(un, BG_MSG_SHORT);
    last := HourOf(bg.date).ToString + ':' + MinuteOf(bg.date).tostring;
    if DateOf(bg.date) <> Dateof(now) then
      last := last + ' ' + Format(RS_DAYS_AGO, [DaysBetween(now, bg.date)]);
    pnWarnLast.Caption := Format(RS_LAST_RECIEVE, [val, last]);
  end
  else
    pnWarnLast.Caption := RS_LAST_RECIEVE_NO;

  pnWarnLast.Caption := pnWarnLast.Caption + LineEnding +
    Format(RS_LAST_RECIEVE_AGE, [DATA_FRESHNESS_THRESHOLD_MINUTES]);

  // Set pnWarnLast font size relative to main font, but with bounds checking
  pnWarnLast.font.size := Max(8, Min(20, calculatedFontSize div 3));
  pnWarning.Canvas.Font.size := pnWarnLast.font.size;

  // Position pnWarnLast at bottom of panel
  pnWarnLast.Height := pnWarning.Canvas.TextHeight(pnWarnLast.Caption) * 2;
  pnWarnLast.Width := Min(pnWarning.Width - 10,
    pnWarning.Canvas.TextWidth(pnWarnLast.Caption) + 10);
  pnWarnLast.left := 5;
  pnWarnLast.top := pnWarning.Height - pnWarnLast.Height - 5;
end;

procedure TfBG.showWarningPanel(const message: string;
clearDisplayValues: boolean = false);
var
  i: integer;
begin
  pnWarning.Visible := true;
  pnWarning.Caption := '⚠️ ' + message;

  if clearDisplayValues then
    if (not TryStrToInt(lVal.Caption[1], i)) or (lArrow.Caption = 'lArrow') then
    begin // Dont show "Setup" or similar on boot
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;

  pnWarning.Caption := '';  // Clear for now
  fixWarningPanel;
  // Ensure visual effects are applied when showing the panel
  ApplyAlphaControl(pnWarning, 235);

  // Force a complete UI update to ensure proper rendering on all platforms
  pnWarning.Refresh;
  lMissing.Refresh;
end;

function TfBG.DoFetchAndValidateReadings(const ForceRefresh: boolean): boolean;
var
  {$ifdef DEBUG}
  res: string;
  {$endif}
  i: integer;
const
  API_CACHE_SECONDS = 10; // Don't call API more than once per 10 seconds
begin
  Result := false;

  if api = nil then
    Exit;

  // Performance optimization: use cached readings if recent (unless forced)
  if (not ForceRefresh) and (SecondsBetween(Now, FLastAPICall) < API_CACHE_SECONDS) and
    (Length(FCachedReadings) > 0) then
  begin
    bgs := FCachedReadings;
    Result := true;
    Exit;
  end;

  {$ifdef DEBUG}
  bgs := api.getReadings(MAX_MIN, MAX_RESULT, '', res);
  if miDebugBackend.Checked then
    if Showing then
      if res.IsEmpty then
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'),
          '[empty!]', res)
      else
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'),
          '', res, uxmtCustom, 10);
  {$ELSE}
  bgs := api.getReadings(MAX_MIN, MAX_RESULT);
  {$endif}

  // Cache the API call and results
  FLastAPICall := Now;
  SetLength(FCachedReadings, Length(bgs));
  if Length(bgs) > 0 then
    Move(bgs[0], FCachedReadings[0], Length(bgs) * SizeOf(BGReading));

  // Reapply override settings after API fetch (API may have set its own defaults)
  if native.GetBoolSetting('override.enabled') then
  begin
    api.cgmLo := native.GetIntSetting('override.lo', api.cgmLo);
    api.cgmHi := native.GetIntSetting('override.hi', api.cgmHi);
  end;
  if native.GetBoolSetting('override.range') then
  begin
    api.cgmRangeLo := native.GetIntSetting('override.rangelo', api.cgmRangeLo);
    api.cgmRangeHi := native.GetIntSetting('override.rangehi', api.cgmRangeHi);
  end;

  pnWarning.Visible := false;
  if (Length(bgs) < 1) or (not IsDataFresh) then
  begin
    showWarningPanel(RS_NO_BACKEND, true);
    Exit;
  end;

  LogMessage(Format('DoFetchAndValidateReadings: Got %d readings from API', [Length(bgs)]));

  // Call the method to place the points
  PlaceTrendDots(bgs);
  Result := true;
end;

function TfBG.FetchAndValidateReadings: boolean;
begin
  Result := DoFetchAndValidateReadings(false); // Use cached data if available
end;

function TfBG.FetchAndValidateReadingsForced: boolean;
begin
  Result := DoFetchAndValidateReadings(true); // Force fresh API call, bypass cache
end;

procedure TfBG.UpdateOffRangePanel(const Value: double);
var
  on: boolean = true;
begin
  if (Value >= api.cgmHi) or (Value <= api.cgmLo) then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
    if Assigned(fFloat) then
    begin
      ffloat.lRangeDown.Visible := false;
      ffloat.lRangeUp.Visible := false;
    end;
  end
  else
  if Value <= api.cgmRangeLo then
    DisplayLowRange
  else
  if Value >= api.cgmRangeHi then
    DisplayHighRange
  else
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
    on := false;
  end;

  // Apply range color if option is enabled
  if on and miRangeColor.Checked then
    SetColorMode(pnOffRange.Color);
end;

procedure TfBG.UpdateUIColors;
const
  clGoodGreen = $00005900;
var
  r: integer;
begin
  lVal.Font.Color := GetTextColorForBackground(fBG.color);
  //  lVal.BringToFront;
  //  lArrow.Font.Color := LightenColor(fbg.color, 0.3); // GetTextColorForBackground(fBG.color, 0, 0.9);
  lArrow.Font.Color := LightenColor(fBG.color, 0.5);
  lArrow.SendToBack;
  lDiff.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lAgo.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lTir.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);
  lPredict.Font.Color := GetTextColorForBackground(fBG.color, 0.6, 0.4);


  if TryStrToInt(lTir.hint, r) then
    if r < bad_tir then // If the value is under the limit for "bad"
      lTir.Font.color := GetAdjustedColorForBackground(clMaroon,
        fBG.Color, 0.6, 0.4, true)
    else
    if r >= good_tir then
      lTir.Font.color := GetAdjustedColorForBackground(clGoodGreen,
        fBG.Color, 0.6, 0.4, true)// Check time in range
  ;

end;

function TfBG.GetTextColorForBackground(const BgColor: TColor;
const DarkenFactor: double = 0.5; const LightenFactor: double = 0.3): TColor;
begin
  if IsLightColor(BgColor) then
    Result := DarkenColor(BgColor, DarkenFactor)
  else
    Result := LightenColor(BgColor, LightenFactor);
end;

function TfBG.GetAdjustedColorForBackground(const BaseColor: TColor;
const BgColor: TColor; const DarkenFactor: double = 0.6;
const LightenFactor: double = 0.4; const PreferLighter: boolean = false): TColor;
begin
  if PreferLighter then
    Result := LightenColor(BaseColor, LightenFactor)// Tvinga ljusare förgrund

  else
  if IsLightColor(BgColor) then
    Result := DarkenColor(BaseColor, DarkenFactor)
  else
    Result := LightenColor(BaseColor, LightenFactor);
end;

procedure TfBG.DisplayLowRange;
begin
  pnOffRange.Color := bg_rel_color_lo;
  pnOffRangeBar.Color := bg_rel_color_lo;
  pnOffRange.Font.Color := bg_rel_color_lo_txt;
  if miRangeColor.Checked then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else
  begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
    setColorMode;
  end;
  pnOffRangeBar.Visible := true;
  pnOffRange.Caption := Format('↧ %s ↧', [RS_OFF_LO]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then
      ffloat.lRangeDown.Visible := true;
    ffloat.Font.color := bg_rel_color_lo_txt;
  end;
  CenterPanelToCaption(pnOffRange);
end;

procedure TfBG.DisplayHighRange;
begin
  pnOffRange.Color := bg_rel_color_hi;
  pnOffRangeBar.Color := bg_rel_color_hi;
  pnOffRange.Font.Color := bg_rel_color_hi_txt;
  if miRangeColor.Checked then
  begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else
  begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
    setColorMode;
  end;
  pnOffRange.Caption := Format('↥ %s ↥', [RS_OFF_HI]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then
      ffloat.lRangeUp.Visible := true;
    ffloat.Font.color := bg_rel_color_hi_txt;
  end;
  CenterPanelToCaption(pnOffRange);
end;

// PlaceTrendDots method to map readings to TrendDots
procedure TfBG.PlaceTrendDots(const Readings: array of BGReading);
var
  SortedReadings: array of BGReading;
  currentTime: TDateTime;
  currentHash: cardinal;
begin
  if Length(Readings) = 0 then
    Exit;

  // Performance optimization: check if data actually changed
  currentHash := CalculateReadingsHash(Readings);
  if currentHash = FLastReadingsHash then
    Exit; // No change, skip processing

  FLastReadingsHash := currentHash;

  // Prepare
  currentTime := Now;
  SetLength({%H-}SortedReadings, Length(Readings));
  Move(Readings[0], SortedReadings[0], Length(Readings) * SizeOf(BGReading));

  // Sort readings Descending (newest first)
  SortReadingsDescending(SortedReadings);

  // Manage the last readings freshness
  HandleLatestReadingFreshness(SortedReadings[0], currentTime);

  // Process every interval
  ProcessTimeIntervals(SortedReadings, currentTime);
end;

procedure TfBG.HandleLatestReadingFreshness(const LatestReading: BGReading;
CurrentTime: TDateTime);
var
  isFresh: boolean;
begin
  isFresh := MinutesBetween(CurrentTime, LatestReading.date) <=
    DATA_FRESHNESS_THRESHOLD_MINUTES;

  if Assigned(TrendDots[10]) then
  begin
    TrendDots[10].Visible := isFresh;

    if isFresh then
      LogMessage('TrendDots[10] shown as latest reading is fresh.')
    else
      LogMessage('TrendDots[10] hidden due to outdated reading.');
  end;
end;

procedure TfBG.ProcessTimeIntervals(const SortedReadings: array of BGReading;
CurrentTime: TDateTime);
var
  slotIndex, i, labelNumber, searchStart: integer;
  slotStart, slotEnd, anchorTime: TDateTime;
  found: boolean;
  reading: BGReading;
  l: TPaintbox;
begin
  if Length(SortedReadings) = 0 then
    Exit;

  searchStart := 0;
  // Anchor intervals to the most recent reading, not current time
  // This ensures dots remain visible even if data is slightly delayed
  anchorTime := SortedReadings[0].date;
  
  LogMessage(Format('PlaceTrendDots: Processing %d readings, anchor=%s', 
    [Length(SortedReadings), DateTimeToStr(anchorTime)]));

  for slotIndex := 0 to NUM_DOTS - 1 do
  begin
    // Set start and end time for the interval, anchored to latest reading
    // For slot 0, we want to include readings from (anchorTime - 5 min) to anchorTime
    slotEnd := IncMinute(anchorTime, -INTERVAL_MINUTES * slotIndex);
    slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

    found := false;

    LogMessage(Format('Searching slot %d (TrendDots[%d]): %s to %s', 
      [slotIndex, NUM_DOTS - slotIndex, DateTimeToStr(slotStart), DateTimeToStr(slotEnd)]));

    // Search for the most recent reading within this interval
    for i := searchStart to High(SortedReadings) do
    begin
      reading := SortedReadings[i];

      // For slot 0, be more lenient with the upper bound to catch the anchor reading
      // Use a 10 second epsilon to handle timing variations
      if reading.date > slotEnd + (10 / 86400) then
      begin
        LogMessage(Format('  Reading at %s is too new (>%.3f sec after slot end), skipping', 
          [DateTimeToStr(reading.date), (reading.date - slotEnd) * 86400]));
        Continue;
      end;

      // Check if reading falls within this interval BEFORE checking if it's too old
      // This ensures boundary readings (exactly at slotStart) get matched
      if (reading.date >= slotStart - (10 / 86400)) and (reading.date <= slotEnd + (10 / 86400)) then
      begin
        LogMessage(Format('  Found match at %s (value: %.1f, diff from slotEnd: %.1f sec)', 
          [DateTimeToStr(reading.date), reading.val, (slotEnd - reading.date) * 86400]));
        found := UpdateLabelForReading(slotIndex, reading);
        if found then
        begin
          // Only advance searchStart if this reading is NOT on a slot boundary
          // Boundary readings (at slotStart) should be available for the next slot too
          if Abs(reading.date - slotStart) > (10 / 86400) then
            searchStart := i + 1
          else
          begin
            // Reading is at the boundary - next slot should also check it
            LogMessage(Format('  Reading at boundary (%.2f sec from slotStart), not advancing searchStart', 
              [(reading.date - slotStart) * 86400]));
          end;
          Break; // Move to next interval
        end;
      end
      else if reading.date < slotStart - (10 / 86400) then
      begin
        // Stop if we've gone past this interval into older readings
        LogMessage(Format('  Reading at %s is too old (%.3f sec before slot start), stopping', 
          [DateTimeToStr(reading.date), (slotStart - reading.date) * 86400]));
        Break;
      end;
    end;

    // Hide label if no reading found in this interval
    if not found then
    begin
      labelNumber := NUM_DOTS - slotIndex;
      l := TrendDots[labelNumber];

      if Assigned(l) then
      begin
        l.Visible := false;
        l.Hint := '';  // Clear hint to prevent UpdateTrendDots from re-showing stale data
        LogMessage(Format('TrendDots[%d] hidden as no reading found in interval.',
          [labelNumber]));
      end;
    end;
  end;
  
  // Summary log
  LogMessage(Format('PlaceTrendDots complete: anchor=%s', [DateTimeToStr(anchorTime)]));
end;

function TfBG.UpdateLabelForReading(SlotIndex: integer;
const Reading: BGReading): boolean;
var
  labelNumber: integer;
  l: TPaintBox;
  H, M, S, MS: word;
begin
  if firstboot then
    exit;
  Result := false;

  // Mappa slotIndex till etikettens nummer (0 -> lDot10, 1 -> lDot9, ..., 9 -> lDot1)
  labelNumber := NUM_DOTS - SlotIndex;
  l := TrendDots[labelNumber];

  if Assigned(l) then
  begin
    // Uppdatera etikettens egenskaper baserat på läsningen
    l.Visible := true;
    l.Hint := Reading.format(un, BG_MSG_SHORT, BGPrimary);

    DecodeTime(reading.date, H, M, S, MS);
    l.Tag := H * 100 + M; // 10:00 = 1000

    l.Caption := DOT_GRAPH;

    // Use the dot's parent client height to align placement with visibility checks
    setPointHeight(l, Reading.convert(mmol), l.Parent.ClientHeight);

    // Sätt färger baserat på värdet
    l.Font.Color := DetermineColorForReading(Reading);

    LogMessage(Format('TrendDots[%d] updated with reading at %s (Value: %.2f).',
      [labelNumber, DateTimeToStr(Reading.date), Reading.val]));
    l.BringToFront;
    Result := true;
  end;
end;

function TfBG.DetermineColorForReading(const Reading: BGReading): TColor;
begin
  if Reading.val >= api.cgmHi then
    Result := bg_color_hi
  else
  if Reading.val <= api.cgmLo then
    Result := bg_color_lo
  else
  begin
    Result := bg_color_ok_txt;

    if Reading.val <= api.cgmRangeLo then
      Result := bg_rel_color_lo
    else
    if Reading.val >= api.cgmRangeHi then
      Result := bg_rel_color_hi;
  end;

  Result := LightenColor(Result, -0.8);
end;

function TfBG.setColorMode: boolean;
begin
  Result := setColorMode(clFuchsia, true);
end;

function TfBG.setColorMode(bg: tColor; const nocolor: boolean = false): boolean;
begin
  if not nocolor then
    fBG.Color := bg;

  if not multi // not more users than 1
    and titlecolor // we have chosen to color the window
    and miRangeColor.Checked then
  begin // We color the entire window
    Result := SetSingleColorMode;
    Exit;
  end
  else
  if not miRangeColor.Checked // We are not coloring the entire window
    and pnOffRange.Visible then
    if native.SetTitleColor(handle, pnOffRange.Color,
      IfThen(IsLightColor(pnOffRange.Color), clBlack, clWhite)) then
      Exit;

  if customTitleBar and (pnMultiUser.Color <> clBlack) then
    if native.SetTitleColor(handle, pnMultiUser.Color,
      IfThen(IsLightColor(pnMultiUser.Color), clBlack, clWhite)) then
      Exit(true)// Safe that black = standard
  ;

  if native.isDarkMode then
    native.setDarkMode
    {$ifdef windows}
    (self.Handle)
  {$endif}
  ;

  Result := false;
end;

function TfBG.setSingleColorMode: boolean;
begin
  Result := native.SetTitleColor(handle, fBG.color,
    IfThen(IsLightColor(fBG.color), clBlack, clWhite));
end;

// Performance optimization methods
function TfBG.CalculateReadingsHash(const Readings: array of BGReading): cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(Readings) do
    Result := Result xor cardinal(Trunc(Readings[i].val * 100)) xor
      cardinal(Trunc(Readings[i].date * 86400));
end;

function TfBG.ShouldUpdateUI(const NewColor: TColor; const NewCaption: string;
const NewTir: string; const NewTirColor: TColor): boolean;
begin
  Result := (NewColor <> FLastUIColor) or (NewCaption <> FLastUICaption) or
    (NewTir <> FLastTir) or (NewTirColor <> FLastTirColor);
end;

procedure TfBG.CacheUIState(const UIColor: TColor; const UICaption: string;
const UITir: string; const UITirColor: TColor);
begin
  FLastUIColor := UIColor;
  FLastUICaption := UICaption;
  FLastTir := UITir;
  FLastTirColor := UITirColor;
end;

procedure TfBG.CheckForUpdates(ShowUpToDateMessage: boolean = false);
var
  res, rn, r, s: string;
  rok: boolean;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  latestRelease: string;
begin
  try
    // Fetch latest release info from GitHub
    TrndiNative.getURL('https://api.github.com/repos/slicke/trndi/releases/latest', res);
    
    // Try to get the latest release name from JSON
    latestRelease := 'Unknown';
    JsonData := nil;
    try
      JsonData := GetJSON(res);
      if Assigned(JsonData) and (JsonData is TJSONObject) then
      begin
        JsonObj := TJSONObject(JsonData);
        latestRelease := JsonObj.Get('name', JsonObj.Get('tag_name', 'Unknown'));
      end;
    except
      latestRelease := 'Parse error';
    end;
    
    // Free the JSON data after we're done with it
    if Assigned(JsonData) then
      FreeAndNil(JsonData);
    
    rok := HasNewerRelease(res, rn, false);

    if rok then
    begin
      r := GetNewerVersionURL(res);
      if r = '' then r := 'https://github.com/slicke/trndi/releases/latest';
      s := Format(RS_NEWVER, [rn]);
      if UXDialog(uxdAuto, RS_NEWVER_CAPTION, s, [mbYes, mbNo], mtInformation) = mrYes then
        OpenURL(r);
    end
    else if ShowUpToDateMessage then
    begin
      // Show debug info for dev builds
      if BUILD_NUMBER = 'dev' then
        ShowMessage('Up to date (Dev Build)' + LineEnding + 
                   'Build: ' + BUILD_NUMBER + LineEnding +
                   'Branch: ' + GIT_BRANCH + LineEnding +
                   'Build Date: ' + {$I %DATE%} + ' ' + {$I %TIME%} + LineEnding +
                   'Latest release: ' + latestRelease + LineEnding + LineEnding +
                   'Note: Dev builds are always considered "newer" than releases')
      else
        ShowMessage(RS_UPTODATE);
    end;
    // Silently ignore if up to date when ShowUpToDateMessage is false
  except
    on E: Exception do
      if ShowUpToDateMessage then
        ShowMessage('Update check failed: ' + E.Message);
  end;
end;

end.
