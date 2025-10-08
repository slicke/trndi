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
  slicke.ux.alert, usplash, Generics.Collections, trndi.funcs,
  Trndi.native.base, trndi.shared, trndi.api.debug_custom,
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
  trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug, trndi.api.debug_edge, trndi.api.debug_missing, trndi.api.debug_perfect, {$endif}
  {$ifdef LCLQt6}Qt6, QtWidgets,{$endif}
  StrUtils, TouchDetection, ufloat, LCLType;

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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure DotPaint(Sender: TObject);
    procedure lDiffClick(Sender: TObject);
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

    function dotsInView: integer;
    function setColorMode: boolean;
    function setColorMode(bg: tColor; const nocolor: boolean = False): boolean;
    function setSingleColorMode: boolean;
    procedure SetLang;
    procedure fixWarningPanel;
    procedure showWarningPanel(const message: string;
      clearDisplayValues: boolean = False);
    procedure CalcRangeTime;
    function updateReading(boot: boolean = False): boolean;
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
    procedure UpdateOffRangePanel(const Value: single);
    procedure DisplayLowRange;
    procedure DisplayHighRange;
    procedure FinalizeUpdate;
    procedure UpdateFloatingWindow;
    procedure UpdateUIColors;
    function GetTextColorForBackground(const BgColor: TColor;
      const DarkenFactor: double = 0.5; const LightenFactor: double = 0.3): TColor;
    function GetAdjustedColorForBackground(const BaseColor: TColor;
      const BgColor: TColor; const DarkenFactor: double = 0.6;
      const LightenFactor: double = 0.4; const PreferLighter: boolean = False): TColor;

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
  public
    firstboot: boolean;
    procedure AppExceptionHandler(Sender: TObject; {%H-}E: Exception);
    procedure onGH({%H-}Sender: TObject);
    function lastReading: BGReading;
    function tryLastReading(out bg: BGReading): boolean;
  end;


  {$ifdef DARWIN}
function CFStringCreateWithUTF8String(const utf8Str: PAnsiChar): CFStringRef; external name '_CFStringCreateWithUTF8String';
  {$endif}

var
  customTitlebar: boolean = True;
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
  DOT_OFFSET_RANGE: integer = 3; // Fine-tune vertical alignment of threshold lines with dots
  highAlerted: boolean = False; // A high alert is active
  lowAlerted: boolean = False; // A low alert is active
  perfectTriggered: boolean = False; // A perfect reading is active
  PaintRange: boolean = True;
  PaintRangeCGMRange: boolean = True; // Show cgmRangeLo/cgmRangeHi inner threshold lines
  PaintRangeLines: boolean = False;
  // Show threshold lines (if false, only filled areas are drawn)
  {$ifdef darwin}
MacAppDelegate: TMyAppDelegate;
upMenu: TMenuItem;
  {$endif}

var
  last_popup: TDateTime = 0;
  bg_alert: boolean = False;
  // If the BG is high/low since before, so we don't spam notifications
  placed: boolean = False; // If the window has been placed at setup

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

  privacyMode: boolean = False;

  // Handle dragging on window
  DraggingWin: boolean;
  PX, PY: integer;

  {$ifdef X_LINUXBSD}
IsRaspberry: boolean;
  {$endif}

implementation

{$R *.lfm}

{$I ../../inc/tfuncs.inc}

{$ifdef TrndiExt}
// Converts the current reading to system-default, mgdl and mmol
function getBGResults: JSValueRaw;
var
  bgnow: BGReading;
  mmol, curr: single;
  mgdl: integer;
begin
  bgnow := fBG.lastReading;
  curr := bgnow.convert(un);
  mmol := bgnow.convert(BGUnit.mmol);
  mgdl := round(bgnow.convert(BGUnit.mgdl));
  result := TTrndiExtEngine.Instance.CreateJSArray([curr, mgdl, mmol]);
end;

// Helper function that calls a JS function with additional parameters first
// and BG readings appended at the end: functionInJS(param1, param2, bgreadings)
function callFuncWithBGReadings(const funcName: string; const additionalParams: array of const; out exists: boolean): string;
var
  readings: JSValueRaw;
  allParams, tempParams: array of JSValueRaw;
  i: integer;
begin
  result := '';
  exists := false;
  
  if not Assigned(TTrndiExtEngine.Instance) then 
    Exit;
    
  readings := getBGResults;
  try
    // Build array with additional params first, then BG readings
    SetLength(allParams, Length(additionalParams) + 1);
    
    // Convert additional params to JSValueRaw using the refactored helper
    if Length(additionalParams) > 0 then
    begin
      // Create a temporary array with exact size for the helper function
      SetLength(tempParams, Length(additionalParams));
      ConvertVarRecsToJSValueRaw(additionalParams, tempParams);
      // Copy converted values to the first part of allParams
      for i := 0 to High(additionalParams) do
        allParams[i] := tempParams[i];
    end;
    
    // Add BG readings at the end
    allParams[High(allParams)] := readings;
    
    result := callFuncRaw(funcName, allParams, exists, false);
  except
    on E: Exception do
    begin
      exists := false;
      result := '';
    end;
  end;
end;

// Overload without exists parameter for simpler usage
function callFuncWithBGReadings(const funcName: string; const additionalParams: array of const): string;
var
  exists: boolean;
begin
  result := callFuncWithBGReadings(funcName, additionalParams, exists);
end;
{$endif}

// Returns vertical offset needed to bring all trend dots fully inside their parent.
// Sign convention: negative = dots are above (need to move down); positive = dots are below (need to move up).
function TfBG.dotsInView: integer;
var
  x: TPaintBox;
  bottom, overflow: integer;
  adjustSide: TTrndiBool;
const
  Tol = 5; // pixel tolerance to ignore tiny rounding differences
begin
  Result := 0;
  adjustSide := tbUnknown;
  // We can have both hi and lows; lock to first side encountered

  for x in TrendDots do
  begin
    // Ignore dots not currently visible
    if not x.Visible then
      Continue;
    // Use the actual control size, not font metrics, to determine visibility
    bottom := x.Top + x.Height;

    // Out-of-top (negative top)
    if (x.Top < -Tol) then
    begin
      if adjustSide in [tbTrue, tbUnknown] then
      begin
        // Pick the smallest (most negative) top
        Result := Min(Result, x.Top);
        adjustSide := tbTrue;
      end;
    end
    // Out-of-bottom
    else if (bottom > x.Parent.ClientHeight + Tol) then
    begin
      if adjustSide in [tbFalse, tbUnknown] then
      begin
        // Compute actual overflow beyond tolerance to avoid false positives
        overflow := (bottom - (x.Parent.ClientHeight + Tol));
        // Pick the largest overflow
        Result := Max(Result, overflow);
        adjustSide := tbFalse;
      end;
    end;
  end;

end;

procedure TfBG.onGH(Sender: TObject);
begin
  OpenURL('https://github.com/slicke/trndi');
end;

{$IFDEF DARWIN}

procedure TMyAppDelegate.miSettingsMacClick(sender: id);
begin
  (Application.MainForm as TfBG).miSettingsClick(nil);
end;

function TMyAppDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
var
  dockMenu: NSMenu;
  menuItem: NSMenuItem;
  s: string;
  cfStr: CFStringRef;
begin
  // Create a custom dock menu
  dockMenu := NSMenu.alloc.initWithTitle(NSSTR('Trndi'));

  // Add items to the menu
  s := TrimLeftSet((Application.MainForm as TfBG).miSettings.Caption, ['&', ' ']);

  // Create a CFStringRef directly from the UTF-8 string
  cfStr := CFStringCreateWithCString(nil, PChar(s), kCFStringEncodingUTF8);

  // Initialize the menu item with the CFStringRef
  menuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(
    NSString(cfStr), sel_registerName('miSettings:'), NSSTR(''));

  dockMenu.addItem(menuItem);
  menuItem.release;

  // Release the CFStringRef
  CFRelease(cfStr);

  Result := dockMenu;
end;

function TMyAppDelegate.applicationShouldHandleReopen_hasVisibleWindows(sender: NSApplication; hasVisibleWindows: Boolean): Boolean;
begin
  // Show main form when dock icon is clicked
  Application.MainForm.Show;
  Application.MainForm.BringToFront;
  Result := True;

end;
{$ENDIF}

procedure TfBG.AppExceptionHandler(Sender: TObject; E: Exception);
begin
  // Handle exceptions during shutdown gracefully
end;

procedure ShowMessage(const str: string);
begin
  UXMessage(uxdOnForm, sSuccTitle, str, uxmtInformation, fBG);
end;

procedure ShowMessage(const title, str: string);
begin
  UXMessage(uxdOnForm, title, str, uxmtInformation, fBG);
end;

procedure TfBG.placeForm;
{$ifdef DARWIN}
function GetActiveScreen: TMonitor; // Use TMonitor if that's what Screen.Monitors returns
var
  ScreenObject: NSScreen;
  i: Integer;
begin
  Result := nil;
  if Assigned(NSApplication.sharedApplication.mainWindow) then
  begin
    ScreenObject := NSApplication.sharedApplication.mainWindow.screen;
    if Assigned(ScreenObject) then
    begin
      // Attempt to match NSScreen to the TMonitor by comparing frame or unique ID
      for i := 0 to Screen.MonitorCount - 1 do
      begin
        // Add your comparison logic here (e.g., by frame bounds or display ID)
        // Example (pseudo-code):
        // if Screen.Monitors[i].Rect = NSRectToRect(ScreenObject.frame) then
        //   Result := Screen.Monitors[i];
      end;
    end;
  end;
end;
var
activemonitor: TMonitor;
{$endif}
var
  posValue: integer;
begin
  if native.GetBoolSetting('size.main') then
  begin
    Width := native.GetIntSetting('size.last.width', Width);
    Height := native.GetIntSetting('size.last.height', Height);
  end;
  // Hämta och validera position
  posValue := native.GetIntSetting('position.main', Ord(tpoCenter));

  // Validera positionstyp
  if not ((posValue >= Ord(Low(TrndiPos))) and (posValue <= Ord(High(TrndiPos)))) then
    posValue := Ord(tpoCenter);

  // Hantera positionering
  case TrndiPos(posValue) of
    tpoCenter:
    begin
      Left := Screen.WorkAreaLeft + (Screen.WorkAreaWidth - Width) div 2;
      Top := Screen.WorkAreaTop + (Screen.WorkAreaHeight - Height) div 2;
    end;

    tpoBottomLeft:
    begin
      Left := 20;
      Top := Screen.WorkAreaRect.Bottom - Height - 200;
    end;

    tpoBottomRight:
    begin
      Left := Screen.WorkAreaRect.Right - Width - 20;
      Top := Screen.WorkAreaRect.Bottom - Height - 200;
    end;

    tpoTopRight:
    begin
      Left := Screen.WorkAreaRect.Right - Width - 20;
      Top := 200;
    end;

    tpoCustom:
    begin
      Left := native.GetIntSetting('position.last.left', 10);
      Top := native.GetIntSetting('position.last.top', 10);
    end;
  end;

  if native.GetBoolSetting('main.clock') then
    miClock.OnClick(self);

  if native.GetBoolSetting('main.announce') then
    miAnnounce.OnClick(self);

  {$ifdef DARWIN}
  ActiveMonitor := GetActiveScreen;
  if Assigned(ActiveMonitor) then
  begin
    Left := ActiveMonitor.BoundsRect.Left + (ActiveMonitor.WorkAreaRect.Width - Width) div 2;
    Top := ActiveMonitor.BoundsRect.Top + (ActiveMonitor.WorkAreaRect.Height - Height) div 2;
    Exit;
  end;
  {$endif}
end;

{$ifdef TrndiExt}
// Load extension files
procedure TfBG.LoadExtensions;

var
  exts: TStringList;
  s, extdir: string;
begin
  TTrndiExtEngine.Instance;
  // Creates the class, if it's not already
  jsFuncs := TJSfuncs.Create(api);
  // This is an Object, not a class!
  extdir := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
  // Find extensions folder

  ForceDirectoriesUTF8(extdir);
  // Create the directory if it doesn't exist
  exts := FindAllFiles(extdir, '*.js', false);
  // Find .js files

  fSplash.lInfo.Caption := 'Initializing extensions backend..';
  with TTrndiExtEngine.Instance do
  begin
    addClassFunction('getLocale', ExtFunction(@JSLocale), 0);
    addClassFunction('uxProp', ExtFunction(@JSUX), 3);
    addClassFunction('setBadgeSize', ExtFunction(@JSBADGE), -1);
    addClassFunction('setDotSize', ExtFunction(@JSDotSize), -1);
    addClassFunction('setDotAdjust', ExtFunction(@JSDotAdjust), -1);
    addClassFunction('getUnit', ExtFunction(@JSUnit), 0);
    addClassFunction('setLevelColor', ExtFunction(@JSLevelColor), -1);
    addClassFunction('setTimeAndRange', ExtFunction(@JSTimeRange), 2);
    addClassFunction('playSound', ExtFunction(@JSPlay), 1);
    addClassFunction('sayText', ExtFunction(@JSSay), 1);
    addClassFunction('getCurrentUser', ExtFunction(@JSActiveUser), 0);
    addClassFunction('getCurrentNickname', ExtFunction(@JSActiveUserNick), 0);
    addClassFunction('setOverrideThresholdMinutes', ExtFunction(@JSvar_DATA_FRESHNESS_THRESHOLD_MINUTES), 1);
    addClassFunction('setClockInterval', ExtFunction(@JSvar_ClockInterval), 1);

    // Add the UX modification function, as declared in this file
    for s in exts do begin
       fSplash.lInfo.Caption := RS_SPLASH_LOADING + s;
      // Run all found files
      ExecuteFile(s);
    end;
    exts.Free;
  end;
end;
{$endif}

// Apply a procedure to all trend points; also provides an index
procedure TfBG.actOnTrend(proc: TTrendProcLoop);
var
  ix: integer;
  ls: array[1..10] of TPaintBox;
begin
  ls := TrendDots; // Directly use the TrendDots array
  for ix := 1 to NUM_DOTS do
  begin
    proc(ls[ix], NUM_DOTS, ix, ls);
    ls[ix].Repaint;
  end;
  // Run the procedure on the given label
end;

// Apply a procedure to all trend points
procedure TfBG.actOnTrend(proc: TTrendProc);
var
  ix: integer;
  ls: array[1..10] of TPaintBox;
begin
  ls := TrendDots; // Directly use the TrendDots array
  for ix := 1 to NUM_DOTS do
  begin
    proc(ls[ix], NUM_DOTS, ix);
    ls[ix].Repaint;
  end;
end;

procedure tfBG.SetLang;
var
  lang: string;
begin
  lang := native.GetSetting('locale', '');
  if (lang = 'auto') or (lang = '') then
    lang := native.GetOSLanguage;
  applocale := lang;
  Application.ProcessMessages;

  SetDefaultLang(lang, getLangPath);
end;

{$ifdef X_LINUXBSD}
{$endif}

{$ifdef DARWIN}
procedure TfBG.InitializePlatformMenus;
var
  MainMenu: TMainMenu;
  AppMenu,
  forceMenu,
  SettingsMenu,
  HelpMenu,
  GithubMenu: TMenuItem;
begin
  MacAppDelegate := TMyAppDelegate.alloc.init;
  NSApp.setDelegate(NSObject(MacAppDelegate));

  Application.Title := 'Trndi';
  MainMenu := TMainMenu.Create(self);
  fBg.Menu := MainMenu;
  AppMenu := TMenuItem.Create(Self); // Application menu
  AppMenu.Caption := #$EF#$A3#$BF;   // Unicode Apple logo char
  MainMenu.Items.Insert(0, AppMenu);
  SettingsMenu := TMenuitem.Create(self);
  settingsmenu.Caption := miSettings.Caption;
  settingsmenu.OnClick := misettings.OnClick;
  AppMenu.Insert(0, SettingsMenu);

  forcemenu := TMenuItem.Create(self);
  forcemenu.Caption := miForce.caption;
  forcemenu.onclick := miForce.OnClick;
  AppMenu.Insert(1, forceMenu);

  helpmenu := TMenuItem.Create(self);
  helpmenu.Caption := 'Help';
  MainMenu.Items.Insert(1, helpMenu);

  upmenu := TMenuItem.Create(self);
  upmenu.Caption := mirefresh.Caption;
  upmenu.Enabled := false;

  githubmenu := TMenuItem.Create(self);
  githubmenu.Caption := RS_TRNDI_GIHUB;
  githubmenu.onclick := @onGH;
  helpMenu.Insert(0, githubMenu);

  helpMenu.Insert(0, upMenu);
end;
{$endif}

procedure TfBG.InitializeUIComponents;
var
  i: integer;
  s: string;
begin
  // Set dots
  DOT_GRAPH := native.GetWideCharSetting('font.dot', System.widechar($2B24));
  DOT_FRESH := native.GetWideCharSetting('font.dot_fresh', System.widechar($2600));

  // Load fonts
  s := native.GetSetting('font.val', 'default');
  if s <> 'default' then
    lVal.Font.Name := s;
  s := native.GetSetting('font.arrow', 'default');
  if s <> 'default' then
    lArrow.Font.Name := s;

  s := native.GetSetting('font.ago', 'default');
  if s <> 'default' then
  begin
    lAgo.Font.Name := s;
    lTir.Font.Name := s;
  end;

  // Sensitive data
  DATA_FRESHNESS_THRESHOLD_MINUTES :=
    native.GetIntSetting('system.fresh_threshold', DATA_FRESHNESS_THRESHOLD_MINUTES);

  // Check graph
  for i := 1 to NUM_DOTS do
  begin
    s := 'lDot' + IntToStr(i);
    TrendDots[i] := FindComponent(s) as TPaintBox;
    if not Assigned(TrendDots[i]) then
      ShowMessage(Format('Label %s is missing!', [s]))
    else
      LogMessage(Format('Label %s assigned to TrendDots[%d].', [s, i]));
  end;

  // Check touch screen
  HasTouch := native.HasTouchScreen(HasMultiTouch);
  if HasMultiTouch then
    touchHelper := TTouchDetector.Create;

  miATouch.Checked := hastouch;

  actOnTrend(@initDot);

  //--- Colors
  bg_color_ok := native.GetColorSetting('ux.bg_color_ok', bg_color_ok);
  bg_color_hi := native.GetColorSetting('ux.bg_color_hi', bg_color_hi);
  bg_color_lo := native.GetColorSetting('ux.bg_color_lo', bg_color_lo);

  bg_color_ok_txt := native.GetColorSetting('ux.bg_color_ok_txt', bg_color_ok_txt);
  bg_color_hi_txt := native.GetColorSetting('ux.bg_color_hi_txt', bg_color_hi_txt);
  bg_color_lo_txt := native.GetColorSetting('ux.bg_color_lo_txt', bg_color_lo_txt);

  bg_rel_color_hi := native.GetColorSetting('ux.bg_rel_color_hi', bg_rel_color_hi);
  bg_rel_color_lo := native.GetColorSetting('ux.bg_rel_color_lo', bg_rel_color_lo);

  bg_rel_color_lo_txt := native.GetColorSetting('ux.bg_rel_color_lo_txt',
    bg_rel_color_lo_txt);
  bg_rel_color_hi_txt := native.GetColorSetting('ux.bg_rel_color_hi_txt',
    bg_rel_color_hi_txt);

  // Color title bar (on Windows?)
  titlecolor := native.GetBoolSetting('ux.title_color', True);
end;

procedure TfBG.InitializeSplashScreen;
begin
  fSplash := TfSplash.Create(nil);
  if IsProblematicWM then // It might hide dialogs behind the splash screen
    if not IsSemiProblematicWM then // Gnome etc kind of works
      with fsplash do
      begin
        fSplash.Image1.Hide;
        lSplashWarn.hide;
        linfo.Caption := 'Trndi is loading...';
        linfo.top := 0;
        linfo.left := 400;
        Height := linfo.canvas.TextHeight('Pq') + 5;

        label1.AutoSize := True;
        label1.Caption := 'Trndi | You need to accept the license agreement! ';
        label1.top := 0;
        label1.left := 0;
        label1.font := linfo.font;
        label1.font.color := clWhite;
        Application.ProcessMessages;
      end;
  FStoredWindowInfo.Initialized := False;
  fSplash.Image1.Picture.Icon := Application.Icon;
  fSplash.lInfo.Caption := '';
  fSplash.lInfo.Font.Color := fSplash.lSplashWarn.Font.color;
  {$ifdef X_WIN}
    fSplash.ShowInTaskBar := stAlways;
  {$endif}
  fSplash.Show;
end;

procedure TfBG.LoadUserProfile;
var
  i: integer;
  s: string;
begin
  if username <> '' then
  begin
    with TStringList.Create do
    begin
      AddCommaText(username);
      i := ExtList(uxdAuto, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE,
        RS_MULTIUSER_BOX, ToStringArray, True);

      if (i > -1) and (strings[i] <> '') then
      begin
        username := strings[i];
        native.configUser := username;
        s := native.GetSettingEx('user.nick', username);

        fbg.Caption := Format(RS_USER_CAPTION, [s, fBG.Caption]);
        multinick := s;
      end
      else
      begin
        username := '';
        s := native.GetSettingEx('user.nick', RS_DEFAULT_ACCOUNT);

        multinick := s;
        fbg.Caption := Format(RS_USER_CAPTION, [s, fBG.Caption]);
      end;
      Free;
    end;// Load possible other users
    multi := True;
    pnMultiUser.Color := native.GetColorSetting('user.color', clBlack);
    if pnMultiUser.Color <> clBlack then
    begin
      pnMultiUser.Visible := native.GetRootSetting('users.colorbox', 'true') = 'true';
      customTitlebar := setColorMode;
      // Set the custom title bar value depending if the panel is showing
    end;
  end
  else
    multi := False;
end;

procedure TfBG.CheckAndAcceptLicense;
const
  license = '⚠️ IMPORTANT MEDICAL WARNING ⚠️'#10#13 + #10 +
    'This app is NOT a medical device.'#10 +
    '• Do NOT make medical decisions based on this data'#10 +
    '• Data may be WRONG, delayed, or unavailable'#10 +
    '• Always verify with your official CGM device'#10 +
    '• For emergencies, contact medical professionals'#10 + #10 +
    'By continuing, you acknowledge that:'#10 +
    '• You use this app at your own risk'#10 +
    '• The developers have NO LIABILITY'#10 +
    '• You have read and agree to the full terms';
var
  i: integer;
begin
  if native.GetBoolSetting('license.250608') <> True then
    while i <> mrYes do
    begin
      i := ExtMsg(uxdAuto, 'License', 'You must accept the full terms conditions',
        'Do you agree to the terms and full license?', license,
        uxclWhite, uxclRed, [mbYes, mbCancel, mbUxRead], uxmtCustom, 5);
      if i = mrYes then
        native.SetBoolSetting('license.250608', True)
      else if i = mrCancel then
      begin
        Application.Terminate;
        Exit;
      end
      else
        OpenURL('https://github.com/slicke/trndi/blob/main/LICENSE.md');
    end;
end;

function TfBG.InitializeAPI: boolean;
var
  apiTarget, apiCreds: string;
begin
  Result := True;

  apiTarget := native.GetSetting('remote.target');
  apiCreds := native.GetSetting('remote.creds');

  case native.GetSetting('remote.type') of
    'NightScout':
      api := NightScout.Create(apiTarget, apiCreds, '');
    'NightScout v3':
      api := NightScout3.Create(apiTarget, apiCreds, '');
    'Dexcom (USA)':
      api := Dexcom.Create(apiTarget, apiCreds, 'usa');
    'Dexcom (Outside USA)':
      api := Dexcom.Create(apiTarget, apiCreds, 'world');
    'xDrip':
      api := xDrip.Create(apiTarget, apiCreds, '');
      {$ifdef DEBUG}
    '* Debug Backend *':
      api := DebugAPI.Create(apiTarget, apiCreds, '');
    '* Debug Missing Backend *':
      api := DebugMissingAPI.Create(apiTarget, apiCreds, '');
    '* Debug Perfect Backend *':
      api := DebugPerfectAPI.Create(apiTarget, apiCreds, '');
    '* Debug Custom Backend *':
      api := DebugCustomAPI.Create(apiTarget, apiCreds, '');
   '* Debug Edge Backend *':
      api := DebugEdgeAPI.Create(apiTarget, apiCreds, '');
    {$endif}
    else
      Result := False;
  end;
end;

{$ifdef X_LINUXBSD}
procedure TfBG.InitializeLinuxPlatform;
var
  x, s: string;
begin
  x := scanLinuxDistro(['fedora','ubuntu','debian']);
  case x of
  'fedora': s := 'Poppins';
  'ubuntu': s := 'Sans';
  else
    s := 'default';
  end;

  IsRaspberry := false;
  if x = 'debian' then
    IsRaspberry := FileExists('/etc/rpi-issue');

  fBG.Font.Name := s;

  {$ifndef LCLQt6}
     Showmessage('This release of Trndi was compiled for a non-supported platform ("widgetset")'#10'Performance might be bad and features might not work as intended!'#10#10'Please download the official release (Qt6) from github.com/slicke/trndi');
  {$endif}
  isWSL := TrndiNative.DetectWSL.IsWSL;
  if isWSL then begin
     Showmessage('Windows Linux Subsystem (WSL) detected. Due to limitations in WSL, graphic issues may occur. Commonly, windows will appear at random positions an not where expected!');
  end;
end;
{$endif}

// Initialize the TrendDots array in FormCreate
procedure TfBG.FormCreate(Sender: TObject);

  procedure haltBoot;
  begin
    firstboot := True;
    fSplash.Hide;
    fSplash.Free;
    tMain.Enabled := False;
    lArrow.Caption := '';
    lVal.Caption := RS_SETUP;
    lVal.Cursor := crHandPoint;
    bSettings.Show;
  end;

var
  i: integer;
  guifontName, txtfontName, apiTarget, apiCreds: string;
  fil: boolean;
  userlocale: TFormatSettings;
begin
  firstboot := False;
  // Initialize splash screen first
  InitializeSplashScreen;
  Application.ProcessMessages;
  Application.OnException := @AppExceptionHandler;

  // Start media backend early
  fSplash.lInfo.Caption := 'Starting Media Backend...';
  MediaController := TSystemMediaController.Create(Self);
  MediaController.Initialize;
  Application.ProcessMessages;

  // Font validation
  fil := FontGUIInList(guifontName);
  if not fil then
    ShowMessage(Format(RS_FONT_ERROR, [guifontName]));

  fil := FontTxtInList(txtfontName);
  if not fil then
    ShowMessage(Format(RS_FONT_ERROR, [txtfontName]));

  // Platform-specific menu setup
  {$ifdef DARWIN}
  InitializePlatformMenus;
  {$endif}

  // Initialize native interface
  native := TrndiNative.Create;
  if ssShift in GetKeyShiftState then
    if native.DetectTouchScreen(fil) then
      native.touchOverride := tbFalse
    else
      native.touchOverride := tbTrue;

  setColorMode;

  // Platform-specific initialization
  {$ifdef X_LINUXBSD}
  InitializeLinuxPlatform;
  {$endif}

  // Set border style
  {$ifdef DARWIN}
  BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}

  Application.ProcessMessages;

  // Set fonts for non-Darwin platforms
  {$ifndef DARWIN}
  fBG.font.Name := txtFontName;
  lArrow.Font.Name := guifontName;
  {$endif}

  // Initialize UI components
  InitializeUIComponents;
  Application.ProcessMessages;

  // Force first UI update by initializing cached UI state to sentinel values
  // This ensures ShouldUpdateUI(...) returns true on first valid update
  FLastUIColor := TColor(-1);
  FLastTirColor := TColor(-1);
  FLastUICaption := '<uninitialized>';
  FLastTir := '<uninitialized>';

  // Configuration and user setup
  with native do
  begin
    SetLang;
    username := GetRootSetting('users.names', '');
    LoadUserProfile;

    // Locale setup
    userlocale := DefaultFormatSettings;
    userlocale.DecimalSeparator := GetCharSetting('locale.separator', '.');
    badge_adjust := GetIntSetting('ux.badge_size', 0) / 10;
    native.locale := userlocale;

    // License check
    CheckAndAcceptLicense;
    Application.ProcessMessages;

    // Privacy and unit settings
    privacyMode := GetSetting('ext.privacy', '0') = '1';
    if GetSetting('unit', 'mmol') = 'mmol' then
      un := BGUnit.mmol
    else
      un := BGUnit.mgdl;

    // API setup and validation
    apiTarget := GetSetting('remote.target');
    if apiTarget = '' then
    begin
      tMain.Enabled := False;
      for i := 0 to fBG.ComponentCount - 1 do
        if (fbg.Components[i] is TLabel) and (fbg.Components[i] <> lval) then
          (fbg.Components[i] as TLabel).Caption := '';
      miSettings.Click;
      ShowMessage(RS_FORCE_QUIT_SETUP);
      Application.Terminate;
      Exit;
    end;

    Application.ProcessMessages;
    if not InitializeAPI then
    begin
      haltBoot;
      Exit;
    end;

    Application.ProcessMessages;
    if not api.Connect then
    begin
      haltBoot;
      ShowMessage(api.ErrorMsg);
      miSettings.Click;
      self.Close;
      Exit;
    end;


    // UI preferences
    dotscale := GetIntSetting('ux.dot_scale', 1);
    DOT_ADJUST := GetFloatSetting('ux.dot_adjust', 0);
    // DOT_VISUAL_OFFSET := CalculateDotVisualOffset;  // No longer needed - centering uses half dot height
    miRangeColor.Checked := GetBoolSetting('ux.range_color', True);
    PaintRange := native.GetBoolSetting('ux.paint_range', True);
    PaintRangeLines := native.GetBoolSetting('ux.paint_range_lines', False);
    PaintRangeCGMRange := native.GetBoolSetting('ux.paint_range_cgmrange', False);
    // Extensions
    {$ifdef TrndiExt}
    fSplash.lInfo.Caption := RS_SPLASH_LOADING_INIT;
    LoadExtensions;
    {$endif}

    // Override settings
    if GetIntSetting('override.enabled', 0) = 1 then
    begin
      api.cgmLo := GetIntSetting('override.lo', api.cgmLo);
      api.cgmHi := GetIntSetting('override.hi', api.cgmHi);
      api.cgmRangeLo := GetIntSetting('override.rangelo', api.cgmRangeLo);
      api.cgmRangeHi := GetIntSetting('override.rangehi', api.cgmRangeHi);
    end;
  end;

  // Final initialization and first reading
  Application.ProcessMessages;
  if not updateReading(True) then
  begin
    updateReading; // Second attempt for setup
    showWarningPanel(RS_NO_BOOT_READING);
  end;

  // Cleanup splash screen
  fSplash.Close;
  fSplash.Free;
  tmain.Enabled := True;
  tsetup.Enabled := True;
end;

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
  FShuttingDown := True;

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
      trndi.types.BGHigh: begin
        native.speak('High');
      end;
      trndi.types.BGLOW: begin
        native.speak('Low');
      end;
      trndi.types.BGRange: begin
        native.speak('Good');
      end;
      trndi.types.BGRangeHI: begin
        native.speak('Going high');
      end;
      trndi.types.BGRangeLO: begin
        native.speak('Going low');
      end;
    end;
end;

procedure TfBG.FormKeyPress(Sender: TObject; var Key: char);
begin
  case key of
    #27: begin
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
  L.AutoSize := False;

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
  miATouchYes.Checked := False;
  miATouchNo.Checked := False;
  miATouchAuto.Checked := False;
  (Sender as TMenuItem).Checked := True;

  native.touchOverride := tbUnknown;
end;

procedure TfBG.miADotAdjustClick(Sender: TObject);
var
  mr: TModalResult;
  da: single;
begin
  da := (ExtNumericInput(uxdAuto, 'Dot Adjustment', 'Add dot adjustment',
    'You can enter plus or minus. Plus = down. 0 = neutral', DOT_ADJUST *
    100, False, mr) / 100);

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
  sysver: string;
  {$ifdef Linux}
  s,ver: string;
  {$endif}
begin
  {$if defined(LCLWin32)}
    sysver := SysUtils.Win32MajorVersion.tostring + '.' + SysUtils.Win32MinorVersion.tostring + ' - Build ' +  Win32BuildNumber.ToString;
  {$elseif defined(Linux)}
    s := getlinuxdistro(ver);
    sysver := s + ' ' + ver;
  {$endif}

  ShowMessage({$I %FPCTargetOS%} + '(' + {$I %FPCTargetCPU%} + ')' + LineEnding +
    {$if defined(LCLQt6)}
    'QT6 - ' + qtVersion + ' - ' + sysver
    {$elseif defined(LCLGTK2)}
    'GTK2 - '  + sysver
    {$elseif defined(LCLGTK3)}
    'GTK3'
    {$elseif defined(LCLWIN32)}
    'Windows Native - ' + sysver
    {$elseif defined(LCLCocoa)}
    'macOS Native'
    {$else}
    'unsupportd widgetset'
    {$endif}
    + LineEnding + 'Default separator: ' + DefaultFormatSettings.DecimalSeparator

    );

end;

procedure TfBG.miATouchClick(Sender: TObject);
begin

end;

procedure TfBG.miATouchNoClick(Sender: TObject);
begin
  miATouchYes.Checked := False;
  miATouchNo.Checked := False;
  miATouchAuto.Checked := False;
  (Sender as TMenuItem).Checked := True;
  native.touchOverride := tbFalse;

end;

procedure TfBG.miATouchYesClick(Sender: TObject);
begin
  miATouchYes.Checked := False;
  miATouchNo.Checked := False;
  miATouchAuto.Checked := False;
  (Sender as TMenuItem).Checked := True;
  native.touchOverride := tbTrue;
end;

procedure TfBG.miDebugBackendClick(Sender: TObject);
begin
  miDebugBackend.Checked := not miDebugBackend.Checked;
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
    tTouch.Enabled := False; // Dont popup stuff while moving
    // Use the resize timer with a very short interval for smooth panel scaling during drag
    // This prevents too frequent calls while still providing responsive scaling
    if not tResize.Enabled then
    begin
      tResize.Interval := 50; // Very short interval during dragging for responsiveness
      tResize.Enabled := True;
    end;
  end
  else
  begin
    // Don't enable touch timer on general mouse movement - only on actual lVal interaction
    // The timer will be enabled in lValMouseDown when there's actual touch/click on lVal
  end;
end;

// FormCloseQuery event handler - called BEFORE FormClose
procedure TfBG.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {$ifdef TrndiExt}
  // Show immediate user feedback for shutdown process using big lVal text
  lVal.Caption := 'Shutting down extensions...';
  lVal.Visible := True;
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
  CanClose := True;
end;

// FormClose event handler
procedure TfBG.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  posValue: integer;
  {$ifdef Darwin}
    mr: TModalResult;
  {$endif}
begin
  // Prevent recursive shutdown calls
  if FShuttingDown then
    Exit;
  FShuttingDown := True;

  {$ifdef Darwin}
  if not firstboot then
  if self.Showing then
  begin
    mr := UXDialog(uxdAuto, 'Quit or Minimize?', 'Would you like to minimize to the Dock, or close Trndi?',
                   [mbClose, mbUXMinimize, mbCancel]);
    case mr of
      mrClose: CloseAction := caFree;
      mrCancel: 
      begin
        FShuttingDown := False; // Reset flag if user cancels
        Abort;
      end;
      else
      begin
        CloseAction := caHide;
        FShuttingDown := False; // Reset flag for hide
        Exit;
      end;
    end;

  end;
  {$else}

  if not firstboot then
    if UXDialog(uxdAuto, RS_QUIT_CAPTION, RS_QUIT_MSG, [mbYes, mbNo], uxmtOK) = mrNo then
    begin
      FShuttingDown := False; // Reset flag if user cancels
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
    if Assigned(tAgo) then tAgo.Enabled := false;
    if Assigned(tClock) then tClock.Enabled := false;
    if Assigned(tSwap) then tSwap.Enabled := false;
    if Assigned(tResize) then tResize.Enabled := false;
    if Assigned(tMissed) then tMissed.Enabled := false;
    if Assigned(tTouch) then tTouch.Enabled := false;
    if Assigned(tMain) then tMain.Enabled := false;
    
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
    lDiff.Hide;
    lAgo.hide;
    Application.ProcessMessages;
    
    // Now safely shutdown the extension engine
    TTrndiExtEngine.ReleaseInstance;
  except
    // Ignore shutdown errors - the OS will clean up remaining resources
  end;
  {$endif}

  // Let normal form closure process continue with CloseAction := caFree
end;

procedure TfBG.fbReadingsDblClick(Sender: TObject);
begin

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
    begin
      l.top := l.top - da; // da is negative on top so this is valid both ways
    end;

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
    if v < BG_API_MIN then v := BG_API_MIN;
    if v > BG_API_MAX then v := BG_API_MAX;
    Position := Padding + Round((v - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) *
      UsableHeight);
    Result := (clientH - Position) - 1;
  end;

begin
  // Only draw when form has been created and API thresholds available
  if not Assigned(Self) then Exit;

  // Exit early if nothing is enabled
  if not (PaintRange or PaintRangeCGMRange) then Exit;

  cnv := Self.Canvas;
  // Prefer the TrendDots parent client height if available so mapping matches SetPointHeight
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) and
    Assigned(TrendDots[1].Parent) then
    clientH := TrendDots[1].Parent.ClientHeight
  else
    clientH := Self.ClientHeight;

  // Get dot height for centering lines through the middle of dots
  // Calculate using a temporary bitmap to avoid affecting actual dots during paint
  dotHeight := 0;
  if (Length(TrendDots) > 0) and Assigned(TrendDots[1]) then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Canvas.Font.Assign(TrendDots[1].Font);
      bmp.Canvas.Font.Size := (ClientWidth div 24) * dotscale;
      dotHeight := bmp.Canvas.TextHeight(DOT_GRAPH);
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
    cnv.Brush.Color := DarkenColor(fBG.Color, 0.9);
    cnv.Pen.Style := psClear;
    cnv.FillRect(Classes.Rect(0, hiY, Self.ClientWidth, loY));
  end;

  // Fill the area between inner range thresholds with a lightened background color
  if drawRangeLo and drawRangeHi then
  begin
    cnv.Brush.Style := bsSolid;
    cnv.Brush.Color := DarkenColor(fBG.Color, 0.85);
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
    found := False;

    for y := 0 to bmp.Height - 1 do
    begin
      for x := 0 to bmp.Width - 1 do
      begin
        pixelColor := bmp.Canvas.Pixels[x, y];
        // Check if pixel is significantly different from background
        if pixelColor <> bgColor then
        begin
          firstPixelY := y;
          found := True;
          break;
        end;
      end;
      if found then break;
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
  l.Visible := False;
end;

// Shows a dot
procedure TfBG.ShowDot(l: TPaintBox; c, ix: integer);
begin
  l.Visible := True;
end;

// Scales a dot's font size
procedure TfBG.ResizeDot(l: TPaintBox; c, ix: integer);
var
  th, tw, minSize: integer;
begin
  l.AutoSize := False;
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
    begin
      tResize.Enabled := True;
    end
    else if not DraggingWin then
    begin
      // Only restart timer when not dragging to prevent rapid resize interruptions
      tResize.Enabled := False;
      tResize.Enabled := True;
    end;

    if not DraggingWin then
    begin
      lVal.Visible := False;
      lAgo.Visible := False;
      lTir.Visible := False;
    end;

    // Apply alpha control only - rounded corners are handled by pnWarningPaint
    ApplyAlphaControl(pnWarning, 235);
  end;
end;

procedure TfBG.FormShow(Sender: TObject);
begin
  placeForm;
  placed := True;
  lVal.font.Quality := fqCleartype;
end;

procedure TfBG.lAgoClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  if firstboot then exit; // Dont trigger lastReading

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
    FStoredWindowInfo.Initialized := True;
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
      FStoredWindowInfo.Initialized := False;
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
    if IsProblematicWM then
      BorderStyle := bsToolWindow;
    // Restore the borderstyle on Rpi (etc??) as the window wont fill otherwise
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
  begin
    msg := Format(RS_TIR_M, [minTotal, lo, hi, rlo, rhi]);
  end
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
  if firstboot then exit;
  // Acknowledge active high/low alert by user click
  if native <> nil then
  begin
    if (not highAlerted) and (fBG.Color = bg_color_hi) then
      highAlerted := True;
    if (not lowAlerted) and (fBG.Color = bg_color_lo) then
      lowAlerted := True;
    // Stop flashing if any
    native.StopBadgeFlash;
  end;
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
    DraggingWin := True;
    PX := X;
    PY := Y;
    if not hasTouch then
      Exit;
  end;


  if not hasTouch then
    Exit;

  // Handle touch screens
  StartTouch := Now;
  IsTouched := True;
  tTouch.Enabled := True;

end;

// Handle mouse up on lVal
procedure TfBG.lValMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  IsTouched := False;
  tTouch.Enabled := False;
  StartTouch := 0; // Reset the touch start time

  if DraggingWin then
  begin
    DraggingWin := False;
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
      fFloat.pnMultiUser.Visible := True;
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
  begin
    // Cache has expired, normal update is fine
    updateReading;
  end;
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
  flags: NativeUInt;
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
          Self.Visible := True;
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
      end
      else
      begin
        fsLo.Value := GetIntSetting('override.lo', api.cgmLo);
        fsHi.Value := GetIntSetting('override.hi', api.cgmHi);
      end;

      cbTIR.Checked := native.GetBoolSetting('range.custom', True);

      cbOffBar.Checked := native.GetBoolSetting('ux.off_bar', False);
      cbPaintHiLo.Checked := native.GetBoolSetting('ux.paint_range', True);
      cbPaintLines.Checked := native.GetBoolSetting('ux.paint_range_lines', False);
      cbPaintHiLoRange.Checked :=
        native.GetBoolSetting('ux.paint_range_cgmrange', False);
      edCommaSep.Text := GetCharSetting('locale.separator', '.');
      edTray.Value := GetIntSetting('ux.badge_size', 0);

      if GetSetting('unit', 'mmol') = 'mmol' then
        rbUnitClick(Self);

      cbCust.Checked := GetIntSetting('override.enabled', 0) = 1;
      edMusicHigh.Text := GetSetting('media.url_high', '');
      edMusicLow.Text := GetSetting('media.url_low', '');
      edMusicPerfect.Text := GetSetting('media.url_perfect', '');
      cbMusicPause.Checked := GetBoolSetting('media.pause');
      fsHi.Enabled := cbCust.Checked;
      fsLo.Enabled := cbCust.Checked;

      // User customizations
      s := GetRootSetting('users.names', '');
      lbUsers.Clear;
      lbUsers.Items.CommaText := s;
      if lbUsers.Items.Count < 1 then
        lbUsers.Enabled := False;

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
      cbFlashHi.Checked := native.getBoolSetting('alerts.flash.high', False);
      cbFlashLow.Checked := native.getBoolSetting('alerts.flash.low', False);
      cbFlashPerfect.Checked := native.getBoolSetting('alerts.flash.perfect', False);
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
        if GetSetting('locale', '') = s then
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

      cbTitleColor.Checked := native.GetBoolSetting('ux.title_color', True);
    end;
  end;

  procedure SetupExtensions(f: TfConf);
  begin
    with f, native do
    begin
      {$ifdef TrndiExt}
      eExt.Text := GetAppConfigDirUTF8(False, True) + 'extensions' + DirectorySeparator;
    {$else}
      eExt.Text := '- ' + RS_noPlugins + ' -';
      eExt.Enabled := False;
    {$endif}
      cbPrivacy.Checked := GetSetting('ext.privacy', '0') = '1';
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
      SetSetting('ext.privacy', IfThen(cbPrivacy.Checked, '1', '0'));

      SetSetting('system.fresh_threshold', IntToStr(spTHRESHOLD.Value));

      // Save unit-specific settings
      if rbUnit.ItemIndex = 0 then
      begin // mmol
        SetSetting('override.lo', Round(fsLo.Value * TrndiAPI.toMGDL).ToString);
        SetSetting('override.hi', Round(fsHi.Value * TrndiAPI.toMGDL).ToString);
      end
      else
      begin
        SetSetting('override.lo', Round(fsLo.Value).ToString);
        SetSetting('override.hi', Round(fsHi.Value).ToString);
      end;

      native.SetBoolSetting('range.custom', cbTIR.Checked);
      native.SetBoolSetting('ux.off_bar', cbOffBar.Checked);
      native.SetBoolSetting('ux.paint_range', cbPaintHiLo.Checked);
      native.SetBoolSetting('ux.paint_range_lines', cbPaintLines.Checked);
      native.SetBoolSetting('ux.paint_range_cgmrange', cbPaintHiLoRange.Checked);
      native.SetSetting('locale.separator', edCommaSep.Text);
      native.SetSetting('ux.badge_size', edTray.Value.ToString);

      SetSetting('override.enabled', IfThen(cbCust.Checked, '1', '0'));
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

    // Save settings when dialog closes
    SaveUserSettings(fConf);
    ShowMessage(RS_RESTART_APPLY);

    if firstboot then exit;
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
  if TextStr = '' then Exit;
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
  function SafeQtStyle(Handle: QWidgetH; const Style: string): Boolean;
  var
    ws: WideString;
  begin
    Result := False;

    // Safey checks for Qt6
    if Handle = nil then Exit;
    if PtrUInt(Handle) < $1000 then Exit;

    try
      // Test a simple operation
      if not QWidget_isEnabled(Handle) and QWidget_isEnabled(Handle) then
        Exit; // If false, the handle is not OK

      ws := UTF8Decode(Style);
      QWidget_setStyleSheet(Handle, PWideString(@ws));
      Result := True;

    except
      Result := False;
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
  miBorders.Checked := self.BorderStyle = bsNone;
  miATouch.Checked := HasTouch;
  {$ifdef DEBUG}
    miDebugBackend.Visible := true;
  {$endif}

  miDotNormal.Checked := False;
  miDotBig.Checked := False;
  miDotHuge.Checked := False;
  miDotVal.Checked := False;

  case dotscale of
    1: miDotNormal.Checked := True;
    2: miDotBig.Checked := True;
    3: miDotHuge.Checked := True;
    else
      miDotVal.Checked := True;
  end;

  if (Sender as TPopupMenu).PopupComponent is TPaintBox then
  begin
    tpb := (Sender as TPopupMenu).PopupComponent as TPaintBox;
    H := tpb.Tag div 100;
    M := tpb.Tag mod 100;

    miDotVal.Visible := True;
    miDotVal.Caption := Format(sReadingHere, [tpb.hint, H, M]);
  end
  else
    miDotVal.Visible := False;
  {$ifdef LCLQt6}
  Exit; // This crashes!
  if pmSettings.Tag <> 1 then
  begin
    if not SafeQtStyle(QWidgetH(pmSettings.Handle),
        'QMenu { font-size: 16pt; font-weight: bold; }' + LineEnding +
        'QMenu::item { padding: 8px 12px; }' + LineEnding +
        'QMenu::item:selected { background-color: highlight; }') then
    begin
      {$IFDEF DEBUG}
      ShowMessage('Qt styling failed, using fallback menu');
      {$ENDIF}
    end;

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
  begin
    if multinick <> RS_DEFAULT_ACCOUNT then
      ShowMessage(Format(RS_MULTINAME_DEF_NAMED, [multinick]))
    else
      ShowMessage(RS_MULTINAME_DEF);
  end;
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
  if firstboot then exit; // Dont run on first boot

  if sizeof(bgs) < 1 then
  begin
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO;
  end
  else
  begin
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
end;

procedure TfBG.tClockTimer(Sender: TObject);
var
  s: string;
  {$ifdef TrndiExt}
    ex: boolean; // If the function exists
  {$endif}
begin
  tClock.Enabled := False;
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
    lArrow.Visible := False;
  end
  else
  begin
    lval.Caption := lval.hint;
    tClock.Interval := clockInterval;
    lArrow.Visible := True;
  end;
  tClock.Enabled := True;
end;

procedure TfBG.tEdgesTimer(Sender: TObject);
begin

end;

procedure TfBG.tInitTimer(Sender: TObject);
begin
  tInit.Enabled := False;
  self.Width := self.Width + 1;
  // Force a natural resize call, calling onResize doesnt work
end;

procedure TfBG.tResizeTimer(Sender: TObject);
begin
  tResize.Enabled := False;

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
    miRangeColor.Enabled := False;
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
  //  lAgo.Top := 1 + IfThen(pnOffRange.Visible, pnOffRange.Height, 3);
  lAgo.Height := ClientHeight div 9;
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
  lTir.AutoSize := False;
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
  lArrow.OptimalFill := True;

  pnMultiUser.Width := min(max(clientwidth div 30, 5), 50);
  pnMultiUser.Height := clientheight;
  pnMultiUser.top := 0;
  pnMultiUser.left := 0;
end;

procedure TfBG.UpdateTrendDots;
var
  Dot: TPaintBox;
  Value: single; // Parsed from hint (user unit), then normalized to mmol/L
  ok: boolean;
begin
  for Dot in TrendDots do
  begin
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

      Dot.Visible := True;
    end
    else
    begin
      // Hide labels without valid Hint values
      Dot.Visible := False;
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
    ALabel.Visible := True;

  if ALabel.Caption = '' then
    Exit; // No text

  // Check size
  if (ALabel.Width <= 0) or (ALabel.Height <= 0) then
  begin
    ALabel.Width := 250;
    ALabel.Height := 250;
  end;

  // Format
  ALabel.AutoSize := False;
  ALabel.WordWrap := False;
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
    begin
      High := Mid - 1;
    end;
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
  if firstboot then exit;

  d := lastReading.date; // Last reading time

  min := MilliSecondsBetween(Now, d) div 60000;  // Minutes since last
  sec := (MilliSecondsBetween(Now, d) mod 60000) div 1000; // Seconds since last

  lDiff.Caption := Format(RS_OUTDATED_TIME, [FormatDateTime('H:mm', d), min, sec]);
end;

procedure TfBG.tSetupTimer(Sender: TObject);
begin
  tSetup.Enabled := False;
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
    lArrow.Visible := True;
    Exit;
  end;

  tSwap.Enabled := False;
  s := lval.Caption;

  if s.IndexOf(':') > 0 then // Clock showing
    UpdateUIColors   // Resets standard coloring
  else if lVal.font.color <> lArrow.font.color then
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
  tSwap.Enabled := True;
end;

// Handle a touch screen's long touch
procedure TfBG.tTouchTimer(Sender: TObject);
var
  p: TPoint;
  touchDuration: integer;
begin
  tTouch.Enabled := False;
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
  if firstboot then exit;
  b := lastReading;

  // Update value label
  if not privacyMode then
  begin
    if b.val > 400 then
      lVal.Caption := RS_HIGH
    else if b.val < 40 then
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
  if firstboot then exit;

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
    tMissed.Enabled := True;          // Keep the missed timer running
    lArrow.Caption := '';             // Dont show arrow when not fresh
    native.setBadge('--', clBlack, badge_width + badge_adjust,
      badge_font + round(badge_adjust * 10)); // Update system/taskbar badge
    native.StopBadgeFlash;            // Stop any flashing when stale
  end
  else
  begin
    tMissed.Enabled := False;         // Data is fresh; stop missed timer
    bg_alert := True;                 // Allow alerts again
  end;
end;

procedure TfBG.SetNextUpdateTimer(const LastReadingTime: TDateTime);
var
  i: int64;
begin
  if firstboot then exit;
  tMain.Enabled := False;

  i := SecondsBetween(LastReadingTime, now); // Seconds from last
  i := min(BG_REFRESH, BG_REFRESH - (i * 1000));
  // 5 min or less if there's a recent reading
  i := max(120000, i); // Don't allow too small refresh time (min 2 minutes)

  tMain.Interval := i + 15000; // Add 15 secs to allow sync
  tMain.Enabled := True;

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
  if native.GetBoolSetting('range.custom', True) then
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
  lTir.Visible := True; // Ensure TIR label is visible after calculation
end;

function TfBG.updateReading(boot: boolean = False): boolean;
begin
  Result := False;
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
    Result := True;

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
  tAgo.Enabled := True;
  tAgo.OnTimer(self);
  Self.OnResize(lVal);

  // Update floating window if assigned
  UpdateFloatingWindow;

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
  else if b.val <= api.cgmLo then
    HandleLowGlucose(b)
  else
    HandleNormalGlucose(b);

  pnOffReading.Visible := native.GetBoolSetting('ux.off_bar', False);
  case b.level of
    trndi.types.BGHigh: begin
      txt := RS_HIGH;
    end;
    trndi.types.BGLOW: begin
      txt := RS_LOW;
    end;
    trndi.types.BGRange: begin
      txt := '';
      pnOffReading.Visible := False;
    end;
    trndi.types.BGRangeHI: begin
      txt := RS_OFF_HI;
    end;
    trndi.types.BGRangeLO: begin
      txt := RS_OFF_LO;
    end;
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

  url := native.GetSetting('media.url_high', '');
  if url <> '' then
  begin
    highAlerted := True;
    MediaController.PlayTrackFromURL(url);
  end;
  doFlash := native.GetBoolSetting('alerts.flash.high', False);
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

  url := native.GetSetting('media.url_low', '');
  if url <> '' then
  begin
    lowAlerted := True;
    MediaController.PlayTrackFromURL(url);
  end;
  doFlash := native.GetBoolSetting('alerts.flash.low', False);
  if (not lowAlerted) and doFlash then
    native.StartBadgeFlash(lVal.Caption, bg_color_lo, 20000, 400);
end;

procedure TfBG.HandleNormalGlucose(const b: BGReading);
var
  s, url: string;
  i: integer;
  f: single;
  go: boolean = False;
begin
  bg_alert := False;
  SetColorMode(bg_color_ok);
  highAlerted := False;
  lowAlerted := False;
  native.StopBadgeFlash; // cease alerts when normal

  if un = mmol then
  begin
    s := b.format(mmol, BG_MSG_SHORT, BGPrimary);
    if (TryStrToFloat(s, f, native.locale)) and (f = 5.5) then
      go := True
    else
      perfecttriggered := False;
  end
  else
  begin
    s := b.format(mgdl, BG_MSG_SHORT, BGPrimary);
    if (TryStrToInt(s, i)) and (i = 100) then
      go := True
    else
      perfecttriggered := False;
  end;

  if go and (not perfecttriggered) then
  begin
    perfectTriggered := True;

    url := native.GetSetting('media.url_perfect', '');
    if url <> '' then
      MediaController.PlayTrackFromURL(url);
    if native.GetBoolSetting('alerts.flash.perfect', False) then
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
  pnwarning.AutoSize := False;
  pnwarning.Width := ClientWidth - (padding * 2);  // Use padding on both sides
  pnWarning.left := padding;
  pnwarning.top := padding;
  pnWarning.Height := ClientHeight - (padding * 2);  // Use padding on top and bottom

  // Configure the main warning label first
  if Pos(sLineBreak, lMissing.Caption) < 1 then // Ugly solution
    lMissing.Caption := '🕑' + sLineBreak + lMissing.Caption;

  lMissing.AutoSize := False;
  lMissing.left := 5;
  lMissing.top := 5;
  lMissing.Width := pnWarning.Width - 10;
  lMissing.Height := pnWarning.Height - 60; // Leave room for pnWarnLast at bottom
  lMissing.wordwrap := True;
  lMissing.OptimalFill := True;

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
  clearDisplayValues: boolean = False);
var
  i: integer;
begin
  pnWarning.Visible := True;
  pnWarning.Caption := '⚠️ ' + message;

  if clearDisplayValues then
  begin
    if (not TryStrToInt(lVal.Caption[1], i)) or (lArrow.Caption = 'lArrow') then
    begin // Dont show "Setup" or similar on boot
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;
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
  Result := False;

  if api = nil then
    Exit;

  // Performance optimization: use cached readings if recent (unless forced)
  if not ForceRefresh and (SecondsBetween(Now, FLastAPICall) < API_CACHE_SECONDS) and
    (Length(FCachedReadings) > 0) then
  begin
    bgs := FCachedReadings;
    Result := True;
    Exit;
  end;

  {$ifdef DEBUG}
    bgs := api.getReadings(MAX_MIN, MAX_RESULT, '', res);
    if miDebugBackend.Checked then begin
      if Showing then
        if res.IsEmpty then
          slicke.ux.alert.ExtLog(uxdAuto, 
                                 IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'), 
                                 '[empty!]', res)
        else
          slicke.ux.alert.ExtLog(uxdAuto, 
                                 IfThen(ForceRefresh, 'Debug Info (Forced)', 'Debug Info'), 
                                 '', res, uxmtCustom, 10);
     end;
  {$ELSE}
  bgs := api.getReadings(MAX_MIN, MAX_RESULT);
  {$endif}

  // Cache the API call and results
  FLastAPICall := Now;
  SetLength(FCachedReadings, Length(bgs));
  if Length(bgs) > 0 then
    Move(bgs[0], FCachedReadings[0], Length(bgs) * SizeOf(BGReading));

  pnWarning.Visible := False;
  if (Length(bgs) < 1) or (not IsDataFresh) then
  begin
    showWarningPanel(RS_NO_BACKEND, True);
    Exit;
  end;

  // Call the method to place the points
  PlaceTrendDots(bgs);
  Result := True;
end;

function TfBG.FetchAndValidateReadings: boolean;
begin
  Result := DoFetchAndValidateReadings(False); // Use cached data if available
end;

function TfBG.FetchAndValidateReadingsForced: boolean;
begin
  Result := DoFetchAndValidateReadings(True); // Force fresh API call, bypass cache
end;

procedure TfBG.UpdateOffRangePanel(const Value: single);
var
  on: boolean = True;
begin
  if (Value >= api.cgmHi) or (Value <= api.cgmLo) then
  begin
    pnOffRange.Visible := False;
    pnOffRangeBar.Visible := False;
    if Assigned(fFloat) then
    begin
      ffloat.lRangeDown.Visible := False;
      ffloat.lRangeUp.Visible := False;
    end;
  end
  else if Value <= api.cgmRangeLo then
    DisplayLowRange
  else if Value >= api.cgmRangeHi then
    DisplayHighRange
  else
  begin
    pnOffRange.Visible := False;
    pnOffRangeBar.Visible := False;
    on := False;
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


  if TryStrToInt(lTir.hint, r) then
  begin  // Check time in range
    if r < bad_tir then // If the value is under the limit for "bad"
      lTir.Font.color := GetAdjustedColorForBackground(clMaroon,
        fBG.Color, 0.6, 0.4, True)
    else if r >= good_tir then
      lTir.Font.color := GetAdjustedColorForBackground(clGoodGreen,
        fBG.Color, 0.6, 0.4, True);
  end;

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
  const LightenFactor: double = 0.4; const PreferLighter: boolean = False): TColor;
begin
  if PreferLighter then
  begin
    // Tvinga ljusare förgrund
    Result := LightenColor(BaseColor, LightenFactor);
  end
  else
  begin
    if IsLightColor(BgColor) then
      Result := DarkenColor(BaseColor, DarkenFactor)
    else
      Result := LightenColor(BaseColor, LightenFactor);
  end;
end;

procedure TfBG.DisplayLowRange;
begin
  pnOffRange.Color := bg_rel_color_lo;
  pnOffRangeBar.Color := bg_rel_color_lo;
  pnOffRange.Font.Color := bg_rel_color_lo_txt;
  if miRangeColor.Checked then
  begin
    pnOffRange.Visible := False;
    pnOffRangeBar.Visible := False;
  end
  else
  begin
    pnOffRange.Visible := True;
    pnOffRangeBar.Visible := True;
    setColorMode;
  end;
  pnOffRangeBar.Visible := True;
  pnOffRange.Caption := Format('↧ %s ↧', [RS_OFF_LO]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then
    begin
      ffloat.lRangeDown.Visible := True;
    end;
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
    pnOffRange.Visible := False;
    pnOffRangeBar.Visible := False;
  end
  else
  begin
    pnOffRange.Visible := True;
    pnOffRangeBar.Visible := True;
    setColorMode;
  end;
  pnOffRange.Caption := Format('↥ %s ↥', [RS_OFF_HI]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then
    begin
      ffloat.lRangeUp.Visible := True;
    end;
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
  slotIndex, i, labelNumber: integer;
  slotStart, slotEnd: TDateTime;
  found: boolean;
  reading: BGReading;
  l: TPaintbox;
begin
  for slotIndex := 0 to NUM_DOTS - 1 do
  begin
    // Set start and end time for the intervall
    slotEnd := IncMinute(CurrentTime, -INTERVAL_MINUTES * slotIndex);
    slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

    found := False;

    // Sök efter den senaste läsningen inom intervallet
    for i := 0 to High(SortedReadings) do
    begin
      reading := SortedReadings[i];

      if (reading.date <= slotEnd) and (reading.date > slotStart) then
      begin
        found := UpdateLabelForReading(slotIndex, reading);
        Break; // Gå till nästa tidsintervall
      end;
    end;

    // Hide label if no reading
    if not found then
    begin
      labelNumber := NUM_DOTS - slotIndex;
      l := TrendDots[labelNumber];

      if Assigned(l) then
      begin
        l.Visible := False;
        LogMessage(Format('TrendDots[%d] hidden as no reading found in interval.',
          [labelNumber]));
      end;
    end;
  end;
end;

function TfBG.UpdateLabelForReading(SlotIndex: integer;
  const Reading: BGReading): boolean;
var
  labelNumber: integer;
  l: TPaintBox;
  H, M, S, MS: word;
begin
  if firstboot then exit;
  Result := False;

  // Mappa slotIndex till etikettens nummer (0 -> lDot10, 1 -> lDot9, ..., 9 -> lDot1)
  labelNumber := NUM_DOTS - SlotIndex;
  l := TrendDots[labelNumber];

  if Assigned(l) then
  begin
    // Uppdatera etikettens egenskaper baserat på läsningen
    l.Visible := True;
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
    Result := True;
  end;
end;

function TfBG.DetermineColorForReading(const Reading: BGReading): TColor;
begin
  if Reading.val >= api.cgmHi then
    Result := bg_color_hi
  else if Reading.val <= api.cgmLo then
    Result := bg_color_lo
  else
  begin
    Result := bg_color_ok_txt;

    if Reading.val <= api.cgmRangeLo then
      Result := bg_rel_color_lo
    else if Reading.val >= api.cgmRangeHi then
      Result := bg_rel_color_hi;
  end;

  Result := LightenColor(Result, -0.8);
end;

function TfBG.setColorMode: boolean;
begin
  Result := setColorMode(clFuchsia, True);
end;

function TfBG.setColorMode(bg: tColor; const nocolor: boolean = False): boolean;
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
  begin // Safe that black = standard
    if native.SetTitleColor(handle, pnMultiUser.Color,
      IfThen(IsLightColor(pnMultiUser.Color), clBlack, clWhite)) then
      Exit(True);
  end;

  if native.isDarkMode then
    native.setDarkMode
  {$ifdef windows}
(self.Handle)
  {$endif}
  ;

  Result := False;
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

end.
