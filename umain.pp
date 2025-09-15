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

{$I native.inc}

{$ifdef Darwin}
  {$modeswitch objectivec1}
{$endif}


interface

uses
trndi.strings, LCLTranslator, Classes, Menus, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
trndi.api.dexcom, trndi.api.nightscout, trndi.types, math, DateUtils, FileUtil, LclIntf, TypInfo, LResources,
slicke.ux.alert, usplash, Generics.Collections, trndi.funcs,
SystemMediaController,
{$ifdef TrndiExt}
trndi.Ext. Engine, trndi.Ext.jsfuncs,
{$endif}
{$ifdef Darwin}
CocoaAll, MacOSAll,
{$endif}
{$ifdef windows}
LCLType,
{$endif}
LazFileUtils, uconf, trndi.native, Trndi.API, trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug, trndi.api.debug_edge, trndi.api.debug_missing, trndi.api.debug_perfect, {$endif}
{$ifdef LCLQt6}Qt6, QtWidgets,{$endif}
StrUtils, TouchDetection, ufloat;

type
TFloatIntDictionary = specialize TDictionary<Single, Integer>; // Specialized TDictionary
  // Procedures which are applied to the trend drawing
TTrendProc = procedure(l: TPaintBox; c, ix: integer) of object;
TTrendProcLoop = procedure(l: TPaintBox; c, ix: integer; ls: array of TPaintbox) of object;
TrndiPos = (tpoCenter = 0, tpoBottomLeft = 1, tpoBottomRight = 2, tpoCustom = 3, tpoTopRight = 4);
TPONames = array[TrndiPos] of string;
var
TrndiPosNames: TPONames = (  RS_tpoCenter,  RS_tpoBottomLeft , RS_tpoBottomRight,  RS_tpoCustom, RS_tpoTopRight );
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
  lTir:TLabel;
  lAgo:TLabel;
  miExit: TMenuItem;
  miCustomDots: TMenuItem;
  miDebugBackend: TMenuItem;
  miDotVal: TMenuItem;
  misep: TMenuItem;
  miATouchAuto: TMenuItem;
  miATouchNo: TMenuItem;
  miATouchYes: TMenuItem;
  miADotAdjust: TMenuItem;
  miASystemInfo: TMenuItem;
  miADots: TMenuItem;
  miATouch: TMenuItem;
  miAdvanced: TMenuItem;
  lDot1: TPaintBox;
  miSplit6: TMenuItem;
  miSplit5: TMenuItem;
  miAnnounce:TMenuItem;
  miDotHuge:TMenuItem;
  miDotBig:TMenuItem;
  miDotNormal:TMenuItem;
  miDotSize:TMenuItem;
  miAlternate:TMenuItem;
  miHistory:TMenuItem;
  miClock:TMenuItem;
  miRangeColor:TMenuItem;
  miPref:TMenuItem;
  miFloatOn:TMenuItem;
  pnOffReading: TPanel;
  pnWarning:TPanel;
  pnMultiUser:TPanel;
  pnOffRangeBar:TPanel;
  Separator1:TMenuItem;
  miBorders:TMenuItem;
  miFullScreen:TMenuItem;
  miOnTop:TMenuItem;
  miRefresh:TMenuItem;
  miSplit4:TMenuItem;
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
  mSplit5:TMenuItem;
  Separator4: TMenuItem;
  tAgo:TTimer;
  tClock:TTimer;
  tSwap:TTimer;
  tResize:TTimer;
  tMissed:TTimer;
  tTouch: TTimer;
  tMain: TTimer;
  procedure AdjustGraph;
  procedure fbReadingsDblClick(Sender:TObject);
  procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure FormDblClick(Sender: TObject);
  procedure FormDestroy(Sender:TObject);
  procedure FormKeyPress(Sender:TObject;var Key:char);
  procedure DotPaint(Sender: TObject);
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
  procedure pnWarningPaint(Sender: TObject);
  procedure speakReading;
  procedure FormMouseLeave(Sender:TObject);
  procedure FormMouseMove(Sender:TObject;{%H-}Shift:TShiftState;X,Y:integer);
  procedure FormResize(Sender: TObject);
  procedure FormShow(Sender:TObject);
  procedure lAgoClick(Sender:TObject);
  procedure lArrowClick(Sender:TObject);
  procedure lDiffDblClick(Sender: TObject);
  procedure lDot7DblClick(Sender:TObject);
  procedure lgMainClick(Sender: TObject);
  procedure lTirClick(Sender:TObject);
  procedure lValClick(Sender: TObject);
  procedure lValDblClick(Sender: TObject);
  procedure lValMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: integer);
  procedure lValMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
  procedure lValStartDrag(Sender: TObject; var {%H-}DragObject: TDragObject);
  procedure miAnnounceClick(Sender:TObject);
  procedure miAlternateClick(Sender:TObject);
  procedure miClockClick(Sender:TObject);
  procedure miDotNormalClick(Sender:TObject);
  procedure miFloatOnClick(Sender:TObject);
  procedure miHistoryClick(Sender:TObject);
  procedure miRangeColorClick(Sender:TObject);
  procedure miBordersClick(Sender:TObject);
  procedure miForceClick(Sender: TObject);
  procedure miLimitExplainClick(Sender: TObject);
  procedure miOnTopClick(Sender:TObject);
  procedure miSettingsClick(Sender: TObject);
  procedure onTrendClick(Sender: TObject);
  procedure pnOffReadingPaint(Sender: TObject);
  procedure pmSettingsMeasureItem(Sender:TObject;ACanvas:TCanvas;var AWidth,
    AHeight:integer);
  procedure pmSettingsPopup(Sender:TObject);
  procedure pnMultiUserClick(Sender:TObject);
  procedure pnOffRangeClick(Sender: TObject);
  procedure tAgoTimer(Sender:TObject);
  procedure tClockTimer(Sender:TObject);
  procedure tEdgesTimer(Sender:TObject);
  procedure tResizeTimer(Sender:TObject);
  procedure tMainTimer(Sender: TObject);
  procedure tMissedTimer(Sender:TObject);
  procedure tSwapTimer(Sender:TObject);
  procedure tTouchTimer(Sender: TObject);
  procedure TfFloatOnHide(Sender:TObject);
private
  FStoredWindowInfo: record
    Left, Top, Width, Height: Integer;
    WindowState: TWindowState;
    BorderStyle: TFormBorderStyle;
    FormStyle: TFormStyle;
    Initialized: Boolean;
  end;
    // Array to hold references to lDot1 - lDot10
  TrendDots: array[1..10] of TPaintBox;
  multi: boolean; // Multi user
  MediaController: TSystemMediaController;

  procedure SetLang;
  procedure fixWarningPanel;
  function lastReading: BGReading;
  procedure CalcRangeTime;
  function updateReading(boot: boolean = false): boolean;
  procedure PlaceTrendDots(const Readings: array of BGReading);
  procedure actOnTrend(proc: TTrendProc);
  procedure actOnTrend(proc: TTrendProcLoop);
  procedure setDotWidth(l: TPaintBox; c, ix: integer; {%H-}ls: array of TPaintBox);
  procedure HideDot(l: TPaintBox; {%H-}c, {%H-}ix: integer);
  procedure ResizeDot(l: TPaintBox; {%H-}c, ix: integer);
  procedure initDot(l: TPaintBox; c, ix: integer);
  procedure ExpandDot(l: TPaintBox; c, ix: integer);
  procedure placeForm;

  // Helper methods for update procedure
  function FetchAndValidateReadings: Boolean;
  procedure ProcessCurrentReading;
  function IsDataFresh: Boolean;
  procedure SetNextUpdateTimer(const LastReadingTime: TDateTime);
  procedure UpdateUIBasedOnGlucose;
  procedure HandleHighGlucose(const {%H-}b: BGReading);
  procedure HandleLowGlucose(const {%H-}b: BGReading);
  procedure HandleNormalGlucose(const b: BGReading);
  procedure UpdateOffRangePanel(const Value: Single);
  procedure DisplayLowRange;
  procedure DisplayHighRange;
  procedure FinalizeUpdate;
  procedure UpdateFloatingWindow;
  procedure UpdateUIColors;
  function GetTextColorForBackground(const BgColor: TColor;
    const DarkenFactor: Double = 0.5;
    const LightenFactor: Double = 0.3): TColor;
  function GetAdjustedColorForBackground(
  const BaseColor: TColor;
  const BgColor: TColor;
  const DarkenFactor: Double = 0.6;
  const LightenFactor: Double = 0.4;
  const PreferLighter: Boolean = False): TColor;

  procedure UpdateTrendElements;
  procedure UpdateApiInformation;
  procedure ResizeUIElements;
  procedure UpdateTrendDots;
  procedure ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter; customTl: TTextLayout = tlCenter);

  procedure HandleLatestReadingFreshness(const LatestReading: BGReading; CurrentTime: TDateTime);
  procedure ProcessTimeIntervals(const SortedReadings: array of BGReading; CurrentTime: TDateTime);
  function UpdateLabelForReading(SlotIndex: Integer; const Reading: BGReading): Boolean;
  function DetermineColorForReading(const Reading: BGReading): TColor;
  procedure DoFullScreen;
  {$ifdef DARWIN}
     procedure ToggleFullscreenMac;
  {$endif}

  {$ifdef TrndiExt}
  procedure LoadExtensions;
  {$endif}
public
   procedure AppExceptionHandler(Sender: TObject; {%H-}E: Exception);
   procedure onGH({%H-}sender: TObject);
end;


{$ifdef DARWIN}
function CFStringCreateWithUTF8String(const utf8Str: PAnsiChar): CFStringRef; external name '_CFStringCreateWithUTF8String';
{$endif}

var
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
{$ifdef darwin}
MacAppDelegate: TMyAppDelegate;
upMenu: TMenuItem;
{$endif}

var
bg_alert: boolean = false; // If the BG is high/low since before, so we don't spam notifications
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
{$I tfuncs.inc}


procedure ApplyRoundedCorners(APanel: TPanel; Radius: Integer);
{$IFDEF WINDOWS}
var
  Rgn: HRGN;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Windows: Set a real rounded window region
  Rgn := CreateRoundRectRgn(0, 0, APanel.Width, APanel.Height, Radius, Radius);
  SetWindowRgn(APanel.Handle, Rgn, True);
  {$ENDIF}
end;

procedure TfBG.onGH(sender: TObject);
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

procedure Showmessage(const str: string);
begin
  UXMessage(uxdOnForm, sSuccTitle, str, uxmtInformation, fBG);
end;

procedure Showmessage(const title, str: string);
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
  if native.GetBoolSetting('size.main') then begin
    Width := native.GetIntSetting('size.last.width', width);
    Height := native.GetIntSetting('size.last.height', height);
  end;
  // Hämta och validera position
  posValue := native.GetIntSetting('position.main', Ord(tpoCenter));

  // Validera positionstyp
  if not ((posValue >= Ord(Low(TrndiPos))) and (posValue <= Ord(High(TrndiPos)))) then
    posValue := ord(tpoCenter);

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

// For darkening (multiply each component by 0.8)
function DarkenColor(originalColor: TColor; factor: double = 0.8): TColor;
var
  r, g, b: byte;
begin
  // Extract RGB components
  r := GetRValue(originalColor);
  g := GetGValue(originalColor);
  b := GetBValue(originalColor);

  // Multiply by factor
  r := Round(r * factor);
  g := Round(g * factor);
  b := Round(b * factor);

  // Create new color
  Result := RGB(r, g, b);
end;

// For lightening (increase each component towards 255)
function LightenColor(originalColor: TColor; factor: double = 0.8): TColor;
var
  r, g, b: Integer; // Use Integer to hold intermediate results
begin
  // Extract RGB components
  r := GetRValue(originalColor);
  g := GetGValue(originalColor);
  b := GetBValue(originalColor);
  // Add factor * (255 - component) to each component
  r := Round(r + (factor * (255 - r)));
  g := Round(g + (factor * (255 - g)));
  b := Round(b + (factor * (255 - b)));
  // Clip the values to the range 0..255
  r := Min(255, Max(0, r));
  g := Min(255, Max(0, g));
  b := Min(255, Max(0, b));
  // Create new color
  Result := RGB(r, g, b);
end;


function IsLightColor(bgColor: TColor): boolean;
var
  R, G, B: byte;
  r2, g2, b2: double;
  L: double;
begin
  // Get RBG
  R := GetRValue(bgColor);
  G := GetGValue(bgColor);
  B := GetBValue(bgColor);

  // Convert to 0-1
  r2 := R / 255.0;
  g2 := G / 255.0;
  b2 := B / 255.0;

  // Correct gamma
  if r2 <= 0.04045 then
    r2 := r2 / 12.92 else r2 := Power((r2 + 0.055) / 1.055, 2.4);
  if g2 <= 0.04045 then
    g2 := g2 / 12.92 else g2 := Power((g2 + 0.055) / 1.055, 2.4);
  if b2 <= 0.04045 then
    b2 := b2 / 12.92 else b2 := Power((b2 + 0.055) / 1.055, 2.4);

  // Calculate luminance
  L := 0.2126 * r2 + 0.7152 * g2 + 0.0722 * b2;

  // If L > 0.179 black is more suitable than white
  Result := (L > 0.179);
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
  for ix := 1 to NUM_DOTS do begin
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
  for ix := 1 to NUM_DOTS do begin
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
  Application.processmessages;

  SetDefaultLang(lang, getLangPath);
end;

{$ifdef X_LINUXBSD}
// Moved out as to support the advanced menu aswell
function GetLinuxSystem: string;
  const
    Issue = '/etc/os-release';
  begin
    if FileExists(Issue) then
      Result := ReadFileToString(Issue)
    else
      Result := '';
  end;

function GetLinuxDistro(out ver: string): string;
var
  sys, s: string;
  start, stop: integer;
begin
    sys := GetLinuxSystem;

    start := Pos('ID=', sys)+3; // ID=...
    if start > 0 then begin
      s := Copy(sys, start);
      stop := Pos(#10, s);
      result := Copy(s, 0, stop-1);
      result := TrimSet(result, ['"', #10]);
    end else result := '';

    if (result.IsEmpty) or (result[1] in ['0'..'9']) then begin
      start := Pos('NAME=', sys)+5; // NAME=...
      if start > 0 then begin
        s := Copy(sys, start);
        stop := Pos(#10, s);
        result := Copy(s, 0, stop-1);
        result := TrimSet(result, ['"', #10]);
      end else result := 'unknown';
    end;

    start := Pos('VERSION=', sys)+8; // VERSION="..."
    if start > 0 then begin
      s := Copy(sys, start);
      stop := Pos(#10, s);
      ver := Copy(s, 0, stop-1);
      ver := TrimSet(ver, ['"', #10]);
    end;
end;

function ScanLinuxDistro(const opts: TStringArray): string;
var
  s, sys: string;
begin
  sys := LowerCase(GetLinuxDistro(s));
  result := s;
  for s in opts do
    if Pos(LowerCase(s), sys) > -1 then begin
      result := s;
      Exit;
    end;
end;
  {$endif}

// Initialize the TrendDots array in FormCreate
procedure TfBG.FormCreate(Sender: TObject);
var
  i: integer;
  s, x, guifontName, txtFontName, apiTarget, apiCreds: string;
  fil: boolean;
  userlocale: TFormatSettings;
  {$ifdef X_MAC}
  procedure addTopMenu;
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
       {$else}
  procedure addJumpList;
  begin

  end;
  {$endif}

  procedure prepUI;
  var
    i: integer;
  begin
    // Load fonts
    s := native.GetSetting('font.val', 'default');
    if s <> 'default' then
      lVal.Font.name := s;
    s := native.GetSetting('font.arrow', 'default');
    if s <> 'default' then
      lArrow.Font.name := s;

    s := native.GetSetting('font.ago', 'default');
    if s <> 'default' then
    lAgo.Font.name := s;
    lTir.Font.name := s;

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
    HasTouch :=  native.HasTouchScreen(HasMultiTouch);
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

    bg_rel_color_lo_txt := native.GetColorSetting('ux.bg_rel_color_lo_txt', bg_rel_color_lo_txt);
    bg_rel_color_hi_txt := native.GetColorSetting('ux.bg_rel_color_hi_txt', bg_rel_color_hi_txt);
  end;

procedure initSplash;
begin
  fSplash := TfSplash.Create(nil);
  FStoredWindowInfo.Initialized := False;
  fSplash.Image1.Picture.Icon := Application.Icon;
  fSplash.lInfo.Caption := '';
  fSplash.lInfo.Font.Color := fSplash.lSplashWarn.Font.color;
  fSplash.Show;
end;

procedure loadProfile;
var
  mr: tmodalresult;
begin
  if username <> '' then
    begin
      with TStringList.Create do
      begin
        AddCommaText(username);
        Add('');
//        i := InputCombo(RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX, ToStringArray);
          i := ExtList(uxdAuto, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX, ToStringArray, true);

        if (i > -1) and (strings[i] <> '') then
        begin
          username := strings[i];
          s :=  native.GetSetting('user.nick', '');
          if s = '' then
            s := username;

          fbg.Caption := Format(RS_USER_CAPTION, [s, fBG.Caption]);

          native.configUser :=  username;
        end
        else
          username := '';
      end;// Load possible other users
      multi := true;
      s := native.GetSetting('user.color');
      if s <> '' then
        pnMultiUser.Color := StringToColor(s);
      if pnMultiUser.Color <> clBlack then
        pnMultiUser.Visible := true;
    end
    else
      multi := false;
end;

procedure checkLicense;
const
  license = '⚠️ IMPORTANT MEDICAL WARNING ⚠️'#10#13+
#10+
'This app is NOT a medical device.'#10 +
'• Do NOT make medical decisions based on this data'#10+
'• Data may be WRONG, delayed, or unavailable'#10+
'• Always verify with your official CGM device'#10+
'• For emergencies, contact medical professionals'#10+
#10+
'By continuing, you acknowledge that:'#10+
'• You use this app at your own risk'#10+
'• The developers have NO LIABILITY'#10+
'• You have read and agree to the full terms';
begin
  if native.GetBoolSetting('license.250608') <> true then
  while i <> mrYes do begin
    i :=  ExtMsg(uxdAuto, 'License', 'You must accept the full terms conditions', 'Do you agree to the terms and full license?', license, $00F5F2FD,$003411A9, [mbYes, mbCancel, mbUxRead], uxmtCustom,5);
    if i = mrYes then
       native.SetBoolSetting('license.250608', true)
    else if i = mrCancel then begin
      Application.Terminate;
      Exit;
    end
    else
       OpenURL('https://github.com/slicke/trndi/blob/main/LICENSE.md');
  end;
end;

function loadApi: boolean;
begin
  result := true;
  case native.GetSetting('remote.type') of
    'NightScout':
      api := NightScout.Create(apiTarget, apiCreds, '');
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
   '* Debug Edge Backend *':
      api := DebugEdgeAPI.Create(apiTarget, apiCreds, '');
      {$endif}
    else
      result := false;
    end;
end;

{$ifdef X_LINUXBSD}
procedure linuxinit;
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

begin
  InitSplash;
  Application.processmessages;
  Application.OnException := @AppExceptionHandler;

  // Do this early
  fSplash.lInfo.Caption := 'Starting Media Backend...';
  MediaController := TSystemMediaController.Create(Self);
  MediaController.Initialize;

  Application.ProcessMessages;

  fil := FontGUIInList(guifontName);
  if not fil then
    ShowMessage(Format(RS_FONT_ERROR, [guifontName]));

  fil := FontTxtInList(txtfontName);
  if not fil then
    ShowMessage(Format(RS_FONT_ERROR, [txtfontName]));

  {$ifdef darwin}
    addTopMenu;
  {$endif}
  native := TrndiNative.Create;
  if ssShift in GetKeyShiftState then
    if native.DetectTouchScreen(fil) then
      native.touchOverride := tbFalse
    else
      native.touchOverride := tbTrue;

  if native.isDarkMode then
     native.setDarkMode{$ifdef windows}(self.Handle){$endif};
  {$ifdef X_LINUXBSD}
   linuxinit;
  {$endif}
  {$ifdef DARWIN}
  BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}
  Application.processmessages;
  {$ifndef DARWIN}
   fBG.font.Name := txtFontName; // This will be applyed down later
   lArrow.Font.Name := guifontName;
  {$endif}

  PrepUI;

  // Assign labels to the TrendDots array
  Application.processmessages;
  with native do
  begin
  setLang;
  // Idea for using multiple person/account support
    username := GetRootSetting('users.names','');

   loadProfile;

      userlocale := DefaultFormatSettings;
      userlocale.DecimalSeparator := GetCharSetting('locale.separator', '.');
      badge_adjust := GetIntSetting('ux.badge_size', 0) / 10;
      native.locale := userlocale;

      checkLicense;


    Application.processmessages;
    privacyMode := GetSetting('ext.privacy', '0') = '1';
    if GetSetting('unit', 'mmol') = 'mmol' then
      un := BGUnit.mmol
    else
      un := BGUnit.mgdl;

    apiTarget := GetSetting('remote.target');
    if apiTarget = '' then
    begin
      tMain.Enabled := false;
      for i := 0 to fBG.ComponentCount-1 do // Clear default texts, I want them in the designer window so I won't clear there
        if (fbg.Components[i] is TLabel) and (fbg.Components[i] <> lval) then
          (fbg.Components[i] as TLabel).Caption := '';
      miSettings.Click;
      ShowMessage(RS_FORCE_QUIT_SETUP);
      Application.Terminate;
      Exit;
    end;
    apiCreds := GetSetting('remote.creds');
    Application.processmessages;
    if not loadapi then
      exit;

    Application.processmessages;
    if not api.Connect then
    begin
      ShowMessage(api.ErrorMsg);
      tMain.Enabled := false;
      fSplash.Close;
      fSplash.Free;
      Exit;
    end;

    dotscale := native.GetIntSetting('ux.dot_scale', 1);
    DOT_ADJUST := native.GetFloatSetting('ux.dot_adjust', 0);
    miRangeColor.Checked := native.GetSetting('ux.range_color') = 'true';

    {$ifdef TrndiExt}
    fSplash.lInfo.Caption := RS_SPLASH_LOADING_INIT;
    LoadExtensions;
    {$endif}

    if GetIntSetting('override.enabled', 0) = 1 then
    begin
      api.cgmLo      := GetIntSetting('override.lo', api.cgmLo);
      api.cgmHi      := GetIntSetting('override.hi', api.cgmHi);

      api.cgmRangeLo := GetIntSetting('override.rangelo', api.cgmRangeLo);
      api.cgmRangeHi := GetIntSetting('override.rangehi', api.cgmRangeHi);
    end;
  end;

  Application.processmessages;
  if not updateReading(true) then begin // First reading attempt failed
    updateReading; // We call it twice, to first setup the dots and then make it black
    pnWarning.Visible := true;
    pnWarning.Caption := '⚠️ ' + RS_NO_BOOT_READING;
  end;
  fsplash.Close;
  fsplash.Free;
end;

procedure TfBG.FormDblClick(Sender: TObject);
begin
  if HasTouch then
    DoFullScreen;
end;

function TfBG.lastReading: BGReading;
begin
  result := bgs[Low(bgs)];
end;

procedure TfBG.FormDestroy(Sender:TObject);
begin
  if assigned(native) then
     native.free;
  if assigned(api) then
    api.Free;

  {$ifdef TRNDIEXT}
    TTrndiExtEngine.ReleaseInstance;
  {$endif}

end;

procedure TfBG.speakReading;
begin
  if not privacyMode then
     native.Speak(lVal.Caption)
  else case bgs[Low(bgs)].level of
        trndi.types.BGHigh: begin native.speak('High'); end;
        trndi.types.BGLOW: begin native.speak('Low'); end;
        trndi.types.BGRange: begin native.speak('Good'); end;
        trndi.types.BGRangeHI: begin native.speak('Going high'); end;
        trndi.types.BGRangeLO: begin native.speak('Going low');  end;
   end;
end;

procedure TfBG.FormKeyPress(Sender:TObject;var Key:char);
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
    if slicke.UX.alert.UXDialog(uxdAuto, sRefrshQ, sForceRefresh, [mbYes, mbNo]) = mrYes then
         miForce.Click;
  'I', 'i':
    miSettings.Click;
  end;
end;

procedure TfBG.DotPaint(Sender: TObject);
var
  X, Y: Integer;
  S, fontn: String;
  L: TPaintBox;
  hasfont: boolean;
begin
  L := Sender as TPaintBox;
  S := L.Caption;

  if S = DOT_GRAPH then
     hasFont := FontGUIInList(fontn)
  else
     hasFont := FontTXTInList(fontn);

  with L.Canvas do
  begin
    // Transparent
    Brush.Style := bsClear;

    if hasFont then
      Font.Name := fontn;

  //  Font.Size := 32 + (15 * dotscale);
    //if S <> DOT_GRAPH then
    //  Font.Size := Font.Size div 10;

    Font.Size := L.Font.Size;

    // Center horizintally
    X := (L.Width - TextWidth(S)) div 2;
    // Center vertically -1
    Y := (L.Height - TextHeight(S)) div 2 - 1;

    // Draw
    TextOut(X, Y, S);
  end;
  L.width := max(L.width, L.font.size);
  L.height := max(L.height, L.font.size);
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
    dots := ExtIntInput(uxdAuto, sDotSize, sCustomiseDotSize, sEnterDotSize, dotscale, mr);
    if mr = mrOK then begin
       native.SetSetting('ux.dot_scale', dots.tostring);
       dotscale := dots;
    end;
end;

procedure TfBG.miATouchAutoClick(Sender: TObject);
begin
    miATouchYes.Checked := false;
    miATouchNo.Checked := false;
    miATouchAuto.Checked := false;
    (sender as TMenuItem).Checked:=true;

    native.touchOverride := tbUnknown;
end;

procedure TfBG.miADotAdjustClick(Sender: TObject);
var
  mr: TModalResult;
  da: single;
begin
  da := (ExtNumericInput(uxdAuto, 'Dot Adjustment','Add dot adjustment','You can enter plus or minus. Plus = down. 0 = neutral', DOT_ADJUST*100,false, mr) / 100);

    if mr = mrOK then begin
      DOT_ADJUST := da;

      if ExtMsg(uxdAuto, sChangesSave, sChangesRemember, sChangesApply, FormatFloat('0.00', da), $00F5F2FD,$003411A9, [mbYes, mbNo]) = mrYes then
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

    ShowMessage({$I %FPCTargetOS%} + '(' +{$I %FPCTargetCPU%}+ ')' + ' | ' +
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
  + ' | Default separator: ' + DefaultFormatSettings.DecimalSeparator

  ) ;



end;

procedure TfBG.miATouchClick(Sender: TObject);
begin


end;

procedure TfBG.miATouchNoClick(Sender: TObject);
begin
      miATouchYes.Checked := false;
    miATouchNo.Checked := false;
    miATouchAuto.Checked := false;
    (sender as TMenuItem).Checked:=true;
  native.touchOverride := tbFalse;

end;

procedure TfBG.miATouchYesClick(Sender: TObject);
begin
      miATouchYes.Checked := false;
    miATouchNo.Checked := false;
    miATouchAuto.Checked := false;
    (sender as TMenuItem).Checked:=true;
    native.touchOverride := tbTrue;
end;

procedure TfBG.miDebugBackendClick(Sender: TObject);
begin
  miDebugBackend.Checked := not miDebugBackend.Checked;
end;

procedure TfBG.pnWarningPaint(Sender: TObject);
{$ifndef Windows}
const
  radius = 20;
var
  P: TPanel;
{$endif}
begin
{$ifndef Windows}
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
  end;
{$endif}
end;

procedure TfBG.FormMouseLeave(Sender:TObject);
begin

end;

procedure TfBG.FormMouseMove(Sender:TObject;Shift:TShiftState;X,Y:integer);
begin
  if DraggingWin then
  begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
    tTouch.Enabled := false; // Dont popup stuff while moving
  end;
end;

// FormClose event handler
procedure TfBG.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  posValue: integer;
  {$ifdef Darwin}
    mr: TModalResult;
  {$endif}
begin
  {$ifdef Darwin}
  if self.Showing then
  begin
    mr := UXDialog(uxdAuto, 'Quit or Minimize?', 'Would you like to minimize to the Dock, or close Trndi?',
                   [mbClose, mbUXMinimize, mbCancel]);
    case mr of
      mrClose: CloseAction := caFree;
      mrCancel: Abort;
      else
      begin
        CloseAction := caHide;
        Exit;
      end;
    end;

  end;
  {$else}

  if UXDialog(uxdAuto, RS_QUIT_CAPTION, RS_QUIT_MSG, [mbYes, mbNo], uxmtOK) = mrNo then
    Abort;
  {$endif}
  {$ifdef TrndiExt}
  TTrndiExtEngine.ReleaseInstance;
  {$endif}

  // Hämta och validera position
  posValue := native.GetIntSetting('position.main', ord(tpoCenter));

  if not ((posValue >= Ord(Low(TrndiPos))) and (posValue <= Ord(High(TrndiPos)))) then
    posValue := ord(tpoCenter);

  // Spara positionen om den är custom
  if TrndiPos(posValue) = tpoCustom then
  begin
    native.SetSetting('position.last.left', self.left.toString);
    native.SetSetting('position.last.top', self.top.toString);
  end;

  if native.GetBoolSetting('size.main') then
  begin
    native.SetSetting('size.last.width', self.width.tostring);
    native.SetSetting('size.last.height', self.height.tostring);
  end;
end;

procedure TfBG.fbReadingsDblClick(Sender:TObject);
begin

end;

// Post-process the graph
procedure TfBG.AdjustGraph;
var
  l: TPaintBox;
begin
  for l in TrendDots do
    l.top := l.top + round(ClientHeight * DOT_ADJUST);


end;

procedure TfBG.initDot(l: TPaintBox; c, ix: integer);
begin
  {$ifdef Windows}
    l.Font.Name := 'DejaVu Sans Mono';
  {$endif}
end;

// Expands a trend dot to show actual bg value with highlighting for latest reading
procedure TfBG.ExpandDot(l: TPaintBox; c, ix: integer);
var
  isDot: boolean;
begin

  if privacyMode then // We dont show values while in privacy mode
     exit;

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
  if not isDot then begin // Returning to dot
    ResizeDot(l, c, ix) ;
    l.Font.Size := (ClientWidth div 24)*dotscale;
  end
  else begin
    l.font.Size := (lVal.Font.Size div c);
  end;

end;

// Hides a dot
procedure TfBG.HideDot(l: TPaintBox; c, ix: integer);
begin
  l.Visible := false;
end;

// Scales a dot's font size
procedure TfBG.ResizeDot(l: TPaintBox; c, ix: integer);
begin
  l.AutoSize := true;
  l.Font.Size := Max((lVal.Font.Size div 8)*dotscale, 28); // Ensure minimum font size
  LogMessage(Format('TrendDots[%d] resized with Font Size = %d.', [ix, l.Font.Size]));
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
  if sender = lval then
    tResize.OnTimer(self)
  else
  begin
    tResize.Enabled := false;
    tResize.Enabled := true;
    lVal.Visible := false;
    lAgo.Visible := false;
    lTir.Visible := false;
  end;
  ApplyRoundedCorners(pnWarning, 20);
end;

procedure TfBG.FormShow(Sender: TObject);
begin
  placeForm;
  placed := true;
  lVal.font.Quality := fqCleartype;
end;

procedure TfBG.lAgoClick(Sender:TObject);
var
  s: string;
  i: integer;
begin
  s := miRefresh.Caption;

  if lastReading.getRSSI(i) then
    s += LineEnding + Format(sRSSI, [i]);
  if lastReading.getNoise(i) then
    s += LineEnding + Format(sNoise, [i]);

  s += LineEnding + Format(sDevice, [lastReading.sensor]);;

  ShowMessage(s);

end;

procedure TfBG.lArrowClick(Sender:TObject);
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
  IsCurrentlyFullscreen: Boolean;
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
                          ((WindowState = wsFullScreen) or
                           (BoundsRect.Width >= Screen.Width) and
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
  end;

  // Adjust for dark mode if applicable
  if native.isDarkMode then
    native.setDarkMode{$ifdef windows}(self.Handle){$endif};
end;

procedure TfBG.lDot7DblClick(Sender:TObject);
begin
end;

// Empty event handler
procedure TfBG.lgMainClick(Sender: TObject);
begin
  // Event handler can be left empty if not used
end;

procedure TfBG.lTirClick(Sender:TObject);
var
  minTotal, hours, mins: Integer;
  msg: String;
begin
  minTotal := MinutesBetween(now, bgs[High(bgs)].date);
  if minTotal < 60 then
  begin
    msg := Format(RS_TIR_M, [minTotal]);
  end
  else
  begin
    hours := minTotal div 60;
    mins := minTotal mod 60;
    if hours < 2 then
      msg := Format(RS_TIR_H1, [hours, mins])
    else
      msg := Format(RS_TIR_H, [hours, mins]);
  end;
  ShowMessage(msg);
end;


// Handle lVal click
procedure TfBG.lValClick(Sender: TObject);
begin
  if lVal.Caption = RS_SETUP then
    miSettings.Click;
end;

procedure TfBG.lValDblClick(Sender: TObject);
begin
  if HasTouch then
    DoFullScreen;
end;

// Handle mouse down on lVal
procedure TfBG.lValMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
procedure TfBG.lValMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  IsTouched := false;
  tTouch.Enabled := false;

  DraggingWin := false;
end;

// Empty drag event handler
procedure TfBG.lValStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  // Event handler can be left empty if not used
end;

procedure TfBG.miAnnounceClick(Sender:TObject);
begin
  miAnnounce.Checked := not miAnnounce.Checked;
  native.SetBoolSetting('main.announce', miAnnounce.Checked);
  native.speak(IfThen(miAnnounce.Checked, sAnnounceOn, sAnnounceOff));
end;

procedure TfBG.miAlternateClick(Sender:TObject);
begin
  miAlternate.Checked := not miAlternate.Checked;
  tSwap.Enabled := miAlternate.Checked;
end;

procedure TfBG.miClockClick(Sender:TObject);
begin
  miClock.Checked := not miClock.Checked;
  tClock.Enabled := miClock.Checked;
  native.SetBoolSetting('main.clock', tClock.Enabled);
end;

procedure TfBG.miDotNormalClick(Sender:TObject);
begin
  dotscale := StrToInt((sender as TMenuItem).Hint);
  native.SetSetting('ux.dot_scale', dotscale.ToString);
  FormResize(fBG);
end;

procedure TfBG.TfFloatOnHide(Sender:TObject);
begin
  miFloatOn.Checked := fFloat.Showing;
end;

procedure TfBG.miFloatOnClick(Sender:TObject);
begin
  {$ifdef LCLGTK3}
  Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
  Exit;
  {$endif}
  if fFloat.Showing then
    fFloat.Hide else
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

procedure TfBG.miHistoryClick(Sender:TObject);
var
  i:integer;
  keys, vals: TStringArray;
  b: BGReading;
  xval: integer;
  rssi, noise: string;
begin
SetLength({%H-}keys, high(bgs)+1);
SetLength({%H-}vals, high(bgs)+1);

for i := Low(bgs) to High(bgs) do begin
 if bgs[i].empty then
   continue;

 keys[i] := TimeToStr(bgs[i].date);
 vals[i] := bgs[i].format(un, BG_MSG_SHORT, BGPrimary);
end;


i := ExtTable (true, RS_RHISTORY, RS_RH_TITLE, RS_RH_INFO, keys, vals, uxmtCustom, RS_RH_TIME, RS_RH_READING);
if i > 0 then begin
  b := bgs[i-1];
  if b.getRSSI(xval) then
    rssi := xval.ToString
  else
    rssi := RS_RH_UNKNOWN;

  if b.getNoise(xval) then
    noise := xval.ToString
  else
    noise := RS_RH_UNKNOWN;

  showmessage(TimeToStr(b.date), Format(RS_HISTORY_ITEM, [
                     b.format(un, BG_MSG_SHORT, BGPrimary),
                     b.format(un, BG_MSG_SHORT, BGDelta),
                     b.trend.Img,
                     rssi,
                     noise,
                     b.source,
                     b.sensor
                     ]
  ));
end;
end;

procedure TfBG.miRangeColorClick(Sender:TObject);
begin
  miRangeColor.Checked := not miRangeColor.Checked;
  miForce.Click;
  native.SetBoolSetting('ux.range_color', miRangeColor.Checked);
end;

procedure TfBG.miBordersClick(Sender:TObject);
begin
  {$ifdef LCLGTK3}
    Dialogs.ShowMessage('Your widgetset does not support this feature, pleae use QT');
    Exit;
  {$endif}
  miBorders.Checked := not miBorders.Checked;
  if miBorders.checked then
    self.BorderStyle := bsNone
  else
    {$ifdef DARWIN}
    BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}

  native := TrndiNative.Create;
  if native.isDarkMode then
     native.setDarkMode{$ifdef windows}(self.Handle){$endif};
end;

// Force update on menu click
procedure TfBG.miForceClick(Sender: TObject);
begin
  updateReading;
end;

// Explain limit menu click
procedure TfBG.miLimitExplainClick(Sender: TObject);
begin
//  MessageDlg('Trndi', RS_LIMIT_EXPLAIN_TEXT, mtInformation, [mbOK], '');
ShowMessage(RS_LIMIT_EXPLAIN_TEXT);
end;

procedure TfBG.miOnTopClick(Sender:TObject);
begin
  miOnTop.Checked := not miOnTop.Checked;
  if miOnTop.Checked then
    self.FormStyle := fsSystemStayOnTop
  else
    self.FormStyle := fsNormal;

  if native.isDarkMode then
     native.setDarkMode{$ifdef windows}(self.Handle){$endif};
end;

// Handle settings menu click
procedure TfBG.miSettingsClick(Sender: TObject);
var
  fConf: TfConf;
  lastUsers: Integer;

  procedure LoadUserSettings(f: TfConf);
  var
    s: String;
    i: Integer;
    posValue: Integer;
    po: TrndiPos;
  begin
    with f, native do
    begin
      // Remote and user settings
      s := GetSetting('remote.type');
      for i := 0 to cbSys.Items.Count - 1 do
        if cbSys.Items[i] = s then
          cbSys.ItemIndex := i;
      eAddr.Text := GetSetting('remote.target');
      ePass.Text := GetSetting('remote.creds');
      rbUnit.ItemIndex := IfThen(GetSetting('unit', 'mmol') = 'mmol', 0, 1);

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

      cbTIR.Checked := native.GetBoolSetting('range.custom', true);

      cbOffBar.Checked := native.GetBoolSetting('ux.off_bar', false);
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
      lbUsers.Items.CommaText := s;
      gbMulti.Enabled := s <> '';
      if s = '' then
        lCurrentAcc.Caption := RS_CURRENT_ACC_NO
      else if username = '' then
        lCurrentAcc.Caption := RS_CURRENT_ACC_DEF
      else
        lCurrentAcc.Caption := Format(RS_CURRENT_ACC, [TrimRightSet(username, ['_'])]);

      edNick.Text := GetSetting('user.nick', '');
      s := GetSetting('user.color');
      if s <> '' then
        cbUser.ButtonColor := StringToColor(s);

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
    end;

  end;

  procedure LoadLanguageSettings(f: TfConf);
  var
    i: Integer;
    s: String;
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
  begin
    with f do
    begin
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

      cl_ok_txt.ButtonColor := native.GetColorSetting('ux.bg_color_ok_txt', bg_color_ok_txt);
      cl_hi_txt.ButtonColor := native.GetColorSetting('ux.bg_color_hi_txt', bg_color_hi_txt);
      cl_lo_txt.ButtonColor := native.GetColorSetting('ux.bg_color_lo_txt', bg_color_lo_txt);

      cl_hi_bg_cust.ButtonColor := native.GetColorSetting('ux.bg_rel_color_hi', bg_rel_color_hi);
      cl_lo_bg_cust.ButtonColor := native.GetColorSetting('ux.bg_rel_color_lo', bg_rel_color_lo);

      cl_hi_txt_cust.ButtonColor := native.GetColorSetting('ux.bg_rel_color_hi_txt', bg_rel_color_hi_txt);
      cl_lo_txt_cust.ButtonColor := native.GetColorSetting('ux.bg_rel_color_lo_txt', bg_rel_color_lo_txt);
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
    s: String;
  begin
    with f, native do
    begin
      SetSetting('font.val', lVal.Font.Name);
      SetSetting('font.arrow', lArrow.Font.Name);
      SetSetting('font.ago', lAgo.Font.Name);
      s := ExtractLangCode(cbLang.Items[cbLang.ItemIndex]);
      SetSetting('locale', s);
      native.SetSetting('position.main', IntToStr(cbPos.ItemIndex));
      native.setBoolSetting('size.main', cbSize.Checked);
      SetSetting('user.color', ColorToString(cbUser.ButtonColor));
      SetSetting('user.nick', edNick.Text);

      // Handle user list changes
      if lbUsers.Count > 0 then
        SetSetting('users.names', lbUsers.Items.CommaText)
      else
        SetSetting('users.names', '');
      if lbUsers.Count < lastUsers then
        ShowMessage(RS_REMOVE_ACC);

      // Save remote and override settings
      SetSetting('remote.type', cbSys.Text);
      SetSetting('remote.target', eAddr.Text);
      SetSetting('remote.creds', ePass.Text);
      SetSetting('unit', IfThen(rbUnit.ItemIndex = 0, 'mmol', 'mgdl'));
      SetSetting('ext.privacy', IfThen(cbPrivacy.Checked, '1', '0'));

      // Save unit-specific settings
      if rbUnit.ItemIndex = 0 then
      begin // mmol
        SetSetting('override.lo', Round(fsLo.Value * 18.0182).ToString);
        SetSetting('override.hi', Round(fsHi.Value * 18.0182).ToString);
      end
      else
      begin
        SetSetting('override.lo', Round(fsLo.Value).ToString);
        SetSetting('override.hi', Round(fsHi.Value).ToString);
      end;

      native.SetBoolSetting('range.custom', cbTIR.Checked);
      native.SetBoolSetting('ux.off_bar', cbOffBar.Checked);
      native.SetSetting('locale.separator', edCommaSep.text);
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
    end;
  end;

begin
  fConf := TfConf.Create(Self);
  try
    with native do
    begin
      {$ifdef DEBUG}
      fConf.cbSys.Items.Add('* Debug Backend *');
      fConf.cbSys.Items.Add('* Debug Missing Backend *');
      fConf.cbSys.Items.Add('* Debug Perfect Backend *');
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

    // Show dialog
    fConf.ShowModal;

    // Save settings when dialog closes
    SaveUserSettings(fConf);

    SetLang;
    miForce.Click;

    ShowMessage(RS_RESTART_APPLY);
  finally
    fConf.Free;
  end;
end;

// Swap dots with their readings
procedure TfBG.onTrendClick(Sender: TObject);
var
  isdot: boolean;
  l: TPaintbox;
begin
  l := sender as tPaintbox;

  actOnTrend(@ExpandDot);
  isDot := l.Caption = DOT_GRAPH;;
  if isDot then
    tResize.OnTimer(self);
end;

procedure TfBG.pnOffReadingPaint(Sender: TObject);
var
  Panel: TPanel;
  X, Y: Integer;
  TextStr: string;
  TextW, TextH: Integer;
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

procedure TfBG.pmSettingsMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
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
  H,M: integer;
begin
  // Shift down
  miAdvanced.Visible := ssShift in GetKeyShiftState;
  miBorders.Checked := self.BorderStyle = bsNone;
  miATouch.Checked := HasTouch;
  {$ifdef DEBUG}
    miDebugBackend.Visible := true;
  {$endif}

  if (sender as TPopupMenu).PopupComponent is TPaintBox then begin
    tpb := (sender as TPopupMenu).PopupComponent as TPaintBox;
    H := tpb.Tag div 100;
    M := tpb.Tag mod 100;

    miDotVal.visible := true;
    miDotVal.Caption := Format(sReadingHere, [tpb.hint, H, M]);
  end else
    miDotVal.visible := false;
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
end;

procedure TfBG.pnMultiUserClick(Sender:TObject);
begin
if username <> '' then
   ShowMessage(Format(RS_MULTINAME, [username]))
else
   ShowMessage(RS_MULTINAME_DEF);
end;

// Handle off range panel click
procedure TfBG.pnOffRangeClick(Sender: TObject);
begin
  ShowMessage(Format(RS_RANGE_EXPLANATION,
    [IfThen((Sender as TPanel).Color = bg_rel_color_hi, RS_OVER, RS_UNDER)]));
end;

procedure TfBG.tAgoTimer(Sender:TObject);
var
  d: TDateTime;
  min: int64;
begin
 if sizeof(bgs) < 1 then begin
    lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO;
 end else begin
   try
    d := bgs[Low(bgs)].date; // Last reading time
    min := MilliSecondsBetween(Now, d) div 60000;  // Minutes since last
  {$ifndef lclgtk2} // UTF support IS LIMITED
    lAgo.Caption := '🕑 ' + Format(RS_LAST_UPDATE, [min]);
  {$else}
    lAgo.Caption := '⌚ ' + Format(RS_LAST_UPDATE, [min]);
  {$endif}
   except
     lAgo.Caption := '🕑 ' + RS_COMPUTE_FAILED_AGO;
   end;
 end;
end;

procedure TfBG.tClockTimer(Sender:TObject);
begin
tClock.Enabled := false;
  if Pos(':', lval.Caption) < 1 then begin
    lval.caption :=  FormatDateTime(ShortTimeFormat, Now);
    tClock.Interval := 5000;
    lArrow.Visible := false;
  end else begin
    lval.caption :=  lval.hint;
    tClock.Interval := 20000;
    lArrow.Visible := true;
  end;
  tClock.Enabled := true;
end;

procedure TfBG.tEdgesTimer(Sender:TObject);
begin

end;

procedure TfBG.tResizeTimer(Sender: TObject);
begin
  tResize.Enabled := False;

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

  // Post-process the dots
  AdjustGraph;

  fixWarningPanel;

  if lDot1.Caption <> DOT_GRAPH then
    actOnTrend(@ExpandDot);
end;

procedure TfBG.UpdateTrendElements;
begin
  // Uppdatera punktplacering
  actOnTrend(@SetDotWidth);

  // Ändra storlek på punkterna
  actOnTrend(@ResizeDot);
end;

procedure TfBG.UpdateApiInformation;
begin
  // Uppdatera meny- och inställningsinformation
  miHi.Caption := Format(RS_HI_LEVEL, [api.cgmHi * BG_CONVERTIONS[un][mgdl]]);
  miLo.Caption := Format(RS_LO_LEVEL, [api.cgmLo * BG_CONVERTIONS[un][mgdl]]);

  // Hantera övre intervall
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
  pnOffRangeBar.Height := pnOffRange.height div 4;
  pnOffRangeBar.width := ClientWidth+10;

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
    lArrow.width := ClientWidth;
    lArrow.left := 0;
    lArrow.top := 0;
    ScaleLbl(lArrow);

// TIR
    lTir.font.size := lAgo.Font.Size;
    lTir.Height := lAgo.Height;
    lTir.width := lAgo.Width;
    {$ifdef LCLQt6}
    if isWSL then
     lTir.Width := 50;
   {$endif}
    lTir.left := ClientWidth-lTir.Width;
    lTir.top := 0;
    ScaleLbl(lTir);

//  lArrow.font.color := LightenColor(fBG.Color, 0.8);
//  ScaleLbl(lArrow, taRightJustify, tlTop);
  lArrow.OptimalFill := true;

  pnMultiUser.width := clientwidth div 10;
  pnMultiUser.height := clientheight div 10;
  pnMultiUser.top := clientheight-pnMultiuser.Height;
end;

procedure TfBG.UpdateTrendDots;
var
  UniquePositions: TFloatIntDictionary; // Specialized dictionary
  Dot: TPaintBox;
  Value: Single;
  DPosition: Integer;
begin
  // Initialize dictionary
  UniquePositions := TFloatIntDictionary.Create;
  try
    for Dot in TrendDots do
    begin
      dot.Font.Size := (ClientWidth div 24)*dotscale;
      if TryStrToFloat(Dot.Hint, Value, native.locale) then
      begin
        // Use stored position if value already mapped
        if not UniquePositions.TryGetValue(Value, DPosition) then
        begin
          DPosition := Round((Value - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) * fBG.ClientHeight);
          UniquePositions.Add(Value, DPosition);
        end;

        // Set the label's Top property
        Dot.Top := fBG.ClientHeight - DPosition;
        Dot.Visible := True; // Ensure label is visible
      end
      else
      begin
        // Hide labels without valid Hint values
        Dot.Visible := False;
      end;
    end;
  finally
    // Free dictionary
    UniquePositions.Free;
  end;
end;

procedure TfBG.ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter; customTl: TTextLayout = tlCenter);
var
  Low, High, Mid: Integer;
  MaxWidth, MaxHeight: Integer;
  TextWidth, TextHeight: Integer;
  OptimalSize: Integer;
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
begin
  updateReading;
  {$ifdef TrndiExt}
  try
     TTrndiExtEngine.Instance.CallFunction('updateCallback', [bgs[Low(bgs)].val.ToString, DateTimeToStr(Now)]);
  finally
  end;
  {$endif}
end;

procedure TfBG.tMissedTimer(Sender:TObject);
var
  d: TDateTime;
  min, sec: int64;
begin
  d := bgs[Low(bgs)].date; // Last reading time

  min := MilliSecondsBetween(Now, d) div 60000;  // Minutes since last
  sec := (MilliSecondsBetween(Now, d) mod 60000) div 1000; // Seconds since last

  lDiff.Caption := Format(RS_OUTDATED_TIME, [FormatDateTime('H:mm', d), min, sec]);
end;

procedure TfBG.tSwapTimer(Sender:TObject);
var
  c: TColor;
  s: string;
begin
  // Dont swap when the reading is old
  if fsStrikeOut in Lval.Font.Style then begin
    lArrow.Visible := true;
    Exit;
  end;

  tSwap.Enabled := false;
  s := lval.Caption;

  if s.IndexOf(':') > 0 then // Clock showing
    UpdateUIColors   // Resets standard coloring
  else if lVal.font.color <> lArrow.font.color then begin // Neutral mode
    lArrow.BringToFront;
    c := lArrow.Font.Color;
    lArrow.Font.color := lVal.Font.color;
    lVal.Font.color := c;
  end else begin
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
begin
  tTouch.Enabled := false;
  if IsTouched then
  begin
    p := Mouse.CursorPos;
    pmSettings.PopUp(p.X, p.Y);
  end;
end;

// Request data from the backend and update GUI
procedure TfBG.ProcessCurrentReading;
var
  b: BGReading;
begin
  b := bgs[Low(bgs)];

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

  lval.hint := lval.caption;

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

function TfBG.IsDataFresh: Boolean;
var
  b: BGReading;
  i: integer;
begin
  b := bgs[Low(bgs)];

  // Check if the latest reading is fresh
  Result := MinutesBetween(Now, b.date) <= DATA_FRESHNESS_THRESHOLD_MINUTES;

  if not Result then
  begin
    tMissed.OnTimer(tMissed);
    lVal.Font.Style := [fsStrikeOut];
    if (not TryStrToInt(lVal.Caption[1], i)) or (lArrow.Caption = 'lArrow') then begin // Dont show "Setup" or similar on boot
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;
    fBG.Color := clBlack;
    lVal.Font.Color := clWhite;
    tMissed.Enabled := true;
    lArrow.Caption := ''; // Dont show arrow when not fresh
    native.setBadge('--', clBlack,badge_width+badge_adjust,badge_font+round(badge_adjust*10));
  end
  else
  begin
    tMissed.Enabled := false;
    bg_alert := true;
  end;
end;

procedure TfBG.SetNextUpdateTimer(const LastReadingTime: TDateTime);
var
  i: Int64;
begin
  tMain.Enabled := false;

  i := SecondsBetween(LastReadingTime, now); // Seconds from last
  i := min(BG_REFRESH, BG_REFRESH-(i*1000)); // 5 min or less if there's a recent reading
  i := max(120000, i); // Don't allow too small refresh time (min 2 minutes)

  tMain.Interval := i+15000; // Add 15 secs to allow sync
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
  if native.GetBoolSetting('range.custom', true) then
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
end;

function TfBG.updateReading(boot: boolean = false): boolean;
begin
  result := false;
  lTir.Caption := '';
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
  if not IsDataFresh then begin
    if not boot then
      Exit;
  end else
    result := true;

  // Update UI based on glucose values
  UpdateUIBasedOnGlucose;

  // Complete update and finalize
  FinalizeUpdate;

  // Calc ranges
  CalcRangeTime;

  {$ifdef extensions}
  TTrndiExtEngine.Instance.CallFunction('fetchCallback',[
     bgs[0].format(mgdl, BG_MSG_SHORT), //mgdl reading
     bgs[0].format(mmol, BG_MSG_SHORT), //mmol reading
     bgs[0].format(mgdl, BG_MSG_SIG_SHORT, BGDelta), //mgdl diff
     bgs[0].format(mmol, BG_MSG_SHORT, BGDelta), //mmol diff
     IfThen(bgs[0].empty, 'false', 'true') // has reading?
     ]);
  {$endif}
end;

procedure TfBG.FinalizeUpdate;
begin
  lastup := Now;

  // Handle privacy mode display
  if privacyMode then
  begin
    lval.caption := privacyIcon(bgs[Low(bgs)].level);
    lVal.hint := lval.caption;
  end;

  // Update timers and UI
  tAgo.Enabled := true;
  tAgo.OnTimer(self);
  Self.OnResize(lVal);

  // Update floating window if assigned
  UpdateFloatingWindow;

  // Update text colors based on background
  UpdateUIColors;

  // Update system integration
  native.setBadge(lVal.Caption, fBG.Color{$ifdef lclwin32},badge_width,badge_font{$endif});
  native.done;

  if privacyMode then
    DOT_ADJUST := randomrange(-3, 3) / 10

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
  b := bgs[Low(bgs)];

  if b.val >= api.cgmHi then
    HandleHighGlucose(b)
  else if b.val <= api.cgmLo then
    HandleLowGlucose(b)
  else
    HandleNormalGlucose(b);

  pnOffReading.Visible := native.GetBoolSetting('ux.off_bar', false);
  case b.level of
       trndi.types.BGHigh: begin txt := RS_HIGH; end;
       trndi.types.BGLOW: begin txt := RS_LOW; end;
       trndi.types.BGRange: begin txt := ''; pnOffReading.Visible := false; end;
       trndi.types.BGRangeHI: begin txt := RS_OFF_HI; end;
       trndi.types.BGRangeLO: begin txt := RS_OFF_LO;  end;
  end;

  pnOffReading.Caption := txt;

    pnOffReading.color := GetAdjustedColorForBackground(fBG.Color, fBG.Color);
    pnOffReading.font.color := GetTextColorForBackground(pnOffReading.Color, 0, 0.7);
end;

procedure TfBG.HandleHighGlucose(const b: BGReading);
var
  url: string;
begin
  fBG.Color := bg_color_hi;

  if not bg_alert then
    native.attention(Format(RS_WARN_BG_HI, [lVal.Caption]));

  if highAlerted then
    Exit;

  if native.GetBoolSetting('media.pause') then
    MediaController.Pause;

  url := native.GetSetting('media.url_high', '');
  if url <> '' then begin
     highAlerted := true;
     MediaController.PlayTrackFromURL(url);
  end;
end;

procedure TfBG.HandleLowGlucose(const b: BGReading);
var
  url: string;
begin
  fBG.Color := bg_color_lo;

  if not bg_alert then
    native.attention(Format(RS_WARN_BG_LO, [lVal.Caption]));

  if lowAlerted then
    exit;

  if native.GetBoolSetting('media.pause') then
    MediaController.Pause;

  url := native.GetSetting('media.url_low', '');
  if url <> '' then begin
     lowAlerted := true;
     MediaController.PlayTrackFromURL(url);
  end;
end;

procedure TfBG.HandleNormalGlucose(const b: BGReading);
var
  s, url: string;
  i: integer;
  f: single;
  go: boolean = false;
begin
  bg_alert := false;
  fBG.Color := bg_color_ok;
  highAlerted := false;
  lowAlerted := false;

  if un = mmol then begin
    s := b.format(mmol, BG_MSG_SHORT, BGPrimary);
    if (TryStrToFloat(s, f, native.locale)) and (f = 5.5) then
      go := true
    else
      perfecttriggered := false;
  end else begin
    s := b.format(mgdl, BG_MSG_SHORT, BGPrimary);
    if (TryStrToInt(s, i)) and (i = 100) then
      go := true
    else
      perfecttriggered := false;
  end;

  if go and (not perfecttriggered) then begin
    perfectTriggered := true;

    url := native.GetSetting('media.url_perfect', '');
    if url <> '' then
      MediaController.PlayTrackFromURL(url);
  end;

  UpdateOffRangePanel(b.val);
end;

procedure TfBG.fixWarningPanel;
begin
    pnwarning.width := min(ClientWidth, 300);
    pnWarning.left := ClientWidth div 10;
    pnwarning.top := ClientHeight div 10;
    pnWarning.height := ClientHeight-(pnwarning.top*2);
    CenterPanelToCaption(PNwARNING);

    if Pos(sLineBreak, lMissing.Caption) < 1 then // Ugly solution
      lMissing.Caption := '🕑'+sLineBreak+lMissing.Caption;
    lMissing.AutoSize := false;
    lMissing.left := 5;
    lMissing.top := 5;
    lMissing.width := pnWarning.Width-10;
    lMissing.height := pnWarning.height div 5;
    lMissing.OptimalFill := true;
    if native.HasTouchScreen then begin
      lMissing.wordwrap := true;
      lmissing.font.size :=  clientwidth div 10;
      lmissing.width := pnWarning.width;
      lmissing.height := pnWarning.Height;
      pnWarning.caption := '';
    end;
    pnOffReading.Height := ClientHeight;
    pnOffReading.ClientWidth := ClientWidth div 35;
end;

function TfBG.FetchAndValidateReadings: Boolean;
{$ifdef DEBUG}
var
  res: string;
{$endif}
i: integer;
begin
  Result := False;

  if api = nil then
    Exit;

  {$ifdef DEBUG}
    bgs := api.getReadings(MAX_MIN, MAX_RESULT, '', res);
    if miDebugBackend.Checked then begin
      if Showing then
        if res.IsEmpty then
          slicke.ux.alert.ExtLog(uxdAuto, 'Debug Info', '[empty!]', res)
        else
          slicke.ux.alert.ExtLog(uxdAuto, 'Debug Info', '', res, uxmtCustom, 10);
     end;
  {$ELSE}
       bgs := api.getReadings(MAX_MIN, MAX_RESULT);
  {$endif}


  pnWarning.Visible := false;
  if (Length(bgs) < 1) or (not IsDataFresh) then
  begin
    pnWarning.Visible := true;

    pnWarning.Caption := '⚠️ ' + RS_NO_BACKEND;
    if (not TryStrToInt(lVal.Caption[1], i)) or (lArrow.Caption = 'lArrow') then begin // Dont show "Setup" or similar on boot
      lVal.Caption := '--';
      lArrow.Caption := '';
    end;
    FixWarningPanel;
    Exit;
  end;

  // Call the method to place the points
  PlaceTrendDots(bgs);
  Result := True;
end;

procedure TfBG.UpdateOffRangePanel(const Value: Single);
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
  else if Value <= api.cgmRangeLo then
    DisplayLowRange
  else if Value >= api.cgmRangeHi then
    DisplayHighRange
  else begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
    on := false;
  end;

  // Apply range color if option is enabled
  if on and miRangeColor.Checked then
    fBG.Color := pnOffRange.Color;
end;

procedure TfBG.UpdateUIColors;
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


  if TryStrToInt(lTir.hint, r) then begin  // Check time in range
     if r < bad_tir then // If the value is under the limit for "bad"
       lTir.Font.color := GetAdjustedColorForBackground(clMaroon, fBG.Color, 0.6, 0.4, true)
     else if r > good_tir then
       lTir.Font.color := GetAdjustedColorForBackground($00005900, fBG.Color, 0.6, 0.4, true);
  end;


end;

function TfBG.GetTextColorForBackground(const BgColor: TColor;
  const DarkenFactor: Double = 0.5;
  const LightenFactor: Double = 0.3): TColor;
begin
  if IsLightColor(BgColor) then
    Result := DarkenColor(BgColor, DarkenFactor)
  else
    Result := LightenColor(BgColor, LightenFactor);
end;

function TfBG.GetAdjustedColorForBackground(
  const BaseColor: TColor;
  const BgColor: TColor;
  const DarkenFactor: Double = 0.6;
  const LightenFactor: Double = 0.4;
  const PreferLighter: Boolean = False): TColor;
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
  if miRangeColor.Checked then begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
  end;
  pnOffRangeBar.Visible := true;
  pnOffRange.Caption := Format('↧ %s ↧', [RS_OFF_LO]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then begin
      ffloat.lRangeDown.Visible := true;
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
  if miRangeColor.Checked then begin
    pnOffRange.Visible := false;
    pnOffRangeBar.Visible := false;
  end
  else begin
    pnOffRange.Visible := true;
    pnOffRangeBar.Visible := true;
  end;
  pnOffRange.Caption := Format('↥ %s ↥', [RS_OFF_HI]);

  if Assigned(fFloat) then
  begin
    if not miRangeColor.Checked then begin
      ffloat.lRangeUp.Visible := true;
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
begin
  if Length(Readings) = 0 then
    Exit;

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

procedure TfBG.HandleLatestReadingFreshness(const LatestReading: BGReading; CurrentTime: TDateTime);
var
  isFresh: Boolean;
begin
  isFresh := MinutesBetween(CurrentTime, LatestReading.date) <= DATA_FRESHNESS_THRESHOLD_MINUTES;

  if Assigned(TrendDots[10]) then
  begin
    TrendDots[10].Visible := isFresh;

    if isFresh then
      LogMessage('TrendDots[10] shown as latest reading is fresh.')
    else
      LogMessage('TrendDots[10] hidden due to outdated reading.');
  end;
end;

procedure TfBG.ProcessTimeIntervals(const SortedReadings: array of BGReading; CurrentTime: TDateTime);
var
  slotIndex, i, labelNumber: Integer;
  slotStart, slotEnd: TDateTime;
  found: Boolean;
  reading: BGReading;
  l: TPaintbox;
begin
  for slotIndex := 0 to NUM_DOTS - 1 do
  begin
    // Set start and end time for the intervall
    slotEnd := IncMinute(CurrentTime, -INTERVAL_MINUTES * slotIndex);
    slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

    found := false;

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
        l.Visible := false;
        LogMessage(Format('TrendDots[%d] hidden as no reading found in interval.', [labelNumber]));
      end;
    end;
  end;
end;

function TfBG.UpdateLabelForReading(SlotIndex: Integer; const Reading: BGReading): Boolean;
var
  labelNumber: Integer;
  l: TPaintBox;
  H, M, S, MS: Word;
begin
  Result := False;

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
    setPointHeight(l, Reading.convert(mmol), fBG.ClientHeight);

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

  result := LightenColor(result, -0.8);
end;


end.
