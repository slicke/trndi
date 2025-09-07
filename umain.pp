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
LazFileUtils, uconf, trndi.native, Trndi.API, trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug, trndi.api.debug_edge, trndi.api.debug_missing, trndi.api.debug_perfect, {$endif}
{$ifdef LCLQt6}Qt6, QtWidgets,{$endif}
StrUtils, TouchDetection, ufloat;

type
TFloatIntDictionary = specialize TDictionary<Single, Integer>; // Specialized TDictionary
  // Procedures which are applied to the trend drawing
TTrendProc = procedure(l: TLabel; c, ix: integer) of object;
TTrendProcLoop = procedure(l: TLabel; c, ix: integer; ls: array of TLabel) of object;
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
  lMissing: TLabel;
  lTir:TLabel;
  lAgo:TLabel;
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
  lDot1: TLabel;
  lDot10: TLabel;
  lDot2: TLabel;
  lDot3: TLabel;
  lDot4: TLabel;
  lDot5: TLabel;
  lDot6: TLabel;
  lDot7: TLabel;
  lDot8: TLabel;
  lDot9: TLabel;
  lVal: TLabel;
  miSettings: TMenuItem;
  pmSettings: TPopupMenu;
  mSplit5:TMenuItem;
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
  procedure miExitClick(Sender:TObject);
  procedure miForceClick(Sender: TObject);
  procedure miLimitExplainClick(Sender: TObject);
  procedure miOnTopClick(Sender:TObject);
  procedure miSettingsClick(Sender: TObject);
  procedure onTrendClick(Sender: TObject);
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
  TrendDots: array[1..10] of TLabel;
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
  procedure setDotWidth(l: TLabel; c, ix: integer; {%H-}ls: array of TLabel);
  procedure HideDot(l: TLabel; {%H-}c, {%H-}ix: integer);
  procedure ResizeDot(l: TLabel; {%H-}c, ix: integer);
  procedure ExpandDot(l: TLabel; c, ix: integer);
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
  UXMessage(sSuccTitle, str, system.widechar($2139));
end;

procedure Showmessage(const title, str: string);
begin
  UXMessage(title, str, system.widechar($2139));
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
  if native.GetBoolSetting(username + 'size.main') then begin
    Width := native.GetIntSetting(username + 'size.last.width', width);
    Height := native.GetIntSetting(username + 'size.last.height', height);
  end;
  // Hämta och validera position
  posValue := native.GetIntSetting(username + 'position.main', Ord(tpoCenter));

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
        Left := native.GetIntSetting(username + 'position.last.left', 10);
        Top := native.GetIntSetting(username + 'position.last.top', 10);
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
  ls: array[1..10] of TLabel;
begin
  ls := TrendDots; // Directly use the TrendDots array
  for ix := 1 to NUM_DOTS do
    proc(ls[ix], NUM_DOTS, ix, ls);
  // Run the procedure on the given label
end;

// Apply a procedure to all trend points
procedure TfBG.actOnTrend(proc: TTrendProc);
var
  ix: integer;
  ls: array[1..10] of TLabel;
begin
  ls := TrendDots; // Directly use the TrendDots array
  for ix := 1 to NUM_DOTS do
    proc(ls[ix], NUM_DOTS, ix);
end;

procedure tfBG.SetLang;
var
 lang: string;
begin
  lang := native.GetSetting(username +'locale', '');
  if (lang = 'auto') or (lang = '') then
   lang := native.GetOSLanguage;
  applocale := lang;
  Application.processmessages;

  SetDefaultLang(lang, getLangPath);
end;

// Initialize the TrendDots array in FormCreate
procedure TfBG.FormCreate(Sender: TObject);
var
  i: integer;
  s, fontName, apiTarget, apiCreds: string;
  fil: boolean;
{$ifdef X_LINUXBSD}
function GetLinuxDistro: string;
  const
    Issue = '/etc/os-release';
  begin
    if FileExists(Issue) then
      Result := ReadFileToString(Issue)
    else
      Result := '';
  end;

  {$endif}
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
    s := native.GetSetting(username + 'font.val', 'default');
    if s <> 'default' then
      lVal.Font.name := s;
    s := native.GetSetting(username + 'font.arrow', 'default');
    if s <> 'default' then
      lArrow.Font.name := s;

    s := native.GetSetting(username + 'font.ago', 'default');
    if s <> 'default' then
    lAgo.Font.name := s;
    lTir.Font.name := s;

    // Check graph
    for i := 1 to NUM_DOTS do
      begin
        s := 'lDot' + IntToStr(i);
        TrendDots[i] := FindComponent(s) as TLabel;
        if not Assigned(TrendDots[i]) then
          ShowMessage(Format('Label %s is missing!', [s]))
        else
          LogMessage(Format('Label %s assigned to TrendDots[%d].', [s, i]));
      end;

    // Check touch screen
    HasTouch :=  native.HasTouchScreen(HasMultiTouch);
    if HasMultiTouch then
      touchHelper := TTouchDetector.Create;
  end;

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
  fSplash := TfSplash.Create(nil);
  FStoredWindowInfo.Initialized := False;
  fSplash.Image1.Picture.Icon := Application.Icon;
  fSplash.lInfo.Caption := '';
  fSplash.lInfo.Font.Color := fSplash.lSplashWarn.Font.color;
  fSplash.Show;
  Application.processmessages;
  Application.OnException := @AppExceptionHandler;

  // Do this early
  fSplash.lInfo.Caption := 'Starting Media Backend...';
  MediaController := TSystemMediaController.Create(Self);
  MediaController.Initialize;


  Application.ProcessMessages;

  fil := FontInList(fontName);
  if not fil then
    ShowMessage(Format(RS_FONT_ERROR, [fontName]));

    {$ifdef darwin}
  addTopMenu;
  {$endif}
  native := TrndiNative.Create;
  if native.isDarkMode then
     native.setDarkMode{$ifdef windows}(self.Handle){$endif};
  {$ifdef X_LINUXBSD}
  s := GetLinuxDistro;
  if (Pos('ID=fedora', s) > -1) then
    s := 'Poppins'
  else
  if (Pos('ID=ubuntu', s) > -1) then
    s := 'Sans'
  else
    s := 'default';
  fBG.Font.Name := s;

  IsRaspberry := false;
  if (Pos('ID=debian', s) > -1) then
    IsRaspberry := FileExists('/etc/rpi-issue');

  {$ifndef LCLQt6}
     Showmessage('This release of Trndi was compiled for a non-supported platform ("widgetset")'#10'Performance might be bad and features might not work as intended!'#10#10'Please download the official release (Qt6) from github.com/slicke/trndi');
  {$endif}
  isWSL := TrndiNative.DetectWSL.IsWSL;
  if isWSL then begin
     Showmessage('Windows Linux Subsystem (WSL) detected. Due to limitations in WSL, graphic issues may occur. Commonly, windows will appear at random positions an not where expected!');
  end;
  {$endif}
  {$ifdef DARWIN}
  BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}
  Application.processmessages;
  {$ifndef DARWIN}
      lArrow.Font.Name := fontName;
  {$endif}

  PrepUI;

  // Assign labels to the TrendDots array
  Application.processmessages;
  with native do
  begin
  setLang;
  // Idea for using multiple person/account support
    username := GetSetting('users.names','');
    if username <> '' then
    begin
      with TStringList.Create do
      begin
        AddCommaText(username);
        Add('');
//        i := InputCombo(RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX, ToStringArray);
          i := ExtList(RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX_TITLE, RS_MULTIUSER_BOX, ToStringArray);

        if (i > -1) and (strings[i] <> '') then
        begin
          username := strings[i];
          s :=  GetSetting(username + '_' + 'user.nick', '');
          if s = '' then
            s := username;

          fbg.Caption := Format(RS_USER_CAPTION, [s, fBG.Caption]);
          username := username+'_';
        end
        else
          username := '';
      end;// Load possible other users
      multi := true;
      s := GetSetting(username + 'user.color');
      if s <> '' then
        pnMultiUser.Color := StringToColor(s);
      if pnMultiUser.Color <> clBlack then
        pnMultiUser.Visible := true;
    end
    else
      multi := false;

      //-----LICENSE DO NOT MODIFY
      if native.GetBoolSetting(username + 'license.250608') <> true then
      while i <> mrYes do begin
              i :=  ExtMsg('License', 'You must accept the full terms conditions', 'Do you agree to the terms and full license?', license, $00F5F2FD,$003411A9, [mbYes, mbCancel, mbUxRead], system.widechar($2699));
              if i = mrYes then
                 native.SetBoolSetting(username + 'license.250608', true)
              else if i = mrCancel then begin
                Application.Terminate;
                Exit;
              end
              else
                 OpenURL('https://github.com/slicke/trndi/blob/main/LICENSE.md');
      end;
      //-----END LICENSE

    Application.processmessages;
    privacyMode := GetSetting(username +'ext.privacy', '0') = '1';
    if GetSetting(username +'unit', 'mmol') = 'mmol' then
      un := BGUnit.mmol
    else
      un := BGUnit.mgdl;
    apiTarget := GetSetting(username +'remote.target');
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
    apiCreds := GetSetting(username +'remote.creds');
    Application.processmessages;
    case GetSetting(username +'remote.type') of
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
      Exit;
    end;

    Application.processmessages;
    if not api.Connect then
    begin
      ShowMessage(api.ErrorMsg);
      tMain.Enabled := false;
      fSplash.Close;
      fSplash.Free;
      Exit;
    end;

    {$ifdef TrndiExt}
    fSplash.lInfo.Caption := RS_SPLASH_LOADING_INIT;
    LoadExtensions;
    {$endif}

    if GetIntSetting(username+'override.enabled', 0) = 1 then
    begin
      api.cgmLo      := GetIntSetting(username+'override.lo', api.cgmLo);
      api.cgmHi      := GetIntSetting(username+'override.hi', api.cgmHi);

      api.cgmRangeLo := GetIntSetting(username+'override.rangelo', api.cgmRangeLo);
      api.cgmRangeHi := GetIntSetting(username+'override.rangehi', api.cgmRangeHi);
    end;
  end;
  miRangeColor.Checked := native.GetSetting(username + 'ux.range_color') = 'true';
  dotscale := native.GetIntSetting(username + 'ux.dot_scale', 1);
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
    native.Speak(lVal.Caption);
  'A', 'a':
    miAnnounce.Click;
  'R', 'r':
    if slicke.UX.alert.UXDialog(sRefrshQ, sForceRefresh, [mbYes, mbNo]) = mrYes then
         miForce.Click;
  'I', 'i':
    miSettings.Click;
  end;
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
    mr := UXDialog('Quit or Minimize?', 'Would you like to minimize to the Dock, or close Trndi?',
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

  if UXDialog(RS_QUIT_CAPTION, RS_QUIT_MSG, [mbYes, mbNo], widechar($2705)) = mrNo then
    Abort;
  {$endif}
  {$ifdef TrndiExt}
  TTrndiExtEngine.ReleaseInstance;
  {$endif}

  // Hämta och validera position
  posValue := native.GetIntSetting(username + 'position.main', ord(tpoCenter));

  if not ((posValue >= Ord(Low(TrndiPos))) and (posValue <= Ord(High(TrndiPos)))) then
    posValue := ord(tpoCenter);

  // Spara positionen om den är custom
  if TrndiPos(posValue) = tpoCustom then
  begin
    native.SetSetting(username + 'position.last.left', self.left.toString);
    native.SetSetting(username + 'position.last.top', self.top.toString);
  end;

  if native.GetBoolSetting(username + 'size.main') then
  begin
    native.SetSetting(username + 'size.last.width', self.width.tostring);
    native.SetSetting(username + 'size.last.height', self.height.tostring);
  end;
end;

procedure TfBG.fbReadingsDblClick(Sender:TObject);
begin

end;

// Post-process the graph
procedure TfBG.AdjustGraph;
var
  l: Tlabel;
begin
  for l in TrendDots do
    l.top := l.top + round(ClientHeight * DOT_ADJUST);


end;

// Expands a trend dot to show actual bg value with highlighting for latest reading
procedure TfBG.ExpandDot(l: TLabel; c, ix: integer);
var
  isDot: boolean;
begin

  // Check if label currently shows a dot
  isDot := l.Caption = DOT_GRAPH;

  // Handle differently based on position in trend sequence
  if ix = NUM_DOTS then
    // Latest reading: toggle between fresh indicator and dot
    l.Caption := IfThen(isDot, DOT_FRESH, DOT_GRAPH)
  else
    // Earlier readings: toggle between actual value and dot
    l.Caption := IfThen(isDot, l.Hint, DOT_GRAPH);

  l.Caption := IfThen(isDot, l.Caption, l.Caption);
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
procedure TfBG.HideDot(l: TLabel; c, ix: integer);
begin
  l.Visible := false;
end;

// Scales a dot's font size
procedure TfBG.ResizeDot(l: TLabel; c, ix: integer);
begin
  l.AutoSize := true;
  l.Font.Size := Max((lVal.Font.Size div 8)*dotscale, 28); // Ensure minimum font size
  LogMessage(Format('TrendDots[%d] resized with Font Size = %d.', [ix, l.Font.Size]));
end;

// Sets the width (NOT the font) of a dot
procedure TfBG.SetDotWidth(l: TLabel; c, ix: integer; ls: array of TLabel);
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
  native.SetSetting(username + 'ux.dot_scale', dotscale.ToString);
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


i := ExtTable ( RS_RHISTORY, RS_RH_TITLE, RS_RH_INFO, keys, vals, System.WideChar($2699), RS_RH_TIME, RS_RH_READING);
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
  native.SetBoolSetting(username + 'ux.range_color', miRangeColor.Checked);
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

procedure TfBG.miExitClick(Sender:TObject);
begin

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
      s := GetSetting(username + 'remote.type');
      for i := 0 to cbSys.Items.Count - 1 do
        if cbSys.Items[i] = s then
          cbSys.ItemIndex := i;
      eAddr.Text := GetSetting(username + 'remote.target');
      ePass.Text := GetSetting(username + 'remote.creds');
      rbUnit.ItemIndex := IfThen(GetSetting(username + 'unit', 'mmol') = 'mmol', 0, 1);

      // Override range settings
      if api = nil then
      begin
        fsLo.Value := GetIntSetting(username + 'override.lo', 60);
        fsHi.Value := GetIntSetting(username + 'override.hi', 160);
      end
      else
      begin
        fsLo.Value := GetIntSetting(username + 'override.lo', api.cgmLo);
        fsHi.Value := GetIntSetting(username + 'override.hi', api.cgmHi);
      end;

      cbTIR.Checked := native.GetBoolSetting(username + 'range.custom', true);

      if GetSetting(username + 'unit', 'mmol') = 'mmol' then
        rbUnitClick(Self);

      cbCust.Checked := GetIntSetting(username + 'override.enabled', 0) = 1;
      edMusicHigh.Text := GetSetting(username + 'media_url_high', '');
      edMusicLow.Text := GetSetting(username + 'media_url_low', '');
      edMusicPerfect.Text := GetSetting(username + 'media_url_perfect', '');
      cbMusicPause.Checked := GetBoolSetting(username + 'media_pause');
      fsHi.Enabled := cbCust.Checked;
      fsLo.Enabled := cbCust.Checked;

      // User customizations
      s := GetSetting('users.names', '');
      lbUsers.Items.CommaText := s;
      gbMulti.Enabled := s <> '';
      if s = '' then
        lCurrentAcc.Caption := RS_CURRENT_ACC_NO
      else if username = '' then
        lCurrentAcc.Caption := RS_CURRENT_ACC_DEF
      else
        lCurrentAcc.Caption := Format(RS_CURRENT_ACC, [TrimRightSet(username, ['_'])]);

      edNick.Text := GetSetting(username + 'user.nick', '');
      s := GetSetting(username + 'user.color');
      if s <> '' then
        cbUser.ButtonColor := StringToColor(s);

      // Load position settings
posValue := native.GetIntSetting(username + 'position.main', Ord(tpoCenter));

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

      cbSize.Checked := GetBoolSetting(username + 'size.main');
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
        if GetSetting(username + 'locale', '') = s then
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
      cbPrivacy.Checked := GetSetting(username + 'ext.privacy', '0') = '1';
    end;
  end;

  procedure SetupTouchAndNotifications(f: TfConf);
  begin
    with f, native do
    begin
      cbTouch.Checked := HasTouch;
      cbMultiTouch.Checked := HasMultiTouch;

      {$if defined(X_LINUXBSD)}
      cbNotice.Checked := IsNotifySendAvailable;
      cbNotice.Caption := cbNotice.Caption + ' (Notify Daemon)';
      {$else}
      cbNotice.Checked := True;
      {$endif}
    end;
  end;

  procedure SaveUserSettings(f: TfConf);
  var
    s: String;
  begin
    with f, native do
    begin
      SetSetting(username + 'font.val', lVal.Font.Name);
      SetSetting(username + 'font.arrow', lArrow.Font.Name);
      SetSetting(username + 'font.ago', lAgo.Font.Name);
      s := ExtractLangCode(cbLang.Items[cbLang.ItemIndex]);
      SetSetting(username + 'locale', s);
      native.SetSetting(username + 'position.main', IntToStr(cbPos.ItemIndex));
      native.setBoolSetting(username + 'size.main', cbSize.Checked);
      SetSetting(username + 'user.color', ColorToString(cbUser.ButtonColor));
      SetSetting(username + 'user.nick', edNick.Text);

      // Handle user list changes
      if lbUsers.Count > 0 then
        SetSetting('users.names', lbUsers.Items.CommaText)
      else
        SetSetting('users.names', '');
      if lbUsers.Count < lastUsers then
        ShowMessage(RS_REMOVE_ACC);

      // Save remote and override settings
      SetSetting(username + 'remote.type', cbSys.Text);
      SetSetting(username + 'remote.target', eAddr.Text);
      SetSetting(username + 'remote.creds', ePass.Text);
      SetSetting(username + 'unit', IfThen(rbUnit.ItemIndex = 0, 'mmol', 'mgdl'));
      SetSetting(username + 'ext.privacy', IfThen(cbPrivacy.Checked, '1', '0'));

      // Save unit-specific settings
      if rbUnit.ItemIndex = 0 then
      begin // mmol
        SetSetting(username + 'override.lo', Round(fsLo.Value * 18.0182).ToString);
        SetSetting(username + 'override.hi', Round(fsHi.Value * 18.0182).ToString);
      end
      else
      begin
        SetSetting(username + 'override.lo', Round(fsLo.Value).ToString);
        SetSetting(username + 'override.hi', Round(fsHi.Value).ToString);
      end;

      native.SetBoolSetting(username + 'range.custom', cbTIR.Checked);

      SetSetting(username + 'override.enabled', IfThen(cbCust.Checked, '1', '0'));
      SetSetting(username + 'media_url_high', edMusicHigh.Text);
      SetSetting(username + 'media_url_low', edMusicLow.Text);
      SetSetting(username + 'media_url_perfect', edMusicPerfect.Text);
      SetBoolSetting(username + 'media_pause', cbMusicPause.Checked);
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
  l: tlabel;
begin
  l := sender as tlabel;
  actOnTrend(@ExpandDot);
  isDot := l.Caption = DOT_GRAPH;;
  if isDot then
    tResize.OnTimer(self);
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
begin
  miBorders.Checked := self.BorderStyle = bsNone;
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
  Dot: TLabel;
  Value: Single;
  DPosition: Integer;
begin
  // Initialize dictionary
  UniquePositions := TFloatIntDictionary.Create;
  try
    for Dot in TrendDots do
    begin
      dot.Font.Size := (ClientWidth div 24)*dotscale;
      if TryStrToFloat(Dot.Hint, Value) then
      begin
        // Use stored position if value already mapped
        if not UniquePositions.TryGetValue(Value, DPosition) then
        begin
          DPosition := Round((Value - BG_API_MIN) / (BG_API_MAX - BG_API_MIN) * fBG.ClientHeight);
          UniquePositions.Add(Value, DPosition);
        end;

        // Set the label's Top property
        Dot.Top := fBG.ClientHeight - DPosition - 100;
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
  // Kontrollera grundläggande synlighetsvillkor
  if not ALabel.Visible then
    ALabel.Visible := True;

  if ALabel.Caption = '' then
    Exit; // Ingen text att visa

  // Kontrollera att etiketten har storlek
  if (ALabel.Width <= 0) or (ALabel.Height <= 0) then
  begin
    ALabel.Width := 100;
    ALabel.Height := 30;
  end;

  // Sätt korrekt formatering
  ALabel.AutoSize := False;
  ALabel.WordWrap := False;
  ALabel.Alignment := customAl;
  ALabel.Layout := customTl;

  // Se till att texten är synlig mot bakgrunden
  if ALabel.Font.Color = ALabel.Color then
    ALabel.Font.Color := clBlack;

  // Maximal bredd och höjd för texten
  MaxWidth := ALabel.Width - 4; // Lite padding
  MaxHeight := ALabel.Height - 4;

  // Utför binärsökning för att hitta optimal fontstorlek
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

  // Sätt den optimala fontstorleken
  ALabel.Font.Size := OptimalSize;

  // Se till att inställningarna används
  ALabel.Refresh;
end;
// Update remote on timer
procedure TfBG.tMainTimer(Sender: TObject);
begin
  updateReading;
  {$ifdef TrndiExt}
  TTrndiExtEngine.Instance.CallFunction('updateCallback', [bgs[Low(bgs)].val.ToString, DateTimeToStr(Now)]);
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
    native.Speak(lval.Caption);

  // Set next update time
  SetNextUpdateTimer(b.date);
end;

function TfBG.IsDataFresh: Boolean;
var
  b: BGReading;
begin
  b := bgs[Low(bgs)];

  // Check if the latest reading is fresh
  Result := MinutesBetween(Now, b.date) <= DATA_FRESHNESS_THRESHOLD_MINUTES;

  if not Result then
  begin
    tMissed.OnTimer(tMissed);
    lVal.Font.Style := [fsStrikeOut];
    fBG.Color := clBlack;
    lVal.Font.Color := clWhite;
    tMissed.Enabled := true;
    lArrow.Caption := ''; // Dont show arrow when not fresh
    native.setBadge('--', clBlack{$ifdef lclwin32},badge_width,badge_font{$endif});
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
  if native.GetBoolSetting(username + 'range.custom', true) then
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
    if fBG.Color = bg_color_hi then
      lVal.Caption := '⭱'
    else if fBG.Color = bg_color_lo then
      lVal.Caption := '⭳'
    else
      lVal.Caption := '✓';
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
begin
  b := bgs[Low(bgs)];

  if b.val >= api.cgmHi then
    HandleHighGlucose(b)
  else if b.val <= api.cgmLo then
    HandleLowGlucose(b)
  else
    HandleNormalGlucose(b);
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

  url := native.GetSetting(username +'media_url_high', '');
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
  url := native.GetSetting(username +'media_url_low', '');
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
    if (TryStrToFloat(s, f)) and (f = 5.5) then
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
    if GetBoolSetting(username + 'media_pause') then
      MediaController.Pause;

    url := native.GetSetting(username +'media_url_perfect', '');
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
end;

function TfBG.FetchAndValidateReadings: Boolean;
{$ifdef DEBUG}
var
  res: string;
{$endif}
begin
  Result := False;

  if api = nil then
    Exit;

  {$ifdef DEBUG}
      bgs := api.getReadings(MAX_MIN, MAX_RESULT, '', res);
      if Showing then
        if res.IsEmpty then
          slicke.ux.alert.ExtLog('Debug Info', '[empty!]', res)
        else
          slicke.ux.alert.ExtLog('Debug Info', '', res, System.widechar($2699), 10);
  {$ELSE}
       bgs := api.getReadings(MAX_MIN, MAX_RESULT);
  {$endif}


  pnWarning.Visible := false;
  if (Length(bgs) < 1) or (not IsDataFresh) then
  begin
    pnWarning.Visible := true;
    pnWarning.Caption := '⚠️ ' + RS_NO_BACKEND;
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
  l: TLabel;
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
  l: TLabel;
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

    l.Caption := DOT_GRAPH; // Eller annan symbol
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
