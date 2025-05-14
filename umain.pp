(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit umain;

{$mode objfpc}{$H+}
{$ifdef Darwin}
{$modeswitch objectivec1}
{$endif}

interface

uses
trndi.strings, LCLTranslator, Classes, Menus, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
trndi.api.dexcom, trndi.api.nightscout, trndi.types, math, DateUtils, FileUtil, LclIntf, TypInfo, LResources,
slicke.ux.alert, usplash,
{$ifdef TrndiExt}
trndi.Ext. Engine, trndi.Ext.jsfuncs,
{$endif}
{$ifdef Darwin}
CocoaAll,
{$endif}
LazFileUtils, uconf, trndi.native, Trndi.API, trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug,{$endif}
StrUtils, TouchDetection, ufloat;

type
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
  lAgo:TLabel;
  miFloatOn:TMenuItem;
  miRangeColor:TMenuItem;
  pnMultiUser:TPanel;
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
  tResize:TTimer;
  tMissed:TTimer;
  tTouch: TTimer;
  tMain: TTimer;
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender:TObject);
  procedure FormKeyPress(Sender:TObject;var Key:char);
  procedure FormMouseLeave(Sender:TObject);
  procedure FormMouseMove(Sender:TObject;Shift:TShiftState;X,Y:integer);
  procedure FormResize(Sender: TObject);
  procedure FormShow(Sender:TObject);
  procedure lAgoClick(Sender:TObject);
  procedure lArrowClick(Sender:TObject);
  procedure lDiffDblClick(Sender: TObject);
  procedure lgMainClick(Sender: TObject);
  procedure lValClick(Sender: TObject);
  procedure lValMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  procedure lValMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  procedure lValStartDrag(Sender: TObject; var DragObject: TDragObject);
  procedure miFloatOnClick(Sender:TObject);
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
  procedure pnOffRangeClick(Sender: TObject);
  procedure tAgoTimer(Sender:TObject);
  procedure tEdgesTimer(Sender:TObject);
  procedure tResizeTimer(Sender:TObject);
  procedure tMainTimer(Sender: TObject);
  procedure tMissedTimer(Sender:TObject);
  procedure tTouchTimer(Sender: TObject);
  procedure TfFloatOnHide(Sender:TObject);
  procedure onGH(Sender:TObject);
private
    // Array to hold references to lDot1 - lDot10
  TrendDots: array[1..10] of TLabel;
  multi: boolean; // Multi user

  procedure update;
  procedure PlaceTrendDots(const Readings: array of BGReading);
  procedure actOnTrend(proc: TTrendProc);
  procedure actOnTrend(proc: TTrendProcLoop);
  procedure setDotWidth(l: TLabel; c, ix: integer; ls: array of TLabel);
  procedure HideDot(l: TLabel; c, ix: integer);
  procedure ResizeDot(l: TLabel; c, ix: integer);
  procedure ExpandDot(l: TLabel; c, ix: integer);
  procedure placeForm;
  {$ifdef TrndiExt}
  procedure LoadExtensions;
  {$endif}
public
   procedure AppExceptionHandler(Sender: TObject; E: Exception);
end;



var
native: TrndiNative;
{$ifdef darwin}
MacAppDelegate: TMyAppDelegate;
{$endif}


procedure SetPointHeight(L: TLabel; value: single);

const
MAX_MIN = 1440; // Max time to request
INTERVAL_MINUTES = 5; // Each time interval is 5 minutes
NUM_DOTS = 10;        // Total number of labels (lDot1 - lDot10)
DATA_FRESHNESS_THRESHOLD_MINUTES = 7; // Max minutes before data is considered outdated

BG_API_MIN = 2;
  // NS can't read lower
BG_API_MAX = 22;
  // NS can't read higher
BG_REFRESH = 300000; // 5 min refresh

DOT_GRAPH =  '•';
DOT_FRESH = LineEnding +'☉';

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

fBG: TfBG;
api: TrndiAPI;
un: BGUnit = BGUnit.mmol;
bgs: BGResults;
{$ifdef TrndiExt}
jsFuncs: TJSfuncs;
{$endif}

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

{$ifdef LINUX}
IsRaspberry: boolean;
{$endif}

implementation

{$R *.lfm}
{$I tfuncs.inc}

{$IFDEF DARWIN}

procedure TMyAppDelegate.miSettingsMacClick(sender: id);
begin
  (Application.MainForm as TfBG).miSettingsClick(nil);
end;

procedure TfBG.onGH(sender: TObject);
begin
OpenURL('https://github.com/slicke/trndi');
end;

function TMyAppDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
var
  dockMenu: NSMenu;
  menuItem: NSMenuItem;
begin

  // Create a custom dock menu

  dockMenu := NSMenu.alloc.initWithTitle(NSSTR('Trndi'));


  // Add items to the menu

  menuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(

    NSSTR(TrimLeftSet(
    (Application.MainForm as TfBG).miSettings.Caption, ['&', ' '])), sel_registerName('miSettings:'), NSSTR(''));

  dockMenu.addItem(menuItem);

  menuItem.release;



  // Add a separator

//  dockMenu.addItem(NSMenuItem.separatorItem);


  Result := dockMenu;

end;

  function TMyAppDelegate.applicationShouldHandleReopen_hasVisibleWindows(

  sender: NSApplication; hasVisibleWindows: Boolean): Boolean;

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
  UXMessage(sSuccTitle, str, widechar($2139));
end;

procedure TfBG.placeForm;
var
  pos, cust: integer;
begin
  pos := native.GetIntSetting(username + 'position.main', ord(tpoCenter));
  if not ((pos >= Ord(Low(TrndiPos))) and (pos <= Ord(High(TrndiPos)))) then
    pos := ord(tpoCenter);

  case TrndiPos(pos) of
  tpoCenter:
  begin
    self.Left := Screen.WorkAreaLeft + (Screen.WorkAreaWidth - Width) div 2;
    self.Top := Screen.WorkAreaTop + (Screen.WorkAreaHeight - Height) div 2;
  end;
  tpoBottomLeft:
  begin
    self.Left := 20;
    self.Top := (Screen.WorkAreaRect.Bottom - Height) - 200;
  end;
  tpoBottomRight:
  begin
    self.Left := Screen.WorkAreaRect.Right - 20;
    self.Top := (Screen.WorkAreaRect.Bottom - Height) - 200;
  end;
  tpoTopRight:
  begin
    self.Left := Screen.WorkAreaRect.Right - (self.width) - 20;
    self.Top := 200;
  end;
  tpoCustom:
  begin
    pos := native.GetIntSetting(username +'position.last.left', 10);
    self.left := pos;
    pos := native.GetIntSetting(username +'position.last.top', 10);
    self.top := pos;
  end;
  end;
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
  r, g, b: byte;
begin
  // Extract RGB components
  r := GetRValue(originalColor);
  g := GetGValue(originalColor);
  b := GetBValue(originalColor);

  // Add factor * (255 - component) to each component
  r := Round(r + (factor * (255 - r)));
  g := Round(g + (factor * (255 - g)));
  b := Round(b + (factor * (255 - b)));

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

procedure PaintLbl(Sender: TLabel; OutlineWidth: integer = 1; OutlineColor: TColor = clBlack);
var
  X, Y: integer;
  OriginalColor: TColor;
  TextRect: TRect;
  TextStyle: TTextStyle;
begin
  with Sender as TLabel do
  begin
    // Create draw area
    TextRect := ClientRect;

    // Set the text
    TextStyle := Canvas.TextStyle;
    TextStyle.Alignment := Alignment;
    TextStyle.Layout := Layout;
    TextStyle.Wordbreak := WordWrap;
    TextStyle.SingleLine := not WordWrap;
    TextStyle.Clipping := true;

    // Remember original color
    OriginalColor := Font.Color;

    // Set canvas font
    Canvas.Font := Font;

    // Paint contour ("outline color")
    Canvas.Font.Color := outlinecolor;

    for X := -OutlineWidth to OutlineWidth do
      for Y := -OutlineWidth to OutlineWidth do
        if (X <> 0) or (Y <> 0) then
          Canvas.TextRect(
            Rect(TextRect.Left + X, TextRect.Top + Y,
            TextRect.Right + X, TextRect.Bottom + Y),
            0, 0, // Not used with text style
            Caption,
            TextStyle)// Make a copy
    ;

    // Re-draw original color
    Canvas.Font.Color := OriginalColor;
    Canvas.TextRect(TextRect, 0, 0, Caption, TextStyle);
  end;
end;

{$ifdef DEBUG}
procedure LogMessage(const Msg: string);
const
  MaxLines = 500; // Max lines in file
var
  LogLines: TStringList;
begin
  LogLines := TStringList.Create;
  try
    // Load log if exists
    if FileExists('trndi.log') then
      LogLines.LoadFromFile('trndi.log');

    // Delete overflowing lines
    while LogLines.Count >= MaxLines do
      LogLines.Delete(0);

    // Add new message
    LogLines.Add('['+DateTimeToStr(Now) + '] ' + Msg);

    // Save
    LogLines.SaveToFile('trndi.log');
  finally
    LogLines.Free;
  end;
end;
{$else}
// Remove when launching
procedure LogMessage(const Msg: string);
begin

end;
{$endif}

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

  with TTrndiExtEngine.Instance do
  begin
    addClassFunction('uxProp', ExtFunction(@JSUX), 3);
    addClassFunction('getUnit', ExtFunction(@JSUnit), 0);
    addClassFunction('setLevelColor', ExtFunction(@JSLevelColor), -1);
    // Add the UX modification function, as declared in this file
    for s in exts do
      // Run all found files
      ExecuteFile(s);
    exts.Free;
  end;
end;
{$endif}

// Implement a simple insertion sort for BGReading
procedure SortReadingsDescending(var Readings: array of BGReading);
var
  i, j: integer;
  temp: BGReading;
begin
  for i := 1 to High(Readings) do
  begin
    temp := Readings[i];
    j := i - 1;
    while (j >= 0) and (Readings[j].date < temp.date) do
    begin
      Readings[j + 1] := Readings[j];
      Dec(j);
    end;
    Readings[j + 1] := temp;
  end;
end;

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

// Initialize the TrendDots array in FormCreate
procedure TfBG.FormCreate(Sender: TObject);
var
  i: integer;
  s, apiTarget, apiCreds, lang: string;
  fs: TfSplash;
{$ifdef Linux}
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
  {$ifdef darwin}
     MainMenu: TMainMenu;
     AppMenu,
     forceMenu,
     SettingsMenu,
     HelpMenu,
     GithubMenu: TMenuItem;
  {$endif}
begin
  fs := TfSplash.Create(nil);
  fs.Image1.Picture.Icon := Application.Icon;
fs.Show;
Application.processmessages;
Application.OnException := @AppExceptionHandler;

  if not FontInList(s) then
    ShowMessage(Format(RS_FONT_ERROR, [s]));


  {$ifdef darwin}
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

    githubmenu := TMenuItem.Create(self);
    githubmenu.Caption := 'Trndi on GitHub';
    githubmenu.onclick := @onGH;
    helpMenu.Insert(0, githubMenu);

  {$endif}
  native := TrndiNative.Create;
  if native.isDarkMode then
     native.setDarkMode{$ifdef windows}(self.Handle){$endif};
  {$ifdef Linux}
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
  {$endif}

  {$ifdef DARWIN}
  BorderStyle := bsSizeable;
  {$else}
  BorderStyle := bsSizeToolWin;
  {$endif}
  Application.processmessages;
  lVal.Font.name := native.GetSetting(username + 'font.val', lVal.Font.name);
  lArrow.Font.name := native.GetSetting(username + 'font.arrow', lArrow.Font.name);
  lAgo.Font.name := native.GetSetting(username + 'font.ago', lAgo.Font.name);


  // Assign labels to the TrendDots array
  for i := 1 to NUM_DOTS do
  begin
    s := 'lDot' + IntToStr(i);
    TrendDots[i] := FindComponent(s) as TLabel;
    if not Assigned(TrendDots[i]) then
      ShowMessage(Format('Label %s is missing!', [s]))
    else
      LogMessage(Format('Label %s assigned to TrendDots[%d].', [s, i]));
  end;
  Application.processmessages;
  with native do
  begin
    HasTouch :=  HasTouchScreen(HasMultiTouch);
    if HasMultiTouch then
      touchHelper := TTouchDetector.Create;
    lang := GetSetting(username +'locale', '');
    if (lang = 'auto') or (lang = '') then
      lang := GetOSLanguage;
    Application.processmessages;

    SetDefaultLang(lang,'lang');
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
      {$endif}
    else
      Exit;
    end;

    Application.processmessages;
    if not api.Connect then
    begin
      ShowMessage(api.ErrorMsg);
      tMain.Enabled := false;
        fs.Close;
  fs.Free;
      Exit;
    end;

    {$ifdef TrndiExt}
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
  Application.processmessages;
  update;
  fs.Close;
  fs.Free;
end;

procedure TfBG.FormDestroy(Sender:TObject);
begin
  if assigned(native) then
     native.free;
  if assigned(api) then
    api.Free;
end;

procedure TfBG.FormKeyPress(Sender:TObject;var Key:char);
begin
  if key = #27 then
  begin // esc
    lDiffDblClick(self);

    key := #0;  // Prevent other handlers of esc
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
  i: integer;
  pos: integer;
  mr : TModalResult;
begin
  {$ifdef Darwin}
   if self.Showing then begin
//     mr := ExtMsg('Minimize to dock?','Close Trndi?','Close or Minimize Trndi?','', $00F5F2FD,$003411A9,[mbClose , mbUXMinimize, mbCancel]);
     mr := UXDialog('Quit or Minimize?', 'Would you like to minimize to the Dock, or close Trndi?',[mbClose, mbUXMinimize, mbCancel]);
     case mr of
       mrClose: CloseAction:= caFree;
       mrCancel: Abort;
       else begin
         CloseAction := caHide;
         Exit;
       end;
     end;
     Exit;
   end;
  {$endif}
    if UXDialog(RS_QUIT_CAPTION, RS_QUIT_MSG, [mbYes, mbNo], widechar($2705)) = mrNo then
      Abort;

  {$ifdef TrndiExt}
  TTrndiExtEngine.ReleaseInstance;
  {$endif}

  pos := native.GetIntSetting(username +'position.main', ord(tpoCenter));
  if not ((pos >= Ord(Low(TrndiPos))) and (pos <= Ord(High(TrndiPos)))) then
    pos := ord(tpoCenter);
  if TrndiPos(pos) = tpoCustom then
  begin
    native.SetSetting(username +'position.last.left', self.left.toString);
    native.SetSetting(username +'position.last.top', self.top.toString);
  end;
//  LogMessage('Trend closed.');
end;

// Changes a trend dot from a dot to the actual bg value with highlighting for the latest reading
procedure TfBG.ExpandDot(l: TLabel; c, ix: integer);
var
  gnow: boolean;
begin
  gnow := l.Caption = DOT_GRAPH; // Graph now

  if ix = NUM_DOTS then // Latest reading at lDot10
    l.Caption := IfThen(gnow, DOT_FRESH, DOT_GRAPH)
  else
    l.Caption := IfThen(gnow, LineEnding + l.Hint, DOT_GRAPH);

  if not gnow then
    ResizeDot(l, c, ix)
  else
    l.font.Size := lVal.Font.Size div c;
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
  l.Font.Size := Max(lVal.Font.Size div 8, 28); // Ensure minimum font size
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
    lArrow.Visible := false;
  end;
end;

procedure TfBG.FormShow(Sender:TObject);
begin
  placeForm;
  placed := true;
end;

procedure TfBG.lAgoClick(Sender:TObject);
begin

end;

procedure TfBG.lArrowClick(Sender:TObject);
begin

end;

// Handle full screen toggle on double-click
procedure TfBG.lDiffDblClick(Sender: TObject);
function IsMaximized(Form: TForm): boolean;
  begin
    {$IFDEF DARWIN}
  // Jämför med skärmstorlek minus menubar/dock
    Result := (Form.BoundsRect.Width >= Screen.WorkAreaWidth) and
      (Form.BoundsRect.Height >= Screen.WorkAreaHeight);
    {$ELSE}
    Result := Form.WindowState = wsMaximized;
    {$ENDIF}
  end;


begin
  if IsMaximized(self) then
  begin
    {$ifdef DARWIN}
    Showmessage('macOS cant restore the main window, please restart!');
//  Application.Terminate;
    borderstyle := bsSizeable;
    fBG.WindowState := wsNormal;
    fBG.FormStyle := fsNormal;
    Exit;
    {$else}
    BorderStyle := bsSizeToolWin;
    fBG.WindowState := wsNormal;
    fBG.FormStyle := fsNormal;
    {$endif}
    fBG.Width := Max(Screen.Width div 5, 200);
    fBG.height := Max(Screen.Height div 5, 300);
    fBG.Top := (screen.Height div 2) - (fbg.Height div 2);
    fBG.left := (screen.width div 2) - (fbg.width div 2)
  end
  else
  begin
    fBG.WindowState := wsFullScreen;
    fBG.FormStyle := fsStayOnTop;
    fBG.BorderStyle := bsNone;
  end;
    if native.isDarkMode then
     native.setDarkMode{$ifdef windows}(self.Handle){$endif};
end;

// Empty event handler
procedure TfBG.lgMainClick(Sender: TObject);
begin
  // Event handler can be left empty if not used
end;


// Handle lVal click
procedure TfBG.lValClick(Sender: TObject);
begin
  if lVal.Caption = RS_SETUP then
    miSettings.Click;
end;

// Handle mouse down on lVal
procedure TfBG.lValMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  // Handle touch screens
  StartTouch := Now;
  IsTouched := true;
  tTouch.Enabled := true;

  if (Button = mbLeft) and (self.BorderStyle = bsNone) then
  begin   // Handle window moving
    DraggingWin := true;
    PX := X;
    PY := Y;
  end;
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

procedure TfBG.miRangeColorClick(Sender:TObject);
begin
  miRangeColor.Checked := not miRangeColor.Checked;
  if miRangeColor.Checked then
    ShowMessage(RS_RANGE_COLOR);
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
end;

procedure TfBG.miExitClick(Sender:TObject);
begin

end;

// Force update on menu click
procedure TfBG.miForceClick(Sender: TObject);
begin
  update;
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
end;

// Handle settings menu click
procedure TfBG.miSettingsClick(Sender: TObject);
var
  i, lastusers: integer;
  s: string;
  po: TrndiPos;
  mTouch: boolean;
begin
  with TfConf.Create(self) do begin
    with native do
    begin
      {$ifdef DEBUG}
      cbSys.Items.Add('* Debug Backend *');
      {$endif}
      s := GetSetting(username +'remote.type');
      for i := 0 to cbSys.Items.Count - 1 do
        if cbSys.Items[i] = s then
          cbSys.ItemIndex := i;

      eAddr.Text := GetSetting(username +'remote.target');
      ePass.Text := GetSetting(username +'remote.creds');
      rbUnit.ItemIndex := IfThen(GetSetting(username +'unit', 'mmol') = 'mmol', 0, 1);

      if api = nil then
      begin
        fsLo.Value := GetIntSetting(username+'override.lo', 60);
        fsHi.Value := GetIntSetting(username+'override.hi', 160);
      end
      else
      begin
        fsLo.Value := GetIntSetting(username+'override.lo', api.cgmLo);
        fsHi.Value := GetIntSetting(username+'override.hi', api.cgmhi)
      end;

      if GetSetting(username +'unit', 'mmol') = 'mmol' then
        rbUnitClick(self)//    fshi.DecimalPlaces := 1;
//   fslo.DecimalPlaces := 1;
//        fsHi.Value := fsHi.Value / 18;
//      fsLo.Value := fsLo.Value / 18;
      ;

      cbCust.Checked := GetIntSetting(username+'override.enabled', 0) = 1;
      fsHi.Enabled :=  cbCust.Checked;
      fsLo.Enabled :=  cbCust.Checked;

      {$ifdef TrndiExt}
      eExt.Text := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
      {$else}
      eExt.Text := '- '+RS_noPlugins +' -';
      eExt.Enabled := false;
      {$endif}
      cbPrivacy.Checked := GetSetting(username +'ext.privacy', '0') = '1';

      s := GetSetting('users.names','');
      lbUsers.Items.AddCommaText(s);
      if (s <> '') then begin
        gbMulti.Enabled := true;
        if username = '' then
          lCurrentAcc.Caption := RS_CURRENT_ACC_DEF
        else
          lCurrentAcc.Caption := Format(RS_CURRENT_ACC, [TrimRightSet(username, ['_'])]);
      end else
        lCurrentAcc.Caption := RS_CURRENT_ACC_NO;

      s := GetSetting(username + 'user.color');
      if s <> '' then
        cbUser.ButtonColor := StringToColor(s);

      // Load positions of the form
      i := native.GetIntSetting(username +'position.main', ord(tpoCenter));
      for po in TrndiPos do
      begin
        s := TrndiPosNames[po]; // Localized name of the pos
        cbPos.items.Add(s);
        if ord(po) = i then
          cbPos.ItemIndex := i;
      end;
      if cbPos.ItemIndex = -1 then
        cbPos.ItemIndex := 0;

      lastusers := lbusers.count;
      edNick.Text := GetSetting(username + 'user.nick', '');

      ListLanguageFiles(cbLang.Items, ExtractFileDir(Application.ExeName) + DirectorySeparator + 'lang');
      cbLang.Items.Add('Trndi.en');
      cbLang.Items.Add('Trndi.auto');

      for i := 0 to cbLang.items.Count-1 do begin
        s := cbLang.Items[i];
        cbLang.Items[i] := ExtractDelimited(2,s,['.']);
        s := cbLang.Items[i];
        cbLang.Items[i] := Format('%s (%s)', [GetLanguageName(s), s]);;
        if GetSetting(username +'locale', '') = s then
          cbLang.ItemIndex := i;
      end;
      if cbLang.ItemIndex = -1 then
        cbLang.ItemIndex := cbLang.Items.Count-1;

      lVal.Font.name := self.lVal.Font.name;
      lArrow.Font.name := self.lArrow.Font.name;
      lAgo.Font.name := self.lAgo.Font.name;

      lVal.font.color := self.lVal.font.color;
      lArrow.font.color := self.lArrow.font.color;
      lAgo.font.color := self.lAgo.font.color;

      lVal.caption := self.lVal.caption;
      lArrow.caption := self.lArrow.caption;
      lAgo.caption := self.lAgo.caption;
      pnDisplay.Color := self.color;
      pnDisplay.Font := fBG.font;

      cbTouch.Checked := native.HasTouchScreen(mTouch);
      cbMultiTouch.Checked := mTouch;
      {$if defined(LINUX)}
        cbNotice.Checked := IsNotifySendAvailable;
        cbNotice.Caption := cbNotice.Caption + ' (Notify Daemon)';
      {$else}
        cbNotice.Checked := true;
      {$endif}
      //--
      ShowModal;
      //---

      SetSetting(username + 'font.val', lVal.Font.name);
      SetSetting(username + 'font.arrow', lArrow.Font.name);
      SetSetting(username + 'font.ago', lAgo.Font.name);

      s := ExtractLangCode(cblang.Items[cbLang.ItemIndex]);

      SetSetting(username +'locale', s);

      native.SetSetting(username +'position.main', IntToStr(cbPos.ItemIndex));

      SetSetting(username + 'user.color', ColorToString(cbUser.ButtonColor));
      SetSetting(username + 'user.nick', edNick.Text);

      if lbUsers.Count > 0 then
        SetSetting('users.names',lbUsers.Items.CommaText)
      else
        SetSetting('users.names', '');

      if lbUsers.Count < lastusers then
         ShowMessage(RS_REMOVE_ACC);

      SetSetting(username +'remote.type', cbSys.Text);
      SetSetting(username +'remote.target', eAddr.Text);
      SetSetting(username +'remote.creds', ePass.Text);
      SetSetting(username +'unit', IfThen(rbUnit.ItemIndex = 0, 'mmol', 'mgdl'));
      SetSetting(username +'ext.privacy', IfThen(cbPrivacy.Checked, '1', '0'));


      if rbUnit.ItemIndex = 0 then
      begin//mmol
        SetSetting(username+'override.lo', round(fsLo.Value * 18.0182).ToString);
        SetSetting(username+'override.hi', round(fsHi.value * 18.0182).tostring);
      end
      else
      begin
        SetSetting(username+'override.lo', round(fsLo.Value).tostring);
        SetSetting(username+'override.hi', round(fsHi.value).tostring);
      end;

      SetSetting(username+'override.enabled', IfThen(cbCust.Checked, '1', '0'));

      ShowMessage(RS_RESTART_APPLY);
    end;
    Free;
  end;

end;

// Swap dots with their readings
procedure TfBG.onTrendClick(Sender: TObject);
begin
  actOnTrend(@ExpandDot);
end;

procedure TfBG.pmSettingsMeasureItem(Sender:TObject;ACanvas:TCanvas;var AWidth,
AHeight:integer);
var
  MenuItem: TMenuItem;
  TextX, TextY: integer;
begin
  MenuItem := TMenuItem(Sender);

  if menuitem.Caption = '-' then
    exit;
  // Set desired font
  ACanvas.Font.Name := 'Arial';
  ACanvas.Font.Size := 16;
  ACanvas.Font.Style := [fsBold];

  // Calculate text dimensions
  Textx := ACanvas.TextWidth(MenuItem.Caption);
  Texty := ACanvas.TextHeight(MenuItem.Caption);

  // Set item dimensions with padding
  AWidth := Textx + 40;  // Add 40 pixels for padding and icons
  AHeight := Texty + 8;  // Add 8 pixels for vertical padding

  // Ensure minimum width
  if AWidth < 100 then
    AWidth := 100;

  // Ensure minimum height
  if AHeight < 25 then
    AHeight := 25;
end;

procedure TfBG.pmSettingsPopup(Sender:TObject);
begin
  miBorders.Checked := self.BorderStyle = bsNone;
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
  d := bgs[Low(bgs)].date; // Last reading time

  min := MilliSecondsBetween(Now, d) div 60000;  // Minutes since last

  {$ifndef lclgtk2} // UTF support IS LIMITED
    lAgo.Caption := '🕑 ' + Format(RS_LAST_UPDATE, [min]);
  {$else}
    lAgo.Caption := '⌚ ' + Format(RS_LAST_UPDATE, [min]);
  {$endif}
end;

procedure TfBG.tEdgesTimer(Sender:TObject);
begin

end;

procedure TfBG.tResizeTimer(Sender:TObject);
procedure ScaleLbl(ALabel: TLabel);
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
  ALabel.Alignment := taCenter;
  ALabel.Layout := tlCenter;

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

var
 dot: TLabel;

begin
  tResize.Enabled := false;
  // Update dot placement
  actOnTrend(@SetDotWidth);

  // Remove or comment out the following line to prevent labels from being hidden:
  // actOnTrend(@HideDot);

  // Adjust label sizes
//  lArrow.Height := fBG.clientHeight div 3;


  // Resize the dots
  actOnTrend(@ResizeDot);

  if not assigned(api) then
    Exit;

  // Set info
  miHi.Caption := Format(RS_HI_LEVEL, [api.cgmHi * BG_CONVERTIONS[un][mgdl]]);
  miLo.Caption := Format(RS_LO_LEVEL, [api.cgmLo * BG_CONVERTIONS[un][mgdl]]);

  if api.cgmRangeHi <> 500 then
    miRangeHi.Caption := Format(RS_RANGE_HI, [api.cgmRangeHi * BG_CONVERTIONS[un][mgdl]])
  else
    miRangeHi.Caption := RS_RANGE_HI_UNSUPPORTED;

  if api.cgmRangeLo <> 0 then
    miRangeLo.Caption := Format(RS_RANGE_LO, [api.cgmRangeLo * BG_CONVERTIONS[un][mgdl]])
  else
  begin
    miRangeLo.Caption := RS_RANGE_LO_UNSUPPORTED;
    miRangeColor.Enabled := false;
  end;

  if not api.active then
    Exit;

//  pnOffRange.width := clientwidth div 4;
  pnOffRange.height := clientheight div 10;
 // pnOffRange.left := 0;
 // pnOffRange.top := 0;
  pnOffRange.Font.Size := 7 + pnOffRange.Height div 5;

  scaleLbl(lVal);

  lDiff.Width := ClientWidth;

  lAgo.top := 1 + IfThen(pnOffRange.Visible, pnOffRange.height, 3); // Move the icon

  lAgo.Height := ClientHeight div 9;
  lDiff.Height := lAgo.Height - 10;
  lArrow.Height := ClientHeight div 4;

  lDiff.top := ClientHeight-lDiff.Height + 1;

  scaleLbl(lDiff);
  scaleLbl(lAgo);
  scaleLbl(lArrow);

  PlaceTrendDots(bgs);
  for dot in TrendDots do begin
    dot.AutoSize := false;
    dot.height := ClientWidth div 7;
    dot.width := ClientWidth div 7;
    ScaleLbl(dot);
  end;
end;

// Update remote on timer
procedure TfBG.tMainTimer(Sender: TObject);
begin
  update;
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
procedure TfBG.update;
var
  b: BGReading;
  i: int64;
begin
  native.start;
  lastup := 0;
  // If not looking for new values, the last reading is unknown
  if not tAgo.Enabled then
    lAgo.Caption :=  '🕑 ' + RS_UNKNOWN_TIME;

  // Fetch current readings
  if api = nil then
    Exit;

  bgs := api.getReadings(MAX_MIN, 25);
  if Length(bgs) < 1 then
  begin
    ShowMessage(RS_NO_BACKEND);
    Exit;
  end;

  // Call the new method to place the points
  PlaceTrendDots(bgs);

  // Update other GUI elements based on the latest reading
  b := bgs[Low(bgs)];
  if not privacyMode then
  begin
    if b.val > 400 then
      lVal.Caption := RS_HIGH
    else
    if b.val < 40 then
      lVal.Caption := RS_LOW
    else
      lVal.Caption := b.format(un, BG_MSG_SHORT, BGPrimary)

  end
  else
    lVal.Caption := '';
  lDiff.Caption := b.format(un, BG_MSG_SIG_SHORT, BGDelta);
  lArrow.Caption := b.trend.Img;
  lVal.Font.Style := [];

  // Log latest reading
  LogMessage(Format(RS_LATEST_READING, [b.val, DateTimeToStr(b.date)]));

  // Set next update time
  tMain.Enabled := false;
  i := SecondsBetween(b.date, now); // Seconds from last
  i := min(BG_REFRESH,  // 5 min
    BG_REFRESH-(i*1000) // 5 minutes minus time from last check
    ); // Minimal 5 min to next check

  i := max(120000, i); // Don't allow too small refresh time. Now we have a time between 2-5 mins

  tMain.Interval := i;
  tMain.Enabled := true;
  miRefresh.Caption := Format(RS_REFRESH, [TimeToStr (b.date), TimeToStr(IncMilliSecond(Now, i))]);

  // Check if the latest reading is fresh
  if MinutesBetween(Now, b.date) > DATA_FRESHNESS_THRESHOLD_MINUTES then
  begin
//    lDiff.Caption := TimeToStr(b.date) + ' (' + MinutesBetween(Now, b.date).ToString + ' min)';
    tMissed.OnTimer(tMissed);
    lVal.Font.Style := [fsStrikeOut];
    fBG.Color := clBlack;
    lVal.Font.Color := clWhite;
    {$ifdef lclqt6}
  //    if assigned(ffloat) then
//        fFloat.lvl := bgoff;
    {$endif}
    tMissed.Enabled := true;
    Exit;
  end;
  tMissed.Enabled := false;

  bg_alert := true;
  // Set background color based on the latest reading
  if b.val >= api.cgmHi then
  begin
    fBG.Color := bg_color_hi;
    {$ifdef LCLQt6}
  //    if assigned(fFloat) then
//        ffloat.lvl := BGHigh;
    {$endif}
//    with TrndiNative.create  do
      if not bg_alert then
        native.attention(Format(RS_WARN_BG_HI, [lVal.Caption]));
  end
  else
  if b.val <= api.cgmLo then
  begin
    fBG.Color := bg_color_lo;
    {$ifdef LCLQt6}
  //    if assigned(fFloat) then
//        ffloat.lvl := BGLOW;
    {$endif}
//    with TrndiNative.create  do
      if not bg_alert then
        native.attention(Format(RS_WARN_BG_LO, [lVal.Caption]));
  end
  else
  begin
    bg_alert := false;
    fBG.Color := bg_color_ok;
    // Check personalized limit

    if (b.val >= api.cgmHi) or (b.val <= api.cgmLo) then
    begin
      pnOffRange.Visible := false; // block off elses
      if Assigned(fFloat) then
      begin
        ffloat.lRangeDown.Visible := false;
        ffloat.lRangeUp.Visible := false;
      end;
    end
    else
    if b.val <= api.cgmRangeLo then
    begin
      pnOffRange.Color := bg_rel_color_lo;
      pnOffRange.Font.Color := bg_rel_color_lo_txt;
      pnOffRange.Visible := true;
      pnOffRange.Caption := Format('↧ %s ↧', [RS_OFF_LO]);
      if Assigned(fFloat) then
      begin
        ffloat.lRangeDown.Visible := true;
        ffloat.Font.color := bg_rel_color_lo_txt;
      end;
    end
    else
    if b.val >= api.cgmRangeHi then
    begin
      pnOffRange.Color := bg_rel_color_hi;
      pnOffRange.Font.Color := bg_rel_color_hi_txt;
      pnOffRange.Visible := true;
      pnOffRange.Caption := Format('↥ %s ↥', [RS_OFF_HI]);
      if Assigned(fFloat) then
      begin
        ffloat.lRangeUp.Visible := true;
        ffloat.Font.color := bg_rel_color_hi_txt;
      end;
    end;
  end;
  lastup := Now;
  if privacyMode then
    if fBG.Color = bg_color_hi then
      lVal.Caption := '⭱'
    else
    if fBG.Color =  bg_color_lo then
      lVal.Caption := '⭳'
    else
      lVal.Caption := '✓';

  tAgo.Enabled := true;
  tAgo.OnTimer(self);
  Self.OnResize(lVal);

  if pnOffRange.Visible and miRangeColor.Checked then
    fBG.Color := pnOffRange.Color;

  if Assigned(fFloat) then
  begin
    fFloat.Color := fBg.Color;
    fFloat.lVal.Caption := lval.Caption;
    fFloat.lArrow.Caption := lArrow.Caption;
  end;
  lVal.Font.Color := ifThen(IsLightColor(fBG.color), DarkenColor(fbg.Color, 0.5), LightenColor(fBG.Color, 0.3));
  lArrow.Font.Color := ifThen(IsLightColor(fBG.color), DarkenColor(fbg.Color, 0.5), LightenColor(fBG.Color, 0.3));
  lDiff.Font.Color := ifThen(IsLightColor(fBG.color), DarkenColor(fbg.Color, 0.6), LightenColor(fBG.Color, 0.4));
  lAgo.Font.Color := ifThen(IsLightColor(fBG.color), DarkenColor(fbg.Color, 0.6), LightenColor(fBG.Color, 0.4));

  with native do
  begin
    setBadge(lVal.Caption);
    done;
  end;

end;

// PlaceTrendDots method to map readings to TrendDots
procedure TfBG.PlaceTrendDots(const Readings: array of BGReading);
var
  SortedReadings: array of BGReading;
  i: integer;
  slotIndex: integer;
  l: TLabel;
  slotStart, slotEnd: TDateTime;
  reading: BGReading;
  found: boolean;
  labelNumber: integer;
begin
  if Length(Readings) = 0 then
    Exit;

  // Copy Readings to SortedReadings
  SetLength(SortedReadings, Length(Readings));
  for i := 0 to High(Readings) do
    SortedReadings[i] := Readings[i];

  // Sort SortedReadings in descending order based on date (latest first)
  SortReadingsDescending(SortedReadings);

  // Check if the latest reading is fresh
  if (MinutesBetween(Now, SortedReadings[0].date) > DATA_FRESHNESS_THRESHOLD_MINUTES) then
  begin
    // Hide the last label if the latest reading is outdated
    if Assigned(TrendDots[10]) then
    begin
      TrendDots[10].Visible := false;
      LogMessage('TrendDots[10] hidden due to outdated reading.');
    end;
  end
  else
  if Assigned(TrendDots[10]) then
  begin
    TrendDots[10].Visible := true;
    LogMessage('TrendDots[10] shown as latest reading is fresh.');
  end// Ensure the last label is visible if the latest reading is fresh
  ;

  // Iterate through each time interval and corresponding label
  for slotIndex := 0 to NUM_DOTS - 1 do
  begin
    // Define the start and end time for the interval
    slotEnd := IncMinute(Now, -INTERVAL_MINUTES * slotIndex);
    slotStart := IncMinute(slotEnd, -INTERVAL_MINUTES);

    found := false;

    // Search through the readings to find the latest one that falls within the interval
    for i := 0 to High(SortedReadings) do
    begin
      reading := SortedReadings[i];
      if (reading.date <= slotEnd) and (reading.date > slotStart) then
      begin
        // Map slotIndex to label number (0 -> lDot10, 1 -> lDot9, ..., 9 -> lDot1)
        labelNumber := NUM_DOTS - slotIndex;
        l := TrendDots[labelNumber];

        if Assigned(l) then
        begin
          // Update label properties based on the reading
          l.Visible := true;
          l.Hint := reading.format(un, BG_MSG_SHORT, BGPrimary);
          l.Caption := DOT_GRAPH; // Or another symbol
          setPointHeight(l, reading.convert(mmol));

          // Set colors based on the value
          if reading.val >= api.cgmHi then
            l.Font.Color := bg_color_hi_txt
          else
          if reading.val <= api.cgmLo then
            l.Font.Color := bg_color_lo_txt
          else
          begin
            l.Font.Color := bg_color_ok_txt;
            if reading.val <= api.cgmRangeLo then
              l.Font.Color := bg_rel_color_lo_txt
            else
            if reading.val >= api.cgmRangeHi then
              l.Font.Color := bg_rel_color_hi_txt;
          end;

          LogMessage(Format('TrendDots[%d] updated with reading at %s (Value: %.2f).', [labelNumber, DateTimeToStr(reading.date), reading.val]));
        end;

        found := true;
        Break; // Move to the next time interval
      end;
    end;

    // If no reading was found within the interval, hide the label
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

  // Adjust the layout after updating the labels
  // FormResize(Self); <-- we need to call this manually, or we get an infinite loop
end;

// SetPointHeight procedure
procedure SetPointHeight(L: TLabel; value: single);
var
  Padding, UsableHeight, Position: integer;
begin
  padding := 0;
  UsableHeight := 0;
  if (Value >= 2) and (Value <= 22) then
  begin
    Padding := Round(fBG.ClientHeight * 0.1);
    // 10% padding
    UsableHeight := fBG.ClientHeight - 2 * Padding;

    // Calculate placement, respecting padding
    Position := Padding + Round((Value - 2) / 20 * UsableHeight);

    // Clamp Position within usable range
    if Position < Padding then
      Position := Padding
    else
    if Position > (Padding + UsableHeight) then
      Position := Padding + UsableHeight;

    L.Top := fBG.ClientHeight - Position;
    // Optional: Log the vertical position if label index is available
  end
  else
  if Value < 2 then
    l.top := UsableHeight+2
  else
    l.top := padding-2;
//    ShowMessage('Cannot draw graph points outside 2 and 22');
end;




end.
