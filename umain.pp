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

interface

uses
trndi.strings, LCLTranslator, Classes, Menus, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
trndi.api.dexcom, trndi.api.nightscout, trndi.types, math, DateUtils, FileUtil,
{$ifdef TrndiExt}
trndi.Ext.Engine, trndi.Ext.jsfuncs,
{$endif}
LazFileUtils, uconf, trndi.native, Trndi.API, trndi.api.xDrip,{$ifdef DEBUG} trndi.api.debug,{$endif}
StrUtils, TouchDetection;

type
  // Procedures which are applied to the trend drawing
TTrendProc = procedure(l: TLabel; c, ix: integer) of object;
TTrendProcLoop = procedure(l: TLabel; c, ix: integer; ls: array of TLabel) of object;

  { TfBG }

TfBG = class(TForm)
  lAgo:TLabel;
  miExit:TMenuItem;
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
  procedure FormMouseLeave(Sender:TObject);
  procedure FormMouseMove(Sender:TObject;Shift:TShiftState;X,Y:integer);
  procedure FormResize(Sender: TObject);
  procedure lArrowClick(Sender:TObject);
  procedure lDiffDblClick(Sender: TObject);
  procedure lgMainClick(Sender: TObject);
  procedure lValClick(Sender: TObject);
  procedure lValMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  procedure lValMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  procedure lValStartDrag(Sender: TObject; var DragObject: TDragObject);
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
private
    // Array to hold references to lDot1 - lDot10
  TrendDots: array[1..10] of TLabel;

  procedure update;
  procedure PlaceTrendDots(const Readings: array of BGReading);
  procedure actOnTrend(proc: TTrendProc);
  procedure actOnTrend(proc: TTrendProcLoop);
  procedure setDotWidth(l: TLabel; c, ix: integer; ls: array of TLabel);
  procedure HideDot(l: TLabel; c, ix: integer);
  procedure ResizeDot(l: TLabel; c, ix: integer);
  procedure ExpandDot(l: TLabel; c, ix: integer);
  {$ifdef TrndiExt}
  procedure LoadExtensions;
  {$endif}
public

end;

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
DOT_FRESH = '☉';
var
bg_alert: boolean = false; // If the BG is high/low since before, so we don't spam notifications

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
begin
  {$ifdef Linux}
  s := GetLinuxDistro;
  if (Pos('ID=fedora', s) > -1) then
    s := 'Poppins'
  else
  if (Pos('ID=ubuntu', s) > -1) then
    s := 'Ubuntu'
  else
    s := 'default';
  fBG.Font.Name := s;

  IsRaspberry := false;
  if (Pos('ID=debian', s) > -1) then
    IsRaspberry := FileExists('/etc/rpi-issue');
  {$endif}

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

  with TrndiNative.Create do
  begin
    HasTouch :=  HasTouchScreen(HasMultiTouch);
    if HasMultiTouch then
      touchHelper := TTouchDetector.Create;
    lang := GetSetting('locale', '');

    SetDefaultLang(lang,'lang');
  // Idea for using multiple person/account support
    username := GetSetting('users.names','');
    if username <> '' then
      with TStringList.Create do
      begin
        AddCommaText(username);

        i := InputCombo('User', 'Trndi found multiple accounts. Please choose one for this instance', ToStringArray);
        if i > -1 then
        begin
          username := strings[i];
          fbg.Caption := Format(RS_USER_CAPTION, [username, fBG.Caption]);
          username := username+'_';
        end
        else
          username := '';
        Free;
      end// Load possible other users
    ;


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


    if not api.Connect then
    begin
      ShowMessage(api.ErrorMsg);
      tMain.Enabled := false;
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

  update;
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
begin
  {$ifdef TrndiExt}
  TTrndiExtEngine.ReleaseInstance;
  {$endif}
  for i := 0 to fbg.ComponentCount-1 do
    if fbg.Components[i] is TTimer then
      (fbg.Components[i] as TTimer).Enabled := false; // Shutting down stuff will cause pointer exceptions if we dont stop these first

  api.Free;
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
    l.Caption := IfThen(gnow, l.Hint, DOT_GRAPH);

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
  end;
end;

procedure TfBG.lArrowClick(Sender:TObject);
begin

end;

// Handle full screen toggle on double-click
procedure TfBG.lDiffDblClick(Sender: TObject);
begin
  if fBG.WindowState = wsMaximized then
  begin
    fBG.WindowState := wsNormal;
    fBG.FormStyle := fsNormal;
    fBG.BorderStyle := bsSizeable;
  end
  else
  begin
    fBG.WindowState := wsFullScreen;
    fBG.FormStyle := fsStayOnTop;
    fBG.BorderStyle := bsNone;
  end;
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

procedure TfBG.miBordersClick(Sender:TObject);
begin
  miBorders.Checked := not miBorders.Checked;
  if miBorders.checked then
    self.BorderStyle := bsNone
  else
    BorderStyle := bsSizeToolWin;
end;

procedure TfBG.miExitClick(Sender:TObject);
begin
  if MessageDlg(RS_QUIT_CAPTION, RS_QUIT_MSG, mtWarning, [mbYes, mbNo], '') = mrYes then
    Application.Terminate;
end;

// Force update on menu click
procedure TfBG.miForceClick(Sender: TObject);
begin
  update;
end;

// Explain limit menu click
procedure TfBG.miLimitExplainClick(Sender: TObject);
begin
  MessageDlg('Trndi', RS_LIMIT_EXPLAIN_TEXT, mtInformation, [mbOK], '');
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
  i: integer;
  s: string;
begin
  with TfConf.Create(self) do
    with TrndiNative.Create do
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
      begin
    //    fshi.DecimalPlaces := 1;
     //   fslo.DecimalPlaces := 1;
//        fsHi.Value := fsHi.Value / 18;
  //      fsLo.Value := fsLo.Value / 18;
        rbUnitClick(self);
      end;

      cbCust.Checked := GetIntSetting(username+'override.enabled', 0) = 1;
      fsHi.Enabled :=  cbCust.Checked;
      fsLo.Enabled :=  cbCust.Checked;
      if cbCust.Checked then
        tbAdvanced.Checked := true;

      {$ifdef TrndiExt}
      eExt.Text := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
      {$else}
      eExt.Text := '- Built Without Support -';
      eExt.Enabled := false;
      {$endif}
      cbPrivacy.Checked := GetSetting(username +'ext.privacy', '0') = '1';
      ShowModal;
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

  lAgo.Caption := '🕑 ' + Format(RS_LAST_UPDATE, [min]);
end;

procedure TfBG.tEdgesTimer(Sender:TObject);
begin

end;

procedure TfBG.tResizeTimer(Sender:TObject);
procedure scaleLbl(ALabel: TLabel);
  var
    MaxWidth, MaxHeight: integer;
    TestSize, MaxFontSize: integer;
    TextWidth, TextHeight: integer;
  begin
    // Set the maximum feasible font size
    MaxFontSize := 150;

    // Set the maximum feasible width and height
    MaxWidth := ALabel.Width;
    MaxHeight := ALabel.Height;

    // Check if the font will fit
    for TestSize := 1 to MaxFontSize do
    begin
      ALabel.Font.Size := TestSize;
      TextWidth := ALabel.Canvas.TextWidth(ALabel.Caption);
      TextHeight := ALabel.Canvas.TextHeight(ALabel.Caption);

      // Exit if the font won't fit
      if (TextWidth > MaxWidth) or (TextHeight > MaxHeight) then
      begin
        ALabel.Font.Size := TestSize - 1;
        Exit;
      end;
    end;

    // If we never exited, set the max feasible size
    ALabel.Font.Size := MaxFontSize;
  end;

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
    miRangeLo.Caption := RS_RANGE_LO_UNSUPPORTED;

  if not api.active then
    Exit;

//  pnOffRange.width := clientwidth div 4;
  pnOffRange.height := clientheight div 10;
 // pnOffRange.left := 0;
 // pnOffRange.top := 0;
  pnOffRange.Font.Size := 7 + pnOffRange.Height div 5;

  scaleLbl(lVal);
  scaleLbl(lArrow);
  lDiff.Width := ClientWidth;
  lDiff.Height := lVal.Height div 7;
  lDiff.top := 1 + IfThen(pnOffRange.Visible, pnOffRange.height, 3); // Move the icon

  lAgo.height := max(10, round(lDiff.height / 1.7));
  lAgo.top := ldiff.top;
  scaleLbl(lDiff);


  PlaceTrendDots(bgs);
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
  lastup := 0;
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
      else if b.val < 40 then
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
    tMissed.Enabled := true;
    Exit;
  end;
  tMissed.Enabled := false;

  bg_alert := true;
  // Set background color based on the latest reading
  if b.val >= api.cgmHi then
  begin
    fBG.Color := bg_color_hi;
    with TrndiNative.create  do
      if not bg_alert then
        attention(Format(RS_WARN_BG_HI, [lVal.Caption]));
  end
  else
  if b.val <= api.cgmLo then
  begin
    fBG.Color := bg_color_lo;
    with TrndiNative.create  do
      if not bg_alert then
        attention(Format(RS_WARN_BG_LO, [lVal.Caption]));
  end
  else
  begin
    bg_alert := false;
    fBG.Color := bg_color_ok;
    // Check personalized limit

    if (b.val >= api.cgmHi) or (b.val <= api.cgmLo) then
      pnOffRange.Visible := false // block off elses
    else
    if b.val <= api.cgmRangeLo then
    begin
      pnOffRange.Color := bg_rel_color_lo;
      pnOffRange.Font.Color := bg_rel_color_lo_txt;
      pnOffRange.Visible := true;
    end
    else
    if b.val >= api.cgmRangeHi then
    begin
      pnOffRange.Color := bg_rel_color_hi;
      pnOffRange.Font.Color := bg_rel_color_hi_txt;
      pnOffRange.Visible := true;
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
  else if Value < 2 then
    l.top := UsableHeight+2
  else
    l.top := padding-2;
//    ShowMessage('Cannot draw graph points outside 2 and 22');
end;


end.
