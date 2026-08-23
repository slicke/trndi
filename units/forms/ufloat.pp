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

unit ufloat;

{$mode ObjFPC}{$H+}

{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}



interface

uses
Classes, ExtCtrls, Menus, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs,
LCLIntf, LCLType, InterfaceBase, trndi.native, utrendarrow
{$IFDEF DARWIN},
CocoaAll
{$ENDIF}
{$IFDEF LCLQT6},
qt6, qtwidgets
{$ENDIF};

type

  { TfFloat }

TfFloat = class(TForm)
  lTime: TLabel;
  lArrow: TLabel;
  lRangeDown: TLabel;
  lRangeUp: TLabel;
  lVal: TLabel;
  MenuItem1: TMenuItem;
  miFontMain: TMenuItem;
  miSep: TMenuItem;
  miFontBlack: TMenuItem;
  miFontWhite: TMenuItem;
  miFontColor: TMenuItem;
  miMain: TMenuItem;
  miClock: TMenuItem;
  miCustomSize: TMenuItem;
  Separator1: TMenuItem;
  miXL: TMenuItem;
  miSmall: TMenuItem;
  miNormal: TMenuItem;
  miBig: TMenuItem;
  miSIze: TMenuItem;
  miSplit: TMenuItem;
  miVisible: TMenuItem;
  miOp100: TMenuItem;
  miOp25: TMenuItem;
  miOp50: TMenuItem;
  miOp75: TMenuItem;
  miCustomVisible: TMenuItem;
  miSplit1: TMenuItem;
  pMain: TPopupMenu;
  pnMultiUser: TPanel;
  tClock: TTimer;
  procedure FormCreate(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
  procedure FormMouseMove({%H-}Sender: TObject; {%H-}Shift: TShiftState; X, Y: integer);
  procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
  procedure FormResize({%H-}Sender: TObject);
  procedure FormShow({%H-}Sender: TObject);
  procedure MenuItem1Click({%H-}Sender: TObject);
  procedure miClockClick({%H-}Sender: TObject);
  procedure miCustomSizeClick({%H-}Sender: TObject);
  procedure miCustomVisibleClick({%H-}Sender: TObject);
  procedure miFontBlackClick({%H-}Sender: TObject);
  procedure miFontWhiteClick({%H-}Sender: TObject);
  procedure miFontMainClick({%H-}Sender: TObject);
  procedure miMainClick({%H-}Sender: TObject);
  procedure miNormalClick({%H-}Sender: TMenuItem);
  procedure miNormalClick({%H-}Sender: TObject);
  procedure miOp100Click({%H-}Sender: TObject);
  procedure tClockTimer({%H-}Sender: TObject);
private
  FDragStartX: integer;
  FDragStartY: integer;
  FDraggingWin: boolean;
  FTrendArrow: TTrendArrow; // Rotating trend arrow overlay (mirrors the main window)
  procedure SetFormOpacity(Opacity: double);
  procedure ApplyRoundedCorners;
  procedure ApplyClock(AEnabled: boolean);
  procedure SetFixedFontColor(AColor: TColor);
  procedure SyncSizeMenu;
  procedure SyncOpacityMenu(AOpacity: single);
public
  {** Mirror the main window's rotating trend arrow.
      @param(AEnabled Whether the rotating arrow replaces the glyph.)
      @param(AAngle Rotation in degrees (0 = flat, + = up, - = down).)
      @param(AColor Stroke colour for the arrow.) }
  procedure SetTrendArrow(AEnabled: boolean; AAngle: single; AColor: TColor);
end;

resourcestring
RS_CUSTOM_OP = 'You can use shift+<number> to manually set visibility (0 = 100% visible)';
RS_CUSTOM_SIZE =
  'You can hold down shift and plus (+) or minus (-) to change the window size';

var
fFloat: TfFloat;
SettingsNative: TrndiNative = nil;


implementation

{$R *.lfm}

procedure ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
customTl: TTextLayout = tlCenter);
var
  Low, High, Mid: integer;
  MaxWidth, MaxHeight: integer;
  TextWidth, TextHeight: integer;
  OptimalSize: integer;
  bmp: TBitmap;
begin
  // Check basic visibility conditions
  if not ALabel.Visible then
    ALabel.Visible := true;

  if ALabel.Caption = '' then
    Exit; // No text to display

  // Check that the label has size
  if (ALabel.Width <= 0) or (ALabel.Height <= 0) then
  begin
    ALabel.Width := 100;
    ALabel.Height := 30;
  end;

  // Set correct formatting
  ALabel.AutoSize := false;
  ALabel.WordWrap := false;
  ALabel.Alignment := customAl;
  ALabel.Layout := customTl;

  // Ensure text is visible against the background
  if ALabel.Font.Color = ALabel.Color then
    ALabel.Font.Color := clBlack;

  // Maximum width and height for the text
  MaxWidth := ALabel.Width - 4; // Small padding
  MaxHeight := ALabel.Height - 4;

  // Perform binary search to find optimal font size
  Low := 1;
  High := 150;
  OptimalSize := 1;

  // Measure on a temp TBitmap. TLabel.Canvas outside paint is unsafe on the
  // Cocoa widgetset (TCocoaContext.SetAntialiasing SIGABRTs on a nil ctx).
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(ALabel.Font);

    while Low <= High do
    begin
      Mid := (Low + High) div 2;
      bmp.Canvas.Font.Size := Mid;

      TextWidth := bmp.Canvas.TextWidth(ALabel.Caption);
      TextHeight := bmp.Canvas.TextHeight(ALabel.Caption);

      if (TextWidth <= MaxWidth) and (TextHeight <= MaxHeight) then
      begin
        OptimalSize := Mid;
        Low := Mid + 1;
      end
      else
        High := Mid - 1;
    end;
  finally
    bmp.Free;
  end;

  // Set the optimal font size
  ALabel.Font.Size := OptimalSize;

  // Ensure settings are applied
  ALabel.Refresh;
end;

// Local helpers to read/write Trndi settings without referencing the global
// `native` variable (which isn't visible in this unit).
function GetSettingsNative: TrndiNative;
var
  activeUser: string;
begin
  if SettingsNative = nil then
  begin
    SettingsNative := TrndiNative.Create;
    SettingsNative.noFree := true;
  end;

  // Keep float settings scoped to the active user selected in the main form.
  activeUser := SettingsNative.GetRootSetting('users.active', '');
  if SettingsNative.configUser <> activeUser then
    SettingsNative.configUser := activeUser;

  Result := SettingsNative;
end;

function ReadIntSetting(const key: string; def: integer): integer;
begin
  Result := GetSettingsNative.GetIntSetting(key, def);
end;

function ReadFloatSetting(const key: string; def: single): single;
begin
  Result := GetSettingsNative.GetFloatSetting(key, def);
end;

procedure SaveSetting(const key: string; val: integer);
begin
  GetSettingsNative.SetSetting(key, val);
end;

procedure SaveFloatSetting(const key: string; val: single);
begin
  GetSettingsNative.SetFloatSetting(key, val, false);
end;

procedure TfFloat.FormCreate({%H-}Sender: TObject);
{$IFDEF LCLQt6}
var
  QtWidget: TQtWidget;
  style: string;
{$endif}
begin
  {$IFDEF LCLQt6}
  if HandleAllocated then
  begin
    QtWidget := TQtWidget(Handle);
    if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
    begin
      QtWidget.setAttribute(QtWA_TranslucentBackground, true);
      QtWidget.setWindowFlags(QtWidget.windowFlags or QtFramelessWindowHint);
      style := 'border-radius:15px; background-color:rgba(255,255,255,200);';
      QWidget_setStyleSheet(QtWidget.Widget, PWideString(style));
    end;
  end;
  {$ENDIF}

end;

procedure TfFloat.ApplyRoundedCorners;
var
  {$IF DEFINED(DARWIN)}
  NSViewHandle: NSView;
  NSWin: NSWindow;
  Mask: NSBezierPath;
  {$ELSEIF DEFINED(LCLQT6)}
  StyleStr: widestring;
  {$ELSE}
  ABitmap: TBitmap;
  {$ENDIF}
begin
  {$IF DEFINED(DARWIN)}
  try
    // Get NSView + NSWindow from handle
    if HandleAllocated then
    begin
      NSViewHandle := NSView(Handle);
      if Assigned(NSViewHandle) then
      begin
        NSWin := NSViewHandle.window;
        if Assigned(NSWin) then
        begin
          // Set transparency
          NSWin.setOpaque(false);
          NSWin.setBackgroundColor(NSColor.clearColor);

          // Make corners roundable
          NSViewHandle.setWantsLayer(true);

          // Use a bezier path to round corners
          if Assigned(NSViewHandle.layer) then
          begin
            NSViewHandle.layer.setCornerRadius(10.0); // Set to 10 roundness
            NSViewHandle.layer.setMasksToBounds(true);
          end;
        end;
      end;
    end;
  except
    // Ignore any errors
  end;
  {$ELSEIF DEFINED(LCLQT6)}
  StyleStr := 'border-radius: 10px; background-color: rgba(240, 240, 240, 255);';
  Self.BorderStyle := bsNone; // Remove border
  if HandleAllocated then
    QWidget_setStyleSheet(TQtWidget(Handle).Widget, @stylestr);
  {$ELSE}
  Self.BorderStyle := bsNone; // Remove border
  // Use LCL stuff when Windows (or not Qt really)
  try
    ABitmap := TBitmap.Create;
    ABitmap.Monochrome := true;
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    // Black will become transparent
    ABitmap.Canvas.Brush.Color := clBlack;
    ABitmap.Canvas.FillRect(0, 0, Width, Height);
    // Paint the form itself in white
    ABitmap.Canvas.Brush.Color := clWhite;
    ABitmap.Canvas.RoundRect(0, 0, Width, Height, 20, 20);

    // Shape the form
    SetShape(ABitmap);
  finally
    ABitmap.Free;
  end;
  {$ENDIF}
end;

procedure TfFloat.SetFormOpacity(Opacity: double);
{$IF DEFINED(LCLQt6) OR DEFINED(DARWIN)}
var
{$endif}
  {$IFDEF DARWIN}
  NSViewHandle: NSView;
  NSWin: NSWindow;
  {$ENDIF}
  {$IFDEF LCLQt6}
  StyleStr: widestring;
  {$ENDIF}
begin
  {$IFDEF DARWIN}
  if HandleAllocated then
  try
    NSViewHandle := NSView(Handle);
    if Assigned(NSViewHandle) then
    begin
      NSWin := NSViewHandle.window;
      if Assigned(NSWin) then
        NSWin.setAlphaValue(Opacity);
    end;
  except
      // Ignore any errors
  end;
  {$ELSE}
  {$IFDEF LCLQt6}
  if HandleAllocated then
  begin
      // For Qt6, use style sheets to set opacity
    StyleStr := Format('background-color: rgba(240, 240, 240, %.0f);', [Opacity * 255]);
    QWidget_setStyleSheet(TQtWidget(Handle).Widget, @StyleStr);
  end;
  {$ENDIF}
  // Standard LCL approach for other platforms
  AlphaBlend := Opacity < 1.0;
  AlphaBlendValue := Round(Opacity * 255);
  {$ENDIF}
  // Persist opacity for next run
  SaveFloatSetting('ux.float.opacity', Opacity);
end;

procedure TfFloat.FormShow(Sender: TObject);
var
  storedH, storedW, storedLeft, storedTop: integer;
  storedOp: single;
begin
  // Do not auto-center here; respect current position so user dragging stays stable
  ApplyRoundedCorners;

  // Restore size & position if available (do this BEFORE any default size code)
  storedH := ReadIntSetting('size.float.height', Height);
  storedW := ReadIntSetting('size.float.width', Width);
  storedLeft := ReadIntSetting('position.float.left', Left);
  storedTop := ReadIntSetting('position.float.top', Top);

  Height := storedH;
  Width := storedW;
  Left := storedLeft;
  Top := storedTop;

  // Ensure labels/fonts are scaled for the restored size
  FormResize(Self);

  // Set the opacity (persisted or default)
  storedOp := ReadFloatSetting('ux.float.opacity', 0.5);
  SetFormOpacity(storedOp);

  // Reflect the restored size/opacity in the menu checkmarks
  SyncSizeMenu;
  SyncOpacityMenu(storedOp);

  // Restore the font color choice (0 = black, 1 = white, 2 = follow main window)
  case ReadIntSetting('ux.float.fontcolor', 0) of
  1:
    begin
      miFontWhite.Checked := true;
      SetFixedFontColor(clWhite);
    end;
  2:
    miFontMain.Checked := true; // colors arrive with the next main-window sync
  else
    begin
      miFontBlack.Checked := true;
      SetFixedFontColor(clBlack);
    end;
  end;

  // Restore the clock
  ApplyClock(ReadIntSetting('ux.float.clock', 0) = 1);
end;

procedure TfFloat.SyncSizeMenu;
begin
  miXL.Checked := Height = Screen.DesktopHeight div 5;
  miBig.Checked := Height = Screen.DesktopHeight div 10;
  miNormal.Checked := Height = Screen.DesktopHeight div 25;
  miSmall.Checked := Height = Screen.DesktopHeight div 50;
  miCustomSize.Checked := not (miXL.Checked or miBig.Checked or miNormal.Checked or
    miSmall.Checked);
end;

procedure TfFloat.SyncOpacityMenu(AOpacity: single);

  function Near(v: single): boolean;
  begin
    Result := Abs(AOpacity - v) < 0.01;
  end;

begin
  miOp25.Checked := Near(0.25);
  miOp50.Checked := Near(0.5);
  miOp75.Checked := Near(0.75);
  miOp100.Checked := Near(1);
  miCustomVisible.Checked := not (miOp25.Checked or miOp50.Checked or
    miOp75.Checked or miOp100.Checked);
end;

procedure TfFloat.MenuItem1Click({%H-}Sender: TObject);
begin
  Hide;
end;

procedure TfFloat.ApplyClock(AEnabled: boolean);
begin
  miClock.Checked := AEnabled;
  lTime.Visible := AEnabled;
  tClock.Enabled := AEnabled;
  if AEnabled then
    tClockTimer(tClock);
end;

procedure TfFloat.miClockClick(Sender: TObject);
begin
  ApplyClock(not miClock.Checked);
  SaveSetting('ux.float.clock', ord(miClock.Checked));
end;

procedure TfFloat.miCustomSizeClick(Sender: TObject);
begin
  ShowMessage(RS_CUSTOM_SIZE);
end;

procedure TfFloat.miCustomVisibleClick(Sender: TObject);
begin
  ShowMessage(RS_CUSTOM_OP);
end;

procedure TfFloat.SetFixedFontColor(AColor: TColor);
begin
  lVal.Font.Color := AColor;
  lArrow.Font.Color := AColor;
  if Assigned(FTrendArrow) then
    FTrendArrow.ArrowColor := AColor;
end;

procedure TfFloat.miFontBlackClick(Sender: TObject);
begin
  SetFixedFontColor(clBlack);
  SaveSetting('ux.float.fontcolor', 0);
end;

procedure TfFloat.miFontWhiteClick(Sender: TObject);
begin
  SetFixedFontColor(clWhite);
  SaveSetting('ux.float.fontcolor', 1);
end;

procedure TfFloat.miFontMainClick(Sender: TObject);
begin
  // Colors are picked up from the main window on its next sync
  SaveSetting('ux.float.fontcolor', 2);
end;


procedure TfFloat.miMainClick(Sender: TObject);
begin
  if Application.MainForm.Visible then
    Application.MainForm.Hide
  else
    Application.MainForm.Show;
end;

procedure TfFloat.miNormalClick(Sender: TMenuItem);
var
  h: integer;
begin
  h := Height;

  if Sender = miXL then
    h := Screen.DesktopHeight div 5
  else
  if Sender = miBig then
    h := Screen.DesktopHeight div 10
  else
  if Sender = miNormal then
    h := Screen.DesktopHeight div 25
  else
  if Sender = miSmall then
    h := Screen.DesktopHeight div 50;

  Height := h;
  Width := round(Height * 1.55);
  lVal.Width := round(clientwidth * 0.75);
  lArrow.Width := round(clientwidth * 0.25);
  //---
  ApplyRoundedCorners;
  SyncSizeMenu;
  // Persist selected size
  SaveSetting('size.float.height', Height);
  SaveSetting('size.float.width', Width);
end;

procedure TfFloat.miNormalClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    miNormalClick(Sender as TMenuItem);
end;

procedure TfFloat.miOp100Click(Sender: TObject);
var
  i: integer;
  v: double;
begin
  if not TryStrToInt((Sender as TMenuItem).hint, i) then
    Exit;
  v := i / 100;

  SetFormOpacity(v);
  (Sender as TMenuItem).Checked := true;
end;

procedure TfFloat.tClockTimer(Sender: TObject);
begin
  lTime.Caption := FormatDateTime(DefaultFormatSettings.ShortTimeFormat, Now);
  // Re-anchor to the top-right corner; the caption width just changed
  lTime.AdjustSize;
  lTime.Left := ClientWidth - lTime.Width - 8;
  if lTime.Visible = false then
    (Sender as TTimer).Enabled := false;
end;

procedure TfFloat.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  ScreenPt: TPoint;
  DeltaX, DeltaY: integer;
begin
  if FDraggingWin then
  begin
    // Convert to screen coordinates to handle moves from child controls
    if Sender is TControl then
      ScreenPt := (Sender as TControl).ClientToScreen(Point(X, Y))
    else
      ScreenPt := ClientToScreen(Point(X, Y));
    
    // Calculate the delta (how much the mouse moved)
    DeltaX := ScreenPt.X - FDragStartX;
    DeltaY := ScreenPt.Y - FDragStartY;
    
    // Move the window by the delta
    Left := Left + DeltaX;
    Top := Top + DeltaY;
    
    // Update stored position for next move
    FDragStartX := ScreenPt.X;
    FDragStartY := ScreenPt.Y;
  end;
end;

procedure TfFloat.FormMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
  FDraggingWin := false;
  // Persist current position
  SaveSetting('position.float.left', Left);
  SaveSetting('position.float.top', Top);
end;

procedure TfFloat.FormResize(Sender: TObject);
begin
  lVal.Left := 0;
  lVal.Top := 0;
  lVal.Height := ClientHeight;
  lVal.Width := Round(ClientWidth * 0.75);

  lArrow.Left := lVal.Width;
  lArrow.Top := 0;
  lArrow.Height := ClientHeight;
  lArrow.Width := ClientWidth - lVal.Width;

  ScaleLbl(lVal, taLeftJustify, tlCenter);
  ScaleLbl(lArrow, taCenter, tlCenter);

  // Keep the clock tucked into the top-right corner, above the arrow
  lTime.Font.Size := lArrow.Font.Size div 3;
  lTime.AdjustSize;
  lTime.Left := ClientWidth - lTime.Width - 8;
  lTime.Top := 4;

  // Off-range markers live in the top-left corner, mirroring the clock
  lRangeDown.Font.Size := lTime.Font.Size;
  lRangeUp.Font.Size := lTime.Font.Size;
  lRangeDown.AdjustSize;
  lRangeUp.AdjustSize;
  lRangeDown.Left := 8;
  lRangeDown.Top := 4;
  lRangeUp.Left := 8;
  lRangeUp.Top := 4;

  // Keep the rotating arrow overlay tracking lArrow's bounds. ScaleLbl re-shows
  // lArrow, so re-hide the glyph while the vector arrow is active.
  if Assigned(FTrendArrow) then
  begin
    FTrendArrow.BoundsRect := lArrow.BoundsRect;
    if FTrendArrow.Visible then
      lArrow.Visible := false;
  end;
end;

procedure TfFloat.SetTrendArrow(AEnabled: boolean; AAngle: single; AColor: TColor);
begin
  if not AEnabled then
  begin
    if Assigned(FTrendArrow) then
      FTrendArrow.Visible := false;
    lArrow.Visible := lArrow.Caption <> '';
    Exit;
  end;

  if not Assigned(FTrendArrow) then
  begin
    FTrendArrow := TTrendArrow.Create(Self);
    FTrendArrow.Parent := lArrow.Parent;
  end;

  FTrendArrow.ArrowColor := AColor;
  FTrendArrow.BoundsRect := lArrow.BoundsRect;
  FTrendArrow.Angle := AAngle;
  lArrow.Visible := false;
  FTrendArrow.Visible := true;
end;

procedure TfFloat.FormMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
var
  ScreenPt: TPoint;
  {$IFDEF LCLQt6}
  QtWidget: TQtWidget;
  sessionType: string;
  qwin: QWindowH;
  {$ENDIF}
begin
  if Button = mbLeft then
  begin
    {$IFDEF LCLQt6}
    sessionType := GetEnvironmentVariable('XDG_SESSION_TYPE');
    if LowerCase(sessionType) = 'wayland' then
      if HandleAllocated then
      begin
        QtWidget := TQtWidget(Handle);
        if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
        begin
          qwin := QWidget_windowHandle(QtWidget.Widget);
          if qwin <> nil then
            if QWindow_startSystemMove(qwin) then
            begin
              // The compositor swallows the matching mouse-up (KWin), so
              // drop the implicit capture or it sticks to the pressed
              // control and hijacks every later press.
              SetCaptureControl(nil);
              Exit; // compositor handles the move
            end;
        end;
      end;
    {$ENDIF}

    FDraggingWin := true;
    
    // Convert to screen coordinates to handle clicks from child controls
    if Sender is TControl then
      ScreenPt := (Sender as TControl).ClientToScreen(Point(X, Y))
    else
      ScreenPt := ClientToScreen(Point(X, Y));
    
    FDragStartX := ScreenPt.X;
    FDragStartY := ScreenPt.Y;
  end;
end;

procedure TfFloat.FormKeyDown({%H-}Sender: TObject; var Key: word; Shift: TShiftState);
var
  num: double;
begin
  if key = VK_ESCAPE then
  begin
    Hide;
    key := 0;
  end;
  if ((ssShift in Shift) and (Key >= 48) and (Key <= 57)) then
  begin
    num := (key - 48) / 10;
    if num < 0.1 then
      num := 1;

    SetFormOpacity(num);
    miCustomVisible.Checked := true;
  end;

  if ((ssShift in Shift) and (Key in [{$ifdef LINUX}63 (*linux **),{$endif} 187, (* + *) 189, 191 (*mac +*)])) then
  begin
    if key <> 189 then // not -
      Height := Height + 5
    else
      Height := Height - 5;
    miNormalClick(miCustomSize);
  end;
end;

finalization
  FreeAndNil(SettingsNative);

end.
