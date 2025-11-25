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
LCLIntf, LCLType, InterfaceBase
{$IFDEF DARWIN},
CocoaAll
{$ENDIF}
{$IFDEF LCLQT6},
qt6, qtwidgets, qtobjects, qtint
{$ENDIF};

{$ifdef LCLQt6}
type
BGValLevel = (BGHigh, BGLOW, BGOFF);    // Range = depending on API
{$endif}

type

  { TfFloat }

TfFloat = class(TForm)
  lTime: TLabel;
  lArrow: TLabel;
  lRangeDown: TLabel;
  lRangeUp: TLabel;
  lVal: TLabel;
  MenuItem1: TMenuItem;
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
  tTitlebar: TTimer;
  procedure FormCreate(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  procedure FormKeyPress(Sender: TObject; var Key: char);
  procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
  procedure FormMouseEnter(Sender: TObject);
  procedure FormMouseLeave(Sender: TObject);
  procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
  procedure FormPaint(Sender: TObject);
  procedure FormResize(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure lValMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
  procedure MenuItem1Click(Sender: TObject);
  procedure miBigClick(Sender: TObject);
  procedure miClockClick(Sender: TObject);
  procedure miCustomSizeClick(Sender: TObject);
  procedure miCustomVisibleClick(Sender: TObject);
  procedure miHideTitleClick(Sender: TObject);
  procedure miMainClick(Sender: TObject);
  procedure miNormalClick(Sender: TMenuItem);
  procedure miNormalClick(Sender: TObject);
  procedure miOp100Click(Sender: TObject);
  procedure tClockTimer(Sender: TObject);
  procedure tTitlebarTimer(Sender: TObject);
private
  procedure SetFormOpacity(Opacity: double);
  procedure ApplyRoundedCorners;
  procedure CreateRoundedCorners;
public
  {$ifdef LCLQt6}
  lvl: BGValLevel;
  {$endif}
end;

resourcestring
rsCustomOp = 'You can use shift+<number> to manually set visibility (0 = 100% visible)';
rsCustomSize =
  'You can hold down shift and plus (+) or minus (-) to change the window size';

var
fFloat: TfFloat;
PX, PY: integer;
DraggingWin: boolean;


implementation

{$R *.lfm}

procedure ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
customTl: TTextLayout = tlCenter);
var
  Low, High, Mid: integer;
  MaxWidth, MaxHeight: integer;
  TextWidth, TextHeight: integer;
  OptimalSize: integer;
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

  // Set the optimal font size
  ALabel.Font.Size := OptimalSize;

  // Ensure settings are applied
  ALabel.Refresh;
end;

procedure TfFloat.FormCreate(Sender: TObject);
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
//  Self.BorderStyle := bsNone; // Remove border
  self.borderstyle := bsToolWindow;
  if HandleAllocated then
    QWidget_setStyleSheet(TQtWidget(Handle).Widget, @stylestr);
  CreateRoundedCorners;
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
end;

procedure TfFloat.FormShow(Sender: TObject);
begin
  // Do not auto-center here; respect current position so user dragging stays stable
  ApplyRoundedCorners;

  miNormal.Click;
  // Set the opacity
  SetFormOpacity(0.5);
end;

procedure TfFloat.lValMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
end;

procedure TfFloat.MenuItem1Click(Sender: TObject);
begin
  Hide;
end;

procedure TfFloat.miBigClick(Sender: TObject);
begin

end;

procedure TfFloat.miClockClick(Sender: TObject);
begin
  miClock.Checked := not miClock.Checked;
  lTime.Visible := miClock.Checked;
  if lTime.Visible then
  begin
    tClock.OnTimer(tClock);
    tClock.Enabled := true;
  end;
end;

procedure TfFloat.miCustomSizeClick(Sender: TObject);
begin
  ShowMessage(rsCustomSize);
end;

procedure TfFloat.miCustomVisibleClick(Sender: TObject);
begin
  ShowMessage(rsCustomOp);
end;

procedure TfFloat.miHideTitleClick(Sender: TObject);
begin

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
  miBig.Checked := false;
  miNormal.Checked := false;
  miSmall.Checked := false;
  miXL.Checked := false;

  if Sender = miXL then
  begin
    miXL.Checked := true;
    h := Screen.DesktopHeight div 5;
  end
  else
  if Sender = miBig then
  begin
    miBig.Checked := true;
    h := Screen.DesktopHeight div 10;
  end
  else
  if Sender = miNormal then
  begin
    miNormal.Checked := true;
    h := Screen.DesktopHeight div 25;
  end
  else
  if Sender = miSmall then
  begin
    miSmall.Checked := true;
    h := Screen.DesktopHeight div 50;
  end
  else
  if Sender = miCustomSize then
    h := Height;

  Height := h;
  Width := round(Height * 1.55);
  lVal.Width := round(clientwidth * 0.75);
  lArrow.Width := round(clientwidth * 0.25);
  //---
  ApplyRoundedCorners;
end;

procedure TfFloat.miNormalClick(Sender: TObject);
begin

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
  lTime.Caption := FormatDateTime(ShortTimeFormat, Now);
  if lTime.Visible = false then
    (Sender as TTimer).Enabled := false;
end;

procedure TfFloat.tTitlebarTimer(Sender: TObject);
begin
  {$IF NOT DEFINED(DARWIN)}
  // On non-macOS, keep float always borderless; timer does nothing
  tTitlebar.Enabled := False;
  {$ENDIF}
end;

procedure TfFloat.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  ScreenPt: TPoint;
  DeltaX, DeltaY: Integer;
begin
  if DraggingWin then
  begin
    // Convert to screen coordinates to handle moves from child controls
    if Sender is TControl then
      ScreenPt := (Sender as TControl).ClientToScreen(Point(X, Y))
    else
      ScreenPt := ClientToScreen(Point(X, Y));
    
    // Calculate the delta (how much the mouse moved)
    DeltaX := ScreenPt.X - PX;
    DeltaY := ScreenPt.Y - PY;
    
    // Move the window by the delta
    Left := Left + DeltaX;
    Top := Top + DeltaY;
    
    // Update stored position for next move
    PX := ScreenPt.X;
    PY := ScreenPt.Y;
  end;
end;

procedure TfFloat.FormMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
  DraggingWin := false;
  {$IFDEF DARWIN}
  // On macOS, restore borderless immediately after dragging
  BorderStyle := bsNone;
  {$ENDIF}
end;


procedure TfFloat.CreateRoundedCorners;
{$IFDEF LCLQt6}
var
  Path: QPainterPathH;
  Painter: QPainterH;
  QtWidget: TQtWidget;
  XBrush: QBrushH;
  Pen: QPenH;
  WidgetHandle: QWidgetH;
  BlackColor: QColorH;
  PaintDevice: QPaintDeviceH;
  bgcol: QtGlobalColor;
  {$endif}
begin
  {$IFDEF LCLQt6_DISABLED}
  case lvl of
  BGHigh:
    bgcol := QtYellow;
  BGLOW:
    bgcol := Qtblue;
  BGOff:
    bgcol := Qtblack;
  else
    bgcol := QtGreen;
  end;
  // Convert LCL handle to Qt widget
  QtWidget := TQtWidget(Handle);
  WidgetHandle := QtWidget.GetContainerWidget;

  // Ensure the widget has a transparent background
  QWidget_setAttribute(WidgetHandle, QtWA_TranslucentBackground, true);


  // Create QColor for black
  BlackColor := QColor_create(0, 0, 0);

  // Get paint device from widget
  PaintDevice := QWidget_to_QPaintDevice(WidgetHandle);

  // Create painter directly on the widget
  Painter := QPainter_create();
  QPainter_begin(Painter, PaintDevice);

  // Enable antialiasing
  QPainter_setRenderHint(Painter, QPainterAntialiasing, true);

  // Create a path for rounded corners
  Path := QPainterPath_create();
  QPainterPath_addRoundedRect(Path, 0, 0, Width, Height, 20, 20);

  // Create brush for filling
  XBrush := QBrush_create();
  QBrush_setStyle(XBrush, QtSolidPattern);
  // For QBrush_setColor, QtGlobalColor enum is used
  QBrush_setColor(XBrush, bgcol);

  // Create pen for outline
  Pen := QPen_create();
  QPen_setStyle(Pen, QtSolidLine);
  // For QPen_setColor, PQColor (QColorH) is used
  QPen_setColor(Pen, @BlackColor);
  QPen_setWidth(Pen, 1);

  // Draw directly on the widget
  QPainter_setPen(Painter, Pen);
  QPainter_setBrush(Painter, XBrush);
  QPainter_drawPath(Painter, Path);

  // End painting
  QPainter_end(Painter);

  // Clean up
  QPainterPath_destroy(Path);
  QBrush_destroy(XBrush);
  QPen_destroy(Pen);
  QColor_destroy(BlackColor);
  QPainter_destroy(Painter);
  {$ENDIF}
end;



procedure TfFloat.FormPaint(Sender: TObject);
begin
  CreateRoundedCorners;
end;

procedure TfFloat.FormResize(Sender: TObject);
begin
  ScaleLbl(lVal, taLeftJustify, tlCenter);
  ScaleLbl(lArrow, taCenter, tlCenter);
  lTime.Font.Size := lArrow.font.size div 3;
  lTime.left := larrow.left - (lTime.Width div 2);
end;

procedure TfFloat.FormMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
var
  ScreenPt: TPoint;
begin
  if Button = mbLeft then
  begin
    DraggingWin := true;
    
    // Convert to screen coordinates to handle clicks from child controls
    if Sender is TControl then
      ScreenPt := (Sender as TControl).ClientToScreen(Point(X, Y))
    else
      ScreenPt := ClientToScreen(Point(X, Y));
    
    PX := ScreenPt.X;
    PY := ScreenPt.Y;
  end;
end;

procedure TfFloat.FormMouseEnter(Sender: TObject);
begin
  {$IF NOT DEFINED(DARWIN)}
  // On Linux/Windows keep the form borderless; no hover titlebar
  Exit;
  {$ENDIF}
end;

procedure TfFloat.FormMouseLeave(Sender: TObject);
begin
  {$IF NOT DEFINED(DARWIN)}
  // No hover titlebar on non-macOS
  Exit;
  {$ENDIF}
end;

procedure TfFloat.FormKeyPress(Sender: TObject; var Key: char);
begin
end;

procedure TfFloat.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
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

end.
