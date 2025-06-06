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
    lTime:TLabel;
    lArrow:TLabel;
    lRangeDown:TLabel;
    lRangeUp:TLabel;
    lVal: TLabel;
    MenuItem1:TMenuItem;
    miClock:TMenuItem;
    miCustomSize:TMenuItem;
    Separator1:TMenuItem;
    miXL:TMenuItem;
    miSmall:TMenuItem;
    miNormal:TMenuItem;
    miBig:TMenuItem;
    miSIze:TMenuItem;
    miSplit:TMenuItem;
    miVisible: TMenuItem;
    miOp100: TMenuItem;
    miOp25: TMenuItem;
    miOp50: TMenuItem;
    miOp75: TMenuItem;
    miCustomVisible: TMenuItem;
    miSplit1: TMenuItem;
    pMain: TPopupMenu;
    pnMultiUser:TPanel;
    tClock:TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender:TObject);
    procedure FormShow(Sender: TObject);
    procedure lValMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuItem1Click(Sender:TObject);
    procedure miBigClick(Sender:TObject);
    procedure miClockClick(Sender:TObject);
    procedure miCustomSizeClick(Sender:TObject);
    procedure miCustomVisibleClick(Sender: TObject);
    procedure miNormalClick(Sender:TMenuItem);
    procedure miNormalClick(Sender:TObject);
    procedure miOp100Click(Sender: TObject);
    procedure tClockTimer(Sender:TObject);
    procedure tTitlebarTimer(Sender: TObject);
  private
    procedure SetFormOpacity(Opacity: Double);
    procedure ApplyRoundedCorners;
    procedure CreateRoundedCorners;
  public
      {$ifdef LCLQt6}
    lvl: BGValLevel;
  {$endif}
  end;

resourcestring
  rsCustomOp = 'You can use shift+<number> to manually set visibility (0 = 100% visible)';
  rsCustomSize = 'You can hold down shift and plus (+) or minus (-) to change the window size';

var
  fFloat: TfFloat;
  PX, PY: integer;
  DraggingWin: boolean;


implementation

{$R *.lfm}

procedure ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter; customTl: TTextLayout = tlCenter);
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

procedure TfFloat.FormCreate(Sender: TObject);
{$IFNDEF LCLQt6}
begin
{$else}
  var QtWidget: TQtWidget;
    style: string;
begin
  if HandleAllocated then
  begin
    QtWidget := TQtWidget(Handle);
    if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
    begin
      QtWidget.setAttribute(QtWA_TranslucentBackground, True);
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
{$ELSEIF DEFINED(LCLQT6_DISABLED)}
  StyleStr: WideString;
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
          NSWin.setOpaque(False);
          NSWin.setBackgroundColor(NSColor.clearColor);

          // Make corners roundable
          NSViewHandle.setWantsLayer(True);

          // Use a bezier path to round corners
          if Assigned(NSViewHandle.layer) then
          begin
            NSViewHandle.layer.setCornerRadius(10.0); // Set to 10 roundness
            NSViewHandle.layer.setMasksToBounds(True);
          end;
        end;
      end;
    end;
  except
    // Ignore any errors
  end;
  {$ELSEIF DEFINED(LCLQT6_DISABLED)}
    StyleStr := 'border-radius: 10px; background-color: rgba(240, 240, 240, 255);';
  Self.BorderStyle := bsNone; // Remove border
if HandleAllocated then
QWidget_setStyleSheet(TQtWidget(Handle).Widget,
  @stylestr);
  CreateRoundedCorners;
   {$ELSE}
  Self.BorderStyle := bsNone; // Remove border
  // Use LCL stuff when Windows (or not Qt really)
  try
    ABitmap := TBitmap.Create;
    ABitmap.Monochrome := True;
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

procedure TfFloat.SetFormOpacity(Opacity: Double);
{$IFDEF DARWIN}
var
  NSViewHandle: NSView;
  NSWin: NSWindow;
{$ENDIF}
{$IFDEF LCLQt6}
var
  StyleStr: WideString;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  if HandleAllocated then
  begin
    try
      NSViewHandle := NSView(Handle);
      if Assigned(NSViewHandle) then
      begin
        NSWin := NSViewHandle.window;
        if Assigned(NSWin) then
        begin
          NSWin.setAlphaValue(Opacity);
        end;
      end;
    except
      // Ignore any errors
    end;
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
  ApplyRoundedCorners;

  miNormal.Click;
  // Set the opacity
  SetFormOpacity(0.5);
end;

procedure TfFloat.lValMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
end;

procedure TfFloat.MenuItem1Click(Sender:TObject);
begin
  Hide;
end;

procedure TfFloat.miBigClick(Sender:TObject);
begin

end;

procedure TfFloat.miClockClick(Sender:TObject);
begin
  miClock.Checked := not miClock.Checked;
  lTime.Visible := miClock.Checked;
  if lTime.visible then begin
    tClock.OnTimer(tClock);
    tClock.Enabled := true;
  end;
end;

procedure TfFloat.miCustomSizeClick(Sender:TObject);
begin
ShowMessage(rsCustomSize);
end;

procedure TfFloat.miCustomVisibleClick(Sender: TObject);
begin
  ShowMessage(rsCustomOp);
end;

procedure TfFloat.miNormalClick(Sender:TMenuItem);
var
 h: integer;
begin
    miBig.Checked := false;
    miNormal.Checked := false;
    miSmall.Checked := false;
    miXL.Checked := false;

    if sender = miXL then begin
      miXL.Checked := true;
      h := Screen.DesktopHeight div 5;
    end else if sender = miBig then begin
      miBig.Checked := true;
      h := Screen.DesktopHeight div 10;
    end else if sender = miNormal then begin
      miNormal.Checked := true;
      h := Screen.DesktopHeight div 25;
    end else if sender = miSmall then begin
      miSmall.Checked := true;
      h := Screen.DesktopHeight div 50;
    end else if sender = miCustomSize then begin
      h := height;
    end;

    height := h;
    width := round(height * 1.55);
    lVal.width := round(clientwidth * 0.75);
    lArrow.width := round(clientwidth * 0.25);
  //---
  ApplyRoundedCorners;
end;

procedure TfFloat.miNormalClick(Sender:TObject);
begin

end;

procedure TfFloat.miOp100Click(Sender: TObject);
var
  i: integer;
  v: double;
begin
  if not TryStrToInt((sender as TMenuItem).hint, i) then
    Exit;
  v := i / 100;

  SetFormOpacity(v);
  (sender as TMenuItem).Checked := true;
end;

procedure TfFloat.tClockTimer(Sender:TObject);
begin
  lTime.caption :=  FormatDateTime(ShortTimeFormat, Now);
  if lTime.Visible = false then
    (sender as TTimer).Enabled := false;
end;

procedure TfFloat.tTitlebarTimer(Sender: TObject);
var
  x,y: integer;
begin
end;

procedure TfFloat.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if DraggingWin then
  begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end;
end;

procedure TfFloat.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
DraggingWin := false;
{$IF DEFINED(DARWIN) OR DEFINED(LCLQt6)}
   BorderStyle:= bsNone;
{$endif}
end;


procedure TfFloat.CreateRoundedCorners;
{$IFDEF LCLQt6_DISABLED}
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
begin

  case lvl of
    BGHigh: bgcol := QtYellow;
    BGLOW: bgcol := Qtblue;
    BGOff: bgcol := Qtblack;
    else
      bgcol := QtGreen;
  end;
  // Konvertera LCL handle till Qt widget
  QtWidget := TQtWidget(Handle);
  WidgetHandle := QtWidget.GetContainerWidget;

  // Se till att widgeten har transparent bakgrund
  QWidget_setAttribute(WidgetHandle, QtWA_TranslucentBackground, true);


  // Skapa QColor för svart
  BlackColor := QColor_create(0, 0, 0);

  // Hämta paint device från widget
  PaintDevice := QWidget_to_QPaintDevice(WidgetHandle);

  // Skapa målare direkt på widgeten
  Painter := QPainter_create();
  QPainter_begin(Painter, PaintDevice);

  // Aktivera antialiasing
  QPainter_setRenderHint(Painter, QPainterAntialiasing, true);

  // Skapa en path för rundade hörn
  Path := QPainterPath_create();
  QPainterPath_addRoundedRect(Path, 0, 0, Width, Height, 20, 20);

  // Skapa pensel för fyllning
  XBrush := QBrush_create();
  QBrush_setStyle(XBrush, QtSolidPattern);
  // För QBrush_setColor används QtGlobalColor enum
  QBrush_setColor(XBrush, bgcol);

  // Skapa penna för kontur
  Pen := QPen_create();
  QPen_setStyle(Pen, QtSolidLine);
  // För QPen_setColor används PQColor (QColorH)
  QPen_setColor(Pen, @BlackColor);
  QPen_setWidth(Pen, 1);

  // Rita direkt på widgeten
  QPainter_setPen(Painter, Pen);
  QPainter_setBrush(Painter, XBrush);
  QPainter_drawPath(Painter, Path);

  // Avsluta målning
  QPainter_end(Painter);

  // Städa upp
  QPainterPath_destroy(Path);
  QBrush_destroy(XBrush);
  QPen_destroy(Pen);
  QColor_destroy(BlackColor);
  QPainter_destroy(Painter);
  {$else}
  begin
      {$ENDIF}

end;



procedure TfFloat.FormPaint(Sender: TObject);
begin
  CreateRoundedCorners;
end;

procedure TfFloat.FormResize(Sender:TObject);
begin
  ScaleLbl(lVal,taLeftJustify,tlCenter);
  ScaleLbl(lArrow,taCenter,tlCenter);
  lTime.Font.Size := lArrow.font.size div 3;
  lTime.left := larrow.left - (lTime.Width div 2);
end;

procedure TfFloat.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  DraggingWin := true;
  PX := X;
  PY := Y;
end;

procedure TfFloat.FormMouseEnter(Sender: TObject);
begin

end;

procedure TfFloat.FormMouseLeave(Sender: TObject);
begin

end;

procedure TfFloat.FormKeyPress(Sender: TObject; var Key: char);
begin
end;

procedure TfFloat.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  num: double;
begin
  if key = VK_ESCAPE then begin
    Hide;
    key := 0;
  end;
  if ((ssShift in Shift) and (Key >= 48) and (Key <= 57)) then begin
    num := (key-48) / 10;
    if num < 0.1 then
      num := 1;

    SetFormOpacity(num);
    miCustomVisible.Checked := true;
  end;

  if ((ssShift in Shift) and (Key in [187, (* + *) 189, 191 (*mac +*)])) then begin
    if key <> 189 then // not -
        height := height + 5
    else
        height := height - 5;
    miNormalClick(miCustomSize);
  end;
end;

end.

