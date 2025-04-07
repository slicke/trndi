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
    lRangeDown:TLabel;
    lArrow: TLabel;
    lRangeUp:TLabel;
    lVal: TLabel;
    miVisible: TMenuItem;
    miOp100: TMenuItem;
    miOp25: TMenuItem;
    miOp50: TMenuItem;
    miOp75: TMenuItem;
    miCustomVisible: TMenuItem;
    miSplit1: TMenuItem;
    pMain: TPopupMenu;
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
    procedure FormShow(Sender: TObject);
    procedure lValMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure miCustomVisibleClick(Sender: TObject);
    procedure miOp100Click(Sender: TObject);
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

var
  fFloat: TfFloat;
  PX, PY: integer;
  DraggingWin: boolean;


implementation

{$R *.lfm}

procedure TfFloat.FormCreate(Sender: TObject);
{$IF DEFINED(LCLQt6)}
var
  QtWidget: TQtWidget;
{$ENDIF}
begin
  // We dont want any borders, as its a float
  BorderStyle := bsNone;
  {$IFNDEF DARWIN}
  Color := clWhite; //Set the background color
   {$endif}
   {$IFDEF LCLQt6}
  if HandleAllocated then
  begin
    QtWidget := TQtWidget(Handle);
    if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
    begin
      // Set translucent background
      QtWidget.setAttribute(QtWA_TranslucentBackground, True);

      // Set frameless window hint
      QtWidget.setWindowFlags(QtWidget.windowFlags or QtFramelessWindowHint);
    end;
  end;
  {$ENDIF}
end;

procedure TfFloat.ApplyRoundedCorners;
var
{$IFDEF DARWIN}
  NSViewHandle: NSView;
  NSWin: NSWindow;
  Mask: NSBezierPath;
{$ENDIF}
{$IFDEF Windows}
 ABitmap: TBitmap;
{$ENDIF}
{$IFDEF LCLQT6}
  StyleStr: WideString;
{$ENDIF}
{$IFDEF LCLGTK2}
 ABitmap: TBitmap;
{$ENDIF}
begin
  {$IFDEF DARWIN}
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
  {$ENDIF}
  {$IFDEF LCLQT6}
    StyleStr := 'border-radius: 10px; background-color: rgba(240, 240, 240, 255);';
  Self.BorderStyle := bsNone; // Remove border
if HandleAllocated then
QWidget_setStyleSheet(TQtWidget(Handle).Widget,
  @stylestr);
  CreateRoundedCorners;
  {$ELSif false}
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

  // Set the opacity
  SetFormOpacity(0.5);
end;

procedure TfFloat.lValMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
end;

procedure TfFloat.miCustomVisibleClick(Sender: TObject);
begin
  ShowMessage(rsCustomOp);
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
{$ifdef DARWIN}
   BorderStyle:= bsNone;
{$endif}
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
  if not ((ssShift in Shift) and (Key >= 48) and (Key <= 57)) then
    Exit;
  num := (key-48) / 10;
  if num < 0.1 then
    num := 1;

  SetFormOpacity(num);
  miCustomVisible.Checked := true;
end;

end.

