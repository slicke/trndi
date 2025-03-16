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
  {$ENDIF};

type
  TfFloat = class(TForm)
    lArrow: TLabel;
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
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure lValMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure miCustomVisibleClick(Sender: TObject);
    procedure miOp100Click(Sender: TObject);
  private
    procedure SetFormOpacity(Opacity: Double);
    procedure ApplyRoundedCorners;
  public

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
begin
  // We dont want any borders, as its a float
  BorderStyle := bsNone;
  {$IFNDEF DARWIN}
  Color := clWhite; //Set the background color
  {$ENDIF}
end;

procedure TfFloat.ApplyRoundedCorners;
{$IFDEF DARWIN}
var
  NSViewHandle: NSView;
  NSWin: NSWindow;
  Mask: NSBezierPath;
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
  {$ELSE}
  // Use LCL stuff when not Mac
  var ABitmap := TBitmap.Create;
  try
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
  // Standard LCL approach för andra plattformar
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

procedure TfFloat.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  DraggingWin := true;
  PX := X;
  PY := Y;
end;

procedure TfFloat.FormKeyPress(Sender: TObject; var Key: char);
begin
end;

procedure TfFloat.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  num: double;
begin
  if not ((ssShift in Shift) and (Key >= 48) and (Key <= 57)) then
    Exit;
  num := (key-48) / 10;
  if num < 0.1 then
    num := 1;

  SetFormOpacity(num);
  miCustomVisible.Checked := true;
end;

end.

