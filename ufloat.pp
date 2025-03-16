unit ufloat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls,Menus,StdCtrls,SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TfFloat = class(TForm)
    lArrow:TLabel;
    lVal:TLabel;
    miVisible:TMenuItem;
    miOp100:TMenuItem;
    miOp25:TMenuItem;
    miOp50:TMenuItem;
    miOp75:TMenuItem;
    miCustomVisible:TMenuItem;
    miSplit1:TMenuItem;
    pMain:TPopupMenu;
    procedure FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure FormKeyPress(Sender:TObject;var Key:char);
    procedure FormMouseDown(Sender:TObject;Button:TMouseButton;Shift:TShiftState
      ;X,Y:Integer);
    procedure FormMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
    procedure FormMouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X
      ,Y:Integer);
    procedure FormShow(Sender:TObject);
    procedure lValMouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X
      ,Y:Integer);
    procedure miCustomVisibleClick(Sender:TObject);
    procedure miOp100Click(Sender:TObject);
  private

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

procedure TfFloat.FormShow(Sender: TObject);
var
  ABitmap: TBitmap;
begin
  // Hide the titlebar
  BorderStyle := bsNone;

  ABitmap := TBitmap.Create;
  ABitmap.Monochrome := True;
  ABitmap.Width := Width;
  ABitmap.Height := Height;

  // We set the background as black (which will be transparent)
  ABitmap.Canvas.Brush.Color := clBlack;
  ABitmap.Canvas.FillRect(0, 0, Width, Height);

  // Now we draw our shape in White
  ABitmap.Canvas.Brush.Color := clWhite;

  // RoundRect istället för Ellipse
  // Parametrarna är: x1, y1, x2, y2, hörnbredd, hörnhöjd
  ABitmap.Canvas.RoundRect(0, 0, Width, Height, 20, 20);

  SetShape(ABitmap);
  ABitmap.Free;

  AlphaBlend := True;
  AlphaBlendValue := 128; // 0 (helt genomskinlig) till 255 (helt ogenomskinlig)
end;

procedure TfFloat.lValMouseUp(Sender:TObject;Button:TMouseButton;Shift:
  TShiftState;X,Y:Integer);
begin

end;

procedure TfFloat.miCustomVisibleClick(Sender:TObject);
begin
ShowMessage(rsCustomOp);
end;

procedure TfFloat.miOp100Click(Sender:TObject);
var
i: integer;
v: double;
begin
if not TryStrToInt((sender as TMenuItem).hint, i) then
 Exit;

 v := i / 100;

 AlphaBlendValue := round(255 * v);
 (sender as TMenuItem).Checked := true;
end;

procedure TfFloat.FormMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
begin
  if DraggingWin then
  begin
     SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end;

end;

procedure TfFloat.FormMouseUp(Sender:TObject;Button:TMouseButton;Shift:
  TShiftState;X,Y:Integer);
begin
  DraggingWin := false;
end;

procedure TfFloat.FormMouseDown(Sender:TObject;Button:TMouseButton;Shift:
  TShiftState;X,Y:Integer);
begin
  DraggingWin := true;
  PX := X;
  PY := Y;
end;

procedure TfFloat.FormKeyPress(Sender:TObject;var Key:char);
begin

end;

procedure TfFloat.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
var
 num: double;
begin
   if not ( (ssShift in Shift) and (Key >= 48) and (Key <= 57) ) then
  Exit;

  num := (key-48) / 10;

  if num < 0.1 then
    num := 1;
  AlphaBlendValue := round(255 * num);

  miCustomVisible.Checked := true;
end;

end.

