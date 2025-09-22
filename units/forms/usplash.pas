unit usplash;

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLType;

type

  { TfSplash }

  TfSplash = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    lInfo: TLabel;
    lSplashWarn:TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  fSplash: TfSplash;

implementation

{$R *.lfm}

{ TfSplash }

procedure TfSplash.Image1Click(Sender: TObject);
begin

end;

procedure TfSplash.FormCreate(Sender: TObject);
begin
  {$ifdef X_LINUXBSD}
     lSplashWarn.Font.Size := 8;
  {$endif}
end;

procedure TfSplash.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if key = VK_ESCAPE then
     Hide;
end;

procedure TfSplash.FormKeyPress(Sender: TObject; var Key: char);
begin

end;

end.

