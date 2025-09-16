unit usplash;

{$I inc/native.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfSplash }

  TfSplash = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    lInfo: TLabel;
    lSplashWarn:TLabel;
    procedure FormCreate(Sender: TObject);
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

end.

