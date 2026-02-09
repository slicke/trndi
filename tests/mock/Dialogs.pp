unit Dialogs;

{$mode ObjFPC}{$H+}

interface

uses Classes;

const
  mtWarning = 2;
  mtInformation = 1;
  mtError = 3;
  mbOK = 1;
  mrYes = 6;

function MessageDlg(const Msg: string; Flags: integer; Buttons: integer; HelpCtx: longint): integer; overload;

type
  TOpenDialog = class
  public
    Title: string;
    Filter: string;
    DefaultExt: string;
    FileName: string;
    constructor Create(AOwner: TComponent = nil);
    function Execute: Boolean;
  end;

  TSaveDialog = class(TOpenDialog)
  end;

implementation

function MessageDlg(const Msg: string; Flags: integer; Buttons: integer; HelpCtx: longint): integer; overload;
begin
  Result := 0;
end;

constructor TOpenDialog.Create(AOwner: TComponent = nil);
begin
  FileName := '';
end;

function TOpenDialog.Execute: Boolean;
begin
  Result := True;
end;

end.
