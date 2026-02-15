unit Dialogs;

{$mode ObjFPC}{$H+}

interface

uses Classes;

const
  mtWarning = 2;
  mtInformation = 1;
  mtError = 3;

  // Button constants (mb*) used by UX dialogs
  mbOK = 1;
  mbCancel = 2;
  mbAbort = 3;
  mbRetry = 4;
  mbIgnore = 5;
  mbYes = 6;
  mbNo = 7;
  mbAll = 14;
  mbClose = 15;
  mbUXMinimize = 16;

  // Modal result constants (mr*) returned by dialogs
  mrNone = 0;
  mrOk = 1;
  mrCancel = 2;
  mrAbort = 3;
  mrRetry = 4;
  mrIgnore = 5;
  mrYes = 6;
  mrNo = 7;
  mrClose = 15;
  mrNoToAll = 16;
  mrYesToAll = 17;

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
