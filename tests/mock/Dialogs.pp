unit Dialogs;

{$mode ObjFPC}{$H+}

interface

function MessageDlg(const Msg: string; Flags: integer; Buttons: integer; HelpCtx: longint): integer; overload;

implementation

function MessageDlg(const Msg: string; Flags: integer; Buttons: integer; HelpCtx: longint): integer; overload;
begin
  Result := 0;
end;

end.
