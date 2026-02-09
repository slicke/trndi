unit slicke.ux.alert;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Forms, Graphics;

type
  TUXDialogSize = (uxdAuto, uxdOnForm, uxdNormal, uxdInline);
  UXImage = LongInt;

  TUXMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose, mbUXOpenFile, mbUXMinimize, mbUXAgree, mbUXRead, mbUXDefault, mbUXSnooze);
  TUXMsgDlgBtns = set of TUXMsgDlgBtn;

  TModalResult = Integer;

procedure UXMessage(const dialogsize: TUXDialogSize; const title, message: string; const icon: UXImage = 0; sender: TForm = nil);
const
  sHTMLLineBreak = '<br>';
  uxmtInformation = 0;
  uxmtOK = 1;
  uxmtWarning = 2;
  uxmtCustom = 99;
  mrOk = 1;
  mrCancel = 2;
  mrNo = 3;
  mrYes = 6;
  sSuccTitle  = 'Information';
  uxclWhite = $00FFFFFF;
  uxclRed = $000000FF;
  uxclLightBlue = $00E0FFFF;

  uxscSmall = 0.75;
  uxscNormal = 1;
  uxscBig = 5;
  uxscBigger = 7.5;
  uxscLarge = 10;
  uxscHuge = 20;
  uxscEnormous = 30;

procedure UXMessage(const title, message: string; const icon: UXImage = 0; sender: TForm = nil);

function ExtHTML(const dialogsize: TUXDialogSize; const caption, html: string; buttons: TUXMsgDlgBtns = [mbOK]; const icon: UXImage = uxmtInformation; scale: Extended = 1): TModalResult;

function ExtMsg(const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TUXMsgDlgBtns = [mbAbort]; const icon: UXImage = uxmtInformation; extra: LongInt = 0; scale: Extended = 1): TModalResult; overload;
function ExtMsg(const dialogsize: TUXDialogSize; const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TUXMsgDlgBtns = [mbAbort]; const mtype: Integer = 0; extra: LongInt = 0; scale: Extended = 1): TModalResult; overload;
function ExtMsg(const dialogsize: TUXDialogSize; const title, message: string; buttons: TUXMsgDlgBtns; const mtype: Integer; extra: LongInt = 0; scale: Extended = 1): TModalResult; overload;
function ExtMsgYesNo(const caption, desc: string; const micon: UXImage = uxmtInformation): boolean; overload;
function ExtMsgYesNo(const dialogsize: TUXDialogSize; const caption, desc: string; const micon: UXImage = uxmtInformation): boolean; overload;

function ExtText(const dialogsize: TUXDialogSize; const caption, text: string; buttons: TUXMsgDlgBtns = [mbOK]; const icon: UXImage = uxmtInformation; scale: Extended = 1): TModalResult;
function ExtInput(const caption, prompt: string; const def: string = ''): string; overload;
function ExtInput(const dialogsize: TUXDialogSize; const title, prompt, labelText, def: string; var mr: TModalResult): string; overload;
function ExtList(const caption: string; var items: TStringList): integer; overload;
function ExtList(const dialogsize: TUXDialogSize; const title, header, desc: string; const items: array of unicodestring): LongInt; overload;
function ExtList(const dialogsize: TUXDialogSize; const title, header, desc: string; const items: TStringArray; const showOk: Boolean = True): LongInt; overload;

function ExtFontPicker(const dialogsize: TUXDialogSize; const caption, title, msg, title2: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont; overload;
function ExtFontPicker(const dialogsize: TUXDialogSize; const caption, title, msg: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont; overload;

function UXDialog(const dialogsize: TUXDialogSize; const title, message: string; buttons: TUXMsgDlgBtns): TModalResult; overload;
function UXDialog(const dialogsize: TUXDialogSize; const title, message: string; buttons: TUXMsgDlgBtns; const mtype: Integer): TModalResult; overload;
function UXDialog(const dialogsize: TUXDialogSize; const header, title, message: string; buttons: TUXMsgDlgBtns; const mtype: Integer): TModalResult; overload;

implementation

procedure UXMessage(const dialogsize: TUXDialogSize; const title, message: string; const icon: UXImage = 0; sender: TForm = nil);
begin
  // No-op for headless tests
end;

procedure UXMessage(const title, message: string; const icon: UXImage = 0; sender: TForm = nil);
begin
  // No-op for headless tests
end;
function ExtHTML(const dialogsize: TUXDialogSize; const caption, html: string; buttons: TUXMsgDlgBtns = [mbOK]; const icon: UXImage = uxmtInformation; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function ExtMsg(const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TUXMsgDlgBtns = [mbAbort]; const icon: UXImage = uxmtInformation; extra: LongInt = 0; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function ExtMsg(const dialogsize: TUXDialogSize; const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TUXMsgDlgBtns = [mbAbort]; const mtype: Integer = 0; extra: LongInt = 0; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function ExtMsg(const dialogsize: TUXDialogSize; const title, message: string; buttons: TUXMsgDlgBtns; const mtype: Integer; extra: LongInt = 0; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;
function ExtMsgYesNo(const caption, desc: string; const micon: UXImage = uxmtInformation): boolean;
begin
  Result := True;
end;

function ExtMsgYesNo(const dialogsize: TUXDialogSize; const caption, desc: string; const micon: UXImage = uxmtInformation): boolean;
begin
  Result := True;
end;

function ExtText(const dialogsize: TUXDialogSize; const caption, text: string; buttons: TUXMsgDlgBtns = [mbOK]; const icon: UXImage = uxmtInformation; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function ExtInput(const caption, prompt: string; const def: string = ''): string; overload;
begin
  Result := def;
end;

function ExtInput(const dialogsize: TUXDialogSize; const title, prompt, labelText, def: string; var mr: TModalResult): string; overload;
begin
  mr := mrOk;
  Result := def;
end;

function ExtList(const caption: string; var items: TStringList): integer; overload;
begin
  Result := 0;
end;

function ExtList(const dialogsize: TUXDialogSize; const title, header, desc: string; const items: array of unicodestring): LongInt; overload;
begin
  Result := 0;
end;

function ExtList(const dialogsize: TUXDialogSize; const title, header, desc: string; const items: TStringArray; const showOk: Boolean = True): LongInt; overload;
begin
  // Headless: present the list and return 0 as default selection
  Result := 0;
end;

function ExtFontPicker(const dialogsize: TUXDialogSize; const caption, title, msg, title2: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont;
begin
  // Return a copy of the provided font for headless tests
  Result := TFont.Create;
  Result.Name := AFont.Name;
  Result.Size := AFont.Size;
  Result.Color := AFont.Color;
  mr := mrOk;
end;

function ExtFontPicker(const dialogsize: TUXDialogSize; const caption, title, msg: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont;
begin
  // Provide fallback overload used by some callers
  Result := ExtFontPicker(dialogsize, caption, title, msg, '', AFont, sampleText, mr);
end;

function UXDialog(const dialogsize: TUXDialogSize; const title, message: string; buttons: TUXMsgDlgBtns): TModalResult; overload;
begin
  Result := mrOk;
end;

function UXDialog(const dialogsize: TUXDialogSize; const title, message: string; buttons: TUXMsgDlgBtns; const mtype: Integer): TModalResult; overload;
begin
  Result := mrOk;
end;

function UXDialog(const dialogsize: TUXDialogSize; const header, title, message: string; buttons: TUXMsgDlgBtns; const mtype: Integer): TModalResult; overload;
begin
  Result := mrOk;
end;

end.
