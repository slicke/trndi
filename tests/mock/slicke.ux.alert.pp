(*
 * slicke.ux.alert.pas
 * Headless test stub of slicke.ux.alert.
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 * License: Apache License 2.0
 *)
unit slicke.ux.alert;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Forms, Graphics;

type
  TSlickeDialogSize = (sdsAuto, sdsOnForm, sdsNormal, sdsInline);
  SlickeUXImage = LongInt;

  TSlickeMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose, mbSlickeOpenFile, mbSlickeMinimize, mbSlickeAgree, mbSlickeRead, mbSlickeDefault, mbSlickeSnooze);
  TSlickeMsgDlgBtns = set of TSlickeMsgDlgBtn;

  TModalResult = Integer;

procedure SlickeMessage(const dialogsize: TSlickeDialogSize; const title, message: string; const icon: SlickeUXImage = 0; sender: TForm = nil);
const
  sHTMLLineBreak = '<br>';
  uxmtInformation = 0;
  uxmtOK = 1;
  uxmtWarning = 2;
  uxmtCog = 99; // gear emoji placeholder for headless tests
  uxmtCustom = uxmtCog;
  mrOk = 1;
  mrCancel = 2;
  mrNo = 3;
  mrYes = 6;
  sSuccTitle  = 'Information';
  uxclWhite = $00FFFFFF;
  uxclRed = $000000FF;
  uxclLightBlue = $00E0FFFF;
  // sentinel used by numeric dialogs
  FLOAT_NONE = -999.66;

  uxscSmall = 0.75;
  uxscNormal = 1;
  uxscBig = 5;
  uxscBigger = 7.5;
  uxscLarge = 10;
  uxscHuge = 20;
  uxscEnormous = 30;

procedure SlickeMessage(const title, message: string; const icon: SlickeUXImage = 0; sender: TForm = nil);

function SlickeHTMLMsg(const dialogsize: TSlickeDialogSize; const caption, html: string; buttons: TSlickeMsgDlgBtns = [mbOK]; const icon: SlickeUXImage = uxmtInformation; scale: Extended = 1): TModalResult;

function SlickeMsg(const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TSlickeMsgDlgBtns = [mbAbort]; const icon: SlickeUXImage = uxmtInformation; extra: LongInt = 0; scale: Extended = 1): TModalResult; overload;
function SlickeMsg(const dialogsize: TSlickeDialogSize; const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TSlickeMsgDlgBtns = [mbAbort]; const mtype: Integer = 0; extra: LongInt = 0; scale: Extended = 1): TModalResult; overload;
function SlickeMsg(const dialogsize: TSlickeDialogSize; const title, message: string; buttons: TSlickeMsgDlgBtns; const mtype: Integer; extra: LongInt = 0; scale: Extended = 1): TModalResult; overload;
function SlickeMsgYesNo(const caption, desc: string; const micon: SlickeUXImage = uxmtInformation): boolean; overload;
function SlickeMsgYesNo(const dialogsize: TSlickeDialogSize; const caption, desc: string; const micon: SlickeUXImage = uxmtInformation): boolean; overload;

function SlickePrompt(const dialogsize: TSlickeDialogSize; const caption, text: string; buttons: TSlickeMsgDlgBtns = [mbOK]; const icon: SlickeUXImage = uxmtInformation; scale: Extended = 1): TModalResult;
function SlickeInput(const dialogsize: TSlickeDialogSize; const title, prompt, labelText, def: string; var mr: TModalResult): string; overload;
function SlickeList(const dialogsize: TSlickeDialogSize; const title, header, desc: string; const items: array of unicodestring; const Default: boolean = false): LongInt; overload;
function SlickeList(const dialogsize: TSlickeDialogSize; const title, header, desc: string; const items: array of string; const Default: boolean = false): LongInt; overload;

// Numeric and date inputs (headless stubs)
function ExtIntInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: integer;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): integer;

function ExtNumericInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: double;
AMin, AMax: double;
float: boolean;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): double;

function ExtDatePicker(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: TDateTime;
AMinDate: TDateTime;
AMaxDate: TDateTime;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog): TDateTime;

function ExtFontPicker(const dialogsize: TSlickeDialogSize; const caption, title, msg, title2: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont; overload;
function ExtFontPicker(const dialogsize: TSlickeDialogSize; const caption, title, msg: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont; overload;

function UXDialog(const dialogsize: TSlickeDialogSize; const title, message: string; buttons: TSlickeMsgDlgBtns): TModalResult; overload;
function UXDialog(const dialogsize: TSlickeDialogSize; const title, message: string; buttons: TSlickeMsgDlgBtns; const mtype: Integer): TModalResult; overload;
function UXDialog(const dialogsize: TSlickeDialogSize; const header, title, message: string; buttons: TSlickeMsgDlgBtns; const mtype: Integer): TModalResult; overload;

implementation

procedure SlickeMessage(const dialogsize: TSlickeDialogSize; const title, message: string; const icon: SlickeUXImage = 0; sender: TForm = nil);
begin
  // No-op for headless tests
end;

procedure SlickeMessage(const title, message: string; const icon: SlickeUXImage = 0; sender: TForm = nil);
begin
  // No-op for headless tests
end;
function SlickeHTMLMsg(const dialogsize: TSlickeDialogSize; const caption, html: string; buttons: TSlickeMsgDlgBtns = [mbOK]; const icon: SlickeUXImage = uxmtInformation; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function SlickeMsg(const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TSlickeMsgDlgBtns = [mbAbort]; const icon: SlickeUXImage = uxmtInformation; extra: LongInt = 0; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function SlickeMsg(const dialogsize: TSlickeDialogSize; const caption, title, desc, logmsg: string; dumpbg: TColor = uxclWhite; dumptext: TColor = uxclRed; buttons: TSlickeMsgDlgBtns = [mbAbort]; const mtype: Integer = 0; extra: LongInt = 0; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function SlickeMsg(const dialogsize: TSlickeDialogSize; const title, message: string; buttons: TSlickeMsgDlgBtns; const mtype: Integer; extra: LongInt = 0; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;
function SlickeMsgYesNo(const caption, desc: string; const micon: SlickeUXImage = uxmtInformation): boolean;
begin
  Result := True;
end;

function SlickeMsgYesNo(const dialogsize: TSlickeDialogSize; const caption, desc: string; const micon: SlickeUXImage = uxmtInformation): boolean;
begin
  Result := True;
end;

function SlickePrompt(const dialogsize: TSlickeDialogSize; const caption, text: string; buttons: TSlickeMsgDlgBtns = [mbOK]; const icon: SlickeUXImage = uxmtInformation; scale: Extended = 1): TModalResult;
begin
  Result := mrOk;
end;

function SlickeInput(const dialogsize: TSlickeDialogSize; const title, prompt, labelText, def: string; var mr: TModalResult): string; overload;
begin
  mr := mrOk;
  Result := def;
end;

// Headless numeric/date input stubs
function ExtIntInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: integer;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): integer;
begin
  // call numeric input with no min/max limits
  Result := round(ExtNumericInput(dialogsize, ACaption, ATitle, ADesc, ADefault, FLOAT_NONE, FLOAT_NONE, false, ModalResult, icon));
end;

function ExtNumericInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: double;
AMin, AMax: double;
float: boolean;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): double;
begin
  // ignore limits for headless tests and just return default
  ModalResult := mrOk;
  Result := ADefault;
end;

function ExtDatePicker(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: TDateTime;
AMinDate: TDateTime;
AMaxDate: TDateTime;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog): TDateTime;
begin
  ModalResult := mrOk;
  Result := ADefault;
end;

function SlickeList(const dialogsize: TSlickeDialogSize; const title, header, desc: string; const items: array of unicodestring; const Default: boolean = false): LongInt; overload;
begin
  Result := 0;
end;

function SlickeList(const dialogsize: TSlickeDialogSize; const title, header, desc: string; const items: array of string; const Default: boolean = false): LongInt; overload;
begin
  Result := 0;
end;


function ExtFontPicker(const dialogsize: TSlickeDialogSize; const caption, title, msg, title2: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont;
begin
  // Return a copy of the provided font for headless tests
  Result := TFont.Create;
  Result.Name := AFont.Name;
  Result.Size := AFont.Size;
  Result.Color := AFont.Color;
  mr := mrOk;
end;

function ExtFontPicker(const dialogsize: TSlickeDialogSize; const caption, title, msg: string; AFont: TFont; const sampleText: string; var mr: TModalResult): TFont;
begin
  // Provide fallback overload used by some callers
  Result := ExtFontPicker(dialogsize, caption, title, msg, '', AFont, sampleText, mr);
end;

function UXDialog(const dialogsize: TSlickeDialogSize; const title, message: string; buttons: TSlickeMsgDlgBtns): TModalResult; overload;
begin
  Result := mrOk;
end;

function UXDialog(const dialogsize: TSlickeDialogSize; const title, message: string; buttons: TSlickeMsgDlgBtns; const mtype: Integer): TModalResult; overload;
begin
  Result := mrOk;
end;

function UXDialog(const dialogsize: TSlickeDialogSize; const header, title, message: string; buttons: TSlickeMsgDlgBtns; const mtype: Integer): TModalResult; overload;
begin
  Result := mrOk;
end;

end.
