
(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit slicke.ux.alert;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
Classes, SysUtils, Dialogs, Forms, ExtCtrls, StdCtrls, Controls, Graphics, Math,
IntfGraphics, FPImage, graphtype, lcltype, Trndi.Native,
{$ifdef Windows}
DX12.D2D1, DX12.DXGI, DX12.DWrite, DX12.DCommon, DX12.WinCodec, Windows, Buttons,
{$endif}StrUtils;

resourcestring
dlgErr = 'An error occured while creating a message dalog';
sMsgTitle = 'Message';
sSuccTitle = 'Information';
sExtTitle = 'Extension error';
sExtErr = 'Error occured in extension';
sErr = 'Script execution failed';

smbYes = 'Yes';
smbUXNo = 'No';
smbUXOK = 'OK';
smbUXCancel = 'Cancel';
smbUXAbort = 'Abort';
smbUXRetry = 'Retry';
smbUXIgnore = 'Ignore';
smbUXAll = 'All';
smbUXNoToAll = 'No To All';
smbUXYesToAll = 'Yes To All';
smbUXHelp = 'Help';
smbUXClose = 'Close';
smbUXOpenFile = 'Open File';
smbUxMinimize = 'Minimize';
smbSelect = 'Select';

const
muiStop = $26D4;
muiStar = $2B50;
muiCog = $2699;

mbUXYes = mbYes;
mbUXNo = mbNo;
mbUXOK = mbOK;
mbUXCancel = mbCancel;
mbUXAbort = mbAbort;
mbUXRetry = mbRetry;
mbUXIgnore = mbIgnore;
mbUXAll = mbAll;
mbUXNoToAll = mbNoToAll;
mbUXYesToAll = mbYesToAll;
mbUXHelp = mbHelp;
mbUXClose = mbClose;
mbUXUXOpenFile = 110;


type
TDialogForm = class(TForm)
  procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
protected
  procedure CreateWnd; override;
 {$ifdef windows}
public
 procedure ButtonDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TButtonState);
  {$endif}
end;

TUXMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
  mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose, mbUXOpenFile, mbUXMinimize);

TUXMsgDlgBtns = set of TUXMsgDlgBtn;

UXMessageBox = record
  title: string;
  message: string;
  buttons: TMsgDlgButtons;
  icon: widechar;
    //function Execute: ModalResult;
end;
ButtonLangs = array[TUXMsgDlgBtn] of string;
var
langs : ButtonLangs = (smbYes, smbUXNo, smbUXOK, smbUXCancel, smbUXAbort, smbUXRetry, smbUXIgnore, smbUXAll, smbUXNoToAll, smbUXYesToAll, smbUXHelp, smbUXClose, smbUXOpenFile, smbUXMinimize);


//function UXShowMessage(const message: string; buttons: TMsgDlgButtons; const icon: Widechar): TModalResult;

procedure UXMessage(const title, message: string; const icon: widechar = widechar($2705));
function UXDialog(const title, message: string; buttons: TUXMsgDlgBtns; const icon: widechar = widechar($2705)): TModalResult;
function UXDialog(const title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;
function UXDialog(const header, title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;

//function UXShowMessage(const caption, title, desc, message: string; buttons: TMsgDlgButtons; const icon: WideChar): TModalResult;
function ExtMsg(const  caption, title, desc, logmsg: string; dumpbg: TColor = $00F5F2FD; dumptext:
TColor = $003411A9; buttons: TUXMsgDlgBtns = [mbAbort]; const icon: widechar =
widechar($2699)): TModalResult;
function ExtLog(const caption, msg, log: string; const icon: widechar = widechar($2699)):
TModalResult
;
function ExtList(
  const ACaption, ATitle, ADesc: string;
  const Choices: array of string;
  const icon: WideChar = WideChar($2699)
): Integer;
function ExtInput(
  const ACaption, ATitle, ADesc, ADefault: string;
  var ModalResult: TModalResult;
  const icon: WideChar = WideChar($2699)
): string;
function ExtError(const msg, error: string; const icon: widechar = widechar($2699)): TModalResult;
function ExtError(const error: string; const icon: widechar = widechar($2699)): TModalResult;
function ExtSucc(const msg, desc, output: string; dumpbg: TColor = $0095EEC4; dumptext: TColor = $00147C4A; const icon: widechar = widechar($2705)): TModalResult;
function FontInList(out fname: string): Boolean;
{$ifdef Windows}
function CoCreateInstance(const clsid: TGUID; unkOuter: IUnknown; dwClsContext: longint;
const iid: TGUID; out pv): HResult;
stdcall;
external 'ole32.dll';
{$endif}

implementation

{$if DEFINED(LINUX)}
function FontInList(out fname: string): Boolean;
var
  sl: TStrings;
begin
  fname := 'Noto Color Emoji';
  sl := TStringList.Create;
  try
    sl := Screen.Fonts; // List fonts
    Result := (sl.IndexOf('Noto Emoji') >= 0) or (sl.IndexOf('Noto Color Emoji') >= 0);
  finally
    sl.Free;
  end;
end;
{$elseif DEFINED(WINDOWS)}
function FontInList(out fname: string): Boolean;
var
  FontsList: TStrings;
begin
  FontsList := TStrings.Create;
  fname := 'Segoe UI Emoji';
  try
    FontsList := Screen.Fonts;
    Result := FontsList.IndexOf('Segoe UI Emoji') >= 0;
  finally
    FontsList.Free;
  end;
end;

{$else}
function FontInList(out fname: string): Boolean;
begin
  fname := 'font';
  result := true;
end;

{$endif}

function UXButtonToModalResult(Btn: TUXMsgDlgBtn): TModalResult;
begin
  case Btn of
  mbYes:
    Result := mrYes;
  mbNo:
    Result := mrNo;
  mbOK:
    Result := mrOK;
  mbCancel:
    Result := mrCancel;
  mbAbort:
    Result := mrAbort;
  mbRetry:
    Result := mrRetry;
  mbIgnore:
    Result := mrIgnore;
  mbAll:
    Result := mrAll;
  mbNoToAll:
    Result := mrNoToAll;
  mbYesToAll:
    Result := mrYesToAll;
//    mbUXHelp:      Result := mrHelp;
  mbClose:     Result := mrClose;
  else
    // t.ex. ditt specialfall för OpenFile
    Result := TModalResult(110);
  end;
end;

procedure TDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  o: TCustomButton;
  i: integer;
  cancel, no, ok, ct: integer;
  btns: TComponent;
  target: TWinControl;
begin
if not key in [VK_ESCAPE, VK_RETURN] then
  Exit;
cancel := -1;
no := -1;
ct := 0;
btns := self.FindComponent('pnButtons');
if btns = nil then
target := self
else
target := btns as TPanel;
 for i := 0 to target.ComponentCount-1 do begin
   if target.components[i] is TCustomButton then begin
     o := target.components[i] as TCustomButton;
     if o.ModalResult = mrCancel then cancel := i;
     if o.ModalResult = mrNo then no := i;
     if o.ModalResult = mrOK then ok := i;
     if o.ModalResult = mrClose then ok := i;
     Inc(ct);
   end;

 end;

 if key = vk_escape then begin
   if cancel >= 0 then
     (target.Components[cancel] as TCustomButton).click
   else if no >= 0
     then (target.Components[no] as TCustomButton).click;
 end else if key = VK_RETURN then begin
    if ((ct = 1) and (ok >= 0)) then
      (target.Components[ok] as TCustomButton).click
    else if ((ct = 2) and (ok = 1)) then
      (target.Components[ok] as TCustomButton).click;
 end;
end;

{$ifndef Windows}
procedure TDialogForm.CreateWnd;
begin
   inherited CreateWnd;
     KeyPreview := True;
end;
{$endif}

{$ifdef Windows}
procedure TDialogForm.CreateWnd;
const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
var
  Value: integer;
begin
  inherited CreateWnd;
   if HandleAllocated then
  begin
    SetWindowLong(Handle, GWL_STYLE,
      GetWindowLong(Handle, GWL_STYLE) or WS_SYSMENU);
  end;
  KeyPreview := True;
  if not TrndiNative.isDarkMode then
    Exit;
  Value := 1;
  // Nu är Handle giltigt – anropa DWM
  DwmSetWindowAttribute(Handle,
    DWMWA_USE_IMMERSIVE_DARK_MODE, @Value, SizeOf(Value));
  // Tvinga omritning av non-client area
  SetWindowPos(Handle, 0,0,0,0,0,
    SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
end;

procedure AssignEmoji(Image: TImage; const Emoji: widestring; dark: boolean = false);

var
  D2DFactory: ID2D1Factory;
  DWFactory: IDWriteFactory;
  WICFactory: IWICImagingFactory;
  WICBitmap: IWICBitmap;
  RenderTarget: ID2D1RenderTarget;
  TextFormat: IDWriteTextFormat;
  BackgroundBrush: ID2D1SolidColorBrush;
  TextRect: TD2D1_RECT_F;
  BackgroundColor: TD2D1_COLOR_F;
  hr: HRESULT;
  Bitmap: Graphics.TBitmap;
  RenderProps: TD2D1_RENDER_TARGET_PROPERTIES;
  BitmapP: TD2D1_BITMAP_PROPERTIES;
  ibitmap: ID2D1Bitmap;
  bLock: IWICBitmapLock;
  logheight: integer;

procedure wicToBitmap;
  const
    WICBitmapLockRead = $1;
  // Flagga för läsning
    WICBitmapLockWrite = $2;
  // Flagga för skrivning (om du vill skriva till bitmapen)

  var
    WICLock: IWICBitmapLock;
    BitmapData: Pointer;
    BitmapStride, BitmapSize: UINT;
    x, y: integer;
    PixelPtr: pbyte;
    PixelColor: TColor;
    Rect: TWICRect;
  begin
  // Definiera en rektangel som täcker hela IWICBitmap för att låsa hela bilden
    Rect.X := 0;
    Rect.Y := 0;
    Rect.Width := Image.Width;
    Rect.Height := Image.Height;

    Bitmap.SetSize(rect.width, rect.height);
    Bitmap.PixelFormat := pf32bit;

  // Lås bitmapen för läsning och få tillgång till dess data
    if Failed(WICBitmap.Lock(@Rect, WICBitmapLockRead, WICLock)) then
    begin
      ShowMessage(dlgErr);
      Exit;
    end;

    try
    // Hämta pekare till bitmapens data och dess stride (antal bytes per rad)
      if Failed(WICLock.GetDataPointer(BitmapSize, BitmapData)) or
        Failed(WICLock.GetStride(BitmapStride)) then
      begin
        ShowMessage(dlgErr);
        Exit;
      end;

    // Konfigurera TBitmap för att matcha IWICBitmap:ens storlek och format
      Bitmap.PixelFormat := pf32bit;
      Bitmap.SetSize(Rect.Width, Rect.Height);

    // Kopiera pixeldata rad för rad från IWICBitmap till TBitmap
      for y := 0 to Bitmap.Height - 1 do
      begin
        PixelPtr := BitmapData;
        for x := 0 to Bitmap.Width - 1 do
        begin
            // Läs RGB-komponenterna från IWICBitmap och kopiera till TBitmap
          PixelColor := RGB(PixelPtr^, (PixelPtr + 1)^, (PixelPtr + 2)^);
          Bitmap.Canvas.Pixels[x, y] := PixelColor;
          Inc(PixelPtr, 4);
            // Hoppa över RGBA (4 bytes per pixel)
        end;
        Inc(pbyte(BitmapData), BitmapStride);
        // Nästa rad i WIC-bitmapen
      end;
    finally
      WICLock := nil;
    // Släpp låset på IWICBitmap
    end;
  end;
begin
  // Skapa en TBitmap som ska visa resultatet senare
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(Image.Width, Image.Height);

    // Skapa en WIC-fabrik
    hr := CoCreateInstance(CLSID_WICImagingFactory, nil, 1, IID_IWICImagingFactory, WICFactory);
    if Failed(hr) then
    begin
      ShowMessage(dlgErr+ '  ('+ IntToStr(hr) + ')');
      Exit;
    end;

    // Skapa en IWICBitmap som vi kommer att använda som render-target
    hr := WICFactory.CreateBitmap(Image.Width, Image.Height, GUID_WICPixelFormat32bppPBGRA,
      WICBitmapCacheOnLoad, WICBitmap);
    if Failed(hr) then
    begin
      ShowMessage(dlgErr+ '  ('+ IntToStr(hr) + ')');
      Exit;
    end;

    // Skapa en Direct2D-fabrik
    if Failed(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, ID2D1Factory, nil, D2DFactory))
    then
    begin
      ShowMessage(dlgErr);
      Exit;
    end;

    // Skapa en DirectWrite-fabrik
    if Failed(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, IUnknown(DWFactory)))
    then
    begin
      ShowMessage(dlgErr);
      Exit;
    end;

    // Ställ in render target-egenskaper
    RenderProps.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
    RenderProps.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
    RenderProps.dpiX := 75.0;
    RenderProps.dpiY := 75.0;
    RenderProps._type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
    RenderProps.usage := D2D1_RENDER_TARGET_USAGE_NONE;
    RenderProps.minLevel := D2D1_FEATURE_LEVEL_DEFAULT;

    // Skapa render target för att rita på IWICBitmap
    hr := D2DFactory.CreateWicBitmapRenderTarget(WICBitmap, RenderProps, RenderTarget);
    if Failed(hr) then
    begin
      ShowMessage(dlgErr + ' (' + IntToStr(hr) + ')');
      Exit;
    end;

    // Skapa en solid färgborste för bakgrunden
    if not dark then
      BackgroundColor := ColorF(1.0, 1.0, 1.0, 1.0)
    else
      BackgroundColor := ColorF(39/255, 43/255, 50/255, 1.0);

    // Vit bakgrund
    if Failed(RenderTarget.CreateSolidColorBrush(@BackgroundColor, nil, BackgroundBrush)) then
    begin
      ShowMessage(dlgErr);
      Exit;
    end;

    // Skapa textformat för emoji
    if Failed(DWFactory.CreateTextFormat(pwidechar('Segoe UI Emoji'), nil, DWRITE_FONT_WEIGHT_NORMAL
      ,
      DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL,
      48, 'en-us', TextFormat)) then
    begin
      ShowMessage(dlgErr);
      Exit;
    end;

    // Definiera rektangeln för texten
    TextRect := RectF(0, 0, Image.Width, Image.Height);

    // Börja rita på render target
    RenderTarget.BeginDraw;
    try
      // Fyll bakgrunden med vit färg
      RenderTarget.Clear(BackgroundColor);

      // Rita emoji-texten i färg
      RenderTarget.DrawText(pwidechar(Emoji), Length(Emoji), TextFormat, @TextRect, BackgroundBrush,
        D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT, DWRITE_MEASURING_MODE_NATURAL);
      hr := RenderTarget.EndDraw;

      wicToBitmap;
      if Failed(hr) then
        ShowMessage(Format( '%s (%d)', [dlgErr, hr]));
    except
      ShowMessage(dlgErr);
    end;

    Image.Picture.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;
{$else}
procedure AssignEmoji(Image: TImage; const Emoji: widestring; dark: boolean = false);
begin
  // Ställ in storleken på Canvas och bakgrundsfärgen
  Image.Picture.Bitmap.SetSize(Image.Width, Image.Height);
  Image.Picture.Bitmap.Canvas.Brush.Color := IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);
  Image.Picture.Bitmap.Canvas.FillRect(0, 0, Image.Width, Image.Height);

  // Ställ in teckensnittet (här använder vi ett standardteckensnitt)
  Image.Picture.Bitmap.Canvas.Font.Name := 'Noto Color Emoji';
  // Ett populärt emoji-teckensnitt för Linux
  Image.Picture.Bitmap.Canvas.Font.Size := 48;
  {$if DEFINED(LCLQt6) OR DEFINED(LCLGTK2)}
     Image.Picture.Bitmap.Canvas.Font.Size := 30;
  {$endif}
  // Justera storleken efter behov
  Image.Picture.Bitmap.Canvas.Font.Color :=  IfThen(TrndiNative.isDarkMode, clWhite, clBlack);;

  // Rita emojin/texten centrerat på Canvas
  Image.Picture.Bitmap.Canvas.TextOut(
    (Image.Width - Image.Picture.Bitmap.Canvas.TextWidth(Emoji))
    div 2,
    (Image.Height - Image.Picture.Bitmap.Canvas.TextHeight(Emoji)) div 2,
    Emoji
    );
end;
{$endif}

function ExtSucc(const msg, desc, output: string; dumpbg: TColor = $0095EEC4; dumptext: TColor = $00147C4A; const icon: widechar = widechar($2705)): TModalResult;
begin
  result := ExtMsg(sSuccTitle, msg,  desc, output, $00AA6004, $00FDD8AA, [mbOK], widechar(icon));
end;


function ExtLog(const caption, msg, log: string; const icon: widechar = widechar($2699)):
TModalResult
;
begin
  result := ExtMsg(sMsgTitle, caption, msg, log, $00AA6004, $00FDD8AA, [mbOK], widechar(icon));
end;

function ExtError(const error: string; const icon: widechar = widechar($2699)): TModalResult;
begin
  result := ExtMsg(sExtErr, sExtTitle, sErr, error, $00F5F2FD, $003411A9, [mbAbort], icon);
end;

function ExtError(const msg, error: string; const icon: widechar = widechar($2699)): TModalResult;
begin
  result := ExtMsg(sExtErr, sErr, msg, error, $00F5F2FD, $003411A9, [mbAbort], icon);
end;

procedure UXMessage(const title, message: string; const icon: widechar = widechar($2705));
begin
  ExtMsg(sMsgTitle, title, message, '', $00AA6004, $00FDD8AA, [mbOK], widechar(icon));
end;

function UXDialog(const title, message: string; buttons: TUXMsgDlgBtns; const icon: widechar = widechar($2705)): TModalResult;
begin
  result := ExtMsg(sMsgTitle, title, message, '', $00AA6004, $00FDD8AA, buttons, widechar(icon));
end;

function UXDialog(const title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;
begin
  result := UXDialog(sMsgTitle, title, message, buttons, mtype);
end;

function UXDialog(const header, title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;
var
 icon: widechar;
begin

case mtype of
  mtWarning:
    icon := widechar($26A0); // ⚠️ WARNING SIGN
  mtError:
    icon := widechar($274C); // ❌ CROSS MARK
  mtInformation:
    icon := widechar($2139); // ℹ️ INFORMATION SOURCE
  mtConfirmation:
    icon := widechar($2753); // ❓ BLACK QUESTION MARK ORNAMENT
  mtCustom:
    icon := widechar($2699); // ⚙️ GEAR
else
//  icon := widechar('');
end;
 result := ExtMsg(header, title, message, '', $00AA6004, $00FDD8AA, buttons, icon);
end;

procedure btnmodal(sender: tbutton);
begin

end;

{$ifdef windows}
procedure TDialogForm.ButtonDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TButtonState);
var
  Btn: TBitBtn absolute Sender;
  TxtFlags: Cardinal;
begin
  // 1) Bakgrund
  if bsDown = State then
    ACanvas.Brush.Color := RGBToColor(30,30,30)
  else
    ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(ARect);

  // 2) Ram
  ACanvas.Pen.Color := RGBToColor(80,80,80);
  ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  // 3) Text
  ACanvas.Font.Assign(Btn.Font);
  ACanvas.Font.Color := clWhite;
  TxtFlags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  DrawText(ACanvas.Handle, PChar(Btn.Caption), Length(Btn.Caption),
    ARect, TxtFlags);

  // 4) Fokusindikator
  if bsHot = State then
    ACanvas.DrawFocusRect(ARect);
end;

{$endif}

function ExtInput(
  const ACaption, ATitle, ADesc, ADefault: string;
  var ModalResult: TModalResult;
  const icon: WideChar = WideChar($2699)
): string;
const
  Padding = 16;
  IconSize = 48;
  InputWidth = 260;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Edit: TEdit;
  OkButton, CancelButton: TButton;
  bgcol: TColor;
begin
  Result := '';
  ModalResult := mrCancel; // Default

  // Bakgrundsfärg beroende på darkmode
  bgcol := IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;
    Dialog.ClientWidth := InputWidth + IconSize + 4 * Padding;
    Dialog.Color := bgcol;

    // Emoji-ikon
    IconBox := TImage.Create(Dialog);
    IconBox.Parent := Dialog;
    IconBox.Width := IconSize;
    IconBox.Height := IconSize;
    IconBox.Left := Padding;
    IconBox.Top := Padding;
    IconBox.Color := bgcol;
    {$ifdef Windows}
    IconBox.Font.Name := 'Segoe UI Emoji';
    {$endif}
    Dialog.HandleNeeded;
    AssignEmoji(IconBox, icon, TrndiNative.isDarkMode);

    // Titel (fetstil)
    TitleLabel := TLabel.Create(Dialog);
    TitleLabel.Parent := Dialog;
    TitleLabel.Caption := ATitle;
    TitleLabel.AutoSize := True;
    TitleLabel.Font.Style := [fsBold];
    TitleLabel.Left := IconBox.Left + IconBox.Width + Padding;
    TitleLabel.Top := Padding;
    TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Förklaring (normal)
    DescLabel := TLabel.Create(Dialog);
    DescLabel.Parent := Dialog;
    DescLabel.Caption := ADesc;
    DescLabel.AutoSize := True;
    DescLabel.Font.Style := [];
    DescLabel.Left := TitleLabel.Left;
    DescLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding div 10;
    DescLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Inmatningsruta (TEdit)
    Edit := TEdit.Create(Dialog);
    Edit.Parent := Dialog;
    Edit.Left := TitleLabel.Left;
    Edit.Width := Max(InputWidth, DescLabel.Canvas.TextWidth(ADesc));

    Edit.Top := DescLabel.Top + DescLabel.Height + Padding div 2;
    Edit.Text := ADefault;


    // OK-knapp
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbSelect;
    OkButton.ModalResult := mrOk;

    OkButton.Width := 80;
    {$ifdef Windows}
    OkButton.SetFocus;
    {$endif}

    // Avbryt-knapp
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.Caption := smbUXCancel;
    CancelButton.ModalResult := mrCancel;
    CancelButton.Width := 80;

    OkButton.Top := Edit.Top + Edit.Height + Padding * 2;
    CancelButton.Top := OkButton.Top;
    OkButton.Left := Edit.Left + (Edit.Width div 2) - OkButton.Width - Padding div 2;
    CancelButton.Left := Edit.Left + (Edit.Width div 2) + Padding div 2;

    // Dialogstorlek
    Dialog.ClientHeight := OkButton.Top + OkButton.Height + Padding;
    Dialog.ClientWidth := Max(
      Edit.Left + Edit.Width + Padding,
      IconBox.Left + IconBox.Width + Padding
    );
    Dialog.ClientWidth := Max(Dialog.ClientWidth, DescLabel.Width + DescLabel.left + Padding div 2);
    Dialog.Color := bgcol;

    Dialog.ActiveControl := Edit;

    ModalResult := Dialog.ShowModal;
    if ModalResult = mrOk then
      Result := Edit.Text;
  finally
    Dialog.Free;
  end;
end;



function ExtList(
  const ACaption, ATitle, ADesc: string;
  const Choices: array of string;
  const icon: WideChar = WideChar($2699)
): Integer;
const
  Padding = 16;
  IconSize = 48;
  ComboWidth = 500;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Combo: TComboBox;
  OkButton, CancelButton: TButton;
  bgcol: TColor;
  i: Integer;
begin
  Result := -1;

  // Bakgrundsfärg beroende på darkmode
  bgcol := IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;
    Dialog.ClientWidth := ComboWidth + IconSize + 4 * Padding;
    Dialog.Color := bgcol;

    // Emoji-ikon
    IconBox := TImage.Create(Dialog);
    IconBox.Parent := Dialog;
    IconBox.Width := IconSize;
    IconBox.Height := IconSize;
    IconBox.Left := Padding;
    IconBox.Top := Padding;
    IconBox.Color := bgcol;
    {$ifdef Windows}
    IconBox.Font.Name := 'Segoe UI Emoji';
    {$endif}
    Dialog.HandleNeeded;
    AssignEmoji(IconBox, icon, TrndiNative.isDarkMode);

    // Titel (fet)
    TitleLabel := TLabel.Create(Dialog);
    TitleLabel.Parent := Dialog;
    TitleLabel.Caption := ATitle;
    TitleLabel.AutoSize := True;
    TitleLabel.Font.Style := [fsBold];
    TitleLabel.Left := IconBox.Left + IconBox.Width + Padding;
    TitleLabel.Top := Padding;
    TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Förklaring (normal)
    DescLabel := TLabel.Create(Dialog);
    DescLabel.Parent := Dialog;
    DescLabel.Caption := ADesc;
    DescLabel.AutoSize := True;
    DescLabel.Font.Style := [];
    DescLabel.Left := TitleLabel.Left;
    DescLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding div 10;
    DescLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Dropdown
    Combo := TComboBox.Create(Dialog);
    Combo.Parent := Dialog;
    for i := 0 to High(Choices) do
      Combo.Items.Add(Choices[i]);
    combo.ReadOnly := true;
    combo.style := csDropDownList;
    Combo.Left := TitleLabel.Left;
    Combo.Width := Max(ComboWidth, DescLabel.Canvas.TextWidth(ADesc));
    Combo.Top := DescLabel.Top + DescLabel.Height + Padding div 2;
    Combo.ItemIndex := 0;

    // OK-knapp
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbSelect;
    OkButton.ModalResult := mrOk;
    OkButton.Width := 80;
    {$ifdef Windows}
      OkButton.SetFocus;
    {$endif}

    // Avbryt-knapp
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.Caption := 'Avbryt';
    CancelButton.ModalResult := mrCancel;
    CancelButton.Width := 80;

    // Knapparnas placering
    OkButton.Top := Combo.Top + Combo.Height + Padding * 2;
    CancelButton.Top := OkButton.Top;
    OkButton.Left := Combo.Left + (Combo.Width div 2) - OkButton.Width - Padding div 2;
    CancelButton.Left := Combo.Left + (Combo.Width div 2) + Padding div 2;

    // Dialogstorlek
    Dialog.ClientHeight := OkButton.Top + OkButton.Height + Padding;
    Dialog.ClientWidth := Max(
      Combo.Left + Combo.Width + Padding,
      IconBox.Left + IconBox.Width + Padding
    );
    Dialog.Color := bgcol;

    Dialog.ActiveControl := Combo;

    if Dialog.ShowModal = mrOk then
      Result := Combo.ItemIndex
    else
      Result := -1;
  finally
    Dialog.Free;
  end;
end;

function ExtMsg(const caption, title, desc, logmsg: string; dumpbg: TColor = $00F5F2FD; dumptext:
TColor = $003411A9; buttons: TUXMsgDlgBtns = [mbAbort]; const icon: widechar =
widechar($2699)): TModalResult;

const
  btnWidth = 75;
  padding = 10;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel: TLabel;
  MessageLabel: TLabel;
  log: TMemo;
  logPanel: TPanel;
  OkButton: {$ifdef Windows}TBitBtn{$else}TButton{$endif};
  ContentWidth: integer;
  p2, p3: TPanel;
  mr: TUXMsgDlgBtn;
  last: integer;
  bgcol: TColor;
begin
  bgcol :=  IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);
  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := caption;
    Dialog.BorderStyle := bsDialog;
    {$ifdef LCLGTK3}
       Dialog.BorderStyle := bsSizeable;
    {$endif}
    Dialog.Position := poWorkAreaCenter;

    // Huvudpanelen
    p2 := TPanel.Create(Dialog);
    p2.Parent := Dialog;
    p2.Align := alClient;
    p2.Color :=  bgcol;
    p2.BevelInner := bvNone;
    p2.BevelOuter := bvNone;

    // Emoji-ikon som visas i en TImage
    IconBox := TImage.Create(p2);
    IconBox.Parent := p2;
    IconBox.Width := 50;
    IconBox.Height := IconBox.Width;
    IconBox.Left := Padding;
    IconBox.Top := Padding;
    {$ifdef Windows}
    IconBox.Font.Name := 'Segoe UI Emoji';
    {$endif}
    //    IconBox.Align := alleft;

    Dialog.HandleNeeded;
    // Rita emoji på IconBox
    AssignEmoji(IconBox, icon, TrndiNative.isDarkMode);

    // Titel
    TitleLabel := TLabel.Create(p2);
    TitleLabel.Parent := p2;
    TitleLabel.Caption := Title;
    TitleLabel.Font.Style := [fsBold];
    TitleLabel.AutoSize := true;
    TitleLabel.Left := IconBox.Left + IconBox.Width + Padding;
    TitleLabel.Top := Padding;
    TitleLabel.Font.Color :=  IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Meddelandetext
    MessageLabel := TLabel.Create(p2);
    MessageLabel.Parent := p2;
    MessageLabel.Caption := desc;
    MessageLabel.AutoSize := true;
    MessageLabel.Left := TitleLabel.Left;
    MessageLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding div 2;
    MessageLabel.Font.Color :=  IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
    //  MessageLabel.Font.Size := 8;


    logPanel := TPanel.Create(p2);

    with logPanel do
    begin
      Width := ClientWidth;
      left := 0;
      Height := 50;
      Parent := p2;
      align := alBottom;
      Color := dumpbg;
      top := Dialog.ClientHeight-height;
    end;

    //logPanel.top := p2.ClientHeight-50;

    log := TMemo.Create(logPanel);

    p2.height := p2.height+logPanel.height;

    with log do
    begin
      Text := TrimSet(logmsg, [#10,#13]);
      parent := logpanel;
      ReadOnly := true;
      Color := dumpbg;
      font.color := dumptext;
      ScrollBars := ssNone;
      BorderStyle := bsNone;
      ControlStyle := [csNoFocus];
      left := IconBox.Left;
      width := logpanel.ClientWidth - 20;
      top := 10;
      left := 10;
    end;

    ///--
         //For UX Message
    if logmsg = '' then
    begin
      logpanel.Visible := false;
      dialog.ClientHeight := dialog.ClientHeight - logpanel.height;
    ///--
    end;





    // Bottenpanelen för knappen
    p3 := TPanel.Create(Dialog);
    p3.Parent := Dialog;
    p3.Align := alBottom;
    p3.BorderStyle := bsNone;
    p3.BevelOuter := bvNone;
    p3.color := bgcol;
    p3.name := 'pnButtons';
    p3.caption := '';

    last := p3.ClientWidth - Padding;
    // OK-knapp
    for mr in buttons do
    begin
      {$ifdef Windows}
        OkButton := TBitBtn.Create(p3);
      {$else}
        OkButton := TButton.Create(p3);
      {$endif}
      OkButton.Parent := p3;
      OkButton.Caption := langs[mr];

      OkButton.ModalResult := UXButtonToModalResult(mr);
      if OkButton.ModalResult = mrOK then begin
        {$ifdef Windows}
          OkButton.SetFocus;
        {$endif}
      end;
      OkButton.Width := btnwidth;
      OkButton.Top := (p3.Height div 2) - (OkButton.Height div 2);
      OkButton.left := last-padding-btnwidth;
      okbutton.name := 'btn'+IntToStr(ord(mr));
      last := OkButton.left;
    end;

    // Nu kan vi beräkna p3.Height efter att OkButton har skapats
    p3.Height := Padding * 2 + OkButton.Height;

    // Beräkna dialogens bredd och höjd baserat på innehållet
    ContentWidth := Max(Max(Dialog.Canvas.TextWidth(TitleLabel.Caption), Dialog.Canvas.TextWidth(
      MessageLabel.Caption)), 300 (* Log size *)) + IconBox.Width + Padding * 5;
    log.Width := ContentWidth;
    Dialog.Width := ContentWidth;
    Dialog.Height := Padding * 2 + MessageLabel.Top + MessageLabel.Height + p3.Height;
    // Set first (last?) button pos
    last := Dialog.Width;
    for mr in buttons do
    begin
      OkButton := p3.FindChildControl('btn'+IntToStr(ord(mr))) as {$ifdef Windows}TBitBtn{$else}TButton{$endif};

      OkButton.Left := last - padding - btnWidth;
      last := OkButton.left;
    end;

    log.Text := string(log.Text).TrimRight([#13, #10]);
    log.Height := dialog.Canvas.TextHeight(logmsg) * (string(log.Text).CountChar(#10)+1);
    logpanel.height := log.Height+20;
    Dialog.height := Dialog.Height + logPanel.height;

(*    IconBox.Center := true;
    IconBox.Proportional := True; *)
    // Visa dialogen som en modal dialog
    Dialog.ShowModal;
    result := dialog.ModalResult;
  finally
    p2.Free;
    p3.Free;
    Dialog.Free;
  end;
end;

end.
