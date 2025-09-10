
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

{$I native.inc}
{$modeswitch advancedrecords}
interface

uses
Classes, SysUtils, Dialogs, Forms, ExtCtrls, StdCtrls, Controls, Graphics, Math,
IntfGraphics, FPImage, graphtype, lcltype, Trndi.Native, Grids,
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
smbUxAgree = 'Agree';
smbUxRead = 'Read...';

sKey = 'Key';
sValue = 'Value';

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
  procedure UXMessageOnClick(sender: TObject);
end;

TUXMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
  mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose, mbUXOpenFile, mbUXMinimize, mbUXAgree, mbUXRead);

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
langs : ButtonLangs = (smbYes, smbUXNo, smbUXOK, smbUXCancel, smbUXAbort, smbUXRetry, smbUXIgnore, smbUXAll, smbUXNoToAll, smbUXYesToAll, smbUXHelp, smbUXClose, smbUXOpenFile, smbUXMinimize, smbUXAgree, smbUXRead);


//function UXShowMessage(const message: string; buttons: TMsgDlgButtons; const icon: Widechar): TModalResult;

procedure UXMessage(const title, message: string; const icon: widechar = widechar($2705); sender: TForm = nil; onForm: boolean = false);
function UXDialog(const big: boolean; const title, message: string; buttons: TUXMsgDlgBtns; const icon: widechar = widechar($2705)): TModalResult;
function UXDialog(const big: boolean; const title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;
function UXDialog(const big: boolean; const header, title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;

//function UXShowMessage(const caption, title, desc, message: string; buttons: TMsgDlgButtons; const icon: WideChar): TModalResult;
function ExtMsg(const big: boolean; const  caption, title, desc, logmsg: string; dumpbg: TColor = $00F5F2FD; dumptext:
TColor = $003411A9; buttons: TUXMsgDlgBtns = [mbAbort]; const icon: widechar =
widechar($2699); scale: integer = 1): TModalResult;
function ExtLog(const big: boolean; const caption, msg, log: string; const icon: widechar = widechar($2699); scale: integer = 1):
TModalResult
;
function ExtList(
  const ACaption, ATitle, ADesc: string;
  const Choices: array of string;
  const big: boolean;
  const icon: WideChar = WideChar($2699)
): Integer;
function ExtInput(
  const ACaption, ATitle, ADesc, ADefault: string;
  var ModalResult: TModalResult;
  const icon: WideChar = WideChar($2699)
): string;

function ExtTable(
  const ACaption, ATitle, ADesc: string;
  const Keys, Values: array of string;
  const icon: WideChar = WideChar($2699);
  const key: string = '';
  const value: string = ''
): Integer;
function ExtError(const big: boolean; const msg, error: string; const icon: widechar = widechar($2699)): TModalResult;
function ExtError(const big: boolean; const error: string; const icon: widechar = widechar($2699)): TModalResult;
function ExtSucc(const big: boolean; const msg, desc, output: string; dumpbg: TColor = $0095EEC4; dumptext: TColor = $00147C4A; const icon: widechar = widechar($2705)): TModalResult;
function ExtSuccEx(const big: boolean; const msg, desc, output: string; btns: TUXMsgDlgBtns; dumpbg: TColor = $0095EEC4; dumptext: TColor = $00147C4A; const icon: widechar = widechar($2705)): TModalResult;
function FontInList(out fname: string): Boolean;
{$ifdef Windows}
function CoCreateInstance(const clsid: TGUID; unkOuter: IUnknown; dwClsContext: longint;
const iid: TGUID; out pv): HResult;
stdcall;
external 'ole32.dll';
{$endif}

implementation

{$if DEFINED(X_LINUXBSD)}
function FontInList(out fname: string): Boolean;
begin
  fname := 'Noto Color Emoji';
  try
    Result := (Screen.Fonts.IndexOf('Noto Emoji') >= 0) or (Screen.Fonts.IndexOf('Noto Color Emoji') >= 0);
  finally
  end;
end;
{$elseif DEFINED(WINDOWS)}
function FontInList(out fname: string): Boolean;
begin
  fname := 'Consolas';
  try
    Result := Screen.Fonts.IndexOf(fname) >= 0;
  finally

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
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and (Win32BuildNumber < 17763)) then
    Exit; // Dark mode stöds först i Windows 10 1809 (build 17763)
  Value := 1;
  // Nu är Handle giltigt – anropa DWM
  try DwmSetWindowAttribute(Handle,
    DWMWA_USE_IMMERSIVE_DARK_MODE, @Value, SizeOf(Value)) except end;
  // Tvinga omritning av non-client area
  SetWindowPos(Handle, 0,0,0,0,0,
    SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
end;

function TColorToColorF(const Col: TColor; Alpha: Single = 1.0): TD2D1_COLOR_F;
var
  rgb: TColor;
begin
  // Ensure proper RGB order
  rgb := ColorToRGB(Col);

  Result.R := GetRValue(rgb) / 255.0;
  Result.G := GetGValue(rgb) / 255.0;
  Result.B := GetBValue(rgb) / 255.0;
  Result.A := Alpha;
end;

procedure AssignEmoji(Image: TImage; const Emoji: widestring; bgcol: TColor = clWhite);

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
(*    if not dark then
      BackgroundColor := ColorF(1.0, 1.0, 1.0, 1.0)
    else
      BackgroundColor := ColorF(39/255, 43/255, 50/255, 1.0); *)

    BackgroundColor := TColorToColorF(bgcol);

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
      image.Width , 'en-us', TextFormat)) then
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
procedure AssignEmoji(Image: TImage; const Emoji: widestring; bgcol: TColor = clWhite);
begin
  // Ställ in storleken på Canvas och bakgrundsfärgen
  Image.Picture.Bitmap.SetSize(Image.Width, Image.Height);
  Image.Picture.Bitmap.Canvas.Brush.Color := bgcol;
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

function ExtSuccEx(const big: boolean; const msg, desc, output: string; btns: TUXMsgDlgBtns; dumpbg: TColor = $0095EEC4; dumptext: TColor = $00147C4A; const icon: widechar = widechar($2705)): TModalResult;
begin
  result := ExtMsg(big, sSuccTitle, msg,  desc, output, dumpbg, dumptext, btns, widechar(icon));
end;

function ExtSucc(const big: boolean; const msg, desc, output: string; dumpbg: TColor = $0095EEC4; dumptext: TColor = $00147C4A; const icon: widechar = widechar($2705)): TModalResult;
begin
  result := ExtMsg(big, sSuccTitle, msg,  desc, output, dumpbg, dumptext, [mbOK], widechar(icon));
end;


function ExtLog(const big: boolean; const caption, msg, log: string; const icon: widechar = widechar($2699); scale: integer = 1):
TModalResult
;
begin
  result := ExtMsg(big, sMsgTitle, caption, msg, log, $00AA6004, $00FDD8AA, [mbOK], widechar(icon), scale);
end;

function ExtError(const big: boolean; const error: string; const icon: widechar = widechar($2699)): TModalResult;
begin
  result := ExtMsg(big, sExtErr, sExtTitle, sErr, error, $00F5F2FD, $003411A9, [mbAbort], icon);
end;

function ExtError(const big: boolean; const msg, error: string; const icon: widechar = widechar($2699)): TModalResult;
begin
  result := ExtMsg(big, sExtErr, sErr, msg, error, $00F5F2FD, $003411A9, [mbAbort], icon);
end;

procedure TDialogForm.UXMessageOnClick(sender: TObject);
begin
  ((sender as TButton).parent as TPanel).Hide;
  ((sender as TButton).parent as TPanel).Free;
  self.Free;
end;

procedure UXMessage(const title, message: string; const icon: widechar = widechar($2705); sender: TForm = nil; onForm: boolean = false);
var
  tp: TPanel;
  tb: TButton;
  df: TDialogForm;
begin
  if onForm and (sender <> nil) and (sender.showing) then begin
    // On eg touch screens display a full screen message
    tp := TPanel.Create(sender); // Create a panel to cover the screen
    tp.Parent := sender;
    tp.top := 0;
    tp.left := 0;
    tp.height := sender.height;
    tp.width := sender.width;
    tp.BringToFront;
    tp.color := $00FDD8AA;
    tp.font.color := $00AA6004;

    tp.caption := message;
    tp.font.Size := tp.width div 25;
    tp.WordWrap := true;
    tb := TButton.Create(tp);
    tb.parent := tp;
    tb.AutoSize := true;
    tb.Caption := smbUXOK;

    if tb.height < (tp.height div 5) then begin
      tb.AutoSize := false;
      tb.height := tp.height div 5;
    end;
    tb.left := 0;
    tb.width := tp.width;
    tb.Top := tp.height-tb.height-10;
    tb.Font.Color := sender.font.color; // This will be blue or such on Linux otherwise
    //tb.left := (tp.width div 2) - (tb.width div 2); // Center the button
    // Note the dialog wont resize with the window, that's known

    df := TDialogForm.CreateNew(nil);
    tb.OnClick := @df.UXMessageOnClick;
  end else
    ExtMsg(false, sMsgTitle, title, message, '', $00AA6004, $00FDD8AA, [mbOK], widechar(icon));
end;

function UXDialog(const big: boolean; const title, message: string; buttons: TUXMsgDlgBtns; const icon: widechar = widechar($2705)): TModalResult;
begin
  result := ExtMsg(big, sMsgTitle, title, message, '', $00AA6004, $00FDD8AA, buttons, widechar(icon));
end;

function UXDialog(const big: boolean; const title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;
begin
  result := UXDialog(big, sMsgTitle, title, message, buttons, mtype);
end;

function UXDialog(const big: boolean; const header, title, message: string; buttons: TUXMsgDlgBtns; const mtype: TMsgDlgType): TModalResult;
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
 result := ExtMsg(big, header, title, message, '', $00AA6004, $00FDD8AA, buttons, icon);
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
    AssignEmoji(IconBox, icon, bgcol);

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

function ExtTable(
  const ACaption, ATitle, ADesc: string;
  const Keys, Values: array of string;
  const icon: WideChar = WideChar($2699);
  const key: string = '';
  const value: string = ''
): Integer;
const
  Padding = 16;
  IconSize = 48;
  GridHeight = 200;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Grid: TStringGrid;
  BgCol: TColor;
  OkButton, CancelButton: TButton;
  i: Integer;
begin
  Result := -1;
  // Anpassa bakgrundsfärgen för dark mode
  BgCol := IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);

  // Skapa dialogrutan
  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;
    Dialog.ClientWidth := 500 + IconSize + 4 * Padding;
    Dialog.Color := BgCol;

    // Emoji-ikon
    IconBox := TImage.Create(Dialog);
    IconBox.Parent := Dialog;
    IconBox.Width := IconSize;
    IconBox.Height := IconSize;
    IconBox.Left := Padding;
    IconBox.Top := Padding;

    {$IFDEF Windows}
    IconBox.Font.Name := 'Segoe UI Emoji';
    {$ENDIF}

    Dialog.HandleNeeded;
    AssignEmoji(IconBox, icon, bgcol);

    // Titel
    TitleLabel := TLabel.Create(Dialog);
    TitleLabel.Parent := Dialog;
    TitleLabel.Caption := ATitle;
    TitleLabel.AutoSize := True;
    TitleLabel.Font.Style := [fsBold];
    TitleLabel.Left := IconBox.Left + IconBox.Width + Padding;
    TitleLabel.Top := Padding;
    TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Beskrivning
    DescLabel := TLabel.Create(Dialog);
    DescLabel.Parent := Dialog;
    DescLabel.Caption := ADesc;
    DescLabel.AutoSize := True;
    DescLabel.Left := TitleLabel.Left;
    DescLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding div 2;
    DescLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Skapa tabellen
    Grid := TStringGrid.Create(Dialog);
    Grid.Parent := Dialog;
    Grid.Left := Padding;
    Grid.Top := DescLabel.Top + DescLabel.Height + Padding;
    Grid.Width := Dialog.ClientWidth - 2 * Padding;
    Grid.Height := GridHeight;
    Grid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing];
    Grid.ColCount := 2;
    Grid.RowCount := Length(Keys) + 1;

    Grid.Cells[0, 0] := IfThen(key = '', sKEY, key);
    Grid.Cells[1, 0] := IfThen(value = '', sVALUE, value);

    for i := 0 to High(Keys) do
    begin
      Grid.Cells[0, i + 1] := Keys[i];
      Grid.Cells[1, i + 1] := Values[i];
    end;

    // OK-knapp
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbUXOK;
    OkButton.ModalResult := mrOk;
    OkButton.Width := 80;

    // Avbryt-knapp
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.Caption := smbUXCancel;
    CancelButton.ModalResult := mrCancel;
    CancelButton.Width := 80;

    // Placera knapparna
    OkButton.Top := Grid.Top + Grid.Height + Padding;
    CancelButton.Top := OkButton.Top;

    OkButton.Left := Grid.Left + (Grid.Width div 2) - OkButton.Width - Padding div 2;
    CancelButton.Left := Grid.Left + (Grid.Width div 2) + Padding div 2;

    Dialog.ClientHeight := OkButton.Top + OkButton.Height + Padding;

    // Visa dialogen
    if Dialog.ShowModal = mrOk then
      Result := Grid.Row; // Returnera vald rad
  finally
    Dialog.Free;
  end;
end;

function ExtList(
  const ACaption, ATitle, ADesc: string;
  const Choices: array of string;
  const big: boolean;
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
    if big then begin
      IconBox.Width := IconSize*2;
      IconBox.Height := IconSize*2;
    end;
    IconBox.Left := Padding;
    IconBox.Top := Padding;
    IconBox.Color := bgcol;
    {$ifdef Windows}
    IconBox.Font.Name := 'Segoe UI Emoji';
    {$endif}
    Dialog.HandleNeeded;
    AssignEmoji(IconBox, icon, bgcol);

    // Titel (fet)
    TitleLabel := TLabel.Create(Dialog);
    TitleLabel.Parent := Dialog;
    TitleLabel.Caption := ATitle;
    TitleLabel.AutoSize := True;
    TitleLabel.Font.Style := [fsBold];
    if big then
       TitleLabel.Font.Size := 24;
    TitleLabel.AdjustSize;
    TitleLabel.Left := IconBox.Left + IconBox.Width + Padding;
    TitleLabel.Top := Padding;
    TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

    // Förklaring (normal)
    DescLabel := TLabel.Create(Dialog);
    DescLabel.Parent := Dialog;
    DescLabel.Caption := ADesc;
    DescLabel.AutoSize := True;
    DescLabel.Font.Style := [];
    if big then
       DescLabel.Font.Size := 24;
    DescLabel.AdjustSize;

    DescLabel.Left := TitleLabel.Left;
    DescLabel.Top := TitleLabel.Top +
                 TitleLabel.Canvas.TextHeight(TitleLabel.Caption) +
                 Padding div 10;
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
    Combo.Top := DescLabel.Top + DescLabel.Canvas.TextHeight(DescLabel.Caption) + Padding div 2;
    Combo.ItemIndex := 0;

    if big then
    begin
      Combo.Font.Size := 20;
      {$IFDEF Windows}
        Combo.Height := 42; // needed on Windows to match bigger font
      {$ENDIF}
    end;

    // OK-knapp
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbSelect;
    OkButton.ModalResult := mrOk;
    OkButton.Width := 80;
    {$ifdef Windows}
      OkButton.SetFocus;
    {$endif}
    if big then begin
      OkButton.width :=  OkButton.width*2;
      OkButton.height := OkButton.height*2;
    end;


    // Avbryt-knapp
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.Caption := smbUXCancel;
    CancelButton.ModalResult := mrCancel;
    CancelButton.Width := 80;
    if big then begin
      CancelButton.width :=  CancelButton.width*2;
      CancelButton.height := CancelButton.height*2;
    end;

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

function MeasureWrappedHeight(const AText: string; AFont: TFont; MaxWidth: Integer): Integer;
var
  bmp: Graphics.TBitmap;
  words: TStringList;
  line: string;
  i: Integer;
  lineCount: Integer;
begin
  bmp := Graphics.TBitmap.Create;
  words := TStringList.Create;
  try
    bmp.Canvas.Font.Assign(AFont);
    words.StrictDelimiter := True;
    words.Delimiter := ' ';
    words.DelimitedText := AText;

    line := '';
    lineCount := 1;

    for i := 0 to words.Count - 1 do
    begin
      if bmp.Canvas.TextWidth(Trim(line + ' ' + words[i])) > MaxWidth then
      begin
        Inc(lineCount);
        line := words[i];
      end
      else
        line := Trim(line + ' ' + words[i]);
    end;

    Result := lineCount * bmp.Canvas.TextHeight('Hg');
  finally
    bmp.Free;
    words.Free;
  end;
end;

function ExtMsg(
  const big: boolean;
  const caption, title, desc, logmsg: string;
  dumpbg: TColor = $00F5F2FD;
  dumptext: TColor = $003411A9;
  buttons: TUXMsgDlgBtns = [mbAbort];
  const icon: widechar = widechar($2699);
  scale: integer = 1
): TModalResult;
const
  btnWidth = 75;
  padding  = 10;
var
  Dialog: TDialogForm;
  MainPanel, TopPanel, TextPanel, LogPanel, ButtonPanel: TPanel;
  IconBox: TImage;
  TitleLabel, MsgLabel: TLabel;
  MsgScroll: TScrollBox;
  LogMemo: TMemo;
  OkButton: {$ifdef Windows}TBitBtn{$else}TButton{$endif};
  mr: TUXMsgDlgBtn;
  ButtonActualWidth, MaxDialogHeight, MsgWidth, NeededHeight,
  TitlePixelWidth, DescPixelWidth, TextPixelWidth,
  posX, ProposedWidth, btnCount, totalBtnWidth: Integer;
  bgcol: TColor;
  TempFont: TFont;
begin
  bgcol := IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);

  Dialog := TDialogForm.CreateNew(nil);
  try
    Dialog.Caption := caption;
    Dialog.BorderStyle := bsDialog;
    {$ifdef LCLGTK3}Dialog.BorderStyle := bsSizeable;{$endif}
    Dialog.Position := poWorkAreaCenter;
    Dialog.Color := bgcol;
    Dialog.AutoSize := True;
    MaxDialogHeight := Round(Screen.Height * 0.8);

    // Main panel
    MainPanel := TPanel.Create(Dialog);
    MainPanel.Parent := Dialog;
    MainPanel.Align := alClient;
    MainPanel.BevelOuter := bvNone;
    MainPanel.Color := bgcol;
    MainPanel.AutoSize := True;

    // Top panel
    TopPanel := TPanel.Create(MainPanel);
    TopPanel.Parent := MainPanel;
    TopPanel.Align := alTop;
    TopPanel.BevelOuter := bvNone;
    TopPanel.Color := bgcol;
    TopPanel.AutoSize := True;

    // Icon
    IconBox := TImage.Create(TopPanel);
    IconBox.Parent := TopPanel;
    IconBox.Align := alLeft;
    IconBox.Width := IfThen(big, 100, 50);
    IconBox.Height := IconBox.Width;
    {$ifdef Windows}IconBox.Font.Name := 'Segoe UI Emoji';{$endif}
    Dialog.HandleNeeded;
    AssignEmoji(IconBox, icon, bgcol);

    // Text panel
    TextPanel := TPanel.Create(TopPanel);
    TextPanel.Parent := TopPanel;
    TextPanel.Align := alClient;
    TextPanel.BevelOuter := bvNone;
    TextPanel.Color := bgcol;

    // Width calculations
    TitlePixelWidth := 0;
    if Trim(title) <> '' then
      TitlePixelWidth := Dialog.Canvas.TextWidth(title);
    DescPixelWidth := Dialog.Canvas.TextWidth(desc);
    TextPixelWidth := Max(TitlePixelWidth, DescPixelWidth);

    ProposedWidth := IconBox.Width + TextPixelWidth + (padding * 6) + 20;

    if big then
    begin
      if ProposedWidth < 650 then ProposedWidth := 650;
    end
    else
    begin
      if ProposedWidth < 400 then ProposedWidth := 400;
    end;

    if logmsg <> '' then
      if ProposedWidth < 500 then ProposedWidth := 500;

    if ProposedWidth > 900 then ProposedWidth := 900;

    Dialog.ClientWidth := ProposedWidth;
    MsgWidth := Dialog.ClientWidth - (IconBox.Width + (padding * 3));

    // Desc height
    TempFont := TFont.Create;
    try
      if big then TempFont.Size := 24;
      TempFont.Style := [];
      TempFont.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
      NeededHeight := MeasureWrappedHeight(desc, TempFont, MsgWidth);
    finally
      TempFont.Free;
    end;

    if big then
    begin
      // BIG-MODE: send desc first
      if NeededHeight > (MaxDialogHeight div 2) then
      begin
        MsgScroll := TScrollBox.Create(TextPanel);
        MsgScroll.Parent := TextPanel;
        MsgScroll.Align := alTop;
        MsgScroll.BorderSpacing.Left := padding;
        MsgScroll.BorderSpacing.Right := padding;
        MsgScroll.BorderSpacing.Bottom := padding; // Padding toward log
        MsgScroll.Width := MsgWidth;
        MsgScroll.Height := MaxDialogHeight div 2;
        MsgScroll.VertScrollBar.Visible := True;
        MsgScroll.BorderStyle := bsNone;

        MsgLabel := TLabel.Create(MsgScroll);
        MsgLabel.Parent := MsgScroll;
        MsgLabel.WordWrap := True;
        MsgLabel.AutoSize := True;
        MsgLabel.Font.Size := 24;
        MsgLabel.Font.Style := [];
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
        MsgLabel.Align := alTop;
      end
      else
      begin
        MsgLabel := TLabel.Create(TextPanel);
        MsgLabel.Parent := TextPanel;
        MsgLabel.WordWrap := True;
        MsgLabel.AutoSize := True;
        MsgLabel.Font.Size := 24;
        MsgLabel.Font.Style := [];
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
        MsgLabel.Align := alTop;
        MsgLabel.BorderSpacing.Left := padding;
        MsgLabel.BorderSpacing.Right := padding;
        MsgLabel.BorderSpacing.Bottom := padding; // Padding towards log
      end;

      // Title last
      if Trim(title) <> '' then
      begin
        TitleLabel := TLabel.Create(TextPanel);
        TitleLabel.Parent := TextPanel;
        TitleLabel.WordWrap := True;
        TitleLabel.AutoSize := True;
        TitleLabel.Font.Size := 24;
        TitleLabel.Font.Style := [fsBold];
        TitleLabel.Caption := title;
        TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
        TitleLabel.Align := alTop;
        TitleLabel.BorderSpacing.Left := padding;
        TitleLabel.BorderSpacing.Right := padding;
      end;
    end
    else
    begin
      // NON-BIG: title first
      if Trim(title) <> '' then
      begin
        TitleLabel := TLabel.Create(TextPanel);
        TitleLabel.Parent := TextPanel;
        TitleLabel.WordWrap := True;
        TitleLabel.AutoSize := False;
        TitleLabel.Font.Style := [fsBold];
        TitleLabel.Caption := title;
        TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
        TitleLabel.Top := padding;
        TitleLabel.Left := padding;
        TitleLabel.Width := MsgWidth;
        TitleLabel.Height := TitleLabel.Canvas.TextHeight(TitleLabel.Caption);
      end
      else
        TitleLabel := nil;

      // NON-BIG: desc after title
      if NeededHeight > (MaxDialogHeight div 2) then
      begin
        MsgScroll := TScrollBox.Create(TextPanel);
        MsgScroll.Parent := TextPanel;
        if Assigned(TitleLabel) then
          MsgScroll.Top := TitleLabel.Top + TitleLabel.Height + padding
        else
          MsgScroll.Top := padding;
        MsgScroll.Left := padding;
        MsgScroll.Width := MsgWidth;
        MsgScroll.Height := MaxDialogHeight div 2;
        MsgScroll.VertScrollBar.Visible := True;
        MsgScroll.BorderStyle := bsNone;
        MsgScroll.BorderSpacing.Bottom := padding; // Padding to log panel

        MsgLabel := TLabel.Create(MsgScroll);
        MsgLabel.Parent := MsgScroll;
        MsgLabel.WordWrap := True;
        MsgLabel.AutoSize := False;
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
        MsgLabel.Width := MsgWidth;
        MsgLabel.Height := NeededHeight;
      end
      else
      begin
        MsgLabel := TLabel.Create(TextPanel);
        MsgLabel.Parent := TextPanel;
        MsgLabel.WordWrap := True;
        MsgLabel.AutoSize := False;
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
        if Assigned(TitleLabel) then
          MsgLabel.Top := TitleLabel.Top + TitleLabel.Height + padding
        else
          MsgLabel.Top := padding;
        MsgLabel.Left := padding;
        MsgLabel.Width := MsgWidth;
        MsgLabel.Height := NeededHeight;
        MsgLabel.BorderSpacing.Bottom := padding; // Padding towards log panel
      end;
    end;

    // LOG PANEL
    LogPanel := TPanel.Create(Dialog);
    LogPanel.Parent := Dialog;
    LogPanel.Align := alBottom;
    LogPanel.Height := IfThen(big, 100, 50)*scale;
    LogPanel.Color := dumpbg;
    LogPanel.BevelOuter := bvNone;
    LogPanel.Visible := logmsg <> '';

    LogMemo := TMemo.Create(LogPanel);
    LogMemo.Parent := LogPanel;
    LogMemo.Align := alClient;
    LogMemo.ReadOnly := True;
    LogMemo.Color := dumpbg;
    LogMemo.Font.Color := dumptext;
    LogMemo.ScrollBars := ssAutoVertical;
    LogMemo.BorderStyle := bsNone;
    LogMemo.Text := TrimSet(logmsg, [#10, #13]);

    // BUTTON PANEL
    ButtonPanel := TPanel.Create(Dialog);
    ButtonPanel.Parent := Dialog;
    ButtonPanel.Align := alBottom;
    ButtonPanel.BevelOuter := bvNone;
    ButtonPanel.Color := bgcol;

    ButtonActualWidth := IfThen(big, btnWidth * 2, btnWidth);
    btnCount := 0;
    for mr in buttons do Inc(btnCount);
    if btnCount = 0 then btnCount := 1;

    totalBtnWidth := (ButtonActualWidth * btnCount) + (padding * (btnCount - 1));
    posX := (Dialog.ClientWidth - totalBtnWidth) div 2;
    if posX < padding then posX := padding;

    for mr in buttons do
    begin
      {$ifdef Windows}OkButton := TBitBtn.Create(ButtonPanel);{$else}OkButton := TButton.Create(ButtonPanel);{$endif}
      OkButton.Parent := ButtonPanel;
      OkButton.Caption := langs[mr];
      OkButton.ModalResult := UXButtonToModalResult(mr);
      OkButton.Width := ButtonActualWidth;
      if big then OkButton.Height := OkButton.Height * 2;
      OkButton.Top := padding;
      OkButton.Left := posX;
      posX := posX + OkButton.Width + padding;
    end;

    ButtonPanel.Height := OkButton.Top + OkButton.Height + (padding * 2);

    if Dialog.Height > MaxDialogHeight then
      Dialog.Height := MaxDialogHeight;

    Dialog.ShowModal;
    Result := Dialog.ModalResult;
  finally
    Dialog.Free;
  end;
end;

end.
