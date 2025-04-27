
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
DX12.D2D1, DX12.DXGI, DX12.DWrite, DX12.DCommon, DX12.WinCodec, Windows,
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
  TUXMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose, mbUXOpenFile);

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
  langs : ButtonLangs = (smbYes, smbUXNo, smbUXOK, smbUXCancel, smbUXAbort, smbUXRetry, smbUXIgnore, smbUXAll, smbUXNoToAll, smbUXYesToAll, smbUXHelp, smbUXClose, smbUXOpenFile);


//function UXShowMessage(const message: string; buttons: TMsgDlgButtons; const icon: Widechar): TModalResult;

procedure UXMessage(const title, message: string; const icon: widechar = widechar($2705));
function UXDialog(const title, message: string; buttons: TUXMsgDlgBtns; const icon: widechar = widechar($2705)): TModalResult;

//function UXShowMessage(const caption, title, desc, message: string; buttons: TMsgDlgButtons; const icon: WideChar): TModalResult;
function ExtMsg(const  caption, title, desc, logmsg: string; dumpbg: TColor = $00F5F2FD; dumptext:
TColor = $003411A9; buttons: TUXMsgDlgBtns = [mbAbort]; const icon: widechar =
widechar($2699)): TModalResult;
function ExtLog(const caption, msg, log: string; const icon: widechar = widechar($2699)):
TModalResult
;
function ExtError(const msg, error: string; const icon: widechar = widechar($2699)): TModalResult;
function ExtError(const error: string; const icon: widechar = widechar($2699)): TModalResult;
function ExtSucc(const msg, desc, output: string; dumpbg: TColor = $0095EEC4; dumptext: TColor = $00147C4A; const icon: widechar = widechar($2705)): TModalResult;
{$ifdef Windows}
function CoCreateInstance(const clsid: TGUID; unkOuter: IUnknown; dwClsContext: longint;
const iid: TGUID; out pv): HResult;
stdcall;
external 'ole32.dll';
{$endif}

implementation

function UXButtonToModalResult(Btn: TUXMsgDlgBtn): TModalResult;
begin
  case Btn of
    mbYes:       Result := mrYes;
    mbNo:        Result := mrNo;
    mbOK:        Result := mrOK;
    mbCancel:    Result := mrCancel;
    mbAbort:     Result := mrAbort;
    mbRetry:     Result := mrRetry;
    mbIgnore:    Result := mrIgnore;
    mbAll:       Result := mrAll;
    mbNoToAll:   Result := mrNoToAll;
    mbYesToAll:  Result := mrYesToAll;
//    mbUXHelp:      Result := mrHelp;
//    mbUXClose:     Result := mrClose;
  else
    // t.ex. ditt specialfall för OpenFile
    Result := TModalResult(110);
  end;
end;

{$ifdef Windows}
procedure AssignEmoji(Image: TImage; const Emoji: widestring);

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
    BackgroundColor := ColorF(1.0, 1.0, 1.0, 1.0);
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
procedure AssignEmoji(Image: TImage; const Emoji: widestring);
begin
  // Ställ in storleken på Canvas och bakgrundsfärgen
  Image.Picture.Bitmap.SetSize(Image.Width, Image.Height);
  Image.Picture.Bitmap.Canvas.Brush.Color := IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);
  Image.Picture.Bitmap.Canvas.FillRect(0, 0, Image.Width, Image.Height);

  // Ställ in teckensnittet (här använder vi ett standardteckensnitt)
  Image.Picture.Bitmap.Canvas.Font.Name := 'Noto Color Emoji';
  // Ett populärt emoji-teckensnitt för Linux
  Image.Picture.Bitmap.Canvas.Font.Size := 48;
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

procedure btnmodal(sender: tbutton);
begin

end;

function ExtMsg(const caption, title, desc, logmsg: string; dumpbg: TColor = $00F5F2FD; dumptext:
TColor = $003411A9; buttons: TUXMsgDlgBtns = [mbAbort]; const icon: widechar =
widechar($2699)): TModalResult;

const
  btnWidth = 75;
  padding = 10;
var 
  Dialog: TForm;
  IconBox: TImage;
  TitleLabel: TLabel;
  MessageLabel: TLabel;
  log: TMemo;
  logPanel: TPanel;
  OkButton: TButton;
  ContentWidth: integer;
  p2, p3: TPanel;
  mr: TUXMsgDlgBtn;
  last: integer;
begin
  Dialog := TForm.CreateNew(nil);
  try
    Dialog.Caption := caption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poWorkAreaCenter;

    // Huvudpanelen
    p2 := TPanel.Create(Dialog);
    p2.Parent := Dialog;
    p2.Align := alClient;
    p2.Color :=  IfThen(TrndiNative.isDarkMode, $00322B27, clWhite);;
    p2.BevelInner := bvNone;
    p2.BevelOuter := bvNone;

    // Emoji-ikon som visas i en TImage
    IconBox := TImage.Create(p2);
    IconBox.Parent := p2;
    IconBox.Width := 50;
    IconBox.Height := 50;
    IconBox.Left := Padding;
    IconBox.Top := Padding;
    {$ifdef Windows}
    IconBox.Font.Name := 'Segoe UI Emoji';
    {$endif}
    //    IconBox.Align := alleft;

    Dialog.HandleNeeded;
    // Rita emoji på IconBox
    AssignEmoji(IconBox, icon);

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
         if logmsg = '' then begin
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

    last := p3.ClientWidth - Padding;
    // OK-knapp
    for mr in buttons do begin
      OkButton := TButton.Create(p3);
      OkButton.Parent := p3;
      OkButton.Caption := langs[mr];

      OkButton.ModalResult := UXButtonToModalResult(mr);
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
    for mr in buttons do begin
       OkButton := p3.FindChildControl('btn'+IntToStr(ord(mr))) as TButton;

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
