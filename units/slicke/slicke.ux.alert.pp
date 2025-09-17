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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

{**
  @unit slicke.ux.alert
  @brief Adaptive Lazarus/FPC dialogs and input helpers with emoji icons, dark mode and touch-aware layout.

  @details
  This unit provides a small UX toolkit for Lazarus/FPC that renders message dialogs,
  extended messages with logs, and input selectors (text, numeric, list, table).
  It adapts the layout (big/small/auto) for touch screens, supports dark mode, and
  draws emoji icons using Direct2D/DirectWrite on Windows or text rendering elsewhere.

  The public API centers around:
  - @link(UXMessage) for simple, one-button informational messages.
  - @link(UXDialog) overloads for message dialogs with button sets or Lazarus TMsgDlgType mapping.
  - @link(ExtMsg), @link(ExtLog), @link(ExtError), @link(ExtSucc), @link(ExtSuccEx) for rich dialogs with dumps/logs.
  - @link(ExtInput), @link(ExtNumericInput), @link(ExtIntInput), @link(ExtList), @link(ExtTable) for data entry.
  - Font helpers @link(FontGUIInList) and @link(FontTXTInList) to locate suitable UI/text fonts.

  Platform support:
  - Windows: emoji rendering via Direct2D/DirectWrite; custom dark-titlebar opt-in where possible.
  - Linux/BSD: emoji/text via canvas using Noto fonts when available.

  @author
  Björn Lindh, with PasDoc annotations added.
}
unit slicke.ux.alert;

{$I inc/native.inc}
{$modeswitch advancedrecords}
interface

uses
  Classes, SysUtils, Dialogs, Forms, ExtCtrls, StdCtrls, Controls, Graphics, Math,
  IntfGraphics, FPImage, graphtype, lcltype, Trndi.Native, Grids, Spin,
  {$ifdef Windows}
  DX12.D2D1, DX12.DXGI, DX12.DWrite, DX12.DCommon, DX12.WinCodec, Windows, Buttons, ActiveX, ComObj,
  {$endif}
  StrUtils;

{**
  @name Localizable strings
  @desc
  Localized captions used across dialogs. Override via Lazarus resource machinery if needed.
}
resourcestring
  dlgErr      = 'An error occurred while creating a message dialog';
  sMsgTitle   = 'Message';
  sSuccTitle  = 'Information';
  sExtTitle   = 'Extension error';
  sExtErr     = 'Error occurred in extension';
  sErr        = 'Script execution failed';

  smbYes          = 'Yes';
  smbUXNo         = 'No';
  smbUXOK         = 'OK';
  smbUXCancel     = 'Cancel';
  smbUXAbort      = 'Abort';
  smbUXRetry      = 'Retry';
  smbUXIgnore     = 'Ignore';
  smbUXAll        = 'All';
  smbUXNoToAll    = 'No To All';
  smbUXYesToAll   = 'Yes To All';
  smbUXHelp       = 'Help';
  smbUXClose      = 'Close';
  smbUXOpenFile   = 'Open File';
  smbUxMinimize   = 'Minimize';
  smbSelect       = 'Select';
  smbUxAgree      = 'Agree';
  smbUxRead       = 'Read...';
  smbUXDefault    = 'Default';

  sKey   = 'Key';
  sValue = 'Value';

const
  {**
    @name Emoji-based dialog icons
    @desc
    Unicode codepoints rendered as emoji or symbols on supported platforms.
    Fallbacks depend on available system fonts.
  }
  uxmtOK            = widechar($2705); // ✅ Ticked box
  uxmtWarning       = widechar($26A0); // ⚠️ Warning sign
  uxmtError         = widechar($274C); // ❌ Cross mark
  uxmtInformation   = widechar($2139); // ℹ️ Info symbol
  uxmtConfirmatio   = widechar($2753); // ❓ Question mark
  uxmtCog           = widechar($2699); // ⚙️ Gear
  uxmtSquare        = widechar($274F); // ❏ Square
  uxmtCustom        = uxmtCog;


  uxclBlue = $00AA6004;
  uxclLightBlue = $00FDD8AA;
  uxclWhite = $00F5F2FD;
  uxclRed = $003411A9;
  uxclLightGreen = $0095EEC4;
  uxclDarkGreen = $00147C4A;
  uxclGray = $00322B27;

  {**
    @name Dialog button aliases
    @desc
    Aliases mapping to Lazarus modal buttons for clarity and consistency with UX naming.
  }
  mbUXYes       = mbYes;
  mbUXNo        = mbNo;
  mbUXOK        = mbOK;
  mbUXCancel    = mbCancel;
  mbUXAbort     = mbAbort;
  mbUXRetry     = mbRetry;
  mbUXIgnore    = mbIgnore;
  mbUXAll       = mbAll;
  mbUXNoToAll   = mbNoToAll;
  mbUXYesToAll  = mbYesToAll;
  mbUXHelp      = mbHelp;
  mbUXClose     = mbClose;

type
  {** Emoji glyph used for icons. Typically a single widechar codepoint. }
  UXImage = widechar;

  {**
    Modal dialog form used internally by UX helpers.
    @remarks
      - Overrides @code(CreateWnd) for platform tweaks (e.g., dark title bar on Windows).
      - Handles keyboard navigation (Enter/Escape) in @link(FormKeyDown).
      - On Windows can owner-draw buttons via @link(ButtonDrawItem).
  }
  TDialogForm = class(TForm)
    {** Keyboard shortcuts: Enter to confirm (when appropriate), Esc to cancel/No. }
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    {** Finalizes platform window style, sets KeyPreview, and enables dark mode title bar where supported. }
    procedure CreateWnd; override;
    {$ifdef windows}
  public
    {** Owner-draw routine for bit buttons on Windows to match dark mode styling. }
    procedure ButtonDrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; State: TButtonState);
    {$endif}
    {** OnClick handler used by inline full-screen message overlays created via @link(UXMessage). }
    procedure UXMessageOnClick(sender: TObject);
  end;

  {**
    Size preset for dialog layout.
    @value uxdNormal Standard dialog layout.
    @value uxdBig Larger layout suitable for touch/TV screens.
    @value uxdAuto Auto-detect (big if touch screen available).
    @value uxdOnForm Render message inline on an existing form (used by @link(UXMessage)).
  }
  TUXDialogSize = (uxdNormal = 0, uxdBig = 1, uxdAuto = 3, uxdOnForm = 4);

  {**
    Available dialog buttons for UX helpers.
    @remarks Includes standard Lazarus modal buttons and a few custom labels (e.g. OpenFile, Minimize, Agree, Read, Default).
  }
  TUXMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose, mbUXOpenFile, mbUXMinimize, mbUXAgree, mbUXRead, mbUXDefault);

  {** A set of @link(TUXMsgDlgBtn) to specify multiple buttons. }
  TUXMsgDlgBtns = set of TUXMsgDlgBtn;

  {** Mapping of @link(TUXMsgDlgBtn) to localized captions. }
  ButtonLangs = array[TUXMsgDlgBtn] of string;

  {**
    Show a simple message dialog, optionally inline on a form in @code(uxdOnForm) mode.
    @param dialogsize Layout preset; @seealso(TUXDialogSize)
    @param title Dialog title text (top label).
    @param message Main message body.
    @param icon Emoji icon; defaults to @code(uxmtOK).
    @param sender Optional form used when @code(dialogsize = uxdOnForm) to render a full-screen overlay.
  }
  procedure UXMessage(const dialogsize: TUXDialogSize; const title, message: string; const icon: uximage = uxmtOK; sender: TForm = nil);

  {**
    Generic dialog with custom button set and emoji icon.
    @param dialogsize Layout preset; @seealso(TUXDialogSize)
    @param title Title text displayed above @code(message).
    @param message Description/body text.
    @param buttons Button set to display.
    @param icon Emoji icon; defaults to @code(uxmtOK).
    @returns Lazarus modal result corresponding to the button clicked.
  }
  function UXDialog(const dialogsize: TUXDialogSize;
                    const title, message: string;
                    buttons: TUXMsgDlgBtns;
                    const icon: UXImage = uxmtOK): TModalResult; overload;

  {**
    Generic dialog with custom button set and Lazarus message type mapped to a default icon.
    @param dialogsize Layout preset; @seealso(TUXDialogSize)
    @param title Title text displayed above @code(message).
    @param message Description/body text.
    @param buttons Button set to display.
    @param mtype Lazarus message dialog type; maps to a reasonable emoji icon.
    @returns Lazarus modal result corresponding to the button clicked.
  }
  function UXDialog(const dialogsize: TUXDialogSize;
                    const title, message: string;
                    buttons: TUXMsgDlgBtns;
                    const mtype: TMsgDlgType): TModalResult; overload;

  {**
    Generic dialog with custom header line (caption), title and message and TMsgDlgType mapping.
    @param dialogsize Layout preset; @seealso(TUXDialogSize)
    @param header Window caption (top title bar).
    @param title Title text (bold, in content).
    @param message Description/body text.
    @param buttons Button set to display.
    @param mtype Lazarus message dialog type; maps to a reasonable emoji icon.
    @returns Lazarus modal result corresponding to the button clicked.
  }
  function UXDialog(const dialogsize: TUXDialogSize;
                    const header, title, message: string;
                    buttons: TUXMsgDlgBtns;
                    const mtype: TMsgDlgType): TModalResult; overload;

  {**
    Extended message dialog supporting an optional log/dump panel with custom colors.
    @param dialogsize Layout preset; @seealso(TUXDialogSize)
    @param caption Window caption.
    @param title Title text.
    @param desc Description/body text (supports wrapping and scrolling in big mode).
    @param logmsg Optional log/dump text displayed in a fixed panel at the bottom; pass empty to hide.
    @param dumpbg Background color for log/dump panel (ARGB).
    @param dumptext Text color for log/dump panel (ARGB).
    @param buttons Button set to display (default [mbAbort]).
    @param icon Emoji icon to render.
    @param scale Optional log panel vertical scale multiplier (for big outputs).
    @returns Lazarus modal result corresponding to the button clicked.
  }
  function ExtMsg(const dialogsize: TUXDialogSize;
                  const caption, title, desc, logmsg: string;
                  dumpbg: TColor = uxclWhite;
                  dumptext: TColor = uxclRed;
                  buttons: TUXMsgDlgBtns = [mbAbort];
                  const icon: UXImage = uxmtCog;
                  scale: integer = 1): TModalResult;

  {**
    Convenience wrapper for @link(ExtMsg) that shows a message and a log/dump with an OK button.
    @param dialogsize Layout preset.
    @param caption Window caption.
    @param msg Title text.
    @param log Log or output text.
    @param icon Emoji icon (default gear).
    @param scale Log panel vertical scale multiplier.
    @returns @code(mrOK) if confirmed, otherwise the modal result selected by the user.
  }
  function ExtLog(const dialogsize: TUXDialogSize;
                  const caption, msg, log: string;
                  const icon: UXImage = uxmtCog;
                  scale: integer = 1): TModalResult;

  {**
    Show an error dialog with a short message and an error dump in the log panel.
    @param dialogsize Layout preset.
    @param msg Short explanation shown as description.
    @param error Detailed error text shown in the log panel.
    @param icon Emoji icon (default gear).
    @returns Modal result (default is [mbAbort]).
  }
  function ExtError(const dialogsize: TUXDialogSize;
                    const msg, error: string;
                    const icon: UXImage = uxmtCog): TModalResult; overload;

  {**
    Show an error dialog with standard captions and the given error in the log panel.
    @param dialogsize Layout preset.
    @param error Error text to display in log panel.
    @param icon Emoji icon (default gear).
    @returns Modal result (default is [mbAbort]).
  }
  function ExtError(const dialogsize: TUXDialogSize;
                    const error: string;
                    const icon: UXImage = uxmtCog): TModalResult; overload;

  {**
    Show a success/information dialog with a dump panel.
    @param dialogsize Layout preset.
    @param msg Title text.
    @param desc Description/body text.
    @param output Dump/log text.
    @param dumpbg Background color for dump panel.
    @param dumptext Text color for dump panel.
    @param icon Emoji icon (default @code(uxmtOK)).
    @returns Modal result (OK by default).
  }
  function ExtSucc(const dialogsize: TUXDialogSize;
                   const msg, desc, output: string;
                   dumpbg: TColor = uxclLightGreen;
                   dumptext: TColor = uxclDarkGreen;
                   const icon: UXImage = uxmtOK): TModalResult;

  {**
    Variant of @link(ExtSucc) that accepts a custom button set.
    @param dialogsize Layout preset.
    @param msg Title text.
    @param desc Description/body text.
    @param output Dump/log text.
    @param btns Custom buttons to display.
    @param dumpbg Background color for dump panel.
    @param dumptext Text color for dump panel.
    @param icon Emoji icon (default @code(uxmtOK)).
    @returns Modal result.
  }
  function ExtSuccEx(const dialogsize: TUXDialogSize;
                     const msg, desc, output: string;
                     btns: TUXMsgDlgBtns;
                     dumpbg: TColor = uxclLightGreen;
                     dumptext: TColor = uxclDarkGreen;
                     const icon: UXImage = uxmtOK): TModalResult;

  {**
    Show a selection dialog using a drop-down list.
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param Choices Array of strings to populate the combo box.
    @param Default If @true, the cancel button is labeled "Default" to indicate defaulting.
    @param icon Emoji icon (default gear).
    @returns Selected index (0-based) on OK, or -1 on cancel.
  }
  function ExtList(const dialogsize: TUXDialogSize;
                   const ACaption, ATitle, ADesc: string;
                   const Choices: array of string;
                   const Default: boolean = false;
                   const icon: UXImage = uxmtCog): Integer;

  {**
    Show a single-line string input dialog.
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial text value.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns The entered string when @code(ModalResult = mrOK); otherwise the previous/default content.
  }
  function ExtInput(const dialogsize: TUXDialogSize;
                    const ACaption, ATitle, ADesc, ADefault: string;
                    var ModalResult: TModalResult;
                    const icon: UXImage = uxmtCog): string;

  {**
    Show a numeric input dialog using @code(TFloatSpinEdit).
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial numeric value.
    @param float If @true, allow fractional values (2 decimal places); if @false, integer-only.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns Entered numeric value if OK; otherwise returns @code(ADefault).
  }
  function ExtNumericInput(const dialogsize: TUXDialogSize;
                   const ACaption, ATitle, ADesc: string;
                   ADefault: double;
                   float: boolean;
                   var ModalResult: TModalResult;
                   const icon: UXImage = uxmtCog): double;

  {**
    Convenience wrapper over @link(ExtNumericInput) for integer-only input.
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial integer value.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns Entered integer value if OK; otherwise returns @code(ADefault).
  }
  function ExtIntInput(
                    const dialogsize: TUXDialogSize;
                    const ACaption, ATitle, ADesc: string;
                    ADefault: integer;
                    var ModalResult: TModalResult;
                    const icon: UXImage = uxmtCog
                  ): integer;

  {**
    Show a two-column table (key/value) dialog using @code(TStringGrid).
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param Keys Row header values for the first column (without header row).
    @param Values Row values for the second column (must match @code(Keys) length).
    @param icon Emoji icon (default gear).
    @param key Column 0 header (defaults to localized @code(sKey)).
    @param value Column 1 header (defaults to localized @code(sValue)).
    @returns Selected row index on OK; -1 if canceled.
  }
  function ExtTable(const dialogsize: TUXDialogSize;
                    const ACaption, ATitle, ADesc: string;
                    const Keys, Values: array of string;
                    const icon: UXImage = uxmtCog;
                    const key: string = '';
                    const value: string = ''): Integer;

  {**
    Check if a suitable UI font exists on this system and return its name.
    @param fname Out parameter that receives a preferred UI font name for emoji/mono display.
    @returns @true if the font (or a fallback) is available; otherwise @false.
  }
  function FontGUIInList(out fname: string): Boolean;

  {**
    Check if a suitable text font exists on this system and return its name.
    @param fname Out parameter that receives a preferred UI text font.
    @returns @true if the font is present; otherwise @false. On unknown platforms always @true with a generic name.
  }
  function FontTXTInList(out fname: string): Boolean;

var
  {** Localized captions for each @link(TUXMsgDlgBtn). Initialized from resource strings. }
  langs : ButtonLangs = (smbYes, smbUXNo, smbUXOK, smbUXCancel, smbUXAbort, smbUXRetry, smbUXIgnore,
                         smbUXAll, smbUXNoToAll, smbUXYesToAll, smbUXHelp, smbUXClose,
                         smbUXOpenFile, smbUxMinimize, smbUxAgree, smbUxRead, smbUxDefault);

implementation

{**
  Determine whether dialogs should use the large layout.
  @param dialogsize Requested size mode.
  @returns @true if big layout should be used; @false otherwise.
  @remarks When @code(dialogsize = uxdAuto), it checks @code(TrndiNative.HasTouchScreen).
}
function UXDialogIsBig(dialogsize: TUXDialogSize): boolean;
begin
  case dialogsize of
    uxdNormal: result := false;
    uxdBig:    result := true;
    uxdAuto:   result := TrndiNative.HasTouchScreen;
    else
      result := TrndiNative.HasTouchScreen;
  end;
end;

{**
  Compute the wrapped text height for a label given its fixed width.
  @param ALabel Label with font and width already assigned.
  @returns Pixel height needed to display the caption.
}
function CalcWrappedHeight(ALabel: TLabel): Integer;
var
  bmp: Graphics.TBitmap;
  textParts: TStringList;
  line: string;
  i, lineCount: Integer;
begin
  bmp := Graphics.TBitmap.Create;
  textParts := TStringList.Create;
  try
    bmp.Canvas.Font.Assign(ALabel.Font);
    textParts.Delimiter := ' ';
    textParts.StrictDelimiter := True;
    textParts.DelimitedText := ALabel.Caption;

    line := '';
    lineCount := 1;

    for i := 0 to textParts.Count - 1 do
    begin
      if bmp.Canvas.TextWidth(Trim(line + ' ' + textParts[i])) > ALabel.Width then
      begin
        Inc(lineCount);
        line := textParts[i];
      end
      else
        line := Trim(line + ' ' + textParts[i]);
    end;

    Result := lineCount * bmp.Canvas.TextHeight('Hg');
  finally
    bmp.Free;
    textParts.Free;
  end;
end;

{**
  Create and place a bold title label and a normal description label to the right of the icon.
  Performs word-wrapping, spacing and big-mode scaling.
  @param AOwner Parent form/dialog.
  @param big Use big layout when @true.
  @param ATitle Title text (bold).
  @param ADesc Description text.
  @param IconBox Icon control already created and positioned.
  @param TitleLabel Out: created title label.
  @param DescLabel Out: created description label.
}
procedure CreateTitleAndDescription(
  AOwner: TForm;
  const big: Boolean;
  const ATitle, ADesc: string;
  IconBox: TImage;
  out TitleLabel, DescLabel: TLabel
);
const
  Padding = 16;
  MinDialogWidth = 650;
var
  RightContentLeft, AvailableWidth: Integer;
begin
  // Minimum width for big mode
  if big and (AOwner.ClientWidth < MinDialogWidth) then
    AOwner.ClientWidth := MinDialogWidth;

  RightContentLeft := IconBox.Left + IconBox.Width + Padding;
  AvailableWidth := AOwner.ClientWidth - RightContentLeft - Padding;

  // --- Title ---
  TitleLabel := TLabel.Create(AOwner);
  TitleLabel.Parent := AOwner;
  TitleLabel.WordWrap := True;
  TitleLabel.AutoSize := False;   // lock width for wrapping
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := RightContentLeft;
  TitleLabel.Width := AvailableWidth;
  if big then TitleLabel.Font.Size := 26;
  TitleLabel.Top := Padding;
  TitleLabel.Caption := ATitle;
  TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
  TitleLabel.AdjustSize;  // now calculates proper height for given width

 // Description
 DescLabel := TLabel.Create(AOwner);
 DescLabel.Parent := AOwner;
 DescLabel.WordWrap := True;
 DescLabel.AutoSize := False;
 DescLabel.Font.Style := [];
 DescLabel.Left := TitleLabel.Left;
 DescLabel.Width := AvailableWidth;
 if big then
   DescLabel.Font.Size := 24;
 DescLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding;
 DescLabel.Caption := ADesc;
 DescLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);

 // ✅ Force correct height for wrapped text
 DescLabel.Height := CalcWrappedHeight(DescLabel);
end;

{**
  Measure wrapped text height for a given font and maximum width (label-independent).
  @param AText The text to measure.
  @param AFont Font to use for measurement (copied onto a temp canvas).
  @param MaxWidth Maximum width in pixels before wrapping.
  @returns Required height in pixels.
}
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

{**
  Map UX button enum to Lazarus modal results.
  @param Btn UX button.
  @returns Corresponding @code(TModalResult).
}
function UXButtonToModalResult(Btn: TUXMsgDlgBtn): TModalResult;
begin
  case Btn of
    mbYes:       Result := mrYes;
    mbNo:        Result := mrNo;
    mbOK:        Result := mrOk;
    mbCancel:    Result := mrCancel;
    mbAbort:     Result := mrAbort;
    mbRetry:     Result := mrRetry;
    mbIgnore:    Result := mrIgnore;
    mbAll:       Result := mrAll;
    mbNoToAll:   Result := mrNoToAll;
    mbYesToAll:  Result := mrYesToAll;
    mbClose:     Result := mrClose;
  else
    // fallback / custom button
    Result := TModalResult(110);
  end;
end;

{$ifdef Windows}
{**
  Convert a Lazarus @code(TColor) to a Direct2D color with alpha.
  @param Col Lazarus color.
  @param Alpha Alpha in [0..1].
  @returns Direct2D color struct.
}
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

{**
  Render an emoji into a @code(TImage) using Direct2D/DirectWrite on Windows.
  @param Image Target image control.
  @param Emoji Emoji text (usually a single codepoint).
  @param bgcol Background color (fills the bitmap).
}
procedure AssignEmoji(Image: TImage; const Emoji: widestring; bgcol: TColor = clWhite);
var
  D2DFactory: ID2D1Factory;
  DWFactory: IDWriteFactory;
  RT: ID2D1DCRenderTarget;
  TextFormat: IDWriteTextFormat;
  Brush: ID2D1SolidColorBrush;
  TargetProps: TD2D1_RENDER_TARGET_PROPERTIES;
  Bitmap: Graphics.TBitmap;
  TextRect: TD2D1_RECT_F;
  BG: TD2D1_COLOR_F;
  R: TRect;
  Inset: Single;
begin
  CoInitialize(nil);
  try
    Bitmap := Graphics.TBitmap.Create;
    try
      Bitmap.SetSize(Image.Width, Image.Height);
      Bitmap.PixelFormat := pf32bit;
      Bitmap.Canvas.Brush.Color := bgcol;
      Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);

      // Create Direct2D factory
      D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, ID2D1Factory, nil, D2DFactory);

      // Render target properties
      FillChar(TargetProps, SizeOf(TargetProps), 0);
      TargetProps._type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
      TargetProps.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
      TargetProps.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
      TargetProps.dpiX := 96;
      TargetProps.dpiY := 96;

      // Create DC render target
      D2DFactory.CreateDCRenderTarget(@TargetProps, RT);

      // Bind Direct2D target to Lazarus DC
      R := Classes.Rect(0, 0, Bitmap.Width, Bitmap.Height);
      RT.BindDC(Bitmap.Canvas.Handle, @R);

      // Create DirectWrite factory & text format
      DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, IUnknown(DWFactory));

      // Add 20% inset for padding
      Inset := Image.Width * 0.20;

      DWFactory.CreateTextFormat(
        PWideChar('Segoe UI Emoji'), nil,
        DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL,
        DWRITE_FONT_STRETCH_NORMAL,
        Image.Height - Trunc(Inset * 2),
        'en-us', TextFormat
      );

      // Brush for text rendering
      BG := TColorToColorF(bgcol, 1.0);
      RT.CreateSolidColorBrush(@BG, nil, Brush);

      // Drawing area with inset
      TextRect := RectF(Inset, Inset, Image.Width - Inset, Image.Height - Inset);

      // Draw
      RT.BeginDraw;
      RT.Clear(BG);
      RT.DrawText(PWideChar(Emoji), Length(Emoji), TextFormat,
                  @TextRect, Brush,
                  D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT,
                  DWRITE_MEASURING_MODE_NATURAL);
      RT.EndDraw;

      // Assign to TImage
      Image.Picture.Assign(Bitmap);
      Image.Transparent := True;
    finally
      Bitmap.Free;
    end;
  finally
    CoUninitialize;
  end;
end;

{$else}
{**
  Render an emoji into a @code(TImage) using the standard canvas (non-Windows).
  Uses "Noto Color Emoji" when available.
  @param Image Target image control.
  @param Emoji Emoji text (usually a single codepoint).
  @param bgcol Background color.
}
procedure AssignEmoji(Image: TImage; const Emoji: widestring; bgcol: TColor = clWhite);
var
  Inset: Integer;
begin
  Image.Picture.Bitmap.SetSize(Image.Width, Image.Height);
  Image.Transparent := True;

  Image.Picture.Bitmap.Canvas.Brush.Color := bgcol;
  Image.Picture.Bitmap.Canvas.FillRect(0, 0, Image.Width, Image.Height);

  Inset := Round(Image.Width * 0.15); // 15% padding around the emoji

  Image.Picture.Bitmap.Canvas.Font.Name := 'Noto Color Emoji';
  Image.Picture.Bitmap.Canvas.Font.Size := Image.Height - (Inset * 2);
  Image.Picture.Bitmap.Canvas.Font.Color := clBlack;

  Image.Picture.Bitmap.Canvas.TextOut(
    (Image.Width - Image.Picture.Bitmap.Canvas.TextWidth(Emoji)) div 2,
    (Image.Height - Image.Picture.Bitmap.Canvas.TextHeight(Emoji)) div 2,
    Emoji
  );
end;
{$endif}

{**
  Shared helper to lay out icon, title and description on a dialog.
  @param Dialog Target dialog.
  @param big Use big layout when @true.
  @param icon Emoji icon to render into @code(IconBox).
  @param bgcol Dialog background color.
  @param ATitle Title text.
  @param ADesc Description text.
  @param IconBox Pre-created TImage for the icon (parent/size set here).
  @param TitleLabel Out title label instance.
  @param DescLabel Out description label instance.
  @param MinWidthNormal Minimum width in normal mode.
  @param MinWidthBig Minimum width in big mode.
  @param IconSize Base icon size (doubled in big mode).
  @param Padding Inner spacing between controls.
}
procedure SetupDialogTitleDesc(
  Dialog: TForm;
  const big: Boolean;
  const icon: UXImage;
  const bgcol: TColor;
  const ATitle, ADesc: string;
  IconBox: TImage;
  out TitleLabel, DescLabel: TLabel;
  MinWidthNormal: Integer = 650;
  MinWidthBig: Integer = 800;
  IconSize: Integer = 48;
  Padding: Integer = 16
);
var
  availableWidth: Integer;
  currentIconSize: Integer;
begin
  // --- Ensure minimum dialog width ---
  Dialog.ClientWidth := MinWidthNormal;
  if big and (Dialog.ClientWidth < MinWidthBig) then
    Dialog.ClientWidth := MinWidthBig;
  Dialog.Color := bgcol;

  // --- Icon size scaling ---
  currentIconSize := IconSize;
  if big then
    currentIconSize := IconSize * 2;

  // --- Create & position the icon ---
  IconBox.Parent := Dialog;
  IconBox.Width := currentIconSize;
  IconBox.Height := currentIconSize;
  {$IFDEF LINUX}
  IconBox.Left := Padding * 2; // extra gap on Linux
  IconBox.Top  := Padding * 2;
  {$ELSE}
  IconBox.Left := Padding;     // default gap on other platforms
  IconBox.Top  := Padding;
  {$ENDIF}
  AssignEmoji(IconBox, Icon, bgcol);

  // --- Title label ---
  TitleLabel.Parent := Dialog;
  TitleLabel.WordWrap := True;
  TitleLabel.AutoSize := False;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := IconBox.Left + IconBox.Width + Padding;
  availableWidth := Dialog.ClientWidth - TitleLabel.Left - Padding;
  TitleLabel.Width := availableWidth;
  if big then TitleLabel.Font.Size := 26;
  TitleLabel.Top := IconBox.Top; // aligns with icon top
  TitleLabel.Caption := ATitle;
  TitleLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
  TitleLabel.Height := CalcWrappedHeight(TitleLabel);

  // --- Description label ---
  DescLabel.Parent := Dialog;
  DescLabel.WordWrap := True;
  DescLabel.AutoSize := False;
  DescLabel.Font.Style := [];
  DescLabel.Left := TitleLabel.Left;
  DescLabel.Width := availableWidth;
  if big then DescLabel.Font.Size := 24;
  DescLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding;
  DescLabel.Caption := ADesc;
  DescLabel.Font.Color := IfThen(TrndiNative.isDarkMode, clWhite, clBlack);
  DescLabel.Height := CalcWrappedHeight(DescLabel);
end;

{** See interface docs for behavior and parameters. }
function ExtIntInput(
  const dialogsize: TUXDialogSize;
  const ACaption, ATitle, ADesc: string;
  ADefault: integer;
  var ModalResult: TModalResult;
  const icon: UXImage = uxmtCog
): integer;
begin
  result := round(ExtNumericInput(dialogsize,ACaption,ATitle,ADesc,ADefault, false, ModalResult, icon));
end;

{** See interface docs for behavior and parameters. }
function ExtNumericInput(
  const dialogsize: TUXDialogSize;
  const ACaption, ATitle, ADesc: string;
  ADefault: double;
  float: boolean;
  var ModalResult: TModalResult;
  const icon: UXImage = uxmtCog
): double;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Edit: TFloatSpinEdit;
  OkButton, CancelButton: TButton;
  bgcol: TColor;
  big: Boolean;
  totalButtonsWidth: Integer;
begin
  big := UXDialogIsBig(dialogsize);
  Result := 0;
  ModalResult := mrCancel;
  bgcol := IfThen(TrndiNative.isDarkMode, uxclGray, clWhite);

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    // Use shared helper for title + description
    SetupDialogTitleDesc(Dialog, big, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Numeric input ---
    Edit := TFloatSpinEdit.Create(Dialog);
    Edit.Parent := Dialog;
    Edit.Left := DescLabel.Left;
    Edit.Width := DescLabel.Width;
    Edit.Top := DescLabel.Top + DescLabel.Height + ifthen(big, Padding * 2, Padding);
    Edit.Value := ADefault;
    if float then
      Edit.DecimalPlaces := 2
    else
      Edit.DecimalPlaces := 0;
    if big then
    begin
      Edit.Font.Size := 20;
      {$IFDEF Windows} Edit.Height := 42; {$ENDIF}
    end;

    // --- OK Button ---
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbSelect;
    OkButton.ModalResult := mrOk;
    OkButton.Width := 80;
    if big then
    begin
      OkButton.Width := OkButton.Width * 2;
      OkButton.Height := OkButton.Height * 2;
      OkButton.Font.Size := 12;
    end;
    Dialog.ActiveControl := OkButton;

    // --- Cancel Button ---
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.Caption := smbUXCancel;
    CancelButton.ModalResult := mrCancel;
    CancelButton.Width := 80;
    if big then
    begin
      CancelButton.Width := CancelButton.Width * 2;
      CancelButton.Height := CancelButton.Height * 2;
      CancelButton.Font.Size := 12;
    end;

    // --- Center buttons ---
    OkButton.Top := Edit.Top + Edit.Height + ifthen(big, Padding * 3, Padding * 2);
    CancelButton.Top := OkButton.Top;
    totalButtonsWidth := OkButton.Width + Padding + CancelButton.Width;
    OkButton.Left := (Dialog.ClientWidth - totalButtonsWidth) div 2;
    CancelButton.Left := OkButton.Left + OkButton.Width + Padding;

    Dialog.ClientHeight := OkButton.Top + OkButton.Height + Padding;
    Dialog.ActiveControl := Edit;

    ModalResult := Dialog.ShowModal;
    if ModalResult = mrOk then
      Result := Edit.Value;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function UXDialog(const dialogsize: TUXDialogSize;
                  const title, message: string;
                  buttons: TUXMsgDlgBtns;
                  const icon: UXImage = uxmtOK): TModalResult;
begin
  Result := ExtMsg(dialogsize, sMsgTitle, title, message, '',
                   uxclBlue, uxclLightBlue, buttons, widechar(icon));
end;

{** See interface docs for behavior and parameters. }
function UXDialog(const dialogsize: TUXDialogSize;
                  const title, message: string;
                  buttons: TUXMsgDlgBtns;
                  const mtype: TMsgDlgType): TModalResult;
begin
  Result := UXDialog(dialogsize, sMsgTitle, title, message, buttons, mtype);
end;

{** See interface docs for behavior and parameters. }
function UXDialog(const dialogsize: TUXDialogSize;
                  const header, title, message: string;
                  buttons: TUXMsgDlgBtns;
                  const mtype: TMsgDlgType): TModalResult;
var
  icon: UXImage;
begin
  case mtype of
    mtWarning:      icon := widechar($26A0); // ⚠️ Warning sign
    mtError:        icon := widechar($274C); // ❌ Cross mark
    mtInformation:  icon := widechar($2139); // ℹ️ Info symbol
    mtConfirmation: icon := widechar($2753); // ❓ Question mark
    mtCustom:       icon := uxmtCog; // ⚙️ Gear
  else
    icon := uxmtCog;
  end;

  Result := ExtMsg(dialogsize, header, title, message, '',
                   uxclBlue, uxclLightBlue, buttons, icon);
end;

{**
  See interface docs. Renders inline panel when @code(dialogsize = uxdOnForm) and a sender is available.
}
procedure UXMessage(const dialogsize: TUXDialogSize; const title, message: string;
  const icon: UXImage = uxmtOK;
  sender: TForm = nil);
var
  tp: TPanel;
  tl: TLabel;
  tb: TButton;
  df: TDialogForm;
begin
  if (dialogsize = uxdOnForm) then begin

    if (sender <> nil) and (sender.Showing) and (UXDialogIsBig(uxdAuto)) then
    begin
      // On e.g. touch screens display a full screen message
      tp := TPanel.Create(sender); // Create a panel to cover the screen
      tp.Parent := sender;
      tp.Top := 0;
      tp.Left := 0;
      tp.Height := sender.Height;
      tp.Width := sender.Width;
      tp.BringToFront;
      tp.Color := uxclLightBlue;

      tl := TLabel.Create(tp);
      tl.parent := tp;
      tl.autosize := false;
      tl.Font.Color := uxclBlue;
      tl.Caption := message;
      tl.Font.Size := tp.Width div 20;
      tl.WordWrap := True;
      tl.top := 5;
      tl.left := 5;
      tl.width := tp.width-10;
      tl.height := tp.height;
      tl.WordWrap := true;

      tb := TButton.Create(tp);
      tb.Parent := tp;
      tb.AutoSize := True;
      tb.Caption := smbUXOK;

      if tb.Height < (tp.Height div 5) then
      begin
        tb.AutoSize := False;
        tb.Height := tp.Height div 5;
      end;

      tb.Left := 0;
      tb.Width := tp.Width;
      tb.Top := tp.Height - tb.Height - 10;
      tb.Font.Color := sender.Font.Color; // Match parent form font color

      // Close logic
      df := TDialogForm.CreateNew(nil);
      tb.OnClick := @df.UXMessageOnClick;
    end
    else
    ExtMsg(uxdAuto, sMsgTitle, title, message, '',
           uxclBlue, uxclLightBlue, [mbOK], widechar(icon))
  end else
    ExtMsg(dialogsize, sMsgTitle, title, message, '',
           uxclBlue, uxclLightBlue, [mbOK], widechar(icon))
end;

{** See interface docs for behavior and parameters. }
function ExtInput(
  const dialogsize: TUXDialogSize;
  const ACaption, ATitle, ADesc, ADefault: string;
  var ModalResult: TModalResult;
  const icon: UXImage = uxmtCog
): string;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Edit: TEdit;
  OkButton, CancelButton: TButton;
  bgcol: TColor;
  big: Boolean;
  totalButtonsWidth: Integer;
begin
  Result := '';
  ModalResult := mrCancel;
  big := UXDialogIsBig(dialogsize);
  bgcol := IfThen(TrndiNative.isDarkMode, uxclGray, clWhite);

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    // Use shared helper for consistent title/description layout
    SetupDialogTitleDesc(Dialog, big, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Input field ---
    Edit := TEdit.Create(Dialog);
    Edit.Parent := Dialog;
    Edit.Left := DescLabel.Left;
    Edit.Width := DescLabel.Width;
    Edit.Top := DescLabel.Top + DescLabel.Height + ifthen(big, Padding * 2, Padding);
    Edit.Text := ADefault;
    if big then
    begin
      Edit.Font.Size := 20;
      {$IFDEF Windows} Edit.Height := 42; {$ENDIF}
    end;

    // --- OK Button ---
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbSelect;
    OkButton.ModalResult := mrOk;
    OkButton.Width := 80;
    if big then
    begin
      OkButton.Width := OkButton.Width * 2;
      OkButton.Height := OkButton.Height * 2;
      OkButton.Font.Size := 12;
    end;
    Dialog.ActiveControl := OkButton;

    // --- Cancel Button ---
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.Caption := smbUXCancel;
    CancelButton.ModalResult := mrCancel;
    CancelButton.Width := 80;
    if big then
    begin
      CancelButton.Width := CancelButton.Width * 2;
      CancelButton.Height := CancelButton.Height * 2;
      CancelButton.Font.Size := 12;
    end;

    // --- Center buttons ---
    OkButton.Top := Edit.Top + Edit.Height + ifthen(big, Padding * 3, Padding * 2);
    CancelButton.Top := OkButton.Top;
    totalButtonsWidth := OkButton.Width + Padding + CancelButton.Width;
    OkButton.Left := (Dialog.ClientWidth - totalButtonsWidth) div 2;
    CancelButton.Left := OkButton.Left + OkButton.Width + Padding;

    Dialog.ClientHeight := OkButton.Top + OkButton.Height + Padding;
    Dialog.ActiveControl := Edit;

    ModalResult := Dialog.ShowModal;
    if ModalResult = mrOk then
      Result := Edit.Text;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function ExtList(
  const dialogsize: TUXDialogSize;
  const ACaption, ATitle, ADesc: string;
  const Choices: array of string;
  const Default: boolean = false;
  const icon: UXImage = uxmtCog
): Integer;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Combo: TComboBox;
  OkButton, CancelButton: TButton;
  bgcol: TColor;
  i, totalButtonsWidth: Integer;
  big: Boolean;
begin
  Result := -1;
  big := UXDialogIsBig(dialogsize);
  bgcol := IfThen(TrndiNative.isDarkMode, uxclGray, clWhite);

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    SetupDialogTitleDesc(Dialog, big, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- ComboBox ---
    Combo := TComboBox.Create(Dialog);
    Combo.Parent := Dialog;
    for i := 0 to High(Choices) do
      Combo.Items.Add(Choices[i]);
    Combo.ReadOnly := True;
    Combo.Style := csDropDownList;
    Combo.Left := DescLabel.Left;
    Combo.Width := DescLabel.Width;
    if big then
    begin
      Combo.Font.Size := 20;
      {$IFDEF Windows} Combo.Height := 42; {$ENDIF}
    end;
    Combo.Top := DescLabel.Top + DescLabel.Height + ifthen(big, Padding * 2, Padding);
    Combo.ItemIndex := 0;

    // --- OK Button ---
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbSelect;
    OkButton.ModalResult := mrOk;
    OkButton.Width := 80;
    if big then
    begin
      OkButton.Width := OkButton.Width * 2;
      OkButton.Height := OkButton.Height * 2;
      OkButton.Font.Size := 12;
    end;
    Dialog.ActiveControl := OkButton;

    // --- Cancel Button ---
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    if default then begin
       CancelButton.Caption := smbUXDefault;
       CancelButton.ModalResult := mrCancel;
    end
    else begin
       CancelButton.Caption := smbUXCancel;
       CancelButton.ModalResult := mrCancel;
    end;

    CancelButton.Width := 80;
    if big then
    begin
      CancelButton.Width := CancelButton.Width * 2;
      CancelButton.Height := CancelButton.Height * 2;
      CancelButton.Font.Size := 12;
    end;

    // --- Center buttons ---
    OkButton.Top := Combo.Top + Combo.Height + ifthen(big, Padding * 3, Padding * 2);
    CancelButton.Top := OkButton.Top;
    totalButtonsWidth := OkButton.Width + Padding + CancelButton.Width;
    OkButton.Left := (Dialog.ClientWidth - totalButtonsWidth) div 2;
    CancelButton.Left := OkButton.Left + OkButton.Width + Padding;

    // Final height
    Dialog.ClientHeight := OkButton.Top + OkButton.Height + Padding;
    Dialog.ActiveControl := Combo;

    if Dialog.ShowModal = mrOk then
      Result := Combo.ItemIndex
    else
      Result := -1;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function ExtTable(
  const dialogsize: TUXDialogSize;
  const ACaption, ATitle, ADesc: string;
  const Keys, Values: array of string;
  const icon: UXImage = uxmtCog;
  const key: string = '';
  const value: string = ''
): Integer;
const
  Padding = 16;
  GridHeight = 200;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Grid: TStringGrid;
  BgCol: TColor;
  OkButton, CancelButton: TButton;
  totalButtonsWidth, i: Integer;
  big: boolean;
begin
  Result := -1;
  big := UXDialogIsBig(dialogsize);
  BgCol := IfThen(TrndiNative.isDarkMode, uxclGray, clWhite);

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := True;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    SetupDialogTitleDesc(Dialog, big, icon, BgCol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Grid ---
    Grid := TStringGrid.Create(Dialog);
    Grid.Parent := Dialog;
    Grid.Left := DescLabel.Left;
    Grid.Width := DescLabel.Width;
    Grid.Top := DescLabel.Top + DescLabel.Height + Padding;
    Grid.Height := ifthen(big, GridHeight + 80, GridHeight);
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

    if big then
    begin
      Grid.Font.Size := 14;
      {$IFDEF Windows} Grid.DefaultRowHeight := 28; {$ENDIF}
    end;

    // --- OK Button ---
    OkButton := TButton.Create(Dialog);
    OkButton.Parent := Dialog;
    OkButton.Caption := smbUXOK;
    OkButton.ModalResult := mrOk;
    OkButton.Width := 80;
    if big then
    begin
      OkButton.Width := OkButton.Width * 2;
      OkButton.Height := OkButton.Height * 2;
      OkButton.Font.Size := 12;
    end;

    // --- Cancel Button ---
    CancelButton := TButton.Create(Dialog);
    CancelButton.Parent := Dialog;
    CancelButton.Caption := smbUXCancel;
    CancelButton.ModalResult := mrCancel;
    CancelButton.Width := 80;
    if big then
    begin
      CancelButton.Width := CancelButton.Width * 2;
      CancelButton.Height := CancelButton.Height * 2;
      CancelButton.Font.Size := 12;
    end;

    // --- Center buttons ---
    OkButton.Top := Grid.Top + Grid.Height + ifthen(big, Padding * 3, Padding * 2);
    CancelButton.Top := OkButton.Top;
    totalButtonsWidth := OkButton.Width + Padding + CancelButton.Width;
    OkButton.Left := (Dialog.ClientWidth - totalButtonsWidth) div 2;
    CancelButton.Left := OkButton.Left + OkButton.Width + Padding;

    Dialog.ClientHeight := OkButton.Top + OkButton.Height + Padding;

    if Dialog.ShowModal = mrOk then
      Result := Grid.Row;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function ExtLog(
  const dialogsize: TUXDialogSize;
  const caption, msg, log: string;
  const icon: UXImage = uxmtCog;
  scale: integer = 1
): TModalResult;
begin
  Result := ExtMsg(dialogsize, sMsgTitle, caption, msg, log,
                   uxclBlue, uxclLightBlue, [mbOK], icon, scale);
end;

{** See interface docs for behavior and parameters. }
function ExtMsg(
  const dialogsize: TUXDialogSize;
  const caption, title, desc, logmsg: string;
  dumpbg: TColor = uxclWhite;
  dumptext: TColor = uxclRed;
  buttons: TUXMsgDlgBtns = [mbAbort];
  const icon: UXImage = uxmtCog;
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
  big: boolean;
begin
  bgcol := IfThen(TrndiNative.isDarkMode, uxclGray, clWhite);
  big := UXDialogIsBig(dialogsize);

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
    LogPanel.Height := IfThen(big, 100, 50) * scale;
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

{** See interface docs for behavior and parameters. }
function ExtError(const dialogsize: TUXDialogSize;
                  const msg, error: string;
                  const icon: UXImage = uxmtCog): TModalResult;
begin
  Result := ExtMsg(dialogsize,
                   sExtErr,  // caption
                   sErr,     // title
                   msg,      // description
                   error,    // log/dump text
                   uxclWhite, // dump background color
                   uxclRed, // dump text color
                   [mbAbort], // buttons
                   icon);
end;

{** See interface docs for behavior and parameters. }
function ExtError(const dialogsize: TUXDialogSize;
                  const error: string;
                  const icon: UXImage = uxmtCog): TModalResult;
begin
  Result := ExtMsg(dialogsize,
                   sExtErr,   // caption
                   sExtTitle, // title
                   sErr,      // description
                   error,     // log
                   uxclWhite,
                   uxclRed,
                   [mbAbort],
                   icon);
end;

{** See interface docs for behavior and parameters. }
function ExtSucc(const dialogsize: TUXDialogSize;
                 const msg, desc, output: string;
                 dumpbg: TColor = uxclLightGreen;
                 dumptext: TColor = uxclDarkGreen;
                 const icon: UXImage = uxmtOK): TModalResult;
begin
  Result := ExtMsg(dialogsize,
                   sSuccTitle, // caption
                   msg,        // title
                   desc,       // description
                   output,     // log/dump
                   dumpbg,
                   dumptext,
                   [mbOK],
                   widechar(icon));
end;

{** See interface docs for behavior and parameters. }
function ExtSuccEx(const dialogsize: TUXDialogSize;
                   const msg, desc, output: string;
                   btns: TUXMsgDlgBtns;
                   dumpbg: TColor = uxclLightGreen;
                   dumptext: TColor = uxclDarkGreen;
                   const icon: UXImage = uxmtOK): TModalResult;
begin
  Result := ExtMsg(dialogsize,
                   sSuccTitle,
                   msg,
                   desc,
                   output,
                   dumpbg,
                   dumptext,
                   btns,
                   widechar(icon));
end;

{**
  See interface docs. Attempts OS-appropriate defaults.
}
function FontTXTInList(out fname: string): Boolean;
begin
  {$if DEFINED(X_LINUXBSD)}
    fname := 'Noto Sans';
    try
      Result := Screen.Fonts.IndexOf(fname) >= 0;
    finally
    end;
  {$elseif DEFINED(WINDOWS)}
    fname := 'Segoe UI';
    try
      Result := Screen.Fonts.IndexOf(fname) >= 0;
    finally
    end;
  {$else}
    fname := 'font';
    Result := True;
  {$endif}
end;

{**
  See interface docs. Returns a font suitable for UI/emoji display if available.
}
function FontGUIInList(out fname: string): Boolean;
begin
  {$if DEFINED(X_LINUXBSD)}
    fname := 'Noto Color Emoji';
    try
      Result := (Screen.Fonts.IndexOf('Noto Emoji') >= 0) or
                (Screen.Fonts.IndexOf('Noto Color Emoji') >= 0);
    finally
    end;
  {$elseif DEFINED(WINDOWS)}
    fname := 'Consolas';
    try
      Result := Screen.Fonts.IndexOf(fname) >= 0;
    finally
    end;
  {$else}
    fname := 'font';
    Result := True;
  {$endif}
end;

{**
  Handle Enter/Esc keys to activate default/escape buttons.
  @param Sender Dialog form.
  @param Key Key code pressed.
  @param Shift Shift-state (unused).
}
procedure TDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  o: TCustomButton;
  i, cancel, no, ok, ct: Integer;
  btns: TComponent;
  target: TWinControl;
begin
  if not (key in [VK_ESCAPE, VK_RETURN]) then Exit;
  cancel := -1;
  no := -1;
  ct := 0;
  btns := Self.FindComponent('pnButtons');
  if btns = nil then
    target := Self
  else
    target := btns as TPanel;

  for i := 0 to target.ComponentCount - 1 do
  begin
    if target.components[i] is TCustomButton then
    begin
      o := target.components[i] as TCustomButton;
      if o.ModalResult = mrCancel then cancel := i;
      if o.ModalResult = mrNo then no := i;
      if o.ModalResult = mrOk then ok := i;
      if o.ModalResult = mrClose then ok := i;
      Inc(ct);
    end;
  end;

  if key = vk_escape then
  begin
    if cancel >= 0 then
      (target.Components[cancel] as TCustomButton).Click
    else if no >= 0 then
      (target.Components[no] as TCustomButton).Click;
  end
  else if key = VK_RETURN then
  begin
    if ((ct = 1) and (ok >= 0)) then
      (target.Components[ok] as TCustomButton).Click
    else if ((ct = 2) and (ok = 1)) then
      (target.Components[ok] as TCustomButton).Click;
  end;
end;

{$ifndef Windows}
{** Ensure KeyPreview is set on non-Windows upon handle creation. }
procedure TDialogForm.CreateWnd;
begin
  inherited CreateWnd;
  KeyPreview := True;
end;
{$endif}

{$ifdef Windows}
{**
  Set system menu style, apply immersive dark titlebar when available, and refresh frame.
}
procedure TDialogForm.CreateWnd;
const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
var
  Value: Integer;
begin
  inherited CreateWnd;
  if HandleAllocated then
    SetWindowLong(Handle, GWL_STYLE,
      GetWindowLong(Handle, GWL_STYLE) or WS_SYSMENU);

  KeyPreview := True;
  if not TrndiNative.isDarkMode then Exit;
  if (Win32MajorVersion < 10) or
     ((Win32MajorVersion = 10) and (Win32BuildNumber < 17763)) then
    Exit; // Dark mode supported from Windows 10 1809 (build 17763)

  Value := 1;
  try
    DwmSetWindowAttribute(Handle, DWMWA_USE_IMMERSIVE_DARK_MODE,
      @Value, SizeOf(Value));
  except end;

  SetWindowPos(Handle, 0,0,0,0,0,
    SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
end;
{$endif}

{** Close handler for full-screen overlay messages created by @link(UXMessage). }
procedure TDialogForm.UXMessageOnClick(sender: TObject);
begin
  ((sender as TButton).Parent as TPanel).Hide;
  ((sender as TButton).Parent as TPanel).Free;
  Self.Free;
end;

{$ifdef Windows}
{**
  Owner-draw for dark buttons on Windows.
  @param Sender The @code(TBitBtn) being drawn.
  @param ACanvas Canvas to draw on.
  @param ARect Button rectangle.
  @param State Button state (up/down/hot).
}
procedure TDialogForm.ButtonDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TButtonState);
var
  Btn: TBitBtn absolute Sender;
  TxtFlags: Cardinal;
begin
  // 1) Background
  if bsDown = State then
    ACanvas.Brush.Color := RGBToColor(30, 30, 30)
  else
    ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(ARect);

  // 2) Border
  ACanvas.Pen.Color := RGBToColor(80, 80, 80);
  ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  // 3) Text
  ACanvas.Font.Assign(Btn.Font);
  ACanvas.Font.Color := clWhite;
  TxtFlags := DT_CENTER or  DT_VCENTER or DT_SINGLELINE;
  DrawText(ACanvas.Handle, PChar(Btn.Caption), Length(Btn.Caption),
    ARect, TxtFlags);

  // 4) Focus indicator
  if bsHot = State then
    ACanvas.DrawFocusRect(ARect);
end;
{$endif}

end.
