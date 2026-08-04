(*
 * slicke.ux.alert.pp
 * Adaptive Lazarus/FPC dialogs and input helpers (emoji icons, dark mode, touch-aware layout).
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 * License: Apache License 2.0
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
  - @link(SlickeMessage) for simple, one-button informational messages.
  - @link(SlickeDialog) overloads for message dialogs with button sets or Lazarus TMsgDlgType mapping.
  - @link(SlickeMsg), @link(SlickeLog), @link(SlickeError), @link(SlickeSucc), @link(SlickeSuccEx) for rich dialogs with dumps/logs.
  - @link(SlickeInput), @link(SlickePasswordInput), @link(SlickeNumericInput), @link(SlickeIntInput), @link(SlickeList), @link(SlickeTable) for data entry.
  - @link(SlickeDatePicker) for date selection with optional min/max constraints.

  Platform support:
  - Windows: emoji rendering via Direct2D/DirectWrite; custom dark-titlebar opt-in where possible.
  - Linux/BSD: emoji/text via canvas using Noto fonts when available.

  @author
  Björn Lindh, with PasDoc annotations added.
}
unit slicke.ux.alert;

{$I ../../inc/native.inc}
{$modeswitch advancedrecords}
interface

uses
Classes, SysUtils, Dialogs, Forms, ExtCtrls, StdCtrls, Controls, Graphics, Math,
IntfGraphics, FPImage, graphtype, lcltype, Trndi.Native, Grids, Spin, IpHtml, Iphttpbroker, slicke.ux.native, SpinEx, LCLIntf,
EditBtn, Clipbrd,
{$ifdef X_MAC}
CocoaAll, nsutils.cocoahelpers,
{$endif}
{$ifdef X_WIN}
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
sErrTitle   = 'Error';
sErrMsg     = 'An error occurred';
sURLTitle   = 'Open external link?';
sURL        = 'Leave the app and open your browser? The link may not be secure!';

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
smbSlickeOpenFile   = 'Open File';
smbSlickeMinimize   = 'Minimize';
smbSelect       = 'Select';
smbSlickeAgree      = 'Agree';
smbSlickeRead       = 'Read...';
smbSlickeDefault    = 'Default';
smbSlickeSnooze     = 'Snooze';

sKey   = 'Key';
sValue = 'Value';

const
  FLOAT_NONE = -999.66; // < For use in dialogs where a float value is unlimited
  {**
    @name Emoji-based dialog icons
    @desc
    Unicode codepoints rendered as emoji or symbols on supported platforms.
    Fallbacks depend on available system fonts.
  }
uxmtOK             = WChar($2705); // ✅ Ticked box
uxmtWarning        = WChar($26A0); // ⚠️ Warning sign
uxmtError          = WChar($274C); // ❌ Cross mark
uxmtInformation    = WChar($2139); // ℹ️ Info symbol
uxmtConfirmation   = WChar($2753); // ❓ Question mark
uxmtCog            = WChar($2699); // ⚙️ Gear
uxmtSquare         = WChar($2B1C); // ⬜ Square (U+274F is a dingbat, absent from every emoji font)
uxmtCustom         = uxmtCog;

  {**
    @name Dialog colors
    @desc
    Dialog colors presets
  }
uxclBlue = $00AA6004;
uxclLightBlue = $00FDD8AA;
uxclWhite = $00F5F2FD;
uxclRed = $003411A9;
uxclLightGreen = $0095EEC4;
uxclDarkGreen = $00147C4A;
uxclGray = $00322B27;

 {**
    @name Dialog Scales
    @desc
    Presets for dialog scales
  }
uxscSmall = 0.75;
uxscNormal = 1;
uxscBig = 5;
uxscBigger = 7.5;
uxscLarge = 10;
uxscHuge = 20;
uxscEnormous = 30;

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

  {**
    @name System constants
    @desc
    Constants used for system-related things
  }
sHTMLLineBreak = '<br>';
type
  {** Emoji glyph used for icons. Typically a single WChar codepoint. }
SlickeUXImage = WChar;

  {**
    Modal dialog form used internally by UX helpers.
    @remarks
      - Overrides @code(CreateWnd) for platform tweaks (e.g., dark title bar on Windows).
      - Handles keyboard navigation (Enter/Escape) in @link(FormKeyDown).
      - On Windows can owner-draw buttons via @link(ButtonDrawItem).
  }
TDialogForm = class(TForm)
public
  // For log message expansion
  LogExpandWrapper: TPanel;
  LogExpandMemo: TMemo;
  LogExpandHtmlPanel: TIpHtmlPanel;
  LogExpandButton: TControl;
  LogIsHTML: boolean;

  procedure addButton(const btnName: string);
  {** Sets the title and content text for the dialog. }
  procedure setContent(const titleValue, value: string; const extraValue: string = '');
  {** Expands/collapses the log/dump panel. }
  procedure ExpandLogDialog(Sender: TObject);
  {** Keyboard shortcuts: Enter to confirm (when appropriate), Esc to cancel/No. }
  procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
protected
  title, content, extra: string;
  hasHTML: boolean;
  buttons: TStringArray;

  function getContent: string;
    {** Finalizes platform window style, sets KeyPreview, and enables dark mode title bar where supported. }
  procedure CreateWnd; override;
  // Override DoShow instead of using an OnShow event method name.
  procedure DoShow; override;
public
  {$ifdef X_WIN}
    {** Owner-draw routine for bit buttons on Windows to match dark mode styling. }
  procedure ButtonDrawItem(Sender: TObject;
    ACanvas: TCanvas; ARect: TRect; State: TButtonState);
  {$endif}
    {** OnClick handler used by inline full-screen message overlays created via @link(SlickeMessage). }
  procedure SlickeMessageOnClick(sender: TObject);
    {** OnMouseDown companion — fires on first touch contact so the overlay
        closes even when the Qt release point drifts outside the button rect. }
  procedure SlickeMessageOnMouseDown(sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
public
    {** Helper fields for font picker dialog. }
  FontPickerPreview: TLabel;

  property contents: string read getContent;
  property titleText: string write title;
  property contentText: string write content;
  property extraText: string write extra;
    {** OnChange handler for font combo box in SlickeFontPicker. }
  procedure FontComboChange(Sender: TObject);
  procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
  procedure HTMLHotClick(Sender: TObject);
  procedure ElementKeyDown(Sender: TObject; var Key: char);
end;

{$ifdef X_WIN}
  {**
    Uses TCustomControl for full custom painting and TWinControl capabilities.
  }
TDarkButton = class(TCustomControl)
private
  FModalResult: TModalResult;
  FDown: boolean;
  FHot: boolean;
  FFocused: boolean;
  FCaption: string;
  procedure SetCaption(const AValue: string);
protected
  procedure Paint; override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  procedure MouseEnter; override;
  procedure MouseLeave; override;
  procedure Click; override;
  procedure KeyDown(var Key: word; Shift: TShiftState); override;
  procedure DoEnter; override;
  procedure DoExit; override;
public
  constructor Create(AOwner: TComponent); override;
  property Caption: string read FCaption write SetCaption;
  property ModalResult: TModalResult read FModalResult write FModalResult;
end;
{$endif}

  {**
    Size preset for dialog layout.
    @value sdsNormal Standard dialog layout.
    @value sdsBig Larger layout suitable for touch/TV screens.
    @value sdsAuto Auto-detect (big if touch screen available).
    @value sdsOnForm Render message inline on an existing form (used by @link(SlickeMessage)).
  }
TSlickeDialogSize = (sdsNormal = 0, sdsBig = 1, sdsAuto = 3, sdsOnForm = 4, sdsMedium = 5);

  {**
    Available dialog buttons for UX helpers.
    @remarks Includes standard Lazarus modal buttons and a few custom labels (e.g. OpenFile, Minimize, Agree, Read, Default).
  }
TSlickeMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
  mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose, mbSlickeOpenFile, mbSlickeMinimize, mbSlickeAgree, mbSlickeRead, mbSlickeDefault, mbSlickeSnooze,
  {** Not a button: "no explicit default" sentinel for the @code(ADefault) parameter. }
  mbSlickeNone);

  {** An ordered list of @link(TSlickeMsgDlgBtn) specifying which buttons to show.
      Written as @code([mbClose, mbCancel]) exactly like a set, but — unlike a set —
      the order is preserved: buttons are laid out left to right in the order given,
      and the same button may appear more than once. Pass @code(nil) (or omit the
      parameter) to get the dialog's default buttons. }
TSlickeMsgDlgBtns = array of TSlickeMsgDlgBtn;

  {**
    One button row per platform convention, written as a nested list:
    @code([[mbOK, mbCancel], [mbCancel, mbOK]]).

    The first row is used where the affirmative button comes first (Windows, KDE),
    the second where it comes last (macOS, GNOME) — see @link(SlickeUseReversedButtons).
    A single-element list @code([[mbOK, mbCancel]]) is used everywhere, and is
    equivalent to passing the flat list @code([mbOK, mbCancel]).

    @remarks A flat @link(TSlickeMsgDlgBtns) is never reordered: it means "show
      exactly this, everywhere". Pass a nested list when the dialog should follow
      the local convention.
  }
TSlickeMsgDlgBtnRows = array of TSlickeMsgDlgBtns;

  {**
    Which button-order convention dialogs follow.
    @value smdlAuto Detect from the platform/desktop; @seealso(SlickeUseReversedButtons)
    @value smdlAffirmativeFirst Affirmative button first, dismissive last (Windows, KDE).
    @value smdlAffirmativeLast Affirmative button last (macOS, GNOME).
  }
TSlickeMsgDlgLayout = (smdlAuto, smdlAffirmativeFirst, smdlAffirmativeLast);

  {** Mapping of @link(TSlickeMsgDlgBtn) to localized captions. }
ButtonLangs = array[TSlickeMsgDlgBtn] of string;

  {**
    Show a simple message dialog, optionally inline on a form in @code(sdsOnForm) mode.
    @param dialogsize Layout preset; @seealso(TSlickeDialogSize)
    @param title Dialog title text (top label).
    @param message Main message body.
    @param icon Emoji icon; defaults to @code(uxmtOK).
    @param sender Optional form used when @code(dialogsize = sdsOnForm) to render a full-screen overlay.
  }
procedure SlickeMessage(const dialogsize: TSlickeDialogSize; const title, message: string; const icon: SlickeUXImage = uxmtOK; sender: TForm = nil);

{**
  Show a simple message dialog, optionally inline on a form in @code(sdsOnForm) mode.
  @param title Dialog title text (top label).
  @param message Main message body.
  @param icon Emoji icon; defaults to @code(uxmtOK).
  @param sender Optional form used when @code(dialogsize = sdsOnForm) to render a full-screen overlay.
}
procedure SlickeMessage(const title, message: string; const icon: SlickeUXImage = uxmtOK; sender: TForm = nil);

  {**
    Generic dialog with custom button set and emoji icon.
    @param dialogsize Layout preset; @seealso(TSlickeDialogSize)
    @param title Title text displayed above @code(message).
    @param message Description/body text.
    @param buttons Buttons to display, in left-to-right order.
    @param icon Emoji icon; defaults to @code(uxmtOK).
    @returns Lazarus modal result corresponding to the button clicked.
  }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
buttons: TSlickeMsgDlgBtns;
const icon: SlickeUXImage = uxmtOK;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {**
    Generic dialog with custom button set and Lazarus message type mapped to a default icon.
    @param dialogsize Layout preset; @seealso(TSlickeDialogSize)
    @param title Title text displayed above @code(message).
    @param message Description/body text.
    @param buttons Buttons to display, in left-to-right order.
    @param mtype Lazarus message dialog type; maps to a reasonable emoji icon.
    @returns Lazarus modal result corresponding to the button clicked.
  }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
buttons: TSlickeMsgDlgBtns;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {**
    Generic dialog with custom header line (caption), title and message and TMsgDlgType mapping.
    @param dialogsize Layout preset; @seealso(TSlickeDialogSize)
    @param header Window caption (top title bar).
    @param title Title text (bold, in content).
    @param message Description/body text.
    @param buttons Buttons to display, in left-to-right order.
    @param mtype Lazarus message dialog type; maps to a reasonable emoji icon.
    @returns Lazarus modal result corresponding to the button clicked.
  }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const header, title, message: string;
buttons: TSlickeMsgDlgBtns;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

{**
  Simplified Extended message dialog for displaying yes/no dialogs
  @param dialogsize Layout preset; @seealso(TSlickeDialogSize)
  @param caption Window caption.
  @param title Title text.
  @param desc Description of dialog.
  @param micon Icon for the dialog
  @param scale Size for the actual dialog
  @returns Lazarus modal result corresponding to the button clicked.
}
function SlickeMsgYesNo(
const dialogsize: TSlickeDialogSize;
const caption, desc: string;
const micon: SlickeUXImage = uxmtConfirmation;
const scale: single = 1): boolean;

{**
  Simplified Extended message dialog for displaying yes/no dialogs
  @param caption Window caption.
  @param title Title text.
  @param desc Description of dialog.
  @param micon Icon for the dialog
  @param scale The size of the actual dialog
  @returns Lazarus modal result corresponding to the button clicked.
}
function SlickeMsgYesNo(
const caption, desc: string;
const micon: SlickeUXImage = uxmtConfirmation;
const scale: single = 1): boolean;

{**
  Extended message dialog supporting an optional log/dump panel with custom colors.
  @param caption Window caption.
  @param title Title text.
  @param desc Description/body text (supports wrapping and scrolling in big mode).
  @param logmsg Optional log/dump text displayed in a fixed panel at the bottom; pass empty to hide.
  @param dumpbg Background color for log/dump panel (ARGB).
  @param dumptext Text color for log/dump panel (ARGB).
  @param buttons Buttons to display, in left-to-right order (default [mbAbort]).
  @param icon Emoji icon to render.
  @param scale Optional log panel vertical scale multiplier (for big outputs).
  @returns Lazarus modal result corresponding to the button clicked.
}
function SlickeMsg(
const caption, title, desc, logmsg: string;
dumpbg: TColor = uxclWhite;
dumptext: TColor = uxclRed;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;

  {**
    Extended message dialog supporting an optional log/dump panel with custom colors.
    @param dialogsize Layout preset; @seealso(TSlickeDialogSize)
    @param caption Window caption.
    @param title Title text.
    @param desc Description/body text (supports wrapping and scrolling in big mode).
    @param logmsg Optional log/dump text displayed in a fixed panel at the bottom; pass empty to hide.
    @param dumpbg Background color for log/dump panel (ARGB).
    @param dumptext Text color for log/dump panel (ARGB).
    @param buttons Buttons to display, in left-to-right order (default [mbAbort]).
    @param icon Emoji icon to render.
    @param scale Optional log panel vertical scale multiplier (for big outputs).
    @returns Lazarus modal result corresponding to the button clicked.
  }
function SlickeMsg(const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
dumpbg: TColor = uxclWhite;
dumptext: TColor = uxclRed;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;

  {**
    HTML-only dialog with buttons.
    @param dialogsize Layout preset.
    @param caption Window caption.
    @param html HTML content to display in the dialog.
    @param buttons Buttons to display, in left-to-right order (default [mbAbort]).
    @param icon Emoji icon (default gear).
    @param scale Content height multiplier (default 1).
    @returns Modal result based on user button selection.
    @remarks This variant displays only HTML content without title/description sections.
  }
function SlickeMsg(const dialogsize: TSlickeDialogSize;
const caption, html: string;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1; hpadding: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {**
    Alias for @lnk(SlickeMsg) with HTML data
  }
function SlickeHTMLMsg(const dialogsize: TSlickeDialogSize;
const caption, html: string;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;

  {**
    Helper for @lnk(SlickeMsg) with text data
  }
function SlickePrompt(const dialogsize: TSlickeDialogSize;
const caption, text: string;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;

  {**
    Extended message dialog with HTML support in log panel.
    @param dialogsize Layout preset.
    @param caption Window caption.
    @param title Title text (bold).
    @param desc Description text.
    @param logmsg Log or dump text (can be plain text or HTML).
    @param isHTML If @true, logmsg is interpreted as HTML data using TIpHtmlPanel; otherwise plain text with TMemo.
    @param dumpbg Background color for log panel (default white).
    @param dumptext Text color for log panel (default red, ignored if HTML is used).
    @param buttons Buttons to display, in left-to-right order (default [mbAbort]).
    @param icon Emoji icon (default gear).
    @param scale Log panel vertical scale multiplier.
    @returns Modal result based on user button selection.
    @remarks HTML rendering is supported cross-platform via TIpHtmlPanel from IpHtml unit. Use standard HTML tags like &lt;b&gt;, &lt;i&gt;, &lt;font color="red"&gt;, etc.
  }
function SlickeMsgEx(const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
isHTML: boolean;
dumpbg: TColor = uxclWhite;
dumptext: TColor = uxclRed;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

{**
  @name Platform-ordered button rows
  @desc
  Overloads taking a @link(TSlickeMsgDlgBtnRows) instead of a single button list, so a
  dialog can follow the local convention: @code([[mbOK, mbCancel], [mbCancel, mbOK]])
  shows the first row on Windows/KDE and the second on macOS/GNOME. Everything else
  behaves exactly like the matching flat-list version.
}

  {** Row-aware @link(SlickeDialog). }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtOK;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware @link(SlickeDialog) with a Lazarus message type. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
const buttons: TSlickeMsgDlgBtnRows;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware @link(SlickeDialog) with a separate window caption. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const header, title, message: string;
const buttons: TSlickeMsgDlgBtnRows;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware @link(SlickeMsg). }
function SlickeMsg(
const caption, title, desc, logmsg: string;
dumpbg: TColor;
dumptext: TColor;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware @link(SlickeMsg) with a layout preset. }
function SlickeMsg(const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
dumpbg: TColor;
dumptext: TColor;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware HTML-only @link(SlickeMsg). }
function SlickeMsg(const dialogsize: TSlickeDialogSize;
const caption, html: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1; hpadding: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware @link(SlickeHTMLMsg). }
function SlickeHTMLMsg(const dialogsize: TSlickeDialogSize;
const caption, html: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware @link(SlickePrompt). }
function SlickePrompt(const dialogsize: TSlickeDialogSize;
const caption, text: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {** Row-aware @link(SlickeMsgEx). }
function SlickeMsgEx(const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
isHTML: boolean;
dumpbg: TColor;
dumptext: TColor;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult; overload;

  {**
    Convenience wrapper for @link(SlickeMsg) that shows a message and a log/dump with an OK button.
    @param dialogsize Layout preset.
    @param caption Window caption.
    @param msg Title text.
    @param log Log or output text.
    @param icon Emoji icon (default gear).
    @param scale Log panel vertical scale multiplier.
    @returns @code(mrOK) if confirmed, otherwise the modal result selected by the user.
  }
function SlickeLog(const dialogsize: TSlickeDialogSize;
const caption, msg, log: string;
const icon: SlickeUXImage = uxmtCog;
scale: integer = 1): TModalResult;

  {**
    Show a general error dialog with a short message and an error dump in the log panel.
    @param dialogsize Layout preset.
    @param msg Short explanation shown as description.
    @param error Detailed error text shown in the log panel.
    @param icon Emoji icon (default warning).
    @returns Modal result (default is [mbAbort]).
    @remarks Captions are generic; callers needing subsystem-specific wording
    should wrap @link(SlickeMsg) directly rather than extend this.
  }
function SlickeError(const dialogsize: TSlickeDialogSize;
const msg, error: string;
const icon: SlickeUXImage = uxmtWarning): TModalResult;

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
function SlickeSucc(const dialogsize: TSlickeDialogSize;
const msg, desc, output: string;
dumpbg: TColor = uxclLightGreen;
dumptext: TColor = uxclDarkGreen;
const icon: SlickeUXImage = uxmtOK): TModalResult;

  {**
    Variant of @link(SlickeSucc) that accepts a custom button set.
    @param dialogsize Layout preset.
    @param msg Title text.
    @param desc Description/body text.
    @param output Dump/log text.
    @param btns Custom buttons to display.
    @param dumpbg Background color for dump panel.
    @param dumptext Text color for dump panel.
    @param icon Emoji icon (default @code(uxmtOK)).
    @param scale The size of the text box (multiplyer)
    @returns Modal result.
  }
function SlickeSuccEx(const dialogsize: TSlickeDialogSize;
const msg, desc, output: string;
btns: TSlickeMsgDlgBtns;
dumpbg: TColor = uxclLightGreen;
dumptext: TColor = uxclDarkGreen;
const icon: SlickeUXImage = uxmtOK;
const scale: integer = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;

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
function SlickeList(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
const Choices: array of unicodestring;
const Default: boolean = false;
const icon: SlickeUXImage = uxmtCog): integer; overload;

function SlickeList(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
const Choices: array of string;
const Default: boolean = false;
const icon: SlickeUXImage = uxmtCog): integer; overload;
  {**
    Show a single-line string input dialog.
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial text value.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @param AMasked If @true, the input is masked like a password field.
    @returns The entered string when @code(ModalResult = mrOK); otherwise the previous/default content.
  }
function SlickeInput(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc, ADefault: string;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog;
const AMasked: boolean = false): string;

  {**
    Convenience wrapper over @link(SlickeInput) for masked (password/secret) input.
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial text value.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns The entered string when @code(ModalResult = mrOK); otherwise the previous/default content.
  }
function SlickePasswordInput(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc, ADefault: string;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog): string;

  {**
    Show a numeric input dialog using @code(TFloatSpinEditEx).
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial numeric value.
    @param AMin The lowest possible value
    @param AMax The highst possible value
    @param float If @true, allow fractional values (2 decimal places); if @false, integer-only.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns Entered numeric value if OK; otherwise returns @code(ADefault).
  }
function SlickeNumericInput(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: double;
AMin, AMax: double;
float: boolean;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog): double;

  {**
    Convenience wrapper over @link(SlickeNumericInput) for integer-only input.
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial integer value.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns Entered integer value if OK; otherwise returns @code(ADefault).
  }
function SlickeIntInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: integer;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
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
function SlickeTable(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
const Keys, Values: array of string;
const icon: SlickeUXImage = uxmtCog;
const key: string = '';
const value: string = ''): integer;

  {**
    Show a font picker dialog.
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefaultFont Initial font to display in the picker.
    @param AFontSample Sample text to display in the preview label.
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns The selected TFont object if OK; otherwise returns @code(ADefaultFont).
    @remarks The returned font is a new instance; caller is responsible for freeing it.
  }
function SlickeFontPicker(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefaultFont: TFont;
const AFontSample: string;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog): TFont;

  {**
    Show a date picker dialog using @code(TDateEdit).
    @param dialogsize Layout preset.
    @param ACaption Window caption.
    @param ATitle Title text.
    @param ADesc Description text.
    @param ADefault Initial date value.
    @param AMinDate Minimum allowed date (pass 0 to disable minimum).
    @param AMaxDate Maximum allowed date (pass 0 to disable maximum).
    @param ModalResult Out parameter holding the modal result after closing.
    @param icon Emoji icon (default gear).
    @returns Selected date if OK; otherwise returns @code(ADefault).
  }
function SlickeDatePicker(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: TDateTime;
AMinDate: TDateTime;
AMaxDate: TDateTime;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog): TDateTime;

var
  {** Localized captions for each @link(TSlickeMsgDlgBtn). Initialized from resource strings. }
langs : ButtonLangs = (smbYes, smbUXNo, smbUXOK, smbUXCancel, smbUXAbort, smbUXRetry, smbUXIgnore,
  smbUXAll, smbUXNoToAll, smbUXYesToAll, smbUXHelp, smbUXClose,
  smbSlickeOpenFile, smbSlickeMinimize, smbSlickeAgree, smbSlickeRead, smbSlickeDefault, smbSlickeSnooze,
  '');   // mbSlickeNone is a sentinel, never rendered
  {** When @true, dialogs created by this unit (@link(SlickeDialog), @link(SlickeList),
      @link(SlickeInput), etc.) get their own taskbar button. Defaults to @false,
      which keeps them off the taskbar as transient windows of the initiator.
      Set this before opening a dialog. }
  SlickeDialogsInTaskbar: boolean = false;
  {** Button-order convention used to pick a row from a @link(TSlickeMsgDlgBtnRows).
      Defaults to @code(smdlAuto) (detect from the platform/desktop); set it to force
      one convention, e.g. when testing the other platform's layout. }
  SlickeButtonLayout: TSlickeMsgDlgLayout = smdlAuto;

  {**
    @returns @true when this platform puts the affirmative button last (macOS, GNOME),
      @false when it comes first (Windows, KDE, Haiku).
    @remarks Honours @link(SlickeButtonLayout) and the @code(TRNDI_BUTTON_LAYOUT)
      environment variable (@code(win) / @code(mac) / @code(auto)); the desktop sniff
      behind @code(smdlAuto) is evaluated once and cached.
  }
function SlickeUseReversedButtons: boolean;

  {**
    Pick the row matching this platform's convention.
    @param rows Per-convention button rows; @seealso(TSlickeMsgDlgBtnRows)
    @returns The row for this platform, or the only row when a single one is supplied.
  }
function SlickeResolveButtonRow(const rows: TSlickeMsgDlgBtnRows): TSlickeMsgDlgBtns;

implementation
{$ifdef X_WIN}
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
{$endif}

var
  { Resolved once — the desktop cannot change under a running process. }
  ButtonLayoutCache  : TSlickeMsgDlgLayout = smdlAuto;
  ButtonLayoutCached : boolean = false;

{ Detect the local button-order convention: macOS and the GNOME-derived desktops
  put the affirmative button last, Windows/KDE/Haiku put it first. }
function DetectButtonLayout: TSlickeMsgDlgLayout;
var
  s: string;
begin
  // An explicit override wins, so a mis-detected desktop is fixable without a rebuild.
  // Qualified: the Windows unit exports a different GetEnvironmentVariable.
  s := LowerCase(Trim(SysUtils.GetEnvironmentVariable('TRNDI_BUTTON_LAYOUT')));
  if (s = 'win') or (s = 'kde') then
    Exit(smdlAffirmativeFirst);
  if (s = 'mac') or (s = 'gnome') then
    Exit(smdlAffirmativeLast);

  {$if defined(X_MAC)}
  Result := smdlAffirmativeLast;
  {$elseif defined(X_LINUXBSD)}
  s := LowerCase(Trim(SysUtils.GetEnvironmentVariable('XDG_CURRENT_DESKTOP') + ' ' +
    SysUtils.GetEnvironmentVariable('DESKTOP_SESSION') + ' ' +
    SysUtils.GetEnvironmentVariable('XDG_SESSION_DESKTOP')));
  if (Pos('gnome', s) > 0) or (Pos('cinnamon', s) > 0) or (Pos('budgie', s) > 0) or
    (Pos('pantheon', s) > 0) or (Pos('unity', s) > 0) then
    Result := smdlAffirmativeLast
  else
    Result := smdlAffirmativeFirst;
  {$else}
  Result := smdlAffirmativeFirst;
  {$endif}
end;

{** See interface docs for behavior and parameters. }
function SlickeUseReversedButtons: boolean;
begin
  if SlickeButtonLayout <> smdlAuto then
    Exit(SlickeButtonLayout = smdlAffirmativeLast);

  if not ButtonLayoutCached then
  begin
    ButtonLayoutCache := DetectButtonLayout;
    ButtonLayoutCached := true;
  end;
  Result := ButtonLayoutCache = smdlAffirmativeLast;
end;

{** See interface docs for behavior and parameters. }
function SlickeResolveButtonRow(const rows: TSlickeMsgDlgBtnRows): TSlickeMsgDlgBtns;
begin
  if Length(rows) = 0 then
    Exit(nil);
  // A single row means "same everywhere"; a second row is the reversed-convention one.
  if (Length(rows) > 1) and SlickeUseReversedButtons then
    Result := rows[1]
  else
    Result := rows[0];
end;

{ Which button carries the dialog's default: the explicit override when one was
  given, otherwise the first button as written. For a row list the caller passes
  down row 0's first button, so the default never moves when the row is reversed. }
function PickDefaultButton(const buttons: TSlickeMsgDlgBtns;
  const ADefault: TSlickeMsgDlgBtn): TSlickeMsgDlgBtn;
begin
  if ADefault <> mbSlickeNone then
    Result := ADefault
  else
  if Length(buttons) > 0 then
    Result := buttons[0]
  else
    Result := mbSlickeNone;
end;

{ The default for a row list: the explicit override, else row 0's first button.
  Row 0 is the affirmative-first order, so this stays correct when row 1 is used. }
function RowDefault(const rows: TSlickeMsgDlgBtnRows;
  const ADefault: TSlickeMsgDlgBtn): TSlickeMsgDlgBtn;
begin
  if ADefault <> mbSlickeNone then
    Result := ADefault
  else
  if (Length(rows) > 0) and (Length(rows[0]) > 0) then
    Result := rows[0][0]
  else
    Result := mbSlickeNone;
end;

{ Focus the default button, and mark it default where the widget supports it —
  the Windows TDarkButton is a TCustomControl, so it only draws a focus ring. }
procedure ApplyDefaultButton(Dialog: TDialogForm; Ctrl: TWinControl);
begin
  if not Assigned(Ctrl) then
    Exit;
  if Ctrl is TCustomButton then
    (Ctrl as TCustomButton).Default := true;
  Dialog.ActiveControl := Ctrl;
end;

{ ---- Platform-ordered button rows -------------------------------------------
  Each of these resolves the row for the local convention and hands off to the
  matching flat-list version; no layout logic lives here. }

{** See interface docs for behavior and parameters. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtOK;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeDialog(dialogsize, title, message,
    SlickeResolveButtonRow(buttons), icon, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
const buttons: TSlickeMsgDlgBtnRows;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeDialog(dialogsize, title, message,
    SlickeResolveButtonRow(buttons), mtype, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const header, title, message: string;
const buttons: TSlickeMsgDlgBtnRows;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeDialog(dialogsize, header, title, message,
    SlickeResolveButtonRow(buttons), mtype, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickeMsg(
const caption, title, desc, logmsg: string;
dumpbg: TColor;
dumptext: TColor;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeMsg(caption, title, desc, logmsg, dumpbg, dumptext,
    SlickeResolveButtonRow(buttons), icon, scale, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickeMsg(const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
dumpbg: TColor;
dumptext: TColor;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeMsg(dialogsize, caption, title, desc, logmsg, dumpbg, dumptext,
    SlickeResolveButtonRow(buttons), icon, scale, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickeMsg(const dialogsize: TSlickeDialogSize;
const caption, html: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1; hpadding: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeMsg(dialogsize, caption, html,
    SlickeResolveButtonRow(buttons), icon, scale, hpadding, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickeHTMLMsg(const dialogsize: TSlickeDialogSize;
const caption, html: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeHTMLMsg(dialogsize, caption, html,
    SlickeResolveButtonRow(buttons), icon, scale, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickePrompt(const dialogsize: TSlickeDialogSize;
const caption, text: string;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickePrompt(dialogsize, caption, text,
    SlickeResolveButtonRow(buttons), icon, scale, RowDefault(buttons, ADefault));
end;

{** See interface docs for behavior and parameters. }
function SlickeMsgEx(const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
isHTML: boolean;
dumpbg: TColor;
dumptext: TColor;
const buttons: TSlickeMsgDlgBtnRows;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeMsgEx(dialogsize, caption, title, desc, logmsg, isHTML,
    dumpbg, dumptext, SlickeResolveButtonRow(buttons), icon, scale,
    RowDefault(buttons, ADefault));
end;

{$ifdef X_MAC}
function MacNSColorToTColor(const AColor: NSColor; const Fallback: TColor): TColor;
var
  RGBColor: NSColor;
  R, G, B: Double;
begin
  Result := Fallback;
  if AColor = nil then
    Exit;

  try
    RGBColor := AColor.colorUsingColorSpaceName(NSDeviceRGBColorSpace);
    if RGBColor = nil then
      RGBColor := AColor;

    R := RGBColor.redComponent;
    G := RGBColor.greenComponent;
    B := RGBColor.blueComponent;

    Result := RGBToColor(
      EnsureRange(Round(R * 255), 0, 255),
      EnsureRange(Round(G * 255), 0, 255),
      EnsureRange(Round(B * 255), 0, 255)
    );
  except
    Result := Fallback;
  end;
end;

function MacDialogTextColor(const Fallback: TColor): TColor;
begin
  Result := MacNSColorToTColor(NSColor.textColor, Fallback);
end;

function MacDialogBackgroundColor(const Fallback: TColor): TColor;
begin
  Result := MacNSColorToTColor(NSColor.controlBackgroundColor, Fallback);
end;

function MacInputTextColor(const Fallback: TColor): TColor;
begin
  Result := MacNSColorToTColor(NSColor.textColor, Fallback);
end;

function MacInputBackgroundColor(const Fallback: TColor): TColor;
begin
  Result := MacNSColorToTColor(NSColor.textBackgroundColor, Fallback);
end;
{$endif}


{**
   Helper for getting the base color, based on color mode
}
function getBaseColor: TColor;
var
  light, dark: TColor;
begin
  {$ifdef X_WIN}
  light := GetSysColor(COLOR_WINDOWTEXT);
  dark := clWhite;
  {$else}
  {$ifdef X_MAC}
  // Use Cocoa semantic text color so dialogs match current macOS appearance.
  light := MacDialogTextColor(clWindowText);
  dark := light;
  {$else}
  dark := clWindowText;
  light := dark;
  {$endif}
  {$endif}

  result := IfThen(TrndiNative.isDarkMode, dark, light);
end;

{**
   Helper for getting the background color, based on color mode
}
function getBackground: TColor;
var
  light, dark: TColor;
begin
  {$ifdef X_WIN}
  light := GetSysColor(COLOR_BTNFACE);
  dark := RGB(32, 32, 32);
//    bg := IfThen(TrndiNative.isDarkMode, uxclGray, bg);
  {$else}
  {$ifdef X_MAC}
  // Use Cocoa semantic control background color for native macOS dialogs.
  light := MacDialogBackgroundColor(clBtnFace);
  dark := light;
  {$else}
  light := clBtnFace;
  dark := light;
  {$endif}
  {$endif}

  result := IfThen(TrndiNative.isDarkMode, dark, light);
end;

{**
  Convert TColor to HTML color format (#RRGGBB).
  @param AColor Lazarus TColor value.
  @returns HTML color string in #RRGGBB format.
}
function TColorToHTML(AColor: TColor): string;
var
  rgb: TColor;
  R, G, B: byte;
begin
  rgb := ColorToRGB(AColor);
  { Extract bytes: Windows provides GetRValue/GetGValue/GetBValue, but
    those are not available on Linux; use bit operations which are
    portable. ColorToRGB returns a COLORREF-like value where
    low byte = red, next = green, next = blue. }
  R := byte(rgb and $FF);
  G := byte((rgb shr 8) and $FF);
  B := byte((rgb shr 16) and $FF);
  Result := Format('#%.2x%.2x%.2x', [R, G, B]);
end;

{ ---------------------------------------------------------------------------
  Screen fitting helpers

  Dialog geometry is expressed in fixed pixel budgets. On small touch panels
  (Raspberry Pi 7" 800x480, 1024x600 hats) those budgets exceed the display, so
  every hard-coded width/height is passed through these clamps.
  --------------------------------------------------------------------------- }

const
 { Largest share of the usable screen a dialog may occupy. }
  MaxDialogScreenWidthFraction = 0.95;
 { Smallest screen that still gets the full sdsBig layout; below this a touch
   screen falls back to sdsMedium instead. }
  BigLayoutMinScreenWidth  = 1024;
  BigLayoutMinScreenHeight = 700;

{ Usable screen width in pixels: the work area (taskbars/panels excluded) with
  fallbacks for widgetsets that report nothing useful. }
function ScreenUsableWidth: integer;
begin
  Result := Screen.WorkAreaWidth;
  if Result <= 0 then
    Result := Screen.Width;
  if Result <= 0 then
    Result := 640;
end;

{ Usable screen height in pixels; see ScreenUsableWidth. }
function ScreenUsableHeight: integer;
begin
  Result := Screen.WorkAreaHeight;
  if Result <= 0 then
    Result := Screen.Height;
  if Result <= 0 then
    Result := 480;
end;

{ Left edge of the usable area. Non-zero on a left-hand panel, and on a
  multi-monitor desktop where the work area starts on a secondary screen.
  Falls back to 0 whenever ScreenUsableWidth had to fall back too. }
function ScreenUsableLeft: integer;
begin
  if Screen.WorkAreaWidth > 0 then
    Result := Screen.WorkAreaLeft
  else
    Result := 0;
end;

{ Top edge of the usable area; see ScreenUsableLeft. }
function ScreenUsableTop: integer;
begin
  if Screen.WorkAreaHeight > 0 then
    Result := Screen.WorkAreaTop
  else
    Result := 0;
end;

{ Clamp a proposed dialog width to what the display can actually show. }
function FitDialogWidth(const AWidth: integer): integer;
begin
  Result := Min(AWidth, Round(ScreenUsableWidth * MaxDialogScreenWidthFraction));
end;

{**
  Determine whether dialogs should use the large layout.
  @param dialogsize Requested size mode.
  @returns @true if big layout should be used; @false otherwise.
  @remarks When @code(dialogsize = sdsAuto), it checks @code(TrndiNative.HasTouchScreen)
    and the usable screen size: big layout needs both touch and room for it, so
    small touch panels get @code(sdsMedium) rather than a dialog wider than the
    screen.
}
function GeTSlickeDialogSize(dialogsize: TSlickeDialogSize): TSlickeDialogSize;
begin
  case dialogsize of
  sdsNormal,
  sdsBig,
  sdsMedium:
    result := dialogsize;
  sdsAuto:
    if not TrndiNative.HasTouchScreen then
      result := sdsNormal
    else
    if (ScreenUsableWidth >= BigLayoutMinScreenWidth) and
      (ScreenUsableHeight >= BigLayoutMinScreenHeight) then
      result := sdsBig
    else
      result := sdsMedium;
  else
    result := GeTSlickeDialogSize(sdsAuto);
  end;
end;

{**
  Compute the wrapped text height for a label given its fixed width.
  @param ALabel Label with font and width already assigned.
  @returns Pixel height needed to display the caption.
}
function NormalizeLineBreaks(const S: string): string;
begin
  Result := StringReplace(S, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function CalcWrappedHeight(ALabel: TLabel): integer;
var
  bmp: Graphics.TBitmap;
  paragraphs, words: TStringList;
  para, token, currentLine: string;
  i, p, totalLines, lineCount: integer;
begin
  bmp := Graphics.TBitmap.Create;
  paragraphs := TStringList.Create;
  words := TStringList.Create;
  try
    // A 0x0 bitmap has no usable canvas on GTK3 (Gdk-CRITICAL, zero metrics),
    // which collapses every measured label; give it a surface first.
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(ALabel.Font);

    // Split by explicit line breaks first (each is a forced new line)
    paragraphs.Text := NormalizeLineBreaks(ALabel.Caption);
    // TStringList.Text splits on LF boundaries

    totalLines := 0;
    for p := 0 to paragraphs.Count - 1 do
    begin
      para := TrimRight(paragraphs[p]); // preserve empty lines meaningfully
      if para = '' then
      begin
        Inc(totalLines); // empty paragraph still consumes one empty line
        Continue;
      end;

      words.Clear;
      words.Delimiter := ' ';
      words.StrictDelimiter := true;
      words.DelimitedText := para;

      currentLine := '';
      lineCount := 1;
      for i := 0 to words.Count - 1 do
      begin
        token := words[i];
        if bmp.Canvas.TextWidth(Trim(currentLine + ' ' + token)) > ALabel.Width then
        begin
          Inc(lineCount);
          currentLine := token;
        end
        else
          currentLine := Trim(currentLine + ' ' + token);
      end;
      Inc(totalLines, lineCount);
    end;

    // If there were zero paragraphs, height is zero; ensure at least one line
    if (paragraphs.Count = 0) then
      totalLines := 1;

    Result := totalLines * bmp.Canvas.TextHeight('Hg');
    {$ifdef Darwin}
    if totalLines > 0 then
      Inc(Result, bmp.Canvas.TextHeight('Hg') div 2);
    {$endif}
  finally
    bmp.Free;
    paragraphs.Free;
    words.Free;
  end;
end;

{**
  Overloaded function from slicke.ux.native for our dialog form
}
function ShowModalSafe(aDialog: TDialogForm): integer;
var
  oldStyle: TFormStyle;
begin
  if not Assigned(aDialog) then
    Exit(mrNone);
  {$ifndef Windows}
  // Only apply the aggressive fallback on known-bad/lightweight WMs.
  if IsProblematicWM then
  begin
    oldStyle := aDialog.FormStyle;
    aDialog.FormStyle := fsStayOnTop;
    try
      Result := aDialog.ShowModal;
    finally
      try
        if Assigned(aDialog) then
          aDialog.FormStyle := oldStyle;
      except end;
    end;
    Exit;
  end;
  {$endif}
  Result := aDialog.ShowModal;
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
const DialogSize: TSlickeDialogSize;
const ATitle, ADesc: string;
IconBox: TImage;
out TitleLabel, DescLabel: TLabel
);
const
  Padding = 16;
  MinDialogWidth = 650;
var
  RightContentLeft, AvailableWidth: integer;
begin
  // Minimum width for big mode
  if (dialogsize = sdsBig) and (AOwner.ClientWidth < MinDialogWidth) then
    AOwner.ClientWidth := MinDialogWidth;

  RightContentLeft := IconBox.Left + IconBox.Width + Padding;
  AvailableWidth := AOwner.ClientWidth - RightContentLeft - Padding;

  // --- Title ---
  TitleLabel := TLabel.Create(AOwner);
  TitleLabel.Parent := AOwner;
  TitleLabel.WordWrap := true;
  TitleLabel.AutoSize := false;   // lock width for wrapping
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := RightContentLeft;
  TitleLabel.Width := AvailableWidth;
  case DialogSize of
  sdsBig:
    TitleLabel.Font.Size := 24;
  sdsMedium:
    TitleLabel.Font.Size := 20;
  end;
  TitleLabel.Top := Padding;
  TitleLabel.Caption := ATitle;
  TitleLabel.Font.Color := getBaseColor;
  TitleLabel.AdjustSize;  // now calculates proper height for given width

 // Description
  DescLabel := TLabel.Create(AOwner);
  DescLabel.Parent := AOwner;
  DescLabel.WordWrap := true;
  DescLabel.AutoSize := false;
  DescLabel.Font.Style := [];
  DescLabel.Left := TitleLabel.Left;
  DescLabel.Width := AvailableWidth;
  case DialogSize of
  sdsBig:
    DescLabel.Font.Size := 24;
  sdsMedium:
    DescLabel.Font.Size := 20;
  end;
  DescLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding;
  DescLabel.Caption := ADesc;
  DescLabel.Font.Color := getBaseColor;

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
function MeasureWrappedHeight(const AText: string; AFont: TFont; MaxWidth: integer): integer;
var
  bmp: Graphics.TBitmap;
  paragraphs, words: TStringList;
  para, token, currentLine: string;
  i, p, totalLines, lineCount: integer;
begin
  bmp := Graphics.TBitmap.Create;
  paragraphs := TStringList.Create;
  words := TStringList.Create;
  try
    // See CalcWrappedHeight: measuring on a 0x0 canvas yields zero metrics on GTK3.
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(AFont);

    paragraphs.Text := NormalizeLineBreaks(AText);

    totalLines := 0;
    for p := 0 to paragraphs.Count - 1 do
    begin
      para := TrimRight(paragraphs[p]);
      if para = '' then
      begin
        Inc(totalLines);
        Continue;
      end;

      words.Clear;
      words.Delimiter := ' ';
      words.StrictDelimiter := true;
      words.DelimitedText := para;

      currentLine := '';
      lineCount := 1;
      for i := 0 to words.Count - 1 do
      begin
        token := words[i];
        if bmp.Canvas.TextWidth(Trim(currentLine + ' ' + token)) > MaxWidth then
        begin
          Inc(lineCount);
          currentLine := token;
        end
        else
          currentLine := Trim(currentLine + ' ' + token);
      end;
      Inc(totalLines, lineCount);
    end;

    if (paragraphs.Count = 0) then
      totalLines := 1;

    Result := totalLines * bmp.Canvas.TextHeight('Hg');
    {$ifdef Darwin}
    // Cocoa adds fractional inter-line spacing that the bitmap canvas underestimates;
    // without this buffer the last line is clipped on macOS.
    if totalLines > 0 then
      Inc(Result, bmp.Canvas.TextHeight('Hg') div 2);
    {$endif}
  finally
    bmp.Free;
    paragraphs.Free;
    words.Free;
  end;
end;

{**
  Map UX button enum to Lazarus modal results.
  @param Btn UX button.
  @returns Corresponding @code(TModalResult).
}
function UXButtonToModalResult(Btn: TSlickeMsgDlgBtn): TModalResult;
begin
  case Btn of
  mbSlickeAgree,
  mbYes:
    Result := mrYes;
  mbNo:
    Result := mrNo;
  mbOK:
    Result := mrOk;
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
  mbClose:
    Result := mrClose;
  else
    // fallback / custom button
    Result := TModalResult(110);
  end;
end;

{$ifdef X_WIN}
{**
  Convert a Lazarus @code(TColor) to a Direct2D color with alpha.
  @param Col Lazarus color.
  @param Alpha Alpha in [0..1].
  @returns Direct2D color struct.
}
function TColorToColorF(const Col: TColor; Alpha: single = 1.0): TD2D1_COLOR_F;
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
  BG, FG: TD2D1_COLOR_F;
  R: TRect;
  Inset: single;
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
        pwidechar('Segoe UI Emoji'), nil,
        DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL,
        DWRITE_FONT_STRETCH_NORMAL,
        Image.Height - Trunc(Inset * 2),
        'en-us', TextFormat
        );

      // Brush for text rendering. Colour glyphs carry their own layers and
      // ignore it, but a codepoint Segoe UI Emoji lacks falls back to a
      // monochrome face -- drawing that in the background colour hides it.
      BG := TColorToColorF(bgcol, 1.0);
      FG := TColorToColorF(getBaseColor, 1.0);
      RT.CreateSolidColorBrush(@FG, nil, Brush);

      // Drawing area with inset
      TextRect := RectF(Inset, Inset, Image.Width - Inset, Image.Height - Inset);

      // Draw
      RT.BeginDraw;
      RT.Clear(BG);
      RT.DrawText(pwidechar(Emoji), Length(Emoji), TextFormat,
        @TextRect, Brush,
        D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT,
        DWRITE_MEASURING_MODE_NATURAL);
      RT.EndDraw;

      // Assign to TImage
      Image.Picture.Assign(Bitmap);
      Image.Transparent := true;
    finally
      Bitmap.Free;
    end;
  finally
    CoUninitialize;
  end;
end;

{$else}

{$ifdef X_MAC}
type
  // SF Symbols landed in macOS 11, long after FPC 3.2.2's Cocoa headers were
  // generated, so the class selector is declared by hand here. MacDrawSymbol
  // gates the call on the OS version before it is ever sent.
NSImageSFSymbols = objccategory external (NSImage)
  class function imageWithSystemSymbolName_accessibilityDescription(
    symbolName: NSString; descr: NSString): NSImage;
    message 'imageWithSystemSymbolName:accessibilityDescription:';
end;

// Map a dialog icon codepoint onto the closest SF Symbol and the system colour
// macOS draws it in. An empty name means no good match, which sends the caller
// back to the emoji renderer.
function MacSymbolForIcon(const Emoji: widestring; out tint: NSColor): string;

  function C(r, g, b: double): NSColor;
  begin
    Result := NSColor.colorWithCalibratedRed_green_blue_alpha(r, g, b, 1.0);
  end;

begin
  Result := '';
  tint := nil;
  if Emoji = '' then
    Exit;
  case Word(Emoji[1]) of
  $2705:                        // systemGreen
  begin
    Result := 'checkmark.circle.fill';
    tint := C(0.20, 0.78, 0.35);
  end;
  $26A0:                        // systemOrange
  begin
    Result := 'exclamationmark.triangle.fill';
    tint := C(1.00, 0.58, 0.00);
  end;
  $274C:                        // systemRed
  begin
    Result := 'xmark.circle.fill';
    tint := C(1.00, 0.23, 0.19);
  end;
  $2139:                        // systemBlue
  begin
    Result := 'info.circle.fill';
    tint := C(0.00, 0.48, 1.00);
  end;
  $2753:                        // systemBlue
  begin
    Result := 'questionmark.circle.fill';
    tint := C(0.00, 0.48, 1.00);
  end;
  $2699:                        // systemGray
  begin
    Result := 'gearshape.fill';
    tint := C(0.56, 0.56, 0.58);
  end;
  $2B1C:                        // systemGray
  begin
    Result := 'square';
    tint := C(0.56, 0.56, 0.58);
  end;
  end;
end;

// Draw the icon as an SF Symbol at W x H device pixels. Vector source, so it is
// resolution-independent, and it gives the icons the shape and colour macOS
// itself uses rather than the emoji font's boxed glyphs. Returns false whenever
// anything is unavailable so the caller falls back to the emoji renderer.
function MacDrawSymbol(bmp: Graphics.TBitmap; const Emoji: widestring;
W, H: integer; bgcol: TColor): boolean;
var
  symName: string;
  tint: NSColor;
  img: NSImage;
  rep: NSBitmapImageRep;
  prevCtx, ctx: NSGraphicsContext;
  full: NSRect;
  inset: CGFloat;
  lazimg: TLazIntfImage;
  raw, p: PByte;
  x, y, rowBytes: integer;
  br, bg_, bb, r8, g8, b8: byte;
  osv: NSOperatingSystemVersion;
begin
  Result := false;
  if (W <= 0) or (H <= 0) then
    Exit;
  osv := NSProcessInfo.processInfo.operatingSystemVersion;
  if osv.majorVersion < 11 then
    Exit;

  symName := MacSymbolForIcon(Emoji, tint);
  if (symName = '') or (tint = nil) then
    Exit;

  img := NSImage.imageWithSystemSymbolName_accessibilityDescription(
    NSString.stringWithUTF8String(PChar(symName)), nil);
  if img = nil then
    Exit;

  rep := NSBitmapImageRep(NSBitmapImageRep.alloc).
    initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel(
    nil, W, H, 8, 4, true, false, NSDeviceRGBColorSpace, W * 4, 32);
  if rep = nil then
    Exit;
  try
    ctx := NSGraphicsContext.graphicsContextWithBitmapImageRep(rep);
    if ctx = nil then
      Exit;

    full := NSMakeRect(0, 0, W, H);
    inset := W * 0.08;          // keep the glyph off the very edge

    prevCtx := NSGraphicsContext.currentContext;
    NSGraphicsContext.setCurrentContext(ctx);
    try
      // Symbol first on a clear rep, then tint only what it covers via
      // sourceAtop, then slide the background in underneath with
      // destinationOver -- tinting after the fill would recolour the fill too.
      img.drawInRect_fromRect_operation_fraction(
        NSMakeRect(inset, inset, W - inset * 2, H - inset * 2),
        NSZeroRect, NSCompositeSourceOver, 1.0);
      tint.set_;
      NSRectFillUsingOperation(full, NSCompositeSourceAtop);
      RedGreenBlue(ColorToRGB(bgcol), br, bg_, bb);
      NSColor.colorWithCalibratedRed_green_blue_alpha(
        br / 255, bg_ / 255, bb / 255, 1.0).set_;
      NSRectFillUsingOperation(full, NSCompositeDestinationOver);
    finally
      NSGraphicsContext.setCurrentContext(prevCtx);
    end;

    raw := PByte(rep.bitmapData);
    if raw = nil then
      Exit;
    rowBytes := rep.bytesPerRow;

    lazimg := TLazIntfImage.Create(W, H, [riqfRGB]);
    try
      for y := 0 to H - 1 do
      begin
        p := raw + (y * rowBytes);
        for x := 0 to W - 1 do
        begin
          // RGBA8888; the destinationOver fill leaves every pixel opaque, so
          // premultiplication is the identity here and the bytes are direct.
          r8 := p[0];
          g8 := p[1];
          b8 := p[2];
          lazimg.Colors[x, y] := FPColor((r8 shl 8) or r8, (g8 shl 8) or g8,
            (b8 shl 8) or b8, alphaOpaque);
          Inc(p, 4);
        end;
      end;
      bmp.LoadFromIntfImage(lazimg);
      Result := true;
    finally
      lazimg.Free;
    end;
  finally
    rep.release;
  end;
end;
{$endif}

{**
  Render an emoji into a @code(TImage) using the standard canvas (non-Windows).
  Uses SF Symbols on macOS 11+, otherwise "Apple Color Emoji" on macOS and
  "Noto Color Emoji" elsewhere.
  @param Image Target image control.
  @param Emoji Emoji text (usually a single codepoint).
  @param bgcol Background color.
}
procedure AssignEmoji(Image: TImage; const Emoji: widestring; bgcol: TColor = clWhite);
var
  Inset, W, H: integer;
  scale: double;
  bmp: Graphics.TBitmap;
begin
  // Render at the display's backing scale factor. The canvas draws in logical
  // points, so a 1:1 bitmap is upscaled by the compositor and comes out soft on
  // Retina; an oversized bitmap stretched back into the control's logical
  // bounds lands on device pixels instead.
  scale := 1;
  {$ifdef X_MAC}
  if NSScreen.mainScreen <> nil then
    scale := NSScreen.mainScreen.backingScaleFactor;
  if scale < 1 then
    scale := 1;
  {$endif}

  W := Round(Image.Width * scale);
  H := Round(Image.Height * scale);

  bmp := Image.Picture.Bitmap;
  bmp.SetSize(W, H);
  Image.Stretch := true;   // no-op when scale = 1, since bitmap = control size
  Image.Transparent := true;

  {$ifdef X_MAC}
  // Native symbols where macOS offers one; the emoji font below is the fallback.
  if MacDrawSymbol(bmp, Emoji, W, H, bgcol) then
    Exit;
  {$endif}

  bmp.Canvas.Brush.Color := bgcol;
  bmp.Canvas.FillRect(0, 0, W, H);

  Inset := Round(W * 0.15); // 15% padding around the emoji

  {$ifdef Darwin}
  bmp.Canvas.Font.Name := 'Apple Color Emoji';
  {$else}
  bmp.Canvas.Font.Name := 'Noto Color Emoji';
  {$endif}
  bmp.Canvas.Font.Size := H - (Inset * 2);
  // Colour glyphs ignore this; it only reaches codepoints the emoji font lacks
  // and that fall back to a monochrome face, which must not be hardcoded black
  // on a dark dialog background.
  bmp.Canvas.Font.Color := getBaseColor;

  bmp.Canvas.TextOut(
    (W - bmp.Canvas.TextWidth(Emoji)) div 2,
    (H - bmp.Canvas.TextHeight(Emoji)) div 2,
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
  @param TitleLabel Pre-created title label (parent/layout set here).
  @param DescLabel Pre-created description label (parent/layout set here).
  @param MinWidthNormal Minimum width in normal mode.
  @param MinWidthBig Minimum width in big mode.
  @param IconSize Base icon size (doubled in big mode).
  @param Padding Inner spacing between controls.
}
procedure SetupDialogTitleDesc(
Dialog: TForm;
const size: TSlickeDialogSize;
const icon: SlickeUXImage;
const bgcol: TColor;
const ATitle, ADesc: string;
IconBox: TImage;
TitleLabel, DescLabel: TLabel;
MinWidthNormal: integer = 650;
MinWidthBig: integer = 800;
IconSize: integer = 48;
Padding: integer = 16
);
var
  availableWidth: integer;
  currentIconSize: integer;
begin
  // --- Ensure minimum dialog width, never wider than the display ---
  Dialog.ClientWidth := FitDialogWidth(MinWidthNormal);
  if (size = sdsBig) and (Dialog.ClientWidth < MinWidthBig) then
    Dialog.ClientWidth := FitDialogWidth(MinWidthBig);
  Dialog.Color := bgcol;

  // --- Icon size scaling ---
  currentIconSize := IconSize;
  if (size = sdsBig) then
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
  {$IFDEF DARWIN}
  // Drop the icon row below the blended macOS title bar so it doesn't sit
  // flush against the empty titlebar strip.
  IconBox.Top := IconBox.Top + Padding;
  {$ENDIF}
  {$ENDIF}
  AssignEmoji(IconBox, Icon, bgcol);

  // --- Title label ---
  TitleLabel.Parent := Dialog;
  TitleLabel.WordWrap := true;
  TitleLabel.AutoSize := false;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := IconBox.Left + IconBox.Width + Padding;
  availableWidth := Dialog.ClientWidth - TitleLabel.Left - Padding;
  TitleLabel.Width := availableWidth;
  if (size = sdsBig) then
    TitleLabel.Font.Size := 26;
  if (size = sdsMedium) then
    TitleLabel.Font.Size := 22;
  TitleLabel.Top := IconBox.Top; // aligns with icon top
  TitleLabel.Caption := ATitle;
  TitleLabel.Font.Color := getBaseColor;
  TitleLabel.Height := CalcWrappedHeight(TitleLabel);

  // --- Description label ---
  DescLabel.Parent := Dialog;
  DescLabel.WordWrap := true;
  DescLabel.AutoSize := false;
  DescLabel.Font.Style := [];
  DescLabel.Left := TitleLabel.Left;
  DescLabel.Width := availableWidth;
  case size of
  sdsBig:
    DescLabel.Font.Size := 24;
  sdsMedium:
    DescLabel.Font.Size := 20;
  end;
  DescLabel.Top := TitleLabel.Top + TitleLabel.Height + Padding;
  DescLabel.Caption := ADesc;
  DescLabel.Font.Color := getBaseColor;
  DescLabel.Height := CalcWrappedHeight(DescLabel);
end;

{ Create a platform-appropriate dialog button (TDarkButton on Windows, TButton elsewhere),
  apply big-mode scaling, and optionally register it in the dialog's button list. }
function MakeDialogButton(Dialog: TDialogForm; const size: TSlickeDialogSize;
  const ACaption: string; AModalResult: TModalResult;
  AddToButtons: boolean = true): TWinControl;
var
  {$ifdef X_WIN}
  Btn: TDarkButton;
  {$else}
  Btn: TButton;
  {$endif}
begin
  {$ifdef X_WIN}Btn := TDarkButton.Create(Dialog);{$else}Btn := TButton.Create(Dialog);{$endif}
  Btn.Parent := Dialog;
  {$ifdef LCLGTK2}Btn.Font.Color := clBlack;{$endif}
  Btn.Caption := ACaption;
  Btn.ModalResult := AModalResult;
  Btn.Width := 80;
  if size = sdsBig then
  begin
    Btn.Width := Btn.Width * 2;
    Btn.Height := Btn.Height * 2;
    Btn.Font.Size := 12;
  end;
  if AddToButtons then
    Dialog.addButton(ACaption);
  Result := Btn;
end;

{ Position two buttons centered below a control and set the dialog's client height.
  Every caller passes (accept, reject); on macOS/GNOME that pair is swapped so the
  input dialogs follow the same convention as the message dialogs. }
procedure CenterButtons(Dialog: TDialogForm; Btn1, Btn2: TWinControl;
  AboveBottom: integer; const size: TSlickeDialogSize; Padding: integer);
var
  total: integer;
  first, second: TWinControl;
begin
  if SlickeUseReversedButtons then
  begin
    first  := Btn2;
    second := Btn1;
  end
  else
  begin
    first  := Btn1;
    second := Btn2;
  end;

  first.Top := AboveBottom + ifthen(size = sdsBig, Padding * 3, Padding * 2);
  second.Top := first.Top;
  total := first.Width + Padding + second.Width;
  first.Left := (Dialog.ClientWidth - total) div 2;
  second.Left := first.Left + first.Width + Padding;
  Dialog.ClientHeight := first.Top + first.Height + Padding;
end;

{** See interface docs for behavior and parameters. }
function SlickeIntInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: integer;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): integer;
begin
  result := round(SlickeNumericInput(dialogsize,ACaption,ATitle,ADesc,ADefault, FLOAT_NONE, FLOAT_NONE, false, ModalResult, icon));
end;

{** See interface docs for behavior and parameters. }
function SlickeNumericInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: double;
AMin, AMax: double;
float: boolean;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): double;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Edit: TFloatSpinEditEx;
  OkButton, CancelButton: TWinControl;
  bgcol: TColor;
  size: TSlickeDialogSize;
begin
  size := GeTSlickeDialogSize(dialogsize);
  Result := ADefault;
  ModalResult := mrCancel;
  bgcol := getBackground;

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := true;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    // Use shared helper for title + description
    SetupDialogTitleDesc(Dialog, size, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Numeric input ---
    Edit := TFloatSpinEditEx.Create(Dialog);
    Edit.Parent := Dialog;
    Edit.Left := DescLabel.Left;
    Edit.Width := DescLabel.Width;
    Edit.Top := DescLabel.Top + DescLabel.Height + ifthen(size = sdsBig, Padding * 2, Padding);
    Edit.Value := ADefault;
    if AMin <> FLOAT_NONE then
      Edit.minvalue := AMin;
    if AMax <> FLOAT_NONE then
      Edit.maxvalue := AMax;
    {$ifdef X_WIN}
    if TrndiNative.isDarkMode then
    begin
      Edit.Color := RGBToColor(53, 53, 53);
      Edit.Font.Color := RGBToColor(245, 245, 245);
    end;
    {$else}
    {$ifdef X_MAC}
    Edit.Color := MacInputBackgroundColor(Edit.Color);
    Edit.Font.Color := MacInputTextColor(Edit.Font.Color);
    {$endif}
    {$endif}
    if float then
    begin
      Edit.DecimalPlaces := 2;
      Edit.Increment := 0.01;
    end
    else
      Edit.DecimalPlaces := 0;
    if (size = sdsBig) then
      Edit.Font.Size := 20;

    OkButton     := MakeDialogButton(Dialog, size, smbSelect,   mrOk,     false);
    CancelButton := MakeDialogButton(Dialog, size, smbUXCancel, mrCancel);
    CenterButtons(Dialog, OkButton, CancelButton, Edit.Top + Edit.Height, size, Padding);
    Dialog.ActiveControl := Edit;

    ModalResult := ShowModalSafe(Dialog);
    if ModalResult = mrOk then
      Result := Edit.Value;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
buttons: TSlickeMsgDlgBtns;
const icon: SlickeUXImage = uxmtOK;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeMsg(dialogsize, sMsgTitle, title, message, '',
    uxclBlue, uxclLightBlue, buttons, WChar(icon), 1, ADefault);
end;

{** See interface docs for behavior and parameters. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const title, message: string;
buttons: TSlickeMsgDlgBtns;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeDialog(dialogsize, sMsgTitle, title, message, buttons, mtype, ADefault);
end;

{** See interface docs for behavior and parameters. }
function SlickeDialog(const dialogsize: TSlickeDialogSize;
const header, title, message: string;
buttons: TSlickeMsgDlgBtns;
const mtype: TMsgDlgType;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
var
  icon: SlickeUXImage;
begin
  case mtype of
  mtWarning:
    icon := uxmtWarning; // ⚠️ Warning sign
  mtError:
    icon := uxmtError; // ❌ Cross mark
  mtInformation:
    icon := uxmtInformation; // ℹ️ Info symbol
  mtConfirmation:
    icon := uxmtConfirmation; // ❓ Question mark
  mtCustom:
    icon := uxmtCog; // ⚙️ Gear
  else
    icon := uxmtCog;
  end;

  Result := SlickeMsg(dialogsize, header, title, message, '',
    uxclBlue, uxclLightBlue, buttons, icon, 1, ADefault);
end;


procedure SlickeMessage(const title, message: string; const icon: SlickeUXImage = uxmtOK; sender: TForm = nil);
begin
  SlickeMessage(sdsAuto, title, message, icon, sender);
end;

{**
  See interface docs. Renders inline panel when @code(dialogsize = sdsOnForm) and a sender is available.
}
procedure SlickeMessage(const dialogsize: TSlickeDialogSize; const title, message: string;
const
icon: SlickeUXImage = uxmtOK;
sender: TForm = nil);
const
  onFormName: string = 'uxd_on_form';
  Margin = 5;
  Gap = 10;
  { Height the scrollable message area keeps even when the title wants it all. }
  MinMessageHeight = 40;
var
  tp: TPanel;
  tl, tt: TLabel;
  ts: TScrollBox;
  {$ifdef X_WIN}
  tb: TButton; // TDarkButton;
  {$else}
  tb: TButton;
  {$endif}
  df: TDialogForm;
begin
  if (dialogsize = sdsOnForm) and ((sender <> nil) and (sender.FindComponent(onFormName) = nil)) then
  begin

    // Gate on touch, not on the resolved layout size: small touch panels now
    // resolve to sdsMedium, and the full-screen overlay is wanted most exactly
    // there. This is the original meaning of the test.
    if (sender <> nil) and (sender.Showing) and TrndiNative.HasTouchScreen then
    begin
      // On e.g. touch screens display a full screen message. Child coordinates
      // are in the parent's client space, and all four sides are anchored so the
      // overlay keeps covering the form when it is resized or the screen rotates.
      tp := TPanel.Create(sender); // Create a panel to cover the screen
      tp.Name := onFormName;
      tp.caption := '';
      tp.Parent := sender;
      tp.Top := 0;
      tp.Left := 0;
      tp.Width := sender.ClientWidth;
      tp.Height := sender.ClientHeight;
      tp.Anchors := [akLeft, akTop, akRight, akBottom];
      tp.BringToFront;
      tp.Color := uxclLightBlue;

      // --- Title: measured, not guessed, so the body never lands on top of it ---
      tt := TLabel.Create(tp);
      tt.parent := tp;
      tt.autosize := false;
      tt.WordWrap := true;
      tt.Font.Color := uxclBlue;
      tt.Font.Style := [fsBold];
      tt.Font.Size := tp.Width div 20;
      tt.left := Margin;
      tt.top := Margin;
      tt.width := tp.Width - (Margin * 2);
      tt.Anchors := [akLeft, akTop, akRight];
      tt.Caption := title;
      tt.Height := MeasureWrappedHeight(title, tt.Font, tt.Width);

      // Button created first so we know its final Top before sizing the message
      // area below.
      {$ifdef X_WIN}tb := TButton.Create(tp);{$else}tb := TButton.Create(tp);{$endif}
      tb.Parent := tp;
      tb.AutoSize := true;
      tb.Caption := smbUXOK;

      if tb.Height < (tp.Height div 5) then
      begin
        tb.AutoSize := false;
        tb.Height := tp.Height div 5;
      end;

      tb.Left := 0;
      tb.Width := tp.Width;
      tb.Top := tp.Height - tb.Height - Gap;
      tb.Anchors := [akLeft, akRight, akBottom];
      tb.Font.Color := sender.Font.Color;

      // --- Message: scrollable, so a long text is reachable instead of clipped ---
      ts := TScrollBox.Create(tp);
      ts.Parent := tp;
      ts.BorderStyle := bsNone;
      ts.ParentColor := false;
      ts.Color := uxclLightBlue;
      ts.Left := Margin;
      ts.Top := tt.Top + tt.Height + Margin;
      ts.Width := tp.Width - (Margin * 2);
      // A title long enough to wrap past the button leaves no room at all, which
      // would give a negative height (an LCL range error, or a zero-size widget
      // that hides the message). Give the message area its minimum and let the
      // title be the part that gets cut instead.
      if (tb.Top - ts.Top - Margin) < MinMessageHeight then
      begin
        ts.Top := Max(tt.Top, tb.Top - Margin - MinMessageHeight);
        tt.Height := Max(0, ts.Top - tt.Top - Margin);
      end;
      ts.Height := Max(0, tb.Top - ts.Top - Margin);
      ts.Anchors := [akLeft, akTop, akRight, akBottom];
      ts.HorzScrollBar.Visible := false;
      ts.VertScrollBar.Visible := true;
      // Realise the handle so ClientWidth below excludes the vertical scroll
      // bar; without it the label is measured too wide and clips on the right.
      ts.HandleNeeded;

      tl := TLabel.Create(ts);
      tl.parent := ts;
      tl.autosize := false;
      tl.Font.Color := uxclBlue;
      tl.Font.Size := tp.Width div 20;
      if IsProblematicWM then
        tl.Font.size := 38;
      tl.WordWrap := true;
      tl.top := 0;
      tl.left := 0;
      tl.width := ts.ClientWidth;
      tl.Anchors := [akLeft, akTop, akRight];
      tl.Caption := message;
      // Font is final before measuring; the scroll box supplies whatever height
      // the wrapped text needs beyond the visible area.
      tl.Height := MeasureWrappedHeight(message, tl.Font, tl.Width);

      // BringToFront is essential: in Qt the last-created sibling has the
      // highest z-order, so without it the message area (created after) sits on
      // top and its widget intercepts touch events over the button area.
      tb.BringToFront;

      // Owned by the overlay panel, not by sender: releasing the panel disposes
      // of the handler host too, so repeated messages don't accumulate hidden
      // forms on the main window for the lifetime of the app.
      df := TDialogForm.CreateNew(tp);
      tb.OnClick := @df.SlickeMessageOnClick;
      tb.OnMouseDown := @df.SlickeMessageOnMouseDown;
    end
    else
      SlickeMsg(sdsAuto, sMsgTitle, title, message, '',
        uxclBlue, uxclLightBlue, [mbOK], WChar(icon))
  end
  else
    SlickeMsg(dialogsize, sMsgTitle, title, message, '',
      uxclBlue, uxclLightBlue, [mbOK], WChar(icon))
end;

{** See interface docs for behavior and parameters. }
function SlickeInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc, ADefault: string;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog;
const AMasked: boolean = false
): string;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Edit: TEdit;
  OkButton, CancelButton: TWinControl;
  bgcol: TColor;
  size: TSlickeDialogSize;
begin
  Result := ADefault;
  ModalResult := mrCancel;
  size := GeTSlickeDialogSize(dialogsize);
  bgcol := getBackground;

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := true;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    // Use shared helper for consistent title/description layout
    SetupDialogTitleDesc(Dialog, size, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Input field ---
    Edit := TEdit.Create(Dialog);
    Edit.Parent := Dialog;
    Edit.Left := DescLabel.Left;
    Edit.Width := DescLabel.Width;
    Edit.Top := DescLabel.Top + DescLabel.Height + ifthen((size = sdsBig), Padding * 2, Padding);
    Edit.Text := ADefault;
    if AMasked then
      Edit.EchoMode := emPassword;
    {$ifdef X_WIN}
    if TrndiNative.isDarkMode then
    begin
      Edit.Color := RGBToColor(53, 53, 53);
      Edit.Font.Color := RGBToColor(245, 245, 245);
    end;
    {$else}
    {$ifdef X_MAC}
    Edit.Color := MacInputBackgroundColor(Edit.Color);
    Edit.Font.Color := MacInputTextColor(Edit.Font.Color);
    {$endif}
    {$endif}
    if (size = sdsBig) then
      Edit.Font.Size := 20;

    OkButton     := MakeDialogButton(Dialog, size, smbSelect,   mrOk);
    CancelButton := MakeDialogButton(Dialog, size, smbUXCancel, mrCancel);
    CenterButtons(Dialog, OkButton, CancelButton, Edit.Top + Edit.Height, size, Padding);
    Dialog.ActiveControl := Edit;

    ModalResult := ShowModalSafe(Dialog);
    if ModalResult = mrOk then
      Result := Edit.Text;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function SlickePasswordInput(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc, ADefault: string;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): string;
begin
  Result := SlickeInput(dialogsize, ACaption, ATitle, ADesc, ADefault, ModalResult, icon, true);
end;

function SlickeList(const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
const Choices: array of string;
const Default: boolean = false;
const icon: SlickeUXImage = uxmtCog): integer; overload;
var
  UChoices: array of unicodestring;
  i: integer;
begin
  SetLength(UChoices, Length(Choices));
  for i := 0 to High(Choices) do
    UChoices[i] := unicodestring(Choices[i]);

  Result := SlickeList(dialogsize, ACaption, ATitle, ADesc, UChoices, Default, icon);
end;

{** See interface docs for behavior and parameters. }
function SlickeList(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
const Choices: array of unicodestring;
const Default: boolean = false;
const icon: SlickeUXImage = uxmtCog
): integer;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Combo: TComboBox;
  OkButton, CancelButton: TWinControl;
  bgcol: TColor;
  i: integer;
  size: TSlickeDialogSize;
begin
  Result := -1;
  size := GeTSlickeDialogSize(dialogsize);
  bgcol := getBackground;

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := true;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    SetupDialogTitleDesc(Dialog, size, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- ComboBox ---
    Combo := TComboBox.Create(Dialog);
    Combo.Parent := Dialog;
    for i := 0 to High(Choices) do
      Combo.Items.Add(UTF8Encode(Choices[i]));
    Combo.ReadOnly := true;
    Combo.Style := csDropDownList;
    Combo.Left := DescLabel.Left;
    Combo.Width := DescLabel.Width;
    {$ifdef X_WIN}
    if TrndiNative.isDarkMode then
    begin
      Combo.Color := RGBToColor(53, 53, 53);
      Combo.Font.Color := RGBToColor(245, 245, 245);
    end;
    {$else}
    {$ifdef X_MAC}
    Combo.Color := MacInputBackgroundColor(Combo.Color);
    Combo.Font.Color := MacInputTextColor(Combo.Font.Color);
    {$endif}
    {$endif}
    if (size = sdsBig) then
      Combo.Font.Size := 20;
    Combo.Top := DescLabel.Top + DescLabel.Height + ifthen((size = sdsBig) , Padding * 2, Padding);
    Combo.ItemIndex := 0;

    OkButton     := MakeDialogButton(Dialog, size, smbSelect, mrOk);
    CancelButton := MakeDialogButton(Dialog, size,
      ifthen(Default, smbSlickeDefault, smbUXCancel), mrCancel);
    CenterButtons(Dialog, OkButton, CancelButton, Combo.Top + Combo.Height, size, Padding);
    Dialog.ActiveControl := Combo;

    if ShowModalSafe(Dialog) = mrOk then
      Result := Combo.ItemIndex
    else
      Result := -1;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function SlickeTable(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
const Keys, Values: array of string;
const icon: SlickeUXImage = uxmtCog;
const key: string = '';
const value: string = ''
): integer;
const
  Padding = 16;
  GridHeight = 200;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  Grid: TStringGrid;
  BgCol: TColor;
  OkButton, CancelButton: TWinControl;
  i: integer;
  size: TSlickeDialogSize;
begin
  Result := -1;
  size := GeTSlickeDialogSize(dialogsize);
  BgCol := getBackground;

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := true;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    SetupDialogTitleDesc(Dialog, size, icon, BgCol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Grid ---
    Grid := TStringGrid.Create(Dialog);
    Grid.Parent := Dialog;
    Grid.Left := DescLabel.Left;
    Grid.Width := DescLabel.Width;
    Grid.Top := DescLabel.Top + DescLabel.Height + Padding;
    Grid.Height := ifthen((size = sdsBig) , GridHeight + 80, GridHeight);
    Grid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing];
    {$ifdef X_WIN}
    if TrndiNative.isDarkMode then
    begin
      Grid.Color := RGBToColor(53, 53, 53);
      Grid.FixedColor := RGBToColor(35, 35, 35);
      Grid.Font.Color := RGBToColor(245, 245, 245);
    end;
    {$else}
    {$ifdef X_MAC}
    Grid.Color := MacInputBackgroundColor(Grid.Color);
    Grid.FixedColor := MacDialogBackgroundColor(Grid.FixedColor);
    Grid.Font.Color := MacInputTextColor(Grid.Font.Color);
    {$endif}
    {$endif}
    Grid.ColCount := 2;
    Grid.RowCount := Length(Keys) + 1;
    Grid.ColWidths[0] := 120;
    Grid.ColWidths[1] := 120;

    Grid.Cells[0, 0] := IfThen(key = '', sKEY, key);
    Grid.Cells[1, 0] := IfThen(value = '', sVALUE, value);
    for i := 0 to High(Keys) do
    begin
      Grid.Cells[0, i + 1] := Keys[i];
      Grid.Cells[1, i + 1] := Values[i];
    end;

    if (size = sdsBig) then
      Grid.Font.Size := 14;

    OkButton     := MakeDialogButton(Dialog, size, smbSelect,   mrOk);
    CancelButton := MakeDialogButton(Dialog, size, smbUXCancel, mrCancel);
    CenterButtons(Dialog, OkButton, CancelButton, Grid.Top + Grid.Height, size, Padding);

    if ShowModalSafe(Dialog) = mrOk then
      Result := Grid.Row;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function SlickeFontPicker(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefaultFont: TFont;
const AFontSample: string;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): TFont;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  PreviewLabel: TLabel;
  FontCombo: TComboBox;
  OkButton, CancelButton: TWinControl;
  bgcol: TColor;
  size: TSlickeDialogSize;
  SelectedFont: TFont;
  i, initialIndex: integer;

begin
  Result := TFont.Create;
  Result.Assign(ADefaultFont);
  ModalResult := mrCancel;
  size := GeTSlickeDialogSize(dialogsize);
  bgcol := getBackground;

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := true;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  SelectedFont := TFont.Create;
  SelectedFont.Assign(ADefaultFont);
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    // Use shared helper for consistent title/description layout
    SetupDialogTitleDesc(Dialog, size, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Font ComboBox ---
    FontCombo := TComboBox.Create(Dialog);
    FontCombo.Parent := Dialog;
    FontCombo.Left := DescLabel.Left;
    FontCombo.Width := DescLabel.Width;
    FontCombo.Top := DescLabel.Top + DescLabel.Height + ifthen((size = sdsBig) , Padding * 2, Padding);
    FontCombo.Style := csDropDownList;
    FontCombo.Sorted := true;
    {$ifdef X_WIN}
    if TrndiNative.isDarkMode then
    begin
      FontCombo.Color := RGBToColor(53, 53, 53);
      FontCombo.Font.Color := RGBToColor(245, 245, 245);
    end;
    {$else}
    {$ifdef X_MAC}
    FontCombo.Color := MacInputBackgroundColor(FontCombo.Color);
    FontCombo.Font.Color := MacInputTextColor(FontCombo.Font.Color);
    {$endif}
    {$endif}
    
    // Populate with system fonts
    FontCombo.Items.Assign(Screen.Fonts);
    
    // Find and select the default font
    initialIndex := FontCombo.Items.IndexOf(ADefaultFont.Name);
    if initialIndex >= 0 then
      FontCombo.ItemIndex := initialIndex
    else
    if FontCombo.Items.Count > 0 then
      FontCombo.ItemIndex := 0;
    
    if (size = sdsBig) then
      FontCombo.Font.Size := 16;

    // --- Preview Label ---
    PreviewLabel := TLabel.Create(Dialog);
    PreviewLabel.Parent := Dialog;
    PreviewLabel.Left := DescLabel.Left;
    PreviewLabel.Width := DescLabel.Width;
    PreviewLabel.Top := FontCombo.Top + FontCombo.Height + ifthen((size = sdsBig) , Padding * 2, Padding);
    PreviewLabel.Caption := AFontSample;
    PreviewLabel.AutoSize := false;
    PreviewLabel.Alignment := taCenter;
    PreviewLabel.Font.Assign(SelectedFont);
    PreviewLabel.Font.Color := getBaseColor;

    case size of
    sdsBig:
    begin
      PreviewLabel.Height := 80;
      PreviewLabel.Font.Size := 24;
    end;
    sdsMedium:
    begin
      PreviewLabel.Height := 65;
      PreviewLabel.Font.Size := 20;
    end
    else
    begin
      PreviewLabel.Height := 50;
      PreviewLabel.Font.Size := 16;
    end;

    end;


    // Set up live preview update
    Dialog.FontPickerPreview := PreviewLabel;
    FontCombo.OnChange := @Dialog.FontComboChange;

    OkButton     := MakeDialogButton(Dialog, size, smbUXOK,     mrOk);
    CancelButton := MakeDialogButton(Dialog, size, smbUXCancel, mrCancel);
    CenterButtons(Dialog, OkButton, CancelButton,
      PreviewLabel.Top + PreviewLabel.Height, size, Padding);

    ModalResult := ShowModalSafe(Dialog);
    if ModalResult = mrOk then
    begin
      // Get selected font name from combo box
      if FontCombo.ItemIndex >= 0 then
        SelectedFont.Name := FontCombo.Items[FontCombo.ItemIndex];
      Result.Assign(SelectedFont);
    end;
  finally
    SelectedFont.Free;
    Dialog.Free;
  end;
end;

function SlickeDatePicker(
const dialogsize: TSlickeDialogSize;
const ACaption, ATitle, ADesc: string;
ADefault: TDateTime;
AMinDate: TDateTime;
AMaxDate: TDateTime;
var ModalResult: TModalResult;
const icon: SlickeUXImage = uxmtCog
): TDateTime;
const
  Padding = 16;
var
  Dialog: TDialogForm;
  IconBox: TImage;
  TitleLabel, DescLabel: TLabel;
  DatePicker: TDateEdit;
  OkButton, CancelButton: TWinControl;
  bgcol: TColor;
  size: TSlickeDialogSize;
begin
  size := GeTSlickeDialogSize(dialogsize);
  Result := ADefault;
  ModalResult := mrCancel;
  bgcol := getBackground;

  Dialog := TDialogForm.CreateNew(nil);
  Dialog.KeyPreview := true;
  Dialog.OnKeyDown := @Dialog.FormKeyDown;
  try
    Dialog.Caption := ACaption;
    Dialog.BorderStyle := bsDialog;
    Dialog.Position := poScreenCenter;

    IconBox := TImage.Create(Dialog);
    TitleLabel := TLabel.Create(Dialog);
    DescLabel := TLabel.Create(Dialog);

    // Use shared helper for title + description
    SetupDialogTitleDesc(Dialog, size, icon, bgcol, ATitle, ADesc, IconBox, TitleLabel, DescLabel);

    // --- Date picker ---
    DatePicker := TDateEdit.Create(Dialog);
    DatePicker.Parent := Dialog;
    DatePicker.Left := DescLabel.Left;
    DatePicker.Width := DescLabel.Width;
    DatePicker.Top := DescLabel.Top + DescLabel.Height + ifthen(size = sdsBig, Padding * 2, Padding);
    DatePicker.Date := ADefault;
    
    // Set min/max dates if specified (non-zero values)
    if AMinDate <> 0 then
      DatePicker.MinDate := AMinDate;
    if AMaxDate <> 0 then
      DatePicker.MaxDate := AMaxDate;
    
    {$ifdef X_WIN}
    if TrndiNative.isDarkMode then
    begin
      DatePicker.Color := RGBToColor(53, 53, 53);
      DatePicker.Font.Color := RGBToColor(245, 245, 245);
    end;
    {$else}
    {$ifdef X_MAC}
    DatePicker.Color := MacInputBackgroundColor(DatePicker.Color);
    DatePicker.Font.Color := MacInputTextColor(DatePicker.Font.Color);
    {$endif}
    {$endif}
    if (size = sdsBig) then
    begin
      DatePicker.Font.Size := 20;
      DatePicker.Height := DatePicker.Height * 2;
    end;

    OkButton     := MakeDialogButton(Dialog, size, smbSelect,   mrOk);
    CancelButton := MakeDialogButton(Dialog, size, smbUXCancel, mrCancel);
    CenterButtons(Dialog, OkButton, CancelButton,
      DatePicker.Top + DatePicker.Height, size, Padding);
    Dialog.ActiveControl := DatePicker;

    ModalResult := ShowModalSafe(Dialog);
    if ModalResult = mrOk then
      Result := DatePicker.Date;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function SlickeLog(
const dialogsize: TSlickeDialogSize;
const caption, msg, log: string;
const icon: SlickeUXImage = uxmtCog;
scale: integer = 1
): TModalResult;
begin
  Result := SlickeMsg(dialogsize, sMsgTitle, caption, msg, log,
    IfThen(TrndiNative.isDarkMode, uxclGray, uxclBlue), uxclLightBlue, [mbOK], icon, scale);
end;

function SlickeMsgYesNo(
const caption, desc: string;
const micon: SlickeUXImage = uxmtConfirmation;
const scale: single = 1): boolean;
begin
  result :=SlickeMsgYesNo(sdsAuto, caption, desc, micon, scale);
end;

function SlickeMsgYesNo(
const dialogsize: TSlickeDialogSize;
const caption, desc: string;
const micon: SlickeUXImage = uxmtConfirmation;
const scale: single = 1): boolean;
begin
  result := SlickeMsg(dialogsize, caption, desc,
    [[mbYes, mbNo], [mbNo, mbYes]], micon, scale) = mrYes;
end;

function SlickeMsg(
const caption, title, desc, logmsg: string;
dumpbg: TColor = uxclWhite;
dumptext: TColor = uxclRed;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  // A dynamic array can only default to nil, so restore this function's own
  // documented default before forwarding.
  if Length(buttons) = 0 then
    buttons := [mbAbort];
  result := SlickeMsg(sdsAuto, caption, title, desc, logmsg, dumpbg, dumptext, buttons, icon, scale, ADefault);
end;

{** See interface docs for behavior and parameters. }
function SlickeMsg(
const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
dumpbg: TColor = uxclWhite;
dumptext: TColor = uxclRed;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  if Length(buttons) = 0 then
    buttons := [mbAbort];
  // Call SlickeMsgEx with isHTML = false for backward compatibility
  Result := SlickeMsgEx(dialogsize, caption, title, desc, logmsg, false,
    dumpbg, dumptext, buttons, icon, scale, ADefault);
end;


function SlickePrompt(const dialogsize: TSlickeDialogSize;
const caption, text: string;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  if Length(buttons) = 0 then
    buttons := [mbOK];
  result := SlickeMsg(dialogsize, 'Trndi', caption, text, '', uxclWhite, uxclRed, buttons, icon, scale, ADefault);
end;

{** Alias for SlickeMsg. }
function SlickeHTMLMsg(
const dialogsize: TSlickeDialogSize;
const caption, html: string;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtInformation;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  if Length(buttons) = 0 then
    buttons := [mbOK];
  result := SlickeMsg(dialogsize, caption, html, buttons, icon, scale, 1, ADefault);
end;

{** See interface docs for behavior and parameters. }
function SlickeMsg(
const dialogsize: TSlickeDialogSize;
const caption, html: string;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
hpadding: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
const
  btnWidth = 75;
  padding  = 10;
var
  Dialog: TDialogForm;
  HtmlPanel: TPanel;
  IconBox: TImage;
  HtmlViewer: TIpHtmlPanel;
  {$ifdef X_WIN}
  OkButton:TDarkButton;
  {$else}
  OkButton: TButton;
  {$endif}
  mr, defBtn: TSlickeMsgDlgBtn;
  DefaultCtrl: TWinControl;
  ButtonActualWidth, posX, ProposedWidth, btnCount, totalBtnWidth: integer;
  bgcol: TColor;
  size: TSlickeDialogSize;
  sysfont, htmlstr: string;
  contentHeight, maxHeight, finalHeight: integer;
  hpd: TIpHttpDataProvider;
  htmldata: string;

function DecorateLinks(const Src, LinkColorHtml: string): string;
  var
    lower: string;
    searchPos, openPos, tagEndPos, closePos: SizeInt;
  begin
    Result := Src;
    lower := LowerCase(Result);
    searchPos := 1;
    while true do
    begin
      openPos := PosEx('<a', lower, searchPos);
      if openPos = 0 then
        Break;

      tagEndPos := PosEx('>', lower, openPos);
      if tagEndPos = 0 then
        Break;

      Insert('<u><font color="' + LinkColorHtml + '">', Result, tagEndPos + 1);
      lower := LowerCase(Result);

      closePos := PosEx('</a>', lower, tagEndPos + 1);
      if closePos = 0 then
        Break;

      Insert('</font></u>', Result, closePos);
      lower := LowerCase(Result);

      searchPos := closePos + Length('</a>') + Length('</font></u>');
    end;
  end;
begin
  // An empty list would leave OkButton unassigned when the dialog height is
  // computed below; fall back to this function's documented default.
  if Length(buttons) = 0 then
    buttons := [mbAbort];
  bgcol := getBackground;
  size := GeTSlickeDialogSize(dialogsize);

  Dialog := TDialogForm.CreateNew(nil);
  try

    Dialog.Caption := caption;
    Dialog.BorderStyle := bsDialog;
    {$ifdef LCLGTK3}Dialog.BorderStyle := bsSizeable;{$endif}
    Dialog.Position := poWorkAreaCenter;
    Dialog.Color := bgcol;
    Dialog.KeyPreview := true;
    Dialog.OnKeyDown := @Dialog.FormKeyDown;

    ProposedWidth := ifthen((size = sdsBig) , 650, 500);
    if ProposedWidth > 900 then
      ProposedWidth := 900;
    Dialog.ClientWidth := FitDialogWidth(floor(ProposedWidth * hpadding));

    // Icon at top
    IconBox := TImage.Create(Dialog);
    IconBox.Parent := Dialog;
    IconBox.Left := padding;
    IconBox.Top := padding;
    {$ifdef Darwin}
    // Drop the icon below the blended macOS title bar.
    IconBox.Top := IconBox.Top + padding;
    {$endif}
    IconBox.Width := ifthen((size = sdsBig) , 80, 48);
    IconBox.Height := IconBox.Width;
    // No font to set here: AssignEmoji renders onto its own bitmap canvas and
    // never consults IconBox.Font.
    AssignEmoji(IconBox, icon, bgcol);

    // HTML panel
    HtmlPanel := TPanel.Create(Dialog);
    HtmlPanel.Name := 'HtmlPanel';
    HtmlPanel.Parent := Dialog;
    HtmlPanel.Left := IconBox.Left + IconBox.Width + padding;
    HtmlPanel.Top := IconBox.Top;
    HtmlPanel.Width := Dialog.ClientWidth - HtmlPanel.Left - padding;
    HtmlPanel.Color := bgcol;
    HtmlPanel.BevelOuter := bvNone;

    HtmlViewer := TIpHtmlPanel.Create(HtmlPanel);
    HtmlViewer.Name := 'HtmlViewer';

    hpd := TIpHttpDataProvider.Create(Dialog);
    HtmlViewer.DataProvider := hpd;
    hpd.OnGetImage := @dialog.HTMLGetImageX;
    HtmlViewer.OnHotClick := @dialog.HTMLHotClick;
    HtmlViewer.OnKeyPress := @dialog.ElementKeyDown;

    HtmlViewer.Parent := HtmlPanel;
    HtmlViewer.Left := 0;
    HtmlViewer.Top := 0;
    HtmlViewer.Width := HtmlPanel.Width;
    HtmlViewer.FixedTypeface := 'Courier New';
    HtmlViewer.DefaultTypeFace := ifthen((size = sdsBig) , 'Segoe UI', 'Tahoma');
    HtmlViewer.DefaultFontSize := ifthen((size = sdsBig) , 16, 12);
    HtmlViewer.FlagErrors := false;
    HtmlViewer.Color := bgcol;
    HtmlViewer.AllowTextSelect := false;  // Prevent text selection like TLabel
    
    // Load HTML content with system font and colors
    FontTXTInList(sysfont);
    htmlstr := DecorateLinks(html, TColorToHTML(getBaseColor));
    htmldata := '<html><body bgcolor="' + TColorToHTML(bgcol) + '" text="' + TColorToHTML(getBaseColor) + '" style="font-family: ' + sysfont + ';">' +
      htmlstr +
      '</body></html>';
    HtmlViewer.SetHtmlFromStr(
      htmldata
      );

    // Calculate content height and adjust dialog
    maxHeight := Round(ScreenUsableHeight * 0.8);
    contentHeight := Round((HtmlViewer.GetContentSize.cy + 20) * scale);  // Apply scale multiplier to height
    if contentHeight < 150 then
      contentHeight := 150;  // Minimum height
    if contentHeight > (maxHeight - 200) then
      contentHeight := maxHeight - 200;  // Leave room for icon and buttons
    
    HtmlViewer.Height := contentHeight;
    HtmlPanel.Height := contentHeight;

    // Count buttons
    btnCount := Length(buttons);
    defBtn := PickDefaultButton(buttons, ADefault);
    DefaultCtrl := nil;

    ButtonActualWidth := ifthen((size = sdsBig) , btnWidth * 2, btnWidth);
    totalBtnWidth := (btnCount * ButtonActualWidth) + ((btnCount - 1) * padding);
    posX := (Dialog.ClientWidth - totalBtnWidth) div 2;

    // Create buttons
    for mr in buttons do
    begin
      {$ifdef X_WIN}
      OkButton := TDarkButton.Create(Dialog);
      {$else}
      OkButton := TButton.Create(Dialog);
      {$endif}
      OkButton.Parent := Dialog;
      {$ifdef LCLGTK2}OkButton.Font.Color := clBlack;{$endif}
      OkButton.Caption := langs[mr];
      dialog.addButton(okbutton.caption);
      OkButton.Width := ButtonActualWidth;
      OkButton.Height := ifthen((size = sdsBig) , 50, 25);
      if (size = sdsBig) then
        OkButton.Font.Size := 12;
      OkButton.Left := posX;
      OkButton.Top := HtmlPanel.Top + HtmlPanel.Height + padding;
      OkButton.ModalResult := UXButtonToModalResult(mr);
      Inc(posX, ButtonActualWidth + padding);
      // Default is chosen by identity, not position, so it survives a reversed row
      if (mr = defBtn) and (DefaultCtrl = nil) then
        DefaultCtrl := OkButton;
    end;

    ApplyDefaultButton(Dialog, DefaultCtrl);

    // Set final dialog height based on content
    finalHeight := OkButton.Top + OkButton.Height + padding;
    Dialog.ClientHeight := finalHeight;

    dialog.setContent(caption, htmldata);
    dialog.hasHTML := true;

    Result := ShowModalSafe(Dialog);
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function SlickeMsgEx(
const dialogsize: TSlickeDialogSize;
const caption, title, desc, logmsg: string;
isHTML: boolean;
dumpbg: TColor = uxclWhite;
dumptext: TColor = uxclRed;
buttons: TSlickeMsgDlgBtns = nil;
const icon: SlickeUXImage = uxmtCog;
scale: single = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
const
  btnWidth = 75;
  padding  = 10;
  memoPadLeft = 10;
  memoPadTop  = 8;
var
  Dialog: TDialogForm;
  MainPanel, TopPanel, TextPanel, LogPanel, ButtonPanel: TPanel;
  IconBox: TImage;
  TitleLabel, MsgLabel: TLabel;
  MsgScroll: TScrollBox;
  LogMemo: TMemo;
  LogHtmlPanel: TIpHtmlPanel;
  {$ifdef X_WIN}
  OkButton:TDarkButton;
  {$else}
  OkButton: TButton;
  {$endif}
  mr, defBtn: TSlickeMsgDlgBtn;
  DefaultCtrl: TWinControl;
  ButtonActualWidth, MaxDialogHeight, MsgWidth, NeededHeight,
  TitlePixelWidth, DescPixelWidth, TextPixelWidth,
  posX, ProposedWidth, btnCount, totalBtnWidth: integer;
  bgcol: TColor;
  TempFont: TFont;
  size: TSlickeDialogSize;
  MemoWrapper: TPanel;
  sysfont, htmlstr: string;
  hpd: TIpHttpDataProvider;
begin
  // An empty list would leave OkButton unassigned when the button panel height
  // is computed below; fall back to this function's documented default.
  if Length(buttons) = 0 then
    buttons := [mbAbort];
  bgcol := getBackground;
  size := GeTSlickeDialogSize(dialogsize);

  Dialog := TDialogForm.CreateNew(nil);
  try
    Dialog.Caption := caption;
    Dialog.BorderStyle := bsDialog;
    {$ifdef LCLGTK3}Dialog.BorderStyle := bsSizeable;{$endif}
    Dialog.Position := poWorkAreaCenter;
    Dialog.Color := bgcol;
    Dialog.AutoSize := true;
    Dialog.KeyPreview := true;
    Dialog.OnKeyDown := @Dialog.FormKeyDown;
    MaxDialogHeight := Round(ScreenUsableHeight * 0.8);

    // Main panel
    MainPanel := TPanel.Create(Dialog);
    MainPanel.Parent := Dialog;
    MainPanel.Align := alClient;
    MainPanel.BevelOuter := bvNone;
    MainPanel.Color := bgcol;
    MainPanel.AutoSize := true;

    // Top panel
    TopPanel := TPanel.Create(MainPanel);
    TopPanel.Parent := MainPanel;
    TopPanel.Align := alTop;
    TopPanel.BevelOuter := bvNone;
    TopPanel.Color := bgcol;
    TopPanel.AutoSize := true;
    {$ifdef Darwin}
    // Drop the icon/text row below the blended macOS title bar.
    TopPanel.BorderSpacing.Top := padding;
    {$endif}

    // Icon
    IconBox := TImage.Create(TopPanel);
    IconBox.Parent := TopPanel;
    IconBox.Align := alLeft;
    {$ifdef Darwin}
    // bsDialog has no native chrome inset on macOS with the melted titlebar,
    // so indent the icon to match the visual padding other platforms get.
    IconBox.BorderSpacing.Left := padding;
    {$endif}
    case size of
    sdsBig:
      IconBox.Width := 100;
    sdsMedium:
      IconBox.Width :=75;
    else
      IconBox.Width := 50;
    end;
    IconBox.Height := IconBox.Width;
    // No font to set here: AssignEmoji renders onto its own bitmap canvas and
    // never consults IconBox.Font.
    Dialog.HandleNeeded;
    AssignEmoji(IconBox, icon, bgcol);

    // Text panel
    TextPanel := TPanel.Create(TopPanel);
    TextPanel.Parent := TopPanel;
    TextPanel.Align := alClient;
    TextPanel.BevelOuter := bvNone;
    TextPanel.Color := bgcol;

    // Width calculations. Measure via a temporary TBitmap rather than
    // Dialog.Canvas: TForm/TPanel canvases outside a paint event can SIGABRT
    // on the Cocoa widgetset (see memory `cocoa-label-canvas`). The Dialog
    // hasn't been shown yet here, so its canvas context is not guaranteed.
    TitlePixelWidth := 0;
    with Graphics.TBitmap.Create do
    try
      SetSize(1, 1);
      Canvas.Font.Assign(Dialog.Font);
      if Trim(title) <> '' then
        TitlePixelWidth := Canvas.TextWidth(title);
      DescPixelWidth := Canvas.TextWidth(desc);
    finally
      Free;
    end;
    TextPixelWidth := Max(TitlePixelWidth, DescPixelWidth);

    ProposedWidth := IconBox.Width + TextPixelWidth + (padding * 6) + 20;

    if (size = sdsBig) then
    begin
      if ProposedWidth < 650 then
        ProposedWidth := 650;
    end
    else
    if ProposedWidth < 400 then
      ProposedWidth := 400;

    if logmsg <> '' then
      if ProposedWidth < 500 then
        ProposedWidth := 500;

    if ProposedWidth > 900 then
      ProposedWidth := 900;
    // The minimums above can exceed a small panel's width; clamp last.
    ProposedWidth := FitDialogWidth(ProposedWidth);

    Dialog.ClientWidth := ProposedWidth;
    MsgWidth := Dialog.ClientWidth - (IconBox.Width + (padding * 3));

    // Desc height
    TempFont := TFont.Create;
    try
      case size of
      sdsBig:
        TempFont.Size := 24;
      sdsMedium:
        TempFont.Size := 20;
      end;
      TempFont.Style := [];
      TempFont.Color := getBaseColor;
      NeededHeight := MeasureWrappedHeight(desc, TempFont, MsgWidth);
    finally
      TempFont.Free;
    end;

    MsgScroll := nil;  // may stay nil when NeededHeight fits without scrolling

    if (size = sdsBig) then
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
        MsgScroll.VertScrollBar.Visible := true;
        MsgScroll.BorderStyle := bsNone;

        MsgLabel := TLabel.Create(MsgScroll);
        MsgLabel.Name := 'MsgLabel';
        MsgLabel.Parent := MsgScroll;
        MsgLabel.WordWrap := true;
        MsgLabel.AutoSize := true;
        MsgLabel.Font.Size := 24;
        MsgLabel.Font.Style := [];
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := getBaseColor;
        MsgLabel.Align := alTop;
      end
      else
      begin
        MsgLabel := TLabel.Create(TextPanel);
        MsgLabel.Parent := TextPanel;
        MsgLabel.Name := 'MsgLabel';
        MsgLabel.WordWrap := true;
        MsgLabel.AutoSize := true;
        MsgLabel.Font.Size := 24;
        MsgLabel.Font.Style := [];
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := getBaseColor;
        MsgLabel.Align := alTop;
        MsgLabel.BorderSpacing.Left := padding;
        MsgLabel.BorderSpacing.Right := padding;
        MsgLabel.BorderSpacing.Bottom := padding; // Padding towards log
      end;

      // Title last
      if Trim(title) <> '' then
      begin
        TitleLabel := TLabel.Create(TextPanel);
        TitleLabel.name := 'TitleLabel';
        TitleLabel.Parent := TextPanel;
        TitleLabel.WordWrap := true;
        TitleLabel.AutoSize := true;
        TitleLabel.Font.Size := 24;
        TitleLabel.Font.Style := [fsBold];
        TitleLabel.Caption := title;
        TitleLabel.Font.Color := getBaseColor;
        TitleLabel.Align := alTop;
        TitleLabel.BorderSpacing.Left := padding;
        TitleLabel.BorderSpacing.Right := padding;
        dialog.titleText := titlelabel.caption;
      end;
    end
    else
    begin
      // NON-BIG: title first
      if Trim(title) <> '' then
      begin
        TitleLabel := TLabel.Create(TextPanel);
        TitleLabel.Name := 'TitleLabel';
        TitleLabel.Parent := TextPanel;
        TitleLabel.WordWrap := true;
        TitleLabel.AutoSize := false;
        if size = sdsMedium then
          TitleLabel.Font.Size := 20;
        TitleLabel.Font.Style := [fsBold];
        TitleLabel.Caption := title;
        TitleLabel.Font.Color := getBaseColor;
        TitleLabel.Top := padding;
        TitleLabel.Left := padding;
        TitleLabel.Width := MsgWidth;
        TitleLabel.Height := CalcWrappedHeight(TitleLabel);
        dialog.titleText := titlelabel.caption;
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
        MsgScroll.VertScrollBar.Visible := true;
        MsgScroll.BorderStyle := bsNone;
        MsgScroll.BorderSpacing.Bottom := padding; // Padding to log panel

        MsgLabel := TLabel.Create(MsgScroll);
        MsgLabel.Name := 'MsgLabel';
        MsgLabel.Parent := MsgScroll;
        MsgLabel.WordWrap := true;
        MsgLabel.AutoSize := false;
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := getBaseColor;
        MsgLabel.Width := MsgWidth;
        MsgLabel.Height := NeededHeight;
        dialog.contentText := msglabel.Caption;
      end
      else
      begin
        MsgLabel := TLabel.Create(TextPanel);
        MsgLabel.Name := 'MsgLabel';
        MsgLabel.Parent := TextPanel;
        MsgLabel.WordWrap := true;
        MsgLabel.AutoSize := false;
        MsgLabel.Caption := desc;
        MsgLabel.Font.Color := getBaseColor;
        if Assigned(TitleLabel) then
          MsgLabel.Top := TitleLabel.Top + TitleLabel.Height + padding
        else
          MsgLabel.Top := padding;
        MsgLabel.Left := padding;
        MsgLabel.Width := MsgWidth;
        MsgLabel.Height := NeededHeight;
        MsgLabel.BorderSpacing.Bottom := padding; // Padding towards log panel
        dialog.contentText := msglabel.Caption;
      end;
    end;

    // In non-big mode the labels use absolute positioning (no Align), which
    // macOS Cocoa AutoSize ignores — TopPanel collapses to the icon height and
    // the text is clipped.  Measure the content bottom and pin the height.
    if size <> sdsBig then
    begin
      TopPanel.AutoSize := false;
      if Assigned(MsgScroll) then
        TopPanel.Height := Max(IconBox.Height, MsgScroll.Top + MsgScroll.Height + padding)
      else
        TopPanel.Height := Max(IconBox.Height, MsgLabel.Top + MsgLabel.Height + padding);
    end;

    // LOG PANEL
    LogPanel := TPanel.Create(Dialog);
    LogPanel.Parent := Dialog;
    LogPanel.Align := alBottom;
    LogPanel.Height := round(ifthen((size = sdsBig) , 100, 50) * scale);
    LogPanel.Color := dumpbg;
    LogPanel.BevelOuter := bvNone;
    LogPanel.Visible := logmsg <> '';

    // Wrapper panel to simulate padding
    MemoWrapper := TPanel.Create(LogPanel);
    MemoWrapper.Parent := LogPanel;
    MemoWrapper.Align := alClient;
    MemoWrapper.Color := dumpbg;
    MemoWrapper.BevelOuter := bvNone;

    if isHTML then
    begin
      // Use TIpHtmlPanel for HTML content (cross-platform)
      LogHtmlPanel := TIpHtmlPanel.Create(MemoWrapper);
      LogHtmlPanel.Name := 'HtmlViewer';
      hpd := TIpHttpDataProvider.Create(Dialog);
      LogHtmlPanel.DataProvider := hpd;
      hpd.OnGetImage := @dialog.HTMLGetImageX;
      LogHTMLPanel.OnHotClick := @dialog.HTMLHotClick;
      LogHtmlPanel.OnKeyPress := @dialog.ElementKeyDown;
      LogHtmlPanel.Parent := MemoWrapper;
      LogHtmlPanel.Left := MemoPadLeft;
      LogHtmlPanel.Top := MemoPadTop;
      LogHtmlPanel.Width := MemoWrapper.ClientWidth - MemoPadLeft;
      LogHtmlPanel.Height := MemoWrapper.ClientHeight - MemoPadTop;
      LogHtmlPanel.Anchors := [akLeft, akTop, akRight, akBottom];
      LogHtmlPanel.Color := dumpbg;
      LogHtmlPanel.BorderStyle := bsNone;
      LogHtmlPanel.TabStop := false;  // Prevent focus
      LogHtmlPanel.OnKeyDown := @Dialog.FormKeyDown;
      LogHTMLPanel.MarginHeight:=0;
      LogHTMLPanel.MarginWidth:=0;
      // Load HTML content with body tag for background color
      try
        // Wrap content in HTML structure with background color from dumpbg
        FontTXTInList(sysfont);
        htmlstr := StringReplace(
          logmsg,
          '">', '"><u><font color="' + TColorToHTML(getBaseColor) + '">',
          [rfReplaceAll]
          );

        htmlstr := StringReplace(
          htmlstr,
          '</a>',
          '</font></u></a>',
          [rfReplaceAll]
          );
        htmlstr :=           '<html><body bgcolor="' + TColorToHTML(dumpbg) + '" text="' + TColorToHTML(dumptext) + '" style="font-family: ' + sysfont + ';">' +
          htmlstr +
          '</body></html>';
        LogHtmlPanel.SetHtmlFromStr(
          htmlstr
          );
        dialog.contentText := htmlstr;
        dialog.hasHTML := true;
      except
        on E: Exception do
        begin
          // Fallback to plain text if HTML parsing fails
          LogHtmlPanel.SetHtmlFromStr(
            '<html><body bgcolor="' + TColorToHTML(dumpbg) + '" text="' + TColorToHTML(dumptext) + '" style="font-family: Verdana, Arial, sans-serif;"><pre>' +
            logmsg +
            '</pre></body></html>'
            );
        end;
      end;
    end
    else
    begin
      // Use TMemo for plain text
      LogMemo := TMemo.Create(MemoWrapper);
      LogMemo.Name := 'LogMemo';
      LogMemo.Parent := MemoWrapper;
      LogMemo.Left := MemoPadLeft;
      LogMemo.Top := MemoPadTop;
      LogMemo.Width := MemoWrapper.ClientWidth - MemoPadLeft;
      LogMemo.Height := MemoWrapper.ClientHeight - MemoPadTop;
      LogMemo.Anchors := [akLeft, akTop, akRight, akBottom];
      LogMemo.ReadOnly := true;
      LogMemo.Color := dumpbg;
      LogMemo.Font.Color := dumptext;
      LogMemo.ScrollBars := ssAutoVertical;
      LogMemo.BorderStyle := bsNone;
      LogMemo.Text := TrimSet(logmsg, [#10, #13]);
      LogMemo.OnKeyDown := @Dialog.FormKeyDown;
      dialog.extraText := LOgMemo.Text;
    end;

    // BUTTON PANEL
    ButtonPanel := TPanel.Create(Dialog);
    ButtonPanel.Name := 'pnButtons';
    ButtonPanel.Caption := '';
    ButtonPanel.Parent := Dialog;
    ButtonPanel.Align := alBottom;
    ButtonPanel.BevelOuter := bvNone;
    ButtonPanel.Color := bgcol;

    case size of
    sdsBig:
      ButtonActualWidth := btnWidth*2;
    sdsMedium:
      ButtonActualWidth := ceil(btnWidth*1.5);
    else
      ButtonActualWidth := btnWidth;
    end;

    btnCount := Length(buttons);
    if btnCount = 0 then
      btnCount := 1;

    totalBtnWidth := (ButtonActualWidth * btnCount) + (padding * (btnCount - 1));
    
    // Always center the action buttons
    posX := (Dialog.ClientWidth - totalBtnWidth) div 2;
    if posX < padding then
      posX := padding;
    
    // Add expand button only if log content is truncated/needs scrolling
    // Position it independently on the left side
    if (logmsg <> '') and 
      ((not isHTML and (LogMemo.Lines.Count > 3)) or
      (isHTML and (LogHtmlPanel.GetContentSize.cy > LogPanel.Height))) then
    begin
      {$ifdef X_WIN}
      OkButton := TDarkButton.Create(ButtonPanel);
      {$else}
      OkButton := TButton.Create(ButtonPanel);
      {$endif}
      OkButton.Parent := ButtonPanel;
      {$ifdef LCLGTK2}OkButton.Font.Color := clBlack;{$endif}
      OkButton.Caption := '⛶';  // Maximize symbol
// No dialog add here
      OkButton.Width := ifthen((size = sdsBig), 50, 30);
      OkButton.Height := ifthen((size = sdsBig), 50, 25);
      OkButton.Left := padding;
      OkButton.Top := padding;
      OkButton.OnClick := @Dialog.ExpandLogDialog;
      OkButton.TabStop := false;
      
      // Store references for expand method — only assign the live branch
      Dialog.LogExpandWrapper := LogPanel;
      if isHTML then
      begin
        Dialog.LogExpandMemo := nil;
        Dialog.LogExpandHtmlPanel := LogHtmlPanel;
      end
      else
      begin
        Dialog.LogExpandMemo := LogMemo;
        Dialog.LogExpandHtmlPanel := nil;
      end;
      Dialog.LogExpandButton := OkButton;
      Dialog.LogIsHTML := isHTML;
    end;

    defBtn := PickDefaultButton(buttons, ADefault);
    DefaultCtrl := nil;

    for mr in buttons do
    begin
      {$ifdef X_WIN}OkButton := TDarkButton.Create(ButtonPanel);{$else}OkButton := TButton.Create(ButtonPanel);{$endif}
      OkButton.Parent := ButtonPanel;
      {$ifdef LCLGTK2}OkButton.Font.Color := clBlack;{$endif}
      OkButton.Caption := langs[mr];
      dialog.addButton(okbutton.caption);
      OkButton.ModalResult := UXButtonToModalResult(mr);
      OkButton.Width := ButtonActualWidth;
      case size of
      sdsBig:
        OkButton.Height := OkButton.Height * 2;
      sdsMedium:
        OkButton.Height := ceil(OkButton.Height * 1.5);
      end;
      OkButton.Top := padding;
      OkButton.Left := posX;
      posX := posX + OkButton.Width + padding;
      // Default is chosen by identity, not position, so it survives a reversed row
      if (mr = defBtn) and (DefaultCtrl = nil) then
        DefaultCtrl := OkButton;
    end;

    ButtonPanel.Height := OkButton.Top + OkButton.Height + (padding * 2);
    ApplyDefaultButton(Dialog, DefaultCtrl);

    if Dialog.Height > MaxDialogHeight then
      Dialog.Height := MaxDialogHeight;

    ShowModalSafe(Dialog);
    Result := Dialog.ModalResult;
  finally
    Dialog.Free;
  end;
end;

{** See interface docs for behavior and parameters. }
function SlickeError(const dialogsize: TSlickeDialogSize;
const msg, error: string;
const icon: SlickeUXImage = uxmtWarning): TModalResult;
begin
  Result := SlickeMsg(dialogsize,
    sErrTitle, // caption
    sErrMsg,   // title
    msg,       // description
    error,     // log/dump text
    uxclWhite, // dump background color
    uxclRed, // dump text color
    [mbAbort], // buttons
    icon);
end;

{** See interface docs for behavior and parameters. }
function SlickeSucc(const dialogsize: TSlickeDialogSize;
const msg, desc, output: string;
dumpbg: TColor = uxclLightGreen;
dumptext: TColor = uxclDarkGreen;
const icon: SlickeUXImage = uxmtOK): TModalResult;
begin
  Result := SlickeMsg(dialogsize,
    sSuccTitle, // caption
    msg,        // title
    desc,       // description
    output,     // log/dump
    dumpbg,
    dumptext,
    [mbOK],
    WChar(icon));
end;

{** See interface docs for behavior and parameters. }
function SlickeSuccEx(const dialogsize: TSlickeDialogSize;
const msg, desc, output: string;
btns: TSlickeMsgDlgBtns;
dumpbg: TColor = uxclLightGreen;
dumptext: TColor = uxclDarkGreen;
const icon: SlickeUXImage = uxmtOK;
const scale: integer = 1;
ADefault: TSlickeMsgDlgBtn = mbSlickeNone): TModalResult;
begin
  Result := SlickeMsg(dialogsize,
    sSuccTitle,
    msg,
    desc,
    output,
    dumpbg,
    dumptext,
    btns,
    WChar(icon),
    scale);
end;

procedure TDialogForm.addButton(const btnName: string);
begin
  SetLength(buttons, Length(buttons) + 1);
  buttons[High(buttons)] := btnName;
end;

function TDialogForm.getContent: string;
var
  s: string;
begin
  Result := Format('[%s]'+LineEnding+'%s', [title, content]);
  if extra <> '' then
    Result := Result + LineEnding + '------' + LineEnding + extra;
  result := result + LineEnding;
  for s in buttons do
    Result := Result + '[' + s + '] ';
end;


procedure TDialogForm.setContent(const titleValue, value: string; const extraValue: string = '');
begin
  title := titleValue;
  content := value;
  extra := extraValue;
end;

{** Override to apply custom title bar colors on show. }

procedure TDialogForm.DoShow;
begin
  inherited DoShow;
  // Your show-time logic here. Example placeholder:
  if TrndiNative.isDarkMode then
    TrndiNative.SetTitleColor(handle, self.Color, clWhite)
  else
    TrndiNative.SetTitleColor(handle, self.Color, clBlack);
end;

{**
  Handle Enter/Esc keys to activate default/escape buttons.
  @param Sender Dialog form.
  @param Key Key code pressed.
  @param Shift Shift-state (unused).
}
procedure TDialogForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i, cancel, no, yes, ok, abort, ct, firstBtn: integer;
  btns: TComponent;
  target: TWinControl;
  modalRes, firstBtnResult: TModalResult;

function GetModalResult(comp: TComponent): TModalResult;
  begin
    Result := mrNone;
    if comp is TCustomButton then
      Result := (comp as TCustomButton).ModalResult
    {$ifdef Windows}
    else if comp is TDarkButton then
      Result := (comp as TDarkButton).ModalResult
    {$endif};
  end;

procedure ClickButton(idx: integer);
  var
    comp: TComponent;
  begin
    if idx < 0 then
      Exit;
    comp := target.Components[idx];
    if comp is TCustomButton then
      (comp as TCustomButton).Click
    {$ifdef Windows}
    else if comp is TDarkButton then
      (comp as TDarkButton).Click
    {$endif};
    // Consume the key: the default button would otherwise fire a second time
    Key := 0;
  end;

begin
  if (ssCtrl in Shift) and (Key = Ord('C')) then
  begin
    // A focused edit control owns Ctrl+C for its own selection; only copy the
    // dialog text when no text-editing control has focus.
    if (ActiveControl is TCustomEdit) or (ActiveControl is TCustomComboBox) or
      (ActiveControl is TCustomFloatSpinEditEx) then
      Exit;
    Clipboard.AsText := getContent;
    Key := 0;
    Exit;
  end;

  if not (key in [VK_ESCAPE, VK_RETURN]) then
    Exit;

  cancel := -1;
  no := -1;
  yes := -1;
  ok := -1;
  abort := -1;
  ct := 0;
  firstBtn := -1;
  firstBtnResult := mrNone;

  btns := Self.FindComponent('pnButtons');
  if btns = nil then
    target := Self
  else
    target := btns as TPanel;

  // Scan for buttons and their modal results
  for i := 0 to target.ComponentCount - 1 do
  begin
    modalRes := GetModalResult(target.Components[i]);
    if modalRes = mrNone then
      Continue;

    if firstBtn < 0 then
    begin
      firstBtn := i;
      firstBtnResult := modalRes;
    end;

    case modalRes of
    mrCancel:
      cancel := i;
    mrNo:
      no := i;
    mrYes:
      yes := i;
    mrOk, mrClose:
      ok := i;
    mrAbort:
      abort := i;
    end;
    Inc(ct);
  end;

  // Handle key presses
  case key of
  VK_ESCAPE:
    if cancel >= 0 then
      ClickButton(cancel)
    else
    if no >= 0 then
      ClickButton(no)
    else
    if abort >= 0 then
      ClickButton(abort)
    else
    if (ct = 1) and (ok >= 0) then
      ClickButton(ok);// ESC priority: Cancel > No > Abort > OK/Close (single-button only)

  VK_RETURN:
    if ok >= 0 then
      ClickButton(ok)
    else
    if yes >= 0 then
      ClickButton(yes)
    else
    if (ct = 1) and (firstBtnResult in [mrClose, mrIgnore, mrRetry]) then
      ClickButton(firstBtn);// ENTER priority: OK > Yes > Close/Ignore/Retry (single-button only)

  end;
end;

{** Expand log message area to 3/4 screen size }
procedure TDialogForm.ExpandLogDialog(Sender: TObject);
var
  newHeight, newWidth: integer;
begin
  // Work area rather than raw screen: on panels/taskbars the 3/4 box would
  // otherwise be centred partly underneath them.
  newHeight := Round(ScreenUsableHeight * 0.75);
  newWidth := Round(ScreenUsableWidth * 0.75);

  // Resize the entire dialog
  Self.ClientWidth := newWidth;
  Self.ClientHeight := newHeight;

  // Manually center the dialog. The work area's origin matters as much as its
  // size: a top or left panel, or a work area that begins on a secondary
  // monitor, gives a non-zero origin that a bare width/2 ignores.
  Self.Left := ScreenUsableLeft + ((ScreenUsableWidth - Self.Width) div 2);
  Self.Top := ScreenUsableTop + ((ScreenUsableHeight - Self.Height) div 2);
  
  if Assigned(LogExpandWrapper) then
    LogExpandWrapper.Height := Round(newHeight * 0.7)// LogExpandWrapper is the LogPanel - expand it to fill available space
// Leave room for button panel at bottom (which is alBottom, so it auto-positions)
  ;

  // Hide expand button after expansion
  if Assigned(LogExpandButton) then
    LogExpandButton.Visible := false;
  
  // No need to recenter action buttons - they're already centered
end;

{$ifndef Windows}
{** Ensure KeyPreview is set on non-Windows upon handle creation. }
procedure TDialogForm.CreateWnd;
{$ifdef X_MAC}
var
  CocoaWin: NSWindow;
{$endif}
begin
  // Decide taskbar presence before the handle exists so it is honoured in
  // CreateParams on every widgetset. Opt-in via SlickeDialogsInTaskbar.
  if SlickeDialogsInTaskbar then
    ShowInTaskBar := stAlways
  else
    ShowInTaskBar := stDefault;
  inherited CreateWnd;
  hasHTML := false;
  KeyPreview := true;
  // Ensure dialogs have an explicit popup owner so X11/Wayland window managers
  // can treat them as transient for the initiating window. Also provide a
  // conservative fallback on non-Windows systems by keeping the dialog on top
  // briefly which mitigates cases where the WM ignores transient hints
  // (common on some Raspberry Pi/embedded setups).
  try
    // Prefer the currently active form (most likely the initiator) as the popup
    // parent. Fall back to Owner (if it's a TForm) and then Application.MainForm.
    if Assigned(Screen) and Assigned(Screen.ActiveForm) then
    begin
      PopupMode := pmExplicit;
      PopupParent := Screen.ActiveForm;
    end
    else
    if Assigned(Owner) and (Owner is TForm) then
    begin
      PopupMode := pmExplicit;
      PopupParent := TForm(Owner);
    end
    else
    if Assigned(Application) and Assigned(Application.MainForm) then
    begin
      PopupMode := pmExplicit;
      PopupParent := Application.MainForm;
    end;
  except
    // Some LCL backends may raise; ignore and continue.
  end;

  {$ifdef X_MAC}
  // Make the title bar blend into the dialog: transparent titlebar + full-size
  // content view so there's no visible divider between bar and content. Hide
  // the traffic-light buttons too — SlickeDialog answers come from the on-screen
  // buttons, so the red X would be an ambiguous second exit path.
  if HandleAllocated then
  try
    CocoaWin := NSView(Handle).window;
    SetCocoaUnifiedTitlebar(CocoaWin, True);
    HideCocoaWindowButtons(CocoaWin);
  except
    // Ignore Cocoa errors silently — degrade to a normal title bar.
  end;
  {$endif}
end;
{$endif}

{$ifdef X_WIN}
{**
  Set system menu style, apply immersive dark titlebar when available, and refresh frame.
}
procedure TDialogForm.CreateWnd;
const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
var
  Value: integer;
begin
  // Decide taskbar presence before the handle exists so CreateParams applies
  // the correct WS_EX_APPWINDOW state. Opt-in via SlickeDialogsInTaskbar.
  if SlickeDialogsInTaskbar then
    ShowInTaskBar := stAlways
  else
    ShowInTaskBar := stDefault;
  inherited CreateWnd;
  hasHTML := false;
  if HandleAllocated then
    SetWindowLong(Handle, GWL_STYLE,
      GetWindowLong(Handle, GWL_STYLE) or WS_SYSMENU);

  KeyPreview := true;
  // As above, ensure PopupMode/PopupParent is set where possible.
  try
    if Assigned(Screen) and Assigned(Screen.ActiveForm) then
    begin
      PopupMode := pmExplicit;
      PopupParent := Screen.ActiveForm;
    end
    else
    if Assigned(Owner) and (Owner is TForm) then
    begin
      PopupMode := pmExplicit;
      PopupParent := TForm(Owner);
    end
    else
    if Assigned(Application) and Assigned(Application.MainForm) then
    begin
      PopupMode := pmExplicit;
      PopupParent := Application.MainForm;
    end;
  except end;
  if not TrndiNative.isDarkMode then
    Exit;
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

{$ifdef X_WIN}
{ TDarkButton - Adapted from metadarkstyle's DrawPushButton }

constructor TDarkButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModalResult := mrNone;
  FDown := false;
  FHot := false;
  FFocused := false;
  FCaption := '';
  Width := 75;
  Height := 25;
  Cursor := crDefault;
  TabStop := true;
  ControlStyle := ControlStyle + [csClickEvents, csCaptureMouse, csOpaque];
end;

procedure TDarkButton.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Invalidate;
  end;
end;

procedure TDarkButton.Paint;
var
  BtnRect: TRect;
  TextStyle: TTextStyle;
  i: integer;
begin
  BtnRect := ClientRect;
  Canvas.Brush.Style := bsSolid;
  
  if not TrndiNative.isDarkMode then
  begin
    // Light mode - draw standard button appearance
    Canvas.Pen.Color := GetSysColor(COLOR_BTNFACE);  // Match form background
    
    if FDown then
    begin
      Canvas.Brush.Color := RGBToColor(200, 200, 200);  // Pressed
    end
    else if FHot then
    begin
      Canvas.Brush.Color := RGBToColor(229, 241, 251);  // Hover
    end
    else
    begin
      Canvas.Brush.Color := RGBToColor(225, 225, 225);  // Normal
    end;
    
    Canvas.RoundRect(BtnRect, 2, 2);
    
    // Focus indicator in light mode
    if FFocused then
    begin
      Canvas.Pen.Color := RGBToColor(0, 120, 215);  // Blue focus
      Canvas.Pen.Width := 1;
      Canvas.Brush.Style := bsClear;
      Canvas.RoundRect(BtnRect.Left + 2, BtnRect.Top + 2, 
                       BtnRect.Right - 2, BtnRect.Bottom - 2, 2, 2);
      Canvas.Brush.Style := bsSolid;
    end;
    
    // Text in dark color for light mode
    Canvas.Font.Color := clBlack;
  end
  else
  begin
    // Dark mode
    Canvas.Pen.Color := RGBToColor(32, 32, 32);  // Match form background
    
    if FDown then
    begin
      Canvas.Brush.Color := RGBToColor(30, 30, 30);  // Darker when pressed
    end
    else if FHot then
    begin
      Canvas.Brush.Color := RGBToColor(80, 80, 80);  // Lighter on hover
    end
    else
    begin
      Canvas.Brush.Color := RGBToColor(53, 53, 53);  // Normal state
    end;
    
    Canvas.RoundRect(BtnRect, 4, 4);
    
    // Focus indicator in dark mode
    if FFocused then
    begin
      Canvas.Pen.Color := RGBToColor(160, 160, 160);  // Subtle light gray
      Canvas.Pen.Width := 1;
      Canvas.Brush.Style := bsClear;
      Canvas.RoundRect(BtnRect.Left + 2, BtnRect.Top + 2, 
                       BtnRect.Right - 2, BtnRect.Bottom - 2, 3, 3);
      Canvas.Brush.Style := bsSolid;
    end;
    
    // Text in light color for dark mode
    Canvas.Font.Color := RGBToColor(245, 245, 245);
  end;
  
  // Draw text centered
  Canvas.Font.Size := Font.Size;
  Canvas.Font.Name := Font.Name;
  
  TextStyle := Canvas.TextStyle;
  TextStyle.Alignment := taCenter;
  TextStyle.Layout := tlCenter;
  TextStyle.Opaque := false;
  TextStyle.Clipping := true;
  
  Canvas.TextRect(BtnRect, 0, 0, FCaption, TextStyle);
end;

procedure TDarkButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FDown := true;
    Invalidate;
  end;
end;

procedure TDarkButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  WasDown: boolean;
begin
  WasDown := FDown;
  FDown := false;
  inherited MouseUp(Button, Shift, X, Y);
  Invalidate;
  
  if WasDown and (Button = mbLeft) and PtInRect(ClientRect, Classes.Point(X, Y)) then
    Click;
end;

procedure TDarkButton.MouseEnter;
begin
  inherited MouseEnter;
  FHot := true;
  Invalidate;
end;

procedure TDarkButton.MouseLeave;
begin
  inherited MouseLeave;
  FHot := false;
  FDown := false;
  Invalidate;
end;

procedure TDarkButton.Click;
var
  Form: TCustomForm;
begin
  inherited Click;
  if FModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) then
      Form.ModalResult := FModalResult;
  end;
end;

procedure TDarkButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  // Handle Enter and Space keys to activate button
  if (Key = VK_RETURN) or (Key = VK_SPACE) then
  begin
    Click;
    Key := 0; // Mark as handled
  end;
end;

procedure TDarkButton.DoEnter;
begin
  inherited DoEnter;
  FFocused := true;
  Invalidate;
end;

procedure TDarkButton.DoExit;
begin
  inherited DoExit;
  FFocused := false;
  Invalidate;
end;
{$endif}

procedure TDialogForm.SlickeMessageOnMouseDown(sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SlickeMessageOnClick(sender);
end;

{** Close handler for full-screen overlay messages created by @link(SlickeMessage). }
procedure TDialogForm.SlickeMessageOnClick(sender: TObject);
var
  P: TPanel;
begin
  P := (sender as TButton).parent as TPanel;
  // Clear the name so the next SlickeMessage call can create a new overlay.
  // Never free directly here — in Qt, destroying a QWidget from inside its own
  // clicked() signal makes the signal dispatch touch freed memory. Hide now and
  // let the LCL release it from the message loop once dispatch has unwound;
  // that also frees the child labels, the button and the handler form.
  P.Name := '';
  P.Hide;
  Application.ReleaseComponent(P);
end;

{** OnChange handler for font combo in SlickeFontPicker - updates live preview. }
procedure TDialogForm.FontComboChange(Sender: TObject);
var
  Combo: TComboBox;
begin
  Combo := Sender as TComboBox;
  if Assigned(FontPickerPreview) and (Combo.ItemIndex >= 0) then
    FontPickerPreview.Font.Name := Combo.Items[Combo.ItemIndex];
end;

{$ifdef X_WIN}
{**
  Owner-draw for dark buttons on Windows.
  @param Sender The @code(TDarkButton) being drawn.
  @param ACanvas Canvas to draw on.
  @param ARect Button rectangle.
  @param State Button state (up/down/hot).
}
procedure TDialogForm.ButtonDrawItem(Sender: TObject;
ACanvas: TCanvas; ARect: TRect; State: TButtonState);
var
  Btn: TDarkButton absolute Sender;
  TxtFlags: cardinal;
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
  DrawText(ACanvas.Handle, pchar(Btn.Caption), Length(Btn.Caption),
    ARect, TxtFlags);

  // 4) Focus indicator
  if bsHot = State then
    ACanvas.DrawFocusRect(ARect);
end;
{$endif}

procedure TDialogForm.ElementKeyDown(Sender: TObject; var Key: char);
begin
  key := #0;
end;

procedure TDialogForm.HTMLHotClick(Sender: TObject);
begin
  if SlickePrompt(sdsAuto, sURLTitle, sURL, [[mbYes, mbNo], [mbNo, mbYes]]) = mrYes then
    OpenURL((sender as TIpHtmlPanel).HotURL);
end;

procedure TDialogForm.HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
var Picture: TPicture);
var
  res: string;
  ms: TStringStream;
begin
  // A nil Picture tells TIpHtmlPanel to render the image as missing; never
  // let a fetch/decode failure escape into the HTML layout code.
  Picture := nil;
  if not TrndiNative.getURL(url, res) then
    Exit;
  ms := TStringStream.Create(res);
  try
    Picture := TPicture.Create;
    try
      Picture.LoadFromStream(ms);
    except
      FreeAndNil(Picture);
    end;
  finally
    ms.Free;
  end;
end;



end.
