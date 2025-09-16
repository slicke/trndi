(*
 * This file is part of Trndi (https://github.com/slicke/trndi or http://xxx.github.io).
 * Copyright (c) 2021-25 Bj√∂rn Lindh.
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

{
  This file contains platform-native code, written to make Trndi faster and smoother,
  and to minimize the need for 3rd-party libraries. It also provides native features
  such as Windows registry access and OpenSSL on Linux.
}

unit trndi.native;

{$I inc/native.inc} // Depending on your project setup, this may define X_WIN, X_PC, X_MAC, etc.

interface

uses
Classes, SysUtils, Graphics
{$IF DEFINED(X_MAC)},
NSMisc,
ns_url_request,
CocoaAll,
SimpleDarkMode
{$ELSEIF DEFINED(X_WIN)},
Windows, Registry, Dialogs, StrUtils, winhttpclient, shellapi, comobj,
Forms, variants
{$ELSEIF DEFINED(X_PC)},
fphttpclient, openssl, opensslsockets, IniFiles, Dialogs, extctrls, forms, math, LCLIntf
{$ENDIF}
{$IFDEF LCLQt6}//,
//qt6, qtwidgets, forms, QtWSForms, QtWSComCtrls
{$endif}
, process;

type
  TWSLVersion = (wslNone, wslVersion1, wslVersion2, wslUnknown);
  TTrndiBool = (tbUnset, tbTrue, tbFalse, tbUnknown);

  TWSLInfo = record
    IsWSL: Boolean;
    Version: TWSLVersion;
    DistroName: string;
    KernelVersion: string;
  end;
  { TrndiNative
    -----------
    Provides platform-native methods
  }
TrndiNative = class
private
  cfguser:   string;  // User prefix for config
  fsettings: TFormatSettings;

  function buildKey(const key: string; global: boolean): string;
  procedure updateLocale(const l: TFormatSettings);
public

  class var touchOverride: TTrndiBool;
    // Indicates if the user system is in a "dark mode" theme
  dark: boolean;

  procedure Speak(const Text: string);
  procedure attention(message: string);
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''): string;

  procedure SetRootSetting(keyname: string; const val: string);
  procedure SetSetting(const keyname: string; const val: string; global: boolean = false);
  procedure SetBoolSetting(const keyname: string; const val: boolean);
  procedure SetFloatSetting(const keyname: string; const val: single);
  procedure SetColorSetting(const keyname: string; val: TColor);
  function GetColorSetting(const keyname: string; const def: TColor = $000000): TColor;
  procedure DeleteSetting(const keyname: string; global: boolean = false);
  procedure DeleteRootSetting(keyname: string; const val: string);
  function GetRootSetting(const keyname: string; def: string = ''): string;
  function GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
  function GetCharSetting(const keyname: string; def: char = #0): char;
  function GetIntSetting(const keyname: string; def: integer = -1): integer;

  function GetFloatSetting(const keyname: string; def: single = -1): single;

  function GetBoolSetting(const keyname: string; def: boolean = false): boolean;
  class function isDarkMode: boolean;
  class function DetectTouchScreen(out multi: boolean): boolean;
  class function HasTouchScreen(out multi: boolean): boolean;
  class function HasTouchScreen: boolean;
  class function getURL(const url: string; out res: string): boolean; static;

    // Constructor/Destructor
  destructor Destroy; override;
  procedure start;
  procedure done;
  procedure setBadge(const Value: string; badgeColor: Tcolor; badge_size_ratio: double = 0.8; min_font_size: integer = 8);
  class procedure PlaySound(const FileName: string);

  constructor create(ua, base: string); overload;
  constructor create(ua: string); overload;
  constructor create; overload;
  {$if DEFINED(X_WIN)}
    class function SetDarkMode(win: HWND; Enable: Boolean = True): Boolean;
  {$elseif DEFINED(X_MAC)}
    class function setDarkMode: boolean;
  {$else}
    class function setDarkMode: boolean;
  {$endif}
  class function GetOSLanguage: string;
  class function HasDangerousChars(const FileName: string): Boolean;
  class function DetectWSL: TWSLInfo;

  property configUser: string read cfguser write cfguser;
  property locale: TFormatSettings read fsettings write updateLocale;
protected
  useragent: string;  // HTTP User-Agent string
  baseurl:   string;  // Base URL for requests
  {$ifdef lclqt6}
    tray: TTrayIcon;
  {$endif}

  {$IF DEFINED(X_PC)}
  inistore: TINIFile; // Linux/PC settings store
  {$ENDIF}

  {$ifdef windows}
const
  // Gr√§nssnitt f√∂r Windows 7+ Taskbar API
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11D0-958A-006097C9A090}';
  IID_ITaskbarList3: TGUID = '{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}';
  TBPF_NOPROGRESS     = $00000000;
  TBPF_INDETERMINATE  = $00000001;
  TBPF_NORMAL         = $00000002;
  TBPF_ERROR          = $00000004;
  TBPF_PAUSED         = $00000008;

type
  // Definiera Windows Taskbar API gr√§nssnitt
  ITaskbarList = interface(IUnknown)
    ['{56FDF342-FD6D-11d0-958A-006097C9A090}']
    function HrInit: HRESULT; stdcall;
    function AddTab(hwnd: HWND): HRESULT; stdcall;
    function DeleteTab(hwnd: HWND): HRESULT; stdcall;
    function ActivateTab(hwnd: HWND): HRESULT; stdcall;
    function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429b-A66E-1935E44F4317}']
    function MarkFullscreenWindow(hwnd: HWND; fFullscreen: BOOL): HRESULT; stdcall;
  end;

  ITaskbarList3 = interface(ITaskbarList2)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function SetProgressValue(hwnd: HWND; ullCompleted, ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: Cardinal): HRESULT; stdcall;
    function RegisterTab(hwndTab, hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab, hwndInsertBefore: HWND): HRESULT; stdcall;
    function SetTabActive(hwndTab, hwndMDI: HWND; tbatFlags: Cardinal): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT; pButton: Pointer): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT; pButton: Pointer): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: HWND; hIcon: HICON; pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: HWND; var prcClip: TRect): HRESULT; stdcall;
  end;

    ITaskbarList4 = interface(ITaskbarList3)
    ['{C43DC798-95D1-4BEA-9030-BB99E2983A1A}']
    function SetTabProperties(hwndTab: HWND; stpFlags: DWORD): HRESULT; stdcall;
    function SetTabActive(hwndTab: HWND; hwndMDI: HWND; dwReserved: DWORD): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT; pButton: Pointer): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT; pButton: Pointer): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: HWND; hIcon: HICON; pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: HWND; prcClip: PRect): HRESULT; stdcall;
    function SetTabOrder(hwndTab: HWND; hwndInsertBefore: HWND): HRESULT; stdcall;
    function SetProgressValue(hwnd: HWND; ullCompleted: ULONGLONG; ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: DWORD): HRESULT; stdcall;
    function RegisterTab(hwndTab: HWND; hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab: HWND): HRESULT; stdcall;
  end;
{$ENDIF}

end;

{$ifdef X_LINUXBSD}
function IsNotifySendAvailable: boolean;
{$endif}
//procedure QWindow_setWindowBadge(window: QWindowH; badge: PChar); cdecl; external 'libQt6Gui.so.6';
{$ifdef Windows}
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
function IsBurntToastAvailable: boolean;
{$endif}

implementation

{------------------------------------------------------------------------------
  TrndiNative.updateLocale
  ------------------------
  Sets formatting settings
 ------------------------------------------------------------------------------}
procedure TrndiNative.updateLocale(const l: TFormatSettings);
begin
  fsettings := l;
  DefaultFormatSettings := fsettings; // We need this for now
end;

{------------------------------------------------------------------------------
  TrndiNative.buildKey
  -------------------
  Gets the key name in the ini file/registry/etc
 ------------------------------------------------------------------------------}
function TrndiNative.buildKey(const key: string; global: boolean): string;
begin
  if global then
    result := key
  else if Trim(cfguser) <> '' then // Prepend the username and _
    result := Format('%s_%s', [cfguser, key])
  else
    result := key;
end;

{------------------------------------------------------------------------------
  TrndiNative.speak
  -----------------
  Uses TTS to read out a text
 ------------------------------------------------------------------------------}
{$if defined(X_WIN)}
procedure TrndiNative.Speak(const Text: string);
var
  Voice, Voices: OleVariant;
  lang: LANGID;
  LangHex: string;
begin
  Voice := CreateOleObject('SAPI.SpVoice');
  lang := GetUserDefaultLangID;

  // SAPI language filter expects hex without 0x, usually without leading zeros (e.g. "409")
  LangHex := UpperCase(IntToHex(lang, 1)); // e.g. 0x0409 -> "409"

  Voices := Voice.GetVoices('Language=' + LangHex, '');
  if (not VarIsEmpty(Voices)) and (Voices.Count > 0) then
    Voice.Voice := Voices.Item(0);
  // else: keep default SAPI voice

  Voice.Speak(Text, 0);
end;
{$elseif defined(X_MAC)}
procedure TrndiNative.Speak(const Text: string);
var
  o: string;
begin
  RunCommand('/usr/bin/say', [Text], o);
end;  
{$else}
function FindInPath(const FileName: string): string;
var
  PathVar, Dir: string;
  Paths: TStringList;
  i: Integer;
begin
  Result := '';
  PathVar := GetEnvironmentVariable('PATH');
  Paths := TStringList.Create;
  try
    Paths.Delimiter := ':';
    Paths.StrictDelimiter := True;
    Paths.DelimitedText := PathVar;
    for i := 0 to Paths.Count - 1 do
    begin
      Dir := IncludeTrailingPathDelimiter(Paths[i]);
      if FileExists(Dir + FileName) then
        Exit(Dir + FileName);
    end;
  finally
    Paths.Free;
  end;
end;

{------------------------------------------------------------------------------
  GetSystemLangTag
  -------------------
  Resturns the system's language code
 ------------------------------------------------------------------------------}
function GetSystemLangTag: string;
  function FirstSegment(const S, Sep: string): string;
  var P: SizeInt;
  begin
    Result := S;
    P := Pos(Sep, Result);
    if P > 0 then
      Result := Copy(Result, 1, P-1);
  end;
var
  L: string;
  P: SizeInt;
begin
  // Priority: LC_ALL > LANGUAGE (may be colon-separated) > LANG
  L := GetEnvironmentVariable('LC_ALL');
  if L = '' then
    L := GetEnvironmentVariable('LANGUAGE');
  if L = '' then
    L := GetEnvironmentVariable('LANG');

  if L = '' then
    Exit(''); // Let spd-say decide

  // If LANGUAGE is colon-separated (e.g., "de_CH:de:en_US"), take first
  L := FirstSegment(L, ':');

  // Remove encoding suffix (e.g., ".UTF-8")
  P := Pos('.', L);
  if P > 0 then
    L := Copy(L, 1, P-1);

  // Convert "en_US" -> "en-US"
  L := StringReplace(L, '_', '-', [rfReplaceAll]);

  // Normalize case: language lower, region upper
  L := LowerCase(L);
  P := Pos('-', L);
  if P > 0 then
    L := Copy(L, 1, P) + UpperCase(Copy(L, P+1, MaxInt));

  Result := L;
end;

procedure TrndiNative.Speak(const Text: string);
var
  CmdPath, Lang: string;
  Proc: TProcess;
begin
  CmdPath := FindInPath('spd-say');
  if CmdPath = '' then
  begin
    ShowMessage('Error: spd-say is not installed.');
    Exit;
  end;

  Lang := GetSystemLangTag;

  Proc := TProcess.Create(nil);
  try
    Proc.Executable := CmdPath;
    if Lang <> '' then
      Proc.Parameters.AddStrings(['-l', Lang]) // e.g., "en-US" or "de"
    else
      ; // leave to spd-say defaults

    // Protect text from being misread as an option
    Proc.Parameters.Add('--');
    Proc.Parameters.Add(Text);

    Proc.Options := [];
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;
{$endif}

{------------------------------------------------------------------------------
  TrndiNative.setDarkMode
  -------------------
  Sets the app/window to dark mode
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
class function TrndiNative.SetDarkMode(win: HWND; Enable: Boolean = True): Boolean;
const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
var
  Value: Integer;
begin
  Result := False;

  // Kolla Windows-version
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and (Win32BuildNumber < 17763)) then
    Exit; // Windows 10 1809 (build 17763)

  Value := Ord(Enable);
  Result := Succeeded(
    DwmSetWindowAttribute(win, DWMWA_USE_IMMERSIVE_DARK_MODE, @Value, SizeOf(Value))
  );
end;
{$elseif DEFINED(X_MAC)}
class function TrndiNative.setDarkMode: Boolean;
begin
  SimpleDarkMode.EnableAppDarkMode;
end;
{$else}
class function TrndiNative.setDarkMode: Boolean;
begin

end;
{$endif}
{$IFDEF Windows}
procedure TrndiNative.start;
begin

end;
{$ENDIF}

{$IFDEF Windows}
procedure TrndiNative.done;
begin

end;
{$ENDIF}

{$IFNDEF Windows}
procedure TrndiNative.start;
begin

end;

procedure TrndiNative.done;
begin

end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.setBadge
  -------------------
  Sets an overlay icon on the taskbar or such
 ------------------------------------------------------------------------------}
{$IFDEF LCLGTK3}
procedure TrndiNative.SetBadge(const Value: string;  badgeColor: Tcolor);
begin
end;
{$ENDIF}

{$IFDEF LCLGTK2}
procedure TrndiNative.SetBadge(const Value: string;  badgeColor: Tcolor);
begin
end;
{$ENDIF}

{$IFDEF LCLFPGUI}
procedure TrndiNative.SetBadge(const Value: string;  badgeColor: Tcolor);
begin
end;
{$ENDIF}

{$IFDEF LCLWIN32}
// Helper method to use the overlay icon approach for taskbar badges

procedure TrndiNative.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double = 0.8; min_font_size: integer = 8);
const
  // Badge sizing constants
//  BADGE_SIZE_RATIO = 0.8;      // Badge size as a ratio of icon size (0.5 = half)
//  MIN_FONT_SIZE = 8;           // Minimum font size for readability
  INITIAL_FONT_SIZE_RATIO = 0.5; // Initial font size as ratio of badge size
  TEXT_PADDING = 4;            // Padding inside badge in pixels
  CORNER_RADIUS = 6;           // Radius for rounded corners (except bottom-right)
var
  AppIcon, TempIcon: TIcon;
  Bitmap: Graphics.TBitmap;
  BadgeText: string;
  TextWidth, TextHeight: Integer;
  BadgeRect: Classes.TRect;
  IconWidth, IconHeight, BadgeSize: Integer;
  FontSize, Radius: Integer;
  TextColor: TColor;
  SavedDC: Integer;
  Region, SquareRegion: HRGN;
  RgnRect: Classes.TRect;
  dval: double;
begin
  // Create objects
  AppIcon := TIcon.Create;
  TempIcon := TIcon.Create;
  Bitmap := Graphics.TBitmap.Create;

  try
    // Clear badge if empty value
    if Value = '' then
    begin
      Application.Icon.Assign(Application.MainForm.Icon);
      {$IFDEF WINDOWS}
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG, 0);
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL, 0);
      {$ENDIF}
      Exit;
    end;

    // Format text
    try
      if TryStrToFloat(value, dval, fsettings) then
          BadgeText := FormatFloat('0.0', dval, fsettings)
      else
          BadgeText := value;
    except
      BadgeText := Value;
    end;

    // Get application icon and its dimensions
    AppIcon.Assign(Application.Icon);
    IconWidth := AppIcon.Width;
    IconHeight := AppIcon.Height;

    // Set minimum size if icon dimensions are invalid
    if (IconWidth <= 0) or (IconHeight <= 0) then
    begin
      IconWidth := 32;
      IconHeight := 32;
    end;

    // Calculate badge size based on icon size (use the smaller dimension for consistent badges)
    if IconWidth < IconHeight then
      BadgeSize := Round(IconWidth * BADGE_SIZE_RATIO)
    else
      BadgeSize := Round(IconHeight * BADGE_SIZE_RATIO);

    // Create working bitmap at proper icon size
    Bitmap.SetSize(IconWidth, IconHeight);
    Bitmap.Canvas.Brush.Color := clNone;
    Bitmap.Canvas.FillRect(Classes.Rect(0, 0, IconWidth, IconHeight));

    // Draw original icon
    DrawIconEx(Bitmap.Canvas.Handle, 0, 0, AppIcon.Handle, IconWidth, IconHeight, 0, 0, DI_NORMAL);

    // Position badge at bottom-right
    BadgeRect := Classes.Rect(
      IconWidth - BadgeSize,
      IconHeight - BadgeSize,
      IconWidth,
      IconHeight
    );

    // Setup for drawing the speech bubble shape
    Bitmap.Canvas.Brush.Color := BadgeColor;
    Bitmap.Canvas.Pen.Color := BadgeColor;

    // Draw speech bubble with square bottom-right corner
    // For smaller icons, use simpler shape to ensure clarity
    if BadgeSize <= 12 then
    begin
      // Simple square badge for very small icons
      Bitmap.Canvas.FillRect(BadgeRect);
    end
    else
    begin
      // Get the corner radius (scale with badge size but keep it reasonable)
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then
        Radius := 2;

      // Save the current region to restore later
      SavedDC := SaveDC(Bitmap.Canvas.Handle);

      // Create region variables
      Region := 0;
      SquareRegion := 0;

      try
        // Create a rounded rectangle region (except bottom-right corner)
        RgnRect := BadgeRect;
        Region := CreateRoundRectRgn(
          RgnRect.Left, RgnRect.Top,
          RgnRect.Right, RgnRect.Bottom,
          Radius * 2, Radius * 2
        );

        // Create a square region for the bottom-right corner
        SquareRegion := CreateRectRgn(
          RgnRect.Right - Radius, RgnRect.Bottom - Radius,
          RgnRect.Right, RgnRect.Bottom
        );

        // Combine the regions
        CombineRgn(Region, Region, SquareRegion, RGN_OR);

        // Select the combined region and fill it
        SelectClipRgn(Bitmap.Canvas.Handle, Region);
        Bitmap.Canvas.FillRect(BadgeRect);
      finally
        // Clean up the regions
        if SquareRegion <> 0 then
          DeleteObject(SquareRegion);
        if Region <> 0 then
          DeleteObject(Region);

        // Restore the original region
        RestoreDC(Bitmap.Canvas.Handle, SavedDC);
      end;
    end;

    // Determine text color based on background brightness
    if (0.299 * GetRValue(BadgeColor) + 0.587 * GetGValue(BadgeColor) + 0.114 * GetBValue(BadgeColor)) > 128 then
      TextColor := clBlack  // Use black text on light backgrounds
    else
      TextColor := clWhite; // Use white text on dark backgrounds

    // Configure text settings
    Bitmap.Canvas.Font.Name := 'Arial';
    Bitmap.Canvas.Font.Style := [fsBold];
    Bitmap.Canvas.Font.Color := TextColor;

    // Set font size - scale dynamically based on badge size
    FontSize := Round(BadgeSize * INITIAL_FONT_SIZE_RATIO);
    if FontSize < MIN_FONT_SIZE then
      FontSize := MIN_FONT_SIZE; // Ensure minimum readable size
    Bitmap.Canvas.Font.Size := FontSize;

    // Get text dimensions
    TextWidth := Bitmap.Canvas.TextWidth(BadgeText);
    TextHeight := Bitmap.Canvas.TextHeight(BadgeText);

    // Scale down if necessary
    while (TextWidth > (BadgeSize - TEXT_PADDING)) and (FontSize > MIN_FONT_SIZE - 2) do
    begin
      Dec(FontSize);
      Bitmap.Canvas.Font.Size := FontSize;
      TextWidth := Bitmap.Canvas.TextWidth(BadgeText);
      TextHeight := Bitmap.Canvas.TextHeight(BadgeText);
    end;

    // Center text in badge
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.TextOut(
      BadgeRect.Left + ((BadgeRect.Right - BadgeRect.Left) - TextWidth) div 2,
      BadgeRect.Top + ((BadgeRect.Bottom - BadgeRect.Top) - TextHeight) div 2,
      BadgeText
    );

    // Convert to icon and apply
    TempIcon.Assign(Bitmap);
    Application.Icon.Assign(TempIcon);

    // Update window icons
    {$IFDEF WINDOWS}
    SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG, Application.Icon.Handle);
    SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL, Application.Icon.Handle);
    {$ENDIF}
  finally
    Bitmap.Free;
    AppIcon.Free;
    TempIcon.Free;
  end;
end;


{$ENDIF}

{$ifdef X_PC}
procedure TrndiNative.SetBadge(const Value: string; BadgeColor: TColor;
                       badge_size_ratio: double = 0.8; min_font_size: integer = 8);
const
  INITIAL_FONT_SIZE_RATIO = 0.5;
  TEXT_PADDING = 3;
  CORNER_RADIUS = 6;
var
  BaseIcon, OutIcon: TIcon;
  Bmp: TBitmap;
  W, H, BadgeSize, Radius: Integer;
  BadgeRect: TRect;
  TextW, TextH: Integer;
  FontSize: Integer;
  TextColor: TColor;
  rgb: LongInt;
  r, g, b: Byte;
  BadgeText: string;
begin
  if not assigned(Tray) then   tray := TTrayIcon.Create(Application.MainForm);

  // Tomt => Âterst‰ll tray-ikonen frÂn Application.Icon
  if Value = '' then
  begin
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      Tray.Icon.Assign(Application.Icon);
    Tray.Visible := False; Tray.Visible := True;
    Exit;
  end;

  // Basikon: Application.Icon (enkel och stabil bas varje gÂng)
  BaseIcon := TIcon.Create;
  OutIcon  := TIcon.Create;
  Bmp      := TBitmap.Create;
  try
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      BaseIcon.Assign(Application.Icon)
    else
      BaseIcon.SetSize(24, 24); // fallback om app-ikon saknas

    W := BaseIcon.Width; H := BaseIcon.Height;
    if (W <= 0) or (H <= 0) then begin W := 24; H := 24; end;

    BadgeSize := Round(Min(W, H) * badge_size_ratio);
    if BadgeSize < 10 then BadgeSize := 10;

    Bmp.SetSize(W, H);
    Bmp.PixelFormat := pf32bit;

    // Rita bas
    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.Brush.Color := clNone;
    Bmp.Canvas.FillRect(Rect(0,0,W,H));
    Bmp.Canvas.Draw(0, 0, BaseIcon);

    // Badge-rektangel (nere till hˆger)
    BadgeRect := Rect(W - BadgeSize, H - BadgeSize, W, H);

    // Textf‰rg utifrÂn bakgrundens luminans
    rgb := ColorToRGB(BadgeColor);
    r := Byte(rgb); g := Byte(rgb shr 8); b := Byte(rgb shr 16);
    if (0.299*r + 0.587*g + 0.114*b) > 128 then
      TextColor := clBlack
    else
      TextColor := clWhite;

    // Rita badge-bubbla
    Bmp.Canvas.Brush.Color := BadgeColor;
    Bmp.Canvas.Pen.Color := BadgeColor;

    if BadgeSize <= 12 then
      Bmp.Canvas.FillRect(BadgeRect)
    else
    begin
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then Radius := 2;
      Bmp.Canvas.RoundRect(BadgeRect.Left, BadgeRect.Top, BadgeRect.Right, BadgeRect.Bottom,
                           Radius*2, Radius*2);
      // Liten \u201cfyrkant\u201d i nederhˆrnet fˆr tydlighet
      Bmp.Canvas.FillRect(Rect(BadgeRect.Right - Radius, BadgeRect.Bottom - Radius,
                               BadgeRect.Right, BadgeRect.Bottom));
    end;

    // Text
    Bmp.Canvas.Font.Name := 'DejaVu Sans';
    Bmp.Canvas.Font.Style := [fsBold];
    Bmp.Canvas.Font.Color := TextColor;

    FontSize := Round(BadgeSize * INITIAL_FONT_SIZE_RATIO);
    if FontSize < min_font_size then FontSize := min_font_size;
    Bmp.Canvas.Font.Size := FontSize;

    TextW := Bmp.Canvas.TextWidth(BadgeText);
    TextH := Bmp.Canvas.TextHeight(BadgeText);
    while (TextW > (BadgeSize - TEXT_PADDING)) and (FontSize > min_font_size) do
    begin
      Dec(FontSize);
      Bmp.Canvas.Font.Size := FontSize;
      TextW := Bmp.Canvas.TextWidth(BadgeText);
      TextH := Bmp.Canvas.TextHeight(BadgeText);
    end;

    // Centrera texten i badgen
    Bmp.Canvas.Brush.Style := bsClear;
    Bmp.Canvas.TextOut(
      BadgeRect.Left + ((BadgeRect.Right - BadgeRect.Left) - TextW) div 2,
      BadgeRect.Top  + ((BadgeRect.Bottom - BadgeRect.Top) - TextH) div 2,
      BadgeText
    );

    // Tilldela till tray-ikonen och refresha
    OutIcon.Assign(Bmp);
    Tray.Icon.Assign(OutIcon);
    Tray.Visible := False; Tray.Visible := True;
  finally
    Bmp.Free; OutIcon.Free; BaseIcon.Free;
  end;
end;
{$endif}

{$IFDEF DARWIN}
procedure TrndiNative.SetBadge(const Value: string; BadgeColor: TColor;
                       badge_size_ratio: double = 0.8; min_font_size: integer = 8);
var
  NSS: NSString;
begin
  NSS := NSSTR(Value);
  NSApp.dockTile.setBadgeLabel(NSS);
  NSS.release;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.playSound
  -------------------
  Plays an audio file
 ------------------------------------------------------------------------------}
class procedure TrndiNative.PlaySound(const FileName: string);
function sIsValidAudioFile(const FileName: string): Boolean;
var
  Ext: string;
  ValidExtensions: array[0..6] of string = (
    '.wav', '.mp3', '.ogg', '.flac', '.aac', '.wma', '.m4a'
  );
  i: Integer;
begin
  Result := False;

  // Kontrollera om filen existerar
  if not FileExists(FileName) then
    Exit;

  Ext := LowerCase(ExtractFileExt(FileName));
  for i := 0 to High(ValidExtensions) do
  begin
    if Ext = ValidExtensions[i] then
    begin
      Result := True;
      Break;
    end;
  end;

  if not Result then
    Exit;

  if HasDangerousChars(FileName) then
    Exit;

  Result := True;
end;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);

  try
    {$IFDEF X_WIN}
    Process.CommandLine := 'mplay32 /play /close ' + FileName;
    {$ENDIF}

    {$IFDEF X_LINUXBSD}
    Process.CommandLine := 'aplay ' + FileName;
    {$ENDIF}

    {$IFDEF X_MAC}
    Process.CommandLine := 'afplay ' + FileName;
    {$ENDIF}
    Process.Execute;
  finally
    Process.Free;
  end;
end;

{------------------------------------------------------------------------------
  TrndiNative.Destroy
  -------------------
  Cleans up any allocated resources. On Linux/PC, frees the INI file handle.
 ------------------------------------------------------------------------------}
destructor TrndiNative.Destroy;
begin
  {$IF DEFINED(X_PC)}
  if Assigned(inistore) then
    inistore.Free;
  if assigned(tray) then
    tray.free;
  {$ENDIF}
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TrndiNative.DetectTouchScreen
  --------------------------
  Overridable touch screen setting
 ------------------------------------------------------------------------------}
 class function TrndiNative.HasTouchScreen(out multi: boolean): boolean;
 begin
   DetectTouchScreen(multi);
   result := HasTouchScreen;
 end;

 {------------------------------------------------------------------------------
  TrndiNative.hasTouchScreen
  -------------------
  Detects touch screen
 ------------------------------------------------------------------------------}
 class function TrndiNative.HasTouchScreen: boolean;
 var
   mt: boolean;
 begin
   case touchOverride of
     tbTrue: result := true;
     tbFalse: result := false;
   else
     result := DetectTouchScreen(mt);
   end;
 end;

 {------------------------------------------------------------------------------
   TrndiNative.DetectTouchScreen
   --------------------------
   Platform-specific detection of touch hardware.
  ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
class function TrndiNative.DetectTouchScreen(out multi: boolean): boolean;
const
  TABLET_CONFIG_NONE = $00000000;
  NID_INTEGRATED_TOUCH = $00000001;
  NID_EXTERNAL_TOUCH = $00000002;
  NID_INTEGRATED_PEN = $00000004;
  NID_EXTERNAL_PEN = $00000008;
  NID_MULTI_INPUT = $00000040;
  NID_READY = $00000080;
function IsTouchReady: boolean;
  var
    value: integer;
  begin
    value := GetSystemMetrics(SM_DIGITIZER);
    Result := value and NID_READY <> 0;
  end;

{------------------------------------------------------------------------------
  TrndiNative.isMultiTouch
  -------------------
  Detects more than one touch point
 ------------------------------------------------------------------------------}
function IsMultiTouch: boolean;
  var
    value: integer;
  begin
    value := GetSystemMetrics(SM_DIGITIZER);
    Result := value and NID_MULTI_INPUT <> 0;
  end;

{------------------------------------------------------------------------------
  TrndiNative.hasIntegratedTouch
  -------------------
  Detects a touch-first device
 ------------------------------------------------------------------------------}
function HasIntegratedTouch: boolean;
  var
    value: integer;
  begin
    value := GetSystemMetrics(SM_DIGITIZER);
    Result := value and NID_INTEGRATED_TOUCH <> 0;
  end;
var
  val: integer;
const
  SM_MAXIMUMTOUCHES = 95;
begin
  result := (HasIntegratedTouch) and (IsTouchReady);
  multi := IsMultiTouch;
end;
{$ELSEIF DEFINED(X_MAC)}
class function TrndiNative.DetectTouchScreen(out multi: boolean): boolean;
begin
  // macOS: Typically no standard touchscreen (unless iOS)
  Result := false;
  multi := false;
end;
{$ELSE}
class function TrndiNative.DetectTouchScreen(out multi: boolean): boolean;
var
  SL, Block: TStringList;
  i: integer;
  inBlock: boolean;
begin
  Result := false;
  multi := false;
  inBlock := false;
  Block := TStringList.Create;
  SL := TStringList.Create;
  try
    if FileExists('/proc/bus/input/devices') then
    begin
      SL.LoadFromFile('/proc/bus/input/devices');

      Block.Clear;
      for i := 0 to SL.Count - 1 do
      begin
        if Trim(SL[i]) = '' then
        begin
          // Blocket √§r slut, analysera det
          if Block.Count > 0 then
          begin
            // S√∂k touch i blocket
            if (Block.Text.ToLower.Contains('touch')) then
            begin
              Result := true;
              // S√∂k multicapabilities i blocket
              if (Block.Text.Contains('ABS_MT_POSITION')) or
                 (Block.Text.Contains('ABS_MT_SLOT')) or
                 (Block.Text.Contains('ABS_MT_TRACKING_ID')) then
                multi := true;
            end;
            Block.Clear;
          end;
        end
        else
          Block.Add(SL[i]);
      end;
      // Kolla sista blocket om filen inte slutar med blankrad
      if Block.Count > 0 then
      begin
        if (Block.Text.ToLower.Contains('touch')) then
        begin
          Result := true;
          if (Block.Text.Contains('ABS_MT_POSITION')) or
             (Block.Text.Contains('ABS_MT_SLOT')) or
             (Block.Text.Contains('ABS_MT_TRACKING_ID')) then
            multi := true;
        end;
      end;
    end;
  finally
    SL.Free;
    Block.Free;
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.create (default)
  ----------------------------
  An empty constructor that defaults useragent/baseurl to minimal placeholders.
 ------------------------------------------------------------------------------}
constructor TrndiNative.create;
begin
  // Provide a default user-agent and empty base URL
  create('Mozilla/5.0 (compatible; trndi) TrndiAPI', '');
end;

{------------------------------------------------------------------------------
  TrndiNative.create (overload)
  ----------------------------
  Allow a custom user-agent.
 ------------------------------------------------------------------------------}
constructor TrndiNative.create(ua: string);
begin
  // Provide a default user-agent and empty base URL
  create(ua, '');
end;

{------------------------------------------------------------------------------
  TrndiNative.create (overload)
  -----------------------------
  Allows specifying a custom user-agent and a base URL.
 -------------------------------------------------------
 -----------------------}
constructor TrndiNative.create(ua, base: string);
begin
  useragent := ua;
  baseurl   := base;
  // Check if we're in dark mode on creation
  dark := isDarkMode;
  fsettings := DefaultFormatSettings;
  if touchOverride = tbUnset then
     touchOverride := tbUnknown;
  cfguser := '';
end;

  {$IFDEF MSWINDOWS}
  {------------------------------------------------------------------------------
  TrndiNative.getLocaleInformation
  -------------------
    Windows only; Gets locale data
 ------------------------------------------------------------------------------}
function GetLocaleInformation(Flag: integer): string;
var
  wbuf: array[0..9] of WideChar;
begin
  if GetLocaleInfoW(LOCALE_USER_DEFAULT,
                    LOCALE_SISO639LANGNAME,
                    wbuf, Length(wbuf)) > 0 then
    Result := UTF8Encode(WideString(wbuf))
  else
    Result := '';
end;

{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.getOSLanguage
  -------------------
  Gets the operating system's language
 ------------------------------------------------------------------------------}
class function TrndiNative.GetOSLanguage: string;
begin
  {$IFDEF MSWINDOWS}
   Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
  {$ELSE}
    {$IFDEF DARWIN}
      result := NSLocale.currentLocale.localeIdentifier.UTF8String;
    {$ELSE}
       Result := SysUtils.GetEnvironmentVariable('LANG');
    {$ENDIF}
  {$ENDIF}
end;


{$ifdef X_LINUXBSD}
{------------------------------------------------------------------------------
  TrndiNative.isNotifySendAvailable
  ---------------------------------
  Checks if notify-send can be used
 ------------------------------------------------------------------------------}
function IsNotifySendAvailable: boolean;
var
  AProcess: TProcess;
  OutputLines: TStringList;
begin
  Result := false;
  AProcess := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    AProcess.Executable := '/usr/bin/which';
    AProcess.Parameters.Add('notify-send');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    AProcess.Execute;

    OutputLines.LoadFromStream(AProcess.Output);

    if (OutputLines.Count > 0) and FileExists(Trim(OutputLines[0])) then
      Result := true;

  except
    on E: Exception do
      Result := false;
  end;
  OutputLines.Free;
  AProcess.Free;
end;
{$endif}
{------------------------------------------------------------------------------
  TrndiNative.attention
  -------------------
  Flashes something depending on the system
 ------------------------------------------------------------------------------}

procedure TrndiNative.attention(message: string);
{$if  DEFINED(X_LINUXBSD)}


procedure SendNotification(Title, Message: string);
  var
    AProcess: TProcess;
  begin
    if IsNotifySendAvailable then
    begin
      AProcess := TProcess.Create(nil);
      try
        AProcess.Executable := '/usr/bin/notify-send';
        AProcess.Parameters.Add(Title);
        AProcess.Parameters.Add(Message);
        AProcess.Options := AProcess.Options + [poNoConsole];
        AProcess.Execute;
      finally
        AProcess.Free;
      end;
    end
    else
      ShowMessage('Notifieringsfunktionen √§r inte tillg√§nglig eftersom "notify-send" inte √§r installerat.')// Hantera fallet d√§r notify-send inte √§r installerat
// Alternativt kan du v√§lja att anv√§nda en annan notifieringsmetod eller inaktivera notifieringsfunktionen
    ;
  end;
  {$endif}
  {$if defined(X_WIN)}

  {------------------------------------------------------------------------------
  TrndiNative.PSQuote
  -------------------
  Quote text for PowerShell
 ------------------------------------------------------------------------------}
function PSQuote(const S: UnicodeString): UnicodeString;
begin
  // PowerShell single-quoted literal; escape embedded single quotes
  Result := '''' + StringReplace(S, '''', '''''', [rfReplaceAll]) + '''';
end;

{------------------------------------------------------------------------------
  TrndiNative.GetExePathW
  -------------------
  Get the app's path on Windows as a UTF8 string
 ------------------------------------------------------------------------------}
function GetExePathW: UnicodeString;
var
  Buf: array[0..32767] of WideChar;
  Len: DWORD;
begin
  Len := GetModuleFileNameW(0, @Buf[0], Length(Buf));
  SetString(Result, PWideChar(@Buf[0]), Len);
end;

{------------------------------------------------------------------------------
  TrndiNative.GetEnvVarW
  -------------------
  Get the PATH on Windows as a UTF8 string
 ------------------------------------------------------------------------------}
function GetEnvVarW(const Name: UnicodeString): UnicodeString;
var
  Buf: array[0..32767] of WideChar;
  Len: DWORD;
begin
  Len := GetEnvironmentVariableW(PWideChar(Name), @Buf[0], Length(Buf));
  if Len = 0 then
    Result := ''
  else
    SetString(Result, PWideChar(@Buf[0]), Len);
end;

{------------------------------------------------------------------------------
  TrndiNative.SendNotification
  -------------------
  Send a notification to the desktop
 ------------------------------------------------------------------------------}
procedure SendNotification(const Title, Msg: UnicodeString);
var
  AppPath, TempDir, TempPng, LogPath: UnicodeString;
  Script, CommandLine: UnicodeString;
  SI: Windows.STARTUPINFOW;
  PI: Windows.PROCESS_INFORMATION;
begin
  AppPath := GetExePathW;
  TempDir := GetEnvVarW('TEMP');
  if (TempDir <> '') and (TempDir[Length(TempDir)] <> '\') then
    TempDir := TempDir + '\';
  TempPng := TempDir + ExtractFileName(ChangeFileExt(AppPath, '')) + '-toast-logo.png';
  LogPath := TempDir + 'burnttoast-error.log';

  // PS script: try to extract icon -> PNG; if any error, write to log and still show toast without logo
  Script :=
    '$ErrorActionPreference = ''Stop''; ' +
    '$log = ' + PSQuote(LogPath) + '; ' +
    'try { ' +
      'Import-Module BurntToast; ' +
      'Add-Type -AssemblyName System.Drawing; ' +
      '$exe = ' + PSQuote(AppPath) + '; ' +
      '$png = ' + PSQuote(TempPng) + '; ' +
      '$ico = [System.Drawing.Icon]::ExtractAssociatedIcon($exe); ' +
      'if ($ico) { ' +
        '$bmp = $ico.ToBitmap(); ' +
        '$bmp2 = New-Object System.Drawing.Bitmap 64,64; ' +
        '$g = [System.Drawing.Graphics]::FromImage($bmp2); ' +
        '$g.Clear([System.Drawing.Color]::Transparent); ' +
        '$g.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic; ' +
        '$g.DrawImage($bmp,0,0,64,64); ' +
        '$bmp2.Save($png, [System.Drawing.Imaging.ImageFormat]::Png); ' +
        '$g.Dispose(); $bmp.Dispose(); $bmp2.Dispose(); $ico.Dispose(); ' +
      '} ' +
      'if (Test-Path $png) { ' +
        'New-BurntToastNotification -AppLogo $png -Text ' + PSQuote(Title) + ', ' + PSQuote(Msg) + '; ' +
      '} else { ' +
        'New-BurntToastNotification -Text ' + PSQuote(Title) + ', ' + PSQuote(Msg) + '; ' +
      '} ' +
    '} catch { ' +
      'try { $_ | Out-String | Set-Content -Path $log -Encoding UTF8 } catch {} ' +
      'New-BurntToastNotification -Text ' + PSQuote(Title) + ', ' + PSQuote(Msg) + '; ' +
    '}';

  CommandLine := 'powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "' + Script + '"';

  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;

  UniqueString(CommandLine);

  if not Windows.CreateProcessW(
    nil,
    PWideChar(CommandLine),
    nil, nil,
    False,
    CREATE_NO_WINDOW,
    nil,
    nil,
    SI,
    PI
  ) then
    RaiseLastOSError
  else
  begin
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

  {$endif}
  {$IF DEFINED(X_MAC)}
  procedure SendNotification(const title, msg: string);
  var
  Notification: NSUserNotification;
begin
  Notification := NSUserNotification.alloc.init;
  Notification.setTitle(NSSTR(Title));
  Notification.setInformativeText(NSSTR(Message));
  NSUserNotificationCenter.defaultUserNotificationCenter.deliverNotification(Notification);
  Notification.release;
end;
  {$endif}
begin
  SendNotification('Trndi', message);
end;

{------------------------------------------------------------------------------
  TrndiNative.request
  -------------------
  Sends a GET or POST request, depending on the "post" parameter.
  Behavior differs by platform. Each platform has its own implementation block:
    - X_MAC uses TNSHTTPSendAndReceive
    - X_WIN uses TWinHTTPClient
    - X_PC (Linux) uses TFPHttpClient with OpenSSL
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
function TrndiNative.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''): string;
var
  res, send: TStringStream;
  headers: TStringList;
  sx: string;
begin
  res := TStringStream.Create('');
  send := TStringStream.Create('');
  headers := TStringList.Create;
  try
    // We use a custom TNSHTTPSendAndReceive
    with TNSHTTPSendAndReceive.Create do
    try
      address := Format('%s/%s', [baseurl, endpoint]);
      if post then
        method := 'POST'
      else
        method := 'GET';

      // If a custom header is provided
      if header <> '' then
        Headers.Add(header);

      // If we have JSON data, we assume it's for POST
      if jsondata <> '' then
      begin
        Headers.Add('Content-Type=application/json');
        if useragent <> '' then
          Headers.Add('User-Agent=' + useragent);

        // Write JSON to send stream
        send.Write(jsondata[1], Length(jsondata));
        Headers.Add('Content-Length=' + IntToStr(send.Size));
      end
      else
      if Length(params) > 0 then
      begin
        // If we have query params, append them
        address := address + '?';
        for sx in params do
          address := address + '&' + sx;
      end;

      // Perform the request
      if SendAndReceive(send, res, headers) then
        Result := Trim(res.DataString)
      else
        Result := '+' + LastErrMsg;
    finally
      Free; // free the TNSHTTPSendAndReceive
    end;
  finally
    res.Free;
    send.Free;
    headers.Free;
  end;
end;

{$ELSEIF DEFINED(X_WIN)}
function TrndiNative.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''): string;
var
  client: TWinHTTPClient;
  sx, address: string;
  headers: array of string;
  hasParams: boolean;
  ResStr: string;
begin
  hasParams := (Length(params) > 0);
  client := TWinHTTPClient.Create(useragent);
  try
    // Construct the full URL
    address := Format('%s/%s', [TrimRightSet(baseurl, ['/']), TrimLeftSet(endpoint, ['/'])]);

    // Add default required headers
    client.AddHeader('User-Agent', useragent);

    // Add optional custom header
    if header <> '' then
    begin
      headers := header.Split(['=']);
      if Length(headers) = 2 then
        client.AddHeader(headers[0], headers[1]);
    end;

    // Handle JSON data or query params
    if jsondata <> '' then
    begin
      client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      client.AddHeader('Accept', 'application/json');
      client.SetRequestBody(jsondata);
    end
    else
    if hasParams then
    begin
      address := address + '?';
      for sx in params do
        address := address + '&' + sx;
    end;

    // Perform the request (GET or POST)
    try
      if post then
        ResStr := client.Post(address)
      else
        ResStr := client.Get(address, params); // TWinHTTPClient supports passing params to Get
    except
      on E: Exception do
      begin
        Result := E.Message;
        Exit;
      end;
    end;

    Result := ResStr;
  finally
    client.Free;
  end;
end;

{$ELSE}
{$IFNDEF DARWIN}
function TrndiNative.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string = '';
const header: string = ''): string;
var
  client:  TFPHttpClient;
  res:     TStringStream;
  sx, address: string;
  headers: array of string;
begin
  client := TFPHttpClient.Create(nil);
  try
    // Set user-agent
    client.AddHeader('User-Agent', useragent);
    address := Format('%s/%s', [baseurl, endpoint]);

    // Add optional custom header
    if header <> '' then
    begin
      headers := header.Split(['=']);
      if Length(headers) = 2 then
        client.AddHeader(headers[0], headers[1]);
    end;

    // If we have JSON data
    if jsondata <> '' then
    begin
      client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
      client.AddHeader('Accept', 'application/json');
      client.RequestBody := TRawByteStringStream.Create(jsondata);
    end
    else
    if Length(params) > 0 then
    begin
      // Build URL with query parameters
      address := address + '?';
      for sx in params do
        address := address + '&' + sx;
    end;

    // Prepare a response stream
    res := TStringStream.Create('');
    try
      // Send GET or POST
      if post then
        client.Post(address, res)
      else
        client.Get(address, res);

      // Return the server response as a string
      Result := res.DataString;
    except
      on E: EHttpClient do
        Result := E.Message;
    end;
  finally
    // Cleanup
    if Assigned(client.RequestBody) then
      client.RequestBody.Free;
    client.Free;
    res.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.GetExePathW
  -------------------
  Returns a char from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TrndiNative.GetCharSetting(const keyname: string; def: char = #0): char;
var
 res: string;
begin
  res := GetSetting(keyname,'');
  if res = '' then
    result := def
  else
    result := res[1];
end;

{------------------------------------------------------------------------------
  TrndiNative.GetExePathW
  -------------------
  Returns a bool from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TrndiNative.GetRootSetting(const keyname: string; def: string = ''): string;
begin
  result := GetSetting(keyname,def, true);
end;

{------------------------------------------------------------------------------
  TrndiNative.GetSetting
  ----------------------
  Platform-specific string retrieval. Returns the default if the key isn't found.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
function TrndiNative.GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
var
  reg: TRegistry;
  key: string;
begin
  key := buildKey(keyname, global);

  Result := def;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('\SOFTWARE\Trndi\') then
      Result := reg.ReadString(key);
  finally
    reg.Free;
  end;
end;

{$ELSEIF DEFINED(X_PC)}
function TrndiNative.GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
var
  key: string;
begin
  key := buildKey(keyname, global);
  if not Assigned(inistore) then
    inistore := TINIFile.Create(GetAppConfigFile(false));
  Result := inistore.ReadString('trndi', key, def);
end;

{$ELSEIF DEFINED(X_MAC)}
function TrndiNative.GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
var
  key: string;
begin
  key := buildKey(keyname, global);
  Result := GetPrefString(key); // macOS-based method
  if Result = '' then
    Result := def;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.GetIntSetting
  -------------------------
  Returns an integer from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TrndiNative.GetIntSetting(const keyname: string; def: integer = -1): integer;
var
  r: string;
begin
  r := GetSetting(keyname, 'fail');

  if not TryStrToInt(r, Result) then
    Result := def;
end;

{------------------------------------------------------------------------------
  TrndiNative.GetFloatSetting
  -------------------------
  Returns a single from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TrndiNative.GetFloatSetting(const keyname: string; def: single = -1): single;
var
  r: string;
  f: TFormatSettings;
begin
  r := GetSetting(keyname, 'fail');

  f := DefaultFormatSettings;
  f.DecimalSeparator := '.';
  result := StrToFloatDef(r, def, f);
end;

{------------------------------------------------------------------------------
  TrndiNative.GetBoolSetting
  -------------------------
  Returns a bool from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TrndiNative.GetBoolSetting(const keyname: string; def: boolean = false): boolean;
var
  r: string;
begin
  r := GetSetting(keyname, '-');
  case r of
    'true': result := true;
    'false': result := false;
    else
      result := def;
  end;
end;

{------------------------------------------------------------------------------
  TrndiNative.SetSetting
  ----------------------
  Stores a TColor value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TrndiNative.SetColorSetting(const keyname: string; val: TColor);
begin
  SetSetting(keyname, IntToStr(Integer(val)));
end;

{------------------------------------------------------------------------------
  TrndiNative.GetBoolSetting
  -------------------------
  Returns a TColor from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function Trndinative.GetColorSetting(const keyname: string; const def: TColor): TColor;
var
  i: Integer;
  s: string;
begin
  s := GetSetting(keyname, '');

  if s.IsEmpty then
    Exit(def);

  if TryStrToInt(s, i) then
    Exit(TColor(i));

  Result := def;
end;

{------------------------------------------------------------------------------
  TrndiNative.SetBoolSetting
  ----------------------
  Stores a bool value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TrndiNative.SetBoolSetting(const keyname: string; const val: boolean);
begin
  if val then
    SetSetting(keyname, 'true')
  else
    SetSetting(keyname, 'false');
end;

{------------------------------------------------------------------------------
  TrndiNative.SetFloatSetting
  ----------------------
  Stores a float value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TrndiNative.SetFloatSetting(const keyname: string; const val: single);
var
  f: TFormatSettings;
begin
    f := DefaultFormatSettings;
    f.DecimalSeparator := '.';
    SetSetting(keyname, FormatFloat('0.00',val,f));
end;

{------------------------------------------------------------------------------
  TrndiNative.SetRootSetting
  ----------------------
  Stores a non-user specific string value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TrndiNative.SetRootSetting(keyname: string; const val: string);
begin
  SetSetting(keyname, val, true);
end;

{------------------------------------------------------------------------------
  TrndiNative.SetSetting
  ----------------------
  Stores a string value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TrndiNative.DeleteRootSetting(keyname: string; const val: string);
begin
  DeleteSetting(keyname, true);
end;

{------------------------------------------------------------------------------
  TrndiNative.DeleteSetting
  -------------------------
  Deletes a stored key/value from platform-specific storage completely.
  - X_MAC sets key to blank
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
procedure TrndiNative.DeleteSetting(const keyname: string; global: boolean = false);
begin
  SetSetting(keyname,'',global);
end;


{$ELSEIF DEFINED(X_WIN)}
procedure TrndiNative.DeleteSetting(const keyname: string; global: boolean = false);
var
  reg: TRegistry;
  key: string;
begin
  key := buildKey(keyname, global);

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', false) then  // false = do not create if missing
    begin
      if reg.ValueExists(key) then
        reg.DeleteValue(key)
      else
        ShowMessage('Value not found: ' + key);
    end
    else
      ShowMessage('Registry path not found.');
  finally
    reg.Free;
  end;
end;


{$ELSEIF DEFINED(X_PC)}
procedure TrndiNative.DeleteSetting(const keyname: string; global: boolean = false);
var
  key: string;
begin
  key := buildKey(keyname, global);
  if not Assigned(inistore) then
    inistore := TINIFile.Create(GetAppConfigFile(false));

  inistore.DeleteKey('trndi', key);
end;
{$ENDIF}


{------------------------------------------------------------------------------
  TrndiNative.SetSetting
  ----------------------
  Stores a string value to platform-specific storage.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
procedure TrndiNative.SetSetting(const keyname: string; const val: string; global: boolean = false);
var
  key: string;
begin
  key := buildKey(keyname, global);
  SetPrefString(key, val); // macOS-based method
end;


{$ELSEIF DEFINED(X_WIN)}
procedure TrndiNative.SetSetting(const keyname: string; const val: string; global: boolean = false);
var
  reg: TRegistry;
  key: string;
begin
  key := buildKey(keyname, global);

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', true) then
      reg.WriteString(key, val)
    else
      ShowMessage('Error saving to registry!');
  finally
    reg.Free;
  end;
end;

{$ELSEIF DEFINED(X_PC)}
procedure TrndiNative.SetSetting(const keyname: string; const val: string; global: boolean = false);
var
  key: string;
begin
  key := buildKey(keyname, global);
  if not Assigned(inistore) then
    inistore := TINIFile.Create(GetAppConfigFile(false));
  inistore.WriteString('trndi', key, val);
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.isDarkMode
  ----------------------
  Determines if the user's system is in "dark mode," per platform.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
class function TrndiNative.isDarkMode: boolean;
begin
  // Typically, AppleInterfaceStyle = 'Dark' if dark mode is active
  Result := Pos('DARK', UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
end;

{$ELSEIF DEFINED(X_WIN)}
class function TrndiNative.isDarkMode: boolean;
const
  regtheme = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  reglight = 'AppsUseLightTheme';
var
  reg: TRegistry;
begin
  Result := false;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(regtheme) and reg.OpenKey(regtheme, false) then
    try
      if reg.ValueExists(reglight) then
        // If AppsUseLightTheme = 0 => dark mode
        Result := (reg.ReadInteger(reglight) = 0);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

{$ELSE}
class function TrndiNative.isDarkMode: boolean;
  // A simplistic Linux approach:
  // Compare luminance of clWindow and clWindowText to guess if it's "dark".
function Brightness(C: TColor): double;
  begin
    // Simple formula for perceived luminance
    Result := (Red(C) * 0.3) + (Green(C) * 0.59) + (Blue(C) * 0.11);
  end;
begin
  // If the background (clWindow) is darker than the text (clWindowText), assume dark mode
  Result := (Brightness(ColorToRGB(clWindow)) < Brightness(ColorToRGB(clWindowText)));
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.getURL (class function)
  -----------------------------------
  A static method for a quick GET request. Returns True if successful, False on error.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
class function TrndiNative.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  send, response: TStringStream;
  headers: TStringList;
  httpClient: TNSHTTPSendAndReceive;
begin
  res := '';
  send := TStringStream.Create('');
  response := TStringStream.Create('');
  headers := TStringList.Create;
  httpClient := TNSHTTPSendAndReceive.Create;
  try
    try
      httpClient.address := url;
      httpClient.method := 'GET';
      headers.Add('User-Agent=' + DEFAULT_USER_AGENT);
      //httpClient. Headers := headers;

      if httpClient.SendAndReceive(send, response, headers) then
      begin
        res := Trim(response.DataString);
        Result := true;
      end
      else
      begin
        res := Trim(httpClient.LastErrMsg);
        Result := false;
      end;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
      end;
    end;
  finally
    httpClient.Free;
    send.Free;
    response.Free;
    headers.Free;
  end;
end;

{$ELSEIF DEFINED(X_WIN)}
class function TrndiNative.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  client: TWinHTTPClient;
  responseStr: string;
begin
  res := '';
  client := TWinHTTPClient.Create(DEFAULT_USER_AGENT);
  try
    try
      responseStr := client.Get(url, []);
      res := responseStr;
      Result := true;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
      end;
    end;
  finally
    client.Free;
  end;
end;

{$ELSE}
// Linux/PC or other
class function TrndiNative.getURL(const url: string; out res: string): boolean;
const
  DEFAULT_USER_AGENT = 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
var
  client: TFPHttpClient;
  responseStream: TStringStream;
begin
  res := '';
  client := TFPHttpClient.Create(nil);
  responseStream := TStringStream.Create('');
  try
    try
      client.AddHeader('User-Agent', DEFAULT_USER_AGENT);
      client.Get(url, responseStream);
      res := Trim(responseStream.DataString);
      Result := true;
    except
      on E: Exception do
      begin
        res := E.Message;
        Result := false;
      end;
    end;
  finally
    client.Free;
    responseStream.Free;
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------
  TrndiNative.HasDangerousChars
  ----------------------
  Detects chars which the console is not fond of
 ------------------------------------------------------------------------------}
class function TrndiNative.HasDangerousChars(const FileName: string): Boolean;
function HasCharsInSet(const Str: string; const CharSet: TSysCharSet): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(Str) do
  begin
    if Str[i] in CharSet then
    begin
      Result := True;
      Exit;
    end;
  end;
end;
var
  DangerousChars: TSysCharSet;
begin
  {$IFDEF WINDOWS}
  DangerousChars := ['&', '|', ';', '`', '$', '(', ')', '<', '>', '"', ''''];
  {$ELSE}
  DangerousChars := ['&', '|', ';', '`', '$', '(', ')', '<', '>', '"', '''', '\'];
  {$ENDIF}

  Result := HasCharsInSet(FileName, DangerousChars);
end;

{------------------------------------------------------------------------------
  TrndiNative.DetectWSL
  ----------------------
  Detects if the app is running under Windows Subsystem for Linux
 ------------------------------------------------------------------------------}
class function TrndiNative.DetectWSL: TWSLInfo;
var
  Output: TStringList;
  Content: string;
  EnvVar: string;
begin
  // Initiera resultat
  Result.IsWSL := False;
  Result.Version := wslNone;
  Result.DistroName := '';
  Result.KernelVersion := '';

  {$IFDEF LINUX}
  // Kontrollera /proc/version
  if FileExists('/proc/version') then
  begin
    Output := TStringList.Create;
    try
      Output.LoadFromFile('/proc/version');
      if Output.Count > 0 then
      begin
        Content := Output[0];
        Result.KernelVersion := Content;

        Content := LowerCase(Content);
        if Pos('microsoft', Content) > 0 then
        begin
          Result.IsWSL := True;
          if Pos('wsl2', Content) > 0 then
            Result.Version := wslVersion2
          else
            Result.Version := wslVersion1;
        end
        else if Pos('wsl', Content) > 0 then
        begin
          Result.IsWSL := True;
          Result.Version := wslVersion2;
        end;
      end;
    finally
      Output.Free;
    end;
  end;

  // Kontrollera milj√∂variabler
  EnvVar := GetEnvironmentVariable('WSL_DISTRO_NAME');
  if EnvVar <> '' then
  begin
    Result.IsWSL := True;
    Result.DistroName := EnvVar;
    if Result.Version = wslNone then
      Result.Version := wslUnknown;
  end;

  // WSL_INTEROP (WSL2 specifik)
  if GetEnvironmentVariable('WSL_INTEROP') <> '' then
  begin
    Result.IsWSL := True;
    if Result.Version = wslNone then
      Result.Version := wslVersion2;
  end;

  // Ytterligare kontroller om vi inte hittat WSL √§n
  if not Result.IsWSL then
  begin
    // Kontrollera /proc/sys/kernel/osrelease
    if FileExists('/proc/sys/kernel/osrelease') then
    begin
      Output := TStringList.Create;
      try
        Output.LoadFromFile('/proc/sys/kernel/osrelease');
        if Output.Count > 0 then
        begin
          Content := LowerCase(Output[0]);
          if (Pos('microsoft', Content) > 0) or
             (Pos('wsl', Content) > 0) then
          begin
            Result.IsWSL := True;
            Result.Version := wslUnknown;
          end;
        end;
      finally
        Output.Free;
      end;
    end;
  end;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TrndiNative.IsBurntToastAvailable
  ----------------------
  Checks if the PowerShell BurntToast module is installed
 ------------------------------------------------------------------------------}
function IsBurntToastAvailable: Boolean;
var
  Output: TStringList;
  AProcess: TProcess;
begin
  Result := False;
  Output := TStringList.Create;
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'powershell';
    AProcess.Parameters.Add('-NoProfile');
    AProcess.Parameters.Add('-Command');

    AProcess.Parameters.Add('if (Get-Module -ListAvailable -Name BurntToast) { Write-Host "YES" }');
    AProcess.Options := [poUsePipes, poWaitOnExit];

    AProcess.Execute;
    Output.LoadFromStream(AProcess.Output);

    Result := (Pos('YES', Output.Text) > 0);
  finally
    AProcess.Free;
    Output.Free;
  end;
end;
end.

