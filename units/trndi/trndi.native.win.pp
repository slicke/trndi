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
 *
 * GitHub: https://github.com/slicke/trndi
 *)
unit trndi.native.win;

{**
  @abstract(Windows-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeWindows) which derives from
  @link(TTrndiNativeBase) and implements behaviors that require Windows APIs
  (SAPI for TTS, DWM for caption colors and immersive dark mode).

  Consumers should use the façade unit @code(trndi.native), which exposes the
  alias @code(TrndiNative) to the correct platform class at compile time.

  @bold(Key responsibilities)
  - Text-to-speech using SAPI (@link(TTrndiNativeWindows.Speak))
  - Toggle immersive dark mode on a window (@link(TTrndiNativeWindows.SetDarkMode))
  - Set window caption and text colors (@link(TTrndiNativeWindows.SetTitleColor))
  - Simple HTTP GET via WinHTTP (@link(TTrndiNativeWindows.getURL))
  - Persist settings in Windows Registry (@link(TTrndiNativeWindows.GetSetting))

  @seealso(TTrndiNativeBase)
}

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Graphics, Windows, Registry, Dialogs, StrUtils, winhttpclient, shellapi,
  Forms, variants, dwmapi, trndi.native.base;

type
  {**
    @abstract(Windows implementation of @link(TTrndiNativeBase).)
    Uses SAPI for speech and DWM for window appearance tweaks.
  }
  TTrndiNativeWindows = class(TTrndiNativeBase)
  public
    {** Speaks @param(Text) using SAPI; falls back to default voice if a
        locale-matching voice is not found. }
    procedure Speak(const Text: string); override;
    {** Toggles immersive dark mode for @param(win).
        Requires Windows 10 1809+ (build >= 17763).
        @returns(True if the DWM call succeeds) }
  class function SetDarkMode(win: HWND; Enable: Boolean = True): Boolean;
    {** Applies caption (@param(bg)) and text (@param(text)) colors via DWM.
        @returns(True if both attributes are set successfully) }
  class function SetTitleColor(form: THandle; bg, text: TColor): boolean; override;
    {** Draw a badge with @param(Value) on the application icon.
        @param(BadgeColor Color of the badge circle/rounded rect)
        @param(badge_size_ratio Badge diameter relative to icon size)
        @param(min_font_size Minimum font size while fitting text) }
  procedure SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer); override;
  {** Simple HTTP GET using WinHTTP client with default UA.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
  class function getURL(const url: string; out res: string): boolean; override;
  {** Determine if Windows is using dark app theme (AppsUseLightTheme=0).
      @returns(True if dark mode is active) }
  class function isDarkMode: boolean; override;
  {** Returns True if the BurntToast PowerShell module is available.
    This is used as a proxy for whether native toast notifications can be sent
    via PowerShell from this process. }
  class function isNotificationSystemAvailable: boolean; override;
  {** Identify the notification backend for Windows.
      Returns 'BurntToast' when the PowerShell module is available; otherwise 'none'. }
  class function getNotificationSystem: string; override;

  {** Settings API overrides (Windows Registry)
    Keys are stored under HKCU\Software\Trndi\ with the same scoping rules
    used by the base implementation. }
  {** Retrieve a setting from HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(def Default value if not present)
    @param(global If True, use global scope; otherwise per-user)
    @returns(Value if present, otherwise def) }
    function GetSetting(const keyname: string; def: string = ''; global: boolean = false): string; override;
  {** Persist a setting to HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(val Value to write)
    @param(global If True, use global scope; otherwise per-user) }
    procedure SetSetting(const keyname: string; const val: string; global: boolean = false); override;
  {** Delete a setting from HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(global If True, use global scope; otherwise per-user) }
    procedure DeleteSetting(const keyname: string; global: boolean = false); override;
  {** Refresh settings cache, if any. Registry access is on-demand here,
    so nothing needs to be reloaded. }
    procedure ReloadSettings; override;
  end;

implementation

uses
  ComObj, Process;
{------------------------------------------------------------------------------
  IsBurntToastAvailable
  ---------------------
  Check if the BurntToast PowerShell module is available on this system.
  Used by isNotificationSystemAvailable as a proxy for toast support.
 ------------------------------------------------------------------------------}
{** Check if the BurntToast module is available in PowerShell.
    Implementation detail for @link(TTrndiNativeWindows.isNotificationSystemAvailable). }
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

{------------------------------------------------------------------------------
  isNotificationSystemAvailable
  -----------------------------
  Returns True if native toast notifications are likely available (BurntToast).
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.isNotificationSystemAvailable: boolean;
begin
  Result := IsBurntToastAvailable;
end;

{------------------------------------------------------------------------------
  getNotificationSystem
  ---------------------
  Return 'BurntToast' if the PowerShell module is present; otherwise 'none'.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.getNotificationSystem: string;
begin
  if IsBurntToastAvailable then
    Result := 'BurntToast'
  else
    Result := 'none';
end;


{------------------------------------------------------------------------------
  isDarkMode
  ----------
  Detect Windows App theme: AppsUseLightTheme = 0 means dark mode for apps.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.isDarkMode: boolean;
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
      // AppsUseLightTheme = 0 means dark mode for apps is enabled
      if reg.ValueExists(reglight) then
        Result := (reg.ReadInteger(reglight) = 0);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

{$ifdef Windows}
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
{$endif}

{ DWM attribute constants used for caption and text colors, and dark mode }
const
  DWMWA_CAPTION_COLOR = 35;
  DWMWA_TEXT_COLOR    = 36;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

{------------------------------------------------------------------------------
  Speak
  -----
  Use SAPI to speak text; pick a voice matching user locale when possible.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.Speak(const Text: string);
var
  Voice, Voices: OleVariant;
  lang: LANGID;
  LangHex: string;
begin
  // Use SAPI (COM-based) text-to-speech. We try to pick a voice matching the
  // user locale; if none are available, the default SAPI voice is used.
  // Note: This call is synchronous and will block the calling thread until
  // speech completes unless SAPI is configured for async. Keep messages short
  // or dispatch from a background thread when necessary.
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

{------------------------------------------------------------------------------
  getURL
  ------
  Simple HTTP GET using WinHTTP client with a default User-Agent.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.getURL(const url: string; out res: string): boolean;
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

{------------------------------------------------------------------------------
  SetDarkMode
  -----------
  Toggle immersive dark mode for a window (Windows 10 1809+ required).
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SetDarkMode(win: HWND; Enable: Boolean = True): Boolean;
var
  Value: Integer;
begin
  Result := False;
  // Check Windows version (Windows 10 1809+ required for immersive dark mode)
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and (Win32BuildNumber < 17763)) then
    Exit; // Windows 10 1809 (build 17763)

  Value := Ord(Enable);
  Result := Succeeded(
    DwmSetWindowAttribute(win, DWMWA_USE_IMMERSIVE_DARK_MODE, @Value, SizeOf(Value))
  );
end;

function SetDwmAttr(hWnd: HWND; Attr: DWORD; const Data; Size: DWORD): HRESULT;
begin
  Result := DwmSetWindowAttribute(hWnd, Attr, @Data, Size);
end;

function HrSucceeded(hr: HRESULT): Boolean; inline;
begin
  Result := hr >= 0; // SUCCEEDED(hr)
end;

{------------------------------------------------------------------------------
  SetTitleColor
  -------------
  Apply caption and text colors to a window using DWM attributes.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SetTitleColor(form: THandle; bg, text: TColor): Boolean;
const
  MIN_DWM_COLOR_BUILD = 17763; // Win10 1809 (2018-10)
var
  bgColor, textColor: COLORREF;
  hrCaption, hrText: HRESULT;
begin
  // Guard: DWMWA_CAPTION_COLOR (35) & DWMWA_TEXT_COLOR (36) are supported from
  // Windows 10 1809 (build 17763, Oct 2018). Earlier versions will just fail.
  if (Win32MajorVersion < 10) or
     ((Win32MajorVersion = 10) and (Win32BuildNumber < MIN_DWM_COLOR_BUILD)) then
    Exit(False);

  // TColor and COLORREF share 0x00BBGGRR layout; no byte swap required.
  bgColor   := COLORREF(ColorToRGB(bg));
  textColor := COLORREF(ColorToRGB(text));

  hrCaption := SetDwmAttr(form, DWMWA_CAPTION_COLOR, bgColor, SizeOf(bgColor));
  hrText    := SetDwmAttr(form, DWMWA_TEXT_COLOR, textColor, SizeOf(textColor));

  Result := HrSucceeded(hrCaption) and HrSucceeded(hrText);
end;

{------------------------------------------------------------------------------
  SetBadge
  --------
  Compose app icon with a badge showing Value; applies to taskbar icon.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer);
const
  INITIAL_FONT_SIZE_RATIO = 0.5;
  TEXT_PADDING = 4;
  CORNER_RADIUS = 6;
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
  AppIcon := TIcon.Create;
  TempIcon := TIcon.Create;
  Bitmap := Graphics.TBitmap.Create;
  try
    if Value = '' then
    begin
      Application.Icon.Assign(Application.MainForm.Icon);
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG, 0);
      SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL, 0);
      Exit;
    end;

    try
      if TryStrToFloat(value, dval, fsettings) then
        BadgeText := FormatFloat('0.0', dval, fsettings)
      else
        BadgeText := value;
    except
      BadgeText := Value;
    end;

    AppIcon.Assign(Application.Icon);
    IconWidth := AppIcon.Width;
    IconHeight := AppIcon.Height;
    if (IconWidth <= 0) or (IconHeight <= 0) then
    begin
      IconWidth := 32;
      IconHeight := 32;
    end;

    if IconWidth < IconHeight then
      BadgeSize := Round(IconWidth * badge_size_ratio)
    else
      BadgeSize := Round(IconHeight * badge_size_ratio);

    Bitmap.SetSize(IconWidth, IconHeight);
    Bitmap.Canvas.Brush.Color := clNone;
    // Note: Use Classes.Rect/TRect explicitly to avoid Windows unit name clashes
    Bitmap.Canvas.FillRect(Classes.Rect(0, 0, IconWidth, IconHeight));

    DrawIconEx(Bitmap.Canvas.Handle, 0, 0, AppIcon.Handle, IconWidth, IconHeight, 0, 0, DI_NORMAL);

    // Compute a badge rectangle in the lower-right quadrant with size
    // proportional to the current icon dimensions.
    BadgeRect := Classes.Rect(
      IconWidth - BadgeSize,
      IconHeight - BadgeSize,
      IconWidth,
      IconHeight
    );

    Bitmap.Canvas.Brush.Color := BadgeColor;
    Bitmap.Canvas.Pen.Color := BadgeColor;

    if BadgeSize <= 12 then
    begin
      // Tiny badges: simple square for speed/stability
      Bitmap.Canvas.FillRect(BadgeRect);
    end
    else
    begin
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then
        Radius := 2;

      // Clip drawing to a rounded rectangle region to get smooth corners.
      // Use SaveDC/RestoreDC to avoid leaking the clipping region state.
      SavedDC := SaveDC(Bitmap.Canvas.Handle);
      Region := 0; SquareRegion := 0;
      try
        RgnRect := BadgeRect;
        // Build a rounded-rect clipping region to paint smooth corners
        Region := CreateRoundRectRgn(
          RgnRect.Left, RgnRect.Top,
          RgnRect.Right, RgnRect.Bottom,
          Radius * 2, Radius * 2
        );
        // Expand the rounded region with a square piece to smooth the corner join
        SquareRegion := CreateRectRgn(
          RgnRect.Right - Radius, RgnRect.Bottom - Radius,
          RgnRect.Right, RgnRect.Bottom
        );
        CombineRgn(Region, Region, SquareRegion, RGN_OR);
        SelectClipRgn(Bitmap.Canvas.Handle, Region);
        Bitmap.Canvas.FillRect(BadgeRect);
      finally
        if SquareRegion <> 0 then DeleteObject(SquareRegion);
        if Region <> 0 then DeleteObject(Region);
        RestoreDC(Bitmap.Canvas.Handle, SavedDC);
      end;
    end;

    // Choose text color based on perceived luminance for contrast.
    if (0.299 * GetRValue(BadgeColor) + 0.587 * GetGValue(BadgeColor) + 0.114 * GetBValue(BadgeColor)) > 128 then
      TextColor := clBlack
    else
      TextColor := clWhite;

    Bitmap.Canvas.Font.Name := 'Arial';
    Bitmap.Canvas.Font.Style := [fsBold];
    Bitmap.Canvas.Font.Color := TextColor;
    FontSize := Round(BadgeSize * INITIAL_FONT_SIZE_RATIO);
    if FontSize < min_font_size then
      FontSize := min_font_size;
    Bitmap.Canvas.Font.Size := FontSize;

    TextWidth := Bitmap.Canvas.TextWidth(BadgeText);
    TextHeight := Bitmap.Canvas.TextHeight(BadgeText);
    // Fit text within the badge; avoid shrinking below the requested minimum.
    while (TextWidth > (BadgeSize - TEXT_PADDING)) and (FontSize > min_font_size - 2) do
    begin
      Dec(FontSize);
      Bitmap.Canvas.Font.Size := FontSize;
      TextWidth := Bitmap.Canvas.TextWidth(BadgeText);
      TextHeight := Bitmap.Canvas.TextHeight(BadgeText);
    end;

    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.TextOut(
      BadgeRect.Left + ((BadgeRect.Right - BadgeRect.Left) - TextWidth) div 2,
      BadgeRect.Top + ((BadgeRect.Bottom - BadgeRect.Top) - TextHeight) div 2,
      BadgeText
    );

    // Assign the composed bitmap to the app icon and notify the window.
    TempIcon.Assign(Bitmap);
    Application.Icon.Assign(TempIcon);
    SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG, Application.Icon.Handle);
    SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL, Application.Icon.Handle);
  finally
    Bitmap.Free;
    AppIcon.Free;
    TempIcon.Free;
  end;
end;

{------------------------------------------------------------------------------
  GetSetting
  ----------
  Read a value from HKCU\Software\Trndi\; returns def if not present.
 ------------------------------------------------------------------------------}
function TTrndiNativeWindows.GetSetting(const keyname: string; def: string; global: boolean): string;
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
    begin
      if reg.ValueExists(key) then
        Result := reg.ReadString(key)
      else
        Result := def;
    end;
  finally
    reg.Free;
  end;
end;

{------------------------------------------------------------------------------
  SetSetting
  ----------
  Write a value to HKCU\Software\Trndi\.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetSetting(const keyname: string; const val: string; global: boolean);
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
      ;
  finally
    reg.Free;
  end;
end;

{------------------------------------------------------------------------------
  DeleteSetting
  -------------
  Delete a value from HKCU\Software\Trndi\ if it exists.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.DeleteSetting(const keyname: string; global: boolean);
var
  reg: TRegistry;
  key: string;
begin
  key := buildKey(keyname, global);
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\Trndi\', false) then
    begin
      if reg.ValueExists(key) then
        reg.DeleteValue(key);
    end;
  finally
    reg.Free;
  end;
end;

{------------------------------------------------------------------------------
  ReloadSettings
  --------------
  No-op for registry-backed settings (access is on-demand).
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.ReloadSettings;
begin
  // Registry access is performed on demand; there is no persistent
  // handle or cache to refresh here.
end;

end.
