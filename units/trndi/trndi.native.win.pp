(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2026 Björn Lindh.
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
Classes, SysUtils, Graphics, Windows, Registry, Dialogs, StrUtils,
winhttpclient, shellapi,
Forms, variants, dwmapi, trndi.native.base, ExtCtrls;

type
  {**
    @abstract(Windows implementation of @link(TTrndiNativeBase).)
    Uses SAPI for speech and DWM for window appearance tweaks.
  }
TTrndiNativeWindows = class(TTrndiNativeBase)
private
  FFlashTimer: TTimer;
  FFlashEnd: TDateTime;
  FFlashPhase: integer;
  FFlashValue: string;
  FFlashBaseColor: TColor;
  FFlashCycleMS: integer;
  procedure FlashTimerTick(Sender: TObject);
public
    {** Speaks @param(Text) using SAPI; falls back to default voice if a
        locale-matching voice is not found. }
  procedure Speak(const Text: string); override;
    {** Toggles immersive dark mode for @param(win).
        Requires Windows 10 1809+ (build >= 17763).
        @returns(True if the DWM call succeeds) }
  class function SetDarkMode(win: HWND; Enable: boolean = true): boolean;
    {** Applies caption (@param(bg)) and text (@param(text)) colors via DWM.
        @returns(True if both attributes are set successfully) }
  class function SetTitleColor(form: THandle; bg, Text: TColor): boolean; override;
    {** Draw a badge with @param(Value) on the application icon.
        @param(BadgeColor Color of the badge circle/rounded rect)
        @param(badge_size_ratio Badge diameter relative to icon size)
        @param(min_font_size Minimum font size while fitting text) }
  procedure SetBadge(const Value: string; BadgeColor: TColor;
    badge_size_ratio: double; min_font_size: integer); override;
  procedure StartBadgeFlash(const Value: string; badgeColor: TColor;
    DurationMS: integer = 10000; CycleMS: integer = 400); override;
  procedure StopBadgeFlash; override;
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
    {** Check whether platform TTS is available. }
  class function SpeakAvailable: boolean; override;
    {** Name of the software used for speech on Windows (e.g., 'SAPI'). }
  class function SpeakSoftwareName: string; override;
    {** Best-effort window manager name for Windows. }
  class function GetWindowManagerName: string; override;

  {** Settings API overrides (Windows Registry)
    Keys are stored under HKCU\Software\Trndi\ with the same scoping rules
    used by the base implementation. }
  {** Retrieve a setting from HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(def Default value if not present)
    @param(global If True, use global scope; otherwise per-user)
    @returns(Value if present, otherwise def) }
  function GetSetting(const keyname: string; def: string = '';
    global: boolean = false): string; override;
  {** Persist a setting to HKCU\Software\Trndi\.
    @param(keyname Logical key name; base will prefix with scope)
    @param(val Value to write)
    @param(global If True, use global scope; otherwise per-user) }
  procedure SetSetting(const keyname: string; const val: string;
    global: boolean = false); override;
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
function IsBurntToastAvailable: boolean;
var
  Output: TStringList;
  AProcess: TProcess;
begin
  Result := false;
  Output := TStringList.Create;
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'powershell';
    AProcess.Parameters.Add('-NoProfile');
    AProcess.Parameters.Add('-Command');
    AProcess.Parameters.Add(
      'if (Get-Module -ListAvailable -Name BurntToast) { Write-Host "YES" }');
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
  SpeakAvailable (Windows)
  ------------------------
  Windows supports native TTS via SAPI; assume available.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SpeakAvailable: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  SpeakSoftwareName (Windows)
  ---------------------------
  Name of the speech backend used on Windows.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SpeakSoftwareName: string;
begin
  Result := 'SAPI';
end;

{------------------------------------------------------------------------------
  GetWindowManagerName (Windows)
  ------------------------------
  Return a stable identifier for Windows. No separate window manager process
  is exposed like on X11, so we return a human-friendly value.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.GetWindowManagerName: string;
begin
  Result := 'Windows Desktop';
end; 

procedure TTrndiNativeWindows.FlashTimerTick(Sender: TObject);
var
  phaseColor: TColor;
  factor: double;
begin
  if (Now > FFlashEnd) or (FFlashValue = '') then
  begin
    StopBadgeFlash;
    Exit;
  end;

  // Simple 4-phase pulse: normal -> lighter -> normal -> darker
  case FFlashPhase mod 4 of
  0:
    factor := 1.0;   // base
  1:
    factor := 1.35;  // brighten
  2:
    factor := 1.0;   // base
  3:
    factor := 0.70;  // darken
  else
    factor := 1.0;
  end;

  // Adjust color
  phaseColor := RGB(Min(255, Round(GetRValue(ColorToRGB(FFlashBaseColor)) * factor)),
    Min(255, Round(GetGValue(ColorToRGB(FFlashBaseColor)) * factor)),
    Min(255, Round(GetBValue(ColorToRGB(FFlashBaseColor)) * factor)));

  // Draw badge with pulsed color
  SetBadge(FFlashValue, phaseColor, DEFAULT_BADGE_SIZE_RATIO, DEFAULT_MIN_FONT_SIZE);

  Inc(FFlashPhase);
end;

procedure TTrndiNativeWindows.StartBadgeFlash(const Value: string;
badgeColor: TColor; DurationMS: integer; CycleMS: integer);
begin
  // Initialize or update flashing parameters
  FFlashValue := Value;
  FFlashBaseColor := badgeColor;
  FFlashCycleMS := CycleMS;
  FFlashEnd := Now + (DurationMS / (24 * 60 * 60 * 1000)); // ms to TDateTime
  FFlashPhase := 0;

  if FFlashTimer = nil then
  begin
    FFlashTimer := TTimer.Create(nil);
    FFlashTimer.OnTimer := @FlashTimerTick;
  end;
  FFlashTimer.Interval := CycleMS;
  FFlashTimer.Enabled := true;

  // Immediate first frame
  FlashTimerTick(nil);
end;

procedure TTrndiNativeWindows.StopBadgeFlash;
begin
  if Assigned(FFlashTimer) then
  begin
    FFlashTimer.Enabled := false;
    FreeAndNil(FFlashTimer);
  end;
  // Restore static badge with base color if we still have a value
  if FFlashValue <> '' then
    SetBadge(FFlashValue, FFlashBaseColor, DEFAULT_BADGE_SIZE_RATIO,
      DEFAULT_MIN_FONT_SIZE);
  FFlashValue := '';
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
DWMWA_TEXT_COLOR = 36;
DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

{------------------------------------------------------------------------------
  Speak
  -----
  Use SAPI to speak text; pick a voice matching user locale when possible.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.Speak(const Text: string);
var
  Voice, Voices: olevariant;
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
class function TTrndiNativeWindows.SetDarkMode(win: HWND;
Enable: boolean = true): boolean;
var
  Value: integer;
begin
  Result := false;
  // Check Windows version (Windows 10 1809+ required for immersive dark mode)
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and
    (Win32BuildNumber < 17763)) then
    Exit; // Windows 10 1809 (build 17763)

  Value := Ord(Enable);
  Result := Succeeded(DwmSetWindowAttribute(win, DWMWA_USE_IMMERSIVE_DARK_MODE,
    @Value, SizeOf(Value)));
end;

function SetDwmAttr(hWnd: HWND; Attr: DWORD; const Data; Size: DWORD): HRESULT;
begin
  Result := DwmSetWindowAttribute(hWnd, Attr, @Data, Size);
end;

function HrSucceeded(hr: HRESULT): boolean; inline;
begin
  Result := hr >= 0; // SUCCEEDED(hr)
end;

{------------------------------------------------------------------------------
  SetTitleColor
  -------------
  Apply caption and text colors to a window using DWM attributes.
 ------------------------------------------------------------------------------}
class function TTrndiNativeWindows.SetTitleColor(form: THandle;
bg, Text: TColor): boolean;
const
  MIN_DWM_COLOR_BUILD = 17763; // Win10 1809 (2018-10)
var
  bgColor, textColor: COLORREF;
  hrCaption, hrText: HRESULT;
begin
  // Guard: DWMWA_CAPTION_COLOR (35) & DWMWA_TEXT_COLOR (36) are supported from
  // Windows 10 1809 (build 17763, Oct 2018). Earlier versions will just fail.
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and
    (Win32BuildNumber < MIN_DWM_COLOR_BUILD)) then
    Exit(false);

  // TColor and COLORREF share 0x00BBGGRR layout; no byte swap required.
  bgColor := COLORREF(ColorToRGB(bg));
  textColor := COLORREF(ColorToRGB(Text));

  hrCaption := SetDwmAttr(form, DWMWA_CAPTION_COLOR, bgColor, SizeOf(bgColor));
  hrText := SetDwmAttr(form, DWMWA_TEXT_COLOR, textColor, SizeOf(textColor));

  Result := HrSucceeded(hrCaption) and HrSucceeded(hrText);
end;

{------------------------------------------------------------------------------
  SetBadge
  --------
  Compose app icon with a badge showing Value; applies to taskbar icon.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetBadge(const Value: string; BadgeColor: TColor;
badge_size_ratio: double; min_font_size: integer);
const
  INITIAL_FONT_SIZE_RATIO = 0.5;
  TEXT_PADDING = 4;
  CORNER_RADIUS = 6;
var
  AppIcon, TempIcon: TIcon;
  Bitmap: Graphics.TBitmap;
  BadgeText: string;
  TextWidth, TextHeight: integer;
  BadgeRect: Classes.TRect;
  IconWidth, IconHeight, BadgeSize: integer;
  FontSize, Radius: integer;
  TextColor: TColor;
  SavedDC: integer;
  Region, SquareRegion: HRGN;
  RgnRect: Classes.TRect;
  dval: double;
  // New styling helpers
  highlightColor, shadowColor, borderColor: TColor;
  y: integer;
  t: double;
  lineColor: TColor;

function Luminance(c: TColor): double;
  var
    rc: longint;
    r, g, b: byte;
  begin
    rc := ColorToRGB(c);
    r := GetRValue(rc);
    g := GetGValue(rc);
    b := GetBValue(rc);
    Result := 0.299 * r + 0.587 * g + 0.114 * b;
  end;

function AdjustColor(c: TColor; factor: double): TColor;
  var
    rc: longint;
    r, g, b: integer;
  begin
    rc := ColorToRGB(c);
    r := Round(GetRValue(rc) * factor);
    if r > 255 then
      r := 255;
    g := Round(GetGValue(rc) * factor);
    if g > 255 then
      g := 255;
    b := Round(GetBValue(rc) * factor);
    if b > 255 then
      b := 255;
    Result := RGB(r, g, b);
  end;

function Blend(a, b: TColor; tt: double): TColor;
  var
    ar, ag, ab, br, bg, bb: byte;
    rc1, rc2: longint;
    r, g, _b: integer;
  begin
    if tt < 0 then
      tt := 0
    else
    if tt > 1 then
      tt := 1;
    rc1 := ColorToRGB(a);
    rc2 := ColorToRGB(b);
    ar := GetRValue(rc1);
    ag := GetGValue(rc1);
    ab := GetBValue(rc1);
    br := GetRValue(rc2);
    bg := GetGValue(rc2);
    bb := GetBValue(rc2);
    r := Round(ar + (br - ar) * tt);
    g := Round(ag + (bg - ag) * tt);
    _b := Round(ab + (bb - ab) * tt);
    Result := RGB(r, g, _b);
  end;

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
      if TryStrToFloat(Value, dval, fsettings) then
        BadgeText := FormatFloat('0.0', dval, fsettings)
      else
        BadgeText := Value;
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

    DrawIconEx(Bitmap.Canvas.Handle, 0, 0, AppIcon.Handle, IconWidth,
      IconHeight, 0, 0, DI_NORMAL);

    // Compute a badge rectangle in the lower-right quadrant with size
    // proportional to the current icon dimensions.
    BadgeRect := Classes.Rect(IconWidth - BadgeSize, IconHeight -
      BadgeSize, IconWidth, IconHeight);

    // Pre-compute styling colors (light top highlight, dark shadow bottom, and border)
    // Use luminance to decide highlight/shadow intensity for contrast.
    highlightColor := AdjustColor(BadgeColor, 1.3); // 30% lighter
    shadowColor := AdjustColor(BadgeColor, 0.6);    // 40% darker
    // Border color: choose darker or lighter depending on base luminance
    if Luminance(BadgeColor) > 140 then
      borderColor := AdjustColor(BadgeColor, 0.55)
    else
      borderColor := AdjustColor(BadgeColor, 1.35);

    Bitmap.Canvas.Brush.Color := BadgeColor;
    Bitmap.Canvas.Pen.Color := borderColor;

    if BadgeSize <= 12 then
      Bitmap.Canvas.FillRect(BadgeRect)// Tiny badges: simple square for speed/stability
// simple tiny badge (no extra effects)

    else
    begin
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then
        Radius := 2;

      // Clip drawing to a rounded rectangle region to get smooth corners.
      // Use SaveDC/RestoreDC to avoid leaking the clipping region state.
      SavedDC := SaveDC(Bitmap.Canvas.Handle);
      Region := 0;
      SquareRegion := 0;
      try
        RgnRect := BadgeRect;
        // Build a rounded-rect clipping region to paint smooth corners
        Region := CreateRoundRectRgn(RgnRect.Left, RgnRect.Top,
          RgnRect.Right, RgnRect.Bottom, Radius * 2, Radius * 2);
        // Expand the rounded region with a square piece to smooth the corner join
        SquareRegion := CreateRectRgn(RgnRect.Right - Radius,
          RgnRect.Bottom - Radius, RgnRect.Right, RgnRect.Bottom);
        CombineRgn(Region, Region, SquareRegion, RGN_OR);
        SelectClipRgn(Bitmap.Canvas.Handle, Region);

        // Custom gradient fill (vertical) since LCL GradientFill may not exist everywhere.
        for y := BadgeRect.Top to BadgeRect.Bottom - 1 do
        begin
          t := (y - BadgeRect.Top) / (BadgeRect.Bottom - BadgeRect.Top - 1);
          // Bias t slightly so highlight band is thinner
          lineColor := Blend(highlightColor, shadowColor, t * 0.85);
          Bitmap.Canvas.Pen.Color := lineColor;
          Bitmap.Canvas.MoveTo(BadgeRect.Left, y);
          Bitmap.Canvas.LineTo(BadgeRect.Right, y);
        end;

        // Draw an inner rounded outline for bevel illusion (light top-left, dark bottom-right)
        Bitmap.Canvas.Pen.Style := psSolid;
        Bitmap.Canvas.Pen.Width := 1;
        // Top/left bevel
        Bitmap.Canvas.Pen.Color := highlightColor;
        RoundRect(Bitmap.Canvas.Handle,
          BadgeRect.Left, BadgeRect.Top,
          BadgeRect.Right, BadgeRect.Bottom,
          Radius * 2, Radius * 2);
        // Bottom/right bevel overlay using shadow color (draw partial arcs/lines)
        Bitmap.Canvas.Pen.Color := shadowColor;
        // simple approach: inset rectangle to avoid overwriting highlight too much
        InflateRect(BadgeRect, -1, -1);
        RoundRect(Bitmap.Canvas.Handle,
          BadgeRect.Left, BadgeRect.Top,
          BadgeRect.Right, BadgeRect.Bottom,
          Radius * 2 - 2, Radius * 2 - 2);
        InflateRect(BadgeRect, 1, 1); // restore
      finally
        if SquareRegion <> 0 then
          DeleteObject(SquareRegion);
        if Region <> 0 then
          DeleteObject(Region);
        RestoreDC(Bitmap.Canvas.Handle, SavedDC);
      end;
    end;

    // Outer border (rounded) for readability on bright or cluttered taskbars
    if BadgeSize > 10 then
    begin
      Bitmap.Canvas.Pen.Color := borderColor;
      Bitmap.Canvas.Brush.Style := bsClear;
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then
        Radius := 2;
      RoundRect(Bitmap.Canvas.Handle,
        BadgeRect.Left, BadgeRect.Top,
        BadgeRect.Right, BadgeRect.Bottom,
        Radius * 2, Radius * 2);
    end;

    // Choose text color based on perceived luminance for contrast.
    if (0.299 * GetRValue(BadgeColor) + 0.587 * GetGValue(BadgeColor) +
      0.114 * GetBValue(BadgeColor)) > 128 then
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
    SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_BIG,
      Application.Icon.Handle);
    SendMessage(Application.MainForm.Handle, WM_SETICON, ICON_SMALL,
      Application.Icon.Handle);
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
function TTrndiNativeWindows.GetSetting(const keyname: string; def: string;
global: boolean): string;
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
      if reg.ValueExists(key) then
        Result := reg.ReadString(key)
      else
        Result := def;
  finally
    reg.Free;
  end;
end;

{------------------------------------------------------------------------------
  SetSetting
  ----------
  Write a value to HKCU\Software\Trndi\.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetSetting(const keyname: string;
const val: string; global: boolean);
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
      if reg.ValueExists(key) then
        reg.DeleteValue(key);
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
