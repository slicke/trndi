(*
 * This file is part of Trndi (https://github.com/slicke/trndi or http://xxx.github.io).
 * Copyright (c) 2021-25 Björn Lindh.
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

unit trndi.native.base;

{$I ../../inc/native.inc} // Depending on your project setup, this may define X_WIN, X_PC, X_MAC, etc.

interface

{ Note (2025-09-19):
  This unit now exposes a base class (TTrndiNativeBase) and platform-specific subclasses:
    - TTrndiNativeWindows, TTrndiNativeLinux, TTrndiNativeMac
  For backwards compatibility, the public type name TrndiNative is defined in unit `trndi.native`
  as an alias that resolves to the appropriate subclass at compile time. }

uses
  Classes, SysUtils, Graphics
{$IF DEFINED(X_MAC)}
  , NSMisc, ns_url_request, CocoaAll, LCLType
{$ELSEIF DEFINED(X_WIN)}
  , Windows, Registry, Dialogs, StrUtils, winhttpclient, shellapi, comobj,
    Forms, variants, dwmapi
{$ELSEIF DEFINED(X_PC)}
  , fphttpclient, openssl, opensslsockets, IniFiles, Dialogs, LCLType
{$ENDIF}
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
TTrndiNativeBase = class
private
  cfguser:   string;  // User prefix for config

  function buildKey(const key: string; global: boolean): string;
  procedure updateLocale(const l: TFormatSettings);
protected
  fsettings: TFormatSettings;
  // HTTP defaults
  useragent: string;
  baseurl:   string;
  {$IF DEFINED(X_PC)}
  inistore: TINIFile; // Linux/PC settings store
  {$ENDIF}
public
  // Config
  noFree: boolean;
  noticeDuration: integer;
  class var touchOverride: TTrndiBool;
  // Indicates if the user system is in a "dark mode" theme
  dark: boolean;

  // Core actions
  procedure Speak(const Text: string); virtual; abstract;
  procedure attention(topic, message: string); virtual;
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''): string;

  // Settings API
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
  procedure ReloadSettings;

  // Theme/Env
  class function isDarkMode: boolean;
  class function DetectTouchScreen(out multi: boolean): boolean;
  class function HasTouchScreen(out multi: boolean): boolean;
  class function HasTouchScreen: boolean;
  class function getURL(const url: string; out res: string): boolean; static;
  class function GetOSLanguage: string;
  class function HasDangerousChars(const FileName: string): Boolean; static;
  class function DetectWSL: TWSLInfo;
  // Notifications
  class function isNotificationSystemAvailable: boolean; static;

  // Lifecycle and UI
  destructor Destroy; override;
  procedure start;
  procedure done;
  // Badge: provide a convenience overload and a virtual full version
  procedure setBadge(const Value: string; badgeColor: TColor); overload;
  procedure setBadge(const Value: string; badgeColor: TColor; badge_size_ratio: Double; min_font_size: Integer); virtual; overload;
  function SetTitleColor(form: THandle; bg, text: TColor): boolean; virtual;
  class procedure PlaySound(const FileName: string);

  // Constructors
  constructor create(ua, base: string); overload;
  constructor create(ua: string); overload;
  constructor create; overload;

  // Properties
  property configUser: string read cfguser write cfguser;
  property locale: TFormatSettings read fsettings write updateLocale;

end;

{$ifdef X_LINUXBSD}
function IsNotifySendAvailable: boolean;
{$endif}
//procedure QWindow_setWindowBadge(window: QWindowH; badge: PChar); cdecl; external 'libQt6Gui.so.6';
{$ifdef Windows}
function IsBurntToastAvailable: boolean;
{$endif}

const
  DWMWA_CAPTION_COLOR = 35;
  DWMWA_TEXT_COLOR    = 36;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  // Default badge rendering parameters
  DEFAULT_BADGE_SIZE_RATIO = 0.8;
  DEFAULT_MIN_FONT_SIZE    = 8;

// (implementation continued)

implementation
{------------------------------------------------------------------------------
  TTrndiNativeBase.updateLocale
  ------------------------
  Sets formatting settings
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.updateLocale(const l: TFormatSettings);
begin
  fsettings := l;
  DefaultFormatSettings := fsettings; // We need this for now
end;

{------------------------------------------------------------------------------
  TTrndiNativeBase.buildKey
  -------------------
  Gets the key name in the ini file/registry/etc
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.buildKey(const key: string; global: boolean): string;
begin
  if global then
    result := key
  else if Trim(cfguser) <> '' then // Prepend the username and _
    result := Format('%s_%s', [cfguser, key])
  else
    result := key;
end;

{------------------------------------------------------------------------------
  isNotificationSystemAvailable (class)
  -------------------------------------
  Returns True if a native desktop notification mechanism is available.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.isNotificationSystemAvailable: boolean;
begin
  {$IF DEFINED(X_WIN)}
    Result := IsBurntToastAvailable;
  {$ELSEIF DEFINED(X_MAC)}
    // NSUserNotificationCenter exists on macOS; assume available
    Result := True;
  {$ELSE}
    // Linux/BSD: notify-send is common; if missing, caller may fallback
    Result := IsNotifySendAvailable;
  {$ENDIF}
end;

{$IFDEF Windows}
procedure TTrndiNativeBase.start;
begin

end;

procedure TTrndiNativeBase.done;
begin

end;
{$ENDIF}

{$IFNDEF Windows}
procedure TTrndiNativeBase.start;
begin

end;

procedure TTrndiNativeBase.done;
begin

end;
{$ENDIF}

{------------------------------------------------------------------------------
  setBadge
  -------------------
  Sets an overlay icon on the taskbar or such
 ------------------------------------------------------------------------------}
{$IFDEF LCLGTK3}
procedure TTrndiNativeBase.SetBadge(const Value: string;  badgeColor: TColor);
begin
end;
{$ENDIF}

{$IFDEF LCLGTK2}
procedure TTrndiNativeBase.SetBadge(const Value: string;  badgeColor: TColor);
begin
end;
{$ENDIF}

{$IFDEF LCLFPGUI}
procedure TTrndiNativeBase.SetBadge(const Value: string;  badgeColor: TColor);
begin
end;
{$ENDIF}

// Convenience overload that calls the virtual full version with defaults
// On macOS, a platform-specific 2-arg implementation exists below; therefore
// this generic convenience method is excluded there to avoid duplicate bodies.
{$IFNDEF DARWIN}
procedure TTrndiNativeBase.SetBadge(const Value: string; badgeColor: TColor);
begin
  SetBadge(Value, badgeColor, DEFAULT_BADGE_SIZE_RATIO, DEFAULT_MIN_FONT_SIZE);
end;
{$ENDIF}

// Default no-op full overload for non-Windows/non-macOS builds
{$IFNDEF LCLWIN32}
{$IFNDEF DARWIN}
procedure TTrndiNativeBase.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer);
begin
  // No-op by default in base; platform classes may override
end;
{$ENDIF}
{$ENDIF}

{$IFDEF LCLWIN32}
// Helper method to use the overlay icon approach for taskbar badges (full parameters)

procedure TTrndiNativeBase.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer);
const
  // Badge sizing constants
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
      BadgeSize := Round(IconWidth * badge_size_ratio)
    else
      BadgeSize := Round(IconHeight * badge_size_ratio);

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
    if FontSize < min_font_size then
      FontSize := min_font_size; // Ensure minimum readable size
    Bitmap.Canvas.Font.Size := FontSize;

    // Get text dimensions
    TextWidth := Bitmap.Canvas.TextWidth(BadgeText);
    TextHeight := Bitmap.Canvas.TextHeight(BadgeText);

    // Scale down if necessary
    while (TextWidth > (BadgeSize - TEXT_PADDING)) and (FontSize > min_font_size - 2) do
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

{$IFDEF DARWIN}
procedure TTrndiNativeBase.SetBadge(const Value: string; BadgeColor: TColor);
var
  NSS: NSString;
begin
  NSS := NSSTR(Value);
  NSApp.dockTile.setBadgeLabel(NSS);
  NSS.release;
end;
// Full overload ignored on macOS, use Value only
procedure TTrndiNativeBase.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: Double; min_font_size: Integer);
begin
  SetBadge(Value, BadgeColor);
end;
{$ENDIF}

{------------------------------------------------------------------------------
  PlaySound
  -------------------
  Plays an audio file
 ------------------------------------------------------------------------------}
class procedure TTrndiNativeBase.PlaySound(const FileName: string);
  function sIsValidAudioFile(const FileName: string): Boolean;
  var
    Ext: string;
    ValidExtensions: array[0..6] of string = (
      '.wav', '.mp3', '.ogg', '.flac', '.aac', '.wma', '.m4a'
    );
    i: Integer;
  begin
    Result := False;

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
  // Validate file before attempting to play
  if not sIsValidAudioFile(FileName) then
    Exit;

  Process := TProcess.Create(nil);

  try
    {$IFDEF X_WIN}
    Process.Executable := 'mplay32';
    Process.Parameters.Add('/play');
    Process.Parameters.Add('/close');
    Process.Parameters.Add(FileName);
    {$ENDIF}

    {$IFDEF X_LINUXBSD}
    Process.Executable := 'aplay';
    Process.Parameters.Add(FileName);
    {$ENDIF}

    {$IFDEF X_MAC}
    Process.Executable := 'afplay';
    Process.Parameters.Add(FileName);
    {$ENDIF}
    Process.Execute;
  finally
    Process.Free;
  end;
end;

{------------------------------------------------------------------------------
  Destroy
  -------------------
  Cleans up any allocated resources. On Linux/PC, frees the INI file handle.
 ------------------------------------------------------------------------------}
destructor TTrndiNativeBase.Destroy;
begin
  {$IF DEFINED(X_PC)}
  if Assigned(inistore) then
    inistore.Free;
  {$ENDIF}
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  DetectTouchScreen
  --------------------------
  Overridable touch screen setting
 ------------------------------------------------------------------------------}
 class function TTrndiNativeBase.HasTouchScreen(out multi: boolean): boolean;
 begin
   DetectTouchScreen(multi);
   result := HasTouchScreen;
 end;

{------------------------------------------------------------------------------
  HasTouchScreen
  -------------------
  Detects touch screen
 ------------------------------------------------------------------------------}
 class function TTrndiNativeBase.HasTouchScreen: boolean;
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
  DetectTouchScreen (platform)
   --------------------------
   Platform-specific detection of touch hardware.
  ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
class function TTrndiNativeBase.DetectTouchScreen(out multi: boolean): boolean;
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
class function TTrndiNativeBase.DetectTouchScreen(out multi: boolean): boolean;
begin
  // macOS: Typically no standard touchscreen (unless iOS)
  Result := false;
  multi := false;
end;
{$ELSE}
class function TTrndiNativeBase.DetectTouchScreen(out multi: boolean): boolean;
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
          // Blocket är slut, analysera det
          if Block.Count > 0 then
          begin
            // Sök touch i blocket
            if (Block.Text.ToLower.Contains('touch')) then
            begin
              Result := true;
              // Sök multicapabilities i blocket
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
  create (default)
  ----------------------------
  An empty constructor that defaults useragent/baseurl to minimal placeholders.
 ------------------------------------------------------------------------------}
constructor TTrndiNativeBase.create;
begin
  // Provide a default user-agent and empty base URL
  create('Mozilla/5.0 (compatible; trndi) TrndiAPI', '');
end;

{------------------------------------------------------------------------------
  create (overload)
  ----------------------------
  Allow a custom user-agent.
 ------------------------------------------------------------------------------}
constructor TTrndiNativeBase.create(ua: string);
begin
  // Provide a default user-agent and empty base URL
  create(ua, '');
end;

{------------------------------------------------------------------------------
  create (overload)
  -----------------------------
  Allows specifying a custom user-agent and a base URL.
 -------------------------------------------------------
 -----------------------}
constructor TTrndiNativeBase.create(ua, base: string);
begin
  useragent := ua;
  baseurl   := base;
  // Check if we're in dark mode on creation
  dark := isDarkMode;
  fsettings := DefaultFormatSettings;
  if touchOverride = tbUnset then
     touchOverride := tbUnknown;
  cfguser := '';
  nofree := true;
   noticeDuration := 5000;
end;

  {$IFDEF X_WIN}
  {------------------------------------------------------------------------------
    getLocaleInformation
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
  GetOSLanguage
  -------------------
  Gets the operating system's language
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.GetOSLanguage: string;
begin
  {$IFDEF X_WIN}
   Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
  {$ELSE}
    {$IFDEF X_MAC}
      result := NSLocale.currentLocale.localeIdentifier.UTF8String;
    {$ELSE}
       Result := SysUtils.GetEnvironmentVariable('LANG');
    {$ENDIF}
  {$ENDIF}
end;


{$ifdef X_LINUXBSD}
{------------------------------------------------------------------------------
  isNotifySendAvailable
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
  attention
  -------------------
  Flashes something depending on the system
 ------------------------------------------------------------------------------}

procedure TTrndiNativeBase.attention(topic, message: string);
{$if  DEFINED(X_LINUXBSD)}
function RunAndCapture(const Exec: string; const Params: array of string;
                       out StdoutS, StderrS: string; out ExitCode: Integer): Boolean;
var
  P: TProcess;
  i: Integer;
  OutStr, ErrStr: TStringStream;
  Buf: array[0..4095] of byte;
  n: SizeInt;
begin
  Result := False;
  StdoutS := ''; StderrS := ''; ExitCode := -1;

  P := TProcess.Create(nil);
  OutStr := TStringStream.Create('');
  ErrStr := TStringStream.Create('');
  try
    P.Executable := Exec;
    for i := 0 to High(Params) do
      P.Parameters.Add(Params[i]);
    P.Options := [poUsePipes, poWaitOnExit];
    P.ShowWindow := swoHIDE;
    P.Execute;

    // Drain stdout/stderr fully (works on Linux/Qt6)
    repeat
      while P.Output.NumBytesAvailable > 0 do
      begin
        n := P.Output.Read(Buf, SizeOf(Buf));
        if n > 0 then OutStr.WriteBuffer(Buf, n) else Break;
      end;
      while P.Stderr.NumBytesAvailable > 0 do
      begin
        n := P.Stderr.Read(Buf, SizeOf(Buf));
        if n > 0 then ErrStr.WriteBuffer(Buf, n) else Break;
      end;
      if not P.Running then Break;
      Sleep(5);
    until False;

    ExitCode := P.ExitStatus;
    StdoutS := OutStr.DataString;
    StderrS := ErrStr.DataString;
    Result := ExitCode = 0;
  finally
    ErrStr.Free; OutStr.Free; P.Free;
  end;
end;

procedure SendNotification(Title, Message: string);
var
  AProcess: TProcess;
begin
  {$IFDEF X_PC}
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
    ShowMessage('Notifieringsfunktionen är inte tillgänglig eftersom "notify-send" inte är installerat.');
  {$ENDIF}
end;
  {$endif}
  {$if defined(X_WIN)}

  {------------------------------------------------------------------------------
    PSQuote
  -------------------
  Quote text for PowerShell
 ------------------------------------------------------------------------------}
function PSQuote(const S: UnicodeString): UnicodeString;
begin
  // PowerShell single-quoted literal; escape embedded single quotes
  Result := '''' + StringReplace(S, '''', '''''', [rfReplaceAll]) + '''';
end;

{------------------------------------------------------------------------------
  GetExePathW
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
  GetEnvVarW
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
  SendNotification
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
  SendNotification(topic, message);
end;

{------------------------------------------------------------------------------
  request
  -------------------
  Sends a GET or POST request, depending on the "post" parameter.
  Behavior differs by platform. Each platform has its own implementation block:
    - X_MAC uses TNSHTTPSendAndReceive
    - X_WIN uses TWinHTTPClient
    - X_PC (Linux) uses TFPHttpClient with OpenSSL
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
function TTrndiNativeBase.request(const post: boolean; const endpoint: string;
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
function TTrndiNativeBase.request(const post: boolean; const endpoint: string;
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
function TTrndiNativeBase.request(const post: boolean; const endpoint: string;
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
  GetCharSetting
  -------------------
  Returns a char from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetCharSetting(const keyname: string; def: char = #0): char;
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
  GetRootSetting
  -------------------
  Returns a bool from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetRootSetting(const keyname: string; def: string = ''): string;
begin
  result := GetSetting(keyname, def, true);
end;

{------------------------------------------------------------------------------
  GetSetting
  ----------------------
  Platform-specific string retrieval. Returns the default if the key isn't found.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_WIN)}
function TTrndiNativeBase.GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
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
  if result = '' then
    result := def;
end;

{$ELSEIF DEFINED(X_PC)}
function TTrndiNativeBase.GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
var
  key: string;
begin
  key := buildKey(keyname, global);
  if not Assigned(inistore) then
    inistore := TINIFile.Create(GetAppConfigFile(false));
  Result := inistore.ReadString('trndi', key, def);
end;

{$ELSEIF DEFINED(X_MAC)}
function TTrndiNativeBase.GetSetting(const keyname: string; def: string = ''; global: boolean = false): string;
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
  GetIntSetting
  -------------------------
  Returns an integer from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetIntSetting(const keyname: string; def: integer = -1): integer;
var
  r: string;
begin
  r := GetSetting(keyname, 'fail');

  if not TryStrToInt(r, Result) then
    Result := def;
end;

{------------------------------------------------------------------------------
  GetFloatSetting
  -------------------------
  Returns a single from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetFloatSetting(const keyname: string; def: single = -1): single;
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
  GetBoolSetting
  -------------------------
  Returns a bool from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetBoolSetting(const keyname: string; def: boolean = false): boolean;
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
  SetColorSetting
  ----------------------
  Stores a TColor value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetColorSetting(const keyname: string; val: TColor);
begin
  SetSetting(keyname, IntToStr(Integer(val)));
end;

{------------------------------------------------------------------------------
  GetColorSetting
  -------------------------
  Returns a TColor from settings if parseable, else returns `def`.
 ------------------------------------------------------------------------------}
function TTrndiNativeBase.GetColorSetting(const keyname: string; const def: TColor): TColor;
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
  SetBoolSetting
  ----------------------
  Stores a bool value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetBoolSetting(const keyname: string; const val: boolean);
begin
  if val then
    SetSetting(keyname, 'true')
  else
    SetSetting(keyname, 'false');
end;

{------------------------------------------------------------------------------
  ----------------------
  Stores a float value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetFloatSetting(const keyname: string; const val: single);
var
  f: TFormatSettings;
begin
    f := DefaultFormatSettings;
    f.DecimalSeparator := '.';
    SetSetting(keyname, FormatFloat('0.00',val,f));
end;

{------------------------------------------------------------------------------
  SetRootSetting
  ----------------------
  Stores a non-user specific string value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.SetRootSetting(keyname: string; const val: string);
begin
  SetSetting(keyname, val, true);
end;

{------------------------------------------------------------------------------
  DeleteRootSetting
  ----------------------
  Stores a string value to platform-specific storage.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeBase.DeleteRootSetting(keyname: string; const val: string);
begin
  DeleteSetting(keyname, true);
end;

{------------------------------------------------------------------------------
  DeleteSetting
  -------------------------
  Deletes a stored key/value from platform-specific storage completely.
  - X_MAC sets key to blank
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
procedure TTrndiNativeBase.DeleteSetting(const keyname: string; global: boolean = false);
begin
  SetSetting(keyname,'',global);
end;


{$ELSEIF DEFINED(X_WIN)}
procedure TTrndiNativeBase.DeleteSetting(const keyname: string; global: boolean = false);
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
procedure TTrndiNativeBase.DeleteSetting(const keyname: string; global: boolean = false);
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
  SetSetting
  ----------------------
  Stores a string value to platform-specific storage.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
procedure TTrndiNativeBase.SetSetting(const keyname: string; const val: string; global: boolean = false);
var
  key: string;
begin
  key := buildKey(keyname, global);
  SetPrefString(key, val); // macOS-based method
end;

procedure TTrndiNativeBase.ReloadSettings;
begin

end;

{$ELSEIF DEFINED(X_WIN)}
procedure TTrndiNativeBase.SetSetting(const keyname: string; const val: string; global: boolean = false);
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

procedure TTrndiNativeBase.ReloadSettings;
begin

end;

{$ELSEIF DEFINED(X_PC)}
procedure TTrndiNativeBase.ReloadSettings;
begin
  FreeAndNil(inistore);
  inistore := TIniFile.Create(GetAppConfigFile(false));
end;

procedure TTrndiNativeBase.SetSetting(const keyname: string; const val: string; global: boolean = false);
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
  isDarkMode
  ----------------------
  Determines if the user's system is in "dark mode," per platform.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
class function TTrndiNativeBase.isDarkMode: boolean;
begin
  // Typically, AppleInterfaceStyle = 'Dark' if dark mode is active
  Result := Pos('DARK', UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
end;

{$ELSEIF DEFINED(X_WIN)}
class function TTrndiNativeBase.isDarkMode: boolean;
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
class function TTrndiNativeBase.isDarkMode: boolean;
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
  getURL (class function)
  -----------------------------------
  A static method for a quick GET request. Returns True if successful, False on error.
 ------------------------------------------------------------------------------}
{$IF DEFINED(X_MAC)}
class function TTrndiNativeBase.getURL(const url: string; out res: string): boolean;
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
class function TTrndiNativeBase.getURL(const url: string; out res: string): boolean;
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
class function TTrndiNativeBase.getURL(const url: string; out res: string): boolean;
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
  HasDangerousChars
  ----------------------
  Detects chars which the console is not fond of
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.HasDangerousChars(const FileName: string): Boolean;
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
  DetectWSL
  ----------------------
  Detects if the app is running under Windows Subsystem for Linux
 ------------------------------------------------------------------------------}
class function TTrndiNativeBase.DetectWSL: TWSLInfo;
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

  // Kontrollera miljövariabler
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

  // Ytterligare kontroller om vi inte hittat WSL än
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
  IsBurntToastAvailable
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

function TTrndiNativeBase.SetTitleColor(form: THandle; bg, text: TColor): boolean;
begin
  result := false;
end;

end.

