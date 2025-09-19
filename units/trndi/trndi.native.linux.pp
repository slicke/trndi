unit trndi.native.linux;

{**
  @abstract(Linux/BSD-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeLinux) which derives from
  @link(TTrndiNativeBase) and implements behaviors using common Linux tools
  and LCL facilities.

  Responsibilities include:
  - Text-to-speech via @code(spd-say) (@link(TTrndiNativeLinux.Speak))
  - Drawing a badge on the system tray icon (@link(TTrndiNativeLinux.SetTray))
  - Synchronizing KDE taskbar badge (@link(TTrndiNativeLinux.SetBadge))
  - Placeholder for dark mode toggling (@link(TTrndiNativeLinux.setDarkMode))

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Graphics, fphttpclient, openssl, opensslsockets, IniFiles, Dialogs,
  ExtCtrls, Forms, Math, LCLIntf, KDEBadge, trndi.native.base;

type
  {!
    @abstract(Linux implementation of @link(TTrndiNativeBase).)
    Uses spd-say for speech and draws badges on tray/KDE taskbar.
  }
  TTrndiNativeLinux = class(TTrndiNativeBase)
  public
    {** Speaks @param(Text) using spd-say, if available.
        Shows a user-visible message when the tool is not present. }
    procedure Speak(const Text: string); override;
    {** Draw a badge with @param(Value) text on tray icon using @param(BadgeColor).
        @param(badge_size_ratio Determines badge diameter relative to icon size)
        @param(min_font_size Lower bound for font size while fitting text) }
    procedure SetTray(const Value: string; BadgeColor: TColor; badge_size_ratio: double = 0.8; min_font_size: integer = 8);
    {** Convenience overload: delegates to base 2-arg SetBadge. }
    procedure setBadge(const Value: string; BadgeColor: TColor); overload; reintroduce;
    {** Synchronize KDE badge with numeric value and update tray badge drawing. }
    procedure setBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer); overload; override;
    {** Placeholder; desktop environments differ. Implement where feasible. }
    class function setDarkMode: boolean; // no-op placeholder
  end;

implementation

uses
  Process, Types, LCLType;

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
  L := GetEnvironmentVariable('LC_ALL');
  if L = '' then
    L := GetEnvironmentVariable('LANGUAGE');
  if L = '' then
    L := GetEnvironmentVariable('LANG');

  if L = '' then
    Exit('');

  L := FirstSegment(L, ':');

  P := Pos('.', L);
  if P > 0 then
    L := Copy(L, 1, P-1);

  L := StringReplace(L, '_', '-', [rfReplaceAll]);

  L := LowerCase(L);
  P := Pos('-', L);
  if P > 0 then
    L := Copy(L, 1, P) + UpperCase(Copy(L, P+1, MaxInt));

  Result := L;
end;

procedure TTrndiNativeLinux.Speak(const Text: string);
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
      Proc.Parameters.AddStrings(['-l', Lang])
    else
      ;

    Proc.Parameters.Add('--');
    Proc.Parameters.Add(Text);

    Proc.Options := [];
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;

procedure TTrndiNativeLinux.SetTray(const Value: string; BadgeColor: TColor;
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
  dval: double;
begin
  // Ensure we have a tray icon instance
  if not Assigned(Tray) then
    Tray := TTrayIcon.Create(Application.MainForm);

  if Value = '' then
  begin
    // Reset to app icon and force refresh
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      Tray.Icon.Assign(Application.Icon);
    Tray.Visible := False;
    Tray.Visible := True;
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

  BaseIcon := TIcon.Create;
  OutIcon  := TIcon.Create;
  Bmp      := TBitmap.Create;
  try
    if (Application.Icon <> nil) and (Application.Icon.Width > 0) then
      BaseIcon.Assign(Application.Icon)
    else
      BaseIcon.SetSize(24, 24);

    W := BaseIcon.Width;
    H := BaseIcon.Height;
    if (W <= 0) or (H <= 0) then
    begin
      W := 24; H := 24;
    end;

    BadgeSize := Round(Min(W, H) * badge_size_ratio);
    if BadgeSize < 10 then
      BadgeSize := 10;

    Bmp.SetSize(W, H);
    Bmp.PixelFormat := pf32bit;

    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.Brush.Color := clNone;
    Bmp.Canvas.FillRect(Rect(0, 0, W, H));
    Bmp.Canvas.Draw(0, 0, BaseIcon);

    BadgeRect := Rect(W - BadgeSize, H - BadgeSize, W, H);

    rgb := ColorToRGB(BadgeColor);
    r := Byte(rgb);
    g := Byte(rgb shr 8);
    b := Byte(rgb shr 16);
    if (0.299*r + 0.587*g + 0.114*b) > 128 then
      TextColor := clBlack
    else
      TextColor := clWhite;

    Bmp.Canvas.Brush.Color := BadgeColor;
    Bmp.Canvas.Pen.Color := BadgeColor;

    if BadgeSize <= 12 then
      Bmp.Canvas.FillRect(BadgeRect)
    else
    begin
      Radius := Round(CORNER_RADIUS * BadgeSize / 32);
      if Radius < 2 then
        Radius := 2;

      Bmp.Canvas.RoundRect(
        BadgeRect.Left, BadgeRect.Top,
        BadgeRect.Right, BadgeRect.Bottom,
        Radius * 2, Radius * 2
      );

      Bmp.Canvas.FillRect(
        Rect(BadgeRect.Right - Radius, BadgeRect.Bottom - Radius, BadgeRect.Right, BadgeRect.Bottom)
      );
    end;

    Bmp.Canvas.Font.Name := 'DejaVu Sans';
    Bmp.Canvas.Font.Style := [fsBold];
    Bmp.Canvas.Font.Color := TextColor;

    FontSize := Round(BadgeSize * INITIAL_FONT_SIZE_RATIO);
    if FontSize < min_font_size then
      FontSize := min_font_size;
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

    Bmp.Canvas.Brush.Style := bsClear;
    Bmp.Canvas.TextOut(
      BadgeRect.Left + ((BadgeRect.Right - BadgeRect.Left) - TextW) div 2,
      BadgeRect.Top  + ((BadgeRect.Bottom - BadgeRect.Top) - TextH) div 2,
      BadgeText
    );

    OutIcon.Assign(Bmp);
    Tray.Icon.Assign(OutIcon);
    Tray.Visible := False;
    Tray.Visible := True;
  finally
    Bmp.Free;
    OutIcon.Free;
    BaseIcon.Free;
  end;
end;

procedure TTrndiNativeLinux.SetBadge(const Value: string; BadgeColor: TColor;
                       badge_size_ratio: double; min_font_size: integer); 
var
  f: double;
begin
  f := 0.0;
  TryStrToFloat(value, f);
  ClearBadge;
  KDEBadge.SetBadge(f);
  SetTray(value,badgecolor,badge_size_ratio, min_font_size);
end;

procedure TTrndiNativeLinux.SetBadge(const Value: string; BadgeColor: TColor);
begin
  inherited SetBadge(Value, BadgeColor);
end;
class function TTrndiNativeLinux.setDarkMode: Boolean;
begin
  // Placeholder: implement when desktop environment APIs are standardized
end;

end.
