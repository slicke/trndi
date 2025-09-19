unit trndi.native.win;

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Graphics, Windows, Registry, Dialogs, StrUtils, winhttpclient, shellapi,
  Forms, variants, dwmapi, trndi.native.base;

type
  TTrndiNativeWindows = class(TTrndiNativeBase)
  public
    procedure Speak(const Text: string); override;
    class function SetDarkMode(win: HWND; Enable: Boolean = True): Boolean;
    function SetTitleColor(form: THandle; bg, text: TColor): boolean; override;
  end;

implementation

uses
  ComObj;

{$ifdef Windows}
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
{$endif}

const
  DWMWA_CAPTION_COLOR = 35;
  DWMWA_TEXT_COLOR    = 36;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

procedure TTrndiNativeWindows.Speak(const Text: string);
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

class function TTrndiNativeWindows.SetDarkMode(win: HWND; Enable: Boolean = True): Boolean;
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

function SetDwmAttr(hWnd: HWND; Attr: DWORD; const Data; Size: DWORD): HRESULT;
begin
  Result := DwmSetWindowAttribute(hWnd, Attr, @Data, Size);
end;

function HrSucceeded(hr: HRESULT): Boolean; inline;
begin
  Result := hr >= 0; // SUCCEEDED(hr)
end;

function TTrndiNativeWindows.SetTitleColor(form: THandle; bg, text: TColor): Boolean;
var
  bgColor, textColor: COLORREF;
  hrCaption, hrText: HRESULT;
begin
  // TColor and COLORREF are both 0x00BBGGRR; no swap needed
  bgColor   := COLORREF(ColorToRGB(bg));
  textColor := COLORREF(ColorToRGB(text));

  hrCaption := SetDwmAttr(form, DWMWA_CAPTION_COLOR, bgColor, SizeOf(bgColor));
  hrText    := SetDwmAttr(form, DWMWA_TEXT_COLOR, textColor, SizeOf(textColor));

  Result := HrSucceeded(hrCaption) and HrSucceeded(hrText);
end;

end.
