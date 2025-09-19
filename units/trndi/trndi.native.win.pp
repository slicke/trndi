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
    function SetTitleColor(form: THandle; bg, text: TColor): boolean; override;
  end;

implementation

uses
  ComObj;

{$ifdef Windows}
function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
{$endif}

{ DWM attribute constants used for caption and text colors, and dark mode }
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
  // Use SAPI (COM-based) text-to-speech. We try to pick a voice matching the
  // user locale; if none are available, the default SAPI voice is used.
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
  // Apply caption and text colors for the given window using DWM attributes.
  // TColor and COLORREF are both 0x00BBGGRR; no swap needed
  bgColor   := COLORREF(ColorToRGB(bg));
  textColor := COLORREF(ColorToRGB(text));

  hrCaption := SetDwmAttr(form, DWMWA_CAPTION_COLOR, bgColor, SizeOf(bgColor));
  hrText    := SetDwmAttr(form, DWMWA_TEXT_COLOR, textColor, SizeOf(textColor));

  Result := HrSucceeded(hrCaption) and HrSucceeded(hrText);
end;

end.
