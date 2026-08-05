(*
 * slicke.ux.native.pas
 * Native UI helpers (font selection, safe modal fallback) for the Slicke UX toolkit.
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 * License: Apache License 2.0
 *)

{**
  @unit slicke.ux.fonts
  @brief Native helpers for the Slicke's UX toolkit.

  @details
  This unit provides a set of native-related helper functions

  The public API centers around:
  - Platform-specific font selection for UI.
  - Font helpers @link(FontGUIInList) and @link(FontTXTInList) to locate suitable UI/text fonts.

  Platform support:
  - Windows: emoji rendering via Direct2D/DirectWrite; custom dark-titlebar opt-in where possible.
  - Linux/BSD: emoji/text via canvas using Noto fonts when available.

  @author
  Björn Lindh, with PasDoc annotations added.
}
unit slicke.ux.native;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, forms, controls;

  {**
    Check if a suitable UI font exists on this system and return its name.
    @param fname Out parameter that receives a preferred UI font name for emoji/mono display.
    @returns @true if the font (or a fallback) is available; otherwise @false.
  }
function FontGUIInList(out fname: string): boolean; overload;

  {**
    As @link(FontGUIInList), but also reports whether the selected face is a
    monochrome one. Monochrome faces honour @code(Font.Color), so the caller can
    tint an icon; colour-bitmap emoji fonts ignore it.
    @param fname Out parameter that receives a preferred UI font name for emoji/mono display.
    @param monochrome Out parameter, @true when the face draws in @code(Font.Color).
    @returns @true if the font (or a fallback) is available; otherwise @false.
  }
function FontGUIInList(out fname: string; out monochrome: boolean): boolean; overload;

  {**
    Check if a suitable text font exists on this system and return its name.
    @param fname Out parameter that receives a preferred UI text font.
    @returns @true if the font is present; otherwise @false. On unknown platforms always @true with a generic name.
  }
function FontTXTInList(out fname: string): boolean;

  {**
    Check if the Window Manaager is problematic, which means it can't handle ShowModal
    @returns @true if the VM is problematic
    }
function IsProblematicWM: boolean;

  {**
    Check if the Window Manaager is problematic to a lesser extent - ie "just" fails with showmodal
    @returns @true if the VM is semi-problematic
    }
function IsSemiProblematicWM: boolean;

  {**
    Show a standard TForm modally with the same 'bad-WM' fallback logic as
    the other UX dialog helpers. Returns the form ModalResult.
  }
function ShowFormModalSafe(aForm: TForm): integer;

implementation

{**
  See interface docs. Attempts OS-appropriate defaults.
}
function FontTXTInList(out fname: string): boolean;
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
  {$elseif DEFINED(HAIKU)}
  fname := 'Noto Sans';
  result := true; // System font!
  {$else}
  fname := 'font';
  Result := true;
  {$endif}
end;

{$if DEFINED(X_LINUXBSD)}
type
  TUXIconFont = record
    name: string;
    monochrome: boolean;
  end;

const
  // Tried in order. The monochrome face comes first on purpose: it is outline
  // based, so it stays crisp when scaled up to icon size, and it draws in
  // Font.Color, which lets the dialogs tint each icon the way macOS tints its
  // SF Symbols. Noto Color Emoji is a CBDT bitmap font -- it upscales soft and
  // ignores the tint -- so it is the fallback rather than the first choice.
  // Symbola is last: plain outlines, rarely installed, but it covers the whole
  // dingbat range when neither Noto is present.
  UXIconFonts: array[0..2] of TUXIconFont = (
    (name: 'Noto Emoji'; monochrome: true),
    (name: 'Noto Color Emoji'; monochrome: false),
    (name: 'Symbola'; monochrome: true));
{$endif}

{**
  See interface docs. Returns a font suitable for UI/emoji display if available.
}
function FontGUIInList(out fname: string; out monochrome: boolean): boolean;
{$if DEFINED(X_LINUXBSD)}
var
  i: integer;
{$endif}
begin
  {$if DEFINED(X_LINUXBSD)}
  for i := Low(UXIconFonts) to High(UXIconFonts) do
    if Screen.Fonts.IndexOf(UXIconFonts[i].name) >= 0 then
    begin
      fname := UXIconFonts[i].name;
      monochrome := UXIconFonts[i].monochrome;
      Exit(true);
    end;
  // Nothing usable: hand back the preferred name so the caller can name it in
  // the "please install a font" message, and claim nothing about the substitute
  // fontconfig will end up picking.
  fname := UXIconFonts[Low(UXIconFonts)].name;
  monochrome := false;
  Result := false;
  {$elseif DEFINED(WINDOWS)}
  fname := 'Segoe UI Symbol';
  monochrome := true;
  try
    Result := Screen.Fonts.IndexOf(fname) >= 0;
  finally
  end;
  {$elseif DEFINED(HAIKU)}
  fname := 'Noto Sans';
  monochrome := true;
  result := true; // System font!
  {$else}
  fname := 'font';
  monochrome := false;
  Result := true;
  {$endif}
end;

{**
  See interface docs. Thin wrapper for callers that only need the font name.
}
function FontGUIInList(out fname: string): boolean;
var
  monochrome: boolean;
begin
  Result := FontGUIInList(fname, monochrome);
end;

{$ifdef X_LINUXBSD}
{**
  Detect a window manager likely to ignore showmodal, but supports other things.
}
function IsSemiProblematicWM: boolean;
var
  env, s: string;
  i: integer;
const
  Bad: array[0..0] of string = (
    'gnome'
    );
begin
  // Overrides
  env := GetEnvironmentVariable('TRNDI_DISABLE_MODAL_FALLBACK');
  if env = '1' then
    Exit(false);                               // This shouldnt really trigger as problematic would be false
  env := GetEnvironmentVariable('TRNDI_FORCE_MODAL_FALLBACK');
  if env = '1' then
    Exit(false);                               // "We're" a problematic vm

  s := LowerCase(Trim(GetEnvironmentVariable('XDG_CURRENT_DESKTOP') + ' ' +
    GetEnvironmentVariable('DESKTOP_SESSION') + ' ' +
    GetEnvironmentVariable('XDG_SESSION_DESKTOP') + ' ' +
    GetEnvironmentVariable('WINDOW_MANAGER')));

  for i := Low(Bad) to High(Bad) do
    if Pos(Bad[i], s) > 0 then
      Exit(true);

  Result := false;
end;

{**
  Detect a window manager likely to ignore transient/owner hints.
  Uses environment variables as a lightweight heuristic and supports
  runtime overrides via TRNDI_FORCE_MODAL_FALLBACK / TRNDI_DISABLE_MODAL_FALLBACK.
}
function IsProblematicWM: boolean;
var
  env, s: string;
  i: integer;
const
  Bad: array[0..12] of string = (
    'openbox', 'matchbox', 'fluxbox', 'fvwm', 'icewm', 'twm', 'pekwm',
    'lxde', 'lxde-pi', 'lxsession', 'pixel', 'raspbian', 'gnome'
    );
begin
  // Overrides
  env := GetEnvironmentVariable('TRNDI_DISABLE_MODAL_FALLBACK');
  if env = '1' then
    Exit(false);
  env := GetEnvironmentVariable('TRNDI_FORCE_MODAL_FALLBACK');
  if env = '1' then
    Exit(true);

  s := LowerCase(Trim(GetEnvironmentVariable('XDG_CURRENT_DESKTOP') + ' ' +
    GetEnvironmentVariable('DESKTOP_SESSION') + ' ' +
    GetEnvironmentVariable('XDG_SESSION_DESKTOP') + ' ' +
    GetEnvironmentVariable('WINDOW_MANAGER')));

  for i := Low(Bad) to High(Bad) do
    if Pos(Bad[i], s) > 0 then
      Exit(true);

  Result := false;
end;
{$else}
function IsProblematicWM: boolean;
begin
  result := false; // Win and mac are always good
end;

function IsSemiProblematicWM: boolean;
begin
  result := false;
end;

{$endif}

{**
  Show a dialog in a safe way on platforms where transient/owner hints
  may be ignored by the window manager. On non-Windows systems we use
  fsStayOnTop as a conservative fallback while the dialog is active.
}
function ShowFormModalSafe(aForm: TForm): integer;
var
  oldStyle: TFormStyle;
begin
  if not Assigned(aForm) then
    Exit(mrNone);
  // Attempt to set popup owner where possible
  try
    if Assigned(Application) and Assigned(Application.MainForm) then
    begin
      aForm.PopupMode := pmExplicit;
      aForm.PopupParent := Application.MainForm;
    end;
  except end;

  {$ifndef Windows}
  if IsProblematicWM then
  begin
    oldStyle := aForm.FormStyle;
    aForm.FormStyle := fsStayOnTop;
    try
      aForm.ShowModal;
      Result := aForm.ModalResult;
    finally
      try if Assigned(aForm) then
          aForm.FormStyle := oldStyle; except end;
    end;
    Exit;
  end;
  {$endif}

  aForm.ShowModal;
  Result := aForm.ModalResult;
end;



end.
