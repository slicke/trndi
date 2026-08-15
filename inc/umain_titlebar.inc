(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)

// Application-drawn title bar (Wayland/Linux "full color" parity).
//
// Wayland compositors own the window decorations: they cannot be tinted (so
// SetTitleColor has nowhere to go) and on some desktops they cannot even be
// styled away. When the platform reports NeedsCustomTitleBar — or the user
// forces it via the 'ux.own_titlebar' setting ('auto'/'on'/'off') — the main
// window drops its native frame (bsNone) and shows a TSlickeTitleBar instead,
// with TSlickeWindowGrips keeping the frameless window resizable.
//
// Tiling compositors (Hyprland, sway, …) are excluded from the automatic
// path: NeedsCustomTitleBar returns false there, since those desktops
// intentionally draw no titlebar and manage windows via keybinds. 'on'
// still forces the bar for users running Trndi floating.
//
// Coloring needs no new logic: the bar registers a title-color sink with
// TrndiNative, so the existing native.SetTitleColor calls in
// umain_glucose.inc (glucose color, off-range color, multi-user color) land
// on the drawn bar and return true, exactly like DWM/Cocoa do on Windows and
// macOS. The handful of top-anchored controls (off-range panel, TIR label,
// clock, warning banner) offset themselves by TitleBarOffset.
//
// Owning the bar also means the multi-user name can live in it: the bar draws
// an identity chip (TSlickeTitleBar.SetUserBadge) where Windows overlays its
// native title-bar badge, so Wayland gets the same "[name] Trndi" prefix-free
// caption. RefreshUserBadge in umain_glucose.inc picks between the two.

{------------------------------------------------------------------------------
  State queries
 ------------------------------------------------------------------------------}

function TfBG.OwnTitleBarActive: boolean;
begin
  Result := Assigned(FTitleBar);
end;

// Vertical space the bar occupies; top-anchored layout code adds this to its
// Top coordinates so nothing hides underneath the bar.
function TfBG.TitleBarOffset: integer;
begin
  if Assigned(FTitleBar) and FTitleBar.Visible then
    Result := FTitleBar.Height
  else
    Result := 0;
end;

// Handle provider for the title-color sink registry: called at SetTitleColor
// time so LCL handle recreation (BorderStyle changes) can't orphan the entry.
function TfBG.TitleBarWindowHandle: PtrUInt;
begin
  Result := PtrUInt(Self.Handle);
end;

{------------------------------------------------------------------------------
  Sink + lifecycle
 ------------------------------------------------------------------------------}

// Receives the (bg, text) pair the Windows/macOS paths would hand to the
// native title bar. RefreshTitle runs unconditionally: the form caption
// changes with every reading even when the colors do not.
procedure TfBG.TitleBarColorSink(bg, txt: TColor);
begin
  if not Assigned(FTitleBar) then
    Exit;
  FTitleBar.SetColors(bg, txt);
  FTitleBar.RefreshTitle;
end;

// The maximize button (and a double-click on the bar) maps to Trndi's own
// fullscreen rather than a plain maximize: a frameless maximized window is
// visually identical to fullscreen anyway, except it would keep the bar. In
// fullscreen the bar hides; Esc or F (or double-clicking the delta label)
// brings the window - and the bar - back.
procedure TfBG.TitleBarMaximizeRequest({%H-}Sender: TObject);
begin
  // A window maximized behind our back (drag-to-top, WM keybind) shows the
  // restore glyph — honor it. Otherwise the button means "fill the screen",
  // which for Trndi is fullscreen.
  if WindowState = wsMaximized then
    WindowState := wsNormal
  else
    DoFullScreen;
end;

// The identity chip is the drawn bar's counterpart to the Windows title-bar
// badge, and leads to the same place: Settings, where the account is switched.
procedure TfBG.TitleBarBadgeClick({%H-}Sender: TObject);
begin
  UserBadgeClicked;
end;

// Helper forms shown modally (Settings, first-run wizard) are plain TForms,
// not TDialogForm, so they don't inherit the alert dialogs' bar. Dress them
// on the same Wayland condition before they show; SlickeDressWithTitleBar
// no-ops when the form is already frameless or already dressed, so calling
// this repeatedly (Settings reopens often) is safe.
procedure TfBG.DressModalForm(AForm: TCustomForm);
begin
  if not (Assigned(AForm) and native.NeedsCustomTitleBar) then
    Exit;
  SlickeDressWithTitleBar(AForm, AForm.Color,
    IfThen(IsLightColor(AForm.Color), clBlack, clWhite), [stbClose]);
end;

// Create or tear down the bar according to platform capability and settings.
// Runs at boot (FormCreate) and again on every settings apply.
procedure TfBG.ApplyOwnTitleBar;
var
  mode: string;
  want: boolean;
begin
  {$ifdef X_LINUXBSD}
  mode := LowerCase(Trim(native.GetSetting('ux.own_titlebar', 'auto')));
  case mode of
  'on':
    want := true; // X11 opt-in: colored bar despite working native decorations
  'off':
    want := false;
  else
    want := native.NeedsCustomTitleBar;
  end;
  // The fully frameless preference wins — no decorations of any kind.
  if native.GetBoolSetting('main.noborders', false) then
    want := false;
  {$else}
  // Linux/BSD only: Windows and macOS have colorable native title bars, and
  // an imported settings file carrying ux.own_titlebar=on must not strip
  // their window frames.
  want := false;
  {$endif}

  if want then
  begin
    BorderStyle := bsNone;
    if not Assigned(FTitleBar) then
    begin
      FTitleBar := TSlickeTitleBar.Create(Self);
      FTitleBar.Parent := Self;
      FTitleBar.Font.Assign(Font);
      FTitleBar.Font.Height := 0; // default size, not lVal's scaled-up size
      FTitleBar.UpdateMetrics;
      // Start from the current window color; the first setColorMode pass
      // recolors it through the sink.
      FTitleBar.SetColors(Color, IfThen(IsLightColor(Color), clBlack, clWhite));
      FTitleBar.OnMaximizeRequest := @TitleBarMaximizeRequest;
      // Tiling compositors have no minimize/maximize concept.
      if native.nobuttonsVM then
        FTitleBar.Buttons := [stbClose];
      TrndiNative.RegisterTitleColorSink(@TitleBarWindowHandle, @TitleBarColorSink);
    end;
    if not Assigned(FWindowGrips) then
      FWindowGrips := TSlickeWindowGrips.Create(Self);
    UpdateOwnTitleBarState;
  end
  else
  begin
    if Assigned(FTitleBar) then
    begin
      TrndiNative.UnregisterTitleColorSink(@TitleBarColorSink);
      FreeAndNil(FTitleBar);
    end;
    FreeAndNil(FWindowGrips);
    // The caller (FormCreate/ApplySettingsInstantly) owns BorderStyle when
    // the bar is off; nothing to restore here.
  end;

  // The bar carries the multi-user identity chip, so its arrival or departure
  // decides whether the name sits in the bar or back in the caption text.
  RefreshUserBadge;
end;

// Keep bar/grip visibility in sync with the window state: fullscreen hides
// the bar (kiosk/fullscreen wants every pixel), the grips additionally hide
// while maximized (handled inside TSlickeWindowGrips).
procedure TfBG.UpdateOwnTitleBarState;
begin
  if Assigned(FTitleBar) then
  begin
    FTitleBar.Visible := WindowState <> wsFullScreen;
    FTitleBar.BringToFront;
  end;
  if Assigned(FWindowGrips) then
  begin
    FWindowGrips.SetActive(WindowState <> wsFullScreen);
    FWindowGrips.EnsureOnTop;
  end;
end;
