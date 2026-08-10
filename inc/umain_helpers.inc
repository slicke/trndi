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
// Main form helper functions


procedure StopFlashing;
begin
  // No-op: tray flash removed; kept as call site stub.
end;

procedure TfBG.APIReceiver(const msg: string; etype: TrndiAPIMsg);
begin
  case etype of
  TrndiAPIMsg.alert:
    SlickeMessage('API Alert', msg, uxmtWarning);
  TrndiAPIMsg.notice:
  begin
    // User-relevant but non-urgent (e.g. CareLink picking the first of several
    // followed patients): show as a desktop notification, never a modal dialog
    native.attention(RS_API_NOTICE, msg);
    TrndiDLog(msg);
  end;
  TrndiAPIMsg.status:
    TrndiDLog(msg);
  {$ifdef DEBUG}
  TrndiAPIMsg.log:
    TrndiDLog(Format('[%s] %s', [self.className, msg]));
  {$endif}
  end;
end;

// Persist rotated backend credentials (e.g. CareLink refresh tokens) into the
// same user-scoped setting InitializeAPI reads them from. Losing a rotated
// refresh token invalidates the session permanently, so this must be saved
// as soon as the backend reports it.
procedure TfBG.APICredentialsChanged(const newCreds: string);
begin
  native.SetSetting('remote.creds', newCreds);
  TrndiDLog('API credentials rotated and saved');
end;

// Returns vertical offset needed to bring all trend dots fully inside their parent.
// Sign convention: negative = dots are above (need to move down); positive = dots are below (need to move up).
// Pure helper operating on plain data for unit tests
function DotsInViewFromInfos(const Dots: array of TDotInfo; ParentHeight: integer): integer;
var
  x: TDotInfo;
  bottom, overflow: integer;
  topResult, bottomResult: integer;
const
  Tol = 5; // pixel tolerance to ignore tiny rounding differences
begin
  topResult    := 0;
  bottomResult := 0;

  for x in Dots do
  begin
    if not x.Visible then
      Continue;

    bottom := x.Top + x.Height;

    if x.Top < -Tol then
      topResult := Min(topResult, x.Top)
    else if (ParentHeight > 0) and (bottom > ParentHeight + Tol) then
    begin
      overflow := bottom - (ParentHeight + Tol);
      bottomResult := Max(bottomResult, overflow);
    end;
  end;

  // Top overflow takes priority so AdjustGraph's second pass can handle bottom.
  if topResult < 0 then
    Result := topResult
  else
    Result := bottomResult;
end;

// Smallest circle DotPaint will draw. The analytic rasterizers bail out below
// 2 px; 3 still reads as a dot rather than a smudge. This is the floor that
// decides how many dots a window can hold — a larger one does not make a
// crowded trend more legible, it just makes the dots overlap sooner.
const
  MIN_DOT_DIAMETER_PX = 3;

// Horizontal room (px) one trend column needs before its dots start to crowd.
//
// ResizeDot draws a dot at 0.6 * column width, floored at MIN_DOT_DIAMETER_PX,
// so the two rules cross at 3 / 0.6 = 5 px. Above that the 40% gap between
// neighbours scales with the window; below it the dot can no longer shrink and
// the gap collapses into overlap — which is what smears the trend into a solid
// bar. 6 sits just above the crossover.
//
// This is deliberately permissive: at the 320 px startup width it admits ~53
// columns, so a 50-dot trend renders in full at the default window size and
// the clamp only engages past that.
const
  MIN_DOT_COLUMN_PX = 6;

// How many trend slots to advance between rendered dots so a narrow window
// never draws more dots than it has room for. 1 = render every slot.
//
// This is purely visual: the skipped slots keep their readings and their
// placement, they are just not shown. The trend still spans the time window
// the user picked (ACTIVE_DOTS * INTERVAL_MINUTES), only at a coarser
// resolution — resizing must never change *which* history is on screen.
//
// Pure helper operating on plain data for unit tests
function TrendDotStride(ClientWidth, ColumnCount: integer): integer;
var
  maxCols: integer;
begin
  Result := 1;
  if (ClientWidth <= 0) or (ColumnCount <= 1) then
    Exit;

  maxCols := ClientWidth div MIN_DOT_COLUMN_PX;
  if maxCols < 1 then
    maxCols := 1;
  if ColumnCount <= maxCols then
    Exit;

  // Round up, so the kept count never exceeds what fits: 30 columns with room
  // for 20 gives stride 2 (15 rendered), not stride 1.
  Result := (ColumnCount + maxCols - 1) div maxCols;
end;

// True when the trend slot at SlotIndex (1-based; ActiveDots is the newest)
// is one of the slots the given stride renders. Anchored on the newest slot
// so the latest reading — which carries the "fresh" ring and is the one the
// user actually looks at — is never the dot that gets dropped.
// Pure helper operating on plain data for unit tests
function TrendDotVisibleInStride(SlotIndex, ActiveDots, Stride: integer): boolean;
begin
  Result := (Stride <= 1) or (SlotIndex >= ActiveDots) or
    (((ActiveDots - SlotIndex) mod Stride) = 0);
end;

// Decode a stored ux.dot_color_mode into the enum, falling back to the default
// for anything out of range (an older profile, a hand-edited settings store).
// Pure helper operating on plain data for unit tests
function DotColorModeFromSetting(Value: integer): TDotColorMode;
begin
  if (Value < Ord(Low(TDotColorMode))) or (Value > Ord(High(TDotColorMode))) then
    Result := dcmAuto
  else
    Result := TDotColorMode(Value);
end;

// Map a dot's identity color — the range color DetermineColorForReading picked
// for the reading — to the color it is actually drawn in, per the user's dot
// coloring mode. Called from DotPaint, where the background is final.
//
// Every mode except dcmClassic ends at EnsureContrast, so the dot is never
// allowed to sink into the background: Lighter and Darker say which way the
// user wants the dot pushed, not that it may become invisible if pushed toward
// the background's own tone.
//
// dcmClassic is the pre-contrast behaviour, deliberately unguarded. The flat
// step it takes is applied to a color drawn from the same palette that colors
// the background, so a reading in the same band as the current one is close to
// invisible — that is what the mode reproduces, and why it is not the default.
//
// Pure helper operating on plain data for unit tests
function DotDisplayColor(Identity, Background: TColor;
  Mode: TDotColorMode): TColor;
begin
  case Mode of
  dcmClassic:
    Result := LightenColor(Identity, DOT_CLASSIC_DARKEN);
  dcmLighter:
    Result := EnsureContrast(LightenColor(Identity, DOT_TONE_SHIFT), Background,
      DOT_MIN_CONTRAST);
  dcmDarker:
    // DarkenColor's factor is the fraction of each channel *kept*, so the
    // complement of the shift is what makes the two directions symmetric.
    Result := EnsureContrast(DarkenColor(Identity, 1.0 - DOT_TONE_SHIFT),
      Background, DOT_MIN_CONTRAST);
  else
    Result := EnsureContrast(Identity, Background, DOT_MIN_CONTRAST);
  end;
end;



function TfBG.dotsInView: integer;
var
  x: TDotControl;
  infos: array of TDotInfo;
  idx, cnt: integer;
  parentHeight: integer;
begin
  // Pre-count valid dots and allocate once to avoid repeated reallocations
  cnt := 0;
  for x in TrendDots do
    if Assigned(x) then
      Inc(cnt);
  SetLength(infos, cnt);
  idx := 0;
  parentHeight := 0;
  for x in TrendDots do
  begin
    if not Assigned(x) then
      Continue;

    // Capture first valid parent height (most UIs use a single parent for dots)
    if (parentHeight = 0) and Assigned(x.Parent) and (x.Parent is TControl) then
      parentHeight := TControl(x.Parent).ClientHeight;

    infos[idx].Top := x.Top;
    infos[idx].Height := x.Height;
    infos[idx].Visible := x.Visible;
    Inc(idx);
  end;

  Result := DotsInViewFromInfos(infos, parentHeight);
end;

procedure TfBG.onGH({%H-}Sender: TObject);
begin
  OpenURL('https://github.com/slicke/trndi');
end;

{$IFDEF DARWIN}

procedure TMyAppDelegate.miSettingsMacClick(sender: id);
begin
  if Application.MainForm is TfBG then
    (Application.MainForm as TfBG).miSettingsClick(nil);
end;

function TMyAppDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
var
  dockMenu: NSMenu;
  menuItem: NSMenuItem;
  menuTitle: string;
  cfStr: CFStringRef;
begin
  // During the setup wizard MainForm is nil (or the wizard form); returning
  // nil is required here — a bare Exit leaves Result as garbage and AppKit
  // crashes dereferencing it in the event loop.
  if not (Application.MainForm is TfBG) then
    Exit(nil);

  // Create a custom dock menu
  dockMenu := NSMenu.alloc.initWithTitle(NSSTR('Trndi'));

  // Add Settings item
  menuTitle := TrimLeftSet((Application.MainForm as TfBG).miSettings.Caption, ['&', ' ']);
  cfStr := CFStringCreateWithCString(nil, pchar(menuTitle), kCFStringEncodingUTF8);
  menuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(
    NSString(cfStr), sel_registerName('miSettings:'), NSSTR(''));
  dockMenu.addItem(menuItem);
  menuItem.release;
  CFRelease(cfStr);

  // Add separator
  dockMenu.addItem(NSMenuItem.separatorItem);

  // Add Quit item
  menuTitle := 'Quit Trndi';
  cfStr := CFStringCreateWithCString(nil, pchar(menuTitle), kCFStringEncodingUTF8);
  menuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(
    NSString(cfStr), sel_registerName('terminate:'), NSSTR('q'));
  dockMenu.addItem(menuItem);
  menuItem.release;
  CFRelease(cfStr);

  // AppKit only retains the returned menu while it is shown; hand our
  // owning reference to the autorelease pool or a menu leaks per right-click.
  Result := NSMenu(dockMenu.autorelease);
end;

function TMyAppDelegate.applicationShouldHandleReopen_hasVisibleWindows(sender: NSApplication; hasVisibleWindows: boolean): boolean;
begin
  // Show main form when dock icon is clicked
  Result := true;
  if not Assigned(Application.MainForm) then
    Exit;
  Application.MainForm.Show;
  Application.MainForm.BringToFront;
end;
{$ENDIF}

procedure TfBG.AppExceptionHandler({%H-}Sender: TObject; E: Exception);
begin
  // Handle exceptions during shutdown gracefully
end;

procedure ShowMessage(const str: string);
begin
  if semiTouchMode then
    SlickeMessage(sSuccTitle, str, uxmtInformation, fBG)
  else
    SlickeMessage(sdsOnForm, sSuccTitle, str, uxmtInformation, fBG);
end;

procedure ShowMessage(const title, str: string);
begin
  if semiTouchMode then
    SlickeMessage(title, str, uxmtInformation, fBG)
  else
    SlickeMessage(sdsOnForm, title, str, uxmtInformation, fBG);
end;

{------------------------------------------------------------------------------
  Hints
  ----------------------
  Informational popups explaining what a part of the window does, each of which
  the user can silence for good with the dialog's "Don't show again" button.

  Two levels of suppression, both settable from the Customization tab: the
  master 'ux.hints' switch, and the list of individually dismissed hints. The
  master switch deliberately does not clear that list - turning hints back on
  restores the ones that were never dismissed, and nothing more.

  The dismissed hints are one CSV setting rather than a key each, so "show
  hidden explanations" in the settings dialog is a single delete and needs no
  registry of every hint name that has ever existed.
 ------------------------------------------------------------------------------}
function HintDismissed(const key: string): boolean;
var
  hidden: TStringArray;
  seen: string;
begin
  Result := false;
  if not native.TryGetCSVSetting(HINTS_HIDDEN_KEY, hidden) then
    Exit;
  for seen in hidden do
    if seen = key then
      Exit(true);
end;

procedure DismissHint(const key: string);
var
  hidden: TStringArray;
begin
  if not native.TryGetCSVSetting(HINTS_HIDDEN_KEY, hidden) then
    hidden := [];
  Insert(key, hidden, Length(hidden));
  native.SetCSVSetting(HINTS_HIDDEN_KEY, hidden);
end;

procedure ShowOnce(const key, title, str: string);
begin
  if not native.GetBoolSetting(HINTS_ENABLED_KEY, true) then
    Exit;
  if HintDismissed(key) then
    Exit;

  // Modal, unlike the ShowMessage overloads above. Those reach SlickeMessage,
  // whose touch path draws a modeless overlay on the form and returns before
  // the user has touched anything, so a button result could never come back.
  if SlickeDialog(sdsAuto, title, str,
    [[mbOK, mbSlickeNever], [mbSlickeNever, mbOK]], uxmtInformation) = mrSlickeNever then
    DismissHint(key);
end;

procedure ShowOnce(const key, str: string);
begin
  ShowOnce(key, sSuccTitle, str);
end;

// Helper function to get validated position setting
function TfBG.GetValidatedPosition: TrndiPos;
var
  posValue: integer;
begin
  posValue := native.GetIntSetting('position.main', Ord(tpoCenter));
  
  // Validate position type
  if (posValue >= Ord(Low(TrndiPos))) and (posValue <= Ord(High(TrndiPos))) then
    Result := TrndiPos(posValue)
  else
    Result := tpoCenter; // Default fallback
end;

procedure TfBG.placeForm;
var
  posValue: TrndiPos;
begin
  if native.GetBoolSetting('size.main') then
  begin
    Width := native.GetIntSetting('size.last.width', Width);
    Height := native.GetIntSetting('size.last.height', Height);
  end;
  
  // Get and validate position
  posValue := GetValidatedPosition;

  // Handle positioning
  case posValue of
  tpoCenter:
  begin
    Left := Screen.WorkAreaLeft + (Screen.WorkAreaWidth - Width) div 2;
    Top := Screen.WorkAreaTop + (Screen.WorkAreaHeight - Height) div 2;
  end;

  tpoBottomLeft:
  begin
    Left := 20;
    Top := Screen.WorkAreaRect.Bottom - Height - 200;
  end;

  tpoBottomRight:
  begin
    Left := Screen.WorkAreaRect.Right - Width - 20;
    Top := Screen.WorkAreaRect.Bottom - Height - 200;
  end;

  tpoTopRight:
  begin
    Left := Screen.WorkAreaRect.Right - Width - 20;
    Top := 200;
  end;

  tpoCustom:
  begin
    Left := native.GetIntSetting('position.last.left', 10);
    Top := native.GetIntSetting('position.last.top', 10);
    
    // Validate position is within screen bounds
    // Ensure at least 50 pixels of the window is visible
    if Left < Screen.DesktopLeft - Width + 50 then
      Left := Screen.DesktopLeft;
    if Left > Screen.DesktopWidth - 50 then
      Left := Screen.DesktopWidth - Width;
    if Top < Screen.DesktopTop - Height + 50 then
      Top := Screen.DesktopTop;
    if Top > Screen.DesktopHeight - 50 then
      Top := Screen.DesktopHeight - Height;
  end;
  end;

  tClock.Enabled := native.GetBoolSetting('main.clock', false);
  tSwap.Enabled  := native.GetBoolSetting('main.alternate', false);

  if native.GetBoolSetting('main.ontop', false) then
    Self.FormStyle := fsStayOnTop;

  if native.GetBoolSetting('main.noborders', false) then
    Self.BorderStyle := bsNone;
end;
