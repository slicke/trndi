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
