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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)
// Main form helper functions

// Returns vertical offset needed to bring all trend dots fully inside their parent.
// Sign convention: negative = dots are above (need to move down); positive = dots are below (need to move up).
function TfBG.dotsInView: integer;
var
  x: TPaintBox;
  bottom, overflow: integer;
  adjustSide: TTrndiBool;
const
  Tol = 5; // pixel tolerance to ignore tiny rounding differences
begin
  Result := 0;
  adjustSide := tbUnknown;
  // We can have both hi and lows; lock to first side encountered

  for x in TrendDots do
  begin
    // Ignore dots not currently visible
    if not x.Visible then
      Continue;
    // Use the actual control size, not font metrics, to determine visibility
    bottom := x.Top + x.Height;

    // Out-of-top (negative top)
    if (x.Top < -Tol) then
    begin
      if adjustSide in [tbTrue, tbUnknown] then
      begin
        // Pick the smallest (most negative) top
        Result := Min(Result, x.Top);
        adjustSide := tbTrue;
      end;
    end
    // Out-of-bottom
    else
    if (bottom > x.Parent.ClientHeight + Tol) then
      if adjustSide in [tbFalse, tbUnknown] then
      begin
        // Compute actual overflow beyond tolerance to avoid false positives
        overflow := (bottom - (x.Parent.ClientHeight + Tol));
        // Pick the largest overflow
        Result := Max(Result, overflow);
        adjustSide := tbFalse;
      end;
  end;

end;

procedure TfBG.onGH(Sender: TObject);
begin
  OpenURL('https://github.com/slicke/trndi');
end;

{$IFDEF DARWIN}

procedure TMyAppDelegate.miSettingsMacClick(sender: id);
begin
  (Application.MainForm as TfBG).miSettingsClick(nil);
end;

function TMyAppDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
var
  dockMenu: NSMenu;
  menuItem: NSMenuItem;
  s: string;
  cfStr: CFStringRef;
begin
  // Create a custom dock menu
  dockMenu := NSMenu.alloc.initWithTitle(NSSTR('Trndi'));

  // Add items to the menu
  s := TrimLeftSet((Application.MainForm as TfBG).miSettings.Caption, ['&', ' ']);

  // Create a CFStringRef directly from the UTF-8 string
  cfStr := CFStringCreateWithCString(nil, pchar(s), kCFStringEncodingUTF8);

  // Initialize the menu item with the CFStringRef
  menuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(
    NSString(cfStr), sel_registerName('miSettings:'), NSSTR(''));

  dockMenu.addItem(menuItem);
  menuItem.release;

  // Release the CFStringRef
  CFRelease(cfStr);

  Result := dockMenu;
end;

function TMyAppDelegate.applicationShouldHandleReopen_hasVisibleWindows(sender: NSApplication; hasVisibleWindows: boolean): boolean;
begin
  // Show main form when dock icon is clicked
  Application.MainForm.Show;
  Application.MainForm.BringToFront;
  Result := true;

end;
{$ENDIF}

procedure TfBG.AppExceptionHandler(Sender: TObject; E: Exception);
begin
  // Handle exceptions during shutdown gracefully
end;

procedure ShowMessage(const str: string);
begin
  UXMessage(uxdOnForm, sSuccTitle, str, uxmtInformation, fBG);
end;

procedure ShowMessage(const title, str: string);
begin
  UXMessage(uxdOnForm, title, str, uxmtInformation, fBG);
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
{$ifdef DARWIN}
function GetActiveScreen: TMonitor; // Use TMonitor if that's what Screen.Monitors returns
  var
    ScreenObject: NSScreen;
    i: integer;
  begin
    Result := nil;
    if Assigned(NSApplication.sharedApplication.mainWindow) then
    begin
      ScreenObject := NSApplication.sharedApplication.mainWindow.screen;
      if Assigned(ScreenObject) then
        for i := 0 to Screen.MonitorCount - 1 do
      // Add your comparison logic here (e.g., by frame bounds or display ID)
// Example (pseudo-code):
// if Screen.Monitors[i].Rect = NSRectToRect(ScreenObject.frame) then
//   Result := Screen.Monitors[i];
// Attempt to match NSScreen to the TMonitor by comparing frame or unique ID
      ;
    end;
  end;
var
  activemonitor: TMonitor;
  {$endif}
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
  end;
  end;

  if native.GetBoolSetting('main.clock') then
    miClock.OnClick(self);

  if native.GetBoolSetting('main.announce') then
    miAnnounce.OnClick(self);

  {$ifdef DARWIN}
  ActiveMonitor := GetActiveScreen;
  if Assigned(ActiveMonitor) then
  begin
    Left := ActiveMonitor.BoundsRect.Left + (ActiveMonitor.WorkAreaRect.Width - Width) div 2;
    Top := ActiveMonitor.BoundsRect.Top + (ActiveMonitor.WorkAreaRect.Height - Height) div 2;
    Exit;
  end;
  {$endif}
end;
