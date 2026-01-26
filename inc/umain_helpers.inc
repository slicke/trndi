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


// Stops flashing the tray
procedure StopFlashing;
begin
  // Acknowledge active high/low alert by user click
  if native <> nil then
  begin
    if (not highAlerted) and (fBG.Color = bg_color_hi) then
      highAlerted := true;
    if (not lowAlerted) and (fBG.Color = bg_color_lo) then
      lowAlerted := true;
    // Stop flashing if any
    native.StopBadgeFlash;
  end;
end;

// Returns vertical offset needed to bring all trend dots fully inside their parent.
// Sign convention: negative = dots are above (need to move down); positive = dots are below (need to move up).
function TfBG.dotsInView: integer;
var
  x: TDotControl;
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

// ObjC runtime / CoreFoundation imports for Apple Event handling
const
  ObjCLib = '/usr/lib/libobjc.A.dylib';
  CFLib   = '/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation';
  kCFStringEncodingUTF8 = $08000100;

type
  id  = Pointer;
  SEL = Pointer;

// ObjC runtime helpers
function objc_getClass(name: MarshaledAString): id;        cdecl; external ObjCLib;
function sel_registerName(name: MarshaledAString): SEL;    cdecl; external ObjCLib;
function CFStringCreateWithCString(alloc: Pointer; cStr: MarshaledAString; encoding: longword): Pointer; cdecl; external CFLib;
procedure CFRelease(cf: Pointer); cdecl; external CFLib;

function objc_msgSend0(obj: id; sel: SEL): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend1(obj: id; sel: SEL; p1: id): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend2(obj: id; sel: SEL; p1: id; p2: UInt32): id; cdecl; external ObjCLib name 'objc_msgSend';
function objc_msgSend4(obj: id; sel: SEL; p1: id; p2: SEL; p3: UInt32; p4: UInt32): id; cdecl; external ObjCLib name 'objc_msgSend';

// Helper to encode four-char OSType codes
function FourCharCode(const s: string): UInt32;
begin
  if Length(s) <> 4 then Exit(0);
  Result := (Ord(s[1]) shl 24) or (Ord(s[2]) shl 16) or (Ord(s[3]) shl 8) or Ord(s[4]);
end;

procedure TMyAppDelegate.miSettingsMacClick(sender: id);
begin
  (Application.MainForm as TfBG).miSettingsClick(nil);
end;

function TMyAppDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
var
  dockMenu: NSMenu;
  menuItem: NSMenuItem;
  menuTitle: string;
  cfStr: CFStringRef;
begin
  // Create a custom dock menu
  dockMenu := NSMenu.alloc.initWithTitle(NSSTR('Trndi'));

  // Add items to the menu
  menuTitle := TrimLeftSet((Application.MainForm as TfBG).miSettings.Caption, ['&', ' ']);

  // Create a CFStringRef directly from the UTF-8 string
  cfStr := CFStringCreateWithCString(nil, pchar(menuTitle), kCFStringEncodingUTF8);

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

// ---------------- AppleScript handlers ----------------
// These handlers set a string result into the reply event (as a simple PoC).
procedure TMyAppDelegate.handleGetCurrentUser_withReplyEvent_(command: id; replyEvent: id);
var
  userName: string;
  cfStr: Pointer;
  desc: id;
  selSetDesc: SEL;
begin
  userName := (Application.MainForm as TfBG).multinick;
  cfStr := CFStringCreateWithCString(nil, pchar(userName), kCFStringEncodingUTF8);
  desc := objc_msgSend1(objc_getClass('NSAppleEventDescriptor'), sel_registerName('descriptorWithString:'), cfStr);
  selSetDesc := sel_registerName('setDescriptor:forKeyword:');
  objc_msgSend2(replyEvent, selSetDesc, desc, FourCharCode('----'));
  CFRelease(cfStr);
end;

procedure TMyAppDelegate.handleGetCurrentNickname_withReplyEvent_(command: id; replyEvent: id);
var
  nick: string;
  cfStr: Pointer;
  desc: id;
  selSetDesc: SEL;
begin
  nick := (Application.MainForm as TfBG).multinick;
  cfStr := CFStringCreateWithCString(nil, pchar(nick), kCFStringEncodingUTF8);
  desc := objc_msgSend1(objc_getClass('NSAppleEventDescriptor'), sel_registerName('descriptorWithString:'), cfStr);
  selSetDesc := sel_registerName('setDescriptor:forKeyword:');
  objc_msgSend2(replyEvent, selSetDesc, desc, FourCharCode('----'));
  CFRelease(cfStr);
end;

procedure TMyAppDelegate.handleGetCurrentReading_withReplyEvent_(command: id; replyEvent: id);
var
  bg: BGReading;
  ok: boolean;
  payload: string;
  cfStr: Pointer;
  desc: id;
  selSetDesc: SEL;
  jo: TJSONObject;
  fs: TFormatSettings;
begin
  ok := (Application.MainForm as TfBG).tryLastReading(bg);
  if not ok then
  begin
    // No reading available — keep legacy behaviour
    payload := 'false';
  end
  else
  begin
    fs.DecimalSeparator := '.';
    jo := TJSONObject.Create;
    try
      jo.Add('value_system', FormatFloat('0.0', bg.convert(mmol, BGPrimary), fs));
      jo.Add('value_mgdl', Round(bg.val));
      jo.Add('value_mmol', FormatFloat('0.00', bg.convert(mmol, BGPrimary), fs));
      jo.Add('delta_mgdl', Round(bg.delta));
      jo.Add('delta_mmol', FormatFloat('0.00', bg.convert(mmol, BGDelta), fs));
      jo.Add('direction', bg.direction);
      jo.Add('timestamp', DateTimeToStr(bg.date));
      jo.Add('age_seconds', SecondsBetween(Now, bg.date));
      payload := jo.AsJSON;
    finally
      jo.Free;
    end;
  end;

  cfStr := CFStringCreateWithCString(nil, pchar(payload), kCFStringEncodingUTF8);
  desc := objc_msgSend1(objc_getClass('NSAppleEventDescriptor'), sel_registerName('descriptorWithString:'), cfStr);
  selSetDesc := sel_registerName('setDescriptor:forKeyword:');
  objc_msgSend2(replyEvent, selSetDesc, desc, FourCharCode('----'));
  CFRelease(cfStr);
end;

procedure TMyAppDelegate.handlePredictReadings_withReplyEvent_(command: id; replyEvent: id);
var
  preds: BGResults;
  payload: string;
  cfStr: Pointer;
  desc: id;
  selSetDesc: SEL;
  ja: TJSONArray;
  jo: TJSONObject;
  fs: TFormatSettings;
  i: integer;
begin
  preds := (Application.MainForm as TfBG).GetPredictionsForWeb();
  if (Length(preds) = 0) then
  begin
    jo := TJSONObject.Create;
    try
      jo.Add('status', 'not available');
      payload := jo.AsJSON;
    finally
      jo.Free;
    end;
  end
  else
  begin
    fs.DecimalSeparator := '.';
    ja := TJSONArray.Create;
    try
      for i := Low(preds) to High(preds) do
      begin
        jo := TJSONObject.Create;
        jo.Add('mgdl', round(preds[i].val));
        jo.Add('mmol', FormatFloat('0.00', preds[i].convert(mmol, BGPrimary), fs));
        jo.Add('timestamp', DateTimeToStr(preds[i].date));
        ja.Add(jo);
      end;
      payload := ja.AsJSON;
    finally
      ja.Free;
    end;
  end;

  cfStr := CFStringCreateWithCString(nil, pchar(payload), kCFStringEncodingUTF8);
  desc := objc_msgSend1(objc_getClass('NSAppleEventDescriptor'), sel_registerName('descriptorWithString:'), cfStr);
  selSetDesc := sel_registerName('setDescriptor:forKeyword:');
  objc_msgSend2(replyEvent, selSetDesc, desc, FourCharCode('----'));
  CFRelease(cfStr);
end;
end;

{$ENDIF}

procedure TfBG.AppExceptionHandler(Sender: TObject; E: Exception);
begin
  // Handle exceptions during shutdown gracefully
end;

procedure ShowMessage(const str: string);
begin
  if semiTouchMode then
    UXMessage(sSuccTitle, str, uxmtInformation, fBG)
  else
    UXMessage(uxdOnForm, sSuccTitle, str, uxmtInformation, fBG);
end;

procedure ShowMessage(const title, str: string);
begin
  if semiTouchMode then
    UXMessage(title, str, uxmtInformation, fBG)
  else
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
