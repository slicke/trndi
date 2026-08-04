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
unit nsutils.simpledarkmode;

{$mode objfpc}{$H+}
{$modeswitch cvar}
{$linkframework Cocoa}
{$linkframework CoreFoundation}

interface

procedure EnableAppDarkMode;

implementation

uses
SysUtils;

const
ObjCLib = '/usr/lib/libobjc.A.dylib';
CFLib   = '/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation';
kCFStringEncodingUTF8 = $08000100;

type
id  = Pointer;
SEL = Pointer;

//— Typed imports of objc_msgSend with different arities —//
// 0-param (for –sharedApplication)
function objc_msgSend0(obj: id; sel: SEL): id; cdecl; external ObjCLib name 'objc_msgSend';
// 1-param (for –setAppearance:)
function objc_msgSend1(obj: id; sel: SEL; p1: id): id; cdecl; external ObjCLib name 'objc_msgSend';

// Runtime and CF APIs
function objc_getClass(name: MarshaledAString): id;        cdecl; external ObjCLib;
function sel_registerName(name: MarshaledAString): SEL;    cdecl; external ObjCLib;
function CFStringCreateWithCString(
alloc: Pointer;
cStr: MarshaledAString;
encoding: longword
): Pointer; cdecl; external CFLib;

procedure EnableAppDarkMode;
var
  selSharedApp, selAppearNamed, selSetAppear: SEL;
  NSAppClass, AppClass: id;
  NSApp, DarkName, DarkApp: id;
begin
  {$ifdef LCLCOCOA}
  // 1) Prepare selectors
  selSharedApp      := sel_registerName('sharedApplication');
  selAppearNamed    := sel_registerName('appearanceNamed:');
  selSetAppear      := sel_registerName('setAppearance:');

  // 2) [NSApplication sharedApplication]
  NSAppClass := objc_getClass('NSApplication');
  NSApp      := objc_msgSend0(NSAppClass, selSharedApp);
  if NSApp = nil then Exit;

  // 3) Create an NSString* for the constant name
  DarkName := CFStringCreateWithCString(
    nil,
    MarshaledAString('NSAppearanceNameDarkAqua'),
    kCFStringEncodingUTF8
  );
  if DarkName = nil then Exit;

  // 4) [NSAppearance appearanceNamed: DarkName]
  AppClass := objc_getClass('NSAppearance');
  DarkApp  := objc_msgSend1(AppClass, selAppearNamed, DarkName);
  if DarkApp = nil then Exit;

  // 5) [NSApp setAppearance: DarkApp]
  objc_msgSend1(NSApp, selSetAppear, DarkApp);
  {$endif}
end;

end.
