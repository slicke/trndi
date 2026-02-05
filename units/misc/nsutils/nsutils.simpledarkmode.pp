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
