unit NSMisc;

{
  Misc. Foundation-based routines.

  Author:    Phil Hess.
  Copyright: Copyright 2012 Phil Hess.
  License:   Modified LGPL (see Free Pascal's rtl/COPYING.FPC).
             This means you can link your code to this compiled unit (statically 
             in a standalone executable or dynamically in a library) without 
             releasing your code. Only changes to this unit need to be made 
             publicly available.
}

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  SysUtils,
{$IF (DEFINED(IPHONESIM) OR DEFINED(CPUARM) OR DEFINED(CPUAARCH64)) AND (NOT DEFINED(LCLCOCOA)) }  //iOS
 {$IFDEF NoiPhoneAll}
  Foundation
 {$ELSE}
  iPhoneAll
 {$ENDIF}
{$ELSE}  //macOS
 {$IFDEF NoCocoaAll}
  Foundation
 {$ELSE}
  CocoaAll
 {$ENDIF}
{$ENDIF};

function GetInfoPlistString(const KeyName : string) : string;

function GetInfoPlistUTF8String(const KeyName : string) : AnsiString;


function GetPrefString(const KeyName : string) : string;

function GetPrefUTF8String(const KeyName : string) : string;

procedure SetPrefString(const KeyName : string;
                        const Value   : string);

procedure SetPrefUTF8String(const KeyName : string;
                            const Value   : AnsiString);


function GetResourcesPath : string;

function GetDocumentsPath : string;


implementation

{$IFDEF LCLCOCOA}
uses
  CocoaUtils;
{$ENDIF}

function StringToNSString(const Value: string): NSString;
begin
{$IFDEF LCLCOCOA}
  Result := CocoaUtils.StrToNSStr(Value);
{$ELSE}
  Result := NSString.stringWithUTF8String(PAnsiChar(UTF8String(Value)));
{$ENDIF}
end;

function Utf8ToNSString(const Value: AnsiString): NSString;
begin
{$IFDEF LCLCOCOA}
  Result := CocoaUtils.StrToNSStr(string(Value));
{$ELSE}
  Result := NSString.stringWithUTF8String(PAnsiChar(Value));
{$ENDIF}
end;

function NSStringToStringSafe(const Value: NSString): string;
begin
  if Value = nil then
    Exit('');
{$IFDEF LCLCOCOA}
  Result := CocoaUtils.NSStringToString(Value);
{$ELSE}
  Result := string(UTF8Decode(UTF8String(Value.UTF8String)));
{$ENDIF}
end;

function NSStringToUtf8(const Value: NSString): AnsiString;
begin
  if Value = nil then
    Exit('');
{$IFDEF LCLCOCOA}
  Result := UTF8String(CocoaUtils.NSStringToString(Value));
{$ELSE}
  Result := AnsiString(UTF8String(Value.UTF8String));
{$ENDIF}
end;

function GetInfoPlistString(const KeyName : string) : string;
 {Retrieve key's string value from app bundle's Info.plist file.}
begin
  Result := NSStringToStringSafe(NSBundle.mainBundle.objectForInfoDictionaryKey(
                        StringToNSString(KeyName)));
end;


function GetInfoPlistUTF8String(const KeyName : string) : AnsiString;
 {Retrieve key's string value from app bundle's Info.plist file as UTF8.}
begin
  Result := NSStringToUtf8(NSBundle.mainBundle.objectForInfoDictionaryKey(
             StringToNSString(KeyName)));
end;


function GetPrefString(const KeyName : string) : string;
 {Retrieve key's string value from preferences.}
begin
  Result := NSStringToStringSafe(NSUserDefaults.standardUserDefaults.stringForKey(
                        StringToNSString(KeyName)));
end;


function GetPrefUTF8String(const KeyName : string) : AnsiString;
 {Retrieve key's string value from preferences as UTF8.}
begin
  Result := NSStringToUtf8(NSUserDefaults.standardUserDefaults.stringForKey(
             StringToNSString(KeyName)));
end;


procedure SetPrefString(const KeyName : string;
                        const Value   : string);
 {Set key's string value in preferences.}
begin
  NSUserDefaults.standardUserDefaults.setObject_forKey(
  StringToNSString(Value), StringToNSString(KeyName));
end;


procedure SetPrefUTF8String(const KeyName : string;
                            const Value   : AnsiString);
 {Set key's string value in preferences using UTF8 data.}
begin
  NSUserDefaults.standardUserDefaults.setObject_forKey(
  Utf8ToNSString(Value), StringToNSString(KeyName));
end;


function GetResourcesPath : string;
 {On OS X, returns path to Resources folder in app bundle;
   on iOS, returns path to app bundle.
  If called from console app, returns app executable's folder.}
begin
  Result := NSStringToStringSafe(NSBundle.mainBundle.resourcePath);
end;


function GetDocumentsPath : string;
 {On OS X, returns path to user's Documents folder;
   on iOS, returns path to app's Documents folder.
  Can also use URLForDirectory_inDomain_appropriateForURL_create_error
   on OS X 10.6, iOS 4.0 and later.}
var
  paths : NSArray;
begin
  paths := NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, True);
  Result := NSStringToStringSafe(NSString(paths.objectAtIndex(0)));
end;


end.
