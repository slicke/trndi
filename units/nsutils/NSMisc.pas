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
  Foundation,
 {$ELSE}
  iPhoneAll,
 {$ENDIF}
{$ELSE}  //macOS
 {$IFDEF NoCocoaAll}
  Foundation,
 {$ELSE}
  CocoaAll,
 {$ENDIF}
{$ENDIF}
  NSStringUtils;

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

function GetInfoPlistString(const KeyName : string) : string;
 {Retrieve key's string value from app bundle's Info.plist file.}
begin
  Result := NSStringToString(NSBundle.mainBundle.objectForInfoDictionaryKey(
                        NSStringFromString(KeyName)));
end;


function GetInfoPlistUTF8String(const KeyName : string) : AnsiString;
 {Retrieve key's string value from app bundle's Info.plist file as UTF8.}
begin
  Result := NSBundle.mainBundle.objectForInfoDictionaryKey(
             NSStringFromString(KeyName)).UTF8String;
end;


function GetPrefString(const KeyName : string) : string;
 {Retrieve key's string value from preferences.}
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(
                        NSStringFromString(KeyName)));
end;


function GetPrefUTF8String(const KeyName : string) : AnsiString;
 {Retrieve key's string value from preferences as UTF8.}
begin
  Result := NSUserDefaults.standardUserDefaults.stringForKey(
             NSStringFromString(KeyName)).UTF8String;
end;


procedure SetPrefString(const KeyName : string;
                        const Value   : string);
 {Set key's string value in preferences.}
begin
  NSUserDefaults.standardUserDefaults.setObject_forKey(
  NSStringFromString(Value), NSStringFromString(KeyName));
end;


procedure SetPrefUTF8String(const KeyName : string;
                            const Value   : AnsiString);
 {Set key's string value in preferences using UTF8 data.}
begin
  NSUserDefaults.standardUserDefaults.setObject_forKey(
  NSStringFromUTF8(Value), NSStringFromString(KeyName));
end;


function GetResourcesPath : string;
 {On OS X, returns path to Resources folder in app bundle;
   on iOS, returns path to app bundle.
  If called from console app, returns app executable's folder.}
begin
  Result := NSStringToString(NSBundle.mainBundle.resourcePath);
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
  Result := NSStringToString(paths.objectAtIndex(0));
end;


end.
