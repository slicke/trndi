unit NSStringUtils;
// (c) slicke. 2025, LGPL License, with FPC linking exception.
{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  SysUtils,
{$IF (DEFINED(IPHONESIM) OR DEFINED(CPUARM) OR DEFINED(CPUAARCH64)) AND (NOT DEFINED(LCLCOCOA)) }
 {$IFDEF NoiPhoneAll}
  Foundation,
  CocoaUtils;
 {$ELSE}
  iPhoneAll;
 {$ENDIF}
{$ELSE}
 {$IFDEF NoCocoaAll}
  Foundation;
 {$ELSE}
  CocoaAll;
 {$ENDIF}
{$ENDIF}

function NSStringFromString(const S: string): NSString;
function NSStringFromUTF8(const S: AnsiString): NSString;
function NSStringToString(const S: NSString): string;
function NSStringToUTF8(const S: NSString): AnsiString;

implementation

function NSStringFromString(const S: string): NSString;
begin
  Result := StrToNSStr(S);
end;

function NSStringFromUTF8(const S: AnsiString): NSString;
begin
  Result := StrToNSStr(string(S));
end;

function NSStringToString(const S: NSString): string;
begin
  Result := CocoaUtils.NSStringToString(S);
end;

function NSStringToUTF8(const S: NSString): AnsiString;
begin
  Result := UTF8String(CocoaUtils.NSStringToString(S));
end;

end.
