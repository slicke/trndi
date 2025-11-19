unit NSStringUtils;

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  SysUtils,
{$IF (DEFINED(IPHONESIM) OR DEFINED(CPUARM) OR DEFINED(CPUAARCH64)) AND (NOT DEFINED(LCLCOCOA)) }
 {$IFDEF NoiPhoneAll}
  Foundation;
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
var
  Utf8Value: UTF8String;
begin
  Utf8Value := UTF8String(S);
  Result := NSString.stringWithUTF8String(PAnsiChar(Utf8Value));
end;

function NSStringFromUTF8(const S: AnsiString): NSString;
var
  Utf8Value: UTF8String;
begin
  Utf8Value := UTF8String(S);
  Result := NSString.stringWithUTF8String(PAnsiChar(Utf8Value));
end;

function NSStringToString(const S: NSString): string;
var
  Utf8Value: UTF8String;
begin
  if S = nil then
    Exit('');
  Utf8Value := UTF8String(S.UTF8String);
  Result := string(Utf8Value);
end;

function NSStringToUTF8(const S: NSString): AnsiString;
begin
  if S = nil then
    Exit('');
  Result := AnsiString(UTF8String(S.UTF8String));
end;

end.
