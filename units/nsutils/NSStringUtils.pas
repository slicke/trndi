unit NSStringUtils;
// (c) slicke. 2025, LGPL License, with FPC linking exception.
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
  Utf8Value: RawByteString;
begin
  Utf8Value := RawByteString(UTF8Encode(S));
  Result := NSString.stringWithUTF8String(PAnsiChar(Utf8Value));
end;

function NSStringFromUTF8(const S: AnsiString): NSString;
var
  Utf8Value: RawByteString;
begin
  Utf8Value := RawByteString(S);
  Result := NSString.stringWithUTF8String(PAnsiChar(Utf8Value));
end;

function NSStringToString(const S: NSString): string;
begin
  if S = nil then
    Exit('');
  Result := string(NSStringToUTF8(S));
end;

function NSStringToUTF8(const S: NSString): AnsiString;
var
  Ptr: PAnsiChar;
begin
  if S = nil then
    Exit('');
  Ptr := S.UTF8String;
  if Ptr = nil then
    Exit('');
  Result := StrPas(Ptr);
end;

end.
