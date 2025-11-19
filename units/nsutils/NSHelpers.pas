unit NSHelpers;

{
  Unit of handy routines for use with Foundation and Core Foundation.

  Author:    Phil Hess.
  Copyright: Copyright 2011 Phil Hess.
  License:   Modified LGPL. This means you can link your code to this
             compiled unit (statically in a standalone executable or 
             dynamically in a library) without releasing your code. Only
             changes to this unit need to be made publicly available.

  CFStrToAnsiStr was adapted from the Lazarus CarbonProc unit's
   CFStringToStr function.

  Note that objects returned by functions with "Create" or "Copy"
   in the function name need to be released by the calling code.
   For example, CFStringCreateWithCString is called in AnsiStrToCFStr,
   meaning this applies to code that calls AnsiStrToCFStr as well.
  FreeCFRef and FreeAndNilCFRef are convenience routines provided 
   for that purpose.
  See Apple docs for more information on the so-called Create Rule 
   and Get Rule: 
  https://developer.apple.com/library/mac/#documentation/CoreFoundation/
          Conceptual/CFMemoryMgmt/Concepts/Ownership.html

  (c) slicke. Classified and updated by Björn Lindh, November 2025
}

{$MODE Delphi}
{$modeswitch ObjectiveC1}

interface

uses
  CFBase,
  CFString,
  {$IF (DEFINED(IPHONESIM) OR DEFINED(CPUARM) OR DEFINED(CPUAARCH64)) AND (NOT DEFINED(LCLCOCOA)) }  //iOS
 {$IFDEF NoiPhoneAll}
  Foundation;
 {$ELSE}
  iPhoneAll;
 {$ENDIF}
{$ELSE}  //macOS
 {$IFDEF NoCocoaAll}
  Foundation;
 {$ELSE}
  CocoaAll;
 {$ENDIF}
{$ENDIF}
  
 {Routines to convert CFString to and from:
   - AnsiString with specified encoding
   - string with specified encoding
   - UTF8-encoded AnsiString
   - WideString
   - UnicodeString}
function CFStrToAnsiStr(cfStr    : CFStringRef;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;

procedure AnsiStrToCFStr(const aStr     : AnsiString;
                           out cfStr    : CFStringRef;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
                               
function CFStrToStr(cfStr    : CFStringRef;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;

procedure StrToCFStr(const aStr     : string;
                       out cfStr    : CFStringRef;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
                               
function CFStrToUtf8Str(cfStr : CFStringRef): AnsiString;

procedure Utf8StrToCFStr(const aStr  : AnsiString;
                           out cfStr : CFStringRef);
                               
function CFStrToWideStr(cfStr : CFStringRef): WideString;

procedure WideStrToCFStr(const aStr  : WideString;
                           out cfStr : CFStringRef);

function CFStrToUniStr(cfStr : CFStringRef): UnicodeString;

procedure UniStrToCFStr(const aStr  : UnicodeString;
                          out cfStr : CFStringRef);

procedure FreeCFRef(var cfRef: CFTypeRef);

procedure FreeAndNilCFRef(var cfRef : CFTypeRef);


 {Routines to convert NSString to and from: 
   - AnsiString with specified encoding
   - string with specified encoding
   - UTF8-encoded AnsiString
   - WideString
   - UnicodeString}
function NSStrToAnsiStr(aNSStr   : NSString;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;

procedure AnsiStrToNSStr(const aStr     : AnsiString;
                           out aNSStr   : NSString;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1); overload;

function AnsiStrToNSStr(const aStr     : AnsiString;
                              encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString; overload;                             
                               

function NSStrToStr(aNSStr   : NSString;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;

procedure StrToNSStr(const aStr     : string;
                       out aNSStr   : NSString;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1); overload;
                               
function StrToNSStr(const aStr     : string;
                          encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString; overload;
                               

function NSStrToUtf8Str(aNSStr : NSString): AnsiString;

procedure Utf8StrToNSStr(const aStr   : AnsiString;
                           out aNSStr : NSString); overload;

function Utf8StrToNSStr(const aStr : AnsiString) : NSString; overload;


function NSStrToWideStr(aNSStr : NSString): WideString;

procedure WideStrToNSStr(const aStr   : WideString;
                           out aNSStr : NSString); overload;

function WideStrToNSStr(const aStr : WideString) : NSString; overload;


function NSStrToUniStr(aNSStr : NSString): UnicodeString;

procedure UniStrToNSStr(const aStr   : UnicodeString;
                          out aNSStr : NSString); overload;

function UniStrToNSStr(const aStr : UnicodeString) : NSString; overload;


implementation

function CFStringHasContent(const cfStr: CFStringRef; out range: CFRange): Boolean;
begin
  range.location := 0;
  if cfStr = nil then
    begin
    range.length := 0;
    Exit(False);
    end;

  range.length := CFStringGetLength(cfStr);
  Result := range.length > 0;
end;

function CFStringToAnsiInternal(const cfStr: CFStringRef; const encoding: CFStringEncoding): AnsiString;
var
  directPtr : PAnsiChar;
  range     : CFRange;
  bufferLen : CFIndex;
  bytesUsed : CFIndex;
begin
  if cfStr = nil then
    Exit('');

  directPtr := PAnsiChar(CFStringGetCStringPtr(cfStr, encoding));
  if directPtr <> nil then
    Exit(AnsiString(directPtr));

  if not CFStringHasContent(cfStr, range) then
    Exit('');

  bufferLen := 0;
  CFStringGetBytes(cfStr, range, encoding, Ord('?'), False, nil, 0, bufferLen);
  if bufferLen <= 0 then
    Exit('');

  SetLength(Result, bufferLen);
  bytesUsed := CFStringGetBytes(cfStr, range, encoding, Ord('?'), False, @Result[1], bufferLen, bufferLen);

  if bytesUsed <= 0 then
    Result := ''
  else if bytesUsed < bufferLen then
    SetLength(Result, bytesUsed);
end;

function CFStringCreateWithBytesInternal(const buffer: PUInt8; const count: CFIndex; const encoding: CFStringEncoding): CFStringRef;
begin
  Result := CFStringCreateWithBytes(nil, buffer, count, encoding, False);
end;

function CFStringFromAnsi(const value: AnsiString; const encoding: CFStringEncoding): CFStringRef;
var
  count : CFIndex;
  bytes : PUInt8;
begin
  count := Length(value);
  if count > 0 then
    bytes := PUInt8(@value[1])
  else
    bytes := nil;

  Result := CFStringCreateWithBytesInternal(bytes, count, encoding);
end;

function NSStringWrap(const cfStr: CFStringRef): NSString;
begin
  if cfStr = nil then
    Exit(nil);
  Result := NSString(cfStr);
end;

function NSStringUnwrap(const aNSStr: NSString): CFStringRef;
begin
  Result := CFStringRef(aNSStr);
end;

function NSStringFromAnsi(const value: AnsiString; const encoding: CFStringEncoding): NSString;
begin
  Result := NSStringWrap(CFStringFromAnsi(value, encoding));
end;

function NSStringFromUtf8(const value: AnsiString): NSString;
begin
  Result := NSStringWrap(CFStringFromAnsi(value, kCFStringEncodingUTF8));
end;

function NSStringFromWide(const value: WideString): NSString;
begin
  Result := NSStringWrap(CFStringFromAnsi(UTF8Encode(value), kCFStringEncodingUTF8));
end;

function NSStringFromUnicode(const value: UnicodeString): NSString;
begin
  Result := NSStringWrap(CFStringFromAnsi(UTF8Encode(value), kCFStringEncodingUTF8));
end;


function CFStrToAnsiStr(cfStr    : CFStringRef;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;
begin
  Result := CFStringToAnsiInternal(cfStr, encoding);
end;

procedure AnsiStrToCFStr(const aStr     : AnsiString;
                           out cfStr    : CFStringRef;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
begin
  cfStr := CFStringFromAnsi(aStr, encoding);
end;

function CFStrToStr(cfStr    : CFStringRef;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;
begin
  Result := CFStrToAnsiStr(cfStr, encoding);
end;

procedure StrToCFStr(const aStr     : string;
                       out cfStr    : CFStringRef;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
begin
  AnsiStrToCFStr(aStr, cfStr, encoding);
end;

function CFStrToUtf8Str(cfStr : CFStringRef): AnsiString;
begin
  Result := CFStringToAnsiInternal(cfStr, kCFStringEncodingUTF8);
end;

procedure Utf8StrToCFStr(const aStr  : AnsiString;
                           out cfStr : CFStringRef);
begin
  cfStr := CFStringFromAnsi(aStr, kCFStringEncodingUTF8);
end;

function CFStrToWideStr(cfStr : CFStringRef): WideString;
begin
  Result := UTF8Decode(CFStringToAnsiInternal(cfStr, kCFStringEncodingUTF8));
end;

procedure WideStrToCFStr(const aStr  : WideString;
                           out cfStr : CFStringRef);
begin
  cfStr := CFStringFromAnsi(UTF8Encode(aStr), kCFStringEncodingUTF8);
end;

function CFStrToUniStr(cfStr : CFStringRef): UnicodeString;
begin
  Result := UTF8Decode(CFStringToAnsiInternal(cfStr, kCFStringEncodingUTF8));
end;

procedure UniStrToCFStr(const aStr  : UnicodeString;
                          out cfStr : CFStringRef);
begin
  cfStr := CFStringFromAnsi(UTF8Encode(aStr), kCFStringEncodingUTF8);
end;

procedure FreeCFRef(var cfRef: CFTypeRef);
begin
  if Assigned(cfRef) then
    CFRelease(cfRef);
end;

procedure FreeAndNilCFRef(var cfRef : CFTypeRef);
begin
  FreeCFRef(cfRef);
  cfRef := nil;
end;

function NSStrToAnsiStr(aNSStr   : NSString;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;
begin
  Result := CFStringToAnsiInternal(NSStringUnwrap(aNSStr), encoding);
end;

procedure AnsiStrToNSStr(const aStr     : AnsiString;
                           out aNSStr   : NSString;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
begin
  aNSStr := NSStringFromAnsi(aStr, encoding);
end;

function AnsiStrToNSStr(const aStr     : AnsiString;
                              encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString;
begin
  Result := NSStringFromAnsi(aStr, encoding);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToStr(aNSStr   : NSString;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;
begin
  Result := CFStringToAnsiInternal(NSStringUnwrap(aNSStr), encoding);
end;

procedure StrToNSStr(const aStr     : string;
                       out aNSStr   : NSString;
                           encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
begin
  AnsiStrToNSStr(aStr, aNSStr, encoding);
end;

function StrToNSStr(const aStr     : string;
                          encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString;
begin
  Result := NSStringFromAnsi(aStr, encoding);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToUtf8Str(aNSStr : NSString): AnsiString;
begin
  Result := CFStringToAnsiInternal(NSStringUnwrap(aNSStr), kCFStringEncodingUTF8);
end;

procedure Utf8StrToNSStr(const aStr   : AnsiString;
                           out aNSStr : NSString);
begin
  aNSStr := NSStringFromUtf8(aStr);
end;

function Utf8StrToNSStr(const aStr : AnsiString) : NSString;
begin
  Result := NSStringFromUtf8(aStr);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToWideStr(aNSStr : NSString): WideString;
begin
  Result := UTF8Decode(CFStringToAnsiInternal(NSStringUnwrap(aNSStr), kCFStringEncodingUTF8));
end;

procedure WideStrToNSStr(const aStr   : WideString;
                           out aNSStr : NSString);
begin
  aNSStr := NSStringFromWide(aStr);
end;

function WideStrToNSStr(const aStr : WideString) : NSString;
begin
  Result := NSStringFromWide(aStr);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToUniStr(aNSStr : NSString): UnicodeString;
begin
  Result := UTF8Decode(CFStringToAnsiInternal(NSStringUnwrap(aNSStr), kCFStringEncodingUTF8));
end;

procedure UniStrToNSStr(const aStr   : UnicodeString;
                          out aNSStr : NSString);
begin
  aNSStr := NSStringFromUnicode(aStr);
end;

function UniStrToNSStr(const aStr : UnicodeString) : NSString;
begin
  Result := NSStringFromUnicode(aStr);
  if Result <> nil then
    Result.autorelease;
end;


end.
