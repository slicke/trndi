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

const
  LossySubstitutionChar = Ord('?');

type
  TCFStringBridge = class sealed
  strict private
    class function ConvertToAnsi(const cfStr: CFStringRef; const encoding: CFStringEncoding): AnsiString; static;
    class function CreateWithBytes(const buffer: PUInt8; const count: CFIndex; const encoding: CFStringEncoding): CFStringRef; static;
    class function CreateWithChars(const chars: PUniChar; const count: CFIndex): CFStringRef; static;
    class function HasContent(const cfStr: CFStringRef; out range: CFRange): Boolean; static; inline;
  public
    class function ToAnsi(const cfStr: CFStringRef; const encoding: CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString; static; inline;
    class function ToUtf8(const cfStr: CFStringRef): AnsiString; static; inline;
    class function ToWide(const cfStr: CFStringRef): WideString; static;
    class function ToUnicode(const cfStr: CFStringRef): UnicodeString; static;
    class function FromAnsi(const value: AnsiString; const encoding: CFStringEncoding = kCFStringEncodingWindowsLatin1): CFStringRef; static;
    class function FromUtf8(const value: AnsiString): CFStringRef; static; inline;
    class function FromWide(const value: WideString): CFStringRef; static;
    class function FromUnicode(const value: UnicodeString): CFStringRef; static;
  end;

  TNSStringBridge = class sealed
  strict private
    class function Wrap(const cfStr: CFStringRef): NSString; static; inline;
    class function Unwrap(const aNSStr: NSString): CFStringRef; static; inline;
  public
    class function ToAnsi(const aNSStr: NSString; const encoding: CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString; static; inline;
    class function ToUtf8(const aNSStr: NSString): AnsiString; static; inline;
    class function ToWide(const aNSStr: NSString): WideString; static; inline;
    class function ToUnicode(const aNSStr: NSString): UnicodeString; static; inline;
    class function FromAnsi(const value: AnsiString; const encoding: CFStringEncoding = kCFStringEncodingWindowsLatin1): NSString; static;
    class function FromUtf8(const value: AnsiString): NSString; static; inline;
    class function FromWide(const value: WideString): NSString; static; inline;
    class function FromUnicode(const value: UnicodeString): NSString; static; inline;
  end;


class function TCFStringBridge.HasContent(const cfStr: CFStringRef; out range: CFRange): Boolean;
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

class function TCFStringBridge.ConvertToAnsi(const cfStr: CFStringRef; const encoding: CFStringEncoding): AnsiString;
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

  if not HasContent(cfStr, range) then
    Exit('');

  bufferLen := 0;
  CFStringGetBytes(cfStr, range, encoding, LossySubstitutionChar, False, nil, 0, bufferLen);
  if bufferLen <= 0 then
    Exit('');

  SetLength(Result, bufferLen);
  bytesUsed := CFStringGetBytes(cfStr, range, encoding, LossySubstitutionChar, False, @Result[1], bufferLen, bufferLen);

  if bytesUsed <= 0 then
    Result := ''
  else if bytesUsed < bufferLen then
    SetLength(Result, bytesUsed);
end;

class function TCFStringBridge.CreateWithBytes(const buffer: PUInt8; const count: CFIndex; const encoding: CFStringEncoding): CFStringRef;
begin
  Result := CFStringCreateWithBytes(nil, buffer, count, encoding, False);
end;

class function TCFStringBridge.CreateWithChars(const chars: PUniChar; const count: CFIndex): CFStringRef;
begin
  Result := CFStringCreateWithCharacters(nil, chars, count);
end;

class function TCFStringBridge.ToAnsi(const cfStr: CFStringRef; const encoding: CFStringEncoding): AnsiString;
begin
  Result := ConvertToAnsi(cfStr, encoding);
end;

class function TCFStringBridge.ToUtf8(const cfStr: CFStringRef): AnsiString;
begin
  Result := ConvertToAnsi(cfStr, kCFStringEncodingUTF8);
end;

class function TCFStringBridge.ToWide(const cfStr: CFStringRef): WideString;
var
  range : CFRange;
begin
  if not HasContent(cfStr, range) then
    Exit('');

  SetLength(Result, range.length);
  CFStringGetCharacters(cfStr, range, PUniChar(@Result[1]));
end;

class function TCFStringBridge.ToUnicode(const cfStr: CFStringRef): UnicodeString;
var
  range : CFRange;
begin
  if not HasContent(cfStr, range) then
    Exit('');

  SetLength(Result, range.length);
  CFStringGetCharacters(cfStr, range, PUniChar(@Result[1]));
end;

class function TCFStringBridge.FromAnsi(const value: AnsiString; const encoding: CFStringEncoding): CFStringRef;
var
  count : CFIndex;
  bytes : PUInt8;
begin
  count := Length(value);
  if count > 0 then
    bytes := PUInt8(@value[1])
  else
    bytes := nil;

  Result := CreateWithBytes(bytes, count, encoding);
end;

class function TCFStringBridge.FromUtf8(const value: AnsiString): CFStringRef;
begin
  Result := FromAnsi(value, kCFStringEncodingUTF8);
end;

class function TCFStringBridge.FromWide(const value: WideString): CFStringRef;
var
  count : CFIndex;
  chars : PUniChar;
begin
  count := Length(value);
  if count > 0 then
    chars := PUniChar(@value[1])
  else
    chars := nil;

  Result := CreateWithChars(chars, count);
end;

class function TCFStringBridge.FromUnicode(const value: UnicodeString): CFStringRef;
var
  count : CFIndex;
  chars : PUniChar;
begin
  count := Length(value);
  if count > 0 then
    chars := PUniChar(@value[1])
  else
    chars := nil;

  Result := CreateWithChars(chars, count);
end;


class function TNSStringBridge.Wrap(const cfStr: CFStringRef): NSString;
begin
  if cfStr = nil then
    Exit(nil);
  Result := NSString(cfStr);
end;

class function TNSStringBridge.Unwrap(const aNSStr: NSString): CFStringRef;
begin
  Result := CFStringRef(aNSStr);
end;

class function TNSStringBridge.ToAnsi(const aNSStr: NSString; const encoding: CFStringEncoding): AnsiString;
begin
  Result := TCFStringBridge.ToAnsi(Unwrap(aNSStr), encoding);
end;

class function TNSStringBridge.ToUtf8(const aNSStr: NSString): AnsiString;
begin
  Result := TCFStringBridge.ToUtf8(Unwrap(aNSStr));
end;

class function TNSStringBridge.ToWide(const aNSStr: NSString): WideString;
begin
  Result := TCFStringBridge.ToWide(Unwrap(aNSStr));
end;

class function TNSStringBridge.ToUnicode(const aNSStr: NSString): UnicodeString;
begin
  Result := TCFStringBridge.ToUnicode(Unwrap(aNSStr));
end;

class function TNSStringBridge.FromAnsi(const value: AnsiString; const encoding: CFStringEncoding): NSString;
begin
  Result := Wrap(TCFStringBridge.FromAnsi(value, encoding));
end;

class function TNSStringBridge.FromUtf8(const value: AnsiString): NSString;
begin
  Result := Wrap(TCFStringBridge.FromUtf8(value));
end;

class function TNSStringBridge.FromWide(const value: WideString): NSString;
begin
  Result := Wrap(TCFStringBridge.FromWide(value));
end;

class function TNSStringBridge.FromUnicode(const value: UnicodeString): NSString;
begin
  Result := Wrap(TCFStringBridge.FromUnicode(value));
end;


function CFStrToAnsiStr(cfStr    : CFStringRef;
                        encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): AnsiString;
begin
  Result := TCFStringBridge.ToAnsi(cfStr, encoding);
end;

procedure AnsiStrToCFStr(const aStr     : AnsiString;
                           out cfStr    : CFStringRef;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
begin
  cfStr := TCFStringBridge.FromAnsi(aStr, encoding);
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
  Result := TCFStringBridge.ToUtf8(cfStr);
end;

procedure Utf8StrToCFStr(const aStr  : AnsiString;
                           out cfStr : CFStringRef);
begin
  cfStr := TCFStringBridge.FromUtf8(aStr);
end;

function CFStrToWideStr(cfStr : CFStringRef): WideString;
begin
  Result := TCFStringBridge.ToWide(cfStr);
end;

procedure WideStrToCFStr(const aStr  : WideString;
                           out cfStr : CFStringRef);
begin
  cfStr := TCFStringBridge.FromWide(aStr);
end;

function CFStrToUniStr(cfStr : CFStringRef): UnicodeString;
begin
  Result := TCFStringBridge.ToUnicode(cfStr);
end;

procedure UniStrToCFStr(const aStr  : UnicodeString;
                          out cfStr : CFStringRef);
begin
  cfStr := TCFStringBridge.FromUnicode(aStr);
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
  Result := TNSStringBridge.ToAnsi(aNSStr, encoding);
end;

procedure AnsiStrToNSStr(const aStr     : AnsiString;
                           out aNSStr   : NSString;
                               encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1);
begin
  aNSStr := TNSStringBridge.FromAnsi(aStr, encoding);
end;

function AnsiStrToNSStr(const aStr     : AnsiString;
                              encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1) : NSString;
begin
  Result := TNSStringBridge.FromAnsi(aStr, encoding);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToStr(aNSStr   : NSString;
                    encoding : CFStringEncoding = kCFStringEncodingWindowsLatin1): string;
begin
  Result := TNSStringBridge.ToAnsi(aNSStr, encoding);
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
  Result := TNSStringBridge.FromAnsi(aStr, encoding);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToUtf8Str(aNSStr : NSString): AnsiString;
begin
  Result := TNSStringBridge.ToUtf8(aNSStr);
end;

procedure Utf8StrToNSStr(const aStr   : AnsiString;
                           out aNSStr : NSString);
begin
  aNSStr := TNSStringBridge.FromUtf8(aStr);
end;

function Utf8StrToNSStr(const aStr : AnsiString) : NSString;
begin
  Result := TNSStringBridge.FromUtf8(aStr);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToWideStr(aNSStr : NSString): WideString;
begin
  Result := TNSStringBridge.ToWide(aNSStr);
end;

procedure WideStrToNSStr(const aStr   : WideString;
                           out aNSStr : NSString);
begin
  aNSStr := TNSStringBridge.FromWide(aStr);
end;

function WideStrToNSStr(const aStr : WideString) : NSString;
begin
  Result := TNSStringBridge.FromWide(aStr);
  if Result <> nil then
    Result.autorelease;
end;

function NSStrToUniStr(aNSStr : NSString): UnicodeString;
begin
  Result := TNSStringBridge.ToUnicode(aNSStr);
end;

procedure UniStrToNSStr(const aStr   : UnicodeString;
                          out aNSStr : NSString);
begin
  aNSStr := TNSStringBridge.FromUnicode(aStr);
end;

function UniStrToNSStr(const aStr : UnicodeString) : NSString;
begin
  Result := TNSStringBridge.FromUnicode(aStr);
  if Result <> nil then
    Result.autorelease;
end;


end.
