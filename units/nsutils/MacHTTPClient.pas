unit MacHTTPClient;
(*
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke
 *)
{!
  Lightweight macOS HTTP helper that wraps Apple's NSURL loading system.
  Provides synchronous helpers for simple GET/POST style requests without
  pulling in external libraries.
}

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils,
{$IF (DEFINED(IPHONESIM) OR DEFINED(CPUARM) OR DEFINED(CPUAARCH64)) AND (NOT DEFINED(LCLCOCOA)) } // iOS targets
 {$IFDEF NoiPhoneAll}
  Foundation
 {$ELSE}
  iPhoneAll
 {$ENDIF}
{$ELSE} // macOS
 {$IFDEF NoCocoaAll}
  Foundation
 {$ELSE}
  CocoaAll
 {$ENDIF}
{$ENDIF};

function MacHttpSend(const Url, Method: string;
                     RequestBody: TStream;
                     ResponseBody: TStream;
                     Headers: TStrings;
                     out ErrorMessage: string;
                     TimeoutSec: Double = 30.0): Boolean;

function MacHttpGet(const Url: string;
                    out ResponseText: string;
                    out ErrorMessage: string;
                    Headers: TStrings = nil;
                    TimeoutSec: Double = 30.0): Boolean;

implementation

function NSStringFromString(const Value: string): NSString;
var
  Utf8Value: UTF8String;
begin
  Utf8Value := UTF8String(Value);
  Result := NSString.stringWithUTF8String(PAnsiChar(Utf8Value));
end;

function NSStringToStringSafe(const Value: NSString): string;
begin
  if Value = nil then
    Exit('');
  Result := string(UTF8Decode(UTF8String(Value.UTF8String)));
end;

function MacHttpSend(const Url, Method: string;
                     RequestBody: TStream;
                     ResponseBody: TStream;
                     Headers: TStrings;
                     out ErrorMessage: string;
                     TimeoutSec: Double): Boolean;
var
  Request: NSMutableURLRequest;
  UrlObj: NSURL;
  ResponseData: NSData;
  Response: NSURLResponse;
  Error: NSError;
  BodyBuffer: NSMutableData;
  Name, Value: string;
  I: Integer;
  TimeoutValue: NSTimeInterval;
begin
  ErrorMessage := '';
  Result := False;

  if Assigned(ResponseBody) then
  begin
    ResponseBody.Position := 0;
    if ResponseBody is TMemoryStream then
      TMemoryStream(ResponseBody).SetSize(0);
  end;

  UrlObj := NSURL.URLWithString(NSStringFromString(Url));
  if not Assigned(UrlObj) then
  begin
    ErrorMessage := 'Invalid URL';
    Exit;
  end;

  TimeoutValue := TimeoutSec;
  Request := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(
               UrlObj,
               NSURLRequestUseProtocolCachePolicy,
               TimeoutValue);

  if Method <> '' then
    Request.setHTTPMethod(NSStringFromString(Method));

  if Assigned(Headers) then
    for I := 0 to Headers.Count - 1 do
    begin
      Name := Headers.Names[I];
      if Name = '' then
        Continue;
      Value := Headers.ValueFromIndex[I];
      Request.setValue_forHTTPHeaderField(NSStringFromString(Value),
                                          NSStringFromString(Name));
    end;

  if Assigned(RequestBody) and (RequestBody.Size > 0) then
  begin
    BodyBuffer := NSMutableData.alloc.initWithLength(RequestBody.Size);
    try
      RequestBody.Position := 0;
      RequestBody.ReadBuffer(BodyBuffer.mutableBytes^, RequestBody.Size);
      Request.setHTTPBody(BodyBuffer);
    finally
      BodyBuffer.release;
    end;
  end
  else
    Request.setHTTPBody(nil);

  ResponseData := NSURLConnection.sendSynchronousRequest_returningResponse_error(
                    Request, @Response, @Error);

  if not Assigned(ResponseData) then
  begin
    if Assigned(Error) then
      ErrorMessage := NSStringToStringSafe(Error.localizedDescription)
    else
      ErrorMessage := 'No response received';
    Exit;
  end;

  if Assigned(ResponseBody) then
  begin
    if ResponseData.length > 0 then
    begin
      ResponseBody.Position := 0;
      if ResponseBody is TMemoryStream then
        TMemoryStream(ResponseBody).SetSize(ResponseData.length);
      ResponseBody.WriteBuffer(ResponseData.bytes^, ResponseData.length);
      ResponseBody.Position := 0;
    end;
  end;

  Result := True;
end;

function MacHttpGet(const Url: string;
                    out ResponseText: string;
                    out ErrorMessage: string;
                    Headers: TStrings;
                    TimeoutSec: Double): Boolean;
var
  ResponseStream: TMemoryStream;
  Size: LongInt;
begin
  ResponseText := '';
  ResponseStream := TMemoryStream.Create;
  try
    Result := MacHttpSend(Url, 'GET', nil, ResponseStream, Headers,
                          ErrorMessage, TimeoutSec);
    if Result then
    begin
      Size := ResponseStream.Size;
      SetLength(ResponseText, Size);
      if Size > 0 then
      begin
        ResponseStream.Position := 0;
        ResponseStream.ReadBuffer(ResponseText[1], Size);
      end;
    end;
  finally
    ResponseStream.Free;
  end;
end;

end.
