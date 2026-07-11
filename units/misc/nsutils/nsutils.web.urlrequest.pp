unit nsutils.web.urlrequest;

{
  TNSHTTPSendAndReceive class for use by itself as an HTTP client or with 
   Web Service Toolkit (http://wiki.freepascal.org/Web_Service_Toolkit).

  Author:    Phil Hess.
  Copyright: Copyright 2011 Phil Hess.
  License:   Modified LGPL (see Free Pascal's rtl/COPYING.FPC). 
             This means you can link your code to this compiled unit (statically 
             in a standalone executable or dynamically in a library) without 
             releasing your code. Only changes to this unit need to be made 
             publicly available.
}

{$modeswitch ObjectiveC1}

interface

uses 
SysUtils,
Classes,
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
nsutils.nshelpers;

type
TNSHTTPSendAndReceive = class(TObject)
private
  FAddress : string;
  FMethod : string;
  FTimeOut : integer;
  FLastErrMsg : string;
public
  property Address : string read FAddress write FAddress;
  property Method : string read FMethod write FMethod;
  property TimeOut : integer read FTimeOut write FTimeOut;
  property LastErrMsg : string read FLastErrMsg;
  constructor Create;
  function SendAndReceive(ARequest  : TStream;
    AResponse : TStream;
    Headers   : TStringList) : boolean; overload;
  (* Extended SendAndReceive which also returns response headers, HTTP status and final URL.
     Unlike SendAndReceive, this variant does NOT follow redirects: a 3xx
     response is returned to the caller (with its Location header) so the
     caller can implement its own redirect/cookie policy. Cookies are not
     attached automatically either; pass an explicit Cookie request header. *)
  function SendAndReceiveEx(ARequest  : TStream;
    AResponse : TStream; RequestHeaders : TStringList; ResponseHeaders: TStringList;
    out StatusCode : Integer; out FinalURL : string): boolean; overload;
  function SendAndReceive(out AResponse : string) : boolean; overload;
  (* Convenience overload to get response headers/status/final URL. *)
  function SendAndReceiveEx(out AResponse : string; out ResponseHeaders: TStringList;
    out StatusCode: Integer; out FinalURL: string): boolean; overload;
  function PostForm(const FormFields : string;
    out AResponse  : string) : boolean; overload;
end;


implementation

constructor TNSHTTPSendAndReceive.Create;
begin
  inherited Create;
  FMethod := 'GET';
  FTimeOut := 30;
end;

{------------------------------------------------------------------------------
  NSURLSession plumbing for SendAndReceiveEx
  -------------------------------------------
  NSURLConnection.sendSynchronousRequest follows redirects internally, which
  made SendAndReceiveEx blind to 3xx responses (the manual redirect loop in
  trndi.native.mac.requestEx never saw a Location header) and made
  followRedirects=false impossible to honor. NSURLSession exposes redirect
  policy through its delegate, so SendAndReceiveEx now runs a data task with
  a delegate that refuses every redirect, handing the 3xx back to the caller.

  The NSURLSession* classes are reached via objc_msgSend rather than typed
  bindings so the unit still compiles against older CocoaAll translations;
  the delegate itself only needs NSObject.
 ------------------------------------------------------------------------------}
const
  ObjCRuntimeLib = '/usr/lib/libobjc.A.dylib';

type
  ObjCId  = Pointer;
  ObjCSel = Pointer;

function objc_getClass(name: MarshaledAString): ObjCId; cdecl;
  external ObjCRuntimeLib;
function sel_registerName(name: MarshaledAString): ObjCSel; cdecl;
  external ObjCRuntimeLib;
function ns_msgSend0(obj: ObjCId; sel: ObjCSel): ObjCId; cdecl;
  external ObjCRuntimeLib name 'objc_msgSend';
function ns_msgSend1(obj: ObjCId; sel: ObjCSel; p1: ObjCId): ObjCId; cdecl;
  external ObjCRuntimeLib name 'objc_msgSend';
function ns_msgSend3(obj: ObjCId; sel: ObjCSel; p1, p2, p3: ObjCId): ObjCId; cdecl;
  external ObjCRuntimeLib name 'objc_msgSend';
procedure ns_msgSendBool(obj: ObjCId; sel: ObjCSel; p1: Boolean); cdecl;
  external ObjCRuntimeLib name 'objc_msgSend';

type
  { ABI layout of an Objective-C block literal; invoke is the entry point and
    receives the block itself as its first argument. }
  PNSBlockLiteral = ^TNSBlockLiteral;
  TNSBlockLiteral = record
    isa: Pointer;
    flags: LongInt;
    reserved: LongInt;
    invoke: Pointer;
  end;
  TNSBlockInvokeId = procedure(block: Pointer; arg: ObjCId); cdecl;

procedure CallBlockWithId(block: Pointer; arg: ObjCId);
begin
  if block <> nil then
    TNSBlockInvokeId(PNSBlockLiteral(block)^.invoke)(block, arg);
end;

type
  { Delegate for the synchronous data task: accumulates the body, refuses
    redirects, and signals completion. NSURLSession retains its delegate
    until the session is invalidated, and delegate callbacks run on the
    session's own serial queue while the calling thread waits on DoneEvent. }
  TTrndiURLSessionDelegate = objcclass(NSObject)
  public
    Body: NSMutableData;   // owned by the Pascal side (+1)
    FailError: NSError;    // retained when the task fails
    Completed: Boolean;
    DoneEvent: PRTLEvent;
    procedure URLSession_dataTask_didReceiveData(
      {%H-}session: ObjCId; {%H-}dataTask: ObjCId; data: NSData);
      message 'URLSession:dataTask:didReceiveData:';
    procedure URLSession_task_didCompleteWithError(
      {%H-}session: ObjCId; {%H-}task: ObjCId; error: NSError);
      message 'URLSession:task:didCompleteWithError:';
    procedure URLSession_task_willPerformHTTPRedirection_newRequest_completionHandler(
      {%H-}session: ObjCId; {%H-}task: ObjCId; {%H-}response: NSHTTPURLResponse;
      {%H-}request: NSURLRequest; completionHandler: Pointer);
      message 'URLSession:task:willPerformHTTPRedirection:newRequest:completionHandler:';
  end;

procedure TTrndiURLSessionDelegate.URLSession_dataTask_didReceiveData(
  session: ObjCId; dataTask: ObjCId; data: NSData);
begin
  if Body <> nil then
    Body.appendData(data);
end;

procedure TTrndiURLSessionDelegate.URLSession_task_didCompleteWithError(
  session: ObjCId; task: ObjCId; error: NSError);
begin
  if error <> nil then
  begin
    error.retain;
    FailError := error;
  end;
  Completed := true;
  if DoneEvent <> nil then
    RTLEventSetEvent(DoneEvent);
end;

procedure TTrndiURLSessionDelegate.URLSession_task_willPerformHTTPRedirection_newRequest_completionHandler(
  session: ObjCId; task: ObjCId; response: NSHTTPURLResponse;
  request: NSURLRequest; completionHandler: Pointer);
begin
  // Never auto-follow: complete the task with the 3xx response so the
  // caller decides what to do with the Location header.
  CallBlockWithId(completionHandler, nil);
end;

function TNSHTTPSendAndReceive.SendAndReceive(ARequest  : TStream;
AResponse : TStream;
Headers   : TStringList) : boolean;
 {Send HTTP request to current Address URL, returning downloaded data
   in AResponse stream and True as function result. If error occurs,
   return False and set LastErrMsg.
  Optional ARequest stream can be used to set the HTTP request body.
  Optional Headers list of name-value pairs can be used to set
   HTTP headers.
  Strings cross the ObjC boundary as UTF-8 (StrToNSStr's CP1252 default
   mangled non-ASCII URLs, header values and localized error messages).}
var
  pool        : NSAutoreleasePool;
  addrNS      : NSString;
  urlObj      : NSURL;
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  HdrNum      : integer;
  urlResponse : NSURLResponse;
  error       : NSError;
  urlData     : NSData;
begin
  Result := false;
  // Own pool: this runs on API worker threads that provide none.
  pool := NSAutoreleasePool.alloc.init;
  try
    try
      addrNS := Utf8StrToNSStr(Address);
      if addrNS = nil then
      begin
        FLastErrMsg := 'Invalid URL encoding: ' + Address;
        Exit;
      end;
      urlObj := NSURL.URLWithString(addrNS);
      if urlObj = nil then
      begin
        FLastErrMsg := 'Invalid URL: ' + Address;
        Exit;
      end;

      urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(
        urlObj, NSURLRequestUseProtocolCachePolicy, TimeOut);

      if Method <> '' then
        urlRequest.setHTTPMethod(Utf8StrToNSStr(Method));

      if Assigned(ARequest) and (ARequest.Size > 0) then
      try
        requestData := NSMutableData.alloc.initWithLength(ARequest.Size);
        ARequest.Position := 0;
        ARequest.ReadBuffer(requestData.mutableBytes^, ARequest.Size);
        urlRequest.setHTTPBody(requestData);
      finally
        requestData.release;
      end;

      if Assigned(Headers) then
        for HdrNum := 0 to Headers.Count-1 do
          urlRequest.addValue_forHTTPHeaderField(
            Utf8StrToNSStr(Headers.ValueFromIndex[HdrNum]),
            Utf8StrToNSStr(Headers.Names[HdrNum]));

      // Pre-clear both out-parameters: on failure the API is not guaranteed
      // to write them, and reading a garbage NSError pointer crashes.
      urlResponse := nil;
      error := nil;
      urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(
        urlRequest, @urlResponse, @error);
      if not Assigned(urlData) then
      begin
        if error <> nil then
          FLastErrMsg := NSStrToUtf8Str(error.localizedDescription)
        else
          FLastErrMsg := 'Request failed (no error details available)';
        Exit;
      end;

      AResponse.Position := 0;
      AResponse.WriteBuffer(urlData.bytes^, urlData.length);
      AResponse.Position := 0;
      Result := true;

    except
      on E : Exception do
      begin
        FLastErrMsg := E.Message;
      end;
    end;
  finally
    pool.release;
  end;
end;


function TNSHTTPSendAndReceive.SendAndReceiveEx(ARequest  : TStream;
  AResponse : TStream; RequestHeaders : TStringList; ResponseHeaders: TStringList;
  out StatusCode : Integer; out FinalURL : string): boolean;
 {Extended send that also returns response headers, HTTP status and final URL.
  Runs an NSURLSession data task and blocks until the delegate signals
  completion. Redirects are NOT followed (the 3xx response is returned) and
  cookies are neither stored nor attached automatically — the caller owns
  both policies. Strings cross the ObjC boundary as UTF-8.}
var
  pool        : NSAutoreleasePool;
  addrNS      : NSString;
  urlObj      : NSURL;
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  HdrNum      : integer;
  configClass, sessionClass : ObjCId;
  config, session, task, respObj : ObjCId;
  delegate    : TTrndiURLSessionDelegate;
  httpResp    : NSHTTPURLResponse;
  hdrDict     : NSDictionary;
  hdrKeys, cookieArr : NSArray;
  keyNS       : NSString;
  cookieObj   : NSHTTPCookie;
  i           : NativeInt;
  keyStr      : string;
begin
  Result := false;
  StatusCode := 0;
  FinalURL := '';
  FLastErrMsg := '';
  delegate := nil;
  session := nil;
  // Own pool: this runs on API worker threads that provide none.
  pool := NSAutoreleasePool.alloc.init;
  try
    try
      addrNS := Utf8StrToNSStr(Address);
      if addrNS = nil then
      begin
        FLastErrMsg := 'Invalid URL encoding: ' + Address;
        Exit;
      end;
      urlObj := NSURL.URLWithString(addrNS);
      if urlObj = nil then
      begin
        FLastErrMsg := 'Invalid URL: ' + Address;
        Exit;
      end;

      urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(
        urlObj, NSURLRequestUseProtocolCachePolicy, TimeOut);

      if Method <> '' then
        urlRequest.setHTTPMethod(Utf8StrToNSStr(Method));

      if Assigned(ARequest) and (ARequest.Size > 0) then
      try
        requestData := NSMutableData.alloc.initWithLength(ARequest.Size);
        ARequest.Position := 0;
        ARequest.ReadBuffer(requestData.mutableBytes^, ARequest.Size);
        urlRequest.setHTTPBody(requestData);
      finally
        requestData.release;
      end;

      if Assigned(RequestHeaders) then
        for HdrNum := 0 to RequestHeaders.Count-1 do
          urlRequest.addValue_forHTTPHeaderField(
            Utf8StrToNSStr(RequestHeaders.ValueFromIndex[HdrNum]),
            Utf8StrToNSStr(RequestHeaders.Names[HdrNum]));

      configClass := objc_getClass('NSURLSessionConfiguration');
      sessionClass := objc_getClass('NSURLSession');
      if (configClass = nil) or (sessionClass = nil) then
      begin
        FLastErrMsg := 'NSURLSession is not available';
        Exit;
      end;

      // Ephemeral: nothing persists across runs. Cookie injection is turned
      // off so only the caller's explicit Cookie header is sent (matching
      // the libcurl behavior on Linux, where the jar is replayed manually).
      config := ns_msgSend0(configClass,
        sel_registerName('ephemeralSessionConfiguration'));
      if config = nil then
      begin
        FLastErrMsg := 'Could not create NSURLSession configuration';
        Exit;
      end;
      ns_msgSendBool(config, sel_registerName('setHTTPShouldSetCookies:'), false);

      delegate := TTrndiURLSessionDelegate.alloc.init;
      delegate.Body := NSMutableData.alloc.init;
      delegate.DoneEvent := RTLEventCreate;

      session := ns_msgSend3(sessionClass,
        sel_registerName('sessionWithConfiguration:delegate:delegateQueue:'),
        config, delegate, nil);
      if session = nil then
      begin
        FLastErrMsg := 'Could not create NSURLSession';
        Exit;
      end;

      task := ns_msgSend1(session, sel_registerName('dataTaskWithRequest:'),
        urlRequest);
      if task = nil then
      begin
        FLastErrMsg := 'Could not create NSURLSession data task';
        Exit;
      end;
      ns_msgSend0(task, sel_registerName('resume'));

      // The request-level timeout makes the task fail on its own; the extra
      // margin here is only a backstop against a wedged session.
      RTLEventWaitFor(delegate.DoneEvent, (TimeOut + 30) * 1000);
      if not delegate.Completed then
      begin
        ns_msgSend0(task, sel_registerName('cancel'));
        RTLEventWaitFor(delegate.DoneEvent, 5000);
      end;
      if not delegate.Completed then
      begin
        // Late delegate callbacks could still arrive; leak the delegate and
        // its event rather than risk a use-after-free.
        ns_msgSend0(session, sel_registerName('invalidateAndCancel'));
        session := nil;
        delegate := nil;
        FLastErrMsg := 'Request timed out';
        Exit;
      end;

      if delegate.FailError <> nil then
      begin
        FLastErrMsg := NSStrToUtf8Str(delegate.FailError.localizedDescription);
        Exit;
      end;

      respObj := ns_msgSend0(task, sel_registerName('response'));
      if (respObj <> nil) and
         NSObject(respObj).isKindOfClass(objc_getClass('NSHTTPURLResponse')) then
      begin
        httpResp := NSHTTPURLResponse(respObj);
        StatusCode := Integer(httpResp.statusCode);
        if httpResp.URL <> nil then
          FinalURL := NSStrToUtf8Str(httpResp.URL.absoluteString);

        if Assigned(ResponseHeaders) then
        begin
          hdrDict := httpResp.allHeaderFields;
          if hdrDict <> nil then
          begin
            hdrKeys := hdrDict.allKeys;
            for i := 0 to NativeInt(hdrKeys.count) - 1 do
            begin
              keyNS := NSString(hdrKeys.objectAtIndex(i));
              keyStr := NSStrToUtf8Str(keyNS);
              // allHeaderFields comma-coalesces repeated Set-Cookie headers;
              // skip the raw line and re-emit one line per cookie below.
              if LowerCase(keyStr) = 'set-cookie' then
                Continue;
              ResponseHeaders.Add(keyStr + ': ' +
                NSStrToUtf8Str(NSString(hdrDict.objectForKey(keyNS))));
            end;

            cookieArr := NSHTTPCookie.cookiesWithResponseHeaderFields_forURL(
              hdrDict, httpResp.URL);
            if cookieArr <> nil then
              for i := 0 to NativeInt(cookieArr.count) - 1 do
              begin
                cookieObj := NSHTTPCookie(cookieArr.objectAtIndex(i));
                ResponseHeaders.Add('Set-Cookie: ' +
                  NSStrToUtf8Str(cookieObj.name) + '=' +
                  NSStrToUtf8Str(cookieObj.value));
              end;
          end;
        end;
      end;

      AResponse.Position := 0;
      if delegate.Body.length > 0 then
        AResponse.WriteBuffer(delegate.Body.bytes^, delegate.Body.length);
      AResponse.Position := 0;

      Result := true;
    except
      on E : Exception do
        FLastErrMsg := E.Message;
    end;
  finally
    if session <> nil then
      ns_msgSend0(session, sel_registerName('finishTasksAndInvalidate'));
    if delegate <> nil then
    begin
      if delegate.DoneEvent <> nil then
      begin
        RTLEventDestroy(delegate.DoneEvent);
        delegate.DoneEvent := nil;
      end;
      if delegate.Body <> nil then
      begin
        delegate.Body.release;
        delegate.Body := nil;
      end;
      if delegate.FailError <> nil then
      begin
        delegate.FailError.release;
        delegate.FailError := nil;
      end;
      delegate.release;
    end;
    pool.release;
  end;
end;

function TNSHTTPSendAndReceive.SendAndReceiveEx(out AResponse : string; out ResponseHeaders: TStringList;
  out StatusCode: Integer; out FinalURL: string): boolean;
var
  Data : TMemoryStream;
begin
  Data := TMemoryStream.Create;
  ResponseHeaders := TStringList.Create;
  try
    Result := SendAndReceiveEx(nil, Data, nil, ResponseHeaders, StatusCode, FinalURL);
    if Result then
    begin
      SetLength(AResponse, Data.Size);
      if Data.Size > 0 then
        Data.Read(AResponse[1], Data.Size);
    end;
  finally
    Data.Free;
  end;
end;

function TNSHTTPSendAndReceive.SendAndReceive(out AResponse : string) : boolean;
var
  Data : TMemoryStream;
  tmpHeaders: TStringList;
  status: Integer;
  finalUrl: string;
begin
  Data := TMemoryStream.Create;
  tmpHeaders := TStringList.Create;
  try
    Result := SendAndReceiveEx(nil, Data, nil, tmpHeaders, status, finalUrl);
    if Result then
    begin
      SetLength(AResponse, Data.Size);
      if Data.Size > 0 then
        Data.Read(AResponse[1], Data.Size);
    end;
  finally
    Data.Free;
    tmpHeaders.Free;
  end;
end;

function TNSHTTPSendAndReceive.PostForm(const FormFields : string;
out AResponse  : string) : boolean;
var
  Request : TMemoryStream;
  Headers : TStringList;
  Data    : TMemoryStream;
  tmpRespHeaders: TStringList;
  status: Integer;
  finalUrl: string;
begin
  Request := TMemoryStream.Create;
  Headers := TStringList.Create;
  Data := TMemoryStream.Create;
  tmpRespHeaders := TStringList.Create;
  try
    FMethod := 'POST';
    if FormFields <> '' then
      Request.Write(FormFields[1], Length(FormFields));
    Headers.Add('Content-Type=application/x-www-form-urlencoded');
    Headers.Add('Content-Length=' + IntToStr(Request.Size));
    Result := SendAndReceiveEx(Request, Data, Headers, tmpRespHeaders, status, finalUrl);
    if Result then
    begin
      SetLength(AResponse, Data.Size);
      if Data.Size > 0 then
        Data.Read(AResponse[1], Data.Size);
    end;
  finally
    Request.Free;
    Headers.Free;
    Data.Free;
    tmpRespHeaders.Free;
  end;
end;


end.
