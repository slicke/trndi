unit nsuntils.web.nsurlrequest;

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
  (* Extended SendAndReceive which also returns response headers, HTTP status and final URL. *)
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

function TNSHTTPSendAndReceive.SendAndReceive(ARequest  : TStream;
AResponse : TStream;
Headers   : TStringList) : boolean;
 {Send HTTP request to current Address URL, returning downloaded data 
   in AResponse stream and True as function result. If error occurs, 
   return False and set LastErrMsg.
  Optional ARequest stream can be used to set the HTTP request body.
  Optional Headers list of name-value pairs can be used to set 
   HTTP headers.}
var
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  HdrNum      : integer;
  urlResponse : NSURLResponse;
  error       : NSError;
  urlData     : NSData;
begin
  Result := false;
  try
    urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(
      NSURL.URLWithString(StrToNSStr(Address)),
      NSURLRequestUseProtocolCachePolicy, TimeOut);

    if Method <> '' then
      urlRequest.setHTTPMethod(StrToNSStr(Method));

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
        urlRequest.addValue_forHTTPHeaderField(StrToNSStr(Headers.ValueFromIndex[HdrNum]),
          StrToNSStr(Headers.Names[HdrNum]));

    urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(
      urlRequest, @urlResponse, @error);
    if not Assigned(urlData) then
    begin
      FLastErrMsg := NSStrToStr(error.localizedDescription);
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
end;


function TNSHTTPSendAndReceive.SendAndReceiveEx(ARequest  : TStream;
  AResponse : TStream; RequestHeaders : TStringList; ResponseHeaders: TStringList;
  out StatusCode : Integer; out FinalURL : string): boolean;
 {Extended send that also returns response headers, HTTP status and final URL}
var
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  HdrNum      : integer;
  urlResponse : NSURLResponse;
  error       : NSError;
  urlData     : NSData;
  httpResp    : id; // NSHTTPURLResponse
  hdrKeys     : NSArray;
  i: Integer;
  cnt: NativeInt;
  keyObj, valObj: id;
  tmp, tmp2, hdrDict: id;
  // Cookie extraction helpers
  cookieStorage: id;
  cookies: NSArray;
  cookieObj: id;
begin
  Result := false;
  StatusCode := 0;
  FinalURL := '';
  try
    urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(
      NSURL.URLWithString(StrToNSStr(Address)),
      NSURLRequestUseProtocolCachePolicy, TimeOut);

    if Method <> '' then
      urlRequest.setHTTPMethod(StrToNSStr(Method));

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
        urlRequest.addValue_forHTTPHeaderField(StrToNSStr(RequestHeaders.ValueFromIndex[HdrNum]),
          StrToNSStr(RequestHeaders.Names[HdrNum]));

    urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(
      urlRequest, @urlResponse, @error);
    if not Assigned(urlData) then
    begin
      FLastErrMsg := NSStrToStr(error.localizedDescription);
      Exit;
    end;

    AResponse.Position := 0;
    AResponse.WriteBuffer(urlData.bytes^, urlData.length);
    AResponse.Position := 0;

    // Try to extract status, headers and final URL dynamically via KVC
    try
      // statusCode (NSNumber)
      try
        tmp := urlResponse.valueForKey(StrToNSStr('statusCode'));
        if assigned(tmp) then
          StatusCode := Integer(tmp.intValue)
        else
          StatusCode := 0;
      except
        StatusCode := 0;
      end;
      // Final URL
      try
        tmp2 := urlResponse.valueForKey(StrToNSStr('URL'));
        if assigned(tmp2) then
        begin
          try
            FinalURL := NSStrToStr(NSString(tmp2.absoluteString));
          except
            FinalURL := '';
          end;
        end;
      except
        FinalURL := '';
      end;

      // Headers dictionary
      if Assigned(ResponseHeaders) then
      try
        hdrDict := urlResponse.valueForKey(StrToNSStr('allHeaderFields'));
        if assigned(hdrDict) then
        begin
          hdrKeys := hdrDict.allKeys;
          cnt := NativeInt(hdrKeys.count);
          if cnt > 0 then
          begin
            for i := 0 to cnt - 1 do
            begin
              keyObj := hdrKeys.objectAtIndex(i);
              valObj := hdrDict.objectForKey(keyObj);
              ResponseHeaders.Add(NSStrToStr(NSString(keyObj)) + ': ' + NSStrToStr(NSString(valObj)));
            end;
          end;
        end;

        // Also include cookies from the shared cookie storage as Set-Cookie: lines
        try
          cookieStorage := NSHTTPCookieStorage.sharedHTTPCookieStorage;
          cookies := cookieStorage.cookiesForURL(NSURL.URLWithString(StrToNSStr(Address)));
          if assigned(cookies) then
          begin
            cnt := NativeInt(cookies.count);
            if cnt > 0 then
            begin
              for i := 0 to cnt - 1 do
              begin
                cookieObj := cookies.objectAtIndex(i);
                try
                  ResponseHeaders.Add('Set-Cookie: ' + NSStrToStr(NSString(cookieObj.name)) + '=' + NSStrToStr(NSString(cookieObj.value)));
                except
                  // ignore cookie value extraction failures
                end;
              end;
            end;
          end;
        except
          // ignore cookie extraction failures
        end;
      except
        // ignore header extraction failures
      end;
    except
      // Fallback: attempt to return URL from response
      try
        FinalURL := NSStrToStr(NSURL(urlResponse.URL).absoluteString);
      except
        FinalURL := '';
      end;
    end;

    Result := true;
  except
    on E : Exception do
    begin
      FLastErrMsg := E.Message;
    end;
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
