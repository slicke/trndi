unit MacHTTPClient;

{!
  Thin HTTP wrapper that routes macOS requests through libcurl, mirroring the
  Linux implementation so we share the same behaviour across both targets.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, Math, Types, ctypes, libpascurl;

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

function StreamWriteCallback(ptr: Pointer; size, nmemb: csize_t; userdata: Pointer): csize_t; cdecl;
var
  bytesTotal: csize_t;
  target: TStream;
begin
  bytesTotal := size * nmemb;
  if (userdata <> nil) and (bytesTotal > 0) then
  begin
    target := TStream(userdata);
    try
      target.WriteBuffer(ptr^, SizeInt(bytesTotal));
    except
      Exit(0);
    end;
  end;
  Result := bytesTotal;
end;

function BuildHeaderList(const Headers: TStrings): pcurl_slist;
var
  i, eqPos: Integer;
  line, name, value, headerLine: string;
  headerUtf8: UTF8String;
begin
  Result := nil;
  if Headers = nil then
    Exit;

  for i := 0 to Headers.Count - 1 do
  begin
    line := Headers[i];
    eqPos := Pos('=', line);
    if eqPos > 0 then
    begin
      name := Trim(Copy(line, 1, eqPos - 1));
      value := Trim(Copy(line, eqPos + 1, MaxInt));
      headerLine := Format('%s: %s', [name, value]);
    end
    else
      headerLine := line;

    if headerLine = '' then
      Continue;

    headerUtf8 := UTF8String(headerLine);
    Result := curl_slist_append(Result, PAnsiChar(headerUtf8));
  end;
end;

function MacHttpSend(const Url, Method: string;
                     RequestBody: TStream;
                     ResponseBody: TStream;
                     Headers: TStrings;
                     out ErrorMessage: string;
                     TimeoutSec: Double): Boolean;
var
  curlHandle: CURL;
  headerList: pcurl_slist;
  errCode: CURLcode;
  methodUpper: string;
  urlUtf8: UTF8String;
  customMethod: UTF8String;
  timeoutSeconds: LongInt;
  responseOwner: Boolean;
  localResponse: TMemoryStream;
  requestData: TBytes;
  requestSize: SizeInt;
  errorBuffer: array[0..CURL_ERROR_SIZE-1] of AnsiChar;
begin
  Result := False;
  ErrorMessage := '';
  headerList := nil;
  curl_global_init(CURL_GLOBAL_DEFAULT);

  responseOwner := False;
  if ResponseBody = nil then
  begin
    localResponse := TMemoryStream.Create;
    ResponseBody := localResponse;
    responseOwner := True;
  end
  else
  begin
    ResponseBody.Position := 0;
    if ResponseBody is TMemoryStream then
      TMemoryStream(ResponseBody).SetSize(0);
  end;

  curlHandle := curl_easy_init();
  if curlHandle = nil then
  begin
    ErrorMessage := 'curl: failed to initialise';
    if responseOwner then
      localResponse.Free;
    Exit;
  end;

  try
    FillChar(errorBuffer, SizeOf(errorBuffer), 0);
    curl_easy_setopt(curlHandle, CURLOPT_ERRORBUFFER, @errorBuffer[0]);

    urlUtf8 := UTF8String(Url);
    curl_easy_setopt(curlHandle, CURLOPT_URL, PAnsiChar(urlUtf8));
    curl_easy_setopt(curlHandle, CURLOPT_FOLLOWLOCATION, LongInt(1));
    curl_easy_setopt(curlHandle, CURLOPT_NOSIGNAL, LongInt(1));

    if TimeoutSec > 0 then
    begin
      timeoutSeconds := Ceil(TimeoutSec);
      curl_easy_setopt(curlHandle, CURLOPT_CONNECTTIMEOUT, timeoutSeconds);
      curl_easy_setopt(curlHandle, CURLOPT_TIMEOUT, timeoutSeconds);
    end;

    methodUpper := UpperCase(Trim(Method));
    if methodUpper = '' then
      methodUpper := 'GET';

    if methodUpper = 'GET' then
      curl_easy_setopt(curlHandle, CURLOPT_HTTPGET, LongInt(1))
    else if methodUpper = 'POST' then
      curl_easy_setopt(curlHandle, CURLOPT_POST, LongInt(1))
    else
    begin
      customMethod := UTF8String(methodUpper);
      curl_easy_setopt(curlHandle, CURLOPT_CUSTOMREQUEST, PAnsiChar(customMethod));
    end;

    headerList := BuildHeaderList(Headers);
    if headerList <> nil then
      curl_easy_setopt(curlHandle, CURLOPT_HTTPHEADER, headerList);

    if Assigned(RequestBody) and (RequestBody.Size > 0) then
    begin
      requestSize := SizeInt(RequestBody.Size);
      SetLength(requestData, requestSize);
      RequestBody.Position := 0;
      if requestSize > 0 then
        RequestBody.ReadBuffer(requestData[0], requestSize);
      curl_easy_setopt(curlHandle, CURLOPT_POSTFIELDSIZE, LongInt(requestSize));
      curl_easy_setopt(curlHandle, CURLOPT_POSTFIELDS, @requestData[0]);
    end
    else if methodUpper = 'POST' then
    begin
      curl_easy_setopt(curlHandle, CURLOPT_POSTFIELDSIZE, 0);
      curl_easy_setopt(curlHandle, CURLOPT_POSTFIELDS, nil);
    end;

    curl_easy_setopt(curlHandle, CURLOPT_WRITEFUNCTION, @StreamWriteCallback);
    curl_easy_setopt(curlHandle, CURLOPT_WRITEDATA, Pointer(ResponseBody));

    errCode := curl_easy_perform(curlHandle);
    if errCode = CURLE_OK then
    begin
      if ResponseBody <> nil then
        ResponseBody.Position := 0;
      Result := True;
    end
    else
    begin
      if errorBuffer[0] <> #0 then
        ErrorMessage := string(UTF8String(PAnsiChar(@errorBuffer[0])))
      else
        ErrorMessage := string(UTF8String(curl_easy_strerror(errCode)));
    end;
  finally
    if headerList <> nil then
      curl_slist_free_all(headerList);
    curl_easy_cleanup(curlHandle);
    if responseOwner then
      localResponse.Free;
  end;
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
