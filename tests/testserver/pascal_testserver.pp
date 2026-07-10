unit pascal_testserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StartPascalTestServer(out BaseURL: string): boolean;
procedure StopPascalTestServer;

implementation

uses
  sockets, sha1, dateutils, fpjson, jsonparser, base64;

var
  ServerThread: TThread = nil;
  ServerListenSocket: TSocket = -1;
  ServerShouldStop: Boolean = False;
  LatestValue: string = '102';
  CareLinkRefreshCount: Integer = 0;

function GenerateObjectId: string;
var
  ts: LongInt;
begin
  // AInputIsUTC=False: Now is local time, not UTC. Default True would
  // skew the timestamp by the local UTC offset.
  ts := DateTimeToUnix(Now, False);
  Result := LowerCase(IntToHex(ts, 8))
    + LowerCase(IntToHex(Random($FFFFFF), 10))
    + LowerCase(IntToHex(Random($FFFFFF), 6));
end;

function NowMillis: Int64;
begin
  Result := Int64(DateTimeToUnix(Now, False)) * 1000;
end;

function ISO8601From(ms: Int64): string;
var
  dt: TDateTime;
  millis: Int64;
begin
  // AReturnUTC=True: render the Unix timestamp as UTC wall-clock (the 'Z' suffix).
  dt := UnixToDateTime(ms div 1000, True);
  millis := ms mod 1000;
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', dt)
    + '.' + Format('%.3d', [millis]) + 'Z';
end;

// ---------- CareLink helpers -------------------------------------------------

function Base64UrlEncode(const s: string): string;
begin
  Result := EncodeStringBase64(s);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  while (Length(Result) > 0) and (Result[Length(Result)] = '=') do
    SetLength(Result, Length(Result) - 1);
end;

// Unsigned JWT good for one hour, carrying the claims the CareLink client
// reads: exp (refresh scheduling) and token_details.preferred_username.
function BuildCareLinkJWT: string;
var
  expEpoch: Int64;
begin
  // AInputIsUTC=False: Now is local time (see GenerateObjectId).
  expEpoch := DateTimeToUnix(Now, False) + 3600;
  Result := Base64UrlEncode('{"alg":"none","typ":"JWT"}') + '.' +
    Base64UrlEncode(Format(
      '{"exp":%d,"token_details":{"preferred_username":"carelinkuser"}}',
      [expEpoch])) + '.testsig';
end;

// Naive local timestamp (no zone designator), as the CareLink sgs entries
// carry pump-local time.
function CareLinkLocalStamp(minutesAgo: Integer; base: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss',
    base - (minutesAgo / 1440.0));
end;

// ---------- NS3 rotate-token state (persisted in temp file) -----------------

function NS3StateFile: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'trndi-ns3-rotate-state.json';
end;

procedure LoadNS3State(out authCount, entriesSuccessCount: Integer; out currentToken: string);
var
  fs: TFileStream;
  ss: TStringStream;
  txt: string;
  json: TJSONData;
  obj: TJSONObject;
begin
  authCount := 0;
  entriesSuccessCount := 0;
  currentToken := '';
  if not FileExists(NS3StateFile) then Exit;
  try
    ss := TStringStream.Create('');
    fs := TFileStream.Create(NS3StateFile, fmOpenRead or fmShareDenyNone);
    try
      ss.CopyFrom(fs, 0);
      txt := ss.DataString;
    finally
      fs.Free;
      ss.Free;
    end;
    if Trim(txt) = '' then Exit;
    json := GetJSON(txt);
    try
      if json is TJSONObject then
      begin
        obj := TJSONObject(json);
        authCount := obj.Get('authCount', 0);
        entriesSuccessCount := obj.Get('entriesSuccessCount', 0);
        currentToken := obj.Get('currentToken', '');
      end;
    finally
      json.Free;
    end;
  except
    // ignore - keep defaults
  end;
end;

procedure SaveNS3State(authCount, entriesSuccessCount: Integer; const currentToken: string);
var
  obj: TJSONObject;
  s: string;
  fs: TFileStream;
begin
  obj := TJSONObject.Create;
  try
    obj.Add('authCount', authCount);
    obj.Add('entriesSuccessCount', entriesSuccessCount);
    obj.Add('currentToken', currentToken);
    s := obj.AsJSON;
  finally
    obj.Free;
  end;
  try
    fs := TFileStream.Create(NS3StateFile, fmCreate);
    try
      if Length(s) > 0 then
        fs.WriteBuffer(s[1], Length(s));
    finally
      fs.Free;
    end;
  except
  end;
end;

procedure ResetNS3State;
begin
  if FileExists(NS3StateFile) then
    DeleteFile(NS3StateFile);
end;

// ---------- request handler -------------------------------------------------

procedure ServeOnce(Listener: LongInt);
var
  ClientSock: LongInt;
  Addr: TSockAddr;
  Len: Integer;
  Buffer: array[0..16383] of AnsiChar;
  rcv, i: Integer;
  Req, Line, Method, Path, Rest: string;
  Headers: TStringList;
  Body, headPart, h: string;
  pb, p, colon: Integer;
  sub, items, entries, str: string;
  ms, nowMs, readingMs: Int64;
  pathOnly, query, d, provided, hdrName, hdrVal: string;
  tokenPart, authHeader, bearerToken, expectedToken: string;
  qpos, count, cnt: Integer;
  expectedHash: string;
  authCount, entriesSuccessCount: Integer;
  currentToken: string;
  clBase: TDateTime;

  function ReadRequest: Boolean;
  var
    chunk: string;
    contentLen, k: Integer;
  begin
    Result := False;
    Req := '';

    // A request may arrive fragmented (headers and POST body in separate
    // segments); keep reading until the full header block is in.
    repeat
      rcv := fprecv(ClientSock, @Buffer, SizeOf(Buffer), 0);
      if rcv <= 0 then Break;
      SetString(chunk, PAnsiChar(@Buffer[0]), rcv);
      Req := Req + chunk;
      pb := Pos(#13#10#13#10, Req);
    until (pb > 0) or (Length(Req) > 4 * SizeOf(Buffer));
    pb := Pos(#13#10#13#10, Req);
    if pb = 0 then Exit;

    i := Pos(#13#10, Req);
    Line := Copy(Req, 1, i - 1);
    if Pos(' ', Line) = 0 then Exit;
    Method := Trim(Copy(Line, 1, Pos(' ', Line) - 1));
    Rest := Trim(Copy(Line, Pos(' ', Line) + 1, MaxInt));
    if Pos(' ', Rest) = 0 then Path := Rest
    else Path := Trim(Copy(Rest, 1, Pos(' ', Rest) - 1));

    Headers := TStringList.Create;
    Headers.NameValueSeparator := ':';
    headPart := '';
    if pb > i + 2 then headPart := Copy(Req, i + 2, pb - (i + 2));
    while Length(headPart) > 0 do
    begin
      p := Pos(#13#10, headPart);
      if p = 0 then
      begin
        h := headPart;
        headPart := '';
      end
      else
      begin
        h := Copy(headPart, 1, p - 1);
        Delete(headPart, 1, p + 1);
      end;
      colon := Pos(':', h);
      if colon > 0 then
        Headers.Add(Trim(Copy(h, 1, colon - 1)) + ':' + Trim(Copy(h, colon + 1, MaxInt)));
    end;

    Body := '';
    if (Method = 'POST') or (Method = 'PUT') then
    begin
      Body := Copy(Req, pb + 4, MaxInt);
      // Honor Content-Length: read until the body is complete.
      contentLen := 0;
      for k := 0 to Headers.Count - 1 do
        if SameText(Headers.Names[k], 'Content-Length') then
        begin
          contentLen := StrToIntDef(Headers.ValueFromIndex[k], 0);
          Break;
        end;
      while Length(Body) < contentLen do
      begin
        rcv := fprecv(ClientSock, @Buffer, SizeOf(Buffer), 0);
        if rcv <= 0 then Break;
        SetString(chunk, PAnsiChar(@Buffer[0]), rcv);
        Body := Body + chunk;
      end;
    end;
    Result := True;
  end;

  function HeaderLookup(const name: string): string;
  var
    k: Integer;
  begin
    Result := '';
    if not Assigned(Headers) then Exit;
    for k := 0 to Headers.Count - 1 do
      if SameText(Headers.Names[k], name) then
      begin
        Result := Headers.ValueFromIndex[k];
        Exit;
      end;
  end;

  procedure SendResponse(const cntType, data: string; code: Integer = 200; const extraHeaders: string = '');
  var
    RespHdr, status: string;
  begin
    case code of
      200: status := 'OK';
      302: status := 'Found';
      401: status := 'Unauthorized';
      404: status := 'Not Found';
      500: status := 'Internal Server Error';
    else
      status := 'OK';
    end;
    RespHdr := Format('HTTP/1.1 %d %s'#13#10'Content-Type: %s'#13#10'Content-Length: %d'#13#10,
      [code, status, cntType, Length(data)]);
    if extraHeaders <> '' then
      RespHdr := RespHdr + extraHeaders;
    RespHdr := RespHdr + #13#10;
    fpsend(ClientSock, PAnsiChar(RespHdr), Length(RespHdr), 0);
    if Length(data) > 0 then fpsend(ClientSock, PAnsiChar(data), Length(data), 0);
  end;

  function QueryParam(const q, key: string): string;
  var
    kp, ap: Integer;
    seg: string;
  begin
    Result := '';
    kp := Pos(key + '=', q);
    if kp = 0 then Exit;
    seg := Copy(q, kp + Length(key) + 1, MaxInt);
    ap := Pos('&', seg);
    if ap = 0 then Result := seg
    else Result := Copy(seg, 1, ap - 1);
  end;

  function CheckAPISecret: Boolean;
  begin
    provided := HeaderLookup('API-SECRET');
    expectedHash := SHA1Print(SHA1String('test22'));
    Result := (provided = expectedHash);
  end;

  function CookieValue(const cookieHdr, name: string): string;
  var
    cpos2, semi: Integer;
    seg: string;
  begin
    Result := '';
    if (cookieHdr = '') or (name = '') then Exit;
    cpos2 := Pos(name + '=', cookieHdr);
    if cpos2 = 0 then Exit;
    seg := Copy(cookieHdr, cpos2 + Length(name) + 1, MaxInt);
    semi := Pos(';', seg);
    if semi = 0 then Result := Trim(seg)
    else Result := Trim(Copy(seg, 1, semi - 1));
  end;

begin
  Len := SizeOf(Addr);
  ClientSock := fpaccept(Listener, @Addr, @Len);
  if ClientSock <= 0 then Exit;
  Headers := nil;
  try
    if not ReadRequest then Exit;

    // Split path and query, normalize duplicate slashes
    pathOnly := Path;
    query := '';
    qpos := Pos('?', Path);
    if qpos > 0 then
    begin
      pathOnly := Copy(Path, 1, qpos - 1);
      query := Copy(Path, qpos + 1, MaxInt);
    end;
    while Pos('//', pathOnly) > 0 do
      pathOnly := StringReplace(pathOnly, '//', '/', [rfReplaceAll]);

    // -- /debug
    if pathOnly = '/debug' then
    begin
      SendResponse('text/plain', 'Requested path: ' + pathOnly);
      Exit;
    end;

    // -- /error500 (and any subpath)
    if Pos('/error500', pathOnly) = 1 then
    begin
      SendResponse('text/plain', 'Internal Server Error', 500);
      Exit;
    end;

    // -- Dexcom Share
    if Pos('/ShareWebServices/Services/', pathOnly) = 1 then
    begin
      sub := Copy(pathOnly, Length('/ShareWebServices/Services/') + 1, MaxInt);
      if sub = 'General/LoginPublisherAccountByName' then
        SendResponse('application/json', '"TEST-DEXCOM-SESSION"')
      else if sub = 'General/SystemUtcTime' then
      begin
        ms := NowMillis;
        SendResponse('application/json', '"/Date(' + IntToStr(ms) + ')/"');
      end
      else if sub = 'Publisher/ReadPublisherLatestGlucoseValues' then
      begin
        nowMs := NowMillis;
        items := '[';
        for i := 0 to 2 do
        begin
          if i > 0 then items := items + ',';
          items := items + Format('{"WT":"/Date(%d)/","ST":"/Date(%d)/","Value":%d,"Trend":"Flat"}',
            [nowMs - (i * 5 * 60 * 1000), nowMs - (i * 5 * 60 * 1000), 120 + i]);
        end;
        items := items + ']';
        SendResponse('application/json', items);
      end
      else
        SendResponse('application/json', '{"error":"Unknown Dexcom endpoint","path":"' + sub + '"}', 404);
      Exit;
    end;

    // -- /pebble (xDrip simplified payload, requires API-SECRET)
    if pathOnly = '/pebble' then
    begin
      if not CheckAPISecret then
      begin
        SendResponse('text/plain', 'Authentication failed');
        Exit;
      end;
      nowMs := NowMillis;
      readingMs := nowMs - (5 * 60 * 1000);
      entries := Format(
        '{"status":[{"now":%d}],"bgs":[{"sgv":"9,6","trend":4,"direction":"Flat","datetime":%d,' +
        '"filtered":0,"unfiltered":-127,"noise":1,"bgdelta":"-0.2","battery":"","iob":"unknown"}]}',
        [nowMs, readingMs]);
      SendResponse('application/json', entries);
      Exit;
    end;

    // -- /sgv.json (xDrip entries, requires API-SECRET)
    if pathOnly = '/sgv.json' then
    begin
      if not CheckAPISecret then
      begin
        SendResponse('text/plain', 'Authentication failed');
        Exit;
      end;
      count := StrToIntDef(QueryParam(query, 'count'), 16);
      if count < 0 then count := 0;
      if count > 200 then count := 200;
      entries := '[';
      for i := 0 to count - 1 do
      begin
        if i > 0 then entries := entries + ',';
        d := GenerateObjectId;
        nowMs := NowMillis;
        entries := entries + Format(
          '{"_id":"%s","device":"xDrip-DexcomG5","date":%d,"dateString":"%s","sgv":%d,' +
          '"delta":1.009,"direction":"Flat","type":"sgv","filtered":0,"unfiltered":0,' +
          '"rssi":100,"noise":1,"sysTime":"%s","utcOffset":60,"mills":%d}',
          [d, nowMs, ISO8601From(nowMs), 60 + (i mod 140), ISO8601From(nowMs), nowMs]);
      end;
      entries := entries + ']';
      SendResponse('application/json', entries);
      Exit;
    end;

    // -- /status.json (xDrip thresholds, requires API-SECRET)
    if pathOnly = '/status.json' then
    begin
      if not CheckAPISecret then
      begin
        SendResponse('text/plain', 'Authentication failed');
        Exit;
      end;
      SendResponse('application/json',
        '{"status":"ok","bgHigh":260,"bgLow":55,"bgTargetTop":180,"bgTargetBottom":80}');
      Exit;
    end;

    // -- Cookie endpoints
    if pathOnly = '/cookie/set' then
    begin
      hdrName := QueryParam(query, 'name');
      if hdrName = '' then hdrName := 'testcookie';
      hdrVal := QueryParam(query, 'value');
      if hdrVal = '' then hdrVal := '1';
      SendResponse('text/plain', 'OK', 200,
        'Set-Cookie: ' + hdrName + '=' + hdrVal + '; Path=/; HttpOnly'#13#10);
      Exit;
    end;

    if pathOnly = '/cookie/set-redirect' then
    begin
      hdrName := QueryParam(query, 'name');
      if hdrName = '' then hdrName := 'testcookie';
      hdrVal := QueryParam(query, 'value');
      if hdrVal = '' then hdrVal := '1';
      SendResponse('text/plain', '', 302,
        'Set-Cookie: ' + hdrName + '=' + hdrVal + '; Path=/; HttpOnly'#13#10 +
        'Location: /cookie/echo?name=' + hdrName + #13#10);
      Exit;
    end;

    if pathOnly = '/cookie/echo' then
    begin
      hdrName := QueryParam(query, 'name');
      authHeader := HeaderLookup('Cookie');
      if hdrName <> '' then
      begin
        hdrVal := CookieValue(authHeader, hdrName);
        if hdrVal = '' then
          SendResponse('application/json', '{"' + hdrName + '":null}')
        else
          SendResponse('application/json', '{"' + hdrName + '":"' + hdrVal + '"}');
      end
      else
        SendResponse('application/json', '{}');
      Exit;
    end;

    // -- Nightscout v2 token request
    if Pos('/api/v2/authorization/request/', pathOnly) = 1 then
    begin
      tokenPart := Copy(pathOnly, Length('/api/v2/authorization/request/') + 1, MaxInt);
      if tokenPart = 'token=rotate' then
      begin
        LoadNS3State(authCount, entriesSuccessCount, currentToken);
        Inc(authCount);
        currentToken := 'rotate-' + IntToStr(authCount);
        SaveNS3State(authCount, entriesSuccessCount, currentToken);
        SendResponse('application/json', '{"token":"' + currentToken + '"}');
      end
      else if tokenPart = 'token=test22' then
      begin
        ResetNS3State;
        SendResponse('application/json', '{"token":"testtoken"}');
      end
      else
        SendResponse('application/json', '{"error":"Invalid token"}', 401);
      Exit;
    end;

    // -- Nightscout v3 API
    if Pos('/api/v3/', pathOnly) = 1 then
    begin
      sub := Copy(pathOnly, Length('/api/v3/') + 1, MaxInt);
      if sub = 'status' then
      begin
        nowMs := NowMillis;
        SendResponse('application/json', Format(
          '{"status":200,"result":{"version":"0.10.2-release-20171201","apiVersion":"3.0.0",' +
          '"srvDate":%d,"storage":{"type":"mongodb","version":"4.0.6"},' +
          '"apiPermissions":{"devicestatus":"crud","entries":"r","food":"crud","profile":"r","treatments":"crud"}}}',
          [nowMs]));
        Exit;
      end;

      if sub = 'entries' then
      begin
        authHeader := HeaderLookup('Authorization');
        bearerToken := '';
        if (Length(authHeader) >= 7) and (LowerCase(Copy(authHeader, 1, 7)) = 'bearer ') then
          bearerToken := Trim(Copy(authHeader, 8, MaxInt));

        if Pos('rotate-', bearerToken) = 1 then
        begin
          LoadNS3State(authCount, entriesSuccessCount, currentToken);
          expectedToken := currentToken;
          if (expectedToken = '') or (bearerToken <> expectedToken) then
          begin
            SendResponse('application/json', '{"error":"Unauthorized","status":401}', 401);
            Exit;
          end;
          Inc(entriesSuccessCount);
          // Force one mid-session expiry: after the first successful read, rotate the
          // expected token so the next call with the old bearer fails and the client
          // must hit /api/v2/authorization/request/token=rotate again.
          if entriesSuccessCount = 1 then
            currentToken := 'rotate-' + IntToStr(authCount + 1);
          SaveNS3State(authCount, entriesSuccessCount, currentToken);
        end;

        nowMs := NowMillis;
        entries := '[';
        for i := 0 to 4 do
        begin
          if i > 0 then entries := entries + ',';
          ms := nowMs - (i * 5 * 60 * 1000);
          entries := entries + Format(
            '{"type":"sgv","sgv":%d,"date":%d,"dateString":"%s","device":"test",' +
            '"direction":"Flat","delta":0,"rssi":100,"noise":1}',
            [120 + i, ms, ISO8601From(ms)]);
        end;
        entries := entries + ']';
        SendResponse('application/json', entries);
        Exit;
      end;

      SendResponse('application/json', '{"error":"Unknown NSv3 endpoint","path":"' + sub + '"}', 404);
      Exit;
    end;

    // -- Nightscout v1 status (full payload)
    if pathOnly = '/api/v1/status.json' then
    begin
      nowMs := NowMillis;
      if not CheckAPISecret then
      begin
        // Match the PHP server: 200 with body "Unauthorized" for v1 endpoints.
        SendResponse('text/plain', 'Unauthorized');
        Exit;
      end;
      str := Format(
        '{"status":"ok","name":"nightscout","version":"15.0.2",' +
        '"serverTime":"%s","serverTimeEpoch":%d,' +
        '"apiEnabled":true,"careportalEnabled":true,"boluscalcEnabled":false,' +
        '"settings":{"units":"mmol","timeFormat":24,"dayStart":7,"dayEnd":21,' +
        '"nightMode":false,"editMode":true,"showRawbg":"never","customTitle":"Nightscout",' +
        '"theme":"colors","alarmUrgentHigh":true,"alarmUrgentHighMins":[30,60,90,120],' +
        '"alarmHigh":true,"alarmHighMins":[30,60,90,120],' +
        '"alarmLow":true,"alarmLowMins":[15,30,45,60],' +
        '"alarmUrgentLow":true,"alarmUrgentLowMins":[15,30,45],' +
        '"alarmUrgentMins":[30,60,90,120],"alarmWarnMins":[30,60,90,120],' +
        '"alarmTimeagoWarn":true,"alarmTimeagoWarnMins":15,' +
        '"alarmTimeagoUrgent":true,"alarmTimeagoUrgentMins":30,' +
        '"alarmPumpBatteryLow":false,"language":"sv","scaleY":"linear",' +
        '"showPlugins":"dbsize delta direction upbat","showForecast":"ar2",' +
        '"focusHours":3,"heartbeat":60,"baseURL":"http://127.0.0.1",' +
        '"authDefaultRoles":"denied",' +
        '"thresholds":{"bgHigh":260,"bgTargetTop":180,"bgTargetBottom":80,"bgLow":55},' +
        '"insecureUseHttp":true,"secureHstsHeader":true,"secureHstsHeaderIncludeSubdomains":false,' +
        '"secureHstsHeaderPreload":false,"secureCsp":false,"deNormalizeDates":false,' +
        '"showClockDelta":false,"showClockLastTime":false,' +
        '"frameUrl1":"","frameUrl2":"","frameUrl3":"","frameUrl4":"","frameUrl5":"","frameUrl6":"","frameUrl7":"","frameUrl8":"",' +
        '"frameName1":"","frameName2":"","frameName3":"","frameName4":"","frameName5":"","frameName6":"","frameName7":"","frameName8":"",' +
        '"authFailDelay":5000,"adminNotifiesEnabled":true,"authenticationPromptOnLoad":false,' +
        '"DEFAULT_FEATURES":["bgnow","delta","direction","timeago"],' +
        '"alarmTypes":["predict"],"enable":["careportal","maker","bolus","bridge"]},' +
        '"extendedSettings":{"devicestatus":{"advanced":true,"days":1}},' +
        '"authorized":null,"runtimeState":"loaded"}',
        [ISO8601From(nowMs), nowMs]);
      SendResponse('application/json', str);
      Exit;
    end;

    // -- Nightscout v1 entries
    if pathOnly = '/api/v1/entries/sgv.json' then
    begin
      if not CheckAPISecret then
      begin
        SendResponse('text/plain', 'Unauthorized');
        Exit;
      end;
      cnt := StrToIntDef(QueryParam(query, 'count'), 16);
      if cnt < 0 then cnt := 0;
      if cnt > 200 then cnt := 200;

      entries := '[';
      for i := 0 to cnt - 1 do
      begin
        if i > 0 then entries := entries + ',';
        d := GenerateObjectId;
        nowMs := NowMillis;
        entries := entries + Format(
          '{"_id":"%s","device":"xDrip-DexcomG5","date":%d,"dateString":"%s","sgv":%d,' +
          '"delta":1.009,"direction":"Flat","type":"sgv","filtered":0,"unfiltered":0,' +
          '"rssi":100,"noise":1,"sysTime":"%s","utcOffset":60,"mills":%d}',
          [d, nowMs, ISO8601From(nowMs), 60 + (i mod 140), ISO8601From(nowMs), nowMs]);
      end;
      entries := entries + ']';
      SendResponse('application/json', entries);
      Exit;
    end;

    // -- CareLink (Medtronic follower): OAuth2 token refresh.
    // Rotates the refresh token on every use, like the real endpoint.
    if pathOnly = '/carelink/token' then
    begin
      if (Pos('grant_type=refresh_token', Body) = 0) or
         (Pos('refresh_token=', Body) = 0) or
         (Pos('client_id=', Body) = 0) then
      begin
        SendResponse('application/json', '{"error":"invalid_request"}', 401);
        Exit;
      end;
      Inc(CareLinkRefreshCount);
      SendResponse('application/json', Format(
        '{"access_token":"%s","refresh_token":"rotated-refresh-%d",' +
        '"token_type":"Bearer","expires_in":3600}',
        [BuildCareLinkJWT, CareLinkRefreshCount]));
      Exit;
    end;

    // -- CareLink display/message: sgs payload with a warm-up gap (sg=0),
    // out-of-range clamp candidates and the device's own trend arrow.
    if pathOnly = '/carelink/display/message' then
    begin
      authHeader := HeaderLookup('Authorization');
      if Pos('Bearer ', authHeader) <> 1 then
      begin
        SendResponse('application/json', '{"error":"unauthorized"}', 401);
        Exit;
      end;
      clBase := Now;
      entries := Format(
        '{"lastSGTrend":"UP",' +
        '"activeInsulin":{"amount":2.5,"datetime":"%s"},' +
        '"sgs":[' +
        '{"sg":0,"timestamp":"%s"},' +    // gap slot: must be skipped
        '{"sg":30,"timestamp":"%s"},' +   // below 40: clamped to 39
        '{"sg":450,"timestamp":"%s"},' +  // above 400: clamped to 401
        '{"sg":100,"timestamp":"%s"},' +
        '{"sg":110,"timestamp":"%s"},' +
        '{"sg":120,"timestamp":"%s"}]}',
        [CareLinkLocalStamp(0, clBase),
         CareLinkLocalStamp(25, clBase), CareLinkLocalStamp(20, clBase),
         CareLinkLocalStamp(15, clBase), CareLinkLocalStamp(10, clBase),
         CareLinkLocalStamp(5, clBase), CareLinkLocalStamp(0, clBase)]);
      SendResponse('application/json', entries);
      Exit;
    end;

    // -- Pascal-only test helpers
    if pathOnly = '/reset' then
    begin
      LatestValue := '102';
      SendResponse('text/plain', 'ok');
      Exit;
    end;

    if pathOnly = '/latest' then
    begin
      SendResponse('text/plain', LatestValue);
      Exit;
    end;

    SendResponse('text/plain', 'not found', 404);
  finally
    if Assigned(Headers) then Headers.Free;
    CloseSocket(ClientSock);
  end;
end;

procedure ServerRunner;
var
  Listener: LongInt;
  Addr: TInetSockAddr;
begin
  Listener := fpsocket(AF_INET, SOCK_STREAM, 0);
  if Listener <= 0 then Exit;
  ServerListenSocket := Listener;
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(8080);
  // Bind only to loopback (127.0.0.1). Prevents accidental external exposure
  // and avoids any Windows Firewall ambiguity for non-loopback listeners.
  Addr.sin_addr.s_addr := htonl($7F000001);
  if fpbind(Listener, @Addr, SizeOf(Addr)) <> 0 then
  begin
    CloseSocket(Listener);
    ServerListenSocket := -1;
    Exit;
  end;
  if fplisten(Listener, 5) <> 0 then
  begin
    CloseSocket(Listener);
    ServerListenSocket := -1;
    Exit;
  end;
  ServerShouldStop := False;
  while not ServerShouldStop do
  begin
    ServeOnce(Listener);
    Sleep(1);
  end;
  CloseSocket(Listener);
  ServerListenSocket := -1;
end;

function StartPascalTestServer(out BaseURL: string): boolean;
begin
  Result := False;
  BaseURL := 'http://127.0.0.1:8080';
  if ServerThread <> nil then
  begin
    Result := True;
    Exit;
  end;

  ServerThread := TThread.CreateAnonymousThread(@ServerRunner);
  ServerThread.FreeOnTerminate := False;
  ServerThread.Start;
  Result := True;
end;

procedure StopPascalTestServer;
begin
  if ServerThread = nil then Exit;
  ServerShouldStop := True;
  if ServerListenSocket <> -1 then
    CloseSocket(ServerListenSocket);
  while (ServerListenSocket <> -1) do
    Sleep(10);
  ServerThread.Free;
  ServerThread := nil;
end;

end.
