unit pascal_testserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StartPascalTestServer(out BaseURL: string): boolean;
procedure StopPascalTestServer;

implementation

uses
  sockets, sha1, dateutils, fpjson;

var
  ServerThread: TThread = nil;
  ServerListenSocket: TSocket = -1;
  ServerShouldStop: Boolean = False;
  LatestValue: string = '102';

function generateObjectId: string;
var
  ts: LongInt;
begin
  ts := DateTimeToUnix(Now);
  Result := LowerCase(IntToHex(ts, 8)) + LowerCase(IntToHex(Random($FFFFFF), 10)) + LowerCase(IntToHex(Random($FFFFFF), 6));
end;

procedure ServeOnce(Listener: LongInt);
var
  ClientSock: LongInt;
  Addr: TSockAddr;
  Len: Integer;
  Buffer: array[0..8191] of AnsiChar;
  rcv, i: Integer;
  Req, Line, Method, Path, Rest: string;
  Headers: TStringList;
  Body: string;
  pb: Integer;
  sub, items, entries: string;
  ms, nowMs: Int64;
  pathOnly, query, d, dt, cstr, cstr2, provided: string;
  qpos, cpos, amp, amp2, count, cnt: Integer;
  okAuth: Boolean;
  // Simple handlers
  function ReadRequest: Boolean;
  var
    j, p, colon: Integer;
    headPart, h: string;
  begin
    Result := False;
    rcv := fprecv(ClientSock, @Buffer, SizeOf(Buffer), 0);
    if rcv <= 0 then Exit;
    Req := string(copy(Buffer, 1, rcv));
    // get first line
    i := Pos(#13#10, Req);
    if i = 0 then Exit;
    Line := Copy(Req, 1, i-1);
    if Pos(' ', Line) = 0 then Exit;
    Method := Trim(Copy(Line, 1, Pos(' ', Line)-1));
    Rest := Trim(Copy(Line, Pos(' ', Line)+1, MaxInt));
    if Pos(' ', Rest) = 0 then Path := Rest
    else Path := Trim(Copy(Rest, 1, Pos(' ', Rest)-1));

    // For simplicity, parse headers naively
    Headers := TStringList.Create;
    headPart := '';
    pb := Pos(#13#10#13#10, Req);
    if pb > 0 then headPart := Copy(Req, i+2, pb - (i+2) + 1);
    j := 1;
    while j <= Length(headPart) do
    begin
      p := Pos(#13#10, headPart);
      if p = 0 then Break;
      h := Copy(headPart, 1, p-1);
      colon := Pos(':', h);
      if colon > 0 then
        Headers.Values[Trim(Copy(h,1,colon-1))] := Trim(Copy(h,colon+1,MaxInt));
      Delete(headPart, 1, p+1);
      Inc(j);
    end;
    // detect body
    Body := '';
    if Method = 'POST' then
    begin
      if pb > 0 then Body := Copy(Req, pb+4, MaxInt);
    end;
    Result := True;
  end;

  procedure SendResponse(cntType, data: string; code: Integer = 200);
  var
    RespHdr: string;
  begin
    RespHdr := Format('HTTP/1.1 %d OK'#13#10'Content-Type: %s'#13#10'Content-Length: %d'#13#10#13#10, [code, cntType, Length(data)]);
    fpsend(ClientSock, PAnsiChar(RespHdr), Length(RespHdr), 0);
    if Length(data) > 0 then fpsend(ClientSock, PAnsiChar(data), Length(data), 0);
  end;

begin
  Len := SizeOf(Addr);
  ClientSock := fpaccept(Listener, @Addr, @Len);
  if ClientSock <= 0 then Exit;
  Headers := nil;
  try
    if not ReadRequest then Exit;
    // very basic routing based on Path
    if Path = '/debug' then
    begin
      SendResponse('text/plain', 'ok');
      Exit;
    end;
    else if Pos('/ShareWebServices/Services/', Path) = 1 then
    begin
      // Extract the subpath (path may include query string but Dexcom tests don't use it)
      sub := Copy(Path, Length('/ShareWebServices/Services/')+1, MaxInt);
      if sub = 'General/LoginPublisherAccountByName' then
        SendResponse('application/json', '"TEST-DEXCOM-SESSION"')
      else if sub = 'General/SystemUtcTime' then
      begin
        ms := Int64(DateTimeToUnix(Now));
        // Return a JSON string similar to real Share: "/Date(1234567890)/"
        SendResponse('application/json', '"/Date(' + IntToStr(ms * 1000) + ')/"');
      end
      else if sub = 'Publisher/ReadPublisherLatestGlucoseValues' then
      begin
        // Emit a small array of readings similar to the PHP test server.
        nowMs := Int64(DateTimeToUnix(Now));
        items := '[';
        for i := 0 to 2 do
        begin
          if i > 0 then items := items + ',';
          items := items + Format('{"WT":"/Date%d)/","ST":"/Date%d)/","Value":%d,"Trend":"Flat"}',
            [nowMs * 1000 - (i * 5 * 60 * 1000), nowMs * 1000 - (i * 5 * 60 * 1000), 120 + i]);
        end;
        items := items + ']';
        SendResponse('application/json', items);
      end
      else
        SendResponse('application/json', '{"error":"Unknown dexcom"}', 404);
    end
    else
    begin
      // Normalize path-only (exclude querystring)
      pathOnly := Path;
      qpos := Pos('?', Path);
      query := '';
      if qpos > 0 then
      begin
        pathOnly := Copy(Path, 1, qpos - 1);
        query := Copy(Path, qpos + 1, MaxInt);
      end;

      if pathOnly = '/pebble' then
      begin
        // Return a realistic pebble payload with millisecond timestamps and numeric SGV
        ms := Int64(DateTimeToUnix(Now));
        entries := Format('{"status":[{"now":%d}],"bgs":[{"sgv":%d,"trend":4,"direction":"Flat","datetime":%d,"filtered":0,"unfiltered":-127,"noise":1,"bgdelta":-0.2,"battery":"","iob":"unknown"}]}',
          [ms * 1000, 102, (ms * 1000) - (5 * 60 * 1000)]);
        SendResponse('application/json', entries);
      end
      else if pathOnly = '/sgv.json' then
      begin
        // Support optional count parameter in querystring (default 16 like PHP server)
        count := 16;
        if Pos('count=', query) > 0 then
        begin
          cpos := Pos('count=', query) + 6;
          amp := Pos('&', Copy(query, cpos, MaxInt));
          cstr := '';
          if amp = 0 then cstr := Copy(query, cpos, MaxInt)
          else cstr := Copy(query, cpos, amp - 1);
          count := StrToIntDef(cstr, 16);
        end;
        if count < 0 then count := 0;
        if count > 200 then count := 200;
        entries := '[';
        for i := 0 to count - 1 do
        begin
          if i > 0 then entries := entries + ',';
          d := generateObjectId;
          dt := IntToStr(Int64(DateTimeToUnix(Now)) * 1000);
          entries := entries + Format('{"_id":"%s","device":"xDrip-DexcomG5","date":%s,"dateString":"2020-01-01T00:00:00Z","sgv":%d}', [d, dt, 60 + (i mod 140)]);
        end;
        entries := entries + ']';
        SendResponse('application/json', entries);
      end
      else if pathOnly = '/status.json' then
      begin
        SendResponse('application/json', '{"status":"ok","bgHigh":260,"bgLow":55,"bgTargetTop":180,"bgTargetBottom":80}');
      end
      else if (pathOnly = '/api/v1/status.json') then
      begin
        // Check API-SECRET header; if provided and incorrect, return Unauthorized
        okAuth := True;
        provided := '';
        if Assigned(Headers) then
        begin
          provided := Headers.Values['API-SECRET'];
          if provided = '' then provided := Headers.Values['api-secret'];
          if provided <> '' then
            okAuth := (provided = SHA1Print(SHA1String('test22')));
        end;
        if not okAuth then
        begin
          SendResponse('text/plain', 'Unauthorized', 401);
        end
        else
        begin
          SendResponse('application/json', '{"status":"ok","thresholds":{"bgHigh":260,"bgTargetTop":180,"bgTargetBottom":80,"bgLow":55}}');
        end;
      end
      else if pathOnly = '/api/v1/entries/sgv.json' then
      begin
        // Respect 'count' query parameter and return an array of entries
        cnt := 16;
        if Pos('count=', query) > 0 then
        begin
          cpos := Pos('count=', query) + 6;
          amp := Pos('&', Copy(query, cpos, MaxInt));
          cstr := '';
          if amp = 0 then
            cstr := Copy(query, cpos, MaxInt)
          else
            cstr := Copy(query, cpos, amp - 1);
          cnt := StrToIntDef(cstr, 16);
        end;
        if cnt < 0 then cnt := 0;
        if cnt > 200 then cnt := 200;

        entries := '[';
        for i := 0 to cnt - 1 do
        begin
          if i > 0 then entries := entries + ',';
          d := generateObjectId;
          dt := IntToStr(Int64(DateTimeToUnix(Now)) * 1000);
          entries := entries + Format('{"_id":"%s","device":"NightScoutTest","date":%s,"dateString":"2020-01-01T00:00:00Z","sgv":%d}',
            [d, dt, 60 + (i mod 140)]);
        end;
        entries := entries + ']';
        SendResponse('application/json', entries);
      end
      else if pathOnly = '/reset' then
      begin
        LatestValue := '102';
        SendResponse('text/plain', 'ok');
      end
      else if pathOnly = '/latest' then
      begin
        SendResponse('text/plain', LatestValue);
      end
      else
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
  // Bind to any address (0.0.0.0)
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
    // already started
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
  // Close listening socket to interrupt accept
  if ServerListenSocket <> -1 then
    CloseSocket(ServerListenSocket);
  // wait for it to stop
  while (ServerListenSocket <> -1) do
    Sleep(10);
  ServerThread.Free;
  ServerThread := nil;
end;

end.
