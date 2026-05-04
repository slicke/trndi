(* Async helpers: thread implementations moved out of umain_helpers.inc
   This include defines background worker threads used by the main form.
   It is intended to be included into units/forms/umain.pp only.
*)

constructor TConnectivityCheckThread.Create(AOwner: TfBG; IsDNSRequest: boolean);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FIsDNSRequest := IsDNSRequest;
  Start;
end;

procedure TConnectivityCheckThread.ApplyResult;
begin
  if not Assigned(FOwner) then
    Exit;

  if FOnline then
  begin
    if FIsDNSRequest then
    begin
      ShowMessage(RS_DNS_INTERNET_OK);
      FOwner.FLastConnectionDetail := '';
      FOwner.SetConnectionBadge(RS_CONN_OK, RGBToColor(60, 150, 90));
    end;
  end
  else
  begin
    FOwner.FLastConnectionDetail := RS_NO_INTERNET;
    FOwner.SetConnectionBadge(RS_CONN_RETRYING, RGBToColor(185, 60, 45));
  end;

  if Assigned(FOwner.tPing) then
    FOwner.tPing.Enabled := true;
end;

procedure TConnectivityCheckThread.Execute;
{$ifdef Windows}
function IsURLReachable(const URL: string; Port: word = 80): boolean;
var
  XSocket: TSocket;
  Addr: TSockAddr;
  HostEnt: PHostEnt;
begin
  Result := false;

  XSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if XSocket = INVALID_SOCKET then
    Exit;

  try
    try
      HostEnt := gethostbyname(pchar(URL));
      if HostEnt = nil then
        Exit;

      FillByte(Addr, SizeOf(Addr), 0);
      PSockAddrIn(@Addr)^.sin_family := AF_INET;
      PSockAddrIn(@Addr)^.sin_port := htons(Port);
      PSockAddrIn(@Addr)^.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;

      if connect(XSocket, Addr, SizeOf(Addr)) = 0 then
        Result := true;
    except
      Result := false;
    end;
  finally
    closesocket(XSocket);
  end;
end;
function IsInternetOnline: boolean;
var
  IPs: array of string;
  i: integer;
begin
  IPs := [
    '8.8.8.8',
    '1.1.1.1',
    '208.67.222.222'
    ];

  for i := Low(IPs) to High(IPs) do
    if IsURLReachable(IPs[i], 53) then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
end;
{$else}
function IsURLReachable(const URL: string; Port: word = 80): boolean;
  var
    Socket: longint;
    Addr: TInetSockAddr;
    HostAddr: THostAddr;
  begin
    Result := false;

    Socket := fpSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if Socket < 0 then
      Exit;

    try
      try
        HostAddr := StrToHostAddr(URL);

        if HostAddr.s_addr = 0 then
          Exit;

        FillByte(Addr, SizeOf(Addr), 0);
        Addr.sin_family := AF_INET;
        Addr.sin_port := htons(Port);
        Addr.sin_addr := HostAddr;

        if fpConnect(Socket, @Addr, SizeOf(Addr)) = 0 then
          Result := true;
      except
        Result := false;
      end;
    finally
      CloseSocket(Socket);
    end;
  end;
function IsInternetOnline: boolean;
  var
    IPs: array of string;
    i: integer;
  begin
    IPs := [
      '8.8.8.8',
      '1.1.1.1',
      '208.67.222.222'
      ];

    for i := Low(IPs) to High(IPs) do
      if IsURLReachable(IPs[i], 53) then
      begin
        Result := true;
        Exit;
      end;

    Result := false;
  end;
{$endif}
begin
  FOnline := IsInternetOnline;
  if Terminated then
    Exit;
  Synchronize(@ApplyResult);
end;

constructor TPredictionThread.Create(AOwner: TfBG; NumPredictions: integer);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FNumPredictions := NumPredictions;
  if Assigned(FOwner) then
    FPredictionLastReadingTime := FOwner.lastReading.date
  else
    FPredictionLastReadingTime := 0;
  Start;
end;

procedure TPredictionThread.ApplyResult;
begin
  if not Assigned(FOwner) then
    Exit;

  if FOwner.lastReading.date <> FPredictionLastReadingTime then
    Exit;

  // Store cached predictions and let the form render them
  PredictionCache := FPreds;
  PredictionLastReadingTime := FOwner.lastReading.date;
  FOwner.RenderPredictionCache(FPreds);
end;

procedure TPredictionThread.Execute;
var
  ok: boolean;
begin
  // Perform prediction work off the main thread
  ok := false;
  try
    if Assigned(FOwner) and Assigned(api) then
      ok := api.predictReadings(FNumPredictions, FPreds);
  except
    ok := false;
  end;

  // If prediction failed, ensure we clear any partial results
  if not ok then
    SetLength(FPreds, 0);

  if Terminated then
    Exit;
  Synchronize(@ApplyResult);
end;
