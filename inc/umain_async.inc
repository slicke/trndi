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

  if FOwner.FShuttingDown then
    Exit;

  if FOnline then
  begin
    if FIsDNSRequest then
      ShowMessage(RS_DNS_INTERNET_OK);
    FOwner.FLastConnectionDetail := '';
    FOwner.SetConnectionBadge(RS_CONN_OK, RGBToColor(60, 150, 90));
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
  FDSet: TFDSet;
  TimeVal: TTimeVal;
  SelectResult: longint;
  NonBlocking: u_long;
  ErrCode: integer;
  ErrLen: integer;
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

      // Set socket to non-blocking mode
      NonBlocking := 1;
      if ioctlsocket(XSocket, -2147195266, @NonBlocking) <> 0 then
        Exit;

      // Attempt connect (will return WSAEWOULDBLOCK)
      if connect(XSocket, Addr, SizeOf(Addr)) = 0 then
      begin
        Result := true;
      end
      else if WSAGetLastError = WSAEWOULDBLOCK then
      begin
        // Use select with 2-second timeout to wait for writability
        FD_ZERO(FDSet);
        FD_SET(XSocket, FDSet);
        TimeVal.tv_sec := 2;
        TimeVal.tv_usec := 0;

        SelectResult := select(XSocket + 1, nil, @FDSet, nil, @TimeVal);
        if SelectResult > 0 then
        begin
          ErrLen := SizeOf(ErrCode);
          if getsockopt(XSocket, SOL_SOCKET, SO_ERROR, PChar(@ErrCode), ErrLen) = 0 then
            Result := ErrCode = 0;
        end;
        // SelectResult = 0 means timeout (not reachable)
        // SelectResult < 0 means select error
      end;
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
  begin
    if Terminated then
    begin
      Result := false;
      Exit;
    end;
    if IsURLReachable(IPs[i], 53) then
    begin
      Result := true;
      Exit;
    end;
  end;

  Result := false;
end;
{$else}
function IsURLReachable(const URL: string; Port: word = 80): boolean;
  var
    Socket: longint;
    Addr: TInetSockAddr;
    HostAddr: THostAddr;
    FDSet: TFDSet;
    TimeVal: TTimeVal;
    SelectResult: longint;
    Flags: longint;
    ErrCode: LongInt;
    ErrLen: TSockLen;
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

        // Set socket to non-blocking mode
        Flags := fpFcntl(Socket, F_GETFL, 0);
        if Flags < 0 then
          Exit;
        if fpFcntl(Socket, F_SETFL, Flags or O_NONBLOCK) < 0 then
          Exit;

        // Attempt connect (will return -1 with errno = EINPROGRESS)
        if fpConnect(Socket, @Addr, SizeOf(Addr)) = 0 then
          Result := true
        else
        if fpGetErrno = ESysEINPROGRESS then
        begin
          // Use fpSelect with 2-second timeout to wait for writability
          FpFD_ZERO(FDSet);
          FpFD_SET(Socket, FDSet);
          TimeVal.tv_sec := 2;
          TimeVal.tv_usec := 0;

          SelectResult := fpSelect(Socket + 1, nil, @FDSet, nil, @TimeVal);
          if SelectResult > 0 then
          begin
            ErrLen := SizeOf(ErrCode);
            if fpGetSockOpt(Socket, SOL_SOCKET, SO_ERROR, @ErrCode, @ErrLen) = 0 then
              Result := ErrCode = 0;
          end;
          // SelectResult = 0 means timeout (not reachable)
          // SelectResult < 0 means select error
        end;
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
    begin
      if Terminated then
      begin
        Result := false;
        Exit;
      end;
      if IsURLReachable(IPs[i], 53) then
      begin
        Result := true;
        Exit;
      end;
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
  FSuccess := false;
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

  if FOwner.FShuttingDown then
    Exit;

  if FOwner.lastReading.date <> FPredictionLastReadingTime then
    Exit;

  if not FSuccess then
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

  FSuccess := ok;

  // If prediction failed, keep existing cache by not overwriting it
  if not ok then
    SetLength(FPreds, 0);

  if Terminated then
    Exit;
  Synchronize(@ApplyResult);
end;
