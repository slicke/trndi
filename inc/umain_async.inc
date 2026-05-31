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

{------------------------------------------------------------------------------
  TGlucoseFetchThread
  -------------------
  Off-main-thread CGM fetch. Owns the synchronous api.getReadings call so the
  UI stays responsive while the network round-trip is in flight.
 ------------------------------------------------------------------------------}
constructor TGlucoseFetchThread.Create(AOwner: TfBG; Boot: boolean;
Force: boolean);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FBoot := Boot;
  FForce := Force;
  FErrorMsg := '';
  SetLength(FResults, 0);
  Start;
end;

procedure TGlucoseFetchThread.Execute;
begin
  // Worker thread — must not touch LCL/UI. api.getReadings is the synchronous
  // HTTP call we want to keep off the main thread. The Boot flag triggers
  // the legacy "two quick attempts" semantics from the old FormCreate path.
  try
    try
      if not Assigned(api) then
        Exit;

      {$ifdef DEBUG}
      FResults := api.getReadings(MAX_MIN, MAX_RESULT, '', FDiag);
      {$else}
      FResults := api.getReadings(MAX_MIN, MAX_RESULT);
      {$endif}

      if FBoot and (Length(FResults) = 0) and (not Terminated) then
      begin
        {$ifdef DEBUG}
        FResults := api.getReadings(MAX_MIN, MAX_RESULT, '', FDiag);
        {$else}
        FResults := api.getReadings(MAX_MIN, MAX_RESULT);
        {$endif}
      end;

      if Assigned(api) then
        FErrorMsg := api.errormsg;
    except
      on E: Exception do
      begin
        SetLength(FResults, 0);
        FErrorMsg := E.ClassName + ': ' + E.Message;
      end;
    end;
  finally
    if not Terminated then
      Synchronize(@ApplyResult);
  end;
end;

procedure TGlucoseFetchThread.ApplyResult;
var
  applied: boolean;
begin
  if not Assigned(FOwner) then
    Exit;

  if FOwner.FShuttingDown then
  begin
    // Owner is tearing down. Don't touch UI; only clear the in-flight flag
    // so a future fetch (post-restart) isn't gated.
    FOwner.FFetchInFlight := false;
    if FBoot then
      FOwner.FBootFetchPending := false;
    Exit;
  end;

  // Stop the boot spinner before the heavy main-thread work below — the
  // post-fetch chain (PlaceTrendDots, FinalizeUIUpdate → deferred resize)
  // can stall the message loop briefly, and a frozen mid-cycle spinner
  // reads as a hang. Lands on a "loaded" glyph; ProcessCurrentReading
  // overwrites it with the real value moments later.
  if FBoot then
    FOwner.StopBootSpinner;

  try
    {$ifdef DEBUG}
    if Assigned(FOwner.miDebugBackend) and FOwner.miDebugBackend.Checked and FOwner.Showing then
    begin
      if FDiag.IsEmpty then
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(FForce, 'Debug Info (Forced)', 'Debug Info'),
          '[empty!]', FDiag)
      else
        slicke.ux.alert.ExtLog(uxdAuto,
          IfThen(FForce, 'Debug Info (Forced)', 'Debug Info'),
          '', FDiag, uxmtCustom, 10);
    end;
    {$endif}
    applied := FOwner.ApplyFetchedReadings(FResults, FErrorMsg, FBoot);
    if applied then
    begin
      FOwner.ProcessCurrentReading;
      FOwner.FinalizeReadingUpdate(FBoot);
    end;
  finally
    FOwner.FFetchInFlight := false;
    if FBoot then
      FOwner.FBootFetchPending := false;
    if Assigned(native) then
      native.updateDone;
    // The thread reference owned by TfBG will be cleared by the next
    // RequestAsyncFetch (which creates a fresh instance) or at shutdown.
  end;
end;

{------------------------------------------------------------------------------
  THistoryFetchThread
  -------------------
  Off-main-thread CGM fetch for the history graph. Same shape as
  TGlucoseFetchThread but the window (minutes, max readings) is supplied per
  call and the result feeds ShowHistoryGraph on the main thread rather than
  the live UI's bgs slot.
 ------------------------------------------------------------------------------}
constructor THistoryFetchThread.Create(AOwner: TfBG; Minutes, MaxReadings: integer;
Mode: THistoryFetchMode);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FMinutes := Minutes;
  FMaxReadings := MaxReadings;
  FMode := Mode;
  FErrorMsg := '';
  SetLength(FResults, 0);
  Start;
end;

procedure THistoryFetchThread.Execute;
var
  diag: string;
begin
  // Worker thread — must not touch LCL/UI. The synchronous api.getReadings
  // call is the only thing here.
  try
    try
      if not Assigned(api) then
        Exit;

      FResults := api.getReadings(FMinutes, FMaxReadings, '', diag);

      if Assigned(api) then
        FErrorMsg := api.errormsg;
      if (FErrorMsg = '') and (diag <> '') then
        FErrorMsg := diag;
    except
      on E: Exception do
      begin
        SetLength(FResults, 0);
        FErrorMsg := E.ClassName + ': ' + E.Message;
      end;
    end;
  finally
    if not Terminated then
      Synchronize(@ApplyResult);
  end;
end;

procedure THistoryFetchThread.ApplyResult;
var
  cgmHi, cgmLo, cgmRangeHi, cgmRangeLo: integer;
begin
  if not Assigned(FOwner) then
    Exit;

  if FOwner.FShuttingDown then
  begin
    // Owner is tearing down — drop everything, just clear the flag so a
    // hypothetical post-restart fetch isn't gated.
    FOwner.FHistoryFetchInFlight := false;
    Exit;
  end;

  try
    if Length(FResults) = 0 then
    begin
      case FMode of
      hfm24h:
        if FErrorMsg <> '' then
          ShowMessage('No readings returned: ' + FErrorMsg)
        else
          ShowMessage('No readings returned for the last 24 hours.');
      hfmDatePicker:
        if FErrorMsg <> '' then
          ShowMessage(Format(RS_DATE_PICKER_NO_READINGS_ERR, [FErrorMsg]))
        else
          ShowMessage(RS_DATE_PICKER_NO_READINGS);
      end;
      Exit;
    end;

    if Assigned(api) then
    begin
      cgmHi := api.cgmHi;
      cgmLo := api.cgmLo;
      cgmRangeHi := api.cgmRangeHi;
      cgmRangeLo := api.cgmRangeLo;
    end
    else
    begin
      cgmHi := 180; cgmLo := 60; cgmRangeHi := 160; cgmRangeLo := 80;
    end;

    ShowHistoryGraph(FResults, un, CurrentHistoryGraphPalette,
      cgmHi, cgmLo, cgmRangeHi, cgmRangeLo);
    FOwner.AutoEnableBasalOverlay;
    if FMode = hfm24h then
      FOwner.AutoAddPredictionOverlay;
  finally
    FOwner.FHistoryFetchInFlight := false;
    if Assigned(FOwner.mi24h) then
      FOwner.mi24h.Enabled := true;
    if Assigned(FOwner.miReadingsSince) then
      FOwner.miReadingsSince.Enabled := true;
  end;
end;

{------------------------------------------------------------------------------
  TUpdateCheckThread
  ------------------
  Off-main-thread GitHub releases check. The synchronous TrndiNative.getURL
  call, the JSON parse, and the HasNewerRelease/GetNewerVersionURL helpers all
  run in Execute; ApplyResult is the only place that touches dialogs or
  native.SetSetting.
 ------------------------------------------------------------------------------}
constructor TUpdateCheckThread.Create(AOwner: TfBG; ShowUpToDateMessage: boolean);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FShowUpToDate := ShowUpToDateMessage;
  FFetchOK := false;
  FHasNewer := false;
  FLatestRelease := 'Unknown';
  Start;
end;

procedure TUpdateCheckThread.Execute;
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
begin
  try
    try
      FFetchOK := TrndiNative.getURL(
        'https://api.github.com/repos/slicke/trndi/releases/latest', FResponse);
      if (not FFetchOK) or Terminated then
        Exit;

      JsonData := nil;
      try
        JsonData := GetJSON(FResponse);
        if Assigned(JsonData) and (JsonData is TJSONObject) then
        begin
          JsonObj := TJSONObject(JsonData);
          FLatestRelease := JsonObj.Get('name', JsonObj.Get('tag_name', 'Unknown'));
        end;
      except
        FLatestRelease := 'Parse error';
      end;
      if Assigned(JsonData) then
        FreeAndNil(JsonData);

      if Terminated then
        Exit;

      FHasNewer := HasNewerRelease(FResponse, FReleaseName, false);
      if FHasNewer then
      begin
        FDownloadURL := GetNewerVersionURL(FResponse);
        if FDownloadURL = '' then
          FDownloadURL := 'https://github.com/slicke/trndi/releases/latest';
      end;
    except
      FFetchOK := false;
    end;
  finally
    if not Terminated then
      Synchronize(@ApplyResult);
  end;
end;

procedure TUpdateCheckThread.ApplyResult;
var
  newVersionMessage: string;
begin
  if not Assigned(FOwner) then
    Exit;

  if FOwner.FShuttingDown then
  begin
    FOwner.FUpdateCheckInFlight := false;
    Exit;
  end;

  try
    if not FFetchOK then
    begin
      if FShowUpToDate then
        ShowMessage('Update check failed.');
      Exit;
    end;

    if FHasNewer then
    begin
      newVersionMessage := Format(RS_NEWVER, [FReleaseName]);
      case UXDialog(uxdAuto, RS_NEWVER_CAPTION, newVersionMessage,
        [mbYes, mbNo, mbIgnore], mtInformation) of
      mrYes:
        OpenURL(FDownloadURL);
      mrIgnore:
        begin
          if Assigned(native) then
            native.SetSetting('update.ignore.date',
              FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
          ShowMessage(Format(RS_UPDATE_SNOOZE, [DateTimeToStr(IncDay(Now, 14))]));
        end;
      end;
    end
    else if FShowUpToDate then
    begin
      if BUILD_NUMBER = 'dev' then
        ShowMessage('Up to date (Dev Build)' + LineEnding +
          'Build: ' + BUILD_NUMBER + LineEnding +
          'Branch: ' + GIT_BRANCH + LineEnding +
          'Build Date: ' + {$I %DATE%} + ' ' + {$I %TIME%} + LineEnding +
          'Latest release: ' + FLatestRelease + LineEnding + LineEnding +
          'Note: Dev builds are always considered "newer" than releases')
      else
        ShowMessage(RS_UPTODATE);
    end;
  finally
    FOwner.FUpdateCheckInFlight := false;
  end;
end;
