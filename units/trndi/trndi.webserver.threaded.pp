unit trndi.webserver.threaded;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, BaseUnix, fpjson, jsonparser, trndi.funcs, trndi.types;

type
  { Callback function types for thread-safe data access }
  TGetCurrentReadingFunc = function: BGReading of object;
  TGetPredictionsFunc = function: BGResults of object;

  { TWebServerThread }
  TWebServerThread = class(TThread)
  private
    FPort: Word;
    FAuthToken: string;
    FServerSocket: TSocket;
    FGetCurrentReading: TGetCurrentReadingFunc;
    FGetPredictions: TGetPredictionsFunc;
    function ReadingToJSON(const Reading: BGReading; IncludeDelta: Boolean = True): TJSONObject;
    function HandleRequest(const Request: string): string;
    function CheckAuth(const Headers: string): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Word; const AAuthToken: string;
                       AGetCurrentReading: TGetCurrentReadingFunc;
                       AGetPredictions: TGetPredictionsFunc);
    destructor Destroy; override;
    procedure CloseServerSocket;
  end;

  { TTrndiWebServer }
  TTrndiWebServer = class
  private
    FThread: TWebServerThread;
    FPort: Word;
    FEnabled: Boolean;
  public
    constructor Create(APort: Word; const AAuthToken: string;
                       AGetCurrentReading: TGetCurrentReadingFunc;
                       AGetPredictions: TGetPredictionsFunc);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function Active: Boolean;
    property Port: Word read FPort;
    property Enabled: Boolean read FEnabled;
  end;

implementation

const
  INVALID_SOCKET = TSocket(-1);

{ TWebServerThread }

constructor TWebServerThread.Create(APort: Word; const AAuthToken: string;
                                    AGetCurrentReading: TGetCurrentReadingFunc;
                                    AGetPredictions: TGetPredictionsFunc);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := True;  // Let thread clean itself up
  FPort := APort;
  FAuthToken := AAuthToken;
  FGetCurrentReading := AGetCurrentReading;
  FGetPredictions := AGetPredictions;
  FServerSocket := INVALID_SOCKET;
end;

destructor TWebServerThread.Destroy;
begin
  if FServerSocket <> INVALID_SOCKET then
    CloseSocket(FServerSocket);
  inherited Destroy;
end;

procedure TWebServerThread.CloseServerSocket;
begin
  if FServerSocket <> INVALID_SOCKET then
  begin
    // Shutdown the socket to interrupt any blocking accept/recv calls
    fpShutdown(FServerSocket, 2);  // SHUT_RDWR = 2
    CloseSocket(FServerSocket);
    FServerSocket := INVALID_SOCKET;
  end;
end;

function TWebServerThread.CheckAuth(const Headers: string): Boolean;
var
  Lines: TStringList;
  i: Integer;
  Line: string;
begin
  Result := True; // Allow if no token configured
  if FAuthToken = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := Headers;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      if (Pos('Authorization:', Line) = 1) or (Pos('authorization:', Line) = 1) then
      begin
        Result := Pos('Bearer ' + FAuthToken, Line) > 0;
        Exit;
      end;
    end;
  finally
    Lines.Free;
  end;
  Result := False;
end;

function TWebServerThread.ReadingToJSON(const Reading: BGReading; IncludeDelta: Boolean): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    // Add both mg/dL and mmol/L values
    Result.Add('mgdl', FormatFloat('0.0', Reading.val));
    Result.Add('mmol', FormatFloat('0.0', Reading.convert(mmol, BGPrimary)));
    
    if IncludeDelta then
    begin
      Result.Add('mgdl_delta', FormatFloat('0.0', Reading.delta));
      Result.Add('mmol_delta', FormatFloat('0.0', Reading.convert(mmol, BGDelta)));
    end;
    
    Result.Add('trend', Integer(Reading.trend));
    Result.Add('timestamp', DateTimeToStr(Reading.date));
  except
    Result.Free;
    raise;
  end;
end;

function TWebServerThread.HandleRequest(const Request: string): string;
var
  Lines: TStringList;
  Method, URI, Headers: string;
  ResponseObj: TJSONObject;
  CurrentReading: BGReading;
  Predictions: BGResults;
  PredArray: TJSONArray;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Request;
    if Lines.Count = 0 then
    begin
      Result := 'HTTP/1.1 400 Bad Request'#13#10#13#10;
      Exit;
    end;

    // Parse first line: METHOD URI HTTP/1.x
    Method := Copy(Lines[0], 1, Pos(' ', Lines[0]) - 1);
    URI := Copy(Lines[0], Pos(' ', Lines[0]) + 1, 1000);
    URI := Copy(URI, 1, Pos(' ', URI) - 1);
    Headers := Lines.Text;

    // CORS preflight
    if Method = 'OPTIONS' then
    begin
      Result := 'HTTP/1.1 204 No Content'#13#10 +
                'Access-Control-Allow-Origin: *'#13#10 +
                'Access-Control-Allow-Methods: GET, POST, OPTIONS'#13#10 +
                'Access-Control-Allow-Headers: Content-Type, Authorization'#13#10#13#10;
      Exit;
    end;

    // Check auth
    if not CheckAuth(Headers) then
    begin
      Result := 'HTTP/1.1 401 Unauthorized'#13#10 +
                'Content-Type: application/json'#13#10 +
                'Access-Control-Allow-Origin: *'#13#10#13#10 +
                '{"error":"Unauthorized"}';
      Exit;
    end;

    // Route requests
    ResponseObj := TJSONObject.Create;
    try
      if URI = '/glucose' then
      begin
        if Assigned(FGetCurrentReading) then
        begin
          CurrentReading := FGetCurrentReading();
          if not CurrentReading.empty then
          begin
            ResponseObj.Add('current', ReadingToJSON(CurrentReading, True));
            Result := 'HTTP/1.1 200 OK'#13#10;
          end
          else
          begin
            ResponseObj.Add('error', 'No data available');
            Result := 'HTTP/1.1 503 Service Unavailable'#13#10;
          end;
        end
        else
        begin
          ResponseObj.Add('error', 'Service not configured');
          Result := 'HTTP/1.1 500 Internal Server Error'#13#10;
        end;
      end
      else if URI = '/predict' then
      begin
        PredArray := TJSONArray.Create;
        if Assigned(FGetPredictions) then
        begin
          Predictions := FGetPredictions();
          for i := 0 to High(Predictions) do
            PredArray.Add(ReadingToJSON(Predictions[i], True));
        end;
        ResponseObj.Add('predictions', PredArray);
        Result := 'HTTP/1.1 200 OK'#13#10;
      end
      else if URI = '/status' then
      begin
        ResponseObj.Add('status', 'ok');
        ResponseObj.Add('data_available', Assigned(FGetCurrentReading));
        Result := 'HTTP/1.1 200 OK'#13#10;
      end
      else
      begin
        ResponseObj.Add('error', 'Not found');
        Result := 'HTTP/1.1 404 Not Found'#13#10;
      end;

      Result := Result +
                'Content-Type: application/json'#13#10 +
                'Access-Control-Allow-Origin: *'#13#10 +
                'Connection: close'#13#10#13#10 +
                ResponseObj.AsJSON;
    finally
      ResponseObj.Free;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TWebServerThread.Execute;
var
  ClientSocket: TSocket;
  Request, Response: string;
  Buffer: array[0..4095] of Char;
  BytesRead: Integer;
  SockAddr: TInetSockAddr;
  SockLen: TSockLen;
  OptVal: Integer;
  ReadFDs: TFDSet;
  TimeVal: TTimeVal;
  SelectResult: Integer;
begin
  try
    // Create socket
    FServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
    if FServerSocket = INVALID_SOCKET then
      Exit;

    // Set socket options
    OptVal := 1;
    fpSetSockOpt(FServerSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

    // Bind to port
    FillChar(SockAddr, SizeOf(SockAddr), 0);
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_port := htons(FPort);
    SockAddr.sin_addr.s_addr := INADDR_ANY;
    
    if fpBind(FServerSocket, @SockAddr, SizeOf(SockAddr)) <> 0 then
    begin
      CloseSocket(FServerSocket);
      FServerSocket := INVALID_SOCKET;
      Exit;
    end;

    // Listen
    if fpListen(FServerSocket, 5) <> 0 then
    begin
      CloseSocket(FServerSocket);
      FServerSocket := INVALID_SOCKET;
      Exit;
    end;

    // Accept loop with select timeout
    while not Terminated do
    begin
      // Check if socket was closed (during shutdown)
      if FServerSocket = INVALID_SOCKET then
        Break;
      
      // Use select with timeout to check for incoming connections
      fpFD_ZERO(ReadFDs);
      fpFD_SET(FServerSocket, ReadFDs);
      TimeVal.tv_sec := 0;
      TimeVal.tv_usec := 500000;  // 500ms timeout
      
      SelectResult := fpSelect(FServerSocket + 1, @ReadFDs, nil, nil, @TimeVal);
      
      // Check terminated after select
      if Terminated then
        Break;
      
      // If select returned error or timeout, continue loop
      if SelectResult <= 0 then
        Continue;
        
      // Socket is ready for accept
      SockLen := SizeOf(SockAddr);
      ClientSocket := fpAccept(FServerSocket, @SockAddr, @SockLen);
      
      // Check terminated again after accept
      if Terminated then
      begin
        if ClientSocket <> INVALID_SOCKET then
          CloseSocket(ClientSocket);
        Break;
      end;
      
      if ClientSocket <> INVALID_SOCKET then
      begin
        try
          // Read request
          Request := '';
          repeat
            FillChar(Buffer, SizeOf(Buffer), 0);
            BytesRead := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer), 0);
            if BytesRead > 0 then
              Request := Request + Copy(Buffer, 0, BytesRead);
          until (BytesRead <= 0) or (Pos(#13#10#13#10, Request) > 0);

          if BytesRead > 0 then
          begin
            // Handle request and send response
            Response := HandleRequest(Request);
            fpSend(ClientSocket, @Response[1], Length(Response), 0);
          end;
        finally
          CloseSocket(ClientSocket);
        end;
      end;
    end;
  except
    // Silently handle exceptions during shutdown
  end;
  
  if FServerSocket <> INVALID_SOCKET then
  begin
    CloseSocket(FServerSocket);
    FServerSocket := INVALID_SOCKET;
  end;
end;

{ TTrndiWebServer }

constructor TTrndiWebServer.Create(APort: Word; const AAuthToken: string;
                                   AGetCurrentReading: TGetCurrentReadingFunc;
                                   AGetPredictions: TGetPredictionsFunc);
begin
  inherited Create;
  FPort := APort;
  FEnabled := False;
  FThread := TWebServerThread.Create(APort, AAuthToken, AGetCurrentReading, AGetPredictions);
end;

destructor TTrndiWebServer.Destroy;
begin
  Stop;
  // Thread will free itself (FreeOnTerminate = True)
  inherited Destroy;
end;

procedure TTrndiWebServer.Start;
begin
  if not FEnabled and Assigned(FThread) then
  begin
    FThread.Start;
    FEnabled := True;
  end;
end;

procedure TTrndiWebServer.Stop;
begin
  if FEnabled and Assigned(FThread) then
  begin
    // Close the server socket first to unblock fpAccept
    FThread.CloseServerSocket;
    
    // Just signal termination, don't wait (thread will free itself)
    FThread.Terminate;
    FThread := nil;  // Clear reference since thread will free itself
    FEnabled := False;
  end;
end;

function TTrndiWebServer.Active: Boolean;
begin
  Result := FEnabled and Assigned(FThread) and not FThread.Finished;
end;

end.
