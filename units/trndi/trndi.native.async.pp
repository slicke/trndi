unit trndi.native.async;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, process, trndi.native.base;

type
  {
  THTTPResponseCallback:
    Callback invoked on the worker thread when an HTTP response is available.
    Ownership: the callback executes on the worker thread; the caller must
    not assume execution on the main/UI thread. The `THTTPResponse` record
    contains owned `TStringList` instances for `Headers` and `Cookies` which
    are created by the worker and remain valid for the duration of the
    callback. Callers copying or storing pointers must duplicate them.

  TRunAndCaptureCallback:
    Callback invoked with captured stdout and an exit code. Called on the
    worker thread. On timeout the ExitCode will be -1 and Stdout contains
    any data captured until termination.
  }
  THTTPResponseCallback = procedure(const resp: THTTPResponse) of object;
  TRunAndCaptureCallback = procedure(const OutS: string; ExitCode: integer) of object;

  TRequestExWorker = class(TThread)
  private
    FNativeObj: TTrndiNativeBase;
    FPost: boolean;
    FEndpoint: string;
    FParams: TStringList;
    FJsonData: string;
    FCookieJar: TStringList;
    FFollowRedirects: boolean;
    FMaxRedirects: integer;
    FCustomHeaders: TStringList;
    FPrefix: boolean;
    FCallback: THTTPResponseCallback;
    FResponse: THTTPResponse;
    FDone: TEvent;
    FCookieJarOwned: TStringList;
    FCustomHeadersOwned: TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create(ANativeObj: TTrndiNativeBase; const APost: boolean;
      const AEndpoint: string; const AParams: array of string; const AJsonData: string;
      ACookieJar: TStringList; AFollowRedirects: boolean; AMaxRedirects: integer;
      ACustomHeaders: TStringList; APrefix: boolean; ACallback: THTTPResponseCallback);
    destructor Destroy; override;
    property Response: THTTPResponse read FResponse;
  end;

  TRunAndCaptureWorker = class(TThread)
  private
    FExec: string;
    FParams: TStringList;
    FCallback: TRunAndCaptureCallback;
    FStdoutS: string;
    FExitCode: integer;
    FDone: TEvent;
    FTerminatedByCaller: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AExec: string; const AParams: array of string;
      ACallback: TRunAndCaptureCallback);
    destructor Destroy; override;
    property StdoutS: string read FStdoutS;
    property ExitCode: integer read FExitCode;
  end;

function RequestExAsync(const nativeObj: TTrndiNativeBase; const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; callback: THTTPResponseCallback = nil): TThread;

function RunAndCaptureSimpleAsync(const Exec: string;
  const Params: array of string; onFinish: TRunAndCaptureCallback): TThread;

function RunAndCaptureSimpleWait(const Exec: string; const Params: array of string;
  out StdoutS: string; out ExitCode: integer; TimeoutMs: Cardinal = 2000): boolean;

function RequestExWait(const nativeObj: TTrndiNativeBase; const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; TimeoutMs: Cardinal = 5000): THTTPResponse;

implementation

constructor TRequestExWorker.Create(ANativeObj: TTrndiNativeBase; const APost: boolean;
  const AEndpoint: string; const AParams: array of string; const AJsonData: string;
  ACookieJar: TStringList; AFollowRedirects: boolean; AMaxRedirects: integer;
  ACustomHeaders: TStringList; APrefix: boolean; ACallback: THTTPResponseCallback);
var
  i: integer;
begin
  inherited Create(True);
  FreeOnTerminate := False; // caller controls lifetime for wait-based usage
  FNativeObj := ANativeObj;
  FPost := APost;
  FEndpoint := AEndpoint;
  FJsonData := AJsonData;
  FParams := TStringList.Create;
  for i := Low(AParams) to High(AParams) do
    FParams.Add(AParams[i]);
  // take ownership copies of string lists to avoid use-after-free
  if Assigned(ACookieJar) then
  begin
    FCookieJarOwned := TStringList.Create;
    FCookieJarOwned.Assign(ACookieJar);
    FCookieJar := FCookieJarOwned;
  end
  else
    FCookieJar := nil;
  FFollowRedirects := AFollowRedirects;
  FMaxRedirects := AMaxRedirects;
  if Assigned(ACustomHeaders) then
  begin
    FCustomHeadersOwned := TStringList.Create;
    FCustomHeadersOwned.Assign(ACustomHeaders);
    FCustomHeaders := FCustomHeadersOwned;
  end
  else
    FCustomHeaders := nil;
  FPrefix := APrefix;
  FCallback := ACallback;
  FDone := TEvent.Create(nil, True, False, '');
end;

destructor TRequestExWorker.Destroy;
begin
  FParams.Free;
  FCustomHeadersOwned.Free;
  FCookieJarOwned.Free;
  FDone.Free;
  inherited Destroy;
end;

procedure TRequestExWorker.Execute;
begin
  try
    try
      FResponse := FNativeObj.requestEx(FPost, FEndpoint, FParams.ToStringArray, FJsonData,
        FCookieJar, FFollowRedirects, FMaxRedirects, FCustomHeaders, FPrefix);
      if Assigned(FCallback) then
        FCallback(FResponse);
    except
      on E: Exception do
      begin
        // provide a defined failure response on exception
        FResponse.Body := '';
        FResponse.Headers := TStringList.Create;
        FResponse.Cookies := TStringList.Create;
        FResponse.StatusCode := -1;
        FResponse.FinalURL := '';
        FResponse.RedirectCount := 0;
        FResponse.Success := False;
        FResponse.ErrorMessage := E.ClassName + ': ' + E.Message;
        if Assigned(FCallback) then
          FCallback(FResponse);
      end;
    end;
  finally
    FDone.SetEvent;
  end;
end;

constructor TRunAndCaptureWorker.Create(const AExec: string;
  const AParams: array of string; ACallback: TRunAndCaptureCallback);
var
  i: integer;
begin
  inherited Create(True);
  FreeOnTerminate := False; // caller will free after wait
  FExec := AExec;
  FParams := TStringList.Create;
  for i := Low(AParams) to High(AParams) do
    FParams.Add(AParams[i]);
  FCallback := ACallback;
  FDone := TEvent.Create(nil, True, False, '');
end;

destructor TRunAndCaptureWorker.Destroy;
begin
  FParams.Free;
  FDone.Free;
  inherited Destroy;
end;

procedure TRunAndCaptureWorker.Execute;
var
  Proc: TProcess;
  buf: array[0..4095] of byte;
  n: integer;
  outS: string;
begin
  FStdoutS := '';
  FExitCode := -1;
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := FExec;
    Proc.Options := Proc.Options + [poUsePipes];
    Proc.ShowWindow := swoHide;
    Proc.Parameters.Assign(FParams);
    Proc.Execute;
    while (Proc.Running or (Proc.Output.NumBytesAvailable > 0)) and (not Terminated) do
    begin
      n := Proc.Output.Read(buf, SizeOf(buf));
      if n > 0 then
      begin
        SetString(outS, PAnsiChar(@buf[0]), n);
        FStdoutS := FStdoutS + outS;
      end
      else
        Sleep(5);
    end;
    // If termination requested while process running, try graceful terminate
    if Terminated and Proc.Running then
    begin
      try
        {$IF DEFINED(UNIX)}
        Proc.Terminate(15); // SIGTERM on Unix/Linux
        {$ELSE}
        Proc.Terminate;     // Windows
        {$ENDIF}
      except end;
      // wait for process to exit (with small loop)
      while Proc.Running do
      begin
        Sleep(10);
      end;
    end;
    // capture exit status if available
    try
      FExitCode := Proc.ExitStatus;
    except
      FExitCode := -1;
    end;
    if Assigned(FCallback) then
      FCallback(FStdoutS, FExitCode);
  finally
    Proc.Free;
    FDone.SetEvent;
  end;
end;

function RequestExAsync(const nativeObj: TTrndiNativeBase; const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; callback: THTTPResponseCallback = nil): TThread;
begin
  Result := TRequestExWorker.Create(nativeObj, post, endpoint, params, jsondata,
    cookieJar, followRedirects, maxRedirects, customHeaders, prefix, callback);
  // async callers expect the worker to free itself
  Result.FreeOnTerminate := True;
  Result.Start;
end;

function RequestExWait(const nativeObj: TTrndiNativeBase; const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; TimeoutMs: Cardinal = 5000): THTTPResponse;
var
  worker: TRequestExWorker;
begin
  worker := TRequestExWorker.Create(nativeObj, post, endpoint, params, jsondata,
    cookieJar, followRedirects, maxRedirects, customHeaders, prefix, nil);
  worker.Start;
  if worker.FDone.WaitFor(TimeoutMs) = wrSignaled then
  begin
    // worker finished; copy the response (deep copy lists) and free worker
    Result.Body := worker.Response.Body;
    Result.StatusCode := worker.Response.StatusCode;
    Result.FinalURL := worker.Response.FinalURL;
    Result.RedirectCount := worker.Response.RedirectCount;
    Result.Success := worker.Response.Success;
    Result.ErrorMessage := worker.Response.ErrorMessage;
    Result.Headers := TStringList.Create;
    if Assigned(worker.Response.Headers) then
      Result.Headers.Assign(worker.Response.Headers);
    Result.Cookies := TStringList.Create;
    if Assigned(worker.Response.Cookies) then
      Result.Cookies.Assign(worker.Response.Cookies);
    // update caller's cookieJar if provided
    if Assigned(cookieJar) then
      cookieJar.Assign(Result.Cookies);
    worker.WaitFor;
    worker.Free;
  end
  else
  begin
    // timeout: return a timeout-shaped response. We attempt to cancel the
    // worker but do not block indefinitely; worker owns its copies of lists
    // so returning now is safe w.r.t. those objects.
    Result.Body := '';
    Result.Headers := TStringList.Create;
    Result.Cookies := TStringList.Create;
    Result.StatusCode := -1;
    Result.FinalURL := '';
    Result.RedirectCount := 0;
    Result.Success := False;
    Result.ErrorMessage := 'timeout';
    // request cancellation attempt: signal thread termination and give it a short grace period
    try
      worker.Terminate;
      worker.FDone.WaitFor(5000);
      if worker.FDone.WaitFor(0) = wrSignaled then
      begin
        // finished within grace period; free resources
        worker.WaitFor;
        worker.Free;
      end;
    except end;
  end;
end;

function RunAndCaptureSimpleAsync(const Exec: string;
  const Params: array of string; onFinish: TRunAndCaptureCallback): TThread;
begin
  Result := TRunAndCaptureWorker.Create(Exec, Params, onFinish);
  Result.FreeOnTerminate := True;
  Result.Start;
end;

function RunAndCaptureSimpleWait(const Exec: string; const Params: array of string;
  out StdoutS: string; out ExitCode: integer; TimeoutMs: Cardinal = 2000): boolean;
var
  worker: TRunAndCaptureWorker;
begin
  worker := TRunAndCaptureWorker.Create(Exec, Params, nil);
  worker.Start;
  if worker.FDone.WaitFor(TimeoutMs) = wrSignaled then
  begin
    StdoutS := worker.StdoutS;
    ExitCode := worker.ExitCode;
    Result := ExitCode = 0;
    worker.WaitFor;
    worker.Free;
  end
  else
  begin
    // timeout: request termination and wait a short grace period for cleanup
    try
      worker.Terminate;
      worker.FDone.WaitFor(5000);
      if worker.FDone.WaitFor(0) = wrSignaled then
      begin
        StdoutS := worker.StdoutS;
        ExitCode := worker.ExitCode;
        Result := ExitCode = 0;
        worker.WaitFor;
        worker.Free;
      end
      else
      begin
        StdoutS := '';
        ExitCode := -1;
        Result := False;
      end;
    except
      StdoutS := '';
      ExitCode := -1;
      Result := False;
    end;
  end;
end;

end.
