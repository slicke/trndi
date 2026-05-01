unit trndi.native.async;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, process, trndi.native.base;

type
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
  FreeOnTerminate := False;
  FNativeObj := ANativeObj;
  FPost := APost;
  FEndpoint := AEndpoint;
  FJsonData := AJsonData;
  FParams := TStringList.Create;
  for i := Low(AParams) to High(AParams) do
    FParams.Add(AParams[i]);
  FCookieJar := ACookieJar;
  FFollowRedirects := AFollowRedirects;
  FMaxRedirects := AMaxRedirects;
  FCustomHeaders := ACustomHeaders;
  FPrefix := APrefix;
  FCallback := ACallback;
end;

destructor TRequestExWorker.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TRequestExWorker.Execute;
begin
  FResponse := FNativeObj.requestEx(FPost, FEndpoint, FParams.ToStringArray, FJsonData,
    FCookieJar, FFollowRedirects, FMaxRedirects, FCustomHeaders, FPrefix);
  if Assigned(FCallback) then
    FCallback(FResponse);
end;

constructor TRunAndCaptureWorker.Create(const AExec: string;
  const AParams: array of string; ACallback: TRunAndCaptureCallback);
var
  i: integer;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FExec := AExec;
  FParams := TStringList.Create;
  for i := Low(AParams) to High(AParams) do
    FParams.Add(AParams[i]);
  FCallback := ACallback;
end;

destructor TRunAndCaptureWorker.Destroy;
begin
  FParams.Free;
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
    while Proc.Running or (Proc.Output.NumBytesAvailable > 0) do
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
    FExitCode := Proc.ExitStatus;
    if Assigned(FCallback) then
      FCallback(FStdoutS, FExitCode);
  finally
    Proc.Free;
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
  try
    worker.Start;
    worker.WaitFor;
    Result := worker.Response;
  finally
    worker.Free;
  end;
end;

function RunAndCaptureSimpleAsync(const Exec: string;
  const Params: array of string; onFinish: TRunAndCaptureCallback): TThread;
begin
  Result := TRunAndCaptureWorker.Create(Exec, Params, onFinish);
  Result.Start;
end;

function RunAndCaptureSimpleWait(const Exec: string; const Params: array of string;
  out StdoutS: string; out ExitCode: integer; TimeoutMs: Cardinal = 2000): boolean;
var
  worker: TRunAndCaptureWorker;
begin
  worker := TRunAndCaptureWorker.Create(Exec, Params, nil);
  try
    worker.Start;
    worker.WaitFor;
    StdoutS := worker.StdoutS;
    ExitCode := worker.ExitCode;
    Result := ExitCode = 0;
  finally
    worker.Free;
  end;
end;

end.
