unit trndi.native.async;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, process, trndi.native;

type
  THTTPResponseCallback = procedure(const resp: THTTPResponse) of object;
  TRunAndCaptureCallback = procedure(const OutS: string; ExitCode: integer) of object;

function RequestExAsync(const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; callback: THTTPResponseCallback = nil): TThread;

function RunAndCaptureSimpleAsync(const Exec: string;
  const Params: array of string; onFinish: TRunAndCaptureCallback): TThread;

function RunAndCaptureSimpleWait(const Exec: string; const Params: array of string;
  out StdoutS: string; out ExitCode: integer; TimeoutMs: Cardinal = 2000): boolean;

function RequestExWait(const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; TimeoutMs: Cardinal = 5000): THTTPResponse;

implementation

function RequestExAsync(const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; callback: THTTPResponseCallback = nil): TThread;
var
  resp: THTTPResponse;
begin
  Result := TThread.CreateAnonymousThread(
    procedure
    begin
      resp := TrndiNative.requestEx(post, endpoint, params, jsondata,
        cookieJar, followRedirects, maxRedirects, customHeaders, prefix);
      if Assigned(callback) then
        TThread.Synchronize(nil, procedure begin callback(resp); end);
    end);
  Result.Start;
end;

function RequestExWait(const post: boolean; const endpoint: string;
  const params: array of string; const jsondata: string = '';
  cookieJar: TStringList = nil; followRedirects: boolean = true;
  maxRedirects: integer = 10; customHeaders: TStringList = nil;
  prefix: boolean = true; TimeoutMs: Cardinal = 5000): THTTPResponse;
var
  ev: TEvent;
  done: Boolean;
  respLocal: THTTPResponse;
begin
  // Initialize empty response
  respLocal.Body := '';
  respLocal.Headers := TStringList.Create;
  respLocal.Cookies := TStringList.Create;
  respLocal.StatusCode := -1;
  respLocal.FinalURL := '';
  respLocal.RedirectCount := 0;
  respLocal.Success := False;
  respLocal.ErrorMessage := 'timeout';

  done := False;
  ev := TEvent.Create(nil, True, False, '');
  try
    RequestExAsync(post, endpoint, params, jsondata, cookieJar, followRedirects,
      maxRedirects, customHeaders, prefix,
      procedure(const r: THTTPResponse)
      begin
        // copy result into local and signal
        respLocal := r;
        done := True;
        ev.SetEvent;
      end);

    if ev.WaitFor(TimeoutMs) = wrSignaled then
    begin
      Result := respLocal;
    end
    else
    begin
      Result := respLocal; // contains timeout marker
    end;
  finally
    ev.Free;
  end;
end;

function RunAndCaptureSimpleAsync(const Exec: string;
  const Params: array of string; onFinish: TRunAndCaptureCallback): TThread;
var
  Proc: TProcess;
  i: Integer;
  acc: TStringList;
begin
  acc := TStringList.Create;
  Proc := TProcess.Create(nil);
  Proc.Executable := Exec;
  for i := Low(Params) to High(Params) do
    Proc.Parameters.Add(Params[i]);
  Proc.Options := Proc.Options + [poUsePipes];

  Result := TThread.CreateAnonymousThread(
    procedure
    var
      buf: array[0..4095] of byte;
      n: integer;
      outS: string;
      exitCode: integer;
    begin
      try
        Proc.Execute;
        while Proc.Running or (Proc.Output.NumBytesAvailable > 0) do
        begin
          n := Proc.Output.Read(buf, SizeOf(buf));
          if n > 0 then
          begin
            SetString(outS, PAnsiChar(@buf[0]), n);
            acc.Text := acc.Text + outS;
          end
          else
            Sleep(5);
        end;
        exitCode := Proc.ExitStatus;
        if Assigned(onFinish) then
          TThread.Synchronize(nil, procedure begin onFinish(acc.Text, exitCode); end);
      finally
        Proc.Free;
        acc.Free;
      end;
    end);
  Result.Start;
end;

function RunAndCaptureSimpleWait(const Exec: string; const Params: array of string;
  out StdoutS: string; out ExitCode: integer; TimeoutMs: Cardinal = 2000): boolean;
var
  ev: TEvent;
  done: Boolean;
begin
  StdoutS := '';
  ExitCode := -1;
  done := False;
  ev := TEvent.Create(nil, True, False, '');
  try
    RunAndCaptureSimpleAsync(Exec, Params,
      procedure(const OutS: string; ECode: integer)
      begin
        StdoutS := OutS;
        ExitCode := ECode;
        done := True;
        ev.SetEvent;
      end);

    if ev.WaitFor(TimeoutMs) = wrSignaled then
      Result := done and (ExitCode = 0)
    else
      Result := False;
  finally
    ev.Free;
  end;
end;

end.
