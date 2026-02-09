unit php_server_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fphttpclient;

function StartOrUseTestServer(var PHPProcess: TProcess; out BaseURL: string): boolean;
procedure StopLocalTestServer(var PHPProcess: TProcess);

implementation

var
  SkipPHPNoticePrinted: Boolean = False;
  RunningNoticePrinted: Boolean = False;


function StartOrUseTestServer(var PHPProcess: TProcess; out BaseURL: string): boolean;
var
  env: string;
  client: TFPHTTPClient;
  i: Integer;
  url: string;
  phpExe: string;
  probeProc: TProcess;
begin
  Result := False;
  // Respect TRNDI_NO_PHP env to explicitly skip PHP-dependent tests
  if GetEnvironmentVariable('TRNDI_NO_PHP') = '1' then
  begin
    PHPProcess := nil;
    BaseURL := '';
    // Indicate server not available so callers will skip tests
    Result := False;
    Exit;
  end;

  env := GetEnvironmentVariable('TRNDI_TEST_SERVER_URL');
  if env <> '' then
  begin
    PHPProcess := nil;
    BaseURL := env;
    Result := True;
    Exit;
  end;

  // Start external PHP built-in test server on localhost:8080
  PHPProcess := nil;

  // Detect PHP executable. Prefer Windows default C:\php\php.exe if present,
  // otherwise probe for a 'php' on PATH by attempting to execute 'php -v'.
  // Allow explicit override via env var (useful for CI / custom installs)
  phpExe := GetEnvironmentVariable('TRNDI_PHP_EXECUTABLE');
  if phpExe <> '' then
  begin
    // Normalize quoted paths (e.g. "C:\path with spaces\php.exe") so FileExists works
    if (Length(phpExe) >= 2) and (phpExe[1] = '"') and (phpExe[Length(phpExe)] = '"') then
      phpExe := Copy(phpExe, 2, Length(phpExe) - 2);

    // If value is 'php' we will probe the PATH here; otherwise accept a valid path
    if phpExe = 'php' then
    begin
      probeProc := TProcess.Create(nil);
      try
        probeProc.Options := probeProc.Options + [poNoConsole];
        probeProc.CommandLine := 'php -v';
        try
          probeProc.Execute;
          // php on PATH - keep phpExe = 'php'
        except
          phpExe := '';
        end;
      finally
        probeProc.Free;
      end;
    end
    else if not FileExists(phpExe) then
      phpExe := '';
  end;

  // If not set via env, prefer Windows default install path
  if phpExe = '' then
  begin
    if FileExists('C:\php\php.exe') then
      phpExe := 'C:\php\php.exe'
    else
    begin
      probeProc := TProcess.Create(nil);
      try
        probeProc.Options := probeProc.Options + [poNoConsole];
        probeProc.CommandLine := 'php -v';
        try
          probeProc.Execute;
          phpExe := 'php';
        except
          phpExe := '';
        end;
      finally
        probeProc.Free;
      end;
    end;
  end;

  if phpExe = '' then
  begin
    // No php found - don't attempt to launch, caller will skip PHP-requiring tests
    PHPProcess := nil;
    if not SkipPHPNoticePrinted then
    begin
      Writeln('Test Server tests ignored, missing php');
      SkipPHPNoticePrinted := True;
    end;
    Exit(False);
  end;

  try
    if not RunningNoticePrinted then
    begin
      Writeln('Running Test Server');
      RunningNoticePrinted := True;
    end;
    PHPProcess := TProcess.Create(nil);
    // Launch: use explicit php executable path (quoted) to handle spaces in path
    PHPProcess.Options := PHPProcess.Options + [poNoConsole, poNewProcessGroup];
    PHPProcess.CommandLine := '"' + phpExe + '" -S 127.0.0.1:8080 -t tests/testserver tests/testserver/index.php';
    try
      PHPProcess.Execute;
    except
      PHPProcess.Free;
      PHPProcess := nil;
      Exit(False);
    end;

    BaseURL := 'http://127.0.0.1:8080';

    // Poll the server's /debug endpoint up to ~2s
    url := BaseURL + '/debug';
    client := TFPHTTPClient.Create(nil);
    try
      for i := 1 to 20 do
      begin
        try
          client.Get(url);
          Result := True;
          Exit;
        except
          Sleep(100);
        end;
      end;
    finally
      client.Free;
    end;
  except
    if Assigned(PHPProcess) then
    begin
      try PHPProcess.Terminate(0) except end;
      PHPProcess.Free;
      PHPProcess := nil;
    end;
    Exit(False);
  end;
end;

procedure StopLocalTestServer(var PHPProcess: TProcess);
begin
  if Assigned(PHPProcess) then
  begin
    try
      PHPProcess.Terminate(0);
    except
    end;
    PHPProcess.Free;
    PHPProcess := nil;
  end;
end;

end.
