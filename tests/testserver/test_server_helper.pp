unit test_server_helper;

{$mode objfpc}{$H+}

interface

function StartOrUseTestServer(out BaseURL: string): boolean;
procedure StopLocalTestServer;

implementation

uses
  SysUtils, fphttpclient, pascal_testserver;

var
  RunningNoticePrinted: Boolean = False;
  EmbeddedStarted: Boolean = False;
  EmbeddedBaseURL: string = '';

function StartOrUseTestServer(out BaseURL: string): boolean;
var
  env: string;
  client: TFPHTTPClient;
  i: Integer;
begin
  Result := False;
  BaseURL := '';

  // Honor an explicit skip for CI / sandboxed runs
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
    Exit(False);

  // Reuse an externally-launched server when the caller points at one
  env := GetEnvironmentVariable('TRNDI_TEST_SERVER_URL');
  if env <> '' then
  begin
    BaseURL := env;
    Exit(True);
  end;

  // Reuse the in-process server if it's already running
  if EmbeddedStarted then
  begin
    BaseURL := EmbeddedBaseURL;
    Exit(True);
  end;

  if not RunningNoticePrinted then
  begin
    Writeln('Starting embedded Pascal test server');
    RunningNoticePrinted := True;
  end;

  if not StartPascalTestServer(EmbeddedBaseURL) then
    Exit(False);

  // Poll /debug for readiness (up to ~3s)
  client := TFPHTTPClient.Create(nil);
  try
    for i := 1 to 30 do
    begin
      try
        client.Get(EmbeddedBaseURL + '/debug');
        EmbeddedStarted := True;
        BaseURL := EmbeddedBaseURL;
        Exit(True);
      except
        Sleep(100);
      end;
    end;
  finally
    client.Free;
  end;

  StopPascalTestServer;
  Result := False;
end;

procedure StopLocalTestServer;
begin
  // No-op: the embedded server runs for the process lifetime so successive
  // tests reuse it without paying re-bind cost. Process exit reclaims the port.
end;

end.
