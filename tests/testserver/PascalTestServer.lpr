program PascalTestServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  pascal_testserver;

var
  baseURL: string;
begin
  if not StartPascalTestServer(baseURL) then
  begin
    Writeln('Failed to start Pascal test server');
    Halt(1);
  end;
  Writeln('Pascal test server listening on ', baseURL);
  Flush(Output);
  // Parent process is expected to terminate us when tests complete.
  while True do
    Sleep(1000);
end.
