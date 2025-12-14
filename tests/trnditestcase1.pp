unit trndiTestCase1;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry,
trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.types, dialogs, dateutils,
process;

type

TAPITester = class(TTestCase)
protected
  procedure SetUp; override;
  procedure TearDown; override;
published
  procedure TestDexcom;
  procedure TestNightScout;
end;

implementation

procedure TAPITester.TestDexcom;
var
  api: TrndiAPI;
begin
  // Can create
  api := Dexcom.create('test','test','usa');
  // Test if the connect function runs
  AssertFalse('API Connect Fail', api.connect);
  asserttrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  api.Free;
end;

procedure TAPITester.TestNightscout;
var
  api: TrndiAPI;
  bg: BGreading;
  PHPProcess: TProcess;
begin
  // Start PHP server
  PHPProcess := TProcess.Create(nil);
  try
    PHPProcess.Executable :={$ifdef Windows}'c:\tools\php84\'+{$endif}'php'; // Choco path for win
    PHPProcess.Parameters.Add('-S');
    PHPProcess.Parameters.Add('localhost:8080');
      PHPProcess.Parameters.Add('-t');
      // Use OS-agnostic path joining so tests work on both Windows and Unix-like systems
      PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'testserver');
      // Use the router so the built-in server forwards API paths to index.php
      PHPProcess.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'router.php');
    PHPProcess.Options := [poNoConsole];  // Run without console window
    PHPProcess.Execute;

    // Wait a bit for the server to start
    Sleep(1000);  // Give it a second to start up

    try
      // Your existing test code
      api := NightScout.create('test','test','blah');
      AssertFalse('API Connect Fail', api.connect);
      AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
      api.Free;

      api := NightScout.create('http://localhost:8080','test22','');
      asserttrue('Connect to local NS', api.connect);
      asserttrue('Get readings', api.getCurrent(bg));
      api.free;

    finally
      // Terminate PHP server
      PHPProcess.Terminate(0);
    end;

  finally
    PHPProcess.Free;
  end;
end;


procedure TAPITester.SetUp;
begin

end;

procedure TAPITester.TearDown;
begin

end;

initialization

RegisterTest(TAPITester);
end.
