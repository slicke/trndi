unit trndiTestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  trndi.native, trndi.api, trndi.api.nightscout, trndi.api.dexcom, trndi.types, dialogs, dateutils;

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
begin
  // Can create
  api := NightScout.create('test','test','blah');

  // Test if the connect function runs
  AssertFalse('API Connect Fail', api.connect);
  AssertTrue('Time correct', api.getBasetime > IncHour(DateTimeToUnix(now), -2));
  api.Free;

  // Try with local fake server
  api := NightScout.create('http://localhost:8080','test22','');
  asserttrue('Connect to local NS', api.connect);
  asserttrue('Get readings', api.getCurrent(bg));
  api.free;
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

