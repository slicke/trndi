program DexcomTimeTest;

{$mode objfpc}{$H+}

uses
  SysUtils, dateutils, trndi.api.dexcom_time;

procedure RunCase(const Name, S: string);
var
  dt: TDateTime;
  ok: boolean;
begin
  ok := ParseDexcomTime(S, dt);
  if ok then
    Writeln(Format('%s: OK -> %s', [Name, DateTimeToStr(dt)]))
  else
    Writeln(Format('%s: FAIL', [Name]));
end;

begin
  RunCase('XML', '<SystemTime>2021-01-12T12:32:04</SystemTime>');
  RunCase('DateMs', '/Date(1610464324000)/');
  RunCase('JSONDate', '{"ServerTime":"/Date(1610464324000)/"}');
  RunCase('JSONNum', '{"ServerTime":1610464324000}');
  RunCase('ISO', '2021-01-12T12:32:04Z');
end.
