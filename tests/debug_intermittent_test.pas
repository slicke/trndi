program debug_intermittent_test;

{$mode ObjFPC}{$H+}

uses
  SysUtils, trndi.api.debug_intermittentmissing, trndi.api.debug, trndi.types;

procedure DumpReadingsAndMissing(r: BGResults);
var
  i: Integer;
begin
  for i := 0 to Length(r)-1 do
  begin
    if r[i].exists then
      Write(Format('%d ', [r[i].val]))
    else
      Write('X ');
  end;
  Writeln;
end;

var
  api: DebugIntermittentMissingAPI;
  resStr: string;
  r: BGResults;
  i: Integer;
begin
  try
    api := DebugIntermittentMissingAPI.Create('2-4', '');
    try
      for i := 1 to 10 do
      begin
        r := api.getReadings(0, 11, '', resStr);
        DumpReadingsAndMissing(r);
      end;
    finally
      api.Free;
    end;
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.
