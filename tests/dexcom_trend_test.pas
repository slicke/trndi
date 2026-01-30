program DexcomTrendTest;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson, jsonparser, trndi.types;

procedure RunCase(const Name, TrendStr: string);
var
  code: integer;
  t: BGTrend;
begin
  Writeln('Case: ', Name);
  if TryStrToInt(TrendStr, code) then
  begin
    if (code >= Ord(Low(BGTrend))) and (code <= Ord(High(BGTrend))) then
      t := BGTrend(code)
    else
      t := TdPlaceholder;
    Writeln('  Numeric -> ', Ord(t), ' ', BG_TRENDS_STRING[t]);
  end
  else
  begin
    // textual lookup
    t := TdPlaceholder;
    for t in BGTrend do
      if BG_TRENDS_STRING[t] = TrendStr then
      begin
        Writeln('  Text -> ', Ord(t), ' ', BG_TRENDS_STRING[t]);
        Exit;
      end;
    Writeln('  Text -> Not found');
  end;
end;

begin
  RunCase('Numeric 0', '0');
  RunCase('Numeric 3', '3');
  RunCase('Numeric 7', '7');
  RunCase('Text Flat', 'Flat');
  RunCase('Unknown', 'Banana');
end.
