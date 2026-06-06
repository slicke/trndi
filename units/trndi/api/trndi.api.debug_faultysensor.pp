unit trndi.api.debug_faultysensor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
  fpjson, jsonparser, dateutils, trndi.api.debug;

type
  DebugFaultySensorAPI = class(DebugAPI)
  protected
    function getSystemName: string; override;
  public
    function getReadings(min, maxNum: integer; extras: string; out res: string;
      noCache: boolean): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

function DebugFaultySensorAPI.getSystemName: string;
begin
  Result := 'Debug Faulty Sensor API';
end;

function DebugFaultySensorAPI.getReadings(min, maxNum: integer; extras: string;
  out res: string; {%H-}noCache: boolean): BGResults;
const
  // 2 -> 16 -> 5 -> 17 -> 3 -> 20 mmol/L in mg/dL
  FaultyPattern: array[0..5] of integer = (36, 288, 90, 306, 54, 360);
var
  i: integer;
  readingValue: integer;
  previousValue: integer;
  deltaValue: integer;
  newestTime: TDateTime;
  rssi, noise: MaybeInt;
begin
  res := '';
  rssi.exists := true;
  noise.exists := true;
  rssi.value := 55;
  noise.value := 22;

  newestTime := RecodeSecond(RecodeMilliSecond(Now, 0), 0);
  SetLength(Result, 11);
  for i := 0 to High(Result) do
  begin
    readingValue := FaultyPattern[i mod Length(FaultyPattern)];
    previousValue := FaultyPattern[(i + 1) mod Length(FaultyPattern)];
    deltaValue := readingValue - previousValue;

    Result[i].Init(mgdl, Self.systemName);
    Result[i].date := IncMinute(newestTime, -(i * 5));
    Result[i].update(readingValue, deltaValue);
    Result[i].trend := CalculateTrendFromDelta(deltaValue);
    Result[i].level := getLevel(Result[i].val);
    Result[i].updateEnv('Debug', rssi, noise);
  end;
end;

class function DebugFaultySensorAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  Result := inherited ParamLabel(LabelName);
  case LabelName of
  APLUser:
    Result := '(ignored for debug backend)';
  APLPass:
    Result := '(ignored for debug backend)';
  APLDesc:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend simulates a faulty sensor by rapidly jumping between extreme values. The generated repeating pattern is 2, 16, 5, 17, 3, 20 mmol/L.';
  APLDescHTML:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend simulates a <b>faulty sensor</b> by rapidly jumping between extreme values. The generated repeating pattern is <b>2, 16, 5, 17, 3, 20 mmol/L</b>.';
  APLCopyright:
    Result := 'Bjorn Lindh <github.com/slicke>';
  end;
end;

end.
