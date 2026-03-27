unit trndi.api.debug_sensorexpiry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
  fpjson, jsonparser, dateutils, trndi.api.debug;

type
  DebugSensorExpiryAPI = class(DebugAPI)
  protected
    function getSystemName: string; override;
  public
    function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

function DebugSensorExpiryAPI.getSystemName: string;
begin
  Result := 'Debug Sensor Expiry API';
end;

function DebugSensorExpiryAPI.getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
var
  i: integer;
  readingValue: integer;
  rssi, noise: MaybeInt;
  newestTime: TDateTime;
  sensorText: string;
begin
  res := '';
  rssi.exists := true;
  noise.exists := true;
  rssi.value := 75;
  noise.value := 1;

  // Keep an explicit sensor suffix so the top-center sensor badge can be
  // tested even when real backends do not expose sensor metadata.
  sensorText := 'Debug (sensor 3d 6h left)';

  newestTime := RecodeSecond(RecodeMilliSecond(Now, 0), 0);
  SetLength(Result, 11);
  for i := 0 to High(Result) do
  begin
    readingValue := 118 - i;
    Result[i].Init(mgdl, Self.systemName);
    Result[i].date := IncMinute(newestTime, -(i * 5));
    Result[i].update(readingValue, -1);
    Result[i].trend := CalculateTrendFromDelta(-1);
    Result[i].level := getLevel(Result[i].val);
    Result[i].updateEnv(sensorText, rssi, noise);
  end;
end;

class function DebugSensorExpiryAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  Result := inherited ParamLabel(LabelName);
  case LabelName of
  APLUser:
    Result := '(ignored for debug backend)';
  APLPass:
    Result := '(ignored for debug backend)';
  APLDesc:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend always emits synthetic sensor metadata so the sensor expiry badge can be tested without backend support.';
  APLDescHTML:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend always emits synthetic <b>sensor metadata</b> so the sensor expiry badge can be tested without backend support.';
  APLCopyright:
    Result := 'Bjorn Lindh <github.com/slicke>';
  end;
end;

end.
