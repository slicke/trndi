unit trndi.api.debug_lowsoon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
  fpjson, jsonparser, dateutils, trndi.api.debug;

type
  DebugLowSoonAPI = class(DebugAPI)
  protected
    function getSystemName: string; override;
  public
    function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

function DebugLowSoonAPI.getSystemName: string;
begin
  Result := 'Debug Low Soon API';
end;

function DebugLowSoonAPI.getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
var
  i: integer;
  readingValue: integer;
  rssi, noise: MaybeInt;
  newestTime: TDateTime;
begin
  res := '';
  rssi.exists := true;
  noise.exists := true;
  rssi.value := 80;
  noise.value := 2;

  // Keep the current reading just above low and fall 1 mg/dL every 20 seconds.
  // With the built-in prediction limiter, this makes the 7th prediction land on
  // the low threshold within the existing "soon" warning window.
  newestTime := RecodeMilliSecond(Now, 0);
  SetLength(Result, 11);
  for i := 0 to High(Result) do
  begin
    readingValue := 67 + i;
    Result[i].Init(mgdl, self.systemname);
    Result[i].date := IncSecond(newestTime, -(i * 20));
    Result[i].update(readingValue, -1);
    Result[i].trend := CalculateTrendFromDelta(-1);
    Result[i].level := getLevel(Result[i].val);
    Result[i].updateEnv('Debug', rssi, noise);
  end;
end;

class function DebugLowSoonAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  Result := inherited ParamLabel(LabelName);
  case LabelName of
  APLUser:
    Result := '(ignored for debug backend)';
  APLPass:
    Result := '(ignored for debug backend)';
  APLDesc:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend generates a short falling sequence where the 7th prediction crosses low so the prediction warning shows the "Low Predicted soon!" path.';
  APLDescHTML:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend generates a short falling sequence where the <b>7th prediction</b> crosses low so the prediction warning shows the <b>Low Predicted soon!</b> path.';
  APLCopyright:
    Result := 'Björn Lindh <github.com/slicke>';
  end;
end;

end.