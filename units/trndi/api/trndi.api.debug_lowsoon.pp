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
const
  // Linear fall from ~10 mmol/L (180 mg/dL) down to ~3.7 mmol/L (67 mg/dL)
  // across the 11 generated readings (i=10 oldest, i=0 newest).
  START_MGDL = 180;
  END_MGDL   = 67;
var
  i: integer;
  readingValue, nextValue, readingDelta: integer;
  rssi, noise: MaybeInt;
  newestTime: TDateTime;
begin
  res := '';
  rssi.exists := true;
  noise.exists := true;
  rssi.value := 80;
  noise.value := 2;

  // Readings are spaced at the main window's 5-minute slot interval so they
  // render as distinct dots, and the steep falling slope drives the prediction
  // path toward low within the existing "soon" warning window.
  newestTime := RecodeMilliSecond(Now, 0);
  SetLength(Result, 11);
  for i := 0 to High(Result) do
  begin
    readingValue := END_MGDL + (i * (START_MGDL - END_MGDL)) div High(Result);
    if i < High(Result) then
    begin
      nextValue := END_MGDL + ((i + 1) * (START_MGDL - END_MGDL)) div High(Result);
      readingDelta := readingValue - nextValue;
    end
    else
      readingDelta := -((START_MGDL - END_MGDL) div High(Result));

    Result[i].Init(mgdl, self.systemname);
    Result[i].date := IncMinute(newestTime, -(i * 5));
    Result[i].update(readingValue, readingDelta);
    Result[i].trend := CalculateTrendFromDelta(readingDelta);
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