(*
 * Trndi
 * Debug Intermittent Missing Backend
 *)

unit trndi.api.debug_intermittentmissing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
  fpjson, jsonparser, dateutils, trndi.api.debug, trndi.log;

type
  // Main class
  DebugIntermittentMissingAPI = class(DebugAPI)
  protected
    MinMissing: integer;
    MaxMissing: integer;
    function getSystemName: string; override;
  public
    constructor Create(user, pass: string); override;
    function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

{------------------------------------------------------------------------------
  Constructor - parse user parameter as "N" or "min-max" (e.g. "2-4")
------------------------------------------------------------------------------}
constructor DebugIntermittentMissingAPI.Create(user, pass: string);
var
  p: integer;
  s: string;
begin
  inherited Create(user, pass);
  // Defaults
  MinMissing := 2;
  MaxMissing := 4;
  s := Trim(user);
  if s = '' then exit;
  // user can be single number or range like "2-4"
  p := Pos('-', s);
  if p > 0 then
  begin
    MinMissing := StrToIntDef(Trim(Copy(s, 1, p - 1)), MinMissing);
    MaxMissing := StrToIntDef(Trim(Copy(s, p + 1, MaxInt)), MaxMissing);
  end
  else
  begin
    MinMissing := StrToIntDef(s, MinMissing);
    MaxMissing := MinMissing;
  end;
  if MinMissing < 0 then MinMissing := 0;
  if MaxMissing < MinMissing then MaxMissing := MinMissing;
  Randomize;
end;

function DebugIntermittentMissingAPI.getSystemName: string;
begin
  result := 'Debug Intermittent Missing API';
end;

// getReadings - randomly clear between MinMissing and MaxMissing readings
// among the most recent N readings (default N = 10) to simulate intermittent
// gaps and try to break UI/mapping logic.
function DebugIntermittentMissingAPI.getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
var
  i, j, toClear, rangeLen, rng: integer;
  idxs: array of integer;
  picked: TList;
  idx: integer;
  logmsg: string;
begin
  result := inherited getReadings(min, maxNum, extras, res);
  if Length(result) < 1 then
    Exit;
  // Only consider the newest N values so we don't clear very old data
  if Length(result) < 10 then rangeLen := Length(result) else rangeLen := 10;
  // Calculate a safe random range and clamp
  rng := MaxMissing - MinMissing + 1;
  if rng < 1 then rng := 1;
  toClear := MinMissing + Random(rng);
  if toClear > rangeLen then toClear := rangeLen;

  picked := TList.Create;
  try
    // choose toClear unique indices in [0..rangeLen-1]
    while picked.Count < toClear do
    begin
      idx := Random(rangeLen); // 0-based
      if picked.IndexOf(Pointer(idx)) = -1 then
        picked.Add(Pointer(idx));
    end;

    logmsg := Format('DebugIntermittentMissing: Clearing %d readings at indices: ', [picked.Count]);
    for i := 0 to picked.Count - 1 do
    begin
      idx := integer(picked.Items[i]);
      // Convert to array index (newest at Result[0]) -> keep same mapping
      // Clear selected reading
      result[idx].Clear;
      logmsg := logmsg + Format('%d(%s) ', [idx, DateTimeToStr(result[idx].date)]);
    end;

    LogMessageToFile(logmsg);
  finally
    picked.Free;
  end;
end;

class function DebugIntermittentMissingAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  if LabelName = APLUser then
    Result := 'Number of missing readings (N) or range (min-max), e.g. "2" or "2-4"';
  if LabelName = APLDesc then
    Result := result + sLineBreak + sLineBreak + 'This debug backend randomly clears between the specified number of newest readings (default 2-4), selecting them from the most recent 10 readings. Use to reproduce intermittent/misaligned missing values.';
  if LabelName = APLDescHTML then
    Result := result + sLineBreak + sLineBreak + 'Use <b>"2"</b> or <b>"2-4"</b> in the username field to control how many readings are cleared. Readings are chosen randomly from the most recent 10 values.';
  if LabelName = APLCopyright then
    Result := 'Björn Lindh <github.com/slicke>';
end;

end.
