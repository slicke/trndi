(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)

unit trndi.api.debug_sensorexpiry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
  fpjson, jsonparser, dateutils, trndi.api.debug;

type
  DebugSensorExpiryAPI = class(DebugAPI)
  protected
    FFetch: integer; /// Fetches so far; indexes SENSOR_LIFE_SEQUENCE
    function getSystemName: string; override;
  public
    function getReadings(min, maxNum: integer; extras: string; out res: string;
      noCache: boolean): BGResults; override;
    function getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean; override;
    class function ParamLabel(LabelName: APIParamLabel): string; override;
  end;

implementation

const
  // Remaining sensor life handed out one entry per fetch, so a few refreshes
  // walk the whole expiry ladder: outside it, every step in turn, then a sensor
  // change that re-arms it. Wraps, so the run can be repeated without a restart.
  SENSOR_LIFE_SEQUENCE: array[0..7] of integer = (26, 24, 20, 8, 4, 2, 1, 168);

function DebugSensorExpiryAPI.getSystemName: string;
begin
  Result := 'Debug Sensor Expiry API';
end;

function DebugSensorExpiryAPI.getReadings(min, maxNum: integer; extras: string;
  out res: string; {%H-}noCache: boolean): BGResults;
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

  Inc(FFetch);
end;

{------------------------------------------------------------------------------
  Report a falling sensor life so the expiry notifications can be exercised
  without waiting out a real session. Like the real backends this answers from
  the last fetch rather than making a request of its own.
 ------------------------------------------------------------------------------}
function DebugSensorExpiryAPI.getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean;
begin
  clearDeviceStatus(AStatus);
  if FFetch <= 0 then
    Exit(false); // Nothing fetched yet, so nothing to report on

  AStatus.sensorDurationHours :=
    SENSOR_LIFE_SEQUENCE[(FFetch - 1) mod Length(SENSOR_LIFE_SEQUENCE)];
  AStatus.sensorState := 'NO_ERROR_MESSAGE';
  Result := true;
end;

class function DebugSensorExpiryAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  // User/pass/copyright inherit DebugAPI's shared defaults; only the
  // backend-specific description is customised here.
  Result := inherited ParamLabel(LabelName);
  case LabelName of
  APLDesc:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend always emits synthetic sensor metadata so the sensor expiry badge can be tested without backend support. Its reported sensor life also falls one step per refresh, walking the expiry notifications from 24 hours down to 1 and then through a sensor change.';
  APLDescHTML:
    Result := Result + sLineBreak + sLineBreak +
      'This debug backend always emits synthetic <b>sensor metadata</b> so the sensor expiry badge can be tested without backend support. Its reported <b>sensor life</b> also falls one step per refresh, walking the expiry notifications from 24 hours down to 1 and then through a sensor change.';
  end;
end;

end.
