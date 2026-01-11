
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

unit trndi.api.debug_edge;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
fpjson, jsonparser, dateutils;

type
  // Main class
DebugEdgeAPI = class(TrndiAPI)
protected
public
  constructor Create(user, pass, extra: string); override;
  function connect: boolean; override;
  function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
    override;
private

published
  property remote: string read baseUrl;

protected
    {** Get the value which represents the maximum reading for the backend
  }
  function getLimitHigh: integer; override;
  
  {** Get the value which represents the minimum reading for the backend
  }
  function getLimitLow: integer; override;
end;

implementation

{------------------------------------------------------------------------------
  Constructor
------------------------------------------------------------------------------}
constructor DebugEdgeAPI.Create(user, pass, extra: string);
begin
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := user;
  inherited;
end;

{------------------------------------------------------------------------------
  Connect: set extreme thresholds and zero time diff for edge-case testing
------------------------------------------------------------------------------}
function DebugEdgeAPI.Connect: boolean;
begin
  cgmHi := 160;
  cgmLo := 60;
  cgmRangeHi := 140;
  cgmRangeLo := 90;

  TimeDiff := 0;

  Result := true;
end;

{------------------------------------------------------------------------------
  Generate alternating high/low extreme readings to exercise edge conditions
------------------------------------------------------------------------------}
function DebugEdgeAPI.getReadings(min, maxNum: integer; extras: string;
out res: string): BGResults;
var
  i: integer;
  val, diff: integer;
  dbase: TDateTime;
  hi: boolean;
  nodata: maybeint;
begin
  nodata.exists := false;
  res := '';
  SetLength(Result, 11);
  dbase := IncMinute(now, 5);
  for i := 0 to 10 do
  begin
    hi := MinuteOf(dbase) mod 2 = 0;
    dbase := IncMinute(dbase, -5);
    Result[i].Init(mgdl);
    Result[i].date := dbase;
    if hi then
    begin
      Result[i].update(400, cgmHi);
      Result[i].trend := BGTrend.TdDoubleUp;
      Result[i].level := BGHigh;
    end
    else
    begin
      Result[i].update(40, -cgmHi);
      Result[i].trend := BGTrend.TdDoubleDown;
      Result[i].level := BGLOW;
    end;
    Result[i].updateEnv('DebugEdge', nodata, nodata);
  end;

end;

function DebugEdgeAPI.getLimitHigh: integer;
begin
  Result := 400; // DebugEdge maximum high limit
end;  

function DebugEdgeAPI.getLimitLow: integer;
begin
  Result := 40; // DebugEdge minimum low limit
end;

end.
