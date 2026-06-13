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

unit trndi.api.debug_slow;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, trndi.types, trndi.api, trndi.api.debug;

type
  // Main class: identical to DebugAPI but simulates a slow response
DebugSlowAPI = class(DebugAPI)
protected
  function getSystemName: string; override;
public
  function getReadings(min, maxNum: integer; extras: string; out res: string;
    noCache: boolean): BGResults; override;
  class function ParamLabel(LabelName: APIParamLabel): string; override;
end;

implementation

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DebugSlowAPI.getSystemName: string;
begin
  result := 'Debug Slow API';
end;

{------------------------------------------------------------------------------
  Simulate a slow API response, then return the standard debug series.
------------------------------------------------------------------------------}
function DebugSlowAPI.getReadings(min, maxNum: integer; extras: string;
out res: string; noCache: boolean): BGResults;
begin
  Sleep(5000);
  Result := inherited getReadings(min, maxNum, extras, res, noCache);
end;

class function DebugSlowAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  case LabelName of
  APLDesc:
    Result := 'This is a special debug backend for testing purposes only. It simulates a slow API response (5 seconds delay). It does not connect to any real service.';
  APLDescHTML:
    Result := 'This is a special <b>debug backend</b> for testing purposes <u>only</u>. It simulates a <b>slow API response (5 seconds delay)</b>. It does <i>not</i> connect to any real service.';
  end;
end;

end.
