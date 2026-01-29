
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

unit trndi.api.debug_firstXmissing;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
fpjson, jsonparser, dateutils, trndi.api.debug;

type
  // Main class
DebugFirstXMissingAPI = class(DebugAPI)
protected
  missing: integer;
  function getSystemName: string; override;
public
  constructor Create(user, pass: string); override;
  function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
    override;
  class function ParamLabel(LabelName: APIParamLabel): string; override;

end;

implementation

{------------------------------------------------------------------------------
  Constructor
------------------------------------------------------------------------------}
constructor DebugFirstXMissingAPI.Create(user, pass: string);
begin
  missing := StrToIntDef(user, 4);
  inherited;
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DebugFirstXMissingAPI.getSystemName: string;
begin
  result := 'Debug First X Missing API';
end;

{------------------------------------------------------------------------------
  Generate fake readings over the last 50 minutes at 5-minute intervals,
  with the First X readings missing
------------------------------------------------------------------------------}
function DebugFirstXMissingAPI.getReadings(min, maxNum: integer; extras: string;
out res: string): BGResults;
var
  i: integer;
begin
  result := inherited getReadings(min, maxNum, extras, res);
  // Clamp requested missing count to available results and ensure non-negative
  if missing <= 0 then
    missing := 0
  else if missing > Length(Result) then
    missing := Length(Result);

  // Mark the first N readings as missing (clear numeric values)
  for i := Low(Result) to missing-1 do
    Result[i].Clear;

  // Short debug trace to help when testing the provider
  if missing < Length(Result) then
    LogMessage(Format('DebugFirstXMissing: Cleared %d readings; newest remaining at %s', [missing, DateTimeToStr(Result[missing].date)]))
  else
    LogMessage(Format('DebugFirstXMissing: Cleared all %d readings', [missing]));
end;

class function DebugFirstXMissingAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  if LabelName = APLUser then
    Result := 'Readings to remove, from the most recent';
  if LabelName = APLDesc then
    Result := result + sLineBreak + sLineBreak + 'You can enter any amount of readings, in the username field. That amount will be marked as missing (from most recent to oldest)';
  if LabelName = APLDescHTML then
    Result := result + sLineBreak + sLineBreak + 'You can enter <b>any amount</b> of readings, in the <u>username</u> field. That amount will be marked as <i>missing</i> (from most recent to oldest)';
  if LabelName = APLCopyright then
    Result := 'Björn Lindh <github.com/slicke>';
end;

end.
