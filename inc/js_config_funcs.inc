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
// JavaScript Configuration Functions - Modify app settings and behavior

// Set data freshness threshold in minutes
function JSvar_DATA_FRESHNESS_THRESHOLD_MINUTES(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  a0: variant;
begin
  ctx^^.ToVariant(argv^[0], a0);

  if varType(a0) <> varInteger then
  begin
    ExtError(uxdAuto, '(DATA_FRESHNESS_THRESHOLD_MINUTES) Mins must be integer');
    exit;
  end;

  if a0 < 6 then
    a0 := 6;
  DATA_FRESHNESS_THRESHOLD_MINUTES := a0;
end;

// Set clock interval and display settings
function JSvar_CLockInterval(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  a0, a1: variant;
begin
  ctx^^.ToVariant(argv^[0], a0);
  ctx^^.ToVariant(argv^[1], a1);

  if (varType(a0) <> varInteger) or (varType(a1) <> varInteger) then
  begin
    ExtError(uxdAuto, '(ClockInterval) secs must be integer');
    exit;
  end;

  clockInterval := a0;
  clockDisplay := a1;
end;

// Sets the max time to fetch in minutes, and the range to X readings
function JSTimeRange(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  vmins, vcount: variant;
begin
  if argc <> 2 then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vmins);
  ctx^^.ToVariant(argv^[1], vcount);

  if (not varType(vmins) = varInteger) or (not varType(vcount) = varInteger) then
  begin
    ExtError(uxdAuto, 'Mins and Count must be integers');
    exit;
  end;

  MAX_MIN := vmins;
  MAX_RESULT := vcount;
end;
