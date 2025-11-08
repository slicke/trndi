(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)
// JavaScript Configuration Functions - Modify app settings and behavior

// Set data freshness threshold in minutes
function JSvar_DATA_FRESHNESS_THRESHOLD_MINUTES(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  a0: Variant;
begin
  ctx^^.ToVariant(argv^[0], a0);

  if varType(a0) <> varInteger then begin
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
  a0, a1: Variant;
begin
  ctx^^.ToVariant(argv^[0], a0);
  ctx^^.ToVariant(argv^[1], a1);

  if (varType(a0) <> varInteger) or (varType(a1) <> varInteger) then begin
    ExtError(uxdAuto, '(ClockInterval) secs must be integer');
    exit;
  end;

  clockInterval := a0;
  clockDisplay := a1;
end;

// Sets the max time to fetch in minutes, and the range to X readings
function JSTimeRange(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
 vmins, vcount: Variant;
begin
  if argc <> 2 then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vmins);
  ctx^^.ToVariant(argv^[1], vcount);

  if (not varType(vmins) = varInteger) or (not varType(vcount) = varInteger) then begin
    ExtError(uxdAuto, 'Mins and Count must be integers');
    exit;
  end;

 MAX_MIN := vmins;
 MAX_RESULT := vcount;
end;
