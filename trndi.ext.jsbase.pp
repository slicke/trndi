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
function JSDoLog(ctx: PJSContext; this_val: JSValueRaw; argc: Integer; argv: PJSValueRaw): JSValueRaw; cdecl;
var
  i: Integer;
  LogMessage: RawUtf8;
begin
  LogMessage := '';
  for i := 0 to argc - 1 do
    LogMessage := LogMessage + JS_ToCString(ctx^, argv[i]) + ' ';
  FOutput := Trim(LogMessage);
  Result := JS_UNDEFINED;
end;


