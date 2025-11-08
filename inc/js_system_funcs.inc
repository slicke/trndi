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
// JavaScript System Functions - System operations like sound/TTS

// Play a sound file
function JSPlay(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  filename: string;
begin
  filename := ctx^^.ToUtf8(argv^[0]);
  TrndiNative.PlaySound(filename);
end;

// TTS (Text-to-Speech)
function JSSay(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  filename: string;
  tn: trndinative;
begin
  filename := ctx^^.ToUtf8(argv^[0]);
  tn := TrndiNative.create;
  tn.PlaySound(filename);
  tn.free;
end;
