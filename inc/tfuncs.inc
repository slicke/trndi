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
// Include file with utility functions and JS extension functions
// This file now includes sub-files with categorized JS functions for better organization

{$ifdef TrndiExt}
uses 
mormot.lib.quickjs, variants;

// Utility function: Convert HTML color (#RRGGBB) to TColor
function HTMLColorToTColor(const HTMLColor: string): TColor;
var 
  R, G, B: integer;
  ColorStr: string;
begin
  if (Length(HTMLColor) = 7) and (HTMLColor[1] = '#') then
  begin
    ColorStr := Copy(HTMLColor, 2, 6); // Remove '#'

    R := StrToInt('$' + Copy(ColorStr, 1, 2));
    G := StrToInt('$' + Copy(ColorStr, 3, 2));
    B := StrToInt('$' + Copy(ColorStr, 5, 2));

    Result := (B shl 16) or (G shl 8) or R;
  end
  else
    raise Exception.Create('Unsupported color');
end;

// Include categorized JS function files
{$I js_api_funcs.inc}       // JavaScript API bridge functions (expose app data)
{$I js_ux_funcs.inc}         // JavaScript UX functions (modify UI)
{$I js_config_funcs.inc}     // JavaScript configuration functions (settings)
{$I js_system_funcs.inc}     // JavaScript system functions (sound, TTS)

{$endif}

