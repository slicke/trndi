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
// Include file with JS functions
{$ifdef TrndiExt}
uses 
mormot.lib.quickjs;



// Convert a color, eg for a ux call in JS, to TColor
function HTMLColorToTColor(const HTMLColor: string): TColor;

var 
  R, G, B: integer;
  ColorStr: string;
begin
  if (Length(HTMLColor) = 7) and (HTMLColor[1] = '#') then
  begin
    ColorStr := Copy(HTMLColor, 2, 6);
      // Ta bort '#'

    R := StrToInt('$' + Copy(ColorStr, 1, 2));
    G := StrToInt('$' + Copy(ColorStr, 3, 2));
    B := StrToInt('$' + Copy(ColorStr, 5, 2));

    Result := (B shl 16) or (G shl 8) or R;
  end
  else
    raise Exception.Create('Unsupported color');
end;



// Handle the active bg unit
function JSUnit(ctx: JSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw; cdecl;
cdecl;

var
  curr: rawbytestring;
  res: JSValue;
begin
  curr := UTF8Encode(BG_UNIT_NAMES[un]);
  res := ctx^.From(curr);
  result := res.raw;
end;

// UX modifications from JS ("uxProp()")
function JSUX(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
cdecl;

var 
  comp, what, val: string;
begin
  // Parse the parameters
  comp := ctx^^.ToUtf8(argv^[0]);
  what :=  ctx^^.ToUtf8(argv^[1]);
  val :=  ctx^^.ToUtf8(argv^[2]);

  if fBG.FindSubComponent(comp) <> nil then // Look for the control
    with fBG.FindSubComponent(comp) as TGraphicControl do
      with fBG.FindSubComponent(comp) as TGraphicControl do
        if Pos(what, 'color') > 0 then
          case what of
          'color':
            Color := HTMLColorToTColor(val);
          'font-color':
            font.Color := HTMLColorToTColor(val);
          else
            ShowMessage(Format(
              'An extension tried to access %s on %s. But the property does not exist.'
              , [what, comp]));
          end// Match the action

        else
        if Pos(what, 'font') > -1 then
          font.Name := val// Use the control
// Cast it as a graphic control
// Component
  ;

  Result := JS_UNDEFINED;
  TTrndiExtEngine.Instance.CallFunction('uxCallback', [comp, val]);
end;

function JSLevelColor(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
begin

  if not (argc in [3,6,10]) then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  if argc >= 3 then
  begin
    bg_color_ok := HTMLColorToTColor(ctx^^.ToUtf8(argv^[0]));
    bg_color_hi := HTMLColorToTColor(ctx^^.ToUtf8(argv^[1]));
    bg_color_lo := HTMLColorToTColor(ctx^^.ToUtf8(argv^[2]));
  end;
  if argc >= 6 then
  begin
    bg_color_ok_txt := HTMLColorToTColor(ctx^^.ToUtf8(argv^[3]));
    bg_color_hi_txt := HTMLColorToTColor(ctx^^.ToUtf8(argv^[4]));
    bg_color_lo_txt := HTMLColorToTColor(ctx^^.ToUtf8(argv^[5]));
  end;
  if argc = 10 then
  begin
    bg_rel_color_hi := HTMLColorToTColor(ctx^^.ToUtf8(argv^[5]));
    bg_rel_color_lo := HTMLColorToTColor(ctx^^.ToUtf8(argv^[6]));
    bg_rel_color_hi_txt := HTMLColorToTColor(ctx^^.ToUtf8(argv^[7]));
    bg_rel_color_lo_txt := HTMLColorToTColor(ctx^^.ToUtf8(argv^[8]));
  end;

end;

{$endif}
