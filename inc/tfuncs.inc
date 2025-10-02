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
mormot.lib.quickjs, variants;


function JSActiveUser(ctx: JSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw; cdecl;
var
  curr: rawbytestring;
  res: JSValue;
begin
  if username <> '' then begin
    curr := username;
    res := ctx^.From(curr);
    result := res.raw;
  end else
    // Return a JS boolean false when no user has been loaded
    result := JS_FALSE;
end;

function JSActiveUserNick(ctx: JSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw; cdecl;
var
  curr: rawbytestring;
  res: JSValue;
begin
  if fBG.multi then
  begin
    curr := fBG.multinick;
    res := ctx^.From(curr);
    result := res.raw;
  end
  else
  begin
    // Return a real JS boolean false when not in multi-user mode
    result := JS_FALSE;
  end;
end;


// Convert a color, eg for a ux call in JS, to TColor
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



// Handle the active bg unit
function JSUnit(ctx: JSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw; cdecl;

var
  curr: rawbytestring;
  res: JSValue;
begin
  curr := UTF8Encode(BG_UNIT_NAMES[un]);
  res := ctx^.From(curr);
  result := res.raw;
end;

// Handle the active language
function JSLocale(ctx: JSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw; cdecl;

var
  curr: rawbytestring;
  res: JSValue;
begin
  curr := applocale; // UTF8Encode(BG_UNIT_NAMES[un]);
  res := ctx^.From(curr);
  result := res.raw;
end;

// Play a sound file
function JSPlay(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  filename: string;
begin
  filename := ctx^^.ToUtf8(argv^[0]);
  TrndiNative.PlaySound(filename);
end;

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


// TTS
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
    ExtError(uxdAuto, 'Mins and Count ust be integers');
    exit;
  end;

 MAX_MIN := vmins;
 MAX_RESULT := vcount;
end;

// Set the UX colors
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

// Handle badge size, mainly on Windows
function JSBadge(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
 vbw, vbf: Variant;
begin
  if not (argc in [1,2]) then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vbw);

  if not varType(vbw) = varDecimal then begin
    ExtError(uxdAuto, 'Badge size must be percentage: 0.1-1.0');
    exit;
  end;
  badge_width := vbw;

  if argc <> 2 then
  begin
     ctx^^.ToVariant(argv^[1], vbf);
    if not VarType(vbf) = varInteger then begin
      ExtError(uxdAuto, 'Badge font size must be an integer!');
      exit;
      end;
      badge_font := vbf;
  end;

end;

// Set the dot size
function JSDotSize(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
 vds: Variant;
begin
  if argc <> 1 then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vds);

  if not varType(vds) = varInteger then begin
    ExtError(uxdAuto, 'Dot size must be an integer');
    exit;
  end;
  dotscale := vds;

end;

// Set the dot adjustment
function JSDotAdjust(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
 vda: Variant;
begin
  if argc <> 1 then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vda);

  if not varType(vda) = varSingle then begin
    ExtError(uxdAuto, 'Dot size must be a floating number');
    exit;
  end;
;
  DOT_ADJUST := vda;

end;

{$endif}


