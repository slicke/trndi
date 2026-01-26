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
// JavaScript UX Functions - Modify UI appearance and display

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

//Post a notification
function JSAttention(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  s: string;
begin
  s := ctx^^.ToUtf8(argv^[0]);

  native.attention('Trndi - Extension Message', s);
end;

// Display an HTML message dialog
function JSHTMLMsg(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  vbs: variant; //Variant box size
begin
  if argc <> 5 then
    ExtError(uxdAuto, '[htmlMsg] A dialog box could not be displayed, as the parameter count was wrong; got' + IntToStr(argc) + ', expected 5');

  ctx^^.ToVariant(argv^[4], vbs);

  if not varType(vbs) = varSingle then
  begin
    ExtError(uxdAuto, 'Box Size must be a float or integer');
    exit;
  end;

 // Title, Desc, html, size
  ExtMessage(uxdAuto, ctx^^.ToUtf8(argv^[0]), ctx^^.ToUtf8(argv^[1]), ctx^^.ToUtf8(argv^[2]), ctx^^.ToUtf8(argv^[3]), true,
    $00AA6004, $00FDD8AA,  [mbOK], uxmtOK, vbs);
end;

// Display an HTML message dialog
function JSHTMLDlg(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  vbs: variant; //Variant box size
begin
  if argc <> 3 then
    ExtError(uxdAuto, '[htmlDlg] A dialog box could not be displayed, as the parameter count was wrong; got' + IntToStr(argc) + ', expected 3');

  ctx^^.ToVariant(argv^[2], vbs);

  if not varType(vbs) = varSingle then
  begin
    ExtError(uxdAuto, 'Box Size must be a float or integer');
    exit;
  end;

 // Title, Desc, html, size
  ExtMsg(uxdAuto, ctx^^.ToUtf8(argv^[0]), ctx^^.ToUtf8(argv^[1]), [mbOK], uxmtOk, vbs);
end;

function JSHTMLYesNo(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  vbs: variant; //Variant box size
begin
  if argc <> 3 then
    ExtError(uxdAuto, '[htmlYesNo] A dialog box could not be displayed, as the parameter count was wrong; got' + IntToStr(argc) + ', expected 3');

  ctx^^.ToVariant(argv^[2], vbs);

  if not varType(vbs) = varSingle then
  begin
    ExtError(uxdAuto, 'Box Size must be a float or integer');
    exit;
  end;

 // Title, Desc, html, size
  if ExtMsg(uxdAuto, ctx^^.ToUtf8(argv^[0]), ctx^^.ToUtf8(argv^[1]), [mbYes, mbNo], uxmtOk, vbs) = mrYes then
    Result := JS_TRUE
  else
    Result := JS_FALSE;
end;

// Set the UX colors for different BG levels
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
  vbw, vbf: variant;
begin
  if not (argc in [1,2]) then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vbw);

  if not varType(vbw) = varDecimal then
  begin
    ExtError(uxdAuto, 'Badge size must be percentage: 0.1-1.0');
    exit;
  end;
  badge_width := vbw;

  if argc <> 2 then
  begin
    ctx^^.ToVariant(argv^[1], vbf);
    if not VarType(vbf) = varInteger then
    begin
      ExtError(uxdAuto, 'Badge font size must be an integer!');
      exit;
    end;
    badge_font := vbf;
  end;
end;

// Set the dot size
function JSDotSize(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  vds: variant;
begin
  if argc <> 1 then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vds);

  if not varType(vds) = varInteger then
  begin
    ExtError(uxdAuto, 'Dot size must be an integer');
    exit;
  end;
  dotscale := vds;
end;

// Set the dot adjustment
function JSDotAdjust(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  vda: variant;
begin
  if argc <> 1 then
  begin
    Result := JS_FALSE;
    Exit;
  end;

  ctx^^.ToVariant(argv^[0], vda);

  if not varType(vda) = varSingle then
  begin
    ExtError(uxdAuto, 'Dot size must be a floating number');
    exit;
  end;

  DOT_ADJUST := vda;
end;
