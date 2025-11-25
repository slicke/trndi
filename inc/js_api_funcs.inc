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
// JavaScript API Bridge Functions - Expose app data to extensions

// Return the active username
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

// Return the active user nickname (multi-user mode)
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
  curr := applocale;
  res := ctx^.From(curr);
  result := res.raw;
end;

// Predict future blood glucose readings
function JSPredictReadings(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw;
var
  numPredictions: Variant;
  predictions: BGResults;
  success: boolean;
  i: integer;
begin
  // Default to predicting 3 readings if no parameter provided
  numPredictions := 3;
  
  if argc > 0 then
  begin
    ctx^^.ToVariant(argv^[0], numPredictions);
    if not (VarType(numPredictions) = varInteger) then
    begin
      ExtError(uxdAuto, 'Number of predictions must be an integer');
      Result := JS_FALSE;
      Exit;
    end;
  end;
  
  // Call the prediction function
  success := api.predictReadings(numPredictions, predictions);
  
  if not success then
  begin
    // Return empty array if prediction failed
    Result := TTrndiExtEngine.Instance.CreateJSArray([]);
    Exit;
  end;
  
  // Build array of prediction arrays
  // Each prediction is: [value_in_current_unit, mgdl, mmol, timestamp]
  case Length(predictions) of
    0: Result := TTrndiExtEngine.Instance.CreateJSArray([]);
    1: Result := TTrndiExtEngine.Instance.CreateJSArray([
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[0].convert(un),
           Round(predictions[0].convert(BGUnit.mgdl)),
           predictions[0].convert(BGUnit.mmol),
           predictions[0].date
         ])
       ]);
    2: Result := TTrndiExtEngine.Instance.CreateJSArray([
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[0].convert(un),
           Round(predictions[0].convert(BGUnit.mgdl)),
           predictions[0].convert(BGUnit.mmol),
           predictions[0].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[1].convert(un),
           Round(predictions[1].convert(BGUnit.mgdl)),
           predictions[1].convert(BGUnit.mmol),
           predictions[1].date
         ])
       ]);
    3: Result := TTrndiExtEngine.Instance.CreateJSArray([
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[0].convert(un),
           Round(predictions[0].convert(BGUnit.mgdl)),
           predictions[0].convert(BGUnit.mmol),
           predictions[0].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[1].convert(un),
           Round(predictions[1].convert(BGUnit.mgdl)),
           predictions[1].convert(BGUnit.mmol),
           predictions[1].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[2].convert(un),
           Round(predictions[2].convert(BGUnit.mgdl)),
           predictions[2].convert(BGUnit.mmol),
           predictions[2].date
         ])
       ]);
    4: Result := TTrndiExtEngine.Instance.CreateJSArray([
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[0].convert(un),
           Round(predictions[0].convert(BGUnit.mgdl)),
           predictions[0].convert(BGUnit.mmol),
           predictions[0].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[1].convert(un),
           Round(predictions[1].convert(BGUnit.mgdl)),
           predictions[1].convert(BGUnit.mmol),
           predictions[1].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[2].convert(un),
           Round(predictions[2].convert(BGUnit.mgdl)),
           predictions[2].convert(BGUnit.mmol),
           predictions[2].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[3].convert(un),
           Round(predictions[3].convert(BGUnit.mgdl)),
           predictions[3].convert(BGUnit.mmol),
           predictions[3].date
         ])
       ]);
    5: Result := TTrndiExtEngine.Instance.CreateJSArray([
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[0].convert(un),
           Round(predictions[0].convert(BGUnit.mgdl)),
           predictions[0].convert(BGUnit.mmol),
           predictions[0].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[1].convert(un),
           Round(predictions[1].convert(BGUnit.mgdl)),
           predictions[1].convert(BGUnit.mmol),
           predictions[1].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[2].convert(un),
           Round(predictions[2].convert(BGUnit.mgdl)),
           predictions[2].convert(BGUnit.mmol),
           predictions[2].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[3].convert(un),
           Round(predictions[3].convert(BGUnit.mgdl)),
           predictions[3].convert(BGUnit.mmol),
           predictions[3].date
         ]),
         TTrndiExtEngine.Instance.CreateJSArray([
           predictions[4].convert(un),
           Round(predictions[4].convert(BGUnit.mgdl)),
           predictions[4].convert(BGUnit.mmol),
           predictions[4].date
         ])
       ]);
  else
    // For more than 5 predictions, return simpler format: just the values in current unit
    Result := TTrndiExtEngine.Instance.CreateJSArray([]);
  end;
end;
