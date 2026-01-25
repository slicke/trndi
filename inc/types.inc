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
type

{$ifdef DARWIN}
WChar = System.WideChar; // < To avoid writing system.widechar for macOS all the time
{$else}
WChar = WideChar; // < Use system's native widechar
{$endif}

BGTrend = (TdDoubleUp = 0, TdSingleUp = 1, TdFortyFiveUp = 2, TdFlat =
  3, TdFortyFiveDown = 4,
  TdSingleDown = 5, TdDoubleDown = 6, TdNotComputable = 7, TdPlaceholder);
  //< Trends for delta

BGTrends = array[BGTrend] of string;

BGUnit = (mmol, mgdl);              //< Glucose units with def
BGUnitMeta = array[BGUnit] of string;
  //< Unique strings for each unit, when specialising etc
BGUnitVals = array[BGUnit] of single;    //< Unique data for units

BGVal = single;
  //< Glucose unit storage (signed, as its used for delta)
  //  BGValLevel  = (BGHIGH, BGLOW, BGNORMAL, BGLOST);
BGValLevel = (BGRangeHI, BGRangeLO, BGRange, BGHigh, BGLOW);
  // Range = depending on API

BGUnitArray = array[BGUnit, BGUnit] of single;

const
BG_TRENDS_STRING: array[BGTrend] of string = //< Standard API terms for trends
  ('DoubleUp', 'SingleUp', 'FortyFiveUp', 'Flat', 'FortyFiveDown',
  'SingleDown', 'DoubleDown', 'NOT COMPUTABLE', 'NOT FOUND');

BG_TRENDS: array[BGTrend] of string = //< Trends as text
  ('Rising Fast', 'Rising', 'Rising Slightly', 'Steady', 'Falling slightly',
  'Falling', 'Falling Fast', 'Unknown', 'Not found');

BG_TREND_ARROWS_UTF: array[BGTrend] of string =  //< Trends as UTF
  ('↑↑', '↑', '↗', '→', '↘', '↓', '↓↓', '?', 'X');

BG_TREND_ARROWS: array[BGTrend] of string = //< Trends as ASCII
  ('^^', '^', '/´', '->', '\_', '\/', '\\//', '?', 'X');

BG_UNIT_NAMES: array[BGUnit] of string = ('mmol/L', 'mg/dL');

BG_MSG_DEF: BGUnitMeta = ('%.1f mmol/L', '%0.f mg/dL'); // Text format for units
BG_MSG_SHORT: BGUnitMeta = ('%.1f', '%0.f');
BG_MSG_SIGNED: BGUnitMeta = ('%+%.1f mmol/L', '%+%0.f mg/dL'); // Text format for units
BG_MSG_SIG_SHORT: BGUnitMeta = ('%+%.1f', '%+%0.f');

BG_NO_VAL = -904; //< Magic value for placeholder BG

BG_CONVERTIONS: BGUnitArray = ((1, 0.0555555556), (18.0182, 1)); //< Conversions
