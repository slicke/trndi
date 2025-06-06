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
type
BGTrend = (TdDoubleUp = 0, TdSingleUp = 1, TdFortyFiveUp = 2, TdFlat = 3, TdFortyFiveDown = 4,
  TdSingleDown = 5, TdDoubleDown = 6, TdNotComputable = 7, TdPlaceholder);  //< Trends for delta

BGTrends = array[BGTrend] of string;

BGUnit = (mmol, mgdl);              //< Glucose units with def
BGUnitMeta = array[BGUnit] of string;   //< Unique strings for each unit, when specialising etc
BGUnitVals = array[BGUnit] of single;    //< Unique data for units

BGVal = single;                              //< Glucose unit storage (signed, as its used for delta)
//  BGValLevel  = (BGHIGH, BGLOW, BGNORMAL, BGLOST);
BGValLevel = (BGRangeHI, BGRangeLO, BGRange, BGHigh, BGLOW);    // Range = depending on API

BGUnitArray = array[BGUnit, BGUnit] of single;

const
BG_TRENDS_STRING: array[BGTrend] of string = //< Standard API terms for trends
  ('DoubleUp', 'SingleUp', 'FortyFiveUp', 'Flat', 'FortyFiveDown',
  'SingleDown', 'DoubleDown', 'NOT COMPUTABLE', 'NOT FOUND');

BG_TRENDS: array[BGTrend] of string = //< Trends as text
  ('Rising Fast', 'Rising', 'Rising Slightly', 'Steady', 'Falling slightly',
  'Falling', 'Falling Fast', 'Unknown', 'Not found');

BG_TREND_ARROWS_UTF: array[BGTrend] of string =  //< Trends as UTF
  ('↑↑', '↑', '↗', '→', '↘',
  '↓', '↓↓', '?', 'X');

BG_TREND_ARROWS: array[BGTrend] of string = //< Trends as ASCII
  ( '^^', '^', '/´', '->', '\_', '\/', '\\//',
  '?', 'X');

BG_UNIT_NAMES: array[BGUnit] of string = ('mmol/L', 'mg/dL');

BG_MSG_DEF: BGUnitMeta = ('%.1f mmol/L', '%0.f mg/dL'); // Text format for units
BG_MSG_SHORT: BGUnitMeta = ('%.1f', '%0.f');
BG_MSG_SIGNED: BGUnitMeta = ('%+%.1f mmol/L', '%+%0.f mg/dL'); // Text format for units
BG_MSG_SIG_SHORT: BGUnitMeta = ('%+%.1f', '%+%0.f');

BG_NO_VAL = -904; //< Magic value for placeholder BG

BG_CONVERTIONS: BGUnitArray = ((1, 0.0555555556), (18.0182, 1)); //< Conversions
