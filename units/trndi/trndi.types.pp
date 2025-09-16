(*
 * This file is part of Trndi (https://github.com/xxxx or http://xxx.github.io).
 * Copyright (c) 2021-2024 Björn Lindh.
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
unit trndi.types;

{$mode ObjFPC}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  SysUtils, Dialogs;

{$I inc/types.inc}

type
  {** Selects which BG value is referenced: the primary reading or its delta. }
  BGValType = (BGPrimary, BGDelta);

  {** A single blood glucose (BG) reading with metadata and helpers.
      Stores the current value, change (delta), units, level classification,
      and environment details such as device, signal, and noise. Provides
      conversion, formatting, and update helpers.

      Typical usage:
      - Construct with @code(Init) indicating internal and return units.
      - Call @code(update) to set values and optionally delta.
      - Read via properties @code(val), @code(delta), @code(level), etc.
   }
  BGReading = object
  private
    curr: BGVal;          //< The current reading value (native internal unit)
    change: BGVal;        //< The difference since last reading (native internal unit)
    valu: BGUnit;         //< Internal unit in which the reading is stored
    retu: BGUnit;         //< Preferred output unit for property-based access
    lvl: BGValLevel;      //< Classification limits as provided by the API
    src: shortstring;     //< Source/API identifier that reported this value
    rssi: integer;        //< Received Signal Strength Indicator (-1 if unknown)
    device: shortstring;  //< Device name/identifier (may be empty)
    noise: integer;       //< Noise indicator (-1 if unknown)

    function getCurr: single;
    function getChange: single;
    function checkEmpty: boolean;
    function getSrc: shortstring;
    function GetLevel: BGValLevel;
    function getDevice: string;
    procedure setReturn(bgu: BGUnit);
    procedure setLevel(bglvl: BGValLevel);

  public
    trend: BGTrend;       //< Trend “arrow” enumeration
    date: TDateTime;      //< Timestamp when the reading occurred

    {** Initialize with same storage and return unit.
        @param(vtype Internal storage unit)
        @param(api   Optional API/source name) }
    constructor Init(vtype: BGUnit; api: shortstring = '');

    {** Initialize with explicit storage and preferred return units.
        @param(vtype Internal storage unit)
        @param(rtype Preferred return unit for property access)
        @param(api   Optional API/source name) }
    constructor Init(vtype, rtype: BGUnit; api: shortstring = '');

    {** Update either primary or delta value with an explicit source unit.
        Value is converted into internal storage unit.
        @param(val   Numeric value)
        @param(which Which value to update: primary or delta)
        @param(u     Unit of the provided value) }
    procedure update(val: BGVal; which: BGValType; u: BGUnit);

    {** Update both current and delta values with an explicit source unit.
        @param(valCurr  Current reading)
        @param(valDelta Delta)
        @param(u        Unit of the provided values) }
    procedure update(valCurr, valDelta: BGVal; u: BGUnit);

    {** Update either primary or delta value, assuming the object’s internal unit. }
    procedure update(val: BGVal; which: BGValType);

    {** Update both current and delta values, assuming the object’s internal unit. }
    procedure update(valCurr, valDelta: BGVal);

    {** Populate environment metadata (device, rssi, noise).
        @param(valdevice Device name)
        @param(valrssi   Signal indicator; -1 if unknown)
        @param(valnoise  Noise indicator; -1 if unknown) }
    procedure updateEnv(valdevice: shortstring; valrssi: integer = -1; valnoise: integer = -1);// NS specific stuff

    {** Clear the numeric values (sets to @code(BG_NO_VAL)). }
    procedure clear;

    {** Convert a value to another unit.
        @param(u     Target unit)
        @param(which Which value to convert: primary or delta)
        @returns Converted value as single }
    function convert(u: BGUnit; which: BGValType = BGPrimary): single;

    {** Convert then round to nearest integer (for display).
        @param(u     Target unit)
        @param(which Which value: primary or delta)
        @returns Rounded smallint }
    function round(u: BGUnit; which: BGValType = BGPrimary): smallint;

    {** Format a value using a unit-specific format string.
        The format map @code(fmt) holds per-unit format strings. A special
        token '%+' in the format string is replaced by a sign marker:
        '+' for positive, '±' for zero, and '' for negative (minus comes from value).
        @param(u       Target unit)
        @param(fmt     Format strings map per unit)
        @param(which   Primary or delta value)
        @param(rounded If true, uses rounded value)
        @returns Formatted string }
    function format(u: BGUnit; fmt: BgUnitMeta; which: BGValType = BGPrimary; rounded: boolean =
      false): string;

    {** Current value in preferred return unit. }
    property val: single read GetCurr;

    {** Set the preferred return unit for property-based reads. }
    property return: BGUnit write SetReturn;

    {** Delta value in preferred return unit. }
    property delta: single read GetChange;

    {** True if the primary BG value is unset (@code(BG_NO_VAL)). }
    property empty: boolean read CheckEmpty;

    {** Level classification as provided by the API or computed elsewhere. }
    property level: BGValLevel read GetLevel write SetLevel;

    {** Source identifier (API) string. }
    property source: shortstring read GetSrc;

    {** Device/sensor string; returns '<unknown>' if empty. }
    property sensor: string read getDevice;

    {** Get RSSI value if present.
        @param(outval Receives RSSI)
        @returns True if RSSI is defined (>=0) }
    function getRSSI(out outval: integer): boolean;

    {** Get noise value if present.
        @param(outval Receives noise)
        @returns True if noise is defined (>=0) }
    function getNoise(out outval: integer): boolean;
  end;

  {** Dynamic array of readings (timeline or batch). }
  BGResults = array of BGReading; //< Array definition for mulitple readings

  {** Helper for @code(BGTrend) to produce textual/UTF representations. }
  BGTrendHelper =
    type helper for BGTrend //< A type helper to transform trends to images/text
      {** UTF representation (emoji/arrow) for the trend. }
      function Img: string;
      {** Text (ASCII) representation for the trend. }
      function Text: string;
    end;

implementation

function BGReading.getRSSI(out outval: integer): boolean;
begin
  outval := self.rssi;
  result := Self.rssi > -1;
end;

function BGReading.getNoise(out outval: integer): boolean;
begin
  outval := self.noise;
  result := Self.noise > -1;
end;

function BGReading.getDevice: string;
begin
  result := self.device;
  if result.IsEmpty then
    result := '<unknown>';
end;

{ Sets the preffered return unit, for properties
  @param(bgu A unit )
  @raises( none )
}
procedure BGReading.setReturn(bgu: BGUnit);
begin
  retu := bgu;
end;

{ Converts/returns the current reading as a type
  @param(u Requested unit )
  @param(which which result to return, eg the reading or the delta )
  @returns( the value )
  @raises( none )
}
function BGReading.convert(u: BGUnit; which: BGValType = BGPrimary
): single;
begin
  case which of
  BGPrimary:
    result := curr * BG_CONVERTIONS[u][valu];
  BGDelta:
    result  := change * BG_CONVERTIONS[u][valu];
  end;
end;

{ Returns the current reading in integer form
  @param(u Requested unit )
  @param(which which result to return, eg the reading or the delta )
  @returns( A rounded integer value )
  @raises( none )
}
function BGReading.round(u: BGUnit; which: BGValType = BGPrimary):
smallint;
begin
  result := system.Round(self.convert(u,which));
end;

{ Returns a text/string representation
  @param(u Requested unit )
  @param(fmt An array with "format" strings for each unit type @see BG_MSG_*)
  @param(which which result to return, eg the reading or the delta )
  @param(rounded Use the round function before converting to string? )
  @returns( Format ran over the requested value, in the "u" unit )
  @raises( none )
}
function BGReading.format(u: BGUnit; fmt: BgUnitMeta; which:
BGValType = BGPrimary; rounded: boolean = false): string;
var
  cval: single;
  str: string;

{ Returns a sign, eg + or +/- to make string output signed, not only when -
  @returns( description )
  @raises( none )
}
function sign: string;
  begin
    if cval = 0 then
      result := '±'
    else
    if cval >= 0 then
      result := '+'
    else // Minus is appended by the conversion
      result := '';
  end;

begin
  if rounded then
    cval := round(u, which)
  else
    cval := convert(u, which);
                                  // We replace our owhen %- with the sign
  str := StringReplace(fmt[u], '%+', sign,  [rfReplaceAll]);
  result := SysUtils.Format(str, [cval])
end;

constructor BGReading.Init(vtype: BGUnit; api: shortstring = '');
begin
  Init(vtype, vtype, api);
end;

{ Initializes the objevt
  @param(vtype The unit in which the data is stored internally )
  @param(rtype Preffered return unit, used for properties. Can be omitted )
  @raises( none )
}
constructor BGReading.Init(vtype, rtype: BGUnit; api: shortstring
= '');
begin
  valu := vtype;
  retu := rtype;
  curr := BG_NO_VAL;
  change := BG_NO_VAL;
  src := api;
end;

{ Checks if the primary bg value is unset
  @returns( description )
  @raises( none )
}
function BGReading.checkEmpty: boolean;
begin
  result := curr = BG_NO_VAL;
end;

{ Returns the API's native high/low
  @returns( Description of BG value )
  @raises( none )
}
function BGReading.GetLevel: BGValLevel;
begin
  result := lvl;
end;

{ Returns the API's native high/low
  @returns( Description of BG value )
  @raises( none )
}
procedure BGReading.SetLevel(bglvl: BGValLevel);
begin
  lvl := bglvl;
end;

{ Returns the current reading, in the pre-set default return unit
  @returns( The reading )
  @raises( none )
}
function BGReading.getCurr: single;
begin
  result := self.convert(retu);
end;

{ Returns the current delta, in the pre-set default return unit
  @returns( The delta )
  @raises( none )
}
function BGReading.getChange: single;
begin
  result := self.convert(retu, BGDelta);
end;

{ Set the glucose data
  @param(val The reding )
  @param(which Which value is being set, eg delta or primary )
  @param(u unit in which the data is provided)
  @raises( none )
}
procedure BGReading.update(val: BGVal; which: BGValType; u: BGUnit
);
begin
  case which of
  BGPrimary:
    self.curr := val * BG_CONVERTIONS[u][self.valu];
  BGDelta:
    self.change  := val * BG_CONVERTIONS[u][self.valu];
  end;
end;

procedure BGReading.updateEnv(valdevice: shortstring; valrssi: integer = -1; valnoise: integer = -1);
begin
  self.rssi := valrssi;
  self.Noise := valnoise;
  self.device := valdevice;
end;

procedure BGReading.update(val: BGVal; which: BGValType);
begin
  update(val, which, valu);
end;

procedure BGReading.update(valCurr, valDelta: BGVal; u: BGUnit);
begin
  self.update(valCurr, BGPrimary, u);
  self.update(valDelta, BGDelta, u);
end;

procedure BGReading.update(valCurr, valDelta: BGVal);
begin
  self.update(valCurr, valDelta, valu);
end;

                                  //--
{ Returns the image (UTF) representation of a trend
  @returns( UTF )
  @raises( none )
}
function BGTrendHelper.Img: string;
begin
  result := BG_TREND_ARROWS_UTF[self];
end;

{ Returns the tect (ASCII) representation of a trend
  @returns( UTF )
  @raises( none )
}
function BGTrendHelper.Text: string;
begin
  result := BG_TREND_ARROWS[self];
end;

{ Clears out all bg data and sets it to undefined
  @raises( none )
}
procedure BgReading.clear;
begin
  self.curr := BG_NO_VAL;
  self.change := BG_NO_VAL;
end;

function BgReading.getSrc: shortstring;
begin
  result := src;
end;

(*
// Example convenience text builder:
function getDefDisplay(u: BGUnit; short: boolean = true; signed: boolean = false): string;
begin
  if short then begin
    if signed then
      result := self.toString(u, BG_MSG_SHORTSIGN)
    else
      result := self.toString(u, BG_MSG_SHORT);
  end
  else begin
   if signed then
    result := self.toString(u, BG_MSG_SIGNED)
  else
      result := self.toString(u, BG_MSG_DEF);
  end;
end;
*)
end.
