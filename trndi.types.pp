
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

{$I types.inc}

type 
BGValType = (BGPrimary, BGDelta);

BGReading = object
private
  curr: BGVal;
      //< The current (as in at this point in time, readng)
  change: BGVal;
      //< The difference since last reading
  valu: BGUnit;
      //< The value unit in which the reading is stored
  retu: BGUnit;
      //< The preffered output unit, it's generally better to call a function with data
  lvl: BGValLevel;
      //< The API's high/low limits
  src: shortstring;
      //< The API that reportd this value

  rssi: integer;
      //< The Received Signal Strength Indicator
  device: shortstring;
      //< Name of the device
  noise: integer;
      //< Noise


  function getCurr: single;
  function getChange: single;
  function checkEmpty: boolean;
  function getSrc: shortstring;
  function GetLevel: BGValLevel;
  procedure setReturn(bgu: BGUnit);
  procedure setLevel(bglvl: BGValLevel);
public

  trend: BGTrend;
      //< The trend "arrow"
  date: TDateTime;
      //< When the reading happened

  constructor Init(vtype: BGUnit; api: shortstring = '');
  constructor Init(vtype, rtype: BGUnit; api: shortstring = '');

  procedure update(val: BGVal; which: BGValType; u: BGUnit);
  procedure update(valCurr, valDelta: BGVal; u: BGUnit);
  procedure update(val: BGVal; which: BGValType);
  procedure update(valCurr, valDelta: BGVal);
  procedure updateEnv(valdevice: shortstring; valrssi: integer = 100; valnoise: integer = 0);// NS specific stuff
  procedure clear;
  function convert(u: BGUnit; which: BGValType = BGPrimary): single;
  function round(u: BGUnit; which: BGValType = BGPrimary): smallint;
  function format(u: BGUnit; fmt: BgUnitMeta; which: BGValType = BGPrimary; rounded: boolean =
    false): string;

  property val: single read GetCurr;
  property return: BGUnit write SetReturn;
  property delta: single read GetChange;
  property empty: boolean read CheckEmpty;
  property level: BGValLevel read GetLevel write SetLevel;
  property source: shortstring read GetSrc;
end;

BGResults = array of BGReading;
  //< Array definition for mulitple readings

BGTrendHelper =

  type helper for BGTrend //< A type helper to transform trends to images/text
  function Img: string;
  function Text: string;
end;

implementation

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

procedure BGReading.updateEnv(valdevice: shortstring; valrssi: integer = 100; valnoise: integer = 0);
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
