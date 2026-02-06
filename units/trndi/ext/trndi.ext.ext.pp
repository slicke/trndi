
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

unit trndi.ext.ext;

interface

{**
  @abstract(Runtime loader for Trndi extensions.)
  Intended to discover and manage external shared libraries that implement
  the Trndi extension ABI. Note: the implementation is currently disabled
  in source (commented) and kept for future iterations.
}

implementation
{
{$mode ObjFPC}{$H+}{$ModeSwitch advancedrecords}

interface

uses 
Classes, SysUtils, StrUtils, dynlibs;

type 



  TrndiExt = class
    private 
      slug: ShortString;
      lib: TLibHandle;
      obj: pointer;
      //< The object in the library
      prio: SmallInt;
      apilvl: integer;
      author: string;

      function hasLib: boolean;
    public 
      property loaded: boolean read hasLib;
      constructor Create(fn: string; _slug: string; _prio: smallint);
      destructor Destroy;
      function Init(const bg: integer): boolean;
      //< Run the constructor in the lib
      procedure done;
      //< Destructor
      function config: string;
  end;

  TrndiExtList = record
    items: array of TrndiExt;
    function Add(fn: string; slug: string; prio: smallint): boolean;
    function Delete(id: integer): boolean;
    function IndexOf(slug: string): integer;
  end;

implementation

function TrndiExtList.Add(fn: string; slug: string; prio: smallint): boolean;
begin
  result := false;
  insert([TrndiExt.Create(fn, slug, 0)], items, 0);

  if items[0].loaded = false then
    result := true
  else
    Delete(0);
end;

function TrndiExtList.Delete(id: integer): boolean;
begin
  items[id].Destroy;
  System.Delete(items, id, 1);
end;

function TrndiExtList.IndexOf(slug: string): integer;

var 
  i: integer;
begin
  result := -1;
  for i := low(items) to high(items) do
    begin
      if items[i].slug = slug then
        begin
          result := i;
          Exit;
        end;
    end;
end;

destructor TrndiExt.Destroy;
begin
  // Ensure cleanup
  if obj <> nil then
    done; // Call done to free obj and unload lib
  if lib <> 0 then
    UnloadLibrary(lib);
end;

function TrndiExt.config: string;

var 
  _config: procedure (obj: Pointer; var author: PAnsiChar; var apilvl: integer);
  cdecl;
  auth: PAnsiChar;
  lvl: integer;
begin
  // Second, use the instance
  Pointer(_config) := GetProcAddress(lib, 'config');
  if @_config <> nil then
    begin
      GetMem(auth, 255);
      try
        _config(obj, auth, lvl);
        apilvl := lvl;
        author := auth;
      finally
        FreeMem(auth);
      end;
    end;
end;

procedure TrndiExt.done;

var 
  _done: procedure (obj: Pointer);
  cdecl;
begin
  pointer(_done) := GetProcAddress(lib, 'done');
  if @_done <> nil then
    begin
      _done(obj);
      obj := nil; // Assume _done frees obj
    end;
  if lib <> 0 then
    UnloadLibrary(lib);
  lib := 0; // Prevent double unload
end;

function TrndiExt.init(const bg: integer): boolean;

var 
  _init: procedure (obj: Pointer; bg: integer);
  cdecl;
begin
  result := false;
  Pointer(_init) := GetProcAddress(lib, 'init');
  if @_init <> nil then
    _init(obj, bg)
  else
    result := false;
end;

function TrndiExt.hasLib: boolean;
begin
  result := lib <> NilHandle;
end;

constructor TrndiExt.Create(fn: string; _slug: string; _prio: smallint);
begin
  slug := _slug;
  prio := _prio;
  lib := 0;
  obj := nil; // Explicitly initialize
  lib := LoadLibrary(fn);
  if lib = NilHandle then
    raise Exception.Create('Failed to load library: ' + fn);
end;


(*function TrndiExtList.AddExt(const fn, id: string): boolean;
begin
  result := false;
  insert([LibItem.init(id, LoadLibrary(fn), 0)], loaded, 0);

  if loaded[0].lib <> NilHandle then
    result := true
  else
    Delete(loaded, 0, 1);
  end;
end;
  *)
*}
end.
