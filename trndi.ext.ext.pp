(*
(c) 2024-2025 github.com/slicke - See LICENSE file, GPLv3, Written with the aid of GPT
*)
unit trndi.ext.ext;

interface

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
     obj: pointer; //< The object in the library
     prio: SmallInt;
     apilvl: integer;
     author: string;

     function hasLib: boolean;
   public
     property loaded: boolean read hasLib;
     constructor Create(fn: string; _slug: string; _prio: smallint);
     destructor Destroy;
     function Init(const bg: integer): boolean; //< Run the constructor in the lib
     procedure done; //< Destructor
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
  for i := low(items) to high(items) do begin
    if items[i].slug = slug then begin
      result := i;
      Exit;
    end;
  end;
end;

destructor TrndiExt.Destroy;
begin
  //todo: check FREE!!!!!
  if lib <> 0 then
    UnloadLibrary(lib);
end;

function TrndiExt.config: string;
var
   _config: procedure (obj: Pointer; var author: PAnsiChar; var apilvl: integer); cdecl;
   auth: PAnsiChar;
   lvl: integer;
begin
      // Second, use the instance
    Pointer(_config) := GetProcAddress(lib, 'config');
    if @_config <> nil then
    begin
      GetMem(auth, 255);
      _config(obj, auth, lvl);
      apilvl := lvl;
      author := auth;
      FreeMem(_config);
    end;
end;

procedure TrndiExt.done;
var
    _done: procedure (obj: Pointer); cdecl;
begin
    pointer(_done) := GetProcAddress(lib, 'done');
    if @_done <> nil then
    begin
      _done(obj);
      UnloadLibrary(lib);
    end;
end;

function TrndiExt.init(const bg: integer): boolean;
var
    _init: procedure (obj: Pointer; bg: integer); cdecl;
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
   lib := LoadLibrary(fn);
   if lib <> NilHandle then
     Exit;
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

