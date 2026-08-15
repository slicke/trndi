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
unit linutils.dbus;

{**
  @abstract(Minimal D-Bus client for Linux/BSD, talking to libdbus-1 directly.)

  Trndi's desktop integration is all D-Bus: notifications, the launcher badge,
  the appearance portal and logind's sleep signal. Each of those used to run
  the @code(gdbus) command-line tool, which forks a process per call, parses
  its printed output back into values, and is missing entirely on systems
  without GLib's binary package (Debian's @code(libglib2.0-bin) is not pulled
  in by KDE).

  This unit replaces that with the real thing: @code(libdbus-1.so.3) opened at
  runtime, so there is no link-time dependency and no build-system change —
  when the library is absent @link(DBusAvailable) returns @false and callers
  fall back to whatever they did before.

  The API is deliberately small; it covers exactly the shapes Trndi sends:

  @unorderedList(
    @item(method calls with a reply — @link(TDBusConn.CallBlocking))
    @item(signals — @link(TDBusConn.Send))
    @item(watching for someone else's signals — @link(TDBusConn.AddMatch) plus
          @link(TDBusConn.NextSignal))
  )

  Arguments are appended in order with the @code(Add*) methods;
  string-to-variant dictionaries — which every one of the above needs — are
  built between @link(TDBusMessage.OpenDict) and @link(TDBusMessage.CloseDict).

  Example — the appearance portal's color-scheme:
  @longcode(#
  conn := TDBusConn.Create(dbSession);
  msg := conn.NewCall('org.freedesktop.portal.Desktop',
    '/org/freedesktop/portal/desktop', 'org.freedesktop.portal.Settings', 'Read');
  msg.AddString('org.freedesktop.appearance');
  msg.AddString('color-scheme');
  reply := conn.CallBlocking(msg, 3000);
  if Assigned(reply) and reply.ReadUInt32(scheme) then ...
  #)

  Every call is safe to make when the library or the bus is missing: the
  constructor leaves @link(TDBusConn.Connected) @false and the rest no-op.
}

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, ctypes, dynlibs;

type
{** Which bus to talk to. The session bus carries the desktop services
    (notifications, portals, the launcher badge); the system bus carries
    logind. }
TDBusBusKind = (dbSession, dbSystem);

{** Opaque libdbus pointers. }
PDBusConnection = Pointer;
PDBusMessageRec = Pointer;

{** Opaque stand-in for @code(DBusMessageIter). libdbus only ever writes
    through a pointer to it and the real struct is 64 bytes on 64-bit, so an
    over-sized byte block is both ABI-safe and version-proof. }
TDBusIter = record
  Data: array[0..127] of byte;
end;
PDBusIter = ^TDBusIter;

{**
  @abstract(One D-Bus message, under construction or received.)

  Outgoing messages come from @link(TDBusConn.NewCall) or
  @link(TDBusConn.NewSignal), take their arguments through the @code(Add*)
  methods in signature order, and are consumed (and freed) by
  @link(TDBusConn.Send) or @link(TDBusConn.CallBlocking).

  Incoming messages — replies and signals — are read with the @code(Read*)
  methods and must be freed by the caller.
}
TDBusMessage = class
private
  FMsg: PDBusMessageRec;
  FAppend: TDBusIter;      // top-level append cursor
  FDict: TDBusIter;        // open dictionary array, between OpenDict/CloseDict
  FInDict: boolean;
  FRead: TDBusIter;        // top-level read cursor
  FReadStarted: boolean;
    // Append one variant-wrapped basic value as a dict entry.
  procedure DictAdd(const key: string; const signature: string;
    argType: cint; val: Pointer);
    // Position the read cursor, unwrapping any variant nesting on the way.
  function ReadBasic(argType: cint; val: Pointer): boolean;
public
  {** Wrap a raw libdbus message. Takes ownership: destroying this object
      unrefs it. }
  constructor Create(msg: PDBusMessageRec);
  destructor Destroy; override;

  {** Append a string (@code(s)). }
  procedure AddString(const val: string);
  {** Append a 32-bit unsigned integer (@code(u)). }
  procedure AddUInt32(val: cardinal);
  {** Append a 32-bit signed integer (@code(i)). }
  procedure AddInt32(val: integer);
  {** Append an array of strings (@code(as)); an empty array is fine. }
  procedure AddStringArray(const items: array of string);

  {** Begin a string-to-variant dictionary (D-Bus signature: array of
      dict-entry of string and variant). Follow with any number of
      @code(DictAdd*) calls and close it with @link(CloseDict); an empty
      dictionary (open immediately followed by close) is valid and is what
      "no hints" looks like on the wire. }
  procedure OpenDict;
  {** Add a string-valued entry to the open dictionary. }
  procedure DictAddString(const key, val: string);
  {** Add a byte-valued entry (notification urgency, for one). }
  procedure DictAddByte(const key: string; val: byte);
  {** Add a boolean-valued entry. }
  procedure DictAddBool(const key: string; val: boolean);
  {** Add a 32-bit signed integer entry (the launcher badge count). }
  procedure DictAddInt32(const key: string; val: integer);
  {** Add a 64-bit signed integer entry. }
  procedure DictAddInt64(const key: string; val: int64);
  {** Add a double entry (the launcher progress bar). }
  procedure DictAddDouble(const key: string; val: double);
  {** Close the dictionary opened by @link(OpenDict). }
  procedure CloseDict;

  {** Read the next argument as a 32-bit unsigned integer. Variants are
      unwrapped on the way, however deeply they nest — the portal returns its
      settings as a variant inside a variant. }
  function ReadUInt32(out val: cardinal): boolean;
  {** Read the next argument as a boolean, unwrapping variants. }
  function ReadBoolean(out val: boolean): boolean;
  {** Read the next argument as an object path (the portal's request
      handle), unwrapping variants. }
  function ReadObjectPath(out val: string): boolean;

  {** True when this message is the named signal. }
  function IsSignal(const iface, member: string): boolean;

  {** The underlying libdbus message. }
  property Handle: PDBusMessageRec read FMsg;
end;

{**
  @abstract(A connection to one of the buses.)

  Construction never raises: when libdbus is missing, or no bus is reachable
  (a headless session, a sandbox without the socket), @link(Connected) stays
  @false and every method is a no-op returning @false or @nil.
}
TDBusConn = class
private
  FConn: PDBusConnection;
  FPrivate: boolean;
public
  {** Connect to @param(kind).

      @param(usePrivate Ask for a connection of this object's own rather than
        the process-wide shared one. Threads that run their own read loop —
        the sleep monitor — need this, since popping messages off a shared
        connection would steal them from everyone else.) }
  constructor Create(kind: TDBusBusKind; usePrivate: boolean = false);
  destructor Destroy; override;

  {** True when there is a live bus connection to use. }
  function Connected: boolean;

  {** Start a method call. Free it, or hand it to @link(Send) or
      @link(CallBlocking) which free it for you. @nil when not connected. }
  function NewCall(const dest, path, iface, method: string): TDBusMessage;
  {** Start a signal to broadcast. @nil when not connected. }
  function NewSignal(const path, iface, member: string): TDBusMessage;

  {** Send @param(msg) without waiting for anything, and flush. Frees
      @param(msg). }
  function Send(msg: TDBusMessage): boolean;
  {** Send @param(msg) and wait up to @param(timeoutMS) for the reply. Frees
      @param(msg); the returned reply belongs to the caller. @nil on error,
      timeout, or a D-Bus error reply. }
  function CallBlocking(msg: TDBusMessage; timeoutMS: integer): TDBusMessage;

  {** Subscribe to signals matching a rule, e.g.
      @code(type='signal',interface='org.freedesktop.login1.Manager'). }
  function AddMatch(const rule: string): boolean;
  {** Wait up to @param(timeoutMS) for the next incoming message and return
      it, or @nil if none arrived. The caller frees it. Only meaningful on a
      private connection. }
  function NextSignal(timeoutMS: integer): TDBusMessage;
end;

{** True when libdbus-1 could be loaded. Everything else in this unit is a
    no-op when this is @false, so callers can simply try and check the
    result — but the notification/dark-mode probes report which backend they
    would use, and need to ask up front. }
function DBusAvailable: boolean;

implementation

// ---------------------------------------------------------------------------
// libdbus-1 ABI
// ---------------------------------------------------------------------------

const
  // Bus types (DBusBusType).
  DBUS_BUS_SESSION = 0;
  DBUS_BUS_SYSTEM = 1;

  // Argument type codes; these are the ASCII characters of the signature.
  DBUS_TYPE_INVALID = 0;
  DBUS_TYPE_BOOLEAN = 98;   // 'b'
  DBUS_TYPE_INT32 = 105;    // 'i'
  DBUS_TYPE_UINT32 = 117;   // 'u'
  DBUS_TYPE_INT64 = 120;    // 'x'
  DBUS_TYPE_DOUBLE = 100;   // 'd'
  DBUS_TYPE_STRING = 115;   // 's'
  DBUS_TYPE_OBJECT_PATH = 111; // 'o'
  DBUS_TYPE_ARRAY = 97;     // 'a'
  DBUS_TYPE_VARIANT = 118;  // 'v'
  DBUS_TYPE_DICT_ENTRY = 101; // 'e'
  DBUS_TYPE_BYTE = 121;     // 'y'

type
PDBusError = ^TDBusError;
{** Mirrors @code(DBusError). Only name/message are read here; the trailing
    block covers the private fields libdbus keeps after them. }
TDBusError = record
  name: PAnsiChar;
  message: PAnsiChar;
  dummies: cuint;
  padding1: Pointer;
  reserve: array[0..63] of byte;
end;

var
  DBusLib: TLibHandle = NilHandle;
  DBusTried: boolean = false;
  DBusOK: boolean = false;

  dbus_threads_init_default: function: cint; cdecl;
  dbus_error_init: procedure(err: PDBusError); cdecl;
  dbus_error_free: procedure(err: PDBusError); cdecl;
  dbus_error_is_set: function(err: PDBusError): cint; cdecl;
  dbus_bus_get: function(kind: cint; err: PDBusError): PDBusConnection; cdecl;
  dbus_bus_get_private: function(kind: cint; err: PDBusError): PDBusConnection; cdecl;
  dbus_bus_add_match: procedure(conn: PDBusConnection; rule: PAnsiChar;
    err: PDBusError); cdecl;
  dbus_connection_close: procedure(conn: PDBusConnection); cdecl;
  dbus_connection_unref: procedure(conn: PDBusConnection); cdecl;
  dbus_connection_set_exit_on_disconnect: procedure(conn: PDBusConnection;
    val: cint); cdecl;
  dbus_connection_get_is_connected: function(conn: PDBusConnection): cint; cdecl;
  dbus_connection_send: function(conn: PDBusConnection; msg: PDBusMessageRec;
    serial: pcuint32): cint; cdecl;
  dbus_connection_send_with_reply_and_block: function(conn: PDBusConnection;
    msg: PDBusMessageRec; timeout: cint; err: PDBusError): PDBusMessageRec; cdecl;
  dbus_connection_flush: procedure(conn: PDBusConnection); cdecl;
  dbus_connection_read_write: function(conn: PDBusConnection;
    timeout: cint): cint; cdecl;
  dbus_connection_pop_message: function(conn: PDBusConnection): PDBusMessageRec; cdecl;
  dbus_message_new_method_call: function(dest, path, iface,
    method: PAnsiChar): PDBusMessageRec; cdecl;
  dbus_message_new_signal: function(path, iface,
    member: PAnsiChar): PDBusMessageRec; cdecl;
  dbus_message_unref: procedure(msg: PDBusMessageRec); cdecl;
  dbus_message_is_signal: function(msg: PDBusMessageRec; iface,
    member: PAnsiChar): cint; cdecl;
  dbus_message_iter_init_append: procedure(msg: PDBusMessageRec;
    iter: PDBusIter); cdecl;
  dbus_message_iter_append_basic: function(iter: PDBusIter; argType: cint;
    val: Pointer): cint; cdecl;
  dbus_message_iter_open_container: function(iter: PDBusIter; argType: cint;
    signature: PAnsiChar; sub: PDBusIter): cint; cdecl;
  dbus_message_iter_close_container: function(iter, sub: PDBusIter): cint; cdecl;
  dbus_message_iter_init: function(msg: PDBusMessageRec;
    iter: PDBusIter): cint; cdecl;
  dbus_message_iter_get_arg_type: function(iter: PDBusIter): cint; cdecl;
  dbus_message_iter_get_basic: procedure(iter: PDBusIter; val: Pointer); cdecl;
  dbus_message_iter_recurse: procedure(iter, sub: PDBusIter); cdecl;

{------------------------------------------------------------------------------
  Sym
  ---
  Resolve one symbol, recording a miss so LoadDBus can bail out wholesale
  rather than leave a half-populated function table behind.
 ------------------------------------------------------------------------------}
function Sym(const name: string; var missing: boolean): Pointer;
begin
  Result := GetProcedureAddress(DBusLib, name);
  if Result = nil then
    missing := true;
end;

{------------------------------------------------------------------------------
  LoadDBus
  --------
  Open libdbus-1 once, by SONAME first (the versioned file is what is actually
  installed; the unversioned symlink belongs to the -dev package and is
  usually absent on user machines).
 ------------------------------------------------------------------------------}
function LoadDBus: boolean;
var
  missing: boolean;
begin
  if DBusTried then
    Exit(DBusOK);
  DBusTried := true;
  DBusOK := false;

  DBusLib := LoadLibrary('libdbus-1.so.3');
  if DBusLib = NilHandle then
    DBusLib := LoadLibrary('libdbus-1.so');
  if DBusLib = NilHandle then
    Exit(false);

  missing := false;
  Pointer(dbus_threads_init_default) := Sym('dbus_threads_init_default', missing);
  Pointer(dbus_error_init) := Sym('dbus_error_init', missing);
  Pointer(dbus_error_free) := Sym('dbus_error_free', missing);
  Pointer(dbus_error_is_set) := Sym('dbus_error_is_set', missing);
  Pointer(dbus_bus_get) := Sym('dbus_bus_get', missing);
  Pointer(dbus_bus_get_private) := Sym('dbus_bus_get_private', missing);
  Pointer(dbus_bus_add_match) := Sym('dbus_bus_add_match', missing);
  Pointer(dbus_connection_close) := Sym('dbus_connection_close', missing);
  Pointer(dbus_connection_unref) := Sym('dbus_connection_unref', missing);
  Pointer(dbus_connection_set_exit_on_disconnect) :=
    Sym('dbus_connection_set_exit_on_disconnect', missing);
  Pointer(dbus_connection_get_is_connected) :=
    Sym('dbus_connection_get_is_connected', missing);
  Pointer(dbus_connection_send) := Sym('dbus_connection_send', missing);
  Pointer(dbus_connection_send_with_reply_and_block) :=
    Sym('dbus_connection_send_with_reply_and_block', missing);
  Pointer(dbus_connection_flush) := Sym('dbus_connection_flush', missing);
  Pointer(dbus_connection_read_write) := Sym('dbus_connection_read_write', missing);
  Pointer(dbus_connection_pop_message) := Sym('dbus_connection_pop_message', missing);
  Pointer(dbus_message_new_method_call) :=
    Sym('dbus_message_new_method_call', missing);
  Pointer(dbus_message_new_signal) := Sym('dbus_message_new_signal', missing);
  Pointer(dbus_message_unref) := Sym('dbus_message_unref', missing);
  Pointer(dbus_message_is_signal) := Sym('dbus_message_is_signal', missing);
  Pointer(dbus_message_iter_init_append) :=
    Sym('dbus_message_iter_init_append', missing);
  Pointer(dbus_message_iter_append_basic) :=
    Sym('dbus_message_iter_append_basic', missing);
  Pointer(dbus_message_iter_open_container) :=
    Sym('dbus_message_iter_open_container', missing);
  Pointer(dbus_message_iter_close_container) :=
    Sym('dbus_message_iter_close_container', missing);
  Pointer(dbus_message_iter_init) := Sym('dbus_message_iter_init', missing);
  Pointer(dbus_message_iter_get_arg_type) :=
    Sym('dbus_message_iter_get_arg_type', missing);
  Pointer(dbus_message_iter_get_basic) :=
    Sym('dbus_message_iter_get_basic', missing);
  Pointer(dbus_message_iter_recurse) := Sym('dbus_message_iter_recurse', missing);

  if missing then
  begin
    UnloadLibrary(DBusLib);
    DBusLib := NilHandle;
    Exit(false);
  end;

  // Trndi calls out from worker threads (the sleep monitor, async fetches),
  // so libdbus needs its locking primitives installed before first use.
  dbus_threads_init_default;
  DBusOK := true;
  Result := true;
end;

{------------------------------------------------------------------------------
  DBusAvailable
 ------------------------------------------------------------------------------}
function DBusAvailable: boolean;
begin
  Result := LoadDBus;
end;

// ---------------------------------------------------------------------------
// TDBusMessage
// ---------------------------------------------------------------------------

constructor TDBusMessage.Create(msg: PDBusMessageRec);
begin
  inherited Create;
  FMsg := msg;
  FInDict := false;
  FReadStarted := false;
  if FMsg <> nil then
    dbus_message_iter_init_append(FMsg, @FAppend);
end;

destructor TDBusMessage.Destroy;
begin
  if FMsg <> nil then
    dbus_message_unref(FMsg);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  AddString / AddUInt32 / AddInt32
  --------------------------------
  append_basic takes the address of the value, and for strings that means the
  address of the char pointer, not the characters. libdbus copies out of it
  before returning, so the local is enough to keep alive.
 ------------------------------------------------------------------------------}
procedure TDBusMessage.AddString(const val: string);
var
  s: RawByteString;
  p: PAnsiChar;
begin
  if FMsg = nil then
    Exit;
  s := val;
  p := PAnsiChar(s);
  dbus_message_iter_append_basic(@FAppend, DBUS_TYPE_STRING, @p);
end;

procedure TDBusMessage.AddUInt32(val: cardinal);
var
  v: cuint32;
begin
  if FMsg = nil then
    Exit;
  v := val;
  dbus_message_iter_append_basic(@FAppend, DBUS_TYPE_UINT32, @v);
end;

procedure TDBusMessage.AddInt32(val: integer);
var
  v: cint32;
begin
  if FMsg = nil then
    Exit;
  v := val;
  dbus_message_iter_append_basic(@FAppend, DBUS_TYPE_INT32, @v);
end;

{------------------------------------------------------------------------------
  AddStringArray
  --------------
  An array is a container even when empty — the signature has to be declared
  either way, which is how the receiver knows what it did not get.
 ------------------------------------------------------------------------------}
procedure TDBusMessage.AddStringArray(const items: array of string);
var
  sub: TDBusIter;
  s: RawByteString;
  p: PAnsiChar;
  i: integer;
begin
  if FMsg = nil then
    Exit;
  if dbus_message_iter_open_container(@FAppend, DBUS_TYPE_ARRAY, 's', @sub) = 0 then
    Exit;
  for i := 0 to High(items) do
  begin
    s := items[i];
    p := PAnsiChar(s);
    dbus_message_iter_append_basic(@sub, DBUS_TYPE_STRING, @p);
  end;
  dbus_message_iter_close_container(@FAppend, @sub);
end;

{------------------------------------------------------------------------------
  OpenDict / CloseDict
 ------------------------------------------------------------------------------}
procedure TDBusMessage.OpenDict;
begin
  if (FMsg = nil) or FInDict then
    Exit;
  if dbus_message_iter_open_container(@FAppend, DBUS_TYPE_ARRAY, '{sv}',
    @FDict) <> 0 then
    FInDict := true;
end;

procedure TDBusMessage.CloseDict;
begin
  if (FMsg = nil) or (not FInDict) then
    Exit;
  dbus_message_iter_close_container(@FAppend, @FDict);
  FInDict := false;
end;

{------------------------------------------------------------------------------
  DictAdd
  -------
  One dictionary entry: a dict-entry container holding the key and a variant, and
  the variant in turn holding the value. Each level has to be closed in
  reverse or the message is malformed and the bus drops the caller.
 ------------------------------------------------------------------------------}
procedure TDBusMessage.DictAdd(const key: string; const signature: string;
argType: cint; val: Pointer);
var
  entry, variant: TDBusIter;
  k: RawByteString;
  sig: RawByteString;
  p: PAnsiChar;
begin
  if (FMsg = nil) or (not FInDict) then
    Exit;
  if dbus_message_iter_open_container(@FDict, DBUS_TYPE_DICT_ENTRY, nil,
    @entry) = 0 then
    Exit;
  k := key;
  p := PAnsiChar(k);
  dbus_message_iter_append_basic(@entry, DBUS_TYPE_STRING, @p);
  sig := signature;
  if dbus_message_iter_open_container(@entry, DBUS_TYPE_VARIANT,
    PAnsiChar(sig), @variant) <> 0 then
  begin
    dbus_message_iter_append_basic(@variant, argType, val);
    dbus_message_iter_close_container(@entry, @variant);
  end;
  dbus_message_iter_close_container(@FDict, @entry);
end;

procedure TDBusMessage.DictAddString(const key, val: string);
var
  s: RawByteString;
  p: PAnsiChar;
begin
  s := val;
  p := PAnsiChar(s);
  DictAdd(key, 's', DBUS_TYPE_STRING, @p);
end;

procedure TDBusMessage.DictAddByte(const key: string; val: byte);
var
  v: byte;
begin
  v := val;
  DictAdd(key, 'y', DBUS_TYPE_BYTE, @v);
end;

procedure TDBusMessage.DictAddBool(const key: string; val: boolean);
var
  v: cint;
begin
  // dbus_bool_t is a 32-bit int, not a Pascal ByteBool: passing a one-byte
  // value here would leave three bytes of stack garbage in the message.
  if val then
    v := 1
  else
    v := 0;
  DictAdd(key, 'b', DBUS_TYPE_BOOLEAN, @v);
end;

procedure TDBusMessage.DictAddInt32(const key: string; val: integer);
var
  v: cint32;
begin
  v := val;
  DictAdd(key, 'i', DBUS_TYPE_INT32, @v);
end;

procedure TDBusMessage.DictAddInt64(const key: string; val: int64);
var
  v: cint64;
begin
  v := val;
  DictAdd(key, 'x', DBUS_TYPE_INT64, @v);
end;

procedure TDBusMessage.DictAddDouble(const key: string; val: double);
var
  v: cdouble;
begin
  v := val;
  DictAdd(key, 'd', DBUS_TYPE_DOUBLE, @v);
end;

{------------------------------------------------------------------------------
  ReadBasic
  ---------
  Read the argument under the cursor, stepping into variants first. The
  appearance portal answers Read() with a variant wrapping a variant wrapping
  the value, and nothing says a service cannot nest one deeper still, so the
  unwrapping loops rather than counting.
 ------------------------------------------------------------------------------}
function TDBusMessage.ReadBasic(argType: cint; val: Pointer): boolean;
var
  iter, sub: TDBusIter;
  guard: integer;
begin
  Result := false;
  if FMsg = nil then
    Exit;
  if not FReadStarted then
  begin
    if dbus_message_iter_init(FMsg, @FRead) = 0 then
      Exit; // no arguments at all
    FReadStarted := true;
  end;
  iter := FRead;

  guard := 0;
  while (dbus_message_iter_get_arg_type(@iter) = DBUS_TYPE_VARIANT) and
    (guard < 8) do
  begin
    dbus_message_iter_recurse(@iter, @sub);
    iter := sub;
    Inc(guard);
  end;

  if dbus_message_iter_get_arg_type(@iter) <> argType then
    Exit;
  dbus_message_iter_get_basic(@iter, val);
  Result := true;
end;

function TDBusMessage.ReadUInt32(out val: cardinal): boolean;
var
  v: cuint32;
begin
  v := 0;
  Result := ReadBasic(DBUS_TYPE_UINT32, @v);
  val := v;
end;

function TDBusMessage.ReadBoolean(out val: boolean): boolean;
var
  v: cint;
begin
  v := 0;
  Result := ReadBasic(DBUS_TYPE_BOOLEAN, @v);
  val := v <> 0;
end;

function TDBusMessage.ReadObjectPath(out val: string): boolean;
var
  p: PAnsiChar;
begin
  p := nil;
  val := '';
  // The pointed-to characters belong to the message; copy before it is freed.
  Result := ReadBasic(DBUS_TYPE_OBJECT_PATH, @p);
  if Result and (p <> nil) then
    val := string(p);
end;

function TDBusMessage.IsSignal(const iface, member: string): boolean;
begin
  Result := (FMsg <> nil) and
    (dbus_message_is_signal(FMsg, PAnsiChar(RawByteString(iface)),
    PAnsiChar(RawByteString(member))) <> 0);
end;

// ---------------------------------------------------------------------------
// TDBusConn
// ---------------------------------------------------------------------------

{------------------------------------------------------------------------------
  Create
  ------
  Connect, and immediately turn off libdbus's default behaviour of calling
  exit() when the bus goes away: a restarted session bus must not take Trndi
  down with it.
 ------------------------------------------------------------------------------}
constructor TDBusConn.Create(kind: TDBusBusKind; usePrivate: boolean = false);
var
  err: TDBusError;
  busType: cint;
begin
  inherited Create;
  FConn := nil;
  FPrivate := usePrivate;
  if not LoadDBus then
    Exit;

  if kind = dbSystem then
    busType := DBUS_BUS_SYSTEM
  else
    busType := DBUS_BUS_SESSION;

  FillChar(err, SizeOf(err), 0);
  dbus_error_init(@err);
  try
    if usePrivate then
      FConn := dbus_bus_get_private(busType, @err)
    else
      FConn := dbus_bus_get(busType, @err);
  except
    FConn := nil;
  end;
  if dbus_error_is_set(@err) <> 0 then
    dbus_error_free(@err);

  if FConn <> nil then
    dbus_connection_set_exit_on_disconnect(FConn, 0);
end;

destructor TDBusConn.Destroy;
begin
  if FConn <> nil then
  begin
    // Only private connections may be closed — the shared one belongs to the
    // whole process and closing it would break every other user. Both kinds
    // hand out a reference that has to go back.
    if FPrivate then
      dbus_connection_close(FConn);
    dbus_connection_unref(FConn);
    FConn := nil;
  end;
  inherited Destroy;
end;

function TDBusConn.Connected: boolean;
begin
  Result := (FConn <> nil) and (dbus_connection_get_is_connected(FConn) <> 0);
end;

function TDBusConn.NewCall(const dest, path, iface, method: string): TDBusMessage;
var
  msg: PDBusMessageRec;
begin
  Result := nil;
  if FConn = nil then
    Exit;
  msg := dbus_message_new_method_call(PAnsiChar(RawByteString(dest)),
    PAnsiChar(RawByteString(path)), PAnsiChar(RawByteString(iface)),
    PAnsiChar(RawByteString(method)));
  if msg <> nil then
    Result := TDBusMessage.Create(msg);
end;

function TDBusConn.NewSignal(const path, iface, member: string): TDBusMessage;
var
  msg: PDBusMessageRec;
begin
  Result := nil;
  if FConn = nil then
    Exit;
  msg := dbus_message_new_signal(PAnsiChar(RawByteString(path)),
    PAnsiChar(RawByteString(iface)), PAnsiChar(RawByteString(member)));
  if msg <> nil then
    Result := TDBusMessage.Create(msg);
end;

function TDBusConn.Send(msg: TDBusMessage): boolean;
begin
  Result := false;
  if msg = nil then
    Exit;
  try
    if (FConn = nil) or (msg.Handle = nil) then
      Exit;
    Result := dbus_connection_send(FConn, msg.Handle, nil) <> 0;
    if Result then
      dbus_connection_flush(FConn);
  finally
    msg.Free;
  end;
end;

{------------------------------------------------------------------------------
  CallBlocking
  ------------
  A D-Bus error reply is a message like any other, so the error struct is what
  distinguishes "the service said no" from "the service answered".
 ------------------------------------------------------------------------------}
function TDBusConn.CallBlocking(msg: TDBusMessage;
timeoutMS: integer): TDBusMessage;
var
  err: TDBusError;
  reply: PDBusMessageRec;
begin
  Result := nil;
  if msg = nil then
    Exit;
  try
    if (FConn = nil) or (msg.Handle = nil) then
      Exit;
    FillChar(err, SizeOf(err), 0);
    dbus_error_init(@err);
    reply := dbus_connection_send_with_reply_and_block(FConn, msg.Handle,
      timeoutMS, @err);
    if dbus_error_is_set(@err) <> 0 then
    begin
      dbus_error_free(@err);
      if reply <> nil then
      begin
        dbus_message_unref(reply);
        reply := nil;
      end;
    end;
    if reply <> nil then
      Result := TDBusMessage.Create(reply);
  finally
    msg.Free;
  end;
end;

function TDBusConn.AddMatch(const rule: string): boolean;
var
  err: TDBusError;
begin
  Result := false;
  if FConn = nil then
    Exit;
  FillChar(err, SizeOf(err), 0);
  dbus_error_init(@err);
  dbus_bus_add_match(FConn, PAnsiChar(RawByteString(rule)), @err);
  Result := dbus_error_is_set(@err) = 0;
  if not Result then
    dbus_error_free(@err)
  else
    dbus_connection_flush(FConn);
end;

{------------------------------------------------------------------------------
  NextSignal
  ----------
  read_write + pop_message rather than read_write_dispatch: dispatching would
  hand messages to filter functions we do not install, and they would never
  reach the queue this pops from.
 ------------------------------------------------------------------------------}
function TDBusConn.NextSignal(timeoutMS: integer): TDBusMessage;
var
  msg: PDBusMessageRec;
begin
  Result := nil;
  if FConn = nil then
    Exit;
  msg := dbus_connection_pop_message(FConn);
  if msg = nil then
  begin
    if dbus_connection_read_write(FConn, timeoutMS) = 0 then
      Exit; // disconnected
    msg := dbus_connection_pop_message(FConn);
  end;
  if msg <> nil then
    Result := TDBusMessage.Create(msg);
end;

end.
