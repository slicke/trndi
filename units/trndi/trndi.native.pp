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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

{**
  @abstract(Façade that selects the platform-native implementation.)
  Re-exports @link(TTrndiNativeBase) and aliases @code(TrndiNative) to the
  platform-specific class based on compile-time defines.
  
  Consumers should reference the @code(TrndiNative) alias in their code
  instead of touching platform units directly. This keeps the application
  portable and avoids conditional compilation in call-sites. All public API
  is defined in @link(TTrndiNativeBase) and implemented by the platform class.
}

unit trndi.native;

{$I ../../inc/native.inc}

interface

uses
trndi.native.base
{$IF DEFINED(X_WIN)}, trndi.native.win
{$ELSEIF DEFINED(X_MAC)}, trndi.native.mac
{$ELSEIF DEFINED(HAIKU)}, trndi.native.haiku
{$ELSEIF DEFINED(BSD)}, trndi.native.bsd
{$ELSE}, trndi.native.linux
{$ENDIF}
;

type
  // Re-export base types
TTrndiNativeBase = trndi.native.base.TTrndiNativeBase;
TWSLVersion = trndi.native.base.TWSLVersion;
TTrndiBool = trndi.native.base.TTrndiBool;
TWSLInfo = trndi.native.base.TWSLInfo;

{$IF DEFINED(X_WIN)}
TTrndiNativeWindows = trndi.native.win.TTrndiNativeWindows;
TrndiNative = TTrndiNativeWindows;
{$ELSEIF DEFINED(X_MAC)}
TTrndiNativeMac = trndi.native.mac.TTrndiNativeMac;
TrndiNative = TTrndiNativeMac;
{$ELSEIF DEFINED(HAIKU)}
TTrndiNativeHaiku = trndi.native.haiku.TTrndiNativeHaiku;
TrndiNative = TTrndiNativeHaiku;
{$ELSEIF DEFINED(BSD)}
TTrndiNativeBSD = trndi.native.bsd.TTrndiNativeBSD;
TrndiNative = TTrndiNativeBSD;
{$ELSE}
TTrndiNativeLinux = trndi.native.linux.TTrndiNativeLinux;
TrndiNative = TTrndiNativeLinux;
{$ENDIF}
  // Tip: Use TrndiNative in your code, not platform-specific class names.
  // Example: native := TrndiNative.create('UA', 'https://...');

// Convenience: expose a simple helper that returns the current window manager
// name via the platform class. This keeps existing call-sites that expect a
// plain function working.
function GetWindowManagerName: string;

// Convenience: expose a helper returning the TTS software name used by the
// current platform (e.g., 'spd-say', 'SAPI', 'say').
function GetSpeakSoftwareName: string;
implementation

function GetWindowManagerName: string;
begin
  Result := TrndiNative.GetWindowManagerName;
end;

function GetSpeakSoftwareName: string;
begin
  Result := TrndiNative.SpeakSoftwareName;
end;

end.
