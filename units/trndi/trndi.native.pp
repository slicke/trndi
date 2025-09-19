(*
 * This file is part of Trndi (https://github.com/slicke/trndi or http://xxx.github.io).
 * Copyright (c) 2021-25 Björn Lindh.
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

{
  This file contains platform-native code, written to make Trndi faster and smoother,
  and to minimize the need for 3rd-party libraries. It also provides native features
  such as Windows registry access and OpenSSL on Linux.
}

unit trndi.native;

{$I ../../inc/native.inc}

interface

uses
  trndi.native.base
  {$IF DEFINED(X_WIN)}, trndi.native.win
  {$ELSEIF DEFINED(X_MAC)}, trndi.native.mac
  {$ELSE}, trndi.native.linux
  {$ENDIF}
  ;

type
  // Re-export base types
  TTrndiNativeBase = trndi.native.base.TTrndiNativeBase;
  TWSLVersion      = trndi.native.base.TWSLVersion;
  TTrndiBool       = trndi.native.base.TTrndiBool;
  TWSLInfo         = trndi.native.base.TWSLInfo;

{$IF DEFINED(X_WIN)}
  TTrndiNativeWindows = trndi.native.win.TTrndiNativeWindows;
  TrndiNative = TTrndiNativeWindows;
{$ELSEIF DEFINED(X_MAC)}
  TTrndiNativeMac = trndi.native.mac.TTrndiNativeMac;
  TrndiNative = TTrndiNativeMac;
{$ELSE}
  TTrndiNativeLinux = trndi.native.linux.TTrndiNativeLinux;
  TrndiNative = TTrndiNativeLinux;
{$ENDIF}

implementation

end.

