
(*
 * This file is part of Trndi (https://github.com/xxxx or http://xxx.github.io).
 * Copyright (c) 2021 Björn Lindh.
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

{$mode objfpc}{$H+}
{$ifdef darwin}
  {$modeswitch objectivec2}
{$endif}

{$IF DEFINED(WINDOWS)}
  {$DEFINE X_WIN}// Windows
  {$IFDEF CPU32}{$DEFINE X_WIN32}{$ENDIF}
  {$IFDEF CPU64}{$DEFINE X_WIN64}{$ENDIF}
{$ELSEIF DEFINED(UNIX)}
  {$DEFINE X_POSIX}
// All Unix

  {$IFDEF DARWIN}
    {$DEFINE X_MAC}
// macOS
    {$IFDEF CPU32}{$DEFINE X_MAC32}{$ENDIF}
    {$IFDEF CPU64}{$DEFINE X_MAC64}{$ENDIF}
// Not tested on ARM!
    {$IFDEF CPUX86_64}{$DEFINE X_MACX64}{$ENDIF}
    {$IFDEF CPUARM}{$DEFINE X_MACAPPLE}{$ENDIF}
  {$ELSE}
    {$DEFINE X_PC}
// Linux and BSD
    {$IFDEF CPU32}{$DEFINE X_PC32}{$ENDIF}
    {$IFDEF CPU64}{$DEFINE X_PC64}{$ENDIF}
// Not tested on ARM!
    {$IFDEF CPU386}{$DEFINE X_PCX32}{$ENDIF}
    {$IFDEF CPUX86_64}{$DEFINE X_PCX64}{$ENDIF}
    {$IFDEF CPUARM}{$DEFINE X_PCARM}{$ENDIF}
    {$IFDEF CPUPOWERPC32}{$DEFINE X_PCPPC32}{$ENDIF}
    {$IFDEF CPUPOWERPC64}{$DEFINE X_PCPPC64}{$ENDIF}
  {$ENDIF}
{$ELSE}
  {$Error Missing platoform suppport}
{$ENDIF}

{$IFDEF LINUX}
{$DEFINE X_LINUX}
   {$ENDIF}
