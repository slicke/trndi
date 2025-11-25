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

  {$mode objfpc}{$H+}
  {$ifdef darwin}
  {$modeswitch objectivec2}
  {$endif}

  {$IF DEFINED(WINDOWS)}
  {$DEFINE X_WIN}// Windows
  {$IFDEF CPU32}{$DEFINE X_WIN32}{$ENDIF}
  {$IFDEF CPU64}{$DEFINE X_WIN64}{$ENDIF}
  {$IFDEF CPUARM}{$DEFINE X_WINARM}{$ENDIF}
  {$ELSEIF DEFINED(UNIX)}
  {$DEFINE X_POSIX} // All Unix
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

  {$IF DEFINED(FreeBSD) or DEFINED(OpenBSD) or DEFINED(NetBSD)}
   {$DEFINE X_BSD}
  {$ENDIF}

  {$IF DEFINED(X_BSD) OR DEFINED(X_LINUX)}
  {$DEFINE X_LINUXBSD}
  {$ENDIF}
