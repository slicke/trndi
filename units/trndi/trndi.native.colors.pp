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

{**
  @abstract(The colour type and default dark-mode probe the native layer builds on.)

  This unit is the single place that knows whether a build has the LCL. It
  exists so that @code(trndi.native.base) — the platform-neutral contract —
  needs no build-variant branching of its own: base imports this unit
  unconditionally and re-exports @link(TColor).

  Two things vary between an LCL build and an LCL-free one (@code(X_CONSOLE)),
  and both are resolved here:

  - @link(TColor): an alias of @code(Graphics.TColor) with the LCL, and an
    identical-range subrange type without it. Because the ranges match, colour
    settings written by a GUI build read back unchanged in a console build.
  - @link(DefaultIsDarkMode): compares the luminance of the LCL system colours,
    which a build with no widgetset has no equivalent for.

  Why a unit and not @code(inc/native.inc): a type declared in an include file
  is a @italic(distinct) type in every unit that includes it, so the colour
  APIs would stop being assignment-compatible across the native layer.

  This is a build-mode split, not a platform one — @code(X_CONSOLE) is set by
  the build, never by @code(inc/native.inc)'s platform dispatch — so it does
  not belong in a platform unit either. Platform units cannot supply it in any
  case: @code(TColor) appears in @code(trndi.native.base)'s own method
  signatures, and they depend on base rather than the other way round.
}

unit trndi.native.colors;

{$I ../../inc/native.inc}

interface

{$ifndef X_CONSOLE}
uses
Graphics;                       // LCL — the real colour type and system colours
{$endif}

type
{$ifdef X_CONSOLE}
  {** Stand-in for @code(Graphics.TColor) in builds without the LCL. Same
      underlying range as the LCL type. }
TColor = -$7FFFFFFF-1..$7FFFFFFF;
{$else}
  {** The LCL colour type, re-exported so consumers need not import Graphics. }
TColor = Graphics.TColor;
{$endif}

{** Cross-platform dark-mode heuristic used as @code(TTrndiNativeBase.isDarkMode)'s
    default. Compares the luminance of the window and window-text system
    colours; platform classes override when they can detect it properly.
    Without a widgetset there are no system colours to compare, so the answer
    is False and console front ends are expected to override (e.g. from
    COLORFGBG or a terminal query). }
function DefaultIsDarkMode: boolean;

implementation

{------------------------------------------------------------------------------
  DefaultIsDarkMode
  -----------------
  Luminance comparison of clWindow against clWindowText. Moved here from
  trndi.native.base so that unit has one unconditional implementation.
 ------------------------------------------------------------------------------}
function DefaultIsDarkMode: boolean;
{$ifndef X_CONSOLE}

function Brightness(C: TColor): double;
  begin
    Result := (Red(C) * 0.3) + (Green(C) * 0.59) + (Blue(C) * 0.11);
  end;

{$endif}
begin
{$ifdef X_CONSOLE}
  Result := false;
{$else}
  Result := (Brightness(ColorToRGB(clWindow)) < Brightness(ColorToRGB(clWindowText)));
{$endif}
end;

end.
