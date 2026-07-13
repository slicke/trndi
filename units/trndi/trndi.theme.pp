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

unit trndi.theme;

{$mode objfpc}{$H+}

interface

uses Graphics;

type
  {** Color scheme for the main Trndi window.
      TColor values use Lazarus COLORREF layout: low byte = Red, middle = Green,
      high byte = Blue.  Use RGBToColor() or $00BBGGRR hex literals when
      defining new themes. }
  TTrndiTheme = record
    ColorOk:            TColor; // In-range background
    ColorOkText:        TColor; // In-range foreground text
    ColorHigh:          TColor; // Above upper limit background
    ColorHighText:      TColor; // Above upper limit foreground text
    ColorLow:           TColor; // Below lower limit background
    ColorLowText:       TColor; // Below lower limit foreground text
    ColorRangeHigh:     TColor; // Approaching upper limit (personal-hi) background
    ColorRangeHighText: TColor;
    ColorRangeLow:      TColor; // Approaching lower limit (personal-lo) background
    ColorRangeLowText:  TColor;
  end;

{** Modern flat-design palette — default for new installations. }
function TrndiThemeModern: TTrndiTheme;

{** Original Trndi palette — preserved for reference and reset flows. }
function TrndiThemeClassic: TTrndiTheme;

implementation

function TrndiThemeModern: TTrndiTheme;
begin
  // Flat UI palette: calm, high-contrast, minimal.
  // Hex literals are $00BBGGRR (Lazarus COLORREF byte order).
  Result.ColorOk            := $0060AE27; // Emerald    #27AE60
  Result.ColorOkText        := $000F2204; // Dark green #0F2204 (5.9:1 on emerald; was light green #E8FBE8 at 2.7:1)

  Result.ColorHigh          := $00227EE6; // Pumpkin    #E67E22
  Result.ColorHighText      := $00001B2D; // Very dark brown

  Result.ColorLow           := $002B39C0; // Pomegranate #C0392B
  Result.ColorLowText       := $00EEEBFF; // Very light pink

  Result.ColorRangeHigh     := $00129CF3; // Sunflower  #F39C12
  Result.ColorRangeHighText := $00001B2D; // Very dark brown

  Result.ColorRangeLow      := $00AD448E; // Amethyst   #8E44AD
  Result.ColorRangeLowText  := $00F5E5F3; // Light lavender
end;

function TrndiThemeClassic: TTrndiTheme;
begin
  Result.ColorOk            := $0000DC84;
  Result.ColorOkText        := $00F2FFF2;
  Result.ColorHigh          := $0007DAFF;
  Result.ColorHighText      := $000052FB;
  Result.ColorLow           := $00FFBE0B;
  Result.ColorLowText       := $00FFFEE9;
  Result.ColorRangeHigh     := $0072C9DE;
  Result.ColorRangeHighText := $001C6577;
  Result.ColorRangeLow      := $00A859EE;
  Result.ColorRangeLowText  := $002D074E;
end;

end.
