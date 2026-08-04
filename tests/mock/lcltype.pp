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
unit LCLType;

{$mode objfpc}{$H+}

interface

// Minimal stub of LCLType to satisfy headless builds.
// Add symbols as needed; keep tiny to avoid colliding with real LCL.

uses Controls, Types;

type
  // Reuse the shift state definitions from Controls to keep types compatible
  TShiftStateEnum = Controls.TShiftStateEnum;
  TShiftState = Controls.TShiftState;

  TMouseButton = Controls.TMouseButton;

// Virtual key codes (only a few used by the project)
const
  VK_S = 83; // 'S'
  VK_C = 67; // 'C'
  VK_X = 88; // 'X'
  VK_ESCAPE = 27; // Escape key
  crHandPoint = 12; // cursor constant used by umain
  crDefault = 0;
  crNone = 1; // hide cursor for screenshots/headless mode

// Minimal Mouse global used by umain
type
  TMouseRec = record
    CursorPos: TPoint;
  end;

var
  Mouse: TMouseRec;

implementation

end.