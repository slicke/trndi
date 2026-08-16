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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-08-16: Removed a stray '{' that preceded the unit declaration. It
 *   opened a comment that ran to the '}' of the following {$mode ObjFPC},
 *   swallowing the declaration itself.
 *)
unit trndi.ext.types;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses 
fgl, trndi.ext.shared;

type 
  // Specialization for JSValueVal
  generic TJSValList<T> = class(specialize TFPGList<T>)
    public 
      // Method to find an entry based on a specific function
      function FindByFunc(FuncPtr: Pointer): T;
  end;

  // Specialization for JSValueVal
  TJSValListJSValueVal = specialize TJSValList<JSValueVal>;

implementation

{ TJSValList<T> }

function TJSValList<T>.FindByFunc(FuncPtr: Pointer): T;

var 
  i: Integer;
begin
  Result := Default(T);
  // Or another appropriate default value depending on T
  for i := 0 to Self.Count - 1 do
    begin
      // Assume T is JSValueVal
      if (Self[i].data.match = JD_FUNC) and (Self[i].data.Func = FuncPtr) then
        begin
          Result := Self[i];
          Exit;
        end;
    end;
end;

end.                  }
