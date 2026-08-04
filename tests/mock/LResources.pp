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
unit LResources;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// Minimal placeholder for LResources used in headless builds. Real Lazarus
// resources (see units/trndi/api/carelink_assets.lrs) are compiled-in binary
// data that headless console tests never need to read; LazarusResources.Add
// is a no-op here and TLazarusResourceStream always yields an empty stream.

type
  TLResourceList = class(TObject)
  public
    procedure Add(const Name, ValueType: AnsiString; const Values: array of string);
  end;

  TLazarusResourceStream = class(TMemoryStream)
  public
    constructor Create(const ResName: string; ResType: PChar);
  end;

var
  LazarusResources: TLResourceList;

implementation

procedure TLResourceList.Add(const Name, ValueType: AnsiString; const Values: array of string);
begin
  // no-op: headless test builds don't need the embedded asset bytes
end;

constructor TLazarusResourceStream.Create(const ResName: string; ResType: PChar);
begin
  inherited Create;
end;

initialization
  LazarusResources := TLResourceList.Create;

finalization
  LazarusResources.Free;

end.
