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
unit ufloat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Menus;

// Minimal mock of the TfFloat form used in the app so tests can run headless.
// Only expose fields and methods that other units/tests reference.

type
  TfFloat = class(TObject)
  public
    // Lightweight placeholders for commonly-used properties
    Showing: Boolean;
    OnHide: TNotifyEvent;
    Color: TColor;
    Font: TFont;
    lVal: TLabel;
    lArrow: TLabel;
    pnMultiUser: TPanel;
    miFontMain: TMenuItem;
    lRangeDown: TLabel;
    lRangeUp: TLabel;
    procedure FormResize(Sender: TObject); virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure SetTrendArrow(AEnabled: boolean; AAngle: single; AColor: TColor); virtual;
    class function Instance: TfFloat; static;
  end;

var
  fFloat: TfFloat = nil;

procedure ShowMessage(const S: string);

implementation

var
  _FloatInstance: TfFloat = nil;

procedure TfFloat.FormResize(Sender: TObject);
begin
  // no-op for headless tests
end;

procedure TfFloat.Show;
begin
  // no-op for headless tests
end;

procedure TfFloat.Hide;
begin
  // no-op for headless tests
end;

procedure TfFloat.SetTrendArrow(AEnabled: boolean; AAngle: single; AColor: TColor);
begin
  // no-op for headless tests
end;

class function TfFloat.Instance: TfFloat;
begin
  if _FloatInstance = nil then
    _FloatInstance := TfFloat.Create;
  Result := _FloatInstance;
end;

procedure ShowMessage(const S: string);
begin
  // no-op in tests
end;

initialization
finalization
  _FloatInstance.Free;

end.
