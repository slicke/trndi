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
unit Spin;

{$mode ObjFPC}{$H+}

interface

uses Controls, Classes;

// Minimal placeholder for Spin used in headless builds.

type
  TSpinEdit = class(TControl)
  private
    FValue: Integer;
    FMinValue: Integer;
    FMaxValue: Integer;
    FDecimalPlaces: Integer;
    FOnChange: TNotifyEvent;
  public
    constructor Create; virtual;
    property Value: Integer read FValue write FValue;
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property DecimalPlaces: Integer read FDecimalPlaces write FDecimalPlaces;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFloatSpinEdit = class(TControl)
  private
    FValue: Double;
    FMinValue: Double;
    FMaxValue: Double;
    FDecimalPlaces: Integer;
    FOnChange: TNotifyEvent;
  public
    constructor Create; virtual;
    property Value: Double read FValue write FValue;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property DecimalPlaces: Integer read FDecimalPlaces write FDecimalPlaces;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;



implementation

constructor TSpinEdit.Create;
begin
  inherited Create;
  FValue := 0;
  FMinValue := 0;
  FMaxValue := 100;
  FDecimalPlaces := 0;
end;

constructor TFloatSpinEdit.Create;
begin
  inherited Create;
  FValue := 0.0;
  FMinValue := 0.0;
  FMaxValue := 100.0;
  FDecimalPlaces := 2;
end;

end.
