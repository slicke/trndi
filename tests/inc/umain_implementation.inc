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
function TfBG.GetValidatedPositionForTests: TrndiPos;
begin
  Result := GetValidatedPosition;
end;

function TfBG.DotsInViewForTests: integer;
begin
  Result := dotsInView;
end;

function TfBG.DotsInViewForTestsFromInfos(const Dots: array of TDotInfo; ParentHeight: Integer): Integer;
begin
  Result := DotsInViewFromInfos(Dots, ParentHeight);
end;

procedure TfBG.SetTrendDotForTests(i: integer; d: TDotControl);
begin
  if (i >= Low(TrendDots)) and (i <= High(TrendDots)) then
    TrendDots[i] := d;
end;

function TfBG.GetTrendDotForTests(i: integer): TDotControl;
begin
  if (i >= Low(TrendDots)) and (i <= High(TrendDots)) then
    Result := TrendDots[i]
  else
    Result := nil;
end;

// Named A-prefixed: a plain ClientWidth parameter shadows TControl's property
// of the same name and FPC rejects the declaration outright.
function TfBG.TrendDotStrideForTests(AClientWidth, AColumnCount: integer): integer;
begin
  Result := TrendDotStride(AClientWidth, AColumnCount);
end;

function TfBG.TrendDotVisibleInStrideForTests(SlotIndex, ActiveDots, Stride: integer): boolean;
begin
  Result := TrendDotVisibleInStride(SlotIndex, ActiveDots, Stride);
end;

function TfBG.DotColorModeFromSettingForTests(Value: integer): TDotColorMode;
begin
  Result := DotColorModeFromSetting(Value);
end;

function TfBG.DotDisplayColorForTests(Identity, Partner, Background,
  ValueColor: TColor; Mode: TDotColorMode): TColor;
begin
  Result := DotDisplayColor(Identity, Partner, Background, ValueColor, Mode);
end;

function TfBG.DotBandPartnerForTests(Identity: TColor): TColor;
begin
  Result := DotBandPartner(Identity);
end;

function TfBG.DotSeparatedFromValueForTests(Candidate, Background,
  ValueColor: TColor): boolean;
begin
  Result := DotSeparatedFromValue(Candidate, Background, ValueColor);
end;

function TfBG.DotOutlineColorForTests(Background: TColor): TColor;
begin
  Result := DotOutlineColor(Background);
end;

procedure TfBG.ProcessCurrentReadingForTests;
begin
  ProcessCurrentReading;
end;

procedure TfBG.UpdateUIBasedOnGlucoseForTests;
begin
  UpdateUIBasedOnGlucose;
end;