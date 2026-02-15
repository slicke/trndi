
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

unit trndi.api.debug_firstXmissing;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native, trndi.funcs,
fpjson, jsonparser, dateutils, trndi.api.debug, trndi.log;

type
  // Main class
DebugFirstXMissingAPI = class(DebugAPI)
protected
  missing: integer;
  dexcomMode: boolean;
  dexcomKind: string;
  tandemMode: boolean;
  tandemKind: string;
  tandemTargets: array of integer; // offsets (0 = first remaining, 1 = second remaining, ...)
  function getSystemName: string; override;
public
  constructor Create(user, pass: string); override;
  function getReadings(min, maxNum: integer; extras: string; out res: string): BGResults;
    override;
  class function ParamLabel(LabelName: APIParamLabel): string; override;

end; 

implementation

{------------------------------------------------------------------------------
  Constructor
------------------------------------------------------------------------------}
constructor DebugFirstXMissingAPI.Create(user, pass: string);
var
  baseUser, rest: string;
  colonPos, eqPos: integer;
  atPos, pstart, comma, l, v: integer;
  targetsStr, sub, part: string;
begin
  // Support "N" or "N:dexcom" or "N:dexcom=mode" and "N:tandem" or "N:tandem=mode"
  baseUser := user;
  dexcomMode := False;
  dexcomKind := 'default';
  tandemMode := False;
  tandemKind := 'default';
  colonPos := Pos(':', user);
  if colonPos > 0 then
  begin
    baseUser := Copy(user, 1, colonPos - 1);
    rest := AnsiLowerCase(Copy(user, colonPos + 1, Length(user)));
    // dexcom family
    if Pos('dexcom', rest) = 1 then
    begin
      dexcomMode := True;
      eqPos := Pos('=', rest);
      if eqPos > 0 then
        dexcomKind := Copy(rest, eqPos + 1, Length(rest))
      else
        dexcomKind := 'default';
    end
    // tandem family
    else if Pos('tandem', rest) = 1 then
    begin
      tandemMode := True;
      eqPos := Pos('=', rest);
      if eqPos > 0 then
      begin
        tandemKind := Copy(rest, eqPos + 1, Length(rest));
        // Support optional target offsets after an '@', e.g. "tandem=duplicate@1" or "tandem=duplicate@0,1"
        atPos := Pos('@', tandemKind);
        if atPos > 0 then
        begin
          targetsStr := Copy(tandemKind, atPos + 1, Length(tandemKind));
          tandemKind := Copy(tandemKind, 1, atPos - 1);
          // parse comma-separated ints
          SetLength(tandemTargets, 0);
          pstart := 1;
          while pstart <= Length(targetsStr) do
          begin
            sub := Copy(targetsStr, pstart, Length(targetsStr) - pstart + 1);
            comma := Pos(',', sub);
            if comma > 0 then
            begin
              part := Trim(Copy(sub, 1, comma - 1));
              pstart := pstart + comma;
            end
            else
            begin
              part := Trim(sub);
              pstart := Length(targetsStr) + 1;
            end;
            v := StrToIntDef(part, -1);
            if v >= 0 then
            begin
              l := Length(tandemTargets);
              SetLength(tandemTargets, l + 1);
              tandemTargets[l] := v;
            end;
          end;
        end
        else
        begin
          SetLength(tandemTargets, 1);
          tandemTargets[0] := 0; // default: apply to first remaining
        end;
      end
      else
      begin
        tandemKind := 'default';
        SetLength(tandemTargets, 1);
        tandemTargets[0] := 0;
      end;
    end;
  end;
  missing := StrToIntDef(baseUser, 4);
  inherited;
end; 

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function DebugFirstXMissingAPI.getSystemName: string;
begin
  result := 'Debug First X Missing API';
end;

{------------------------------------------------------------------------------
  Generate fake readings over the last 50 minutes at 5-minute intervals,
  with the First X readings missing
------------------------------------------------------------------------------}
function DebugFirstXMissingAPI.getReadings(min, maxNum: integer; extras: string;
out res: string): BGResults;
var
  i: integer;
  temp: BGReading;
  rssiVal, noiseVal, gapMinutes: integer;
  rssiM, noiseM: MaybeInt;
  tIdx, off, idx, rem: integer;
  // helper for processing multiple targets
  offs: array of integer;
  i2, j2, tmp: integer;
begin
  result := inherited getReadings(min, maxNum, extras, res);
  // Clamp requested missing count to available results and ensure non-negative
  if missing <= 0 then
    missing := 0
  else
  if missing > Length(Result) then
    missing := Length(Result);

  // If requested count removes all available results, return an empty array
  if missing <= 0 then
    missing := 0
  else if missing >= Length(Result) then
  begin
    SetLength(Result, 0);
    TrndiDLog(Format('DebugFirstXMissing: Cleared all %d readings', [missing]));
    Exit;
  end
  else
  begin
    // Remove the first N readings by shifting remaining readings to the front
    rem := Length(Result) - missing;
    for i := 0 to rem - 1 do
      Result[i] := Result[i + missing];
    SetLength(Result, rem);

    // Short debug trace to help when testing the provider
    TrndiDLog(Format('DebugFirstXMissing: Removed %d readings; newest remaining at %s', [missing, DateTimeToStr(Result[0].date)]));

    // Optionally inject a Dexcom-like reading into the newest remaining slot
    if dexcomMode then
    begin
      // Only inject if there is a remaining reading to base from
      if Length(Result) > 0 then
      begin

        // Initialize temp as a Dexcom reading and copy numeric values
        temp.Init(mgdl, 'Dexcom');
        temp.update(Result[0].convert(mgdl), BGPrimary, mgdl);

        if dexcomKind = 'missing-delta' then
          // clear delta only
          temp.update(BG_NO_VAL, BGDelta, mgdl)
        else
          temp.update(Result[0].delta, BGDelta, mgdl);

        temp.trend := Result[0].trend;
        temp.level := Result[0].level;

        // Timestamp behavior depends on mode
        if dexcomKind = 'future' then
        begin
          // Make the injected timestamp slightly later than the next reading (not several minutes later)
          if (1) < Length(Result) then
            temp.date := IncSecond(Result[1].date, 30)
          else
            temp.date := IncSecond(Result[0].date, 30);
        end
        else
        begin
          gapMinutes := (missing * 5) + 10;
          temp.date := IncMinute(Result[0].date, gapMinutes);
        end;

        // Copy RSSI/noise if present
        if Result[0].TryGetRSSI(rssiVal) then
        begin
          rssiM.exists := True; rssiM.value := rssiVal;
        end
        else
        begin
          rssiM.exists := False; rssiM.value := 0;
        end;

        if Result[0].TryGetNoise(noiseVal) then
        begin
          noiseM.exists := True; noiseM.value := noiseVal;
        end
        else
        begin
          noiseM.exists := False; noiseM.value := 0;
        end;

        temp.updateEnv('Dexcom', rssiM, noiseM);

        // Replace the earliest remaining reading with the Dexcom-mimic
        Result[0] := temp;
        TrndiDLog(Format('DebugFirstXMissing: Injected Dexcom-mode (%s) at index %d, date=%s', [dexcomKind, 0, DateTimeToStr(temp.date)]));
      end;
    end;

    // Tandem-mimic injection: variants useful to reproduce Tandem-style anomalies
    if tandemMode then
    begin
      if Length(Result) > 0 then
      begin
        // For each requested target offset apply a Tandem-like injection
        // Process offsets in descending order so earlier replacements don't affect later targets
        if Length(tandemTargets) > 0 then
        begin
          SetLength(offs, Length(tandemTargets));
          for tIdx := 0 to High(tandemTargets) do
            offs[tIdx] := tandemTargets[tIdx];
          // simple descending sort
          for i2 := 0 to High(offs) do
            for j2 := 0 to High(offs) - 1 do
              if offs[j2] < offs[j2 + 1] then
              begin
                tmp := offs[j2];
                offs[j2] := offs[j2 + 1];
                offs[j2 + 1] := tmp;
              end;

          for tIdx := 0 to High(offs) do
          begin
            off := offs[tIdx];
            idx := off; // offsets now relative to index 0 (first remaining)
            if (idx >= 0) and (idx < Length(Result)) then
            begin
              // Initialize temp as a Tandem-like reading and copy numeric values
              temp.Init(mgdl, 'Tandem');
              temp.update(Result[idx].convert(mgdl), BGPrimary, mgdl);

              if tandemKind = 'missing-delta' then
                temp.update(BG_NO_VAL, BGDelta, mgdl)
              else
                temp.update(Result[idx].delta, BGDelta, mgdl);

              temp.trend := Result[idx].trend;
              temp.level := Result[idx].level;

              // Timestamp behavior depends on mode
              if tandemKind = 'duplicate' then
              begin
                // Duplicate timestamp should equal the following reading's timestamp when available
                if (idx + 1) < Length(Result) then
                  temp.date := Result[idx+1].date // duplicate timestamp equal to following reading
                else
                  temp.date := Result[idx].date; // fallback to current slot
              end
              else if tandemKind = 'backwards' then
              begin
                // Backwards mode should produce a timestamp earlier than the following reading
                if (idx + 1) < Length(Result) then
                  temp.date := IncSecond(Result[idx+1].date, -1) // slightly earlier than the following reading
                else
                  temp.date := IncMinute(Result[idx].date, -((off * 5) + 1)); // fallback behavior
              end
              else if tandemKind = 'future' then
              begin
                // Make the injected timestamp slightly later than the following reading when possible
                if (idx + 1) < Length(Result) then
                  temp.date := IncSecond(Result[idx+1].date, 30)
                else
                  temp.date := IncSecond(Result[idx].date, 30); // fallback
              end
              else
              begin
                gapMinutes := ((missing + off) * 5) + 10;
                temp.date := IncMinute(Result[idx].date, gapMinutes); // large gap
              end;

              // Copy RSSI/noise if present
              if Result[idx].TryGetRSSI(rssiVal) then
              begin
                rssiM.exists := True; rssiM.value := rssiVal;
              end
              else
              begin
                rssiM.exists := False; rssiM.value := 0;
              end;

              if Result[idx].TryGetNoise(noiseVal) then
              begin
                noiseM.exists := True; noiseM.value := noiseVal;
              end
              else
              begin
                noiseM.exists := False; noiseM.value := 0;
              end;

              temp.updateEnv('Tandem', rssiM, noiseM);

              Result[idx] := temp;
              TrndiDLog(Format('DebugFirstXMissing: Injected Tandem-mode (%s) at index %d, date=%s', [tandemKind, idx, DateTimeToStr(temp.date)]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

class function DebugFirstXMissingAPI.ParamLabel(LabelName: APIParamLabel): string;
begin
  result := inherited ParamLabel(LabelName);
  if LabelName = APLUser then
    Result := 'Readings to remove, from the most recent';
  if LabelName = APLDesc then
    Result := result + sLineBreak + sLineBreak + 'You can enter any amount of readings, in the username field. That amount will be marked as missing (from most recent to oldest)';
  if LabelName = APLDescHTML then
    Result := result + sLineBreak + sLineBreak + 'You can enter <b>any amount</b> of readings, in the <u>username</u> field. That amount will be marked as <i>missing</i> (from most recent to oldest).' +
      sLineBreak + sLineBreak +
      '<b>Modes</b>: append <code>:tandem</code> or <code>:dexcom</code> to inject a backend-like reading into one or more remaining slots. You can specify a mode with <code>=</code>, for example: <code>:tandem=duplicate</code>, <code>:tandem=backwards</code>, <code>:tandem=future</code>, <code>:tandem=missing-delta</code>, or <code>:tandem=gap</code> (default).' +
      sLineBreak + sLineBreak +
      '<b>Targeting</b>: append <code>@</code> and a comma-separated list of offsets to target other remaining slots. Offsets are relative to the first remaining reading (0 = first remaining, 1 = second remaining, ...). Examples: <code>2:tandem=duplicate@1</code> (apply to second remaining) or <code>4:tandem=duplicate@0,1</code> (apply to first and second remaining).' +
      sLineBreak + sLineBreak +
      '<b>Examples</b>: <code>2:tandem=duplicate</code>, <code>3:dexcom=future</code>';
  if LabelName = APLCopyright then
    Result := 'Björn Lindh <github.com/slicke>';
end;

end.
