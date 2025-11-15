(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)
 // Extension loading and helper functions
{$ifdef TrndiExt}
// Converts the current reading to system-default, mgdl and mmol
function getBGResults: JSValueRaw;
var
  bgnow: BGReading;
  mmol, curr: single;
  mgdl: integer;
begin
  bgnow := fBG.lastReading;
  curr := bgnow.convert(un);
  mmol := bgnow.convert(BGUnit.mmol);
  mgdl := round(bgnow.convert(BGUnit.mgdl));
  result := TTrndiExtEngine.Instance.CreateJSArray([curr, mgdl, mmol]);
end;

// Helper function that calls a JS function with additional parameters first
// and BG readings appended at the end: functionInJS(param1, param2, bgreadings)
function callFuncWithBGReadings(const funcName: string; const additionalParams: array of const; out exists: boolean): string;
var
  readings: JSValueRaw;
  allParams, tempParams: array of JSValueRaw;
  i: integer;
begin
  result := '';
  exists := false;
  
  if not Assigned(TTrndiExtEngine.Instance) then 
    Exit;
    
  readings := getBGResults;
  try
    // Build array with additional params first, then BG readings
    SetLength(allParams, Length(additionalParams) + 1);
    
    // Convert additional params to JSValueRaw using the refactored helper
    if Length(additionalParams) > 0 then
    begin
      // Create a temporary array with exact size for the helper function
      SetLength(tempParams, Length(additionalParams));
      ConvertVarRecsToJSValueRaw(additionalParams, tempParams);
      // Copy converted values to the first part of allParams
      for i := 0 to High(additionalParams) do
        allParams[i] := tempParams[i];
    end;
    
    // Add BG readings at the end
    allParams[High(allParams)] := readings;
    
    result := callFuncRaw(funcName, allParams, exists, false);
  except
    on E: Exception do
    begin
      exists := false;
      result := '';
    end;
  end;
end;

// Overload without exists parameter for simpler usage
function callFuncWithBGReadings(const funcName: string; const additionalParams: array of const): string;
var
  exists: boolean;
begin
  result := callFuncWithBGReadings(funcName, additionalParams, exists);
end;
{$endif}

{$ifdef TrndiExt}
// Load extension files
procedure TfBG.LoadExtensions;

var
  exts: TStringList;
  s, extdir: string;
begin
  TTrndiExtEngine.Instance;
  // Creates the class, if it's not already
  jsFuncs := TJSfuncs.Create(api);
  // This is an Object, not a class!
  extdir := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
  // Find extensions folder

  ForceDirectoriesUTF8(extdir);
  // Create the directory if it doesn't exist
  exts := FindAllFiles(extdir, '*.js', false);
  // Find .js files

  fSplash.lInfo.Caption := 'Initializing extensions backend..';
  with TTrndiExtEngine.Instance do
  begin
    fSplash.lInfo.Caption := 'Registering Core functions...';
    addClassFunction('getLocale', ExtFunction(@JSLocale), 0);
    addClassFunction('getUnit', ExtFunction(@JSUnit), 0);
    addClassFunction('getCurrentUser', ExtFunction(@JSActiveUser), 0);
    addClassFunction('getCurrentNickname', ExtFunction(@JSActiveUserNick), 0);
    addClassFunction('setTimeAndRange', ExtFunction(@JSTimeRange), 2);
    addClassFunction('setOverrideThresholdMinutes', ExtFunction(@JSvar_DATA_FRESHNESS_THRESHOLD_MINUTES), 1);
    addClassFunction('predictReadings', ExtFunction(@JSPredictReadings), -1);

    fSplash.lInfo.Caption := 'Registering UX functions...';
    addClassFunction('uxProp', ExtFunction(@JSUX), 3);
    addClassFunction('setBadgeSize', ExtFunction(@JSBADGE), -1);
    addClassFunction('setDotSize', ExtFunction(@JSDotSize), -1);
    addClassFunction('setDotAdjust', ExtFunction(@JSDotAdjust), -1);
    addClassFunction('setLevelColor', ExtFunction(@JSLevelColor), -1);
    addClassFunction('playSound', ExtFunction(@JSPlay), 1);
    addClassFunction('sayText', ExtFunction(@JSSay), 1);
    addClassFunction('htmlMsg', ExtFunction(@JSHTMLMsg), -1);
    addClassFunction('setClockInterval', ExtFunction(@JSvar_ClockInterval), 1);

    // Add the UX modification function, as declared in this file
    if ssCtrl in GetKeyShiftState then
     SHowMessage('Trndi is running in Safe Mode, no extensions will be loaded')
    else begin
      for s in exts do
      begin
        fSplash.lInfo.Caption := RS_SPLASH_LOADING + ExtractFileName(s);
        // Run all found files
        ExecuteFile(s);
      end;
    end;
    exts.Free;
  end;
end;
{$endif}
