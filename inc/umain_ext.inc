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
 // Extension loading and helper functions
{$ifdef TrndiExt}
// Converts the current reading to system-default, mgdl and mmol
function getBGResults: JSValueRaw;
var
  bgnow: BGReading;
  mmol, curr: glucose;
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
  extFile, extdir, stmp: string;
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

  fSplash.incProgress(79, 'Initializing extensions backend..');
  with TTrndiExtEngine.Instance do
  begin
    fSplash.incProgress(80, 'Registering Core functions...');
    addClassFunction('getLocale', ExtFunction(@JSLocale), 0);
    addClassFunction('getUnit', ExtFunction(@JSUnit), 0);
    addClassFunction('getCurrentUser', ExtFunction(@JSActiveUser), 0);
    addClassFunction('getCurrentNickname', ExtFunction(@JSActiveUserNick), 0);
    addClassFunction('setTimeAndRange', ExtFunction(@JSTimeRange), 2);
    addClassFunction('setOverrideThresholdMinutes', ExtFunction(@JSvar_DATA_FRESHNESS_THRESHOLD_MINUTES), 1);
    addClassFunction('predictReadings', ExtFunction(@JSPredictReadings), -1);

    fSplash.incProgress(85, 'Registering UX functions...');
    addClassFunction('uxProp', ExtFunction(@JSUX), 3);
    addClassFunction('setBadgeSize', ExtFunction(@JSBADGE), -1);
    addClassFunction('setDotSize', ExtFunction(@JSDotSize), -1);
    addClassFunction('setDotAdjust', ExtFunction(@JSDotAdjust), -1);
    addClassFunction('setLevelColor', ExtFunction(@JSLevelColor), -1);
    addClassFunction('playSound', ExtFunction(@JSPlay), 1);
    addClassFunction('sayText', ExtFunction(@JSSay), 1);
    addClassFunction('htmlMsg', ExtFunction(@JSHTMLMsg), -1);
    addClassFunction('htmlDlg', ExtFunction(@JSHTMLDlg), -1);    
    addClassFunction('htmlYesNo', ExtFunction(@JSHTMLYesNo), -1);    
    addClassFunction('setClockInterval', ExtFunction(@JSvar_ClockInterval), 1);

    addClassFunction('attention', ExtFunction(@JSAttention), 1);
    addClassFunction('getSetting', ExtFunction(@JSGetSetting), -1);
    addClassFunction('setSetting', ExtFunction(@JSSetSetting), -1);
    addClassFunction('getBuild', ExtFunction(@JSGetBuild), -1);
    addClassFunction('getCurrentAPI', ExtFunction(@JSGetAPI), -1);
    addClassFunction('getReading', ExtFunction(@JSGetReading), -1);

    // Add the UX modification function, as declared in this file
    if ssCtrl in GetKeyShiftState then
      ExtHTML(uxdAuto, RS_SAFEMODE, RS_SAFEMODE_DESC, [mbOK], uxmtInformation, uxscLarge)
    else for extFile in exts do
      begin
        fSplash.incProgress(-1, RS_SPLASH_LOADING + ExtractFileName(extFile));
        // Run all found files
        stmp := ExecuteFile(extFile);
        // Only show error if execution actually failed (starts with 'Error:')
        if Pos('Error:', stmp) = 1 then
          {$ifdef debug}
          ExtMessage(uxdAuto,'Extension Activation Failed!','The ' + ExtractFileNameOnly(extFile) + ' extension was loaded with an error', 'Output below', 'Return: ' + stmp + sLineBreak + sLineBreak + 'Log: ' + Output, false);
        {$else}
        ShowMessage(Format(RS_EXTFAILED, [extFile]));
        {$endif}
      end;
    exts.Free;
  end;
end;
{$endif}
