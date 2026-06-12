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
// Build the (curr, mgdl, mmol) JS array fresh inside @code(ctx). Used by the
// broadcast callback path so each per-extension context gets its own value.
function buildBGReadingsIn(ctx: JSContext): JSValueRaw;
var
  bgnow: BGReading;
  mmol, curr: glucose;
  mgdl: integer;
  arr: JSValueRaw;
  tmp: JSValue;
begin
  bgnow := fBG.lastReading;
  curr := bgnow.convert(un);
  mmol := bgnow.convert(BGUnit.mmol);
  mgdl := round(bgnow.convert(BGUnit.mgdl));

  arr := JS_NewArray(ctx);
  tmp.FromFloat(curr);
  JS_SetPropertyUint32(ctx, arr, 0, tmp.Raw);
  tmp.From32(mgdl);
  JS_SetPropertyUint32(ctx, arr, 1, tmp.Raw);
  tmp.FromFloat(mmol);
  JS_SetPropertyUint32(ctx, arr, 2, tmp.Raw);
  Result := arr;
end;

// Marshal an additional params array-of-const into JSValueRaw values bound to ctx.
procedure MarshalAdditionalParamsIn(ctx: JSContext;
  const additionalParams: array of const; var outArr: array of JSValueRaw);
var
  i: integer;
  s: RawUtf8;
  tmpv: JSValue;
begin
  for i := 0 to High(additionalParams) do
    case additionalParams[i].VType of
    vtInteger:
    begin
      tmpv.From32(additionalParams[i].VInteger);
      outArr[i] := tmpv.Raw;
    end;
    vtInt64:
    begin
      tmpv.From64(additionalParams[i].VInt64^);
      outArr[i] := tmpv.Raw;
    end;
    vtExtended:
    begin
      tmpv.FromFloat(additionalParams[i].VExtended^);
      outArr[i] := tmpv.Raw;
    end;
    vtBoolean:
    begin
      tmpv.From(additionalParams[i].VBoolean);
      outArr[i] := tmpv.Raw;
    end;
    vtChar:
    begin
      s := RawUtf8(additionalParams[i].VChar);
      outArr[i] := JS_NewString(ctx, pchar(s));
    end;
    vtPChar:
      outArr[i] := JS_NewString(ctx, additionalParams[i].VPChar);
    vtAnsiString:
    begin
      s := RawUtf8(ansistring(additionalParams[i].VAnsiString));
      outArr[i] := JS_NewString(ctx, pchar(s));
    end;
    vtUnicodeString, vtWideString:
    begin
      s := RawUtf8(unicodestring(additionalParams[i].VUnicodeString));
      outArr[i] := JS_NewString(ctx, pchar(s));
    end;
    vtString:
    begin
      s := RawUtf8(shortstring(additionalParams[i].VString^));
      outArr[i] := JS_NewString(ctx, pchar(s));
    end;
    else
      outArr[i] := JS_NewString(ctx, '{unsupported}');
    end;
end;

// Helper function that calls a JS function with additional parameters first
// and BG readings appended at the end: functionInJS(param1, param2, bgreadings).
// With Path B isolation, this broadcasts across every loaded extension context,
// re-marshalling args (including the readings JS array) inside each ctx so the
// values are heap-correct.
function callFuncWithBGReadings(const funcName: string;
  const additionalParams: array of const; out exists: boolean): string;
var
  i, j, n: integer;
  ext: PExtContextInfo;
  ctx: JSContext;
  args: array of JSValueRaw;
  globalObj, funcObj, retVal: JSValueRaw;
  cstr: pansichar;
  partial: string;
begin
  result := '';
  exists := false;

  if not Assigned(TTrndiExtEngine.Instance) then Exit;

  n := TTrndiExtEngine.Instance.ExtensionCount;
  for i := 0 to n - 1 do
  begin
    ext := TTrndiExtEngine.Instance.ExtensionAt(i);
    if ext = nil then Continue;
    ctx := ext^.Ctx;
    if ctx = nil then Continue;
    if not ContextHasFunction(ctx, funcName) then Continue;
    exists := true;

    SetLength(args, Length(additionalParams) + 1);
    if Length(additionalParams) > 0 then
      MarshalAdditionalParamsIn(ctx, additionalParams, args);
    args[High(args)] := buildBGReadingsIn(ctx);

    globalObj := JS_GetGlobalObject(ctx);
    funcObj := JS_GetPropertyStr(ctx, globalObj, pchar(funcName));
    retVal := JS_UNDEFINED;
    try
      try
        if not JS_IsFunction(ctx, funcObj) then Continue;
        retVal := JS_Call(ctx, funcObj, globalObj, Length(args), @args[0]);
        if not JS_IsError(ctx, retVal) then
        begin
          cstr := JS_ToCString(ctx, retVal);
          if cstr <> nil then
          begin
            partial := cstr;
            JS_FreeCString(ctx, cstr);
            if partial <> '' then result := partial;
          end;
        end;
      except
        on E: Exception do
          ; // continue to next extension; one bad call shouldn't block others
      end;
    finally
      for j := 0 to High(args) do
        ctx^.FreeInlined(args[j]);
      ctx^.FreeInlined(retVal);
      ctx^.FreeInlined(funcObj);
      ctx^.FreeInlined(globalObj);
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
// Per-extension provisioning. Registers gated Trndi.* methods + promise helpers
// into the engine's current registration context. Caller must wrap this in
// BeginRegistration/EndRegistration so addClassFunctionIf writes into the right ctx.
procedure RegisterTrndiFunctionsForCurrent;
begin
  with TTrndiExtEngine.Instance do
  begin
    // Data-read group (baseline; no prompt). Identity/locale + readings + stats.
    addClassFunctionIf(epData, 'getLocale',           ExtFunction(@JSLocale),         0);
    addClassFunctionIf(epData, 'getUnit',             ExtFunction(@JSUnit),           0);
    addClassFunctionIf(epData, 'getCurrentUser',      ExtFunction(@JSActiveUser),     0);
    addClassFunctionIf(epData, 'getCurrentNickname',  ExtFunction(@JSActiveUserNick), 0);
    addClassFunctionIf(epData, 'predictReadings',     ExtFunction(@JSPredictReadings),-1);
    addClassFunctionIf(epData, 'getBuild',            ExtFunction(@JSGetBuild),       -1);
    addClassFunctionIf(epData, 'getCurrentAPI',       ExtFunction(@JSGetAPI),         -1);
    addClassFunctionIf(epData, 'getReading',          ExtFunction(@JSGetReading),     -1);
    addClassFunctionIf(epData, 'getCurrentReading',   ExtFunction(@JSGetCurrentReading), 0);
    addClassFunctionIf(epData, 'getLimits',           ExtFunction(@JSGetLimits),      0);
    addClassFunctionIf(epData, 'getStatistics',       ExtFunction(@JSGetStatistics),  -1);
    addClassFunctionIf(epData, 'getBasalRate',        ExtFunction(@JSGetBasalRate),   0);

    // UI group (baseline; no prompt). Visual + audio + notifications.
    addClassFunctionIf(epUI, 'uxProp',           ExtFunction(@JSUX),               3);
    addClassFunctionIf(epUI, 'setBadgeSize',     ExtFunction(@JSBADGE),           -1);
    addClassFunctionIf(epUI, 'setDotSize',       ExtFunction(@JSDotSize),         -1);
    addClassFunctionIf(epUI, 'setDotAdjust',     ExtFunction(@JSDotAdjust),       -1);
    addClassFunctionIf(epUI, 'setLevelColor',    ExtFunction(@JSLevelColor),      -1);
    addClassFunctionIf(epUI, 'playSound',        ExtFunction(@JSPlay),             1);
    addClassFunctionIf(epUI, 'sayText',          ExtFunction(@JSSay),              1);
    addClassFunctionIf(epUI, 'htmlMsg',          ExtFunction(@JSHTMLMsg),         -1);
    addClassFunctionIf(epUI, 'htmlDlg',          ExtFunction(@JSHTMLDlg),         -1);
    addClassFunctionIf(epUI, 'htmlYesNo',        ExtFunction(@JSHTMLYesNo),       -1);
    addClassFunctionIf(epUI, 'setClockInterval', ExtFunction(@JSvar_ClockInterval),1);
    addClassFunctionIf(epUI, 'attention',        ExtFunction(@JSAttention),        1);

    // Settings group (promptable). Both reads and writes go through here because
    // settings keys (incl. extval.*) carry user-private data.
    addClassFunctionIf(epSettings, 'setTimeAndRange',
      ExtFunction(@JSTimeRange), 2);
    addClassFunctionIf(epSettings, 'setOverrideThresholdMinutes',
      ExtFunction(@JSvar_DATA_FRESHNESS_THRESHOLD_MINUTES), 1);
    addClassFunctionIf(epSettings, 'getSetting', ExtFunction(@JSGetSetting), -1);
    addClassFunctionIf(epSettings, 'setSetting', ExtFunction(@JSSetSetting), -1);

    // Promise-style helpers (net/exec/settings/data, individually gated).
    if Assigned(jsFuncs) then
      jsFuncs.RegisterForCurrent;
  end;
end;

// Tear down every loaded extension context and re-scan the extensions folder.
// The QuickJS runtime stays alive, so this is the cheap "apply my changes"
// path the Settings → Extensions "Reload" button binds to.
procedure TfBG.ReloadExtensions(Sender: TObject);
var
  engine: TTrndiExtEngine;
begin
  // Instance() returns nil during shutdown (IsExtShuttingDown). If we got
  // re-entered via ProcessMessages while UnloadExtensions is draining, treat
  // it as "nothing to do" — the in-flight reload will complete and the user
  // can retry. Don't fall through to LoadExtensions with a nil engine.
  engine := TTrndiExtEngine.Instance;
  if engine = nil then
    Exit;
  engine.UnloadExtensions;
  LoadExtensions;
end;

// Load extension files. Path B: each .js script gets its own JSContext, with
// only the functions allowed by the file's manifest + user-stored grant.
procedure TfBG.LoadExtensions;
var
  exts: TStringList;
  extFile, extdir, stmp: string;
  fileSrc: RawUtf8;
  fileStream: TFileStream;
  ss: TStringStream;
  manifest: TExtManifest;
  extId, hash: string;
  granted: TExtPermSet;
  extInfo: PExtContextInfo;
  nativeRef: TrndiNative;
begin
  // Bail if the engine isn't available (shutdown in progress). Without this,
  // the loop below would invoke NewExtensionContext on a nil Self and crash
  // at the first field access (`if FRuntime = nil then Exit;`).
  if TTrndiExtEngine.Instance = nil then
    Exit;
  if not Assigned(jsFuncs) then
    jsFuncs := TJSfuncs.Create(api);

  extdir := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator;
  ForceDirectoriesUTF8(extdir);
  exts := FindAllFiles(extdir, '*.js', false);

  if Assigned(fSplash) then
    fSplash.incProgress(79, 'Initializing extensions backend..');

  // Safe mode: holding Ctrl during startup skips all extension loading.
  if ssCtrl in GetKeyShiftState then
  begin
    ExtHTML(uxdAuto, RS_SAFEMODE, RS_SAFEMODE_DESC, [mbOK], uxmtInformation, uxscLarge);
    exts.Free;
    Exit;
  end;

  nativeRef := TrndiNative.Create;
  try
    if Assigned(fSplash) then
      fSplash.incProgress(80, 'Loading extensions...');
    for extFile in exts do
    begin
      if Assigned(fSplash) then
        fSplash.incProgress(-1, RS_SPLASH_LOADING + ExtractFileName(extFile));

      // Read once for manifest + hash. Re-read happens inside ExtExecuteFile
      // (cheap relative to the user prompt cost).
      try
        fileStream := TFileStream.Create(extFile, fmOpenRead or fmShareDenyWrite);
        ss := TStringStream.Create;
        try
          ss.CopyFrom(fileStream, fileStream.Size);
          fileSrc := ss.DataString;
        finally
          ss.Free;
          fileStream.Free;
        end;
      except
        on E: Exception do
        begin
          ShowMessage(Format(RS_EXTFAILED, [extFile]));
          Continue;
        end;
      end;

      manifest := ParseExtManifest(string(fileSrc));
      extId := ExtIdFromPath(extFile);
      hash := HashScript(string(fileSrc));

      // User-toggled enable flag (Extensions settings tab). Default true so
      // existing extensions keep loading without an opt-in step.
      if not nativeRef.GetBoolSetting('ext.enabled.' + extId, true) then
        Continue;

      // Ask the user (or load remembered decision) for the promptable groups.
      granted := GrantPermissionsFor(extId, extFile, hash, manifest, nativeRef);
      if granted = [] then
        Continue; // User denied; skip silently this launch.

      // GrantPermissionsFor above shows a modal prompt that pumps messages;
      // the engine may have been released in the meantime (FormClose path).
      // Bail rather than dereference nil inside NewExtensionContext.
      if TTrndiExtEngine.Instance = nil then
        Break;

      // Create the per-extension JSContext with engine-internal baselines provisioned.
      extInfo := TTrndiExtEngine.Instance.NewExtensionContext(
        extId, extFile, hash, manifest.DisplayName, manifest.Author, granted);
      if extInfo = nil then
      begin
        ShowMessage(Format(RS_EXTFAILED, [extFile]));
        Continue;
      end;

      // Register umain-level Trndi.* methods + promises gated by grants.
      TTrndiExtEngine.Instance.BeginRegistration(extInfo);
      try
        RegisterTrndiFunctionsForCurrent;
      finally
        TTrndiExtEngine.Instance.EndRegistration;
      end;

      // Evaluate the script in its own ctx.
      stmp := TTrndiExtEngine.Instance.ExtExecuteFile(extInfo);
      if Pos('Error:', stmp) = 1 then
        {$ifdef debug}
        ExtMessage(uxdAuto, 'Extension Activation Failed!',
          'The ' + ExtractFileNameOnly(extFile) + ' extension was loaded with an error',
          'Output below',
          'Return: ' + stmp + sLineBreak + sLineBreak + 'Log: ' +
            TTrndiExtEngine.Instance.Output, false);
        {$else}
        ShowMessage(Format(RS_EXTFAILED, [extFile]));
        {$endif}
    end;
  finally
    nativeRef.Free;
    exts.Free;
  end;
end;
{$endif}
