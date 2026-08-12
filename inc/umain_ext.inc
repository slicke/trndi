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
        // JS_IsException, not JS_IsError: a throw returns the exception marker
        // rather than an Error object. This used to fall into the branch below,
        // stringify the marker to nothing and leave the exception pending on a
        // context this same function calls again on the next reading.
        if JS_IsException(retVal) then
          DumpJSError(ctx)
        // A callback without a return statement hands back undefined; grabbing
        // its C-string turned that into the literal text 'undefined', which the
        // clockView path then painted on the reading label. No value = no
        // contribution to the result.
        else if not (JS_IsUndefined(retVal) or JS_IsNull(retVal)) then
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
        ctx^.FreeInlined(PJSValue(@args[j]));
      ctx^.FreeInlined(PJSValue(@retVal));
      ctx^.FreeInlined(PJSValue(@funcObj));
      ctx^.FreeInlined(PJSValue(@globalObj));
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
const
  { The v2 facade is additive: it delegates to established methods, preserving
    compatibility while giving new extensions a discoverable Trndi.* surface. }
  API_FACADE_PREFIX: RawUtf8 =
    '(function(){var t=Trndi,has=function(n){return typeof globalThis[n]==="function";};' +
    't.api={version:"2.0",extensionId:"';
  API_FACADE_SUFFIX: RawUtf8 =
    { storage must probe the Trndi class, not globalThis: getSetting/setSetting are
      registered via addClassFunctionIf onto the Trndi object. fetch/runCMD are
      genuine globals (AddPromise writes to global scope), so has() is right there. }
    '",permissions:["data","ui","timers"],capabilities:{storage:typeof t.getSetting==="function"&&typeof t.setSetting==="function",network:has("fetch"),exec:has("runCMD")}};' +
    't.permissions={has:function(n){return t.api.permissions.indexOf(String(n))>=0;},require:function(n){if(!this.has(n))throw new Error("Trndi permission not granted: "+n+". Add it to @perms and approve the extension.");return true;}};' +
    'if(t.api.capabilities.network)t.api.permissions.push("net");if(t.api.capabilities.exec)t.api.permissions.push("exec");if(t.api.capabilities.storage)t.api.permissions.push("settings");' +
    { readings: minutes filtering runs facade-side on timestamp_ms so the
      native getReadings keeps its single count-cap parameter. }
    't.data={current:function(){return t.getCurrentReading();},readings:function(o){var r=t.getReadings(o&&o.limit||0);if(o&&o.minutes>0){var c=Date.now()-o.minutes*60000;r=r.filter(function(x){return x.timestamp_ms>=c;});}return r;},limits:function(){return t.getLimits();},statistics:function(o){return t.getStatistics(o&&o.minutes);},predict:function(o){return {values:t.predictReadings(o&&o.count),confidence:t.predictionConfidence()};}};' +
    { Event-listener API over the magic callback globals. Trndi.on installs a
      per-event dispatcher that chains any already-declared global (function
      declarations are hoisted, so script-defined callbacks are seen), lets
      several listeners coexist, and returns the last non-undefined listener
      result so clockView/uxClick return-value semantics keep working. The
      dispatcher is only installed for events actually subscribed to, so a
      mere Trndi.on("reading",...) doesn't make Pascal-side broadcasts think
      unrelated callbacks exist. }
    't.events={reading:"updateCallback",fetch:"fetchCallback",clock:"clockView",dot:"dotClicked",uxclick:"uxClick",unload:"unloadCallback"};var ls={};' +
    't.on=function(n,f){n=String(n);var g=t.events[n];if(!g)throw new Error("Trndi.on: unknown event \""+n+"\"; valid: "+Object.keys(t.events).join(", "));if(typeof f!=="function")throw new TypeError("Trndi.on: listener must be a function");' +
    'if(!ls[n]){ls[n]=[];var prev=globalThis[g];globalThis[g]=function(){var a=arguments,out,i,r;if(typeof prev==="function")out=prev.apply(this,a);for(i=0;i<ls[n].length;i++){try{r=ls[n][i].apply(null,a);if(r!==undefined)out=r;}catch(e){try{console.push("[Trndi.on "+n+"] "+e);}catch(_e){}}}return out;};}' +
    'ls[n].push(f);return f;};' +
    't.off=function(n,f){n=String(n);if(!t.events[n])throw new Error("Trndi.off: unknown event \""+n+"\"; valid: "+Object.keys(t.events).join(", "));var a=ls[n];if(a){var i=a.indexOf(f);if(i>=0)a.splice(i,1);}};' +
    'if(t.api.capabilities.network){var f=globalThis.fetch;t.net={fetch:function(url,init){t.permissions.require("net");return f(url,init);}};delete globalThis.fetch;delete globalThis.httpRequest;}' +
    'if(t.api.capabilities.storage){var p="extval."+t.api.extensionId+".";t.storage={get:function(k){return t.getSetting(p+String(k));},set:function(k,v){t.setSetting(p+String(k),String(v));},remove:function(k){t.setSetting(p+String(k),"");},' +
    { JSON convenience on top of the string backend; getJSON falls back to
      dflt (or null) on missing keys and unparsable values alike. }
    'getJSON:function(k,dflt){var v=this.get(k);if(v===false||v==="")return dflt===undefined?null:dflt;try{return JSON.parse(v);}catch(e){return dflt===undefined?null:dflt;}},setJSON:function(k,v){this.set(k,JSON.stringify(v));}};}' +
    '})();';
var
  facade, extId: RawUtf8;
begin
  with TTrndiExtEngine.Instance do
  begin
    // Data-read group (baseline; no prompt). Identity/locale + readings + stats.
    addClassFunctionIf(epData, 'getLocale',           @JSLocale,         0);
    addClassFunctionIf(epData, 'getUnit',             @JSUnit,           0);
    addClassFunctionIf(epData, 'getCurrentUser',      @JSActiveUser,     0);
    addClassFunctionIf(epData, 'getCurrentNickname',  @JSActiveUserNick, 0);
    addClassFunctionIf(epData, 'predictReadings',     @JSPredictReadings,-1);
    addClassFunctionIf(epData, 'predictionConfidence', @JSPredictionConfidence, 0);
    addClassFunctionIf(epData, 'getBuild',            @JSGetBuild,       -1);
    addClassFunctionIf(epData, 'getCurrentAPI',       @JSGetAPI,         -1);
    addClassFunctionIf(epData, 'getReading',          @JSGetReading,     -1);
    addClassFunctionIf(epData, 'getCurrentReading',   @JSGetCurrentReading, 0);
    addClassFunctionIf(epData, 'getReadings',         @JSGetReadings,    -1);
    addClassFunctionIf(epData, 'getLimits',           @JSGetLimits,      0);
    addClassFunctionIf(epData, 'getStatistics',       @JSGetStatistics,  -1);
    addClassFunctionIf(epData, 'getBasalRate',        @JSGetBasalRate,   0);

    // UI group (baseline; no prompt). Visual + audio + notifications.
    addClassFunctionIf(epUI, 'uxProp',           @JSUX,               3);
    addClassFunctionIf(epUI, 'setBadgeSize',     @JSBADGE,           -1);
    addClassFunctionIf(epUI, 'setDotSize',       @JSDotSize,         -1);
    addClassFunctionIf(epUI, 'setDotAdjust',     @JSDotAdjust,       -1);
    addClassFunctionIf(epUI, 'setLevelColor',    @JSLevelColor,      -1);
    addClassFunctionIf(epUI, 'playSound',        @JSPlay,             1);
    addClassFunctionIf(epUI, 'sayText',          @JSSay,              1);
    addClassFunctionIf(epUI, 'htmlMsg',          @JSHTMLMsg,         -1);
    addClassFunctionIf(epUI, 'htmlDlg',          @JSHTMLDlg,         -1);
    addClassFunctionIf(epUI, 'htmlYesNo',        @JSHTMLYesNo,       -1);
    addClassFunctionIf(epUI, 'setClockInterval', @JSvar_ClockInterval,1);
    addClassFunctionIf(epUI, 'attention',        @JSAttention,       -1);

    // Settings group (promptable). Both reads and writes go through here because
    // settings keys (incl. extval.*) carry user-private data.
    addClassFunctionIf(epSettings, 'setTimeAndRange',
      @JSTimeRange, 2);
    addClassFunctionIf(epSettings, 'setOverrideThresholdMinutes',
      @JSvar_DATA_FRESHNESS_THRESHOLD_MINUTES, 1);
    addClassFunctionIf(epSettings, 'getSetting', @JSGetSetting, -1);
    addClassFunctionIf(epSettings, 'setSetting', @JSSetSetting, -1);

    // Promise-style helpers (net/exec/settings/data, individually gated).
    if Assigned(jsFuncs) then
      jsFuncs.RegisterForCurrent;

    // Install after all gated helpers so metadata reflects this extension's
    // actual grants, not merely what the manifest requested.
    // The id is spliced into a double-quoted JS string literal, so escape the
    // two characters that could break out of it (ids come from file names, so
    // backslashes and quotes are unlikely but not impossible).
    extId := RawUtf8(CurrentRegistrationExtensionId);
    extId := StringReplace(extId, '\', '\\', [rfReplaceAll]);
    extId := StringReplace(extId, '"', '\"', [rfReplaceAll]);
    facade := API_FACADE_PREFIX + extId + API_FACADE_SUFFIX;
    ExecuteInCurrent(facade, '<Trndi API facade>');
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
    SlickeHTMLMsg(sdsAuto, RS_SAFEMODE, RS_SAFEMODE_DESC, [mbOK], uxmtInformation, uxscLarge);
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
      if not manifest.IsValid then
      begin
        ShowMessage('Invalid extension manifest in ' + ExtractFileName(extFile) + ':' +
          sLineBreak + manifest.ErrorMessage);
        Continue;
      end;
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
        SlickeMsgEx(sdsAuto, 'Extension Activation Failed!',
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
