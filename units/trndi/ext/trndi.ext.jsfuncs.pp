
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
// Handle JS functions

unit trndi.ext.jsfuncs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.ext.functions, trndi.types, slicke.ux.alert,
  trndi.api, trndi.ext.engine, trndi.ext.perm,
  Trndi.Native, fpjson,
  Dialogs;

type
  TJSFuncs = class(TObject)
  public
    function httpRequest(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function jsonget(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function bgDump(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function setLimits(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function querySvc(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;
    function runCMD(ctx: pointer; const func: string; const params: JSParameters;
      out res: JSValueVal): boolean;

    constructor Create(cgm: TrndiAPI);

    {** Register the gated promise-style helpers (httpRequest/jsonGet/runCMD/
        setLimits/setLevelColor/querySvc) and the fetch()/asyncGet/asyncPost JS
        shims into the engine's current registration context. Must be called
        inside a TTrndiExtEngine.BeginRegistration/EndRegistration block so
        each call is filtered against that extension's grants. }
    procedure RegisterForCurrent;

  private
    tapi: TrndiAPI;
    procedure ShowMsg(const str: string);
  end;

implementation

// Create this function JS handler. With Path B per-extension contexts, this no
// longer registers anything globally; promises are registered per-extension via
// RegisterForCurrent during LoadExtensions.
constructor TJSFuncs.Create(cgm: TrndiAPI);
begin
  tapi := cgm;
end;

const
  {** JS-side fetch() shim. Wraps the threaded httpRequest promise (registered
      below) in a WHATWG-fetch-shaped API: fetch(url, {method, headers, body,
      timeout}) resolves to a Response-like object with status/ok/url/headers/
      text()/json(). The non-standard timeout option (milliseconds) rejects the
      promise with a TypeError containing 'timeout' when the server does not
      answer in time. Header lookup is case-insensitive since servers may send
      lowercase names (HTTP/2). Bodies are buffered — no streaming,
      AbortController or binary. Also defines asyncGet/asyncPost as thin
      aliases over httpRequest so the legacy promises keep working (and gain
      the non-blocking path) without a native backend. }
FETCH_SHIM: string =
  '(function () {' + LineEnding +
  '  if (typeof httpRequest !== "function" || typeof globalThis.fetch === "function")' + LineEnding +
  '    return;' + LineEnding +
  '  function makeHeaders(obj) {' + LineEnding +
  '    var lower = {};' + LineEnding +
  '    for (var k in obj) lower[k.toLowerCase()] = obj[k];' + LineEnding +
  '    return {' + LineEnding +
  '      get: function (n) { var v = lower[String(n).toLowerCase()]; return v === undefined ? null : v; },' + LineEnding +
  '      has: function (n) { return String(n).toLowerCase() in lower; },' + LineEnding +
  '      keys: function () { return Object.keys(lower); }' + LineEnding +
  '    };' + LineEnding +
  '  }' + LineEnding +
  '  globalThis.fetch = function (url, init) {' + LineEnding +
  '    init = init || {};' + LineEnding +
  '    var method = String(init.method || "GET").toUpperCase();' + LineEnding +
  '    var body = (init.body === undefined || init.body === null) ? "" : String(init.body);' + LineEnding +
  '    var timeout = 0;' + LineEnding +
  '    if (init.timeout !== undefined) {' + LineEnding +
  '      timeout = Number(init.timeout);' + LineEnding +
  '      if (!isFinite(timeout) || timeout < 0)' + LineEnding +
  '        throw new TypeError("fetch: timeout must be a non-negative number of milliseconds");' + LineEnding +
  '      timeout = Math.floor(timeout);' + LineEnding +
  '    }' + LineEnding +
  '    var hdrs = {}, hasCT = false;' + LineEnding +
  '    if (init.headers)' + LineEnding +
  '      for (var k in init.headers) {' + LineEnding +
  '        hdrs[k] = String(init.headers[k]);' + LineEnding +
  '        if (k.toLowerCase() === "content-type") hasCT = true;' + LineEnding +
  '      }' + LineEnding +
  '    if (body !== "" && !hasCT) hdrs["Content-Type"] = "text/plain;charset=UTF-8";' + LineEnding +
  '    return httpRequest(method, String(url), JSON.stringify(hdrs), body, timeout).then(' + LineEnding +
  '      function (raw) {' + LineEnding +
  '        var r = JSON.parse(raw);' + LineEnding +
  '        return {' + LineEnding +
  '          status: r.status,' + LineEnding +
  '          ok: r.status >= 200 && r.status < 300,' + LineEnding +
  '          url: r.url,' + LineEnding +
  '          redirected: r.redirected,' + LineEnding +
  '          headers: makeHeaders(r.headers),' + LineEnding +
  '          text: function () { return Promise.resolve(r.body); },' + LineEnding +
  '          json: function () {' + LineEnding +
  '            try { return Promise.resolve(JSON.parse(r.body)); }' + LineEnding +
  '            catch (e) { return Promise.reject(e); }' + LineEnding +
  '          }' + LineEnding +
  '        };' + LineEnding +
  '      },' + LineEnding +
  '      function (err) { throw new TypeError(String(err)); }' + LineEnding +
  '    );' + LineEnding +
  '  };' + LineEnding +
  '  globalThis.asyncGet = function (url) {' + LineEnding +
  '    return httpRequest("GET", String(url), "{}", "").then(' + LineEnding +
  '      function (raw) { return JSON.parse(raw).body; },' + LineEnding +
  '      function () { throw "Cannot fetch URL " + url; }' + LineEnding +
  '    );' + LineEnding +
  '  };' + LineEnding +
  '  globalThis.asyncPost = function (url, body, contentType) {' + LineEnding +
  '    var h = {};' + LineEnding +
  '    if (contentType === undefined) h["Content-Type"] = "application/json";' + LineEnding +
  '    else if (contentType !== "") h["Content-Type"] = String(contentType);' + LineEnding +
  '    return httpRequest("POST", String(url), JSON.stringify(h),' + LineEnding +
  '      (body === undefined || body === null) ? "" : String(body)).then(' + LineEnding +
  '      function (raw) { return JSON.parse(raw).body; },' + LineEnding +
  '      function (err) { throw "Cannot POST to URL " + url + ": " + String(err); }' + LineEnding +
  '    );' + LineEnding +
  '  };' + LineEnding +
  '})();';

procedure TJSFuncs.RegisterForCurrent;
begin
  with TTrndiExtEngine.Instance do
  begin
    AddPromiseIf(epNet,      'jsonGet',       JSCallbackFunction(@jsonGet), 2);
    // Threaded: the HTTP round-trip runs on the promise worker thread so the
    // UI never blocks. httpRequest must therefore stay free of UI/JS calls.
    AddPromiseIf(epNet,      'httpRequest',   JSCallbackFunction(@httpRequest), 4, 5, true);
    AddPromiseIf(epExec,     'runCMD',        JSCallbackFunction(@runCMD));
    AddPromiseIf(epData,     'querySvc',      JSCallbackFunction(@querySvc));
    AddPromiseIf(epSettings, 'setLimits',     JSCallbackFunction(@setLimits), 2, 5);
    AddPromiseIf(epUI,       'setLevelColor', JSCallbackFunction(@setLimits), 3, 6);

    if CanRegister(epNet) then
      ExecuteInCurrent(FETCH_SHIM, '<fetch shim>');
  end;
end;

procedure TJSFuncs.ShowMsg(const str: string);
begin
  ExtLog(sdsAuto, 'Message from Extension', 'An extension triggered a message',
    str, uxmtSquare);
end;

// Blood Glucose dump, from JS.
// Not yet implemented. The previous body issued a synchronous getReadings
// call on the main thread and discarded the result, which blocked the UI
// during the HTTP round-trip without returning anything to JS. A proper
// implementation needs a deferred-resolve path in the JS engine so the
// fetch can run on a TGlucoseFetchThread-style worker and resolve the
// promise from ApplyResult.
function TJSFuncs.bgDump(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
begin
  ShowMsg('bgDump is not yet implemented');
  Result := False;
end;

// Native backend for the JS fetch() shim. Registered with threaded=true, so it
// executes on the TJSAsyncTask worker thread: no UI, no JS context, no shared
// engine state may be touched here. params: (method, url, headersJson, body
// [, timeoutMs]) — timeoutMs 0/absent means no explicit timeout (the
// transport's own limits still apply). Resolves with a JSON envelope
// {status, url, redirected, headers, body}; returns false (= promise
// rejection) on transport failure, timeout or bad arguments.
function TJSFuncs.httpRequest(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
var
  method, url, hjson, body, hline, hname, hvalue: string;
  hdrs: TStringList;
  net: TrndiNative;
  resp: THTTPResponse;
  envelope, hobj: TJSONObject;
  jd: TJSONData;
  i, p, hidx: integer;
  timeoutMs: Int64;
  timedOut: boolean;
begin
  Result := false;

  for i := 0 to 3 do
    if not params[i]^.match(JD_STR) then
    begin
      res := StringToValueVal(Format('fetch: argument %d must be a string', [i]));
      Exit;
    end;

  timeoutMs := 0;
  if params.Count > 4 then
  begin
    if not (params[4]^.match(JD_INT) or params[4]^.match(JD_F64)) then
    begin
      res := StringToValueVal('fetch: timeout must be a number');
      Exit;
    end;
    timeoutMs := round(params[4]^.floatify);
    if timeoutMs < 0 then
      timeoutMs := 0;
    if timeoutMs > 86400000 then // cap at 24h; RequestExWait takes a cardinal
      timeoutMs := 86400000;
  end;

  method := UpperCase(params[0]^.Data.StrVal);
  url := params[1]^.Data.StrVal;
  hjson := params[2]^.Data.StrVal;
  body := params[3]^.Data.StrVal;

  if (method <> 'GET') and (method <> 'POST') then
  begin
    res := StringToValueVal('fetch: only GET and POST are supported');
    Exit;
  end;

  hdrs := TStringList.Create;
  try
    if (hjson <> '') and (hjson <> '{}') then
    try
      jd := GetJSON(hjson);
      try
        if jd is TJSONObject then
          with TJSONObject(jd) do
            for i := 0 to Count - 1 do
              hdrs.Add(Names[i] + '=' + Items[i].AsString);
      finally
        jd.Free;
      end;
    except
      res := StringToValueVal('fetch: invalid headers');
      Exit;
    end;

    net := TrndiNative.Create;
    try
      if timeoutMs > 0 then
        // Short abandon-grace so the promise rejects close to the requested
        // timeout instead of timeout+5s.
        resp := net.RequestExWait(method = 'POST', url, [], body, nil, true, 10,
          hdrs, false, cardinal(timeoutMs), 250)
      else
        resp := net.requestEx(method = 'POST', url, [], body, nil, true, 10,
          hdrs, false);
    finally
      // On timeout RequestExWait may have abandoned a worker thread that is
      // still inside net.requestEx; freeing net under it would be a
      // use-after-free, so the instance is deliberately leaked in that case.
      timedOut := (not resp.Success) and (resp.ErrorMessage = 'timeout');
      if not timedOut then
        net.Free;
    end;
  finally
    hdrs.Free;
  end;

  try
    if not resp.Success then
    begin
      res := StringToValueVal('fetch: ' + resp.ErrorMessage);
      Exit;
    end;

    envelope := TJSONObject.Create;
    try
      hobj := TJSONObject.Create;
      envelope.Add('headers', hobj); // envelope owns hobj
      // resp.Headers holds raw 'Name: value' lines (plus the status line),
      // so split on the first colon — TStringList.Names expects 'Name=value'
      // and returned '' (or garbage when the value contained '=')
      if Assigned(resp.Headers) then
        for i := 0 to resp.Headers.Count - 1 do
        begin
          hline := resp.Headers[i];
          p := Pos(':', hline);
          if p <= 1 then // no name; also skips the 'HTTP/1.1 200 OK' line
            Continue;
          hname := Trim(Copy(hline, 1, p - 1));
          hvalue := Trim(Copy(hline, p + 1, MaxInt));
          if hname = '' then
            Continue;
          hidx := hobj.IndexOfName(hname, true);
          if hidx >= 0 then // duplicates combine per the fetch spec
            hobj.Items[hidx].AsString := hobj.Items[hidx].AsString + ', ' + hvalue
          else
            hobj.Add(hname, hvalue);
        end;
      envelope.Add('status', resp.StatusCode);
      envelope.Add('url', resp.FinalURL);
      envelope.Add('redirected', resp.RedirectCount > 0);
      envelope.Add('body', resp.Body);
      res := StringToValueVal(envelope.AsJSON);
    finally
      envelope.Free;
    end;
    Result := true;
  finally
    resp.Headers.Free;
    resp.Cookies.Free;
  end;
end;

function TJSFuncs.jsonget(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
var
  s, r: string;
  v, v2: JSValueVal;
  jsonData, jval: TJSONData;
begin
  v := params[0]^;
  v2 := params[1]^;

  // URL must be a string (param 0) and path must be a string (param 1)
  if not v.mustbe(JD_STR, func, 0) then
  begin
    Result := False;
    r := 'Wrong data type for URL';
    v := StringToValueVal(r);
    res := v;
    Exit(False);
  end;
  if not v2.mustbe(JD_STR, func, 1) then
  begin
    Result := False;
    r := 'Wrong data type for JSON path';
    v := StringToValueVal(r);
    res := v;
    Exit(False);
  end;

  if not TrndiNative.getURL(v.Data.StrVal, s) then
  begin
    Result := False;
    r := 'Cannot fetch URL ' + v.Data.StrVal;
    v := StringToValueVal(r);
    res := v;
    Exit(False);
  end;

  // Try to parse JSON and find the path. Uses fpjson.GetJSON and FindPath
  jsonData := nil;
  try
    jsonData := GetJSON(s);
    jval := jsonData.FindPath(v2.Data.StrVal); // path like "a.b[0].c"
    if jval = nil then
    begin
      Result := False;
      r := 'JSON path not found: ' + v2.Data.StrVal;
    end
    else
    begin
      // Prefer native string for JSON strings, otherwise return JSON text for objects/numbers/arrays
      if jval.JSONType = jtString then
        r := jval.AsString
      else
        r := jval.AsJSON;
      Result := True;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      r := 'JSON parse error: ' + E.Message;
    end;
  end;
  if Assigned(jsonData) then
    jsonData.Free;

  v := StringToValueVal(r);
  res := v;
end;

// Set high/low values
function TJSFuncs.setLimits(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
var
  v: JSValueVal; // Return data
  times: single; // Unit multiplier
  f: boolean;
begin
  // Values has to be int and might have a bool
  if checkJSParams(params, [JD_INT, JD_INT, JD_INT, JD_INT], [JD_INT, JD_INT]) =
    JS_PARAM_OK then
  begin
    times := 1;
    f := False;
  end
  else
  if checkJSParams(params, [JD_F64, JD_F64, JD_F64, JD_F64], [JD_F64, JD_F64]) =
    JS_PARAM_OK then
  begin
    times := 18.0182;
    f := True;
  end
  else
  begin
    Result := False;
    res.Data.Int32Val := -1;
    Exit(False);
  end;

  tapi.cgmLo := round(params[0]^.floatify * times);
  tapi.cgmHi := round(params[1]^.floatify * times);
  if params.Count = 4 then
  begin
    tapi.cgmRangeLo := round(params[2]^.floatify * times);
    tapi.cgmRangeHi := round(params[3]^.floatify * times);
  end;


  v := IntToValueVal(tapi.cgmHi);
  res := v;
  Result := True;
end;

// Query the backend via JS
// Not yet implemented: reject the promise with a clear message rather than
// leaving Result/res uninitialized (which resolved with stack garbage).
function TJSFuncs.querySvc(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
begin
  res := StringToValueVal('querySvc is not implemented');
  Result := false;
end;

// Query the backend via JS
function TJSFuncs.runCMD(ctx: pointer; const func: string;
  const params: JSParameters; out res: JSValueVal): boolean;
begin
  if checkJSParams(params, [JD_STR], [JD_STR, JD_STR, JD_STR]) <> JS_PARAM_OK then
  begin
    Result := False;
    res.Data.Int32Val := -1;
    Exit(False);
  end;

  if params.Count = 2 then
    res.Data.Int32Val := ExecuteProcess(params[0]^.stringify, [])
  else
    res.Data.Int32Val := ExecuteProcess(params[0]^.stringify,
      params[1]^.stringify.Split(params[2]^.stringify));

  Result := True;
end;

end.
