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

{** Construct a QuickJS object of the registered Trndi class.
    Creates and returns a new JS object whose class ID is @code(TrndiClassID),
    enabling methods registered on that class to be invoked from JavaScript.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value provided by QuickJS (unused here)
    @param(argc)     Number of JS arguments passed (unused)
    @param(argv)     Pointer to argument array (unused)
    @returns         A new JS object with class @code(TrndiClassID)
}
procedure TrndiConstructor(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
begin
  // Create a new object of the Trndi class
  res^ := JS_NewObjectClass(ctx, TrndiClassID);
end;

{** Log function exposed to JavaScript.
    Concatenates all arguments into UTF-8 and appends them to the engine’s
    output buffer via @code(TTrndiExtEngine.SetOutput). Intended for simple
    console-like logging.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value from JavaScript (unused)
    @param(argc)     Number of arguments to process
    @param(argv)     Array of argument values to stringify
    @returns         @code(JS_UNDEFINED)
}
procedure JSDoLog(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
begin
  TTrndiExtEngine.Instance.SetOutput(ctx, argv, argc - 1);
  res^ := JS_UNDEFINED;
end;

{** Show an informational alert dialog with concatenated text.
    Stringifies all arguments and displays them using the engine’s alert.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value from JavaScript (unused)
    @param(argc)     Number of arguments to display
    @param(argv)     Values to stringify and concatenate
    @returns         @code(JS_UNDEFINED)
}
procedure JSDoAlert(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  i: integer;
  output: rawutf8;
  totalLen: integer;
  p: PAnsiChar;
begin
  res^ := JS_UNDEFINED;
  if argc <= 0 then
  begin
    TTrndiExtEngine.Instance.alert('');
    Exit;
  end;
  totalLen := 0;
  for i := 0 to argc - 1 do
    Inc(totalLen, Length(ctx^.ToUtf8(argv^[i])));
  SetLength(output, totalLen);
  p := PAnsiChar(output);
  for i := 0 to argc - 1 do
  begin
    Move(PAnsiChar(ctx^.ToUtf8(argv^[i]))^, p^, Length(ctx^.ToUtf8(argv^[i])));
    Inc(p, Length(ctx^.ToUtf8(argv^[i])));
  end;
  TTrndiExtEngine.Instance.alert(Output);
end;

{** Confirmation dialog that returns a boolean to JavaScript.
    Builds a message from all arguments and shows a Yes/No dialog. Returns
    @code(JS_TRUE) if Yes was selected, otherwise @code(JS_FALSE).

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value from JavaScript (unused)
    @param(argc)     Number of arguments to include in the prompt
    @param(argv)     Values to stringify and concatenate
    @returns         @code(JS_TRUE) on Yes; @code(JS_FALSE) on No
}
procedure JSDoYesNo(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  i: integer;
  output: rawutf8;
  dialogResult: TModalResult;
  totalLen: integer;
  p: PAnsiChar;
begin
  try
    if argc <= 0 then
    begin
      dialogResult := SlickeDialog(sdsAuto, RS_EXT_MSG, '', '', [mbYes, mbNo], mtConfirmation);
    end
    else
    begin
      totalLen := 0;
      for i := 0 to argc - 1 do
        Inc(totalLen, Length(ctx^.ToUtf8(argv^[i])));
      SetLength(output, totalLen);
      p := PAnsiChar(output);
      for i := 0 to argc - 1 do
      begin
        Move(PAnsiChar(ctx^.ToUtf8(argv^[i]))^, p^, Length(ctx^.ToUtf8(argv^[i])));
        Inc(p, Length(ctx^.ToUtf8(argv^[i])));
      end;
      dialogResult := SlickeDialog(sdsAuto, RS_EXT_MSG, '', output, [mbYes, mbNo], mtConfirmation);
    end;
    
    if dialogResult = mrYes then
      res^ := JS_TRUE
    else
      res^ := JS_FALSE;
  except
    on E: Exception do
    begin
      ExtError(sdsAuto, 'Error in confirm dialog: ' + E.Message);
      res^ := JS_FALSE;
    end;
  end;
end;

{** Prompt (input) dialog returning the user input as a JS string.
    Expects:
      - argv[0]: caption/title
      - argv[1]: description/label
      - argv[2]: default value
    Returns empty string if cancelled.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value from JavaScript (unused)
    @param(argc)     Argument count (expects at least 3)
    @param(argv)     caption, description, default value
    @returns         JS string of user input (or empty on cancel)
}
procedure JSInput(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  i: integer;
  Caption, desc, def: rawutf8;
  response: string;
  x: tmodalresult;
begin
  res^ := JS_UNDEFINED;
  if argc < 3 then
  begin
    ExtError(sdsAuto, 'prompt requires caption, description and default value');
    Exit;
  end;
  Caption := ctx^.ToUtf8(argv^[0]);
  desc := ctx^.ToUtf8(argv^[1]);
  def := ctx^.ToUtf8(argv^[2]);
  response := SlickeInput(sdsAuto, RS_EXT_MSG, Caption, desc, def, x);
  if x = mrCancel then
    response := '';
  res^ := JS_NewString(ctx, pansichar(Response));
end;

{** Combo-box selection dialog returning the selected index (BigInt).
    Expected arguments:
      - argv[0]: caption
      - argv[1]: description
      - argv[2..N]: options to display
    Returns zero-based index of selection as BigInt.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (>= 3 typical)
    @param(argv)     caption, description, followed by options
    @returns         BigInt index of selected option
}
procedure JSCombo(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  i: integer;
  Caption, desc: rawutf8;
  response: int64;
  vals: TStringList;
begin
  if argc < 3 then
  begin
    ExtError(sdsAuto, 'select requires caption, description and at least one option');
    res^ := JS_NewBigInt64(ctx, -1);
    Exit;
  end;
  vals := TStringList.Create;
  for i := 0 to argc - 1 do
    vals.Add(ctx^.ToUtf8(argv^[i]));
  Caption := ctx^.ToUtf8(argv^[0]);
  desc := ctx^.ToUtf8(argv^[1]);
  response := SlickeList(sdsAuto, RS_EXT_MSG, Caption, desc, vals.ToStringArray(2, argc - 1));
  // Ignore the first params
  //  res := vals.strings[response+2];
  vals.Free;
  res^ := JS_NewBigInt64(ctx, response);
end;

{** Fetch a URL and return the response body as a JS string.
    Uses @code(TrndiNative.getURL) to perform a simple GET for argv[0] (the URL).
    Returns @code(JS_UNDEFINED) if no URL argument is supplied.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects at least 1)
    @param(argv)     argv[0] = URL string
    @returns         JS string with the HTTP response body or @code(JS_UNDEFINED)
}
procedure JSFetch(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  URL, body: string;
  response: RawUtf8;
begin
  res^ := JS_UNDEFINED;
  if argc > 0 then
  begin
    url := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0);
    TrndiNative.getURL(url, body);
    response := body;
    res^ := JS_NewString(ctx, pansichar(Response));
  end;
end;

// provide try/catch
(* // try {
  throw new Error("Something went wrong");
} catch (error) {
  handleError(error.message);
}  *)

{** Bridge to handle an error string from JavaScript (try/catch helper).
    Reads the first argument as a message string; current implementation
    does not throw natively—adapt to log or raise as needed.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 1)
    @param(argv)     argv[0] = error message string
    @returns         @code(JS_UNDEFINED)
}
procedure JSHandleError(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  err: string;
begin
  if argc > 0 then
    err := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0);
  res^ := JS_UNDEFINED;
end;

{ Replacements for mORMot's StringFromFile / FileFromString, which went away
  with the rest of the mORMot dependency. Both swallow IO errors and report
  through their result, matching the behaviour the callers below expect. }

function ExtReadFile(const FileName: string): RawUtf8;
var
  fs: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  try
    fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, fs.Size);
      if fs.Size > 0 then
        fs.ReadBuffer(Result[1], fs.Size);
    finally
      fs.Free;
    end;
  except
    Result := '';
  end;
end;

function ExtWriteFile(const Content: RawUtf8; const FileName: string): boolean;
var
  fs: TFileStream;
begin
  Result := false;
  try
    fs := TFileStream.Create(FileName, fmCreate);
    try
      if Length(Content) > 0 then
        fs.WriteBuffer(Content[1], Length(Content));
      Result := true;
    finally
      fs.Free;
    end;
  except
    Result := false;
  end;
end;

// const data = readFile('test.txt');

{** Read a whole file and return its contents as a JS string.
    Expects argv[0] = file name (UTF-8). Returns @code(JS_UNDEFINED) if
    no file name argument is provided.

    @param(ctx)      Pointer to QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 1)
    @param(argv)     argv[0] = file path string
    @returns         JS string with file contents or @code(JS_UNDEFINED)
}
procedure JSReadFile(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  FileName, FileContent: RawUtf8;
begin
  res^ := JS_UNDEFINED;
  if argc > 0 then
  begin
    FileName := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0);
    FileContent := ExtReadFile(UTF8ToString(FileName));
    res^ := JS_NewString(ctx, pansichar(FileContent));
  end;
end;

// writeFile('test.txt', 'Hello from JavaScript!');

{** Write a string to a file; return JS boolean for success.
    Expects:
      - argv[0] = file name (UTF-8)
      - argv[1] = content (UTF-8)
    Returns @code(true) on success, @code(false) on failure, or @code(JS_UNDEFINED)
    if insufficient arguments.

    @param(ctx)      Pointer to QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 2)
    @param(argv)     argv[0]=file name, argv[1]=content
    @returns         JS boolean indicating write success
}
procedure JSWriteFile(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  FileName, FileContent: RawUtf8;
begin
  res^ := JS_UNDEFINED;
  if argc > 1 then
  begin
    FileName := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0);
    FileContent := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 1);
    // Was JS_ToBool, which yields an int rather than a JS value; the result is
    // handed straight back to JavaScript, so build a real boolean value.
    res^ := JS_NewBool(ctx, ExtWriteFile(FileContent, UTF8ToString(FileName)));
  end;
end;

{**
  Promise rejection tracker callback for QuickJS.

  Invoked by the JS engine whenever a Promise is rejected.
  If the rejection is not handled by user code (i.e., no `.catch()` or
  a handler was not attached in time), this tracker displays an alert
  so the issue is visible during development/runtime.

  Parameters:
    @param(ctx)         JS context in which the rejection occurred
    @param(promise)     The Promise object that was rejected
    @param(reason)      The rejection reason (any JS value, typically an Error)
    @param(is_handled)  True if the rejection was (or became) handled; False if unhandled
    @param(opaque)      User data pointer passed when the tracker was registered
}
procedure PromiseRejectionTracker(ctx: JSContext; promise, reason: PJSValue;
is_handled: integer; opaque: Pointer); cdecl;
var
  error_msg: RawUtf8;
  eng: TTrndiExtEngine;
begin
  // The runtime calls this from inside a C frame, so nothing may unwind out of
  // it - and Instance returns nil once shutdown has been signalled, which is
  // exactly when a rejected promise can still be dropped by the runtime's own
  // teardown. Bail out rather than report; there is nowhere left to report to.
  if not TTrndiExtEngine.HasInstance then
    Exit;
  eng := TTrndiExtEngine.Instance;
  if eng = nil then
    Exit;

  try
    // QuickJS fires this at reject time when no handler is attached yet, and
    // again with is_handled=true if a handler arrives later (e.g. .catch() on
    // the next line, or Promise.allSettled/any wrapping an already-rejected
    // promise). Reporting is therefore deferred: note the rejection now and
    // let the job pump alert only the ones still unhandled after the queue
    // drains (FlushPendingRejections).
    if is_handled <> 0 then
    begin
      // Identity used to be a straight cast of the NaN-boxed value; JSValue is a
      // struct now, so key on the payload pointer instead (see JSValueId).
      eng.NoteRejectionHandled(JSValueId(promise^));
      Exit;
    end;

    try
      // Convert the JS 'reason' value to a string for display/logging
      error_msg := JSValueConstToUtf8(ctx, reason^);
    except
      on E: Exception do
        error_msg := '[error retrieving message]';
    end;
    eng.NoteRejection(JSValueId(promise^), string(error_msg));
  except
    // Swallowing is the only option here: the tracker returns void, so there is
    // no way to hand the failure back to JavaScript.
    on E: Exception do
      ;
  end;
end;

{**
  Async entry-point wrapper exposed to JavaScript as a function that returns a Promise.

  This function is registered with JS_NewCFunctionData so that @code(func_data) carries
  the bound Pascal callback name to invoke. When called from JS, it:
  1) Resolves the bound callback metadata (callback pointer, arity constraints).
  2) Parses JS arguments and validates min/max arity.
  3) Creates a new Promise and captures its resolve/reject functions.
  4) Starts an asynchronous task (TJSAsyncTask) which will later resolve/reject the Promise.

  Parameters:
    @param(ctx)        QuickJS context for this invocation
    @param(this_val)   The JavaScript 'this' value (unused here)
    @param(argc)       Number of JS arguments
    @param(argv)       Pointer to the array of JS arguments
    @param(magic)      Optional small integer to specialize behavior (unused here)
    @param(func_data)  Opaque data passed at registration time; here it encodes the callback name

  Returns:
    A newly created Promise object. The promise’s resolve/reject functions are captured in
    @code(funcs) and handed to TJSAsyncTask, which is responsible for eventually calling one
    of them and for freeing/correctly disposing their JS values.

  Notes:
    - @code(JS_NewPromiseCapability) returns the Promise and fills funcs[0]=resolve, funcs[1]=reject.
    - Argument parsing via @code(JSParseParameters) allocates data that must be freed later
      (the async task should do the cleanup once done).
    - If arity validation fails, we currently just log an error and continue; you might prefer
      to reject the Promise immediately in that case.
}
procedure AsyncTask(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  func: string;        // Name of the registered Pascal callback (from magic)
  cb: PJSCallback;     // Pointer to the callback metadata (includes arity and handler)
  params: integer;     // Number of parsed/validated parameters
  funcs: JSDoubleVal;  // Promise resolve/reject function pair captured by QuickJS
  promise: JSValueRaw; // The Promise object to return to JS
  arityMsg: string;    // Error text when the argument count is out of range
  errVal, callRes: JSValueRaw; // Temporaries for the immediate-reject path
begin
  res^ := JS_UNDEFINED;
  // 1) Recover which promise entry point was called. Every one of them shares
  //    this procedure and is distinguished by its registration magic, which
  //    replaces the bound func_data string the previous binding carried.
  func := string(NativeName(magic));
  if func = '' then
    raise Exception.Create('Missing function to run!');

  // 2) Resolve the callback metadata by name; ensure it exists and is valid.
  try
    cb := TTrndiExtEngine.Instance.promise[func];
    if (cb = nil) or (cb^.func = '') then
      raise Exception.Create('Missing function definition!');
  except
    // Normalize all lookup failures to a consistent error message
    raise Exception.Create('Missing function definition!');
  end;

  // 3) Parse/collect JS arguments into the callback’s parameter holder.
  //    NOTE: JSParseParameters likely allocates memory; the async task must free it later.
  cb^.params.values.Data := JSParseParameters(ctx, argc, argv);
  // Free later in TJSAsyncTask
  params := cb^.params.values.Data.Count;

  // 4) Create a Promise and capture its resolving functions.
  //    funcs[0] will be the resolve function, funcs[1] the reject function.
  try
    promise := JS_NewPromiseCapability(ctx, @funcs[0]);
  except
    on E: Exception do
    begin
      // If creation fails, log and bail out. Consider throwing a JS exception instead.
      ExtError(sdsAuto, 'Init failed');
      cb^.params.values.Data.Free;
      cb^.params.values.Data := nil;
      Exit; // returns an undefined/empty JSValueRaw; caller in JS sees undefined
    end;
  end;

  // 5) Validate arity against min/max constraints (unless min=-1 which means “free-form”).
  //    On mismatch the promise is rejected and the task never runs.
  if (not ((params >= cb^.params.min) and (params <= cb^.params.max))) and
    (cb^.params.min > -1) then
  begin
    if cb^.params.min = cb^.params.max then
      arityMsg := Format('[%s] Needs %d parameters, but we got %d',
        [func, cb^.params.min, params])
    else
      arityMsg := Format('[%s] Needs %d - %d parameters, but we got %d',
        [func, cb^.params.min, cb^.params.max, params]);
    ExtError(sdsAuto, arityMsg);

    errVal := JS_NewString(ctx, pansichar(RawUtf8(arityMsg)));
    callRes := JS_Call(ctx, funcs[JprReject], JS_UNDEFINED, 1, @errVal);
    ctx^.FreeInlined(PJSValue(@callRes));
    ctx^.FreeInlined(PJSValue(@errVal));
    ctx^.FreeInlined(PJSValue(@funcs[JprResolve]));
    ctx^.FreeInlined(PJSValue(@funcs[JprReject]));

    cb^.params.values.Data.Free;
    cb^.params.values.Data := nil;
    res^ := promise;
    Exit;
  end;

  // Optional: handle error promises explicitly (currently no-op).
  if JS_IsError(promise) then
  // Consider: js_std_dump_error(ctx); or returning a rejected promise.
// Currently left as a no-op to preserve original behavior.
  ;

  // 6) Kick off the asynchronous work.
  //    The task is responsible for:
  //      - Performing the actual native operation tied to 'func'
  //      - Calling funcs[0] (resolve) or funcs[1] (reject) with a JS value
  //      - Freeing any allocated argument/JS values when done
  TJSAsyncTask.Create(ctx, cb^, @funcs);
  // Avoid further use of params by the engine; ownership transferred to the task
  cb^.params.values.Data := nil;

  // 7) Return the Promise object to JavaScript immediately.
  res^ := promise;
end;

{******************************************************************************
  JavaScript Timer Functions (setTimeout, setInterval, clearTimeout, clearInterval)
  Modified: January 16, 2026 - Added setTimeout/setInterval support
******************************************************************************}

{** Shared implementation for setTimeout/setInterval. Holds a duplicated
    reference to the callback value itself, so anonymous/arrow functions work
    just like named ones, and forwards any extra arguments per the standard
    setTimeout(fn, delay, ...args) signature. The duplicated values are owned
    by the timer record and released in DisposeTimerInfo.

    @param(ctx)        Current QuickJS context
    @param(argc)       Number of arguments (expects at least 1: callback)
    @param(argv)       argv[0] = callback, argv[1] = delay in ms (optional),
                       argv[2..] = extra arguments passed to the callback
    @param(IsInterval) True for setInterval, False for setTimeout
    @returns           Timer ID as a plain JS number (per the standard; also
                       required so clearTimeout's JS_ToInt32 can parse it —
                       BigInt IDs made clearing timers fail)
}
function JSSetTimerImpl(ctx: JSContext; argc: integer; argv: PJSValues;
IsInterval: boolean): JSValueRaw;
var
  Engine: TTrndiExtEngine;
  TimerInfo: PJSTimerInfo;
  DelayMs: Int32;
  Timer: TFPTimer;
  Handler: TJSTimerHandler;
  i: integer;
  idVal: JSValue;
begin
  Result := JS_UNDEFINED;

  if (argc < 1) or IsGlobalShutdown then
    Exit;

  Engine := TTrndiExtEngine.Instance;
  if Engine = nil then
    Exit;

  // Verify first argument is a function
  if not JS_IsFunction(ctx, argv^[0].Raw) then
    Exit;

  // Delay is optional per the standard; missing or unparsable means 0 ms
  DelayMs := 0;
  if argc > 1 then
  begin
    if JS_ToInt32(ctx, @DelayMs, argv^[1].Raw) <> 0 then
      DelayMs := 0;
    if DelayMs < 0 then
      DelayMs := 0;
  end;
  // TFPCustomTimer treats Interval = 0 as "inactive" (see IsActive in fptimer),
  // so a literal 0 ms would arm a timer that never fires - and setTimeout(fn, 0)
  // is the single most common form there is. 0 means "as soon as possible", so
  // round it up to the shortest interval the timer can actually run at.
  if DelayMs < 1 then
    DelayMs := 1;

  // Create timer info; New initializes the managed Args field
  New(TimerInfo);
  Inc(Engine.FNextTimerID);
  TimerInfo^.TimerID := Engine.FNextTimerID;
  // quickjs-ng has no context-free duplicate, so take the reference explicitly.
  TimerInfo^.Callback := JS_DupValue(ctx, argv^[0]);
  if argc > 2 then
  begin
    SetLength(TimerInfo^.Args, argc - 2);
    for i := 2 to argc - 1 do
      TimerInfo^.Args[i - 2] := JS_DupValue(ctx, argv^[i]);
  end;
  TimerInfo^.IsInterval := IsInterval;
  TimerInfo^.Context := ctx;

  // Create timer handler and timer
  Handler := TJSTimerHandler.Create;
  Timer := TFPTimer.Create(nil);
  Timer.Interval := DelayMs;
  Timer.Tag := PtrInt(TimerInfo); // Store timer info in Tag
  Timer.OnTimer := @Handler.OnTimer;
  TimerInfo^.Timer := Timer;
  TimerInfo^.Handler := Handler; // Keep reference to prevent premature freeing

  // Store in map, then arm
  Engine.FJSTimers.Add(TimerInfo^.TimerID, TimerInfo);
  Timer.Enabled := True;

  // Return timer ID as a plain number
  idVal.From64(TimerInfo^.TimerID);
  Result := idVal.Raw;
end;

{** setTimeout(callback, delay, ...args) - Execute callback once after delay
    milliseconds. Returns a timer ID that can be passed to clearTimeout.
}
procedure JSSetTimeout(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
begin
  res^ := JSSetTimerImpl(ctx, argc, argv, false);
end;

{** setInterval(callback, delay, ...args) - Execute callback repeatedly every
    delay milliseconds. Returns a timer ID that can be passed to clearInterval.
}
procedure JSSetInterval(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
begin
  res^ := JSSetTimerImpl(ctx, argc, argv, true);
end;

{** clearTimeout(timerID) / clearInterval(timerID) - Cancel a timer.
    Works for both setTimeout and setInterval timers.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 1: timerID)
    @param(argv)     argv[0] = timer ID to cancel
    @returns         JS_UNDEFINED
}
procedure JSClearTimer(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  Engine: TTrndiExtEngine;
  TimerID: Int32;
  idx: Integer;
  TimerInfo: PJSTimerInfo;
begin
  res^ := JS_UNDEFINED;

  if (argc < 1) or IsGlobalShutdown then
    Exit;

  Engine := TTrndiExtEngine.Instance;
  if Engine = nil then
    Exit;

  // Get timer ID
  if JS_ToInt32(ctx, @TimerID, argv^[0].Raw) <> 0 then
    Exit;

  // Find and remove the timer
  idx := Engine.FJSTimers.IndexOf(TimerID);
  if idx >= 0 then
  begin
    TimerInfo := Engine.FJSTimers.Data[idx];

    // Stop the timer, but defer destruction: clearTimeout/clearInterval may be
    // called from inside the timer's own callback, and freeing it here would
    // destroy objects whose event frame is still on the stack.
    TimerInfo^.Timer.Enabled := False;
    Engine.FJSTimers.Delete(idx);
    Engine.FDeadTimers.Add(TimerInfo);
  end;
end;

var
  // Time origin for performance.now(), captured on first call. All JS runs on
  // the main thread, so the lazy init needs no locking.
PerfOriginUs: Int64 = 0;

{** Native backend for performance.now(): monotonic milliseconds since the
    first call, with microsecond precision (QueryPerformanceCounter on Windows,
    CLOCK_MONOTONIC on POSIX via mORMot).

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (none expected)
    @param(argv)     Unused
    @returns         JS number (float ms)
}
procedure JSPerfNow(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
var
  us: Int64;
  v: JSValue;
begin
  us := TrndiNative.MonotonicMicroseconds;
  if PerfOriginUs = 0 then
    PerfOriginUs := us;
  v.FromFloat((us - PerfOriginUs) / 1000.0);
  res^ := v.Raw;
end;

{** Native backend for the crypto shim: return @code(argv[0]) cryptographically
    secure random bytes as a lowercase hex string, taken from the platform
    entropy source via @code(TrndiNative.GetRandomBytes). Returns
    @code(JS_UNDEFINED) when the count is missing, outside 1..65536 (the
    WebCrypto getRandomValues quota), or when the platform has no secure
    source - never a weaker substitute.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 1)
    @param(argv)     argv[0] = number of bytes
    @returns         JS string of 2×count hex digits, or @code(JS_UNDEFINED)
}
procedure JSRandomHex(res: PJSValue; ctx: JSContext; this_val: PJSValue;
argc: integer; argv: PJSValues; magic: integer);
const
  HEXCHARS: array[0..15] of ansichar = '0123456789abcdef';
var
  n, i: Int32;
  raw: array of byte;
  hex: RawUtf8;
begin
  res^ := JS_UNDEFINED;
  if (argc < 1) or (JS_ToInt32(ctx, @n, argv^[0].Raw) <> 0) then
    Exit;
  if (n < 1) or (n > 65536) then
    Exit;

  SetLength(raw, n);
  // Bail out rather than degrade: callers treat this as secure randomness.
  if not TrndiNative.GetRandomBytes(@raw[0], n) then
    Exit;

  SetLength(hex, n * 2);
  for i := 0 to n - 1 do
  begin
    hex[i * 2 + 1] := HEXCHARS[raw[i] shr 4];
    hex[i * 2 + 2] := HEXCHARS[raw[i] and $0F];
  end;
  FillChar(raw[0], n, 0);

  res^ := JS_NewString(ctx, pansichar(hex));
end;
