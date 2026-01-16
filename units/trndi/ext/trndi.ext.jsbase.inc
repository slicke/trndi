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
function TrndiConstructor(ctx: JSContext; this_val: JSValueRaw;
argc: integer; argv: PJSValues): JSValueRaw; cdecl;
begin
  // Create a new object of the Trndi class
  Result := JS_NewObjectClass(ctx, TrndiClassID);
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
function JSDoLog(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  i: integer;
  buffer: TSynTempBuffer;
  res: RawUtf8;
begin
  TTrndiExtEngine.Instance.SetOutput(@ctx, argv, argc - 1);
  Result := JS_UNDEFINED;
end;

{** Show an informational alert dialog with concatenated text.
    Stringifies all arguments and displays them using the engine’s alert.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value from JavaScript (unused)
    @param(argc)     Number of arguments to display
    @param(argv)     Values to stringify and concatenate
    @returns         @code(JS_UNDEFINED)
}
function JSDoAlert(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  i: integer;
  output: rawutf8;
begin
  for i := 0 to argc - 1 do
    output := output + ctx^.ToUtf8(argv^[i]);
  TTrndiExtEngine.Instance.alert(Output);
  Result := JS_UNDEFINED;
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
function JSDoYesNo(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  i: integer;
  output: rawutf8;
begin
  for i := 0 to argc - 1 do
    output := output + ctx^.ToUtf8(argv^[i]);
  if UXDialog(uxdAuto, sExtMsg, '', output, [mbYes, mbNo], mtConfirmation) = mrYes then
    Result := JS_TRUE
  else
    Result := JS_FALSE;
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
function JSInput(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  i: integer;
  Caption, desc, def: rawutf8;
  response: string;
  x: tmodalresult;
begin
  Caption := ctx^.ToUtf8(argv^[0]);
  desc := ctx^.ToUtf8(argv^[1]);
  def := ctx^.ToUtf8(argv^[2]);
  response := ExtInput(uxdAuto, sExtMsg, Caption, desc, def, x);
  if x = mrCancel then
    response := '';
  Result := JS_NewString(ctx, pansichar(Response));
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
function JSCombo(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  i: integer;
  Caption, desc: rawutf8;
  response: int64;
  vals: TStringList;
  res: rawutf8;
begin
  vals := TStringList.Create;
  for i := 0 to argc - 1 do
    vals.Add(ctx^.ToUtf8(argv^[i]));
  Caption := ctx^.ToUtf8(argv^[0]);
  desc := ctx^.ToUtf8(argv^[1]);
  response := ExtList(uxdAuto, sExtMsg, Caption, desc, vals.ToStringArray(2, argc - 1));
  // Ignore the first params
  //  res := vals.strings[response+2];
  vals.Free;
  Result := JS_NewBigInt64(ctx, response);
  //    Result := JS_NewString(ctx, pansichar(res));
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
function JSFetch(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  URL, res: string;
  response: RawUtf8;
begin
  if argc > 0 then
  begin
    url := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0);
    TrndiNative.getURL(url, res);
    response := res;
    Result := JS_NewString(ctx, pansichar(Response));
  end
  else
    Result := JS_UNDEFINED;
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
function JSHandleError(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  err: string;
begin
  if argc > 0 then
    err := TTrndiExtEngine.Instance.ParseArgv(@ctx, argv, 0)
  //    ;                                     ctx^^.ErrorDump(true, nil)IsException
  //  TTrndiExtEngine.Instance.excepion(err,);
  ;
  Result := JS_UNDEFINED;
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
function JSReadFile(ctx: PJSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  FileName, FileContent: RawUtf8;
begin
  if argc > 0 then
  begin
    FileName := TTrndiExtEngine.Instance.ParseArgv(ctx, argv, 0);
    FileContent := StringFromFile(UTF8ToString(FileName));
    Result := JS_NewString(ctx^, pansichar(FileContent));
  end
  else
    Result := JS_UNDEFINED;
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
function JSWriteFile(ctx: PJSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  FileName, FileContent: RawUtf8;
begin
  if argc > 1 then
  begin
    FileName := TTrndiExtEngine.Instance.ParseArgv(ctx, argv, 0);
    FileContent := TTrndiExtEngine.Instance.ParseArgv(ctx, argv, 1);
    if FileFromString(FileContent, UTF8ToString(FileName)) then
      Result := JS_ToBool(ctx^, JS_TRUE)
    else
      Result := JS_ToBool(ctx^, JS_FALSE);
  end
  else
    Result := JS_UNDEFINED;
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
procedure PromiseRejectionTracker(ctx: PJSContext; promise, reason: JSValueConst;
is_handled: longbool; opaque: Pointer); cdecl;
var
  error_msg: RawUtf8;
begin
  // Only act on genuinely unhandled rejections
  if not is_handled then
  try
      // Convert the JS 'reason' value to a string for display/logging
      // JSValueConstToUtf8 expects JSContext (not PJSContext), so we pass ctx^ 
    error_msg := JSValueConstToUtf8(ctx^, reason);

      // Show a helpful alert message. Fall back to a generic message if no text was produced.
    if error_msg <> '' then
      TTrndiExtEngine.Instance.alert('Unhandled Promise rejection: ' +
        string(error_msg))
    else
      TTrndiExtEngine.Instance.alert(
        'Unhandled Promise rejection: [no error message]');
  except
    on E: Exception do
        // If conversion fails completely, show a generic error
      TTrndiExtEngine.Instance.alert(
        'Unhandled Promise rejection: [error retrieving message]');
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
function AsyncTask(ctx: JSContext; this_val: JSValueConst;
argc: integer; argv: PJSValueConst; magic: integer;
func_data: PJSValueRaw): JSValueRaw; cdecl;
var
  func: string;        // Name of the registered Pascal callback (bound via func_data)
  cb: PJSCallback;     // Pointer to the callback metadata (includes arity and handler)
  params: integer;     // Number of parsed/validated parameters
  funcs: JSDoubleVal;  // Promise resolve/reject function pair captured by QuickJS
  promise: JSValueRaw; // The Promise object to return to JS
  res: array[0..0] of JSValue; // Unused (can be removed); placeholder from earlier code
begin
  // 1) Retrieve the bound Pascal callback name from func_data.
  //    This was set when registering the JS function (e.g., in AddPromise).
  func := JSValueConstToUtf8(ctx, func_data^);
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

  // 4) Validate arity against min/max constraints (unless min=-1 which means “free-form”).
  if (not ((params >= cb^.params.min) and (params <= cb^.params.max))) and
    (cb^.params.min > -1) then
    if cb^.params.min = cb^.params.max then
      ExtError(uxdAuto, Format('[%s] Needs %d parameters, but we got %d',
        [func, cb^.params.min, params]))
    else
      ExtError(uxdAuto, Format('[%s] Needs %d - %d parameters, but we got %d',
        [func, cb^.params.min, cb^.params.max, params]))// Inform the user about the mismatch. Consider rejecting the Promise immediately instead.
  ;

  // 5) Create a Promise and capture its resolving functions.
  //    funcs[0] will be the resolve function, funcs[1] the reject function.
  try
    promise := JS_NewPromiseCapability(ctx, @funcs[0]);
  except
    on E: Exception do
    begin
      // If creation fails, log and bail out. Consider throwing a JS exception instead.
      ExtError(uxdAuto, 'Init failed');
      Exit; // returns an undefined/empty JSValueRaw; caller in JS sees undefined
    end;
  end;

  // Optional: handle error promises explicitly (currently no-op).
  if JS_IsError(ctx, promise) then
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
  Result := promise;
end;

{******************************************************************************
  JavaScript Timer Functions (setTimeout, setInterval, clearTimeout, clearInterval)
  Modified: January 16, 2026 - Added setTimeout/setInterval support
******************************************************************************}

{** setTimeout(callback, delay) - Execute callback once after delay milliseconds.
    Returns a timer ID that can be passed to clearTimeout.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 2: callback, delay)
    @param(argv)     argv[0] = callback function, argv[1] = delay in ms
    @returns         Integer timer ID
}
function JSSetTimeout(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  Engine: TTrndiExtEngine;
  TimerInfo: PJSTimerInfo;
  DelayMs: Int32;
  Timer: TFPTimer;
  Handler: TJSTimerHandler;
  FuncName: PAnsiChar;
begin
  Result := JS_UNDEFINED;

  if (argc < 2) or IsGlobalShutdown then
    Exit;

  Engine := TTrndiExtEngine.Instance;
  if Engine = nil then
    Exit;

  // Verify first argument is a function  
  if not JS_IsFunction(ctx, argv^[0].Raw) then
    Exit;

  // Get the function name - it must be a named function, not anonymous
  FuncName := JS_ToCString(ctx, JS_GetPropertyStr(ctx, argv^[0].Raw, 'name'));
  if (FuncName = nil) or (FuncName[0] = #0) then
  begin
    Engine.SetOutput('setTimeout requires a named function, not an anonymous function');
    if FuncName <> nil then
      JS_FreeCString(ctx, FuncName);
    Exit;
  end;

  // Get delay in milliseconds
  if JS_ToInt32(ctx, @DelayMs, argv^[1].Raw) <> 0 then
    DelayMs := 0;
  if DelayMs < 0 then
    DelayMs := 0;

  // Create timer info
  New(TimerInfo);
  Inc(Engine.FNextTimerID);
  TimerInfo^.TimerID := Engine.FNextTimerID;
  TimerInfo^.FunctionName := RawUtf8(FuncName); // Store the function name
  JS_FreeCString(ctx, FuncName);
  TimerInfo^.IsInterval := False;
  TimerInfo^.Context := ctx;

  // Create timer handler and timer
  Handler := TJSTimerHandler.Create;
  Timer := TFPTimer.Create(nil);
  Timer.Interval := DelayMs;
  Timer.Tag := PtrInt(TimerInfo); // Store timer info in Tag
  Timer.OnTimer := @Handler.OnTimer;
  TimerInfo^.Timer := Timer;
  TimerInfo^.Handler := Handler; // Keep reference to prevent premature freeing
  Timer.Enabled := True; // Enable after all setup is complete

  // Store in map
  Engine.FJSTimers.Add(TimerInfo^.TimerID, TimerInfo);
  
  // Debug output
  Engine.SetOutput('setTimeout created: ID=' + IntToStr(TimerInfo^.TimerID) + ' delay=' + IntToStr(DelayMs) + 'ms');

  // Return timer ID as BigInt
  Result := JS_NewBigInt64(ctx, TimerInfo^.TimerID);
end;

{** setInterval(callback, delay) - Execute callback repeatedly every delay milliseconds.
    Returns a timer ID that can be passed to clearInterval.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 2: callback, delay)
    @param(argv)     argv[0] = callback function, argv[1] = delay in ms
    @returns         Integer timer ID
}
function JSSetInterval(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  Engine: TTrndiExtEngine;
  TimerInfo: PJSTimerInfo;
  DelayMs: Int32;
  Timer: TFPTimer;
  Handler: TJSTimerHandler;
  FuncName: PAnsiChar;
begin
  Result := JS_UNDEFINED;

  if (argc < 2) or IsGlobalShutdown then
    Exit;

  Engine := TTrndiExtEngine.Instance;
  if Engine = nil then
    Exit;

  // Verify first argument is a function
  if not JS_IsFunction(ctx, argv^[0].Raw) then
    Exit;

  // Get the function name - it must be a named function, not anonymous
  FuncName := JS_ToCString(ctx, JS_GetPropertyStr(ctx, argv^[0].Raw, 'name'));
  if (FuncName = nil) or (FuncName[0] = #0) then
  begin
    Engine.SetOutput('setInterval requires a named function, not an anonymous function');
    if FuncName <> nil then
      JS_FreeCString(ctx, FuncName);
    Exit;
  end;

  // Get delay in milliseconds
  if JS_ToInt32(ctx, @DelayMs, argv^[1].Raw) <> 0 then
    DelayMs := 0;
  if DelayMs < 0 then
    DelayMs := 0;

  // Create timer info
  New(TimerInfo);
  Inc(Engine.FNextTimerID);
  TimerInfo^.TimerID := Engine.FNextTimerID;
  TimerInfo^.FunctionName := RawUtf8(FuncName); // Store the function name
  JS_FreeCString(ctx, FuncName);
  TimerInfo^.IsInterval := True; // This is an interval timer
  TimerInfo^.Context := ctx;

  // Create timer handler and timer
  Handler := TJSTimerHandler.Create;
  Timer := TFPTimer.Create(nil);
  Timer.Interval := DelayMs;
  Timer.Tag := PtrInt(TimerInfo); // Store timer info in Tag
  Timer.OnTimer := @Handler.OnTimer;
  Timer.Enabled := True;
  TimerInfo^.Timer := Timer;
  TimerInfo^.Handler := Handler; // Keep reference to prevent premature freeing

  // Store in map
  Engine.FJSTimers.Add(TimerInfo^.TimerID, TimerInfo);

  // Return timer ID as BigInt
  Result := JS_NewBigInt64(ctx, TimerInfo^.TimerID);
end;

{** clearTimeout(timerID) / clearInterval(timerID) - Cancel a timer.
    Works for both setTimeout and setInterval timers.

    @param(ctx)      Current QuickJS context
    @param(this_val) The 'this' value (unused)
    @param(argc)     Number of arguments (expects 1: timerID)
    @param(argv)     argv[0] = timer ID to cancel
    @returns         JS_UNDEFINED
}
function JSClearTimer(ctx: JSContext; this_val: JSValueRaw; argc: integer;
argv: PJSValues): JSValueRaw; cdecl;
var
  Engine: TTrndiExtEngine;
  TimerID: Int32;
  idx: Integer;
  TimerInfo: PJSTimerInfo;
begin
  Result := JS_UNDEFINED;

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
    
    // Stop and free timer
    TimerInfo^.Timer.Enabled := False;
    TimerInfo^.Timer.Free;
    TimerInfo^.Handler.Free;
    
    // Free timer info and remove from map
    Dispose(TimerInfo);
    Engine.FJSTimers.Delete(idx);
  end;
end;
