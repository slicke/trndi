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

// JS promise handler
unit trndi.ext.promise;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, trndi.ext.quickjs, Dialogs,
trndi.ext.functions, slicke.ux.alert, fgl, trndi.strings, trndi.native;

type
  {** Pair holding QuickJS resolve/reject functions for a Promise.
      Index mapping used in this unit:
      - index 0 = resolve (see JprResolve)
      - index 1 = reject  (see JprReject) }
JSDoubleVal = array[0..1] of JSValueRaw;

  {** Pointer alias used when passing resolve/reject function slots to tasks. }
PJSDoubleVal = ^JSDoubleVal;

  {** Asynchronous task that bridges native (Pascal) callbacks to JS Promises.
      Workflow:
      - Created with a JS context, a registered callback descriptor (PJSCallback),
        and the Promise resolve/reject pair captured in @code(funcs).
      - Runs the callback and captures a JS-compatible result value.
      - Schedules a call to the proper resolve/reject using the QuickJS API.
      - Frees any argument structures allocated during parameter parsing. }
TJSAsyncTask = class(TThread)
private
  FContext: JSContext;      /// QuickJS context for this task
  funcs: JSDoubleVal;       /// Promise resolve/reject function handles
  FPromise: TJSCallback;
    /// Registered callback descriptor (name, handler, params)
  FResult: JSVAlueVal;
    /// Result produced by the callback (QuickJS value wrapper)
  FSuccess: boolean;        /// True if callback indicates success; otherwise reject

protected
      {** Thread entry point. Invokes the registered callback (via ProcessResult),
          then calls resolve or reject with the produced JS value. For callbacks
          registered with threaded=true, the callback itself runs on this worker
          thread and only FinishPromise is synchronized to the main thread. }
  procedure Execute; override;

      {** Run the registered callback, storing success flag and result.
          Executed via Synchronize to ensure safe interaction with GUI/JS context
          if required by the surrounding framework. }
  procedure ProcessResult;

      {** Invoke the native callback and capture status/result. When @code(quiet)
          is true (worker-thread path) errors are captured into FResult as the
          rejection value instead of showing UI dialogs. }
  procedure RunCallback(quiet: boolean);

      {** Resolve or reject the JS promise from FSuccess/FResult and release the
          QuickJS values and parsed parameters. Must run on the main thread. }
  procedure FinishPromise;
public
      {** Create and start an async task bound to a JS Promise.
          @param(Context) QuickJS context
          @param(func)    Registered callback descriptor with handler and params
          @param(cbfunc)  Pointer to the resolve/reject function pair (JSDoubleVal) }
  constructor Create(Context: JSContext; func: TJSCallback; cbfunc: PJSDoubleVal);
end;

// Global shutdown guard for extensions/promises
procedure SetExtShuttingDown(const Value: boolean);
function IsExtShuttingDown: boolean;

{** Number of TJSAsyncTask threads created but not yet finished. Shutdown and
    reload paths wait until this drops to zero instead of a fixed delay. }
function ActiveAsyncTaskCount: integer;

const
  {** Index of the Promise resolve function in @code(JSDoubleVal). }
JprResolve = 0;
  // const for "async function worked"

  {** Index of the Promise reject function in @code(JSDoubleVal). }
JprReject = 1;
  // for "didnt work"

implementation

uses Forms;

var
gExtShuttingDown: boolean = false;
gActiveAsyncTasks: longint = 0;

procedure SetExtShuttingDown(const Value: boolean);
begin
  gExtShuttingDown := Value;
end;

function IsExtShuttingDown: boolean;
begin
  Result := gExtShuttingDown;
end;

function ActiveAsyncTaskCount: integer;
begin
  Result := gActiveAsyncTasks;
end;

{** Construct and launch an asynchronous task bound to a JS Promise.
    Stores the context, callback descriptor, and resolve/reject functions.
    Sets FreeOnTerminate so the thread self-frees after completion. }
constructor TJSAsyncTask.Create(Context: JSContext; func: TJSCallback;
cbfunc: PJSDoubleVal);
begin
  FContext := Context;
  FPromise := func;
  // Copy resolve/reject function refs locally
  if cbfunc <> nil then
    funcs := cbfunc^;
  FreeOnTerminate := true;
  // Count from creation (on the main thread) so shutdown waiters never observe
  // a started-but-not-yet-counted task; Execute decrements when done.
  InterLockedIncrement(gActiveAsyncTasks);
  inherited Create(false);
  // This will start the thread
end;

{** Thread execution:
    - If a callback is assigned, run ProcessResult (synchronized as needed).
    - Convert the stored @code(FResult) to a JSValue.
    - Call resolve or reject depending on @code(FSuccess).
    - Free parameter data allocated during parsing.
    - Terminate the thread. }
procedure TJSAsyncTask.Execute;
begin
  try
    // Skip if application is terminating
    if Application.Terminated or IsExtShuttingDown then
      Exit;

    if Assigned(FPromise.callback) then // Run the main function, if it's actually set
    begin
      if FPromise.threaded then
      begin
        // Threaded callback: do the (potentially slow) native work right here on
        // the worker thread so the UI stays responsive; only the JS resolve/reject
        // is synchronized to the main thread, since QuickJS is single-threaded.
        RunCallback(true);
        if not (Application.Terminated or IsExtShuttingDown) then
        try
          Synchronize(@FinishPromise);
        except
          FSuccess := false;
          Exit;
        end;
      end
      // Check again before synchronizing, as shutdown might have occurred
      else if not (Application.Terminated or IsExtShuttingDown) then
      try
        Synchronize(@ProcessResult);
      except
          // If synchronize fails (e.g., main thread shutting down), just exit
        FSuccess := false;
        Exit;
      end// Additional safety check: Ensure the main thread is still processing messages
      ;
    end
    else
    begin
      // Complain that it wasn't set
      if not (Application.Terminated or IsExtShuttingDown) then
        ExtError(sdsAuto, 'Error: Missing Function');
      FSuccess := false;
      Exit;
    end;
    self.Terminate;
  finally
    InterLockedDecrement(gActiveAsyncTasks);
  end;
end;

{** Execute the registered native callback and capture status/result.
    On success, @code(FSuccess) is True and @code(FResult) holds the JS value to return.
    On errors/exceptions, sets @code(FSuccess:=False) and reports via UX helpers.
    Runs entirely on the main thread (legacy, non-threaded path). }
procedure TJSAsyncTask.ProcessResult;
begin
  RunCallback(false);
  FinishPromise;
end;

{** Invoke the native callback. quiet=true means we are on the worker thread:
    no UI dialogs may be shown, so error text becomes the promise rejection
    value instead. quiet=false preserves the legacy main-thread behavior. }
procedure TJSAsyncTask.RunCallback(quiet: boolean);
begin
  // Avoid doing work if app is shutting down
  if Application.Terminated or IsExtShuttingDown then
  begin
    FSuccess := false;
    Exit;
  end;

  // Additional check: ensure context is still valid
  if (FContext = nil) then
  begin
    FSuccess := false;
    Exit;
  end;

  with FPromise do
    if Assigned(Callback) then
    begin
      try
        FSuccess := FPromise.Callback(@FContext, func, params.values.Data, FResult);
      except
        on E: EInvalidCast do
        begin
          FSuccess := false;
          if quiet then
            FResult := StringToValueVal(E.Message)
          else
          if not (Application.Terminated or IsExtShuttingDown) then
            ExtError(sdsAuto, RS_TYPE_ERROR_MSG, e.message);
        end;
        on E: Exception do
        begin
          FSuccess := false;
          if quiet then
            FResult := StringToValueVal(E.Message)
          else
          if not (Application.Terminated or IsExtShuttingDown) then
            ExtError(sdsAuto, Format(RS_PROM_ERR_CAPT, [func]), e.Message);
        end;
      end;
    end
    else
      FSuccess := false;
end;

{** Resolve/reject the promise from FSuccess/FResult and free the captured
    QuickJS values and parsed parameters. Main thread only. }
procedure TJSAsyncTask.FinishPromise;
var
  xres: JSValue;
  callRes: JSValueRaw;
begin
  // Final check before making JS calls
  if Application.Terminated or IsExtShuttingDown then
    Exit;

  if (FContext = nil) then
    Exit;

  try
    // Convert and resolve/reject the promise
    xres := JSValueValToValue(FContext, FResult);
    if FSuccess then
      callRes := JS_Call(FContext, funcs[JprResolve], JS_UNDEFINED, 1, @xres)
    else
      callRes := JS_Call(FContext, funcs[JprReject], JS_UNDEFINED, 1, @xres);
    // Release the call result, the marshalled argument, and the resolve/reject
    // pair captured by JS_NewPromiseCapability — all leaked before this fix.
    FContext^.FreeInlined(PJSValue(@callRes));
    FContext^.Free(xres);
    FContext^.FreeInlined(PJSValue(@funcs[JprResolve]));
    FContext^.FreeInlined(PJSValue(@funcs[JprReject]));
  except
    // Silently ignore JS calls that fail during shutdown
    // This prevents crashes when the runtime is being destroyed
  end;

  // Free parsed parameters captured earlier
  if Assigned(FPromise.params.values.Data) then
    FPromise.params.values.Data.Free;
end;

end.
