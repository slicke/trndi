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

// JS promise handler
unit trndi.ext.promise;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mORMot.lib.quickjs, mormot.core.base, dialogs,
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
      FPromise: TJSCallback;    /// Registered callback descriptor (name, handler, params)
      FResult: JSVAlueVal;      /// Result produced by the callback (QuickJS value wrapper)
      FSuccess: boolean;        /// True if callback indicates success; otherwise reject

    protected
      {** Thread entry point. Invokes the registered callback (via ProcessResult),
          then calls resolve or reject with the produced JS value. }
      procedure Execute; override;

      {** Run the registered callback, storing success flag and result.
          Executed via Synchronize to ensure safe interaction with GUI/JS context
          if required by the surrounding framework. }
      procedure ProcessResult;

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

procedure SetExtShuttingDown(const Value: boolean);
begin
  gExtShuttingDown := Value;
end;

function IsExtShuttingDown: boolean;
begin
  Result := gExtShuttingDown;
end;

{** Construct and launch an asynchronous task bound to a JS Promise.
    Stores the context, callback descriptor, and resolve/reject functions.
    Sets FreeOnTerminate so the thread self-frees after completion. }
constructor TJSAsyncTask.Create(Context: JSContext; func: TJSCallback; cbfunc: PJSDoubleVal);
begin
  FContext := Context;
  FPromise := func;
  // Copy resolve/reject function refs locally
  if cbfunc <> nil then
    funcs := cbfunc^;
  FreeOnTerminate := True;
  inherited Create(False);
  // This will start the thread
end;

{** Thread execution:
    - If a callback is assigned, run ProcessResult (synchronized as needed).
    - Convert the stored @code(FResult) to a JSValue.
    - Call resolve or reject depending on @code(FSuccess).
    - Free parameter data allocated during parsing.
    - Terminate the thread. }
procedure TJSAsyncTask.Execute;
var
  xres: JSValue;
begin
  // Skip if application is terminating
  if Application.Terminated or IsExtShuttingDown then
    Exit;

  if Assigned(FPromise.callback) then // Run the main function, if it's actually set
  begin
    // Check again before synchronizing, as shutdown might have occurred
    if not (Application.Terminated or IsExtShuttingDown) then
    begin
      // Additional safety check: Ensure the main thread is still processing messages
      try
        Synchronize(@ProcessResult);
      except
        // If synchronize fails (e.g., main thread shutting down), just exit
        FSuccess := false;
        Exit;
      end;
    end;
  end
  else
    begin
      // Complain that it wasn't set
      if not (Application.Terminated or IsExtShuttingDown) then
        ExtError(uxdAuto, 'Error: Missing Function');
      FSuccess := false;
      Exit;
    end;
  self.Terminate;
end;

{** Execute the registered native callback and capture status/result.
    On success, @code(FSuccess) is True and @code(FResult) holds the JS value to return.
    On errors/exceptions, sets @code(FSuccess:=False) and reports via UX helpers. }
procedure TJSAsyncTask.ProcessResult;
var
  xres: JSValue;
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
  begin
    if Assigned(Callback) then
    begin
      try
        FSuccess := FPromise.Callback(@FContext, func, params.values.data, FResult);
      except
        on E: EInvalidCast do
        begin
          FSuccess := false;
          if not (Application.Terminated or IsExtShuttingDown) then
            ExtError(uxdAuto, sTypeErrMsg, e.message);
        end;
        on E: Exception do
        begin
          FSuccess := false;
          if not (Application.Terminated or IsExtShuttingDown) then
            ExtError(uxdAuto, Format(sPromErrCapt, [func]), e.Message);
        end;
      end;
    end
    else
      FSuccess := false;
  end;

  // Final check before making JS calls
  if Application.Terminated or IsExtShuttingDown then
    Exit;

  if (FContext = nil) then
    Exit;

  try
    // Convert and resolve/reject the promise
    xres := JSValueValToValue(FContext, FResult);
    if FSuccess then
      JS_Call(FContext, funcs[JprResolve], JS_UNDEFINED, 1, @xres)
    else
      JS_Call(FContext, funcs[JprReject], JS_UNDEFINED, 1, @xres);
  except
    // Silently ignore JS calls that fail during shutdown
    // This prevents crashes when the runtime is being destroyed
  end;

  // Free parsed parameters captured earlier
  if Assigned(FPromise.params.values.data) then
    FPromise.params.values.data.Free;
end;

end.
