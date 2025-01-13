unit trndiext.promise;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mORMot.lib.quickjs, mormot.core.base, dialogs, trndiext.functions, slicke.ux.alert, fgl;

  type
  JSDoubleVal = array[0..1] of JSValueRaw;
  PJSDoubleVal = ^JSValueRaw;

  TJSAsyncTask = class(TThread)
  private
    FContext: JSContext;
    funcs: JSDoubleVal;
    FPromise: PJSCallback ;
    FResult: JSVAlueVal;
    FSuccess: boolean;
  protected
    procedure Execute; override;
    procedure ProcessResult;
  public
    constructor Create(Context: JSContext; func: PJSCallback; cbfunc: PJSDoubleVal);
  end;

  const
  JprResolve = 0;
  JprReject = 1;

implementation
resourcestring
  sTypeErrCapt = 'A data type differes from what was expected';
  sTypeErrDesc = 'The extension was stopped';
  sTypeErrmsg = 'A data type was expected, but another was found';
  sPromErrCapt = 'The asyncronous function %s failed to complete';


constructor TJSAsyncTask.Create(Context: JSContext; func: PJSCallback; cbfunc: PJSDoubleVal);
begin
  FContext := Context;
  FPromise := func;
  funcs := cbfunc;
  FreeOnTerminate := True;
  inherited Create(False);  // Starta tråden direkt
end;

procedure TJSAsyncTask.Execute;
var
    xres: JSValue;
begin

  if Assigned(FPromise^.callback) then
    Synchronize(@ProcessResult)
  else begin
    ExtError('Error: Missing Function');
    FSuccess := false;
    Exit;
  end;

    xres := JSValueValToValue(FContext, FResult);

  if FSuccess then
    JS_Call(FContext, funcs[JprResolve], JS_UNDEFINED, 1, @xres)
  else
    JS_Call(FContext, funcs[JprReject], JS_UNDEFINED, 1, @xres);

//    FContext^.Free(JSValue(Promise));
//    JS_Free(FContext, @Promise);
     FPromise^.params.values.data.Free;

end;

procedure TJSAsyncTask.ProcessResult;
begin
 with FPromise^ do begin
  if Assigned(Callback) then begin
  try
      FSuccess := FPromise^.Callback(@FContext, func, params.values.data, FResult);
  except on E: EInvalidCast do begin
      FSuccess := false;
      ExtError(sTypeErrMsg, e.message);
      end;
    on E: Exception do begin
      fsuccess := false;
      ExtError(Format(sPromErrCapt, [func]),e.Message);
      end;
  end;
  end else fsuccess := false;
 end;


end;

end.


