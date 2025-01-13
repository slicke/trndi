function JSDoLog(ctx: PJSContext; this_val: JSValueRaw; argc: Integer; argv: PJSValueRaw): JSValueRaw; cdecl;
var
  i: Integer;
  LogMessage: RawUtf8;
begin
  LogMessage := '';
  for i := 0 to argc - 1 do
    LogMessage := LogMessage + JS_ToCString(ctx^, argv[i]) + ' ';
  FOutput := Trim(LogMessage);
  Result := JS_UNDEFINED;
end;


