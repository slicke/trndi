uses
  mormot.lib.quickjs;


function HTMLColorToTColor(const HTMLColor: string): TColor;
var
  R, G, B: Integer;
  ColorStr: string;
begin
  if (Length(HTMLColor) = 7) and (HTMLColor[1] = '#') then
  begin
    ColorStr := Copy(HTMLColor, 2, 6); // Ta bort '#'

    R := StrToInt('$' + Copy(ColorStr, 1, 2));
    G := StrToInt('$' + Copy(ColorStr, 3, 2));
    B := StrToInt('$' + Copy(ColorStr, 5, 2));

    Result := (B shl 16) or (G shl 8) or R;
  end
  else
    raise Exception.Create('Unsupported color');
end;

function JSUX(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw; cdecl;
var
  comp, what, val: string;
begin
  comp := ctx^^.ToUtf8(argv^[0]);
  what :=  ctx^^.ToUtf8(argv^[1]);
  val :=  ctx^^.ToUtf8(argv^[2]);

  if what = 'color' then begin
    if fBG.FindSubComponent(comp) <> nil then
       with fBG.FindSubComponent(comp) as TGraphicControl do begin
          case what of
            'color': Color := HTMLColorToTColor(val);
          end;
       end;
  end;
  Result := JS_UNDEFINED;
  if TTrndiExtEngine.Instance.FunctionExists('uxCallback') then
     TTrndiExtEngine.Instance.CallFunction('uxCallback', [comp, val]);
end;

