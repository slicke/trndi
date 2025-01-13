// Include file with JS functions
uses
  mormot.lib.quickjs;

// Convert a color, eg for a ux call in JS, to TColor
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

// UX modifications from JS ("uxProp()")
function JSUX(ctx: PJSContext; this_val: JSValueRaw; argc: integer; argv: PJSValues): JSValueRaw; cdecl;
var
  comp, what, val: string;
begin
  // Parse the parameters
  comp := ctx^^.ToUtf8(argv^[0]);
  what :=  ctx^^.ToUtf8(argv^[1]);
  val :=  ctx^^.ToUtf8(argv^[2]);

  if fBG.FindSubComponent(comp) <> nil then // Look for the control
    with fBG.FindSubComponent(comp) as TGraphicControl do begin // Cast it as a graphic control
        with fBG.FindSubComponent(comp) as TGraphicControl do begin // Use the control
          if Pos(what, 'color') > 0 then begin // Match the action
            case what of
              'color': Color := HTMLColorToTColor(val);
              'font-color': font.Color := HTMLColorToTColor(val);
             else
                ShowMessage(Format('An extension tried to access %s on %s. But the property does not exist.', [what, comp]));
          end;
          end else if Pos(what, 'font') > -1 then
            font.Name := val;
       end; // Component
     end;

  Result := JS_UNDEFINED;
  if TTrndiExtEngine.Instance.FunctionExists('uxCallback') then
     TTrndiExtEngine.Instance.CallFunction('uxCallback', [comp, val]);
end;

