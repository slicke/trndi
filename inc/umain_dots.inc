(* Trend dot creation, layout, painting and interaction for TfBG.
   Extracted from units/forms/umain.pp.
   Included in the implementation section of umain.pp only.
*)

// DotPaint: Custom paint handler for trend dot TPaintBox controls.
// NOTE (macOS/Cocoa): We use TLabel for trend dots on macOS (TDotControl = TLabel)
// to avoid TPaintBox.OnPaint / CheckDC() issues under the Cocoa widgetset.
procedure TfBG.DotPaint(Sender: TObject);
var
  S, fontn: string;
  L: TDotControl;
  hasfont: boolean;
  needsRecalc: boolean;
begin
  L := Sender as TDotControl;
  S := L.Caption;

  {$ifdef DARWIN}
  L.AutoSize := true;
  L.Alignment := taCenter;
  L.Layout := tlCenter;
  Exit;
  {$endif}

  L.AutoSize := false;

  if S = DOT_GRAPH then
    hasfont := FontGUIInList(fontn)
  else
    hasfont := FontTXTInList(fontn);

  needsRecalc := (FCachedFontSize <> L.Font.Size) or (FCachedFontName <> fontn) or
    (FCachedTextWidth = 0) or (FCachedTextHeight = 0) or (S <> DOT_GRAPH);

  with L.Canvas do
  begin
    Brush.Style := bsClear;

    if hasfont then
      Font.Name := fontn;

    Font.Size := L.Font.Size;

    if needsRecalc then
    begin
      FCachedTextWidth := TextWidth(S);
      FCachedTextHeight := TextHeight(S);
      if S = DOT_GRAPH then
      begin
        FCachedFontSize := L.Font.Size;
        FCachedFontName := fontn;
      end;
    end;

    if S = DOT_GRAPH then
    begin
      L.Width := FCachedTextWidth;
      L.Height := FCachedTextHeight;
    end
    else
    begin
      L.Width := TextWidth(S) + 4;
      L.Height := TextHeight(S) + 2;
    end;

    TextOut(0, 0, S);
  end;
end;

procedure TfBG.BuildDotWindowMenu;
const
  WindowSizes: array[0..4] of integer = (6, 10, 18, 24, 36);
var
  i: integer;
  item: TMenuItem;
begin
  if not Assigned(FDotWindowMenu) then
  begin
    FDotWindowMenu := TMenuItem.Create(pmSettings);
    FDotWindowMenu.Caption := sTrendWindow;
    miSplit4.Parent.Insert(miSplit4.MenuIndex, FDotWindowMenu);
    for i := 0 to High(WindowSizes) do
    begin
      item := TMenuItem.Create(FDotWindowMenu);
      item.Caption := Format(sTrendWindowFmt,
        [WindowSizes[i] * INTERVAL_MINUTES, WindowSizes[i]]);
      item.Tag := WindowSizes[i];
      item.RadioItem := true;
      item.OnClick := @miDotWindowClick;
      FDotWindowMenu.Add(item);
    end;
  end;
  for i := 0 to FDotWindowMenu.Count - 1 do
    FDotWindowMenu.Items[i].Checked :=
      FDotWindowMenu.Items[i].Tag = ActiveDots;
end;

procedure TfBG.miDotWindowClick(Sender: TObject);
var
  i: integer;
  newCount: integer;
  ownerMenu: TMenuItem;
begin
  if not (Sender is TMenuItem) then Exit;
  newCount := TMenuItem(Sender).Tag;
  if (newCount < 1) or (newCount > MAX_DOT_COUNT) then Exit;

  ownerMenu := TMenuItem(Sender).Parent;
  if Assigned(ownerMenu) then
    for i := 0 to ownerMenu.Count - 1 do
      ownerMenu.Items[i].Checked := false;
  TMenuItem(Sender).Checked := true;

  native.SetSetting('ux.dot_count', newCount);
  ActiveDots := newCount;
  if MAX_MIN < ActiveDots * INTERVAL_MINUTES + 10 then
    MAX_MIN := ActiveDots * INTERVAL_MINUTES + 10;

  for i := 1 to High(TrendDots) do
    if Assigned(TrendDots[i]) then
    begin
      TrendDots[i].Free;
      TrendDots[i] := nil;
    end;
  SetLength(TrendDots, 0);
  for i := 1 to High(FWarningDots) do
    if Assigned(FWarningDots[i]) then
    begin
      FWarningDots[i].Free;
      FWarningDots[i] := nil;
    end;
  SetLength(FWarningDots, 0);

  if MAX_RESULT < ActiveDots * INTERVAL_MINUTES then
    MAX_RESULT := ActiveDots * INTERVAL_MINUTES;

  CreateTrendDots;

  // Force PlaceTrendDots to reprocess even though bgs hasn't changed.
  FLastReadingsHash := 0;

  Application.ProcessMessages;

  tResize.OnTimer(Self);

  if Length(bgs) > 0 then
    PlaceTrendDots(bgs);
  UpdateTrendDots;
  AdjustGraph;
end;

// Post-process the graph
procedure TfBG.AdjustGraph;
var
  da: integer;
begin
  ApplyTrendDotTopOffset(round(ClientHeight * DOT_ADJUST));

  da := dotsInView;
  if da <> 0 then
  begin
    ApplyTrendDotTopOffset(-da);
    da := dotsInView;
    if da <> 0 then
      ApplyTrendDotTopOffset(-da);
  end;

  if (Length(TrendDots) > 1) and TrendDots[1].Visible then
  begin
    lRef.Top     := TrendDots[1].Top;
    lRef.Caption := TrendDots[1].Hint;
  end
  else
    lRef.Caption := '';
  Update;

  RepaintVisibleTrendDots;
end;

// Create trend dot controls dynamically at runtime.
// Uses TLabel on macOS (Darwin) to avoid Cocoa CheckDC bugs, TPaintBox elsewhere.
// TrendDots is 1-based: index 0 is unused, valid range is 1..ActiveDots.
procedure TfBG.CreateTrendDots;
type
  PDotControl = ^TDotControl;
const
  MAX_NAMED = 10;
var
  i: integer;
  namedDots: array[1..MAX_NAMED] of PDotControl;
begin
  namedDots[1]  := @lDot1;
  namedDots[2]  := @lDot2;
  namedDots[3]  := @lDot3;
  namedDots[4]  := @lDot4;
  namedDots[5]  := @lDot5;
  namedDots[6]  := @lDot6;
  namedDots[7]  := @lDot7;
  namedDots[8]  := @lDot8;
  namedDots[9]  := @lDot9;
  namedDots[10] := @lDot10;

  SetLength(TrendDots, ActiveDots + 1);

  for i := 1 to ActiveDots do
  begin
    {$ifdef DARWIN}
    TrendDots[i] := TLabel.Create(Self);
    with TLabel(TrendDots[i]) do
    begin
      Parent := Self;
      AutoSize := true;
      Alignment := taCenter;
      Layout := tlCenter;
      Transparent := true;
    end;
    {$else}
    TrendDots[i] := TPaintBox.Create(Self);
    with TPaintBox(TrendDots[i]) do
    begin
      Parent := Self;
      AutoSize := false;
      OnPaint := @DotPaint;
    end;
    {$endif}

    with TrendDots[i] do
    begin
      Left := 72;
      Top := 191;
      Width := 33;
      Height := 49;
      Visible := false;
      Caption := DOT_GRAPH;
      PopupMenu := pmSettings;
      OnClick := @onTrendClick;
    end;

    if i <= MAX_NAMED then
      namedDots[i]^ := TrendDots[i];
  end;

  for i := ActiveDots + 1 to MAX_NAMED do
    namedDots[i]^ := nil;

  TrndiDLog(Format('Trend dots created: %d dots (%d min window)',
    [ActiveDots, ActiveDots * INTERVAL_MINUTES]));
end;

procedure TfBG.initDot(l: TDotControl; c, ix: integer);
begin
  {$ifdef Windows}
  l.Font.Name := 'DejaVu Sans Mono';
  {$endif}
end;

function TfBG.CalculateDotVisualOffset: integer;
var
  bmp: TBitmap;
  y, x: integer;
  fontSize: integer;
  halfHeight: integer;
  firstPixelY: integer;
  bgColor, pixelColor: TColor;
  found: boolean;
  fontName: string;
begin
  Result := 0;
  bmp := TBitmap.Create;
  try
    try
      fontSize := 24;
      bmp.Width := fontSize * 2;
      bmp.Height := fontSize * 2;

      {$ifdef DARWIN}
      if bmp.Canvas.Handle = 0 then ;
      {$endif}

      bgColor := clWhite;
      bmp.Canvas.Brush.Color := bgColor;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

      if FontGUIInList(fontName) then
        bmp.Canvas.Font.Name := fontName
      else
        bmp.Canvas.Font.Name := 'default';
      bmp.Canvas.Font.Size := fontSize;
      bmp.Canvas.Font.Color := clBlack;

      bmp.Canvas.TextOut(fontSize div 2, fontSize div 2, DOT_GRAPH);

      firstPixelY := -1;
      found := false;

      for y := 0 to bmp.Height - 1 do
      begin
        for x := 0 to bmp.Width - 1 do
        begin
          pixelColor := bmp.Canvas.Pixels[x, y];
          if pixelColor <> bgColor then
          begin
            firstPixelY := y;
            found := true;
            break;
          end;
        end;
        if found then
          break;
      end;

      if found then
      begin
        halfHeight := bmp.Height div 2;
        Result := firstPixelY - halfHeight;
        TrndiDLog(Format(
          'DOT_VISUAL_OFFSET calculated: font=%s, firstPixel=%d, halfHeight=%d, offset=%d',
          [fontName, firstPixelY, halfHeight, Result]));
      end;
    except
      on E: Exception do
      begin
        {$ifdef DARWIN}
        TrndiDLog('CalculateDotVisualOffset error on macOS: ' + E.Message);
        Result := 0;
        {$else}
        raise;
        {$endif}
      end;
    end;
  finally
    bmp.Free;
  end;
end;

// Expands a trend dot to show actual bg value with highlighting for latest reading
procedure TfBG.ExpandDot(l: TDotControl; c, ix: integer);
var
  isDot: boolean;
begin
  if privacyMode then
    exit;

  if l.hint = '' then
    Exit;

  isDot := l.Caption = DOT_GRAPH;

  if ix = ActiveDots then
    l.Caption := IfThen(isDot, DOT_FRESH, DOT_GRAPH)
  else
    l.Caption := IfThen(isDot, l.Hint, DOT_GRAPH);

  if not isDot then
  begin
    ResizeDot(l, c, ix);
  end
  else
  begin
    l.font.Size := (lVal.Font.Size div c);
    FCachedTextWidth := 0;
    FCachedTextHeight := 0;
    FCachedFontSize := 0;
    FCachedFontName := '';
    l.Invalidate;
  end;
end;

// Hides a dot
procedure TfBG.HideDot(l: TDotControl; c, ix: integer);
begin
  l.Visible := false;
end;

procedure TfBG.ApplyTrendDotTopOffset(const Offset: integer);
var
  l: TDotControl;
begin
  if Offset = 0 then
    Exit;

  for l in TrendDots do
  begin
    if not Assigned(l) then Continue;
    l.Top := l.Top + Offset;
  end;
end;

procedure TfBG.RepaintVisibleTrendDots;
var
  l: TDotControl;
begin
  for l in TrendDots do
  begin
    if not Assigned(l) then Continue;
    if l.Visible then
      l.Repaint;
  end;
end;

// Shows a dot only if it has valid data (non-empty Hint)
procedure TfBG.ShowDot(l: TDotControl; c, ix: integer);
begin
  if l.Hint <> '' then
    l.Visible := true;
end;

// Scales a dot to fill ~85% of its slot width so adjacent dots never overlap.
procedure TfBG.ResizeDot(l: TDotControl; c, ix: integer);
var
  th, tw, slotW: integer;
begin
  slotW := Max(1, fBG.ClientWidth div Max(c, 1));
  {$ifdef DARWIN}
  l.AutoSize := true;
  l.Font.Size := Max(8, slotW);
  // Canvas.TextWidth must not be called outside drawRect: on Cocoa — it corrupts
  // the NSGraphicsContext (CheckDC bug) and turns the whole window solid green.
  l.AdjustSize;
  TrndiDLog(Format('TrendDots[%d] resized with Font Size = %d, Height=%d (AutoSize).',
    [ix, l.Font.Size, l.Height]));
  {$else}
  l.AutoSize := false;
  l.Font.Size := Max(8, slotW);
  tw := l.Canvas.TextWidth(DOT_GRAPH);
  if tw > 0 then
    l.Font.Size := Max(8, Round(l.Font.Size * (slotW * 85 div 100) / tw));
  tw := l.Canvas.TextWidth(DOT_GRAPH);
  th := l.Canvas.TextHeight(DOT_GRAPH);
  l.Width := Max(1, tw);
  l.Height := Max(th, l.Font.Size);
  TrndiDLog(Format('TrendDots[%d] resized with Font Size = %d, W=%d, H=%d.',
    [ix, l.Font.Size, l.Width, l.Height]));
  {$endif}
end;

// Centers a dot within its slot across ClientWidth.
procedure TfBG.SetDotWidth(l: TDotControl; c, ix: integer; ls: array of TDotControl);
var
  leftPx, rightPx, slotWidth: integer;
  cw: integer;
begin
  if c < 1 then Exit;
  cw        := fBG.ClientWidth;
  leftPx    := Round((ix - 1) * cw / c);
  rightPx   := Round(ix       * cw / c);
  if rightPx > cw then rightPx := cw;
  slotWidth := rightPx - leftPx;
  l.Left    := Max(0, Min(leftPx + (slotWidth - l.Width) div 2, cw - l.Width));
  TrndiDLog(Format('TrendDots[%d] Left=%d Width=%d.', [ix, l.Left, l.Width]));
end;

// Swap dots with their readings
procedure TfBG.onTrendClick(Sender: TObject);
var
  isdot: boolean;
  l: TDotControl;
  {$ifdef TrndiExt}
  fs: TFormatSettings;
  {$endif}
begin
  l := Sender as TDotControl;

  actOnTrend(@ExpandDot);
  isDot := UnicodeSameText(l.Caption, DOT_GRAPH);

  {$ifdef TrndiExt}
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  callFunc('dotClicked',[
    isDot,
    StrToFloat(l.Hint) * BG_CONVERTIONS[mgdl][un],
    StrToFloat(l.Hint) * BG_CONVERTIONS[mmol][un],
    l.tag
    ]);
  {$endif}

  if isDot then
    tResize.OnTimer(self);
end;
