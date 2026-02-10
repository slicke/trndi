function TfBG.GetValidatedPositionForTests: TrndiPos;
begin
  Result := GetValidatedPosition;
end;

function TfBG.DotsInViewForTests: integer;
begin
  Result := dotsInView;
end;

function TfBG.DotsInViewForTestsFromInfos(const Dots: array of TDotInfo; ParentHeight: Integer): Integer;
begin
  Result := DotsInViewFromInfos(Dots, ParentHeight);
end;

procedure TfBG.SetTrendDotForTests(i: integer; d: TDotControl);
begin
  if (i >= Low(TrendDots)) and (i <= High(TrendDots)) then
    TrendDots[i] := d;
end;

function TfBG.GetTrendDotForTests(i: integer): TDotControl;
begin
  if (i >= Low(TrendDots)) and (i <= High(TrendDots)) then
    Result := TrendDots[i]
  else
    Result := nil;
end;