// Public test wrappers
function TSystemMediaController.ExtractTrackIDForTest(const URL: string; Player: TMediaPlayer): string;
begin
  Result := ExtractTrackID(URL, Player);
end;

function TSystemMediaController.DetectPlayerFromURLForTest(const URL: string): TMediaPlayer;
begin
  Result := DetectPlayerFromURL(URL);
end;