unit system_media_controller_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  SystemMediaController;

type
  TSystemMediaControllerTests = class(TTestCase)
  published
    procedure TestExtractSpotifyHTTP;
    procedure TestExtractSpotifyURI;
    procedure TestExtractSpotifyWithQueryAndFragment;
    procedure TestExtractDeezer;
    procedure TestExtractDeezerWithTrailingSlashAndQuery;
    procedure TestDetectPlayerFromURL;
    procedure TestPlaySpotifyTrackInvalidReturnsFalse;
    procedure TestPlayDeezerTrackInvalidReturnsFalse;
  end;

implementation

procedure TSystemMediaControllerTests.TestExtractSpotifyHTTP;
var
  Ctrl: TSystemMediaController;
  ID: string;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    ID := Ctrl.ExtractTrackIDForTest('https://open.spotify.com/track/4iV5W9uYEdYUVa79Axb7Rh', mpSpotify);
    AssertEquals('spotify http track id', '4iV5W9uYEdYUVa79Axb7Rh', ID);
  finally
    Ctrl.Free;
  end;
end;

procedure TSystemMediaControllerTests.TestExtractSpotifyURI;
var
  Ctrl: TSystemMediaController;
  ID: string;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    ID := Ctrl.ExtractTrackIDForTest('spotify:track:4iV5W9uYEdYUVa79Axb7Rh', mpSpotify);
    AssertEquals('spotify uri track id', '4iV5W9uYEdYUVa79Axb7Rh', ID);
  finally
    Ctrl.Free;
  end;
end;

procedure TSystemMediaControllerTests.TestExtractSpotifyWithQueryAndFragment;
var
  Ctrl: TSystemMediaController;
  ID: string;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    ID := Ctrl.ExtractTrackIDForTest('https://open.spotify.com/track/4iV5W9uYEdYUVa79Axb7Rh?si=abc#fragment', mpSpotify);
    AssertEquals('spotify http with query/fragment', '4iV5W9uYEdYUVa79Axb7Rh', ID);
  finally
    Ctrl.Free;
  end;
end;

procedure TSystemMediaControllerTests.TestExtractDeezer;
var
  Ctrl: TSystemMediaController;
  ID: string;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    ID := Ctrl.ExtractTrackIDForTest('https://www.deezer.com/track/123456789', mpDeezer);
    AssertEquals('deezer track id', '123456789', ID);
  finally
    Ctrl.Free;
  end;
end;

procedure TSystemMediaControllerTests.TestExtractDeezerWithTrailingSlashAndQuery;
var
  Ctrl: TSystemMediaController;
  ID: string;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    ID := Ctrl.ExtractTrackIDForTest('https://www.deezer.com/track/123456789/?utm=1', mpDeezer);
    AssertEquals('deezer trailing slash and query', '123456789', ID);
  finally
    Ctrl.Free;
  end;
end;

procedure TSystemMediaControllerTests.TestDetectPlayerFromURL;
var
  Ctrl: TSystemMediaController;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    AssertEquals('detect spotify by domain', Ord(mpSpotify), Ord(Ctrl.DetectPlayerFromURLForTest('https://open.spotify.com/track/abc')));
    AssertEquals('detect spotify by scheme', Ord(mpSpotify), Ord(Ctrl.DetectPlayerFromURLForTest('spotify:track:abc')));
    AssertEquals('detect deezer by domain', Ord(mpDeezer), Ord(Ctrl.DetectPlayerFromURLForTest('https://www.deezer.com/track/123')));
    AssertEquals('unknown url', Ord(mpUnknown), Ord(Ctrl.DetectPlayerFromURLForTest('https://example.com/foo')));
  finally
    Ctrl.Free;
  end;
end;

procedure TSystemMediaControllerTests.TestPlaySpotifyTrackInvalidReturnsFalse;
var
  Ctrl: TSystemMediaController;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    // invalid spotify URL (no track id) -> should return false before platform open
    AssertFalse('play spotify track invalid', Ctrl.PlaySpotifyTrack('https://open.spotify.com/album/123'));
  finally
    Ctrl.Free;
  end;
end;

procedure TSystemMediaControllerTests.TestPlayDeezerTrackInvalidReturnsFalse;
var
  Ctrl: TSystemMediaController;
begin
  Ctrl := TSystemMediaController.Create(nil);
  try
    // invalid deezer URL (no track id)
    AssertFalse('play deezer track invalid', Ctrl.PlayDeezerTrack('https://www.deezer.com/artist/123'));
  finally
    Ctrl.Free;
  end;
end;

initialization
  RegisterTest(TSystemMediaControllerTests);

end.
