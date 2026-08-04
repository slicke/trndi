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
unit system_media_controller_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  slicke.systemmediacontroller;

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
