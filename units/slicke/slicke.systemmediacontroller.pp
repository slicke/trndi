{==============================================================================
  Unit: SystemMediaController

  Cross-platform system media controller with Deezer support
  Uses array approach for player management

  License: LGPLv3+

  By: Björn Lindh and Claude AI
==============================================================================}

unit slicke.systemmediacontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, ExtCtrls, DateUtils,
  {$IFDEF WINDOWS}
  Windows, ComObj, ActiveX, MMSystem, ShellAPI,
  {$ENDIF}
  typinfo, Math;

type
  TSystemMediaController = class;

  // Enumerations
  TMediaPlayer = (mpUnknown, mpSpotify, mpDeezer, mpItunes, mpMusicApp,
                  mpVLC, mpFoobar2000, mpWinamp, mpMPV, mpRhythmbox,
                  mpAmarok, mpClementine, mpAudacious);
  TPlaybackState = (psUnknown, psPlaying, psPaused, psStopped);
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  // Records
  TTrackInfo = record
    Title: string;
    Artist: string;
    Album: string;
    Duration: Integer;
    Position: Integer;
    IsValid: Boolean;
  end;

  TPlayerInfo = record
    Player: TMediaPlayer;
    ProcessName: string;
    DisplayName: string;
    IsRunning: Boolean;
    SupportsVolumeControl: Boolean;
    SupportsTrackInfo: Boolean;
  end;

  TMediaState = record
    CurrentPlayer: TMediaPlayer;
    State: TPlaybackState;
    CurrentTrack: TTrackInfo;
    Volume: Integer;
    IsMuted: Boolean;
    LastUpdate: TDateTime;
  end;

  // Array types
  TPlayerInfoArray = array of TPlayerInfo;

  // Events
  TTrackChangeEvent = procedure(Sender: TObject; const OldTrack, NewTrack: TTrackInfo) of object;
  TStateChangeEvent = procedure(Sender: TObject; const OldState, NewState: TPlaybackState) of object;
  TPlayerChangeEvent = procedure(Sender: TObject; const OldPlayer, NewPlayer: TMediaPlayer) of object;
  TErrorEvent = procedure(Sender: TObject; const ErrorMessage: string) of object;

  { TSystemMediaController }
  TSystemMediaController = class(TComponent)
  private
    FCriticalSection: TRTLCriticalSection;
    FRefreshTimer: TTimer;
    FCurrentState: TMediaState;
    FAvailablePlayers: TPlayerInfoArray;
    FIsInitialized: Boolean;
    FLastTrackTitle: string;
    FVolumeStep: Integer;

    // Events
    FOnTrackChange: TTrackChangeEvent;
    FOnStateChange: TStateChangeEvent;
    FOnPlayerChange: TPlayerChangeEvent;
    FOnError: TErrorEvent;

    // Private methods
    procedure DetectAvailablePlayers;
    function IsPlayerRunning(const ProcessName: string): Boolean;
    function GetPlayerInfo(Player: TMediaPlayer): TPlayerInfo;
    function FindBestAvailablePlayer: TMediaPlayer;

    {$IFDEF WINDOWS}
    function WindowsExecuteMediaCommand(Command: string; Player: TMediaPlayer): Boolean;
    function WindowsGetTrackInfo(Player: TMediaPlayer): TTrackInfo;
    function WindowsSetVolume(Volume: Integer): Boolean;
    {$ENDIF}

    {$IFDEF DARWIN}
    function MacOSExecuteAppleScript(const Script: string): string;
    function MacOSExecuteMediaCommand(Command: string; Player: TMediaPlayer): Boolean;
    function MacOSGetTrackInfo(Player: TMediaPlayer): TTrackInfo;
    {$ENDIF}

    {$IFDEF LINUX}
    function LinuxExecuteDBusCommand(const Command: string; Player: TMediaPlayer): string;
    function LinuxExecuteMediaCommand(Command: string; Player: TMediaPlayer): Boolean;
    function LinuxGetTrackInfo(Player: TMediaPlayer): TTrackInfo;
    function GetMPRISPlayerName(Player: TMediaPlayer): string;
    {$ENDIF}

    function PlayerToString(Player: TMediaPlayer): string;
    function ExecuteCommand(const Command: string): string;
    function ExecuteCommandOK(const Command: string): Boolean;
    procedure OnRefreshTimer(Sender: TObject);
    procedure UpdateCurrentState;
    function ExtractTrackID(const URL: string; Player: TMediaPlayer): string;
    function DetectPlayerFromURL(const URL: string): TMediaPlayer;

    // Property getters
    function GetCurrentPlayer: TMediaPlayer;
    function GetCurrentTrack: TTrackInfo;
    function GetCurrentState: TPlaybackState;
    function GetCurrentVolume: Integer;
    function GetPlayerCount: Integer;
    function GetPlayer(Index: Integer): TPlayerInfo;

    // Platform specific URL management
    {$IFDEF WINDOWS}
    function WindowsOpenURL(const URL: string): Boolean;
    {$ENDIF}
    {$IFDEF DARWIN}
    function MacOSPlaySpotifyURL(const URL: string): Boolean;
    function MacOSPlayDeezerURL(const URL: string): Boolean;
    {$ENDIF}
    {$IFDEF LINUX}
    function LinuxPlayTrackURL(const URL: string; Player: TMediaPlayer): Boolean;
    {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Main methods
    procedure Initialize;
    procedure RefreshPlayerList;

    // Playback control
    function Play: Boolean;
    function Pause: Boolean;
    function TogglePlayPause: Boolean;
    function Next: Boolean;
    function Previous: Boolean;

    // Volume control
    function SetVolume(Volume: Integer): Boolean;
    function VolumeUp: Boolean;
    function VolumeDown: Boolean;

    // Information
    function GetTrackInfo: TTrackInfo;
    function UpdateState: Boolean;

    // Player management
    function SwitchToPlayer(Player: TMediaPlayer): Boolean;
    function IsPlayerAvailable(Player: TMediaPlayer): Boolean;

    // Playback
    function PlayTrackFromURL(const URL: string): Boolean;
    function PlaySpotifyTrack(const SpotifyURL: string): Boolean;
    function PlayDeezerTrack(const DeezerURL: string): Boolean;

    // Properties
    property CurrentPlayer: TMediaPlayer read GetCurrentPlayer;
    property CurrentTrack: TTrackInfo read GetCurrentTrack;
    property CurrentState: TPlaybackState read GetCurrentState;
    property CurrentVolume: Integer read GetCurrentVolume;
    property IsInitialized: Boolean read FIsInitialized;
    property VolumeStep: Integer read FVolumeStep write FVolumeStep;
    property PlayerCount: Integer read GetPlayerCount;
    property Players[Index: Integer]: TPlayerInfo read GetPlayer;

    // Events
    property OnTrackChange: TTrackChangeEvent read FOnTrackChange write FOnTrackChange;
    property OnStateChange: TStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnPlayerChange: TPlayerChangeEvent read FOnPlayerChange write FOnPlayerChange;
    property OnError: TErrorEvent read FOnError write FOnError;
  public
    {$IFDEF TEST}
    {$I ../../tests/inc/systemmediacontroller_implementation.inc}
    {$ENDIF}
  end;

// Helper functions
function CreateEmptyTrackInfo: TTrackInfo;

implementation

// Helper functions
function CreateEmptyTrackInfo: TTrackInfo;
begin
  FillChar(Result, SizeOf(TTrackInfo), 0);
  Result.Title := '';
  Result.Artist := '';
  Result.Album := '';
  Result.Duration := 0;
  Result.Position := 0;
  Result.IsValid := False;
end;

{ TSystemMediaController }

constructor TSystemMediaController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  InitCriticalSection(FCriticalSection);
  FRefreshTimer := TTimer.Create(nil);
  FRefreshTimer.OnTimer := @OnRefreshTimer;
  FRefreshTimer.Interval := 2000;
  FRefreshTimer.Enabled := False;

  SetLength(FAvailablePlayers, 0);

  FCurrentState.CurrentPlayer := mpUnknown;
  FCurrentState.State := psUnknown;
  FCurrentState.CurrentTrack := CreateEmptyTrackInfo;
  FCurrentState.Volume := 50;
  FCurrentState.IsMuted := False;

  FIsInitialized := False;
  FLastTrackTitle := '';
  FVolumeStep := 5;
end;

destructor TSystemMediaController.Destroy;
begin
  FRefreshTimer.Enabled := False;
  FreeAndNil(FRefreshTimer);
  SetLength(FAvailablePlayers, 0);
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

procedure TSystemMediaController.Initialize;
begin
  try
    DetectAvailablePlayers;
    FCurrentState.CurrentPlayer := FindBestAvailablePlayer;

    if FCurrentState.CurrentPlayer <> mpUnknown then
    begin
      FRefreshTimer.Enabled := True;
      UpdateCurrentState;
      FIsInitialized := True;
    end;

  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError(Self, 'Initialize failed: ' + E.Message);
  end;
end;

procedure TSystemMediaController.DetectAvailablePlayers;
var
  Player: TMediaPlayer;
  PlayerInfo: TPlayerInfo;
  Count: Integer;
begin
  SetLength(FAvailablePlayers, 0);
  Count := 0;

  for Player := Low(TMediaPlayer) to High(TMediaPlayer) do
  begin
    if Player = mpUnknown then Continue;

    PlayerInfo := GetPlayerInfo(Player);
    if IsPlayerRunning(PlayerInfo.ProcessName) then
    begin
      PlayerInfo.IsRunning := True;
      SetLength(FAvailablePlayers, Count + 1);
      FAvailablePlayers[Count] := PlayerInfo;
      Inc(Count);
    end;
  end;
end;

function TSystemMediaController.GetPlayerInfo(Player: TMediaPlayer): TPlayerInfo;
begin
  FillChar(Result, SizeOf(TPlayerInfo), 0);
  Result.Player := Player;
  Result.SupportsVolumeControl := True;
  Result.SupportsTrackInfo := True;
  Result.IsRunning := False;

  case Player of
    mpSpotify:
      begin
        Result.ProcessName := {$IFDEF WINDOWS}'Spotify.exe'{$ELSE}'Spotify'{$ENDIF};
        Result.DisplayName := 'Spotify';
      end;
    mpDeezer:
      begin
        Result.ProcessName := {$IFDEF WINDOWS}'Deezer.exe'{$ELSE}'Deezer'{$ENDIF};
        Result.DisplayName := 'Deezer';
      end;
    mpItunes:
      begin
        Result.ProcessName := {$IFDEF WINDOWS}'iTunes.exe'{$ELSE}'iTunes'{$ENDIF};
        Result.DisplayName := 'iTunes';
      end;
    mpMusicApp:
      begin
        Result.ProcessName := 'Music';
        Result.DisplayName := 'Music';
      end;
    mpVLC:
      begin
        Result.ProcessName := {$IFDEF WINDOWS}'vlc.exe'{$ELSE}'vlc'{$ENDIF};
        Result.DisplayName := 'VLC Media Player';
      end;
    mpFoobar2000:
      begin
        Result.ProcessName := 'foobar2000.exe';
        Result.DisplayName := 'Foobar2000';
      end;
    mpWinamp:
      begin
        Result.ProcessName := 'winamp.exe';
        Result.DisplayName := 'Winamp';
      end;
    mpRhythmbox:
      begin
        Result.ProcessName := 'rhythmbox';
        Result.DisplayName := 'Rhythmbox';
      end;
    mpAmarok:
      begin
        Result.ProcessName := 'amarok';
        Result.DisplayName := 'Amarok';
      end;
  end;
end;

function TSystemMediaController.FindBestAvailablePlayer: TMediaPlayer;
var
  I: Integer;
begin
  Result := mpUnknown;

  // Priority: Spotify > Deezer > iTunes/Music > VLC > Others
  for I := 0 to High(FAvailablePlayers) do
  begin
    case FAvailablePlayers[I].Player of
      mpSpotify:
        begin
          Result := mpSpotify;
          Break;
        end;
      mpDeezer:
        if Result = mpUnknown then
          Result := mpDeezer;
      mpItunes, mpMusicApp:
        if Result = mpUnknown then
          Result := FAvailablePlayers[I].Player;
      mpVLC:
        if Result = mpUnknown then
          Result := mpVLC;
      else
        if Result = mpUnknown then
          Result := FAvailablePlayers[I].Player;
    end;
  end;
end;

function TSystemMediaController.IsPlayerRunning(const ProcessName: string): Boolean;
{$IFDEF WINDOWS}
var
  Output: string;
begin
  Result := False;
  if ProcessName = '' then Exit;

  try
    Output := ExecuteCommand('tasklist /FI "IMAGENAME eq ' + ProcessName + '"');
    Result := (Pos(ProcessName, Output) > 0) and
              (Pos('No tasks are running', Output) = 0);
  except
    Result := False;
  end;
end;
{$ELSE}
var
  Output: string;
begin
  Result := False;
  if ProcessName = '' then Exit;

  try
    Output := ExecuteCommand('pgrep "' + ProcessName + '"');
    Result := (Output <> '') and (Pos('not found', LowerCase(Output)) = 0);
  except
    Result := False;
  end;
end;
{$ENDIF}

// Playback control methods
function TSystemMediaController.Play: Boolean;
begin
  Result := False;
  if FCurrentState.CurrentPlayer = mpUnknown then Exit;

  {$IFDEF WINDOWS}
  Result := WindowsExecuteMediaCommand('play', FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := MacOSExecuteMediaCommand('play', FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := LinuxExecuteMediaCommand('play', FCurrentState.CurrentPlayer);
  {$ENDIF}
end;

function TSystemMediaController.Pause: Boolean;
begin
  Result := False;
  if FCurrentState.CurrentPlayer = mpUnknown then Exit;

  {$IFDEF WINDOWS}
  Result := WindowsExecuteMediaCommand('pause', FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := MacOSExecuteMediaCommand('pause', FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := LinuxExecuteMediaCommand('pause', FCurrentState.CurrentPlayer);
  {$ENDIF}
end;

function TSystemMediaController.TogglePlayPause: Boolean;
begin
  Result := False;
  if FCurrentState.CurrentPlayer = mpUnknown then Exit;

  {$IFDEF WINDOWS}
  keybd_event(VK_MEDIA_PLAY_PAUSE, 0, 0, 0);
  keybd_event(VK_MEDIA_PLAY_PAUSE, 0, KEYEVENTF_KEYUP, 0);
  Result := True;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := MacOSExecuteMediaCommand('playpause', FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := LinuxExecuteMediaCommand('playpause', FCurrentState.CurrentPlayer);
  {$ENDIF}
end;

function TSystemMediaController.Next: Boolean;
begin
  Result := False;
  if FCurrentState.CurrentPlayer = mpUnknown then Exit;

  {$IFDEF WINDOWS}
  keybd_event(VK_MEDIA_NEXT_TRACK, 0, 0, 0);
  keybd_event(VK_MEDIA_NEXT_TRACK, 0, KEYEVENTF_KEYUP, 0);
  Result := True;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := MacOSExecuteMediaCommand('next', FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := LinuxExecuteMediaCommand('next', FCurrentState.CurrentPlayer);
  {$ENDIF}
end;

function TSystemMediaController.Previous: Boolean;
begin
  Result := False;
  if FCurrentState.CurrentPlayer = mpUnknown then Exit;

  {$IFDEF WINDOWS}
  keybd_event(VK_MEDIA_PREV_TRACK, 0, 0, 0);
  keybd_event(VK_MEDIA_PREV_TRACK, 0, KEYEVENTF_KEYUP, 0);
  Result := True;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := MacOSExecuteMediaCommand('previous', FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := LinuxExecuteMediaCommand('previous', FCurrentState.CurrentPlayer);
  {$ENDIF}
end;

// Platform-specific implementations
{$IFDEF WINDOWS}
function TSystemMediaController.WindowsExecuteMediaCommand(Command: string; Player: TMediaPlayer): Boolean;
begin
  Result := True;
  case LowerCase(Command) of
    'play', 'pause', 'playpause':
      begin
        keybd_event(VK_MEDIA_PLAY_PAUSE, 0, 0, 0);
        keybd_event(VK_MEDIA_PLAY_PAUSE, 0, KEYEVENTF_KEYUP, 0);
      end;
    'next':
      begin
        keybd_event(VK_MEDIA_NEXT_TRACK, 0, 0, 0);
        keybd_event(VK_MEDIA_NEXT_TRACK, 0, KEYEVENTF_KEYUP, 0);
      end;
    'previous':
      begin
        keybd_event(VK_MEDIA_PREV_TRACK, 0, 0, 0);
        keybd_event(VK_MEDIA_PREV_TRACK, 0, KEYEVENTF_KEYUP, 0);
      end;
    else
      Result := False;
  end;
end;

function TSystemMediaController.WindowsGetTrackInfo(Player: TMediaPlayer): TTrackInfo;
begin
  Result := CreateEmptyTrackInfo;
  // Implementation would require specific APIs
end;

function TSystemMediaController.WindowsSetVolume(Volume: Integer): Boolean;
begin
  Result := True;
  Volume := Max(0, Min(100, Volume));
  FCurrentState.Volume := Volume;
  // Implementation would use Windows volume APIs
end;
{$ENDIF}

{$IFDEF DARWIN}
function TSystemMediaController.MacOSExecuteAppleScript(const Script: string): string;
var
  Process: TProcess;
  OutputStream: TStringStream;
begin
  Result := '';
  Process := TProcess.Create(nil);
  OutputStream := TStringStream.Create;
  try
    Process.Executable := 'osascript';
    Process.Parameters.Add('-e');
    Process.Parameters.Add(Script);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.ShowWindow := swoHide;

    Process.Execute;
    OutputStream.CopyFrom(Process.Output, 0);
    Result := Trim(OutputStream.DataString);
  finally
    Process.Free;
    OutputStream.Free;
  end;
end;

function TSystemMediaController.MacOSExecuteMediaCommand(Command: string; Player: TMediaPlayer): Boolean;
var
  Script: string;
  AppName: string;
begin
  Result := False;

  case Player of
    mpSpotify: AppName := 'Spotify';
    mpDeezer: AppName := 'Deezer';
    mpItunes: AppName := 'iTunes';
    mpMusicApp: AppName := 'Music';
    else Exit;
  end;

  case LowerCase(Command) of
    'play': Script := Format('tell application "%s" to play', [AppName]);
    'pause': Script := Format('tell application "%s" to pause', [AppName]);
    'playpause': Script := Format('tell application "%s" to playpause', [AppName]);
    'next': Script := Format('tell application "%s" to next track', [AppName]);
    'previous': Script := Format('tell application "%s" to previous track', [AppName]);
    else Exit;
  end;

  try
    MacOSExecuteAppleScript(Script);
    Result := True;
  except
    Result := False;
  end;
end;

function TSystemMediaController.MacOSGetTrackInfo(Player: TMediaPlayer): TTrackInfo;
begin
  Result := CreateEmptyTrackInfo;
  // Implementation would use AppleScript
end;
{$ENDIF}

{$IFDEF LINUX}
function TSystemMediaController.GetMPRISPlayerName(Player: TMediaPlayer): string;
begin
  case Player of
    mpSpotify: Result := 'org.mpris.MediaPlayer2.spotify';
    mpDeezer: Result := 'org.mpris.MediaPlayer2.deezer';
    mpVLC: Result := 'org.mpris.MediaPlayer2.vlc';
    mpRhythmbox: Result := 'org.mpris.MediaPlayer2.rhythmbox';
    mpAmarok: Result := 'org.mpris.MediaPlayer2.amarok';
    else Result := '';
  end;
end;

function TSystemMediaController.LinuxExecuteDBusCommand(const Command: string; Player: TMediaPlayer): string;
var
  PlayerName: string;
  FullCommand: string;
begin
  Result := '';
  // Check if dbus-send is available
  {$IFDEF UNIX}
  if ExeSearch('dbus-send', '') = '' then
    Exit;
  {$ENDIF}
  PlayerName := GetMPRISPlayerName(Player);
  if PlayerName = '' then Exit;

  FullCommand := Format('dbus-send --type=method_call --dest=%s /org/mpris/MediaPlayer2 %s',
                       [PlayerName, Command]);
  Result := ExecuteCommand(FullCommand);
end;

function TSystemMediaController.LinuxExecuteMediaCommand(Command: string; Player: TMediaPlayer): Boolean;
var
  DBusCommand: string;
begin
  Result := False;

  case LowerCase(Command) of
    'play': DBusCommand := 'org.mpris.MediaPlayer2.Player.Play';
    'pause': DBusCommand := 'org.mpris.MediaPlayer2.Player.Pause';
    'playpause': DBusCommand := 'org.mpris.MediaPlayer2.Player.PlayPause';
    'next': DBusCommand := 'org.mpris.MediaPlayer2.Player.Next';
    'previous': DBusCommand := 'org.mpris.MediaPlayer2.Player.Previous';
    else Exit;
  end;

  try
    LinuxExecuteDBusCommand(DBusCommand, Player);
    Result := True;
  except
    Result := False;
  end;
end;

function TSystemMediaController.LinuxGetTrackInfo(Player: TMediaPlayer): TTrackInfo;
begin
  Result := CreateEmptyTrackInfo;
  // Implementation would use D-Bus
end;
{$ENDIF}

// Utility methods
function TSystemMediaController.ExecuteCommand(const Command: string): string;
var
  Process: TProcess;
  OutputStream: TStringStream;
begin
  Result := '';
  Process := TProcess.Create(nil);
  OutputStream := TStringStream.Create;
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'cmd';
    Process.Parameters.Add('/c');
    Process.Parameters.Add(Command);
    {$ELSE}
    Process.Executable := '/bin/sh';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Command);
    {$ENDIF}

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.ShowWindow := swoHide;
    Process.Execute;

    OutputStream.CopyFrom(Process.Output, 0);
    Result := Trim(OutputStream.DataString);
  finally
    Process.Free;
    OutputStream.Free;
  end;
end;

function TSystemMediaController.ExecuteCommandOK(const Command: string): Boolean;
var
  Process: TProcess;
begin
  Result := False;
  Process := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'cmd';
    Process.Parameters.Add('/c');
    Process.Parameters.Add(Command);
    {$ELSE}
    Process.Executable := '/bin/sh';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Command);
    {$ENDIF}

    Process.Options := [poWaitOnExit];
    Process.ShowWindow := swoHide;
    Process.Execute;
    Result := Process.ExitStatus = 0;
  finally
    Process.Free;
  end;
end;

function TSystemMediaController.PlayerToString(Player: TMediaPlayer): string;
begin
  case Player of
    mpSpotify: Result := 'Spotify';
    mpDeezer: Result := 'Deezer';
    mpItunes: Result := 'iTunes';
    mpMusicApp: Result := 'Music';
    mpVLC: Result := 'VLC';
    mpFoobar2000: Result := 'Foobar2000';
    mpWinamp: Result := 'Winamp';
    mpRhythmbox: Result := 'Rhythmbox';
    mpAmarok: Result := 'Amarok';
    else Result := 'Unknown';
  end;
end;

// Volume control
function TSystemMediaController.SetVolume(Volume: Integer): Boolean;
begin
  Result := False;
  Volume := Max(0, Min(100, Volume));

  {$IFDEF WINDOWS}
  Result := WindowsSetVolume(Volume);
  {$ELSE}
  FCurrentState.Volume := Volume;
  Result := True;
  {$ENDIF}
end;

function TSystemMediaController.VolumeUp: Boolean;
begin
  Result := SetVolume(Min(100, FCurrentState.Volume + FVolumeStep));
end;

function TSystemMediaController.VolumeDown: Boolean;
begin
  Result := SetVolume(Max(0, FCurrentState.Volume - FVolumeStep));
end;

// Timer and state management
procedure TSystemMediaController.OnRefreshTimer(Sender: TObject);
begin
  UpdateCurrentState;
end;

procedure TSystemMediaController.UpdateCurrentState;
var
  NewTrack: TTrackInfo;
  NewTrackKey: string;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if FCurrentState.CurrentPlayer = mpUnknown then
      FCurrentState.CurrentPlayer := FindBestAvailablePlayer;

    if FCurrentState.CurrentPlayer <> mpUnknown then
    begin
      NewTrack := GetTrackInfo;
      // detect change by title+artist to avoid false positives when only position changes
      NewTrackKey := Trim(NewTrack.Title + '|' + NewTrack.Artist);
      if NewTrackKey <> FLastTrackTitle then
      begin
        if Assigned(FOnTrackChange) then
          FOnTrackChange(Self, FCurrentState.CurrentTrack, NewTrack);
        FCurrentState.CurrentTrack := NewTrack;
        FLastTrackTitle := NewTrackKey;
      end;
    end;

    FCurrentState.LastUpdate := Now;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

// Property getters
function TSystemMediaController.GetCurrentPlayer: TMediaPlayer;
begin
  Result := FCurrentState.CurrentPlayer;
end;

function TSystemMediaController.GetCurrentTrack: TTrackInfo;
begin
  Result := FCurrentState.CurrentTrack;
end;

function TSystemMediaController.GetCurrentState: TPlaybackState;
begin
  Result := FCurrentState.State;
end;

function TSystemMediaController.GetCurrentVolume: Integer;
begin
  Result := FCurrentState.Volume;
end;

function TSystemMediaController.GetPlayerCount: Integer;
begin
  Result := Length(FAvailablePlayers);
end;

function TSystemMediaController.GetPlayer(Index: Integer): TPlayerInfo;
begin
  if (Index >= 0) and (Index < Length(FAvailablePlayers)) then
    Result := FAvailablePlayers[Index]
  else
  begin
    FillChar(Result, SizeOf(TPlayerInfo), 0);
    Result.Player := mpUnknown;
  end;
end;

// Public methods
function TSystemMediaController.GetTrackInfo: TTrackInfo;
begin
  Result := CreateEmptyTrackInfo;
  if FCurrentState.CurrentPlayer = mpUnknown then Exit;

  {$IFDEF WINDOWS}
  Result := WindowsGetTrackInfo(FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := MacOSGetTrackInfo(FCurrentState.CurrentPlayer);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := LinuxGetTrackInfo(FCurrentState.CurrentPlayer);
  {$ENDIF}
end;

{$IFDEF TEST}
{$I ../../tests/inc/systemmediacontroller_systemmediacontroller.inc}
{$ENDIF}

function TSystemMediaController.UpdateState: Boolean;
begin
  UpdateCurrentState;
  Result := True;
end;

procedure TSystemMediaController.RefreshPlayerList;
begin
  DetectAvailablePlayers;
  FCurrentState.CurrentPlayer := FindBestAvailablePlayer;
end;

function TSystemMediaController.SwitchToPlayer(Player: TMediaPlayer): Boolean;
begin
  Result := IsPlayerAvailable(Player);
  if Result then
    FCurrentState.CurrentPlayer := Player;
end;

function TSystemMediaController.IsPlayerAvailable(Player: TMediaPlayer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FAvailablePlayers) do
    if FAvailablePlayers[I].Player = Player then
    begin
      Result := FAvailablePlayers[I].IsRunning;
      Break;
    end;
end;

// URL-baserade uppspelningsmetoder
function TSystemMediaController.PlayTrackFromURL(const URL: string): Boolean;
var
  DetectedPlayer: TMediaPlayer;
begin
  Result := False;
  if URL = '' then Exit;

  DetectedPlayer := DetectPlayerFromURL(URL);

  case DetectedPlayer of
    mpSpotify: Result := PlaySpotifyTrack(URL);
    mpDeezer: Result := PlayDeezerTrack(URL);
    else
    begin
      // Fallback: open in browser
      {$IFDEF WINDOWS}
      Result := WindowsOpenURL(URL);
      {$ENDIF}
      {$IFDEF DARWIN}
      Result := ExecuteCommandOK('open "' + URL + '"');
      {$ENDIF}
      {$IFDEF LINUX}
      Result := ExecuteCommandOK('xdg-open "' + URL + '"');
      {$ENDIF}
    end;
  end;
end;

function TSystemMediaController.PlaySpotifyTrack(const SpotifyURL: string): Boolean;
var
  TrackID, SpotifyURI: string;
begin
  Result := False;

  // Extrahera Track ID
  TrackID := ExtractTrackID(SpotifyURL, mpSpotify);
  if TrackID = '' then Exit;

  // Create Spotify URI
  SpotifyURI := 'spotify:track:' + TrackID;

  {$IFDEF WINDOWS}
  // Use ShellExecute to open Spotify URI
  Result := WindowsOpenURL(SpotifyURI);
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := MacOSPlaySpotifyURL(SpotifyURI);
  {$ENDIF}

  {$IFDEF LINUX}
  Result := LinuxPlayTrackURL(SpotifyURI, mpSpotify);
  {$ENDIF}
end;

function TSystemMediaController.PlayDeezerTrack(const DeezerURL: string): Boolean;
var
  TrackID: string;
begin
  Result := False;

  TrackID := ExtractTrackID(DeezerURL, mpDeezer);
  if TrackID = '' then Exit;

  {$IFDEF WINDOWS}
  Result := WindowsOpenURL(DeezerURL);
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := MacOSPlayDeezerURL(DeezerURL);
  {$ENDIF}

  {$IFDEF LINUX}
  Result := LinuxPlayTrackURL(DeezerURL, mpDeezer);
  {$ENDIF}
end;

function TSystemMediaController.ExtractTrackID(const URL: string; Player: TMediaPlayer): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';

  case Player of
    mpSpotify:
      begin
        // https://open.spotify.com/track/4iV5W9uYEdYUVa79Axb7Rh
        // eller spotify:track:4iV5W9uYEdYUVa79Axb7Rh
        if Pos('open.spotify.com/track/', URL) > 0 then
        begin
          StartPos := Pos('/track/', URL) + 7;
          EndPos := StartPos;
          while (EndPos <= Length(URL)) and (URL[EndPos] <> '/') and (URL[EndPos] <> '?') and (URL[EndPos] <> '#') do
            Inc(EndPos);
          Result := Copy(URL, StartPos, EndPos - StartPos);
        end
        else if Pos('spotify:track:', URL) > 0 then
        begin
          StartPos := Pos('spotify:track:', URL) + 14;
          EndPos := StartPos;
          while (EndPos <= Length(URL)) and (URL[EndPos] <> '/') and (URL[EndPos] <> '?') and (URL[EndPos] <> '#') do
            Inc(EndPos);
          Result := Copy(URL, StartPos, EndPos - StartPos);
        end;
      end;

    mpDeezer:
      begin
        // https://www.deezer.com/track/123456789
        if Pos('deezer.com/track/', URL) > 0 then
        begin
          StartPos := Pos('/track/', URL) + 7;
          EndPos := StartPos;
          while (EndPos <= Length(URL)) and (URL[EndPos] <> '/') and (URL[EndPos] <> '?') and (URL[EndPos] <> '#') do
            Inc(EndPos);
          Result := Copy(URL, StartPos, EndPos - StartPos);
        end;
      end;
  end;

  // Remove query/fragments/trailing slashes
  Result := Trim(Result);
  while (Result <> '') and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
  EndPos := Pos('?', Result);
  if EndPos > 0 then
    Result := Copy(Result, 1, EndPos - 1);
  EndPos := Pos('#', Result);
  if EndPos > 0 then
    Result := Copy(Result, 1, EndPos - 1);
end;

function TSystemMediaController.DetectPlayerFromURL(const URL: string): TMediaPlayer;
begin
  Result := mpUnknown;

  if (Pos('spotify.com', URL) > 0) or (Pos('spotify:', URL) > 0) then
    Result := mpSpotify
  else if Pos('deezer.com', URL) > 0 then
    Result := mpDeezer;
end;

// Platform-specifika implementationer
{$IFDEF WINDOWS}
function TSystemMediaController.WindowsOpenURL(const URL: string): Boolean;
begin
  Result := ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL) > 32;
end;
{$ENDIF}

{$IFDEF DARWIN}
function TSystemMediaController.MacOSPlaySpotifyURL(const URL: string): Boolean;
var
  Script: string;
begin
  Result := False;
  try
    Script := Format('tell application "Spotify" to play track "%s"', [URL]);
    MacOSExecuteAppleScript(Script);
    Result := True;
  except
    // Fallback: open in browser
    Result := ExecuteCommandOK('open "' + URL + '"');
  end;
end;

function TSystemMediaController.MacOSPlayDeezerURL(const URL: string): Boolean;
begin
  Result := False;
  try
    // Deezer has limited AppleScript support, use browser
    Result := ExecuteCommand('open "' + URL + '"') <> '';
  except
    Result := False;
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
function TSystemMediaController.LinuxPlayTrackURL(const URL: string; Player: TMediaPlayer): Boolean;
var
  PlayerName, Command: string;
begin
  Result := False;

  PlayerName := GetMPRISPlayerName(Player);
  if PlayerName <> '' then
  begin
    // Check if dbus-send is available
    {$IFDEF UNIX}
    if ExeSearch('dbus-send', '') = '' then
    begin
      Result := ExecuteCommand('xdg-open "' + URL + '"') <> '';
      Exit;
    end;
    {$ENDIF}
    try
      // Try D-Bus first
      Command := Format('dbus-send --type=method_call --dest=%s /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.OpenUri string:"%s"',
                       [PlayerName, URL]);
      ExecuteCommand(Command);
      Result := True;
    except
      // Fallback: xdg-open
      Result := ExecuteCommandOK('xdg-open "' + URL + '"');
    end;
  end
  else
    Result := ExecuteCommandOK('xdg-open "' + URL + '"');
end;
{$ENDIF}

end.
