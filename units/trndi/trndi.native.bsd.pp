unit trndi.native.bsd;

{**
  @abstract(BSD-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeBSD) which is simply an alias to
  @link(TTrndiNativeLinux) since BSD systems use the same tooling and
  conventions as Linux (notify-send, spd-say, INI files, libcurl, etc.).

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
  trndi.native.linux;

type
  {!
    @abstract(BSD implementation - subclass of Linux implementation.)
    BSD systems use the same tools and conventions as Linux. Declaring a
    distinct subclass allows BSD-specific overrides in future without
    changing callers.
  }
TTrndiNativeBSD = class(TTrndiNativeLinux)
  public
    {** TTS: allow BSD to alter detection/behavior later without changing callers. }
    class function SpeakAvailable: boolean; override;
    class function SpeakSoftwareName: string; override;
    procedure Speak(const Text: string); override;

    {** Notifications: same rationale — delegate today, override later if needed. }
    class function isNotificationSystemAvailable: boolean; override;
    class function getNotificationSystem: string; override;

    // BSD-specific overrides can be added here later.
  end;

implementation

uses
  Classes, SysUtils, Process, Dialogs;

{------------------------------------------------------------------------------
  BSD: TTS fallback + small helpers.
  - Prefer the Linux/spd-say implementation (inherited) when available.
  - Fallback order: spd-say (inherited) → espeak → flite.
  - SpeakAvailable/SpeakSoftwareName reflect available fallback engines.
------------------------------------------------------------------------------}

function ExecInPath(const FileName: string): string;
var
  PathVar: string;
  Paths: TStringList;
  i: Integer;
  Dir: string;
begin
  Result := '';
  PathVar := GetEnvironmentVariable('PATH');
  if PathVar = '' then
    Exit;
  Paths := TStringList.Create;
  try
    Paths.Delimiter := ':';
    Paths.StrictDelimiter := True;
    Paths.DelimitedText := PathVar;
    for i := 0 to Paths.Count - 1 do
    begin
      Dir := IncludeTrailingPathDelimiter(Paths[i]);
      if FileExists(Dir + FileName) then
        Exit(Dir + FileName);
    end;
  finally
    Paths.Free;
  end;
end;

class function TTrndiNativeBSD.SpeakAvailable: boolean;
begin
  // Prefer Linux implementation (spd-say). If not present, check common BSD TTS.
  if inherited SpeakAvailable then
    Exit(True);
  Result := (ExecInPath('espeak') <> '') or (ExecInPath('flite') <> '');
end;

class function TTrndiNativeBSD.SpeakSoftwareName: string;
begin
  if inherited SpeakAvailable then
    Exit(inherited SpeakSoftwareName);
  if ExecInPath('espeak') <> '' then
    Exit('espeak');
  if ExecInPath('flite') <> '' then
    Exit('flite');
  Result := '';
end;

procedure TTrndiNativeBSD.Speak(const Text: string);
var
  CmdPath: string;
  Rate: Integer;
  Proc: TProcess;
begin
  // If spd-say is available, reuse Linux implementation which already
  // handles language/voice/rate settings.
  if inherited SpeakAvailable then
  begin
    inherited Speak(Text);
    Exit;
  end;

  // Try espeak fallback
  CmdPath := ExecInPath('espeak');
  if CmdPath <> '' then
  begin
    Rate := GetIntSetting('tts.rate', 0);
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := CmdPath;
      // espeak: -s speed (words per minute)
      if Rate <> 0 then
        Proc.Parameters.Add('-s' + IntToStr(Rate));
      Proc.Parameters.Add(Text);
      Proc.Options := [];
      Proc.Execute; // run asynchronously
    except
      on E: Exception do
      begin
        if not ttsErrorShown then
        begin
          ShowMessage('TTS Error: ' + E.Message);
          ttsErrorShown := true;
        end;
      end;
    end;
    Exit;
  end;

  // Try flite fallback
  CmdPath := ExecInPath('flite');
  if CmdPath <> '' then
  begin
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := CmdPath;
      Proc.Parameters.Add('-t');
      Proc.Parameters.Add(Text);
      Proc.Options := [];
      Proc.Execute;
    except
      on E: Exception do
      begin
        if not ttsErrorShown then
        begin
          ShowMessage('TTS Error: ' + E.Message);
          ttsErrorShown := true;
        end;
      end;
    end;
    Exit;
  end;

  // No engine available — show a single error message to the user.
  if not ttsErrorShown then
  begin
    ShowMessage('Error: no TTS engine available. Install speech-dispatcher, espeak or flite.');
    ttsErrorShown := true;
  end;
end;

{------------------------------------------------------------------------------
  Notification stubs still delegate to Linux implementation.
------------------------------------------------------------------------------}
class function TTrndiNativeBSD.isNotificationSystemAvailable: boolean;
begin
  Result := inherited isNotificationSystemAvailable;
end;

class function TTrndiNativeBSD.getNotificationSystem: string;
begin
  Result := inherited getNotificationSystem;
end;

end.
