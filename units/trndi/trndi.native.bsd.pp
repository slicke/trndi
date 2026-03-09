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
  Classes, SysUtils, Process, Dialogs, trndi.log;

{------------------------------------------------------------------------------
  BSD: TTS fallback + small helpers.
  - Prefer the Linux/spd-say implementation (inherited) when available.
  - Fallback order: spd-say (inherited) → espeak-ng → espeak → flite.
  - SpeakAvailable/SpeakSoftwareName reflect available fallback engines.
------------------------------------------------------------------------------}

function ExecInPath(const FileName: string): string;
var
  PathVar: string;
  Paths: TStringList;
  i: Integer;
  Dir: string;
  ExtraDirs: array[0..3] of string = ('/usr/local/bin', '/usr/pkg/bin', '/usr/sbin', '/sbin');
  j: Integer;
begin
  Result := '';
  PathVar := GetEnvironmentVariable('PATH');
  if PathVar <> '' then
  begin
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

  // Check common extra locations (FreeBSD/NetBSD/pkg convention)
  for j := Low(ExtraDirs) to High(ExtraDirs) do
  begin
    Dir := IncludeTrailingPathDelimiter(ExtraDirs[j]);
    if FileExists(Dir + FileName) then
      Exit(Dir + FileName);
  end;
end;

class function TTrndiNativeBSD.SpeakAvailable: boolean;
begin
  // Prefer Linux implementation (spd-say). If not present, check common BSD TTS.
  if inherited SpeakAvailable then
    Exit(True);
  Result := (ExecInPath('espeak-ng') <> '') or (ExecInPath('espeak') <> '') or (ExecInPath('flite') <> '');
end;

class function TTrndiNativeBSD.SpeakSoftwareName: string;
begin
  if inherited SpeakAvailable then
    Exit(inherited SpeakSoftwareName);
  if ExecInPath('espeak-ng') <> '' then
    Exit('espeak-ng');
  if ExecInPath('espeak') <> '' then
    Exit('espeak');
  if ExecInPath('flite') <> '' then
    Exit('flite');
  Result := '';
end;

procedure TTrndiNativeBSD.Speak(const Text: string);
var
  CmdPath, VoiceType, EspeakVoice, EngineName: string;
  Rate, EspeakWPM: Integer;
  Proc: TProcess;

  function GetLangPrefix: string;
  var
    L: string;
    P: Integer;
  begin
    L := GetEnvironmentVariable('LC_ALL');
    if L = '' then
      L := GetEnvironmentVariable('LANGUAGE');
    if L = '' then
      L := GetEnvironmentVariable('LANG');
    if L = '' then
      Exit('en');
    P := Pos(':', L);
    if P > 0 then
      L := Copy(L, 1, P - 1);
    P := Pos('.', L);
    if P > 0 then
      L := Copy(L, 1, P - 1);
    P := Pos('-', L);
    if P > 0 then
      L := Copy(L, 1, P - 1);
    Result := LowerCase(L);
  end;

  function MapEspeakVoice(const VName: string): string;
  var
    LPrefix: string;
  begin
    if (VName = '') or (VName = 'Default') then
      Exit('');
    LPrefix := GetLangPrefix;
    if LPrefix = '' then
      LPrefix := 'en';
    if VName = 'Male 1' then
      Result := LPrefix + '+m1'
    else if VName = 'Male 2' then
      Result := LPrefix + '+m2'
    else if VName = 'Male 3' then
      Result := LPrefix + '+m3'
    else if VName = 'Female 1' then
      Result := LPrefix + '+f1'
    else if VName = 'Female 2' then
      Result := LPrefix + '+f2'
    else if VName = 'Female 3' then
      Result := LPrefix + '+f3'
    else
      Result := ''; // unknown mapping -> let espeak default
  end;

begin
  // If spd-say is available, reuse Linux implementation which already
  // handles language/voice/rate settings.
  if inherited SpeakAvailable then
  begin
    inherited Speak(Text);
    Exit;
  end;

  VoiceType := GetSetting('tts.voice.name', '');
  Rate := GetIntSetting('tts.rate', 0);

  // Try espeak-ng/espeak fallback with voice/rate mapping
  CmdPath := ExecInPath('espeak-ng');
  EngineName := 'espeak-ng';
  if CmdPath = '' then
  begin
    CmdPath := ExecInPath('espeak');
    EngineName := 'espeak';
  end;
  if CmdPath <> '' then
  begin
    EspeakVoice := MapEspeakVoice(VoiceType);

    // Map UI rate (-100..100) to espeak WPM (default ~175)
    EspeakWPM := Round(175 * (1 + Rate / 100.0));
    if EspeakWPM < 50 then
      EspeakWPM := 50;
    if EspeakWPM > 450 then
      EspeakWPM := 450;

    Proc := TProcess.Create(nil);
    try
      Proc.Executable := CmdPath;
      if EspeakVoice <> '' then
      begin
        Proc.Parameters.Add('-v');
        Proc.Parameters.Add(EspeakVoice);
      end;
      if Rate <> 0 then
        Proc.Parameters.Add('-s' + IntToStr(EspeakWPM));
      Proc.Parameters.Add(Text);
      Proc.Options := [];
      Proc.Execute; // run asynchronously

      TrndiDLog(Format('TTS: %s fallback used (voice=%s rate=%d)', [EngineName, EspeakVoice, EspeakWPM]));
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

  // Try flite fallback (no voice/rate mapping currently)
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

      TrndiDLog('TTS: flite fallback used');
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
    ShowMessage('Error: no TTS engine available. Install speech-dispatcher, espeak/espeak-ng or flite.');
    ttsErrorShown := true;
  end;
end;

{------------------------------------------------------------------------------
  Notification stubs still delegate to Linux implementation.
------------------------------------------------------------------------------}
class function TTrndiNativeBSD.isNotificationSystemAvailable: boolean;
begin
  // If kdialog exists and a display is available, consider notifications available
  if (ExecInPath('kdialog') <> '') and
    ((GetEnvironmentVariable('DISPLAY') <> '') or (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '')) then
    Exit(True);

  Result := inherited isNotificationSystemAvailable;
end;

class function TTrndiNativeBSD.getNotificationSystem: string;
var
  d: string;
begin
  // Prefer kdialog on KDE-like sessions when available
  if ExecInPath('kdialog') <> '' then
  begin
    d := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
    if d = '' then
      d := GetEnvironmentVariable('DESKTOP_SESSION');
    if Pos('KDE', UpperCase(d)) > 0 then
    begin
      TrndiDLog('Notification system: using kdialog on BSD');
      Exit('kdialog');
    end;
  end;

  Result := inherited getNotificationSystem;
end;

end.
