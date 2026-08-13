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
unit trndi.native.bsd;

{**
  @abstract(BSD-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeBSD), a subclass of
  @link(TTrndiNativeLinux). Most behavior is shared with Linux (INI files,
  libcurl, notify-send, spd-say), but BSD adds fallbacks for setups where
  the Linux tooling is missing: espeak-ng/espeak/flite for TTS, and kdialog
  for notifications.

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
    BSD systems mostly use the same tools and conventions as Linux; this
    subclass adds fallbacks for the pieces that are often missing on BSD
    (speech-dispatcher, notify-send).
  }
TTrndiNativeBSD = class(TTrndiNativeLinux)
  public
    {** TTS: prefer the inherited spd-say path; fall back to
        espeak-ng/espeak/flite when speech-dispatcher is absent. }
    class function SpeakAvailable: boolean; override;
    class function SpeakSoftwareName: string; override;
    procedure Speak(const Text: string); override;

    {** Notifications: inherited gdbus/notify-send path, with kdialog
        preferred on KDE sessions and used as a fallback elsewhere. }
    class function isNotificationSystemAvailable: boolean; override;
    class function getNotificationSystem: string; override;
    procedure attention(topic, message: string); override;

    // BSD-specific overrides can be added here later.
  end;

implementation

uses
  Classes, SysUtils, Dialogs, trndi.log, trndi.native.async;

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
  Args: array of string;

  procedure AddArgs(const a: array of string);
  var
    i, base: Integer;
  begin
    base := Length(Args);
    SetLength(Args, base + Length(a));
    for i := 0 to High(a) do
      Args[base + i] := a[i];
  end;

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

    Args := nil;
    if EspeakVoice <> '' then
      AddArgs(['-v', EspeakVoice]);
    if Rate <> 0 then
      AddArgs(['-s', IntToStr(EspeakWPM)]);
    AddArgs([Text]);

    // Fire-and-forget via the async worker: doesn't block the UI, and the
    // worker thread reaps the child process so zombies don't accumulate.
    RunAndCaptureSimpleAsync(CmdPath, Args, nil);
    TrndiDLog(Format('TTS: %s fallback used (voice=%s rate=%d)', [EngineName, EspeakVoice, EspeakWPM]));
    Exit;
  end;

  // Try flite fallback (no voice/rate mapping currently)
  CmdPath := ExecInPath('flite');
  if CmdPath <> '' then
  begin
    RunAndCaptureSimpleAsync(CmdPath, ['-t', Text], nil);
    TrndiDLog('TTS: flite fallback used');
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
  Notifications (BSD)
  -------------------
  The inherited gdbus/notify-send path is the default; kdialog is preferred
  on KDE-like sessions and used as a last resort on any desktop when the
  inherited tooling is missing. UseKDialog keeps availability, the reported
  system name and the actual send path in agreement.
------------------------------------------------------------------------------}
function HasGuiDisplay: boolean;
begin
  Result := (GetEnvironmentVariable('DISPLAY') <> '') or
    (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '');
end;

function IsKDESession: boolean;
var
  d: string;
begin
  d := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
  if d = '' then
    d := GetEnvironmentVariable('DESKTOP_SESSION');
  Result := Pos('KDE', UpperCase(d)) > 0;
end;

function UseKDialog: boolean;
begin
  Result := (ExecInPath('kdialog') <> '') and HasGuiDisplay and
    (IsKDESession or not TTrndiNativeLinux.isNotificationSystemAvailable);
end;

class function TTrndiNativeBSD.isNotificationSystemAvailable: boolean;
begin
  if UseKDialog then
    Exit(True);

  Result := inherited isNotificationSystemAvailable;
end;

class function TTrndiNativeBSD.getNotificationSystem: string;
begin
  if UseKDialog then
  begin
    TrndiDLog('Notification system: using kdialog on BSD');
    Exit('kdialog');
  end;

  Result := inherited getNotificationSystem;
end;

procedure TTrndiNativeBSD.attention(topic, message: string);
begin
  if UseKDialog then
  begin
    // Fire-and-forget via the async worker so the child gets reaped (no zombies)
    RunAndCaptureSimpleAsync(ExecInPath('kdialog'),
      ['--title', topic, '--passivepopup', message, '5'], nil);
    Exit;
  end;

  inherited attention(topic, message);
end;

end.
