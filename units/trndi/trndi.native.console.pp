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

{**
  @abstract(Console/TUI implementation of @link(TTrndiNativeGeneric).)

  This unit defines @link(TTrndiNativeConsole) for front ends built without a
  widgetset (command-line tools, Free Vision TUIs). Settings and HTTP come from
  @link(TTrndiNativeGeneric) (INI file + TFPHTTPClient); the genuinely native
  parts degrade gracefully:

  - Notifications go through @code(notify-send) when a desktop session provides
    one, otherwise they are written to @code(stderr) so they end up in the
    terminal or the service log.
  - Speech uses @code(spd-say) or @code(espeak) when available.
  - Badges and window-manager integration are no-ops.
  - HTTP goes through libcurl (@code(trndi.native.request.curl) — the same
    transport the desktop Linux build uses), overriding the generic base's
    TFPHTTPClient. libcurl brings the system TLS stack and CA store, avoiding
    FPC's OpenSSL version-loading fragility on modern distributions.

  Selected by @code(trndi.native)'s dispatch when the build defines
  @code(X_CONSOLE) (a build-mode define, not a platform one — any OS target can
  set it).

  Threading: notifications and speech are launched through
  @code(trndi.native.async), which runs each child on a worker thread so it gets
  waited on rather than left as a zombie. A console program must therefore link
  a thread driver — put @code(cthreads) first in the program's uses clause on
  Unix, as LCL builds do implicitly. Without one the RTL aborts with runtime
  error 232 on the first notification, which no @code(try..except) can catch.
}

unit trndi.native.console;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, trndi.native.base, trndi.native.generic,
trndi.native.request.curl;

type
  {!
    @abstract(Console implementation of @link(TTrndiNativeGeneric).)
    notify-send/stderr for notifications, spd-say/espeak for speech; settings
    and HTTP come from the generic base.
  }
TTrndiNativeConsole = class(TTrndiNativeGeneric)
public
    {** Send a desktop notification via notify-send, falling back to stderr. }
  procedure attention(topic, message: string); override; overload;
    {** Speaks @param(Text) using spd-say or espeak, if available. }
  procedure Speak(const Text: string); override;
    {** Badges have no meaning in a terminal; no-op. }
  procedure setBadge(const Value: string; BadgeColor: TColor;
    badge_size_ratio: double; min_font_size: integer); override;
    {** True when notify-send resolves in PATH (stderr fallback always works,
        but callers use this to decide whether a toast will actually appear). }
  class function isNotificationSystemAvailable: boolean; override;
    {** Identify notification backend: 'notify-send' or 'stderr'. }
  class function getNotificationSystem: string; override;
    {** Check whether TTS is available (spd-say or espeak in PATH). }
  class function SpeakAvailable: boolean; override;
    {** Name of the TTS helper this platform expects. }
  class function SpeakSoftwareName: string; override;
    {** No window manager from a terminal's point of view. }
  class function GetWindowManagerName: string; override;

  // HTTP: libcurl transport (shared with desktop Linux), replacing the
  // generic base's TFPHTTPClient implementations.
    {** Simple HTTP GET via libcurl. }
  class function getURL(const url: string; out res: string): boolean; override;
    {** Simple HTTP POST via libcurl. }
  class function postURL(const url: string; const body: string;
    const contentType: string; out res: string): boolean; override;
    {** Proxy-only HTTP GET via libcurl (settings "Test proxy" action). }
  class function TestProxyURL(const url: string; const proxyHost: string;
    const proxyPort: string; const proxyUser: string; const proxyPass: string;
    out res: string): boolean; override;
    {** HTTP GET/POST via libcurl, honouring proxy.* root settings. }
  function request(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    const header: string = ''; prefix: boolean = true): string; override;
    {** Cookie-aware, redirect-following HTTP via libcurl. }
  function requestEx(const post: boolean; const endpoint: string;
    const params: array of string; const jsondata: string = '';
    cookieJar: TStringList = nil; followRedirects: boolean = true;
    maxRedirects: integer = 10; customHeaders: TStringList = nil;
    prefix: boolean = true): THTTPResponse; override;
end;

implementation

uses
trndi.native.async;

{------------------------------------------------------------------------------
  FindSpeechCmd
  -------------
  First TTS helper found in PATH: spd-say (speech-dispatcher, present on most
  desktop Linux) then espeak. Empty string when neither resolves.
 ------------------------------------------------------------------------------}
function FindSpeechCmd: string;
begin
  if TTrndiNativeConsole.ToolAvailable('spd-say') then
    Exit('spd-say');
  if TTrndiNativeConsole.ToolAvailable('espeak') then
    Exit('espeak');
  Result := '';
end;

{------------------------------------------------------------------------------
  attention
  ---------
  Prefer a real desktop toast (a TUI often runs inside a terminal on a desktop
  session); otherwise write to stderr so the message lands in the terminal or
  journal rather than vanishing.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeConsole.attention(topic, message: string);
begin
  if ToolAvailable('notify-send') then
  try
    // Fire-and-forget via the async worker, as the Linux unit does: the worker
    // thread waits on the child, so notifications don't leave zombies behind.
    RunAndCaptureSimpleAsync('notify-send',
      ['--app-name=Trndi', topic, message], nil);
    Exit;
  except
    // Fall through to the stderr path below
  end;
  Flush(StdErr);
  Writeln(StdErr, Format('[Trndi] %s: %s', [topic, message]));
end;

{------------------------------------------------------------------------------
  Speak
  -----
  Fire-and-forget via the async worker, as the Linux unit does: speech doesn't
  block the caller, and the worker thread waits on the child so it gets reaped.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeConsole.Speak(const Text: string);
var
  cmd: string;
begin
  cmd := FindSpeechCmd;
  if cmd = '' then
    Exit;

  try
    RunAndCaptureSimpleAsync(cmd, [Text], nil);
  except
    // Silently fail if speech doesn't work
  end;
end;

{------------------------------------------------------------------------------
  setBadge
  --------
  No tray, no taskbar: nothing to draw on. The TUI shows the value itself.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeConsole.setBadge(const Value: string; BadgeColor: TColor;
badge_size_ratio: double; min_font_size: integer);
begin
  // Intentionally empty
end;

class function TTrndiNativeConsole.isNotificationSystemAvailable: boolean;
begin
  Result := ToolAvailable('notify-send');
end;

class function TTrndiNativeConsole.getNotificationSystem: string;
begin
  if ToolAvailable('notify-send') then
    Result := 'notify-send'
  else
    Result := 'stderr';
end;

class function TTrndiNativeConsole.SpeakAvailable: boolean;
begin
  Result := FindSpeechCmd <> '';
end;

class function TTrndiNativeConsole.SpeakSoftwareName: string;
begin
  Result := 'spd-say or espeak';
end;

class function TTrndiNativeConsole.GetWindowManagerName: string;
begin
  Result := 'console';
end;

{------------------------------------------------------------------------------
  HTTP via the shared libcurl transport. The class functions read proxy
  settings through a short-lived instance, exactly like the Linux unit.
 ------------------------------------------------------------------------------}
class function TTrndiNativeConsole.getURL(const url: string; out res: string): boolean;
var
  tempInstance: TTrndiNativeConsole;
  proxy: TCurlProxy;
begin
  tempInstance := TTrndiNativeConsole.Create;
  tempInstance.noFree := true;
  try
    proxy := FetchCurlProxy(tempInstance);
  finally
    tempInstance.Free;
  end;
  Result := CurlGetURL(url, proxy, res);
end;

class function TTrndiNativeConsole.postURL(const url: string; const body: string;
const contentType: string; out res: string): boolean;
var
  tempInstance: TTrndiNativeConsole;
  proxy: TCurlProxy;
begin
  tempInstance := TTrndiNativeConsole.Create;
  tempInstance.noFree := true;
  try
    proxy := FetchCurlProxy(tempInstance);
  finally
    tempInstance.Free;
  end;
  Result := CurlPostURL(url, body, contentType, proxy, res);
end;

class function TTrndiNativeConsole.TestProxyURL(const url: string;
const proxyHost: string; const proxyPort: string; const proxyUser: string;
const proxyPass: string; out res: string): boolean;
begin
  Result := CurlTestProxyURL(url, proxyHost, proxyPort, proxyUser, proxyPass, res);
end;

function TTrndiNativeConsole.request(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
const header: string; prefix: boolean): string;
begin
  Result := CurlRequest(post, baseurl, useragent, endpoint, params, jsondata,
    header, prefix, FetchCurlProxy(self));
end;

function TTrndiNativeConsole.requestEx(const post: boolean; const endpoint: string;
const params: array of string; const jsondata: string;
cookieJar: TStringList; followRedirects: boolean;
maxRedirects: integer; customHeaders: TStringList;
prefix: boolean): THTTPResponse;
begin
  Result := CurlRequestEx(post, baseurl, useragent, endpoint, params, jsondata,
    cookieJar, followRedirects, maxRedirects, customHeaders, prefix,
    FetchCurlProxy(self));
end;

end.
