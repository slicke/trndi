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
unit test_server_helper;

{$mode objfpc}{$H+}

interface

function StartOrUseTestServer(out BaseURL: string): boolean;
procedure StopLocalTestServer;

implementation

uses
  SysUtils, fphttpclient, pascal_testserver;

var
  RunningNoticePrinted: Boolean = False;
  EmbeddedStarted: Boolean = False;
  EmbeddedBaseURL: string = '';

function StartOrUseTestServer(out BaseURL: string): boolean;
var
  env: string;
  client: TFPHTTPClient;
  i: Integer;
begin
  Result := False;
  BaseURL := '';

  // Honor an explicit skip for CI / sandboxed runs
  if GetEnvironmentVariable('TRNDI_NO_TESTSERVER') = '1' then
    Exit(False);

  // Reuse an externally-launched server when the caller points at one
  env := GetEnvironmentVariable('TRNDI_TEST_SERVER_URL');
  if env <> '' then
  begin
    BaseURL := env;
    Exit(True);
  end;

  // Reuse the in-process server if it's already running
  if EmbeddedStarted then
  begin
    BaseURL := EmbeddedBaseURL;
    Exit(True);
  end;

  if not RunningNoticePrinted then
  begin
    Writeln('Starting embedded Pascal test server');
    RunningNoticePrinted := True;
  end;

  if not StartPascalTestServer(EmbeddedBaseURL) then
    Exit(False);

  // Poll /debug for readiness (up to ~3s)
  client := TFPHTTPClient.Create(nil);
  try
    for i := 1 to 30 do
    begin
      try
        client.Get(EmbeddedBaseURL + '/debug');
        EmbeddedStarted := True;
        BaseURL := EmbeddedBaseURL;
        Exit(True);
      except
        Sleep(100);
      end;
    end;
  finally
    client.Free;
  end;

  StopPascalTestServer;
  Result := False;
end;

procedure StopLocalTestServer;
begin
  // No-op: the embedded server runs for the process lifetime so successive
  // tests reuse it without paying re-bind cost. Process exit reclaims the port.
end;

end.
