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
unit trndi_native_mock_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, trndi.native, SysUtils;

type
  TTrndiNativeMockTest = class(TTestCase)
  published
    procedure TestSettings;
    procedure TestHTTPExample;
    procedure TestProxyURLInvalid;
  end;

implementation

procedure TTrndiNativeMockTest.TestSettings;
var
  n: TTrndiNativeBase;
begin
  n := TrndiNative.Create;
  try
    n.SetSetting('unittest.testkey', 'unittest-value');
    AssertEquals('unittest-value', n.GetSetting('unittest.testkey', ''));
    n.DeleteSetting('unittest.testkey');
    AssertEquals('', n.GetSetting('unittest.testkey', ''));
  finally
    n.Free;
  end;
end;

procedure TTrndiNativeMockTest.TestHTTPExample;
var
  res: string;
  ok: boolean;
begin
  // Allow skipping network tests by setting TRNDI_OFFLINE_TESTS=1 in environment
  if GetEnvironmentVariable('TRNDI_OFFLINE_TESTS') = '1' then
    Exit;

  ok := TrndiNative.getURL('http://example.com', res);
  AssertTrue(ok);
  AssertTrue(Pos('Example Domain', res) > 0);
end;

procedure TTrndiNativeMockTest.TestProxyURLInvalid;
var
  res: string;
  ok: boolean;
begin
  ok := TrndiNative.TestProxyURL('http://example.com', '127.0.0.1:9', '9', '', '', res);
  // Expect false when proxy is not reachable
  AssertFalse(ok);
end;

initialization
  RegisterTest(TTrndiNativeMockTest);

end.
