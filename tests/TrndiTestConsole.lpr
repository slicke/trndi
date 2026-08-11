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
program TrndiTestConsole;

{$mode objfpc}{$H+}
{$DEFINE TEST}

uses
  {$IFDEF UNIX}cthreads, {$ENDIF}
  sysutils, fpcunit, testregistry, testreport,
  dexcom_time_tests,
  dexcom_trend_tests,
  tandem_trend_tests,
  tandem_fixture_tests,
  api_tandem_treatment_tests,
  debug_intermit_test,
  debug_firstx_dexcom_test,
  debug_firstx_tandem_test,
  trndi_native_mock_test,
  umain_tests,
  native_cookie_tests,
  api_general_tests,
  api_registry_tests,
  api_dexcom_tests,
  api_dexcom_new_tests,
  api_xdrip_tests,
  api_carelink_tests,
  api_carelink_treatment_tests,
  carelink_time_tests,
  api_librelinkup_tests,
  api_nightscout_tests,
  api_nightscout3_tests,
  api_nightscout3_treatment_tests,
  alert_engine_tests,
    system_media_controller_tests,
  ext_manifest_tests
  // Only where externals/quickjs ships a prebuilt engine (see the project's
  // Conditionals); other targets have nothing to link against.
  {$IFDEF HAVE_QUICKJS}, ext_js_tests{$ENDIF};

var
  LResult: TTestResult;
  LWriter: TPlainResultsWriter;

begin
  // Run all registered tests and report results to stdout
  LResult := TTestResult.Create;
  try
    LWriter := TPlainResultsWriter.Create;
    try
      LResult.AddListener(LWriter);
      GetTestRegistry.Run(LResult);
      LWriter.WriteResult(LResult);
      Halt(LResult.NumberOfFailures + LResult.NumberOfErrors);
    finally
      LWriter.Free;
    end;
  finally
    LResult.Free;
  end;
end.
