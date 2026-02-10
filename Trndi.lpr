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
program trndi;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
{$IFNDEF DARWIN}
{$ifndef HAIKU}
{$linklib gcc}  // We cant link with the QUickJS lib on Linux otherwise
cthreads,
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF HASAMIGA}
athreads,
{$ENDIF}
LCLTranslator,
Interfaces, // this includes the LCL widgetset
Forms,lazcontrols,trndi.types,trndi.native,
{$IFDEF TrndiExt}
trndi.ext.functions,
{$ENDIF}
{$IFDEF DEBUG}
sysutils,
{$ENDIF}
trndi.api.dexcom, umain, uconf, ufloat, slicke.ux.alert,
buildinfo, razer.chroma.factory
{ you can add units after this };

{$R *.res}

begin
{$IFDEF DEBUG}
  // Set up -gh output for the Leakview package:
if FileExists('heap.trc') then
  DeleteFile('heap.trc');
SetHeapTraceOutput('heap.trc');
{$ENDIF DEBUG}
RequireDerivedFormResource:=true;
  Application.Scaled:=True;
Application.{%H-}MainFormOnTaskbar:=true;
Application.Initialize;
Application.CreateForm(TfBG, fBG);
Application.CreateForm(TfFloat, fFloat);
Application.Run;
end.
