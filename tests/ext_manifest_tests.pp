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
 *   from use of this software.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)

unit ext_manifest_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, trndi.ext.perm;

type
  TExtensionManifestTests = class(TTestCase)
  published
    procedure TestExplicitManifest;
    procedure TestLegacyManifest;
    procedure TestUnknownPermissionIsRejected;
    procedure TestUtf8BomIsAccepted;
  end;

implementation

procedure TExtensionManifestTests.TestExplicitManifest;
var
  manifest: TExtManifest;
begin
  manifest := ParseExtManifest('/*' + LineEnding +
    '@name Example extension' + LineEnding +
    '@copyright (c) Example author' + LineEnding +
    '@version 1.2.3' + LineEnding +
    '@description Demonstrates manifests' + LineEnding +
    '@homepage https://example.com/extension' + LineEnding +
    '@license MIT' + LineEnding +
    '@perms net, settings' + LineEnding +
    '*/');

  AssertTrue('manifest should be valid', manifest.IsValid);
  AssertEquals('name', 'Example extension', manifest.DisplayName);
  AssertEquals('copyright', '(c) Example author', manifest.Author);
  AssertEquals('version', '1.2.3', manifest.Version);
  AssertEquals('description', 'Demonstrates manifests', manifest.Description);
  AssertEquals('homepage', 'https://example.com/extension', manifest.Homepage);
  AssertEquals('license', 'MIT', manifest.License);
  AssertTrue('net requested', epNet in manifest.Requested);
  AssertTrue('settings requested', epSettings in manifest.Requested);
end;

procedure TExtensionManifestTests.TestLegacyManifest;
var
  manifest: TExtManifest;
begin
  manifest := ParseExtManifest('/* Legacy extension' + LineEnding +
    '(c) Existing author' + LineEnding + '@perms exec */');

  AssertTrue('legacy manifest should remain valid', manifest.IsValid);
  AssertEquals('legacy name', 'Legacy extension', manifest.DisplayName);
  AssertEquals('legacy author', '(c) Existing author', manifest.Author);
  AssertTrue('exec requested', epExec in manifest.Requested);
end;

procedure TExtensionManifestTests.TestUnknownPermissionIsRejected;
var
  manifest: TExtManifest;
begin
  manifest := ParseExtManifest('/*' + LineEnding + '@name Invalid' + LineEnding +
    '@perms net, netwerk' + LineEnding + '*/');

  AssertFalse('unknown permissions must reject the manifest', manifest.IsValid);
  AssertTrue('error identifies the unknown permission',
    Pos('netwerk', manifest.ErrorMessage) > 0);
end;

procedure TExtensionManifestTests.TestUtf8BomIsAccepted;
var
  manifest: TExtManifest;
begin
  manifest := ParseExtManifest(#$EF#$BB#$BF + '/* @name BOM extension */');

  AssertTrue('manifest after BOM should be valid', manifest.IsValid);
  AssertEquals('name after BOM', 'BOM extension', manifest.DisplayName);
end;

initialization
  RegisterTest(TExtensionManifestTests);

end.
