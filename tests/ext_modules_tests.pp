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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-09-25: The unit declares its source codepage (UTF-8) so the
 *   non-ASCII literal in TestJSStringLiteralEscapes is labelled correctly
 *   on Windows.
 *)

{ Tests for trndi.ext.modules: module detection, specifier resolution and the
  generated bootstrap source. None of it touches QuickJS, so these run on
  every target the console runner builds for. }
unit ext_modules_tests;

{$mode objfpc}{$H+}
// The source is UTF-8, and the literals below have to be labelled as such:
// without this, 'åäö' is UTF-8 bytes tagged with the system codepage, which on
// a Windows runner (CP1252) double-encodes on every conversion and made
// TestJSStringLiteralEscapes fail there while passing on the Unixes.
{$codepage utf8}

interface

uses
  fpcunit, testregistry, SysUtils, Classes, trndi.ext.modules
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  TExtModulesTests = class(TTestCase)
  private
    FRoot: string;
    procedure Touch(const RelPath: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStaticImportIsModule;
    procedure TestExportIsModule;
    procedure TestManifestCommentBeforeImport;
    procedure TestDynamicImportIsNotModule;
    procedure TestImportMetaIsNotModule;
    procedure TestCommentedImportIsNotModule;
    procedure TestClassicScriptIsNotModule;
    procedure TestTrndiSpecifierResolves;
    procedure TestRelativeResolvesAgainstImporter;
    procedure TestEntryWithoutDirectoryUsesRoot;
    procedure TestParentInsideRootIsAllowed;
    procedure TestEscapingRootIsRejected;
    procedure TestSymlinkEscapeIsRejected;
    procedure TestBareSpecifierIsRejected;
    procedure TestMissingExtensionFallsBackToJs;
    procedure TestNoRootRejectsFileImports;
    procedure TestJSStringLiteralEscapes;
    procedure TestBootstrapQuotesEntryPath;
    procedure TestTrndiModuleSourceExportsFacade;
  end;

implementation

procedure TExtModulesTests.SetUp;
begin
  FRoot := IncludeTrailingPathDelimiter(GetTempDir(false)) +
    'trndi-modules-' + IntToStr(GetProcessID) + '-' + IntToStr(Random(MaxInt));
  ForceDirectories(FRoot + DirectorySeparator + 'lib');
end;

procedure TExtModulesTests.TearDown;
begin
  DeleteFile(FRoot + DirectorySeparator + 'lib' + DirectorySeparator + 'util.js');
  DeleteFile(FRoot + DirectorySeparator + 'main.js');
  RemoveDir(FRoot + DirectorySeparator + 'lib');
  RemoveDir(FRoot);
end;

procedure TExtModulesTests.Touch(const RelPath: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FRoot + DirectorySeparator +
    StringReplace(RelPath, '/', DirectorySeparator, [rfReplaceAll]), fmCreate);
  f.Free;
end;

{******************************************************************************
  Detection
******************************************************************************}

procedure TExtModulesTests.TestStaticImportIsModule;
begin
  AssertTrue('import x from', ScriptLooksLikeModule(
    'import { fmt } from "./lib/util.js";' + LineEnding + 'fmt(1);'));
  AssertTrue('import string', ScriptLooksLikeModule('import "./side-effect.js";'));
  AssertTrue('import star', ScriptLooksLikeModule('import * as u from "./u.js";'));
  AssertTrue('indented import', ScriptLooksLikeModule('  import x from "./x.js";'));
end;

procedure TExtModulesTests.TestExportIsModule;
begin
  AssertTrue('export function', ScriptLooksLikeModule(
    'const a = 1;' + LineEnding + 'export function clockView() { return "x"; }'));
  AssertTrue('export brace', ScriptLooksLikeModule('export { a };'));
  AssertTrue('export star', ScriptLooksLikeModule('export * from "./x.js";'));
end;

procedure TExtModulesTests.TestManifestCommentBeforeImport;
begin
  AssertTrue('manifest then import', ScriptLooksLikeModule(
    '/*' + LineEnding + '@name Demo' + LineEnding + '@perms net' + LineEnding +
    '*/' + LineEnding + 'import Trndi from "trndi";'));
  AssertTrue('one-line comment then import', ScriptLooksLikeModule(
    '/* @name Demo */ import Trndi from "trndi";'));
  AssertTrue('crlf line endings', ScriptLooksLikeModule(
    '/*'#13#10'@name Demo'#13#10'*/'#13#10'export const x = 1;'));
end;

procedure TExtModulesTests.TestDynamicImportIsNotModule;
begin
  AssertFalse('import()', ScriptLooksLikeModule(
    'import("./lib/util.js").then(m => m.fmt(1));'));
end;

procedure TExtModulesTests.TestImportMetaIsNotModule;
begin
  AssertFalse('import.meta', ScriptLooksLikeModule('import.meta.url;'));
end;

procedure TExtModulesTests.TestCommentedImportIsNotModule;
begin
  AssertFalse('line comment', ScriptLooksLikeModule(
    '// import x from "./x.js";' + LineEnding + 'const y = 1;'));
  AssertFalse('block comment', ScriptLooksLikeModule(
    '/*' + LineEnding + 'import x from "./x.js";' + LineEnding +
    'export const y = 1;' + LineEnding + '*/' + LineEnding + 'const y = 1;'));
end;

procedure TExtModulesTests.TestClassicScriptIsNotModule;
begin
  AssertFalse('plain script', ScriptLooksLikeModule(
    '/* @name Classic */' + LineEnding +
    'function clockView() { return "hi"; }' + LineEnding +
    'const important = 1;' + LineEnding +
    'const exported = 2;'));
  AssertFalse('empty', ScriptLooksLikeModule(''));
end;

{******************************************************************************
  Resolution
******************************************************************************}

procedure TExtModulesTests.TestTrndiSpecifierResolves;
var
  resolved: string;
begin
  AssertEquals('no error', '', ResolveModuleSpecifier('main.js', 'trndi', FRoot, resolved));
  AssertEquals('kept as-is', 'trndi', resolved);
  // The synthetic module needs no folder at all.
  AssertEquals('no root needed', '', ResolveModuleSpecifier('main.js', 'trndi', '', resolved));
end;

procedure TExtModulesTests.TestRelativeResolvesAgainstImporter;
var
  resolved: string;
begin
  Touch('lib/util.js');
  AssertEquals('no error', '', ResolveModuleSpecifier(
    FRoot + DirectorySeparator + 'main.js', './lib/util.js', FRoot, resolved));
  AssertEquals('absolute path', FRoot + DirectorySeparator + 'lib' +
    DirectorySeparator + 'util.js', resolved);
end;

procedure TExtModulesTests.TestEntryWithoutDirectoryUsesRoot;
var
  resolved: string;
begin
  Touch('lib/util.js');
  AssertEquals('no error', '', ResolveModuleSpecifier(
    '<bootstrap>', './lib/util.js', FRoot, resolved));
  AssertEquals('resolved under root', FRoot + DirectorySeparator + 'lib' +
    DirectorySeparator + 'util.js', resolved);
end;

procedure TExtModulesTests.TestParentInsideRootIsAllowed;
var
  resolved: string;
begin
  Touch('main.js');
  AssertEquals('no error', '', ResolveModuleSpecifier(
    FRoot + DirectorySeparator + 'lib' + DirectorySeparator + 'util.js',
    '../main.js', FRoot, resolved));
  AssertEquals('collapsed ..', FRoot + DirectorySeparator + 'main.js', resolved);
end;

procedure TExtModulesTests.TestEscapingRootIsRejected;
var
  resolved, err: string;
begin
  err := ResolveModuleSpecifier(FRoot + DirectorySeparator + 'main.js',
    '../../etc/passwd.js', FRoot, resolved);
  AssertTrue('relative escape rejected: ' + err, Pos('extension folder', err) > 0);
  AssertEquals('nothing resolved', '', resolved);

  err := ResolveModuleSpecifier(FRoot + DirectorySeparator + 'main.js',
    IncludeTrailingPathDelimiter(GetTempDir(false)) + 'elsewhere.js', FRoot, resolved);
  AssertTrue('absolute escape rejected: ' + err, Pos('extension folder', err) > 0);
end;

procedure TExtModulesTests.TestSymlinkEscapeIsRejected;
{$IFDEF UNIX}
var
  resolved, err, outside, link: string;
begin
  // lib/out -> a folder outside the root: the path text stays inside, the
  // file does not.
  outside := FRoot + '-outside';
  link := FRoot + DirectorySeparator + 'lib' + DirectorySeparator + 'out';
  ForceDirectories(outside);
  FileClose(FileCreate(outside + DirectorySeparator + 'evil.js'));
  AssertEquals('symlink created', 0, fpSymlink(pchar(outside), pchar(link)));
  try
    err := ResolveModuleSpecifier(FRoot + DirectorySeparator + 'main.js',
      './lib/out/evil.js', FRoot, resolved);
    AssertTrue('link escape rejected: ' + err, Pos('symbolic link', err) > 0);
    AssertEquals('nothing resolved', '', resolved);
  finally
    DeleteFile(link);
    DeleteFile(outside + DirectorySeparator + 'evil.js');
    RemoveDir(outside);
  end;
end;
{$ELSE}
begin
  // Creating symlinks needs extra privileges on Windows.
end;
{$ENDIF}

procedure TExtModulesTests.TestBareSpecifierIsRejected;
var
  resolved, err: string;
begin
  err := ResolveModuleSpecifier(FRoot + DirectorySeparator + 'main.js',
    'lodash', FRoot, resolved);
  AssertTrue('bare name rejected: ' + err, Pos('bare module', err) > 0);
  AssertTrue('hint names the fix', Pos('./lodash.js', err) > 0);
end;

procedure TExtModulesTests.TestMissingExtensionFallsBackToJs;
var
  resolved: string;
begin
  Touch('lib/util.js');
  AssertEquals('no error', '', ResolveModuleSpecifier(
    FRoot + DirectorySeparator + 'main.js', './lib/util', FRoot, resolved));
  AssertEquals('.js appended', FRoot + DirectorySeparator + 'lib' +
    DirectorySeparator + 'util.js', resolved);
end;

procedure TExtModulesTests.TestNoRootRejectsFileImports;
var
  resolved, err: string;
begin
  err := ResolveModuleSpecifier('main.js', './lib/util.js', '', resolved);
  AssertTrue('needs an extension: ' + err, err <> '');
end;

{******************************************************************************
  Source builders
******************************************************************************}

procedure TExtModulesTests.TestJSStringLiteralEscapes;
begin
  AssertEquals('"C:\\ext\\a \"b\"\n"', string(JSStringLiteral('C:\ext\a "b"' + #10)));
  AssertEquals('"\u0001"', string(JSStringLiteral(#1)));
  // Both sides pass through `string` (the system codepage): the literal is
  // converted from UTF-8 into it, the result from the function's UTF-8 back
  // into it, so they meet in the same encoding on every platform.
  AssertEquals('"åäö"', string(JSStringLiteral('åäö')));
end;

procedure TExtModulesTests.TestBootstrapQuotesEntryPath;
var
  src: string;
begin
  src := string(ModuleBootstrapScript('C:\Users\x\ext\main.js'));
  AssertTrue('import of quoted path', Pos('import("C:\\Users\\x\\ext\\main.js")', src) > 0);
  AssertTrue('error slot present', Pos(ModuleBootstrapGlobal, src) > 0);
  AssertTrue('rethrows once the slot is gone', Pos('else throw e', src) > 0);
  AssertTrue('does not shadow globals', Pos('key in globalThis', src) > 0);
end;

procedure TExtModulesTests.TestTrndiModuleSourceExportsFacade;
var
  src: string;
begin
  src := string(TrndiModuleSource);
  AssertTrue('default export', Pos('export default T', src) > 0);
  AssertTrue('data export', Pos('export const data', src) > 0);
  AssertTrue('on export', Pos('export const on', src) > 0);
end;

initialization
  RegisterTest(TExtModulesTests);

end.
