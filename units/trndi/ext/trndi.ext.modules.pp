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
 * - 2026-09-25: JSStringLiteral converts its input to UTF-8 before escaping
 *   instead of copying the bytes of a system-codepage string under a UTF-8
 *   label, which handed QuickJS invalid UTF-8 for non-ASCII paths on Windows.
 *)

{** ES module support for extensions: the pure, engine-independent half.

    The QuickJS side (normalizer and loader callbacks, evaluation) lives in
    @code(trndi.ext.engine). Everything here is plain string and path work so
    it can be unit-tested by the console runner, which has no engine.

    - @link(ScriptLooksLikeModule) decides whether an extension file is
      evaluated as an ES module or as a classic global script.
    - @link(ResolveModuleSpecifier) turns an @code(import) specifier into an
      absolute file path, confined to the extension folder.
    - @link(ModuleBootstrapScript) is the classic script that loads a module
      extension through dynamic @code(import()) and publishes its exported
      functions as the globals Trndi's callback lookup expects.
    - @link(TrndiModuleSource) is the synthetic @code("trndi") module. }
unit trndi.ext.modules;

{$mode objfpc}{$H+}

interface

uses
SysUtils;

const
  {** Bare specifier of the synthetic module that re-exports the @code(Trndi)
      object: @code(import Trndi from "trndi"). }
  TrndiModuleSpecifier = 'trndi';

  {** Prefix the normalizer puts in front of a resolution error. QuickJS only
      lets the normalizer hand back a name, so the failure rides along in the
      name and the loader turns it into a readable exception. }
  ModuleErrorPrefix = 'trndi-module-error:';

  {** Global object the bootstrap script uses to hand a load error back to
      Pascal while the load is still synchronous. }
  ModuleBootstrapGlobal = '__trndiModule';

{** True when @code(Src) contains a top-level static @code(import) or an
    @code(export) declaration, i.e. it must be evaluated as an ES module.

    The check is line based: a line whose first token is @code(import) (not
    followed by @code(() or @code(.), which are dynamic import and
    @code(import.meta) and are legal in scripts) or @code(export) marks the
    file as a module. Lines inside block comments and @code(//) lines are
    skipped, so a manifest comment does not interfere. }
function ScriptLooksLikeModule(const Src: UTF8String): boolean;

{** Resolve an import specifier.

    @param(BaseName Absolute path of the importing module, or any name
      without a directory part for the entry point; relative specifiers are
      resolved against its directory, falling back to @code(Root).)
    @param(Name The specifier as written in the @code(import) statement.)
    @param(Root The extension folder. Every resolved file must stay inside it,
      and no path component below it may be a symbolic link or junction.)
    @param(Resolved Receives the absolute path (or @link(TrndiModuleSpecifier)).)
    @returns(An empty string on success, otherwise the error message.) }
function ResolveModuleSpecifier(const BaseName, Name, Root: string;
  out Resolved: string): string;

{** Quote @code(S) as a JavaScript string literal (double quoted, escaped). }
function JSStringLiteral(const S: string): UTF8String;

{** The classic script that loads a module extension. It evaluates the
    module through @code(import()), copies every exported function that does
    not shadow an existing global onto @code(globalThis) (so
    @code(export function clockView) works like a top-level declaration in
    a classic script), and stores a load error in
    @link(ModuleBootstrapGlobal) while the loader is still synchronous. Once
    Pascal has read that slot and removed the object, later errors (after a
    top-level @code(await) that outlived the load) are rethrown so the
    engine's unhandled-rejection tracker reports them. }
function ModuleBootstrapScript(const EntryPath: string): UTF8String;

{** Source of the synthetic @code("trndi") module: a default export of the
    context's @code(Trndi) object plus named exports of its namespaces. }
function TrndiModuleSource: UTF8String;

implementation

{******************************************************************************
  Module detection
******************************************************************************}

{ Does the line (already trimmed of leading whitespace) start with an
  import/export keyword in statement position? }
function LineStartsModuleKeyword(const Line: string): boolean;
var
  next: char;
begin
  Result := false;
  if Copy(Line, 1, 6) = 'import' then
  begin
    if Length(Line) <= 6 then
      Exit(false);              // a bare identifier, not a statement
    next := Line[7];
    // import x from / import { / import "side-effect" / import * as
    Result := next in [' ', #9, '{', '"', '''', '*'];
  end
  else if Copy(Line, 1, 6) = 'export' then
  begin
    if Length(Line) <= 6 then
      Exit(false);
    next := Line[7];
    // export const / export function / export { / export * from
    Result := next in [' ', #9, '{', '*'];
  end;
end;

function ScriptLooksLikeModule(const Src: UTF8String): boolean;
var
  p, len, lineStart: integer;
  inBlockComment: boolean;
  line: string;
  q: integer;
begin
  Result := false;
  len := Length(Src);
  inBlockComment := false;
  p := 1;
  while p <= len do
  begin
    // Extract one line (either line ending flavour).
    lineStart := p;
    while (p <= len) and not (Src[p] in [#10, #13]) do
      Inc(p);
    line := Copy(Src, lineStart, p - lineStart);
    if (p <= len) and (Src[p] = #13) and (p < len) and (Src[p + 1] = #10) then
      Inc(p);
    Inc(p);

    // Walk block comments spanning this line. Whatever precedes a comment
    // opener or follows a closer on the same line is examined for the
    // keyword, so "/* @name x */ import ..." on one line still counts.
    line := TrimLeft(line);
    while line <> '' do
    begin
      if inBlockComment then
      begin
        q := Pos('*/', line);
        if q = 0 then
          Break;                  // comment continues on the next line
        inBlockComment := false;
        line := TrimLeft(Copy(line, q + 2, MaxInt));
        Continue;
      end;
      if Copy(line, 1, 2) = '//' then
        Break;                    // rest of the line is a comment
      if Copy(line, 1, 2) = '/*' then
      begin
        inBlockComment := true;
        line := Copy(line, 3, MaxInt);
        Continue;
      end;
      if LineStartsModuleKeyword(line) then
        Exit(true);
      Break;                      // an ordinary statement: next line
    end;
  end;
end;

{******************************************************************************
  Specifier resolution
******************************************************************************}

function IsRelativeSpecifier(const Name: string): boolean;
begin
  Result := (Copy(Name, 1, 2) = './') or (Copy(Name, 1, 3) = '../');
end;

function IsAbsoluteSpecifier(const Name: string): boolean;
begin
  Result := (Name <> '') and ((Name[1] in ['/', '\']) or
    ((Length(Name) > 1) and (Name[2] = ':')));
end;

{ Compare two normalized paths the way the file system does. }
function SamePathText(const A, B: string): boolean;
begin
  if FileNameCaseSensitive then
    Result := A = B
  else
    Result := SameText(A, B);
end;

{ Is Path a symbolic link, or on Windows any reparse point (junctions too)? }
function IsLinkPath(const Path: string): boolean;
var
  link: TRawbyteSymLinkRec;
  sr: TSearchRec;
begin
  Result := FileGetSymLinkTarget(Path, link);
  if (not Result) and (FindFirst(Path, faAnyFile or faSymLink, sr) = 0) then
  begin
    Result := (sr.Attr and faSymLink) <> 0;
    FindClose(sr);
  end;
end;

{ Does any component of Candidate below RootDir (which ends in a delimiter)
  go through a link? A link would point the textual confinement check at a
  file outside the folder, so every step from the root down is checked. }
function PathHasLinkBelowRoot(const RootDir, Candidate: string): boolean;
var
  rest, current, part: string;
  sep: integer;
begin
  Result := False;
  rest := Copy(Candidate, Length(RootDir) + 1, MaxInt);
  current := ExcludeTrailingPathDelimiter(RootDir);
  while rest <> '' do
  begin
    sep := Pos(DirectorySeparator, rest);
    if sep = 0 then
      sep := Length(rest) + 1;
    part := Copy(rest, 1, sep - 1);
    Delete(rest, 1, sep);
    if part = '' then
      Continue;
    current := current + DirectorySeparator + part;
    if IsLinkPath(current) then
      Exit(True);
  end;
end;

function ResolveModuleSpecifier(const BaseName, Name, Root: string;
out Resolved: string): string;
var
  baseDir, rootDir, candidate: string;
begin
  Result := '';
  Resolved := '';

  if Name = TrndiModuleSpecifier then
  begin
    Resolved := TrndiModuleSpecifier;
    Exit;
  end;

  if Root = '' then
    Exit('only an extension may import modules');

  rootDir := IncludeTrailingPathDelimiter(ExpandFileName(Root));

  if IsRelativeSpecifier(Name) then
  begin
    baseDir := ExtractFilePath(BaseName);
    if baseDir = '' then
      baseDir := rootDir;
    candidate := SetDirSeparators(baseDir + Name);
  end
  else if IsAbsoluteSpecifier(Name) then
    candidate := SetDirSeparators(Name)
  else
    Exit(Format('cannot import "%s": bare module names are not supported, ' +
      'use a relative path such as "./%s.js"', [Name, Name]));

  candidate := ExpandFileName(candidate);

  // tsc leaves "./util" as written; be forgiving and try the .js twin.
  if (ExtractFileExt(candidate) = '') and (not FileExists(candidate)) and
    FileExists(candidate + '.js') then
    candidate := candidate + '.js';

  // Confine imports to the extension folder. Nothing outside it is part of
  // an extension, and the loader would otherwise execute any file on disk.
  if not SamePathText(Copy(candidate, 1, Length(rootDir)), rootDir) then
    Exit(Format('cannot import "%s": modules must live inside the extension ' +
      'folder %s', [Name, Root]));

  // The check above is textual; a symlink or junction inside the folder could
  // still lead out of it, so refuse module paths that pass through one.
  if PathHasLinkBelowRoot(rootDir, candidate) then
    Exit(Format('cannot import "%s": modules may not be reached through a ' +
      'symbolic link', [Name]));

  Resolved := candidate;
end;

{******************************************************************************
  JavaScript source builders
******************************************************************************}

function JSStringLiteral(const S: string): UTF8String;
var
  u: UTF8String;
  i: integer;
  c: char;
begin
  // Convert first, escape the UTF-8 bytes after. S is a system-codepage
  // string (CP1252 on a Windows runner; UTF-8 on the Unixes, where this is a
  // no-op), and appending its chars to the UTF-8 result copies the bytes
  // unconverted -- so a path like C:\Users\Björn reached the engine as
  // Latin-1 bytes under a UTF-8 label.
  u := UTF8String(S);
  Result := '"';
  for i := 1 to Length(u) do
  begin
    c := u[i];
    case c of
    '"': Result := Result + '\"';
    '\': Result := Result + '\\';
    #10: Result := Result + '\n';
    #13: Result := Result + '\r';
    #9: Result := Result + '\t';
    #0..#8, #11, #12, #14..#31:
      Result := Result + '\u' + IntToHex(Ord(c), 4);
    else
      Result := Result + c;
    end;
  end;
  Result := Result + '"';
end;

function ModuleBootstrapScript(const EntryPath: string): UTF8String;
const
  G = 'globalThis.' + ModuleBootstrapGlobal;
begin
  Result :=
    G + ' = { error: "" };' + LineEnding +
    'import(' + JSStringLiteral(EntryPath) + ').then(function (ns) {' + LineEnding +
    '  for (const key of Object.keys(ns)) {' + LineEnding +
    '    if (key === "default") continue;' + LineEnding +
    '    if (typeof ns[key] !== "function") continue;' + LineEnding +
    '    if (key in globalThis) continue;' + LineEnding +
    '    globalThis[key] = ns[key];' + LineEnding +
    '  }' + LineEnding +
    '}).catch(function (e) {' + LineEnding +
    '  const text = (e && e.stack) ? String(e) + "\n" + String(e.stack) : String(e);' + LineEnding +
    '  if (' + G + ') ' + G + '.error = text;' + LineEnding +
    '  else throw e;' + LineEnding +
    '});' + LineEnding;
end;

function TrndiModuleSource: UTF8String;
begin
  Result :=
    'const T = globalThis.Trndi;' + LineEnding +
    'export default T;' + LineEnding +
    'export const api = T.api;' + LineEnding +
    'export const permissions = T.permissions;' + LineEnding +
    'export const data = T.data;' + LineEnding +
    'export const net = T.net;' + LineEnding +
    'export const storage = T.storage;' + LineEnding +
    'export const on = (event, fn) => T.on(event, fn);' + LineEnding +
    'export const off = (event, fn) => T.off(event, fn);' + LineEnding;
end;

end.
