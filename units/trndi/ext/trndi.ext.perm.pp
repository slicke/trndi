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

unit trndi.ext.perm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  slicke.sha256;

type
  {** Coarse permission groups. data/ui/timers are baseline (granted without prompt);
      net/exec/settings must be declared in the extension manifest and approved. }
  TExtPermGroup = (epData, epUI, epTimers, epNet, epExec, epSettings);
  TExtPermSet = set of TExtPermGroup;

  {** Parsed manifest from an extension's leading /* ... */ comment block. }
  TExtManifest = record
    DisplayName: string;       // first non-empty line of the header
    Author: string;            // line beginning with "(c)"
    Requested: TExtPermSet;    // groups listed on a "@perms ..." line
  end;

const
  {** Always granted, no prompt. }
  PermBaseline: TExtPermSet = [epData, epUI, epTimers];
  {** Must appear in @perms and be approved by the user. }
  PermPromptable: TExtPermSet = [epNet, epExec, epSettings];
  {** Human-readable names. Order matches TExtPermGroup. }
  PermGroupName: array[TExtPermGroup] of string =
    ('data', 'ui', 'timers', 'net', 'exec', 'settings');
  PermGroupDesc: array[TExtPermGroup] of string =
    ('Read glucose data and app info',
     'Show dialogs, play sounds, change visual settings',
     'Schedule timers',
     'Make network requests (asyncGet/jsonGet)',
     'Run external programs (runCMD)',
     'Read/write Trndi settings and CGM thresholds');

{** Parse the leading /* ... */ block from an extension source. }
function ParseExtManifest(const Script: string): TExtManifest;

{** SHA-256 hex digest of the script source. Used to detect edits and force re-prompt. }
function HashScript(const Script: string): string;

{** Stable extension id derived from the file path. Lower-case basename, no extension. }
function ExtIdFromPath(const FileName: string): string;

{** Convert a permission set to comma-separated names, baseline excluded. }
function PermSetToCSV(const s: TExtPermSet): string;

{** Parse "net, exec, settings" into a set. Unknown names are ignored. }
function CSVToPermSet(const s: string): TExtPermSet;

{** Map a name like "net" to epNet. Returns False if unknown. }
function ParsePermName(const s: string; out g: TExtPermGroup): boolean;

implementation

function ParsePermName(const s: string; out g: TExtPermGroup): boolean;
var
  gi: TExtPermGroup;
  lc: string;
begin
  Result := False;
  lc := LowerCase(Trim(s));
  for gi := Low(TExtPermGroup) to High(TExtPermGroup) do
    if PermGroupName[gi] = lc then
    begin
      g := gi;
      Exit(True);
    end;
end;

function PermSetToCSV(const s: TExtPermSet): string;
var
  g: TExtPermGroup;
begin
  Result := '';
  for g := Low(TExtPermGroup) to High(TExtPermGroup) do
    if g in s then
    begin
      if Result <> '' then Result := Result + ',';
      Result := Result + PermGroupName[g];
    end;
end;

function CSVToPermSet(const s: string): TExtPermSet;
var
  parts: TStringArray;
  i: integer;
  g: TExtPermGroup;
begin
  Result := [];
  parts := s.Split([',', ' ', #9], TStringSplitOptions.ExcludeEmpty);
  for i := 0 to High(parts) do
    if ParsePermName(parts[i], g) then
      Include(Result, g);
end;

function HashScript(const Script: string): string;
var
  digest: TSHA256Digest;
  i: integer;
const
  HexChars: array[0..15] of char = '0123456789abcdef';
begin
  digest := SHA256String(Script);
  SetLength(Result, SizeOf(digest) * 2);
  for i := 0 to SizeOf(digest) - 1 do
  begin
    Result[1 + i * 2]     := HexChars[digest[i] shr 4];
    Result[2 + i * 2 + 0] := HexChars[digest[i] and $0F];
  end;
end;

function ExtIdFromPath(const FileName: string): string;
var
  base: string;
begin
  base := ExtractFileName(FileName);
  if LowerCase(ExtractFileExt(base)) = '.js' then
    base := Copy(base, 1, Length(base) - 3);
  Result := LowerCase(base);
end;

{ Strip leading "//" or "*" decoration from a manifest body line. }
function StripLineDecoration(const line: string): string;
var
  s: string;
begin
  s := Trim(line);
  if (Length(s) >= 1) and (s[1] = '*') then
    s := Trim(Copy(s, 2, MaxInt));
  Result := s;
end;

function ParseExtManifest(const Script: string): TExtManifest;
var
  s, body, line: string;
  startIdx, endIdx: integer;
  lines: TStringArray;
  i: integer;
  trimmed, lc: string;
  haveName: boolean;
begin
  Result.DisplayName := '';
  Result.Author := '';
  Result.Requested := [];

  s := Script;
  if s = '' then Exit;

  startIdx := Pos('/*', s);
  if startIdx <> 1 then
    Exit;  // header block must start the file (mirrors existing convention)

  endIdx := PosEx('*/', s, startIdx + 2);
  if endIdx = 0 then
    Exit;

  body := Copy(s, startIdx + 2, endIdx - startIdx - 2);
  lines := body.Split([#13, #10], TStringSplitOptions.None);

  haveName := False;
  for i := 0 to High(lines) do
  begin
    trimmed := StripLineDecoration(lines[i]);
    if trimmed = '' then Continue;

    lc := LowerCase(trimmed);

    if (not haveName) and (Pos('@perms', lc) <> 1) and (Pos('(c)', lc) <> 1)
       and (Pos('copyright', lc) <> 1) then
    begin
      Result.DisplayName := trimmed;
      haveName := True;
      Continue;
    end;

    if (Pos('(c)', lc) = 1) or (Pos('copyright', lc) = 1) then
    begin
      if Result.Author = '' then
        Result.Author := trimmed;
      Continue;
    end;

    if Pos('@perms', lc) = 1 then
    begin
      Result.Requested := CSVToPermSet(Copy(trimmed, Length('@perms') + 1, MaxInt));
      Continue;
    end;
  end;
end;

end.
