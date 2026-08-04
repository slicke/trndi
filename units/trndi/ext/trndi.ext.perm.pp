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

  {** Parsed manifest from an extension's leading /* ... */ comment block.
      @name and @copyright are preferred; the historical first-line and (c)
      forms remain supported for existing extensions. }
  TExtManifest = record
    DisplayName: string;
    Author: string;
    Version: string;
    Description: string;
    Homepage: string;
    License: string;
    Requested: TExtPermSet;
    IsValid: boolean;
    ErrorMessage: string;
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
     'Make network requests (fetch/asyncGet/jsonGet)',
     'Run external programs (runCMD)',
     'Read/write Trndi settings and CGM thresholds');

{** Parse the leading /* ... */ block from an extension source. Invalid
    directives make IsValid False and explain the reason in ErrorMessage. }
function ParseExtManifest(const Script: string): TExtManifest;

{** SHA-256 hex digest of the script source. Used to detect edits and force re-prompt. }
function HashScript(const Script: string): string;

{** Stable extension id derived from the file path. Lower-case basename, no extension. }
function ExtIdFromPath(const FileName: string): string;

{** Convert a permission set to comma-separated names, baseline excluded. }
function PermSetToCSV(const s: TExtPermSet): string;

{** Parse "net, exec, settings" into a set. Unknown names are ignored.
    ParseExtManifest validates permission names for extension manifests. }
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

{ Strip leading "*" decoration from a manifest body line. }
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
  s, body: string;
  startIdx, endIdx: integer;
  lines, parts: TStringArray;
  i, j: integer;
  trimmed, lc, value: string;
  haveName: boolean;
  g: TExtPermGroup;

  function DirectiveValue(const Directive: string; out AValue: string): boolean;
  var
    rest: string;
  begin
    Result := False;
    if Copy(lc, 1, Length(Directive)) <> Directive then Exit;
    rest := Copy(trimmed, Length(Directive) + 1, MaxInt);
    if (rest <> '') and not (rest[1] in [' ', #9]) then Exit;
    AValue := Trim(rest);
    Result := True;
  end;

  procedure AddError(const Msg: string);
  begin
    Result.IsValid := False;
    if Result.ErrorMessage <> '' then
      Result.ErrorMessage := Result.ErrorMessage + sLineBreak;
    Result.ErrorMessage := Result.ErrorMessage + Msg;
  end;

begin
  Result.DisplayName := '';
  Result.Author := '';
  Result.Version := '';
  Result.Description := '';
  Result.Homepage := '';
  Result.License := '';
  Result.Requested := [];
  Result.IsValid := True;
  Result.ErrorMessage := '';

  s := Script;
  if s = '' then Exit;
  if Copy(s, 1, 3) = #$EF#$BB#$BF then
    Delete(s, 1, 3); // Permit UTF-8 files saved with a BOM.

  startIdx := Pos('/*', s);
  if startIdx <> 1 then Exit;
  endIdx := PosEx('*/', s, startIdx + 2);
  if endIdx = 0 then Exit;

  body := Copy(s, startIdx + 2, endIdx - startIdx - 2);
  lines := body.Split([#13, #10], TStringSplitOptions.None);
  haveName := False;
  for i := 0 to High(lines) do
  begin
    trimmed := StripLineDecoration(lines[i]);
    if trimmed = '' then Continue;
    lc := LowerCase(trimmed);

    if DirectiveValue('@name', value) then
    begin
      Result.DisplayName := value;
      haveName := value <> '';
    end
    else if DirectiveValue('@copyright', value) then
      Result.Author := value
    else if DirectiveValue('@version', value) then
      Result.Version := value
    else if DirectiveValue('@description', value) then
      Result.Description := value
    else if DirectiveValue('@homepage', value) then
      Result.Homepage := value
    else if DirectiveValue('@license', value) then
      Result.License := value
    else if DirectiveValue('@perms', value) then
    begin
      parts := value.Split([',', ' ', #9], TStringSplitOptions.ExcludeEmpty);
      for j := 0 to High(parts) do
        if ParsePermName(parts[j], g) then
          Include(Result.Requested, g)
        else
          AddError('Unknown permission: ' + parts[j]);
    end
    else if (Pos('(c)', lc) = 1) or (Pos('copyright', lc) = 1) then
    begin
      if Result.Author = '' then Result.Author := trimmed;
    end
    else if trimmed[1] = '@' then
      AddError('Unknown manifest directive: ' + trimmed)
    else if not haveName then
    begin
      Result.DisplayName := trimmed;
      haveName := True;
    end;
  end;
end;

end.
