(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation.
 *)

unit trndi.ext.grant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, forms,
  trndi.native, trndi.ext.perm,
  slicke.ux.alert;

{** Decide which permissions are granted for an extension before it is loaded.

    Storage layout (under TrndiNative settings):
      ext.perm.<id>.hash    - SHA-256 of the file contents that was last approved
      ext.perm.<id>.granted - comma-separated promptable groups approved

    Flow:
      - If the file declares no promptable perms, return baseline silently.
      - If the stored hash matches the current hash, restore baseline + stored
        granted set (no prompt).
      - Otherwise, prompt the user with the requested groups. On accept, save
        the hash and granted set and return baseline + requested. On deny,
        return [] so the caller skips loading the extension. }
function GrantPermissionsFor(const ExtId, FileName, Hash: string;
  const Manifest: TExtManifest; native: TrndiNative): TExtPermSet;

implementation

function FormatPermList(const perms: TExtPermSet): string;
var
  g: TExtPermGroup;
begin
  Result := '';
  for g := Low(TExtPermGroup) to High(TExtPermGroup) do
    if g in perms then
      Result := Result + '  - ' + PermGroupName[g] + ': ' + PermGroupDesc[g] + sLineBreak;
end;

function GrantPermissionsFor(const ExtId, FileName, Hash: string;
  const Manifest: TExtManifest; native: TrndiNative): TExtPermSet;
var
  needed, stored: TExtPermSet;
  storedHash, storedCsv: string;
  prompt, title, headLine: string;
  res: TModalResult;
begin
  Result := PermBaseline;
  needed := Manifest.Requested * PermPromptable;

  // Nothing promptable requested: baseline only, no prompt.
  if needed = [] then
    Exit;

  // Restore previous decision when the file is unchanged.
  if native <> nil then
  begin
    storedHash := native.GetSetting('ext.perm.' + ExtId + '.hash', '');
    if (storedHash <> '') and (storedHash = Hash) then
    begin
      storedCsv := native.GetSetting('ext.perm.' + ExtId + '.granted', '');
      stored := CSVToPermSet(storedCsv);
      Result := PermBaseline + (stored * PermPromptable);
      Exit;
    end;
  end;

  // First load or file changed since last grant: ask the user.
  if Manifest.DisplayName <> '' then
    title := Manifest.DisplayName
  else
    title := ExtractFileName(FileName);
  headLine := 'Extension requests permissions';
  prompt :=
    title + sLineBreak +
    sLineBreak +
    'File: ' + ExtractFileName(FileName) + sLineBreak;
  if Manifest.Author <> '' then
    prompt := prompt + 'Author: ' + Manifest.Author + sLineBreak;
  prompt := prompt + sLineBreak +
    'Requested permissions:' + sLineBreak +
    FormatPermList(needed) +
    sLineBreak +
    'Baseline (always granted): ' + PermSetToCSV(PermBaseline) + sLineBreak +
    sLineBreak +
    'Allow this extension to load with the requested permissions?';

  res := UXDialog(uxdAuto, headLine, title, prompt, [mbYes, mbNo],
    mtConfirmation);

  if res = mrYes then
  begin
    Result := PermBaseline + needed;
    if native <> nil then
    begin
      native.SetSetting('ext.perm.' + ExtId + '.hash', Hash);
      native.SetSetting('ext.perm.' + ExtId + '.granted', PermSetToCSV(needed));
      if Manifest.Author <> '' then
        native.SetSetting('ext.perm.' + ExtId + '.author', Manifest.Author);
    end;
  end
  else
  begin
    // Denied: skip loading; do not persist (re-ask next launch).
    Result := [];
  end;
end;

end.
