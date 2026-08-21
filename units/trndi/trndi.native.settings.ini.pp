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
 * - 2026-08-21: New unit. The INI settings backend that trndi.native.linux
 *   and trndi.native.generic each carried as byte-identical copies now lives
 *   here once; both classes delegate to these functions. One deliberate
 *   consolidation: the two class-wide stores became one process-wide store —
 *   unobservable in practice, since any real binary only ever instantiates
 *   one platform class.
 *)

{**
  @abstract(Process-wide INI settings store shared by the INI-backed
  platform classes.)

  The store is process-wide rather than per-instance because TIniFile
  rewrites the whole file from its in-memory snapshot on UpdateFile: with
  per-instance stores, the settings dialog's writes were erased by the main
  window's next write (and vice versa). Everything here runs under one lock;
  the fetch thread reads proxy settings while the UI thread writes.

  Callers pass a path resolver instead of a path so resolution stays lazy —
  it runs only when the store is first created (or re-created after
  @link(IniSettingsReload)). Keys are stored under the canonical
  @code([trndi]) section; the Windows registry and macOS defaults are flat,
  so sections carry no meaning across platforms.
}
unit trndi.native.settings.ini;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, IniFiles;

type
  {** Resolves the INI file's path; invoked at most once per store lifetime,
      when the store is first created. }
TIniPathResolver = function: string of object;

{** Read @param(fullKey) (already scoped via buildKey) from the [trndi]
    section; @param(def) when absent. }
function IniSettingsGet(const resolver: TIniPathResolver;
  const fullKey, def: string): string;

{** Write @param(fullKey) under the canonical [trndi] section and flush. }
procedure IniSettingsSet(const resolver: TIniPathResolver;
  const fullKey, val: string);

{** Delete @param(fullKey) from the [trndi] section and flush. }
procedure IniSettingsDelete(const resolver: TIniPathResolver;
  const fullKey: string);

{** Drop the store; it is lazily re-created (re-reading the file) on the
    next access. }
procedure IniSettingsReload;

{** Serialize every section and key to INI text. }
function IniSettingsExport(const resolver: TIniPathResolver): string;

{** Merge INI text into the store, preserving the source sections, with a
    single flush at the end. }
procedure IniSettingsImport(const resolver: TIniPathResolver;
  const iniData: string);

implementation

var
  gIniStore: TIniFile;
  gIniLock: TRTLCriticalSection;

// Create the store on first use; the resolver supplies the path and the
// directory is created if missing.
procedure EnsureIni(const resolver: TIniPathResolver);
var
  path: string;
begin
  if not Assigned(gIniStore) then
  begin
    path := resolver();
    if ExtractFilePath(path) <> '' then
      ForceDirectories(ExtractFilePath(path));
    gIniStore := TIniFile.Create(path);
  end;
end;

function IniSettingsGet(const resolver: TIniPathResolver;
  const fullKey, def: string): string;
begin
  EnterCriticalSection(gIniLock);
  try
    EnsureIni(resolver);
    Result := gIniStore.ReadString('trndi', fullKey, def);
  finally
    LeaveCriticalSection(gIniLock);
  end;
end;

procedure IniSettingsSet(const resolver: TIniPathResolver;
  const fullKey, val: string);
begin
  EnterCriticalSection(gIniLock);
  try
    EnsureIni(resolver);
    // Write under a canonical section
    gIniStore.WriteString('trndi', fullKey, val);
    gIniStore.UpdateFile;
  finally
    LeaveCriticalSection(gIniLock);
  end;
end;

procedure IniSettingsDelete(const resolver: TIniPathResolver;
  const fullKey: string);
begin
  EnterCriticalSection(gIniLock);
  try
    EnsureIni(resolver);
    gIniStore.DeleteKey('trndi', fullKey);
    gIniStore.UpdateFile;
  finally
    LeaveCriticalSection(gIniLock);
  end;
end;

procedure IniSettingsReload;
begin
  EnterCriticalSection(gIniLock);
  try
    FreeAndNil(gIniStore);
    // will be recreated on next access
  finally
    LeaveCriticalSection(gIniLock);
  end;
end;

function IniSettingsExport(const resolver: TIniPathResolver): string;
var
  sl: TStringList;
  sections, keys: TStringList;
  i, j: integer;
  section, key, value: string;
begin
  // Allocate inside the try block: should a Create raise, the finally must
  // still release the lock (Free on the nil ones is safe).
  sl := nil;
  sections := nil;
  keys := nil;
  EnterCriticalSection(gIniLock);
  try
    sl := TStringList.Create;
    sections := TStringList.Create;
    keys := TStringList.Create;
    EnsureIni(resolver);
    gIniStore.ReadSections(sections);
    for i := 0 to sections.Count - 1 do
    begin
      section := sections[i];
      sl.Add('[' + section + ']');
      gIniStore.ReadSection(section, keys);
      for j := 0 to keys.Count - 1 do
      begin
        key := keys[j];
        value := gIniStore.ReadString(section, key, '');
        sl.Add(key + '=' + value);
      end;
      if i < sections.Count - 1 then
        sl.Add(''); // Add blank line between sections
    end;
    Result := sl.Text;
  finally
    keys.Free;
    sections.Free;
    sl.Free;
    LeaveCriticalSection(gIniLock);
  end;
end;

procedure IniSettingsImport(const resolver: TIniPathResolver;
  const iniData: string);
var
  sl: TStringList;
  mem: TMemoryStream;
  ini: TMemIniFile;
  sections, keys: TStringList;
  i, j: integer;
  section, key, value: string;
begin
  if iniData = '' then
    Exit;
  // Allocate inside the try block: should a Create raise, the finally must
  // still release the lock (Free on the nil ones is safe).
  sl := nil;
  mem := nil;
  ini := nil;
  sections := nil;
  keys := nil;
  EnterCriticalSection(gIniLock);
  try
    sl := TStringList.Create;
    mem := TMemoryStream.Create;
    sections := TStringList.Create;
    keys := TStringList.Create;
    EnsureIni(resolver);
    mem.WriteBuffer(iniData[1], Length(iniData));
    mem.Position := 0;
    sl.LoadFromStream(mem);

    // Create a temporary INI file in memory
    ini := TMemIniFile.Create('');
    ini.SetStrings(sl);

    ini.ReadSections(sections);
    for i := 0 to sections.Count - 1 do
    begin
      section := sections[i];
      ini.ReadSection(section, keys);
      for j := 0 to keys.Count - 1 do
      begin
        key := keys[j];
        value := ini.ReadString(section, key, '');
        gIniStore.WriteString(section, key, value);
      end;
    end;
    gIniStore.UpdateFile;
  finally
    keys.Free;
    sections.Free;
    ini.Free;
    mem.Free;
    sl.Free;
    LeaveCriticalSection(gIniLock);
  end;
end;

initialization
  InitCriticalSection(gIniLock);

finalization
  // The store is process-wide (see the unit abstract); freed here rather
  // than in any instance destructor.
  FreeAndNil(gIniStore);
  DoneCriticalSection(gIniLock);

end.
