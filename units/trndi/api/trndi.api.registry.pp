(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2026 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * GitHub: https://github.com/slicke/trndi
 *)

{** Single source of truth for the available CGM backends. Maps between the
    stable settings code (stored in 'remote.type'), the display name shown in
    backend pickers, and the implementing TrndiAPI metaclass — replacing the
    per-form if/case chains that previously had to be kept in sync by hand
    (settings form, first-run wizard and main-form load/save/create paths).
    Adding a backend means adding one BackendRegistry entry plus its unit in
    the uses clause below. }
unit trndi.api.registry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, trndi.api,
  trndi.api.nightscout, trndi.api.nightscout3,
  trndi.api.dexcom, trndi.api.dexcomNew,
  trndi.api.tandem, trndi.api.carelink, trndi.api.xdrip
{$ifdef DEBUG}
  , trndi.api.debug, trndi.api.debug_missing, trndi.api.debug_perfect,
  trndi.api.debug_custom, trndi.api.debug_edge, trndi.api.debug_firstmissing,
  trndi.api.debug_secondmissing, trndi.api.debug_firstXmissing,
  trndi.api.debug_intermittentmissing, trndi.api.debug_slow,
  trndi.api.debug_lowsoon, trndi.api.debug_sensorexpiry,
  trndi.api.debug_faultysensor, trndi.api.debug_latemissing
{$endif};

{$I ../../../inc/defines.inc}

type
  TBackendEntry = record
    code: string;        // Stable identifier stored in settings ('remote.type')
    name: string;        // Display name shown in backend pickers
    cls: TrndiAPIClass;  // Implementing backend class
  end;

  {** Which shared credential rule failed; callers map this to their own
      localized error message. }
  TBackendCredError = (bceNone, bceAddress, bceEmail, bcePassword, bceToken);

const
  {** Canonical backend list; picker order follows this table. }
  BackendRegistry: array of TBackendEntry = (
  (code: 'API_NS'; name: API_NS; cls: NightScout),
  (code: 'API_NS3'; name: API_NS3; cls: NightScout3),
  (code: 'API_DEX_USA'; name: API_DEX_USA; cls: DexcomUSA),
  (code: 'API_DEX_EU'; name: API_DEX_EU; cls: DexcomWorld),
  (code: 'API_DEX_NEW_USA'; name: API_DEX_NEW_USA; cls: DexcomUSANew),
  (code: 'API_DEX_NEW_EU'; name: API_DEX_NEW_EU; cls: DexcomWorldNew),
  (code: 'API_DEX_NEW_JP'; name: API_DEX_NEW_JP; cls: DexcomJapanNew),
  (code: 'API_TANDEM_USA'; name: API_TANDEM_USA; cls: TandemUSA),
  (code: 'API_TANDEM_EU'; name: API_TANDEM_EU; cls: TandemEU),
  (code: 'API_CARELINK_US'; name: API_CARELINK_US; cls: CareLinkUS),
  (code: 'API_CARELINK_EU'; name: API_CARELINK_EU; cls: CareLinkEU),
  (code: 'API_XDRIP'; name: API_XDRIP; cls: xDrip)
{$ifdef DEBUG}
  ,
  (code: 'API_D_DEBUG'; name: API_D_DEBUG; cls: DebugAPI),
  (code: 'API_D_MISSING'; name: API_D_MISSING; cls: DebugMissingAPI),
  (code: 'API_D_PERFECT'; name: API_D_PERFECT; cls: DebugPerfectAPI),
  (code: 'API_D_CUSTOM'; name: API_D_CUSTOM; cls: DebugCustomAPI),
  (code: 'API_D_EDGE'; name: API_D_EDGE; cls: DebugEdgeAPI),
  (code: 'API_D_FIRST'; name: API_D_FIRST; cls: DebugFirstMissingAPI),
  (code: 'API_D_SECOND'; name: API_D_SECOND; cls: DebugSecondMissingAPI),
  (code: 'API_D_FIRSTX'; name: API_D_FIRSTX; cls: DebugFirstXMissingAPI),
  (code: 'API_D_INTERMITTENT'; name: API_D_INTERMITTENT; cls: DebugIntermittentMissingAPI),
  (code: 'API_D_SLOW'; name: API_D_SLOW; cls: DebugSlowAPI),
  (code: 'API_D_LOWSOON'; name: API_D_LOWSOON; cls: DebugLowSoonAPI),
  (code: 'API_D_SENSOR_EXPIRY'; name: API_D_SENSOR_EXPIRY; cls: DebugSensorExpiryAPI),
  (code: 'API_D_FAULTY'; name: API_D_FAULTY; cls: DebugFaultySensorAPI),
  (code: 'API_D_LATE_MISSING'; name: API_D_LATE_MISSING; cls: DebugLateMissingAPI)
{$endif}
  );

{** All backend display names in registry order; used to fill backend pickers.
    Debug entries (debug builds only) can be excluded, e.g. for the first-run
    wizard. }
procedure ListBackendNames(items: TStrings; includeDebug: boolean = true);

{** Metaclass for a display name or settings code; nil if unknown. }
function BackendClassOf(const nameOrCode: string): TrndiAPIClass;

{** Display name → stable settings code. Falls back to 'API_NS' for unknown
    names, matching the historical save behavior. }
function BackendCode(const displayName: string): string;

{** Settings code → display name. Falls back to the NightScout name for
    unknown codes, matching the historical load behavior. }
function BackendDisplayName(const code: string): string;

{** Instantiate the backend stored in settings. Accepts a settings code or a
    display name (legacy settings stored the latter); nil if unknown. }
function CreateBackend(const nameOrCode, target, creds: string): TrndiAPI;

{** Shared per-backend credential rules used by the settings form and the
    first-run wizard; bceNone when the values pass. }
function CheckBackendCredentials(const nameOrCode, addr, pass: string): TBackendCredError;

implementation

function FindEntry(const nameOrCode: string; out entry: TBackendEntry): boolean;
var
  e: TBackendEntry;
begin
  for e in BackendRegistry do
    if (e.code = nameOrCode) or (e.name = nameOrCode) then
    begin
      entry := e;
      Exit(true);
    end;
  Result := false;
end;

procedure ListBackendNames(items: TStrings; includeDebug: boolean);
var
  e: TBackendEntry;
begin
  for e in BackendRegistry do
    if includeDebug or (Copy(e.code, 1, 6) <> 'API_D_') then
      items.Add(e.name);
end;

function BackendClassOf(const nameOrCode: string): TrndiAPIClass;
var
  entry: TBackendEntry;
begin
  if FindEntry(nameOrCode, entry) then
    Result := entry.cls
  else
    Result := nil;
end;

function BackendCode(const displayName: string): string;
var
  entry: TBackendEntry;
begin
  if FindEntry(displayName, entry) then
    Result := entry.code
  else
    Result := 'API_NS';
end;

function BackendDisplayName(const code: string): string;
var
  entry: TBackendEntry;
begin
  if FindEntry(code, entry) then
    Result := entry.name
  else
    Result := API_NS;
end;

function CreateBackend(const nameOrCode, target, creds: string): TrndiAPI;
var
  entry: TBackendEntry;
begin
  if FindEntry(nameOrCode, entry) then
    Result := entry.cls.Create(target, creds)
  else
    Result := nil;
end;

function CheckBackendCredentials(const nameOrCode, addr, pass: string): TBackendCredError;
var
  entry: TBackendEntry;
begin
  Result := bceNone;
  if not FindEntry(nameOrCode, entry) then
    Exit;

  case entry.code of
  'API_NS', 'API_NS3':
    if Copy(addr, 1, 4) <> 'http' then
      Result := bceAddress;
  'API_TANDEM_USA', 'API_TANDEM_EU':
    if Pos('@', addr) = 0 then
      Result := bceEmail
    else if Length(pass) < 5 then
      Result := bcePassword;
  'API_DEX_USA', 'API_DEX_EU', 'API_DEX_NEW_USA', 'API_DEX_NEW_EU', 'API_DEX_NEW_JP':
    if Length(pass) < 5 then
      Result := bcePassword;
  'API_CARELINK_US', 'API_CARELINK_EU':
    // The credential is the captured token blob, which is always JSON
    if Copy(TrimLeft(pass), 1, 1) <> '{' then
      Result := bceToken;
  end;
end;

end.
