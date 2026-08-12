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
 * - 2026-02-03: Nightscout v3 driver now prefers APIv3 endpoints without the
 *   legacy ".json" suffix (with automatic fallback), and normalizes the v2
 *   access token so both "abc123" and "token=abc123" work.
 * - 2026-03-02: Added runtime auth-refresh handling for Nightscout v3 so
 *   expired bearer tokens are renewed automatically during polling.
 * - 2026-06-06: Switched Nightscout v3 noCache cache-busters to a monotonic
 *   per-request token so repeated forced requests do not collide within one
 *   second.
 * - 2026-07-12: getReadings now keeps a readings-window cache: a cheap
 *   /api/v3/lastModified probe skips the entries fetch entirely when the
 *   collection is unchanged, and a date$gt filter fetches only entries newer
 *   than the cached window when it did change. A periodic full refetch
 *   (cache TTL) still picks up backfilled or edited entries.
 * - 2026-08-11: the metadata probe now also fills the sensor/pump status,
 *   insulin-delivery and carbohydrate caches from the devicestatus and
 *   treatments collections, so the history graph's treatment overlays and the
 *   reservoir / sensor-expiry / pump-battery notifications work on Nightscout.
 * - 2026-08-12: added supportsRapidPolling, reporting when the /lastModified
 *   probe makes an unchanged poll a single small GET, so the UI can retry on
 *   a tight cadence while a reading is overdue.
 *   Timestamps parsed out of those collections are now converted to local
 *   time; they were previously left in UTC and compared against a local Now,
 *   which put the sensor-age suffix out by the machine's UTC offset.
 * - 2026-08-11: the sensor-age suffix probe now walks every fetched
 *   devicestatus record instead of only the newest one, matching the
 *   field extraction — on a multi-uploader site the newest record is
 *   routinely a phone record carrying no sensor detail at all.
 * - 2026-08-11: string fields in the devicestatus/treatments walks
 *   (eventType, pump.status.status, xdripjs.stateString) are now read
 *   null-tolerantly; a single record carrying a JSON null there aborted
 *   the whole metadata probe for a full cache TTL.
 *)
unit trndi.api.nightscout3;

{$mode ObjFPC}{$H+}

interface

uses
Classes, SysUtils, trndi.types, trndi.api, trndi.native, trndi.funcs, {$ifdef debug} trndi.log,{$endif}
fpjson, jsonparser, jsonscanner, dateutils, StrUtils;

const
  {** Base path for Nightscout v3 API endpoints (appended to the provided base URL). }
NS3_URL_BASE = '/api/v3/';

  {** Default paths/endpoints. }
// APIv3 tutorial uses endpoints without the legacy ".json" suffix.
// Keep ".json" variants for compatibility with older deployments.
NS3_STATUS = 'status';
NS3_STATUS_JSON = 'status.json';
NS3_ENTRIES = 'entries';
NS3_ENTRIES_JSON = 'entries.json';
NS3_SETTINGS = 'settings';
NS3_SETTINGS_JSON = 'settings.json';
NS3_PROFILE = 'profile';
NS3_PROFILE_JSON = 'profile.json';
NS3_VERSION = 'version';
NS3_VERSION_JSON = 'version.json';
NS3_DEVICESTATUS = 'devicestatus';
NS3_DEVICESTATUS_JSON = 'devicestatus.json';
NS3_TREATMENTS = 'treatments';
NS3_TREATMENTS_JSON = 'treatments.json';
NS3_LASTMODIFIED = 'lastModified';

type
  {** NightScout v3 API client with bearer authorization.
      Obtains a JWT using the v2 authorization request flow, then performs
      v3 API calls using the Authorization: Bearer header. Compatible with
      TrndiAPI. }
NightScout3 = class(TrndiAPI)
private
  FSiteBase: string;    // Base site URL (no trailing slash), e.g. https://example.com
  FToken: string;       // Bearer JWT token (obtained via v2 authorization)
  FTokenSuffix: string; // Token suffix used in v2 authorization
    // Cached sensor-age suffix from the devicestatus/treatments probe. Sensor
    // age moves at hour granularity, so re-probing on every readings fetch
    // adds up to three extra HTTP round trips; getReadings refreshes it when
    // the TTL lapses or on a forced (noCache) fetch. Empty results are cached
    // too so servers without sensor metadata aren't re-probed every fetch.
  FSensorSuffix: string;
  FSensorSuffixAt: TDateTime; // Last probe time; 0 = never probed

    // Sensor/pump housekeeping and treatments, filled by the same probe that
    // refreshes FSensorSuffix. Each has a "valid" flag because the accessors
    // must be able to say "the server reported nothing" — an empty bolus list
    // read as "no insulin was given" would be a dangerous thing to show.
  FDeviceStatus: TCGMDeviceStatus; // Newest value of each field across the probe
  FDeviceStatusValid: boolean;     // True once a payload filled at least one field
  FBoluses: TBolusList;            // Insulin deliveries, oldest first
  FBolusesValid: boolean;          // True once a treatments payload was walked
  FCarbs: TCarbList;               // Carbohydrate entries, oldest first
  FCarbsValid: boolean;            // True once a treatments payload was walked

    // Readings-window cache backing two v3-only optimizations in getReadings:
    // a /lastModified probe that skips the entries fetch entirely when the
    // collection is unchanged, and a date$gt incremental fetch that only
    // pulls entries newer than the cached window. Backfilled or edited
    // entries are picked up by the periodic full refetch (cache TTL). The v1
    // fallback path invalidates the cache instead of feeding it.
  FCache: BGResults;          // Parsed readings, newest first
  FCacheRaw: string;          // Raw response that last wrote the cache (for `res`)
  FCacheMaxNum: integer;      // Window size the cache was filled for
  FCacheFullAt: TDateTime;    // Last FULL (non-incremental) fetch; 0 = cache
                              // empty/invalidated. Anchors the TTL so steady
                              // incremental merges can't postpone the full
                              // refetch that picks up backfill/edits.
  FCacheNewestMs: int64;      // Raw `date` (ms epoch) of the newest cached entry
  FCacheLastModified: int64;  // entries lastModified at last cache write; 0 = unknown
  FLastModifiedUnsupported: boolean; // Server answered /lastModified with an unusable shape

  class function NormalizeV2AccessToken(const AccessToken: string): string;
  function IsAuthFailureResponse(const Resp: string): boolean;
  function TryRequestV3(const PathPreferred, PathLegacyJson: string;
    const Params: array of string; out Resp: string): boolean;
  function TryGetEntriesLastModified(out msVal: int64): boolean;

  function BuildAuthURL: string;
  function GetAuthToken(out Err: string): boolean;
  function BearerHeader: string;
    // Fetch thresholds using legacy Nightscout status (v1) similar to the v2 controller
  function FetchLegacyThresholds: boolean;
public
  constructor Create(user, pass: string); override; overload;
  function connect: boolean; override;
  function getReadings(minNum, maxNum: integer; extras: string;
    out res: string; noCache: boolean): BGResults; override;
  function supportsBasal: boolean; override;
  function getBasalProfile(out profile: TBasalProfile): boolean; override;

    {** Sensor and pump housekeeping from the newest devicestatus records the
        last metadata probe fetched. What a site reports depends entirely on
        what uploads to it: an AAPS or Loop rig publishes reservoir, pump
        battery and suspend state, an xDrip+ uploader publishes sensor session
        detail, and a site fed only by a CGM bridge publishes none of it. Every
        field is therefore checked against its "unknown" sentinel before use.
     }
  function getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean; override;

    {** Insulin deliveries from the treatments the last metadata probe fetched.
        Deliveries an uploader marked as loop-initiated (@code(isSMB),
        @code(automatic), or an @code(SMB) event type) are flagged automatic,
        so a looping site's constant micro-boluses can be kept off the graph
        separately from the ones the user gave.
     }
  function getBoluses(out ABoluses: TBolusList): boolean; override;

    {** True: Nightscout's treatments collection carries insulin deliveries. }
  function supportsBoluses: boolean; override;

    {** Carbohydrate entries from the treatments the last metadata probe
        fetched. }
  function getCarbs(out ACarbs: TCarbList): boolean; override;

    {** True: Nightscout's treatments collection carries carbohydrate entries. }
  function supportsCarbs: boolean; override;

    {** True while an unchanged poll costs a single /lastModified GET: the
        readings-window cache is warm and the server's entries stamp is known.
        The UI uses this to retry on a tight cadence when a reading is late. }
  function supportsRapidPolling: boolean; override;

    {** Test NightScout credentials
    }
  class function testConnection(user, pass: string; var res: string): maybebool; override;
    {** UI parameter label provider (override).
        1: NightScout URL
        2: Auth token suffix (v2)
        3: (unused)
     }
  class function ParamLabel(LabelName: APIParamLabel): string; override;
published
  property siteBase: string read FSiteBase;  // e.g. https://example.com
  property token: string read FToken;        // JWT when connected
    // For parity with v2 unit; exposes the effective API base URL in use
  property remote: string read baseUrl;
protected
    {** Fill the sensor/pump status cache from a devicestatus payload.

        A site can have several uploaders writing to the collection, and the
        newest record is not necessarily the one carrying the field we want —
        a phone uploading battery every five minutes will outrank the rig that
        reports the reservoir. Each field is therefore taken from the newest
        record that actually carries it, tracked with its own timestamp, rather
        than from the newest record overall.

        Protected rather than private so the tests can drive it from a fixture
        payload instead of standing up a fake server for every field shape.
        @param(AResponse Raw devicestatus response body) }
  procedure ExtractDeviceStatus(const AResponse: string);

    {** Fill the insulin-delivery and carbohydrate caches from a treatments
        payload. Replaces whatever the previous probe left, so it must only run
        on a payload that actually parsed.

        A Nightscout treatment is one record that may carry insulin, carbs or
        both, so — unlike CareLink, where a meal can appear twice — there is
        nothing to reconcile: each record contributes at most one entry to each
        list. Protected for the same reason as @link(ExtractDeviceStatus).
        @param(AResponse Raw treatments response body) }
  procedure ExtractTreatments(const AResponse: string);

    {** Get the value which represents the maximum reading for the backend
     }
  function getLimitHigh: integer; override;
  
    {** Get the value which represents the minimum reading for the backend
     }
  function getLimitLow: integer; override;

    {** gets the name of the API
    }
  function getSystemName: string; override;

{** Get the maximum age (in minutes) of readings provided by the backend
        @returns(Maximum age in minutes)
     }
  function getMaxAge: integer; override;

    {** Retrieve the current basal rate from the Nightscout server.
        Fetches basal rate data from the server's profile/basal endpoints.
        @returns(Current basal rate in U/hr, or 0 if unavailable)
     }
  function getBasalRate: single; override;
end;

implementation

const
  // How long a probed sensor-age suffix stays valid before getReadings spends
  // extra devicestatus/treatments round trips on refreshing it.
  NS3_SENSOR_SUFFIX_TTL_MIN = 10;

  // How long the readings-window cache may serve incremental/short-circuited
  // fetches before getReadings refetches the full window. The full refetch is
  // what picks up entries that were backfilled or edited *behind* the newest
  // entry (a date$gt fetch cannot see those), so this bounds their staleness.
  NS3_READINGS_CACHE_TTL_MIN = 10;

  // How many devicestatus records the metadata probe asks for. More than one
  // because a site can have several uploaders writing to the collection and
  // each field is taken from the newest record that carries it; small because
  // they arrive every few minutes, so ten covers well under an hour and there
  // is no point reading further back for a "current" reservoir level.
  NS3_DEVICESTATUS_LIMIT = 10;

  // How many treatments the metadata probe asks for. This one figure has to
  // serve two windows that pull in opposite directions: the overlays want the
  // last day or so, while the sensor-age fallback wants to reach back to the
  // last Sensor Start, which on a 10-day sensor can be hundreds of treatments
  // ago. 200 covers a day comfortably for most people and reaches a sensor
  // change for many; a looping site logging a micro-bolus every five minutes
  // fills it in well under a day, and there the sensor-age fallback simply
  // finds nothing — which is why the devicestatus expiry is the primary source
  // for sensor life and this is only a fallback.
  NS3_TREATMENTS_LIMIT = 200;

var
  NS3NoCacheTokenSeq: LongInt = 0;

function NS3FormatDurationHours(const hoursTotal: integer): string;
var
  d, h: integer;
begin
  d := hoursTotal div 24;
  h := hoursTotal mod 24;
  if d > 0 then
    Result := Format('%dd %dh', [d, h])
  else
    Result := Format('%dh', [h]);
end;

function NS3NextNoCacheToken: string;
begin
  Result := IntToStr(GetTickCount64) + IntToStr(InterlockedIncrement(NS3NoCacheTokenSeq));
end;

{------------------------------------------------------------------------------
  Read a Nightscout timestamp — ms or second epoch, or an ISO 8601 string — and
  return it in *local* time.

  Local, not UTC, because every consumer compares the result against Now or
  plots it beside a reading whose date is already local. Nightscout writes
  created_at in UTC, and FPC treats an ISO string with no zone designator as
  UTC too, which is the right reading for this collection: uploaders that omit
  the Z still mean UTC.
 ------------------------------------------------------------------------------}
function NS3TryDateTimeFromJsonValue(const value: TJSONData; out dt: TDateTime): boolean;
var
  raw: string;
  epoch: int64;
  asNum: double;
begin
  Result := false;
  dt := 0;
  if not Assigned(value) then
    Exit;

  case value.JSONType of
    jtNumber:
      begin
        asNum := value.AsFloat;
        if asNum > 1.0e11 then
          dt := UnixToDateTime(Trunc(asNum / 1000), False)
        else if asNum > 1.0e9 then
          dt := UnixToDateTime(Trunc(asNum), False)
        else
          Exit;
        Result := true;
      end;
    jtString:
      begin
        raw := Trim(value.AsString);
        if raw = '' then
          Exit;

        if TryStrToInt64(raw, epoch) then
        begin
          if epoch > 100000000000 then
            dt := UnixToDateTime(epoch div 1000, False)
          else if epoch > 1000000000 then
            dt := UnixToDateTime(epoch, False)
          else
            Exit;
          Result := true;
          Exit;
        end;

        Result := TryISO8601ToDate(raw, dt, False) and (dt > 0);
        if not Result then
          dt := 0;
      end;
  end;
end;

function NS3TryGetPathDate(const root: TJSONData; const path: string; out dt: TDateTime): boolean;
begin
  Result := NS3TryDateTimeFromJsonValue(root.FindPath(path), dt);
end;

{------------------------------------------------------------------------------
  Find the record array in a v3 response.

  Three shapes are in circulation for the same collection: a bare array (v1 and
  the ".json" endpoints), the v3 wrapper with the records under "result", and
  the older nesting with them under "result" and then the collection's own name.
  Any of them can come back depending on which endpoint the request fell through
  to, so every caller that walks records goes through here.
 ------------------------------------------------------------------------------}
function NS3FindArrayNode(const root: TJSONData; const nestedName: string): TJSONData;
var
  node: TJSONData;
begin
  Result := nil;
  if not Assigned(root) then
    Exit;

  if root.JSONType = jtArray then
    Exit(root);
  if root.JSONType <> jtObject then
    Exit;

  node := root.FindPath('result');
  if Assigned(node) and (node.JSONType = jtArray) then
    Exit(node);

  if nestedName <> '' then
  begin
    node := root.FindPath('result.' + nestedName);
    if Assigned(node) and (node.JSONType = jtArray) then
      Exit(node);
  end;
end;

{------------------------------------------------------------------------------
  When a devicestatus record says the sensor session ends.

  The field names below are xDrip+/xdrip-js spellings that have all been seen
  in the wild; Nightscout itself imposes no schema on the plugin sections of a
  devicestatus record, so the list is a set of candidates rather than a spec.
 ------------------------------------------------------------------------------}
function NS3TryFindSensorExpiry(const node: TJSONData; out expiresAt: TDateTime): boolean;
begin
  Result := NS3TryGetPathDate(node, 'xdripjs.sensor.expires', expiresAt) or
    NS3TryGetPathDate(node, 'xdripjs.sensor.expiresAt', expiresAt) or
    NS3TryGetPathDate(node, 'xdripjs.sensor.expiry', expiresAt) or
    NS3TryGetPathDate(node, 'xdripjs.sensor.expiration', expiresAt) or
    NS3TryGetPathDate(node, 'xdripjs.sensor.expires_at', expiresAt) or
    NS3TryGetPathDate(node, 'sensor.expiresAt', expiresAt) or
    NS3TryGetPathDate(node, 'sensor.expiry', expiresAt);
end;

{------------------------------------------------------------------------------
  When a devicestatus record says the sensor session started. Only good for an
  age, never for an expiry: how long a session lasts depends on the sensor
  (10 days for a G6, 14 for a Libre), and guessing it would turn a fresh sensor
  into an expiry warning.
 ------------------------------------------------------------------------------}
function NS3TryFindSensorStart(const node: TJSONData; out startedAt: TDateTime): boolean;
begin
  Result := NS3TryGetPathDate(node, 'xdripjs.sensor.started_at', startedAt) or
    NS3TryGetPathDate(node, 'xdripjs.sensor.startedAt', startedAt) or
    NS3TryGetPathDate(node, 'xdripjs.sensor.startDate', startedAt) or
    NS3TryGetPathDate(node, 'xdripjs.sessionStart', startedAt) or
    NS3TryGetPathDate(node, 'xdripjs.session_start', startedAt) or
    NS3TryGetPathDate(node, 'xdripjs.started_at', startedAt) or
    NS3TryGetPathDate(node, 'sensor.started_at', startedAt);
end;

{------------------------------------------------------------------------------
  When a record happened, in local time.

  created_at is what Nightscout's own UI sorts treatments by; mills/date are the
  ms-epoch forms different uploaders write instead. srvCreated is deliberately
  last: it is when the server was told, which on a delayed upload is not when
  the insulin went in.
 ------------------------------------------------------------------------------}
function NS3TryRecordTime(const node: TJSONData; out stamp: TDateTime): boolean;
begin
  Result := NS3TryGetPathDate(node, 'created_at', stamp) or
    NS3TryGetPathDate(node, 'createdAt', stamp) or
    NS3TryGetPathDate(node, 'mills', stamp) or
    NS3TryGetPathDate(node, 'date', stamp) or
    NS3TryGetPathDate(node, 'timestamp', stamp) or
    NS3TryGetPathDate(node, 'sysTime', stamp) or
    NS3TryGetPathDate(node, 'srvCreated', stamp);
end;

{------------------------------------------------------------------------------
  Read a number from a path. A JSON null, an empty string or a field that is
  simply absent all mean "not reported" and return False, so a field we cannot
  read never becomes a dose, a meal or an empty reservoir.
 ------------------------------------------------------------------------------}
function NS3TryGetPathNumber(const node: TJSONData; const path: string;
  out value: double): boolean;
var
  data: TJSONData;
begin
  value := 0;
  data := node.FindPath(path);
  Result := (data is TJSONNumber);
  if Result then
    value := data.AsFloat;
end;

{------------------------------------------------------------------------------
  Read a string from a path. A JSON null, a non-string value or an absent
  field all read as the empty string rather than raising: these collections
  carry whatever an uploader wrote, and TJSONNull.AsString throws — which,
  with the whole metadata probe wrapped in one try..except, would let a
  single bad record silently cost the device status, both treatment overlays
  and the sensor badge for an entire cache TTL.
 ------------------------------------------------------------------------------}
function NS3PathString(const node: TJSONData; const path: string): string;
var
  data: TJSONData;
begin
  Result := '';
  data := node.FindPath(path);
  if Assigned(data) and (data.JSONType = jtString) then
    Result := data.AsString;
end;

function NS3ExtractSensorStatusSuffix(const devStatusResp: string): string;
var
  js, arrNode, node: TJSONData;
  i, count: integer;
  expiresAt, startedAt: TDateTime;
  hoursLeft, ageHours: integer;

  // The record to inspect: an array element, or the payload itself when a
  // one-item query came back as the lone record, unwrapped.
  function RecordAt(const idx: integer): TJSONData;
  begin
    if Assigned(arrNode) then
      Result := arrNode.Items[idx]
    else
      Result := js;
  end;

begin
  Result := '';
  if Trim(devStatusResp) = '' then
    Exit;

  js := nil;
  try
    js := GetJSON(devStatusResp);
  except
    Exit;
  end;

  try
    arrNode := NS3FindArrayNode(js, 'devicestatus');
    if Assigned(arrNode) then
      count := arrNode.Count
    else if js.JSONType = jtObject then
      count := 1
    else
      count := 0;

    // Every fetched record is walked, newest first, not just the newest one:
    // a site can have several uploaders writing to the collection, and the
    // newest record is routinely a phone with nothing on it but its battery —
    // the same reason ExtractDeviceStatus takes each field from the newest
    // record that carries it. An expiry anywhere beats a session start
    // anywhere, since a start only ever yields an age.
    for i := 0 to count - 1 do
    begin
      node := RecordAt(i);
      if not Assigned(node) then
        Continue;

      if NS3TryFindSensorExpiry(node, expiresAt) then
      begin
        hoursLeft := Trunc((expiresAt - Now) * 24);
        if hoursLeft < 0 then
          Exit(' (sensor expired)');
        Exit(' (sensor ' + NS3FormatDurationHours(hoursLeft) + ' left)');
      end;
    end;

    for i := 0 to count - 1 do
    begin
      node := RecordAt(i);
      if not Assigned(node) then
        Continue;

      if NS3TryFindSensorStart(node, startedAt) then
      begin
        ageHours := Trunc((Now - startedAt) * 24);
        if ageHours >= 0 then
          Exit(' (sensor age ' + NS3FormatDurationHours(ageHours) + ')');
      end;
    end;
  finally
    js.Free;
  end;
end;

function NS3ExtractSensorStatusSuffixFromTreatments(const treatmentsResp: string): string;
var
  js, arrNode, item: TJSONData;
  i: integer;
  evtType: string;
  startedAt: TDateTime;
  ageHours: integer;
begin
  Result := '';
  if Trim(treatmentsResp) = '' then
    Exit;

  js := nil;
  try
    js := GetJSON(treatmentsResp);
  except
    Exit;
  end;

  try
    arrNode := NS3FindArrayNode(js, 'treatments');
    if arrNode = nil then
      Exit;

    for i := 0 to arrNode.Count - 1 do
    begin
      item := arrNode.Items[i];
      if not Assigned(item) then
        Continue;

      evtType := NS3PathString(item, 'eventType');
      if (evtType <> 'Sensor Start') and (evtType <> 'Sensor Change') then
        Continue;

      startedAt := 0;
      if NS3TryRecordTime(item, startedAt) then
      begin
        ageHours := Trunc((Now - startedAt) * 24);
        if ageHours >= 0 then
          Exit(' (sensor age ' + NS3FormatDurationHours(ageHours) + ')');
      end;
    end;
  finally
    js.Free;
  end;
end;

resourcestring
sParamUsername = 'NightScout URL';
sParamPassword = 'Auth token';
sParamDesc = '** BETA DRIVER - Please use "NightScout" for daily use! **' + #13#10 +
  'NightScout v3 setup (use FULL access token):' + #13#10#13#10 +
  '1) Open your NightScout site (e.g., https://your-site).' + #13#10 +
  '2) Go to Admin -> Tokens — or API Secret.' + #13#10 +
  '3) If you use Tokens:' + #13#10 + '   - Create a token with at least READ scope.' +
  #13#10 + '   - Copy the FULL access token value exactly as shown.' + #13#10 +
  '4) In Trndi:' + #13#10 + '   - Address: enter your NightScout URL' + #13#10 +
  '   - Auth: paste the FULL access token.' + #13#10 + #13#10 +
  'Tip: Both "abc123" and "token=abc123" formats are accepted.' + #13#10 +
  'Note: If you instead use the legacy API Secret, paste your API Secret value as-is.' + #10#13 +
  'Note 2: Your access token should look like: trndi-abc123 (or whatever name you chose)';
sParamDescHTML =
  '<div style="font-family: Arial, sans-serif; line-height: 1.6;">' +
  '<div style="background: #dc3545; color: white; padding: 15px; border-radius: 6px; margin-bottom: 20px; font-weight: bold; text-align: center; border: 2px solid #c82333;">' +
  '⚠️ BETA DRIVER - Please use "NightScout" for daily use! ⚠️' +
  '</div>' +
  '<h2 style="margin-bottom: 10px;">🌙 NightScout v3 Setup</h2>' +
  '<p style="color: #7f8c8d; font-style: italic; margin-bottom: 15px;">(use FULL access token)</p>' +
  '<ol style="padding-left: 20px;">' +
  '<li style="margin-bottom: 10px;">Open your NightScout site (e.g., <code style="background: #6495ED; padding: 2px 6px; border-radius: 3px;">https://your-site</code>).</li>' +
  '<li style="margin-bottom: 10px;">Go to <strong>Admin → Tokens</strong> — or <strong>API Secret</strong>.</li>' +
  '<li style="margin-bottom: 10px;">If you use Tokens:' +
  '<ul style="margin-top: 5px; padding-left: 20px;">' +
  '<li>Create a token with at least <strong>READ</strong> scope.</li>' +
  '<li>Copy the <strong>FULL</strong> access token value exactly as shown.</li>' +
  '</ul>' +
  '</li>' +
  '<li style="margin-bottom: 10px;">In Trndi:' +
  '<ul style="margin-top: 5px; padding-left: 20px;">' +
  '<li><strong>Address:</strong> enter your NightScout URL</li>' +
  '<li><strong>Auth:</strong> paste the FULL access token.</li>' +
  '</ul>' +
  '</li>' +
  '</ol>' +
  '<div style="border-left: 4px solid #0d6efd; padding: 12px; margin-top: 15px; border-radius: 4px;">' +
  '<p style="margin: 0;"><strong>💡 Tip:</strong> Both <code style="background: #6F8FAF; padding: 2px 6px; border-radius: 3px;">abc123</code> and <code style="background: #6F8FAF; padding: 2px 6px; border-radius: 3px;">token=abc123</code> are accepted.</p>' +
  '</div>' +
  '<div style="border-left: 4px solid #ffc107; padding: 12px; margin-top: 15px; border-radius: 4px;">' +
  '<p style="margin: 0 0 8px 0;"><strong>📝 Note:</strong> If you instead use the legacy API Secret, paste your API Secret value as-is.</p>' +
  '<p style="margin: 0;"><strong>📝 Note 2:</strong> Your access token should look like: <code style="background: #6F8FAF; padding: 2px 6px; border-radius: 3px;">trndi-abc123</code> (or whatever name you chose).</p>' +
  '</div>' +
  '</div>';

class function NightScout3.NormalizeV2AccessToken(const AccessToken: string): string;
var
  t: string;
begin
  t := Trim(AccessToken);
  if t = '' then
    Exit('');

  // Nightscout's v2 auth request expects a path segment like "token=<name>-<secret>".
  // Accept both raw token value and already-prefixed input.
  if (Length(t) >= 6) and (LowerCase(Copy(t, 1, 6)) = 'token=') then
    Exit(t);

  Exit('token=' + t);
end;

function NightScout3.TryRequestV3(const PathPreferred, PathLegacyJson: string;
  const Params: array of string; out Resp: string): boolean;
var
  authErr: string;

  function IsSuccessResponse(const AResp: string): boolean;
  begin
    Result := (Trim(AResp) <> '') and not ((AResp <> '') and (AResp[1] = '+')) and
      (not IsAuthFailureResponse(AResp));
  end;

  function TryRefreshToken: boolean;
  begin
    Result := false;
    if FTokenSuffix = '' then
      Exit;

    authErr := '';
    Result := GetAuthToken(authErr);
    if (not Result) and (Trim(authErr) <> '') then
      lastErr := authErr;
  end;
begin
  Resp := native.request(false, PathPreferred, Params, '', BearerHeader);
  if IsSuccessResponse(Resp) then
    Exit(true);

  if IsAuthFailureResponse(Resp) and TryRefreshToken then
  begin
    Resp := native.request(false, PathPreferred, Params, '', BearerHeader);
    if IsSuccessResponse(Resp) then
      Exit(true);
  end;

  Resp := native.request(false, PathLegacyJson, Params, '', BearerHeader);
  if IsSuccessResponse(Resp) then
    Exit(true);

  if IsAuthFailureResponse(Resp) and TryRefreshToken then
  begin
    Resp := native.request(false, PathLegacyJson, Params, '', BearerHeader);
    if IsSuccessResponse(Resp) then
      Exit(true);
  end;

  Result := false;
end;

function NightScout3.IsAuthFailureResponse(const Resp: string): boolean;
var
  L: string;
begin
  L := LowerCase(Trim(Resp));
  if L = '' then
    Exit(false);

  Result :=
    (Pos('unauthorized', L) > 0) or
    (Pos('forbidden', L) > 0) or
    (Pos('jwt expired', L) > 0) or
    (Pos('token expired', L) > 0) or
    (Pos('invalid token', L) > 0) or
    (Pos('"status":401', L) > 0) or
    (Pos('"status": 401', L) > 0) or
    (Pos('"code":401', L) > 0) or
    (Pos('"code": 401', L) > 0);

  if (not Result) and (Resp <> '') and (Resp[1] = '+') then
    Result := (Pos('401', L) > 0) or (Pos('unauthorized', L) > 0);
end;

{------------------------------------------------------------------------------
  TryGetEntriesLastModified
  -------------------------
  Probe /api/v3/lastModified and extract the entries collection's last
  modification timestamp (ms epoch). Returns false without side effects on
  transport-level failures (so a network blip doesn't disable the probe for
  the whole session); marks the endpoint unsupported when the server answers
  but the payload has no usable collections.entries value (e.g. an older
  server 404-ing with an error body).
 ------------------------------------------------------------------------------}
function NightScout3.TryGetEntriesLastModified(out msVal: int64): boolean;
var
  resp: string;
  js, node: TJSONData;
begin
  Result := false;
  msVal := 0;
  if FLastModifiedUnsupported then
    Exit;

  // TryRequestV3 handles bearer refresh; lastModified has no legacy ".json"
  // variant, so the same path doubles as the fallback.
  if not TryRequestV3(NS3_LASTMODIFIED, NS3_LASTMODIFIED, [], resp) then
    Exit;

  js := nil;
  try
    js := GetJSON(resp);
  except
    FLastModifiedUnsupported := true;
    Exit;
  end;

  try
    node := js.FindPath('collections.entries');
    if not Assigned(node) then
      node := js.FindPath('result.collections.entries');
    if Assigned(node) and (node.JSONType = jtNumber) then
    begin
      msVal := node.AsInt64;
      Result := msVal > 0;
    end;
    if not Result then
      FLastModifiedUnsupported := true;
  finally
    js.Free;
  end;
end;

{------------------------------------------------------------------------------
  getMaxAge
  --------------------
  Returns the maximum age (in minutes) of readings provided by the backend
 ------------------------------------------------------------------------------}
function NightScout3.getMaxAge: integer;
begin
  result := -1; // No specific maximum age enforced
end;

{------------------------------------------------------------------------------
  getSystemName
  --------------------
  Returns the name of this API
 ------------------------------------------------------------------------------}
function NightScout3.getSystemName: string;
begin
  result := 'NightScout v3';
end;

{------------------------------------------------------------------------------
  Helper: Normalize and store site base and API base URL.
 ------------------------------------------------------------------------------}
constructor NightScout3.Create(user, pass: string);
begin
  // Normalize site base (no trailing slash)
  FSiteBase := TrimRightSet(user, ['/']);
  // Nightscout v3 uses v2 auth request flow, which expects a path segment
  // like "token=<accessToken>".
  FTokenSuffix := NormalizeV2AccessToken(pass);

  // Set UA and API base URL before inherited (so native is initialized correctly)
  ua := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := FSiteBase + NS3_URL_BASE;

  // Nothing fetched yet: the accessors must report "not reported" rather than
  // an empty delivery list, which would read as "no insulin given".
  FDeviceStatusValid := false;
  clearDeviceStatus(FDeviceStatus);
  FBolusesValid := false;
  SetLength(FBoluses, 0);
  FCarbsValid := false;
  SetLength(FCarbs, 0);

  inherited;
end;

function NightScout3.BearerHeader: string;
begin
  if FToken <> '' then
    Result := 'Authorization=Bearer ' + FToken
  else
    Result := '';
end;

function NightScout3.BuildAuthURL: string;
begin
  // v2 authorization endpoint lives under /api/v2/
  Result := FSiteBase + '/api/v2/authorization/request/' + FTokenSuffix;
end;

{------------------------------------------------------------------------------
  GetAuthToken
  -------------
  Perform a GET against the v2 authorization endpoint to retrieve a JWT.
 ------------------------------------------------------------------------------}
function NightScout3.GetAuthToken(out Err: string): boolean;
var
  url, res: string;
  js: TJSONData;
begin
  Result := false;
  Err := '';

  if FTokenSuffix = '' then
  begin
    Err := 'Missing access token for Nightscout v2 authorization.';
    Exit;
  end;

  url := BuildAuthURL;
  if not TrndiNative.getURL(url, res) then
  begin
    Err := 'Failed to contact Nightscout auth endpoint';
    Exit;
  end;

  if Trim(res) = '' then
  begin
    Err := 'Empty response from Nightscout auth endpoint';
    Exit;
  end;

  try
    js := GetJSON(res);
    try
      if (js.JSONType = jtObject) and (TJSONObject(js).IndexOfName('token') <> -1) then
      begin
        FToken := TJSONObject(js).Get('token');
        Result := FToken <> '';
        if not Result then
          Err := 'Auth token missing from response';
      end
      else
        Err := 'Unexpected auth JSON shape';
    finally
      js.Free;
    end;
  except
    on E: Exception do
      Err := 'Auth JSON parse error: ' + E.Message;
  end;
end;

{------------------------------------------------------------------------------
  connect
  -------
  Acquire bearer JWT, fetch status.json from v3 for time calibration,
  and always fetch thresholds from settings.json.
 ------------------------------------------------------------------------------}
function NightScout3.connect: boolean;
var
  resp: string;
  js: TJSONData;
  o, topObj, settings, thresholds: TJSONObject;
  serverEpoch: int64;
  UTCDateTime: TDateTime;
  authErr: string;
  node: TJSONData;
  // no thresholds parsing from v3 status; handled by FetchLegacyThresholds
  // we no longer try to read thresholds from status
begin
  Result := false;
  lastErr := '';

  // 1) Acquire JWT via v2 authorization flow (only if a token suffix is configured)
  if FTokenSuffix <> '' then
  begin
    if not GetAuthToken(authErr) then
    begin
      lastErr := authErr;
      Exit;
    end;
  end;

  // 2) Fetch v3 status (bearer). Prefer /api/v3/status, fall back to status.json.
  TryRequestV3(NS3_STATUS, NS3_STATUS_JSON, [], resp);
  {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams([])]));{$endif}

  if Trim(resp) = '' then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := 'Did not receive any data from the server!';
      Exit;
    end// Try fallback to v1 status.json without relying on baseUrl
  ;

  if (resp <> '') and (resp[1] = '+') then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := TrimLeftSet(resp, ['+']);
      Exit;
    end// Try fallback to v1 status.json as absolute URL
  ;

  if Pos('Unau', resp) > 0 then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
    begin
      lastErr := 'Unauthorized: invalid or expired token';
      Exit;
    end// Fallback to v1 status.json (some setups may allow public status)
  ;

  // 3) Parse JSON
  try
    js := GetJSON(resp);
    try
      if js.JSONType <> jtObject then
      begin
        lastErr := 'Unexpected JSON structure (not an object).';
        Exit;
      end;

      o := TJSONObject(js);

      // Some Nightscout v3 endpoints wrap payload in { status, result: { ... } }
      topObj := o;
      if (o.IndexOfName('result') <> -1) and (o.Find('result').JSONType = jtObject) then
        topObj := TJSONObject(o.Find('result'));

      // server time in ms epoch: prefer srvDate (v3), else fall back to serverTimeEpoch
      serverEpoch := topObj.Get('srvDate', int64(0));
      if serverEpoch <= 0 then
        serverEpoch := topObj.Get('serverTimeEpoch', int64(0));

      // thresholds are not parsed from status anymore

    finally
      // keep js for serverEpoch check below? No, parsed needed values already
      js.Free;
    end;
  except
    on E: Exception do
    begin
      lastErr := 'JSON parse error: ' + E.Message;
      Exit;
    end;
  end;

  // 4) Time calibration
  // serverEpoch now set during JSON parse; no further JSON access here

  if serverEpoch > 0 then
  begin
    // Interpret serverEpoch as UTC
    UTCDateTime := UnixToDateTime(serverEpoch div 1000, True);
    // Calculate time difference: server UTC time minus local UTC time (accounts for clock skew only)
    timeDiff := Round((UTCDateTime - LocalTimeToUniversal(Now)) * 86400);
    // Set tz so JSToDateTime applies the correction to reading timestamps (seconds)
    tz := timeDiff;

    // Debug: log calibration values to help diagnose timezone/sign issues
    {$ifdef DEBUG}
    if DEBUG_LOG_ALERT then
      TrndiDLog('[' + {$i %file%} + ':' + {$i %Line%} + '] time calibration: serverEpoch=' + IntToStr(serverEpoch) +
        ' UTCDateTime=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', UTCDateTime) +
        ' LocalUTC(Now)=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', LocalTimeToUniversal(Now)) +
        ' timeDiff(s)=' + IntToStr(timeDiff) + ' tz(s)=' + IntToStr(tz));
    {$endif}
  end
  else
  begin
    timeDiff := 0;
    tz := 0;
  end;

  // 5) Fetch thresholds using legacy status (aligns with v2 controller semantics)
  FetchLegacyThresholds; // thresholds remain defaults if this fails

  Result := true;
end;

{------------------------------------------------------------------------------
  FetchLegacyThresholds
  ---------------------
  Try to read thresholds from Nightscout v1 status.json (settings.thresholds),
  first attempting an authenticated request via native.request with baseUrl
  temporarily redirected to /api/v1/, then falling back to an unauthenticated
  absolute GET. Returns true if thresholds were found and set.
 ------------------------------------------------------------------------------}
function NightScout3.FetchLegacyThresholds: boolean;
var
  resp: string;
  js: TJSONData;
  o, settings, th: TJSONObject;
begin
  Result := false;

  // Attempt via native.request using absolute v1 URL and bearer header (no prefix)
  resp := native.request(false, FSiteBase + '/api/v1/status.json',
    [], '', BearerHeader, false {no prefix});
  {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams([])]));{$endif}

  // If empty or app-level error, try plain GET
  if (Trim(resp) = '') or ((resp <> '') and (resp[1] = '+')) then
    if not TrndiNative.getURL(FSiteBase + '/api/v1/status.json', resp) then
      Exit;

  try
    js := GetJSON(resp);
  except
    Exit;
  end;

  try
    if js.JSONType <> jtObject then
      Exit;
    o := TJSONObject(js);
    settings := nil;
    if (o.IndexOfName('settings') <> -1) and (o.Find('settings').JSONType =
      jtObject) then
      settings := TJSONObject(o.Find('settings'));
    if Assigned(settings) and (settings.Find('thresholds') <> nil) and
      (settings.Find('thresholds').JSONType = jtObject) then
    begin
      th := TJSONObject(settings.Find('thresholds'));
      cgmHi := th.Get('bgHigh', 0);
      cgmLo := th.Get('bgLow', 0);
      cgmRangeHi := th.Get('bgTargetTop', CGM_RANGE_HI_DISABLED);
      cgmRangeLo := th.Get('bgTargetBottom', 0);
      Result := true;
    end;
  finally
    js.Free;
  end;
end;

(*******************************************************************************
  Sensor/pump status and treatments

  Nightscout is a store, not a device: what these two collections contain is
  whatever somebody's uploader chose to write. An AAPS or Loop rig publishes a
  reservoir level, a pump battery and a suspend flag; an xDrip+ uploader
  publishes sensor session detail; a site fed only by a CGM bridge publishes
  neither. Nothing is inferred from an absent field — every one of them keeps
  its "unknown" sentinel, because a site that never reports a reservoir must
  not look like a pump that has run dry.
 ******************************************************************************)

{------------------------------------------------------------------------------
  Read a boolean that an uploader may have written as a JSON boolean or as the
  string "true". Anything else, including absent, is False.
 ------------------------------------------------------------------------------}
function NS3PathIsTrue(const node: TJSONData; const path: string): boolean;
var
  data: TJSONData;
begin
  Result := false;
  data := node.FindPath(path);
  if not Assigned(data) then
    Exit;

  case data.JSONType of
    jtBoolean:
      Result := data.AsBoolean;
    jtString:
      Result := LowerCase(Trim(data.AsString)) = 'true';
  end;
end;

{------------------------------------------------------------------------------
  Whether a treatment was given by a loop rather than by the user.

  AAPS and Trio mark their super-micro-boluses with isSMB, Loop marks its
  automatic corrections with `automatic`, and some uploaders use an SMB event
  type instead. Any of the three is enough: the point of the flag is to let a
  looping site's steady drip of micro-boluses be kept off the graph separately
  from the doses the user gave, and one missed flag would put a hairline stem
  among them.
 ------------------------------------------------------------------------------}
function NS3TreatmentIsAutomatic(const node: TJSONData; const kind: string): boolean;
begin
  Result := (kind = 'SMB') or (kind = 'SUPER MICRO BOLUS') or
    NS3PathIsTrue(node, 'isSMB') or
    NS3PathIsTrue(node, 'automatic') or
    NS3PathIsTrue(node, 'isAutomatic');
end;

{------------------------------------------------------------------------------
  Fill the sensor/pump status cache from a devicestatus payload.
 ------------------------------------------------------------------------------}
procedure NightScout3.ExtractDeviceStatus(const AResponse: string);
var
  js, arrNode: TJSONData;
  i, filled, hours: integer;
  stamp, expiresAt: TDateTime;
  reservoirAt, batteryAt, suspendAt, sensorAt, statusAt: TDateTime;
  num: double;
  txt, lowered: string;

  // The newest record that carries a field wins it. A record with no usable
  // timestamp counts as the beginning of time, so it only fills a field that
  // no timestamped record filled.
  function Newer(const filledAt: TDateTime): boolean;
  begin
    Result := (filledAt = 0) or (stamp > filledAt);
  end;

  procedure ReadRecord(const item: TJSONData);
  begin
    if not Assigned(item) then
      Exit;
    if not NS3TryRecordTime(item, stamp) then
      stamp := 0;

    // Units left in the cartridge. Nightscout has no percentage counterpart,
    // so reservoirPercent stays unknown.
    if NS3TryGetPathNumber(item, 'pump.reservoir', num) and (num >= 0) and
      Newer(reservoirAt) then
    begin
      FDeviceStatus.reservoirUnits := num;
      reservoirAt := stamp;
      Inc(filled);
    end;

    // The *pump's* battery. uploader.battery is deliberately ignored: that is
    // the phone doing the uploading, and a flat phone is not a flat pump.
    if NS3TryGetPathNumber(item, 'pump.battery.percent', num) and
      (num >= 0) and (num <= 100) and Newer(batteryAt) then
    begin
      FDeviceStatus.pumpBatteryPercent := Round(num);
      batteryAt := stamp;
      Inc(filled);
    end;

    if Assigned(item.FindPath('pump.status.suspended')) and Newer(suspendAt) then
    begin
      FDeviceStatus.pumpSuspended := NS3PathIsTrue(item, 'pump.status.suspended');
      suspendAt := stamp;
      Inc(filled);
    end;

    // A null or non-string status reads as '' — the text fields' own unknown
    // sentinel — and does not claim the slot, so it cannot mask a real status
    // in an older record.
    txt := Trim(NS3PathString(item, 'pump.status.status'));
    if (txt <> '') and Newer(statusAt) then
    begin
      FDeviceStatus.statusMessage := txt;
      statusAt := stamp;
      Inc(filled);

      // Some uploaders report the suspend state only as this text. It is held
      // to the same recency rule as the explicit flag rather than simply
      // overwriting it, or an old record reading "suspended" would undo a
      // newer one that says delivery has resumed.
      if (LowerCase(txt) = 'suspended') and Newer(suspendAt) then
      begin
        FDeviceStatus.pumpSuspended := true;
        suspendAt := stamp;
      end;
    end;

    // Sensor life is only ever taken from an explicit expiry timestamp. A
    // session start is an age, and turning an age into a remaining life needs
    // the session length, which varies by sensor — guessing it would announce
    // a fresh sensor as an expiring one.
    if NS3TryFindSensorExpiry(item, expiresAt) and Newer(sensorAt) then
    begin
      hours := Trunc((expiresAt - Now) * 24);
      if hours < 0 then
        hours := 0;
      FDeviceStatus.sensorDurationHours := hours;
      sensorAt := stamp;
      Inc(filled);

      txt := Trim(NS3PathString(item, 'xdripjs.stateString'));
      if txt = '' then
        txt := Trim(NS3PathString(item, 'xdripjs.stateStringShort'));
      FDeviceStatus.sensorState := txt;

      // Only a state that names a failure counts as one. "Stopped" does not:
      // a session the user ended is not a sensor that broke, and sensorOK
      // defaults to True precisely so silence is never read as a fault.
      lowered := LowerCase(txt);
      if (Pos('fail', lowered) > 0) or (Pos('error', lowered) > 0) or
        (Pos('expired', lowered) > 0) then
        FDeviceStatus.sensorOK := false;
    end;
  end;

begin
  clearDeviceStatus(FDeviceStatus);
  FDeviceStatusValid := false;
  if Trim(AResponse) = '' then
    Exit;

  js := nil;
  try
    js := GetJSON(AResponse);
  except
    Exit;
  end;

  try
    filled := 0;
    reservoirAt := 0;
    batteryAt := 0;
    suspendAt := 0;
    sensorAt := 0;
    statusAt := 0;

    arrNode := NS3FindArrayNode(js, 'devicestatus');
    if Assigned(arrNode) then
      for i := 0 to arrNode.Count - 1 do
        ReadRecord(arrNode.Items[i])
    else if js.JSONType = jtObject then
      // A lone record, unwrapped: some deployments answer a one-item query
      // with the object itself rather than an array of one.
      ReadRecord(js);

    // Transmitter battery is left unknown on purpose: what Nightscout carries
    // for it is xdripjs's voltagea/voltageb in millivolts, and a percentage
    // derived from those would be an invention of ours, not a device reading.
    FDeviceStatusValid := filled > 0;
  finally
    js.Free;
  end;
end;

{------------------------------------------------------------------------------
  Fill the insulin-delivery and carbohydrate caches from a treatments payload.
 ------------------------------------------------------------------------------}
procedure NightScout3.ExtractTreatments(const AResponse: string);
var
  js, arrNode, item: TJSONData;
  i, j, bolusCount, carbCount: integer;
  stamp: TDateTime;
  insulin, grams: double;
  kind: string;
  swapBolus: TBolusEntry;
  swapCarb: TCarbEntry;
begin
  SetLength(FBoluses, 0);
  FBolusesValid := false;
  SetLength(FCarbs, 0);
  FCarbsValid := false;
  if Trim(AResponse) = '' then
    Exit;

  js := nil;
  try
    js := GetJSON(AResponse);
  except
    Exit;
  end;

  try
    arrNode := NS3FindArrayNode(js, 'treatments');
    if arrNode = nil then
      Exit;

    SetLength(FBoluses, arrNode.Count);
    SetLength(FCarbs, arrNode.Count);
    bolusCount := 0;
    carbCount := 0;

    for i := 0 to arrNode.Count - 1 do
    begin
      item := arrNode.Items[i];
      if not Assigned(item) then
        Continue;

      // A record we cannot place on the timeline is no use to an overlay, and
      // a dose plotted at the wrong time is worse than one not plotted at all.
      if not NS3TryRecordTime(item, stamp) then
        Continue;

      kind := UpperCase(Trim(NS3PathString(item, 'eventType')));

      if not NS3TryGetPathNumber(item, 'carbs', grams) then
        grams := 0;

      // `insulin` is what was delivered. On a Combo Bolus it is the immediate
      // part only; the extended remainder is delivered over the following hours
      // and Nightscout records it as a rate, not as insulin that has gone in.
      if NS3TryGetPathNumber(item, 'insulin', insulin) and (insulin > 0) then
      begin
        FBoluses[bolusCount] := Default(TBolusEntry);
        FBoluses[bolusCount].time := stamp;
        FBoluses[bolusCount].units := insulin;
        FBoluses[bolusCount].kind := kind;
        FBoluses[bolusCount].automatic := NS3TreatmentIsAutomatic(item, kind);
        if grams > 0 then
          FBoluses[bolusCount].carbs := grams;
        Inc(bolusCount);
      end;

      // One record carries both figures, so a meal bolus contributes one entry
      // to each list and there is no double-counting to reconcile the way
      // CareLink's separate carb markers need.
      if grams > 0 then
      begin
        FCarbs[carbCount] := Default(TCarbEntry);
        FCarbs[carbCount].time := stamp;
        FCarbs[carbCount].grams := grams;
        FCarbs[carbCount].kind := kind;
        Inc(carbCount);
      end;
    end;

    SetLength(FBoluses, bolusCount);
    SetLength(FCarbs, carbCount);

    // Nightscout answers newest-first; the overlays want oldest-first. Insertion
    // sort rather than a reverse, because the sort key is created_at while the
    // server sorted on whatever the query asked for, and a delayed upload can
    // leave the two disagreeing.
    for i := 1 to High(FBoluses) do
    begin
      swapBolus := FBoluses[i];
      j := i - 1;
      while (j >= 0) and (FBoluses[j].time > swapBolus.time) do
      begin
        FBoluses[j + 1] := FBoluses[j];
        Dec(j);
      end;
      FBoluses[j + 1] := swapBolus;
    end;

    for i := 1 to High(FCarbs) do
    begin
      swapCarb := FCarbs[i];
      j := i - 1;
      while (j >= 0) and (FCarbs[j].time > swapCarb.time) do
      begin
        FCarbs[j + 1] := FCarbs[j];
        Dec(j);
      end;
      FCarbs[j + 1] := swapCarb;
    end;

    // Valid even when empty: the collection was read and had nothing in it,
    // which is a different answer from never having been read.
    FBolusesValid := true;
    FCarbsValid := true;
  finally
    js.Free;
  end;
end;

{------------------------------------------------------------------------------
  Accessors. Each reports "nothing was reported" as False rather than as an
  empty list, so a caller can tell a quiet night from a collection it never
  managed to read.
 ------------------------------------------------------------------------------}
function NightScout3.getDeviceStatus(out AStatus: TCGMDeviceStatus): boolean;
begin
  if not FDeviceStatusValid then
  begin
    clearDeviceStatus(AStatus);
    Exit(false);
  end;
  AStatus := FDeviceStatus;
  Result := true;
end;

function NightScout3.getBoluses(out ABoluses: TBolusList): boolean;
begin
  ABoluses := Copy(FBoluses);
  Result := FBolusesValid and (Length(ABoluses) > 0);
end;

function NightScout3.supportsBoluses: boolean;
begin
  Result := true;
end;

function NightScout3.getCarbs(out ACarbs: TCarbList): boolean;
begin
  ACarbs := Copy(FCarbs);
  Result := FCarbsValid and (Length(ACarbs) > 0);
end;

function NightScout3.supportsCarbs: boolean;
begin
  Result := true;
end;

{------------------------------------------------------------------------------
  supportsRapidPolling
  --------------------
  True only while an unchanged getReadings really is one small GET: the server
  answers /lastModified usably, the readings-window cache holds a v3 window,
  and the entries stamp has been learned (that takes until the second fetch —
  the first full fetch stores the window with an unknown stamp). The TTL is
  deliberately not checked: a rapid poll that lands after the TTL lapses does
  one full refetch, re-anchors FCacheFullAt, and the polls after it are cheap
  again — amortized, the cadence stays a probe loop with one full fetch per
  TTL window.
------------------------------------------------------------------------------}
function NightScout3.supportsRapidPolling: boolean;
begin
  Result := (not FLastModifiedUnsupported) and (FCacheFullAt <> 0) and
    (Length(FCache) > 0) and (FCacheLastModified <> 0);
end;

{------------------------------------------------------------------------------
  getReadings
  -----------
  Fetch SGV entries via v3 entries.json. Supports either the v3 object shape
  (status + result array) or a direct array for robustness.
 ------------------------------------------------------------------------------}
function NightScout3.getReadings(minNum, maxNum: integer; extras: string;
out res: string; noCache: boolean): BGResults;
var
  resp: string;
  js, arrNode: TJSONData;
  i: integer;
  t: BGTrend;
  s, dev, sensorSuffix, devStatusResp: string;
  treatmentsResp: string;
  params: array of string;
  fbparams: array of string;
  statusParams: array of string;
  treatParams: array of string;
  oldBase: string;
  deltaField, rssiField, noiseField: TJSONData;
  itemNode, sgvField, deviceField, directionField, prevSgvField: TJSONData;
  deltaValue: glucose;
  currentSgv, prevSgv: integer;
  j, w: integer;
  tempReading: BGReading;
  rssiValue, noiseValue: maybeInt;
  authErr: string;
  useWindowCache, incremental, usedV1Fallback: boolean;
  lmMs, newestMs: int64;
  limitN, pIdx, keep, mergeCount: integer;

function ExtractArrayNode(const jd: TJSONData): TJSONData;
  var
    jo: TJSONObject;
  begin
    Result := nil;
    if not Assigned(jd) then
      Exit;

    if jd.JSONType = jtArray then
      Exit(jd)
    else
    if jd.JSONType = jtObject then
    begin
      jo := TJSONObject(jd);
      if (jo.IndexOfName('result') <> -1) and (jo.Find('result').JSONType = jtArray) then
        Exit(jo.Find('result'))
      else
        Exit(nil);
    end
    else
      Exit(nil);
  end;

var ts, ts2: int64;
LDateMs: int64;
LDateStr: string ;
LUtcOffset: integer;
LMethod: string;
LUnixTrue, LUnixFalse: TDateTime;
LLocalOffsetMin: integer;


begin
  // Default endpoint
  if extras = '' then
    extras := NS3_ENTRIES;

  // The readings-window cache only backs the default v3 entries endpoint;
  // custom endpoints bypass it entirely.
  useWindowCache := extras = NS3_ENTRIES;
  incremental := false;
  usedV1Fallback := false;
  lmMs := 0;
  newestMs := 0;
  limitN := maxNum;

  // When the cache can satisfy this request, first ask /lastModified whether
  // the entries collection changed at all — if not, serve the cached window
  // without touching the entries endpoint. If it changed (or the probe is
  // unavailable), fetch only entries newer than the cached newest (date$gt)
  // and merge below. Forced fetches (noCache) skip both and refetch fully.
  if useWindowCache and (not noCache) and (FCacheFullAt <> 0) and
    (Length(FCache) > 0) and (maxNum <= FCacheMaxNum) and
    (MinutesBetween(Now, FCacheFullAt) < NS3_READINGS_CACHE_TTL_MIN) then
  begin
    if TryGetEntriesLastModified(lmMs) and (FCacheLastModified <> 0) and
      (lmMs = FCacheLastModified) then
    begin
      res := FCacheRaw;
      if maxNum < Length(FCache) then
        Exit(Copy(FCache, 0, maxNum))
      else
        Exit(Copy(FCache, 0, Length(FCache)));
    end;
    incremental := FCacheNewestMs > 0;
    // Keep the cache filled for the largest window it has served so far.
    if incremental and (FCacheMaxNum > limitN) then
      limitN := FCacheMaxNum;
  end;

  // Build v3 query params
  // Nightscout v3 API may have different sort syntax than v1/v2
  // Try using sort$desc for descending order
  SetLength(params, 3 + Ord(incremental) + Ord(noCache));
  params[0] := 'limit=' + IntToStr(limitN);
  params[1] := 'sort$desc=date';  // Try v3 syntax for descending sort
  params[2] := 'fields=date,sgv,delta,direction,device,rssi,noise';
  pIdx := 3;
  if incremental then
  begin
    params[pIdx] := 'date$gt=' + IntToStr(FCacheNewestMs);
    Inc(pIdx);
  end;
  // When noCache is set, append a monotonic `_=` token so intermediaries /
  // client-side HTTP caches don't serve a stale response. NS3 ignores unknown
  // query keys.
  if noCache then
    params[pIdx] := '_=' + NS3NextNoCacheToken;

  try
    // Prefer /api/v3/entries and fall back to entries.json when using default.
    if extras = NS3_ENTRIES then
    begin
      if not TryRequestV3(NS3_ENTRIES, NS3_ENTRIES_JSON, params, resp) then
        resp := '';
    end
    else
      resp := native.request(false, extras, params, '', BearerHeader);
    {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams(params)]));{$endif}
  except
    lastErr := 'Could not contact Nightscout entries endpoint (request failed)';
    Exit; // return empty set
  end;

  res := resp;

  // If v3 denied or errored, fall back to v1 entries
  if Trim(resp) = '' then
  begin
    lastErr := 'Empty response from Nightscout v3 entries endpoint (auth may be required)';
  end
  else if IsAuthFailureResponse(resp) then
  begin
    lastErr := 'Unauthorized accessing Nightscout v3 entries (invalid or missing token)';
  end;

  if (Trim(resp) = '') or IsAuthFailureResponse(resp) or
    ((resp <> '') and (resp[1] = '+')) then
  begin
    // v1 has a different query model (no date$gt); fetch the full window and
    // keep it out of the v3 readings cache.
    incremental := false;
    usedV1Fallback := true;

    // Token may have expired while app is running; refresh before v1 fallback.
    if IsAuthFailureResponse(resp) and (FTokenSuffix <> '') then
    begin
      authErr := '';
      if not GetAuthToken(authErr) then
        if Trim(authErr) <> '' then
          lastErr := authErr;
    end;

    // v1 typically supports count parameter and returns newest-first
    if noCache then
      SetLength(fbparams, 2)
    else
      SetLength(fbparams, 1);
    fbparams[0] := 'count=' + IntToStr(limitN);
    if noCache then
      fbparams[1] := '_=' + NS3NextNoCacheToken;
    resp := native.request(false, FSiteBase + '/api/v1/entries.json',
      fbparams, '', BearerHeader, false {no prefix});
    {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams(fbParams)]));{$endif}
    if Trim(resp) = '' then
    begin
      lastErr := 'Empty response from Nightscout v1 entries endpoint (fallback failed)';
      Exit;
    end;

    if IsAuthFailureResponse(resp) then
    begin
      lastErr := 'Unauthorized accessing Nightscout entries endpoint (token expired or invalid)';
      Exit;
    end;

    res := resp;
  end;

  // Parse JSON and extract entries array
  js := nil;
  try
    js := GetJSON(resp);
  except
    on E: Exception do
      Exit; // broken payload; return empty
  end;

  arrNode := ExtractArrayNode(js);
  if (arrNode = nil) or (arrNode.JSONType <> jtArray) then
  begin
    // Try v1 fallback if we haven't already and current payload isn't an array
    incremental := false;
    usedV1Fallback := true;
    if noCache then
      SetLength(fbparams, 2)
    else
      SetLength(fbparams, 1);
    fbparams[0] := 'count=' + IntToStr(limitN);
    if noCache then
      fbparams[1] := '_=' + NS3NextNoCacheToken;
    resp := native.request(false, FSiteBase + '/api/v1/entries.json',
      fbparams, '', BearerHeader, false {no prefix});
    {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_STATUS, resp, debugParams(fbparams)]));{$endif}
    if Trim(resp) = '' then
    begin
      js.Free;
      Exit;
    end;
    FreeAndNil(js);
    try
      js := GetJSON(resp);
    except
      on E: Exception do
      begin
        js.Free;
        Exit;
      end;
    end;
    arrNode := ExtractArrayNode(js);
    if (arrNode = nil) or (arrNode.JSONType <> jtArray) then
    begin
      js.Free;
      Exit;
    end;
  end;

  // Metadata probe: the sensor age/expiry hint appended to the device name, the
  // sensor/pump status behind the reservoir, sensor-expiry and pump-battery
  // notifications, and the treatments behind the history graph's insulin and
  // carbohydrate overlays. All of it comes from two collections, so it is
  // fetched together and served from a short-lived cache — a normal readings
  // fetch then costs one HTTP round trip rather than three. Forced (noCache)
  // fetches always re-probe.
  if (not noCache) and (FSensorSuffixAt <> 0) and
    (MinutesBetween(Now, FSensorSuffixAt) < NS3_SENSOR_SUFFIX_TTL_MIN) then
    sensorSuffix := FSensorSuffix
  else
  begin
    sensorSuffix := '';
    // v3 pages on `limit`, v1 on `count`; each ignores the other's key, so
    // sending both means one param list serves whichever endpoint answers.
    // Several records rather than one because a site can have more than one
    // uploader writing here, and the newest record is not necessarily the one
    // carrying the pump — see ExtractDeviceStatus.
    if noCache then
      SetLength(statusParams, 4)
    else
      SetLength(statusParams, 3);
    statusParams[0] := 'count=' + IntToStr(NS3_DEVICESTATUS_LIMIT);
    statusParams[1] := 'limit=' + IntToStr(NS3_DEVICESTATUS_LIMIT);
    statusParams[2] := 'sort$desc=created_at';
    if noCache then
      statusParams[3] := '_=' + NS3NextNoCacheToken;
    try
      if not TryRequestV3(NS3_DEVICESTATUS, NS3_DEVICESTATUS_JSON, statusParams, devStatusResp) then
        devStatusResp := native.request(false, FSiteBase + '/api/v1/devicestatus.json',
          statusParams, '', BearerHeader, false {no prefix});
      ExtractDeviceStatus(devStatusResp);
      sensorSuffix := NS3ExtractSensorStatusSuffix(devStatusResp);

      if noCache then
        SetLength(treatParams, 4)
      else
        SetLength(treatParams, 3);
      treatParams[0] := 'limit=' + IntToStr(NS3_TREATMENTS_LIMIT);
      treatParams[1] := 'count=' + IntToStr(NS3_TREATMENTS_LIMIT);
      treatParams[2] := 'sort$desc=created_at';
      if noCache then
        treatParams[3] := '_=' + NS3NextNoCacheToken;
      if not TryRequestV3(NS3_TREATMENTS, NS3_TREATMENTS_JSON, treatParams, treatmentsResp) then
        treatmentsResp := native.request(false, FSiteBase + '/api/v1/treatments.json',
          treatParams, '', BearerHeader, false {no prefix});
      ExtractTreatments(treatmentsResp);

      // Fallback: derive age from latest Sensor Start/Change treatment events.
      if sensorSuffix = '' then
        sensorSuffix := NS3ExtractSensorStatusSuffixFromTreatments(treatmentsResp);
    except
      sensorSuffix := '';
    end;
    FSensorSuffix := sensorSuffix;
    FSensorSuffixAt := Now;
  end;

  try
  SetLength(Result, arrNode.Count);
  w := 0;
  for i := 0 to arrNode.Count - 1 do
  begin
    // The v1 fallback endpoint returns mixed collection entries (cal, mbg, ...)
    // alongside sgv ones; an entry lacking 'sgv' is not a glucose reading and
    // must be skipped rather than treated as a zero-valued one.
    itemNode := arrNode.FindPath(Format('[%d]', [i]));
    if not Assigned(itemNode) then
      Continue;
    sgvField := itemNode.FindPath('sgv');
    if not Assigned(sgvField) then
      Continue;
    currentSgv := sgvField.AsInteger;
    if currentSgv <= 0 then
      Continue;

    with itemNode do
    begin
      deviceField := FindPath('device');
      if Assigned(deviceField) then
        dev := deviceField.AsString
      else
        dev := '';
      if sensorSuffix <> '' then
        dev := dev + sensorSuffix;

      Result[w].Init(mgdl, Self.SystemName);

      // Value and trend delta.
      // Some Nightscout entries may not include delta field
      deltaField := FindPath('delta');
      if Assigned(deltaField) then
        deltaValue := single(deltaField.AsFloat)
      else
      if i < arrNode.Count - 1 then
      begin
          // Get the previous (older) reading's SGV
        prevSgvField := arrNode.FindPath(Format('[%d].sgv', [i + 1]));
        if Assigned(prevSgvField) then
          prevSgv := prevSgvField.AsInteger
        else
          prevSgv := 0;
        deltaValue := single(currentSgv - prevSgv);
      end
      else
          // Last (oldest) entry has no previous reading to compare
        deltaValue := 0// Calculate delta manually from previous reading
// Nightscout returns entries in reverse chronological order (newest first)
      ;

      // Receiver environment details (optional fields in Nightscout).
      rssiField := FindPath('rssi');
      noiseField := FindPath('noise');
      if Assigned(rssiField) then
      begin
        rssiValue.value := rssiField.AsInteger;
        rssiValue.exists := rssiValue.value <> -1;
      end
      else
      begin
        rssiValue.value := 0;
        rssiValue.exists := false;
      end;

      if Assigned(noiseField) then
      begin
        noiseValue.value := noiseField.AsInteger;
        noiseValue.exists := noiseValue.value <> -1;
      end
      else
      begin
        noiseValue.value := 0;
        noiseValue.exists := false;
      end;

      Result[w].update(currentSgv, deltaValue);
      Result[w].updateEnv(dev, rssiValue, noiseValue);

      // Trend mapping by name
      directionField := FindPath('direction');
      if Assigned(directionField) then
        s := directionField.AsString
      else
        s := '';
      // Default to not computable, then try to find a matching textual mapping
      Result[w].trend := TdNotComputable;
      for t := Low(BGTrend) to High(BGTrend) do
      begin
        if BG_TRENDS_STRING[t] = s then
        begin
          Result[w].trend := t;
          Break;
        end;
      end;

      // Use ms epoch when available. Prefer explicit utcOffset when provided
      LDateMs := 0;
      LDateStr := '';
      LUtcOffset := 0;

      if Assigned(FindPath('date')) then
      begin
        LDateMs := FindPath('date').AsInt64;
        // Remember the newest raw timestamp; it becomes the date$gt boundary
        // for the next incremental fetch.
        if LDateMs > newestMs then
          newestMs := LDateMs;
      end;
      if Assigned(FindPath('dateString')) then
        LDateStr := FindPath('dateString').AsString;
      if Assigned(FindPath('utcOffset')) then
        LUtcOffset := FindPath('utcOffset').AsInteger;

      if LDateMs <> 0 then
      begin
        // Timestamp in ms since epoch
        ts := LDateMs div 1000;
        // Prefer explicit ISO date string (UTC) when available to avoid double-applying UTC offsets.
        if LDateStr <> '' then
        begin
          // dateString is ISO 8601 with Z (UTC).
          // If we have a tz calibration, apply it (JSToDateTime) so we correct for server clock skew.
          // If tz is zero (no calibration), convert to system local time using UnixToDateTime(ts, True).
          if tz <> 0 then
          begin
            Result[w].date := JSToDateTime(LDateMs, True);
            LMethod := 'dateString';
          end
          else
          begin
            Result[w].date := UnixToDateTime(ts, False); // system-local conversion (UseUTC=false gives local time on this platform)
            LMethod := 'dateString+local';
          end;
        end
        else if LUtcOffset <> 0 then
        begin
          // No dateString available: apply server-provided utcOffset to UTC epoch
          Result[w].date := UnixToDateTime(ts, False) + (LUtcOffset / 1440); // minutes -> days
          LMethod := 'utcOffset';
        end
        else
        begin
          Result[w].date := JSToDateTime(LDateMs, True);
          LMethod := 'JSTo';
        end;
      end
      else if Assigned(FindPath('srvModified')) then
      begin
        LDateMs := FindPath('srvModified').AsInt64;
        ts2 := LDateMs div 1000;
        if LDateStr <> '' then
        begin
          Result[w].date := UnixToDateTime(ts2, False);
          LMethod := 'dateString';
        end
        else if Assigned(FindPath('utcOffset')) then
        begin
          Result[w].date := UnixToDateTime(ts2, False) + (FindPath('utcOffset').AsInteger / 1440);
          LMethod := 'utcOffset';
        end
        else
        begin
          Result[w].date := JSToDateTime(LDateMs, True);
          LMethod := 'JSTo';
        end;
      end
      else
      begin
        Result[w].date := Now; // fallback
        LMethod := 'Now';
      end;

      {$ifdef DEBUG}
      if DEBUG_LOG_ALERT then
      begin
        // Extra diagnostics: compare UnixToDateTime(true/false) and show system local offset
        try
          LUnixTrue := UnixToDateTime(ts, True);
          LUnixFalse := UnixToDateTime(ts, False);
          LLocalOffsetMin := Round((Now - LocalTimeToUniversal(Now)) * 1440); // minutes
          TrndiDLog('[' + {$i %file%} + ':' + {$i %Line%} + '] debug: unixTrue=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', LUnixTrue) +
            ' unixFalse=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', LUnixFalse) + ' localOffsetMin=' + IntToStr(LLocalOffsetMin));
        except
          // ignore diagnostics failure
        end;

        // Diagnostic log to help debug timezone/timestamp issues
        try
          // Use simple concatenation to avoid Format exceptions while debugging
          TrndiDLog('[' + {$i %file%} + ':' + {$i %Line%} + '] NightScout entry ' + IntToStr(i) +
            ': dateMs=' + IntToStr(LDateMs) + ' dateString="' + LDateStr + '" utcOffset=' + IntToStr(LUtcOffset) + ' method=' + LMethod + ' tz=' + IntToStr(tz) +
            ' computed=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Result[w].date));
        except
          on E: Exception do
            TrndiDLog('[' + {$i %file%} + ':' + {$i %Line%} + '] NightScout entry ' + IntToStr(i) + ': diagnostic log failed: ' + E.Message);
        end;
      end;
      {$endif}

      Result[w].level := getLevel(Result[w].val);
    end;
    Inc(w);
  end;
  SetLength(Result, w);
  finally
    js.Free;
  end;

  // Data from Nightscout v3 should come in descending order (newest first) due to sort=-date
  // However, check and reverse if needed for compatibility
  if (Length(Result) > 1) and (Result[0].date < Result[Length(Result) - 1].date) then
    for i := 0 to (Length(Result) div 2) - 1 do
    begin
      j := Length(Result) - 1 - i;
      tempReading := Result[i];
      Result[i] := Result[j];
      Result[j] := tempReading;
    end// Data came in ascending order (oldest first) - need to reverse
  ;

  // Merge an incremental (date$gt) fetch with the cached window: keep only
  // entries strictly newer than the cached newest — defends against servers
  // or intermediaries that ignore the date$gt filter and return the full
  // window again — then append the cached readings behind the new ones
  // (both are newest-first).
  if incremental and (not usedV1Fallback) then
  begin
    keep := 0;
    for i := 0 to High(Result) do
      if Result[i].date > FCache[0].date then
      begin
        if keep <> i then
          Result[keep] := Result[i];
        Inc(keep);
      end;
    mergeCount := keep + Length(FCache);
    if mergeCount > limitN then
      mergeCount := limitN;
    SetLength(Result, mergeCount);
    for i := keep to mergeCount - 1 do
      Result[i] := FCache[i - keep];
  end;

  // Recalculate deltas for all readings (newest should be first)
  for i := 0 to Length(Result) - 1 do
    if i < Length(Result) - 1 then
    begin
      deltaValue := single(Result[i].val - Result[i + 1].val);
      Result[i].update(Result[i].val, deltaValue);
    end
    else
      Result[i].update(Result[i].val, 0)// Last (oldest) entry has no previous reading
  ;

  // Refresh the readings-window cache with v3 data; the v1 fallback (or an
  // empty window) invalidates it instead so later calls never merge against
  // mixed or stale data. Written before the maxNum trim so the cache keeps
  // the full limitN window for future smaller requests.
  if useWindowCache then
    if (not usedV1Fallback) and (Length(Result) > 0) then
    begin
      FCache := Copy(Result, 0, Length(Result));
      FCacheRaw := res;
      FCacheMaxNum := limitN;
      if not incremental then
      begin
        FCacheFullAt := Now;
        FCacheNewestMs := newestMs;
      end
      else if newestMs > FCacheNewestMs then
        FCacheNewestMs := newestMs;
      FCacheLastModified := lmMs;
    end
    else
      FCacheFullAt := 0;

  // Enforce the maxNum contract: an incremental merge can hold a larger
  // window than this call asked for, and some servers ignore `limit`.
  if Length(Result) > maxNum then
    SetLength(Result, maxNum);
end;

{------------------------------------------------------------------------------
  Provide parameter label captions for Settings UI (NightScout v3 backend).
------------------------------------------------------------------------------}
class function NightScout3.ParamLabel(LabelName: APIParamLabel): string;
begin
  case LabelName of
  APLUser:
    Result := sParamUsername;
  APLPass:
    Result := sParamPassword;
  APLDesc:
    Result := sParamDesc;
  APLDescHTML:
    Result := sParamDescHTML;
  APLCopyright:
    Result := 'Björn Lindh <github.com/slicke>';
  else
    Result := inherited ParamLabel(LabelName);
  end;
end;

{------------------------------------------------------------------------------
  Test connection details for Nightscout v3
  - Performs a v2 authorization request if a token suffix (pass) is provided.
  - Probes v3 status.json using the retrieved bearer token (if available),
    falling back to a public v3 or v1 status endpoint where applicable.
 ------------------------------------------------------------------------------}
class function NightScout3.testConnection(user, pass: string; var res: string): MaybeBool;
var
  tn: TrndiNative;
  base, authURL, resp, localToken, xres: string;
  js: TJSONData;
  rootObj, topObj: TJSONObject;
  serverEpoch: int64;
begin
  res := 'An unknown error occured';
  Result := MaybeBool.False; // default to failure

  // Basic sanity checks for URL
  if (Copy(user, 1, 4) <> 'http') then
    Exit;

  base := TrimRightSet(user, ['/']);

  // If pass is provided, try v2 auth flow to obtain a token
  localToken := '';
  if (Trim(pass) <> '') then
  begin
    authURL := base + '/api/v2/authorization/request/' + NightScout3.NormalizeV2AccessToken(pass);
    if TrndiNative.getURL(authURL, xres) then
    begin
      // parse JSON and extract token
      try
        js := GetJSON(xres);
        try
          if (js.JSONType = jtObject) and (TJSONObject(js).IndexOfName('token') <> -1) then
            localToken := TJSONObject(js).Get('token');
        finally
          js.Free;
        end;
      except
        // JSON parse error -> treat as failure
        res := 'Could not read the response from the server';
        Result := MaybeBool.False;
        Exit;
      end;
    end
    else
    begin
      // Couldn't contact auth endpoint
      res := 'Could not connect to the authentication endpoint';
      Result := MaybeBool.False;
      Exit;
    end;
  end;

  // Try the v3 status endpoint with bearer header if we have a token
  tn := TrndiNative.Create('Mozilla/5.0 (compatible; trndi) TrndiAPI',
    base + NS3_URL_BASE);
  try
    if localToken <> '' then
    begin
      resp := tn.Request(false, NS3_STATUS, [], '', 'Authorization=Bearer ' + localToken);
      if (Trim(resp) = '') or ((resp <> '') and (resp[1] = '+')) then
        resp := tn.Request(false, NS3_STATUS_JSON, [], '', 'Authorization=Bearer ' + localToken);
    end
    else
    begin
      resp := tn.Request(false, NS3_STATUS, [], '', '');
      if (Trim(resp) = '') or ((resp <> '') and (resp[1] = '+')) then
        resp := tn.Request(false, NS3_STATUS_JSON, [], '', '');
    end;

    if Trim(resp) = '' then
      if not TrndiNative.getURL(base + '/api/v1/status.json', resp) then
      begin
        res := 'Could not fetch the glucose data endpoint';
        Result := MaybeBool.False;
        Exit;
      end// Try fallback to v1 absolute URL without auth
    ;

    // Application-level errors prefixed with '+'
    if (resp <> '') and (resp[1] = '+') then
    begin
      res := 'An application-level error occured: ' + resp;
      Result := MaybeBool.False;
      Exit;
    end;

    // Basic unauthorized check
    if Pos('Unau', resp) > 0 then
    begin
      res := 'The api key is wrong, or Trndi does not have enough access rights';
      Result := MaybeBool.False;
      Exit;
    end;

    // Parse JSON and require a valid server time
    try
      js := GetJSON(resp);
      try
        if js.JSONType <> jtObject then
        begin
          res := 'The response could not be parsed';
          Result := MaybeBool.False;
          Exit;
        end;
        rootObj := TJSONObject(js);
        topObj := rootObj;
        // v3 may have a result wrapper
        if (rootObj.IndexOfName('result') <> -1) and
          (rootObj.Find('result').JSONType = jtObject) then
          topObj := TJSONObject(rootObj.Find('result'));

        serverEpoch := topObj.Get('srvDate', int64(0));
        if serverEpoch <= 0 then
          serverEpoch := topObj.Get('serverTimeEpoch', int64(0));
        if serverEpoch <= 0 then
        begin
          res := 'The server time is not correct';
          Result := MaybeBool.False;
          Exit;
        end;
      finally
        js.Free;
      end;
    except
      on E: Exception do
      begin
        res := 'An error occured trying to contact the server: '#10 + E.Message;
        Result := MaybeBool.False;
        Exit;
      end;
    end;

    Result := MaybeBool.True; // success
  finally
    tn.Free;
  end;
end;

function NightScout3.getLimitHigh: integer;
begin
  result := 400;
end;

function NightScout3.getLimitLow: integer;
begin
  result := 40;
end;

function NightScout3.supportsBasal: boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  getBasalRate
  ------------
  Retrieve the current basal rate from the Nightscout v3 server.
  This fetches basal rate data from the server's profile endpoints.
 ------------------------------------------------------------------------------}
function NightScout3.getBasalRate: single;
var
  ResponseStr: string;
  JSONData: TJSONData;
  RootObject: TJSONObject;
  StoreArray: TJSONArray;
  DefaultProfile: TJSONObject;
  BasalArray: TJSONArray;
  BasalEntry: TJSONObject;
  CurrentTime: TDateTime;
  CurrentMinutes: integer;
  i, entryMin, h, m: integer;
  tstr: string;
begin
  result := 0;
  
  // Fetch basal rate from Nightscout v3 API
  try
    if not TryRequestV3(NS3_PROFILE, NS3_PROFILE_JSON, [], ResponseStr) then
      ResponseStr := '';
    {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_PROFILE, responsestr, debugParams([])]));{$endif}
    
    if Trim(ResponseStr) = '' then
    begin
      lastErr := 'No basal rate data received from server';
      Exit;
    end;

    // Parse JSON response
    try
      JSONData := GetJSON(ResponseStr);
    except
      on E: Exception do
      begin
        lastErr := 'Error parsing basal rate JSON: ' + E.Message;
        Exit;
      end;
    end;

    try
      if not (JSONData is TJSONObject) then
        Exit;

      RootObject := TJSONObject(JSONData);

      // Navigate to store array -> default profile -> basal array
      if not (Assigned(RootObject.FindPath('store')) and
              RootObject.FindPath('store').InheritsFrom(TJSONArray)) then
        Exit;
      StoreArray := TJSONArray(RootObject.FindPath('store'));
      if StoreArray.Count = 0 then
        Exit;

      if Assigned(RootObject.FindPath('store[0].defaultProfile')) and
         RootObject.FindPath('store[0].defaultProfile').InheritsFrom(TJSONObject) then
        DefaultProfile := TJSONObject(RootObject.FindPath('store[0].defaultProfile'))
      else if Assigned(RootObject.FindPath('store[0].Default')) and
              RootObject.FindPath('store[0].Default').InheritsFrom(TJSONObject) then
        DefaultProfile := TJSONObject(RootObject.FindPath('store[0].Default'))
      else
        Exit;

      if not (Assigned(DefaultProfile.FindPath('basal')) and
              DefaultProfile.FindPath('basal').InheritsFrom(TJSONArray)) then
        Exit;
      BasalArray := TJSONArray(DefaultProfile.FindPath('basal'));
      if BasalArray.Count = 0 then
        Exit;

      CurrentTime := Now;
      CurrentMinutes := HourOf(CurrentTime) * 60 + MinuteOf(CurrentTime);

      for i := 0 to BasalArray.Count - 1 do
      begin
        BasalEntry := BasalArray.Objects[i];
        if not Assigned(BasalEntry) then
          Continue;
        tstr := BasalEntry.Get('time', '00:00');
        h := 0; m := 0;
        if Pos(':', tstr) > 0 then
        begin
          h := StrToIntDef(Copy(tstr, 1, Pos(':', tstr) - 1), 0);
          m := StrToIntDef(Copy(tstr, Pos(':', tstr) + 1, 2), 0);
        end
        else
        begin
          h := StrToIntDef(tstr, 0) div 60;
          m := StrToIntDef(tstr, 0) mod 60;
        end;
        entryMin := h * 60 + m;
        if entryMin <= CurrentMinutes then
          result := BasalEntry.Get('value', single(0));
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
    begin
      lastErr := 'Error fetching basal rate: ' + E.Message;
      result := 0;
    end;
  end;
end;

function NightScout3.getBasalProfile(out profile: TBasalProfile): boolean;
var
  ResponseStr, defName: string;
  JSONData: TJSONData;
  RootObject, StoreObj: TJSONObject;
  StoreArray: TJSONArray;
  DefaultProfile: TJSONObject;
  BasalArray: TJSONArray;
  BasalObj: TJSONObject;
  ResNode, StoreNode: TJSONData;
  i: integer;
  tstr: string;
  h, m: integer;
  be: TBasalEntry;
begin
  Result := False;
  SetLength(profile, 0);
  try
    if not TryRequestV3(NS3_PROFILE, NS3_PROFILE_JSON, [], ResponseStr) then
      ResponseStr := '';
   {$ifdef DEBUG} if DEBUG_LOG_ALERT then TrndiDLog(Format('[%s:%s] / %s'#10'%s'#10'[%s]', [{$i %file%}, {$i %Line%}, NS3_PROFILE, responseStr, debugParams([])]));{$endif}
  except
    lastErr := 'HTTP request failed while fetching profile endpoint';
    Exit;
  end;

  if Trim(ResponseStr) = '' then
  begin
    lastErr := 'Empty response from profile endpoint';
    Exit;
  end;

  try
    JSONData := GetJSON(ResponseStr);
  except
    lastErr := 'Failed to parse JSON from profile endpoint';
    Exit;
  end;

  try
    if not (JSONData is TJSONObject) then
    begin
      lastErr := 'Profile response is not a JSON object';
      Exit;
    end;
    RootObject := TJSONObject(JSONData);
    DefaultProfile := nil;
    // Some Nightscout instances wrap payloads in {"status":..,"result":[{...}]}
   ResNode := RootObject.FindPath('result');
    if Assigned(ResNode) and (ResNode.InheritsFrom(TJSONArray)) and (TJSONArray(ResNode).Count > 0) and (TJSONArray(ResNode).Items[0] is TJSONObject) then
      RootObject := TJSONObject(TJSONArray(ResNode).Items[0]);

    // `store` can be an array (old shape) or an object keyed by profile id (observed)
    StoreNode := RootObject.FindPath('store');
    if Assigned(StoreNode) then
    begin
      if StoreNode.InheritsFrom(TJSONArray) then
      begin
        StoreArray := TJSONArray(StoreNode);
        if StoreArray.Count = 0 then
        begin
          lastErr := 'Empty "store" array in profile.json';
          Exit;
        end;
        DefaultProfile := StoreArray.Objects[0];
      end
      else if StoreNode.InheritsFrom(TJSONObject) then
      begin
        StoreObj := TJSONObject(StoreNode);
        defName := RootObject.Get('defaultProfile', '');
        if defName = '' then
          defName := RootObject.Get('Default', '');
        if defName <> '' then
        begin
          StoreNode := StoreObj.FindPath(defName);
          if Assigned(StoreNode) and (StoreNode is TJSONObject) then
            DefaultProfile := TJSONObject(StoreNode);
        end
        else
        begin
          // fallback: take the first property object found inside store
          if StoreObj.Count > 0 then
            if StoreObj.Items[0] is TJSONObject then
              DefaultProfile := TJSONObject(StoreObj.Items[0]);
        end;
      end;
    end
    else
    begin
      lastErr := 'No "store" element in profile.json';
      Exit;
    end;

    if not Assigned(DefaultProfile) then
    begin
      lastErr := 'No default profile found in profile.json store';
      Exit;
    end;

    BasalArray := DefaultProfile.FindPath('basal') as TJSONArray;
    if not Assigned(BasalArray) then
    begin
      lastErr := 'No "basal" array in default profile';
      Exit;
    end;

    SetLength(profile, BasalArray.Count);
    for i := 0 to BasalArray.Count - 1 do
    begin
      BasalObj := BasalArray.Objects[i];
      tstr := BasalObj.Get('time', '00:00');
      h := 0; m := 0;
      if Pos(':', tstr) > 0 then
      begin
        h := StrToIntDef(Copy(tstr, 1, Pos(':', tstr) - 1), 0);
        m := StrToIntDef(Copy(tstr, Pos(':', tstr) + 1, 2), 0);
      end
      else
      begin
        // If time is numeric (minutes), try parse as integer
        h := StrToIntDef(tstr, 0) div 60;
        m := StrToIntDef(tstr, 0) mod 60;
      end;

      be.startMin := (h * 60) + m;
      be.value := BasalObj.Get('value', single(0));
      be.name := BasalObj.Get('name', '');
      profile[i] := be;
    end;

    Result := True;
  finally
    JSONData.Free;
  end;
end;

end.
