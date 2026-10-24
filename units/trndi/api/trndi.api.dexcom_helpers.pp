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
 * - 2026-08-16: Numeric Dexcom trend codes are read as 1-based, matching the
 *   Share API; they were previously read 0-based, which shifted every arrow by
 *   one step. Credential failures are no longer reported as recoverable
 *   session failures, so a wrong password is not retried, and are described by
 *   the new DexcomAuthFailureMessage. DEXCOM_NULL_UUID moved here from
 *   trndi.api.dexcomNew so both Share drivers share one definition.
 * - 2026-10-16: Added the pieces both Share drivers now share: DexcomExtractId
 *   (reads the quoted-string and the newer {"accountId"} object replies alike),
 *   DexcomIsPasswordRejected and DexcomFindAccountRegion (a rejected password
 *   is also what a wrong region looks like, so the test-connection path probes
 *   the other servers), DexcomDropDuplicateReadings and DexcomComputeDeltas
 *   (G7 Direct-to-Watch uploads the same reading twice, a second apart), and
 *   DexcomClampWindow (the request window is clamped instead of raising).
 *   The credentials message mentions the region.
 *)
unit trndi.api.dexcom_helpers;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, DateUtils, trndi.types, trndi.api.dexcom_time;

const
  {** All-zero GUID. Dexcom returns it in place of an account or session ID when
      the request was understood but produced no identity, so it must be
      rejected rather than passed on to the next call. Shared by both Share
      drivers so the two cannot disagree on what an empty identity looks like. }
DEXCOM_NULL_UUID = '00000000-0000-0000-0000-000000000000';

  {** User-agent string both Share drivers present; the one the Dexcom Share
      iOS app sent, and what every third-party Share client still uses. }
DEXCOM_USER_AGENT = 'Dexcom Share/3.0.2.11 CFNetwork/711.2.23 Darwin/14.0.0';

  {** Largest window ReadPublisherLatestGlucoseValues accepts, in minutes. }
DEXCOM_MAX_MINUTES = 1440;
  {** Most readings ReadPublisherLatestGlucoseValues returns per call. }
DEXCOM_MAX_COUNT = 288;

  {** Two uploads of one sensor reading (phone and watch both publishing) land
      this close together; anything nearer than this is the same reading. }
DEXCOM_DUPLICATE_WINDOW_SECONDS = 10;

type
  {** One Dexcom Share region: where its accounts live and which application
      id that server accepts. }
TDexcomRegionInfo = record
  Name: string;     /// Display name, matching the backend list ("USA" etc.)
  BaseUrl: string;  /// Share service root, no trailing slash
  AppId: string;    /// Application id that server accepts
end;

const
  {** The three Share regions. Japan has its own application id; the US and
      worldwide servers share one. }
DEXCOM_REGIONS: array[0..2] of TDexcomRegionInfo = (
  (Name: 'Outside USA';
   BaseUrl: 'https://shareous1.dexcom.com/ShareWebServices/Services';
   AppId: 'd89443d2-327c-4a6f-89e5-496bbb0317db'),
  (Name: 'USA';
   BaseUrl: 'https://share2.dexcom.com/ShareWebServices/Services';
   AppId: 'd89443d2-327c-4a6f-89e5-496bbb0317db'),
  (Name: 'Japan';
   BaseUrl: 'https://share.dexcom.jp/ShareWebServices/Services';
   AppId: 'd8665ade-9673-4e27-9ff6-92db4ce13d13'));

{** Escape a string for safe inclusion in a JSON value. Worst-case size is 2x
    the input (every char escaped); never under-allocates. }
function JSONEscape(const S: string): string;

{** Map a Dexcom trend representation (string or numeric) into the internal
    `BGTrend` enum. Prefers textual mapping via `BG_TRENDS_STRING`. If the value
    is numeric, it is read as a Dexcom Share trend code, which is 1-based
    against `BGTrend`'s 0-based ordinals. As a final fallback, recognizes the
    Dexcom Share API's CamelCase textual trend names and converts them to the
    corresponding enum. }
function MapDexcomTrendToEnum(const S: string): BGTrend;

{** Date a reading from Dexcom's three candidate timestamp fields, trying
    @code(AWT), then @code(ADT), then @code(AST).

    Candidates are chosen by whether they *parse*, not merely by being
    non-empty: a present-but-malformed WT must not shadow a DT or ST that does
    parse, which would leave the reading dated 1899 -- the outcome the fallback
    chain exists to avoid.

    WT leads because it is a bare epoch in milliseconds with no offset suffix to
    interpret. DT is next, being the field pydexcom reads. ST is the receiver's
    own system clock, which drifts, so it is the last resort.

    @param(ADate Receives the parsed timestamp, or 0 when none of the three
      parses.)
    @returns(True when one of the candidates yielded a timestamp.) }
function DexcomReadingTime(const AWT, ADT, AST: string;
  out ADate: TDateTime): boolean;

{** Heuristic: does a Dexcom Share response body indicate a dead/rejected
    session (so the caller should re-authenticate)? Matches both prose
    ("Session ID not found") and the CamelCase error codes Dexcom actually
    sends ("SessionIdNotFound", "SessionNotValid") by comparing with spaces
    stripped.

    Deliberately excludes credential failures: see the implementation note. }
function DexcomLooksLikeSessionFailure(const Response: string): boolean;

{** Recognize the Dexcom Share authentication failures that deserve a clearer
    message than the raw payload, and report it in @code(AMessage).

    Covers the codes pydexcom's `_handle_error_code` singles out:
    `AccountPasswordInvalid`, `SSO_AuthenticateMaxAttemptsExceeded`, and
    `SSO_InternalError` carrying "Cannot Authenticate by AccountName/AccountId"
    -- the last being how Dexcom reports a rejected login rather than via a
    dedicated code.

    @returns(True when the response is a recognized auth failure.) }
function DexcomAuthFailureMessage(const Response: string;
  out AMessage: string): boolean;

{** Pull the account or session id out of a Share login reply.

    Dexcom has answered AuthenticatePublisherAccount and the two login calls
    with a JSON-quoted GUID for years, and still does for most accounts. Since
    mid-2026 newer (G7-era) accounts get an object with an accountId member
    from the authenticate call instead. Both forms are read here; an error object
    (one carrying Code, Message or error), an empty body, or anything that
    does not look like a single token yields ''.

    @returns(The bare id, or '' when the reply holds none.) }
function DexcomExtractId(const Response: string): string;

{** True when the reply is Dexcom's AccountPasswordInvalid. That is also the
    answer for an account that lives on another region's server, so a caller
    that can afford it should offer the other regions (see
    @link(DexcomFindAccountRegion)) before telling the user the password is
    wrong. }
function DexcomIsPasswordRejected(const Response: string): boolean;

{** Try the credentials against every Share region except the one at
    @code(ASkipBaseUrl), and name the first one that issues a session.

    Intended for the test-connection path only, after a password rejection:
    each probe is a sign-in attempt that Dexcom counts, so it is not something
    to do on every connect.

    @param(ARegionName Receives the matching region's display name.)
    @returns(True when another region accepted the credentials.) }
function DexcomFindAccountRegion(const AUser, APass, ASkipBaseUrl: string;
  out ARegionName: string): boolean;

{** Drop readings that duplicate the one kept just before them. @code(R) is
    newest-first as Share returns it; a reading whose timestamp is within
    @code(AWindowSeconds) of the previous kept reading is the same sensor
    sample uploaded twice (phone and watch), and the newer upload is kept.
    Undated readings are never treated as duplicates. }
procedure DexcomDropDuplicateReadings(var R: BGResults;
  AWindowSeconds: integer = DEXCOM_DUPLICATE_WINDOW_SECONDS);

{** Fill in each reading's delta from the reading after it (older, since the
    list is newest-first). A reading whose neighbour is empty keeps the delta
    unset, so the UI falls back to its own computation instead of showing 0. }
procedure DexcomComputeDeltas(var R: BGResults);

{** Clamp a request window to what Share accepts (1..1440 minutes, 1..288
    readings). The window comes from user-editable settings, so an oversized
    one is shrunk rather than refused. }
procedure DexcomClampWindow(var AMinutes, AMaxCount: integer);

{** The message for an account that @link(DexcomFindAccountRegion) found on
    another region's server, naming that region. }
function DexcomWrongRegionMessage(const ARegionName: string): string;

implementation

uses
  fpjson, jsonparser, trndi.native;

resourcestring
sDexErrCredentials = 'Incorrect username or password combination, or the ' +
  'account belongs to another Dexcom region';
sDexErrMaxAttempts = 'Too many failed Dexcom sign-in attempts. The account is ' +
  'temporarily locked - wait before trying again, as further attempts keep it locked.';
sDexErrWrongRegion = 'The password was accepted by the "%s" Dexcom region. ' +
  'Choose the Dexcom backend for that region instead.';

{------------------------------------------------------------------------------
  DexcomNormalize
  --------------------
  Fold a response body into the form the substring matchers below expect:
  spaces stripped, lowercased. Dexcom sends the same condition as prose
  ("Session ID not found") and as a CamelCase code ("SessionIdNotFound"), and
  this reduces both to one needle.
 ------------------------------------------------------------------------------}
function DexcomNormalize(const Response: string): string;
begin
  Result := LowerCase(StringReplace(Response, ' ', '', [rfReplaceAll]));
end;

function JSONEscape(const S: string): string;
var
  i, idx: integer;
  c: char;
begin
  SetLength(Result, Length(S) * 2);
  idx := 1;
  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
    '"':
      begin Result[idx] := '\'; Inc(idx); Result[idx] := '"'; Inc(idx); end;
    '\':
      begin Result[idx] := '\'; Inc(idx); Result[idx] := '\'; Inc(idx); end;
    #8:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'b'; Inc(idx); end;
    #9:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 't'; Inc(idx); end;
    #10:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'n'; Inc(idx); end;
    #12:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'f'; Inc(idx); end;
    #13:
      begin Result[idx] := '\'; Inc(idx); Result[idx] := 'r'; Inc(idx); end;
    else
      begin Result[idx] := c; Inc(idx); end;
    end;
  end;
  SetLength(Result, idx - 1);
end;

function MapDexcomTrendToEnum(const S: string): BGTrend;
const
  // Dexcom Share API CamelCase textual trend names (alias of BG_TRENDS_STRING,
  // which holds the uppercased/spaced variant). Indexed by BGTrend ordinal (0..7).
  // 'RateOutOfRange' is handled as an alias of 'NotComputable' below.
  DEXCOM_TREND_NAMES: array[0..7] of string = (
    'DoubleUp', 'SingleUp', 'FortyFiveUp', 'Flat',
    'FortyFiveDown', 'SingleDown', 'DoubleDown', 'NotComputable'
  );
var
  code, idx: integer;
  L: string;
begin
  L := Trim(S);

  // 1) Canonical textual mapping (BG_TRENDS_STRING)
  for Result := Low(BGTrend) to High(BGTrend) do
    if BG_TRENDS_STRING[Result] = L then
      Exit;

  // 2) Numeric: Dexcom Share's trend codes run
  //      0=None 1=DoubleUp 2=SingleUp 3=FortyFiveUp 4=Flat
  //      5=FortyFiveDown 6=SingleDown 7=DoubleDown 8=NotComputable
  //      9=RateOutOfRange
  //    which is BGTrend's own order shifted by one, so the code maps to
  //    BGTrend(code - 1). Confirmed against pydexcom's DEXCOM_TREND_DIRECTIONS.
  //
  //    This used to try a 0-based reading first, which silently shifted every
  //    arrow by one step -- a Dexcom 4 ("Flat") came out as TdFortyFiveDown.
  //    Only the tail of the range happened to land correctly, via the 1-based
  //    fallback that fired once the 0-based read went out of bounds.
  //
  //    Current Dexcom Share sends the textual names instead (branches 1 and 3);
  //    pydexcom dropped integer support outright on that basis. This path is
  //    therefore for older payloads only, which is why the skew went unnoticed.
  if TryStrToInt(L, code) then
  begin
    if (code >= 1) and (code <= Ord(TdNotComputable) + 1) then
      Result := BGTrend(code - 1)
    else if code = 9 then
      // RateOutOfRange. No distinct enum member, so it follows the textual
      // alias in branch 3 rather than degrading to a placeholder.
      Result := TdNotComputable
    else
      // 0 is Dexcom's "None", i.e. no trend rather than a specific arrow.
      Result := TdPlaceholder;
    Exit;
  end;

  // 3) Dexcom CamelCase textual trend names
  idx := -1;
  if L = 'RateOutOfRange' then
    idx := 7
  else
    for code := 0 to High(DEXCOM_TREND_NAMES) do
      if DEXCOM_TREND_NAMES[code] = L then
      begin
        idx := code;
        Break;
      end;

  if (idx >= Ord(Low(BGTrend))) and (idx <= Ord(High(BGTrend))) then
    Result := BGTrend(idx)
  else
    Result := TdPlaceholder;
end;

{------------------------------------------------------------------------------
  DexcomReadingTime
  --------------------
  Dexcom sends three timestamps per reading and they are not interchangeable.
  ST is the receiver's own system clock, which drifts, and is the field
  third-party Share clients trust least: pydexcom ignores it entirely, and
  others rank it below WT. Both Trndi drivers used to read it first, which was
  the odd choice out.

  WT leads because it is unambiguous: a bare epoch in milliseconds, with no
  offset suffix to interpret. DT arrives as "Date(<ms>+0000)", and whether those
  milliseconds are a true epoch or already shifted by the offset decides whether
  a reading lands on the right minute or hours away. pydexcom treats them as a
  true epoch, and DT is its only source, which is good evidence -- but WT needs
  no such judgement call, so it goes first and DT backs it up.

  ST stays as the last resort: a drifting clock still beats no timestamp.
 ------------------------------------------------------------------------------}
function DexcomReadingTime(const AWT, ADT, AST: string;
  out ADate: TDateTime): boolean;
begin
  // Each candidate is offered to the parser rather than merely tested for
  // content, so a field that is present but unparseable steps aside for the
  // next one instead of dating the reading 0.
  if (AWT <> '') and ParseDexcomTime(AWT, ADate) then
    Exit(true);
  if (ADT <> '') and ParseDexcomTime(ADT, ADate) then
    Exit(true);
  if (AST <> '') and ParseDexcomTime(AST, ADate) then
    Exit(true);
  // ParseDexcomTime zeroes its out parameter on entry, but a caller reading
  // ADate after a False result should not depend on that.
  ADate := 0;
  Result := false;
end;

function DexcomLooksLikeSessionFailure(const Response: string): boolean;
var
  L: string;
begin
  // Strip spaces before lowercasing so "Session ID not found" and
  // "SessionIdNotFound" both reduce to the same needle. Glucose payloads
  // never contain the word "session", so this cannot misfire on real data.
  L := DexcomNormalize(Response);

  // "AccountPasswordInvalid" used to be matched here too, which made a wrong
  // password look like an expired session: the caller re-authenticated and
  // resubmitted the same bad credentials, so every poll cost two failed
  // sign-ins instead of one. Dexcom counts those and answers with
  // SSO_AuthenticateMaxAttemptsExceeded, so the retry was driving the account
  // toward a lockout. Credential failures are terminal -- pydexcom likewise
  // retries on SessionError only, never on AccountError -- and are reported
  // through DexcomAuthFailureMessage instead.
  Result :=
    ((Pos('session', L) > 0) and
    ((Pos('invalid', L) > 0) or (Pos('expired', L) > 0) or
    (Pos('notvalid', L) > 0) or (Pos('notfound', L) > 0) or
    (Pos('sessionidnull', L) > 0))) or
    (Pos('unauthorized', L) > 0) or
    (Pos('forbidden', L) > 0);
end;

function DexcomAuthFailureMessage(const Response: string;
  out AMessage: string): boolean;
var
  L: string;
begin
  AMessage := '';
  Result := false;
  L := DexcomNormalize(Response);
  if L = '' then
    Exit;

  if Pos('sso_authenticatemaxattemptsexceeded', L) > 0 then
    AMessage := sDexErrMaxAttempts
  else if Pos('accountpasswordinvalid', L) > 0 then
    AMessage := sDexErrCredentials
  // The SSO route's answer to an email or phone number it does not know;
  // as terminal as a wrong password, and worded the same for the user
  else if Pos('sso_authenticateaccountnotfound', L) > 0 then
    AMessage := sDexErrCredentials
  else if (Pos('sso_internalerror', L) > 0) and
    ((Pos('cannotauthenticatebyaccountname', L) > 0) or
    (Pos('cannotauthenticatebyaccountid', L) > 0)) then
    AMessage := sDexErrCredentials
  else
    Exit;

  Result := true;
end;

{------------------------------------------------------------------------------
  DexcomExtractId
  --------------------
  Three reply shapes carry an id: a JSON string ("guid"), an object with an
  accountId/sessionId member, and -- from the old plain-text path -- a bare
  token. Error objects and transport-error text yield '' so the caller can
  tell "no id" from "an id", which the old quote-stripping could not: it
  stripped the quotes off the accountId object and sent the rest as the id.
 ------------------------------------------------------------------------------}
function DexcomExtractId(const Response: string): string;
const
  IdFields: array[0..3] of string = ('accountId', 'sessionId', 'AccountId', 'SessionId');
var
  js: TJSONData;
  s: string;
  i: integer;
begin
  Result := '';
  s := Trim(Response);
  if s = '' then
    Exit;

  if (s[1] = '{') or (s[1] = '"') or (s[1] = '[') then
  begin
    try
      js := GetJSON(s);
    except
      Exit;
    end;
    try
      case js.JSONType of
      jtString:
        s := js.AsString;
      jtObject:
      begin
        if (TJSONObject(js).Find('Code') <> nil) or
          (TJSONObject(js).Find('Message') <> nil) or
          (TJSONObject(js).Find('error') <> nil) or
          (TJSONObject(js).Find('Error') <> nil) then
          Exit;
        s := '';
        for i := Low(IdFields) to High(IdFields) do
        begin
          s := TJSONObject(js).Get(IdFields[i], '');
          if s <> '' then
            Break;
        end;
      end;
      else
        Exit;
      end;
    finally
      js.Free;
    end;
  end;

  s := Trim(s);
  // A token has no whitespace or markup; transport errors ("+Connection
  // refused") and HTML error pages do.
  if (s = '') or (s[1] = '+') or (s[1] = '<') or (Pos(' ', s) > 0) or
    (Pos('error', LowerCase(s)) > 0) or (Pos('invalid', LowerCase(s)) > 0) then
    Exit;
  Result := s;
end;

function DexcomIsPasswordRejected(const Response: string): boolean;
begin
  Result := Pos('accountpasswordinvalid', DexcomNormalize(Response)) > 0;
end;

{------------------------------------------------------------------------------
  DexcomFindAccountRegion
  --------------------
  A password rejection from the wrong server is indistinguishable from a wrong
  password, and picking the wrong region is the most common Share setup
  mistake. xdripswift answers it by trying every region; this does the same,
  but only from the test-connection path and only after a rejection.
 ------------------------------------------------------------------------------}
function DexcomFindAccountRegion(const AUser, APass, ASkipBaseUrl: string;
  out ARegionName: string): boolean;
var
  i: integer;
  tn: TrndiNative;
  body, id: string;
  skip: string;
begin
  Result := false;
  ARegionName := '';
  skip := Trim(ASkipBaseUrl);
  while (skip <> '') and (skip[Length(skip)] = '/') do
    Delete(skip, Length(skip), 1);
  for i := Low(DEXCOM_REGIONS) to High(DEXCOM_REGIONS) do
  begin
    if SameText(DEXCOM_REGIONS[i].BaseUrl, skip) then
      Continue;
    tn := TrndiNative.Create(DEXCOM_USER_AGENT, DEXCOM_REGIONS[i].BaseUrl);
    try
      body := Format('{"accountName":"%s","password":"%s","applicationId":"%s"}',
        [JSONEscape(AUser), JSONEscape(APass), DEXCOM_REGIONS[i].AppId]);
      id := DexcomExtractId(tn.Request(true, 'General/AuthenticatePublisherAccount', [], body));
      if (id = '') or (id = DEXCOM_NULL_UUID) then
        Continue;
      body := Format('{"accountId":"%s","password":"%s","applicationId":"%s"}',
        [id, JSONEscape(APass), DEXCOM_REGIONS[i].AppId]);
      id := DexcomExtractId(tn.Request(true, 'General/LoginPublisherAccountById', [], body));
      if (id <> '') and (id <> DEXCOM_NULL_UUID) then
      begin
        ARegionName := DEXCOM_REGIONS[i].Name;
        Exit(true);
      end;
    finally
      tn.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  DexcomDropDuplicateReadings
  --------------------
  G7 Direct-to-Watch: the watch and the phone both publish the same sample,
  about a second apart, and Share returns both (pydexcom #95 leaves the
  dedupe to the consumer; xDrip+ uses a 10 s window). Without this the
  newest reading's delta is 0 and the graph gets a doubled dot.
 ------------------------------------------------------------------------------}
procedure DexcomDropDuplicateReadings(var R: BGResults; AWindowSeconds: integer);
var
  i, kept: integer;
begin
  if Length(R) < 2 then
    Exit;
  kept := 1;
  for i := 1 to High(R) do
  begin
    if (R[i].date <> 0) and (R[kept - 1].date <> 0) and
      (Abs(SecondsBetween(R[i].date, R[kept - 1].date)) < AWindowSeconds) then
      Continue;
    if kept <> i then
      R[kept] := R[i];
    Inc(kept);
  end;
  SetLength(R, kept);
end;

procedure DexcomComputeDeltas(var R: BGResults);
var
  i: integer;
begin
  for i := 0 to High(R) - 1 do
    if (not R[i].empty) and (not R[i + 1].empty) then
      R[i].update(R[i].val - R[i + 1].val, BGDelta);
end;

function DexcomWrongRegionMessage(const ARegionName: string): string;
begin
  Result := Format(sDexErrWrongRegion, [ARegionName]);
end;

procedure DexcomClampWindow(var AMinutes, AMaxCount: integer);
begin
  if AMinutes < 1 then
    AMinutes := 1
  else if AMinutes > DEXCOM_MAX_MINUTES then
    AMinutes := DEXCOM_MAX_MINUTES;
  if AMaxCount < 1 then
    AMaxCount := 1
  else if AMaxCount > DEXCOM_MAX_COUNT then
    AMaxCount := DEXCOM_MAX_COUNT;
end;

end.
