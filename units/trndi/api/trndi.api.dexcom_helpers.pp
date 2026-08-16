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
 *   the new DexcomAuthFailureMessage.
 *)
unit trndi.api.dexcom_helpers;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, trndi.types, trndi.api.dexcom_time;

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

implementation

resourcestring
sDexErrCredentials = 'Incorrect username or password combination';
sDexErrMaxAttempts = 'Too many failed Dexcom sign-in attempts. The account is ' +
  'temporarily locked - wait before trying again, as further attempts keep it locked.';

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
  else if (Pos('sso_internalerror', L) > 0) and
    ((Pos('cannotauthenticatebyaccountname', L) > 0) or
    (Pos('cannotauthenticatebyaccountid', L) > 0)) then
    AMessage := sDexErrCredentials
  else
    Exit;

  Result := true;
end;

end.
