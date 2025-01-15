
(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit dexapi;

{$mode ObjFPC}{$H+}

interface

uses 
Classes, SysUtils, Dialogs, trndi.types, trndi.api, trndi.native,
fpjson, jsonparser, dateutils, StrUtils;

const 
  DEXCOM_LOGIN_ENDPOINT = 'General/LoginPublisherAccountByName';

const 
  DEXCOM_AUTHENTICATE_ENDPOINT = 'General/AuthenticatePublisherAccount';

const 
  DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT = 
                                         'Publisher/CheckMonitoredReceiverAssignmentStatus';

const 
  DEXCOM_TIME_ENDPOINT = 'General/SystemUtcTime';

const 
  DEXCOM_GLUCOSE_READINGS_ENDPOINT = 'Publisher/ReadPublisherLatestGlucoseValues';

const 
  DEXCOM_APPLICATION_ID = 'd89443d2-327c-4a6f-89e5-496bbb0317db';

const 
  DEXCOM_BASE_URL_US = 'https://share2.dexcom.com/ShareWebServices/Services';

const 
  DEXCOM_BASE_URL_WORLD = 'https://shareous1.dexcom.com/ShareWebServices/Services';

const 
  DEXCOM_HOST_US = 'share2.dexcom.com';

const 
  DEXCOM_URL_BASE = '/ShareWebServices/Services/';

const 
  DEXCOM_HOST_WORLD = 'shareous1.dexcom.com';

const 
  DEXCOM_BASE_URLS: array [false..true] of string = 
                                                    (DEXCOM_BASE_URL_WORLD, DEXCOM_BASE_URL_US);

const 
  DEXCOM_BASE_HOSTS: array [false..true] of string = 
                                                     (DEXCOM_HOST_WORLD, DEXCOM_HOST_US);

type 
  // Main class
  Dexcom = class(TrndiAPI)
    private 
      // API Host
      baseHost:  string;
      // Username for authentication
      username:  string;
      // Password
      password:  string;
      // Session ID provided by API
      sessionID: string;
      // Dex doesn't do diff so we can calc it...
      doDiff: boolean;
    public 
      constructor create(user, pass, extra: string);
      override;
      constructor create(user, pass, extra: string; diff: boolean);
      function connect: boolean;
      override;
      function getReadings(min, maxNum: integer; extras: string = ''): BGResults;
      override;
    private 
      function checkSession: boolean;
      function checkSN(src: string): boolean;
    published 
      property remote: string read baseUrl;
      property user: string read username;
      property session: string read sessionID;
      property calcDiff: boolean read doDiff;
  end;


implementation
resourcestring
sErrDexPostLogin = 'Login error after verification';
sErrDexPass = 'Dexcom password invalid';
sErrDexLogin = 'Login error, unknown account?';


constructor Dexcom.create(user, pass, extra: string);
begin
  Create(user, pass, extra, true);
end;

constructor Dexcom.create(user, pass, extra: string; diff: boolean);
begin
  // Use Dexcom Follow api
  ua      := 'Dexcom Share/3.0.2.11 CFNetwork/711.2.23 Darwin/14.0.0';
  // Figure out the addresses
  baseUrl := DEXCOM_BASE_URLS[extra = 'usa'];
  baseHost := DEXCOM_BASE_HOSTS[extra = 'usa'];
  // Store access credentials
  username := user;
  password := pass;

  doDiff := diff;

  inherited Create(user, pass, extra);
end;

function Dexcom.connect: boolean;

var 
  json, y, yt: string;
  td: tdatetime;
begin
  // Check that username / pass is OK!
  json := Format('{ "accountName": "%s", "password": "%s", "applicationId": "%s" }',
          [username, password, DEXCOM_APPLICATION_ID]);

  sessionID := StringReplace(native.request(true, DEXCOM_AUTHENTICATE_ENDPOINT,
               [], json), '"', '', [rfReplaceAll]);

  if Pos('AccountPassword', sessionID) > 0 then
    begin
      result  := false;
      lastErr := sErrDexPass + ' (Dex1)';
      exit;
    end;

  if not checkSession then
    begin
      result  := false;
      lastErr := sErrDexLogin  + ' (Dex2)';
      Exit;
    end;

  sessionID := StringReplace(native.request(true, DEXCOM_LOGIN_ENDPOINT, [], json),
               '"', '', [rfReplaceAll]);
  if not checkSession then
    begin
      result  := false;
      lastErr := sErrDexPostLogin  + ' (Dex3)';
      Exit;
    end;

  y := native.request(false, DEXCOM_TIME_ENDPOINT, [], '');

  // Check XML return
  if Pos('>', y) > 0 then
    begin
      yt := ExtractDelimited(5, y, ['>', '<']);
      if yt <> '' then
        td := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', Copy(yt, 1, 19));
    end
  else
    begin
      // With WinInet and possibly other system backends json is returned?
      yt := ExtractDelimited(2, y, ['(', ')']);
      if yt <> '' then
        td := JSToDateTime(strtoint64(yt), false);
    end;

  if yt = '' then
    begin
      lastErr := 'Cannot parse Dexcom time zone data';
      result  := false;
      Exit;
    end;

  timeDiff := SecondsBetween(td, LocalTimeToUniversal(now));
  if timeDiff < 0 then
    timeDiff := 0;

  timeDiff := -1 * timeDiff;
  result   := true;
end;

function Dexcom.checkSession: boolean;
begin
  showmessage(sessionid);
  result := (sessionID <> '') and
            (sessionID <> '00000000-0000-0000-0000-000000000000');

  if (not result) and (sessionID[1] = '+') then;

end;

function Dexcom.checkSN(src: string): boolean;

var 
  params: array[1..2] of string;
  r:      string;

begin
  result := false;

  params[1] := 'sessionId=' + encodeStr(sessionID);
  params[2] := 'serialNumber=' + encodeStr(src);

  r      := native.request(true, DEXCOM_VERIFY_SERIAL_NUMBER_ENDPOINT, params, '');

  result := r = 'AssignedToYou';
end;

function Dexcom.getReadings(min, maxNum: integer; extras: string = ''): BGResults;

function getNumeric(num: string): uint64;

var 
  c:   char;
  str: string;
begin
  str := '';
  for c in num do
    if CharInSet(c, ['0'..'9']) then
      str := str + c;

  if length(str) > 0 then
    result := strToUInt64(str)
  else
    result := 0;
end;

var 
  params: array[1..3] of string;
  vals, trends: string;
  res:    tjsondata;
  i, trendi, trendtry: integer;
begin
  if (min < 1) or (min > 1440) then
    abort;

  if (maxNum < 1) or (maxNum > 288) then
    abort;

  params[1] := 'sessionId=' + sessionID;
  params[2] := 'minutes=' + IntToStr(min);
  params[3] := 'maxCount=' + IntToStr(maxNum);


  vals := native.request(true, DEXCOM_GLUCOSE_READINGS_ENDPOINT, params, '');

  if vals = '' then
    begin
      SetLength(result, 0);
      exit;
    end;

  res := GetJSON(vals);
  SetLength(result, res.count);

  for i := 0 to res.count - 1 do
    try
      result[i].Init(mgdl);

      if (calcDiff) and (i > 0) then
        result[i].update(res.items[i].FindPath('Value').AsFloat, res.items[i].FindPath('Value').
        AsFloat-res.items[i-1].FindPath('Value').AsFloat)
      else if calcDiff then
             result[i].update(res.items[i].FindPath('Value').AsFloat, 0)
      else
        result[i].update(res.items[i].FindPath('Value').AsFloat, BG_NO_VAL);
      // No val as dex doesnt provide delta

      trends := res.items[i].FindPath('Trend').AsString;

      if not TryStrToInt(trends, trendtry) then
        for trendi := low(ord(BGTrend)) to High(ord(BGTrend)) do
          begin
            // := low(BG_TRENDS_STRING) to High(BG_TRENDS_STRING) do begin
            if BG_TRENDS_STRING[BGTrend(trendi)] = trends then
              begin
                result[i].trend := BGTrend(trendi);
                break;
              end;
            result[i].trend := TdPlaceholder;
            // High(BG_TRENDS_STRING) + 1;
          end;
      result[i].date := UnixToDateTime(
                        (getNumeric(res.items[i].FindPath('ST').AsString) div 1000) - tz);

//  result[i].reading.setDiff(BG_NO_VAL, mgdl); // Not supported by Dexcom, I set -1000 to have the GUI calculate this itself
      result[i].level := getLevel(result[i].val);
    except
      on E: exception do
            begin
              result[i].clear;
            end;
end;
end;

end.
