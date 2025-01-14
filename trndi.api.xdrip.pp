(*
(c) 2024-2025 github.com/slicke - See LICENSE file, GPLv3, Written with the aid of GPT
*)
unit trndi.api.xdrip;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, strutils, sha1, nsapi, trndi.native, trndi.types, dateutils,
  dialogs;

const
  XDRIP_READINGS = 'sgv.json';
  XDRIP_STATUS = 'pebble'; // This contains the current timestamp of the server

type
  xDrip = class(NightScout)
    public
      constructor create(user, pass, extra: string); override;
      function getReadings(min, maxNum: integer; path: string = ''): BGResults; override;
      function connect: boolean; override;
  end;

implementation

constructor xDrip.create(user, pass, extra: string);
begin
  ua      := 'Mozilla/5.0 (compatible; trndi) TrndiAPI';
  baseUrl := TrimRightSet(user, ['/']) + '/';

  key     := IfThen(pass <> '', 'api-secret=' + SHA1Print(SHA1String(pass)), '');

  timezone := GetLocalTimeOffset;
  native := TrndiNative.create(ua, baseUrl);
end;

function xDrip.getReadings(min, maxNum: integer; path: string = ''): BGResults;
begin
  if path = '' then path := XDRIP_READINGS;

  result := inherited getReadings(min, maxNum, path);
end;

function xDrip.connect: boolean;
var
  y:  string;
  td: tdatetime;
  t: int64;
begin
  if Copy(baseUrl, 1, 4) <> 'http' then begin
     result := false;
     lasterr := 'Invalid address. It must start with http:// or https://!';
     Exit;
  end;
  y := native.request(false, XDRIP_STATUS, [], '', key);

  if pos('uthentication failed', y) > 0 then begin
     lasterr := 'Acess token rejected by xDrip, is it correct?';
     Result := false;
     Exit;
  end;

  y := copy(y, pos('"now":', y) + 6, 13);
  if not TryStrToInt64(y,t) then begin
     lasterr := 'xDrip could not initialize was the clocks were not able tp sync!';
     Result := false;
     Exit;
  end;

  td := UnixToDateTime(t div 1000);
  timeDiff := SecondsBetween(td, LocalTimeToUniversal(now));
  if timeDiff < 0 then
    timeDiff := 0;

  timeDiff := -1 * timeDiff;
  result   := true;
end;

end.

