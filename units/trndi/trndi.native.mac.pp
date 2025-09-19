unit trndi.native.mac;

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Graphics, NSMisc, ns_url_request, CocoaAll, SimpleDarkMode,
  trndi.native.base;

type
  TTrndiNativeMac = class(TTrndiNativeBase)
  public
    procedure Speak(const Text: string); override;
    class function setDarkMode: boolean;
  end;

implementation

uses
  Process;

procedure TTrndiNativeMac.Speak(const Text: string);
var
  o: string;
begin
  RunCommand('/usr/bin/say', [Text], o);
end;

class function TTrndiNativeMac.setDarkMode: Boolean;
begin
  SimpleDarkMode.EnableAppDarkMode;
end;

end.
