unit trndi.native.mac;

{**
  @abstract(macOS-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeMac) which derives from
  @link(TTrndiNativeBase) and implements:
  - Text-to-speech via the built-in @code(say) command
  - Enabling dark appearance via @code(SimpleDarkMode)

  Use the façade unit @code(trndi.native) which provides the platform alias.
}

{$I ../../inc/native.inc}

interface

uses
  Classes, SysUtils, Graphics, NSMisc, ns_url_request, CocoaAll, SimpleDarkMode,
  trndi.native.base;

type
  {!
    @abstract(macOS implementation of @link(TTrndiNativeBase).)
    Relies on system tools for speech and dark appearance toggling.
  }
  TTrndiNativeMac = class(TTrndiNativeBase)
  public
    {** Speaks @param(Text) using the system @code(say) command. }
    procedure Speak(const Text: string); override;
    {** Enables dark appearance for the application via SimpleDarkMode. }
    class function setDarkMode: boolean;
  end;

implementation

uses
  Process;

procedure TTrndiNativeMac.Speak(const Text: string);
var
  o: string;
begin
  // Use the built-in macOS speech synthesis
  RunCommand('/usr/bin/say', [Text], o);
end;

class function TTrndiNativeMac.setDarkMode: Boolean;
begin
  // Enable dark appearance for the app's UI via SimpleDarkMode
  SimpleDarkMode.EnableAppDarkMode;
end;

end.
