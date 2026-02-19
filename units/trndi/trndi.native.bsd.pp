unit trndi.native.bsd;

{**
  @abstract(BSD-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeBSD) which is simply an alias to
  @link(TTrndiNativeLinux) since BSD systems use the same tooling and
  conventions as Linux (notify-send, spd-say, INI files, libcurl, etc.).

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
trndi.native.linux;

type
  {!
    @abstract(BSD implementation - subclass of Linux implementation.)
    BSD systems use the same tools and conventions as Linux. Declaring a
    distinct subclass allows BSD-specific overrides in future without
    changing callers.
  }
TTrndiNativeBSD = class(trndi.native.linux.TTrndiNativeLinux)
  public
    {** TTS: allow BSD to alter detection/behavior later without changing callers. }
    class function SpeakAvailable: boolean; override;
    class function SpeakSoftwareName: string; override;

    {** Notifications: same rationale — delegate today, override later if needed. }
    class function isNotificationSystemAvailable: boolean; override;
    class function getNotificationSystem: string; override;

    // BSD-specific overrides can be added here later.
  end;

implementation

{------------------------------------------------------------------------------
  BSD: simple delegation stubs. They call the Linux implementation today so
  behavior stays identical; override them later to implement BSD-specific
  detection/fallbacks (espeak, flite, different notify-send locations, etc.).
------------------------------------------------------------------------------}
class function TTrndiNativeBSD.SpeakAvailable: boolean;
begin
  Result := inherited SpeakAvailable;
end;

class function TTrndiNativeBSD.SpeakSoftwareName: string;
begin
  Result := inherited SpeakSoftwareName;
end;

class function TTrndiNativeBSD.isNotificationSystemAvailable: boolean;
begin
  Result := inherited isNotificationSystemAvailable;
end;

class function TTrndiNativeBSD.getNotificationSystem: string;
begin
  Result := inherited getNotificationSystem;
end;

end.
