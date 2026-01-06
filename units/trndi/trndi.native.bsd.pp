unit trndi.native.bsd;

{**
  @abstract(BSD-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeBSD) which is simply an alias to
  @link(TTrndiNativeLinux) since BSD systems use the same tooling and
  conventions as Linux (notify-send, spd-say, INI files, etc.).

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
trndi.native.linux;

type
  {!
    @abstract(BSD implementation - alias to Linux implementation.)
    BSD systems use the same tools and conventions as Linux, so we simply
    inherit from TTrndiNativeLinux.
  }
TTrndiNativeBSD = trndi.native.linux.TTrndiNativeLinux;

implementation

end.
