unit trndi.native.bsd;

{**
  @abstract(BSD-specific native features for Trndi.)

  This unit defines @link(TTrndiNativeBSD) which derives from
  @link(TTrndiNativeLinux) but overrides HTTP functionality to use
  TFPHTTPClient instead of libcurl for better BSD compatibility.

  Prefer using the façade unit @code(trndi.native) which selects the platform
  class alias automatically.
}

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, trndi.native.linux, fphttpclient, opensslsockets;

type
  {!
    @abstract(BSD implementation - inherits Linux with TFPHTTPClient for HTTP.)
    BSD systems use the same tools as Linux (notify-send, spd-say, INI files),
    but use TFPHTTPClient for HTTP requests instead of libcurl.
  }
TTrndiNativeBSD = class(TTrndiNativeLinux)
public
  {** HTTP GET using TFPHTTPClient instead of libcurl.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
  class function getURL(const url: string; out res: string): boolean; override;
end;

implementation

{------------------------------------------------------------------------------
  getURL
  ------
  HTTP GET using TFPHTTPClient for BSD compatibility.
 ------------------------------------------------------------------------------}
class function TTrndiNativeBSD.getURL(const url: string; out res: string): boolean;
var
  HTTP: TFPHTTPClient;
begin
  Result := false;
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.AllowRedirect := true;
    try
      res := HTTP.Get(url);
      Result := true;
    except
      on E: Exception do
      begin
        res := 'Error: ' + E.Message;
        Result := false;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

end.
