(*
 * Dummy BSD RazerChroma driver for builds where native SDK is unavailable.
 * Provides a no-op TRazerChromaBSD class so BSD builds link when Razer code
 * is referenced. This unit is compiled only on BSD (FreeBSD, OpenBSD, NetBSD).
 *)
unit RazerChromaBSD;

{$mode objfpc}{$H+}

interface

{$ifdef BSD}
uses
  SysUtils, Classes, RazerChroma;

type
  {** No-op BSD implementation of TRazerChromaBase. }
  TRazerChromaBSD = class(TRazerChromaBase)
  protected
    function DoInitialize: Boolean; override;
    procedure DoFinalize; override;
    procedure DoRefreshDevices; override;

    function DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean; override;
    function DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean; override;
    function DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean; override;
    function DoSetBreathRandom(const ADevice: TRazerDevice): Boolean; override;
    function DoSetSpectrum(const ADevice: TRazerDevice): Boolean; override;
    function DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean; override;
    function DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean; override;
    function DoSetNone(const ADevice: TRazerDevice): Boolean; override;
    function DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean; override;
    function DoGetBrightness(const ADevice: TRazerDevice): Byte; override;
  public
    constructor Create; override;
  end;

{$endif}

implementation

{$ifdef BSD}

{ TRazerChromaBSD }

constructor TRazerChromaBSD.Create;
begin
  inherited Create;
end;

function TRazerChromaBSD.DoInitialize: Boolean;
begin
  // No native SDK available in this dummy implementation.
  Result := False;
  FLastError := 'Razer BSD driver not implemented (dummy).';
end;

procedure TRazerChromaBSD.DoFinalize;
begin
  // Nothing to clean up in dummy
end;

procedure TRazerChromaBSD.DoRefreshDevices;
begin
  // No devices discovered
  FDevices.Clear;
end;

function TRazerChromaBSD.DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
begin
  Result := False;
  FLastError := 'Not implemented on BSD dummy driver.';
end;

function TRazerChromaBSD.DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoSetBreathRandom(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoSetSpectrum(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoSetNone(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean;
begin
  Result := False;
end;

function TRazerChromaBSD.DoGetBrightness(const ADevice: TRazerDevice): Byte;
begin
  // Return a safe default brightness
  Result := 100;
end;

{$endif}

end.
