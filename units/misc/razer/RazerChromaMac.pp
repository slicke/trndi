(*
 * Dummy macOS RazerChroma driver for builds where native SDK is unavailable.
 * Provides a no-op TRazerChromaMac class so macOS builds link when Razer code
 * is referenced. This unit is compiled only on Darwin.
 *)
unit RazerChromaMac;

{$mode objfpc}{$H+}

interface

{$ifdef DARWIN}
uses
  SysUtils, Classes, RazerChroma;

type
  {** No-op macOS implementation of TRazerChromaBase. }
  TRazerChromaMac = class(TRazerChromaBase)
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

{$ifdef DARWIN}

{ TRazerChromaMac }

constructor TRazerChromaMac.Create;
begin
  inherited Create;
end;

function TRazerChromaMac.DoInitialize: Boolean;
begin
  // No native SDK available in this dummy implementation.
  Result := False;
  FLastError := 'Razer macOS driver not implemented (dummy).';
end;

procedure TRazerChromaMac.DoFinalize;
begin
  // Nothing to clean up in dummy
end;

procedure TRazerChromaMac.DoRefreshDevices;
begin
  // No devices discovered
  FDevices.Clear;
end;

function TRazerChromaMac.DoSetStatic(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
begin
  Result := False;
  FLastError := 'Not implemented on macOS dummy driver.';
end;

function TRazerChromaMac.DoSetBreathSingle(const ADevice: TRazerDevice; const AColor: TRGBColor): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoSetBreathDual(const ADevice: TRazerDevice; const AColor1, AColor2: TRGBColor): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoSetBreathRandom(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoSetSpectrum(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoSetReactive(const ADevice: TRazerDevice; const AColor: TRGBColor; ASpeed: TRazerEffectSpeed): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoSetWave(const ADevice: TRazerDevice; ADirection: Integer): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoSetNone(const ADevice: TRazerDevice): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoSetBrightness(const ADevice: TRazerDevice; ABrightness: Byte): Boolean;
begin
  Result := False;
end;

function TRazerChromaMac.DoGetBrightness(const ADevice: TRazerDevice): Byte;
begin
  // Return a safe default brightness
  Result := 100;
end;

{$endif}

end.
