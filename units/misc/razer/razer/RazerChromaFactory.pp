unit RazerChromaFactory;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, RazerChroma;

type
  { TRazerChromaFactory }
  TRazerChromaFactory = class
  public
    class function CreateInstance: TRazerChromaBase;
    class function GetPlatformName: string;
  end;

implementation

uses
  {$IFDEF LINUX}
  RazerChromaLinux;
  {$ENDIF}
  {$IFDEF WINDOWS}
  RazerChromaWindows;
  {$ENDIF}

class function TRazerChromaFactory.CreateInstance: TRazerChromaBase;
begin
  {$IFDEF LINUX}
  Result := TRazerChromaLinux.Create;
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := TRazerChromaWindows.Create;
  {$ENDIF}
  {$IFDEF DARWIN}
  raise Exception.Create('macOS is not supported');
  {$ENDIF}
end;

class function TRazerChromaFactory.GetPlatformName: string;
begin
  {$IFDEF LINUX}
  Result := 'Linux (OpenRazer)';
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := 'Windows (Chroma SDK)';
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := 'macOS (Not Supported)';
  {$ENDIF}
end;

end.