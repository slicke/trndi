unit razer.chroma.factory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, razer.chroma;

type
  TRazerChromaFactory = class
public
  class function CreateInstance: TRazerChromaBase;
  class function GetPlatformName: string;
end;

implementation

class function TRazerChromaFactory.CreateInstance: TRazerChromaBase;
begin
  // Return a mock TRazerChromaBase instance for headless tests
  Result := TRazerChromaBase.Create;
end;

class function TRazerChromaFactory.GetPlatformName: string;
begin
  Result := '';
end;

end.
