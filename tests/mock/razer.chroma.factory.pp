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
  // Headless test stub - return nil
  Result := nil;
end;

class function TRazerChromaFactory.GetPlatformName: string;
begin
  Result := '';
end;

end.
