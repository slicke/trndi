(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2026 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)
{**
  @unit RazerChromaFactory
  @brief Creates the platform-specific Razer Chroma driver.
  @details
    Selects the Windows Chroma SDK implementation when building on Windows or the
    OpenRazer variant when building on Linux so callers can treat `TRazerChromaBase`
    uniformly.
}
unit RazerChromaFactory;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, RazerChroma;

type
  {** Factory for creating and describing the available Razer Chroma driver. }
  TRazerChromaFactory = class
  public
    {** Instantiate the driver for the current platform. }
    class function CreateInstance: TRazerChromaBase;
    {** Human-friendly name for the loaded driver implementation. }
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
  {$IFDEF DARWIN}
  RazerChromaMac;
  {$ENDIF}
  {$IFDEF HAIKU}
  RazerChromaLinux;
  {$ENDIF}  
  {$IF DEFINED(BSD) AND NOT DEFINED(DARWIN)}
  RazerChromaBSD;
  {$ENDIF} 

class function TRazerChromaFactory.CreateInstance: TRazerChromaBase;
begin
  {$IFDEF LINUX}
  Result := TRazerChromaLinux.Create;
  {$ENDIF}
  {$IFDEF HAIKU}
  Result := TRazerChromaLinux.Create;
  {$ENDIF}  
  {$IFDEF WINDOWS}
  Result := TRazerChromaWindows.Create;
  {$ENDIF}
  {$IFDEF DARWIN}
  result := TRazerChromaMac.Create;
  {$ENDIF}
  {$IF DEFINED(BSD) AND NOT DEFINED(DARWIN)}
  Result := TRazerChromaBSD.Create;
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
  {$IFDEF BSD}
  Result := 'BSD (Not Supported)';
  {$ENDIF}
end;

end.
