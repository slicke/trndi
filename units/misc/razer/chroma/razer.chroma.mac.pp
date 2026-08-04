(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)
(*
 * Dummy macOS RazerChroma driver for builds where native SDK is unavailable.
 * Provides a no-op TRazerChromaMac class so macOS builds link when Razer code
 * is referenced. This unit is compiled only on Darwin.
 *)
unit razer.chroma.mac;

{$mode objfpc}{$H+}

interface

{$ifdef DARWIN}
uses
  SysUtils, Classes, razer.chroma;

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
