(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
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
program trndi;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
{$IFNDEF DARWIN}
{$linklib gcc}  // We cant link with the QUickJS lib on Linux otherwise
cthreads,
{$ENDIF}
{$ENDIF}
{$IFDEF HASAMIGA}
athreads,
{$ENDIF}
Interfaces, // this includes the LCL widgetset
Forms,lazcontrols,trndi.types,trndi.native,
{$IFDEF TrndiExt}
trndi.ext.promise, trndi.ext.functions,trndi.ext.ext,
{$ENDIF}
trndi.api,trndi.api.nightscout,
trndi.api.dexcom,umain,uconf, ufloat
{ you can add units after this };

{$R *.res}

begin
RequireDerivedFormResource:=true;
  Application.Scaled:=True;
Application.{%H-}MainFormOnTaskbar:=true;
Application.Initialize;
Application.CreateForm(TfBG,fBG);
  Application.CreateForm(TfFloat,fFloat);
Application.Run;
end.
