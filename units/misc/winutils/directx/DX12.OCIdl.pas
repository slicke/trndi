{$REGION 'Copyright (C) CMC Development Team'}
{ **************************************************************************
  Copyright (C) 2015 CMC Development Team

  CMC is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.

  CMC is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with CMC. If not, see <http://www.gnu.org/licenses/>.
  ************************************************************************** }

{ **************************************************************************
  Additional Copyright (C) for this modul:

  Copyright (c) Microsoft Corporation.  All rights reserved.

  This unit is a Pascal translation of Windows SDK header files. Trndi uses it
  under the "or any later version" option above, as part of a GPLv3 work.
  ************************************************************************** }
{$ENDREGION}
unit DX12.OCIdl;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

{$Z4}

uses
    Windows, Classes, SysUtils, ActiveX;

const
    MULTICLASSINFO_GETTYPEINFO = $00000001;
    MULTICLASSINFO_GETNUMRESERVEDDISPIDS = $00000002;
    MULTICLASSINFO_GETIIDPRIMARY = $00000004;
    MULTICLASSINFO_GETIIDSOURCE = $00000008;
    TIFLAGS_EXTENDDISPATCHONLY = $00000001;

const
    IID_IEnumConnections: TGUID = '{B196B287-BAB4-101A-B69C-00AA00341D07}';
    IID_IConnectionPoint: TGUID = '{B196B286-BAB4-101A-B69C-00AA00341D07}';
    IID_IEnumConnectionPoints: TGUID = '{B196B285-BAB4-101A-B69C-00AA00341D07}';
    IID_IConnectionPointContainer: TGUID = '{B196B284-BAB4-101A-B69C-00AA00341D07}';
    IID_IClassFactory2: TGUID = '{B196B28F-BAB4-101A-B69C-00AA00341D07}';


    IID_IPropertyBag2: TGUID = '{22F55882-280B-11d0-A8A9-00A0C90C2004}';

type

    {$IFNDEF FPC}
    LONG = longint;
    SIZE_T = ULONG_PTR;
    HMONITOR = THANDLE;
    {$ENDIF}

    TtagUASFLAGS = (
        UAS_NORMAL = 0,
        UAS_BLOCKED = $1,
        UAS_NOPARENTENABLE = $2,
        UAS_MASK = $3
        );


    TtagREADYSTATE = (
        READYSTATE_UNINITIALIZED = 0,
        READYSTATE_LOADING = 1,
        READYSTATE_LOADED = 2,
        READYSTATE_INTERACTIVE = 3,
        READYSTATE_COMPLETE = 4
        );

    TtagCONNECTDATA = record
        pUnk: IUnknown;
        dwCookie: DWORD;
    end;
    PtagCONNECTDATA = ^TtagCONNECTDATA;

    TtagLICINFO = record
        cbLicInfo: LONG;
        fRuntimeKeyAvail: longbool;
        fLicVerified: longbool;
    end;

    PtagLICINFO = ^TtagLICINFO;

    TtagGUIDKIND = (
        GUIDKIND_DEFAULT_SOURCE_DISP_IID = 1
        );

    TtagCONTROLINFO = record
        cb: ULONG;
        hAccel: HACCEL;
        cAccel: USHORT;
        dwFlags: DWORD;
    end;
    PtagCONTROLINFO = ^TtagCONTROLINFO;

    TtagCTRLINFO = (
        CTRLINFO_EATS_RETURN = 1,
        CTRLINFO_EATS_ESCAPE = 2
        );

    TtagPOINTF = record
        x: single;
        y: single;
    end;
    PtagPOINTF = ^TtagPOINTF;


    TtagXFORMCOORDS = (
        XFORMCOORDS_POSITION = $1,
        XFORMCOORDS_SIZE = $2,
        XFORMCOORDS_HIMETRICTOCONTAINER = $4,
        XFORMCOORDS_CONTAINERTOHIMETRIC = $8,
        XFORMCOORDS_EVENTCOMPAT = $10
        );

    (*
    IEnumConnections = interface(IUnknown)
        ['{B196B287-BAB4-101A-B69C-00AA00341D07}']
        function Next(cConnections: ULONG; out rgcd: PIConnectData; out pcFetched: ULONG): HResult; stdcall;
        function Skip(cConnections: ULONG): HResult; stdcall;
        function Reset(): HResult; stdcall;
        function Clone(out ppEnum: IEnumConnections): HResult; stdcall;
    end;

    PIEnumConnections = ^IEnumConnections;    *)


    { this is not defined in ActiveX }

    TtagPROPBAG2_TYPE = (
        PROPBAG2_TYPE_UNDEFINED = 0,
        PROPBAG2_TYPE_DATA = 1,
        PROPBAG2_TYPE_URL = 2,
        PROPBAG2_TYPE_OBJECT = 3,
        PROPBAG2_TYPE_STREAM = 4,
        PROPBAG2_TYPE_STORAGE = 5,
        PROPBAG2_TYPE_MONIKER = 6
        );

    TtagPROPBAG2 = record
        dwType: DWORD;
        vt: TVARTYPE;
        cfType: TClipFormat;
        dwHint: DWORD;
        pstrName: POLESTR;
        clsid: TGUID;
    end;
    PtagPROPBAG2 = ^TtagPROPBAG2;

    TPROPBAG2 = TtagPROPBAG2;
    PPROPBAG2 = ^TPROPBAG2;



    IPropertyBag2 = interface(IUnknown)
        ['{22F55882-280B-11d0-A8A9-00A0C90C2004}']
        function Read(cProperties: ULONG; pPropBag: PPROPBAG2; pErrLog: IErrorLog; pvarValue: PVARIANT;
            var phrError: HRESULT): HResult; stdcall;
        function Write(cProperties: ULONG; pPropBag: PPROPBAG2; pvarValue: PVARIANT): HResult; stdcall;
        function CountProperties(out pcProperties: ULONG): HResult; stdcall;
        function GetPropertyInfo(iProperty: ULONG; cProperties: ULONG; out pPropBag: PPROPBAG2; out pcProperties: ULONG): HResult; stdcall;
        function LoadObject(pstrName: POleStr; dwHint: DWORD; pUnkObject: IUnknown; pErrLog: IErrorLog): HResult; stdcall;
    end;

    PIPropertyBag2 = ^IPropertyBag2;

implementation

end.



























