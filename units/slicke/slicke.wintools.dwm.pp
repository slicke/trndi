(*
 * slicke.wintools.dwm.pas
 * Low-level shared plumbing for DWM attribute interop.
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 * License: Apache License 2.0
 *)

{**
  @unit slicke.wintools.dwm
  @brief Windows-only Desktop Window Manager attribute interop.

  @details
  Low-level shared plumbing for @code(DwmSetWindowAttribute)/@code(DwmGetWindowAttribute):
  the raw externals, the @code(DWMWA_*) attribute ids Trndi uses (caption/text color,
  immersive dark mode), and the two tiny helpers @link(SetDwmAttr) and
  @link(HrSucceeded). Consumed by @code(trndi.native.win) (caption colors, badge
  geometry) and @code(slicke.wintools.menutheme) (immersive dark mode); centralising
  it here keeps a single copy of the attribute ids and the DWM entry points.

  @author
  Björn Lindh.
}
unit slicke.wintools.dwm;

{$I ../../inc/native.inc}

interface

uses
Windows;

const
  { DWM attribute ids used for caption and text colors, and immersive dark mode. }
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  DWMWA_CAPTION_COLOR           = 35;
  DWMWA_TEXT_COLOR              = 36;

function DwmSetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer;
  cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
function DwmGetWindowAttribute(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer;
  cbAttribute: DWORD): HRESULT; stdcall; external 'dwmapi.dll';

  {**
    Typed convenience wrapper around @code(DwmSetWindowAttribute): pass the
    attribute value by reference and its size.
  }
function SetDwmAttr(hWnd: HWND; Attr: DWORD; const Data; Size: DWORD): HRESULT;

  {**
    @code(SUCCEEDED(hr)) — true when the HRESULT is not a failure code.
  }
function HrSucceeded(hr: HRESULT): boolean; inline;

implementation

function SetDwmAttr(hWnd: HWND; Attr: DWORD; const Data; Size: DWORD): HRESULT;
begin
  Result := DwmSetWindowAttribute(hWnd, Attr, @Data, Size);
end;

function HrSucceeded(hr: HRESULT): boolean; inline;
begin
  Result := hr >= 0; // SUCCEEDED(hr)
end;

end.
