(*
 * slicke.wintools.menutheme.pas
 * Dark theming for Win11 popup menus (uxtheme opt-in, CBT hook, frame subclass).
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 * License: Apache License 2.0
 *)

{**
  @unit slicke.wintools.menutheme
  @brief Windows-only dark theming for Win11 popup menus.

  @details
  Extracted from @code(trndi.native.win); this unit owns the free-standing Win32
  plumbing that darkens native popup menus (uxtheme opt-in, per-thread WH_CBT
  hook, popup-HWND subclassing that overpaints the frame and submenu chevrons).
  None of it depends on @code(TTrndiNativeWindows), so it lives in the reusable
  @code(slicke) layer; @code(trndi.native.win) delegates into the two public
  entry points below.

  The public surface is deliberately tiny — everything else (hook procs,
  subclass externals, painters) is implementation-only.

  @author
  Björn Lindh.
}
unit slicke.wintools.menutheme;

{$I ../../inc/native.inc}

interface

uses
Windows, SysUtils, Forms, slicke.wintools.dwm;

  {**
    Toggle immersive dark mode on a single window (DWMWA_USE_IMMERSIVE_DARK_MODE).
    @param win The target window handle.
    @param Enable @true for dark, @false for light.
    @returns @true if the attribute was applied; @false on Windows < 10 1809.
  }
function WinApplyImmersiveDark(win: HWND; Enable: boolean = true): boolean;

  {**
    Opt the process into dark popup menus: dark app mode plus a per-thread CBT
    hook that themes and subclasses each popup-menu HWND on creation so its frame
    and submenu chevrons render dark. Idempotent; call once at startup.
    @returns @true if dark mode was engaged; @false on unsupported Windows builds.
  }
function EnableDarkPopupMenus: boolean;

implementation

type
  TSetPreferredAppMode   = function(AppMode: integer): integer; stdcall;
  TAllowDarkModeForApp   = function(Allow: BOOL): BOOL; stdcall;
  TAllowDarkModeForWindow= function(hwnd: HWND; Allow: BOOL): BOOL; stdcall;
  TSetWindowThemeFn      = function(hwnd: HWND; pszSubAppName, pszSubIdList: PWideChar): HRESULT; stdcall;
  TFlushMenuThemes       = procedure; stdcall;

const
  DarkModeMenuTheme: array[0..13] of WideChar =
    ('D','a','r','k','M','o','d','e','_','M','e','n','u', #0);
  // Subclass id used for the menu border subclass. Unique per logical purpose.
  MENU_BORDER_SUBCLASS_ID = $7242D14;
  // Timer id used to re-overpaint chevrons. Hover-out repaints on Win11 popup
  // menus bypass WM_PRINT/WM_PRINTCLIENT/WM_PAINT (the theme paints directly),
  // so we re-decorate from a low-frequency timer for the popup's lifetime.
  MENU_CHEVRON_TIMER_ID  = $7242D15;
  MENU_CHEVRON_TIMER_MS  = 33;

type
  TSubclassProc = function(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
    uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;

function SetWindowSubclass(hWnd: HWND; pfnSubclass: TSubclassProc;
  uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): BOOL; stdcall;
  external 'comctl32.dll' name 'SetWindowSubclass';
function DefSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
  external 'comctl32.dll' name 'DefSubclassProc';
function RemoveWindowSubclass(hWnd: HWND; pfnSubclass: TSubclassProc;
  uIdSubclass: UINT_PTR): BOOL; stdcall;
  external 'comctl32.dll' name 'RemoveWindowSubclass';

var
  GUxThemeModule: HMODULE = 0;
  GAllowDarkModeForWindow: TAllowDarkModeForWindow = nil;
  GSetWindowThemeFn: TSetWindowThemeFn = nil;
  GMenuCBTHook: HHOOK = 0;

{**
  Toggle immersive dark mode on a window. Moved from
  TTrndiNativeWindows.SetDarkMode so the menu hook (and the class method that
  used to own this) can share one implementation without a native<->slicke cycle.
}
function WinApplyImmersiveDark(win: HWND; Enable: boolean = true): boolean;
var
  Value: integer;
begin
  Result := false;
  // Check Windows version (Windows 10 1809+ required for immersive dark mode)
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and
    (Win32BuildNumber < 17763)) then
    Exit; // Windows 10 1809 (build 17763)

  Value := Ord(Enable);
  Result := HrSucceeded(SetDwmAttr(win, DWMWA_USE_IMMERSIVE_DARK_MODE,
    Value, SizeOf(Value)));
end;

// Overpaint the submenu chevron for items with a submenu. The DarkMode_Menu
// theme draws a dark chevron in the normal state and a white one when the
// item is hovered, which gives an inconsistent two-tone look. After the
// default WM_PRINT paint we walk the menu, look up each submenu-bearing
// item's rect, and paint our own light chevron over the theme's output.
procedure PaintMenuChevrons(hwnd: HWND; dc: HDC);
const
  MN_GETHMENU_       = $01E1;
  MIIM_STATE_        = $0001;
  MIIM_SUBMENU_      = $0004;
  MFS_HILITE_        = $0080;
  BgNormal           = $00202020;
  BgSelected         = $00383838;
  ChevColor          = $00E0E0E0;
  ChevAreaRight      = 4;   // px from item's right edge to chevron right
  ChevAreaWidth      = 18;  // px wide swatch we overpaint
var
  menu: HMENU;
  i, count: integer;
  mii: TMenuItemInfoW;
  rcItem, rcWnd, rcCh: TRect;
  isHot: boolean;
  bgCol: COLORREF;
  bgBrush, chBrush, chPen, oldBrush, oldPen: HGDIOBJ;
  pts: array[0..2] of TPoint;
  cx, cy, h, w: integer;
begin
  menu := HMENU(SendMessage(hwnd, MN_GETHMENU_, 0, 0));
  if menu = 0 then
    Exit;
  GetWindowRect(hwnd, rcWnd);
  count := GetMenuItemCount(menu);

  for i := 0 to count - 1 do
  begin
    FillChar(mii, SizeOf(mii), 0);
    mii.cbSize := SizeOf(mii);
    mii.fMask := MIIM_SUBMENU_ or MIIM_STATE_;
    if not GetMenuItemInfoW(menu, i, true, @mii) then
      Continue;
    if mii.hSubMenu = 0 then
      Continue;
    if not GetMenuItemRect(hwnd, menu, i, rcItem) then
      Continue;

    // GetMenuItemRect returns screen coords; convert to window-DC space.
    OffsetRect(rcItem, -rcWnd.Left, -rcWnd.Top);
    isHot := (mii.fState and MFS_HILITE_) <> 0;
    if isHot then
      bgCol := BgSelected
    else
      bgCol := BgNormal;

    rcCh.Left   := rcItem.Right - ChevAreaWidth - ChevAreaRight;
    rcCh.Top    := rcItem.Top + 2;
    rcCh.Right  := rcItem.Right - ChevAreaRight;
    rcCh.Bottom := rcItem.Bottom - 2;
    bgBrush := CreateSolidBrush(bgCol);
    if bgBrush <> 0 then
    try
      FillRect(dc, rcCh, bgBrush);
    finally
      DeleteObject(bgBrush);
    end;

    // Draw our own chevron in a consistent light color, sized like the LCL menu.
    h := 4;
    w := 4;
    cx := rcItem.Right - ChevAreaRight - 6;
    cy := (rcItem.Top + rcItem.Bottom) div 2;
    pts[0].x := cx - w div 2; pts[0].y := cy - h;
    pts[1].x := cx + w div 2; pts[1].y := cy;
    pts[2].x := cx - w div 2; pts[2].y := cy + h;

    chBrush := CreateSolidBrush(ChevColor);
    chPen := CreatePen(PS_SOLID, 1, ChevColor);
    oldBrush := SelectObject(dc, chBrush);
    oldPen := SelectObject(dc, chPen);
    try
      Polygon(dc, pts[0], 3);
    finally
      SelectObject(dc, oldBrush);
      SelectObject(dc, oldPen);
      DeleteObject(chBrush);
      DeleteObject(chPen);
    end;
  end;
end;

// Subclass procedure attached to popup menu HWNDs so we can paint a dark
// border ourselves. Win11 draws the popup frame via the visual style and does
// not honor DWMWA_BORDER_COLOR on menu HWNDs (returns E_HANDLE). The menu is
// rendered via WM_PRINT into an off-screen DC that DWM composites, so
// WM_NCPAINT is never delivered — we hook WM_PRINT instead.
// Paint our dark border + light chevrons onto the popup's DC. Shared by the
// WM_PRINT (initial composite) and WM_PAINT (hover-driven repaint) branches.
procedure DecorateMenuPopup(hwnd: HWND; dc: HDC);
const
  BorderColor = $00202020;
var
  rcWnd: TRect;
  br: HBRUSH;
begin
  PaintMenuChevrons(hwnd, dc);

  GetWindowRect(hwnd, rcWnd);
  OffsetRect(rcWnd, -rcWnd.Left, -rcWnd.Top);
  br := CreateSolidBrush(BorderColor);
  if br <> 0 then
  try
    FrameRect(dc, rcWnd, br); InflateRect(rcWnd, -1, -1);
    FrameRect(dc, rcWnd, br); InflateRect(rcWnd, -1, -1);
    FrameRect(dc, rcWnd, br);
  finally
    DeleteObject(br);
  end;
end;

function MenuBorderSubclassProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM; uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
const
  WM_PAINT_        = $000F;
  WM_NCDESTROY_    = $0082;
  WM_TIMER_        = $0113;
  WM_PRINT_        = $0317;
  WM_PRINTCLIENT_  = $0318;
var
  dc: HDC;
begin
  case uMsg of
    WM_PRINT_, WM_PRINTCLIENT_:
    begin
      // Initial paint and any partial repaints that go through the print
      // messages: decorate over the default output.
      Result := DefSubclassProc(hwnd, uMsg, wParam, lParam);
      dc := HDC(wParam);
      if dc <> 0 then
        DecorateMenuPopup(hwnd, dc);
      // Install the chevron-overpaint timer for the rest of the popup's life.
      // Hover-out repaints bypass every window message we can hook, so a
      // low-frequency timer is the only reliable way to keep the chevrons
      // light. SetTimer is idempotent on the same (hwnd, id) pair.
      SetTimer(hwnd, MENU_CHEVRON_TIMER_ID, MENU_CHEVRON_TIMER_MS, nil);
      Exit;
    end;
    WM_PAINT_:
    begin
      Result := DefSubclassProc(hwnd, uMsg, wParam, lParam);
      dc := GetWindowDC(hwnd);
      if dc <> 0 then
      try
        DecorateMenuPopup(hwnd, dc);
      finally
        ReleaseDC(hwnd, dc);
      end;
      Exit;
    end;
    WM_TIMER_:
    begin
      if wParam = MENU_CHEVRON_TIMER_ID then
      begin
        dc := GetWindowDC(hwnd);
        if dc <> 0 then
        try
          PaintMenuChevrons(hwnd, dc);
        finally
          ReleaseDC(hwnd, dc);
        end;
        Result := 0;
        Exit;
      end;
    end;
    WM_NCDESTROY_:
    begin
      KillTimer(hwnd, MENU_CHEVRON_TIMER_ID);
      RemoveWindowSubclass(hwnd, TSubclassProc(@MenuBorderSubclassProc), uIdSubclass);
    end;
  end;
  Result := DefSubclassProc(hwnd, uMsg, wParam, lParam);
end;

function MenuDarkModeCBTProc(nCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
const
  PopupMenuClass = '#32768';
var
  cls: array[0..31] of AnsiChar;
begin
  if (nCode = HCBT_CREATEWND) and (HWND(wParam) <> 0) then
  begin
    if (GetClassNameA(HWND(wParam), @cls[0], SizeOf(cls)) > 0) and
       (StrComp(@cls[0], PopupMenuClass) = 0) then
    begin
      if Assigned(GAllowDarkModeForWindow) then
        GAllowDarkModeForWindow(HWND(wParam), true);
      if Assigned(GSetWindowThemeFn) then
        GSetWindowThemeFn(HWND(wParam), @DarkModeMenuTheme[0], nil);
      WinApplyImmersiveDark(HWND(wParam), true);
      // Subclass the popup so WM_PRINT paints a dark border. SetWindowSubclass
      // is idempotent per (hwnd, proc, id); menu HWNDs are also torn down and
      // recreated each time the menu opens, so re-installs are safe.
      SetWindowSubclass(HWND(wParam), @MenuBorderSubclassProc,
        MENU_BORDER_SUBCLASS_ID, 0);
    end;
  end;
  Result := CallNextHookEx(GMenuCBTHook, nCode, wParam, lParam);
end;

{**
  Dark mode for popup menus
  -------------------------
  Three steps are needed to fully darken Win11 popup menus:

  1. Opt the process into uxtheme's dark mode via SetPreferredAppMode
     (ordinal 135, Win10 1903+) or AllowDarkModeForApp (ordinal 132, Win10 1809).
  2. Install a per-thread WH_CBT hook that catches each popup-menu HWND (class
     "#32768") on creation and calls AllowDarkModeForWindow +
     SetWindowTheme(hwnd, 'DarkMode_Menu', nil) + immersive-dark on it.
  3. Subclass each popup-menu HWND to overpaint the frame. The visual style and
     DWM still draw a light gray 2-3 px border that none of (1)/(2) reach;
     Win11 renders the menu via WM_PRINT into an off-screen DC that DWM
     composites, so we hook WM_PRINT and stack a few dark FrameRects on top.

  uxtheme.dll is kept loaded for the lifetime of the process — releasing it
  would invalidate the hook procedure's resolved pointers.

  Item-level dark colors are handled separately via TMenuItem.OnDrawItem
  (see TfBG.pmSettingsDrawItem); this code only handles the popup frame
  + window-level theming so the two halves match.
}
function EnableDarkPopupMenus: boolean;
const
  PreferredAppMode_AllowDark = 1;
var
  fnSetPreferred: TSetPreferredAppMode;
  fnAllowApp: TAllowDarkModeForApp;
  fnFlush: TFlushMenuThemes;
begin
  Result := false;
  if (Win32MajorVersion < 10) or ((Win32MajorVersion = 10) and
    (Win32BuildNumber < 17763)) then
    Exit;

  if GUxThemeModule = 0 then
    GUxThemeModule := LoadLibrary('uxtheme.dll');
  if GUxThemeModule = 0 then
    Exit;

  fnSetPreferred := TSetPreferredAppMode(GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(135)));
  if Assigned(fnSetPreferred) then
  begin
    fnSetPreferred(PreferredAppMode_AllowDark);
    Result := true;
  end
  else
  begin
    fnAllowApp := TAllowDarkModeForApp(GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(132)));
    if Assigned(fnAllowApp) then
    begin
      fnAllowApp(true);
      Result := true;
    end;
  end;

  if not Assigned(GAllowDarkModeForWindow) then
    GAllowDarkModeForWindow := TAllowDarkModeForWindow(
      GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(133)));
  if not Assigned(GSetWindowThemeFn) then
    GSetWindowThemeFn := TSetWindowThemeFn(GetProcAddress(GUxThemeModule, 'SetWindowTheme'));

  // Theme the AppHandle window (menu's owner in LCL's TrackPopupMenuEx call).
  // Windows uses the owner window's dark-mode flag when drawing popup frames.
  if Assigned(Application) and (Application.Handle <> 0) then
  begin
    if Assigned(GAllowDarkModeForWindow) then
      GAllowDarkModeForWindow(Application.Handle, true);
    WinApplyImmersiveDark(Application.Handle, true);
  end;

  // Install the per-thread CBT hook so popup menu HWNDs get themed and
  // subclassed on creation. Idempotent — only installs once per process.
  if (GMenuCBTHook = 0) and Assigned(GSetWindowThemeFn) then
    GMenuCBTHook := SetWindowsHookEx(WH_CBT, @MenuDarkModeCBTProc, 0, GetCurrentThreadId);

  fnFlush := TFlushMenuThemes(GetProcAddress(GUxThemeModule, MAKEINTRESOURCE(136)));
  if Assigned(fnFlush) then
    fnFlush;
end;

end.
