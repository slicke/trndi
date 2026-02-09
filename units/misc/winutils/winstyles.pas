// Based on code by Espectr0 on Lazarus forums
unit WinStyles;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Windows, Forms, Classes, SysUtils, Controls, StdCtrls, Menus;

const
  dwmapi_lib = 'dwmapi.dll';
  DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 = 19;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

  uxtheme_lib = 'uxtheme.dll';

type
  TWinItemColor = (wicBack, wicFront, wicSelect, wicText, wicText2, wicInputBack, wicInputBorder);


  // 1903 18362
  {$scopedEnums on}
  TPreferredAppMode =
  (
    Default,
    AllowDark,
    ForceDark,
    ForceLight,
    Max
  );
    TAppColorMode = (
    light = 0,
    dark = 1,
    defval = 2,
    alwaysdark = 3
  );

  {$scopedEnums off}

  // 1809 17763
  TShouldAppsUseDarkMode = function(): bool; stdcall; // ordinal 132
  TAllowDarkModeForWindow = function(hWnd: HWND; allow: bool): bool; stdcall; // ordinal 133
  TAllowDarkModeForApp = function(allow: bool): bool; stdcall; // ordinal 135, in 1809
  TFlushMenuThemes = procedure(); stdcall; // ordinal 136
  TRefreshImmersiveColorPolicyState = procedure(); stdcall; // ordinal 104
  // 18334
  TSetPreferredAppMode = function(appMode: TPreferredAppMode): TPreferredAppMode; stdcall; // ordinal 135, 1903

  TDwmSetWindowAttribute = function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  TSetWindowTheme = function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;

  { TWinStyleMode }

  TWinStyleMode = class
  private
    hLibDWM: TLibHandle;
    DwmSetWindowAttribute: TDwmSetWindowAttribute;
    hLibUX: HMODULE;
    SetWindowTheme: TSetWindowTheme;

    ShouldAppsUseDarkMode: TShouldAppsUseDarkMode;
    AllowDarkModeForWindow: TAllowDarkModeForWindow;
    AllowDarkModeForApp: TAllowDarkModeForApp;
    FlushMenuThemes: TFlushMenuThemes;
    RefreshImmersiveColorPolicyState: TRefreshImmersiveColorPolicyState;
    SetPreferredAppMode: TSetPreferredAppMode;

    procedure setAppDarkMode(const colormode: TAppColorMode = TAppColorMode.defval);
  public
    constructor Create(const AForm: TForm; AMode: TPreferredAppMode = TPreferredAppMode.Default); overload;
    destructor Destroy; override;
    function Loaded: Boolean;
    function SetFormStyle(const AForm: TForm; const AMode: TPreferredAppMode): Boolean;
    function setFormStyle(const AForm: TForm; const dark: TAppColorMode = TAppColorMode.defval): Boolean;
    function setContainerStyle(const AItem: TWinControl; const AValue: Bool; const explorer: boolean = true): Boolean;
  end;

procedure MyFillMenuBkg(const AMenu: TMenu; dark: boolean = true);
function IsWindows10OrGreater(const ABuild: Integer = 0): Boolean;
function winUiColor(const col: TWinItemColor; dark: boolean): int64;
function winUiColor(const col: TWinItemColor; amode: TPreferredAppMode): int64;

const
  TWinItemColorDark: array[TWinItemColor] of int64 = ($002A2A2A, $00353535, $002E2E2E, $F0F0F0,
            $009D9D9D, $00262626, $00454545);
  TWinItemColorLight: array[TWinItemColor] of  int64 = ($F0F0F0, $00F2F2F2, $00FBFBFB, $000B0B0B,
             $00656565, $F0F0F0, $00EFEFEF);
// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TWinStyleMode }

// -----------------------------------------------------------------------------

function isDark(amode: TPreferredAppMode): boolean;
begin
  result := (amode = TPreferredAppMode.AllowDark) or (amode = TPreferredAppMode.ForceDark);
end;

function matchMode(amode: TPreferredAppMode): TAppColorMode;
begin
    case amode of
      TPreferredAppMode.AllowDark,
      TPreferredAppMode.ForceDark: result := TAppColorMode.dark;
      TPreferredAppMode.ForceLight: result := TAppColorMode.light;
      else
          result := TAppColorMode.defval;
      end;
end;

function matchMode(amode: TAppColorMode): TPreferredAppMode;
begin
    case amode of
      TAppColorMode.dark: result := TPreferredAppMode.AllowDark;
      TAppColorMode.alwaysdark: result := TPreferredAppMode.ForceDark;
      TAppColorMode.light: result := TPreferredAppMode.ForceLight;
    else
        result := TPreferredAppMode.Default;
    end;
end;

function winUiColor(const col: TWinItemColor; dark: boolean): int64;
begin
  if dark then
   result := TWinItemColorDark[col]
  else
   result := TWinItemColorLight[col];

end;
function winUiColor(const col: TWinItemColor; amode: TPreferredAppMode): int64;
begin
  result := winuiColor(col, isdark(amode));
end;

constructor TWinStyleMode.Create(const AForm: TForm; AMode: TPreferredAppMode = TPreferredAppMode.Default);
begin
  DwmSetWindowAttribute := NIL;
  SetWindowTheme  := NIL;

  hLibDWM := LoadLibraryExW(dwmapi_lib, 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
  if hLibDWM <> 0 then Pointer(DwmSetWindowAttribute) := GetProcAddress(hLibDWM, 'DwmSetWindowAttribute');

  hLibUX := LoadLibraryExW(uxtheme_lib, 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
  if hLibUX <> 0 then
  begin
    Pointer(SetWindowTheme) := GetProcAddress(hLibUX, 'SetWindowTheme');
    Pointer(RefreshImmersiveColorPolicyState) := GetProcAddress(hLibUX, MakeIntResource(104));
    Pointer(ShouldAppsUseDarkMode) := GetProcAddress(hLibUX, MakeIntResource(132));
    Pointer(AllowDarkModeForWindow) := GetProcAddress(hLibUX, MakeIntResource(133));
    Pointer(FlushMenuThemes) := GetProcAddress(hLibUX, MakeIntResource(136));

    if not IsWindows10OrGreater(18362) then
      Pointer(AllowDarkModeForApp) := GetProcAddress(hLibUX, MakeIntResource(135))
    else
      Pointer(SetPreferredAppMode) := GetProcAddress(hLibUX, MakeIntResource(135));
  end;

  SetFormStyle(AForm, matchmode(amode));
end;

// -----------------------------------------------------------------------------

destructor TWinStyleMode.Destroy;
begin
  if Pointer(DwmSetWindowAttribute) <> NIL then DwmSetWindowAttribute := NIL;
  if hLibDWM <> 0 then FreeLibrary(hLibDWM);

  if Pointer(SetWindowTheme) <> NIL then SetWindowTheme := NIL;

  if Pointer(ShouldAppsUseDarkMode) <> NIL then ShouldAppsUseDarkMode := NIL;
  if Pointer(AllowDarkModeForWindow) <> NIL then AllowDarkModeForWindow := NIL;
  if Pointer(AllowDarkModeForApp) <> NIL then AllowDarkModeForApp := NIL;
  if Pointer(FlushMenuThemes) <> NIL then FlushMenuThemes := NIL;
  if Pointer(RefreshImmersiveColorPolicyState) <> NIL then RefreshImmersiveColorPolicyState := NIL;

  if hLibUX <> 0 then FreeLibrary(hLibUX);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TWinStyleMode.Loaded: Boolean;
begin
  Result := (hLibDWM <> 0) and (hLibUX <> 0)
    and Assigned(DwmSetWindowAttribute) and Assigned(SetWindowTheme);
end;

// -----------------------------------------------------------------------------

procedure TWinStyleMode.setAppDarkMode(const colormode: TAppColorMode = TAppColorMode.defval);
var
  dark: boolean;
begin
(*  if Assigned(AllowDarkModeForApp) then
     AllowDarkModeForApp(allow)
   else if Assigned(SetPreferredAppMode) then
   begin
     case colormode of
       TPreferredAppMode.ForceDark:
     if allow then

     else                                             *)
    dark := colormode <> TAppColorMode.light;
    if Assigned(AllowDarkModeForApp) then
     AllowDarkModeForApp(dark)
     else
       SetPreferredAppMode(TPreferredAppMode.Default);
end;

// -----------------------------------------------------------------------------

function TWinStyleMode.SetContainerStyle(const AItem: TWinControl; const AValue: Bool; const explorer: boolean = true): Boolean;
var
  attr: integer;
  mode: string;
begin
  Result := False;
  if (AItem = NIL) or not Loaded then Exit;

//  AllowDarkModeForWindow(AItem.Handle, AValue);
  attr := DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1;
  if IsWindows10OrGreater(18985) then attr := DWMWA_USE_IMMERSIVE_DARK_MODE;

  if explorer then
    mode := 'Explorer'
  else
    mode := 'CFD';

  if AValue then
    mode := 'DarkMode_' + mode;
//  DwmSetWindowAttribute(AItem.Handle, attr, @AValue, SizeOf(AValue));
  SetWindowTheme(Aitem.Handle, PWideChar(mode), nil);
  AllowDarkModeForWindow(AItem.Handle, AValue);
  Aitem.Color := winuiColor(wicInputBack, avalue);
  SendMessageW(AItem.handle, WM_THEMECHANGED, 0, 0);
end;

function TWinStyleMode.setFormStyle(const AForm: TForm; const dark: TAppColorMode = TAppColorMode.defval): Boolean;
begin
  result := setFormStyle(aform, matchmode(dark));
end;


function TWinStyleMode.setFormStyle(const AForm: TForm; const AMode: TPreferredAppMode): Boolean;
var
  attr: DWord;
  C: TComponent;
  dark: longbool;
begin
  Result := False;
  if (AForm = NIL) or not Loaded then Exit;
  dark := isdark(AMode);
  SetAppDarkMode(matchmode(amode));
  AllowDarkModeForWindow(AForm.Handle, dark);
  RefreshImmersiveColorPolicyState;
  FlushMenuThemes;
  if dark then
     SetWindowTheme(AForm.Handle, 'DarkMode_Explorer', NIL)
  else
     SetWindowTheme(AForm.Handle, 'Explorer', NIL);

  if IsWindows10OrGreater(17763) then
  begin
    attr := DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1;
    if IsWindows10OrGreater(18985) then attr := DWMWA_USE_IMMERSIVE_DARK_MODE;

    DwmSetWindowAttribute(AForm.Handle, attr, @dark, SizeOf(dark));
  end;

  with AForm do
  begin
    Color      := winuiColor(wicBack, amode);
    Font.Color := winuiColor(wicText, amode);
  end;

  for C in AForm do
  begin
    if (C is TWinControl) then
    begin
      if not TWinControl(C).IsParentColor then
        TWinControl(C).Color := winuiColor(wicInputBack, amode);

      if (C is TComboBox) or (C is TEdit) or (C is TMemo) then begin
         setContainerStyle(TWinControl(C), dark, false);
      TWinControl(c).Font.Color := winUIColor(wicText, dark);
   //     SetWindowTheme(TWinControl(C).Handle, 'DarkMode_CFD', NIL)
      end else
              setContainerStyle(TWinControl(C), dark, true)
     //   SetWindowTheme(TWinControl(C).Handle, 'DarkMode_Explorer', NIL);
    end
    else if (C is TMenu) then
      MyFillMenuBkg(TMenu(C))
    else if (C is TGraphicControl) and (TGraphicControl(c).Font.Color = $20000000) then
      TGraphicControl(c).Font.Color := winUIColor(wicText, dark);
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure MyFillMenuBkg(const AMenu: TMenu; dark: boolean = true);
var
  MenuInfo: TMenuInfo;
begin
  if AMenu <> NIL then
  begin
    FillByte(MenuInfo, SizeOf(TMenuInfo), 0);
    MenuInfo.cbSize  := SizeOf(TMenuInfo);
    MenuInfo.fMask   := MIM_BACKGROUND or MIM_APPLYTOSUBMENUS or MIM_STYLE;
    MenuInfo.hbrBack := CreateSolidBrush(winuiColor(wicInputBack, dark));
    SetMenuInfo(AMenu.Handle, @MenuInfo);
  end;
end;

// -----------------------------------------------------------------------------

function IsWindows10OrGreater(const ABuild: Integer = 0): Boolean;
begin
  Result := (Win32MajorVersion >= 10) and (Win32BuildNumber >= ABuild);
end;

// -----------------------------------------------------------------------------

end.
