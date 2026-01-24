unit wintaskbar;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Windows, Classes, Forms, Controls, Graphics, ComObj, ActiveX, Dialogs;

const
  // Taskbar Progress States
  TBPF_NOPROGRESS     = $0000;
  TBPF_INDETERMINATE  = $0001;
  TBPF_NORMAL         = $0002;
  TBPF_ERROR          = $0004;
  TBPF_PAUSED         = $0008;

  // GUID for TaskbarList
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';

type
  // Progress style enum
  TTaskBarProgressStyle = (tbpsNone, tbpsIndeterminate, tbpsNormal, tbpsError, tbpsPaused);

  // Interfaces for Windows 7+ Taskbar
  ITaskbarList = interface(IUnknown)
    ['{56FDF342-FD6D-11D0-958A-006097C9A090}']
    function HrInit: HRESULT; stdcall;
    function AddTab(hwnd: HWND): HRESULT; stdcall;
    function DeleteTab(hwnd: HWND): HRESULT; stdcall;
    function ActivateTab(hwnd: HWND): HRESULT; stdcall;
    function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
  end;

  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429B-A66E-1935E44F4317}']
    function MarkFullscreenWindow(hwnd: HWND; fFullscreen: BOOL): HRESULT; stdcall;
  end;

  ITaskbarList3 = interface(ITaskbarList2)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function SetProgressValue(hwnd: HWND; ullCompleted: ULONGLONG; ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: DWORD): HRESULT; stdcall;
    function RegisterTab(hwndTab: HWND; hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab: HWND; hwndInsertBefore: HWND): HRESULT; stdcall;
    function SetTabActive(hwndTab: HWND; hwndMDI: HWND; tbatFlags: DWORD): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT; pButton: Pointer): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT; pButton: Pointer): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: HWND; himl: HIMAGELIST): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: HWND; hIcon: HICON; pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: HWND; prcClip: PRECT): HRESULT; stdcall;
  end;

  // Simple Windows Taskbar Progress class
  TWinTaskbar = class
  private
    FTaskbar: ITaskbarList3;
    FWindowHandle: HWND;
    FInitialized: Boolean;
    FLastError: string;

    // Helper functions
    function CreateBadgeIcon(const Text: string; BackColor: TColor = clRed; TextColor: TColor = clWhite): HICON;
    function WideString(const S: string): WideString;
    procedure LogError(const Msg: string);
  public
    // Basic lifecycle methods
    constructor Create(WindowHandle: HWND = 0);
    destructor Destroy; override;

    // Progress methods
    function SetProgressValue(Current, Max: UInt64): Boolean;
    function SetProgressState(ProgressState: TTaskBarProgressStyle): Boolean;

    // Badge methods
    function SetBadge(const Text: string): Boolean;
    function ClearBadge: Boolean;

    // Diagnostic properties
    property Initialized: Boolean read FInitialized;
    property LastError: string read FLastError;
  end;

var
  GlobalTaskbar: TWinTaskbar;

implementation

{ Helper function to convert integer to string with error checking }
function IntToStrSafe(Value: Integer): string;
begin
  try
    Result := IntToStr(Value);
  except
    Result := '?';
  end;
end;

{ Convert standard string to WideString with error checking }
function TWinTaskbar.WideString(const S: string): WideString;
begin
  try
    Result := System.WideString(S);
  except
    Result := '';
  end;
end;

{ Log error message and store in LastError }
procedure TWinTaskbar.LogError(const Msg: string);
begin
  FLastError := Msg;
  OutputDebugString(PChar('[TaskbarError] ' + Msg));
end;

{ Constructor: Initialize COM and get taskbar interface }
constructor TWinTaskbar.Create(WindowHandle: HWND = 0);
var
  HR: HRESULT;
begin
  inherited Create;

  FInitialized := False;
  FLastError := '';

  // Initialize COM if needed
  try
    HR := CoInitialize(nil);
    if (HR <> S_OK) and (HR <> S_FALSE) then
    begin
      LogError('Failed to initialize COM. HR=' + IntToStrSafe(HR));
      Exit;
    end;

    // Set window handle
    if WindowHandle = 0 then
      FWindowHandle := Application.Handle
    else
      FWindowHandle := WindowHandle;

    if FWindowHandle = 0 then
    begin
      LogError('Invalid window handle');
      Exit;
    end;

    // Create taskbar interface
    try
      FTaskbar := CreateComObject(CLSID_TaskbarList) as ITaskbarList3;
      if FTaskbar = nil then
      begin
        LogError('Failed to create TaskbarList COM object');
        Exit;
      end;

      // Initialize taskbar
      HR := FTaskbar.HrInit;
      if HR <> S_OK then
      begin
        LogError('Failed to initialize taskbar. HR=' + IntToStrSafe(HR));
        FTaskbar := nil;
        Exit;
      end;

      FInitialized := True;
    except
      on E: Exception do
      begin
        LogError('Exception creating taskbar: ' + E.Message);
        FTaskbar := nil;
      end;
    end;
  except
    on E: Exception do
    begin
      LogError('Initialization exception: ' + E.Message);
    end;
  end;
end;

{ Destructor: Cleanup }
destructor TWinTaskbar.Destroy;
begin
  if FTaskbar <> nil then
  begin
    // Clear progress state
    try
      FTaskbar.SetProgressState(FWindowHandle, TBPF_NOPROGRESS);
    except
      // Ignore errors during cleanup
    end;

    // Clear overlay icon
    try
      FTaskbar.SetOverlayIcon(FWindowHandle, 0, nil);
    except
      // Ignore errors during cleanup
    end;

    // Release interface
    FTaskbar := nil;
  end;

  // Uninitialize COM
  CoUninitialize;

  inherited Destroy;
end;

{ Set progress value }
function TWinTaskbar.SetProgressValue(Current, Max: UInt64): Boolean;
begin
  Result := False;
  if not FInitialized then
  begin
    LogError('Not initialized');
    Exit;
  end;

  if Max = 0 then Max := 100;  // Prevent division by zero

  try
    if FTaskbar.SetProgressValue(FWindowHandle, Current, Max) = S_OK then
      Result := True
    else
      LogError('SetProgressValue failed');
  except
    on E: Exception do
    begin
      LogError('Exception in SetProgressValue: ' + E.Message);
      Result := False;
    end;
  end;
end;

{ Set progress state }
function TWinTaskbar.SetProgressState(ProgressState: TTaskBarProgressStyle): Boolean;
const
  StateFlags: array[TTaskBarProgressStyle] of DWORD = (
    TBPF_NOPROGRESS,
    TBPF_INDETERMINATE,
    TBPF_NORMAL,
    TBPF_ERROR,
    TBPF_PAUSED
  );
begin
  Result := False;
  if not FInitialized then
  begin
    LogError('Not initialized');
    Exit;
  end;

  try
    if FTaskbar.SetProgressState(FWindowHandle, StateFlags[ProgressState]) = S_OK then
    begin
      Result := True;

      // For indeterminate progress, set a dummy value to ensure it's visible
      if ProgressState = tbpsIndeterminate then
        SetProgressValue(1, 100);
    end
    else
      LogError('SetProgressState failed');
  except
    on E: Exception do
    begin
      LogError('Exception in SetProgressState: ' + E.Message);
      Result := False;
    end;
  end;
end;

{ Create a badge icon with text }
function TWinTaskbar.CreateBadgeIcon(const Text: string; BackColor: TColor = clRed; TextColor: TColor = clWhite): HICON;
var
  Bitmap: TBitmap;
  Canvas: TCanvas;
  Icon: TIcon;
  TextWidth, TextHeight: Integer;
  CenterX, CenterY: Integer;
  BitmapSize: Integer;
begin
  Result := 0;

  try
    BitmapSize := 16; // Small size works better for overlays

    // Create bitmap
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32bit;
      Bitmap.Width := BitmapSize;
      Bitmap.Height := BitmapSize;

      // Get canvas
      Canvas := Bitmap.Canvas;

      // Clear bitmap (transparent)
      Canvas.Brush.Color := clFuchsia; // Use as transparency color
      Canvas.FillRect(Rect(0, 0, BitmapSize, BitmapSize));

      // Draw filled circle for background
      Canvas.Brush.Color := BackColor;
      Canvas.Pen.Color := BackColor;
      Canvas.Ellipse(0, 0, BitmapSize, BitmapSize);

      // Configure text
      Canvas.Font.Color := TextColor;
      Canvas.Font.Size := 7;
      Canvas.Font.Style := [fsBold];

      // Center text
      TextWidth := Canvas.TextWidth(Text);
      TextHeight := Canvas.TextHeight(Text);
      CenterX := (BitmapSize - TextWidth) div 2;
      CenterY := (BitmapSize - TextHeight) div 2;

      // Draw text
      Canvas.TextOut(CenterX, CenterY, Text);

      // Convert to icon
      Icon := TIcon.Create;
      try
        // Optional: Set bitmap transparent color
        Bitmap.TransparentColor := clFuchsia;
        Bitmap.Transparent := True;

        // Convert bitmap to icon
        Icon.Assign(Bitmap);

        // Get icon handle
        Result := CopyIcon(Icon.Handle);
      finally
        Icon.Free;
      end;
    finally
      Bitmap.Free;
    end;
  except
    on E: Exception do
    begin
      LogError('Error creating badge icon: ' + E.Message);
      if Result <> 0 then
        DestroyIcon(Result);
      Result := 0;
    end;
  end;
end;

{ Set badge with text }
function TWinTaskbar.SetBadge(const Text: string): Boolean;
var
  IconHandle: HICON;
  DescriptionW: WideString;
begin
  Result := False;
  if not FInitialized then
  begin
    LogError('Not initialized');
    Exit;
  end;

  IconHandle := 0;
  try
    // Create icon with text
    IconHandle := CreateBadgeIcon(Text);
    if IconHandle = 0 then
    begin
      LogError('Failed to create badge icon');
      Exit;
    end;

    // Convert description to WideString
    DescriptionW := WideString(Text);

    // Set overlay
    if FTaskbar.SetOverlayIcon(FWindowHandle, IconHandle, PWideChar(DescriptionW)) = S_OK then
      Result := True
    else
      LogError('SetOverlayIcon failed');
 finally
    // Clean up icon
    if IconHandle <> 0 then
      DestroyIcon(IconHandle);
  end;
end;

{ Clear badge }
function TWinTaskbar.ClearBadge: Boolean;
begin
  Result := False;
  if not FInitialized then
  begin
    LogError('Not initialized');
    Exit;
  end;

  try
    if FTaskbar.SetOverlayIcon(FWindowHandle, 0, nil) = S_OK then
      Result := True
    else
      LogError('ClearBadge failed');
  except
    on E: Exception do
    begin
      LogError('Exception in ClearBadge: ' + E.Message);
      Result := False;
    end;
  end;
end;

initialization
  // Create global instance with default application window
  GlobalTaskbar := nil;
  try
    GlobalTaskbar := TWinTaskbar.Create;
    if not GlobalTaskbar.Initialized then
    begin
      FreeAndNil(GlobalTaskbar);
      OutputDebugString('[Taskbar] Failed to initialize global taskbar instance');
    end;
  except
    on E: Exception do
    begin
      OutputDebugString(PChar('[Taskbar] Exception creating global instance: ' + E.Message));
      GlobalTaskbar := nil;
    end;
  end;

finalization
  // Free global instance
  if Assigned(GlobalTaskbar) then
    GlobalTaskbar.Free;

end.

