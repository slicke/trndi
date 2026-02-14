unit winutils.wintaskbar;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Windows, Classes, Forms, Controls, Graphics, ComObj, ActiveX, Dialogs {$ifdef DEBUG},trndi.log{$endif};

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
    {** Window handle used when making taskbar calls (diagnostic). }
    property WindowHandle: HWND read FWindowHandle;
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
  {$ifdef debug}
  LogMessageToFile(PChar('[TaskbarError] ' + Msg));
  {$endif}
end;

// Helper used to find a top-level visible window owned by this process. This
// is a best-effort fallback for cases where Application/MainForm handles are
// not available or do not correspond to the taskbar button the OS displays.
type
  PFindWndInfo = ^TFindWndInfo;
  TFindWndInfo = record
    TargetPID: DWORD;
    FoundHandle: HWND;
  end;

function EnumWindowsProc_FindPID(hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  pid: DWORD;
  info: PFindWndInfo;
  exstyle: NativeUInt;
begin
  info := PFindWndInfo(lParam);
  GetWindowThreadProcessId(hwnd, @pid);
  if pid <> info^.TargetPID then
  begin
    Result := True; // continue enumeration
    Exit;
  end;

  // Only consider top-level visible windows with a non-empty caption
  if (GetWindow(hwnd, GW_OWNER) <> 0) then
  begin
    Result := True;
    Exit;
  end;
  if not IsWindowVisible(hwnd) and not IsIconic(hwnd) then
  begin
    Result := True;
    Exit;
  end;
  if GetWindowTextLength(hwnd) = 0 then
  begin
    Result := True;
    Exit;
  end;

  // Exclude toolwindows; they normally don't get a taskbar button
  // exstyle := NativeUInt(GetWindowLongPtr(hwnd, GWL_EXSTYLE));
  // if (exstyle and NativeUInt(WS_EX_TOOLWINDOW)) <> 0 then
  // begin
  //   Result := True;
  //   Exit;
  // end;

  info^.FoundHandle := hwnd;
  Result := False; // stop enumeration
end;

function FindTopLevelWindowForCurrentProcess: HWND;
var
  info: TFindWndInfo;
begin
  Result := 0;
  info.TargetPID := GetCurrentProcessId;
  info.FoundHandle := 0;
  EnumWindows(@EnumWindowsProc_FindPID, LPARAM(@info));
  Result := info.FoundHandle;
end;

function IsProbablyTaskbarWindow(hwnd: HWND): Boolean;
var
  exstyle: NativeUInt;
begin
  Result := False;
  if (hwnd = 0) or (not IsWindow(hwnd)) then Exit;
  if not IsWindowVisible(hwnd) and not IsIconic(hwnd) then Exit;
  if GetWindow(hwnd, GW_OWNER) <> 0 then Exit;
  if GetWindowTextLength(hwnd) = 0 then Exit;

  // Avoid windows marked as "tool windows" (these normally don't get taskbar buttons)
  // exstyle := NativeUInt(GetWindowLongPtr(hwnd, GWL_EXSTYLE));
  // if (exstyle and NativeUInt(WS_EX_TOOLWINDOW)) <> 0 then Exit;

  Result := True;
end;

{ Constructor: Initialize COM and get taskbar interface }
constructor TWinTaskbar.Create(WindowHandle: HWND = 0);
var
  HR: HRESULT;
  chosen: HWND;
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

    // Determine handle to use. Preference order:
    //  1) explicit WindowHandle argument (only if it *looks* like a taskbar window)
    //  2) visible Application.MainForm.Handle
    //  3) a top-level visible window owned by this process (enum)
    //  4) Application.Handle as a last resort
    chosen := WindowHandle;

    // If caller passed a non-zero handle, validate it; reject toolwindows so we
    // don't accidentally target a hidden/tool window that won't have a taskbar button.
    if (chosen <> 0) and (not IsProbablyTaskbarWindow(chosen)) then
    begin
      {$ifdef DEBUG}
      LogMessageToFile(PChar(Format('[TWinTaskbar] Passed WindowHandle=%d rejected (not a taskbar window). Falling back.', [chosen])));
      {$endif}
      chosen := 0;
    end;

    if chosen = 0 then
    begin
      if Assigned(Application) and Assigned(Application.MainForm) and
         (Application.MainForm.Handle <> 0) and IsProbablyTaskbarWindow(Application.MainForm.Handle) then
        chosen := Application.MainForm.Handle
      else
        chosen := FindTopLevelWindowForCurrentProcess;

      if chosen = 0 then
        chosen := Application.Handle;
    end;

    FWindowHandle := chosen;

    if FWindowHandle = 0 then
    begin
      LogError('Invalid window handle (no Application/MainForm handle available)');
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
      // Diagnostic: report selected HWND so caller can validate which window is targeted
      {$ifdef DEBUG}
      LogMessageToFile(PChar(Format('[TWinTaskbar] Initialized (HWND=%d)', [FWindowHandle])));
      {$endif}
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
var
  hr: HRESULT;
begin
  Result := False;
  if not FInitialized then
  begin
    LogError('Not initialized');
    Exit;
  end;

  // If the stored HWND was destroyed/recreated, re-resolve a suitable top-level
  // window so taskbar calls target the window the OS shows.
  if (FWindowHandle = 0) or (not IsWindow(FWindowHandle)) then
  begin
   {$ifdef DEBUG}
    LogMessageToFile(PChar('[TWinTaskbar] WindowHandle invalid; attempting re-resolve'));
   {$endif}
    if Assigned(Application) and Assigned(Application.MainForm) and
       IsProbablyTaskbarWindow(Application.MainForm.Handle) then
      FWindowHandle := Application.MainForm.Handle
    else
      FWindowHandle := FindTopLevelWindowForCurrentProcess;

    {$ifdef DEBUG}
    if (FWindowHandle <> 0) and IsWindow(FWindowHandle) then
      LogMessageToFile(PChar(Format('[TWinTaskbar] Re-resolved WindowHandle=%d', [FWindowHandle])))
    else
      LogError('Unable to re-resolve a valid window handle for taskbar calls');
   {$endif}
  end;

  // Ensure the re-resolved window is suitable for taskbar operations (not a ToolWindow, etc.)
  LogError(Format('Window check: Visible=%s Iconic=%s Owner=%d TitleLen=%d', [BoolToStr(IsWindowVisible(FWindowHandle)), BoolToStr(IsIconic(FWindowHandle)), GetWindow(FWindowHandle, GW_OWNER), GetWindowTextLength(FWindowHandle)]));
  if not IsProbablyTaskbarWindow(FWindowHandle) then
  begin
    LogError('Re-resolved window is not a taskbar window (e.g., ToolWindow or hidden)');
    Result := False;
    Exit;
  end;

  if Max = 0 then Max := 100;  // Prevent division by zero

  try
    hr := FTaskbar.SetProgressValue(FWindowHandle, Current, Max);
    if hr = S_OK then
    begin
      Result := True;
     {$ifdef DEBUG}
      LogMessageToFile(PChar(Format('[TWinTaskbar] SetProgressValue OK HWND=%d %d/%d', [FWindowHandle, Current, Max])));
     {$endif}
    end
    else
      LogError('SetProgressValue failed HR=' + IntToHex(hr, 8));
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
var
  hr: HRESULT;
begin
  Result := False;
  if not FInitialized then
  begin
    LogError('Not initialized');
    Exit;
  end;

  // Ensure the target HWND is still valid; re-resolve if necessary
  if (FWindowHandle = 0) or (not IsWindow(FWindowHandle)) then
  begin
    {$ifdef DEBUG}
    LogMessageToFile(PChar('[TWinTaskbar] WindowHandle invalid before SetProgressState; attempting re-resolve'));
    {$endif}
    if Assigned(Application) and Assigned(Application.MainForm) and
       IsProbablyTaskbarWindow(Application.MainForm.Handle) then
      FWindowHandle := Application.MainForm.Handle
    else
      FWindowHandle := FindTopLevelWindowForCurrentProcess;

    {$ifdef DEBUG}
    if (FWindowHandle <> 0) and IsWindow(FWindowHandle) then
      LogMessageToFile(PChar(Format('[TWinTaskbar] Re-resolved WindowHandle=%d', [FWindowHandle])))
    else
      LogError('Unable to re-resolve a valid window handle for taskbar calls');
   {$endif}
  end;

  // Ensure the re-resolved window is suitable for taskbar operations (not a ToolWindow, etc.)
  LogError(Format('Window check: Visible=%s Iconic=%s Owner=%d TitleLen=%d', [BoolToStr(IsWindowVisible(FWindowHandle)), BoolToStr(IsIconic(FWindowHandle)), GetWindow(FWindowHandle, GW_OWNER), GetWindowTextLength(FWindowHandle)]));
  if not IsProbablyTaskbarWindow(FWindowHandle) then
  begin
    LogError('Re-resolved window is not a taskbar window (e.g., ToolWindow or hidden)');
    Result := False;
    Exit;
  end;

  try
    hr := FTaskbar.SetProgressState(FWindowHandle, StateFlags[ProgressState]);
    if hr = S_OK then
    begin
      Result := True;
      {$ifdef DEBUG}
      LogMessageToFile(PChar(Format('[TWinTaskbar] SetProgressState OK HWND=%d state=%d', [FWindowHandle, Ord(ProgressState)])));
      {$endif}
      // For indeterminate progress, set a dummy value to ensure it's visible
      if ProgressState = tbpsIndeterminate then
        SetProgressValue(1, 100);
    end
    else
      LogError('SetProgressState failed HR=' + IntToHex(hr, 8));
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
  // Defer creation of the global taskbar instance to lazy init in callers.
  // Previously we eagerly created GlobalTaskbar here which could pick an
  // incorrect window handle at startup (Application.MainForm not ready).
  GlobalTaskbar := nil;


finalization
  // Free global instance
  if Assigned(GlobalTaskbar) then
    GlobalTaskbar.Free;

end.

