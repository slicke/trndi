unit TouchDetection;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS}
Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
BaseUnix, Linux, Unix,
{$ENDIF}
Classes, SysUtils;

type
TTouchPoint = record
  X, Y: integer;
  Pressure: single;
  ID: integer;
  IsValid: boolean;
end;

TTouchInfo = record
  Count: integer;
  Points: array of TTouchPoint;
end;

  { TTouchDetector }

TTouchDetector = class
private
  FLastError: string;
  {$IFDEF WINDOWS}
  function InitializeWindowsTouch: boolean;
  function GetWindowsTouchInfo: TTouchInfo;
  {$ENDIF}
  {$IFDEF LINUX}
  function FindTouchDevice: string;
  function GetLinuxTouchInfo: TTouchInfo;
  {$ENDIF}
public
  constructor Create;
  destructor Destroy; override;

  function GetTouchInfo: TTouchInfo;
  function GetActiveTouchCount: integer;

  property LastError: string read FLastError;
end;

implementation

{$IFDEF WINDOWS}
const
  // Windows touch API constants
TOUCH_MASK_CONTACTAREA = $0004;
TOUCH_MASK_PRESSURE    = $0002;
TWF_WANTPALM          = $00000002;

type
TTouchInput = record
  x: integer;
  y: integer;
  hSource: THandle;
  dwID: DWORD;
  dwFlags: DWORD;
  dwMask: DWORD;
  dwTime: DWORD;
  dwExtraInfo: ULONG_PTR;
  cxContact: DWORD;
  cyContact: DWORD;
end;
PTouchInput = ^TTouchInput;

function GetTouchInputInfo(hTouchInput: THandle; cInputs: UINT;
pInputs: PTouchInput; cbSize: integer): BOOL; stdcall; external 'user32.dll';
function RegisterTouchWindow(hwnd: HWND; ulFlags: ULONG): BOOL; stdcall; external 'user32.dll';
function CloseTouchInputHandle(hTouchInput: THandle): BOOL; stdcall; external 'user32.dll';
{$ENDIF}

{ TTouchDetector }

constructor TTouchDetector.Create;
begin
  inherited Create;
  FLastError := '';

  {$IFDEF WINDOWS}
  if not InitializeWindowsTouch then
    FLastError := 'Failed to initialize Windows touch';
  {$ENDIF}
end;

destructor TTouchDetector.Destroy;
begin
  inherited Destroy;
end;

function TTouchDetector.GetActiveTouchCount: integer;
var
  Info: TTouchInfo;
begin
  Info := GetTouchInfo;
  Result := Info.Count;
end;

{$IFDEF WINDOWS}
function TTouchDetector.InitializeWindowsTouch: boolean;
var
  Wnd: HWND;
begin
  Result := false;
  Wnd := GetActiveWindow;
  if Wnd <> 0 then
    Result := RegisterTouchWindow(Wnd, TWF_WANTPALM);
end;

function TTouchDetector.GetWindowsTouchInfo: TTouchInfo;
var
  TouchInput: array[0..63] of TTouchInput;
  TouchHandle: THandle;
  i: integer;
begin
  Result.Count := 0;
  SetLength(Result.Points, 64);

  TouchHandle := GetMessageExtraInfo;
  if TouchHandle = 0 then
  begin
    FLastError := 'No touch input available';
    Exit;
  end;

  // Get the touch input information - this fills the TouchInput array
  if GetTouchInputInfo(TouchHandle, Length(TouchInput), @TouchInput[0], SizeOf(TTouchInput)) then
    for i := 0 to High(TouchInput) do
      if (TouchInput[i].dwFlags and $0001) > 0 then  // TOUCHEVENTF_DOWN
      begin
        Result.Points[Result.Count].X := TouchInput[i].x;
        Result.Points[Result.Count].Y := TouchInput[i].y;
        Result.Points[Result.Count].ID := TouchInput[i].dwID;
        Result.Points[Result.Count].Pressure := 1.0;
        Result.Points[Result.Count].IsValid := true;
        Inc(Result.Count);
      end// Process the touch inputs that were written to the TouchInput array
  ;

  // Always close the touch input handle
//  CloseTouchInputHandle(TouchHandle); No such function
end;
{$ENDIF}

{$IFDEF LINUX}
function TTouchDetector.FindTouchDevice: string;
var
  F: Text;
  Line, EventNum: string;
  Found: boolean;
begin
  Result := '';
  Found := false;

  if not FileExists('/proc/bus/input/devices') then
  begin
    FLastError := 'Cannot find input devices file';
    Exit;
  end;

  AssignFile(F, '/proc/bus/input/devices');
  try
    Reset(F);
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      if Pos('Touchscreen', Line) > 0 then
      begin
        Found := true;
        Continue;
      end;

      if Found and (Pos('Handlers=', Line) > 0) then
      begin
        EventNum := Copy(Line, Pos('event', Line), 7);
        Result := '/dev/input/' + EventNum;
        Break;
      end;
    end;
  finally
    CloseFile(F);
  end;

  if Result = '' then
    FLastError := 'No touchscreen device found';
end;

function TTouchDetector.GetLinuxTouchInfo: TTouchInfo;
var
  DevicePath: string;
  SlotPath: string;
  F: Text;
  i: integer;
  Line: string;
begin
  Result.Count := 0;
  SetLength(Result.Points, 64);  // Maximum supported touch points

  DevicePath := FindTouchDevice;
  if DevicePath = '' then
    Exit;

  // Base path for multi-touch slots
  SlotPath := '/sys/class/input/event0/device/mt/slots';

  if DirectoryExists(SlotPath) then
    for i := 0 to 63 do
    begin
      if not FileExists(SlotPath + '/slot' + IntToStr(i) + '/tracking_id') then
        Continue;

      AssignFile(F, SlotPath + '/slot' + IntToStr(i) + '/tracking_id');
      try
        Reset(F);
        ReadLn(F, Line);
        if StrToIntDef(Line, -1) >= 0 then
        begin
          Result.Points[Result.Count].ID := i;
          Result.Points[Result.Count].IsValid := true;

          // Try to read position if available
          if FileExists(SlotPath + '/slot' + IntToStr(i) + '/position_x') then
          begin
            AssignFile(F, SlotPath + '/slot' + IntToStr(i) + '/position_x');
            Reset(F);
            ReadLn(F, Line);
            Result.Points[Result.Count].X := StrToIntDef(Line, 0);
            CloseFile(F);
          end;

          if FileExists(SlotPath + '/slot' + IntToStr(i) + '/position_y') then
          begin
            AssignFile(F, SlotPath + '/slot' + IntToStr(i) + '/position_y');
            Reset(F);
            ReadLn(F, Line);
            Result.Points[Result.Count].Y := StrToIntDef(Line, 0);
            CloseFile(F);
          end;

          // Try to read pressure if available
          if FileExists(SlotPath + '/slot' + IntToStr(i) + '/pressure') then
          begin
            AssignFile(F, SlotPath + '/slot' + IntToStr(i) + '/pressure');
            Reset(F);
            ReadLn(F, Line);
            Result.Points[Result.Count].Pressure := StrToFloatDef(Line, 1.0);
            CloseFile(F);
          end
          else
            Result.Points[Result.Count].Pressure := 1.0;

          Inc(Result.Count);
        end;
      finally
        CloseFile(F);
      end;
    end;
end;
{$ENDIF}

function TTouchDetector.GetTouchInfo: TTouchInfo;
begin
  {$IFDEF WINDOWS}
  Result := GetWindowsTouchInfo;
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetLinuxTouchInfo;
  {$ENDIF}

  {$IFDEF DARWIN}
  Result.Count := 0;
  SetLength(Result.Points, 0);
  FLastError := 'Touch detection not implemented for macOS';
  {$ENDIF}
end;

end.
