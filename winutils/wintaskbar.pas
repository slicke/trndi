{*******************************************************************************
Windows TaskBar Progress unit
Originally by Alexey Torgashin:
https://github.com/Alexey-T/Win32TaskbarProgress

Updated to taskbar 4 support and additional features other than progress by
https://github.com/slicke

License: MIT
*******************************************************************************}

{
## NOTE ##
To use any buttons, your main form must implement (also use "messages" and "shlobj" (or reference the btn as $1800):
protected
   procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;

procedure <form>.WMCommand(var Message: TWMCommand);
  begin
    if Message.NotifyCode = THBN_CLICKED then
      // Handle Message.ItemID (= button id)
    inherited;
  end;

If privileged, you may need to also add
function ChangeWindowMessageFilter(msg: Cardinal; action: Word): BOOL; stdcall;
  external 'user32.dll';
}
unit wintaskbar;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ShlObj, ComObj, Forms, Graphics, Windows,
  Dialogs (* Remove! *);

type
  TTaskBarProgressStyle = (tbpsNone, tbpsAnimation, tbpsNormal, tbpsError, tbpsPause);
  TThumbButton = THUMBBUTTON;
  TThumbButtons = array of TThumbButton;

  { TWinTaskBar }

  TWinTaskBar = class
  private
    FHandle: THandle;
    FMin: Integer;
    FMax: Integer;
    FValue: Integer;
    FStyle: TTaskBarProgressStyle;
    FIntf: ITaskbarList4;
    FIcon: TIcon;
    procedure SetProgress(const AValue: Integer);
    procedure SetMax(const AValue: Integer);
    procedure SetStyle(const AValue: TTaskBarProgressStyle);
    procedure SetIcon(const AValue: TIcon);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetButtons(var ThumbButtons: TThumbButtons);
    procedure SetButtons(var ThumbButton: TThumbButton);

    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin;
    property Style: TTaskBarProgressStyle read FStyle write SetStyle;
    property Progress: Integer read FValue write SetProgress;
    property Icon: TIcon read FIcon write SetIcon;

  end;

var
  GlobalTaskbarProgress: TWinTaskBar = nil;
  FThumbButtons: array of TThumbButton;

implementation



// Added by slicke
procedure TWinTaskBar.SetButtons(var ThumbButton: TThumbButton);
begin
  SetButtons((ThumbButton));
end;

// Added by slicke
procedure TWinTaskBar.SetButtons(var ThumbButtons: TThumbButtons);
{var
  Button: TThumbButton;
begin
//  ZeroMemory(@FThumbButtons, SizeOf(TThumbButton) * length(ThumbButtons));

  Button.iId := 42;
  Button.iBitmap := 0;
  Button.dwMask := THB_FLAGS or THB_BITMAP or THB_TOOLTIP;
  Button.dwFlags :=  THBF_ENABLED or THBF_NOBACKGROUND;
  StrCopy(Button.szTip, PWideChar('Answer'));
  FIntf.ThumbBarAddButtons(FHandle, 1, @Button);
  ShowMessage('yes?');
                }
var
 i: integer;
begin
  SetLength(FThumbButtons, Length(FThumbButtons) + Length(ThumbButtons));
  for i := 0 to High(ThumbButtons) do begin
     FThumbButtons[i + Length(ThumbButtons)] := ThumbButtons[i];
     if FThumbButtons[i + Length(ThumbButtons)].iId = 999 then
        FThumbButtons[i + Length(ThumbButtons)].iId := i + Length(ThumbButtons) + 10;
  end;

//  FIntf.ThumbBarUpdateButtons(
  FIntf.ThumbBarAddButtons(
  FHandle,
  Length(FThumbButtons), @FThumbButtons);

end;

// Added by slicke
procedure TWinTaskBar.SetIcon(const AValue: TIcon);
begin
 FIcon.AssignImage(AValue);
 FIntf.SetOverlayIcon(FHandle, FIcon.Handle, '');
end;

procedure TWinTaskBar.SetMax(const AValue: Integer);
begin
  FMax := AValue;
  SetProgress(FValue);
end;

procedure TWinTaskBar.SetProgress(const AValue: Integer);
begin
  if (FIntf <> nil) and (FHandle <> 0) then
  begin
    FValue := AValue;
    if FStyle <> tbpsAnimation then
      FIntf.SetProgressValue(FHandle, UInt64(FValue), UInt64(FMax));
  end;
end;

procedure TWinTaskBar.SetStyle(const AValue: TTaskBarProgressStyle);
const
  Flags: array[TTaskBarProgressStyle] of Cardinal = (0, 1, 2, 4, 8);
begin
  if (FIntf <> nil) and (FHandle <> 0) then
    FIntf.SetProgressState(FHandle, Flags[AValue]);

  FStyle := AValue;
end;

constructor TWinTaskBar.Create;
begin

  // ITaskbarList4 is added in Windows 7
  if (Win32BuildNumber < 7600) then exit;

  FHandle:= Application.{%H-}Handle;

  try
    FIntf:= CreateComObject(CLSID_TaskbarList) as ITaskbarList4;

    if FIntf <> nil then
      FIntf.SetProgressState(FHandle, 0);

    FMin := 0;
    FMax := 100;
    FValue := 10;
    FStyle := tbpsNone;
    SetStyle(FStyle);

    FIcon := TIcon.Create;
  except
    FIntf := nil;
  end;
end;


destructor TWinTaskBar.Destroy;
begin
  if (FIntf <> nil) then
  begin
    FIntf.SetProgressState(FHandle, 0);
    FIntf := nil;
    FIcon.Free;
  end;
end;


end.
