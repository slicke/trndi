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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-08-21: Parenting now tracks children so ControlCount/Controls[]
 *   (used by umain's shutdown-screen sweep) enumerate something real.
 *)
(* MODIFICATION NOTICE (2026-08-05): Added Screen.Fonts to mirror the LCL
   screen font list used by Linux-specific UI initialization in headless tests. *)
unit Controls;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Types, Graphics, Classes;

type
  // Make Controls.TComponent inherit from Classes.TComponent to keep compatibility
  // with code expecting Classes.TComponent
  TComponent = class(Classes.TComponent)
  end;

  TWinControl = class;

  TNotifyEvent = procedure(Sender: TObject) of object;

  // Minimal mouse and shift state types used in event signatures
  TMouseButton = (mbLeft, mbRight, mbMiddle);
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl, ssMeta, ssLeft, ssRight, ssMiddle);
  TShiftState = set of TShiftStateEnum;

  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer) of object;

  // Mirrors LCL's CM_HITTEST plumbing (controls.pp/controlconsts.inc): the
  // trend surface overrides the hit-test message handler to stay mouse-
  // transparent between the dots. The mock never dispatches the message —
  // the declaration only has to compile.
  TCMHitTest = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Unused: PtrInt;
    XPos: SmallInt;
    YPos: SmallInt;
    Result: PtrInt;
  end;

const
  CM_HITTEST = $B000 + 10; // matches LCL's CM_BASE + 10

type

  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);

  TControlStyleType = (csOpaque, csClickEvents, csCaptureMouse);
  TControlStyle = set of TControlStyleType;

  TControl = class(TComponent)
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FParent: TWinControl;
    FCaption: string;
    FName: string;
    FEnabled: Boolean;
    FVisible: Boolean;
    FColor: TColor;
    FAlign: TAlign;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FCursor: Integer;
    FAutoSize: Boolean;
    FPopupMenu: TComponent;
    FOnPaint: TNotifyEvent;
    FHint: string;
    FOnResize: TNotifyEvent;
    FOptimalFill: Boolean;
    FHandle: PtrUInt; // Mock window handle for Windows-specific code
    FAnchors: TAnchors;
    FShowHint: Boolean;
    procedure SetParentControl(AValue: TWinControl);
  protected
    FCanvas: TCanvas;
    FFont: TFont;
    FControlStyle: TControlStyle;
    property ControlStyle: TControlStyle read FControlStyle write FControlStyle;
  public
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    constructor Create(AOwner: TComponent = nil); virtual;
    destructor Destroy; override;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Parent: TWinControl read FParent write SetParentControl;
    property Canvas: TCanvas read FCanvas;
    property Font: TFont read FFont write FFont;
    property Caption: string read FCaption write FCaption;
    property Name: string read FName write FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Color: TColor read FColor write FColor;
    property Align: TAlign read FAlign write FAlign;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property Visible: Boolean read FVisible write FVisible;
    property Cursor: Integer read FCursor write FCursor;
    property PopupMenu: TComponent read FPopupMenu write FPopupMenu;
    property Hint: string read FHint write FHint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OptimalFill: Boolean read FOptimalFill write FOptimalFill;
    property Handle: PtrUInt read FHandle write FHandle; // Provide a mock Handle for Windows-specific APIs
    property Anchors: TAnchors read FAnchors write FAnchors;
    property ShowHint: Boolean read FShowHint write FShowHint;
    function ClientRect: TRect; virtual;
    function GetClientWidth: Integer; virtual;
    function GetClientHeight: Integer; virtual;
    procedure SetClientWidth(AValue: Integer); virtual;
    procedure SetClientHeight(AValue: Integer); virtual;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
    procedure Hide; virtual;
    procedure Show; virtual;
    procedure SetFocus; virtual;
    procedure Update; virtual;
    // Real LCL declares AdjustSize on TControl, so any control may be asked to
    // re-run auto-sizing. Declared here (not just on TLabel) so mock TPaintBox
    // and friends match: umain's macOS-only trend-dot path calls it on a
    // TDotControl, which is a TPaintBox.
    procedure AdjustSize; virtual;
    procedure Repaint; virtual;
    procedure Refresh; virtual;
    procedure SendToBack; virtual;

    // Bounds helpers
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    function GetBoundsRect: TRect; virtual;
    procedure SetBoundsRect(const AValue: TRect); virtual;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;

    // Basic UI event hooks commonly overridden by forms/controls
    procedure Paint; virtual;
    procedure Resize; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;

    procedure Invalidate; virtual;
    procedure BringToFront; virtual;
    // Headless: no screen, so control and screen coordinates coincide
    function ScreenToClient(const P: TPoint): TPoint; virtual;
  end;


  TWinControl = class(TControl)
  private
    FChildControls: TList; // children reference themselves here; not owned
    function GetControlCount: Integer;
    function GetControlByIndex(Index: Integer): TControl;
  public
    destructor Destroy; override;
    procedure AddChildControl(AControl: TControl);
    procedure RemoveChildControl(AControl: TControl);
    // Real LCL child enumeration used by umain's shutdown-screen sweep
    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TControl read GetControlByIndex;
  end;

  // Windowless control; in the real LCL it paints onto the parent's canvas.
  TGraphicControl = class(TControl)
  end;

  // Owner-draw state and drag object used in event signatures
  TOwnerDrawStateEnum = (odSelected, odFocused, odDisabled, odChecked, odGrayed, odDefault, odHotLight, odInactive, odNoAccel);
  TOwnerDrawState = set of TOwnerDrawStateEnum;

  TDragObject = class(TObject)
  public
    Source: TObject;
  end;

  // Minimal Monitor class used by some units (matches LCL's TMonitor)
  TMonitor = class
  public
    BoundsRect: TRect;
    WorkAreaRect: TRect;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  // Minimal Screen record used by some units
  TScreen = record
    Width: Integer;
    Height: Integer;
    Cursor: Integer;
    // Work area and desktop properties used by umain helpers
    WorkAreaLeft: Integer;
    WorkAreaTop: Integer;
    WorkAreaWidth: Integer;
    WorkAreaHeight: Integer;
    WorkAreaRect: TRect;
    DesktopLeft: Integer;
    DesktopTop: Integer;
    DesktopWidth: Integer;
    DesktopHeight: Integer;
    // Multi-monitor support (headless defaults to single monitor)
    MonitorCount: Integer;
    Monitors: array of TMonitor;
    ActiveForm: TObject;
    // Menu font used by owner-draw menu code paths
    MenuFont: TFont;
    // Installed font names exposed by the real LCL screen object.
    Fonts: TStringList;
  end;

var
  Screen: TScreen;

implementation

var
  _MockMonitorI: Integer; // used in finalization to clean up monitors

constructor TControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := nil;
  FFont := TFont.Create;
  FOnResize := nil; // default no-op event
  FOptimalFill := False;
  FHandle := 0; // default mock handle
  FAnchors := [akLeft, akTop];
  FShowHint := False;
end;

constructor TMonitor.Create;
begin
  inherited Create;
  BoundsRect := Rect(0,0,0,0);
  WorkAreaRect := Rect(0,0,0,0);
end;

destructor TMonitor.Destroy;
begin
  inherited Destroy;
end;

destructor TControl.Destroy;
begin
  if Assigned(FParent) then
    FParent.RemoveChildControl(Self);
  if Assigned(FCanvas) then
    FCanvas.Free;
  if Assigned(FFont) then
    FFont.Free;
  inherited Destroy;
end;

procedure TControl.SetParentControl(AValue: TWinControl);
begin
  if FParent = AValue then
    Exit;
  if Assigned(FParent) then
    FParent.RemoveChildControl(Self);
  FParent := AValue;
  if Assigned(FParent) then
    FParent.AddChildControl(Self);
end;

{ TWinControl }

destructor TWinControl.Destroy;
begin
  FChildControls.Free; // entries are not owned
  FChildControls := nil;
  inherited Destroy;
end;

procedure TWinControl.AddChildControl(AControl: TControl);
begin
  if FChildControls = nil then
    FChildControls := TList.Create;
  if FChildControls.IndexOf(AControl) < 0 then
    FChildControls.Add(AControl);
end;

procedure TWinControl.RemoveChildControl(AControl: TControl);
begin
  if Assigned(FChildControls) then
    FChildControls.Remove(AControl);
end;

function TWinControl.GetControlCount: Integer;
begin
  if Assigned(FChildControls) then
    Result := FChildControls.Count
  else
    Result := 0;
end;

function TWinControl.GetControlByIndex(Index: Integer): TControl;
begin
  Result := TControl(FChildControls[Index]);
end;

function TControl.ClientRect: TRect;
begin
  Result := Rect(Left, Top, Left + Width, Top + Height);
end;

function TControl.GetClientWidth: Integer;
begin
  Result := Width;
end;

procedure TControl.SetClientWidth(AValue: Integer);
begin
  Width := AValue;
end;

function TControl.GetClientHeight: Integer;
begin
  Result := Height;
end;

procedure TControl.SetClientHeight(AValue: Integer);
begin
  Height := AValue;
end;

procedure TControl.Hide;
begin
  Visible := False;
end;

procedure TControl.Show;
begin
  Visible := True;
end;

procedure TControl.SetFocus;
begin
  // no-op for headless tests
end;

procedure TControl.AdjustSize;
begin
  // In real LCL this re-runs auto-sizing, which leaves a control whose AutoSize
  // is off exactly as it was — the case for every mock control that does not
  // override this. TLabel overrides it to measure its caption.
end;

procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  Left := ALeft;
  Top := ATop;
  Width := AWidth;
  Height := AHeight;
end;

function TControl.GetBoundsRect: TRect;
begin
  Result := Rect(Left, Top, Left + Width, Top + Height);
end;

procedure TControl.SetBoundsRect(const AValue: TRect);
begin
  SetBounds(AValue.Left, AValue.Top,
    AValue.Right - AValue.Left, AValue.Bottom - AValue.Top);
end;

procedure TControl.MouseEnter;
begin
end;

procedure TControl.MouseLeave;
begin
end;

procedure TControl.Invalidate;
begin
  // Default to repainting in headless tests
  Repaint;
end;

procedure TControl.Refresh;
begin
  // no-op for headless tests (explicit refresh)
end;

procedure TControl.SendToBack;
begin
  // no-op for headless tests
end;

procedure TControl.BringToFront;
begin
  // no-op in headless tests
end;

function TControl.ScreenToClient(const P: TPoint): TPoint;
begin
  Result := P;
end;

procedure TControl.Update;
begin
  // no-op
end;

procedure TControl.Repaint;
begin
  Paint;
end;

procedure TControl.Paint;
begin
  // no-op for headless tests
end;

procedure TControl.Resize;
begin
  // no-op for headless tests
end;

procedure TControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // no-op for headless tests
end;

procedure TControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // no-op for headless tests
end;

initialization
  Screen.Width := 1024;
  Screen.Height := 768;
  // Default work area and desktop to whole screen in headless tests
  Screen.WorkAreaLeft := 0;
  Screen.WorkAreaTop := 0;
  Screen.WorkAreaWidth := Screen.Width;
  Screen.WorkAreaHeight := Screen.Height;
  Screen.WorkAreaRect := Rect(Screen.WorkAreaLeft, Screen.WorkAreaTop, Screen.WorkAreaLeft + Screen.WorkAreaWidth, Screen.WorkAreaTop + Screen.WorkAreaHeight);
  Screen.DesktopLeft := 0;
  Screen.DesktopTop := 0;
  Screen.DesktopWidth := Screen.Width;
  Screen.DesktopHeight := Screen.Height;
  // Default single monitor setup for headless tests
  Screen.MonitorCount := 1;
  SetLength(Screen.Monitors, 1);
  Screen.Monitors[0] := TMonitor.Create;
  Screen.Monitors[0].BoundsRect := Rect(0, 0, Screen.Width, Screen.Height);
  Screen.Monitors[0].WorkAreaRect := Screen.WorkAreaRect;
  Screen.ActiveForm := nil;
  Screen.MenuFont := TFont.Create;
  Screen.Fonts := TStringList.Create;

finalization
  if Assigned(Screen.Fonts) then
  begin
    Screen.Fonts.Free;
    Screen.Fonts := nil;
  end;
  if Assigned(Screen.MenuFont) then
  begin
    Screen.MenuFont.Free;
    Screen.MenuFont := nil;
  end;
  // Free any mock monitors created
  if Screen.MonitorCount > 0 then
  begin
    for _MockMonitorI := 0 to Screen.MonitorCount - 1 do
      if Assigned(Screen.Monitors[_MockMonitorI]) then
        Screen.Monitors[_MockMonitorI].Free;
    SetLength(Screen.Monitors, 0);
    Screen.MonitorCount := 0;
  end;

end.
