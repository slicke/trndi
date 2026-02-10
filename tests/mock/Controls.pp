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

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);

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
    FCursor: Integer;
    FAutoSize: Boolean;
    FPopupMenu: TComponent;
    FOnPaint: TNotifyEvent;
    FHint: string;
    FOnResize: TNotifyEvent;
    FOptimalFill: Boolean;
    FHandle: PtrUInt; // Mock window handle for Windows-specific code
  protected
    FCanvas: TCanvas;
    FFont: TFont;
  public
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    constructor Create(AOwner: TComponent = nil); virtual;
    destructor Destroy; override;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Parent: TWinControl read FParent write FParent;
    property Canvas: TCanvas read FCanvas;
    property Font: TFont read FFont write FFont;
    property Caption: string read FCaption write FCaption;
    property Name: string read FName write FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Color: TColor read FColor write FColor;
    property Align: TAlign read FAlign write FAlign;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Visible: Boolean read FVisible write FVisible;
    property Cursor: Integer read FCursor write FCursor;
    property PopupMenu: TComponent read FPopupMenu write FPopupMenu;
    property Hint: string read FHint write FHint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OptimalFill: Boolean read FOptimalFill write FOptimalFill;
    property Handle: PtrUInt read FHandle write FHandle; // Provide a mock Handle for Windows-specific APIs
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
    procedure Repaint; virtual;
    procedure Refresh; virtual;
    procedure SendToBack; virtual;

    // Bounds helpers
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    function GetBoundsRect: TRect; virtual;
    property BoundsRect: TRect read GetBoundsRect;

    // Basic UI event hooks commonly overridden by forms/controls
    procedure Paint; virtual;
    procedure Resize; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;

    procedure Invalidate; virtual;
    procedure BringToFront; virtual;
  end;


  TWinControl = class(TControl)
  end;

  // Owner-draw state and drag object used in event signatures
  TOwnerDrawStateEnum = (odSelected, odFocused, odDisabled, odChecked, odGrayed, odDefault, odHotLight, odInactive);
  TOwnerDrawState = set of TOwnerDrawStateEnum;

  TDragObject = class(TObject)
  public
    Source: TObject;
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
  end; 

var
  Screen: TScreen;

implementation

constructor TControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := nil;
  FFont := TFont.Create;
  FOnResize := nil; // default no-op event
  FOptimalFill := False;
  FHandle := 0; // default mock handle
end;

destructor TControl.Destroy;
begin
  if Assigned(FCanvas) then
    FCanvas.Free;
  if Assigned(FFont) then
    FFont.Free;
  inherited Destroy;
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

end.
