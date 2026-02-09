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
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle);
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
    property Font: TFont read FFont;
    property Caption: string read FCaption write FCaption;
    property Name: string read FName write FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Color: TColor read FColor write FColor;
    property Align: TAlign read FAlign write FAlign;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Visible: Boolean read FVisible write FVisible;
    property Cursor: Integer read FCursor write FCursor;
    function ClientRect: TRect; virtual;
    function ClientWidth: Integer; virtual;
    function ClientHeight: Integer; virtual;
    procedure Hide; virtual;
    procedure Show; virtual;
    procedure SetFocus; virtual;
    procedure Update; virtual;
    procedure Repaint; virtual;

    // Basic UI event hooks commonly overridden by forms/controls
    procedure Paint; virtual;
    procedure Resize; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
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

function TControl.ClientWidth: Integer;
begin
  Result := Width;
end;

function TControl.ClientHeight: Integer;
begin
  Result := Height;
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
