unit ExtCtrls;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Controls, Classes, Graphics, Menus;

type
  TPanel = class(TWinControl)
  end;

  TTimer = class(TComponent)
  public
    Interval: integer;
    Enabled: boolean;
    OnTimer: TNotifyEvent;
  end;

  TTrayIcon = class(TComponent)
  private
    FHint: string;
    FVisible: Boolean;
    FIcon: TIcon;
    FOnClick: TNotifyEvent;
    FPopupMenu: TPopupMenu;
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    property Hint: string read FHint write FHint;
    property Visible: Boolean read FVisible write FVisible;
    property Icon: TIcon read FIcon write FIcon;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property PopUpMenu: TPopupMenu read FPopupMenu write FPopupMenu;
  end;

  TImage = class(TControl)
  private
    FPicture: TPicture;
  public
    constructor Create(AOwner: Controls.TComponent = nil); override;
    destructor Destroy; override;
    property Picture: TPicture read FPicture write FPicture;
  end;

  TPaintBox = class(TControl)
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  end;

  TBevel = class(TControl)
  public
    Shape: Integer;
  end;

implementation

constructor TTrayIcon.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FHint := '';
  FVisible := False;
  FIcon := TIcon.Create;
  FOnClick := nil;
  FPopupMenu := nil;
end;

destructor TTrayIcon.Destroy;
begin
  if Assigned(FIcon) then
    FIcon.Free;
  inherited Destroy;
end;

constructor TImage.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
end;

destructor TImage.Destroy;
begin
  if Assigned(FPicture) then
    FPicture.Free;
  inherited Destroy;
end;

constructor TPaintBox.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  if FCanvas = nil then
    FCanvas := TCanvas.Create;
end;

destructor TPaintBox.Destroy;
begin
  // Do not free FCanvas here — TControl.Destroy frees it. Avoid double-free.
  inherited Destroy;
end;

end.
