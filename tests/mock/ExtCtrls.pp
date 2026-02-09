unit ExtCtrls;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Controls, Classes, Graphics;

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
  public
    property Hint: string read FHint write FHint;
    property Visible: Boolean read FVisible write FVisible;
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
  if Assigned(FCanvas) then
    FCanvas.Free;
  inherited Destroy;
end;

end.
