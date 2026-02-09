unit Controls;

{$mode ObjFPC}{$H+}

interface

uses Types, Graphics;

type
  TComponent = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TWinControl = class;

  TControl = class(TComponent)
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FParent: TWinControl;
    FCaption: string;
    FName: string;
  protected
    FCanvas: TCanvas;
    FFont: TFont;
  public
    constructor Create; override;
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
    function ClientRect: TRect; virtual;
    function ClientWidth: Integer; virtual;
    function ClientHeight: Integer; virtual;
  end;

  TWinControl = class(TControl)
  end;

  // Minimal Screen record used by some units
  TScreen = record
    Width: Integer;
    Height: Integer;
  end;

var
  Screen: TScreen;

implementation

constructor TComponent.Create;
begin
  inherited Create;
end;

destructor TComponent.Destroy;
begin
  inherited Destroy;
end;

constructor TControl.Create;
begin
  inherited Create;
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

initialization
  Screen.Width := 1024;
  Screen.Height := 768;

end.
