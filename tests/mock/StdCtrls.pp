unit StdCtrls;

{$mode ObjFPC}{$H+}

interface

uses Controls, Graphics;

type

  TLabel = class(TControl)
  private
    FCaption: string;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FWordWrap: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Caption: string read FCaption write FCaption;
    property Font: TFont read FFont write FFont;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Layout: TTextLayout read FLayout write FLayout;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
  end;

  TButton = class(TControl)
  end;

implementation

constructor TLabel.Create;
begin
  inherited Create;
  FAlignment := taLeftJustify;
  FLayout := tlTop;
  FWordWrap := False;
  // Ensure a canvas exists for drawing helpers
  if FCanvas = nil then
    FCanvas := TCanvas.Create;
end;

destructor TLabel.Destroy;
begin
  if Assigned(FCanvas) then
    FCanvas.Free;
  inherited Destroy;
end;

end.
