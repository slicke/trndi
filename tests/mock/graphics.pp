unit Graphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types;

type
  // Minimal TColor definition compatible with existing code
  TColor = LongInt;

const
  clBlack   = $00000000;
  clWhite   = $00FFFFFF;
  clRed     = $000000FF;
  clFuchsia = $00FF00FF;
  clNone    = -1; // used as transparent sentinel
  clDefault = clWhite;

type
  TBrushStyle = (bsSolid, bsClear);
  TPenStyle = (psSolid);
  TFontStyle = (fsBold);
  TFontStyles = set of TFontStyle;

  TPen = class(TPersistent)
  private
    FColor: TColor;
    FWidth: Integer;
    FStyle: TPenStyle;
  public
    property Color: TColor read FColor write FColor;
    property Width: Integer read FWidth write FWidth;
    property Style: TPenStyle read FStyle write FStyle;
  end;

  TBrush = class(TPersistent)
  private
    FColor: TColor;
    FStyle: TBrushStyle;
  public
    property Color: TColor read FColor write FColor;
    property Style: TBrushStyle read FStyle write FStyle;
  end;

  TFont = class(TPersistent)
  private
    FName: string;
    FSize: Integer;
    FColor: TColor;
    FStyle: TFontStyles;
  public
    property Name: string read FName write FName;
    property Size: Integer read FSize write FSize;
    property Color: TColor read FColor write FColor;
    property Style: TFontStyles read FStyle write FStyle;
    constructor Create; virtual;
  end;

  // Alignment/layout types used across widgets
  TAlignment = (taLeftJustify, taRightJustify, taCenter);
  TTextLayout = (tlTop, tlCenter, tlBottom);

  // Minimal text style used by Canvas.TextStyle
  TTextStyle = record
    Alignment: TAlignment;
    Layout: TTextLayout;
    Wordbreak: Boolean;
    SingleLine: Boolean;
    Clipping: Boolean;
  end;

  TCanvas = class(TObject)
  private
    FPen: TPen;
    FBrush: TBrush;
    FFont: TFont;
    FHandle: PtrUInt;
    FTextStyle: TTextStyle;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Pen: TPen read FPen;
    property Brush: TBrush read FBrush;
    property Font: TFont read FFont write FFont;
    property Handle: PtrUInt read FHandle;
    property TextStyle: TTextStyle read FTextStyle write FTextStyle;

    function TextWidth(const S: string): Integer; virtual;
    function TextHeight(const S: string): Integer; virtual;
    procedure TextOut(X, Y: Integer; const S: string); virtual;
    procedure TextRect(const R: TRect; X, Y: Integer; const S: string; const AStyle: TTextStyle); overload; virtual;
    procedure TextRect(const R: TRect; X, Y: Integer; const S: string); overload; virtual;
    procedure FillRect(const R: TRect); overload; virtual;
    procedure FillRect(X1, Y1, X2, Y2: Integer); overload; virtual;
    procedure RoundRect(const R: TRect; RoundX, RoundY: Integer); overload; virtual;
    procedure RoundRect(X1, Y1, X2, Y2, RoundX, RoundY: Integer); overload; virtual;
    procedure MoveTo(X, Y: Integer); virtual;
    procedure LineTo(X, Y: Integer); virtual;
    function Pixels(X, Y: Integer): TColor; virtual;
    procedure Draw(X, Y: Integer; Src: TObject); virtual;
  end;

  TIcon = TObject;

  TPicture = class(TObject)
  private
    FIcon: TIcon;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Icon: TIcon read FIcon write FIcon;
  end;

  TBitmap = class(TObject)
  private
    FCanvas: TCanvas;
    FWidth: Integer;
    FHeight: Integer;
    FTransparentColor: TColor;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor;
    procedure LoadFromResourceName(Instance: THandle; const AName: string); virtual;
  end;

// color helpers
function ColorToRGB(AColor: TColor): LongInt;
function RGB(R, G, B: Byte): TColor;
function RGBToColor(R, G, B: Byte): TColor;
function Red(AColor: TColor): Byte;
function Green(AColor: TColor): Byte;
function Blue(AColor: TColor): Byte;

// Windows-like macros used in project
function GetRValue(C: LongInt): Byte;
function GetGValue(C: LongInt): Byte;
function GetBValue(C: LongInt): Byte;

const
  clWindow = $00FFFFFF; // white
  clWindowText = $00000000; // black

implementation

{ TFont }
constructor TFont.Create;
begin
  inherited Create;
  FName := 'default';
  FSize := 10;
  FColor := clBlack;
  FStyle := [];
end;

{ TCanvas }
constructor TCanvas.Create;
begin
  inherited Create;
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FHandle := 0;
end;

destructor TCanvas.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  FFont.Free;
  inherited Destroy;
end;

function TCanvas.TextWidth(const S: string): Integer;
begin
  // simple approximation: 8px per char
  Result := Length(S) * 8;
end;

function TCanvas.TextHeight(const S: string): Integer;
begin
  // approximate using font size if set
  if FFont.Size > 0 then
    Result := FFont.Size
  else
    Result := 12;
end;

procedure TCanvas.TextOut(X, Y: Integer; const S: string);
begin
  // no-op for headless
end;

procedure TCanvas.TextRect(const R: TRect; X, Y: Integer; const S: string; const AStyle: TTextStyle);
begin
  // no-op
end;

procedure TCanvas.TextRect(const R: TRect; X, Y: Integer; const S: string);
begin
  // no-op
end;

procedure TCanvas.FillRect(const R: TRect);
begin
  // no-op
end;

procedure TCanvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  // no-op
end;

procedure TCanvas.RoundRect(const R: TRect; RoundX, RoundY: Integer);
begin
  // no-op
end;

procedure TCanvas.RoundRect(X1, Y1, X2, Y2, RoundX, RoundY: Integer);
begin
  // no-op
end;

procedure TCanvas.MoveTo(X, Y: Integer);
begin
  // no-op
end;

procedure TCanvas.LineTo(X, Y: Integer);
begin
  // no-op
end;

function TCanvas.Pixels(X, Y: Integer): TColor;
begin
  // default transparent / background
  Result := clWhite;
end;

procedure TCanvas.Draw(X, Y: Integer; Src: TObject);
begin
  // no-op
end;

{ TBitmap }
constructor TPicture.Create;
begin
  inherited Create;
  FIcon := nil;
end;

destructor TPicture.Destroy;
begin
  // no-op
  inherited Destroy;
end;

constructor TBitmap.Create;
begin
  inherited Create;
  FCanvas := TCanvas.Create;
  FWidth := 0;
  FHeight := 0;
  FTransparentColor := clNone;
end;

destructor TBitmap.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TBitmap.LoadFromResourceName(Instance: THandle; const AName: string);
begin
  // no-op
end;

{ Color helpers }
function ColorToRGB(AColor: TColor): LongInt;
begin
  // Mask alpha component, return as 24-bit RGB (0x00BBGGRR)
  Result := AColor and $00FFFFFF;
end;

function Red(AColor: TColor): Byte;
begin
  Result := AColor and $FF;
end;

function Green(AColor: TColor): Byte;
begin
  Result := (AColor shr 8) and $FF;
end;

function Blue(AColor: TColor): Byte;
begin
  Result := (AColor shr 16) and $FF;
end;

function RGB(R, G, B: Byte): TColor;
begin
  // produce $00BBGGRR as project expects
  Result := (B shl 16) or (G shl 8) or R;
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := RGB(R, G, B);
end;

// Windows-style helpers
function GetRValue(C: LongInt): Byte;
begin
  Result := Red(ColorToRGB(C));
end;

function GetGValue(C: LongInt): Byte;
begin
  Result := Green(ColorToRGB(C));
end;

function GetBValue(C: LongInt): Byte;
begin
  Result := Blue(ColorToRGB(C));
end;

end.