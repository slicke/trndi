unit StdCtrls;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Controls, Graphics, Classes;

type

  TLabel = class(TControl)
  private
    FCaption: string;
    FAlignment: Graphics.TAlignment;
    FLayout: Graphics.TTextLayout;
    FWordWrap: Boolean;
    FAutoSize: Boolean;
    FTransparent: Boolean;
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    procedure AdjustSize; // macOS AutoSize helper
    property Caption: string read FCaption write FCaption;
    property Font: TFont read FFont write FFont;
    property Alignment: Graphics.TAlignment read FAlignment write FAlignment;
    property Layout: Graphics.TTextLayout read FLayout write FLayout;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property Transparent: Boolean read FTransparent write FTransparent;
  end;

  TButton = class(TControl)
  end;

  TCheckBox = class(TControl)
  private
    FChecked: Boolean;
  public
    property Checked: Boolean read FChecked write FChecked;
  end;

  TComboBox = class(TWinControl)
  public
    Items: TStringList;
    ItemIndex: Integer;
    Text: string;
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
  end;

  TEdit = class(TControl)
  private
    FOnChange: TNotifyEvent;
  public
    Text: string;
    PasswordChar: WideChar;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCheckGroup = class(TWinControl)
  public
    Items: TStringList;
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
  end;

  // Additional mocked controls used by uconf
  TGroupBox = class(TWinControl)
  end;

  TRadioButton = class(TControl)
  private
    FChecked: Boolean;
  public
    property Checked: Boolean read FChecked write FChecked;
  end;

  TRadioGroup = class(TWinControl)
  public
    Items: TStringList;
    ItemIndex: Integer;
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    property ItemsList: TStringList read Items;
  end;


  TListBox = class(TWinControl)
  public
    Items: TStringList;
    ItemIndex: Integer;
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    function GetSelectedText: string; virtual;
    procedure AddItem(const S: string; AObject: TObject);
    procedure DeleteSelected;
    procedure Clear;
    function GetCount: Integer;
    property SelectedText: string read GetSelectedText;
    property Count: Integer read GetCount;
  end;

  
  TColorButton = class(TControl)
  private
    FColor: Graphics.TColor;
    FButtonColor: Graphics.TColor;
    FChecked: Boolean;
  public
    property Color: Graphics.TColor read FColor write FColor;
    property ButtonColor: Graphics.TColor read FButtonColor write FButtonColor;
    property Checked: Boolean read FChecked write FChecked;
  end;

implementation

constructor TComboBox.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  Items := TStringList.Create;
end;

destructor TComboBox.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

constructor TCheckGroup.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  Items := TStringList.Create;
end;

destructor TCheckGroup.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

constructor TLabel.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FAlignment := Graphics.taLeftJustify;
  FLayout := Graphics.tlTop;
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

procedure TLabel.AdjustSize;
begin
  // Measure caption and resize control accordingly. Keep simple for tests.
  if Assigned(FCanvas) then
  begin
    // Ensure the canvas uses the label's font for measurement
    FCanvas.Font.Assign(FFont);
    // Basic single-line sizing. WordWrap and complex layout ignored for mocks.
    Width := FCanvas.TextWidth(FCaption);
    Height := FCanvas.TextHeight(FCaption);
    // Minimal padding to avoid zero-size
    if Width < 1 then Width := 1;
    if Height < 1 then Height := 1;
  end;
end;

constructor TRadioGroup.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  Items := TStringList.Create;
end;

destructor TRadioGroup.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

constructor TListBox.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  Items := TStringList.Create;
  ItemIndex := -1;
end;

destructor TListBox.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

function TListBox.GetSelectedText: string;
begin
  if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    Result := Items[ItemIndex]
  else
    Result := '';
end;

function TListBox.GetCount: Integer;
begin
  Result := Items.Count;
end;

procedure TListBox.AddItem(const S: string; AObject: TObject);
begin
  Items.Add(S);
end;

procedure TListBox.Clear;
begin
  Items.Clear;
end;

procedure TListBox.DeleteSelected;
begin
  if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    Items.Delete(ItemIndex);
end;


end.
