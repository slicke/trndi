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
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    property Caption: string read FCaption write FCaption;
    property Font: TFont read FFont write FFont;
    property Alignment: Graphics.TAlignment read FAlignment write FAlignment;
    property Layout: Graphics.TTextLayout read FLayout write FLayout;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
  end;

  TButton = class(TControl)
  end;

  TCheckBox = class(TControl)
  public
    Checked: Boolean;
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
  public
    Checked: Boolean;
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
    property SelectedText: string read GetSelectedText;
  end;

  
  TColorButton = class(TControl)
  private
    FColor: Graphics.TColor;
    FButtonColor: Graphics.TColor;
  public
    property Color: Graphics.TColor read FColor write FColor;
    property ButtonColor: Graphics.TColor read FButtonColor write FButtonColor;
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

procedure TListBox.AddItem(const S: string; AObject: TObject);
begin
  Items.Add(S);
end;

procedure TListBox.DeleteSelected;
begin
  if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    Items.Delete(ItemIndex);
end;


end.
