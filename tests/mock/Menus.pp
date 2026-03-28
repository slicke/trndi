unit Menus;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Classes, Controls, SysUtils;

type
  TMenuItem = class(TComponent)
  private
    FCaption: string;
    FOnClick: TNotifyEvent;
    FItems: array of TMenuItem;
    FParent: TMenuItem;
    FShortCut: Integer;
    FEnabled: Boolean;
    FChecked: Boolean;
    FVisible: Boolean;
    FHint: string;
    function GetCount: Integer;
    function GetMenuIndex: Integer;
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    procedure Insert(Index: Integer; Item: TMenuItem);
    function Add(Item: TMenuItem): TMenuItem;
    function GetItem(Index: Integer): TMenuItem;
    procedure Click; virtual;
    property Items[Index: Integer]: TMenuItem read GetItem; default;
    property Caption: string read FCaption write FCaption;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property ShortCut: Integer read FShortCut write FShortCut;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Checked: Boolean read FChecked write FChecked;
    property Visible: Boolean read FVisible write FVisible;
    property Hint: string read FHint write FHint;
    property Count: Integer read GetCount;
    property MenuIndex: Integer read GetMenuIndex;
  end;

  TPopupMenu = class(TComponent)
  private
    FPopupComponent: Controls.TComponent;
    FItems: TMenuItem;
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    procedure PopUp(X, Y: Integer); virtual;
    property PopupComponent: Controls.TComponent read FPopupComponent write FPopupComponent;
    property Items: TMenuItem read FItems;
  end;

  TMainMenu = class(TComponent)
  private
    FItems: TMenuItem;
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    property Items: TMenuItem read FItems;
  end;

implementation

{ TMenuItem }

constructor TMenuItem.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FCaption := '';
  FShortCut := 0;
  FVisible := True;
  FParent := nil;
  SetLength(FItems, 0);
end;

destructor TMenuItem.Destroy;
begin
  // free children
  while Length(FItems) > 0 do
  begin
    FItems[High(FItems)].Free;
    SetLength(FItems, Length(FItems) - 1);
  end;
  inherited Destroy;
end;

procedure TMenuItem.Insert(Index: Integer; Item: TMenuItem);
var
  i: Integer;
begin
  if Index < 0 then Index := 0;
  if Index > Length(FItems) then Index := Length(FItems);
  SetLength(FItems, Length(FItems) + 1);
  for i := High(FItems) - 1 downto Index do
    FItems[i + 1] := FItems[i];
  FItems[Index] := Item;
  // If the item is inserted it might need to adopt visibility from parent
  if Assigned(Item) then
  begin
    Item.FParent := Self;
    Item.Visible := FVisible;
  end;
end;

function TMenuItem.Add(Item: TMenuItem): TMenuItem;
begin
  Insert(Length(FItems), Item);
  Result := Item;
end;

procedure TMenuItem.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(self);
end;

function TMenuItem.GetItem(Index: Integer): TMenuItem;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index]
  else
    Result := nil;
end;

function TMenuItem.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TMenuItem.GetMenuIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  if not Assigned(FParent) then
    Exit;

  for i := 0 to High(FParent.FItems) do
    if FParent.FItems[i] = Self then
    begin
      Result := i;
      Exit;
    end;
end;

{ TPopupMenu }

constructor TPopupMenu.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FPopupComponent := nil;
  FItems := TMenuItem.Create(AOwner);
end;

destructor TPopupMenu.Destroy;
begin
  if Assigned(FItems) then
    FItems.Free;
  inherited Destroy;
end;

procedure TPopupMenu.PopUp(X, Y: Integer);
begin
  // no-op for headless tests
end;

{ TMainMenu }

constructor TMainMenu.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FItems := TMenuItem.Create(AOwner);
end;

destructor TMainMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

end.
