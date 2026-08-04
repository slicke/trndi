(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)
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
    procedure AdjustSize; override; // macOS AutoSize helper
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
