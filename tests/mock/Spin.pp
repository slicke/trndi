unit Spin;

{$mode ObjFPC}{$H+}

interface

uses Controls, Classes;

// Minimal placeholder for Spin used in headless builds.

type
  TSpinEdit = class(TControl)
  private
    FValue: Integer;
    FMinValue: Integer;
    FMaxValue: Integer;
    FDecimalPlaces: Integer;
    FOnChange: TNotifyEvent;
  public
    constructor Create; virtual;
    property Value: Integer read FValue write FValue;
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property DecimalPlaces: Integer read FDecimalPlaces write FDecimalPlaces;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFloatSpinEdit = class(TControl)
  private
    FValue: Double;
    FMinValue: Double;
    FMaxValue: Double;
    FDecimalPlaces: Integer;
    FOnChange: TNotifyEvent;
  public
    constructor Create; virtual;
    property Value: Double read FValue write FValue;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property DecimalPlaces: Integer read FDecimalPlaces write FDecimalPlaces;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;



implementation

constructor TSpinEdit.Create;
begin
  inherited Create;
  FValue := 0;
  FMinValue := 0;
  FMaxValue := 100;
  FDecimalPlaces := 0;
end;

constructor TFloatSpinEdit.Create;
begin
  inherited Create;
  FValue := 0.0;
  FMinValue := 0.0;
  FMaxValue := 100.0;
  FDecimalPlaces := 2;
end;

end.
