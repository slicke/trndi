unit ComCtrls;

{$mode ObjFPC}{$H+}

interface

uses Controls, Classes;

type
  TProgressBar = class(TWinControl)
  private
    FPosition: Integer;
  public
    property Position: Integer read FPosition write FPosition;
  end;

  TTabSheet = class(TWinControl)
  end;

  TPageControl = class(TWinControl)
  private
    FActivePage: TTabSheet;
  public
    property ActivePage: TTabSheet read FActivePage write FActivePage;
  end;

implementation

end.
