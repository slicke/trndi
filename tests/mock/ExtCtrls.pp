unit ExtCtrls;

{$mode ObjFPC}{$H+}

interface

uses Controls, Classes;

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

implementation

end.
