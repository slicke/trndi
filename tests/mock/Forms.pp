unit Forms;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Controls, Menus, SysUtils;

type
  TCloseAction = (caNone, caHide, caFree, caMinimize);

  // Minimal window/form enums used by umain
  TWindowState = (wsNormal, wsMinimized, wsMaximized);
  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsSizeToolWin);
  TFormStyle = (fsNormal, fsStayOnTop, fsMDIChild);

  // Minimal application properties type used on forms
  TApplicationProperties = class(TComponent)
  public
    ShowHint: Boolean;
    Title: string;
    MainFormOnTaskBar: Boolean;
  end;

  TForm = class(TWinControl)
  private
    FShowing: Boolean;
    FMenu: TMainMenu;
    FBorderStyle: TFormBorderStyle;
    FCursor: Integer;
  public
    procedure Close; virtual;
    procedure DoClose(var CloseAction: TCloseAction); virtual;
    property Showing: Boolean read FShowing write FShowing;
    property Menu: TMainMenu read FMenu write FMenu;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle;
    property Cursor: Integer read FCursor write FCursor;
    procedure Repaint; virtual;
  end;

  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  TApplication = class(TComponent)
  private
    FExeName: string;
    FIcon: TObject;
    FOnException: TExceptionEvent;
    FCursor: Integer;
  public
    constructor Create(AOwner: TComponent = nil); virtual;
    property ExeName: string read FExeName write FExeName;
    property Icon: TObject read FIcon write FIcon;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property Cursor: Integer read FCursor write FCursor;
    procedure ProcessMessages; virtual;
    procedure Terminate; virtual;
  end;

var
  Application: TApplication;

implementation

constructor TApplication.Create(AOwner: TComponent = nil);
begin
  inherited Create(AOwner);
  FIcon := nil;
  FOnException := nil;
  FCursor := 0;
end;

procedure TApplication.ProcessMessages;
begin
  // no-op in test environment
end;

procedure TApplication.Terminate;
begin
  // no-op in test environment
end;

procedure TForm.Close;
begin
  // no-op for headless
end;

procedure TForm.Repaint;
begin
  // no-op for headless tests
end;

procedure TForm.DoClose(var CloseAction: TCloseAction);
begin
  // no-op for headless
end;

initialization
  Application := TApplication.Create;
  Application.ExeName := ParamStr(0);

end.
