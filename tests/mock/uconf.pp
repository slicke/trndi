unit uconf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics;

type
  TRazerChromaBase = class
  end;

  TfConf = class(TObject)
  public
    cbSys: TComboBox;
    lbChroma: TListBox;
    lbUsers: TListBox;
    lbExtensions: TListBox;
    lOS: TLabel;
    lWidgetset: TLabel;
    lWM: TLabel;
    lArch: TLabel;
    lExtCount: TLabel;
    tsExt: TPanel;
    Chroma: TRazerChromaBase;
    constructor Create(AOwner: TComponent = nil);
    destructor Destroy; override;
  end;

implementation

constructor TfConf.Create(AOwner: TComponent = nil);
begin
  inherited Create;
  cbSys := TComboBox.Create(nil);
  lbChroma := TListBox.Create(nil);
  lbUsers := TListBox.Create(nil);
  lbExtensions := TListBox.Create(nil);
  lOS := TLabel.Create(nil);
  lWidgetset := TLabel.Create(nil);
  lWM := TLabel.Create(nil);
  lArch := TLabel.Create(nil);
  lExtCount := TLabel.Create(nil);
  tsExt := TPanel.Create(nil);
  Chroma := nil; // tests or caller may assign via TRazerChromaFactory
end;

destructor TfConf.Destroy;
begin
  tsExt.Free;
  lExtCount.Free;
  lArch.Free;
  lWM.Free;
  lWidgetset.Free;
  lOS.Free;
  lbExtensions.Free;
  lbUsers.Free;
  lbChroma.Free;
  cbSys.Free;
  inherited Destroy;
end;

end.
