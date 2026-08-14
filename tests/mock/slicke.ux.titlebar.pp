(*
 * slicke.ux.titlebar.pp (test mock)
 * No-op stand-in for the custom title bar so umain builds against the mock LCL.
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 * License: Apache License 2.0
 *)

{**
  @unit slicke.ux.titlebar (mock)
  @brief Mirrors the public surface umain consumes from the real unit; every
         operation is a no-op. Tests never exercise the drawn title bar — the
         mock exists purely so the TfBG code that references it compiles.
}
unit slicke.ux.titlebar;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Controls, Graphics;

type
TSlickeTitleBarButton = (stbMinimize, stbMaximize, stbClose);
TSlickeTitleBarButtons = set of TSlickeTitleBarButton;

TSlickeResizeEdge = (sreLeft, sreTop, sreRight, sreBottom);
TSlickeResizeEdges = set of TSlickeResizeEdge;

TSlickeTitleBar = class(TWinControl)
private
  FTitle: string;
  FButtons: TSlickeTitleBarButtons;
  FBg: TColor;
  FText: TColor;
  FOnCloseRequest: TNotifyEvent;
  FOnMinimizeRequest: TNotifyEvent;
  FOnMaximizeRequest: TNotifyEvent;
public
  constructor Create(AOwner: TComponent = nil); override;
  procedure UpdateMetrics;
  procedure SetColors(ABg, AText: TColor);
  procedure RefreshTitle;
  property Title: string read FTitle write FTitle;
  property Buttons: TSlickeTitleBarButtons read FButtons write FButtons;
  property BarColor: TColor read FBg;
  property BarTextColor: TColor read FText;
  property OnCloseRequest: TNotifyEvent read FOnCloseRequest write FOnCloseRequest;
  property OnMinimizeRequest: TNotifyEvent read FOnMinimizeRequest write FOnMinimizeRequest;
  property OnMaximizeRequest: TNotifyEvent read FOnMaximizeRequest write FOnMaximizeRequest;
end;

TSlickeWindowGrips = class(TComponent)
private
  FActive: boolean;
public
  procedure EnsureOnTop;
  procedure SetActive(AActive: boolean);
  property Active: boolean read FActive;
end;

implementation

constructor TSlickeTitleBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtons := [stbMinimize, stbMaximize, stbClose];
  Height := 30;
end;

procedure TSlickeTitleBar.UpdateMetrics;
begin
end;

procedure TSlickeTitleBar.SetColors(ABg, AText: TColor);
begin
  FBg := ABg;
  FText := AText;
end;

procedure TSlickeTitleBar.RefreshTitle;
begin
end;

procedure TSlickeWindowGrips.EnsureOnTop;
begin
end;

procedure TSlickeWindowGrips.SetActive(AActive: boolean);
begin
  FActive := AActive;
end;

end.
