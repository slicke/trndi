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
unit ExtCtrls;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses Controls, Classes, Graphics, Menus;

type
  TShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle);

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
    FIcon: TIcon;
    FOnClick: TNotifyEvent;
    FPopupMenu: TPopupMenu;
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    property Hint: string read FHint write FHint;
    property Visible: Boolean read FVisible write FVisible;
    property Icon: TIcon read FIcon write FIcon;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property PopUpMenu: TPopupMenu read FPopupMenu write FPopupMenu;
  end;

  TImage = class(TControl)
  private
    FPicture: TPicture;
  public
    constructor Create(AOwner: Controls.TComponent = nil); override;
    destructor Destroy; override;
    property Picture: TPicture read FPicture write FPicture;
  end;

  TPaintBox = class(TControl)
  public
    constructor Create(AOwner: Controls.TComponent = nil);
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  end;

  TBevel = class(TControl)
  public
    Shape: Integer;
  end;

  TShape = class(TControl)
  private
    FBrush: TBrush;
    FPen: TPen;
    FShape: TShapeType;
  public
    constructor Create(AOwner: Controls.TComponent = nil); override;
    destructor Destroy; override;
    property Brush: TBrush read FBrush;
    property Pen: TPen read FPen;
    property Shape: TShapeType read FShape write FShape;
  end;

implementation

constructor TTrayIcon.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FHint := '';
  FVisible := False;
  FIcon := TIcon.Create;
  FOnClick := nil;
  FPopupMenu := nil;
end;

destructor TTrayIcon.Destroy;
begin
  if Assigned(FIcon) then
    FIcon.Free;
  inherited Destroy;
end;

constructor TImage.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
end;

destructor TImage.Destroy;
begin
  if Assigned(FPicture) then
    FPicture.Free;
  inherited Destroy;
end;

constructor TPaintBox.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  if FCanvas = nil then
    FCanvas := TCanvas.Create;
end;

destructor TPaintBox.Destroy;
begin
  // Do not free FCanvas here — TControl.Destroy frees it. Avoid double-free.
  inherited Destroy;
end;

constructor TShape.Create(AOwner: Controls.TComponent = nil);
begin
  inherited Create(AOwner);
  FBrush := TBrush.Create;
  FPen := TPen.Create;
  FShape := stRectangle;
end;

destructor TShape.Destroy;
begin
  if Assigned(FBrush) then
    FBrush.Free;
  if Assigned(FPen) then
    FPen.Free;
  inherited Destroy;
end;

end.
