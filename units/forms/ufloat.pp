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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-09-15: FormMouseDown detects a Wayland session through
 *   TrndiNative.IsWaylandSession instead of reading XDG_SESSION_TYPE itself.
 *)

unit ufloat;

{$mode ObjFPC}{$H+}

{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}



interface

uses
Classes, ExtCtrls, Menus, StdCtrls, SysUtils, Math, Forms, Controls, Graphics,
Dialogs, LCLIntf, LCLType, InterfaceBase, trndi.native, trndi.shared, utrendarrow,
// After StdCtrls on purpose: utabularlabel's TLabel interposer must win the
// name so the value can typeset its digits in equal cells like the main window
utabularlabel
{$IFDEF DARWIN},
CocoaAll
{$ENDIF}
{$IFDEF LCLQT6},
qt6, qtwidgets, LMessages
{$ENDIF};

type

  { TfFloat }

TfFloat = class(TForm)
  lTime: TLabel;
  lDelta: TLabel;
  lArrow: TLabel;
  lRangeDown: TLabel;
  lRangeUp: TLabel;
  lVal: TLabel;
  MenuItem1: TMenuItem;
  miFontMain: TMenuItem;
  miSep: TMenuItem;
  miFontBlack: TMenuItem;
  miFontWhite: TMenuItem;
  miFontColor: TMenuItem;
  miMain: TMenuItem;
  miClock: TMenuItem;
  miDelta: TMenuItem;
  miCustomSize: TMenuItem;
  Separator1: TMenuItem;
  miXL: TMenuItem;
  miSmall: TMenuItem;
  miNormal: TMenuItem;
  miBig: TMenuItem;
  miSIze: TMenuItem;
  miSplit: TMenuItem;
  miVisible: TMenuItem;
  miOp100: TMenuItem;
  miOp25: TMenuItem;
  miOp50: TMenuItem;
  miOp75: TMenuItem;
  miCustomVisible: TMenuItem;
  miSplit1: TMenuItem;
  pMain: TPopupMenu;
  pnMultiUser: TPanel;
  tClock: TTimer;
  procedure FormCreate(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
  procedure FormMouseMove({%H-}Sender: TObject; {%H-}Shift: TShiftState; X, Y: integer);
  procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
  procedure FormPaint({%H-}Sender: TObject);
  procedure FormResize({%H-}Sender: TObject);
  procedure FormShow({%H-}Sender: TObject);
  procedure MenuItem1Click({%H-}Sender: TObject);
  procedure miClockClick({%H-}Sender: TObject);
  procedure miDeltaClick({%H-}Sender: TObject);
  procedure miCustomSizeClick({%H-}Sender: TObject);
  procedure miCustomVisibleClick({%H-}Sender: TObject);
  procedure miFontBlackClick({%H-}Sender: TObject);
  procedure miFontWhiteClick({%H-}Sender: TObject);
  procedure miFontMainClick({%H-}Sender: TObject);
  procedure miMainClick({%H-}Sender: TObject);
  procedure miNormalClick({%H-}Sender: TMenuItem);
  procedure miNormalClick({%H-}Sender: TObject);
  procedure miOp100Click({%H-}Sender: TObject);
  procedure tClockTimer({%H-}Sender: TObject);
private
  FDragStartX: integer;
  FDragStartY: integer;
  FDraggingWin: boolean;
  FPressX: integer;         // Screen position of the last left press — click detection
  FPressY: integer;
  FPressed: boolean;        // A left press began on this window and is not released yet
  {$IFDEF LCLQt6}
  FSystemMovePending: boolean; // Wayland: hand the press to the compositor once it travels past the click slop
  {$ENDIF}
  FTrendArrow: TTrendArrow; // Rotating trend arrow overlay (mirrors the main window)
  FProgressBox: TPaintBox;  // Slim next-refresh strip along the left edge (mirrors the main tube)
  FProgFrac: double;        // Fill fraction the strip currently shows
  FProgFill: TColor;        // Fill colour pushed from the main window
  FProgLevel: integer;      // Quantised fill level (px) last shown — change gate
  FOpacity: double;         // Window opacity currently applied, 0..1
  FBackdropBottom: TColor;  // Gradient tone at the bottom edge; clNone = flat
  procedure SetFormOpacity(Opacity: double);
  procedure ApplyRoundedCorners;
  procedure ApplyClock(AEnabled: boolean);
  procedure ApplyDelta(AEnabled: boolean);
  procedure SyncSizeMenu;
  procedure SyncOpacityMenu(AOpacity: single);
  procedure ProgressBoxPaint({%H-}Sender: TObject);
  procedure RaiseMainWindow;
  {$IFDEF LCLQt6}
  function StartSystemMove: boolean;
  procedure ApplyQtStyle;
  procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
  {$ENDIF}
public
  {** Mirror the main window's backdrop gradient: Color at the top edge
      running to ABottom at the bottom edge, the same slope the main window
      paints. Pass Color itself (or clNone) for a flat fill, which is what
      the high-contrast mode asks for.
      @param(ABottom Tone at the bottom edge.) }
  procedure SetBackdrop(ABottom: TColor);
  {** Mirror the delta since the previous reading into the bottom-right
      corner. Pushed from the main window on every sync.
      @param(AText The signed delta text; empty hides the field.)
      @param(AColor Its text colour, or clNone to keep the colour SetTextColor
        last applied.) }
  procedure SetDelta(const AText: string; AColor: TColor);
  {** Colour every piece of text on the float: the value, the trend glyph or
      vector arrow, the clock and the off-range markers. Both the fixed
      black/white menu choice and the main window's colour sync go through
      here, so the small corner texts never fall out of step with the value.
      @param(AColor The text colour.) }
  procedure SetTextColor(AColor: TColor);
  {** Mirror the main window's rotating trend arrow.
      @param(AEnabled Whether the rotating arrow replaces the glyph.)
      @param(AAngle Rotation in degrees (0 = flat, + = up, - = down).)
      @param(AColor Stroke colour for the arrow.) }
  procedure SetTrendArrow(AEnabled: boolean; AAngle: single; AColor: TColor);
  {** Mirror the main window's next-refresh progress onto the slim edge strip.
      Pushed from TfBG.tProgressTimer; calls that change nothing are ignored.
      @param(AShow Whether the strip is visible at all.)
      @param(AFrac Fill fraction, 0..1.)
      @param(AFill Fill colour, matching the main bar's current stage colour.) }
  procedure SetNextProgress(AShow: boolean; AFrac: double; AFill: TColor);
end;

resourcestring
RS_CUSTOM_OP = 'You can use shift+<number> to manually set visibility (0 = 100% visible)';
RS_CUSTOM_SIZE =
  'You can hold down shift and plus (+) or minus (-) to change the window size';

var
fFloat: TfFloat;
SettingsNative: TrndiNative = nil;


implementation

{$R *.lfm}

procedure ScaleLbl(ALabel: TLabel; customAl: TAlignment = taCenter;
customTl: TTextLayout = tlCenter);
var
  Low, High, Mid: integer;
  MaxWidth, MaxHeight: integer;
  TextWidth, TextHeight: integer;
  OptimalSize: integer;
  bmp: TBitmap;
begin
  // Check basic visibility conditions
  if not ALabel.Visible then
    ALabel.Visible := true;

  if ALabel.Caption = '' then
    Exit; // No text to display

  // Check that the label has size
  if (ALabel.Width <= 0) or (ALabel.Height <= 0) then
  begin
    ALabel.Width := 100;
    ALabel.Height := 30;
  end;

  // Set correct formatting
  ALabel.AutoSize := false;
  ALabel.WordWrap := false;
  ALabel.Alignment := customAl;
  ALabel.Layout := customTl;

  // Ensure text is visible against the background
  if ALabel.Font.Color = ALabel.Color then
    ALabel.Font.Color := clBlack;

  // Maximum width and height for the text
  MaxWidth := ALabel.Width - 4; // Small padding
  MaxHeight := ALabel.Height - 4;

  // Perform binary search to find optimal font size
  Low := 1;
  High := 150;
  OptimalSize := 1;

  // Measure on a temp TBitmap. TLabel.Canvas outside paint is unsafe on the
  // Cocoa widgetset (TCocoaContext.SetAntialiasing SIGABRTs on a nil ctx).
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(ALabel.Font);

    while Low <= High do
    begin
      Mid := (Low + High) div 2;
      bmp.Canvas.Font.Size := Mid;

      // A tabular label paints its digits in equal cells, wider than the
      // proportional run whenever a narrow digit is in the number; fit that
      // width or the cells will not fit exactly when it matters.
      if ALabel.TabularDigits then
        TextWidth := TLabel.TabularTextWidth(bmp.Canvas, ALabel.Caption)
      else
        TextWidth := bmp.Canvas.TextWidth(ALabel.Caption);
      TextHeight := bmp.Canvas.TextHeight(ALabel.Caption);

      if (TextWidth <= MaxWidth) and (TextHeight <= MaxHeight) then
      begin
        OptimalSize := Mid;
        Low := Mid + 1;
      end
      else
        High := Mid - 1;
    end;
  finally
    bmp.Free;
  end;

  // Set the optimal font size
  ALabel.Font.Size := OptimalSize;

  // Ensure settings are applied
  ALabel.Refresh;
end;

// Local helpers to read/write Trndi settings without referencing the global
// `native` variable (which isn't visible in this unit).
function GetSettingsNative: TrndiNative;
var
  activeUser: string;
begin
  if SettingsNative = nil then
  begin
    SettingsNative := TrndiNative.Create;
    SettingsNative.noFree := true;
  end;

  // Keep float settings scoped to the active user selected in the main form.
  activeUser := SettingsNative.GetRootSetting('users.active', '');
  if SettingsNative.configUser <> activeUser then
    SettingsNative.configUser := activeUser;

  Result := SettingsNative;
end;

function ReadIntSetting(const key: string; def: integer): integer;
begin
  Result := GetSettingsNative.GetIntSetting(key, def);
end;

function ReadFloatSetting(const key: string; def: single): single;
begin
  Result := GetSettingsNative.GetFloatSetting(key, def);
end;

procedure SaveSetting(const key: string; val: integer);
begin
  GetSettingsNative.SetSetting(key, val);
end;

procedure SaveFloatSetting(const key: string; val: single);
begin
  GetSettingsNative.SetFloatSetting(key, val, false);
end;

procedure TfFloat.FormCreate({%H-}Sender: TObject);
{$IFDEF LCLQt6}
var
  QtWidget: TQtWidget;
{$endif}
begin
  // Opacity is applied through the same path everywhere, so seed it from the
  // stored value before anything paints with it.
  FOpacity := ReadFloatSetting('ux.float.opacity', 0.5);
  FBackdropBottom := clNone;

  {$IFDEF LCLQt6}
  if HandleAllocated then
  begin
    QtWidget := TQtWidget(Handle);
    if Assigned(QtWidget) and Assigned(QtWidget.Widget) then
    begin
      QtWidget.setAttribute(QtWA_TranslucentBackground, true);
      QtWidget.setWindowFlags(QtWidget.windowFlags or QtFramelessWindowHint);
      ApplyQtStyle;
    end;
  end;
  {$ENDIF}

  // Next-refresh strip: created hidden; the main window shows and feeds it via
  // SetNextProgress while the progress-bar feature is on. Mouse events forward
  // to the form handlers so the strip doesn't punch a hole in window dragging.
  FProgressBox := TPaintBox.Create(Self);
  FProgressBox.Parent := Self;
  FProgressBox.Visible := false;
  FProgressBox.OnPaint := @ProgressBoxPaint;
  FProgressBox.OnMouseDown := @FormMouseDown;
  FProgressBox.OnMouseMove := @FormMouseMove;
  FProgressBox.OnMouseUp := @FormMouseUp;
end;

procedure TfFloat.ApplyRoundedCorners;
var
  {$IF DEFINED(DARWIN)}
  NSViewHandle: NSView;
  NSWin: NSWindow;
  Mask: NSBezierPath;
  {$ELSEIF DEFINED(LCLQT6)}
  {$ELSE}
  ABitmap: TBitmap;
  {$ENDIF}
begin
  {$IF DEFINED(DARWIN)}
  try
    // Get NSView + NSWindow from handle
    if HandleAllocated then
    begin
      NSViewHandle := NSView(Handle);
      if Assigned(NSViewHandle) then
      begin
        NSWin := NSViewHandle.window;
        if Assigned(NSWin) then
        begin
          // Set transparency
          NSWin.setOpaque(false);
          NSWin.setBackgroundColor(NSColor.clearColor);

          // Make corners roundable
          NSViewHandle.setWantsLayer(true);

          // Use a bezier path to round corners
          if Assigned(NSViewHandle.layer) then
          begin
            NSViewHandle.layer.setCornerRadius(10.0); // Set to 10 roundness
            NSViewHandle.layer.setMasksToBounds(true);
          end;
        end;
      end;
    end;
  except
    // Ignore any errors
  end;
  {$ELSEIF DEFINED(LCLQT6)}
  Self.BorderStyle := bsNone; // Remove border
  ApplyQtStyle;
  {$ELSE}
  Self.BorderStyle := bsNone; // Remove border
  // Use LCL stuff when Windows (or not Qt really)
  try
    ABitmap := TBitmap.Create;
    ABitmap.Monochrome := true;
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    // Black will become transparent
    ABitmap.Canvas.Brush.Color := clBlack;
    ABitmap.Canvas.FillRect(0, 0, Width, Height);
    // Paint the form itself in white
    ABitmap.Canvas.Brush.Color := clWhite;
    ABitmap.Canvas.RoundRect(0, 0, Width, Height, 20, 20);

    // Shape the form
    SetShape(ABitmap);
  finally
    ABitmap.Free;
  end;
  {$ENDIF}
end;

procedure TfFloat.SetFormOpacity(Opacity: double);
{$IFDEF DARWIN}
var
  NSViewHandle: NSView;
  NSWin: NSWindow;
{$ENDIF}
begin
  FOpacity := Opacity;
  {$IFDEF DARWIN}
  if HandleAllocated then
  try
    NSViewHandle := NSView(Handle);
    if Assigned(NSViewHandle) then
    begin
      NSWin := NSViewHandle.window;
      if Assigned(NSWin) then
        NSWin.setAlphaValue(Opacity);
    end;
  except
      // Ignore any errors
  end;
  {$ELSE}
  {$IFDEF LCLQt6}
  // Qt6 carries the opacity in the same style sheet as the corners and colour
  ApplyQtStyle;
  {$ENDIF}
  // Standard LCL approach for other platforms
  AlphaBlend := Opacity < 1.0;
  AlphaBlendValue := Round(Opacity * 255);
  {$ENDIF}
  // Persist opacity for next run
  SaveFloatSetting('ux.float.opacity', Opacity);
end;

procedure TfFloat.FormShow(Sender: TObject);
var
  storedH, storedW, storedLeft, storedTop: integer;
  storedOp: single;
begin
  // Do not auto-center here; respect current position so user dragging stays stable
  ApplyRoundedCorners;

  // Restore size & position if available (do this BEFORE any default size code)
  storedH := ReadIntSetting('size.float.height', Height);
  storedW := ReadIntSetting('size.float.width', Width);
  storedLeft := ReadIntSetting('position.float.left', Left);
  storedTop := ReadIntSetting('position.float.top', Top);

  Height := storedH;
  Width := storedW;
  Left := storedLeft;
  Top := storedTop;

  // Ensure labels/fonts are scaled for the restored size
  FormResize(Self);

  // Set the opacity (persisted or default)
  storedOp := ReadFloatSetting('ux.float.opacity', 0.5);
  SetFormOpacity(storedOp);

  // Reflect the restored size/opacity in the menu checkmarks
  SyncSizeMenu;
  SyncOpacityMenu(storedOp);

  // Restore the font color choice (0 = black, 1 = white, 2 = follow main window)
  case ReadIntSetting('ux.float.fontcolor', 0) of
  1:
    begin
      miFontWhite.Checked := true;
      SetTextColor(clWhite);
    end;
  2:
    miFontMain.Checked := true; // colors arrive with the next main-window sync
  else
    begin
      miFontBlack.Checked := true;
      SetTextColor(clBlack);
    end;
  end;

  // Restore the clock and the delta
  ApplyClock(ReadIntSetting('ux.float.clock', 0) = 1);
  ApplyDelta(ReadIntSetting('ux.float.delta', 1) = 1);
end;

procedure TfFloat.SyncSizeMenu;
begin
  miXL.Checked := Height = Screen.DesktopHeight div 5;
  miBig.Checked := Height = Screen.DesktopHeight div 10;
  miNormal.Checked := Height = Screen.DesktopHeight div 25;
  miSmall.Checked := Height = Screen.DesktopHeight div 50;
  miCustomSize.Checked := not (miXL.Checked or miBig.Checked or miNormal.Checked or
    miSmall.Checked);
end;

procedure TfFloat.SyncOpacityMenu(AOpacity: single);

  function Near(v: single): boolean;
  begin
    Result := Abs(AOpacity - v) < 0.01;
  end;

begin
  miOp25.Checked := Near(0.25);
  miOp50.Checked := Near(0.5);
  miOp75.Checked := Near(0.75);
  miOp100.Checked := Near(1);
  miCustomVisible.Checked := not (miOp25.Checked or miOp50.Checked or
    miOp75.Checked or miOp100.Checked);
end;

procedure TfFloat.MenuItem1Click({%H-}Sender: TObject);
begin
  Hide;
end;

procedure TfFloat.ApplyClock(AEnabled: boolean);
begin
  miClock.Checked := AEnabled;
  lTime.Visible := AEnabled;
  tClock.Enabled := AEnabled;
  if AEnabled then
    tClockTimer(tClock);
end;

procedure TfFloat.miClockClick(Sender: TObject);
begin
  ApplyClock(not miClock.Checked);
  SaveSetting('ux.float.clock', ord(miClock.Checked));
end;

procedure TfFloat.ApplyDelta(AEnabled: boolean);
begin
  miDelta.Checked := AEnabled;
  lDelta.Visible := AEnabled and (lDelta.Caption <> '');
end;

procedure TfFloat.miDeltaClick(Sender: TObject);
begin
  ApplyDelta(not miDelta.Checked);
  SaveSetting('ux.float.delta', ord(miDelta.Checked));
end;

procedure TfFloat.SetDelta(const AText: string; AColor: TColor);
begin
  lDelta.Caption := AText;
  if AColor <> clNone then
    lDelta.Font.Color := AColor;
  lDelta.AdjustSize;
  // FormResize places it; the main window runs that right after this sync
  ApplyDelta(miDelta.Checked);
end;

procedure TfFloat.miCustomSizeClick(Sender: TObject);
begin
  ShowMessage(RS_CUSTOM_SIZE);
end;

procedure TfFloat.miCustomVisibleClick(Sender: TObject);
begin
  ShowMessage(RS_CUSTOM_OP);
end;

procedure TfFloat.SetBackdrop(ABottom: TColor);
begin
  if ABottom = Color then
    ABottom := clNone;
  if ABottom = FBackdropBottom then
    Exit;
  FBackdropBottom := ABottom;
  Invalidate;
end;

{------------------------------------------------------------------------------
  The backdrop gradient. Not on Qt6: there the style sheet owns the background
  (rounded corners and the translucent fill), and a canvas fill would paint
  square, opaque corners over it.
 ------------------------------------------------------------------------------}
procedure TfFloat.FormPaint(Sender: TObject);
begin
  {$IFNDEF LCLQt6}
  if FBackdropBottom = clNone then
    Exit;
  Canvas.Brush.Style := bsSolid;
  Canvas.GradientFill(ClientRect, ColorToRGB(Color), ColorToRGB(FBackdropBottom),
    gdVertical);
  {$ENDIF}
end;

procedure TfFloat.SetTextColor(AColor: TColor);
begin
  lVal.Font.Color := AColor;
  lArrow.Font.Color := AColor;
  lTime.Font.Color := AColor;
  lDelta.Font.Color := AColor;
  lRangeDown.Font.Color := AColor;
  lRangeUp.Font.Color := AColor;
  if Assigned(FTrendArrow) then
    FTrendArrow.ArrowColor := AColor;
end;

procedure TfFloat.miFontBlackClick(Sender: TObject);
begin
  SetTextColor(clBlack);
  SaveSetting('ux.float.fontcolor', 0);
end;

procedure TfFloat.miFontWhiteClick(Sender: TObject);
begin
  SetTextColor(clWhite);
  SaveSetting('ux.float.fontcolor', 1);
end;

procedure TfFloat.miFontMainClick(Sender: TObject);
begin
  // Colors are picked up from the main window on its next sync
  SaveSetting('ux.float.fontcolor', 2);
end;


procedure TfFloat.miMainClick(Sender: TObject);
begin
  if Application.MainForm.Visible then
    Application.MainForm.Hide
  else
    Application.MainForm.Show;
end;

procedure TfFloat.miNormalClick(Sender: TMenuItem);
var
  h: integer;
begin
  h := Height;

  if Sender = miXL then
    h := Screen.DesktopHeight div 5
  else
  if Sender = miBig then
    h := Screen.DesktopHeight div 10
  else
  if Sender = miNormal then
    h := Screen.DesktopHeight div 25
  else
  if Sender = miSmall then
    h := Screen.DesktopHeight div 50;

  Height := h;
  Width := round(Height * 1.55);
  lVal.Width := round(clientwidth * 0.75);
  lArrow.Width := round(clientwidth * 0.25);
  //---
  ApplyRoundedCorners;
  SyncSizeMenu;
  // Persist selected size
  SaveSetting('size.float.height', Height);
  SaveSetting('size.float.width', Width);
end;

procedure TfFloat.miNormalClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    miNormalClick(Sender as TMenuItem);
end;

procedure TfFloat.miOp100Click(Sender: TObject);
var
  i: integer;
  v: double;
begin
  if not TryStrToInt((Sender as TMenuItem).hint, i) then
    Exit;
  v := i / 100;

  SetFormOpacity(v);
  (Sender as TMenuItem).Checked := true;
end;

procedure TfFloat.tClockTimer(Sender: TObject);
begin
  lTime.Caption := FormatDateTime(DefaultFormatSettings.ShortTimeFormat, Now);
  // Re-anchor to the top-right corner; the caption width just changed
  lTime.AdjustSize;
  lTime.Left := ClientWidth - lTime.Width - Scale96ToForm(8);
  if lTime.Visible = false then
    (Sender as TTimer).Enabled := false;
end;

const
  CLICK_SLOP = 4; // px of travel still counted as a click rather than a drag

procedure TfFloat.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  ScreenPt: TPoint;
  DeltaX, DeltaY: integer;
begin
  if not (FDraggingWin {$IFDEF LCLQt6} or FSystemMovePending {$ENDIF}) then
    Exit;

  // Convert to screen coordinates to handle moves from child controls
  if Sender is TControl then
    ScreenPt := (Sender as TControl).ClientToScreen(Point(X, Y))
  else
    ScreenPt := ClientToScreen(Point(X, Y));

  {$IFDEF LCLQt6}
  if FSystemMovePending then
  begin
    // Still within the click slop: keep waiting, a release here is a click.
    if (Abs(ScreenPt.X - FPressX) <= CLICK_SLOP) and
      (Abs(ScreenPt.Y - FPressY) <= CLICK_SLOP) then
      Exit;
    FSystemMovePending := false;
    if StartSystemMove then
    begin
      // The compositor owns the pointer from here and swallows the mouse-up,
      // so this press can no longer end as a click.
      FPressed := false;
      Exit;
    end;
    // No compositor move for this window: drag by hand like everywhere else.
    FDraggingWin := true;
  end;
  {$ENDIF}

  if FDraggingWin then
  begin
    // Calculate the delta (how much the mouse moved)
    DeltaX := ScreenPt.X - FDragStartX;
    DeltaY := ScreenPt.Y - FDragStartY;
    
    // Move the window by the delta
    Left := Left + DeltaX;
    Top := Top + DeltaY;
    
    // Update stored position for next move
    FDragStartX := ScreenPt.X;
    FDragStartY := ScreenPt.Y;
  end;
end;

procedure TfFloat.FormMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
var
  ScreenPt: TPoint;
  wasClick: boolean;
begin
  FDraggingWin := false;
  {$IFDEF LCLQt6}
  FSystemMovePending := false;
  {$ENDIF}
  // Persist current position
  SaveSetting('position.float.left', Left);
  SaveSetting('position.float.top', Top);

  // A left press released (almost) where it began is a click, not a drag:
  // hand the user the main window back. Opening the float moves the focus to
  // it, and once the main window has slipped behind other apps the float is
  // the only Trndi surface left on screen, so a click on it is the way home.
  if Sender is TControl then
    ScreenPt := (Sender as TControl).ClientToScreen(Point(X, Y))
  else
    ScreenPt := ClientToScreen(Point(X, Y));
  wasClick := FPressed and (Button = mbLeft) and
    (Abs(ScreenPt.X - FPressX) <= CLICK_SLOP) and
    (Abs(ScreenPt.Y - FPressY) <= CLICK_SLOP);
  FPressed := false;
  if wasClick then
    RaiseMainWindow;
end;

{------------------------------------------------------------------------------
  Bring the main window back: restore it when minimised, show it when hidden
  and raise it. BringToFront ends in SetForegroundWindow, which Windows only
  honours for the process that owns the foreground window — true right after
  a click on the float, which is why this runs from the click itself.
 ------------------------------------------------------------------------------}
procedure TfFloat.RaiseMainWindow;
var
  mf: TCustomForm;
begin
  mf := Application.MainForm;
  if (mf = nil) or (csDestroying in mf.ComponentState) then
    Exit;
  if mf.WindowState = wsMinimized then
    mf.WindowState := wsNormal;
  mf.Show;
  mf.BringToFront;
end;

procedure TfFloat.FormResize(Sender: TObject);
var
  inset, textH, split: integer;
  edge, corner, stripW: integer;
begin
  // Every fixed distance is a 96-dpi design value scaled to the form's dpi
  edge := Scale96ToForm(8);     // Margin from the window edge to the corner texts
  corner := Scale96ToForm(4);   // Margin from the top edge, and past the strip
  stripW := Max(Scale96ToForm(3), ClientWidth div 60);

  // Lay the next-refresh strip out first: the value's left edge depends on it.
  inset := 0;
  if Assigned(FProgressBox) then
  begin
    // Inset past the rounded corners so the strip never pokes out of the shape
    FProgressBox.SetBounds(Scale96ToForm(2), edge, stripW,
      Max(corner, ClientHeight - 2 * edge));
    if FProgressBox.Visible then
      inset := FProgressBox.Left + FProgressBox.Width + corner;
  end;

  // The multi-user bar is aligned to the bottom edge; keep the text above it.
  textH := ClientHeight;
  if pnMultiUser.Visible then
    textH := textH - pnMultiUser.Height;

  // Value on the left three quarters, arrow on the right quarter. The value is
  // left-justified, so it starts past the strip rather than under it.
  split := Round(ClientWidth * 0.75);
  lVal.SetBounds(inset, 0, Max(1, split - inset), textH);
  lArrow.SetBounds(split, 0, ClientWidth - split, textH);

  ScaleLbl(lVal, taLeftJustify, tlCenter);
  ScaleLbl(lArrow, taCenter, tlCenter);
  // Each fitter fills its own box, and the arrow's box is a full-height
  // quarter while the value's holds three or four characters, so a lone
  // glyph could come out taller than the digits it qualifies. Hold the arrow
  // to the value's size: equal when both fit, never larger.
  if lArrow.Font.Size > lVal.Font.Size then
    lArrow.Font.Size := lVal.Font.Size;

  // Keep the clock tucked into the top-right corner, above the arrow. Sized
  // from the window rather than the arrow's fitted font, so it stays put
  // when a new reading refits the big text.
  lTime.Font.Height := -Max(Scale96ToForm(8), ClientHeight div 6);
  lTime.AdjustSize;
  lTime.Left := ClientWidth - lTime.Width - edge;
  lTime.Top := corner;

  // Off-range markers live in the top-left corner, mirroring the clock
  lRangeDown.Font.Height := lTime.Font.Height;
  lRangeUp.Font.Height := lTime.Font.Height;
  lRangeDown.AdjustSize;
  lRangeUp.AdjustSize;
  lRangeDown.Left := edge;
  lRangeDown.Top := corner;
  lRangeUp.Left := edge;
  lRangeUp.Top := corner;

  // The delta sits in the bottom-right corner, under the arrow, in the
  // clock's size, above the multi-user bar when that shows
  lDelta.Font.Height := lTime.Font.Height;
  lDelta.AdjustSize;
  lDelta.Left := ClientWidth - lDelta.Width - edge;
  lDelta.Top := textH - lDelta.Height - corner;

  // Keep the rotating arrow overlay tracking lArrow's bounds. ScaleLbl re-shows
  // lArrow, so re-hide the glyph while the vector arrow is active.
  if Assigned(FTrendArrow) then
  begin
    FTrendArrow.BoundsRect := lArrow.BoundsRect;
    if FTrendArrow.Visible then
      lArrow.Visible := false;
  end;
end;

procedure TfFloat.SetTrendArrow(AEnabled: boolean; AAngle: single; AColor: TColor);
begin
  if not AEnabled then
  begin
    if Assigned(FTrendArrow) then
      FTrendArrow.Visible := false;
    lArrow.Visible := lArrow.Caption <> '';
    Exit;
  end;

  if not Assigned(FTrendArrow) then
  begin
    FTrendArrow := TTrendArrow.Create(Self);
    FTrendArrow.Parent := lArrow.Parent;
    // Forward mouse events like the labels do, so dragging and click-to-raise
    // also work over the arrow.
    FTrendArrow.OnMouseDown := @FormMouseDown;
    FTrendArrow.OnMouseMove := @FormMouseMove;
    FTrendArrow.OnMouseUp := @FormMouseUp;
    // The overlay is created after the corner texts and would otherwise
    // paint over them where its bounds reach the corners
    lTime.BringToFront;
    lDelta.BringToFront;
    lRangeDown.BringToFront;
    lRangeUp.BringToFront;
  end;

  FTrendArrow.ArrowColor := AColor;
  FTrendArrow.BoundsRect := lArrow.BoundsRect;
  FTrendArrow.Angle := AAngle;
  lArrow.Visible := false;
  FTrendArrow.Visible := true;
end;

procedure TfFloat.SetNextProgress(AShow: boolean; AFrac: double; AFill: TColor);
var
  lvl: integer;
begin
  if not Assigned(FProgressBox) then
    Exit;
  if not AShow then
  begin
    if FProgressBox.Visible then
    begin
      // Give the value its left edge back
      FProgressBox.Visible := false;
      FormResize(Self);
    end;
    Exit;
  end;

  // Only repaint on a visible change: at a few pixels wide the level moves
  // roughly once per handful of seconds, and the caller ticks every second.
  lvl := Round(AFrac * FProgressBox.Height);
  if FProgressBox.Visible and (lvl = FProgLevel) and (AFill = FProgFill) then
    Exit;

  FProgFrac := AFrac;
  FProgFill := AFill;
  FProgLevel := lvl;
  if not FProgressBox.Visible then
  begin
    // The strip takes a slice off the value's left edge: relayout the labels
    FProgressBox.Visible := true;
    FormResize(Self);
  end;
  FProgressBox.Invalidate;
end;

procedure TfFloat.ProgressBoxPaint(Sender: TObject);
var
  w, h, lvl: integer;
  track: TColor;
begin
  w := FProgressBox.Width;
  h := FProgressBox.Height;
  // Recessed track against the float's own background — the float mirrors the
  // main window's range colour, so derive per paint rather than caching.
  if IsLightColor(Color) then
    track := BlendColors(clBlack, Color, 0.14)
  else
    track := BlendColors(clWhite, Color, 0.10);
  with FProgressBox.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Style := psClear;
    Brush.Color := track;
    FillRect(Classes.Rect(0, 0, w, h));
    lvl := Round(FProgFrac * h);
    if lvl > 0 then
    begin
      Brush.Color := FProgFill;
      FillRect(Classes.Rect(0, h - lvl, w, h));
    end;
  end;
end;

{$IFDEF LCLQt6}
{------------------------------------------------------------------------------
  Ask the compositor to move this window with the current press. Wayland
  offers no way for a client to place its own toplevel, so this is the only
  move that works there. True when the compositor took the gesture.
 ------------------------------------------------------------------------------}
function TfFloat.StartSystemMove: boolean;
var
  QtWidget: TQtWidget;
  qwin: QWindowH;
begin
  Result := false;
  if not HandleAllocated then
    Exit;
  QtWidget := TQtWidget(Handle);
  if (QtWidget = nil) or (QtWidget.Widget = nil) then
    Exit;
  qwin := QWidget_windowHandle(QtWidget.Widget);
  if qwin = nil then
    Exit;
  Result := QWindow_startSystemMove(qwin);
  if Result then
    // The compositor swallows the matching mouse-up (KWin), so drop the
    // implicit capture or it sticks to the pressed control and hijacks every
    // later press.
    SetCaptureControl(nil);
end;

{------------------------------------------------------------------------------
  Qt6 draws the float through one style sheet, and a style sheet wins over
  the widget palette. Corner radius, the range colour pushed into Color and
  the opacity therefore all have to travel together: setting any one of them
  alone used to drop the other two, leaving a square grey window.
 ------------------------------------------------------------------------------}
procedure TfFloat.ApplyQtStyle;
const
  CORNER_RADIUS = 10;
var
  rgb: longint;
  StyleStr: widestring;
begin
  if not HandleAllocated then
    Exit;
  rgb := ColorToRGB(Color);
  StyleStr := UTF8Decode(Format('border-radius: %dpx; background-color: rgba(%d, %d, %d, %d);',
    [CORNER_RADIUS, Red(rgb), Green(rgb), Blue(rgb), Round(FOpacity * 255)]));
  QWidget_setStyleSheet(TQtWidget(Handle).Widget, @StyleStr);
end;

// The main window mirrors its range colour into Color; on Qt6 that only
// shows once it is written into the style sheet as well.
procedure TfFloat.CMColorChanged(var Message: TLMessage);
begin
  inherited;
  ApplyQtStyle;
end;
{$ENDIF}

procedure TfFloat.FormMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
var
  ScreenPt: TPoint;
begin
  if Button = mbLeft then
  begin
    // Convert to screen coordinates to handle clicks from child controls
    if Sender is TControl then
      ScreenPt := (Sender as TControl).ClientToScreen(Point(X, Y))
    else
      ScreenPt := ClientToScreen(Point(X, Y));

    FDragStartX := ScreenPt.X;
    FDragStartY := ScreenPt.Y;
    FPressX := ScreenPt.X;
    FPressY := ScreenPt.Y;
    FPressed := true;

    {$IFDEF LCLQt6}
    // On Wayland the compositor has to do the move, and it also swallows the
    // mouse-up that ends the gesture - so starting the move on the press
    // itself turned every click into a drag and the click-to-restore never
    // fired. Wait for the pointer to leave the click slop instead
    // (FormMouseMove); a release before that reaches FormMouseUp as a click,
    // the same as on every other platform. The shared helper also honours a
    // toolkit forced onto XWayland, which gets X11 window management and so
    // takes the ordinary drag path below.
    if TrndiNative.IsWaylandSession then
    begin
      FSystemMovePending := true;
      Exit;
    end;
    {$ENDIF}

    FDraggingWin := true;
  end;
end;

procedure TfFloat.FormKeyDown({%H-}Sender: TObject; var Key: word; Shift: TShiftState);
var
  num: double;
begin
  if key = VK_ESCAPE then
  begin
    Hide;
    key := 0;
  end;
  if ((ssShift in Shift) and (Key >= 48) and (Key <= 57)) then
  begin
    num := (key - 48) / 10;
    if num < 0.1 then
      num := 1;

    SetFormOpacity(num);
    miCustomVisible.Checked := true;
  end;

  if ((ssShift in Shift) and (Key in [{$ifdef LINUX}63 (*linux **),{$endif} 187, (* + *) 189, 191 (*mac +*)])) then
  begin
    if key <> 189 then // not -
      Height := Height + 5
    else
      Height := Height - 5;
    miNormalClick(miCustomSize);
  end;
end;

finalization
  FreeAndNil(SettingsNative);

end.
