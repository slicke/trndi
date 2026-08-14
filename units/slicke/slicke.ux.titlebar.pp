(*
 * slicke.ux.titlebar.pp
 * Custom in-client title bar and resize grips for frameless LCL forms.
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 * License: Apache License 2.0
 *)

{**
  @unit slicke.ux.titlebar
  @brief Owner-drawn title bar and window resize grips for frameless (bsNone) forms.

  @details
  Wayland compositors do not let an application color or replace the server-side
  window decorations, and some desktops force client-side decorations. This unit
  lets a form drop its native frame entirely and draw its own title bar instead:
  a colored strip with the form caption and minimize/maximize/close buttons,
  plus invisible edge/corner grips so the window stays resizable.

  Window moving and resizing are handed to the compositor whenever the widgetset
  exposes the required call (Qt6: @code(QWindow_startSystemMove/Resize), the
  primary supported widgetset; GTK3: @code(gtk_window_begin_move_drag/
  begin_resize_drag) as best effort). Compositor-driven moves keep edge snapping
  and drag-to-top gestures working. Where no system call is available the unit
  falls back to moving/resizing the form manually from mouse deltas (X11 and
  other widgetsets).

  The public API centers around:
  - @link(TSlickeTitleBar): the bar itself. Parent it to a form, call
    @link(TSlickeTitleBar.SetColors) to restyle it at runtime.
  - @link(TSlickeWindowGrips): creates and manages the resize grips.
  - @link(SlickeStartSystemMove) / @link(SlickeStartSystemResize): the raw
    compositor glue, usable on their own.

  The unit is self-contained (LCL only) so it can be reused outside Trndi.
}
unit slicke.ux.titlebar;

{$I ../../inc/native.inc}

interface

uses
Classes, SysUtils, Controls, Graphics, Forms, LCLType, LCLIntf, Math;

type
  {** Which caption buttons the bar shows. }
TSlickeTitleBarButton = (stbMinimize, stbMaximize, stbClose);
TSlickeTitleBarButtons = set of TSlickeTitleBarButton;

  {** Window edges involved in a resize drag. }
TSlickeResizeEdge = (sreLeft, sreTop, sreRight, sreBottom);
TSlickeResizeEdges = set of TSlickeResizeEdge;

  {**
    Ask the compositor/window manager to start an interactive move of
    @param(AForm), as if the user grabbed the native title bar. Must be called
    from a mouse-down/mouse-move handler while the button is held: the
    compositor validates the request against the active grab.
    @returns(@true when the system took over the drag — after that no further
             mouse events reach the LCL until the button is released.)
  }
function SlickeStartSystemMove(AForm: TCustomForm): boolean;

  {**
    Ask the compositor/window manager to start an interactive resize of
    @param(AForm) from the given @param(Edges). Same grab rules as
    @link(SlickeStartSystemMove).
    @returns(@true when the system took over the resize.)
  }
function SlickeStartSystemResize(AForm: TCustomForm; Edges: TSlickeResizeEdges): boolean;

type
  {**
    Owner-drawn title bar for a frameless form. Create it with the form as
    owner, set @code(Parent := form) — it aligns @code(alTop) and sizes itself
    from the form font. The bar paints @link(Title) (or the parent form's
    caption when empty) and the enabled caption buttons, hands drags to the
    compositor, toggles maximize on double click, and recolors instantly via
    @link(SetColors) — which is how a host application implements "full color"
    title bars on platforms whose native decorations cannot be tinted.
  }
TSlickeTitleBar = class(TCustomControl)
private
  FBg: TColor;
  FText: TColor;
  FTitle: string;
  FTitleAlignment: TAlignment;
  FMetricHeight: integer; // Height UpdateMetrics derived from the font; the
                          // bar re-asserts it when anything else resizes it
  FButtons: TSlickeTitleBarButtons;
  FHoverBtn: integer;   // index into visible-button order, -1 = none
  FPressedBtn: integer; // button armed by mouse-down, -1 = none
  FMaybeDrag: boolean;  // left button down outside the buttons, slop not yet left
  FManualDrag: boolean; // fallback drag (no compositor support) in progress
  FDownScreen: TPoint;  // screen coords at mouse-down (drag origin)
  FFormOrigin: TPoint;  // form Left/Top at mouse-down (manual drag base)
  FOnCloseRequest: TNotifyEvent;
  FOnMinimizeRequest: TNotifyEvent;
  FOnMaximizeRequest: TNotifyEvent;
  function ButtonRect(AIndex: integer): TRect;
  function ButtonAt(X, Y: integer): integer;
  function ButtonKind(AIndex: integer): TSlickeTitleBarButton;
  function ButtonCount: integer;
  procedure DoButtonAction(AKind: TSlickeTitleBarButton);
  procedure SetTitle(const AValue: string);
  procedure SetTitleAlignment(const AValue: TAlignment);
  procedure SetButtons(const AValue: TSlickeTitleBarButtons);
protected
  procedure Paint; override;
  procedure Resize; override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  procedure MouseLeave; override;
  procedure DblClick; override;
public
  constructor Create(AOwner: TComponent); override;
    {** Recompute the bar height from the current font/DPI. Called on creation;
        call again after changing @code(Font). }
  procedure UpdateMetrics;
    {** Restyle the bar: @param(ABg) fills it, @param(AText) is used for the
        caption text and button glyphs. Cheap — repaints only on change. }
  procedure SetColors(ABg, AText: TColor);
    {** Repaint the caption text (call after the form caption changed). }
  procedure RefreshTitle;
    {** Bar text; when empty the parent form's Caption is drawn. }
  property Title: string read FTitle write SetTitle;
    {** How the caption text sits in the bar. Default left-justified, the
        Windows/KDE convention; taCenter gives the GNOME/macOS look. }
  property TitleAlignment: TAlignment read FTitleAlignment write SetTitleAlignment;
    {** Which caption buttons to draw. Default: all three. }
  property Buttons: TSlickeTitleBarButtons read FButtons write SetButtons;
    {** Current bar background color. }
  property BarColor: TColor read FBg;
    {** Current text/glyph color. }
  property BarTextColor: TColor read FText;
    {** Fired by the close button; when unassigned the parent form is closed. }
  property OnCloseRequest: TNotifyEvent read FOnCloseRequest write FOnCloseRequest;
    {** Fired by the minimize button; default minimizes the parent form. }
  property OnMinimizeRequest: TNotifyEvent read FOnMinimizeRequest write FOnMinimizeRequest;
    {** Fired by the maximize button and double-click; default toggles
        wsMaximized/wsNormal on the parent form. }
  property OnMaximizeRequest: TNotifyEvent read FOnMaximizeRequest write FOnMaximizeRequest;
end;

  {**
    Convert a decorated form into a frameless one carrying a drawn title bar:
    drops the native frame, creates the bar (anchored across the top), shifts
    the existing content down by the bar height (absolutely-placed controls
    via Top, aligned controls touching the top strip via BorderSpacing) and
    grows the form to keep the bottom padding. Purely mechanical — the caller
    decides *whether* (e.g. only on Wayland) and with which colors.
    @returns(The bar, or @nil when the form is already frameless (deliberate
             overlays) or already carries a TSlickeTitleBar.)
  }
function SlickeDressWithTitleBar(AForm: TCustomForm; ABarBg, ABarText: TColor;
  AButtons: TSlickeTitleBarButtons = [stbClose]): TSlickeTitleBar;

type
  {**
    Invisible edge/corner grips that keep a frameless form resizable. Create
    with the form as owner: eight @code(TGraphicControl) strips are placed
    along the borders, wired to @link(SlickeStartSystemResize) with a manual
    fallback, and re-laid out whenever the form resizes. The grips hide
    themselves while the form is maximized or fullscreen.
  }
TSlickeWindowGrips = class(TComponent)
private
  FForm: TCustomForm;
  FGrips: array of TGraphicControl;
  FActive: boolean;
  procedure FormResized(Sender: TObject);
  procedure UpdateLayout;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
    {** Re-stack the grips above sibling controls (call after the host form
        rearranged/raised its own controls). }
  procedure EnsureOnTop;
    {** Show or hide all grips (hide while fullscreen/maximized). }
  procedure SetActive(AActive: boolean);
  property Active: boolean read FActive;
end;

implementation

{$ifdef LCLQt6}
uses qt6, qtwidgets;
{$endif}
{$ifdef LCLGTK3}
uses LazGtk3, LazGdk3, gtk3widgets;
{$endif}

const
  // Grip geometry: how thick the invisible resize strips are, and how far the
  // corner zones extend along each edge.
GRIP_THICKNESS = 6;
GRIP_CORNER = 14;
  // Pixels the pointer must travel from mouse-down before a bar-drag starts;
  // below this a press stays a (double-)click. Mirrors umain's touch slop idea
  // at title-bar scale.
DRAG_SLOP = 4;
MIN_FORM_W = 120;
MIN_FORM_H = 120;

{------------------------------------------------------------------------------
  Compositor glue
 ------------------------------------------------------------------------------}

{$ifdef LCLQt6}
function QtTopWindow(AForm: TCustomForm): QWindowH;
var
  w: TQtWidget;
begin
  Result := nil;
  if (AForm = nil) or not AForm.HandleAllocated then
    Exit;
  w := TQtWidget(AForm.Handle);
  if (w = nil) or (w.Widget = nil) then
    Exit;
  Result := QWidget_windowHandle(w.Widget);
end;
{$endif}

{$ifdef LCLGTK3}
function Gtk3TopWindow(AForm: TCustomForm): PGtkWindow;
var
  w: PGtkWidget;
begin
  Result := nil;
  if (AForm = nil) or not AForm.HandleAllocated then
    Exit;
  w := TGtk3Widget(AForm.Handle).Widget;
  if w = nil then
    Exit;
  Result := PGtkWindow(w);
end;
{$endif}

function SlickeStartSystemMove(AForm: TCustomForm): boolean;
{$ifdef LCLQt6}
var
  win: QWindowH;
begin
  Result := false;
  win := QtTopWindow(AForm);
  if win <> nil then
    Result := QWindow_startSystemMove(win);
end;
{$else}
{$ifdef LCLGTK3}
var
  win: PGtkWindow;
  p: TPoint;
begin
  Result := false;
  win := Gtk3TopWindow(AForm);
  if win = nil then
    Exit;
  p := Mouse.CursorPos;
  // The real event timestamp matters: GDK_CURRENT_TIME makes some compositors
  // reject the grab.
  gtk_window_begin_move_drag(win, 1, p.x, p.y, gtk_get_current_event_time);
  Result := true;
end;
{$else}
begin
  Result := false; // No system move on this widgetset; callers fall back.
end;
{$endif}
{$endif}

function SlickeStartSystemResize(AForm: TCustomForm; Edges: TSlickeResizeEdges): boolean;
{$ifdef LCLQt6}
var
  win: QWindowH;
  e: QtEdge;
begin
  Result := false;
  if Edges = [] then
    Exit;
  win := QtTopWindow(AForm);
  if win = nil then
    Exit;
  e := 0;
  if sreTop in Edges then
    e := e or QtTopEdge;
  if sreLeft in Edges then
    e := e or QtLeftEdge;
  if sreRight in Edges then
    e := e or QtRightEdge;
  if sreBottom in Edges then
    e := e or QtBottomEdge;
  Result := QWindow_startSystemResize(win, e);
end;
{$else}
{$ifdef LCLGTK3}
var
  win: PGtkWindow;
  p: TPoint;
  edge: TGdkWindowEdge;
begin
  Result := false;
  win := Gtk3TopWindow(AForm);
  if win = nil then
    Exit;
  // GDK wants a single compass direction rather than an edge set.
  if Edges = [sreTop, sreLeft] then
    edge := GDK_WINDOW_EDGE_NORTH_WEST
  else
  if Edges = [sreTop, sreRight] then
    edge := GDK_WINDOW_EDGE_NORTH_EAST
  else
  if Edges = [sreBottom, sreLeft] then
    edge := GDK_WINDOW_EDGE_SOUTH_WEST
  else
  if Edges = [sreBottom, sreRight] then
    edge := GDK_WINDOW_EDGE_SOUTH_EAST
  else
  if Edges = [sreTop] then
    edge := GDK_WINDOW_EDGE_NORTH
  else
  if Edges = [sreBottom] then
    edge := GDK_WINDOW_EDGE_SOUTH
  else
  if Edges = [sreLeft] then
    edge := GDK_WINDOW_EDGE_WEST
  else
  if Edges = [sreRight] then
    edge := GDK_WINDOW_EDGE_EAST
  else
    Exit;
  p := Mouse.CursorPos;
  gtk_window_begin_resize_drag(win, edge, 1, p.x, p.y, gtk_get_current_event_time);
  Result := true;
end;
{$else}
begin
  Result := false;
end;
{$endif}
{$endif}

{------------------------------------------------------------------------------
  Small color helpers (self-contained on purpose — no trndi dependencies)
 ------------------------------------------------------------------------------}

function MixColors(const A, B: TColor; const Amount: double): TColor;
var
  ra, ga, ba, rb, gb, bb: byte;
  ca, cb: TColor;
begin
  ca := ColorToRGB(A);
  cb := ColorToRGB(B);
  ra := Red(ca);
  ga := Green(ca);
  ba := Blue(ca);
  rb := Red(cb);
  gb := Green(cb);
  bb := Blue(cb);
  Result := RGBToColor(
    EnsureRange(Round(ra + (rb - ra) * Amount), 0, 255),
    EnsureRange(Round(ga + (gb - ga) * Amount), 0, 255),
    EnsureRange(Round(ba + (bb - ba) * Amount), 0, 255));
end;

{------------------------------------------------------------------------------
  TSlickeTitleBar
 ------------------------------------------------------------------------------}

constructor TSlickeTitleBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBg := RGBToColor(32, 32, 32);
  FText := clWhite;
  FTitleAlignment := taLeftJustify;
  FButtons := [stbMinimize, stbMaximize, stbClose];
  FHoverBtn := -1;
  FPressedBtn := -1;
  Align := alTop;
  Height := 30; // placeholder until UpdateMetrics runs with a parent font
  Cursor := crDefault;
end;

procedure TSlickeTitleBar.UpdateMetrics;
var
  bmp: TBitmap;
  th: integer;
begin
  // Measure on a scratch bitmap: control canvases are only valid while
  // painting on some widgetsets, and a fresh bitmap must be sized before its
  // canvas is touched (GTK3 rejects 0x0 surfaces).
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Assign(Font);
    th := bmp.Canvas.TextHeight('Wg');
  finally
    bmp.Free;
  end;
  FMetricHeight := Max(24, th + 12);
  Height := FMetricHeight;
end;

procedure TSlickeTitleBar.Resize;
begin
  inherited Resize;
  // The bar owns its height. LCL's DPI auto-adjust rescales runtime-created
  // controls on monitor scale events (Wayland fires these liberally, and
  // fractional setups can fire them repeatedly), which compounded the height
  // on every pass. Re-derive from the font instead of trusting the scaled
  // value: on a genuine DPI change the font was scaled too, so the re-measure
  // lands on the correct new height; on a spurious event it heals back.
  if (FMetricHeight > 0) and (Height <> FMetricHeight) then
    UpdateMetrics;
end;

procedure TSlickeTitleBar.SetColors(ABg, AText: TColor);
begin
  if (FBg = ABg) and (FText = AText) then
    Exit;
  FBg := ABg;
  FText := AText;
  Invalidate;
end;

procedure TSlickeTitleBar.RefreshTitle;
begin
  Invalidate;
end;

procedure TSlickeTitleBar.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TSlickeTitleBar.SetTitleAlignment(const AValue: TAlignment);
begin
  if FTitleAlignment = AValue then
    Exit;
  FTitleAlignment := AValue;
  Invalidate;
end;

procedure TSlickeTitleBar.SetButtons(const AValue: TSlickeTitleBarButtons);
begin
  if FButtons = AValue then
    Exit;
  FButtons := AValue;
  FHoverBtn := -1;
  FPressedBtn := -1;
  Invalidate;
end;

function TSlickeTitleBar.ButtonCount: integer;
var
  b: TSlickeTitleBarButton;
begin
  Result := 0;
  for b in FButtons do
    Inc(Result);
end;

function TSlickeTitleBar.ButtonKind(AIndex: integer): TSlickeTitleBarButton;
var
  b: TSlickeTitleBarButton;
  i: integer;
begin
  // Fixed left-to-right order: minimize, maximize, close.
  Result := stbClose;
  i := 0;
  for b := Low(TSlickeTitleBarButton) to High(TSlickeTitleBarButton) do
    if b in FButtons then
    begin
      if i = AIndex then
        Exit(b);
      Inc(i);
    end;
end;

function TSlickeTitleBar.ButtonRect(AIndex: integer): TRect;
var
  bw, right: integer;
begin
  // Full-height hit zones (easy targets) sized for the round discs drawn
  // inside them, plus a small right margin so the last disc isn't flush
  // against the window edge.
  bw := Round(Height * 1.15);
  right := ClientWidth - (ButtonCount - 1 - AIndex) * bw - (Height div 6);
  Result := Rect(right - bw, 0, right, Height);
end;

function TSlickeTitleBar.ButtonAt(X, Y: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to ButtonCount - 1 do
    if PtInRect(ButtonRect(i), Point(X, Y)) then
      Exit(i);
end;

procedure TSlickeTitleBar.Paint;
var
  i, gs, ds, cx, cy: integer;
  r, gr: TRect;
  kind: TSlickeTitleBarButton;
  fillC, glyphC: TColor;
  s: string;
  frm: TCustomForm;
  ts: TTextStyle;
begin
  Canvas.Brush.Color := FBg;
  Canvas.FillRect(ClientRect);

  // Caption text, centered over the whole bar but clipped clear of the buttons.
  s := FTitle;
  frm := GetParentForm(Self);
  if (s = '') and (frm <> nil) then
    s := frm.Caption;
  if s <> '' then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Font.Color := FText;
    Canvas.Brush.Style := bsClear;
    ts := Canvas.TextStyle;
    ts.Alignment := FTitleAlignment;
    ts.Layout := tlCenter;
    ts.SingleLine := true;
    ts.Clipping := true;
    ts.EndEllipsis := true;
    r := Rect(Height div 2, 0,
      ClientWidth - ButtonCount * Round(Height * 1.15) - Height div 4, Height);
    Canvas.TextRect(r, r.Left, 0, s, ts);
    Canvas.Brush.Style := bsSolid;
  end;

  // Buttons in the KDE/Breeze-flat style: bare glyphs at rest, and hover or
  // press shows a soft rounded-square highlight behind the glyph (red for
  // close). Rectangles rather than discs on purpose — the LCL canvas draws
  // unantialiased ellipses, which read as jagged blobs at this size. Glyphs
  // are line-drawn (fonts are not trustworthy for these three shapes across
  // Linux font setups).
  for i := 0 to ButtonCount - 1 do
  begin
    r := ButtonRect(i);
    kind := ButtonKind(i);
    glyphC := FText;
    fillC := FBg;
    if (i = FPressedBtn) and (kind = stbClose) then
    begin
      fillC := RGBToColor(160, 18, 35);
      glyphC := clWhite;
    end
    else
    if (i = FHoverBtn) and (kind = stbClose) then
    begin
      fillC := RGBToColor(199, 22, 43);
      glyphC := clWhite;
    end
    else
    if i = FPressedBtn then
      fillC := MixColors(FBg, FText, 0.26)
    else
    if i = FHoverBtn then
      fillC := MixColors(FBg, FText, 0.16);

    cx := (r.Left + r.Right) div 2;
    cy := Height div 2;
    if (i = FHoverBtn) or (i = FPressedBtn) then
    begin
      // Highlight square, slightly larger than the glyph, gently rounded.
      ds := Round(Height * 0.72);
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := fillC;
      Canvas.Pen.Style := psClear;
      Canvas.RoundRect(cx - ds div 2, cy - ds div 2,
        cx - ds div 2 + ds, cy - ds div 2 + ds, ds div 4, ds div 4);
      Canvas.Pen.Style := psSolid;
    end;

    gs := Max(8, Height div 3);
    gr := Rect(cx - gs div 2, cy - gs div 2, cx - gs div 2 + gs, cy - gs div 2 + gs);
    Canvas.Pen.Color := glyphC;
    Canvas.Pen.Width := Max(1, Height div 24);
    Canvas.Brush.Style := bsClear;
    case kind of
    stbMinimize:
      Canvas.Line(gr.Left, cy, gr.Right, cy);
    stbMaximize:
      if (GetParentForm(Self) <> nil) and
        (GetParentForm(Self).WindowState = wsMaximized) then
      begin
          // "Restore": two offset squares (brush is bsClear, so Rectangle
          // draws outlines only).
        Canvas.Rectangle(gr.Left, gr.Top + 2, gr.Right - 2, gr.Bottom);
        Canvas.MoveTo(gr.Left + 2, gr.Top + 2);
        Canvas.LineTo(gr.Left + 2, gr.Top);
        Canvas.LineTo(gr.Right, gr.Top);
        Canvas.LineTo(gr.Right, gr.Bottom - 2);
        Canvas.LineTo(gr.Right - 2, gr.Bottom - 2);
      end
      else
        Canvas.Rectangle(gr.Left, gr.Top, gr.Right, gr.Bottom);
    stbClose:
    begin
      Canvas.Line(gr.Left, gr.Top, gr.Right, gr.Bottom);
      Canvas.Line(gr.Left, gr.Bottom, gr.Right, gr.Top);
    end;
    end;
    Canvas.Brush.Style := bsSolid;
  end;

  // No separator along the bottom edge on purpose: DWM/Cocoa draw a colored
  // caption flush against the client area, and full-color mode wants the same
  // seamless single-surface look here.
end;

procedure TSlickeTitleBar.DoButtonAction(AKind: TSlickeTitleBarButton);
var
  frm: TCustomForm;
begin
  frm := GetParentForm(Self);
  case AKind of
  stbClose:
    if Assigned(FOnCloseRequest) then
      FOnCloseRequest(Self)
    else
    if frm <> nil then
      frm.Close;
  stbMinimize:
    if Assigned(FOnMinimizeRequest) then
      FOnMinimizeRequest(Self)
    else
    if frm <> nil then
      frm.WindowState := wsMinimized;
  stbMaximize:
    if Assigned(FOnMaximizeRequest) then
      FOnMaximizeRequest(Self)
    else
    if frm <> nil then
      if frm.WindowState = wsMaximized then
        frm.WindowState := wsNormal
      else
        frm.WindowState := wsMaximized;
  end;
end;

procedure TSlickeTitleBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
var
  frm: TCustomForm;
begin
  inherited MouseDown(Button, Shift, X, Y);
  // A compositor-side move/resize swallows the matching mouse-up, so stale
  // state from the previous drag must never gate this press.
  FMaybeDrag := false;
  FManualDrag := false;
  if Button <> mbLeft then
    Exit;
  FPressedBtn := ButtonAt(X, Y);
  if FPressedBtn >= 0 then
  begin
    Invalidate;
    Exit;
  end;
  frm := GetParentForm(Self);
  if frm = nil then
    Exit;
  FMaybeDrag := true;
  FDownScreen := Mouse.CursorPos;
  FFormOrigin := Point(frm.Left, frm.Top);
end;

procedure TSlickeTitleBar.MouseMove(Shift: TShiftState; X, Y: integer);
var
  p: TPoint;
  frm: TCustomForm;
  hov: integer;
begin
  inherited MouseMove(Shift, X, Y);
  frm := GetParentForm(Self);

  if FManualDrag and (frm <> nil) then
  begin
    p := Mouse.CursorPos;
    frm.SetBounds(FFormOrigin.x + (p.x - FDownScreen.x),
      FFormOrigin.y + (p.y - FDownScreen.y), frm.Width, frm.Height);
    Exit;
  end;

  if FMaybeDrag and (frm <> nil) then
  begin
    p := Mouse.CursorPos;
    if (Abs(p.x - FDownScreen.x) >= DRAG_SLOP) or
      (Abs(p.y - FDownScreen.y) >= DRAG_SLOP) then
    begin
      FMaybeDrag := false;
      if SlickeStartSystemMove(frm) then
      begin
        // The compositor owns the pointer now; no mouse-up will arrive. Drop
        // the implicit capture so the next press starts clean.
        SetCaptureControl(nil);
        Exit;
      end;
      FManualDrag := true;
    end;
    Exit;
  end;

  hov := ButtonAt(X, Y);
  if hov <> FHoverBtn then
  begin
    FHoverBtn := hov;
    Invalidate;
  end;
end;

procedure TSlickeTitleBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
var
  hit: integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMaybeDrag := false;
  FManualDrag := false;
  if Button <> mbLeft then
    Exit;
  if FPressedBtn >= 0 then
  begin
    hit := ButtonAt(X, Y);
    if hit = FPressedBtn then
      DoButtonAction(ButtonKind(hit));
    FPressedBtn := -1;
    Invalidate;
  end;
end;

procedure TSlickeTitleBar.MouseLeave;
begin
  inherited MouseLeave;
  if (FHoverBtn >= 0) or (FPressedBtn >= 0) then
  begin
    FHoverBtn := -1;
    FPressedBtn := -1;
    Invalidate;
  end;
end;

procedure TSlickeTitleBar.DblClick;
begin
  inherited DblClick;
  // Double-click on the free area toggles maximize, like a native bar. A
  // double-click on a button never reaches here: mouse-down armed the button.
  if FPressedBtn < 0 then
    DoButtonAction(stbMaximize);
end;

{------------------------------------------------------------------------------
  TSlickeWindowGrips
 ------------------------------------------------------------------------------}

type
  {** One invisible resize strip. Graphic control: no handle, no paint, just
      a cursor and a mouse-down that hands the resize to the compositor (with
      a manual fallback). }
TSlickeGrip = class(TGraphicControl)
private
  FEdges: TSlickeResizeEdges;
  FResizing: boolean;
  FDownScreen: TPoint;
  FStartBounds: TRect;
protected
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
end;

procedure TSlickeGrip.MouseDown(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
var
  frm: TCustomForm;
begin
  inherited MouseDown(Button, Shift, X, Y);
  FResizing := false;
  if Button <> mbLeft then
    Exit;
  frm := GetParentForm(Self);
  if frm = nil then
    Exit;
  if SlickeStartSystemResize(frm, FEdges) then
  begin
    SetCaptureControl(nil);
    Exit;
  end;
  FResizing := true;
  FDownScreen := Mouse.CursorPos;
  FStartBounds := frm.BoundsRect;
end;

procedure TSlickeGrip.MouseMove(Shift: TShiftState; X, Y: integer);
var
  frm: TCustomForm;
  p: TPoint;
  dx, dy: integer;
  r: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  if not FResizing then
    Exit;
  frm := GetParentForm(Self);
  if frm = nil then
    Exit;
  p := Mouse.CursorPos;
  dx := p.x - FDownScreen.x;
  dy := p.y - FDownScreen.y;
  r := FStartBounds;
  if sreLeft in FEdges then
    r.Left := Min(r.Left + dx, r.Right - MIN_FORM_W);
  if sreRight in FEdges then
    r.Right := Max(r.Right + dx, r.Left + MIN_FORM_W);
  if sreTop in FEdges then
    r.Top := Min(r.Top + dy, r.Bottom - MIN_FORM_H);
  if sreBottom in FEdges then
    r.Bottom := Max(r.Bottom + dy, r.Top + MIN_FORM_H);
  frm.BoundsRect := r;
end;

procedure TSlickeGrip.MouseUp(Button: TMouseButton; Shift: TShiftState;
X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FResizing := false;
end;

constructor TSlickeWindowGrips.Create(AOwner: TComponent);
const
  GRIP_EDGES: array[0..7] of TSlickeResizeEdges = (
    [sreTop, sreLeft], [sreTop, sreRight], [sreBottom, sreLeft],
    [sreBottom, sreRight],
    [sreLeft], [sreTop], [sreRight], [sreBottom]);
  GRIP_CURSORS: array[0..7] of TCursor = (
    crSizeNWSE, crSizeNESW, crSizeNESW, crSizeNWSE,
    crSizeWE, crSizeNS, crSizeWE, crSizeNS);
var
  i: integer;
  g: TSlickeGrip;
begin
  inherited Create(AOwner);
  FActive := true;
  FForm := AOwner as TCustomForm;
  SetLength(FGrips, Length(GRIP_EDGES));
  for i := 0 to High(GRIP_EDGES) do
  begin
    g := TSlickeGrip.Create(Self);
    g.FEdges := GRIP_EDGES[i];
    g.Cursor := GRIP_CURSORS[i];
    g.Parent := FForm;
    FGrips[i] := g;
  end;
  UpdateLayout;
  FForm.AddHandlerOnResize(@FormResized);
end;

destructor TSlickeWindowGrips.Destroy;
begin
  if Assigned(FForm) and not (csDestroying in FForm.ComponentState) then
    FForm.RemoveHandlerOnResize(@FormResized);
  inherited Destroy;
end;

procedure TSlickeWindowGrips.FormResized(Sender: TObject);
begin
  UpdateLayout;
end;

procedure TSlickeWindowGrips.UpdateLayout;
var
  w, h, i: integer;
  vis: boolean;
begin
  if (FForm = nil) or (Length(FGrips) < 8) then
    Exit;
  w := FForm.ClientWidth;
  h := FForm.ClientHeight;
  // Corners first (indices 0..3), edges between them (4..7).
  FGrips[0].SetBounds(0, 0, GRIP_CORNER, GRIP_CORNER);
  FGrips[1].SetBounds(w - GRIP_CORNER, 0, GRIP_CORNER, GRIP_CORNER);
  FGrips[2].SetBounds(0, h - GRIP_CORNER, GRIP_CORNER, GRIP_CORNER);
  FGrips[3].SetBounds(w - GRIP_CORNER, h - GRIP_CORNER, GRIP_CORNER, GRIP_CORNER);
  FGrips[4].SetBounds(0, GRIP_CORNER, GRIP_THICKNESS, Max(0, h - 2 * GRIP_CORNER));
  FGrips[5].SetBounds(GRIP_CORNER, 0, Max(0, w - 2 * GRIP_CORNER), GRIP_THICKNESS);
  FGrips[6].SetBounds(w - GRIP_THICKNESS, GRIP_CORNER, GRIP_THICKNESS,
    Max(0, h - 2 * GRIP_CORNER));
  FGrips[7].SetBounds(GRIP_CORNER, h - GRIP_THICKNESS,
    Max(0, w - 2 * GRIP_CORNER), GRIP_THICKNESS);
  // Resizing a maximized/fullscreen window makes no sense and the strips
  // would shadow real controls along the edges.
  vis := FActive and (FForm.WindowState = wsNormal);
  for i := 0 to High(FGrips) do
    FGrips[i].Visible := vis;
  EnsureOnTop;
end;

procedure TSlickeWindowGrips.EnsureOnTop;
var
  i: integer;
begin
  for i := 0 to High(FGrips) do
    FGrips[i].BringToFront;
end;

procedure TSlickeWindowGrips.SetActive(AActive: boolean);
begin
  FActive := AActive;
  UpdateLayout;
end;

{------------------------------------------------------------------------------
  SlickeDressWithTitleBar
 ------------------------------------------------------------------------------}

function SlickeDressWithTitleBar(AForm: TCustomForm; ABarBg, ABarText: TColor;
AButtons: TSlickeTitleBarButtons): TSlickeTitleBar;
var
  off, i: integer;
  c: TControl;
  wasSizeable: boolean;
begin
  Result := nil;
  if AForm = nil then
    Exit;
  // A form that is already frameless chose that on purpose (fullscreen
  // overlays); and a form that already carries a bar must not get another.
  if AForm.BorderStyle = bsNone then
    Exit;
  for i := 0 to AForm.ControlCount - 1 do
    if AForm.Controls[i] is TSlickeTitleBar then
      Exit;

  wasSizeable := AForm.BorderStyle in [bsSizeable, bsSizeToolWin];
  AForm.BorderStyle := bsNone;
  Result := TSlickeTitleBar.Create(AForm);
  Result.Align := alNone; // callers lay out with absolute coords/anchors
  Result.Parent := AForm;
  Result.Font.Assign(AForm.Font);
  Result.Font.Height := 0;
  Result.UpdateMetrics;
  off := Result.Height;
  Result.SetBounds(0, 0, AForm.ClientWidth, off);
  Result.Anchors := [akLeft, akTop, akRight];
  Result.Buttons := AButtons;
  Result.SetColors(ABarBg, ABarText);

  // Make room: the content was laid out against the undecorated client area.
  for i := 0 to AForm.ControlCount - 1 do
  begin
    c := AForm.Controls[i];
    if c = Result then
      Continue;
    if c.Align = alNone then
    begin
      // Purely bottom-anchored controls follow the Height increase below on
      // their own; shifting them here too would move them twice.
      if (akTop in c.Anchors) or not (akBottom in c.Anchors) then
        c.Top := c.Top + off;
    end
    else
    if (c.Align in [alTop, alClient, alLeft, alRight]) and (c.Top < off) then
      c.BorderSpacing.Top := c.BorderSpacing.Top + off;
  end;
  AForm.Height := AForm.Height + off;
  Result.BringToFront;

  // A form that was resizable keeps that ability: without its native frame
  // there are no compositor resize edges, so give it grips. Owned by the
  // form; freed with it.
  if wasSizeable then
    TSlickeWindowGrips.Create(AForm);
end;

end.
