unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  dexapi, nsapi,
  trndi.types, math, DateUtils, FileUtil,
  TrndiExt.Engine,
  TrndiExt.Ext,
  trndiExt.jsfuncs,
  LazFileUtils;

type

  TTrendProc = procedure(l: TLabel; c, ix: integer) of object;
  TTrendProcLoop = procedure(l: TLabel; c, ix: integer; ls: array of TLabel) of object;


  { TfBG }

  TfBG = class(TForm)
    lArrow: TLabel;
    lDiff: TLabel;
    lDot1: TLabel;
    lDot10: TLabel;
    lDot2: TLabel;
    lDot3: TLabel;
    lDot4: TLabel;
    lDot5: TLabel;
    lDot6: TLabel;
    lDot7: TLabel;
    lDot8: TLabel;
    lDot9: TLabel;
    lVal: TLabel;
    tMain: TTimer;
    procedure FormCreate(Sender:TObject);
    procedure FormResize(Sender: TObject);
    procedure lDiffDblClick(Sender: TObject);
    procedure lgMainClick(Sender: TObject);
    procedure lValStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure onTrendClick(Sender: TObject);
    procedure tMainTimer(Sender: TObject);
  private
    procedure update;
    procedure actOnTrend(proc: TTrendProc);
    procedure actOnTrend(proc: TTrendProcLoop);
    procedure setDotWidth(l: TLabel; c, ix: integer; ls: array of TLabel);
    procedure HideDot(l: TLabel; c, ix: integer);
    procedure ResizeDot(l: TLabel; c, ix: integer);
    procedure ExpandDot(l: TLabel; c, ix: integer);
    procedure LoadExtensions;
  public

  end;

const
  bgmin = 2;
  bgmax = 22;
  bgcok = $0000DC84;
  bgcoktxt = $00F2FFF2;
  bgchi = $0007DAFF;
  bgchitxt = $00F3FEFF;
  bgclo = $00FFBE0B;
  bgclotxt = $00FFFEE9;


var
  fBG: TfBG;
  api: nsapi.NightScout;
  un: BGUnit = BGUnit.mmol;
  bgs: BGResults;
  jsFuncs:  TJSfuncs;

implementation

{$R *.lfm}
{$I tfuncs.inc}


procedure TfBG.LoadExtensions;
var
 exts : TStringList;
 s, extdir: string;
begin
 TTrndiExtEngine.Instance;
 jsFuncs := TJSfuncs.Create(api); // This is an Object, not a class!
 extdir := GetAppConfigDirUTF8(false, true) + 'extensions' + DirectorySeparator; // Find extensions folder
 ShowMessage(extdir);
 ForceDirectoriesUTF8(extdir);
 exts := FindAllFiles(extdir, '*.js', false); // Find .js

 with TTrndiExtEngine.Instance do begin
  addFunction('tProp', ExtFunction(@JSUX), 3); // 2 args
   for s in exts do
     ExecuteFile(s);
   exts.Free;
 end;
end;

procedure TfBG.actOnTrend(proc: TTrendProcLoop);
var
  ix, lx: integer;
  ls: array of TLabel;
begin
  ls := [lDot1,lDot2,lDot3,lDot4,lDot5,lDot6,lDot7,lDot8,lDot9,lDot10]; // All dots that make up the graph
  lx := length(ls);
  for ix:= 0 to length(ls)-1 do
      proc(ls[ix], lx, ix, ls); // Run the method on the given label
end;

procedure TfBG.actOnTrend(proc: TTrendProc);
var
  ix, lx: integer;
  ls: array of TLabel;
begin
  ls := [lDot1,lDot2,lDot3,lDot4,lDot5,lDot6,lDot7,lDot8,lDot9,lDot10];
  lx := length(ls);
  for ix:= 0 to length(ls)-1 do
      proc(ls[ix], lx, ix);
end;

procedure TfBG.FormCreate(Sender:TObject);
var
  s: string;
{$ifdef Linux}
  function GetLinuxDistro: String;
  const
    Issue = '/etc/os-release';
  begin
    case FileExists(Issue) of
      True:  Result := ReadFileToString(Issue);
      False: Result := '';
    end;
  end;

begin
  s := GetLinuxDistro;
  if (Pos('ID=fedora', s) > -1) then
    s := 'Poppins'
  else if (Pos('ID=ubuntu', s) > -1) then
    s := 'Ubuntu'
  else
    s := 'default';
  fBG.Font.Name := s;
  {$else}
  begin
  {$endif}
  api := NightScout.create('https://***REMOVED***','***REMOVED***', '');
  if not api.connect then begin
    ShowMessage(api.errormsg);
    exit;
  end;
  LoadExtensions;
  update;
end;


procedure TfBG.ExpandDot(l: TLabel; c, ix: integer);
begin
  if l.Caption = '•' then
    l.Caption := l.Hint
  else
    l.Caption := '•';
end;

procedure TfBG.HideDot(l: TLabel; c, ix: integer);
begin
  l.Visible:=false;
end;

procedure TfBG.ResizeDot(l: TLabel; c, ix: integer);
begin
  l.AutoSize := true;
  l.font.Size := lVal.Font.Size div 8;

end;

procedure TfBG.SetDotWidth(l: TLabel; c, ix: integer; ls: array of TLabel);
var
  i: integer;
begin
  i := fBG.Width div c;

    if ix > 0 then
      l.left := ls[ix-1].left + i
    else
      l.left := i div 2;
end;

procedure TfBG.FormResize(Sender: TObject);
  procedure scaleLbl(ALabel: TLabel);
  var
  MaxWidth, MaxHeight: Integer;
  TestSize, MaxFontSize: Integer;
  TextWidth, TextHeight: Integer;
begin
  // Maximal fontstorlek att testa
  MaxFontSize := 150;

  // Hämta etikettens maximala storlek
  MaxWidth := ALabel.Width;
  MaxHeight := ALabel.Height;

  // Kontrollera om texten passar redan från början
  for TestSize := 1 to MaxFontSize do
  begin
    ALabel.Font.Size := TestSize;
    TextWidth := ALabel.Canvas.TextWidth(ALabel.Caption);
    TextHeight := ALabel.Canvas.TextHeight(ALabel.Caption);

    // Bryt om texten inte längre får plats
    if (TextWidth > MaxWidth) or (TextHeight > MaxHeight) then
    begin
      ALabel.Font.Size := TestSize - 1;
      Exit;
    end;
  end;

  // Sätt max fontstorlek om loopen aldrig bryts
  ALabel.Font.Size := MaxFontSize;
end;

  procedure SetHeight(L: TLabel; value: single);
  var
    Padding, UsableHeight, Position: Integer;
  begin
    if (Value >= 2) and (Value <= 22) then
    begin
      Padding := Round(fBG.ClientHeight * 0.1);   // 10% padding
      UsableHeight := fBG.ClientHeight - 2 * Padding;

      // Beräkna placering inom det användbara området
      Position := Padding + Round((Value - 2) / 20 * UsableHeight);

      L.Top := fBG.ClientHeight - Position;
    end
    else
      ShowMessage('Värdet måste vara mellan 2 och 22');
  end;


var
  l: TLabel;
  pos, i, x: integer;
  b: BGReading;
 r: single;
begin

 actOnTrend(@SetDotWidth);
 actOnTrend(@HideDot);

  pos := (bgmax - bgmin) * lDot1.Parent.ClientHeight;

  for i:= Low(bgs) to min(9, high(bgs)) do begin
     b := bgs[i];
     l := fBG.FindChildControl('lDot'+(10-i).ToString) as TLabel;

     r := b.convert(mmol);

     if r > bgmax then
       l.Visible := false
     else if r < bgmin then
       l.Visible := false
     else begin
       l.visible := true;
       setHeight(l, r);
     end;

     l.Hint := b.format(un, BG_MSG_SHORT , BGPrimary);;

     case b.level of
       BGValLevel.BGRange: l.Font.color := bgcoktxt;
       BGValLevel.BGRangeLO: l.Font.color := bgclotxt;
       BGValLevel.BGRangeHI: l.Font.color := bgchitxt;

       BGValLevel.BGHigh: l.Font.color := bgchitxt;
       BGValLevel.BGLOW: l.Font.color := bgclotxt;
       BGValLevel.BGNormal: l.Font.color := bgcoktxt;
     end;

  end;

  lArrow.Height := lVal.Height div 5;
  scaleLbl(lVal);
  scaleLbl(lArrow);
  lDiff.Width := ClientWidth;;
  lDiff.Height := lval.Height div 7;
  scaleLbl(lDiff);

  actOnTrend(@ResizeDot);

end;

procedure TfBG.lDiffDblClick(Sender: TObject);
begin
  if fBG.WindowState = wsMaximized then begin
     fBG.WindowState := wsNormal;
     fBG.FormStyle   := fsNormal;
     fBG.BorderStyle := bsSizeable;
  end else begin
     fBG.WindowState := wsMaximized;
     fBG.FormStyle   := fsStayOnTop;
     fBG.BorderStyle := bsNone;
end;

end;

procedure TfBG.lgMainClick(Sender: TObject);
begin

end;

procedure TfBG.lValStartDrag(Sender: TObject; var DragObject: TDragObject);
begin

end;



procedure TfBG.onTrendClick(Sender: TObject);
begin
  actOnTrend(@ExpandDot);
end;

procedure TfBG.tMainTimer(Sender: TObject);
begin
  update;
end;

procedure TfBG.update;
var
  i: integer;
  l: TLabel;
  b: BGReading;
begin
  bgs := api.getReadings(10,25);
  if length(bgs) < 1 then begin
    Showmessage('Ingen kontakt');
    Exit;
  end;
  b := bgs[low(bgs)];
  lVal.Caption := b.format(un, BG_MSG_SHORT , BGPrimary);
  lDiff.Caption := b.format(un, BG_MSG_SIG_SHORT, BGDelta);
  lArrow.Caption := b.trend.Img;
  lVal.Font.Style:= [];

  if MinutesBetween(Now, b.date) > 7 then begin
     lDiff.Caption := TimeToStr(b.date);
     lVal.Font.Style:= [fsStrikeOut];
     fBG.Color := clBlack;
     Exit;
  end;
  case b.level of
    BGValLevel.BGRange: fBG.Color := bgcok;
    BGValLevel.BGRangeLO: fBG.Color := bgclo;
    BGValLevel.BGRangeHI: fBG.Color := bgchi;

    BGValLevel.BGHigh: fBG.Color := bgchi;
    BGValLevel.BGLOW: fBG.Color := bgclo;
    BGValLevel.BGNormal: fBG.Color := bgcok;
  end;


end;

end.

