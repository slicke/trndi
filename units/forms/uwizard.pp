unit uwizard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IpHtml, trndi.native, slicke.ux.alert, slicke.ux.native, lclintf, trndi.strings;

type

  { TfWizard }

  TfWizard = class(TForm)
    bPrev: TButton;
    bNext: TButton;
    hMain: TIpHtmlPanel;
    pnCtrl: TPanel;
    pnPrev: TPanel;
    pnNext: TPanel;
    procedure bNextClick(Sender: TObject);
    procedure bPrevClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HTMLHotClick(Sender: TObject);
  private
    htmlpage: shortint;
    acol: string;
    procedure loadSlide(i: shortint);
    procedure SetHTML(const html: string; const page: shortint);
  public

  end;

const
  maxpage = 3;
var
  fWizard: TfWizard;

resourcestring
  RS_WELCOME_TOP = 'Welcome to Trndi!';
  RS_WELCOME_DESC = 'This dialog shows at first start, and when there have been major changes made';
  RS_WELCOME_THNX = 'Thank you for choosing Trndi! Trndi is a non-profit open source project and contributions are welcome.';
  RS_WELCOME_TITLE1 = 'Quick Guide:';
  RS_WELCOME_1 = 'Right-click Trndi to open the menu';
  RS_WELCOME_2 = 'You can change your time-in-range in settings';
  RS_WELCOME_3 = 'You can activate predictions in settings';
  RS_WELCOME_4 = 'Questions? Contact us at %s';
  RS_WELCOME_5 = 'Bugs? Report them at %s';

  RS_W3_TITLE = 'Good to go!';
  RS_W3_COMPLETE = 'Everything''s ready! Welcome to Trndi!';
  RS_W3_CLOSE = 'You may now close this guide';

  RS_BTN_NEXT = 'Next';
implementation

{$R *.lfm}

{ TfWizard }

procedure TfWizard.SetHTML(const html: string; const page: shortint);
var
  bg, fg, str, sysfont: string;
begin
    htmlpage := page;
    getColorScheme(bg, fg);
    str := '<html><body bgcolor="' + bg+ '" text="' + fg + '"';
    if FontTXTInList(sysfont) then
      str += ' style="font-family: ' + sysfont + ';"';
    hMain.SetHtmlFromStr(Format('%s>%s</body></html>', [str, html]));
end;

procedure TfWizard.FormCreate(Sender: TObject);
begin
end;

procedure TfWizard.bNextClick(Sender: TObject);
begin
  loadSlide(htmlpage+1);
  bPrev.Enabled := htmlpage > 1;
  if htmlpage = maxpage then begin
    bNext.Enabled := false;
    bNext.Caption := smbUXClose;
  end
  else begin
    bNext.Enabled := true;
    bNext.Caption := RS_BTN_NEXT;
  end;
end;

procedure TfWizard.bPrevClick(Sender: TObject);
begin
  LoadSlide(htmlpage-1);
  bPrev.Enabled := htmlpage > 1;
  if htmlpage = maxpage then begin
    bNext.Enabled := false;
    bNext.Caption := smbUXClose;
  end
  else begin
    bNext.Enabled := true;
    bNext.Caption := RS_BTN_NEXT;
  end;
end;

procedure TfWizard.FormShow(Sender: TObject);
begin
    if trndinative.isDarkMode then begin
      TrndiNative.SetDarkMode(self.handle);
      pnCtrl.Color := clBackground;
      acol := ' style="color: #FFFFFF;"';
    end else
      acol := ' style="color: #000000;"';
    LoadSlide(1);
end;

procedure TfWizard.loadSlide(i: shortint);
  function E(const S: string): string;
  begin
    Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
    Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
  end;
begin
  case i of
    1: SetHTML(Format('<h1>%s</h1><center><div style="background-color: #EDC2B7; color: #000000;">%s</div></center><br><br>%s<h3>%s</h3>' +
    '<ul>'+
    '<li>%s</li>' +
    '<li>%s</li>' +
    '<li>%s</li>' +
    '<li>%s</li>' +
    '<li>%s</li>', [e(RS_WELCOME_TOP), e(RS_WELCOME_DESC), StringReplace(e(RS_WELCOME_THNX), '!', '!<br>', []), e(RS_WELCOME_TITLE1), e(RS_WELCOME_1), e(RS_WELCOME_2), e(RS_WELCOME_3), Format(e(RS_WELCOME_4), ['<a'+acol+' href="https://discord.gg/sda8nzFt">Discord</a>']), Format(e(RS_WELCOME_5), ['<a'+acol+' href="https://github.com/slicke/trndi/issues">GitHub</a>'])]),i);
    2: SetHTML( '<h1>About readings</h1><li>Data displayed may be inaccurate, delayed, or unavailable</li>' +
    '<li> Always verify readings with your official CGM device</li>' +
    '<li> Never make medical decisions based solely on this app</li>' +
    '<li> Consult healthcare professionals for medical advice</li>'+
    '</ul>' + sHTMLLineBreak +
    '<h3>BY USING THIS APP, YOU ACKNOWLEDGE THAT</h3>' + sHTMLLineBreak +
    '- The developers assume NO LIABILITY for any harm, injury, or damages' + sHTMLLineBreak +
    '- You use this app entirely at your own risk' + sHTMLLineBreak +
    '- This app may contain bugs or errors that could display incorrect data' + sHTMLLineBreak + sHTMLLineBreak +
    '<b><i>- IF YOU DO NOT AGREE WITH THESE TERMS, DO NOT USE THIS APP. -</i></b>', 2);
    3: SetHTML(Format('<h1>%s</h1>%s<br><br><i>%s</i>', [e(RS_W3_TITLE),e(RS_W3_COMPLETE),e(RS_W3_CLOSE)]), 3);
  end;
end;

procedure TfWizard.HTMLHotClick(Sender: TObject);
begin
 OpenURL((sender as TIpHtmlPanel).HotURL);
end;

end.

