
(*
 * This file is part of Trndi (https://github.com/slicke/trndi).
 * Copyright (c) 2021-2025 Björn Lindh.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ---------
 *
 * GitHub: https://github.com/slicke/trndi
 *)

unit uconf;

{$mode ObjFPC}{$H+}

interface

uses 
Classes,ComCtrls,ExtCtrls,Spin,StdCtrls,SysUtils,Forms,Controls,
Graphics,Dialogs,LCLTranslator, trndi.native, lclintf, slicke.ux.alert,
VersionInfo, trndi.funcs, buildinfo,
// Backend APIs for label captions
trndi.api, trndi.api.nightscout, trndi.api.nightscout3, trndi.api.dexcom, trndi.api.xdrip;

type

  { TfConf }

TfConf = class(TForm)
  bAdd: TButton;
  bBackendHelp: TButton;
  bOverrideHelp: TButton;
  bPrivacyHelp: TButton;
  bRemove: TButton;
  bSysNotice: TButton;
  bSysTouch: TButton;
  bTestSpeech: TButton;
  btUserSave: TButton;
  Button2: TButton;
  btReset: TButton;
  bTestAnnounce: TButton;
  Button3: TButton;
  Button4: TButton;
  bvExt: TBevel;
  bvExt1: TBevel;
  cbCust: TCheckBox;
  cbFlashLow: TCheckBox;
  cbFlashPerfect: TCheckBox;
  cbLang: TComboBox;
  cbMultiTouch: TCheckBox;
  cbMusicPause: TCheckBox;
  cbNotice: TCheckBox;
  cbOffBar: TCheckBox;
  cbPos: TComboBox;
  cbPrivacy: TCheckBox;
  cbSize: TCheckBox;
  cbSys: TComboBox;
  cbTIR: TCheckBox;
  cbTouch: TCheckBox;
  cbUser: TColorButton;
  cbUserColor: TCheckBox;
  cbTitleColor: TCheckBox;
  cbFlash: TCheckGroup;
  cbFlashHi: TCheckBox;
  cl_hi_bg: TColorButton;
  cl_hi_txt: TColorButton;
  cl_hi_txt_cust: TColorButton;
  cl_lo_bg: TColorButton;
  cl_lo_txt: TColorButton;
  cl_lo_txt_cust: TColorButton;
  cl_lo_bg_cust: TColorButton;
  cl_hi_bg_cust: TColorButton;
  cl_ok_bg: TColorButton;
  cl_ok_txt: TColorButton;
  eAddr: TEdit;
  edCommaSep: TEdit;
  eDot: TEdit;
  edNick: TEdit;
  eDotNow: TEdit;
  edTray: TSpinEdit;
  ePass: TEdit;
  eExt: TEdit;
  edMusicHigh: TEdit;
  edMusicLow: TEdit;
  edMusicPerfect: TEdit;
  fdFont:TFontDialog;
  fsHi: TFloatSpinEdit;
  fsLo: TFloatSpinEdit;
  gbColBack: TGroupBox;
  gbMulti: TGroupBox;
  gbOverride: TGroupBox;
  gbOverride2: TGroupBox;
  GroupBox1: TGroupBox;
  GroupBox2: TGroupBox;
  GroupBox3: TGroupBox;
  gbDisplayPrefs: TGroupBox;
  GroupBox4: TGroupBox;
  Image1: TImage;
  Label1: TLabel;
  Label16: TLabel;
  Label17: TLabel;
  Label18: TLabel;
  lProblematic: TLabel;
  lDot: TLabel;
  Label2: TLabel;
  lDot1: TLabel;
  lDot2: TLabel;
  lDot3: TLabel;
  lDotCurr: TLabel;
  lDotNow: TLabel;
  lWidgetset: TLabel;
  lOS: TLabel;
  Label3: TLabel;
  Label5: TLabel;
  lUserName: TLabel;
  lWaitSys: TLabel;
  Label10: TLabel;
  Label11: TLabel;
  Label12: TLabel;
  Label13: TLabel;
  Label14: TLabel;
  Label15: TLabel;
  lPass: TLabel;
  lExt: TLabel;
  lTestAnnounce: TLabel;
  lUserTrack: TLabel;
  Label4: TLabel;
  Label6: TLabel;
  Label8: TLabel;
  Label9: TLabel;
  lAck: TButton;
  lAgo: TLabel;
  lArrow: TLabel;
  lbUsers: TListBox;
  lCopyright: TLabel;
  lHiOver: TLabel;
  lLicense: TButton;
  lLounder: TLabel;
  lTestAnnounce1: TLabel;
  lTitle: TLabel;
  lTray: TLabel;
  lVal: TLabel;
  lVersion: TLabel;
  lArch: TLabel;
  Panel1: TPanel;
  Panel2: TPanel;
  Panel3: TPanel;
  Panel4: TPanel;
  Panel5: TPanel;
  Panel6: TPanel;
  Panel7: TPanel;
  Panel8: TPanel;
  Panel9: TPanel;
  pnBackend: TPanel;
  pnHelp: TPanel;
  pUserColor: TPanel;
  pnMisc: TPanel;
  pTray: TPanel;
  pcMain: TPageControl;
  pnDisplay: TPanel;
  pDecimal: TPanel;
  pUserNick: TPanel;
  pUserSave2: TPanel;
  rbUnit: TRadioGroup;
  spTHRESHOLD: TSpinEdit;
  tsColors: TTabSheet;
  tsCustom: TTabSheet;
  tsDisplay: TTabSheet;
  tsGeneral: TTabSheet;
  tsIntegration: TTabSheet;
  tsMulti: TTabSheet;
  tsSystem: TTabSheet;
  procedure bAddClick(Sender:TObject);
  procedure bLimitsClick(Sender:TObject);
  procedure bOverrideHelpClick(Sender:TObject);
  procedure bPrivacyHelpClick(Sender:TObject);
  procedure bRemoveClick(Sender:TObject);
  procedure bBackendHelpClick(Sender: TObject);
  procedure bSysNoticeClick(Sender: TObject);
  procedure bSysTouchClick(Sender: TObject);
  procedure bTestAnnounceClick(Sender: TObject);
  procedure bTestSpeechClick(Sender: TObject);
  procedure btResetClick(Sender: TObject);
  procedure btUserSaveClick(Sender: TObject);
  procedure Button1Click(Sender:TObject);
  procedure Button2Click(Sender:TObject);
  procedure Button3Click(Sender:TObject);
  procedure Button4Click(Sender: TObject);
  procedure cbCustChange(Sender:TObject);
  procedure cbSysChange(Sender:TObject);
  procedure cbUserClick(Sender:TObject);
  procedure cbUserColorChanged(Sender: TObject);
  procedure dotClick(Sender: TObject);
  procedure edNickChange(Sender: TObject);
  procedure eDotChange(Sender: TObject);
  procedure ePassEnter(Sender: TObject);
  procedure ePassExit(Sender: TObject);
  procedure FormCreate(Sender:TObject);
  procedure FormDestroy(Sender: TObject);
  procedure FormResize(Sender: TObject);
  procedure Label12Click(Sender: TObject);
  procedure lAckClick(Sender:TObject);
  procedure lbUsersEnter(Sender: TObject);
  procedure lbUsersSelectionChange(Sender: TObject; User: boolean);
  procedure lLicenseClick(Sender:TObject);
  procedure lValClick(Sender:TObject);
  procedure pcMainChange(Sender: TObject);
  procedure rbUnitClick(Sender:TObject);
  procedure spTHRESHOLDChange(Sender: TObject);
  procedure tbAdvancedChange(Sender:TObject);
  procedure ToggleBox1Change(Sender:TObject);
  procedure tsDisplayShow(Sender: TObject);
  procedure tsSystemShow(Sender: TObject);
private

public

end;

var
  tnative: TrndiNative;

resourcestring
RS_OVERRIDE_HELP =
  'Setting values here allows you to define your own high and low blood sugar limits in Trndi.'
  +#10+#10+
  'NightScout:'#10+
  'Trndi automatically retrieves your custom high and low settings from NightScout, so manually setting them here is usually unnecessary.'
  +#10+#10+
  'Dexcom:'+#10+
  'Dexcom servers do not provide custom high and low values. By setting them here, you can establish your own thresholds for Dexcom data.';
RS_OVERRIDE_NS =
  'You are using the NightScout backend, you should set these values on your server (if possible), as Trndi uses your NightScout preferences by default';

RS_PRIVACY_HELP =
  'When in Privacy Mode, the actual blood glucose value is hidden. Trndi will only tell the user if it''s good, high or low.';

RS_DEX =
  'Dexcom servers do not provide custom high and low blood sugar values. Please set your own thresholds at the bottom of this window.';

RS_ENTER_USER = 'Enter a username';
RS_ENTER_NAME = 'Letters, space and numbers only';
RS_ENTER_ANY = 'Please enter a name';
RS_DUPE_NAME = 'Names must be unique';
RS_CURRENT_ACC = 'This only applies for the current loaded account:  %s';
RS_CURRENT_ACC_NO = 'These settings are available when using a multi-account';
RS_CURRENT_ACC_DEF = 'Settings for "default" only apply while multi-user is active';
RS_REMOVE_ACC = 'Removed accounts are made inactive, and can be restored by adding the same name again';
RS_AUTO = 'Auto-detect';
RS_UPTODATE = 'You are up to date';
RS_NEWVER = 'Version %s is available, would you like to go to the downloads page?';
RS_NEWVER_PRE = 'A new pre-release for %s is available, would you like to go to the downloads page?';
RS_NEWVER_CAPTION = 'New version available';
RS_SELECT_FONT = 'Select a font';


RS_NOTIFICATIONS = 'Notifications';
RS_NOTIFY_TITLE = 'A notification system is required';
RS_NOTIFY_TXT = 'Trndi uses a system called "%s" to send desktop notices, you need to have this system installed in order to recieve notices.';
RS_NOTIFY_SYSTEM = 'Notifications will appear where you normally get notification messages.';

RS_HASTOUCH = 'Shows if Trndi detected a touch screen';

RS_Saftey_Hi = 'Trndi won''t allow a larger limit, for your own saftey. This can be overridden manually/via plugin';
RS_Saftey_Low = 'Trndi won''t allow a lower limit, the backend system only reports values every 5 minutes. 6 = one reading missing.';

// Backend-specific help texts
RS_HELP_NS_V2 =
  'NightScout v2 setup (use FULL access token):'+#13#10#13#10+
  '1) Open your NightScout site (e.g., https://your-site).'+#13#10+
  '2) Go to Admin -> Tokens — or API Secret.'+#13#10+
  '3) If you use Tokens:'+#13#10+
  '   - Create a token with at least READ scope.'+#13#10+
  '   - Copy the FULL access token value exactly as shown.'+#13#10+
  '4) In Trndi:'+#13#10+
  '   - Address: enter your NightScout URL'+#13#10+
  '   - Auth: paste the FULL access token (not just a suffix).'+#13#10+#13#10+
  'Note: If you instead use the legacy API Secret, paste your API Secret value as-is.';

RS_HELP_NS_V3 =
  '** ALPHA DRIVER - Please use "NightScout" for daily use! **'+#13#10+
  'NightScout v3 setup (use FULL access token):'+#13#10#13#10+
  '1) Open your NightScout site (e.g., https://your-site).'+#13#10+
  '2) Go to Admin -> Tokens — or API Secret.'+#13#10+
  '3) If you use Tokens:'+#13#10+
  '   - Create a token with at least READ scope.'+#13#10+
  '   - Copy the FULL access token value exactly as shown.'+#13#10+
  '4) In Trndi:'+#13#10+
  '   - Address: enter your NightScout URL'+#13#10+
  '   - Auth: paste the FULL access token.'+#13#10+#13#10+
  'Note: If you instead use the legacy API Secret, paste your API Secret value as-is.';

RS_HELP_DEX_REGION =
  'Dexcom region selection:'#13#10''#13#10'' +
  'Choose the server based on your account region:'+LineEnding +
  '• Dexcom (USA): for accounts served by share2.dexcom.com'+LineEnding +
  '• Dexcom (Outside USA): for accounts served by shareous1.dexcom.com'+LineEnding+LineEnding +
  'If you are unsure, try “Outside USA” first if you live outside the US.'+LineEnding +
  'Your username and password are your Dexcom Account (not Share) credentials.';

RS_HELP_XDRIP = 'xDrip setup:'#13#10''#13#10'Address: your xDrip REST endpoint (base URL).'#13#10'Auth: API secret (plain text; server hashes it).';
RS_DEFAULT_ACCOUNT = 'Default';
RS_CHOOSE_SYSTEM = 'Select your CGM system in the list, then enter your credentials';

RS_WAYLAND = 'Your desktop session is Wayland. Wayland compositors (including KWin) restrict third-party apps from forcing windows above others.'+LineEnding+
  'Right-click the title bar, select more options and check the always-on-top option (or similar)';
var 
fConf: TfConf;

procedure ListLanguageFiles(list: TStrings; const Path: string);
function GetLanguageName(const ACode: string): string;
function ExtractLangCode(const AText: string): string;

implementation

{$R *.lfm}

procedure Showmessage(const str: string);
begin
 UXMessage(uxdAuto, sSuccTitle, str, uxmtInformation);
end;

function ExtractLangCode(const AText: string): string;
var
  L, R: Integer;
begin
  L := LastDelimiter('(', AText);
  R := LastDelimiter(')', AText);
  if (L > 0) and (R > L) then
    Result := Copy(AText, L+1, R-L-1)
  else
    Result := '';
end;


function GetLanguageName(const ACode: string): string;
var
  C: string;
begin
  C := LowerCase(Trim(ACode));

  case C of
    'aa': Result := 'Afar';
    'ab': Result := 'Abkhazian';
    'ae': Result := 'Avestan';
    'af': Result := 'Afrikaans';
    'ak': Result := 'Akan';
    'sq': Result := 'Albanian';
    'am': Result := 'Amharic';
    'ar': Result := 'Arabic';
    'an': Result := 'Aragonese';
    'hy': Result := 'Armenian';
    'as': Result := 'Assamese';
    'av': Result := 'Avaric';
    'ay': Result := 'Aymara';
    'az': Result := 'Azerbaijani';
    'bm': Result := 'Bambara';
    'ba': Result := 'Bashkir';
    'eu': Result := 'Basque';
    'be': Result := 'Belarusian';
    'bn': Result := 'Bengali';
    'bh': Result := 'Bihari';
    'bi': Result := 'Bislama';
    'bs': Result := 'Bosnian';
    'br': Result := 'Breton';
    'bg': Result := 'Bulgarian';
    'my': Result := 'Burmese';
    'ca': Result := 'Catalan';
    'ch': Result := 'Chamorro';
    'ce': Result := 'Chechen';
    'ny': Result := 'Chichewa';        // also Nyanja
    'zh': Result := 'Chinese';
    'cu': Result := 'Church Slavonic';
    'cv': Result := 'Chuvash';
    'kw': Result := 'Cornish';
    'co': Result := 'Corsican';
    'cr': Result := 'Cree';
    'hr': Result := 'Croatian';
    'cs': Result := 'Czech';
    'da': Result := 'Danish';
    'dv': Result := 'Divehi';
    'nl': Result := 'Dutch';
    'dz': Result := 'Dzongkha';
    'en': Result := 'English';
    'eo': Result := 'Esperanto';
    'et': Result := 'Estonian';
    'ee': Result := 'Ewe';
    'fo': Result := 'Faroese';
    'fj': Result := 'Fijian';
    'fi': Result := 'Finnish';
    'fr': Result := 'French';
    'ff': Result := 'Fulah';
    'gl': Result := 'Galician';
    'ka': Result := 'Georgian';
    'de': Result := 'German';
    'el': Result := 'Greek';
    'gu': Result := 'Gujarati';
    'ht': Result := 'Haitian Creole';
    'ha': Result := 'Hausa';
    'he': Result := 'Hebrew';
    'hi': Result := 'Hindi';
    'ho': Result := 'Hiri Motu';
    'hu': Result := 'Hungarian';
    'is': Result := 'Icelandic';
    'io': Result := 'Ido';
    'ig': Result := 'Igbo';
    'id': Result := 'Indonesian';
    'ia': Result := 'Interlingua';
    'ie': Result := 'Interlingue';
    'iu': Result := 'Inuktitut';
    'ik': Result := 'Inupiaq';
    'ga': Result := 'Irish';
    'it': Result := 'Italian';
    'ja': Result := 'Japanese';
    'jv': Result := 'Javanese';
    'kn': Result := 'Kannada';
    'kr': Result := 'Kanuri';
    'ks': Result := 'Kashmiri';
    'kk': Result := 'Kazakh';
    'km': Result := 'Khmer';
    'ki': Result := 'Kikuyu';
    'rw': Result := 'Kinyarwanda';
    'rn': Result := 'Rundi';
    'kv': Result := 'Komi';
    'kg': Result := 'Kongo';
    'ko': Result := 'Korean';
    'kj': Result := 'Kuanyama';
    'ku': Result := 'Kurdish';
    'ky': Result := 'Kyrgyz';
    'lo': Result := 'Lao';
    'la': Result := 'Latin';
    'lv': Result := 'Latvian';
    'lt': Result := 'Lithuanian';
    'lb': Result := 'Luxembourgish';
    'mk': Result := 'Macedonian';
    'mg': Result := 'Malagasy';
    'ms': Result := 'Malay';
    'ml': Result := 'Malayalam';
    'mt': Result := 'Maltese';
    'mi': Result := 'Maori';
    'mr': Result := 'Marathi';
    'mn': Result := 'Mongolian';
    'na': Result := 'Nauru';
    'nv': Result := 'Navajo';
    'ng': Result := 'Ndonga';
    'ne': Result := 'Nepali';
    'no': Result := 'Norwegian';
    'nb': Result := 'Norwegian Bokmål';
    'nn': Result := 'Norwegian Nynorsk';
    'nd': Result := 'North Ndebele';
    'nr': Result := 'South Ndebele';
    'oc': Result := 'Occitan';
    'oj': Result := 'Ojibwe';
    'om': Result := 'Oromo';
    'or': Result := 'Odia';
    'os': Result := 'Ossetian';
    'pa': Result := 'Punjabi';
    'pi': Result := 'Pali';
    'pl': Result := 'Polish';
    'ps': Result := 'Pashto';
    'fa': Result := 'Persian';
    'pt': Result := 'Portuguese';
    'qu': Result := 'Quechua';
    'rm': Result := 'Romansh';
    'ro': Result := 'Romanian';
    'ru': Result := 'Russian';
    'se': Result := 'Northern Sami';
    'sm': Result := 'Samoan';
    'sg': Result := 'Sango';
    'sa': Result := 'Sanskrit';
    'sc': Result := 'Sardinian';
    'sd': Result := 'Sindhi';
    'si': Result := 'Sinhala';
    'sk': Result := 'Slovak';
    'sl': Result := 'Slovenian';
    'so': Result := 'Somali';
    'es': Result := 'Spanish';
    'su': Result := 'Sundanese';
    'sw': Result := 'Swahili';
    'ss': Result := 'Swati';
    'sv': Result := 'Swedish';
    'tl': Result := 'Tagalog';
    'ty': Result := 'Tahitian';
    'tg': Result := 'Tajik';
    'ta': Result := 'Tamil';
    'tt': Result := 'Tatar';
    'te': Result := 'Telugu';
    'th': Result := 'Thai';
    'bo': Result := 'Tibetan';
    'ti': Result := 'Tigrinya';
    'ts': Result := 'Tsonga';
    'tn': Result := 'Tswana';
    'tr': Result := 'Turkish';
    'tk': Result := 'Turkmen';
    'tw': Result := 'Twi';
    'ug': Result := 'Uyghur';
    'uk': Result := 'Ukrainian';
    'ur': Result := 'Urdu';
    'uz': Result := 'Uzbek';
    've': Result := 'Venda';
    'vi': Result := 'Vietnamese';
    'vo': Result := 'Volapük';
    'wa': Result := 'Walloon';
    'cy': Result := 'Welsh';
    'wo': Result := 'Wolof';
    'fy': Result := 'Western Frisian';
    'xh': Result := 'Xhosa';
    'yi': Result := 'Yiddish';
    'yo': Result := 'Yoruba';
    'za': Result := 'Zhuang';
    'zu': Result := 'Zulu';
    'auto' : Result := RS_AUTO;
  else
    Result := c;
  end;
end;


procedure ListLanguageFiles(list: TStrings; const Path: string);
var
  sr: TSearchRec;
begin
  list.Clear;
  // Sök efter alla .po i angiven katalog
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*.po',
               faAnyFile, sr) = 0 then
  begin
    repeat
      // Hoppa över undermappar
      if (sr.Attr and faDirectory) = 0 then
        // Extrahera språkkod utan ".po"
        list.Add(ChangeFileExt(sr.Name, ''));
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TfConf.lAckClick(Sender:TObject);
const
  txt = 'Trndi makes use of the following 3rd party libraries:'#10+
    'macOS native code libraries by Phil Hess'#10+
    'WinStyles library by Espectr0'#10#10+
    'Extensions use JavaScript engine QuickJS by Fabrice Bellard and Charlie Gordo'#10+

    'integration of said engine is made possible with mORMot2 by Synopse Informatique - Arnaud Bouchez'
    +#10#10+
    'Built in Object Pascal, using the Lazarus component library (LCL) and FreePascal.';
  begin
  ExtSucc(uxdAuto, 'Trndi', 'Libraries', txt, $00AA6004, $00FDD8AA);
end;

procedure TfConf.lbUsersEnter(Sender: TObject);
begin

end;

procedure TfConf.lbUsersSelectionChange(Sender: TObject; User: boolean);
var
  u: string;
begin
  btUserSave.Enabled := false;
  if lbUsers.ItemIndex < 0 then begin
    gbMulti.Enabled := false;
    Exit;
  end;

  u := lbUsers.Items[lbusers.ItemIndex] ;

  if u[1] = '-' then begin
    tNative.configUser := '';
    bRemove.Enabled := false;
  end
  else begin
    tNative.configUser := u;
    bRemove.Enabled := true;
  end;

  cbUser.ButtonColor :=  tNative.GetColorSetting('user.color', clBlack);
  edNick.Text := tNative.GetSetting('user.nick', '') ;
  lUserName.Caption := u;

  gbMulti.Enabled := true;
  btUserSave.Enabled := false; // Twice as the fields change during update
end;

procedure TfConf.cbSysChange(Sender:TObject);
begin
  gbOverride.Color := clDefault;
  if Pos('Dexcom', cbSys.Text) > 0 then
  begin
    gbOverride.Color := $00D3D2EE;
    ShowMessage(RS_DEX);
  end
  ;
  // Update parameter labels above edits based on backend
  // Defaults from base class
  Label15.Caption := TrndiAPI.ParamLabel(1);
  lPass.Caption   := TrndiAPI.ParamLabel(2);


    case cbSys.Text of
    'NightScout':
       begin
         Label15.Caption := NightScout.ParamLabel(1);
         lPass.Caption   := NightScout.ParamLabel(2);
       end;
    'NightScout v3':
       begin
         Label15.Caption := NightScout3.ParamLabel(1);
         lPass.Caption   := NightScout3.ParamLabel(2);
       end;
    'Dexcom (USA)':
       begin
         Label15.Caption := Dexcom.ParamLabel(1);
         lPass.Caption   := Dexcom.ParamLabel(2);
       end;
    'Dexcom (Outside USA)':
       begin
         Label15.Caption := Dexcom.ParamLabel(1);
         lPass.Caption   := Dexcom.ParamLabel(2);
       end;
    'xDrip':
       begin
         Label15.Caption := xDrip.ParamLabel(1);
         lPass.Caption   := xDrip.ParamLabel(2);
       end;
    {$ifdef DEBUG}
    '* Debug Backend *',
    '* Debug Missing Backend *',
    '* Debug Perfect Backend *',
    '* Debug Edge Backend *':
       begin
         Label15.Caption := '<DEBUG IGNORED>';
         lPass.Caption   := '<DEBUG IGNORED>';
       end;
      {$endif}
    end;
end;

procedure TfConf.cbUserClick(Sender:TObject);
begin

end;

procedure TfConf.cbUserColorChanged(Sender: TObject);
begin
  btUserSave.Enabled := true;
end;

procedure TfConf.dotClick(Sender: TObject);
var
  bg: tcolor;
begin
  eDot.SetFocus;
end;

procedure TfConf.edNickChange(Sender: TObject);
begin
    btUserSave.Enabled := true;
end;

procedure TfConf.eDotChange(Sender: TObject);
var
  i: integer;
  lbl: TLabel;
begin
  if sender = eDot then
    lbl := lDot
  else
    lbl := lDotNow;

  if tryStrToInt('$' + (sender as TEdit).text, i) then begin
    lbl.Caption := WideChar(i);
    if sender = eDot then begin
      lDot1.Caption :=  lbl.Caption;
      lDot2.Caption :=  lbl.Caption;
      lDot3.Caption :=  lbl.Caption;
    end else
      lDotCurr.Caption := lbl.Caption;
  end
  else
    lbl.Caption := '- ERROR -';
end;

procedure TfConf.ePassEnter(Sender: TObject);
begin
  ePass.PasswordChar := #0;
end;

procedure TfConf.ePassExit(Sender: TObject);
begin
  ePass.PasswordChar := '*';
end;

procedure TfConf.bLimitsClick(Sender:TObject);
begin
end;

procedure TfConf.bAddClick(Sender:TObject);
var
 s, x: string;
 c: char;
 mr: TModalResult;
begin
  s := ExtInput(uxdAuto, RS_ENTER_USER, RS_ENTER_USER, RS_ENTER_NAME,'',mr);
  if mr = mrOK then begin
    if Trim(s) = '' then begin
      ShowMessage(RS_ENTER_ANY);
      Exit;
    end;
    for c in s do begin
      if not (c in ['0'..'9', 'A'..'z',' ']) then begin
        ShowMessage(RS_ENTER_NAME);
        Exit;
      end;
    end;

    // No duplicates!
    for x in lbusers.Items do
      if s = x then begin
      ShowMessage(RS_DUPE_NAME);
        Exit;
      end;
    lbUsers.AddItem(s, nil);
    lbUsers.Enabled := true;
  end;
end;

procedure TfConf.bOverrideHelpClick(Sender:TObject);
begin
  ShowMessage(RS_OVERRIDE_HELP);
end;

procedure TfConf.bPrivacyHelpClick(Sender:TObject);
begin
  ShowMessage(RS_PRIVACY_HELP);
end;

procedure TfConf.bRemoveClick(Sender:TObject);
begin
if lbUsers.ItemIndex > -1 then
  lbUsers.DeleteSelected;
if lbUsers.Items.Count <= 1 then begin
  lbUsers.Enabled := false;
  gbMulti.Enabled := false;
end;

  ShowMessage(RS_REMOVE_ACC);
  bRemove.Enabled := false; // No item selexted now
end;

procedure TfConf.bBackendHelpClick(Sender: TObject);
var
  s: string;
begin
  s := '';
  if SameText(cbSys.Text, 'NightScout') then
    s := RS_HELP_NS_V2
  else if SameText(cbSys.Text, 'NightScout v3') then
    s := RS_HELP_NS_V3
  else if Pos('Dexcom', cbSys.Text) > 0 then
    s := RS_HELP_DEX_REGION
  else if SameText(cbSys.Text, 'xDrip') then
    s := RS_HELP_XDRIP
  else if cbSys.Text[1] = '*' then
    s := 'This is a debug backend, it''s used to test Trndi. It should not be used by non-developers!'
  else
    s := RS_CHOOSE_SYSTEM;

  if s <> '' then
    ShowMessage(s);
end;

procedure TfConf.bSysNoticeClick(Sender: TObject);
var
 ns, url: string;
begin
  ns := tnative.getNotificationSystem;
  url := '';

  case ns of
    'BurntToast': url := 'https://www.powershellgallery.com/packages/BurntToast';
    'notify-send': url := 'https://www.google.com/search?q=notify-send';
  end;
  // Natives:
  // NSUserNotification
  // gdbus

  if url <> '' then begin
    if ExtMsg(uxdAuto, RS_NOTIFICATIONS, RS_NOTIFY_TITLE, Format(RS_NOTIFY_TXT, [ns]), '', uxclWhite, uxclRed, [mbOK, mbUXRead], uxmtCustom,0) <> mrOK then
       OpenURL(url)
  end else
     UXMessage(uxdAuto, RS_NOTIFICATIONS, RS_NOTIFY_SYSTEM);

end;

procedure TfConf.bSysTouchClick(Sender: TObject);
begin
    SHowMessage(RS_HASTOUCH);
end;

procedure TfConf.bTestAnnounceClick(Sender: TObject);
begin
  tnative.attention('Trndi Test', 'test');
end;

procedure TfConf.bTestSpeechClick(Sender: TObject);
begin
  tnative.speak('test 5.5');
end;

procedure TfConf.btResetClick(Sender: TObject);
const
  bg_color_ok: TColor = $0000DC84;
  bg_color_ok_txt: TColor = $00F2FFF2;
  // Hi
  bg_color_hi: TColor = $0007DAFF;
  bg_color_hi_txt: TColor = $000052FB;
  // Low
  bg_color_lo: TColor = $00FFBE0B;
  bg_color_lo_txt: TColor = $00FFFEE9;

  // Personal hi
  bg_rel_color_lo: TColor = $00A859EE;
  bg_rel_color_lo_txt: TColor = $002D074E;
  // Personal low
  bg_rel_color_hi: TColor = $0072C9DE;
  bg_rel_color_hi_txt: TColor = $001C6577;
begin
  cl_ok_bg.ButtonColor := bg_color_ok;
  cl_hi_bg.ButtonColor := bg_color_hi;
  cl_lo_bg.ButtonColor := bg_color_lo;

  cl_ok_txt.ButtonColor := bg_color_ok_txt;
  cl_hi_txt.ButtonColor := bg_color_hi_txt;
  cl_lo_txt.ButtonColor := bg_color_lo_txt;

  cl_hi_bg_cust.ButtonColor := bg_rel_color_hi;
  cl_lo_bg_cust.ButtonColor := bg_rel_color_lo;

  cl_hi_txt_cust.ButtonColor := bg_rel_color_hi_txt;
  cl_lo_txt_cust.ButtonColor := bg_rel_color_lo_txt;
end;

procedure TfConf.btUserSaveClick(Sender: TObject);
begin
  if lUserName.Caption[1] = '-' then
    tNative.configUser := ''
  else
    tNative.configUser := lUserName.Caption;

  tNative.SetColorSetting('user.color', cbUser.ButtonColor);
  tNative.SetSetting('user.nick', edNick.Text);

    btUserSave.Enabled := false;
end;

procedure TfConf.Button1Click(Sender:TObject);
begin
  Openurl('https://github.com/slicke/trndi/blob/main/LANGUAGES.md');
end;

{------------------------------------------------------------------------------
  Apply the display panel's font to preview labels.

  This lets the user quickly see how the selected UI font looks on key
  readouts (value, arrow, and "ago") without changing global styles.
------------------------------------------------------------------------------}
procedure TfConf.Button2Click(Sender:TObject);
begin
  // Mirror the panel font to individual preview labels
  lVal.Font.Name := pnDisplay.Font.Name;   // main value readout
  lArrow.Font.Name := pnDisplay.Font.Name; // trend arrow glyph/text
  lAgo.Font.Name := pnDisplay.Font.Name;   // time since last reading
end;

{------------------------------------------------------------------------------
  Check for the latest Trndi release (final, all platforms) and offer to open
  the downloads page. Pre-releases and platform-specific filtering are no
  longer used.

  Flow:
  - Fetch https://api.github.com/repos/slicke/trndi/releases/latest
  - Compare with current version via HasNewerRelease(..., pre=false)
  - Resolve a suitable asset URL via GetNewerVersionURL(res)
  - Prompt the user to open the browser
------------------------------------------------------------------------------}
procedure TfConf.Button3Click(Sender:TObject);
var
  res,   // raw JSON from GitHub API
  r,     // download URL
  rn,    // latest version name/tag
  s: string;
  rok: boolean;
begin
  TrndiNative.getURL('https://api.github.com/repos/slicke/trndi/releases/latest', res);
  rok := HasNewerRelease(res, rn, false);

  if rok then
  begin
    r := GetNewerVersionURL(res);
    s := Format(RS_NEWVER, [rn]);
    if UXDialog(uxdAuto, RS_NEWVER_CAPTION, s, [mbYes, mbNo], mtInformation) = mrYes then
      OpenURL(r);
  end
  else
    ShowMessage(RS_UPTODATE);
end;

procedure TfConf.Button4Click(Sender: TObject);
begin
  eDot.text := '2B24';
  eDotNow.Text := '2600';
end;

procedure TfConf.cbCustChange(Sender:TObject);
begin
  fsHi.Enabled :=  cbCust.Checked;
  fsLo.Enabled :=  cbCust.Checked;
  if (cbCust.Checked) and (cbSys.Text = 'NightScout') then
     ShowMessage(RS_OVERRIDE_NS);
end;

procedure TfConf.FormCreate(Sender:TObject);
begin
  // Base app version + build date + widgetset + target CPU
  lVersion.Caption := GetProductVersionMajorMinor('2.x');
  // If CI embedded a real build number, append it
  if CI and (BUILD_NUMBER <> 'dev') then
    lVersion.Caption := lVersion.Caption + '.' + BUILD_NUMBER
  else
    lVersion.Caption := Format('%s-dev (%s)', [lVersion.Caption, StringReplace({$I %DATE%}, '/', '-', [rfReplaceAll])]);
  lversion.left := lversion.left - 20;

  pcMain.ActivePage := tsGeneral;
  {$ifdef darwin}
    self.width := self.width + (self.width div 9);
  {$endif}
  {$ifdef lclqt6}
    self.font.size := 10;
  {$endif}
  {$ifdef lclgtk2}
    self.font.size := 10;
  {$endif}
  tnative := TrndiNative.create;
  tnative.noFree := true;
  if tnative.isDarkMode then
    tnative.setDarkMode{$ifdef windows}(self.Handle){$endif};

  {$ifdef darwin}
  edTray.Enabled := false; // No support
  {$endif}

  // Initialize parameter labels for current backend selection
  cbSysChange(Self);
end;

procedure TfConf.FormDestroy(Sender: TObject);
begin
  tnative.Free;
end;

procedure TfConf.FormResize(Sender: TObject);
begin
  eAddr.width := cbSys.Width;
  ePass.width := cbSys.Width;
  bvExt.Width := cbSys.Width;
  eExt.Width := cbSys.Width;
end;

procedure TfConf.Label12Click(Sender: TObject);
begin

end;

procedure TfConf.lLicenseClick(Sender:TObject);
const
  txt = 'Trndi - CGM viewer'#10+
    'A re-imagination of TrayTrend by Björn Lindh'#10#10+
    'Copyright (C) 2017-2025 Björn Lindh'#10#10+

    'This program is free software: you can redistribute it and/or modify it'#10+
    'under the terms of the GNU General Public License version 3 as published'#10+
    'by the Free Software Foundation.'#10#10+

    'For more information, refer to the accompanying license file or visit:'#10+
    'https://www.gnu.org/licenses/gpl-3.0'#10#10+
    'Trndi is hobby project, verify all data with an officially approved'#10+
    'medical app before acting on any shown data!'#10#10 +

    'This app is NOT a medical device and is NOT intended for:'#10+
'- Medical diagnosis, treatment, or prevention'#10+
'- Making medical decisions'#10+
'- Replacing your CGM app or medical devices'#10+
'- Emergency medical situations'#10#10+
'### IMPORTANT WARNINGS ###'#10+
'• Data displayed may be inaccurate, delayed, or unavailable'#10+
'• Always verify readings with your official CGM device'#10+
'• Never make medical decisions based solely on this app'#10+
'• Consult healthcare professionals for medical advice'#10#10+
'### BY USING THIS APP, YOU ACKNOWLEDGE THAT ###'#10+
'- The developers assume NO LIABILITY for any harm, injury, or damages'#10+
'- You use this app entirely at your own risk'#10+
'- This app may contain bugs or errors that could display incorrect data'#10#10+
'- IF YOU DO NOT AGREE WITH THESE TERMS, DO NOT USE THIS APP. -';
begin
 if ExtSuccEx(uxdAuto, 'Trndi', 'License', txt, [mbOK, mbUxRead], $00AA6004, $00FDD8AA,uxmtOK, 8) <> mrOK then
    OpenURL('https://github.com/slicke/trndi/blob/main/LICENSE.md');
end;

procedure TfConf.lValClick(Sender:TObject);
begin
fdFont.PreviewText := (sender as TLabel).Caption;
fdFont.Title := RS_SELECT_FONT;
if fdFont.Execute then
with (sender as TLabel).Font do begin
    name := fdFont.Font.Name;
    style := fdFont.Font.style;
end;
end;

procedure TfConf.pcMainChange(Sender: TObject);
begin

end;

procedure TfConf.rbUnitClick(Sender:TObject);
begin
  if (sender is TForm) or (rbUnit.ItemIndex = 0) then
  begin
    fsHi.DecimalPlaces := 1;
    fsHi.Value := round(fsHi.Value * TrndiAPI.toMMOL * 10) / 10; // Do the / 10 thing to keep the decimal
    fsLo.Value := round(fsLo.Value * TrndiAPI.toMMOL * 10) / 10;
  end
  else
  begin
    fsHi.Value := round(fsHi.Value * TrndiAPI.toMgdl);
    fsLo.Value := round(fsLo.Value * TrndiAPI.toMgdl);
    fsHi.DecimalPlaces := 0;
  end;

  fsLo.DecimalPlaces := fsHi.DecimalPlaces;
end;

procedure TfConf.spTHRESHOLDChange(Sender: TObject);
begin
  if spTHRESHOLD.Value > 30 then begin
    spTHRESHOLD.Value := 30;
    ShowMessage(RS_Saftey_Hi);
  end
  else if spTHRESHOLD.Value < 6 then begin
    spTHRESHOLD.Value := 6;
    ShowMessage(RS_Saftey_Low);
  end;
end;

procedure TfConf.tbAdvancedChange(Sender:TObject);
begin

end;

procedure TfConf.ToggleBox1Change(Sender:TObject);
begin

end;

procedure TfConf.tsDisplayShow(Sender: TObject);
var
  l: tlabel;
begin
  // The label caption looks ugly and invalidate doesnt help. Neiter does redraw at this stage
  for l in [lDot1, lDot2, lDot3, lDotCurr] do
    l.caption := '';
//  eDot.OnChange(eDot);
//  eDotNow.OnChange(eDotNow);
end;

procedure TfConf.tsSystemShow(Sender: TObject);
begin
   lWaitSys.Show;
   tsSystem.Update;
   Application.ProcessMessages;

   cbNotice.Checked := TrndiNative.isNotificationSystemAvailable;
   lWaitSys.visible := false;
   lProblematic.Visible := false;
   if IsProblematicWM then begin
     if not IsSemiProblematicWM then
       lProblematic.Visible := true;
   end;
end;

end.
