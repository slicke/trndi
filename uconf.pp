
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
VersionInfo;

type

  { TfConf }

TfConf = class(TForm)
  bAdd: TButton;
  Button1:TButton;
  Button2:TButton;
  bvExt:TBevel;
  bOverrideHelp:TButton;
  bPrivacyHelp: TButton;
  bRemove: TButton;
  cbCust:TCheckBox;
  cbLang:TComboBox;
  cbPrivacy: TCheckBox;
  cbSize:TCheckBox;
  cbSys:TComboBox;
  cbPos:TComboBox;
  cbUser:TColorButton;
  cbTouch: TCheckBox;
  cbNotice: TCheckBox;
  cbMultiTouch: TCheckBox;
  cbTIR:TCheckBox;
  eAddr:TLabeledEdit;
  edNick:TEdit;
  eExt:TLabeledEdit;
  ePass:TLabeledEdit;
  fdFont:TFontDialog;
  fsHi: TFloatSpinEdit;
  fsLo: TFloatSpinEdit;
  gbMulti:TGroupBox;
  gbOverride:TGroupBox;
  gbOverride1:TGroupBox;
  GroupBox1: TGroupBox;
  Image1: TImage;
  Label1: TLabel;
  Label2:TLabel;
  Label3:TLabel;
  Label6:TLabel;
  Label7:TLabel;
  Label8:TLabel;
  lAck: TButton;
  lAgo:TLabel;
  lArrow:TLabel;
  lCopyright: TLabel;
  lCurrentAcc:TLabel;
  Label4:TLabel;
  Label5:TLabel;
  lbUsers:TListBox;
  lDot1:TLabel;
  lDot2:TLabel;
  lDot3:TLabel;
  lHiOver: TLabel;
  lLicense: TButton;
  lLounder: TLabel;
  lTitle: TLabel;
  lVal:TLabel;
  lVersion:TLabel;
  Panel1: TPanel;
  Panel2: TPanel;
  Panel3: TPanel;
  Panel4: TPanel;
  Panel5: TPanel;
  Panel6:TPanel;
  pnDisplay:TPanel;
  pcMain:TPageControl;
  rbUnit:TRadioGroup;
  tsDisplay:TTabSheet;
  tsMulti:TTabSheet;
  tsGeneral:TTabSheet;
  tsCustom:TTabSheet;
  procedure bAddClick(Sender:TObject);
  procedure bLimitsClick(Sender:TObject);
  procedure bOverrideHelpClick(Sender:TObject);
  procedure bPrivacyHelpClick(Sender:TObject);
  procedure bRemoveClick(Sender:TObject);
  procedure Button1Click(Sender:TObject);
  procedure Button2Click(Sender:TObject);
  procedure cbCustChange(Sender:TObject);
  procedure cbSysChange(Sender:TObject);
  procedure cbUserClick(Sender:TObject);
  procedure FormCreate(Sender:TObject);
  procedure FormResize(Sender: TObject);
  procedure lAckClick(Sender:TObject);
  procedure lLicenseClick(Sender:TObject);
  procedure lValClick(Sender:TObject);
  procedure rbUnitClick(Sender:TObject);
  procedure tbAdvancedChange(Sender:TObject);
  procedure ToggleBox1Change(Sender:TObject);
private

public

end;

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
var 
fConf: TfConf;

procedure ListLanguageFiles(list: TStrings; const Path: string);
function GetLanguageName(const ACode: string): string;
function ExtractLangCode(const AText: string): string;

implementation

{$R *.lfm}

procedure Showmessage(const str: string);
begin
  UXMessage(sSuccTitle, str, widechar($2139));
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
  ExtSucc('Trndi', 'Libraries', txt);
end;

procedure TfConf.cbSysChange(Sender:TObject);
begin
  gbOverride.Color := clDefault;
  if Pos('Dexcom', cbSys.Text) > 0 then
  begin
    gbOverride.Color := $00D3D2EE;
    ShowMessage(RS_DEX);
  end
end;

procedure TfConf.cbUserClick(Sender:TObject);
begin

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
  s := ExtInput(RS_ENTER_USER, RS_ENTER_USER, RS_ENTER_NAME,'',mr);
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
end;

procedure TfConf.Button1Click(Sender:TObject);
begin
  Openurl('https://github.com/slicke/trndi/blob/main/LANGUAGES.md');
end;

procedure TfConf.Button2Click(Sender:TObject);
begin
lVal.font.name := pnDisplay.font.name;
lArrow.font.name := pnDisplay.font.name;
lAgo.font.name := pnDisplay.font.name;
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
  lVersion.Caption := GetProductVersion(lVersion.Caption) + ' | Built '+StringReplace({$I %DATE%}, '/', '-', [rfReplaceAll]) + ' | ' +
  {$if defined(LCLQt6)}
    'QT6'
  {$elseif defined(LCLGTK2)}
    'GTK2'
  {$elseif defined(LCLGTK3)}
    'GTK3'
  {$elseif defined(LCLWIN32)}
    'Windows'
  {$elseif defined(LCLCocoa)}
    'macOS'
  {$else}
    'custom'
  {$endif}
  + ' | ' + {$I %FPCTARGETCPU%};
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
  with TrndiNative.create do begin
      if isDarkMode then
       setDarkMode{$ifdef windows}(self.Handle){$endif};
      Free;
  end;
end;

procedure TfConf.FormResize(Sender: TObject);
begin
  eAddr.width := cbSys.Width;
  ePass.width := cbSys.Width;
  bvExt.Width := cbSys.Width;
  eExt.Width := cbSys.Width;
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
    'medical app before acting on any shown data!';
begin
  ExtSucc('Trndi', 'License', txt);
end;

procedure TfConf.lValClick(Sender:TObject);
begin
if fdFont.Execute then
with (sender as TLabel).Font do begin
    name := fdFont.Font.Name;
    style := fdFont.Font.style;
end;
end;

procedure TfConf.rbUnitClick(Sender:TObject);
begin
  if (sender is TForm) or (rbUnit.ItemIndex = 0) then
  begin
    fsHi.DecimalPlaces := 1;
    fsHi.Value := round(fsHi.Value * 0.0555 * 10) / 10; // Do the / 10 thing to keep the decimal
    fsLo.Value := round(fsLo.Value * 0.0555 * 10) / 10;
  end
  else
  begin
    fsHi.Value := round(fsHi.Value * 18.0182);
    fsLo.Value := round(fsLo.Value * 18.0182);
    fsHi.DecimalPlaces := 0;
  end;

  fsLo.DecimalPlaces := fsHi.DecimalPlaces;
end;

procedure TfConf.tbAdvancedChange(Sender:TObject);
begin

end;

procedure TfConf.ToggleBox1Change(Sender:TObject);
begin

end;

end.
