function GetLanguageName(const ACode: string): string;
var
  C: string;
begin
  C := LowerCase(Trim(ACode));

  case C of
  'aa':
    Result := 'Afar';
  'ab':
    Result := 'Abkhazian';
  'ae':
    Result := 'Avestan';
  'af':
    Result := 'Afrikaans';
  'ak':
    Result := 'Akan';
  'sq':
    Result := 'Albanian';
  'am':
    Result := 'Amharic';
  'ar':
    Result := 'Arabic';
  'an':
    Result := 'Aragonese';
  'hy':
    Result := 'Armenian';
  'as':
    Result := 'Assamese';
  'av':
    Result := 'Avaric';
  'ay':
    Result := 'Aymara';
  'az':
    Result := 'Azerbaijani';
  'bm':
    Result := 'Bambara';
  'ba':
    Result := 'Bashkir';
  'eu':
    Result := 'Basque';
  'be':
    Result := 'Belarusian';
  'bn':
    Result := 'Bengali';
  'bh':
    Result := 'Bihari';
  'bi':
    Result := 'Bislama';
  'bs':
    Result := 'Bosnian';
  'br':
    Result := 'Breton';
  'bg':
    Result := 'Bulgarian';
  'my':
    Result := 'Burmese';
  'ca':
    Result := 'Catalan';
  'ch':
    Result := 'Chamorro';
  'ce':
    Result := 'Chechen';
  'ny':
    Result := 'Chichewa';        // also Nyanja
  'zh':
    Result := 'Chinese';
  'cu':
    Result := 'Church Slavonic';
  'cv':
    Result := 'Chuvash';
  'kw':
    Result := 'Cornish';
  'co':
    Result := 'Corsican';
  'cr':
    Result := 'Cree';
  'hr':
    Result := 'Croatian';
  'cs':
    Result := 'Czech';
  'da':
    Result := 'Danish';
  'dv':
    Result := 'Divehi';
  'nl':
    Result := 'Dutch';
  'dz':
    Result := 'Dzongkha';
  'en':
    Result := 'English';
  'eo':
    Result := 'Esperanto';
  'et':
    Result := 'Estonian';
  'ee':
    Result := 'Ewe';
  'fo':
    Result := 'Faroese';
  'fj':
    Result := 'Fijian';
  'fi':
    Result := 'Finnish';
  'fr':
    Result := 'French';
  'ff':
    Result := 'Fulah';
  'gl':
    Result := 'Galician';
  'ka':
    Result := 'Georgian';
  'de':
    Result := 'German';
  'el':
    Result := 'Greek';
  'gu':
    Result := 'Gujarati';
  'ht':
    Result := 'Haitian Creole';
  'ha':
    Result := 'Hausa';
  'he':
    Result := 'Hebrew';
  'hi':
    Result := 'Hindi';
  'ho':
    Result := 'Hiri Motu';
  'hu':
    Result := 'Hungarian';
  'is':
    Result := 'Icelandic';
  'io':
    Result := 'Ido';
  'ig':
    Result := 'Igbo';
  'id':
    Result := 'Indonesian';
  'ia':
    Result := 'Interlingua';
  'ie':
    Result := 'Interlingue';
  'iu':
    Result := 'Inuktitut';
  'ik':
    Result := 'Inupiaq';
  'ga':
    Result := 'Irish';
  'it':
    Result := 'Italian';
  'ja':
    Result := 'Japanese';
  'jv':
    Result := 'Javanese';
  'kn':
    Result := 'Kannada';
  'kr':
    Result := 'Kanuri';
  'ks':
    Result := 'Kashmiri';
  'kk':
    Result := 'Kazakh';
  'km':
    Result := 'Khmer';
  'ki':
    Result := 'Kikuyu';
  'rw':
    Result := 'Kinyarwanda';
  'rn':
    Result := 'Rundi';
  'kv':
    Result := 'Komi';
  'kg':
    Result := 'Kongo';
  'ko':
    Result := 'Korean';
  'kj':
    Result := 'Kuanyama';
  'ku':
    Result := 'Kurdish';
  'ky':
    Result := 'Kyrgyz';
  'lo':
    Result := 'Lao';
  'la':
    Result := 'Latin';
  'lv':
    Result := 'Latvian';
  'lt':
    Result := 'Lithuanian';
  'lb':
    Result := 'Luxembourgish';
  'mk':
    Result := 'Macedonian';
  'mg':
    Result := 'Malagasy';
  'ms':
    Result := 'Malay';
  'ml':
    Result := 'Malayalam';
  'mt':
    Result := 'Maltese';
  'mi':
    Result := 'Maori';
  'mr':
    Result := 'Marathi';
  'mn':
    Result := 'Mongolian';
  'na':
    Result := 'Nauru';
  'nv':
    Result := 'Navajo';
  'ng':
    Result := 'Ndonga';
  'ne':
    Result := 'Nepali';
  'no':
    Result := 'Norwegian';
  'nb':
    Result := 'Norwegian Bokmål';
  'nn':
    Result := 'Norwegian Nynorsk';
  'nd':
    Result := 'North Ndebele';
  'nr':
    Result := 'South Ndebele';
  'oc':
    Result := 'Occitan';
  'oj':
    Result := 'Ojibwe';
  'om':
    Result := 'Oromo';
  'or':
    Result := 'Odia';
  'os':
    Result := 'Ossetian';
  'pa':
    Result := 'Punjabi';
  'pi':
    Result := 'Pali';
  'pl':
    Result := 'Polish';
  'ps':
    Result := 'Pashto';
  'fa':
    Result := 'Persian';
  'pt':
    Result := 'Portuguese';
  'qu':
    Result := 'Quechua';
  'rm':
    Result := 'Romansh';
  'ro':
    Result := 'Romanian';
  'ru':
    Result := 'Russian';
  'se':
    Result := 'Northern Sami';
  'sm':
    Result := 'Samoan';
  'sg':
    Result := 'Sango';
  'sa':
    Result := 'Sanskrit';
  'sc':
    Result := 'Sardinian';
  'sd':
    Result := 'Sindhi';
  'si':
    Result := 'Sinhala';
  'sk':
    Result := 'Slovak';
  'sl':
    Result := 'Slovenian';
  'so':
    Result := 'Somali';
  'es':
    Result := 'Spanish';
  'su':
    Result := 'Sundanese';
  'sw':
    Result := 'Swahili';
  'ss':
    Result := 'Swati';
  'sv':
    Result := 'Swedish';
  'jm':
    Result := 'Jämtländska (Jamska)';
  'tl':
    Result := 'Tagalog';
  'ty':
    Result := 'Tahitian';
  'tg':
    Result := 'Tajik';
  'ta':
    Result := 'Tamil';
  'tt':
    Result := 'Tatar';
  'te':
    Result := 'Telugu';
  'th':
    Result := 'Thai';
  'bo':
    Result := 'Tibetan';
  'ti':
    Result := 'Tigrinya';
  'ts':
    Result := 'Tsonga';
  'tn':
    Result := 'Tswana';
  'tr':
    Result := 'Turkish';
  'tk':
    Result := 'Turkmen';
  'tw':
    Result := 'Twi';
  'ug':
    Result := 'Uyghur';
  'uk':
    Result := 'Ukrainian';
  'ur':
    Result := 'Urdu';
  'uz':
    Result := 'Uzbek';
  've':
    Result := 'Venda';
  'vi':
    Result := 'Vietnamese';
  'vo':
    Result := 'Volapük';
  'wa':
    Result := 'Walloon';
  'cy':
    Result := 'Welsh';
  'wo':
    Result := 'Wolof';
  'fy':
    Result := 'Western Frisian';
  'xh':
    Result := 'Xhosa';
  'yi':
    Result := 'Yiddish';
  'yo':
    Result := 'Yoruba';
  'za':
    Result := 'Zhuang';
  'zu':
    Result := 'Zulu';
  'auto':
    Result := RS_AUTO;
  else
    Result := c;
  end;
end;
