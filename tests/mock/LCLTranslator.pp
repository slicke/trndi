unit LCLTranslator;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

{ Minimal mock of LCLTranslator used by tests.
  Provides SetDefaultLang used by UI initialization in `umain`.
}

function SetDefaultLang(const lang, langPath: string; const domain: string = ''): string;

implementation

function SetDefaultLang(const lang, langPath: string; const domain: string = ''): string;
begin
  // In tests we simply return the requested language to indicate success.
  Result := lang;
end;

end.
