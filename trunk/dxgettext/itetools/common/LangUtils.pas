unit LangUtils;

interface
uses
  Windows, Sysutils;

type
  TISO639Languages = class(TLanguages)
  private
    function GetISOCountry(Index: integer): string;
    function GetISOLang(Index: integer): string;
  public
    property ISOName[Index:integer]:string read GetISOLang;
    property ISOCountry[Index:integer]:string read GetISOCountry;
  end;

implementation

{ TISO639Languages }

function TISO639Languages.GetISOCountry(Index: integer): string;
begin
  SetLength(Result,4);
  SetLength(Result,GetLocaleInfo(LocaleID[Index],LOCALE_SISO3166CTRYNAME,PChar(Result),Length(Result)));
end;

function TISO639Languages.GetISOLang(Index: integer): string;
begin
  SetLength(Result,4);
  SetLength(Result,GetLocaleInfo(LocaleID[Index],LOCALE_SISO639LANGNAME,PChar(Result),Length(Result)));
end;

end.
