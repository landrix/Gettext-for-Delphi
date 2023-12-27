unit u_Languages;

interface

uses
  SysUtils,
  Classes,
  StrUtils;

type
  TdxLanguage = record
  private
    FCode: string;
    procedure AssertValid;
  public
    class function Create(const _Code: string): TdxLanguage; static;
    procedure InitFromCode(const _Code: string);
    function TryInitFromCode(const _Code: string): boolean;
    procedure InitFromName(const _EnglishName: string);
    function TryInitFromName(const _EnglishName: string): boolean;
    function IsValid: boolean;
    function Code: string;
    function EnglishName: string;
    function LocalizedName: string;
  end;

type
  TdxLanguages = class
  private
    FLanguageCodes: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetCodesAndLanguages(_sl: TStrings);
    procedure GetLanguageCodes(_sl: TStrings);
    procedure GetLanguages(_sl: TStrings);
    procedure GetLoacalizedLanguages(_sl: TStrings);
    function GetCodeForLanguage(const _Lng: string): string;
    function GetLanguageForCode(const _Code: string): string;
    function TryGetCodeForLanguage(const _Lng: string; out _Code: string): boolean;
    function TryGetLanguageForCode(const _Code: string; out _Lng: string): boolean;
  end;

function DxLanguages: TdxLanguages;

implementation

uses
  gnugettext;

var
  gblLanguages: TdxLanguages = nil;

function dxLanguages: TdxLanguages;
begin
  if not Assigned(gblLanguages) then
    gblLanguages := TdxLanguages.Create;
  Result := gblLanguages;
end;

{ TdxLanguages }

constructor TdxLanguages.Create;
begin
  inherited Create;
  FLanguageCodes := TStringList.Create;
  FLanguageCodes.BeginUpdate;
  try
    FLanguageCodes.Add('aa=Afar');
    FLanguageCodes.Add('ab=Abkhazian');
    FLanguageCodes.Add('ae=Avestan');
    FLanguageCodes.Add('af=Afrikaans');
    FLanguageCodes.Add('ak=Akan');
    FLanguageCodes.Add('am=Amharic');
    FLanguageCodes.Add('an=Aragonese');
    FLanguageCodes.Add('ar=Arabic');
    FLanguageCodes.Add('as=Assamese');
    FLanguageCodes.Add('av=Avaric');
    FLanguageCodes.Add('ay=Aymara');
    FLanguageCodes.Add('az=Azerbaijani');
    FLanguageCodes.Add('ba=Bashkir');
    FLanguageCodes.Add('be=Belarusian');
    FLanguageCodes.Add('bg=Bulgarian');
    FLanguageCodes.Add('bh=Bihari');
    FLanguageCodes.Add('bi=Bislama');
    FLanguageCodes.Add('bm=Bambara');
    FLanguageCodes.Add('bn=Bengali');
    FLanguageCodes.Add('bo=Tibetan');
    FLanguageCodes.Add('br=Breton');
    FLanguageCodes.Add('bs=Bosnian');
    FLanguageCodes.Add('ca=Catalan');
    FLanguageCodes.Add('ce=Chechen');
    FLanguageCodes.Add('ch=Chamorro');
    FLanguageCodes.Add('co=Corsican');
    FLanguageCodes.Add('cr=Cree');
    FLanguageCodes.Add('cs=Czech');
    FLanguageCodes.Add('cv=Chuvash');
    FLanguageCodes.Add('cy=Welsh');
    FLanguageCodes.Add('da=Danish');
    FLanguageCodes.Add('de=German');
    FLanguageCodes.Add('de_AT=Austrian German');
    FLanguageCodes.Add('de_CH=Swiss German');
    FLanguageCodes.Add('dv=Divehi');
    FLanguageCodes.Add('dz=Dzongkha');
    FLanguageCodes.Add('ee=Ewe');
    FLanguageCodes.Add('el=Greek');
    FLanguageCodes.Add('en=English');
    FLanguageCodes.Add('en_AU=Australian English');
    FLanguageCodes.Add('en_CA=Canadian English');
    FLanguageCodes.Add('en_GB=British English');
    FLanguageCodes.Add('en_US=American English');
    FLanguageCodes.Add('eo=Esperanto');
    FLanguageCodes.Add('es=Spanish');
    FLanguageCodes.Add('et=Estonian');
    FLanguageCodes.Add('eu=Basque');
    FLanguageCodes.Add('fa=Persian');
    FLanguageCodes.Add('ff=Fulah');
    FLanguageCodes.Add('fi=Finnish');
    FLanguageCodes.Add('fj=Fijian');
    FLanguageCodes.Add('fo=Faroese');
    FLanguageCodes.Add('fr=French');
    FLanguageCodes.Add('fr_BE=Walloon');
    FLanguageCodes.Add('fy=Frisian');
    FLanguageCodes.Add('ga=Irish');
    FLanguageCodes.Add('gd=Gaelic');
    FLanguageCodes.Add('gl=Gallegan');
    FLanguageCodes.Add('gn=Guarani');
    FLanguageCodes.Add('gu=Gujarati');
    FLanguageCodes.Add('gv=Manx');
    FLanguageCodes.Add('ha=Hausa');
    FLanguageCodes.Add('he=Hebrew');
    FLanguageCodes.Add('hi=Hindi');
    FLanguageCodes.Add('ho=Hiri Motu');
    FLanguageCodes.Add('hr=Croatian');
    FLanguageCodes.Add('ht=Haitian');
    FLanguageCodes.Add('hu=Hungarian');
    FLanguageCodes.Add('hy=Armenian');
    FLanguageCodes.Add('hz=Herero');
    FLanguageCodes.Add('ia=Interlingua');
    FLanguageCodes.Add('id=Indonesian');
    FLanguageCodes.Add('ie=Interlingue');
    FLanguageCodes.Add('ig=Igbo');
    FLanguageCodes.Add('ii=Sichuan Yi');
    FLanguageCodes.Add('ik=Inupiaq');
    FLanguageCodes.Add('io=Ido');
    FLanguageCodes.Add('is=Icelandic');
    FLanguageCodes.Add('it=Italian');
    FLanguageCodes.Add('iu=Inuktitut');
    FLanguageCodes.Add('ja=Japanese');
    FLanguageCodes.Add('jv=Javanese');
    FLanguageCodes.Add('ka=Georgian');
    FLanguageCodes.Add('kg=Kongo');
    FLanguageCodes.Add('ki=Kikuyu');
    FLanguageCodes.Add('kj=Kuanyama');
    FLanguageCodes.Add('kk=Kazakh');
    FLanguageCodes.Add('kl=Greenlandic');
    FLanguageCodes.Add('km=Khmer');
    FLanguageCodes.Add('kn=Kannada');
    FLanguageCodes.Add('ko=Korean');
    FLanguageCodes.Add('kr=Kanuri');
    FLanguageCodes.Add('ks=Kashmiri');
    FLanguageCodes.Add('ku=Kurdish');
    FLanguageCodes.Add('kw=Cornish');
    FLanguageCodes.Add('kv=Komi');
    FLanguageCodes.Add('ky=Kirghiz');
    FLanguageCodes.Add('la=Latin');
    FLanguageCodes.Add('lb=Luxembourgish');
    FLanguageCodes.Add('lg=Ganda');
    FLanguageCodes.Add('li=Limburgan');
    FLanguageCodes.Add('ln=Lingala');
    FLanguageCodes.Add('lo=Lao');
    FLanguageCodes.Add('lt=Lithuanian');
    FLanguageCodes.Add('lu=Luba-Katanga');
    FLanguageCodes.Add('lv=Latvian');
    FLanguageCodes.Add('mg=Malagasy');
    FLanguageCodes.Add('mh=Marshallese');
    FLanguageCodes.Add('mi=Maori');
    FLanguageCodes.Add('mk=Macedonian');
    FLanguageCodes.Add('ml=Malayalam');
    FLanguageCodes.Add('mn=Mongolian');
    FLanguageCodes.Add('mo=Moldavian');
    FLanguageCodes.Add('mr=Marathi');
    FLanguageCodes.Add('ms=Malay');
    FLanguageCodes.Add('mt=Maltese');
    FLanguageCodes.Add('my=Burmese');
    FLanguageCodes.Add('na=Nauru');
    FLanguageCodes.Add('nb=Norwegian Bokmaal');
    FLanguageCodes.Add('nd=Ndebele, North');
    FLanguageCodes.Add('ne=Nepali');
    FLanguageCodes.Add('ng=Ndonga');
    FLanguageCodes.Add('nl=Dutch');
    FLanguageCodes.Add('nl_BE=Flemish');
    FLanguageCodes.Add('nn=Norwegian Nynorsk');
    FLanguageCodes.Add('no=Norwegian');
    FLanguageCodes.Add('nr=Ndebele, South');
    FLanguageCodes.Add('nv=Navajo');
    FLanguageCodes.Add('ny=Chichewa');
    FLanguageCodes.Add('oc=Occitan');
    FLanguageCodes.Add('oj=Ojibwa');
    FLanguageCodes.Add('om=Oromo');
    FLanguageCodes.Add('or=Oriya');
    FLanguageCodes.Add('os=Ossetian');
    FLanguageCodes.Add('pa=Panjabi');
    FLanguageCodes.Add('pi=Pali');
    FLanguageCodes.Add('pl=Polish');
    FLanguageCodes.Add('ps=Pushto');
    FLanguageCodes.Add('pt=Portuguese');
    FLanguageCodes.Add('pt_BR=Brazilian Portuguese');
    FLanguageCodes.Add('qu=Quechua');
    FLanguageCodes.Add('rm=Raeto-Romance');
    FLanguageCodes.Add('rn=Rundi');
    FLanguageCodes.Add('ro=Romanian');
    FLanguageCodes.Add('ru=Russian');
    FLanguageCodes.Add('rw=Kinyarwanda');
    FLanguageCodes.Add('sa=Sanskrit');
    FLanguageCodes.Add('sc=Sardinian');
    FLanguageCodes.Add('sd=Sindhi');
    FLanguageCodes.Add('se=Northern Sami');
    FLanguageCodes.Add('sg=Sango');
    FLanguageCodes.Add('si=Sinhalese');
    FLanguageCodes.Add('sk=Slovak');
    FLanguageCodes.Add('sl=Slovenian');
    FLanguageCodes.Add('sm=Samoan');
    FLanguageCodes.Add('sn=Shona');
    FLanguageCodes.Add('so=Somali');
    FLanguageCodes.Add('sq=Albanian');
    FLanguageCodes.Add('sr=Serbian');
    FLanguageCodes.Add('ss=Swati');
    FLanguageCodes.Add('st=Sotho, Southern');
    FLanguageCodes.Add('su=Sundanese');
    FLanguageCodes.Add('sv=Swedish');
    FLanguageCodes.Add('sw=Swahili');
    FLanguageCodes.Add('ta=Tamil');
    FLanguageCodes.Add('te=Telugu');
    FLanguageCodes.Add('tg=Tajik');
    FLanguageCodes.Add('th=Thai');
    FLanguageCodes.Add('ti=Tigrinya');
    FLanguageCodes.Add('tk=Turkmen');
    FLanguageCodes.Add('tl=Tagalog');
    FLanguageCodes.Add('tn=Tswana');
    FLanguageCodes.Add('to=Tonga');
    FLanguageCodes.Add('tr=Turkish');
    FLanguageCodes.Add('ts=Tsonga');
    FLanguageCodes.Add('tt=Tatar');
    FLanguageCodes.Add('tw=Twi');
    FLanguageCodes.Add('ty=Tahitian');
    FLanguageCodes.Add('ug=Uighur');
    FLanguageCodes.Add('uk=Ukrainian');
    FLanguageCodes.Add('ur=Urdu');
    FLanguageCodes.Add('uz=Uzbek');
    FLanguageCodes.Add('ve=Venda');
    FLanguageCodes.Add('vi=Vietnamese');
    FLanguageCodes.Add('vo=Volapuk');
    FLanguageCodes.Add('wa=Walloon');
    FLanguageCodes.Add('wo=Wolof');
    FLanguageCodes.Add('xh=Xhosa');
    FLanguageCodes.Add('yi=Yiddish');
    FLanguageCodes.Add('yo=Yoruba');
    FLanguageCodes.Add('za=Zhuang');
    FLanguageCodes.Add('zh=Chinese');
    FLanguageCodes.Add('zu=Zulu');
  finally
    FLanguageCodes.EndUpdate;
  end;
end;

destructor TdxLanguages.Destroy;
begin
  FreeAndNil(FLanguageCodes);
  inherited;
end;

procedure TdxLanguages.GetCodesAndLanguages(_sl: TStrings);
begin
  _sl.Assign(FLanguageCodes);
end;

procedure TdxLanguages.GetLanguageCodes(_sl: TStrings);
var
  i: Integer;
begin
  _sl.BeginUpdate;
  try
    for i := 0 to FLanguageCodes.Count - 1 do
      _sl.Add(FLanguageCodes.Names[i]);
  finally
    _sl.EndUpdate;
  end;
end;

procedure TdxLanguages.GetLanguages(_sl: TStrings);
var
  i: Integer;
begin
  _sl.BeginUpdate;
  try
    for i := 0 to FLanguageCodes.Count - 1 do
      _sl.Add(FLanguageCodes.ValueFromIndex[i]);
  finally
    _sl.EndUpdate;
  end;
end;

procedure TdxLanguages.GetLoacalizedLanguages(_sl: TStrings);
var
  i: Integer;
begin
  _sl.BeginUpdate;
  try
    for i := 0 to FLanguageCodes.Count - 1 do
      _sl.Add(TdxLanguage.Create(FLanguageCodes.Names[i]).LocalizedName);
  finally
    _sl.EndUpdate;
  end;
end;

function TdxLanguages.GetCodeForLanguage(const _Lng: string): string;
begin
  if not TryGetCodeForLanguage(_Lng, Result) then
    Result := '';
end;

function TdxLanguages.TryGetCodeForLanguage(const _Lng: string; out _Code: string): boolean;
var
  i: Integer;
begin
  for i := 0 to FLanguageCodes.Count - 1 do begin
    if SameText(FLanguageCodes.ValueFromIndex[i], _Lng) then begin
      _Code := FLanguageCodes.Names[i];
      Result := true;
      exit;
    end;
  end;
  Result := false;
end;

function TdxLanguages.GetLanguageForCode(const _Code: string): string;
begin
  Result := FLanguageCodes.Values[_Code];
end;

function TdxLanguages.TryGetLanguageForCode(const _Code: string; out _Lng: string): boolean;
var
  i: Integer;
begin
  for i := 0 to FLanguageCodes.Count - 1 do
  begin
    if SameText( FLanguageCodes.Names[i], _Code) then
    begin
      _Lng := FLanguageCodes.ValueFromIndex[i];

      Result := True;
      exit;
    end;
  end;

  //*** Test only the first 2 chars
  for i := 0 to FLanguageCodes.Count - 1 do
  begin
    if SameText( FLanguageCodes.Names[i], MidStr( _Code, 1, 2)) then
    begin
      _Lng := FLanguageCodes.ValueFromIndex[i];

      Result := True;
      exit;
    end;
  end;

  Result := False;
end;

{ TdxLanguage }

class function TdxLanguage.Create(const _Code: string): TdxLanguage;
begin
  Result.InitFromCode(_Code);
end;

function TdxLanguage.IsValid: boolean;
begin
  Result := (FCode <> '');
end;

procedure TdxLanguage.AssertValid;
begin
  if not IsValid then
    raise Exception.Create(_('TdxLanguage is not valid'));
end;

procedure TdxLanguage.InitFromCode(const _Code: string);
begin
  if not TryInitFromCode(_Code) then
    raise Exception.CreateFmt(_('Unknown language code %s.'), [_Code]);
end;

function TdxLanguage.TryInitFromCode(const _Code: string): boolean;
var
  lng: string;
begin
  Result := DxLanguages.TryGetLanguageForCode(_Code, lng);
  if Result then
    FCode := _Code
  else
    FCode := '';
  // I am not sure wether this is a problem with my code or with reference counting in the compiler,
  // but if the following is not called, the reference count of the parameter string might reach
  // zero even though there are still references to it.
  UniqueString(FCode);
end;

procedure TdxLanguage.InitFromName(const _EnglishName: string);
begin
  if not TryInitFromName(_EnglishName) then
    raise Exception.CreateFmt(_('Code for language %s not found.'), [_EnglishName]);
end;

function TdxLanguage.TryInitFromName(const _EnglishName: string): boolean;
begin
  Result := DxLanguages.TryGetCodeForLanguage(_EnglishName, FCode);
  if not Result then
    FCode := '';
  // I am not sure wether this is a problem with my code or with reference counting in the compiler,
  // but if the following is not called, the reference count of the parameter string might reach
  // zero even though there are still references to it.
  UniqueString(FCode);
end;

function TdxLanguage.Code: string;
begin
  AssertValid;
  Result := FCode;
end;

function TdxLanguage.EnglishName: string;
begin
  Result := DxLanguages.GetLanguageForCode(Code);
end;

function TdxLanguage.LocalizedName: string;
begin
  Result := dgettext('languages', EnglishName);
end;

initialization
finalization
  FreeAndNil(gblLanguages);
end.

