unit u_TranslatorEngineGetText;

interface

uses
  SysUtils,
  Classes,
  u_TranslatorEngine,
  u_Languages;

type
  TTranslatorEngineGetText = class(TTranslatorEngine)
  private
    FMoFile: string;
    FDomain: string;
  protected
    function doTranslate(const _Orig: string; out _Translation: string): TTranslatorEngine.TTranslateResult; override;
  public
    constructor Create(const _MoFile: string; const _Domain: string = 'parallel');
    function GetDescription: string; override;
    function TargetLanguage: TdxLanguage; override;
  end;

implementation

uses
  StrUtils,
  gnugettext,
  poparser;

{ TTranslatorEngineGetText }

constructor TTranslatorEngineGetText.Create(const _MoFile: string; const _Domain: string = 'parallel');
begin
  inherited Create;
  FMoFile := _MoFile;
  FDomain := _Domain;
  DefaultInstance.bindtextdomainToFile(FDomain, FMoFile);
end;

function TTranslatorEngineGetText.GetDescription: string;
begin
  Result := Format(_('From MO file %s'), [FMoFile]);
end;

function TTranslatorEngineGetText.TargetLanguage: TdxLanguage;
var
  Header: string;
  Lng: string;
begin
  Header := dgettext(FDomain, '');
  Lng := GetPoHeaderEntry(Header, PO_HEADER_LANGUAGE);
  Result.TryInitFromName(Lng);
end;

function TTranslatorEngineGetText.doTranslate(const _Orig: string;
  out _Translation: string): TTranslatorEngine.TTranslateResult;

  function CrLf2Cr(const s: string): string;
  begin
    Result := ReplaceStr(s, #13#10, #13);
    Result := ReplaceStr(Result, #10, #13);
  end;

var
  s: string;
begin
  s := dgettext(FDomain, _Orig);
  if (s = '') or (CrLf2Cr(_Orig) = CrLf2Cr(s)) then
    Result := trFailed
  else begin
    _Translation := s;
    Result := trOK;
  end;
end;

end.

