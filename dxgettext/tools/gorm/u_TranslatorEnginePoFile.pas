unit u_TranslatorEnginePoFile;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  StrUtils,
  u_TranslatorEngine,
  u_Languages;

type
  TTranslatorEnginePoFile = class(TTranslatorEngine)
  private
    FPoFile: string;
    FLanguage: TdxLanguage;
    FDictionary: TDictionary < string, string > ;
  protected
    function doTranslate(const _Orig: string; out _Translation: string): TTranslatorEngine.TTranslateResult; override;
  public
    constructor Create(const _PoFile: string);
    destructor Destroy; override;
    function GetDescription: string; override;
    function TargetLanguage: TdxLanguage; override;
  end;

implementation

uses
  GnuGetText,
  poparser;

{ TTranslatorEnginePoFile }

constructor TTranslatorEnginePoFile.Create(const _PoFile: string);
var
  pol: TPoEntryList;
  po: TPoEntry;
begin
  inherited Create;
  FPoFile := _PoFile;
  FDictionary := TDictionary < string, string > .Create;

  Pol := TPoEntryList.Create;
  try
    try
      pol.LoadFromFile(FPoFile);
    except
      on E: exception do
        ; // ignore any exceptions
    end;
    po := pol.FindFirst;
    while Assigned(po) do begin
      if (po.MsgId <> '') and (po.MsgStr <> '') and not (po.Fuzzy) then
        FDictionary.AddOrSetValue(po.MsgId, po.MsgStr);
      po := pol.FindNext(po);
    end;
    FLanguage.TryInitFromCode( MidStr( pol.Language_New, 1, 2));
  finally
    FreeAndNil(pol);
  end;
end;

destructor TTranslatorEnginePoFile.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

function TTranslatorEnginePoFile.GetDescription: string;
begin
  Result := Format(_('From PO file %s'), [FPoFile]);
end;

function TTranslatorEnginePoFile.TargetLanguage: TdxLanguage;
begin
  Result := FLanguage;
end;

function TTranslatorEnginePoFile.doTranslate(const _Orig: string;
  out _Translation: string): TTranslatorEngine.TTranslateResult;
var
  s: string;
begin
  if FDictionary.TryGetValue(_Orig, s) then begin
    _Translation := s;
    Result := trOK;
  end else begin
    Result := trFailed;
  end
end;

end.

