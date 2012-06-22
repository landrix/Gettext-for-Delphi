unit u_TranslatorEngineRepository;

interface

uses
  SysUtils,
  Classes,
  u_TranslatorEngine,
  ADODB,
  DB,
  u_Languages,
  u_TranslationRepository;

type
  TTranslatorEngineRepository = class(TTranslatorEngine)
  private
    FAccessDb: string;
    FConnection: TAdoConnection;
    FEnglish: TAdoQuery;
    FEnglishParam: TParameter;
    FTranslation: TAdoQuery;
    FTranslationParam: TParameter;
    FLanguage: TdxLanguage;
    function CreateAdoQuery(const _SQL: string): TADOQuery;
  protected
    function doTranslate(const _Orig: string; out _Translation: string): TTranslatorEngine.TTranslateResult; override;
  public
    constructor Create(_Repository: TTranslationRepository; const _LngCode: string); overload;
    constructor Create(const _AccessDb, _LngCode: string); overload;
    destructor Destroy; override;
    function GetDescription: string; override;
    function TargetLanguage: TdxLanguage; override;
  end;

implementation

uses
  gnugettext,
  u_TranslationDBAccess;

{ TTranslatorEngineRepository }

constructor TTranslatorEngineRepository.Create(_Repository: TTranslationRepository;
  const _LngCode: string);
var
  fn: string;
begin
  FLanguage.InitFromCode(_LngCode);
  if not _Repository.TryGetDatabaseFilename(_LngCode, fn) then
    raise Exception.CreateFmt(_('Translation repository for language %s does not exist.'), [FLanguage.EnglishName]);
  Create(fn, _LngCode);
end;

constructor TTranslatorEngineRepository.Create(const _AccessDb, _LngCode: string);
const
  CONNECTION_STRING = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%S;';
begin
  inherited Create;

  FLanguage.InitFromCode(_LngCode);

  FAccessDb := _AccessDb;

  FConnection := TADOConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.Mode := cmShareDenyNone;
  FConnection.Provider := 'Microsoft.Jet.OLEDB.4.0';
  FConnection.Connected := false;
  FConnection.ConnectionString := Format(CONNECTION_STRING, [_AccessDb]);
  FConnection.Connected := true;

  FEnglish := CreateAdoQuery('select ID,Content from English where Content=:s');
  FEnglish.Name := 'adq_English';
  FEnglishParam := FEnglish.Parameters.ParamByName('s');

  FTranslation := CreateAdoQuery('select Content from TargetLang where English_ID = :d');
  FTranslation.Name := 'adq_Translation';
  FTranslationParam := FTranslation.Parameters.ParamByName('d');
end;

destructor TTranslatorEngineRepository.Destroy;
begin
  FreeAndNil(FTranslation);
  FreeAndNil(FEnglish);
  FreeAndNil(FConnection);

  inherited;
end;

function TTranslatorEngineRepository.CreateAdoQuery(const _SQL: string): TADOQuery;
begin
  Result := TADOQuery.Create(nil);
  Result.Connection := FConnection;
  Result.DisableControls;
  Result.CursorLocation := clUseServer;
  Result.CursorType := ctKeyset;
  Result.SQL.Text := _SQL;
  Result.Prepared := true;
end;

function TTranslatorEngineRepository.GetDescription: string;
begin
  Result := Format(_('Translation repository %s'), [ExtractFileName(FAccessDb)]);
end;

function TTranslatorEngineRepository.TargetLanguage: TdxLanguage;
begin
  Result := FLanguage;
end;

function TTranslatorEngineRepository.doTranslate(const _Orig: string; out _Translation: string): TTranslatorEngine.TTranslateResult;
var
  EnglishValueContent: TField;
  Orig: string;
begin
  Result := trFailed;

  Orig := TTranslationDbAccess.EscapeStr(_Orig);

  FEnglish.Close;
  FEnglishParam.Value := Orig;
  FEnglish.Open;
  EnglishValueContent := FEnglish.FieldByName('Content');

  // this caters for case insensitivity of JET databases
  while not FEnglish.Eof and (EnglishValueContent.AsString <> Orig) do
    FEnglish.Next;
  if FEnglish.Eof then
    exit;

  FTranslation.Close;
  FTranslationParam.Value := FEnglish.FieldByName('ID').AsInteger;
  FTranslation.Open;
  if FTranslation.IsEmpty then
    exit;

  _Translation := TTranslationDbAccess.UnescStr(FTranslation.FieldByName('Content').AsString);
  Result := trOk;
end;

end.

