unit u_TranslationRepository;

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_TranslationDBAccess;

type
  TTranslationRepository = class
  private
    FTemplateFile: string;
    FRepDirBS: string;
  public
    constructor Create(const _RepDir: string; const _TemplateFile: string = '');
    procedure GetExistingLanguages(_sl: TStrings);
    function TryGetRepository(const _LngCode: string; out _dm: TTranslationDbAccess): boolean;
    function TryGetDatabaseFilename(const _LngCode: string; out _fn: string): boolean;
    procedure GenerateNew(const _LngCode: string);
  end;

implementation

uses
  gnugettext;

{ TTranslationRepository }

constructor TTranslationRepository.Create(const _RepDir: string; const _TemplateFile: string = '');
begin
  inherited Create;
  FRepDirBS := IncludeTrailingPathDelimiter(_RepDir);
  if _TemplateFile = '' then
    FTemplateFile := FRepDirBS + 'TranslateDB.mdb'
  else
    FTemplateFile := _TemplateFile;
end;

procedure TTranslationRepository.GenerateNew(const _LngCode: string);
var
  fn: string;
  ErrCode: LongWord;
  Err: string;
begin
  fn := FRepDirBS + 'TranslateDB_' + _LngCode + '.mdb';
  if CopyFile(PChar(FTemplateFile), PChar(fn), true) then
    exit;
  ErrCode := GetLastError;
  Err := SysErrorMessage(ErrCode);
  raise Exception.CreateFmt(_('Error copying file'#13#10'%s'#13#10'to'#13#10'%s'#13#10'%s (%d)'),
    [FTemplateFile, fn, Err, ErrCode]);
end;

procedure TTranslationRepository.GetExistingLanguages(_sl: TStrings);
var
  sr: TSearchRec;
  fn: string;
begin
  _sl.Clear;
  if 0 = FindFirst(FRepDirBS + 'TranslateDB_*.mdb', faAnyFile, sr) then begin
    try
      repeat
        if (sr.Attr and faDirectory) = 0 then begin
          fn := sr.Name;
          fn := ChangeFileExt(fn, '');
          _sl.Add(Copy(fn, 13));
        end;
      until 0 <> FindNext(sr);
    finally
      FindClose(sr);
    end;
  end;
end;

function TTranslationRepository.TryGetDatabaseFilename(const _LngCode: string;
  out _fn: string): boolean;
begin
  _fn := FRepDirBS + 'TranslateDB_' + _LngCode + '.mdb';
  Result := FileExists(_fn);
end;

function TTranslationRepository.TryGetRepository(const _LngCode: string;
  out _dm: TTranslationDbAccess): boolean;
var
  fn: string;
begin
  Result := TryGetDatabaseFilename(_LngCode, fn);
  if Result then begin
    _dm := TTranslationDbAccess.Create(fn);
  end;
end;

end.

