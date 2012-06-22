unit u_TranslationDBAccess;

interface

uses
  SysUtils,
  Classes,
  DB,
  ADODB,
  u_dzAdoDbUniqueId;

type
  TTranslationDbAccess = class
  private
    adc_Translation: TADOConnection;
    adq_English: TADOQuery;
    adt_Ids: TAdoTable;
    FIdProvider: TAdoDbUniqueIdProvider;
    FEnglishIdProvider: ITableIdProvider;
    FSourceIdProvider: ITableIdProvider;
    FLanguageIdProvider: ITableIdProvider;
    FAccessDb: string;
    FPrevSource: string;
    FPrevSourceId: Integer;
    function CreateAdoQuery(const _SQL: string): TADOQuery;
    function LocateInQuery(_qry: TAdoQuery; const _Content: string): boolean;
    function TryInsertSource(const _Source: string; out _SourceId: integer): boolean;
    function GetSource(_SourceId: integer): string;
  public
    constructor Create(const _AccessDb: string);
    destructor Destroy; override;
    procedure GetCurrentTranslations(_Translations, _Sources: TStrings);
    ///<summary> If Simulate is true, nothing will be added but the result will be true, if
    ///          the item would have been added, false otherwise </summary>
    function TryAddTranslation(const _English, _Translation, _Source: string; _Simulate: boolean): boolean;
    procedure ChangeTranslation(const _English, _OldTrans, _NewTrans, _Source: string);
    procedure DeleteTranslation(const _English, _Translation: string);
    function LocateEnglish(const _English: string): boolean;
    function LocateTranslation(const _Translation: string): boolean;
    function GetEnglishDs: TDataSet;
    function GetMdbFilename: string;
    class function EscapeStr(const s: string): string;
    class function UnescStr(const _s: string): string;
  end;

implementation

uses
  Variants,
  StrUtils,
  PoTools,
  poparser;

function TryVar2Int(const _v: variant; out _Value: integer): boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        Result := False;
    end;
end;

class function TTranslationDbAccess.EscapeStr(const s: string): string;
var
  i: integer;
  c: char;
  escnext: boolean;
begin
  Result := '';
  escnext := False;
  for i := 1 to length(s) do begin
    c := s[i];
    case c of
      #32..#33,
        #35..pred('%'),
        succ('%')..pred('*'),
        succ('*')..pred('\'),
        succ('\')..#65535: begin
          // This will also support 20-bit unicode points
          if escnext then
            Result := Result + '\';
          Result := Result + c;
          escnext := False;
        end;
      '%': begin
          Result := Result + '\x25';
          escnext := False;
        end;
      '*': begin
          Result := Result + '\x2A';
          escnext := False;
        end;
      '\': begin
          Result := Result + '\\';
          escnext := False;
        end;
      #13: ; // Do nothing
      #10: begin
          Result := Result + '\n';
          escnext := False;
        end;
      '"': begin
          Result := Result + '\x22';
          escnext := False;
        end;
      #0: begin
          Result := Result + '\0';
          escnext := True;
        end;
      #9: begin
          Result := Result + '\t';
          escnext := False;
        end;
    else
      Result := Result + '\x' + IntToHex(ord(c), 2);
      escnext := True;
    end;
  end;
end;

class function TTranslationDbAccess.UnescStr(const _s: string): string;
var
  i: Integer;
  WasEscape: boolean;
  s: string;
  Code: integer;
begin
  Result := '';
  WasEscape := False;
  i := 1;
  while i <= Length(_s) do begin
    if WasEscape then begin
      case _s[i] of
        '\': begin
            Result := Result + '\';
            Inc(i);
          end;
        'n': begin
            Result := Result + #10;
            Inc(i);
          end;
        '0': begin
            Result := Result + #0;
            Inc(i);
          end;
        't': begin
            Result := Result + #9;
            Inc(i);
          end;
        'x': begin
            Inc(i);
            s := Copy(_s, i, 2);
            Code := StrToInt('$' + s);
            s := chr(Code);
            Result := Result + s;
            Inc(i, 2);
          end;
      else
        Inc(i);
      end;
      WasEscape := False;
    end else begin
      if _s[i] = '\' then begin
        WasEscape := True;
        Inc(i);
      end else begin
        Result := Result + _s[i];
        Inc(i);
      end;
    end;
  end;
end;

{ Tdm_TranslationDbAccess }

constructor TTranslationDbAccess.Create(const _AccessDb: string);
const
  CONNECTION_STRING = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%S;';
var
  adc: TADOConnection;
begin
  inherited Create;

  FAccessDb := _AccessDb;

  adc_Translation := TADOConnection.Create(nil);
  adc := adc_Translation;
  adc.Name := 'adc_Translation';
  adc.LoginPrompt := False;
  adc.Mode := cmShareDenyNone;
  adc.Provider := 'Microsoft.Jet.OLEDB.4.0';
  adc.Connected := false;
  adc.ConnectionString := Format(CONNECTION_STRING, [_AccessDb]);
  adc.Connected := true;

  adq_English := CreateAdoQuery('select * from English order by Content');
  adq_English.Name := 'adq_English';
  adq_English.Open;
  adq_English.EnableControls;

  adt_Ids := TADOTable.Create(nil);
  adt_Ids.Name := 'adt_IDs';
  adt_Ids.Connection := adc_Translation;
  adt_Ids.TableName := 'IDs';
  FIdProvider := TAdoDbUniqueIdProvider.Create(adt_Ids);
  FEnglishIdProvider := FIdProvider.GetTableIdProvider('ENGLISH');
  FLanguageIdProvider := FIdProvider.GetTableIdProvider('TargetLang');
  FSourceIdProvider := FIdProvider.GetTableIdProvider('TranslationSources')
end;

destructor TTranslationDbAccess.Destroy;
begin
  FSourceIdProvider := nil;
  FEnglishIdProvider := nil;
  FLanguageIdProvider := nil;
  FreeAndNil(FIdProvider);
  FreeAndNil(adt_Ids);
  FreeAndNil(adq_English);
  FreeAndNil(adc_Translation);
  inherited;
end;

function TTranslationDbAccess.GetMdbFilename: string;
begin
  Result := FAccessDb;
end;

function TTranslationDbAccess.CreateAdoQuery(const _SQL: string): TADOQuery;
begin
  Result := TADOQuery.Create(nil);
  Result.Connection := adc_Translation;
  Result.DisableControls;
  Result.CursorLocation := clUseServer;
  Result.CursorType := ctKeyset;
  Result.SQL.Text := _SQL;
  Result.Prepared := true;
end;

function TTranslationDbAccess.LocateInQuery(_qry: TAdoQuery; const _Content: string): boolean;
var
  ContentField: TField;
begin
  ContentField := _qry.FieldByName('Content');
  // Unfortunately JET is case insensitive so we need to check ourselves whether the string
  // actually matches.
  while not _qry.Eof and (ContentField.AsString <> _Content) do begin
    _qry.Next;
  end;
  Result := (ContentField.AsString = _Content);
end;

function TTranslationDbAccess.GetSource(_SourceId: integer): string;
var
  qry: TAdoQuery;
begin
  if _SourceId = FPrevSourceId then begin
    Result := FPrevSource;
    exit;
  end;
  qry := CreateAdoQuery('select * from TranslationSources where ID = :d');
  try
    qry.Parameters.ParamByName('d').Value := _SourceId;
    qry.Open;
    if not qry.IsEmpty then
      Result := qry['Source']
    else
      Result := '';
  finally
    FreeAndNil(qry);
  end;
end;

function TTranslationDbAccess.TryInsertSource(const _Source: string; out _SourceId: integer): boolean;
var
  qry: TADOQuery;
  SourceField: TField;
begin
  Result := False;
  if _Source = FPrevSource then begin
    _SourceId := FPrevSourceId;
    exit;
  end;

  qry := CreateAdoQuery('select * from TranslationSources where Source = :s');
  try
    qry.Parameters.ParamByName('s').Value := _Source;
    qry.Open;
    // Unfortunately JET is case insensitive so we need to check ourselves whether the string
    // actually matches.
    SourceField := qry.FieldByName('Source');
    while not qry.Eof and (SourceField.AsString <> _Source) do begin
      qry.Next;
    end;
    if (SourceField.AsString = _Source) then begin
      _SourceId := qry['ID'];
      FPrevSourceId := _SourceId;
      FPrevSource := _Source;
      exit;
    end;
    qry.Insert;
    try
      SourceField.AsString := LeftStr(_Source, 50);
      _SourceId := FSourceIdProvider.GetNewId;
      qry['ID'] := _SourceId;
      qry.Post;
      FPrevSourceId := _SourceId;
      FPrevSource := _Source;
      Result := true;
    except
      qry.Cancel;
      raise;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

function TTranslationDbAccess.TryAddTranslation(const _English, _Translation, _Source: string;
  _Simulate: boolean): boolean;
var
  Id: integer;
  qry: TADOQuery;
  ContentField: TField;
  IdField: TField;
  English: string;
  Translation: string;
  SourceId: Integer;
begin
  Result := false;

  English := EscapeStr(_English);
  Translation := EscapeStr(_Translation);
  if (Length(English) > 255) or (Length(Translation) > 255) then
    exit;

  Id := 0; // otherwise the compiler complains at the end of the method, that ID might not have been
           // initialized
  qry := CreateAdoQuery('select * from English where Content = :s');
  try
    qry.Parameters.ParamByName('s').Value := English;
    qry.Open;
    ContentField := qry.FieldByName('Content');
    IdField := qry.FieldByName('ID');
    if not LocateInQuery(qry, English) then begin
      if _Simulate then begin
        Result := True;
        exit; // =>
      end;
      qry.Insert;
      try
        IdField.AsInteger := FEnglishIdProvider.GetNewId;
        ContentField.AsString := English;
        qry.Post;
      except
        qry.Cancel;
        raise;
      end;
    end;
    Assert(qry['Content'] = English);

    Id := IdField.AsInteger;
  finally
    FreeAndNil(qry);
  end;

  qry := CreateAdoQuery('select * from TargetLang where (English_ID = :d) and (Content = :s)');
  try
    qry.Parameters.ParamByName('d').Value := Id;
    qry.Parameters.ParamByName('s').Value := Translation;
    qry.Open;
    if not LocateInQuery(qry, Translation) then begin
      if _Simulate then begin
        Result := True;
        exit; // =>
      end;
      TryInsertSource(_Source, SourceId);
      qry.Insert;
      try
        qry['ID'] := FLanguageIdProvider.GetNewId;
        qry['English_ID'] := Id;
        qry['Content'] := Translation;
        qry['Source_ID'] := SourceId;
        qry.Post;
        Result := true;
      except
        qry.Cancel;
        raise;
      end;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

procedure TTranslationDbAccess.ChangeTranslation(const _English, _OldTrans, _NewTrans, _Source: string);
var
  Id: integer;
  qry: TADOQuery;
  ContentField: TField;
  IdField: TField;
  English: string;
  OldTrans: string;
  NewTrans: string;
  SourceId: Integer;
begin
  English := EscapeStr(_English);
  OldTrans := EscapeStr(_OldTrans);
  NewTrans := EscapeStr(_NewTrans);

  Id := 0; // otherwise the compiler complains at the end of the method, that ID might not have been
           // initialized
  qry := CreateAdoQuery('select * from English where Content = :s');
  try
    qry.Parameters.ParamByName('s').Value := English;
    qry.Open;
    ContentField := qry.FieldByName('Content');
    IdField := qry.FieldByName('ID');
    if not LocateInQuery(qry, English) then begin
      qry.Insert;
      try
        IdField.AsInteger := FEnglishIdProvider.GetNewId;
        ContentField.AsString := English;
        qry.Post;
      except
        qry.Cancel;
        raise;
      end;
    end;
    Assert(qry['Content'] = English);
    Id := IdField.AsInteger;
  finally
    FreeAndNil(qry);
  end;

  qry := CreateAdoQuery('select * from TargetLang where (English_ID = :d) and (Content = :s)');
  try
    // search OldTrans and delete it, if found
    qry.Parameters.ParamByName('d').Value := Id;
    qry.Parameters.ParamByName('s').Value := OldTrans;
    qry.Open;
    ContentField := qry.FieldByName('Content');
    while not qry.Eof do begin
      if ContentField.AsString = OldTrans then
        qry.Delete
      else
        qry.Next;
    end;
    qry.Close;

    // search NewTrans, add if if not found
    qry.Parameters.ParamByName('d').Value := Id;
    qry.Parameters.ParamByName('s').Value := NewTrans;
    qry.Open;
    if not LocateInQuery(qry, NewTrans) then begin
      TryInsertSource(_Source, SourceId);
      qry.Insert;
      try
        qry['ID'] := FLanguageIdProvider.GetNewId;
        qry['English_ID'] := Id;
        qry['Content'] := NewTrans;
        qry['Source_ID'] := SourceId;
        qry.Post;
      except
        qry.Cancel;
        raise;
      end;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

procedure TTranslationDbAccess.DeleteTranslation(const _English, _Translation: string);
var
  Id: integer;
  qry: TADOQuery;
  ContentField: TField;
  English: string;
  Translation: string;
begin
  English := EscapeStr(_English);
  Translation := EscapeStr(_Translation);

  qry := CreateAdoQuery('select * from English where Content = :s');
  try
    qry.Parameters.ParamByName('s').Value := English;
    qry.Open;
    if not LocateInQuery(qry, English) then
      Exit;
    Assert(qry['Content'] = English);
    Id := qry['ID'];
  finally
    FreeAndNil(qry);
  end;

  qry := CreateAdoQuery('select * from TargetLang where (English_ID = :d) and (Content = :s)');
  try
    qry.Parameters.ParamByName('d').Value := Id;
    qry.Parameters.ParamByName('s').Value := Translation;
    qry.Open;
    ContentField := qry.FieldByName('Content');
    while not qry.Eof do begin
      if ContentField.AsString = Translation then
        qry.Delete
      else
        qry.Next;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

procedure TTranslationDbAccess.GetCurrentTranslations(_Translations, _Sources: TStrings);
var
  Id: Integer;
  qry: TADOQuery;
  SourceId: Integer;
begin
  if not Assigned(self) or not Assigned(adq_English) then
    exit;
  if not TryVar2Int(adq_English['ID'], Id) then
    exit;

  qry := CreateAdoQuery('select * from TargetLang where English_ID = :d');
  try
    qry.Parameters.ParamByName('d').Value := Id;
    qry.Open;
    while not qry.Eof do begin
      _Translations.Add(UnescStr(qry['Content']));
      if TryVar2Int(qry['Source_ID'], SourceId) then
        _Sources.Add(GetSource(SourceId));
      qry.Next;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

function TTranslationDbAccess.GetEnglishDs: TDataSet;
begin
  Result := adq_English;
end;

function TTranslationDbAccess.LocateEnglish(const _English: string): boolean;
var
  English: string;
begin
  English := EscapeStr(_English);
  Result := adq_English.Locate('Content', English, [loCaseInsensitive, loPartialKey]);
end;

function TTranslationDbAccess.LocateTranslation(const _Translation: string): boolean;
var
  Id: Integer;
  CurrId: integer;
  qry: TADOQuery;
  Translation: string;
begin
  Result := false;
  if not TryVar2Int(adq_English['ID'], CurrId) then
    exit;

  Translation := EscapeStr(_Translation);
  try
    qry := CreateAdoQuery('select * from TargetLang where Content like :s');
    try
      qry.Parameters.ParamByName('s').Value := Translation + '%';
      qry.Open;
      if qry.IsEmpty then
        exit;

      // we do not care whether the entry matches the case
      Id := qry['English_Id'];
    finally
      FreeAndNil(qry);
    end;

    if not adq_English.Locate('ID', Id, []) then
      exit;
    Result := true;
  finally
    if not Result then
      adq_English.Locate('ID', CurrId, []);
  end;
end;

end.

