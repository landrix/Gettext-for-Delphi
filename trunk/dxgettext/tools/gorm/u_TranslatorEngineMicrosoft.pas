unit u_TranslatorEngineMicrosoft;

interface

uses
  SysUtils,
  Classes,
  Forms,
  Menus,
  Generics.Collections,
  PoTools,
  u_TranslatorEngine,
  u_Languages,
  u_dzMicrosoftTranslate;

type
  TTranslatorEngineMicrosoft = class(TTranslatorEngine)
  private
    FFromLngCode: string;
    FToLngCode: string;
    FMicrosoftTranslate: TMicrosoftTranslate;
    FPendingList: TStringList;
    FDictionary: TDictionary<string, string>;
    FAppId: string;
    function GetCacheFilename: string;
    function Request: boolean;
    procedure LoadDictionaryFromFile(const _Filename: string);
    procedure SaveDictionaryToFile(const _Filename: string);
  protected
    function doTranslate(const _Orig: string; out _Translation: string): TTranslatorEngine.TTranslateResult; override;
  public
    constructor Create(const _AppId: string; const _FromLngCode, _ToLngCode: string);
    destructor Destroy; override;
    ///<summary>The Microsoft Translate API has limits on the number of translations per hour and day.
    ///         We will not do batch translations just in case.</summary>
    function AllowsBatchTranslation: boolean; override;
    function GetDescription: string; override;
    function TargetLanguage: TdxLanguage; override;
    function BatchTranslate(_Orig, _Trans: TStrings): TTranslatorEngine.TTranslateResult; override;
    procedure AsyncTranslate(const _Orig: string); override;
  end;

implementation

uses
  poparser,
  gnugettext;

{ TTranslatorEngineMicrosoft }

constructor TTranslatorEngineMicrosoft.Create(const _AppId: string; const _FromLngCode, _ToLngCode: string);
var
  fn: string;
begin
  inherited Create;
  FAppId := _AppId;
  FFromLngCode := _FromLngCode;
  FToLngCode := _ToLngCode;
  FPendingList := TStringList.Create;
  FPendingList.Sorted := true;
  FPendingList.Duplicates := dupIgnore;
  FDictionary := TDictionary<string, string>.Create;
  fn := GetCacheFilename;
  if FileExists(fn) then
    LoadDictionaryFromFile(fn);
  FMicrosoftTranslate := TMicrosoftTranslate.Create(FAppId, FFromLngCode, FToLngCode);
end;

destructor TTranslatorEngineMicrosoft.Destroy;
var
  fn: string;
begin
  FreeAndNil(FMicrosoftTranslate);
  if Assigned(FDictionary) and (FDictionary.Count > 0) then begin
    try
      fn := GetCacheFilename;
      SaveDictionaryToFile(fn);
    except
      // ignore
    end;
  end;
  FDictionary.Free;
  FreeAndNil(FPendingList);
  inherited;
end;

function TTranslatorEngineMicrosoft.GetDescription: string;
begin
  Result := Format(_('Microsoft translate %s -> %s (%s)'), [FFromLngCode, FToLngCode, FMicrosoftTranslate.GetBranding]);
end;

function TTranslatorEngineMicrosoft.GetCacheFilename: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
    + Format('Microsoft-%s-%s.po', [FFromLngCode, FToLngCode]);
end;

function TTranslatorEngineMicrosoft.TargetLanguage: TdxLanguage;
begin
  Result.TryInitFromCode(FToLngCode);
end;

procedure TTranslatorEngineMicrosoft.LoadDictionaryFromFile(const _Filename: string);
var
  pol: TPoEntryList;
  po: TPoEntry;
begin
  pol := TPoEntryList.Create;
  try
    try
      pol.LoadFromFile(_Filename);
    except
      on E: exception do
        ; // ignore any exceptions
    end;
    po := pol.FindFirst;
    while Assigned(po) do begin
      if (po.MsgId <> '') and (po.MsgStr <> '') then
        FDictionary.AddOrSetValue(po.MsgId, po.MsgStr);
      po := pol.FindNext(po);
    end;
  finally
    FreeAndNil(pol);
  end;
end;

procedure TTranslatorEngineMicrosoft.SaveDictionaryToFile(const _Filename: string);
var
  pol: TPoEntryList;
  po: TPoEntry;
  Orig: string;
  Trans: string;
  enum: TDictionary<string, string>.TPairEnumerator;
begin
  pol := TPoEntryList.Create;
  try
    po := TPoEntry.Create;
    po.UserCommentList.Add('# Cache for Gorm''s Microsoft Translation Engine');
    po.MsgStr := 'automatically created from the entries that get fetched by the engine' + #13#10
      + 'POT-Creation-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + #13#10
      + 'PO-Revision-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + #13#10
      + 'Last-Translator: Somebody <your.email@address.com>' + #13#10
      + 'MIME-Version: 1.0' + #13#10
      + 'Content-Type: text/plain; charset=UTF-8' + #13#10
      + 'Content-Transfer-Encoding: 8bit' + #13#10
      + 'X-Generator: Gorm' + #13#10;
    pol.Add(po);
    enum := FDictionary.GetEnumerator;
    while enum.MoveNext do begin
      Orig := enum.Current.Key;
      Trans := enum.Current.Value;
      po := TPoEntry.Create;
      po.MsgId := Orig;
      po.MsgStr := Trans;
      pol.Add(po);
    end;
    try
      pol.SaveToFile(_Filename);
    except
      on E: exception do
        ; // ignore any exceptions
    end;
  finally
    FreeAndNil(pol);
  end;
end;

function TTranslatorEngineMicrosoft.doTranslate(const _Orig: string; out _Translation: string): TTranslatorEngine.TTranslateResult;
var
  s: string;
  Stripped: string;
begin
   // Microsoft does not handle & very well so we strip them for better results
  Stripped := StripHotkey(_Orig);
  if FDictionary.TryGetValue(Stripped, s) then begin
    _Translation := s;
    Result := trOK;
  end else begin
    AsyncTranslate(Stripped);
    Result := trRetryLater
  end
end;

function TTranslatorEngineMicrosoft.BatchTranslate(_Orig, _Trans: TStrings): TTranslatorEngine.TTranslateResult;
var
  i: Integer;
  Orig: string;
  Trans: string;
begin
  _Trans.Clear;
  for i := 0 to _Orig.Count - 1 do begin
    Orig := StripHotkey(_Orig[i]);
    if not FDictionary.ContainsKey(Orig) then
      FPendingList.Add(Orig);
  end;
  if FPendingList.Count > 0 then
    Request;
  for i := 0 to _Orig.Count - 1 do begin
    Orig := StripHotkey(_Orig[i]);
    if FDictionary.TryGetValue(Orig, Trans) then
      _Trans.Add(Trans)
    else
      _Trans.Add('');
  end;
  Result := trOK;
end;

function TTranslatorEngineMicrosoft.AllowsBatchTranslation: boolean;
begin
  Result := false;
end;

procedure TTranslatorEngineMicrosoft.AsyncTranslate(const _Orig: string);
const
  MIN_COUNT_TO_TRANSLATE = 1; // was 10 previously
var
  s: string;
  Stripped: string;
begin
  // Microsoft does not handle & very well so we strip them for better results
  Stripped := StripHotkey(_Orig);
  if not FDictionary.TryGetValue(Stripped, s) then begin
    FPendingList.Add(Stripped);
    if FPendingList.Count >= MIN_COUNT_TO_TRANSLATE then
      Request;
  end;
end;

function TTranslatorEngineMicrosoft.Request: boolean;
var
  i: Integer;
  Orig: string;
  Trans: string;
begin
  for i := FPendingList.Count - 1 downto 0 do begin
    Orig := FPendingList[i];
    FPendingList.Delete(i);
    Trans := FMicrosoftTranslate.Translate(Orig);
    FDictionary.AddOrSetValue(Orig, Trans);
    doOnTranslated(Orig, Trans);
  end;
  Result := true;
end;

end.

