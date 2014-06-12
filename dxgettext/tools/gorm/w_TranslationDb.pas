unit w_TranslationDb;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Types,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  DBGrids,
  DB,
  u_TranslationDbAccess,
  ExtCtrls,
  DBCtrls,
  ComCtrls,
  ActnList,
  StdCtrls,
  u_TranslationRepository;

type
  Tf_TranslationDb = class(TForm)
    ds_English: TDataSource;
    al_Translation: TActionList;
    act_TransAdd: TAction;
    act_TransEdit: TAction;
    act_TransDel: TAction;
    act_TransEditOk: TAction;
    act_TransEditCancel: TAction;
    act_TransAddOk: TAction;
    act_TransAddCancel: TAction;
    tc_Language: TTabControl;
    p_Bottom: TPanel;
    pc_Translations: TPageControl;
    p_BottomRight: TPanel;
    b_One: TButton;
    b_Two: TButton;
    b_Delete: TButton;
    m_TransEdit: TMemo;
    dbg_English: TDBGrid;
    dbn_English: TDBNavigator;
    b_Close: TButton;
    act_Close: TAction;
    p_Locate: TPanel;
    ed_LocateEnglish: TEdit;
    l_LookupEnglish: TLabel;
    l_LookupTranslation: TLabel;
    ed_LocateTranslation: TEdit;
    procedure ds_EnglishDataChange(Sender: TObject; Field: TField);
    procedure act_TransEditExecute(Sender: TObject);
    procedure act_TransDelExecute(Sender: TObject);
    procedure act_TransEditOkExecute(Sender: TObject);
    procedure act_TransEditCancelExecute(Sender: TObject);
    procedure act_TransAddExecute(Sender: TObject);
    procedure act_TransAddOkExecute(Sender: TObject);
    procedure act_TransAddCancelExecute(Sender: TObject);
    procedure m_TransEditChange(Sender: TObject);
    procedure tc_LanguageChanging(Sender: TObject; var AllowChange: Boolean);
    procedure tc_LanguageChange(Sender: TObject);
    procedure act_CloseExecute(Sender: TObject);
    procedure ed_LocateEnglishChange(Sender: TObject);
    procedure ed_LocateTranslationChange(Sender: TObject);
  private
    Fdm: TTranslationDbAccess;
    FRepository: TTranslationRepository;
    FOriginalTranslation: string;
    procedure SetRepository(_Repository: TTranslationRepository);
    procedure SwitchToTranslationEdit(_Idx: integer);
    procedure SwitchToTranslationView(out _s: string);
    function AddTranslationPage: integer;
    procedure DeleteTranslationPage(_Idx: integer);
    function CreateNewLanguage(_Owner: TComponent; out _LngCode: string): boolean;
    procedure UpdateTranslationDisplay;
    procedure SwitchLanguage(_LngCode: string);
    procedure InitLanguages;
  public
    class procedure Execute(_Owner: TComponent; _Repository: TTranslationRepository);
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  u_dzVclUtils,
  u_Languages,
  w_TranslationDbNew;

{$R *.dfm}

{ Tf_TranslationDb }

class procedure Tf_TranslationDb.Execute(_Owner: TComponent; _Repository: TTranslationRepository);
var
  frm: Tf_TranslationDb;
begin
  frm := Tf_TranslationDb.Create(_Owner);
  try
    frm.SetRepository(_Repository);
    frm.UpdateTranslationDisplay;
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_TranslationDb.Create(_Owner: TComponent);
begin
  inherited;

  TForm_ReadPlacement(self, fpePosAndSize);

  TranslateComponent(Self);
  // make sure the field name does not get translated
  dbg_English.Columns[0].FieldName := 'Content';
end;

destructor Tf_TranslationDb.Destroy;
begin
  TForm_StorePlacement(self, fpePosAndSize);

  FreeAndNil(Fdm);
  inherited;
end;

procedure Tf_TranslationDb.InitLanguages;
var
  sl: TStringList;
  s: string;
  lng: TdxLanguage;
begin
  sl := TStringList.Create;
  try
    FRepository.GetExistingLanguages(sl);
    if sl.Count = 0 then begin
      if not CreateNewLanguage(Owner, s) then
        Abort;
      FRepository.GetExistingLanguages(sl);
    end;

    tc_Language.Tabs.BeginUpdate;
    try
      tc_Language.Tabs.Clear;
      for s in sl do begin
        lng.InitFromCode(s);
        tc_Language.Tabs.Add(lng.LocalizedName);
      end;
      tc_Language.Tabs.Add(_('new ...'));
    finally
      tc_Language.Tabs.EndUpdate;
    end;
    if sl.Count = 0 then
      fdm := nil
    else if not FRepository.TryGetRepository(sl[0], Fdm) then
      raise Exception.CreateFmt(_('Could not open repository for language %s'), [sl[0]]);
  finally
    FreeAndNil(sl);
  end;
end;

procedure Tf_TranslationDb.m_TransEditChange(Sender: TObject);
var
  b: boolean;
begin
  b := m_TransEdit.Lines.Text <> '';
  act_TransEditOk.Enabled := b;
  act_TransAddOk.Enabled := b;
end;

procedure Tf_TranslationDb.act_CloseExecute(Sender: TObject);
begin
  Close;
end;

procedure Tf_TranslationDb.act_TransAddExecute(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := AddTranslationPage;
  pc_Translations.ActivePageIndex := Idx;
  b_One.Action := act_TransAddOk;
  b_Two.Action := act_TransAddCancel;
  SwitchToTranslationEdit(Idx);
end;

procedure Tf_TranslationDb.act_TransEditExecute(Sender: TObject);
var
  Idx: integer;
begin
  Idx := pc_Translations.ActivePageIndex;
  if Idx = -1 then
    exit;
  b_One.Action := act_TransEditOk;
  b_Two.Action := act_TransEditCancel;
  SwitchToTranslationEdit(Idx);
end;

procedure Tf_TranslationDb.act_TransDelExecute(Sender: TObject);
var
  engl: string;
  Trans: string;
  Idx: Integer;
  m: TMemo;
begin
  Idx := pc_Translations.ActivePageIndex;
  if Idx = -1 then
    exit;
  m := pc_Translations.Pages[Idx].Controls[0] as TMemo;
  Trans := m.Lines.Text;
  engl := TTranslationDbAccess.UnescStr(ds_English.DataSet['Content']);
  if MessageDlg(Format(_('Really delete this translation?'#13#10
    + '%s'#13#10
    + '-->'#13#10
    + '%s'), [engl, trans]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;
  Fdm.DeleteTranslation(Engl, Trans);
  UpdateTranslationDisplay;
end;

procedure Tf_TranslationDb.act_TransAddOkExecute(Sender: TObject);
var
  engl: string;
  Trans: string;
begin
  SwitchToTranslationView(Trans);
  engl := ds_English.DataSet['Content'];
  Fdm.TryAddTranslation(engl, Trans, 'manual', False);
  UpdateTranslationDisplay;
end;

procedure Tf_TranslationDb.act_TransAddCancelExecute(Sender: TObject);
var
  s: string;
begin
  SwitchToTranslationView(s);
  DeleteTranslationPage(pc_Translations.PageCount - 1);
end;

procedure Tf_TranslationDb.act_TransEditOkExecute(Sender: TObject);
var
  engl: string;
  Trans: string;
begin
  SwitchToTranslationView(Trans);
  if Trans = FOriginalTranslation then
    exit;
  engl := ds_English.DataSet['Content'];
  Fdm.ChangeTranslation(engl, FOriginalTranslation, Trans, 'manual');
  UpdateTranslationDisplay;
end;

procedure Tf_TranslationDb.act_TransEditCancelExecute(Sender: TObject);
var
  s: string;
begin
  SwitchToTranslationView(s);
end;

procedure Tf_TranslationDb.SwitchToTranslationEdit(_Idx: integer);
var
  m: TMemo;
  pnt: TPoint;
begin
  m := pc_Translations.Pages[_Idx].Controls[0] as TMemo;
  pnt := m.ClientToParent(Point(0, 0), p_Bottom);
  m_TransEdit.SetBounds(pnt.X, pnt.Y, m.Width, m.Height);
  m_TransEdit.Lines.Assign(m.Lines);
  m_TransEdit.Visible := true;
  FOriginalTranslation := m.Lines.Text;

  pc_Translations.Enabled := false;
  dbg_English.Enabled := false;
  b_Delete.Visible := false;
  dbn_English.Enabled := false;

  m_TransEdit.SetFocus;
end;

procedure Tf_TranslationDb.SwitchToTranslationView(out _s: string);
begin
  _s := m_TransEdit.Lines.Text;
  m_TransEdit.Visible := false;
  pc_Translations.Enabled := true;
  dbg_English.Enabled := true;
  b_One.Action := act_TransAdd;
  b_Two.Action := act_TransEdit;
  b_Delete.Visible := true;
  dbn_English.Enabled := true;
end;

function Tf_TranslationDb.CreateNewLanguage(_Owner: TComponent; out _LngCode: string): boolean;
begin
  Result := false;
  if not Tf_TranslationDbNew.Execute(_Owner, FRepository, _LngCode) then
    exit;
  Result := true;
  FRepository.GenerateNew(_Lngcode);
end;

procedure Tf_TranslationDb.tc_LanguageChange(Sender: TObject);
var
  Idx: integer;
  s: string;
  Code: string;
begin
  Idx := tc_Language.TabIndex;
  if Idx = -1 then
    exit;
  if Idx = tc_Language.Tabs.Count - 1 then begin
    if not CreateNewLanguage(Self, Code) then begin
      tc_Language.TabIndex := 0;
      exit;
    end;
    InitLanguages;
  end else begin
    s := tc_Language.Tabs[Idx];
    Code := DxLanguages.GetCodeForLanguage(s);
  end;
  SwitchLanguage(Code);
end;

procedure Tf_TranslationDb.SwitchLanguage(_LngCode: string);
begin
  ds_English.DataSet := nil;
  FreeAndNil(Fdm);
  if not FRepository.TryGetRepository(_LngCode, Fdm) then
    raise Exception.CreateFmt(_('Could not open repository for language %s'), [_LngCode]);
  ds_English.DataSet := Fdm.GetEnglishDs;
end;

procedure Tf_TranslationDb.tc_LanguageChanging(Sender: TObject; var AllowChange: Boolean);
begin
  { TODO -otwm : check if editing }
  AllowChange := true;
end;

procedure Tf_TranslationDb.SetRepository(_Repository: TTranslationRepository);
begin
  Assert(Assigned(_Repository));

  FRepository := _Repository;
  InitLanguages;
  if Assigned(Fdm) then
    ds_English.DataSet := Fdm.GetEnglishDs;
end;

function Tf_TranslationDb.AddTranslationPage: integer;
var
  m: TMemo;
  ts: TTabSheet;
  ed: TEdit;
begin
  Result := pc_Translations.PageCount;
  ts := TTabSheet.Create(self);
  ts.Name := Format('ts_Translate%d', [Result]);
  ts.Parent := pc_Translations;
  ts.PageControl := pc_Translations;
  ts.Caption := Format(_('Translation %d'), [Result + 1]);
  m := TMemo.Create(ts);
  m.Parent := ts;
  m.Align := alClient;
  m.Name := Format('m_Translate%d', [Result]);
  m.Lines.Clear;
  m.ReadOnly := true;
  ed := TEdit.Create(ts);
  ed.Parent := ts;
  ed.Align := alBottom;
  ed.Name := Format('ed_Source%d', [Result]);
  ed.Text := '';
  ed.ReadOnly := true;
end;

procedure Tf_TranslationDb.DeleteTranslationPage(_Idx: integer);
begin
  pc_Translations.Pages[_Idx].Free;
end;

procedure Tf_TranslationDb.ds_EnglishDataChange(Sender: TObject; Field: TField);
begin
  UpdateTranslationDisplay;
end;

procedure Tf_TranslationDb.UpdateTranslationDisplay;
var
  Translations: TStringList;
  Sources: TStringList;
  i: Integer;
  ts: TTabSheet;
  m: TMemo;
  cnt: Integer;
  ed: TEdit;
begin
  Sources := nil;
  Translations := TStringList.Create;
  try
    Sources := TStringList.Create;
    Fdm.GetCurrentTranslations(Translations, Sources);
    cnt := Translations.Count;
    while pc_Translations.PageCount > cnt do begin
      DeleteTranslationPage(pc_Translations.PageCount - 1);
    end;
    while pc_Translations.PageCount < cnt do begin
      AddTranslationPage;
    end;

    for i := 0 to Translations.Count - 1 do begin
      ts := pc_Translations.Pages[i];
      Assert(ts.ControlCount > 0);
      m := ts.Controls[0] as TMemo;
      m.Lines.Text := Translations[i];
      ed := ts.Controls[1] as TEdit;
      ed.Text := Sources[i];
    end;
  finally
    FreeAndNil(Sources);
    FreeAndNil(Translations);
  end;
end;

procedure Tf_TranslationDb.ed_LocateEnglishChange(Sender: TObject);
var
  s: string;
begin
  s := ed_LocateEnglish.Text;
  if s <> '' then
    Fdm.LocateEnglish(s)
end;

procedure Tf_TranslationDb.ed_LocateTranslationChange(Sender: TObject);
var
  s: string;
begin
  s := ed_LocateTranslation.Text;
  if s <> '' then
    Fdm.LocateTranslation(s);
end;

end.

