unit w_TranslationDbNew;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  u_TranslationRepository;

type
  Tf_TranslationDbNew = class(TForm)
    l_Language: TLabel;
    cmb_Language: TComboBox;
    b_Ok: TButton;
    b_Cancel: TButton;
    procedure cmb_LanguageChange(Sender: TObject);
  private
    procedure SetData(_Repository: TTranslationRepository; const _LngCode: string);
    procedure GetData(var _LngCode: string);
  public
    class function Execute(_Owner: TComponent; _Repository: TTranslationRepository;
      var _LngCode: string): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_Languages,
  gnugettext;

{ Tf_TranslationDbNew }

class function Tf_TranslationDbNew.Execute(_Owner: TComponent; _Repository: TTranslationRepository;
  var _LngCode: string): boolean;
var
  frm: Tf_TranslationDbNew;
begin
  frm := Tf_TranslationDbNew.Create(_Owner);
  try
    frm.SetData(_Repository, _LngCode);
    Result := (mrOk = frm.ShowModal);
    if Result then
      frm.GetData(_LngCode);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_TranslationDbNew.Create(_Owner: TComponent);
begin
  inherited;

  TranslateComponent(Self);
  dxLanguages.GetLoacalizedLanguages(cmb_Language.Items);
end;

procedure Tf_TranslationDbNew.cmb_LanguageChange(Sender: TObject);
begin
  b_Ok.Enabled := (cmb_Language.ItemIndex <> -1);
end;

procedure Tf_TranslationDbNew.SetData(_Repository: TTranslationRepository; const _LngCode: string);
var
  Language: string;
  Idx: Integer;
  sl: TStringList;
  s: string;
  code: string;
begin
  sl := TStringList.Create;
  try
    _Repository.GetExistingLanguages(sl);
    dxLanguages.GetLanguages(cmb_Language.Items);
    for s in sl do begin
      code := dxLanguages.GetCodeForLanguage(s);
      idx := cmb_Language.Items.IndexOf(code);
      if Idx <> -1 then
        cmb_Language.Items.Delete(Idx);
    end;
  finally
    FreeAndNil(sl);
  end;
  if dxLanguages.TryGetLanguageForCode(_LngCode, Language) then begin
    Idx := cmb_Language.Items.IndexOf(Language);
    if Idx <> -1 then
      cmb_Language.ItemIndex := Idx
  end;
end;

procedure Tf_TranslationDbNew.GetData(var _LngCode: string);
var
  Idx: Integer;
  Language: string;
begin
  Idx := cmb_Language.ItemIndex;
  Language := cmb_Language.Items[Idx];
  _LngCode := dxLanguages.GetCodeForLanguage(Language);
end;

end.

