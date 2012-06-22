unit w_TranslationDbLearnOptions;

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
  u_TranslationDBAccess,
  StdCtrls;

type
  Tf_TranslationDbLearnOptions = class(TForm)
    l_Filename: TLabel;
    ed_Filename: TEdit;
    b_Ok: TButton;
    b_Cancel: TButton;
    chk_AddTagToRepository: TCheckBox;
    ed_RepositoryTag: TEdit;
    ed_PoTag: TEdit;
    chk_Preview: TCheckBox;
    procedure chk_AddTagToRepositoryClick(Sender: TObject);
  private
    procedure SetData(_dm: TTranslationDbAccess; _Preview: boolean;
      const _RepoTag: string; const _PoFileTag: string);
    procedure GetData(out _Preview: boolean; out _RepoTag: string;
      out _PoFileTag: string);
  public
    class function Execute(_Owner: TWinControl; _dm: TTranslationDbAccess;
      var _Preview: boolean; var _RepoTag: string; var _PoFileTag: string): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  gnugettext;

{ Tf_TranslationDbLearnOptions }

class function Tf_TranslationDbLearnOptions.Execute(_Owner: TWinControl; _dm: TTranslationDbAccess;
  var _Preview: boolean; var _RepoTag: string; var _PoFileTag: string): boolean;
var
  frm: Tf_TranslationDbLearnOptions;
begin
  frm := Tf_TranslationDbLearnOptions.Create(_Owner);
  try
    frm.SetData(_dm, _Preview, _RepoTag, _PoFileTag);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Preview, _RepoTag, _PoFileTag);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_TranslationDbLearnOptions.Create(_Owner: TComponent);
begin
  inherited;

  TranslateComponent(Self);

  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure Tf_TranslationDbLearnOptions.SetData(_dm: TTranslationDbAccess;
  _Preview: boolean; const _RepoTag: string; const _PoFileTag: string);
begin
  ed_Filename.Text := _dm.GetMdbFilename;
  chk_Preview.Checked := _Preview;
  ed_RepositoryTag.Text := _RepoTag;
  ed_PoTag.Text := _PoFileTag;
end;

procedure Tf_TranslationDbLearnOptions.GetData(out _Preview: boolean;
  out _RepoTag: string; out _PoFileTag: string);
begin
  _Preview := chk_Preview.Checked;
  if chk_AddTagToRepository.Checked then
    _RepoTag := ed_RepositoryTag.Text
  else
    _RepoTag := '';
  _PoFileTag := ed_PoTag.Text
end;

procedure Tf_TranslationDbLearnOptions.chk_AddTagToRepositoryClick(Sender: TObject);
begin
  ed_RepositoryTag.Enabled := chk_AddTagToRepository.Checked;
end;

end.

