unit w_IgnoreSave;

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
  ImgList,
  ExtCtrls,
  u_dzVclUtils;

type
  TButtonedEdit = class(TdzButtonedEdit)
  end;

type
  Tf_IgnoreSave = class(TForm)
    l_IgnoreFile: TLabel;
    ed_Filename: TButtonedEdit;
    ilOpenFile: TImageList;
    l_Blurb: TLabel;
    b_Ok: TButton;
    b_Cancel: TButton;
    procedure ed_FilenameChange(Sender: TObject);
    procedure ed_FilenameRightButtonClick(Sender: TObject);
  private
    procedure SetData(const _Filename: string);
    procedure GetData(out _Filename: string);
  public
    class function Execute(_Owner: TWinControl; var _Filename: string): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  gnugettext;

{ Tf_IgnoreSave }

class function Tf_IgnoreSave.Execute(_Owner: TWinControl; var _Filename: string): boolean;
var
  frm: Tf_IgnoreSave;
begin
  frm := Tf_IgnoreSave.Create(_Owner);
  try
    frm.SetData(_Filename);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Filename);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_IgnoreSave.Create(_Owner: TComponent);
begin
  inherited;

  TranslateComponent(Self);
end;

procedure Tf_IgnoreSave.GetData(out _Filename: string);
begin
  _Filename := ed_Filename.Text;
end;

procedure Tf_IgnoreSave.SetData(const _Filename: string);
begin
  ed_Filename.Text := _Filename;
end;

procedure Tf_IgnoreSave.ed_FilenameChange(Sender: TObject);
begin
  b_Ok.Enabled := (ed_Filename.Text <> '');
end;

procedure Tf_IgnoreSave.ed_FilenameRightButtonClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(Self);
  try
    sd.FileName := ed_Filename.Text;
    sd.Filter := _('Ignore.po files') + ' (ignore.po)|ignore.po'
      + '|' + _('PO files') + ' (*.po)|*.po'
      + '|' + _('All files') + ' (*.*)|*.*';
    sd.Options := sd.Options + [ofOverwritePrompt, ofPathMustExist];
    if sd.Execute then
      ed_Filename.Text := sd.FileName;
  finally
    FreeAndNil(sd);
  end;
end;

end.

