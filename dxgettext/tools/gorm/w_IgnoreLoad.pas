unit w_IgnoreLoad;

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
  ExtCtrls,
  ImgList,
  u_dzVclUtils,
  System.UITypes, System.ImageList;

type
  TButtonedEdit = class(TdzButtonedEdit)
  end;

type
  Tf_IgnoreLoad = class(TForm)
    ilOpenFile: TImageList;
    ed_Filename: TButtonedEdit;
    l_IgnoreFile: TLabel;
    chk_ClearFirst: TCheckBox;
    chk_LabelAs: TCheckBox;
    ed_Label: TEdit;
    chk_AutoFilter: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
    l_Blurb: TLabel;
    procedure ed_FilenameChange(Sender: TObject);
    procedure chk_LabelAsClick(Sender: TObject);
    procedure ed_LabelChange(Sender: TObject);
    procedure ed_FilenameRightButtonClick(Sender: TObject);
    procedure chk_ClearFirstClick(Sender: TObject);
  private
    procedure SetData(const _Filename: string; _ClearFirst: boolean; const _LabelToAdd: string;
      _AutoFilter: boolean);
    procedure GetData(out _Filename: string; out _ClearFirst: boolean; out _LabelToAdd: string;
      out _AutoFilter: boolean);
    procedure CheckInput;
  public
    class function Execute(_Owner: TWinControl; var _Filename: string;
      var _ClearFirst: boolean; var _LabelToAdd: string; var _AutoFilter: boolean): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  gnugettext;

{ Tf_IgnoreLoad }

class function Tf_IgnoreLoad.Execute(_Owner: TWinControl; var _Filename: string;
  var _ClearFirst: boolean; var _LabelToAdd: string; var _AutoFilter: boolean): boolean;
var
  frm: Tf_IgnoreLoad;
begin
  frm := Tf_IgnoreLoad.Create(_Owner);
  try
    frm.SetData(_Filename, _ClearFirst, _LabelToAdd, _AutoFilter);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Filename, _ClearFirst, _LabelToAdd, _AutoFilter);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_IgnoreLoad.Create(_Owner: TComponent);
begin
  inherited;

  TranslateComponent(Self);
end;

procedure Tf_IgnoreLoad.GetData(out _Filename: string; out _ClearFirst: boolean;
  out _LabelToAdd: string; out _AutoFilter: boolean);
begin
  _Filename := ed_Filename.Text;
  _ClearFirst := chk_ClearFirst.Checked;
  if chk_LabelAs.Checked then begin
    _LabelToAdd := ed_Label.Text;
    _AutoFilter := chk_AutoFilter.Checked;
  end else begin
    _LabelToAdd := '';
    _AutoFilter := False;
  end;
end;

procedure Tf_IgnoreLoad.SetData(const _Filename: string; _ClearFirst: boolean;
  const _LabelToAdd: string; _AutoFilter: boolean);
begin
  ed_Filename.Text := _Filename;
  chk_ClearFirst.Checked := _ClearFirst;
  chk_LabelAs.Checked := (_LabelToAdd <> '');
  ed_Label.Text := _LabelToAdd;
  chk_AutoFilter.Checked := _AutoFilter;
end;

procedure Tf_IgnoreLoad.CheckInput;
var
  b: Boolean;
begin
  b := ed_Filename.Text <> '';
  b := b and FileExists(ed_Filename.Text);
  b := b and ((not chk_LabelAs.Checked) or (ed_Label.Text <> ''));
  b_Ok.Enabled := b;
end;

procedure Tf_IgnoreLoad.chk_ClearFirstClick(Sender: TObject);
begin
  if chk_ClearFirst.Checked then
    if mrYes <> MessageDlg(_('Are you sure you want to clear the ignore flag for all currently ignored entries?'),
      mtConfirmation, mbYesNoCancel, 0, mbYes) then
      chk_ClearFirst.Checked := False;
end;

procedure Tf_IgnoreLoad.chk_LabelAsClick(Sender: TObject);
begin
  if chk_LabelAs.Checked then begin
    ed_Label.Enabled := true;
    chk_AutoFilter.Enabled := true;
  end else begin
    ed_Label.Enabled := false;
    chk_AutoFilter.Enabled := false;
  end;
  CheckInput;
end;

procedure Tf_IgnoreLoad.ed_FilenameChange(Sender: TObject);
begin
  CheckInput;
end;

procedure Tf_IgnoreLoad.ed_FilenameRightButtonClick(Sender: TObject);
var
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(self);
  try
    od.FileName := ed_Filename.Text;
    od.Filter := _('Ignore.po files') + ' (ignore.po)|ignore.po'
      + '|' + _('PO files') + ' (*.po)|*.po'
      + '|' + _('All files') + ' (*.*)|*.*';
    if od.Execute then
      ed_Filename.Text := od.FileName;
  finally
    FreeAndNil(od);
  end;
end;

procedure Tf_IgnoreLoad.ed_LabelChange(Sender: TObject);
begin
  CheckInput;
end;

end.

