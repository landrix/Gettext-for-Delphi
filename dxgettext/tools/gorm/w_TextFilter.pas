unit w_TextFilter;

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
  StdCtrls;

type
  TSearchWhereEnum = (swMsgId, swMsgStr, swComments, swSource);
  TSearchWhereSet = set of TSearchWhereEnum;
type
  TSearchWhereEnumNameArr = array[TSearchWhereEnum] of string;
const
  SEARCH_WHERE: TSearchWhereEnumNameArr = (
    'MSGID', 'MSGSTR', 'Comments', 'Source');

type
  Tf_Textfilter = class(TForm)
    chk_MsgId: TCheckBox;
    l_LookIn: TLabel;
    chk_MsgStr: TCheckBox;
    chk_Comments: TCheckBox;
    chk_Source: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
    chk_Inverted: TCheckBox;
  private
    procedure SetData(const _SearchWhere: TSearchWhereSet; _Inverted: boolean);
    procedure GetData(var _SearchWhere: TSearchWhereSet; var _Inverted: boolean);
  public
    class function Execute(_Owner: TComponent; var _SearchWhere: TSearchWhereSet;
      var _Inverted: boolean): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  gnugettext;

{ Tf_Textfilter }

class function Tf_Textfilter.Execute(_Owner: TComponent; var _SearchWhere: TSearchWhereSet;
  var _Inverted: boolean): boolean;
var
  frm: Tf_Textfilter;
begin
  frm := Tf_Textfilter.Create(_Owner);
  try
    frm.SetData(_SearchWhere, _Inverted);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_SearchWhere, _Inverted);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_Textfilter.Create(_Owner: TComponent);
begin
  inherited;
  TranslateComponent(Self);
end;

procedure Tf_Textfilter.SetData(const _SearchWhere: TSearchWhereSet; _Inverted: boolean);
begin
  chk_MsgId.Checked := (swMsgId in _SearchWhere);
  chk_MsgStr.Checked := (swMsgStr in _SearchWhere);
  chk_Comments.Checked := (swComments in _SearchWhere);
  chk_Source.Checked := (swSource in _SearchWhere);
  chk_Inverted.Checked := _Inverted;
end;

procedure Tf_Textfilter.GetData(var _SearchWhere: TSearchWhereSet; var _Inverted: boolean);
begin
  _SearchWhere := [];

  if chk_MsgId.Checked then
    Include(_SearchWhere, swMsgId);
  if chk_MsgStr.Checked then
    Include(_SearchWhere, swMsgStr);
  if chk_Comments.Checked then
    Include(_SearchWhere, swComments);
  if chk_Source.Checked then
    Include(_SearchWhere, swSource);

  if _SearchWhere = [] then
    _SearchWhere := [swMsgId, swMsgStr];

  _Inverted := chk_Inverted.Checked;
end;

end.

