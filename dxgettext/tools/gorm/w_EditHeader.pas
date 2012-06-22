unit w_EditHeader;

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
  poparser;

type
  Tf_EditHeader = class(TForm)
    l_ProjectName: TLabel;
    ed_Project: TEdit;
    l_Team: TLabel;
    ed_Team: TEdit;
    l_TeamEmail: TLabel;
    ed_TeamEmail: TEdit;
    l_Language: TLabel;
    cmb_Language: TComboBox;
    l_Charset: TLabel;
    cmb_Charset: TComboBox;
    b_Ok: TButton;
    b_Cancel: TButton;
    l_Comments: TLabel;
    m_Comments: TMemo;
    l_LastTranslator: TLabel;
    l_LastEmail: TLabel;
    ed_LastTranslator: TEdit;
    ed_LastEmail: TEdit;
  private
    procedure SetData(_Item: TPoEntry);
    procedure GetData(_Item: TPoEntry);
  public
    class function Execute(_Owner: TComponent; _Item: TPoEntry): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  gnugettext,
  u_Languages;

{ Tf_EditHeader }

class function Tf_EditHeader.Execute(_Owner: TComponent; _Item: TPoEntry): boolean;
var
  frm: Tf_EditHeader;
begin
  frm := Tf_EditHeader.Create(_Owner);
  try
    frm.SetData(_Item);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Item);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_EditHeader.Create(_Owner: TComponent);
begin
  inherited;

  TranslateComponent(Self);

  dxLanguages.GetLanguages(cmb_Language.Items);
end;

procedure Tf_EditHeader.GetData(_Item: TPoEntry);
var
  Header: string;
begin
  _Item.UserCommentList.Assign(m_Comments.Lines);
  Header := _Item.MsgStr;
  SetPoHeaderEntry(Header, PO_HEADER_PROJECT_ID_VERSION, ed_Project.Text);
  SetPoHeaderEntry(Header, PO_HEADER_LANGUAGE_TEAM, ed_Team.Text + ' <' + ed_TeamEmail.Text + '>');
  SetPoHeaderEntry(Header, PO_HEADER_LAST_TRANSLATOR, ed_LastTranslator.Text + ' <' + ed_LastEmail.Text + '>');
  SetPoHeaderEntry(Header, PO_HEADER_LANGUAGE, cmb_Language.Text);
  SetPoHeaderEntry(Header, PO_HEADER_CONTENT_TYPE, 'text/plain; charset=' + cmb_Charset.Text);
  _Item.MsgStr := Header;
end;

procedure Tf_EditHeader.SetData(_Item: TPoEntry);

  function DequoteStr(const _s: string; _StartQuote: char = ''''; _EndQuote: char = ''''): string;
  var
    Len: integer;
  begin
    Len := Length(_s);
    if (Len >= 2) and (_s[1] = _StartQuote) and (_s[Len] = _EndQuote) then
      Result := Copy(_s, 2, Len - 2)
    else
      Result := _s;
  end;

  procedure SplitAtSemicolon(const _In: string; out _First, _Second: string);
  var
    p: Integer;
  begin
    p := Pos(';', _In);
    if p = 0 then begin
      _First := Trim(_In);
      _Second := '';
    end else begin
      _First := Trim(LeftStr(_In, p - 1));
      _Second := Trim(Copy(_in, p + 1));
    end;
  end;

  procedure SplitNameAndMail(const _In: string; out _Name, _Mail: string);
  var
    s: string;
    p: integer;
  begin
    s := Trim(_In);
    p := Length(s);
    while p > 0 do begin
      if s[p] = ' ' then begin
        _Name := Trim(LeftStr(s, p - 1));
        _Mail := Trim(Copy(s, p + 1));
        exit;
      end;
      Dec(p);
    end;
    _Name := '';
    _Mail := s;
  end;

var
  Value: string;
  Name: string;
  Mail: string;
  First: string;
  Second: string;
  Header: string;
begin
  m_Comments.Lines.Assign(_Item.UserCommentList);
  Header := _Item.MsgStr;

  ed_Project.Text := GetPoHeaderEntry(Header, PO_HEADER_PROJECT_ID_VERSION);

  Value := GetPoHeaderEntry(Header, PO_HEADER_LANGUAGE_TEAM);
  SplitNameAndMail(Value, Name, Mail);
  ed_Team.Text := Name;
  ed_TeamEmail.Text := DequoteStr(Mail, '<', '>');

  Value := GetPoHeaderEntry(Header, PO_HEADER_LAST_TRANSLATOR);
  SplitNameAndMail(Value, Name, Mail);
  ed_LastTranslator.Text := Name;
  ed_LastEmail.Text := DequoteStr(Mail, '<', '>');

  cmb_Language.Text := GetPoHeaderEntry(Header, PO_HEADER_LANGUAGE);

  Value := GetPoHeaderEntry(Header, PO_HEADER_CONTENT_TYPE);
  SplitAtSemicolon(Value, First, Second);
  if StartsText('charset=', Second) then
    cmb_Charset.Text := Copy(Second, 9)
  else
    cmb_Charset.Text := Second;
end;

end.

