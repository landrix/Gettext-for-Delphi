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
  ImgList,
  ExtCtrls,
  Forms,
  Dialogs,
  StdCtrls,
  poparser,
  u_dzVclUtils;

type
  TButtonedEdit = class(TdzButtonedEdit)
  end;

  type
  Tf_EditHeader = class(TForm)
    l_ProjectName: TLabel;
    ed_Project: TEdit;
    l_Team: TLabel;
    ed_Team: TEdit;
    l_TeamEmail: TLabel;
    ed_TeamEmail: TEdit;
    l_Language: TLabel;
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
    l_BasePath: TLabel;
    ed_BasePath: TButtonedEdit;
    ilOpenFile: TImageList;
    e_Language: TEdit;
    procedure cmb_LanguageChange(Sender: TObject);
    procedure ed_BasePathRightButtonClick(Sender: TObject);
  private
    FForceLanguageInput: Boolean;
    procedure SetData(_Item: TPoEntry; _ForceLanguageInput: boolean);
    procedure GetData(_Item: TPoEntry);
    procedure CheckInput;
  public
    class function Execute(_Owner: TComponent; _Item: TPoEntry;
      _ForceLanguageInput: boolean): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  gnugettext,
  u_Languages;

{ Tf_EditHeader }

class function Tf_EditHeader.Execute(_Owner: TComponent; _Item: TPoEntry;
  _ForceLanguageInput: boolean): boolean;
var
  frm: Tf_EditHeader;
begin
  frm := Tf_EditHeader.Create(_Owner);
  try
    frm.SetData(_Item, _ForceLanguageInput);
    frm.CheckInput;
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Item);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_EditHeader.cmb_LanguageChange(Sender: TObject);
begin
  CheckInput;
end;

constructor Tf_EditHeader.Create(_Owner: TComponent);
begin
  inherited;

  TranslateComponent(Self);
end;

procedure Tf_EditHeader.ed_BasePathRightButtonClick(Sender: TObject);
var
  dir: string;
begin
  dir := ed_BasePath.Text;
  if dzSelectDirectory(dir, Self) then
    ed_BasePath.Text := dir;
end;

procedure Tf_EditHeader.CheckInput;
var
 OK: boolean;
begin
  OK := (e_Language.Text <> '');

  if FForceLanguageInput then
  begin
    if OK then
    begin
      e_Language.Color := clWindow;
    end
    else
    begin
      e_Language.Color := clYellow;
    end;

    b_Ok.Enabled := OK;
  end;
end;

procedure Tf_EditHeader.GetData(_Item: TPoEntry);
var
  Header: string;
  lLanguageText: String;
begin
  _Item.UserCommentList.Assign(m_Comments.Lines);
  Header := _Item.MsgStr;
  SetPoHeaderEntry(Header, PO_HEADER_PROJECT_ID_VERSION, ed_Project.Text);
  SetPoHeaderEntry(Header, PO_HEADER_LANGUAGE_TEAM, ed_Team.Text + ' <' + ed_TeamEmail.Text + '>');
  SetPoHeaderEntry(Header, PO_HEADER_LAST_TRANSLATOR, ed_LastTranslator.Text + ' <' + ed_LastEmail.Text + '>');

  //*** only change the old entry if it was set before
  if (GetPoHeaderEntry( Header, PO_HEADER_POEDIT_LANGUAGE) <> '') then
  begin
    lLanguageText := '';
    if not DxLanguages.TryGetLanguageForCode( e_Language.Text,
                                              lLanguageText) then
    begin
      lLanguageText := '';
    end;

    SetPoHeaderEntry( Header,
                      PO_HEADER_POEDIT_LANGUAGE,
                      lLanguageText);
  end;

  SetPoHeaderEntry( Header,
                    PO_HEADER_LANGUAGE_new,
                    e_Language.Text);

  SetPoHeaderEntry(Header, PO_HEADER_CONTENT_TYPE, 'text/plain; charset=' + cmb_Charset.Text);
  SetPoHeaderEntry(Header, PO_HEADER_Poedit_BasePath, ed_BasePath.Text);

  _Item.MsgStr := Header;
end;

procedure Tf_EditHeader.SetData(_Item: TPoEntry; _ForceLanguageInput: boolean);

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
  lLanguageParam, lLanguageCode: String;
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

  lLanguageParam := GetPoHeaderEntry( Header,
                                      PO_HEADER_POEDIT_LANGUAGE);
  if (lLanguageParam <> '') then
  begin
    lLanguageCode := '';
    if DxLanguages.TryGetCodeForLanguage( lLanguageParam,
                                          lLanguageCode) and
       (lLanguageCode <> '') then
    begin
      lLanguageParam := lLanguageCode;
    end;
  end
  else
  begin
    lLanguageParam := GetPoHeaderEntry( Header,
                                        PO_HEADER_LANGUAGE_new);
  end;
  e_Language.Text := lLanguageParam;

  Value := GetPoHeaderEntry(Header, PO_HEADER_CONTENT_TYPE);
  SplitAtSemicolon(Value, First, Second);
  if StartsText('charset=', Second) then
    cmb_Charset.Text := Copy(Second, 9)
  else
    cmb_Charset.Text := Second;

  ed_BasePath.Text := GetPoHeaderEntry(Header, PO_HEADER_Poedit_BasePath);

  FForceLanguageInput := _ForceLanguageInput;
end;

end.

