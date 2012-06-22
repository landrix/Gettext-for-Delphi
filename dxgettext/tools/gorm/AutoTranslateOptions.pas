unit AutoTranslateOptions;

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
  ExtCtrls;

type
  TAutoTranslateInclude = (atiUntranslated, atiFuzzy, atiTranslated);

type
  TFormAutoTranslateOptions = class(TForm)
    l_Method: TLabel;
    ed_Method: TEdit;
    rg_TranslateWhich: TRadioGroup;
    chk_MarkFuzzy: TCheckBox;
    chk_LabelAs: TCheckBox;
    ed_Label: TEdit;
    b_OK: TButton;
    b_Cancel: TButton;
    chk_Unfuzzy: TCheckBox;
    l_Blurb: TLabel;
    chk_MarkOnly: TCheckBox;
  private
    procedure SetData(const _Method: string; _Include: TAutoTranslateInclude; _MarkFuzzy, _UnFuzzy: boolean;
      const _Label: string; _LabelOnly: boolean);
    procedure GetData(out _Include: TAutoTranslateInclude; out _MarkFuzzy, _UnFuzzy: boolean;
      out _Label: string; out _LabelOnly: boolean);
  public
    class function Execute(_Owner: TComponent; const _Method: string; var _Include: TAutoTranslateInclude;
      var _MarkFuzzy: boolean; var _UnFuzzy: boolean; var _Label: string; var _LabelOnly: boolean): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GnuGetText;

{ TFormAutoTranslateOptions }

class function TFormAutoTranslateOptions.Execute(_Owner: TComponent; const _Method: string;
  var _Include: TAutoTranslateInclude; var _MarkFuzzy: boolean; var _UnFuzzy: boolean;
  var _Label: string; var _LabelOnly: boolean): boolean;
var
  frm: TFormAutoTranslateOptions;
begin
  frm := TFormAutoTranslateOptions.Create(_Owner);
  try
    frm.SetData(_Method, _Include, _MarkFuzzy, _UnFuzzy, _Label, _LabelOnly);
    Result := (frm.ShowModal = mrOK);
    if Result then
      frm.GetData(_Include, _MarkFuzzy, _UnFuzzy, _Label, _LabelOnly);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TFormAutoTranslateOptions.Create(_Owner: TComponent);
begin
  inherited;
  TranslateComponent(Self);
end;

procedure TFormAutoTranslateOptions.SetData(const _Method: string; _Include: TAutoTranslateInclude;
  _MarkFuzzy, _UnFuzzy: boolean; const _Label: string; _LabelOnly: boolean);
begin
  ed_Method.Text := _Method;
  case _Include of
    atiFuzzy: rg_TranslateWhich.ItemIndex := 1;
    atiTranslated: rg_TranslateWhich.ItemIndex := 2;
  else // atiUntranslated:
    rg_TranslateWhich.ItemIndex := 0;
  end;
  chk_MarkFuzzy.Checked := _MarkFuzzy;
  chk_Unfuzzy.Checked := _UnFuzzy;
  ed_Label.Text := _Label;
  chk_LabelAs.Checked := _Label <> '';
  chk_MarkOnly.Checked := _LabelOnly;
end;

procedure TFormAutoTranslateOptions.GetData(out _Include: TAutoTranslateInclude;
  out _MarkFuzzy, _UnFuzzy: boolean; out _Label: string; out _LabelOnly: boolean);
begin
  case rg_TranslateWhich.ItemIndex of
    1: _Include := atiFuzzy;
    2: _Include := atiTranslated;
  else
    _Include := atiUntranslated;
  end;
  _MarkFuzzy := chk_MarkFuzzy.Checked;
  _UnFuzzy := chk_Unfuzzy.Checked;
  _LabelOnly := chk_MarkOnly.Checked;
  if chk_LabelAs.Checked then begin
    _Label := ed_Label.Text;
    if _Label = '' then
      chk_LabelAs.Checked := False;
  end else
    _Label := '';
end;

end.

