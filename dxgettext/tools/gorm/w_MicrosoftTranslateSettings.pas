unit w_MicrosoftTranslateSettings;

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
  Menus,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ShellAPI,
  StrUtils;

type
  Tf_MicrosoftTranslationSettings = class(TForm)
    rg_Language: TRadioGroup;
    b_Ok: TButton;
    b_Cancel: TButton;
    ed_BingAppId: TLabeledEdit;
    l_BingAppIdInfo: TLabel;
    lb_OKhint: TLabel;
    procedure l_BingAppIdInfoMouseEnter(Sender: TObject);
    procedure l_BingAppIdInfoMouseLeave(Sender: TObject);
    procedure l_BingAppIdInfoClick(Sender: TObject);
    procedure b_OkClick(Sender: TObject);
    procedure ed_BingAppIdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetData(const _AppId, _Code: string);
    procedure GetData(var _AppId, _Code: string);
  public
    class function Execute(_Owner: TComponent; var _AppId, _Code: string): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GnuGetText,
  GoogleTranslate,
  AppSettings;

class function Tf_MicrosoftTranslationSettings.Execute(_Owner: TComponent; var _AppId, _Code: string): boolean;
var
  frm: Tf_MicrosoftTranslationSettings;
begin
  frm := Tf_MicrosoftTranslationSettings.Create(_Owner);
  try
    frm.SetData(_AppId, _Code);

    Result := (frm.ShowModal = mrOk);
    if Result then
    begin
      frm.GetData(_AppId, _Code);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_MicrosoftTranslationSettings.FormCreate(Sender: TObject);
begin
  ed_BingAppIdChange(Sender);
end;

procedure Tf_MicrosoftTranslationSettings.b_OkClick(Sender: TObject);
begin
  SetSetting('Microsoft', 'BingAppId', ed_BingAppId.Text);
end;

constructor Tf_MicrosoftTranslationSettings.Create(_Owner: TComponent);
var
  i: integer;
begin
  inherited;
  rg_Language.Items.Clear;
  for i := Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do
    rg_Language.Items.Add(_(TRANSLATION_ARR[i].Caption));
  ed_BingAppId.Text := GetSettingApplicationBingAppId;
end;

procedure Tf_MicrosoftTranslationSettings.ed_BingAppIdChange(Sender: TObject);
begin
  // It will not be work without the Google API Key. So...
  if ed_BingAppId.Text = '' then begin
    b_OK.Enabled := false;
    lb_OKhint.Visible := True;
  end else begin
    b_OK.Enabled := true;
    lb_OKhint.Visible := false;
  end;
  // At the time of implement this function Google API can't verify the key.
  // When this feature will be realized it must be placed here
end;

procedure Tf_MicrosoftTranslationSettings.GetData(var _AppId, _Code: string);
begin
  _AppId := ed_BingAppId.Text;

  if rg_Language.ItemIndex = -1 then
  begin
    _Code := '';
  end
  else
  begin
    _Code := TRANSLATION_ARR[rg_Language.ItemIndex].GTString;
  end;
end;

// It work like TLinkLabel in Vista, Win7

procedure Tf_MicrosoftTranslationSettings.l_BingAppIdInfoClick(Sender: TObject);
begin
  // Open default browser with Microsoft Translator V2 api description
  ShellExecute(GetDesktopWindow(), 'open', 'http://msdn.microsoft.com/en-us/library/ff512423.aspx', '', '', SW_SHOWDEFAULT);
end;

procedure Tf_MicrosoftTranslationSettings.l_BingAppIdInfoMouseEnter(Sender: TObject);
begin
  l_BingAppIdInfo.Font.Color := clHotlight;
end;

procedure Tf_MicrosoftTranslationSettings.l_BingAppIdInfoMouseLeave(Sender: TObject);
begin
  l_BingAppIdInfo.Font.Color := clHighlight;
end;

procedure Tf_MicrosoftTranslationSettings.SetData(const _AppId, _Code: string);
var
  i: Integer;
begin
  ed_BingAppId.text := _AppId;

  for i := Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do
  begin
    if TRANSLATION_ARR[i].GTString = _Code then
    begin
      rg_Language.ItemIndex := i;
      exit;
    end;
  end;

  //*** Test only the first 2 chars
  for i := Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do
  begin
    if TRANSLATION_ARR[i].GTString = MidStr( _Code, 1, 2) then
    begin
      rg_Language.ItemIndex := i;
      exit;
    end;
  end;
end;

end.

