unit GoogleTranslateSettings;

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
  TGoogleTranslationSettings = class(TForm)
    rg_Language: TRadioGroup;
    b_Ok: TButton;
    b_Cancel: TButton;
    edGoogleAPIKey: TLabeledEdit;
    lbGoogleAPIinfo: TLabel;
    lb_OKhint: TLabel;
    procedure lbGoogleAPIinfoMouseEnter(Sender: TObject);
    procedure lbGoogleAPIinfoMouseLeave(Sender: TObject);
    procedure lbGoogleAPIinfoClick(Sender: TObject);
    procedure b_OkClick(Sender: TObject);
    procedure edGoogleAPIKeyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetData(const _Code: string);
    procedure GetData(var _Code: string);
  public
    class function Execute(_Owner: TComponent; var _Code: string): boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GnuGetText,
  GoogleTranslate, AppSettings;

class function TGoogleTranslationSettings.Execute(_Owner: TComponent; var _Code: string): boolean;
var
  frm: TGoogleTranslationSettings;
begin
  frm := TGoogleTranslationSettings.Create(_Owner);
  try
    frm.SetData(_Code);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Code);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TGoogleTranslationSettings.FormCreate(Sender: TObject);
begin
  edGoogleAPIKeyChange(Sender);
end;

procedure TGoogleTranslationSettings.b_OkClick(Sender: TObject);
begin
  SetSetting('Google','GoogleAPIkey', edGoogleAPIKey.Text);
end;

constructor TGoogleTranslationSettings.Create(_Owner: TComponent);
var
  i: integer;
begin
  inherited;
  rg_Language.Items.Clear;
  for i := Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do
    rg_Language.Items.Add(_(TRANSLATION_ARR[i].Caption));
  edGoogleAPIKey.Text := GetSettingApplicationGoogleAPIkey;
end;

procedure TGoogleTranslationSettings.edGoogleAPIKeyChange(Sender: TObject);
begin
  // It will not be work without the Google API Key. So...
  if edGoogleAPIKey.Text = ''
    then begin
      b_OK.Enabled := false;
      lb_OKhint.Visible := True;
    end
    else begin
      b_OK.Enabled := true;
      lb_OKhint.Visible := false;
    end;
  // At the time of implement this function Google API can't verify the key.
  // When this feature will be realized it must be placed here
end;

procedure TGoogleTranslationSettings.GetData(var _Code: string);
begin
  if rg_Language.ItemIndex = -1 then begin
    _Code := '';
  end else begin
    _Code := TRANSLATION_ARR[rg_Language.ItemIndex].GTString;
  end;
end;

// It work like TLinkLabel in Vista, Win7

procedure TGoogleTranslationSettings.lbGoogleAPIinfoClick(Sender: TObject);
begin
  // Open default browser with Google starter guide
  ShellExecute(GetDesktopWindow(), 'open', 'https://developers.google.com/translate/v2/faq', '', '', SW_SHOWDEFAULT);
end;

procedure TGoogleTranslationSettings.lbGoogleAPIinfoMouseEnter(
  Sender: TObject);
begin
  lbGoogleAPIinfo.Font.Color := clHotlight;
end;

procedure TGoogleTranslationSettings.lbGoogleAPIinfoMouseLeave(
  Sender: TObject);
begin
  lbGoogleAPIinfo.Font.Color := clHighlight;
end;

procedure TGoogleTranslationSettings.SetData(const _Code: string);
var
  i: Integer;
begin
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
    if (TRANSLATION_ARR[i].GTString =  MidStr( _Code, 1, 2)) then
    begin
      rg_Language.ItemIndex := i;

      exit;
    end;
  end;
end;

end.

