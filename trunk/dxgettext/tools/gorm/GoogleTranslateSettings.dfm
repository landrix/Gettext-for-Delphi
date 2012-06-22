object GoogleTranslationSettings: TGoogleTranslationSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Google Translation Settings (Powered by Google)'
  ClientHeight = 304
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbGoogleAPIinfo: TLabel
    Left = 208
    Top = 256
    Width = 59
    Height = 13
    Cursor = crHandPoint
    Hint = 
      'The Google Translate API requires the use of an API key, which y' +
      'ou can get from the Google APIs console.'
    Caption = 'what is this?'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = lbGoogleAPIinfoClick
    OnMouseEnter = lbGoogleAPIinfoMouseEnter
    OnMouseLeave = lbGoogleAPIinfoMouseLeave
  end
  object lb_OKhint: TLabel
    Left = 320
    Top = 245
    Width = 187
    Height = 13
    Caption = 'You need a Google API key to enable it'
    Visible = False
  end
  object rg_Language: TRadioGroup
    Left = 8
    Top = 8
    Width = 513
    Height = 225
    Caption = 'Language'
    Columns = 3
    Items.Strings = (
      '&Chinese simplified'
      '&Danish'
      'Dutc&h'
      '&Finnish'
      'French'
      '&German'
      '&Indonesian'
      'It&alian'
      '&Japanese'
      '&Korean'
      '&Norwegian'
      '&Portuguese'
      '&Russian'
      'Spanish'
      '&Swedish'
      '&Thai'
      '&Ukrainian')
    TabOrder = 0
  end
  object b_Ok: TButton
    Left = 365
    Top = 264
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = b_OkClick
  end
  object b_Cancel: TButton
    Left = 446
    Top = 264
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edGoogleAPIKey: TLabeledEdit
    Left = 8
    Top = 269
    Width = 265
    Height = 21
    EditLabel.Width = 74
    EditLabel.Height = 13
    EditLabel.Caption = 'Google API Key'
    TabOrder = 3
    OnChange = edGoogleAPIKeyChange
  end
end
