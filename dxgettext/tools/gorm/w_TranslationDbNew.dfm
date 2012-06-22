object f_TranslationDbNew: Tf_TranslationDbNew
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New Translation DB'
  ClientHeight = 89
  ClientWidth = 168
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object l_Language: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Language'
  end
  object cmb_Language: TComboBox
    Left = 8
    Top = 24
    Width = 153
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cmb_LanguageChange
  end
  object b_Ok: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 88
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
