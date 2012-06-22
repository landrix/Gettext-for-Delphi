object FormAutoTranslateOptions: TFormAutoTranslateOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Automatic Translation Options'
  ClientHeight = 369
  ClientWidth = 537
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
  object l_Method: TLabel
    Left = 8
    Top = 8
    Width = 142
    Height = 13
    Caption = 'Automatically translate using:'
  end
  object l_Blurb: TLabel
    Left = 8
    Top = 56
    Width = 518
    Height = 26
    Caption = 
      'This operation may take 30 seconds or longer, depending on the t' +
      'ranslation method used and possibly your internet connection.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ed_Method: TEdit
    Left = 8
    Top = 24
    Width = 521
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object rg_TranslateWhich: TRadioGroup
    Left = 8
    Top = 104
    Width = 521
    Height = 89
    Caption = 'Select which entries to translate'
    ItemIndex = 1
    Items.Strings = (
      'Only translate untranslated entries.'
      'Also translate entries currently marked as fuzzy.'
      'Also re-translate already translated entries.')
    TabOrder = 1
  end
  object chk_MarkFuzzy: TCheckBox
    Left = 8
    Top = 208
    Width = 521
    Height = 17
    Caption = 'Mark translated entries as fuzzy'
    TabOrder = 2
  end
  object chk_LabelAs: TCheckBox
    Left = 8
    Top = 256
    Width = 521
    Height = 17
    Caption = 'Label translated entries as'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object ed_Label: TEdit
    Left = 24
    Top = 272
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'auto'
  end
  object b_OK: TButton
    Left = 376
    Top = 336
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object b_Cancel: TButton
    Left = 456
    Top = 336
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object chk_Unfuzzy: TCheckBox
    Left = 8
    Top = 232
    Width = 521
    Height = 17
    Caption = 'Remove fuzzy from translated entries.'
    TabOrder = 3
  end
  object chk_MarkOnly: TCheckBox
    Left = 8
    Top = 304
    Width = 361
    Height = 17
    Caption = 'Do not change the translation, only label these entries'
    TabOrder = 6
  end
end
