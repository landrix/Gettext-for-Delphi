object FormConfig: TFormConfig
  Left = 301
  Top = 198
  ActiveControl = EditBasepath
  BorderStyle = bsDialog
  Caption = 'Configuration'
  ClientHeight = 246
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    350
    246)
  PixelsPerInch = 96
  TextHeight = 13
  object EditMask: TLabeledEdit
    Left = 8
    Top = 88
    Width = 337
    Height = 21
    Hint = 'Here you can specify which files you want to scan.'
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Filemasks'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = '*.pas *.dfm *.c *.cpp *.inc *.rc *.dfm *.xfm *.dpr'
  end
  object CheckBoxRecurse: TCheckBox
    Left = 8
    Top = 48
    Width = 337
    Height = 17
    Caption = 'Also search subdirectories'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 96
    Top = 216
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 192
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object EditBasepath: TLabeledEdit
    Left = 8
    Top = 24
    Width = 337
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 70
    EditLabel.Height = 13
    EditLabel.Caption = 'Base directory:'
    TabOrder = 0
  end
  object CheckBoxSaveSettings: TCheckBox
    Left = 8
    Top = 192
    Width = 337
    Height = 17
    Hint = 
      'Save settings for the base directory in an ini file in this dire' +
      'ctory'
    Caption = 'Remember settings'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
  end
  object CBCreateIgnore: TCheckBox
    Left = 8
    Top = 120
    Width = 329
    Height = 17
    Hint = 
      'Create a new ignore.po file or add likely ignores to existing ig' +
      'nore.po file (default domain)'
    Caption = 'Add likely ignores to ignore.po file'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object CBRemoveIgnore: TCheckBox
    Left = 8
    Top = 144
    Width = 329
    Height = 17
    Hint = 
      'Don'#39't store items from default domain that are present in ignore' +
      '.po'
    Caption = 'Remove items from default domain present in ignore.po file'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
  object CheckBoxAllowNonAscii: TCheckBox
    Left = 8
    Top = 168
    Width = 334
    Height = 17
    Hint = 
      'Check this if you want your po file to contain msgid values that' +
      ' contain non-ascii characters.'
    Caption = 'Allow non-ASCII texts'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
  end
end
