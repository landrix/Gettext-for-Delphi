object FormLabelApplicationByRules: TFormLabelApplicationByRules
  Left = 0
  Top = 0
  ActiveControl = EditFilename
  BorderStyle = bsDialog
  Caption = 'Apply label by rules'
  ClientHeight = 217
  ClientWidth = 363
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
  object MemoExplanation: TMemo
    Left = 8
    Top = 8
    Width = 337
    Height = 121
    TabStop = False
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 88
    Top = 187
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
  end
  object ButtonCancel: TButton
    Left = 184
    Top = 187
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object EditFilename: TLabeledEdit
    Left = 16
    Top = 160
    Width = 307
    Height = 21
    EditLabel.Width = 49
    EditLabel.Height = 13
    EditLabel.Caption = 'File name:'
    TabOrder = 1
    OnChange = EditFilenameChange
  end
  object ButtonSelectFilename: TButton
    Left = 329
    Top = 158
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = ButtonSelectFilenameClick
  end
end
