object f_Textfilter: Tf_Textfilter
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Textfilter'
  ClientHeight = 209
  ClientWidth = 225
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
  object l_LookIn: TLabel
    Left = 8
    Top = 8
    Width = 95
    Height = 13
    Caption = 'Look for string in ...'
  end
  object chk_MsgId: TCheckBox
    Left = 8
    Top = 24
    Width = 209
    Height = 17
    Caption = 'MSGID'
    TabOrder = 0
  end
  object chk_MsgStr: TCheckBox
    Left = 8
    Top = 48
    Width = 209
    Height = 17
    Caption = 'MSGSTR'
    TabOrder = 1
  end
  object chk_Comments: TCheckBox
    Left = 8
    Top = 72
    Width = 209
    Height = 17
    Caption = 'Comments and instructions'
    TabOrder = 2
  end
  object chk_Source: TCheckBox
    Left = 8
    Top = 96
    Width = 209
    Height = 17
    Caption = 'Source code location'
    TabOrder = 3
  end
  object b_Ok: TButton
    Left = 64
    Top = 176
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object b_Cancel: TButton
    Left = 144
    Top = 176
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object chk_Inverted: TCheckBox
    Left = 8
    Top = 136
    Width = 209
    Height = 17
    Caption = 'Inverted'
    TabOrder = 4
  end
end
