object FormLabel: TFormLabel
  Left = 0
  Top = 0
  Caption = 'Label'
  ClientHeight = 93
  ClientWidth = 196
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
  object EditLabelName: TLabeledEdit
    Left = 8
    Top = 32
    Width = 177
    Height = 21
    EditLabel.Width = 142
    EditLabel.Height = 13
    EditLabel.Caption = 'Label name (ascii letters only)'
    TabOrder = 0
    OnChange = EditLabelNameChange
  end
  object ButtonOK: TButton
    Left = 24
    Top = 59
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 105
    Top = 59
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
