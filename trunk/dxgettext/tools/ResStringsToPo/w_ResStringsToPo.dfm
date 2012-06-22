object f_ResStringsToPo: Tf_ResStringsToPo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Resourcestrings to PO'
  ClientHeight = 201
  ClientWidth = 440
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
  object l_SourceLng: TLabel
    Left = 8
    Top = 8
    Width = 232
    Height = 13
    Caption = 'PAS file with resource strings in source language'
  end
  object l_DestLanguage: TLabel
    Left = 8
    Top = 56
    Width = 253
    Height = 13
    Caption = 'PAS file with resource strings in destination language'
  end
  object l_PoFile: TLabel
    Left = 8
    Top = 112
    Width = 86
    Height = 13
    Caption = 'Append to PO File'
  end
  object ed_SourceLanguage: TEdit
    Left = 8
    Top = 24
    Width = 393
    Height = 21
    TabOrder = 0
  end
  object b_SourceLanguage: TButton
    Left = 408
    Top = 22
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = b_SourceLanguageClick
  end
  object ed_DestLanguage: TEdit
    Left = 8
    Top = 72
    Width = 393
    Height = 21
    TabOrder = 2
  end
  object b_DestLanguage: TButton
    Left = 408
    Top = 70
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 3
    OnClick = b_DestLanguageClick
  end
  object ed_PoFile: TEdit
    Left = 8
    Top = 128
    Width = 393
    Height = 21
    TabOrder = 4
  end
  object b_PoFile: TButton
    Left = 408
    Top = 126
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 5
    OnClick = b_PoFileClick
  end
  object b_Execute: TButton
    Left = 280
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Execute'
    Default = True
    TabOrder = 6
    OnClick = b_ExecuteClick
  end
  object b_Close: TButton
    Left = 360
    Top = 168
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 7
    OnClick = b_CloseClick
  end
  object od_Pas: TOpenDialog
    Filter = 'Pascal files (*.pas)|*.pas|All files (*.*)|*.*'
    Left = 352
    Top = 16
  end
  object sd_Po: TSaveDialog
    Filter = 'PO files (*.po)|*.po|All files (*.*)|*.*'
    Left = 360
    Top = 120
  end
end
