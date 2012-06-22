object f_ConvertTranslateDb: Tf_ConvertTranslateDb
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Convert TranslateDb'
  ClientHeight = 121
  ClientWidth = 449
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
  object ed_InputFile: TEdit
    Left = 8
    Top = 8
    Width = 401
    Height = 21
    TabOrder = 0
    Text = 
      'C:\src\berlios\dxgettext\dxgettext\output\debug\gorm\OriginalTra' +
      'nslateDB_de.mdb'
  end
  object b_InputFile: TButton
    Left = 416
    Top = 6
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = b_InputFileClick
  end
  object ed_OutputFile: TEdit
    Left = 8
    Top = 48
    Width = 401
    Height = 21
    TabOrder = 2
    Text = 
      'C:\src\berlios\dxgettext\dxgettext\output\debug\gorm\ChangedTran' +
      'slateDB_de.mdb'
  end
  object b_OutputFile: TButton
    Left = 416
    Top = 46
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 3
    OnClick = b_OutputFileClick
  end
  object b_Convert: TButton
    Left = 280
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 4
    OnClick = b_ConvertClick
  end
  object b_Exit: TButton
    Left = 368
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 5
    OnClick = b_ExitClick
  end
  object od_MdbFile: TOpenDialog
    Filter = 'Access DB (*.MDB)|*.MDB|all files (*.*)|*.*'
    Left = 136
    Top = 16
  end
end
