object f_EditHeader: Tf_EditHeader
  Left = 335
  Top = 202
  BorderStyle = bsDialog
  Caption = 'Edit Header'
  ClientHeight = 401
  ClientWidth = 401
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
  object l_ProjectName: TLabel
    Left = 8
    Top = 112
    Width = 122
    Height = 13
    Caption = 'Project name and version'
  end
  object l_Team: TLabel
    Left = 8
    Top = 160
    Width = 74
    Height = 13
    Caption = 'Language team'
  end
  object l_TeamEmail: TLabel
    Left = 168
    Top = 160
    Width = 101
    Height = 13
    Caption = 'Team'#39's email address'
  end
  object l_Language: TLabel
    Left = 8
    Top = 272
    Width = 47
    Height = 13
    Caption = 'Language'
  end
  object l_Charset: TLabel
    Left = 168
    Top = 256
    Width = 217
    Height = 26
    Caption = 'Charset (use UTF-8 unless you know exactly what you are doing!)'
    WordWrap = True
  end
  object l_Comments: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = 'Comments'
  end
  object l_LastTranslator: TLabel
    Left = 8
    Top = 208
    Width = 70
    Height = 13
    Caption = 'Last translator'
  end
  object l_LastEmail: TLabel
    Left = 168
    Top = 208
    Width = 145
    Height = 13
    Caption = 'Last translator'#39's email address'
  end
  object l_BasePath: TLabel
    Left = 8
    Top = 320
    Width = 48
    Height = 13
    Caption = 'Base path'
  end
  object ed_Project: TEdit
    Left = 8
    Top = 128
    Width = 385
    Height = 21
    TabOrder = 1
  end
  object ed_Team: TEdit
    Left = 8
    Top = 176
    Width = 153
    Height = 21
    TabOrder = 2
  end
  object ed_TeamEmail: TEdit
    Left = 168
    Top = 176
    Width = 225
    Height = 21
    TabOrder = 3
  end
  object cmb_Language: TComboBox
    Left = 8
    Top = 288
    Width = 153
    Height = 21
    TabOrder = 6
    OnChange = cmb_LanguageChange
  end
  object cmb_Charset: TComboBox
    Left = 168
    Top = 288
    Width = 161
    Height = 21
    TabOrder = 7
    Items.Strings = (
      'ASCII'
      'ISO-8859-1'
      'ISO-8859-2'
      'ISO-8859-3'
      'ISO-8859-4'
      'ISO-8859-5'
      'ISO-8859-6'
      'ISO-8859-7'
      'ISO-8859-8'
      'ISO-8859-9'
      'ISO-8859-13'
      'ISO-8859-14'
      'ISO-8859-15'
      'KOI8-R'
      'KOI8-U'
      'KOI8-T'
      'CP850'
      'CP866'
      'CP874'
      'CP932'
      'CP949'
      'CP950'
      'CP1250'
      'CP1251'
      'CP1252'
      'CP1253'
      'CP1254'
      'CP1255'
      'CP1256'
      'CP1257'
      'GB2312'
      'EUC-JP'
      'EUC-KR'
      'EUC-TW'
      'BIG5'
      'BIG5-HKSCS'
      'GBK'
      'GB18030'
      'SHIFT_JIS'
      'JOHAB'
      'TIS-620'
      'VISCII'
      'GEORGIAN-PS'
      'UTF-8')
  end
  object b_Ok: TButton
    Left = 240
    Top = 368
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object b_Cancel: TButton
    Left = 320
    Top = 368
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object m_Comments: TMemo
    Left = 8
    Top = 24
    Width = 385
    Height = 81
    TabOrder = 0
    WordWrap = False
  end
  object ed_LastTranslator: TEdit
    Left = 8
    Top = 224
    Width = 153
    Height = 21
    TabOrder = 4
  end
  object ed_LastEmail: TEdit
    Left = 168
    Top = 224
    Width = 225
    Height = 21
    TabOrder = 5
  end
  object ed_BasePath: TEdit
    Left = 8
    Top = 336
    Width = 385
    Height = 21
    TabOrder = 8
  end
end
