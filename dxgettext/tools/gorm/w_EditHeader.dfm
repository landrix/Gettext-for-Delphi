object f_EditHeader: Tf_EditHeader
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit Header'
  ClientHeight = 353
  ClientWidth = 337
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
    Left = 136
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
    Width = 151
    Height = 26
    Caption = 'Charset (use UTF-8 unless you know what you are doing!)'
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
    Left = 136
    Top = 208
    Width = 145
    Height = 13
    Caption = 'Last translator'#39's email address'
  end
  object ed_Project: TEdit
    Left = 8
    Top = 128
    Width = 321
    Height = 21
    TabOrder = 0
  end
  object ed_Team: TEdit
    Left = 8
    Top = 176
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object ed_TeamEmail: TEdit
    Left = 136
    Top = 176
    Width = 193
    Height = 21
    TabOrder = 2
  end
  object cmb_Language: TComboBox
    Left = 8
    Top = 288
    Width = 153
    Height = 21
    TabOrder = 5
  end
  object cmb_Charset: TComboBox
    Left = 168
    Top = 288
    Width = 153
    Height = 21
    TabOrder = 6
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
    Left = 176
    Top = 320
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object b_Cancel: TButton
    Left = 256
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object m_Comments: TMemo
    Left = 8
    Top = 24
    Width = 321
    Height = 81
    TabOrder = 9
    WordWrap = False
  end
  object ed_LastTranslator: TEdit
    Left = 8
    Top = 224
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object ed_LastEmail: TEdit
    Left = 136
    Top = 224
    Width = 193
    Height = 21
    TabOrder = 4
  end
end
