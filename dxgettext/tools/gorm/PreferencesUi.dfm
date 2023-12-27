object FormPreferences: TFormPreferences
  Left = 70
  Top = 141
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 621
  ClientWidth = 577
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
  object LabelPath: TStaticText
    Left = 16
    Top = 16
    Width = 22
    Height = 17
    Caption = '123'
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 416
    Top = 584
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 7
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 496
    Top = 584
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object GroupBoxStartupSettings: TGroupBox
    Left = 8
    Top = 39
    Width = 265
    Height = 218
    Caption = '  Settings to be applied at startup  '
    TabOrder = 1
    object LabelParallelTranslation: TLabel
      Left = 16
      Top = 96
      Width = 88
      Height = 13
      Caption = 'Parallel translation'
    end
    object l_OpenFile: TLabel
      Left = 16
      Top = 48
      Width = 47
      Height = 13
      Caption = 'Open file:'
    end
    object ComboBoxParallelTranslation: TComboBox
      Left = 16
      Top = 112
      Width = 177
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'Off')
    end
    object CheckBoxStartupAddStdLabels: TCheckBox
      Left = 16
      Top = 144
      Width = 241
      Height = 17
      Caption = 'Automatic add standard labels'
      TabOrder = 3
    end
    object CheckBoxApplySettingsAtStartup: TCheckBox
      Left = 16
      Top = 24
      Width = 241
      Height = 17
      Hint = 'This is good before sending files to the translator'
      Caption = 'Apply these settings at startup'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object CheckBoxAutoUpgrade: TCheckBox
      Left = 16
      Top = 168
      Width = 241
      Height = 17
      Caption = 'Enable automatic upgrade via internet'
      TabOrder = 4
    end
    object chk_ShowStatus: TCheckBox
      Left = 16
      Top = 192
      Width = 241
      Height = 17
      Caption = 'Show status bar'
      TabOrder = 5
    end
    object EditFilenameToOpen: TButtonedEdit
      Left = 16
      Top = 64
      Width = 233
      Height = 21
      Images = ilOpenFile
      ParentShowHint = False
      RightButton.DisabledImageIndex = 1
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      ShowHint = True
      TabOrder = 1
      OnRightButtonClick = EditFilenameToOpenRightButtonClick
    end
  end
  object GroupBoxTranslationTest: TGroupBox
    Left = 8
    Top = 272
    Width = 265
    Height = 169
    Caption = '  Run app to test translation  '
    TabOrder = 2
    object l_ApplicationExeFilename: TLabel
      Left = 16
      Top = 24
      Width = 116
      Height = 13
      Caption = 'Application exe filename'
    end
    object l_MoFilename: TLabel
      Left = 16
      Top = 72
      Width = 60
      Height = 13
      Caption = 'Mo file name'
    end
    object l_msgfmtexe: TLabel
      Left = 16
      Top = 120
      Width = 116
      Height = 13
      Caption = 'Path to msgfmt.exe tool'
    end
    object ed_ApplicationExeFilename: TButtonedEdit
      Left = 16
      Top = 40
      Width = 233
      Height = 21
      Images = ilOpenFile
      RightButton.DisabledImageIndex = 1
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      TabOrder = 0
      OnRightButtonClick = ButtonBrowseExenameClick
    end
    object ed_MoFilename: TButtonedEdit
      Left = 16
      Top = 88
      Width = 233
      Height = 21
      Images = ilOpenFile
      RightButton.DisabledImageIndex = 1
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      TabOrder = 1
      OnRightButtonClick = ed_MoFilenameRightButtonClick
    end
    object ed_msgfmtexe: TButtonedEdit
      Left = 16
      Top = 136
      Width = 233
      Height = 21
      Images = ilOpenFile
      RightButton.DisabledImageIndex = 1
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      TabOrder = 2
      OnRightButtonClick = ed_msgfmtexeRightButtonClick
    end
  end
  object grp_SaveSettings: TGroupBox
    Left = 288
    Top = 494
    Width = 281
    Height = 83
    Caption = '  Save settings  '
    TabOrder = 8
    object ed_WrapLinesAfter: TEdit
      Left = 40
      Top = 52
      Width = 57
      Height = 21
      Enabled = False
      TabOrder = 2
      Text = '0'
    end
    object chk_WrapAtNCharacters: TCheckBox
      Left = 16
      Top = 32
      Width = 193
      Height = 17
      Caption = 'Wrap lines after [characters]'
      TabOrder = 1
      OnClick = chk_WrapAtNCharactersClick
    end
    object udWrap: TUpDown
      Left = 97
      Top = 52
      Width = 17
      Height = 21
      Associate = ed_WrapLinesAfter
      Max = 255
      TabOrder = 3
      Thousands = False
      Wrap = True
    end
    object cb_UseGetTextDefaultFormatting: TCheckBox
      Left = 16
      Top = 16
      Width = 193
      Height = 17
      Caption = 'Use gettext default formatting'
      TabOrder = 0
      OnClick = cb_UseGetTextDefaultFormattingClick
    end
  end
  object groupTranslationMemory: TGroupBox
    Left = 285
    Top = 39
    Width = 284
    Height = 162
    Caption = '  Translation memory  '
    TabOrder = 4
    object CheckBoxTranslationMemory: TCheckBox
      Left = 16
      Top = 23
      Width = 265
      Height = 17
      Caption = 'Enable translation memory'
      TabOrder = 0
      OnClick = CheckBoxTranslationMemoryClick
    end
    object edMaxMemoryFileSize: TLabeledEdit
      Left = 32
      Top = 56
      Width = 34
      Height = 21
      EditLabel.Width = 86
      EditLabel.Height = 13
      EditLabel.Caption = ' Max file size, MB:'
      LabelPosition = lpRight
      NumbersOnly = True
      TabOrder = 1
      Text = '0'
    end
    object edCompareAccuracy: TLabeledEdit
      Left = 33
      Top = 94
      Width = 33
      Height = 21
      Hint = 
        'Difference between two strings that admissible in suggestions li' +
        'st: ( 0 - only perfect matches)'
      EditLabel.Width = 82
      EditLabel.Height = 13
      EditLabel.Caption = ' Search accuracy'
      LabelPosition = lpRight
      NumbersOnly = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = '0'
    end
    object edSuggestionsNumber: TLabeledEdit
      Left = 32
      Top = 129
      Width = 33
      Height = 21
      EditLabel.Width = 148
      EditLabel.Height = 13
      EditLabel.Caption = ' Number of sugestions to show'
      LabelPosition = lpRight
      NumbersOnly = True
      TabOrder = 5
      Text = '0'
    end
    object udMaxMemoryFSize: TUpDown
      Left = 66
      Top = 56
      Width = 16
      Height = 21
      Associate = edMaxMemoryFileSize
      TabOrder = 2
      Thousands = False
    end
    object unMemCopmAccur: TUpDown
      Left = 66
      Top = 94
      Width = 16
      Height = 21
      Associate = edCompareAccuracy
      TabOrder = 4
      Thousands = False
    end
    object udMemSuggNum: TUpDown
      Left = 65
      Top = 129
      Width = 16
      Height = 21
      Associate = edSuggestionsNumber
      TabOrder = 6
      Thousands = False
    end
  end
  object gbSymbols: TGroupBox
    Left = 285
    Top = 213
    Width = 284
    Height = 180
    Caption = '  Special chars inserting  '
    TabOrder = 5
    object l_ListOfChars: TLabel
      Left = 16
      Top = 21
      Width = 62
      Height = 13
      Caption = 'List of chars:'
    end
    object memSpecialChars: TMemo
      Left = 16
      Top = 40
      Width = 257
      Height = 89
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial Unicode MS'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object edSymbolsFontSize: TLabeledEdit
      Left = 16
      Top = 143
      Width = 33
      Height = 21
      EditLabel.Width = 109
      EditLabel.Height = 13
      EditLabel.Caption = ' Special chars font size'
      LabelPosition = lpRight
      NumbersOnly = True
      TabOrder = 2
      Text = '12'
    end
    object udSymbolsFontSize: TUpDown
      Left = 49
      Top = 143
      Width = 16
      Height = 21
      Associate = edSymbolsFontSize
      Position = 12
      TabOrder = 3
      Thousands = False
    end
    object b_Default: TButton
      Left = 200
      Top = 136
      Width = 75
      Height = 25
      Caption = 'Default'
      TabOrder = 1
      OnClick = b_DefaultClick
    end
  end
  object grp_TranslationRepositiory: TGroupBox
    Left = 288
    Top = 408
    Width = 281
    Height = 73
    Caption = 'Translation Repositiory'
    TabOrder = 6
    object l_TransRepDir: TLabel
      Left = 16
      Top = 24
      Width = 191
      Height = 13
      Caption = 'Directory to store translation repository'
    end
    object ed_TransRepDir: TButtonedEdit
      Left = 16
      Top = 40
      Width = 249
      Height = 21
      Hint = 
        'Paths must be relative if you want to move the files to a differ' +
        'ent location.'
      Images = ilOpenFile
      RightButton.DisabledImageIndex = 1
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      TabOrder = 0
      OnRightButtonClick = ed_TransRepDirRightButtonClick
    end
  end
  object gb_ExternalEditor: TGroupBox
    Left = 8
    Top = 456
    Width = 265
    Height = 89
    Caption = 'External editor'
    TabOrder = 3
    object l_ExternalEditor: TLabel
      Left = 16
      Top = 24
      Width = 141
      Height = 13
      Caption = 'Exernal Editor for source files'
    end
    object cb_ExternalEditorUseLineNumbers: TCheckBox
      Left = 16
      Top = 64
      Width = 241
      Height = 17
      Caption = 'add "-n#" to goto line number'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object ed_ExternalEditor: TButtonedEdit
      Left = 16
      Top = 40
      Width = 233
      Height = 21
      Images = ilOpenFile
      ParentShowHint = False
      RightButton.DisabledImageIndex = 1
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      ShowHint = True
      TabOrder = 0
      OnRightButtonClick = ed_ExternalEditorRightButtonClick
    end
  end
  object ilOpenFile: TImageList
    Left = 528
    Top = 8
    Bitmap = {
      494C010102000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CFE0E93083AFC77C6A9DB795E0E0E01FE5E5E51AE5E5E51AE5E5
      E51AE5E5E51AE5E5E51AE5E5E51AEEEEEE110000000000000000000000000000
      000000000000D6D6D6309595957C7C7C7C95E0E0E01FE5E5E51AE5E5E51AE5E5
      E51AE5E5E51AE5E5E51AE5E5E51AEEEEEE110000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AFCE
      DF5066A2C099549ABEC067AFD3E0236786E2818171938C8C7B8A8F8F7E878F8F
      7E878F8F7E878F8F7E878F8F7E87A3A3996A000000000000000000000000BFBF
      BF50828282997E7E7EC09C9C9CE0343434E26C6C6C937575758A787878877878
      78877878788778787887787878879595956A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EEEEEE11DBDBDB246790A6983F95
      C0F26BB9DDFA82CBECFF85CEEEFF4186A8FFE5E5E5FFE5E5E4FFFEFEFDFFFEFE
      FCFFFDFDFAFFFCFCF9FFFEFEF9FF96968588EEEEEE11DBDBDB24696969987C7C
      7CF2ACACACFABFBFBFFFC2C2C2FF6B6B6BFFE6E6E6FFE6E6E6FFFFFFFFFFFFFF
      FFFFFCFCFCFFFCFCFCFFFFFFFFFF797979880000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F609EDEDED124D94B7B288D0
      EFFF7FCAE9FF7FCAE9FF87D0EFFF4489ACFFE4E4E4FFE3E3E2FFFCFCFBFFFBFB
      F8FFFAFAF6FFF8F8F4FFFBFBF5FFA8A8977DF6F6F609EDEDED12707070B2C4C4
      C4FFBABABAFFBABABAFFC4C4C4FF6E6E6EFFE3E3E3FFE3E3E3FFFCFCFCFFFAFA
      FAFFFAFAFAFFF7F7F7FFFAFAFAFF9393937D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005BA2C6A48AD3
      F0FF82CDEBFF82CDEBFF8AD3F0FF468BAFFFB7B7A8FFB6B6A7FFDBDBCAFFD1D1
      C0FFF8F8F4FFF7F7F1FFFAFAF4FFB1B1A0760000000000000000848484A4C4C4
      C4FFBDBDBDFFBDBDBDFFC4C4C4FF737373FFE3E3E3FFE3E3E3FFFAFAFAFFFAFA
      FAFFF7F7F7FFF7F7F7FFFAFAFAFF9E9E9E760000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000061A8CB9E8ED6
      F2FF87D0EDFF87D0EDFF8ED6F2FF488FB2FFD5D5C6FFD4D4C5FFEBEBDAFFE9E9
      D8FFF7F7F1FFF5F5EFFFFAFAF2FFB5B5A47400000000000000008B8B8B9EC9C9
      C9FFC2C2C2FFC2C2C2FFC9C9C9FF757575FFE3E3E3FFE0E0E0FFFAFAFAFFF7F7
      F7FFF7F7F7FFF5F5F5FFF7F7F7FFA2A2A2740000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000064ABCE9B92DA
      F4FF8BD4F0FF8BD4F0FF92DAF4FF4C92B6FFB5B5A6FFC4C4B4FFC7C7B6FFD6D6
      C5FFD4D4C3FFCACAB9FFF8F8EEFFB8B8A77200000000000000009090909BCCCC
      CCFFC7C7C7FFC7C7C7FFCCCCCCFF7A7A7AFFE0E0E0FFE0E0E0FFF7F7F7FFF7F7
      F7FFF5F5F5FFF2F2F2FFF5F5F5FFA7A7A7720000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000068AFD29797DE
      F6FF90D8F2FF90D8F2FF97DEF6FF4F96B8FFD3D3C4FFD1D1C2FFE7E7D6FFE5E5
      D4FFE4E4D3FFE2E2D1FFF6F6E8FFBCBCAB70000000000000000096969697CFCF
      CFFFC9C9C9FFC9C9C9FFCFCFCFFF808080FFE0E0E0FFDEDEDEFFF7F7F7FFF5F5
      F5FFF2F2F2FFEDEDEDFFF0F0F0FFABABAB700000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006BB2D5949BE1
      F7FF94DBF4FF94DBF4FF9BE1F7FF5299BCFFB3B3A3FFC0C0B1FFC3C3B2FFC2C2
      B1FFC0C0AFFFBEBEADFFF3F3E3FFBFBFAE6E00000000000000009A9A9A94D4D4
      D4FFCFCFCFFFCFCFCFFFD4D4D4FF858585FFDEDEDEFFDEDEDEFFF5F5F5FFF2F2
      F2FFEDEDEDFFE6E6E6FFEDEDEDFFAFAFAF6E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006FB5D8909EE5
      F9FF98DFF6FF98DFF6FF9EE5F9FF569CBFFFCFCFC0FFCECEBEFFE4E4D3FFF0F0
      E6FFEBEBDDFFE7E7D6FFF2F2E1FFC2C2B16C00000000000000009E9E9E90D6D6
      D6FFCFCFCFFFCFCFCFFFD6D6D6FF878787FFDEDEDEFFDBDBDBFFF2F2F2FFEDED
      EDFFE6E6E6FFE3E3E3FFEDEDEDFFB3B3B36C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000072B8DB8DA3E8
      FBFF9DE3F9FF9DE3F9FFA3E8FBFF589FC2FFAFAFA0FFBDBDAEFFC8C8B7FFEBEB
      DDFFA4A493FFA4A493FFA4A493FFA6A6947C0000000000000000A3A3A38DD6D6
      D6FFD6D6D6FFD6D6D6FFD6D6D6FF8A8A8AFFDBDBDBFFD9D9D9FFEDEDEDFFE6E6
      E6FF999999FF999999FF999999FF8F8F8F7C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000075BBDD8AA6EB
      FCFFA1E6FBFFA1E6FBFFA6EBFCFF62A8C9FFDBDBD4FFD8D8CFFFEBEBDDFFE7E7
      D6FFB6B6A5FFFFFFFFFFC7C7B669EBEBE5250000000000000000A6A6A68ADBDB
      DBFFD6D6D6FFD6D6D6FFDBDBDBFF969696FFD9D9D9FFD6D6D6FFE6E6E6FFE3E3
      E3FFADADADFFFFFFFFFFB9B9B969E6E6E6250000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000078BDE087A9EE
      FDFFA4E9FCFFA4E9FCFFAAEFFDFF6DAFCDFFEDEDE4FFF4F4E7FFF4F4E3FFF2F2
      E1FFC2C2B1FF79A2A6B8EBEBE525000000000000000000000000A9A9A987DBDB
      DBFFD9D9D9FFD9D9D9FFDBDBDBFF9E9E9EFFEBEBEBFFF0F0F0FFF0F0F0FFEDED
      EDFFBABABAFF858585B8E7E7E725000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007ABFE285ADF1
      FFFFABEFFEFF94E2F8FF6EC8EDFF4397B9FF8EB7BAFF8EB7BAFF8EB7BAFF8EB7
      BAFF93BCBBFF78B6D09600000000000000000000000000000000ACACAC85E0E0
      E0FFDEDEDEFFCFCFCFFFB8B8B8FF7A7A7AFFA3A3A3FFA3A3A3FFA3A3A3FFA3A3
      A3FFA8A8A8FF9D9D9D9600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007CC1E38388DB
      F4FF60C1E9FF5FBFEAFF80D3F4FF9CE3FDFFA2E6FFFFA2E6FFFFA2E6FFFFA2E6
      FFFFA6EAFFFF7CC1E38300000000000000000000000000000000AEAEAE83C9C9
      C9FFADADADFFB0B0B0FFC4C4C4FFD6D6D6FFDBDBDBFFDBDBDBFFDBDBDBFFDBDB
      DBFFDBDBDBFFAEAEAE8300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009ED1EB617EC2
      E5817EC2E5817EC2E5817EC2E5817EC2E5817EC2E5817EC2E5817EC2E5817EC2
      E5817EC2E5819ED1EB6100000000000000000000000000000000C4C4C461B1B1
      B181B1B1B181B1B1B181B1B1B181B1B1B181B1B1B181B1B1B181B1B1B181B1B1
      B181B1B1B181C4C4C46100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F800F80000000000E000E00000000000
      00000000000000000000000000000000C000C00000000000C000C00000000000
      C000C00000000000C000C00000000000C000C00000000000C000C00000000000
      C000C00000000000C000C00000000000C001C00100000000C003C00300000000
      C003C00300000000C003C0030000000000000000000000000000000000000000
      000000000000}
  end
end
