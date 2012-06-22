object f_TranslationDb: Tf_TranslationDb
  Left = 0
  Top = 0
  Caption = 'Translation DB'
  ClientHeight = 348
  ClientWidth = 535
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
  object tc_Language: TTabControl
    Left = 0
    Top = 0
    Width = 535
    Height = 348
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Latin'
      'Interkosmo'
      'new ...')
    TabIndex = 0
    OnChange = tc_LanguageChange
    OnChanging = tc_LanguageChanging
    object p_Bottom: TPanel
      Left = 4
      Top = 192
      Width = 527
      Height = 152
      Align = alBottom
      BevelOuter = bvNone
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      TabOrder = 0
      object pc_Translations: TPageControl
        Left = 0
        Top = 0
        Width = 440
        Height = 152
        Align = alClient
        TabOrder = 0
      end
      object p_BottomRight: TPanel
        Left = 440
        Top = 0
        Width = 87
        Height = 152
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object b_One: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Action = act_TransAdd
          TabOrder = 0
        end
        object b_Two: TButton
          Left = 8
          Top = 40
          Width = 75
          Height = 25
          Action = act_TransEdit
          TabOrder = 1
        end
        object b_Delete: TButton
          Left = 8
          Top = 72
          Width = 75
          Height = 25
          Action = act_TransDel
          TabOrder = 2
        end
        object b_Close: TButton
          Left = 8
          Top = 120
          Width = 75
          Height = 25
          Action = act_Close
          TabOrder = 3
        end
      end
      object m_TransEdit: TMemo
        Left = 8
        Top = 24
        Width = 409
        Height = 100
        TabOrder = 2
        Visible = False
        OnChange = m_TransEditChange
      end
    end
    object dbg_English: TDBGrid
      Left = 4
      Top = 73
      Width = 527
      Height = 119
      Align = alClient
      BorderStyle = bsNone
      DataSource = ds_English
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'Content'
          Title.Caption = 'English Text'
          Width = 500
          Visible = True
        end>
    end
    object dbn_English: TDBNavigator
      Left = 4
      Top = 24
      Width = 527
      Height = 25
      DataSource = ds_English
      Align = alTop
      TabOrder = 2
    end
    object p_Locate: TPanel
      Left = 4
      Top = 49
      Width = 527
      Height = 24
      Align = alTop
      TabOrder = 3
      object l_LookupEnglish: TLabel
        Left = 8
        Top = 4
        Width = 129
        Height = 13
        AutoSize = False
        Caption = 'Lookup English'
      end
      object l_LookupTranslation: TLabel
        Left = 264
        Top = 4
        Width = 129
        Height = 13
        AutoSize = False
        Caption = 'Lookup Translation'
      end
      object ed_LocateEnglish: TEdit
        Left = 136
        Top = 0
        Width = 121
        Height = 21
        TabOrder = 0
        OnChange = ed_LocateEnglishChange
      end
      object ed_LocateTranslation: TEdit
        Left = 392
        Top = 0
        Width = 129
        Height = 21
        TabOrder = 1
        OnChange = ed_LocateTranslationChange
      end
    end
  end
  object ds_English: TDataSource
    OnDataChange = ds_EnglishDataChange
    Left = 136
    Top = 80
  end
  object al_Translation: TActionList
    Left = 104
    Top = 160
    object act_TransAdd: TAction
      Category = 'trans'
      Caption = '&Add'
      OnExecute = act_TransAddExecute
    end
    object act_TransEdit: TAction
      Category = 'trans'
      Caption = '&Edit'
      OnExecute = act_TransEditExecute
    end
    object act_TransDel: TAction
      Category = 'trans'
      Caption = '&Delete'
      OnExecute = act_TransDelExecute
    end
    object act_TransEditOk: TAction
      Category = 'transedit'
      Caption = '&OK'
      Enabled = False
      OnExecute = act_TransEditOkExecute
    end
    object act_TransEditCancel: TAction
      Category = 'transedit'
      Caption = '&Cancel'
      OnExecute = act_TransEditCancelExecute
    end
    object act_TransAddOk: TAction
      Category = 'transadd'
      Caption = '&OK'
      Enabled = False
      OnExecute = act_TransAddOkExecute
    end
    object act_TransAddCancel: TAction
      Category = 'transadd'
      Caption = '&Cancel'
      OnExecute = act_TransAddCancelExecute
    end
    object act_Close: TAction
      Caption = 'Close'
      OnExecute = act_CloseExecute
    end
  end
end
