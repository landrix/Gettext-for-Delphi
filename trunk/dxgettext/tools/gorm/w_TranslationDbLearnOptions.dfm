object f_TranslationDbLearnOptions: Tf_TranslationDbLearnOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Translation Repository Learn Options'
  ClientHeight = 193
  ClientWidth = 433
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
  object l_Filename: TLabel
    Left = 8
    Top = 8
    Width = 127
    Height = 13
    Caption = 'Translation Repository File'
  end
  object ed_Filename: TEdit
    Left = 8
    Top = 24
    Width = 417
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object b_Ok: TButton
    Left = 272
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object b_Cancel: TButton
    Left = 352
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object chk_AddTagToRepository: TCheckBox
    Left = 8
    Top = 64
    Width = 201
    Height = 17
    Caption = 'Add tag to repository'
    TabOrder = 1
    OnClick = chk_AddTagToRepositoryClick
  end
  object ed_RepositoryTag: TEdit
    Left = 24
    Top = 88
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 2
    Text = 'auto'
  end
  object ed_PoTag: TEdit
    Left = 224
    Top = 88
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'add'
  end
  object chk_Preview: TCheckBox
    Left = 224
    Top = 128
    Width = 201
    Height = 17
    Caption = 'Preview changes by adding label'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object chk_AddTagToPoFile: TCheckBox
    Left = 224
    Top = 64
    Width = 153
    Height = 17
    Caption = 'Add tag to po file'
    TabOrder = 4
  end
end
