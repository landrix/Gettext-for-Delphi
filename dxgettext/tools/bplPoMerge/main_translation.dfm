object frmMain: TfrmMain
  Left = 219
  Top = 122
  Caption = 'Autotranslate BPL-Ressources to PO file'
  ClientHeight = 718
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = TheMainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splitMain: TSplitter
    Left = 0
    Top = 369
    Width = 740
    Height = 4
    Cursor = crVSplit
    Align = alTop
    MinSize = 10
  end
  object panelClient: TPanel
    Left = 0
    Top = 0
    Width = 740
    Height = 369
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Values: TListView
      Left = 0
      Top = 89
      Width = 740
      Height = 280
      Align = alClient
      Columns = <
        item
          Caption = 'ID'
          Width = 100
        end
        item
          Caption = 'BPL (english)'
          Width = 320
        end
        item
          Caption = 'language dependent'
          Width = 320
        end>
      Constraints.MinHeight = 20
      TabOrder = 0
      ViewStyle = vsReport
      OnCompare = ValuesCompare
    end
    object panel1: TGroupBox
      Left = 0
      Top = 0
      Width = 740
      Height = 89
      Align = alTop
      Caption = 'Step 1'
      TabOrder = 1
      DesignSize = (
        740
        89)
      object l_BplDir: TLabel
        Left = 9
        Top = 16
        Width = 88
        Height = 13
        Caption = 'BPL files to extract'
      end
      object l_LanguageExtension: TLabel
        Left = 272
        Top = 16
        Width = 299
        Height = 13
        Caption = 'Extension for corresponding language dependent ressourcefiles'
      end
      object lbLoadInfo: TLabel
        Left = 328
        Top = 32
        Width = 54
        Height = 13
        Caption = 'Loading %s'
      end
      object lbEntriesInfo: TLabel
        Left = 328
        Top = 48
        Width = 115
        Height = 13
        Caption = '%d translations available'
      end
      object edBPL: TEdit
        Left = 8
        Top = 32
        Width = 217
        Height = 21
        TabOrder = 0
        Text = 'c:\windows\*.bpl'
      end
      object edLang: TEdit
        Left = 272
        Top = 32
        Width = 41
        Height = 21
        TabOrder = 2
        Text = '.de'
      end
      object btExtract: TBitBtn
        Left = 608
        Top = 28
        Width = 121
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Extract ressources'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 3
        OnClick = btExtractClick
      end
      object progressLoad: TProgressBar
        Left = 8
        Top = 64
        Width = 721
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
      end
      object b_SelectBplSourceDir: TButton
        Left = 232
        Top = 30
        Width = 25
        Height = 25
        Caption = '...'
        TabOrder = 1
        OnClick = b_SelectBplSourceDirClick
      end
    end
  end
  object p_Bottom: TPanel
    Left = 0
    Top = 373
    Width = 740
    Height = 345
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object panel2: TGroupBox
      Left = 0
      Top = 0
      Width = 740
      Height = 105
      Align = alTop
      Caption = 'Step 2'
      TabOrder = 0
      DesignSize = (
        740
        105)
      object l_PoFile: TLabel
        Left = 8
        Top = 16
        Width = 116
        Height = 13
        Caption = 'PO file to merge / create'
      end
      object btMergePO: TButton
        Left = 600
        Top = 12
        Width = 129
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Merge PO file'
        TabOrder = 3
        OnClick = btMergePOClick
      end
      object edPO: TEdit
        Left = 8
        Top = 32
        Width = 545
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'delphi7.po'
      end
      object progressBusy: TProgressBar
        Left = 8
        Top = 80
        Width = 721
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
      end
      object chkUTF8: TCheckBox
        Left = 8
        Top = 56
        Width = 275
        Height = 17
        Caption = 'write conforming to UTF8 (see your PO file setting)'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object btCreatePO: TButton
        Left = 600
        Top = 40
        Width = 129
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Create PO file'
        TabOrder = 4
        OnClick = btCreatePOClick
      end
      object b_SelectPo: TButton
        Left = 560
        Top = 30
        Width = 25
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = b_SelectPoClick
      end
    end
    object listboxMerge: TListBox
      Left = 0
      Top = 105
      Width = 740
      Height = 240
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object SaveDialog: TSaveDialog
    Left = 384
    Top = 144
  end
  object OpenDialog: TOpenDialog
    Left = 312
    Top = 144
  end
  object TheMainMenu: TMainMenu
    Left = 72
    Top = 136
    object Files1: TMenuItem
      Caption = '&Files'
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object Extractions1: TMenuItem
      Caption = '&Ressources'
      object Extract1: TMenuItem
        Caption = '&Extract'
        OnClick = btExtractClick
      end
      object Load1: TMenuItem
        Caption = '&Load'
        OnClick = Load1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
    end
    object POfiles1: TMenuItem
      Caption = '&PO files'
      object Merge1: TMenuItem
        Caption = '&Merge'
      end
      object Create1: TMenuItem
        Caption = '&Create'
      end
    end
    object N1: TMenuItem
      Caption = '&?'
      object Help1: TMenuItem
        Caption = '&Help'
        OnClick = Help1Click
      end
    end
  end
end
