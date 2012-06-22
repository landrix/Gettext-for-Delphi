object frmMain: TfrmMain
  Left = 219
  Top = 122
  Width = 756
  Height = 560
  Caption = 'Autotranslate BPL-Ressources to PO file'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splitMain: TSplitter
    Left = 0
    Top = 286
    Width = 748
    Height = 4
    Cursor = crVSplit
    Align = alTop
    MinSize = 10
  end
  object panel2: TGroupBox
    Left = 0
    Top = 445
    Width = 748
    Height = 69
    Align = alBottom
    Caption = 'Step 2'
    TabOrder = 0
    object Label3: TLabel
      Left = 8
      Top = 20
      Width = 81
      Height = 13
      Caption = 'PO file to merge :'
    end
    object btMergePO: TButton
      Left = 440
      Top = 12
      Width = 129
      Height = 25
      Caption = 'Merge PO file'
      TabOrder = 0
      OnClick = btMergePOClick
    end
    object edPO: TEdit
      Left = 150
      Top = 16
      Width = 259
      Height = 21
      TabOrder = 1
      Text = 'delphi7.po'
    end
    object progressBusy: TProgressBar
      Left = 584
      Top = 32
      Width = 129
      Height = 16
      TabOrder = 2
    end
    object chkUTF8: TCheckBox
      Left = 150
      Top = 40
      Width = 275
      Height = 17
      Caption = 'write conforming to UTF8 (see your PO file setting)'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object btCreatePO: TButton
      Left = 440
      Top = 40
      Width = 129
      Height = 25
      Caption = 'Create PO file'
      TabOrder = 4
      OnClick = btCreatePOClick
    end
  end
  object listboxMerge: TListBox
    Left = 0
    Top = 290
    Width = 748
    Height = 155
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object panelClient: TPanel
    Left = 0
    Top = 0
    Width = 748
    Height = 286
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Values: TListView
      Left = 0
      Top = 0
      Width = 748
      Height = 214
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
      Top = 214
      Width = 748
      Height = 72
      Align = alBottom
      Caption = 'Step 1'
      TabOrder = 1
      object Label1: TLabel
        Left = 16
        Top = 20
        Width = 94
        Height = 13
        Caption = 'BPL files to extract :'
      end
      object Label2: TLabel
        Left = 16
        Top = 44
        Width = 305
        Height = 13
        Caption = 'Extension for corresponding language dependent ressourcefiles :'
      end
      object lbLoadInfo: TLabel
        Left = 584
        Top = 16
        Width = 3
        Height = 13
        Caption = '-'
      end
      object lbEntriesInfo: TLabel
        Left = 584
        Top = 48
        Width = 3
        Height = 13
        Caption = '-'
      end
      object edBPL: TEdit
        Left = 184
        Top = 16
        Width = 217
        Height = 21
        TabOrder = 0
        Text = 'c:\winnt\system32\*.bpl'
      end
      object edLang: TEdit
        Left = 360
        Top = 40
        Width = 41
        Height = 21
        TabOrder = 1
        Text = '.de'
      end
      object btExtract: TBitBtn
        Left = 440
        Top = 28
        Width = 121
        Height = 25
        Caption = 'Extract ressources'
        TabOrder = 2
        OnClick = btExtractClick
      end
      object progressLoad: TProgressBar
        Left = 584
        Top = 32
        Width = 129
        Height = 16
        TabOrder = 3
      end
    end
  end
  object SaveDialog: TSaveDialog
    Left = 504
    Top = 296
  end
  object OpenDialog: TOpenDialog
    Left = 448
    Top = 296
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 24
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
