object Form1: TForm1
  Left = 352
  Top = 246
  Width = 393
  Height = 329
  ActiveControl = ButtonOK
  Caption = 'Embed translations in executable'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    385
    295)
  PixelsPerInch = 96
  TextHeight = 14
  object LabelExplanation: TLabel
    Left = 8
    Top = 8
    Width = 369
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This will append the following translations to your executable f' +
      'ile.'
    WordWrap = True
  end
  object LabelExeHeading: TLabel
    Left = 8
    Top = 72
    Width = 73
    Height = 14
    Caption = 'Executable file:'
  end
  object LabelExeName: TLabel
    Left = 88
    Top = 72
    Width = 288
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
  end
  object ListBoxTranslations: TCheckListBox
    Left = 8
    Top = 88
    Width = 368
    Height = 166
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 14
    TabOrder = 0
  end
  object PanelButtons: TPanel
    Left = 88
    Top = 256
    Width = 177
    Height = 36
    Anchors = [akLeft, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonOK: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
  object XPManifest: TXPManifest
    Left = 192
    Top = 112
  end
end
