object FormWork: TFormWork
  Left = 192
  Top = 114
  Width = 437
  Height = 354
  Caption = 'Extracting text'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    429
    320)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelProgress: TLabel
    Left = 8
    Top = 266
    Width = 413
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Initializing...'
  end
  object MemoProgress: TMemo
    Left = 8
    Top = 8
    Width = 413
    Height = 243
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 120
    Top = 290
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Close'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object XPManifest: TXPManifest
    Left = 112
    Top = 40
  end
  object TimerActivate: TTimer
    Interval = 10
    OnTimer = TimerActivateTimer
    Left = 168
    Top = 56
  end
end
