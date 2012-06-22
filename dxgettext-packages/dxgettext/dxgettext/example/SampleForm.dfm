object FormMain: TFormMain
  Left = 502
  Top = 215
  Width = 354
  Height = 179
  Caption = 'GNU gettext sample application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonTestGettext: TButton
    Left = 64
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Click me'
    TabOrder = 0
    OnClick = ButtonTestGettextClick
  end
  object ButtonTestResourcestring: TButton
    Left = 144
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Click me'
    TabOrder = 1
    OnClick = ButtonTestResourcestringClick
  end
end
