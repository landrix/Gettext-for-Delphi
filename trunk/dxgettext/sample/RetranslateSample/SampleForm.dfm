object FormMain: TFormMain
  Left = 278
  Top = 233
  ActiveControl = ListBoxLanguage
  Caption = 'GNU gettext sample application'
  ClientHeight = 199
  ClientWidth = 383
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial Unicode MS'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    383
    199)
  PixelsPerInch = 96
  TextHeight = 16
  object ButtonTestGettext: TButton
    Left = 144
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Click me'
    TabOrder = 1
    OnClick = ButtonTestGettextClick
  end
  object ButtonTestResourcestring: TButton
    Left = 264
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Click me'
    TabOrder = 2
    OnClick = ButtonTestResourcestringClick
  end
  object ListBoxLanguage: TListBox
    Left = 8
    Top = 8
    Width = 129
    Height = 185
    Anchors = [akLeft, akTop, akBottom]
    ExtendedSelect = False
    ItemHeight = 16
    Items.Strings = (
      'System default'
      'Catalan'
      'Chinese'
      'Croatian'
      'Danish'
      'Dutch'
      'English'
      'Estonian'
      'French'
      'German'
      'Japanese'
      'Norwegian'
      'Russian'
      'Swedish')
    TabOrder = 0
    OnClick = ListBoxLanguageClick
  end
end
