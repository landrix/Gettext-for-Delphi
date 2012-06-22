object FormHelp: TFormHelp
  Left = 0
  Top = 0
  Caption = 'Help'
  ClientHeight = 342
  ClientWidth = 621
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
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 621
    Height = 342
    TabStop = False
    Align = alClient
    BorderStyle = bsNone
    Color = clWhite
    Lines.Strings = (
      'Keyboard shortcuts:'
      ''
      '* Control-Up, PageUp: Go to previous item.'
      '* Control-Down, PageDown: Go to next item.'
      ''
      'Make sales demo:'
      ''
      '* Identify the units that you want to demonstrate'
      
        '* Create a rules set that applies a label to translations that a' +
        're in these units'
      '* Filter by this label'
      '* Translate the items found'
      ''
      'Tips:'
      ''
      
        '* During translation, apply a "reviewprogrammers" label to those' +
        ' items, that the programmers should improve.')
    ReadOnly = True
    TabOrder = 0
  end
end
