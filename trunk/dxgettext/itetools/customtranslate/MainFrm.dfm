object Form1: TForm1
  Left = 286
  Top = 126
  AutoScroll = False
  Caption = 'Custom Translator Demo'
  ClientHeight = 477
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    657
    477)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 198
    Top = 177
    Width = 457
    Height = 261
    ActivePage = tabExtract
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 1
    TabOrder = 3
    object tabExplain: TTabSheet
      BorderWidth = 2
      Caption = 'Explanation'
      object RichEdit1: TRichEdit
        Left = 0
        Top = 0
        Width = 445
        Height = 229
        Align = alClient
        Lines.Strings = (
          
            'dxgettext automatically extracts published string properties fro' +
            'm dfm files and resourcestrings and string constants from source' +
            ' files. There are, however, some properties it can'#39't extract aut' +
            'omatically. You must then handle these translations yourself and' +
            ' this is a two step process:'
          ''
          '1. Get the original strings into the PO file'
          '2. Find and apply the translations at run-time'
          ''
          
            'For step 1, you have two options: add the strings manually to th' +
            'e PO file or add them to the source file as consts or resourcest' +
            'rings. Note that you don'#39't need to actually use the const or res' +
            'ourcestring variables: it is enough that they are defined as str' +
            'ings somewhere in the source.'
          ''
          
            'In this demo, the strings in the listview is extracted by clicki' +
            'ng the "Extract" button. The extracted strings are put into a me' +
            'mo in PO format. Next, you will need to copy these strings into ' +
            'your PO file (open with Notepad and add the strings at the botto' +
            'm). The treeview is "auto-translated" since all the strings used' +
            ' by it are declared as resourcestrings in the source.'
          ''
          
            'For step 2, you need to write a custom translation handler and r' +
            'egister it. This demo has example handlers for both the listview' +
            ' and the treeview that you can copy verbatim to your own project' +
            '. Make sure that the handler is registered (by calling TP_Global' +
            'HandleClass) before calling TranslateProperties!'
          '')
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object tabExtract: TTabSheet
      BorderWidth = 2
      Caption = 'Extracted strings'
      ImageIndex = 1
      object RichEdit2: TRichEdit
        Left = 0
        Top = 0
        Width = 445
        Height = 229
        Align = alClient
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 193
    Height = 477
    Align = alLeft
    Indent = 19
    TabOrder = 0
    Items.Data = {
      06000000240000000000000000000000FFFFFFFFFFFFFFFF0000000001000000
      0B456E7669726F6E6D656E74200000000000000000000000FFFFFFFFFFFFFFFF
      00000000000000000747656E6572616C1F0000000000000000000000FFFFFFFF
      FFFFFFFF000000000100000006566965776572200000000000000000000000FF
      FFFFFFFFFFFFFF00000000000000000747656E6572616C1E0000000000000000
      000000FFFFFFFFFFFFFFFF00000000020000000547616E747420000000000000
      0000000000FFFFFFFFFFFFFFFF00000000000000000747656E6572616C290000
      000000000000000000FFFFFFFFFFFFFFFF000000000000000010466F6E747320
      616E6420436F6C6F7273220000000000000000000000FFFFFFFFFFFFFFFF0000
      000001000000095265736F7572636573200000000000000000000000FFFFFFFF
      FFFFFFFF00000000000000000747656E6572616C1E0000000000000000000000
      FFFFFFFFFFFFFFFF0000000002000000055461736B7320000000000000000000
      0000FFFFFFFFFFFFFFFF00000000000000000747656E6572616C210000000000
      000000000000FFFFFFFFFFFFFFFF000000000000000008546F6F6C546970731F
      0000000000000000000000FFFFFFFFFFFFFFFF0000000002000000064F726465
      7273200000000000000000000000FFFFFFFFFFFFFFFF00000000000000000747
      656E6572616C1F0000000000000000000000FFFFFFFFFFFFFFFF000000000000
      000006436F6C6F7273}
  end
  object ListView1: TListView
    Left = 198
    Top = 0
    Width = 459
    Height = 170
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = 'Filename'
        Width = -1
        WidthType = (
          -1)
      end
      item
        AutoSize = True
        Caption = 'Type'
      end>
    Items.Data = {
      490300001400000000000000FFFFFFFFFFFFFFFF0100000000000000082E6376
      73706173730C637673706173732E66696C6500000000FFFFFFFFFFFFFFFF0100
      0000000000000961677265702E646F6308646F632D66696C6500000000FFFFFF
      FFFFFFFFFF01000000000000000961677265702E657865086578652D66696C65
      00000000FFFFFFFFFFFFFFFF01000000000000000C6175746F657865632E6261
      74086261742D66696C6500000000FFFFFFFFFFFFFFFF01000000000000000862
      6F6F742E696E6908696E692D66696C6500000000FFFFFFFFFFFFFFFF01000000
      000000000C626F6F74666F6E742E62696E0862696E2D66696C6500000000FFFF
      FFFFFFFFFFFF01000000000000000B626F6F746C6F672E747874087478742D66
      696C6500000000FFFFFFFFFFFFFFFF01000000000000000C626F6F7473656374
      2E646F7308646F732D66696C6500000000FFFFFFFFFFFFFFFF01000000000000
      000B636F6D6D616E642E636F6D08636F6D2D66696C6500000000FFFFFFFFFFFF
      FFFF01000000000000000A636F6E6669672E62696E0862696E2D66696C650000
      0000FFFFFFFFFFFFFFFF01000000000000000A636F6E6669672E737973087379
      732D66696C6500000000FFFFFFFFFFFFFFFF0100000000000000076374782E64
      6174086461742D66696C6500000000FFFFFFFFFFFFFFFF010000000000000006
      696F2E737973087379732D66696C6500000000FFFFFFFFFFFFFFFF0100000000
      000000096D73646F732E737973087379732D66696C6500000000FFFFFFFFFFFF
      FFFF01000000000000000C6E746465746563742E636F6D08636F6D2D66696C65
      00000000FFFFFFFFFFFFFFFF0100000000000000056E746C6472052D66696C65
      00000000FFFFFFFFFFFFFFFF01000000000000000C7061676566696C652E7379
      73087379732D66696C6500000000FFFFFFFFFFFFFFFF01000000000000000D70
      646F7875736572732E6E6574086E65742D66696C6500000000FFFFFFFFFFFFFF
      FF01000000000000000970757474792E726E6408726E642D66696C6500000000
      FFFFFFFFFFFFFFFF0100000000000000077379732E657272086572722D66696C
      65FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF}
    TabOrder = 1
    ViewStyle = vsReport
  end
  object btnExtract: TButton
    Left = 566
    Top = 444
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Extract'
    TabOrder = 2
    OnClick = btnExtractClick
  end
  object btnUseSV: TButton
    Left = 210
    Top = 444
    Width = 109
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use Swedish'
    TabOrder = 4
    OnClick = btnUseSVClick
  end
end
