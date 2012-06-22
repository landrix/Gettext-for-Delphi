object Form1: TForm1
  Left = 328
  Top = 150
  Width = 526
  Height = 531
  Caption = 'MO reader/compiler demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    518
    504)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 6
    Top = 6
    Width = 505
    Height = 445
    ActivePage = tabMO
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 0
    object tabMO: TTabSheet
      Caption = 'Read MO'
      DesignSize = (
        497
        417)
      object Label1: TLabel
        Left = 12
        Top = 6
        Width = 46
        Height = 13
        Caption = 'Filename:'
      end
      object edMOFile: TEdit
        Left = 12
        Top = 24
        Width = 451
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object reMO: TRichEdit
        Left = 10
        Top = 54
        Width = 481
        Height = 313
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Reads an MO file and display the content here')
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
      object btnMOOpen: TButton
        Left = 468
        Top = 24
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 2
        OnClick = btnMOOpenClick
      end
      object btnRead: TButton
        Left = 408
        Top = 380
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Read'
        TabOrder = 3
        OnClick = btnReadClick
      end
    end
    object tabPO: TTabSheet
      Caption = 'Compile PO'
      ImageIndex = 1
      DesignSize = (
        497
        417)
      object Label2: TLabel
        Left = 12
        Top = 6
        Width = 46
        Height = 13
        Caption = 'Filename:'
      end
      object edPOFile: TEdit
        Left = 12
        Top = 24
        Width = 451
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object btnPOOpen: TButton
        Left = 468
        Top = 24
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = btnPOOpenClick
      end
      object rePO: TRichEdit
        Left = 10
        Top = 54
        Width = 481
        Height = 313
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Compiles a PO file into MO format and displays the MO file here')
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
      object btnCompilePO: TButton
        Left = 402
        Top = 380
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Compile'
        TabOrder = 3
        OnClick = btnCompilePOClick
      end
    end
    object tabMerge: TTabSheet
      Caption = 'Merge PO'#39's'
      ImageIndex = 2
      DesignSize = (
        497
        417)
      object Label3: TLabel
        Left = 12
        Top = 6
        Width = 42
        Height = 13
        Caption = 'First PO:'
      end
      object Label4: TLabel
        Left = 12
        Top = 54
        Width = 56
        Height = 13
        Caption = 'Second PO:'
      end
      object edPO1: TEdit
        Left = 12
        Top = 24
        Width = 451
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object Button1: TButton
        Left = 468
        Top = 24
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = Button1Click
      end
      object edPO2: TEdit
        Left = 12
        Top = 72
        Width = 451
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object Button2: TButton
        Left = 468
        Top = 72
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 3
        OnClick = Button2Click
      end
      object reMerge: TRichEdit
        Left = 12
        Top = 108
        Width = 475
        Height = 257
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Merges two PO files and displays the result here')
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
      end
      object btnMerge: TButton
        Left = 400
        Top = 376
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Merge'
        TabOrder = 5
        OnClick = btnMergeClick
      end
    end
    object tabParse: TTabSheet
      Caption = 'Parse Files'
      ImageIndex = 3
      DesignSize = (
        497
        417)
      object Label5: TLabel
        Left = 12
        Top = 6
        Width = 55
        Height = 13
        Caption = 'Parse Files:'
      end
      object btnParseAdd: TButton
        Left = 12
        Top = 168
        Width = 75
        Height = 25
        Caption = '&Add...'
        TabOrder = 0
        OnClick = btnParseAddClick
      end
      object btnParse: TButton
        Left = 400
        Top = 376
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Parse'
        TabOrder = 1
        OnClick = btnParseClick
      end
      object reParsed: TRichEdit
        Left = 8
        Top = 208
        Width = 475
        Height = 159
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Displays the result from parsing the files added above')
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
      object reFilesToParse: TRichEdit
        Left = 8
        Top = 24
        Width = 475
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        Lines.Strings = (
          
            'Add filenames here, one per row that you want to parse for strin' +
            'gs')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 3
        WordWrap = False
      end
      object btnCancelParse: TButton
        Left = 16
        Top = 376
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Cancel'
        TabOrder = 4
        OnClick = btnCancelParseClick
      end
    end
  end
  object OpenMO: TOpenDialog
    Filter = 'MO files (*.mo)|*.mo|All files (*.*)|*.*'
    Left = 32
    Top = 136
  end
  object OpenPO: TOpenDialog
    Filter = 'PO files (*.po)|*.po|All files (*.*)|*.*'
    Left = 84
    Top = 136
  end
end
