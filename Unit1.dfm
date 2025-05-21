object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 619
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 250
    Top = 0
    Height = 619
    ExplicitLeft = 112
    ExplicitTop = 272
    ExplicitHeight = 100
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 250
    Height = 619
    Align = alLeft
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnClick = TreeView1Click
  end
  object Memo1: TMemo
    Left = 253
    Top = 0
    Width = 547
    Height = 619
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnChange = Memo1Change
  end
  object PanelDetailView: TPanel
    Left = 253
    Top = 0
    Width = 547
    Height = 619
    Align = alClient
    TabOrder = 2
    Visible = False
    object LabelSelectNode: TLabel
      Left = 26
      Top = 16
      Width = 248
      Height = 15
      Caption = 'Select a node from the tree to view/edit details.'
      Visible = False
    end
    object GroupBoxAttributes: TGroupBox
      Left = 24
      Top = 16
      Width = 481
      Height = 201
      Caption = 'Node Attributes'
      TabOrder = 0
      object StringGridAttributes: TStringGrid
        Left = 3
        Top = 64
        Width = 475
        Height = 134
        Align = alCustom
        ColCount = 2
        DefaultColWidth = 237
        RowCount = 2
        ScrollBars = ssHorizontal
        TabOrder = 0
      end
      object btnAddAttribute: TButton
        Left = 241
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 1
      end
      object btnDeleteAttribute: TButton
        Left = 403
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 2
      end
      object btnEditAttribute: TButton
        Left = 322
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 3
      end
    end
    object GroupBoxText: TGroupBox
      Left = 24
      Top = 239
      Width = 481
      Height = 123
      Caption = 'Node Text'
      TabOrder = 1
      object MemoNodeText: TMemo
        Left = 2
        Top = 17
        Width = 476
        Height = 48
        Lines.Strings = (
          'MemoNodeText')
        TabOrder = 0
      end
      object btnEditNodeText: TButton
        Left = 322
        Top = 82
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 1
      end
      object btnDeleteNodeText: TButton
        Left = 403
        Top = 82
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 2
      end
    end
    object GroupBoxChildren: TGroupBox
      Left = 26
      Top = 384
      Width = 481
      Height = 217
      Caption = 'Node Children'
      TabOrder = 2
      Visible = False
      object ListBoxChildren: TListBox
        Left = 3
        Top = 24
        Width = 150
        Height = 174
        ItemHeight = 15
        TabOrder = 0
      end
      object BtnAddChild: TButton
        Left = 241
        Top = 184
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 1
      end
      object BtnEditChild: TButton
        Left = 322
        Top = 184
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 2
      end
      object BtnDeleteChild: TButton
        Left = 403
        Top = 184
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 3
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 744
    Top = 8
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
        ShortCut = 16462
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save As...'
        ShortCut = 24659
        OnClick = SaveAs1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object ToggleRawView1: TMenuItem
        Caption = 'Toggle Raw View'
        OnClick = ToggleRawView1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = #39'.xml'#39
    Filter = #39'XML files (*.xml)|*.xml|All files (*.*)|*.*'#39
    Left = 744
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = #39'.xml'#39
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'#39
    Left = 744
    Top = 128
  end
end
