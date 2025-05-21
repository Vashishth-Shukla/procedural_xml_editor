object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
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
    Left = 121
    Top = 0
    Height = 441
    ExplicitLeft = 112
    ExplicitTop = 272
    ExplicitHeight = 100
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 441
    Align = alLeft
    Indent = 19
    TabOrder = 0
    ExplicitLeft = 48
    ExplicitTop = 120
    ExplicitHeight = 97
  end
  object Memo1: TMemo
    Left = 124
    Top = 0
    Width = 500
    Height = 441
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnChange = Memo1Change
    ExplicitLeft = 121
    ExplicitWidth = 503
  end
  object MainMenu1: TMainMenu
    Left = 584
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
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = #39'.xml'#39
    Filter = #39'XML files (*.xml)|*.xml|All files (*.*)|*.*'#39
    Left = 584
    Top = 56
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = #39'.xml'#39
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'#39
    Left = 584
    Top = 112
  end
end
