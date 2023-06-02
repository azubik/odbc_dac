object Form1: TForm1
  Left = 206
  Top = 107
  Width = 682
  Height = 480
  Caption = 'TODBCTable component Demo - Softvector.com'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 185
    Top = 41
    Width = 3
    Height = 393
    Cursor = crHSplit
  end
  object plTop: TPanel
    Left = 0
    Top = 0
    Width = 674
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      674
      41)
    object DBNavigator: TDBNavigator
      Left = 418
      Top = 8
      Width = 240
      Height = 25
      DataSource = DataSource
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object btnConnect: TButton
      Left = 16
      Top = 8
      Width = 150
      Height = 25
      Caption = '&Connect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 171
      Top = 8
      Width = 150
      Height = 25
      Caption = '&Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = btnDisconnectClick
    end
  end
  object PageControl: TPageControl
    Left = 188
    Top = 41
    Width = 486
    Height = 393
    ActivePage = TabSheet
    Align = alClient
    TabIndex = 0
    TabOrder = 1
    object TabSheet: TTabSheet
      Caption = 'Data'
      object dbgData: TDBGrid
        Left = 0
        Top = 0
        Width = 478
        Height = 365
        Align = alClient
        DataSource = DataSource
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
  end
  object plLeft: TPanel
    Left = 0
    Top = 41
    Width = 185
    Height = 393
    Align = alLeft
    TabOrder = 2
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 183
      Height = 24
      Align = alTop
      AutoSize = False
      Caption = '  Tables'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object TreeView: TTreeView
      Left = 1
      Top = 25
      Width = 183
      Height = 367
      Align = alClient
      Constraints.MinWidth = 30
      Indent = 19
      ReadOnly = True
      ShowButtons = False
      ShowLines = False
      TabOrder = 0
      OnChange = TreeViewChange
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 674
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object DataSource: TDataSource
    DataSet = ODBCTable
    Left = 472
    Top = 160
  end
  object ODBCConnection: TODBCConnection
    Environment = Default.Owner
    Left = 400
    Top = 160
  end
  object ODBCTable: TODBCTable
    Connection = ODBCConnection
    BufferChunks = 32
    CachedUpdates = False
    TableType = ttTable
    Left = 440
    Top = 160
  end
end
