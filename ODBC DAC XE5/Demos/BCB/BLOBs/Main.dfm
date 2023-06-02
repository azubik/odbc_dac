object frmMain: TfrmMain
  Left = 208
  Top = 120
  BorderStyle = bsDialog
  Caption = 'Blobs demo (implemented using ODBC DAC)'
  ClientHeight = 453
  ClientWidth = 678
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
  object btnConnect: TButton
    Left = 8
    Top = 4
    Width = 213
    Height = 35
    Caption = 'CONNECT TO DATABASE'
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object btnCreateTable: TButton
    Left = 8
    Top = 46
    Width = 213
    Height = 35
    Caption = 'CREATE TABLE'
    TabOrder = 1
    OnClick = btnCreateTableClick
  end
  object btnPopulateTable: TButton
    Left = 8
    Top = 88
    Width = 213
    Height = 35
    Caption = 'POPULATE TABLE'
    TabOrder = 2
    OnClick = btnPopulateTableClick
  end
  object btnOpen: TButton
    Left = 8
    Top = 130
    Width = 213
    Height = 35
    Caption = 'OPEN TABLE'
    TabOrder = 3
    OnClick = btnOpenClick
  end
  object plNavigator: TPanel
    Left = 230
    Top = 1
    Width = 448
    Height = 209
    TabOrder = 6
    object DBNavigator1: TDBNavigator
      Left = 196
      Top = 6
      Width = 240
      Height = 25
      DataSource = DataSource
      TabOrder = 0
    end
    object DBGrid1: TDBGrid
      Left = 1
      Top = 36
      Width = 446
      Height = 172
      Align = alBottom
      DataSource = DataSource
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object DBMemo1: TDBMemo
    Left = 0
    Top = 253
    Width = 337
    Height = 200
    DataField = 'c4'
    DataSource = DataSource
    ScrollBars = ssBoth
    TabOrder = 7
    WordWrap = False
  end
  object DBImage1: TDBImage
    Left = 340
    Top = 253
    Width = 337
    Height = 200
    DataField = 'c5'
    DataSource = DataSource
    TabOrder = 8
  end
  object plMiddle: TPanel
    Left = 0
    Top = 210
    Width = 678
    Height = 43
    TabOrder = 5
    object btnLoadText: TButton
      Left = 8
      Top = 9
      Width = 150
      Height = 25
      Caption = 'Load text from file...'
      TabOrder = 0
      OnClick = btnLoadTextClick
    end
    object btnSaveText: TButton
      Left = 166
      Top = 9
      Width = 150
      Height = 25
      Caption = 'Save text to file ...'
      TabOrder = 1
      OnClick = btnSaveTextClick
    end
    object btnLoadImage: TButton
      Left = 361
      Top = 9
      Width = 150
      Height = 25
      Caption = 'Load image from file ...'
      TabOrder = 2
      OnClick = btnLoadImageClick
    end
    object btnSaveImage: TButton
      Left = 519
      Top = 9
      Width = 150
      Height = 25
      Caption = 'Save image to file ...'
      TabOrder = 3
      OnClick = btnSaveImageClick
    end
  end
  object btnDropTable: TButton
    Left = 8
    Top = 171
    Width = 213
    Height = 35
    Caption = 'DROP TABLE'
    TabOrder = 4
    OnClick = btnDropTableClick
  end
  object ODBCConnection: TODBCConnection
    Environment = Default.Owner
    LoginPrompt = False
    AfterConnect = ODBCConnectionAfterConnect
    AfterDisconnect = ODBCConnectionAfterDisconnect
    Left = 48
    Top = 366
  end
  object ODBCTable: TODBCTable
    Connection = ODBCConnection
    BufferChunks = 32
    CachedUpdates = False
    TableName = 'BLOB_TEST'
    TableType = ttTable
    Left = 90
    Top = 366
  end
  object DataSource: TDataSource
    DataSet = ODBCTable
    Left = 124
    Top = 366
  end
  object ODBCQuery: TODBCQuery
    Connection = ODBCConnection
    BufferChunks = 32
    CachedUpdates = False
    ParamCheck = True
    Left = 92
    Top = 334
  end
  object SaveDialog: TSaveDialog
    Left = 258
    Top = 304
  end
  object OpenDialog: TOpenDialog
    Left = 222
    Top = 304
  end
end
