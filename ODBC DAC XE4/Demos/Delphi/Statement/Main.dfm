object frmMain: TfrmMain
  Left = 275
  Top = 108
  BorderStyle = bsDialog
  Caption = 'ODBC statement  demo (implemented using ODBC DAC)'
  ClientHeight = 453
  ClientWidth = 443
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
    Left = 34
    Top = 8
    Width = 375
    Height = 35
    Caption = 'CONNECT TO DATABASE'
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object btnCreateTable: TButton
    Left = 34
    Top = 50
    Width = 375
    Height = 35
    Caption = 'CREATE TABLE'
    TabOrder = 1
    OnClick = btnCreateTableClick
  end
  object btnPopulateTable: TButton
    Left = 34
    Top = 92
    Width = 375
    Height = 35
    Caption = 'POPULATE TABLE'
    TabOrder = 2
    OnClick = btnPopulateTableClick
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 260
    Width = 443
    Height = 193
    Align = alBottom
    DataSource = DataSource
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object btnOpen: TButton
    Left = 34
    Top = 134
    Width = 375
    Height = 35
    Caption = 'OPEN TABLE'
    TabOrder = 3
    OnClick = btnOpenClick
  end
  object plNavigator: TPanel
    Left = 0
    Top = 224
    Width = 443
    Height = 36
    Align = alBottom
    TabOrder = 4
    object DBNavigator1: TDBNavigator
      Left = 196
      Top = 6
      Width = 240
      Height = 25
      DataSource = DataSource
      TabOrder = 0
    end
  end
  object btnDropTable: TButton
    Left = 34
    Top = 175
    Width = 375
    Height = 35
    Caption = 'DROP TABLE'
    TabOrder = 6
    OnClick = btnDropTableClick
  end
  object ODBCConnection: TODBCConnection
    Environment = Default.Owner
    AfterConnect = ODBCConnectionAfterConnect
    AfterDisconnect = ODBCConnectionAfterDisconnect
    Left = 48
    Top = 326
  end
  object ODBCStatement: TODBCStatement
    Connection = ODBCConnection
    ParamCheck = True
    Left = 130
    Top = 326
  end
  object ODBCTable: TODBCTable
    Connection = ODBCConnection
    BufferChunks = 32
    CachedUpdates = False
    TableName = 'stmt_test'
    TableType = ttTable
    Left = 50
    Top = 396
  end
  object DataSource: TDataSource
    DataSet = ODBCTable
    Left = 204
    Top = 326
  end
end
