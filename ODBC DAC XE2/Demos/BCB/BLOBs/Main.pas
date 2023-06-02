
{*******************************************************}
{                                                       }
{       ODBC DAC - Blob Demo                            }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit Main;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs,
  ExtCtrls, DBCtrls, Grids, DBGrids, StdCtrls,
  Db,
  ODBCConnection,
  ODBCCustomDataset,
  ODBCTable,
  ODBCStmt,
  ODBCQuery;

//{$DEFINE MySQL}
//{$DEFINE MSSQL}
//{$DEFINE INFORMIX}
//{$DEFINE ORACLE}
//{$DEFINE SAPDB}

{$DEFINE DB2}


type
  TfrmMain = class(TForm)
    btnConnect: TButton;
    btnCreateTable: TButton;
    btnPopulateTable: TButton;
    btnOpen: TButton;
    plNavigator: TPanel;
    DBNavigator1: TDBNavigator;
    ODBCConnection: TODBCConnection;
    ODBCTable: TODBCTable;
    DataSource: TDataSource;
    ODBCQuery: TODBCQuery;
    DBGrid1: TDBGrid;
    DBMemo1: TDBMemo;
    DBImage1: TDBImage;
    plMiddle: TPanel;
    btnLoadText: TButton;
    btnSaveText: TButton;
    btnLoadImage: TButton;
    btnSaveImage: TButton;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    btnDropTable: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ODBCConnectionAfterConnect(Sender: TObject);
    procedure ODBCConnectionAfterDisconnect(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnPopulateTableClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnLoadTextClick(Sender: TObject);
    procedure btnSaveTextClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure btnDropTableClick(Sender: TObject);

  private
    procedure CheckActive;

  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  odbcsqltypes, odbcsql, odbcsqlext;

{$R *.DFM}


procedure TfrmMain.CheckActive;
begin
  if not ODBCTable.Active then
    DatabaseError('Cannot perform this operation on closed table.')
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ODBCConnection.DriverCompletion := dcPrompt;
end;

procedure TfrmMain.ODBCConnectionAfterConnect(Sender: TObject);
begin
  btnConnect.Caption := 'DISCONNECT FROM DATABASE';
  ODBCConnection.SetAttribute(SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER);
end;

procedure TfrmMain.ODBCConnectionAfterDisconnect(Sender: TObject);
begin
  btnConnect.Caption := 'CONNECT TO DATABASE';
end;

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  ODBCConnection.Connected := not ODBCConnection.Connected;
end;

procedure TfrmMain.btnCreateTableClick(Sender: TObject);
const
{$IFDEF MySQL}
  CR_TABLE = 'CREATE TABLE blob_test (c1 integer PRIMARY KEY, c2 float, c3 char(5), c4 text, c5 blob)';
{$ENDIF}

{$IFDEF MSSQL}
  CR_TABLE = 'CREATE TABLE blob_test (c1 integer PRIMARY KEY, c2 float, c3 char(5), c4 text, c5 image)';
{$ENDIF}

{$IFDEF ORACLE}
  CR_TABLE = 'CREATE TABLE blob_test (c1 integer PRIMARY KEY, c2 float, c3 char(5), c4 clob, c5 blob)';
{$ENDIF}

{$IFDEF SAPDB}
  CR_TABLE = 'CREATE TABLE blob_test (c1 integer PRIMARY KEY, c2 float, c3 char(5), c4 long, c5 long byte)';
{$ENDIF}

{$IFDEF INFORMIX}
  CR_TABLE = 'CREATE TABLE blob_test (c1 integer PRIMARY KEY, c2 float, c3 char(5), c4 text, c5 byte)';
{$ENDIF}

{$IFDEF DB2}
  CR_TABLE = 'CREATE TABLE blob_test (c1 integer not null PRIMARY KEY, c2 float, c3 char(5), c4 clob, c5 blob)';
{$ENDIF}

begin
  // CREATE TABLE
  ODBCQuery.SQL.Text := CR_TABLE;
  ODBCQuery.ExecSQL;

  MessageDlg('TABLE blob_test was created successfully', mtInformation, [mbOK], 0);
end;

procedure TfrmMain.btnPopulateTableClick(Sender: TObject);
const
  PL_TABLE = 'INSERT INTO blob_test VALUES(:p1, :p2, :p3, :p4, :p5)';
var
  i: Integer;
begin
  ODBCQuery.SQL.Text := PL_TABLE;

  // init parameter type
  ODBCQuery.Params.ParamByName('p1').ParamType := ptInput;
  ODBCQuery.Params.ParamByName('p2').ParamType := ptInput;
  ODBCQuery.Params.ParamByName('p3').ParamType := ptInput;
  ODBCQuery.Params.ParamByName('p4').ParamType := ptInput;
  ODBCQuery.Params.ParamByName('p5').ParamType := ptInput;

  ODBCQuery.Params.ParamByName('p1').SQLType := SQL_INTEGER;
  ODBCQuery.Params.ParamByName('p2').SQLType := SQL_DOUBLE;
  ODBCQuery.Params.ParamByName('p3').SQLType := SQL_CHAR;
  ODBCQuery.Params.ParamByName('p4').SQLType := SQL_LONGVARCHAR;
  ODBCQuery.Params.ParamByName('p5').SQLType := SQL_LONGVARBINARY;

  ODBCQuery.Params.ParamByName('p1').ColumnSize := 10;
  ODBCQuery.Params.ParamByName('p2').ColumnSize := 15;
  ODBCQuery.Params.ParamByName('p3').ColumnSize := 5;

  for i := 1 to 5 do
  begin
    // init parameter value
    ODBCQuery.Params.ParamByName('p1').AsInteger := i;
    ODBCQuery.Params.ParamByName('p2').AsFloat := i + 0.007;

    ODBCQuery.Params.ParamByName('p3').AsString := 'test' + IntToStr(i);

    ODBCQuery.Params.ParamByName('p4').LoadFromFile('texts\t' + IntToStr(i) + '.txt', ftMemo);
    ODBCQuery.Params.ParamByName('p5').LoadFromFile('images\i' + IntToStr(i) + '.bmp', ftGraphic);

    ODBCQuery.ExecSQL;
  end;

  MessageDlg('TABLE blob_test was populated successfully', mtInformation, [mbOK], 0);
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  ODBCTable.Active := not ODBCTable.Active;

  if ODBCTable.Active then
    btnOpen.Caption := 'CLOSE TABLE'
  else
    btnOpen.Caption := 'OPEN TABLE';
end;

procedure TfrmMain.btnLoadTextClick(Sender: TObject);
begin
  CheckActive;

  if OpenDialog.Execute then
    TMemoField(ODBCTable.FieldByName('c4')).LoadFromFile(OpenDialog.FileName);
end;

procedure TfrmMain.btnSaveTextClick(Sender: TObject);
begin
  CheckActive;

  if SaveDialog.Execute then
    TMemoField(ODBCTable.FieldByName('c4')).SaveToFile(SaveDialog.FileName);
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  CheckActive;

  if OpenDialog.Execute then
    TGraphicField(ODBCTable.FieldByName('c5')).LoadFromFile(OpenDialog.FileName);
end;

procedure TfrmMain.btnSaveImageClick(Sender: TObject);
begin
  CheckActive;

  if SaveDialog.Execute then
    TGraphicField(ODBCTable.FieldByName('c5')).SaveToFile(SaveDialog.FileName);
end;

procedure TfrmMain.btnDropTableClick(Sender: TObject);
const
  DR_TABLE = 'DROP TABLE blob_test';
begin
  if ODBCTable.Active then
    DatabaseError('Cannot perform this operation on active table');

  // DROP TABLE
  ODBCQuery.SQL.Text := DR_TABLE;
  ODBCQuery.ExecSQL;

  MessageDlg('TABLE blob_test was dropped successfully', mtInformation, [mbOK], 0);
end;

end.

