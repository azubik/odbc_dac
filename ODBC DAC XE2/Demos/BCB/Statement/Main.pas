
{*******************************************************}
{                                                       }
{       ODBC DAC - Statement Demo                       }
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
  Windows, SysUtils, Classes, Controls, Forms,
  ExtCtrls, DBCtrls, Grids, DBGrids, StdCtrls, Db,

  ODBCConnection,
  ODBCCustomDataset,
  ODBCTable,
  ODBCStmt,
  ODBCQuery;

type
  TfrmMain = class(TForm)
    btnConnect: TButton;
    btnCreateTable: TButton;
    btnPopulateTable: TButton;
    DBGrid1: TDBGrid;
    btnOpen: TButton;
    plNavigator: TPanel;
    DBNavigator1: TDBNavigator;
    ODBCConnection: TODBCConnection;
    ODBCStatement: TODBCStatement;
    ODBCTable: TODBCTable;
    DataSource: TDataSource;
    btnDropTable: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ODBCConnectionAfterConnect(Sender: TObject);
    procedure ODBCConnectionAfterDisconnect(Sender: TObject);

    procedure btnConnectClick(Sender: TObject);
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnPopulateTableClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnDropTableClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation
uses
  Dialogs,
  odbcsqlext, odbcsql;

{$R *.DFM}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ODBCConnection.DriverCompletion := dcPrompt;
end;

procedure TfrmMain.ODBCConnectionAfterConnect(Sender: TObject);
begin
  btnConnect.Caption := 'DISCONNECT FROM DATABASE'
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
  CR_TABLE = 'CREATE TABLE stmt_test (c1 integer not null PRIMARY KEY, c2 float, c3 char(5))';
begin
  // CREATE TABLE
  ODBCStatement.SQL.Text := CR_TABLE;
  ODBCStatement.Execute;

  MessageDlg('TABLE stmt_test was created successfully', mtInformation, [mbOK], 0);
end;

procedure TfrmMain.btnPopulateTableClick(Sender: TObject);
const
  PL_TABLE = 'INSERT INTO stmt_test VALUES(:p1, :p2, :p3)';
var
  i: Integer;
begin
  ODBCStatement.SQL.Text := PL_TABLE;

  ODBCStatement.Params[0].ParamType := ptInput;
  ODBCStatement.Params[1].ParamType := ptInput;
  ODBCStatement.Params[2].ParamType := ptInput;

  ODBCStatement.Params[0].SQLType := SQL_INTEGER;
  ODBCStatement.Params[0].ColumnSize := 10;
  ODBCStatement.Params[0].DecimalDigits := 0;

  ODBCStatement.Params[1].SQLType := SQL_DOUBLE;
  ODBCStatement.Params[1].ColumnSize := 15;
  ODBCStatement.Params[1].DecimalDigits := 0;

  ODBCStatement.Params[2].SQLType := SQL_CHAR;
  ODBCStatement.Params[2].ColumnSize := 5;
  ODBCStatement.Params[2].DecimalDigits := 0;

  for i := 0 to 4 do
  begin
    // init parameter values
    ODBCStatement.Params.ParamByName('p1').AsInteger := i;
    ODBCStatement.Params.ParamByName('p2').AsFloat := i + 0.007;
    ODBCStatement.Params.ParamByName('p3').AsString := 'test' + IntToStr(i);

    ODBCStatement.Execute;
  end;

  MessageDlg('TABLE stmt_test was populated successfully', mtInformation, [mbOK], 0);
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  ODBCTable.Active := not ODBCTable.Active;

  if ODBCTable.Active then
    btnOpen.Caption := 'CLOSE TABLE'
  else
    btnOpen.Caption := 'OPEN TABLE';
end;

procedure TfrmMain.btnDropTableClick(Sender: TObject);
const
  DR_TABLE = 'DROP TABLE stmt_test';
begin
  if ODBCTable.Active then
    DatabaseError('Cannot perform this operation on active table');

  ODBCStatement.SQL.Text := DR_TABLE;
  ODBCStatement.Execute;

  MessageDlg('TABLE stmt_test was dropped successfully', mtInformation, [mbOK], 0);
end;

end.

