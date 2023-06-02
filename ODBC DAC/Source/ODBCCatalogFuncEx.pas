
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCCatalogFuncEx                               }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCCatalogFuncEx;
{$I sv.inc}
interface
uses
  Classes,
  DB,

  odbcsqltypes,
  odbcsql,

  ODBCStmt,
  ODBCCustomDataSet,
  ODBCQueryEx;

type

  { TODBCCatalogStatementEx }

  TODBCCatalogStatementEx = class(TODBCStatementEx)
  protected
    procedure DoPrepare; override;
    procedure DoUnprepare; override;

  public
    procedure SQLTables(const CatalogName, SchemaName, TableName, TableType: string);
    procedure SQLColumns(const CatalogName, SchemaName, TableName, ColumnName: string);
    procedure SQLStatistics(const CatalogName, SchemaName, TableName: string; Unique, Reserved: SQLUSMALLINT);
    procedure SQLSpecialColumns(IdentifierType: SQLSMALLINT; const CatalogName, SchemaName, TableName: string; Scope, Nullable: SQLSMALLINT);
    procedure SQLPrimaryKeys(const CatalogName, SchemaName, TableName: string);
    procedure SQLForeignKeys(const PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName: string);
    procedure SQLTablePrivileges(const CatalogName, SchemaName, TableName: string);
    procedure SQLColumnPrivileges(const CatalogName, SchemaName, TableName, ColumnName: string);
    procedure SQLProcedures(const CatalogName, SchemaName, ProcName: string);
    procedure SQLProcedureColumns(const CatalogName, SchemaName, ProcName, ColumnName: string);
    procedure SQLGetTypeInfo(DataType: SQLSMALLINT);
  end;


  { TODBCCatalogFuncEx }

  TODBCCatalogFuncEx = class(TODBCCustomDataSet)
  private
    function GetConnectionHandle: SQLHDBC;
    procedure SetConnectionHandle(Value: SQLHDBC);

  protected
    function GetStatementClass: TODBCCustomStatementClass; override;
    procedure InternalExecute; override;

  public
    destructor Destroy; override;

    procedure SQLTables(const CatalogName, SchemaName, TableName, TableType: string);
    procedure SQLColumns(const CatalogName, SchemaName, TableName, ColumnName: string);
    procedure SQLStatistics(const CatalogName, SchemaName, TableName: string; Unique, Reserved: SQLUSMALLINT);
    procedure SQLSpecialColumns(IdentifierType: SQLSMALLINT; const CatalogName, SchemaName, TableName: string; Scope, Nullable: SQLSMALLINT);
    procedure SQLPrimaryKeys(const CatalogName, SchemaName, TableName: string);
    procedure SQLForeignKeys(const PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName: string);
    procedure SQLTablePrivileges(const CatalogName, SchemaName, TableName: string);
    procedure SQLColumnPrivileges(const CatalogName, SchemaName, TableName, ColumnName: string);
    procedure SQLProcedures(const CatalogName, SchemaName, ProcName: string);
    procedure SQLProcedureColumns(const CatalogName, SchemaName, ProcName, ColumnName: string);
    procedure SQLGetTypeInfo(DataType: SQLSMALLINT);

    property ConnectionHandle: SQLHDBC read GetConnectionHandle write SetConnectionHandle;
  end;


implementation
uses
  ODBCIntf,
  ODBCUtils,
  ODBCConsts;


{ TODBCCatalogStatementEx }

procedure TODBCCatalogStatementEx.DoPrepare;
begin
  AllocateHandle;
end;

procedure TODBCCatalogStatementEx.DoUnprepare;
begin
  FreeHandle;
end;

procedure TODBCCatalogStatementEx.SQLTables(const CatalogName, SchemaName, TableName, TableType: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLTables(Handle, PSQLCHAR(CatalogName), Length(CatalogName) {SQL_NTS}, PSQLCHAR(SchemaName), Length(SchemaName) {SQL_NTS}, PSQLCHAR(TableName), Length(TableName) {SQL_NTS}, PSQLCHAR(TableType), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLColumns(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLColumns(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, PSQLCHAR(ColumnName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLStatistics(const CatalogName, SchemaName, TableName: string; Unique, Reserved: SQLUSMALLINT);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLStatistics(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, Unique, Reserved));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLSpecialColumns(IdentifierType: SQLSMALLINT; const CatalogName, SchemaName, TableName: string; Scope, Nullable: SQLSMALLINT);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLSpecialColumns(Handle, IdentifierType, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, Scope, Nullable));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLPrimaryKeys(const CatalogName, SchemaName, TableName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLPrimaryKeys(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLForeignKeys(const PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLForeignKeys(Handle, PSQLCHAR(PKCatalogName), SQL_NTS, PSQLCHAR(PKSchemaName), SQL_NTS, PSQLCHAR(PKTableName), SQL_NTS, PSQLCHAR(FKCatalogName), SQL_NTS, PSQLCHAR(FKSchemaName), SQL_NTS, PSQLCHAR(FKTableName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLTablePrivileges(const CatalogName, SchemaName, TableName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLTablePrivileges(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLColumnPrivileges(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLColumnPrivileges(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, PSQLCHAR(ColumnName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLProcedures(const CatalogName, SchemaName, ProcName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLProcedures(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(ProcName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLProcedureColumns(const CatalogName, SchemaName, ProcName, ColumnName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLProcedureColumns(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(ProcName), SQL_NTS, PSQLCHAR(ColumnName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatementEx.SQLGetTypeInfo(DataType: SQLSMALLINT);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLGetTypeInfo(Handle, DataType));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;


{ TODBCCatalogFuncEx }

destructor TODBCCatalogFuncEx.Destroy;
begin
  Close;
  ConnectionHandle := nil;

  inherited;
end;

function TODBCCatalogFuncEx.GetConnectionHandle: SQLHDBC;
begin
  Result := (QStmt as TODBCCatalogStatementEx).ConnectionHandle;
end;

procedure TODBCCatalogFuncEx.SetConnectionHandle(Value: SQLHDBC);
begin
  CheckInactive;
  (QStmt as TODBCCatalogStatementEx).ConnectionHandle := Value;
end;

function TODBCCatalogFuncEx.GetStatementClass: TODBCCustomStatementClass;
begin
  Result := TODBCCatalogStatementEx;
end;

procedure TODBCCatalogFuncEx.InternalExecute;
begin
  // do nothing !!!
end;

procedure TODBCCatalogFuncEx.SQLTables(const CatalogName, SchemaName, TableName, TableType: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLTables(CatalogName, SchemaName, TableName, TableType);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLColumns(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLColumns(CatalogName, SchemaName, TableName, ColumnName);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLStatistics(const CatalogName, SchemaName, TableName: string; Unique, Reserved: SQLUSMALLINT);
begin
  (QStmt as TODBCCatalogStatementEx).SQLStatistics(CatalogName, SchemaName, TableName, Unique, Reserved);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLSpecialColumns(IdentifierType: SQLSMALLINT; const CatalogName, SchemaName, TableName: string; Scope, Nullable: SQLSMALLINT);
begin
  (QStmt as TODBCCatalogStatementEx).SQLSpecialColumns(IdentifierType, CatalogName, SchemaName, TableName, Scope, Nullable);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLPrimaryKeys(const CatalogName, SchemaName, TableName: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLPrimaryKeys(CatalogName, SchemaName, TableName);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLForeignKeys(const PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLForeignKeys(PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLTablePrivileges(const CatalogName, SchemaName, TableName: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLTablePrivileges(CatalogName, SchemaName, TableName);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLColumnPrivileges(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLColumnPrivileges(CatalogName, SchemaName, TableName, ColumnName);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLProcedures(const CatalogName, SchemaName, ProcName: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLProcedures(CatalogName, SchemaName, ProcName);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLProcedureColumns(const CatalogName, SchemaName, ProcName, ColumnName: string);
begin
  (QStmt as TODBCCatalogStatementEx).SQLProcedureColumns(CatalogName, SchemaName, ProcName, ColumnName);
  Open;
end;

procedure TODBCCatalogFuncEx.SQLGetTypeInfo(DataType: SQLSMALLINT);
begin
  (QStmt as TODBCCatalogStatementEx).SQLGetTypeInfo(DataType);
  Open;
end;


end.
