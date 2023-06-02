
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCCatalogFunc                                 }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCCatalogFunc;
{$I sv.inc}
interface
uses
  Classes, DB,
  odbcsqltypes, odbcsql,
  ODBCConnection, ODBCStmt, ODBCQuery, ODBCCustomDataset;

type

  { TODBCCatalogStatement }

  TODBCCatalogStatement = class(TODBCStatement)
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


  { TODBCCatalogFunc }

  TODBCCatalogFunc = class(TODBCCustomDataSetEx)
  private

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

  end;


implementation
uses
  ODBCIntf,
  ODBCUtils,
  ODBCConsts;


{ TODBCCatalogStatement }

procedure TODBCCatalogStatement.DoPrepare;
begin
  AllocateHandle;
end;

procedure TODBCCatalogStatement.DoUnprepare;
begin
  FreeHandle;
end;

procedure TODBCCatalogStatement.SQLTables(const CatalogName, SchemaName, TableName, TableType: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLTables(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, PSQLCHAR(TableType), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLColumns(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLColumns(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, PSQLCHAR(ColumnName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLStatistics(const CatalogName, SchemaName, TableName: string; Unique, Reserved: SQLUSMALLINT);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLStatistics(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, Unique, Reserved));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLSpecialColumns(IdentifierType: SQLSMALLINT; const CatalogName, SchemaName, TableName: string; Scope, Nullable: SQLSMALLINT);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLSpecialColumns(Handle, IdentifierType, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, Scope, Nullable));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLPrimaryKeys(const CatalogName, SchemaName, TableName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLPrimaryKeys(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLForeignKeys(const PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLForeignKeys(Handle, PSQLCHAR(PKCatalogName), SQL_NTS, PSQLCHAR(PKSchemaName), SQL_NTS, PSQLCHAR(PKTableName), SQL_NTS, PSQLCHAR(FKCatalogName), SQL_NTS, PSQLCHAR(FKSchemaName), SQL_NTS, PSQLCHAR(FKTableName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLTablePrivileges(const CatalogName, SchemaName, TableName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLTablePrivileges(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLColumnPrivileges(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLColumnPrivileges(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(TableName), SQL_NTS, PSQLCHAR(ColumnName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLProcedures(const CatalogName, SchemaName, ProcName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLProcedures(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(ProcName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLProcedureColumns(const CatalogName, SchemaName, ProcName, ColumnName: string);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLProcedureColumns(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(ProcName), SQL_NTS, PSQLCHAR(ColumnName), SQL_NTS));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLGetTypeInfo(DataType: SQLSMALLINT);
begin
  Prepare;
  // call
  CheckError(ODBCIntf.SQLGetTypeInfo(Handle, DataType));

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;


{ TODBCCatalogFunc }

destructor TODBCCatalogFunc.Destroy;
begin
  Close;
  Connection := nil;
  inherited;
end;

function TODBCCatalogFunc.GetStatementClass: TODBCCustomStatementClass;
begin
  Result := TODBCCatalogStatement;
end;

procedure TODBCCatalogFunc.InternalExecute;
begin
  // do nothing !!!
end;

procedure TODBCCatalogFunc.SQLTables(const CatalogName, SchemaName, TableName, TableType: string);
begin
  (QStmt as TODBCCatalogStatement).SQLTables(CatalogName, SchemaName, TableName, TableType);
  Open;
end;

procedure TODBCCatalogFunc.SQLColumns(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  (QStmt as TODBCCatalogStatement).SQLColumns(CatalogName, SchemaName, TableName, ColumnName);
  Open;
end;

procedure TODBCCatalogFunc.SQLStatistics(const CatalogName, SchemaName, TableName: string; Unique, Reserved: SQLUSMALLINT);
begin
  (QStmt as TODBCCatalogStatement).SQLStatistics(CatalogName, SchemaName, TableName, Unique, Reserved);
  Open;
end;

procedure TODBCCatalogFunc.SQLSpecialColumns(IdentifierType: SQLSMALLINT; const CatalogName, SchemaName, TableName: string; Scope, Nullable: SQLSMALLINT);
begin
  (QStmt as TODBCCatalogStatement).SQLSpecialColumns(IdentifierType, CatalogName, SchemaName, TableName, Scope, Nullable);
  Open;
end;

procedure TODBCCatalogFunc.SQLPrimaryKeys(const CatalogName, SchemaName, TableName: string);
begin
  (QStmt as TODBCCatalogStatement).SQLPrimaryKeys(CatalogName, SchemaName, TableName);
  Open;
end;

procedure TODBCCatalogFunc.SQLForeignKeys(const PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName: string);
begin
  (QStmt as TODBCCatalogStatement).SQLForeignKeys(PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName);
  Open;
end;

procedure TODBCCatalogFunc.SQLTablePrivileges(const CatalogName, SchemaName, TableName: string);
begin
  (QStmt as TODBCCatalogStatement).SQLTablePrivileges(CatalogName, SchemaName, TableName);
  Open;
end;

procedure TODBCCatalogFunc.SQLColumnPrivileges(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  (QStmt as TODBCCatalogStatement).SQLColumnPrivileges(CatalogName, SchemaName, TableName, ColumnName);
  Open;
end;

procedure TODBCCatalogFunc.SQLProcedures(const CatalogName, SchemaName, ProcName: string);
begin
  (QStmt as TODBCCatalogStatement).SQLProcedures(CatalogName, SchemaName, ProcName);
  Open;
end;

procedure TODBCCatalogFunc.SQLProcedureColumns(const CatalogName, SchemaName, ProcName, ColumnName: string);
begin
  (QStmt as TODBCCatalogStatement).SQLProcedureColumns(CatalogName, SchemaName, ProcName, ColumnName);
  Open;
end;

procedure TODBCCatalogFunc.SQLGetTypeInfo(DataType: SQLSMALLINT);
begin
  (QStmt as TODBCCatalogStatement).SQLGetTypeInfo(DataType);
  Open;
end;


end.
