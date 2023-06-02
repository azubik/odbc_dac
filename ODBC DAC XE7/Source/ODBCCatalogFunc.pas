
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
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLTablesA(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(SchemaName), SQL_NTS, PSQLCHAR(AnsiString(TableName)), SQL_NTS, PSQLCHAR(AnsiString(TableType)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLTablesW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(TableName), SQL_NTS, PSQLWCHAR(TableType), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLColumns(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLColumnsA(Handle, PSQLCHAR(AnsiString(CatalogName)), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(TableName)), SQL_NTS, PSQLCHAR(AnsiString(ColumnName)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLColumnsW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(TableName), SQL_NTS, PSQLWCHAR(ColumnName), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLStatistics(const CatalogName, SchemaName, TableName: string; Unique, Reserved: SQLUSMALLINT);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLStatisticsA(Handle, PSQLCHAR(CatalogName), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(TableName)), SQL_NTS, Unique, Reserved));
{$ELSE}
  CheckError(ODBCIntf.SQLStatisticsW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(TableName), SQL_NTS, Unique, Reserved));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLSpecialColumns(IdentifierType: SQLSMALLINT; const CatalogName, SchemaName, TableName: string; Scope, Nullable: SQLSMALLINT);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLSpecialColumnsA(Handle, IdentifierType, PSQLCHAR(AnsiString(CatalogName)), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(TableName)), SQL_NTS, Scope, Nullable));
{$ELSE}
  CheckError(ODBCIntf.SQLSpecialColumnsW(Handle, IdentifierType, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(TableName), SQL_NTS, Scope, Nullable));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLPrimaryKeys(const CatalogName, SchemaName, TableName: string);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLPrimaryKeysA(Handle, PSQLCHAR(AnsiString(CatalogName)), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(TableName)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLPrimaryKeysW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(TableName), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLForeignKeys(const PKCatalogName, PKSchemaName, PKTableName, FKCatalogName, FKSchemaName, FKTableName: string);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLForeignKeysA(Handle, PSQLCHAR(AnsiString(PKCatalogName)), SQL_NTS, PSQLCHAR(AnsiString(PKSchemaName)), SQL_NTS, PSQLCHAR(AnsiString(PKTableName)), SQL_NTS, PSQLCHAR(AnsiString(FKCatalogName)), SQL_NTS, PSQLCHAR(AnsiString(FKSchemaName)), SQL_NTS, PSQLCHAR(AnsiString(FKTableName)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLForeignKeysW(Handle, PSQLWCHAR(PKCatalogName), SQL_NTS, PSQLWCHAR(PKSchemaName), SQL_NTS, PSQLWCHAR(PKTableName), SQL_NTS, PSQLWCHAR(FKCatalogName), SQL_NTS, PSQLWCHAR(FKSchemaName), SQL_NTS, PSQLWCHAR(FKTableName), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLTablePrivileges(const CatalogName, SchemaName, TableName: string);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLTablePrivilegesA(Handle, PSQLCHAR(AnsiString(CatalogName)), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(TableName)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLTablePrivilegesW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(TableName), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLColumnPrivileges(const CatalogName, SchemaName, TableName, ColumnName: string);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLColumnPrivilegesA(Handle, PSQLCHAR(AnsiString(CatalogName)), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(TableName)), SQL_NTS, PSQLCHAR(AnsiString(ColumnName)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLColumnPrivilegesW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(TableName), SQL_NTS, PSQLWCHAR(ColumnName), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLProcedures(const CatalogName, SchemaName, ProcName: string);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLProceduresA(Handle, PSQLCHAR(AnsiString(CatalogName)), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(ProcName)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLProceduresW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(ProcName), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLProcedureColumns(const CatalogName, SchemaName, ProcName, ColumnName: string);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLProcedureColumnsA(Handle, PSQLCHAR(AnsiString(CatalogName)), SQL_NTS, PSQLCHAR(AnsiString(SchemaName)), SQL_NTS, PSQLCHAR(AnsiString(ProcName)), SQL_NTS, PSQLCHAR(AnsiString(ColumnName)), SQL_NTS));
{$ELSE}
  CheckError(ODBCIntf.SQLProcedureColumnsW(Handle, PSQLWCHAR(CatalogName), SQL_NTS, PSQLWCHAR(SchemaName), SQL_NTS, PSQLWCHAR(ProcName), SQL_NTS, PSQLWCHAR(ColumnName), SQL_NTS));
{$ENDIF}

  // init Cursor
  Cursor.Init(Handle);

  if Cursor.Columns.Count > 0 then
    Cursor.Open;
end;

procedure TODBCCatalogStatement.SQLGetTypeInfo(DataType: SQLSMALLINT);
begin
  Prepare;
  // call
{$IFNDEF USE_UNICODE_DRIVER}
  CheckError(ODBCIntf.SQLGetTypeInfoA(Handle, DataType));
{$ELSE}
  CheckError(ODBCIntf.SQLGetTypeInfoW(Handle, DataType));
{$ENDIF}

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
