
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCIntf                                        }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCIntf;
{$I sv.inc}
interface
uses
{$IFDEF WIN32}
  Windows,
{$ENDIF}
  odbcsql,
{$IFDEF USE_UNICODE_DRIVER}
  odbcsqlucode,
{$ENDIF}
  odbcsqlext;


const
{$IFDEF WIN32}
  ODBCLibraryName = 'odbc32.dll';                 { do not localize }
{$ENDIF}
{$IFDEF LINUX}
  ODBCLibraryName = '*******.so';                 {do not localize}
{$ENDIF}


var
  // sql.h
  SQLAllocConnect: TSQLAllocConnect;
  SQLAllocEnv: TSQLAllocEnv;
  SQLAllocHandle: TSQLAllocHandle;
  SQLAllocStmt: TSQLAllocStmt;
  SQLBindCol: TSQLBindCol;
  SQLBindParam: TSQLBindParam;
  SQLCancel: TSQLCancel;
  SQLCloseCursor: TSQLCloseCursor;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLColAttributeA: TSQLColAttribute;
  SQLColumnsA: TSQLColumns;
  SQLConnectA: TSQLConnect;
{$ELSE}
  SQLColAttributeW: TSQLColAttributeW;
  SQLColumnsW: TSQLColumnsW;
  SQLConnectW: TSQLConnectW;
{$ENDIF}
  SQLCopyDesc: TSQLCopyDesc;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLDataSourcesA: TSQLDataSources;
  SQLDescribeColA: TSQLDescribeCol;
{$ELSE}
  SQLDataSourcesW: TSQLDataSourcesW;
  SQLDescribeColW: TSQLDescribeColW;
{$ENDIF}
  SQLDisconnect: TSQLDisconnect;
  SQLEndTran: TSQLEndTran;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLErrorA: TSQLError;
  SQLExecDirectA: TSQLExecDirect;
{$ELSE}
  SQLErrorW: TSQLErrorW;
  SQLExecDirectW: TSQLExecDirectW;
{$ENDIF}
  SQLExecute: TSQLExecute;
  SQLFetch: TSQLFetch;
  SQLFetchScroll: TSQLFetchScroll;
  SQLFreeConnect: TSQLFreeConnect;
  SQLFreeEnv: TSQLFreeEnv;
  SQLFreeHandle: TSQLFreeHandle;
  SQLFreeStmt: TSQLFreeStmt;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLGetConnectAttrA: TSQLGetConnectAttr;
  SQLGetConnectOptionA: TSQLGetConnectOption;
  SQLGetCursorNameA: TSQLGetCursorName;
{$ELSE}
  SQLGetConnectAttrW: TSQLGetConnectAttrW;
  SQLGetConnectOptionW: TSQLGetConnectOptionW;
  SQLGetCursorNameW: TSQLGetCursorNameW;
{$ENDIF}
  SQLGetData: TSQLGetData;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLGetDescFieldA: TSQLGetDescField;
  SQLGetDescRecA: TSQLGetDescRec;
  SQLGetDiagFieldA: TSQLGetDiagField;
  SQLGetDiagRecA: TSQLGetDiagRec;
{$ELSE}
  SQLGetDescFieldW: TSQLGetDescFieldW;
  SQLGetDescRecW: TSQLGetDescRecW;
  SQLGetDiagFieldW: TSQLGetDiagFieldW;
  SQLGetDiagRecW: TSQLGetDiagRecW;
{$ENDIF}
  SQLGetEnvAttr: TSQLGetEnvAttr;
  SQLGetFunctions: TSQLGetFunctions;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLGetInfoA: TSQLGetInfo;
  SQLGetStmtAttrA: TSQLGetStmtAttr;
{$ELSE}
  SQLGetInfoW: TSQLGetInfoW;
  SQLGetStmtAttrW: TSQLGetStmtAttrW;
{$ENDIF}
  SQLGetStmtOption: TSQLGetStmtOption;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLGetTypeInfoA: TSQLGetTypeInfo;
{$ELSE}
  SQLGetTypeInfoW: TSQLGetTypeInfoW;
{$ENDIF}
  SQLNumResultCols: TSQLNumResultCols;
  SQLParamData: TSQLParamData;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLPrepareA: TSQLPrepare;
{$ELSE}
  SQLPrepareW: TSQLPrepareW;
{$ENDIF}
  SQLPutData: TSQLPutData;
  SQLRowCount: TSQLRowCount;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLSetConnectAttrA: TSQLSetConnectAttr;
  SQLSetConnectOptionA: TSQLSetConnectOption;
  SQLSetCursorNameA: TSQLSetCursorName;
  SQLSetDescFieldA: TSQLSetDescField;
{$ELSE}
  SQLSetConnectAttrW: TSQLSetConnectAttrW;
  SQLSetConnectOptionW: TSQLSetConnectOptionW;
  SQLSetCursorNameW: TSQLSetCursorNameW;
  SQLSetDescFieldW: TSQLSetDescFieldW;
{$ENDIF}
  SQLSetDescRec: TSQLSetDescRec;
  SQLSetEnvAttr: TSQLSetEnvAttr;
  SQLSetParam: TSQLSetParam;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLSetStmtAttrA: TSQLSetStmtAttr;
{$ELSE}
  SQLSetStmtAttrW: TSQLSetStmtAttrW;
{$ENDIF}
  SQLSetStmtOption: TSQLSetStmtOption;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLSpecialColumnsA: TSQLSpecialColumns;
  SQLStatisticsA: TSQLStatistics;
{$ELSE}
  SQLSpecialColumnsW: TSQLSpecialColumnsW;
  SQLStatisticsW: TSQLStatisticsW;
{$ENDIF}

  // sqlext.h
{$IFNDEF USE_UNICODE_DRIVER}
  SQLPrimaryKeysA: TSQLPrimaryKeys;
  SQLForeignKeysA: TSQLForeignKeys;
  SQLTablePrivilegesA: TSQLTablePrivileges;
  SQLColumnPrivilegesA: TSQLColumnPrivileges;

  SQLTablesA: TSQLTables;
{$ELSE}
  SQLPrimaryKeysW: TSQLPrimaryKeysW;
  SQLForeignKeysW: TSQLForeignKeysW;
  SQLTablePrivilegesW: TSQLTablePrivilegesW;
  SQLColumnPrivilegesW: TSQLColumnPrivilegesW;

  SQLTablesW: TSQLTablesW;
{$ENDIF}
  SQLTransact: TSQLTransact;

{$IFNDEF USE_UNICODE_DRIVER}
  SQLDriversA: TSQLDrivers;

  SQLProceduresA: TSQLProcedures;
  SQLProcedureColumnsA: TSQLProcedureColumns;

  SQLDriverConnectA: TSQLDriverConnect;
{$ELSE}
  SQLDriversW: TSQLDriversW;

  SQLProceduresW: TSQLProceduresW;
  SQLProcedureColumnsW: TSQLProcedureColumnsW;

  SQLDriverConnectW: TSQLDriverConnectW;
{$ENDIF}
  SQLDescribeParam: TSQLDescribeParam;
  SQLBindParameter: TSQLBindParameter;


  { Library Initialization }
procedure CheckODBCLoaded;
function TryODBCLoad: Boolean;
procedure LoadODBCLibrary;
procedure FreeODBCLibrary;


implementation
uses
  SysUtils, DB,
{$IFDEF CLX}
  QForms, QControls,
{$ELSE}
  Forms, Controls,
{$ENDIF}
  ODBCConsts;


var
{$IFDEF WIN32}
  ODBCLibraryHandle: THandle;
{$ENDIF}
{$IFDEF LINUX}
  ODBCLibraryHandle: Pointer;
{$ENDIF}


procedure CheckODBCLoaded;
begin
  if not TryODBCLoad then
    DatabaseError(SODBCMissing);
end;

function TryODBCLoad: Boolean;
begin
  Result := False;

{$IFDEF WIN32}
  if ODBCLibraryHandle <= HINSTANCE_ERROR then
    LoadODBCLibrary;

  if ODBCLibraryHandle > HINSTANCE_ERROR then
    Result := True;
{$ENDIF}

{$IFDEF LINUX}
  if ODBCLibraryHandle = nil then
    LoadODBCLibrary;
  Result := ODBCLibraryHandle <> nil;
{$ENDIF}
end;

procedure LoadODBCLibrary;

{$IFDEF WIN32}
  function GetProcAddr(ProcName: PChar): Pointer;
  begin
    Result := GetProcAddress(ODBCLibraryHandle, ProcName);

    if not Assigned(Result) then
{$IFDEF VER130}
      RaiseLastWin32Error;
{$ELSE}
      RaiseLastOSError;
{$ENDIF}
  end;
{$ENDIF}

{$IFDEF LINUX}
  function GetProcAddr(ProcName: PChar): Pointer;
  begin
    Result := dlsym(ODBCLibraryHandle, ProcName);

    if not Assigned(Result) then
      DACErrorFmt(SErrorLoadLib, [ProcName]);
  end;
{$ENDIF}

begin
{$IFDEF WIN32}
  ODBCLibraryHandle := LoadLibrary(PChar(ODBCLibraryName));
  if (ODBCLibraryHandle > HINSTANCE_ERROR) then
{$ENDIF}
{$IFDEF LINUX}
    ODBCLibraryHandle := dlopen(PChar(ODBCLibraryName), RTLD_GLOBAL);
  if ODBCLibraryHandle <> nil then
{$ENDIF}
  begin
    // odbc32.dll

    {do not localize}
    SQLAllocConnect := GetProcAddr('SQLAllocConnect');
    SQLAllocEnv := GetProcAddr('SQLAllocEnv');
    SQLAllocHandle := GetProcAddr('SQLAllocHandle');
    SQLAllocStmt := GetProcAddr('SQLAllocStmt');
    SQLBindCol := GetProcAddr('SQLBindCol');
    SQLBindParam := GetProcAddr('SQLBindParam');
    SQLCancel := GetProcAddr('SQLCancel');
    SQLCloseCursor := GetProcAddr('SQLCloseCursor');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLColAttributeA := GetProcAddr('SQLColAttributeA');         //A
    SQLColumnsA := GetProcAddr('SQLColumnsA');                   //A
    SQLConnectA := GetProcAddr('SQLConnectA');                   //A
{$ELSE}
    SQLColAttributeW := GetProcAddr('SQLColAttributeW');         //W
    SQLColumnsW := GetProcAddr('SQLColumnsW');                   //W
    SQLConnectW := GetProcAddr('SQLConnectW');                   //W
{$ENDIF}
    SQLCopyDesc := GetProcAddr('SQLCopyDesc');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLDataSourcesA := GetProcAddr('SQLDataSourcesA');           //A
    SQLDescribeColA := GetProcAddr('SQLDescribeColA');           //A
{$ELSE}
    SQLDataSourcesW := GetProcAddr('SQLDataSourcesW');           //W
    SQLDescribeColW := GetProcAddr('SQLDescribeColW');           //W
{$ENDIF}
    SQLDisconnect := GetProcAddr('SQLDisconnect');
    SQLEndTran := GetProcAddr('SQLEndTran');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLErrorA := GetProcAddr('SQLErrorA');                       //A
    SQLExecDirectA := GetProcAddr('SQLExecDirectA');             //A
{$ELSE}
    SQLErrorW := GetProcAddr('SQLErrorW');                       //W
    SQLExecDirectW := GetProcAddr('SQLExecDirectW');             //W
{$ENDIF}
    SQLExecute := GetProcAddr('SQLExecute');
    SQLFetch := GetProcAddr('SQLFetch');
    SQLFetchScroll := GetProcAddr('SQLFetchScroll');
    SQLFreeConnect := GetProcAddr('SQLFreeConnect');
    SQLFreeEnv := GetProcAddr('SQLFreeEnv');
    SQLFreeHandle := GetProcAddr('SQLFreeHandle');
    SQLFreeStmt := GetProcAddr('SQLFreeStmt');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLGetConnectAttrA := GetProcAddr('SQLGetConnectAttrA');     //A
    SQLGetConnectOptionA := GetProcAddr('SQLGetConnectOptionA'); //A
    SQLGetCursorNameA := GetProcAddr('SQLGetCursorNameA');       //A
{$ELSE}
    SQLGetConnectAttrW := GetProcAddr('SQLGetConnectAttrW');     //W
    SQLGetConnectOptionW := GetProcAddr('SQLGetConnectOptionW'); //W
    SQLGetCursorNameW := GetProcAddr('SQLGetCursorNameW');       //W
{$ENDIF}
    SQLGetData := GetProcAddr('SQLGetData');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLGetDescFieldA := GetProcAddr('SQLGetDescFieldA');         //A
    SQLGetDescRecA := GetProcAddr('SQLGetDescRecA');             //A
    SQLGetDiagFieldA := GetProcAddr('SQLGetDiagFieldA');         //A
    SQLGetDiagRecA := GetProcAddr('SQLGetDiagRecA');             //A
{$ELSE}
    SQLGetDescFieldW := GetProcAddr('SQLGetDescFieldW');         //W
    SQLGetDescRecW := GetProcAddr('SQLGetDescRecW');             //W
    SQLGetDiagFieldW := GetProcAddr('SQLGetDiagFieldW');         //W
    SQLGetDiagRecW := GetProcAddr('SQLGetDiagRecW');             //W
{$ENDIF}
    SQLGetEnvAttr := GetProcAddr('SQLGetEnvAttr');
    SQLGetFunctions := GetProcAddr('SQLGetFunctions');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLGetInfoA := GetProcAddr('SQLGetInfoA');                   //A
    SQLGetStmtAttrA := GetProcAddr('SQLGetStmtAttrA');           //A
{$ELSE}
    SQLGetInfoW := GetProcAddr('SQLGetInfoW');                   //W
    SQLGetStmtAttrW := GetProcAddr('SQLGetStmtAttrW');           //W
{$ENDIF}
    SQLGetStmtOption := GetProcAddr('SQLGetStmtOption');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLGetTypeInfoA := GetProcAddr('SQLGetTypeInfoA');           //A
{$ELSE}
    SQLGetTypeInfoW := GetProcAddr('SQLGetTypeInfoW');           //W
{$ENDIF}
    SQLNumResultCols := GetProcAddr('SQLNumResultCols');
    SQLParamData := GetProcAddr('SQLParamData');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLPrepareA := GetProcAddr('SQLPrepareA');                   //A
{$ELSE}
    SQLPrepareW := GetProcAddr('SQLPrepareW');                   //W
{$ENDIF}
    SQLPutData := GetProcAddr('SQLPutData');
    SQLRowCount := GetProcAddr('SQLRowCount');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLSetConnectAttrA := GetProcAddr('SQLSetConnectAttrA');     //A
    SQLSetConnectOptionA := GetProcAddr('SQLSetConnectOptionA'); //A
    SQLSetCursorNameA := GetProcAddr('SQLSetCursorNameA');       //A
    SQLSetDescFieldA := GetProcAddr('SQLSetDescFieldA');         //A
{$ELSE}
    SQLSetConnectAttrW := GetProcAddr('SQLSetConnectAttrW');     //W
    SQLSetConnectOptionW := GetProcAddr('SQLSetConnectOptionW'); //W
    SQLSetCursorNameW := GetProcAddr('SQLSetCursorNameW');       //W
    SQLSetDescFieldW := GetProcAddr('SQLSetDescFieldW');         //W
{$ENDIF}
    SQLSetDescRec := GetProcAddr('SQLSetDescRec');
    SQLSetEnvAttr := GetProcAddr('SQLSetEnvAttr');
    SQLSetParam := GetProcAddr('SQLSetParam');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLSetStmtAttrA := GetProcAddr('SQLSetStmtAttrA');           //A
{$ELSE}
    SQLSetStmtAttrW := GetProcAddr('SQLSetStmtAttrW');           //W
{$ENDIF}
    SQLSetStmtOption := GetProcAddr('SQLSetStmtOption');
{$IFNDEF USE_UNICODE_DRIVER}
    SQLSpecialColumnsA := GetProcAddr('SQLSpecialColumnsA');     //A
    SQLStatisticsA := GetProcAddr('SQLStatisticsA');             //A
    SQLPrimaryKeysA := GetProcAddr('SQLPrimaryKeysA');           //A
    SQLForeignKeysA := GetProcAddr('SQLForeignKeysA');           //A
    SQLTablePrivilegesA := GetProcAddr('SQLTablePrivilegesA');   //A
    SQLColumnPrivilegesA := GetProcAddr('SQLColumnPrivilegesA'); //A
{$ELSE}
    SQLSpecialColumnsW := GetProcAddr('SQLSpecialColumnsW');     //W
    SQLStatisticsW := GetProcAddr('SQLStatisticsW');             //W
    SQLPrimaryKeysW := GetProcAddr('SQLPrimaryKeysW');           //W
    SQLForeignKeysW := GetProcAddr('SQLForeignKeysW');           //W
    SQLTablePrivilegesW := GetProcAddr('SQLTablePrivilegesW');   //W
    SQLColumnPrivilegesW := GetProcAddr('SQLColumnPrivilegesW'); //W
{$ENDIF}

{$IFNDEF USE_UNICODE_DRIVER}
    SQLTablesA := GetProcAddr('SQLTablesA');                     //A
{$ELSE}
    SQLTablesW := GetProcAddr('SQLTablesW');                     //W
{$ENDIF}
    SQLTransact := GetProcAddr('SQLTransact');

    // sqlext.h
{$IFNDEF USE_UNICODE_DRIVER}
    SQLDriversA := GetProcAddr('SQLDriversA');                   //A
    SQLProceduresA := GetProcAddr('SQLProceduresA');             //A
    SQLProcedureColumnsA := GetProcAddr('SQLProcedureColumnsA'); //A
    SQLDriverConnectA := GetProcAddr('SQLDriverConnectA');       //A
{$ELSE}
    SQLDriversW := GetProcAddr('SQLDriversW');                   //W
    SQLProceduresW := GetProcAddr('SQLProceduresW');             //W
    SQLProcedureColumnsW := GetProcAddr('SQLProcedureColumnsW'); //W
    SQLDriverConnectW := GetProcAddr('SQLDriverConnectW');       //W
{$ENDIF}
    SQLDescribeParam := GetProcAddr('SQLDescribeParam');
    SQLBindParameter := GetProcAddr('SQLBindParameter');
  end;
end;

procedure FreeODBCLibrary;
begin
{$IFDEF WIN32}
  if ODBCLibraryHandle > HINSTANCE_ERROR then
  begin
    FreeLibrary(ODBCLibraryHandle);
    ODBCLibraryHandle := 0;
  end;
{$ENDIF}
{$IFDEF LINUX}
  if ODBCLibraryHandle <> nil then
  begin
    dlclose(ODBCLibraryHandle);
    ODBCLibraryHandle := nil;
  end;
{$ENDIF}
end;


initialization

finalization
  FreeODBCLibrary;

end.
