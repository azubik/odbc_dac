
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
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  odbcsql, odbcsqlext;


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
  SQLColAttribute: TSQLColAttribute;
  SQLColumns: TSQLColumns;
  SQLConnect: TSQLConnect;
  SQLCopyDesc: TSQLCopyDesc;
  SQLDataSources: TSQLDataSources;
  SQLDescribeCol: TSQLDescribeCol;
  SQLDisconnect: TSQLDisconnect;
  SQLEndTran: TSQLEndTran;
  SQLError: TSQLError;
  SQLExecDirect: TSQLExecDirect;
  SQLExecute: TSQLExecute;
  SQLFetch: TSQLFetch;
  SQLFetchScroll: TSQLFetchScroll;
  SQLFreeConnect: TSQLFreeConnect;
  SQLFreeEnv: TSQLFreeEnv;
  SQLFreeHandle: TSQLFreeHandle;
  SQLFreeStmt: TSQLFreeStmt;
  SQLGetConnectAttr: TSQLGetConnectAttr;
  SQLGetConnectOption: TSQLGetConnectOption;
  SQLGetCursorName: TSQLGetCursorName;
  SQLGetData: TSQLGetData;
  SQLGetDescField: TSQLGetDescField;
  SQLGetDescRec: TSQLGetDescRec;
  SQLGetDiagField: TSQLGetDiagField;
  SQLGetDiagRec: TSQLGetDiagRec;
  SQLGetEnvAttr: TSQLGetEnvAttr;
  SQLGetFunctions: TSQLGetFunctions;
  SQLGetInfo: TSQLGetInfo;
  SQLGetStmtAttr: TSQLGetStmtAttr;
  SQLGetStmtOption: TSQLGetStmtOption;
  SQLGetTypeInfo: TSQLGetTypeInfo;
  SQLNumResultCols: TSQLNumResultCols;
  SQLParamData: TSQLParamData;
  SQLPrepare: TSQLPrepare;
  SQLPutData: TSQLPutData;
  SQLRowCount: TSQLRowCount;
  SQLSetConnectAttr: TSQLSetConnectAttr;
  SQLSetConnectOption: TSQLSetConnectOption;
  SQLSetCursorName: TSQLSetCursorName;
  SQLSetDescField: TSQLSetDescField;
  SQLSetDescRec: TSQLSetDescRec;
  SQLSetEnvAttr: TSQLSetEnvAttr;
  SQLSetParam: TSQLSetParam;
  SQLSetStmtAttr: TSQLSetStmtAttr;
  SQLSetStmtOption: TSQLSetStmtOption;
  SQLSpecialColumns: TSQLSpecialColumns;
  SQLStatistics: TSQLStatistics;

  // sqlext.h
  SQLPrimaryKeys: TSQLPrimaryKeys;
  SQLForeignKeys: TSQLForeignKeys;
  SQLTablePrivileges: TSQLTablePrivileges;
  SQLColumnPrivileges: TSQLColumnPrivileges;

  SQLTables: TSQLTables;
  SQLTransact: TSQLTransact;

  SQLDrivers: TSQLDrivers;

  SQLProcedures: TSQLProcedures;
  SQLProcedureColumns: TSQLProcedureColumns;

  SQLDriverConnect: TSQLDriverConnect;
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
{$IFDEF VCL50}
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
    SQLColAttribute := GetProcAddr('SQLColAttribute');
    SQLColumns := GetProcAddr('SQLColumns');
    SQLConnect := GetProcAddr('SQLConnect');
    SQLCopyDesc := GetProcAddr('SQLCopyDesc');
    SQLDataSources := GetProcAddr('SQLDataSources');
    SQLDescribeCol := GetProcAddr('SQLDescribeCol');
    SQLDisconnect := GetProcAddr('SQLDisconnect');
    SQLEndTran := GetProcAddr('SQLEndTran');
    SQLError := GetProcAddr('SQLError');
    SQLExecDirect := GetProcAddr('SQLExecDirect');
    SQLExecute := GetProcAddr('SQLExecute');
    SQLFetch := GetProcAddr('SQLFetch');
    SQLFetchScroll := GetProcAddr('SQLFetchScroll');
    SQLFreeConnect := GetProcAddr('SQLFreeConnect');
    SQLFreeEnv := GetProcAddr('SQLFreeEnv');
    SQLFreeHandle := GetProcAddr('SQLFreeHandle');
    SQLFreeStmt := GetProcAddr('SQLFreeStmt');
    SQLGetConnectAttr := GetProcAddr('SQLGetConnectAttr');
    SQLGetConnectOption := GetProcAddr('SQLGetConnectOption');
    SQLGetCursorName := GetProcAddr('SQLGetCursorName');
    SQLGetData := GetProcAddr('SQLGetData');
    SQLGetDescField := GetProcAddr('SQLGetDescField');
    SQLGetDescRec := GetProcAddr('SQLGetDescRec');
    SQLGetDiagField := GetProcAddr('SQLGetDiagField');
    SQLGetDiagRec := GetProcAddr('SQLGetDiagRec');
    SQLGetEnvAttr := GetProcAddr('SQLGetEnvAttr');
    SQLGetFunctions := GetProcAddr('SQLGetFunctions');
    SQLGetInfo := GetProcAddr('SQLGetInfo');
    SQLGetStmtAttr := GetProcAddr('SQLGetStmtAttr');
    SQLGetStmtOption := GetProcAddr('SQLGetStmtOption');
    SQLGetTypeInfo := GetProcAddr('SQLGetTypeInfo');
    SQLNumResultCols := GetProcAddr('SQLNumResultCols');
    SQLParamData := GetProcAddr('SQLParamData');
    SQLPrepare := GetProcAddr('SQLPrepare');
    SQLPutData := GetProcAddr('SQLPutData');
    SQLRowCount := GetProcAddr('SQLRowCount');
    SQLSetConnectAttr := GetProcAddr('SQLSetConnectAttr');
    SQLSetConnectOption := GetProcAddr('SQLSetConnectOption');
    SQLSetCursorName := GetProcAddr('SQLSetCursorName');
    SQLSetDescField := GetProcAddr('SQLSetDescField');
    SQLSetDescRec := GetProcAddr('SQLSetDescRec');
    SQLSetEnvAttr := GetProcAddr('SQLSetEnvAttr');
    SQLSetParam := GetProcAddr('SQLSetParam');
    SQLSetStmtAttr := GetProcAddr('SQLSetStmtAttr');
    SQLSetStmtOption := GetProcAddr('SQLSetStmtOption');
    SQLSpecialColumns := GetProcAddr('SQLSpecialColumns');
    SQLStatistics := GetProcAddr('SQLStatistics');
    SQLPrimaryKeys := GetProcAddr('SQLPrimaryKeys');
    SQLForeignKeys := GetProcAddr('SQLForeignKeys');
    SQLTablePrivileges := GetProcAddr('SQLTablePrivileges');
    SQLColumnPrivileges := GetProcAddr('SQLColumnPrivileges');

    SQLTables := GetProcAddr('SQLTables');
    SQLTransact := GetProcAddr('SQLTransact');

    // sqlext.h
    SQLDrivers := GetProcAddr('SQLDrivers');
    SQLProcedures := GetProcAddr('SQLProcedures');
    SQLProcedureColumns := GetProcAddr('SQLProcedureColumns');

    SQLDriverConnect := GetProcAddr('SQLDriverConnect');
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
