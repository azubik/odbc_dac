{*******************************************************}
{                                                       }
{       "sqlucode.h"                                    }
{                                                       }
{   Translated to Object Pascal by Andrey Zubik         }
{   e-mail: andrey.zubik@gmail.com                      } 
{                                                       }
{*******************************************************}

unit odbcsqlucode;

interface
uses
  odbcsqltypes, odbcsqlext;

const
  SQL_WCHAR = (-8);
  SQL_WVARCHAR = (-9);
  SQL_WLONGVARCHAR = (-10);
  SQL_C_WCHAR = SQL_WCHAR;

{$IFDEF UNICODE}
  SQL_C_TCHAR = SQL_C_WCHAR;
{$ELSE}
  SQL_C_TCHAR = SQL_C_CHAR;
{$ENDIF}

  SQL_SQLSTATE_SIZEW = 10;                        { size of SQLSTATE for unicode }


  SQL_UNICODE = SQL_WCHAR;
  SQL_UNICODE_VARCHAR = SQL_WVARCHAR;
  SQL_UNICODE_LONGVARCHAR = SQL_WLONGVARCHAR;
  SQL_UNICODE_CHAR = SQL_WCHAR;


  // {$IFDEF RC_INVOKED}
type


  // UNICODE versions

  TSQLColAttributeW = function
    (
    hstmt: SQLHSTMT;
    iCol: SQLUSMALLINT;
    iField: SQLUSMALLINT;
    pCharAttr: SQLPOINTER;
    cbCharAttrMax: SQLSMALLINT;
    pcbCharAttr: PSQLSMALLINT;
    pNumAttr: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLColAttributesW = function
    (
    hstmt: SQLHSTMT;
    icol: SQLUSMALLINT;
    fDescType: SQLUSMALLINT;
    rgbDesc: SQLPOINTER;
    cbDescMax: SQLSMALLINT;
    pcbDesc: PSQLSMALLINT;
    pfDesc: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLConnectW = function
    (
    hdbc: SQLHDBC;
    szDSN: PSQLWCHAR;
    cbDSN: SQLSMALLINT;
    szUID: PSQLWCHAR;
    cbUID: SQLSMALLINT;
    szAuthStr: PSQLWCHAR;
    cbAuthStr: SQLSMALLINT
    ): SQLRETURN; stdcall;


  TSQLDescribeColW = function
    (
    hstmt: SQLHSTMT;
    icol: SQLUSMALLINT;
    szColName: PSQLWCHAR;
    cbColNameMax: SQLSMALLINT;
    pcbColName: PSQLSMALLINT;
    pfSqlType: PSQLSMALLINT;
    pcbColDef: PSQLUINTEGER;
    pibScale: PSQLSMALLINT;
    pfNullable: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLErrorW = function
    (
    henv: SQLHENV;
    hdbc: SQLHDBC;
    hstmt: SQLHSTMT;
    szSqlState: PSQLWCHAR;
    pfNativeError: PSQLINTEGER;
    szErrorMsg: PSQLWCHAR;
    cbErrorMsgMax: SQLSMALLINT;
    pcbErrorMsg: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLExecDirectW = function
    (
    hstmt: SQLHSTMT;
    szSqlStr: PSQLWCHAR;
    cbSqlStr: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetConnectAttrW = function
    (
    hdbc: SQLHDBC;
    fAttribute: SQLINTEGER;
    rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetCursorNameW = function
    (
    hstmt: SQLHSTMT;
    szCursor: PSQLWCHAR;
    cbCursorMax: SQLSMALLINT;
    pcbCursor: PSQLSMALLINT
    ): SQLRETURN; stdcall;


  //#if (ODBCVER >= 0x0300)

  TSQLSetDescFieldW = function
    (
    DescriptorHandle: SQLHDESC;
    RecNumber: SQLSMALLINT;
    FieldIdentifier: SQLSMALLINT;
    Value: SQLPOINTER;
    BufferLength: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetDescFieldW = function
    (
    hdesc: SQLHDESC;
    iRecord: SQLSMALLINT;
    iField: SQLSMALLINT;
    rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetDescRecW = function
    (
    hdesc: SQLHDESC;
    iRecord: SQLSMALLINT;
    szName: PSQLWCHAR;
    cbNameMax: SQLSMALLINT;
    pcbName: PSQLSMALLINT;
    pfType: PSQLSMALLINT;
    pfSubType: PSQLSMALLINT;
    pLength: PSQLINTEGER;
    pPrecision: PSQLSMALLINT;
    pScale: PSQLSMALLINT;
    pNullable: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetDiagFieldW = function
    (
    fHandleType: SQLSMALLINT;
    handle: SQLHANDLE;
    iRecord: SQLSMALLINT;
    fDiagField: SQLSMALLINT;
    rgbDiagInfo: SQLPOINTER;
    cbDiagInfoMax: SQLSMALLINT;
    pcbDiagInfo: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetDiagRecW = function
    (
    fHandleType: SQLSMALLINT;
    handle: SQLHANDLE;
    iRecord: SQLSMALLINT;
    szSqlState: PSQLWCHAR;
    pfNativeError: PSQLINTEGER;
    szErrorMsg: PSQLWCHAR;
    cbErrorMsgMax: SQLSMALLINT;
    pcbErrorMsg: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  // #endif { (ODBCVER >= 0x0300) }


  TSQLPrepareW = function
    (
    hstmt: SQLHSTMT;
    szSqlStr: PSQLWCHAR;
    cbSqlStr: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetConnectAttrW = function
    (
    hdbc: SQLHDBC;
    fAttribute: SQLINTEGER;
    rgbValue: SQLPOINTER;
    cbValue: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetCursorNameW = function
    (
    hstmt: SQLHSTMT;
    szCursor: PSQLWCHAR;
    cbCursor: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLColumnsW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLWCHAR;
    cbTableName: SQLSMALLINT;
    szColumnName: PSQLWCHAR;
    cbColumnName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetConnectOptionW = function
    (
    hdbc: SQLHDBC;
    fOption: SQLUSMALLINT;
    pvParam: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLGetInfoW = function
    (
    hdbc: SQLHDBC;
    fInfoType: SQLUSMALLINT;
    rgbInfoValue: SQLPOINTER;
    cbInfoValueMax: SQLSMALLINT;
    pcbInfoValue: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetTypeInfoW = function
    (
    StatementHandle: SQLHSTMT;
    DataTyoe: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLSetConnectOptionW = function
    (
    hdbc: SQLHDBC;
    fOption: SQLUSMALLINT;
    vParam: SQLUINTEGER
    ): SQLRETURN; stdcall;

  TSQLSpecialColumnsW = function
    (
    hstmt: SQLHSTMT;
    fColType: SQLUSMALLINT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLWCHAR;
    cbTableName: SQLSMALLINT;
    fScope: SQLUSMALLINT;
    fNullable: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLStatisticsW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLWCHAR;
    cbTableName: SQLSMALLINT;
    fUnique: SQLUSMALLINT;
    fAccuracy: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLTablesW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLWCHAR;
    cbTableName: SQLSMALLINT;
    szTableType: PSQLWCHAR;
    cbTableType: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDataSourcesW = function
    (
    henv: SQLHENV;
    fDirection: SQLUSMALLINT;
    szDSN: PSQLWCHAR;
    cbDSNMax: SQLSMALLINT;
    pcbDSN: PSQLSMALLINT;
    szDescription: PSQLWCHAR;
    cbDescriptionMax: SQLSMALLINT;
    pcbDescription: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDriverConnectW = function
    (
    hdbc: SQLHDBC;
    hwnd: SQLHWND;
    szConnStrIn: PSQLWCHAR;
    cbConnStrIn: SQLSMALLINT;
    szConnStrOut: PSQLWCHAR;
    cbConnStrOutMax: SQLSMALLINT;
    pcbConnStrOut: PSQLSMALLINT;
    fDriverCompletion: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLBrowseConnectW = function
    (
    hdbc: SQLHDBC;
    szConnStrIn: PSQLWCHAR;
    cbConnStrIn: SQLSMALLINT;
    szConnStrOut: PSQLWCHAR;
    cbConnStrOutMax: SQLSMALLINT;
    pcbConnStrOut: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLColumnPrivilegesW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLWCHAR;
    cbTableName: SQLSMALLINT;
    szColumnName: PSQLWCHAR;
    cbColumnName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetStmtAttrW = function
    (
    hstmt: SQLHSTMT;
    fAttribute: SQLINTEGER;
    rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetStmtAttrW = function
    (
    hstmt: SQLHSTMT;
    fAttribute: SQLINTEGER;
    rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLForeignKeysW = function
    (
    hstmt: SQLHSTMT;
    szPkCatalogName: PSQLWCHAR;
    cbPkCatalogName: SQLSMALLINT;
    szPkSchemaName: PSQLWCHAR;
    cbPkSchemaName: SQLSMALLINT;
    szPkTableName: PSQLWCHAR;
    cbPkTableName: SQLSMALLINT;
    szFkCatalogName: PSQLWCHAR;
    cbFkCatalogName: SQLSMALLINT;
    szFkSchemaName: PSQLWCHAR;
    cbFkSchemaName: SQLSMALLINT;
    szFkTableName: PSQLWCHAR;
    cbFkTableName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLNativeSqlW = function
    (
    hdbc: SQLHDBC;
    szSqlStrIn: PSQLWCHAR;
    cbSqlStrIn: SQLINTEGER;
    szSqlStr: PSQLWCHAR;
    cbSqlStrMax: SQLINTEGER;
    pcbSqlStr: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLPrimaryKeysW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLWCHAR;
    cbTableName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLProcedureColumnsW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szProcName: PSQLWCHAR;
    cbProcName: SQLSMALLINT;
    szColumnName: PSQLWCHAR;
    cbColumnName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLProceduresW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szProcName: PSQLWCHAR;
    cbProcName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLTablePrivilegesW = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLWCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLWCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLWCHAR;
    cbTableName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDriversW = function
    (
    henv: SQLHENV;
    fDirection: SQLUSMALLINT;
    szDriverDesc: PSQLWCHAR;
    cbDriverDescMax: SQLSMALLINT;
    pcbDriverDesc: PSQLSMALLINT;
    szDriverAttributes: PSQLWCHAR;
    cbDrvrAttrMax: SQLSMALLINT;
    pcbDrvrAttr: PSQLSMALLINT
    ): SQLRETURN; stdcall;








  // ANSI versions


  TSQLColAttributeA = function
    (
    hstmt: SQLHSTMT;
    iCol: SQLSMALLINT;
    iField: SQLSMALLINT;
    pCharAttr: SQLPOINTER;
    cbCharAttrMax: SQLSMALLINT;
    pcbCharAttr: PSQLSMALLINT;
    pNumAttr: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLColAttributesA = function
    (
    hstmt: SQLHSTMT;
    icol: SQLUSMALLINT;
    fDescType: SQLUSMALLINT;
    rgbDesc: SQLPOINTER;
    cbDescMax: SQLSMALLINT;
    pcbDesc: PSQLSMALLINT;
    pfDesc: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLConnectA = function
    (
    hdbc: SQLHDBC;
    szDSN: PSQLCHAR;
    cbDSN: SQLSMALLINT;
    szUID: PSQLCHAR;
    cbUID: SQLSMALLINT;
    szAuthStr: PSQLCHAR;
    cbAuthStr: SQLSMALLINT
    ): SQLRETURN; stdcall;


  TSQLDescribeColA = function
    (
    hstmt: SQLHSTMT;
    icol: SQLUSMALLINT;
    szColName: PSQLCHAR;
    cbColNameMax: SQLSMALLINT;
    pcbColName: PSQLSMALLINT;
    pfSqlType: PSQLSMALLINT;
    pcbColDef: PSQLUINTEGER;
    pibScale: PSQLSMALLINT;
    pfNullable: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLErrorA = function
    (
    henv: SQLHENV;
    hdbc: SQLHDBC;
    hstmt: SQLHSTMT;
    szSqlState: PSQLCHAR;
    pfNativeError: PSQLINTEGER;
    szErrorMsg: PSQLCHAR;
    cbErrorMsgMax: SQLSMALLINT;
    pcbErrorMsg: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLExecDirectA = function
    (
    hstmt: SQLHSTMT;
    szSqlStr: PSQLCHAR;
    cbSqlStr: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetConnectAttrA = function
    (
    hdbc: SQLHDBC;
    fAttribute: SQLINTEGER;
    rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetCursorNameA = function
    (
    hstmt: SQLHSTMT;
    szCursor: PSQLCHAR;
    cbCursorMax: SQLSMALLINT;
    pcbCursor: PSQLSMALLINT
    ): SQLRETURN; stdcall;


  //#if (ODBCVER >= 0x0300)

  TSQLGetDescFieldA = function
    (
    hdesc: SQLHDESC;
    iRecord: SQLSMALLINT;
    iField: SQLSMALLINT;
    rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetDescRecA = function
    (
    hdesc: SQLHDESC;
    iRecord: SQLSMALLINT;
    szName: PSQLCHAR;
    cbNameMax: SQLSMALLINT;
    pcbName: PSQLSMALLINT;
    pfType: PSQLSMALLINT;
    pfSubType: PSQLSMALLINT;
    pLength: PSQLINTEGER;
    pPrecision: PSQLSMALLINT;
    pScale: PSQLSMALLINT;
    pNullable: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetDiagFieldA = function
    (
    fHandleType: SQLSMALLINT;
    handle: SQLHANDLE;
    iRecord: SQLSMALLINT;
    fDiagField: SQLSMALLINT;
    rgbDiagInfo: SQLPOINTER;
    cbDiagInfoMax: SQLSMALLINT;
    pcbDiagInfo: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetDiagRecA = function
    (
    fHandleType: SQLSMALLINT;
    handle: SQLHANDLE;
    iRecord: SQLSMALLINT;
    szSqlState: PSQLCHAR;
    pfNativeError: PSQLINTEGER;
    szErrorMsg: PSQLCHAR;
    cbErrorMsgMax: SQLSMALLINT;
    pcbErrorMsg: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetStmtAttrA = function
    (
    hstmt: SQLHSTMT;
    fAttribute: SQLINTEGER;
    rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER
    ): SQLRETURN; stdcall;

  // #endif { (ODBCVER >= 0x0300) }


  TSQLGetTypeInfoA = function
    (
    StatementHandle: SQLHSTMT;
    DataTyoe: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLPrepareA = function
    (
    hstmt: SQLHSTMT;
    szSqlStr: PSQLCHAR;
    cbSqlStr: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetConnectAttrA = function
    (
    hdbc: SQLHDBC;
    fAttribute: SQLINTEGER;
    rgbValue: SQLPOINTER;
    cbValue: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetCursorNameA = function
    (
    hstmt: SQLHSTMT;
    szCursor: PSQLCHAR;
    cbCursor: SQLSMALLINT
    ): SQLRETURN; stdcall;


  TSQLColumnsA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR;
    cbTableName: SQLSMALLINT;
    szColumnName: PSQLCHAR;
    cbColumnName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetConnectOptionA = function
    (
    hdbc: SQLHDBC;
    fOption: SQLUSMALLINT;
    pvParam: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLGetInfoA = function
    (
    hdbc: SQLHDBC;
    fInfoType: SQLUSMALLINT;
    rgbInfoValue: SQLPOINTER;
    cbInfoValueMax: SQLSMALLINT;
    pcbInfoValue: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetStmtOptionA = function
    (
    hstmt: SQLHSTMT;
    fOption: SQLUSMALLINT;
    pvParam: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLSetConnectOptionA = function
    (
    hdbc: SQLHDBC;
    fOption: SQLUSMALLINT;
    vParam: SQLUINTEGER
    ): SQLRETURN; stdcall;

  TSQLSpecialColumnsA = function
    (
    hstmt: SQLHSTMT;
    fColType: SQLUSMALLINT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR;
    cbTableName: SQLSMALLINT;
    fScope: SQLUSMALLINT;
    fNullable: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLStatisticsA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR;
    cbTableName: SQLSMALLINT;
    fUnique: SQLUSMALLINT;
    fAccuracy: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLTablesA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR;
    cbTableName: SQLSMALLINT;
    szTableType: PSQLCHAR;
    cbTableType: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDataSourcesA = function
    (
    henv: SQLHENV;
    fDirection: SQLUSMALLINT;
    szDSN: PSQLCHAR;
    cbDSNMax: SQLSMALLINT;
    pcbDSN: PSQLSMALLINT;
    szDescription: PSQLCHAR;
    cbDescriptionMax: SQLSMALLINT;
    pcbDescription: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDriverConnectA = function
    (
    hdbc: SQLHDBC;
    hwnd: SQLHWND;
    szConnStrIn: PSQLCHAR;
    cbConnStrIn: SQLSMALLINT;
    szConnStrOut: PSQLCHAR;
    cbConnStrOutMax: SQLSMALLINT;
    pcbConnStrOut: PSQLSMALLINT;
    fDriverCompletion: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLBrowseConnectA = function
    (
    hdbc: SQLHDBC;
    szConnStrIn: PSQLCHAR;
    cbConnStrIn: SQLSMALLINT;
    szConnStrOut: PSQLCHAR;
    cbConnStrOutMax: SQLSMALLINT;
    pcbConnStrOut: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLColumnPrivilegesA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR;
    cbTableName: SQLSMALLINT;
    szColumnName: PSQLCHAR;
    cbColumnName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDescribeParamA = function
    (
    hstmt: SQLHSTMT;
    ipar: SQLUSMALLINT;
    pfSqlType: PSQLSMALLINT;
    pcbParamDef: PSQLUINTEGER;
    pibScale: PSQLSMALLINT;
    pfNullable: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLForeignKeysA = function
    (
    hstmt: SQLHSTMT;
    szPkCatalogName: PSQLCHAR;
    cbPkCatalogName: SQLSMALLINT;
    szPkSchemaName: PSQLCHAR;
    cbPkSchemaName: SQLSMALLINT;
    szPkTableName: PSQLCHAR;
    cbPkTableName: SQLSMALLINT;
    szFkCatalogName: PSQLCHAR;
    cbFkCatalogName: SQLSMALLINT;
    szFkSchemaName: PSQLCHAR;
    cbFkSchemaName: SQLSMALLINT;
    szFkTableName: PSQLCHAR;
    cbFkTableName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLNativeSqlA = function
    (
    hdbc: SQLHDBC;
    szSqlStrIn: PSQLCHAR;
    cbSqlStrIn: SQLINTEGER;
    szSqlStr: PSQLCHAR;
    cbSqlStrMax: SQLINTEGER;
    pcbSqlStr: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLPrimaryKeysA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR;
    cbTableName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLProcedureColumnsA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szProcName: PSQLCHAR;
    cbProcName: SQLSMALLINT;
    szColumnName: PSQLCHAR;
    cbColumnName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLProceduresA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szProcName: PSQLCHAR;
    cbProcName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLTablePrivilegesA = function
    (
    hstmt: SQLHSTMT;
    szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT;
    szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR;
    cbTableName: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDriversA = function
    (
    henv: SQLHENV;
    fDirection: SQLUSMALLINT;
    szDriverDesc: PSQLCHAR;
    cbDriverDescMax: SQLSMALLINT;
    pcbDriverDesc: PSQLSMALLINT;
    szDriverAttributes: PSQLCHAR;
    cbDrvrAttrMax: SQLSMALLINT;
    pcbDrvrAttr: PSQLSMALLINT
    ): SQLRETURN; stdcall;


  // {$ENDIF}    // RC_INVOKED


implementation

(*


//---------------------------------------------
// Mapping macros for Unicode
//---------------------------------------------

#ifndef	SQL_NOUNICODEMAP	// define this to disable the mapping
#ifdef 	UNICODE

#define	SQLColAttribute		SQLColAttributeW
#define	SQLColAttributes	SQLColAttributesW
#define	SQLConnect			SQLConnectW
#define	SQLDescribeCol		SQLDescribeColW
#define	SQLError			SQLErrorW
#define	SQLExecDirect		SQLExecDirectW
#define	SQLGetConnectAttr	SQLGetConnectAttrW
#define	SQLGetCursorName	SQLGetCursorNameW
#define	SQLGetDescField		SQLGetDescFieldW
#define	SQLGetDescRec		SQLGetDescRecW
#define	SQLGetDiagField		SQLGetDiagFieldW
#define	SQLGetDiagRec		SQLGetDiagRecW
#define	SQLPrepare			SQLPrepareW
#define	SQLSetConnectAttr	SQLSetConnectAttrW
#define	SQLSetCursorName	SQLSetCursorNameW
#define	SQLSetDescField		SQLSetDescFieldW
#define SQLSetStmtAttr		SQLSetStmtAttrW
#define SQLGetStmtAttr		SQLGetStmtAttrW
#define	SQLColumns			SQLColumnsW
#define	SQLGetConnectOption	SQLGetConnectOptionW
#define	SQLGetInfo			SQLGetInfoW
#define SQLGetTypeInfo		SQLGetTypeInfoW
#define	SQLSetConnectOption	SQLSetConnectOptionW
#define	SQLSpecialColumns	SQLSpecialColumnsW
#define	SQLStatistics		SQLStatisticsW
#define	SQLTables			SQLTablesW
#define	SQLDataSources		SQLDataSourcesW
#define	SQLDriverConnect	SQLDriverConnectW
#define	SQLBrowseConnect	SQLBrowseConnectW
#define	SQLColumnPrivileges	SQLColumnPrivilegesW
#define	SQLForeignKeys		SQLForeignKeysW
#define	SQLNativeSql		SQLNativeSqlW
#define	SQLPrimaryKeys		SQLPrimaryKeysW
#define	SQLProcedureColumns	SQLProcedureColumnsW
#define	SQLProcedures		SQLProceduresW
#define	SQLTablePrivileges	SQLTablePrivilegesW
#define	SQLDrivers			SQLDriversW

#endif	/* UNICODE */
#endif	/* SQL_NOUNICODEMAP	*/

#endif /* RC_INVOKED */


#ifdef __cplusplus
}                                    /* End of extern "C" { */
#endif  /* __cplusplus */

#endif  /* #ifndef __SQLUCODE */

 *)

end.
