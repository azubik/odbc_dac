
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       Driver Specific Unit                            }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCDrvSpec;
{$I sv.inc}
interface
uses
  odbcsqltypes;


const
  // These contants have been imported from Edward Benson's dbExpress driver for
  // ODBC

  // Copyright (c) 2001, 2003 Edward Benson

  //  This library is free software; you can redistribute it and/or
  //  modify it under the terms of the GNU Lesser General Public License
  //  as published by the Free Software Foundation; either version 2.1
  //  of the License, or (at your option) any later version.


  ////////////////////////////////////////////////////////////////////////////////
  // DB2
  ////////////////////////////////////////////////////////////////////////////////

    // IBM DB2 extensions to ODBC API:
  SQL_LONGDATA_COMPAT = 1253;
  SQL_LD_COMPAT_NO = 0;
  SQL_LD_COMPAT_YES = 1;
  ////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////
  // Informix
  ////////////////////////////////////////////////////////////////////////////////

    // INFORMIX extensions to ODBC API (see in INFORMIX CLIENT SDK: "INFXCLI.H"):

    // For extended errors:

  SQL_INFX_ADIAG_ISAM_ERROR = 13;
  SQL_INFX_ADIAG_XA_ERROR = 14;


  //----------------------------------------------------------------------------
  { START -- Q+E Software's SQLSetStmtOption extensions (1040 to 1139)
    defines here for backwards compatibility: }
  SQL_STMTOPT_START = 1040;
  // Get the rowid for the last row inserted:
  SQL_GET_ROWID = (SQL_STMTOPT_START + 8);
  // Get the value for the serial column in the last row inserted:
  SQL_GET_SERIAL_VALUE = (SQL_STMTOPT_START + 9);
  { END -- Q+E Software's SQLSetStmtOption extensions (1040 to 1139) }
  //----------------------------------------------------------------------------

  //    Informix extensions:

  // Informix Column Attributes Flags Definitions:
  INFX_FDNULLABLE = $0001;                        // null allowed in field
  INFX_FDDISTINCT = $0002;                        // distinct of all
  INFX_FDDISTLVARCHAR = $0004;                    // distinct of SQLLVARCHAR
  INFX_FDDISTBOOLEAN = $0008;                     // distinct of SQLBOOL
  INFX_FDDISTSIMP = $0010;                        // distinct of simple type
  INFX_FDCSTTYPE = $0020;                         // constructor type
  INFX_FDNAMED = $0040;                           // named row type

  //#define ISNULLABLE( flags ) ( flags & FDNULLABLE ? 1 : 0)
  //#define ISDISTINCT( flags ) ( flags & FDDISTINCT ? 1 : 0)

  // Informix Type Estensions:
  SQL_INFX_UDT_FIXED = (-100);
  SQL_INFX_UDT_VARYING = (-101);
  SQL_INFX_UDT_BLOB = (-102);
  SQL_INFX_UDT_CLOB = (-103);
  SQL_INFX_UDT_LVARCHAR = (-104);
  SQL_INFX_RC_ROW = (-105);
  SQL_INFX_RC_COLLECTION = (-106);
  SQL_INFX_RC_LIST = (-107);
  SQL_INFX_RC_SET = (-108);
  SQL_INFX_RC_MULTISET = (-109);
  SQL_INFX_UNSUPPORTED = (-110);

  // typedef void * HINFX_RC; // row & collection handle

  // Informix Connect Attributes Extensions:
  SQL_INFX_OPT_LONGID = 2251;
  SQL_INFX_ATTR_LONGID = SQL_INFX_OPT_LONGID;
  SQL_INFX_ATTR_LEAVE_TRAILING_SPACES = 2252;
  SQL_INFX_ATTR_DEFAULT_UDT_FETCH_TYPE = 2253;
  SQL_INFX_ATTR_ENABLE_SCROLL_CURSORS = 2254;
  SQL_INFX_ATTR_ENABLE_INSERT_CURSORS = 2255;
  SQL_INFX_ATTR_OPTIMIZE_AUTOCOMMIT = 2256;
  SQL_INFX_ATTR_ODBC_TYPES_ONLY = 2257;
  SQL_INFX_ATTR_FETCH_BUFFER_SIZE = 2258;
  SQL_INFX_ATTR_OPTOFC = 2259;
  SQL_INFX_ATTR_OPTMSG = 2260;
  SQL_INFX_ATTR_REPORT_KEYSET_CURSORS = 2261;
  SQL_INFX_ATTR_LO_AUTOMATIC = 2262;
  // Report Standard ODBC Types" Can  be
  // placed in ODBC Connection String options: "ODTYP=1".
  // When SQL_INFX_ATTR_LO_AUTOMATIC = SQL_FALSE then ODBC Driver return the next
  // code for LOB fields: SQL_INFX_UDT_BLOB, SQL_INFX_UDT_CLOB, ...
  // Access is possible through old api: ifx_lo. For more details see:
  // http://www-3.ibm.com/software/data/informix/pubs/library/notes/relnotes/odb0331r31.htm
  // For "Data Direct Informix Wire Protocol ODBC Driver" informix LOB types mapped to standatd ODBC Types:
  // http://www.datadirect-technologies.com/download/docs/odbc64/ODBC64REF/Rinf_wp.html

  SQL_INFX_ATTR_AUTO_FREE = 2263;
  SQL_INFX_ATTR_DEFERRED_PREPARE = 2265;
  // Informix Descriptor Extensions:
  SQL_INFX_ATTR_FLAGS = 1900;                     // UDWORD
  SQL_INFX_ATTR_EXTENDED_TYPE_CODE = 1901;        // UDWORD
  SQL_INFX_ATTR_EXTENDED_TYPE_NAME = 1902;        // UCHAR ptr
  SQL_INFX_ATTR_EXTENDED_TYPE_OWNER = 1903;       // UCHAR ptr
  SQL_INFX_ATTR_EXTENDED_TYPE_ALIGNMENT = 1904;   // UDWORD
  SQL_INFX_ATTR_SOURCE_TYPE_CODE = 1905;          // UDWORD

  // Informix Statement Attributes Extensions:
  SQL_VMB_CHAR_LEN = 2325;
  SQL_INFX_ATTR_VMB_CHAR_LEN = SQL_VMB_CHAR_LEN;
  SQL_INFX_ATTR_MAX_FET_ARR_SIZE = 2326;

  // Informix fOption, SQL_VMB_CHAR_LEN vParam:
  SQL_VMB_CHAR_EXACT = 0;
  SQL_VMB_CHAR_ESTIMATE = 1;

  // Informix row/collection traversal constants:
  SQL_INFX_RC_NEXT = 1;
  SQL_INFX_RC_PRIOR = 2;
  SQL_INFX_RC_FIRST = 3;
  SQL_INFX_RC_LAST = 4;
  SQL_INFX_RC_ABSOLUTE = 5;
  SQL_INFX_RC_RELATIVE = 6;
  SQL_INFX_RC_CURRENT = 7;

  {****************************************************************************
   * Large Object (LO) related structures
   *
   * LO_SPEC: Large object spec structure
   * It is used for creating smartblobs. The user may examin and/or set certain
   * fields of LO_SPEC by using ifx_lo_spec[set|get]_* accessor functions.
   *
   * LO_PTR: Large object pointer structure
   * Identifies the LO and provides ancillary, security-related information.
   *
   * LO_STAT: Large object stat structure
   * It is used in querying attribtes of smartblobs. The user may examin fields
   * herein by using ifx_lo_stat[set|get]_* accessor functions.
   *
   * These structures are opaque to the user. Accessor functions are provided
   * for these structures.
   ****************************************************************************}

  // Informix GetInfo Extensions to obtain length of LO related structures:
  SQL_INFX_LO_SPEC_LENGTH = 2250;                 // UWORD
  SQL_INFX_LO_PTR_LENGTH = 2251;                  // UWORD
  SQL_INFX_LO_STAT_LENGTH = 2252;                 // UWORD

  {****************************************************************************
   * LO Open flags: (see documentation for further explanation)
   *
   * INFX_LO_APPEND   - Positions the seek position to end-of-file + 1. By itself,
   *               it is equivalent to write only mode followed by a seek to the
   *               end of large object. Read opeartions will fail.
   *               You can OR the LO_APPEND flag with another access mode.
   * INFX_LO_WRONLY   - Only write operations are valid on the data.
   * INFX_LO_RDONLY   - Only read operations are valid on the data.
   * INFX_LO_RDWR     - Both read and write operations are valid on the data.
   *
   * INFX_LO_RANDOM   - If set overrides optimizer decision. Indicates that I/O is
   *               random and that the system should not read-ahead.
   * INFX_LO_SEQUENTIAL - If set overrides optimizer decision. Indicates that
   *               reads are sequential in either forward or reverse direction.
   *
   * INFX_LO_FORWARD  - Only used for sequential access. Indicates that the sequential
   *               access will be in a forward direction, i.e. from low offset
   *               to higher offset.
   * INFX_LO_REVERSE  - Only used for sequential access. Indicates that the sequential
   *               access will be in a reverse direction.
   *
   * INFX_LO_BUFFER   - If set overrides optimizer decision. I/O goes through the
   *               buffer pool.
   * INFX_LO_NOBUFFER - If set then I/O does not use the buffer pool.
   ****************************************************************************}

  INFX_LO_APPEND = $1;
  INFX_LO_WRONLY = $2;
  INFX_LO_RDONLY = $4;                            // default
  INFX_LO_RDWR = $8;

  INFX_LO_RANDOM = $20;                           // default is determined by optimizer
  INFX_LO_SEQUENTIAL = $40;                       // default is determined by optimizer

  INFX_LO_FORWARD = $80;                          // default
  INFX_LO_REVERSE = $100;

  INFX_LO_BUFFER = $200;                          // default is determined by optimizer
  INFX_LO_NOBUFFER = $400;                        // default is determined by optimizer

  INFX_LO_DIRTY_READ = $10;
  INFX_LO_NODIRTY_READ = $800;

  {****************************************************************************
   * LO create-time flags:
   *
   * Bitmask - Set/Get via ifx_lo_specset_flags() on LO_SPEC.
   ****************************************************************************}

  INFX_LO_ATTR_LOG = $01;
  INFX_LO_ATTR_NOLOG = $02;
  INFX_LO_ATTR_DELAY_LOG = $04;
  INFX_LO_ATTR_KEEP_LASTACCESS_TIME = $08;
  INFX_LO_ATTR_NOKEEP_LASTACCESS_TIME = $10;
  INFX_LO_ATTR_HIGH_INTEG = $20;
  INFX_LO_ATTR_MODERATE_INTEG = $40;

  {****************************************************************************
   * Symbolic constants for the "lseek" routine
   ****************************************************************************}

  INFX_LO_SEEK_SET = 0;                           // Set curr. pos. to "offset"
  INFX_LO_SEEK_CUR = 1;                           // Set curr. pos. to current + "offset"
  INFX_LO_SEEK_END = 2;                           // Set curr. pos. to EOF + "offset"

  {****************************************************************************
   * Intersolv specific infoTypes for SQLGetInfo
   ****************************************************************************}

  SQL_RESERVED_WORDS = 1011;
  SQL_PSEUDO_COLUMNS = 11012;
  SQL_FROM_RESERVED_WORDS = 11013;
  SQL_WHERE_CLAUSE_TERMINATORS = 11014;
  SQL_COLUMN_FIRST_CHARS = 11015;
  SQL_COLUMN_MIDDLE_CHARS = 11016;
  SQL_TABLE_FIRST_CHARS = 11018;
  SQL_TABLE_MIDDLE_CHARS = 11019;
  SQL_FAST_SPECIAL_COLUMNS = 11021;
  SQL_ACCESS_CONFLICTS = 11022;
  SQL_LOCKING_SYNTAX = 11023;
  SQL_LOCKING_DURATION = 11024;
  SQL_RECORD_OPERATIONS = 11025;
  SQL_QUALIFIER_SYNTAX = 11026;
  ////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////
  // MSSQL
  ////////////////////////////////////////////////////////////////////////////////

    // MSSQL extensions to ODBC API
  SQL_MSSQL_VARIANT = (-150);                     // map to SQL_BINARY/fldBYTES
  ////////////////////////////////////////////////////////////////////////////////


type

  // TODBCDrvType and TDBMSType are internal meta types which used
  // to customize data processing response to custom driver specific

  TODBCDrvType = (
    drvtUnspecified,
    drvtGupta,
    drvtMsSqlServer,
    drvtIbmDb2,
    drvtMsJet,
    drvtMySql,
    drvtMySql3,
    drvtInterbase,
    drvtInformix,
    drvtOracle,
    drvtSybase,
    drvtSQLLite,
    drvtThinkSQL,
    drvtMerantOle,
    drvtPervasive,                                {bad support MixedFetch}
    drvtNexusDbFlashFiler,
    drvtPostgreSQL,                               {very bad driver}
    drvtInterSystemCache,
    drvtMerantDBASE,
    drvtSAPDB
    );

  TDBMSType = (
    dbmstUnspecified,
    dbmstGupta,
    dbmstMsSqlServer,
    dbmstIbmDb2,
    dbmstMySql,
    dbmstMySqlMax,
    dbmstMsAccess,
    dbmstExcel,
    dbmstText,
    dbmstDBase,
    dbmstParadox,
    dbmstOracle,
    dbmstInterbase,
    dbmstInformix,
    dbmstSybase,
    dbmstSQLLite,                                 {Any type is mapped into the text, with maximum length 2048 }
    dbmstThinkSQL,
    dbmstSapDb,
    dbmstPervasiveSQL,
    dbmstFlashFiler,
    dbmstPostgreSQL,
    dbmstInterSystemCache
    );



function GetODBCDriverType(ConnHandle: SQLHDBC): TODBCDrvType;
function GetDBMSType(ConnHandle: SQLHDBC): TDBMSType;

procedure UpdateSQLTypeToDrvSpec(DrvType: TODBCDrvType; var SQL_DT: SQLSMALLINT);


implementation
uses
  SysUtils,
  DB,
  odbcsql,
  odbcsqlext,
  odbcsqlucode,
  ODBCIntf,
  ODBCException,
  ODBCUtils,
  ODBCConsts;

function GetODBCDriverType(ConnHandle: SQLHDBC): TODBCDrvType;
var
  UC_DriverName: string;

{$IFNDEF USE_UNICODE_DRIVER}
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  Buffer: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;
  RetCode: SQLRETURN;
begin
  // Get Driver Name
{$IFNDEF USE_UNICODE_DRIVER}
  RetCode := SQLGetInfoA(ConnHandle, SQL_DRIVER_NAME, @Buffer, SizeOf(Buffer), @BufLen);
{$ELSE}
  RetCode := SQLGetInfoW(ConnHandle, SQL_DRIVER_NAME, @Buffer, SizeOf(Buffer), @BufLen);
{$ENDIF}

  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_DBC, ConnHandle, RetCode);

{$IFNDEF USE_UNICODE_DRIVER}
  UC_DriverName := UpperCase(UnicodeString(AnsiString(PSQLCHAR(@Buffer))));
{$ELSE}
  UC_DriverName := UpperCase(UnicodeString(PSQLWCHAR(@Buffer)));
{$ENDIF}

  // SQL Base:
  if (StrLComp(PChar(UC_DriverName), 'C2GUP', 5) = 0) or
    (StrLComp(PChar(UC_DriverName), 'IVGUP', 5) = 0) {// DataDirect SQLBase ODBC Driver} then
    Result := drvtGupta

    // SQL Server:
  else if (StrLComp(PChar(UC_DriverName), 'SQLSRV', 6) = 0) // SQL Server Microsoft Corporation ODBC Driver
  or (StrLComp(PChar(UC_DriverName), 'IVSS', 4) = 0) // DataDirect SQL Server ODBC Driver
  or (StrLComp(PChar(UC_DriverName), 'IVMSSS', 6) = 0) // DataDirect SQL Server Wire Protocol ODBC Driver
  or                                              // extended comparing
  ((StrLComp(PChar(UC_DriverName), 'NTL', 3) = 0) and (PChar(UC_DriverName)[5] = 'M'))
    {// OpenLink Lite for MS-SQL Server (32 Bit) ODBC Driver}then
    Result := drvtMsSqlServer

    // IBM DB2:
  else if (StrLComp(PChar(UC_DriverName), 'DB2CLI', 6) = 0) // IBM DB2 ODBC DRIVER
  or (StrLComp(PChar(UC_DriverName), 'LIBDB2', 6) = 0) // IBM

  //temporary commented
  //    or (StrLComp(PChar(UC_DriverName), 'IVDB2', 5) = 0) {// DataDirect DB2 Wire Protocol ODBC Driver}
  then
    Result := drvtIbmDb2

    // Microsoft desktop databases:
  else if StrLComp(PChar(UC_DriverName), 'ODBCJT', 6) = 0
    {//(Microsoft Paradox Driver, Microsoft dBASE Driver, ...).}then
    Result := drvtMsJet
      // This driver does not allow SQL_DECIMAL.
    // It driverType usagheb for detecting this situation.

  // My SQL ODBC Version 3 Driver:
  else if StrLComp(PChar(UC_DriverName), 'MYODBC3', 7) = 0 then
    Result := drvtMySql3

    // My SQL:
  else if StrLComp(PChar(UC_DriverName), 'MYODBC', 6) = 0 then
    Result := drvtMySql

    // INFORMIX:
  else if (StrLComp(PChar(UC_DriverName), 'ICLI', 4) = 0) // "INFORMIX 3.32 32 BIT" ODBC Driver
  or (StrLComp(PChar(UC_DriverName), 'IVINF', 5) = 0) // DataDirect Informix ODBC Driver
  or (StrLComp(PChar(UC_DriverName), 'IVIFCL', 6) = 0) // DataDirect Informix Wire Protocol ODBC Driver
  or (StrLComp(PChar(UC_DriverName), 'PDINF', 5) = 0) // INTERSOLV Inc ODBC Driver (1997. Now is DataDirect)
  or                                              // extended comparing
  ((StrLComp(PChar(UC_DriverName), 'NTL', 3) = 0) and (PChar(UC_DriverName)[5] = 'I'))
    {// OpenLink Lite for Informix 7 (32 Bit) ODBC Driver}then
    Result := drvtInformix

    // SYBASE
  else if (StrLComp(PChar(UC_DriverName), 'SYODASE', 7) = 0) then // SYBASE ACE ODBC Driver
    Result := drvtSybase

    // SQL Lite:
  else if StrLComp(PChar(UC_DriverName), 'SQLITEODBC', 10) = 0 then
    Result := drvtSQLLite

    // INTERBASE:
  else if StrLComp(PChar(UC_DriverName), 'IB6ODBC', 7) = 0 {// Easysoft ODBC Driver} then
    Result := drvtInterbase
  else if StrLComp(PChar(UC_DriverName), 'ODBCJDBC', 8) = 0 then {// IBPhoenix ODBC Driver: http://www.ibphoenix.com/}
    Result := drvtInterbase
  else if StrLComp(PChar(UC_DriverName), 'IB6XTG', 6) = 0 then
    // Open Firebird, Interbase6 ODBC Driver: http://www.xtgsystems.com/
    Result := drvtInterbase
      // bug in bcd: returned uncorrected BCD column info (SQLDescribeCol)
    // fConnectionOptionsDrv[coMaxBCD] := osOn; // - added handles in BindResultSet

  // Think SQL:
  else if StrLComp(PChar(UC_DriverName), 'THINKSQL', 8) = 0 {// ThinkSQL ODBC Driver} then
    Result := drvtThinkSQL

    // ORACLE:
  else if (StrLComp(PChar(UC_DriverName), 'SQORA', 5) = 0) // Oracle ODBC Driver
  or (StrLComp(PChar(UC_DriverName), 'MSORCL', 6) = 0) // Microsoft ODBC for Oracle
  or (StrLComp(PChar(UC_DriverName), 'IVOR', 4) = 0) // DataDirect Oracle ODBC Driver
  or (StrLComp(PChar(UC_DriverName), 'IVORA', 5) = 0) {// DataDirect Oracle Wire Protocol ODBC Driver} then
    Result := drvtOracle

  else if (StrLComp(PChar(UC_DriverName), 'INOLE', 5) = 0) {// MERANT ODBC-OLE DB Adapter Driver} then
    Result := drvtMerantOle

    // SYBASE:
    {
       ( StrLComp(buffer, 'IVASE', 5) = 0 )  // DataDirect SybaseWire Protocol ODBC Driver
    }
    // BTRIEVE:
    {
    ( StrLComp(buffer, 'IVBTR', 5) = 0 )  // DataDirect Btrieve (*.dta) ODBC Driver
    }
    // PROGRESS:
    {
    ( StrLComp(buffer, 'IVPRO', 5) = 0 )  // DataDirect Progress ODBC Driver
    }
    // OTHER:
    {
    Microsoft Visual FoxPro Driver (*.dbf)    'VFPODBC'
    DataDirect dBASE File (*.dbf)             'IVDBF'
    DataDirect FoxPro 3.0 database (*.dbc)    'IVDBF'
    DataDirect Excel Workbook (*.xls)         'IVXLWB'
    DataDirect Paradox File (*.db)            'IVDP'
    DataDirect Text File (*.*)                'IVTXT'
    DataDirect XML                            'IVXML'
    SQLLite                                   'SQLITEODBC'
    ThinkSQL                                  'THINKSQL'
    }

  // Pervasive.SQL
  else if (StrLComp(PChar(UC_DriverName), 'W3ODBCCI', 8) = 0) {// Pervasive.SQL ODBC Driver Client Interface } then
    Result := drvtPervasive
  else if (StrLComp(PChar(UC_DriverName), 'W3ODBCEI', 8) = 0) {// Pervasive.SQL ODBC Driver Engine Interface } then
    Result := drvtPervasive

    // FlasfFiller
  else if (StrLComp(PChar(UC_DriverName), 'NXODBCDRIVER', 12) = 0) {// NexusDb FlashFiler Driver } then
    Result := drvtNexusDbFlashFiler

    // PostgreSQL
  else if (StrLComp(PChar(UC_DriverName), 'PSQLODBC', 8) = 0) then
    Result := drvtPostgreSQL

    // Cache
  else if (StrLComp(PChar(UC_DriverName), 'CACHEODBC', 9) = 0) then
    Result := drvtInterSystemCache

    // Merant Clipper, DBASE, FoxPro
  else if (StrLComp(PChar(UC_DriverName), 'IVDBF', 5) = 0) then {MERANT DBASE ODBC DRIVER}
    Result := drvtMerantDBASE                     // Clipper, DBASE, FoxPro
    // 'SAP DB' ODBC Driver by SAP AG
  else if (GetDBMSType(ConnHandle) = dbmstSAPDB) and
    (StrLComp(PChar(UC_DriverName), 'SQLOD', 5) = 0) then {'SAP DB' ODBC Driver by SAP AG}
    Result := drvtSAPDB
  else
    Result := drvtUnspecified;
end;

function GetDBMSType(ConnHandle: SQLHDBC): TDBMSType;
var
  UC_DBMSName: string;
{$IFNDEF USE_UNICODE_DRIVER}
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  Buffer: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;
  RetCode: SQLRETURN;
begin
  // Get DBMS Name
{$IFNDEF USE_UNICODE_DRIVER}
  RetCode := SQLGetInfoA(ConnHandle, SQL_DBMS_NAME, @Buffer, SizeOf(Buffer), @BufLen);
{$ELSE}
  RetCode := SQLGetInfoW(ConnHandle, SQL_DBMS_NAME, @Buffer, SizeOf(Buffer), @BufLen);
{$ENDIF}
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_DBC, ConnHandle, RetCode);

{$IFNDEF USE_UNICODE_DRIVER}
  UC_DBMSName := UpperCase(UnicodeString(AnsiString(PSQLCHAR(@Buffer))));
{$ELSE}
  UC_DBMSName := UpperCase(UnicodeString(PSQLWCHAR(@Buffer)));
{$ENDIF}

  if UC_DBMSName = 'SQLBASE' then
    Result := dbmstGupta
  else if UC_DBMSName = 'MICROSOFT SQL SERVER' then
    Result := dbmstMsSqlServer
  else if UC_DBMSName = 'IBMDB2' then
    Result := dbmstIbmDb2
  else if (StrLComp(PChar(UC_DBMSName), 'MYSQL', 5) = 0) then
    Result := dbmstMySql

    // JET databases
  else if UC_DBMSName = 'ACCESS' then
    Result := dbmstMsAccess
  else if UC_DBMSName = 'EXCEL' then
    Result := dbmstExcel
  else if UC_DBMSName = 'TEXT' then
    Result := dbmstText
  else if (StrLComp(PChar(UC_DBMSName), 'DBASE', 5) = 0) then // DBASE II, IV, V
    Result := dbmstDBase
  else if UC_DBMSName = 'PARADOX' then
    Result := dbmstParadox

    // Ohter databases, not fully tested
  else if UC_DBMSName = 'ORACLE' then
    Result := dbmstOracle
  else if UC_DBMSName = 'INFORMIX' then
    Result := dbmstInformix
  else if UC_DBMSName = 'INTERBASE' then
    Result := dbmstInterbase
  else if (StrLComp(PChar(UC_DBMSName), 'FIREBIRD', 8) = 0) then // 'FIREBIRD / INTERBASE(R)'
    Result := dbmstInterbase

    // Ohter databases, not tested at all
  else if UC_DBMSName = 'SYBASE' then
    Result := dbmstSybase
  else if UC_DBMSName = 'SQLITE' then
    Result := dbmstSQLLite
  else if (StrLComp(PChar(UC_DBMSName), 'THINKSQL', 8) = 0) then // 'THINKSQL RELATIONAL DATABASE MANAGEMENT SYSTEM'
    Result := dbmstThinkSQL
  else if UC_DBMSName = 'SAP DB' then
    Result := dbmstSAPDB
  else if UC_DBMSName = 'PERVASIVE.SQL' then
    Result := dbmstPervasiveSQL
  else if UC_DBMSName = 'TURBOPOWER FLASHFILER 2' then
    Result := dbmstFlashFiler
  else if (StrLComp(PChar(UC_DBMSName), 'POSTGRESQL', 10) = 0) then
    Result := dbmstPostgreSQL
  else if UC_DBMSName = 'INTERSYSTEMS CACHE' then
    Result := dbmstInterSystemCache
  else
    Result := dbmstUnspecified;
end;

procedure UpdateSQLTypeToDrvSpec(DrvType: TODBCDrvType; var SQL_DT: SQLSMALLINT);
begin
  case DrvType of
    // INFORMIX Driver Specific ////////////////////////////////////////////////////
    drvtInformix:
      case SQL_DT of
        // SQL_INFX_UDT_FIXED = (-100);
        SQL_INFX_UDT_FIXED:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_UDT_VARYING = (-101);
        SQL_INFX_UDT_VARYING:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_UDT_BLOB = (-102);
        SQL_INFX_UDT_BLOB:
          SQL_DT := SQL_LONGVARBINARY;

        // SQL_INFX_UDT_CLOB = (-103);
        SQL_INFX_UDT_CLOB:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_UDT_LVARCHAR = (-104);
        SQL_INFX_UDT_LVARCHAR:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_RC_ROW = (-105);
        SQL_INFX_RC_ROW:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_RC_COLLECTION = (-106);
        SQL_INFX_RC_COLLECTION:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_RC_LIST = (-107);
        SQL_INFX_RC_LIST:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_RC_SET = (-108);
        SQL_INFX_RC_SET:
          SQL_DT := SQL_LONGVARCHAR;

        // SQL_INFX_RC_MULTISET = (-109);
        SQL_INFX_RC_MULTISET:
          SQL_DT := SQL_LONGVARCHAR;
      end;
    // MSSQL Driver Specific ///////////////////////////////////////////////////////
    drvtMsSqlServer:
      if SQL_DT = SQL_MSSQL_VARIANT then
        SQL_DT := SQL_BINARY;

  end;
end;

end.
