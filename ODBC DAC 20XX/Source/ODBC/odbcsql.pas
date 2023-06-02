{*******************************************************}
{                                                       }
{       "sql.h"                                         }
{                                                       }
{   Translated to Object Pascal by Andrey Zubik         }
{   e-mail: andrey.zubik@gmail.com                      } 
{                                                       }
{*******************************************************}

unit odbcsql;

interface
uses
  odbcsqltypes;


const

  { special length/indicator values }
  SQL_NULL_DATA = -1;
  SQL_DATA_AT_EXEC = -2;

  { return values from functions }
  SQL_SUCCESS = 0;
  SQL_SUCCESS_WITH_INFO = 1;
  SQL_NO_DATA = 100;
  SQL_ERROR = -1;
  SQL_INVALID_HANDLE = -2;
  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;

  { test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO }
  // #define SQL_SUCCEEDED(rc) (((rc)&(~1))==0)
function SQL_SUCCEEDED(RetCode: SQLRETURN): Boolean;

const
  { flags for null-terminated string }
  SQL_NTS = -3;
  SQL_NTSL = LongInt(-3);

  { maximum message length }
  SQL_MAX_MESSAGE_LENGTH = 512;

  { date/time length constants }
  SQL_DATE_LEN = 10;
  SQL_TIME_LEN = 8;                               // add P+1 if precision is nonzero
  SQL_TIMESTAMP_LEN = 19;                         // add P+1 if precision is nonzero

  { handle type identifiers }
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

  { environment attribute }
  SQL_ATTR_OUTPUT_NTS = 10001;

  { connection attributes }
  SQL_ATTR_AUTO_IPD = 10001;
  SQL_ATTR_METADATA_ID = 10014;

  { statement attributes }
  SQL_ATTR_APP_ROW_DESC = 10010;
  SQL_ATTR_APP_PARAM_DESC = 10011;
  SQL_ATTR_IMP_ROW_DESC = 10012;
  SQL_ATTR_IMP_PARAM_DESC = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE = -1;
  SQL_ATTR_CURSOR_SENSITIVITY = -2;

  { SQL_ATTR_CURSOR_SCROLLABLE values }
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE = 1;

  { identifiers of fields in the SQL descriptor }
  SQL_DESC_COUNT = 1001;
  SQL_DESC_TYPE = 1002;
  SQL_DESC_LENGTH = 1003;
  SQL_DESC_OCTET_LENGTH_PTR = 1004;
  SQL_DESC_PRECISION = 1005;
  SQL_DESC_SCALE = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE = 1008;
  SQL_DESC_INDICATOR_PTR = 1009;
  SQL_DESC_DATA_PTR = 1010;
  SQL_DESC_NAME = 1011;
  SQL_DESC_UNNAMED = 1012;
  SQL_DESC_OCTET_LENGTH = 1013;
  SQL_DESC_ALLOC_TYPE = 1099;

  { identifiers of fields in the diagnostics area }
  SQL_DIAG_RETURNCODE = 1;
  SQL_DIAG_NUMBER = 2;
  SQL_DIAG_ROW_COUNT = 3;
  SQL_DIAG_SQLSTATE = 4;
  SQL_DIAG_NATIVE = 5;
  SQL_DIAG_MESSAGE_TEXT = 6;
  SQL_DIAG_DYNAMIC_FUNCTION = 7;
  SQL_DIAG_CLASS_ORIGIN = 8;
  SQL_DIAG_SUBCLASS_ORIGIN = 9;
  SQL_DIAG_CONNECTION_NAME = 10;
  SQL_DIAG_SERVER_NAME = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

  { dynamic function codes }
  SQL_DIAG_ALTER_DOMAIN = 3;
  SQL_DIAG_ALTER_TABLE = 4;
  SQL_DIAG_CALL = 7;
  SQL_DIAG_CREATE_ASSERTION = 6;
  SQL_DIAG_CREATE_CHARACTER_SET = 8;
  SQL_DIAG_CREATE_COLLATION = 10;
  SQL_DIAG_CREATE_DOMAIN = 23;
  SQL_DIAG_CREATE_INDEX = -1;
  SQL_DIAG_CREATE_SCHEMA = 64;
  SQL_DIAG_CREATE_TABLE = 77;
  SQL_DIAG_CREATE_TRANSLATION = 79;
  SQL_DIAG_CREATE_VIEW = 84;
  SQL_DIAG_DELETE_WHERE = 19;
  SQL_DIAG_DROP_ASSERTION = 24;
  SQL_DIAG_DROP_CHARACTER_SET = 25;
  SQL_DIAG_DROP_COLLATION = 26;
  SQL_DIAG_DROP_DOMAIN = 27;
  SQL_DIAG_DROP_INDEX = -2;
  SQL_DIAG_DROP_SCHEMA = 31;
  SQL_DIAG_DROP_TABLE = 32;
  SQL_DIAG_DROP_TRANSLATION = 33;
  SQL_DIAG_DROP_VIEW = 36;
  SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
  SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
  SQL_DIAG_GRANT = 48;
  SQL_DIAG_INSERT = 50;
  SQL_DIAG_REVOKE = 59;
  SQL_DIAG_SELECT_CURSOR = 85;
  SQL_DIAG_UNKNOWN_STATEMENT = 0;
  SQL_DIAG_UPDATE_WHERE = 82;

  { SQL data type codes }
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR = 1;
  SQL_NUMERIC = 2;
  SQL_DECIMAL = 3;
  SQL_INTEGER = 4;
  SQL_SMALLINT = 5;
  SQL_FLOAT = 6;
  SQL_REAL = 7;
  SQL_DOUBLE = 8;

  SQL_DATETIME = 9;

  SQL_VARCHAR = 12;

  { One-parameter shortcuts for date/time data types }
  SQL_TYPE_DATE = 91;
  SQL_TYPE_TIME = 92;
  SQL_TYPE_TIMESTAMP = 93;

  { Statement attribute values for cursor sensitivity }
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE = 2;

  { GetTypeInfo() request for all data types }
  SQL_ALL_TYPES = 0;

  { Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData() }
  SQL_DEFAULT = 99;

  { SQLGetData() code indicating that the application row descriptor
    specifies the data type }
  SQL_ARD_TYPE = -99;

  { SQL date/time type subcodes }
  SQL_CODE_DATE = 1;
  SQL_CODE_TIME = 2;
  SQL_CODE_TIMESTAMP = 3;

  { CLI option values }
  SQL_FALSE = 0;
  SQL_TRUE = 1;

  { values of NULLABLE field in descriptor }
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;

  { Value returned by SQLGetTypeInfo() to denote that it is
    not known whether or not a data type supports null values. }
  SQL_NULLABLE_UNKNOWN = 2;

  { Values returned by SQLGetTypeInfo() to show WHERE clause supported }
  SQL_PRED_NONE = 0;
  SQL_PRED_CHAR = 1;
  SQL_PRED_BASIC = 2;

  { values of UNNAMED field in descriptor }
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

  { values of ALLOC_TYPE field in descriptor }
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

  { FreeStmt() options }
  SQL_CLOSE = 0;
  SQL_DROP = 1;
  SQL_UNBIND = 2;
  SQL_RESET_PARAMS = 3;

  { Codes used for FetchOrientation in SQLFetchScroll(),
    and in SQLDataSources() }
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

  { Other codes used for FetchOrientation in SQLFetchScroll() }
  SQL_FETCH_LAST = 3;
  SQL_FETCH_PRIOR = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;

  { SQLEndTran() options }
  SQL_COMMIT = 0;
  SQL_ROLLBACK = 1;

  { null handles returned by SQLAllocHandle() }
  SQL_NULL_HENV = 0;
  SQL_NULL_HDBC = 0;
  SQL_NULL_HSTMT = 0;
  SQL_NULL_HDESC = 0;

  { null handle used in place of parent handle when allocating HENV }
  SQL_NULL_HANDLE = LongInt(0);

  { Values that may appear in the result set of SQLSpecialColumns() }
  SQL_SCOPE_CURROW = 0;
  SQL_SCOPE_TRANSACTION = 1;
  SQL_SCOPE_SESSION = 2;

  SQL_PC_UNKNOWN = 0;
  SQL_PC_NON_PSEUDO = 1;
  SQL_PC_PSEUDO = 2;

  { Reserved value for the IdentifierType argument of SQLSpecialColumns() }
  SQL_ROW_IDENTIFIER = 1;

  { Reserved values for UNIQUE argument of SQLStatistics() }
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL = 1;

  { Values that may appear in the result set of SQLStatistics() }
  SQL_INDEX_CLUSTERED = 1;
  SQL_INDEX_HASHED = 2;
  SQL_INDEX_OTHER = 3;

  { SQLGetFunctions() values to identify ODBC APIs }
  SQL_API_SQLALLOCCONNECT = 1;
  SQL_API_SQLALLOCENV = 2;
  SQL_API_SQLALLOCHANDLE = 1001;
  SQL_API_SQLALLOCSTMT = 3;
  SQL_API_SQLBINDCOL = 4;
  SQL_API_SQLBINDPARAM = 1002;
  SQL_API_SQLCANCEL = 5;
  SQL_API_SQLCLOSECURSOR = 1003;
  SQL_API_SQLCOLATTRIBUTE = 6;
  SQL_API_SQLCOLUMNS = 40;
  SQL_API_SQLCONNECT = 7;
  SQL_API_SQLCOPYDESC = 1004;
  SQL_API_SQLDATASOURCES = 57;
  SQL_API_SQLDESCRIBECOL = 8;
  SQL_API_SQLDISCONNECT = 9;
  SQL_API_SQLENDTRAN = 1005;
  SQL_API_SQLERROR = 10;
  SQL_API_SQLEXECDIRECT = 11;
  SQL_API_SQLEXECUTE = 12;
  SQL_API_SQLFETCH = 13;
  SQL_API_SQLFETCHSCROLL = 1021;
  SQL_API_SQLFREECONNECT = 14;
  SQL_API_SQLFREEENV = 15;
  SQL_API_SQLFREEHANDLE = 1006;
  SQL_API_SQLFREESTMT = 16;
  SQL_API_SQLGETCONNECTATTR = 1007;
  SQL_API_SQLGETCONNECTOPTION = 42;
  SQL_API_SQLGETCURSORNAME = 17;
  SQL_API_SQLGETDATA = 43;
  SQL_API_SQLGETDESCFIELD = 1008;
  SQL_API_SQLGETDESCREC = 1009;
  SQL_API_SQLGETDIAGFIELD = 1010;
  SQL_API_SQLGETDIAGREC = 1011;
  SQL_API_SQLGETENVATTR = 1012;
  SQL_API_SQLGETFUNCTIONS = 44;
  SQL_API_SQLGETINFO = 45;
  SQL_API_SQLGETSTMTATTR = 1014;
  SQL_API_SQLGETSTMTOPTION = 46;
  SQL_API_SQLGETTYPEINFO = 47;
  SQL_API_SQLNUMRESULTCOLS = 18;
  SQL_API_SQLPARAMDATA = 48;
  SQL_API_SQLPREPARE = 19;
  SQL_API_SQLPUTDATA = 49;
  SQL_API_SQLROWCOUNT = 20;
  SQL_API_SQLSETCONNECTATTR = 1016;
  SQL_API_SQLSETCONNECTOPTION = 50;
  SQL_API_SQLSETCURSORNAME = 21;
  SQL_API_SQLSETDESCFIELD = 1017;
  SQL_API_SQLSETDESCREC = 1018;
  SQL_API_SQLSETENVATTR = 1019;
  SQL_API_SQLSETPARAM = 22;
  SQL_API_SQLSETSTMTATTR = 1020;
  SQL_API_SQLSETSTMTOPTION = 51;
  SQL_API_SQLSPECIALCOLUMNS = 52;
  SQL_API_SQLSTATISTICS = 53;
  SQL_API_SQLTABLES = 54;
  SQL_API_SQLTRANSACT = 23;

  { Information requested by SQLGetInfo() }
  SQL_MAX_DRIVER_CONNECTIONS = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME = 2;
  SQL_FETCH_DIRECTION = 8;
  SQL_SERVER_NAME = 13;
  SQL_SEARCH_PATTERN_ESCAPE = 14;
  SQL_DBMS_NAME = 17;
  SQL_DBMS_VER = 18;
  SQL_ACCESSIBLE_TABLES = 19;
  SQL_ACCESSIBLE_PROCEDURES = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR = 23;
  SQL_DATA_SOURCE_READ_ONLY = 25;
  SQL_DEFAULT_TXN_ISOLATION = 26;
  SQL_IDENTIFIER_CASE = 28;
  SQL_IDENTIFIER_QUOTE_CHAR = 29;
  SQL_MAX_COLUMN_NAME_LEN = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN = 35;
  SQL_SCROLL_CONCURRENCY = 43;
  SQL_TXN_CAPABLE = 46;
  SQL_TRANSACTION_CAPABLE = SQL_TXN_CAPABLE;
  SQL_USER_NAME = 47;
  SQL_TXN_ISOLATION_OPTION = 72;
  SQL_TRANSACTION_ISOLATION_OPTION = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY = 73;
  SQL_GETDATA_EXTENSIONS = 81;
  SQL_NULL_COLLATION = 85;
  SQL_ALTER_TABLE = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
  SQL_SPECIAL_CHARACTERS = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE = 101;
  SQL_MAX_INDEX_SIZE = 102;
  SQL_MAXIMUM_INDEX_SIZE = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE = 104;
  SQL_MAXIMUM_ROW_SIZE = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES = 115;
  SQL_OUTER_JOIN_CAPABILITIES = SQL_OJ_CAPABILITIES;

  SQL_XOPEN_CLI_YEAR = 10000;
  SQL_CURSOR_SENSITIVITY = 10001;
  SQL_DESCRIBE_PARAMETER = 10002;
  SQL_CATALOG_NAME = 10003;
  SQL_COLLATION_SEQ = 10004;
  SQL_MAX_IDENTIFIER_LEN = 10005;
  SQL_MAXIMUM_IDENTIFIER_LENGTH = SQL_MAX_IDENTIFIER_LEN;

  { SQL_ALTER_TABLE bitmasks }
  SQL_AT_ADD_COLUMN = LongInt($00000001);
  SQL_AT_DROP_COLUMN = LongInt($00000002);
  SQL_AT_ADD_CONSTRAINT = LongInt($00000008);

  { The following bitmasks are ODBC extensions and defined in sqlext.h }
  // SQL_AT_COLUMN_SINGLE		     = LongInt($00000020);
  // SQL_AT_ADD_COLUMN_DEFAULT		     = LongInt($00000040);
  // SQL_AT_ADD_COLUMN_COLLATION	     = LongInt($00000080);
  // SQL_AT_SET_COLUMN_DEFAULT		     = LongInt($00000100);
  // SQL_AT_DROP_COLUMN_DEFAULT		     = LongInt($00000200);
  // SQL_AT_DROP_COLUMN_CASCADE		     = LongInt($00000400);
  // SQL_AT_DROP_COLUMN_RESTRICT	     = LongInt($00000800);
  // SQL_AT_ADD_TABLE_CONSTRAINT	     = LongInt($00001000);
  // SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE    = LongInt($00002000);
  // SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT   = LongInt($00004000);
  // SQL_AT_CONSTRAINT_NAME_DEFINITION	     = LongInt($00008000);
  // SQL_AT_CONSTRAINT_INITIALLY_DEFERRED    = LongInt($00010000);
  // SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE   = LongInt($00020000);
  // SQL_AT_CONSTRAINT_DEFERRABLE	     = LongInt($00040000);
  // SQL_AT_CONSTRAINT_NON_DEFERRABLE	     = LongInt($00080000);

  { SQL_ASYNC_MODE values }
  SQL_AM_NONE = 0;
  SQL_AM_CONNECTION = 1;
  SQL_AM_STATEMENT = 2;

  { SQL_CURSOR_COMMIT_BEHAVIOR values }
  SQL_CB_DELETE = 0;
  SQL_CB_CLOSE = 1;
  SQL_CB_PRESERVE = 2;

  { SQL_FETCH_DIRECTION bitmasks }
  SQL_FD_FETCH_NEXT = LongInt($00000001);
  SQL_FD_FETCH_FIRST = LongInt($00000002);
  SQL_FD_FETCH_LAST = LongInt($00000004);
  SQL_FD_FETCH_PRIOR = LongInt($00000008);
  SQL_FD_FETCH_ABSOLUTE = LongInt($00000010);
  SQL_FD_FETCH_RELATIVE = LongInt($00000020);

  { SQL_GETDATA_EXTENSIONS bitmasks }
  SQL_GD_ANY_COLUMN = LongInt($00000001);
  SQL_GD_ANY_ORDER = LongInt($00000002);

  { SQL_IDENTIFIER_CASE values }
  SQL_IC_UPPER = 1;
  SQL_IC_LOWER = 2;
  SQL_IC_SENSITIVE = 3;
  SQL_IC_MIXED = 4;

  { SQL_OJ_CAPABILITIES bitmasks
    NB: this means 'outer join', not what  you may be thinking }
  SQL_OJ_LEFT = LongInt($00000001);
  SQL_OJ_RIGHT = LongInt($00000002);
  SQL_OJ_FULL = LongInt($00000004);
  SQL_OJ_NESTED = LongInt($00000008);
  SQL_OJ_NOT_ORDERED = LongInt($00000010);
  SQL_OJ_INNER = LongInt($00000020);
  SQL_OJ_ALL_COMPARISON_OPS = LongInt($00000040);

  { SQL_SCROLL_CONCURRENCY bitmasks }
  SQL_SCCO_READ_ONLY = LongInt($00000001);
  SQL_SCCO_LOCK = LongInt($00000002);
  SQL_SCCO_OPT_ROWVER = LongInt($00000004);
  SQL_SCCO_OPT_VALUES = LongInt($00000008);

  { SQL_TXN_CAPABLE values }
  SQL_TC_NONE = 0;
  SQL_TC_DML = 1;
  SQL_TC_ALL = 2;
  SQL_TC_DDL_COMMIT = 3;
  SQL_TC_DDL_IGNORE = 4;

  { SQL_TXN_ISOLATION_OPTION bitmasks }
  SQL_TXN_READ_UNCOMMITTED = LongInt($00000001);
  SQL_TRANSACTION_READ_UNCOMMITTED = SQL_TXN_READ_UNCOMMITTED;
  SQL_TXN_READ_COMMITTED = LongInt($00000002);
  SQL_TRANSACTION_READ_COMMITTED = SQL_TXN_READ_COMMITTED;
  SQL_TXN_REPEATABLE_READ = LongInt($00000004);
  SQL_TRANSACTION_REPEATABLE_READ = SQL_TXN_REPEATABLE_READ;
  SQL_TXN_SERIALIZABLE = LongInt($00000008);
  SQL_TRANSACTION_SERIALIZABLE = SQL_TXN_SERIALIZABLE;

  { SQL_NULL_COLLATION values }
  SQL_NC_HIGH = 0;
  SQL_NC_LOW = 1;


type

  TSQLAllocConnect = function(
    EnvironmentHandle: SQLHENV;
    ConnectionHandle: PSQLHDBC
    ): SQLRETURN; stdcall;

  TSQLAllocEnv = function(
    EnvironmentHandle: PSQLHENV
    ): SQLRETURN; stdcall;

  TSQLAllocHandle = function(
    HandleType: SQLSMALLINT;
    InputHandle: SQLHANDLE;
    OutputHandle: PSQLHANDLE
    ): SQLRETURN; stdcall;

  TSQLAllocStmt = function(
    ConnectionHandle: SQLHDBC;
    StatementHandle: PSQLHSTMT
    ): SQLRETURN; stdcall;

  TSQLBindCol = function(
    StatementHandle: SQLHSTMT;
    ColumnNumber: SQLUSMALLINT;
    TargetType: SQLSMALLINT;
    TargetValue: SQLPOINTER;
    BufferLength: SQLINTEGER;
    StrLen_or_Ind: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLBindParam = function(
    StatementHandle: SQLHSTMT;
    ParameterNumber: SQLUSMALLINT;
    ValueType: SQLSMALLINT;
    ParameterType: SQLSMALLINT;
    LengthPrecision: SQLUINTEGER;
    ParameterScale: SQLSMALLINT;
    ParameterValue: SQLPOINTER;
    StrLen_or_Ind: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLCancel = function(StatementHandle: SQLHSTMT): SQLRETURN; stdcall;

  TSQLCloseCursor = function(StatementHandle: SQLHSTMT): SQLRETURN; stdcall;

  TSQLColAttribute = function(
    StatementHandle: SQLHSTMT;
    ColumnNumber: SQLUSMALLINT;
    FieldIdentifier: SQLUSMALLINT;
    CharacterAttribute: SQLPOINTER;
    BufferLength: SQLSMALLINT;
    StringLength: PSQLSMALLINT;
    NumericAttribute: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLColumns = function(
    StatementHandle: SQLHSTMT;
    CatalogName: PSQLCHAR;
    NameLength1: SQLSMALLINT;
    SchemaName: PSQLCHAR;
    NameLength2: SQLSMALLINT;
    TableName: PSQLCHAR;
    NameLength3: SQLSMALLINT;
    ColumnName: PSQLCHAR;
    NameLength4: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLConnect = function(
    ConnectionHandle: SQLHDBC;
    ServerName: PSQLCHAR;
    NameLength1: SQLSMALLINT;
    UserName: PSQLCHAR;
    NameLength2: SQLSMALLINT;
    Authentication: PSQLCHAR;
    NameLength3: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLCopyDesc = function(
    SourceDescHandle: SQLHDESC;
    TargetDescHandle: SQLHDESC
    ): SQLRETURN; stdcall;

  TSQLDataSources = function(
    EnvironmentHandle: SQLHENV;
    Direction: SQLUSMALLINT;
    ServerName: PSQLCHAR;
    BufferLength1: SQLSMALLINT;
    NameLength1: PSQLSMALLINT;
    Description: PSQLCHAR;
    BufferLength2: SQLSMALLINT;
    NameLength2: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDescribeCol = function(
    StatementHandle: SQLHSTMT;
    ColumnNumber: SQLUSMALLINT;
    ColumnName: PSQLCHAR;
    BufferLength: SQLSMALLINT;
    NameLength: PSQLSMALLINT;
    DataType: PSQLSMALLINT;
    ColumnSize: PSQLUINTEGER;
    DecimalDigits: PSQLSMALLINT;
    Nullable: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLDisconnect = function(
    ConnectionHandle: SQLHDBC
    ): SQLRETURN; stdcall;

  TSQLEndTran = function(
    HandleType: SQLSMALLINT;
    Handle: SQLHANDLE;
    CompletionType: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLError = function(
    EnvironmentHandle: SQLHENV;
    ConnectionHandle: SQLHDBC;
    StatementHandle: SQLHSTMT;
    Sqlstate: PSQLCHAR;
    NativeError: PSQLINTEGER;
    MessageText: PSQLCHAR;
    BufferLength: SQLSMALLINT;
    TextLength: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLExecDirect = function(
    StatementHandle: SQLHSTMT;
    StatementText: PSQLCHAR;
    TextLength: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLExecute = function(StatementHandle: SQLHSTMT): SQLRETURN; stdcall;

  TSQLFetch = function(StatementHandle: SQLHSTMT): SQLRETURN; stdcall;

  TSQLFetchScroll = function(
    StatementHandle: SQLHSTMT;
    FetchOrientation: SQLSMALLINT;
    FetchOffset: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLFreeConnect = function(
    ConnectionHandle: SQLHDBC
    ): SQLRETURN; stdcall;

  TSQLFreeEnv = function(
    EnvironmentHandle: SQLHENV
    ): SQLRETURN; stdcall;

  TSQLFreeHandle = function(
    HandleType: SQLSMALLINT;
    Handle: SQLHANDLE
    ): SQLRETURN; stdcall;

  TSQLFreeStmt = function(
    StatementHandle: SQLHSTMT;
    Option: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetConnectAttr = function(
    ConnectionHandle: SQLHDBC;
    Attribute: SQLINTEGER;
    Value: SQLPOINTER;
    BufferLength: SQLINTEGER;
    StringLength: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetConnectOption = function(
    ConnectionHandle: SQLHDBC;
    Option: SQLUSMALLINT;
    Value: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLGetCursorName = function(
    StatementHandle: SQLHSTMT;
    CursorName: PSQLCHAR;
    BufferLength: SQLSMALLINT;
    NameLength: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetData = function(
    StatementHandle: SQLHSTMT;
    ColumnNumber: SQLUSMALLINT;
    TargetType: SQLSMALLINT;
    TargetValue: SQLPOINTER;
    BufferLength: SQLINTEGER;
    StrLen_or_Ind: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetDescField = function(
    DescriptorHandle: SQLHDESC;
    RecNumber: SQLSMALLINT;
    FieldIdentifier: SQLSMALLINT;
    Value: SQLPOINTER;
    BufferLength: SQLINTEGER;
    StringLength: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetDescRec = function(
    DescriptorHandle: SQLHDESC;
    RecNumber: SQLSMALLINT;
    Name: PSQLCHAR;
    BufferLength: SQLSMALLINT;
    StringLength: PSQLSMALLINT;
    AType: PSQLSMALLINT;
    SubType: PSQLSMALLINT;
    Length: PSQLINTEGER;
    Precision: PSQLSMALLINT;
    Scale: PSQLSMALLINT;
    Nullable: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetDiagField = function(
    HandleType: SQLSMALLINT;
    Handle: SQLHANDLE;
    RecNumber: SQLSMALLINT;
    DiagIdentifier: SQLSMALLINT;
    DiagInfo: SQLPOINTER;
    BufferLength: SQLSMALLINT;
    StringLength: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetDiagRec = function(
    HandleType: SQLSMALLINT;
    Handle: SQLHANDLE;
    RecNumber: SQLSMALLINT;
    Sqlstate: PSQLCHAR;
    NativeError: PSQLINTEGER;
    MessageText: PSQLCHAR;
    BufferLength: SQLSMALLINT;
    TextLength: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetEnvAttr = function(
    EnvironmentHandle: SQLHENV;
    Attribute: SQLINTEGER;
    Value: SQLPOINTER;
    BufferLength: SQLINTEGER;
    StringLength: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetFunctions = function(
    ConnectionHandle: SQLHDBC;
    FunctionId: SQLUSMALLINT;
    Supported: PSQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetInfo = function(
    ConnectionHandle: SQLHDBC;
    InfoType: SQLUSMALLINT;
    InfoValue: SQLPOINTER;
    BufferLength: SQLSMALLINT;
    StringLength: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLGetStmtAttr = function(
    StatementHandle: SQLHSTMT;
    Attribute: SQLINTEGER;
    Value: SQLPOINTER;
    BufferLength: SQLINTEGER;
    StringLength: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLGetStmtOption = function(
    StatementHandle: SQLHSTMT;
    Option: SQLUSMALLINT;
    Value: SQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLGetTypeInfo = function(
    StatementHandle: SQLHSTMT;
    DataType: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLNumResultCols = function(
    StatementHandle: SQLHSTMT;
    ColumnCount: PSQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLParamData = function(
    StatementHandle: SQLHSTMT;
    Value: PSQLPOINTER
    ): SQLRETURN; stdcall;

  TSQLPrepare = function(
    StatementHandle: SQLHSTMT;
    StatementText: PSQLCHAR;
    TextLength: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLPutData = function(
    StatementHandle: SQLHSTMT;
    Data: SQLPOINTER;
    StrLen_or_Ind: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLRowCount = function(
    StatementHandle: SQLHSTMT;
    RowCount: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetConnectAttr = function(
    ConnectionHandle: SQLHDBC;
    Attribute: SQLINTEGER;
    Value: SQLPOINTER;
    StringLength: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetConnectOption = function(
    ConnectionHandle: SQLHDBC;
    Option: SQLUSMALLINT;
    Value: SQLUINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetCursorName = function(
    StatementHandle: SQLHSTMT;
    CursorName: PSQLCHAR;
    NameLength: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLSetDescField = function(
    DescriptorHandle: SQLHDESC;
    RecNumber: SQLSMALLINT;
    FieldIdentifier: SQLSMALLINT;
    Value: SQLPOINTER;
    BufferLength: SQLINTEGER
    ): SQLRETURN; stdcall;


  TSQLSetDescRec = function(
    DescriptorHandle: SQLHDESC;
    RecNumber: SQLSMALLINT;
    AType: SQLSMALLINT;
    SubType: SQLSMALLINT;
    Length: SQLINTEGER;
    Precision: SQLSMALLINT;
    Scale: SQLSMALLINT;
    Data: SQLPOINTER;
    StringLength: PSQLINTEGER;
    Indicator: PSQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetEnvAttr = function(
    EnvironmentHandle: SQLHENV;
    Attribute: SQLINTEGER;
    Value: SQLPOINTER;
    StringLength: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetParam = function(
    StatementHandle: SQLHSTMT;
    ParameterNumber: SQLUSMALLINT;
    ValueType: SQLSMALLINT;
    ParameterType: SQLSMALLINT;
    LengthPrecision: SQLUINTEGER;
    ParameterScale: SQLSMALLINT;
    ParameterValue: SQLPOINTER;
    StrLen_or_Ind: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetStmtAttr = function(
    StatementHandle: SQLHSTMT;
    Attribute: SQLINTEGER;
    Value: SQLPOINTER;
    StringLength: SQLINTEGER
    ): SQLRETURN; stdcall;

  TSQLSetStmtOption = function(
    StatementHandle: SQLHSTMT;
    Option: SQLUSMALLINT;
    Value: SQLUINTEGER
    ): SQLRETURN; stdcall;

  TSQLSpecialColumns = function(
    StatementHandle: SQLHSTMT;
    IdentifierType: SQLUSMALLINT;
    CatalogName: PSQLCHAR;
    NameLength1: SQLSMALLINT;
    SchemaName: PSQLCHAR;
    NameLength2: SQLSMALLINT;
    TableName: PSQLCHAR;
    NameLength3: SQLSMALLINT;
    Scope: SQLUSMALLINT;
    Nullable: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLStatistics = function(
    StatementHandle: SQLHSTMT;
    CatalogName: PSQLCHAR;
    NameLength1: SQLSMALLINT;
    SchemaName: PSQLCHAR;
    NameLength2: SQLSMALLINT;
    TableName: PSQLCHAR;
    NameLength3: SQLSMALLINT;
    Unique: SQLUSMALLINT;
    Reserved: SQLUSMALLINT
    ): SQLRETURN; stdcall;

  TSQLTables = function(
    StatementHandle: SQLHSTMT;
    CatalogName: PSQLCHAR;
    NameLength1: SQLSMALLINT;
    SchemaName: PSQLCHAR;
    NameLength2: SQLSMALLINT;
    TableName: PSQLCHAR;
    NameLength3: SQLSMALLINT;
    TableType: PSQLCHAR;
    NameLength4: SQLSMALLINT
    ): SQLRETURN; stdcall;

  TSQLTransact = function(
    EnvironmentHandle: SQLHENV;
    ConnectionHandle: SQLHDBC;
    CompletionType: SQLUSMALLINT
    ): SQLRETURN; stdcall;


implementation

function SQL_SUCCEEDED(RetCode: SQLRETURN): Boolean;
begin
  Result := (RetCode and (not 1)) = 0;
end;


end.
