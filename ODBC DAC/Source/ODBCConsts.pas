
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCConsts                                      }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCConsts;
{$I sv.inc}
interface

const
  CRLF = {$IFDEF WIN32}#13#10{$ENDIF}{$IFDEF LINUX}#10{$ENDIF};


resourcestring
  SODBC_DAC_Version = ' 2.0.5';

  SErrorLoadLib = 'Error loading %s';
  SODBCMissing = 'ODBC library %s not found in the path';
  SUnsupportedFeature = 'Unsupported feature';

  // Error handling
  SUnknownError = 'Unknown error';
  SUNKNOWN_RETCODE = 'Unknown return code: %d';
  SSQL_NO_DATA = 'No data found';
  SSQL_INVALID_HANDLE = 'Invalid handle';
  SSQL_STILL_EXECUTING = 'Still executing';
  SSQL_NEED_DATA = 'Need data';
  SSQLState = 'SQLSTATE : ';
  SNativeErrCode = 'Native error code: ';
  SNoODBCInfo = 'ODBC Return Code: %d' + CRLF + 'No ODBC diagnostic info available';

  // Parse SQL
  SInvalidPreprocessorDirective = 'Invalid preprocessor directive: ''%s''';
  SMissingOpeningBracket = 'Missing opening bracket';
  SMissingClosingBracket = 'Missing closing bracket';
  SMissingDirective = 'Missing directive: ''%s''';

  // Environment
  SEnvActive = 'Cannot perform this operation on an active environment';
  SEnvInactive = 'Cannot perform this operation on an inactive environment';
  SInactiveConns = 'Cannot perform this operation while at least one connection is active';
  SEnvironmentMissing = 'Environment property required for this operation';

  // Connection
  SLoginError = 'Operation cancelled at user''s request';
  SMissingConnParams = 'Not enough actual parameters to connect';
  SUnknownIL = 'Unknown isolation level const: %d';
  SUnsupportedAttr = 'Unsupported attribute';
  SSQLDescrPrmUnsupported = 'Driver %s does not support SQLDescribeParam function';

  SDatabaseOpen = 'Cannot perform this operation on an open connection';
  SDatabaseClosed = 'Cannot perform this operation on a closed connection';

  // SqlDA
  SInvalidDataConversion = 'Invalid data conversion';
  SSQLVarOutOfRange = 'SQLVar index out of range';
  SSQLDANameDoesNotExist = 'Cannot find SQLVar with name (%s)';
  SParameterNotFound = 'Parameter ''%s'' not found';
  SInvalidStmtHandle = 'Invalid statement handle';


  // Cursor
  SCursorOpen = 'Cannot perform this operation on an open cursor';
  SCursorClosed = 'Cannot perform this operation on a closed cursor';

  // Statement
  SNotSupported = 'Unsupported feature';
  SPreparedStmt = 'Cannot perform this operation on prepared statement';
  SUnpreparedStmt = 'Cannot perform this operation on unprepared statement';

  SMissingConnection = 'Connection property not assigned';
  SMissingConnectionHandle = 'ConnectionHandle property not assigned';

  SEmptyQuery = 'There are not statements to run';
  SNoUniqueKey = 'Insufficient key column information for updating or refreshing';
  SBookmarksNotSupported = 'Bookmarks are not supported';

  // For FMTBcd
  SBcdOverflow = 'BCD overflow';
  SInvalidBcdValue = '%s is not a valid BCD value';

  SInvalidVersion = 'Unable to load bind parameters';


  // CustomDataSet, Query
  S_COLUMN = 'COLUMN';

  SCircularDataLink = 'Circular datalinks are not allowed';
  SErrorMappingError = 'Error mapping failed';

  SNoRecords = 'No records';
  SNotEditing = 'Dataset not in edit or insert mode';

  SRequiredParamNotSet = 'Required Param value not set';
  SCannotInsert = 'Cannot insert into dataset. (No insert query)';
  SCannotPost = 'Cannot post. (No update/insert query)';
  SCannotUpdate = 'Cannot update. (No update query)';
  SCannotDelete = 'Cannot delete from dataset. (No delete query)';
  SCannotRefresh = 'Cannot refresh row. (No refresh query)';
  SCannotModifyDelRec = 'Cannot modify deleted record';
  SBookmarkNotFound = 'Bookmark not found';

  SFieldIsAutoInc = 'Field ''%s'' is auto-incremented';
  SFieldReadOnly = 'Field ''%s'' cannot be modified';
  SNoFieldAccess = 'Cannot access field ''%s'' in a filter';
  SNoBlobCache = 'Blob cache is turned off';
  SNoCachedUpdates = 'Not in cached update mode';
  SDataSetUnidirectional = 'Operation not allowed on a unidirectional dataset';
  SBufferNotSet = 'Buffer not set';
  SUserAbort = 'User abort';
  SUnSupportedDataType = 'Unsupported data type %s';

  { Table }
  SNoFieldIndexes = 'No index currently active';
  SNotIndexField = 'Field ''%s'' is not indexed and cannot be modified';
  SIndexFieldMissing = 'Index Field Missing';
  SFieldIndexError = 'Field index out of range';
  SNoTableName = 'No Table Name assigned';
  STableMismatch = 'Source and destination tables are incompatible';

  { UpdateSQL }
  SUpdateFailed = 'Update failed';
  SSQLDataSetOpen = 'Unable to determine field names for %s';
  SNoDataSet = 'No dataset association';
  SSQLGenSelect = 'Must select at least one key field and one update field';
  SSQLNotGenerated = 'Update SQL statements not generated, exit anyway?';
  SNoRecordsAffected = 'No Records Affected';


implementation

end.
