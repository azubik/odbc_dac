
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCUtils                                       }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCUtils;
{$I sv.inc}
interface
uses
  Classes,
  DB,
  FMTBcd,
  odbcsqltypes;

const
  STR_LEN = 128 + 1;

  SCHEMA_SEPARATOR = '.';
  SQL_DECIMAL_SEPARATOR = '.';

  { Utility functions }
function SQLDescribeParamSupported(ConnHandle: SQLHDBC): Boolean;

procedure GetSQLTypeValues(Proc: TGetStrProc);
function SQLTypeToIdent(SQLType: Integer; var Ident: string): Boolean;
function IdentToSQLType(const Ident: string; var SQLType: Integer): Boolean;

function ParamTypeToParamConst(AParamType: TParamType): SmallInt;
function ParamConstToParamType(AParamConst: SmallInt): TParamType;

function SQLTypeToCType(SQLType: SQLSMALLINT): SQLSMALLINT;
function SQLTypeToFieldType(SQLType: SQLSMALLINT): TFieldType;

function FieldTypeToCType(FieldType: TFieldType): SQLSMALLINT;
function FieldTypeToSQLType(FieldType: TFieldType): SQLSMALLINT;

function ISBLOBTYPE(SQLType: SmallInt): Boolean;
function ISCHARTYPE(SQLType: SmallInt): Boolean;

function GetConnPoolConst(const ConnPool: Byte): Cardinal;
function GetTableTypeConst(const ATableType: Byte): string;

// TXN Isolation level
function ILValueToILConst(const ILValue: Byte): LongInt;
function ILConstToILValue(const ILConst: LongInt): Byte;

// Connection string and parameters
function ParamsToConnStr(Strings: TStrings): string;
procedure ConnStrToParams(const ConnStr: string; AParams: TStrings);


function Max(A, B: Integer): Integer;
function Min(A, B: Integer): Integer;

function QuoteStr(const Value: string; Q: Char): string;

function SQL_DATE_STRUCT__To__DateTime(Value: SQL_DATE_STRUCT): TDateTime;
function SQL_TIME_STRUCT__To__DateTime(Value: SQL_TIME_STRUCT): TDateTime;
function SQL_TIMESTAMP_STRUCT__To__DateTime(Value: SQL_TIMESTAMP_STRUCT): TDateTime;

procedure DateTime__To__SQL_DATE_STRUCT(Source: TDateTime; Dest: PSQL_DATE_STRUCT);
procedure DateTime__To__SQL_TIME_STRUCT(Source: TDateTime; Dest: PSQL_TIME_STRUCT);
procedure DateTime__To__SQL_TIMESTAMP_STRUCT(Source: TDateTime; Dest: PSQL_TIMESTAMP_STRUCT);

function ClearBcdThousandSeparators(const ARegionalBcdStr: string): string;
function ReplaceRegionalDecimalSeparator(const ARegionalBcdStr: string): string;


implementation
uses
  SysUtils,
  Variants,

  odbcsql,
  odbcsqlext,
  odbcsqlucode,

  ODBCIntf,
  ODBCException,
  ODBCConsts;


const

  { SQLType mapping routines }

  SQLTypeMap: array[0..25] of TIdentMapEntry =
  (
    (Value: SQL_UNKNOWN_TYPE; Name: 'SQL_UNKNOWN_TYPE'),
    (Value: SQL_CHAR; Name: 'SQL_CHAR'),

    (Value: SQL_NUMERIC; Name: 'SQL_NUMERIC'),
    (Value: SQL_DECIMAL; Name: 'SQL_DECIMAL'),

    (Value: SQL_INTEGER; Name: 'SQL_INTEGER'),
    (Value: SQL_SMALLINT; Name: 'SQL_SMALLINT'),

    (Value: SQL_FLOAT; Name: 'SQL_FLOAT'),
    (Value: SQL_REAL; Name: 'SQL_REAL'),
    (Value: SQL_DOUBLE; Name: 'SQL_DOUBLE'),

    (Value: SQL_DATETIME; Name: 'SQL_DATETIME'),
    (Value: SQL_DATE; Name: 'SQL_DATE'),
    (Value: SQL_TIME; Name: 'SQL_TIME'),
    (Value: SQL_INTERVAL; Name: 'SQL_INTERVAL'),
    (Value: SQL_TIMESTAMP; Name: 'SQL_TIMESTAMP'),

    (Value: SQL_VARCHAR; Name: 'SQL_VARCHAR'),
    (Value: SQL_LONGVARCHAR; Name: 'SQL_LONGVARCHAR'),
    (Value: SQL_BINARY; Name: 'SQL_BINARY'),
    (Value: SQL_VARBINARY; Name: 'SQL_VARBINARY'),
    (Value: SQL_LONGVARBINARY; Name: 'SQL_LONGVARBINARY'),

    (Value: SQL_BIGINT; Name: 'SQL_BIGINT'),
    (Value: SQL_TINYINT; Name: 'SQL_TINYINT'),
    (Value: SQL_BIT; Name: 'SQL_BIT'),

    (Value: SQL_WCHAR; Name: 'SQL_WCHAR'),
    (Value: SQL_WVARCHAR; Name: 'SQL_WVARCHAR'),
    (Value: SQL_WLONGVARCHAR; Name: 'SQL_WLONGVARCHAR'),

    (Value: SQL_GUID; Name: 'SQL_GUID')
    );

  // Connection pool map
  CP_Map: array[0..3] of Cardinal =
  (
    SQL_CP_OFF,
    SQL_CP_DEFAULT,
    SQL_CP_ONE_PER_DRIVER,
    SQL_CP_ONE_PER_HENV
    );

  // Transaction isolation level map
  TXN_IL_Map: array[0..3] of LongInt =
  (
    SQL_TXN_READ_UNCOMMITTED,                     // ilReadUncommitted
    SQL_TXN_READ_COMMITTED,                       // ilReadCommitted
    SQL_TXN_REPEATABLE_READ,                      // ilRepeatableRead
    SQL_TXN_SERIALIZABLE                          // ilSerializable
    );

  // Table type map
  TableType_Map: array[0..7] of string =
  ('TABLE',
    'VIEW',
    'SYSTEM TABLE',
    'GLOBAL TEMPORARY',
    'LOCAL TEMPORARY',
    'ALIAS',
    'SYNONYM',
    'TABLE,VIEW,SYSTEM TABLE,GLOBAL TEMPORARY,LOCAL TEMPORARY,ALIAS,SYNONYM'
    );

function SQLDescribeParamSupported(ConnHandle: SQLHDBC): Boolean;
var
{$IFNDEF USE_UNICODE_DRIVER}
  Buffer: array[0..1] of SQLCHAR;
{$ELSE}
  Buffer: array[0..1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;

  RetCode: SQLRETURN;
begin
  if ConnHandle = nil then
    DatabaseError(SDatabaseClosed);

{$IFNDEF USE_UNICODE_DRIVER}
  RetCode := SQLGetInfoA(ConnHandle, SQL_DESCRIBE_PARAMETER, @Buffer, SizeOf(Buffer), @BufLen);
{$ELSE}
  RetCode := SQLGetInfoW(ConnHandle, SQL_DESCRIBE_PARAMETER, @Buffer, SizeOf(Buffer), @BufLen);
{$ENDIF}
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_DBC, ConnHandle, RetCode);

{$IFNDEF USE_UNICODE_DRIVER}
  Result := Buffer[0] = 'Y';                      //don't localize
{$ELSE}
  Result := WideChar(Buffer[0]) = 'Y';            //don't localize
{$ENDIF}

end;

procedure GetSQLTypeValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(SQLTypeMap) to High(SQLTypeMap) do
    Proc(SQLTypeMap[i].Name);
end;

function SQLTypeToIdent(SQLType: Integer; var Ident: string): Boolean;
begin
  Result := IntToIdent(SQLType, Ident, SQLTypeMap);
end;

function IdentToSQLType(const Ident: string; var SQLType: Integer): Boolean;
begin
  Result := IdentToInt(Ident, SQLType, SQLTypeMap);
end;

function SQLTypeToCType(SQLType: SQLSMALLINT): SQLSMALLINT;
begin
  case SQLType of
    SQL_CHAR, SQL_VARCHAR, SQL_LONGVARCHAR:
      Result := SQL_C_CHAR;

    SQL_WCHAR, SQL_WVARCHAR, SQL_WLONGVARCHAR:
      Result := SQL_C_WCHAR;

    SQL_DECIMAL, SQL_NUMERIC:
      Result := SQL_C_CHAR;

    SQL_BIT:
      Result := SQL_C_BIT;

    SQL_SMALLINT, SQL_TINYINT:
      Result := SQL_C_SSHORT;

    SQL_INTEGER:
      Result := SQL_C_SLONG;

    SQL_BIGINT:
      Result := SQL_C_SBIGINT;

    SQL_FLOAT, SQL_DOUBLE:
      Result := SQL_C_DOUBLE;

    SQL_REAL:
      Result := SQL_C_DOUBLE;                     // Result := SQL_C_FLOAT;

    SQL_BINARY, SQL_VARBINARY,
      SQL_LONGVARBINARY:
      Result := SQL_C_BINARY;

    SQL_TYPE_DATE:
      Result := SQL_C_TYPE_DATE;

    SQL_TYPE_TIME:
      Result := SQL_C_TYPE_TIME;

    SQL_TYPE_TIMESTAMP:
      Result := SQL_C_TYPE_TIMESTAMP;

    SQL_INTERVAL:
      Result := SQL_C_CHAR;

    SQL_INTERVAL_YEAR..SQL_INTERVAL_MINUTE_TO_SECOND:
      Result := SQL_C_CHAR;

    SQL_GUID:
      Result := SQL_C_GUID;
  else
    Result := SQL_UNKNOWN_TYPE;
  end;
end;

function ParamTypeToParamConst(AParamType: TParamType): SmallInt;
begin
  case AParamType of
    ptInput: Result := SQL_PARAM_INPUT;

    ptOutput: Result := SQL_PARAM_OUTPUT;

    ptInputOutput: Result := SQL_PARAM_INPUT_OUTPUT;

    ptResult: Result := SQL_RETURN_VALUE;
  else
    Result := SQL_PARAM_TYPE_UNKNOWN;
  end;
end;

function ParamConstToParamType(AParamConst: SmallInt): TParamType;
begin
  case AParamConst of
    SQL_PARAM_INPUT:
      Result := ptInput;

    SQL_PARAM_OUTPUT:
      Result := ptOutput;

    SQL_PARAM_INPUT_OUTPUT:
      Result := ptInputOutput;

    SQL_RETURN_VALUE:
      Result := ptResult;
  else
    Result := ptUnknown;
  end;
end;

function SQLTypeToFieldType(SQLType: SQLSMALLINT): TFieldType;
begin
  case SQLType of
    // chars
    SQL_CHAR:
      Result := ftFixedChar;

    SQL_VARCHAR:
      Result := ftString;

    SQL_WCHAR:
      Result := ftFixedWideChar;

    SQL_WVARCHAR:
      Result := ftWideString;

    // integers
    SQL_SMALLINT, SQL_TINYINT:
      Result := ftSmallint;

    SQL_INTEGER:
      Result := ftInteger;

    SQL_BIGINT:
      Result := ftLargeInt;

    // floats
    SQL_REAL, SQL_FLOAT, SQL_DOUBLE:
      Result := ftFloat;

    SQL_DECIMAL, SQL_NUMERIC:
      Result := ftFMTBCD;

    // boolean
    SQL_BIT:
      Result := ftBoolean;

    // date/time
    SQL_TYPE_DATE:
      Result := ftDate;

    SQL_TYPE_TIME:
      Result := ftTime;

    SQL_TYPE_TIMESTAMP:
      Result := ftDateTime;

    // blobs
    SQL_BINARY:
      Result := ftBytes;

    SQL_VARBINARY:
      Result := ftVarBytes;

    SQL_LONGVARBINARY:
      Result := ftBlob;

    SQL_LONGVARCHAR:
      Result := ftMemo;

    SQL_WLONGVARCHAR:
      Result := ftWideMemo;

    SQL_GUID:
      Result := ftGuid;

    SQL_INTERVAL:
      Result := ftString;

    SQL_INTERVAL_YEAR..SQL_INTERVAL_MINUTE_TO_SECOND:
      Result := ftString;
  else
    Result := ftUnknown;
  end;
end;

function FieldTypeToSQLType(FieldType: TFieldType): SQLSMALLINT;
begin
  case FieldType of
    ftAutoInc:
      Result := SQL_INTEGER;

    ftBCD:
      Result := SQL_DECIMAL;

    ftBlob, ftGraphic:
      Result := SQL_LONGVARBINARY;

    ftBoolean:
      Result := SQL_BIT;

    ftCurrency:
      Result := SQL_DECIMAL;

    ftDateTime:
      Result := SQL_TYPE_TIMESTAMP;

    ftDate:
      Result := SQL_TYPE_DATE;

    ftFixedChar:
      Result := SQL_CHAR;

    ftFixedWideChar:
      Result := SQL_WCHAR;

    ftFmtBCD:
      Result := SQL_DECIMAL;

    ftFloat:
      Result := SQL_DOUBLE;

    ftGuid:
      Result := SQL_GUID;

    ftInteger:
      Result := SQL_INTEGER;

    ftLargeInt:
      Result := SQL_BIGINT;

    ftMemo:
      Result := SQL_LONGVARCHAR;

    ftWideMemo:
      Result := SQL_WLONGVARCHAR;

    ftSmallInt:
      Result := SQL_SMALLINT;

    ftString:
      Result := SQL_VARCHAR;

    ftTime:
      Result := SQL_TYPE_TIME;

    ftBytes:
      Result := SQL_BINARY;

    ftVarBytes:
      Result := SQL_VARBINARY;

    ftWideString:
      Result := SQL_WVARCHAR;

    ftWord:
      Result := SQL_SMALLINT;

    ftTypedBinary:
      Result := SQL_LONGVARBINARY;

    ftParadoxOle:
      Result := SQL_LONGVARBINARY;

    ftDBaseOle:
      Result := SQL_LONGVARBINARY;

    ftCursor:
      Result := SQL_LONGVARBINARY;

    ftADT:
      Result := SQL_LONGVARBINARY;

    ftOraBlob:
      Result := SQL_LONGVARBINARY;

    ftOraClob:
      Result := SQL_LONGVARCHAR;
  else
    Result := SQL_UNKNOWN_TYPE;
  end;
end;

function FieldTypeToCType(FieldType: TFieldType): SQLSMALLINT;
begin
  case FieldType of
    ftString:
      Result := SQL_C_CHAR;
    ftSmallint:
      Result := SQL_C_SSHORT;
    ftInteger:
      Result := SQL_C_SLONG;
    ftWord:
      Result := SQL_C_USHORT;
    ftBoolean:
      Result := SQL_C_BIT;
    ftFloat:
      Result := SQL_C_DOUBLE;
    ftCurrency:
      Result := SQL_C_DOUBLE;
    ftBCD:
      Result := SQL_C_DOUBLE;                     // !SQL_C_CHAR;

    ftDate,                                       // Result := SQL_C_TYPE_DATE;
    ftTime,                                       // Result := SQL_C_TYPE_TIME;
    ftDateTime:
      Result := SQL_C_TYPE_TIMESTAMP;

    ftBytes:
      Result := SQL_C_BINARY;
    ftVarBytes:
      Result := SQL_C_BINARY;

    ftAutoInc:
      Result := SQL_C_ULONG;
    ftBlob:
      Result := SQL_C_BINARY;
    ftMemo:
      Result := SQL_C_CHAR;
    ftGraphic:
      Result := SQL_C_BINARY;
    ftWideMemo:
      Result := SQL_C_WCHAR;
    ftParadoxOle:
      Result := SQL_C_BINARY;
    ftDBaseOle:
      Result := SQL_C_BINARY;
    ftTypedBinary:
      Result := SQL_C_BINARY;
    ftCursor:
      Result := SQL_C_BINARY;
    ftFixedChar:
      Result := SQL_C_CHAR;
    ftFixedWideChar:
      Result := SQL_C_WCHAR;
    ftWideString:
      Result := SQL_C_WCHAR;
    ftLargeInt:
      Result := SQL_C_SBIGINT;
    ftADT:
      Result := SQL_C_BINARY;

    ftOraBlob:
      Result := SQL_C_BINARY;
    ftOraClob:
      Result := SQL_C_BINARY;

    ftGuid:
      Result := SQL_C_GUID;

    ftFmtBCD:
      Result := SQL_C_CHAR;
  else
    Result := SQL_C_CHAR;
  end;
end;

function ISBLOBTYPE(SQLType: SmallInt): Boolean;
begin
  // To workaround Informix problem with defining the size of BINARY, VARBINARY
  // we make a late binding for BINARY, VARBINARY data types (like a Blob)

  Result :=
    (SQLType = SQL_BINARY) or
    (SQLType = SQL_VARBINARY) or
    (SQLType = SQL_LONGVARBINARY) or
    (SQLType = SQL_LONGVARCHAR) or
    (SQLType = SQL_WLONGVARCHAR);
end;

function ISCHARTYPE(SQLType: SmallInt): Boolean;
begin
  Result :=
    (SQLType = SQL_CHAR) or
    (SQLType = SQL_VARCHAR) or
    (SQLType = SQL_WCHAR) or
    (SQLType = SQL_WVARCHAR);
end;

function GetConnPoolConst(const ConnPool: Byte): Cardinal;
begin
  Result := CP_Map[ConnPool];
end;

function GetTableTypeConst(const ATableType: Byte): string;
begin
  Result := TableType_Map[ATableType];
end;

function ILValueToILConst(const ILValue: Byte): LongInt;
begin
  Result := TXN_IL_Map[ILValue];
end;

function ILConstToILValue(const ILConst: LongInt): Byte;
begin
  Result := 100;                                  // error IL

  case ILConst of
    SQL_TXN_READ_UNCOMMITTED: Result := 0;
    SQL_TXN_READ_COMMITTED: Result := 1;
    SQL_TXN_REPEATABLE_READ: Result := 2;
    SQL_TXN_SERIALIZABLE: Result := 3;
  else
    DatabaseErrorFmt(SUnknownIL, [ILConst]);
  end;
end;

function ParamsToConnStr(Strings: TStrings): string;
var
  TmpStr: string;
  i: Integer;
begin
  Result := '';

  for i := 0 to Strings.Count - 1 do
  begin
    TmpStr := Trim(Strings[i]);
    if Result <> '' then
      Result := Result + ';';

    Result := Result + TmpStr;
  end;
end;

procedure ConnStrToParams(const ConnStr: string; AParams: TStrings);
begin
  AParams.Text := StringReplace(ConnStr, ';', CRLF, [rfReplaceAll]);
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function QuoteStr(const Value: string; Q: Char): string;
begin
  if (Q = '') or (Q = ' ') then
    Result := Value
  else
    Result := AnsiQuotedStr(Value, Q);
end;

function SQL_DATE_STRUCT__To__DateTime(Value: SQL_DATE_STRUCT): TDateTime;
begin
  with Value do
    Result := EncodeDate(year, month, day);
end;

function SQL_TIME_STRUCT__To__DateTime(Value: SQL_TIME_STRUCT): TDateTime;
begin
  with Value do
    Result := EncodeTime(hour, minute, second, 0);
end;

function SQL_TIMESTAMP_STRUCT__To__DateTime(Value: SQL_TIMESTAMP_STRUCT): TDateTime;
var
  D, T: TDateTime;
begin
  with Value do
  begin
    D := EncodeDate(year, month, day);
    T := EncodeTime(hour, minute, second, fraction div 1000000); // ODBC fraction 0..999999999
  end;

  if D < 0 then
    Result := D - T
  else
    Result := D + T;
end;

procedure DateTime__To__SQL_DATE_STRUCT(Source: TDateTime; Dest: PSQL_DATE_STRUCT);
begin
  with Dest^ do
    DecodeDate(Source, Word(year), month, day);
end;

procedure DateTime__To__SQL_TIME_STRUCT(Source: TDateTime; Dest: PSQL_TIME_STRUCT);
var
  MSec: Word;
begin
  with Dest^ do
    DecodeTime(Source, hour, minute, second, MSec);
end;

procedure DateTime__To__SQL_TIMESTAMP_STRUCT(Source: TDateTime; Dest: PSQL_TIMESTAMP_STRUCT);
var
  MSec: Word;
begin
  with Dest^ do
  begin
    DecodeDate(Source, Word(year), month, day);
    DecodeTime(Source, hour, minute, second, MSec);
    fraction := MSec * 1000000;                   // ODBC fraction 0..999999999
  end;
end;

function ClearBcdThousandSeparators(const ARegionalBcdStr: string): string;
begin
  Result := StringReplace(ARegionalBcdStr, {$IF RTLVersion >= 16.0}FormatSettings.{$IFEND}ThousandSeparator, '', [rfReplaceAll]);
end;

function ReplaceRegionalDecimalSeparator(const ARegionalBcdStr: string): string;
begin
  Result := StringReplace(ARegionalBcdStr, {$IF RTLVersion >= 16.0}FormatSettings.{$IFEND}DecimalSeparator, SQL_DECIMAL_SEPARATOR, [rfReplaceAll]);
end;


end.
