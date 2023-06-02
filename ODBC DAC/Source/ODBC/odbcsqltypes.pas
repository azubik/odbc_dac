{*******************************************************}
{                                                       }
{       "sqltypes.h"                                    }
{                                                       }
{   Translated to Object Pascal by Andrey Zubik         }
{   e-mail: andrey.zubik@gmail.com                      }
{                                                       }
{*******************************************************}

unit odbcsqltypes;

interface
uses
  Windows;

type

  { API declaration data types }
  SQLCHAR = Char;
  SQLSCHAR = ShortInt;
  SQLDATE = Byte;
  SQLDECIMAL = Byte;
  SQLDOUBLE = Double;
  SQLFLOAT = Double;
  SQLINTEGER = Integer;
  SQLUINTEGER = Cardinal;
  SQLNUMERIC = Byte;
  SQLPOINTER = Pointer;
  SQLREAL = Single;
  SQLSMALLINT = Smallint;
  SQLUSMALLINT = Word;

  SQLTIME = Byte;
  SQLTIMESTAMP = Byte;
  SQLVARCHAR = Byte;

  { Function return type }
  SQLRETURN = SQLSMALLINT;

  { Generic data structures }
  SQLHANDLE = Pointer;

  SQLHENV = SQLHANDLE;
  SQLHDBC = SQLHANDLE;
  SQLHSTMT = SQLHANDLE;
  SQLHDESC = SQLHANDLE;

  { SQL portable types for C }
  UCHAR = Byte;
  SCHAR = ShortInt;
  SDWORD = LongInt;
  SWORD = SmallInt;
  UDWORD = Longword;
  UWORD = Word;

  SLONG = LongInt;
  SSHORT = SmallInt;
  ULONG = Cardinal;
  USHORT = Word;
  SDOUBLE = Double;
  LDOUBLE = Double;
  SFLOAT = Single;

  PTR = Pointer;
  HENV = Pointer;
  HDBC = Pointer;
  HSTMT = Pointer;

  RETCODE = Smallint;
  SQLHWND = HWND;

  { Transfer types for DATE, TIME, TIMESTAMP }
  tagDATE_STRUCT = record
    year: SQLSMALLINT;
    month: SQLUSMALLINT;
    day: SQLUSMALLINT;
  end;
  DATE_STRUCT = tagDATE_STRUCT;
  SQL_DATE_STRUCT = DATE_STRUCT;
  PSQL_DATE_STRUCT = ^SQL_DATE_STRUCT;

  tagTIME_STRUCT = record
    hour: SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
  end;
  TIME_STRUCT = tagTIME_STRUCT;
  SQL_TIME_STRUCT = TIME_STRUCT;
  PSQL_TIME_STRUCT = ^SQL_TIME_STRUCT;

  tagTIMESTAMP_STRUCT = record
    year: SQLSMALLINT;
    month: SQLUSMALLINT;
    day: SQLUSMALLINT;
    hour: SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
    fraction: SQLUINTEGER;
  end;
  TIMESTAMP_STRUCT = tagTIMESTAMP_STRUCT;
  SQL_TIMESTAMP_STRUCT = TIMESTAMP_STRUCT;
  PSQL_TIMESTAMP_STRUCT = ^SQL_TIMESTAMP_STRUCT;


const
  // Enumerations for DATETIME_INTERVAL_SUBCODE values for interval data types
  // these values are from SQL-92
  SQL_IS_YEAR = 1;
  SQL_IS_MONTH = 2;
  SQL_IS_DAY = 3;
  SQL_IS_HOUR = 4;
  SQL_IS_MINUTE = 5;
  SQL_IS_SECOND = 6;
  SQL_IS_YEAR_TO_MONTH = 7;
  SQL_IS_DAY_TO_HOUR = 8;
  SQL_IS_DAY_TO_MINUTE = 9;
  SQL_IS_DAY_TO_SECOND = 10;
  SQL_IS_HOUR_TO_MINUTE = 11;
  SQL_IS_HOUR_TO_SECOND = 12;
  SQL_IS_MINUTE_TO_SECOND = 13;

type
  SQLINTERVAL = Byte;

  tagSQL_YEAR_MONTH = record
    year: SQLUINTEGER;
    month: SQLUINTEGER;
  end;
  SQL_YEAR_MONTH_STRUCT = tagSQL_YEAR_MONTH;
  PSQL_YEAR_MONTH_STRUCT = ^SQL_YEAR_MONTH_STRUCT;

  tagSQL_DAY_SECOND = record
    day: SQLUINTEGER;
    hour: SQLUINTEGER;
    minute: SQLUINTEGER;
    second: SQLUINTEGER;
    fraction: SQLUINTEGER;
  end;
  SQL_DAY_SECOND_STRUCT = tagSQL_DAY_SECOND;
  PSQL_DAY_SECOND_STRUCT = ^SQL_DAY_SECOND_STRUCT;

  tagSQL_INTERVAL_STRUCT = record
    interval_type: SQLINTERVAL;
    interval_sign: SQLSMALLINT;
    case Integer of
      0: (year_month: SQL_YEAR_MONTH_STRUCT);
      1: (day_second: SQL_DAY_SECOND_STRUCT);
  end;
  SQL_INTERVAL_STRUCT = tagSQL_INTERVAL_STRUCT;
  PSQL_INTERVAL_STRUCT = ^SQL_INTERVAL_STRUCT;

  // Type INT64
  SQLBIGINT = Int64;
  SQLUBIGINT = Int64;


const
  // Internal representation of numeric data type
  SQL_MAX_NUMERIC_LEN = 16;

type
  tagSQL_NUMERIC_STRUCT = record
    precision: SQLCHAR;
    scale: SQLSCHAR;
    sign: SQLCHAR;                                // 1 if positive, 0 if negative
    val: array[0..SQL_MAX_NUMERIC_LEN - 1] of SQLCHAR;
  end;
  SQL_NUMERIC_STRUCT = tagSQL_NUMERIC_STRUCT;
  PSQL_NUMERIC_STRUCT = ^SQL_NUMERIC_STRUCT;

{$IFDEF ODBCVER3_50}
  SQLGUID = TGUID;
{$ESLE}
  tagSQLGUID = record
    Data1: DWORD;
    Data2: WORD;
    Data3: WORD;
    Data4: array[0..7] of Byte;
  end;
  SQLGUID = tagSQLGUID;
  PSQLGUID = ^SQLGUID;
{$ENDIF}

  BOOKMARK = Cardinal;

  SQLWCHAR = Word;
{$IFDEF UNICODE}
  SQLTCHAR = SQLWCHAR;
{$ELSE}
  SQLTCHAR = SQLCHAR;
{$ENDIF}




  // ----------------------------------------------
  // Pointer section (specially to use in Object Pascal)

  PSQLCHAR = ^SQLCHAR;
  PSQLSCHAR = ^SQLSCHAR;
  PSQLSMALLINT = ^SQLSMALLINT;
  PSQLUSMALLINT = ^SQLUSMALLINT;
  PSQLINTEGER = ^SQLINTEGER;
  PSQLUINTEGER = ^SQLUINTEGER;
  PSQLDOUBLE = ^SQLDOUBLE;
  PSQLFLOAT = ^SQLFLOAT;
  PSQLREAL = ^SQLREAL;

  PSQLPOINTER = ^SQLPOINTER;


  PSQLHANDLE = ^SQLHANDLE;

  PSQLHENV = ^SQLHENV;
  PSQLHDBC = ^SQLHDBC;
  PSQLHSTMT = ^SQLHSTMT;

  PSQLWCHAR = ^SQLWCHAR;

  PByte = ^Byte;


implementation

end.
