
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
{$IFDEF VER140_OR_ABOVE}
  FMTBcd,
{$ENDIF}
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

{$IFNDEF VER140_OR_ABOVE}
const
  NullBcd: TBcd = (Precision: 0; SignSpecialPlaces: 0; Fraction: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

  // Data conversion
function BcdToStr(const Bcd: TBcd): string;
function BcdToDouble(const Bcd: TBcd): Double;

function TryStrToBcd(const Value: string; var Bcd: TBcd): Boolean;
function StrToBcd(const Value: string): TBcd;
function DoubleToBcd(const Value: Double): TBcd;
function IntegerToBcd(const Value: Integer): TBcd;
{$ENDIF}

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
{$IFDEF VER140_OR_ABOVE}
  Variants,
{$ENDIF}
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
  Buffer: array[0..1] of SQLCHAR;
  BufLen: SQLSMALLINT;

  RetCode: SQLRETURN;
begin
  if ConnHandle = nil then
    DatabaseError(SDatabaseClosed);

  RetCode := SQLGetInfo(ConnHandle, SQL_DESCRIBE_PARAMETER, @Buffer, SizeOf(Buffer), @BufLen);
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_DBC, ConnHandle, RetCode);

  Result := Buffer[0] = 'Y';                      //don't localize
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
      Result := SQL_C_DOUBLE;                     //! Result := SQL_C_FLOAT;

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

    SQL_WCHAR, SQL_WVARCHAR:
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
{$IFDEF VER140_OR_ABOVE}
      Result := ftFMTBCD;
{$ELSE}
      Result := ftBCD;
{$ENDIF}

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
      Result := ftFmtMemo;

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

{$IFDEF VER140_OR_ABOVE}
    ftFmtBCD:
      Result := SQL_DECIMAL;
{$ENDIF}

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

    ftFmtMemo:
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
    ftFmtMemo:
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

{$IFDEF VER140_OR_ABOVE}
    ftFmtBCD:
      Result := SQL_C_CHAR;
{$ENDIF}

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

{$IFNDEF VER140_OR_ABOVE}

{ BCD routine for D5/BCB5 }
const
  MaxStringDigits = 100;
  _NoDecimal = -255;
  _DefaultDecimals = 10;

  { From DB.pas }
  { Max supported by Midas }
  MaxFMTBcdFractionSize = 64;
  { Max supported by Midas }
  MaxFMTBcdDigits = 32;
  DefaultFMTBcdScale = 6;
  MaxBcdPrecision = 18;
  MaxBcdScale = 4;


type
  { Exception classes }

  EBcdException = class(Exception);
  EBcdOverflowException = class(EBcdException);

procedure BcdError(const Message: string);
begin
  raise EBcdException.Create(Message);
end;

procedure BcdErrorFmt(const Message, BcdAsString: string);
begin
  BcdError(Format(Message, [BcdAsString]));
end;

procedure OverflowError(const Message: string);
begin
  raise EBcdOverflowException.Create(Message);
end;

function BcdToStr(const Bcd: TBcd): string;

  function GetBcdDigit(const Bcd: TBcd; Digit: Integer): Byte;
  begin
    if Digit mod 2 = 0 then
      Result := Byte((Bcd.Fraction[Digit div 2]) SHR 4)
    else
      Result := Byte(Byte((Bcd.Fraction[Digit div 2]) AND 15));
  end;

  function BcdScale(const Bcd: TBcd): Word;
  begin
    Result := (Bcd.SignSpecialPlaces and 63);
  end;

var
  I: Integer;
  DecPos: SmallInt;
  Negative: Boolean;
  C, Dot: Char;
begin
  if (Bcd.Precision = 0) or (Bcd.Precision > MaxFMTBcdFractionSize) or (BcdScale(Bcd) > Bcd.Precision) then
    OverFlowError(SBcdOverFlow)
  else
  begin
    Dot := DecimalSeparator;
    Negative := Bcd.SignSpecialPlaces and (1 shl 7) <> 0;
    DecPos := ShortInt(Bcd.Precision - (Bcd.SignSpecialPlaces and 63));
    Result := '';
    for I := 0 to Bcd.Precision - 1 do
    begin
      if I = DecPos then
      begin
        if (I = 0) or (Result = '') then
          Result := '0' + Dot
        else
          Result := Result + Dot;
      end;
      C := Char(GetBcdDigit(Bcd, I) + 48);
      { avoid leading 0's }
      if (Result <> '') or (C <> '0') or (I >= DecPos) then
        Result := Result + C;
    end;
    { if there is a decimal trim trailing '0's }
    if DecPos < Bcd.Precision then
    begin
      while Result[Length(Result)] = '0' do
        Result := Copy(Result, 1, Length(Result) - 1);
      if Result[Length(Result)] = Dot then
        Result := Copy(Result, 1, Length(Result) - 1);
    end;
    if Result = '' then
      Result := '0'
    else if Negative and (Result <> '0') then
      Result := '-' + Result;
  end;
end;

function BcdToDouble(const Bcd: TBcd): Double;
begin
  Result := StrToFloat(BcdToStr(Bcd));
end;

function InvalidBcdString(PValue: PChar): Boolean;
var
  Dot: Char;
  P: PChar;
begin
  Dot := DecimalSeparator;
  P := PValue;
  Result := False;
  while P^ <> #0 do
  begin
    if not (P^ in ['0'..'9', '-', Dot]) then
    begin
      Result := True;
      break;
    end;
    Inc(P);
  end;
end;

procedure StrToFraction(pTo: PChar; pFrom: PChar; count: SmallInt); pascal;
var
  Dot: Char;
begin
  Dot := DecimalSeparator;
  asm
   // From bytes to nibbles, both left aligned
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,pFrom  // move pFrom to ESI
        MOV     EDI,pTo    // move pTo to EDI
        XOR     ECX,ECX    // set ECX to 0
        MOV     CX,count   // store count in CX
        MOV     DL,0       // Flag: when to store
        CLD
@@1:    LODSB              // moves [ESI] into al
        CMP     AL,Dot
        JE      @@4
        SUB     AL,'0'
        CMP     DL,0
        JNE     @@2
        SHL     AL,4
        MOV     AH,AL
        JMP     @@3
@@2:    OR      AL,AH     // takes AH and ors in AL
        STOSB             // always moves AL into [EDI]
@@3:    NOT     dl        // flip all bits
@@4:    LOOP    @@1       // decrements cx and checks if it's 0
        CMP     DL,0      // are any bytes left unstored?
        JE      @@5
        MOV     AL,AH     // if so, move to al
        STOSB             // and store to [EDI]
@@5:    POP     EBX
        POP     EDI
        POP     ESI
  end;
end;

function TryStrToBcd(const Value: string; var Bcd: TBcd): Boolean;
const
  spaceChars: set of Char = [' ', #6, #10, #13, #14];
  digits: set of Char = ['0'..'9'];
var
  Neg: Boolean;
  NumDigits, DecimalPos: Word;
  pTmp, pSource: PChar;
  Dot: Char;
begin
  Dot := DecimalSeparator;
  if InvalidBcdString(PChar(Value)) then
  begin
    Result := False;
    exit;
  end;
  if (Value = '0') or (Value = '') then
  begin
    Result := True;
    Bcd.Precision := 8;
    Bcd.SignSpecialPlaces := 2;
    pSource := PChar(@Bcd.Fraction);
    FillChar(PSource^, SizeOf(Bcd.Fraction), 0);
    Exit
  end;
  Result := True;
  Neg := False;
  DecimalPos := Pos(Dot, Value);

  pSource := pCHAR(Value);
  { Strip leading whitespace }
  while (pSource^ in spaceChars) or (pSource^ = '0') do
  begin
    Inc(pSource);
    if DecimalPos > 0 then Dec(DecimalPos);
  end;

  { Strip trailing whitespace }
  pTmp := @pSource[StrLen(pSource) - 1];
  while pTmp^ in spaceChars do
  begin
    pTmp^ := #0;
    Dec(pTmp);
  end;

  { Is the number negative? }
  if pSource^ = '-' then
  begin
    Neg := TRUE;
    if DecimalPos > 0 then Dec(DecimalPos);
  end;
  if (pSource^ = '-') or (pSource^ = '+') then
    Inc(pSource);

  { Clear structure }
  pTmp := pCHAR(@Bcd.Fraction);
  FillChar(pTmp^, SizeOf(Bcd.Fraction), 0);
  if (pSource[0] = '0') then
  begin
    Inc(PSource);                                 // '0.' scenario
    if DecimalPos > 0 then Dec(DecimalPos);
  end;
  NumDigits := StrLen(pSource);
  if (NumDigits > MaxFMTBcdFractionSize) then
  begin
    if (DecimalPos > 0) and (DecimalPos <= MaxFMTBcdFractionSize) then
      NumDigits := MaxFMTBcdFractionSize          // truncate to 64
    else begin
      Bcd.Precision := NumDigits;
      Exit;
    end;
  end;

  if NumDigits > 0 then
    StrToFraction(pTmp, pSource, SmallInt(NumDigits))
  else begin
    Bcd.Precision := 10;
    Bcd.SignSpecialPlaces := 2;
  end;

  if DecimalPos > 0 then
  begin
    Bcd.Precision := Byte(NumDigits - 1);
    if Neg then
      Bcd.SignSpecialPlaces := (1 shl 7) + (BYTE(NumDigits - DecimalPos))
    else
      Bcd.SignSpecialPlaces := (0 shl 7) + (BYTE(NumDigits - DecimalPos));
  end else
  begin
    Bcd.Precision := Byte(NumDigits);
    if Neg then
      Bcd.SignSpecialPlaces := (1 shl 7)
    else
      Bcd.SignSpecialPlaces := (0 shl 7);
  end;
end;

function StrToBcd(const Value: string): TBcd;
var
  Success: Boolean;
begin
  Success := TryStrToBcd(Value, Result);
  if not Success then
    BcdErrorFmt(SInvalidBcdValue, Value);
end;

function DoubleToBcd(const Value: Double): TBcd;
begin
  Result := StrToBcd(FloatToStr(Value));
end;

function IntegerToBcd(const Value: Integer): TBcd;
begin
  Result := StrToBcd(IntToStr(Value));
end;
{$ENDIF}

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
  Result := StringReplace(ARegionalBcdStr, ThousandSeparator, '', [rfReplaceAll]);
end;

function ReplaceRegionalDecimalSeparator(const ARegionalBcdStr: string): string;
begin
  Result := StringReplace(ARegionalBcdStr, DecimalSeparator, SQL_DECIMAL_SEPARATOR, [rfReplaceAll]);
end;

end.
