
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCStmt                                        }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCStmt;
{$I sv.inc}
interface
uses
  Classes,
  DB,
  SysUtils,
  Variants,
  FMTBcd,
  odbcsqltypes,
  odbcsql;

const
  DefaultMaxBlobChunkSize = 1024 * 32;

  SQL_NO_UNIQUE_KEY = 'SQL_NO_UNIQUE_KEY';

type
  TSQLType = type SmallInt;
  TCType = type SmallInt;


  { TODBCSQLVAR }

  TODBCSQLVAR = class(TCollectionItem)
  private
    FData: Pointer;
    FDataBufSize: Cardinal;                       // variable buffer size in Bytes

    FName: string;
    FModified: Boolean;
    FCType: TCType;
    FSQLType: TSQLType;
    FColumnSize: SQLUINTEGER;
    FDecimalDigits: SQLSMALLINT;
    FDataLength: SQLINTEGER;                      // variable data size in Bytes
    FStrLen_Or_Ind: SQLINTEGER;

    FIsAutoInc: Boolean;

    function GetAsBcd: TBcd;
    function GetAsBytes: TBytes;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsGuid: TGUID;
    function GetAsInteger: Integer;
    function GetAsLargeInt: LargeInt;
    function GetAsSmallInt: SmallInt;
    function GetAsString: string;
    function GetAsAnsiString: AnsiString;
    function GetAsVariant: Variant;
    function GetAsWord: Word;

    function GetIsBlob: Boolean;
    function GetIsNull: Boolean;

    procedure SetAsBcd(Value: TBcd);
    procedure SetAsBytes(const Value: TBytes);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsFloat(Value: Double);
    procedure SetAsGuid(Value: TGuid);
    procedure SetAsInteger(Value: Integer);
    procedure SetAsLargeInt(Value: LargeInt);
    procedure SetAsSingle(Value: Single);
    procedure SetAsSmallInt(Value: SmallInt);
    procedure SetAsString(const Value: string);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsWord(Value: Word);

    procedure SetAsAnsiString(const Value: AnsiString);
    procedure SetAsAnsiMemo(const Value: AnsiString);
    procedure SetAsWideMemo(const Value: string);

  protected
    property DataBufSize: Cardinal read FDataBufSize write FDataBufSize;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string; BlobType: TBlobType);
    procedure LoadFromStream(Stream: TStream; BlobType: TBlobType);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);

    procedure Clear;

    property AsBcd: TBcd read GetAsBcd write SetAsBcd;
    property AsBlob: TBytes read GetAsBytes write SetAsBytes;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsGuid: TGUID read GetAsGuid write SetAsGuid;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    property AsString: string read GetAsString write SetAsString;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsWord: Word read GetAsWord write SetAsWord;

    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsAnsiMemo: AnsiString read GetAsAnsiString write SetAsAnsiMemo;
    property AsWideMemo: string read GetAsString write SetAsWideMemo;

    property Value: Variant read GetAsVariant write SetAsVariant;
    property Data: Pointer read FData write FData;

    property IsAutoInc: Boolean read FIsAutoInc;
    property IsBlob: Boolean read GetIsBlob;
    property IsNull: Boolean read GetIsNull;

    property Name: string read FName write FName;
    property Modified: Boolean read FModified;

    property CType: TCType read FCType write FCType;
    property SQLType: TSQLType read FSQLType write FSQLType;
    property ColumnSize: SQLUINTEGER read FColumnSize write FColumnSize;
    property DecimalDigits: SQLSMALLINT read FDecimalDigits write FDecimalDigits;
    property StrLen_Or_Ind: SQLINTEGER read FStrLen_Or_Ind write FStrLen_Or_Ind;

    property DataLength: SQLINTEGER read FDataLength write FDataLength;
  end;


  { TODBCParam }

  TODBCParam = class(TODBCSQLVAR)
  private
    FBound: Boolean;
    FParamType: TParamType;

  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure AssignField(Field: TField);
    procedure AssignParam(Param: TParam);
    procedure AssignODBCParam(Param: TODBCParam);

    function GetDisplayName: string; override;
    function IsEqual(Param: TODBCParam): Boolean;

  public
    procedure Assign(Source: TPersistent); override;

    property Bound: Boolean read FBound write FBound;

  published
    property ParamType: TParamType read FParamType write FParamType;
    property Name;
    property ColumnSize;
    property DecimalDigits;
    property SQLType;

    property Value;
  end;


  { TODBCParams }

  TODBCParams = class(TCollection)
  private
    FOwner: TPersistent;
    FModified: Boolean;

    function GetItem(Index: Integer): TODBCParam;
    procedure SetItem(Index: Integer; const Value: TODBCParam);

  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

  public
    constructor Create(AOwner: TPersistent);

    procedure AssignValues(Value: TODBCParams);

    function IsEqual(Value: TODBCParams): Boolean;
    function FindParam(const Value: string): TODBCParam;
    function ParamByName(const Value: string): TODBCParam;
    function ParseSQL(SQL: string; DoCreate: Boolean): string;

    property Items[Index: Integer]: TODBCParam read GetItem write SetItem; default;
  end;


  { TODBCColumn }

  TODBCColumn = class(TODBCSQLVAR)
  private
    FNullable: Boolean;

  public
    property Nullable: Boolean read FNullable default False;

  end;


  { TODBCColumns }

  TODBCColumns = class(TCollection)
  private
    FOwner: TPersistent;

    function GetRecordSize: Integer;
    function GetItem(Index: Integer): TODBCColumn;
    procedure SetItem(Index: Integer; const Value: TODBCColumn);

  protected
    function GetOwner: TPersistent; override;

  public
    constructor Create(AOwner: TPersistent);

    function FindColumn(const Value: string): TODBCColumn;
    function ColumnByName(const Value: string): TODBCColumn;

    property RecordSize: Integer read GetRecordSize;
    property Items[Index: Integer]: TODBCColumn read GetItem write SetItem; default;
  end;


  { TODBCCursor }
  TODBCCursorType = (ctUnspecified, ctOpenForwardOnly, ctKeyset, ctDynamic, ctStatic);

  TODBCCursor = class(TComponent)
  private
    FHandle: SQLHSTMT;

    FColumns: TODBCColumns;
    FBLOBsList: TList;

    FCursorType: TODBCCursorType;

    FActive: Boolean;
    FBOF: Boolean;
    FEOF: Boolean;

    FColumnCount: SmallInt;
    FRecordCount: Integer;

    FGotoFirstRecord: Boolean;

    function GetBlobColumnCount: Integer;

    procedure CheckOpen;
    procedure CheckError(RetCode: SQLRETURN);

  protected
    procedure InitColumns;
    procedure DescribeColumns;
    procedure BindColumns;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(AHandle: SQLHSTMT);
    procedure Clear;

    procedure Open;
    procedure Next;
    procedure Close;

    property CursorType: TODBCCursorType read FCursorType write FCursorType;

    property Active: Boolean read FActive;
    property BOF: Boolean read FBOF;
    property EOF: Boolean read FEOF;

    property BlobColumnCount: Integer read GetBlobColumnCount;
    property RecordCount: Integer read FRecordCount;
    property GoToFirstRecord: Boolean read FGotoFirstRecord write FGotoFirstRecord default True;
    property Columns: TODBCColumns read FColumns;

  end;


  { TODBCCustomStatement }

  TODBCCustomStatement = class(TComponent)
  private
    FConnHandle: SQLHDBC;                         // Connection handle
    FStmtHandle: SQLHSTMT;                        // Statement handle

    FCursor: TODBCCursor;
    FParams: TODBCParams;

    FSQL: TStrings;
    FProcessedSQL: string;

    FParamCheck: Boolean;
    FPrepared: Boolean;
    //FDriverDescribeParams: Boolean;

    function GetRowsAffected: SQLINTEGER;
    procedure SetSQL(Value: TStrings);

  protected
    procedure CheckPrepared;
    procedure CheckClosed;
    procedure CheckError(RetCode: SQLRETURN);

    procedure SetPrepared(Value: Boolean);

    function GetConnectionHandle: SQLHDBC; virtual; abstract;

    procedure AllocateHandle;
    procedure FreeHandle;

    //procedure DescribeParams;
    procedure BindParams;

    procedure DoPrepare; virtual;
    procedure DoUnprepare; virtual;

    procedure QueryChanged(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER);
    procedure SetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; StringLength: SQLINTEGER);

    procedure Prepare;
    procedure UnPrepare;

    procedure Execute;
    procedure Close;

    function IsCursorOpen: Boolean;

    property Handle: SQLHSTMT read FStmtHandle;
    property Prepared: Boolean read FPrepared write SetPrepared;
    property RowsAffected: SQLINTEGER read GetRowsAffected;

    property Cursor: TODBCCursor read FCursor;

    property ParamCheck: Boolean read FParamCheck write FParamCheck;
    property Params: TODBCParams read FParams;
    property SQL: TStrings read FSQL write SetSQL;

  end;
  TODBCCustomStatementClass = class of TODBCCustomStatement;


implementation
uses
  odbcsqlext,
  odbcsqlucode,

{$IFDEF USE_DRV_SPEC}
  ODBCDrvSpec,
{$ENDIF}
  ODBCIntf,
  ODBCException,
  ODBCUtils,
  ODBCConsts;


const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';


  { TODBCSQLVAR }

constructor TODBCSQLVAR.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDataBufSize := 0;

  FSQLType := SQL_UNKNOWN_TYPE;
  FColumnSize := 0;
  FDecimalDigits := 0;

  FCType := 0;
  FDataLength := 0;
  FStrLen_Or_Ind := SQL_NULL_DATA;
end;

destructor TODBCSQLVAR.Destroy;
begin
  FreeMem(FData);
  FData := nil;

  inherited;
end;

function TODBCSQLVAR.GetAsBcd: TBcd;
begin
  Result := NullBcd;
  if not IsNull then
    case FCType of
      SQL_C_CHAR, SQL_C_WCHAR:
        Result := StrToBcd(ClearBcdThousandSeparators(AsString));

      SQL_C_DOUBLE:
        Result := DoubleToBcd(PSQLDOUBLE(FData)^);

      SQL_C_LONG:
        Result := IntegerToBcd(PSQLINTEGER(FData)^);
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsBytes: TBytes;
begin
  SetLength(Result, 0);
  if not IsNull then
    case FCType of
      SQL_C_BINARY, SQL_C_CHAR, SQL_C_WCHAR:
      begin
        if FData <> nil then
        begin
          SetLength(Result, FDataLength);
          Move(PByte(FData)^, PByte(Result)^, FDataLength);
        end;
      end
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsBoolean: Boolean;
begin
  Result := False;
  if not IsNull then
    case FCType of
      SQL_C_BIT:
        Result := Boolean(PByte(FData)^);
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsDateTime: TDateTime;
begin
  Result := 0;
  if not IsNull then
    case FCType of
      SQL_C_CHAR, SQL_C_WCHAR:
        try
          Result := StrToDateTime(AsString);
        except
          on E: EConvertError do
            DatabaseError(SInvalidDataConversion);
        end;

      SQL_C_TYPE_DATE, SQL_C_DATE:
        Result := SQL_DATE_STRUCT__To__DateTime(PSQL_DATE_STRUCT(FData)^);

      SQL_C_TYPE_TIME, SQL_C_TIME:
        Result := SQL_TIME_STRUCT__To__DateTime(PSQL_TIME_STRUCT(FData)^);

      SQL_C_TYPE_TIMESTAMP, SQL_C_TIMESTAMP:
        Result := SQL_TIMESTAMP_STRUCT__To__DateTime(PSQL_TIMESTAMP_STRUCT(FData)^);

    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsCurrency: Currency;
begin
  Result := 0;
  if not IsNull then
    case FCType of
      SQL_C_CHAR, SQL_C_WCHAR:
        try
          Result := StrToCurr(AsString);
        except
          DatabaseError(SInvalidDataConversion);
        end;

      SQL_C_DOUBLE:
        Result := AsFloat;
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsFloat: Double;
begin
  Result := 0;
  if not IsNull then
    case FCType of
      SQL_C_DOUBLE:
        Result := PSQLDOUBLE(FData)^;

      SQL_C_CHAR, SQL_C_WCHAR:
        try
          Result := StrToFloat(AsString);
        except
          DatabaseError(SInvalidDataConversion);
        end;
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsGuid: TGUID;
begin
  Result := GUID_NULL;
  if not IsNull then
    case FCType of
      SQL_C_CHAR, SQL_C_WCHAR:
        try
          Result := StringToGUID(AsString);
        except
          DatabaseError(SInvalidDataConversion);
        end;

      SQL_C_GUID:
        Result := PGuid(FData)^;
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsInteger: Integer;
begin
  Result := 0;
  if not IsNull then
    case FCType of
      SQL_C_LONG, SQL_C_SLONG:
        Result := PSQLINTEGER(FData)^;

      SQL_C_ULONG:
        Result := PSQLUINTEGER(FData)^;

      SQL_C_SHORT, SQL_C_SSHORT:
        Result := AsSmallInt;

      SQL_C_USHORT:
        Result := PSQLUSMALLINT(FData)^;

      SQL_C_BIT:
        Result := PByte(FData)^;

      SQL_C_STINYINT:
        Result := PSQLSMALLINT(FData)^;

      SQL_C_UTINYINT:
        Result := PByte(FData)^;

      SQL_C_CHAR, SQL_C_WCHAR:
        try
          Result := StrToInt(AsString);
        except
          DatabaseError(SInvalidDataConversion);
        end;
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsLargeInt: LargeInt;
begin
  Result := 0;
  if not IsNull then
    case FCType of
      SQL_C_SBIGINT, SQL_C_UBIGINT:
        Result := PInt64(FData)^;

      SQL_C_CHAR, SQL_C_WCHAR:
        try
          Result := StrToInt64(AsString);
        except
          DatabaseError(SInvalidDataConversion);
        end;
    else
      Result := GetAsInteger
    end;
end;

function TODBCSQLVAR.GetAsSmallInt: SmallInt;
begin
  Result := 0;
  if not IsNull then
    case FCType of
      SQL_C_SHORT, SQL_C_SSHORT:
        Result := PSQLSMALLINT(FData)^;

      SQL_C_CHAR, SQL_C_WCHAR:
        try
          Result := StrToInt(AsString);
        except
          DatabaseError(SInvalidDataConversion);
        end;
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsString: string;
begin
  Result := '';
  if not IsNull then
    case FCType of
      SQL_C_CHAR:
        Result := UnicodeString(AnsiString(PAnsiChar(FData)));

      SQL_C_WCHAR:
        Result := PWideChar(FData);

      SQL_C_SHORT, SQL_C_SSHORT, SQL_C_LONG, SQL_C_SLONG:
        Result := IntToStr(GetAsInteger);

      SQL_C_DOUBLE {, SQL_C_FLOAT}:
        Result := FloatToStr(GetAsFloat);

      SQL_C_TYPE_DATE, SQL_C_DATE, SQL_C_TYPE_TIME, SQL_C_TIME,
        SQL_C_TYPE_TIMESTAMP, SQL_C_TIMESTAMP:
        Result := DateTimeToStr(GetAsDateTime);

      SQL_C_BINARY:
        if FData <> nil then
        begin
          SetLength(Result, FDataLength);
          Move(PByte(FData)^, PByte(Result)^, FDataLength);
        end;

      SQL_C_GUID:
        Result := GuidToString(PGuid(FData)^);
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsAnsiString: AnsiString;
begin
  Result := '';
  if not IsNull then
    case FCType of
      SQL_C_CHAR:
        Result := PAnsiChar(FData);

      SQL_C_WCHAR:
        Result := AnsiString(PWideChar(FData));

      SQL_C_SHORT, SQL_C_SSHORT, SQL_C_LONG, SQL_C_SLONG:
        Result := AnsiString(IntToStr(GetAsInteger));

      SQL_C_DOUBLE {, SQL_C_FLOAT}:
        Result := AnsiString(FloatToStr(GetAsFloat));

      SQL_C_TYPE_DATE, SQL_C_DATE, SQL_C_TYPE_TIME, SQL_C_TIME,
        SQL_C_TYPE_TIMESTAMP, SQL_C_TIMESTAMP:
        Result := AnsiString(DateTimeToStr(GetAsDateTime));

      SQL_C_BINARY:
        if FData <> nil then
        begin
          SetLength(Result, FDataLength);
          Move(PByte(FData)^, PByte(Result)^, FDataLength);
        end;

      SQL_C_GUID:
        Result := AnsiString(GuidToString(PGuid(FData)^));
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsVariant: Variant;
begin
  if IsNull then
    Result := NULL
  else
    case FCType of
      SQL_C_CHAR:
        Result := AsAnsiString;

      SQL_C_WCHAR:
        Result := AsString;

      SQL_C_LONG, SQL_C_SLONG, SQL_C_ULONG:
        Result := AsInteger;

      SQL_C_SHORT, SQL_C_SSHORT:
        Result := AsSmallInt;

      SQL_C_USHORT:
        Result := AsWord;

      SQL_C_DOUBLE {, SQL_C_FLOAT}:
        Result := AsFloat;

      SQL_C_BIT:
        Result := AsBoolean;

      SQL_C_STINYINT, SQL_C_UTINYINT:
        Result := AsSmallInt;

      SQL_C_TYPE_DATE, SQL_C_TYPE_TIME, SQL_C_TYPE_TIMESTAMP:
        Result := AsDateTime;

      SQL_C_BINARY:
        Result := AsBlob;

      SQL_C_SBIGINT, SQL_C_UBIGINT:
        Result := GetAsLargeInt;
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetAsWord: Word;
begin
  Result := 0;
  if not IsNull then
    case FCType of
      SQL_C_CHAR:
        try
          Result := StrToInt(AsString);
        except
          DatabaseError(SInvalidDataConversion);
        end;

      SQL_C_USHORT:
        Result := PSQLUSMALLINT(FData)^;
    else
      DatabaseError(SInvalidDataConversion);
    end;
end;

function TODBCSQLVAR.GetIsBlob: Boolean;
begin
  Result := ISBLOBTYPE(FSQLType);
end;

function TODBCSQLVAR.GetIsNull: Boolean;
begin
  Result := FStrLen_Or_Ind = SQL_NULL_DATA; ;
end;

procedure TODBCSQLVAR.SetAsBcd(Value: TBcd);
var
  RegionalBcdStr: string;
  BcdStr: AnsiString;
begin
  Clear;
  RegionalBcdStr := BcdToStr(Value);
  BcdStr := AnsiString(ReplaceRegionalDecimalSeparator(RegionalBcdStr));

  FDataLength := Length(BcdStr);
  FDataBufSize := FDataLength + 1;                // null terminator
  GetMem(FData, FDataBufSize);
  Move(PByte(BcdStr)^, FData^, FDataBufSize);

  FCType := SQL_C_CHAR;
  FStrLen_Or_Ind := SQL_NTS;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsBytes(const Value: TBytes);
begin
  Clear;

  FDataLength := Length(Value);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(PByte(Value)^, FData^, FDataBufSize);

  FCType := SQL_C_BINARY;
  FStrLen_Or_Ind := SQL_DATA_AT_EXEC;             //SQL_LEN_DATA_AT_EXEC(FDataLength);

  FModified := True;
end;

procedure TODBCSQLVAR.SetAsBoolean(Value: Boolean);
begin
  Clear;

  FDataLength := SizeOf(Boolean);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_BIT;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsCurrency(Value: Currency);
begin
  SetAsFloat(Value);
end;

procedure TODBCSQLVAR.SetAsDateTime(Value: TDateTime);
begin
  Clear;

  FDataLength := SizeOf(SQL_TIMESTAMP_STRUCT);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  DateTime__To__SQL_TIMESTAMP_STRUCT(Value, FData);

  FCType := SQL_C_TYPE_TIMESTAMP;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsDate(Value: TDateTime);
begin
  Clear;

  FDataLength := SizeOf(SQL_DATE_STRUCT);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  DateTime__To__SQL_DATE_STRUCT(Value, FData);

  FCType := SQL_C_TYPE_DATE;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsFloat(Value: Double);
begin
  Clear;

  FDataLength := SizeOf(Double);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_DOUBLE;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsGuid(Value: TGuid);
begin
  Clear;

  FDataLength := SizeOf(TGUID);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_GUID;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsInteger(Value: Integer);
begin
  Clear;

  FDataLength := SizeOf(Integer);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_SLONG;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsLargeInt(Value: LargeInt);
begin
  Clear;

  FDataLength := SizeOf(Int64);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_SBIGINT;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsAnsiMemo(const Value: AnsiString);
begin
  Clear;

  FDataLength := Length(Value);
  FDataBufSize := FDataLength + 1;                // FDataLength + 1 is null terminator
  GetMem(FData, FDataBufSize);
  Move(PAnsiChar(Value)^, FData^, FDataBufSize);

  FCType := SQL_C_CHAR;
  FStrLen_Or_Ind := SQL_DATA_AT_EXEC;             //SQL_LEN_DATA_AT_EXEC(FDataLength);

  FModified := True;
end;

procedure TODBCSQLVAR.SetAsSingle(Value: Single);
begin
  Clear;

  FDataLength := SizeOf(Double);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_DOUBLE;                         //SQL_C_FLOAT;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsSmallInt(Value: SmallInt);
begin
  Clear;

  FDataLength := SizeOf(SmallInt);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_SSHORT;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsAnsiString(const Value: AnsiString);
begin
  Clear;

  FDataLength := Length(Value);
  FDataBufSize := {(}FDataLength + 1{) * SizeOf(SQLCHAR)};                // FDataLength + 1 is null terminator
  GetMem(FData, FDataBufSize);
  Move(PAnsiChar(Value)^, FData^, FDataBufSize);

  FCType := SQL_C_CHAR;
  FStrLen_Or_Ind := SQL_NTS;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsString(const Value: string);
begin
  Clear;

  FDataLength := Length(Value) * SizeOf(WideChar);
  FDataBufSize := (FDataLength + 1) * SizeOf(WideChar); // FDataLength +1 is null terminator
  GetMem(FData, FDataBufSize);
  Move(PByte(Value)^, FData^, FDataBufSize);

  FCType := SQL_C_WCHAR;
  FStrLen_Or_Ind := SQL_NTS;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsTime(Value: TDateTime);
begin
  Clear;

  FDataLength := SizeOf(SQL_TIME_STRUCT);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  DateTime__To__SQL_TIME_STRUCT(Value, FData);

  FCType := SQL_C_TYPE_TIME;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsVariant(const Value: Variant);
begin
  if VarIsNull(Value) then
    Clear
  else
    case VarType(Value) of
      varEmpty, varNull:
        Clear;
      varSmallint, varByte:
        SetAsSmallInt(Value);
      varBoolean:
        SetAsBoolean(Value);
      varInteger:
        SetAsInteger(Value);
      varSingle:
        SetAsSingle(Value);
      varDouble:
        SetAsFloat(Value);
      varCurrency:
        SetAsCurrency(Value);
      varDate:
        SetAsDateTime(Value);
      varString:
        SetAsAnsiString(AnsiString(Value));
      varOleStr, varUString:
        SetAsString(Value);
      varInt64:
        SetAsLargeInt(Value);
    else
      if VarIsFmtBcd(Value) then
        SetAsBcd(VarToBcd(Value))
      else
        if VarIsArray(Value) then
          SetAsBytes(Value)
        else
          DatabaseError(SInvalidDataConversion);
    end;
end;

procedure TODBCSQLVAR.SetAsWideMemo(const Value: string);
begin
  Clear;

  FDataLength := Length(Value) * SizeOf(WideChar);
  FDataBufSize := (FDataLength + 1) * SizeOf(WideChar); // null terminator
  GetMem(FData, FDataBufSize);
  Move(PByte(Value)^, FData^, FDataBufSize);

  FCType := SQL_C_WCHAR;
  FStrLen_Or_Ind := SQL_DATA_AT_EXEC;             //SQL_LEN_DATA_AT_EXEC(FDataLength);
  FModified := True;
end;

procedure TODBCSQLVAR.SetAsWord(Value: Word);
begin
  Clear;

  FDataLength := SizeOf(Word);
  FDataBufSize := FDataLength;
  GetMem(FData, FDataBufSize);
  Move(Value, FData^, FDataBufSize);

  FCType := SQL_C_USHORT;
  FStrLen_Or_Ind := 0;
  FModified := True;
end;

procedure TODBCSQLVAR.LoadFromFile(const FileName: string; BlobType: TBlobType);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, BlobType);
  finally
    Stream.Free;
  end;
end;

procedure TODBCSQLVAR.LoadFromStream(Stream: TStream; BlobType: TBlobType);
var
  DataAnsiStr: AnsiString;
  DataUnicodeStr: UnicodeString;
  Len: Integer;
begin
  with Stream do
  begin
    Position := 0;
    Len := Size;
  end;

  case BlobType of
    ftMemo:
    begin
      SetLength(DataAnsiStr, Len);
      Stream.ReadBuffer(Pointer(DataAnsiStr)^, Len);

      SetAsAnsiMemo(DataAnsiStr);
    end;

    ftWideMemo:
    begin
      SetLength(DataUnicodeStr, Len div 2);
      Stream.ReadBuffer(Pointer(DataUnicodeStr)^, Len);

      SetAsWideMemo(DataUnicodeStr);
    end;

    ftBlob, ftGraphic, ftFmtMemo:
    begin
      SetLength(DataAnsiStr, Len);
      Stream.ReadBuffer(Pointer(DataAnsiStr)^, Len);

      SetAsBytes(BytesOf(DataAnsiStr));
    end;
  else
    DatabaseError(SNotSupported);
  end;
end;

procedure TODBCSQLVAR.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TODBCSQLVAR.SaveToStream(Stream: TStream);
begin
  with TMemoryStream.Create do
  try
    WriteBuffer(FData^, FDataBufSize);
    SaveToStream(Stream);
  finally
    Free;
  end;
end;

procedure TODBCSQLVAR.Clear;
begin
  FCType := SQLTypeToCType(FSQLType);

  FreeMem(FData);
  FData := nil;

  FStrLen_Or_Ind := SQL_NULL_DATA;
end;


{ TODBCParam }

procedure TODBCParam.AssignTo(Dest: TPersistent);

  procedure AssignToParam(Param: TParam);
  begin
    Param.Name := Name;
    Param.ParamType := ParamType;
    Param.DataType := SQLTypeToFieldType(SQLType);
    Param.Value := Value;
  end;

begin
  if Dest is TField then
    TField(Dest).Value := Value
  else
    if Dest is TParam then
      AssignToParam(TParam(Dest))
    else
      inherited AssignTo(Dest);
end;

function TODBCParam.GetDisplayName: string;
begin
  if Name = '' then
    Result := inherited GetDisplayName
  else
    Result := Name;
end;

function TODBCParam.IsEqual(Param: TODBCParam): Boolean;
begin
  Result :=
    (CType = Param.CType) and
    (Value = Param.Value) and
    (Name = Param.Name) and
    (SQLType = Param.SQLType) and
    (ParamType = Param.ParamType) and
    (Value = Param.Value) and
    (IsNull = Value.IsNull) and
    (Bound = Value.Bound);
end;


procedure TODBCParam.Assign(Source: TPersistent);
begin
  if Source is TField then
    AssignField(TField(Source))
  else
    if Source is TParam then
      AssignParam(TParam(Source))
    else
      if Source is TODBCParam then
        AssignODBCParam(TODBCParam(Source))
      else
        inherited Assign(Source);
end;

procedure TODBCParam.AssignField(Field: TField);
var
  Old: Boolean;
begin
  if Field = nil then Exit;

  {+}/////////////////////////////////////////////////////////////////////////////
  Old := CompareText(Copy(Name, 1, 4), 'OLD_') = 0; {do not localize}
  if Old then
  begin
    if VarIsNull(Field.OldValue) then
      Clear
    else
      Value := Field.OldValue
  end
  else
  begin
    if Field.IsNull then
      Clear
    else
      Value := Field.Value;
  end;
  {-}/////////////////////////////////////////////////////////////////////////////

  CType := FieldTypeToCType(Field.DataType);
  SQLType := FieldTypeToSQLType(Field.DataType);

  case SQLType of
    SQL_CHAR, SQL_VARCHAR, SQL_WCHAR, SQL_WVARCHAR:
      ColumnSize := Field.Size;

    SQL_DECIMAL, SQL_NUMERIC:
      begin
        ColumnSize := TFmtBCDField(Field).Precision;
        DecimalDigits := Field.Size;
      end;

    SQL_TYPE_TIME,
      SQL_TYPE_TIMESTAMP,
      SQL_INTERVAL_SECOND,
      SQL_INTERVAL_DAY_TO_SECOND,
      SQL_INTERVAL_HOUR_TO_SECOND,
      SQL_INTERVAL_MINUTE_TO_SECOND:
      begin
        ColumnSize := 26;
        DecimalDigits := 6;
      end;

    SQL_BINARY, SQL_VARBINARY, SQL_LONGVARBINARY, SQL_LONGVARCHAR, SQL_WLONGVARCHAR:
      begin
        ColumnSize := 0;
        DecimalDigits := 0;

        if not Field.IsNull then
          StrLen_Or_Ind := SQL_DATA_AT_EXEC;      //SQL_LEN_DATA_AT_EXEC(FDataLength);
      end;
  else
    ColumnSize := 0;
    DecimalDigits := 0;
  end;
end;

procedure TODBCParam.AssignParam(Param: TParam);
begin
  if Param = nil then Exit;

  FName := Param.Name;
  FParamType := Param.ParamType;

  if Param.IsNull then
    Clear
  else
    Value := Param.Value;

  CType := FieldTypeToCType(Param.DataType);
  SQLType := FieldTypeToSQLType(Param.DataType);

  case SQLType of
    SQL_CHAR, SQL_VARCHAR, SQL_WCHAR, SQL_WVARCHAR:
      ColumnSize := Param.Size;

    SQL_DECIMAL, SQL_NUMERIC:
      begin
        ColumnSize := Param.Precision;
        DecimalDigits := Param.Size;
      end;

    SQL_TYPE_TIME,
      SQL_TYPE_TIMESTAMP,
      SQL_INTERVAL_SECOND,
      SQL_INTERVAL_DAY_TO_SECOND,
      SQL_INTERVAL_HOUR_TO_SECOND,
      SQL_INTERVAL_MINUTE_TO_SECOND:
      begin
        ColumnSize := 26;
        DecimalDigits := 6;
      end;

    SQL_BINARY, SQL_VARBINARY, SQL_LONGVARBINARY, SQL_LONGVARCHAR, SQL_WLONGVARCHAR:
      begin
        ColumnSize := 0;
        DecimalDigits := 0;

        if not Param.IsNull then
          StrLen_Or_Ind := SQL_DATA_AT_EXEC;      //SQL_LEN_DATA_AT_EXEC(FDataLength);
      end;
  else
    ColumnSize := 0;
    DecimalDigits := 0;
  end;
end;

procedure TODBCParam.AssignODBCParam(Param: TODBCParam);
begin
  if Param = nil then Exit;

  FName := Param.Name;
  FParamType := Param.ParamType;

  if Param.IsNull then
    Clear
  else
    Value := Param.Value;

  FCType := Param.CType;
  FSQLType := Param.SQLType;
  FColumnSize := Param.ColumnSize;
  FDecimalDigits := Param.DecimalDigits;
  FStrLen_Or_Ind := Param.StrLen_Or_Ind;

  FIsAutoInc := Param.IsAutoInc;
end;


{ TODBCParams }

constructor TODBCParams.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create(TODBCParam);
end;

function TODBCParams.GetItem(Index: Integer): TODBCParam;
begin
  Result := TODBCParam(inherited Items[Index]);
end;

procedure TODBCParams.SetItem(Index: Integer; const Value: TODBCParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

function TODBCParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TODBCParams.Update(Item: TCollectionItem);
begin
  FModified := True;
end;

procedure TODBCParams.AssignValues(Value: TODBCParams);
var
  Param: TODBCParam;
  i: Integer;
begin
  for i := 0 to Value.Count - 1 do
  begin
    Param := FindParam(Value[i].Name);
    if Param <> nil then
      Param.Assign(Value[i]);
  end;
end;

function TODBCParams.IsEqual(Value: TODBCParams): Boolean;
var
  i: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for i := 0 to Count - 1 do
    begin
      Result := Items[i].IsEqual(Value.Items[i]);
      if not Result then Break;
    end
end;

function TODBCParams.FindParam(const Value: string): TODBCParam;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := TODBCParam(inherited Items[i]);
    if CompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TODBCParams.ParamByName(const Value: string): TODBCParam;
begin
  Result := FindParam(Value);
  if Result = nil then
    DatabaseErrorFmt(SParameterNotFound, [Value]);
end;

function TODBCParams.ParseSQL(SQL: string; DoCreate: Boolean): string;
const
  Literals = ['''', '"', '`'];
var
  Value, CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function NameDelimiter: Boolean;
  begin
    Result := CharInSet(CurChar, [' ', ',', ';', ')', #13, #10]);
  end;

  function IsLiteral: Boolean;
  begin
    Result := CharInSet(CurChar, Literals);
  end;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar;
    begin
      if CharInSet(TempBuf^, Literals) then
      begin
        StrMove(TempBuf, TempBuf + 1, Len - 1);
        if CharInSet((TempBuf + (Len-2))^, Literals) then
          (TempBuf + Len-2)^ := #0;
      end;
    end;

  begin
    Len := StrLen(Buffer);
    TempBuf := AllocMem((Len + 1) * SizeOf(Char));
    try
      StrCopy(TempBuf, Buffer);
      StripChar;
      Result := TempBuf;
    finally
      FreeMem(TempBuf, (Len + 1) * SizeOf(Char));
    end;
  end;

begin
  Result := SQL;
  Value := PChar(Result);
  if DoCreate then Clear;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = ':') and not Literal and ((CurPos + 1)^ <> ':') then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter) do
      begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := string(StartPos + 1);
      if DoCreate then
        TODBCParam(Add).Name := Name;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = ':') and not Literal and ((CurPos + 1)^ = ':') then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;


{ TODBCColumn }

//..


{ TODBCColumns }

constructor TODBCColumns.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create(TODBCColumn);
end;

function TODBCColumns.GetRecordSize: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + GetItem(i).DataLength;
end;

function TODBCColumns.GetItem(Index: Integer): TODBCColumn;
begin
  Result := TODBCColumn(inherited Items[Index]);
end;

procedure TODBCColumns.SetItem(Index: Integer; const Value: TODBCColumn);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

function TODBCColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TODBCColumns.FindColumn(const Value: string): TODBCColumn;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := TODBCColumn(inherited Items[i]);
    if CompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TODBCColumns.ColumnByName(const Value: string): TODBCColumn;
begin
  Result := FindColumn(Value);
  if Result = nil then
    DatabaseErrorFmt(SSQLDANameDoesNotExist, [Value]);
end;


{ TODBCCursor }

constructor TODBCCursor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TODBCColumns.Create(Self);
  FBLOBsList := TList.Create;
end;

destructor TODBCCursor.Destroy;
begin
  FBLOBsList.Free;
  FColumns.Free;
  inherited;
end;

function TODBCCursor.GetBlobColumnCount: Integer;
begin
  Result := FBLOBsList.Count;
end;

procedure TODBCCursor.CheckOpen;
begin
  if not FActive then
    DatabaseError(SCursorClosed);
end;

procedure TODBCCursor.CheckError(RetCode: SQLRETURN);
begin
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_STMT, FHandle, RetCode);
end;

procedure TODBCCursor.InitColumns;
var
  i: SmallInt;
begin
  for i := 0 to FColumnCount do
    FColumns.Add;
end;

procedure TODBCCursor.DescribeColumns;

  function GetColDataLength(SQLType: SQLSMALLINT; ColSize: SQLUINTEGER): SQLUINTEGER;
  begin
    case SQLType of
      SQL_CHAR, SQL_VARCHAR:
        Result := ColSize;

      SQL_WCHAR, SQL_WVARCHAR:
        Result := ColSize * SizeOf(WideChar);

      SQL_SMALLINT, SQL_TINYINT:
        Result := SizeOf(SQLSMALLINT);

      SQL_INTEGER:
        Result := SizeOf(SQLINTEGER);

      SQL_REAL:
        Result := SizeOf(SQLDOUBLE);

      SQL_FLOAT, SQL_DOUBLE:
        Result := SizeOf(SQLDOUBLE);

      SQL_NUMERIC, SQL_DECIMAL:
        Result := ColSize + 12;                   //(sign, decimal_sep, thousand_sep, #0.

      SQL_BIT:
        Result := SizeOf(Byte);

      SQL_BIGINT:
        Result := SizeOf(Int64);

      SQL_TYPE_DATE:
        Result := SizeOf(SQL_DATE_STRUCT);

      SQL_TYPE_TIME:
        Result := SizeOf(SQL_TIME_STRUCT);

      SQL_TYPE_TIMESTAMP:
        Result := SizeOf(SQL_TIMESTAMP_STRUCT);

      SQL_INTERVAL, SQL_INTERVAL_YEAR..SQL_INTERVAL_MINUTE_TO_SECOND:
        Result := ColSize;

      // To workaround TBytesField and TVarBytesField data size check
      SQL_BINARY, SQL_VARBINARY:
        Result := 1;

      SQL_GUID:
        // SQL_GUID  ColimnSize = 36 (the number of characters in the aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee format)
        Result := 16;                             // the number of used bytes
    else
      //SQL_LONGVARBINARY
      //SQL_LONGVARCHAR
      //SQL_WLONGVARCHAR
      Result := 0;
    end;
  end;

  function GetColDataBufSize(SQLType: SQLSMALLINT; ColDataLen: SQLUINTEGER): SQLUINTEGER;
  begin
    case SQLType of
      SQL_CHAR, SQL_VARCHAR:
        Result := ColDataLen + SizeOf(AnsiChar);      // terminated 0

      SQL_WCHAR, SQL_WVARCHAR:
        Result := ColDataLen + SizeOf(WideChar);  // terminated 0

      SQL_INTERVAL, SQL_INTERVAL_YEAR..SQL_INTERVAL_MINUTE_TO_SECOND:
        Result := ColDataLen + SizeOf(AnsiChar);      // terminated 0
    else
      Result := ColDataLen;
    end;
  end;

var
  Column: TODBCColumn;
{$IFNDEF ODBC_DAC_UNICODE}
  ColNameA: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  ColNameW: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  ColNameLength: SQLSMALLINT;
  SQLType: SQLSMALLINT;
  ColumnSize: SQLUINTEGER;
  DecimalDigits: SQLSMALLINT;
  Nullable: SQLSMALLINT;

  NumAttr: Integer;
  i: Integer;
{$IFDEF USE_DRV_SPEC}
  ODBCDriverType: TODBCDrvType;
{$ENDIF}
begin
  FColumns[0].Name := 'ODBC_BOOKMARK';
  FColumns[0].CType := SQL_C_VARBOOKMARK;

  for i := 1 to FColumns.Count - 1 do             // 1 based column index
  begin
{$IFNDEF ODBC_DAC_UNICODE}
    CheckError(SQLDescribeColA(FHandle, i, @ColNameA, STR_LEN, @ColNameLength, @SQLType, @ColumnSize, @DecimalDigits, @Nullable));
    CheckError(SQLColAttributeA(FHandle, i, SQL_DESC_AUTO_UNIQUE_VALUE, nil, 0, nil, @NumAttr));
{$ELSE}
    CheckError(SQLDescribeColW(FHandle, i, @ColNameW, STR_LEN, @ColNameLength, @SQLType, @ColumnSize, @DecimalDigits, @Nullable));
    CheckError(SQLColAttributeW(FHandle, i, SQL_DESC_AUTO_UNIQUE_VALUE, nil, 0, nil, @NumAttr));
{$ENDIF}

    Column := FColumns[i];
{$IFNDEF ODBC_DAC_UNICODE}
    Column.Name := UnicodeString(AnsiString(PSQLCHAR(@ColNameA)));
{$ELSE}
    Column.Name := UnicodeString(PSQLWCHAR(@ColNameW));
{$ENDIF}

{$IFDEF USE_DRV_SPEC}
    // Adjust data processing relative custom driver specific
    ODBCDriverType := GetODBCDriverType((Owner as TODBCCustomStatement).GetConnectionHandle);
    UpdateSQLTypeToDrvSpec(ODBCDriverType, SQLType);
{$ENDIF}

    Column.SQLType := SQLType;
    Column.CType := SQLTypeToCType(SQLType);

{$IFDEF USE_DRV_SPEC}
    // for some drivers which return DecimalDigits > Precision
    // set DecimalDigits := Precision (for example Informix driver)
    if ColumnSize < SQLUINTEGER(DecimalDigits) then
      DecimalDigits := ColumnSize;
{$ENDIF}

    Column.ColumnSize := ColumnSize;
    Column.DecimalDigits := DecimalDigits;

    Column.DataLength := GetColDataLength(SQLType, ColumnSize);

    // Workaround for wrong result of SQLDescribeCol function
    // for VARCHAR datatype in ODBC driver for MySQL
{$IFDEF USE_DRV_SPEC}
    // Workaround for MySQL server for VARCHAR data type
    if (ODBCDriverType = drvtMySql)
      and (SQLType = SQL_VARCHAR) then
      Column.DataLength := 255 + 1;
{$ENDIF}

    Column.FNullable := Nullable = SQL_NULLABLE;
    Column.FIsAutoInc := NumAttr = SQL_TRUE;

    Column.DataBufSize := GetColDataBufSize(SQLType, Column.DataLength);

    if Column.IsBlob then
      FBLOBsList.Add(Column)
    else
      Column.Data := AllocMem(Column.DataBufSize);
  end;
end;

procedure TODBCCursor.BindColumns;
var
  Column: TODBCColumn;
  i: Integer;
begin
  for i := 1 to FColumns.Count - 1 do             // 1 based column index
  begin
    Column := FColumns[i];

    if not Column.IsBlob then
      CheckError(SQLBindCol(FHandle, i, Column.CType, Column.Data, Column.DataBufSize, @(Column.StrLen_Or_Ind)));
  end;
end;

procedure TODBCCursor.Init(AHandle: SQLHSTMT);
begin
  FHandle := AHandle;

  CheckError(SQLNumResultCols(AHandle, @FColumnCount));

  if FColumnCount > 0 then
  begin
    InitColumns;
    DescribeColumns;
    BindColumns;
  end;
end;

procedure TODBCCursor.Clear;
begin
  FColumns.Clear;
  FBLOBsList.Clear;
end;

procedure TODBCCursor.Open;
begin
  FActive := True;
  FBOF := True;
  FEOF := False;
  FRecordCount := 0;

  if FGoToFirstRecord then
    Next;
end;

procedure TODBCCursor.Next;

  procedure GetLongData;
  const
    BLOBCHUNKSIZE = 1024;
  var
    Column: TODBCColumn;
    DataOfs: Integer;
    RetCode: SQLRETURN;
    i: Integer;
  begin
    for i := 0 to FBLOBsList.Count - 1 do
    begin
      Column := TODBCColumn(FBLOBsList[i]);

      Column.DataBufSize := 0;
      DataOfs := 0;

      // clear dynamically allocated previous long data
      FreeMem(Column.FData);
      Column.StrLen_Or_Ind := SQL_NULL_DATA;

      Column.FData := AllocMem(Column.DataBufSize);

      case Column.CType of
        SQL_C_CHAR:
          repeat
            if Column.FData <> nil then
              Inc(DataOfs, BLOBCHUNKSIZE - 1);

            if DataOfs > 0 then
              Inc(Column.FDataBufSize, BLOBCHUNKSIZE - 1)
            else
              Inc(Column.FDataBufSize, BLOBCHUNKSIZE);

            ReallocMem(Column.FData, Column.DataBufSize);
            RetCode := SQLGetData(FHandle, Column.Index, Column.CType, PByte(Column.FData) + DataOfs, BLOBCHUNKSIZE, @(Column.FStrLen_Or_Ind));
            CheckError(RetCode);

          until RetCode <> SQL_SUCCESS_WITH_INFO;

        SQL_C_WCHAR:
          repeat
            if Column.FData <> nil then
              Inc(DataOfs, BLOBCHUNKSIZE - 2);

            if DataOfs > 0 then
              Inc(Column.FDataBufSize, BLOBCHUNKSIZE - 2)
            else
              Inc(Column.FDataBufSize, BLOBCHUNKSIZE);

            ReallocMem(Column.FData, Column.DataBufSize);
            RetCode := SQLGetData(FHandle, Column.Index, Column.CType, PByte(Column.FData) + DataOfs, BLOBCHUNKSIZE, @(Column.FStrLen_Or_Ind));
            CheckError(RetCode);

          until RetCode <> SQL_SUCCESS_WITH_INFO;
      else
        repeat
          if Column.FData <> nil then
            Inc(DataOfs, BLOBCHUNKSIZE);

          Inc(Column.FDataBufSize, BLOBCHUNKSIZE);
          ReallocMem(Column.FData, Column.DataBufSize);
          RetCode := SQLGetData(FHandle, Column.Index, Column.CType, PByte(Column.FData) + DataOfs, BLOBCHUNKSIZE, @(Column.FStrLen_Or_Ind));
          CheckError(RetCode);

        until RetCode <> SQL_SUCCESS_WITH_INFO;
      end;
      Column.DataLength := DataOfs + Column.FStrLen_Or_Ind;
    end;
  end;

var
  RetCode: SQLRETURN;
begin
  CheckOpen;

  //RetCode := SQLFetchScroll(FHandle, SQL_FETCH_NEXT, 1);
  RetCode := SQLFetch(FHandle);

  if RetCode = SQL_NO_DATA then
    FEOF := True
  else
  begin
    CheckError(RetCode);

    // GetBookmarkData; (not implemented)
    GetLongData;

    Inc(FRecordCount);
    FBOF := False;
  end;
end;

procedure TODBCCursor.Close;
begin
  FActive := False;
  FEOF := False;
  FBOF := False;
  FRecordCount := 0;
end;




{ TODBCCustomStatement }

constructor TODBCCustomStatement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := QueryChanged;

  FParams := TODBCParams.Create(Self);
  FParamCheck := True;

  FCursor := TODBCCursor.Create(Self);
  FCursor.GotoFirstRecord := True;
end;

destructor TODBCCustomStatement.Destroy;
begin
  FCursor.Free;
  FParams.Free;
  FSQL.Free;
  inherited;
end;

function TODBCCustomStatement.GetRowsAffected: SQLINTEGER;
begin
  CheckError(SQLRowCount(FStmtHandle, @Result));
end;

procedure TODBCCustomStatement.SetSQL(Value: TStrings);
begin
  FSQL.Assign(Value);
end;

procedure TODBCCustomStatement.CheckPrepared;
begin
  if not Prepared then
    DatabaseError(SUnpreparedStmt);
end;

procedure TODBCCustomStatement.CheckClosed;
begin
  if IsCursorOpen then
    DatabaseError(SCursorOpen);
end;

procedure TODBCCustomStatement.CheckError(RetCode: SQLRETURN);
begin
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_STMT, FStmtHandle, RetCode);
end;

procedure TODBCCustomStatement.SetPrepared(Value: Boolean);
begin
  CheckClosed;

  if FPrepared <> Value then
  begin
    if Value then
      DoPrepare
    else
      DoUnprepare;

    FPrepared := Value;
  end;
end;

procedure TODBCCustomStatement.AllocateHandle;
var
  RetCode: SQLRETURN;
begin
  if FStmtHandle = nil then
  begin
    FConnHandle := GetConnectionHandle;

    RetCode := SQLAllocHandle(SQL_HANDLE_STMT, FConnHandle, @FStmtHandle);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_DBC, FConnHandle, RetCode);
  end;
end;

procedure TODBCCustomStatement.FreeHandle;
begin
  if FStmtHandle <> nil then
  begin
    CheckClosed;
    CheckError(SQLFreeHandle(SQL_HANDLE_STMT, FStmtHandle));
    FStmtHandle := nil;
  end;
end;

procedure TODBCCustomStatement.BindParams;
var
  Param: TODBCParam;
  PT: SQLSMALLINT;                                // Param type
  ColDef: SQLUINTEGER;
  DataPtr: SQLPOINTER;
  i: SmallInt;
{$IFDEF USE_DRV_SPEC}
  ODBCDriverType: TODBCDrvType;
{$ENDIF}
begin
  for i := 1 to FParams.Count do
  begin
    Param := FParams[i - 1];
    PT := ParamTypeToParamConst(Param.ParamType);

{$IFDEF USE_DRV_SPEC}
    ODBCDriverType := GetODBCDriverType(GetConnectionHandle);
{$ENDIF}

    if ISBLOBTYPE(Param.SQLType) then
    begin
      ColDef := Param.DataLength;
      DataPtr := Pointer(Param.Index);
    end
    else
    begin
{$IFDEF USE_DRV_SPEC}
      case ODBCDriverType of
        // MSSQL Driver Specific
        drvtMsSqlServer:
          case Param.SQLType of
            // TIMESTAMP data size adjust
            SQL_TYPE_TIME,
              SQL_TYPE_TIMESTAMP,
              SQL_INTERVAL_SECOND,
              SQL_INTERVAL_DAY_TO_SECOND,
              SQL_INTERVAL_HOUR_TO_SECOND,
              SQL_INTERVAL_MINUTE_TO_SECOND:
              begin
                Param.ColumnSize := 23;
                Param.DecimalDigits := 3;
              end;
          end;
        // MS Jet Driver Specific
        drvtMsJet:
          if Param.SQLType = SQL_DECIMAL then
            Param.SQLType := SQL_NUMERIC;

      end;
{$ENDIF}

      ColDef := Param.ColumnSize;
      DataPtr := Param.Data;
    end;

{$IFDEF USE_DRV_SPEC}
    // MSSQL Driver Specific
    // non-zero ColDef
    if (ODBCDriverType = drvtMsSqlServer) and (Param.StrLen_Or_Ind = SQL_NULL_DATA) and (ColDef = 0) then
      ColDef := 1;
{$ENDIF}
    CheckError(SQLBindParameter(FStmtHandle, i, PT, Param.CType, Param.SQLType, ColDef, Param.DecimalDigits, DataPtr, Param.DataBufSize, @(Param.StrLen_Or_Ind)));
  end;
end;

procedure TODBCCustomStatement.DoPrepare;

  procedure CheckProcessedSQL;
  begin
    if Trim(FProcessedSQL) = '' then
      DatabaseError(SEmptyQuery)
    else
      if Trim(FProcessedSQL) = SQL_NO_UNIQUE_KEY then
        DatabaseError(SNoUniqueKey);
  end;

begin
  CheckProcessedSQL;

  AllocateHandle;

  // set static cursor to avoid problems with blobs
  // prepare SQL
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLSetStmtAttrA(FStmtHandle, SQL_ATTR_CURSOR_TYPE, Pointer(SQL_CURSOR_STATIC), SQL_IS_UINTEGER));
  CheckError(SQLPrepareA(FStmtHandle, PSQLCHAR(AnsiString(FProcessedSQL)), SQL_NTS));
{$ELSE}
  CheckError(SQLSetStmtAttrW(FStmtHandle, SQL_ATTR_CURSOR_TYPE, Pointer(SQL_CURSOR_STATIC), SQL_IS_UINTEGER));
  CheckError(SQLPrepareW(FStmtHandle, PSQLWCHAR(FProcessedSQL), SQL_NTS));
{$ENDIF}
end;

procedure TODBCCustomStatement.DoUnprepare;
begin
  CheckError(SQLFreeStmt(FStmtHandle, SQL_RESET_PARAMS));
  FParams.Clear;

  FreeHandle;
end;

procedure TODBCCustomStatement.QueryChanged(Sender: TObject);
var
  List: TODBCParams;
begin
  if not (csReading in ComponentState) then
  begin
    Close;
    SetPrepared(False);

    if ParamCheck or (csDesigning in ComponentState) then
    begin
      List := TODBCParams.Create(Self);
      try
        FProcessedSQL := List.ParseSQL(SQL.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end
    else
      FProcessedSQL := FSQL.Text;
  end
  else
    FProcessedSQL := FParams.ParseSQL(FSQL.Text, False);
end;

procedure TODBCCustomStatement.GetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER);
begin
  CheckPrepared;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLGetStmtAttrA(FStmtHandle, Attribute, ValuePtr, BufferLength, StringLengthPtr));
{$ELSE}
  CheckError(SQLGetStmtAttrW(FStmtHandle, Attribute, ValuePtr, BufferLength, StringLengthPtr));
{$ENDIF}
end;

procedure TODBCCustomStatement.SetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; StringLength: SQLINTEGER);
begin
  CheckPrepared;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLSetStmtAttrA(FStmtHandle, Attribute, ValuePtr, StringLength));
{$ELSE}
  CheckError(SQLSetStmtAttrW(FStmtHandle, Attribute, ValuePtr, StringLength));
{$ENDIF}
end;

procedure TODBCCustomStatement.Prepare;
begin
  SetPrepared(True);
end;

procedure TODBCCustomStatement.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TODBCCustomStatement.Execute;

  procedure PutLongData;
  var
    ParamIndex: SmallInt;
    ParamLength: Integer;

    DataPtr: Pointer;
    DataSize: Integer;
    Ofs: Integer;

    RetCode: SQLRETURN;
  begin
    repeat
      RetCode := SQLParamData(FStmtHandle, @ParamIndex);
      if RetCode <> SQL_NEED_DATA then
        CheckError(RetCode);

      if RetCode = SQL_NEED_DATA then
      begin
        DataPtr := FParams[ParamIndex].Data;
        ParamLength := FParams[ParamIndex].DataLength;

        Ofs := 0;

        if ParamLength > DefaultMaxBlobChunkSize then
          DataSize := DefaultMaxBlobChunkSize
        else
          DataSize := ParamLength;

        repeat
          CheckError(SQLPutData(FStmtHandle, DataPtr, DataSize));
          Inc(PByte(DataPtr), DataSize);
          Inc(Ofs, DataSize);

          if not ((ParamLength - Ofs) > DefaultMaxBlobChunkSize) then
            DataSize := ParamLength mod DefaultMaxBlobChunkSize;

        until Ofs >= ParamLength;
      end;

    until RetCode <> SQL_NEED_DATA;
  end;

begin
  CheckClosed;

  if not FPrepared then
    Prepare;

  if FParams.Count > 0 then
    BindParams;

  // execute
  try
    CheckError(SQLExecute(FStmtHandle));
  except
    on E: EODBCError do
      if E.RetCode = SQL_NEED_DATA then
        PutLongData
      else
        if E.RetCode = SQL_NO_DATA then
        begin
          // do nothing (silent exit)
        end
        else
          raise;
  end;

  // init Cursor
  FCursor.Init(FStmtHandle);

  if FCursor.Columns.Count > 0 then
    FCursor.Open;
end;

procedure TODBCCustomStatement.Close;
begin
  if not IsCursorOpen then
    Exit;

  CheckError(SQLCloseCursor(FStmtHandle));
  FCursor.Close;
  FCursor.Clear;
end;

function TODBCCustomStatement.IsCursorOpen: Boolean;
begin
  Result := FCursor.Active;
end;

end.
