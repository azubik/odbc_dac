
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCConnection                                  }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCConnection;
{$I sv.inc}
interface
uses
  Classes,
  DB,
  odbcsqltypes,
  odbcsql,
{$IFDEF ODBC_DAC_UNICODE}
  odbcsqlucode,
{$ENDIF}
  odbcsqlext;

type

  { Forward declaration }
  TODBCConnection = class;


  { TODBCEnvironment }
  TODBCDataSourceType = (dsUser, dsSystem, dsFile, dsAny);

  TODBCEnvironment = class(TComponent)
  private
    FODBCLoaded: Boolean;

    FHandle: SQLHENV;
    FConnections: TList;

    FStreamedActive: Boolean;

    function GetActive: Boolean;
    function GetConnection(Index: Integer): TODBCConnection;
    function GetConnectionCount: Integer;

    procedure SetActive(Value: Boolean);

    procedure CheckActive;
    procedure CheckInactiveConnections;
    procedure CheckError(RetCode: SQLRETURN);

    procedure ClearEnvironmentUsers;

  protected
    procedure Loaded; override;

    procedure DoOpen;
    procedure DoClose;

    procedure RegisterConnection(Connection: TODBCConnection; Event: TConnectChangeEvent = nil);
    procedure UnregisterConnection(Connection: TODBCConnection);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER);
    procedure SetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; StringLength: SQLINTEGER);

    procedure Open;
    procedure Close;

    procedure CloseConnections;

    procedure GetDataSourceNames(DataSourceType: TODBCDataSourceType; List: TStrings);
    procedure GetDriverNames(List: TStrings);

    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: TODBCConnection read GetConnection;

    property Handle: SQLHENV read FHandle;

  published
    property Active: Boolean read GetActive write SetActive default False;
  end;


  { TODBCConnection }

  TDriverCompletion = (dcNoPrompt, dcComplete, dcPrompt, dcComplete_Required);

  TODBCTableType = (ttTable, ttView, ttSystemTable, ttGlobalTemporary, ttLocalTemporary, ttAlias, ttSynonym, ttAny);

  TODBCConnection = class(TCustomConnection)
  private
    FEnvironment: TODBCEnvironment;
    FHandle: SQLHDBC;

    FStatements: TList;
    FParams: TStrings;

    FDriverCompletion: TDriverCompletion;
    FKeepConnection: Boolean;

    FLoginTimeOut: Cardinal;
    FLoginTimeOutUpdated: Boolean;

    function GetDriverLib: string;
    function GetDBMSName: string;

    function GetCatalogName: string;
    function GetSchemaName: string;

    function GetInTransaction: Boolean;
    function GetLoginTimeOut: Cardinal;
    function GetQuoteChar: Char;

    function GetStatement(Index: Integer): TObject;
    function GetStatementCount: Integer;

    procedure SetEnvironment(Value: TODBCEnvironment);
    procedure SetKeepConnection(const Value: Boolean);
    procedure SetLoginTimeOut(Value: SQLUINTEGER);
    procedure SetParams(Value: TStrings);

    procedure CheckEnvironment;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckDisconnect;

    procedure ActivateEnvironment;
    procedure ClearConnectionUsers;
    procedure Login(LoginParams: TStrings);

  protected
    procedure CheckError(RetCode: SQLRETURN);

    procedure DoConnect; override;
    procedure DoDisconnect; override;

    function GetConnected: Boolean; override;

{$IFDEF USE_DRV_SPEC}
    procedure SetDriverSpecificAttrs;
{$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetInfo(InfoType: SQLUSMALLINT; InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT);
    function SQLDescribeParamSupported: Boolean;

    procedure GetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER);
    procedure SetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; StringLength: SQLINTEGER);

    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); override;
    procedure UnRegisterClient(Client: TObject); override;

    procedure GetStoredProcNames(List: TStrings; const Schema: string = '');
    procedure GetTableNames(List: TStrings; ATableType: TODBCTableType; const Schema: string = '');

    procedure ApplyUpdates(const DataSets: array of TDataSet);
    procedure CloseDataSets;

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    property Handle: SQLHDBC read FHandle;

    property DriverCompletion: TDriverCompletion read FDriverCompletion write FDriverCompletion;

    property DriverLib: string read GetDriverLib;
    property DBMSName: string read GetDBMSName;
    property CatalogName: string read GetCatalogName;
    property SchemaName: string read GetSchemaName;

    property InTransaction: Boolean read GetInTransaction;
    property LoginTimeOut: Cardinal read GetLoginTimeOut write SetLoginTimeOut;
    property QuoteChar: Char read GetQuoteChar;

    property Statements[Index: Integer]: TObject read GetStatement;
    property StatementCount: Integer read GetStatementCount;

  published
    property Connected;
    property Environment: TODBCEnvironment read FEnvironment write SetEnvironment;
    property KeepConnection: Boolean read FKeepConnection write SetKeepConnection default True;
    property LoginPrompt default True;
    property Params: TStrings read FParams write SetParams;

    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
    property OnLogin;

  end;

var
  ODBCEnvironment: TODBCEnvironment;


implementation
uses
  SysUtils,
  Forms,
  DbLogDlg,
{$IFDEF USE_DRV_SPEC}
  ODBCDrvSpec,                                    // custom driver specific
{$ENDIF}
  ODBCIntf,
  ODBCException,
  ODBCUtils,
  ODBCConsts,
  ODBCCustomDataSet,
  ODBCQuery;


const
  szDSN = 'DSN';                                  { do not localize }
  szFILEDSN = 'FILEDSN';                          { do not localize }
  szDRIVER = 'DRIVER';                            { do not localize }
  szUSER = 'UID';                                 { do not localize }
  szPASSWORD = 'PWD';                             { do not localize }


  { TODBCEnvironment }

constructor TODBCEnvironment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FODBCLoaded := False;
  CheckODBCLoaded;
  FODBCLoaded := True;

  FConnections := TList.Create;
end;

destructor TODBCEnvironment.Destroy;
begin
  if FODBCLoaded then
  begin
    Close;
    ClearEnvironmentUsers;
    FConnections.Free;
  end;
  inherited Destroy;
end;

function TODBCEnvironment.GetActive: Boolean;
begin
  Result := FHandle <> nil;
end;

function TODBCEnvironment.GetConnection(Index: Integer): TODBCConnection;
begin
  Result := FConnections[Index];
end;

function TODBCEnvironment.GetConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

procedure TODBCEnvironment.SetActive(Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Active <> Value then
      if Value then
        DoOpen
      else
        DoClose;
end;

procedure TODBCEnvironment.CheckActive;
begin
  if not Active then
    DatabaseError(SEnvInactive);
end;

procedure TODBCEnvironment.CheckInactiveConnections;
var
  i: Integer;
begin
  for i := 0 to ConnectionCount - 1 do
    if TODBCConnection(FConnections[i]).Connected then
      DatabaseError(SInactiveConns);
end;

procedure TODBCEnvironment.CheckError(RetCode: SQLRETURN);
begin
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_ENV, FHandle, RetCode);
end;

procedure TODBCEnvironment.ClearEnvironmentUsers;
begin
  while FConnections.Count > 0 do
    TODBCConnection(FConnections[0]).Environment := nil;
end;

procedure TODBCEnvironment.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then SetActive(True);
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TODBCEnvironment.DoOpen;
var
  RetCode: SQLRETURN;
begin
  // Allocate environment handle
  RetCode := SQLAllocHandle(SQL_HANDLE_ENV, SQLHANDLE(SQL_NULL_HANDLE), @FHandle);
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_ENV, SQLHANDLE(SQL_NULL_HANDLE), RetCode);

  // Set ODBC version attribute
  CheckError(SQLSetEnvAttr(FHandle, SQL_ATTR_ODBC_VERSION, SQLPOINTER(SQL_OV_ODBC3), 0));
end;

procedure TODBCEnvironment.DoClose;
begin
  CheckError(SQLFreeHandle(SQL_HANDLE_ENV, FHandle));
  FHandle := nil;
end;

procedure TODBCEnvironment.RegisterConnection(Connection: TODBCConnection; Event: TConnectChangeEvent = nil);
begin
  FConnections.Add(Connection);
end;

procedure TODBCEnvironment.UnregisterConnection(Connection: TODBCConnection);
begin
  FConnections.Remove(Connection);
end;

procedure TODBCEnvironment.GetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER);
begin
  CheckActive;
  CheckError(SQLGetEnvAttr(FHandle, Attribute, ValuePtr, BufferLength, StringLengthPtr));
end;

procedure TODBCEnvironment.SetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; StringLength: SQLINTEGER);
begin
  CheckActive;
  CheckInactiveConnections;

  CheckError(SQLSetEnvAttr(FHandle, Attribute, SQLPOINTER(ValuePtr), StringLength));
end;

procedure TODBCEnvironment.Open;
begin
  SetActive(True);
end;

procedure TODBCEnvironment.Close;
begin
  SetActive(False);
end;

procedure TODBCEnvironment.CloseConnections;
var
  i: Integer;
begin
  for i := 0 to ConnectionCount - 1 do
    with Connections[i] do
      if Connected then
        Close;
end;


procedure TODBCEnvironment.GetDataSourceNames(DataSourceType: TODBCDataSourceType; List: TStrings);
var
  Direction: SQLUSMALLINT;
{$IFNDEF ODBC_DAC_UNICODE}
  ServerNameA: array[0..SQL_MAX_DSN_LENGTH] of SQLCHAR;
  DescriptionA: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
{$ELSE}
  ServerNameW: array[0..SQL_MAX_DSN_LENGTH] of SQLWCHAR;
  DescriptionW: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLWCHAR;
{$ENDIF}
  ServerName: string;
  Description: string;
  Strings: TStrings;
begin
  List.Clear;

  case DataSourceType of
    dsUser:
      Direction := SQL_FETCH_FIRST_USER;

    dsSystem:
      Direction := SQL_FETCH_FIRST_SYSTEM;

    dsAny:
      Direction := SQL_FETCH_FIRST;
  else
    Exit;
  end;

  Strings := TStringList.Create;
  try
    while SQL_SUCCEEDED(
{$IFNDEF ODBC_DAC_UNICODE}
    SQLDataSourcesA(FHandle, Direction, @ServerNameA, SQL_MAX_DSN_LENGTH, nil, @DescriptionA, SQL_MAX_OPTION_STRING_LENGTH, nil)
{$ELSE}
    SQLDataSourcesW(FHandle, Direction, @ServerNameW, SQL_MAX_DSN_LENGTH, nil, @DescriptionW, SQL_MAX_OPTION_STRING_LENGTH, nil)
{$ENDIF}
    ) do
    begin
{$IFNDEF ODBC_DAC_UNICODE}
      ServerName := UnicodeString(AnsiString(PSQLCHAR(@ServerNameA)));
      Description := UnicodeString(AnsiString(PSQLCHAR(@DescriptionA)));
{$ELSE}
      ServerName := UnicodeString(PSQLWCHAR(@ServerNameW));
      Description := UnicodeString(PSQLWCHAR(@DescriptionW));
{$ENDIF}

      // Build commatext string
      Strings.Clear;
      Strings.Add(ServerName);
      Strings.Add(Description);
      // Add commatext string
      List.Add(Strings.CommaText);

      Direction := SQL_FETCH_NEXT;
    end;
  finally
    Strings.Free;
  end;
end;

procedure TODBCEnvironment.GetDriverNames(List: TStrings);
var
  Direction: SQLUSMALLINT;
{$IFNDEF ODBC_DAC_UNICODE}
  DriverDescA: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
{$ELSE}
  DriverDescW: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLWCHAR;
{$ENDIF}
  DriverDesc: string;
begin
  List.Clear;
  Direction := SQL_FETCH_FIRST;

  while SQL_SUCCEEDED(
{$IFNDEF ODBC_DAC_UNICODE}
  SQLDriversA(FHandle, Direction, @DriverDescA, SQL_MAX_OPTION_STRING_LENGTH, nil, nil, 0, nil)
{$ELSE}
  SQLDriversW(FHandle, Direction, @DriverDescW, SQL_MAX_OPTION_STRING_LENGTH, nil, nil, 0, nil)
{$ENDIF}
  ) do
  begin
{$IFNDEF ODBC_DAC_UNICODE}
    DriverDesc := UnicodeString(AnsiString(PSQLCHAR(@DriverDescA)));
{$ELSE}
    DriverDesc := UnicodeString(PSQLWCHAR(@DriverDescW));
{$ENDIF}
    List.Add(DriverDesc);
    Direction := SQL_FETCH_NEXT;
  end;
end;


{ TODBCConnection }

constructor TODBCConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStatements := TList.Create;
  FParams := TStringList.Create;

  if AOwner is TODBCEnvironment then
    Environment := TODBCEnvironment(AOwner)
  else
    Environment := ODBCConnection.ODBCEnvironment;

  FKeepConnection := True;
  LoginPrompt := True;
end;

destructor TODBCConnection.Destroy;
begin
  Close;
  ClearConnectionUsers;

  SetEnvironment(nil);

  FParams.Free;
  FStatements.Free;

  inherited;
end;

function TODBCConnection.GetDriverLib: string;
var
{$IFNDEF ODBC_DAC_UNICODE}
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  Buffer: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetInfo(SQL_DRIVER_NAME, @Buffer, SizeOf(Buffer), @BufLen);
{$IFNDEF ODBC_DAC_UNICODE}
  Result := UnicodeString(AnsiString(PSQLCHAR(@Buffer)));
{$ELSE}
  Result := UnicodeString(PSQLWCHAR(@Buffer));
{$ENDIF}
end;

function TODBCConnection.GetDBMSName: string;
var
{$IFNDEF ODBC_DAC_UNICODE}
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  Buffer: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetInfo(SQL_DBMS_NAME, @Buffer, SizeOf(Buffer), @BufLen);
{$IFNDEF ODBC_DAC_UNICODE}
  Result := UnicodeString(AnsiString(PSQLCHAR(@Buffer)));
{$ELSE}
  Result := UnicodeString(PSQLWCHAR(@Buffer));
{$ENDIF}
end;

function TODBCConnection.GetCatalogName: string;
var
{$IFNDEF ODBC_DAC_UNICODE}
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  Buffer: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetAttribute(SQL_ATTR_CURRENT_CATALOG, @Buffer, SizeOf(Buffer), @BufLen);
{$IFNDEF ODBC_DAC_UNICODE}
  Result := UnicodeString(AnsiString(PSQLCHAR(@Buffer)));
{$ELSE}
  Result := UnicodeString(PSQLWCHAR(@Buffer));
{$ENDIF}
end;

function TODBCConnection.GetSchemaName: string;
var
{$IFNDEF ODBC_DAC_UNICODE}
  BufferA: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  BufferW: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;
  ST: string;
{$IFDEF USE_DRV_SPEC}
  ODBCDriverType: TODBCDrvType;
{$ENDIF}
begin
  CheckActive;
{$IFNDEF ODBC_DAC_UNICODE}
  GetInfo(SQL_SCHEMA_TERM, @BufferA, SizeOf(BufferA), @BufLen);
  ST := UnicodeString(AnsiString(PSQLCHAR(@BufferA)));
  if ST <> '' then
    GetInfo(SQL_USER_NAME, @BufferA, SizeOf(BufferA), @BufLen);

  Result := UnicodeString(AnsiString(PSQLCHAR(@BufferA)));
{$ELSE}
  GetInfo(SQL_SCHEMA_TERM, @BufferW, SizeOf(BufferW), @BufLen);
  ST := UnicodeString(PSQLWCHAR(@BufferW));
  if ST <> '' then
    GetInfo(SQL_USER_NAME, @BufferW, SizeOf(BufferW), @BufLen);

  Result := UnicodeString(PSQLWCHAR(@BufferW));
{$ENDIF}

{$IFDEF USE_DRV_SPEC}
  ODBCDriverType := GetODBCDriverType(Handle);
  // Oracle
  if ODBCDriverType = drvtOracle then
    Result := UpperCase(Result);
{$ENDIF}
end;

function TODBCConnection.GetInTransaction: Boolean;
var
  TransactMode: SQLUINTEGER;
begin
  CheckActive;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLGetConnectAttrA(FHandle, SQL_ATTR_AUTOCOMMIT, @TransactMode, SQL_IS_UINTEGER, nil));
{$ELSE}
  CheckError(SQLGetConnectAttrW(FHandle, SQL_ATTR_AUTOCOMMIT, @TransactMode, SQL_IS_UINTEGER, nil));
{$ENDIF}

  Result := TransactMode = SQL_AUTOCOMMIT_OFF;
end;

function TODBCConnection.GetLoginTimeOut: SQLUINTEGER;
begin
  CheckActive;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLGetConnectAttrA(FHandle, SQL_ATTR_LOGIN_TIMEOUT, @Result, SQL_IS_UINTEGER, nil));
{$ELSE}
  CheckError(SQLGetConnectAttrW(FHandle, SQL_ATTR_LOGIN_TIMEOUT, @Result, SQL_IS_UINTEGER, nil));
{$ENDIF}
end;

function TODBCConnection.GetQuoteChar: Char;
var
{$IFNDEF ODBC_DAC_UNICODE}
  p: array[0..1] of SQLCHAR;
{$ELSE}
  p: array[0..1] of SQLWCHAR;
{$ENDIF}
begin
  GetInfo(SQL_IDENTIFIER_QUOTE_CHAR, @p, SizeOf(p), nil);
{$IFNDEF ODBC_DAC_UNICODE}
  Result := WideChar(SQLCHAR(p[0]));
{$ELSE}
  Result := Char(SQLWCHAR(p[0]));
{$ENDIF}
end;

function TODBCConnection.GetStatement(Index: Integer): TObject;
begin
  Result := FStatements[Index];
end;

function TODBCConnection.GetStatementCount: Integer;
begin
  Result := FStatements.Count;
end;

procedure TODBCConnection.SetEnvironment(Value: TODBCEnvironment);
begin
  CheckInactive;

  if FEnvironment <> Value then
  begin
    if Assigned(FEnvironment) then
      FEnvironment.UnRegisterConnection(Self);

    FEnvironment := Value;

    if Assigned(FEnvironment) then
      FEnvironment.RegisterConnection(Self);
  end;
end;

procedure TODBCConnection.SetKeepConnection(const Value: Boolean);
begin
  if FKeepConnection <> Value then
  begin
    FKeepConnection := Value;
    CheckDisconnect;
  end;
end;

procedure TODBCConnection.SetLoginTimeOut(Value: SQLUINTEGER);
begin
  CheckInactive;

  FLoginTimeOut := Value;
  FLoginTimeOutUpdated := True;
end;

procedure TODBCConnection.SetParams(Value: TStrings);
begin
  CheckInactive;
  FParams.Assign(Value);
end;

procedure TODBCConnection.CheckEnvironment;
begin
  if FEnvironment = nil then
    DatabaseError(SEnvironmentMissing);
end;

procedure TODBCConnection.CheckActive;
begin
  if FHandle = nil then
    DatabaseError(SDatabaseClosed);
end;

procedure TODBCConnection.CheckInactive;
begin
  if Assigned(FHandle) then
    if csDesigning in ComponentState then
      Close
    else
      DatabaseError(SDatabaseOpen);
end;

procedure TODBCConnection.CheckDisconnect;
var
  i: Integer;
begin
  if Connected and not (KeepConnection or InTransaction or (csLoading in ComponentState)) then
  begin
    for I := 0 to DataSetCount - 1 do
      if DataSets[I].State <> dsInActive then
        Exit;
    Close;
  end;
end;

procedure TODBCConnection.ActivateEnvironment;
begin
  CheckEnvironment;

  if not FEnvironment.Active then
  begin
    FEnvironment.Open;
    //..
  end;
end;

procedure TODBCConnection.ClearConnectionUsers;
begin
  while FStatements.Count > 0 do
    TODBCStatement(FStatements[0]).Connection := nil;

  while DataSetCount > 0 do
    TODBCCustomDataSetEx(DataSets[0]).Connection := nil;
end;

procedure TODBCConnection.Login(LoginParams: TStrings);
var
  DatabaseName: string;
  UserName: string;
  Password: string;
begin
  DatabaseName := Trim(LoginParams.Values[szDSN]);
  if DatabaseName = '' then
    DatabaseName := ExtractFileName(Trim(LoginParams.Values[szFILEDSN]));

  UserName := LoginParams.Values[szUSER];
  Password := LoginParams.Values[szPASSWORD];

  if Assigned(OnLogin) then
    OnLogin(Self, UserName, Password)
  else
    if LoginDialogEx(DatabaseName, UserName, Password, False) then
    begin
      LoginParams.Values[szUSER] := UserName;
      LoginParams.Values[szPASSWORD] := Password;
    end
    else
      DatabaseErrorFmt(SLoginError, [DatabaseName]);
end;

procedure TODBCConnection.CheckError(RetCode: SQLRETURN);
begin
  if not SQL_SUCCEEDED(RetCode) then
    ODBCError(SQL_HANDLE_DBC, FHandle, RetCode);
end;

procedure TODBCConnection.DoConnect;
const
  OUT_BUFSIZE = 1024;
var
  LoginParams: TStrings;
{$IFNDEF ODBC_DAC_UNICODE}
  ConnectStrOutA: array[0..OUT_BUFSIZE - 1] of SQLCHAR;
{$ELSE}
  ConnectStrOutW: array[0..OUT_BUFSIZE - 1] of SQLWCHAR;
{$ENDIF}
  ConnectStr: string;

  RetCode: SQLRETURN;
begin
  ActivateEnvironment;

  LoginParams := TStringList.Create;
  try
    LoginParams.Assign(FParams);
    if LoginPrompt then
      Login(LoginParams);

    // Alloc Connection Handle
    RetCode := SQLAllocHandle(SQL_HANDLE_DBC, FEnvironment.Handle, @FHandle);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_ENV, Environment.Handle, RetCode);

    try
      // Set login timeout
      // while using default value

      // Open Connection
{$IFNDEF ODBC_DAC_UNICODE}
      CheckError(SQLDriverConnectA(FHandle, Application.Handle, PSQLCHAR(AnsiString(ParamsToConnStr(LoginParams))), SQL_NTS, @ConnectStrOutA, OUT_BUFSIZE, nil, SQLUSMALLINT(FDriverCompletion)));
{$ELSE}
      CheckError(SQLDriverConnectW(FHandle, Application.Handle, PSQLWCHAR(ParamsToConnStr(LoginParams)), SQL_NTS, @ConnectStrOutW, OUT_BUFSIZE, nil, SQLUSMALLINT(FDriverCompletion)));
{$ENDIF}

{$IFDEF USE_DRV_SPEC}
      SetDriverSpecificAttrs;
{$ENDIF}
    except
      on E: EODBCError do
      begin
        CheckError(SQLFreeHandle(SQL_HANDLE_DBC, FHandle));
        FHandle := nil;

        if E.RetCode = SQL_NO_DATA then
          Abort
        else
          raise;
      end;
    end;

{$IFNDEF ODBC_DAC_UNICODE}
    ConnectStr := UnicodeString(AnsiString(PSQLCHAR(@ConnectStrOutA)));
{$ELSE}
    ConnectStr := UnicodeString(PSQLWCHAR(@ConnectStrOutW));
{$ENDIF}
    ConnStrToParams(ConnectStr, FParams);
  finally
    LoginParams.Free;
  end;
end;

procedure TODBCConnection.DoDisconnect;
begin
  while InTransaction do
    Rollback;

  CloseDataSets;

  CheckError(SQLDisconnect(FHandle));
  CheckError(SQLFreeHandle(SQL_HANDLE_DBC, FHandle));
  FHandle := nil;
end;

function TODBCConnection.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

{$IFDEF USE_DRV_SPEC}

procedure TODBCConnection.SetDriverSpecificAttrs;
var
  ODBCDriverType: TODBCDrvType;
begin
  ODBCDriverType := GetODBCDriverType(Handle);

  case ODBCDriverType of
    drvtIbmDb2:
      // IBM DB2 has driver-specific longdata type,
      // but setting this option makes it ODBC compatible:
      SetAttribute(SQL_LONGDATA_COMPAT, SQLPOINTER(SQL_LD_COMPAT_YES), 0);

    drvtInformix:
      begin
        // INFORMIX
        SetAttribute(SQL_INFX_ATTR_LO_AUTOMATIC, SQLPOINTER(SQL_TRUE), 0);

        // The row below commented to process additional Informix data types
        //SetAttribute(SQL_INFX_ATTR_ODBC_TYPES_ONLY, SQLPOINTER(SQL_TRUE), 0);

        SetAttribute(SQL_INFX_ATTR_DEFAULT_UDT_FETCH_TYPE, SQLPOINTER(SQL_C_CHAR), 0);
      end;

    //...
  end;
end;
{$ENDIF}

procedure TODBCConnection.GetInfo(InfoType: SQLUSMALLINT; InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT);
begin
  CheckActive;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLGetInfoA(FHandle, InfoType, InfoValuePtr, BufferLength, StringLengthPtr));
{$ELSE}
  CheckError(SQLGetInfoW(FHandle, InfoType, InfoValuePtr, BufferLength, StringLengthPtr));
{$ENDIF}
end;

function TODBCConnection.SQLDescribeParamSupported: Boolean;
var
{$IFNDEF ODBC_DAC_UNICODE}
  Buffer: array[0..1] of SQLCHAR;
{$ELSE}
  Buffer: array[0..1] of SQLWCHAR;
{$ENDIF}
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetInfo(SQL_DESCRIBE_PARAMETER, @Buffer, SizeOf(Buffer), @BufLen);

{$IFNDEF ODBC_DAC_UNICODE}
  Result := Buffer[0] = SQLCHAR('Y');                      //don't localize
{$ELSE}
  Result := Buffer[0] = SQLWCHAR('Y');                      //don't localize
{$ENDIF}
end;

procedure TODBCConnection.GetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER);
begin
  CheckActive;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLGetConnectAttrA(FHandle, Attribute, ValuePtr, BufferLength, StringLengthPtr));
{$ELSE}
  CheckError(SQLGetConnectAttrW(FHandle, Attribute, ValuePtr, BufferLength, StringLengthPtr));
{$ENDIF}
end;

procedure TODBCConnection.SetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; StringLength: SQLINTEGER);
begin
  CheckActive;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLSetConnectAttrA(FHandle, Attribute, ValuePtr, StringLength));
{$ELSE}
  CheckError(SQLSetConnectAttrW(FHandle, Attribute, ValuePtr, StringLength));
{$ENDIF}
end;

procedure TODBCConnection.RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil);
begin
  inherited;
  if (Client is TODBCStatement) and not (TODBCStatement(Client).Owner is TDataSet) then
    FStatements.Add(Client);
end;

procedure TODBCConnection.UnRegisterClient(Client: TObject);
begin
  inherited;
  if (Client is TODBCStatement) and not (TODBCStatement(Client).Owner is TDataSet) then
    FStatements.Remove(Client);
end;

procedure TODBCConnection.GetStoredProcNames(List: TStrings; const Schema: string = '');
const
  SCHEMA_NAME_COL_NUMBER = 2;
  PROC_NAME_COL_NUMBER = 3;
var
{$IFNDEF ODBC_DAC_UNICODE}
  SchemaNameBufA: array[0..STR_LEN - 1] of SQLCHAR;
  ProcNameBufA: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  SchemaNameBufW: array[0..STR_LEN - 1] of SQLWCHAR;
  ProcNameBufW: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  SchemaName: string;
  ProcName: string;

  StrLen_or_Ind: SQLINTEGER;

  Data: string;

  TmpHandle: SQLHSTMT;
  RetCode: SQLRETURN;
begin
  CheckActive;
  List.Clear;

  CheckError(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @TmpHandle));
  try
{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLProceduresA(TmpHandle, nil, 0, PSQLCHAR(AnsiString(Schema)), Length(Schema), nil, 0);
{$ELSE}
    RetCode := SQLProceduresW(TmpHandle, nil, 0, PSQLWCHAR(Schema), Length(Schema), nil, 0);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLBindCol(TmpHandle, SCHEMA_NAME_COL_NUMBER, SQL_CHAR, @SchemaNameBufA, STR_LEN, @StrLen_or_Ind);
{$ELSE}
    RetCode := SQLBindCol(TmpHandle, SCHEMA_NAME_COL_NUMBER, SQL_WCHAR, @SchemaNameBufW, STR_LEN, @StrLen_or_Ind);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLBindCol(TmpHandle, PROC_NAME_COL_NUMBER, SQL_CHAR, @ProcNameBufA, STR_LEN, @StrLen_or_Ind);
{$ELSE}
    RetCode := SQLBindCol(TmpHandle, PROC_NAME_COL_NUMBER, SQL_WCHAR, @ProcNameBufW, STR_LEN, @StrLen_or_Ind);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    while SQL_SUCCEEDED(SQLFetch(TmpHandle)) do
    begin
{$IFNDEF ODBC_DAC_UNICODE}
      SchemaName := UnicodeString(AnsiString(PSQLCHAR(@SchemaNameBufA)));
      ProcName := UnicodeString(AnsiString(PSQLCHAR(@ProcNameBufA)));
{$ELSE}
      SchemaName := UnicodeString(PSQLWCHAR(@SchemaNameBufW));
      ProcName := UnicodeString(PSQLWCHAR(@ProcNameBufW));
{$ENDIF}

      Data := SchemaName;
      if Data <> '' then
        Data := Data + SCHEMA_SEPARATOR;
      Data := Data + ProcName;

      List.Add(Data);
    end;
  finally
    SQLFreeHandle(SQL_HANDLE_STMT, TmpHandle);
  end;
end;

procedure TODBCConnection.GetTableNames(List: TStrings; ATableType: TODBCTableType; const Schema: string = '');
const
  SCHEMA_NAME_COL_NUMBER = 2;
  TABLE_NAME_COL_NUMBER = 3;
var
{$IFNDEF ODBC_DAC_UNICODE}
  SchemaNameBufA: array[0..STR_LEN - 1] of SQLCHAR;
  TableNameBufA: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  SchemaNameBufW: array[0..STR_LEN - 1] of SQLWCHAR;
  TableNameBufW: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  SchemaName: string;
  TableName: string;

  TableType: string;
  StrLen_or_Ind: SQLINTEGER;

  Data: string;

  TmpHandle: SQLHSTMT;
  RetCode: SQLRETURN;
begin
  CheckActive;
  List.Clear;

  // table type string
  TableType := GetTableTypeConst(Byte(ATableType));

  CheckError(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @TmpHandle));
  try
{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLTablesA(TmpHandle, nil, 0, PSQLCHAR(AnsiString(Schema)), Length(Schema), nil, 0, PSQLCHAR(AnsiString(TableType)), SQL_NTS);
{$ELSE}
    RetCode := SQLTablesW(TmpHandle, nil, 0, PSQLWCHAR(Schema), Length(Schema), nil, 0, PSQLWCHAR(TableType), SQL_NTS);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLBindCol(TmpHandle, SCHEMA_NAME_COL_NUMBER, SQL_CHAR, @SchemaNameBufA, STR_LEN, @StrLen_or_Ind);
{$ELSE}
    RetCode := SQLBindCol(TmpHandle, SCHEMA_NAME_COL_NUMBER, SQL_WCHAR, @SchemaNameBufW, STR_LEN, @StrLen_or_Ind);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLBindCol(TmpHandle, TABLE_NAME_COL_NUMBER, SQL_CHAR, @TableNameBufA, STR_LEN, @StrLen_or_Ind);
{$ELSE}
    RetCode := SQLBindCol(TmpHandle, TABLE_NAME_COL_NUMBER, SQL_WCHAR, @TableNameBufW, STR_LEN, @StrLen_or_Ind);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    while SQL_SUCCEEDED(SQLFetch(TmpHandle)) do
    begin
{$IFNDEF ODBC_DAC_UNICODE}
      SchemaName := UnicodeString(AnsiString(PSQLCHAR(@SchemaNameBufA)));
      TableName := UnicodeString(AnsiString(PSQLCHAR(@TableNameBufA)));
{$ELSE}
      SchemaName := UnicodeString(PSQLWCHAR(@SchemaNameBufW));
      TableName := UnicodeString(PSQLWCHAR(@TableNameBufW));
{$ENDIF}
      Data := SchemaName;
      if Data <> '' then
        Data := Data + SCHEMA_SEPARATOR;
      Data := Data + TableName;

      List.Add(Data);
    end;

  finally
    SQLFreeHandle(SQL_HANDLE_STMT, TmpHandle);
  end;
end;

procedure TODBCConnection.ApplyUpdates(const DataSets: array of TDataSet);
var
  i: Integer;
begin
  for i := Low(DataSets) to High(DataSets) do
    TODBCCustomDataSet(DataSets[i]).ApplyUpdates;
end;

procedure TODBCConnection.CloseDataSets;
var
  i: Integer;
begin
  for i := 0 to DataSetCount - 1 do
    with DataSets[i] do
      if Active then
        Close;
end;

procedure TODBCConnection.StartTransaction;
begin
  CheckActive;
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLSetConnectAttrA(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_OFF), SQL_IS_UINTEGER));
{$ELSE}
  CheckError(SQLSetConnectAttrW(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_OFF), SQL_IS_UINTEGER));
{$ENDIF}
end;

procedure TODBCConnection.Commit;
begin
  CheckActive;
  CheckError(SQLEndTran(SQL_HANDLE_DBC, FHandle, SQL_COMMIT));
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLSetConnectAttrA(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER));
{$ELSE}
  CheckError(SQLSetConnectAttrW(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER));
{$ENDIF}
end;

procedure TODBCConnection.Rollback;
begin
  CheckActive;
  CheckError(SQLEndTran(SQL_HANDLE_DBC, FHandle, SQL_ROLLBACK));
{$IFNDEF ODBC_DAC_UNICODE}
  CheckError(SQLSetConnectAttrA(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER));
{$ELSE}
  CheckError(SQLSetConnectAttrW(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER));
{$ENDIF}
end;


initialization
  ODBCEnvironment := TODBCEnvironment.Create(nil);
  ODBCEnvironment.Name := 'Default';              { Do not localize }
  ODBCEnvironment.Open;

finalization
  ODBCEnvironment.Free;

end.
