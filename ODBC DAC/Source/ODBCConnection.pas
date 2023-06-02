
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
  ServerName: array[0..SQL_MAX_DSN_LENGTH] of SQLCHAR;
  Description: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
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
    while SQL_SUCCEEDED(SQLDataSources(FHandle, Direction, @ServerName, SQL_MAX_DSN_LENGTH, nil, @Description, SQL_MAX_OPTION_STRING_LENGTH, nil)) do
    begin
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
  DriverDesc: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
begin
  List.Clear;
  Direction := SQL_FETCH_FIRST;

  while SQL_SUCCEEDED(SQLDrivers(FHandle, Direction, @DriverDesc, SQL_MAX_OPTION_STRING_LENGTH, nil, nil, 0, nil)) do
  begin
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
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetInfo(SQL_DRIVER_NAME, @Buffer, STR_LEN, @BufLen);
  Result := Buffer;
end;

function TODBCConnection.GetDBMSName: string;
var
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetInfo(SQL_DBMS_NAME, @Buffer, STR_LEN, @BufLen);
  Result := Buffer;
end;

function TODBCConnection.GetCatalogName: string;
var
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetAttribute(SQL_ATTR_CURRENT_CATALOG, @Buffer, STR_LEN, @BufLen);
  Result := Buffer;
end;

function TODBCConnection.GetSchemaName: string;
var
  Buffer: array[0..STR_LEN - 1] of SQLCHAR;
  BufLen: SQLSMALLINT;
  ST: string;
{$IFDEF USE_DRV_SPEC}
  ODBCDriverType: TODBCDrvType;
{$ENDIF}
begin
  CheckActive;
  GetInfo(SQL_SCHEMA_TERM, @Buffer, STR_LEN, @BufLen);
  ST := Buffer;
  if ST <> '' then
    GetInfo(SQL_USER_NAME, @Buffer, STR_LEN, @BufLen);

  Result := Buffer;
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
  CheckError(SQLGetConnectAttr(FHandle, SQL_ATTR_AUTOCOMMIT, @TransactMode, SQL_IS_UINTEGER, nil));

  Result := TransactMode = SQL_AUTOCOMMIT_OFF;
end;

function TODBCConnection.GetLoginTimeOut: SQLUINTEGER;
begin
  CheckActive;
  CheckError(SQLGetConnectAttr(FHandle, SQL_ATTR_LOGIN_TIMEOUT, @Result, SQL_IS_UINTEGER, nil));
end;

function TODBCConnection.GetQuoteChar: Char;
var
  p: array[0..1] of SQLCHAR;
begin
  GetInfo(SQL_IDENTIFIER_QUOTE_CHAR, @p, 2, nil);
  Result := p[0];
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
  ConnectStrOut: array[0..OUT_BUFSIZE - 1] of SQLCHAR;

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
      CheckError(SQLDriverConnect(FHandle, Application.Handle, PSQLCHAR(ParamsToConnStr(LoginParams)), SQL_NTS, @ConnectStrOut, OUT_BUFSIZE, nil, SQLUSMALLINT(FDriverCompletion)));

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

    ConnStrToParams(ConnectStrOut, FParams);
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
  CheckError(SQLGetInfo(FHandle, InfoType, InfoValuePtr, BufferLength, StringLengthPtr));
end;

function TODBCConnection.SQLDescribeParamSupported: Boolean;
var
  Buffer: array[0..1] of SQLCHAR;
  BufLen: SQLSMALLINT;
begin
  CheckActive;
  GetInfo(SQL_DESCRIBE_PARAMETER, @Buffer, SizeOf(Buffer), @BufLen);

  Result := Buffer[0] = 'Y';                      //don't localize
end;

procedure TODBCConnection.GetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER);
begin
  CheckActive;
  CheckError(SQLGetConnectAttr(FHandle, Attribute, ValuePtr, BufferLength, StringLengthPtr));
end;

procedure TODBCConnection.SetAttribute(const Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; StringLength: SQLINTEGER);
begin
  CheckActive;
  CheckError(SQLSetConnectAttr(FHandle, Attribute, ValuePtr, StringLength));
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
  SchemaNameBuf: array[0..STR_LEN - 1] of SQLCHAR;
  ProcNameBuf: array[0..STR_LEN - 1] of SQLCHAR;

  StrLen_or_Ind: SQLINTEGER;

  Data: string;

  TmpHandle: SQLHSTMT;
  RetCode: SQLRETURN;
begin
  CheckActive;
  List.Clear;

  CheckError(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @TmpHandle));
  try
    RetCode := SQLProcedures(TmpHandle, nil, 0, PSQLCHAR(Schema), Length(Schema), nil, 0);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    RetCode := SQLBindCol(TmpHandle, SCHEMA_NAME_COL_NUMBER, SQL_CHAR, @SchemaNameBuf, STR_LEN, @StrLen_or_Ind);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    RetCode := SQLBindCol(TmpHandle, PROC_NAME_COL_NUMBER, SQL_CHAR, @ProcNameBuf, STR_LEN, @StrLen_or_Ind);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    //!    CurSchemaName := SchemaName;
    while SQL_SUCCEEDED(SQLFetch(TmpHandle)) do
    begin
      Data := SchemaNameBuf;
      if Data <> '' then
        Data := Data + SCHEMA_SEPARATOR;
      Data := Data + ProcNameBuf;

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
  SchemaNameBuf: array[0..STR_LEN - 1] of SQLCHAR;
  TableNameBuf: array[0..STR_LEN - 1] of SQLCHAR;

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
    RetCode := SQLTables(TmpHandle, nil, 0, PSQLCHAR(Schema), Length(Schema), nil, 0, PSQLCHAR(TableType), SQL_NTS);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    RetCode := SQLBindCol(TmpHandle, SCHEMA_NAME_COL_NUMBER, SQL_CHAR, @SchemaNameBuf, STR_LEN, @StrLen_or_Ind);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    RetCode := SQLBindCol(TmpHandle, TABLE_NAME_COL_NUMBER, SQL_CHAR, @TableNameBuf, STR_LEN, @StrLen_or_Ind);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    while SQL_SUCCEEDED(SQLFetch(TmpHandle)) do
    begin
      Data := SchemaNameBuf;
      if Data <> '' then
        Data := Data + SCHEMA_SEPARATOR;
      Data := Data + TableNameBuf;

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
  CheckError(SQLSetConnectAttr(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_OFF), SQL_IS_UINTEGER));
end;

procedure TODBCConnection.Commit;
begin
  CheckActive;
  CheckError(SQLEndTran(SQL_HANDLE_DBC, FHandle, SQL_COMMIT));
  CheckError(SQLSetConnectAttr(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER));
end;

procedure TODBCConnection.Rollback;
begin
  CheckActive;
  CheckError(SQLEndTran(SQL_HANDLE_DBC, FHandle, SQL_ROLLBACK));
  CheckError(SQLSetConnectAttr(FHandle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_ON), SQL_IS_UINTEGER));
end;


initialization
  ODBCEnvironment := TODBCEnvironment.Create(nil);
  ODBCEnvironment.Name := 'Default';              { Do not localize }
  ODBCEnvironment.Open;

finalization
  ODBCEnvironment.Free;

end.
